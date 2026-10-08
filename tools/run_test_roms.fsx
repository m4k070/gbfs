// 公開テスト ROM (blargg / mooneye-test-suite) を gbfs の Native CPU で流し、
// ROM ごとに PASS / FAIL / TIMEOUT を出すランナー。
//
// 使い方 (リポジトリのルートで):
//   nix develop -c dotnet build src/gbfs.Lib/gbfs.Lib.fsproj -c Release
//   nix develop -c dotnet fsi tools/run_test_roms.fsx -- [options] <rom.gb | dir>...
//
// options:
//   --max-seconds <n>   1 本あたりのエミュレート時間上限 (実機換算秒、既定 blargg 120 / mooneye 20)
//   --protocol <blargg|mooneye>   通知方式を明示 (既定はパスから推定: "mts-" / "mooneye" を含めば mooneye)
//   --tsv <path>        結果を TSV で書き出す
//   --quiet             ROM ごとの詳細 (シリアル末尾・レジスタ・STOP 実行有無) を省く
//
// 合否判定は tools/TestRomVerdict.fs (エミュレータ非依存の純粋関数)。
// このスクリプトは gbfs 固有の「観測」(シリアル書込・LD B,B・0xA000) だけを担う。
//
// シリアル: gbfs.Lib の Memory はシリアル転送を実装していない (SB/SC は単なる I/O 配列)。
// そこで各 step の後に SC を見て、0x81 (内部クロックで転送開始) なら SB を 1 文字として拾い、
// 実機の「相手なし」転送完了と同じく SB=0xFF・SC bit7 クリア・IF bit3 セットにする。

#r "../src/gbfs.Lib/bin/Release/net8.0/gbfs.Lib.dll"
#load "TestRomVerdict.fs"

open System
open System.IO
open gbfs.Lib
open TestRomVerdict

// ============================================================
// 定数
// ============================================================

/// DMG の CPU クロック (T サイクル / 秒)
let CpuHz = 4194304L

/// cpu_instrs.gb (合成版) は実機で約 55 秒かかるので余裕を持たせる
let DefaultMaxSecondsBlargg = 120.0
let DefaultMaxSecondsMooneye = 20.0

/// blargg の終了語を見つけた後、後続の文字 ("Failed #3" の番号など) を拾うために追加で回す時間
let BlarggGraceCycles = CpuHz / 4L

/// 判定関数を呼ぶ間隔 (step 数)。毎 step 文字列を走査しないため
let JudgeIntervalSteps = 4096

let SerialDataAddr = 0xFF01us
let SerialControlAddr = 0xFF02us
let InterruptFlagAddr = 0xFF0Fus
let InterruptEnableAddr = 0xFFFFus
let SerialInterruptBit = 0x08uy
let SerialNoPartnerByte = 0xFFuy
let StopOpcode = 0x10uy
let CbPrefixOpcode = 0xCBuy

// ============================================================
// 引数
// ============================================================

type Options = {
    Paths: string list
    MaxSeconds: float option
    Protocol: Protocol option
    TsvPath: string option
    Quiet: bool
    /// マシン種別 (既定 DMG)。CGB 用テスト ROM (misc/ の -C / cgb 系) は --machine cgb で回す
    Machine: Memory.MachineMode
}

let rec parseArgs (opts: Options) (args: string list) : Result<Options, string> =
    match args with
    | [] -> Ok { opts with Paths = List.rev opts.Paths }
    | "--" :: rest -> parseArgs opts rest
    | "--max-seconds" :: v :: rest ->
        match Double.TryParse(v, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
        | true, n when n > 0.0 -> parseArgs { opts with MaxSeconds = Some n } rest
        | _ -> Error (sprintf "--max-seconds must be a positive number, got %s" v)
    | "--protocol" :: "blargg" :: rest -> parseArgs { opts with Protocol = Some BlarggSerial } rest
    | "--protocol" :: "mooneye" :: rest -> parseArgs { opts with Protocol = Some MooneyeFibonacci } rest
    | "--protocol" :: v :: _ -> Error (sprintf "--protocol must be blargg or mooneye, got %s" v)
    | "--machine" :: "dmg" :: rest -> parseArgs { opts with Machine = Memory.Dmg } rest
    | "--machine" :: "cgb" :: rest -> parseArgs { opts with Machine = Memory.Cgb } rest
    | "--machine" :: v :: _ -> Error (sprintf "--machine must be dmg or cgb, got %s" v)
    | "--tsv" :: v :: rest -> parseArgs { opts with TsvPath = Some v } rest
    | "--quiet" :: rest -> parseArgs { opts with Quiet = true } rest
    | a :: _ when a.StartsWith("--") -> Error (sprintf "unknown option: %s" a)
    | p :: rest -> parseArgs { opts with Paths = p :: opts.Paths } rest

/// ファイルはそのまま、ディレクトリは配下の *.gb を再帰的に (名前順で) 集める
let collectRoms (paths: string list) : Result<string list, string> =
    let expand (p: string) =
        if File.Exists p then Ok [ p ]
        elif Directory.Exists p then
            Directory.GetFiles(p, "*.gb", SearchOption.AllDirectories)
            |> Array.sortWith (fun a b -> String.CompareOrdinal(a, b))
            |> List.ofArray
            |> Ok
        else Error (sprintf "no such file or directory: %s" p)
    let folder acc p =
        match acc, expand p with
        | Ok xs, Ok ys -> Ok (xs @ ys)
        | Error e, _ | _, Error e -> Error e
    paths |> List.fold folder (Ok [])

// ============================================================
// 観測 (gbfs 固有)
// ============================================================

type RunStats = {
    Instructions: int64
    Cycles: int64
    WallTime: TimeSpan
    /// 実行した非 CB opcode (256) と CB opcode (256) の回数
    OpcodeCounts: int64 array
    CbOpcodeCounts: int64 array
}

type RomResult = {
    Path: string
    Protocol: Protocol
    Verdict: Verdict
    Stats: RunStats
    SerialText: string
    MooneyeRegisters: MooneyeRegisters option
}

let readReg8 (reg: Cpu.Reg8) (regs: Cpu.Register) : byte =
    byte (Cpu.getRegisterValue (Cpu.R8 reg) regs)

let captureMooneyeRegisters (regs: Cpu.Register) : MooneyeRegisters =
    { B = readReg8 Cpu.B regs; C = readReg8 Cpu.C regs; D = readReg8 Cpu.D regs
      E = readReg8 Cpu.E regs; H = readReg8 Cpu.H regs; L = readReg8 Cpu.L regs }

/// blargg の結果領域 (0xA000..) を MBC の RAM 有効フラグを介さずに直接読む
let readBlarggCartRam (mem: Memory.MemoryBus) : BlarggCartRam option =
    let ram = mem.ExtRam
    let textStart = 4
    let maxTextLength = 0x1000
    if ram.Length < textStart + 1 then None
    else
        let textEnd =
            let limit = min ram.Length (textStart + maxTextLength)
            let zeroAt = Array.IndexOf(ram, 0uy, textStart, limit - textStart)
            if zeroAt < 0 then limit else zeroAt
        Some { Status = ram.[0]
               Signature = ram.[1..3]
               Text = Text.Encoding.ASCII.GetString(ram, textStart, textEnd - textStart) }

/// mooneye の .sym (ROM と同名) から assert 保存領域のアドレスを引く。
/// 行の形式は "01:ff88 hram.regs_flags"
type MooneyeAssertionAddrs = { SaveAddr: uint16; FlagsAddr: uint16; ExpectedAddr: uint16 }

let MooneyeAssertionRegisterCount = 8

let loadMooneyeAssertionAddrs (romPath: string) : MooneyeAssertionAddrs option =
    let symPath = Path.ChangeExtension(romPath, ".sym")
    if not (File.Exists symPath) then None
    else
        let labels =
            File.ReadAllLines symPath
            |> Array.choose (fun line ->
                match line.Split(' ', StringSplitOptions.RemoveEmptyEntries) with
                | [| bankAddr; label |] when bankAddr.Contains(':') ->
                    let addrHex = bankAddr.Substring(bankAddr.IndexOf(':') + 1)
                    Some (label, Convert.ToUInt16(addrHex, 16))
                | _ -> None)
            |> Map.ofArray
        match labels.TryFind "hram.regs_save", labels.TryFind "hram.regs_flags", labels.TryFind "hram.regs_assert" with
        | Some save, Some flags, Some expected -> Some { SaveAddr = save; FlagsAddr = flags; ExpectedAddr = expected }
        | _ -> None

let readMooneyeAssertions (addrs: MooneyeAssertionAddrs) (mem: Memory.MemoryBus) : MooneyeAssertions =
    let readBlock (start: uint16) =
        Array.init MooneyeAssertionRegisterCount (fun i -> Memory.read (start + uint16 i) mem)
    { Saved = readBlock addrs.SaveAddr
      Flags = Memory.read addrs.FlagsAddr mem
      Expected = readBlock addrs.ExpectedAddr }

/// この step で割込みディスパッチが起きるか (起きるなら命令は実行されない)
let willDispatchInterrupt (cpu: Decoder.CpuState) : bool =
    let ie = Memory.read InterruptEnableAddr cpu.Mem
    let ifReg = Memory.read InterruptFlagAddr cpu.Mem
    cpu.Ime && (ie &&& ifReg &&& 0x1Fuy) <> 0uy

/// この step で実行される命令のアドレス (HALT 中・割込みディスパッチなら None)
let nextInstructionAddr (cpu: Decoder.CpuState) : uint16 option =
    if cpu.Halted || willDispatchInterrupt cpu then None
    elif cpu.HaltBug then Some (cpu.Regs.PC + 1us)
    else Some cpu.Regs.PC

/// SC bit7 が 0→1 になった瞬間に送信バイトを拾う。
/// 転送の完了 (SB = 0xFF / SC bit7 クリア / IF bit3) はエミュレータ側の Memory.tickSerial が行う
let pollSerial (prevSc: byte) (mem: Memory.MemoryBus) : byte option =
    let sc = Memory.read SerialControlAddr mem
    if (sc &&& 0x80uy) <> 0uy && (prevSc &&& 0x80uy) = 0uy then
        Some (Memory.read SerialDataAddr mem)
    else None

// ============================================================
// 1 本の実行
// ============================================================

let runRom (protocol: Protocol) (machine: Memory.MachineMode) (maxSeconds: float) (path: string) : RomResult =
    let rom = File.ReadAllBytes path
    let maxCycles = int64 (maxSeconds * float CpuHz)
    let limitDescription = sprintf "%.0f emulated seconds (%d cycles)" maxSeconds maxCycles
    let opcodeCounts = Array.zeroCreate<int64> 256
    let cbOpcodeCounts = Array.zeroCreate<int64> 256
    let serial = Text.StringBuilder()
    let sw = Diagnostics.Stopwatch.StartNew()

    let mutable cpu = Decoder.loadRomToState rom (Decoder.createStateFor machine ())
    let mutable instructions = 0L
    let mutable cycles = 0L
    let mutable steps = 0
    let mutable progress = InProgress
    /// blargg の終了語を見つけた後、このサイクルまで回して後続の文字を拾う
    let mutable graceUntil : int64 option = None
    let mutable mooneyeRegs : MooneyeRegisters option = None
    let mutable finished = false
    let assertionAddrs =
        match protocol with
        | MooneyeFibonacci -> loadMooneyeAssertionAddrs path
        | BlarggSerial -> None

    while not finished do
        match nextInstructionAddr cpu with
        | Some addr ->
            let opcode = Memory.read addr cpu.Mem
            opcodeCounts.[int opcode] <- opcodeCounts.[int opcode] + 1L
            if opcode = CbPrefixOpcode then
                let cbOpcode = Memory.read (addr + 1us) cpu.Mem
                cbOpcodeCounts.[int cbOpcode] <- cbOpcodeCounts.[int cbOpcode] + 1L
            instructions <- instructions + 1L
            let isMooneyeBreakpoint = protocol = MooneyeFibonacci && opcode = MooneyeBreakpointOpcode
            if isMooneyeBreakpoint then
                let regs = captureMooneyeRegisters cpu.Regs
                mooneyeRegs <- Some regs
                let assertions = assertionAddrs |> Option.map (fun a -> readMooneyeAssertions a cpu.Mem)
                progress <- Concluded (judgeMooneye regs assertions)
                finished <- true
        | None -> ()

        if not finished then
            let prevSc = Memory.read SerialControlAddr cpu.Mem
            let (next, stepCycles) = Decoder.stepWithCycles cpu
            cpu <- next
            pollSerial prevSc cpu.Mem |> Option.iter (fun b -> serial.Append(char b) |> ignore)
            cycles <- cycles + int64 stepCycles
            steps <- steps + 1

            let shouldJudge = protocol = BlarggSerial && graceUntil.IsNone && steps % JudgeIntervalSteps = 0
            if shouldJudge then
                progress <- judgeBlargg (serial.ToString()) (readBlarggCartRam cpu.Mem)
                match progress with
                | Concluded _ -> graceUntil <- Some (cycles + BlarggGraceCycles)
                | InProgress -> ()

            match graceUntil with
            | Some until when cycles >= until ->
                // 猶予中に届いた文字も含めて判定し直す (失敗理由の全文を残すため)
                progress <- judgeBlargg (serial.ToString()) (readBlarggCartRam cpu.Mem)
                finished <- true
            | _ -> ()

            if cycles >= maxCycles then finished <- true

    sw.Stop()
    { Path = path
      Protocol = protocol
      Verdict = finalizeAtLimit limitDescription progress
      Stats =
        { Instructions = instructions
          Cycles = cycles
          WallTime = sw.Elapsed
          OpcodeCounts = opcodeCounts
          CbOpcodeCounts = cbOpcodeCounts }
      SerialText = serial.ToString()
      MooneyeRegisters = mooneyeRegs }

// ============================================================
// 出力
// ============================================================

let serialTail (maxChars: int) (text: string) : string =
    let t = text.TrimEnd()
    let tail = if t.Length > maxChars then "..." + t.Substring(t.Length - maxChars) else t
    tail.Replace("\n", "\\n")

let displayName (path: string) : string =
    let dir = Path.GetFileName(Path.GetDirectoryName(path))
    dir + "/" + Path.GetFileName(path)

let stopExecuted (r: RomResult) : bool =
    r.Stats.OpcodeCounts.[int StopOpcode] > 0L

/// 画面表示で失敗理由を切り詰める長さ (TSV には全文を書く)
let DisplayReasonMaxChars = 400

/// 失敗理由と観測値を 1 行にまとめる。maxReasonChars を超える理由は末尾だけ残す
let detailOf (maxReasonChars: int) (r: RomResult) : string =
    let verdictDetail =
        match r.Verdict with
        | Passed -> ""
        | Failed reason -> serialTail maxReasonChars reason
        | Timeout limit -> limit
    let observation =
        match r.Protocol, r.MooneyeRegisters, r.Verdict with
        // 失敗理由にレジスタ値・出力テキストが含まれるので重ねて出さない
        | _, _, Failed _ -> ""
        | MooneyeFibonacci, Some regs, _ -> formatMooneyeRegisters regs
        | MooneyeFibonacci, None, _ -> "LD B,B not reached"
        | BlarggSerial, _, _ -> "serial: " + serialTail 200 r.SerialText
    [ verdictDetail; observation ] |> List.filter (fun s -> s <> "") |> String.concat " | "

let printResult (quiet: bool) (r: RomResult) : unit =
    let emulatedSeconds = float r.Stats.Cycles / float CpuHz
    printfn "%-7s %-45s instr=%11d cycles=%12d emu=%6.2fs wall=%6.1fs%s"
        (verdictLabel r.Verdict) (displayName r.Path)
        r.Stats.Instructions r.Stats.Cycles emulatedSeconds r.Stats.WallTime.TotalSeconds
        (if stopExecuted r then " STOP" else "")
    if not quiet then
        printfn "        %s" (detailOf DisplayReasonMaxChars r)

let writeTsv (path: string) (results: RomResult list) : unit =
    let header = "rom\tverdict\tinstructions\tcycles\temulated_seconds\twall_seconds\tstop_executed\tdistinct_opcodes\tdistinct_cb_opcodes\tdetail"
    let distinct (counts: int64 array) = counts |> Array.filter (fun c -> c > 0L) |> Array.length
    let row (r: RomResult) =
        String.concat "\t" [
            r.Path
            verdictLabel r.Verdict
            string r.Stats.Instructions
            string r.Stats.Cycles
            sprintf "%.3f" (float r.Stats.Cycles / float CpuHz)
            sprintf "%.3f" r.Stats.WallTime.TotalSeconds
            string (stopExecuted r)
            string (distinct r.Stats.OpcodeCounts)
            string (distinct r.Stats.CbOpcodeCounts)
            (detailOf Int32.MaxValue r).Replace("\t", " ")
        ]
    File.WriteAllLines(path, header :: List.map row results)

// ============================================================
// main
// ============================================================

let main (argv: string list) : int =
    let emptyOptions = { Paths = []; MaxSeconds = None; Protocol = None; TsvPath = None; Quiet = false; Machine = Memory.Dmg }
    let parsed = parseArgs emptyOptions argv |> Result.bind (fun o -> collectRoms o.Paths |> Result.map (fun roms -> (o, roms)))
    match parsed with
    | Error e ->
        eprintfn "error: %s" e
        2
    | Ok (_, []) ->
        eprintfn "usage: dotnet fsi tools/run_test_roms.fsx -- [--max-seconds n] [--protocol blargg|mooneye] [--machine dmg|cgb] [--tsv out.tsv] [--quiet] <rom.gb|dir>..."
        2
    | Ok (opts, roms) ->
        let runOne path =
            let protocol = opts.Protocol |> Option.defaultValue (inferProtocol path)
            let defaultSeconds =
                match protocol with
                | BlarggSerial -> DefaultMaxSecondsBlargg
                | MooneyeFibonacci -> DefaultMaxSecondsMooneye
            let result = runRom protocol opts.Machine (opts.MaxSeconds |> Option.defaultValue defaultSeconds) path
            printResult opts.Quiet result
            result
        let results = roms |> List.map runOne
        opts.TsvPath |> Option.iter (fun p -> writeTsv p results)
        let count label = results |> List.filter (fun r -> verdictLabel r.Verdict = label) |> List.length
        printfn ""
        printfn "total=%d PASS=%d FAIL=%d TIMEOUT=%d" results.Length (count "PASS") (count "FAIL") (count "TIMEOUT")
        if count "PASS" = results.Length then 0 else 1

exit (main (List.ofArray fsi.CommandLineArgs |> List.tail))
