// メモリフラグ駆動のステートトリガ型入力ドライバ (Task 4)
//
// usage:
//   dotnet fsi tools/state_trigger_driver.fsx -- <rom.gbc> <seq.txt>
//        [--mode frame|flag|settle] [--frames N] [--hold N] [--log-every N]
//        [--min-gap N] [--out csv] [--png out.png] [--ascii out.txt]
//
// seq.txt: 1 行 1 イベント "<frame> <button>" (PyBoy のタイムライン)
//
// 3 つのトリガ:
//   frame  — Emulator.runFrame と同一の境界 (LY 143+ -> 0) で 1 フレーム進め、
//            入力は「emulator のフレーム番号 = N」で撃つ (現状のスクリプト入力と同じ)
//   flag   — $CFA7 (フレーム解放フラグ) が N 回落ちたら撃つ。フレームは数えない
//   settle — 画面が K フレーム連続で同一 (--idle K、既定 30) かつ前回の押下後に
//            内容が変わったときだけ撃つ (モデル不要の「入力が要るか」判定)
//
// 実測 (2026-10-10, Crystal JP, seq=pyboy_game_seq.txt, hold=3, 16,000 フレーム上限):
//   frame  : 59 回押下 / flagFalls=12,414 (0.776/frame) / 本編到達 = frame 14,650
//   flag   :  4 回押下 / flagFalls=9,448  (0.591/frame) / 性別選択画面で停止
//   settle : 16 回押下 / flagFalls=15,880 (0.993/frame) / タイトル画面のまま
//   → $CFA7 は「ゲームがフレーム待ちに入っている」ことの観測点であって、
//     フレーム番号の代用にはならない (待ちループに入らない局面では落ちない)。
// 詳細: docs/state-trigger-driver.md
//
// 出力 CSV の行:
//   press,idx,target,mode,frame,flagFalls,steps,button
//   state,idx,target,mode,frame,flagFalls,steps,fbHash,vramHash,cf57,cfa7,ff9d
#r "../src/gbfs.Lib/bin/Release/net8.0/gbfs.Lib.dll"

open System
open System.IO
open System.IO.Compression
open System.Diagnostics
open System.Security.Cryptography
open gbfs.Lib

// ============================================================
// 引数
// ============================================================
let argv = fsi.CommandLineArgs |> Array.skip 1
let opts = System.Collections.Generic.Dictionary<string, string>()
let mutable pos = []
let mutable ai = 0

while ai < argv.Length do
    if argv.[ai].StartsWith "--" then
        if ai + 1 < argv.Length && not (argv.[ai + 1].StartsWith "--") then
            opts.[argv.[ai]] <- argv.[ai + 1]
            ai <- ai + 2
        else
            opts.[argv.[ai]] <- "true"
            ai <- ai + 1
    else
        pos <- argv.[ai] :: pos
        ai <- ai + 1

let posArr = pos |> List.rev |> List.toArray

let opt (name: string) (dflt: string) =
    match opts.TryGetValue name with
    | true, v -> v
    | _ -> dflt

if posArr.Length < 2 then
    eprintfn "usage: state_trigger_driver.fsx -- <rom.gbc> <seq.txt> [--mode frame|flag|settle] [--frames N]"
    exit 2

let romPath = posArr.[0]
let seqPath = posArr.[1]
let mode = (opt "--mode" "frame").ToLowerInvariant()
let maxFrames = int (opt "--frames" "16000")
let hold = int (opt "--hold" "3")
let logEvery = int (opt "--log-every" "200")
let minGap = int (opt "--min-gap" "1")
let idleK = int (opt "--idle" "30")
let outPath = opt "--out" ""
let pngPath = opt "--png" ""
let asciiPath = opt "--ascii" ""
let maxStepsPerFrame = int (opt "--max-steps" "70224")

if not (List.contains mode [ "frame"; "flag"; "settle" ]) then
    eprintfn "--mode must be frame|flag|settle"
    exit 2

// ============================================================
// PNG ライタ (dump_frames.fsx と同じ最小実装)
// ============================================================
let private crcTable =
    Array.init 256 (fun n ->
        let mutable c = uint32 n
        for _ in 1..8 do
            c <- if (c &&& 1u) <> 0u then 0xEDB88320u ^^^ (c >>> 1) else c >>> 1
        c)

let private crc32 (data: byte array) =
    let mutable c = 0xFFFFFFFFu
    for b in data do
        c <- crcTable.[int ((c ^^^ uint32 b) &&& 0xFFu)] ^^^ (c >>> 8)
    c ^^^ 0xFFFFFFFFu

let private be32 (v: uint32) =
    [| byte (v >>> 24); byte (v >>> 16); byte (v >>> 8); byte v |]

let private chunk (tag: string) (data: byte array) =
    let payload = Array.concat [ Text.Encoding.ASCII.GetBytes tag; data ]
    Array.concat [ be32 (uint32 data.Length); payload; be32 (crc32 payload) ]

let writePngRgb (w: int) (h: int) (rgb: byte array) (path: string) =
    let ihdr =
        Array.concat [ be32 (uint32 w); be32 (uint32 h); [| 8uy; 2uy; 0uy; 0uy; 0uy |] ]
    let raw = Array.zeroCreate ((w * 3 + 1) * h)
    for y in 0 .. h - 1 do
        raw.[y * (w * 3 + 1)] <- 0uy
        Array.blit rgb (y * w * 3) raw (y * (w * 3 + 1) + 1) (w * 3)
    let idat =
        use ms = new MemoryStream()
        (use z = new ZLibStream(ms, CompressionLevel.Optimal, true)
         z.Write(raw, 0, raw.Length))
        ms.ToArray()
    let png =
        Array.concat
            [ [| 0x89uy; 0x50uy; 0x4Euy; 0x47uy; 0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy |]
              chunk "IHDR" ihdr
              chunk "IDAT" idat
              chunk "IEND" [||] ]
    File.WriteAllBytes(path, png)

// ============================================================
// 入力系列
// ============================================================
let seqEvents =
    File.ReadAllLines seqPath
    |> Array.choose (fun l ->
        let p = l.Split([| ' '; '\t'; ',' |], StringSplitOptions.RemoveEmptyEntries)
        if p.Length >= 2 then
            match Int32.TryParse p.[0] with
            | true, f -> Some(f, p.[1].Trim().ToLowerInvariant())
            | _ -> None
        else None)
    |> Array.sortBy fst

// ============================================================
// エミュレータ
// ============================================================
let rom = File.ReadAllBytes romPath
let mutable st = Emulator.loadRom rom (Emulator.createWithMachine Memory.Cgb)

/// $CFA7 = WRAM 0xC000-0xCFFF (固定バンク) の 0xFA7 バイト。
/// ゲームの VBlank ハンドラ ($02FD) が 1 -> 0 に落とす「フレーム解放フラグ」。
let flag () : byte = st.Cpu.Mem.Wram.[0xFA7]

// 直接読みが Memory.read と一致することを確認 (取り違え防止)
let directOk =
    let a = Memory.read 0xCFA7us st.Cpu.Mem
    a = flag ()
if not directOk then
    eprintfn "FATAL: Wram.[0xFA7] != Memory.read 0xCFA7"
    exit 3

let fb () = Emulator.getFrameBuffer st
let rgb () = Emulator.getRgbFrameBuffer st

let mutable frames = 0
let mutable flagFalls = 0
let mutable steps = 0L
let mutable pressIdx = 0
let mutable active: (string * int) option = None
let mutable prevFlag = flag ()
let mutable prevFbHash = ""
let mutable lastPressHash = ""
let mutable stableFrames = 0

let sha (b: byte array) =
    use h = SHA256.Create()
    BitConverter.ToString(h.ComputeHash b).Replace("-", "").Substring(0, 16)

let sb = Text.StringBuilder()
if outPath <> "" then
    sb.AppendLine("kind,idx,target,mode,frame,flagFalls,steps,button,fbHash,vramHash,cf57,cfa7,ff9d") |> ignore

let logState (tag: string) (target: int) (button: string) =
    if outPath <> "" then
        let fbH = sha (fb ())
        let vram = st.Cpu.Mem.Vram.[0 .. 0x1FFF]
        let vramH = sha vram
        sb.AppendLine(
            sprintf "state,%d,%d,%s,%d,%d,%d,%s,%s,%s,%02X,%02X,%02X"
                pressIdx target mode frames flagFalls steps button fbH vramH
                (Memory.read 0xCF57us st.Cpu.Mem) (flag ()) (Memory.read 0xFF9Dus st.Cpu.Mem))
        |> ignore

let logPress (idx: int) (target: int) (button: string) =
    if outPath <> "" then
        let fbH = sha (fb ())
        let vramH = sha (st.Cpu.Mem.Vram.[0 .. 0x1FFF])
        sb.AppendLine(
            sprintf "press,%d,%d,%s,%d,%d,%d,%s,%s,%s,%02X,%02X,%02X"
                idx target mode frames flagFalls steps button fbH vramH
                (Memory.read 0xCF57us st.Cpu.Mem) (flag ()) (Memory.read 0xFF9Dus st.Cpu.Mem))
        |> ignore

let pressButton (b: string) =
    st <- Emulator.pressButton b st

let releaseButton (b: string) =
    st <- Emulator.releaseButton b st

// ---- 1 フレーム進める (runFrame と同じ境界判定 + $CFA7 の立ち下がり計数) ----
let advanceFrame () =
    let mutable ly = st.Cpu.Ppu.LY
    let mutable n = 0
    let mutable finished = false
    while not finished && n < maxStepsPerFrame do
        st <- Emulator.step st
        n <- n + 1
        let f = flag ()
        if prevFlag = 1uy && f = 0uy then flagFalls <- flagFalls + 1
        prevFlag <- f
        let ly2 = st.Cpu.Ppu.LY
        if ly > 0uy && ly2 = 0uy then finished <- true
        ly <- ly2
    frames <- frames + 1
    steps <- steps + int64 n
    // hold はフレーム数で数える。flag モードは 1 ループで複数フレーム進むので、
    // 解放もフレーム境界 (ここ) で行う。ループ回数で数えると保持が数百フレームになる。
    match active with
    | Some(b, releaseFrame) when frames >= releaseFrame ->
        releaseButton b
        active <- None
    | _ -> ()
    // 状態ログはフレーム境界で必ず出す
    if outPath <> "" && frames % logEvery = 0 then
        logState "state" -1 ""
    n

// ============================================================
// メインループ
// ============================================================
let sw = Stopwatch.StartNew()
let mutable lastPressFrame = -100

// 起動直後の状態を 1 行出す
logState "state" -1 ""

let mutable running = true

while running && frames < maxFrames do
    // ---- 1. モードに応じて進める ----
    match mode with
    | "flag" ->
        // $CFA7 が次の目標回数だけ 1 -> 0 に落ちるまで進める (フレーム数は数えない)
        let target =
            if pressIdx < seqEvents.Length then fst seqEvents.[pressIdx] else Int32.MaxValue
        while flagFalls < target && frames < maxFrames do
            advanceFrame () |> ignore
    | _ ->
        advanceFrame () |> ignore

    // ---- 2. 現在の画面状態 ----
    let h = sha (fb ())
    if h = prevFbHash then stableFrames <- stableFrames + 1 else stableFrames <- 0
    prevFbHash <- h

    // ---- 3. 押下の解放は advanceFrame 内 (フレーム境界) で行う ----

    // ---- 4. 入力の発火判定 ----
    let trigger =
        match mode with
        | "settle" ->
            stableFrames >= idleK && h <> lastPressHash && (frames - lastPressFrame) >= minGap
        | "flag" ->
            pressIdx < seqEvents.Length && flagFalls >= fst seqEvents.[pressIdx]
        | _ -> pressIdx < seqEvents.Length && frames >= fst seqEvents.[pressIdx]

    if trigger && active.IsNone && pressIdx < seqEvents.Length then
        let (target, button) = seqEvents.[pressIdx]
        pressButton button
        active <- Some(button, frames + hold)
        lastPressHash <- h
        lastPressFrame <- frames
        logPress pressIdx target button
        pressIdx <- pressIdx + 1

    // 系列を使い切ったら終了
    if pressIdx >= seqEvents.Length && active.IsNone then
        running <- false

sw.Stop()

// ============================================================
// 結果
// ============================================================
printfn "MODE=%s rom=%s seq=%s events=%d" mode (Path.GetFileName romPath) (Path.GetFileName seqPath) seqEvents.Length
printfn "RESULT frames=%d flagFalls=%d steps=%d presses=%d hold=%d elapsed_s=%.1f" frames flagFalls steps pressIdx hold sw.Elapsed.TotalSeconds
printfn "RATIO flagFalls/frames=%.4f steps/frame=%.0f" (float flagFalls / float (max 1 frames)) (float steps / float (max 1 frames))
printfn "FINAL fbHash=%s vramHash=%s CF57=%02X CFA7=%02X FF9D=%02X FF9E=%02X"
    (sha (fb ())) (sha (st.Cpu.Mem.Vram.[0 .. 0x1FFF]))
    (Memory.read 0xCF57us st.Cpu.Mem) (flag ()) (Memory.read 0xFF9Dus st.Cpu.Mem)
    (Memory.read 0xFF9Eus st.Cpu.Mem)

// 最後の 8 イベントを表示 (実際に撃ったフレーム)
if outPath <> "" then
    File.WriteAllText(outPath, sb.ToString())
    printfn "WROTE %s" outPath

if pngPath <> "" then
    writePngRgb 160 144 (rgb ()) pngPath
    printfn "WROTE %s" pngPath

if asciiPath <> "" then
    File.WriteAllText(asciiPath, Emulator.getScreenAsText st)
    printfn "WROTE %s" asciiPath
