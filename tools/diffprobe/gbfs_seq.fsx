// gbfs 側のプローブ: state をロードして座標/VRAM ハッシュを出し、入力列を流してダンプする。
// PyBoy 側 (pyboy_probe.py) と同じ指標を出すのが目的。
// レジスタの 1 フレームごとのログ (--reglog FILE) で層のタイミング差を数値で比較できる。
#r "/home/makoto/work/gbfs-pr/src/gbfs.Lib/bin/Release/net8.0/gbfs.Lib.dll"

open gbfs.Lib
open System.IO

let rom = "/home/makoto/work/gb-roms/Pocket Monsters - Crystal Version (Japan).gbc"
let dir = "/home/makoto/.hermes/cache/scratch/mcp/"
let outDir = "/tmp/gbfsseq"

// 引数: state 名 [--inputs "right:16,up:8"] [--dump] [--reglog FILE]
let argv = System.Environment.GetCommandLineArgs() |> Array.skip 1
let stateName =
    match argv |> Array.tryFind (fun a -> a.EndsWith(".state.json")) with
    | Some x -> x
    | None -> "prestairs.state.json"
let argOf flag =
    let i = argv |> Array.tryFindIndex (fun a -> a = flag)
    match i with
    | Some k when k + 1 < argv.Length -> Some argv.[k + 1]
    | _ -> None
let inputsSpec = argOf "--inputs" |> Option.defaultValue ""
let doDump = argv |> Array.contains "--dump"
let doBoot = argv |> Array.contains "--boot"
let regLog = argOf "--reglog"

Memory.setRtcClock (fun () -> 1_700_000_000L)
let mutable st =
    if doBoot then
        // state を読まず素の起動から始める (PyBoy 側 --boot と揃える)
        Emulator.loadRomWithSave rom (Emulator.createWithMachine Memory.Cgb)
    else
        State.loadFrom (dir + stateName)
            (Emulator.loadRomWithSave rom (Emulator.createWithMachine Memory.Cgb))

let rd (a: int) = int (Memory.read (uint16 a) st.Cpu.Mem)
let pos () = (rd 0xD4D9, rd 0xD4DA, rd 0xD4D1)

let vramHash () =
    let mutable h = 0UL
    for a in 0x8000 .. 0x9FFF do
        h <- h * 31UL + uint64 (rd a)
    h

printfn "loaded: pos=%A vram=%016X lcdc=%02X wy=%d wx=%d" (pos ()) (vramHash ()) (rd 0xFF40) (rd 0xFF4A) (rd 0xFF4B)

if doDump then Directory.CreateDirectory(outDir) |> ignore
let regOut = regLog |> Option.map (fun p -> new StreamWriter(p))
let logRegs (n: int) =
    match regOut with
    | Some w ->
        // LCDC/STAT/SCY/SCX/LY/LYC/WY/WX + 座標 + VRAM ハッシュ
        w.WriteLine(sprintf "%d %02X %02X %02X %02X %02X %02X %02X %02X %d %d %d %016X"
            n (rd 0xFF40) (rd 0xFF41) (rd 0xFF42) (rd 0xFF43) (rd 0xFF44) (rd 0xFF45)
            (rd 0xFF4A) (rd 0xFF4B)
            (rd 0xD4D9) (rd 0xD4DA) (rd 0xD4D1) (vramHash ()))
    | None -> ()

let dump (n: int) =
    if doDump then
        // 人間が見る用 (PNG) と、PyBoy 側と機械比較する用 (PPM) の両方を出す
        let b = Png.encodeRgb 160 144 (Emulator.getRgbFrameBuffer st)
        File.WriteAllBytes(sprintf "%s/g_%05d.png" outDir n, b)
        use fs = new FileStream(sprintf "%s/g_%05d.ppm" outDir n, FileMode.Create)
        let hdr = System.Text.Encoding.ASCII.GetBytes("P6\n160 144\n255\n")
        fs.Write(hdr, 0, hdr.Length)
        let fb = Emulator.getRgbFrameBuffer st
        fs.Write(fb, 0, 160 * 144 * 3)

let mutable n = 0
let tick () =
    st <- Emulator.runFrame st
    n <- n + 1
    if doDump && n % 5 = 0 then dump n
    logRegs n

let pressHold (btn: string) (holds: int) =
    st <- Emulator.pressButton btn st
    for _ in 1 .. holds do tick ()
    st <- Emulator.releaseButton btn st

logRegs 0
if inputsSpec <> "" then
    for spec in inputsSpec.Split(',') do
        let parts = spec.Trim().Split(':')
        let btn = parts.[0].Trim()
        let holds = if parts.Length > 1 then int parts.[1] else 16
        if btn = "-" || btn = "" then
            for _ in 1 .. holds do tick ()
        else
            pressHold btn holds

printfn "after %d frames: pos=%A vram=%016X lcdc=%02X wy=%d wx=%d" n (pos ()) (vramHash ()) (rd 0xFF40) (rd 0xFF4A) (rd 0xFF4B)
if doDump then
    dump n
    printfn "dumped to %s/g_*.png" outDir
match regOut with
| Some w -> w.Flush(); w.Close(); printfn "reglog -> %s" regLog.Value
| None -> ()
