// 実 ROM をヘッドレスで走らせて画面を PNG に出す (CGB 対応の目視検証用)
//
// usage:
//   dotnet fsi tools/dump_frames.fsx -- <rom.gb> [--machine dmg|cgb] [--frames 120] [--out out.png]
//
// バッテリー付きカートは ROM の隣の .sav を読む (書き戻しはしない)。
// 出力は 8bit グレースケール (DMG の 4 階調を 255/170/85/0 に対応)。
// CGB のカラー描画を実装したら RGB 出力に切り替える。
#r "/home/makoto/work/gbfs-bootregs/src/gbfs.Lib/bin/Release/net8.0/gbfs.Lib.dll"

open System
open System.IO
open System.IO.Compression
open gbfs.Lib

// ============================================================
// 最小の PNG ライタ (8bit グレースケール、無圧縮に近い zlib ストリーム)
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

let writePngGray (w: int) (h: int) (gray: byte array) (path: string) =
    let ihdr =
        Array.concat [ be32 (uint32 w); be32 (uint32 h); [| 8uy; 0uy; 0uy; 0uy; 0uy |] ]
    // 各行の先頭にフィルタ種別 0 を付ける
    let raw = Array.zeroCreate ((w + 1) * h)
    for y in 0 .. h - 1 do
        raw.[y * (w + 1)] <- 0uy
        Array.blit gray (y * w) raw (y * (w + 1) + 1) w
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
// 引数
// ============================================================

let mutable romPath = ""
let mutable machine = Memory.Dmg
let mutable frames = 120
let mutable outPath = ""

let args = fsi.CommandLineArgs |> Array.skip 1
let mutable i = 0
while i < args.Length do
    match args.[i] with
    | "--machine" ->
        machine <- (if args.[i + 1] = "cgb" then Memory.Cgb else Memory.Dmg)
        i <- i + 2
    | "--frames" ->
        frames <- int args.[i + 1]
        i <- i + 2
    | "--out" ->
        outPath <- args.[i + 1]
        i <- i + 2
    | a ->
        romPath <- a
        i <- i + 1

if romPath = "" then
    eprintfn "usage: dotnet fsi tools/dump_frames.fsx -- <rom.gb> [--machine dmg|cgb] [--frames N] [--out out.png]"
    exit 2

if outPath = "" then
    outPath <- Path.Combine(Path.GetDirectoryName(Path.GetFullPath romPath), Path.GetFileNameWithoutExtension romPath + ".png")

// ============================================================
// 実行
// ============================================================

let sw = Diagnostics.Stopwatch.StartNew()
let state =
    Emulator.createWithMachine machine
    |> Emulator.loadRomWithSave romPath
    |> Emulator.runFrames frames

let fb = Emulator.getFrameBuffer state
let shades = fb |> Array.countBy id |> Array.sortBy fst
let gray = fb |> Array.map (fun s -> byte (255 - 85 * int (min 3uy s)))

writePngGray 160 144 gray outPath
sw.Stop()

printfn "rom        = %s" (Path.GetFileName romPath)
printfn "machine    = %s / mapper = %s / CGB mode = %b"
    (Memory.machineName machine) (Emulator.getMapperName state) state.Cpu.Mem.CgbMode
printfn "frames     = %d / cycles = %d" state.FrameCount state.TotalCycles
printfn "framebuffer= %A" shades
printfn "out        = %s (%.2fs)" outPath sw.Elapsed.TotalSeconds
