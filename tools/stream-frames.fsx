// gbfs の画面を PNG (160x144) で stdout に流し続ける (--png 無しなら raw RGB24)。
//
// 配信では tools/gbfs.StreamFeeder (コンパイル済み) を使うこと。こちらは fsi の逐次実行のため
// Sleep 粒度の影響で実時間が約 0.37% 遅れる (feeder は実測 59.729fps / 目標 59.7275)。
// ラッパー (~/.local/bin/gbfs-stream) は feeder が無いときのフォールバックとしてこれを使う。
//
// 用法:
//   dotnet fsi tools/stream-frames.fsx -- <rom.gb> [state.json] [--fps 59.7275] [--audio-fifo PATH]
//
// --audio-fifo を渡すと APU の音声 (s16le 44100Hz stereo) をその FIFO に書き出す。
//
// stdin から入力を与えられる (1行1コマンド)。ストリーム中でもエージェント/人間が操作できる。
//   <button>:<frames>   そのボタンを frames フレーム押す  例) right:16
//   -:<frames>          何も押さずに frames フレーム待つ  例) -:60
//   button は up/down/left/right/a/b/start/select
#r "/home/makoto/work/gbfs-pr/src/gbfs.Lib/bin/Release/net8.0/gbfs.Lib.dll"

open gbfs.Lib
open System
open System.Diagnostics
open System.Threading
open System.Collections.Concurrent

let argv =
    fsi.CommandLineArgs
    |> Array.filter (fun a -> not (a.EndsWith(".fsx")) && a <> "--")

let rom = if argv.Length >= 1 then argv.[0] else "/home/makoto/work/gb-roms/Pocket Monsters - Crystal Version (Japan).gbc"
let stateFile = argv |> Array.tryFind (fun a -> a.EndsWith(".json"))
let fps =
    let i = argv |> Array.tryFindIndex (fun a -> a = "--fps")
    match i with
    | Some k when k + 1 < argv.Length -> float argv.[k + 1]
    | _ -> 65536.0 / 1097.0
let audioFifo =
    let i = argv |> Array.tryFindIndex (fun a -> a = "--audio-fifo")
    match i with
    | Some k when k + 1 < argv.Length -> Some argv.[k + 1]
    | _ -> None
// --png: 生 RGB24 ではなく PNG を stdout に流す (image2pipe)。
// 映像入力を FIFO と併用すると生 RAW は読み取りの途中で切れて化けるため、自己フレーム化できる PNG を使う。
let usePng = argv |> Array.contains "--png"

Memory.setRtcClock (fun () -> 1_700_000_000L)

let mutable st =
    match stateFile with
    | Some f -> State.loadFrom f (Emulator.loadRomWithSave rom (Emulator.createWithMachine Memory.Cgb))
    | None -> Emulator.loadRomWithSave rom (Emulator.createWithMachine Memory.Cgb)

// 映像の出力は fd 1 を素の FileStream で開く。Console.OpenStandardOutput() は
// 部分書き込み（short write）を書き切り保証しないため、パイプが詰まると PNG 列が壊れる。
let out =
    new IO.FileStream(
        new Microsoft.Win32.SafeHandles.SafeFileHandle(nativeint 1, false),
        IO.FileAccess.Write, 65536, false)

// 音声: APU の L/R interleaved float を s16le stereo にして FIFO へ書く。
// FIFO を O_WRONLY で開くと「読み手が来るまで」ブロックし、ffmpeg 側の O_RDONLY と
// 噛み合わずデッドロックする。O_RDWR で開けば open がブロックしないので確実。
let audio =
    audioFifo
    |> Option.map (fun p ->
        Console.Error.WriteLine(
            sprintf "audio-fifo: %s (s16le stereo, 宣言レートは使い手側が指定)" p )
        new IO.FileStream(p, IO.FileMode.Open, IO.FileAccess.ReadWrite))

// 音声は別スレッドで書く。同一スレッドで FIFO に書くと、FIFO が詰まった瞬間に
// 映像の供給が止まり、ffmpeg の入力読み取り順と噛み合わずデッドロックする。
let audioQueue = ConcurrentQueue<byte[]>()
let MAX_QUEUED = 120   // 約 2 秒分。ffmpeg が詰まったら古い音声は捨てる

let audioWriter =
    audio
    |> Option.map (fun a ->
        let t =
            Thread(fun () ->
                try
                    while true do
                        match audioQueue.TryDequeue() with
                        | true, buf ->
                            a.Write(buf, 0, buf.Length)
                            a.Flush()
                        | _ -> Thread.Sleep(5)
                with _ -> ())
        t.IsBackground <- true
        t.Start()
        t)

let drainAudio () =
    match audio with
    | None -> ()
    | Some _ ->
        let samples = Emulator.getAudioBuffer st
        if samples.Length > 0 then
            let bytes = Array.zeroCreate<byte> (samples.Length * 2)
            for i in 0 .. samples.Length - 1 do
                let v = max -1.0 (min 1.0 samples.[i])
                let u = (int (v * 32767.0)) &&& 0xFFFF
                bytes.[i * 2] <- byte (u &&& 0xFF)
                bytes.[i * 2 + 1] <- byte (u >>> 8)
            if audioQueue.Count < MAX_QUEUED then audioQueue.Enqueue bytes
            st <- Emulator.clearAudioBuffer st

let q = ConcurrentQueue<string>()
let reader = Thread(fun () ->
    try
        let mutable line = Console.In.ReadLine()
        while line <> null do
            q.Enqueue line
            line <- Console.In.ReadLine()
    with _ -> ())
reader.IsBackground <- true
reader.Start()

let W = 160
let H = 144
let frameMs = 1000.0 / fps

let mutable held = ""
let mutable remaining = 0
let sw = Stopwatch.StartNew()
let mutable n = 0L

let writeVideo () =
    if usePng then
        let b = Png.encodeRgb W H (Emulator.getRgbFrameBuffer st)
        out.Write(b, 0, b.Length)
    else
        out.Write(Emulator.getRgbFrameBuffer st, 0, W * H * 3)
    out.Flush()

// 状態ファイルには未消費の APU バッファが最大 4096 サンプル (= 93ms) 残っていることがある。
// そのまま流すと配信の頭で音だけ先行するので捨てる (これを数えると APU のレートを
// 過大に見積もる。実測 738.35 サンプル/フレーム = 44100Hz が正しい値)。
st <- Emulator.clearAudioBuffer st

// 最初の1フレームを描画させてから配信開始
st <- Emulator.runFrame st
drainAudio ()
writeVideo ()
n <- 1L

while true do
    let mutable cmd = ""
    while q.TryDequeue(&cmd) do
        let parts = cmd.Trim().Split(':')
        if parts.Length = 2 then
            let b = parts.[0].Trim()
            match Int32.TryParse(parts.[1].Trim()) with
            | true, frames when frames > 0 ->
                if held <> "" then
                    st <- Emulator.releaseButton held st
                    held <- ""
                if b <> "-" && b <> "" then
                    st <- Emulator.pressButton b st
                    held <- b
                remaining <- frames
            | _ -> ()
    st <- Emulator.runFrame st
    if remaining > 0 then
        remaining <- remaining - 1
        if remaining = 0 && held <> "" then
            st <- Emulator.releaseButton held st
            held <- ""
    writeVideo ()
    drainAudio ()
    n <- n + 1L
    let target = float n * frameMs
    let wait = target - sw.Elapsed.TotalMilliseconds
    if wait > 2.0 then Thread.Sleep(int (wait - 1.0))
    elif wait > 0.0 then Thread.SpinWait(400)
