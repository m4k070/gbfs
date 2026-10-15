// gbfs の画面を PNG (160x144) で stdout に流し続ける。ffmpeg の -f image2pipe 入力用。
//
// 用法:
//   gbfs.StreamFeeder <rom.gb> [state.json] [--fps 59.7275] [--png] [--audio-fifo PATH]
//
// --audio-fifo を渡すと APU の音声 (s16le stereo) をその FIFO に書き出す。
// レートは APU の実測どおり 44100Hz (738.35 サンプル/フレーム)。
//
// stdin から入力を与えられる (1行1コマンド)。ストリーム中でも操作できる。
//   <button>:<frames>   そのボタンを frames フレーム押す  例) right:16
//   -:<frames>          何も押さずに frames フレーム待つ  例) -:60
//   button は up/down/left/right/a/b/start/select
//
// tools/stream-frames.fsx と等価だが、fsi (逐次実行) は 60fps にわずかに追いつかず
// 実時間のペースが約 0.37% 遅れるため、配信ではこちらを使う。
module gbfs.StreamFeeder.Program

open gbfs.Lib
open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Threading
open System.Collections.Concurrent

/// feeder が実行するアクション
type Act =
    /// ボタンを frames フレーム押す
    | Press of button: string * frames: int
    /// その方向へ、座標が実際に tiles マスぶん変わるまで押し続ける。
    /// 「hold N フレーム = 何マス」という較正に依存せず、実際の座標変化で止める。
    | Walk of button: string * tiles: int
    /// 何も押さずに frames フレーム待つ
    | Wait of frames: int

[<EntryPoint>]
let main argv =
    let args = argv |> Array.filter (fun a -> a <> "--")

    let rom =
        if args.Length >= 1 then args.[0]
        else "/home/makoto/work/gb-roms/Pocket Monsters - Crystal Version (Japan).gbc"

    let stateFile = args |> Array.tryFind (fun a -> a.EndsWith(".json"))

    let fps =
        let i = args |> Array.tryFindIndex (fun a -> a = "--fps")
        match i with
        | Some k when k + 1 < args.Length -> float args.[k + 1]
        | _ -> 65536.0 / 1097.0

    let audioFifo =
        let i = args |> Array.tryFindIndex (fun a -> a = "--audio-fifo")
        match i with
        | Some k when k + 1 < args.Length -> Some args.[k + 1]
        | _ -> None

    let usePng = args |> Array.contains "--png"

    // stdin から "shot" が来たら現在の画面を PNG として書き出す (配信中の確認用)。
    let shotPath =
        let i = args |> Array.tryFindIndex (fun a -> a = "--shot")
        match i with
        | Some k when k + 1 < args.Length -> args.[k + 1]
        | _ -> "/tmp/gbfs-shot.png"

    // stdin から "save" が来たら現在の状態を state JSON として書き出す (再開用)。
    let savePath =
        let i = args |> Array.tryFindIndex (fun a -> a = "--save")
        match i with
        | Some k when k + 1 < args.Length -> args.[k + 1]
        | _ -> "/tmp/gbfs-save.state.json"

    // 移動履歴の追記先 (JSON Lines)。地図作成と移動量の検証に使う。
    let tracePath =
        let i = args |> Array.tryFindIndex (fun a -> a = "--trace")
        match i with
        | Some k when k + 1 < args.Length -> args.[k + 1]
        | _ -> "/tmp/gbfs-trace.jsonl"

    Memory.setRtcClock (fun () -> 1_700_000_000L)

    let mutable st =
        match stateFile with
        | Some f ->
            State.loadFrom f (Emulator.loadRomWithSave rom (Emulator.createWithMachine Memory.Cgb))
        | None -> Emulator.loadRomWithSave rom (Emulator.createWithMachine Memory.Cgb)

    // Gen2 の座標とマップ: $D4D9/$D4DA = プレイヤーのタイル座標, $D4D1 = 向き (0=下 4=上 8=左 12=右),
    // $DCB5/$DCB6 = 現在のマップ (group, number)。
    let rdA (a: int) = int (Memory.read (uint16 a) st.Cpu.Mem)
    let px () = rdA 0xD4D9
    let py () = rdA 0xD4DA
    let facing () = rdA 0xD4D1

    // 映像の出力は fd 1 を素の FileStream で開く。Console.OpenStandardOutput() は
    // 部分書き込み (short write) を書き切り保証しないため、パイプが詰まると PNG 列が壊れる。
    let out =
        new FileStream(
            new Microsoft.Win32.SafeHandles.SafeFileHandle(nativeint 1, false),
            FileAccess.Write, 65536, false)

    // 音声: FIFO を O_RDWR で開く。O_WRONLY だと「読み手が来るまで」open がブロックし、
    // ffmpeg 側の O_RDONLY と噛み合わずデッドロックする。
    let audio =
        audioFifo
        |> Option.map (fun p ->
            Console.Error.WriteLine(
                sprintf "audio-fifo: %s (s16le stereo, 宣言レートは使い手側が指定)" p)
            new FileStream(p, FileMode.Open, FileAccess.ReadWrite))

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
    let reader =
        Thread(fun () ->
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

    // stdin のコマンドは「アクション列」として 1 つずつ順番に実行する。
    // 一度に複数行を流し込んでも取りこぼさない (各行は前の行が終わってから始まる)。
    let actions = Queue<Act>()
    let mutable curAct: Act option = None
    let mutable curButton = ""
    let mutable curLeft = 0
    let mutable walkDone = 0          // walk で実際に動けたマス数 (座標の変化で数える)
    let mutable walkStill = 0         // 動けないまま経過したフレーム数
    let mutable prevXY = (0, 0)
    let mutable seg = 0               // マップ区間。座標が大きく飛んだら (マップ移動) 次の区間
    let sw = Stopwatch.StartNew()
    let mutable n = 0L

    // 移動履歴 (座標が変わったときだけ 1 行追記する)
    let traceWriter = new StreamWriter(tracePath, true)
    traceWriter.AutoFlush <- true
    let tracePos (note: string) =
        traceWriter.WriteLine(
            sprintf "{\"t\":%d,\"seg\":%d,\"x\":%d,\"y\":%d,\"dir\":%d,\"note\":\"%s\"}"
                n seg (px ()) (py ()) (facing ()) note)

    let writeVideo () =
        if usePng then
            let b = Png.encodeRgb W H (Emulator.getRgbFrameBuffer st)
            out.Write(b, 0, b.Length)
        else
            out.Write(Emulator.getRgbFrameBuffer st, 0, W * H * 3)
        out.Flush()

    // 等倍 4 倍拡大 (shot コマンド用)
    let scale4 (src: byte array) =
        let dst = Array.zeroCreate<byte> (W * 4 * H * 4 * 3)
        for y in 0 .. H - 1 do
            for x in 0 .. W - 1 do
                let s = (y * W + x) * 3
                for dy in 0 .. 3 do
                    for dx in 0 .. 3 do
                        let o = ((y * 4 + dy) * (W * 4) + (x * 4 + dx)) * 3
                        dst.[o] <- src.[s]
                        dst.[o + 1] <- src.[s + 1]
                        dst.[o + 2] <- src.[s + 2]
        dst

    let saveShot () =
        File.WriteAllBytes(shotPath, Png.encodeRgb (W * 4) (H * 4) (scale4 (Emulator.getRgbFrameBuffer st)))
        Console.Error.WriteLine(sprintf "shot: %s" shotPath)

    let saveState () =
        let n = State.saveTo savePath st
        Console.Error.WriteLine(sprintf "save: %s (%d bytes)" savePath n)

    // 状態ファイルには未消費の APU バッファが最大 4096 サンプル (= 93ms) 残っていることがある。
    // そのまま流すと配信の頭で音だけ先行するので捨てる。
    st <- Emulator.clearAudioBuffer st

    // 最初の1フレームを描画させてから配信開始
    st <- Emulator.runFrame st
    drainAudio ()
    writeVideo ()
    n <- 1L
    prevXY <- (px (), py ())
    tracePos "start"

    // ペース制御: 絶対時刻の期限に合わせる。Thread.Sleep は 1ms 単位でしか眠れず
    // 数十 µs 遅れるため、期限の 1.5ms 手前までは Sleep、そこからは spin で詰める。
    // (fsi 版は Sleep 任せで実時間が 0.37% 遅れていた)
    let pace (targetMs: float) =
        let remainMs = targetMs - sw.Elapsed.TotalMilliseconds
        if remainMs > 0.0 then
            if remainMs > 1.5 then Thread.Sleep(int (remainMs - 1.5))
            while sw.Elapsed.TotalMilliseconds < targetMs do
                Thread.SpinWait(64)

    while true do
        let mutable cmd = ""
        while q.TryDequeue(&cmd) do
            let trimmed = cmd.Trim()
            if trimmed = "shot" then
                saveShot ()
            elif trimmed = "save" then
                saveState ()
            else
                let parts = trimmed.Split(' ')
                if parts.Length = 3 && parts.[0].Trim() = "walk" then
                    // walk <方向> <マス数>: 座標が実際にそのマス数ぶん変わるまで押し続ける
                    match Int32.TryParse(parts.[2].Trim()) with
                    | true, k when k > 0 -> actions.Enqueue(Walk(parts.[1].Trim(), k))
                    | _ -> ()
                else
                    let parts2 = trimmed.Split(':')
                    if parts2.Length = 2 then
                        let b = parts2.[0].Trim()
                        match Int32.TryParse(parts2.[1].Trim()) with
                        | true, frames when frames > 0 ->
                            actions.Enqueue(if b = "" || b = "-" then Wait frames else Press(b, frames))
                        | _ -> ()
        st <- Emulator.runFrame st

        // 座標の変化を監視する (walk の判定と移動履歴の記録)
        let xy = (px (), py ())
        // マップ遷移中はオブジェクト座標が (0,0) に落ちるので、無効値として無視する
        // (これを移動と数えると walk が止まらず、ドアを入り続ける等の誤動作になる)
        if xy <> (0, 0) then
            if xy <> prevXY then
                // 大きな座標ジャンプ = マップ移動 (各マップは固有の原点を持つ) として区間を分ける
                let (ox, oy) = prevXY
                if abs (fst xy - ox) > 16 || abs (snd xy - oy) > 16 then seg <- seg + 1
                tracePos ""
                prevXY <- xy
                match curAct with
                | Some (Walk _) -> walkDone <- walkDone + 1
                | _ -> ()
            else
                match curAct with
                | Some (Walk _) -> walkStill <- walkStill + 1
                | _ -> ()

        // 進行中のアクションの終了判定
        match curAct with
        | Some (Press _)
        | Some (Wait _) ->
            curLeft <- curLeft - 1
            if curLeft <= 0 then
                if curButton <> "" then
                    st <- Emulator.releaseButton curButton st
                    curButton <- ""
                curAct <- None
        | Some (Walk(b, need)) ->
            if walkDone >= need || walkStill > 120 then
                st <- Emulator.releaseButton curButton st
                // 指示マス数 (want) と実績 (got) を記録する。判定は facing で行う:
                //   facing が指示方向 = 入力は届いた (壁なので動けない) → blocked
                //   facing が別方向     = 入力自体が届いていない (戦闘/メニュー等) → ignored
                let dirOf =
                    match b with
                    | "up" -> 4
                    | "down" -> 0
                    | "left" -> 8
                    | "right" -> 12
                    | _ -> -1
                let f = facing ()
                let status =
                    if walkDone >= need then "done"
                    elif f = dirOf then "blocked"
                    else "ignored"
                Console.Error.WriteLine(
                    sprintf "walk %s want=%d got=%d %s facing=%d" b need walkDone status f)
                tracePos (sprintf "walk %s want=%d got=%d %s" b need walkDone status)
                curButton <- ""
                curAct <- None
        | None -> ()

        // 次のアクションを始める
        if curAct.IsNone && actions.Count > 0 then
            let a = actions.Dequeue()
            curAct <- Some a
            match a with
            | Press(b, f) ->
                st <- Emulator.pressButton b st
                curButton <- b
                curLeft <- f
            | Walk(b, _) ->
                st <- Emulator.pressButton b st
                curButton <- b
                walkDone <- 0
                walkStill <- 0
                prevXY <- (px (), py ())
            | Wait f -> curLeft <- f
        writeVideo ()
        drainAudio ()
        n <- n + 1L
        if n % 600L = 0L then
            Console.Error.WriteLine(
                sprintf "frames=%d 実時間=%.2fs 実効=%.3f fps (目標 %.4f)"
                    n sw.Elapsed.TotalSeconds (float n / sw.Elapsed.TotalSeconds) fps)
        pace (float n * frameMs)

    0
