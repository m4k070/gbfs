module gbfs.Desktop.Main

#nowarn "9"

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Media.Imaging
open Avalonia.Platform
open Avalonia.Rendering
open Avalonia.Threading
open Microsoft.FSharp.NativeInterop
open gbfs.Lib

// ============================================================
// AudioOut — Lib の APU サンプル (44100Hz L/R float) を OpenAL へ queue
// ============================================================

module AudioOut =
    open Silk.NET.OpenAL

    type T =
        | Unavailable of reason: string
        | Ready of
            al: AL *
            source: uint *
            buffers: uint array

    let create () : T =
        try
            let alc = ALContext.GetApi()
            let device = alc.OpenDevice(null)
            if obj.ReferenceEquals(device, null) then
                Unavailable "no OpenAL device"
            else
                let ctx = alc.CreateContext(device, NativePtr.ofNativeInt 0n)
                alc.MakeContextCurrent(ctx)
                let al = AL.GetApi()
                let source = al.GenSource()
                let buffers = Array.init 4 (fun _ -> al.GenBuffer())
                // 再生開始直後に processed が無いと止まるため、無音で初期キュー
                let silence = Array.zeroCreate<int16> 1470 // 1 tick (16.6ms) 分
                for b in buffers do
                    al.BufferData(b, BufferFormat.Stereo16, silence, Apu.SAMPLE_RATE)
                al.SourceQueueBuffers(source, buffers)
                al.SourcePlay(source)
                Ready(al, source, buffers)
        with ex ->
            Unavailable ex.Message

    let private toPcm16 (samples: float array) : int16[] =
        Array.init samples.Length (fun i ->
            let v = max -1.0 (min 1.0 samples.[i])
            int16 (v * 32000.0))

    /// processed バッファを回収して新しいサンプルで埋め直す
    let queue (t: T) (samples: float array) : unit =
        match t with
        | Unavailable _ -> ()
        | Ready(al, source, _) when samples.Length > 0 ->
            let mutable processed = 0
            al.GetSourceProperty(source, GetSourceInteger.BuffersProcessed, &processed)
            if processed > 0 then
                let pcm = toPcm16 samples
                let recycled = Array.zeroCreate<uint32> processed
                al.SourceUnqueueBuffers(source, recycled)
                for b in recycled do
                    al.BufferData(b, BufferFormat.Stereo16, pcm, Apu.SAMPLE_RATE)
                    al.SourceQueueBuffers(source, [| b |])
            let mutable v = 0
            al.GetSourceProperty(source, GetSourceInteger.SourceState, &v)
            if enum<SourceState> v <> SourceState.Playing then
                al.SourcePlay(source)
        | _ -> ()

    let dispose (t: T) : unit =
        match t with
        | Unavailable _ -> ()
        | Ready(al, source, buffers) ->
            try
                al.SourceStop(source)
                for b in buffers do al.DeleteBuffer(b)
                al.DeleteSource(source)
            with _ -> ()

// ============================================================
// MVU (Model / Message / update) — Main.fs から移植可能な形
// ============================================================

/// DMG 4 shades (緑パレット)
let private palette =
    [| 0xFF9BBC0Fu   // 0: 白
       0xFF8BAC0Fu   // 1: 薄緑
       0xFF306230u   // 2: 濃緑
       0xFF0F380Fu |] // 3: 黒

type Model =
    { State: Emulator.EmulatorState
      RomPath: string option
      Running: bool
      Error: string option }

type Msg =
    | Start
    | Stop
    | Tick
    | Press of button: string
    | Release of button: string

let private mapKey (key: string) : string option =
    match key with
    | "Up" -> Some "up"
    | "Down" -> Some "down"
    | "Left" -> Some "left"
    | "Right" -> Some "right"
    | "Z" -> Some "a"
    | "X" -> Some "b"
    | "Enter" -> Some "start"
    | "RightShift" | "LeftShift" -> Some "select"
    | _ -> None

let tryLoadRom (path: string) (model: Model) : Model =
    try
        let rom = File.ReadAllBytes(path)
        { model with
            State = Emulator.loadRom rom (Emulator.create ())
            RomPath = Some path
            Error = None
            Running = false }
    with ex ->
        { model with Error = Some $"ROM load failed: %s{ex.Message}" }

let update (audio: AudioOut.T) (msg: Msg) (model: Model) : Model =
    match msg with
    | Start when model.RomPath.IsSome -> { model with Running = true }
    | Stop -> { model with Running = false }
    | Tick when model.Running ->
        let s = Emulator.runFrame model.State
        let samples = Emulator.getAudioBuffer s
        AudioOut.queue audio samples
        { model with State = Emulator.clearAudioBuffer s }
    | Press button when model.Running ->
        { model with State = Emulator.pressButton button model.State }
    | Release button when model.Running ->
        { model with State = Emulator.releaseButton button model.State }
    | _ -> model

// ============================================================
// View — 素の Avalonia (FuncUI なし)
// ============================================================

type MainWindow() as this =
    inherit Window()

    let mutable model =
        { State = Emulator.create ()
          RomPath = None
          Running = false
          Error = None }

    // 160x144 BGRA フレームバッファ (framebuffer の byte 0..3 → palette)
    let bitmap =
        new WriteableBitmap(
            PixelSize(160, 144),
            Vector(96.0, 96.0),
            PixelFormat.Bgra8888,
            AlphaFormat.Opaque)

    let screen = Image()
    do RenderOptions.SetBitmapInterpolationMode(screen, BitmapInterpolationMode.None)
    do screen.Source <- bitmap
    do screen.Width <- 480.0   // 3x
    do screen.Height <- 432.0

    let statusText = TextBlock()
    let errorText = TextBlock(Foreground = Brushes.IndianRed)
    let startBtn = Button(Content = "Start")
    let stopBtn = Button(Content = "Stop")

    let audio = AudioOut.create ()

    let renderFrame () =
        let fb = Emulator.getFrameBuffer model.State
        if fb.Length >= 160 * 144 then
            let locked = bitmap.Lock()
            try
                let dst = NativePtr.ofNativeInt<byte> locked.Address
                for i in 0 .. (160 * 144 - 1) do
                    let color = palette.[int fb.[i] &&& 3]
                    NativePtr.set dst (i * 4 + 0) (byte (color &&& 0xFFu))
                    NativePtr.set dst (i * 4 + 1) (byte ((color >>> 8) &&& 0xFFu))
                    NativePtr.set dst (i * 4 + 2) (byte ((color >>> 16) &&& 0xFFu))
                    NativePtr.set dst (i * 4 + 3) 0xFFuy
            finally
                locked.Dispose()
            screen.InvalidateVisual()

    let refresh () =
        statusText.Text <-
            match model.RomPath with
            | Some p ->
                let st = model.State
                (if model.Running then "Running" else "Stopped")
                + $" | Frame %d{st.FrameCount} | PC=0x%04X{st.Cpu.Regs.PC}"
                + $" | %s{Path.GetFileName p}"
            | None -> "No ROM (pass a .gb path as argv[0], or put test.gb in cwd)"
        errorText.Text <-
            [ model.Error |> Option.defaultValue ""
              match audio with
              | AudioOut.Unavailable r -> $"audio unavailable: %s{r}"
              | _ -> "" ]
            |> List.filter (fun s -> s <> "")
            |> String.concat " | "

    let dispatch msg =
        let wasRunning = model.Running
        model <- update audio msg model
        if msg = Tick && model.Running then renderFrame ()
        if (msg <> Tick) || (model.Running <> wasRunning) then refresh ()

    // 60fps ゲームループ
    let timer =
        DispatcherTimer(TimeSpan.FromMilliseconds(16.6), DispatcherPriority.Background, fun _ _ -> dispatch Tick)

    do
        this.Title <- "gbfs (spike: Avalonia)"
        this.Width <- 540.0
        this.Height <- 560.0

        let panel = StackPanel()
        panel.Margin <- Thickness(16.0)
        panel.Spacing <- 8.0
        screen.HorizontalAlignment <- HorizontalAlignment.Center
        panel.Children.Add(screen) |> ignore

        let buttons = StackPanel()
        buttons.Orientation <- Orientation.Horizontal
        buttons.Spacing <- 8.0
        startBtn.Click.Add(fun _ -> dispatch Start)
        stopBtn.Click.Add(fun _ -> dispatch Stop)
        buttons.Children.Add(startBtn) |> ignore
        buttons.Children.Add(stopBtn) |> ignore
        panel.Children.Add(buttons) |> ignore
        panel.Children.Add(statusText) |> ignore
        panel.Children.Add(errorText) |> ignore
        this.Content <- panel

        this.KeyDown.Add(fun e ->
            mapKey (string e.Key)
            |> Option.iter (fun b -> dispatch (Press b)))
        this.KeyUp.Add(fun e ->
            mapKey (string e.Key)
            |> Option.iter (fun b -> dispatch (Release b)))

        // ROM: argv[0] → cwd/test.gb
        let romPath =
            match Environment.GetCommandLineArgs() with
            | [| _; path; _ |] -> Some path
            | [| _; path |] -> Some path
            | _ ->
                let dflt = Path.Combine(Environment.CurrentDirectory, "test.gb")
                if File.Exists dflt then Some dflt else None

        match romPath with
        | Some p when File.Exists p -> model <- tryLoadRom p model
        | Some p -> model <- { model with Error = Some $"not found: %s{p}" }
        | None -> ()

        refresh ()
        // ROM 読込では自動再生しない (起動直後に音が鳴るのを防ぐ) — Start は手動
        refresh ()
        timer.Start()

    override this.OnKeyDown e =
        base.OnKeyDown e
        // Space で Start/Stop トグル (キー入力と干渉しない機能確認用)
        if e.Key = Key.Space then
            dispatch (if model.Running then Stop else Start)

    override _.OnClosed(_) =
        AudioOut.dispose audio
        timer.Stop()

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(Avalonia.Themes.Fluent.FluentTheme())

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop ->
            desktop.MainWindow <- MainWindow()
        | _ -> ()
        base.OnFrameworkInitializationCompleted()

module Program =
    [<EntryPoint>]
    let main argv =
        AppBuilder.Configure<App>()
            .UsePlatformDetect()
            .LogToTrace()
            .StartWithClassicDesktopLifetime(argv)
        |> ignore
        0
