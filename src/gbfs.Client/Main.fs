module gbfs.Client.Main

open System
open Bolero
open Bolero.Html
open Elmish
open Microsoft.JSInterop
open Microsoft.AspNetCore.Components
open Microsoft.AspNetCore.Components.Web
open gbfs.Lib

// ====================
// The Elmish Application
// ====================

type Model = {
    EmulatorState: Decoder.CpuState
    RomLoaded: bool
    Running: bool
    Error: string option
    FrameCount: int
}

type Message =
    | LoadRomClicked
    | RomLoaded of byte[]
    | StartEmulator
    | StopEmulator
    | RunFrame
    | SetError of string
    | Reset

let init () : Model * Cmd<Message> =
    {
        EmulatorState = Decoder.createState()
        RomLoaded = false
        Running = false
        Error = None
        FrameCount = 0
    }, Cmd.none

let update (jsRuntime: IJSRuntime) (msg: Message) (model: Model) : Model * Cmd<Message> =
    match msg with
    | LoadRomClicked ->
        let loadRomTask = async {
            let! hasFile = jsRuntime.InvokeAsync<bool>("hasFileSelected").AsTask() |> Async.AwaitTask
            if not hasFile then
                return SetError "Please select a ROM file first"
            else
                let! base64Data = jsRuntime.InvokeAsync<string>("readRomFile").AsTask() |> Async.AwaitTask
                let romData = Convert.FromBase64String(base64Data)
                return RomLoaded romData
        }
        model, Cmd.OfAsync.perform (fun () -> loadRomTask) () id

    | RomLoaded rom ->
        let newState = Decoder.loadRomToState rom model.EmulatorState
        { model with EmulatorState = newState; RomLoaded = true; Running = true; Error = None; FrameCount = 0 }, Cmd.ofMsg RunFrame

    | StartEmulator ->
        if model.RomLoaded then
            { model with Running = true }, Cmd.ofMsg RunFrame
        else
            model, Cmd.none

    | StopEmulator ->
        { model with Running = false }, Cmd.none

    | Reset ->
        { model with
            EmulatorState = Decoder.createState()
            RomLoaded = false
            Running = false
            Error = None
            FrameCount = 0 }, Cmd.none

    | RunFrame ->
        if model.Running && model.RomLoaded then
            let cyclesPerFrame = 70224
            let newState = Decoder.run cyclesPerFrame model.EmulatorState

            let frameTask = async {
                do! jsRuntime.InvokeVoidAsync("drawScreen", newState.Ppu.FrameBuffer).AsTask() |> Async.AwaitTask
                do! jsRuntime.InvokeVoidAsync("requestAnimationFrameAsync").AsTask() |> Async.AwaitTask
                return RunFrame
            }
            let frameCmd = Cmd.OfAsync.perform (fun () -> frameTask) () id

            { model with EmulatorState = newState; FrameCount = model.FrameCount + 1 }, frameCmd
        else
            model, Cmd.none

    | SetError msg ->
        { model with Error = Some msg; Running = false }, Cmd.none

let view (model: Model) (dispatch: Message -> unit) =
    div {
        attr.style "font-family: sans-serif; padding: 20px; max-width: 600px; margin: 0 auto;"

        h1 {
            attr.style "color: #2c3e50; margin-bottom: 20px; text-align: center;"
            "gbfs - Game Boy Emulator"
        }

        div {
            attr.style "display: flex; flex-direction: column; align-items: center; gap: 15px;"

            // File input for ROM loading
            div {
                attr.style "display: flex; align-items: center; gap: 10px;"
                label {
                    attr.``for`` "rom-input"
                    attr.style "font-weight: bold;"
                    "ROM File: "
                }
                input {
                    attr.id "rom-input"
                    attr.``type`` "file"
                    attr.accept ".gb,.gbc,.bin"
                    attr.style "padding: 5px;"
                }
            }

            // Control buttons
            div {
                attr.style "display: flex; gap: 10px;"

                button {
                    attr.style "padding: 10px 20px; font-size: 14px; cursor: pointer; background: #3498db; color: white; border: none; border-radius: 5px;"
                    on.click (fun _ -> dispatch LoadRomClicked)
                    "Load ROM"
                }
                button {
                    attr.disabled (not model.RomLoaded || model.Running)
                    attr.style "padding: 10px 20px; font-size: 14px; cursor: pointer; background: #27ae60; color: white; border: none; border-radius: 5px;"
                    on.click (fun _ -> dispatch StartEmulator)
                    "Start"
                }
                button {
                    attr.disabled (not model.Running)
                    attr.style "padding: 10px 20px; font-size: 14px; cursor: pointer; background: #e74c3c; color: white; border: none; border-radius: 5px;"
                    on.click (fun _ -> dispatch StopEmulator)
                    "Stop"
                }
                button {
                    attr.style "padding: 10px 20px; font-size: 14px; cursor: pointer; background: #95a5a6; color: white; border: none; border-radius: 5px;"
                    on.click (fun _ -> dispatch Reset)
                    "Reset"
                }
            }

            // Canvas for emulator display
            div {
                attr.style "border: 4px solid #2c3e50; border-radius: 8px; overflow: hidden; box-shadow: 0 4px 6px rgba(0,0,0,0.1);"
                canvas {
                    attr.id "emulator-screen"
                    attr.width 160
                    attr.height 144
                    attr.style "display: block; width: 320px; height: 288px; image-rendering: pixelated; background: #9bbc0f;"
                }
            }

            // Status
            div {
                attr.style "font-size: 14px; color: #7f8c8d; text-align: center;"
                if model.RomLoaded then
                    if model.Running then
                        text $"Status: Running (Frame: {model.FrameCount})"
                    else
                        text "Status: ROM loaded, press Start to begin"
                else
                    text "Status: Select a ROM file and click Load ROM"
            }

            // Error display
            cond model.Error <| function
                | Some err ->
                    div {
                        attr.style "color: #e74c3c; margin-top: 10px; padding: 10px; background: #fde8e8; border-radius: 5px;"
                        text $"Error: {err}"
                    }
                | None -> empty()
        }
    }

// ====================
// Bolero Component
// ====================

type EmulatorApp() =
    inherit ProgramComponent<Model, Message>()

    [<Inject>]
    member val JSRuntime: IJSRuntime = Unchecked.defaultof<_> with get, set

    override this.Program =
        let update = update this.JSRuntime
        Program.mkProgram (fun _ -> init()) update view
