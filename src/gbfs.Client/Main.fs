module gbfs.Client.Main

open Bolero
open Bolero.Html
open Elmish
open Fable.Core.JS
open Fable.Web.Dom
open gbfs.Lib

// JS interop to draw the screen
promise {
    let screen = import "drawScreen" "./js/app.js"
    (window :?> obj)?drawScreen <- screen
} |> ignore

let drawScreen (data: byte[]) =
    ((window :?> obj)?drawScreen :?> (byte[] -> unit)) data

// ====================
// The Elmish Application
// ====================

type Model = {
    emulatorState: Decoder.CpuState
    romLoaded: bool
    error: string option
}

type Message =
    | FileSelected of File
    | RomLoaded of byte[]
    | RunEmulator
    | ShowError of string

let readFileAsync (file: File) : Async<byte[]> =
    async {
        let! buffer = (file.As<Blob>()).arrayBuffer() |> Promise.toAsync
        let view = new Uint8Array(buffer)
        return Array.ofTypedArray view
    }

let init (): Model * Cmd<Message> =
    {
        emulatorState = Decoder.createState()
        romLoaded = false
        error = None
    }, Cmd.none

let update (msg: Message) (model: Model): Model * Cmd<Message> =
    match msg with
    | FileSelected file ->
        let cmd = Cmd.OfAsync.perform readFileAsync file RomLoaded (fun ex -> ShowError ex.Message)
        model, cmd

    | RomLoaded rom ->
        let newState = Decoder.loadRomToState rom model.emulatorState
        { model with emulatorState = newState; romLoaded = true; error = None }, Cmd.ofMsg RunEmulator

    | RunEmulator ->
        if model.romLoaded then
            // Schedule the next frame
            let cmd = Cmd.ofSub (fun dispatch -> window.requestAnimationFrame (fun _ -> dispatch RunEmulator) |> ignore)
            
            // Run the emulator for one frame's worth of cycles
            // Clock speed is 4.194304 MHz. Screen refresh is 59.7 Hz.
            // Cycles per frame = 4194304 / 59.7 ~= 70224
            let cyclesPerFrame = 70224
            let newState = Decoder.run cyclesPerFrame model.emulatorState

            // Draw the result
            drawScreen newState.Ppu.FrameBuffer
            
            { model with emulatorState = newState }, cmd
        else
            model, Cmd.none

    | ShowError msg ->
        { model with error = Some msg }, Cmd.none


let view (model: Model) (dispatch: Message -> unit) =
    div {
        h1 { "gbfs - Game Boy Emulator" }
        div {
            style {
                display "flex"
                flexDirection "column"
                alignItems "flex-start"
            }
            
            div {
                style { marginBottom "10px" }
                label { "Load ROM:" }
                input {
                    attr.``type`` "file"
                    on.change (fun ev ->
                        let files = (ev.target :?> HTMLInputElement).files
                        if files.length > 0 then
                            dispatch (FileSelected files.[0]))
                }
            }

            canvas {
                attr.id "emulator-screen"
                attr.width 160
                attr.height 144
                style {
                    border "1px solid black"
                    width "320px"  // Scale up for better viewing
                    height "288px"
                    imageRendering "pixelated" // Crisp pixels
                }
            }
        }
        
        match model.error with
        | Some err -> div { style { color "red"; marginTop "10px" }; str err }
        | None -> empty()
    }

// ====================
// Bolero Setup
// ====================

type MyApp() =
    inherit ProgramComponent<Model, Message>()
    override this.Program =
        Program.mkProgram init update view
        |> Program.withJsInterop
#if DEBUG
        |> Program.withHotReload
#endif
