namespace gbfs.McpServer

open System
open System.ComponentModel
open System.IO
open System.Net.Http
open System.Threading.Tasks
open ModelContextProtocol.Server
open gbfs.Lib

/// Mutable emulator state held in-process
module EmulatorHolder =
    let mutable state = Emulator.create()
    /// 最後にロードした ROM のパス (.sav の保存先を決めるのに使う)
    let mutable romPath: string option = None

module FrameRelay =
    let private client = new HttpClient()
    let private baseUrl =
        match Environment.GetEnvironmentVariable("GBFS_SERVER_URL") with
        | null | "" -> "http://localhost:5032"
        | url -> url

    let postFrame () : Task =
        task {
            try
                let fb = Emulator.getFrameBuffer EmulatorHolder.state
                use content = new ByteArrayContent(fb)
                let! _ = client.PostAsync(baseUrl + "/api/mcp/frame", content)
                ()
            with _ -> ()
        }

[<McpServerToolType>]
type EmulatorTools() =

    [<McpServerTool>]
    [<Description("Load a ROM file into the emulator. Resets any previous state. Restores .sav (battery-backed cartridge RAM) if present next to the ROM.")>]
    static member load_rom([<Description("Absolute path to the .gb ROM file")>] path: string) : string =
        try
            let rom = File.ReadAllBytes(path)
            // カートのヘッダ 0x0143 bit7 = 1 なら CGB マシンで起動する (CGB 専用カートは DMG では動かない)
            let machine =
                if rom.Length > 0x0143 && (rom.[0x0143] &&& 0x80uy) <> 0uy then Memory.Cgb
                else Memory.Dmg
            EmulatorHolder.state <- Emulator.loadRomWithSave path (Emulator.createWithMachine machine)
            EmulatorHolder.romPath <- Some path
            let savPath = Emulator.saveRamPath path
            let savNote =
                if Emulator.hasBattery EmulatorHolder.state && File.Exists savPath then
                    sprintf " | restored %s" (Path.GetFileName savPath)
                else ""
            sprintf "ROM loaded: %s (%d bytes) | machine %s | mapper %s%s%s"
                (Path.GetFileName path) rom.Length (Memory.machineName machine)
                (Emulator.getMapperName EmulatorHolder.state)
                (if Emulator.hasBattery EmulatorHolder.state then " +battery" else "")
                savNote
        with ex ->
            sprintf "Error loading ROM: %s" ex.Message

    [<McpServerTool>]
    [<Description("Write battery-backed cartridge RAM to <rom>.sav. No-op (returns a message) for ROMs without a battery.")>]
    static member save_ram() : string =
        match EmulatorHolder.romPath with
        | None -> "No ROM loaded"
        | Some path ->
            if not (Emulator.hasBattery EmulatorHolder.state) then
                sprintf "%s has no battery-backed RAM; nothing to save" (Path.GetFileName path)
            else
                let savPath = Emulator.saveRamPath path
                File.WriteAllBytes(savPath, Emulator.getSaveRam EmulatorHolder.state)
                sprintf "Saved %d bytes to %s" (Emulator.getSaveRam EmulatorHolder.state).Length (Path.GetFileName savPath)

    [<McpServerTool>]
    [<Description("Reset the emulator to initial state (clears ROM)")>]
    static member reset() : string =
        EmulatorHolder.state <- Emulator.reset EmulatorHolder.state
        EmulatorHolder.romPath <- None
        "Emulator reset"

    [<McpServerTool>]
    [<Description("Run the emulator for N frames (default 1)")>]
    static member run_frames([<Description("Number of frames to run (default 1)")>] count: int) : Task<string> =
        task {
            let n = if count <= 0 then 1 else count
            EmulatorHolder.state <- Emulator.runFrames n EmulatorHolder.state
            do! FrameRelay.postFrame()
            let s = EmulatorHolder.state
            return sprintf "Ran %d frame(s). Total frames: %d, Total cycles: %d" n s.FrameCount s.TotalCycles
        }

    [<McpServerTool>]
    [<Description("Press buttons, run N frames, then release. Simulates a button press action.")>]
    static member press_buttons(
        [<Description("Button names: up, down, left, right, a, b, start, select")>] buttons: string array,
        [<Description("Number of frames to hold the buttons (default 1)")>] frames: int) : Task<string> =
        task {
            let n = if frames <= 0 then 1 else frames
            // Press buttons
            for b in buttons do
                EmulatorHolder.state <- Emulator.pressButton b EmulatorHolder.state
            // Run frames
            EmulatorHolder.state <- Emulator.runFrames n EmulatorHolder.state
            // Release buttons
            for b in buttons do
                EmulatorHolder.state <- Emulator.releaseButton b EmulatorHolder.state
            do! FrameRelay.postFrame()
            let buttonStr = String.Join(", ", buttons)
            return sprintf "Pressed [%s] for %d frame(s). Frame: %d" buttonStr n EmulatorHolder.state.FrameCount
        }

    [<McpServerTool>]
    [<Description("Execute a batch of button presses in one call. Each step is \"buttons[:hold_frames[:wait_frames]]\" where buttons are joined with '+' (e.g. \"a:15:105\", \"down+start:10:60\"). Use 'repeat' to run the whole sequence N times (e.g. one step \"a:15:105\" with repeat=20 = 20 dialog advances in a single call). Returns the frames advanced and a screen hash, so the caller can tell whether the screen actually changed.")>]
    static member press_sequence(
        [<Description("Steps, e.g. [\"a:15:105\"; \"down:10:30\"]")>] steps: string array,
        [<Description("Repeat the whole sequence this many times (default 1)")>] repeat: int) : Task<string> =
        task {
            let reps = if repeat <= 0 then 1 else repeat
            let parsed =
                steps
                |> Array.map (fun s ->
                    let parts = s.Split(':')
                    let buttons = parts.[0].Split('+') |> Array.filter (fun b -> b <> "")
                    let hold =
                        if parts.Length > 1 then
                            match Int32.TryParse parts.[1] with
                            | true, v when v > 0 -> v
                            | _ -> 1
                        else 1
                    let wait =
                        if parts.Length > 2 then
                            match Int32.TryParse parts.[2] with
                            | true, v when v >= 0 -> v
                            | _ -> 0
                        else 0
                    (buttons, hold, wait))
            let mutable frames = 0
            for _ in 1 .. reps do
                for (buttons, hold, wait) in parsed do
                    for b in buttons do
                        EmulatorHolder.state <- Emulator.pressButton b EmulatorHolder.state
                    EmulatorHolder.state <- Emulator.runFrames hold EmulatorHolder.state
                    for b in buttons do
                        EmulatorHolder.state <- Emulator.releaseButton b EmulatorHolder.state
                    if wait > 0 then
                        EmulatorHolder.state <- Emulator.runFrames wait EmulatorHolder.state
                    frames <- frames + hold + wait
            do! FrameRelay.postFrame()
            let s = EmulatorHolder.state
            let fb = Emulator.getFrameBuffer s
            let nonBlank = fb |> Array.filter (fun v -> v <> 0uy) |> Array.length
            let hash =
                System.Security.Cryptography.SHA256.HashData(fb)
                |> Convert.ToHexString
                |> fun h -> h.Substring(0, 12)
            return
                sprintf "Executed %d step(s) x%d = %d frames. Frame: %d | screen hash: %s | non-blank pixels: %d"
                    parsed.Length reps frames s.FrameCount hash nonBlank
        }

    [<McpServerTool>]
    [<Description("Write the current screen to a PNG file (160x144, grayscale) and return its path. Use this when a vision-capable model needs to read the screen (menus, dialog text).")>]
    static member screenshot_png([<Description("Output path of the PNG file")>] path: string) : string =
        try
            let fb = Emulator.getFrameBuffer EmulatorHolder.state
            let png = Png.encodeFrameBuffer 160 144 fb
            let full = Path.GetFullPath path
            let dir = Path.GetDirectoryName full
            if not (String.IsNullOrEmpty dir) then
                Directory.CreateDirectory dir |> ignore
            File.WriteAllBytes(full, png)
            sprintf "Wrote %s (%d bytes, 160x144 grayscale) at frame %d" full png.Length EmulatorHolder.state.FrameCount
        with ex ->
            sprintf "Error writing screenshot: %s" ex.Message

    [<McpServerTool>]
    [<Description("Get the current screen as ASCII art (160x144)")>]
    static member get_screen() : string =
        Emulator.getScreenAsText EmulatorHolder.state

    [<McpServerTool>]
    [<Description("Get the current emulator state: registers, flags, frame count, and PC location")>]
    static member get_state() : string =
        let s = EmulatorHolder.state
        let regs = Emulator.getRegisters s
        let flags = Emulator.getFlags s
        let af = regs.["AF"]
        let bc = regs.["BC"]
        let de = regs.["DE"]
        let hl = regs.["HL"]
        let pc = regs.["PC"]
        let sp = regs.["SP"]
        let z = flags.["Z"]
        let n = flags.["N"]
        let h = flags.["H"]
        let c = flags.["C"]
        let lines = [
            sprintf "Frame: %d" s.FrameCount
            sprintf "Total Cycles: %d" s.TotalCycles
            sprintf "Halted: %b" s.Cpu.Halted
            sprintf "IME: %b" s.Cpu.Ime
            ""
            "Registers:"
            sprintf "  AF = 0x%04X" af
            sprintf "  BC = 0x%04X" bc
            sprintf "  DE = 0x%04X" de
            sprintf "  HL = 0x%04X" hl
            sprintf "  PC = 0x%04X" pc
            sprintf "  SP = 0x%04X" sp
            ""
            "Flags:"
            sprintf "  Z=%b N=%b H=%b C=%b" z n h c
        ]
        String.Join("\n", lines)

    [<McpServerTool>]
    [<Description("Read memory at a given address and return a hex dump")>]
    static member read_memory(
        [<Description("Start address (0x0000-0xFFFF)")>] address: int,
        [<Description("Number of bytes to read (max 256)")>] length: int) : string =
        let len = min (max length 1) 256
        let addr = uint16 (address &&& 0xFFFF)
        let data = Emulator.readMemory addr len EmulatorHolder.state
        let hexLines =
            data
            |> Array.chunkBySize 16
            |> Array.mapi (fun i chunk ->
                let offset = address + i * 16
                let hex = chunk |> Array.map (fun b -> sprintf "%02X" b) |> String.concat " "
                let ascii =
                    chunk
                    |> Array.map (fun b -> if b >= 0x20uy && b <= 0x7Euy then char b else '.')
                    |> String.Concat
                sprintf "%04X: %-48s %s" offset hex ascii)
        String.Join("\n", hexLines)
