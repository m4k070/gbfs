namespace gbfs.Lib

open Cpu
open Memory
open Ppu
open Apu
open Joypad
open Timer
open Decoder

module Emulator =

    type EmulatorState = {
        Cpu: Decoder.CpuState
        FrameCount: int
        TotalCycles: int64
    }

    // ============================================================
    // Lifecycle
    // ============================================================

    let create () = {
        Cpu = Decoder.createState()
        FrameCount = 0
        TotalCycles = 0L
    }

    let loadRom (rom: byte array) (state: EmulatorState) =
        { state with Cpu = Decoder.loadRomToState rom state.Cpu }

    let reset (state: EmulatorState) =
        { state with
            Cpu = Decoder.createState()
            FrameCount = 0
            TotalCycles = 0L }

    // ============================================================
    // Execution
    // ============================================================

    /// Execute a single CPU step (one instruction or one HALT cycle)
    let step (state: EmulatorState) =
        let newCpu = Decoder.step state.Cpu
        { state with Cpu = newCpu }

    /// Run for one frame (until VBlank, ~70224 cycles).
    /// Detects frame boundary by watching LY wrap from 143+ back to 0.
    let runFrame (state: EmulatorState) =
        let mutable cpu = state.Cpu
        let mutable steps = 0
        let maxSteps = 70224 // safety limit (1 step = at least 4 cycles, so max ~17556 instructions)
        let prevLY = cpu.Ppu.LY
        let mutable passedVBlank = false

        while steps < maxSteps && not passedVBlank do
            let oldLY = cpu.Ppu.LY
            cpu <- Decoder.step cpu
            steps <- steps + 1
            // Detect when LY wraps from 143 (end of visible) or higher back to scanline 0
            // This indicates a new frame has started
            if oldLY > 0uy && cpu.Ppu.LY = 0uy then
                passedVBlank <- true

        { state with
            Cpu = cpu
            FrameCount = state.FrameCount + 1
            TotalCycles = state.TotalCycles + int64 (steps * 4) } // approximate

    /// Run N frames
    let runFrames (count: int) (state: EmulatorState) =
        let mutable s = state
        for _ in 1..count do
            s <- runFrame s
        s

    // ============================================================
    // Input
    // ============================================================

    let pressButton (button: string) (state: EmulatorState) =
        let newJoypad = Joypad.pressButton button state.Cpu.Joypad
        { state with Cpu = { state.Cpu with Joypad = newJoypad } }

    let releaseButton (button: string) (state: EmulatorState) =
        let newJoypad = Joypad.releaseButton button state.Cpu.Joypad
        { state with Cpu = { state.Cpu with Joypad = newJoypad } }

    // ============================================================
    // Screen
    // ============================================================

    /// Get the raw frame buffer (160x144, values 0-3)
    let getFrameBuffer (state: EmulatorState) : byte array =
        state.Cpu.Ppu.FrameBuffer

    /// Get the screen as ASCII art (for MCP / text-based consumption)
    let getScreenAsText (state: EmulatorState) : string =
        let fb = state.Cpu.Ppu.FrameBuffer
        let sb = System.Text.StringBuilder()
        for y in 0..143 do
            for x in 0..159 do
                let pixel = fb.[y * 160 + x]
                let ch =
                    match pixel with
                    | 0uy -> ' '   // lightest
                    | 1uy -> '.'   // light
                    | 2uy -> 'o'   // dark
                    | _   -> '#'   // darkest
                sb.Append(ch) |> ignore
            sb.AppendLine() |> ignore
        sb.ToString()

    // ============================================================
    // Audio
    // ============================================================

    /// Get audio samples (L/R interleaved float array)
    let getAudioBuffer (state: EmulatorState) : float array =
        let apu = state.Cpu.Apu
        if apu.SampleBufferPos > 0 then
            apu.SampleBuffer.[0..apu.SampleBufferPos - 1]
        else
            Array.empty

    /// Clear audio buffer after consuming samples
    let clearAudioBuffer (state: EmulatorState) : EmulatorState =
        let newApu = { state.Cpu.Apu with SampleBufferPos = 0 }
        { state with Cpu = { state.Cpu with Apu = newApu } }

    // ============================================================
    // State Inspection
    // ============================================================

    /// Get register values as a map
    let getRegisters (state: EmulatorState) : Map<string, uint16> =
        let regs = state.Cpu.Regs
        Map.ofList [
            "AF", regs.AF
            "BC", regs.BC
            "DE", regs.DE
            "HL", regs.HL
            "PC", regs.PC
            "SP", regs.SP
        ]

    /// Get CPU flags
    let getFlags (state: EmulatorState) : Map<string, bool> =
        let regs = state.Cpu.Regs
        Map.ofList [
            "Z", Cpu.isZ regs
            "N", Cpu.isN regs
            "H", Cpu.isH regs
            "C", Cpu.isC regs
        ]

    /// Read memory at address for given length
    let readMemory (addr: uint16) (length: int) (state: EmulatorState) : byte array =
        [| for i in 0..length-1 do
            Memory.read (addr + uint16 i) state.Cpu.Mem |]

    /// Write bytes to memory starting at address
    let writeMemory (addr: uint16) (data: byte array) (state: EmulatorState) : EmulatorState =
        let mutable mem = state.Cpu.Mem
        for i in 0..data.Length-1 do
            mem <- Memory.write (addr + uint16 i) data.[i] mem
        { state with Cpu = { state.Cpu with Mem = mem } }
