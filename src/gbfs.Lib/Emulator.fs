namespace gbfs.Lib

open Cpu
open Memory
open Ppu
open Apu
open Joypad
open Timer
open Decoder


/// Resolve a CpuKind tag to its concrete CpuCore implementation.
module CpuImpl =
    let ofKind (kind: CpuKind) : CpuCore =
        match kind with
        | Native ->
            { Step = Decoder.step; CreateState = Decoder.createState; LoadRom = Decoder.loadRomToState }
        | WireLevel ->
            WireLevelCpu.core
module Emulator =


    // Serialized state — CpuKind is a DU tag (serializable), not a function record.
    type EmulatorState = {
        Cpu: Decoder.CpuState
        CpuKind: CpuKind
        FrameCount: int
        TotalCycles: int64
    }

    /// Resolve the CPU implementation for this state.
    let private implOf (state: EmulatorState) = CpuImpl.ofKind state.CpuKind

    // ============================================================
    // Lifecycle
    // ============================================================

    /// Create a new emulator state with the native F# SM83 CPU.
    let create () =
        let impl = CpuImpl.ofKind Native
        { Cpu = impl.CreateState()
          CpuKind = Native
          FrameCount = 0
          TotalCycles = 0L }

    /// Create a new emulator state with the specified CPU kind.
    let createWith (kind: CpuKind) =
        let impl = CpuImpl.ofKind kind
        { Cpu = impl.CreateState()
          CpuKind = kind
          FrameCount = 0
          TotalCycles = 0L }

    /// マシン種別 (DMG / CGB) を指定してエミュレータを作る。
    /// 実 CGB カートを動かすときは Memory.Cgb を渡す
    /// (カート側の CGB 対応はヘッダ 0x0143 bit7 で判定され、CGB モードが解禁される)
    let createWithMachine (machine: Memory.MachineMode) =
        { Cpu = Decoder.createStateFor machine ()
          CpuKind = Native
          FrameCount = 0
          TotalCycles = 0L }

    let loadRom (rom: byte array) (state: EmulatorState) =
        { state with Cpu = (implOf state).LoadRom rom state.Cpu }

    // ============================================================
    // Cartridge RAM (バッテリーバックアップ)
    // ============================================================

    /// カートリッジの外部 RAM (バッテリー付きカートなら .sav の保存対象)
    let getSaveRam (state: EmulatorState) : byte array = state.Cpu.Mem.ExtRam

    /// この ROM がバッテリーバックアップを持つか (ROM ヘッダ 0x0147)
    let hasBattery (state: EmulatorState) = state.Cpu.Mem.Mbc.HasBattery

    /// マッパー名 (ROM / MBC1 / MBC2 / MBC3 / MBC5)
    let getMapperName (state: EmulatorState) = mbcTypeName state.Cpu.Mem.Mbc.MbcType

    /// 外部 RAM を差し替える (ROM の RAM サイズに合わせて切り詰め / 0 埋め)
    let setSaveRam (data: byte array) (state: EmulatorState) =
        let mem = state.Cpu.Mem
        let ram = Array.zeroCreate mem.ExtRam.Length
        Array.blit data 0 ram 0 (min data.Length ram.Length)
        { state with Cpu = { state.Cpu with Mem = { mem with ExtRam = ram } } }

    /// ROM パスに対応する .sav のパス (拡張子だけ差し替え)
    let saveRamPath (romPath: string) =
        System.IO.Path.ChangeExtension(romPath, ".sav")

    /// ROM を読み込み、バッテリー付きカートなら隣接する .sav を復元する
    let loadRomWithSave (romPath: string) (state: EmulatorState) =
        let state = loadRom (System.IO.File.ReadAllBytes romPath) state
        let path = saveRamPath romPath
        if hasBattery state && System.IO.File.Exists path then
            setSaveRam (System.IO.File.ReadAllBytes path) state
        else state

    /// バッテリー付きカートの外部 RAM を .sav に書き出す。書き出したら true。
    let saveRam (romPath: string) (state: EmulatorState) =
        if hasBattery state then
            let path = saveRamPath romPath
            let data = getSaveRam state
            System.IO.File.WriteAllBytes(path, data)
            true
        else false

    let reset (state: EmulatorState) =
        // マシン種別は維持する (CGB のセッションが DMG に戻らないように)
        { state with
            Cpu = Decoder.createStateFor state.Cpu.Mem.Machine ()
            FrameCount = 0
            TotalCycles = 0L }

    // ============================================================
    // Execution
    // ============================================================

    /// Execute a single CPU step (one instruction or one HALT cycle)
    let step (state: EmulatorState) =
        let newCpu = (implOf state).Step state.Cpu
        { state with Cpu = newCpu }

    /// Run for one frame (until VBlank, ~70224 cycles).
    /// Detects frame boundary by watching LY wrap from 143+ back to 0.
    let runFrame (state: EmulatorState) =
        let impl = implOf state
        let mutable cpu = state.Cpu
        let mutable steps = 0
        let maxSteps = 70224
        let mutable passedVBlank = false

        while steps < maxSteps && not passedVBlank do
            let oldLY = cpu.Ppu.LY
            cpu <- impl.Step cpu
            steps <- steps + 1
            if oldLY > 0uy && cpu.Ppu.LY = 0uy then
                passedVBlank <- true

        { state with
            Cpu = cpu
            FrameCount = state.FrameCount + 1
            TotalCycles = state.TotalCycles + int64 (steps * 4) }

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

    /// 実際の色 (RGB888、160 * 144 * 3 バイト) のフレームバッファ。CGB のカラー出力用 (Issue #19)
    let getRgbFrameBuffer (state: EmulatorState) : byte array =
        state.Cpu.Ppu.RgbFrameBuffer

    /// Get the screen as ASCII art (for MCP / text-based consumption)
    let getScreenAsText (state: EmulatorState) : string =
        let fb = state.Cpu.Ppu.FrameBuffer
        let sb = System.Text.StringBuilder()
        for y in 0..143 do
            for x in 0..159 do
                let pixel = fb.[y * 160 + x]
                let ch =
                    match pixel with
                    | 0uy -> ' '
                    | 1uy -> '.'
                    | 2uy -> 'o'
                    | _   -> '#'
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
