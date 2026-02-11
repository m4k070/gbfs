namespace gbfs.Lib

module Apu =

    // ============================================================
    // APU I/O Register Addresses
    // ============================================================

    // Channel 1: Square wave with sweep
    let NR10 = 0xFF10us // Sweep period, negate, shift
    let NR11 = 0xFF11us // Duty, Length load (64-L)
    let NR12 = 0xFF12us // Starting volume, Envelope add mode, period
    let NR13 = 0xFF13us // Frequency LSB
    let NR14 = 0xFF14us // Trigger, Length enable, Frequency MSB

    // Channel 2: Square wave
    let NR21 = 0xFF16us // Duty, Length load
    let NR22 = 0xFF17us // Starting volume, Envelope add mode, period
    let NR23 = 0xFF18us // Frequency LSB
    let NR24 = 0xFF19us // Trigger, Length enable, Frequency MSB

    // Channel 3: Wave output
    let NR30 = 0xFF1Aus // DAC power
    let NR31 = 0xFF1Bus // Length load (256-L)
    let NR32 = 0xFF1Cus // Volume code
    let NR33 = 0xFF1Dus // Frequency LSB
    let NR34 = 0xFF1Eus // Trigger, Length enable, Frequency MSB

    // Channel 4: Noise
    let NR41 = 0xFF20us // Length load (64-L)
    let NR42 = 0xFF21us // Starting volume, Envelope add mode, period
    let NR43 = 0xFF22us // Clock shift, Width mode of LFSR, Divisor code
    let NR44 = 0xFF23us // Trigger, Length enable

    // Master control
    let NR50 = 0xFF24us // Vin L enable, Left vol, Vin R enable, Right vol
    let NR51 = 0xFF25us // Left enables, Right enables
    let NR52 = 0xFF26us // Power control/status, Channel length statuses

    // Wave RAM
    let WAVE_RAM_START = 0xFF30us
    let WAVE_RAM_END   = 0xFF3Fus

    // ============================================================
    // Constants
    // ============================================================

    let SAMPLE_RATE = 44100
    let CPU_CLOCK = 4194304
    let SAMPLE_BUFFER_SIZE = 4096
    let FRAME_SEQ_PERIOD = 8192 // CPU cycles per frame sequencer tick (512 Hz)

    /// Duty cycle waveforms (8 steps each)
    /// 0: 12.5% - 00000001
    /// 1: 25%   - 00000011
    /// 2: 50%   - 00001111
    /// 3: 75%   - 11111100
    let private dutyTable = [|
        [| 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 1uy |] // 12.5%
        [| 1uy; 0uy; 0uy; 0uy; 0uy; 0uy; 0uy; 1uy |] // 25%
        [| 1uy; 0uy; 0uy; 0uy; 0uy; 1uy; 1uy; 1uy |] // 50%
        [| 0uy; 1uy; 1uy; 1uy; 1uy; 1uy; 1uy; 0uy |] // 75%
    |]

    /// Noise channel divisor lookup table
    let private noiseDivisors = [| 8; 16; 32; 48; 64; 80; 96; 112 |]

    // ============================================================
    // Channel Types
    // ============================================================

    type SquareChannel = {
        Enabled: bool
        DacEnabled: bool
        DutyPattern: int          // 0-3
        LengthCounter: int        // カウントダウン
        LengthEnabled: bool
        Volume: int               // 0-15
        VolumeEnvInitial: int
        VolumeEnvDirection: int   // 1=増加, -1=減少
        VolumeEnvPeriod: int
        VolumeEnvTimer: int
        FrequencyTimer: int       // 周波数タイマー
        Frequency: int            // 11bit 周波数値
        DutyPosition: int         // 0-7
        // Sweep (Ch1 only)
        SweepEnabled: bool
        SweepPeriod: int
        SweepTimer: int
        SweepShift: int
        SweepNegate: bool
        SweepShadowFreq: int
    }

    type WaveChannel = {
        Enabled: bool
        DacEnabled: bool
        LengthCounter: int
        LengthEnabled: bool
        VolumeCode: int           // 0-3 (output level shift)
        FrequencyTimer: int
        Frequency: int
        SamplePosition: int       // 0-31 (position in wave RAM)
        SampleBuffer: byte        // current sample value
    }

    type NoiseChannel = {
        Enabled: bool
        DacEnabled: bool
        LengthCounter: int
        LengthEnabled: bool
        Volume: int
        VolumeEnvInitial: int
        VolumeEnvDirection: int
        VolumeEnvPeriod: int
        VolumeEnvTimer: int
        FrequencyTimer: int
        ClockShift: int
        WidthMode: bool           // true=7bit LFSR, false=15bit LFSR
        DivisorCode: int
        Lfsr: uint16              // Linear Feedback Shift Register
    }

    type ApuState = {
        Enabled: bool              // NR52 bit 7
        FrameSequencerCycles: int  // フレームシーケンサ用サイクルカウンタ
        FrameSequencerStep: int    // 0-7
        Channel1: SquareChannel
        Channel2: SquareChannel
        Channel3: WaveChannel
        Channel4: NoiseChannel
        SampleTimer: int           // サンプル生成タイマー
        SampleBuffer: float array  // オーディオ出力バッファ (interleaved L/R)
        SampleBufferPos: int
    }

    // ============================================================
    // Initialization
    // ============================================================

    let private createSquareChannel () = {
        Enabled = false
        DacEnabled = false
        DutyPattern = 0
        LengthCounter = 0
        LengthEnabled = false
        Volume = 0
        VolumeEnvInitial = 0
        VolumeEnvDirection = 0
        VolumeEnvPeriod = 0
        VolumeEnvTimer = 0
        FrequencyTimer = 0
        Frequency = 0
        DutyPosition = 0
        SweepEnabled = false
        SweepPeriod = 0
        SweepTimer = 0
        SweepShift = 0
        SweepNegate = false
        SweepShadowFreq = 0
    }

    let private createWaveChannel () = {
        Enabled = false
        DacEnabled = false
        LengthCounter = 0
        LengthEnabled = false
        VolumeCode = 0
        FrequencyTimer = 0
        Frequency = 0
        SamplePosition = 0
        SampleBuffer = 0uy
    }

    let private createNoiseChannel () = {
        Enabled = false
        DacEnabled = false
        LengthCounter = 0
        LengthEnabled = false
        Volume = 0
        VolumeEnvInitial = 0
        VolumeEnvDirection = 0
        VolumeEnvPeriod = 0
        VolumeEnvTimer = 0
        FrequencyTimer = 0
        ClockShift = 0
        WidthMode = false
        DivisorCode = 0
        Lfsr = 0x7FFFus
    }

    let create () = {
        Enabled = false
        FrameSequencerCycles = 0
        FrameSequencerStep = 0
        Channel1 = createSquareChannel ()
        Channel2 = createSquareChannel ()
        Channel3 = createWaveChannel ()
        Channel4 = createNoiseChannel ()
        SampleTimer = 0
        SampleBuffer = Array.zeroCreate (SAMPLE_BUFFER_SIZE * 2) // L/R interleaved
        SampleBufferPos = 0
    }

    // ============================================================
    // Channel tick functions
    // ============================================================

    let private tickSquareChannel (ch: SquareChannel) : SquareChannel =
        let timer = ch.FrequencyTimer - 1
        if timer <= 0 then
            let newTimer = (2048 - ch.Frequency) * 4
            { ch with FrequencyTimer = newTimer; DutyPosition = (ch.DutyPosition + 1) % 8 }
        else
            { ch with FrequencyTimer = timer }

    let private tickWaveChannel (ch: WaveChannel) (mem: Memory.MemoryBus) : WaveChannel =
        let timer = ch.FrequencyTimer - 1
        if timer <= 0 then
            let newTimer = (2048 - ch.Frequency) * 2
            let newPos = (ch.SamplePosition + 1) % 32
            // Wave RAM: each byte contains 2 samples (upper nibble first)
            let byteAddr = WAVE_RAM_START + uint16 (newPos / 2)
            let waveByte = Memory.read byteAddr mem
            let sample =
                if newPos % 2 = 0 then (waveByte >>> 4) &&& 0x0Fuy
                else waveByte &&& 0x0Fuy
            { ch with FrequencyTimer = newTimer; SamplePosition = newPos; SampleBuffer = sample }
        else
            { ch with FrequencyTimer = timer }

    let private tickNoiseChannel (ch: NoiseChannel) : NoiseChannel =
        let timer = ch.FrequencyTimer - 1
        if timer <= 0 then
            let divisor = noiseDivisors.[ch.DivisorCode]
            let newTimer = divisor <<< ch.ClockShift
            // LFSR feedback
            let xorResult = (ch.Lfsr &&& 1us) ^^^ ((ch.Lfsr >>> 1) &&& 1us)
            let mutable newLfsr = (ch.Lfsr >>> 1) ||| (xorResult <<< 14)
            if ch.WidthMode then
                // 7-bit mode: also set bit 6
                newLfsr <- (newLfsr &&& ~~~(1us <<< 6)) ||| (xorResult <<< 6)
            { ch with FrequencyTimer = newTimer; Lfsr = newLfsr }
        else
            { ch with FrequencyTimer = timer }

    // ============================================================
    // Frame sequencer components
    // ============================================================

    let private tickLengthSquare (ch: SquareChannel) : SquareChannel =
        if ch.LengthEnabled && ch.LengthCounter > 0 then
            let newLen = ch.LengthCounter - 1
            if newLen = 0 then
                { ch with LengthCounter = 0; Enabled = false }
            else
                { ch with LengthCounter = newLen }
        else
            ch

    let private tickLengthWave (ch: WaveChannel) : WaveChannel =
        if ch.LengthEnabled && ch.LengthCounter > 0 then
            let newLen = ch.LengthCounter - 1
            if newLen = 0 then
                { ch with LengthCounter = 0; Enabled = false }
            else
                { ch with LengthCounter = newLen }
        else
            ch

    let private tickLengthNoise (ch: NoiseChannel) : NoiseChannel =
        if ch.LengthEnabled && ch.LengthCounter > 0 then
            let newLen = ch.LengthCounter - 1
            if newLen = 0 then
                { ch with LengthCounter = 0; Enabled = false }
            else
                { ch with LengthCounter = newLen }
        else
            ch

    let private tickVolumeEnvelopeSquare (ch: SquareChannel) : SquareChannel =
        if ch.VolumeEnvPeriod = 0 then ch
        else
            let timer = ch.VolumeEnvTimer - 1
            if timer <= 0 then
                let newVol = ch.Volume + ch.VolumeEnvDirection
                if newVol >= 0 && newVol <= 15 then
                    { ch with Volume = newVol; VolumeEnvTimer = ch.VolumeEnvPeriod }
                else
                    { ch with VolumeEnvTimer = ch.VolumeEnvPeriod }
            else
                { ch with VolumeEnvTimer = timer }

    let private tickVolumeEnvelopeNoise (ch: NoiseChannel) : NoiseChannel =
        if ch.VolumeEnvPeriod = 0 then ch
        else
            let timer = ch.VolumeEnvTimer - 1
            if timer <= 0 then
                let newVol = ch.Volume + ch.VolumeEnvDirection
                if newVol >= 0 && newVol <= 15 then
                    { ch with Volume = newVol; VolumeEnvTimer = ch.VolumeEnvPeriod }
                else
                    { ch with VolumeEnvTimer = ch.VolumeEnvPeriod }
            else
                { ch with VolumeEnvTimer = timer }

    let private calculateSweepFrequency (ch: SquareChannel) : int =
        let shifted = ch.SweepShadowFreq >>> ch.SweepShift
        if ch.SweepNegate then ch.SweepShadowFreq - shifted
        else ch.SweepShadowFreq + shifted

    let private tickSweep (ch: SquareChannel) : SquareChannel =
        if not ch.SweepEnabled then ch
        else
            let timer = ch.SweepTimer - 1
            if timer <= 0 then
                let newTimer = if ch.SweepPeriod > 0 then ch.SweepPeriod else 8
                if ch.SweepPeriod > 0 then
                    let newFreq = calculateSweepFrequency ch
                    if newFreq > 2047 then
                        { ch with Enabled = false; SweepTimer = newTimer }
                    elif ch.SweepShift > 0 then
                        // Write new frequency and do overflow check again
                        let ch2 = { ch with Frequency = newFreq; SweepShadowFreq = newFreq; SweepTimer = newTimer }
                        let overflowCheck = calculateSweepFrequency ch2
                        if overflowCheck > 2047 then
                            { ch2 with Enabled = false }
                        else
                            ch2
                    else
                        { ch with SweepTimer = newTimer }
                else
                    { ch with SweepTimer = newTimer }
            else
                { ch with SweepTimer = timer }

    // ============================================================
    // Channel trigger functions
    // ============================================================

    let private triggerSquareChannel (ch: SquareChannel) (nr2: byte) (freqLo: byte) (freqHi: byte) (hasSweep: bool) (nr10: byte) : SquareChannel =
        let freq = int freqLo ||| ((int freqHi &&& 0x07) <<< 8)
        let volInitial = int (nr2 >>> 4)
        let envDir = if (nr2 &&& 0x08uy) <> 0uy then 1 else -1
        let envPeriod = int (nr2 &&& 0x07uy)
        let dacEnabled = (nr2 &&& 0xF8uy) <> 0uy
        let lengthEnabled = (freqHi &&& 0x40uy) <> 0uy
        let lengthCounter = if ch.LengthCounter = 0 then 64 else ch.LengthCounter

        let mutable newCh = { ch with
                                Enabled = dacEnabled
                                DacEnabled = dacEnabled
                                Frequency = freq
                                FrequencyTimer = (2048 - freq) * 4
                                Volume = volInitial
                                VolumeEnvInitial = volInitial
                                VolumeEnvDirection = envDir
                                VolumeEnvPeriod = envPeriod
                                VolumeEnvTimer = envPeriod
                                LengthCounter = lengthCounter
                                LengthEnabled = lengthEnabled }

        if hasSweep then
            let sweepPeriod = int ((nr10 >>> 4) &&& 0x07uy)
            let sweepNegate = (nr10 &&& 0x08uy) <> 0uy
            let sweepShift = int (nr10 &&& 0x07uy)
            newCh <- { newCh with
                        SweepPeriod = sweepPeriod
                        SweepShift = sweepShift
                        SweepNegate = sweepNegate
                        SweepShadowFreq = freq
                        SweepTimer = if sweepPeriod > 0 then sweepPeriod else 8
                        SweepEnabled = (sweepPeriod > 0 || sweepShift > 0) }
            // Overflow check on trigger
            if sweepShift > 0 then
                let newFreq = calculateSweepFrequency newCh
                if newFreq > 2047 then
                    newCh <- { newCh with Enabled = false }

        newCh

    let private triggerWaveChannel (ch: WaveChannel) (freqLo: byte) (freqHi: byte) (nr30: byte) : WaveChannel =
        let freq = int freqLo ||| ((int freqHi &&& 0x07) <<< 8)
        let dacEnabled = (nr30 &&& 0x80uy) <> 0uy
        let lengthEnabled = (freqHi &&& 0x40uy) <> 0uy
        let lengthCounter = if ch.LengthCounter = 0 then 256 else ch.LengthCounter

        { ch with
            Enabled = dacEnabled
            DacEnabled = dacEnabled
            Frequency = freq
            FrequencyTimer = (2048 - freq) * 2
            SamplePosition = 0
            LengthCounter = lengthCounter
            LengthEnabled = lengthEnabled }

    let private triggerNoiseChannel (ch: NoiseChannel) (nr42: byte) (nr43: byte) (nr44: byte) : NoiseChannel =
        let volInitial = int (nr42 >>> 4)
        let envDir = if (nr42 &&& 0x08uy) <> 0uy then 1 else -1
        let envPeriod = int (nr42 &&& 0x07uy)
        let dacEnabled = (nr42 &&& 0xF8uy) <> 0uy
        let clockShift = int (nr43 >>> 4)
        let widthMode = (nr43 &&& 0x08uy) <> 0uy
        let divisorCode = int (nr43 &&& 0x07uy)
        let lengthEnabled = (nr44 &&& 0x40uy) <> 0uy
        let lengthCounter = if ch.LengthCounter = 0 then 64 else ch.LengthCounter
        let divisor = noiseDivisors.[divisorCode]

        { ch with
            Enabled = dacEnabled
            DacEnabled = dacEnabled
            Volume = volInitial
            VolumeEnvInitial = volInitial
            VolumeEnvDirection = envDir
            VolumeEnvPeriod = envPeriod
            VolumeEnvTimer = envPeriod
            ClockShift = clockShift
            WidthMode = widthMode
            DivisorCode = divisorCode
            FrequencyTimer = divisor <<< clockShift
            Lfsr = 0x7FFFus
            LengthCounter = lengthCounter
            LengthEnabled = lengthEnabled }

    // ============================================================
    // Channel output
    // ============================================================

    let private getSquareOutput (ch: SquareChannel) : int =
        if not ch.Enabled || not ch.DacEnabled then 0
        else
            let dutyOut = dutyTable.[ch.DutyPattern].[ch.DutyPosition]
            if dutyOut = 1uy then ch.Volume else 0

    let private getWaveOutput (ch: WaveChannel) : int =
        if not ch.Enabled || not ch.DacEnabled then 0
        else
            let sample = int ch.SampleBuffer
            match ch.VolumeCode with
            | 0 -> 0              // mute
            | 1 -> sample         // 100%
            | 2 -> sample >>> 1   // 50%
            | 3 -> sample >>> 2   // 25%
            | _ -> 0

    let private getNoiseOutput (ch: NoiseChannel) : int =
        if not ch.Enabled || not ch.DacEnabled then 0
        else
            // Output is inverted bit 0 of LFSR
            let bit = int (~~~ch.Lfsr &&& 1us)
            bit * ch.Volume

    // ============================================================
    // Register read helpers (for trigger detection)
    // ============================================================

    let private readChannelRegisters (mem: Memory.MemoryBus) =
        let nr10 = Memory.read NR10 mem
        let nr11 = Memory.read NR11 mem
        let nr12 = Memory.read NR12 mem
        let nr13 = Memory.read NR13 mem
        let nr14 = Memory.read NR14 mem
        let nr21 = Memory.read NR21 mem
        let nr22 = Memory.read NR22 mem
        let nr23 = Memory.read NR23 mem
        let nr24 = Memory.read NR24 mem
        let nr30 = Memory.read NR30 mem
        let nr31 = Memory.read NR31 mem
        let nr32 = Memory.read NR32 mem
        let nr33 = Memory.read NR33 mem
        let nr34 = Memory.read NR34 mem
        let nr42 = Memory.read NR42 mem
        let nr43 = Memory.read NR43 mem
        let nr44 = Memory.read NR44 mem
        (nr10, nr11, nr12, nr13, nr14,
         nr21, nr22, nr23, nr24,
         nr30, nr31, nr32, nr33, nr34,
         nr42, nr43, nr44)

    // ============================================================
    // Step function
    // ============================================================

    let step (cycles: int) (apuState: ApuState) (mem: Memory.MemoryBus) : ApuState * Memory.MemoryBus =
        let nr52 = Memory.read NR52 mem
        let enabled = (nr52 &&& 0x80uy) <> 0uy

        if not enabled then
            // APU disabled: clear all state except length counters
            ({ apuState with Enabled = false }, mem)
        else

        // Read registers for trigger detection
        let (nr10, nr11, nr12, nr13, nr14,
             _nr21, nr22, nr23, nr24,
             nr30, _nr31, nr32, nr33, nr34,
             nr42, nr43, nr44) = readChannelRegisters mem

        let mutable state = { apuState with Enabled = true }
        let mutable newMem = mem

        // Check for channel triggers (NRx4 bit 7)
        if (nr14 &&& 0x80uy) <> 0uy then
            let duty = int (nr11 >>> 6)
            let lenLoad = int (nr11 &&& 0x3Fuy)
            let ch = { state.Channel1 with DutyPattern = duty; LengthCounter = if state.Channel1.LengthCounter = 0 then 64 - lenLoad else state.Channel1.LengthCounter }
            state <- { state with Channel1 = triggerSquareChannel ch nr12 nr13 nr14 true nr10 }
            newMem <- Memory.write NR14 (nr14 &&& 0x7Fuy) newMem // Clear trigger bit

        if (nr24 &&& 0x80uy) <> 0uy then
            let duty = int (_nr21 >>> 6)
            let lenLoad = int (_nr21 &&& 0x3Fuy)
            let ch = { state.Channel2 with DutyPattern = duty; LengthCounter = if state.Channel2.LengthCounter = 0 then 64 - lenLoad else state.Channel2.LengthCounter }
            state <- { state with Channel2 = triggerSquareChannel ch nr22 nr23 nr24 false 0uy }
            newMem <- Memory.write NR24 (nr24 &&& 0x7Fuy) newMem

        if (nr34 &&& 0x80uy) <> 0uy then
            let lenLoad = int _nr31
            let volCode = int ((nr32 >>> 5) &&& 0x03uy)
            let ch = { state.Channel3 with VolumeCode = volCode; LengthCounter = if state.Channel3.LengthCounter = 0 then 256 - lenLoad else state.Channel3.LengthCounter }
            state <- { state with Channel3 = triggerWaveChannel ch nr33 nr34 nr30 }
            newMem <- Memory.write NR34 (nr34 &&& 0x7Fuy) newMem

        if (nr44 &&& 0x80uy) <> 0uy then
            let lenLoad = int (Memory.read NR41 newMem &&& 0x3Fuy)
            let ch = { state.Channel4 with LengthCounter = if state.Channel4.LengthCounter = 0 then 64 - lenLoad else state.Channel4.LengthCounter }
            state <- { state with Channel4 = triggerNoiseChannel ch nr42 nr43 nr44 }
            newMem <- Memory.write NR44 (nr44 &&& 0x7Fuy) newMem

        // Process cycles one at a time
        for _ in 1 .. cycles do
            // Tick channel frequency timers
            state <- { state with
                        Channel1 = tickSquareChannel state.Channel1
                        Channel3 = tickWaveChannel state.Channel3 newMem
                        Channel4 = tickNoiseChannel state.Channel4 }
            state <- { state with Channel2 = tickSquareChannel state.Channel2 }

            // Frame sequencer
            let fsCycles = state.FrameSequencerCycles + 1
            if fsCycles >= FRAME_SEQ_PERIOD then
                let fsStep = state.FrameSequencerStep
                // Length counter: steps 0, 2, 4, 6
                let ch1, ch2, ch3, ch4 =
                    if fsStep % 2 = 0 then
                        (tickLengthSquare state.Channel1,
                         tickLengthSquare state.Channel2,
                         tickLengthWave state.Channel3,
                         tickLengthNoise state.Channel4)
                    else
                        (state.Channel1, state.Channel2, state.Channel3, state.Channel4)

                // Sweep: steps 2, 6
                let ch1 =
                    if fsStep = 2 || fsStep = 6 then tickSweep ch1
                    else ch1

                // Volume envelope: step 7
                let ch1, ch2, ch4 =
                    if fsStep = 7 then
                        (tickVolumeEnvelopeSquare ch1,
                         tickVolumeEnvelopeSquare ch2,
                         tickVolumeEnvelopeNoise ch4)
                    else
                        (ch1, ch2, ch4)

                state <- { state with
                            Channel1 = ch1
                            Channel2 = ch2
                            Channel3 = ch3
                            Channel4 = ch4
                            FrameSequencerCycles = 0
                            FrameSequencerStep = (fsStep + 1) % 8 }
            else
                state <- { state with FrameSequencerCycles = fsCycles }

            // Sample generation
            let sampleTimer = state.SampleTimer + SAMPLE_RATE
            if sampleTimer >= CPU_CLOCK then
                let sampleTimer = sampleTimer - CPU_CLOCK
                state <- { state with SampleTimer = sampleTimer }

                if state.SampleBufferPos < SAMPLE_BUFFER_SIZE * 2 then
                    let ch1Out = float (getSquareOutput state.Channel1)
                    let ch2Out = float (getSquareOutput state.Channel2)
                    let ch3Out = float (getWaveOutput state.Channel3)
                    let ch4Out = float (getNoiseOutput state.Channel4)

                    let nr50 = Memory.read NR50 newMem
                    let nr51 = Memory.read NR51 newMem

                    let leftVol = float ((int (nr50 >>> 4) &&& 0x07) + 1)
                    let rightVol = float ((int nr50 &&& 0x07) + 1)

                    // Mix channels based on NR51 panning
                    let mutable left = 0.0
                    let mutable right = 0.0

                    if (nr51 &&& 0x10uy) <> 0uy then left <- left + ch1Out
                    if (nr51 &&& 0x20uy) <> 0uy then left <- left + ch2Out
                    if (nr51 &&& 0x40uy) <> 0uy then left <- left + ch3Out
                    if (nr51 &&& 0x80uy) <> 0uy then left <- left + ch4Out

                    if (nr51 &&& 0x01uy) <> 0uy then right <- right + ch1Out
                    if (nr51 &&& 0x02uy) <> 0uy then right <- right + ch2Out
                    if (nr51 &&& 0x04uy) <> 0uy then right <- right + ch3Out
                    if (nr51 &&& 0x08uy) <> 0uy then right <- right + ch4Out

                    // Apply master volume and normalize
                    // Max per channel = 15, 4 channels = 60, volume 1-8 = 480
                    left <- (left * leftVol) / 480.0
                    right <- (right * rightVol) / 480.0

                    state.SampleBuffer.[state.SampleBufferPos] <- left
                    state.SampleBuffer.[state.SampleBufferPos + 1] <- right
                    state <- { state with SampleBufferPos = state.SampleBufferPos + 2 }
            else
                state <- { state with SampleTimer = sampleTimer }

        // Update NR52 status bits (bits 0-3 = channel enabled status)
        let statusBits =
            (if state.Channel1.Enabled then 0x01uy else 0uy) |||
            (if state.Channel2.Enabled then 0x02uy else 0uy) |||
            (if state.Channel3.Enabled then 0x04uy else 0uy) |||
            (if state.Channel4.Enabled then 0x08uy else 0uy)
        let newNr52 = (nr52 &&& 0xF0uy) ||| statusBits
        newMem <- Memory.write NR52 newNr52 newMem

        (state, newMem)
