namespace gbfs.Lib

module Timer =

    // ============================================================
    // Timer Register Addresses
    // ============================================================

    let DIV  = 0xFF04us // Divider Register (increments at 16384 Hz)
    let TIMA = 0xFF05us // Timer Counter
    let TMA  = 0xFF06us // Timer Modulo (reload value)
    let TAC  = 0xFF07us // Timer Control

    // ============================================================
    // Constants
    // ============================================================

    /// The internal 16-bit counter increments every CPU cycle (4194304 Hz).
    /// DIV is the upper 8 bits of this counter (increments at 16384 Hz = every 256 cycles).
    /// TIMA increment rate is selected by TAC bits 0-1:
    ///   00: 4096 Hz   (every 1024 cycles) - bit 9 of internal counter
    ///   01: 262144 Hz (every 16 cycles)   - bit 3 of internal counter
    ///   10: 65536 Hz  (every 64 cycles)   - bit 5 of internal counter
    ///   11: 16384 Hz  (every 256 cycles)  - bit 7 of internal counter
    let private timaRateBits = [| 9; 3; 5; 7 |]

    // ============================================================
    // Types
    // ============================================================

    type TimerState = {
        /// Internal 16-bit counter. DIV = upper 8 bits (counter >>> 8).
        InternalCounter: uint16
        /// Tracks the previous state of the selected TIMA bit for falling-edge detection.
        PreviousTiBit: bool
        /// Whether TIMA overflow occurred (reload TMA next cycle).
        OverflowPending: bool
    }

    // ============================================================
    // Initialization
    // ============================================================

    let create () = {
        InternalCounter = 0us
        PreviousTiBit = false
        OverflowPending = false
    }

    // ============================================================
    // Helpers
    // ============================================================

    /// Gets the bit position of the internal counter used for TIMA increment.
    let private getTimaBitPos (tac: byte) : int =
        timaRateBits.[int (tac &&& 0x03uy)]

    /// Checks if the selected bit of the internal counter is set.
    let private getTimaBit (counter: uint16) (tac: byte) : bool =
        let bitPos = getTimaBitPos tac
        (counter >>> bitPos) &&& 1us = 1us

    // ============================================================
    // Step function
    // ============================================================

    /// Advances the timer by the given number of CPU cycles.
    /// Handles DIV increment, TIMA increment with falling-edge detection,
    /// TIMA overflow → TMA reload, and timer interrupt.
    let step (cycles: int) (timerState: TimerState) (mem: Memory.MemoryBus) : TimerState * Memory.MemoryBus =
        let mutable state = timerState
        let mutable newMem = mem

        // Check if DIV was written to (reset to 0)
        // We detect this by comparing memory DIV value with expected value
        let memDiv = Memory.read DIV newMem
        let expectedDiv = byte (state.InternalCounter >>> 8)
        if memDiv <> expectedDiv then
            // DIV was written to by the CPU - reset internal counter
            state <- { state with InternalCounter = 0us }
            newMem <- Memory.write DIV 0uy newMem

        let tac = Memory.read TAC newMem
        let timerEnabled = (tac &&& 0x04uy) <> 0uy

        for _ in 1 .. cycles do
            let oldCounter = state.InternalCounter
            let newCounter = oldCounter + 1us
            state <- { state with InternalCounter = newCounter }

            // Handle TIMA overflow from previous cycle
            if state.OverflowPending then
                let tma = Memory.read TMA newMem
                newMem <- Memory.write TIMA tma newMem
                newMem <- Memory.requestInterrupt Memory.TimerInterrupt newMem
                state <- { state with OverflowPending = false }

            // Falling-edge detection for TIMA increment
            // TIMA increments when the selected bit goes from 1→0 AND timer is enabled
            let currentTiBit = timerEnabled && getTimaBit newCounter tac
            if state.PreviousTiBit && not currentTiBit then
                // Falling edge detected - increment TIMA
                let tima = Memory.read TIMA newMem
                if tima = 0xFFuy then
                    // Overflow: set to 0, schedule TMA reload for next cycle
                    newMem <- Memory.write TIMA 0x00uy newMem
                    state <- { state with OverflowPending = true }
                else
                    newMem <- Memory.write TIMA (tima + 1uy) newMem

            state <- { state with PreviousTiBit = currentTiBit }

        // Update DIV in memory
        newMem <- Memory.write DIV (byte (state.InternalCounter >>> 8)) newMem

        (state, newMem)
