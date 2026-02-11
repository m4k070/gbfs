namespace gbfs.Lib

module Joypad =

    // ============================================================
    // Joypad Register Address
    // ============================================================

    /// JOYP/P1 register (0xFF00)
    ///   Bit 7-6: Not used
    ///   Bit 5:   Select Action buttons    (0=Select)
    ///   Bit 4:   Select Direction buttons  (0=Select)
    ///   Bit 3:   Down  or Start   (0=Pressed)
    ///   Bit 2:   Up    or Select  (0=Pressed)
    ///   Bit 1:   Left  or B       (0=Pressed)
    ///   Bit 0:   Right or A       (0=Pressed)
    let JOYP = 0xFF00us

    // ============================================================
    // Types
    // ============================================================

    type JoypadState = {
        /// Direction buttons
        Right: bool
        Left: bool
        Up: bool
        Down: bool
        /// Action buttons
        A: bool
        B: bool
        Select: bool
        Start: bool
        /// Previously read input bits (for interrupt detection)
        PreviousInput: byte
    }

    // ============================================================
    // Initialization
    // ============================================================

    let create () = {
        Right = false
        Left = false
        Up = false
        Down = false
        A = false
        B = false
        Select = false
        Start = false
        PreviousInput = 0x0Fuy
    }

    // ============================================================
    // Button press/release
    // ============================================================

    let pressButton (button: string) (state: JoypadState) : JoypadState =
        match button with
        | "right"  -> { state with Right = true }
        | "left"   -> { state with Left = true }
        | "up"     -> { state with Up = true }
        | "down"   -> { state with Down = true }
        | "a"      -> { state with A = true }
        | "b"      -> { state with B = true }
        | "select" -> { state with Select = true }
        | "start"  -> { state with Start = true }
        | _ -> state

    let releaseButton (button: string) (state: JoypadState) : JoypadState =
        match button with
        | "right"  -> { state with Right = false }
        | "left"   -> { state with Left = false }
        | "up"     -> { state with Up = false }
        | "down"   -> { state with Down = false }
        | "a"      -> { state with A = false }
        | "b"      -> { state with B = false }
        | "select" -> { state with Select = false }
        | "start"  -> { state with Start = false }
        | _ -> state

    // ============================================================
    // Register read logic
    // ============================================================

    /// Computes the JOYP register value based on the select bits
    /// written by the game and the current button state.
    let readJoyp (joypad: JoypadState) (mem: Memory.MemoryBus) : byte =
        let selectBits = Memory.read JOYP mem
        let selectAction = (selectBits &&& 0x20uy) = 0uy    // Bit 5 low = select action
        let selectDirection = (selectBits &&& 0x10uy) = 0uy  // Bit 4 low = select direction

        let mutable inputBits = 0x0Fuy // All unpressed (high)

        if selectDirection then
            if joypad.Right then inputBits <- inputBits &&& ~~~0x01uy
            if joypad.Left  then inputBits <- inputBits &&& ~~~0x02uy
            if joypad.Up    then inputBits <- inputBits &&& ~~~0x04uy
            if joypad.Down  then inputBits <- inputBits &&& ~~~0x08uy

        if selectAction then
            if joypad.A      then inputBits <- inputBits &&& ~~~0x01uy
            if joypad.B      then inputBits <- inputBits &&& ~~~0x02uy
            if joypad.Select then inputBits <- inputBits &&& ~~~0x04uy
            if joypad.Start  then inputBits <- inputBits &&& ~~~0x08uy

        // Combine: upper bits from select + lower bits from input
        (selectBits &&& 0x30uy) ||| inputBits ||| 0xC0uy

    // ============================================================
    // Memory sync and interrupt
    // ============================================================

    /// Updates the JOYP register in memory and checks for joypad interrupt.
    /// Should be called each step to keep memory in sync with button state.
    let sync (joypad: JoypadState) (mem: Memory.MemoryBus) : JoypadState * Memory.MemoryBus =
        let joypValue = readJoyp joypad mem
        let inputBits = joypValue &&& 0x0Fuy
        let newMem = Memory.write JOYP joypValue mem

        // Joypad interrupt: triggered when any input bit goes from high to low
        // (i.e., a button is newly pressed while that row is selected)
        let newMem =
            if joypad.PreviousInput <> inputBits then
                // Check if any bit went from 1 to 0 (button pressed)
                let newlyPressed = joypad.PreviousInput &&& (~~~inputBits)
                if newlyPressed <> 0uy then
                    Memory.requestInterrupt Memory.JoypadInterrupt newMem
                else
                    newMem
            else
                newMem

        ({ joypad with PreviousInput = inputBits }, newMem)
