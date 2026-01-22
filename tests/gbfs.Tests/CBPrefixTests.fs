module CBPrefixTests

open Xunit
open gbfs.Lib
open gbfs.Lib.Cpu
open gbfs.Lib.Decoder
open gbfs.Lib.CBPrefix

let initCpuState () =
    let state = createState()
    { state with Regs = { state.Regs with PC = 0us } }

let testCbInstruction (memory: byte list) (initialState: CpuState) (assertions: CpuState -> unit) =
    let rom = List.toArray memory
    let state = loadRomToState rom initialState
    let finalState = step state
    assertions finalState
    
// ====================
// RLC命令テスト
// ====================
[<Fact>]
let ``RLC B (0xCB 0x00)`` () =
    let state = initCpuState()
    let state = { state with Regs = setRegisterValue (R8 B) 0x85us state.Regs } // B = 1000 0101
    testCbInstruction [0xcbuy; 0x00uy] state (fun finalState ->
        Assert.Equal(0x0Bus, getRegisterValue (R8 B) finalState.Regs) // B = 0000 1011
        Assert.True(isC finalState.Regs)
        Assert.False(isZ finalState.Regs)
        Assert.False(isH finalState.Regs)
        Assert.False(isN finalState.Regs)
    )

[<Fact>]
let ``RLC (HL) (0xCB 0x06)`` () =
    let state = initCpuState()
    let state = { state with Regs = setRegisterValue (R16 HL) 0xC000us state.Regs }
    let state = { state with Mem = Memory.write 0xC000us 0x01uy state.Mem } // (HL) = 0000 0001
    testCbInstruction [0xcbuy; 0x06uy] state (fun finalState ->
        let value = Memory.read 0xC000us finalState.Mem
        Assert.Equal(0x02uy, value) // (HL) = 0000 0010
        Assert.False(isC finalState.Regs)
    )

// ====================
// BIT命令テスト
// ====================
[<Fact>]
let ``BIT 7,A (0xCB 0x7F) - bit is 1`` () =
    let state = initCpuState()
    let state = { state with Regs = setRegisterValue (R8 A) 0x80us state.Regs } // A = 1000 0000
    testCbInstruction [0xcbuy; 0x7Fuy] state (fun finalState ->
        Assert.False(isZ finalState.Regs) // Z is 0 if bit is 1
        Assert.False(isN finalState.Regs)
        Assert.True(isH finalState.Regs)
    )
    
[<Fact>]
let ``BIT 0,B (0xCB 0x40) - bit is 0`` () =
    let state = initCpuState()
    let state = { state with Regs = setRegisterValue (R8 B) 0xFEus state.Regs } // B = 1111 1110
    testCbInstruction [0xcbuy; 0x40uy] state (fun finalState ->
        Assert.True(isZ finalState.Regs) // Z is 1 if bit is 0
    )

[<Fact>]
let ``BIT 1,(HL) (0xCB 0x4E)`` () =
    let state = initCpuState()
    let state = { state with Regs = setRegisterValue (R16 HL) 0xC000us state.Regs }
    let state = { state with Mem = Memory.write 0xC000us 0b1111_1101uy state.Mem }
    testCbInstruction [0xcbuy; 0x4Euy] state (fun finalState ->
        Assert.True(isZ finalState.Regs) // Z is 1 because bit 1 is 0
    )
    
// ====================
// RES命令テスト
// ====================
[<Fact>]
let ``RES 0,A (0xCB 0x87)`` () =
    let state = initCpuState()
    let state = { state with Regs = setRegisterValue (R8 A) 0xFFus state.Regs }
    testCbInstruction [0xcbuy; 0x87uy] state (fun finalState ->
        Assert.Equal(0xFEus, getRegisterValue (R8 A) finalState.Regs)
    )

[<Fact>]
let ``RES 7,(HL) (0xCB 0xBE)`` () =
    let state = initCpuState()
    let state = { state with Regs = setRegisterValue (R16 HL) 0xC000us state.Regs }
    let state = { state with Mem = Memory.write 0xC000us 0xFFuy state.Mem }
    testCbInstruction [0xcbuy; 0xBEuy] state (fun finalState ->
        let value = Memory.read 0xC000us finalState.Mem
        Assert.Equal(0x7Fuy, value)
    )

// ====================
// SET命令テスト
// ====================
[<Fact>]
let ``SET 7,A (0xCB 0xFF)`` () =
    let state = initCpuState()
    let state = { state with Regs = setRegisterValue (R8 A) 0x00us state.Regs }
    testCbInstruction [0xcbuy; 0xFFuy] state (fun finalState ->
        Assert.Equal(0x80us, getRegisterValue (R8 A) finalState.Regs)
    )

[<Fact>]
let ``SET 0,(HL) (0xCB 0xC6)`` () =
    let state = initCpuState()
    let state = { state with Regs = setRegisterValue (R16 HL) 0xC000us state.Regs }
    let state = { state with Mem = Memory.write 0xC000us 0x00uy state.Mem }
    testCbInstruction [0xcbuy; 0xC6uy] state (fun finalState ->
        let value = Memory.read 0xC000us finalState.Mem
        Assert.Equal(0x01uy, value)
    )

// ====================
// SWAP命令テスト
// ====================
[<Fact>]
let ``SWAP A (0xCB 0x37)`` () =
    let state = initCpuState()
    let state = { state with Regs = setRegisterValue (R8 A) 0xABus state.Regs }
    testCbInstruction [0xcbuy; 0x37uy] state (fun finalState ->
        Assert.Equal(0xBAus, getRegisterValue (R8 A) finalState.Regs)
        Assert.False(isZ finalState.Regs)
    )

[<Fact>]
let ``SWAP results in zero (0xCB 0x37)`` () =
    let state = initCpuState()
    let state = { state with Regs = setRegisterValue (R8 A) 0x00us state.Regs }
    testCbInstruction [0xcbuy; 0x37uy] state (fun finalState ->
        Assert.Equal(0x00us, getRegisterValue (R8 A) finalState.Regs)
        Assert.True(isZ finalState.Regs)
    )
