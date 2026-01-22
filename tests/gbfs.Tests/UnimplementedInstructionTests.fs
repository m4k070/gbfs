/// 実装済み命令のテスト
module UnimplementedInstructionTests

open Xunit
open gbfs.Lib.Cpu
open gbfs.Lib.Decoder
open gbfs.Lib.Memory

let initCpuState () =
    let state = createState()
    { state with Regs = { state.Regs with PC = 0us } }

let testInstruction (memory: byte list) (initialState: CpuState) (assertions: CpuState -> unit) =
    let rom = List.toArray memory
    let state = loadRomToState rom initialState
    let finalState = step state
    assertions finalState

let initRegister () =
    { AF = 0us; BC = 0us; DE = 0us; HL = 0us; PC = 0us; SP = 0us }

// ====================
// ADC (Add with Carry) 命令テスト
// ====================

module AdcTests =
    [<Fact>]
    let ``ADC A,B without carry adds B to A`` () =
        let regs = { initRegister() with AF = 0x1000us; BC = 0x0500us }
        let result = AdcR B regs
        Assert.Equal(0x15us, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``ADC A,B with carry adds B+1 to A`` () =
        let regs = { initRegister() with AF = 0x1010us; BC = 0x0500us } // Cフラグ=1
        let result = AdcR B regs
        Assert.Equal(0x16us, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``ADC sets Z flag when result is zero`` () =
        let regs = { initRegister() with AF = 0xFF10us; BC = 0x0000us } // A=0xFF, C=1
        let result = AdcR B regs
        Assert.True(isZ result)

    [<Fact>]
    let ``ADC sets H flag on half carry`` () =
        let regs = { initRegister() with AF = 0x0F00us; BC = 0x0100us }
        let result = AdcR B regs
        Assert.True(isH result)

    [<Fact>]
    let ``ADC sets C flag on overflow`` () =
        let regs = { initRegister() with AF = 0xFF00us; BC = 0x0100us }
        let result = AdcR B regs
        Assert.True(isC result)

    [<Fact>]
    let ``ADC A,n8 adds immediate with carry`` () =
        let regs = { initRegister() with AF = 0x1010us } // A=0x10, C=1
        let result = AdcN8 0x05us regs
        Assert.Equal(0x16us, getRegisterValue (R8 A) result)

// ====================
// SBC (Subtract with Carry) 命令テスト
// ====================

module SbcTests =
    [<Fact>]
    let ``SBC A,B without carry subtracts B from A`` () =
        let regs = { initRegister() with AF = 0x1000us; BC = 0x0500us }
        let result = SbcR B regs
        Assert.Equal(0x0Bus, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``SBC A,B with carry subtracts B+1 from A`` () =
        let regs = { initRegister() with AF = 0x1010us; BC = 0x0500us } // C=1
        let result = SbcR B regs
        Assert.Equal(0x0Aus, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``SBC sets Z flag when result is zero`` () =
        let regs = { initRegister() with AF = 0x0500us; BC = 0x0500us }
        let result = SbcR B regs
        Assert.True(isZ result)

    [<Fact>]
    let ``SBC sets N flag always`` () =
        let regs = { initRegister() with AF = 0x1000us; BC = 0x0500us }
        let result = SbcR B regs
        Assert.True(isN result)

    [<Fact>]
    let ``SBC sets H flag on half borrow`` () =
        let regs = { initRegister() with AF = 0x1000us; BC = 0x0100us }
        let result = SbcR B regs
        Assert.True(isH result)

    [<Fact>]
    let ``SBC sets C flag on borrow`` () =
        let regs = { initRegister() with AF = 0x0500us; BC = 0x1000us }
        let result = SbcR B regs
        Assert.True(isC result)

// ====================
// AND 命令テスト
// ====================

module AndTests =
    [<Fact>]
    let ``AND B performs bitwise AND`` () =
        let regs = { initRegister() with AF = 0xFF00us; BC = 0x0F00us }
        let result = AndR B regs
        Assert.Equal(0x0Fus, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``AND sets Z flag when result is zero`` () =
        let regs = { initRegister() with AF = 0xF000us; BC = 0x0F00us }
        let result = AndR B regs
        Assert.True(isZ result)

    [<Fact>]
    let ``AND always sets H flag`` () =
        let regs = { initRegister() with AF = 0xFF00us; BC = 0xFF00us }
        let result = AndR B regs
        Assert.True(isH result)

    [<Fact>]
    let ``AND always clears N and C flags`` () =
        let regs = { initRegister() with AF = 0xFF00us; BC = 0xFF00us } |> setFlag false true false true
        let result = AndR B regs
        Assert.False(isN result)
        Assert.False(isC result)

    [<Fact>]
    let ``AND A,n8 performs AND with immediate`` () =
        let regs = { initRegister() with AF = 0xFF00us }
        let result = AndN8 0x0Fus regs
        Assert.Equal(0x0Fus, getRegisterValue (R8 A) result)

// ====================
// OR 命令テスト
// ====================

module OrTests =
    [<Fact>]
    let ``OR B performs bitwise OR`` () =
        let regs = { initRegister() with AF = 0xF000us; BC = 0x0F00us }
        let result = OrR B regs
        Assert.Equal(0xFFus, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``OR sets Z flag when result is zero`` () =
        let regs = { initRegister() with AF = 0x0000us; BC = 0x0000us }
        let result = OrR B regs
        Assert.True(isZ result)

    [<Fact>]
    let ``OR always clears N, H, C flags`` () =
        let regs = { initRegister() with AF = 0xF000us; BC = 0x0F00us } |> setFlag false true true true
        let result = OrR B regs
        Assert.False(isN result)
        Assert.False(isH result)
        Assert.False(isC result)

    [<Fact>]
    let ``OR A,n8 performs OR with immediate`` () =
        let regs = { initRegister() with AF = 0xF000us }
        let result = OrN8 0x0Fus regs
        Assert.Equal(0xFFus, getRegisterValue (R8 A) result)

// ====================
// XOR 命令テスト
// ====================

module XorTests =
    [<Fact>]
    let ``XOR B performs bitwise XOR`` () =
        let regs = { initRegister() with AF = 0xFF00us; BC = 0x0F00us }
        let result = XorR B regs
        Assert.Equal(0xF0us, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``XOR A with itself results in zero`` () =
        let regs = { initRegister() with AF = 0x4200us }
        let result = XorR A regs
        Assert.Equal(0x00us, getRegisterValue (R8 A) result)
        Assert.True(isZ result)

    [<Fact>]
    let ``XOR always clears N, H, C flags`` () =
        let regs = { initRegister() with AF = 0xFF00us; BC = 0x0F00us } |> setFlag false true true true
        let result = XorR B regs
        Assert.False(isN result)
        Assert.False(isH result)
        Assert.False(isC result)

    [<Fact>]
    let ``XOR A,n8 performs XOR with immediate`` () =
        let regs = { initRegister() with AF = 0xFF00us }
        let result = XorN8 0x0Fus regs
        Assert.Equal(0xF0us, getRegisterValue (R8 A) result)

// ====================
// CP (Compare) 命令テスト
// ====================

module CpTests =
    [<Fact>]
    let ``CP B sets Z flag when A equals B`` () =
        let regs = { initRegister() with AF = 0x4200us; BC = 0x4200us }
        let result = CpR B regs
        Assert.True(isZ result)
        Assert.Equal(0x42us, getRegisterValue (R8 A) result) // A unchanged

    [<Fact>]
    let ``CP B sets C flag when A is less than B`` () =
        let regs = { initRegister() with AF = 0x1000us; BC = 0x2000us }
        let result = CpR B regs
        Assert.True(isC result)

    [<Fact>]
    let ``CP B does not set C flag when A is greater than B`` () =
        let regs = { initRegister() with AF = 0x2000us; BC = 0x1000us }
        let result = CpR B regs
        Assert.False(isC result)

    [<Fact>]
    let ``CP always sets N flag`` () =
        let regs = { initRegister() with AF = 0x4200us; BC = 0x4200us }
        let result = CpR B regs
        Assert.True(isN result)

    [<Fact>]
    let ``CP does not modify A register`` () =
        let regs = { initRegister() with AF = 0x4200us; BC = 0x1000us }
        let result = CpR B regs
        Assert.Equal(0x42us, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``CP A,n8 compares with immediate`` () =
        let regs = { initRegister() with AF = 0x4200us }
        let result = CpN8 0x42us regs
        Assert.True(isZ result)

// ====================
// INC r 命令テスト
// ====================

module IncTests =
    [<Fact>]
    let ``INC B increments B by 1`` () =
        let regs = { initRegister() with BC = 0x4200us }
        let result = IncR B regs
        Assert.Equal(0x43us, getRegisterValue (R8 B) result)

    [<Fact>]
    let ``INC B sets Z flag when result is zero`` () =
        let regs = { initRegister() with BC = 0xFF00us }
        let result = IncR B regs
        Assert.Equal(0x00us, getRegisterValue (R8 B) result)
        Assert.True(isZ result)

    [<Fact>]
    let ``INC B sets H flag on half carry`` () =
        let regs = { initRegister() with BC = 0x0F00us }
        let result = IncR B regs
        Assert.True(isH result)

    [<Fact>]
    let ``INC B clears N flag`` () =
        let regs = { initRegister() with BC = 0x4200us } |> setFlag false true false false
        let result = IncR B regs
        Assert.False(isN result)

    [<Fact>]
    let ``INC B does not affect C flag`` () =
        let regs = { initRegister() with BC = 0x4200us } |> setFlag false false false true
        let result = IncR B regs
        Assert.True(isC result)

    [<Fact>]
    let ``INC A increments A register`` () =
        let regs = { initRegister() with AF = 0x4200us }
        let result = IncR A regs
        Assert.Equal(0x43us, getRegisterValue (R8 A) result)

// ====================
// DEC r 命令テスト
// ====================

module DecTests =
    [<Fact>]
    let ``DEC B decrements B by 1`` () =
        let regs = { initRegister() with BC = 0x4200us }
        let result = DecR B regs
        Assert.Equal(0x41us, getRegisterValue (R8 B) result)

    [<Fact>]
    let ``DEC B sets Z flag when result is zero`` () =
        let regs = { initRegister() with BC = 0x0100us }
        let result = DecR B regs
        Assert.Equal(0x00us, getRegisterValue (R8 B) result)
        Assert.True(isZ result)

    [<Fact>]
    let ``DEC B wraps from 0 to 0xFF`` () =
        let regs = { initRegister() with BC = 0x0000us }
        let result = DecR B regs
        Assert.Equal(0xFFus, getRegisterValue (R8 B) result)

    [<Fact>]
    let ``DEC B sets H flag on half borrow`` () =
        let regs = { initRegister() with BC = 0x1000us }
        let result = DecR B regs
        Assert.True(isH result)

    [<Fact>]
    let ``DEC B always sets N flag`` () =
        let regs = { initRegister() with BC = 0x4200us }
        let result = DecR B regs
        Assert.True(isN result)

    [<Fact>]
    let ``DEC B does not affect C flag`` () =
        let regs = { initRegister() with BC = 0x4200us } |> setFlag false false false true
        let result = DecR B regs
        Assert.True(isC result)

// ====================
// 16ビット算術命令テスト
// ====================

module Inc16Tests =
    [<Fact>]
    let ``INC BC increments BC by 1`` () =
        let regs = { initRegister() with BC = 0x1234us }
        let result = Inc16 BC regs
        Assert.Equal(0x1235us, result.BC)

    [<Fact>]
    let ``INC BC wraps from 0xFFFF to 0x0000`` () =
        let regs = { initRegister() with BC = 0xFFFFus }
        let result = Inc16 BC regs
        Assert.Equal(0x0000us, result.BC)

    [<Fact>]
    let ``INC BC does not affect any flags`` () =
        let regs = { initRegister() with BC = 0xFFFFus } |> setFlag true true true true
        let result = Inc16 BC regs
        Assert.True(isZ result)
        Assert.True(isN result)
        Assert.True(isH result)
        Assert.True(isC result)

    [<Fact>]
    let ``INC DE increments DE`` () =
        let regs = { initRegister() with DE = 0x5678us }
        let result = Inc16 DE regs
        Assert.Equal(0x5679us, result.DE)

    [<Fact>]
    let ``INC HL increments HL`` () =
        let regs = { initRegister() with HL = 0x9ABCus }
        let result = Inc16 HL regs
        Assert.Equal(0x9ABDus, result.HL)

    [<Fact>]
    let ``INC SP increments SP`` () =
        let regs = { initRegister() with SP = 0xFFFEus }
        let result = Inc16 SP regs
        Assert.Equal(0xFFFFus, result.SP)

module Dec16Tests =
    [<Fact>]
    let ``DEC BC decrements BC by 1`` () =
        let regs = { initRegister() with BC = 0x1234us }
        let result = Dec16 BC regs
        Assert.Equal(0x1233us, result.BC)

    [<Fact>]
    let ``DEC BC wraps from 0x0000 to 0xFFFF`` () =
        let regs = { initRegister() with BC = 0x0000us }
        let result = Dec16 BC regs
        Assert.Equal(0xFFFFus, result.BC)

    [<Fact>]
    let ``DEC BC does not affect any flags`` () =
        let regs = { initRegister() with BC = 0x0000us } |> setFlag true true true true
        let result = Dec16 BC regs
        Assert.True(isZ result)
        Assert.True(isN result)
        Assert.True(isH result)
        Assert.True(isC result)

    [<Fact>]
    let ``DEC DE decrements DE`` () =
        let regs = { initRegister() with DE = 0x5678us }
        let result = Dec16 DE regs
        Assert.Equal(0x5677us, result.DE)

    [<Fact>]
    let ``DEC HL decrements HL`` () =
        let regs = { initRegister() with HL = 0x9ABCus }
        let result = Dec16 HL regs
        Assert.Equal(0x9ABBus, result.HL)

    [<Fact>]
    let ``DEC SP decrements SP`` () =
        let regs = { initRegister() with SP = 0xFFFEus }
        let result = Dec16 SP regs
        Assert.Equal(0xFFFDus, result.SP)

module AddHL16Tests =
    [<Fact>]
    let ``ADD HL,BC adds BC to HL`` () =
        let regs = { initRegister() with HL = 0x1000us; BC = 0x0234us }
        let result = AddHL BC regs
        Assert.Equal(0x1234us, result.HL)

    [<Fact>]
    let ``ADD HL,BC sets H flag on half carry from bit 11`` () =
        let regs = { initRegister() with HL = 0x0FFFus; BC = 0x0001us }
        let result = AddHL BC regs
        Assert.True(isH result)

    [<Fact>]
    let ``ADD HL,BC sets C flag on overflow`` () =
        let regs = { initRegister() with HL = 0xFFFFus; BC = 0x0001us }
        let result = AddHL BC regs
        Assert.True(isC result)

    [<Fact>]
    let ``ADD HL,BC clears N flag`` () =
        let regs = { initRegister() with HL = 0x1000us; BC = 0x0234us } |> setFlag false true false false
        let result = AddHL BC regs
        Assert.False(isN result)

    [<Fact>]
    let ``ADD HL,BC does not affect Z flag`` () =
        let regs = { initRegister() with HL = 0x0000us; BC = 0x0000us } |> setFlag true false false false
        let result = AddHL BC regs
        Assert.True(isZ result)

    [<Fact>]
    let ``ADD HL,DE adds DE to HL`` () =
        let regs = { initRegister() with HL = 0x1000us; DE = 0x0234us }
        let result = AddHL DE regs
        Assert.Equal(0x1234us, result.HL)

    [<Fact>]
    let ``ADD HL,HL doubles HL`` () =
        let regs = { initRegister() with HL = 0x1000us }
        let result = AddHL HL regs
        Assert.Equal(0x2000us, result.HL)

    [<Fact>]
    let ``ADD HL,SP adds SP to HL`` () =
        let regs = { initRegister() with HL = 0x1000us; SP = 0x0234us }
        let result = AddHL SP regs
        Assert.Equal(0x1234us, result.HL)

// ====================
// ローテート命令テスト
// ====================

module RotateTests =
    [<Fact>]
    let ``RLCA rotates A left through carry`` () =
        let regs = { initRegister() with AF = 0x8500us }
        let result = Rlca regs
        Assert.Equal(0x0Bus, getRegisterValue (R8 A) result)
        Assert.True(isC result)

    [<Fact>]
    let ``RLCA clears Z, N, H flags`` () =
        let regs = { initRegister() with AF = 0x8500us } |> setFlag true true true false
        let result = Rlca regs
        Assert.False(isZ result)
        Assert.False(isN result)
        Assert.False(isH result)

    [<Fact>]
    let ``RLA rotates A left through carry flag`` () =
        let regs = { initRegister() with AF = 0x8010us } // A=0x80, C=1
        let result = Rla regs
        Assert.Equal(0x01us, getRegisterValue (R8 A) result)
        Assert.True(isC result)

    [<Fact>]
    let ``RRCA rotates A right through carry`` () =
        let regs = { initRegister() with AF = 0x0100us }
        let result = Rrca regs
        Assert.Equal(0x80us, getRegisterValue (R8 A) result)
        Assert.True(isC result)

    [<Fact>]
    let ``RRA rotates A right through carry flag`` () =
        let regs = { initRegister() with AF = 0x0110us } // A=0x01, C=1
        let result = Rra regs
        Assert.Equal(0x80us, getRegisterValue (R8 A) result)
        Assert.True(isC result)

// ====================
// その他の命令テスト
// ====================

module MiscTests =
    [<Fact>]
    let ``CPL complements A`` () =
        let regs = { initRegister() with AF = 0xF000us }
        let result = Cpl regs
        Assert.Equal(0x0Fus, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``CPL sets N and H flags`` () =
        let regs = { initRegister() with AF = 0xF000us }
        let result = Cpl regs
        Assert.True(isN result)
        Assert.True(isH result)

    [<Fact>]
    let ``CCF complements carry flag from 0 to 1`` () =
        let regs = initRegister() |> setFlag false false false false
        let result = Ccf regs
        Assert.True(isC result)

    [<Fact>]
    let ``CCF complements carry flag from 1 to 0`` () =
        let regs = initRegister() |> setFlag false false false true
        let result = Ccf regs
        Assert.False(isC result)

    [<Fact>]
    let ``CCF clears N and H flags`` () =
        let regs = initRegister() |> setFlag false true true false
        let result = Ccf regs
        Assert.False(isN result)
        Assert.False(isH result)

    [<Fact>]
    let ``SCF sets carry flag`` () =
        let regs = initRegister() |> setFlag false false false false
        let result = Scf regs
        Assert.True(isC result)

    [<Fact>]
    let ``SCF clears N and H flags`` () =
        let regs = initRegister() |> setFlag false true true false
        let result = Scf regs
        Assert.False(isN result)
        Assert.False(isH result)

    [<Fact>]
    let ``DAA adjusts A for BCD after addition`` () =
        // 0x09 + 0x01 = 0x0A -> DAA -> 0x10
        let regs = { initRegister() with AF = 0x0A00us } // A=0x0A, N=0
        let result = Daa regs
        Assert.Equal(0x10us, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``DAA sets Z flag when result is zero`` () =
        let regs = { initRegister() with AF = 0x0000us }
        let result = Daa regs
        Assert.True(isZ result)

// ====================
// Decoder テスト
// ====================

module DecoderNewInstructionTests =

    [<Fact>]
    let ``Decode ADC A,B (0x88)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 B) 0x05us state.Regs }
        let state = { state with Regs = { state.Regs with AF = 0x1010us } } // A=0x10, C=1
        testInstruction [ 0x88uy ] state (fun finalState ->
            Assert.Equal(0x16us, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode SBC A,B (0x98)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 B) 0x05us state.Regs }
        let state = { state with Regs = { state.Regs with AF = 0x1010us } } // A=0x10, C=1
        testInstruction [ 0x98uy ] state (fun finalState ->
            Assert.Equal(0x0Aus, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode AND B (0xA0)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0xFFus state.Regs }
        let state = { state with Regs = setRegisterValue (R8 B) 0x0Fus state.Regs }
        testInstruction [ 0xA0uy ] state (fun finalState ->
            Assert.Equal(0x0Fus, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode XOR B (0xA8)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0xFFus state.Regs }
        let state = { state with Regs = setRegisterValue (R8 B) 0x0Fus state.Regs }
        testInstruction [ 0xA8uy ] state (fun finalState ->
            Assert.Equal(0xF0us, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode OR B (0xB0)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0xF0us state.Regs }
        let state = { state with Regs = setRegisterValue (R8 B) 0x0Fus state.Regs }
        testInstruction [ 0xB0uy ] state (fun finalState ->
            Assert.Equal(0xFFus, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode CP B (0xB8)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0x42us state.Regs }
        let state = { state with Regs = setRegisterValue (R8 B) 0x42us state.Regs }
        testInstruction [ 0xB8uy ] state (fun finalState ->
            Assert.True(isZ finalState.Regs)
            Assert.Equal(0x42us, getRegisterValue (R8 A) finalState.Regs) // A unchanged
        )

    [<Fact>]
    let ``Decode INC B (0x04)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 B) 0x42us state.Regs }
        testInstruction [ 0x04uy ] state (fun finalState ->
            Assert.Equal(0x43us, getRegisterValue (R8 B) finalState.Regs)
        )

    [<Fact>]
    let ``Decode DEC B (0x05)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 B) 0x42us state.Regs }
        testInstruction [ 0x05uy ] state (fun finalState ->
            Assert.Equal(0x41us, getRegisterValue (R8 B) finalState.Regs)
        )

    [<Fact>]
    let ``Decode INC BC (0x03)`` () =
        let state = initCpuState()
        let state = { state with Regs = { state.Regs with BC = 0x1234us } }
        testInstruction [ 0x03uy ] state (fun finalState ->
            Assert.Equal(0x1235us, finalState.Regs.BC)
        )

    [<Fact>]
    let ``Decode DEC BC (0x0B)`` () =
        let state = initCpuState()
        let state = { state with Regs = { state.Regs with BC = 0x1234us } }
        testInstruction [ 0x0Buy ] state (fun finalState ->
            Assert.Equal(0x1233us, finalState.Regs.BC)
        )

    [<Fact>]
    let ``Decode ADD HL,BC (0x09)`` () =
        let state = initCpuState()
        let state = { state with Regs = { state.Regs with HL = 0x1000us; BC = 0x0234us } }
        testInstruction [ 0x09uy ] state (fun finalState ->
            Assert.Equal(0x1234us, finalState.Regs.HL)
        )

    [<Fact>]
    let ``Decode RLCA (0x07)`` () =
        let state = initCpuState()
        let state = { state with Regs = { state.Regs with AF = 0x8500us } }
        testInstruction [ 0x07uy ] state (fun finalState ->
            Assert.Equal(0x0Bus, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode RRCA (0x0F)`` () =
        let state = initCpuState()
        let state = { state with Regs = { state.Regs with AF = 0x0100us } }
        testInstruction [ 0x0Fuy ] state (fun finalState ->
            Assert.Equal(0x80us, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode RLA (0x17)`` () =
        let state = initCpuState()
        let state = { state with Regs = { state.Regs with AF = 0x8010us } }
        testInstruction [ 0x17uy ] state (fun finalState ->
            Assert.Equal(0x01us, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode RRA (0x1F)`` () =
        let state = initCpuState()
        let state = { state with Regs = { state.Regs with AF = 0x0110us } }
        testInstruction [ 0x1Fuy ] state (fun finalState ->
            Assert.Equal(0x80us, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode DAA (0x27)`` () =
        let state = initCpuState()
        let state = { state with Regs = { state.Regs with AF = 0x0A00us } }
        testInstruction [ 0x27uy ] state (fun finalState ->
            Assert.Equal(0x10us, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode CPL (0x2F)`` () =
        let state = initCpuState()
        let state = { state with Regs = { state.Regs with AF = 0xF000us } }
        testInstruction [ 0x2Fuy ] state (fun finalState ->
            Assert.Equal(0x0Fus, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode SCF (0x37)`` () =
        testInstruction [ 0x37uy ] (initCpuState()) (fun finalState ->
            Assert.True(isC finalState.Regs)
        )

    [<Fact>]
    let ``Decode CCF (0x3F)`` () =
        let state = initCpuState()
        let state = { state with Regs = setFlag false false false true state.Regs }
        testInstruction [ 0x3Fuy ] state (fun finalState ->
            Assert.False(isC finalState.Regs)
        )

    [<Fact>]
    let ``Decode LD B,n8 (0x06)`` () =
        testInstruction [ 0x06uy; 0x42uy ] (initCpuState()) (fun finalState ->
            Assert.Equal(0x42us, getRegisterValue (R8 B) finalState.Regs)
            Assert.Equal(2us, finalState.Regs.PC)
        )

    [<Fact>]
    let ``Decode ADD A,n8 (0xC6)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0x20us state.Regs }
        testInstruction [ 0xC6uy; 0x10uy ] state (fun finalState ->
            Assert.Equal(0x30us, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode ADC A,n8 (0xCE)`` () =
        let state = initCpuState()
        let state = { state with Regs = { state.Regs with AF = 0x2010us } } // C=1
        testInstruction [ 0xCEuy; 0x10uy ] state (fun finalState ->
            Assert.Equal(0x31us, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode SUB n8 (0xD6)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0x30us state.Regs }
        testInstruction [ 0xD6uy; 0x10uy ] state (fun finalState ->
            Assert.Equal(0x20us, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode SBC A,n8 (0xDE)`` () =
        let state = initCpuState()
        let state = { state with Regs = { state.Regs with AF = 0x3010us } } // C=1
        testInstruction [ 0xDEuy; 0x10uy ] state (fun finalState ->
            Assert.Equal(0x1Fus, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode AND n8 (0xE6)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0xFFus state.Regs }
        testInstruction [ 0xE6uy; 0x0Fuy ] state (fun finalState ->
            Assert.Equal(0x0Fus, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode XOR n8 (0xEE)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0xFFus state.Regs }
        testInstruction [ 0xEEuy; 0x0Fuy ] state (fun finalState ->
            Assert.Equal(0xF0us, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode OR n8 (0xF6)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0xF0us state.Regs }
        testInstruction [ 0xF6uy; 0x0Fuy ] state (fun finalState ->
            Assert.Equal(0xFFus, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode CP n8 (0xFE)`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0x42us state.Regs }
        testInstruction [ 0xFEuy; 0x42uy ] state (fun finalState ->
            Assert.True(isZ finalState.Regs)
        )
