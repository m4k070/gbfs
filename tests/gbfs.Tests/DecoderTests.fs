module DecoderTests

open Xunit
open gbfs.Lib.Cpu
open gbfs.Lib.Decoder
open gbfs.Lib.Memory

/// 初期状態のレジスタを作成
let initRegister () =
    { AF = 0us; BC = 0us; DE = 0us; HL = 0us; PC = 0us; SP = 0us }

let initCpuState () =
    let state = createState()
    { state with Regs = { state.Regs with PC = 0us } }

let testInstruction (memory: byte list) (initialState: CpuState) (assertions: CpuState -> unit) =
    let rom = List.toArray memory
    let state = loadRomToState rom initialState
    let finalState = step state
    assertions finalState

// ====================
// アクティブパターンのテスト
// ====================

module NopPatternTests =
    [<Fact>]
    let ``Nop pattern matches 0x00`` () =
        match 0x00uy with
        | Nop -> Assert.True(true)
        | _ -> Assert.Fail("Should match Nop")

    [<Fact>]
    let ``Nop pattern does not match other opcodes`` () =
        match 0x01uy with
        | Nop -> Assert.Fail("Should not match Nop")
        | _ -> Assert.True(true)

module LdRRPatternTests =
    [<Fact>]
    let ``LdRR pattern matches 0x40 (LD B,B)`` () =
        match 0x40uy with
        | LdRR (0, 0) -> Assert.True(true) // B=0, B=0
        | _ -> Assert.Fail("Should match LD B,B")

    [<Fact>]
    let ``LdRR pattern matches 0x41 (LD B,C)`` () =
        match 0x41uy with
        | LdRR (0, 1) -> Assert.True(true) // B=0, C=1
        | _ -> Assert.Fail("Should match LD B,C")

    [<Fact>]
    let ``LdRR pattern matches 0x78 (LD A,B)`` () =
        match 0x78uy with
        | LdRR (7, 0) -> Assert.True(true) // A=7, B=0
        | _ -> Assert.Fail("Should match LD A,B")

    [<Fact>]
    let ``LdRR pattern matches 0x7F (LD A,A)`` () =
        match 0x7Fuy with
        | LdRR (7, 7) -> Assert.True(true) // A=7, A=7
        | _ -> Assert.Fail("Should match LD A,A")

    [<Fact>]
    let ``LdRR pattern does not match 0x76 (HALT)`` () =
        match 0x76uy with
        | LdRR _ -> Assert.Fail("Should not match LdRR for HALT opcode")
        | _ -> Assert.True(true)

    [<Fact>]
    let ``LdRR pattern does not match opcodes below 0x40`` () =
        match 0x3Fuy with
        | LdRR _ -> Assert.Fail("Should not match LdRR")
        | _ -> Assert.True(true)

    [<Fact>]
    let ``LdRR pattern does not match opcodes above 0x7F`` () =
        match 0x80uy with
        | LdRR _ -> Assert.Fail("Should not match LdRR")
        | _ -> Assert.True(true)

module LdHImm16PatternTests =
    [<Fact>]
    let ``LdHImm16 pattern matches 0x01 (LD BC,n16)`` () =
        match 0x01uy with
        | LdHImm16 BC -> Assert.True(true)
        | _ -> Assert.Fail("Should match LD BC,n16")

    [<Fact>]
    let ``LdHImm16 pattern matches 0x11 (LD DE,n16)`` () =
        match 0x11uy with
        | LdHImm16 DE -> Assert.True(true)
        | _ -> Assert.Fail("Should match LD DE,n16")

    [<Fact>]
    let ``LdHImm16 pattern matches 0x21 (LD HL,n16)`` () =
        match 0x21uy with
        | LdHImm16 HL -> Assert.True(true)
        | _ -> Assert.Fail("Should match LD HL,n16")

    [<Fact>]
    let ``LdHImm16 pattern matches 0x31 (LD SP,n16)`` () =
        match 0x31uy with
        | LdHImm16 SP -> Assert.True(true)
        | _ -> Assert.Fail("Should match LD SP,n16")

module AddRPatternTests =
    [<Fact>]
    let ``AddR pattern matches 0x80 (ADD A,B)`` () =
        match 0x80uy with
        | AddR 0 -> Assert.True(true) // B=0
        | _ -> Assert.Fail("Should match ADD A,B")

    [<Fact>]
    let ``AddR pattern matches 0x81 (ADD A,C)`` () =
        match 0x81uy with
        | AddR 1 -> Assert.True(true) // C=1
        | _ -> Assert.Fail("Should match ADD A,C")

    [<Fact>]
    let ``AddR pattern matches 0x87 (ADD A,A)`` () =
        match 0x87uy with
        | AddR 7 -> Assert.True(true) // A=7
        | _ -> Assert.Fail("Should match ADD A,A")

    [<Fact>]
    let ``AddR pattern does not match opcodes outside range`` () =
        match 0x88uy with
        | AddR _ -> Assert.Fail("Should not match AddR")
        | _ -> Assert.True(true)

module SubRPatternTests =
    [<Fact>]
    let ``SubR pattern matches 0x90 (SUB B)`` () =
        match 0x90uy with
        | SubR 0 -> Assert.True(true) // B=0
        | _ -> Assert.Fail("Should match SUB B")

    [<Fact>]
    let ``SubR pattern matches 0x91 (SUB C)`` () =
        match 0x91uy with
        | SubR 1 -> Assert.True(true) // C=1
        | _ -> Assert.Fail("Should match SUB C")

    [<Fact>]
    let ``SubR pattern matches 0x97 (SUB A)`` () =
        match 0x97uy with
        | SubR 7 -> Assert.True(true) // A=7
        | _ -> Assert.Fail("Should match SUB A")

    [<Fact>]
    let ``SubR pattern does not match opcodes outside range`` () =
        match 0x98uy with
        | SubR _ -> Assert.Fail("Should not match SubR")
        | _ -> Assert.True(true)

// ====================
// Decode関数のテスト
// ====================

module DecodeNopTests =
    [<Fact>]
    let ``Decode NOP advances PC by 1`` () =
        testInstruction [ 0x00uy ] (initCpuState()) (fun finalState ->
            Assert.Equal(1us, finalState.Regs.PC)
        )

module DecodeLdRRTests =
    [<Fact>]
    let ``Decode LD B,A copies A to B and advances PC`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0x42us state.Regs }
        testInstruction [ 0x47uy ] state (fun finalState -> // LD B,A
            Assert.Equal(0x42us, getRegisterValue (R8 B) finalState.Regs)
            Assert.Equal(1us, finalState.Regs.PC)
        )

    [<Fact>]
    let ``Decode LD C,D copies D to C`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 D) 0x37us state.Regs }
        testInstruction [ 0x4Auy ] state (fun finalState -> // LD C,D
            Assert.Equal(0x37us, getRegisterValue (R8 C) finalState.Regs)
        )

    [<Fact>]
    let ``Decode LD A,B copies B to A`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 B) 0x55us state.Regs }
        testInstruction [ 0x78uy ] state (fun finalState -> // LD A,B
            Assert.Equal(0x55us, getRegisterValue (R8 A) finalState.Regs)
        )

module DecodeLdHImm16Tests =
    [<Fact>]
    let ``Decode LD BC,n16 loads immediate value`` () =
        testInstruction [ 0x01uy; 0x34uy; 0x12uy ] (initCpuState()) (fun finalState ->
            Assert.Equal(0x1234us, finalState.Regs.BC)
            Assert.Equal(3us, finalState.Regs.PC)
        )

    [<Fact>]
    let ``Decode LD DE,n16 advances PC by 3`` () =
        testInstruction [ 0x11uy; 0xCDuy; 0xABuy ] (initCpuState()) (fun finalState ->
            Assert.Equal(0xABCDus, finalState.Regs.DE)
            Assert.Equal(3us, finalState.Regs.PC)
        )

    [<Fact>]
    let ``Decode LD HL,n16 advances PC by 3`` () =
        testInstruction [ 0x21uy; 0x00uy; 0x80uy ] (initCpuState()) (fun finalState ->
            Assert.Equal(0x8000us, finalState.Regs.HL)
            Assert.Equal(3us, finalState.Regs.PC)
        )

    [<Fact>]
    let ``Decode LD SP,n16 advances PC by 3`` () =
        testInstruction [ 0x31uy; 0xFEuy; 0xFFuy ] (initCpuState()) (fun finalState ->
            Assert.Equal(0xFFFEus, finalState.Regs.SP)
            Assert.Equal(3us, finalState.Regs.PC)
        )

module DecodeAddRTests =
    [<Fact>]
    let ``Decode ADD A,B adds B to A`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0x10us state.Regs }
        let state = { state with Regs = setRegisterValue (R8 B) 0x05us state.Regs }
        testInstruction [ 0x80uy ] state (fun finalState -> // ADD A,B
            Assert.Equal(0x15us, getRegisterValue (R8 A) finalState.Regs)
            Assert.Equal(1us, finalState.Regs.PC)
        )

    [<Fact>]
    let ``Decode ADD A,C adds C to A`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0x20us state.Regs }
        let state = { state with Regs = setRegisterValue (R8 C) 0x03us state.Regs }
        testInstruction [ 0x81uy ] state (fun finalState -> // ADD A,C
            Assert.Equal(0x23us, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode ADD A,A doubles A`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0x10us state.Regs }
        testInstruction [ 0x87uy ] state (fun finalState -> // ADD A,A
            Assert.Equal(0x20us, getRegisterValue (R8 A) finalState.Regs)
        )

module DecodeSubRTests =
    [<Fact>]
    let ``Decode SUB B subtracts B from A`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0x10us state.Regs }
        let state = { state with Regs = setRegisterValue (R8 B) 0x05us state.Regs }
        testInstruction [ 0x90uy ] state (fun finalState -> // SUB B
            Assert.Equal(0x0Bus, getRegisterValue (R8 A) finalState.Regs)
            Assert.Equal(1us, finalState.Regs.PC)
        )

    [<Fact>]
    let ``Decode SUB C subtracts C from A`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0x20us state.Regs }
        let state = { state with Regs = setRegisterValue (R8 C) 0x03us state.Regs }
        testInstruction [ 0x91uy ] state (fun finalState -> // SUB C
            Assert.Equal(0x1Dus, getRegisterValue (R8 A) finalState.Regs)
        )

    [<Fact>]
    let ``Decode SUB A results in zero`` () =
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0x42us state.Regs }
        testInstruction [ 0x97uy ] state (fun finalState -> // SUB A
            Assert.Equal(0x00us, getRegisterValue (R8 A) finalState.Regs)
            Assert.True(isZ finalState.Regs)
        )

module DecodeSequenceTests =
    [<Fact>]
    let ``Decode sequence of instructions`` () =
        // NOP; LD B,A; ADD A,B
        let memory = [ 0x00uy; 0x47uy; 0x80uy ]
        let rom = List.toArray memory
        let state = initCpuState()
        let state = { state with Regs = setRegisterValue (R8 A) 0x10us state.Regs }
        let state = loadRomToState rom state
        
        let state = step state // NOP
        let state = step state // LD B,A
        let finalState = step state // ADD A,B

        // A=0x10, B=0x10 after LD B,A, then A=0x20 after ADD A,B
        Assert.Equal(0x20us, getRegisterValue (R8 A) finalState.Regs)
        Assert.Equal(0x10us, getRegisterValue (R8 B) finalState.Regs)
        Assert.Equal(3us, finalState.Regs.PC)

    [<Fact>]
    let ``Decode starts from PC position`` () =
        let memory = [ 0xFFuy; 0xFFuy; 0x00uy ] // 最初の2バイトはスキップ、NOPを実行
        let state = initCpuState()
        let state = { state with Regs = { state.Regs with PC = 2us } }
        testInstruction memory state (fun finalState ->
            Assert.Equal(3us, finalState.Regs.PC)
        )
