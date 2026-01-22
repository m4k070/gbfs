namespace gbfs.Lib

open Cpu
open Memory
open CBPrefix
open Ppu

module Decoder =
  // オペコードのビット列(0-7)に対応するレジスタの定義
  // 例: 000->B, 001->C, ... 111->A
  // 注: インデックス6は(HL)を表す
  let private r8map =
    [|
      R8 Cpu.B;
      R8 Cpu.C;
      R8 Cpu.D;
      R8 Cpu.E;
      R8 Cpu.H;
      R8 Cpu.L;
      R16 Cpu.HL;  // (HL) - メモリアクセス
      R8 Cpu.A;
    |]

  let private r16map =
    [|
      Cpu.BC;
      Cpu.DE;
      Cpu.HL;
      Cpu.SP;
    |]

  let private r16stkmap =
    [|
      Cpu.BC;
      Cpu.DE;
      Cpu.HL;
      Cpu.AF;
    |]

  // ====================
  // Active Patterns
  // ====================

  // NOP命令 (0x00)
  let (|Nop|_|) (opcode: uint8) =
    if opcode = 0x00uy then Some() else None

  // HALT (0x76)
  let (|Halt|_|) (opcode: uint8) =
    if opcode = 0x76uy then Some() else None
  
  // CB Prefixed instruction
  let (|CBPrefixed|_|) (opcode: uint8) =
    if opcode = 0xCBuy then Some() else None

  // 8bitロード命令: LD r, r' (0x40 - 0x7F, ただし 0x76(HALT)を除く)
  let (|LdRR|_|) (opcode: uint8) =
    if 0x40uy <= opcode && opcode <= 0x7Fuy && opcode <> 0x76uy then
      let dst_idx = (int opcode >>> 3) &&& 0x7
      let src_idx = int opcode &&& 0x7
      Some(dst_idx, src_idx)
    else
      None

  // LD r16,n16
  let (|LdHImm16|_|) (opcode: uint8) =
    if 0b11001111uy &&& opcode = 1uy then
      let dst_idx = int(opcode) >>> 4 &&& 0x3
      Some(r16map.[dst_idx])
    else
      None

  // LD r,n8 (0x06, 0x0E, 0x16, 0x1E, 0x26, 0x2E, 0x36, 0x3E)
  let (|LdRN8|_|) (opcode: uint8) =
    if opcode &&& 0b11000111uy = 0b00000110uy then
      let dst_idx = (int opcode >>> 3) &&& 0x7
      Some(dst_idx)
    else
      None

  // LD A,(BC) - 0x0A
  let (|LdABC|_|) (opcode: uint8) = if opcode = 0x0Auy then Some() else None
  // LD A,(DE) - 0x1A
  let (|LdADE|_|) (opcode: uint8) = if opcode = 0x1Auy then Some() else None
  // LD (BC),A - 0x02
  let (|LdBCA|_|) (opcode: uint8) = if opcode = 0x02uy then Some() else None
  // LD (DE),A - 0x12
  let (|LdDEA|_|) (opcode: uint8) = if opcode = 0x12uy then Some() else None
  // LD A,(HL+) - 0x2A
  let (|LdAHLI|_|) (opcode: uint8) = if opcode = 0x2Auy then Some() else None
  // LD A,(HL-) - 0x3A
  let (|LdAHLD|_|) (opcode: uint8) = if opcode = 0x3Auy then Some() else None
  // LD (HL+),A - 0x22
  let (|LdHLIA|_|) (opcode: uint8) = if opcode = 0x22uy then Some() else None
  // LD (HL-),A - 0x32
  let (|LdHLDA|_|) (opcode: uint8) = if opcode = 0x32uy then Some() else None
  // LD A,(nn) - 0xFA
  let (|LdANN|_|) (opcode: uint8) = if opcode = 0xFAuy then Some() else None
  // LD (nn),A - 0xEA
  let (|LdNNA|_|) (opcode: uint8) = if opcode = 0xEAuy then Some() else None
  // LDH A,(n) - 0xF0
  let (|LdhAN|_|) (opcode: uint8) = if opcode = 0xF0uy then Some() else None
  // LDH (n),A - 0xE0
  let (|LdhNA|_|) (opcode: uint8) = if opcode = 0xE0uy then Some() else None
  // LDH A,(C) - 0xF2
  let (|LdhAC|_|) (opcode: uint8) = if opcode = 0xF2uy then Some() else None
  // LDH (C),A - 0xE2
  let (|LdhCA|_|) (opcode: uint8) = if opcode = 0xE2uy then Some() else None
  // LD SP,HL - 0xF9
  let (|LdSPHL|_|) (opcode: uint8) = if opcode = 0xF9uy then Some() else None
  // LD (nn),SP - 0x08
  let (|LdNNSP|_|) (opcode: uint8) = if opcode = 0x08uy then Some() else None

  // 8bit ALU with register
  let (|AddR|_|) (opcode: uint8) =
    if 0x80uy <= opcode && opcode <= 0x87uy then Some(int opcode &&& 0x7) else None
  let (|AdcR|_|) (opcode: uint8) =
    if 0x88uy <= opcode && opcode <= 0x8Fuy then Some(int opcode &&& 0x7) else None
  let (|SubR|_|) (opcode: uint8) =
    if 0x90uy <= opcode && opcode <= 0x97uy then Some(int opcode &&& 0x7) else None
  let (|SbcR|_|) (opcode: uint8) =
    if 0x98uy <= opcode && opcode <= 0x9Fuy then Some(int opcode &&& 0x7) else None
  let (|AndR|_|) (opcode: uint8) =
    if 0xA0uy <= opcode && opcode <= 0xA7uy then Some(int opcode &&& 0x7) else None
  let (|XorR|_|) (opcode: uint8) =
    if 0xA8uy <= opcode && opcode <= 0xAFuy then Some(int opcode &&& 0x7) else None
  let (|OrR|_|) (opcode: uint8) =
    if 0xB0uy <= opcode && opcode <= 0xB7uy then Some(int opcode &&& 0x7) else None
  let (|CpR|_|) (opcode: uint8) =
    if 0xB8uy <= opcode && opcode <= 0xBFuy then Some(int opcode &&& 0x7) else None

  // INC/DEC r
  let (|IncR|_|) (opcode: uint8) =
    if opcode &&& 0b11000111uy = 0b00000100uy then Some((int opcode >>> 3) &&& 0x7) else None
  let (|DecR|_|) (opcode: uint8) =
    if opcode &&& 0b11000111uy = 0b00000101uy then Some((int opcode >>> 3) &&& 0x7) else None

  // 16-bit ALU
  let (|Inc16|_|) (opcode: uint8) =
    if opcode &&& 0b11001111uy = 0b00000011uy then Some(r16map.[(int opcode >>> 4) &&& 0x3]) else None
  let (|Dec16|_|) (opcode: uint8) =
    if opcode &&& 0b11001111uy = 0b00001011uy then Some(r16map.[(int opcode >>> 4) &&& 0x3]) else None
  let (|AddHL|_|) (opcode: uint8) =
    if opcode &&& 0b11001111uy = 0b00001001uy then Some(r16map.[(int opcode >>> 4) &&& 0x3]) else None

  // Rotate instructions
  let (|Rlca|_|) (opcode: uint8) = if opcode = 0x07uy then Some() else None
  let (|Rrca|_|) (opcode: uint8) = if opcode = 0x0Fuy then Some() else None
  let (|Rla|_|) (opcode: uint8) = if opcode = 0x17uy then Some() else None
  let (|Rra|_|) (opcode: uint8) = if opcode = 0x1Fuy then Some() else None

  // Misc instructions
  let (|Daa|_|) (opcode: uint8) = if opcode = 0x27uy then Some() else None
  let (|Cpl|_|) (opcode: uint8) = if opcode = 0x2Fuy then Some() else None
  let (|Scf|_|) (opcode: uint8) = if opcode = 0x37uy then Some() else None
  let (|Ccf|_|) (opcode: uint8) = if opcode = 0x3Fuy then Some() else None

  // Immediate arithmetic instructions
  let (|AddN8|_|) (opcode: uint8) = if opcode = 0xC6uy then Some() else None
  let (|AdcN8|_|) (opcode: uint8) = if opcode = 0xCEuy then Some() else None
  let (|SubN8|_|) (opcode: uint8) = if opcode = 0xD6uy then Some() else None
  let (|SbcN8|_|) (opcode: uint8) = if opcode = 0xDEuy then Some() else None
  let (|AndN8|_|) (opcode: uint8) = if opcode = 0xE6uy then Some() else None
  let (|XorN8|_|) (opcode: uint8) = if opcode = 0xEEuy then Some() else None
  let (|OrN8|_|) (opcode: uint8) = if opcode = 0xF6uy then Some() else None
  let (|CpN8|_|) (opcode: uint8) = if opcode = 0xFEuy then Some() else None

  // Jump instructions
  let (|JpNN|_|) (opcode: uint8) = if opcode = 0xC3uy then Some() else None
  let (|JpNZ|_|) (opcode: uint8) = if opcode = 0xC2uy then Some() else None
  let (|JpZ|_|) (opcode: uint8) = if opcode = 0xCAuy then Some() else None
  let (|JpNC|_|) (opcode: uint8) = if opcode = 0xD2uy then Some() else None
  let (|JpC|_|) (opcode: uint8) = if opcode = 0xDAuy then Some() else None
  let (|JpHL|_|) (opcode: uint8) = if opcode = 0xE9uy then Some() else None
  let (|JrE8|_|) (opcode: uint8) = if opcode = 0x18uy then Some() else None
  let (|JrNZ|_|) (opcode: uint8) = if opcode = 0x20uy then Some() else None
  let (|JrZ|_|) (opcode: uint8) = if opcode = 0x28uy then Some() else None
  let (|JrNC|_|) (opcode: uint8) = if opcode = 0x30uy then Some() else None
  let (|JrC|_|) (opcode: uint8) = if opcode = 0x38uy then Some() else None

  // Call/Return instructions
  let (|CallNN|_|) (opcode: uint8) = if opcode = 0xCDuy then Some() else None
  let (|CallNZ|_|) (opcode: uint8) = if opcode = 0xC4uy then Some() else None
  let (|CallZ|_|) (opcode: uint8) = if opcode = 0xCCuy then Some() else None
  let (|CallNC|_|) (opcode: uint8) = if opcode = 0xD4uy then Some() else None
  let (|CallC|_|) (opcode: uint8) = if opcode = 0xDCuy then Some() else None
  let (|Ret|_|) (opcode: uint8) = if opcode = 0xC9uy then Some() else None
  let (|RetNZ|_|) (opcode: uint8) = if opcode = 0xC0uy then Some() else None
  let (|RetZ|_|) (opcode: uint8) = if opcode = 0xC8uy then Some() else None
  let (|RetNC|_|) (opcode: uint8) = if opcode = 0xD0uy then Some() else None
  let (|RetC|_|) (opcode: uint8) = if opcode = 0xD8uy then Some() else None
  let (|Reti|_|) (opcode: uint8) = if opcode = 0xD9uy then Some() else None

  // RST instructions
  let (|Rst00|_|) (opcode: uint8) = if opcode = 0xC7uy then Some() else None
  let (|Rst08|_|) (opcode: uint8) = if opcode = 0xCFuy then Some() else None
  let (|Rst10|_|) (opcode: uint8) = if opcode = 0xD7uy then Some() else None
  let (|Rst18|_|) (opcode: uint8) = if opcode = 0xDFuy then Some() else None
  let (|Rst20|_|) (opcode: uint8) = if opcode = 0xE7uy then Some() else None
  let (|Rst28|_|) (opcode: uint8) = if opcode = 0xEFuy then Some() else None
  let (|Rst30|_|) (opcode: uint8) = if opcode = 0xF7uy then Some() else None
  let (|Rst38|_|) (opcode: uint8) = if opcode = 0xFFuy then Some() else None

  // Stack instructions
  let (|PushBC|_|) (opcode: uint8) = if opcode = 0xC5uy then Some() else None
  let (|PushDE|_|) (opcode: uint8) = if opcode = 0xD5uy then Some() else None
  let (|PushHL|_|) (opcode: uint8) = if opcode = 0xE5uy then Some() else None
  let (|PushAF|_|) (opcode: uint8) = if opcode = 0xF5uy then Some() else None
  let (|PopBC|_|) (opcode: uint8) = if opcode = 0xC1uy then Some() else None
  let (|PopDE|_|) (opcode: uint8) = if opcode = 0xD1uy then Some() else None
  let (|PopHL|_|) (opcode: uint8) = if opcode = 0xE1uy then Some() else None
  let (|PopAF|_|) (opcode: uint8) = if opcode = 0xF1uy then Some() else None

  // Interrupt control
  let (|Di|_|) (opcode: uint8) = if opcode = 0xF3uy then Some() else None
  let (|Ei|_|) (opcode: uint8) = if opcode = 0xFBuy then Some() else None

  // ====================
  // Execution State
  // ====================

  type CpuState = {
    Regs: Register
    Mem: MemoryBus
    Ppu: PpuState
    Ime: bool  // Interrupt Master Enable
    Halted: bool
  }

  let createState () = {
    Regs = Cpu.initRegister()
    Mem = Memory.create()
    Ppu = Ppu.create()
    Ime = false
    Halted = false
  }

  let loadRomToState (rom: byte array) (state: CpuState) =
    { state with Mem = Memory.loadRom rom state.Mem }

  // ====================
  // Helper functions
  // ====================

  let inline readByte addr state = Memory.read addr state.Mem
  let inline writeByte addr value state = { state with Mem = Memory.write addr value state.Mem }
  let inline read16 addr state = Memory.read16 addr state.Mem
  let inline write16 addr value state = { state with Mem = Memory.write16 addr value state.Mem }

  let inline advancePc n state = { state with Regs = { state.Regs with PC = state.Regs.PC + uint16 n } }
  let inline setRegs regs state = { state with Regs = regs }

  let inline fetchByte state =
    let value = readByte state.Regs.PC state
    (value, advancePc 1 state)

  let inline fetch16 state =
    let value = read16 state.Regs.PC state
    (value, advancePc 2 state)

  // Placeholder for instruction cycle counts
  let getInstructionCycles opcode =
    // TODO: Implement accurate cycle counts for each instruction.
    4

  // ====================
  // CB-Prefixed Execution
  // ====================
  
  // CB Active Patterns
  let (|CbRlc|_|) (op: byte) = if op <= 0x07uy then Some(int op &&& 0x7) else None
  let (|CbRrc|_|) (op: byte) = if op >= 0x08uy && op <= 0x0Fuy then Some(int op &&& 0x7) else None
  let (|CbRl|_|) (op: byte) = if op >= 0x10uy && op <= 0x17uy then Some(int op &&& 0x7) else None
  let (|CbRr|_|) (op: byte) = if op >= 0x18uy && op <= 0x1Fuy then Some(int op &&& 0x7) else None
  let (|CbSla|_|) (op: byte) = if op >= 0x20uy && op <= 0x27uy then Some(int op &&& 0x7) else None
  let (|CbSra|_|) (op: byte) = if op >= 0x28uy && op <= 0x2Fuy then Some(int op &&& 0x7) else None
  let (|CbSwap|_|) (op: byte) = if op >= 0x30uy && op <= 0x37uy then Some(int op &&& 0x7) else None
  let (|CbSrl|_|) (op: byte) = if op >= 0x38uy && op <= 0x3Fuy then Some(int op &&& 0x7) else None
  let (|CbBit|_|) (op: byte) = if op >= 0x40uy && op <= 0x7Fuy then let bit = (int op >>> 3) &&& 0x7 in let regIdx = int op &&& 0x7 in Some(bit, regIdx) else None
  let (|CbRes|_|) (op: byte) = if op >= 0x80uy && op <= 0xBFuy then let bit = (int op >>> 3) &&& 0x7 in let regIdx = int op &&& 0x7 in Some(bit, regIdx) else None
  let (|CbSet|_|) (op: byte) = if op >= 0xC0uy && op <= 0xFFuy then let bit = (int op >>> 3) &&& 0x7 in let regIdx = int op &&& 0x7 in Some(bit, regIdx) else None

  let stepCb (state: CpuState) : CpuState * int =
      let (opcode, state) = fetchByte state

      // Helper function to apply a CB operation (that returns a new value and regs) to a register or memory
      let apply (op: uint16 -> Register -> (uint16 * Register)) regIdx state =
          if regIdx = 6 then // (HL)
              let addr = Cpu.getRegisterValue (R16 HL) state.Regs
              let value = uint16 (readByte addr state)
              let (result, newRegs) = op value state.Regs
              let newState = writeByte addr (byte result) { state with Regs = newRegs }
              (newState, 16) // (HL) ops take more cycles
          else
              let reg = match r8map.[regIdx] with R8 r -> r | _ -> failwith "Invalid register for CB op"
              let value = Cpu.getRegisterValue (R8 reg) state.Regs
              let (result, newRegs) = op value state.Regs
              let newState = setRegs (Cpu.setRegisterValue (R8 reg) result newRegs) state
              (newState, 8)

      // Helper function to apply a "pure" CB operation (that only returns a new value) to a register or memory
      let applyPure (op: uint16 -> uint16) regIdx state =
          if regIdx = 6 then // (HL)
              let addr = Cpu.getRegisterValue (R16 HL) state.Regs
              let value = uint16 (readByte addr state)
              let result = op value
              let newState = writeByte addr (byte result) state
              (newState, 16)
          else
              let reg = match r8map.[regIdx] with R8 r -> r | _ -> failwith "Invalid register for CB op"
              let value = Cpu.getRegisterValue (R8 reg) state.Regs
              let result = op value
              let newState = setRegs (Cpu.setRegisterValue (R8 reg) result state.Regs) state
              (newState, 8)

      match opcode with
      | CbRlc regIdx  -> apply Rlc regIdx state
      | CbRrc regIdx  -> apply Rrc regIdx state
      | CbRl regIdx   -> apply Rl regIdx state
      | CbRr regIdx   -> apply Rr regIdx state
      | CbSla regIdx  -> apply Sla regIdx state
      | CbSra regIdx  -> apply Sra regIdx state
      | CbSwap regIdx -> apply Swap regIdx state
      | CbSrl regIdx  -> apply Srl regIdx state
      | CbBit (bit, regIdx) ->
          let cycles =
              if regIdx = 6 then // BIT n,(HL)
                  let addr = Cpu.getRegisterValue (R16 HL) state.Regs
                  let value = uint16 (readByte addr state)
                  let newState = setRegs (Bit bit value state.Regs) state
                  (newState, 12)
              else
                  let reg = match r8map.[regIdx] with R8 r -> r | _ -> failwith "Invalid register for BIT"
                  let value = Cpu.getRegisterValue (R8 reg) state.Regs
                  let newState = setRegs (Bit bit value state.Regs) state
                  (newState, 8)
          cycles
      | CbRes (bit, regIdx) -> applyPure (Res bit) regIdx state
      | CbSet (bit, regIdx) -> applyPure (Set bit) regIdx state
      | _ -> (state, 4) // Unknown CB opcode

  // ====================
  // Single Step Execution
  // ====================

  let step (state: CpuState) : CpuState =
    if state.Halted then
      // Halted状態でもPPUは動作し続ける
      let (ppu, mem) = Ppu.step 4 state.Ppu state.Mem
      { state with Ppu = ppu; Mem = mem }
    else
      let opcode = readByte state.Regs.PC state

      // 命令の実行とサイクル計算
      let (newState, cycles) =
        match opcode with
        | Nop -> (advancePc 1 state, 4)
        | Halt -> ({ (advancePc 1 state) with Halted = true }, 4)
        | CBPrefixed ->
            let stateAfterPC = advancePc 1 state
            let (s, c) = stepCb stateAfterPC
            (s, c + 4) // CB命令自体のサイクル(4)を追加

        // 8-bit Load
        | LdRR (dst_idx, src_idx) ->
            let s = advancePc 1 state
            let newState =
              if dst_idx = 6 then // LD (HL), r
                let srcReg = match r8map.[src_idx] with R8 r -> r | _ -> failwith "Invalid src"
                let value = Cpu.getRegisterValue (R8 srcReg) s.Regs
                let addr = Cpu.getRegisterValue (R16 HL) s.Regs
                writeByte addr (byte value) s
              elif src_idx = 6 then // LD r, (HL)
                let dstReg = match r8map.[dst_idx] with R8 r -> r | _ -> failwith "Invalid dst"
                let addr = Cpu.getRegisterValue (R16 HL) s.Regs
                let value = readByte addr s
                setRegs (Cpu.LoadN8 dstReg (uint16 value) s.Regs) s
              else // LD r, r'
                let dstReg = match r8map.[dst_idx] with R8 r -> r | _ -> failwith "Invalid dst"
                let srcReg = match r8map.[src_idx] with R8 r -> r | _ -> failwith "Invalid src"
                setRegs (Cpu.LoadR dstReg srcReg s.Regs) s
            let c = if dst_idx = 6 || src_idx = 6 then 8 else 4
            (newState, c)

        | LdRN8 dst_idx ->
            let (imm, s) = state |> advancePc 1 |> fetchByte
            let newState =
                if dst_idx = 6 then // LD (HL), n8
                    let addr = Cpu.getRegisterValue (R16 HL) s.Regs
                    writeByte addr imm s
                else
                    let dstReg = match r8map.[dst_idx] with R8 r -> r | _ -> failwith "invalid reg"
                    setRegs (Cpu.LoadN8 dstReg (uint16 imm) s.Regs) s
            let c = if dst_idx = 6 then 12 else 8
            (newState, c)

        | LdHImm16 reg ->
            let (imm, s) = state |> advancePc 1 |> fetch16
            let newState = setRegs (Cpu.LoadN16 reg imm s.Regs) s
            (newState, 12)

        | LdABC ->
            let s = advancePc 1 state
            let addr = Cpu.getRegisterValue (R16 BC) s.Regs
            let value = readByte addr s
            let newState = setRegs (Cpu.LoadN8 A (uint16 value) s.Regs) s
            (newState, 8)
        | LdADE ->
            let s = advancePc 1 state
            let addr = Cpu.getRegisterValue (R16 DE) s.Regs
            let value = readByte addr s
            let newState = setRegs (Cpu.LoadN8 A (uint16 value) s.Regs) s
            (newState, 8)
        | LdBCA ->
            let s = advancePc 1 state
            let addr = Cpu.getRegisterValue (R16 BC) s.Regs
            let value = Cpu.getRegisterValue (R8 A) s.Regs
            let newState = writeByte addr (byte value) s
            (newState, 8)
        | LdDEA ->
            let s = advancePc 1 state
            let addr = Cpu.getRegisterValue (R16 DE) s.Regs
            let value = Cpu.getRegisterValue (R8 A) s.Regs
            let newState = writeByte addr (byte value) s
            (newState, 8)

        | LdAHLI ->
            let s = advancePc 1 state
            let addr = Cpu.getRegisterValue (R16 HL) s.Regs
            let value = readByte addr s
            let s_val = setRegs (Cpu.LoadN8 A (uint16 value) s.Regs) s
            let newState = setRegs (Cpu.Inc16 HL s_val.Regs) s_val
            (newState, 8)
        | LdAHLD ->
            let s = advancePc 1 state
            let addr = Cpu.getRegisterValue (R16 HL) s.Regs
            let value = readByte addr s
            let s_val = setRegs (Cpu.LoadN8 A (uint16 value) s.Regs) s
            let newState = setRegs (Cpu.Dec16 HL s_val.Regs) s_val
            (newState, 8)
        | LdHLIA ->
            let s = advancePc 1 state
            let addr = Cpu.getRegisterValue (R16 HL) s.Regs
            let value = Cpu.getRegisterValue (R8 A) s.Regs
            let s_val = writeByte addr (byte value) s
            let newState = setRegs (Cpu.Inc16 HL s_val.Regs) s_val
            (newState, 8)
        | LdHLDA ->
            let s = advancePc 1 state
            let addr = Cpu.getRegisterValue (R16 HL) s.Regs
            let value = Cpu.getRegisterValue (R8 A) s.Regs
            let s_val = writeByte addr (byte value) s
            let newState = setRegs (Cpu.Dec16 HL s_val.Regs) s_val
            (newState, 8)

        | LdANN ->
            let (addr, s) = state |> advancePc 1 |> fetch16
            let value = readByte addr s
            let newState = setRegs (Cpu.LoadN8 A (uint16 value) s.Regs) s
            (newState, 16)
        | LdNNA ->
            let (addr, s) = state |> advancePc 1 |> fetch16
            let value = Cpu.getRegisterValue (R8 A) s.Regs
            let newState = writeByte addr (byte value) s
            (newState, 16)
        | LdNNSP ->
            let (addr, s) = state |> advancePc 1 |> fetch16
            let value = Cpu.getRegisterValue (R16 SP) s.Regs
            let newState = write16 addr value s
            (newState, 20)

        | LdhAN ->
            let (offset, s) = state |> advancePc 1 |> fetchByte
            let addr = 0xFF00us + uint16 offset
            let value = readByte addr s
            let newState = setRegs (Cpu.LoadN8 A (uint16 value) s.Regs) s
            (newState, 12)
        | LdhNA ->
            let (offset, s) = state |> advancePc 1 |> fetchByte
            let addr = 0xFF00us + uint16 offset
            let value = Cpu.getRegisterValue (R8 A) s.Regs
            let newState = writeByte addr (byte value) s
            (newState, 12)
        | LdhAC ->
            let s = advancePc 1 state
            let offset = Cpu.getRegisterValue (R8 C) s.Regs
            let addr = 0xFF00us + offset
            let value = readByte addr s
            let newState = setRegs (Cpu.LoadN8 A (uint16 value) s.Regs) s
            (newState, 8)
        | LdhCA ->
            let s = advancePc 1 state
            let offset = Cpu.getRegisterValue (R8 C) s.Regs
            let addr = 0xFF00us + offset
            let value = Cpu.getRegisterValue (R8 A) s.Regs
            let newState = writeByte addr (byte value) s
            (newState, 8)

        | LdSPHL ->
            let s = advancePc 1 state
            let newState = setRegs (Cpu.LoadH SP HL s.Regs) s
            (newState, 8)

        // 8-bit ALU
        | AddR regIdx ->
            let s = advancePc 1 state
            let newState =
                if regIdx = 6 then // ADD A, (HL)
                    let addr = Cpu.getRegisterValue (R16 HL) s.Regs
                    let value = readByte addr s
                    setRegs (Cpu.AddMem (uint16 value) s.Regs) s
                else
                    let reg = match r8map.[regIdx] with R8 r -> r | _ -> failwith "invalid reg"
                    setRegs (Cpu.AddR reg s.Regs) s
            let c = if regIdx = 6 then 8 else 4
            (newState, c)
        | AdcR regIdx ->
            let s = advancePc 1 state
            let newState =
                if regIdx = 6 then // ADC A, (HL)
                    let addr = Cpu.getRegisterValue (R16 HL) s.Regs
                    let value = readByte addr s
                    setRegs (Cpu.AdcMem (uint16 value) s.Regs) s
                else
                    let reg = match r8map.[regIdx] with R8 r -> r | _ -> failwith "invalid reg"
                    setRegs (Cpu.AdcR reg s.Regs) s
            let c = if regIdx = 6 then 8 else 4
            (newState, c)
        | SubR regIdx ->
            let s = advancePc 1 state
            let newState =
                if regIdx = 6 then // SUB (HL)
                    let addr = Cpu.getRegisterValue (R16 HL) s.Regs
                    let value = readByte addr s
                    setRegs (Cpu.SubMem (uint16 value) s.Regs) s
                else
                    let reg = match r8map.[regIdx] with R8 r -> r | _ -> failwith "invalid reg"
                    setRegs (Cpu.SubR reg s.Regs) s
            let c = if regIdx = 6 then 8 else 4
            (newState, c)
        | SbcR regIdx ->
            let s = advancePc 1 state
            let newState =
                if regIdx = 6 then // SBC A, (HL)
                    let addr = Cpu.getRegisterValue (R16 HL) s.Regs
                    let value = readByte addr s
                    setRegs (Cpu.SbcMem (uint16 value) s.Regs) s
                else
                    let reg = match r8map.[regIdx] with R8 r -> r | _ -> failwith "invalid reg"
                    setRegs (Cpu.SbcR reg s.Regs) s
            let c = if regIdx = 6 then 8 else 4
            (newState, c)
        | AndR regIdx ->
            let s = advancePc 1 state
            let newState =
                if regIdx = 6 then // AND (HL)
                    let addr = Cpu.getRegisterValue (R16 HL) s.Regs
                    let value = readByte addr s
                    setRegs (Cpu.AndMem (uint16 value) s.Regs) s
                else
                    let reg = match r8map.[regIdx] with R8 r -> r | _ -> failwith "invalid reg"
                    setRegs (Cpu.AndR reg s.Regs) s
            let c = if regIdx = 6 then 8 else 4
            (newState, c)
        | XorR regIdx ->
            let s = advancePc 1 state
            let newState =
                if regIdx = 6 then // XOR (HL)
                    let addr = Cpu.getRegisterValue (R16 HL) s.Regs
                    let value = readByte addr s
                    setRegs (Cpu.XorMem (uint16 value) s.Regs) s
                else
                    let reg = match r8map.[regIdx] with R8 r -> r | _ -> failwith "invalid reg"
                    setRegs (Cpu.XorR reg s.Regs) s
            let c = if regIdx = 6 then 8 else 4
            (newState, c)
        | OrR regIdx ->
            let s = advancePc 1 state
            let newState =
                if regIdx = 6 then // OR (HL)
                    let addr = Cpu.getRegisterValue (R16 HL) s.Regs
                    let value = readByte addr s
                    setRegs (Cpu.OrMem (uint16 value) s.Regs) s
                else
                    let reg = match r8map.[regIdx] with R8 r -> r | _ -> failwith "invalid reg"
                    setRegs (Cpu.OrR reg s.Regs) s
            let c = if regIdx = 6 then 8 else 4
            (newState, c)
        | CpR regIdx ->
            let s = advancePc 1 state
            let newState =
                if regIdx = 6 then // CP (HL)
                    let addr = Cpu.getRegisterValue (R16 HL) s.Regs
                    let value = readByte addr s
                    setRegs (Cpu.CpMem (uint16 value) s.Regs) s
                else
                    let reg = match r8map.[regIdx] with R8 r -> r | _ -> failwith "invalid reg"
                    setRegs (Cpu.CpR reg s.Regs) s
            let c = if regIdx = 6 then 8 else 4
            (newState, c)

        | AddN8 ->
            let (imm, s) = state |> advancePc 1 |> fetchByte
            let newState = setRegs (Cpu.AddN8 (uint16 imm) s.Regs) s
            (newState, 8)
        | AdcN8 ->
            let (imm, s) = state |> advancePc 1 |> fetchByte
            let newState = setRegs (Cpu.AdcN8 (uint16 imm) s.Regs) s
            (newState, 8)
        | SubN8 ->
            let (imm, s) = state |> advancePc 1 |> fetchByte
            let newState = setRegs (Cpu.SubN8 (uint16 imm) s.Regs) s
            (newState, 8)
        | SbcN8 ->
            let (imm, s) = state |> advancePc 1 |> fetchByte
            let newState = setRegs (Cpu.SbcN8 (uint16 imm) s.Regs) s
            (newState, 8)
        | AndN8 ->
            let (imm, s) = state |> advancePc 1 |> fetchByte
            let newState = setRegs (Cpu.AndN8 (uint16 imm) s.Regs) s
            (newState, 8)
        | XorN8 ->
            let (imm, s) = state |> advancePc 1 |> fetchByte
            let newState = setRegs (Cpu.XorN8 (uint16 imm) s.Regs) s
            (newState, 8)
        | OrN8 ->
            let (imm, s) = state |> advancePc 1 |> fetchByte
            let newState = setRegs (Cpu.OrN8 (uint16 imm) s.Regs) s
            (newState, 8)
        | CpN8 ->
            let (imm, s) = state |> advancePc 1 |> fetchByte
            let newState = setRegs (Cpu.CpN8 (uint16 imm) s.Regs) s
            (newState, 8)

        | IncR regIdx ->
            let s = advancePc 1 state
            let newState =
                if regIdx = 6 then // INC (HL)
                    let addr = Cpu.getRegisterValue (R16 HL) s.Regs
                    let value = readByte addr s
                    let (res, newRegs) = Cpu.IncMem (uint16 value) s.Regs
                    writeByte addr res { s with Regs = newRegs }
                else
                    let reg = match r8map.[regIdx] with R8 r -> r | _ -> failwith "invalid reg"
                    setRegs (Cpu.IncR reg s.Regs) s
            (newState, 12)
        | DecR regIdx ->
            let s = advancePc 1 state
            let newState =
                if regIdx = 6 then // DEC (HL)
                    let addr = Cpu.getRegisterValue (R16 HL) s.Regs
                    let value = readByte addr s
                    let (res, newRegs) = Cpu.DecMem (uint16 value) s.Regs
                    writeByte addr res { s with Regs = newRegs }
                else
                    let reg = match r8map.[regIdx] with R8 r -> r | _ -> failwith "invalid reg"
                    setRegs (Cpu.DecR reg s.Regs) s
            (newState, 12)
        
        // 16-bit Inc/Dec
        | Inc16 reg ->
            let s = advancePc 1 state
            let newState = setRegs (Cpu.Inc16 reg s.Regs) s
            (newState, 8)
        | Dec16 reg ->
            let s = advancePc 1 state
            let newState = setRegs (Cpu.Dec16 reg s.Regs) s
            (newState, 8)
        
        | AddHL reg ->
            let s = advancePc 1 state
            let newState = setRegs (Cpu.AddHL reg s.Regs) s
            (newState, 8)

        // Jumps
        | JpNN ->
            let (addr, s) = state |> advancePc 1 |> fetch16
            let newState = setRegs (Cpu.Jp addr s.Regs) s
            (newState, 16)
        | JpNZ | JpZ | JpNC | JpC ->
            let (addr, s) = state |> advancePc 1 |> fetch16
            let cond = 
                match opcode with 
                | 0xC2uy -> NZ | 0xCAuy -> Z | 0xD2uy -> NC | _ -> CC
            if Cpu.checkCondition cond s.Regs then
                let newState = setRegs (Cpu.Jp addr s.Regs) s
                (newState, 16)
            else
                (s, 12)
        | JpHL ->
            let s = advancePc 1 state
            let newState = setRegs (Cpu.JpHL s.Regs) s
            (newState, 4)
        | JrE8 ->
            let (offset, s) = state |> advancePc 1 |> fetchByte
            let newState = setRegs (Cpu.Jr offset s.Regs) s
            (newState, 12)
        | JrNZ | JrZ | JrNC | JrC ->
            let (offset, s) = state |> advancePc 1 |> fetchByte
            let cond = 
                match opcode with
                | 0x20uy -> NZ | 0x28uy -> Z | 0x30uy -> NC | _ -> CC
            if Cpu.checkCondition cond s.Regs then
                let newState = setRegs (Cpu.Jr offset s.Regs) s
                (newState, 12)
            else
                (s, 8)

        // Calls
        | CallNN ->
            let (addr, s) = state |> advancePc 1 |> fetch16
            let (sp, pc, newRegs) = Cpu.Call addr s.Regs
            let newState = write16 sp pc { s with Regs = newRegs }
            (newState, 24)
        | CallNZ | CallZ | CallNC | CallC ->
            let (addr, s) = state |> advancePc 1 |> fetch16
            let cond =
                match opcode with
                | 0xC4uy -> NZ | 0xCCuy -> Z | 0xD4uy -> NC | _ -> CC
            if Cpu.checkCondition cond s.Regs then
                let (sp, pc, newRegs) = Cpu.Call addr s.Regs
                let newState = write16 sp pc { s with Regs = newRegs }
                (newState, 24)
            else
                (s, 12)

        // Returns
        | Ret ->
            let s = advancePc 1 state
            let retAddr = read16 s.Regs.SP s
            let newState = setRegs (Cpu.Ret retAddr s.Regs) s
            (newState, 16)
        | RetNZ | RetZ | RetNC | RetC ->
            let s = advancePc 1 state
            let cond =
                match opcode with
                | 0xC0uy -> NZ | 0xC8uy -> Z | 0xD0uy -> NC | _ -> CC
            if Cpu.checkCondition cond s.Regs then
                let retAddr = read16 s.Regs.SP s
                let newState = setRegs (Cpu.Ret retAddr s.Regs) s
                (newState, 20)
            else
                (s, 8)
        | Reti ->
            let s = advancePc 1 state
            let retAddr = read16 s.Regs.SP s
            let newState = setRegs (Cpu.Ret retAddr s.Regs) { s with Ime = true }
            (newState, 16)

        // Rst
        | Rst00 | Rst08 | Rst10 | Rst18 | Rst20 | Rst28 | Rst30 | Rst38 ->
            let s = advancePc 1 state
            let addr = opcode &&& 0x38uy
            let (sp, pc, newRegs) = Cpu.Rst addr s.Regs
            let newState = write16 sp pc { s with Regs = newRegs }
            (newState, 16)

        // Stack
        | PushBC | PushDE | PushHL | PushAF ->
            let s = advancePc 1 state
            let reg = 
                match opcode with
                | 0xC5uy -> BC | 0xD5uy -> DE | 0xE5uy -> HL | _ -> AF
            let value = Cpu.getRegisterValue (R16 reg) s.Regs
            let (sp, _, newRegs) = Cpu.Push value s.Regs
            let newState = write16 sp value { s with Regs = newRegs }
            (newState, 16)
        | PopBC | PopDE | PopHL | PopAF ->
            let s = advancePc 1 state
            let reg = 
                match opcode with
                | 0xC1uy -> BC | 0xD1uy -> DE | 0xE1uy -> HL | _ -> AF
            let value = read16 s.Regs.SP s
            let (_, newRegs) = Cpu.Pop value s.Regs
            let newState = setRegs (Cpu.setRegisterValue (R16 reg) value newRegs) s
            (newState, 12)

        // Misc
        | Rlca ->
            let s = advancePc 1 state
            let newState = setRegs (Cpu.Rlca s.Regs) s
            (newState, 4)
        | Rrca ->
            let s = advancePc 1 state
            let newState = setRegs (Cpu.Rrca s.Regs) s
            (newState, 4)
        | Rla ->
            let s = advancePc 1 state
            let newState = setRegs (Cpu.Rla s.Regs) s
            (newState, 4)
        | Rra ->
            let s = advancePc 1 state
            let newState = setRegs (Cpu.Rra s.Regs) s
            (newState, 4)
        | Daa ->
            let s = advancePc 1 state
            let newState = setRegs (Cpu.Daa s.Regs) s
            (newState, 4)
        | Cpl ->
            let s = advancePc 1 state
            let newState = setRegs (Cpu.Cpl s.Regs) s
            (newState, 4)
        | Scf ->
            let s = advancePc 1 state
            let newState = setRegs (Cpu.Scf s.Regs) s
            (newState, 4)
        | Ccf ->
            let s = advancePc 1 state
            let newState = setRegs (Cpu.Ccf s.Regs) s
            (newState, 4)
        
        // Interrupts
        | Di -> ({ (advancePc 1 state) with Ime = false }, 4)
        | Ei -> ({ (advancePc 1 state) with Ime = true }, 4)

        | _ ->
          // TODO: Log this properly
          // eprintfn "Unimplemented opcode: 0x%02X at 0x%04X" opcode state.Regs.PC
          (advancePc 1 state, 4) // Default for unimplemented

      // PPUをCPUサイクル分進める
      let (ppu, mem) = Ppu.step cycles newState.Ppu newState.Mem
      { newState with Ppu = ppu; Mem = mem }

  // Run until halted or max steps
  let run maxSteps (state: CpuState) =
    let rec loop steps state =
      if steps >= maxSteps || state.Halted then state
      else loop (steps + 1) (step state)
    loop 0 state

  // Legacy Decode function for compatibility with existing tests
  let rec Decode (memory: byte list) (reg: Cpu.Register) =
    let mem = Memory.create()
    let romArray = List.toArray memory
    let mem = Memory.loadRom romArray mem
    let state = { Regs = reg; Mem = mem; Ppu = Ppu.create(); Ime = false; Halted = false }
    let finalState = run (List.length memory * 2) state
    finalState.Regs
