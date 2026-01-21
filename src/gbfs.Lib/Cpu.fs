namespace gbfs.Lib

module Cpu =
  type Reg8 = | A | B | C | D | E | F | H | L

  type Reg16 = | AF | BC | DE | HL | PC | SP

  type Reg =
    | R8 of Reg8
    | R16 of Reg16

  type Register = {
    AF : uint16; // accumlator & flag
    BC : uint16;
    DE : uint16;
    HL : uint16;
    PC : uint16;
    SP : uint16;
  }

  let getRegisterValue (key: Reg) (regs: Register) =
    match key with
    | R8 A -> regs.AF >>> 8 &&& 0x00_FFus
    | R8 B -> regs.BC >>> 8 &&& 0x00_FFus
    | R8 C -> regs.BC &&& 0x00_FFus
    | R8 D -> regs.DE >>> 8 &&& 0x00_FFus
    | R8 E -> regs.DE &&& 0x00_FFus
    | R8 F -> regs.AF &&& 0x00_FFus
    | R8 H -> regs.HL >>> 8 &&& 0x00_FFus
    | R8 L -> regs.HL &&& 0x00_FFus
    | R16 AF -> regs.AF
    | R16 BC -> regs.BC
    | R16 DE -> regs.DE
    | R16 HL -> regs.HL
    | R16 PC -> regs.PC
    | R16 SP -> regs.SP

  let setRegisterValue (key: Reg) value (regs: Register) =
    match key with
    | R8 A -> { regs with AF = (regs.AF &&& 0x00FFus) ||| (value <<< 8) }
    | R8 F -> { regs with AF = (regs.AF &&& 0xFF00us) ||| (value &&& 0x00F0us) }
    | R8 B -> { regs with BC = (regs.BC &&& 0x00FFus) ||| (value <<< 8) }
    | R8 C -> { regs with BC = (regs.BC &&& 0xFF00us) ||| (value &&& 0x00FFus) }
    | R8 D -> { regs with DE = (regs.DE &&& 0x00FFus) ||| (value <<< 8) }
    | R8 E -> { regs with DE = (regs.DE &&& 0xFF00us) ||| (value &&& 0x00FFus) }
    | R8 H -> { regs with HL = (regs.HL &&& 0x00FFus) ||| (value <<< 8) }
    | R8 L -> { regs with HL = (regs.HL &&& 0xFF00us) ||| (value &&& 0x00FFus) }
    | R16 AF -> { regs with AF = value &&& 0xFFF0us }
    | R16 BC -> { regs with BC = value }
    | R16 DE -> { regs with DE = value }
    | R16 HL -> { regs with HL = value }
    | R16 PC -> { regs with PC = value }
    | R16 SP -> { regs with SP = value }

  let b2us value = 
    match value with
    | true -> 1us
    | false -> 0us

  let setFlag z n h c regs =
    let flagVal = (b2us z <<< 7) ||| (b2us n <<< 6) ||| (b2us h <<< 5) ||| (b2us c <<< 4)
    regs |> setRegisterValue (R8 F) flagVal

  let isZ regs = 0x80us &&& regs.AF <> 0us
  let isN regs = 0x40us &&& regs.AF <> 0us
  let isH regs = 0x20us &&& regs.AF <> 0us
  let isC regs = 0x10us &&& regs.AF <> 0us

  // NOP
  let Nop (reg: Register) = reg

  // 8bit load
  let LoadR (dst: Reg8) (src: Reg8) regs =
    let value = getRegisterValue (R8 src) regs
    regs |> setRegisterValue (R8 dst) value

  let LoadN8 (dst: Reg8) value regs =
    regs |> setRegisterValue (R8 dst) value

  // 16bit load
  let LoadH (dst: Reg16) (src: Reg16) regs =
    let value = getRegisterValue (R16 src) regs
    regs |> setRegisterValue (R16 dst) value

  let LoadN16 (dst: Reg16) value regs =
    regs |> setRegisterValue (R16 dst) value

  // 算術
  let hasHalfCarryAdd8 a b = ((a &&& 0x0Fus) + (b &&& 0x0Fus)) > 0x0Fus
  let hasHalfCarryAdd16 a b = ((a &&& 0x0FFFus) + (b &&& 0x0FFFus)) > 0x0FFFus
  let hasHalfCarrySub8 a b = (a &&& 0x0Fus) < (b &&& 0x0Fus)
  let hasHalfCarrySub16 a b = (a &&& 0x0FFFus) < (b &&& 0x0FFFus)

  let AddR (b: Reg8) regs =
    let left = regs |> getRegisterValue (R8 A)
    let right = regs |> getRegisterValue (R8 b)
    let s = left + right
    let z = (s = 0us)
    let hc = hasHalfCarryAdd8 left right
    let c = (left + right) > 0xFFus
    regs |> setRegisterValue (R8 A) s |> setFlag z false hc c

  let AddH a b regs =
    let left = getRegisterValue (R16 a) regs
    let right = getRegisterValue (R16 b) regs
    let s = left + right
    let z = (s = 0us)
    let hc = hasHalfCarryAdd16 left right
    let c = (uint(left) + uint(right)) > 0xFFFFul
    regs |> setRegisterValue (R16 a) s |> setFlag z false hc c

  let AddN8 b regs =
    let a = getRegisterValue (R8 A) regs
    let s = a + b
    let z = (s = 0us)
    let hc = hasHalfCarryAdd8 a b
    let c = (a + b) > 0xFFus
    regs |> setRegisterValue (R8 A) s |> setFlag z false hc c

  let AddN16 b regs =
    let a = getRegisterValue (R8 A) regs
    let s = a + b
    let z = isZ regs // Zフラグは変化しない
    let hc = hasHalfCarryAdd16 a b
    let c = (a + b) > 0xFFFFus
    regs |> setRegisterValue (R8 A) s |> setFlag z false hc c

  let SubR b regs =
    let left = getRegisterValue (R8 A) regs
    let right = getRegisterValue (R8 b) regs
    let s = left - right
    let z = (s = 0us)
    let hc = hasHalfCarrySub8 left right
    let c = left < right
    regs |> setRegisterValue (R8 A) s |> setFlag z true hc c

  let SubH regs a b =
    let left = getRegisterValue (R16 a) regs
    let right = getRegisterValue (R16 b) regs
    let s = left - right
    let z = (s = 0us)
    let hc = hasHalfCarrySub16 left right
    regs |> setRegisterValue (R16 a) s |> setFlag z true hc false

  let SubN8 b regs =
    let a = getRegisterValue (R8 A) regs
    let s = a - b
    let z = (s &&& 0xFFus) = 0us
    let hc = hasHalfCarrySub8 a b
    let c = a < b
    regs |> setRegisterValue (R8 A) s |> setFlag z true hc c

  // ADC: Add with Carry
  let AdcR (b: Reg8) regs =
    let left = regs |> getRegisterValue (R8 A)
    let right = regs |> getRegisterValue (R8 b)
    let carry = if isC regs then 1us else 0us
    let s = left + right + carry
    let z = (s &&& 0xFFus) = 0us
    let hc = ((left &&& 0x0Fus) + (right &&& 0x0Fus) + carry) > 0x0Fus
    let c = (left + right + carry) > 0xFFus
    regs |> setRegisterValue (R8 A) s |> setFlag z false hc c

  let AdcN8 b regs =
    let a = getRegisterValue (R8 A) regs
    let carry = if isC regs then 1us else 0us
    let s = a + b + carry
    let z = (s &&& 0xFFus) = 0us
    let hc = ((a &&& 0x0Fus) + (b &&& 0x0Fus) + carry) > 0x0Fus
    let c = (a + b + carry) > 0xFFus
    regs |> setRegisterValue (R8 A) s |> setFlag z false hc c

  // SBC: Subtract with Carry
  let SbcR (b: Reg8) regs =
    let left = regs |> getRegisterValue (R8 A)
    let right = regs |> getRegisterValue (R8 b)
    let carry = if isC regs then 1us else 0us
    let s = left - right - carry
    let z = (s &&& 0xFFus) = 0us
    let hc = (left &&& 0x0Fus) < ((right &&& 0x0Fus) + carry)
    let c = left < (right + carry)
    regs |> setRegisterValue (R8 A) s |> setFlag z true hc c

  let SbcN8 b regs =
    let a = getRegisterValue (R8 A) regs
    let carry = if isC regs then 1us else 0us
    let s = a - b - carry
    let z = (s &&& 0xFFus) = 0us
    let hc = (a &&& 0x0Fus) < ((b &&& 0x0Fus) + carry)
    let c = a < (b + carry)
    regs |> setRegisterValue (R8 A) s |> setFlag z true hc c

  // AND: Logical AND
  let AndR (b: Reg8) regs =
    let left = regs |> getRegisterValue (R8 A)
    let right = regs |> getRegisterValue (R8 b)
    let s = left &&& right
    let z = s = 0us
    regs |> setRegisterValue (R8 A) s |> setFlag z false true false

  let AndN8 b regs =
    let a = getRegisterValue (R8 A) regs
    let s = a &&& b
    let z = s = 0us
    regs |> setRegisterValue (R8 A) s |> setFlag z false true false

  // OR: Logical OR
  let OrR (b: Reg8) regs =
    let left = regs |> getRegisterValue (R8 A)
    let right = regs |> getRegisterValue (R8 b)
    let s = left ||| right
    let z = s = 0us
    regs |> setRegisterValue (R8 A) s |> setFlag z false false false

  let OrN8 b regs =
    let a = getRegisterValue (R8 A) regs
    let s = a ||| b
    let z = s = 0us
    regs |> setRegisterValue (R8 A) s |> setFlag z false false false

  // XOR: Logical XOR
  let XorR (b: Reg8) regs =
    let left = regs |> getRegisterValue (R8 A)
    let right = regs |> getRegisterValue (R8 b)
    let s = left ^^^ right
    let z = s = 0us
    regs |> setRegisterValue (R8 A) s |> setFlag z false false false

  let XorN8 b regs =
    let a = getRegisterValue (R8 A) regs
    let s = a ^^^ b
    let z = s = 0us
    regs |> setRegisterValue (R8 A) s |> setFlag z false false false

  // CP: Compare (A - r, result discarded)
  let CpR (b: Reg8) regs =
    let left = regs |> getRegisterValue (R8 A)
    let right = regs |> getRegisterValue (R8 b)
    let s = left - right
    let z = (s &&& 0xFFus) = 0us
    let hc = hasHalfCarrySub8 left right
    let c = left < right
    regs |> setFlag z true hc c

  let CpN8 b regs =
    let a = getRegisterValue (R8 A) regs
    let s = a - b
    let z = (s &&& 0xFFus) = 0us
    let hc = hasHalfCarrySub8 a b
    let c = a < b
    regs |> setFlag z true hc c

  // INC r: Increment register (C flag unchanged)
  let IncR (r: Reg8) regs =
    let value = regs |> getRegisterValue (R8 r)
    let s = value + 1us
    let z = (s &&& 0xFFus) = 0us
    let hc = (value &&& 0x0Fus) = 0x0Fus
    let c = isC regs // C flag unchanged
    regs |> setRegisterValue (R8 r) s |> setFlag z false hc c

  // DEC r: Decrement register (C flag unchanged)
  let DecR (r: Reg8) regs =
    let value = regs |> getRegisterValue (R8 r)
    let s = value - 1us
    let z = (s &&& 0xFFus) = 0us
    let hc = (value &&& 0x0Fus) = 0x00us
    let c = isC regs // C flag unchanged
    regs |> setRegisterValue (R8 r) s |> setFlag z true hc c

  // 16bit arithmetic
  // INC rr: Increment 16-bit register (no flags affected)
  let Inc16 (r: Reg16) regs =
    let value = regs |> getRegisterValue (R16 r)
    regs |> setRegisterValue (R16 r) (value + 1us)

  // DEC rr: Decrement 16-bit register (no flags affected)
  let Dec16 (r: Reg16) regs =
    let value = regs |> getRegisterValue (R16 r)
    regs |> setRegisterValue (R16 r) (value - 1us)

  // ADD HL,rr: Add 16-bit register to HL (Z unchanged, N=0, H=half carry, C=carry)
  let AddHL (r: Reg16) regs =
    let left = regs |> getRegisterValue (R16 HL)
    let right = regs |> getRegisterValue (R16 r)
    let s = left + right
    let z = isZ regs // Z flag unchanged
    let hc = hasHalfCarryAdd16 left right
    let c = (uint left + uint right) > 0xFFFFu
    regs |> setRegisterValue (R16 HL) s |> setFlag z false hc c

  // Rotate instructions
  // RLCA: Rotate A left, old bit 7 to carry and bit 0
  let Rlca regs =
    let a = regs |> getRegisterValue (R8 A)
    let bit7 = (a >>> 7) &&& 1us
    let s = ((a <<< 1) ||| bit7) &&& 0xFFus
    let c = bit7 = 1us
    regs |> setRegisterValue (R8 A) s |> setFlag false false false c

  // RRCA: Rotate A right, old bit 0 to carry and bit 7
  let Rrca regs =
    let a = regs |> getRegisterValue (R8 A)
    let bit0 = a &&& 1us
    let s = ((a >>> 1) ||| (bit0 <<< 7)) &&& 0xFFus
    let c = bit0 = 1us
    regs |> setRegisterValue (R8 A) s |> setFlag false false false c

  // RLA: Rotate A left through carry
  let Rla regs =
    let a = regs |> getRegisterValue (R8 A)
    let oldCarry = if isC regs then 1us else 0us
    let bit7 = (a >>> 7) &&& 1us
    let s = ((a <<< 1) ||| oldCarry) &&& 0xFFus
    let c = bit7 = 1us
    regs |> setRegisterValue (R8 A) s |> setFlag false false false c

  // RRA: Rotate A right through carry
  let Rra regs =
    let a = regs |> getRegisterValue (R8 A)
    let oldCarry = if isC regs then 0x80us else 0us
    let bit0 = a &&& 1us
    let s = ((a >>> 1) ||| oldCarry) &&& 0xFFus
    let c = bit0 = 1us
    regs |> setRegisterValue (R8 A) s |> setFlag false false false c

  // Misc instructions
  // CPL: Complement A (flip all bits)
  let Cpl regs =
    let a = regs |> getRegisterValue (R8 A)
    let s = (~~~a) &&& 0xFFus
    let z = isZ regs // Z unchanged
    let c = isC regs // C unchanged
    regs |> setRegisterValue (R8 A) s |> setFlag z true true c

  // CCF: Complement Carry Flag
  let Ccf regs =
    let z = isZ regs // Z unchanged
    let c = not (isC regs)
    regs |> setFlag z false false c

  // SCF: Set Carry Flag
  let Scf regs =
    let z = isZ regs // Z unchanged
    regs |> setFlag z false false true

  // DAA: Decimal Adjust Accumulator
  let Daa regs =
    let a = regs |> getRegisterValue (R8 A)
    let n = isN regs
    let h = isH regs
    let c = isC regs
    let mutable result = int a
    let mutable newC = c

    if n then
      // After subtraction
      if h then result <- result - 0x06
      if c then result <- result - 0x60
    else
      // After addition
      if h || (result &&& 0x0F) > 0x09 then result <- result + 0x06
      if c || result > 0x9F then
        result <- result + 0x60
        newC <- true

    let s = uint16 (result &&& 0xFF)
    let z = s = 0us
    regs |> setRegisterValue (R8 A) s |> setFlag z n false newC

  // ====================
  // メモリアクセス命令用の関数 (値を引数で受け取る)
  // ====================

  // ADD A,(HL) - メモリから読んだ値を加算
  let AddMem value regs =
    let a = regs |> getRegisterValue (R8 A)
    let s = a + value
    let z = (s &&& 0xFFus) = 0us
    let hc = hasHalfCarryAdd8 a value
    let c = (a + value) > 0xFFus
    regs |> setRegisterValue (R8 A) s |> setFlag z false hc c

  // ADC A,(HL)
  let AdcMem value regs =
    let a = regs |> getRegisterValue (R8 A)
    let carry = if isC regs then 1us else 0us
    let s = a + value + carry
    let z = (s &&& 0xFFus) = 0us
    let hc = ((a &&& 0x0Fus) + (value &&& 0x0Fus) + carry) > 0x0Fus
    let c = (a + value + carry) > 0xFFus
    regs |> setRegisterValue (R8 A) s |> setFlag z false hc c

  // SUB (HL)
  let SubMem value regs =
    let a = regs |> getRegisterValue (R8 A)
    let s = a - value
    let z = (s &&& 0xFFus) = 0us
    let hc = hasHalfCarrySub8 a value
    let c = a < value
    regs |> setRegisterValue (R8 A) s |> setFlag z true hc c

  // SBC A,(HL)
  let SbcMem value regs =
    let a = regs |> getRegisterValue (R8 A)
    let carry = if isC regs then 1us else 0us
    let s = a - value - carry
    let z = (s &&& 0xFFus) = 0us
    let hc = (a &&& 0x0Fus) < ((value &&& 0x0Fus) + carry)
    let c = a < (value + carry)
    regs |> setRegisterValue (R8 A) s |> setFlag z true hc c

  // AND (HL)
  let AndMem value regs =
    let a = regs |> getRegisterValue (R8 A)
    let s = a &&& value
    let z = s = 0us
    regs |> setRegisterValue (R8 A) s |> setFlag z false true false

  // OR (HL)
  let OrMem value regs =
    let a = regs |> getRegisterValue (R8 A)
    let s = a ||| value
    let z = s = 0us
    regs |> setRegisterValue (R8 A) s |> setFlag z false false false

  // XOR (HL)
  let XorMem value regs =
    let a = regs |> getRegisterValue (R8 A)
    let s = a ^^^ value
    let z = s = 0us
    regs |> setRegisterValue (R8 A) s |> setFlag z false false false

  // CP (HL)
  let CpMem value regs =
    let a = regs |> getRegisterValue (R8 A)
    let s = a - value
    let z = (s &&& 0xFFus) = 0us
    let hc = hasHalfCarrySub8 a value
    let c = a < value
    regs |> setFlag z true hc c

  // INC (HL) - 戻り値は新しいメモリ値
  let IncMem value regs =
    let s = (value + 1us) &&& 0xFFus
    let z = s = 0us
    let hc = (value &&& 0x0Fus) = 0x0Fus
    let c = isC regs
    let newRegs = regs |> setFlag z false hc c
    (byte s, newRegs)

  // DEC (HL) - 戻り値は新しいメモリ値
  let DecMem value regs =
    let s = (value - 1us) &&& 0xFFus
    let z = s = 0us
    let hc = (value &&& 0x0Fus) = 0x00us
    let c = isC regs
    let newRegs = regs |> setFlag z true hc c
    (byte s, newRegs)

  // ====================
  // ジャンプ命令
  // ====================

  // JP nn - 絶対ジャンプ
  let Jp addr regs =
    regs |> setRegisterValue (R16 PC) addr

  // JP (HL) - HLアドレスにジャンプ
  let JpHL regs =
    let addr = regs |> getRegisterValue (R16 HL)
    regs |> setRegisterValue (R16 PC) addr

  // JR e8 - 相対ジャンプ (e8 は signed offset)
  let Jr (offset: byte) regs =
    let pc = regs |> getRegisterValue (R16 PC)
    // offset は int8 として扱う (byteをsbyteに変換)
    let signedOffset = sbyte offset
    let newPc = uint16 (int pc + int signedOffset)
    regs |> setRegisterValue (R16 PC) newPc

  // 条件チェック
  type Condition = | NZ | Z | NC | CC

  let checkCondition cond regs =
    match cond with
    | NZ -> not (isZ regs)
    | Z -> isZ regs
    | NC -> not (isC regs)
    | CC -> isC regs

  // ====================
  // スタック命令
  // ====================

  // PUSH - SPを減らして値を格納 (戻り値: 新しいSPとレジスタ)
  let Push value regs =
    let sp = (regs |> getRegisterValue (R16 SP)) - 2us
    let newRegs = regs |> setRegisterValue (R16 SP) sp
    (sp, value, newRegs)

  // POP - スタックから値を取り出してSPを増やす
  let Pop value regs =
    let sp = regs |> getRegisterValue (R16 SP)
    let newSp = sp + 2us
    let newRegs = regs |> setRegisterValue (R16 SP) newSp
    (value, newRegs)

  // CALL - PCをスタックにプッシュしてジャンプ (PCは命令の次を指している前提)
  let Call addr regs =
    let pc = regs |> getRegisterValue (R16 PC)
    let sp = (regs |> getRegisterValue (R16 SP)) - 2us
    let newRegs = regs |> setRegisterValue (R16 SP) sp |> setRegisterValue (R16 PC) addr
    (sp, pc, newRegs)

  // RET - スタックからPCをポップ
  let Ret retAddr regs =
    let sp = regs |> getRegisterValue (R16 SP)
    let newSp = sp + 2us
    regs |> setRegisterValue (R16 SP) newSp |> setRegisterValue (R16 PC) retAddr

  // RST n - 固定アドレスへのCALL
  let Rst (addr: byte) regs =
    let pc = regs |> getRegisterValue (R16 PC)
    let sp = (regs |> getRegisterValue (R16 SP)) - 2us
    let newRegs = regs |> setRegisterValue (R16 SP) sp |> setRegisterValue (R16 PC) (uint16 addr)
    (sp, pc, newRegs)

  // ====================
  // 初期状態
  // ====================

  let initRegister () =
    // ゲームボーイ起動時の初期値
    { AF = 0x01B0us  // A=0x01, F=0xB0 (Z=1, N=0, H=1, C=1)
      BC = 0x0013us
      DE = 0x00D8us
      HL = 0x014Dus
      PC = 0x0100us  // エントリポイント
      SP = 0xFFFEus }

  // HALT状態
  let mutable Halted = false

  let Halt regs =
    Halted <- true
    regs
