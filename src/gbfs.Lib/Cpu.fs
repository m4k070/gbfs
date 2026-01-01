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
    let flagVal = ((b2us z <<< 7) ||| (b2us n <<< 6) ||| (b2us h <<< 5) ||| b2us c <<< 4)
    regs |>
    setRegisterValue (R8 F) flagVal

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

