namespace gbfs.Lib

module Cpu =
  type Reg8 =
    | A
    | B
    | C
    | D
    | E
    | F
    | H
    | L

  type Reg16 =
    | AF
    | BC
    | DE
    | HL
    | PC
    | SP

  type Reg =
    | R8 of Reg8
    | R16 of Reg16

  type Register = {
    AF : uint16; // accumlator
    BC : uint16;
    DE : uint16;
    HL : uint16;
    PC : uint16;
    SP : uint16;
  }

  let getRegisterValue (regs: Register) (key: Reg) =
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

  let setRegisterValue (regs: Register) (key: Reg) value =
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

  let setZ regs value = 0x70us &&& (b2us value <<< 3) |> setRegisterValue regs (R8 F)
  let setN regs value = 0xB0us &&& (b2us value <<< 2) |>  setRegisterValue regs (R8 F)
  let setH regs value = 0xD0us &&& (b2us value <<< 1) |>  setRegisterValue regs (R8 F)
  let setC regs value = 0xE0us &&& b2us value |>  setRegisterValue regs (R8 F)

  let isZ regs = 0x0080us &&& regs.AF = 1us
  let isN regs = 0x0040us &&& regs.AF = 1us
  let isH regs = 0x0020us &&& regs.AF = 1us
  let isC regs = 0x0010us &&& regs.AF = 1us

  let nop registers memory = registers memory 

  // 8bit load
  let ld8 regs (dst: Reg) (src: Reg) =
    let value = getRegisterValue regs src
    setRegisterValue regs dst value

  let ld8imm regs memory (dst: Reg) src =
    setRegisterValue regs dst src

  let ld16 regs dst src =
    let value = getRegisterValue regs src
    setRegisterValue regs dst value

  // 算術
