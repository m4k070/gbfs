namespace gbfs.Lib

open Cpu

module CBPrefix =
    // RLC r: Rotate Left
    let Rlc value regs =
        let carry = (value >>> 7) &&& 1us
        let result = ((value <<< 1) ||| carry) &&& 0xFFus
        let z = result = 0us
        let c = carry = 1us
        let newRegs = regs |> setFlag z false false c
        (result, newRegs)

    // RRC r: Rotate Right
    let Rrc value regs =
        let carry = value &&& 1us
        let result = ((value >>> 1) ||| (carry <<< 7)) &&& 0xFFus
        let z = result = 0us
        let c = carry = 1us
        let newRegs = regs |> setFlag z false false c
        (result, newRegs)

    // RL r: Rotate Left through Carry
    let Rl value regs =
        let oldCarry = if isC regs then 1us else 0us
        let newCarry = (value >>> 7) &&& 1us
        let result = ((value <<< 1) ||| oldCarry) &&& 0xFFus
        let z = result = 0us
        let c = newCarry = 1us
        let newRegs = regs |> setFlag z false false c
        (result, newRegs)

    // RR r: Rotate Right through Carry
    let Rr value regs =
        let oldCarry = if isC regs then 0x80us else 0us
        let newCarry = value &&& 1us
        let result = ((value >>> 1) ||| oldCarry) &&& 0xFFus
        let z = result = 0us
        let c = newCarry = 1us
        let newRegs = regs |> setFlag z false false c
        (result, newRegs)

    // SLA r: Shift Left Arithmetic
    let Sla value regs =
        let carry = (value >>> 7) &&& 1us
        let result = (value <<< 1) &&& 0xFFus
        let z = result = 0us
        let c = carry = 1us
        let newRegs = regs |> setFlag z false false c
        (result, newRegs)

    // SRA r: Shift Right Arithmetic
    let Sra value regs =
        let bit7 = value &&& 0x80us // MSB
        let carry = value &&& 1us
        let result = ((value >>> 1) ||| bit7) &&& 0xFFus
        let z = result = 0us
        let c = carry = 1us
        let newRegs = regs |> setFlag z false false c
        (result, newRegs)

    // SRL r: Shift Right Logical
    let Srl value regs =
        let carry = value &&& 1us
        let result = (value >>> 1) &&& 0xFFus
        let z = result = 0us
        let c = carry = 1us
        let newRegs = regs |> setFlag z false false c
        (result, newRegs)

    // SWAP r: Swap nibbles
    let Swap value regs =
        let lo = value &&& 0x0Fus
        let hi = value &&& 0xF0us
        let result = (lo <<< 4) ||| (hi >>> 4)
        let z = result = 0us
        let newRegs = regs |> setFlag z false false false
        (result, newRegs)

    // BIT n, r: Test bit n of register r
    let Bit (bit: int) value regs =
        let mask = 1us <<< bit
        let z = (value &&& mask) = 0us
        let c = isC regs // C flag is not affected
        regs |> setFlag z false true c

    // RES n, r: Reset bit n of register r
    let Res (bit: int) value =
        let mask = ~~~(1us <<< bit)
        (value &&& mask) &&& 0xFFus

    // SET n, r: Set bit n of register r
    let Set (bit: int) value =
        let mask = 1us <<< bit
        (value ||| mask) &&& 0xFFus
