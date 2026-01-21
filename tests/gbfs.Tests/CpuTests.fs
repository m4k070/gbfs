module CpuTests

open Xunit
open gbfs.Lib.Cpu

/// 初期状態のレジスタを作成
let initRegister () =
    { AF = 0us; BC = 0us; DE = 0us; HL = 0us; PC = 0us; SP = 0us }

// ====================
// レジスタ値の取得/設定テスト
// ====================

module GetRegisterValueTests =
    [<Fact>]
    let ``getRegisterValue R8 A returns high byte of AF`` () =
        let regs = { initRegister() with AF = 0x1234us }
        let result = getRegisterValue (R8 A) regs
        Assert.Equal(0x12us, result)

    [<Fact>]
    let ``getRegisterValue R8 F returns low byte of AF`` () =
        let regs = { initRegister() with AF = 0x1234us }
        let result = getRegisterValue (R8 F) regs
        Assert.Equal(0x34us, result)

    [<Fact>]
    let ``getRegisterValue R8 B returns high byte of BC`` () =
        let regs = { initRegister() with BC = 0xABCDus }
        let result = getRegisterValue (R8 B) regs
        Assert.Equal(0xABus, result)

    [<Fact>]
    let ``getRegisterValue R8 C returns low byte of BC`` () =
        let regs = { initRegister() with BC = 0xABCDus }
        let result = getRegisterValue (R8 C) regs
        Assert.Equal(0xCDus, result)

    [<Fact>]
    let ``getRegisterValue R8 D returns high byte of DE`` () =
        let regs = { initRegister() with DE = 0x5678us }
        let result = getRegisterValue (R8 D) regs
        Assert.Equal(0x56us, result)

    [<Fact>]
    let ``getRegisterValue R8 E returns low byte of DE`` () =
        let regs = { initRegister() with DE = 0x5678us }
        let result = getRegisterValue (R8 E) regs
        Assert.Equal(0x78us, result)

    [<Fact>]
    let ``getRegisterValue R8 H returns high byte of HL`` () =
        let regs = { initRegister() with HL = 0x9ABCus }
        let result = getRegisterValue (R8 H) regs
        Assert.Equal(0x9Aus, result)

    [<Fact>]
    let ``getRegisterValue R8 L returns low byte of HL`` () =
        let regs = { initRegister() with HL = 0x9ABCus }
        let result = getRegisterValue (R8 L) regs
        Assert.Equal(0xBCus, result)

    [<Fact>]
    let ``getRegisterValue R16 returns full 16-bit value`` () =
        let regs = { initRegister() with BC = 0x1234us; DE = 0x5678us; HL = 0x9ABCus; SP = 0xDEF0us }
        Assert.Equal(0x1234us, getRegisterValue (R16 BC) regs)
        Assert.Equal(0x5678us, getRegisterValue (R16 DE) regs)
        Assert.Equal(0x9ABCus, getRegisterValue (R16 HL) regs)
        Assert.Equal(0xDEF0us, getRegisterValue (R16 SP) regs)

module SetRegisterValueTests =
    [<Fact>]
    let ``setRegisterValue R8 A sets high byte of AF`` () =
        let regs = initRegister() |> setRegisterValue (R8 A) 0x42us
        Assert.Equal(0x4200us, regs.AF)

    [<Fact>]
    let ``setRegisterValue R8 F sets low byte of AF with mask`` () =
        let regs = initRegister() |> setRegisterValue (R8 F) 0xFFus
        // Fレジスタの下位4ビットは常に0
        Assert.Equal(0x00F0us, regs.AF)

    [<Fact>]
    let ``setRegisterValue R8 B sets high byte of BC`` () =
        let regs = { initRegister() with BC = 0x00FFus } |> setRegisterValue (R8 B) 0xABus
        Assert.Equal(0xABFFus, regs.BC)

    [<Fact>]
    let ``setRegisterValue R8 C sets low byte of BC`` () =
        let regs = { initRegister() with BC = 0xFF00us } |> setRegisterValue (R8 C) 0xCDus
        Assert.Equal(0xFFCDus, regs.BC)

    [<Fact>]
    let ``setRegisterValue R16 AF masks lower 4 bits`` () =
        let regs = initRegister() |> setRegisterValue (R16 AF) 0xFFFFus
        Assert.Equal(0xFFF0us, regs.AF)

    [<Fact>]
    let ``setRegisterValue R16 BC sets full value`` () =
        let regs = initRegister() |> setRegisterValue (R16 BC) 0x1234us
        Assert.Equal(0x1234us, regs.BC)

// ====================
// フラグ操作テスト
// ====================

module FlagTests =
    [<Fact>]
    let ``setFlag sets all flags correctly`` () =
        let regs = initRegister() |> setFlag true true true true
        Assert.Equal(0xF0us, regs.AF &&& 0x00FFus)

    [<Fact>]
    let ``setFlag with Z=true sets bit 7`` () =
        let regs = initRegister() |> setFlag true false false false
        Assert.Equal(0x80us, regs.AF &&& 0x00FFus)

    [<Fact>]
    let ``setFlag with N=true sets bit 6`` () =
        let regs = initRegister() |> setFlag false true false false
        Assert.Equal(0x40us, regs.AF &&& 0x00FFus)

    [<Fact>]
    let ``setFlag with H=true sets bit 5`` () =
        let regs = initRegister() |> setFlag false false true false
        Assert.Equal(0x20us, regs.AF &&& 0x00FFus)

    [<Fact>]
    let ``setFlag with C=true sets bit 4`` () =
        let regs = initRegister() |> setFlag false false false true
        Assert.Equal(0x10us, regs.AF &&& 0x00FFus)

    [<Fact>]
    let ``isZ returns true when Z flag is set`` () =
        let regs = { initRegister() with AF = 0x0080us }
        Assert.True(isZ regs)

    [<Fact>]
    let ``isZ returns false when Z flag is not set`` () =
        let regs = { initRegister() with AF = 0x0000us }
        Assert.False(isZ regs)

    [<Fact>]
    let ``isN returns true when N flag is set`` () =
        let regs = { initRegister() with AF = 0x0040us }
        Assert.True(isN regs)

    [<Fact>]
    let ``isH returns true when H flag is set`` () =
        let regs = { initRegister() with AF = 0x0020us }
        Assert.True(isH regs)

    [<Fact>]
    let ``isC returns true when C flag is set`` () =
        let regs = { initRegister() with AF = 0x0010us }
        Assert.True(isC regs)

// ====================
// NOP命令テスト
// ====================

module NopTests =
    [<Fact>]
    let ``Nop does not modify registers`` () =
        let regs = { AF = 0x1234us; BC = 0x5678us; DE = 0x9ABCus; HL = 0xDEF0us; PC = 0x100us; SP = 0xFFFEus }
        let result = Nop regs
        Assert.Equal(regs, result)

// ====================
// 8bitロード命令テスト
// ====================

module Load8Tests =
    [<Fact>]
    let ``LoadR B A copies A to B`` () =
        let regs = { initRegister() with AF = 0x4200us }
        let result = LoadR B A regs
        Assert.Equal(0x42us, getRegisterValue (R8 B) result)

    [<Fact>]
    let ``LoadR C D copies D to C`` () =
        let regs = { initRegister() with DE = 0x3700us }
        let result = LoadR C D regs
        Assert.Equal(0x37us, getRegisterValue (R8 C) result)

    [<Fact>]
    let ``LoadR same register has no effect`` () =
        let regs = { initRegister() with AF = 0x5500us }
        let result = LoadR A A regs
        Assert.Equal(0x55us, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``LoadN8 loads immediate value to register`` () =
        let regs = initRegister()
        let result = LoadN8 A 0x42us regs
        Assert.Equal(0x42us, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``LoadN8 B loads to B register`` () =
        let regs = initRegister()
        let result = LoadN8 B 0xFFus regs
        Assert.Equal(0xFFus, getRegisterValue (R8 B) result)

// ====================
// 16bitロード命令テスト
// ====================

module Load16Tests =
    [<Fact>]
    let ``LoadH copies 16-bit register`` () =
        let regs = { initRegister() with BC = 0x1234us }
        let result = LoadH DE BC regs
        Assert.Equal(0x1234us, result.DE)

    [<Fact>]
    let ``LoadN16 loads 16-bit immediate`` () =
        let regs = initRegister()
        let result = LoadN16 SP 0xFFFEus regs
        Assert.Equal(0xFFFEus, result.SP)

    [<Fact>]
    let ``LoadN16 BC loads to BC register`` () =
        let regs = initRegister()
        let result = LoadN16 BC 0xABCDus regs
        Assert.Equal(0xABCDus, result.BC)

// ====================
// ハーフキャリー計算テスト
// ====================

module HalfCarryTests =
    [<Fact>]
    let ``hasHalfCarryAdd8 returns true when carry from bit 3`` () =
        Assert.True(hasHalfCarryAdd8 0x0Fus 0x01us)
        Assert.True(hasHalfCarryAdd8 0x08us 0x08us)

    [<Fact>]
    let ``hasHalfCarryAdd8 returns false when no carry from bit 3`` () =
        Assert.False(hasHalfCarryAdd8 0x01us 0x01us)
        Assert.False(hasHalfCarryAdd8 0x00us 0x0Fus)

    [<Fact>]
    let ``hasHalfCarrySub8 returns true when borrow from bit 4`` () =
        Assert.True(hasHalfCarrySub8 0x10us 0x01us)
        Assert.True(hasHalfCarrySub8 0x00us 0x01us)

    [<Fact>]
    let ``hasHalfCarrySub8 returns false when no borrow`` () =
        Assert.False(hasHalfCarrySub8 0x0Fus 0x01us)
        Assert.False(hasHalfCarrySub8 0x0Fus 0x0Fus)

    [<Fact>]
    let ``hasHalfCarryAdd16 returns true when carry from bit 11`` () =
        Assert.True(hasHalfCarryAdd16 0x0FFFus 0x0001us)

    [<Fact>]
    let ``hasHalfCarrySub16 returns true when borrow from bit 12`` () =
        Assert.True(hasHalfCarrySub16 0x1000us 0x0001us)

// ====================
// ADD命令テスト
// ====================

module AddTests =
    [<Fact>]
    let ``AddR adds register to A`` () =
        let regs = { initRegister() with AF = 0x1000us; BC = 0x0500us }
        let result = AddR B regs
        Assert.Equal(0x15us, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``AddR sets Z flag when result is zero`` () =
        let regs = { initRegister() with AF = 0x0000us; BC = 0x0000us }
        let result = AddR B regs
        Assert.True(isZ result)

    [<Fact>]
    let ``AddR clears N flag`` () =
        let regs = initRegister() |> setFlag false true false false
        let result = AddR B regs
        Assert.False(isN result)

    [<Fact>]
    let ``AddR sets H flag on half carry`` () =
        let regs = { initRegister() with AF = 0x0F00us; BC = 0x0100us }
        let result = AddR B regs
        Assert.True(isH result)

    [<Fact>]
    let ``AddR sets C flag on carry`` () =
        let regs = { initRegister() with AF = 0xFF00us; BC = 0x0100us }
        let result = AddR B regs
        Assert.True(isC result)

    [<Fact>]
    let ``AddR wraps on overflow`` () =
        let regs = { initRegister() with AF = 0xFF00us; BC = 0x0200us }
        let result = AddR B regs
        // 0xFF + 0x02 = 0x101 -> wraps to 0x01
        Assert.Equal(0x01us, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``AddN8 adds immediate to A`` () =
        let regs = { initRegister() with AF = 0x2000us }
        let result = AddN8 0x05us regs
        Assert.Equal(0x25us, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``AddH adds 16-bit registers`` () =
        let regs = { initRegister() with HL = 0x1000us; BC = 0x0234us }
        let result = AddH HL BC regs
        Assert.Equal(0x1234us, result.HL)

    [<Fact>]
    let ``AddH sets H flag on half carry from bit 11`` () =
        let regs = { initRegister() with HL = 0x0FFFus; BC = 0x0001us }
        let result = AddH HL BC regs
        Assert.True(isH result)

    [<Fact>]
    let ``AddH sets C flag on 16-bit overflow`` () =
        let regs = { initRegister() with HL = 0xFFFFus; BC = 0x0001us }
        let result = AddH HL BC regs
        Assert.True(isC result)

// ====================
// SUB命令テスト
// ====================

module SubTests =
    [<Fact>]
    let ``SubR subtracts register from A`` () =
        let regs = { initRegister() with AF = 0x1000us; BC = 0x0500us }
        let result = SubR B regs
        Assert.Equal(0x0Bus, getRegisterValue (R8 A) result)

    [<Fact>]
    let ``SubR sets Z flag when result is zero`` () =
        let regs = { initRegister() with AF = 0x0500us; BC = 0x0500us }
        let result = SubR B regs
        Assert.True(isZ result)

    [<Fact>]
    let ``SubR sets N flag`` () =
        let regs = { initRegister() with AF = 0x1000us; BC = 0x0500us }
        let result = SubR B regs
        Assert.True(isN result)

    [<Fact>]
    let ``SubR sets H flag on half borrow`` () =
        let regs = { initRegister() with AF = 0x1000us; BC = 0x0100us }
        let result = SubR B regs
        Assert.True(isH result)

    [<Fact>]
    let ``SubR sets C flag when subtracting larger value`` () =
        let regs = { initRegister() with AF = 0x0500us; BC = 0x0600us }
        let result = SubR B regs
        Assert.True(isC result)

    [<Fact>]
    let ``SubR wraps on underflow`` () =
        let regs = { initRegister() with AF = 0x0000us; BC = 0x0100us }
        let result = SubR B regs
        // 0x00 - 0x01 = -1 -> wraps to 0xFF
        Assert.Equal(0xFFus, getRegisterValue (R8 A) result)
