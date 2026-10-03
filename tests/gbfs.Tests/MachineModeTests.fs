/// マシン種別 (DMG / CGB) ごとの電源投入直後の状態と、CGB の B レジスタ初期値のテスト。
/// 対応する公開テスト ROM: misc/boot_regs-cgb, misc/boot_hwio-C, misc/boot_div-cgbABCDE
/// (これらはヘッダ 0x0143 = 0x00 のため CGB の DMG 互換モードで走る)。
module MachineModeTests

open Xunit
open gbfs.Lib

/// ヘッダ付きの最小 ROM
let private makeRom (licensee: byte) (newLicensee: string) (title: string) =
    let rom = Array.zeroCreate 0x8000
    for i in 0 .. title.Length - 1 do rom.[0x0134 + i] <- byte title.[i]
    rom.[0x014B] <- licensee
    if newLicensee.Length = 2 then
        rom.[0x0144] <- byte newLicensee.[0]
        rom.[0x0145] <- byte newLicensee.[1]
    rom

/// nop / jp 0x0150 の後、nops 個の NOP を置き、その直後に ldh a,(DIV) を置いた ROM
let private makeDivProbeRom (nops: int) =
    let rom = Array.zeroCreate 0x8000
    rom.[0x0100] <- 0x00uy   // nop
    rom.[0x0101] <- 0xC3uy   // jp 0x0150
    rom.[0x0102] <- 0x50uy
    rom.[0x0103] <- 0x01uy
    for i in 0 .. nops - 1 do rom.[0x0150 + i] <- 0x00uy
    rom.[0x0150 + nops] <- 0xF0uy      // ldh a,(DIV)
    rom.[0x0150 + nops + 1] <- 0x04uy
    rom

/// nop + jp + nops 個の NOP を実行し終えた時点 (= ldh a,(DIV) の開始時点) の DIV を返す。
/// gbfs は命令実行後にタイマを進めるため、Io の DIV は「次の命令の開始時点」の値になる。
let private divBeforeLdh (machine: Memory.MachineMode) (nops: int) =
    let rom = makeDivProbeRom nops
    let mutable st = Decoder.loadRomToState rom (Decoder.createStateFor machine ())
    for _ in 1 .. nops + 2 do st <- Decoder.step st
    int (Memory.read 0xFF04us st.Mem)

// ============================================================
// 起動時のレジスタ
// ============================================================

[<Fact>]
let ``DMG の起動時レジスタは A=0x01, HL=0x014D`` () =
    let st = Decoder.createState ()
    Assert.Equal(0x01us, st.Regs.AF >>> 8)
    Assert.Equal(0x014Dus, st.Regs.HL)

[<Fact>]
let ``CGB の起動時レジスタは A=0x11, F=0x80, D=0x00, E=0x08, HL=0x007C`` () =
    let st = Decoder.createStateFor Memory.Cgb ()
    Assert.Equal(0x11us, st.Regs.AF >>> 8)
    Assert.Equal(0x80us, st.Regs.AF &&& 0xFFus)
    Assert.Equal(0x00us, st.Regs.BC >>> 8)
    Assert.Equal(0x0000us, st.Regs.BC &&& 0xFFus)
    Assert.Equal(0x00us, st.Regs.DE >>> 8)
    Assert.Equal(0x08us, st.Regs.DE &&& 0xFFus)
    Assert.Equal(0x007Cus, st.Regs.HL)

// ============================================================
// 起動時の I/O (mooneye misc/boot_hwio-C が根拠)
// ============================================================

[<Fact>]
let ``CGB と DMG で値が異なる I/O レジスタ`` () =
    let dmg = Memory.create ()
    let cgb = Memory.createWith Memory.Cgb ()
    Assert.Equal(0xCFuy, Memory.read 0xFF00us dmg)   // P1 (DMG)
    Assert.Equal(0xFFuy, Memory.read 0xFF00us cgb)   // P1 (CGB のブート ROM は 0x30 を書く)
    Assert.Equal(0xFFuy, Memory.read 0xFF48us dmg)   // OBP0 (DMG は未初期化)
    Assert.Equal(0x00uy, Memory.read 0xFF48us cgb)   // OBP0 (CGB)
    Assert.Equal(0xFFuy, Memory.read 0xFF49us dmg)
    Assert.Equal(0x00uy, Memory.read 0xFF49us cgb)
    Assert.Equal(0xFFuy, Memory.read 0xFF68us dmg)   // BCPS (非 CGB モードは 0xFF)
    Assert.Equal(0xC8uy, Memory.read 0xFF68us cgb)
    Assert.Equal(0xFFuy, Memory.read 0xFF6Aus dmg)   // OCPS
    Assert.Equal(0xD0uy, Memory.read 0xFF6Aus cgb)

[<Fact>]
let ``CGB の未定義レジスタ初期値`` () =
    let cgb = Memory.createWith Memory.Cgb ()
    Assert.Equal(0x00uy, Memory.read 0xFF72us cgb)
    Assert.Equal(0x00uy, Memory.read 0xFF73us cgb)
    Assert.Equal(0xFFuy, Memory.read 0xFF74us cgb)
    Assert.Equal(0x8Fuy, Memory.read 0xFF75us cgb)
    Assert.Equal(0x00uy, Memory.read 0xFF76us cgb)
    Assert.Equal(0x00uy, Memory.read 0xFF77us cgb)

[<Fact>]
let ``CGB の CGB 専用レジスタは非 CGB モードなので 0xFF`` () =
    let cgb = Memory.createWith Memory.Cgb ()
    // ヘッダ 0x0143 = 0x00 のカートでは DMG 互換モードになり、CGB 専用レジスタは 0xFF を読む
    Assert.Equal(0xFFuy, Memory.read 0xFF4Cus cgb)   // KEY0
    Assert.Equal(0xFFuy, Memory.read 0xFF4Dus cgb)   // KEY1
    Assert.Equal(0xFFuy, Memory.read 0xFF4Fus cgb)   // VBK
    Assert.Equal(0xFFuy, Memory.read 0xFF70us cgb)   // SVBK

// ============================================================
// DIV (mooneye boot_div 系が根拠)
// ============================================================

[<Fact>]
let ``DIV の初期値は DMG 0xAB / CGB 0x26`` () =
    Assert.Equal(0xAB, int (Memory.read 0xFF04us (Memory.create ())))
    Assert.Equal(0x26, int (Memory.read 0xFF04us (Memory.createWith Memory.Cgb ())))

[<Fact>]
let ``内部カウンタの初期値は DMG 0xABD5 / CGB 0x2681`` () =
    Assert.Equal(0xABD5us, (Timer.create ()).InternalCounter)
    Assert.Equal(0x2681us, (Timer.createWith Memory.Cgb ()).InternalCounter)

[<Fact>]
let ``DMG の DIV 位相は boot_div-dmgABCmgb の期待値と一致する`` () =
    // 6 nops 後の読み出しが 0xAC (期待値の先頭)
    Assert.Equal(0xAC, divBeforeLdh Memory.Dmg 6)

[<Fact>]
let ``CGB の DIV 位相は boot_div-cgbABCDE の期待値と一致する`` () =
    // 27 nops 後の読み出しが 0x27 (期待値の先頭)
    Assert.Equal(0x27, divBeforeLdh Memory.Cgb 27)

// ============================================================
// CGB の B レジスタ (ライセンスコード依存)
// ============================================================

[<Fact>]
let ``CGB の B は旧ライセンス 0x01 (任天堂) ならタイトル総和`` () =
    let rom = makeRom 0x01uy "" "TITLE"
    let expected = byte (Array.sumBy int rom.[0x0134 .. 0x0143] &&& 0xFF)
    Assert.Equal(expected, Memory.cgbBootB rom)

[<Fact>]
let ``CGB の B は旧ライセンス 0x33 + 新ライセンス "01" ならタイトル総和`` () =
    let rom = makeRom 0x33uy "01" "TITLE"
    let expected = byte (Array.sumBy int rom.[0x0134 .. 0x0143] &&& 0xFF)
    Assert.Equal(expected, Memory.cgbBootB rom)

[<Fact>]
let ``CGB の B は任天堂以外のライセンスなら 0x00`` () =
    let rom = makeRom 0x21uy "ZZ" "TITLE"
    Assert.Equal(0x00uy, Memory.cgbBootB rom)

[<Fact>]
let ``DMG マシンでは ROM をロードしても B は 0x00 のまま`` () =
    let rom = makeRom 0x01uy "" "TITLE"
    let st = Decoder.loadRomToState rom (Decoder.createState ())
    Assert.Equal(0x00us, st.Regs.BC >>> 8)

[<Fact>]
let ``CGB マシンでは ROM をロードすると B がタイトル総和になる`` () =
    let rom = makeRom 0x01uy "" "TITLE"
    let st = Decoder.loadRomToState rom (Decoder.createStateFor Memory.Cgb ())
    Assert.Equal(uint16 (Memory.cgbBootB rom), st.Regs.BC >>> 8)

[<Fact>]
let ``CGB マシンでも C レジスタはロードで変化しない`` () =
    let rom = makeRom 0x01uy "" "TITLE"
    let st = Decoder.loadRomToState rom (Decoder.createStateFor Memory.Cgb ())
    Assert.Equal(0x00us, st.Regs.BC &&& 0xFFus)
