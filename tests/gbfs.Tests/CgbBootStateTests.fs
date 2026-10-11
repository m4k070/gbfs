/// CGB カートをロードした直後 (= ブート ROM 完了時点) の状態のテスト。
/// gbfs はブート ROM を実行せずその完了時点の値を直接作るので、期待値は
/// PyBoy (ブート ROM を実行する参照実装) でクリスタル JP を起動し、PC が 0x0100 に
/// 達した瞬間に実測した値。
/// なおヘッダ 0x0143 bit7 = 0 のカート (CGB の DMG 互換モード) の値は
/// mooneye misc/boot_hwio-C で検証済みで、MachineModeTests が担当する。
module CgbBootStateTests

open Xunit
open gbfs.Lib

/// ヘッダ 0x0143 = 0xC0 (CGB 専用) の最小 ROM
let private cgbRom () =
    let rom = Array.zeroCreate 0x8000
    rom.[0x0143] <- 0xC0uy
    rom

/// ヘッダ 0x0143 = 0x00 (DMG 専用) の最小 ROM
let private dmgRom () = Array.zeroCreate 0x8000

let private loadCgb () =
    Emulator.loadRom (cgbRom ()) (Emulator.createWithMachine Memory.Cgb)

// ============================================================
// I/O レジスタ (CGB モード)
// ============================================================

[<Fact>]
let ``CGB カートのロード直後の I/O はブート ROM 完了時点の値`` () =
    let m = (loadCgb ()).Cpu.Mem
    Assert.True(m.CgbMode)
    Assert.Equal(0x00uy, m.Io.[0x00])   // P1
    Assert.Equal(0xFFuy, m.Io.[0x01])   // SB
    Assert.Equal(0x7Cuy, m.Io.[0x02])   // SC
    Assert.Equal(0xC7uy, m.Io.[0x04])   // DIV
    Assert.Equal(0x93uy, m.Io.[0x10])   // NR10
    Assert.Equal(0xF0uy, m.Io.[0x26])   // NR52
    Assert.Equal(0xFFuy, m.Io.[0x30])   // 波形 RAM
    Assert.Equal(0xFFuy, m.Io.[0x48])   // OBP0
    Assert.Equal(0xFFuy, m.Io.[0x49])   // OBP1
    Assert.Equal(0xFBuy, m.Io.[0x4C])   // KEY0
    Assert.Equal(0x40uy, m.Io.[0x68])   // BCPS
    Assert.Equal(0x40uy, m.Io.[0x6A])   // OCPS
    Assert.Equal(0xFFuy, m.Io.[0x6C])   // OPRI
    Assert.Equal(0x00uy, m.Io.[0x75])

// ============================================================
// CGB パレット (ブート ROM は白で埋めて終わる)
// ============================================================

[<Fact>]
let ``CGB カートのロード直後のパレットは白`` () =
    let m = (loadCgb ()).Cpu.Mem
    Assert.Equal(64, m.BgPalette.Length)
    Assert.All(m.BgPalette, fun b -> Assert.Equal(0xE7uy, b))
    Assert.All(m.ObjPalette, fun b -> Assert.Equal(0xE7uy, b))

// ============================================================
// ブート ROM が使った時間 (DIV と PPU の位置)
// ============================================================

[<Fact>]
let ``CGB カートのロード直後の DIV は 0xC7`` () =
    let st = loadCgb ()
    Assert.Equal(0xC700us, st.Cpu.Timer.InternalCounter)
    Assert.Equal(0xC7uy, Memory.read 0xFF04us st.Cpu.Mem)

[<Fact>]
let ``CGB カートのロード直後は VBlank 中 (LY=0x91)`` () =
    let st = loadCgb ()
    Assert.Equal(0x91uy, st.Cpu.Ppu.LY)
    Assert.Equal(Ppu.VBlank, st.Cpu.Ppu.Mode)
    Assert.Equal(0x91uy, Memory.read 0xFF44us st.Cpu.Mem)

[<Fact>]
let ``CGB カートでも PC は 0x0100 から始まる`` () =
    Assert.Equal(0x0100us, (loadCgb ()).Cpu.Regs.PC)

// ============================================================
// DMG 側は変更しない (ブート ROM 無しの DMG は mooneye 検証済みの値のまま)
// ============================================================

[<Fact>]
let ``DMG カートのロード直後の状態は DMG の値のまま`` () =
    let st = Emulator.loadRom (dmgRom ()) (Emulator.createWithMachine Memory.Dmg)
    Assert.False(st.Cpu.Mem.CgbMode)
    Assert.Equal(0xABuy, Memory.read 0xFF04us st.Cpu.Mem)   // DIV (mooneye boot_div-dmg)
    Assert.Equal(0x00uy, st.Cpu.Ppu.LY)                     // LY
    Assert.Equal(0xABD5us, st.Cpu.Timer.InternalCounter)
