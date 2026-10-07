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

// ============================================================
// CGB モードの解禁 (ヘッダ 0x0143 bit7) とメモリバンク
// ============================================================

/// CGB 対応 (0x0143 = 0x80) の最小 ROM
let private makeCgbRom () =
    let rom = makeRom 0x01uy "" "TITLE"
    rom.[0x0143] <- 0x80uy
    rom

/// CGB モードで ROM をロードした状態
let private cgbState (rom: byte array) =
    Decoder.loadRomToState rom (Decoder.createStateFor Memory.Cgb ())

[<Fact>]
let ``CGB モードはマシンが CGB かつ ヘッダ 0x0143 bit7 = 1 のとき解禁される`` () =
    let cgbRom = makeCgbRom ()
    let dmgRom = makeRom 0x01uy "" "TITLE"
    Assert.True((cgbState cgbRom).Mem.CgbMode)
    Assert.False((cgbState dmgRom).Mem.CgbMode)
    Assert.False((Decoder.loadRomToState cgbRom (Decoder.createState ())).Mem.CgbMode)

[<Fact>]
let ``非 CGB モードでは CGB 専用レジスタの書き込みを無視し 0xFF を読む`` () =
    let st = cgbState (makeRom 0x01uy "" "TITLE")   // ヘッダ 0x0143 = 0x00 → DMG 互換モード
    let mem = Memory.write 0xFF4Dus 0x01uy st.Mem
    Assert.Equal(0xFFuy, Memory.read 0xFF4Dus mem)
    let mem = Memory.write 0xFF4Fus 0x01uy mem
    Assert.Equal(0xFFuy, Memory.read 0xFF4Fus mem)
    let mem = Memory.write 0xFF70us 0x03uy mem
    Assert.Equal(0xFFuy, Memory.read 0xFF70us mem)
    // 非 CGB モードでは SVBK の書き込みが無視され、D000-DFFF はバンク 1 のまま
    let mem = Memory.write 0xD000us 0xAAuy mem
    let mem = Memory.write 0xFF70us 0x03uy mem
    Assert.Equal(0xAAuy, Memory.read 0xD000us mem)

[<Fact>]
let ``KEY1 の初期値は CGB モードで 0x7E`` () =
    Assert.Equal(0x7Euy, Memory.read 0xFF4Dus (cgbState (makeCgbRom ())).Mem)

[<Fact>]
let ``KEY1 は bit0 のみ書き込みでき bit7 は読み出し専用`` () =
    let st = cgbState (makeCgbRom ())
    let mem = Memory.write 0xFF4Dus 0x81uy st.Mem   // bit7 は無視される
    Assert.Equal(0x7Fuy, Memory.read 0xFF4Dus mem)

[<Fact>]
let ``VBK と SVBK の読み出しは上位ビットが 1`` () =
    let st = cgbState (makeCgbRom ())
    Assert.Equal(0xFEuy, Memory.read 0xFF4Fus st.Mem)   // VRAM バンク 0
    Assert.Equal(0xF8uy, Memory.read 0xFF70us st.Mem)   // WRAM バンク 1
    let mem = st.Mem |> Memory.write 0xFF4Fus 0x01uy |> Memory.write 0xFF70us 0x03uy
    Assert.Equal(0xFFuy, Memory.read 0xFF4Fus mem)
    Assert.Equal(0xFBuy, Memory.read 0xFF70us mem)

[<Fact>]
let ``VBK で VRAM バンクを切り替えられ PPU は常にバンク 0 を読む`` () =
    let st = cgbState (makeCgbRom ())
    let mem = Memory.write 0x8000us 0x11uy st.Mem
    let mem = Memory.write 0xFF4Fus 0x01uy mem
    let mem = Memory.write 0x8000us 0x22uy mem
    Assert.Equal(0x22uy, Memory.read 0x8000us mem)            // バンク 1
    Assert.Equal(0x11uy, Memory.readVramBank0 0x8000us mem)   // PPU は常にバンク 0
    let mem = Memory.write 0xFF4Fus 0x00uy mem
    Assert.Equal(0x11uy, Memory.read 0x8000us mem)

[<Fact>]
let ``SVBK で D000-DFFF のバンクを切り替えられる`` () =
    let st = cgbState (makeCgbRom ())
    let mem = Memory.write 0xD000us 0xAAuy st.Mem            // バンク 1
    let mem = Memory.write 0xFF70us 0x03uy mem
    Assert.Equal(0xFBuy, Memory.read 0xFF70us mem)
    Assert.NotEqual(0xAAuy, Memory.read 0xD000us mem)        // バンク 3 は別の領域
    let mem = Memory.write 0xD000us 0xBBuy mem
    let mem = Memory.write 0xFF70us 0x01uy mem
    Assert.Equal(0xAAuy, Memory.read 0xD000us mem)           // バンク 1 に戻ると元の値

[<Fact>]
let ``C000-CFFF はバンク 0 固定で Echo RAM も同じバンクを見る`` () =
    let st = cgbState (makeCgbRom ())
    let mem = Memory.write 0xC000us 0xCCuy st.Mem
    let mem = Memory.write 0xFF70us 0x05uy mem
    Assert.Equal(0xCCuy, Memory.read 0xC000us mem)
    Assert.Equal(0xCCuy, Memory.read 0xE000us mem)           // Echo RAM
    let mem = Memory.write 0xE100us 0xDDuy mem                // Echo RAM への書き込み
    Assert.Equal(0xDDuy, Memory.read 0xC100us mem)

// ============================================================
// STOP と倍速 (Pan Docs: CGB Registers / KEY1)
// ============================================================

/// PC = 0x0150 に STOP (0x10 0x00) を置いた ROM
let private makeStopRom (cgb: bool) =
    let rom = if cgb then makeCgbRom () else makeRom 0x01uy "" "TITLE"
    rom.[0x0150] <- 0x10uy   // STOP
    rom.[0x0151] <- 0x00uy
    rom.[0x0152] <- 0x00uy   // nop
    rom

[<Fact>]
let ``STOP はアーム時に倍速へ切り替えて 8200 サイクル停止する`` () =
    let st = cgbState (makeStopRom true)
    let st = { st with Regs = { st.Regs with PC = 0x0150us } }
    let st = { st with Mem = Memory.write 0xFF4Dus 0x01uy st.Mem }
    let (st, cycles) = Decoder.stepWithCycles st
    Assert.Equal(4, cycles)
    Assert.Equal(0x0152us, st.Regs.PC)                  // STOP は 2 バイト命令
    Assert.Equal(0xFEuy, Memory.read 0xFF4Dus st.Mem)   // 倍速 + アーム解除
    Assert.Equal(8200, st.Stopped)
    // 停止中は命令を実行せず、タイマ (DIV) も進まない
    let timerBefore = st.Timer.InternalCounter
    let (st, cycles) = Decoder.stepWithCycles st
    Assert.Equal(4, cycles)
    Assert.Equal(8196, st.Stopped)
    Assert.Equal(0x0152us, st.Regs.PC)
    Assert.Equal(timerBefore, st.Timer.InternalCounter)

[<Fact>]
let ``STOP は非アーム時 / 非 CGB モードでは速度を変えない`` () =
    // CGB モードだがアームしていない
    let st = cgbState (makeStopRom true)
    let st = { st with Regs = { st.Regs with PC = 0x0150us } }
    let st = fst (Decoder.stepWithCycles st)
    Assert.Equal(0x7Euy, Memory.read 0xFF4Dus st.Mem)
    Assert.Equal(0, st.Stopped)
    // 非 CGB モードでは KEY1 への書き込み自体が無視される
    let st2 = cgbState (makeStopRom false)
    let st2 = { st2 with Regs = { st2.Regs with PC = 0x0150us } }
    let st2 = { st2 with Mem = Memory.write 0xFF4Dus 0x01uy st2.Mem }
    let st2 = fst (Decoder.stepWithCycles st2)
    Assert.Equal(0xFFuy, Memory.read 0xFF4Dus st2.Mem)
    Assert.Equal(0, st2.Stopped)

[<Fact>]
let ``通常速度では PPU がそのままのサイクルだけ進む`` () =
    let st = cgbState (makeStopRom true)
    let ppuBefore = st.Ppu.Cycles
    let (st, cycles) = Decoder.stepWithCycles st   // 0x0100 は nop
    Assert.Equal(4, cycles)
    Assert.Equal(ppuBefore + 4, st.Ppu.Cycles)

[<Fact>]
let ``DMG では 0xFF4D が 0xFF でも倍速と誤判定しない`` () =
    // 非 CGB モードでは 0xFF4D は 0xFF を読む。この bit7 を倍速フラグと誤解すると
    // PPU/APU が半分の速度で動いてしまう (実際に起きた回帰)
    let rom = makeRom 0x01uy "" "TITLE"
    rom.[0x0150] <- 0x00uy   // nop
    let st = Decoder.loadRomToState rom (Decoder.createState ())
    Assert.Equal(0xFFuy, Memory.read 0xFF4Dus st.Mem)
    let st = { st with Regs = { st.Regs with PC = 0x0150us } }
    let ppuBefore = st.Ppu.Cycles
    let (st, _) = Decoder.stepWithCycles st
    Assert.Equal(ppuBefore + 4, st.Ppu.Cycles)

[<Fact>]
let ``倍速時は PPU が半分だけ進みタイマはそのまま進む`` () =
    let st = cgbState (makeStopRom true)
    let st = { st with Regs = { st.Regs with PC = 0x0150us } }
    let st = { st with Mem = Memory.write 0xFF4Dus 0x01uy st.Mem }
    let (st, _) = Decoder.stepWithCycles st        // STOP → 倍速
    Assert.Equal(0xFEuy, Memory.read 0xFF4Dus st.Mem)
    let st = { st with Stopped = 0 }               // 停止を消費したことにする
    let ppuBefore = st.Ppu.Cycles
    let timerBefore = st.Timer.InternalCounter
    let (st, cycles) = Decoder.stepWithCycles st   // 0x0152 の nop
    Assert.Equal(4, cycles)
    Assert.Equal(ppuBefore + 2, st.Ppu.Cycles)                  // LCD は等速 → 半分
    Assert.Equal(timerBefore + 4us, st.Timer.InternalCounter)   // タイマは 2 倍 → そのまま
