/// CGB の HDMA/GDMA (0xFF51-0xFF55) のテスト。
/// Pan Docs: CGB Registers — HDMA1-4 (転送元/転送先)、HDMA5 (bit7: 0 = GDMA 即時 / 1 = HDMA)。
module HdmaTests

open Xunit
open gbfs.Lib

/// ヘッダ 0x0143 bit7 = 1 (CGB 対応) の最小 ROM
let private cgbRom () =
    let rom = Array.zeroCreate 0x8000
    rom.[0x0143] <- 0x80uy // CGB フラグ
    rom.[0x0147] <- 0x00uy // ROM only
    rom

/// CGB モードで ROM をロードした状態 (CgbMode = true になる)
let private cgbState () =
    Decoder.loadRomToState (cgbRom ()) (Decoder.createStateFor Memory.Cgb ())

/// 転送元アドレスと転送先アドレスをレジスタに設定する (16 バイト境界)
let private setAddresses (src: uint16) (dst: uint16) (mem: Memory.MemoryBus) =
    let mutable m = mem
    m <- Memory.write 0xFF51us (byte (src >>> 8)) m
    m <- Memory.write 0xFF52us (byte (src &&& 0xFFus)) m
    m <- Memory.write 0xFF53us (byte ((dst >>> 8) &&& 0x1Fus)) m
    m <- Memory.write 0xFF54us (byte (dst &&& 0xFFus)) m
    m

/// WRAM の addr から n バイトを 0,1,2,... で埋める
let private fillWram (addr: uint16) (n: int) (mem: Memory.MemoryBus) =
    let mutable m = mem
    for i in 0 .. n - 1 do
        m <- Memory.write (addr + uint16 i) (byte i) m
    m

[<Fact>]
let ``転送中に bit7 = 0 を書くと中断され、残りブロック数が読める`` () =
    // Pan Docs: bit7 を 0 にした書き込みは HBlank 転送を停止する (手動終了)。
    // 転送は行わず、bit7 = 1 (非アクティブ) + 残りのブロック数が読める。
    let st = cgbState ()
    let mem = fillWram 0xC000us 96 st.Mem
    let mem = setAddresses 0xC000us 0x9000us mem
    let mem = Memory.write 0xFF55us 0x85uy mem // 6 ブロックの HDMA を開始

    Assert.Equal(0x05uy, Memory.read 0xFF55us mem) // 進行中 (bit7 = 0)

    let mem = Memory.write 0xFF55us 0x00uy mem // 中断 (bit7 = 0 の書き込み)

    Assert.Equal(0x85uy, Memory.read 0xFF55us mem) // bit7 = 1 + 残り 5 (あと 6 ブロック)

    // 中断時に余分なブロックを転送してはいけない
    for i in 0 .. 15 do
        Assert.Equal(0uy, Memory.readVram 0 (0x9000 - 0x8000 + i) mem)

    // 中断後は HBlank でも転送されない
    let mem2 = Memory.hdmaStep mem
    for i in 0 .. 15 do
        Assert.Equal(0uy, Memory.readVram 0 (0x9000 - 0x8000 + i) mem2)

[<Fact>]
let ``GDMA は bit7 = 0 の書き込みで即座に全ブロックを転送する`` () =
    let st = cgbState ()
    let mem = fillWram 0xC000us 32 st.Mem
    let mem = setAddresses 0xC000us 0x9000us mem
    let mem = Memory.write 0xFF55us 0x01uy mem // 2 ブロック (32 バイト) を即時転送

    for i in 0 .. 31 do
        Assert.Equal(byte i, Memory.readVram 0 (0x9000 - 0x8000 + i) mem)

    // 完了すると HDMA5 は 0xFF (bit7 = 1 = 非アクティブ, 残り = 0x7F)
    Assert.Equal(0xFFuy, Memory.read 0xFF55us mem)

[<Fact>]
let ``GDMA 完了後は HDMA1-4 が $FF になる`` () =
    let st = cgbState ()
    let mem = fillWram 0xC000us 32 st.Mem
    let mem = setAddresses 0xC000us 0x9000us mem
    let mem = Memory.write 0xFF55us 0x01uy mem

    // 転送は完了しており (VRAM に書かれている)、完了後はアドレスレジスタが $FF になる。
    // Pan Docs: 「中止時は HDMA1-4 が $FF にならない」= 完了時は $FF。PyBoy 実測も同じ。
    Assert.Equal(0xFFuy, Memory.read 0xFF51us mem)
    Assert.Equal(0xFFuy, Memory.read 0xFF52us mem)
    Assert.Equal(0xFFuy, Memory.read 0xFF53us mem)
    Assert.Equal(0xFFuy, Memory.read 0xFF54us mem)

[<Fact>]
let ``HDMA は HBlank ごとに 16 バイト転送し、残りを HDMA5 に反映する`` () =
    let st = cgbState ()
    let mem = fillWram 0xC000us 32 st.Mem
    let mem = setAddresses 0xC000us 0x9000us mem
    let mem = Memory.write 0xFF55us 0x81uy mem // 2 ブロックを HDMA で開始

    // 開始直後は bit7 = 0 (転送中)、残り 1 ブロック (bits 0-6 = 1)
    Assert.Equal(0x01uy, Memory.read 0xFF55us mem)
    Assert.True(Memory.hdmaActive mem)

    // 1 ライン目: OamScan(80) → Drawing(172) で HBlank に入る (LY = 0)
    let (ppu1, m1) = Ppu.step 80 st.Ppu mem
    let (ppu2, m2) = Ppu.step 172 ppu1 m1

    for i in 0 .. 15 do
        Assert.Equal(byte i, Memory.readVram 0 (0x9000 - 0x8000 + i) m2)
    Assert.Equal(0x00uy, Memory.read 0xFF55us m2) // 残り 1 ブロック → 0
    Assert.True(Memory.hdmaActive m2)

    // 2 ライン目: HBlank → 次のラインの OamScan → Drawing → HBlank
    let (ppu3, m3) = Ppu.step 204 ppu2 m2
    let (ppu4, m4) = Ppu.step 80 ppu3 m3
    let (_, m5) = Ppu.step 172 ppu4 m4

    for i in 0 .. 31 do
        Assert.Equal(byte i, Memory.readVram 0 (0x9000 - 0x8000 + i) m5)
    Assert.Equal(0xFFuy, Memory.read 0xFF55us m5) // 完了 (bit7 = 1)
    Assert.False(Memory.hdmaActive m5)

[<Fact>]
let ``HDMA は VBlank 中 (mode 1) には転送しない`` () =
    let st = cgbState ()
    let mem = fillWram 0xC000us 16 st.Mem
    let mem = setAddresses 0xC000us 0x9000us mem
    let mem = Memory.write 0xFF55us 0x80uy mem // 1 ブロックを HDMA で開始

    // VBlank (mode 1) では Drawing → HBlank の遷移が起きないので転送されない
    let vblankPpu = { st.Ppu with Mode = Ppu.VBlank; Cycles = 0; LY = 144uy }
    let (ppu, m) = Ppu.step 456 vblankPpu mem

    Assert.Equal(0x00uy, Memory.readVram 0 (0x9000 - 0x8000) m) // 未転送
    Assert.True(Memory.hdmaActive m)                             // 進行中のまま
    Assert.Equal(145uy, ppu.LY)

[<Fact>]
let ``HDMA5 への書き込みは 16 バイト単位でない長さもブロック数として扱う`` () =
    let st = cgbState ()
    let mem = fillWram 0xC000us 48 st.Mem
    let mem = setAddresses 0xC000us 0x9000us mem
    let mem = Memory.write 0xFF55us 0x02uy mem // 3 ブロック (48 バイト)

    for i in 0 .. 47 do
        Assert.Equal(byte i, Memory.readVram 0 (0x9000 - 0x8000 + i) mem)
    Assert.Equal(0xFFuy, Memory.read 0xFF55us mem)

[<Fact>]
let ``DMG モードでは HDMA/GDMA レジスタへの書き込みを無視する`` () =
    let rom = cgbRom ()
    let st = Decoder.loadRomToState rom (Decoder.createStateFor Memory.Dmg ())
    let mem = fillWram 0xC000us 16 st.Mem
    let mem = setAddresses 0xC000us 0x9000us mem
    let mem = Memory.write 0xFF55us 0x00uy mem // GDMA を要求しても DMG では無視

    Assert.Equal(0x00uy, Memory.readVram 0 (0x9000 - 0x8000) mem)
    Assert.False(Memory.hdmaActive mem)

[<Fact>]
let ``転送先アドレスは VRAM 内で折り返す`` () =
    let st = cgbState ()
    let mem = fillWram 0xC000us 32 st.Mem
    let mem = setAddresses 0xC000us 0x9FF0us mem // 末尾から 2 ブロック
    let mem = Memory.write 0xFF55us 0x01uy mem

    for i in 0 .. 15 do
        Assert.Equal(byte i, Memory.readVram 0 (0x9FF0 - 0x8000 + i) mem)
    for i in 0 .. 15 do
        Assert.Equal(byte (16 + i), Memory.readVram 0 i mem) // 0x8000 に折り返す
