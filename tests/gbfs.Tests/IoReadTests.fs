/// 未使用ビットが 1 として読める IO レジスタ (SC / TAC) のテスト。
/// PyBoy の同じゲーム状態での読み値 (SC=0x7F, TAC=0xFE) に合わせた。
module IoReadTests

open Xunit
open gbfs.Lib

let private boot () =
    let rom = Array.zeroCreate (32 * 1024)
    rom.[0x0147] <- 0x00uy
    Emulator.loadRom rom (Emulator.create ())

[<Fact>]
let ``SC は bit2-6 が 1 として読める`` () =
    let s = Emulator.writeMemory 0xFF02us [| 0x03uy |] (boot ())
    Assert.Equal(0x7Fuy, (Emulator.readMemory 0xFF02us 1 s).[0])

[<Fact>]
let ``SC の機能ビット (bit7/bit0) は保たれる`` () =
    let s = Emulator.writeMemory 0xFF02us [| 0x81uy |] (boot ())
    Assert.Equal(0xFDuy, (Emulator.readMemory 0xFF02us 1 s).[0]) // 0x81 | 0x7C

[<Fact>]
let ``TAC は bit3-7 が 1 として読める`` () =
    let s = Emulator.writeMemory 0xFF07us [| 0x06uy |] (boot ())
    Assert.Equal(0xFEuy, (Emulator.readMemory 0xFF07us 1 s).[0])

[<Fact>]
let ``TAC の機能ビット (bit0-2) は保たれる`` () =
    let s = Emulator.writeMemory 0xFF07us [| 0x05uy |] (boot ())
    Assert.Equal(0xFDuy, (Emulator.readMemory 0xFF07us 1 s).[0])

// CGB モードが必要なテスト。ヘッダ 0x0143 bit7 = 1 のカートを CGB マシンで動かす。
let private cgbBoot () =
    let rom = Array.zeroCreate (32 * 1024)
    rom.[0x0143] <- 0x80uy // CGB 専用カート
    rom.[0x0147] <- 0x00uy
    Emulator.loadRom rom (Emulator.createWithMachine Memory.Cgb)

/// BCPS/OCPS の bit6 は未使用で、読み出すと常に 1。
/// PyBoy 実測: 0x80 を書いて読むと 0xC0 (bit6 が立って返る)。
[<Fact>]
let ``BCPS は bit6 が 1 として読める`` () =
    let s = Emulator.writeMemory 0xFF68us [| 0x80uy |] (cgbBoot ())
    Assert.Equal(0xC0uy, (Emulator.readMemory 0xFF68us 1 s).[0])

[<Fact>]
let ``BCPS の自動インクリメントビット (bit7) とインデックス (bit0-5) は保たれる`` () =
    let s = Emulator.writeMemory 0xFF68us [| 0x8Auy |] (cgbBoot ())
    Assert.Equal(0xCAuy, (Emulator.readMemory 0xFF68us 1 s).[0])

[<Fact>]
let ``OCPS も bit6 が 1 として読める`` () =
    let s = Emulator.writeMemory 0xFF6Aus [| 0xB2uy |] (cgbBoot ())
    Assert.Equal(0xF2uy, (Emulator.readMemory 0xFF6Aus 1 s).[0])

/// Pan Docs: 「中止時は HDMA1-4 が $FF にならない」= 完了時は $FF になる。
/// PyBoy 実測: GDMA 完了後は FF51-FF54 がすべて 0xFF。
[<Fact>]
let ``GDMA 完了後は HDMA1-4 が 0xFF を返す`` () =
    let mutable s = cgbBoot ()
    s <- Emulator.writeMemory 0xFF51us [| 0x01uy |] s
    s <- Emulator.writeMemory 0xFF52us [| 0x00uy |] s
    s <- Emulator.writeMemory 0xFF53us [| 0x80uy |] s
    s <- Emulator.writeMemory 0xFF54us [| 0x00uy |] s
    s <- Emulator.writeMemory 0xFF55us [| 0x00uy |] s // GDMA 1 ブロック = 即時完了
    Assert.Equal(0x7Fuy, (Emulator.readMemory 0xFF55us 1 s).[0] &&& 0x7Fuy) // 完了時は残り 0x7F
    Assert.Equal(0xFFuy, (Emulator.readMemory 0xFF51us 1 s).[0])
    Assert.Equal(0xFFuy, (Emulator.readMemory 0xFF52us 1 s).[0])
    Assert.Equal(0xFFuy, (Emulator.readMemory 0xFF53us 1 s).[0])
    Assert.Equal(0xFFuy, (Emulator.readMemory 0xFF54us 1 s).[0])

/// LCDC bit7 = 0 (LCD OFF) のとき PPU は停止する。実機 (Pan Docs) では LY=0 に固定され、
/// モードは進まず、VBlank 割り込みも発生しない。ここを見落とすと、ゲームが建物に入る等で
/// LCD を切った瞬間に VBlank 割り込みが無限再入し、割り込みベクタ ($0038) に張り付いて
/// SP が減り続ける (実機では起きないハング)。Crystal の研究所入店で実際に発生した。
[<Fact>]
let ``LCD OFF 中は PPU が停止し VBlank 割り込みを要求しない`` () =
    let mutable ppuState = Ppu.create ()
    let mutable mem = Memory.create ()
    mem <- Memory.write Ppu.LCDC 0x00uy mem
    mem <- Memory.write 0xFF0Fus 0x00uy mem
    // 10 フレーム以上に相当するサイクルを回す
    for _ = 1 to 200 do
        let ps, m = Ppu.step 456 ppuState mem
        ppuState <- ps
        mem <- m
    Assert.Equal(0uy, ppuState.LY)
    Assert.Equal(Ppu.HBlank, ppuState.Mode)
    let ifReg = Memory.read 0xFF0Fus mem
    Assert.Equal(0uy, ifReg &&& 0x01uy) // VBlank ビットは立たない

/// LCD を再度有効化すると通常のモード遷移が再開する
[<Fact>]
let ``LCD を有効化し直すとモード遷移が再開する`` () =
    let mutable ppuState = Ppu.create ()
    let mutable mem = Memory.create ()
    mem <- Memory.write Ppu.LCDC 0x00uy mem
    for _ = 1 to 50 do
        let ps, m = Ppu.step 456 ppuState mem
        ppuState <- ps
        mem <- m
    mem <- Memory.write Ppu.LCDC 0x91uy mem // LCD ON + BG ON
    let ps1, m1 = Ppu.step 204 ppuState mem // HBlank 完了 → 次ラインの OamScan
    ppuState <- ps1
    mem <- m1
    Assert.Equal(Ppu.OamScan, ppuState.Mode)
    Assert.Equal(1uy, ppuState.LY)
    let ps2, m2 = Ppu.step 80 ppuState mem
    ppuState <- ps2
    mem <- m2
    Assert.Equal(Ppu.Drawing, ppuState.Mode)
    let ps3, m3 = Ppu.step 172 ppuState mem
    ppuState <- ps3
    mem <- m3
    Assert.Equal(Ppu.HBlank, ppuState.Mode)
