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
