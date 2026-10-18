/// runFrame のフレーム境界の回帰テスト。
///
/// 1 フレーム = 70,224 PPU ドット (456 x 154)。PPU が動いていない (LCD off) 間は
/// LY wrap が起きないので、ステップ数で数えると 1 フレームが実時間の 4〜5 倍になり、
/// フレーム番号が実時間からずれる (Crystal JP の起動で 21 フレームずれていた)。
/// 倍速時は CPU が 2 倍で走るので 1 フレーム = 140,448 CPU サイクル。
///
/// なお CGB のブート状態は VBlank (LY=0x91) なので、最初の runFrame は端数のフレームになる。
/// テストは 1 回空回ししてから測る。
module FrameBoundaryTests

open Xunit
open gbfs.Lib

/// ヘッダ 0x0143 = 0xC0 (CGB 専用) の最小 ROM
let private cgbRom () =
    let rom = Array.zeroCreate 0x8000
    rom.[0x0143] <- 0xC0uy
    rom

let private boot () = Emulator.loadRom (cgbRom ()) (Emulator.createWithMachine Memory.Cgb)

/// 1 フレーム分の消費サイクルを測る (ブート直後の端数フレームを捨ててから)
let private frameCycles (st: Emulator.EmulatorState) =
    let a = Emulator.runFrame st
    let b = Emulator.runFrame a
    b.TotalCycles - a.TotalCycles

[<Fact>]
let ``LCD off 中でも 1 フレームは 70,224 ドット`` () =
    let st = Emulator.writeMemory 0xFF40us [| 0x00uy |] (boot ())
    // 命令単位の端数が出るので 1 命令 (最大 24 サイクル) 分の余裕を見る
    Assert.InRange(frameCycles st, 70224L, 70224L + 23L)

[<Fact>]
let ``LCD on (通常速度) の 1 フレームも 70,224 サイクル`` () =
    let st = Emulator.writeMemory 0xFF40us [| 0x91uy |] (boot ())
    Assert.InRange(frameCycles st, 70224L, 70224L + 23L)

[<Fact>]
let ``倍速時の 1 フレームは 140,448 サイクル`` () =
    let st = Emulator.writeMemory 0xFF40us [| 0x91uy |] (boot ())
    // KEY1 (0xFF4D) の bit7 は STOP の速度切替でしか立たないので、テストでは直接立てる
    st.Cpu.Mem.Io.[0x4D] <- 0x80uy
    Assert.InRange(frameCycles st, 140448L, 140448L + 23L)
