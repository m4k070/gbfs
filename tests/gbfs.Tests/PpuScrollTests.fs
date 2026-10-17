/// PPU の BG 横スクロールの折り返しの回帰テスト。
///
/// BG マップは 32x32 タイル = 256x256 px。SCX + x が 256 を超えたら
/// 同じ行の先頭 (列 0) に戻る。512 でマスクすると列 32..63 を読み、
/// タイルマップの次の行のデータが混ざる (Crystal JP の intro で
/// SCX=0xAE のとき右下に白い帯が出た)。
module PpuScrollTests

open Xunit
open gbfs.Lib

[<Fact>]
let ``BG の横スクロールは 256px で折り返す`` () =
    let mutable st = Emulator.loadRom (Array.zeroCreate 0x8000) (Emulator.create ())

    // tile 1 = 色 1 (byte1=FF, byte2=00)、tile 2 = 色 2 (byte1=00, byte2=FF)
    let setTile (idx: int) (b1: byte) (b2: byte) =
        for row in 0 .. 7 do
            st <- Emulator.writeMemory (uint16 (0x8000 + idx * 16 + row * 2)) [| b1; b2 |] st
    setTile 1 0xFFuy 0x00uy
    setTile 2 0x00uy 0xFFuy

    // タイルマップ row0 = tile 0、列 0 だけ tile 1。row1 = 全部 tile 2
    st <- Emulator.writeMemory 0x9800us (Array.create 32 0uy) st
    st <- Emulator.writeMemory 0x9800us [| 1uy |] st
    st <- Emulator.writeMemory 0x9820us (Array.create 32 2uy) st

    // LCDC: LCD on / BG on / tile data 0x8000 / tile map 0x9800 / OBJ off
    st <- Emulator.writeMemory 0xFF40us [| 0x91uy |] st
    st <- Emulator.writeMemory 0xFF47us [| 0xE4uy |] st
    st <- Emulator.writeMemory 0xFF42us [| 0uy |] st   // SCY
    st <- Emulator.writeMemory 0xFF43us [| 0xF8uy |] st // SCX = 248

    st <- Emulator.runFrame st
    let fb = Emulator.getFrameBuffer st

    // x=0: mapX=248 -> 列 31 (tile 0) = 色 0 = shade 0
    Assert.Equal(0uy, fb.[0 * 160 + 0])
    // x=10: mapX=258 -> 折り返して 2 -> 列 0 (tile 1) = 色 1 = shade 1
    Assert.Equal(1uy, fb.[0 * 160 + 10])
    // x=20: mapX=268 -> 12 -> 列 1 (tile 0) = 色 0 = shade 0
    Assert.Equal(0uy, fb.[0 * 160 + 20])
    // 折り返しは 8px だけ: x=17 は mapX=265 -> 9 -> 列 1 (tile 0)
    Assert.Equal(0uy, fb.[0 * 160 + 17])
