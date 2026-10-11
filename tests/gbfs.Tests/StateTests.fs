/// エミュレータ状態の保存/復元 (gbfs.Lib.State) のテスト。
module StateTests

open Xunit
open gbfs.Lib

/// 32KB の最小 ROM (中身は NOP)。ヘッダ 0x0147 = ROM ONLY。
let private tinyRom =
    let rom = Array.zeroCreate (32 * 1024)
    rom.[0x0147] <- 0x00uy
    rom

let private boot () = Emulator.loadRom tinyRom (Emulator.create ())

[<Fact>]
let ``save/load で状態が完全に復元され、続きの実行も一致する`` () =
    let s1 = Emulator.runFrames 600 (boot ())
    let s2 = State.deserialize (State.serialize s1) s1
    Assert.Equal(s1.FrameCount, s2.FrameCount)
    Assert.Equal(s1.TotalCycles, s2.TotalCycles)
    Assert.Equal<byte array>(Emulator.getFrameBuffer s1, Emulator.getFrameBuffer s2)
    Assert.Equal<byte array>(Emulator.getRgbFrameBuffer s1, Emulator.getRgbFrameBuffer s2)
    // 再シリアライズした JSON が一致する = 状態が丸ごと往復している
    Assert.Equal(State.serialize s1, State.serialize s2)
    // 復元後の続きも決定論的に一致する
    let a1 = Emulator.runFrames 300 (Emulator.pressButton "a" s1)
    let a2 = Emulator.runFrames 300 (Emulator.pressButton "a" s2)
    Assert.Equal<byte array>(Emulator.getFrameBuffer a1, Emulator.getFrameBuffer a2)
    Assert.Equal(a1.TotalCycles, a2.TotalCycles)

[<Fact>]
let ``ROM のバイト列は保存されない`` () =
    let json = State.serialize (Emulator.runFrames 10 (boot ()))
    // ROM はキーごと消すのではなく空で保存し、復元時に現在の state から差し替える
    Assert.Contains("\"Rom\":\"\"", json)

[<Fact>]
let ``saveTo/loadFrom はファイル経由で往復できる`` () =
    let path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "gbfs-state-test.json")
    let s1 = Emulator.runFrames 120 (boot ())
    let len = State.saveTo path s1
    Assert.True(len > 1000, sprintf "JSON が小さすぎます: %d" len)
    let s2 = State.loadFrom path s1
    Assert.Equal(s1.TotalCycles, s2.TotalCycles)
    Assert.Equal(State.serialize s1, State.serialize s2)
    System.IO.File.Delete path

[<Fact>]
let ``復元した状態は ROM を引き継ぐ`` () =
    let s1 = Emulator.runFrames 60 (boot ())
    let s2 = State.deserialize (State.serialize s1) s1
    Assert.Equal(tinyRom.Length, s2.Cpu.Mem.Rom.Length)
