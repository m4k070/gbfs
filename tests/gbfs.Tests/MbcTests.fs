/// MBC1/2/3/5 (バンク切り替え・外部 RAM・RTC・バッテリー) のテスト。
/// 公開テスト ROM (mooneye emulator-only) は MBC1/2/5 のみをカバーするため、
/// MBC3 と RTC、.sav の入出力はここで検証する。
module MbcTests

open Xunit
open gbfs.Lib

/// テスト用 ROM: 各バンクの先頭 2 バイトにバンク番号 (リトルエンディアン) を置く。
/// ヘッダ領域 (0x0104-0x0133) には非ゼロのダミーロゴを置く (MBC1M 判定が反応しないように)
let private makeRom (banks: int) (cartType: byte) (ramSize: byte) =
    let rom = Array.zeroCreate (banks * 0x4000)
    for b in 0 .. banks - 1 do
        rom.[b * 0x4000] <- byte (b &&& 0xFF)
        rom.[b * 0x4000 + 1] <- byte ((b >>> 8) &&& 0xFF)
    for i in 0 .. 0x2F do
        rom.[0x0104 + i] <- byte (i + 1)
    rom.[0x147] <- cartType
    rom.[0x149] <- ramSize
    rom

/// Nintendo ロゴ相当のバイト列を bank $10 の先頭にも複製した MBC1M 風 ROM
let private makeMulticartRom (banks: int) =
    let rom = makeRom banks 0x01uy 0x00uy
    let logoLen = 0x30
    Array.blit rom 0x0104 rom (0x10 * 0x4000 + 0x0104) logoLen
    rom

let private load (rom: byte array) =
    Memory.loadRom rom (Memory.create ())

let private bankOf (addr: uint16) (mem: Memory.MemoryBus) =
    int (Memory.read addr mem) ||| (int (Memory.read (addr + 1us) mem) <<< 8)

let private enableRam (mem: Memory.MemoryBus) = Memory.write 0x0000us 0x0Auy mem

// ============================================================
// ヘッダ検出
// ============================================================

module HeaderTests =
    [<Fact>]
    let ``cartridge type 0x0147 selects the mapper`` () =
        let ofType t = (load (makeRom 2 t 0x00uy)).Mbc.MbcType
        Assert.Equal(Memory.Mbc1, ofType 0x01uy)
        Assert.Equal(Memory.Mbc1, ofType 0x03uy)
        Assert.Equal(Memory.Mbc2, ofType 0x06uy)
        Assert.Equal(Memory.Mbc3, ofType 0x13uy)
        Assert.Equal(Memory.Mbc5, ofType 0x1Buy)
        Assert.Equal(Memory.NoMbc, ofType 0x00uy)

    [<Fact>]
    let ``battery flag follows cartridge type`` () =
        Assert.True((load (makeRom 2 0x03uy 0x02uy)).Mbc.HasBattery)   // MBC1+RAM+BATTERY
        Assert.True((load (makeRom 2 0x06uy 0x00uy)).Mbc.HasBattery)   // MBC2+BATTERY
        Assert.True((load (makeRom 2 0x13uy 0x03uy)).Mbc.HasBattery)   // MBC3+TIMER+RAM+BATTERY
        Assert.True((load (makeRom 2 0x1Buy 0x03uy)).Mbc.HasBattery)   // MBC5+RAM+BATTERY
        Assert.False((load (makeRom 2 0x01uy 0x02uy)).Mbc.HasBattery)  // MBC1 (no battery)
        Assert.False((load (makeRom 2 0x19uy 0x03uy)).Mbc.HasBattery)  // MBC5 (no battery)

    [<Fact>]
    let ``MBC2 uses 512 nibbles of built-in RAM`` () =
        Assert.Equal(0x200, (load (makeRom 2 0x06uy 0x00uy)).ExtRam.Length)

    [<Fact>]
    let ``RAM size comes from 0x0149`` () =
        Assert.Equal(0x800, (load (makeRom 2 0x03uy 0x01uy)).ExtRam.Length)
        Assert.Equal(0x2000, (load (makeRom 2 0x03uy 0x02uy)).ExtRam.Length)
        Assert.Equal(0x8000, (load (makeRom 2 0x03uy 0x03uy)).ExtRam.Length)

// ============================================================
// MBC1
// ============================================================

module Mbc1Tests =
    [<Fact>]
    let ``ROM bank 0 written to the 5-bit register selects bank 1`` () =
        let mem = Memory.write 0x2000us 0x00uy (load (makeRom 8 0x01uy 0x00uy))
        Assert.Equal(1, bankOf 0x4000us mem)

    [<Fact>]
    let ``5th bit set maps bank 0 into the switchable region on a 256KiB cart`` () =
        // Pan Docs: 00→01 変換は 5bit 全域で判定し、バンク選択はカートのバンク数でマスクされる
        let mem = Memory.write 0x2000us 0x10uy (load (makeRom 16 0x01uy 0x00uy))
        Assert.Equal(0, bankOf 0x4000us mem)

    [<Fact>]
    let ``secondary register supplies ROM bank bits 5-6`` () =
        let rom = makeRom 128 0x01uy 0x00uy    // 2MiB
        let mem =
            load rom
            |> Memory.write 0x2000us 0x01uy
            |> Memory.write 0x4000us 0x03uy
        Assert.Equal(0x61, bankOf 0x4000us mem)

    [<Fact>]
    let ``mode 1 remaps ROM bank 0 via the secondary register`` () =
        let rom = makeRom 128 0x01uy 0x00uy
        let mem =
            load rom
            |> Memory.write 0x2000us 0x01uy
            |> Memory.write 0x4000us 0x02uy
            |> Memory.write 0x6000us 0x01uy
        Assert.Equal(0x40, bankOf 0x0000us mem)   // 0000-3FFF が $40 に差し替わる
        Assert.Equal(0x41, bankOf 0x4000us mem)   // 4000-7FFF は (2bit << 5) + 1 のまま

    [<Fact>]
    let ``mode 0 keeps bank 0 fixed in the low region`` () =
        let rom = makeRom 128 0x01uy 0x00uy
        let mem =
            load rom
            |> Memory.write 0x4000us 0x02uy
            |> Memory.write 0x6000us 0x00uy
        Assert.Equal(0, bankOf 0x0000us mem)

    [<Fact>]
    let ``external RAM requires the 0x0A enable value`` () =
        let rom = makeRom 4 0x03uy 0x02uy
        let disabled = Memory.write 0xA000us 0x12uy (load rom)
        Assert.Equal(0xFFuy, Memory.read 0xA000us disabled)      // 無効時は書けない
        let enabled = Memory.write 0xA000us 0x12uy (enableRam (load rom))
        Assert.Equal(0x12uy, Memory.read 0xA000us enabled)

    [<Fact>]
    let ``mode 0 locks RAM to bank 0`` () =
        let rom = makeRom 8 0x03uy 0x03uy    // 32KiB RAM (4 banks)
        let mem =
            load rom
            |> enableRam
            |> Memory.write 0x4000us 0x01uy       // RAM bank 1 を選択
            |> Memory.write 0x6000us 0x00uy       // mode 0
            |> Memory.write 0xA000us 0xAAuy
        Assert.Equal(0xAAuy, mem.ExtRam.[0x0000])   // bank 0 に書かれる
        Assert.Equal(0x00uy, mem.ExtRam.[0x2000])

    [<Fact>]
    let ``mode 1 selects the RAM bank`` () =
        let rom = makeRom 8 0x03uy 0x03uy
        let mem =
            load rom
            |> enableRam
            |> Memory.write 0x4000us 0x01uy
            |> Memory.write 0x6000us 0x01uy
            |> Memory.write 0xA000us 0xAAuy
        Assert.Equal(0xAAuy, mem.ExtRam.[0x2000])

    [<Fact>]
    let ``8KiB RAM mirrors every RAM bank`` () =
        // 8KiB (1 bank) カートでは 2bit レジスタをどう設定しても同じ RAM が見える
        let rom = makeRom 8 0x03uy 0x02uy
        let mem =
            load rom
            |> enableRam
            |> Memory.write 0x6000us 0x01uy
            |> Memory.write 0x4000us 0x03uy
            |> Memory.write 0xA000us 0x5Auy
        Assert.Equal(0x5Auy, Memory.read 0xA000us (Memory.write 0x4000us 0x00uy mem))

    [<Fact>]
    let ``MBC1M applies the secondary register to bank bits 4-5`` () =
        let mem =
            load (makeMulticartRom 64)
            |> Memory.write 0x2000us 0x01uy
            |> Memory.write 0x4000us 0x01uy
        Assert.Equal(0x11, bankOf 0x4000us mem)

    [<Fact>]
    let ``MBC1M mode 1 selects the game in the low region`` () =
        let mem =
            load (makeMulticartRom 64)
            |> Memory.write 0x4000us 0x02uy
            |> Memory.write 0x6000us 0x01uy
        Assert.Equal(0x20, bankOf 0x0000us mem)

    [<Fact>]
    let ``plain 1 MiB cart is not detected as MBC1M`` () =
        Assert.False((load (makeRom 64 0x01uy 0x00uy)).Mbc.IsMulticart)

// ============================================================
// MBC2
// ============================================================

module Mbc2Tests =
    [<Fact>]
    let ``address bit 8 selects RAM enable vs ROM bank`` () =
        let rom = makeRom 16 0x06uy 0x00uy
        // bit8 = 0 → RAM enable
        let mem = Memory.write 0x0000us 0x0Auy (load rom)
        Assert.True(mem.Mbc.RamEnabled)
        Assert.Equal(1, mem.Mbc.RomBank)
        // bit8 = 1 → ROM bank (0x2100 も有効)
        let mem2 = Memory.write 0x2300us 0x05uy mem
        Assert.Equal(5, mem2.Mbc.RomBank)
        Assert.Equal(5, bankOf 0x4000us mem2)
        // bit8 = 0 → RAM disable
        let mem3 = Memory.write 0x0200us 0x00uy mem2
        Assert.False(mem3.Mbc.RamEnabled)

    [<Fact>]
    let ``ROM bank 0 written selects bank 1`` () =
        let mem = Memory.write 0x2100us 0x00uy (load (makeRom 16 0x06uy 0x00uy))
        Assert.Equal(1, bankOf 0x4000us mem)

    [<Fact>]
    let ``RAM stores 4-bit values and forces 0xF in the upper nibble`` () =
        let mem =
            load (makeRom 16 0x06uy 0x00uy)
            |> Memory.write 0x0000us 0x0Auy
            |> Memory.write 0xA000us 0x5Auy
        Assert.Equal(0xFAuy, Memory.read 0xA000us mem)    // 下位 4bit のみ保持
        Assert.Equal(0xFAuy, Memory.read 0xA200us mem)    // 下位 9bit のみで索引 (A200 は A000 の鏡像)
        let mem2 = Memory.write 0xA200us 0x05uy mem
        Assert.Equal(0xF5uy, Memory.read 0xA000us mem2)   // A200 への書き込みが A000 に届く

    [<Fact>]
    let ``RAM access is disabled until 0x0A is written`` () =
        let mem = Memory.write 0xA000us 0x0Fuy (load (makeRom 16 0x06uy 0x00uy))
        Assert.Equal(0xFFuy, Memory.read 0xA000us mem)

// ============================================================
// MBC3 (バンク + RTC)
// ============================================================

module Mbc3Tests =
    [<Fact>]
    let ``7-bit ROM bank register with 00 to 01 translation`` () =
        let rom = makeRom 128 0x13uy 0x03uy
        Assert.Equal(0x7F, bankOf 0x4000us (Memory.write 0x2000us 0x7Fuy (load rom)))
        Assert.Equal(1, bankOf 0x4000us (Memory.write 0x2000us 0x00uy (load rom)))

    [<Fact>]
    let ``RAM bank 0-3 is selected by 0x4000-0x5FFF`` () =
        let rom = makeRom 8 0x13uy 0x03uy    // 32KiB RAM
        let mem =
            load rom
            |> enableRam
            |> Memory.write 0x4000us 0x02uy
            |> Memory.write 0xA000us 0x77uy
        Assert.Equal(0x77uy, mem.ExtRam.[0x4000])

    [<Fact>]
    let ``RTC registers appear after 00 to 01 latch`` () =
        let mutable now = 1_700_000_000L
        Memory.setRtcClock (fun () -> now)
        try
            let rom = makeRom 8 0x13uy 0x03uy
            let enable = enableRam (load rom)
            let latch mem = mem |> Memory.write 0x6000us 0x00uy |> Memory.write 0x6000us 0x01uy
            // 初期値は 0 (BaseSeconds = 0)
            let t0 = latch enable
            Assert.Equal(0uy, Memory.read 0xA000us (Memory.write 0x4000us 0x08uy t0))
            // S=30 を書き込み、61 秒進めて再ラッチ → S=31, M=1
            let written = Memory.write 0xA000us 30uy (Memory.write 0x4000us 0x08uy enable)
            now <- now + 61L
            let t1 = latch written
            Assert.Equal(31uy, Memory.read 0xA000us (Memory.write 0x4000us 0x08uy t1))
            Assert.Equal(1uy, Memory.read 0xA000us (Memory.write 0x4000us 0x09uy t1))
        finally
            Memory.setRtcClock (fun () -> System.DateTimeOffset.UtcNow.ToUnixTimeSeconds())

    [<Fact>]
    let ``RTC halt flag freezes the clock`` () =
        let mutable now = 1_700_000_000L
        Memory.setRtcClock (fun () -> now)
        try
            let rom = makeRom 8 0x13uy 0x03uy
            let enable = enableRam (load rom)
            let latch mem = mem |> Memory.write 0x6000us 0x00uy |> Memory.write 0x6000us 0x01uy
            // S=10 を書いてから Halt を立てる
            let running =
                enable
                |> Memory.write 0x4000us 0x08uy
                |> Memory.write 0xA000us 10uy
            let halted =
                running
                |> Memory.write 0x4000us 0x0Cuy
                |> Memory.write 0xA000us 0x40uy      // DH bit6 = Halt
            now <- now + 3600L
            let t = latch halted
            let s = Memory.read 0xA000us (Memory.write 0x4000us 0x08uy t)
            let dh = Memory.read 0xA000us (Memory.write 0x4000us 0x0Cuy t)
            Assert.Equal(10uy, s)
            Assert.True((dh &&& 0x40uy) <> 0uy)
        finally
            Memory.setRtcClock (fun () -> System.DateTimeOffset.UtcNow.ToUnixTimeSeconds())

    [<Fact>]
    let ``RTC day counter carries at 512 days`` () =
        let mutable now = 1_700_000_000L
        Memory.setRtcClock (fun () -> now)
        try
            let rom = makeRom 8 0x13uy 0x03uy
            let enable = enableRam (load rom)
            let latch mem = mem |> Memory.write 0x6000us 0x00uy |> Memory.write 0x6000us 0x01uy
            // 日カウンタ = 511 (DL=0xFF, DH bit0=1)
            let at511 =
                enable
                |> Memory.write 0x4000us 0x0Buy
                |> Memory.write 0xA000us 0xFFuy
                |> Memory.write 0x4000us 0x0Cuy
                |> Memory.write 0xA000us 0x01uy
            now <- now + 86400L
            let t = latch at511
            let dh = Memory.read 0xA000us (Memory.write 0x4000us 0x0Cuy t)
            let dl = Memory.read 0xA000us (Memory.write 0x4000us 0x0Buy t)
            Assert.Equal(0uy, dl)                       // 512 日目 → 0
            Assert.True((dh &&& 0x80uy) <> 0uy)         // 桁溢れフラグ
        finally
            Memory.setRtcClock (fun () -> System.DateTimeOffset.UtcNow.ToUnixTimeSeconds())

// ============================================================
// MBC5
// ============================================================

module Mbc5Tests =
    [<Fact>]
    let ``bank 0 is selectable at 0x4000`` () =
        let mem = Memory.write 0x2000us 0x00uy (load (makeRom 8 0x19uy 0x00uy))
        Assert.Equal(0, bankOf 0x4000us mem)

    [<Fact>]
    let ``ROM bank is 9 bits wide`` () =
        let mem =
            load (makeRom 512 0x19uy 0x00uy)     // 8MiB
            |> Memory.write 0x2000us 0x03uy
            |> Memory.write 0x3000us 0x01uy
        Assert.Equal(0x103, bankOf 0x4000us mem)

    [<Fact>]
    let ``0x3000 write only affects bit 8`` () =
        let mem =
            load (makeRom 512 0x19uy 0x00uy)
            |> Memory.write 0x2000us 0x05uy
            |> Memory.write 0x3000us 0x01uy
            |> Memory.write 0x2000us 0x07uy
        Assert.Equal(0x107, bankOf 0x4000us mem)

    [<Fact>]
    let ``RAM bank 0-15 is selected by 0x4000-0x5FFF`` () =
        let rom = makeRom 8 0x1Buy 0x04uy    // 128KiB RAM
        let mem =
            load rom
            |> enableRam
            |> Memory.write 0x4000us 0x0Fuy
            |> Memory.write 0xA000us 0x99uy
        Assert.Equal(0x99uy, mem.ExtRam.[0x0F * 0x2000])

    [<Fact>]
    let ``RAM bank writes are masked by the available RAM size`` () =
        let rom = makeRom 8 0x1Auy 0x02uy    // 8KiB RAM (1 bank)
        let mem =
            load rom
            |> enableRam
            |> Memory.write 0x4000us 0x05uy
            |> Memory.write 0xA000us 0x42uy
        Assert.Equal(0x42uy, mem.ExtRam.[0])

// ============================================================
// バッテリーバックアップ (.sav)
// ============================================================

module SaveRamTests =
    let private withTempDir (f: string -> unit) =
        let dir = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "gbfs-sav-" + System.Guid.NewGuid().ToString("N"))
        System.IO.Directory.CreateDirectory dir |> ignore
        try f dir
        finally System.IO.Directory.Delete(dir, true)

    [<Fact>]
    let ``RAM round-trips through .sav`` () =
        withTempDir (fun dir ->
            let romPath = System.IO.Path.Combine(dir, "game.gb")
            System.IO.File.WriteAllBytes(romPath, makeRom 8 0x13uy 0x03uy)   // MBC3+RAM+TIMER+BATTERY
            let state =
                Emulator.loadRomWithSave romPath (Emulator.create ())
                |> Emulator.writeMemory 0x0000us [| 0x0Auy |]
                |> Emulator.writeMemory 0xA000us [| 0x12uy; 0x34uy; 0x56uy |]
            Assert.True(Emulator.saveRam romPath state)
            Assert.True(System.IO.File.Exists(Emulator.saveRamPath romPath))
            let ram = Emulator.getSaveRam (Emulator.loadRomWithSave romPath (Emulator.create ()))
            Assert.Equal(0x8000, ram.Length)
            Assert.Equal(0x12uy, ram.[0])
            Assert.Equal(0x56uy, ram.[2]))

    [<Fact>]
    let ``no .sav is written for carts without a battery`` () =
        withTempDir (fun dir ->
            let romPath = System.IO.Path.Combine(dir, "nobatt.gb")
            System.IO.File.WriteAllBytes(romPath, makeRom 8 0x01uy 0x02uy)   // MBC1 (no battery)
            let state = Emulator.loadRomWithSave romPath (Emulator.create ())
            Assert.False(Emulator.hasBattery state)
            Assert.False(Emulator.saveRam romPath state)
            Assert.False(System.IO.File.Exists(Emulator.saveRamPath romPath)))

    [<Fact>]
    let ``over-sized .sav is truncated to the RAM size`` () =
        withTempDir (fun dir ->
            let romPath = System.IO.Path.Combine(dir, "big.gb")
            System.IO.File.WriteAllBytes(romPath, makeRom 8 0x03uy 0x02uy)   // 8KiB RAM
            System.IO.File.WriteAllBytes(Emulator.saveRamPath romPath, Array.replicate 0x4000 0xEEuy)
            let state = Emulator.loadRomWithSave romPath (Emulator.create ())
            Assert.Equal(0x2000, (Emulator.getSaveRam state).Length))

    [<Fact>]
    let ``mapper name is reported`` () =
        Assert.Equal("MBC3", Emulator.getMapperName (Emulator.loadRom (makeRom 4 0x13uy 0x03uy) (Emulator.create ())))
