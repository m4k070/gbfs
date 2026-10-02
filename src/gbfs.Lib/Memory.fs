namespace gbfs.Lib

module Memory =
  /// ゲームボーイ メモリマップ
  /// 0x0000-0x3FFF: ROM Bank 0 (16KB)
  /// 0x4000-0x7FFF: ROM Bank 1-N (switchable, 16KB)
  /// 0x8000-0x9FFF: VRAM (8KB)
  /// 0xA000-0xBFFF: External RAM (8KB)
  /// 0xC000-0xDFFF: Work RAM (8KB)
  /// 0xE000-0xFDFF: Echo RAM (mirror of C000-DDFF)
  /// 0xFE00-0xFE9F: OAM (Sprite Attribute Table)
  /// 0xFEA0-0xFEFF: Not usable
  /// 0xFF00-0xFF7F: I/O Registers
  /// 0xFF80-0xFFFE: High RAM (HRAM)
  /// 0xFFFF: Interrupt Enable Register

  /// カートリッジのメモリバンクコントローラ種別 (ROM ヘッダ 0x0147 で決まる)
  type MbcType =
    | NoMbc          // ROM ONLY / ROM+RAM (0x00, 0x08, 0x09)
    | Mbc1
    | Mbc2
    | Mbc3
    | Mbc5

  let mbcTypeName (t: MbcType) =
    match t with
    | NoMbc -> "ROM"
    | Mbc1 -> "MBC1"
    | Mbc2 -> "MBC2"
    | Mbc3 -> "MBC3"
    | Mbc5 -> "MBC5"

  /// MBC3 の RTC。実時間 (unix 秒) を基準値に加算してレジスタ値を導出する簡易モデル。
  /// 実機と同じく電源断の間も時計が進む (バッテリー搭載カートの挙動) ので .sav には保存しない。
  type RtcState = {
    /// RTC 通算秒の基準値
    BaseSeconds: int64
    /// BaseSeconds を観測した実時刻 (unix 秒)
    BaseAt: int64
    /// DH bit6 (Halt)。true の間は時間が止まる
    Halted: bool
    /// Halt 開始時の通算秒
    HaltedSeconds: int64
    /// 0x6000-0x7FFF への直前の書き込み値 (0→1 でラッチ)
    LatchPrev: byte
    /// ラッチ済みレジスタ (S, M, H, DL, DH)。読み出しはこのスナップショットだけを返す
    LatchedRegs: byte array
  }

  /// RTC の現在時刻 (unix 秒)。テストで差し替えられるよう mutable にしてある。
  let mutable private rtcClock: unit -> int64 =
    fun () -> System.DateTimeOffset.UtcNow.ToUnixTimeSeconds()

  /// テスト用: RTC の基準時刻を固定する
  let setRtcClock (f: unit -> int64) = rtcClock <- f

  let rtcCreate () = {
    BaseSeconds = 0L
    BaseAt = rtcClock ()
    Halted = false
    HaltedSeconds = 0L
    LatchPrev = 0uy
    LatchedRegs = Array.zeroCreate 5
  }

  /// 通算秒 → (S, M, H, DL, DH bit0), 日カウンタ桁溢れ
  let private rtcDecode (total: int64) =
    let t = max 0L total
    let days = t / 86400L
    let regs =
      [| byte (t % 60L)
         byte ((t / 60L) % 60L)
         byte ((t / 3600L) % 24L)
         byte (days % 256L)
         byte ((days / 256L) % 2L) |]
    regs, (days >= 512L)

  let private rtcTotal (rtc: RtcState) =
    if rtc.Halted then rtc.HaltedSeconds
    else rtc.BaseSeconds + max 0L (rtcClock () - rtc.BaseAt)

  /// 現在時刻をラッチ済みレジスタへ取り込む (Halt / Carry ビットを DH に反映)
  let private rtcLatch (rtc: RtcState) =
    let regs, carry = rtcDecode (rtcTotal rtc)
    let dh =
      (regs.[4] &&& 0x01uy)
      ||| (if rtc.Halted then 0x40uy else 0uy)
      ||| (if carry then 0x80uy else 0uy)
    { rtc with LatchedRegs = [| regs.[0]; regs.[1]; regs.[2]; regs.[3]; dh |] }

  /// RTC レジスタ (0x08-0x0C) への書き込み。書き込んだ値を基準にカウンタを再設定する。
  let private rtcSetRegister (idx: int) (value: byte) (rtc: RtcState) =
    let cur = rtc.LatchedRegs
    let pick i = if idx = i then value else cur.[i]
    let s = min 59 (int (pick 0))
    let m = min 59 (int (pick 1))
    let h = min 23 (int (pick 2))
    let dl = int (pick 3)
    let dh = pick 4
    let days = dl ||| (int (dh &&& 0x01uy) <<< 8)
    let total = int64 s + 60L * int64 m + 3600L * int64 h + 86400L * int64 days
    let halted = (dh &&& 0x40uy) <> 0uy
    let carry = (dh &&& 0x80uy) <> 0uy
    { rtc with
        BaseSeconds = total
        BaseAt = rtcClock ()
        Halted = halted
        HaltedSeconds = total
        LatchedRegs =
          [| byte s; byte m; byte h; byte (days &&& 0xFF)
             byte ((days / 256) &&& 0x01) ||| (if halted then 0x40uy else 0uy) ||| (if carry then 0x80uy else 0uy) |] }

  type MbcState = {
    MbcType: MbcType
    /// ROM bank レジスタの生値 (MBC1:5bit / MBC2:4bit / MBC3:7bit / MBC5:9bit)。
    /// MBC1 の 00→01 変換は読み出し時に行う (Pan Docs: 変換判定は 5bit 全域を見る)
    RomBank: int
    /// RAM bank (MBC3: 0x00-0x07 RAM / 0x08-0x0C RTC レジスタ選択 / MBC1: 上位 ROM bank 2bit)
    RamBank: int
    RamEnabled: bool
    /// MBC1: 0=simple, 1=advanced (0x6000-0x7FFF)
    BankingMode: int
    /// MBC1M (マルチカート配線)。2bit レジスタが ROM bank の bit4-5 に入る
    IsMulticart: bool
    /// バッテリーバックアップ付き (ROM ヘッダ 0x0147) — .sav の保存対象
    HasBattery: bool
    Rtc: RtcState
  }

  type MemoryBus = {
    Rom: byte array        // Full ROM data
    Vram: byte array       // 8KB Video RAM
    ExtRam: byte array     // External RAM (MBC2 は 512 nibble を 1 byte/nibble で保持)
    Wram: byte array       // 8KB Work RAM
    Oam: byte array        // 160 bytes OAM
    Io: byte array         // 128 bytes I/O
    Hram: byte array       // 127 bytes High RAM
    Ie: byte               // Interrupt Enable Register
    Mbc: MbcState          // MBC state
  }

  /// ROM ヘッダ 0x0147 のマッパー種別 (Pan Docs: The Cartridge Header)
  let private detectMbcType (rom: byte array) =
    if rom.Length > 0x0147 then
      match rom.[0x0147] with
      | 0x00uy | 0x08uy | 0x09uy -> NoMbc
      | 0x01uy | 0x02uy | 0x03uy -> Mbc1
      | 0x05uy | 0x06uy -> Mbc2
      | 0x0Fuy | 0x10uy | 0x11uy | 0x12uy | 0x13uy -> Mbc3
      | 0x19uy | 0x1Auy | 0x1Buy | 0x1Cuy | 0x1Duy | 0x1Euy -> Mbc5
      | _ -> NoMbc
    else NoMbc

  /// ROM ヘッダ 0x0147 がバッテリーバックアップ付きかを示す
  let private detectBattery (rom: byte array) =
    if rom.Length > 0x0147 then
      match rom.[0x0147] with
      | 0x03uy | 0x06uy | 0x09uy | 0x0Duy | 0x0Fuy | 0x10uy | 0x13uy | 0x1Buy | 0x1Euy -> true
      | _ -> false
    else false

  /// MBC1M (マルチカート) 判定: ROM bank $10 の先頭にも Nintendo ロゴ (0x0104-0x0133) があるか
  /// (Pan Docs MBC1「MBC1M multi-game compilation carts」節)
  let private detectMulticart (mbcType: MbcType) (rom: byte array) =
    let logoOffset = 0x0104
    let logoLen = 0x30
    mbcType = Mbc1
    && rom.Length >= 0x11 * 0x4000
    && Array.forall2 (=) rom.[0x10 * 0x4000 + logoOffset .. 0x10 * 0x4000 + logoOffset + logoLen - 1]
                       rom.[logoOffset .. logoOffset + logoLen - 1]

  let private getRamSize (mbcType: MbcType) (rom: byte array) =
    if mbcType = Mbc2 then 0x200   // 512 × 4bit (1 nibble = 1 byte で保持)
    elif rom.Length > 0x0149 then
      match rom.[0x0149] with
      | 0x01uy -> 0x800      // 2KB
      | 0x02uy -> 0x2000     // 8KB
      | 0x03uy -> 0x8000     // 32KB (4 banks)
      | 0x04uy -> 0x20000    // 128KB
      | 0x05uy -> 0x10000    // 64KB
      | _ -> 0x2000           // default 8KB
    else 0x2000

  let create () = {
    Rom = Array.zeroCreate 0x8000      // 32KB ROM (default)
    Vram = Array.zeroCreate 0x2000     // 8KB VRAM
    ExtRam = Array.zeroCreate 0x2000   // 8KB External RAM
    Wram = Array.zeroCreate 0x2000     // 8KB Work RAM
    Oam = Array.zeroCreate 0xA0        // 160 bytes OAM
    Io = Array.zeroCreate 0x80         // 128 bytes I/O
    Hram = Array.zeroCreate 0x7F       // 127 bytes High RAM
    Ie = 0uy
    Mbc =
      { MbcType = NoMbc
        RomBank = 1
        RamBank = 0
        RamEnabled = false
        BankingMode = 0
        IsMulticart = false
        HasBattery = false
        Rtc = rtcCreate () } }

  let loadRom (rom: byte array) (mem: MemoryBus) =
    let mbcType = detectMbcType rom
    let ramSize = getRamSize mbcType rom
    { mem with
        Rom = Array.copy rom
        ExtRam = Array.zeroCreate ramSize
        Mbc =
          { mem.Mbc with
              MbcType = mbcType
              RomBank = 1
              RamBank = 0
              RamEnabled = false
              BankingMode = 0
              IsMulticart = detectMulticart mbcType rom
              HasBattery = detectBattery rom
              Rtc = rtcCreate () } }

  // ============================================================
  // MBC bank 選択
  // ============================================================

  let private bankCount (mem: MemoryBus) = max 1 (mem.Rom.Length / 0x4000)

  /// カートのバンク数に合わせたマスク (Pan Docs: 余分な上位ビットは無視される)
  let private maskBank (mem: MemoryBus) (bank: int) =
    let banks = bankCount mem
    if (banks &&& (banks - 1)) = 0 then bank &&& (banks - 1)
    else ((bank % banks) + banks) % banks

  let private readRomBank (bank: int) (offset: int) (mem: MemoryBus) =
    let addr = maskBank mem bank * 0x4000 + offset
    if addr >= 0 && addr < mem.Rom.Length then mem.Rom.[addr] else 0uy

  /// MBC1 の 0x4000-0x7FFF バンク: (2bit レジスタ << 5) + 変換済み 5bit レジスタ
  let private mbc1Bank (reg: int) (upper: int) =
    (upper <<< 5) + (if reg = 0 then 1 else reg)

  /// MBC1M のバンク: 2bit レジスタは bit4-5 に入り、5bit レジスタの bit4 は無視される
  /// (00→01 変換の判定だけは 5bit 全域で行う)
  let private mbc1mBank (reg: int) (upper: int) =
    (upper <<< 4) + ((if reg = 0 then 1 else reg) &&& 0x0F)

  /// MBC1 の RAM bank (mode 0 では常に bank 0 に固定される)
  let private mbc1RamBank (mem: MemoryBus) =
    if mem.Mbc.BankingMode = 0 then 0 else mem.Mbc.RamBank

  let private readExtRam (offset: int) (mem: MemoryBus) : byte =
    if not mem.Mbc.RamEnabled then 0xFFuy
    else
      match mem.Mbc.MbcType with
      | Mbc2 ->
        // 内蔵 512 nibble。上位 4bit は不定 (0xF を返す実装に合わせる)
        0xF0uy ||| (mem.ExtRam.[offset &&& 0x1FF] &&& 0x0Fuy)
      | Mbc3 when mem.Mbc.RamBank >= 0x08 ->
        let idx = mem.Mbc.RamBank - 0x08
        if idx < 5 then mem.Mbc.Rtc.LatchedRegs.[idx] else 0xFFuy
      | _ ->
        if mem.ExtRam.Length = 0 then 0xFFuy
        else
          let bank = if mem.Mbc.MbcType = Mbc1 then mbc1RamBank mem else mem.Mbc.RamBank
          mem.ExtRam.[(bank * 0x2000 + offset) &&& (mem.ExtRam.Length - 1)]

  let read (addr: uint16) (mem: MemoryBus) : byte =
    let a = int addr
    match a with
    | _ when a < 0x4000 ->
      // ROM Bank 0 (MBC1 mode 1: 2bit レジスタでバンクを差し替え)
      let bank =
        match mem.Mbc.MbcType with
        | Mbc1 when mem.Mbc.BankingMode = 1 ->
          if mem.Mbc.IsMulticart then mem.Mbc.RamBank <<< 4 else mem.Mbc.RamBank <<< 5
        | _ -> 0
      readRomBank bank a mem
    | _ when a < 0x8000 ->
      // ROM Bank 1-N (switchable)
      let offset = a - 0x4000
      match mem.Mbc.MbcType with
      | NoMbc -> readRomBank 1 offset mem
      | Mbc1 ->
        if mem.Mbc.IsMulticart then readRomBank (mbc1mBank mem.Mbc.RomBank mem.Mbc.RamBank) offset mem
        else readRomBank (mbc1Bank mem.Mbc.RomBank mem.Mbc.RamBank) offset mem
      | Mbc2 | Mbc3 -> readRomBank (if mem.Mbc.RomBank = 0 then 1 else mem.Mbc.RomBank) offset mem
      | Mbc5 -> readRomBank mem.Mbc.RomBank offset mem
    | _ when a < 0xA000 ->
      // VRAM
      mem.Vram.[a - 0x8000]
    | _ when a < 0xC000 ->
      // External RAM / RTC (with MBC bank switching)
      let offset = a - 0xA000
      if mem.Mbc.MbcType = NoMbc then
        if mem.ExtRam.Length > 0 then mem.ExtRam.[offset &&& (mem.ExtRam.Length - 1)] else 0xFFuy
      else
        readExtRam offset mem
    | _ when a < 0xE000 ->
      // Work RAM
      mem.Wram.[a - 0xC000]
    | _ when a < 0xFE00 ->
      // Echo RAM (mirror of C000-DDFF)
      mem.Wram.[a - 0xE000]
    | _ when a < 0xFEA0 ->
      // OAM
      mem.Oam.[a - 0xFE00]
    | _ when a < 0xFF00 ->
      // Not usable
      0xFFuy
    | _ when a < 0xFF80 ->
      // I/O
      mem.Io.[a - 0xFF00]
    | _ when a < 0xFFFF ->
      // HRAM
      mem.Hram.[a - 0xFF80]
    | 0xFFFF ->
      // IE register
      mem.Ie
    | _ -> 0uy

  let private setRamEnabled (value: byte) (mem: MemoryBus) =
    { mem with Mbc = { mem.Mbc with RamEnabled = (value &&& 0x0Fuy) = 0x0Auy } }

  let private writeExtRam (offset: int) (value: byte) (mem: MemoryBus) : MemoryBus =
    if not mem.Mbc.RamEnabled then mem
    else
      match mem.Mbc.MbcType with
      | Mbc2 ->
        mem.ExtRam.[offset &&& 0x1FF] <- value &&& 0x0Fuy
        mem
      | Mbc3 when mem.Mbc.RamBank >= 0x08 ->
        let idx = mem.Mbc.RamBank - 0x08
        if idx < 5 then { mem with Mbc = { mem.Mbc with Rtc = rtcSetRegister idx value mem.Mbc.Rtc } }
        else mem
      | _ ->
        if mem.ExtRam.Length > 0 then
          let bank = if mem.Mbc.MbcType = Mbc1 then mbc1RamBank mem else mem.Mbc.RamBank
          mem.ExtRam.[(bank * 0x2000 + offset) &&& (mem.ExtRam.Length - 1)] <- value
        mem

  let write (addr: uint16) (value: byte) (mem: MemoryBus) : MemoryBus =
    let a = int addr
    match a with
    | _ when a < 0x4000 ->
      match mem.Mbc.MbcType with
      | NoMbc -> mem
      | Mbc1 | Mbc3 ->
        if a < 0x2000 then setRamEnabled value mem
        else
          let mask = if mem.Mbc.MbcType = Mbc1 then 0x1F else 0x7F
          { mem with Mbc = { mem.Mbc with RomBank = int value &&& mask } }
      | Mbc2 ->
        // アドレス bit8 が 0: RAM enable / 1: ROM bank (0x0000-0x3FFF の全域でこの規則)
        if (a &&& 0x100) = 0 then setRamEnabled value mem
        else { mem with Mbc = { mem.Mbc with RomBank = int value &&& 0x0F } }
      | Mbc5 ->
        if a < 0x2000 then setRamEnabled value mem
        elif a < 0x3000 then
          // ROM bank 下位 8bit (MBC5 では bank 0 も有効)
          { mem with Mbc = { mem.Mbc with RomBank = (mem.Mbc.RomBank &&& 0x100) ||| int value } }
        else
          // ROM bank bit8
          { mem with Mbc = { mem.Mbc with RomBank = (mem.Mbc.RomBank &&& 0xFF) ||| ((int value &&& 0x01) <<< 8) } }
    | _ when a < 0x6000 ->
      match mem.Mbc.MbcType with
      | Mbc1 -> { mem with Mbc = { mem.Mbc with RamBank = int value &&& 0x03 } }
      | Mbc3 | Mbc5 -> { mem with Mbc = { mem.Mbc with RamBank = int value &&& 0x0F } }
      | _ -> mem
    | _ when a < 0x8000 ->
      match mem.Mbc.MbcType with
      | Mbc1 -> { mem with Mbc = { mem.Mbc with BankingMode = int value &&& 0x01 } }
      | Mbc3 ->
        // $00 → $01 の書き込みで現在時刻をラッチする
        let rtc = mem.Mbc.Rtc
        let latched = if rtc.LatchPrev = 0uy && value = 0x01uy then rtcLatch rtc else rtc
        { mem with Mbc = { mem.Mbc with Rtc = { latched with LatchPrev = value } } }
      | _ -> mem
    | _ when a < 0xA000 ->
      // VRAM
      mem.Vram.[a - 0x8000] <- value
      mem
    | _ when a < 0xC000 ->
      // External RAM / RTC
      let offset = a - 0xA000
      if mem.Mbc.MbcType = NoMbc then
        if mem.ExtRam.Length > 0 then mem.ExtRam.[offset &&& (mem.ExtRam.Length - 1)] <- value
        mem
      else
        writeExtRam offset value mem
    | _ when a < 0xE000 ->
      // Work RAM
      mem.Wram.[a - 0xC000] <- value
      mem
    | _ when a < 0xFE00 ->
      // Echo RAM
      mem.Wram.[a - 0xE000] <- value
      mem
    | _ when a < 0xFEA0 ->
      // OAM
      mem.Oam.[a - 0xFE00] <- value
      mem
    | _ when a < 0xFF00 ->
      // Not usable - ignore
      mem
    | _ when a < 0xFF80 ->
      // I/O
      mem.Io.[a - 0xFF00] <- value
      mem
    | _ when a < 0xFFFF ->
      // HRAM
      mem.Hram.[a - 0xFF80] <- value
      mem
    | 0xFFFF ->
      // IE register
      { mem with Ie = value }
    | _ -> mem

  let read16 (addr: uint16) (mem: MemoryBus) : uint16 =
    let lo = read addr mem
    let hi = read (addr + 1us) mem
    uint16 lo ||| (uint16 hi <<< 8)

  let write16 (addr: uint16) (value: uint16) (mem: MemoryBus) : MemoryBus =
    let lo = byte (value &&& 0xFFus)
    let hi = byte (value >>> 8)
    mem |> write addr lo |> write (addr + 1us) hi

  type InterruptType =
      | VBlankInterrupt
      | LcdStatInterrupt
      | TimerInterrupt
      | SerialInterrupt
      | JoypadInterrupt

  let requestInterrupt (interruptType: InterruptType) (mem: MemoryBus) : MemoryBus =
      let ifReg = read 0xFF0Fus mem
      let newIfReg =
          match interruptType with
          | VBlankInterrupt -> ifReg ||| 0x01uy
          | LcdStatInterrupt -> ifReg ||| 0x02uy
          | TimerInterrupt -> ifReg ||| 0x04uy
          | SerialInterrupt -> ifReg ||| 0x08uy
          | JoypadInterrupt -> ifReg ||| 0x10uy
      write 0xFF0Fus newIfReg mem
