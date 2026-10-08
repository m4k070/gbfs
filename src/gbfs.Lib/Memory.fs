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

  /// マシン種別。CGB でもカートが CGB 非対応 (ヘッダ 0x0143 bit7 = 0) なら
  /// DMG 互換モードで動き、CGB 専用レジスタは 0xFF を読む (Pan Docs: CGB Registers)。
  type MachineMode =
    | Dmg
    | Cgb

  let machineName = function
    | Dmg -> "DMG"
    | Cgb -> "CGB"

  type MemoryBus = {
    /// マシン種別 (電源投入直後の初期値に影響する)
    Machine: MachineMode
    /// CGB モードが解禁されているか (マシンが CGB かつ ヘッダ 0x0143 bit7 = 1)。ROM ロード時に確定する。
    /// false のとき CGB 専用レジスタは 0xFF を読み、書き込みを無視する
    /// (Pan Docs: CGB Registers "you must first unlock CGB features by changing byte 0143")
    CgbMode: bool
    Rom: byte array        // Full ROM data
    Vram: byte array       // Video RAM (CGB は 2 バンク = 16KB)
    VramBank: int          // 現在の VRAM バンク (0/1。CGB モードのみ有効)
    ExtRam: byte array     // External RAM (MBC2 は 512 nibble を 1 byte/nibble で保持)
    Wram: byte array       // Work RAM (CGB は 8 バンク = 32KB)
    /// OAM DMA (0xFF46) の転送要求。0xFF46 への書き込みで立ち、PPU が転送して消費する。
    /// レジスタ値だけで判定すると電源投入直後の 0xFF46 = 0xFF で誤って転送してしまう
    DmaRequest: bool
    /// シリアル転送の残りサイクル (0 = 転送中でない)。完了処理は Memory.tickSerial が行う
    SerialCycles: int
    Oam: byte array        // 160 bytes OAM
    Io: byte array         // 128 bytes I/O
    /// CGB の BG パレット RAM (8 パレット × 4 色 × 2 バイト = RGB555 リトルエンディアン)
    BgPalette: byte array
    /// CGB の OBJ パレット RAM (同上)
    ObjPalette: byte array
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

  /// 電源投入直後 (ブート ROM 完了時点 = PC 0x0100) の I/O レジスタ初期値。
  /// DMG: Pan Docs "Power-Up Sequence" の DMG/MGB 列。同表の根拠は
  ///      mooneye-test-suite acceptance/boot_hwio-dmgABCmgb (実機で検証済み)。
  /// CGB: mooneye-test-suite misc/boot_hwio-C (実機の CGB で検証済み)。
  ///      同 ROM のヘッダは 0x0143 = 0x00 なので CGB の DMG 互換モードの値であり、
  ///      CGB 専用レジスタは 0xFF を読む。
  /// 未使用レジスタはどちらも 0xFF。
  let private postBootIoFor (machine: MachineMode) : byte array =
    let io = Array.create 0x80 0xFFuy
    io.[0x00] <- 0xCFuy   // P1   (ボタン選択なし・押下なし)
    io.[0x01] <- 0x00uy   // SB
    io.[0x02] <- 0x7Euy   // SC
    io.[0x04] <- 0xABuy   // DIV  (Timer.postBootCounter の上位バイトと一致させること)
    io.[0x05] <- 0x00uy   // TIMA
    io.[0x06] <- 0x00uy   // TMA
    io.[0x07] <- 0xF8uy   // TAC  (タイマ停止, 4096Hz 選択)
    io.[0x0F] <- 0xE1uy   // IF   (VBlank フラグ + 未使用ビットは 1)
    Array.blit [| 0x80uy; 0xBFuy; 0xF3uy; 0xFFuy; 0x3Fuy; 0xFFuy; 0x3Fuy; 0x00uy
                  0xFFuy; 0x3Fuy; 0x7Fuy; 0xFFuy; 0x9Fuy; 0xFFuy; 0x3Fuy; 0xFFuy
                  0xFFuy; 0x00uy; 0x00uy; 0x3Fuy; 0x77uy; 0xF3uy; 0xF1uy |] 0 io 0x10 23  // NR10-NR52
    // NR14/NR24/NR34/NR44 の bit7 はトリガ要求フラグとして扱う (書き込み時のみ 1)。
    // 読み出しは常に 1 を返す (read 側でマスク) ので、ここでは 0 (要求なし) にしておく。
    // 初期値を 0xBF にすると起動直後に APU が全チャンネルを誤トリガーする。
    Array.fill io 0x30 16 0x00uy  // 波形 RAM (ブート直後の値は未検証のため 0x00)
    io.[0x40] <- 0x91uy   // LCDC (LCD on / BG on / OBJ on)
    io.[0x41] <- 0x85uy   // STAT (VBlank モード + LYC=LY)
    io.[0x42] <- 0x00uy   // SCY
    io.[0x43] <- 0x00uy   // SCX
    io.[0x44] <- 0x00uy   // LY
    io.[0x45] <- 0x00uy   // LYC
    io.[0x46] <- 0xFFuy   // DMA
    io.[0x47] <- 0xFCuy   // BGP
    io.[0x48] <- 0xFFuy   // OBP0
    io.[0x49] <- 0xFFuy   // OBP1
    io.[0x4A] <- 0x00uy   // WY
    io.[0x4B] <- 0x00uy   // WX
    if machine = Cgb then
      // CGB で DMG と値が異なるレジスタ (mooneye misc/boot_hwio-C の比較対象のみ)
      io.[0x00] <- 0xFFuy   // P1: CGB のブート ROM は 0x30 を書く (どちらのボタン群も非選択)
      io.[0x04] <- 0x26uy   // DIV: Timer.postBootCounter (CGB) の上位バイト
      io.[0x48] <- 0x00uy   // OBP0: CGB では未初期化値が 0x00
      io.[0x49] <- 0x00uy   // OBP1
      io.[0x68] <- 0xC8uy   // BCPS (互換モードの残響値)
      io.[0x6A] <- 0xD0uy   // OCPS
      io.[0x72] <- 0x00uy   // 未定義レジスタ
      io.[0x73] <- 0x00uy
      io.[0x75] <- 0x8Fuy
      io.[0x76] <- 0x00uy
      io.[0x77] <- 0x00uy
    io

  /// CGB マシンでの B レジスタ初期値 (Pan Docs "Power-Up Sequence" 脚注 3)。
  /// 旧ライセンスコードが 0x01、または 0x33 かつ新ライセンスコードが "01" (= 任天堂) なら
  /// タイトル 16 バイト (0x0134-0x0143) の総和、それ以外は 0x00。
  /// (mooneye misc/boot_regs-cgb はライセンス "ZZ" なので 0x00 を期待する)
  let cgbBootB (rom: byte array) : byte =
    if rom.Length < 0x0150 then 0x00uy
    else
      let oldLicensee = rom.[0x014B]
      let isNintendo =
        oldLicensee = 0x01uy
        || (oldLicensee = 0x33uy && rom.[0x0144] = 0x30uy && rom.[0x0145] = 0x31uy)
      if not isNintendo then 0x00uy
      else
        let mutable sum = 0
        for i in 0x0134 .. 0x0143 do sum <- sum + int rom.[i]
        byte (sum &&& 0xFF)

  let createWith (machine: MachineMode) () = {
    Machine = machine
    CgbMode = false                    // ROM ロード時に確定する
    Rom = Array.zeroCreate 0x8000      // 32KB ROM (default)
    Vram = Array.zeroCreate 0x4000     // 16KB VRAM (CGB の 2 バンク分)
    VramBank = 0
    ExtRam = Array.zeroCreate 0x2000   // 8KB External RAM
    Wram = Array.zeroCreate 0x8000     // 32KB Work RAM (CGB の 8 バンク分)
    DmaRequest = false
    SerialCycles = 0
    Oam = Array.zeroCreate 0xA0        // 160 bytes OAM
    Io = Array.copy (postBootIoFor machine)  // 128 bytes I/O (電源投入直後の値)
    BgPalette = Array.zeroCreate 64    // CGB: 8 パレット × 4 色 × 2 バイト
    ObjPalette = Array.zeroCreate 64
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

  /// DMG マシン (既定) のメモリを作る
  let create () = createWith Dmg ()

  let loadRom (rom: byte array) (mem: MemoryBus) =
    let mbcType = detectMbcType rom
    let ramSize = getRamSize mbcType rom
    // CGB モードの解禁はヘッダ 0x0143 bit7 (0x80 = CGB 対応, 0xC0 = CGB 専用)
    let cgbMode = mem.Machine = Cgb && rom.Length > 0x0143 && (rom.[0x0143] &&& 0x80uy) <> 0uy
    let io =
      if not cgbMode then mem.Io
      else
        // CGB モードの起動値。ブート ROM が書き込む値なので検証 ROM が無い (未検証)。
        // 非 CGB モード側の値は mooneye misc/boot_hwio-C で検証済み。
        let io = Array.copy mem.Io
        // Pan Docs "Power-Up Sequence" の CGB 列 (CGB モードの値)。
        // 非 CGB モード (DMG 互換) の値は mooneye misc/boot_hwio-C で検証済みだが、
        // CGB モード側はブート ROM が書く値で検証 ROM が無い
        io.[0x00] <- 0xCFuy   // P1: 0xCF (モデルによっては 0xC7)
        io.[0x02] <- 0x7Fuy   // SC (DMG は 0x7E)
        io.[0x46] <- 0x00uy   // DMA (DMG は 0xFF)
        io.[0x4D] <- 0x00uy   // KEY1: 通常速度 (bit7=0) / 未アーム (bit0=0)
        io.[0x56] <- 0x3Euy   // RP (赤外線ポート)
        io.[0x6C] <- 0x00uy   // OPRI: CGB 方式のオブジェクト優先度
        io.[0x70] <- 0x00uy   // SVBK: 0 を書いた状態 (実効バンクは 1、読み出しは 0xF8)
        io.[0x74] <- 0x00uy   // 未定義レジスタ (CGB モードでは 0x00 から読み書きできる)
        io
    { mem with
        CgbMode = cgbMode
        VramBank = 0
        Io = io
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

  /// VRAM の読み書き。CGB は 2 バンク (0 = タイル/マップ, 1 = 属性)。
  /// 非 CGB モードでは VBK の書き込みが無視されるため bank は常に 0 になる。
  let private vramIndex (bank: int) (offset: int) = (bank &&& 1) * 0x2000 + (offset &&& 0x1FFF)

  /// D000-DFFF に割り当てる WRAM バンク。非 CGB モードではバンク 1 固定。
  /// SVBK (0xFF70) の値 0 はバンク 1 として扱う (Pan Docs: CGB Registers / SVBK)
  let private wramBank (mem: MemoryBus) =
    if not mem.CgbMode then 1
    else
      let b = int mem.Io.[0x70] &&& 0x07
      if b = 0 then 1 else b

  let readVram (bank: int) (offset: int) (mem: MemoryBus) : byte =
    mem.Vram.[vramIndex bank offset]

  let private writeVram (bank: int) (offset: int) (value: byte) (mem: MemoryBus) : MemoryBus =
    mem.Vram.[vramIndex bank offset] <- value
    mem

  /// PPU 用: 常に VRAM バンク 0 を読む。
  /// CGB のタイルデータとタイルマップはバンク 0、属性はバンク 1 にあり、
  /// PPU のアクセスは CPU の VBK とは独立している。
  let readVramBank0 (addr: uint16) (mem: MemoryBus) : byte =
    readVram 0 (int addr - 0x8000) mem

  /// CGB のパレット RAM から RGB555 の色を取り出す (Pan Docs: Color Palettes)。
  /// パレット RAM は 8 パレット × 4 色 × 2 バイトで、色は RGB555 のリトルエンディアン。
  let private paletteColor (palette: byte array) (paletteIndex: int) (colorIndex: int) : uint16 =
    let i = ((paletteIndex &&& 0x07) * 4 + (colorIndex &&& 0x03)) * 2
    uint16 palette.[i] ||| (uint16 palette.[i + 1] <<< 8)

  /// BG パレットの色 (CGB モードのみ意味を持つ)
  let bgPaletteColor (paletteIndex: int) (colorIndex: int) (mem: MemoryBus) : uint16 =
    paletteColor mem.BgPalette paletteIndex colorIndex

  /// OBJ パレットの色 (CGB モードのみ意味を持つ)
  let objPaletteColor (paletteIndex: int) (colorIndex: int) (mem: MemoryBus) : uint16 =
    paletteColor mem.ObjPalette paletteIndex colorIndex

  /// 診断用: 読み出しアドレスを観測するフック (既定は無効)。
  /// 実 ROM が「どのレジスタを待っているか」を特定するために使う。テストや通常実行では触らない
  let mutable private readObserver: (uint16 -> unit) option = None
  let setReadObserver (f: (uint16 -> unit) option) = readObserver <- f

  /// 診断用: 書き込みアドレスと値を観測するフック (既定は無効)
  let mutable private writeObserver: (uint16 -> byte -> unit) option = None
  let setWriteObserver (f: (uint16 -> byte -> unit) option) = writeObserver <- f

  let read (addr: uint16) (mem: MemoryBus) : byte =
    (match readObserver with
     | Some f -> f addr
     | None -> ())
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
      // VRAM (CGB は VBK で 2 バンクを切り替える)
      readVram mem.VramBank (a - 0x8000) mem
    | _ when a < 0xC000 ->
      // External RAM / RTC (with MBC bank switching)
      let offset = a - 0xA000
      if mem.Mbc.MbcType = NoMbc then
        if mem.ExtRam.Length > 0 then mem.ExtRam.[offset &&& (mem.ExtRam.Length - 1)] else 0xFFuy
      else
        readExtRam offset mem
    | _ when a < 0xE000 ->
      // Work RAM (C000-CFFF はバンク 0 固定、D000-DFFF は WramBank)
      if a < 0xD000 then mem.Wram.[a - 0xC000]
      else mem.Wram.[wramBank mem * 0x1000 + (a - 0xD000)]
    | _ when a < 0xFE00 ->
      // Echo RAM (C000-DDFF のミラー)
      let ea = a - 0x2000
      if ea < 0xD000 then mem.Wram.[ea - 0xC000]
      else mem.Wram.[wramBank mem * 0x1000 + (ea - 0xD000)]
    | _ when a < 0xFEA0 ->
      // OAM
      mem.Oam.[a - 0xFE00]
    | _ when a < 0xFF00 ->
      // Not usable
      0xFFuy
    | _ when a < 0xFF80 ->
      // I/O
      // NR14/NR24/NR34/NR44 の bit7 (トリガ) は書き込み専用で、読み出しは常に 1。
      // Io 配列はトリガ要求 (pending) を保持するため、CPU からの読み出し時のみ 1 を立てる。
      let value = mem.Io.[a - 0xFF00]
      match a with
      | 0xFF14 | 0xFF19 | 0xFF1E | 0xFF23 -> value ||| 0x80uy
      // CGB 専用レジスタ。非 CGB モードでは 0xFF を読む (Pan Docs: CGB Registers)
      | 0xFF4C -> if mem.CgbMode then 0xFEuy else 0xFFuy                          // KEY0 (bit0 = DMG 互換モード = 0)
      | 0xFF4D -> if mem.CgbMode then 0x7Euy ||| (value &&& 0x81uy) else 0xFFuy   // KEY1 (bit7 = 倍速, bit0 = アーム)
      | 0xFF4F -> if mem.CgbMode then 0xFEuy ||| byte mem.VramBank else 0xFFuy    // VBK
      | 0xFF6C -> if mem.CgbMode then 0xFEuy ||| (value &&& 0x01uy) else 0xFFuy   // OPRI
      | 0xFF70 -> if mem.CgbMode then 0xF8uy ||| (value &&& 0x07uy) else 0xFFuy    // SVBK
      | 0xFF74 -> if mem.CgbMode then value else 0xFFuy                           // 未定義 (CGB モードでは読み書き可)
      | 0xFF75 -> (value &&& 0x70uy) ||| 0x8Fuy                                   // 未定義 (bit4-6 のみ読み書き可)
      // CGB のパレット RAM。非 CGB モードではレジスタ値 (起動値) をそのまま返す
      | 0xFF69 -> if mem.CgbMode then mem.BgPalette.[int (mem.Io.[0x68] &&& 0x3Fuy)] else value
      | 0xFF6B -> if mem.CgbMode then mem.ObjPalette.[int (mem.Io.[0x6A] &&& 0x3Fuy)] else value
      | _ -> value
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
    (match writeObserver with
     | Some f -> f addr value
     | None -> ())
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
      // VRAM (CGB は VBK で 2 バンクを切り替える)
      writeVram mem.VramBank (a - 0x8000) value mem
    | _ when a < 0xC000 ->
      // External RAM / RTC
      let offset = a - 0xA000
      if mem.Mbc.MbcType = NoMbc then
        if mem.ExtRam.Length > 0 then mem.ExtRam.[offset &&& (mem.ExtRam.Length - 1)] <- value
        mem
      else
        writeExtRam offset value mem
    | _ when a < 0xE000 ->
      // Work RAM (C000-CFFF はバンク 0 固定、D000-DFFF は WramBank)
      if a < 0xD000 then mem.Wram.[a - 0xC000] <- value
      else mem.Wram.[wramBank mem * 0x1000 + (a - 0xD000)] <- value
      mem
    | _ when a < 0xFE00 ->
      // Echo RAM (C000-DDFF のミラー)
      let ea = a - 0x2000
      if ea < 0xD000 then mem.Wram.[ea - 0xC000] <- value
      else mem.Wram.[wramBank mem * 0x1000 + (ea - 0xD000)] <- value
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
      match a with
      // CGB 専用レジスタは非 CGB モードでは書き込みを無視する (Pan Docs: CGB Registers)
      | 0xFF4C -> mem   // KEY0: ブート ROM 完了後にロックされるため書き込み不可
      | 0xFF4D ->       // KEY1: bit0 (速度切替のアーム) のみ書き込み可。bit7 は読み出し専用
        if mem.CgbMode then mem.Io.[0x4D] <- (mem.Io.[0x4D] &&& 0x80uy) ||| (value &&& 0x01uy)
        mem
      | 0xFF4F ->       // VBK
        if mem.CgbMode then { mem with VramBank = int value &&& 0x01 } else mem
      | 0xFF6C ->       // OPRI
        if mem.CgbMode then mem.Io.[0x6C] <- value &&& 0x01uy
        mem
      | 0xFF70 ->       // SVBK: 値そのものを保持する (0 は実効バンク 1、読み出しは 0xF8 ベース)
        if mem.CgbMode then mem.Io.[0x70] <- value &&& 0x07uy
        mem
      | 0xFF46 ->       // OAM DMA の要求 (転送は PPU が行う)
        mem.Io.[0x46] <- value
        { mem with DmaRequest = true }
      | 0xFF02 ->       // SC: bit7 = 転送開始。内部クロック (bit0=1) のみ完了まで進める
        mem.Io.[0x02] <- value
        if (value &&& 0x81uy) = 0x81uy then
          // 通常 8192 Hz → 512 サイクル/ビット = 4096 サイクル/バイト
          // bit1 = 1 の高速モードは 262144 Hz → 16 サイクル/ビット = 128 サイクル/バイト
          { mem with SerialCycles = (if (value &&& 0x02uy) <> 0uy then 128 else 4096) }
        else mem
      // CGB のパレット RAM。BCPS/OCPS (0xFF68/0xFF6A) が bit7 = 自動インクリメント、bit0-5 = インデックス。
      // 非 CGB モードでは従来どおりレジスタ値として保持する (起動値の検証を壊さないため)
      | 0xFF68 -> mem.Io.[0x68] <- value; mem
      | 0xFF69 ->
        if mem.CgbMode then
          let idx = int (mem.Io.[0x68] &&& 0x3Fuy)
          mem.BgPalette.[idx] <- value
          if (mem.Io.[0x68] &&& 0x80uy) <> 0uy then
            mem.Io.[0x68] <- (mem.Io.[0x68] &&& 0x80uy) ||| byte ((idx + 1) &&& 0x3F)
        else mem.Io.[0x69] <- value
        mem
      | 0xFF6A -> mem.Io.[0x6A] <- value; mem
      | 0xFF6B ->
        if mem.CgbMode then
          let idx = int (mem.Io.[0x6A] &&& 0x3Fuy)
          mem.ObjPalette.[idx] <- value
          if (mem.Io.[0x6A] &&& 0x80uy) <> 0uy then
            mem.Io.[0x6A] <- (mem.Io.[0x6A] &&& 0x80uy) ||| byte ((idx + 1) &&& 0x3F)
        else mem.Io.[0x6B] <- value
        mem
      | 0xFF74 ->       // 未定義 (CGB モードでは読み書き可)
        if mem.CgbMode then mem.Io.[0x74] <- value
        mem
      | 0xFF75 ->       // 未定義 (bit4-6 のみ読み書き可)
        mem.Io.[0x75] <- (mem.Io.[0x75] &&& 0x8Fuy) ||| (value &&& 0x70uy)
        mem
      | _ ->
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

  /// シリアル転送を cycles 分進める。完了したら「相手なし」として
  /// SB = 0xFF / SC bit7 = 0 / シリアル割込み (IF bit3) を要求する。
  /// 外部クロック (SC bit0 = 0) の転送は相手が必要なので開始しない (SerialCycles は 0 のまま)
  let tickSerial (cycles: int) (mem: MemoryBus) : MemoryBus =
    if mem.SerialCycles = 0 then mem
    else
      let remaining = mem.SerialCycles - cycles
      if remaining > 0 then { mem with SerialCycles = remaining }
      else
        let m = { mem with SerialCycles = 0 }
        m.Io.[0x01] <- 0xFFuy                        // SB: 受信バイト (相手なし)
        m.Io.[0x02] <- m.Io.[0x02] &&& 0x7Fuy        // SC bit7 クリア (転送完了)
        requestInterrupt SerialInterrupt m
