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

  type MbcType =
    | NoMbc
    | Mbc1

  type MbcState = {
    MbcType: MbcType
    RomBank: int           // Current ROM bank (1-127 for MBC1)
    RamBank: int           // Current RAM bank (0-3 for MBC1)
    RamEnabled: bool       // RAM enable flag
    BankingMode: int       // 0=ROM mode, 1=RAM mode (MBC1)
  }

  type MemoryBus = {
    Rom: byte array        // Full ROM data
    Vram: byte array       // 8KB Video RAM
    ExtRam: byte array     // External RAM (up to 32KB for MBC1)
    Wram: byte array       // 8KB Work RAM
    Oam: byte array        // 160 bytes OAM
    Io: byte array         // 128 bytes I/O
    Hram: byte array       // 127 bytes High RAM
    Ie: byte               // Interrupt Enable register
    Mbc: MbcState          // MBC state
  }

  let private detectMbcType (rom: byte array) =
    if rom.Length > 0x0147 then
      match rom.[0x0147] with
      | 0x01uy | 0x02uy | 0x03uy -> Mbc1
      | _ -> NoMbc
    else NoMbc

  let private getRamSize (rom: byte array) =
    if rom.Length > 0x0149 then
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
    Hram = Array.zeroCreate 0x7F       // 127 bytes HRAM
    Ie = 0uy
    Mbc = { MbcType = NoMbc; RomBank = 1; RamBank = 0; RamEnabled = false; BankingMode = 0 }
  }

  let loadRom (rom: byte array) (mem: MemoryBus) =
    let mbcType = detectMbcType rom
    let ramSize = getRamSize rom
    { mem with
        Rom = Array.copy rom
        ExtRam = Array.zeroCreate ramSize
        Mbc = { mem.Mbc with MbcType = mbcType } }

  let read (addr: uint16) (mem: MemoryBus) : byte =
    let a = int addr
    match a with
    | _ when a < 0x4000 ->
      // ROM Bank 0 (MBC1 mode 1: bank 0 can be remapped via upper bits)
      match mem.Mbc.MbcType with
      | Mbc1 when mem.Mbc.BankingMode = 1 ->
        let bank0 = (mem.Mbc.RamBank <<< 5) % (mem.Rom.Length / 0x4000)
        let romAddr = bank0 * 0x4000 + a
        if romAddr < mem.Rom.Length then mem.Rom.[romAddr] else 0uy
      | _ ->
        if a < mem.Rom.Length then mem.Rom.[a] else 0uy
    | _ when a < 0x8000 ->
      // ROM Bank 1-N (switchable)
      match mem.Mbc.MbcType with
      | Mbc1 ->
        let fullBank = (mem.Mbc.RamBank <<< 5) ||| mem.Mbc.RomBank
        let bank = fullBank % (max 1 (mem.Rom.Length / 0x4000))
        let romAddr = bank * 0x4000 + (a - 0x4000)
        if romAddr < mem.Rom.Length then mem.Rom.[romAddr] else 0uy
      | NoMbc ->
        if a < mem.Rom.Length then mem.Rom.[a] else 0uy
    | _ when a < 0xA000 ->
      // VRAM
      mem.Vram.[a - 0x8000]
    | _ when a < 0xC000 ->
      // External RAM (with MBC bank switching)
      match mem.Mbc.MbcType with
      | Mbc1 when mem.Mbc.RamEnabled ->
        let ramBank = if mem.Mbc.BankingMode = 1 then mem.Mbc.RamBank else 0
        let ramAddr = ramBank * 0x2000 + (a - 0xA000)
        if ramAddr < mem.ExtRam.Length then mem.ExtRam.[ramAddr] else 0xFFuy
      | Mbc1 -> 0xFFuy // RAM disabled
      | NoMbc -> mem.ExtRam.[a - 0xA000]
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

  let write (addr: uint16) (value: byte) (mem: MemoryBus) : MemoryBus =
    let a = int addr
    match a with
    | _ when a < 0x2000 ->
      // MBC1: RAM Enable (0x0000-0x1FFF)
      match mem.Mbc.MbcType with
      | Mbc1 ->
        let enabled = (value &&& 0x0Fuy) = 0x0Auy
        { mem with Mbc = { mem.Mbc with RamEnabled = enabled } }
      | NoMbc -> mem
    | _ when a < 0x4000 ->
      // MBC1: ROM Bank Number lower 5 bits (0x2000-0x3FFF)
      match mem.Mbc.MbcType with
      | Mbc1 ->
        let bank = int (value &&& 0x1Fuy)
        let bank = if bank = 0 then 1 else bank // Bank 0 maps to 1
        { mem with Mbc = { mem.Mbc with RomBank = bank } }
      | NoMbc -> mem
    | _ when a < 0x6000 ->
      // MBC1: RAM Bank Number / Upper ROM Bank bits (0x4000-0x5FFF)
      match mem.Mbc.MbcType with
      | Mbc1 ->
        let bankBits = int (value &&& 0x03uy)
        { mem with Mbc = { mem.Mbc with RamBank = bankBits } }
      | NoMbc -> mem
    | _ when a < 0x8000 ->
      // MBC1: Banking Mode Select (0x6000-0x7FFF)
      match mem.Mbc.MbcType with
      | Mbc1 ->
        let mode = int (value &&& 0x01uy)
        { mem with Mbc = { mem.Mbc with BankingMode = mode } }
      | NoMbc -> mem
    | _ when a < 0xA000 ->
      // VRAM
      mem.Vram.[a - 0x8000] <- value
      mem
    | _ when a < 0xC000 ->
      // External RAM (with MBC bank switching)
      match mem.Mbc.MbcType with
      | Mbc1 when mem.Mbc.RamEnabled ->
        let ramBank = if mem.Mbc.BankingMode = 1 then mem.Mbc.RamBank else 0
        let ramAddr = ramBank * 0x2000 + (a - 0xA000)
        if ramAddr < mem.ExtRam.Length then
          mem.ExtRam.[ramAddr] <- value
        mem
      | Mbc1 -> mem // RAM disabled, ignore
      | NoMbc ->
        mem.ExtRam.[a - 0xA000] <- value
        mem
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

