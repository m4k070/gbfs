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

  type MemoryBus = {
    Rom: byte array        // ROM (up to 32KB for simple cartridges)
    Vram: byte array       // 8KB Video RAM
    ExtRam: byte array     // 8KB External RAM
    Wram: byte array       // 8KB Work RAM
    Oam: byte array        // 160 bytes OAM
    Io: byte array         // 128 bytes I/O
    Hram: byte array       // 127 bytes High RAM
    Ie: byte               // Interrupt Enable register
  }

  let create () = {
    Rom = Array.zeroCreate 0x8000      // 32KB ROM
    Vram = Array.zeroCreate 0x2000     // 8KB VRAM
    ExtRam = Array.zeroCreate 0x2000   // 8KB External RAM
    Wram = Array.zeroCreate 0x2000     // 8KB Work RAM
    Oam = Array.zeroCreate 0xA0        // 160 bytes OAM
    Io = Array.zeroCreate 0x80         // 128 bytes I/O
    Hram = Array.zeroCreate 0x7F       // 127 bytes HRAM
    Ie = 0uy
  }

  let loadRom (rom: byte array) (mem: MemoryBus) =
    let len = min rom.Length mem.Rom.Length
    Array.blit rom 0 mem.Rom 0 len
    mem

  let read (addr: uint16) (mem: MemoryBus) : byte =
    let a = int addr
    match a with
    | _ when a < 0x8000 ->
      // ROM
      if a < mem.Rom.Length then mem.Rom.[a] else 0uy
    | _ when a < 0xA000 ->
      // VRAM
      mem.Vram.[a - 0x8000]
    | _ when a < 0xC000 ->
      // External RAM
      mem.ExtRam.[a - 0xA000]
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
    | _ when a < 0x8000 ->
      // ROM - ignore writes (MBC handling would go here)
      mem
    | _ when a < 0xA000 ->
      // VRAM
      mem.Vram.[a - 0x8000] <- value
      mem
    | _ when a < 0xC000 ->
      // External RAM
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
