namespace gbfs.Lib

open System.Text.Json
open System.Text.Json.Serialization

/// エミュレータ状態の保存/復元 (開発・検証用)。
///
/// - 形式は System.Text.Json の JSON。
/// - ROM のバイト列は保存しない。状態は ROM 固有なので、復元は「同じ ROM をロード済みの
///   state」に対して行い、ROM はそこから引き継ぐ (2MB を毎回書かないため)。
/// - System.Text.Json は F# の判別共用体をそのまま扱えないため、状態に現れる DU の
///   コンバータをここで登録する。
module State =

  type private MachineModeConverter() =
    inherit JsonConverter<Memory.MachineMode>()
    override _.Read(reader, _, _) =
      match reader.GetString() with
      | "Cgb" -> Memory.Cgb
      | _ -> Memory.Dmg
    override _.Write(writer, value, _) =
      writer.WriteStringValue(match value with Memory.Cgb -> "Cgb" | Memory.Dmg -> "Dmg")

  type private CpuKindConverter() =
    inherit JsonConverter<CpuKind>()
    override _.Read(reader, _, _) =
      match reader.GetString() with
      | "WireLevel" -> CpuKind.WireLevel
      | _ -> CpuKind.Native
    override _.Write(writer, value, _) =
      writer.WriteStringValue(
        match value with
        | CpuKind.WireLevel -> "WireLevel"
        | CpuKind.Native -> "Native")

  type private PpuModeConverter() =
    inherit JsonConverter<Ppu.PpuMode>()
    override _.Read(reader, _, _) =
      match reader.GetString() with
      | "VBlank" -> Ppu.VBlank
      | "OamScan" -> Ppu.OamScan
      | "Drawing" -> Ppu.Drawing
      | _ -> Ppu.HBlank
    override _.Write(writer, value, _) =
      writer.WriteStringValue(
        match value with
        | Ppu.VBlank -> "VBlank"
        | Ppu.OamScan -> "OamScan"
        | Ppu.Drawing -> "Drawing"
        | Ppu.HBlank -> "HBlank")

  type private MbcTypeConverter() =
    inherit JsonConverter<Memory.MbcType>()
    override _.Read(reader, _, _) =
      match reader.GetString() with
      | "Mbc1" -> Memory.Mbc1
      | "Mbc2" -> Memory.Mbc2
      | "Mbc3" -> Memory.Mbc3
      | "Mbc5" -> Memory.Mbc5
      | _ -> Memory.NoMbc
    override _.Write(writer, value, _) =
      writer.WriteStringValue(
        match value with
        | Memory.Mbc1 -> "Mbc1"
        | Memory.Mbc2 -> "Mbc2"
        | Memory.Mbc3 -> "Mbc3"
        | Memory.Mbc5 -> "Mbc5"
        | Memory.NoMbc -> "NoMbc")

  let private options =
    let o = JsonSerializerOptions()
    o.IncludeFields <- true
    o.Converters.Add(MachineModeConverter())
    o.Converters.Add(CpuKindConverter())
    o.Converters.Add(PpuModeConverter())
    o.Converters.Add(MbcTypeConverter())
    o

  /// ROM のバイト列は状態に含めない
  let private stripRom (state: Emulator.EmulatorState) =
    { state with Cpu = { state.Cpu with Mem = { state.Cpu.Mem with Rom = [||] } } }

  let private attachRom (rom: byte array) (state: Emulator.EmulatorState) =
    { state with Cpu = { state.Cpu with Mem = { state.Cpu.Mem with Rom = rom } } }

  let serialize (state: Emulator.EmulatorState) : string =
    JsonSerializer.Serialize(stripRom state, options)

  /// current は ROM をロード済みの state。ROM は current のものを引き継ぐ。
  let deserialize (json: string) (current: Emulator.EmulatorState) : Emulator.EmulatorState =
    attachRom current.Cpu.Mem.Rom (JsonSerializer.Deserialize<Emulator.EmulatorState>(json, options))

  /// 状態をファイルに書き出し、書いた JSON の長さを返す
  let saveTo (path: string) (state: Emulator.EmulatorState) : int =
    let json = serialize state
    System.IO.File.WriteAllText(path, json)
    json.Length

  /// ファイルから状態を復元する。current は ROM をロード済みの state
  let loadFrom (path: string) (current: Emulator.EmulatorState) : Emulator.EmulatorState =
    deserialize (System.IO.File.ReadAllText path) current
