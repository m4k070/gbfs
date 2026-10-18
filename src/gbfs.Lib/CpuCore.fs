namespace gbfs.Lib

open Decoder

/// Serializable CPU kind tag.
/// Survives Blazor SSR prerendering → client rehydration.
type CpuKind =
    | Native
    | WireLevel

/// Swappable CPU implementation (record of functions).
/// NOT serializable — use `CpuKind` for state transfer, then resolve with `cpuImplOf`.
type CpuCore = {
    Step: Decoder.CpuState -> Decoder.CpuState
    /// 1 ステップ実行し、消費した T サイクルも返す。
    /// フレーム境界は「70,224 T サイクル」で数えるので、runFrame はこちらを使う
    /// (ステップ数で数えると、命令の長さが 4〜24 サイクルなので実時間とずれる)。
    StepCycles: Decoder.CpuState -> Decoder.CpuState * int
    CreateState: unit -> Decoder.CpuState
    LoadRom: byte array -> Decoder.CpuState -> Decoder.CpuState
}

