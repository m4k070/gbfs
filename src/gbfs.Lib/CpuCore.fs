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
    CreateState: unit -> Decoder.CpuState
    LoadRom: byte array -> Decoder.CpuState -> Decoder.CpuState
}

