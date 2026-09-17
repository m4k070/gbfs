namespace gbfs.Lib

open Decoder

/// WireLevel cellular-automaton SM83 CPU bridge.
///
/// ## Background
///
/// WireLevel (../wwc, DESIGN-CA2.md) is a custom CA designed for digital logic
/// compilation. Unlike classic WireWorld (pulse-based, Moore neighborhood),
/// WireLevel is **level-driven, von Neumann neighborhood, ≤64 states** with
/// dedicated gate cells:
///
/// | Cell | Function |
/// |------|----------|
/// | `LPin v` | Host-writable pin, presents level `v` to all neighbors |
/// | `LWire(dir,v)` | Directional wire: reads from back, presents forward |
/// | `LNand(dir,v)` | NAND gate: `v' = NOT(AND(all non-output neighbors))` |
/// | `Cross(hd,vd,hv,vv)` | Independent 2-channel crossing (eliminates routing congestion) |
/// | `LDff(dir,q,prevClk)` | Edge-triggered DFF: `clk ∧ ¬prevClk ⇒ q := D` |
///
/// The SM83 CPU (../wwc) is compiled from Verilog through yosys into WireLevel
/// grids, then executed on GPU via WebGPU compute shaders.
///
/// ## Architecture
///
/// This bridge adapts the WireLevel CPU to the gbfs `CpuCore` interface,
/// enabling it as a drop-in replacement for the native F# CPU.
///
/// ```
/// ┌──────────────┐     pin r/w      ┌─────────────────┐
/// │  Emulator.fs │ ←─────────────→ │ WireLevelCpu     │
/// │  (host CPU)  │   CpuState       │ (GPU CA grid)    │
/// └──────────────┘                  └─────────────────┘
/// ```
///
/// ## Integration Plan (../wwc DESIGN-CA2.md §5.6)
///
/// 1. **Bus Bridge**: WireLevel CPU has `LPin` cells for addr/data/control.
///    Bridge translates pin states ↔ `Memory.read`/`write` on `MemoryBus`.
///    - CPU read cycle:  set data bus pins as input, run to settle, read addr pins → Memory.read → write data pins, run to settle, read data bus
///    - CPU write cycle: set addr + data pins, run to settle, read control pins → Memory.write
///
/// 2. **Interrupt Bridge**: GB peripherals trigger interrupts via IF/IE registers.
///    Bridge writes interrupt request bits to the CPU's interrupt input `LPin` cells.
///
/// 3. **Timing**: WireLevel is level-driven (settles naturally like real hardware).
///    No strict STA needed — just wait for the grid to reach a fixed point after
///    each pin change. Clock is driven by toggling the CLK `LPin`.
///
/// 4. **State Sync**: After each instruction, read register output `LPin` cells
///    and write values into `CpuState.Regs`.
///
/// ## Current Status
///
/// The WireLevel SM83 CPU exists in two variants:
/// - `sm83_min` (380 gates, 20 instructions, GPU-verified 20/20) — register-only, no memory bus
/// - `sm83_subset` (3553 gates, memory bus) — target for integration, compileWL pending
///
/// The WebGPU runner (`../wwc/wgpu-runner/`) already supports program-driven
/// execution with pin I/O and register dump. The bus bridge is the remaining piece.
///
/// This stub delegates to the native Decoder CPU. Replace `Step`/`CreateState`/`LoadRom`
/// with actual WireLevel grid execution once the bus/interrupt bridge is implemented.
module WireLevelCpu =

    let core: CpuCore = {
        Step = fun state ->
            // TODO(wirelevel): Replace with WireLevel grid step via WebGPU bridge.
            //
            // Per-cycle sequence (level-driven — no per-generation stepping needed):
            //   1. Write CpuState → input LPin cells (data_bus_in, intr_lines, reset)
            //   2. Toggle CLK LPin: 0→1, wait for grid settle (fixed-point detection)
            //   3. Toggle CLK LPin: 1→0, wait for grid settle
            //   4. Read output LPin cells (addr_bus, data_bus_out, rd, wr, reg_values)
            //   5. Translate to CpuState mutations:
            //      - If rd=1: Memory.read(addr_bus) → write to data_bus_in LPin, re-settle, read data_bus_out
            //      - If wr=1: Memory.write(addr_bus, data_bus_out)
            //      - Update CpuState.Regs from reg_value LPins
            //      - Update IME/Halted from control LPins
            //
            // The ../wwc/wgpu-runner already handles grid I/O and fixed-point detection.
            // The bridge adds GB memory bus mapping + interrupt line driving.
            Decoder.step state

        CreateState = fun () ->
            // TODO(wirelevel): Initialize WireLevel grid from sm83_subset .bin.
            // Load the pre-compiled grid (../wwc/web/sm83_mc_*.bin),
            // set initial LPin values (PC=0x0100, SP=0xFFFE, etc.),
            // and run initial settle.
            Decoder.createState()

        LoadRom = fun rom state ->
            // TODO(wirelevel): After loading ROM into MemoryBus,
            // the WireLevel CPU's PC will start fetching from 0x0100.
            // No special grid initialization needed beyond CreateState.
            Decoder.loadRomToState rom state
    }
