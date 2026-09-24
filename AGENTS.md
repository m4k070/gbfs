# AGENTS.md

This file provides guidance to AI coding agents (originally written for Claude Code) when working with code in this repository.

## Project Overview

GameBoy emulator written in F#. Four projects in `src/`:

- **gbfs.Lib** - Pure F# emulator library (no dependencies) — all UIs consume this
- **gbfs.Desktop** - Avalonia desktop UI (framebuffer via WriteableBitmap, OpenAL audio)
- **gbfs.Server** - ASP.NET Core host for the MCP frame relay only (no UI)
- **gbfs.McpServer** - MCP Server for AI-driven game play (stdio transport)

Web UI (Bolero) was removed; `gbfs.Desktop` is the interactive UI. A future web UI
should be added as a new API contract on `gbfs.Server`, not by reviving Bolero —
the contract is specified in `docs/web-api.md`.

## Build Commands

```bash
# Build all projects
dotnet build

# Run tests (224 tests, xUnit)
dotnet test

# Run desktop UI (native deps come from the nix devShell)
nix develop -c dotnet run --project src/gbfs.Desktop -- [path/to/rom.gb]
# Without a ROM argument it loads ./test.gb; Start button begins execution
# (no auto-start: press Start to run/produce audio)

# Run MCP frame relay host (http://localhost:5032)
dotnet run --project src/gbfs.Server

# Run MCP server (stdio transport)
dotnet run --project src/gbfs.McpServer
```

Development environment uses Nix flakes with `dotnet-sdk_8`. Use `direnv allow` to activate.
The devShell also carries native runtime deps for Avalonia/OpenAL (fontconfig, libxkbcommon,
libGL, X11 libs, openal) exported via `LD_LIBRARY_PATH`.

## Architecture

```
┌──────────────┐   ┌──────────────┐   ┌──────────────────┐
│ gbfs.Desktop │   │ McpServer    │   │ gbfs.Server      │
│ (Avalonia)   │   │ (stdio MCP)  │   │ (frame relay)    │
│ 画面+音+入力  │   │ AI 操作      │   │ POST/GET /frame  │
└──────┬───────┘   └──────┬───────┘   └────────┬─────────┘
       │ direct ref       │ POST frame         │ (バイト列のみ、Lib 非依存)
       └────────┬─────────┘                    │
                │                              │
       ┌────────▼────────┐                     │
       │   gbfs.Lib      │◄────────────────────┘
       │ Emulator.fs     │
       │ 高レベル API     │
       └────────┬────────┘
                │
       ┌────────▼────────┐
       │  Decoder / CPU  │
       │  PPU / APU ...  │
       └─────────────────┘
```

### Emulator Core (gbfs.Lib)

Files ordered by compilation in `.fsproj`:
1. `Memory.fs` - Memory bus, MBC1 bank switching
2. `Ppu.fs` - Pixel Processing Unit (BG/Window/Sprite, 4 modes)
3. `Apu.fs` - Audio Processing Unit (4 channels, 44100Hz sampling)
4. `Joypad.fs` - Joypad input (8 buttons, interrupt)
5. `Timer.fs` - Timer/DIV registers (falling-edge detection)
6. `Cpu.fs` - Register types and ALU operations
7. `CBPrefix.fs` - CB-prefixed instructions (all 256)
8. `Decoder.fs` - Opcode decode loop, CpuState, interrupt handling
9. `CpuCore.fs` / `WireLevelCpu.fs` - Swappable CPU implementations (Native / WireLevel stub)
10. `Emulator.fs` - High-level API (create/loadRom/runFrame/pressButton/getScreen...)

Key types:
- `Reg8` / `Reg16` - Register enums
- `Register` - CPU register file record
- `Decoder.CpuState` - Full emulator state (Regs, Mem, Ppu, Apu, Joypad, Timer, IME, Halted)
- `Emulator.EmulatorState` - Wraps CpuState with FrameCount and TotalCycles

### Desktop UI (gbfs.Desktop)

Single-file Avalonia app (`Program.fs`):
- `AudioOut` - OpenAL output: Lib APU samples (44100Hz float) → int16 → queued buffers
- MVU: `Model` / `Msg` / `update audio msg model` (side effects injected as arguments)
- 60fps via `DispatcherTimer`; framebuffer → `WriteableBitmap` (nearest-neighbor 3x)
- Keyboard: Arrow keys = D-Pad, Z = A, X = B, Enter = Start, Shift = Select, Space = Start/Stop toggle
- Test ROMs: `test.gb` (CPU register test, no display), `test-draw.gb` (stripe pattern),
  `test-tone.gb` (256Hz square tone — audio begins only after Start)

### MCP Server (gbfs.McpServer)

AI-driven game play via Model Context Protocol:
- `Tools.fs` - 7 tools: load_rom, reset, run_frames, press_buttons, get_screen, get_state, read_memory
- `FrameRelay.postFrame` - POSTs the framebuffer to gbfs.Server `/api/mcp/frame` (best-effort)
- `Program.fs` - stdio transport entry point
- Uses ModelContextProtocol NuGet package (official C# SDK)
- Config: `.mcp.json` in project root

### MCP frame relay (gbfs.Server)

Minimal Kestrel host: `POST/GET /api/mcp/frame` with an in-memory 160x144 buffer.
No UI, no Razor, no static assets. Port 5032 via `Properties/launchSettings.json`.

## Adding New Instructions

1. Add execution function in `Cpu.fs` following existing patterns (e.g., `AddR`, `SubR`)
2. Add active pattern in `Decoder.fs` for opcode matching
3. Add case to `executeInstruction` match expression in `Decoder.fs`
4. Add tests in `tests/gbfs.Tests/`

## Adding New Emulator Features

1. Implement in appropriate module (Ppu.fs, Apu.fs, etc.)
2. Wire into `Decoder.step` (4 call sites: executeInstruction, HALT wake+interrupt, HALT continue, normal interrupt)
3. Expose via `Emulator.fs` high-level API
4. Update Desktop UI (`gbfs.Desktop/Program.fs`) and/or MCP Server (`Tools.fs`) as needed
