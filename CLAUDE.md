# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

GameBoy emulator written in F# with a Bolero-based web interface. Four projects in `src/`:

- **gbfs.Lib** - Pure F# CPU emulation library (no dependencies)
- **gbfs.Client** - Blazor WebAssembly UI using Bolero framework
- **gbfs.Server** - ASP.NET Core backend hosting the client
- **gbfs.McpServer** - MCP Server for AI-driven game play (stdio transport)

## Build Commands

```bash
# Build all projects
dotnet build

# Run tests (218 tests, xUnit)
dotnet test

# Run web server (auto-builds client, starts at http://localhost:5000)
dotnet run --project src/gbfs.Server

# Run MCP server (stdio transport)
dotnet run --project src/gbfs.McpServer
```

Development environment uses Nix flakes with `dotnet-sdk_8`. Use `direnv allow` to activate.

## Architecture

```
┌─────────────┐    ┌──────────────┐
│  Web UI     │    │  MCP Server  │
│  (Bolero)   │    │  (stdio)     │
└──────┬──────┘    └──────┬───────┘
       │                  │
       └────────┬─────────┘
                │
       ┌────────▼────────┐
       │   Emulator.fs   │  ← 共通インターフェース
       │   高レベル API  │
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
9. `Emulator.fs` - High-level API (create/loadRom/runFrame/pressButton/getScreen...)

Key types:
- `Reg8` / `Reg16` - Register enums
- `Register` - CPU register file record
- `Decoder.CpuState` - Full emulator state (Regs, Mem, Ppu, Apu, Joypad, Timer, IME, Halted)
- `Emulator.EmulatorState` - Wraps CpuState with FrameCount and TotalCycles

### Web UI (gbfs.Client + gbfs.Server)

Uses Bolero framework (F# wrapper for Blazor) with Elmish MVU pattern:
- `Main.fs` - EmulatorApp component, keyboard input, audio output via JS interop
- `app.js` - Canvas rendering, ROM loading, AudioContext playback
- Keyboard: Arrow keys = D-Pad, Z = A, X = B, Enter = Start, Shift = Select

### MCP Server (gbfs.McpServer)

AI-driven game play via Model Context Protocol:
- `Tools.fs` - 7 tools: load_rom, reset, run_frames, press_buttons, get_screen, get_state, read_memory
- `Program.fs` - stdio transport entry point
- Uses ModelContextProtocol NuGet package (official C# SDK)
- Config: `.mcp.json` in project root

## Adding New Instructions

1. Add execution function in `Cpu.fs` following existing patterns (e.g., `AddR`, `SubR`)
2. Add active pattern in `Decoder.fs` for opcode matching
3. Add case to `executeInstruction` match expression in `Decoder.fs`
4. Add tests in `tests/gbfs.Tests/`

## Adding New Emulator Features

1. Implement in appropriate module (Ppu.fs, Apu.fs, etc.)
2. Wire into `Decoder.step` (4 call sites: executeInstruction, HALT wake+interrupt, HALT continue, normal interrupt)
3. Expose via `Emulator.fs` high-level API
4. Update Web UI (`Main.fs`) and/or MCP Server (`Tools.fs`) as needed
