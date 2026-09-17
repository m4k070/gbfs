// Same test but with WireLevel CPU
open gbfs.Lib

let rom = System.IO.File.ReadAllBytes("test.gb")
printfn "ROM loaded: %d bytes" rom.Length
let wl0 = Emulator.createWith WireLevel
let wlState = Emulator.loadRom rom wl0

printfn "Initial PC: 0x%04X" wlState.Cpu.Regs.PC

let mutable s = wlState
for i in 1..12 do
    s <- Emulator.step s
    let a = Cpu.getRegisterValue (Cpu.R8 Cpu.A) s.Cpu.Regs
    let b = Cpu.getRegisterValue (Cpu.R8 Cpu.B) s.Cpu.Regs
    let c = Cpu.getRegisterValue (Cpu.R8 Cpu.C) s.Cpu.Regs
    printfn "Step %2d: PC=0x%04X A=0x%02X B=0x%02X C=0x%02X halted=%b"
        i s.Cpu.Regs.PC a b c s.Cpu.Halted

let a = Cpu.getRegisterValue (Cpu.R8 Cpu.A) s.Cpu.Regs
let b = Cpu.getRegisterValue (Cpu.R8 Cpu.B) s.Cpu.Regs
let c = Cpu.getRegisterValue (Cpu.R8 Cpu.C) s.Cpu.Regs

printfn ""
printfn "=== WireLevel CPU Results ==="
printfn "A = 0x%02X (expected 0x55) %s" a (if a = 0x55us then "OK" else "FAIL")
printfn "B = 0x%02X (expected 0x13) %s" b (if b = 0x13us then "OK" else "FAIL")
printfn "C = 0x%02X (expected 0x55) %s" c (if c = 0x55us then "OK" else "FAIL")
printfn "Halted = %b" s.Cpu.Halted
