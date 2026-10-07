// 実 ROM / テスト ROM がどこで止まっているかを調べる診断ツール
//
// usage: dotnet fsi tools/diag_rom.fsx -- <rom.gb> [--machine dmg|cgb] [--frames 120]
//
// 出力: 実行後のレジスタ、主要 I/O、VRAM/OAM の非ゼロ量、直近の PC 列 (ループ検出)、シリアル出力
#r "/home/makoto/work/gbfs-bootregs/src/gbfs.Lib/bin/Release/net8.0/gbfs.Lib.dll"

open System
open System.IO
open gbfs.Lib

let mutable romPath = ""
let mutable machine = Memory.Dmg
let mutable frames = 120

let args = fsi.CommandLineArgs |> Array.skip 1
let mutable i = 0
while i < args.Length do
    match args.[i] with
    | "--machine" -> machine <- (if args.[i + 1] = "cgb" then Memory.Cgb else Memory.Dmg); i <- i + 2
    | "--frames" -> frames <- int args.[i + 1]; i <- i + 2
    | a -> romPath <- a; i <- i + 1

if romPath = "" then
    eprintfn "usage: dotnet fsi tools/diag_rom.fsx -- <rom.gb> [--machine dmg|cgb] [--frames N]"
    exit 2

// シリアル出力を集めながら 1 フレームずつ進める
let rom = File.ReadAllBytes romPath
let mutable st =
    Emulator.createWithMachine machine
    |> fun s -> Emulator.loadRom rom s

let serial = Text.StringBuilder()
let mutable lastPCs = List.empty

let mutable f = 0
while f < frames do
    let oldLY = st.Cpu.Ppu.LY
    // 1 フレーム分 (LY が 0 に戻るまで) 命令を進める
    let mutable steps = 0
    let mutable done' = false
    while steps < 70224 && not done' do
        lastPCs <- st.Cpu.Regs.PC :: (if lastPCs.Length > 200 then List.take 200 lastPCs else lastPCs)
        st <- Emulator.step st
        // シリアル (SC の bit7 が立ったら 1 バイト送信とみなす)
        let sc = st.Cpu.Mem.Io.[0x02]
        if (sc &&& 0x81uy) = 0x81uy then
            serial.Append(char st.Cpu.Mem.Io.[0x01]) |> ignore
            st.Cpu.Mem.Io.[0x01] <- 0xFFuy
            st.Cpu.Mem.Io.[0x02] <- 0x7Euy
        steps <- steps + 1
        if oldLY > 0uy && st.Cpu.Ppu.LY = 0uy then done' <- true
    f <- f + 1

let mem = st.Cpu.Mem
let io = mem.Io
let read a = int (Memory.read (uint16 a) mem)
let regs = st.Cpu.Regs

printfn "rom        = %s" (Path.GetFileName romPath)
printfn "machine    = %s / CGB mode = %b / mapper = %s"
    (Memory.machineName machine) mem.CgbMode (Emulator.getMapperName st)
printfn "PC=%04X SP=%04X AF=%04X BC=%04X DE=%04X HL=%04X IME=%b Halted=%b Stopped=%d"
    regs.PC regs.SP regs.AF regs.BC regs.DE regs.HL st.Cpu.Ime st.Cpu.Halted st.Cpu.Stopped
printfn "LCDC=%02X STAT=%02X LY=%02X LYC=%02X SCY=%02X SCX=%02X BGP=%02X OBP0=%02X OBP1=%02X WY=%02X WX=%02X"
    (read 0xFF40) (read 0xFF41) (read 0xFF44) (read 0xFF45) (read 0xFF42) (read 0xFF43)
    (read 0xFF47) (read 0xFF48) (read 0xFF49) (read 0xFF4A) (read 0xFF4B)
printfn "KEY0=%02X KEY1=%02X VBK=%02X SVBK=%02X HDMA1-5=%02X %02X %02X %02X %02X OPRI=%02X IE=%02X IF=%02X"
    (read 0xFF4C) (read 0xFF4D) (read 0xFF4F) (read 0xFF70)
    (read 0xFF51) (read 0xFF52) (read 0xFF53) (read 0xFF54) (read 0xFF55) (read 0xFF6C) (read 0xFFFF) (read 0xFF0F)

let nonzero (from: int) (len: int) =
    let mutable c = 0
    for a in from .. from + len - 1 do
        if Memory.read (uint16 a) mem <> 0uy then c <- c + 1
    c
let nonzeroBank (bank: int) =
    let mutable c = 0
    for off in 0 .. 0x1FFF do
        if Memory.readVram bank off mem <> 0uy then c <- c + 1
    c
printfn "VRAM bank0 nonzero=%d/8192  bank1 nonzero=%d/8192  OAM nonzero=%d/160  WRAM nonzero=%d/8192"
    (nonzeroBank 0) (nonzeroBank 1) (nonzero 0xFE00 0xA0) (nonzero 0xC000 0x2000)

let fb = Emulator.getFrameBuffer st
printfn "framebuffer= %A" (fb |> Array.countBy id |> Array.sortBy fst)

// 直近 60 PC が何種類か (ループ検出) と、最後の 30 PC
let recent = lastPCs |> List.truncate 60
printfn "recent unique PCs = %d / 60" (recent |> List.distinct |> List.length)
printfn "last 30 PCs = %s" (lastPCs |> List.truncate 30 |> List.rev |> List.map (fun p -> sprintf "%04X" p) |> String.concat " ")
let s = serial.ToString()
if s.Length > 0 then printfn "serial = %s" (s.Replace("\n", "\\n").Substring(0, min 300 s.Length))
