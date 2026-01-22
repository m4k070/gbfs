namespace gbfs.Tests

open Xunit
open gbfs.Lib

module PpuTests =

    // Helper to create a new MemoryBus for testing
    let createMemoryBus () = Memory.create ()

    // Test for PPU mode transitions and LY register updates
    [<Fact>]
    let ``PPU mode transitions correctly`` () =
        let mutable ppuState = Ppu.create ()
        let mutable mem = createMemoryBus ()

        // Initial state: OamScan (Mode 2)
        Assert.Equal(Ppu.OamScan, ppuState.Mode)
        Assert.Equal(0uy, ppuState.LY)

        // Step through OamScan (80 cycles) -> Drawing
        (ppuState, mem) <- Ppu.step 80 ppuState mem
        Assert.Equal(Ppu.Drawing, ppuState.Mode)
        Assert.Equal(0, ppuState.Cycles) // Cycles reset
        Assert.Equal(0uy, ppuState.LY)

        // Step through Drawing (172 cycles) -> HBlank
        (ppuState, mem) <- Ppu.step 172 ppuState mem
        Assert.Equal(Ppu.HBlank, ppuState.Mode)
        Assert.Equal(0, ppuState.Cycles) // Cycles reset
        Assert.Equal(0uy, ppuState.LY) // LY still 0

        // Step through HBlank (204 cycles) -> next scanline OamScan
        (ppuState, mem) <- Ppu.step 204 ppuState mem
        Assert.Equal(Ppu.OamScan, ppuState.Mode)
        Assert.Equal(0, ppuState.Cycles)
        Assert.Equal(1uy, ppuState.LY) // LY incremented

        // Simulate 143 more scanlines (1 + 143 = 144)
        for _ = 1 to 143 do
            (ppuState, mem) <- Ppu.step (80 + 172 + 204) ppuState mem // Total cycles for OamScan, Drawing, HBlank
            
        Assert.Equal(144uy, ppuState.LY)
        Assert.Equal(Ppu.VBlank, ppuState.Mode) // Should be VBlank now

        // Step through VBlank (10 lines * 456 cycles/line)
        // Simulate one VBlank line (456 cycles)
        (ppuState, mem) <- Ppu.step 456 ppuState mem
        Assert.Equal(Ppu.VBlank, ppuState.Mode)
        Assert.Equal(145uy, ppuState.LY) // LY incremented

        // Simulate 9 more VBlank lines (145 + 9 = 154)
        for _ = 1 to 9 do
            (ppuState, mem) <- Ppu.step 456 ppuState mem
        
        Assert.Equal(154uy, ppuState.LY) // LY should be 154 (144 + 10)
        Assert.Equal(Ppu.OamScan, ppuState.Mode) // Should transition back to OamScan
        Assert.Equal(0uy, ppuState.LY) // LY reset to 0 for new frame
        Assert.Equal(0, ppuState.Cycles) // Cycles reset

    // Test for V-Blank interrupt
    [<Fact>]
    let ``V-Blank interrupt is requested at LY 144`` () =
        let mutable ppuState = Ppu.create ()
        let mutable mem = createMemoryBus ()

        // Fast forward to LY 143 (before HBlank of last scanline)
        for _ = 0 to 143 do // 0 to 143 means 144 lines (0 to 143)
             (ppuState, mem) <- Ppu.step (80 + 172 + 204) ppuState mem

        // Ppu.step was called for LY 143's HBlank, so LY is now 144, and mode should be VBlank
        Assert.Equal(144uy, ppuState.LY)
        Assert.Equal(Ppu.VBlank, ppuState.Mode)

        // Check if V-Blank interrupt flag is set (Bit 0 of IF register 0xFF0F)
        let ifReg = Memory.read 0xFF0Fus mem
        Assert.True((ifReg &&& 0x01uy) <> 0uy, "V-Blank interrupt (IF bit 0) should be set")

    // Test for DMA transfer
    [<Fact>]
    let ``DMA transfer correctly copies data to OAM`` () =
        let mutable mem = createMemoryBus ()

        // Prepare source data in memory (e.g., in WRAM)
        let sourceAddr = 0xC000us
        let testData = Array.init 160 (fun i -> byte i) // 0, 1, 2, ..., 159
        
        for i = 0 to 159 do
            mem <- Memory.write (sourceAddr + uint16 i) testData.[i] mem

        // Trigger DMA by writing to DMA register
        // The value written is the high byte of the source address (0xC0 for 0xC000)
        mem <- Memory.write Ppu.DMA 0xC0uy mem

        // Simulate PPU step to process DMA (DMA is handled in Ppu.step)
        let mutable ppuState = Ppu.create ()
        (ppuState, mem) <- Ppu.step 160 ppuState mem // Enough cycles for DMA

        // Verify OAM content
        for i = 0 to 159 do
            let oamValue = Memory.read (Ppu.OAM_START_ADDR + uint16 i) mem
            Assert.Equal(testData.[i], oamValue)
        
        // Verify DMA register is cleared
        Assert.Equal(0uy, Memory.read Ppu.DMA mem)

    // TODO: Add tests for sprite rendering (visibility, priority, flipping, palette)
    // TODO: Add tests for window rendering (visibility, position, tile map)
    // TODO: Add tests for STAT interrupts (HBlank, OamScan, LYC=LY)
