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

    // --- Helper functions for sprite and tile rendering tests ---

    /// Sets up a simple 8x8 tile at the given tile index in VRAM (0x8000 method)
    /// The tile is a solid block of the given color ID (0-3)
    let setupTile (tileIndex: int) (colorId: byte) (mem: Memory.MemoryBus) : Memory.MemoryBus =
        let tileAddr = 0x8000us + uint16 (tileIndex * 16)
        let mutable newMem = mem
        // Each tile row is 2 bytes. For a solid color:
        // colorId 0: byte1=0x00, byte2=0x00
        // colorId 1: byte1=0xFF, byte2=0x00
        // colorId 2: byte1=0x00, byte2=0xFF
        // colorId 3: byte1=0xFF, byte2=0xFF
        let (byte1, byte2) =
            match colorId with
            | 0uy -> (0x00uy, 0x00uy)
            | 1uy -> (0xFFuy, 0x00uy)
            | 2uy -> (0x00uy, 0xFFuy)
            | 3uy -> (0xFFuy, 0xFFuy)
            | _ -> (0x00uy, 0x00uy)
        for row = 0 to 7 do
            newMem <- Memory.write (tileAddr + uint16 (row * 2)) byte1 newMem
            newMem <- Memory.write (tileAddr + uint16 (row * 2 + 1)) byte2 newMem
        newMem

    /// Sets up a sprite in OAM at the given index
    let setupSprite (index: int) (yPos: byte) (xPos: byte) (tileIndex: byte) (attributes: byte) (mem: Memory.MemoryBus) : Memory.MemoryBus =
        let oamAddr = Ppu.OAM_START_ADDR + uint16 (index * 4)
        let mutable newMem = mem
        newMem <- Memory.write oamAddr yPos newMem
        newMem <- Memory.write (oamAddr + 1us) xPos newMem
        newMem <- Memory.write (oamAddr + 2us) tileIndex newMem
        newMem <- Memory.write (oamAddr + 3us) attributes newMem
        newMem

    /// Enables LCD and sprites in LCDC register
    let enableLcdAndSprites (mem: Memory.MemoryBus) : Memory.MemoryBus =
        // LCDC: Bit 7=LCD Enable, Bit 1=OBJ Enable
        Memory.write Ppu.LCDC 0x83uy mem // LCD on, BG on, Sprites on

    /// Runs PPU through one full scanline (OamScan + Drawing + HBlank)
    let runScanline (ppuState: Ppu.PpuState) (mem: Memory.MemoryBus) : Ppu.PpuState * Memory.MemoryBus =
        Ppu.step (80 + 172 + 204) ppuState mem

    // --- Sprite Rendering Tests ---

    [<Fact>]
    let ``Sprite is rendered when visible on screen`` () =
        let mutable mem = createMemoryBus ()
        let mutable ppuState = Ppu.create ()

        // Enable LCD and sprites
        mem <- enableLcdAndSprites mem

        // Set OBP0 palette (all colors mapped to distinct values)
        // Color 0 is transparent for sprites
        // Colors 1, 2, 3 mapped to 1, 2, 3
        mem <- Memory.write Ppu.OBP0 0xE4uy mem // 11 10 01 00 = 3,2,1,0

        // Set up a solid tile (color ID 3 = non-transparent)
        mem <- setupTile 0 3uy mem

        // Set up sprite at position (16, 24) which means screen position (8, 8)
        // Y=16 means top of sprite at scanline 0, X=24 means left at pixel 16
        mem <- setupSprite 0 24uy 24uy 0uy 0x00uy mem // No flip, OBP0, above BG

        // Run through scanlines 0-7 (the sprite is 8 pixels tall)
        for _ = 0 to 7 do
            (ppuState, mem) <- runScanline ppuState mem

        // Check that sprite pixels are rendered (at screen X=16-23, Y=0-7)
        for y = 0 to 7 do
            for x = 16 to 23 do
                let fbIndex = y * 160 + x
                // Color 3 maps to palette color 3 (bits 6-7 of OBP0 = 11 = 3)
                Assert.Equal(3uy, ppuState.FrameBuffer.[fbIndex])

    [<Fact>]
    let ``Sprite with X-flip is rendered correctly`` () =
        let mutable mem = createMemoryBus ()
        let mutable ppuState = Ppu.create ()

        mem <- enableLcdAndSprites mem
        mem <- Memory.write Ppu.OBP0 0xE4uy mem

        // Set up a tile with gradient (different color per column)
        // We'll make left half color 1, right half color 2
        let tileAddr = 0x8000us
        let mutable newMem = mem
        for row = 0 to 7 do
            // Left 4 pixels: color 1 (byte1=0xF0, byte2=0x00)
            // Right 4 pixels: color 2 (byte1=0x00, byte2=0x0F)
            // Combined: byte1=0xF0, byte2=0x0F
            newMem <- Memory.write (tileAddr + uint16 (row * 2)) 0xF0uy newMem
            newMem <- Memory.write (tileAddr + uint16 (row * 2 + 1)) 0x0Fuy newMem
        mem <- newMem

        // Set up sprite with X-flip (attribute bit 5)
        mem <- setupSprite 0 16uy 8uy 0uy 0x20uy mem // X-flip enabled

        // Run scanline 0
        (ppuState, mem) <- runScanline ppuState mem

        // With X-flip, the left part should now have color 2, right part color 1
        // Pixel 0-3 should be color 2, pixel 4-7 should be color 1
        let y = 0
        for x = 0 to 3 do
            let fbIndex = y * 160 + x
            // Color 2 maps to palette color 2 (bits 4-5 of OBP0 = 10 = 2)
            Assert.Equal(2uy, ppuState.FrameBuffer.[fbIndex])
        for x = 4 to 7 do
            let fbIndex = y * 160 + x
            // Color 1 maps to palette color 1 (bits 2-3 of OBP0 = 01 = 1)
            Assert.Equal(1uy, ppuState.FrameBuffer.[fbIndex])

    [<Fact>]
    let ``Sprite behind BG only shows through BG color 0`` () =
        let mutable mem = createMemoryBus ()
        let mutable ppuState = Ppu.create ()

        // Enable LCD, BG, and sprites
        mem <- Memory.write Ppu.LCDC 0x83uy mem

        // Set BGP palette
        mem <- Memory.write Ppu.BGP 0xE4uy mem // 3,2,1,0
        // Set OBP0 palette
        mem <- Memory.write Ppu.OBP0 0xE4uy mem

        // Set up BG tile 0 as solid color 0 (transparent)
        mem <- setupTile 0 0uy mem
        // Set up sprite tile 1 as solid color 3
        mem <- setupTile 1 3uy mem

        // Set BG tile map to use tile 0
        mem <- Memory.write 0x9800us 0uy mem

        // Set up sprite with priority=behind BG (attribute bit 7)
        mem <- setupSprite 0 16uy 8uy 1uy 0x80uy mem // Behind BG

        // Run scanline 0
        (ppuState, mem) <- runScanline ppuState mem

        // Since BG is color 0, sprite should show through
        for x = 0 to 7 do
            let fbIndex = x
            Assert.Equal(3uy, ppuState.FrameBuffer.[fbIndex])

    [<Fact>]
    let ``Sprite uses OBP1 palette when attribute bit 4 is set`` () =
        let mutable mem = createMemoryBus ()
        let mutable ppuState = Ppu.create ()

        mem <- enableLcdAndSprites mem

        // Set different palettes for OBP0 and OBP1
        mem <- Memory.write Ppu.OBP0 0xE4uy mem // 3,2,1,0
        mem <- Memory.write Ppu.OBP1 0x1Buy mem // 0,1,2,3 (inverted)

        // Set up tile with color 3
        mem <- setupTile 0 3uy mem

        // Set up sprite using OBP1 (attribute bit 4)
        mem <- setupSprite 0 16uy 8uy 0uy 0x10uy mem // OBP1

        // Run scanline 0
        (ppuState, mem) <- runScanline ppuState mem

        // Color 3 with OBP1 (0x1B = 00 01 10 11) -> bits 6-7 = 00 = 0
        for x = 0 to 7 do
            let fbIndex = x
            Assert.Equal(0uy, ppuState.FrameBuffer.[fbIndex])

    // --- Window Rendering Tests ---

    [<Fact>]
    let ``Window is rendered when enabled and visible`` () =
        let mutable mem = createMemoryBus ()
        let mutable ppuState = Ppu.create ()

        // Enable LCD, BG, Window
        // LCDC: Bit 7=LCD on, Bit 5=Window on, Bit 0=BG on
        mem <- Memory.write Ppu.LCDC 0xA1uy mem

        // Set BGP palette
        mem <- Memory.write Ppu.BGP 0xE4uy mem

        // Set up BG tile 0 as color 0
        mem <- setupTile 0 0uy mem
        // Set up Window tile 1 as color 2
        mem <- setupTile 1 2uy mem

        // Set BG tile map to tile 0
        mem <- Memory.write 0x9800us 0uy mem
        // Set Window tile map to tile 1
        mem <- Memory.write 0x9C00us 1uy mem

        // Set window position WY=0, WX=7 (window starts at screen X=0)
        mem <- Memory.write Ppu.WY 0uy mem
        mem <- Memory.write Ppu.WX 7uy mem

        // Run scanline 0
        (ppuState, mem) <- runScanline ppuState mem

        // Window should be rendered with color 2
        for x = 0 to 7 do
            let fbIndex = x
            // Color 2 with BGP 0xE4 -> bits 4-5 = 10 = 2
            Assert.Equal(2uy, ppuState.FrameBuffer.[fbIndex])

    [<Fact>]
    let ``Window respects WX and WY position`` () =
        let mutable mem = createMemoryBus ()
        let mutable ppuState = Ppu.create ()

        // Enable LCD, BG, Window
        mem <- Memory.write Ppu.LCDC 0xA1uy mem

        // Set BGP palette
        mem <- Memory.write Ppu.BGP 0xE4uy mem

        // Set up BG tile 0 as color 1
        mem <- setupTile 0 1uy mem
        // Set up Window tile 1 as color 3
        mem <- setupTile 1 3uy mem

        // Set BG tile map to tile 0
        mem <- Memory.write 0x9800us 0uy mem
        // Set Window tile map to tile 1
        mem <- Memory.write 0x9C00us 1uy mem

        // Set window position WY=4, WX=47 (window starts at screen X=40, Y=4)
        mem <- Memory.write Ppu.WY 4uy mem
        mem <- Memory.write Ppu.WX 47uy mem

        // Run scanlines 0-3 (before window Y)
        for _ = 0 to 3 do
            (ppuState, mem) <- runScanline ppuState mem

        // Check scanline 3 - should be all BG (color 1)
        for x = 0 to 79 do
            let fbIndex = 3 * 160 + x
            Assert.Equal(1uy, ppuState.FrameBuffer.[fbIndex])

        // Run scanline 4 (window starts here)
        (ppuState, mem) <- runScanline ppuState mem

        // Check scanline 4 - X < 40 should be BG (color 1), X >= 40 should be window (color 3)
        for x = 0 to 39 do
            let fbIndex = 4 * 160 + x
            Assert.Equal(1uy, ppuState.FrameBuffer.[fbIndex])
        for x = 40 to 47 do
            let fbIndex = 4 * 160 + x
            Assert.Equal(3uy, ppuState.FrameBuffer.[fbIndex])

    // --- STAT Interrupt Tests ---

    [<Fact>]
    let ``STAT HBlank interrupt is triggered when enabled`` () =
        let mutable mem = createMemoryBus ()
        let mutable ppuState = Ppu.create ()

        // Enable LCD
        mem <- Memory.write Ppu.LCDC 0x80uy mem

        // Enable STAT Mode 0 (HBlank) interrupt (bit 3)
        mem <- Memory.write Ppu.STAT 0x08uy mem

        // Clear IF register
        mem <- Memory.write 0xFF0Fus 0x00uy mem

        // Run through OamScan (80 cycles)
        (ppuState, mem) <- Ppu.step 80 ppuState mem
        Assert.Equal(Ppu.Drawing, ppuState.Mode)

        // Run through Drawing (172 cycles) -> HBlank
        (ppuState, mem) <- Ppu.step 172 ppuState mem
        Assert.Equal(Ppu.HBlank, ppuState.Mode)

        // Check if STAT interrupt (LCD STAT = bit 1 of IF) is set
        let ifReg = Memory.read 0xFF0Fus mem
        Assert.True((ifReg &&& 0x02uy) <> 0uy, "STAT interrupt should be set on HBlank")

    [<Fact>]
    let ``STAT OAM interrupt is triggered when enabled`` () =
        let mutable mem = createMemoryBus ()
        let mutable ppuState = Ppu.create ()

        // Enable LCD
        mem <- Memory.write Ppu.LCDC 0x80uy mem

        // Enable STAT Mode 2 (OAM) interrupt (bit 5)
        mem <- Memory.write Ppu.STAT 0x20uy mem

        // Run a full scanline to get back to OamScan on next line
        (ppuState, mem) <- runScanline ppuState mem
        Assert.Equal(1uy, ppuState.LY)
        Assert.Equal(Ppu.OamScan, ppuState.Mode)

        // Clear IF to check new interrupt
        mem <- Memory.write 0xFF0Fus 0x00uy mem

        // Now we're in OamScan of line 1, but interrupt was already fired on transition
        // So we need to run another full scanline
        (ppuState, mem) <- runScanline ppuState mem

        // Check if STAT interrupt is set (on transition to OamScan)
        let ifReg = Memory.read 0xFF0Fus mem
        Assert.True((ifReg &&& 0x02uy) <> 0uy, "STAT interrupt should be set on OAM scan")

    [<Fact>]
    let ``STAT LYC=LY interrupt is triggered when LY matches LYC`` () =
        let mutable mem = createMemoryBus ()
        let mutable ppuState = Ppu.create ()

        // Enable LCD
        mem <- Memory.write Ppu.LCDC 0x80uy mem

        // Set LYC to 5
        mem <- Memory.write Ppu.LYC 5uy mem

        // Enable STAT LYC=LY interrupt (bit 6)
        mem <- Memory.write Ppu.STAT 0x40uy mem

        // Clear IF register
        mem <- Memory.write 0xFF0Fus 0x00uy mem

        // Run through scanlines 0-4
        for _ = 0 to 4 do
            (ppuState, mem) <- runScanline ppuState mem

        Assert.Equal(5uy, ppuState.LY)

        // Clear IF to isolate the LYC interrupt
        mem <- Memory.write 0xFF0Fus 0x00uy mem

        // Step once more to trigger LYC comparison
        (ppuState, mem) <- Ppu.step 1 ppuState mem

        // Check STAT interrupt
        let ifReg = Memory.read 0xFF0Fus mem
        Assert.True((ifReg &&& 0x02uy) <> 0uy, "STAT interrupt should be set when LY=LYC")

        // Also check STAT register LYC=LY flag (bit 2)
        let statReg = Memory.read Ppu.STAT mem
        Assert.True((statReg &&& 0x04uy) <> 0uy, "STAT LYC=LY flag should be set")
