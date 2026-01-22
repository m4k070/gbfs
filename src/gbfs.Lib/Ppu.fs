namespace gbfs.Lib

module Ppu =

    // PPU I/O Register Addresses
    let LCDC = 0xFF40us // LCD Control
    let STAT = 0xFF41us // LCDC Status
    let SCY  = 0xFF42us // Scroll Y
    let SCX  = 0xFF43us // Scroll X
    let LY   = 0xFF44us // LCDC Y-Coordinate
    let LYC  = 0xFF45us // LY Compare
    let DMA  = 0xFF46us // DMA Transfer and Start Address
    let BGP  = 0xFF47us // BG Palette Data
    let OBP0 = 0xFF48us // Object Palette 0 Data
    let OBP1 = 0xFF49us // Object Palette 1 Data
    let WY   = 0xFF4Aus // Window Y Position
    let WX   = 0xFF4Bus // Window X Position - 7

    // Sprite related constants
    let OAM_START_ADDR = 0xFE00us
    let OAM_SPRITE_COUNT = 40
    let OAM_ENTRY_SIZE = 4 // bytes per sprite

    /// Represents a single sprite's attributes from OAM.
    type Sprite = {
        YPos: int // Y-coordinate (0-255, actual position is YPos - 16)
        XPos: int // X-coordinate (0-255, actual position is XPos - 8)
        TileIndex: byte // Tile number (0-255)
        Palette: byte // CGB Palette Number (0-7) / DMG Palette (0=OBP0, 1=OBP1)
        XFlip: bool
        YFlip: bool
        Priority: byte // 0=OBJ above BG/Window, 1=OBJ behind BG/Window
        OAMIndex: int // Original index in OAM (for tie-breaking)
    }

    /// Gets the current sprite height (8 or 16 pixels) based on LCDC register.
    let private getSpriteHeight (lcdc: byte) =
        if (lcdc &&& 0x04uy) = 0uy then 8 // Bit 2: OBJ (Sprite) Size (0=8x8, 1=8x16)
        else 16

    /// Determines if a sprite pixel should be drawn based on its priority and the background pixel.
    let private shouldDrawSpritePixel (sprite: Sprite) (currentBgPixel: byte) : bool =
        if sprite.Priority = 1uy then // Sprite behind BG/Window
            // Only draw sprite pixel if BG pixel is color 0 (transparent)
            currentBgPixel = 0uy // Assuming 0 is the "transparent" BG color
        else // Sprite above BG/Window
            true

    /// Represents a single sprite's attributes from OAM.
    type PpuMode =
        | HBlank  // Mode 0
        | VBlank  // Mode 1
        | OamScan // Mode 2
        | Drawing // Mode 3

    type PpuState = {
        Mode: PpuMode
        Cycles: int
        LY: byte // Current scanline (0-153)
        FrameBuffer: byte array // 160 * 144 pixels, 4 shades of gray
    }

    let create () = {
        Mode = OamScan
        Cycles = 0
        LY = 0uy
        FrameBuffer = Array.zeroCreate (160 * 144)
    }

    /// Reads all 40 sprites from OAM and parses their attributes.
    let private readOamSprites (mem: Memory.MemoryBus) : Sprite list =
        [ for i in 0 .. OAM_SPRITE_COUNT - 1 do
            let oamAddr = OAM_START_ADDR + uint16(i * OAM_ENTRY_SIZE)
            let y = Memory.read oamAddr mem
            let x = Memory.read (oamAddr + 1us) mem
            let tileIdx = Memory.read (oamAddr + 2us) mem
            let attributes = Memory.read (oamAddr + 3us) mem

            let bgAndWindowOverOam = (attributes &&& 0x80uy) <> 0uy // Bit 7
            let yFlip = (attributes &&& 0x40uy) <> 0uy // Bit 6
            let xFlip = (attributes &&& 0x20uy) <> 0uy // Bit 5
            let palette = (attributes &&& 0x10uy) >>> 4 // Bit 4
            let priority = if bgAndWindowOverOam then 1uy else 0uy // Simpler priority: 0 is above BG/Window, 1 is behind

            yield {
                YPos = int y
                XPos = int x
                TileIndex = tileIdx
                Palette = palette
                XFlip = xFlip
                YFlip = yFlip
                Priority = priority
                OAMIndex = i
            }
        ]

    /// Renders the given list of sprites onto the framebuffer for the current scanline.
    let private renderSprites (ppuState: PpuState) (mem: Memory.MemoryBus) (lcdc: byte) (obp0: byte) (obp1: byte) (sprites: Sprite list) : PpuState =
        let mutable newFrameBuffer = ppuState.FrameBuffer
        let spriteHeight = getSpriteHeight lcdc

        // Iterate through sprites, respecting their drawing order (priority and X-pos)
        // Note: Sprites are already sorted by X-position (and OAM index as tie-breaker)
        for sprite in sprites do
            let spriteY = sprite.YPos - 16 // Actual Y-position on screen
            let spriteX = sprite.XPos - 8  // Actual X-position on screen

            let yOffsetInSprite = int ppuState.LY - spriteY

            // Adjust yOffsetInSprite for Y-flipping
            let yTile = if sprite.YFlip then (spriteHeight - 1) - yOffsetInSprite else yOffsetInSprite

            // Get tile data from VRAM (sprite tiles are always in 0x8000-0x8FFF)
            let tileDataBase = 0x8000us
            let tilePatternAddr = tileDataBase + (uint16 sprite.TileIndex * 16us) // Each tile is 16 bytes (8x8 pixels, 2 bytes per row)

            let tileRowAddr = tilePatternAddr + uint16 (yTile * 2)

            let byte1 = Memory.read tileRowAddr mem
            let byte2 = Memory.read (tileRowAddr + 1us) mem

            // Determine which palette to use
            let objPalette = if sprite.Palette = 0uy then obp0 else obp1

            for xOffsetInSprite in 0 .. 7 do // Iterate 8 pixels horizontally
                let screenX = spriteX + xOffsetInSprite
                // Only draw if within screen bounds (0-159)
                if screenX >= 0 && screenX < 160 then

                    // Adjust xOffsetInSprite for X-flipping
                    let xPixel = if sprite.XFlip then xOffsetInSprite else (7 - xOffsetInSprite)

                    let colorBit1 = (byte1 >>> xPixel) &&& 1uy
                    let colorBit2 = (byte2 >>> xPixel) &&& 1uy
                    let colorId = (colorBit2 <<< 1uy) ||| colorBit1

                    // Color ID 0 is transparent for sprites
                    if colorId <> 0uy then
                        // Map color id to actual color using OBP0 or OBP1
                        let color =
                            match colorId with
                            | 1uy -> (objPalette >>> 2) &&& 0x03uy
                            | 2uy -> (objPalette >>> 4) &&& 0x03uy
                            | 3uy -> (objPalette >>> 6) &&& 0x03uy
                            | _ -> 0uy // Should not happen, as 0uy is transparent

                        let fbIndex = (int ppuState.LY) * 160 + (int screenX)
                        
                        // Apply priority:
                        // If sprite.Priority = 1 (behind BG/Window), only draw if BG pixel is color 0
                        // (i.e., transparent or background color 0)
                        // If sprite.Priority = 0 (above BG/Window), always draw unless it's a BG color 0
                        // which is actually a color, not transparent in BG logic.
                        let currentBgPixel = ppuState.FrameBuffer.[fbIndex] // pixel already rendered for BG

                        // Apply priority:
                        if shouldDrawSpritePixel sprite currentBgPixel then
                            newFrameBuffer.[fbIndex] <- color
        
        { ppuState with FrameBuffer = newFrameBuffer }

    let private shouldDrawSpritePixel (sprite: Sprite) (currentBgPixel: byte) : bool =
        if sprite.Priority = 1uy then // Sprite behind BG/Window
            // Only draw sprite pixel if BG pixel is color 0 (transparent)
            currentBgPixel = 0uy // Assuming 0 is the "transparent" BG color
        else // Sprite above BG/Window
            true

    /// Renders a single scanline to the framebuffer.
    let private renderScanline (ppuState: PpuState) (mem: Memory.MemoryBus) : PpuState =
        let lcdc = Memory.read LCDC mem
        let bgp = Memory.read BGP mem
        let obp0 = Memory.read OBP0 mem
        let obp1 = Memory.read OBP1 mem

        let mutable currentPpuState = ppuState
        let spriteHeight = getSpriteHeight lcdc

        // --- Background/Window Rendering ---
        // Bit 0: BG & Window Display/Priority
        let bgAndWindowDisplayEnable = (lcdc &&& 0x01uy) <> 0uy
        // Bit 5: Window Display Enable
        let windowDisplayEnable = (lcdc &&& 0x20uy) <> 0uy
        // Bit 6: Window Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
        let windowTileMapBase = if (lcdc &&& 0x40uy) = 0uy then 0x9800us else 0x9C00us
        // Bit 4: BG & Window Tile Data Select (0=8800-97FF, 1=8000-8FFF)
        let tileDataBase = if (lcdc &&& 0x10uy) = 0uy then 0x8800us else 0x8000us
        let signedTileIndex = (tileDataBase = 0x8800us)

        // Window position registers
        let wy = Memory.read WY mem
        let wx = Memory.read WX mem
        let actualWx = int wx - 7 // Adjust WX to be 0-based for screen coordinates

        for x in 0 .. 159 do
            let fbIndex = int currentPpuState.LY * 160 + x
            let mutable color = 0uy // Default to background color 0 (transparent)

            // Determine if the current pixel (x, currentPpuState.LY) falls within the window
            let isInWindow =
                windowDisplayEnable &&
                bgAndWindowDisplayEnable && // Window only renders if BG/Window display is also enabled
                currentPpuState.LY >= wy &&
                x >= actualWx &&
                actualWx < 160 && // Window must be at least partially on screen horizontally
                wy < 144 // Window must be at least partially on screen vertically

            if isInWindow then
                // --- Render Window Pixel ---
                let windowLine = currentPpuState.LY - wy
                let windowPixelX = x - actualWx

                let tileMapX = int (uint16 windowPixelX / 8us)
                let tileMapY = int (uint16 windowLine / 8us)
                let tileMapIndex = tileMapY * 32 + tileMapX
                
                let tileIndexAddr = windowTileMapBase + uint16 tileMapIndex
                let tileIndex = int (Memory.read tileIndexAddr mem)

                let tilePatternAddr =
                    if signedTileIndex then
                        let signedIdx = sbyte tileIndex
                        tileDataBase + uint16 ((int signedIdx + 128) * 16)
                    else
                        tileDataBase + uint16 (tileIndex * 16)

                let pixelYInTile = int (windowLine % 8uy)
                let tileRowAddr = tilePatternAddr + uint16 (pixelYInTile * 2)
                
                let byte1 = Memory.read tileRowAddr mem
                let byte2 = Memory.read (tileRowAddr + 1us) mem

                let pixelXInTile = byte (7 - (int (windowPixelX % 8us)))
                let colorBit1 = (byte1 >>> pixelXInTile) &&& 1uy
                let colorBit2 = (byte2 >>> pixelXInTile) &&& 1uy
                let colorId = (colorBit2 <<< 1uy) ||| colorBit1

                color <-
                    match colorId with
                    | 0uy -> bgp &&& 0x03uy
                    | 1uy -> (bgp >>> 2) &&& 0x03uy
                    | 2uy -> (bgp >>> 4) &&& 0x03uy
                    | 3uy -> (bgp >>> 6) &&& 0x03uy
                    | _ -> 0uy // Should not happen
            else if bgAndWindowDisplayEnable then
                // --- Render Background Pixel (if not in window) ---
                let scy = Memory.read SCY mem
                let scx = Memory.read SCX mem

                // Bit 3: BG Tile Map Display Select (0=9800-9BFF, 1=9C00-9FFF)
                let bgTileMapBase = if (lcdc &&& 0x08uy) = 0uy then 0x9800us else 0x9C00us

                let y = currentPpuState.LY + scy // Y coordinate in the full background map
                let mapX = (uint16 x + uint16 scx) &&& 0x01FFus // X coordinate in the full background map
                
                let tileMapX = int (mapX / 8us)
                let tileMapY = int (uint16 y / 8us)
                let tileMapIndex = tileMapY * 32 + tileMapX
                
                let tileIndexAddr = bgTileMapBase + uint16 tileMapIndex
                let tileIndex = int (Memory.read tileIndexAddr mem)

                let tilePatternAddr =
                    if signedTileIndex then
                        let signedIdx = sbyte tileIndex
                        tileDataBase + uint16 ((int signedIdx + 128) * 16)
                    else
                        tileDataBase + uint16 (tileIndex * 16)

                let pixelYInTile = int (y % 8uy)
                let tileRowAddr = tilePatternAddr + uint16 (pixelYInTile * 2)
                
                let byte1 = Memory.read tileRowAddr mem
                let byte2 = Memory.read (tileRowAddr + 1us) mem

                let pixelXInTile = byte (7 - (int (mapX % 8us)))
                let colorBit1 = (byte1 >>> pixelXInTile) &&& 1uy
                let colorBit2 = (byte2 >>> pixelXInTile) &&& 1uy
                let colorId = (colorBit2 <<< 1uy) ||| colorBit1

                color <-
                    match colorId with
                    | 0uy -> bgp &&& 0x03uy
                    | 1uy -> (bgp >>> 2) &&& 0x03uy
                    | 2uy -> (bgp >>> 4) &&& 0x03uy
                    | 3uy -> (bgp >>> 6) &&& 0x03uy
                    | _ -> 0uy // Should not happen
            
            currentPpuState.FrameBuffer.[fbIndex] <- color
        
        // --- Sprite Rendering ---
        // Bit 1: OBJ (Sprite) Display Enable
        if (lcdc &&& 0x02uy) <> 0uy then // If sprites are enabled
            let allSprites = readOamSprites mem

            let visibleSprites =
                allSprites
                |> List.filter (fun s ->
                    let spriteY = s.YPos - 16
                    spriteY <= int currentPpuState.LY && int currentPpuState.LY < (spriteY + spriteHeight) && s.XPos < 168 && s.XPos > 0 // S.XPos must be > 0 (offscreen left) or < 168 (offscreen right)
                )
                |> List.sortWith (fun s1 s2 -> // Sort for priority: lower X first, then lower OAM index
                    if s1.XPos <> s2.XPos then compare s1.XPos s2.XPos
                    else compare s1.OAMIndex s2.OAMIndex
                )
                |> List.take 10 // Limit to 10 sprites per scanline

            currentPpuState <- renderSprites currentPpuState mem lcdc obp0 obp1 visibleSprites
        
        currentPpuState

    /// Initiates an OAM DMA transfer.
    /// Copies 160 bytes from `sourceAddr` to OAM (0xFE00-0xFE9F).
    let private dmaTransfer (sourceAddr: byte) (mem: Memory.MemoryBus) : Memory.MemoryBus =
        let start = uint16 sourceAddr <<< 8 // Source address is sourceAddr * 0x100
        let mutable newMem = mem
        for i in 0 .. OAM_SPRITE_COUNT * OAM_ENTRY_SIZE - 1 do // 160 bytes
            let byteValue = Memory.read (start + uint16 i) newMem
            newMem <- Memory.write (OAM_START_ADDR + uint16 i) byteValue newMem
        newMem

    /// The PPU step function, called from the main CPU loop.
    /// It takes the number of cycles the last CPU instruction took.
    let step (cycles: int) (ppuState: PpuState) (mem: Memory.MemoryBus) : PpuState * Memory.MemoryBus =
        let mutable newPpuState = ppuState
        let mutable newMem = mem
        let mutable remainingCycles = cycles

        // Handle DMA transfer if initiated
        let dmaValue = Memory.read DMA newMem
        if dmaValue <> 0uy then
            newMem <- dmaTransfer dmaValue newMem
            // Reset DMA register after transfer
            newMem <- Memory.write DMA 0uy newMem
            // DMA takes 160 cycles. Subtract from current cycles.
            remainingCycles <- remainingCycles - 160 // Or more accurately, 640 T-cycles / 4 T-cycles per machine cycle = 160 machine cycles.

        newPpuState <- { newPpuState with Cycles = newPpuState.Cycles + remainingCycles }

        // Read current STAT and LYC
        let currentStat = Memory.read STAT newMem
        let lyc = Memory.read LYC newMem

        // Store the old PPU mode for interrupt checking
        let oldPpuMode = newPpuState.Mode

        // Update LYC=LY Flag (Bit 2 of STAT)
        let lycEqLy = (newPpuState.LY = lyc)
        let statWithLycFlag = if lycEqLy then (currentStat ||| 0x04uy) else (currentStat &&& (0xFFuy - 0x04uy))

        // Process PPU modes and update newPpuState, newMem
        match newPpuState.Mode with
        | OamScan ->
            if newPpuState.Cycles >= 80 then
                newPpuState <- { newPpuState with Mode = Drawing; Cycles = newPpuState.Cycles - 80 }
            
        | Drawing ->
            if newPpuState.Cycles >= 172 then
                newPpuState <- renderScanline newPpuState newMem
                
                newPpuState <- { newPpuState with Mode = HBlank; Cycles = newPpuState.Cycles - 172 }

        | HBlank ->
            if newPpuState.Cycles >= 204 then
                newPpuState <- { newPpuState with Cycles = newPpuState.Cycles - 204 }
                
                newPpuState <- { newPpuState with LY = newPpuState.LY + 1uy }

                if newPpuState.LY = 144uy then
                    newPpuState <- { newPpuState with Mode = VBlank }
                    newMem <- Memory.requestInterrupt Memory.VBlankInterrupt newMem // Always request V-Blank interrupt
                else
                    newPpuState <- { newPpuState with Mode = OamScan }

        | VBlank ->
            if newPpuState.Cycles >= 456 then
                newPpuState <- { newPpuState with Cycles = newPpuState.Cycles - 456 }
                newPpuState <- { newPpuState with LY = newPpuState.LY + 1uy }

                if newPpuState.LY > 153uy then
                    newPpuState <- { newPpuState with Mode = OamScan; LY = 0uy }

        // Update STAT register with new mode bits (bits 0-1)
        let newStatModeBits =
            match newPpuState.Mode with
            | HBlank -> 0uy
            | VBlank -> 1uy
            | OamScan -> 2uy
            | Drawing -> 3uy
        let finalStat = (statWithLycFlag &&& 0xFCuy) ||| newStatModeBits // Clear old mode bits, set new ones
        newMem <- Memory.write STAT finalStat newMem

        // Check for STAT interrupts *after* mode transition (if applicable) and LYC comparison
        // LYC=LY interrupt
        if lycEqLy && ((currentStat &&& 0x40uy) <> 0uy) then // Use currentStat for enable bit
            newMem <- Memory.requestInterrupt Memory.LcdStatInterrupt newMem
        
        // Mode 0 H-Blank Interrupt
        if newPpuState.Mode = HBlank && oldPpuMode <> HBlank && ((currentStat &&& 0x08uy) <> 0uy) then
            newMem <- Memory.requestInterrupt Memory.LcdStatInterrupt newMem

        // Mode 1 V-Blank Interrupt (STAT, not general V-Blank)
        if newPpuState.Mode = VBlank && oldPpuMode <> VBlank && ((currentStat &&& 0x10uy) <> 0uy) then
            newMem <- Memory.requestInterrupt Memory.LcdStatInterrupt newMem
        
        // Mode 2 OAM Interrupt
        if newPpuState.Mode = OamScan && oldPpuMode <> OamScan && ((currentStat &&& 0x20uy) <> 0uy) then
            newMem <- Memory.requestInterrupt Memory.LcdStatInterrupt newMem

        // LYレジスタをメモリに書き込む
        newMem <- Memory.write LY (newPpuState.LY) newMem

        (newPpuState, newMem)