namespace gbfs.Lib

/// 依存なしの最小 PNG エンコーダ (8bit グレースケール)。
/// 圧縮は deflate の「無圧縮ブロック」(stored) のみ — 実装が短く、外部ライブラリが要らない。
/// 画面が CGB のカラーになる (issue #19) までは framebuffer の 4 階調をグレーに写して出力する。
module Png =

  let private crcTable =
    Array.init 256 (fun n ->
      let mutable c = uint32 n
      for _ in 0 .. 7 do
        c <- if (c &&& 1u) <> 0u then 0xEDB88320u ^^^ (c >>> 1) else c >>> 1
      c)

  let private crc32 (data: byte array) : uint32 =
    let mutable c = 0xFFFFFFFFu
    for b in data do
      c <- crcTable.[int ((c ^^^ uint32 b) &&& 0xFFu)] ^^^ (c >>> 8)
    c ^^^ 0xFFFFFFFFu

  let private adler32 (data: byte array) : uint32 =
    let mutable a = 1u
    let mutable b = 0u
    for x in data do
      a <- (a + uint32 x) % 65521u
      b <- (b + a) % 65521u
    (b <<< 16) ||| a

  let private be32 (v: uint32) : byte array =
    [| byte (v >>> 24); byte (v >>> 16); byte (v >>> 8); byte v |]

  /// 長さ + タイプ + データ + CRC の 1 チャンク
  let private chunk (tag: string) (payload: byte array) : byte array =
    let body = Array.append (System.Text.Encoding.ASCII.GetBytes tag) payload
    Array.concat [ be32 (uint32 payload.Length); body; be32 (crc32 body) ]

  /// zlib ストリーム (無圧縮 deflate ブロック + adler32)
  let private zlibStore (data: byte array) : byte array =
    let out = System.Collections.Generic.List<byte>()
    out.Add 0x78uy // CMF: deflate, 32K ウィンドウ
    out.Add 0x01uy // FLG: 0x7801 は 256 の倍数 (チェックビット)
    let mutable i = 0
    while i < data.Length do
      let len = min 65535 (data.Length - i)
      let final = if i + len >= data.Length then 1uy else 0uy
      out.Add final // BFINAL + BTYPE = 00 (stored)
      out.Add(byte (len &&& 0xFF))
      out.Add(byte ((len >>> 8) &&& 0xFF))
      out.Add(byte ((~~~len) &&& 0xFF))
      out.Add(byte (((~~~len) >>> 8) &&& 0xFF))
      for j in i .. i + len - 1 do
        out.Add data.[j]
      i <- i + len
    out.AddRange(be32 (adler32 data))
    out.ToArray()

  /// 8bit グレースケール PNG (pixels は width * height バイト)
  let encodeGray (width: int) (height: int) (pixels: byte array) : byte array =
    if pixels.Length < width * height then
      invalidArg "pixels" (sprintf "ピクセル数が足りません (%d < %d)" pixels.Length (width * height))
    let ihdr =
      Array.concat [ be32 (uint32 width); be32 (uint32 height); [| 8uy; 0uy; 0uy; 0uy; 0uy |] ]
    let raw = Array.zeroCreate ((width + 1) * height)
    for y in 0 .. height - 1 do
      raw.[y * (width + 1)] <- 0uy // フィルタ: None
      Array.blit pixels (y * width) raw (y * (width + 1) + 1) width
    Array.concat [
      [| 0x89uy; 0x50uy; 0x4Euy; 0x47uy; 0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy |] // PNG シグネチャ
      chunk "IHDR" ihdr
      chunk "IDAT" (zlibStore raw)
      chunk "IEND" [||]
    ]

  /// gbfs の framebuffer (値 0-3 の 4 階調) をグレーに写して PNG にする。
  /// 写し方は Desktop UI と同じ (0 = 最も明るい)。
  let encodeFrameBuffer (width: int) (height: int) (frameBuffer: byte array) : byte array =
    let gray =
      Array.init (width * height) (fun i ->
        match int frameBuffer.[i] with
        | 0 -> 0xFFuy
        | 1 -> 0xAAuy
        | 2 -> 0x55uy
        | _ -> 0x00uy)
    encodeGray width height gray
