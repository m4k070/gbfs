/// 依存なし PNG エンコーダ (gbfs.Lib.Png) のテスト。
/// 出力を ZLibStream で展開し、IHDR / IDAT / IEND の構造と生スキャンラインを検証する。
module PngTests

open System
open System.IO
open System.IO.Compression
open Xunit
open gbfs.Lib

let private sigBytes = [| 0x89uy; 0x50uy; 0x4Euy; 0x47uy; 0x0Duy; 0x0Auy; 0x1Auy; 0x0Auy |]

/// PNG を走査して (tag, payload) の列を返す
let private chunks (png: byte array) =
    let mutable pos = 8
    let result = ResizeArray<string * byte array>()
    while pos + 12 <= png.Length do
        let len =
            (int png.[pos] <<< 24) ||| (int png.[pos + 1] <<< 16)
            ||| (int png.[pos + 2] <<< 8) ||| int png.[pos + 3]
        let tag = Text.Encoding.ASCII.GetString(png, pos + 4, 4)
        let payload = png.[pos + 8 .. pos + 8 + len - 1]
        result.Add(tag, payload)
        pos <- pos + 12 + len
    List.ofSeq result

let private inflate (data: byte array) =
    use ms = new MemoryStream(data)
    use z = new ZLibStream(ms, CompressionMode.Decompress)
    use out = new MemoryStream()
    z.CopyTo out
    out.ToArray()

[<Fact>]
let ``PNG はシグネチャと IHDR/IDAT/IEND を持つ`` () =
    let png = Png.encodeGray 2 2 [| 0uy; 1uy; 2uy; 3uy |]
    Assert.Equal<byte array>(sigBytes, png.[0..7])
    let cs = chunks png
    Assert.Equal<string list>([ "IHDR"; "IDAT"; "IEND" ], cs |> List.map fst)
    let ihdr = snd cs.[0]
    Assert.Equal(2, (int ihdr.[0] <<< 24) ||| (int ihdr.[1] <<< 16) ||| (int ihdr.[2] <<< 8) ||| int ihdr.[3])
    Assert.Equal(2, (int ihdr.[4] <<< 24) ||| (int ihdr.[5] <<< 16) ||| (int ihdr.[6] <<< 8) ||| int ihdr.[7])
    Assert.Equal(8uy, ihdr.[8]) // bit depth
    Assert.Equal(0uy, ihdr.[9]) // color type: grayscale
    Assert.Equal(0uy, ihdr.[12]) // interlace: none

[<Fact>]
let ``IDAT はフィルタ 0 付きの生スキャンラインに展開できる`` () =
    // 3x2 の画像
    let pixels = [| 10uy; 20uy; 30uy; 40uy; 50uy; 60uy |]
    let png = Png.encodeGray 3 2 pixels
    let cs = chunks png
    let raw = inflate (snd cs.[1])
    Assert.Equal(8, raw.Length) // (3 + 1) * 2
    Assert.Equal<byte array>([| 0uy; 10uy; 20uy; 30uy |], raw.[0..3]) // フィルタ + 1 行目
    Assert.Equal<byte array>([| 0uy; 40uy; 50uy; 60uy |], raw.[4..7])

[<Fact>]
let ``65535 バイトを超える画像でも無圧縮ブロックを分割して書く`` () =
    // 160x144 = 23,040 バイト + 行フィルタで 23,184 バイト → 複数ブロックになる
    let pixels = Array.init (160 * 144) (fun i -> byte (i % 251))
    let png = Png.encodeGray 160 144 pixels
    let cs = chunks png
    let raw = inflate (snd cs.[1])
    Assert.Equal((160 + 1) * 144, raw.Length)
    for y in [ 0; 1; 71; 143 ] do
        Assert.Equal(0uy, raw.[y * 161])
        Assert.Equal(byte ((y * 160) % 251), raw.[y * 161 + 1])

[<Fact>]
let ``framebuffer の 4 階調は明→暗のグレーに写る`` () =
    let png = Png.encodeFrameBuffer 4 1 [| 0uy; 1uy; 2uy; 3uy |]
    let raw = inflate (snd (chunks png).[1])
    Assert.Equal<byte array>([| 0uy; 0xFFuy; 0xAAuy; 0x55uy; 0x00uy |], raw)

[<Fact>]
let ``ピクセル数が足りないと例外`` () =
    Assert.Throws<ArgumentException>(fun () -> Png.encodeGray 4 4 [| 1uy; 2uy |] |> ignore)
        |> ignore
