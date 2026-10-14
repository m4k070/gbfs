// feeder の --trace (JSON Lines) から「歩いた範囲の地図」を PNG に描く。
//
//   dotnet fsi tools/gbfs-map.fsx [tracePath] [outPath]
//
// 仕様:
//   - 座標 (x, y) をそのまま格子にする (マップ内の絶対タイル座標)。
//   - 立ち寄ったマス = 白、現在地 = 赤。
//   - "walk <dir> blocked" の記録があれば、そのマスのその向きに壁 (青) を描く。
//   - 直近のマップ (group/number) のみを描く (マップが変われば別の地図)。
#r "/home/makoto/work/gbfs-pr/src/gbfs.Lib/bin/Release/net8.0/gbfs.Lib.dll"

open gbfs.Lib
open System
open System.IO
open System.Text.Json

type Entry = {
    T: int
    Seg: int
    X: int
    Y: int
    Dir: int
    Note: string
}

let fst3 (a, _, _) = a
let snd3 (_, b, _) = b
let trd3 (_, _, c) = c

let parse (line: string) =
    let d = JsonDocument.Parse line
    let r = d.RootElement
    let mutable noteEl = Unchecked.defaultof<JsonElement>
    let hasNote = r.TryGetProperty("note", &noteEl)
    { T = r.GetProperty("t").GetInt32()
      Seg = r.GetProperty("seg").GetInt32()
      X = r.GetProperty("x").GetInt32()
      Y = r.GetProperty("y").GetInt32()
      Dir = r.GetProperty("dir").GetInt32()
      Note = (if hasNote then noteEl.GetString() else "") }

let loadTrace (path: string) =
    if not (File.Exists path) then
        eprintfn "trace が見つかりません: %s" path
        []
    else
        File.ReadAllLines path
        |> Array.filter (fun l -> l.TrimStart().StartsWith "{" )
        |> Array.choose (fun l -> try Some(parse l) with _ -> None)
        |> List.ofArray

let render (tracePath: string) (outPath: string) =
    let entries = loadTrace tracePath
    if entries.IsEmpty then
        eprintfn "trace に有効な行がありません: %s" tracePath
    else
        let lastSeg = (List.last entries).Seg
        let cells = entries |> List.filter (fun e -> e.Seg = lastSeg)
        printfn "seg=%d  trace %d 行 → このマップ区間 %d 行" lastSeg entries.Length cells.Length

        let xs = cells |> List.map (fun e -> e.X)
        let ys = cells |> List.map (fun e -> e.Y)
        let x0 = List.min xs - 1
        let y0 = List.min ys - 1
        let cw = List.max xs - x0 + 2
        let ch = List.max ys - y0 + 2
        printfn "範囲 x=%d..%d y=%d..%d → %dx%d マス" x0 (x0 + cw - 1) y0 (y0 + ch - 1) cw ch

        let cell = 8
        let W = cw * cell
        let H = ch * cell
        let rgb = Array.zeroCreate<byte> (W * H * 3)

        let setPx (px: int) (py: int) (c: byte * byte * byte) =
            if px >= 0 && px < W && py >= 0 && py < H then
                let i = (py * W + px) * 3
                rgb.[i] <- fst3 c
                rgb.[i + 1] <- snd3 c
                rgb.[i + 2] <- trd3 c

        // 背景 (薄い市松)
        for y in 0 .. H - 1 do
            for x in 0 .. W - 1 do
                let v = if ((x / cell) + (y / cell)) % 2 = 0 then 24uy else 32uy
                setPx x y (v, v, v)

        // 立ち寄ったマス
        for e in cells do
            let cx = (e.X - x0) * cell
            let cy = (e.Y - y0) * cell
            for y in cy .. cy + cell - 1 do
                for x in cx .. cx + cell - 1 do
                    setPx x y (216uy, 216uy, 216uy)

        // 壁 (通れなかった向き)
        for e in cells do
            if e.Note.Contains "blocked" then
                let p = e.Note.Split(' ')
                let vec =
                    if p.Length >= 3 && p.[0] = "walk" then
                        match p.[1] with
                        | "left" -> Some(-1, 0)
                        | "right" -> Some(1, 0)
                        | "up" -> Some(0, -1)
                        | "down" -> Some(0, 1)
                        | _ -> None
                    else None
                match vec with
                | Some(dx, dy) ->
                    let cx = (e.X - x0) * cell
                    let cy = (e.Y - y0) * cell
                    for k in 0 .. cell - 1 do
                        for t in 0 .. 2 do
                            let px = cx + (if dx < 0 then t elif dx > 0 then cell - 1 - t else k)
                            let py = cy + (if dy < 0 then t elif dy > 0 then cell - 1 - t else k)
                            setPx px py (32uy, 96uy, 216uy)
                | None -> ()

        // 現在地
        let here = List.last cells
        let hx = (here.X - x0) * cell
        let hy = (here.Y - y0) * cell
        for y in hy .. hy + cell - 1 do
            for x in hx .. hx + cell - 1 do
                setPx x y (224uy, 32uy, 32uy)

        File.WriteAllBytes(outPath, Png.encodeRgb W H rgb)
        printfn "saved %s (%dx%d px)  現在地=(%d,%d) dir=%d" outPath W H here.X here.Y here.Dir

let argv = fsi.CommandLineArgs |> Array.filter (fun a -> not (a.EndsWith(".fsx")) && a <> "--")
let tracePath = if argv.Length >= 1 then argv.[0] else "/tmp/gbfs-trace.jsonl"
let outPath = if argv.Length >= 2 then argv.[1] else "/tmp/gbfs-map.png"
render tracePath outPath
