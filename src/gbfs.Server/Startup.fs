module gbfs.Server.Program

open System.Threading
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting

module McpFrameStore =
    let private frameBuffer: byte array = Array.zeroCreate (160 * 144)
    let mutable private hasFrame = 0

    let write (data: byte array) =
        let len = min data.Length frameBuffer.Length
        System.Array.Copy(data, frameBuffer, len)
        Volatile.Write(&hasFrame, 1)

    let read () =
        if Volatile.Read(&hasFrame) = 1 then
            let copy = Array.copy frameBuffer
            Some copy
        else
            None

/// gbfs.Server: MCP frame relay のホストのみ。
/// UI は gbfs.Desktop (Avalonia)。Web UI (Bolero) は廃止済み — 将来の
/// Web UI を足す場合はここに API 契約を追加する想定。
[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    let app = builder.Build()

    // MCP frame relay endpoints (gbfs.McpServer の FrameRelay が利用)
    app.MapPost("/api/mcp/frame", fun (ctx: HttpContext) ->
        task {
            use ms = new System.IO.MemoryStream()
            do! ctx.Request.Body.CopyToAsync(ms)
            let data = ms.ToArray()
            if data.Length >= 160 * 144 then
                McpFrameStore.write data
            ctx.Response.StatusCode <- 204
        } :> System.Threading.Tasks.Task
    ) |> ignore

    app.MapGet("/api/mcp/frame", fun (ctx: HttpContext) ->
        task {
            match McpFrameStore.read() with
            | Some data ->
                ctx.Response.ContentType <- "application/octet-stream"
                do! ctx.Response.Body.WriteAsync(data, 0, data.Length)
            | None -> ctx.Response.StatusCode <- 204
        } :> System.Threading.Tasks.Task
    ) |> ignore

    app.Run()
    0
