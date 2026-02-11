module gbfs.Server.Program

open System.Threading
open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Bolero
open Bolero.Server
open gbfs
open Bolero.Templating.Server

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

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    builder.Services.AddRazorComponents()
        .AddInteractiveServerComponents()
        .AddInteractiveWebAssemblyComponents()
    |> ignore
    builder.Services.AddServerSideBlazor() |> ignore
    builder.Services.AddBoleroComponents() |> ignore
#if DEBUG
    builder.Services.AddHotReload(templateDir = __SOURCE_DIRECTORY__ + "/../gbfs.Client") |> ignore
#endif

    let app = builder.Build()

    if app.Environment.IsDevelopment() then
        app.UseWebAssemblyDebugging()

    app
        .UseStaticFiles()
        .UseRouting()
        .UseAntiforgery()
    |> ignore

#if DEBUG
    app.UseHotReload()
#endif

    // MCP frame relay endpoints
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
            | None ->
                ctx.Response.StatusCode <- 204
        } :> System.Threading.Tasks.Task
    ) |> ignore

    app.MapRazorComponents<Index.Page>()
        .AddInteractiveServerRenderMode()
        .AddInteractiveWebAssemblyRenderMode()
        .AddAdditionalAssemblies(typeof<Client.Main.EmulatorApp>.Assembly)
    |> ignore

    app.Run()
    0
