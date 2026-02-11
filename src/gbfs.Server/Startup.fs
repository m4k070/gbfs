module gbfs.Server.Program

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Bolero
open Bolero.Server
open gbfs
open Bolero.Templating.Server

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
    app.MapRazorComponents<Index.Page>()
        .AddInteractiveServerRenderMode()
        .AddInteractiveWebAssemblyRenderMode()
        .AddAdditionalAssemblies(typeof<Client.Main.EmulatorApp>.Assembly)
    |> ignore

    app.Run()
    0
