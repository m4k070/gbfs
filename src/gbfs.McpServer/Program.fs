open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open ModelContextProtocol.Server

[<EntryPoint>]
let main args =
    let builder = Host.CreateApplicationBuilder(args)

    // Keep stdout clean for MCP protocol - only warnings+ to stderr
    builder.Logging.ClearProviders() |> ignore
    builder.Logging.SetMinimumLevel(LogLevel.Warning) |> ignore
    builder.Logging.AddConsole(fun opts ->
        opts.LogToStandardErrorThreshold <- LogLevel.Trace
    ) |> ignore

    builder.Services
        .AddMcpServer()
        .WithStdioServerTransport()
        .WithToolsFromAssembly()
    |> ignore

    builder.Build().Run()
    0
