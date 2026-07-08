# AGENTS.md

## Cursor Cloud specific instructions

This is a Scala 2.13 / sbt project (build tool: `sbt`, JDK 21). `sbt` is preinstalled in the
Cloud VM snapshot and dependencies are refreshed by the startup update script (`sbt update`).

### Services / entrypoint

There is a single service: a read-only HTTP server that proxies debugger memory over DAP.
The entrypoint is `io.github.jacoby6000.daphttp.Cli` (a Decline CLI) with two subcommands:

- `smithy` — load API definitions from Smithy model files (`--smithy <path>` repeatable, `--watch`).
- `cheaders` — load from C headers + a doldecomp symbols file (`--symbols`, `--headers` repeatable,
  `--namespace`, `--service`, `--word-size`).

Both share `--dap-host/--dap-port` (default `127.0.0.1:4711`) and `--bind-host/--bind-port`
(default `0.0.0.0:8080`). Run with `sbt "run <subcommand> ..."`. Standard build/lint/test commands
are documented in `README.md` and `.github/workflows/ci.yml` (`sbt fmt`, `sbt fix`, `sbt test`,
and CI's `scalafmtCheckAll;scalafmtSbtCheck` / `scalafixAll --check`).

### Non-obvious caveats

- `scalafmtOnCompile := true`, so `sbt compile` will reformat sources in place.
- The `/health` and `/routes` endpoints work without a debugger. Generated **data** routes
  (e.g. `/DolDecompApi/GetGPlayerState`) open a fresh TCP socket per read to a DAP adapter on
  `--dap-port`; with no adapter listening they return per-read `error` fields (the HTTP request
  still succeeds). To exercise data routes locally without a real debugger, run a small mock TCP
  server that speaks the DAP `readMemory` framing (`Content-Length: N\r\n\r\n` + JSON body,
  responding with `{"success":true,"body":{"data":"<base64>"}}`).
- First `sbt` invocation downloads sbt/Scala launchers and Coursier deps; expect a slow cold start.
