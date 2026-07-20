# AGENTS.md

## Cursor Cloud specific instructions

This is a Scala 2.13 / sbt project (build tool: `sbt`, JDK 21). `sbt` is preinstalled in the
Cloud VM snapshot and dependencies are refreshed by the startup update script (`sbt update`).

### Services / entrypoint

There is a single service: a read-only HTTP server that proxies debugger memory over DAP.
The entrypoint is `io.github.jacoby6000.daphttp.Cli` (a Decline CLI) with three subcommands:

- `smithy` — load API definitions from Smithy model files (`--smithy <path>` repeatable, `--watch`).
- `cheaders` — load from C headers + a doldecomp symbols file (`--symbols`, `--headers` repeatable,
  `--namespace`, `--service`, `--word-size`, `--data-sections`) and run the HTTP server.
- `cheaders-smithy` — generate a Smithy model from C headers + symbols (`--symbols`, `--headers`,
  `--namespace`, `--service`, `--word-size`, `--data-sections`, `--output`).

`smithy` and `cheaders` share `--dap-host/--dap-port` (default `127.0.0.1:4711`) and
`--bind-host/--bind-port` (default `0.0.0.0:8080`). Run with `sbt "run <subcommand> ..."`.
Standard build/lint/test commands are documented in `README.md` and `.github/workflows/ci.yml`
(`sbt fmt`, `sbt fix`, `sbt test`, and CI's `scalafmtCheckAll;scalafmtSbtCheck` /
`scalafixAll --check`).

### IR pipeline

Input formats converge on a shared IR, then emit HTTP routes or Smithy models:

```mermaid
---
config:
  flowchart:
    curve:
---

flowchart LR
  SMIN["Smithy Models"]

  subgraph Inputs
    direction TD
    subgraph smithyIn [Smithy]
        SMIN --> SmithyIrGenerator
    end
    subgraph C
        CHeaders["C headers symbols"] --> DoldecompIrGenerator
    end
  end

  IR["Intermediate Representation"]

  SmithyIrGenerator --> IR
  DoldecompIrGenerator --> IR

  IR --> HttpRouteIrEmitter
  IR --> SmithyIrEmitter

  subgraph Outputs
    subgraph http
        HttpRouteIrEmitter --> Routes["HTTP routes"]
    end
    subgraph smithyOut [Smithy]
        SmithyIrEmitter --> SMOUT["Smithy Models"]
    end
  end
```

`SmithyIrEmitter` builds a Smithy `Model` with smithy-model shape builders and serializes it via
`SmithyIdlModelSerializer` (do not hand-render Smithy IDL text).

### Modules

- **root (JVM)** — CLI, IR pipeline, http4s server (`io.github.jacoby6000.daphttp.Cli`).
- **ui (Scala.js)** — browser explorer under `ui/`; `Compile / resourceGenerators` copies
  `fastOptJS` + `index.html` into `web/` resources served at `/` and `/assets/main.js`.

### HTTP surface

| Path | Purpose |
|------|---------|
| `GET /` | HTML + Scala.js route explorer |
| `GET /assets/main.js` | Packaged Scala.js bundle |
| `GET /health` | Liveness |
| `GET /routes` | Flat `routes` list + `tree` (for the UI) + `errors` |
| `POST /resume` | DAP `continue` |
| `GET /api/...` | Generated memory / data routes |

### Non-obvious caveats

- `scalafmtOnCompile := true`, so `sbt compile` will reformat sources in place.
- Generated data routes are always under `/api` (`ApiRoutes.normalize`). Meta UI endpoints stay at
  the root so they never collide with a Smithy service named `api`.
- The `/health` and `/routes` endpoints work without a debugger. On startup the server immediately
  tries to connect to the DAP adapter on `--dap-port` (1s TCP timeout per attempt, retrying every
  5s until connected). Generated **data** routes reuse that persistent TCP connection (serialized
  across concurrent requests); if the adapter is not listening they return per-read `error` fields
  (the HTTP request still succeeds). To exercise data routes locally without a real debugger, run a
  small mock TCP server that speaks the DAP `readMemory` framing (`Content-Length: N\r\n\r\n` +
  JSON body, responding with `{"success":true,"body":{"data":"<base64>"}}`).
- `IrSizingWarnings` logs non-fatal warnings to stderr when IR members use ambiguous Smithy
  prelude types (`Integer`, `Long`, `Float`, `Double`) without explicit width traits (`@u32`,
  `@f64`, etc.). Pointer members are excluded.
- C `enum` / Smithy `intEnum` values decode to the enumerator name in JSON. Values that do not
  match any enumerator decode as a hex literal (`0xN`), not a numeric type. Enumerator
  initializers, array bounds, and bitfield widths are evaluated with CDT `ValueFactory` after
  preprocessor expansion — never by hand-parsing `#define` text. Unevaluable explicit enum
  initializers warn instead of silently inventing sequential values.
- Decoded struct JSON includes `"_address": "0x…"` on every struct object (root, nested members,
  and array elements of structs), including pointees from pointer routes. Top-level response
  envelopes no longer carry a separate `address`/`offset` field for the decoded value;
  `pointerAddress` on pointer sub-routes still names where the pointer slot itself was read.
- Member `@size(N)` (also structure `@size`) round-trips doldecomp symbol `size:` as
  `IrMember.readSizeBytes` through `cheaders-smithy` → `smithy`. Structure `@size` still means
  declared structure width; on members it is the explicit DAP read width.
- Global arrays (including pointer arrays) use `readSizeBytes / length` as element stride when that
  exceeds packed layout/pointer width (padding between elements). Pointer-chain outer indices use
  the same stride. Unsized aggregate arrays require a C declarator bound or initializer count:
  symbol size alone cannot distinguish element count from ABI stride padding. Object symbols in
  code sections (`.text`, etc.) and data symbols without `ctype`/C declarations emit per-symbol
  warnings rather than failing silently.
- Duplicate C globals for one symbol name merge deterministically: prefer non-`static` declarations
  with array length metadata; take `pointerDepth` from that primary (not `max`). Conflicting macros,
  structs, typedefs, and enums across scanned files keep the first definition and emit a warning.
  `cheaders-smithy` fails when the service has zero operations.
- First `sbt` invocation downloads sbt/Scala launchers and Coursier deps; expect a slow cold start.
  Building the server also builds the Scala.js UI via `resourceGenerators`.
