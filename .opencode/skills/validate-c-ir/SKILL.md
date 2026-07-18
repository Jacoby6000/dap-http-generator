---
name: validate-c-ir
description: Validates the doldecomp C header + symbols to IR conversion (DoldecompIrGenerator) and its round-trip through the Smithy pipeline (SmithyIrEmitter / SmithyIrGenerator) using the Melee decompilation as the reference corpus. Use when verifying route generation, operation/structure naming, Smithy 2.0 @http trait emission, GET-only HTTP access, or that decoded JSON payloads map 1:1 to C structures. Triggers: "validate C to IR", "melee reference", "cheaders-smithy", "generated/melee.smithy", "operation routes match structures", "@http traits".
---

# Validate C -> IR conversion (Melee reference corpus)

This skill validates the **C headers + doldecomp symbols -> IR** pipeline by feeding it the
Super Smash Bros. Melee decompilation and checking the IR's two downstream outputs against a
fixed set of expectations. The generated Smithy model is the long-lived reference: future
reference implementations will be built against Smithy and the **Smithy -> IR** path, so a C
header run and a Smithy run must produce the same set of routes and the same decoded payloads.

## When to run this validation

Run this validation (or invoke `validate-melee.sh` in this skill directory) whenever you change:

- `src/main/scala/io/github/jacoby6000/daphttp/DoldecompIrGenerator.scala` (C -> IR)
- `src/main/scala/io/github/jacoby6000/daphttp/CHeaderParser.scala` or
  `CHeaderOffsetParser.scala` (C AST / offset comments)
- `src/main/scala/io/github/jacoby6000/daphttp/SmithyIrEmitter.scala` (IR -> Smithy)
- `src/main/scala/io/github/jacoby6000/daphttp/SmithyIrGenerator.scala` (Smithy -> IR)
- `src/main/scala/io/github/jacoby6000/daphttp/HttpRouteIrEmitter.scala` (IR -> route plans + codecs)
- `src/main/scala/io/github/jacoby6000/daphttp/DapHttpServerMain.scala` (HTTP route serving)
- `src/main/smithy/dap-http-traits.smithy` (custom trait definitions)

Also run it whenever you intend to verify the end-to-end expectations listed under
"Definition of done" below.

## Reference corpus

| Input            | Path                                                              |
| ---------------- | ----------------------------------------------------------------- |
| doldecomp symbols| `$HOME/projects/ai/yolo/melee/config/GALE01/symbols.txt`          |
| C headers / src  | `$HOME/projects/ai/yolo/melee/src`                                |
| Generated output| `<repo>/generated/melee.smithy`                                   |

Parameters: `--word-size 32 --namespace melee --service MasterHand`. Melee is GameCube / Wii
era PowerPC, big-endian, 32-bit words.

## Helper script

`validate-melee.sh` lives next to this `SKILL.md` and runs the whole workflow below. Run it
from the repo root:

```sh
.opencode/skills/validate-c-ir/validate-melee.sh
```

It prints a per-check PASS/FAIL summary and exits non-zero on any hard failure. `--strict`
turns the `@http`-trait and operation-naming checks from advisory into hard failures (use this
once the desired-behavior work in the next section is complete).

## Workflow

### Step 1 - Generate the reference Smithy model from melee C headers

```sh
sbt "run cheaders-smithy \
  --symbols $HOME/projects/ai/yolo/melee/config/GALE01/symbols.txt \
  --headers $HOME/projects/ai/yolo/melee/src \
  --word-size 32 \
  --namespace melee \
  --service MasterHand \
  --output generated/melee.smithy"
```

Expect: `generated/melee.smithy` written, validated by the Smithy assembler (no `Left(errors)`
from `SmithyIrEmitter.emit`). First `sbt` invocation may be slow (cold launcher / Coursier).

### Step 2 - Inspect the generated Smithy

Verify the definition-of-done criteria. Quick checks:

```sh
# Service + wordSize present
grep -E '^service MasterHand' generated/melee.smithy
grep -E '@com\.jacoby6000\.daphttp#wordSize\(32\)' generated/melee.smithy

# Count operations (one per statically accessible structure / .data symbol)
grep -cE '^operation ' generated/melee.smithy

# Confirm @http GET traits are attached to operations (desired end state)
grep -cE '@http\(method:\s*"GET"' generated/melee.smithy
```

### Step 3 - Serve routes directly from C headers + symbols

```sh
sbt "run cheaders \
  --symbols $HOME/projects/ai/yolo/melee/config/GALE01/symbols.txt \
  --headers $HOME/projects/ai/yolo/melee/src \
  --word-size 32 \
  --namespace melee \
  --service MasterHand \
  --bind-port 8080"
```

### Step 4 - Smoke the HTTP surface

```sh
curl -s http://127.0.0.1:8080/health         # {"status":"ok"}
curl -s http://127.0.0.1:8080/routes | jq .  # one route per statically accessible structure
curl -s http://127.0.0.1:8080/melee/MasterHand/<Operation> | jq .
```

Without a real debugger the DAP adapter is absent, so each data route still returns HTTP 200
but carries per-read `"error"` fields. `/health` and `/routes` work standalone - that is
enough to validate the **route set** (the structure-name expectations in the definition of
done).

### Step 5 - Round-trip via the Smithy pipeline (the reference path)

```sh
sbt "run smithy \
  --smithy generated/melee.smithy \
  --bind-port 8081"
```

Then compare route sets (the paths returned by `GET /routes` on port 8080 against port 8081).
**These must match.** If the C-header server and the Smithy server expose different routes, the
C -> IR pipeline has drifted from the Smithy -> IR pipeline.

### Step 6 - Verify decoded payloads map 1:1 to C structures (needs memory)

To exercise data routes end-to-end you must supply DAP `readMemory` responses. Two options:

1. **Real debugger** on `--dap-host/--dap-port` (default `127.0.0.1:4711`).
2. **Mock DAP TCP server** that speaks the `Content-Length: N\r\n\r\n{json}` framing and
   answers `readMemory` requests with `{"success":true,"body":{"data":"<base64>"}}`. A
   reference implementation is `DummyDapServer` in
   `src/test/scala/io/github/jacoby6000/daphttp/DapHttpIntegrationSpec.scala:26` - copy it
   and feed canned memory keyed by `(address, count)`.

For each route, issue a `GET` and assert the `decoded` JSON has one field per C struct member
with the right name, width, signedness, and endianness for that struct's layout. For pointer
chains, append numeric index segments (`GET /melee/MasterHand/<Op>/<i>/<j>`) and assert the
deref returns the pointee struct shape (or a C string when the pointee is `char`).

## Definition of done (expected end state)

These are the criteria this skill validates against. Several are **desired future state**, not
the current behavior - the script flags them as `ADVISORY` until the code is brought up.

1. **One route per statically accessible structure.** Every `.data` symbol in `symbols.txt`
   that resolves to a struct-typed (or primitive/pointer/array) global produces exactly one
   HTTP route. `GET /routes` lists them; nothing is silently dropped. Unresolvable symbols
   appear only as non-fatal IR warnings (see `IrSizingWarnings`) and *must not* fail the
   build.

2. **Operation routes match the names of the structures.** The operation name (and thus the
   route path segment) is the **structure name** from the C headers, not `Get<SymbolName>`.
   Today `DoldecompIrGenerator.scala:91` builds `operationName = "Get" + PascalCase(symbol)`
   - that is the gap to close.

3. **Standard Smithy 2.0 `@http` annotations.** Each operation carries
   `@http(method: "GET", uri: "/MasterHand/<structure-name>")` from the `smithy.api` prelude.
   Routes are declared on the model, not derived at runtime by string concatenation
   (`DoldecompIrGenerator.scala:232` / `SmithyIrGenerator.scala:187` are the derivation sites
   to retire). `SmithyIrEmitter.buildOperation` (`SmithyIrEmitter.scala:201`) is where the
   `@http` trait must be added when emitting IR -> Smithy.

4. **GET-only.** Only access operations. No POST/PUT/PATCH/DELETE for mutation - derived C
   structures are read-only snapshots of debugger memory. The `POST /resume` endpoint on the
   HTTP server is DAP-level control, not data mutation, and is unaffected.

5. **Decoded payloads map 1:1 to C structures, in JSON, served over HTTP.** Field names match
   the C struct member names; primitive widths and signedness match the C types (use
   `@u8/@s16/@f32/...` rather than ambiguous `Integer/Long/Float/Double` - see
   `IrSizingWarnings`); offsets from `/* 0xNN */` comments drive byte layout (`@staticAddress`
   on enclosing-struct members and `declaredSizeBits` on `@dapStruct`s); big-endian is the
   service default for Melee (the `@endian` trait is omitted, `@wordSize(32)` is required).
   Bitfields group into `@bitmask` shapes; `char[N]` decode as null-terminated ASCII strings;
   `char*` pointer chains set `followCString` and expose `/<index>` sub-routes.

6. **C -> IR and Smithy -> IR agree.** Loading `generated/melee.smithy` via the `smithy`
   subcommand produces the same route set and the same per-route decoded shape as loading the
   C headers via `cheaders`. The `IrEquivalence` test helper
   (`src/test/scala/io/github/jacoby6000/daphttp/IrEquivalence.scala`) and
   `IrSmithyRoundTripSpec` are the in-process oracles for this.

## Source map

| Concern                       | File:line                                                                 |
| ----------------------------- | ------------------------------------------------------------------------- |
| IR types (`IrService`, etc.)   | `src/main/scala/io/github/jacoby6000/daphttp/IrDefinitions.scala`         |
| Route plan runtime model      | `src/main/scala/io/github/jacoby6000/daphttp/RoutePlanningModel.scala`    |
| C -> IR                       | `src/main/scala/io/github/jacoby6000/daphttp/DoldecompIrGenerator.scala`  |
| C header AST parser           | `src/main/scala/io/github/jacoby6000/daphttp/CHeaderParser.scala`         |
| C `/* 0xNN */` offset parser  | `src/main/scala/io/github/jacoby6000/daphttp/CHeaderOffsetParser.scala`  |
| Smithy -> IR                  | `src/main/scala/io/github/jacoby6000/daphttp/SmithyIrGenerator.scala`     |
| IR -> Smithy                  | `src/main/scala/io/github/jacoby6000/daphttp/SmithyIrEmitter.scala`      |
| IR -> route plans + codecs    | `src/main/scala/io/github/jacoby6000/daphttp/HttpRouteIrEmitter.scala`   |
| HTTP serving + DAP client     | `src/main/scala/io/github/jacoby6000/daphttp/DapHttpServerMain.scala:120` |
| Pointer-chain deref           | `src/main/scala/io/github/jacoby6000/daphttp/PointerChainResolver.scala` |
| Custom Smithy traits          | `src/main/smithy/dap-http-traits.smithy`                                  |
| CLI subcommands               | `src/main/scala/io/github/jacoby6000/daphttp/Cli.scala`                   |
| Mock DAP TCP server pattern    | `src/test/scala/io/github/jacoby6000/daphttp/DapHttpIntegrationSpec.scala:26` |
| C-header integration tests    | `src/test/scala/io/github/jacoby6000/daphttp/DoldecompSmithyGeneratorIntegrationSpec.scala` |
| Codec tests                   | `src/test/scala/io/github/jacoby6000/daphttp/HttpRouteIrEmitterCodecSpec.scala` |
| Smithy round-trip oracle      | `src/test/scala/io/github/jacoby6000/daphttp/IrSmithyRoundTripSpec.scala` |
| IR equivalence helper         | `src/test/scala/io/github/jacoby6000/daphttp/IrEquivalence.scala`         |

## Notes / gotchas

- `scalafmtOnCompile := true`, so `sbt run` rewrites sources in place; do not be surprised by
  formatting churn.
- The HTTP server connects to the DAP adapter with a 1s TCP timeout, retrying every 5s. Data
  routes return `error` fields per read until a DAP connection is established; `/health` and
  `/routes` are independent of the DAP connection.
- Melee is big-endian; do not introduce little-endian defaults when adding `@http` / route
  traits to the emitter.
- `SmithyIrEmitter` must keep going through `SmithyIdlModelSerializer` and Smithy's assembler
  validation - do not hand-render Smithy IDL text.
