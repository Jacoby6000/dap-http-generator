# dap-http-generator
Use smithy models to define http servers that allow structured viewing and modification of program memory via DAP

## Smithy 2.0 Scala/SBT plugin

This repository now contains an SBT-based Smithy 2.0 build plugin: `dap-http-generator`.

### Custom C/DAP traits

Traits are defined in `src/main/smithy/dap-http-traits.smithy`:

- `@dapStruct` marks structures that should be decoded from DAP bytes.
- `@alignment(<n>)` sets struct/member byte alignment.
- `@cString(<bytes>)` marks fixed-size C-style string members.
- Numeric member traits: `@u8`, `@s8`, `@u16`, `@s16`, `@u32`, `@s32`.

### Plugin output

The plugin scans Smithy models for `@dapStruct` structures and writes a manifest describing members, C-focused types, alignment, and C-string lengths. This manifest is designed to be consumed by an HTTP server generator/decoder for DAP-provided data.

Default output path: `dap-http/struct-manifest.json` (configurable with `outputFile` plugin setting).
