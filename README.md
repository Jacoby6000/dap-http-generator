# dap-http-generator
Use smithy models to define http servers that allow structured viewing and modification of program memory via DAP

## Smithy 2.0 Scala/SBT plugin

This repository now contains an SBT-based Smithy 2.0 build plugin: `dap-http-generator`.

### Custom C/DAP traits

Traits are defined in `src/main/smithy/dap-http-traits.smithy`:

- `@dapStruct` marks structures that should be decoded from DAP bytes.
- `@bitmask` marks a structure as a bitmask.
- `@size(<n>)` sets an expected structure width (required for `@bitmask`).
- `@alignment(<n>)` sets struct/member byte alignment.
- `@padding(<n>)` applies repeated bit/byte padding to `Bits`/`Bytes` list members.
- `@endian("big" | "little")` sets default service/member byte order (member overrides service default).
- `@cString(bytes: <n>, encoding: <name>)` marks fixed-size C-style string members; encoding defaults to `ASCII`.
- Numeric member traits: `@u8`, `@s8`, `@u16`, `@s16`, `@u32`, `@s32`.
- Convenience list types: `Bytes` (`list<Byte>`) and `Bits` (`list<Boolean>`).

### Plugin output

The plugin scans Smithy models for `@dapStruct`/`@bitmask` structures and writes a manifest describing members, C-focused types, size/alignment, C-string encodings, padding, endianness, and validation diagnostics.

Default output path: `dap-http/struct-manifest.json` (configurable with `outputFile` plugin setting).
