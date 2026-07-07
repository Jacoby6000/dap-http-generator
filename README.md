# dap-http-generator
Use smithy models to define read-only http servers that proxy debugger memory via DAP

## Runtime server jar (http4s)

This repository now builds a runnable jar with a `main` entrypoint:
`io.github.jacoby6000.daphttp.DapHttpServerMain`.

### Custom C/DAP traits

Traits are defined in `src/main/smithy/dap-http-traits.smithy`:

- `@dapStruct` marks structures that should be decoded from DAP bytes.
- `@bitmask` marks a structure as a bitmask.
- `@size(<n>)` sets an expected structure width (required for `@bitmask`).
- `@alignment(<n>)` sets struct/member byte alignment.
- `@padding(<n>)` applies repeated bit/byte padding to `Bits`/`Bytes` list members.
- `@pointer` marks a member as a pointer-sized field.
- `@array` marks list members as arrays.
- `@length(<n>)` sets array element count for non-pointer list members.
- `@staticAddress(<address>)` marks the debugger memory address used for members inside non-`@dapStruct` shapes.
- `@endian("big" | "little")` sets default service/member byte order (member overrides service default).
- `@wordSize(<n>)` is required on services and sets pointer/`Long` bit width.
- Model C strings as `@pointer` members targeting `@char`; decoding follows the pointer and reads ASCII bytes until a null terminator.
- Numeric member traits: `@u8`, `@s8`, `@u16`, `@s16`, `@u32`, `@s32`, `@u64`, `@s64`, `@f8`, `@f16`, `@f32`, `@f64`, `@char`.
- Convenience list types: `Bytes` (`list<Byte>`) and `Bits` (`list<Boolean>`).

### Runtime behavior

The server:
- loads Smithy source files,
- generates read-only GET routes from service operations (`/<ServiceName>/<OperationName>`),
- requires non-DAP output members to use `@staticAddress(...)`,
- resolves DAP-backed structs (`@dapStruct`/`@bitmask`) and reads memory through a DAP `readMemory` request,
- watches Smithy sources and reloads routes when model files change.

### Running locally

```bash
sbt "run --smithy=/absolute/path/to/models --dapHost=127.0.0.1 --dapPort=4711 --bindPort=8080 --watch=true"
```

### Formatting, linting, and tests

```bash
sbt fmt
sbt fix
sbt test
```
