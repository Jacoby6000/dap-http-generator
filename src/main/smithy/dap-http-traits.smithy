$version: "2"

namespace com.jacoby6000.daphttp

@trait(selector: ":is(structure)")
structure dapStruct {}

@trait(selector: ":is(structure)")
structure bitmask {}

@trait(selector: ":is(structure)")
integer size

@trait(selector: ":is(structure, member)")
integer alignment

@trait(selector: ":is(member)")
integer padding

@trait(selector: ":is(member)")
structure pointer {}

@trait(selector: ":is(member)")
structure array {}

@trait(selector: ":is(member)")
integer length

@trait(selector: ":is(member)")
string staticAddress

@trait(selector: ":is(service, member)")
enum endian {
    BIG = "big"
    LITTLE = "little"
}

@trait(selector: ":is(service)")
integer wordSize

@trait(selector: ":is(member)")
structure u8 {}

@trait(selector: ":is(member)")
structure s8 {}

@trait(selector: ":is(member)")
structure u16 {}

@trait(selector: ":is(member)")
structure s16 {}

@trait(selector: ":is(member)")
structure u32 {}

@trait(selector: ":is(member)")
structure s32 {}

@trait(selector: ":is(member)")
structure u64 {}

@trait(selector: ":is(member)")
structure s64 {}

@trait(selector: ":is(member)")
structure u128 {}

@trait(selector: ":is(member)")
structure s128 {}

@trait(selector: ":is(member)")
structure f8 {}

@trait(selector: ":is(member)")
structure f16 {}

@trait(selector: ":is(member)")
structure f32 {}

@trait(selector: ":is(member)")
structure f64 {}

@trait(selector: ":is(member)")
structure char {}

list Bytes {
    member: Byte
}

list Bits {
    member: Boolean
}
