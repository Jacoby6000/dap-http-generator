$version: "2"

namespace com.jacoby6000.daphttp

use smithy.api#default
use smithy.api#required

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

@trait(selector: ":is(service, member)")
enum endian {
    BIG = "big"
    LITTLE = "little"
}

@trait(selector: ":is(member)")
structure cString {
    @required
    bytes: Integer

    @default("ASCII")
    encoding: String
}

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

list Bytes {
    member: Byte
}

list Bits {
    member: Boolean
}
