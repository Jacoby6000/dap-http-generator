$version: "2"

namespace com.jacoby6000.daphttp

@trait(selector: ":is(structure)")
structure dapStruct {}

@trait(selector: ":is(structure, member)")
integer alignment

@trait(selector: ":is(member)")
integer cString

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
