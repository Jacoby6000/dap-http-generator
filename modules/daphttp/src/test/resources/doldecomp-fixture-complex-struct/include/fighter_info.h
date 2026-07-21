typedef struct Vec3 {
    f32 x;
    f32 y;
    f32 z;
} Vec3;

typedef struct Color {
    u8 r;
    u8 g;
    u8 b;
    u8 a;
} Color;

typedef struct FighterInfo {
    /* 0x00 */ char* name;
    /* 0x04 */ char* costumeNames[4];
    /* 0x14 */ u8 weight;
    /* 0x15 */ u8 speed;
    /* 0x16 */ u8 pad[2];
    /* 0x18 */ s16 wins;
    /* 0x1A */ s16 losses;
    /* 0x1C */ f32 gravity;
    /* 0x20 */ Vec3 spawnPos;
    /* 0x2C */ Color* colors[3];
    /* 0x38 */ u64 uniqueId;
} FighterInfo;
