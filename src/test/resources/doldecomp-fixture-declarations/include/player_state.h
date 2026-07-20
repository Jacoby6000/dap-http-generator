typedef struct Vec3f {
    f32 x;
    f32 y;
    f32 z;
} Vec3f;

typedef struct PlayerState {
    u32 health;
    u128 score;
    Vec3f position;
} PlayerState;
