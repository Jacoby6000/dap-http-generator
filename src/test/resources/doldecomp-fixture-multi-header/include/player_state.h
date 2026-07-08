#include "common_types.h"

typedef struct PlayerState {
    u32 health;
    Vec3f position;
    InventorySlot inventory[2];
    u8* scratch;
} PlayerState;
