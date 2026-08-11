typedef enum GameMode {
    MODE_MENU = 0,
    MODE_VS = 1,
    MODE_STORY = 2
} GameMode;

typedef struct GameState {
    /* 0x00 */ GameMode mode;
    /* 0x04 */ u32 frame;
} GameState;
