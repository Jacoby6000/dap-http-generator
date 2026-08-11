typedef struct GameSceneInfo {
    u8 class_id;
    void* load_data;
    void* leave_data;
} GameSceneInfo;

typedef struct GameScene {
    u8 idx;
    u8 preload;
    u16 flags;
    void (*Prep)(GameScene*);
    void (*Decide)(GameScene*);
    GameSceneInfo info;
} GameScene;
