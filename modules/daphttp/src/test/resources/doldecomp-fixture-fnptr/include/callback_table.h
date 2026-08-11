typedef struct CallbackTable {
    u8 id;
    void (*OnInit)(void);
    s32 (*OnUpdate)(s32 frame);
    void (*OnDestroy)(void);
} CallbackTable;
