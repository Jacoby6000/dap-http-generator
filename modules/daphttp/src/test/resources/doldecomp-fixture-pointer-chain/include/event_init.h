typedef struct EventInitDataLevelTbl {
    u8 kind;
    u8 flags;
    u8 pad2[2];
    union {
        int* extra_character_init_data;
        int coin_goal;
    };
    u32 rules;
} EventInitDataLevelTbl;

extern EventInitDataLevelTbl** event_init_data_level_table[2];
extern u8 event_match_selection_index_to_event_match_id_mapping[];
