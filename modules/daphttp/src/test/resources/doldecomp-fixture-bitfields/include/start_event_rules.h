typedef struct StartEventRules {
    u8 x0_0 : 1;
    u8 x0_1 : 1;
    u8 x0_2 : 1;
    u8 x0_3 : 1;
    u8 x0_4 : 1;
    u8 x0_5 : 1;
    u8 x0_6 : 1;
    u8 x0_7 : 1;
    u8 is_teams : 1;
    s8 x3;
    u16 x6;
} StartEventRules;

extern StartEventRules start_event_rules;
