#include "event_init.h"

static struct EventInitDataLevelTbl** event_init_data_level_table[2];
static u8 event_match_selection_index_to_event_match_id_mapping[] = {
    0x00, 0x11, 0x02, 0x03, 0x04,
};
