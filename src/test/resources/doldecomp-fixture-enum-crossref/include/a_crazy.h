typedef enum ftCrazyhand_MotionState {
    ftCh_MS_Count = ftMh_MS_Count - 1,
    ftCh_MS_SelfCount = ftMh_MS_SelfCount - 1
} ftCrazyhand_MotionState;

typedef struct MotionHolder {
    ftCrazyhand_MotionState state;
} MotionHolder;

MotionHolder gMotionHolder;
