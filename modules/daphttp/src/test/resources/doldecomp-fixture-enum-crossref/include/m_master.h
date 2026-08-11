typedef enum ftMasterhand_MotionState {
    ftMh_MS_Wait = ftCo_MS_Count,
    ftMh_MS_Count,
    ftMh_MS_SelfCount = ftMh_MS_Count - ftCo_MS_Count
} ftMasterhand_MotionState;
