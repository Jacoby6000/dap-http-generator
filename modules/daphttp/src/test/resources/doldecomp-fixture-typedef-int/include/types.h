typedef int enum_t;
typedef int MessageBufferID;

typedef struct Holder {
    enum_t kind;
    MessageBufferID bufferId;
    int rawInt;
} Holder;

Holder gHolder;
