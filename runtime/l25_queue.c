#include "l25_runtime.h"
#include <stdlib.h>
#include <string.h>

/*
 * L25 Queue — 基于环形缓冲区实现的 FIFO 队列
 * push (入队尾) O(1), pop (出队首) O(1), front (查看队首) O(1)
 */

#define QUEUE_INIT_CAP 8

typedef struct {
    void*   data;
    int64_t head;       /* 队首索引 */
    int64_t tail;       /* 队尾下一个可写位置 */
    int64_t size;
    int64_t capacity;
    int64_t elem_size;
} L25Queue;

/* ---------- 内部辅助 ---------- */

static void queue_grow(L25Queue* q) {
    int64_t new_cap = q->capacity * 2;
    void* new_data = calloc((size_t)new_cap, (size_t)q->elem_size);

    for (int64_t i = 0; i < q->size; i++) {
        int64_t phys = (q->head + i) % q->capacity;
        memcpy((char*)new_data + i * q->elem_size,
               (char*)q->data + phys * q->elem_size,
               (size_t)q->elem_size);
    }

    free(q->data);
    q->data     = new_data;
    q->head     = 0;
    q->tail     = q->size;
    q->capacity = new_cap;
}

/* ---------- create / destroy ---------- */

void* l25_queue_create(int64_t elem_size) {
    L25Queue* q = (L25Queue*)malloc(sizeof(L25Queue));
    q->elem_size = elem_size;
    q->size      = 0;
    q->head      = 0;
    q->tail      = 0;
    q->capacity  = QUEUE_INIT_CAP;
    q->data      = calloc((size_t)QUEUE_INIT_CAP, (size_t)elem_size);
    return q;
}

void l25_queue_destroy(void* que) {
    if (!que) return;
    L25Queue* q = (L25Queue*)que;
    free(q->data);
    free(q);
}

/* ---------- push (入队尾) ---------- */

void l25_queue_push(void* que, const void* elem) {
    L25Queue* q = (L25Queue*)que;
    if (q->size >= q->capacity) queue_grow(q);
    memcpy((char*)q->data + q->tail * q->elem_size, elem, (size_t)q->elem_size);
    q->tail = (q->tail + 1) % q->capacity;
    q->size++;
}

/* ---------- pop (出队首) ---------- */

void l25_queue_pop(void* que, void* out_elem) {
    L25Queue* q = (L25Queue*)que;
    if (q->size <= 0) return;
    memcpy(out_elem, (char*)q->data + q->head * q->elem_size, (size_t)q->elem_size);
    q->head = (q->head + 1) % q->capacity;
    q->size--;
}

/* ---------- front (查看队首，不弹出) ---------- */

void* l25_queue_front(void* que) {
    L25Queue* q = (L25Queue*)que;
    return (char*)q->data + q->head * q->elem_size;
}

/* ---------- back (查看队尾，不弹出) ---------- */

void* l25_queue_back(void* que) {
    L25Queue* q = (L25Queue*)que;
    int64_t last = (q->tail - 1 + q->capacity) % q->capacity;
    return (char*)q->data + last * q->elem_size;
}

/* ---------- length ---------- */

int64_t l25_queue_len(void* que) {
    L25Queue* q = (L25Queue*)que;
    return q->size;
}
