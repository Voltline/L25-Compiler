#include "l25_runtime.h"
#include <stdlib.h>
#include <string.h>

/*
 * L25 Deque — 基于环形缓冲区（circular buffer）实现
 * 满足双端 O(1) 摊还插入/删除，O(1) 随机访问
 *
 * 布局:
 *   [  ...  | head ... tail |  ...  ]
 *   head 指向第一个元素的位置
 *   tail 指向最后一个元素的下一个位置
 *   size 维护元素计数
 *   扩容时 capacity 翻倍并重排
 */

#define DEQUE_INIT_CAP 8

typedef struct {
    void*   data;
    int64_t head;       /* 环形缓冲区的头索引 */
    int64_t tail;       /* 环形缓冲区的尾索引（指向下一个可写位置） */
    int64_t size;
    int64_t capacity;
    int64_t elem_size;
} L25Deque;

/* ---------- 内部辅助 ---------- */

static inline void* deque_slot(L25Deque* d, int64_t logical_idx) {
    int64_t phys = (d->head + logical_idx) % d->capacity;
    return (char*)d->data + phys * d->elem_size;
}

static void deque_grow(L25Deque* d) {
    int64_t new_cap = d->capacity * 2;
    void* new_data = calloc((size_t)new_cap, (size_t)d->elem_size);

    /* 将旧环形缓冲区线性化拷贝到新缓冲区 */
    for (int64_t i = 0; i < d->size; i++) {
        int64_t phys = (d->head + i) % d->capacity;
        memcpy((char*)new_data + i * d->elem_size,
               (char*)d->data + phys * d->elem_size,
               (size_t)d->elem_size);
    }

    free(d->data);
    d->data     = new_data;
    d->head     = 0;
    d->tail     = d->size;
    d->capacity = new_cap;
}

/* ---------- create / destroy ---------- */

void* l25_deque_create(int64_t elem_size) {
    L25Deque* d = (L25Deque*)malloc(sizeof(L25Deque));
    d->elem_size = elem_size;
    d->size      = 0;
    d->head      = 0;
    d->tail      = 0;
    d->capacity  = DEQUE_INIT_CAP;
    d->data      = calloc((size_t)DEQUE_INIT_CAP, (size_t)elem_size);
    return d;
}

void l25_deque_destroy(void* deq) {
    if (!deq) return;
    L25Deque* d = (L25Deque*)deq;
    free(d->data);
    free(d);
}

/* ---------- push_front / push_back ---------- */

void l25_deque_push_front(void* deq, const void* elem) {
    L25Deque* d = (L25Deque*)deq;
    if (d->size >= d->capacity) deque_grow(d);
    d->head = (d->head - 1 + d->capacity) % d->capacity;
    memcpy((char*)d->data + d->head * d->elem_size, elem, (size_t)d->elem_size);
    d->size++;
}

void l25_deque_push_back(void* deq, const void* elem) {
    L25Deque* d = (L25Deque*)deq;
    if (d->size >= d->capacity) deque_grow(d);
    memcpy((char*)d->data + d->tail * d->elem_size, elem, (size_t)d->elem_size);
    d->tail = (d->tail + 1) % d->capacity;
    d->size++;
}

/* ---------- pop_front / pop_back ---------- */

void l25_deque_pop_front(void* deq, void* out_elem) {
    L25Deque* d = (L25Deque*)deq;
    if (d->size <= 0) return;
    memcpy(out_elem, (char*)d->data + d->head * d->elem_size, (size_t)d->elem_size);
    d->head = (d->head + 1) % d->capacity;
    d->size--;
}

void l25_deque_pop_back(void* deq, void* out_elem) {
    L25Deque* d = (L25Deque*)deq;
    if (d->size <= 0) return;
    d->tail = (d->tail - 1 + d->capacity) % d->capacity;
    memcpy(out_elem, (char*)d->data + d->tail * d->elem_size, (size_t)d->elem_size);
    d->size--;
}

/* ---------- get / set (随机访问 O(1)) ---------- */

void* l25_deque_get(void* deq, int64_t index) {
    L25Deque* d = (L25Deque*)deq;
    return deque_slot(d, index);
}

void l25_deque_set(void* deq, int64_t index, const void* elem) {
    L25Deque* d = (L25Deque*)deq;
    memcpy(deque_slot(d, index), elem, (size_t)d->elem_size);
}

/* ---------- front / back ---------- */

void* l25_deque_front(void* deq) {
    L25Deque* d = (L25Deque*)deq;
    return (char*)d->data + d->head * d->elem_size;
}

void* l25_deque_back(void* deq) {
    L25Deque* d = (L25Deque*)deq;
    int64_t last = (d->tail - 1 + d->capacity) % d->capacity;
    return (char*)d->data + last * d->elem_size;
}

/* ---------- length ---------- */

int64_t l25_deque_len(void* deq) {
    L25Deque* d = (L25Deque*)deq;
    return d->size;
}
