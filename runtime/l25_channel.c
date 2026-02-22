/*
 * L25 Runtime — Channel (goroutine 间通信)
 *
 * 有界缓冲 channel，支持阻塞 send/recv。
 * 内部使用 mutex + condvar 保证线程安全。
 */
#include "l25_runtime.h"
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ===== Channel 内部结构 ===== */
typedef struct {
    void*           buffer;      /* 循环缓冲区 */
    int64_t         elem_size;   /* 元素字节大小 */
    int64_t         capacity;    /* 缓冲区容量 */
    int64_t         head;        /* 读位置 */
    int64_t         count;       /* 当前元素数量 */
    int             closed;      /* 是否已关闭 */
    pthread_mutex_t mutex;
    pthread_cond_t  not_full;    /* 缓冲区非满信号 */
    pthread_cond_t  not_empty;   /* 缓冲区非空信号 */
} Channel;

/* ===== 创建 ===== */
void* l25_channel_create(int64_t elem_size, int64_t capacity) {
    if (capacity <= 0) capacity = 1; /* 至少 1 个缓冲槽 */
    Channel* ch   = (Channel*)calloc(1, sizeof(Channel));
    ch->elem_size = elem_size;
    ch->capacity  = capacity;
    ch->buffer    = calloc(capacity, elem_size);
    ch->head      = 0;
    ch->count     = 0;
    ch->closed    = 0;
    pthread_mutex_init(&ch->mutex,     NULL);
    pthread_cond_init(&ch->not_full,   NULL);
    pthread_cond_init(&ch->not_empty,  NULL);
    return ch;
}

/* ===== 销毁 ===== */
void l25_channel_destroy(void* ptr) {
    if (!ptr) return;
    Channel* ch = (Channel*)ptr;
    pthread_mutex_destroy(&ch->mutex);
    pthread_cond_destroy(&ch->not_full);
    pthread_cond_destroy(&ch->not_empty);
    free(ch->buffer);
    free(ch);
}

/* ===== 发送（阻塞） ===== */
void l25_channel_send(void* ptr, const void* elem) {
    Channel* ch = (Channel*)ptr;
    pthread_mutex_lock(&ch->mutex);

    /* 等待缓冲区有空位 */
    while (ch->count == ch->capacity && !ch->closed) {
        pthread_cond_wait(&ch->not_full, &ch->mutex);
    }

    if (ch->closed) {
        pthread_mutex_unlock(&ch->mutex);
        fprintf(stderr, "l25 runtime error: send on closed channel\n");
        return;
    }

    int64_t tail = (ch->head + ch->count) % ch->capacity;
    memcpy((char*)ch->buffer + tail * ch->elem_size, elem, ch->elem_size);
    ch->count++;

    pthread_cond_signal(&ch->not_empty);
    pthread_mutex_unlock(&ch->mutex);
}

/* ===== 接收（阻塞） ===== */
void l25_channel_recv(void* ptr, void* out) {
    Channel* ch = (Channel*)ptr;
    pthread_mutex_lock(&ch->mutex);

    /* 等待缓冲区有数据 */
    while (ch->count == 0 && !ch->closed) {
        pthread_cond_wait(&ch->not_empty, &ch->mutex);
    }

    if (ch->count == 0 && ch->closed) {
        /* channel 已关闭且空：返回零值 */
        memset(out, 0, ch->elem_size);
        pthread_mutex_unlock(&ch->mutex);
        return;
    }

    memcpy(out, (char*)ch->buffer + ch->head * ch->elem_size, ch->elem_size);
    ch->head = (ch->head + 1) % ch->capacity;
    ch->count--;

    pthread_cond_signal(&ch->not_full);
    pthread_mutex_unlock(&ch->mutex);
}

/* ===== 缓冲区长度 ===== */
int64_t l25_channel_len(void* ptr) {
    Channel* ch = (Channel*)ptr;
    pthread_mutex_lock(&ch->mutex);
    int64_t len = ch->count;
    pthread_mutex_unlock(&ch->mutex);
    return len;
}

/* ===== 关闭 ===== */
void l25_channel_close(void* ptr) {
    Channel* ch = (Channel*)ptr;
    pthread_mutex_lock(&ch->mutex);
    ch->closed = 1;
    /* 唤醒所有等待者 */
    pthread_cond_broadcast(&ch->not_full);
    pthread_cond_broadcast(&ch->not_empty);
    pthread_mutex_unlock(&ch->mutex);
}
