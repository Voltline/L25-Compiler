/*
 * L25 Runtime — Channel (goroutine 间通信)
 *
 * 支持两种模式：
 *   - 无缓冲 channel (capacity=0)：同步 rendezvous，发送者阻塞直到接收者到来
 *   - 有缓冲 channel (capacity>0)：异步有界缓冲队列
 *
 * Go 语义对齐：
 *   - 向已关闭 channel 发送 / 重复关闭 → abort (panic)
 *   - 从已关闭且空的 channel 接收 → 返回零值 + ok=false
 *   - ch.closed() → 查询关闭状态
 */
#include "l25_runtime.h"
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ===== Channel 内部结构 ===== */
typedef struct {
    void*           buffer;      /* 循环缓冲区（有缓冲模式） */
    int64_t         elem_size;   /* 元素字节大小 */
    int64_t         capacity;    /* 缓冲区容量（0 = 无缓冲） */
    int64_t         head;        /* 读位置 */
    int64_t         count;       /* 当前元素数量 */
    int             closed;      /* 是否已关闭 */
    pthread_mutex_t mutex;
    pthread_cond_t  not_full;    /* 缓冲区非满 / rendezvous 发送端信号 */
    pthread_cond_t  not_empty;   /* 缓冲区非空 / rendezvous 接收端信号 */

    /* ===== 无缓冲 rendezvous 专用字段 ===== */
    const void*     rendezvous_data;   /* 发送者放置数据的指针 */
    int             rendezvous_ready;  /* 1 = 发送者已就绪 */
    int             rendezvous_taken;  /* 1 = 接收者已取走数据 */
    pthread_cond_t  rendezvous_done;   /* 通知发送者：接收者已取走 */
} Channel;

/* ===== 创建 ===== */
void* l25_channel_create(int64_t elem_size, int64_t capacity) {
    if (capacity < 0) capacity = 0;
    Channel* ch   = (Channel*)calloc(1, sizeof(Channel));
    ch->elem_size = elem_size;
    ch->capacity  = capacity;
    if (capacity > 0) {
        ch->buffer = calloc(capacity, elem_size);
    } else {
        ch->buffer = NULL;  /* 无缓冲模式不需要 buffer */
    }
    ch->head      = 0;
    ch->count     = 0;
    ch->closed    = 0;
    ch->rendezvous_data  = NULL;
    ch->rendezvous_ready = 0;
    ch->rendezvous_taken = 0;
    pthread_mutex_init(&ch->mutex,           NULL);
    pthread_cond_init(&ch->not_full,         NULL);
    pthread_cond_init(&ch->not_empty,        NULL);
    pthread_cond_init(&ch->rendezvous_done,  NULL);
    return ch;
}

/* ===== 销毁 ===== */
void l25_channel_destroy(void* ptr) {
    if (!ptr) return;
    Channel* ch = (Channel*)ptr;
    pthread_mutex_destroy(&ch->mutex);
    pthread_cond_destroy(&ch->not_full);
    pthread_cond_destroy(&ch->not_empty);
    pthread_cond_destroy(&ch->rendezvous_done);
    free(ch->buffer);
    free(ch);
}

/* ===== 发送（阻塞） ===== */
void l25_channel_send(void* ptr, const void* elem) {
    Channel* ch = (Channel*)ptr;
    pthread_mutex_lock(&ch->mutex);

    /* Go 语义：向已关闭 channel 发送 → panic */
    if (ch->closed) {
        fprintf(stderr, "l25 runtime panic: send on closed channel\n");
        abort();
    }

    if (ch->capacity == 0) {
        /* ===== 无缓冲 rendezvous 模式 =====
         * 发送者等待前一个 rendezvous 完成，然后放置数据并阻塞直到接收者取走。
         */
        /* 等待上一个 rendezvous 完成 */
        while (ch->rendezvous_ready && !ch->closed) {
            pthread_cond_wait(&ch->not_full, &ch->mutex);
        }
        if (ch->closed) {
            fprintf(stderr, "l25 runtime panic: send on closed channel\n");
            abort();
        }
        ch->rendezvous_data  = elem;
        ch->rendezvous_ready = 1;
        ch->rendezvous_taken = 0;

        /* 唤醒等待的接收者 */
        pthread_cond_signal(&ch->not_empty);

        /* 阻塞直到接收者取走数据 */
        while (!ch->rendezvous_taken && !ch->closed) {
            pthread_cond_wait(&ch->rendezvous_done, &ch->mutex);
        }
        /* recv 已清除 rendezvous_ready/data，通知下一个等待的发送者 */
        pthread_cond_signal(&ch->not_full);

        pthread_mutex_unlock(&ch->mutex);
    } else {
        /* ===== 有缓冲模式 ===== */
        while (ch->count == ch->capacity && !ch->closed) {
            pthread_cond_wait(&ch->not_full, &ch->mutex);
        }
        if (ch->closed) {
            fprintf(stderr, "l25 runtime panic: send on closed channel\n");
            abort();
        }

        int64_t tail = (ch->head + ch->count) % ch->capacity;
        memcpy((char*)ch->buffer + tail * ch->elem_size, elem, ch->elem_size);
        ch->count++;

        pthread_cond_signal(&ch->not_empty);
        pthread_mutex_unlock(&ch->mutex);
    }
}

/* ===== 接收（阻塞），返回 ok 标志 ===== */
int32_t l25_channel_recv_ok(void* ptr, void* out) {
    Channel* ch = (Channel*)ptr;
    pthread_mutex_lock(&ch->mutex);

    if (ch->capacity == 0) {
        /* ===== 无缓冲 rendezvous 模式 ===== */
        while (!ch->rendezvous_ready && !ch->closed) {
            pthread_cond_wait(&ch->not_empty, &ch->mutex);
        }
        if (!ch->rendezvous_ready && ch->closed) {
            memset(out, 0, ch->elem_size);
            pthread_mutex_unlock(&ch->mutex);
            return 0; /* ok = false */
        }
        /* 从发送者处拷贝数据 */
        memcpy(out, ch->rendezvous_data, ch->elem_size);
        /* 先清除 ready 标志，防止后续 recv 看到旧的 ready=1 */
        ch->rendezvous_ready = 0;
        ch->rendezvous_data  = NULL;
        ch->rendezvous_taken = 1;
        /* 唤醒发送者 */
        pthread_cond_signal(&ch->rendezvous_done);
        pthread_mutex_unlock(&ch->mutex);
        return 1; /* ok = true */
    } else {
        /* ===== 有缓冲模式 ===== */
        while (ch->count == 0 && !ch->closed) {
            pthread_cond_wait(&ch->not_empty, &ch->mutex);
        }
        if (ch->count == 0 && ch->closed) {
            memset(out, 0, ch->elem_size);
            pthread_mutex_unlock(&ch->mutex);
            return 0; /* ok = false */
        }

        memcpy(out, (char*)ch->buffer + ch->head * ch->elem_size, ch->elem_size);
        ch->head = (ch->head + 1) % ch->capacity;
        ch->count--;

        pthread_cond_signal(&ch->not_full);
        pthread_mutex_unlock(&ch->mutex);
        return 1; /* ok = true */
    }
}

/* ===== 接收（阻塞，兼容旧接口） ===== */
void l25_channel_recv(void* ptr, void* out) {
    l25_channel_recv_ok(ptr, out);
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

    /* Go 语义：重复关闭 → panic */
    if (ch->closed) {
        fprintf(stderr, "l25 runtime panic: close of closed channel\n");
        abort();
    }

    ch->closed = 1;
    /* 唤醒所有等待者 */
    pthread_cond_broadcast(&ch->not_full);
    pthread_cond_broadcast(&ch->not_empty);
    pthread_cond_broadcast(&ch->rendezvous_done);
    pthread_mutex_unlock(&ch->mutex);
}

/* ===== 查询是否已关闭 ===== */
int32_t l25_channel_closed(void* ptr) {
    Channel* ch = (Channel*)ptr;
    pthread_mutex_lock(&ch->mutex);
    int32_t c = ch->closed;
    pthread_mutex_unlock(&ch->mutex);
    return c;
}

/* ===== 非阻塞尝试发送（用于 select） =====
 * 返回: 1 = 发送成功, 0 = 无法立即完成 / 已关闭
 */
int32_t l25_channel_try_send(void* ptr, const void* elem) {
    Channel* ch = (Channel*)ptr;
    pthread_mutex_lock(&ch->mutex);

    if (ch->closed) {
        pthread_mutex_unlock(&ch->mutex);
        return 0;
    }

    if (ch->capacity == 0) {
        /* 无缓冲模式：需要接收者已在等待（即没有其他发送者 ready） */
        /* 非阻塞无法实现 rendezvous，只在没有 pending sender 时尝试 */
        if (ch->rendezvous_ready) {
            pthread_mutex_unlock(&ch->mutex);
            return 0; /* 另一个发送者正在等待 */
        }
        /* 即使没有 pending sender，我们也无法保证接收者在等，
         * 所以无缓冲 channel 的非阻塞发送总是失败 */
        pthread_mutex_unlock(&ch->mutex);
        return 0;
    } else {
        /* 有缓冲模式：缓冲区有空间则发送 */
        if (ch->count >= ch->capacity) {
            pthread_mutex_unlock(&ch->mutex);
            return 0;
        }
        int64_t tail = (ch->head + ch->count) % ch->capacity;
        memcpy((char*)ch->buffer + tail * ch->elem_size, elem, ch->elem_size);
        ch->count++;
        pthread_cond_signal(&ch->not_empty);
        pthread_mutex_unlock(&ch->mutex);
        return 1;
    }
}

/* ===== 非阻塞尝试接收（用于 select） =====
 * 返回: 1 = 接收成功, 0 = 无数据可用
 * 注意: 即使返回 0，如果 channel 已关闭，ok 状态需由调用者通过 closed() 判断
 */
int32_t l25_channel_try_recv(void* ptr, void* out) {
    Channel* ch = (Channel*)ptr;
    pthread_mutex_lock(&ch->mutex);

    if (ch->capacity == 0) {
        /* 无缓冲模式：只有发送者在等待时才能接收 */
        if (!ch->rendezvous_ready) {
            /* 如果已关闭且没有数据，返回零值 */
            if (ch->closed) {
                memset(out, 0, ch->elem_size);
            }
            pthread_mutex_unlock(&ch->mutex);
            return 0;
        }
        /* 有数据就绪 */
        memcpy(out, ch->rendezvous_data, ch->elem_size);
        ch->rendezvous_ready = 0;
        ch->rendezvous_data  = NULL;
        ch->rendezvous_taken = 1;
        pthread_cond_signal(&ch->rendezvous_done);
        pthread_mutex_unlock(&ch->mutex);
        return 1;
    } else {
        /* 有缓冲模式：缓冲区有数据则接收 */
        if (ch->count == 0) {
            if (ch->closed) {
                memset(out, 0, ch->elem_size);
            }
            pthread_mutex_unlock(&ch->mutex);
            return 0;
        }
        memcpy(out, (char*)ch->buffer + ch->head * ch->elem_size, ch->elem_size);
        ch->head = (ch->head + 1) % ch->capacity;
        ch->count--;
        pthread_cond_signal(&ch->not_full);
        pthread_mutex_unlock(&ch->mutex);
        return 1;
    }
}
