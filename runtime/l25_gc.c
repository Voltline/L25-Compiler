#include "l25_gc.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <pthread.h>

// ===== 三色标记 =====
enum GCColor {
    GC_WHITE = 0,   // 未标记 / 待回收候选
    GC_GRAY  = 1,   // 已发现但子对象未扫描（在灰色队列中）
    GC_BLACK = 2    // 已完全扫描
};

// ===== GC 对象头部 =====
typedef struct GCObject {
    struct GCObject* next;       // 全局对象链表指针
    struct GCObject* gray_next;  // 灰色队列链表指针
    size_t           size;       // 用户数据大小（不含头部）
    uint8_t          color;      // 三色标记（GC_WHITE / GC_GRAY / GC_BLACK）
    uint8_t          dead;       // delete 已调用析构（sweep 时跳过 dtor）
    l25_gc_scan_fn   scan_fn;    // 扫描函数（NULL = 无指针字段）
    l25_gc_dtor_fn   dtor_fn;    // 析构函数（NULL = 无需析构）
    void**           vtable;     // 虚函数表指针（NULL = 无虚表）
} GCObject;

// ===== 每线程根栈 =====
typedef struct {
    void*           stack[L25_ROOT_STACK_MAX];
    int32_t         sp;
    pthread_mutex_t lock;   // 保护 stack/sp 的一致性（GC 扫描 vs 线程 push/pop）
} ThreadRootStack;

// TLS：当前线程的根栈指针
static __thread ThreadRootStack* tls_roots = NULL;

// 全局线程根栈注册表（受 gc_lock 保护）
static ThreadRootStack* all_roots[L25_MAX_THREADS];
static int              all_roots_count = 0;

// ===== GC 全局互斥锁 =====
static pthread_mutex_t gc_lock = PTHREAD_MUTEX_INITIALIZER;

// ===== GC 状态机 =====
typedef enum {
    GC_PHASE_IDLE,           // 空闲（收集完成或尚未开始）
    GC_PHASE_MARKING,        // 增量标记进行中（灰色队列非空）
    GC_PHASE_SWEEP_DTORS,    // 清除第 1 阶段：调用不可达对象的析构器
    GC_PHASE_SWEEP_FREE      // 清除第 2 阶段：释放不可达对象的内存
} GCPhase;

// ===== GC 全局状态 =====
typedef struct {
    GCObject*  objects;          // 所有 GC 对象的链表头
    size_t     bytes_allocated;  // 已分配字节数
    size_t     next_gc;          // 触发 GC 的字节阈值
    size_t     object_count;     // GC 对象计数

    // 增量 GC 状态
    GCPhase    phase;
    GCObject*  gray_list;        // 灰色队列链表头
    GCObject*  sweep_cursor;     // sweep 阶段的当前遍历位置
    GCObject** sweep_prev;       // sweep 阶段的前驱指针

    // 监测统计
    size_t     total_allocs;     // 总分配次数
    size_t     total_collections;// 总 GC 回收周期数
    size_t     total_freed;      // 总释放字节数
    int        paused;           // GC 暂停标志（>0 表示暂停）
} GCState;

static GCState gc = {0};

#define GC_INITIAL_THRESHOLD  (1024 * 64)   // 64 KB
#define GC_GROW_FACTOR        2
#define GC_MARK_STEPS_PER_ALLOC  8          // 每次 alloc 推进的灰色对象数

// ===== 内部辅助 =====
static inline GCObject* get_header(void* ptr) {
    return (GCObject*)((char*)ptr - sizeof(GCObject));
}

static inline void* get_user_ptr(GCObject* obj) {
    return (void*)((char*)obj + sizeof(GCObject));
}

// 将对象加入灰色队列（如果尚未灰色/黑色）
static void shade_gray(GCObject* obj) {
    if (obj->color != GC_WHITE) return;
    obj->color = GC_GRAY;
    obj->gray_next = gc.gray_list;
    gc.gray_list   = obj;
}

// 供 scan_fn 回调使用的标记函数：将用户指针对应的对象标灰
static void mark_gray(void* ptr) {
    if (!ptr) return;
    GCObject* obj = get_header(ptr);
    shade_gray(obj);
}

// ===== 初始化 =====
void l25_gc_init(void) {
    pthread_mutex_lock(&gc_lock);
    gc.objects        = NULL;
    gc.bytes_allocated = 0;
    gc.next_gc        = GC_INITIAL_THRESHOLD;
    gc.object_count   = 0;
    gc.phase          = GC_PHASE_IDLE;
    gc.gray_list      = NULL;
    gc.sweep_cursor   = NULL;
    gc.sweep_prev     = NULL;
    gc.total_allocs   = 0;
    gc.total_collections = 0;
    gc.total_freed    = 0;
    gc.paused         = 0;
    all_roots_count   = 0;
    pthread_mutex_unlock(&gc_lock);

    // 注册主线程
    l25_gc_thread_init();
}

// ===== 线程注册/注销 =====
void l25_gc_thread_init(void) {
    if (tls_roots) return; // 已注册
    ThreadRootStack* rs = (ThreadRootStack*)calloc(1, sizeof(ThreadRootStack));
    rs->sp = 0;
    pthread_mutex_init(&rs->lock, NULL);
    tls_roots = rs;

    pthread_mutex_lock(&gc_lock);
    if (all_roots_count < L25_MAX_THREADS) {
        all_roots[all_roots_count++] = rs;
    }
    pthread_mutex_unlock(&gc_lock);
}

void l25_gc_thread_fini(void) {
    if (!tls_roots) return;
    ThreadRootStack* rs = tls_roots;
    tls_roots = NULL;

    pthread_mutex_lock(&gc_lock);
    for (int i = 0; i < all_roots_count; i++) {
        if (all_roots[i] == rs) {
            all_roots[i] = all_roots[--all_roots_count];
            break;
        }
    }
    pthread_mutex_unlock(&gc_lock);
    pthread_mutex_destroy(&rs->lock);
    free(rs);
}

// ===== 根栈操作（线程安全：per-thread mutex 保护 sp/stack 一致性） =====
void l25_gc_root_push(void** slot) {
    if (!tls_roots) return;
    pthread_mutex_lock(&tls_roots->lock);
    if (tls_roots->sp < L25_ROOT_STACK_MAX) {
        tls_roots->stack[tls_roots->sp++] = (void*)slot;
    }
    pthread_mutex_unlock(&tls_roots->lock);
}

void l25_gc_root_pop(void) {
    if (!tls_roots) return;
    pthread_mutex_lock(&tls_roots->lock);
    if (tls_roots->sp > 0) {
        tls_roots->sp--;
    }
    pthread_mutex_unlock(&tls_roots->lock);
}

// ===== 增量标记：处理 N 个灰色对象 =====
static void incremental_mark(size_t steps) {
    while (steps > 0 && gc.gray_list) {
        GCObject* obj = gc.gray_list;
        gc.gray_list   = obj->gray_next;
        obj->gray_next = NULL;
        obj->color     = GC_BLACK;

        // 扫描此对象的指针字段，将引用对象标灰
        if (obj->scan_fn) {
            obj->scan_fn(get_user_ptr(obj), mark_gray);
        }
        steps--;
    }
}

// ===== 开始新的 GC 周期 =====
static void gc_start_cycle(void) {
    // 1. 将所有对象设为白色
    for (GCObject* obj = gc.objects; obj; obj = obj->next) {
        obj->color     = GC_WHITE;
        obj->gray_next = NULL;
    }
    gc.gray_list = NULL;

    // 2. 从所有线程的根栈出发将直接可达对象标灰
    //    锁定每个线程的根栈 mutex，防止 push/pop 与扫描并发
    for (int t = 0; t < all_roots_count; t++) {
        ThreadRootStack* rs = all_roots[t];
        if (!rs) continue;
        pthread_mutex_lock(&rs->lock);
        for (int32_t i = 0; i < rs->sp; i++) {
            void** slot = (void**)rs->stack[i];
            void* ptr = *slot;
            if (ptr) {
                shade_gray(get_header(ptr));
            }
        }
        pthread_mutex_unlock(&rs->lock);
    }

    gc.phase = GC_PHASE_MARKING;
}

// ===== 清除第 1 阶段（调用析构器，不释放内存） =====
// 遍历对象链表，对 WHITE 且未 dead 的对象调用析构器并标记 dead
static void gc_sweep_dtors_step(size_t steps) {
    while (steps > 0 && gc.sweep_cursor) {
        GCObject* obj = gc.sweep_cursor;
        if (obj->color == GC_WHITE && !obj->dead && obj->dtor_fn) {
            obj->dtor_fn(get_user_ptr(obj));
            obj->dead = 1;
        }
        gc.sweep_cursor = obj->next;
        steps--;
    }

    if (!gc.sweep_cursor) {
        // 析构完毕，进入释放阶段
        gc.phase        = GC_PHASE_SWEEP_FREE;
        gc.sweep_prev   = &gc.objects;
        gc.sweep_cursor = gc.objects;
    }
}

// ===== 清除第 2 阶段（释放不可达对象的内存） =====
static void gc_sweep_free_step(size_t steps) {
    while (steps > 0 && gc.sweep_cursor) {
        GCObject* obj = gc.sweep_cursor;
        if (obj->color == GC_WHITE) {
            // 不可达：从链表中移除并释放
            *gc.sweep_prev  = obj->next;
            gc.sweep_cursor = obj->next;

            size_t freed_size = sizeof(GCObject) + obj->size;
            gc.bytes_allocated -= freed_size;
            gc.total_freed += freed_size;
            gc.object_count--;
            free(obj);
        } else {
            // 可达：重置为白色（为下一轮做准备）
            obj->color = GC_WHITE;
            gc.sweep_prev   = &obj->next;
            gc.sweep_cursor = obj->next;
        }
        steps--;
    }

    if (!gc.sweep_cursor) {
        // 清除完毕
        gc.phase = GC_PHASE_IDLE;
        // 动态调整阈值
        gc.next_gc = gc.bytes_allocated * GC_GROW_FACTOR;
        if (gc.next_gc < GC_INITIAL_THRESHOLD) {
            gc.next_gc = GC_INITIAL_THRESHOLD;
        }
    }
}

// ===== 增量推进 =====
static void gc_step(size_t steps) {
    if (gc.paused) return;
    if (gc.phase == GC_PHASE_IDLE) {
        if (gc.bytes_allocated >= gc.next_gc) {
            gc_start_cycle();
        } else {
            return;
        }
    }

    if (gc.phase == GC_PHASE_MARKING) {
        incremental_mark(steps);
        if (!gc.gray_list) {
            // 标记完毕，进入清除第 1 阶段（析构）
            gc.phase        = GC_PHASE_SWEEP_DTORS;
            gc.sweep_cursor = gc.objects;
        }
    }

    if (gc.phase == GC_PHASE_SWEEP_DTORS) {
        gc_sweep_dtors_step(steps);
    }

    if (gc.phase == GC_PHASE_SWEEP_FREE) {
        gc_sweep_free_step(steps);
    }
}

// ===== 完整回收（STW）—— 内部版本，调用方已持有 gc_lock =====
static void gc_collect_locked(void) {
    // 如果有进行中的增量周期，先完成它
    if (gc.phase != GC_PHASE_IDLE) {
        if (gc.phase == GC_PHASE_MARKING) {
            incremental_mark(SIZE_MAX);
            gc.phase        = GC_PHASE_SWEEP_DTORS;
            gc.sweep_cursor = gc.objects;
        }
        if (gc.phase == GC_PHASE_SWEEP_DTORS) {
            gc_sweep_dtors_step(SIZE_MAX);
        }
        if (gc.phase == GC_PHASE_SWEEP_FREE) {
            gc_sweep_free_step(SIZE_MAX);
        }
        return;
    }

    // 开始新的完整周期
    gc_start_cycle();
    incremental_mark(SIZE_MAX);

    // 两阶段 sweep：先析构，再释放
    gc.phase        = GC_PHASE_SWEEP_DTORS;
    gc.sweep_cursor = gc.objects;
    gc_sweep_dtors_step(SIZE_MAX);

    // Phase 2: free
    gc_sweep_free_step(SIZE_MAX);

    gc.total_collections++;
}

void l25_gc_collect(void) {
    pthread_mutex_lock(&gc_lock);
    gc_collect_locked();
    pthread_mutex_unlock(&gc_lock);
}

// ===== 关闭 =====
void l25_gc_shutdown(void) {
    pthread_mutex_lock(&gc_lock);

    // 先完成完整 GC
    gc_collect_locked();

    // 释放所有剩余对象（两阶段：先调析构，再释放）
    for (GCObject* obj = gc.objects; obj; obj = obj->next) {
        if (!obj->dead && obj->dtor_fn) {
            obj->dtor_fn(get_user_ptr(obj));
            obj->dead = 1;
        }
    }
    GCObject* obj = gc.objects;
    while (obj) {
        GCObject* next = obj->next;
        free(obj);
        obj = next;
    }
    gc.objects       = NULL;
    gc.object_count  = 0;
    gc.bytes_allocated = 0;

    pthread_mutex_unlock(&gc_lock);

    // 注销主线程
    l25_gc_thread_fini();
}

// ===== 分配 GC 管理的对象（线程安全） =====
void* l25_gc_alloc(size_t size, l25_gc_scan_fn scan_fn, l25_gc_dtor_fn dtor_fn, void** vtable) {
    pthread_mutex_lock(&gc_lock);

    // 自适应步进
    size_t steps = GC_MARK_STEPS_PER_ALLOC;
    if (gc.next_gc > 0) {
        size_t threshold_75 = gc.next_gc / 4 * 3;
        if (gc.bytes_allocated >= threshold_75) {
            steps = GC_MARK_STEPS_PER_ALLOC * 8;
        }
        if (gc.bytes_allocated >= gc.next_gc && gc.phase != GC_PHASE_IDLE) {
            gc_collect_locked();
        }
    }
    gc_step(steps);

    GCObject* o = (GCObject*)malloc(sizeof(GCObject) + size);
    if (!o) {
        pthread_mutex_unlock(&gc_lock);
        return NULL;
    }

    memset(o, 0, sizeof(GCObject) + size);
    o->size      = size;
    o->scan_fn   = scan_fn;
    o->dtor_fn   = dtor_fn;
    o->vtable    = vtable;
    o->gray_next = NULL;
    o->dead      = 0;

    if (gc.phase != GC_PHASE_IDLE) {
        o->color = GC_BLACK;
    } else {
        o->color = GC_WHITE;
    }

    o->next    = gc.objects;
    gc.objects = o;

    gc.bytes_allocated += sizeof(GCObject) + size;
    gc.object_count++;
    gc.total_allocs++;

    void* result = get_user_ptr(o);
    pthread_mutex_unlock(&gc_lock);
    return result;
}

// ===== 获取虚函数表 =====
void** l25_gc_get_vtable(void* ptr) {
    if (!ptr) return NULL;
    return get_header(ptr)->vtable;
}

// ===== 兼容接口 =====
void l25_gc_add_root(void** root) {
    l25_gc_root_push(root);
}

void l25_gc_remove_root(void** root) {
    l25_gc_root_pop();
}

// ===== 确定性析构并释放（delete 语句，线程安全） =====
void l25_gc_free(void* ptr) {
    if (!ptr) return;

    pthread_mutex_lock(&gc_lock);

    GCObject* obj = get_header(ptr);

    // 调用析构器（如果尚未调用）
    if (!obj->dead && obj->dtor_fn) {
        obj->dtor_fn(ptr);
    }
    obj->dead = 1;

    // 从 GC 链表中移除
    GCObject** prev = &gc.objects;
    for (GCObject* cur = gc.objects; cur; cur = cur->next) {
        if (cur == obj) {
            *prev = cur->next;
            break;
        }
        prev = &cur->next;
    }

    // 如果 sweep 游标正指向此对象，需要推进
    if (gc.sweep_cursor == obj) {
        gc.sweep_cursor = obj->next;
    }

    // 从灰色队列中移除
    if (obj->color == GC_GRAY) {
        if (gc.gray_list == obj) {
            gc.gray_list = obj->gray_next;
        } else {
            for (GCObject* g = gc.gray_list; g; g = g->gray_next) {
                if (g->gray_next == obj) {
                    g->gray_next = obj->gray_next;
                    break;
                }
            }
        }
    }

    // 扫描所有线程根栈：将指向此对象的 slot 置 null
    for (int t = 0; t < all_roots_count; t++) {
        ThreadRootStack* rs = all_roots[t];
        if (!rs) continue;
        for (int32_t i = 0; i < rs->sp; i++) {
            void** slot = (void**)rs->stack[i];
            if (*slot == ptr) {
                *slot = NULL;
            }
        }
    }

    gc.bytes_allocated -= (sizeof(GCObject) + obj->size);
    gc.object_count--;

    free(obj);

    pthread_mutex_unlock(&gc_lock);
}

// ===== 写屏障（线程安全） =====
void l25_gc_write_barrier(void* new_ptr) {
    if (!new_ptr) return;
    pthread_mutex_lock(&gc_lock);
    if (gc.phase == GC_PHASE_MARKING) {
        GCObject* obj = get_header(new_ptr);
        shade_gray(obj);
    }
    pthread_mutex_unlock(&gc_lock);
}

// ===== GC 监测 API =====
int32_t l25_gc_count(void) {
    pthread_mutex_lock(&gc_lock);
    int32_t val = (int32_t)gc.object_count;
    pthread_mutex_unlock(&gc_lock);
    return val;
}

int64_t l25_gc_bytes(void) {
    pthread_mutex_lock(&gc_lock);
    int64_t val = (int64_t)gc.bytes_allocated;
    pthread_mutex_unlock(&gc_lock);
    return val;
}

int64_t l25_gc_threshold(void) {
    pthread_mutex_lock(&gc_lock);
    int64_t val = (int64_t)gc.next_gc;
    pthread_mutex_unlock(&gc_lock);
    return val;
}

void l25_gc_set_threshold(int64_t bytes) {
    pthread_mutex_lock(&gc_lock);
    if (bytes > 0) gc.next_gc = (size_t)bytes;
    pthread_mutex_unlock(&gc_lock);
}

int64_t l25_gc_total_allocs(void) {
    pthread_mutex_lock(&gc_lock);
    int64_t val = (int64_t)gc.total_allocs;
    pthread_mutex_unlock(&gc_lock);
    return val;
}

int64_t l25_gc_total_collections(void) {
    pthread_mutex_lock(&gc_lock);
    int64_t val = (int64_t)gc.total_collections;
    pthread_mutex_unlock(&gc_lock);
    return val;
}

int64_t l25_gc_total_freed(void) {
    pthread_mutex_lock(&gc_lock);
    int64_t val = (int64_t)gc.total_freed;
    pthread_mutex_unlock(&gc_lock);
    return val;
}

void l25_gc_stats(void) {
    pthread_mutex_lock(&gc_lock);
    fprintf(stderr, "===== L25 GC Stats =====\n");
    fprintf(stderr, "  Live objects:       %zu\n", gc.object_count);
    fprintf(stderr, "  Bytes allocated:    %zu\n", gc.bytes_allocated);
    fprintf(stderr, "  GC threshold:       %zu\n", gc.next_gc);
    fprintf(stderr, "  Total allocations:  %zu\n", gc.total_allocs);
    fprintf(stderr, "  Total collections:  %zu\n", gc.total_collections);
    fprintf(stderr, "  Total freed bytes:  %zu\n", gc.total_freed);
    fprintf(stderr, "  GC phase:           %s\n",
        gc.phase == GC_PHASE_IDLE         ? "idle" :
        gc.phase == GC_PHASE_MARKING      ? "marking" :
        gc.phase == GC_PHASE_SWEEP_DTORS  ? "sweep-dtors" :
        gc.phase == GC_PHASE_SWEEP_FREE   ? "sweep-free" : "unknown");
    fprintf(stderr, "  Paused:             %s\n", gc.paused ? "yes" : "no");
    fprintf(stderr, "  Active threads:     %d\n", all_roots_count);
    fprintf(stderr, "========================\n");
    pthread_mutex_unlock(&gc_lock);
}

void l25_gc_pause(void) {
    pthread_mutex_lock(&gc_lock);
    gc.paused++;
    pthread_mutex_unlock(&gc_lock);
}

void l25_gc_resume(void) {
    pthread_mutex_lock(&gc_lock);
    if (gc.paused > 0) gc.paused--;
    pthread_mutex_unlock(&gc_lock);
}
