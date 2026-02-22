#include "l25_gc.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

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
} GCObject;

// ===== 开放寻址哈希集（存储 void** 根指针）=====
typedef struct {
    void**   buckets;
    size_t   capacity;
    size_t   count;
    size_t   tombstones;
} RootSet;

#define ROOTSET_TOMBSTONE  ((void**)1)
#define ROOTSET_EMPTY      ((void**)0)

static inline size_t root_hash(void** ptr) {
    size_t h = (size_t)ptr >> 3;
    h *= 0x9E3779B97F4A7C15ULL;
    return h;
}

static void rootset_init(RootSet* rs) {
    rs->buckets    = NULL;
    rs->capacity   = 0;
    rs->count      = 0;
    rs->tombstones = 0;
}

static void rootset_free(RootSet* rs) {
    free(rs->buckets);
    rs->buckets    = NULL;
    rs->capacity   = 0;
    rs->count      = 0;
    rs->tombstones = 0;
}

static void rootset_resize(RootSet* rs, size_t new_cap) {
    void** old_buckets = rs->buckets;
    size_t old_cap     = rs->capacity;

    rs->buckets    = (void**)calloc(new_cap, sizeof(void*));
    rs->capacity   = new_cap;
    rs->count      = 0;
    rs->tombstones = 0;

    size_t mask = new_cap - 1;
    for (size_t i = 0; i < old_cap; i++) {
        void** entry = (void**)old_buckets[i];
        if (entry != ROOTSET_EMPTY && entry != ROOTSET_TOMBSTONE) {
            size_t idx = root_hash(entry) & mask;
            while (rs->buckets[idx] != NULL) {
                idx = (idx + 1) & mask;
            }
            rs->buckets[idx] = (void*)entry;
            rs->count++;
        }
    }
    free(old_buckets);
}

static void rootset_insert(RootSet* rs, void** root) {
    if (rs->capacity == 0 ||
        (rs->count + rs->tombstones + 1) * 10 > rs->capacity * 7) {
        size_t new_cap = rs->capacity == 0 ? 64 : rs->capacity * 2;
        rootset_resize(rs, new_cap);
    }

    size_t mask = rs->capacity - 1;
    size_t idx  = root_hash(root) & mask;

    while (1) {
        void** entry = (void**)rs->buckets[idx];
        if (entry == ROOTSET_EMPTY || entry == ROOTSET_TOMBSTONE) {
            if (entry == ROOTSET_TOMBSTONE) rs->tombstones--;
            rs->buckets[idx] = (void*)root;
            rs->count++;
            return;
        }
        if (entry == root) return;
        idx = (idx + 1) & mask;
    }
}

static void rootset_remove(RootSet* rs, void** root) {
    if (rs->capacity == 0) return;
    size_t mask = rs->capacity - 1;
    size_t idx  = root_hash(root) & mask;

    while (1) {
        void** entry = (void**)rs->buckets[idx];
        if (entry == ROOTSET_EMPTY) return;
        if (entry == root) {
            rs->buckets[idx] = (void*)ROOTSET_TOMBSTONE;
            rs->count--;
            rs->tombstones++;
            if (rs->tombstones > rs->count && rs->capacity > 64) {
                rootset_resize(rs, rs->capacity);
            }
            return;
        }
        idx = (idx + 1) & mask;
    }
}

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
    RootSet    roots;            // 根集哈希表
    size_t     bytes_allocated;  // 已分配字节数
    size_t     next_gc;          // 触发 GC 的字节阈值
    size_t     object_count;     // GC 对象计数

    // 增量 GC 状态
    GCPhase    phase;
    GCObject*  gray_list;        // 灰色队列链表头
    GCObject*  sweep_cursor;     // sweep 阶段的当前遍历位置
    GCObject** sweep_prev;       // sweep 阶段的前驱指针
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
    gc.objects        = NULL;
    rootset_init(&gc.roots);
    gc.bytes_allocated = 0;
    gc.next_gc        = GC_INITIAL_THRESHOLD;
    gc.object_count   = 0;
    gc.phase          = GC_PHASE_IDLE;
    gc.gray_list      = NULL;
    gc.sweep_cursor   = NULL;
    gc.sweep_prev     = NULL;
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

    // 2. 从根出发将直接可达对象标灰
    RootSet* rs = &gc.roots;
    for (size_t i = 0; i < rs->capacity; i++) {
        void** entry = (void**)rs->buckets[i];
        if (entry != ROOTSET_EMPTY && entry != ROOTSET_TOMBSTONE) {
            void* ptr = *entry;
            if (ptr) {
                shade_gray(get_header(ptr));
            }
        }
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

            gc.bytes_allocated -= (sizeof(GCObject) + obj->size);
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

// ===== 完整回收（STW） =====
void l25_gc_collect(void) {
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
    // gc_sweep_dtors_step 完成后已自动进入 GC_PHASE_SWEEP_FREE
    gc_sweep_free_step(SIZE_MAX);
}

// ===== 关闭 =====
void l25_gc_shutdown(void) {
    // 先完成完整 GC
    l25_gc_collect();

    // 释放所有剩余对象（两阶段：先调析构，再释放）
    // 阶段 1：析构
    for (GCObject* obj = gc.objects; obj; obj = obj->next) {
        if (!obj->dead && obj->dtor_fn) {
            obj->dtor_fn(get_user_ptr(obj));
            obj->dead = 1;
        }
    }
    // 阶段 2：释放
    GCObject* obj = gc.objects;
    while (obj) {
        GCObject* next = obj->next;
        free(obj);
        obj = next;
    }
    gc.objects       = NULL;
    gc.object_count  = 0;
    gc.bytes_allocated = 0;

    rootset_free(&gc.roots);
}

// ===== 分配 GC 管理的对象 =====
void* l25_gc_alloc(size_t size, l25_gc_scan_fn scan_fn, l25_gc_dtor_fn dtor_fn) {
    // 增量推进 GC
    gc_step(GC_MARK_STEPS_PER_ALLOC);

    GCObject* obj = (GCObject*)malloc(sizeof(GCObject) + size);
    if (!obj) return NULL;

    memset(obj, 0, sizeof(GCObject) + size);
    obj->size      = size;
    obj->scan_fn   = scan_fn;
    obj->dtor_fn   = dtor_fn;
    obj->gray_next = NULL;
    obj->dead      = 0;

    // 新对象在增量标记进行中时直接标黑（保守策略：不会被当前周期收集）
    // 空闲时为白色，下一轮标记会正确处理
    if (gc.phase != GC_PHASE_IDLE) {
        obj->color = GC_BLACK;
    } else {
        obj->color = GC_WHITE;
    }

    obj->next    = gc.objects;
    gc.objects   = obj;

    gc.bytes_allocated += sizeof(GCObject) + size;
    gc.object_count++;

    return get_user_ptr(obj);
}

// ===== 注册根 =====
void l25_gc_add_root(void** root) {
    rootset_insert(&gc.roots, root);
}

// ===== 移除根 =====
void l25_gc_remove_root(void** root) {
    rootset_remove(&gc.roots, root);
}

// ===== 确定性析构（delete 语句） =====
void l25_gc_free(void* ptr) {
    if (!ptr) return;
    GCObject* obj = get_header(ptr);
    if (obj->dead) return;

    if (obj->dtor_fn) {
        obj->dtor_fn(ptr);
    }
    obj->dead = 1;
}

// ===== 写屏障 =====
// Dijkstra-style: 在增量标记期间，当指针字段被赋予新值时，
// 将新值对象标灰，防止已被黑色对象引用的白色对象被遗漏（丢失更新）。
void l25_gc_write_barrier(void* new_ptr) {
    if (gc.phase != GC_PHASE_MARKING) return;
    if (!new_ptr) return;
    GCObject* obj = get_header(new_ptr);
    shade_gray(obj);
}
