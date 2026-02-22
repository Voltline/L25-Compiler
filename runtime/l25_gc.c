#include "l25_gc.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ===== GC 对象头部 =====
typedef struct GCObject {
    struct GCObject* next;       // 全局对象链表指针
    size_t           size;       // 用户数据大小（不含头部）
    uint8_t          marked;     // 标记位
    uint8_t          dead;       // delete 已调用析构（sweep 时跳过 dtor）
    l25_gc_scan_fn   scan_fn;    // 扫描函数（NULL = 无指针字段）
    l25_gc_dtor_fn   dtor_fn;    // 析构函数（NULL = 无需析构）
} GCObject;

// ===== 开放寻址哈希集（存储 void** 根指针）=====
typedef struct {
    void**   buckets;      // 哈希桶数组（NULL = 空槽, (void**)1 = 墓碑）
    size_t   capacity;     // 桶数（总是 2 的幂）
    size_t   count;        // 活跃元素数
    size_t   tombstones;   // 墓碑计数
} RootSet;

#define ROOTSET_TOMBSTONE  ((void**)1)
#define ROOTSET_EMPTY      ((void**)0)

static inline size_t root_hash(void** ptr) {
    // 指针右移 3（对齐到 8 字节），用黄金比例乘法散列
    size_t h = (size_t)ptr >> 3;
    h *= 0x9E3779B97F4A7C15ULL;  // fibonacci hashing
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

// 负载因子超过 70% 时扩容（count + tombstones 占比）
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
            // 重新插入
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
    // 检查是否需要扩容（负载 > 70%）
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
        if (entry == root) return;  // 已存在
        idx = (idx + 1) & mask;
    }
}

static void rootset_remove(RootSet* rs, void** root) {
    if (rs->capacity == 0) return;
    size_t mask = rs->capacity - 1;
    size_t idx  = root_hash(root) & mask;

    while (1) {
        void** entry = (void**)rs->buckets[idx];
        if (entry == ROOTSET_EMPTY) return;   // 不存在
        if (entry == root) {
            rs->buckets[idx] = (void*)ROOTSET_TOMBSTONE;
            rs->count--;
            rs->tombstones++;
            // 墓碑过多时收缩
            if (rs->tombstones > rs->count && rs->capacity > 64) {
                rootset_resize(rs, rs->capacity);  // 原地 rehash 清除墓碑
            }
            return;
        }
        idx = (idx + 1) & mask;
    }
}

// ===== GC 全局状态 =====
typedef struct {
    GCObject*  objects;          // 所有 GC 对象的链表头
    RootSet    roots;            // 根集哈希表
    size_t     bytes_allocated;  // 已分配字节数
    size_t     next_gc;          // 触发 GC 的字节阈值
    size_t     object_count;     // GC 对象计数
} GCState;

static GCState gc = {0};

#define GC_INITIAL_THRESHOLD  (1024 * 64)   // 64 KB
#define GC_GROW_FACTOR        2

// ===== 内部辅助：用户指针 ↔ GC 头部 =====
static inline GCObject* get_header(void* ptr) {
    return (GCObject*)((char*)ptr - sizeof(GCObject));
}

static inline void* get_user_ptr(GCObject* obj) {
    return (void*)((char*)obj + sizeof(GCObject));
}

// ===== 初始化 =====
void l25_gc_init(void) {
    gc.objects        = NULL;
    rootset_init(&gc.roots);
    gc.bytes_allocated = 0;
    gc.next_gc        = GC_INITIAL_THRESHOLD;
    gc.object_count   = 0;
}

// ===== 关闭 =====
void l25_gc_shutdown(void) {
    // 先运行一次完整 GC 回收不可达对象
    l25_gc_collect();

    // 释放所有剩余对象（仍可达的，也强制释放）
    GCObject* obj = gc.objects;
    while (obj) {
        GCObject* next = obj->next;
        if (!obj->dead && obj->dtor_fn) {
            obj->dtor_fn(get_user_ptr(obj));
        }
        free(obj);
        obj = next;
    }
    gc.objects       = NULL;
    gc.object_count  = 0;
    gc.bytes_allocated = 0;

    // 释放根集哈希表
    rootset_free(&gc.roots);
}

// ===== 标记单个对象 =====
static void mark_object(void* ptr) {
    if (!ptr) return;
    GCObject* obj = get_header(ptr);
    if (obj->marked) return;     // 已标记，避免循环引用死循环
    obj->marked = 1;
    // dead 对象仍需扫描其指针字段（可能引用其他存活对象）
    if (obj->scan_fn) {
        obj->scan_fn(ptr, mark_object);
    }
}

// ===== 标记阶段 =====
static void mark_phase(void) {
    RootSet* rs = &gc.roots;
    for (size_t i = 0; i < rs->capacity; i++) {
        void** entry = (void**)rs->buckets[i];
        if (entry != ROOTSET_EMPTY && entry != ROOTSET_TOMBSTONE) {
            void* ptr = *entry;
            if (ptr) {
                mark_object(ptr);
            }
        }
    }
}

// ===== 清除阶段 =====
static void sweep_phase(void) {
    GCObject** prev = &gc.objects;
    GCObject*  obj  = gc.objects;
    while (obj) {
        if (obj->marked) {
            // 可达：保留，重置标记
            obj->marked = 0;
            prev = &obj->next;
            obj  = obj->next;
        } else {
            // 不可达：释放
            GCObject* garbage = obj;
            *prev = obj->next;
            obj   = obj->next;

            // dead 对象的析构已在 delete 时调用，跳过
            if (!garbage->dead && garbage->dtor_fn) {
                garbage->dtor_fn(get_user_ptr(garbage));
            }
            gc.bytes_allocated -= (sizeof(GCObject) + garbage->size);
            gc.object_count--;
            free(garbage);
        }
    }
}

// ===== 触发垃圾回收 =====
void l25_gc_collect(void) {
    mark_phase();
    sweep_phase();
    // 动态调整阈值
    gc.next_gc = gc.bytes_allocated * GC_GROW_FACTOR;
    if (gc.next_gc < GC_INITIAL_THRESHOLD) {
        gc.next_gc = GC_INITIAL_THRESHOLD;
    }
}

// ===== 分配 GC 管理的对象 =====
void* l25_gc_alloc(size_t size, l25_gc_scan_fn scan_fn, l25_gc_dtor_fn dtor_fn) {
    // 检查是否需要触发 GC（在分配前收集，保证新对象不被误收）
    if (gc.bytes_allocated + sizeof(GCObject) + size > gc.next_gc) {
        l25_gc_collect();
    }

    GCObject* obj = (GCObject*)malloc(sizeof(GCObject) + size);
    if (!obj) return NULL;

    // 零初始化整个块（含头部和用户数据）
    memset(obj, 0, sizeof(GCObject) + size);
    obj->size    = size;
    obj->marked  = 1;        // 新分配对象标记为存活，防止在下一次 GC 中被误回收
    obj->dead    = 0;
    obj->scan_fn = scan_fn;
    obj->dtor_fn = dtor_fn;

    // 加入全局对象链表头部
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

// ===== 确定性析构（用于 delete 语句） =====
// 调用析构函数 + 标记 dead，但不释放内存。
// 内存由 GC sweep 阶段在对象不可达时统一释放。
// 如果还有其他根引用此对象，内存保持有效（不会悬挂指针）。
void l25_gc_free(void* ptr) {
    if (!ptr) return;
    GCObject* obj = get_header(ptr);

    // 防止重复析构
    if (obj->dead) return;

    // 调用析构函数
    if (obj->dtor_fn) {
        obj->dtor_fn(ptr);
    }

    // 标记为已析构，sweep 时跳过 dtor 但正常释放内存
    obj->dead = 1;
}
