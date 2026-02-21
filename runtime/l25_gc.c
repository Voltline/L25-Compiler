#include "l25_gc.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

// ===== GC 对象头部 =====
typedef struct GCObject {
    struct GCObject* next;       // 全局对象链表指针
    size_t           size;       // 用户数据大小（不含头部）
    uint8_t          marked;     // 标记位
    l25_gc_scan_fn   scan_fn;    // 扫描函数（NULL = 无指针字段）
    l25_gc_dtor_fn   dtor_fn;    // 析构函数（NULL = 无需析构）
} GCObject;

// ===== GC 全局状态 =====
typedef struct {
    GCObject*  objects;          // 所有 GC 对象的链表头
    void**     roots;            // 根集数组（指向栈上变量地址的指针）
    size_t     root_count;
    size_t     root_capacity;
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
    gc.roots          = NULL;
    gc.root_count     = 0;
    gc.root_capacity  = 0;
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
        if (obj->dtor_fn) {
            obj->dtor_fn(get_user_ptr(obj));
        }
        free(obj);
        obj = next;
    }
    gc.objects       = NULL;
    gc.object_count  = 0;
    gc.bytes_allocated = 0;

    // 释放根集数组
    free(gc.roots);
    gc.roots         = NULL;
    gc.root_count    = 0;
    gc.root_capacity = 0;
}

// ===== 标记单个对象 =====
static void mark_object(void* ptr) {
    if (!ptr) return;
    GCObject* obj = get_header(ptr);
    if (obj->marked) return;     // 已标记，避免循环引用死循环
    obj->marked = 1;
    // 递归扫描此对象引用的其他 GC 对象
    if (obj->scan_fn) {
        obj->scan_fn(ptr, mark_object);
    }
}

// ===== 标记阶段 =====
static void mark_phase(void) {
    for (size_t i = 0; i < gc.root_count; i++) {
        void** slot = gc.roots[i];
        void* ptr = *slot;
        if (ptr) {
            mark_object(ptr);
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

            if (garbage->dtor_fn) {
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
    if (gc.root_count >= gc.root_capacity) {
        gc.root_capacity = gc.root_capacity == 0 ? 64 : gc.root_capacity * 2;
        gc.roots = (void**)realloc(gc.roots, gc.root_capacity * sizeof(void*));
    }
    gc.roots[gc.root_count++] = root;
}

// ===== 移除根 =====
void l25_gc_remove_root(void** root) {
    for (size_t i = 0; i < gc.root_count; i++) {
        if (gc.roots[i] == root) {
            // 与最后一个交换后缩减
            gc.roots[i] = gc.roots[gc.root_count - 1];
            gc.root_count--;
            return;
        }
    }
}

// ===== 显式释放（用于 delete 语句）=====
void l25_gc_free(void* ptr) {
    if (!ptr) return;
    GCObject* target = get_header(ptr);

    // 从全局链表中移除
    GCObject** prev = &gc.objects;
    GCObject*  obj  = gc.objects;
    while (obj) {
        if (obj == target) {
            *prev = obj->next;
            break;
        }
        prev = &obj->next;
        obj  = obj->next;
    }

    // 调用析构函数
    if (target->dtor_fn) {
        target->dtor_fn(ptr);
    }

    gc.bytes_allocated -= (sizeof(GCObject) + target->size);
    gc.object_count--;
    free(target);
}
