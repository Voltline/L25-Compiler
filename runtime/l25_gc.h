#ifndef L25_GC_H
#define L25_GC_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

// 扫描函数类型：遍历对象中的指针字段并对每个 GC 管理的指针调用 mark_fn
typedef void (*l25_gc_scan_fn)(void* obj, void (*mark_fn)(void*));
// 析构函数类型
typedef void (*l25_gc_dtor_fn)(void* obj);

// ===== 每线程根栈 (Per-Thread Root Stack) =====
// 每个线程拥有独立的根栈，GC 扫描时统一收集所有线程的根。
// 编译器通过 l25_gc_root_push/l25_gc_root_pop 操作当前线程的根栈。
#define L25_ROOT_STACK_MAX 65536
#define L25_MAX_THREADS    128

// 初始化 GC 子系统（程序启动时调用一次，同时注册主线程）
void l25_gc_init(void);

// 关闭 GC 子系统，释放所有剩余对象（程序退出前调用）
void l25_gc_shutdown(void);

// ===== 线程生命周期 =====
// 每个 spawn 线程在入口调用 init，退出前调用 fini
void l25_gc_thread_init(void);
void l25_gc_thread_fini(void);

// ===== 根栈操作（线程安全，操作当前线程的根栈） =====
void l25_gc_root_push(void** slot);
void l25_gc_root_pop(void);

// 分配 GC 管理的对象（线程安全）
void* l25_gc_alloc(size_t size, l25_gc_scan_fn scan_fn, l25_gc_dtor_fn dtor_fn);

// 注册 / 移除 GC 根（兼容接口，内部转发到 root_push/pop）
void l25_gc_add_root(void** root);
void l25_gc_remove_root(void** root);

// 手动触发垃圾回收（完整的标记-清除周期，线程安全）
void l25_gc_collect(void);

// 确定性析构（用于 delete 语句，线程安全）
void l25_gc_free(void* ptr);

// 写屏障（线程安全）
void l25_gc_write_barrier(void* new_ptr);

#ifdef __cplusplus
}
#endif

#endif // L25_GC_H
