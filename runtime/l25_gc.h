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

// 初始化 GC 子系统（程序启动时调用一次）
void l25_gc_init(void);

// 关闭 GC 子系统，释放所有剩余对象（程序退出前调用）
void l25_gc_shutdown(void);

// 分配 GC 管理的对象
//   size:     用户对象大小（不含 GC 头部）
//   scan_fn:  扫描函数（NULL 表示本对象无需扫描指针字段）
//   dtor_fn:  析构函数（NULL 表示无需析构）
// 返回：用户数据指针（已零初始化）
void* l25_gc_alloc(size_t size, l25_gc_scan_fn scan_fn, l25_gc_dtor_fn dtor_fn);

// 注册一个 GC 根（栈上指向 GC 对象的指针变量的地址）
void l25_gc_add_root(void** root);

// 移除一个 GC 根
void l25_gc_remove_root(void** root);

// 手动触发垃圾回收（标记-清除）
void l25_gc_collect(void);

// 确定性析构（用于 delete 语句）
// 调用析构函数并标记对象为"已析构"，但不释放内存。
// 实际内存释放由 GC sweep 阶段统一处理，避免悬挂指针。
void l25_gc_free(void* ptr);

#ifdef __cplusplus
}
#endif

#endif // L25_GC_H
