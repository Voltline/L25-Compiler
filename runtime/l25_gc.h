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

// ===== 根栈 (Root Stack) =====
// 编译器直接内联操作这两个全局变量，替代 add_root/remove_root 函数调用。
// 每个槽位存放一个 void** (指向栈上 alloca 的地址)，GC 扫描时
// 解引用即可获得实际的 GC 对象指针。
#define L25_ROOT_STACK_MAX 65536
extern void*   l25_gc_root_stack[L25_ROOT_STACK_MAX];
extern int32_t l25_gc_root_sp;

// 初始化 GC 子系统（程序启动时调用一次）
void l25_gc_init(void);

// 关闭 GC 子系统，释放所有剩余对象（程序退出前调用）
void l25_gc_shutdown(void);

// 分配 GC 管理的对象
//   size:     用户对象大小（不含 GC 头部）
//   scan_fn:  扫描函数（NULL 表示本对象无需扫描指针字段）
//   dtor_fn:  析构函数（NULL 表示无需析构）
// 返回：用户数据指针（已零初始化）
// 每次分配时推进增量标记（处理若干灰色对象）
void* l25_gc_alloc(size_t size, l25_gc_scan_fn scan_fn, l25_gc_dtor_fn dtor_fn);

// 注册一个 GC 根（兼容接口，内部使用根栈）
void l25_gc_add_root(void** root);

// 移除一个 GC 根（兼容接口，内部使用根栈 LIFO 弹出）
void l25_gc_remove_root(void** root);

// 手动触发垃圾回收（完整的标记-清除周期）
void l25_gc_collect(void);

// 确定性析构（用于 delete 语句）
// 调用析构函数并标记对象为"已析构"，但不释放内存。
// 实际内存释放由 GC sweep 阶段统一处理，避免悬挂指针。
void l25_gc_free(void* ptr);

// 写屏障（Dijkstra snapshot-at-the-beginning 变体）
// 当向 GC 管理的对象的指针字段写入新值时调用。
// 如果增量标记正在进行，将新指针对象标灰以保证不遗漏。
void l25_gc_write_barrier(void* new_ptr);

#ifdef __cplusplus
}
#endif

#endif // L25_GC_H
