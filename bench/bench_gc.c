/*
 * L25 GC 基准测试
 * 直接调用 GC 运行时 API，测量各操作的耗时。
 *
 * 编译: make bench_gc
 * 运行: ./bench_gc
 */
#include "l25_gc.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// ===== 计时辅助 =====
static double now_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000.0 + ts.tv_nsec / 1e6;
}

#define BENCH_BEGIN(label) \
    do { \
        const char* _label = (label); \
        double _t0 = now_ms();

#define BENCH_END() \
        double _t1 = now_ms(); \
        printf("  %-42s %10.3f ms\n", _label, _t1 - _t0); \
    } while(0)

// ===== 模拟对象类型 =====

// 无指针字段的简单对象（如 int 值节点）
typedef struct { int value; } SimpleObj;

// 含 1 个指针字段的节点（链表）
typedef struct {
    int   value;
    void* next;   // 指向另一个 ListNode
} ListNode;

static void listnode_scan(void* obj, void (*mark_fn)(void*)) {
    ListNode* n = (ListNode*)obj;
    if (n->next) mark_fn(n->next);
}

// 含 2 个指针字段的节点（二叉树）
typedef struct {
    int   value;
    void* left;
    void* right;
} TreeNode;

static void treenode_scan(void* obj, void (*mark_fn)(void*)) {
    TreeNode* n = (TreeNode*)obj;
    if (n->left)  mark_fn(n->left);
    if (n->right) mark_fn(n->right);
}

// 析构计数器
static int dtor_count = 0;
static void counting_dtor(void* obj) {
    (void)obj;
    dtor_count++;
}

// ===================================================
// Benchmark 1: 纯分配吞吐量（无 scan_fn, 无 dtor）
// ===================================================
static void bench_alloc_simple(int n) {
    l25_gc_init();
    // 用一个栈数组做根，防止 GC 回收
    if (n > L25_ROOT_STACK_MAX) n = L25_ROOT_STACK_MAX;
    void** roots = (void**)calloc(n, sizeof(void*));

    BENCH_BEGIN("alloc (simple, no scan)") {
        for (int i = 0; i < n; i++) {
            roots[i] = l25_gc_alloc(sizeof(SimpleObj), NULL, NULL);
            l25_gc_add_root(&roots[i]);
        }
    } BENCH_END();

    printf("    -> %d objects, each %zu bytes (+ GC header)\n", n, sizeof(SimpleObj));

    for (int i = n - 1; i >= 0; i--) l25_gc_remove_root(&roots[i]);
    free(roots);
    l25_gc_shutdown();
}

// ===================================================
// Benchmark 2: 分配带 scan_fn 对象
// ===================================================
static void bench_alloc_scannable(int n) {
    l25_gc_init();
    if (n > L25_ROOT_STACK_MAX) n = L25_ROOT_STACK_MAX;
    void** roots = (void**)calloc(n, sizeof(void*));

    BENCH_BEGIN("alloc (with scan_fn)") {
        for (int i = 0; i < n; i++) {
            roots[i] = l25_gc_alloc(sizeof(ListNode), listnode_scan, NULL);
            l25_gc_add_root(&roots[i]);
        }
    } BENCH_END();

    printf("    -> %d objects with scan_fn\n", n);

    for (int i = n - 1; i >= 0; i--) l25_gc_remove_root(&roots[i]);
    free(roots);
    l25_gc_shutdown();
}

// ===================================================
// Benchmark 3: STW 完整回收（N 存活对象, M 垃圾对象）
// ===================================================
static void bench_collect_stw(int n_live, int n_garbage) {
    l25_gc_init();
    void** live = (void**)calloc(n_live, sizeof(void*));

    // 分配存活对象并注册根
    for (int i = 0; i < n_live; i++) {
        live[i] = l25_gc_alloc(sizeof(ListNode), listnode_scan, counting_dtor);
        l25_gc_add_root(&live[i]);
    }
    // 建链: live[0] -> live[1] -> ... -> live[n-1]
    for (int i = 0; i < n_live - 1; i++) {
        ((ListNode*)live[i])->next = live[i + 1];
    }

    // 分配垃圾对象（无根引用）
    dtor_count = 0;
    for (int i = 0; i < n_garbage; i++) {
        l25_gc_alloc(sizeof(ListNode), listnode_scan, counting_dtor);
    }

    BENCH_BEGIN("STW collect") {
        l25_gc_collect();
    } BENCH_END();

    printf("    -> %d live, %d garbage, %d dtors called\n", n_live, n_garbage, dtor_count);

    for (int i = n_live - 1; i >= 0; i--) l25_gc_remove_root(&live[i]);
    free(live);
    l25_gc_shutdown();
}

// ===================================================
// Benchmark 4: 增量 GC 摊销开销（alloc 内部推进标记）
// ===================================================
static void bench_incremental_amortized(int n_total) {
    l25_gc_init();
    // 保持 ~100 个活跃对象，其余成为垃圾
    int keep = 100;
    void** ring = (void**)calloc(keep, sizeof(void*));
    for (int i = 0; i < keep; i++) {
        l25_gc_add_root(&ring[i]);
    }

    BENCH_BEGIN("incremental alloc (churn)") {
        for (int i = 0; i < n_total; i++) {
            int slot = i % keep;
            ring[slot] = l25_gc_alloc(sizeof(ListNode), listnode_scan, NULL);
        }
    } BENCH_END();

    printf("    -> %d total allocs, %d live slots\n", n_total, keep);

    for (int i = keep - 1; i >= 0; i--) l25_gc_remove_root(&ring[i]);
    free(ring);
    l25_gc_shutdown();
}

// ===================================================
// Benchmark 5: 根集操作吞吐量
// ===================================================
static void bench_root_ops(int n) {
    l25_gc_init();
    // 限制在根栈容量以内
    if (n > L25_ROOT_STACK_MAX) n = L25_ROOT_STACK_MAX;
    void** slots = (void**)calloc(n, sizeof(void*));

    BENCH_BEGIN("root add") {
        for (int i = 0; i < n; i++) {
            l25_gc_add_root(&slots[i]);
        }
    } BENCH_END();

    BENCH_BEGIN("root remove (LIFO)") {
        for (int i = n - 1; i >= 0; i--) {
            l25_gc_remove_root(&slots[i]);
        }
    } BENCH_END();

    printf("    -> %d root add + remove\n", n);

    free(slots);
    l25_gc_shutdown();
}

// ===================================================
// Benchmark 6: 写屏障开销
// ===================================================
static void bench_write_barrier(int n) {
    l25_gc_init();

    // 分配一些对象供写屏障使用
    int n_objs = 1000;
    void** objs = (void**)calloc(n_objs, sizeof(void*));
    for (int i = 0; i < n_objs; i++) {
        objs[i] = l25_gc_alloc(sizeof(ListNode), listnode_scan, NULL);
        l25_gc_add_root(&objs[i]);
    }

    // 测试 IDLE 阶段（写屏障应迅速返回）
    BENCH_BEGIN("write_barrier (IDLE, no-op)") {
        for (int i = 0; i < n; i++) {
            l25_gc_write_barrier(objs[i % n_objs]);
        }
    } BENCH_END();

    // 强制进入 MARKING 阶段
    // 先生成大量垃圾以达到触发阈值，再手动分配一次触发 gc_start_cycle
    // 更简单的做法: 使用一个大分配突破阈值
    void* big = l25_gc_alloc(1024 * 128, NULL, NULL);  // 触发 GC 周期
    void* big_root = big;
    l25_gc_add_root((void**)&big_root);
    // 此时 gc_step 应该启动了一个周期，但我们不确定处于哪个阶段
    // 使用 collect 来重新启动
    l25_gc_collect();

    // 再次分配大量垃圾以超过阈值
    for (int i = 0; i < 2000; i++) {
        l25_gc_alloc(sizeof(ListNode), listnode_scan, NULL);
    }
    // 此时增量推进可能已触发，但不确定
    // 我们改为测量在混合阶段下的写屏障
    BENCH_BEGIN("write_barrier (mixed phase)") {
        for (int i = 0; i < n; i++) {
            l25_gc_write_barrier(objs[i % n_objs]);
        }
    } BENCH_END();

    printf("    -> %d write_barrier calls\n", n);

    l25_gc_remove_root((void**)&big_root);
    for (int i = n_objs - 1; i >= 0; i--) l25_gc_remove_root(&objs[i]);
    free(objs);
    l25_gc_shutdown();
}

// ===================================================
// Benchmark 7: 二叉树构建 + 回收
// ===================================================
static void* build_tree(int depth) {
    void* node = l25_gc_alloc(sizeof(TreeNode), treenode_scan, counting_dtor);
    ((TreeNode*)node)->value = depth;
    if (depth > 0) {
        ((TreeNode*)node)->left  = build_tree(depth - 1);
        ((TreeNode*)node)->right = build_tree(depth - 1);
    }
    return node;
}

static void bench_tree(int depth) {
    l25_gc_init();
    int nnodes = (1 << (depth + 1)) - 1;

    void* root = NULL;
    l25_gc_add_root((void**)&root);

    BENCH_BEGIN("tree build") {
        root = build_tree(depth);
    } BENCH_END();

    printf("    -> depth=%d, %d nodes\n", depth, nnodes);

    // 丢弃树引用，回收
    dtor_count = 0;
    root = NULL;

    BENCH_BEGIN("tree collect (STW)") {
        l25_gc_collect();
    } BENCH_END();

    printf("    -> %d dtors called\n", dtor_count);

    l25_gc_remove_root((void**)&root);
    l25_gc_shutdown();
}

// ===================================================
// Benchmark 8: 链表构建 + 增量回收
// ===================================================
static void bench_linked_list(int n) {
    l25_gc_init();
    void* head = NULL;
    l25_gc_add_root((void**)&head);

    BENCH_BEGIN("linked list build") {
        for (int i = 0; i < n; i++) {
            ListNode* node = (ListNode*)l25_gc_alloc(sizeof(ListNode), listnode_scan, NULL);
            node->value = i;
            node->next = head;
            l25_gc_write_barrier(head);  // 写屏障
            head = node;
        }
    } BENCH_END();

    printf("    -> %d nodes\n", n);

    // 断开引用
    head = NULL;

    BENCH_BEGIN("linked list collect (STW)") {
        l25_gc_collect();
    } BENCH_END();

    l25_gc_remove_root((void**)&head);
    l25_gc_shutdown();
}

// ===================================================
// Benchmark 9: 大量短生命周期对象的分配-回收循环
// ===================================================
static void bench_churn_cycles(int rounds, int objs_per_round) {
    l25_gc_init();
    void* dummy = NULL;
    l25_gc_add_root((void**)&dummy);

    int total_collected = 0;

    BENCH_BEGIN("churn (alloc + collect cycles)") {
        for (int r = 0; r < rounds; r++) {
            // 分配短命对象（无根，立即成为垃圾）
            for (int i = 0; i < objs_per_round; i++) {
                dummy = l25_gc_alloc(sizeof(TreeNode), treenode_scan, NULL);
            }
            // 丢弃根
            dummy = NULL;
            l25_gc_collect();
            total_collected += objs_per_round;
        }
    } BENCH_END();

    printf("    -> %d rounds x %d objs = %d total\n", rounds, objs_per_round, total_collected);

    l25_gc_remove_root((void**)&dummy);
    l25_gc_shutdown();
}

// =========================
// Main
// =========================
int main(void) {
    printf("=== L25 GC Benchmark (tri-color incremental mark-sweep) ===\n\n");

    printf("[1] Allocation throughput\n");
    bench_alloc_simple(100000);
    printf("\n");
    bench_alloc_scannable(100000);
    printf("\n");

    printf("[2] STW collection\n");
    bench_collect_stw(1000, 10000);
    printf("\n");
    bench_collect_stw(10000, 100000);
    printf("\n");

    printf("[3] Incremental amortized cost\n");
    bench_incremental_amortized(500000);
    printf("\n");

    printf("[4] Root set operations\n");
    bench_root_ops(100000);
    printf("\n");

    printf("[5] Write barrier\n");
    bench_write_barrier(1000000);
    printf("\n");

    printf("[6] Binary tree\n");
    bench_tree(15);     // 65535 nodes
    printf("\n");
    bench_tree(18);     // 524287 nodes
    printf("\n");

    printf("[7] Linked list\n");
    bench_linked_list(100000);
    printf("\n");

    printf("[8] Churn (short-lived objects)\n");
    bench_churn_cycles(100, 10000);
    printf("\n");

    printf("=== Done ===\n");
    return 0;
}
