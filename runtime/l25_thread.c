/*
 * L25 Runtime — Thread Pool (goroutine-like spawn support)
 *
 * 固定大小的工作线程池，通过任务队列调度 spawn 块。
 * 主线程在程序退出前等待所有未完成的任务。
 */
#include "l25_runtime.h"
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

/* ===== 任务节点 ===== */
typedef struct Task {
    void (*fn)(void*);
    void* arg;
    struct Task* next;
} Task;

/* ===== 线程池 ===== */
typedef struct {
    pthread_t*      threads;
    int             num_threads;
    Task*           task_head;
    Task*           task_tail;
    pthread_mutex_t mutex;
    pthread_cond_t  has_task;      /* 有新任务可取 */
    pthread_cond_t  all_done;      /* 所有任务完成 */
    int             shutdown;
    int             active_tasks;  /* 正在执行 + 队列中的任务数 */
} ThreadPool;

static ThreadPool* pool = NULL;

/* ===== 工作线程主循环 ===== */
static void* worker_main(void* arg) {
    (void)arg;
    for (;;) {
        pthread_mutex_lock(&pool->mutex);

        /* 等待任务或关闭信号 */
        while (!pool->task_head && !pool->shutdown) {
            pthread_cond_wait(&pool->has_task, &pool->mutex);
        }

        if (pool->shutdown && !pool->task_head) {
            pthread_mutex_unlock(&pool->mutex);
            break;
        }

        /* 取出队列头部任务 */
        Task* task = pool->task_head;
        pool->task_head = task->next;
        if (!pool->task_head) pool->task_tail = NULL;

        pthread_mutex_unlock(&pool->mutex);

        /* 执行任务 */
        task->fn(task->arg);
        free(task);

        /* 完成计数 */
        pthread_mutex_lock(&pool->mutex);
        pool->active_tasks--;
        if (pool->active_tasks == 0) {
            pthread_cond_signal(&pool->all_done);
        }
        pthread_mutex_unlock(&pool->mutex);
    }
    return NULL;
}

/* ===== 公共 API ===== */

void l25_thread_pool_init(void) {
    if (pool) return;

    pool = (ThreadPool*)calloc(1, sizeof(ThreadPool));

    /* 线程数 = CPU 核心数（至少 2，最多 16） */
    int ncpu = (int)sysconf(_SC_NPROCESSORS_ONLN);
    if (ncpu < 2) ncpu = 2;
    if (ncpu > 16) ncpu = 16;
    pool->num_threads = ncpu;

    pool->threads = (pthread_t*)calloc(pool->num_threads, sizeof(pthread_t));
    pthread_mutex_init(&pool->mutex, NULL);
    pthread_cond_init(&pool->has_task, NULL);
    pthread_cond_init(&pool->all_done, NULL);
    pool->shutdown     = 0;
    pool->active_tasks = 0;
    pool->task_head    = NULL;
    pool->task_tail    = NULL;

    for (int i = 0; i < pool->num_threads; i++) {
        pthread_create(&pool->threads[i], NULL, worker_main, NULL);
    }
}

void l25_thread_spawn(void (*fn)(void*), void* arg) {
    if (!pool) l25_thread_pool_init();

    Task* task  = (Task*)malloc(sizeof(Task));
    task->fn    = fn;
    task->arg   = arg;
    task->next  = NULL;

    pthread_mutex_lock(&pool->mutex);
    pool->active_tasks++;
    if (pool->task_tail) {
        pool->task_tail->next = task;
    } else {
        pool->task_head = task;
    }
    pool->task_tail = task;
    pthread_cond_signal(&pool->has_task);
    pthread_mutex_unlock(&pool->mutex);
}

void l25_thread_pool_shutdown(void) {
    if (!pool) return;

    /* 先等待所有任务完成 */
    pthread_mutex_lock(&pool->mutex);
    while (pool->active_tasks > 0) {
        pthread_cond_wait(&pool->all_done, &pool->mutex);
    }
    pool->shutdown = 1;
    pthread_cond_broadcast(&pool->has_task);
    pthread_mutex_unlock(&pool->mutex);

    /* 等待所有工作线程退出 */
    for (int i = 0; i < pool->num_threads; i++) {
        pthread_join(pool->threads[i], NULL);
    }

    pthread_mutex_destroy(&pool->mutex);
    pthread_cond_destroy(&pool->has_task);
    pthread_cond_destroy(&pool->all_done);
    free(pool->threads);
    free(pool);
    pool = NULL;
}
