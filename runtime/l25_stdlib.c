/*
 * l25_stdlib.c  –  系统调用相关的内置函数
 *
 * sleep_ms(int ms)     – 毫秒级休眠
 * exit(int code)       – 进程退出
 * rand() -> int        – 伪随机整数 [0, RAND_MAX]
 * srand(int seed)      – 设置随机种子
 */

#include <stdlib.h>
#include <time.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

void l25_sleep_ms(int ms)
{
#ifdef _WIN32
    Sleep(ms);
#else
    struct timespec ts;
    ts.tv_sec  = ms / 1000;
    ts.tv_nsec = (ms % 1000) * 1000000L;
    nanosleep(&ts, NULL);
#endif
}

void l25_exit(int code)
{
    exit(code);
}

int l25_rand(void)
{
    return rand();
}

void l25_srand(int seed)
{
    srand((unsigned int)seed);
}
