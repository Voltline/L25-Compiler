/*
 * l25_clock.c  –  高精度计时 API
 *
 * clock_ms()  返回自首次调用以来的毫秒数 (float)
 *             首次调用返回 0.0，后续返回相对偏移
 *             使用相对值保证 float32 有足够精度
 */

#include <time.h>

static struct timespec l25_clock_base;
static int l25_clock_initialized = 0;

float l25_clock_ms(void)
{
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    if (!l25_clock_initialized) {
        l25_clock_base = ts;
        l25_clock_initialized = 1;
    }
    double sec  = (double)(ts.tv_sec  - l25_clock_base.tv_sec);
    double nsec = (double)(ts.tv_nsec - l25_clock_base.tv_nsec);
    return (float)(sec * 1000.0 + nsec / 1000000.0);
}
