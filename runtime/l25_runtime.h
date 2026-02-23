#ifndef L25_RUNTIME_H
#define L25_RUNTIME_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ===== Key type tags (used by map hash/compare) ===== */
#define L25_KEY_INT    0
#define L25_KEY_FLOAT  1
#define L25_KEY_STRING 2
#define L25_KEY_PTR    3
#define L25_KEY_OTHER  4

/* ===== Vector API ===== */

void*   l25_vector_create(int64_t elem_size);
void    l25_vector_destroy(void* vec);
void    l25_vector_push(void* vec, const void* elem);
void    l25_vector_pop(void* vec, void* out_elem);
void*   l25_vector_get(void* vec, int64_t index);
void    l25_vector_set(void* vec, int64_t index, const void* elem);
int64_t l25_vector_len(void* vec);

/* ===== Map API ===== */

void*   l25_map_create(int64_t key_size, int64_t val_size, int32_t key_type);
void    l25_map_destroy(void* map);
void    l25_map_set(void* map, const void* key, const void* val);
void*   l25_map_get(void* map, const void* key);
int32_t l25_map_contains(void* map, const void* key);
void    l25_map_erase(void* map, const void* key);
int64_t l25_map_len(void* map);

/* ===== Deque API ===== */

void*   l25_deque_create(int64_t elem_size);
void    l25_deque_destroy(void* deq);
void    l25_deque_push_front(void* deq, const void* elem);
void    l25_deque_push_back(void* deq, const void* elem);
void    l25_deque_pop_front(void* deq, void* out_elem);
void    l25_deque_pop_back(void* deq, void* out_elem);
void*   l25_deque_get(void* deq, int64_t index);
void    l25_deque_set(void* deq, int64_t index, const void* elem);
void*   l25_deque_front(void* deq);
void*   l25_deque_back(void* deq);
int64_t l25_deque_len(void* deq);

/* ===== Queue API ===== */

void*   l25_queue_create(int64_t elem_size);
void    l25_queue_destroy(void* que);
void    l25_queue_push(void* que, const void* elem);
void    l25_queue_pop(void* que, void* out_elem);
void*   l25_queue_front(void* que);
void*   l25_queue_back(void* que);
int64_t l25_queue_len(void* que);

/* ===== Channel API ===== */

void*   l25_channel_create(int64_t elem_size, int64_t capacity);
void    l25_channel_destroy(void* ch);
void    l25_channel_send(void* ch, const void* elem);
void    l25_channel_recv(void* ch, void* out_elem);
int32_t l25_channel_recv_ok(void* ch, void* out_elem); /* 返回 ok: 1=成功, 0=已关闭且空 */
int64_t l25_channel_len(void* ch);
void    l25_channel_close(void* ch);
int32_t l25_channel_closed(void* ch); /* 返回 1=已关闭, 0=未关闭 */
int32_t l25_channel_try_send(void* ch, const void* elem); /* 非阻塞, 1=成功 */
int32_t l25_channel_try_recv(void* ch, void* out_elem);   /* 非阻塞, 1=成功 */

/* ===== Thread Pool API ===== */

void    l25_thread_pool_init(void);
void    l25_thread_spawn(void (*fn)(void*), void* arg);
void    l25_thread_pool_shutdown(void);

/* ===== Networking API ===== */

int32_t l25_net_tcp_listen(int32_t port);
int32_t l25_net_tcp_accept(int32_t listen_fd);
int32_t l25_net_tcp_connect(const char* host, int32_t port);
int32_t l25_net_tcp_send(int32_t fd, const char* data, int32_t len);
char*   l25_net_tcp_recv(int32_t fd, int32_t max_len, int32_t* out_len);
void    l25_net_close(int32_t fd);
int32_t l25_net_udp_socket(void);
int32_t l25_net_udp_bind(int32_t fd, int32_t port);
int32_t l25_net_udp_sendto(int32_t fd, const char* host, int32_t port, const char* data, int32_t len);
char*   l25_net_udp_recvfrom(int32_t fd, int32_t max_len, int32_t* out_len);

/* ===== String API ===== */

char*   l25_string_substr(const char* data, int32_t data_len, int32_t pos, int32_t sub_len, int32_t* out_len);
int32_t l25_string_find(const char* haystack, int32_t h_len, const char* needle, int32_t n_len);
int32_t l25_string_char_at(const char* data, int32_t data_len, int32_t index);
char*   l25_string_to_upper(const char* data, int32_t data_len, int32_t* out_len);
char*   l25_string_to_lower(const char* data, int32_t data_len, int32_t* out_len);
char*   l25_string_replace(const char* data, int32_t data_len, const char* old_s, int32_t old_len, const char* new_s, int32_t new_len, int32_t* out_len);
int32_t l25_string_contains(const char* haystack, int32_t h_len, const char* needle, int32_t n_len);
char*   l25_readln(int32_t* out_len);
char*   l25_itos(int32_t val, int32_t* out_len);

/* ===== Clock API ===== */

float   l25_clock_ms(void);  /* CLOCK_MONOTONIC 毫秒数 */

/* ===== Stdlib API ===== */

void    l25_sleep_ms(int ms);   /* 毫秒级休眠 */
void    l25_exit(int code);     /* 进程退出 */
int     l25_rand(void);         /* 伪随机整数 [0, RAND_MAX] */
void    l25_srand(int seed);    /* 设置随机种子 */

#ifdef __cplusplus
}
#endif

#endif /* L25_RUNTIME_H */
