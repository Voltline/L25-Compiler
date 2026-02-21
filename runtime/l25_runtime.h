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

#ifdef __cplusplus
}
#endif

#endif /* L25_RUNTIME_H */
