#include "l25_runtime.h"
#include <stdlib.h>
#include <string.h>

#define VEC_INIT_CAP 8

typedef struct {
    void*   data;
    int64_t size;
    int64_t capacity;
    int64_t elem_size;
} L25Vector;

/* ---------- create / destroy ---------- */

void* l25_vector_create(int64_t elem_size) {
    L25Vector* v = (L25Vector*)malloc(sizeof(L25Vector));
    v->elem_size = elem_size;
    v->size      = 0;
    v->capacity  = VEC_INIT_CAP;
    v->data      = calloc((size_t)VEC_INIT_CAP, (size_t)elem_size);
    return v;
}

void l25_vector_destroy(void* vec) {
    if (!vec) return;
    L25Vector* v = (L25Vector*)vec;
    free(v->data);
    free(v);
}

/* ---------- push / pop ---------- */

static void vec_grow(L25Vector* v) {
    int64_t new_cap = v->capacity * 2;
    void* new_data = calloc((size_t)new_cap, (size_t)v->elem_size);
    memcpy(new_data, v->data, (size_t)(v->size * v->elem_size));
    free(v->data);
    v->data     = new_data;
    v->capacity = new_cap;
}

void l25_vector_push(void* vec, const void* elem) {
    L25Vector* v = (L25Vector*)vec;
    if (v->size >= v->capacity) vec_grow(v);
    memcpy((char*)v->data + v->size * v->elem_size, elem, (size_t)v->elem_size);
    v->size++;
}

void l25_vector_pop(void* vec, void* out_elem) {
    L25Vector* v = (L25Vector*)vec;
    if (v->size <= 0) return; /* no-op on empty */
    v->size--;
    memcpy(out_elem, (char*)v->data + v->size * v->elem_size, (size_t)v->elem_size);
}

/* ---------- get / set ---------- */

void* l25_vector_get(void* vec, int64_t index) {
    L25Vector* v = (L25Vector*)vec;
    return (char*)v->data + index * v->elem_size;
}

void l25_vector_set(void* vec, int64_t index, const void* elem) {
    L25Vector* v = (L25Vector*)vec;
    memcpy((char*)v->data + index * v->elem_size, elem, (size_t)v->elem_size);
}

/* ---------- length ---------- */

int64_t l25_vector_len(void* vec) {
    L25Vector* v = (L25Vector*)vec;
    return v->size;
}
