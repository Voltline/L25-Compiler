#include "l25_runtime.h"
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define MAP_INIT_CAP   16
#define MAP_LOAD_NUM   3   /* load factor = 3/4 = 75 % */
#define MAP_LOAD_DEN   4

/* Slot states */
#define SLOT_EMPTY   0
#define SLOT_USED    1
#define SLOT_DELETED 2

/* L25 string layout: { int32_t len, <4‑byte pad>, char* data } */
typedef struct { int32_t len; int32_t _pad; char* data; } L25String;

typedef struct {
    void*    keys;       /* flat buffer: capacity * key_size  */
    void*    vals;       /* flat buffer: capacity * val_size  */
    uint8_t* states;     /* per‑slot state                    */
    int64_t  capacity;
    int64_t  size;       /* number of SLOT_USED entries       */
    int64_t  key_size;
    int64_t  val_size;
    int32_t  key_type;
} L25Map;

/* ------------------------------------------------------------------ */
/*  FNV‑1a hash                                                       */
/* ------------------------------------------------------------------ */
static uint64_t fnv1a(const void* data, int64_t len) {
    const uint8_t* p = (const uint8_t*)data;
    uint64_t h = 14695981039346656037ULL;
    for (int64_t i = 0; i < len; ++i) {
        h ^= p[i];
        h *= 1099511628211ULL;
    }
    return h;
}

static uint64_t hash_key(const void* key, int64_t key_size, int32_t key_type) {
    if (key_type == L25_KEY_STRING) {
        const L25String* s = (const L25String*)key;
        if (!s->data) return 0;
        return fnv1a(s->data, s->len);
    }
    return fnv1a(key, key_size);
}

static int keys_equal(const void* a, const void* b,
                      int64_t key_size, int32_t key_type) {
    if (key_type == L25_KEY_STRING) {
        const L25String* sa = (const L25String*)a;
        const L25String* sb = (const L25String*)b;
        if (sa->len != sb->len) return 0;
        if (!sa->data || !sb->data) return sa->data == sb->data;
        return memcmp(sa->data, sb->data, (size_t)sa->len) == 0;
    }
    return memcmp(a, b, (size_t)key_size) == 0;
}

/* ------------------------------------------------------------------ */
/*  Internal helpers                                                   */
/* ------------------------------------------------------------------ */
static void* key_at(L25Map* m, int64_t i)
{ return (char*)m->keys + i * m->key_size; }

static void* val_at(L25Map* m, int64_t i)
{ return (char*)m->vals + i * m->val_size; }

/* Find slot: returns index.  *found = 1 if existing key. */
static int64_t probe(L25Map* m, const void* key, int* found) {
    uint64_t h = hash_key(key, m->key_size, m->key_type);
    int64_t mask = m->capacity - 1; /* capacity is always a power of 2 */
    int64_t idx = (int64_t)(h & (uint64_t)mask);
    int64_t first_deleted = -1;

    for (int64_t i = 0; i < m->capacity; ++i) {
        int64_t pos = (idx + i) & mask;
        uint8_t st = m->states[pos];
        if (st == SLOT_EMPTY) {
            *found = 0;
            return (first_deleted >= 0) ? first_deleted : pos;
        }
        if (st == SLOT_DELETED) {
            if (first_deleted < 0) first_deleted = pos;
            continue;
        }
        /* SLOT_USED */
        if (keys_equal(key_at(m, pos), key, m->key_size, m->key_type)) {
            *found = 1;
            return pos;
        }
    }
    /* table full (shouldn't happen with proper load factor) */
    *found = 0;
    return (first_deleted >= 0) ? first_deleted : 0;
}

static void map_rehash(L25Map* m) {
    int64_t old_cap = m->capacity;
    void*    old_keys   = m->keys;
    void*    old_vals   = m->vals;
    uint8_t* old_states = m->states;

    int64_t new_cap = old_cap * 2;
    m->capacity = new_cap;
    m->size     = 0;
    m->keys   = calloc((size_t)new_cap, (size_t)m->key_size);
    m->vals   = calloc((size_t)new_cap, (size_t)m->val_size);
    m->states = (uint8_t*)calloc((size_t)new_cap, 1);

    for (int64_t i = 0; i < old_cap; ++i) {
        if (old_states[i] == SLOT_USED) {
            int found;
            int64_t pos = probe(m, (char*)old_keys + i * m->key_size, &found);
            memcpy(key_at(m, pos), (char*)old_keys + i * m->key_size, (size_t)m->key_size);
            memcpy(val_at(m, pos), (char*)old_vals + i * m->val_size, (size_t)m->val_size);
            m->states[pos] = SLOT_USED;
            m->size++;
        }
    }
    free(old_keys);
    free(old_vals);
    free(old_states);
}

/* ------------------------------------------------------------------ */
/*  Public API                                                         */
/* ------------------------------------------------------------------ */

void* l25_map_create(int64_t key_size, int64_t val_size, int32_t key_type) {
    L25Map* m = (L25Map*)malloc(sizeof(L25Map));
    m->key_size  = key_size;
    m->val_size  = val_size;
    m->key_type  = key_type;
    m->capacity  = MAP_INIT_CAP;
    m->size      = 0;
    m->keys   = calloc(MAP_INIT_CAP, (size_t)key_size);
    m->vals   = calloc(MAP_INIT_CAP, (size_t)val_size);
    m->states = (uint8_t*)calloc(MAP_INIT_CAP, 1);
    return m;
}

void l25_map_destroy(void* map) {
    if (!map) return;
    L25Map* m = (L25Map*)map;
    free(m->keys);
    free(m->vals);
    free(m->states);
    free(m);
}

void l25_map_set(void* map, const void* key, const void* val) {
    L25Map* m = (L25Map*)map;
    /* Check load factor BEFORE insert */
    if ((m->size + 1) * MAP_LOAD_DEN > m->capacity * MAP_LOAD_NUM) {
        map_rehash(m);
    }
    int found;
    int64_t pos = probe(m, key, &found);
    memcpy(key_at(m, pos), key, (size_t)m->key_size);
    memcpy(val_at(m, pos), val, (size_t)m->val_size);
    if (!found) {
        m->states[pos] = SLOT_USED;
        m->size++;
    }
}

void* l25_map_get(void* map, const void* key) {
    L25Map* m = (L25Map*)map;
    int found;
    int64_t pos = probe(m, key, &found);
    if (!found) {
        /* Auto‑insert zero entry (like C++ operator[]) */
        if ((m->size + 1) * MAP_LOAD_DEN > m->capacity * MAP_LOAD_NUM) {
            map_rehash(m);
            pos = probe(m, key, &found);
        }
        memcpy(key_at(m, pos), key, (size_t)m->key_size);
        memset(val_at(m, pos), 0, (size_t)m->val_size);
        m->states[pos] = SLOT_USED;
        m->size++;
    }
    return val_at(m, pos);
}

int32_t l25_map_contains(void* map, const void* key) {
    L25Map* m = (L25Map*)map;
    int found;
    probe(m, key, &found);
    return found ? 1 : 0;
}

void l25_map_erase(void* map, const void* key) {
    L25Map* m = (L25Map*)map;
    int found;
    int64_t pos = probe(m, key, &found);
    if (found) {
        m->states[pos] = SLOT_DELETED;
        m->size--;
    }
}

int64_t l25_map_len(void* map) {
    L25Map* m = (L25Map*)map;
    return m->size;
}
