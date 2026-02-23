#include "l25_runtime.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* ===== String methods runtime =====
 * L25 strings are { i32 len, i8* data }.
 * Functions that return a new string return a malloc'd char*
 * and write the new length to *out_len.
 */

/* substr(data, data_len, pos, sub_len, out_len) → char*  */
char* l25_string_substr(const char* data, int32_t data_len,
                        int32_t pos, int32_t sub_len,
                        int32_t* out_len)
{
    if (pos < 0) pos = 0;
    if (pos >= data_len) {
        *out_len = 0;
        char* r = (char*)malloc(1);
        r[0] = '\0';
        return r;
    }
    if (sub_len < 0 || pos + sub_len > data_len)
        sub_len = data_len - pos;
    *out_len = sub_len;
    char* r = (char*)malloc((size_t)sub_len + 1);
    memcpy(r, data + pos, (size_t)sub_len);
    r[sub_len] = '\0';
    return r;
}

/* find(haystack, haystack_len, needle, needle_len) → int32_t index, -1 if not found */
int32_t l25_string_find(const char* haystack, int32_t haystack_len,
                        const char* needle, int32_t needle_len)
{
    (void)haystack_len;
    (void)needle_len;
    const char* p = strstr(haystack, needle);
    if (!p) return -1;
    return (int32_t)(p - haystack);
}

/* char_at(data, data_len, index) → int32_t ASCII value, -1 if out of range */
int32_t l25_string_char_at(const char* data, int32_t data_len, int32_t index)
{
    if (index < 0 || index >= data_len) return -1;
    return (int32_t)(unsigned char)data[index];
}

/* to_upper(data, data_len, out_len) → char* (malloc'd) */
char* l25_string_to_upper(const char* data, int32_t data_len, int32_t* out_len)
{
    *out_len = data_len;
    char* r = (char*)malloc((size_t)data_len + 1);
    for (int32_t i = 0; i < data_len; ++i)
        r[i] = (char)toupper((unsigned char)data[i]);
    r[data_len] = '\0';
    return r;
}

/* to_lower(data, data_len, out_len) → char* (malloc'd) */
char* l25_string_to_lower(const char* data, int32_t data_len, int32_t* out_len)
{
    *out_len = data_len;
    char* r = (char*)malloc((size_t)data_len + 1);
    for (int32_t i = 0; i < data_len; ++i)
        r[i] = (char)tolower((unsigned char)data[i]);
    r[data_len] = '\0';
    return r;
}

/* replace(data, data_len, old_s, old_len, new_s, new_len, out_len) → char*
 * Replaces the FIRST occurrence of old_s in data. Returns data copy if not found. */
char* l25_string_replace(const char* data, int32_t data_len,
                         const char* old_s, int32_t old_len,
                         const char* new_s, int32_t new_len,
                         int32_t* out_len)
{
    const char* p = strstr(data, old_s);
    if (!p) {
        /* no match — return a copy */
        *out_len = data_len;
        char* r = (char*)malloc((size_t)data_len + 1);
        memcpy(r, data, (size_t)data_len + 1);
        return r;
    }
    int32_t prefix = (int32_t)(p - data);
    int32_t result_len = data_len - old_len + new_len;
    *out_len = result_len;
    char* r = (char*)malloc((size_t)result_len + 1);
    memcpy(r, data, (size_t)prefix);
    memcpy(r + prefix, new_s, (size_t)new_len);
    memcpy(r + prefix + new_len, p + old_len, (size_t)(data_len - prefix - old_len));
    r[result_len] = '\0';
    return r;
}

/* contains(haystack, haystack_len, needle, needle_len) → int32_t (0 or 1) */
int32_t l25_string_contains(const char* haystack, int32_t haystack_len,
                            const char* needle, int32_t needle_len)
{
    (void)haystack_len;
    (void)needle_len;
    return strstr(haystack, needle) ? 1 : 0;
}
