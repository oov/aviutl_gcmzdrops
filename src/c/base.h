#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifndef __has_c_attribute
#define __has_c_attribute(x) 0
#endif
#ifndef __has_attribute
#define __has_attribute(x) 0
#endif

#if __has_c_attribute(nodiscard)
#define NODISCARD [[nodiscard]]
#elif __has_attribute(warn_unused_result)
#define NODISCARD __attribute__((warn_unused_result))
#else
#define NODISCARD
#endif

#ifdef __FILE_NAME__
#define SOURCE_CODE_FILE_NAME __FILE_NAME__
#else
char const *base_find_file_name(char const *s);
#define SOURCE_CODE_FILE_NAME (base_find_file_name(__FILE__))
#endif

struct base_filepos
{
  char const *file;
  char const *func;
  long line;
};

void base_init(void);
void base_exit(void);

#define ERR_FILEPOS_PARAMS , struct base_filepos const *const filepos
#define ERR_FILEPOS_VALUES , (&(const struct base_filepos){.file = SOURCE_CODE_FILE_NAME, .func = __func__, .line = __LINE__})
#define ERR_FILEPOS_VALUES_PASSTHRU , filepos

#ifdef ALLOCATE_LOGGER
#define MEM_FILEPOS_PARAMS ERR_FILEPOS_PARAMS
#define MEM_FILEPOS_VALUES ERR_FILEPOS_VALUES
#define MEM_FILEPOS_VALUES_PASSTHRU ERR_FILEPOS_VALUES_PASSTHRU
#else
#define MEM_FILEPOS_PARAMS
#define MEM_FILEPOS_VALUES
#define MEM_FILEPOS_VALUES_PASSTHRU
#endif

#ifdef _WIN32
#define NATIVE_CHAR wchar_t
#define NATIVE_STR wstr
#define NSTR(str) L##str
#define native_unmanaged(char_ptr) (wstr_unmanaged(char_ptr))
#ifndef USE_WSTR
#define USE_WSTR
#endif
#else
#define NATIVE_CHAR char
#define NATIVE_STR str
#define NSTR(str) str
#define native_unmanaged(char_ptr) (str_unmanaged(char_ptr))
#ifndef USE_STR
#define USE_STR
#endif
#endif

#ifdef USE_STR
#include <string.h> // strlen
struct str
{
  char *ptr;
  size_t len;
  size_t cap;
};
#define str_unmanaged(char_ptr) ((struct str){.ptr = (char *)(char_ptr), .len = strlen((char_ptr))})
#endif

#ifdef USE_WSTR
#include <wchar.h> // wcslen
struct wstr
{
  wchar_t *ptr;
  size_t len;
  size_t cap;
};
#define wstr_unmanaged(wchar_ptr) ((struct wstr){.ptr = (wchar_t *)(wchar_ptr), .len = wcslen((wchar_ptr))})
#endif

struct error
{
  int type;
  uint_least32_t code;
  struct NATIVE_STR msg;
  struct base_filepos filepos;

  struct error *next;
};
typedef struct error *error;

// error

enum err_type
{
  err_type_generic = 0,
};

enum err_generic
{
  err_pass_through = 0,
  err_fail = 1,
  err_unexpected = 2,
  err_invalid_arugment = 3,
  err_null_pointer = 4,
  err_out_of_memory = 5,
  err_not_sufficient_buffer = 6,
  err_not_found = 7,
  err_abort = 8,
  err_not_implemented_yet = 9,
};

typedef error (*error_message_mapper)(uint_least32_t const code, struct NATIVE_STR *const message);
NODISCARD error error_register_message_mapper(int const type, error_message_mapper fn);

typedef void (*error_message_reporter)(error err, struct NATIVE_STR const *const msg, struct base_filepos const *const filepos);
void error_default_reporter(error const e, struct NATIVE_STR const *const message, struct base_filepos const *const filepos);
void error_register_reporter(error_message_reporter fn);

NODISCARD error error_add_(error const parent, int const type, uint_least32_t const code, struct NATIVE_STR const *const msg ERR_FILEPOS_PARAMS);
bool error_free_(error *const e MEM_FILEPOS_PARAMS);
NODISCARD static inline bool error_is_(error const err, int const type, uint_least32_t const code)
{
  return err != NULL && err->type == type && err->code == code;
}
NODISCARD error error_to_string_short(error const e, struct NATIVE_STR *const dest);
NODISCARD error error_to_string(error const e, struct NATIVE_STR *const dest);
bool error_report_(error const e, struct NATIVE_STR const *const message ERR_FILEPOS_PARAMS);
bool error_report_free_(error e, struct NATIVE_STR const *const message ERR_FILEPOS_PARAMS);

#define err(type, code) (error_add_(NULL, (type), (code), NULL ERR_FILEPOS_VALUES))
#define errg(code) (err(err_type_generic, (code)))
#define efree(err_ptr) (error_free_((err_ptr)MEM_FILEPOS_VALUES))
#define emsg(type, code, struct_native_str_ptr) (error_add_(NULL, (type), (code), (struct_native_str_ptr)ERR_FILEPOS_VALUES))
#define ethru(parent) (error_add_((parent), err_type_generic, err_pass_through, NULL ERR_FILEPOS_VALUES))
NODISCARD static inline error eok()
{
  return NULL;
}
NODISCARD static inline bool esucceeded(error const err)
{
  return err == NULL;
}
NODISCARD static inline bool efailed(error const err)
{
  return err != NULL;
}
NODISCARD static inline bool eis(error const err, int const type, uint_least32_t const code)
{
  return error_is_(err, type, code);
}
NODISCARD static inline bool eisg(error const err, uint_least32_t const code)
{
  return eis(err, err_type_generic, code);
}
#define ereportmsg(err, struct_native_str_ptr) (error_report_free_((err), (struct_native_str_ptr)ERR_FILEPOS_VALUES))
#define ereport(err) (error_report_free_((err), &native_unmanaged(NSTR("Error occurred.")) ERR_FILEPOS_VALUES))

// Do not use eignore for normal use cases.
// ereport is appropriate for that.
static inline bool eignore(error err)
{
  if (efailed(err))
  {
    efree(&err);
    return false;
  }
  return true;
}

// mem

NODISCARD error mem_(void *const pp, size_t const n, size_t const item_size MEM_FILEPOS_PARAMS);
NODISCARD error mem_free_(void *const pp MEM_FILEPOS_PARAMS);
#define mem(pp, n, item_size) (mem_((pp), (n), (item_size)MEM_FILEPOS_VALUES))
#define mem_free(pp) (mem_free_((pp)MEM_FILEPOS_VALUES))

#ifdef LEAK_DETECTOR
long mem_get_allocated_count(void);
#endif

// array

struct array
{
  void *ptr; // (ptr != NULL && cap == 0) is unmanaged memory
  size_t len;
  size_t cap;
};

bool array_grow_core_(struct array *const p, size_t const elem_size, size_t const least_size MEM_FILEPOS_PARAMS);
NODISCARD error array_grow_(struct array *const p, size_t const elem_size, size_t const least_size MEM_FILEPOS_PARAMS);
void array_free_core_(struct array *const p MEM_FILEPOS_PARAMS);
NODISCARD error array_free_(struct array *const p MEM_FILEPOS_PARAMS);
static inline size_t array_len_(struct array const *const p)
{
  return p ? p->len : 0;
}
static inline size_t array_cap_(struct array const *const p)
{
  return p ? p->cap : 0;
}
#define alen(array_ptr) (array_len_((struct array *)(array_ptr)))
#define acap(array_ptr) (array_cap_((struct array *)(array_ptr)))
#define afree(array_ptr) (array_free_((struct array *)(array_ptr)MEM_FILEPOS_VALUES))
#define agrow(array_ptr, least_size) (array_grow_((struct array *)(array_ptr), sizeof(*(array_ptr)->ptr), (size_t)(least_size)MEM_FILEPOS_VALUES))
#define apush(array_ptr, item) ((array_ptr) ? !((array_ptr)->ptr && !(array_ptr)->cap) ? array_grow_core_((struct array *)(array_ptr), sizeof(*(array_ptr)->ptr), (array_ptr)->len + 1 MEM_FILEPOS_VALUES) ? ((array_ptr)->ptr[(array_ptr)->len++] = (item), eok()) : errg(err_out_of_memory) : errg(err_unexpected) : errg(err_invalid_arugment))
#define apop(array_ptr, item_ptr) ((array_ptr) ? (array_ptr)->len ? (*(item_ptr) = (array_ptr)->ptr[--(array_ptr)->len], eok()) : errg(err_not_found) : errg(err_invalid_arugment))
#define achop(array_ptr) ((array_ptr) ? (array_ptr)->len ? (--(array_ptr)->len, eok()) : errg(err_not_found) : errg(err_invalid_arugment))

// str

#ifdef USE_STR
NODISCARD error str_cpy_(struct str *const s, char const *const s2 MEM_FILEPOS_PARAMS);
NODISCARD error str_ncpy_(struct str *const s, char const *const s2, size_t s2len MEM_FILEPOS_PARAMS);
NODISCARD error str_cat_(struct str *const s, char const *const s2 MEM_FILEPOS_PARAMS);
NODISCARD error str_ncat_(struct str *const s, char const *const s2, size_t s2len MEM_FILEPOS_PARAMS);
NODISCARD error str_str_(struct str const *const s, char const *const s2, int *pos);
NODISCARD error str_replace_all_(struct str *const s, char const *const find, char const *const replacement MEM_FILEPOS_PARAMS);
#endif

// wstr

#ifdef USE_WSTR
NODISCARD error wstr_cpy_(struct wstr *const ws, wchar_t const *const ws2 MEM_FILEPOS_PARAMS);
NODISCARD error wstr_ncpy_(struct wstr *const ws, wchar_t const *const ws2, size_t ws2len MEM_FILEPOS_PARAMS);
NODISCARD error wstr_cat_(struct wstr *const ws, wchar_t const *const ws2 MEM_FILEPOS_PARAMS);
NODISCARD error wstr_ncat_(struct wstr *const ws, wchar_t const *const ws2, size_t ws2len MEM_FILEPOS_PARAMS);
NODISCARD error wstr_str_(struct wstr const *const ws, wchar_t const *const ws2, int *pos);
NODISCARD error wstr_replace_all_(struct wstr *const ws, wchar_t const *const find, wchar_t const *const replacement MEM_FILEPOS_PARAMS);
#endif

#define sfree(struct_str_ptr) afree((struct_str_ptr))
#define sgrow(struct_str_ptr, cap) agrow((struct_str_ptr), (cap))
#if defined(USE_STR) && defined(USE_WSTR)
#define scpy(struct_str_ptr, char_ptr) _Generic( \
    (struct_str_ptr),                            \
    struct wstr *                                \
    : wstr_cpy_,                                 \
      struct wstr *const                         \
    : wstr_cpy_,                                 \
      struct str *                               \
    : str_cpy_,                                  \
      struct str *const                          \
    : str_cpy_)((struct_str_ptr), (char_ptr)MEM_FILEPOS_VALUES)
#define sncpy(struct_str_ptr, char_ptr, size_t) _Generic( \
    (struct_str_ptr),                                     \
    struct wstr *                                         \
    : wstr_ncpy_,                                         \
      struct wstr *const                                  \
    : wstr_ncpy_,                                         \
      struct str *                                        \
    : str_ncpy_,                                          \
      struct str *const                                   \
    : str_ncpy_)((struct_str_ptr), (char_ptr), (size_t)MEM_FILEPOS_VALUES)
#define scat(struct_str_ptr, char_ptr) _Generic( \
    (struct_str_ptr),                            \
    struct wstr *                                \
    : wstr_cat_,                                 \
      struct wstr *const                         \
    : wstr_cat_,                                 \
      struct str *                               \
    : str_cat_,                                  \
      struct str *const                          \
    : str_cat_)((struct_str_ptr), (char_ptr)MEM_FILEPOS_VALUES)
#define sncat(struct_str_ptr, char_ptr, size_t) _Generic( \
    (struct_str_ptr),                                     \
    struct wstr *                                         \
    : wstr_ncat_,                                         \
      struct wstr *const                                  \
    : wstr_ncat_,                                         \
      struct str *                                        \
    : str_ncat_,                                          \
      struct str *const                                   \
    : str_ncat_)((struct_str_ptr), (char_ptr), (size_t)MEM_FILEPOS_VALUES)
#define sstr(struct_str_ptr, char_ptr, int_ptr) _Generic( \
    (struct_str_ptr),                                     \
    struct wstr *                                         \
    : wstr_str_,                                          \
      struct wstr *const                                  \
    : wstr_str_,                                          \
      struct wstr const *                                 \
    : wstr_str_,                                          \
      struct wstr const *const                            \
    : wstr_str_,                                          \
      struct str *                                        \
    : str_str_,                                           \
      struct str *const                                   \
    : str_str_,                                           \
      struct str const *                                  \
    : str_str_,                                           \
      struct str const *const                             \
    : str_str_)((struct_str_ptr), (char_ptr), (int_ptr))
#define sreplace_all(struct_str_ptr, char_ptr_find, char_ptr_replacement) _Generic( \
    (struct_str_ptr),                                                               \
    struct wstr *                                                                   \
    : wstr_replace_all_,                                                            \
      struct wstr *const                                                            \
    : wstr_replace_all_,                                                            \
      struct str *                                                                  \
    : str_replace_all_,                                                             \
      struct str *const                                                             \
    : str_replace_all_)((struct_str_ptr), (char_ptr_find), (char_ptr_replacement)MEM_FILEPOS_VALUES)
#elif defined(USE_STR)
#define scpy(struct_str_ptr, char_ptr) _Generic( \
    (struct_str_ptr),                            \
    struct str *                                 \
    : str_cpy_,                                  \
      struct str *const                          \
    : str_cpy_)((struct_str_ptr), (char_ptr)MEM_FILEPOS_VALUES)
#define sncpy(struct_str_ptr, char_ptr, size_t) _Generic( \
    (struct_str_ptr),                                     \
    struct str *                                          \
    : str_ncpy_,                                          \
      struct str *const                                   \
    : str_ncpy_)((struct_str_ptr), (char_ptr), (size_t)MEM_FILEPOS_VALUES)
#define scat(struct_str_ptr, char_ptr) _Generic( \
    (struct_str_ptr),                            \
    struct str *                                 \
    : str_cat_,                                  \
      struct str *const                          \
    : str_cat_)((struct_str_ptr), (char_ptr)MEM_FILEPOS_VALUES)
#define sncat(struct_str_ptr, char_ptr, size_t) _Generic( \
    (struct_str_ptr),                                     \
    struct str *                                          \
    : str_ncat_,                                          \
      struct str *const                                   \
    : str_ncat_)((struct_str_ptr), (char_ptr), (size_t)MEM_FILEPOS_VALUES)
#define sstr(struct_str_ptr, char_ptr, int_ptr) _Generic( \
    (struct_str_ptr),                                     \
    struct str const *                                    \
    : str_str_,                                           \
      struct str const *const                             \
    : str_str_,                                           \
      struct str *                                        \
    : str_str_,                                           \
      struct str *const                                   \
    : str_str_)((struct_str_ptr), (char_ptr), (int_ptr))
#define sreplace_all(struct_str_ptr, char_ptr_find, char_ptr_replacement) _Generic( \
    (struct_str_ptr),                                                               \
    struct str *                                                                    \
    : str_replace_all_,                                                             \
      struct str *const                                                             \
    : str_replace_all_)((struct_str_ptr), (char_ptr_find), (char_ptr_replacement)MEM_FILEPOS_VALUES)
#elif defined(USE_WSTR)
#define scpy(struct_str_ptr, char_ptr) _Generic( \
    (struct_str_ptr),                            \
    struct wstr *                                \
    : wstr_cpy_,                                 \
      struct wstr *const                         \
    : wstr_cpy_)((struct_str_ptr), (char_ptr)MEM_FILEPOS_VALUES)
#define sncpy(struct_str_ptr, char_ptr, size_t) _Generic( \
    (struct_str_ptr),                                     \
    struct wstr *                                         \
    : wstr_ncpy_,                                         \
      struct wstr *const                                  \
    : wstr_ncpy_)((struct_str_ptr), (char_ptr), (size_t)MEM_FILEPOS_VALUES)
#define scat(struct_str_ptr, char_ptr) _Generic( \
    (struct_str_ptr),                            \
    struct wstr *                                \
    : wstr_cat_,                                 \
      struct wstr *const                         \
    : wstr_cat_)((struct_str_ptr), (char_ptr)MEM_FILEPOS_VALUES)
#define sncat(struct_str_ptr, char_ptr, size_t) _Generic( \
    (struct_str_ptr),                                     \
    struct wstr *                                         \
    : wstr_ncat_,                                         \
      struct wstr *const                                  \
    : wstr_ncat_)((struct_str_ptr), (char_ptr), (size_t)MEM_FILEPOS_VALUES)
#define sstr(struct_str_ptr, char_ptr, int_ptr) _Generic( \
    (struct_str_ptr),                                     \
    struct wstr const *                                   \
    : wstr_str_,                                          \
      struct wstr const *const                            \
    : wstr_str_,                                          \
      struct wstr *                                       \
    : wstr_str_,                                          \
      struct wstr *const                                  \
    : wstr_str_)((struct_str_ptr), (char_ptr), (int_ptr))
#define sreplace_all(struct_str_ptr, char_ptr_find, char_ptr_replacement) _Generic( \
    (struct_str_ptr),                                                               \
    struct wstr *                                                                   \
    : wstr_replace_all_,                                                            \
      struct wstr *const                                                            \
    : wstr_replace_all_)((struct_str_ptr), (char_ptr_find), (char_ptr_replacement)MEM_FILEPOS_VALUES)
#endif

// hash map

typedef void (*hm_get_key)(void const *const item, void const **const key, size_t *const key_bytes);

struct hmap
{
  void *ptr;
  union
  {
    hm_get_key get_key;
    size_t size;
  };
};

NODISCARD error hmap_new_dynamic(struct hmap *const hm, size_t const item_size, size_t const cap, hm_get_key const get_key MEM_FILEPOS_PARAMS);
NODISCARD error hmap_new_static(struct hmap *const hm, size_t const item_size, size_t const cap, size_t const key_size MEM_FILEPOS_PARAMS);
NODISCARD error hmap_free(struct hmap *const hm MEM_FILEPOS_PARAMS);
NODISCARD error hmap_clear(struct hmap *const hm);
NODISCARD error hmap_count(struct hmap const *const hm, size_t *const dest);
NODISCARD error hmap_get(struct hmap *const hm, void const *const key_item, void **const item);
NODISCARD error hmap_set(struct hmap *const hm, void const *const item MEM_FILEPOS_PARAMS);
NODISCARD error hmap_delete(struct hmap *const hm, void const *const key_item MEM_FILEPOS_PARAMS);
NODISCARD error hmap_scan(struct hmap *const hm, bool (*iter)(void const *const item, void *const udata), void *const udata);
#define hmnewd(struct_hmap_ptr, item_size, cap, get_key_fn) hmap_new_dynamic((struct_hmap_ptr), (item_size), (cap), (get_key_fn)MEM_FILEPOS_VALUES)
#define hmnews(struct_hmap_ptr, item_size, cap, key_size) hmap_new_static((struct_hmap_ptr), (item_size), (cap), (key_size)MEM_FILEPOS_VALUES)
#define hmfree(struct_hmap_ptr) hmap_free((struct_hmap_ptr)MEM_FILEPOS_VALUES)
#define hmclear(struct_hmap_ptr) hmap_clear((struct_hmap_ptr))
#define hmcount(struct_hmap_ptr, size_t_ptr) hmap_count((struct_hmap_ptr), (size_t_ptr))
#define hmget(struct_hmap_ptr, key_item_ptr, item_ptr_ptr) hmap_get((struct_hmap_ptr), (key_item_ptr), (void **)(item_ptr_ptr))
#define hmset(struct_hmap_ptr, item_ptr) hmap_set((struct_hmap_ptr), (item_ptr)MEM_FILEPOS_VALUES)
#define hmdelete(struct_hmap_ptr, key_item_ptr) hmap_delete((struct_hmap_ptr), (key_item_ptr)MEM_FILEPOS_VALUES)
#define hmscan(struct_hmap_ptr, iter, udata_ptr) hmap_scan((struct_hmap_ptr), (iter), (udata_ptr)MEM_FILEPOS_VALUES)

// https://xoshiro.di.unimi.it/splitmix64.c
static inline uint64_t base_splitmix64(uint64_t x)
{
  x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9;
  x = (x ^ (x >> 27)) * 0x94d049bb133111eb;
  return x ^ (x >> 31);
}

static inline uint64_t base_splitmix64_next(uint64_t const x)
{
  return x + 0x9e3779b97f4a7c15;
}

// https://github.com/skeeto/hash-prospector
static inline uint32_t base_splitmix32(uint32_t x)
{
  x = (x ^ (x >> 16)) * 0x7feb352d;
  x = (x ^ (x >> 15)) * 0x846ca68b;
  return x ^ (x >> 16);
}

static inline uint32_t base_splitmix32_next(uint32_t const x)
{
  return x + 0x9e3779b9;
}
