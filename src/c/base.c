#include "base.h"

#ifdef _WIN32

#define NEWLINE NSTR("\r\n")
#include <combaseapi.h> // CoTaskMemRealloc
#ifdef _CONSOLE
#include <stdio.h> // fwprintf
#endif

#define REALLOC(ptr, size) (CoTaskMemRealloc(ptr, size))
#define FREE(ptr) (CoTaskMemRealloc(ptr, 0))

#else

#define NEWLINE NSTR("\n")
#include <stdlib.h> // realloc, free
#include <stdio.h>  // fprintf

#define REALLOC(ptr, size) (realloc(ptr, size))
#define FREE(ptr) (free(ptr))

#endif

#if __STDC_VERSION__ >= 201112L && !defined(__STDC_NO_THREADS__)
#include <threads.h>
#else
#include "3rd/threads/threads.h"
#endif

#include "3rd/hashmap/hashmap.h"
#include "3rd/hashmap/hashmap.c"

#ifndef __FILE_NAME__
char const *base_find_file_name(char const *s)
{
  char const *found = s;
  for (; *s != '\0'; ++s)
  {
    if (*s == '/' || *s == '\\')
    {
      found = s + 1;
    }
  }
  return found;
}
#endif

static void report(NATIVE_CHAR const *const str)
{
#ifdef _WIN32
#ifdef _CONSOLE
  // https://docs.microsoft.com/en-us/windows/console/writeconsole
  // WriteConsole fails if it is used with a standard handle that is redirected to a file.
  // If an application processes multilingual output that can be redirected,
  // determine whether the output handle is a console handle
  // (one method is to call the GetConsoleMode function and check whether it succeeds).
  // If the handle is a console handle, call WriteConsole.
  // If the handle is not a console handle, the output is redirected and
  // you should call WriteFile to perform the I/O.
  // Be sure to prefix a Unicode plain text file with a byte order mark.
  // For more information, see Using Byte Order Marks.
  HANDLE const h = GetStdHandle(STD_ERROR_HANDLE);
  size_t const len = wcslen(str);
  if (GetConsoleMode(h, &(DWORD){0}))
  {
    WriteConsoleW(h, str, len, NULL, NULL);
  }
  else
  {
    size_t const plen = WideCharToMultiByte(CP_UTF8, 0, str, len, NULL, 0, NULL, NULL);
    if (plen)
    {
      void *const p = CoTaskMemRealloc(NULL, plen);
      if (WideCharToMultiByte(CP_UTF8, 0, str, len, p, plen, NULL, NULL))
      {
        WriteFile(h, p, plen, NULL, NULL);
      }
      CoTaskMemRealloc(p, 0);
    }
  }
#else
  OutputDebugStringW(str);
#endif

#else
  fprintf(stderr, "%s" NEWLINE, str);
#endif
}

// mem

#ifdef ALLOCATE_LOGGER
static mtx_t g_mem_mtx = {0};
struct hashmap *g_allocated = NULL;
struct allocated_at
{
  void const *const p;
  struct base_filepos const filepos;
};

static void *am_malloc(size_t const s, void *const udata)
{
  (void)udata;
  return REALLOC(NULL, s);
}
static void am_free(void *const p, void *const udata)
{
  (void)udata;
  FREE(p);
}
static uint64_t am_hash(void const *const item, uint64_t const seed0, uint64_t const seed1, void *const udata)
{
  (void)udata;
  struct allocated_at const *const aa = item;
  return hashmap_sip(&aa->p, sizeof(void *), seed0, seed1);
}
static int am_compare(void const *const a, void const *const b, void *udata)
{
  struct allocated_at const *const aa0 = a;
  struct allocated_at const *const aa1 = b;
  (void)udata;
  return (char *)aa1->p - (char *)aa0->p;
}

void allocate_logger_init(void)
{
  mtx_init(&g_mem_mtx, mtx_plain);
  uint64_t hash = base_splitmix64_next(GetTickCount() + GetCurrentProcessId() + GetCurrentThreadId());
  uint64_t const s0 = base_splitmix64(hash);
  hash = base_splitmix64_next(hash);
  uint64_t const s1 = base_splitmix64(hash);
  g_allocated = hashmap_new_with_allocator(
      am_malloc,
      am_free,
      sizeof(struct allocated_at),
      8,
      s0,
      s1,
      am_hash,
      am_compare,
      NULL,
      NULL);
  if (!g_allocated)
  {
    abort();
  }
}

void allocate_logger_exit(void)
{
  hashmap_free(g_allocated);
  g_allocated = NULL;
  mtx_destroy(&g_mem_mtx);
}

static void allocated_put(void const *const p MEM_FILEPOS_PARAMS)
{
  hashmap_set(
      g_allocated,
      &(struct allocated_at){
          .p = p,
          .filepos = *filepos,
      });
  if (hashmap_oom(g_allocated))
  {
    ereportmsg(errg(err_unexpected), &native_unmanaged(NSTR("failed to record allocated memory.")));
  }
}

static void allocated_remove(void const *const p)
{
  struct allocated_at *const aa = hashmap_delete(g_allocated, &(struct allocated_at){.p = p});
  if (aa == NULL)
  {
    ereportmsg(errg(err_unexpected), &native_unmanaged(NSTR("double free detected.")));
  }
}

static bool report_leaks_iterate(void const *const item, void *const udata)
{
  size_t *const n = udata;
  ++*n;
  struct allocated_at const *const aa = item;
  NATIVE_CHAR buf[1024] = {0};
#ifdef _WIN32
  wsprintfW(buf, "Leak #%u: %hs:%d %hs()" NEWLINE, *n, aa->filepos.file, aa->filepos.line, aa->filepos.func);
#else
  sprintf(buf, "Leak #%u: %s:%d %s()" NEWLINE, *n, aa->filepos.file, aa->filepos.line, aa->filepos.func);
#endif
  ereportmsg(errg(err_unexpected), &native_unmanaged(buf));
  return true;
}

static size_t report_leaks(void)
{
  size_t n = 0;
  mtx_lock(&g_mem_mtx);
  hashmap_scan(g_allocated, report_leaks_iterate, &n);
  mtx_unlock(&g_mem_mtx);
  return n;
}

#endif

#ifdef LEAK_DETECTOR
#include <stdatomic.h>
static atomic_long g_allocated_count = 0;

long mem_get_allocated_count(void)
{
  return atomic_load_explicit(&g_allocated_count, memory_order_relaxed);
}
static inline void allocated(void)
{
  atomic_fetch_add_explicit(&g_allocated_count, 1, memory_order_relaxed);
}
static inline void freed(void)
{
  atomic_fetch_sub_explicit(&g_allocated_count, 1, memory_order_relaxed);
}
static void report_allocated_count(void)
{
  long const n = mem_get_allocated_count();
  if (!n)
  {
    return;
  }
  NATIVE_CHAR buf[64] = {0};
#ifdef _WIN32
  wsprintfW(buf, L"Not freed memory blocks: %d" NEWLINE, n);
#else
  sprintf(buf, "Not freed memory blocks: %d" NEWLINE, n);
#endif
  ereportmsg(errg(err_unexpected), &native_unmanaged(buf));
}
#endif

static bool mem_core_(void *const pp, size_t const sz MEM_FILEPOS_PARAMS)
{
  if (sz == 0)
  {
    if (*(void **)pp == NULL)
    {
      return false;
    }
    FREE(*(void **)pp);
#ifdef LEAK_DETECTOR
    freed();
#endif
#ifdef ALLOCATE_LOGGER
    mtx_lock(&g_mem_mtx);
    allocated_remove(*(void **)pp);
    mtx_unlock(&g_mem_mtx);
#endif
    *(void **)pp = NULL;
    return true;
  }
  void *np = REALLOC(*(void **)pp, sz);
  if (!np)
  {
    return false;
  }
  if (*(void **)pp == NULL)
  {
#ifdef LEAK_DETECTOR
    allocated();
#endif
#ifdef ALLOCATE_LOGGER
    mtx_lock(&g_mem_mtx);
    allocated_put(np MEM_FILEPOS_VALUES_PASSTHRU);
    mtx_unlock(&g_mem_mtx);
#endif
  }
  else
  {
#ifdef ALLOCATE_LOGGER
    mtx_lock(&g_mem_mtx);
    allocated_remove(*(void **)pp);
    allocated_put(np MEM_FILEPOS_VALUES_PASSTHRU);
    mtx_unlock(&g_mem_mtx);
#endif
  }
  *(void **)pp = np;
  return true;
}

error mem_(void *const pp, size_t const n, size_t const item_size MEM_FILEPOS_PARAMS)
{
  if (!pp || !item_size)
  {
    return errg(err_invalid_arugment);
  }
  if (!mem_core_(pp, n * item_size MEM_FILEPOS_VALUES_PASSTHRU))
  {
    return errg(err_out_of_memory);
  }
  return eok();
}

error mem_free_(void *const pp MEM_FILEPOS_PARAMS)
{
  if (!pp)
  {
    return errg(err_invalid_arugment);
  }
  mem_core_(pp, 0 MEM_FILEPOS_VALUES_PASSTHRU);
  return eok();
}

// error

void error_default_reporter(error const e, struct NATIVE_STR const *const message, struct base_filepos const *const filepos)
{
  struct NATIVE_STR tmp = {0};
  struct NATIVE_STR msg = {0};
  error err = error_to_string(e, &tmp);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(&msg, message->ptr);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }
  NATIVE_CHAR buf[1024] = {0};
#ifdef _WIN32
  wsprintfW(buf, NEWLINE NSTR("(reported at %hs:%d %hs())") NEWLINE, filepos->file, filepos->line, filepos->func);
#else
  sprintf(buf, NEWLINE NSTR("(reported at %s:%d %s())") NEWLINE, filepos->file, filepos->line, filepos->func);
#endif
  err = scat(&msg, buf);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }
  err = scat(&msg, tmp.ptr);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }
  report(msg.ptr);

cleanup:
  if (efailed(err))
  {
    report(NSTR("failed to report error"));
    efree(&err);
  }
  eignore(sfree(&msg));
  eignore(sfree(&tmp));
}

static mtx_t g_error_mtx = {0};
static struct hmap g_error_message_mapper = {0};
static error_message_reporter g_error_reporter = error_default_reporter;

struct error_message_mapping
{
  int type;
  NODISCARD error_message_mapper get;
};

NODISCARD static error generic_error_message(uint_least32_t const code, struct NATIVE_STR *const message)
{
  switch (code)
  {
  case err_fail:
    return scpy(message, NSTR("処理に失敗しました。"));
  case err_unexpected:
    return scpy(message, NSTR("予期しないエラーです。"));
  case err_invalid_arugment:
    return scpy(message, NSTR("引数が間違っています。"));
  case err_null_pointer:
    return scpy(message, NSTR("ポインターが割り当てられていません。"));
  case err_out_of_memory:
    return scpy(message, NSTR("メモリーが確保できません。"));
  case err_not_sufficient_buffer:
    return scpy(message, NSTR("バッファが小さすぎます。"));
  case err_not_found:
    return scpy(message, NSTR("対象が見つかりませんでした。"));
  case err_abort:
    return scpy(message, NSTR("中断されました。"));
  case err_not_implemented_yet:
    return scpy(message, NSTR("実装されていません。"));
  }
  return scpy(message, NSTR("未知のエラーコードです。"));
}

static void error_init(void)
{
  mtx_init(&g_error_mtx, mtx_plain);

  error err = hmnews(&g_error_message_mapper, sizeof(struct error_message_mapping), 0, sizeof(int));
  if (efailed(err))
  {
    goto failed;
  }
  err = error_register_message_mapper(err_type_generic, generic_error_message);
  if (efailed(err))
  {
    goto failed;
  }
  return;

failed:
  efree(&err);
  report(NSTR("failed to initialize error system"));
  abort();
}

static void error_exit(void)
{
  ereport(hmfree(&g_error_message_mapper));
  mtx_destroy(&g_error_mtx);
}

static error find_last_error(error e)
{
  if (!e)
  {
    return NULL;
  }
  while (e->next != NULL)
  {
    e = e->next;
  }
  return e;
}

error error_register_message_mapper(int const type, error_message_mapper fn)
{
  mtx_lock(&g_error_mtx);
  error err = hmset(
      &g_error_message_mapper,
      (&(struct error_message_mapping){
          .type = type,
          .get = fn,
      }));
  mtx_unlock(&g_error_mtx);
  return err;
}

void error_register_reporter(error_message_reporter const fn)
{
  mtx_lock(&g_error_mtx);
  g_error_reporter = fn ? fn : error_default_reporter;
  mtx_unlock(&g_error_mtx);
}

NODISCARD static error error_get_registered_message_mapper(int const type, struct error_message_mapping **const em)
{
  mtx_lock(&g_error_mtx);
  error err = hmget(
      &g_error_message_mapper,
      &(struct error_message_mapping){.type = type},
      em);
  mtx_unlock(&g_error_mtx);
  return err;
}

error error_add_(error const parent, int const type, uint_least32_t const code, struct NATIVE_STR const *const msg ERR_FILEPOS_PARAMS)
{
  error new_error = eok();
  error err = mem_(&new_error, 1, sizeof(struct error) MEM_FILEPOS_VALUES_PASSTHRU);
  if (efailed(err))
  {
    err = ethru(err);
    return err;
  }
  *new_error = (struct error){
      .type = type,
      .code = code,
      .msg = msg ? *msg : (struct NATIVE_STR){0},
      .filepos = *filepos,
  };
  error last_error = find_last_error(parent);
  if (last_error)
  {
    last_error->next = new_error;
    return parent;
  }
  return new_error;
}

bool error_free_(error *const e MEM_FILEPOS_PARAMS)
{
  if (!e)
  {
    return false;
  }
  error ee = *e;
  while (ee != NULL)
  {
    error next = ee->next;
    array_free_core_((struct array *)&ee->msg MEM_FILEPOS_VALUES_PASSTHRU);
    mem_core_(&ee, 0 MEM_FILEPOS_VALUES_PASSTHRU);
    ee = next;
  }
  *e = NULL;
  return true;
}

error error_to_string_short(error e, struct NATIVE_STR *const dest)
{
  if (e == NULL || (e->type == err_type_generic && e->code == err_pass_through))
  {
    return errg(err_invalid_arugment);
  }
  struct NATIVE_STR tmp = {0};
  struct error_message_mapping *em = NULL;
  error err = error_get_registered_message_mapper(e->type, &em);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }
  err = em->get(e->code, &tmp);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }
  if (e->msg.len)
  {
    err = scat(&tmp, NEWLINE);
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
    err = scpy(&tmp, e->msg.ptr);
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
  }
  err = scpy(dest, tmp.ptr);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&tmp));
  return err;
}

error error_to_string(error e, struct NATIVE_STR *const dest)
{
  NATIVE_CHAR buf[1024] = {0};
  struct NATIVE_STR tmp = {0};

  error err = error_to_string_short(e, &tmp);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }
#ifdef _WIN32
  wsprintfW(buf, NEWLINE NSTR("(error code: %02X:0x%08X)") NEWLINE NSTR("  %hs:%d %hs()"), e->type, e->code, e->filepos.file, e->filepos.line, e->filepos.func);
#else
  sprintf(buf, NEWLINE NSTR("(error code: %02X:0x%08X)") NEWLINE NSTR("  %hs:%d %hs()"), e->type, e->code, e->filepos.file, e->filepos.line, e->filepos.func);
#endif
  err = scat(&tmp, buf);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }
  for (e = e->next; e != NULL; e = e->next)
  {
    if (e->type != err_type_generic || e->code != err_pass_through)
    {
      err = emsg(err_type_generic, err_unexpected, &native_unmanaged(NSTR("incorrect error structure.")));
      goto cleanup;
    }
#ifdef _WIN32
    wsprintfW(buf, NEWLINE NSTR("  %hs:%d %hs()"), e->filepos.file, e->filepos.line, e->filepos.func);
#else
    sprintf(buf, NEWLINE NSTR("  %hs:%d %hs()"), e->filepos.file, e->filepos.line, e->filepos.func);
#endif
    err = scat(&tmp, buf);
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
  }
  err = scpy(dest, tmp.ptr);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  return err;
}

bool error_report_(error const e, struct NATIVE_STR const *const message ERR_FILEPOS_PARAMS)
{
  if (esucceeded(e))
  {
    return true;
  }

  mtx_lock(&g_error_mtx);
  error_message_reporter fn = g_error_reporter;
  mtx_unlock(&g_error_mtx);
  fn(e, message ERR_FILEPOS_VALUES_PASSTHRU);
  return false;
}

bool error_report_free_(error e, struct NATIVE_STR const *const message ERR_FILEPOS_PARAMS)
{
  bool r = error_report_(e, message ERR_FILEPOS_VALUES_PASSTHRU);
  if (!r)
  {
    error_free_(&e MEM_FILEPOS_VALUES_PASSTHRU);
  }
  return r;
}

// array

bool array_grow_core_(struct array *const p, size_t const elem_size, size_t const least_size MEM_FILEPOS_PARAMS)
{
  enum
  {
    block_size = 8,
  };
  if (p->cap >= least_size)
  {
    return true;
  }
  size_t const newcap = (least_size + block_size - 1) & ~(block_size - 1);
  if (!mem_core_(&p->ptr, newcap * elem_size MEM_FILEPOS_VALUES_PASSTHRU))
  {
    return false;
  }
  memset((char *)(p->ptr) + p->cap * elem_size, 0, (newcap - p->cap) * elem_size);
  p->cap = newcap;
  return true;
}

error array_grow_(struct array *const p, size_t const elem_size, size_t const least_size MEM_FILEPOS_PARAMS)
{
  if (!p)
  {
    return errg(err_invalid_arugment);
  }
  if (p->ptr && p->cap == 0)
  {
    return emsg(err_type_generic, err_unexpected, &native_unmanaged(NSTR("Unmanaged pointer cannot be grown.")));
  }
  return array_grow_core_(p, elem_size, least_size MEM_FILEPOS_VALUES_PASSTHRU) ? eok() : errg(err_out_of_memory);
}

void array_free_core_(struct array *const p MEM_FILEPOS_PARAMS)
{
  if (p->ptr)
  {
    if (p->cap == 0)
    {
      p->ptr = NULL; // unmanaged pointer
    }
    else
    {
      mem_core_(&p->ptr, 0 MEM_FILEPOS_VALUES_PASSTHRU);
    }
  }
  p->len = 0;
  p->cap = 0;
}

error array_free_(struct array *const p MEM_FILEPOS_PARAMS)
{
  if (!p)
  {
    return errg(err_invalid_arugment);
  }
  array_free_core_(p MEM_FILEPOS_VALUES_PASSTHRU);
  return eok();
}

// str

NODISCARD static inline error str_grow(struct str *const s, size_t const least_size MEM_FILEPOS_PARAMS)
{
  return array_grow_((struct array *)s, sizeof(char), least_size MEM_FILEPOS_VALUES_PASSTHRU);
}

error str_cpy_(struct str *const s, const char *s2 MEM_FILEPOS_PARAMS)
{
  if (!s || !s2)
  {
    return errg(err_invalid_arugment);
  }

  size_t const s2len = strlen(s2);
  error err = str_grow(s, s2len + 1 MEM_FILEPOS_VALUES_PASSTHRU);
  if (efailed(err))
  {
    err = ethru(err);
    return err;
  }
  strcpy(s->ptr, s2);
  s->len = s2len;
  return eok();
}

error str_ncpy_(struct str *const s, char const *const s2, size_t const s2len MEM_FILEPOS_PARAMS)
{
  if (!s || !s2 || s2len < 0)
  {
    return errg(err_invalid_arugment);
  }

  error err = str_grow(s, s2len + 1 MEM_FILEPOS_VALUES_PASSTHRU);
  if (efailed(err))
  {
    err = ethru(err);
    return err;
  }
  strncpy(s->ptr, s2, s2len);
  s->len = strlen(s->ptr);
  return eok();
}

error str_cat_(struct str *const s, char const *const s2 MEM_FILEPOS_PARAMS)
{
  if (!s || !s2)
  {
    return errg(err_invalid_arugment);
  }

  size_t const s2len = strlen(s2);
  error err = str_grow(s, s->len + s2len + 1 MEM_FILEPOS_VALUES_PASSTHRU);
  if (efailed(err))
  {
    err = ethru(err);
    return err;
  }
  strcat(s->ptr, s2);
  s->len += s2len;
  return eok();
}

error str_ncat_(struct str *const s, char const *const s2, size_t const s2len MEM_FILEPOS_PARAMS)
{
  if (!s || !s2 || s2len < 0)
  {
    return errg(err_invalid_arugment);
  }

  error err = str_grow(s, s->len + s2len + 1 MEM_FILEPOS_VALUES_PASSTHRU);
  if (efailed(err))
  {
    err = ethru(err);
    return err;
  }
  strncat(s->ptr, s2, s2len);
  s->len += strlen(s->ptr + s->len);
  return eok();
}

error str_str_(struct str const *const s, char const *const s2, int *const pos)
{
  if (!s || !s2)
  {
    return errg(err_invalid_arugment);
  }
  if (!pos)
  {
    return errg(err_null_pointer);
  }
  char const *const found = strstr(s->ptr, s2);
  if (!found)
  {
    *pos = -1;
    return eok();
  }
  *pos = found - s->ptr;
  return eok();
}

error str_replace_all_(struct str *const s, char const *const find, char const *const replacement MEM_FILEPOS_PARAMS)
{
  if (!s || !find || !replacement)
  {
    return errg(err_invalid_arugment);
  }
  int const findlen = strlen(find);
  if (findlen == 0)
  {
    return eok();
  }
  error err = eok();
  struct str tmp = {0};
  int pos = 0;
  for (;;)
  {
    char const *const found = strstr(s->ptr + pos, find);
    if (!found)
    {
      err = str_cat_(&tmp, s->ptr + pos MEM_FILEPOS_VALUES_PASSTHRU);
      if (efailed(err))
      {
        err = ethru(err);
        goto cleanup;
      }
      break;
    }
    int const foundpos = found - s->ptr;
    err = str_ncat_(&tmp, s->ptr + pos, foundpos - pos MEM_FILEPOS_VALUES_PASSTHRU);
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
    err = str_cat_(&tmp, replacement MEM_FILEPOS_VALUES_PASSTHRU);
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
    pos = foundpos + findlen;
  }

  err = str_cpy_(s, tmp.ptr MEM_FILEPOS_VALUES_PASSTHRU);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  array_free_core_((struct array *)&tmp MEM_FILEPOS_VALUES_PASSTHRU);
  return err;
}

// wstr

NODISCARD static inline error wstr_grow(struct wstr *const ws, size_t const least_size MEM_FILEPOS_PARAMS)
{
  return array_grow_((struct array *)ws, sizeof(wchar_t), least_size MEM_FILEPOS_VALUES_PASSTHRU);
}

error wstr_cpy_(struct wstr *const ws, wchar_t const *const ws2 MEM_FILEPOS_PARAMS)
{
  if (!ws || !ws2)
  {
    return errg(err_invalid_arugment);
  }

  size_t const ws2len = wcslen(ws2);
  error err = wstr_grow(ws, ws2len + 1 MEM_FILEPOS_VALUES_PASSTHRU);
  if (efailed(err))
  {
    err = ethru(err);
    return err;
  }
  wcscpy(ws->ptr, ws2);
  ws->len = ws2len;
  return eok();
}

error wstr_ncpy_(struct wstr *const ws, wchar_t const *const ws2, size_t const ws2len MEM_FILEPOS_PARAMS)
{
  if (!ws || !ws2 || ws2len < 0)
  {
    return errg(err_invalid_arugment);
  }

  error err = wstr_grow(ws, ws2len + 1 MEM_FILEPOS_VALUES_PASSTHRU);
  if (efailed(err))
  {
    err = ethru(err);
    return err;
  }
  wcsncpy(ws->ptr, ws2, ws2len);
  ws->len = wcslen(ws->ptr);
  return eok();
}

error wstr_cat_(struct wstr *const ws, wchar_t const *const ws2 MEM_FILEPOS_PARAMS)
{
  if (!ws || !ws2)
  {
    return errg(err_invalid_arugment);
  }

  size_t const ws2len = wcslen(ws2);
  error err = wstr_grow(ws, ws->len + ws2len + 1 MEM_FILEPOS_VALUES_PASSTHRU);
  if (efailed(err))
  {
    err = ethru(err);
    return err;
  }
  wcscat(ws->ptr, ws2);
  ws->len += ws2len;
  return eok();
}

error wstr_ncat_(struct wstr *const ws, wchar_t const *const ws2, size_t const ws2len MEM_FILEPOS_PARAMS)
{
  if (!ws || !ws2 || ws2len < 0)
  {
    return errg(err_invalid_arugment);
  }

  error err = wstr_grow(ws, ws->len + ws2len + 1 MEM_FILEPOS_VALUES_PASSTHRU);
  if (efailed(err))
  {
    err = ethru(err);
    return err;
  }
  wcsncat(ws->ptr, ws2, ws2len);
  ws->len += wcslen(ws->ptr + ws->len);
  return eok();
}

error wstr_str_(struct wstr const *const ws, wchar_t const *const ws2, int *const pos)
{
  if (!ws || !ws2)
  {
    return errg(err_invalid_arugment);
  }
  if (!pos)
  {
    return errg(err_null_pointer);
  }
  wchar_t const *const found = wcsstr(ws->ptr, ws2);
  if (!found)
  {
    *pos = -1;
    return eok();
  }
  *pos = found - ws->ptr;
  return eok();
}

error wstr_replace_all_(struct wstr *const ws, wchar_t const *const find, wchar_t const *const replacement MEM_FILEPOS_PARAMS)
{
  if (!ws || !find || !replacement)
  {
    return errg(err_invalid_arugment);
  }
  int const findlen = wcslen(find);
  if (findlen == 0)
  {
    return eok();
  }
  error err = eok();
  struct wstr tmp = {0};
  int pos = 0;
  for (;;)
  {
    wchar_t const *const found = wcsstr(ws->ptr + pos, find);
    if (!found)
    {
      err = wstr_cat_(&tmp, ws->ptr + pos MEM_FILEPOS_VALUES_PASSTHRU);
      if (efailed(err))
      {
        err = ethru(err);
        goto cleanup;
      }
      break;
    }
    int const foundpos = found - ws->ptr;
    err = wstr_ncat_(&tmp, ws->ptr + pos, foundpos - pos MEM_FILEPOS_VALUES_PASSTHRU);
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
    err = wstr_cat_(&tmp, replacement MEM_FILEPOS_VALUES_PASSTHRU);
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
    pos = foundpos + findlen;
  }

  err = wstr_cpy_(ws, tmp.ptr MEM_FILEPOS_VALUES_PASSTHRU);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  array_free_core_((struct array *)&tmp MEM_FILEPOS_VALUES_PASSTHRU);
  return err;
}

// hash map

struct hmap_udata
{
  struct hmap *hm;
  struct base_filepos const *filepos;
};

static void *hm_malloc(size_t const s, void *const udata)
{
#ifdef ALLOCATE_LOGGER
  struct hmap_udata const *const ud = udata;
  struct base_filepos const *const filepos = ud->filepos;
#else
  (void)udata;
#endif
  void *r = NULL;
  if (!mem_core_(&r, s MEM_FILEPOS_VALUES_PASSTHRU))
  {
    return NULL;
  }
  return r;
}

static void hm_free(void *p, void *const udata)
{
#ifdef ALLOCATE_LOGGER
  struct hmap_udata const *const ud = udata;
  struct base_filepos const *const filepos = ud->filepos;
#else
  (void)udata;
#endif
  mem_core_(&p, 0 MEM_FILEPOS_VALUES_PASSTHRU);
}

static uint64_t hm_hash_static(void const *const item, uint64_t const seed0, uint64_t const seed1, void *const udata)
{
  struct hmap_udata const *const ud = udata;
  return hashmap_sip(item, ud->hm->size, seed0, seed1);
}

static int hm_compare_static(const void *a, const void *b, void *const udata)
{
  struct hmap_udata const *const ud = udata;
  return memcmp(a, b, ud->hm->size);
}

static uint64_t hm_hash_dynamic(void const *const item, uint64_t const seed0, uint64_t const seed1, void *const udata)
{
  struct hmap_udata const *const ud = udata;
  void const *p = NULL;
  size_t len = 0;
  ud->hm->get_key(item, &p, &len);
  return hashmap_sip(p, len, seed0, seed1);
}

static int hm_compare_dynamic(void const *const a, void const *const b, void *const udata)
{
  struct hmap_udata const *const ud = udata;
  void const *p0 = NULL, *p1 = NULL;
  size_t len0 = 0, len1 = 0;
  ud->hm->get_key(a, &p0, &len0);
  ud->hm->get_key(b, &p1, &len1);
  int r = memcmp(p0, p1, len0 < len1 ? len0 : len1);
  if (len0 == len1 || r != 0)
  {
    return r;
  }
  return len0 < len1 ? -1 : 1;
}

error hmap_new_static(struct hmap *const hm, size_t const item_size, size_t const cap, size_t const key_size MEM_FILEPOS_PARAMS)
{
  if (!hm || hm->ptr)
  {
    return errg(err_invalid_arugment);
  }
  struct hmap_udata ud = {
      .hm = NULL,
#ifdef ALLOCATE_LOGGER
      .filepos = filepos,
#endif
  };
  uint64_t hash = base_splitmix64_next(GetTickCount() + GetCurrentProcessId() + GetCurrentThreadId());
  uint64_t const s0 = base_splitmix64(hash);
  hash = base_splitmix64_next(hash);
  uint64_t const s1 = base_splitmix64(hash);
  struct hashmap *const h = hashmap_new_with_allocator(
      hm_malloc,
      hm_free,
      item_size,
      cap,
      s0,
      s1,
      hm_hash_static,
      hm_compare_static,
      NULL,
      &ud);
  if (!h)
  {
    return errg(err_out_of_memory);
  }
  hm->ptr = h;
  hm->size = key_size;
  return eok();
}

error hmap_new_dynamic(struct hmap *const hm, size_t const item_size, size_t const cap, hm_get_key const get_key MEM_FILEPOS_PARAMS)
{
  if (!hm || hm->ptr || !get_key)
  {
    return errg(err_invalid_arugment);
  }
  struct hmap_udata ud = {
      .hm = NULL,
#ifdef ALLOCATE_LOGGER
      .filepos = filepos,
#endif
  };
  uint64_t hash = base_splitmix64_next(GetTickCount() + GetCurrentProcessId() + GetCurrentThreadId());
  uint64_t const s0 = base_splitmix64(hash);
  hash = base_splitmix64_next(hash);
  uint64_t const s1 = base_splitmix64(hash);
  struct hashmap *const h = hashmap_new_with_allocator(
      hm_malloc,
      hm_free,
      item_size,
      cap,
      s0,
      s1,
      hm_hash_dynamic,
      hm_compare_dynamic,
      NULL,
      &ud);
  if (!h)
  {
    return errg(err_out_of_memory);
  }
  hm->ptr = h;
  hm->get_key = get_key;
  return eok();
}

error hmap_free(struct hmap *const hm MEM_FILEPOS_PARAMS)
{
  if (!hm)
  {
    return errg(err_invalid_arugment);
  }
  if (hm->ptr)
  {
#ifdef ALLOCATE_LOGGER
    struct hmap_udata ud = {
        .filepos = filepos,
    };
    hashmap_set_udata(hm->ptr, &ud);
#endif
    hashmap_free(hm->ptr);
    hm->ptr = NULL;
  }
  hm->get_key = NULL;
  return eok();
}

error hmap_clear(struct hmap *const hm)
{
  if (!hm)
  {
    return errg(err_invalid_arugment);
  }
  if (hm->ptr)
  {
    hashmap_clear(hm->ptr, true);
  }
  return eok();
}

error hmap_count(struct hmap const *const hm, size_t *const dest)
{
  if (!hm)
  {
    return errg(err_invalid_arugment);
  }
  if (!hm->ptr)
  {
    *dest = 0;
    return eok();
  }
  *dest = hashmap_count(hm->ptr);
  return eok();
}

error hmap_get(struct hmap *const hm, void const *const key_item, void **const item)
{
  if (!hm)
  {
    return errg(err_invalid_arugment);
  }
  if (!hm->ptr)
  {
    *item = NULL;
    return eok();
  }
  struct hmap_udata ud = {
      .hm = hm,
  };
  hashmap_set_udata(hm->ptr, &ud);
  *item = hashmap_get(hm->ptr, key_item);
  return eok();
}

error hmap_set(struct hmap *const hm, void const *const item MEM_FILEPOS_PARAMS)
{
  if (!hm || !hm->ptr)
  {
    return errg(err_invalid_arugment);
  }
  struct hmap_udata ud = {
      .hm = hm,
#ifdef ALLOCATE_LOGGER
      .filepos = filepos,
#endif
  };
  hashmap_set_udata(hm->ptr, &ud);
  hashmap_set(hm->ptr, item);
  return eok();
}

error hmap_delete(struct hmap *const hm, void const *const key_item MEM_FILEPOS_PARAMS)
{
  if (!hm)
  {
    return errg(err_invalid_arugment);
  }
  if (!hm->ptr)
  {
    return eok();
  }
  struct hmap_udata ud = {
      .hm = hm,
#ifdef ALLOCATE_LOGGER
      .filepos = filepos,
#endif
  };
  hashmap_set_udata(hm->ptr, &ud);
  hashmap_delete(hm->ptr, key_item);
  return eok();
}

error hmap_scan(struct hmap *const hm, bool (*iter)(void const *const item, void *const udata), void *const udata)
{
  if (!hm || !hm->ptr)
  {
    return errg(err_invalid_arugment);
  }
  if (!hashmap_scan(hm->ptr, iter, udata))
  {
    return errg(err_abort);
  }
  return eok();
}

////////////////////

void base_init(void)
{
#ifdef ALLOCATE_LOGGER
  allocate_logger_init();
#endif
  error_init();
}

void base_exit(void)
{
  error_exit();
#ifdef ALLOCATE_LOGGER
  report_leaks();
  allocate_logger_exit();
#endif
#ifdef LEAK_DETECTOR
  report_allocated_count();
#endif
}
