#include "base.c"
#include "test.h"

static size_t err_count(error e) {
  size_t n = 0;
  while (e != NULL) {
    ++n;
    e = e->next;
  }
  return n;
}

static void test_error(void) {
  {
    error e = NULL;
    TEST_CHECK(err_count(e) == 0);
  }
  {
    error e = errg(err_fail);
    TEST_EISG(e, err_fail);
    size_t n = err_count(e);
    TEST_CHECK(n == 1);
    TEST_MSG("expected %d", 1);
    TEST_MSG("got %d", n);
    efree(&e);
  }
  {
    error e = errg(err_fail);
    e = ethru(e);
    TEST_EISG(e, err_fail);
    TEST_EISG(e->next, err_pass_through);
    size_t n = err_count(e);
    TEST_CHECK(n == 2);
    TEST_MSG("expected %d", 2);
    TEST_MSG("got %d", n);
    efree(&e);
  }
}

static void test_mem(void) {
  void *p = NULL;
  TEST_SUCCEEDED_F(mem(&p, 8, 1));
  TEST_CHECK(p != NULL);

  TEST_SUCCEEDED_F(mem(&p, 16, 1));
  TEST_CHECK(p != NULL);

  TEST_SUCCEEDED_F(mem(&p, 0, 1));
  TEST_CHECK(p == NULL);

  TEST_SUCCEEDED_F(mem(&p, 8, 1));
  TEST_CHECK(p != NULL);

  TEST_SUCCEEDED_F(mem_free(&p));
  TEST_CHECK(p == NULL);

  TEST_EISG_F(mem(&p, 1, 0), err_invalid_arugment);
  TEST_EISG_F(mem(NULL, 1, 1), err_invalid_arugment);
  TEST_EISG_F(mem_free(NULL), err_invalid_arugment);
}

static void test_array(void) {
  struct {
    int64_t *ptr;
    size_t len;
    size_t cap;
  } arr = {0};
  int64_t v = 0;
  TEST_CHECK(alen(&arr) == 0);
  TEST_CHECK(acap(&arr) == 0);
  TEST_EISG_F(apop(&arr, &v), err_not_found);
  TEST_SUCCEEDED_F(apush(&arr, 100));
  TEST_CHECK(alen(&arr) == 1);
  TEST_CHECK(acap(&arr) >= 1);
  TEST_CHECK(arr.ptr[0] == 100);
  TEST_SUCCEEDED_F(apush(&arr, 200));
  TEST_CHECK(alen(&arr) == 2);
  TEST_CHECK(acap(&arr) >= 2);
  TEST_CHECK(arr.ptr[0] == 100);
  TEST_CHECK(arr.ptr[1] == 200);
  TEST_SUCCEEDED_F(apop(&arr, &v));
  TEST_CHECK(alen(&arr) == 1);
  TEST_CHECK(acap(&arr) >= 1);
  TEST_CHECK(arr.ptr[0] == 100);
  TEST_CHECK(v == 200);
  TEST_SUCCEEDED_F(achop(&arr));
  TEST_CHECK(alen(&arr) == 0);
  TEST_CHECK(acap(&arr) >= 1);
  TEST_SUCCEEDED_F(afree(&arr));
  TEST_CHECK(alen(&arr) == 0);
  TEST_CHECK(acap(&arr) == 0);
  TEST_CHECK(arr.ptr == NULL);
}

static void test_wstr_cpy_free(void) {
  static wchar_t const *const test_str = L"hello";
  struct wstr ws = {0};
  if (TEST_SUCCEEDED_F(scpy(&ws, test_str))) {
    if (TEST_CHECK(ws.ptr != NULL)) {
      TEST_CHECK(wcscmp(ws.ptr, test_str) == 0);
    }
    TEST_CHECK(ws.len == wcslen(test_str));
    TEST_CHECK(ws.cap > wcslen(test_str));
  }

  static wchar_t const *const test_str2 = L"good bye world";
  if (TEST_SUCCEEDED_F(scpy(&ws, test_str2))) {
    if (TEST_CHECK(ws.ptr != NULL)) {
      TEST_CHECK(wcscmp(ws.ptr, test_str2) == 0);
    }
    TEST_CHECK(ws.len == wcslen(test_str2));
    TEST_CHECK(ws.cap > wcslen(test_str2));
  }

  static wchar_t const *const test_str3 = L"";
  if (TEST_SUCCEEDED_F(scpy(&ws, test_str3))) {
    if (TEST_CHECK(ws.ptr != NULL)) {
      TEST_CHECK(wcscmp(ws.ptr, test_str3) == 0);
    }
    TEST_CHECK(ws.len == wcslen(test_str3));
    TEST_CHECK(ws.cap > wcslen(test_str3));
  }

  if (TEST_SUCCEEDED_F(sfree(&ws))) {
    TEST_CHECK(ws.ptr == NULL);
    TEST_CHECK(ws.len == 0);
    TEST_CHECK(ws.cap == 0);
  }

  TEST_EISG_F(scpy(&ws, NULL), err_invalid_arugment);
}

static void test_wstr_ncpy(void) {
  static wchar_t const *const test_str = L"hello";
  struct wstr ws = {0};
  if (TEST_SUCCEEDED_F(sncpy(&ws, test_str, 2))) {
    TEST_CHECK(ws.ptr != NULL);
    TEST_CHECK(ws.len == 2);
    TEST_CHECK(ws.cap > 2);
    TEST_CHECK(ws.ptr[ws.len] == L'\0');
    TEST_SUCCEEDED_F(sfree(&ws));
  }
  if (TEST_SUCCEEDED_F(sncpy(&ws, test_str, 100))) {
    TEST_CHECK(ws.ptr != NULL);
    TEST_CHECK(ws.len == 5);
    TEST_CHECK(ws.cap > 5);
    TEST_CHECK(ws.ptr[ws.len] == L'\0');
    TEST_SUCCEEDED_F(sfree(&ws));
  }
}

static void test_wstr_cat_free(void) {
  static wchar_t const *const test_str = L"hello";
  struct wstr ws = {0};
  TEST_SUCCEEDED_F(scpy(&ws, test_str));
  static wchar_t const *const test_str2 = L"world";
  if (TEST_SUCCEEDED_F(scat(&ws, test_str2))) {
    static wchar_t const *const expected_str = L"helloworld";
    if (TEST_CHECK(ws.ptr != NULL)) {
      TEST_CHECK(wcscmp(ws.ptr, expected_str) == 0);
    }
    TEST_CHECK(ws.len == wcslen(expected_str));
    TEST_CHECK(ws.cap > wcslen(expected_str));
  }

  static wchar_t const *const test_str3 = L"";
  if (TEST_SUCCEEDED_F(scat(&ws, test_str3))) {
    static wchar_t const *const expected_str = L"helloworld";
    if (TEST_CHECK(ws.ptr != NULL)) {
      TEST_CHECK(wcscmp(ws.ptr, expected_str) == 0);
    }
    TEST_CHECK(ws.len == wcslen(expected_str));
    TEST_CHECK(ws.cap > wcslen(expected_str));
  }

  if (TEST_SUCCEEDED_F(sfree(&ws))) {
    TEST_CHECK(ws.ptr == NULL);
    TEST_CHECK(ws.len == 0);
    TEST_CHECK(ws.cap == 0);
  }

  if (TEST_SUCCEEDED_F(scat(&ws, test_str))) {
    if (TEST_CHECK(ws.ptr != NULL)) {
      TEST_CHECK(wcscmp(ws.ptr, test_str) == 0);
    }
    TEST_CHECK(ws.len == wcslen(test_str));
    TEST_CHECK(ws.cap > wcslen(test_str));
  }
  TEST_SUCCEEDED_F(sfree(&ws));
  TEST_EISG_F(scat(&ws, NULL), err_invalid_arugment);
}

static void test_wstr_ncat(void) {
  static wchar_t const *const test_str = L"hello";
  static wchar_t const *const test2_str = L"world";
  struct wstr ws = {0};
  if (TEST_SUCCEEDED_F(sncat(&ws, test_str, 2))) {
    TEST_CHECK(ws.ptr != NULL);
    TEST_CHECK(ws.len == 2);
    TEST_CHECK(ws.cap > 2);
    TEST_CHECK(wcscmp(ws.ptr, L"he") == 0);
    TEST_SUCCEEDED_F(sfree(&ws));
  }
  if (TEST_SUCCEEDED_F(sncat(&ws, test_str, 100))) {
    TEST_CHECK(ws.ptr != NULL);
    TEST_CHECK(ws.len == 5);
    TEST_CHECK(ws.cap > 5);
    TEST_CHECK(ws.ptr[ws.len] == L'\0');
    TEST_CHECK(wcscmp(ws.ptr, L"hello") == 0);
    TEST_SUCCEEDED_F(sfree(&ws));
  }

  if (TEST_SUCCEEDED_F(sncat(&ws, test_str, 2))) {
    if (TEST_SUCCEEDED_F(sncat(&ws, test2_str, 2))) {
      TEST_CHECK(ws.ptr != NULL);
      TEST_CHECK(ws.len == 4);
      TEST_CHECK(ws.cap > 4);
      TEST_CHECK(wcscmp(ws.ptr, L"hewo") == 0);
    }
    TEST_SUCCEEDED_F(sfree(&ws));
  }
  if (TEST_SUCCEEDED_F(sncat(&ws, test_str, 2))) {
    if (TEST_SUCCEEDED_F(sncat(&ws, test2_str, 100))) {
      TEST_CHECK(ws.ptr != NULL);
      TEST_CHECK(ws.len == 7);
      TEST_CHECK(ws.cap > 7);
      TEST_CHECK(wcscmp(ws.ptr, L"heworld") == 0);
    }
    TEST_SUCCEEDED_F(sfree(&ws));
  }
}

static void test_wstr_grow(void) {
  struct wstr ws = {0};
  if (TEST_SUCCEEDED_F(sgrow(&ws, 1))) {
    TEST_CHECK(ws.ptr != NULL);
    TEST_CHECK(ws.len == 0);
    TEST_CHECK(ws.cap >= 1);
  }
  TEST_SUCCEEDED_F(sfree(&ws));
}

static void test_wstr_str(void) {
  struct wstr ws = wstr_unmanaged(L"hello");
  int pos = 0, expected = -1;
  if (TEST_SUCCEEDED_F(sstr(&ws, L"!", &pos))) {
    TEST_CHECK(pos == expected);
    TEST_MSG("expected %d", expected);
    TEST_MSG("got %d", pos);
  }
  expected = 1;
  if (TEST_SUCCEEDED_F(sstr(&ws, L"e", &pos))) {
    TEST_CHECK(pos == expected);
    TEST_MSG("expected %d", expected);
    TEST_MSG("got %d", pos);
  }
  expected = 4;
  if (TEST_SUCCEEDED_F(sstr(&ws, L"o", &pos))) {
    TEST_CHECK(pos == expected);
    TEST_MSG("expected %d", expected);
    TEST_MSG("got %d", pos);
  }
  TEST_EISG_F(sstr(&ws, NULL, NULL), err_invalid_arugment);
  TEST_EISG_F(sstr(&ws, L"e", NULL), err_null_pointer);
}

static void test_wstr_replace_all(void) {
  struct wstr ws = {0};
  if (TEST_SUCCEEDED_F(scpy(&ws, L"hello world"))) {
    if (TEST_SUCCEEDED_F(sreplace_all(&ws, L"o", L"xx"))) {
      static wchar_t const *const expected = L"hellxx wxxrld";
      TEST_CHECK(wcscmp(ws.ptr, expected) == 0);
      TEST_MSG("expected %ls", expected);
      TEST_MSG("got %ls", ws.ptr);
    }
  }
  if (TEST_SUCCEEDED_F(scpy(&ws, L"hello world"))) {
    if (TEST_SUCCEEDED_F(sreplace_all(&ws, L"o", L""))) {
      static wchar_t const *const expected = L"hell wrld";
      TEST_CHECK(wcscmp(ws.ptr, expected) == 0);
      TEST_MSG("expected %ls", expected);
      TEST_MSG("got %ls", ws.ptr);
    }
  }
  if (TEST_SUCCEEDED_F(scpy(&ws, L"hello world"))) {
    if (TEST_SUCCEEDED_F(sreplace_all(&ws, L"d", L"xx"))) {
      static wchar_t const *const expected = L"hello worlxx";
      TEST_CHECK(wcscmp(ws.ptr, expected) == 0);
      TEST_MSG("expected %ls", expected);
      TEST_MSG("got %ls", ws.ptr);
    }
  }
  if (TEST_SUCCEEDED_F(scpy(&ws, L"hello world"))) {
    TEST_SUCCEEDED_F(sreplace_all(&ws, L"", L"xx"));
    TEST_SUCCEEDED_F(sreplace_all(&ws, L"a", L"xx"));
    TEST_EISG_F(sreplace_all(&ws, NULL, NULL), err_invalid_arugment);
    TEST_EISG_F(sreplace_all(&ws, L"o", NULL), err_invalid_arugment);
    TEST_EISG_F(sreplace_all(&ws, NULL, L"xx"), err_invalid_arugment);
  }
  TEST_SUCCEEDED_F(sfree(&ws));
}

static void test_str_cpy_free(void) {
  static char const *const test_str = "hello";
  struct str s = {0};
  if (TEST_SUCCEEDED_F(scpy(&s, test_str))) {
    if (TEST_CHECK(s.ptr != NULL)) {
      TEST_CHECK(strcmp(s.ptr, test_str) == 0);
    }
    TEST_CHECK(s.len == strlen(test_str));
    TEST_CHECK(s.cap > strlen(test_str));
  }

  static char const *const test_str2 = "good bye world";
  if (TEST_SUCCEEDED_F(scpy(&s, test_str2))) {
    if (TEST_CHECK(s.ptr != NULL)) {
      TEST_CHECK(strcmp(s.ptr, test_str2) == 0);
    }
    TEST_CHECK(s.len == strlen(test_str2));
    TEST_CHECK(s.cap > strlen(test_str2));
  }

  static char const *const test_str3 = "";
  if (TEST_SUCCEEDED_F(scpy(&s, test_str3))) {
    if (TEST_CHECK(s.ptr != NULL)) {
      TEST_CHECK(strcmp(s.ptr, test_str3) == 0);
    }
    TEST_CHECK(s.len == strlen(test_str3));
    TEST_CHECK(s.cap > strlen(test_str3));
  }

  if (TEST_SUCCEEDED_F(sfree(&s))) {
    TEST_CHECK(s.ptr == NULL);
    TEST_CHECK(s.len == 0);
    TEST_CHECK(s.cap == 0);
  }

  TEST_EISG_F(scpy(&s, NULL), err_invalid_arugment);
}

static void test_str_ncpy(void) {
  static char const *const test_str = "hello";
  struct str s = {0};
  if (TEST_SUCCEEDED_F(sncpy(&s, test_str, 2))) {
    TEST_CHECK(s.ptr != NULL);
    TEST_CHECK(s.len == 2);
    TEST_CHECK(s.cap > 2);
    TEST_CHECK(s.ptr[s.len] == L'\0');
    TEST_SUCCEEDED_F(sfree(&s));
  }
  if (TEST_SUCCEEDED_F(sncpy(&s, test_str, 100))) {
    TEST_CHECK(s.ptr != NULL);
    TEST_CHECK(s.len == 5);
    TEST_CHECK(s.cap > 5);
    TEST_CHECK(s.ptr[s.len] == L'\0');
    TEST_SUCCEEDED_F(sfree(&s));
  }
}

static void test_str_cat_free(void) {
  static char const *const test_str = "hello";
  struct str s = {0};
  TEST_SUCCEEDED_F(scpy(&s, test_str));
  static char const *const test_str2 = "world";
  if (TEST_SUCCEEDED_F(scat(&s, test_str2))) {
    static char const *const expected_str = "helloworld";
    if (TEST_CHECK(s.ptr != NULL)) {
      TEST_CHECK(strcmp(s.ptr, expected_str) == 0);
    }
    TEST_CHECK(s.len == strlen(expected_str));
    TEST_CHECK(s.cap > strlen(expected_str));
  }

  static char const *const test_str3 = "";
  if (TEST_SUCCEEDED_F(scat(&s, test_str3))) {
    static char const *const expected_str = "helloworld";
    if (TEST_CHECK(s.ptr != NULL)) {
      TEST_CHECK(strcmp(s.ptr, expected_str) == 0);
    }
    TEST_CHECK(s.len == strlen(expected_str));
    TEST_CHECK(s.cap > strlen(expected_str));
  }

  if (TEST_SUCCEEDED_F(sfree(&s))) {
    TEST_CHECK(s.ptr == NULL);
    TEST_CHECK(s.len == 0);
    TEST_CHECK(s.cap == 0);
  }

  if (TEST_SUCCEEDED_F(scat(&s, test_str))) {
    if (TEST_CHECK(s.ptr != NULL)) {
      TEST_CHECK(strcmp(s.ptr, test_str) == 0);
    }
    TEST_CHECK(s.len == strlen(test_str));
    TEST_CHECK(s.cap > strlen(test_str));
  }
  TEST_SUCCEEDED_F(sfree(&s));
  TEST_EISG_F(scat(&s, NULL), err_invalid_arugment);
}

static void test_str_ncat(void) {
  static char const *const test_str = "hello";
  static char const *const test2_str = "world";
  struct str s = {0};
  if (TEST_SUCCEEDED_F(sncat(&s, test_str, 2))) {
    TEST_CHECK(s.ptr != NULL);
    TEST_CHECK(s.len == 2);
    TEST_CHECK(s.cap > 2);
    TEST_CHECK(strcmp(s.ptr, "he") == 0);
    TEST_SUCCEEDED_F(sfree(&s));
  }
  if (TEST_SUCCEEDED_F(sncat(&s, test_str, 100))) {
    TEST_CHECK(s.ptr != NULL);
    TEST_CHECK(s.len == 5);
    TEST_CHECK(s.cap > 5);
    TEST_CHECK(s.ptr[s.len] == L'\0');
    TEST_CHECK(strcmp(s.ptr, "hello") == 0);
    TEST_SUCCEEDED_F(sfree(&s));
  }

  if (TEST_SUCCEEDED_F(sncat(&s, test_str, 2))) {
    if (TEST_SUCCEEDED_F(sncat(&s, test2_str, 2))) {
      TEST_CHECK(s.ptr != NULL);
      TEST_CHECK(s.len == 4);
      TEST_CHECK(s.cap > 4);
      TEST_CHECK(strcmp(s.ptr, "hewo") == 0);
    }
    TEST_SUCCEEDED_F(sfree(&s));
  }
  if (TEST_SUCCEEDED_F(sncat(&s, test_str, 2))) {
    if (TEST_SUCCEEDED_F(sncat(&s, test2_str, 100))) {
      TEST_CHECK(s.ptr != NULL);
      TEST_CHECK(s.len == 7);
      TEST_CHECK(s.cap > 7);
      TEST_CHECK(strcmp(s.ptr, "heworld") == 0);
    }
    TEST_SUCCEEDED_F(sfree(&s));
  }
}

static void test_str_grow(void) {
  struct str s = {0};
  if (TEST_SUCCEEDED_F(sgrow(&s, 1))) {
    TEST_CHECK(s.ptr != NULL);
    TEST_CHECK(s.len == 0);
    TEST_CHECK(s.cap >= 1);
  }
  TEST_SUCCEEDED_F(sfree(&s));
}

static void test_str_str(void) {
  struct str s = str_unmanaged("hello");
  int pos = 0, expected = -1;
  if (TEST_SUCCEEDED_F(sstr(&s, "!", &pos))) {
    TEST_CHECK(pos == expected);
    TEST_MSG("expected %d", expected);
    TEST_MSG("got %d", pos);
  }
  expected = 1;
  if (TEST_SUCCEEDED_F(sstr(&s, "e", &pos))) {
    TEST_CHECK(pos == expected);
    TEST_MSG("expected %d", expected);
    TEST_MSG("got %d", pos);
  }
  expected = 4;
  if (TEST_SUCCEEDED_F(sstr(&s, "o", &pos))) {
    TEST_CHECK(pos == expected);
    TEST_MSG("expected %d", expected);
    TEST_MSG("got %d", pos);
  }
  TEST_EISG_F(sstr(&s, NULL, NULL), err_invalid_arugment);
  TEST_EISG_F(sstr(&s, "e", NULL), err_null_pointer);
}

static void test_str_replace_all(void) {
  struct str s = {0};
  if (TEST_SUCCEEDED_F(scpy(&s, "hello world"))) {
    if (TEST_SUCCEEDED_F(sreplace_all(&s, "o", "xx"))) {
      static char const *const expected = "hellxx wxxrld";
      TEST_CHECK(strcmp(s.ptr, expected) == 0);
      TEST_MSG("expected %hs", expected);
      TEST_MSG("got %hs", s.ptr);
    }
  }
  if (TEST_SUCCEEDED_F(scpy(&s, "hello world"))) {
    if (TEST_SUCCEEDED_F(sreplace_all(&s, "o", ""))) {
      static char const *const expected = "hell wrld";
      TEST_CHECK(strcmp(s.ptr, expected) == 0);
      TEST_MSG("expected %hs", expected);
      TEST_MSG("got %hs", s.ptr);
    }
  }
  if (TEST_SUCCEEDED_F(scpy(&s, "hello world"))) {
    if (TEST_SUCCEEDED_F(sreplace_all(&s, "d", "xx"))) {
      static char const *const expected = "hello worlxx";
      TEST_CHECK(strcmp(s.ptr, expected) == 0);
      TEST_MSG("expected %hs", expected);
      TEST_MSG("got %hs", s.ptr);
    }
  }
  if (TEST_SUCCEEDED_F(scpy(&s, "hello world"))) {
    TEST_SUCCEEDED_F(sreplace_all(&s, "", "xx"));
    TEST_SUCCEEDED_F(sreplace_all(&s, "a", "xx"));
    TEST_EISG_F(sreplace_all(&s, NULL, NULL), err_invalid_arugment);
    TEST_EISG_F(sreplace_all(&s, "o", NULL), err_invalid_arugment);
    TEST_EISG_F(sreplace_all(&s, NULL, "xx"), err_invalid_arugment);
  }
  TEST_SUCCEEDED_F(sfree(&s));
}

struct test_item_dynamic {
  struct wstr key;
  int v;
};

void test_hmap_dynamic_get_key(void const *const item, void const **const key, size_t *const key_bytes) {
  struct test_item_dynamic const *const it = item;
  *key = it->key.ptr;
  *key_bytes = it->key.len * sizeof(wchar_t);
}

static void test_hmap_dynamic(void) {
  struct hmap tmp = {0};
  if (!TEST_SUCCEEDED_F(hmfree(&tmp))) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmnewd(&tmp, sizeof(struct test_item_dynamic), 0, test_hmap_dynamic_get_key))) {
    goto cleanup;
  }
  size_t count = 0;
  if (!TEST_SUCCEEDED_F(hmcount(&tmp, &count))) {
    goto cleanup;
  }
  if (!TEST_CHECK(count == 0)) {
    goto cleanup;
  }
  struct test_item_dynamic *got = NULL;
  if (!TEST_SUCCEEDED_F(hmget(&tmp, &(struct test_item_dynamic){.key = wstr_unmanaged(L"test1")}, &got))) {
    goto cleanup;
  }
  if (!TEST_CHECK(got == NULL)) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmset(&tmp, &((struct test_item_dynamic){.key = wstr_unmanaged(L"test1"), .v = 100})))) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmset(&tmp, &((struct test_item_dynamic){.key = wstr_unmanaged(L"test2"), .v = 200})))) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmcount(&tmp, &count))) {
    goto cleanup;
  }
  if (!TEST_CHECK(count == 2)) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmget(&tmp, &(struct test_item_dynamic){.key = wstr_unmanaged(L"test1")}, &got))) {
    goto cleanup;
  }
  if (!TEST_CHECK(got != NULL)) {
    goto cleanup;
  }
  if (!TEST_CHECK(got->v == 100)) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmdelete(&tmp, &(struct test_item_dynamic){.key = wstr_unmanaged(L"test1")}))) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmcount(&tmp, &count))) {
    goto cleanup;
  }
  if (!TEST_CHECK(count == 1)) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmget(&tmp, &(struct test_item_dynamic){.key = wstr_unmanaged(L"test1")}, &got))) {
    goto cleanup;
  }
  if (!TEST_CHECK(got == NULL)) {
    goto cleanup;
  }
cleanup:
  TEST_SUCCEEDED_F(hmfree(&tmp));
}

static void test_hmap_static(void) {
  struct test_item_static {
    int64_t key;
    int v;
  };
  struct hmap tmp = {0};
  if (!TEST_SUCCEEDED_F(hmfree(&tmp))) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmnews(&tmp, sizeof(struct test_item_static), 0, sizeof(int64_t)))) {
    goto cleanup;
  }
  size_t count = 0;
  if (!TEST_SUCCEEDED_F(hmcount(&tmp, &count))) {
    goto cleanup;
  }
  if (!TEST_CHECK(count == 0)) {
    goto cleanup;
  }
  struct test_item_static *got = NULL;
  if (!TEST_SUCCEEDED_F(hmget(&tmp, &(struct test_item_static){.key = 123}, &got))) {
    goto cleanup;
  }
  if (!TEST_CHECK(got == NULL)) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmset(&tmp, &((struct test_item_static){.key = 123, .v = 100})))) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmset(&tmp, &((struct test_item_static){.key = 456, .v = 200})))) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmcount(&tmp, &count))) {
    goto cleanup;
  }
  if (!TEST_CHECK(count == 2)) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmget(&tmp, &(struct test_item_static){.key = 123}, &got))) {
    goto cleanup;
  }
  if (!TEST_CHECK(got != NULL)) {
    goto cleanup;
  }
  if (!TEST_CHECK(got->v == 100)) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmdelete(&tmp, &(struct test_item_static){.key = 123}))) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmcount(&tmp, &count))) {
    goto cleanup;
  }
  if (!TEST_CHECK(count == 1)) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(hmget(&tmp, &(struct test_item_static){.key = 123}, &got))) {
    goto cleanup;
  }
  if (!TEST_CHECK(got == NULL)) {
    goto cleanup;
  }
cleanup:
  TEST_SUCCEEDED_F(hmfree(&tmp));
}

TEST_LIST = {
    {"test_error", test_error},
    {"test_mem", test_mem},
    {"test_array", test_array},
    {"test_wstr_cpy_free", test_wstr_cpy_free},
    {"test_wstr_ncpy", test_wstr_ncpy},
    {"test_wstr_cat_free", test_wstr_cat_free},
    {"test_wstr_ncat", test_wstr_ncat},
    {"test_wstr_grow", test_wstr_grow},
    {"test_wstr_str", test_wstr_str},
    {"test_wstr_replace_all", test_wstr_replace_all},
    {"test_str_cpy_free", test_str_cpy_free},
    {"test_str_ncpy", test_str_ncpy},
    {"test_str_cat_free", test_str_cat_free},
    {"test_str_ncat", test_str_ncat},
    {"test_str_grow", test_str_grow},
    {"test_str_str", test_str_str},
    {"test_str_replace_all", test_str_replace_all},
    {"test_hmap_dynamic", test_hmap_dynamic},
    {"test_hmap_static", test_hmap_static},
    {NULL, NULL},
};
