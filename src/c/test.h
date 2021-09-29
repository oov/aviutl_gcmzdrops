#pragma once

#include <stdio.h>

#include "base.h"
#include "error_win32.h"
#include "error_gcmz.h"

static inline void test_init_(void)
{
  base_init();
  ereport(error_win32_init());
  ereport(error_gcmz_init());
#ifdef TEST_MY_INIT
  TEST_MY_INIT;
#endif
}

static inline void test_fini_(void)
{
#ifdef TEST_MY_FINI
  TEST_MY_FINI;
#endif
  base_exit();
  if (mem_get_allocated_count())
  {
    printf("!! MEMORY LEAKED !!\n");
    abort();
  }
}
#define TEST_INIT test_init_()
#define TEST_FINI test_fini_()
#include "3rd/acutest.h"

static int test_eis(error err, int const type, uint_least32_t const code, char const *const file, int const line, char const *const fmt, char const *const p1, char const *const p2, char const *const p3)
{
  int r = acutest_check_((err == NULL && type == 0 && code == 0) || eis(err, type, code), file, line, fmt, p1, p2, p3);
  if (!r)
  {
    if (type == 0 && code == 0)
    {
      TEST_MSG("expected NULL");
    }
    else
    {
      TEST_MSG("expected %02x:%08x", type, code);
    }
    if (err == NULL)
    {
      TEST_MSG("got      NULL");
    }
    else
    {
      TEST_MSG("got      %02x:%08x", err->type, err->code);
      struct wstr ws = {0};
      error e = error_to_string(err, &ws);
      if (esucceeded(e))
      {
        TEST_MSG("%ls\n", ws.ptr);
      }
      efree(&e);
      ereport(sfree(&ws));
    }
  }
  return r;
}
#define TEST_EIS(err, type, code) (test_eis((err), (type), (code)) __FILE__, __LINE__, "TEST_EIS(%s, %s, %s)", #err, #type, #code))
#define TEST_EISG(err, code) (test_eis((err), err_type_generic, (code), __FILE__, __LINE__, "TEST_EISG(%s, %s)", #err, #code, NULL))
#define TEST_SUCCEEDED(err) (test_eis((err), 0, 0, __FILE__, __LINE__, "TEST_SUCCEEDED(%s)", #err, NULL, NULL))

static inline bool test_eis_f(error err, int const type, uint_least32_t const code, char const *const file, int const line, char const *const fmt, char const *const p1, char const *const p2, char const *const p3)
{
  bool r = test_eis(err, type, code, file, line, fmt, p1, p2, p3);
  efree(&err);
  return r;
}
#define TEST_EIS_F(err, type, code) (test_eis_f((err), (type), (code), __FILE__, __LINE__, "TEST_EIS_F(%s, %s, %s)", #err, #type, #code))
#define TEST_EISG_F(err, code) (test_eis_f((err), err_type_generic, (code), __FILE__, __LINE__, "TEST_EISG_F(%s, %s)", #err, #code, NULL))
#define TEST_SUCCEEDED_F(err) (test_eis_f((err), 0, 0, __FILE__, __LINE__, "TEST_SUCCEEDED_F(%s)", #err, NULL, NULL))
