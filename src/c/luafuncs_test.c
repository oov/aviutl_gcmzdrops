#include "luafuncs.c"
#include "luafuncs_convertencoding.c"

#include <combaseapi.h>
#include <lauxlib.h>
#include <lualib.h>

NODISCARD static error get_test_dir(struct wstr *dest) {
  struct wstr tmp = {0};
  error err = get_temp_dir(&tmp);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  err = include_trailing_path_delimiter(&tmp);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  err = scat(&tmp, L"gcmztest");
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  err = scpy(dest, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  ereport(sfree(&tmp));
  return eok();
}

static void test_init(void) {
  struct wstr tmp = {0};
  error err = get_test_dir(&tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (!CreateDirectoryW(tmp.ptr, NULL)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  ereportmsg(err, &native_unmanaged(NSTR("test_init failed.")));
}

static void test_exit(void) {
  struct wstr tmp = {0};
  error err = get_test_dir(&tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (!RemoveDirectoryW(tmp.ptr)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  ereportmsg(err, &native_unmanaged(NSTR("test_exit failed.")));
}

#define TEST_MY_INIT test_init()
#define TEST_MY_FINI test_exit()
#include "ovtest.h"

static void test_createfile(void) {
  TEST_EISG_F(luafn_createfile_core(NULL, NULL, NULL), err_invalid_arugment);
  TEST_EISG_F(luafn_createfile_core(&wstr_unmanaged(L"test_createfile"), NULL, NULL), err_invalid_arugment);
  TEST_EISG_F(luafn_createfile_core(&wstr_unmanaged(L"test_createfile"), &wstr_unmanaged(L".txt"), NULL),
              err_null_pointer);

  struct wstr testdir = {0};
  struct wstr tmp = {0};
  struct wstr got1 = {0};
  struct wstr got2 = {0};

  if (!TEST_SUCCEEDED_F(get_test_dir(&testdir))) {
    goto cleanup;
  }

  if (!TEST_SUCCEEDED_F(exclude_trailing_path_delimiter(&testdir))) {
    goto cleanup;
  }

  if (!TEST_SUCCEEDED_F(luafn_createfile_core(&wstr_unmanaged(L"test_createfile"), &wstr_unmanaged(L".txt"), &got1))) {
    goto cleanup;
  }
  TEST_CHECK(got1.len > 0);
  bool found = false;
  TEST_SUCCEEDED_F(file_exists(&got1, &found));
  TEST_CHECK(found);

  if (!TEST_SUCCEEDED_F(scpy(&tmp, got1.ptr))) {
    goto cleanup;
  }

  size_t fnpos = 0;
  if (!TEST_SUCCEEDED_F(extract_file_name(&tmp, &fnpos))) {
    goto cleanup;
  }
  tmp.ptr[fnpos] = L'\0';
  tmp.len = fnpos;

  if (!TEST_SUCCEEDED_F(exclude_trailing_path_delimiter(&tmp))) {
    goto cleanup;
  }

  bool same = false;
  if (!TEST_SUCCEEDED_F(is_same_dir(&testdir, &tmp, &same))) {
    goto cleanup;
  }
  if (!TEST_CHECK(same)) {
    goto cleanup;
  }

  if (!TEST_SUCCEEDED_F(luafn_createfile_core(&wstr_unmanaged(L"test_createfile"), &wstr_unmanaged(L".txt"), &got2))) {
    goto cleanup;
  }
  TEST_SUCCEEDED_F(file_exists(&got2, &found));
  TEST_CHECK(found);

  TEST_CHECK(wcscmp(got1.ptr, got2.ptr) != 0);
  TEST_MSG("expected %ls != %ls but failed", got1.ptr, got2.ptr);

  files_cleanup(false);
  TEST_SUCCEEDED_F(file_exists(&got1, &found));
  TEST_CHECK(found);
  TEST_SUCCEEDED_F(file_exists(&got2, &found));
  TEST_CHECK(found);

  files_cleanup(true);
  TEST_SUCCEEDED_F(file_exists(&got1, &found));
  TEST_CHECK(found);
  TEST_SUCCEEDED_F(file_exists(&got2, &found));
  TEST_CHECK(found);

cleanup:
  ereport(sfree(&testdir));
  ereport(sfree(&tmp));
  if (got1.ptr) {
    ereport(delete_file(&got1));
  }
  ereport(sfree(&got1));
  if (got2.ptr) {
    ereport(delete_file(&got2));
  }
  ereport(sfree(&got2));
}

static void test_createfile_failed(void) {
  struct wstr got1 = {0};

  if (!TEST_SUCCEEDED_F(
          luafn_createfile_core(&wstr_unmanaged(L"test_createfile_failed"), &wstr_unmanaged(L".txt"), &got1))) {
    goto cleanup;
  }
  bool found = false;
  TEST_SUCCEEDED_F(file_exists(&got1, &found));
  TEST_CHECK(found);
  files_cleanup(true);

  TEST_SUCCEEDED_F(file_exists(&got1, &found));
  TEST_CHECK(!found);

cleanup:
  if (got1.ptr) {
    ereport(delete_file(&got1));
  }
  ereport(sfree(&got1));
}

static void test_createtempfile(void) {
  TEST_EISG_F(luafn_createtempfile_core(NULL, NULL, NULL), err_invalid_arugment);
  TEST_EISG_F(luafn_createtempfile_core(&wstr_unmanaged(L"test_createtempfile"), NULL, NULL), err_invalid_arugment);
  TEST_EISG_F(luafn_createtempfile_core(&wstr_unmanaged(L"test_createtempfile"), &wstr_unmanaged(L".txt"), NULL),
              err_null_pointer);

  struct wstr tempdir = {0};
  struct wstr tmp = {0};
  struct wstr got1 = {0};
  struct wstr got2 = {0};

  if (!TEST_SUCCEEDED_F(get_temp_dir(&tempdir))) {
    goto cleanup;
  }

  if (!TEST_SUCCEEDED_F(exclude_trailing_path_delimiter(&tempdir))) {
    goto cleanup;
  }

  if (!TEST_SUCCEEDED_F(
          luafn_createtempfile_core(&wstr_unmanaged(L"test_createtempfile"), &wstr_unmanaged(L".txt"), &got1))) {
    goto cleanup;
  }
  TEST_CHECK(got1.len > 0);
  bool found = false;
  TEST_SUCCEEDED_F(file_exists(&got1, &found));
  TEST_CHECK(found);

  if (!TEST_SUCCEEDED_F(scpy(&tmp, got1.ptr))) {
    goto cleanup;
  }

  size_t fnpos = 0;
  if (!TEST_SUCCEEDED_F(extract_file_name(&tmp, &fnpos))) {
    goto cleanup;
  }
  tmp.ptr[fnpos] = L'\0';
  tmp.len = fnpos;

  if (!TEST_SUCCEEDED_F(exclude_trailing_path_delimiter(&tmp))) {
    goto cleanup;
  }

  bool same = false;
  if (!TEST_SUCCEEDED_F(is_same_dir(&tempdir, &tmp, &same))) {
    goto cleanup;
  }
  if (!TEST_CHECK(same)) {
    goto cleanup;
  }

  if (!TEST_SUCCEEDED_F(
          luafn_createtempfile_core(&wstr_unmanaged(L"test_createtempfile"), &wstr_unmanaged(L".txt"), &got2))) {
    goto cleanup;
  }
  TEST_SUCCEEDED_F(file_exists(&got2, &found));
  TEST_CHECK(found);

  TEST_CHECK(wcscmp(got1.ptr, got2.ptr) != 0);
  TEST_MSG("expected %ls != %ls but failed", got1.ptr, got2.ptr);

  files_cleanup(false);
  TEST_SUCCEEDED_F(file_exists(&got1, &found));
  TEST_CHECK(!found);
  TEST_SUCCEEDED_F(file_exists(&got2, &found));
  TEST_CHECK(!found);

cleanup:
  ereport(sfree(&tempdir));
  ereport(sfree(&tmp));
  if (got1.ptr) {
    ereport(delete_file(&got1));
  }
  ereport(sfree(&got1));
  if (got2.ptr) {
    ereport(delete_file(&got2));
  }
  ereport(sfree(&got2));
}

static void test_createtempfile_failed(void) {
  struct wstr got1 = {0};

  if (!TEST_SUCCEEDED_F(
          luafn_createtempfile_core(&wstr_unmanaged(L"test_createtempfile_failed"), &wstr_unmanaged(L".txt"), &got1))) {
    goto cleanup;
  }
  bool found = false;
  TEST_SUCCEEDED_F(file_exists(&got1, &found));
  TEST_CHECK(found);

  files_cleanup(true);
  TEST_SUCCEEDED_F(file_exists(&got1, &found));
  TEST_CHECK(!found);

cleanup:
  if (got1.ptr) {
    ereport(delete_file(&got1));
  }
  ereport(sfree(&got1));
}

static void test_calcfilehash(void) {
  TEST_EISG_F(luafn_calcfilehash_core(NULL, NULL), err_invalid_arugment);
  TEST_EISG_F(luafn_calcfilehash_core(&wstr_unmanaged(L"test_calcfilehash.txt"), NULL), err_null_pointer);

  struct wstr tempfile = {0};
  if (!TEST_SUCCEEDED_F(get_temp_dir(&tempfile))) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(include_trailing_path_delimiter(&tempfile))) {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(scat(&tempfile, L"test_calcfilehash.txt"))) {
    goto cleanup;
  }
  HANDLE h = CreateFileW(tempfile.ptr,
                         GENERIC_READ | GENERIC_WRITE,
                         0,
                         NULL,
                         CREATE_NEW,
                         FILE_ATTRIBUTE_TEMPORARY | FILE_ATTRIBUTE_NOT_CONTENT_INDEXED,
                         NULL);
  if (!TEST_CHECK(h != INVALID_HANDLE_VALUE)) {
    goto cleanup;
  }
  char const *write_data = "123456789";
  size_t write_data_len = strlen(write_data);
  DWORD written = 0;
  if (!TEST_CHECK(WriteFile(h, write_data, write_data_len, &written, NULL) == TRUE) ||
      !TEST_CHECK(written == write_data_len)) {
    CloseHandle(h);
    goto cleanup;
  }
  CloseHandle(h);

  uint64_t hash = 0;
  if (TEST_SUCCEEDED_F(luafn_calcfilehash_core(&tempfile, &hash))) {
    uint64_t expected = UINT64_C(0xcaa717168609f281);
    TEST_CHECK(hash == expected);
    TEST_MSG("expected %llx", expected);
    TEST_MSG("got %llx", hash);
  }

cleanup:
  if (tempfile.ptr) {
    ereport(delete_file(&tempfile));
  }
  ereport(sfree(&tempfile));
}

static void test_hashtostring(void) {
  TEST_EISG_F(luafn_hashtostring_core(0, NULL), err_null_pointer);

  static const struct test_data {
    uint64_t input;
    wchar_t const *output;
    int code;
  } test_data[] = {
      {
          .input = UINT64_C(16845390139448941002),
          .output = L"egfsndh5kowrl",
          .code = 0,
      },
  };

  struct wstr tmp = {0};
  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d \"%ls\"", i, td->output);
    if (TEST_EISG_F(luafn_hashtostring_core(td->input, &tmp), td->code) && td->code) {
      TEST_CHECK(tmp.len > 0);
      TEST_CHECK(wcscmp(tmp.ptr, td->output) == 0);
      TEST_MSG("expected %ls", td->output);
      TEST_MSG("got %ls", tmp.ptr);
    }
  }
  ereport(sfree(&tmp));
}

static void test_encode_exo_text(void) {
  static wchar_t const input[] = L"hello world";
  static char const expected[] = "680065006c006c006f00200077006f0072006c0064000000";
  struct str got = {0};
  error err = encode_exo_text(&wstr_unmanaged_const(input), &got);
  if (!TEST_SUCCEEDED_F(err)) {
    goto cleanup;
  }
  TEST_CHECK(strncmp(expected, got.ptr, strlen(expected)) == 0);
  TEST_CHECK(got.len == 4096);
  TEST_MSG("expected %d", 4096);
  TEST_MSG("got %d", got.len);

cleanup:
  ereport(sfree(&got));
}

static void test_decode_exo_text(void) {
  struct str input = {0};
  TEST_SUCCEEDED_F(scpy(&input, "680065006c006c006f00200077006f0072006c00640000"));
  while (input.len < 4096) {
    error err = scat(&input, "0000");
    efree(&err);
  }
  static wchar_t const expected[] = L"hello world";
  struct wstr got = {0};
  error err = decode_exo_text(&input, &got);
  if (!TEST_SUCCEEDED_F(err)) {
    goto cleanup;
  }
  TEST_CHECK(wcscmp(expected, got.ptr) == 0);

cleanup:
  ereport(sfree(&got));
  ereport(sfree(&input));
}

static void test_encode_lua_string(void) {
  static char const input[] = "hello\r\nworld";
  static char const expected[] = "\"hello\\r\\nworld\"";
  struct str got = {0};
  error err = encode_lua_string(&str_unmanaged_const(input), &got);
  if (!TEST_SUCCEEDED_F(err)) {
    goto cleanup;
  }
  TEST_CHECK(got.len > 0);
  TEST_CHECK(strcmp(expected, got.ptr) == 0);
  TEST_MSG("expected %hs", expected);
  TEST_MSG("got %hs", got.ptr);

cleanup:
  ereport(sfree(&got));
}

static void test_luafn_isutf8(void) {
  TEST_CHECK(luafn_isutf8_core("", 0) == 1);
  TEST_CHECK(luafn_isutf8_core("hello", 5) == 1);
  TEST_CHECK(luafn_isutf8_core("\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e", 9) == 1);
  TEST_CHECK(luafn_isutf8_core("\x89", 1) == 0);
  TEST_CHECK(luafn_isutf8_core("\xef\xbb\xbf", 3) == 1);
  TEST_CHECK(luafn_isutf8_core("\xef\xbb\xbfhello", 8) == 1);
  TEST_CHECK(luafn_isutf8_core("\xef\xbb\xbf\xe6\x97\xa5\xe6\x9c\xac\xe8\xaa\x9e", 12) == 1);
  TEST_CHECK(luafn_isutf8_core("\xef\xbb\xbf\x89", 4) == 0);
}

static void test_convertencoding(void) {
  static const struct test_data {
    void *input;
    UINT input_cp;
    UINT output_cp;
    void *expected;
  } test_data[] = {
      {
          .input = "\x93\xFA\x96\x7B\x8C\xEA",
          .input_cp = 932,
          .output_cp = 932,
          .expected = "\x93\xFA\x96\x7B\x8C\xEA",
      },
      {
          .input = "\x93\xFA\x96\x7B\x8C\xEA",
          .input_cp = 932,
          .output_cp = 20932,
          .expected = "\xC6\xFC\xCB\xDC\xB8\xEC",
      },
      {
          .input = "\x93\xFA\x96\x7B\x8C\xEA",
          .input_cp = 932,
          .output_cp = 50222,
          .expected = "\x1B\x24\x42\x46\x7C\x4B\x5C\x38\x6C\x1B\x28\x42",
      },
      {
          .input = "\x93\xFA\x96\x7B\x8C\xEA",
          .input_cp = 932,
          .output_cp = 65001,
          .expected = "\xE6\x97\xA5\xE6\x9C\xAC\xE8\xAA\x9E",
      },
      {
          .input = "\x93\xFA\x96\x7B\x8C\xEA",
          .input_cp = 932,
          .output_cp = 65001,
          .expected = "\xE6\x97\xA5\xE6\x9C\xAC\xE8\xAA\x9E",
      },
      {
          .input = "\x93\xFA\x96\x7B\x8C\xEA",
          .input_cp = 932,
          .output_cp = 1200,
          .expected = "\xE5\x65\x2C\x67\x9E\x8A\0",
      },
      {
          .input = "\x93\xFA\x96\x7B\x8C\xEA",
          .input_cp = 932,
          .output_cp = 1201,
          .expected = "\x65\xE5\x67\x2C\x8A\x9E\0",
      },
      {
          .input = "\xE5\x65\x2C\x67\x9E\x8A\0",
          .input_cp = 1200,
          .output_cp = 932,
          .expected = "\x93\xFA\x96\x7B\x8C\xEA",
      },
      {
          .input = "\xE5\x65\x2C\x67\x9E\x8A\0",
          .input_cp = 1200,
          .output_cp = 20932,
          .expected = "\xC6\xFC\xCB\xDC\xB8\xEC",
      },
      {
          .input = "\xE5\x65\x2C\x67\x9E\x8A\0",
          .input_cp = 1200,
          .output_cp = 50222,
          .expected = "\x1B\x24\x42\x46\x7C\x4B\x5C\x38\x6C\x1B\x28\x42",
      },
      {
          .input = "\xE5\x65\x2C\x67\x9E\x8A\0",
          .input_cp = 1200,
          .output_cp = 65001,
          .expected = "\xE6\x97\xA5\xE6\x9C\xAC\xE8\xAA\x9E",
      },
      {
          .input = "\xE5\x65\x2C\x67\x9E\x8A\0",
          .input_cp = 1200,
          .output_cp = 65001,
          .expected = "\xE6\x97\xA5\xE6\x9C\xAC\xE8\xAA\x9E",
      },
      {
          .input = "\xE5\x65\x2C\x67\x9E\x8A\0",
          .input_cp = 1200,
          .output_cp = 1200,
          .expected = "\xE5\x65\x2C\x67\x9E\x8A\0",
      },
      {
          .input = "\xE5\x65\x2C\x67\x9E\x8A\0",
          .input_cp = 1200,
          .output_cp = 1201,
          .expected = "\x65\xE5\x67\x2C\x8A\x9E\0",
      },
      {
          .input = "\x65\xE5\x67\x2C\x8A\x9E\0",
          .input_cp = 1201,
          .output_cp = 932,
          .expected = "\x93\xFA\x96\x7B\x8C\xEA",
      },
      {
          .input = "\x65\xE5\x67\x2C\x8A\x9E\0",
          .input_cp = 1201,
          .output_cp = 20932,
          .expected = "\xC6\xFC\xCB\xDC\xB8\xEC",
      },
      {
          .input = "\x65\xE5\x67\x2C\x8A\x9E\0",
          .input_cp = 1201,
          .output_cp = 50222,
          .expected = "\x1B\x24\x42\x46\x7C\x4B\x5C\x38\x6C\x1B\x28\x42",
      },
      {
          .input = "\x65\xE5\x67\x2C\x8A\x9E\0",
          .input_cp = 1201,
          .output_cp = 65001,
          .expected = "\xE6\x97\xA5\xE6\x9C\xAC\xE8\xAA\x9E",
      },
      {
          .input = "\x65\xE5\x67\x2C\x8A\x9E\0",
          .input_cp = 1201,
          .output_cp = 65001,
          .expected = "\xE6\x97\xA5\xE6\x9C\xAC\xE8\xAA\x9E",
      },
      {
          .input = "\x65\xE5\x67\x2C\x8A\x9E\0",
          .input_cp = 1201,
          .output_cp = 1200,
          .expected = "\xE5\x65\x2C\x67\x9E\x8A\0",
      },
      {
          .input = "\x65\xE5\x67\x2C\x8A\x9E\0",
          .input_cp = 1201,
          .output_cp = 1201,
          .expected = "\x65\xE5\x67\x2C\x8A\x9E\0",
      },
  };
  struct str tmp = {0};
  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d %d -> %d", i, td->input_cp, td->output_cp);
    size_t input_len = (td->input_cp == 1200 || td->input_cp == 1201) ? wcslen(td->input) * 2 : strlen(td->input);
    error err = luafn_convertencoding_core(td->input, input_len, td->input_cp, td->output_cp, &tmp);
    if (TEST_SUCCEEDED_F(err)) {
      TEST_CHECK(tmp.len > 0);
      size_t expected_len = td->output_cp >= 1200 ? wcslen(td->expected) : strlen(td->expected);
      if (td->output_cp == 1200 || td->output_cp == 1201) {
        TEST_CHECK(wcsncmp((void const *)tmp.ptr, (wchar_t const *)td->expected, expected_len) == 0);
      } else {
        TEST_CHECK(strncmp(tmp.ptr, td->expected, expected_len) == 0);
      }
    }
  }
  ereport(sfree(&tmp));
}

static void test_get_preferred_language(void) {
  lua_State *L = luaL_newstate();
  luaL_openlibs(L);
  lua_register(L, "get_preferred_language", luafn_get_preferred_language);
  if (!TEST_CHECK(!luaL_dostring(L, "ret = get_preferred_language()"))) {
    TEST_MSG("%s", lua_tostring(L, -1));
    return;
  }
  lua_getglobal(L, "ret");
  TEST_CHECK(lua_type(L, -1) == LUA_TTABLE);
  TEST_CHECK(lua_objlen(L, -1) > 0);
  lua_close(L);
}

static void test_choose_language(void) {
  static const struct test_data {
    char const *script;
    int ret;
  } test_data[] = {
      {
          .script = "ret = choose_language({'ja_JP', 'en_US'}, {'zh_CN', 'en_US'})",
          .ret = 2,
      },
      {
          .script = "ret = choose_language({'ja_JP', 'en_US'}, {'zh_CN', 'zh_TW'})",
          .ret = 1,
      },
  };
  lua_State *L = luaL_newstate();
  luaL_openlibs(L);
  lua_register(L, "choose_language", luafn_choose_language);
  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d", i);
    int r = luaL_dostring(L, td->script);
    if (!TEST_CHECK((td->ret == 0 && r != 0) || (td->ret != 0 && r == 0))) {
      continue;
    }
    lua_getglobal(L, "ret");
    TEST_CHECK(lua_type(L, -1) == LUA_TNUMBER);
    TEST_CHECK(lua_tointeger(L, -1) == td->ret);
    TEST_MSG("want %d, got %d", td->ret, lua_tointeger(L, -1));
    lua_pop(L, 1);
  }
  lua_close(L);
}

TEST_LIST = {
    {"test_createfile", test_createfile},
    {"test_createfile_failed", test_createfile_failed},
    {"test_createtempfile", test_createtempfile},
    {"test_createtempfile_failed", test_createtempfile_failed},
    {"test_calcfilehash", test_calcfilehash},
    {"test_hashtostring", test_hashtostring},
    {"test_encode_exo_text", test_encode_exo_text},
    {"test_decode_exo_text", test_decode_exo_text},
    {"test_encode_lua_string", test_encode_lua_string},
    {"test_convertencoding", test_convertencoding},
    {"test_luafn_isutf8", test_luafn_isutf8},
    {"test_get_preferred_language", test_get_preferred_language},
    {"test_choose_language", test_choose_language},
    {NULL, NULL},
};

// mocks

error gcmz_get_script_dir(struct wstr *const dest) { return get_test_dir(dest); }

error gcmz_get_project_dir(struct wstr *const dest) { return get_test_dir(dest); }

error gcmz_get_save_dir(struct wstr *const dest) { return get_test_dir(dest); }

error gcmz_is_need_copy(struct wstr const *const path, bool *const need_copy) {
  (void)path;
  *need_copy = true;
  return eok();
}

error gcmz_exists_in_save_dir(struct wstr const *const path, bool *const exists) {
  (void)path;
  (void)exists;
  return errg(err_not_implemented_yet);
}

error gcmz_advance_frame(const int n) {
  (void)n;
  return errg(err_not_implemented_yet);
}

error gcmz_drop(HWND const window, POINT const pt, struct wstr const *const *const filepath) {
  (void)window;
  (void)pt;
  (void)filepath;
  return errg(err_not_implemented_yet);
}

error gcmz_analyse_exedit_window(struct gcmz_analysed_info *const ai) {
  (void)ai;
  return errg(err_not_implemented_yet);
}

error gcmz_prompt(struct wstr const *const caption, struct wstr *const value, bool *const result) {
  (void)caption;
  (void)value;
  (void)result;
  return errg(err_not_implemented_yet);
}

error gcmz_confirm(struct wstr const *const caption, bool *const result) {
  (void)caption;
  (void)result;
  return errg(err_not_implemented_yet);
}

error aviutl_get_patch(int *const patch) {
  *patch = aviutl_patched_default;
  return eok();
}

error aviutl_get_editing_file_info(FILE_INFO *const fi) {
  (void)fi;
  return errg(err_not_implemented_yet);
}

error aviutl_get_exedit_window(HWND *const h) {
  (void)h;
  return errg(err_not_implemented_yet);
}

error aviutl_get_file_info(struct wstr const *const path, FILE_INFO *const fi, int *const samples) {
  (void)path;
  (void)fi;
  (void)samples;
  return errg(err_not_implemented_yet);
}

error clipboard_get(struct clipboard *const c) {
  (void)c;
  return errg(err_not_implemented_yet);
}

error clipboard_free(struct clipboard *const c) {
  (void)c;
  return errg(err_not_implemented_yet);
}
