#include "3rd/base.c/include/test.h"
#include "util.c"

static void test_atoi64(void) {
  static const struct test_data {
    wchar_t const *input;
    uint_least32_t code;
    int64_t output;
  } test_data[] = {
      {
          .input = L"0",
          .output = 0,
          .code = 0,
      },
      {
          .input = L"1",
          .output = 1,
          .code = 0,
      },
      {
          .input = L"-1",
          .output = -1,
          .code = 0,
      },
      {
          .input = L"9223372036854775807",
          .output = INT64_MAX,
          .code = 0,
      },
      {
          .input = L"9223372036854775808",
          .code = err_fail,
      },
      {
          .input = L"-9223372036854775808",
          .output = INT64_MIN,
          .code = 0,
      },
      {
          .input = L"-9223372036854775809",
          .code = err_fail,
      },
      {
          .input = L"0x0",
          .code = err_fail,
      },
      {
          .input = L"hello",
          .code = err_fail,
      },
  };

  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d \"%ls\"", i, td->input);
    int64_t r = 0;
    if (TEST_EISG_F(atoi64(&wstr_unmanaged_const(td->input), &r), td->code) && td->code == 0) {
      TEST_CHECK(r == td->output);
    }
  }
}

static void test_atou64(void) {
  static const struct test_data {
    wchar_t const *input;
    uint_least32_t code;
    uint64_t output;
  } test_data[] = {
      {
          .input = L"0",
          .output = 0,
          .code = 0,
      },
      {
          .input = L"18446744073709551615",
          .output = UINT64_MAX,
          .code = 0,
      },
      {
          .input = L"18446744073709551616",
          .code = err_fail,
      },
      {
          .input = L"-1",
          .code = err_fail,
      },
      {
          .input = L"0x0",
          .code = err_fail,
      },
      {
          .input = L"hello",
          .code = err_fail,
      },
  };

  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d \"%ls\"", i, td->input);
    uint64_t r = 0;
    if (TEST_EISG_F(atou64(&wstr_unmanaged_const(td->input), &r), td->code) && td->code == 0) {
      TEST_CHECK(r == td->output);
    }
  }
}

static void test_utoa64(void) {
  static const struct test_data {
    uint64_t input;
    wchar_t const *output;
    int32_t reserved;
  } test_data[] = {
      {
          .input = 0ULL,
          .output = L"0",
      },
      {
          .input = UINT64_MAX,
          .output = L"18446744073709551615",
      },
  };

  struct wstr tmp = {0};
  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d \"%ls\"", i, td->output);
    if (TEST_SUCCEEDED_F(utoa64(td->input, &tmp))) {
      TEST_CHECK(tmp.len > 0);
      TEST_CHECK(wcscmp(tmp.ptr, td->output) == 0);
      TEST_MSG("expected %ls", td->output);
      TEST_MSG("got %ls", tmp.ptr);
    }
  }
  ereport(sfree(&tmp));
}

static void test_include_trailing_path_delimiter(void) {
  static const struct test_data {
    wchar_t const *input;
    wchar_t const *output;
  } test_data[] = {
      {
          .input = L"hello",
          .output = L"hello\\",
      },
      {
          .input = L"hello\\",
          .output = L"hello\\",
      },
      {
          .input = L"hello\\world",
          .output = L"hello\\world\\",
      },
      {
          .input = L"hello\\world\\",
          .output = L"hello\\world\\",
      },
      {
          .input = L"hello/",
          .output = L"hello/",
      },
      {
          .input = L"hello/world",
          .output = L"hello/world\\",
      },
      {
          .input = L"hello/world/",
          .output = L"hello/world/",
      },
  };

  struct wstr ws = {0};
  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d \"%ls\"", i, td->input);
    TEST_SUCCEEDED_F(scpy(&ws, td->input));
    if (TEST_SUCCEEDED_F(include_trailing_path_delimiter(&ws))) {
      TEST_CHECK(ws.ptr[ws.len] == L'\0');
      TEST_CHECK(wcscmp(ws.ptr, td->output) == 0);
      TEST_MSG("expected %ls", td->output);
      TEST_MSG("got %ls", ws.ptr);
    }
  }
  TEST_SUCCEEDED_F(sfree(&ws));
  TEST_EISG_F(include_trailing_path_delimiter(NULL), err_invalid_arugment);
  TEST_EISG_F(include_trailing_path_delimiter(&ws), err_invalid_arugment);
}

static void test_exclude_trailing_path_delimiter(void) {
  static const struct test_data {
    wchar_t const *input;
    wchar_t const *output;
  } test_data[] = {
      {
          .input = L"hello",
          .output = L"hello",
      },
      {
          .input = L"hello\\",
          .output = L"hello",
      },
      {
          .input = L"hello\\world",
          .output = L"hello\\world",
      },
      {
          .input = L"hello\\world\\",
          .output = L"hello\\world",
      },
      {
          .input = L"hello/",
          .output = L"hello",
      },
      {
          .input = L"hello/world",
          .output = L"hello/world",
      },
      {
          .input = L"hello/world/",
          .output = L"hello/world",
      },
  };

  struct wstr ws = {0};
  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d \"%ls\"", i, td->input);
    TEST_SUCCEEDED_F(scpy(&ws, td->input));
    if (TEST_SUCCEEDED_F(exclude_trailing_path_delimiter(&ws))) {
      TEST_CHECK(ws.ptr[ws.len] == L'\0');
      TEST_CHECK(wcscmp(ws.ptr, td->output) == 0);
      TEST_MSG("expected %ls", td->output);
      TEST_MSG("got %ls", ws.ptr);
    }
  }
  TEST_SUCCEEDED_F(sfree(&ws));
  TEST_EISG_F(exclude_trailing_path_delimiter(NULL), err_invalid_arugment);
  TEST_EISG_F(exclude_trailing_path_delimiter(&ws), err_invalid_arugment);
}

static void test_extract_file_name(void) {
  static const struct test_data {
    wchar_t const *input;
    size_t output;
  } test_data[] = {
      {
          .input = L"your.exe",
          .output = 0,
      },
      {
          .input = L"C:\\test\\your.exe",
          .output = 8,
      },
      {
          .input = L"C:/test/your.exe",
          .output = 8,
      },
      {
          .input = L"C:\\test/your.exe",
          .output = 8,
      },
      {
          .input = L"C:/test\\your.exe",
          .output = 8,
      },
  };
  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d \"%ls\"", i, td->input);
    struct wstr src = wstr_unmanaged_const(td->input);
    size_t pos = 0;
    if (TEST_SUCCEEDED_F(extract_file_name(&src, &pos))) {
      TEST_CHECK(pos == td->output);
      TEST_MSG("expected %d", td->output);
      TEST_MSG("got %d", pos);
    }
  }
}

static void test_extract_file_extension(void) {
  static const struct test_data {
    wchar_t const *input;
    size_t output;
  } test_data[] = {
      {
          .input = L"your.exe",
          .output = 4,
      },
      {
          .input = L"C:\\test\\your.exe",
          .output = 12,
      },
      {
          .input = L"C:/test/your.exe",
          .output = 12,
      },
      {
          .input = L"C:\\test/your.exe",
          .output = 12,
      },
      {
          .input = L"C:/test\\your.exe",
          .output = 12,
      },
      {
          .input = L"your.tmp.exe",
          .output = 8,
      },
      {
          .input = L"C:\\test\\your.tmp.exe",
          .output = 16,
      },
      {
          .input = L"C:/test/your.tmp.exe",
          .output = 16,
      },
      {
          .input = L"C:\\test/your.tmp.exe",
          .output = 16,
      },
      {
          .input = L"C:/test\\your.tmp.exe",
          .output = 16,
      },
      {
          .input = L"your",
          .output = 4,
      },
      {
          .input = L"C:\\test\\your",
          .output = 12,
      },
      {
          .input = L"C:/test/your",
          .output = 12,
      },
      {
          .input = L"C:\\test/your",
          .output = 12,
      },
      {
          .input = L"C:/test\\your",
          .output = 12,
      },
  };
  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d \"%ls\"", i, td->input);
    struct wstr const src = wstr_unmanaged_const(td->input);
    size_t pos = 0;
    if (TEST_SUCCEEDED_F(extract_file_extension(&src, &pos))) {
      TEST_CHECK(pos == td->output);
      TEST_MSG("expected %d", td->output);
      TEST_MSG("got %d", pos);
    }
  }
}

static void test_file_contains(void) {
  static const struct test_data {
    wchar_t const *dir;
    wchar_t const *file;
    bool contains;
    char reserved[3];
  } test_data[] = {
      {
          .dir = L"C:\\example\\dir",
          .file = L"C:\\example\\dir\\file.txt",
          .contains = true,
      },
      {
          .dir = L"C:\\example\\dir",
          .file = L"C:\\example\\file.txt",
          .contains = false,
      },
      {
          .dir = L"C:\\example\\dir\\",
          .file = L"C:\\example\\dir\\file.txt",
          .contains = true,
      },
      {
          .dir = L"C:\\example\\dir\\",
          .file = L"C:\\example\\file.txt",
          .contains = false,
      },
      {
          .dir = L"C:\\example\\DIR",
          .file = L"C:\\example\\dir\\file.txt",
          .contains = true,
      },
      {
          .dir = L"C:\\example\\DIR",
          .file = L"C:\\example\\file.txt",
          .contains = false,
      },
      {
          .dir = L"C:\\example\\DIR\\",
          .file = L"C:\\example\\dir\\file.txt",
          .contains = true,
      },
      {
          .dir = L"C:\\example\\DIR\\",
          .file = L"C:\\example\\file.txt",
          .contains = false,
      },
  };

  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d \"%ls\"", i, td->file);
    bool contains = false;
    if (TEST_SUCCEEDED_F(file_contains(&wstr_unmanaged_const(td->dir), &wstr_unmanaged_const(td->file), &contains))) {
      TEST_CHECK(contains == td->contains);
    }
  }
}

static void test_create_unique_temp_file(void) {
  struct wstr path = {0};
  if (!TEST_SUCCEEDED_F(create_unique_temp_file(L"test_create_unique_temp_file1", L".txt", NULL, 0, &path))) {
    return;
  }
  TEST_CHECK(path.ptr != NULL);
  TEST_CHECK(path.len > 0);
  TEST_SUCCEEDED_F(delete_file(&path));
  TEST_SUCCEEDED_F(sfree(&path));

  if (!TEST_SUCCEEDED_F(create_unique_temp_file(L"test_create_unique_temp_file2", L".txt", "hello", 5, &path))) {
    return;
  }
  TEST_CHECK(path.ptr != NULL);
  HANDLE h = CreateFileW(path.ptr, GENERIC_READ, 0, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (TEST_CHECK(h != INVALID_HANDLE_VALUE)) {
    char buf[6] = {0};
    DWORD read = 0;
    if (TEST_CHECK(ReadFile(h, buf, 6, &read, NULL) == TRUE && read == 5)) {
      TEST_CHECK(strcmp(buf, "hello") == 0);
    }
    TEST_CHECK(CloseHandle(h) == TRUE);
  }
  TEST_SUCCEEDED_F(delete_file(&path));
  TEST_SUCCEEDED_F(sfree(&path));
}

static void test_from_utf8(void) {
  static struct str const src = str_unmanaged("\xE3\x81\x93\xE3\x82\x93\xE3\x81\xAB\xE3\x81\xA1\xE3\x81\xAF");
  static wchar_t const *const ws = L"こんにちは";
  size_t const len = wcslen(ws);
  struct wstr dest = {0};
  if (TEST_SUCCEEDED_F(from_utf8(&src, &dest))) {
    TEST_CHECK(wcscmp(dest.ptr, ws) == 0);
    TEST_CHECK(dest.len == len);
    TEST_MSG("expected %d", len);
    TEST_MSG("got %d", dest.len);
    TEST_SUCCEEDED_F(sfree(&dest));
  }
  TEST_EISG_F(from_utf8(NULL, &dest), err_invalid_arugment);
  TEST_EISG_F(from_utf8(&src, NULL), err_null_pointer);
}

static void test_to_utf8(void) {
  static struct wstr const src = wstr_unmanaged(L"こんにちは");
  static char const *const utf8 = "\xE3\x81\x93\xE3\x82\x93\xE3\x81\xAB\xE3\x81\xA1\xE3\x81\xAF";
  size_t const len = strlen(utf8);
  struct str dest = {0};
  if (TEST_SUCCEEDED_F(to_utf8(&src, &dest))) {
    TEST_CHECK(strcmp(dest.ptr, utf8) == 0);
    TEST_CHECK(dest.len == len);
    ereport(sfree(&dest));
  }
  TEST_EISG_F(to_utf8(NULL, &dest), err_invalid_arugment);
  TEST_EISG_F(to_utf8(&src, NULL), err_null_pointer);
}

TEST_LIST = {
    {"test_atoi64", test_atoi64},
    {"test_atou64", test_atou64},
    {"test_utoa64", test_utoa64},
    {"test_include_trailing_path_delimiter", test_include_trailing_path_delimiter},
    {"test_exclude_trailing_path_delimiter", test_exclude_trailing_path_delimiter},
    {"test_extract_file_name", test_extract_file_name},
    {"test_extract_file_extension", test_extract_file_extension},
    {"test_file_contains", test_file_contains},
    {"test_create_unique_temp_file", test_create_unique_temp_file},
    {"test_from_utf8", test_from_utf8},
    {"test_to_utf8", test_to_utf8},
    {NULL, NULL},
};
