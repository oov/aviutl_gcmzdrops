#include "3rd/base.c/test.h"
#include "datauri.c"

static void test_base64_decode(void) {
  static const struct test_data {
    wchar_t const *input;
    char const *output;
    uint_least32_t code;
  } test_data[] = {
      {
          .input = L"Y",
          .code = err_fail,
      },
      {
          .input = L"Y===",
          .code = err_fail,
      },
      {
          .input = L"=",
          .code = err_fail,
      },
      {
          .input = L"==",
          .code = err_fail,
      },
      {
          .input = L"===",
          .code = err_fail,
      },
      {
          .input = L"====",
          .code = err_fail,
      },
      {
          .input = L"",
          .output = "",
          .code = 0,
      },
      {
          .input = L"YQ==",
          .output = "a",
          .code = 0,
      },
      {
          .input = L"YQ",
          .output = "a",
          .code = 0,
      },
      {
          .input = L"YWI=",
          .output = "ab",
          .code = 0,
      },
      {
          .input = L"YWI",
          .output = "ab",
          .code = 0,
      },
      {
          .input = L"YWJj",
          .output = "abc",
          .code = 0,
      },
      {
          .input = L"YWJjZA==",
          .output = "abcd",
          .code = 0,
      },
      {
          .input = L"YWJjZA",
          .output = "abcd",
          .code = 0,
      },
      {
          .input = L"QWxwaGFCZXRhR2FtbWHml6XmnKzoqp7jga7mlofnq6BVVEYtOA==",
          .output = "\x41\x6C\x70\x68\x61\x42\x65\x74\x61\x47\x61\x6D\x6D\x61\xE6\x97\xA5\xE6\x9C\xAC\xE8\xAA\x9E\xE3"
                    "\x81\xAE\xE6\x96\x87\xE7\xAB\xA0\x55\x54\x46\x2D\x38",
          .code = 0,
      },
  };
  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d %ls", i, td->input);

    size_t decoded_len = 0;
    TEST_EISG_F(base64_decoded_len(td->input, wcslen(td->input), &decoded_len), td->code);
    if (td->code != 0) {
      continue;
    }

    size_t expected_len = strlen(td->output);
    TEST_CHECK(decoded_len == expected_len);
    TEST_MSG("expected %d", expected_len);
    TEST_MSG("got %d", decoded_len);

    void *p = malloc(expected_len);
    TEST_ASSERT(p != NULL);

    TEST_EISG_F(base64_decode(td->input, wcslen(td->input), p, expected_len), td->code);
    if (td->code != 0) {
      free(p);
      continue;
    }
    TEST_CHECK(memcmp(td->output, p, expected_len) == 0);
    TEST_DUMP("expected", td->output, expected_len);
    TEST_DUMP("got", p, expected_len);
    free(p);
  }
}

static void test_base64_error(void) {
  wchar_t const *input = L"YWJjZA==";
  size_t input_len = wcslen(input);
  TEST_EISG_F(base64_decoded_len(NULL, 0, NULL), err_invalid_arugment);
  TEST_EISG_F(base64_decoded_len(NULL, input_len, NULL), err_invalid_arugment);
  TEST_EISG_F(base64_decoded_len(input, input_len, NULL), err_null_pointer);

  char b[4] = {0};
  TEST_EISG_F(base64_decode(NULL, 0, NULL, 0), err_invalid_arugment);
  TEST_EISG_F(base64_decode(NULL, input_len, NULL, 0), err_invalid_arugment);
  TEST_EISG_F(base64_decode(input, input_len, NULL, 0), err_null_pointer);
  TEST_EISG_F(base64_decode(input, input_len, b, 0), err_not_sufficient_buffer);
  TEST_EISG_F(base64_decode(input, input_len, b, 3), err_not_sufficient_buffer);

  TEST_SUCCEEDED_F(base64_decode(L"", 0, NULL, 0)); // special case
}

static void test_percent_decode(void) {
  static const struct test_data {
    wchar_t const *input;
    char const *output;
    uint_least32_t code;
  } test_data[] = {
      {
          .input = L"a%",
          .code = err_fail,
      },
      {
          .input = L"a%1",
          .code = err_fail,
      },
      {
          .input = L"a%1z",
          .code = err_fail,
      },
      {
          .input = L"a%1za",
          .code = err_fail,
      },
      {
          .input = L"\x89\x50\x4E\x47",
          .code = err_fail,
      },
      {
          .input = L"",
          .output = "",
          .code = 0,
      },
      {
          .input = L"a",
          .output = "a",
          .code = 0,
      },
      {
          .input = L"%20",
          .output = " ",
          .code = 0,
      },
      {
          .input = L"a%20",
          .output = "a ",
          .code = 0,
      },
      {
          .input = L"a%20a",
          .output = "a a",
          .code = 0,
      },
      {
          .input = L"%20a",
          .output = " a",
          .code = 0,
      },
  };
  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d %ls", i, td->input);

    size_t decoded_len = 0;
    TEST_EISG_F(percent_decoded_len(td->input, wcslen(td->input), &decoded_len), td->code);
    if (td->code != 0) {
      continue;
    }

    size_t expected_len = strlen(td->output);
    TEST_CHECK(decoded_len == expected_len);
    TEST_MSG("expected %d", expected_len);
    TEST_MSG("got %d", decoded_len);

    void *p = malloc(expected_len);
    TEST_ASSERT(p != NULL);

    TEST_EISG_F(percent_decode(td->input, wcslen(td->input), p, expected_len), td->code);
    if (td->code != 0) {
      free(p);
      continue;
    }
    TEST_CHECK(memcmp(td->output, p, expected_len) == 0);
    TEST_DUMP("expected", td->output, expected_len);
    TEST_DUMP("got", p, expected_len);
    free(p);
  }
}

static void test_percent_error(void) {
  wchar_t const *input = L"a%20bc";
  size_t input_len = wcslen(input);
  TEST_EISG_F(percent_decoded_len(NULL, 0, NULL), err_invalid_arugment);
  TEST_EISG_F(percent_decoded_len(NULL, input_len, NULL), err_invalid_arugment);
  TEST_EISG_F(percent_decoded_len(input, input_len, NULL), err_null_pointer);

  char b[4] = {0};
  TEST_EISG_F(percent_decode(NULL, 0, NULL, 0), err_invalid_arugment);
  TEST_EISG_F(percent_decode(NULL, input_len, NULL, 0), err_invalid_arugment);
  TEST_EISG_F(percent_decode(input, input_len, NULL, 0), err_null_pointer);
  TEST_EISG_F(percent_decode(input, input_len, b, 0), err_not_sufficient_buffer);
  TEST_EISG_F(percent_decode(input, input_len, b, 3), err_not_sufficient_buffer);

  TEST_SUCCEEDED_F(percent_decode(L"", 0, NULL, 0)); // special case
}

static void test_parse_decode(void) {
  static const struct test_data {
    wchar_t const *input;
    struct data_uri output;
    wchar_t const *suggest_filename;
    uint_least32_t code;
  } test_data[] = {
      {
          .input = L"",
          .code = err_fail,
      },
      {
          .input = L"data:",
          .code = err_fail,
      },
      {
          .input = L"data:,",
          .output =
              {
                  .mime = L"text/plain",
                  .charset = L"US-ASCII",
                  .encoding = data_uri_encoding_percent,
                  .decoded = NULL,
                  .decoded_len = 0,
              },
          .suggest_filename = L"noname.txt",
          .code = 0,
      },
      {
          .input = L"data:text/javascript,a%3f",
          .output =
              {
                  .mime = L"text/javascript",
                  .charset = L"",
                  .encoding = data_uri_encoding_percent,
                  .decoded = "a?",
                  .decoded_len = 2,
              },
          .suggest_filename = L"noname.bin",
          .code = 0,
      },
      {
          .input = L"data:text/javascript;charset=UTF-8;base64,YWJj",
          .output =
              {
                  .mime = L"text/javascript",
                  .charset = L"UTF-8",
                  .encoding = data_uri_encoding_base64,
                  .decoded = "abc",
                  .decoded_len = 3,
              },
          .suggest_filename = L"noname.bin",
          .code = 0,
      },
      {
          .input = L"data:text/javascript;base64,YWJj",
          .output =
              {
                  .mime = L"text/javascript",
                  .charset = L"",
                  .encoding = data_uri_encoding_base64,
                  .decoded = "abc",
                  .decoded_len = 3,
              },
          .suggest_filename = L"noname.bin",
          .code = 0,
      },
      {
          .input = L"data:text/javascript;filename=test.js;base64,YWJj",
          .output =
              {
                  .mime = L"text/javascript",
                  .charset = L"",
                  .encoding = data_uri_encoding_base64,
                  .ext_filename = L"test.js",
                  .decoded = "abc",
                  .decoded_len = 3,
              },
          .suggest_filename = L"test.js",
          .code = 0,
      },
  };
  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i) {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d \"%ls\"", i, td->input);

    struct data_uri d = {0};
    TEST_EISG_F(data_uri_parse(td->input, wcslen(td->input), &d), td->code);
    if (td->code != 0) {
      continue;
    }

    TEST_CHECK(wcscmp(td->output.mime, d.mime) == 0);
    TEST_MSG("expected %ls", td->output.mime);
    TEST_MSG("got %ls", d.mime);

    TEST_CHECK(wcscmp(td->output.charset, d.charset) == 0);
    TEST_MSG("expected %ls", td->output.charset);
    TEST_MSG("got %ls", d.charset);

    TEST_CHECK(wcscmp(td->output.ext_filename, d.ext_filename) == 0);
    TEST_MSG("expected %ls(%d)", td->output.ext_filename, wcslen(td->output.ext_filename));
    TEST_MSG("got %ls(%d)", d.ext_filename, wcslen(d.ext_filename));

    TEST_CHECK(td->output.encoding == d.encoding);
    TEST_MSG("expected %d", td->output.encoding);
    TEST_MSG("got %d", d.encoding);

    TEST_CHECK(d.decoded_len == 0);
    TEST_CHECK(d.decoded == NULL);
    if (!TEST_SUCCEEDED_F(data_uri_decode(&d))) {
      continue;
    }
    if (!TEST_CHECK(td->output.decoded_len == d.decoded_len)) {
      continue;
    }
    if (d.decoded_len > 0) {
      TEST_CHECK(memcmp(td->output.decoded, d.decoded, d.decoded_len) == 0);
      TEST_DUMP("expected", td->output.decoded, d.decoded_len);
      TEST_DUMP("got", d.decoded, d.decoded_len);
    } else {
      TEST_CHECK(td->output.decoded == NULL);
    }
    struct wstr fn = {0};
    if (TEST_SUCCEEDED_F(data_uri_suggest_filename(&d, &fn))) {
      TEST_CHECK(wcscmp(fn.ptr, td->suggest_filename) == 0);
      TEST_MSG("expected %ls(%d)", td->suggest_filename, wcslen(td->suggest_filename));
      TEST_MSG("got %ls(%d)", fn.ptr, fn.len);
    }
    ereport(sfree(&fn));

    TEST_SUCCEEDED_F(data_uri_free(&d));
  }
}

static void test_get_mime(void) {
  struct data_uri d = {
      .mime = L"text/plain",
  };
  TEST_EISG_F(data_uri_get_mime(NULL, NULL), err_invalid_arugment);
  TEST_EISG_F(data_uri_get_mime(&d, NULL), err_null_pointer);

  struct wstr tmp = {0};
  if (TEST_SUCCEEDED_F(data_uri_get_mime(&d, &tmp))) {
    TEST_CHECK(wcscmp(tmp.ptr, d.mime) == 0);
  }
  struct data_uri d2 = {
      .mime = L"text/plain",
      .charset = L"UTF-8",
  };
  if (TEST_SUCCEEDED_F(data_uri_get_mime(&d2, &tmp))) {
    TEST_CHECK(wcscmp(tmp.ptr, L"text/plain; charset=UTF-8") == 0);
  }
  ereport(sfree(&tmp));
}

TEST_LIST = {
    {"test_base64_decode", test_base64_decode},
    {"test_base64_error", test_base64_error},
    {"test_percent_decode", test_percent_decode},
    {"test_percent_error", test_percent_error},
    {"test_parse_decode", test_parse_decode},
    {"test_get_mime", test_get_mime},
    {NULL, NULL},
};
