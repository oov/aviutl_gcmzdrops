#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "3rd/base.c/base.h"

enum data_uri_encoding {
  data_uri_encoding_percent = 0,
  data_uri_encoding_base64 = 1,
};

struct data_uri {
  wchar_t mime[256];
  wchar_t charset[128];
  wchar_t ext_filename[128]; // Non-standard extension
  int encoding;
  wchar_t *encoded;
  size_t encoded_len;
  void *decoded;
  size_t decoded_len;
};

NODISCARD error data_uri_parse(wchar_t const *const ws, size_t const wslen, struct data_uri *const d);
NODISCARD error data_uri_decode(struct data_uri *const d);
NODISCARD error data_uri_free(struct data_uri *const d);
NODISCARD error data_uri_suggest_filename(struct data_uri const *const d, struct wstr *const dest);
NODISCARD error data_uri_get_mime(struct data_uri const *const d, struct wstr *const dest);
