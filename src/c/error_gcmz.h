#pragma once

#include "ovbase.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

enum {
  err_type_gcmz = 100,
};

enum err_gcmz {
  err_gcmz_unsupported_aviutl_version = 100,
  err_gcmz_exedit_not_found = 101,
  err_gcmz_exedit_not_found_in_same_dir = 102,
  err_gcmz_lua51_cannot_load = 103,
  err_gcmz_unsupported_exedit_version = 104,
  err_gcmz_project_is_not_open = 105,
  err_gcmz_project_has_not_yet_been_saved = 106,

  err_gcmz_extext_found = 130,
  err_gcmz_oledd_found = 131,

  err_gcmz_failed_to_detect_zoom_level = 200,
  err_gcmz_failed_to_detect_layer_height = 201,
  err_gcmz_exists_different_hash_value_file = 202,

  err_gcmz_lua = 300,
  err_gcmz_invalid_char = 301,
};

NODISCARD error gcmz_error_message(int const type, int const code, struct NATIVE_STR *const dest);

NODISCARD error gcmz_error_to_string(error e, struct wstr *const dest);
NODISCARD error gcmz_error_vformat(
    error e, struct wstr *const dest, wchar_t const *const reference, char const *const format, va_list valist);
NODISCARD error
gcmz_error_format(error e, struct wstr *const dest, wchar_t const *const reference, char const *const format, ...);
void gcmz_error_message_box(error e,
                            HWND const window,
                            bool const deferred,
                            char const *const title,
                            wchar_t const *const reference,
                            char const *const format,
                            ...);
