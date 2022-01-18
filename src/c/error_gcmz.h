#pragma once

#include "ovbase.h"

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

NODISCARD error error_gcmz_init(void);
