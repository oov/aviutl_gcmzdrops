#pragma once

#include <stdint.h>
#include <stdbool.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "base.h"

NODISCARD error gcmz_get_script_dir(struct wstr *const dest);
NODISCARD error gcmz_get_project_dir(struct wstr *const dest);
NODISCARD error gcmz_get_save_dir(struct wstr *const dest);
NODISCARD error gcmz_is_need_copy(struct wstr const *const path, bool *const need_copy);
NODISCARD error gcmz_exists_in_save_dir(struct wstr const *const path, bool *const exists);
NODISCARD error gcmz_advance_frame(int const n);
NODISCARD error gcmz_drop(HWND const window, POINT const pt, struct wstr const *const *const filepath);

enum gcmz_layer_height
{
  gcmz_layer_height_small = 22,
  gcmz_layer_height_medium = 26,
  gcmz_layer_height_large = 31,
};

struct gcmz_analysed_info
{
  POINT edit_cursor;
  int layer_height;
  int zoom_level;
};

NODISCARD error gcmz_analyse_exedit_window(struct gcmz_analysed_info *const ai);
NODISCARD error gcmz_set_zoom_level(int const zoom);

NODISCARD error gcmz_prompt(struct wstr const *const caption, struct wstr *const value, bool *const result);
NODISCARD error gcmz_confirm(struct wstr const *const caption, bool *const result);
