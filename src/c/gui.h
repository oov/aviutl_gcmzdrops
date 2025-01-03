#pragma once

#include <stdbool.h>
#include <stdint.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "ovbase.h"

enum gui_mode {
  gui_mode_auto = 0,
  gui_mode_copy = 1,
  gui_mode_direct = 2,
};

enum gui_extended_menu {
  gui_extended_menu_disable = 0,
  gui_extended_menu_wheel_click = 1,
  gui_extended_menu_shift_ctrl_right_click = 2,
};

NODISCARD error gui_init(HWND const window);
void gui_exit(void);
void gui_lock(void);
void gui_handle_wm_command(HWND const window, WPARAM const wparam, LPARAM const lparam);
NODISCARD error gui_set_save_dir(wchar_t const *const dir);
NODISCARD error gui_set_save_dir_to_default(void);
NODISCARD error gui_get_save_dir(struct wstr *const dest);
NODISCARD error gui_set_save_mode(int const mode);
NODISCARD error gui_set_save_mode_to_default(void);
NODISCARD error gui_get_save_mode(int *const mode);
NODISCARD error gui_set_activation_method(int const method);
NODISCARD error gui_get_activation_method(int *const method);
