#pragma once

#include <stdint.h>
#include <stdbool.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include "base.h"

struct scpopup
{
  HWND window;
  LRESULT(*popup)
  (struct scpopup *const p, POINT pt);
};

struct scpopup_menu_sub_item
{
  struct wstr caption;
  UINT_PTR id;
};

struct scpopup_menu_item
{
  struct scpopup_menu_sub_item *ptr;
  size_t len;
  size_t cap;

  struct wstr caption;
  UINT_PTR id;
};

struct scpopup_menu
{
  struct scpopup_menu_item *ptr;
  size_t len;
  size_t cap;
};

NODISCARD error scpopup_init(struct scpopup *const p, HWND const window);
NODISCARD error scpopup_exit(struct scpopup *const p);
NODISCARD error scpopup_show_popup(HWND const window, POINT const pt, struct scpopup_menu *const m, UINT_PTR *const selected);
NODISCARD error scpopup_menu_free(struct scpopup_menu *const m);
