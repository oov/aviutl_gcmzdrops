#pragma once

#include <stdbool.h>
#include <stdint.h>

#include <oleidl.h>

#include "files.h"
#include "ovbase.h"

struct drag_drop_info {
  POINTL point;
  DWORD key_state;
  DWORD effect;
};

struct drop_target {
  IDropTarget super;
  LONG refcount;
  struct files dragging_files;

  void *userdata;
  void (*drag_enter)(struct files *const f, struct drag_drop_info *const ddi, void *const userdata);
  void (*drag_over)(struct files *const f, struct drag_drop_info *const ddi, void *const userdata);
  void (*drag_leave)(void *const userdata);
  void (*drop)(struct files *const f, struct drag_drop_info *const ddi, void *const userdata);
};

NODISCARD error drop_target_new(struct drop_target **const r);

struct clipboard {
  struct files files;
  IDataObject *dataobj;
};

NODISCARD error clipboard_get(struct clipboard *const c);
NODISCARD error clipboard_free(struct clipboard *const c);
