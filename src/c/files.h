#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "ovbase.h"

struct file {
  struct wstr path;
  struct wstr mime;
  bool temporary;
  char reserved[3];
};

struct files {
  struct file *ptr;
  size_t len;
  size_t cap;
};

NODISCARD error files_add(struct files *const f, struct wstr const *const path, struct wstr const *const mime);
NODISCARD error files_delete_last(struct files *const f, bool const delete_later);
NODISCARD error files_free(struct files *const f, bool const delete_later);

NODISCARD error files_add_delete_on_failure(struct wstr const *const path);
NODISCARD error files_add_delete_on_cleanup(struct wstr const *const path);
void files_cleanup(bool const failed);
