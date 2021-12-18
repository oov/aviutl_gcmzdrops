#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "3rd/base.c/base.h"
#include "files.h"

struct api;
struct api_request_params {
  struct files files;
  int layer;
  int frame_advance;

  error err;
  void *userdata;
};

typedef void (*api_request_complete_func)(struct api_request_params *const params);
typedef void (*api_request_func)(struct api_request_params *const params, api_request_complete_func const complete);

NODISCARD error api_init(struct api **const api);
void api_set_callback(struct api *const api, api_request_func const callback, void *const userdata);
NODISCARD error api_update_mapped_data(struct api *const api);
NODISCARD error api_exit(struct api **const api);
NODISCARD bool api_initialized(struct api const *const api);
