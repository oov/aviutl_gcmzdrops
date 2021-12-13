#pragma once

#include <stdbool.h>
#include <stdint.h>

#if __STDC_VERSION__ >= 201112L && !defined(__STDC_NO_THREADS__)
#include <threads.h>
#else
#include "3rd/base.c/3rd/threads/threads.h"
#endif

#include "3rd/base.c/base.h"
#include "files.h"

struct cv {
  int var;
  cnd_t cnd;
  mtx_t mtx;
};

struct api_request_params {
  struct files files;
  int layer;
  int frame_advance;

  void (*complete)(struct api_request_params *params);

  struct {
    struct cv *process;
  } priv;
};

struct api {
  void (*request)(void *userdata, struct api_request_params *params, error err);
  void *userdata;

  struct {
    HWND window;
    HANDLE mutex;
    HANDLE fmo;

    thrd_t thread;
    struct cv state;
    struct cv process;
  } priv;
};

NODISCARD error api_init(struct api *const api);
NODISCARD error api_update_mapped_data(struct api *const api);
NODISCARD error api_exit(struct api *const api);
NODISCARD bool api_initialized(struct api const *const api);
