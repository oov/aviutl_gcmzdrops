#include "api.h"

#include <jansson.h>

#include "ovnum.h"
#include "ovthreads.h"
#include "ovutil/str.h"
#include "ovutil/win32.h"

#include "aviutl.h"
#include "error_gcmz.h"
#include "task.h"

struct api {
  api_request_func request;
  void *userdata;

  HWND window;
  HANDLE mutex;
  HANDLE fmo;

  thrd_t thread;
  struct cndvar state;
  struct cndvar process;
};

struct api_request_params_internal {
  struct api_request_params params;
  struct cndvar *process;
};

struct gcmzdrops_fmo {
  uint32_t window;
  int32_t width;
  int32_t height;
  int32_t video_rate;
  int32_t video_scale;
  int32_t audio_rate;
  int32_t audio_ch;
  int32_t gcmz_api_ver;
  wchar_t project_path[MAX_PATH];
  uint32_t flags;
};

enum {
  state_undefined = 0,
  state_succeeded = 1,
  state_failed = 2,
};

NODISCARD static error
parse_api0_found_token(struct wstr const *const ws, struct api_request_params *const params, size_t *const found) {
  error err = eok();
  int64_t v = 0;
  switch (*found) {
  case 0:
    if (!ov_atoi(ws->ptr, &v, false)) {
      err = errg(err_fail);
      goto cleanup;
    }
    params->layer = (int)v;
    break;
  case 1:
    if (!ov_atoi(ws->ptr, &v, false)) {
      err = errg(err_fail);
      goto cleanup;
    }
    params->frame_advance = (int)v;
    break;
  default:
    err = files_add(&params->files, ws, &wstr_unmanaged(L"application/octet-stream"));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  ++*found;

cleanup:
  return err;
}

NODISCARD static error parse_api0(struct wstr *const ws, struct api_request_params *const params) {
  struct wstr tmp = {0};

  // ws in not null-terminated and may source is unmanaged buffer so we need copy to modify
  error err = sgrow(&tmp, ws->len + 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  memcpy(tmp.ptr, ws->ptr, ws->len * sizeof(wchar_t));
  tmp.len = ws->len;
  tmp.ptr[tmp.len] = L'\0';

  wchar_t const *p = tmp.ptr;
  size_t found = 0, token = 0;
  size_t const len = tmp.len;
  for (size_t i = 0; i < len; ++i) {
    if (p[i] != L'\0') {
      ++token;
      continue;
    }
    if (token == 0) {
      err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("無効なフォーマットです。")));
      goto cleanup;
    }
    err =
        parse_api0_found_token(&(struct wstr const){.ptr = ov_deconster_(p + i - token), .len = token}, params, &found);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    token = 0;
  }
  if (token) {
    err = parse_api0_found_token(
        &(struct wstr const){.ptr = ov_deconster_(p + len - token), .len = token}, params, &found);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
cleanup:
  ereport(sfree(&tmp));
  return err;
}

NODISCARD static error parse_api1(struct str *const s, struct api_request_params *const params) {
  error err = eok();
  int added = 0;
  struct wstr tmp = {0};
  json_t *root = json_loadb(s->ptr, s->len, 0, NULL);
  if (!json_is_object(root)) {
    err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("JSON としての読み込みに失敗しました。")));
    goto cleanup;
  }

  int64_t layer = 0;
  int64_t frame_advance = 0;

  json_t *v = json_object_get(root, "layer");
  if (!json_is_number(v)) {
    err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("layer の指定が正しくありません。")));
    goto cleanup;
  }
  layer = json_integer_value(v);

  v = json_object_get(root, "frameAdvance");
  if (json_is_number(v)) {
    frame_advance = json_integer_value(v);
  }

  v = json_object_get(root, "files");
  if (!json_is_array(v)) {
    err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("files の指定が正しくありません。")));
    goto cleanup;
  }
  for (size_t i = 0, len = json_array_size(v); i < len; ++i) {
    json_t *item = json_array_get(v, i);
    if (!json_is_string(item)) {
      err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("ファイル名が正しくありません。")));
      goto cleanup;
    }
    err = from_utf8(&str_unmanaged_const(json_string_value(item)), &tmp);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = files_add(&params->files, &tmp, &wstr_unmanaged(L"application/octet-stream"));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    ++added;
  }

  params->layer = (int)layer;
  params->frame_advance = (int)frame_advance;

cleanup:
  if (efailed(err)) {
    for (int i = 0; i < added; ++i) {
      ereport(files_delete_last(&params->files, false));
    }
  }
  ereport(sfree(&tmp));
  if (root) {
    json_decref(root);
  }
  return err;
}

struct process_task_data {
  struct api *api;
  struct api_request_params_internal pi;
  error err;
};

static void process_complete(struct api_request_params *const params) {
  struct api_request_params_internal *pi = (void *)params;
  cndvar_lock(pi->process);
  cndvar_signal(pi->process, state_succeeded);
  cndvar_unlock(pi->process);
}

static void process_task(void *const userdata) {
  struct process_task_data *const d = userdata;
  struct api *const api = d->api;
  if (api->request) {
    api->request(&d->pi.params, process_complete);
  } else {
    efree(&d->pi.params.err);
  }
}

static BOOL process(struct api *const api, HWND const sender, COPYDATASTRUCT *const cds) {
  (void)sender;
  struct process_task_data d = {
      .api = api,
  };
  error err = eok();
  switch (cds->dwData) {
  case 0:
    err = parse_api0(
        &(struct wstr){
            .ptr = cds->lpData,
            .len = cds->cbData / 2,
        },
        &d.pi.params);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    break;
  case 1:
    err = parse_api1(
        &(struct str){
            .ptr = cds->lpData,
            .len = cds->cbData,
        },
        &d.pi.params);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    break;
  default:
    err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("未知の API バージョンです。")));
    goto cleanup;
  }

  if (d.pi.params.layer == 0 || d.pi.params.layer < -100 || d.pi.params.layer > 100) {
    err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("レイヤー番号が不正です。")));
    goto cleanup;
  }
  cndvar_lock(&api->process);
  d.pi.process = &api->process;
  api->process.var = state_undefined;
  task_add(process_task, &d);
  cndvar_wait_while(&api->process, state_undefined);
  cndvar_unlock(&api->process);

cleanup:
  ereport(files_free(&d.pi.params.files, false));
  if (efailed(err)) {
    d.err = err;
    err = NULL; // err is owned by request handler
    cndvar_lock(&api->process);
    d.pi.process = &api->process;
    api->process.var = state_undefined;
    task_add(process_task, &d);
    cndvar_wait_while(&api->process, state_undefined);
    cndvar_unlock(&api->process);
    return FALSE;
  }
  return TRUE;
}

static LRESULT WINAPI wndproc(HWND const window, UINT const message, WPARAM const wparam, LPARAM const lparam) {
  switch (message) {
  case WM_COPYDATA:
    return process((struct api *)(GetWindowLongPtrW(window, 0)), (HWND)wparam, (COPYDATASTRUCT *)lparam);
  case WM_CLOSE:
    DestroyWindow(window);
    return 0;
  case WM_DESTROY:
    PostQuitMessage(0);
    return 0;
  default:
    return DefWindowProcW(window, message, wparam, lparam);
  }
}

static int api_thread(void *const userdata) {
  struct api *const api = userdata;
  error err = eok();
  {
    static wchar_t const window_class_name[] = L"GCMZDropsAPI";
    HINSTANCE hinst = get_hinstance();
    WNDCLASSEXW wc = {0};
    wc.cbSize = sizeof(WNDCLASSEXW);
    wc.lpfnWndProc = wndproc;
    wc.hInstance = hinst;
    wc.lpszClassName = window_class_name;
    wc.cbWndExtra = sizeof(struct api *);
    if (RegisterClassExW(&wc) == 0) {
      err = errhr(HRESULT_FROM_WIN32(GetLastError()));
      goto failed;
    }
    HWND h = CreateWindowExW(0,
                             window_class_name,
                             NULL,
                             WS_OVERLAPPEDWINDOW,
                             CW_USEDEFAULT,
                             CW_USEDEFAULT,
                             CW_USEDEFAULT,
                             CW_USEDEFAULT,
                             HWND_MESSAGE,
                             0,
                             hinst,
                             NULL);
    if (!h) {
      err = errhr(HRESULT_FROM_WIN32(GetLastError()));
      goto failed;
    }
    api->window = h;
    SetWindowLongPtrW(h, 0, (LONG)api);
  }

  cndvar_lock(&api->state);
  cndvar_signal(&api->state, state_succeeded);
  cndvar_unlock(&api->state);

  MSG msg = {0};
  while (GetMessageW(&msg, NULL, 0, 0) > 0) {
    TranslateMessage(&msg);
    DispatchMessageW(&msg);
  }
  return 0;

failed:
  ereportmsg(err, &native_unmanaged(NSTR("API 用スレッドでエラーが発生しました。")));
  cndvar_lock(&api->state);
  cndvar_signal(&api->state, state_failed);
  cndvar_unlock(&api->state);
  return 1;
}

NODISCARD static error api_thread_init(struct api *const api) {
  error err = eok();
  bool thread_started = false;

  cndvar_init(&api->state);
  cndvar_init(&api->process);

  cndvar_lock(&api->state);
  api->state.var = state_undefined;
  if (thrd_create(&api->thread, api_thread, api) != thrd_success) {
    cndvar_unlock(&api->state);
    err = errg(err_fail);
    goto failed;
  }
  thread_started = true;

  cndvar_wait_while(&api->state, state_undefined);
  int const state = api->state.var;
  cndvar_unlock(&api->state);

  switch (state) {
  case state_succeeded:
    break;
  case state_failed:
    err = errg(err_fail);
    goto failed;
  default:
    err = errg(err_unexpected);
    goto failed;
  }
  return eok();

failed:
  if (thread_started) {
    thrd_join(api->thread, NULL);
  }
  cndvar_exit(&api->process);
  cndvar_exit(&api->state);
  return err;
}

NODISCARD static error api_thread_exit(struct api *const api) {
  SendMessage(api->window, WM_SYSCOMMAND, SC_CLOSE, 0);
  thrd_join(api->thread, NULL);
  cndvar_exit(&api->process);
  cndvar_exit(&api->state);
  return eok();
}

error api_init(struct api **const api) {
  error err = eok();
  HANDLE mutex = 0, fmo = 0;

  static wchar_t const mutex_name[] = L"GCMZDropsMutex";
  mutex = CreateMutexW(NULL, FALSE, mutex_name);
  // Do not judge whether mutex is NULL.
  // Because if the error is ERROR_ALREADY_EXISTS, the existing handle will be returned.
  HRESULT hr = HRESULT_FROM_WIN32(GetLastError());
  if (FAILED(hr)) {
    err = errhr(hr);
    goto failed;
  }

  static wchar_t const file_mapping_object_name[] = L"GCMZDrops";
  fmo = CreateFileMappingW(
      INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, sizeof(struct gcmzdrops_fmo), file_mapping_object_name);
  // Do not judge whether fmo is NULL.
  // Because if the error is ERROR_ALREADY_EXISTS, the existing handle will be returned.
  hr = HRESULT_FROM_WIN32(GetLastError());
  if (FAILED(hr)) {
    err = errhr(hr);
    goto failed;
  }

  err = mem(api, 1, sizeof(struct api));
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }
  (*api)->mutex = mutex;
  (*api)->fmo = fmo;

  err = api_thread_init(*api);
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }

  return eok();

failed:
  if (*api) {
    ereport(mem_free(api));
  }
  if (fmo) {
    CloseHandle(fmo);
    fmo = NULL;
  }
  if (mutex) {
    CloseHandle(mutex);
    mutex = NULL;
  }
  return err;
}

void api_set_callback(struct api *const api, api_request_func const callback, void *const userdata) {
  api->request = callback;
  api->userdata = userdata;
}

bool api_initialized(struct api const *const api) { return api && api->fmo && api->mutex; }

error api_update_mapped_data(struct api *const api) {
  if (!api_initialized(api)) {
    return errg(err_unexpected);
  }

  error err = eok();
  struct gcmzdrops_fmo d = {
      .window = (uint32_t)api->window,
      .gcmz_api_ver = 2,
  };
  void *p = NULL;
  bool mtx_locked = false;

  {
    FILE_INFO fi = {0};
    err = aviutl_get_editing_file_info(&fi);
    if (efailed(err)) {
      if (!eis(err, err_type_gcmz, err_gcmz_project_is_not_open)) {
        err = ethru(err);
        goto cleanup;
      }
      efree(&err);
    }
    if (fi.audio_rate != 0 && fi.audio_ch != 0) {
      d.width = fi.w;
      d.height = fi.h;
      d.video_rate = fi.video_rate;
      d.video_scale = fi.video_scale;
      d.audio_rate = fi.audio_rate;
      d.audio_ch = fi.audio_ch;
    }

    SYS_INFO si = {0};
    err = aviutl_get_sys_info(&si);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }

    // AviUtl seems to keep project_name even after the file is closed.
    // So we need to make sure that the project information is correct before use that.
    if (fi.audio_rate != 0 && fi.audio_ch != 0 && si.project_name && si.project_name[0] != '\0') {
      struct wstr tmp = {0};
      err = from_mbcs(&str_unmanaged(si.project_name), &tmp);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      wcsncpy(d.project_path, tmp.ptr, min(tmp.len, MAX_PATH - 1));
      ereport(sfree(&tmp));
    }
  }

  {
    uint32_t flags = 0;
    bool enpatched = false;
    err = aviutl_exedit_is_enpatched(&enpatched);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    if (enpatched) {
      flags |= 1;
    }
    d.flags = flags;
  }

  p = MapViewOfFile(api->fmo, FILE_MAP_WRITE, 0, 0, 0);
  if (!p) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  switch (WaitForSingleObject(api->mutex, INFINITE)) {
  case WAIT_OBJECT_0:
    mtx_locked = true;
    break;
  case WAIT_ABANDONED:
    err = errg(err_abort);
    break;
  case WAIT_FAILED:
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    break;
  default:
    err = errg(err_fail);
    break;
  }
  if (efailed(err)) {
    goto cleanup;
  }
  memcpy(p, &d, sizeof(struct gcmzdrops_fmo));
  if (!FlushViewOfFile(p, 0)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  if (!ReleaseMutex(api->mutex)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  mtx_locked = false;
  if (!UnmapViewOfFile(p)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  p = NULL;

cleanup:
  if (mtx_locked) {
    ReleaseMutex(api->mutex);
  }
  if (p) {
    UnmapViewOfFile(p);
  }
  return err;
}

error api_exit(struct api **const api) {
  ereportmsg(api_thread_exit(*api), &native_unmanaged(NSTR("API 用スレッドの終了に失敗しました。")));
  if ((*api)->mutex) {
    CloseHandle((*api)->mutex);
    (*api)->mutex = NULL;
  }
  if ((*api)->fmo) {
    CloseHandle((*api)->fmo);
    (*api)->fmo = NULL;
  }
  error err = mem_free(api);
  *api = NULL;
  return err;
}
