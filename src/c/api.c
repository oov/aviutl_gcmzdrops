#include "api.h"

#include <jansson.h>

#include "aviutl.h"
#include "error_gcmz.h"
#include "error_win32.h"
#include "task.h"
#include "util.h"

struct gcmzdrops_fmo
{
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

enum
{
  state_undefined = 0,
  state_succeeded = 1,
  state_failed = 2,
};

static void cv_init(struct cv *const cv)
{
  mtx_init(&cv->mtx, mtx_plain | mtx_recursive);
  cnd_init(&cv->cnd);
  cv->var = 0;
}

static void cv_exit(struct cv *const cv)
{
  cnd_destroy(&cv->cnd);
  mtx_destroy(&cv->mtx);
}

static void cv_lock(struct cv *const cv)
{
  mtx_lock(&cv->mtx);
}

static void cv_unlock(struct cv *const cv)
{
  mtx_unlock(&cv->mtx);
}

static void cv_signal(struct cv *const cv, int const var)
{
  cv->var = var;
  cnd_signal(&cv->cnd);
}

static int cv_wait_while(struct cv *cv, int var)
{
  assert(cv != NULL);
  while (cv->var == var)
  {
    cnd_wait(&cv->cnd, &cv->mtx);
  }
  return cv->var;
}

NODISCARD static error parse_api0_found_token(struct wstr *const ws, struct api_request_params *const params, size_t *const found)
{
  error err = eok();
  int64_t v = 0;
  switch (*found)
  {
  case 0:
    err = atoi64(ws, &v);
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
    params->layer = v;
    break;
  case 1:
    err = atoi64(ws, &v);
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
    params->frame_advance = v;
    break;
  default:
    err = files_add(&params->files, ws, &wstr_unmanaged(L"application/octet-stream"));
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
  }
  ++*found;

cleanup:
  return err;
}

NODISCARD static error parse_api0(struct wstr *const ws, struct api_request_params *const params)
{
  struct wstr tmp = {0};

  // ws in not null-terminated and may source is unmanaged buffer so we need copy to modify
  error err = sgrow(&tmp, ws->len + 1);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }
  memcpy(tmp.ptr, ws->ptr, ws->len * sizeof(wchar_t));
  tmp.len = ws->len;
  tmp.ptr[tmp.len] = L'\0';

  wchar_t const *p = tmp.ptr;
  size_t found = 0, token = 0;
  size_t const len = tmp.len;
  for (size_t i = 0; i < len; ++i)
  {
    if (p[i] != L'\0')
    {
      ++token;
      continue;
    }
    if (token == 0)
    {
      err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("無効なフォーマットです。")));
      goto cleanup;
    }
    err = parse_api0_found_token(&(struct wstr){.ptr = (wchar_t *)p + i - token, .len = token}, params, &found);
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
    token = 0;
  }
  if (token)
  {
    err = parse_api0_found_token(&(struct wstr){.ptr = (wchar_t *)p + len - token, .len = token}, params, &found);
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
  }
cleanup:
  ereport(sfree(&tmp));
  return err;
}

NODISCARD static error parse_api1(struct str *const s, struct api_request_params *const params)
{
  error err = eok();
  int added = 0;
  struct wstr tmp = {0};
  json_t *root = json_loadb(s->ptr, s->len, 0, NULL);
  if (!json_is_object(root))
  {
    err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("JSON としての読み込みに失敗しました。")));
    goto cleanup;
  }

  int64_t layer = 0;
  int64_t frame_advance = 0;

  json_t *v = json_object_get(root, "layer");
  if (!json_is_number(v))
  {
    err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("layer の指定が正しくありません。")));
    goto cleanup;
  }
  layer = json_integer_value(v);

  v = json_object_get(root, "frameAdvance");
  if (json_is_number(v))
  {
    frame_advance = json_integer_value(v);
  }

  v = json_object_get(root, "files");
  if (!json_is_array(v))
  {
    err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("files の指定が正しくありません。")));
    goto cleanup;
  }
  for (size_t i = 0, len = json_array_size(v); i < len; ++i)
  {
    json_t *item = json_array_get(v, i);
    if (!json_is_string(item))
    {
      err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("ファイル名が正しくありません。")));
      goto cleanup;
    }
    err = from_utf8(&str_unmanaged(json_string_value(item)), &tmp);
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
    err = files_add(&params->files, &tmp, &wstr_unmanaged(L"application/octet-stream"));
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
    ++added;
  }

  params->layer = layer;
  params->frame_advance = frame_advance;

cleanup:
  if (efailed(err))
  {
    for (int i = 0; i < added; ++i)
    {
      ereport(files_delete_last(&params->files, false));
    }
  }
  ereport(sfree(&tmp));
  if (root)
  {
    json_decref(root);
  }
  return err;
}

struct process_task_data
{
  struct api *api;
  struct api_request_params params;
  error err;
};

static void process_complete(struct api_request_params *const params)
{
  cv_lock(params->priv.process);
  cv_signal(params->priv.process, state_succeeded);
  cv_unlock(params->priv.process);
}

static void process_task(void *const userdata)
{
  struct process_task_data *const d = userdata;
  struct api *const api = d->api;
  if (api->request)
  {
    api->request(api->userdata, &d->params, d->err);
  }
  else
  {
    efree(&d->err);
  }
}

static BOOL process(struct api *const api, HWND const sender, COPYDATASTRUCT *const cds)
{
  (void)sender;
  struct process_task_data d = {
      .api = api,
  };
  error err = eok();
  switch (cds->dwData)
  {
  case 0:
    err = parse_api0(
        &(struct wstr){
            .ptr = cds->lpData,
            .len = cds->cbData / 2,
        },
        &d.params);
    break;
  case 1:
    err = parse_api1(
        &(struct str){
            .ptr = cds->lpData,
            .len = cds->cbData,
        },
        &d.params);
    break;
  default:
    err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("未知の API バージョンです。")));
    break;
  }
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }

  if (d.params.layer == 0 || d.params.layer < -100 || d.params.layer > 100)
  {
    err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("レイヤー番号が不正です。")));
    goto cleanup;
  }
  cv_lock(&api->priv.process);
  d.params.priv.process = &api->priv.process;
  d.params.complete = process_complete;
  api->priv.process.var = state_undefined;
  task_add(process_task, &d);
  cv_wait_while(&api->priv.process, state_undefined);
  cv_unlock(&api->priv.process);

cleanup:
  ereport(files_free(&d.params.files, false));
  if (efailed(err))
  {
    d.err = err;
    err = NULL; // err is owned by request handler
    cv_lock(&api->priv.process);
    d.params.priv.process = &api->priv.process;
    d.params.complete = process_complete;
    api->priv.process.var = state_undefined;
    task_add(process_task, &d);
    cv_wait_while(&api->priv.process, state_undefined);
    cv_unlock(&api->priv.process);
    return FALSE;
  }
  return TRUE;
}

static LRESULT WINAPI wndproc(HWND const window, UINT const message, WPARAM const wparam, LPARAM const lparam)
{
  switch (message)
  {
  case WM_COPYDATA:
    return process((struct api *)GetWindowLongPtrW(window, 0), (HWND)wparam, (COPYDATASTRUCT *)lparam);
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

static int api_thread(void *const userdata)
{
  struct api *const api = userdata;
  error err = eok();
  {
    static wchar_t const *const window_class_name = L"GCMZDropsAPI";
    HINSTANCE hinst = get_hinstance();
    WNDCLASSEXW wc = {0};
    wc.cbSize = sizeof(WNDCLASSEXW);
    wc.lpfnWndProc = wndproc;
    wc.hInstance = hinst;
    wc.lpszClassName = window_class_name;
    wc.cbWndExtra = sizeof(struct api *);
    if (RegisterClassExW(&wc) == 0)
    {
      err = err_hr(HRESULT_FROM_WIN32(GetLastError()));
      goto failed;
    }
    HWND h = CreateWindowExW(
        0,
        window_class_name,
        NULL,
        WS_OVERLAPPEDWINDOW,
        CW_USEDEFAULT, CW_USEDEFAULT,
        CW_USEDEFAULT, CW_USEDEFAULT,
        HWND_MESSAGE,
        0,
        hinst,
        NULL);
    if (!h)
    {
      err = err_hr(HRESULT_FROM_WIN32(GetLastError()));
      goto failed;
    }
    api->priv.window = h;
    SetWindowLongPtrW(h, 0, (LONG)api);
  }

  cv_lock(&api->priv.state);
  cv_signal(&api->priv.state, state_succeeded);
  cv_unlock(&api->priv.state);

  MSG msg = {0};
  while (GetMessageW(&msg, NULL, 0, 0) > 0)
  {
    TranslateMessage(&msg);
    DispatchMessageW(&msg);
  }
  return 0;

failed:
  ereportmsg(err, &native_unmanaged(NSTR("API 用スレッドでエラーが発生しました。")));
  cv_lock(&api->priv.state);
  cv_signal(&api->priv.state, state_failed);
  cv_unlock(&api->priv.state);
  return 1;
}

NODISCARD static error api_thread_init(struct api *const api)
{
  error err = eok();
  bool thread_started = false;

  cv_init(&api->priv.state);
  cv_init(&api->priv.process);

  cv_lock(&api->priv.state);
  api->priv.state.var = state_undefined;
  if (thrd_create(&api->priv.thread, api_thread, api) != thrd_success)
  {
    cv_unlock(&api->priv.state);
    err = errg(err_fail);
    goto failed;
  }
  thread_started = true;

  int state = cv_wait_while(&api->priv.state, state_undefined);
  cv_unlock(&api->priv.state);

  switch (state)
  {
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
  if (thread_started)
  {
    thrd_join(&api->priv.thread, NULL);
  }
  cv_exit(&api->priv.process);
  cv_exit(&api->priv.state);
  return err;
}

NODISCARD static error api_thread_exit(struct api *const api)
{
  SendMessage(api->priv.window, WM_SYSCOMMAND, SC_CLOSE, 0);
  thrd_join(&api->priv.thread, NULL);
  cv_exit(&api->priv.process);
  cv_exit(&api->priv.state);
  return eok();
}

error api_init(struct api *const api)
{
  error err = eok();
  HANDLE mutex = 0, fmo = 0;

  static wchar_t const *const mutex_name = L"GCMZDropsMutex";
  mutex = CreateMutexW(NULL, FALSE, mutex_name);
  // Do not judge whether mutex is NULL.
  // Because if the error is ERROR_ALREADY_EXISTS, the existing handle will be returned.
  HRESULT hr = HRESULT_FROM_WIN32(GetLastError());
  if (FAILED(hr))
  {
    err = err_hr(hr);
    goto failed;
  }

  static wchar_t const *const file_mapping_object_name = L"GCMZDrops";
  fmo = CreateFileMappingW(INVALID_HANDLE_VALUE, NULL, PAGE_READWRITE, 0, sizeof(struct gcmzdrops_fmo), file_mapping_object_name);
  // Do not judge whether fmo is NULL.
  // Because if the error is ERROR_ALREADY_EXISTS, the existing handle will be returned.
  hr = HRESULT_FROM_WIN32(GetLastError());
  if (FAILED(hr))
  {
    err = err_hr(hr);
    goto failed;
  }

  err = api_thread_init(api);
  if (efailed(err))
  {
    err = ethru(err);
    goto failed;
  }
  api->priv.mutex = mutex;
  api->priv.fmo = fmo;
  return eok();

failed:
  if (fmo)
  {
    CloseHandle(fmo);
    fmo = NULL;
  }
  if (mutex)
  {
    CloseHandle(mutex);
    mutex = NULL;
  }
  return err;
}

bool api_initialized(struct api const *const api)
{
  return api->priv.fmo && api->priv.mutex;
}

error api_update_mapped_data(struct api *const api)
{
  if (!api_initialized(api))
  {
    return errg(err_unexpected);
  }

  error err = eok();
  struct gcmzdrops_fmo d = {
      .window = (uint32_t)api->priv.window,
      .gcmz_api_ver = 2,
  };
  void *p = NULL;
  bool mtx_locked = false;

  {
    FILE_INFO fi = {0};
    err = aviutl_get_editing_file_info(&fi);
    if (efailed(err))
    {
      if (!eis(err, err_type_gcmz, err_gcmz_project_is_not_open))
      {
        err = ethru(err);
        goto cleanup;
      }
      efree(&err);
    }
    if (fi.audio_rate != 0 && fi.audio_ch != 0)
    {
      d.width = fi.w;
      d.height = fi.h;
      d.video_rate = fi.video_rate;
      d.video_scale = fi.video_scale;
      d.audio_rate = fi.audio_rate;
      d.audio_ch = fi.audio_ch;
    }

    SYS_INFO si = {0};
    err = aviutl_get_sys_info(&si);
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }

    // AviUtl seems to keep project_name even after the file is closed.
    // So we need to make sure that the project information is correct before use that.
    if (fi.audio_rate != 0 && fi.audio_ch != 0 && si.project_name && si.project_name[0] != '\0')
    {
      struct wstr tmp = {0};
      err = from_mbcs(&str_unmanaged(si.project_name), &tmp);
      if (efailed(err))
      {
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
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }
    if (enpatched)
    {
      flags |= 1;
    }
    d.flags = flags;
  }

  p = MapViewOfFile(api->priv.fmo, FILE_MAP_WRITE, 0, 0, 0);
  if (!p)
  {
    err = err_hr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  switch (WaitForSingleObject(api->priv.mutex, INFINITE))
  {
  case WAIT_OBJECT_0:
    mtx_locked = true;
    break;
  case WAIT_ABANDONED:
    err = errg(err_abort);
    break;
  case WAIT_FAILED:
    err = err_hr(HRESULT_FROM_WIN32(GetLastError()));
    break;
  default:
    err = errg(err_fail);
    break;
  }
  if (efailed(err))
  {
    goto cleanup;
  }
  memcpy(p, &d, sizeof(struct gcmzdrops_fmo));
  if (!FlushViewOfFile(p, 0))
  {
    err = err_hr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  if (!ReleaseMutex(api->priv.mutex))
  {
    err = err_hr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  mtx_locked = false;
  if (!UnmapViewOfFile(p))
  {
    err = err_hr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  p = NULL;

cleanup:
  if (mtx_locked)
  {
    ReleaseMutex(api->priv.mutex);
  }
  if (p)
  {
    UnmapViewOfFile(p);
  }
  return err;
}

error api_exit(struct api *const api)
{
  ereportmsg(api_thread_exit(api), &native_unmanaged(NSTR("API 用スレッドの終了に失敗しました。")));
  if (api->priv.mutex)
  {
    CloseHandle(api->priv.mutex);
    api->priv.mutex = NULL;
  }
  if (api->priv.fmo)
  {
    CloseHandle(api->priv.fmo);
    api->priv.fmo = NULL;
  }
  return eok();
}
