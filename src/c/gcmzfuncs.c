#include "gcmzfuncs.h"

#include <shellapi.h>
#include <shlobj.h>

#include "aviutl.h"
#include "error_gcmz.h"
#include "gcmzdrops.h"
#include "gui.h"
#include "task.h"
#include "util.h"

error gcmz_get_script_dir(struct wstr *const dest) {
  if (!dest) {
    return errg(err_null_pointer);
  }

  struct wstr tmp = {0};
  error err = get_module_file_name(get_hinstance(), &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  int fnpos = 0;
  err = extract_file_name(&tmp, &fnpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp.ptr[fnpos] = L'\0';
  tmp.len = fnpos;

  err = scat(&tmp, L"GCMZDrops");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(dest, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  return err;
}

error gcmz_get_project_dir(struct wstr *const dest) {
  if (!dest) {
    return errg(err_null_pointer);
  }

  struct wstr tmp = {0};
  error err = aviutl_get_project_path(&tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  int pos = 0;
  err = extract_file_name(&tmp, &pos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  tmp.len = pos;
  tmp.ptr[tmp.len] = L'\0';
  err = exclude_trailing_path_delimiter(&tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = scpy(dest, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  return err;
}

error gcmz_get_save_dir(struct wstr *const dest) {
  if (!dest) {
    return errg(err_null_pointer);
  }

  static wchar_t const *const placeholder = L"%PROJECTDIR%";
  struct wstr dir = {0};
  struct wstr proj = {0};
  error err = gui_get_save_dir(&dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  int phpos = 0;
  err = sstr(&dir, placeholder, &phpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  if (phpos != -1) {
    err = gcmz_get_project_dir(&proj);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }

    err = sreplace_all(&dir, placeholder, proj.ptr);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }

  err = exclude_trailing_path_delimiter(&dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = scpy(dest, dir.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&proj));
  ereport(sfree(&dir));
  return err;
}

static inline int to_lower(int const c) {
  if ((c >= 'A') && (c <= 'Z')) {
    return c - 'A' + 'a';
  }
  return c;
}

NODISCARD static error gcmz_is_need_copy_mode_auto(struct wstr const *const path, bool *const need_copy) {
  if (!path) {
    return errg(err_invalid_arugment);
  }
  if (!need_copy) {
    return errg(err_null_pointer);
  }

  struct wstr longpath = {0};
  struct wstr dir = {0};
  error err = get_long_path_name(path, &longpath);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = get_temp_dir(&dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  bool contains = false;
  err = file_contains(&dir, &longpath, &contains);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (contains) {
    *need_copy = true;
    goto cleanup;
  }

  static int const csidls[] = {
      CSIDL_APPDATA,
      CSIDL_LOCAL_APPDATA,
      CSIDL_COMMON_APPDATA,
      CSIDL_COOKIES,
      CSIDL_INTERNET_CACHE,
      CSIDL_PROGRAM_FILES,
      CSIDL_PROGRAM_FILES_COMMON,
      CSIDL_STARTMENU,
      CSIDL_PROGRAMS,
      CSIDL_WINDOWS,
      CSIDL_SYSTEM,
  };

  err = sgrow(&dir, MAX_PATH + 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  size_t const n = sizeof(csidls) / sizeof(csidls[0]);
  for (size_t i = 0; i < n; ++i) {
    HRESULT const hr = SHGetFolderPathW(0, csidls[i], NULL, SHGFP_TYPE_CURRENT, dir.ptr);
    if (FAILED(hr)) {
      err = errhr(hr);
      goto cleanup;
    }
    if (hr == S_FALSE) {
      continue;
    }
    dir.len = wcslen(dir.ptr);
    bool contains = false;
    err = file_contains(&dir, &longpath, &contains);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    if (contains) {
      *need_copy = true;
      goto cleanup;
    }
  }

  *need_copy = false;

cleanup:
  ereport(sfree(&longpath));
  ereport(sfree(&dir));
  return err;
}

NODISCARD static error gcmz_is_need_copy_mode_copy(struct wstr const *const path, bool *const need_copy) {
  if (!path) {
    return errg(err_invalid_arugment);
  }
  if (!need_copy) {
    return errg(err_null_pointer);
  }

  *need_copy = true;
  return eok();
}

NODISCARD static error gcmz_is_need_copy_mode_direct(struct wstr const *const path, bool *const need_copy) {
  if (!path) {
    return errg(err_invalid_arugment);
  }
  if (!need_copy) {
    return errg(err_null_pointer);
  }
  struct wstr longpath = {0};
  struct wstr dir = {0};
  error err = get_long_path_name(path, &longpath);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = get_temp_dir(&dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  bool contains_in_temp = false;
  err = file_contains(&dir, &longpath, &contains_in_temp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *need_copy = contains_in_temp;

cleanup:
  ereport(sfree(&longpath));
  ereport(sfree(&dir));
  return err;
}

error gcmz_is_need_copy(struct wstr const *const path, bool *const need_copy) {
  if (!path) {
    return errg(err_invalid_arugment);
  }
  if (!need_copy) {
    return errg(err_null_pointer);
  }

  struct wstr ext = {0};
  int extpos = 0;
  error err = extract_file_extension(path, &extpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(&ext, path->ptr + extpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  for (size_t i = 0; i < ext.len; ++i) {
    ext.ptr[i] = to_lower(ext.ptr[i]);
  }
  if (wcscmp(ext.ptr, L".txt") == 0 || wcscmp(ext.ptr, L".exo") == 0 || wcscmp(ext.ptr, L".exa") == 0) {
    ereport(sfree(&ext));
    *need_copy = false;
    return eok();
  }
  ereport(sfree(&ext));

  int mode = 0;
  err = gui_get_save_mode(&mode);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  switch (mode) {
  case gui_mode_auto:
    err = gcmz_is_need_copy_mode_auto(path, need_copy);
    break;
  case gui_mode_copy:
    err = gcmz_is_need_copy_mode_copy(path, need_copy);
    break;
  case gui_mode_direct:
    err = gcmz_is_need_copy_mode_direct(path, need_copy);
    break;
  default:
    err = errg(err_unexpected);
    break;
  }
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&ext));
  return err;
}

error gcmz_exists_in_save_dir(struct wstr const *const path, bool *const exists) {
  struct wstr savedir = {0};
  error err = gcmz_get_save_dir(&savedir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  bool contains = false;
  err = file_contains(&savedir, path, &contains);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *exists = contains;

cleanup:
  ereport(sfree(&savedir));
  return err;
}

error gcmz_advance_frame(int const n) {
  int frame = 0;
  int frame_n = 0;
  error err = aviutl_get_frame(&frame);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = aviutl_get_frame_n(&frame_n);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  frame += n;
  if (frame > frame_n) {
    int new_frame_n = frame + 1;
    err = aviutl_set_frame_n(&new_frame_n);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }

    int start = 0, end = 0;
    err = aviutl_get_select_frame(&start, &end);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    if (start != -1 && end != -1) {
      if (end == frame_n - 1) {
        end = new_frame_n - 1;
        err = aviutl_set_select_frame(start, end);
        if (efailed(err)) {
          err = ethru(err);
          goto cleanup;
        }
      }
    }
  }

  err = aviutl_set_frame(&frame);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  return err;
}

NODISCARD static error
create_drop_data(HWND const window, POINT const pt, struct wstr const *const *const filepath, HDROP *const hdrop) {
  if (!filepath) {
    return errg(err_invalid_arugment);
  }
  if (!hdrop) {
    return errg(err_null_pointer);
  }

  HGLOBAL h = NULL;
  DROPFILES *df = NULL;
  error err = eok();
  size_t filepath_bytes = sizeof(wchar_t);
  DWORD files = 0;
  for (size_t i = 0; filepath[i] != NULL; ++i) {
    struct wstr const *const f = filepath[i];
    filepath_bytes += (f->len + 1) * sizeof(wchar_t);
    ++files;
  }
  h = GlobalAlloc(GMEM_ZEROINIT, sizeof(DROPFILES) + filepath_bytes);
  if (!h) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto failed;
  }
  df = GlobalLock(h);
  if (!df) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto failed;
  }
  *df = (DROPFILES){
      .pFiles = sizeof(DROPFILES),
      .fWide = TRUE,
      .fNC = FALSE,
      .pt = pt,
  };
  if (!ScreenToClient(window, &df->pt)) {
    err = errg(err_fail);
    goto failed;
  }
  wchar_t *s = (void *)(df + 1);
  for (size_t i = 0; filepath[i] != NULL; ++i) {
    struct wstr const *const f = filepath[i];
    wcsncpy(s, f->ptr, f->len);
    s += f->len + 1;
  }
  if (!GlobalUnlock(h)) {
    HRESULT hr = HRESULT_FROM_WIN32(GetLastError());
    if (hr != HRESULT_FROM_WIN32(NO_ERROR)) {
      err = errhr(hr);
      goto failed;
    }
  }
  df = NULL;
  s = NULL;
  *hdrop = h;
  return eok();

failed:
  if (!df) {
    GlobalUnlock(h);
    df = NULL;
  }
  if (h) {
    GlobalFree(h);
    h = NULL;
  }
  return err;
}

error gcmz_drop(HWND const window, POINT const pt, struct wstr const *const *const filepath) {
  if (!filepath) {
    return errg(err_invalid_arugment);
  }

  HDROP h = NULL;
  error err = create_drop_data(window, pt, filepath, &h);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }

  SendMessageW(window, WM_DROPFILES, (WPARAM)h, 0);
  return eok();
}

enum {
  gcmz_minimum_window_width = 72,
  gcmz_minimum_window_height = 52,
};

NODISCARD static error analyse_exedit_window_image(uint8_t const *const image,
                                                   size_t const width,
                                                   size_t const height,
                                                   struct gcmz_analysed_info *const ai) {
  if (!image) {
    return errg(err_invalid_arugment);
  }
  if (!ai) {
    return errg(err_null_pointer);
  }
  if (width < gcmz_minimum_window_width || height < gcmz_minimum_window_height) {
    return emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("拡張編集ウィンドウの表示エリアが小さすぎます。")));
  }

  size_t const line_bytes = (width * 3 + 3) & ~3;
  struct gcmz_analysed_info tmp = {
      .edit_cursor = {-1, -1},
      .zoom_level = 0,
      .layer_height = 0,
  };

  {
    enum {
      zoom_left = 5,
      zoom_top = 32,
      zoom_max = 26,
      zoom_active_r24 = 96,
      zoom_active_r16 = 99,
      zoom_deactive_r24 = 32,
      zoom_deactive_r16 = 33,
    };
    uint8_t const *pp = image + zoom_top * line_bytes + zoom_left * 3 + 2;
    for (size_t i = 0; i < zoom_max; ++i) {
      uint8_t r = *pp;
      if (r == zoom_active_r24 || r == zoom_active_r16) {
        ++tmp.zoom_level;
        pp += 2 * 3;
        continue;
      } else if (r == zoom_deactive_r24 || r == zoom_deactive_r16) {
        break;
      }
      return err(err_type_gcmz, err_gcmz_failed_to_detect_zoom_level);
    }
  }

  {
    enum {
      layer_left = 0,
      layer_top = 42,
    };
    uint8_t const *pp = image + layer_top * line_bytes + layer_left * 3;
    uint8_t const tb = pp[0], tg = pp[1], tr = pp[2];
    {
      uint8_t const *bp = pp - line_bytes;
      if (bp[0] == tb && bp[1] == tg && bp[2] == tr) {
        return err(err_type_gcmz, err_gcmz_failed_to_detect_layer_height);
      }
    }
    size_t i = 0;
    while (i < height - layer_top && pp[0] == tb && pp[1] == tg && pp[2] == tr) {
      ++i;
      pp += line_bytes;
    }
    if (i >= height - layer_top) {
      // Failed because it may have overflowed the screen.
      return err(err_type_gcmz, err_gcmz_failed_to_detect_layer_height);
    }
    tmp.layer_height = (int)i + 1;
  }

  {
    enum {
      timeline_left = 64,
      timeline_header_top = 13,
      timeline_top = 48,
      // consider the case where timeline end line(#a0a0a0) and cursor overlap.
      eb24 = 255 - 160,
      eg24 = 255 - 160,
      er24 = 160,
      eb16 = 255 - 165,
      eg16 = 255 - 162,
      er16 = 165,
    };
    uint8_t const *pp = image + timeline_header_top * line_bytes + timeline_left * 3;
    uint8_t const tb = 255 - pp[-3], tg = 255 - pp[-2], tr = pp[-1];
    size_t const w = width - timeline_left;
    for (size_t i = 0; i < w; ++i) {
      uint8_t const b = pp[0], g = pp[1], r = pp[2];
      if ((b == tb && g == tg && r == tr) || (b == eb24 && g == eg24 && r == er24) ||
          (b == eb16 && g == eg16 && r == er16)) {
        tmp.edit_cursor = (POINT){timeline_left + i, timeline_top};
        break;
      }
      pp += 3;
    }
  }

  *ai = tmp;
  return eok();
}

NODISCARD static error save_dib(uint8_t const *const image, size_t const width, size_t const height) {
  if (!image || !width || !height) {
    return errg(err_invalid_arugment);
  }

  struct wstr bmpname = {0};
  error err = get_module_file_name(NULL, &bmpname);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  int fnpos = 0;
  err = extract_file_name(&bmpname, &fnpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  bmpname.ptr[fnpos] = L'\0';
  bmpname.len = fnpos;

  SYSTEMTIME st = {0};
  GetLocalTime(&st);

  wchar_t filename[32] = {0};
  wsprintfW(filename,
            L"gcmz-apierr-%04d%02d%02d-%02d%02d%02d.bmp",
            st.wYear,
            st.wMonth,
            st.wDay,
            st.wHour,
            st.wMinute,
            st.wSecond);
  err = scat(&bmpname, filename);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  HANDLE h = CreateFileW(bmpname.ptr, GENERIC_READ | GENERIC_WRITE, 0, NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, NULL);
  if (h == INVALID_HANDLE_VALUE) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  DWORD written = 0;
  BITMAPFILEHEADER bfh = {
      .bfType = 0x4d42,
      .bfOffBits = sizeof(BITMAPFILEHEADER) + sizeof(BITMAPINFOHEADER),
  };
  BITMAPINFOHEADER bih = {
      .biSize = sizeof(BITMAPINFOHEADER),
      .biWidth = width,
      .biHeight = -height,
      .biPlanes = 1,
      .biBitCount = 24,
      .biCompression = BI_RGB,
  };

  if (!WriteFile(h, &bfh, sizeof(BITMAPFILEHEADER), &written, NULL)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    CloseHandle(h);
    goto cleanup;
  }
  if (written != sizeof(BITMAPFILEHEADER)) {
    err = errg(err_fail);
    CloseHandle(h);
    goto cleanup;
  }
  if (!WriteFile(h, &bih, sizeof(BITMAPINFOHEADER), &written, NULL)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    CloseHandle(h);
    goto cleanup;
  }
  if (written != sizeof(BITMAPINFOHEADER)) {
    err = errg(err_fail);
    CloseHandle(h);
    goto cleanup;
  }
  DWORD const len = ((width * 3 + 3) & ~3) * height;
  if (!WriteFile(h, image, len, &written, NULL)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    CloseHandle(h);
    goto cleanup;
  }
  if (written != len) {
    err = errg(err_fail);
    CloseHandle(h);
    goto cleanup;
  }
  CloseHandle(h);

cleanup:
  ereport(sfree(&bmpname));
  return err;
}

static bool can_use_print_window(void) {
  HMODULE const h = GetModuleHandleW(L"ntdll.dll");
  if (!h) {
    return false;
  }
  if (!GetProcAddress(h, "wine_get_version")) {
    return false; // WINE
  }
  typedef INT(WINAPI * RtlGetVersionFunc)(OSVERSIONINFOEXW *);
  RtlGetVersionFunc fn = (RtlGetVersionFunc)GetProcAddress(h, "RtlGetVersion");
  if (!fn) {
    return false;
  }
  OSVERSIONINFOEXW ver = {0};
  fn(&ver);
  if (ver.dwMajorVersion <= 5) {
    return false; // winXP or earlier
  }
  return true;
}

error gcmz_analyse_exedit_window(struct gcmz_analysed_info *const ai) {
  if (!ai) {
    return errg(err_null_pointer);
  }

  HWND exedit_window = NULL;
  RECT exedit_client_rect = {0};
  HDC exedit_dc = NULL;

  HWND desktop_window = NULL;
  HDC desktop_dc = NULL;

  HDC dc = NULL;
  HBITMAP bmp = NULL;
  HBITMAP old_bmp = NULL;

  error err = aviutl_get_exedit_window(&exedit_window);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }

  if (!GetClientRect(exedit_window, &exedit_client_rect)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }

  exedit_dc = GetDC(exedit_window);
  if (exedit_dc == NULL) {
    err = errg(err_fail);
    goto cleanup;
  }

  desktop_window = GetDesktopWindow();
  desktop_dc = GetDC(desktop_window);
  if (desktop_dc == NULL) {
    err = errg(err_fail);
    goto cleanup;
  }

  dc = CreateCompatibleDC(desktop_dc);
  if (dc == NULL) {
    err = errg(err_fail);
    goto cleanup;
  }

  size_t width = exedit_client_rect.right - exedit_client_rect.left;
  size_t height = exedit_client_rect.bottom - exedit_client_rect.top;

  // Set a cap to avoid consuming too much memory.
  // But there is no basis for this setting value.
  // If the layer height is abnormally large, detection will fail.
  if (height > 160) {
    height = 160;
  }

  uint8_t *p = NULL;
  bmp = CreateDIBSection(dc,
                         &(BITMAPINFO){
                             .bmiHeader =
                                 {
                                     .biSize = sizeof(BITMAPINFOHEADER),
                                     .biWidth = width,
                                     .biHeight = -height,
                                     .biPlanes = 1,
                                     .biBitCount = 24,
                                     .biCompression = BI_RGB,
                                 },
                         },
                         DIB_RGB_COLORS,
                         (void **)&p,
                         NULL,
                         0);
  if (bmp == NULL) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  old_bmp = SelectObject(dc, bmp);
  if (old_bmp == NULL) {
    err = errg(err_fail);
    goto cleanup;
  }
  // We want to use PrintWindow to retrieve the contents of a hidden window.
  // However, this API does not work as expected on WinXP and WINE.
  if (can_use_print_window()) {
    if (!PrintWindow(exedit_window, dc, PW_CLIENTONLY)) {
      err = errg(err_fail);
      goto cleanup;
    }
  } else {
    if (!BitBlt(dc, 0, 0, width, height, exedit_dc, 0, 0, SRCCOPY)) {
      err = errg(err_fail);
      goto cleanup;
    }
  }

  err = analyse_exedit_window_image(p, width, height, ai);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  if (efailed(err) && p != NULL) {
    ereport(save_dib(p, width, height));
  }
  if (old_bmp != NULL) {
    SelectObject(dc, old_bmp);
    old_bmp = NULL;
  }
  if (bmp != NULL) {
    DeleteObject(bmp);
    bmp = NULL;
  }
  if (dc != NULL) {
    DeleteDC(dc);
    dc = NULL;
  }
  if (desktop_dc != NULL) {
    ReleaseDC(desktop_window, desktop_dc);
    desktop_dc = NULL;
  }
  if (exedit_dc != NULL) {
    ReleaseDC(exedit_window, exedit_dc);
    exedit_dc = NULL;
  }
  return err;
}

error gcmz_set_zoom_level(int const zoom) {
  enum {
    zoom_left = 3,
    zoom_top = 32,
    zoom_min = 0,
    zoom_max = 26,
  };
  if (zoom < zoom_min || zoom > zoom_max) {
    return errg(err_invalid_arugment);
  }

  HWND exedit_window = NULL;
  error err = aviutl_get_exedit_window(&exedit_window);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  LPARAM const lp = MAKELONG(zoom_left + zoom * 2, zoom_top);
  SendMessage(exedit_window, WM_LBUTTONDOWN, MK_LBUTTON, lp);
  SendMessage(exedit_window, WM_LBUTTONUP, MK_LBUTTON, lp);

cleanup:
  return err;
}

static wchar_t const *const input_dialog_prop = L"input_dialog";
struct input_dialog {
  struct wstr const *caption;
  struct wstr *value;
  error err;
};

static INT_PTR CALLBACK input_dialog_wndproc(HWND const dlg,
                                             UINT const message,
                                             WPARAM const wparam,
                                             LPARAM const lparam) {
  switch (message) {
  case WM_INITDIALOG: {
    struct input_dialog *const ib = (void *)lparam;
    SetPropW(dlg, input_dialog_prop, (HANDLE)ib);

    SYS_INFO si = {0};
    error err = aviutl_get_sys_info(&si);
    if (efailed(err)) {
      ib->err = err;
      EndDialog(dlg, 0);
      return TRUE;
    }
    SendMessageW(GetDlgItem(dlg, 0), WM_SETFONT, (WPARAM)si.hfont, 0);
    SendMessageW(GetDlgItem(dlg, 1), WM_SETFONT, (WPARAM)si.hfont, 0);
    SendMessageW(GetDlgItem(dlg, 2), WM_SETFONT, (WPARAM)si.hfont, 0);
    SendMessageW(GetDlgItem(dlg, 3), WM_SETFONT, (WPARAM)si.hfont, 0);
    SetWindowTextW(dlg, GCMZDROPS_NAME_VERSION_WIDE);
    SetWindowTextW(GetDlgItem(dlg, 0), ib->value->ptr);
    SetWindowTextW(GetDlgItem(dlg, 3), ib->caption->ptr);
    return TRUE;
  }
  case WM_DESTROY:
    RemovePropW(dlg, input_dialog_prop);
    return 0;
  case WM_COMMAND:
    switch (LOWORD(wparam)) {
    case IDOK: {
      struct input_dialog *const ib = (void *)GetPropW(dlg, input_dialog_prop);
      if (ib) {
        error err = get_window_text(GetDlgItem(dlg, 0), ib->value);
        if (efailed(err)) {
          ib->err = err;
          EndDialog(dlg, 0);
          return TRUE;
        }
      }
      EndDialog(dlg, IDOK);
      return TRUE;
    }
    case IDCANCEL:
      EndDialog(dlg, IDCANCEL);
      return TRUE;
    }
    break;
  }
  return FALSE;
}

error gcmz_prompt(struct wstr const *const caption, struct wstr *const value, bool *const result) {
  if (!caption) {
    return errg(err_invalid_arugment);
  }
  if (!value || !result) {
    return errg(err_null_pointer);
  }

  HWND h = NULL;
  error err = aviutl_get_exedit_window(&h);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }

  struct input_dialog ib = {
      .caption = caption,
      .value = value,
      .err = eok(),
  };
  INT_PTR r = DialogBoxParamW(get_hinstance(), L"INPUTDIALOG", h, input_dialog_wndproc, (LPARAM)&ib);
  if (r == 0 || r == -1) {
    if (efailed(ib.err)) {
      return ib.err;
    }
    return errhr(HRESULT_FROM_WIN32(GetLastError()));
  }
  *result = r == IDOK;
  return eok();
}

error gcmz_confirm(struct wstr const *const caption, bool *const result) {
  if (!caption) {
    return errg(err_invalid_arugment);
  }
  if (!result) {
    return errg(err_null_pointer);
  }

  HWND h = NULL;
  error err = aviutl_get_exedit_window(&h);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  int r = MessageBoxW(h, caption->ptr, GCMZDROPS_NAME_VERSION_WIDE, MB_ICONQUESTION | MB_OKCANCEL);
  if (!r) {
    return errhr(HRESULT_FROM_WIN32(GetLastError()));
  }
  *result = r == IDOK;
  return eok();
}
