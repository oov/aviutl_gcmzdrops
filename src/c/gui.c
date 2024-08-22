#include "gui.h"

#include "ovutil/str.h"
#include "ovutil/win32.h"

#include <shlobj.h>

#include "aviutl.h"
#include "gcmzdrops.h"
#include "gcmzfuncs.h"
#include "i18n.h"

static HWND g_save_mode_label = NULL;
static HWND g_save_mode = NULL;
static HWND g_save_dir_label = NULL;
static HWND g_save_dir = NULL;
static HWND g_save_dir_choose_folder = NULL;

static HWND g_restore_initial = NULL;
static HWND g_restore_default = NULL;
static HWND g_save_as_default = NULL;

static int const g_initial_save_mode = 0;
static wchar_t const g_initial_save_dir[] = L"%PROJECTDIR%";

static int g_default_save_mode = 0;
static struct wstr g_default_save_dir = {0};

NODISCARD static error create_font(HWND const window, HFONT *const font, int *const font_height) {
  if (!window) {
    return errg(err_invalid_arugment);
  }
  if (!font || !font_height) {
    return errg(err_null_pointer);
  }

  SYS_INFO si = {0};
  error err = aviutl_get_sys_info(&si);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  NONCLIENTMETRICSW ncm = {
      .cbSize = sizeof(NONCLIENTMETRICSW),
  };
  if (!SystemParametersInfoW(SPI_GETNONCLIENTMETRICS, sizeof(NONCLIENTMETRICSW), &ncm, 0)) {
    return errhr(HRESULT_FROM_WIN32(GetLastError()));
  }

  HDC dc = GetDC(window);
  if (dc) {
    *font_height = -MulDiv(ncm.lfMessageFont.lfHeight, GetDeviceCaps(dc, LOGPIXELSY), 72);
    ReleaseDC(window, dc);
  } else {
    *font_height = 18;
  }
  *font = si.hfont;
  return eok();
}

static NODISCARD error load_defaults(void) {
  struct str tmp = {0};
  error err = aviutl_ini_load_int(&str_unmanaged_const("save_mode"), g_initial_save_mode, &g_default_save_mode);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = to_mbcs(&wstr_unmanaged_const(g_initial_save_dir), &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = aviutl_ini_load_str(&str_unmanaged_const("save_dir"), &tmp, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = from_mbcs(&tmp, &g_default_save_dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&tmp));
  return err;
}

static NODISCARD error save_defaults(void) {
  struct str tmp = {0};
  error err = aviutl_ini_save_int(&str_unmanaged_const("save_mode"), g_default_save_mode);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = to_mbcs(&g_default_save_dir, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = aviutl_ini_save_str(&str_unmanaged_const("save_dir"), &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&tmp));
  return err;
}

error gui_init(HWND const window) {
  HFONT font = NULL;
  HINSTANCE const hinst = get_hinstance();
  int font_height = 0;
  enum {
    buf_size = 1024,
  };
  wchar_t buf[buf_size];
  error err = create_font(window, &font, &font_height);
  if (efailed(err)) {
    ereportmsg_i18n(err, gettext("Failed to create the font."));
    font_height = 18;
  }

  enum {
    padding = 8,
    client_width = 460,
    save_mode_width = 128,
    save_dir_width = client_width - save_mode_width - padding,
  };
  int const label_height = font_height;
  int const control_height = font_height + GetSystemMetrics(SM_CYFIXEDFRAME) * 2;

  int save_mode_height = padding;
  {
    mo_snprintf_wchar(buf, buf_size, NULL, "%1$s", gettext("Processing mode:"));
    HWND h = CreateWindowExW(0,
                             L"STATIC",
                             buf,
                             WS_CHILD | WS_VISIBLE | ES_LEFT,
                             padding,
                             save_mode_height,
                             save_mode_width,
                             label_height,
                             window,
                             NULL,
                             hinst,
                             NULL);
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    save_mode_height += label_height;
    g_save_mode_label = h;

    h = CreateWindowExW(0,
                        L"COMBOBOX",
                        NULL,
                        WS_CHILD | WS_TABSTOP | WS_VISIBLE | CBS_DROPDOWNLIST,
                        padding,
                        save_mode_height,
                        save_mode_width,
                        control_height + 300,
                        window,
                        (HMENU)1,
                        hinst,
                        NULL);
    mo_snprintf_wchar(buf, buf_size, NULL, "%1$s", gettext("Auto detect"));
    SendMessageW(h, CB_ADDSTRING, 0, (LPARAM)buf);
    mo_snprintf_wchar(buf, buf_size, NULL, "%1$s", gettext("Copy"));
    SendMessageW(h, CB_ADDSTRING, 0, (LPARAM)buf);
    mo_snprintf_wchar(buf, buf_size, NULL, "%1$s", gettext("Direct read"));
    SendMessageW(h, CB_ADDSTRING, 0, (LPARAM)buf);
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    save_mode_height += control_height;
    g_save_mode = h;
  }

  int save_dir_height = padding;
  {
    int const button_width = font_height * 2;
    mo_snprintf_wchar(buf, buf_size, NULL, "%1$s", gettext("Save to:"));
    HWND h = CreateWindowExW(0,
                             L"STATIC",
                             buf,
                             WS_CHILD | WS_VISIBLE | ES_LEFT,
                             padding + save_mode_width + padding,
                             save_dir_height,
                             save_dir_width,
                             label_height,
                             window,
                             NULL,
                             hinst,
                             NULL);
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    save_dir_height += label_height;
    g_save_dir_label = h;

    h = CreateWindowExW(WS_EX_CLIENTEDGE,
                        L"EDIT",
                        L"",
                        WS_CHILD | WS_TABSTOP | WS_VISIBLE | ES_AUTOHSCROLL | ES_LEFT,
                        padding + save_mode_width + padding,
                        save_dir_height,
                        save_dir_width - button_width,
                        control_height,
                        window,
                        (HMENU)2,
                        hinst,
                        NULL);
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    g_save_dir = h;
    h = CreateWindowExW(WS_EX_STATICEDGE,
                        L"BUTTON",
                        L"...",
                        WS_CHILD | WS_TABSTOP | WS_VISIBLE | BS_CENTER | BS_VCENTER,
                        padding + save_mode_width + padding + save_dir_width - button_width,
                        save_dir_height,
                        button_width,
                        control_height,
                        window,
                        (HMENU)3,
                        hinst,
                        NULL);
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    save_dir_height += control_height;
    g_save_dir_choose_folder = h;
  }

  int y = (save_mode_height > save_dir_height ? save_mode_height : save_dir_height) + padding;
  {
    enum {
      button_width = 128,
    };
    mo_snprintf_wchar(buf, buf_size, NULL, "%1$s", gettext("Revert to initial"));
    HWND h = CreateWindowExW(0,
                             L"BUTTON",
                             buf,
                             WS_CHILD | WS_TABSTOP | WS_VISIBLE | BS_CENTER | BS_VCENTER,
                             padding,
                             y,
                             button_width,
                             control_height,
                             window,
                             (HMENU)4,
                             hinst,
                             NULL);
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    g_restore_initial = h;
    mo_snprintf_wchar(buf, buf_size, NULL, "%1$s", gettext("Revert to default"));
    h = CreateWindowExW(0,
                        L"BUTTON",
                        buf,
                        WS_CHILD | WS_TABSTOP | WS_VISIBLE | BS_CENTER | BS_VCENTER,
                        padding + button_width,
                        y,
                        button_width,
                        control_height,
                        window,
                        (HMENU)5,
                        hinst,
                        NULL);
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    g_restore_default = h;
    mo_snprintf_wchar(buf, buf_size, NULL, "%1$s", gettext("Save as default"));
    h = CreateWindowExW(0,
                        L"BUTTON",
                        buf,
                        WS_CHILD | WS_TABSTOP | WS_VISIBLE | BS_CENTER | BS_VCENTER,
                        padding + button_width + button_width,
                        y,
                        client_width - button_width * 2,
                        control_height,
                        window,
                        (HMENU)6,
                        hinst,
                        NULL);
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    g_save_as_default = h;
    y += control_height;
  }

  ereport(set_client_size(window, padding + client_width + padding, y + padding));

  g_default_save_mode = g_initial_save_mode;
  ereport(scpy(&g_default_save_dir, g_initial_save_dir));
  ereport(load_defaults());

  return eok();
}

void gui_exit(void) {
  ereport(save_defaults());
  g_default_save_mode = g_initial_save_mode;
  ereport(sfree(&g_default_save_dir));

  SendMessageW(g_save_mode_label, WM_SETFONT, 0, 0);
  SendMessageW(g_save_mode, WM_SETFONT, 0, 0);
  SendMessageW(g_save_dir_label, WM_SETFONT, 0, 0);
  SendMessageW(g_save_dir, WM_SETFONT, 0, 0);
  SendMessageW(g_save_dir_choose_folder, WM_SETFONT, 0, 0);
  SendMessageW(g_restore_initial, WM_SETFONT, 0, 0);
  SendMessageW(g_restore_default, WM_SETFONT, 0, 0);
  SendMessageW(g_save_as_default, WM_SETFONT, 0, 0);
}

void gui_lock(void) {
  EnableWindow(g_save_mode_label, FALSE);
  EnableWindow(g_save_mode, FALSE);
  EnableWindow(g_save_dir_label, FALSE);
  EnableWindow(g_save_dir, FALSE);
  EnableWindow(g_save_dir_choose_folder, FALSE);
  EnableWindow(g_restore_initial, FALSE);
  EnableWindow(g_restore_default, FALSE);
  EnableWindow(g_save_as_default, FALSE);
}

static int CALLBACK select_directory_callback(HWND const window,
                                              UINT const message,
                                              LPARAM const lparam,
                                              LPARAM const lpdata) {
  (void)lparam;
  if (message == BFFM_INITIALIZED && lpdata) {
    SendMessageW(window, BFFM_SETSELECTIONW, 0, lpdata);
  }
  return 0;
}

NODISCARD static error
select_directory(HWND const parent, struct wstr *const caption, struct wstr *const dir, bool *const result) {
  error err = eok();
  ITEMIDLIST *idl = NULL, *selected = NULL;
  IShellFolder *desktop_folder = NULL;
  HRESULT hr = SHGetDesktopFolder(&desktop_folder);
  if (FAILED(hr)) {
    err = errhr(hr);
    goto cleanup;
  }
  hr = desktop_folder->lpVtbl->ParseDisplayName(desktop_folder, NULL, NULL, dir->ptr, NULL, &idl, NULL);
  BROWSEINFOW bi = {
      .hwndOwner = parent,
      .lpfn = select_directory_callback,
      .lParam = (LPARAM)idl,
      .lpszTitle = caption->ptr,
      .ulFlags = BIF_RETURNONLYFSDIRS | BIF_NEWDIALOGSTYLE | BIF_VALIDATE | BIF_EDITBOX,
  };
  selected = SHBrowseForFolderW(&bi);
  if (!selected) {
    *result = false;
    goto cleanup;
  }
  err = sgrow(dir, MAX_PATH + 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (!SHGetPathFromIDListW(selected, dir->ptr)) {
    err = errg(err_fail);
    goto cleanup;
  }
  dir->len = wcslen(dir->ptr);
  *result = true;

cleanup:
  if (selected) {
    CoTaskMemRealloc(selected, 0);
    selected = NULL;
  }
  if (idl) {
    CoTaskMemRealloc(idl, 0);
    idl = NULL;
  }
  if (desktop_folder) {
    desktop_folder->lpVtbl->Release(desktop_folder);
    desktop_folder = NULL;
  }
  return err;
}

NODISCARD static error click_select_folder_button(HWND const window) {
  struct wstr dir = {0};
  HWND *disabled_windows = NULL;
  error err = disable_family_windows(window, &disabled_windows);
  if (efailed(err)) {
    efree(&err);
  }
  err = gcmz_get_save_dir(&dir);
  if (efailed(err)) {
    efree(&err);
    err = gui_get_save_dir(&dir);
    if (efailed(err)) {
      efree(&err);
    }
  }
  bool ret = false;
  enum {
    buf_size = 1024,
  };
  wchar_t buf[buf_size];
  mo_snprintf_wchar(buf, buf_size, NULL, "%1$s", gettext("Choose Save Directory"));
  err = select_directory(window, &wstr_unmanaged(buf), &dir, &ret);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (!ret) {
    goto cleanup;
  }
  err = gui_set_save_dir(dir.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&dir));
  if (disabled_windows) {
    restore_disabled_family_windows(disabled_windows);
    disabled_windows = NULL;
  }
  return err;
}

NODISCARD static error click_set_to_initial_button(HWND const window) {
  (void)window;
  error err = gui_set_save_dir(g_initial_save_dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = gui_set_save_mode(g_initial_save_mode);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  return err;
}

NODISCARD static error click_set_to_default_button(HWND const window) {
  (void)window;
  error err = gui_set_save_dir_to_default();
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = gui_set_save_mode_to_default();
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  return err;
}

NODISCARD static error click_save_as_default_button(HWND const window) {
  (void)window;
  error err = gui_get_save_mode(&g_default_save_mode);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = gui_get_save_dir(&g_default_save_dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  return err;
}

void gui_handle_wm_command(HWND const window, WPARAM const wparam, LPARAM const lparam) {
  (void)lparam;
  switch (LOWORD(wparam)) {
  case 3:
    ereport(click_select_folder_button(window));
    break;
  case 4:
    ereport(click_set_to_initial_button(window));
    break;
  case 5:
    ereport(click_set_to_default_button(window));
    break;
  case 6:
    ereport(click_save_as_default_button(window));
    break;
  }
}

error gui_set_save_dir(wchar_t const *const dir) {
  if (!SetWindowTextW(g_save_dir, dir)) {
    return errhr(HRESULT_FROM_WIN32(GetLastError()));
  }
  return eok();
}

error gui_set_save_dir_to_default(void) {
  error err = gui_set_save_dir(g_default_save_dir.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  return err;
}

error gui_get_save_dir(struct wstr *const dest) {
  error err = get_window_text(g_save_dir, dest);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  return err;
}

error gui_set_save_mode(int const mode) {
  switch (mode) {
  case gui_mode_auto:
  case gui_mode_copy:
  case gui_mode_direct:
    break;
  default:
    return errg(err_unexpected);
  }
  SendMessageW(g_save_mode, CB_SETCURSEL, (WPARAM)mode, 0);
  return eok();
}

error gui_set_save_mode_to_default(void) {
  error err = gui_set_save_mode(g_default_save_mode);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  return err;
}

error gui_get_save_mode(int *const mode) {
  if (!mode) {
    return errg(err_null_pointer);
  }
  int const m = SendMessageW(g_save_mode, CB_GETCURSEL, 0, 0);
  switch (m) {
  case gui_mode_auto:
  case gui_mode_copy:
  case gui_mode_direct:
    *mode = m;
    break;
  default:
    return errg(err_unexpected);
  }
  return eok();
}
