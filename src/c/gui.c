#include "gui.h"

#include <Shlobj.h>

#include "aviutl.h"
#include "error_win32.h"
#include "gcmzfuncs.h"
#include "util.h"

static HWND g_save_mode_label = NULL;
static HWND g_save_mode = NULL;
static HWND g_save_dir_label = NULL;
static HWND g_save_dir = NULL;
static HWND g_save_dir_choose_folder = NULL;
static HWND g_save_dir_reset = NULL;

NODISCARD static error create_font(HWND const window, HFONT *const font, int *const font_height)
{
  if (!window)
  {
    return errg(err_invalid_arugment);
  }
  if (!font || !font_height)
  {
    return errg(err_null_pointer);
  }

  SYS_INFO si = {0};
  error err = aviutl_get_sys_info(&si);
  if (efailed(err))
  {
    err = ethru(err);
    return err;
  }
  NONCLIENTMETRICSW ncm = {
      .cbSize = sizeof(NONCLIENTMETRICSW),
  };
  if (!SystemParametersInfoW(SPI_GETNONCLIENTMETRICS, sizeof(NONCLIENTMETRICSW), &ncm, 0))
  {
    return err_hr(HRESULT_FROM_WIN32(GetLastError()));
  }

  HDC dc = GetDC(window);
  if (dc)
  {
    *font_height = -MulDiv(ncm.lfMessageFont.lfHeight, GetDeviceCaps(dc, LOGPIXELSY), 72);
    ReleaseDC(window, dc);
  }
  else
  {
    *font_height = 18;
  }
  *font = si.hfont;
  return eok();
}

error gui_init(HWND const window)
{
  HFONT font = NULL;
  HINSTANCE const hinst = get_hinstance();
  int font_height = 0;
  error err = create_font(window, &font, &font_height);
  if (efailed(err))
  {
    ereportmsg(err, &native_unmanaged(NSTR("フォントの作成に失敗しました。")));
    font_height = 18;
  }

  int y = 8;
  {
    int height = font_height;
    HWND const h = CreateWindowExW(0, L"STATIC", L"処理モード:", WS_CHILD | WS_VISIBLE | ES_LEFT, 8, y, 400, height, window, NULL, hinst, NULL);
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    y += height;
    g_save_mode_label = h;
  }
  {
    int height = font_height + GetSystemMetrics(SM_CYFIXEDFRAME) * 2;
    HWND const h = CreateWindowExW(0, L"COMBOBOX", NULL, WS_CHILD | WS_TABSTOP | WS_VISIBLE | CBS_DROPDOWNLIST, 8, y, 400, height + 300, window, (HMENU)1, hinst, NULL);
    SendMessageW(h, CB_ADDSTRING, 0, (LPARAM)L"自動判定");
    SendMessageW(h, CB_ADDSTRING, 0, (LPARAM)L"コピーを作成");
    SendMessageW(h, CB_ADDSTRING, 0, (LPARAM)L"直接読み込み");
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    y += height;
    g_save_mode = h;
  }
  y += 8;
  {
    int height = font_height;
    HWND const h = CreateWindowExW(0, L"STATIC", L"データ保存先:", WS_CHILD | WS_VISIBLE | ES_LEFT, 8, y, 400, height, window, NULL, hinst, NULL);
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    y += height;
    g_save_dir_label = h;
  }
  {
    int height = font_height + GetSystemMetrics(SM_CYFIXEDFRAME) * 2;
    HWND const h = CreateWindowExW(WS_EX_CLIENTEDGE, L"EDIT", L"", WS_CHILD | WS_TABSTOP | WS_VISIBLE | ES_AUTOHSCROLL | ES_LEFT, 8, y, 400, height, window, (HMENU)2, hinst, NULL);
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    y += height;
    g_save_dir = h;
  }
  {
    int height = font_height + GetSystemMetrics(SM_CYEDGE) * 2;
    HWND h = CreateWindowExW(0, L"BUTTON", L"標準設定に戻す", WS_CHILD | WS_TABSTOP | WS_VISIBLE, 8 + 400 - 256, y, 128, height, window, (HMENU)4, hinst, NULL);
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    g_save_dir_reset = h;
    h = CreateWindowExW(0, L"BUTTON", L"フォルダーの選択...", WS_CHILD | WS_TABSTOP | WS_VISIBLE, 8 + 400 - 128, y, 128, height, window, (HMENU)3, hinst, NULL);
    SendMessageW(h, WM_SETFONT, (WPARAM)font, 0);
    g_save_dir_choose_folder = h;
    y += height;
  }
  ereport(set_client_size(window, 8 + 400 + 8, y + 8));

  return eok();
}

void gui_exit(void)
{
  SendMessageW(g_save_mode_label, WM_SETFONT, 0, 0);
  SendMessageW(g_save_mode, WM_SETFONT, 0, 0);
  SendMessageW(g_save_dir_label, WM_SETFONT, 0, 0);
  SendMessageW(g_save_dir, WM_SETFONT, 0, 0);
  SendMessageW(g_save_dir_choose_folder, WM_SETFONT, 0, 0);
  SendMessageW(g_save_dir_reset, WM_SETFONT, 0, 0);
}

void gui_lock(void)
{
  EnableWindow(g_save_mode_label, FALSE);
  EnableWindow(g_save_mode, FALSE);
  EnableWindow(g_save_dir_label, FALSE);
  EnableWindow(g_save_dir, FALSE);
  EnableWindow(g_save_dir_choose_folder, FALSE);
  EnableWindow(g_save_dir_reset, FALSE);
}

static int CALLBACK select_directory_callback(HWND const window, UINT const message, LPARAM const lparam, LPARAM const lpdata)
{
  (void)lparam;
  if (message == BFFM_INITIALIZED && lpdata)
  {
    SendMessageW(window, BFFM_SETSELECTIONW, 0, lpdata);
  }
  return 0;
}

NODISCARD static error select_directory(HWND const parent, struct wstr *const caption, struct wstr *const dir, bool *const result)
{
  error err = eok();
  ITEMIDLIST *idl = NULL, *selected = NULL;
  IShellFolder *desktop_folder = NULL;
  HRESULT hr = SHGetDesktopFolder(&desktop_folder);
  if (FAILED(hr))
  {
    err = err_hr(hr);
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
  if (!selected)
  {
    *result = false;
    goto cleanup;
  }
  err = sgrow(dir, MAX_PATH + 1);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }
  if (!SHGetPathFromIDListW(selected, dir->ptr))
  {
    err = errg(err_fail);
    goto cleanup;
  }
  dir->len = wcslen(dir->ptr);
  *result = true;

cleanup:
  if (selected)
  {
    CoTaskMemRealloc(selected, 0);
    selected = NULL;
  }
  if (idl)
  {
    CoTaskMemRealloc(idl, 0);
    idl = NULL;
  }
  if (desktop_folder)
  {
    desktop_folder->lpVtbl->Release(desktop_folder);
    desktop_folder = NULL;
  }
  return err;
}

NODISCARD static error click_select_folder_button(HWND const window)
{
  struct wstr dir = {0};
  HWND *disabled_windows = NULL;
  error err = disable_family_windows(window, &disabled_windows);
  if (efailed(err))
  {
    efree(&err);
  }
  err = gcmz_get_save_dir(&dir);
  if (efailed(err))
  {
    efree(&err);
    err = gui_get_save_dir(&dir);
    if (efailed(err))
    {
      efree(&err);
    }
  }
  bool ret = false;
  err = select_directory(window, &wstr_unmanaged(L"データ保存先の選択"), &dir, &ret);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }
  if (!ret)
  {
    goto cleanup;
  }
  err = gui_set_save_dir(dir.ptr);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&dir));
  if (disabled_windows)
  {
    restore_disabled_family_windows(disabled_windows);
    disabled_windows = NULL;
  }
  return err;
}

NODISCARD static error click_reset_button(HWND const window)
{
  (void)window;
  ereport(gui_set_save_dir_to_default());
  return eok();
}

void gui_handle_wm_command(HWND const window, WPARAM const wparam, LPARAM const lparam)
{
  (void)lparam;
  switch (LOWORD(wparam))
  {
  case 3:
    ereport(click_select_folder_button(window));
    break;
  case 4:
    ereport(click_reset_button(window));
    break;
  }
}

error gui_set_save_dir(wchar_t const *const dir)
{
  if (!SetWindowTextW(g_save_dir, dir))
  {
    return err_hr(HRESULT_FROM_WIN32(GetLastError()));
  }
  return eok();
}

error gui_set_save_dir_to_default(void)
{
  return gui_set_save_dir(L"%PROJECTDIR%");
}

error gui_get_save_dir(struct wstr *const dest)
{
  error err = get_window_text(g_save_dir, dest);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  return err;
}

error gui_set_save_mode(int const mode)
{
  switch (mode)
  {
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

error gui_set_save_mode_to_default(void)
{
  return gui_set_mode(0);
}

error gui_get_save_mode(int *const mode)
{
  if (!mode)
  {
    return errg(err_null_pointer);
  }
  int const m = SendMessageW(g_save_mode, CB_GETCURSEL, 0, 0);
  switch (m)
  {
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
