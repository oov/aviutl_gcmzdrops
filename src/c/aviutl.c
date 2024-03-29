#include "aviutl.h"

#include "ovutil/str.h"
#include "ovutil/win32.h"

#include "error_gcmz.h"
#include "i18n.h"

static FILTER const *g_fp = NULL;
static void *g_editp = NULL;
static HMODULE g_lua51 = NULL;
static bool g_exedit_is_092 = false;

static FILTER const *g_exedit_fp = NULL;
static int g_aviutl_patched = aviutl_patched_default;

NODISCARD static error verify_installation(void) {
  struct wstr path = {0};
  error err = get_module_file_name(get_hinstance(), &path);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  size_t fnpos = 0;
  err = extract_file_name(&path, &fnpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  path.ptr[fnpos] = L'\0';
  path.len = fnpos;

  err = scat(&path, L"exedit.auf");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  bool found = false;
  err = file_exists(&path, &found);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (!found) {
    err = err(err_type_gcmz, err_gcmz_exedit_not_found_in_same_dir);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&path));
  return err;
}

NODISCARD static error find_exedit_filter(FILTER const **const exedit_fp, int *const patched) {
  static TCHAR const exedit_name_mbcs[] = "\x8a\x67\x92\xa3\x95\xd2\x8f\x57";              // "拡張編集"
  static TCHAR const zhcn_patched_exedit_name_mbcs[] = "\xc0\xa9\xd5\xb9\xb1\xe0\xbc\xad"; // "扩展编辑"
  static TCHAR const en_patched_exedit_name_mbcs[] = "Advanced Editing";

  *exedit_fp = NULL;
  SYS_INFO si = {0};
  error err = aviutl_get_sys_info(&si);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  for (int i = 0; i < si.filter_n; ++i) {
    FILTER const *p = g_fp->exfunc->get_filterp(i);
    if (!p || (p->flag & FILTER_FLAG_AUDIO_FILTER) == FILTER_FLAG_AUDIO_FILTER) {
      continue;
    }
    if (strcmp(p->name, exedit_name_mbcs) == 0) {
      *exedit_fp = p;
      *patched = aviutl_patched_default;
      return eok();
    } else if (strcmp(p->name, zhcn_patched_exedit_name_mbcs) == 0) {
      *exedit_fp = p;
      *patched = aviutl_patched_zh_cn;
      return eok();
    } else if (strcmp(p->name, en_patched_exedit_name_mbcs) == 0) {
      *exedit_fp = p;
      *patched = aviutl_patched_en;
      return eok();
    }
  }
  *exedit_fp = NULL;
  *patched = aviutl_patched_default;
  return err(err_type_gcmz, err_gcmz_exedit_not_found);
}

NODISCARD static error find_blocked_filter(void) {
  // extext.auf and the oledd.auf are unlikely to be used in the translation patched AviUtl,
  // so this block processing does not support them.
  static TCHAR const extext_name_mbcs[] = "\x8e\x9a\x96\x8b\x83\x41\x83\x56\x83\x58\x83\x67"; // "字幕アシスト"
  static TCHAR const gcmzdrops_name_mbcs[] =
      "\x82\xB2\x82\xBF\x82\xE1\x82\xDC\x82\xBA\x83\x68\x83\x8D\x83\x62\x83\x76\x83\x58"; // "ごちゃまぜドロップス"
  static wchar_t const gcmzdrops_old_dll_name[] = L"oledd.auf";
  SYS_INFO si = {0};
  struct wstr s = {0};
  error err = aviutl_get_sys_info(&si);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  for (int i = 0; i < si.filter_n; ++i) {
    FILTER *p = g_fp->exfunc->get_filterp(i);
    if (strcmp(p->name, extext_name_mbcs) == 0) {
      return err(err_type_gcmz, err_gcmz_extext_found);
    }
    if (strcmp(p->name, gcmzdrops_name_mbcs) == 0) {
      err = get_module_file_name(p->dll_hinst, &s);
      if (efailed(err)) {
        efree(&err);
        continue;
      }
      size_t pos = 0;
      err = extract_file_name(&s, &pos);
      if (efailed(err)) {
        efree(&err);
        continue;
      }
      if (wcscmp(s.ptr + pos, gcmzdrops_old_dll_name) == 0) {
        err = err(err_type_gcmz, err_gcmz_oledd_found);
        goto cleanup;
      }
    }
  }

cleanup:
  ereport(sfree(&s));
  return err;
}

NODISCARD static error verify_aviutl_version(void) {
  SYS_INFO si = {0};
  error err = aviutl_get_sys_info(&si);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  if (si.build < 10000) {
    return err(err_type_gcmz, err_gcmz_unsupported_aviutl_version);
  }
  return eok();
}

static size_t atou32(TCHAR const *s, uint32_t *const ret) {
  uint64_t r = 0;
  size_t i = 0;
  while (s[i]) {
    if (i >= 10 || '0' > s[i] || s[i] > '9') {
      break;
    }
    r = r * 10 + (uint64_t)(s[i++] - '0');
  }
  if (i == 0 || r > 0xffffffff) {
    return 0;
  }
  *ret = r & 0xffffffff;
  return i;
}

NODISCARD static error verify_exedit_version(FILTER const *const exedit_fp) {
  static TCHAR const version_token[] = " version ";
  TCHAR const *verstr = strstr(exedit_fp->information, version_token);
  if (!verstr) {
    goto failed;
  }
  verstr += strlen(version_token);
  uint32_t major = 0, minor = 0;
  size_t len = atou32(verstr, &major);
  if (!len) {
    goto failed;
  }
  verstr += len + 1; // skip dot
  len = atou32(verstr, &minor);
  if (!len) {
    goto failed;
  }
  if (major == 0 && minor < 92) {
    goto failed;
  }
  g_exedit_is_092 = major == 0 && minor == 92;
  return eok();

failed:
  return err(err_type_gcmz, err_gcmz_unsupported_exedit_version);
}

NODISCARD static error load_lua51(HMODULE *const lua51) {
  struct wstr path = {0};
  error err = get_module_file_name(get_hinstance(), &path);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  size_t fnpos = 0;
  err = extract_file_name(&path, &fnpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  path.ptr[fnpos] = L'\0';
  path.len = fnpos;

  err = scat(&path, L"lua51.dll");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  *lua51 = LoadLibraryW(path.ptr);
  if (!*lua51) {
    err = err(err_type_gcmz, err_gcmz_lua51_cannot_load);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&path));
  return err;
}

NODISCARD static error apply_window_capture_problem_workaround(FILTER const *const exedit_fp) {
  bool found_auls_transparence = false;
  {
    static TCHAR const auls_transparence_name_mbcs[] =
        "\x8a\x67\x92\xa3\x95\xd2\x8f\x57\x82\xf0\x94\xbc\x93\xa7\x96\xbe\x89\xbb"; // 拡張編集を半透明化
    SYS_INFO si = {0};
    error err = aviutl_get_sys_info(&si);
    if (efailed(err)) {
      err = ethru(err);
      return err;
    }
    for (int i = 0; i < si.filter_n; ++i) {
      FILTER const *const p = g_fp->exfunc->get_filterp(i);
      if (strcmp(p->name, auls_transparence_name_mbcs) == 0) {
        found_auls_transparence = true;
        break;
      }
    }
  }
  SetWindowLong(exedit_fp->hwnd, GWL_EXSTYLE, GetWindowLong(exedit_fp->hwnd, GWL_EXSTYLE) | WS_EX_LAYERED);
  if (!found_auls_transparence) {
    SetLayeredWindowAttributes(exedit_fp->hwnd, 0, 255, LWA_ALPHA);
  }
  return eok();
}

void aviutl_set_pointers(FILTER const *fp, void *editp) {
  g_fp = fp;
  g_editp = editp;
}

error aviutl_init(void) {
  FILTER const *exedit_fp = NULL;
  int patched = aviutl_patched_default;
  HMODULE lua51 = NULL;
  error err = verify_installation();
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = verify_aviutl_version();
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = find_exedit_filter(&exedit_fp, &patched);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = verify_exedit_version(exedit_fp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = find_blocked_filter();
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  // lua51.dll will be lazily loaded, but may fail if it is not on the search path.
  // To avoid this, load with the full path first.
  err = load_lua51(&lua51);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  // Screen capture seems to fail in some environments.
  // Transparency mode allows to avoid problems.
  err = apply_window_capture_problem_workaround(exedit_fp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  g_exedit_fp = exedit_fp;
  g_aviutl_patched = patched;
  g_lua51 = lua51;
  lua51 = NULL;

cleanup:
  if (lua51 != NULL) {
    FreeLibrary(lua51);
    lua51 = NULL;
  }
  return err;
}

bool aviutl_initalized(void) { return g_exedit_fp; }

error aviutl_exit(void) {
  if (g_lua51 != NULL) {
    FreeLibrary(g_lua51);
    g_lua51 = NULL;
  }
  g_exedit_fp = NULL;
  g_aviutl_patched = aviutl_patched_default;
  g_exedit_is_092 = false;
  return eok();
}

NODISCARD error aviutl_get_patch(int *const patched) {
  if (!patched) {
    return errg(err_null_pointer);
  }
  if (!aviutl_initalized()) {
    return errg(err_unexpected);
  }
  *patched = g_aviutl_patched;
  return eok();
}

error aviutl_get_exedit_window(HWND *const h) {
  if (!h) {
    return errg(err_null_pointer);
  }
  if (!g_exedit_fp) {
    return errg(err_unexpected);
  }
  *h = g_exedit_fp->hwnd;
  return eok();
}

HWND aviutl_get_exedit_window_must(void) {
  HWND h = NULL;
  error err = aviutl_get_exedit_window(&h);
  if (efailed(err)) {
    ereportmsg_i18n(err, gettext("Failed to obtain the window handle for Advanced Editing."));
    h = GetDesktopWindow();
  }
  return h;
}

error aviutl_get_my_window(HWND *const h) {
  if (!h) {
    return errg(err_null_pointer);
  }
  if (!g_fp) {
    return errg(err_unexpected);
  }
  *h = g_fp->hwnd;
  return eok();
}

HWND aviutl_get_my_window_must(void) {
  HWND h = NULL;
  error err = aviutl_get_my_window(&h);
  if (efailed(err)) {
    ereportmsg_i18n(err, gettext("Failed to obtain my filter window handle."));
    h = GetDesktopWindow();
  }
  return h;
}

NODISCARD int aviutl_get_exedit_zoom_level(void) {
  if (!g_exedit_fp || !g_exedit_is_092) {
    return -1;
  }
  return *(int *)((size_t)(g_exedit_fp->dll_hinst) + 0xa3fc4);
}

NODISCARD int aviutl_get_exedit_layer_height(void) {
  if (!g_exedit_fp || !g_exedit_is_092) {
    return -1;
  }
  return *(int *)((size_t)(g_exedit_fp->dll_hinst) + 0xa3e20);
}

NODISCARD int aviutl_get_exedit_edit_cursor_position(POINT *const pt) {
  if (!g_exedit_fp || !g_exedit_is_092) {
    return -1;
  }
  uint8_t const *const caption_width = (uint8_t *)((size_t)(g_exedit_fp->dll_hinst) + 0x32b74);
  uint8_t const *const ruler_height = (uint8_t *)((size_t)(g_exedit_fp->dll_hinst) + 0x32c14);
  if (caption_width[0] != 0x83 || caption_width[1] != 0xc0 || ruler_height[0] != 0x83 || ruler_height[1] != 0xc0) {
    return -1;
  }
  double const *const scale = (double *)((size_t)(g_exedit_fp->dll_hinst) + 0x09a548);
  int const *const rate = (int *)((size_t)(g_exedit_fp->dll_hinst) + 0x0a3fc8);
  int const *const scroll_offset = (int *)((size_t)(g_exedit_fp->dll_hinst) + 0x1a52f0);
  int const *const cursor_frame = (int *)((size_t)(g_exedit_fp->dll_hinst) + 0x1a5304);
  pt->x = (int)((*cursor_frame - *scroll_offset) * *rate / *scale) + 0x100 - caption_width[2];
  pt->y = 0x100 - ruler_height[2] + 8;
  return 0;
}

static size_t *g_ptr_1a6b78 = NULL;
static aviutl_on_exo_load g_on_exo_load_handler = NULL;
static void *g_on_exo_load_handler_userdata = NULL;

static HANDLE open_file_hook(char const *const filename, bool const is_exa) {
  struct str dest_filename = {0};
  error err = eok();
  if (g_on_exo_load_handler) {
    err = g_on_exo_load_handler(filename, &dest_filename, is_exa, g_on_exo_load_handler_userdata);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
cleanup:
  *g_ptr_1a6b78 = 0x1000;
  HANDLE h = CreateFileA(dest_filename.ptr ? dest_filename.ptr : filename,
                         GENERIC_READ,
                         0,
                         NULL,
                         OPEN_EXISTING,
                         FILE_ATTRIBUTE_NORMAL,
                         NULL);
  ereport(sfree(&dest_filename));
  ereport(err);
  return h;
}

static HANDLE open_file_hook_exo(char const *const filename) { return open_file_hook(filename, false); }

static HANDLE open_file_hook_exa(char const *const filename) { return open_file_hook(filename, true); }

static NODISCARD error overwrite_call(void *addr, size_t const addr_old, size_t const addr_new) {
  error err = eok();
  DWORD old_protect = 0;
  if (!VirtualProtect(addr, 5, PAGE_EXECUTE_READWRITE, &old_protect)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    return err;
  }
  uint8_t *code = addr;
  if (*code != 0xe8) {
    err = errg(err_unexpected);
    goto cleanup;
  }
  size_t *call_addr = (void *)(code + 1);
  if (*call_addr != addr_old) {
    err = errg(err_unexpected);
    goto cleanup;
  }
  *call_addr = addr_new;
cleanup:
  VirtualProtect(addr, 5, old_protect, &old_protect);
  if (esucceeded(err)) {
    FlushInstructionCache(GetCurrentProcess(), addr, 5);
  }
  return err;
}

NODISCARD error aviutl_set_exo_load_hook(aviutl_on_exo_load fn, void *userdata) {
  if (!g_exedit_fp) {
    return errg(err_unexpected);
  }
  if (!g_exedit_is_092) {
    return err(err_type_gcmz, err_gcmz_unsupported_exedit_version);
  }
  size_t const base_address = (size_t)g_exedit_fp->dll_hinst;
  size_t const exo_offset = 0x29225;
  size_t const exa_offset = 0x29de0;
  size_t const address_old = 0x4df70;
  g_ptr_1a6b78 = (size_t *)(base_address + 0x1a6b78);
  g_on_exo_load_handler = fn;
  g_on_exo_load_handler_userdata = userdata;
  error err = overwrite_call((void *)(base_address + exo_offset),
                             address_old - exo_offset - 5,
                             (size_t)(open_file_hook_exo)-base_address - exo_offset - 5);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = overwrite_call((void *)(base_address + exa_offset),
                       address_old - exa_offset - 5,
                       (size_t)(open_file_hook_exa)-base_address - exa_offset - 5);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  if (efailed(err)) {
    error err2 = overwrite_call((void *)(base_address + exa_offset),
                                (size_t)(open_file_hook_exa)-base_address - exa_offset - 5,
                                address_old - exa_offset - 5);
    efree(&err2);
    err2 = overwrite_call((void *)(base_address + exo_offset),
                          (size_t)(open_file_hook_exo)-base_address - exo_offset - 5,
                          address_old - exo_offset - 5);
    efree(&err2);
  }
  return err;
}

error aviutl_get_sys_info(SYS_INFO *const si) {
  if (!si) {
    return errg(err_null_pointer);
  }
  if (!g_fp) {
    return errg(err_unexpected);
  }
  if (!g_fp->exfunc->get_sys_info(g_editp, si)) {
    return errg(err_fail);
  }
  return eok();
}

error aviutl_get_editing_file_info(FILE_INFO *const fi) {
  if (!fi) {
    return errg(err_null_pointer);
  }
  if (!g_editp || !g_fp) {
    return errg(err_unexpected);
  }
  if (!g_fp->exfunc->get_file_info(g_editp, fi)) {
    return errg(err_fail);
  }
  if (fi->audio_rate == 0 || fi->audio_ch == 0) {
    return err(err_type_gcmz, err_gcmz_project_is_not_open);
  }
  return eok();
}

error aviutl_get_file_info(struct wstr const *const path, FILE_INFO *const fi, int *const samples) {
  if (!path) {
    return errg(err_invalid_arugment);
  }
  if (!fi || !samples) {
    return errg(err_null_pointer);
  }
  if (!g_editp || !g_fp) {
    return errg(err_unexpected);
  }

  FILE_INFO current = {0};
  error err = aviutl_get_editing_file_info(&current);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  struct str s = {0}; // TODO: use TCHAR
  err = to_mbcs(path, &s);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  AVI_FILE_HANDLE afh = g_fp->exfunc->avi_file_open(s.ptr, fi, 0);
  if (!afh) {
    err = errg(err_fail);
    goto cleanup;
  }
  *samples = g_fp->exfunc->avi_file_set_audio_sample_rate(afh, current.audio_rate, current.audio_ch);
  g_fp->exfunc->avi_file_close(afh);

cleanup:
  ereport(sfree(&s));
  return err;
}

error aviutl_get_project_path(struct wstr *const dest) {
  if (!dest) {
    return errg(err_null_pointer);
  }

  SYS_INFO si = {0};
  error err = aviutl_get_sys_info(&si);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }

  FILE_INFO fi = {0};
  err = aviutl_get_editing_file_info(&fi);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }

  if (si.project_name == NULL || si.project_name[0] == '\0') {
    return err(err_type_gcmz, err_gcmz_project_has_not_yet_been_saved);
  }

  err = from_mbcs(&str_unmanaged(si.project_name), dest);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  return eok();
}

error aviutl_get_frame(int *const f) {
  if (!f) {
    return errg(err_null_pointer);
  }
  if (!g_editp || !g_fp) {
    return errg(err_unexpected);
  }
  *f = g_fp->exfunc->get_frame(g_editp);
  return eok();
}

error aviutl_set_frame(int *const f) {
  if (!f) {
    return errg(err_null_pointer);
  }
  if (!g_editp || !g_fp) {
    return errg(err_unexpected);
  }
  *f = g_fp->exfunc->set_frame(g_editp, *f);
  return eok();
}

error aviutl_get_frame_n(int *const n) {
  if (!n) {
    return errg(err_null_pointer);
  }
  if (!g_editp || !g_fp) {
    return errg(err_unexpected);
  }
  *n = g_fp->exfunc->get_frame_n(g_editp);
  return eok();
}

error aviutl_set_frame_n(int *const n) {
  if (!n) {
    return errg(err_null_pointer);
  }
  if (!g_editp || !g_fp) {
    return errg(err_unexpected);
  }
  *n = g_fp->exfunc->set_frame_n(g_editp, *n);
  return eok();
}

error aviutl_get_select_frame(int *const start, int *const end) {
  if (!start || !end) {
    return errg(err_null_pointer);
  }
  if (!g_editp || !g_fp) {
    return errg(err_unexpected);
  }
  if (!g_fp->exfunc->get_select_frame(g_editp, start, end)) {
    *start = -1;
    *end = -1;
  }
  return eok();
}

error aviutl_set_select_frame(int const start, int const end) {
  if (!g_editp || !g_fp) {
    return errg(err_unexpected);
  }
  if (!g_fp->exfunc->set_select_frame(g_editp, start, end)) {
    return errg(err_fail);
  }
  return eok();
}

error aviutl_ini_load_int(struct str const *const key, int const defvalue, int *const dest) {
  if (!key) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }
  if (!g_fp) {
    return errg(err_unexpected);
  }
  *dest = g_fp->exfunc->ini_load_int(ov_deconster_(g_fp), key->ptr, defvalue);
  return eok();
}

error aviutl_ini_load_str(struct str const *const key, struct str const *const defvalue, struct str *const dest) {
  if (!key || !defvalue) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }
  if (!g_fp) {
    return errg(err_unexpected);
  }
  struct str tmp = {0};
  error err = sgrow(&tmp, 1024);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (!g_fp->exfunc->ini_load_str(ov_deconster_(g_fp), key->ptr, tmp.ptr, defvalue->ptr)) {
    err = errg(err_fail);
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

error aviutl_ini_save_int(struct str const *const key, int const value) {
  if (!key) {
    return errg(err_invalid_arugment);
  }
  if (!g_fp) {
    return errg(err_unexpected);
  }
  g_fp->exfunc->ini_save_int(ov_deconster_(g_fp), key->ptr, value);
  return eok();
}

error aviutl_ini_save_str(struct str const *const key, struct str const *const value) {
  if (!key || !value) {
    return errg(err_invalid_arugment);
  }
  if (!g_fp) {
    return errg(err_unexpected);
  }
  if (!g_fp->exfunc->ini_save_str(ov_deconster_(g_fp), key->ptr, value->ptr)) {
    return errg(err_fail);
  }
  return eok();
}
