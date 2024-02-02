#include "luafuncs.h"

#include "3rd/crc64/crc64.h"
#include "3rd/detect/detect.h"
#include "ovutil/str.h"
#include "ovutil/win32.h"

#include "ovutf.h"

#include <combaseapi.h>
#include <lauxlib.h>
#include <lualib.h>

#include "aviutl.h"
#include "droptarget.h"
#include "error_gcmz.h"
#include "gcmzfuncs.h"
#include "i18n.h"
#include "luafuncs_convertencoding.h"
#include "luautil.h"

NODISCARD static error verify_filename(struct wstr const *const ws) {
  struct str s = {0};
  struct wstr v = {0};
  error err = to_mbcs(ws, &s);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = from_mbcs(&s, &v);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (wcscmp(ws->ptr, v.ptr) != 0) {
    err = emsg_i18nf(err_type_gcmz, err_gcmz_invalid_char, NULL, "%1$ls", v.ptr);
  }

cleanup:
  ereport(sfree(&v));
  ereport(sfree(&s));
  return err;
}

error luafn_push_files(lua_State *const L, struct files const *const f) {
  if (!L || !f) {
    return errg(err_invalid_arugment);
  }

  lua_newtable(L);
  for (size_t i = 0; i < f->len; ++i) {
    struct file const *const file = f->ptr + i;
    lua_newtable(L);
    error err = verify_filename(&file->path);
    if (efailed(err)) {
      lua_pop(L, 2);
      err = ethru(err);
      return err;
    }
    err = luautil_push_wstr(L, &file->path);
    if (efailed(err)) {
      lua_pop(L, 2);
      err = ethru(err);
      return err;
    }
    lua_setfield(L, -2, "filepath");

    if (file->mime.len) {
      err = luautil_push_wstr(L, &file->mime);
      if (efailed(err)) {
        lua_pop(L, 2);
        err = ethru(err);
        return err;
      }
      lua_setfield(L, -2, "mediatype");
    }
    lua_rawseti(L, -2, (int)(i + 1));
  }
  return eok();
}

error luafn_push_state(lua_State *const L, POINTL const point, DWORD const key_state) {
  lua_newtable(L);
  lua_pushinteger(L, point.x);
  lua_setfield(L, -2, "x");
  lua_pushinteger(L, point.y);
  lua_setfield(L, -2, "y");
  lua_pushboolean(L, (key_state & MK_CONTROL) == MK_CONTROL);
  lua_setfield(L, -2, "control");
  lua_pushboolean(L, (key_state & MK_SHIFT) == MK_SHIFT);
  lua_setfield(L, -2, "shift");
  lua_pushboolean(L, (key_state & MK_ALT) == MK_ALT);
  lua_setfield(L, -2, "alt");
  lua_pushboolean(L, (key_state & MK_LBUTTON) == MK_LBUTTON);
  lua_setfield(L, -2, "lbutton");
  lua_pushboolean(L, (key_state & MK_MBUTTON) == MK_MBUTTON);
  lua_setfield(L, -2, "mbutton");
  lua_pushboolean(L, (key_state & MK_RBUTTON) == MK_RBUTTON);
  lua_setfield(L, -2, "rbutton");
  return eok();
}

static int luafn_debug_print(lua_State *const L) {
  struct wstr tmp = {0};
  struct wstr msg = {0};
  error err = luautil_towstr(L, 1, &msg);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpym(&tmp, L"GCMZDrops: ", msg.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  OutputDebugStringW(tmp.ptr);

cleanup:
  ereport(sfree(&msg));
  ereport(sfree(&tmp));
  return efailed(err) ? luautil_throw(L, err) : 0;
}

static int luafn_scriptdir(lua_State *const L) {
  struct wstr path = {0};
  error err = gcmz_get_script_dir(&path);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = include_trailing_path_delimiter(&path);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = luautil_push_wstr(L, &path);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&path));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

NODISCARD static error
luafn_createfile_core(struct wstr const *const name_, struct wstr const *const ext_, struct wstr *const dest) {
  if (!name_ || !ext_) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }

  struct wstr name = {0};
  struct wstr ext = {0};
  struct wstr fullpath = {0};
  struct wstr tmp = {0};

  error err = scpy(&name, name_->ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = scpy(&ext, ext_->ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  size_t pos = 0;
  err = extract_file_name(&name, &pos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = sanitize(&(struct wstr){
      .ptr = name.ptr + pos,
      .len = name.len - pos,
  });
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = sanitize(&ext);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = sreplace_all(&ext, L"\\", L"-");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = gcmz_get_save_dir(&tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = include_trailing_path_delimiter(&tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scat(&tmp, name.ptr + pos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = create_unique_file(tmp.ptr, ext.ptr, FILE_ATTRIBUTE_NORMAL, NULL, 0, &fullpath);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = files_add_delete_on_failure(&fullpath);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(dest, fullpath.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&fullpath));
  ereport(sfree(&tmp));
  ereport(sfree(&ext));
  ereport(sfree(&name));
  return err;
}

static int luafn_createfile(lua_State *const L) {
  struct wstr tmp = {0};
  struct wstr name = {0};
  struct wstr ext = {0};
  error err = luautil_towstr(L, 1, &name);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = luautil_towstr(L, 2, &ext);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = luafn_createfile_core(&name, &ext, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = luautil_push_wstr(L, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  ereport(sfree(&ext));
  ereport(sfree(&name));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

NODISCARD static error
luafn_createtempfile_core(struct wstr const *const name_, struct wstr const *const ext_, struct wstr *const dest) {
  if (!name_ || !ext_) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }

  struct wstr fullpath = {0};
  struct wstr name = {0};
  struct wstr ext = {0};
  error err = scpy(&name, name_->ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(&ext, ext_->ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  size_t pos = 0;
  err = extract_file_name(&name, &pos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = sanitize(&(struct wstr){
      .ptr = name.ptr + pos,
      .len = name.len - pos,
  });
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = sanitize(&ext);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = sreplace_all(&ext, L"\\", L"-");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = create_unique_temp_file(name.ptr + pos, ext.ptr, NULL, 0, &fullpath);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = files_add_delete_on_cleanup(&fullpath);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(dest, fullpath.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&fullpath));
  ereport(sfree(&ext));
  ereport(sfree(&name));
  return err;
}

static int luafn_createtempfile(lua_State *const L) {
  struct wstr tmp = {0};
  struct wstr name = {0};
  struct wstr ext = {0};
  error err = luautil_towstr(L, 1, &name);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = luautil_towstr(L, 2, &ext);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = luafn_createtempfile_core(&name, &ext, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = luautil_push_wstr(L, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  ereport(sfree(&ext));
  ereport(sfree(&name));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static int luafn_findallfile(lua_State *const L) {
  struct wstr tmp = {0};
  struct wstr wildcard = {0};
  HANDLE h = INVALID_HANDLE_VALUE;
  error err = luautil_towstr(L, 1, &wildcard);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  lua_newtable(L);

  err = gcmz_get_save_dir(&tmp);
  if (efailed(err)) {
    if (eis(err, err_type_gcmz, err_gcmz_project_is_not_open) ||
        eis(err, err_type_gcmz, err_gcmz_project_has_not_yet_been_saved)) {
      efree(&err);
      goto cleanup;
    }
    err = ethru(err);
    goto cleanup;
  }
  err = include_trailing_path_delimiter(&tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  size_t const base_len = tmp.len;
  err = scat(&tmp, wildcard.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  int idx = 0;

  WIN32_FIND_DATAW fd = {0};
  h = FindFirstFileW(tmp.ptr, &fd);
  if (h == INVALID_HANDLE_VALUE) {
    HRESULT hr = HRESULT_FROM_WIN32(GetLastError());
    if (hr == HRESULT_FROM_WIN32(ERROR_FILE_NOT_FOUND) || hr == HRESULT_FROM_WIN32(ERROR_PATH_NOT_FOUND)) {
      goto cleanup;
    }
    err = errhr(hr);
    goto cleanup;
  }
  do {
    if (wcscmp(fd.cFileName, L".") == 0 || wcscmp(fd.cFileName, L"..") == 0 ||
        (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == FILE_ATTRIBUTE_DIRECTORY) {
      continue;
    }
    tmp.len = base_len;
    tmp.ptr[base_len] = L'\0';
    err = scat(&tmp, fd.cFileName);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }

    err = luautil_push_wstr(L, &tmp);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }

    lua_rawseti(L, -2, ++idx);
  } while (FindNextFileW(h, &fd));

cleanup:
  if (h != INVALID_HANDLE_VALUE) {
    FindClose(h);
    h = INVALID_HANDLE_VALUE;
  }
  ereport(sfree(&wildcard));
  ereport(sfree(&tmp));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static int luafn_englishpatched(lua_State *const L) {
  int patch = aviutl_patched_default;
  error err = aviutl_get_patch(&patch);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushboolean(L, patch == aviutl_patched_en);

cleanup:
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static int luafn_getpatchid(lua_State *const L) {
  int patch = aviutl_patched_default;
  error err = aviutl_get_patch(&patch);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushinteger(L, patch);

cleanup:
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static int luafn_needcopy(lua_State *const L) {
  struct wstr tmp = {0};
  error err = luautil_towstr(L, 1, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  bool need_copy = false;
  err = gcmz_is_need_copy(&tmp, &need_copy);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushboolean(L, need_copy);

cleanup:
  ereport(sfree(&tmp));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static int luafn_calchash(lua_State *const L) {
  error err = eok();

  uint64_t hash = ULLONG_MAX;
  if (lua_isstring(L, 1)) {
    size_t len = 0;
    void const *const p = lua_tolstring(L, 1, &len);
    if (len != 8) {
      err = errg(err_invalid_arugment);
      goto cleanup;
    }
    hash = *(uint64_t const *)(p);
  }
  size_t slen = 0;
  char const *const s = lua_tolstring(L, 2, &slen);
  hash = crc64(hash, (unsigned char const *)s, slen);
  lua_pushlstring(L, (char const *)&hash, sizeof(uint64_t));

cleanup:
  return efailed(err) ? luautil_throw(L, err) : 1;
}

NODISCARD static error luafn_calcfilehash_core(struct wstr const *const path, uint64_t *const dest) {
  if (!path) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }

  HANDLE h = CreateFileW(path->ptr, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (h == INVALID_HANDLE_VALUE) {
    return errhr(HRESULT_FROM_WIN32(GetLastError()));
  }
  enum {
    buf_size = 4096,
  };
  uint8_t buf[buf_size] = {0};
  uint64_t hash = ULLONG_MAX;
  for (;;) {
    DWORD read = 0;
    if (!ReadFile(h, buf, buf_size, &read, NULL)) {
      error err = errhr(HRESULT_FROM_WIN32(GetLastError()));
      CloseHandle(h);
      return err;
    }
    if (read == 0) {
      break;
    }
    hash = crc64(hash, buf, read);
  }
  CloseHandle(h);

  *dest = hash;
  return eok();
}

static int luafn_calcfilehash(lua_State *const L) {
  struct wstr path = {0};
  error err = luautil_towstr(L, 1, &path);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  uint64_t hash = 0;
  err = luafn_calcfilehash_core(&path, &hash);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushlstring(L, (char const *)&hash, sizeof(uint64_t));

cleanup:
  ereport(sfree(&path));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

NODISCARD static error base32_encode(struct str const *const src, struct wstr *const dest) {
  if (!dest) {
    return errg(err_null_pointer);
  }

  struct wstr tmp = {0};
  error err = eok();

  static wchar_t const table[] = L"abcdefghijklmnopqrstuvwxyz234567";
  wchar_t buf[8] = {0};
  uint8_t const *const sp = (void *)src->ptr;
  size_t const slen = src->len;
  size_t i = 0;
  while (i + 4 < slen) {
    uint64_t const v = ((uint64_t)sp[i + 0] << 32) | ((uint64_t)sp[i + 1] << 24) | ((uint64_t)sp[i + 2] << 16) |
                       ((uint64_t)sp[i + 3] << 8) | ((uint64_t)sp[i + 4] << 0);
    buf[0] = table[(v >> 0) & 0x1f];
    buf[1] = table[(v >> 5) & 0x1f];
    buf[2] = table[(v >> 10) & 0x1f];
    buf[3] = table[(v >> 15) & 0x1f];
    buf[4] = table[(v >> 20) & 0x1f];
    buf[5] = table[(v >> 25) & 0x1f];
    buf[6] = table[(v >> 30) & 0x1f];
    buf[7] = table[(v >> 35) & 0x1f];
    err = sncat(&tmp, buf, 8);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    i += 5;
  }

  if (i < slen) {
    uint64_t v = 0;
    size_t const remain = slen - i;
    size_t pad = 0;
    if (remain >= 1) {
      v = (v << 8) | sp[i + 0];
      pad = 6;
    }
    if (remain >= 2) {
      v = (v << 8) | sp[i + 1];
      pad = 4;
    }
    if (remain >= 3) {
      v = (v << 8) | sp[i + 2];
      pad = 3;
    }
    if (remain >= 4) {
      v = (v << 8) | sp[i + 3];
      pad = 1;
    }
    buf[0] = table[(v >> 0) & 0x1f];
    buf[1] = table[(v >> 5) & 0x1f];
    buf[2] = table[(v >> 10) & 0x1f];
    buf[3] = table[(v >> 15) & 0x1f];
    buf[4] = table[(v >> 20) & 0x1f];
    buf[5] = table[(v >> 25) & 0x1f];
    buf[6] = table[(v >> 30) & 0x1f];
    for (i = 0; i < pad; ++i) {
      buf[8 - i - 1] = L'=';
    }
    err = sncat(&tmp, buf, 8);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
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

NODISCARD static error luafn_hashtostring_core(uint64_t const hash, struct wstr *const dest) {
  if (!dest) {
    return errg(err_null_pointer);
  }

  struct wstr tmp = {0};
  char const buf[9] = {
      (char)((hash >> 56) & 0xff),
      (char)((hash >> 48) & 0xff),
      (char)((hash >> 40) & 0xff),
      (char)((hash >> 32) & 0xff),
      (char)((hash >> 24) & 0xff),
      (char)((hash >> 16) & 0xff),
      (char)((hash >> 8) & 0xff),
      (char)((hash >> 0) & 0xff),
      0,
  };
  error err = base32_encode(&str_unmanaged_const(buf), &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = sreplace_all(&tmp, L"=", L"");
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

static int luafn_hashtostring(lua_State *const L) {
  struct wstr tmp = {0};
  error err = eok();

  size_t len = 0;
  void const *const p = lua_tolstring(L, 1, &len);
  if (len != 8) {
    err = errg(err_invalid_arugment);
    goto cleanup;
  }
  err = luafn_hashtostring_core(*(uint64_t const *)p, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = luautil_push_wstr(L, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static int luafn_getexeditfileinfo(lua_State *const L) {
  FILE_INFO fi = {0};
  error err = aviutl_get_editing_file_info(&fi);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  lua_newtable(L);
  lua_pushinteger(L, fi.w);
  lua_setfield(L, -2, "width");
  lua_pushinteger(L, fi.h);
  lua_setfield(L, -2, "height");
  lua_pushinteger(L, fi.video_rate);
  lua_setfield(L, -2, "rate");
  lua_pushinteger(L, fi.video_scale);
  lua_setfield(L, -2, "scale");
  lua_pushinteger(L, fi.frame_n);
  lua_setfield(L, -2, "length");
  lua_pushinteger(L, fi.audio_rate);
  lua_setfield(L, -2, "audio_rate");
  lua_pushinteger(L, fi.audio_ch);
  lua_setfield(L, -2, "audio_ch");

cleanup:
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static int luafn_getfileinfo(lua_State *const L) {
  struct wstr path = {0};
  error err = luautil_towstr(L, 1, &path);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  FILE_INFO fi = {0};
  int samples = 0;
  err = aviutl_get_file_info(&path, &fi, &samples);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  lua_newtable(L);
  lua_pushinteger(L, fi.w);
  lua_setfield(L, -2, "width");
  lua_pushinteger(L, fi.h);
  lua_setfield(L, -2, "height");
  lua_pushinteger(L, fi.video_rate);
  lua_setfield(L, -2, "rate");
  lua_pushinteger(L, fi.video_scale);
  lua_setfield(L, -2, "scale");
  lua_pushinteger(L, fi.frame_n);
  lua_setfield(L, -2, "length");
  lua_pushinteger(L, fi.audio_rate);
  lua_setfield(L, -2, "audio_rate");
  lua_pushinteger(L, fi.audio_ch);
  lua_setfield(L, -2, "audio_ch");
  lua_pushinteger(L, samples);
  lua_setfield(L, -2, "audio_samples");

cleanup:
  ereport(sfree(&path));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

NODISCARD static error encode_exo_text(struct wstr const *const src, struct str *const dest) {
  if (!src) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }

  static char const *const hex = "0123456789abcdef";
  enum {
    textlen = 1024,
  };
  struct str tmp = {0};
  error err = sgrow(&tmp, textlen * 4 + 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  wchar_t const *sp = src->ptr;
  char *dp = tmp.ptr;
  size_t si = 0;
  for (size_t const len = src->len; si < len; ++si) {
    wchar_t wc = sp[si];
    size_t const di = si * 4;
    dp[di + 0] = hex[(wc >> 4) & 0x0f];
    dp[di + 1] = hex[(wc >> 0) & 0x0f];
    dp[di + 2] = hex[(wc >> 12) & 0x0f];
    dp[di + 3] = hex[(wc >> 8) & 0x0f];
  }
  memset(dp + (si * 4), '0', textlen * 4 - (si * 4));
  tmp.len = textlen * 4;
  tmp.ptr[tmp.len] = '\0';

  err = scpy(dest, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  return err;
}

static inline int hex2int(int const c) {
  if (L'0' <= c && c <= L'9') {
    return c - L'0';
  }
  if ((L'A' <= c && c <= L'F') || (L'a' <= c && c <= L'f')) {
    return (c & 0x5f) - L'A' + 10;
  }
  return 0x10000;
}

NODISCARD static error decode_exo_text(struct str const *const src, struct wstr *const dest) {
  if (!src) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }

  struct wstr tmp = {0};
  error err = sgrow(&tmp, src->len / 4 + 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  char const *sp = src->ptr;
  wchar_t *dp = tmp.ptr;
  size_t di = 0;
  for (size_t si = 0, len = src->len; si + 3 < len; si += 4) {
    uint_least32_t c = (uint_least32_t)((hex2int(sp[si + 2]) << 12) | (hex2int(sp[si + 3]) << 8) |
                                        (hex2int(sp[si + 0]) << 4) | (hex2int(sp[si + 1]) << 0));
    if (c >= 0x10000) {
      err = errg(err_fail);
      goto cleanup;
    }
    dp[di++] = c & 0xffff;
  }
  dp[di] = L'\0';
  tmp.len = di;

  err = scpy(dest, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  return err;
}

static int luafn_encodeexotext(lua_State *const L) {
  struct wstr tmp = {0};
  struct str text = {0};
  error err = luautil_towstr(L, 1, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = encode_exo_text(&tmp, &text);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushlstring(L, text.ptr, text.len);

cleanup:
  ereport(sfree(&tmp));
  ereport(sfree(&text));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static int luafn_encodeexotextutf8(lua_State *const L) {
  struct wstr tmp = {0};
  struct str text = {0};

  error err = from_utf8(&str_unmanaged_const(lua_tostring(L, 1)), &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = encode_exo_text(&tmp, &text);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushlstring(L, text.ptr, text.len);

cleanup:
  ereport(sfree(&tmp));
  ereport(sfree(&text));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static int luafn_decodeexotextutf8(lua_State *const L) {
  struct str tmp = {0};
  struct wstr text = {0};
  error err = eok();

  void const *const p = lua_tostring(L, 1);
  if (p == NULL) {
    err = errg(err_invalid_arugment);
    goto cleanup;
  }
  err = decode_exo_text(&str_unmanaged_const(p), &text);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = to_utf8(&text, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushlstring(L, tmp.ptr, tmp.len);

cleanup:
  ereport(sfree(&tmp));
  ereport(sfree(&text));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

NODISCARD static error encode_lua_string(struct str const *const src, struct str *const dest) {
  struct str tmp = {0};
  size_t dlen = src->len + 3; // + 3 is ""\0
  error err = sgrow(&tmp, dlen);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  char const *sp = src->ptr;
  char *dp = tmp.ptr;
  size_t di = 0;
  dp[0] = '"';
  for (size_t si = 0, slen = src->len; si < slen; ++si) {
    char c = sp[si];
    switch (c) {
    case 0x00:
      c = '0';
      break;
    case 0x07:
      c = 'a';
      break;
    case 0x08:
      c = 'b';
      break;
    case 0x09:
      c = 't';
      break;
    case 0x0a:
      c = 'n';
      break;
    case 0x0b:
      c = 'v';
      break;
    case 0x0c:
      c = 'f';
      break;
    case 0x0d:
      c = 'r';
      break;
    case 0x22:
    case 0x27:
    case 0x5c:
      break;
    default:
      dp[++di] = c;
      continue;
    }
    err = sgrow(&tmp, ++dlen);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    dp = tmp.ptr;
    dp[++di] = '\\';
    dp[++di] = c;
  }
  dp[++di] = '"';
  dp[++di] = '\0';
  tmp.len = di;

  err = scpy(dest, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  return err;
}

static int luafn_encodeluastring(lua_State *const L) {
  struct str tmp = {0};
  error err = eok();

  size_t slen = 0;
  char const *const s = lua_tolstring(L, 1, &slen);
  if (!s) {
    err = errg(err_invalid_arugment);
    goto cleanup;
  }

  err = encode_lua_string(
      &(struct str const){
          .ptr = ov_deconster_(s),
          .len = slen,
      },
      &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  lua_pushlstring(L, tmp.ptr, tmp.len);

cleanup:
  ereport(sfree(&tmp));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static bool luafn_isutf8_core(char const *const s, size_t const slen) {
  if (!s) {
    return false;
  }
  if (!slen) {
    return true;
  }
  if (slen >= 3 && memcmp(s, (uint8_t[3]){0xef, 0xbb, 0xbf}, 3) == 0) {
    if (slen == 3) {
      return true;
    }
    return ov_utf8_to_wchar_len(s + 3, slen - 3) > 0;
  }
  return ov_utf8_to_wchar_len(s, slen) > 0;
}

static int luafn_isutf8(lua_State *const L) {
  size_t slen = 0;
  char const *s = lua_tolstring(L, 1, &slen);
  lua_pushboolean(L, luafn_isutf8_core(s, slen));
  return 1;
}

static int luafn_detectencoding(lua_State *const L) {
  error err = eok();
  size_t slen = 0;
  char const *const s = lua_tolstring(L, 1, &slen);
  if (!s) {
    err = errg(err_invalid_arugment);
    goto cleanup;
  }

  struct detector_state ds = {0};
  char const *enc = NULL;
  switch (detect_japanese_encoding(&ds, (uint8_t const *)s, slen)) {
  case ENCODING_UNKNOWN:
    enc = "";
    break;
  case ENCODING_UTF8:
    enc = "utf8";
    break;
  case ENCODING_EUCJP:
    enc = "eucjp";
    break;
  case ENCODING_SHIFTJIS:
    enc = "sjis";
    break;
  case ENCODING_ISO2022JP:
    enc = "iso2022jp";
    break;
  case ENCODING_UTF16:
    enc = "utf16le";
    break;
  case ENCODING_UTF16BE:
    enc = "utf16be";
    break;
  default:
    err = errg(err_unexpected);
    goto cleanup;
  }
  lua_pushstring(L, enc);

cleanup:
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static int luafn_prompt(lua_State *const L) {
  struct wstr caption = {0};
  struct wstr value = {0};
  error err = luautil_towstr(L, 1, &caption);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = luautil_towstr(L, 2, &value);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  bool r = false;
  err = gcmz_prompt(&caption, &value, &r);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushboolean(L, r);
  err = luautil_push_wstr(L, &value);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&value));
  ereport(sfree(&caption));
  return efailed(err) ? luautil_throw(L, err) : 2;
}

static int luafn_confirm(lua_State *const L) {
  struct wstr caption = {0};
  error err = luautil_towstr(L, 1, &caption);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  bool r = false;
  err = gcmz_confirm(&caption, &r);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushboolean(L, r);

cleanup:
  ereport(sfree(&caption));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

NODISCARD static error copy_and_drop(HWND const window, POINT const pt, struct wstr const *const filepath) {
  struct wstr newpath = {0};
  struct wstr const *const files[] = {&newpath, NULL};
  error err = gcmz_get_save_dir(&newpath);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = include_trailing_path_delimiter(&newpath);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  size_t fnpos = 0;
  err = extract_file_name(filepath, &fnpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scat(&newpath, filepath->ptr + fnpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  bool found = false;
  err = file_exists(&newpath, &found);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (found) {
    uint64_t a = 0, b = 0;
    err = luafn_calcfilehash_core(filepath, &a);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = luafn_calcfilehash_core(&newpath, &b);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    if (a != b) {
      err = err(err_type_gcmz, err_gcmz_exists_different_hash_value_file);
      goto cleanup;
    }
    err = gcmz_drop(window, pt, files);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    goto cleanup;
  }
  if (!CopyFileW(filepath->ptr, newpath.ptr, TRUE)) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  err = files_add_delete_on_failure(&newpath);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = gcmz_drop(window, pt, files);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&newpath));
  return err;
}

static int luafn_drop(lua_State *const L) {
  struct wstr filepath = {0};
  struct wstr const *const files[] = {&filepath, NULL};
  HWND exedit_window = NULL;
  error err = aviutl_get_exedit_window(&exedit_window);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  struct gcmz_analysed_info ai = {0};
  err = gcmz_analyse_exedit_window(&ai);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_getfield(L, 2, "x");
  lua_getfield(L, 2, "y");
  POINT pt = {
      .x = lua_tointeger(L, -2),
      .y = lua_tointeger(L, -1),
  };
  lua_pop(L, 2);

  int N = (int)lua_objlen(L, 1);
  for (int i = 1; i <= N; ++i) {
    lua_rawgeti(L, 1, i);

    lua_getfield(L, -1, "filepath");
    err = luautil_towstr(L, -1, &filepath);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    lua_pop(L, 1);

    bool need_copy = false, exists_in_save_dir = false;
    err = gcmz_is_need_copy(&filepath, &need_copy);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    if (need_copy) {
      err = gcmz_exists_in_save_dir(&filepath, &exists_in_save_dir);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
    }
    if (!need_copy || exists_in_save_dir) {
      err = gcmz_drop(exedit_window, pt, files);
    } else {
      err = copy_and_drop(exedit_window, pt, &filepath);
    }
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }

    lua_pop(L, 1);
    pt.y += ai.layer_height;
  }

  lua_getfield(L, 2, "frameadvance");
  int af = lua_tointeger(L, -1);
  lua_pop(L, 1);
  if (af != 0) {
    err = gcmz_advance_frame(af);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  lua_pushboolean(L, 1);

cleanup:
  ereport(sfree(&filepath));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static int luafn_getclipboard(lua_State *const L) {
  struct clipboard c = {0};
  error err = clipboard_get(&c);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = luafn_push_files(L, &c.files);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(clipboard_free(&c));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static int luafn_deleteonfinish(lua_State *const L) {
  struct wstr tmp = {0};
  error err = luautil_towstr(L, 1, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = files_add_delete_on_cleanup(&tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  return efailed(err) ? luautil_throw(L, err) : 0;
}

static int luafn_deleteonabort(lua_State *const L) {
  struct wstr tmp = {0};
  error err = luautil_towstr(L, 1, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = files_add_delete_on_failure(&tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  return efailed(err) ? luautil_throw(L, err) : 0;
}

static int luafn_doevents(lua_State *const L) {
  UINT const msg_min = (UINT)lua_tointeger(L, 1);
  UINT const msg_max = (UINT)lua_tointeger(L, 2);
  MSG m = {0};
  while (PeekMessageW(&m, 0, msg_min, msg_max, PM_REMOVE)) {
    TranslateMessage(&m);
    DispatchMessageW(&m);
  }
  return 0;
}

static int luafn_get_preferred_language(lua_State *const L) {
  struct wstr preferred_languages = {0};
  error err = mo_get_preferred_ui_languages(&preferred_languages);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_newtable(L);
  int idx = 0;
  for (wchar_t const *l = preferred_languages.ptr; *l != L'\0'; l += wcslen(l) + 1) {
    err = luautil_push_wstr(L, &wstr_unmanaged_const(l));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    lua_rawseti(L, -2, ++idx);
  }
cleanup:
  ereport(sfree(&preferred_languages));
  return efailed(err) ? luautil_throw(L, err) : 1;
}

static size_t
choose_language(WORD const *const preferred, size_t const num_preferred, WORD const *const langs, size_t num_langs) {
  size_t candidate = 0;
  for (size_t i = 0; i < num_preferred; ++i) {
    WORD const prefered_primary = PRIMARYLANGID(preferred[i]);
    for (size_t j = 0; j < num_langs; ++j) {
      if (langs[j] == preferred[i]) {
        return j + 1;
      }
      if (!candidate && (prefered_primary == PRIMARYLANGID(langs[j]))) {
        candidate = j + 1;
      }
    }
  }
  return candidate;
}

typedef LCID(WINAPI *LocaleNameToLCIDProc)(LPCWSTR lpName, DWORD dwFlags);

static NODISCARD error table_to_languages(lua_State *const L,
                                          WORD *const langs,
                                          size_t *const num_langs,
                                          LocaleNameToLCIDProc toLCID) {
  if (!L || !langs || !num_langs || !*num_langs) {
    return errg(err_invalid_arugment);
  }
  struct wstr ws = {0};
  error err = eok();
  size_t n = 0, len = *num_langs;
  while (1) {
    lua_rawgeti(L, -1, (int)(n + 1));
    if (lua_isnil(L, -1)) {
      lua_pop(L, 1);
      break;
    }
    if (lua_type(L, -1) != LUA_TSTRING) {
      err = emsg_i18nf(err_type_generic, err_fail, L"%1$s", gettext("\"%1$s\" is not a string."), lua_tostring(L, -1));
      goto cleanup;
    }
    size_t slen = 0;
    char const *s = lua_tolstring(L, -1, &slen);
    if (slen >= 32) {
      err = emsg_i18nf(err_type_generic, err_fail, L"%1$s", gettext("\"%1$s\" is too long."), s);
      goto cleanup;
    }
    err = to_wstr(s, slen, &ws);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    if (ws.len >= 3 && ws.ptr[2] == L'_') {
      ws.ptr[2] = L'-';
    }
    if (n == len) {
      err = emsg_i18nf(
          err_type_generic, err_fail, L"%zu", gettext("More than %zu elements in the language name table."), len);
      goto cleanup;
    }
    langs[n++] = LANGIDFROMLCID(toLCID(ws.ptr, 0));
    lua_pop(L, 1);
  }
  *num_langs = n;
cleanup:
  ereport(sfree(&ws));
  return err;
}

static int luafn_choose_language(lua_State *const L) {
  HMODULE h = NULL;
  struct wstr ws = {0};
  error err = eok();
  if (lua_type(L, 1) != LUA_TTABLE || lua_type(L, 2) != LUA_TTABLE) {
    err = errg(err_invalid_arugment);
    goto cleanup;
  }
  h = LoadLibraryW(L"kernel32.dll");
  if (h == NULL) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  LocaleNameToLCIDProc toLCID = (LocaleNameToLCIDProc)(void *)GetProcAddress(h, "LocaleNameToLCID");
  if (toLCID == NULL) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  enum {
    buf_size = 256,
  };
  WORD preferred[buf_size] = {0};
  size_t num_preferred = buf_size;
  lua_pushvalue(L, 1);
  err = table_to_languages(L, preferred, &num_preferred, toLCID);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pop(L, 1);

  WORD lang[buf_size] = {0};
  size_t num_lang = buf_size;
  lua_pushvalue(L, 2);
  err = table_to_languages(L, lang, &num_lang, toLCID);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pop(L, 1);

  size_t enUS = 0, first = 0;
  for (size_t i = 0; i < num_lang && (!enUS || !first); ++i) {
    if (!enUS && lang[i] == MAKELANGID(LANG_ENGLISH, SUBLANG_ENGLISH_US)) {
      enUS = i + 1;
    }
    if (!first && lang[i]) {
      first = i + 1;
    }
  }
  size_t idx = choose_language(preferred, num_preferred, lang, num_lang);
  if (!idx) {
    idx = enUS;
  }
  if (!idx) {
    idx = first;
  }
  lua_pushinteger(L, (int)idx);
cleanup:
  ereport(sfree(&ws));
  if (h) {
    FreeLibrary(h);
    h = NULL;
  }
  return efailed(err) ? luautil_throw(L, err) : 1;
}

void luafn_register_funcs(lua_State *const L) {
  lua_pushcfunction(L, luafn_debug_print);
  lua_setglobal(L, "debug_print");

  lua_newtable(L);
  lua_pushcfunction(L, luafn_scriptdir);
  lua_setfield(L, -2, "scriptdir");
  lua_pushcfunction(L, luafn_createfile);
  lua_setfield(L, -2, "createfile");
  lua_pushcfunction(L, luafn_createtempfile);
  lua_setfield(L, -2, "createtempfile");
  lua_pushcfunction(L, luafn_findallfile);
  lua_setfield(L, -2, "findallfile");
  lua_pushcfunction(L, luafn_englishpatched);
  lua_setfield(L, -2, "englishpatched");
  lua_pushcfunction(L, luafn_getpatchid);
  lua_setfield(L, -2, "getpatchid");
  lua_pushcfunction(L, luafn_needcopy);
  lua_setfield(L, -2, "needcopy");
  lua_pushcfunction(L, luafn_calchash);
  lua_setfield(L, -2, "calchash");
  lua_pushcfunction(L, luafn_calcfilehash);
  lua_setfield(L, -2, "calcfilehash");
  lua_pushcfunction(L, luafn_hashtostring);
  lua_setfield(L, -2, "hashtostring");
  lua_pushcfunction(L, luafn_getexeditfileinfo);
  lua_setfield(L, -2, "getexeditfileinfo");
  lua_pushcfunction(L, luafn_getfileinfo);
  lua_setfield(L, -2, "getfileinfo");
  lua_pushcfunction(L, luafn_encodeexotext);
  lua_setfield(L, -2, "encodeexotext");
  lua_pushcfunction(L, luafn_encodeexotextutf8);
  lua_setfield(L, -2, "encodeexotextutf8");
  lua_pushcfunction(L, luafn_decodeexotextutf8);
  lua_setfield(L, -2, "decodeexotextutf8");
  lua_pushcfunction(L, luafn_encodeluastring);
  lua_setfield(L, -2, "encodeluastring");
  lua_pushcfunction(L, luafn_isutf8);
  lua_setfield(L, -2, "isutf8");
  lua_pushcfunction(L, luafn_detectencoding);
  lua_setfield(L, -2, "detectencoding");
  lua_pushcfunction(L, luafn_convertencoding);
  lua_setfield(L, -2, "convertencoding");
  lua_pushcfunction(L, luafn_prompt);
  lua_setfield(L, -2, "prompt");
  lua_pushcfunction(L, luafn_confirm);
  lua_setfield(L, -2, "confirm");
  lua_pushcfunction(L, luafn_drop);
  lua_setfield(L, -2, "drop");
  lua_pushcfunction(L, luafn_getclipboard);
  lua_setfield(L, -2, "getclipboard");
  lua_pushcfunction(L, luafn_deleteonfinish);
  lua_setfield(L, -2, "deleteonfinish");
  lua_pushcfunction(L, luafn_deleteonabort);
  lua_setfield(L, -2, "deleteonabort");
  lua_pushcfunction(L, luafn_doevents);
  lua_setfield(L, -2, "doevents");
  lua_pushcfunction(L, luafn_get_preferred_language);
  lua_setfield(L, -2, "get_preferred_language");
  lua_pushcfunction(L, luafn_choose_language);
  lua_setfield(L, -2, "choose_language");

  lua_setglobal(L, "GCMZDrops");
}
