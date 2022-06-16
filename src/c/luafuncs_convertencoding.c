#include "luafuncs_convertencoding.h"

#include "ovbase.h"

#include "ovutil/win32.h"

#include <lua5.1/lauxlib.h>
#include <lua5.1/lualib.h>

#include "luautil.h"

NODISCARD static error to_codepage(lua_State *const L, int const idx, UINT *const cp) {
  if (!L) {
    return errg(err_invalid_arugment);
  }
  if (!cp) {
    return errg(err_null_pointer);
  }
  if (lua_isnumber(L, idx)) {
    *cp = (UINT)lua_tointeger(L, idx);
    return eok();
  }
  if (lua_isstring(L, idx)) {
    char const *const s = lua_tostring(L, idx);
    if (strcmp(s, "sjis") == 0) {
      *cp = 932;
      return eok();
    } else if (strcmp(s, "eucjp") == 0) {
      *cp = 20932;
      return eok();
    } else if (strcmp(s, "iso2022jp") == 0) {
      *cp = 50222;
      return eok();
    } else if (strcmp(s, "utf8") == 0) {
      *cp = 65001;
      return eok();
    } else if (strcmp(s, "utf16le") == 0) {
      *cp = 1200;
      return eok();
    } else if (strcmp(s, "utf16be") == 0) {
      *cp = 1201;
      return eok();
    } else if (strcmp(s, "ansi") == 0) {
      *cp = GetACP();
      return eok();
    }
  }
  return errg(err_fail);
}

NODISCARD static inline uint16_t swap16(uint16_t const x) { return ((x >> 8) & 0x00ff) | ((x << 8) & 0xff00); }

NODISCARD static error luafn_convertencoding_core(
    uint8_t const *src, size_t srclen, UINT const src_cp, UINT const dest_cp, struct str *const dest) {
  if (!src) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }

  error err = eok();
  if (srclen >= 2 &&
      ((src_cp == 1200 && src[0] == 0xff && src[1] == 0xfe) || (src_cp == 1201 && src[0] == 0xfe && src[1] == 0xfe))) {
    src += 2;
    srclen -= 2;
  } else if (srclen >= 3 && src_cp == 65001 && src[0] == 0xef && src[1] == 0xbb && src[2] == 0xbf) {
    src += 3;
    srclen -= 3;
  }

  if (src_cp == dest_cp) {
    err = scpy(dest, (LPCSTR)src);
    if (efailed(err)) {
      err = ethru(err);
      return err;
    }
    return eok();
  }

  // utf16le -> utf16be or utf16be -> utf16le
  if ((src_cp == 1200 || src_cp == 1201) && (dest_cp == 1200 || dest_cp == 1201)) {
    struct wstr tmp = {0};
    err = sncpy(&tmp, (void const *)src, srclen / 2);
    if (efailed(err)) {
      err = ethru(err);
      return err;
    }
    for (uint16_t *s = (void *)tmp.ptr, *end = (void *)(tmp.ptr + tmp.len); s < end; ++s) {
      *s = swap16(*s);
    }
    err = sfree(dest);
    if (efailed(err)) {
      ereport(sfree(&tmp));
      err = ethru(err);
      return err;
    }
    *dest = (struct str){
        .ptr = (void *)tmp.ptr,
        .len = tmp.len * 2,
        .cap = tmp.cap * 2,
    };
    return eok();
  }

  // mbcs to mbcs
  if (src_cp != 1200 && src_cp != 1201 && dest_cp != 1200 && dest_cp != 1201) {
    struct wstr tmp = {0};
    err = from_cp(src_cp, &(struct str const){.ptr = ov_deconster_(src), .len = srclen}, &tmp);
    if (efailed(err)) {
      err = ethru(err);
      return err;
    }
    err = to_cp(dest_cp, &tmp, dest);
    if (efailed(err)) {
      ereport(sfree(&tmp));
      err = ethru(err);
      return err;
    }
    ereport(sfree(&tmp));
    return eok();
  }

  // utf-16 to mbcs
  if (src_cp == 1200 || src_cp == 1201) {
    struct wstr tmp = {0};
    if (src_cp == 1200) {
      tmp = (struct wstr){
          .ptr = ov_deconster_(src),
          .len = srclen / 2,
          .cap = 0,
      };
    } else {
      err = sncpy(&tmp, (void const *)src, srclen / 2);
      if (efailed(err)) {
        err = ethru(err);
        return err;
      }
      for (uint16_t *s = (void *)tmp.ptr, *end = (void *)(tmp.ptr + tmp.len); s < end; ++s) {
        *s = swap16(*s);
      }
    }
    err = to_cp(dest_cp, &tmp, dest);
    if (efailed(err)) {
      ereport(sfree(&tmp));
      err = ethru(err);
      return err;
    }
    ereport(sfree(&tmp));
    return eok();
  }

  // mbcs to utf-16
  struct wstr tmp = {0};
  err = from_cp(src_cp, &(struct str const){.ptr = ov_deconster_(src), .len = srclen}, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  if (dest_cp == 1201) {
    for (uint16_t *s = (void *)tmp.ptr, *end = (void *)(tmp.ptr + tmp.len); s < end; ++s) {
      *s = swap16(*s);
    }
  }
  err = sfree(dest);
  if (efailed(err)) {
    ereport(sfree(&tmp));
    err = ethru(err);
    return err;
  }
  *dest = (struct str){
      .ptr = (void *)tmp.ptr,
      .len = tmp.len * 2,
      .cap = tmp.cap * 2,
  };
  return eok();
}

int luafn_convertencoding(lua_State *const L) {
  struct str tmp = {0};
  size_t srclen = 0;
  uint8_t const *const src = (uint8_t const *)lua_tolstring(L, 1, &srclen);

  UINT src_cp = 0, dest_cp = 0;
  error err = to_codepage(L, 2, &src_cp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = to_codepage(L, 3, &dest_cp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (src_cp == dest_cp) {
    lua_pop(L, 2); // re-use
    goto cleanup;
  }
  err = luafn_convertencoding_core(src, srclen, src_cp, dest_cp, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushlstring(L, tmp.ptr, tmp.len);

cleanup:
  ereport(sfree(&tmp));
  return efailed(err) ? luautil_throw(L, err) : 1;
}
