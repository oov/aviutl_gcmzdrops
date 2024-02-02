#include "luautil.h"

#include "error_gcmz.h"
#include "i18n.h"

#include "ovutil/str.h"
#include "ovutil/win32.h"

#include <lauxlib.h>

void *luautil_alloc(void *const ud, void *ptr, size_t const osize, size_t const nsize) {
  (void)ud;
  (void)osize;
  if (nsize) {
    if (!ereport(mem(&ptr, nsize, 1))) {
      return NULL;
    }
    return ptr;
  }
  ereport(mem_free(&ptr));
  return NULL;
}

int luautil_throw_error(lua_State *const L, error e, char const *const funcname) {
  struct wstr msg = {0};
  struct wstr errmsg = {0};
  struct str s = {0};

  luaL_where(L, 1);
  if (strncmp(funcname, "luafn_", 6) == 0) {
    lua_pushstring(L, "error on GCMZDrops.");
    lua_pushstring(L, funcname + 6);
  } else {
    lua_pushstring(L, "error on ");
    lua_pushstring(L, funcname);
  }
  lua_pushstring(L, "():\r\n");
  error err = error_to_string(e, &errmsg);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scat(&msg, errmsg.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = to_mbcs(&errmsg, &s);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_pushlstring(L, s.ptr, s.len);
  lua_concat(L, 5);

cleanup:
  if (efailed(err)) {
    efree(&err);
    lua_pushstring(L, "failed to build error message");
    lua_concat(L, 5);
  }
  ereport(sfree(&msg));
  ereport(sfree(&errmsg));
  ereport(sfree(&s));
  efree(&e);
  return lua_error(L);
}

error luautil_push_wstr(lua_State *const L, struct wstr const *const ws) {
  struct str s = {0};
  error err = to_mbcs(ws, &s);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  lua_pushstring(L, s.ptr);
  err = sfree(&s);
  if (efailed(err)) {
    lua_pop(L, 1);
    err = ethru(err);
    return err;
  }
  return eok();
}

error luautil_towstr(lua_State *const L, int const idx, struct wstr *const dest) {
  if (!L) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }
  char const *const s = lua_tostring(L, idx);
  if (!s) {
    return errg(err_invalid_arugment);
  }
  error err = from_mbcs(&str_unmanaged_const(s), dest);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  return eok();
}

NODISCARD error luautil_pcall_(lua_State *const L, int const nargs, int const nresults ERR_FILEPOS_PARAMS) {
  if (lua_pcall(L, nargs, nresults, 0) == 0) {
    return eok();
  }
  struct wstr trace = {0};
  error err = luautil_towstr(L, -1, &trace);
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }
  return error_add_(NULL, err_type_gcmz, err_gcmz_lua, &trace ERR_FILEPOS_VALUES_PASSTHRU);

failed:
  ereport(sfree(&trace));
  efree(&err);
  return emsg_i18n(err_type_generic, err_fail, gettext("Failed to build error message."));
}

NODISCARD error luautil_call_function(lua_State *const L, int const num_params, int const num_returns) {
  if (!L) {
    return errg(err_invalid_arugment);
  }
  error err = luautil_pcall(L, num_params, num_returns);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

NODISCARD error luautil_add_include_path(lua_State *const L, struct wstr *const dir) {
  if (!L || !dir) {
    return errg(err_invalid_arugment);
  }
  struct wstr tmp = {0};
  lua_getglobal(L, "package");
  error err = scpy(&tmp, dir->ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = include_trailing_path_delimiter(&tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scat(&tmp, L"?.lua;");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = luautil_push_wstr(L, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_getfield(L, -2, "path");
  lua_concat(L, 2);
  lua_setfield(L, -2, "path");
  tmp.ptr[tmp.len - 4] = L'd';
  tmp.ptr[tmp.len - 3] = L'l';
  tmp.ptr[tmp.len - 2] = L'l';
  err = luautil_push_wstr(L, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_getfield(L, -2, "cpath");
  lua_concat(L, 2);
  lua_setfield(L, -2, "cpath");
  lua_pop(L, 1);

cleanup:
  ereport(sfree(&tmp));
  return err;
}

NODISCARD error luautil_require(lua_State *const L, const char *const name) {
  if (!L || !name) {
    return errg(err_invalid_arugment);
  }
  lua_getglobal(L, "require");
  lua_pushstring(L, name);
  error err = luautil_pcall(L, 1, 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}
