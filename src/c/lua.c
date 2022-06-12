#include "lua.h"

#include "ovutil/str.h"

#include <lua5.1/lauxlib.h>
#include <lua5.1/lualib.h>

#include "error_gcmz.h"
#include "gcmzfuncs.h"
#include "luafuncs.h"

static void *lua_alloc(void *const ud, void *ptr, size_t const osize, size_t const nsize) {
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

NODISCARD static error pcall_(lua_State *const L, int const nargs, int const nresults ERR_FILEPOS_PARAMS) {
  if (lua_pcall(L, nargs, nresults, 0) == 0) {
    return eok();
  }
  struct wstr trace = {0};
  error err = luafn_towstr(L, -1, &trace);
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }
  return error_add_(NULL, err_type_gcmz, err_gcmz_lua, &trace ERR_FILEPOS_VALUES_PASSTHRU);

failed:
  ereport(sfree(&trace));
  efree(&err);
  return emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("エラーメッセージの組み立てに失敗しました。")));
}
#define pcall(L, nargs, nresults) (pcall_((L), (nargs), (nresults)ERR_FILEPOS_VALUES))

NODISCARD static error add_include_path(lua_State *const L, struct wstr *const dir) {
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
  err = luafn_push_wstr(L, &tmp);
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
  err = luafn_push_wstr(L, &tmp);
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

NODISCARD static error push_files(lua_State *const L, struct wstr *const pattern) {
  if (!L || !pattern) {
    return errg(err_invalid_arugment);
  }

  lua_newtable(L);
  int table_index = 0;
  error err = eok();
  WIN32_FIND_DATAW fd = {0};
  HANDLE h = FindFirstFileW(pattern->ptr, &fd);
  if (h == INVALID_HANDLE_VALUE) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  do {
    if (wcscmp(fd.cFileName, L".") == 0 || wcscmp(fd.cFileName, L"..") == 0 ||
        wcscmp(fd.cFileName, L"_entrypoint.lua") == 0 ||
        (fd.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) == FILE_ATTRIBUTE_DIRECTORY) {
      continue;
    }
    size_t extpos = 0;
    struct wstr ws = wstr_unmanaged(fd.cFileName);
    err = extract_file_extension(&ws, &extpos);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    ws.len = extpos;
    ws.ptr[extpos] = L'\0';
    err = luafn_push_wstr(L, &ws);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    lua_rawseti(L, -2, ++table_index);
  } while (FindNextFileW(h, &fd));
cleanup:
  if (h != INVALID_HANDLE_VALUE) {
    FindClose(h);
    h = INVALID_HANDLE_VALUE;
  }
  return err;
}

NODISCARD static error execute_entrypoint(struct lua *const l, struct wstr *const dir) {
  if (!l || !dir) {
    return errg(err_invalid_arugment);
  }

  struct wstr pattern = {0};
  lua_State *const L = l->L;

  lua_getglobal(L, "require");
  lua_pushstring(L, "_entrypoint");
  error err = pcall(L, 1, 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (!lua_istable(L, -1)) {
    err = errg(err_unexpected);
    goto cleanup;
  }

  lua_getfield(L, -1, "init");

  err = scpy(&pattern, dir->ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = include_trailing_path_delimiter(&pattern);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scat(&pattern, L"*.lua");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = push_files(L, &pattern);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = pcall(L, 1, 0);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&pattern));
  return err;
}

error lua_init(struct lua *const l, bool const call_entrypoint) {
  if (!l) {
    return errg(err_invalid_arugment);
  }
  if (l->L) {
    return errg(err_unexpected);
  }

  struct wstr dir = {0};
  error err = eok();
  l->L = lua_newstate(lua_alloc, NULL);
  if (!l->L) {
    err = errg(err_out_of_memory);
    goto cleanup;
  }

  luaL_openlibs(l->L);
  luafn_register_funcs(l->L);

  err = gcmz_get_script_dir(&dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = add_include_path(l->L, &dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (call_entrypoint) {
    err = execute_entrypoint(l, &dir);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
cleanup:
  ereport(sfree(&dir));
  if (efailed(err) && l->L) {
    lua_close(l->L);
    l->L = NULL;
  }
  return err;
}

error lua_exit(struct lua *const l) {
  if (!l) {
    return errg(err_invalid_arugment);
  }
  if (l->L) {
    lua_close(l->L);
    l->L = NULL;
  }
  return eok();
}

error lua_dropper_init(struct lua *const l) {
  if (!l) {
    return errg(err_invalid_arugment);
  }

  struct wstr dir = {0};
  lua_State *const L = l->L;
  error err = gcmz_get_script_dir(&dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = include_trailing_path_delimiter(&dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scat(&dir, L"dropper");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = add_include_path(L, &dir);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_getfield(L, 1, "initdropper");
  err = include_trailing_path_delimiter(&dir);
  if (efailed(err)) {
    lua_pop(L, 1);
    err = ethru(err);
    goto cleanup;
  }
  err = scat(&dir, L"*.lua");
  if (efailed(err)) {
    lua_pop(L, 1);
    err = ethru(err);
    goto cleanup;
  }
  err = push_files(L, &dir);
  if (efailed(err)) {
    lua_pop(L, 1);
    err = ethru(err);
    goto cleanup;
  }
  err = pcall(L, 1, 0);
  if (efailed(err)) {
    lua_pop(L, 1);
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&dir));
  return err;
}

error lua_dropper_build_menu(struct lua *const l, struct scpopup_menu *const menu) {
  if (!l || !l->L) {
    return errg(err_invalid_arugment);
  }
  if (!menu) {
    return errg(err_null_pointer);
  }

  lua_State *const L = l->L;
  lua_getfield(L, 1, "getdroppermenuitems");
  error err = pcall(L, 0, 1);
  struct wstr ws = {0};
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  int const n = (int)lua_objlen(L, -1);
  for (int i = 1; i <= n; ++i) {
    lua_rawgeti(L, -1, i);
    if (lua_isboolean(L, -1)) {
      lua_pop(L, 1);
      continue;
    }
    if (lua_isstring(L, -1)) {
      err = luafn_towstr(L, -1, &ws);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      lua_pop(L, 1);
      err = apush(menu,
                  ((struct scpopup_menu_item){
                      .id = (UINT_PTR)MAKELONG(i, 0),
                      .caption = ws,
                  }));
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      ws = (struct wstr){0};
      continue;
    }
    if (lua_istable(L, -1)) {
      int const m = (int)lua_objlen(L, -1);
      if (m == 0) {
        lua_pop(L, 1);
        continue;
      }
      lua_getfield(L, -1, "name");
      err = luafn_towstr(L, -1, &ws);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      lua_pop(L, 1);
      err = apush(menu,
                  ((struct scpopup_menu_item){
                      .id = (UINT_PTR)MAKELONG(i, 0),
                      .caption = ws,
                  }));
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      struct scpopup_menu_item *menu_item = menu->ptr + menu->len - 1;
      ws = (struct wstr){0};
      for (int j = 1; j <= m; ++j) {
        lua_rawgeti(L, -1, j);
        err = luafn_towstr(L, -1, &ws);
        if (efailed(err)) {
          err = ethru(err);
          goto cleanup;
        }
        lua_pop(L, 1);
        err = apush(menu_item,
                    ((struct scpopup_menu_sub_item){
                        .id = (UINT_PTR)MAKELONG(i, j),
                        .caption = ws,
                    }));
        if (efailed(err)) {
          err = ethru(err);
          goto cleanup;
        }
        ws = (struct wstr){0};
      }
      lua_pop(L, 1);
      continue;
    }
    err = errg(err_unexpected);
    goto cleanup;
  }
  lua_pop(L, 1);

cleanup:
  if (efailed(err)) {
    ereport(scpopup_menu_free(menu));
  }
  ereport(sfree(&ws));
  return err;
}

error lua_dropper_select(struct lua *const l, HWND const window, POINT const pt, UINT_PTR const selected) {
  if (!l || !l->L) {
    return errg(err_invalid_arugment);
  }

  lua_State *const L = l->L;
  lua_getfield(L, 1, "selectdropper");
  if (!lua_isfunction(L, -1)) {
    lua_pop(L, 1);
    return errg(err_unexpected);
  }
  lua_pushinteger(L, LOWORD(selected));
  lua_pushinteger(L, HIWORD(selected));
  lua_newtable(L);
  lua_pushinteger(L, pt.x);
  lua_setfield(L, -2, "x");
  lua_pushinteger(L, pt.y);
  lua_setfield(L, -2, "y");
  lua_pushinteger(L, (lua_Integer)window);
  lua_setfield(L, -2, "parent");
  error err = pcall(L, 3, 2);
  if (efailed(err)) {
    return err;
  }
  return eok();
}

error lua_call_on_drag_enter(struct lua *const l,
                             struct files const *const f,
                             POINTL const point,
                             DWORD const key_state) {
  if (!l || !l->L || !f) {
    return errg(err_invalid_arugment);
  }

  lua_State *const L = l->L;
  lua_getfield(L, 1, "ondragenter");
  error err = luafn_push_files(L, f);
  if (efailed(err)) {
    lua_pop(L, 1);
    err = ethru(err);
    return err;
  }
  err = luafn_push_state(L, point, key_state);
  if (efailed(err)) {
    lua_pop(L, 2);
    err = ethru(err);
    return err;
  }
  err = pcall(L, 2, 1);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  int r = lua_toboolean(L, -1);
  lua_pop(L, 1);
  if (!r) {
    return errg(err_abort);
  }
  return eok();
}

error lua_call_on_drag_over(struct lua *const l,
                            struct files const *const f,
                            POINTL const point,
                            DWORD const key_state) {
  if (!l || !l->L || !f) {
    return errg(err_invalid_arugment);
  }

  lua_State *const L = l->L;
  lua_getfield(L, 1, "ondragover");
  error err = luafn_push_files(L, f);
  if (efailed(err)) {
    lua_pop(L, 1);
    err = ethru(err);
    return err;
  }
  err = luafn_push_state(L, point, key_state);
  if (efailed(err)) {
    lua_pop(L, 2);
    err = ethru(err);
    return err;
  }
  err = pcall(L, 2, 1);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  int const r = lua_toboolean(L, -1);
  lua_pop(L, 1);
  if (!r) {
    return errg(err_abort);
  }
  return eok();
}

error lua_call_on_drag_leave(struct lua *const l) {
  if (!l || !l->L) {
    return errg(err_invalid_arugment);
  }

  lua_State *const L = l->L;
  lua_getfield(L, 1, "ondragleave");
  error err = pcall(L, 0, 0);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  return eok();
}

error lua_call_on_drop(struct lua *const l,
                       struct files const *const f,
                       POINTL const point,
                       DWORD const key_state,
                       int const frame_advance) {
  if (!l || !l->L || !f) {
    return errg(err_invalid_arugment);
  }

  lua_State *const L = l->L;
  lua_getfield(L, 1, "ondrop");
  error err = luafn_push_files(L, f);
  if (efailed(err)) {
    lua_pop(L, 1);
    err = ethru(err);
    return err;
  }
  err = luafn_push_state(L, point, key_state);
  if (efailed(err)) {
    lua_pop(L, 2);
    err = ethru(err);
    return err;
  }
  lua_pushinteger(L, frame_advance);
  lua_setfield(L, -2, "frameadvance");
  err = pcall(L, 2, 1);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  int r = lua_toboolean(L, -1);
  lua_pop(L, 1);
  if (!r) {
    return errg(err_abort);
  }
  return eok();
}

error lua_call_on_drop_simulated(struct lua *const l,
                                 struct files const *const f,
                                 POINTL const point,
                                 DWORD const key_state,
                                 int const frame_advance) {
  if (!l || !l->L || !f) {
    return errg(err_invalid_arugment);
  }

  lua_State *const L = l->L;
  lua_getfield(L, 1, "ondropsimulated");
  error err = luafn_push_files(L, f);
  if (efailed(err)) {
    lua_pop(L, 1);
    err = ethru(err);
    return err;
  }
  err = luafn_push_state(L, point, key_state);
  if (efailed(err)) {
    lua_pop(L, 2);
    err = ethru(err);
    return err;
  }
  lua_pushinteger(L, frame_advance);
  lua_setfield(L, -2, "frameadvance");
  err = pcall(L, 2, 1);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  int r = lua_toboolean(L, -1);
  lua_pop(L, 1);
  if (!r) {
    return errg(err_abort);
  }
  return eok();
}
