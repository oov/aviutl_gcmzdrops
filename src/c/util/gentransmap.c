#include "ovbase.h"
#include "ovnum.h"
#include "ovutil/win32.h"

#include "../error_gcmz.h"
#include "../luafuncs_convertencoding.h"
#include "../luautil.h"

#include <lua.h>
#include <lualib.h>
#include <stdio.h>

static int luafn_output(lua_State *const L) {
  fputs(lua_tostring(L, 1), stdout);
  return 0;
}

int main(int argc, char const *argv[]) {
  if (argc != 6) {
    fputs("gentransmap exo_from_original original_code_page exo_from_patched_env patched_env_code_page\n", stderr);
    return 1;
  }
  ov_init();
  error_set_message_mapper(gcmz_error_message);

  lua_State *L = NULL;
  error err = eok();

  char const *const fn1 = argv[1];
  int cp1 = 0;
  {
    int64_t x;
    if (!ov_atoi(argv[2], &x, true)) {
      err = errg(err_fail);
      goto cleanup;
    }
    cp1 = (int)x;
  }
  char const *const fn2 = argv[3];
  int cp2 = 0;
  {
    int64_t x;
    if (!ov_atoi(argv[4], &x, true)) {
      err = errg(err_fail);
      goto cleanup;
    }
    cp2 = (int)x;
  }
  L = lua_newstate(luautil_alloc, NULL);
  if (!L) {
    err = errg(err_out_of_memory);
    goto cleanup;
  }
  luaL_openlibs(L);
  lua_pushcfunction(L, luafn_output);
  lua_setglobal(L, "output");
  lua_pushcfunction(L, luafn_convertencoding);
  lua_setglobal(L, "convertencoding");
  err = luautil_require(L, "gentransmap");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  lua_getfield(L, 1, "main");
  if (!lua_isfunction(L, -1)) {
    err = errg(err_unexpected);
    goto cleanup;
  }
  lua_pushstring(L, fn1);
  lua_pushinteger(L, cp1);
  lua_pushstring(L, fn2);
  lua_pushinteger(L, cp2);
  lua_pushstring(L, argv[5]);
  err = luautil_call_function(L, 5, 0);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  if (L) {
    lua_close(L);
    L = NULL;
  }
  if (efailed(err)) {
    ereport(err);
    ov_exit();
    return 1;
  }
  ov_exit();
  return 0;
}
