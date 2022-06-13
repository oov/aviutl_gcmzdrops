#include "exoloadhook.h"

#include "aviutl.h"
#include "files.h"
#include "lua.h"
#include "task.h"

static struct lua g_lua = {0};

static void cleanup(void *const userdata) { files_cleanup(userdata == NULL); }

NODISCARD static error
on_exo_load(char const *const src_filepath, struct str *const dest_filepath, bool const is_exa, void *userdata) {
  (void)userdata;
  error err = eok();
  lua_State *L = g_lua.L;
  if (!L) {
    return errg(err_unexpected);
  }
  lua_getfield(L, 1, "on_load");
  if (!lua_isfunction(L, -1)) {
    err = errg(err_unexpected);
    goto cleanup;
  }
  lua_pushstring(L, src_filepath);
  lua_pushboolean(L, is_exa ? 1 : 0);
  err = lua_call_function(&g_lua, 2, 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (!lua_isstring(L, -1)) {
    err = errg(err_unexpected);
    goto cleanup;
  }
  err = scpy(dest_filepath, lua_tostring(L, -1));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  lua_settop(L, 1);
  task_add(cleanup, efailed(err) ? NULL : (void *)(1));
  return err;
}

NODISCARD error exoloadhook_create(void) {
  error err = lua_init(&g_lua, false);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = lua_require(&g_lua, "_exoloadhook");
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = aviutl_set_exo_load_hook(on_exo_load, NULL);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

NODISCARD error exoloadhook_destroy(void) {
  ereport(lua_exit(&g_lua));
  return eok();
}
