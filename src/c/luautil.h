#include "ovbase.h"
#include <lua5.1/lua.h>

void *luautil_alloc(void *const ud, void *ptr, size_t const osize, size_t const nsize);

int luautil_throw_error(lua_State *const L, error e, char const *const funcname);
#define luautil_throw(L, err) luautil_throw_error((L), (err), (__func__))

NODISCARD error luautil_push_wstr(lua_State *const L, struct wstr const *const ws);
NODISCARD error luautil_towstr(lua_State *const L, int const idx, struct wstr *const dest);

NODISCARD error luautil_pcall_(lua_State *const L, int const nargs, int const nresults ERR_FILEPOS_PARAMS);
#define luautil_pcall(L, nargs, nresults) (luautil_pcall_((L), (nargs), (nresults)ERR_FILEPOS_VALUES))

NODISCARD error luautil_call_function(lua_State *const L, int const num_params, int const num_results);

NODISCARD error luautil_add_include_path(lua_State *const L, struct wstr *const dir);
NODISCARD error luautil_require(lua_State *const L, const char *const name);
