#pragma once

#include <stdint.h>
#include <stdbool.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <lua5.1/lua.h>

#include "3rd/base.c/base.h"
#include "files.h"

NODISCARD error luafn_push_wstr(lua_State *const L, struct wstr const *const ws);
NODISCARD error luafn_towstr(lua_State *const L, int const idx, struct wstr *const dest);

NODISCARD error luafn_push_files(lua_State *const L, struct files const *const f);
NODISCARD error luafn_push_state(lua_State *const L, POINTL const point, DWORD const key_state);

void luafn_register_funcs(lua_State *const L);
