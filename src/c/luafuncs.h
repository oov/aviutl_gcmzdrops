#pragma once

#include <stdbool.h>
#include <stdint.h>
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#include <lua.h>

#include "ovbase.h"

#include "files.h"

NODISCARD error luafn_push_files(lua_State *const L, struct files const *const f);
NODISCARD error luafn_push_state(lua_State *const L, POINTL const point, DWORD const key_state);

void luafn_register_funcs(lua_State *const L);
