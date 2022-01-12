#pragma once

#include <stdbool.h>
#include <stdint.h>

#include <lua5.1/lua.h>

#include "ovbase.h"

#include "files.h"
#include "scpopup.h"

struct lua {
  lua_State *L;
};

NODISCARD error lua_init(struct lua *const l);
NODISCARD error lua_exit(struct lua *const l);
NODISCARD error lua_dropper_init(struct lua *const l);
NODISCARD error lua_dropper_build_menu(struct lua *const l, struct scpopup_menu *const m);
NODISCARD error lua_dropper_select(struct lua *const l, HWND const window, POINT const pt, UINT_PTR const selected);
NODISCARD error lua_call_on_drag_enter(struct lua *const l,
                                       struct files const *const f,
                                       POINTL const point,
                                       DWORD const key_state);
NODISCARD error lua_call_on_drag_over(struct lua *const l,
                                      struct files const *const f,
                                      POINTL const point,
                                      DWORD const key_state);
NODISCARD error lua_call_on_drag_leave(struct lua *const l);
NODISCARD error lua_call_on_drop(struct lua *const l,
                                 struct files const *const f,
                                 POINTL const point,
                                 DWORD const key_state,
                                 int const frame_advance);
NODISCARD error lua_call_on_drop_simulated(struct lua *const l,
                                           struct files const *const f,
                                           POINTL const point,
                                           DWORD const key_state,
                                           int const frame_advance);
