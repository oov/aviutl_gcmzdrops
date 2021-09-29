#pragma once

#include <stdint.h>
#include <stdbool.h>

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

typedef void (*task_func)(void *const userdata);
void task_init(HWND const window);
void task_add(task_func const func, void *const userdata);
bool task_process(HWND const window, UINT const message, WPARAM const wparam, LPARAM const lparam);
