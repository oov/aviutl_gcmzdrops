#include "task.h"

static HWND g_window = NULL;
static UINT g_message_id = 0;

void task_init(HWND const window)
{
  g_window = window;
  g_message_id = RegisterWindowMessageW(L"GCMZDropsTask");
}

void task_add(task_func const func, void *const userdata)
{
  PostMessage(g_window, g_message_id, (WPARAM)func, (LPARAM)userdata);
}

bool task_process(HWND const window, UINT const message, WPARAM const wparam, LPARAM const lparam)
{
  (void)window;
  if (message != g_message_id)
  {
    return false;
  }
  ((task_func)wparam)((void *)lparam);
  return true;
}
