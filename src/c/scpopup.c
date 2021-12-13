#include "scpopup.h"

#include <commctrl.h>

error scpopup_show_popup(HWND const window, POINT const pt, struct scpopup_menu *const m, UINT_PTR *const selected)
{
  if (!window || !m)
  {
    return errg(err_invalid_arugment);
  }
  if (!selected)
  {
    return errg(err_null_pointer);
  }
  error err = eok();
  HMENU h = NULL, hh = NULL;
  h = CreatePopupMenu();
  if (!h)
  {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  for (size_t i = 0, mlen = m->len; i < mlen; ++i)
  {
    struct scpopup_menu_item *mi = m->ptr + i;
    if (!mi->len)
    {
      if (!AppendMenuW(h, MF_ENABLED | MF_STRING, mi->id, mi->caption.ptr))
      {
        err = errhr(HRESULT_FROM_WIN32(GetLastError()));
        goto cleanup;
      }
      continue;
    }
    hh = CreatePopupMenu();
    if (!hh)
    {
      err = errhr(HRESULT_FROM_WIN32(GetLastError()));
      goto cleanup;
    }
    for (size_t j = 0, milen = mi->len; j < milen; ++j)
    {
      struct scpopup_menu_sub_item *msi = mi->ptr + j;
      if (!AppendMenuW(hh, MF_ENABLED | MF_STRING, msi->id, msi->caption.ptr))
      {
        err = errhr(HRESULT_FROM_WIN32(GetLastError()));
        goto cleanup;
      }
    }
    if (!AppendMenuW(h, MF_ENABLED | MF_STRING | MF_POPUP, (UINT_PTR)hh, mi->caption.ptr))
    {
      err = errhr(HRESULT_FROM_WIN32(GetLastError()));
      goto cleanup;
    }
    hh = NULL;
  }
  WINBOOL r = TrackPopupMenu(h, TPM_TOPALIGN | TPM_LEFTALIGN | TPM_RETURNCMD | TPM_RIGHTBUTTON, pt.x, pt.y, 0, window, NULL);
  if (!r)
  {
    HRESULT hr = HRESULT_FROM_WIN32(GetLastError());
    if (hr != HRESULT_FROM_WIN32(ERROR_SUCCESS))
    {
      err = errhr(hr);
      goto cleanup;
    }
  }
  *selected = (UINT_PTR)r;

cleanup:
  if (h)
  {
    DestroyMenu(h);
    h = NULL;
  }
  if (hh)
  {
    DestroyMenu(hh);
    hh = NULL;
  }
  return err;
}

static LRESULT WINAPI window_proc(HWND const window, UINT const message, WPARAM const wparam, LPARAM const lparam, UINT_PTR const uid_subclass, DWORD_PTR const ref_data)
{
  struct scpopup *p = (void *)ref_data;
  (void)uid_subclass;
  if (p->popup)
  {
    if (
        (message == WM_MBUTTONDOWN) ||
        (message == WM_RBUTTONDOWN && (wparam & (MK_SHIFT | MK_CONTROL)) == (MK_SHIFT | MK_CONTROL)))
    {
      return p->popup(p, (POINT){.x = lparam & 0xffff, .y = (lparam >> 16) & 0xffff});
    }
  }
  return DefSubclassProc(window, message, wparam, lparam);
}

error scpopup_init(struct scpopup *const p, HWND const window)
{
  if (!p || !window)
  {
    return errg(err_invalid_arugment);
  }
  if (!SetWindowSubclass(window, window_proc, (UINT_PTR)window_proc, (DWORD_PTR)p))
  {
    return errg(err_fail);
  }
  p->window = window;
  return eok();
}

error scpopup_exit(struct scpopup *const p)
{
  if (!p)
  {
    return errg(err_invalid_arugment);
  }
  if (!RemoveWindowSubclass(p->window, window_proc, (UINT_PTR)window_proc))
  {
    return errg(err_fail);
  }
  p->window = NULL;
  return eok();
}

error scpopup_menu_free(struct scpopup_menu *const m)
{
  if (!m)
  {
    return errg(err_invalid_arugment);
  }
  for (size_t i = 0, mlen = m->len; i < mlen; ++i)
  {
    struct scpopup_menu_item *mi = m->ptr + i;
    for (size_t j = 0, milen = mi->len; j < milen; ++j)
    {
      struct scpopup_menu_sub_item *msi = mi->ptr + j;
      ereport(sfree(&msi->caption));
    }
    ereport(sfree(&mi->caption));
    ereport(afree(m->ptr + i));
  }
  ereport(afree(m));
  return eok();
}
