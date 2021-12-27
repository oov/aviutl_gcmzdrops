#include "droptarget.h"

#include <shlobj.h>

#include "datauri.h"
#include "sniffer.h"
#include "util.h"

static IDropTargetVtbl g_drop_target_vtable;

static HRESULT STDMETHODCALLTYPE QueryInterface(struct IDropTarget *This, REFIID rid, void **ppvObject) {
  struct drop_target *const this = (void *)This;
  if (!this || this->super.lpVtbl != &g_drop_target_vtable || !rid || !ppvObject) {
    return E_INVALIDARG;
  }
  if (memcmp(rid, &IID_IUnknown, sizeof(IID)) == 0 || memcmp(rid, &IID_IDropTarget, sizeof(IID)) == 0) {
    this->super.lpVtbl->AddRef(This);
    *ppvObject = This;
    return S_OK;
  }
  return E_NOINTERFACE;
}

static ULONG STDMETHODCALLTYPE AddRef(struct IDropTarget *This) {
  struct drop_target *const this = (void *)This;
  if (!this || this->super.lpVtbl != &g_drop_target_vtable) {
    return 1;
  }
  return (ULONG)InterlockedIncrement(&this->refcount);
}

static ULONG STDMETHODCALLTYPE Release(struct IDropTarget *This) {
  struct drop_target *const this = (void *)This;
  if (!this || this->super.lpVtbl != &g_drop_target_vtable) {
    return 1;
  }
  LONG r = InterlockedDecrement(&this->refcount);
  if (r == 0) {
    CoTaskMemRealloc(this, 0);
  }
  return (ULONG)r;
}

NODISCARD static error
get_data(STGMEDIUM *const sm, IDataObject *const dataobj, CLIPFORMAT const format, LONG const index) {
  if (!sm || !dataobj) {
    return errg(err_invalid_arugment);
  }
  FORMATETC etc = {0};
  etc.cfFormat = format;
  etc.ptd = NULL;
  etc.dwAspect = DVASPECT_CONTENT;
  etc.lindex = index;
  etc.tymed = TYMED_HGLOBAL;
  HRESULT hr = dataobj->lpVtbl->GetData(dataobj, &etc, sm);
  if (FAILED(hr)) {
    return errhr(hr);
  }
  return eok();
}

NODISCARD static error read_from_hglobal(HGLOBAL const h, void **const data, size_t *const len) {
  if (!h) {
    return errg(err_invalid_arugment);
  }
  if (!data || !len) {
    return errg(err_null_pointer);
  }
  error err = eok();
  void *p = GlobalLock(h);
  if (!p) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  SIZE_T sz = GlobalSize(h);
  if (sz == 0) {
    err = errhr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  void *r = NULL;
  err = mem(&r, sz, sizeof(BYTE));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  memcpy(r, p, sz);
  *data = r;
  *len = sz;

cleanup:
  if (p) {
    GlobalUnlock(h);
    p = NULL;
  }
  return err;
}

NODISCARD static error read_from_istream(IStream *const st, void **const data, size_t *const len) {
  if (!st) {
    return errg(err_invalid_arugment);
  }
  if (!data || !len) {
    return errg(err_null_pointer);
  }

  enum {
    bufsize = 4096,
  };
  error err = eok();
  char *r = NULL;
  size_t sz = 0;
  char buf[bufsize] = {0};
  ULONG read = 0;
  HRESULT hr = 0;
  for (;;) {
    hr = st->lpVtbl->Read(st, buf, bufsize, &read);
    if (FAILED(hr)) {
      err = errhr(hr);
      goto cleanup;
    }
    if (hr == S_FALSE) {
      break;
    }
    err = mem(&r, sz + read, sizeof(BYTE));
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    memcpy(r + sz, buf, read);
    sz += read;
    if (read < bufsize) {
      break;
    }
  }
  *data = r;
  r = NULL;
  *len = sz;

cleanup:
  ereport(mem_free(&r));
  return err;
}

NODISCARD static error read_from_stgmedium(STGMEDIUM *const sm, void **const data, size_t *const len) {
  if (!sm) {
    return errg(err_invalid_arugment);
  }
  if (!data || !len) {
    return errg(err_null_pointer);
  }

  error err = eok();
  switch (sm->tymed) {
  case TYMED_HGLOBAL:
    err = read_from_hglobal(sm->hGlobal, data, len);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    break;
  case TYMED_ISTREAM:
    err = read_from_istream(sm->pstm, data, len);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    break;
  default:
    err = errg(err_not_implemented_yet);
    goto cleanup;
  }

cleanup:
  return err;
}

NODISCARD static error read_custom_format(
    IDataObject *const dataobj, CLIPFORMAT const format, int const index, void **const p, size_t *const plen) {
  if (!dataobj) {
    return errg(err_invalid_arugment);
  }
  if (!p || !plen) {
    return errg(err_null_pointer);
  }

  STGMEDIUM sm = {0};
  void *ptr = NULL;
  size_t sz = 0;
  error err = get_data(&sm, dataobj, format, index);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read_from_stgmedium(&sm, &ptr, &sz);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *p = ptr;
  *plen = sz;

cleanup:
  if (sm.tymed != TYMED_NULL) {
    ReleaseStgMedium(&sm);
  }
  return err;
}

NODISCARD static error read_text(IDataObject *const dataobj, struct wstr *const dest) {
  if (!dataobj) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }

  void *ptr = NULL;
  size_t sz = 0;
  error err = read_custom_format(dataobj, CF_UNICODETEXT, -1, &ptr, &sz);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = scpy(dest, ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(mem_free(&ptr));
  return err;
}

NODISCARD static error files_add_temp_file(struct files *const files,
                                           wchar_t const *const base_filename,
                                           wchar_t const *const ext,
                                           wchar_t const *const mime,
                                           void *const data,
                                           size_t const datalen) {
  struct wstr tmp = {0};
  bool created = false;
  error err = create_unique_temp_file(base_filename, ext, data, datalen, &tmp);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = files_add(files, &tmp, &wstr_unmanaged_const(mime));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  files->ptr[files->len - 1].temporary = true;
  created = true;

cleanup:
  if (efailed(err) && created) {
    ereportmsg(delete_file(&tmp), &native_unmanaged(NSTR("一時ファイルの削除に失敗しました。")));
  }
  ereport(sfree(&tmp));
  return err;
}

NODISCARD static error files_add_text_file(struct files *const files, struct wstr *const text) {
  struct str s = {0};
  error err = to_mbcs(text, &s);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  wchar_t const *const mime = GetACP() == 932 ? L"text/plain; charset=Shift_JIS" : L"text/plain";
  err = files_add_temp_file(files, L"temp", L".txt", mime, s.ptr, s.len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&s));
  return err;
}

NODISCARD static error try_parse_data_uri(struct wstr *const src, struct files *const files) {
  if (!src || !files) {
    return errg(err_invalid_arugment);
  }

  struct data_uri d = {0};
  struct wstr filename = {0};
  struct wstr ext = {0};
  struct wstr mime = {0};
  error err = data_uri_parse(src->ptr, src->len, &d);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = data_uri_decode(&d);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = data_uri_suggest_filename(&d, &filename);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  ptrdiff_t extpos = 0;
  err = extract_file_extension(&filename, &extpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scpy(&ext, filename.ptr + extpos);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  filename.ptr[extpos] = L'\0';
  filename.len = (size_t)extpos;

  err = data_uri_get_mime(&d, &mime);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = files_add_temp_file(files, filename.ptr, ext.ptr, mime.ptr, d.decoded, d.decoded_len);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&filename));
  ereport(sfree(&ext));
  ereport(sfree(&mime));
  ereport(data_uri_free(&d));
  return err;
}

NODISCARD static error try_read_from_custom_format(IDataObject *const dataobj,
                                                   WORD const format,
                                                   wchar_t const *const mime,
                                                   wchar_t const *const ext,
                                                   struct files *const files) {
  if (!dataobj || !mime || !ext || !files || wcslen(ext) > 6) {
    return errg(err_invalid_arugment);
  }

  void *p = NULL;
  size_t plen = 0;
  error err = read_custom_format(dataobj, format, -1, &p, &plen);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = files_add_temp_file(files, L"clipboard", ext, mime, p, plen);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(mem_free(&p));
  return err;
}

NODISCARD static error get_file_group_descriptor(IDataObject *const dataobj, FILEGROUPDESCRIPTORW **const r) {
  STGMEDIUM sm = {0};
  FILEGROUPDESCRIPTORW *fgd = NULL;
  size_t datalen = 0;
  error err = get_data(&sm, dataobj, (CLIPFORMAT)RegisterClipboardFormatW(L"FileGroupDescriptorW"), -1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = read_from_stgmedium(&sm, (void **)&fgd, &datalen);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  *r = fgd;

cleanup:
  if (sm.tymed != TYMED_NULL) {
    ReleaseStgMedium(&sm);
  }
  return err;
}

NODISCARD static error try_read_from_file_contents(IDataObject *const dataobj, struct files *const files) {
  struct wstr ext = {0};
  int added = 0;
  FILEGROUPDESCRIPTORW *fgd = NULL;
  void *p = NULL;
  error err = get_file_group_descriptor(dataobj, &fgd);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  UINT const n = fgd->cItems;
  for (UINT i = 0; i < n; ++i) {
    FILEDESCRIPTORW *const fd = fgd->fgd + i;
    ptrdiff_t fnpos = 0;
    err = extract_file_name(&wstr_unmanaged_const(fd->cFileName), &fnpos);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    struct wstr filename = wstr_unmanaged(fd->cFileName + fnpos);
    err = sanitize(&filename);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    ptrdiff_t extpos = 0;
    err = extract_file_extension(&filename, &extpos);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = scpy(&ext, filename.ptr + extpos);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    filename.ptr[extpos] = L'\0';
    filename.len = (size_t)extpos;

    size_t plen = 0;
    err = read_custom_format(dataobj, (CLIPFORMAT)RegisterClipboardFormatW(L"FileContents"), (int)i, &p, &plen);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }

    wchar_t const *mime = NULL;
    err = sniff(p, plen, &mime, NULL);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = files_add_temp_file(files, filename.ptr, ext.ptr, mime, p, plen);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    ++added;
    ereport(mem_free(&p));
  }

cleanup:
  if (efailed(err)) {
    for (int i = 0; i < added; ++i) {
      ereport(files_delete_last(files, false));
    }
  }
  ereport(sfree(&ext));
  ereport(mem_free(&p));
  ereport(mem_free(&fgd));
  return err;
}

NODISCARD static error try_read_from_hdrop(IDataObject *const dataobj, struct files *const files) {
  int added = 0;
  STGMEDIUM sm = {0};
  DROPFILES *df = NULL;
  size_t dflen = 0;
  struct wstr ws = {0};
  error err = get_data(&sm, dataobj, CF_HDROP, -1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = read_from_stgmedium(&sm, (void **)&df, &dflen);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  if (df->fWide) {
    wchar_t const *s = (void const *)((char const *)(df) + df->pFiles);
    while (*s != L'\0') {
      err = files_add(
          files, &wstr_unmanaged_const(s), &wstr_unmanaged_const(L"application/octet-stream")); // TODO: use sniff
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      s += wcslen(s) + 1;
      ++added;
    }
    goto cleanup;
  }

  char const *s = (char const *)(df) + df->pFiles;
  while (*s != L'\0') {
    struct str const mbcs = str_unmanaged_const(s);
    ws = (struct wstr){0};
    err = from_mbcs(&mbcs, &ws);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = files_add(files, &ws, &wstr_unmanaged(L"application/octet-stream")); // TODO: use sniff
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    ++added;
    err = sfree(&ws);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    s += mbcs.len + 1;
  }

cleanup:
  if (efailed(err)) {
    for (int i = 0; i < added; ++i) {
      ereport(files_delete_last(files, false));
    }
  }
  if (sm.tymed != TYMED_NULL) {
    ReleaseStgMedium(&sm);
  }
  ereport(mem_free(&df));
  ereport(sfree(&ws));
  return err;
}

NODISCARD static error try_read_from_dib(IDataObject *const dataobj, struct files *const files) {
  char *p = NULL;
  size_t plen = 0;
  error err = read_custom_format(dataobj, CF_DIB, -1, (void **)&p, &plen);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = mem(&p, plen + sizeof(BITMAPFILEHEADER), 1);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  memmove(p + sizeof(BITMAPFILEHEADER), p, plen);
  plen += sizeof(BITMAPFILEHEADER);

  BITMAPINFOHEADER *const bih = (BITMAPINFOHEADER *)(void *)(p + sizeof(BITMAPFILEHEADER));
  BITMAPFILEHEADER *const bfh = (BITMAPFILEHEADER *)(void *)(p);
  bfh->bfType = 0x4d42;
  bfh->bfSize = sizeof(BITMAPFILEHEADER) + plen;
  bfh->bfReserved1 = 0;
  bfh->bfReserved2 = 0;
  bfh->bfOffBits = sizeof(BITMAPFILEHEADER) + bih->biSize;
  switch (bih->biBitCount) {
  case 1:
  case 4:
  case 8:
    bfh->bfOffBits += (bih->biClrUsed ? bih->biClrUsed : 1 << bih->biBitCount) * sizeof(RGBQUAD);
    break;
  case 16:
  case 24:
  case 32:
    bfh->bfOffBits += bih->biClrUsed * sizeof(RGBQUAD);
    if (bih->biCompression == BI_BITFIELDS) {
      bfh->bfOffBits += sizeof(DWORD) * 3;
    }
    break;
  }
  // XXX: little-endian depended
  err = files_add_temp_file(files, L"clipboard", L".bmp", L"image/bmp", p, plen);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(mem_free(&p));
  return err;
}

NODISCARD static error read_data_object(IDataObject *const dataobj, struct files *const files) {
  if (!dataobj || !files) {
    return errg(err_invalid_arugment);
  }
  struct wstr ws = {0};
  error err = read_text(dataobj, &ws);
  if (esucceeded(err)) {
    err = try_parse_data_uri(&ws, files);
    if (esucceeded(err)) {
      goto succeeded;
    }
  }
  efree(&err);

  err =
      try_read_from_custom_format(dataobj, (CLIPFORMAT)RegisterClipboardFormatW(L"PNG"), L"image/png", L".png", files);
  if (esucceeded(err)) {
    goto succeeded;
  }
  efree(&err);

  err = try_read_from_custom_format(
      dataobj, (CLIPFORMAT)RegisterClipboardFormatW(L"JPEG"), L"image/jpeg", L".jpg", files);
  if (esucceeded(err)) {
    goto succeeded;
  }
  efree(&err);

  err = try_read_from_file_contents(dataobj, files);
  if (esucceeded(err)) {
    goto succeeded;
  }
  efree(&err);

  err = try_read_from_hdrop(dataobj, files);
  if (esucceeded(err)) {
    goto succeeded;
  }
  efree(&err);

  err = try_read_from_dib(dataobj, files);
  if (esucceeded(err)) {
    goto succeeded;
  }
  efree(&err);

  if (ws.len > 0) {
    err = files_add_text_file(files, &ws);
    if (esucceeded(err)) {
      goto succeeded;
    }
    efree(&err);
  }

succeeded:
  ereport(sfree(&ws));
  return eok();
}

static HRESULT STDMETHODCALLTYPE
DragEnter(IDropTarget *This, IDataObject *pDataObj, DWORD grfKeyState, POINTL pt, DWORD *pdwEffect) {
  struct drop_target *const this = (void *)This;
  if (!this || this->super.lpVtbl != &g_drop_target_vtable || !pDataObj || !pdwEffect) {
    return E_INVALIDARG;
  }

  error err = eok();

  if (!this->drag_enter) {
    *pdwEffect = DROPEFFECT_NONE;
    goto cleanup;
  }
  files_cleanup(false);

  struct drag_drop_info ddi = {
      .point = pt,
      .key_state = grfKeyState,
      .effect = *pdwEffect,
  };
  err = read_data_object(pDataObj, &this->dragging_files);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (!this->dragging_files.len) {
    err = emsg(
        err_type_generic, err_abort, &native_unmanaged(NSTR("ドロップすべきファイルがないため処理を中断しました。")));
    goto cleanup;
  }

  this->drag_enter(&this->dragging_files, &ddi, this->userdata);
  *pdwEffect = ddi.effect;

cleanup:
  if (efailed(err)) {
    ereportmsg(err, &native_unmanaged(NSTR("IDropTarget::DragEnter でエラーが発生しました。")));
    *pdwEffect = DROPEFFECT_NONE;
  }
  if (*pdwEffect == DROPEFFECT_NONE) {
    ereport(files_free(&this->dragging_files, false));
  }
  return S_OK;
}

static HRESULT STDMETHODCALLTYPE DragOver(IDropTarget *This, DWORD grfKeyState, POINTL pt, DWORD *pdwEffect) {
  struct drop_target *const this = (void *)This;
  if (!this || this->super.lpVtbl != &g_drop_target_vtable || !pdwEffect) {
    return E_INVALIDARG;
  }

  if (!this->drag_over || !this->dragging_files.len) {
    *pdwEffect = DROPEFFECT_NONE;
    goto cleanup;
  }

  struct drag_drop_info ddi = {
      .point = pt,
      .key_state = grfKeyState,
      .effect = *pdwEffect,
  };
  this->drag_over(&this->dragging_files, &ddi, this->userdata);
  *pdwEffect = ddi.effect;

cleanup:
  if (*pdwEffect == DROPEFFECT_NONE) {
    ereport(files_free(&this->dragging_files, false));
  }
  return S_OK;
}

static HRESULT STDMETHODCALLTYPE DragLeave(IDropTarget *This) {
  struct drop_target *const this = (void *)This;
  if (!this || this->super.lpVtbl != &g_drop_target_vtable) {
    return E_INVALIDARG;
  }

  if (this->drag_leave) {
    this->drag_leave(this->userdata);
  }
  ereport(files_free(&this->dragging_files, false));
  files_cleanup(false);
  return S_OK;
}

static HRESULT STDMETHODCALLTYPE
Drop(IDropTarget *This, IDataObject *pDataObj, DWORD grfKeyState, POINTL pt, DWORD *pdwEffect) {
  struct drop_target *const this = (void *)This;
  if (!this || this->super.lpVtbl != &g_drop_target_vtable || !pDataObj || !pdwEffect) {
    return E_INVALIDARG;
  }

  error err = eok();
  if (!this->drop) {
    *pdwEffect = DROPEFFECT_NONE;
    goto cleanup;
  }

  // Contents may be different from the previous file list, so it needs to be recreated.
  err = files_free(&this->dragging_files, false);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  files_cleanup(false);

  struct drag_drop_info ddi = {
      .point = pt,
      .key_state = grfKeyState,
      .effect = *pdwEffect,
  };
  err = read_data_object(pDataObj, &this->dragging_files);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  if (!this->dragging_files.len) {
    err = emsg(
        err_type_generic, err_abort, &native_unmanaged(NSTR("ドロップすべきファイルがないため処理を中断しました。")));
    goto cleanup;
  }

  this->drop(&this->dragging_files, &ddi, this->userdata);
  this->dragging_files = (struct files){0}; // files is owned by drop handler
  *pdwEffect = ddi.effect;

cleanup:
  if (efailed(err)) {
    ereportmsg(err, &native_unmanaged(NSTR("IDropTarget::Drop でエラーが発生しました。")));
  }
  ereport(files_free(&this->dragging_files, false));
  return S_OK;
}

static IDropTargetVtbl g_drop_target_vtable = {
    .QueryInterface = QueryInterface,
    .AddRef = AddRef,
    .Release = Release,
    .DragEnter = DragEnter,
    .DragOver = DragOver,
    .DragLeave = DragLeave,
    .Drop = Drop,
};

error drop_target_new(struct drop_target **const r) {
  if (!r) {
    return errg(err_null_pointer);
  }

  struct drop_target *dt = CoTaskMemRealloc(NULL, sizeof(struct drop_target));
  if (!dt) {
    return errg(err_out_of_memory);
  }
  memset(dt, 0, sizeof(struct drop_target));
  dt->super.lpVtbl = &g_drop_target_vtable;
  *r = dt;
  return eok();
}

error clipboard_get(struct clipboard *const c) {
  if (!c) {
    return errg(err_null_pointer);
  }

  error err = eok();
  struct files f = {0};
  IDataObject *dataobj = NULL;
  HRESULT hr = OleGetClipboard(&dataobj);
  if (FAILED(hr)) {
    err = errhr(hr);
    goto failed;
  }

  dataobj->lpVtbl->AddRef(dataobj);
  err = read_data_object(dataobj, &f);
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }

  *c = (struct clipboard){
      .files = f,
      .dataobj = dataobj,
  };
  return eok();

failed:
  ereport(files_free(&f, false));
  if (dataobj) {
    dataobj->lpVtbl->Release(dataobj);
    dataobj = NULL;
  }
  return err;
}

error clipboard_free(struct clipboard *const c) {
  if (!c) {
    return errg(err_invalid_arugment);
  }

  ereport(files_free(&c->files, true));
  c->dataobj->lpVtbl->Release(c->dataobj);
  c->dataobj = NULL;
  return eok();
}
