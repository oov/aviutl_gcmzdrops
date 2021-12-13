#include "files.h"

#include "util.h"

enum tempfile_mode {
  tempfile_mode_delete_always = 0,
  tempfile_mode_delete_on_failure = 1,
};

struct tempfile {
  struct wstr path;
  int mode;
};

static struct tempfilelist {
  struct tempfile *ptr;
  size_t len;
  size_t cap;
} g_tempfilelist = {0};

static void
friendly_report(error e, NATIVE_CHAR const *const s1, NATIVE_CHAR const *const s2, NATIVE_CHAR const *const s3) {
  if (esucceeded(e)) {
    return;
  }

  struct NATIVE_STR s = {0};
  error err = scpy(&s, s1);
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }
  err = scat(&s, s2);
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }
  err = scat(&s, s3);
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }
  ereportmsg(e, &s);
  return;

failed:
  ereport(sfree(&s));
  efree(&err);
  ereportmsg(e, &native_unmanaged(NSTR("エラーが発生しました。")));
}

static void tempfilelist_add(struct wstr const *const path, int const mode) {
  struct wstr p = {0};
  error err = scpy(&p, path->ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }
  err = apush(&g_tempfilelist,
              ((struct tempfile){
                  .mode = mode,
                  .path = p,
              }));
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }
  return;

failed:
  ereport(sfree(&p));
  friendly_report(err,
                  NSTR("一時ファイル \""),
                  path->ptr,
                  NSTR("\" を削除候補に登録できませんでした。ファイルは自動で削除されません。"));
}

static void tempfilelist_cleanup(bool const failed) {
  for (size_t i = 0, len = g_tempfilelist.len; i < len; ++i) {
    struct tempfile *const file = g_tempfilelist.ptr + i;
    if (file->mode == tempfile_mode_delete_always || (file->mode == tempfile_mode_delete_on_failure && failed)) {
      error err = delete_file(&file->path);
      if (efailed(err)) {
        friendly_report(err, NSTR("一時ファイル \""), file->path.ptr, NSTR("\" の削除に失敗しました。"));
      }
    }
    ereport(sfree(&file->path));
  }
  ereport(afree(&g_tempfilelist));
}

error files_add(struct files *const f, struct wstr const *const path, struct wstr const *const mime) {
  if (!f || !path || !mime) {
    return errg(err_invalid_arugment);
  }

  struct wstr p = {0};
  struct wstr m = {0};
  error err = scpy(&p, path->ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }

  err = scpy(&m, mime->ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }

  err = apush(f,
              ((struct file){
                  .path = p,
                  .mime = m,
                  .temporary = false,
              }));
  if (efailed(err)) {
    err = ethru(err);
    goto failed;
  }
  return eok();

failed:
  ereport(sfree(&p));
  ereport(sfree(&m));
  return err;
}

error files_delete_last(struct files *const f, bool const delete_later) {
  if (!f || !f->len) {
    return errg(err_invalid_arugment);
  }

  struct file *const file = f->ptr + f->len - 1;
  if (file->temporary) {
    if (delete_later) {
      tempfilelist_add(&file->path, tempfile_mode_delete_always);
    } else {
      error err = delete_file(&file->path);
      if (efailed(err)) {
        // We try to delete it later so can ignore this.
        efree(&err);
        tempfilelist_add(&file->path, tempfile_mode_delete_always);
      }
    }
  }
  ereport(sfree(&file->path));
  ereport(sfree(&file->mime));
  --f->len;
  return eok();
}

error files_free(struct files *const f, bool const delete_later) {
  if (!f) {
    return errg(err_invalid_arugment);
  }

  while (f->len) {
    ereport(files_delete_last(f, delete_later));
  }
  ereport(afree(f));
  return eok();
}

error files_add_delete_on_failure(struct wstr const *const path) {
  if (!path || !path->len) {
    return errg(err_invalid_arugment);
  }
  tempfilelist_add(path, tempfile_mode_delete_on_failure);
  return eok();
}

error files_add_delete_on_cleanup(struct wstr const *const path) {
  if (!path || !path->len) {
    return errg(err_invalid_arugment);
  }
  tempfilelist_add(path, tempfile_mode_delete_always);
  return eok();
}

void files_cleanup(bool const failed) { tempfilelist_cleanup(failed); }
