#include "error_gcmz.h"

#include "ovutil/win32.h"

#include "gcmzdrops.h"
#include "i18n.h"
#include "task.h"
#include "version.h"

static NODISCARD error get_generic_message(int const type, int const code, struct NATIVE_STR *const dest) {
  if (!dest) {
    return errg(err_invalid_arugment);
  }
  if (type != err_type_generic) {
    dest->len = 0;
    return eok();
  }
  switch (code) {
  case err_fail:
    return to_wstr(&str_unmanaged_const(gettext("Failed.")), dest);
  case err_unexpected:
    return to_wstr(&str_unmanaged_const(gettext("Unexpected.")), dest);
  case err_invalid_arugment:
    return to_wstr(&str_unmanaged_const(gettext("Invalid argument.")), dest);
  case err_null_pointer:
    return to_wstr(&str_unmanaged_const(gettext("NULL pointer.")), dest);
  case err_out_of_memory:
    return to_wstr(&str_unmanaged_const(gettext("Out of memory.")), dest);
  case err_not_sufficient_buffer:
    return to_wstr(&str_unmanaged_const(gettext("Not sufficient buffer.")), dest);
  case err_not_found:
    return to_wstr(&str_unmanaged_const(gettext("Not found.")), dest);
  case err_abort:
    return to_wstr(&str_unmanaged_const(gettext("Aborted.")), dest);
  case err_not_implemented_yet:
    return to_wstr(&str_unmanaged_const(gettext("Not implemented yet.")), dest);
  }
  return to_wstr(&str_unmanaged_const(gettext("Unknown error code.")), dest);
}

static NODISCARD error get_gcmz_message(int const type, int const code, struct NATIVE_STR *const dest) {
  if (!dest) {
    return errg(err_invalid_arugment);
  }
  if (type != err_type_gcmz) {
    dest->len = 0;
    return eok();
  }
  switch (code) {
  case err_gcmz_unsupported_aviutl_version:
    return to_wstr(&str_unmanaged_const(gettext("The currently running version of AviUtl is not supported.")), dest);
  case err_gcmz_exedit_not_found:
    return to_wstr(&str_unmanaged_const(gettext("Advanced Editing plug-in exedit.auf cannot be found.")), dest);
  case err_gcmz_exedit_not_found_in_same_dir:
    return to_wstr(&str_unmanaged_const(gettext(
                       "Advanced Editing plug-in exedit.auf cannot be found in the same folder as GCMZDrops.auf.")),
                   dest);
  case err_gcmz_lua51_cannot_load:
    return to_wstr(&str_unmanaged_const(gettext("Failed to load lua51.dll.")), dest);
  case err_gcmz_unsupported_exedit_version:
    return to_wstr(&str_unmanaged_const(gettext("The currently using version of exedit.auf is not supported.")), dest);
  case err_gcmz_project_is_not_open:
    return to_wstr(&str_unmanaged_const(gettext("AviUtl project file (*.aup) has not yet been opened.")), dest);
  case err_gcmz_project_has_not_yet_been_saved:
    return to_wstr(&str_unmanaged_const(gettext("AviUtl project file (*.aup) has not yet been saved.")), dest);

  case err_gcmz_extext_found:
    return to_wstr(&str_unmanaged_const(gettext("This plug-in is not compatible with extext.auf.")), dest);
  case err_gcmz_oledd_found:
    return to_wstr(&str_unmanaged_const(gettext(
                       "This plug-in is not compatible with oledd.auf, which is an older version of GCMZDrops.")),
                   dest);

  case err_gcmz_failed_to_detect_zoom_level:
    return ssprintf(dest,
                    NULL,
                    L"%1$s\n%2$s",
                    gettext("Failed to detect the zoom rate of the timeline."),
                    gettext("If you have installed a plugin that customizes the look and feel of GUI, that may be the "
                            "cause of this error."));
  case err_gcmz_failed_to_detect_layer_height:
    return ssprintf(dest,
                    NULL,
                    L"%1$s\n%2$s",
                    gettext("Failed to detect the layer height."),
                    gettext("If you have installed a plugin that customizes the look and feel of GUI, that may be the "
                            "cause of this error."));
  case err_gcmz_exists_different_hash_value_file:
    return to_wstr(
        &str_unmanaged_const(gettext(
            "File cannot be saved because a file with different contents already exists in the destination folder.")),
        dest);

  case err_gcmz_lua:
    return to_wstr(&str_unmanaged_const(gettext("An error occurred while executing a Lua script.")), dest);
  case err_gcmz_invalid_char:
    return ssprintf(dest,
                    NULL,
                    L"%1$s\n%2$s",
                    gettext("The filename contains characters that cannot be used in AviUtl."),
                    gettext("To load this file, you need to change the ? part."));
  }
  return to_wstr(&str_unmanaged_const(gettext("Unknown error code.")), dest);
}

NODISCARD error gcmz_error_message(int const type, int const code, struct NATIVE_STR *const dest) {
  if (type == err_type_generic) {
    return get_generic_message(type, code, dest);
  }
  if (type == err_type_hresult) {
    return error_win32_message_mapper(type, code, MAKELANGID(LANG_NEUTRAL, SUBLANG_NEUTRAL), dest);
  }
  if (type == err_type_gcmz) {
    return get_gcmz_message(type, code, dest);
  }
  if (type == err_type_errno) {
    return error_errno_message_mapper(type, code, dest);
  }
  return scpy(dest, NSTR("Unknown error code."));
}

NODISCARD error gcmz_error_to_string(error e, struct wstr *const dest) {
  if (esucceeded(e) || !dest) {
    return errg(err_invalid_arugment);
  }
  error err = eis(e, err_type_gcmz, err_gcmz_lua) ? error_to_string_short(e, dest) : error_to_string(e, dest);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  return err;
}

NODISCARD error gcmz_error_vformat(
    error e, struct wstr *const dest, wchar_t const *const reference, char const *const format, va_list valist) {
  if (!dest || !format) {
    return errg(err_invalid_arugment);
  }
  struct wstr mainmsg = {0};
  struct wstr errmsg = {0};
  error err = eok();
  if (esucceeded(e)) {
    err = mo_vsprintf_wstr(dest, reference, format, valist);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    goto cleanup;
  }
  err = mo_vsprintf_wstr(&mainmsg, reference, format, valist);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = gcmz_error_to_string(e, &errmsg);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = scatm(dest, mainmsg.ptr, NSTR("\n\n"), errmsg.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
cleanup:
  ereport(sfree(&mainmsg));
  ereport(sfree(&errmsg));
  return err;
}

NODISCARD error
gcmz_error_format(error e, struct wstr *const dest, wchar_t const *const reference, char const *const format, ...) {
  va_list valist;
  va_start(valist, format);
  error err = gcmz_error_vformat(e, dest, reference, format, valist);
  if (efailed(err)) {
    err = ethru(err);
  }
  va_end(valist);
  return err;
}

struct error_message_box_task_data {
  HWND window;
  UINT flags;

  struct wstr msg;
  struct wstr title;
};

static void error_message_box_task(void *userdata) {
  struct error_message_box_task_data *d = userdata;
  message_box(d->window, d->msg.ptr, d->title.ptr, d->flags);
  ereport(sfree(&d->title));
  ereport(sfree(&d->msg));
  ereport(mem_free(&d));
}

void gcmz_error_message_box(error e,
                            HWND const window,
                            UINT const flags,
                            bool const deferred,
                            char const *const title,
                            wchar_t const *const reference,
                            char const *const format,
                            ...) {
  struct wstr wide_title = {0};
  struct wstr msg = {0};
  va_list valist;
  va_start(valist, format);
  error err = gcmz_error_vformat(e, &msg, reference, format, valist);
  va_end(valist);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  err = ssprintf(&wide_title, NULL, L"%1$s - %2$s %3$s", title, "GCMZDrops", VERSION);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  struct error_message_box_task_data *d = NULL;
  err = mem(&d, 1, sizeof(struct error_message_box_task_data));
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  d->window = window;
  d->flags = flags;
  d->msg = msg;
  msg = (struct wstr){0};
  d->title = wide_title;
  wide_title = (struct wstr){0};

  if (deferred) {
    // On Wine, touching GUI elements directly from the drag drop handlers seems to cause a freeze.
    // Delay the process to avoid this problem.
    task_add(error_message_box_task, d);
  } else {
    error_message_box_task(d);
  }

cleanup:
  ereport(sfree(&wide_title));
  ereport(sfree(&msg));
  if (efailed(err)) {
    ereportmsg_i18n(err, gettext("Failed to display error dialog."));
  }
  efree(&e);
}
