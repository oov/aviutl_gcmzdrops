#include "error_win32.h"

NODISCARD static error get_message(uint_least32_t const code, struct NATIVE_STR *const dest)
{
  if (!dest)
  {
    return errg(err_invalid_arugment);
  }

  error err = eok();
  LPWSTR msg = NULL;
  int msglen = FormatMessageW(
      FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
      NULL,
      (DWORD)code,
      LANG_USER_DEFAULT,
      (LPWSTR)&msg,
      0,
      NULL);
  if (!msglen)
  {
    HRESULT hr = HRESULT_FROM_WIN32(GetLastError());
    if (hr != HRESULT_FROM_WIN32(ERROR_MR_MID_NOT_FOUND))
    {
      err = err_hr(hr);
      goto cleanup;
    }
    err = scpy(dest, L"");
    if (efailed(err))
    {
      err = ethru(err);
      goto cleanup;
    }

    goto cleanup;
  }

  if (msg[msglen - 1] == L'\r' || msg[msglen - 1] == L'\n')
  {
    msg[--msglen] = L'\0';
    if (msg[msglen - 1] == L'\r' || msg[msglen - 1] == L'\n')
    {
      msg[--msglen] = L'\0';
    }
  }
  err = scpy(dest, msg);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }

  goto cleanup;

cleanup:
  if (msg)
  {
    LocalFree(msg);
  }
  return err;
}

error error_win32_init(void)
{
  return error_register_message_mapper(err_type_hresult, get_message);
}
