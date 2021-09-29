#pragma once

#include "base.h"

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

enum
{
  err_type_hresult = 1,
};

#define err_hr(hr) (error_add_(NULL, err_type_hresult, (uint_least32_t)(hr), NULL ERR_FILEPOS_VALUES))
static inline bool err_is_hr(error err, HRESULT hr)
{
  return error_is_(err, err_type_hresult, (uint_least32_t)hr);
}

NODISCARD error error_win32_init(void);
