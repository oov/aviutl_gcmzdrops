#pragma once

#include <stdbool.h>
#include <stdint.h>

#include "3rd/base.c/base.h"

NODISCARD error sniff(void *const data, size_t const len, wchar_t const **const mime, wchar_t const **const ext);
