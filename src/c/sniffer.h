#pragma once

#include <stdint.h>
#include <stdbool.h>

#include "base.h"

NODISCARD error sniff(void *const data, size_t const len, wchar_t const **const mime, wchar_t const **const ext);
