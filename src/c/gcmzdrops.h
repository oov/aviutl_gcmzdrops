#pragma once
#include "aviutl.h"

#define GCMZDROPS_NAME_MBCS "\x82\xB2\x82\xBF\x82\xE1\x82\xDC\x82\xBA\x83\x68\x83\x8D\x83\x62\x83\x76\x83\x58"
#define GCMZDROPS_NAME_WIDE L"ごちゃまぜドロップス"

#ifdef TEST_IMAGE_DIR
#  define VERSION "vX.X.X ( testing )"
#  define VERSION_WIDE L"vX.X.X ( testing )"
#else
#  include "version.h"
#endif
#define GCMZDROPS_NAME_VERSION_MBCS (GCMZDROPS_NAME_MBCS " " VERSION)
#define GCMZDROPS_NAME_VERSION_WIDE (GCMZDROPS_NAME_WIDE L" " VERSION_WIDE)

extern FILTER_DLL g_gcmzdrops_filter_dll;
