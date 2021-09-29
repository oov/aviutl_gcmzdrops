// This code is based on the_platinum_searcher.
// https://github.com/monochromegane/the_platinum_searcher
//
// The MIT License (MIT)
//
// Copyright (c) [2014] [the_platinum_searcher]
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
#include "detect.h"

int detect_japanese_encoding(struct detector_state *ds, uint8_t *bs, size_t bs_len)
{
    if (bs_len >= 2 && bs[0] == 0xff && bs[1] == 0xfe)
    {
      return ds->last_state = ENCODING_UTF16;
    }
    if (bs_len >= 2 && bs[0] == 0xfe && bs[1] == 0xff)
    {
      return ds->last_state = ENCODING_UTF16BE;
    }
    if (bs_len >= 3 && bs[0] == 0xef && bs[1] == 0xbb && bs[2] == 0xbf)
    {
      return ds->last_state = ENCODING_UTF8;
    }

    size_t suspicious_bytes = ds->suspicious_bytes;
    size_t likely_utf8 = ds->likely_utf8;
    size_t likely_eucjp = ds->likely_eucjp;
    size_t likely_shiftjis = ds->likely_shiftjis;
    size_t likely_iso2022jp = ds->likely_iso2022jp;

    for (size_t i = 0; i < bs_len; ++i)
    {
        if ((bs[i] < 7 || bs[i] > 14) && (bs[i] < 32 || bs[i] > 127))
        {
            /* UTF-8 detection */
            if (bs[i] > 193 && bs[i] < 224 && i + 1 < bs_len)
            {
                ++i;
                if (bs[i] > 127 && bs[i] < 192)
                {
                    ++likely_utf8;
                    continue;
                }
            }
            else if (bs[i] > 223 && bs[i] < 240 && i + 2 < bs_len)
            {
                ++i;
                if (bs[i] > 127 && bs[i] < 192 && bs[i + 1] > 127 && bs[i + 1] < 192)
                {
                    ++i;
                    ++likely_utf8;
                    continue;
                }
            }

            /* EUC-JP detection */
            if (bs[i] == 142 && i + 1 < bs_len)
            {
                ++i;
                if (bs[i] > 160 && bs[i] < 224)
                {
                    ++likely_eucjp;
                    continue;
                }
            }
            else if (bs[i] > 160 && bs[i] < 255 && i + 1 < bs_len)
            {
                ++i;
                if (bs[i] > 160 && bs[i] < 255)
                {
                    ++likely_eucjp;
                    continue;
                }
            }

            /* Shift-JIS detection */
            if (bs[i] > 160 && bs[i] < 224)
            {
                ++likely_shiftjis;
                continue;
            }
            else if (((bs[i] > 128 && bs[i] < 160) || (bs[i] > 223 && bs[i] < 240)) && i + 1 < bs_len)
            {
                ++i;
                if ((bs[i] > 63 && bs[i] < 127) || (bs[i] > 127 && bs[i] < 253))
                {
                    ++likely_shiftjis;
                    continue;
                }
            }

            /* ISO-2022-JP detection */
            if (bs[i] == 27 && i + 2 < bs_len)
            {
                ++i;
                if (bs[i] == 36)
                {
                    ++i;
                    if (bs[i] == 64 || bs[i] == 66 || bs[i] == 68)
                    {
                        ++likely_iso2022jp;
                        continue;
                    }
                }
                else if (bs[i] == 40)
                {
                    ++i;
                    if (bs[i] == 66 || bs[i] == 73 || bs[i] == 74)
                    {
                        ++likely_iso2022jp;
                        continue;
                    }
                }
            }

            ++suspicious_bytes;
        }
    }

    ds->len += bs_len;
    ds->suspicious_bytes = suspicious_bytes;
    ds->likely_utf8 = likely_utf8;
    ds->likely_eucjp = likely_eucjp;
    ds->likely_shiftjis = likely_shiftjis;
    ds->likely_iso2022jp = likely_iso2022jp;

    if (suspicious_bytes * 2 > ds->len)
    {
        ds->last_state = ENCODING_UNKNOWN; // Binary
    }
    else if (likely_utf8 == 0 && likely_eucjp == 0 && likely_shiftjis == 0 && likely_iso2022jp == 0)
    {
        ds->last_state = ENCODING_UTF8; // ASCII
    }
    else if (likely_utf8 >= likely_eucjp && likely_utf8 >= likely_shiftjis && likely_utf8 >= likely_iso2022jp)
    {
        ds->last_state = ENCODING_UTF8; // UTF-8
    }
    else if (likely_eucjp >= likely_utf8 && likely_eucjp >= likely_shiftjis && likely_eucjp >= likely_iso2022jp)
    {
        ds->last_state = ENCODING_EUCJP;
    }
    else if (likely_shiftjis >= likely_utf8 && likely_shiftjis >= likely_eucjp && likely_shiftjis >= likely_iso2022jp)
    {
        ds->last_state = ENCODING_SHIFTJIS;
    }
    else if (likely_iso2022jp >= likely_utf8 && likely_iso2022jp >= likely_eucjp && likely_iso2022jp >= likely_shiftjis)
    {
        ds->last_state = ENCODING_ISO2022JP;
    }
    else
    {
        ds->last_state = ENCODING_UTF8;
    }
    return ds->last_state;
}
