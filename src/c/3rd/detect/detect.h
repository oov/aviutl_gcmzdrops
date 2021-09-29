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
#pragma once

#include <stdint.h>

enum
{
    ENCODING_UNKNOWN = 0,
    ENCODING_UTF8 = 1,
    ENCODING_EUCJP = 2,
    ENCODING_SHIFTJIS = 3,
    ENCODING_ISO2022JP = 4,
    ENCODING_UTF16 = 5,
    ENCODING_UTF16BE = 6,
};

struct detector_state
{
    int last_state;
    size_t len;
    size_t suspicious_bytes;
    size_t likely_utf8;
    size_t likely_eucjp;
    size_t likely_shiftjis;
    size_t likely_iso2022jp;
};

int detect_japanese_encoding(struct detector_state *ds, uint8_t *bs, size_t bs_len);
