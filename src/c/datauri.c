#include "datauri.h"

#include "ovutil/str.h"
#include "ovutil/win32.h"

#include "sniffer.h"

static const uint8_t base64_table[128] = {
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 62,
    255, 255, 255, 63,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  255, 255, 255, 255, 255, 255, 255, 0,
    1,   2,   3,   4,   5,   6,   7,   8,   9,   10,  11,  12,  13,  14,  15,  16,  17,  18,  19,  20,  21,  22,
    23,  24,  25,  255, 255, 255, 255, 255, 255, 26,  27,  28,  29,  30,  31,  32,  33,  34,  35,  36,  37,  38,
    39,  40,  41,  42,  43,  44,  45,  46,  47,  48,  49,  50,  51,  255, 255, 255, 255, 255,
};

NODISCARD static error base64_decoded_len(wchar_t const *const ws, size_t const wslen, size_t *const len) {
  if (!ws) {
    return errg(err_invalid_arugment);
  }
  if (!len) {
    return errg(err_null_pointer);
  }
  size_t const omitted_equals = (4 - wslen % 4) & 0x3;
  size_t pad = omitted_equals;
  for (size_t pos = wslen - 1; pos > 0 && ws[pos] == L'=' && pad < 3; --pos) {
    ++pad;
  }
  if (pad > 2) {
    // broken input
    return errg(err_fail);
  }
  *len = ((wslen + omitted_equals) / 4) * 3 - pad;
  return eok();
}

NODISCARD static error
base64_decode(wchar_t const *const ws, size_t const wslen, void *const data, size_t const datalen) {
  if (!ws) {
    return errg(err_invalid_arugment);
  }
  if (wslen > 0 && !data) {
    return errg(err_null_pointer);
  }
  size_t len = 0;
  error err = base64_decoded_len(ws, wslen, &len);
  if (efailed(err)) {
    err = ethru(err);
    return err;
  }
  if (len > datalen) {
    return errg(err_not_sufficient_buffer);
  }
  uint8_t *d = data;
  size_t const end = (len * 4 + 2) / 3;
  size_t const remain = end % 4;
  size_t const last = end - remain;
  for (size_t i = 0; i < last; i += 4) {
    wchar_t const c0 = ws[i + 0], c1 = ws[i + 1], c2 = ws[i + 2], c3 = ws[i + 3];
    if (c0 > 127 || c1 > 127 || c2 > 127 || c3 > 127) {
      return errg(err_fail);
    }
    uint_least8_t const p0 = base64_table[c0], p1 = base64_table[c1], p2 = base64_table[c2], p3 = base64_table[c3];
    if (p0 == 255 || p1 == 255 || p2 == 255 || p3 == 255) {
      return errg(err_fail);
    }
    uint_least32_t const v = (uint_least32_t)((p0 << 18) | (p1 << 12) | (p2 << 6) | (p3 << 0));
    *d++ = (v >> 16) & 0xff;
    *d++ = (v >> 8) & 0xff;
    *d++ = (v >> 0) & 0xff;
  }
  if (remain > 0) {
    wchar_t const c0 = ws[last], c1 = ws[last + 1], c2 = remain == 3 ? ws[last + 2] : L'A';
    if (c0 > 127 || c1 > 127 || c2 > 127) {
      return errg(err_fail);
    }
    uint_least8_t const p0 = base64_table[c0], p1 = base64_table[c1], p2 = base64_table[c2];
    if (p0 == 255 || p1 == 255 || p2 == 255) {
      return errg(err_fail);
    }
    uint_least32_t const v = (uint_least32_t)((p0 << 18) | (p1 << 12) | (p2 << 6));
    *d++ = (v >> 16) & 0xff;
    if (remain == 3) {
      *d++ = (v >> 8) & 0xff;
    }
  }
  return eok();
}

static inline uint_least8_t hex2int(wchar_t c) {
  if (L'0' <= c && c <= L'9') {
    return (c & 0xff) - L'0';
  }
  if ((L'A' <= c && c <= L'F') || (L'a' <= c && c <= L'f')) {
    return (c & 0x5f) - L'A' + 10;
  }
  return 255;
}

NODISCARD static error percent_decoded_len(wchar_t const *const ws, size_t const wslen, size_t *const len) {
  if (!ws) {
    return errg(err_invalid_arugment);
  }
  if (!len) {
    return errg(err_null_pointer);
  }
  size_t n = 0;
  for (size_t i = 0; i < wslen; ++i) {
    wchar_t const c = ws[i];
    if (c > 127) {
      return errg(err_fail);
    }
    if (c != L'%') {
      ++n;
      continue;
    }
    if (i + 2 >= wslen) {
      return errg(err_fail);
    }
    wchar_t const c1 = ws[i + 1], c2 = ws[i + 2];
    if (c1 > 127 || c2 > 127) {
      return errg(err_fail);
    }
    uint_least8_t const v1 = hex2int(c1), v2 = hex2int(c2);
    if (v1 == 255 || v2 == 255) {
      return errg(err_fail);
    }
    ++n;
    i += 2;
  }
  *len = n;
  return eok();
}

NODISCARD static error
percent_decode(wchar_t const *const ws, size_t const wslen, void *const data, size_t const datalen) {
  if (!ws) {
    return errg(err_invalid_arugment);
  }
  if (wslen > 0 && !data) {
    return errg(err_null_pointer);
  }
  uint8_t *d = data;
  size_t n = 0;
  for (size_t i = 0; i < wslen; ++i) {
    wchar_t const c = ws[i];
    if (c > 127) {
      return errg(err_fail);
    }
    if (c != L'%') {
      if (n == datalen) {
        return errg(err_not_sufficient_buffer);
      }
      *d++ = c & 0xff;
      ++n;
      continue;
    }
    if (i + 2 >= wslen) {
      return errg(err_fail);
    }
    wchar_t const c1 = ws[i + 1], c2 = ws[i + 2];
    if (c1 > 127 || c2 > 127) {
      return errg(err_fail);
    }
    uint_least8_t const v1 = hex2int(c1), v2 = hex2int(c2);
    if (v1 == 255 || v2 == 255) {
      return errg(err_fail);
    }
    if (n == datalen) {
      return errg(err_not_sufficient_buffer);
    }
    *d++ = (uint8_t)((v1 << 4) | (v2 << 0));
    ++n;
    i += 2;
  }
  return eok();
}

error data_uri_parse(wchar_t const *ws, size_t wslen, struct data_uri *const d) {
  if (!ws) {
    return errg(err_invalid_arugment);
  }
  if (!d) {
    return errg(err_null_pointer);
  }

  if (wslen < 5 || wcsncmp(ws, L"data:", 5) != 0) {
    return errg(err_fail);
  }
  ws += 5;
  wslen -= 5;
  wchar_t const *const comma = wcsstr(ws, L",");
  if (!comma) {
    return errg(err_fail);
  }

  struct data_uri dd = {0};
  char tmp[128] = {0};
  struct wstr tmp2 = {0};
  error err = eok();

  size_t const headerlen = (size_t)(comma - ws);
  if (headerlen > 0) {
    wchar_t const *cur = ws;
    while (cur < comma) {
      wchar_t const *const sep = wcspbrk(cur, L";,");
      size_t const len = (size_t)(sep - cur);
      if (len == 6 && wcsncmp(cur, L"base64", 6) == 0) {
        dd.encoding = data_uri_encoding_base64;
        cur += len + 1;
        continue;
      }
      if (len > 8 && wcsncmp(cur, L"charset=", 8) == 0) {
        if (len - 8 >= 128) {
          err = errg(err_fail);
          goto cleanup;
        }
        wcsncpy(dd.charset, cur + 8, len - 8);
        dd.charset[len - 8] = L'\0';
        cur += len + 1;
        continue;
      }
      // Non-standard exntension for filename
      if (len > 9 && wcsncmp(cur, L"filename=", 9) == 0) {
        size_t sz = 0;
        err = percent_decoded_len(cur + 9, len - 9, &sz);
        if (efailed(err)) {
          efree(&err);
          err = errg(err_fail);
          goto cleanup;
        }
        if (sz >= 128) {
          err = errg(err_fail);
          goto cleanup;
        }
        err = percent_decode(cur + 9, len - 9, tmp, sz);
        if (efailed(err)) {
          efree(&err);
          err = errg(err_fail);
          goto cleanup;
        }
        tmp[sz] = L'\0';
        err = from_utf8(
            &(struct str){
                .ptr = tmp,
                .len = sz,
            },
            &tmp2);
        if (efailed(err)) {
          efree(&err);
          err = errg(err_fail);
          goto cleanup;
        }
        if (tmp2.len >= 128) {
          err = errg(err_fail);
          goto cleanup;
        }
        wcscpy(dd.ext_filename, tmp2.ptr);
        cur += len + 1;
        continue;
      }
      if (dd.mime[0] != L'\0') {
        // unknown block
        cur += len + 1;
        continue;
      }
      if (len >= 256) {
        err = errg(err_fail);
        goto cleanup;
      }
      wcsncpy(dd.mime, cur, len);
      dd.mime[len] = L'\0';
      cur += len + 1;
    }
  }
  if (dd.mime[0] == L'\0' && dd.charset[0] == L'\0') {
    wcscpy(dd.mime, L"text/plain");
    wcscpy(dd.charset, L"US-ASCII");
  }
  dd.encoded = (wchar_t const *)comma + 1;
  dd.encoded_len = wslen - (size_t)(dd.encoded - ws);
  *d = dd;

cleanup:
  ereport(sfree(&tmp2));
  return err;
}

error data_uri_decode(struct data_uri *const d) {
  if (!d) {
    return errg(err_invalid_arugment);
  }

  error (*decoded_len_func)(wchar_t const *const, size_t const, size_t *const) = NULL;
  error (*decode_func)(wchar_t const *const, size_t const, void *, size_t const) = NULL;
  switch (d->encoding) {
  case data_uri_encoding_percent:
    decoded_len_func = percent_decoded_len;
    decode_func = percent_decode;
    break;
  case data_uri_encoding_base64:
    decoded_len_func = base64_decoded_len;
    decode_func = base64_decode;
    break;
  default:
    return errg(err_unexpected);
  }

  error err = eok();
  void *decoded = NULL;
  size_t decoded_len = 0;
  if (d->encoded_len) {
    err = decoded_len_func(d->encoded, d->encoded_len, &decoded_len);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    if (decoded_len) {
      err = mem(&decoded, decoded_len, 1);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      err = decode_func(d->encoded, d->encoded_len, decoded, decoded_len);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
    }
  }
  d->decoded = decoded;
  decoded = NULL;
  d->decoded_len = decoded_len;

cleanup:
  ereport(mem_free(&decoded));
  return err;
}

error data_uri_free(struct data_uri *const d) {
  if (!d) {
    return errg(err_invalid_arugment);
  }

  ereport(mem_free(&d->decoded));
  d->decoded_len = 0;
  return eok();
}

static wchar_t const *mime_to_extension(wchar_t const *const mime) {
  if (wcscmp(mime, L"image/x-icon") == 0) {
    return L".ico";
  }
  if (wcscmp(mime, L"image/vnd.microsoft.icon") == 0) {
    return L".ico";
  }
  if (wcscmp(mime, L"image/bmp") == 0) {
    return L".bmp";
  }
  if (wcscmp(mime, L"image/gif") == 0) {
    return L".gif";
  }
  if (wcscmp(mime, L"image/webp") == 0) {
    return L".webp";
  }
  if (wcscmp(mime, L"image/png") == 0) {
    return L".png";
  }
  if (wcscmp(mime, L"image/jpeg") == 0) {
    return L".jpg";
  }
  if (wcscmp(mime, L"audio/basic") == 0) {
    return L".snd";
  }
  if (wcscmp(mime, L"audio/aiff") == 0) {
    return L".aiff";
  }
  if (wcscmp(mime, L"audio/mpeg") == 0) {
    return L".mp3";
  }
  if (wcscmp(mime, L"application/ogg") == 0) {
    return L".ogg";
  }
  if (wcscmp(mime, L"audio/midi") == 0) {
    return L".mid";
  }
  if (wcscmp(mime, L"video/avi") == 0) {
    return L".avi";
  }
  if (wcscmp(mime, L"audio/wave") == 0) {
    return L".wav";
  }
  if (wcscmp(mime, L"video/mp4") == 0) {
    return L".mp4";
  }
  if (wcscmp(mime, L"video/webm") == 0) {
    return L".webm";
  }
  if (wcscmp(mime, L"application/pdf") == 0) {
    return L".pdf";
  }
  if (wcscmp(mime, L"text/plain") == 0) {
    return L".txt";
  }
  return NULL;
}

error data_uri_suggest_filename(struct data_uri const *const d, struct wstr *const dest) {
  if (!d) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }

  struct wstr tmp = {0};
  struct wstr filename = {0};
  error err = eok();

  // If filename is stored, use it.
  if (d->ext_filename[0] != L'\0') {
    size_t pos = 0;
    err = extract_file_name(&wstr_unmanaged_const(d->ext_filename), &pos);
    if (esucceeded(err)) {
      err = scpy(dest, d->ext_filename + pos);
      if (efailed(err)) {
        err = ethru(err);
        goto cleanup;
      }
      goto cleanup;
    }
    efree(&err);
  }

  // Use encoded strings in a way similar to a browser.
  if (d->encoded_len >= 24) {
    err = sncpy(&tmp, d->encoded + d->encoded_len - 24, 24);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = sanitize(&tmp);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    size_t pos = 0;
    err = extract_file_name(&tmp, &pos);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
    err = scpy(&filename, tmp.ptr + pos);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  // Last resort.
  if (!filename.len) {
    err = scpy(&filename, L"noname");
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }

  // Guessing file extension by MIME.
  wchar_t const *ext = mime_to_extension(d->mime);
  if (!ext && d->decoded && d->decoded_len >= 16) {
    // Guessing file extension by content.
    err = sniff(d->decoded, d->decoded_len, NULL, &ext);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }
  // Last resort.
  if (!ext) {
    ext = L".bin";
  }

  err = scat(&filename, ext);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

  err = scpy(dest, filename.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&filename));
  ereport(sfree(&tmp));
  return err;
}

error data_uri_get_mime(struct data_uri const *const d, struct wstr *const dest) {
  if (!d) {
    return errg(err_invalid_arugment);
  }
  if (!dest) {
    return errg(err_null_pointer);
  }

  struct wstr tmp = {0};
  error err = scpy(&tmp, d->mime);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }
  bool const has_charset = d->charset[0] != L'\0';
  if (has_charset) {
    err = scatm(&tmp, L"; charset=", d->charset);
    if (efailed(err)) {
      err = ethru(err);
      goto cleanup;
    }
  }

  err = scpy(dest, tmp.ptr);
  if (efailed(err)) {
    err = ethru(err);
    goto cleanup;
  }

cleanup:
  ereport(sfree(&tmp));
  return err;
}
