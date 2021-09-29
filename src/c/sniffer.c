#include "sniffer.h"

error sniff(void *const data, size_t const len, wchar_t const **const mime, wchar_t const **const ext)
{
  if (!data)
  {
    return errg(err_invalid_arugment);
  }
  if (!mime && !ext)
  {
    return errg(err_null_pointer);
  }
  if (len < 16)
  {
    return emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("入力データが短すぎます。")));
  }

  wchar_t const *ext_ = NULL;
  wchar_t const *mime_ = NULL;

  // https://mimesniff.spec.whatwg.org/#matching-a-mime-type-pattern
  // TODO: implement video/mp4, video/webm, audio/mpeg matcher
  uint8_t *b = data;
  if (b[0] == 'G' && b[1] == 'I' && b[2] == 'F' && b[3] == '8' && (b[4] == '7' || b[4] == '9') && b[5] == 'a')
  {
    ext_ = L".gif";
    mime_ = L"image/gif";
  }
  else if (b[0] == 0xff && b[1] == 0xd8 && b[2] == 0xff)
  {
    ext_ = L".jpg";
    mime_ = L"image/jpeg";
  }
  else if (b[0] == 0x89 && b[1] == 'P' && b[2] == 'N' && b[3] == 'G' && b[4] == 0x0d && b[5] == 0x0a && b[6] == 0x1a && b[7] == 0x0a)
  {
    ext_ = L".png";
    mime_ = L"image/png";
  }
  else if (b[0] == 'R' && b[1] == 'I' && b[2] == 'F' && b[3] == 'F' && b[8] == 'W' && b[9] == 'E' && b[10] == 'B' && b[11] == 'P')
  {
    ext_ = L".webp";
    mime_ = L"image/webp";
  }
  else if (b[0] == 0x00 && b[1] == 0x00 && b[2] == 0x01 && b[3] == 0x00)
  {
    ext_ = L".ico";
    mime_ = L"image/x-icon";
  }
  else if (b[0] == 0x00 && b[1] == 0x00 && b[2] == 0x02 && b[3] == 0x00)
  {
    ext_ = L".cur";
    mime_ = L"image/x-icon";
  }
  else if (b[0] == 'B' && b[1] == 'M')
  {
    ext_ = L".bmp";
    mime_ = L"image/bmp";
  }
  else if (b[0] == 'F' && b[1] == 'O' && b[2] == 'R' && b[3] == 'M' && b[8] == 'A' && b[9] == 'I' && b[10] == 'F' && b[11] == 'F')
  {
    ext_ = L".aiff";
    mime_ = L"audio/aiff";
  }
  else if (b[0] == 0x49 && b[1] == 0x44 && b[2] == 0x33)
  {
    ext_ = L".mp3";
    mime_ = L"audio/mpeg";
  }
  else if (b[0] == 'O' && b[1] == 'g' && b[2] == 'g' && b[3] == 'S' && b[4] == 0x00)
  {
    ext_ = L".ogg";
    mime_ = L"application/ogg";
  }
  else if (b[0] == 'M' && b[1] == 'T' && b[2] == 'h' && b[3] == 'd' && b[4] == 0x00 && b[5] == 0x00 && b[6] == 0x00 && b[7] == 0x06)
  {
    ext_ = L".mid";
    mime_ = L"audio/midi";
  }
  else if (b[0] == 'R' && b[1] == 'I' && b[2] == 'F' && b[3] == 'F' && b[8] == 'A' && b[9] == 'V' && b[10] == 'I' && b[11] == ' ')
  {
    ext_ = L".avi";
    mime_ = L"video/avi";
  }
  else if (b[0] == 'R' && b[1] == 'I' && b[2] == 'F' && b[3] == 'F' && b[8] == 'W' && b[9] == 'A' && b[10] == 'V' && b[11] == 'E')
  {
    ext_ = L".wav";
    mime_ = L"audio/wave";
  }
  else if (b[0] == '%' && b[1] == 'P' && b[2] == 'D' && b[3] == 'F' && b[4] == '-')
  {
    ext_ = L".pdf";
    mime_ = L"application/pdf";
  }
  else
  {
    ext_ = L".bin";
    mime_ = L"application/octet-stream";
  }

  if (mime)
  {
    *mime = mime_;
  }
  if (ext)
  {
    *ext = ext_;
  }
  return eok();
}
