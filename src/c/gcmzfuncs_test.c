#include "gcmzfuncs.c"
#include "test.h"

#define STBI_NO_STDIO
#define STBI_NO_LINEAR
#define STBI_NO_HDR
#define STBI_ONLY_PNG
#define STBI_NO_THREAD_LOCALS
#define STB_IMAGE_IMPLEMENTATION
#include "3rd/stb_image.h"

wchar_t const *g_aviutl_project_path = NULL;
wchar_t const *g_gui_save_dir = NULL;
int g_gui_mode = gui_mode_auto;

void test_gcmz_get_script_dir(void)
{
  struct wstr tmp = {0};
  if (TEST_SUCCEEDED_F(gcmz_get_script_dir(&tmp)))
  {
    wchar_t const *dirname = L"\\GCMZDrops";
    int pos = 0;
    TEST_CHECK(tmp.len > 0);
    TEST_SUCCEEDED_F(sstr(&tmp, dirname, &pos));
    TEST_CHECK((int)(tmp.len - wcslen(dirname)) == pos);
    TEST_SUCCEEDED_F(sfree(&tmp));
  }
}

void test_gcmz_get_project_dir(void)
{
  struct wstr tmp = {0};
  g_aviutl_project_path = NULL;
  TEST_EIS_F(gcmz_get_project_dir(&tmp), err_type_gcmz, err_gcmz_project_has_not_yet_been_saved);
  g_aviutl_project_path = L"C:\\test_gcmz_get_project_dir\\example.aup";
  if (TEST_SUCCEEDED_F(gcmz_get_project_dir(&tmp)))
  {
    TEST_CHECK(tmp.len > 0);
    wchar_t const *dirname = L"C:\\test_gcmz_get_project_dir";
    TEST_CHECK(wcscmp(tmp.ptr, dirname) == 0);
    TEST_SUCCEEDED_F(sfree(&tmp));
  }
}

void test_gcmz_get_save_dir(void)
{
  struct wstr tmp = {0};
  g_aviutl_project_path = NULL;
  g_gui_save_dir = L"C:\\test_gcmz_get_save_dir\\";
  if (TEST_SUCCEEDED_F(gcmz_get_save_dir(&tmp)))
  {
    TEST_CHECK(tmp.len > 0);
    wchar_t const *dirname = L"C:\\test_gcmz_get_save_dir";
    TEST_CHECK(wcscmp(tmp.ptr, dirname) == 0);
    TEST_SUCCEEDED_F(sfree(&tmp));
  }

  g_aviutl_project_path = NULL;
  g_gui_save_dir = L"%PROJECTDIR%\\";
  TEST_EIS_F(gcmz_get_save_dir(&tmp), err_type_gcmz, err_gcmz_project_has_not_yet_been_saved);

  g_aviutl_project_path = L"C:\\test_gcmz_get_save_dir2\\example.aup";
  g_gui_save_dir = L"%PROJECTDIR%\\";
  if (TEST_SUCCEEDED_F(gcmz_get_save_dir(&tmp)))
  {
    wchar_t const *dirname = L"C:\\test_gcmz_get_save_dir2";
    TEST_CHECK(wcscmp(tmp.ptr, dirname) == 0);
    TEST_SUCCEEDED_F(sfree(&tmp));
  }

  g_aviutl_project_path = L"C:\\test_gcmz_get_save_dir3\\example.aup";
  g_gui_save_dir = L"%PROJECTDIR%\\gcmz\\";
  if (TEST_SUCCEEDED_F(gcmz_get_save_dir(&tmp)))
  {
    wchar_t const *dirname = L"C:\\test_gcmz_get_save_dir3\\gcmz";
    TEST_CHECK(wcscmp(tmp.ptr, dirname) == 0);
    TEST_SUCCEEDED_F(sfree(&tmp));
  }
}

void test_gcmz_is_need_copy(void)
{
  struct wstr tmp = {0};
  struct wstr tempdir = {0};
  struct wstr desktopdir = {0};
  if (!TEST_SUCCEEDED_F(get_temp_dir(&tempdir)))
  {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(exclude_trailing_path_delimiter(&tempdir)))
  {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(sgrow(&desktopdir, MAX_PATH)))
  {
    goto cleanup;
  }
  HRESULT hr = SHGetFolderPathW(0, CSIDL_DESKTOP, NULL, SHGFP_TYPE_CURRENT, desktopdir.ptr);
  if (!TEST_CHECK(hr == S_OK))
  {
    goto cleanup;
  }
  desktopdir.len = wcslen(desktopdir.ptr);
  if (!TEST_SUCCEEDED_F(exclude_trailing_path_delimiter(&desktopdir)))
  {
    goto cleanup;
  }

  static const struct test_data
  {
    int mode;
    wchar_t const *input;
    bool need_copy;
  } test_data[] = {
      {
          .mode = gui_mode_auto,
          .input = L"%TEMP%\\test_gcmz_is_need_copy.txt",
          .need_copy = false,
      },
      {
          .mode = gui_mode_auto,
          .input = L"%TEMP%\\test_gcmz_is_need_copy.png",
          .need_copy = true,
      },
      {
          .mode = gui_mode_auto,
          .input = L"%DESKTOP%\\test_gcmz_is_need_copy.txt",
          .need_copy = false,
      },
      {
          .mode = gui_mode_auto,
          .input = L"%DESKTOP%\\test_gcmz_is_need_copy.png",
          .need_copy = false,
      },
      {
          .mode = gui_mode_copy,
          .input = L"%TEMP%\\test_gcmz_is_need_copy.txt",
          .need_copy = false,
      },
      {
          .mode = gui_mode_copy,
          .input = L"%TEMP%\\test_gcmz_is_need_copy.png",
          .need_copy = true,
      },
      {
          .mode = gui_mode_copy,
          .input = L"%DESKTOP%\\test_gcmz_is_need_copy.txt",
          .need_copy = false,
      },
      {
          .mode = gui_mode_copy,
          .input = L"%DESKTOP%\\test_gcmz_is_need_copy.png",
          .need_copy = true,
      },
      {
          .mode = gui_mode_direct,
          .input = L"%TEMP%\\test_gcmz_is_need_copy.txt",
          .need_copy = false,
      },
      {
          .mode = gui_mode_direct,
          .input = L"%TEMP%\\test_gcmz_is_need_copy.png",
          .need_copy = true,
      },
      {
          .mode = gui_mode_direct,
          .input = L"%DESKTOP%\\test_gcmz_is_need_copy.txt",
          .need_copy = false,
      },
      {
          .mode = gui_mode_direct,
          .input = L"%DESKTOP%\\test_gcmz_is_need_copy.png",
          .need_copy = false,
      },
  };

  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i)
  {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d mode = %d \"%ls\"", i, td->mode, td->input);
    g_gui_mode = td->mode;
    if (!TEST_SUCCEEDED_F(scpy(&tmp, td->input)))
    {
      goto cleanup;
    }
    if (!TEST_SUCCEEDED_F(sreplace_all(&tmp, L"%TEMP%", tempdir.ptr)))
    {
      goto cleanup;
    }
    if (!TEST_SUCCEEDED_F(sreplace_all(&tmp, L"%DESKTOP%", desktopdir.ptr)))
    {
      goto cleanup;
    }
    HANDLE h = CreateFileW(
        tmp.ptr,
        GENERIC_READ | GENERIC_WRITE,
        0,
        NULL,
        CREATE_NEW,
        FILE_ATTRIBUTE_TEMPORARY | FILE_ATTRIBUTE_NOT_CONTENT_INDEXED,
        NULL);
    if (!TEST_CHECK(h != INVALID_HANDLE_VALUE))
    {
      goto cleanup;
    }
    CloseHandle(h);

    bool need_copy = false;
    if (TEST_SUCCEEDED_F(gcmz_is_need_copy(&tmp, &need_copy)))
    {
      TEST_CHECK(td->need_copy == need_copy);
    }
    ereport(delete_file(&tmp));
  }

cleanup:
  ereport(sfree(&tmp));
  ereport(sfree(&tempdir));
  ereport(sfree(&desktopdir));
}

struct cbdata
{
  HANDLE h;
  bool eof;
};

static int cb_read(void *user, char *data, int size)
{
  struct cbdata *d = user;
  DWORD read = 0;
  if (!ReadFile(d->h, data, size, &read, NULL))
  {
    d->eof = true;
    return 0;
  }
  else
  {
    if (read == 0)
    {
      d->eof = true;
    }
  }
  return read;
}

static void cb_skip(void *user, int n)
{
  struct cbdata *d = user;
  SetFilePointer(d->h, n, 0, FILE_CURRENT);
}

static int cb_eof(void *user)
{
  struct cbdata *d = user;
  return d->eof;
}

NODISCARD static error read_image(struct wstr *path, uint8_t **p, size_t *width, size_t *height)
{
  if (!path)
  {
    return errg(err_invalid_arugment);
  }
  if (!p || !width || !height)
  {
    return errg(err_null_pointer);
  }

  error err = eok();
  uint8_t *ptr = NULL, *image = NULL;
  HANDLE file = CreateFileW(path->ptr, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (file == INVALID_HANDLE_VALUE)
  {
    err = err_hr(HRESULT_FROM_WIN32(GetLastError()));
    goto cleanup;
  }
  int w = 0, h = 0, n = 0;
  image = stbi_load_from_callbacks(
      &(stbi_io_callbacks){
          .read = cb_read,
          .skip = cb_skip,
          .eof = cb_eof,
      },
      &(struct cbdata){
          .h = file,
          .eof = false,
      },
      &w, &h, &n, STBI_rgb);
  if (!image)
  {
    err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("failed to load image")));
    goto cleanup;
  }
  if (n != 3)
  {
    err = emsg(err_type_generic, err_fail, &native_unmanaged(NSTR("format is invalid")));
    goto cleanup;
  }

  // We need a padded bgr data, so remapping pixels.
  size_t sline = w * 3;
  size_t dline = ((w * 3 + 3) & ~3);
  err = mem(&ptr, dline * h, 1);
  if (efailed(err))
  {
    err = ethru(err);
    goto cleanup;
  }

  uint8_t *sp = image;
  uint8_t *dp = ptr;
  for (int y = 0; y < h; ++y)
  {
    for (size_t x = 0; x < sline; x += 3)
    {
      dp[x + 0] = sp[x + 2];
      dp[x + 1] = sp[x + 1];
      dp[x + 2] = sp[x + 0];
    }
    sp += sline;
    dp += dline;
  }
  *p = ptr;
  *width = w;
  *height = h;
  ptr = NULL;

cleanup:
  if (image)
  {
    stbi_image_free(image);
    image = NULL;
  }
  if (ptr)
  {
    ereport(mem_free(&ptr));
  }
  if (file != INVALID_HANDLE_VALUE)
  {
    CloseHandle(file);
    file = INVALID_HANDLE_VALUE;
  }
  return err;
}

void test_analyse_exedit_window_image(void)
{
#define LSTR(x) L##x
#define STR(x) LSTR(#x)
#define STRINGIZE(x) STR(x)
  struct wstr testdir = {0};
  struct wstr bmppath = {0};
  if (!TEST_SUCCEEDED_F(scpy(&testdir, STRINGIZE(TEST_IMAGE_DIR))))
  {
    goto cleanup;
  }
  if (!TEST_SUCCEEDED_F(include_trailing_path_delimiter(&testdir)))
  {
    goto cleanup;
  }

  struct test_data
  {
    wchar_t const *filename;
    struct gcmz_analysed_info ai;
  } test_data[] = {
      {
          .filename = L"00-zl24-lhl-cur-24bit.png",
          .ai = {
              .zoom_level = 24,
              .layer_height = gcmz_layer_height_large,
              .edit_cursor = {154, 48},
          },
      },
      {
          .filename = L"01-zl25-lhl-nocur-24bit.png",
          .ai = {
              .zoom_level = 25,
              .layer_height = gcmz_layer_height_large,
              .edit_cursor = {-1, -1},
          },
      },
      {
          .filename = L"02-zl15-lhm-nocur-24bit.png",
          .ai = {
              .zoom_level = 15,
              .layer_height = gcmz_layer_height_medium,
              .edit_cursor = {-1, -1},
          },
      },
      {
          .filename = L"03-zl23-lhs-cur-24bit.png",
          .ai = {
              .zoom_level = 23,
              .layer_height = gcmz_layer_height_small,
              .edit_cursor = {100, 48},
          },
      },
      {
          .filename = L"04-zl11-lhm-nocur-24bit.png",
          .ai = {
              .zoom_level = 11,
              .layer_height = gcmz_layer_height_medium,
              .edit_cursor = {-1, -1},
          },
      },
      {
          .filename = L"05-zl05-lhl-cur-16bit.png",
          .ai = {
              .zoom_level = 5,
              .layer_height = gcmz_layer_height_large,
              .edit_cursor = {73, 48},
          },
      },
      {
          .filename = L"06-zl10-lhs-cur-16bit.png",
          .ai = {
              .zoom_level = 10,
              .layer_height = gcmz_layer_height_small,
              .edit_cursor = {185, 48},
          },
      },
      {
          .filename = L"07-zl10-lhm-nocur-16bit.png",
          .ai = {
              .zoom_level = 10,
              .layer_height = gcmz_layer_height_medium,
              .edit_cursor = {-1, -1},
          },
      },
      {
          .filename = L"08-zl14-lhs-cur-16bit.png",
          .ai = {
              .zoom_level = 14,
              .layer_height = gcmz_layer_height_medium,
              .edit_cursor = {85, 48},
          },
      },
      {
          .filename = L"09-zlxx-lhxx-cur-24bit.png",
          .ai = {
              .zoom_level = 20,
              .layer_height = 12,
              .edit_cursor = {117, 48},
          },
      },
  };

  size_t n = sizeof(test_data) / sizeof(test_data[0]);
  for (size_t i = 0; i < n; ++i)
  {
    struct test_data const *const td = test_data + i;
    TEST_CASE_("test #%d \"%ls\"", i, td->filename);
    if (!TEST_SUCCEEDED_F(scpy(&bmppath, testdir.ptr)))
    {
      continue;
    }
    if (!TEST_SUCCEEDED_F(scat(&bmppath, td->filename)))
    {
      continue;
    }
    uint8_t *image = NULL;
    size_t width = 0;
    size_t height = 0;
    if (!TEST_SUCCEEDED_F(read_image(&bmppath, &image, &width, &height)))
    {
      continue;
    }
    struct gcmz_analysed_info ai = {0};
    if (!TEST_SUCCEEDED_F(analyse_exedit_window_image(image, width, height, &ai)))
    {
      ereport(mem_free(&image));
      continue;
    }
    ereport(mem_free(&image));
    TEST_CHECK(td->ai.zoom_level == ai.zoom_level);
    TEST_MSG("expected %d", td->ai.zoom_level);
    TEST_MSG("got %d", ai.zoom_level);
    TEST_CHECK(td->ai.layer_height == ai.layer_height);
    TEST_MSG("expected %d", td->ai.layer_height);
    TEST_MSG("got %d", ai.layer_height);
    TEST_CHECK(td->ai.edit_cursor.x == ai.edit_cursor.x && td->ai.edit_cursor.y == ai.edit_cursor.y);
    TEST_MSG("expected %ld, %ld", td->ai.edit_cursor.x, td->ai.edit_cursor.y);
    TEST_MSG("got %ld, %ld", ai.edit_cursor.x, ai.edit_cursor.y);
  }

cleanup:
  ereport(sfree(&testdir));
  ereport(sfree(&bmppath));
}

TEST_LIST = {
    {"test_gcmz_get_script_dir", test_gcmz_get_script_dir},
    {"test_gcmz_get_project_dir", test_gcmz_get_project_dir},
    {"test_gcmz_get_save_dir", test_gcmz_get_save_dir},
    {"test_gcmz_is_need_copy", test_gcmz_is_need_copy},
    {"test_analyse_exedit_window_image", test_analyse_exedit_window_image},
    {NULL, NULL},
};

// mocks

error aviutl_get_sys_info(SYS_INFO *si)
{
  (void)si;
  return errg(err_not_implemented_yet);
}

error aviutl_get_project_path(struct wstr *dest)
{
  if (g_aviutl_project_path == NULL)
  {
    return err(err_type_gcmz, err_gcmz_project_has_not_yet_been_saved);
  }
  return scpy(dest, g_aviutl_project_path);
}

error aviutl_get_exedit_window(HWND *h)
{
  (void)h;
  return errg(err_not_implemented_yet);
}

error aviutl_get_frame(int *f)
{
  (void)f;
  return errg(err_not_implemented_yet);
}

error aviutl_set_frame(int *f)
{
  (void)f;
  return errg(err_not_implemented_yet);
}

error aviutl_get_frame_n(int *n)
{
  (void)n;
  return errg(err_not_implemented_yet);
}

error aviutl_set_frame_n(int *n)
{
  (void)n;
  return errg(err_not_implemented_yet);
}

error aviutl_get_select_frame(int *start, int *end)
{
  (void)start;
  (void)end;
  return errg(err_not_implemented_yet);
}

error aviutl_set_select_frame(int start, int end)
{
  (void)start;
  (void)end;
  return errg(err_not_implemented_yet);
}

error gui_get_save_dir(struct wstr *dest)
{
  return scpy(dest, g_gui_save_dir);
}

error gui_get_mode(int *mode)
{
  *mode = g_gui_mode;
  return eok();
}
