#include "error_gcmz.h"

#include "gcmzdrops.h"

NODISCARD static error get_message(uint_least32_t const code, struct NATIVE_STR *const message) {
  switch (code) {
  case err_gcmz_unsupported_aviutl_version:
    return scpy(message, NSTR("AviUtl のバージョンが動作対象外です。"));
  case err_gcmz_exedit_not_found:
    return scpy(message, NSTR("拡張編集プラグインが見つかりません。"));
  case err_gcmz_exedit_not_found_in_same_dir:
    return scpy(message, NSTR("拡張編集プラグインが同じフォルダー内に見つかりません。"));
  case err_gcmz_lua51_cannot_load:
    return scpy(message, NSTR("lua51.dll の読み込みに失敗しました。"));
  case err_gcmz_unsupported_exedit_version:
    return scpy(message, NSTR("拡張編集プラグインのバージョンが動作対象外です。"));
  case err_gcmz_project_is_not_open:
    return scpy(message, NSTR("AviUtlのプロジェクトファイル(*.aup)が開かれていません。"));
  case err_gcmz_project_has_not_yet_been_saved:
    return scpy(message, NSTR("AviUtlのプロジェクトファイル(*.aup)がまだ保存されていません。"));

  case err_gcmz_extext_found:
    return scpy(message, GCMZDROPS_NAME_WIDE L"は字幕アシストプラグイン(extext.auf) とは共存できません。");
  case err_gcmz_oledd_found:
    return scpy(message, GCMZDROPS_NAME_WIDE L"の古いバージョンである oledd.auf とは共存できません。");

  case err_gcmz_failed_to_detect_zoom_level:
    return scpy(message, NSTR("拡張編集タイムラインの拡大率検出に失敗しました。"));
  case err_gcmz_failed_to_detect_layer_height:
    return scpy(message, NSTR("拡張編集ウィンドウのレイヤーの高さ検出に失敗しました。"));
  case err_gcmz_exists_different_hash_value_file:
    return scpy(message,
                NSTR("保存先フォルダーに内容の異なるファイルが既に存在しているためファイルを保存できません。"));

  case err_gcmz_lua:
    return scpy(message, NSTR("Lua スクリプトの実行中にエラーが発生しました。"));
  case err_gcmz_invalid_char:
    return scpy(message, NSTR("AviUtl で使用できない文字がファイル名に含まれています。"));
  }
  return scpy(message, NSTR("未知のエラーコードです。"));
}

error error_gcmz_init(void) {
  error err = error_register_message_mapper(err_type_generic, generic_error_message_mapper_jp);
  if (efailed(err)) {
    return err;
  }
  err = error_register_message_mapper(err_type_gcmz, get_message);
  if (efailed(err)) {
    return err;
  }
  return eok();
}
