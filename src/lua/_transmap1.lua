-- this file is written in UTF-8 / CRLF.
return {

  ["標準描画"] = {
    ["標準描画"] = "Standard drawing",
    ["拡大率"] = "Zoom%",
    ["透明度"] = "Clearness",
    ["回転"] = "Rotation",
  },
  ["拡張描画"] = {
    ["拡張描画"] = "Advanced drawing",
    ["拡大率"] = "Zoom%",
    ["透明度"] = "Clearness",
    ["縦横比"] = "rAspect",
    ["X軸回転"] = "X-Spin",
    ["Y軸回転"] = "Y-Spin",
    ["Z軸回転"] = "Z-Spin",
    ["中心X"] = "Center X",
    ["中心Y"] = "Center Y",
    ["中心Z"] = "Center Z",
    ["裏面を表示しない"] = "Do not show backside",
  },
  ["標準再生"] = {
    ["標準再生"] = "Standard playback",
    ["音量"] = "Volume",
    ["左右"] = "Left-Right",
  },

  ["動画ファイル"] = {
    ["動画ファイル"] = "Video file",
    ["再生位置"] = "Playback position",
    ["再生速度"] = "vPlay",
    ["ループ再生"] = "Loop playback",
    ["アルファチャンネルを読み込む"] = "Import alpha channel",
  },
  ["画像ファイル"] = {
    ["画像ファイル"] = "Image file",
  },
  ["音声ファイル"] = {
    ["音声ファイル"] = "Audio file",
    ["再生位置"] = "Playback position",
    ["再生速度"] = "vPlay",
    ["ループ再生"] = "Loop playback",
    ["動画ファイルと連携"] = "Sync with video files",
  },
  ["テキスト"] = {
    ["テキスト"] = "Text",
    ["サイズ"] = "Size",
    ["表示速度"] = "vDisplay",
    ["文字毎に個別オブジェクト"] = "1char1obj",
    ["移動座標上に表示する"] = "Show on motion coordinate",
    ["自動スクロール"] = "Automatic scrolling",
  },
  ["図形"] = {
    ["図形"] = "Graphic",
    ["サイズ"] = "Size",
    ["縦横比"] = "rAspect",
    ["ライン幅"] = "Line width",
  },
  ["フレームバッファ"] = {
    ["フレームバッファ"] = "Frame buffer",
    ["フレームバッファをクリア"] = "Clear frame buffer",
  },
  ["音声波形表示"] = {
    ["音声波形表示"] = "Audio waveform display",
    ["横幅"] = "Width",
    ["高さ"] = "Height",
    ["音量"] = "Volume",
    ["再生位置"] = "Playback position",
    ["編集全体の音声を元にする"] = "Restore original sound",
  },
  ["シーン"] = {
    ["シーン"] = "Scene",
    ["再生位置"] = "Playback position",
    ["再生速度"] = "vPlay",
    ["ループ再生"] = "Loop playback",
  },
  ["シーン(音声)"] = {
    ["シーン(音声)"] = "Scene (audio)",
    ["再生位置"] = "Playback position",
    ["再生速度"] = "vPlay",
    ["ループ再生"] = "Loop playback",
    ["シーンと連携"] = "Sync with the scene",
  },
  ["直前オブジェクト"] = {
    ["直前オブジェクト"] = "Previous object",
  },
  ["パーティクル出力"] = {
    ["パーティクル出力"] = "Particle output",
    ["出力頻度"] = "fOutput",
    ["出力速度"] = "vOutput",
    ["加速度"] = "dv/dt",
    ["出力方向"] = "Output>>",
    ["拡散角度"] = "<Diffuse",
    ["透過率"] = "Transmittance",
    ["透過速度"] = "vTransmit",
    ["拡大率"] = "Zoom%",
    ["拡大速度"] = "vSpread",
    ["回転角"] = "<Rotate",
    ["回転速度"] = "vRotation",
    ["重力"] = "Gravity",
    ["生存時間"] = "tSurvival",
    ["出力方向の基準を移動方向にする"] = "Output direction based on movement direction",
    ["移動範囲の座標からランダムに出力"] = "Random output from movement range",
    ["3Dランダム回転"] = "3D random rotation",
    ["終了点で全て消えるように調節する"] = "Adjusted to fully disappear at end point",
  },
  ["カスタムオブジェクト"] = {
    ["カスタムオブジェクト"] = "Custom object",
  },
  ["時間制御"] = {
    ["時間制御"] = "Time control",
    ["位置"] = "Position",
    ["繰り返し"] = "Repeat",
    ["コマ落ち"] = "Decimation",
    ["フレーム番号指定"] = "Specified frame number",
  },
  ["グループ制御"] = {
    ["グループ制御"] = "Group control",
    ["拡大率"] = "Zoom%",
    ["X軸回転"] = "X-Spin",
    ["Y軸回転"] = "Y-Spin",
    ["Z軸回転"] = "Z-Spin",
    ["上位グループ制御の影響を受ける"] = "Affected by the upper group control",
    ["同じグループのオブジェクトを対象にする"] = "Apply to objects in the same group",
  },
  ["カメラ制御"] = {
    ["カメラ制御"] = "Camera control",
    ["目標X"] = "Target X",
    ["目標Y"] = "Target Y",
    ["目標Z"] = "Target Z",
    ["目標ﾚｲﾔ"] = "Target ly",
    ["傾き"] = "Slope",
    ["深度ぼけ"] = "Depth blur",
    ["視野角"] = "<View",
    ["Zバッファ/シャドウマップを有効にする"] = "Enable Z-buffer / shadow map",
  },
  ["カメラ効果"] = {
    ["カメラ効果"] = "Camera effect",
  },
  ["シャドー(カメラ制御)"] = {
    ["シャドー(カメラ制御)"] = "Shadow (camera control)",
    ["光源X"] = "Light X",
    ["光源Y"] = "Light Y",
    ["光源Z"] = "Light Z",
    ["濃さ"] = "Thickness",
    ["精度"] = "Accuracy",
  },
  ["スクリプト(カメラ制御)"] = {
    ["スクリプト(カメラ制御)"] = "Script (camera control)",
  },

  -- 「メディアオブジェクトの追加」->「フィルタ効果の追加」
  -- TODO: add more entries
  ["モザイク"] = {
    ["モザイク"] = "Mosaic",
    ["サイズ"] = "Size",
    ["タイル風"] = "Tile style",
  },
  ["アニメーション効果"] = {
    ["アニメーション効果"] = "Animation effect",
  },
  ["スクリプト制御"] = {
    ["スクリプト制御"] = "Script control",
  },
  ["動画ファイル合成"] = {
    ["動画ファイル合成"] = "Video files synthesis",
    ["再生位置"] = "Playback position",
    ["再生速度"] = "vPlay",
    ["拡大率"] = "Zoom%",
    ["ループ再生"] = "Loop playback",
    ["動画ファイルの同期"] = "Synchronize with video files",
    ["ループ画像"] = "Loop image",
  },

  -- 「フィルタオブジェクトの追加」
  -- TODO: add more entries
  ["シーンチェンジ"] = {
    ["シーンチェンジ"] = "Scene change",
    ["調整"] = "Adjustment",
    ["反転"] = "Reversal",
    ["ページめくり"] = "Page filpping",
  },

}
