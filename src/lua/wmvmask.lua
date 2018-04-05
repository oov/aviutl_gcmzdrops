local P = {}

P.name = "*_mask.wmv があるならマスクを追加"

P.priority = 0

local function fileexists(filepath)
  local f = io.open(filepath, "rb")
  if f ~= nil then
    f:close()
    return true
  end
  return false
end

function P.ondragenter(files, state)
  for i, v in ipairs(files) do
    local ext = v.filepath:match(".[^.]+$")
    local maskfile = v.filepath:sub(1, #v.filepath - #ext) .. "_mask" .. ext
    if ext:lower() == ".wmv" and fileexists(maskfile) then
      -- ファイルの拡張子が .wmv のファイルがあって、かつ *_mask.wmv があるなら true
      return true
    end
  end
  return false
end

function P.ondragover(files, state)
  -- ondragenter で処理できそうなものは ondragover でも処理できそうなので調べず true
  return true
end

function P.ondragleave()
end

function P.ondrop(files, state)
  for i, v in ipairs(files) do
    -- ファイルの拡張子が .wmv のファイルがあって、かつ *_mask.wmv があるなら
    local ext = v.filepath:match(".[^.]+$")
    local maskfile = v.filepath:sub(1, #v.filepath - #ext) .. "_mask" .. ext
    if ext:lower() == ".wmv" and fileexists(maskfile) then
      -- プロジェクトとファイルの情報を取得する
      local proj = GCMZDrops.getexeditfileinfo()
      local ok, fi = pcall(GCMZDrops.getfileinfo, v.filepath)
      if not ok then
        debug_print("動画の読み込みに失敗しました: " .. fi)
        return nil
      end

      -- 動画が現在のプロジェクトで何フレーム分あるのかを計算する
      -- 拡張編集での計算方法と一致する算出方法がわかってないので、もしかしたら１フレーム単位で前後するかも……
      local len = math.floor((fi.length * fi.scale * proj.rate) / (fi.rate * proj.scale) + 0.5)

      local oini = GCMZDrops.inistring("")
      oini:set("exedit", "width", proj.width)
      oini:set("exedit", "height", proj.height)
      oini:set("exedit", "rate", proj.rate)
      oini:set("exedit", "scale", proj.scale)
      oini:set("exedit", "length", len)
      oini:set("exedit", "audio_rate", proj.audio_rate)
      oini:set("exedit", "audio_ch", proj.audio_ch)

      oini:set("0", "start", 1)
      oini:set("0", "end", len)
      oini:set("0", "layer", 1)
      oini:set("0", "overlay", 1)
      oini:set("0", "camera", 0)

      oini:set("0.0", "_name", "動画ファイル")
      oini:set("0.0", "再生位置", 1)
      oini:set("0.0", "再生速度", "100.0")
      oini:set("0.0", "ループ再生", 0)
      oini:set("0.0", "アルファチャンネルを読み込む", 0)
      oini:set("0.0", "file", v.filepath)

      oini:set("0.1", "_name", "動画ファイル合成")
      oini:set("0.1", "再生位置", 0)
      oini:set("0.1", "再生速度", "100.0")
      oini:set("0.1", "X", 0)
      oini:set("0.1", "Y", 0)
      oini:set("0.1", "拡大率", "100.0")
      oini:set("0.1", "ループ再生", 0)
      oini:set("0.1", "動画ファイルの同期", 1)
      oini:set("0.1", "ループ画像", 0)
      oini:set("0.1", "file", maskfile)
      oini:set("0.1", "mode", 1)

      oini:set("0.2", "_name", "標準描画")
      oini:set("0.2", "X", "0.0")
      oini:set("0.2", "Y", "0.0")
      oini:set("0.2", "Z", "0.0")
      oini:set("0.2", "拡大率", "100.0")
      oini:set("0.2", "透明度", 0)
      oini:set("0.2", "回転", "0.00")
      oini:set("0.2", "blend", 0)

      local filepath = GCMZDrops.createtempfile("wmv", ".exo")
      f, err = io.open(filepath, "wb")
      if f == nil then
        error(err)
      end
      f:write(tostring(oini))
      f:close()
      debug_print("["..P.name.."] が " .. v.filepath .. " を exo ファイルに差し替えました。元のファイルは orgfilepath で取得できます。")
      files[i] = {filepath=filepath, orgfilepath=v.filepath}
    end
  end
  -- 他のイベントハンドラーにも処理をさせたいのでここは常に false
  return false
end

return P
