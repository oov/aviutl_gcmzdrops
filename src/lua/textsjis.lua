local P = {}

P.name = [=[テキストファイルを Shift_JIS に自動変換]=]

-- 他のスクリプトが処理した後にファイルを差し替えると不都合があるので
-- このスクリプトは優先的に実行させる
-- なお、このスクリプトがファイルの差し替えを行った場合、
-- 元の filepath は orgfilepath として保存されます
P.priority = 99999

function P.ondragenter(files, state)
  if GCMZDrops.getpatchid() ~= 0 then
    -- 翻訳パッチが適用されている環境では適用しないようにする
    return false
  end
  for i, v in ipairs(files) do
    if (v.filepath:match("[^.]+$"):lower() == "txt")and(v.mediatype ~= "text/plain; charset=Shift_JIS")and(v.mediatype ~= "text/plain; x-gcmz-charset=ACP") then
      -- ファイルの拡張子が txt で mediatype で Shift_JIS またはアクティブなコードページだという事が明示されていなければ調査する必要があるので true
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
    if (v.filepath:match("[^.]+$"):lower() == "txt")and(v.mediatype ~= "text/plain; charset=Shift_JIS")and(v.mediatype ~= "text/plain; x-gcmz-charset=ACP") then
      -- ファイルを全部読み込む
      local f, err = io.open(v.filepath, "rb")
      if f == nil then
        error(err)
      end
      local text = f:read("*all")
      f:close()
      -- 文字エンコーディングが Shift_JIS 以外で変換可能なものなら差し替え
      local enc = GCMZDrops.detectencoding(text)
      if (enc == "utf8")or(enc == "utf16le")or(enc == "utf16be")or(enc == "eucjp")or(enc == "iso2022jp") then
        local filepath = GCMZDrops.createtempfile("gcmztmp", ".txt")
        f, err = io.open(filepath, "wb")
        if f == nil then
          error(err)
        end
        f:write(GCMZDrops.convertencoding(text, enc, "sjis"))
        f:close()
        debug_print("["..P.name.."] が " .. v.filepath .. " を Shift_JIS に変換して差し替えました。元のファイルは orgfilepath で取得できます。")
        files[i] = {filepath=filepath, orgfilepath=v.filepath, mediatype="text/plain; charset=Shift_JIS"}
      end
    end
  end
  -- 他のイベントハンドラーにも処理をさせたいのでここは常に false
  return false
end

return P
