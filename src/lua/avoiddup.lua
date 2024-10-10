local P = {}

P.name = i18n({
  ja_JP = [=[既存ファイルの再利用]=],
  en_US = [=[Reuse existing files]=],
})

-- 他のスクリプトが処理した後にファイルを差し替えると不都合があるので
-- このスクリプトは優先的に実行させる
-- なお、このスクリプトがファイルの差し替えを行った場合、
-- 元の filepath は orgfilepath として保存されます
P.priority = 100000

-- ファイル名入力ダイアログを表示するなら true、しないなら false
-- ごちゃまぜドロップス v0.1.x での挙動に近づけるなら true
P.renamable = false

function P.ondragenter(files, state)
  for i, v in ipairs(files) do
    if GCMZDrops.needcopy(v.filepath) then
      -- needcopy が true を返すファイルは調査する必要があるので true
      return true
    end
  end
  return false
end

function P.ondragover(files, state)
  -- ondragenter で処理できそうなものは ondragover でも処理できそうなので調べず true
  return true
end

function P.ondragleave() end

function P.ondrop(files, state)
  for i, v in ipairs(files) do
    -- コピーが必要なファイルだったら
    if GCMZDrops.needcopy(v.filepath) then
      local filepath, created = P.getfile(v.filepath)
      if created then
        debug_print(string.format(
          i18n({
            ja_JP = [=[%s: %s をハッシュ値付きのファイル名に差し替えました。]=],
            en_US = [=[%s: Renamed %s to filename with hash value.]=],
          }),
          P.name,
          v.filepath
        ))
      else
        if filepath ~= "" then
          debug_print(string.format(
            i18n({
              ja_JP = [=[%s: %s と同じファイルが既にあるので既存のファイルに差し替えました。]=],
              en_US = [=[%s: %s already exists, so it has been replaced with the existing file.]=],
            }),
            P.name,
            v.filepath
          ))
        else
          -- ユーザーがキャンセルしたのでそのまま全体をキャンセル
          return nil
        end
      end
      files[i] = { filepath = filepath, orgfilepath = v.filepath }
    end
  end
  -- 他のイベントハンドラーにも処理をさせたいのでここは常に false
  return false
end

-- f, created = P.getfile(filepath)
--
--   ごちゃまぜドロップスの保存用フォルダーに同じファイルがないか検索し、
--   ある場合は既存ファイルへのパスを、
--   ない場合は保存用フォルダーにファイルをコピーし、コピーしたファイルへのパスを返します。
--
--   [引数]
--     filepath には探したいファイルへのパスを文字列で渡します。
--
--   [戻り値]
--     f にはファイルへのパスが文字列で返りますが、
--     ユーザーにより処理がキャンセルされた場合は空文字列が返ります。
--     created には新しくファイルを作成したかどうかを boolean で返します。
--
function P.getfile(filepath)
  -- ファイルのハッシュ値を計算してテキスト表現に変形
  local hash = GCMZDrops.hashtostring(GCMZDrops.calcfilehash(filepath))
  -- ファイルパスをディレクトリ、ファイル名、拡張子に分解
  local ext = filepath:match("[^.]+$")
  local name = filepath:match("[^/\\]+$")
  local dir = filepath:sub(1, #filepath - #name)
  name = name:sub(1, #name - #ext - 1)

  -- 既に同じハッシュ値と拡張子を持ったファイルがないか探す
  local exists = GCMZDrops.findallfile("*." .. hash .. "." .. ext)
  if #exists > 0 then
    return exists[1], false
  end

  if P.renamable then
    local ok, newname = GCMZDrops.prompt(
      string.format(i18n({
        ja_JP = [=[ファイル名を入力してください]=],
        en_US = [=[Please enter a file name]=],
      })),
      name
    )
    if not ok then
      -- ユーザーがキャンセルした
      return "", false
    end
    -- ファイル名に使えない文字をフィルタリングする
    name = GCMZDrops.convertencoding(newname, "ansi", "utf8")
    name = name:gsub("[\1-\31\34\42\47\58\60\62\63\92\124\127]", "-")
    name = GCMZDrops.convertencoding(name, "utf8", "ansi")
  end

  -- ファイルをコピーするために読み出す
  local f, err = io.open(filepath, "rb")
  if f == nil then
    error(err)
  end
  local data = f:read("*all")
  f:close()
  -- 保存先にファイルを作成して書き込む
  filepath = GCMZDrops.createfile(name, "." .. hash .. "." .. ext)
  f, err = io.open(filepath, "wb")
  if f == nil then
    error(err)
  end
  f:write(data)
  f:close()
  return filepath, true
end

return P
