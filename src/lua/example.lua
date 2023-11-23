-- ごちゃまぜドロップス用イベントハンドラースクリプトの記述サンプル
-- 文字コードは UTF-8 で記述しますが、ごちゃまぜドロップスに Lua 内で記述した文字列をわたす場合はアクティブなコードページに変換してください
local P = {}

-- このハンドラーの名前を指定します。
-- この名前はデバッグ時に使用されます。
-- i18n を使うとUI言語に合わせて翻訳版が自動選択され、アクティブなコードページに変換されます。
P.name = i18n({
  ja_JP = [=[サンプルハンドラー]=],
  en_US = [=[Example Handler]=],
})

-- このハンドラーの処理優先度を整数で指定します。
-- ファイルを処理できるハンドラーが複数ある場合に、値が大きいほど優先されます。
P.priority = 0

-- ondragenter はファイルやテキストをドラッグし、
-- 拡張編集ウィンドウ上にマウスが乗った瞬間に呼ばれます。
function P.ondragenter(files, state)
  -- files にはドラッグされているアイテムが配列（テーブル）で渡されます。
  --
  --   [例1] エクスプローラーなどからファイルがドロップされる場合
  --     files = {
  --       {filepath="C:\Your\File\First.png"},
  --       {filepath="C:\Your\File\Second.wav"}
  --     }
  --
  --     [補足情報]
  --       エクスプローラー以外のソフトからファイルを掴んでいる時は
  --       filepath で示されたファイルがまだ実在しないケースがあります。
  --       ondrop 以外でファイルの内容を参照したい場合は
  --       ファイルが存在しなかった時の事も想定して処理を書いてください。
  --
  --   [例2] Data URL scheme(RFC 2397) のテキストがドロップされる場合
  --     files = {
  --       {filepath="C:\Your\Image.png", mediatype="image/png"}
  --     }
  --
  --     [補足情報]
  --       「data:text/plain,A%20brief%20note」のようなテキストを範囲選択して
  --       拡張編集ウィンドウにドロップされた時の処理です。
  --       ブラウザからのドロップ時にも使われることがあります。
  --       "data:,A%20brief%20note" のような場合は mediatype は空文字列です。
  --       ファイルの拡張子は mediatype の内容に従って以下のように決定されます。
  --       "text/plain" - *.txt
  --       "image/jpeg" - *.jpg
  --       "image/gif"  - *.gif
  --       "image/png"  - *.png
  --       "image/webp" - *.webp
  --       "image/bmp"  - *.bmp
  --       "audio/wav"  - *.wav
  --       それ以外     - *.bin
  --
  --   [例3] ブラウザやテキストエディタで選択したテキストがドロップされる場合
  --     files = {
  --       {filepath="C:\Your\Text.txt", mediatype="text/plain; charset=Shift_JIS"}
  --     }
  --
  --     [補足情報]
  --       テキストがドロップされる場合、元のデータが Unicode だったとしても
  --       事前にアクティブなコードページに変換され、常に同じ mediatype で渡されます。
  --       注意: 日本語環境以外では mediatype は "text/plain; x-gcmz-charset=ACP" になります。
  --
  --   [補足情報]
  --     例1～3 で挙げたものはごちゃまぜドロップスの本体から渡される情報ですが、
  --     自作したスクリプトに処理が回ってくるよりも先に
  --     textsjis.lua など別のスクリプトが実行されることにより、
  --     files 内のファイル情報にはここでは説明されていない属性が追加されたり、
  --     ドロップされていないファイルが追加されていたりすることがあります。
  --
  --     それらの仕組みに振り回されたくない場合は
  --     優先順位をとても大きな値にすることで回避できますが、
  --     状況によっては内容が同じファイルがいくつも作成されてしまうなど、
  --     本来意図していた挙動が壊れてしまう可能性もあるので注意が必要です。
  --
  -- state にはマウスとキーボードに関する情報がテーブルで渡されます。
  --
  --   state = {
  --     x=123,
  --     y=168,
  --     control=false,
  --     shift=false,
  --     alt=false,
  --     lbutton=false,
  --     mbutton=false,
  --     rbutton=false
  --   }
  --
  --   [補足情報]
  --     例えば Shift キーを押したままドロップされたファイルだけに反応したい場合でも、
  --     ondragenter や ondragover の時点ではまだ Shift キーが押されていないこともあります。
  --     その時に return false してしまうと
  --     後続の odragover や ondrop の時にハンドラーが呼ばれないので注意が必要です。

  -- ファイルが処理できる、もしくは処理できそうな場合は true を返してください。
  return false
end

-- ondragover はファイルやテキストをドラッグしたまま、
-- 拡張編集ウィンドウ上でマウスが動く度に呼ばれます。
-- ただし ondragenter や ondragover で false を返していた場合は呼ばれません。
function P.ondragover(files, state)
  -- files, state の内容は ondragenter と同じです。
  -- このイベントハンドラは高頻度で呼ばれるため、重い処理は行わないでください。

  -- ファイルが処理できる、もしくは処理できそうな場合は true を返してください。
  return false
end

-- ondragleave はファイルやテキストをドラッグしているマウスカーソルが
-- 拡張編集ウィンドウ上から離れた時や、ドラッグ自体が中断された時に呼ばれます。
-- ただし ondragenter や ondragover で false を返していた場合は呼ばれません。
function P.ondragleave()
  -- 戻り値はありません。
end

-- ondrop はマウスボタンが離され、ファイルがドロップされた時に呼ばれます。
-- ただし ondragenter や ondragover で false を返していた場合は呼ばれません。
function P.ondrop(files, state)
  -- files, state の内容は ondragenter と同じです。

  -- ファイルを処理できなかった場合は false を返してください。
  -- false を返すと別のイベントハンドラーに処理が移行します。
  return false

  -- 処理中にユーザーの操作によってキャンセルが要求された場合など、
  -- ファイルを処理せず、かつ別のイベントハンドラーも実行すべきでない場合は
  -- nil を返すと処理を完全に中断することができます。
  -- return nil
  --
  -- 処理できた場合はファイル一覧とマウス位置を返してください。
  -- ドロップされたファイルをそのまま拡張編集ウィンドウにドロップする場合は
  -- ondrop に渡された引数をそのまま渡せば全てドロップされます。
  -- return files, state
  --
  -- 自分でドロップするデータを組み立てる場合は以下を参考にしてください。
  -- なお、処理中に作成／コピーされる一時的ではないファイルは
  -- 全てユーザーが設定したフォルダ内におさまるように配置するのが望ましいです。
  -- それ以外の場所に永続的なファイルを作成するスクリプトを作成する場合は
  -- ユーザーにコンセンサスを得た上で（※）行うようにしてください。
  -- ※ドキュメントなどに記載する、GCMZDrops.confirm で確認する、など
  --
  --   files には以下のような配列（テーブル）を指定します。
  --     files = {
  --       {filepath="C:\Your\File.png"},
  --       {filepath="C:\Your\File2.png"},
  --       ...
  --     }
  --  
  --     [補足情報]
  --       files には配列（テーブル）で複数のファイルを指定できますが、
  --       １つのファイルによってタイムライン上に複数のアイテムが追加される場合、
  --       そのファイル以降は正しくない位置にドロップされることがあります。
  --       （例えば *.exo や動画をドロップした場合に発生します）
  --  
  --   state には以下のようなテーブルを渡します。
  --     state = {
  --       -- ファイルをドロップするマウスカーソル位置
  --       x=120,
  --       y=235,
  --       -- ドロップ処理後に進める拡張編集カーソルのフレーム数
  --       -- ※通常のドロップではカーソルは無関係な位置にあるため、指定する必要はありません
  --       frameadvance=0
  --     }
end

-- ごちゃまぜドロップス上の Lua では以下の関数やテーブルが使用可能です。
-- 処理中には引数や状況に応じてエラーが発生することがあり、それらは pcall で捕捉できます。
--
-- debug_print(str)
--
--   AviUtl 側の Lua で使えるものと同じものです。
--   DebugView https://technet.microsoft.com/ja-jp/sysinternals/debugview.aspx
--   などを使うことで出力内容をリアルタイムに確認することができるため見通しが良くなります。
--
--   [引数]
--     str にはデバッグ出力したい文字列を指定します。
--
--   [戻り値]
--     戻り値はありません。
--
-- scriptdir = GCMZDrops.scriptdir()
--
--   ごちゃまぜドロップス用のスクリプトファイルが格納されている
--   GCMZDrops フォルダーの場所を返します。
--
--   [戻り値]
--     GCMZDrops フォルダーの場所を返します。
--     返されるパスの最後には必ず \ が付与されています。
--
-- filename = GCMZDrops.createfile(name, ext)
--
--   ごちゃまぜドロップスの保存用フォルダに
--   name のファイル名と ext の拡張子を持つファイルを作成します。
--   作成されたファイルは処理中にスクリプトエラーが発生した時、
--   およびドロップがキャンセルされた時は削除されます。
--
--   [引数]
--     name には作成したいファイルの名前を "foo" のような形で指定します。
--     ext には作成したいファイルの拡張子を ".txt" のような形で指定します。
--     既に同じ名前のファイルが存在する場合などには名前は変更されることがあります。
--
--   [戻り値]
--     filename には作成されたファイルの名前を返します。
--
-- filename = GCMZDrops.createtempfile(name, ext)
--
--   Windows の Temp フォルダに
--   name のファイル名と ext の拡張子を持つファイルを作成します。
--   作成されたファイルはドロップ処理の終了後に削除されます。
--
--   [引数]
--     name には作成したいファイルの名前を "foo" のような形で指定します。
--     ext には作成したいファイルの拡張子を ".txt" のような形で指定します。
--     既に同じ名前のファイルが存在する場合などには名前は変更されることがあります。
--
--   [戻り値]
--     filename には作成されたファイルの名前を返します。
--
-- files = GCMZDrops.findallfile(wildcard)
--
--   wildcard に一致するファイルを
--   ごちゃまぜドロップスの保存用フォルダの中から検索します。
--
--   [引数]
--     wildcard には調べたいファイル名を "foo.txt" のような形で指定します。
--     また "foo.*" のような指定をすると
--     "foo.txt" "foo.wav" などのファイルにヒットさせることができます。
--
--   [戻り値]
--     files にはヒットしたファイル名が配列（テーブル）で返します。
--     ひとつもヒットしない場合や検索先フォルダーが存在しない場合は空のテーブルを返します。
--
-- r = GCMZDrops.englishpatched()
--
--   拡張編集が英語化パッチが当たったものかどうかを返します。
--   英語化されている場合 "*.exo" や "*.exa" の内容も変更が必要にあります。
--
--   [戻り値]
--     英語化パッチが当たっている場合は true を返します。
--
-- r = GCMZDrops.getpatchid()
--
--   拡張編集が翻訳版かどうかを返します。
--   翻訳されている場合 "*.exo" や "*.exa" の内容に変更が必要な可能性があります。
--
--   [戻り値]
--     0 - 翻訳パッチなし
--     1 - 英語化パッチあり
--     2 - 中国語簡体字パッチあり
--
-- r = GCMZDrops.needcopy(filepath)
--
--   現在の「処理モード」設定で filepath はコピーが必要かどうかを返します。
--   コピーが必要な場所であってもファイルの拡張子が
--   ".txt" ".exo" ".exa" のいずれかの場合は false を返します。
--
--   [引数]
--     filepath にはコピーが必要かどうかを調べたいファイルを指定します。
--
--   [戻り値]
--     r にはコピーが必要かどうかを返します。
--
-- hash = GCMZDrops.calchash(hash, str)
--
--   str のハッシュ値を計算します。
--
--   [引数]
--     hash には前回得られたハッシュ値を渡します。
--     一番最初の呼び出しの時は 0 を渡してください。
--
--     str にはハッシュ値を計算したいデータを文字列として渡します。
--
--   [戻り値]
--     hash には 64bit のハッシュ値を文字列として返します。
--
-- hash = GCMZDrops.calcfilehash(filepath)
--
--   filepath のハッシュ値を計算します。
--
--   [引数]
--     filepath にはハッシュ値を計算したいファイルパスを渡します。
--
--   [戻り値]
--     hash には 64bit のハッシュ値を文字列として返します。
--
-- str = GCMZDrops.hashtostring(hash)
--
--   ハッシュ値をテキストで表現可能な形式に変換します。
--
--   [引数]
--     hash には calchash/calcfilehash の戻り値を渡してください。
--
--   [戻り値]
--     str にはハッシュ値が base32 エンコードされた文字列を返します。
--
-- fileinfo = GCMZDrops.getexeditfileinfo()
--
--   拡張編集で編集しているプロジェクトに関する情報を取得します。
--
--   [戻り値]
--     fileinfo には以下のようなテーブルを返します。
--     fileinfo = {
--       -- 動画の幅
--       width=1280,
--       -- 動画の高さ
--       height=720,
--       -- 動画のフレームレート(30fpsの時は30、59.97fpsの時は5997など)
--       rate=30,
--       -- 動画のフレームレートのスケール(30fpsの時は1、59.97fpsの時は100など)
--       scale=1,
--       -- 動画の総フレーム数
--       length=456,
--       -- 音声のサンプルレート
--       audio_rate=48000,
--       -- 音声のチャンネル数
--       audio_ch=2
--     }
--
-- fileinfo = GCMZDrops.getfileinfo(filepath)
--
--   *.avi や *.wav などを AviUtl で開いて各種情報を取得します。
--   拡張編集でまだプロジェクトが開かれていない場合はエラーが発生します。
--
--   [引数]
--     filepath には情報を取得したいメディアファイルへのパスを渡します。
--
--   [戻り値]
--     fileinfo には以下のようなテーブルを返します。
--     fileinfo = {
--       -- 動画の幅
--       width=1280,
--       -- 動画の高さ
--       height=720,
--       -- 動画のフレームレート(30fpsの時は30、59.97fpsの時は5997など)
--       rate=30,
--       -- 動画のフレームレートのスケール(30fpsの時は1、59.97fpsの時は100など)
--       scale=1,
--       -- 動画の総フレーム数
--       length=456,
--       -- 音声のサンプルレート
--       audio_rate=48000,
--       -- 音声のチャンネル数
--       audio_ch=2,
--       -- サンプルレートなどをプロジェクトに合わせた場合の音声の総サンプル数
--       audio_samples=123343
--     }
--
-- encstr = GCMZDrops.encodeexotext(str)
--
--   文字列を AviUtl の exo ファイルや exa ファイルで使われる
--   テキストオブジェクトのテキスト用形式にエンコードします。
--
--   [引数]
--     str にはエンコードしたい文字列を Shift_JIS で渡します。
--
--   [戻り値]
--     encstr には
--     "533093306b3061306f30164e4c750000..."
--     のような文字列を返します。
--
-- encstr = GCMZDrops.encodeexotextutf8(str)
--
--   文字列を AviUtl の exo ファイルや exa ファイルで使われる
--   テキストオブジェクトのテキスト用形式にエンコードします。
--
--   [引数]
--     str にはエンコードしたい文字列を UTF-8 で渡します。
--
--   [戻り値]
--     encstr には
--     "533093306b3061306f30164e4c750000..."
--     のような文字列を返します。
--
-- decstr = GCMZDrops.decodeexotextutf8(str)
--
--   AviUtl の exo ファイルや exa ファイルで使われる
--   テキストオブジェクトのテキスト用形式を UTF-8 文字列にデコードします。
--
--   [引数]
--     str には
--     "533093306b3061306f30164e4c750000..."
--     のような文字列を渡します。
--
--   [戻り値]
--     decstr にはデコードされた UTF-8 文字列を返します。
--
-- encstr = GCMZDrops.encodeluastring(str)
--
--   文字列を Lua 上での文字列リテラルに変換します。
--
--   [引数]
--     str にはエンコードしたい文字列を渡します。
--
--   [戻り値]
--     encstr には
--     '"C:\\Your\\File.png"'
--     のように特定の文字がエスケープされ
--     全体がダブルクォートで括られた文字列を返します。
--
-- r = GCMZDrops.isutf8(str)
--
--   str が UTF-8 文字列として問題がないか検証します。
--
--   [引数]
--     str には UTF-8 かどうかを調べたい文字列を指定します。
--
--   [戻り値]
--     r には UTF-8 文字列として問題がない場合は true を返します。
--
-- enc = GCMZDrops.detectencoding(str)
--
--   str のエンコーディングを推測します。
--   この処理は確実に正確な値を返すものではありません。
--   "utf16le" と "utf16be" は BOM がある時のみ返されます。
--   日本語環境以外では期待通りに動作しません。
--
--   [引数]
--     str にはエンコーディングを調べたい文字列を指定します。
--
--   [戻り値]
--     enc には以下の値のいずれかを返します。
--       "sjis" - Shift_JIS
--       "eucjp" - EUC-JP
--       "iso2022jp" - ISO-2022-JP
--       "utf8" - UTF-8
--       "utf16le" - UTF-16LE
--       "utf16be" - UTF-16BE
--       "" - バイナリデータ、推測不明など
--
-- converted = GCMZDrops.convertencoding(str, from, to)
--
--   str のエンコーディングを from から to に変換して返します。
--
--   [引数]
--     str にはエンコーディングを変換したい文字列を指定します。
--     from には str の現在のエンコーディングを "sjis" などの文字列かコードページで指定します。
--     to には str の変換先のエンコーディングを "sjis" などの文字列かコードページで指定します。
--     from が utf8 / utf16le / utf16be のときに BOM がある場合は自動的に除去されます。
--     from や to に "ansi" を渡した場合はアクティブなコードページが利用されます。
--
--   [戻り値]
--     converted にはエンコーディング変換後の文字列を返します。
--
-- r, v = GCMZDrops.prompt(caption, value)
--
--   入力用のダイアログを出してユーザーに入力を要求します。
--
--   [引数]
--     caption には何を入力するための入力欄なのかを文字列で指定します。
--     value には入力欄にデフォルトで入力しておく内容を文字列で指定します。
--
--   [戻り値]
--     r には OK が押されたかどうかを返します。
--     v には入力された内容を文字列で返します。
--
-- r = GCMZDrops.confirm(caption)
--
--   確認用のダイアログを出して、ユーザーに OK とキャンセルの判断を要求します。
--
--   [引数]
--     caption にはユーザーに問いかける内容を文字列で指定します。
--
--   [戻り値]
--     r には OK が押されたかどうかを返します。
--
-- ini = GCMZDrops.inistring(str)
--
--   str を INI ファイルとして解析し、編集用オブジェクトを返します。
--   AviUtl においては *.exo や *.exa を読み込んだり書き換えたりする場合に有用です。
--   INI ファイルについては Wikipedia 等を参照してください。
--
--   [引数]
--     str には INI ファイルの内容が格納された文字列を渡します。
--
--   [戻り値]
--     ini には編集用オブジェクトを返します。
--
--   [編集用オブジェクトの使い方]
--     編集用オブジェクトでは以下のメソッドが使用可能です。
--
--     v = ini:get("セクション", "キー", "デフォルト値")
--
--       セクションとキーに対応した値を返します。
--       データが見つからなかった場合は "デフォルト値" を返します。
--
--     ini:set("セクション", "キー", "値")
--
--       新しく値をセットします。
--       セクションとキーに対応する値が既に存在する場合は上書きされます。
--
--     ini:delete("セクション", "キー")
--
--       セクションとキーに対応した値を削除します。
--
--     ini:deletesection("セクション")
--
--       指定されたセクションを丸ごと削除します。
--
--     arr = ini:sections()
--
--       INI ファイルに存在する全てのセクションを配列で返します。
--
--     arr = ini:keys("セクション")
--
--       セクションに存在する全てのキーを配列で返します。
--
--     found = ini:exists("セクション", "キー")
--
--       指定されたセクションのキーが存在するかどうかを真偽値で返します。
--
--     found = ini:sectionexists("セクション")
--
--       指定されたセクションが存在するかどうかを真偽値で返します。
--
--     tostring(ini)
--
--       内容を INI ファイルとして文字列で返します。
--
-- ini = GCMZDrops.inifile(filepath)
--
--   GCMZDrops.inistring とほぼ同じですが、文字列の代わりにファイルを読み込みます。
--
--   [引数]
--     str には INI ファイルへのパスを文字列で渡します。
--
--   [戻り値]
--     ini には編集用オブジェクトを返します。
--     使い方については GCMZDrops.inistring の説明を参照してください。
--
-- processed = GCMZDrops.drop(files, state)
--
--   拡張編集ウィンドウに向けて、指定されたデータをドロップします。
--   通常の用途ではこの処理を直接呼び出す必要はありません。
--
--   [引数]
--     files には以下のような配列（テーブル）を指定します。
--       files = {
--         {filepath="C:\Your\File.png"},
--         {filepath="C:\Your\File2.png"},
--         ...
--       }
--       [補足情報]
--         files には配列（テーブル）で複数のファイルを指定できますが、
--         １つのファイルによりタイムライン上に複数のアイテムが追加される場合、
--         そのファイル以降は正しくない位置にドロップされることがあります。
--         （例えば *.exo や動画をドロップした場合に発生します）
--
--     state には以下のようなテーブルを渡します。
--       state = {
--         -- ファイルをドロップするマウスカーソル位置
--         x=120,
--         y=235
--       }
--
--   [戻り値]
--     processed はドロップ処理が実際に行われたかどうかを返します。
--     ユーザーによってキャンセルされるなど、何らかの原因で中断されることもあります。
--
-- files = GCMZDrops.getclipboard()
--
--   クリップボードに入っているデータを読み取ります。
--   読み取りルールはドロップされたファイルに対するものと同一です。
--
--   [引数]
--     引数はありません。
--
--   [戻り値]
--     files には以下のような配列（テーブル）を返します。
--       files = {
--         {filepath="C:\Your\File.png"},
--         {filepath="C:\Your\File2.png"},
--         ...
--       }
--     ただし、何らかの理由で読み取り自体に失敗した場合は nil を返します。
--
-- GCMZDrops.deleteonfinish(file)
--
--   ドラッグ＆ドロップ処理が完了した時に削除すべきファイルを登録します。
--   通常の用途ではこの処理を呼び出す必要はありません。
--
--   [引数]
--     file にはドラッグ＆ドロップの完了時に削除したいファイル名を指定します。
--
--   [戻り値]
--     戻り値はありません。
--
-- GCMZDrops.deleteonabort(file)
--
--   処理中にスクリプトエラーが発生した時、
--   およびドロップがキャンセルされた時に削除すべきファイルを登録します。
--   通常の用途ではこの処理を呼び出す必要はありません。
--
--   [引数]
--     file にはエラーやキャンセル時に削除したいファイル名を指定します。
--
--   [戻り値]
--     戻り値はありません。
--
-- GCMZDrops.doevents(msgMin, msgMax)
--
--   実行が長時間に渡るスクリプトを実行する際に、
--   定期的に GCMZDrops.doevents(0, 0) を呼び出すと
--   AviUtl が「応答なし」になるのを回避することができます。
--
--   [引数]
--     msgMin, msgMax には一般的な用途では 0 を渡してください。
--
--   [戻り値]
--     戻り値はありません。
--

return P
