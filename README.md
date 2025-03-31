ごちゃまぜドロップス
====================

ごちゃまぜドロップス（ GCMZDrops.auf ）は拡張編集ウィンドウへファイルやテキストをドラッグ＆ドロップした時の挙動を拡張するための AviUtl プラグインです。

これを導入すると拡張編集ウィンドウへのドラッグ＆ドロップ時の挙動が以下のように変化します。

- ブラウザなどから直接画像をドラッグして画像オブジェクトの作成ができるようになる
  - 設定で指定された場所に画像を保存してから読み込みます
  - 同じファイルが既に保存されている場合は再利用されます
- UTF-8 や EUC-JP のテキストファイルを事前に Shift_JIS に変換してから読み込ませられる
  - 文字コードの自動判定に失敗した場合は化けます
- [7zip](https://7-zip.opensource.jp/) で開いた圧縮ファイル内から直接ドラッグ＆ドロップしてファイルを読み込めるようになる
  - ドラッグ＆ドロップでエクスプローラにファイルを作れるソフトなら他のソフトでも対応しているかも
- 複数ファイルを一度にドロップできるようになる
  - 音声とテキストを同時に投げ込んだりできます
  - ただし動画など元々複数レイヤーに配置されることがあるアイテムがあると正常に動きません
- ブラウザやテキストエディタからテキストを選択してドラッグ＆ドロップでテキストオブジェクトが作れる
  - [字幕アシスト](http://aoytsk.blog.jp/aviutl/1412254.html) の基本機能とほぼ同じですが `[exedit]` `[v]` `[vo]` `[a]` `[ao]` などには非対応
- Lua スクリプトを書くことによって独自の振る舞いを追加可能
  - スクリプトを記述することによって、特定のファイルがドロップされた時の振る舞いを変更したり、  
  Shift キーを押しながらドロップした時に挙動を変更したりできます

それ以外にもいくつか機能を提供します。

- 拡張編集ウィンドウ上で `Shift + Ctrl + 右クリック` で、タイムライン拡張ポップアップメニューを表示  
  - 設定から発動条件を `ホイールクリック` に変更したり、無効化したりできます
  - 初期状態では「クリップボードから貼り付け」スクリプト `clipboard.lua` が実装されています
  - スクリプトを追加することで独自の動作を追加できます
- `*.exo`/`*.exa` を英語や簡体字中国語のパッチを当てた AviUtl で読み込めるようにする
  - これらのファイルには日本語のテキストが書かれており翻訳パッチを当てると互換性がなくなりますが、読み込む前に力技で翻訳することによって読み込めるようにします。
  - 翻訳パッチを当てた AviUtl で作成した `*.exo`/`*.exa` を当てていない環境で読み込む機能は有効化されていません。
- 外部連携 API を追加
  - いくつかの Win32 API を呼び出すことで、別のアプリケーションから拡張編集のタイムラインへのファイルドロップが行えます。
  - 詳しくはこのドキュメント内の `外部連携用 API について` の項目を参照してください。

なお、ごちゃまぜドロップスと[字幕アシスト](http://aoytsk.blog.jp/aviutl/1412254.html)は実装上の都合により同時には使用できません。

ごちゃまぜドロップスの動作には AviUtl version 1.00 以降と拡張編集 version 0.92 以降が必要です。  
また、Windows 10 より前の OS では以下のランタイムをインストールしなければ動作しない場合があります。

https://aka.ms/vs/17/release/vc_redist.x86.exe

注意事項
--------

ごちゃまぜドロップスは無保証で提供されます。  
ごちゃまぜドロップスを使用したこと及び使用しなかったことによるいかなる損害について、開発者は責任を負いません。

これに同意できない場合、あなたはごちゃまぜドロップスを使用することができません。

ダウンロード
------------

https://github.com/oov/aviutl_gcmzdrops/releases

インストール／アンインストール
------------------------------

GCMZDrops.auf と GCMZDrops フォルダーを **exedit.auf と同じ場所** に置いてください。  
（この1行しか書いてないのに、これを読まずに「動かない」と SNS に助けを求める人が結構居て、こんなにも読まれてないんだなあ……って思いますね）

削除時もそれらを削除するだけで OK です。

設定
----

AviUtl の `表示` メニューから `ごちゃまぜドロップスの表示` を選ぶと、設定用のウィンドウが開きます。  
この設定は AviUtl のプロジェクトファイル(*.aup) に保存されるため、プロジェクトごとに固有の設定ができます。

![設定のスクリーンショット](img/setting.png "設定のスクリーンショット")

### 処理モード

ブラウザから画像をドラッグ＆ドロップなどで持ち込んだ場合など、ドラッグ元の場所などによってはデータがファイルとして存在しない場合や、将来的に削除される場所から読み込んでしまう可能性があります。  
そのようなファイルを安全に読み込むために、ごちゃまぜドロップスでは必要に応じてファイルを安全な場所にコピーしてから読み込む場合があります。

ここで選択する処理モードによって、どのような時にファイルをコピーするかが決定されます。  
それぞれのモードの動作は以下の通りです。

- 自動判定
  - 読み込むファイルが以下のフォルダの階層以下にある場合に、「データ保存先」にコピーしてから読み込まれます。  
  通常はこの設定で使うことを想定しています。
    - Temp フォルダ
    - CSIDL_APPDATA
    - CSIDL_LOCAL_APPDATA
    - CSIDL_COMMON_APPDATA
    - CSIDL_COOKIES
    - CSIDL_INTERNET_CACHE
    - CSIDL_LOCAL_APPDATA
    - CSIDL_PROGRAM_FILES
    - CSIDL_PROGRAM_FILES_COMMON
    - CSIDL_STARTMENU
    - CSIDL_PROGRAMS
    - CSIDL_WINDOWS
    - CSIDL_SYSTEM
- コピーを作成
  - 常に「データ保存先」にコピーしてから読み込まれます。
- 直接読み込み
  - なるべく直接読み込みます。

なお、どの動作モードを選んだとしてもドラッグ元がファイルではない場合には「データ保存先」にファイルをコピーしなければならないケースがあります。

### データ保存先

ドラッグ元がファイルではなかった場合や、上記の「処理モード」の選択によりコピーしてから読み込む場合のコピー先になるフォルダーです。

初期値は `%PROJECTDIR%` で、プロジェクトファイルが保存されているフォルダーにファイルを保存します。

なお特別なキーワード `%PROJECTDIR%` を使用する場合は事前にプロジェクトファイルが保存されている必要があり、保存されていない場合はスクリプトなどでエラーが発生します。
また、プロジェクトファイルを別の場所に保存し直しても、以前ドロップしたファイルが自動で移動されることはありません。

### 初期設定に戻す

処理モードを `自動` に、データ保存先を `%PROJECTDIR%` に変更します。

### デフォルトに戻す

処理モードとデータ保存先をデフォルト設定に戻します。

### 現在の設定をデフォルトにする

現在の処理モードとデータ保存先を、新規プロジェクト作成時のデフォルト値に設定します。

例えばデータ保存先を `%PROJECTDIR%\gcmz` などのデフォルト値にすると、プロジェクトファイルの場所が変わるのに合わせて保存先を変えるような使い方ができます。

### タイムライン拡張ポップアップメニュー

タイムライン上で `Shift + Ctrl + 右クリック` を押したときに表示されるポップアップメニューに関する設定です。  
この設定を変更することで発動方法を変更できます。

この設定はプロジェクト固有ではなく、AviUtl 全体に適用されます。

### 外部連携API

他のソフトウェアからのファイルの投げ込みを受け付けるための外部連携APIを使用するかどうかを設定します。

AviUtl を複数起動していても、有効にできる外部連携 API はひとつだけです。  
そのため、２つ目以降に起動した AviUtl は `状態` が `エラーが発生しました` になります。  
このような場合でも、全ての AviUtl でチェックを外し、使いたい AviUtl にだけチェックを入れれば複数起動時にも連動先の AviUtl を選ぶことができます。

外部連携 API を使わない場合は `状態` が `エラーが発生しました` になっていても無視して構いません。

この設定はプロジェクト固有ではなく、AviUtl 全体に適用されます。

スクリプトの書き方
------------------

`GCMZDrops` フォルダーの中に `*.lua` ファイルを入れることで、ドロップされたファイルに対して独自の振る舞いを追加することができます。

最も基本的な雛形として参考になるのは `generic.lua` で、ドロップされるファイルを全て受け入れ、そのまま AviUtl に渡すだけのスクリプトです。  
Lua スクリプト内で使えるごちゃまぜドロップス専用の関数などは `example.lua` を参照してください。

また、`dropper` フォルダの中にスクリプトファイルを配置することで、拡張編集ウィンドウ上で `Shift + Ctrl + 右クリック` をした時にメニューを表示し、ファイルを投げ込むためのスクリプトを記述することが可能です。  
ごちゃまぜドロップスでは通常の場合「拡張編集ウィンドウにファイルなどをドラッグで持ち込んだ時」というのが Lua スクリプトの動作の起点になりますが、この仕組みを使うとファイルを持ち込まなくても動作の起点にすることができます。

外部連携用 API について
-----------------------

外部のアプリケーションから拡張編集の現在のカーソル位置へのファイルドロップを実現するために、実験的な外部連携用 API を提供しています。  
ここで紹介する API は ごちゃまぜドロップス v0.3.12 以降で使用可能です。

この API は試験運用中のため、予告なく変更または削除されることがあります。

### 使用例

C言語から呼び出す例です。

```c
#include <stdint.h>
#include <stdio.h>

#define UNICODE
#include <Windows.h>

struct GCMZDropsData {
  uint32_t Window;
  int32_t Width;
  int32_t Height;
  int32_t VideoRate;
  int32_t VideoScale;
  int32_t AudioRate;
  int32_t AudioCh;
  int32_t GCMZAPIVer; /* 1 = v0.3.12 以降 / 2 = v0.3.23 以降 */
  wchar_t ProjectPath[MAX_PATH];
  uint32_t Flags; /* GCMZAPIVer が 2 以上なら存在する */
};

int main(int argc, char *argv[]) {
  HANDLE hMutex = NULL;
  HANDLE hFMO = NULL;
  struct GCMZDropsData *p = NULL;
  BOOL mutexLocked = FALSE;

  hMutex = OpenMutex(MUTEX_ALL_ACCESS, FALSE, TEXT("GCMZDropsMutex"));
  if (hMutex == NULL) {
    printf("OpenMutex failed.\n");
    goto Cleanup;
  }

  hFMO = OpenFileMapping(FILE_MAP_READ, FALSE, TEXT("GCMZDrops"));
  if (hFMO == NULL) {
    printf("OpenFileMapping failed.\n");
    goto Cleanup;
  }

  p = MapViewOfFile(hFMO, FILE_MAP_READ, 0, 0, 0);
  if (p == NULL) {
    printf("MapViewOfFile failed.\n");
    goto Cleanup;
  }

  if (WaitForSingleObject(hMutex, INFINITE) != WAIT_OBJECT_0) {
    printf("WaitForSingleObject failed.\n");
    goto Cleanup;
  }
  mutexLocked = TRUE;

  if (!p->Window) {
    printf("The target window is NULL.\n");
    goto Cleanup;
  }

  if (!p->Width) {
    printf("The project is not open.\n");
    goto Cleanup;
  }

  printf("GCMZAPIVer: %d\n", p->GCMZAPIVer);
  printf("ProjectPath(%d): %ls\n", (int)wcslen(p->ProjectPath), p->ProjectPath);
  printf("Window: %d\n", p->Window);
  printf("Width: %d\n", p->Width);
  printf("Height: %d\n", p->Height);
  printf("VideoRate: %d\n", p->VideoRate);
  printf("VideoScale: %d\n", p->VideoScale);
  printf("AudioRate: %d\n", p->AudioRate);
  printf("AudioCh: %d\n", p->AudioCh);

  if (p->GCMZAPIVer >= 2) {
    /* Flags にアクセスできるのは GCMZAPIVer が 2 以上のときだけ */
    printf("Flags: %d\n", (int)p->Flags);
    if (p->Flags & 1) {
      /* 英語化パッチが当たっている拡張編集だった */
      printf("  English Patched\n");
    }
    if (p->Flags & 2) {
      /* 中国語簡体字パッチが当たっている拡張編集だった */
      printf("  Simplified Chinese Patched\n");
    }
  }

  /*
    GCMZAPIVer が 0 のときは仕様が異なるため動作しません
    その場合は対応せずにバージョンアップを呼びかけるようにしてください
  */
  if (p->GCMZAPIVer == 0) {
    printf("GCMZDrops too old, please update to v0.3.12 or later.\n");
    goto Cleanup;
  }

  {
    COPYDATASTRUCT cds = {0};
    /* 必ず 1 を指定してください */
    cds.dwData = 1;

    /*
      JSON を UTF-8 エンコーディングで渡します
      layer:
        ドロップするレイヤーを決めます。
        指定を省略することはできません。
        -1 ～ -100
            拡張編集上での現在の表示位置からの相対位置へ挿入
            例: 縦スクロールによって一番上に見えるレイヤーが Layer 3 のとき、-1 を指定すると Layer 3、-2 を指定すると Layer 4 へ挿入
        1 ～  100
            スクロール位置に関わらず指定したレイヤー番号へ挿入
      frameAdvance:
        ファイルのドロップした後、指定されたフレーム数だけカーソルを先に進めます。
        進める必要がない場合は省略可能です。
      files:
        投げ込むファイルへのフルパスを配列で渡します。
        ファイル名は UTF-8 にする必要がありますが、拡張編集の仕様上 ShiftJIS の範囲内の文字しか扱えません。
    */
    cds.lpData = "{\"layer\":-1,\"frameAdvance\":12,\"files\":[\"C:\\\\test.bmp\"]}";
    cds.cbData = strlen(cds.lpData);

    /*
        API を呼び出します
        WM_COPYDATA の仕様に従って、WPARAM には データを渡すウィンドウへのハンドル、LPARAM には COPYDATASTRUCT 構造体へのポインタを渡します。
        https://learn.microsoft.com/ja-jp/windows/win32/dataxchg/wm-copydata
        cds.dwData の値が間違っている場合や JSON がおかしい場合など、
        API としての送信フォーマットに問題がある場合には OutputDebugString でエラーメッセージを出力します。
        ドロップするファイルが見つからないなど、ファイルの内容に問題がある場合はメッセージボックスで表示します。
    */
    SendMessage((HWND)(intptr_t)p->Window, WM_COPYDATA, (WPARAM)(GetConsoleWindow()), (LPARAM)&cds);
  }

Cleanup:
  if (p) {
    if (!UnmapViewOfFile(p)) {
      printf("UnmapViewOfVile failed.\n");
    }
  }
  if (mutexLocked) {
    if (!ReleaseMutex(hMutex)) {
      printf("ReleaseMutex failed.\n");
    }
  }
  if (hFMO) {
    if (!CloseHandle(hFMO)) {
      printf("CloseHandle failed.\n");
    }
  }
  if (hMutex) {
    if (!CloseHandle(hMutex)) {
      printf("CloseHandle failed.\n");
    }
  }
  return 0;
}
```

この API を使用する上での一般的な注意事項は以下の通りです。

- **拡張編集ウィンドウについて**  
拡張編集ウィンドウが表示されていないとアイテムの挿入位置を判定できずに API の実行に失敗することがあります。  
また、レイヤーの高さやタイムラインの表示倍率などの検出に失敗した場合はカレントディレクトリに `gcmz-apierr-20191231-235959.bmp` のようなファイル名で失敗時の画像を保存します。
- **挿入位置について**  
挿入先に既にオブジェクトがある場合など、十分なスペースがない場合は想定した場所に挿入されません。
- **複数ファイルのドロップについて**  
一応対応していますが、十分なスペースがない場合は一部のファイルだけがずれた位置に配置されます。  
また、動画や `*.exo` ファイルのようにタイムラインに複数行挿入されるアイテムがある場合、それ以降は正しい位置に挿入されません。
- **タイムラインの表示倍率について**  
正しい位置にファイルをドロップするためには拡張編集のタイムラインを一定以上の拡大率にする必要があるため、条件を満たしていない場合は一時的にタイムラインが拡大されます。
- **多重起動について**  
API が使えるのは最初に起動したインスタンスのみで、AviUtl を多重起動しても２つ目以降では API は無効状態になり、その旨を伝えるエラーダイアログが表示されます。

FAQ
---

- Q. テキストをドロップで投げ込めないソフトがある
  - A. 対応していないソフトもある
- Q. 投げ込むとファイル名の後ろにゴミがつく
  - A. ゴミではない
- Q. 上手くドラッグ＆ドロップできないソフトがある
  - A. そういうソフトもあるだろう
- Q. ドラッグ元やドロップ先のソフトが落ちる
  - A. そういうソフトもあるかも知れない
- Q. エラーが出る
  - A. エラー内容と、AviUtl / 拡張編集 / ごちゃまぜドロップスのバージョンと、  
       ごちゃまぜドロップスの最新版でも起こるかどうかを教えて下さい
  - A. 拡張編集の UI の色を変えるようなプラグインとは競合する可能性があります  
       もし変えている場合は戻してみてください
- Q. 「AviUtl で使用できない文字がファイル名に含まれています」というエラーが出る
  - A. これは AviUtl で使用できない文字がファイル名に含まれているのが原因でエラーが出ています。
       ごちゃまぜドロップスが扱えないのではなく、AviUtl や拡張編集で扱えないという話です。
       ごちゃまぜドロップスを消せばエラーメッセージは出ませんが、ファイルは読み込めません。
       つまり単なる親切心で読み込み前にチェックし、問題がある場合に理由を通知しているだけです。
       問題がある文字がなくなるようにファイル名を変えれば読み込めるようになります。
- Q. 外部連携APIの状態が「エラーが発生しました」になる
  - A. 困ってないか、何の話かわからないなら無視して大丈夫です。

バイナリのビルドについて
------------------------

Windows の Git Bash で以下のコマンドを入力してください。  
必要なコンパイラーなども含めて自動的に準備されリリース用バイナリーが生成されます。

```bash
$ git clone --depth 1 --recursive https://github.com/oov/aviutl_gcmzdrops
$ cd aviutl_gcmzdrops
$ bash build.bash
$ ls -la build/Release/bin
```

Contributors
------------

- Nsyw

Credits
-------

ごちゃまぜドロップス is made possible by the following open source softwares.

### [Acutest](https://github.com/mity/acutest)

<details>
<summary>The MIT License</summary>

```
The MIT License (MIT)

Copyright © 2013-2019 Martin Mitáš

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the “Software”),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
IN THE SOFTWARE.
```
</details>

### [AviUtl Plugin SDK](http://spring-fragrance.mints.ne.jp/aviutl/)

<details>
<summary>The MIT License</summary>

```
The MIT License

Copyright (c) 1999-2012 Kenkun

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
```
</details>

### [crc64.c](https://github.com/redis/redis/blob/2.6/src/crc64.c)

<details>
<summary>The 3-Clause BSD License</summary>

```
Copyright (c) 2012, Salvatore Sanfilippo <antirez at gmail dot com>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.
  * Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
  * Neither the name of Redis nor the names of its contributors may be used
    to endorse or promote products derived from this software without
    specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
```
</details>

### [detect.c](https://github.com/monochromegane/the_platinum_searcher)

<details>
<summary>The MIT License</summary>

```
Copyright (c) [2014] [the_platinum_searcher]

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
</details>

### [hashmap.c](https://github.com/tidwall/hashmap.c)

> [!NOTE]
> This program/library used [a modified version of hashmap.c](https://github.com/oov/hashmap.c/tree/simplify).

<details>
<summary>The MIT License (MIT)</summary>

```
Copyright (c) 2020 Joshua J Baker

Permission is hereby granted, free of charge, to any person obtaining a copy of
this software and associated documentation files (the "Software"), to deal in
the Software without restriction, including without limitation the rights to
use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
the Software, and to permit persons to whom the Software is furnished to do so,
subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```
</details>

### [Lua](http://www.lua.org/)

<details>
<summary>The MIT License (MIT)</summary>

```
Copyright (C) 1994-2003 Tecgraf, PUC-Rio.  All rights reserved.

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```
</details>

### [Mingw-w64](https://github.com/mingw-w64/mingw-w64)

<details>
<summary>MinGW-w64 runtime licensing</summary>

```
MinGW-w64 runtime licensing
***************************

This program or library was built using MinGW-w64 and statically
linked against the MinGW-w64 runtime. Some parts of the runtime
are under licenses which require that the copyright and license
notices are included when distributing the code in binary form.
These notices are listed below.


========================
Overall copyright notice
========================

Copyright (c) 2009, 2010, 2011, 2012, 2013 by the mingw-w64 project

This license has been certified as open source. It has also been designated
as GPL compatible by the Free Software Foundation (FSF).

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

   1. Redistributions in source code must retain the accompanying copyright
      notice, this list of conditions, and the following disclaimer.
   2. Redistributions in binary form must reproduce the accompanying
      copyright notice, this list of conditions, and the following disclaimer
      in the documentation and/or other materials provided with the
      distribution.
   3. Names of the copyright holders must not be used to endorse or promote
      products derived from this software without prior written permission
      from the copyright holders.
   4. The right to distribute this software or to use it for any purpose does
      not give you the right to use Servicemarks (sm) or Trademarks (tm) of
      the copyright holders.  Use of them is covered by separate agreement
      with the copyright holders.
   5. If any files are modified, you must cause the modified files to carry
      prominent notices stating that you changed the files and the date of
      any change.

Disclaimer

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS ``AS IS'' AND ANY EXPRESSED
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO
EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,
OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,
EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

======================================== 
getopt, getopt_long, and getop_long_only
======================================== 

Copyright (c) 2002 Todd C. Miller <Todd.Miller@courtesan.com> 
 
Permission to use, copy, modify, and distribute this software for any 
purpose with or without fee is hereby granted, provided that the above 
copyright notice and this permission notice appear in all copies. 
 	 
THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

Sponsored in part by the Defense Advanced Research Projects
Agency (DARPA) and Air Force Research Laboratory, Air Force
Materiel Command, USAF, under agreement number F39502-99-1-0512.

        *       *       *       *       *       *       * 

Copyright (c) 2000 The NetBSD Foundation, Inc.
All rights reserved.

This code is derived from software contributed to The NetBSD Foundation
by Dieter Baron and Thomas Klausner.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE NETBSD FOUNDATION, INC. AND CONTRIBUTORS
``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE FOUNDATION OR CONTRIBUTORS
BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.


===============================================================
gdtoa: Converting between IEEE floating point numbers and ASCII
===============================================================

The author of this software is David M. Gay.

Copyright (C) 1997, 1998, 1999, 2000, 2001 by Lucent Technologies
All Rights Reserved

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the name of Lucent or any of its entities
not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior
permission.

LUCENT DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
IN NO EVENT SHALL LUCENT OR ANY OF ITS ENTITIES BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.

        *       *       *       *       *       *       *

The author of this software is David M. Gay.

Copyright (C) 2005 by David M. Gay
All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that the copyright notice and this permission notice and warranty
disclaimer appear in supporting documentation, and that the name of
the author or any of his current or former employers not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission.

THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN
NO EVENT SHALL THE AUTHOR OR ANY OF HIS CURRENT OR FORMER EMPLOYERS BE
LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

        *       *       *       *       *       *       *

The author of this software is David M. Gay.

Copyright (C) 2004 by David M. Gay.
All Rights Reserved
Based on material in the rest of /netlib/fp/gdota.tar.gz,
which is copyright (C) 1998, 2000 by Lucent Technologies.

Permission to use, copy, modify, and distribute this software and
its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all
copies and that both that the copyright notice and this
permission notice and warranty disclaimer appear in supporting
documentation, and that the name of Lucent or any of its entities
not be used in advertising or publicity pertaining to
distribution of the software without specific, written prior
permission.

LUCENT DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.
IN NO EVENT SHALL LUCENT OR ANY OF ITS ENTITIES BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
THIS SOFTWARE.


=========================
Parts of the math library
=========================

Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.

Developed at SunSoft, a Sun Microsystems, Inc. business.
Permission to use, copy, modify, and distribute this
software is freely granted, provided that this notice
is preserved.

        *       *       *       *       *       *       *

Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.

Developed at SunPro, a Sun Microsystems, Inc. business.
Permission to use, copy, modify, and distribute this
software is freely granted, provided that this notice
is preserved.

        *       *       *       *       *       *       *

FIXME: Cephes math lib
Copyright (C) 1984-1998 Stephen L. Moshier

It sounds vague, but as to be found at
<http://lists.debian.org/debian-legal/2004/12/msg00295.html>, it gives an
impression that the author could be willing to give an explicit
permission to distribute those files e.g. under a BSD style license. So
probably there is no problem here, although it could be good to get a
permission from the author and then add a license into the Cephes files
in MinGW runtime. At least on follow-up it is marked that debian sees the
version a-like BSD one. As MinGW.org (where those cephes parts are coming
from) distributes them now over 6 years, it should be fine.

===================================
Headers and IDLs imported from Wine
===================================

Some header and IDL files were imported from the Wine project. These files
are prominent maked in source. Their copyright belongs to contributors and
they are distributed under LGPL license.

Disclaimer

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.
```
</details>

### [nanoprintf](https://github.com/charlesnicholson/nanoprintf)

> [!NOTE]
> This program/library used [a modified version of nanoprintf](https://github.com/oov/nanoprintf/tree/custom).

<details>
<summary>UNLICENSE</summary>

```
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org>
```
</details>

### [stb_image.h](https://github.com/nothings/stb)

<details>
<summary>UNLICENSE</summary>

```
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
software, either in source code form or as a compiled binary, for any purpose,
commercial or non-commercial, and by any means.

In jurisdictions that recognize copyright laws, the author or authors of this
software dedicate any and all copyright interest in the software to the public
domain. We make this dedication for the benefit of the public at large and to
the detriment of our heirs and successors. We intend this dedication to be an
overt act of relinquishment in perpetuity of all present and future rights to
this software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```
</details>

### [TinyCThread](https://github.com/tinycthread/tinycthread)

> [!NOTE]
> This program/library used [a modified version of TinyCThread](https://github.com/oov/tinycthread).

<details>
<summary>The zlib/libpng License</summary>

```
Copyright (c) 2012 Marcus Geelnard
              2013-2016 Evan Nemerson

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

    1. The origin of this software must not be misrepresented; you must not
    claim that you wrote the original software. If you use this software
    in a product, an acknowledgment in the product documentation would be
    appreciated but is not required.

    2. Altered source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

    3. This notice may not be removed or altered from any source
    distribution.
```
</details>

### [yyjson](https://github.com/ibireme/yyjson)

<details>
<summary>MIT License</summary>

```
MIT License

Copyright (c) 2020 YaoYuan <ibireme@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
</details>
