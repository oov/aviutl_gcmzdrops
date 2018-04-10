unit LuaFuncs;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  lua;

function LuaSetFuncs(L: Plua_State): integer; cdecl;

implementation

uses
  Windows, SysUtils, Classes, crc, AviUtl, Main, UsedFiles, DropTarget, LuaObj, LuaIni, Util;

function LuaReturn(L: Plua_State; const Ret: integer): integer;
begin
  if Ret = -1 then
    // *** IMPORTANT ***
    // Both Lua and FreePascal are using setjmp/longjmp for error/exception handling.
    // So while using lua_error(luaL_error), we cannot use try statement, any string type, etc.
    // If we do not follow this rule the program crashes in random places.
    Result := luaL_error(L, '%s', lua_tostring(L, -1))
  else
    Result := Ret;
end;

function LuaPushError(L: Plua_State; E: Exception): integer;
var
  SJIS: ShiftJISString;
begin
  SJIS := E.Message;
  lua_pushlstring(L, @SJIS[1], Length(SJIS));
  Result := -1;
end;

function LuaDebugPrint(L: Plua_State): integer; cdecl;
var
  SJIS: ShiftJISString;
begin
  SJIS := lua_tostring(L, -1);
  ODS('%s', [SJIS]);
  Result := 0;
end;


function LuaScriptDir(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SJIS: ShiftJISString;
  begin
    try
      SJIS := ShiftJISString(ExtractFilePath(GetDLLName()) + 'GCMZDrops\');
      lua_pushlstring(L, @SJIS[1], Length(SJIS));
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaCreateFile(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SJIS: ShiftJISString;
    Name, Ext: UTF8String;
    WFS: TWideFileStream;
  begin
    try
      SJIS := lua_tostring(L, -2);
      Name := SJIS;
      Name := Sanitize(ExtractFileName(Name), '-');
      if Name = '' then
        raise Exception.Create('name is empty');

      SJIS := lua_tostring(L, -1);
      Ext := SJIS;
      Ext := StringReplace(Sanitize(Ext, '-'), '\', '-', [rfReplaceAll]);

      lua_pop(L, 2);

      WFS := TWideFileStream.CreateUnique(IncludeTrailingPathDelimiter(
        WideString(GCMZDrops.GetSavePath())) + WideString(Name), WideString(Ext));
      try
        SJIS := ShiftJISString(WFS.FilePath);
        GCMZDrops.RegisterDeleteOnAbort(UTF8String(WFS.FilePath));
      finally
        WFS.Free;
      end;
      lua_pushlstring(L, @SJIS[1], Length(SJIS));
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaCreateTempFile(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SJIS: ShiftJISString;
    Name, Ext: UTF8String;
    TempPath: WideString;
    WFS: TWideFileStream;
  begin
    try
      SJIS := lua_tostring(L, -2);
      Name := SJIS;
      Name := Sanitize(ExtractFileName(Name), '-');
      if Name = '' then
        raise Exception.Create('name is empty');

      SJIS := lua_tostring(L, -1);
      Ext := SJIS;
      Ext := StringReplace(Sanitize(Ext, '-'), '\', '-', [rfReplaceAll]);

      lua_pop(L, 2);

      SetLength(TempPath, MAX_PATH);
      GetTempPathW(MAX_PATH, @TempPath[1]);
      TempPath := PWideChar(TempPath);

      WFS := TWideFileStream.CreateUnique(IncludeTrailingPathDelimiter(
        TempPath) + WideString(Name), WideString(Ext));
      try
        SJIS := ShiftJISString(WFS.FilePath);
        GCMZDrops.RegisterDeleteOnFinish(UTF8String(WFS.FilePath));
      finally
        WFS.Free;
      end;
      lua_pushlstring(L, @SJIS[1], Length(SJIS));
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaFindAllFile(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SJIS: ShiftJISString;
    WildCard, BasePath: WideString;
    sr: TUnicodeSearchRec;
    I: integer;
  begin
    try
      SJIS := lua_tostring(L, -1);
      lua_pop(L, 1);
      WildCard := WideString(SJIS);
      if WildCard = '' then
        raise Exception.Create('wildcard is empty');
      WildCard := ExtractFileName(WildCard);

      BasePath := WideString(IncludeTrailingPathDelimiter(GCMZDrops.GetSavePath()));
      lua_newtable(L);
      I := 1;
      if FindFirst(BasePath + WildCard, 0, sr) = 0 then
      begin
        repeat
          if (sr.Name = '..') or (sr.Name = '.') or
            ((sr.Attr and faDirectory) = faDirectory) then
            continue;
          SJIS := ShiftJISString(BasePath + sr.Name);
          lua_pushlstring(L, @SJIS[1], Length(SJIS));
          lua_rawseti(L, -2, I);
          Inc(I);
        until FindNext(sr) <> 0;
      end;
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaNeedCopy(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SJIS: ShiftJISString;
    FilePath: UTF8String;
  begin
    try
      SJIS := lua_tostring(L, -1);
      lua_pop(L, 1);
      FilePath := SJIS;
      lua_pushboolean(L, GCMZDrops.NeedCopy(FilePath));
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaCalcHash(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    P: PChar;
    Len: size_t;
    Q: QWORD;
  begin
    try
      if lua_isstring(L, -2) then
      begin
        P := lua_tolstring(L, -2, @Len);
        if Len <> SizeOf(Q) then
          raise Exception.Create('unexpected hash value');
        Q := PQWord(P)^;
      end
      else
        Q := crc64(0, nil, 0);
      P := lua_tolstring(L, -1, @Len);
      Q := crc64(Q, PByte(P), Len);
      lua_pop(L, 2);
      lua_pushlstring(L, PChar(@Q), SizeOf(Q));
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaCalcFileHash(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SJIS: ShiftJISString;
    FileName: UTF8String;
    Q: QWORD;
  begin
    try
      SJIS := lua_tostring(L, -1);
      lua_pop(L, 1);
      FileName := SJIS;
      Q := CalcFileHash(WideString(FileName));
      lua_pushlstring(L, PChar(@Q), SizeOf(Q));
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaHashToString(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    P: PChar;
    Len: size_t;
    Q: QWORD;
    h: UTF8String;
  begin
    try
      P := lua_tolstring(L, -1, @Len);
      if Len <> SizeOf(Q) then
        raise Exception.Create('unexpected hash value');
      Q := PQWord(P)^;
      lua_pop(L, 1);

      SetLength(h, 8);
      h[1] := char(Q shr 56);
      h[2] := char(Q shr 48);
      h[3] := char(Q shr 40);
      h[4] := char(Q shr 32);
      h[5] := char(Q shr 24);
      h[6] := char(Q shr 16);
      h[7] := char(Q shr 8);
      h[8] := char(Q);
      h := Base32Encode(h);
      h := StringReplace(h, '=', '', [rfReplaceAll]);
      lua_pushlstring(L, @h[1], Length(h));
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaGetExEditFileInfo(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    FI: TFileInfo;
  begin
    try
      GCMZDrops.GetExEditFileInfo(FI);
      lua_newtable(L);
      lua_pushinteger(L, FI.Width);
      lua_setfield(L, -2, 'width');
      lua_pushinteger(L, FI.Height);
      lua_setfield(L, -2, 'height');
      lua_pushinteger(L, FI.VideoRate);
      lua_setfield(L, -2, 'rate');
      lua_pushinteger(L, FI.VideoScale);
      lua_setfield(L, -2, 'scale');
      lua_pushinteger(L, FI.FrameN);
      lua_setfield(L, -2, 'length');
      lua_pushinteger(L, FI.AudioRate);
      lua_setfield(L, -2, 'audio_rate');
      lua_pushinteger(L, FI.AudioCh);
      lua_setfield(L, -2, 'audio_ch');
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaGetFileInfo(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    FI: TFileInfo;
    Samples: integer;
    SJIS: ShiftJISString;
  begin
    try
      SJIS := lua_tostring(L, -1);
      GCMZDrops.GetFileInfo(FI, Samples, UTF8String(SJIS));
      lua_newtable(L);
      lua_pushinteger(L, FI.Width);
      lua_setfield(L, -2, 'width');
      lua_pushinteger(L, FI.Height);
      lua_setfield(L, -2, 'height');
      lua_pushinteger(L, FI.VideoRate);
      lua_setfield(L, -2, 'rate');
      lua_pushinteger(L, FI.VideoScale);
      lua_setfield(L, -2, 'scale');
      lua_pushinteger(L, FI.FrameN);
      lua_setfield(L, -2, 'length');
      lua_pushinteger(L, FI.AudioRate);
      lua_setfield(L, -2, 'audio_rate');
      lua_pushinteger(L, FI.AudioCh);
      lua_setfield(L, -2, 'audio_ch');
      lua_pushinteger(L, Samples);
      lua_setfield(L, -2, 'audio_samples');
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaEncodeExoText(L: Plua_State): integer; cdecl;

  function Main(): integer;
  const
    Chars: array[0..15] of char =
      ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
  var
    SJIS: ShiftJISString;
    WS: WideString;
    W: word;
    S, D, Len: integer;
  begin
    try
      SJIS := lua_tostring(L, -1);
      WS := WideString(SJIS);
      lua_pop(L, 1);
      SetLength(SJIS, 1024 * 4);
      FillChar(SJIS[1], Length(SJIS), '0');
      Len := Length(WS);
      if Len > 1024 then
        Len := 1024;
      D := 1;
      for S := 1 to Len do
      begin
        W := word(WS[S]);
        SJIS[D + 0] := Chars[(W shr 4) and $0f];
        SJIS[D + 1] := Chars[(W shr 0) and $0f];
        SJIS[D + 2] := Chars[(W shr 12) and $0f];
        SJIS[D + 3] := Chars[(W shr 8) and $0f];
        Inc(D, 4);
      end;
      lua_pushlstring(L, @SJIS[1], Length(SJIS));
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaEncodeExoTextUTF8(L: Plua_State): integer; cdecl;

  function Main(): integer;
  const
    Chars: array[0..15] of char =
      ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f');
  var
    U8: UTF8String;
    WS: WideString;
    W: word;
    S, D, Len: integer;
  begin
    try
      U8 := lua_tostring(L, -1);
      WS := WideString(U8);
      lua_pop(L, 1);
      SetLength(U8, 1024 * 4);
      FillChar(U8[1], Length(U8), '0');
      Len := Length(WS);
      if Len > 1024 then
        Len := 1024;
      D := 1;
      for S := 1 to Len do
      begin
        W := word(WS[S]);
        U8[D + 0] := Chars[(W shr 4) and $0f];
        U8[D + 1] := Chars[(W shr 0) and $0f];
        U8[D + 2] := Chars[(W shr 12) and $0f];
        U8[D + 3] := Chars[(W shr 8) and $0f];
        Inc(D, 4);
      end;
      lua_pushlstring(L, @U8[1], Length(U8));
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaDecodeExoTextUTF8(L: Plua_State): integer; cdecl;

  function Main(): integer;
  const
    Table: array[byte] of byte = (
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $1, $2, $3, $4, $5, $6, $7, $8, $9, $0, $0, $0, $0, $0, $0,
      $0, $a, $b, $c, $d, $e, $f, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $a, $b, $c, $d, $e, $f, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0);
  var
    U8: UTF8String;
    WS: WideString;
    S: PByte;
    D: integer;
  begin
    try
      S := PByte(lua_tostring(L, -1));
      SetLength(WS, StrLen(PChar(S)) div 4);
      for D := 1 to Length(WS) do
      begin
        WS[D] := WideChar((Table[(S+0)^] shl 4) or (Table[(S+1)^] shl 0) or (Table[(S+2)^] shl 12) or (Table[(S+3)^] shl 8));
        Inc(S, 4);
      end;
      lua_pop(L, 1);
      SetLength(WS, StrLen(PWideChar(@WS[1])));
      U8 := UTF8String(WS);
      lua_pushlstring(L, @U8[1], Length(U8));
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaEncodeLuaString(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SJIS, Escaped: ShiftJISString;
    C: char;
    S, D: integer;
  begin
    try
      SJIS := lua_tostring(L, -1);
      lua_pop(L, 1);
      SetLength(Escaped, Length(SJIS) * 2 + 2);
      Escaped[1] := '"';
      D := 2;
      for S := 1 to Length(SJIS) do
      begin
        case SJIS[S] of
          #$07: C := 'a';
          #$08: C := 'b';
          #$09: C := 't';
          #$0a: C := 'n';
          #$0b: C := 'v';
          #$0c: C := 'f';
          #$0d: C := 'r';
          #$22: C := #$22;
          #$27: C := #$27;
          #$5b: C := #$5b;
          #$5c: C := #$5c;
          #$5d: C := #$5d;
          else
          begin
            Escaped[D] := SJIS[S];
            Inc(D);
            continue;
          end;
        end;
        Escaped[D + 0] := '\';
        Escaped[D + 1] := C;
        Inc(D, 2);
      end;
      Escaped[D] := '"';
      lua_pushlstring(L, @Escaped[1], D);
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaDetectEncoding(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    P: PChar;
    Len: size_t;
  begin
    try
      P := lua_tolstring(L, -1, @Len);
      case DetectEncoding(P, Len) of
        932: lua_pushstring(L, 'sjis');
        20932: lua_pushstring(L, 'eucjp');
        50222: lua_pushstring(L, 'iso2022jp');
        CP_UTF8: lua_pushstring(L, 'utf8');
        else
          lua_pushstring(L, '');
      end;
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaConvertEncoding(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SrcCP, DestCP: UINT;
    Src: PChar;
    SrcLen, WideLen, DestLen: size_t;
    WS: WideString;
    Dest: RawByteString;
  begin
    try
      Src := lua_tolstring(L, -3, @SrcLen);
      if SrcLen = 0 then
      begin
        lua_pushstring(L, '');
        Result := 1;
        Exit;
      end;

      if lua_isnumber(L, -2) then
        SrcCP := lua_tointeger(L, -2)
      else if lua_isstring(L, -2) then
        case UTF8String(ShiftJISString(lua_tostring(L, -2))) of
          'sjis': SrcCP := 932;
          'eucjp': SrcCP := 20932;
          'iso2022jp': SrcCP := 50222;
          'utf8': SrcCP := CP_UTF8;
          else
            raise Exception.Create('unexcepted encoding name: ' +
              UTF8String(ShiftJISString(lua_tostring(L, -2))));
        end
      else
        raise Exception.Create('invalid parameter type: to');

      if lua_isnumber(L, -1) then
        DestCP := lua_tointeger(L, -1)
      else if lua_isstring(L, -1) then
        case UTF8String(ShiftJISString(lua_tostring(L, -1))) of
          'sjis': DestCP := 932;
          'eucjp': DestCP := 20932;
          'iso2022jp': DestCP := 50222;
          'utf8': DestCP := CP_UTF8;
          else
            raise Exception.Create('unexcepted encoding name: ' +
              UTF8String(ShiftJISString(lua_tostring(L, -1))));
        end
      else
        raise Exception.Create('invalid parameter type: to');

      WideLen := MultiByteToWideChar(SrcCP, 0, Src, SrcLen, nil, 0);
      if WideLen = 0 then
        RaiseLastOSError();

      SetLength(WS, WideLen);
      if MultiByteToWideChar(SrcCP, 0, Src, SrcLen, @WS[1], WideLen) = 0 then
        RaiseLastOSError();

      DestLen := WideCharToMultiByte(DestCP, 0, @WS[1], WideLen, nil, 0, nil, nil);
      if DestLen = 0 then
        RaiseLastOSError();

      SetLength(Dest, DestLen);
      if WideCharToMultiByte(DestCP, 0, @WS[1], WideLen, @Dest[1],
        DestLen, nil, nil) = 0 then
        RaiseLastOSError();

      Result := 1;
      lua_pushlstring(L, @Dest[1], DestLen);
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaPrompt(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SJIS: ShiftJISString;
    Caption, Value: UTF8String;
  begin
    try
      SJIS := lua_tostring(L, -2);
      Caption := SJIS;
      SJIS := lua_tostring(L, -1);
      Value := SJIS;
      lua_pop(L, 2);
      Result := 2;
      lua_pushboolean(L, GCMZDrops.Prompt(Caption, Value));
      SJIS := Value;
      lua_pushlstring(L, @SJIS[1], Length(SJIS));
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaConfirm(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SJIS: ShiftJISString;
    Caption: UTF8String;
  begin
    try
      SJIS := lua_tostring(L, -1);
      Caption := SJIS;
      lua_pop(L, 1);
      lua_pushboolean(L, GCMZDrops.Confirm(Caption));
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaIniString(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SS: TStringStream;
  begin
    try
      SS := TStringStream.Create(lua_tostring(L, -1));
      try
        Result := LuaIni.LuaIniInit(L, SS);
      finally
        SS.Free;
      end;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaIniFile(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SJIS: ShiftJISString;
    WFS: TWideFileStream;
  begin
    try
      SJIS := lua_tostring(L, -1);
      WFS := TWideFileStream.Create(WideString(SJIS), fmOpenRead);
      try
        Result := LuaIniInit(L, WFS);
      finally
        WFS.Free;
      end;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaDrop(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    H: THandle;
    LayerHeight, N, I: integer;
    pt: TPoint;
    SJIS: ShiftJISString;
    FilePath, NewFilePath: UTF8String;
    WFS, NWFS: TWideFileStream;
  begin
    try
      H := GCMZDrops.ExEditWindow;
      if H = 0 then
        raise Exception.Create('ExEdit window not found');
      LayerHeight := GCMZDrops.ExEditLayerHeight;

      // drop point
      lua_getfield(L, -1, 'x');
      pt.x := lua_tointeger(L, -1);
      lua_getfield(L, -2, 'y');
      pt.y := lua_tointeger(L, -1);
      lua_pop(L, 2);

      N := lua_objlen(L, -2);
      for I := 1 to N do
      begin
        lua_rawgeti(L, -2, I);

        lua_getfield(L, -1, 'filepath');
        SJIS := lua_tostring(L, -1);
        FilePath := SJIS;
        lua_pop(L, 1);
        if (not GCMZDrops.NeedCopy(FilePath)) or GCMZDrops.ExistsInGCMZDir(FilePath) then
          EmulateDropOne(H, pt, FilePath)
        else
        begin
          NewFilePath := IncludeTrailingPathDelimiter(GCMZDrops.GetSavePath()) +
            ExtractFileName(FilePath);
          if FileExists(WideString(NewFilePath)) then
          begin
            if CalcFileHash(WideString(NewFilePath)) =
              CalcFileHash(WideString(FilePath)) then
              EmulateDropOne(H, pt, NewFilePath)
            else
              raise Exception.Create('a same name different file already exists');
          end
          else
          begin
            NWFS := TWideFileStream.Create(WideString(NewFilePath), fmCreate);
            try
              try
                WFS := TWideFileStream.Create(WideString(FilePath), fmOpenRead);
                try
                  NWFS.CopyFrom(WFS, 0);
                finally
                  WFS.Free;
                end;
              finally
                NWFS.Free;
              end;
              EmulateDropOne(H, pt, NewFilePath);
            except
              AddUsedFile(NewFilePath);
              raise;
            end;
          end;
        end;

        lua_pop(L, 1);
        Inc(pt.y, LayerHeight);
      end;

      // increment current frame
      lua_getfield(L, -1, 'frameadvance');
      I := lua_tointeger(L, -1);
      lua_pop(L, 1);
      if I <> 0 then
        GCMZDrops.AdvanceFrame(I);

      Result := 1;
      lua_pushboolean(L, True);
    except
      on E: EAbort do
      begin
        Result := 1;
        lua_pushboolean(L, False);
      end;
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaGetClipboard(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    Files: TFiles;
  begin
    try
      Files := GCMZDrops.GetClipboard();
      LuaPushFiles(L, Files);
      Result := 1;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaDeleteOnFinish(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SJIS: ShiftJISString;
  begin
    try
      SJIS := lua_tostring(L, -1);
      GCMZDrops.RegisterDeleteOnFinish(UTF8String(SJIS));
      Result := 0;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaDeleteOnAbort(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    SJIS: ShiftJISString;
  begin
    try
      SJIS := lua_tostring(L, -1);
      GCMZDrops.RegisterDeleteOnAbort(UTF8String(SJIS));
      Result := 0;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaDoEvents(L: Plua_State): integer; cdecl;

  function Main(): integer;
  var
    msgMin, msgMax: UINT;
    M: TMsg;
  begin
    try
      msgMin := lua_tointeger(L, -2);
      msgMax := lua_tointeger(L, -1);
      while PeekMessageW(M, 0, msgMin, msgMax, PM_REMOVE) do begin
        TranslateMessage(M);
        DispatchMessageW(M);
      end;
      Result := 0;
    except
      on E: Exception do
        Result := LuaPushError(L, E);
    end;
  end;

begin
  Result := LuaReturn(L, Main());
end;

function LuaSetFuncs(L: Plua_State): integer; cdecl;
begin
  LuaIniRegisterMetaTable(L);
  lua_pushcfunction(L, @LuaDebugPrint);
  lua_setglobal(L, 'debug_print');

  lua_newtable(L);
  lua_pushcfunction(L, @LuaScriptDir);
  lua_setfield(L, -2, 'scriptdir');
  lua_pushcfunction(L, @LuaCreateFile);
  lua_setfield(L, -2, 'createfile');
  lua_pushcfunction(L, @LuaCreateTempFile);
  lua_setfield(L, -2, 'createtempfile');
  lua_pushcfunction(L, @LuaFindAllFile);
  lua_setfield(L, -2, 'findallfile');
  lua_pushcfunction(L, @LuaNeedCopy);
  lua_setfield(L, -2, 'needcopy');
  lua_pushcfunction(L, @LuaCalcHash);
  lua_setfield(L, -2, 'calchash');
  lua_pushcfunction(L, @LuaCalcFileHash);
  lua_setfield(L, -2, 'calcfilehash');
  lua_pushcfunction(L, @LuaHashToString);
  lua_setfield(L, -2, 'hashtostring');
  lua_pushcfunction(L, @LuaGetExEditFileInfo);
  lua_setfield(L, -2, 'getexeditfileinfo');
  lua_pushcfunction(L, @LuaGetFileInfo);
  lua_setfield(L, -2, 'getfileinfo');
  lua_pushcfunction(L, @LuaEncodeExoText);
  lua_setfield(L, -2, 'encodeexotext');
  lua_pushcfunction(L, @LuaEncodeExoTextUTF8);
  lua_setfield(L, -2, 'encodeexotextutf8');
  lua_pushcfunction(L, @LuaDecodeExoTextUTF8);
  lua_setfield(L, -2, 'decodeexotextutf8');
  lua_pushcfunction(L, @LuaEncodeLuaString);
  lua_setfield(L, -2, 'encodeluastring');
  lua_pushcfunction(L, @LuaDetectEncoding);
  lua_setfield(L, -2, 'detectencoding');
  lua_pushcfunction(L, @LuaConvertEncoding);
  lua_setfield(L, -2, 'convertencoding');
  lua_pushcfunction(L, @LuaPrompt);
  lua_setfield(L, -2, 'prompt');
  lua_pushcfunction(L, @LuaConfirm);
  lua_setfield(L, -2, 'confirm');
  lua_pushcfunction(L, @LuaIniString);
  lua_setfield(L, -2, 'inistring');
  lua_pushcfunction(L, @LuaIniFile);
  lua_setfield(L, -2, 'inifile');
  lua_pushcfunction(L, @LuaDrop);
  lua_setfield(L, -2, 'drop');
  lua_pushcfunction(L, @LuaGetClipboard);
  lua_setfield(L, -2, 'getclipboard');
  lua_pushcfunction(L, @LuaDeleteOnFinish);
  lua_setfield(L, -2, 'deleteonfinish');
  lua_pushcfunction(L, @LuaDeleteOnAbort);
  lua_setfield(L, -2, 'deleteonabort');
  lua_pushcfunction(L, @LuaDoEvents);
  lua_setfield(L, -2, 'doevents');
  lua_setglobal(L, 'GCMZDrops');

  Result := 0;
end;

end.
