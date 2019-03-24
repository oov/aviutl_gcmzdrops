unit Util;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes;

type
  ShiftJISString = type ansistring(932);

  THandleDynArray = array of THandle;

  { TWideFileStream }

  TWideFileStream = class(THandleStream)
  private
    FFilePath: WideString;
  public
    constructor Create(const FilePath: WideString; Mode: word);
    constructor CreateUnique(const FilePath: WideString; const Ext: WideString);
    destructor Destroy(); override;
    property FilePath: WideString read FFilePath;
  end;

procedure ODS(const Fmt: string; const Args: array of const);
function IsDesktopCompositionEnabled(): boolean;
function Token(const Delimiter: WideString; var S: WideString): WideString;

function DisableFamilyWindows(const Exclude: THandle): THandleDynArray;
procedure EnableFamilyWindows(const Handles: THandleDynArray);
procedure EmulateDropOne(const Window: THandle; const Point: TPoint;
  const FileName: UTF8String);
procedure EmulateDrop(const Window: THandle; const Point: TPoint;
  const FileNames: array of UTF8String);
function Base32Encode(Source: string): string;
function DetectEncoding(S: PChar; Len: cardinal): TSystemCodePage;
function Sanitize(S: UTF8String; ReplaceChar: char): UTF8String;
function SelectDir(ParentWindow: THandle; var Directory: UTF8String): boolean;
function Contains(BasePath, Path: UTF8String): boolean;
function GetKnownFolderPath(const CSIDL: longint): UTF8String;

function GetDLLName(): WideString;
function GetOtherDLLName(const H: THandle): WideString;
function CalcFileHash(FilePath: WideString): QWORD;

implementation

uses
  Windows, ActiveX, SysUtils, ComObj, ShlObj, crc;

procedure ODS(const Fmt: string; const Args: array of const);
begin
  OutputDebugStringW(PWideChar(WideString(Format('GCMZDrops: ' + Fmt, Args))));
end;

function Token(const Delimiter: WideString; var S: WideString): WideString;
var
  P: integer;
begin
  P := Pos(Delimiter, S);
  if P = 0 then
  begin
    Result := S;
    S := '';
  end
  else
  begin
    Result := Copy(S, 1, P - 1);
    Delete(S, 1, P + Length(Delimiter) - 1);
  end;
end;

function IsDesktopCompositionEnabled(): boolean;
type
  DwmIsCompositionEnabledFunc = function(out enabled: BOOL): HRESULT; stdcall;
var
  H: THandle;
  F: DwmIsCompositionEnabledFunc;
  B: BOOL;
begin
  Result := False;
  H := LoadLibrary('Dwmapi.dll');
  if H = 0 then
    Exit;
  try
    Pointer(F) := GetProcAddress(H, 'DwmIsCompositionEnabled');
    if F = nil then
      Exit;
    if F(B) <> S_OK then
      Exit;
  finally
    FreeLibrary(H);
  end;
  Result := B;
end;

type
  TDisableFamilyWindowsData = record
    PID: DWORD;
    Exclude: THandle;
    Handles: THandleDynArray;
  end;
  PDisableFamilyWindowsData = ^TDisableFamilyWindowsData;

function DisableFamilyWindowsCallback(Window: HWND; LParam: LPARAM): WINBOOL; stdcall;
var
  P: PDisableFamilyWindowsData absolute LParam;
  PID: DWORD;
begin
  if IsWindowVisible(Window) and IsWindowEnabled(Window) and
    (P^.Exclude <> Window) then
  begin
    GetWindowThreadProcessId(Window, PID);
    if PID = P^.PID then
    begin
      SetLength(P^.Handles, Length(P^.Handles) + 1);
      P^.Handles[Length(P^.Handles) - 1] := Window;
      EnableWindow(Window, False);
    end;
  end;
  Result := True;
end;

function DisableFamilyWindows(const Exclude: THandle): THandleDynArray;
var
  Data: TDisableFamilyWindowsData;
begin
  Data.PID := GetCurrentProcessId();
  Data.Exclude := Exclude;
  EnumWindows(@DisableFamilyWindowsCallback, LPARAM(@Data));
  Result := Data.Handles;
end;

procedure EnableFamilyWindows(const Handles: THandleDynArray);
var
  I: integer;
begin
  for I := Low(Handles) to High(Handles) do
    EnableWindow(Handles[I], True);
end;

procedure EmulateDropOne(const Window: THandle; const Point: TPoint;
  const FileName: UTF8String);
var
  FileNames: array of UTF8String;
begin
  SetLength(FileNames, 1);
  FileNames[0] := FileName;
  EmulateDrop(Window, Point, FileNames);
end;

procedure EmulateDrop(const Window: THandle; const Point: TPoint;
  const FileNames: array of UTF8String);
var
  I, Len: integer;
  HGlobal: THandle;
  PDF: PDropFiles;
  P: PByte;
  WS: array of WideString;
begin
  SetLength(WS, Length(FileNames));
  Len := 2;
  for I := Low(FileNames) to High(FileNames) do
  begin
    WS[I] := WideString(FileNames[I]);
    Inc(Len, (Length(WS[I]) + 1) * 2);
  end;
  HGlobal := GlobalAlloc(GMEM_ZEROINIT, SizeOf(TDropFiles) + Len);
  PDF := GlobalLock(HGlobal);
  try
    PDF^.pFiles := SizeOf(TDropFiles);
    PDF^.fWide := True;
    PDF^.fNC := False;
    PDF^.pt.x := Point.x;
    PDF^.pt.y := Point.y;
    ScreenToClient(Window, PDF^.pt);
    P := PByte(PDF);
    Inc(P, SizeOf(TDropFiles));
    for I := Low(WS) to High(WS) do
    begin
      Move(WS[I][1], P^, Length(WS[I]) * SizeOf(widechar));
      Inc(P, (Length(WS[I]) + 1) * SizeOf(widechar));
    end;
  finally
    GlobalUnlock(HGlobal);
  end;
  SendMessageW(Window, WM_DROPFILES, WPARAM(HGlobal), 0);
end;

// Based on http://www.experts-exchange.com/questions/20913763/Base32-Encoding-Decoding-in-Delphi.html#a10588822
function Base32Encode(Source: string): string;
const
  Table = 'abcdefghijklmnopqrstuvwxyz234567';
var
  I: integer;
  nr: int64;
begin
  Result := '';
  while length(Source) >= 5 do
  begin
    nr := 0;
    for I := 1 to 5 do
      nr := nr * 256 + Ord(Source[I]);
    for I := 1 to 8 do
    begin
      Result := Result + Table[(nr mod 32) + 1];
      nr := nr div 32;
    end;
    Delete(Source, 1, 5);
  end;
  nr := 0;
  if length(Source) > 0 then
  begin
    for I := 1 to length(Source) do
      nr := nr * 256 + Ord(Source[I]);
    for I := 1 to 8 do
    begin
      if nr > 0 then
      begin
        Result := Result + Table[(nr mod 32) + 1];
        nr := nr div 32;
      end
      else
        Result := Result + '=';
    end;
  end;
end;

// This code is ported from the_platinum_searcher.
// The MIT License (MIT): Copyright (c) [2014] [the_platinum_searcher]
// https://github.com/monochromegane/the_platinum_searcher
function DetectEncoding(S: PChar; Len: cardinal): TSystemCodePage;
var
  Bin, UTF8, EUCJP, ShiftJIS, JIS: integer;
  I: cardinal;
begin
  if Len = 0 then
    Exit;
  // UTF-16 BOM
  if (Len >= 2) and (S[0] = #$ff) and (S[1] = #$fe) then
  begin
    Result := CP_UTF16;
    Exit;
  end;
  // UTF-16BE BOM
  if (Len >= 2) and (S[0] = #$fe) and (S[1] = #$ff) then
  begin
    Result := CP_UTF16BE;
    Exit;
  end;
  // UTF-8 BOM
  if (Len >= 3) and (S[0] = #$ef) and (S[1] = #$bb) and (S[2] = #$bf) then
  begin
    Result := CP_UTF8;
    Exit;
  end;
  // %PDF-.
  if (Len >= 5) and (S[1] = #$25) and (S[2] = #$50) and (S[3] = #$44) and
    (S[4] = #$46) and (S[5] = #$2d) then
  begin
    Result := 0; // binary
    Exit;
  end;

  Bin := 0;
  UTF8 := 0;
  EUCJP := 0;
  ShiftJIS := 0;
  JIS := 0;

  I := 1;
  while I < Len do
  begin
    if S[I] = #0 then
    begin
      Result := 0; // binary
      Exit;
    end;

    if ((S[I] < #7) or (S[I] > #14)) and ((S[I] < #32) or (S[I] > #127)) then
    begin
      // UTF-8 detection
      if (S[I] > #193) and (S[I] < #224) and (I + 1 < Len) then
      begin
        Inc(I);
        if (S[I] > #127) and (S[I] < #192) then
        begin
          Inc(UTF8);
          Inc(I);
          continue;
        end;
      end
      else if (S[I] > #223) and (S[I] < #240) and (I + 2 < Len) then
      begin
        Inc(I);
        if (S[I] > #127) and (S[I] < #192) and (S[I + 1] > #127) and
          (S[I + 1] < #192) then
        begin
          Inc(UTF8);
          Inc(I, 2);
          continue;
        end;
      end;

      // EUC-JP detection
      if (S[I] = #142) and (I + 1 < Len) then
      begin
        Inc(I);
        if (S[I] > #160) and (S[I] < #224) then
        begin
          Inc(EUCJP);
          Inc(I);
          continue;
        end;
      end
      else if (S[I] > #160) and (S[I] < #255) and (I + 1 < Len) then
      begin
        Inc(I);
        if (S[I] > #160) and (S[I] < #255) then
        begin
          Inc(EUCJP);
          Inc(I);
          continue;
        end;
      end;

      // Shift-JIS detection
      if (S[I] > #160) and (S[I] < #224) then
      begin
        Inc(ShiftJIS);
        Inc(I);
        continue;
      end
      else if (((S[I] > #128) and (S[I] < #160)) or
        ((S[I] > #223) and (S[I] < #240))) and (I + 1 < Len) then
      begin
        Inc(I);
        if ((S[I] > #63) and (S[I] < #127)) or ((S[I] > #127) and (S[I] < #253)) then
        begin
          Inc(ShiftJIS);
          Inc(I);
          continue;
        end;
      end;

      // ISO-2022-JP detection
      if (S[I] = #27) and (I + 2 < Len) then
      begin
        Inc(I);
        case S[I] of
          #36:
          begin
            Inc(I);
            if (S[I] = #64) or (S[I] = #66) or (S[I] = #68) then
            begin
              Inc(JIS);
              Inc(I);
              continue;
            end;
          end;
          #40:
          begin
            Inc(I);
            if (S[I] = #66) or (S[I] = #73) or (S[I] = #74) then
            begin
              Inc(JIS);
              Inc(I);
              continue;
            end;
          end;
        end;
      end;

      Inc(Bin);
      if Bin * 2 > Len then
      begin
        Result := 0; // binary
        Exit;
      end;
    end;
    Inc(I);
  end;

  if Bin * 2 > Len then
  begin
    Result := 0; // binary
    Exit;
  end;

  if (UTF8 = 0) and (EUCJP = 0) and (ShiftJIS = 0) and (JIS = 0) then
  begin
    Result := CP_UTF8; // ascii
  end
  else if (UTF8 >= EUCJP) and (UTF8 >= ShiftJIS) and (UTF8 >= JIS) then
  begin
    Result := CP_UTF8; // utf-8
  end
  else if (EUCJP >= UTF8) and (EUCJP >= ShiftJIS) and (EUCJP >= JIS) then
  begin
    Result := 20932; // euc-jp
  end
  else if (ShiftJIS >= UTF8) and (ShiftJIS >= EUCJP) and (ShiftJIS >= JIS) then
  begin
    Result := 932; // shift_jis
  end
  else if (JIS >= UTF8) and (JIS >= EUCJP) and (JIS >= ShiftJIS) then
  begin
    Result := 50222;
  end
  else
  begin
    Result := CP_UTF8; // ascii
  end;
end;

function Sanitize(S: UTF8String; ReplaceChar: char): UTF8String;
var
  si, di: integer;
  c: char;
begin
  SetLength(Result, Length(s));
  di := 1;
  for si := 1 to Length(s) do
  begin
    case s[si] of
      #$00, #$01, #$02, #$03, #$04, #$05, #$06, #$07,
      #$08, #$09, #$0a, #$0b, #$0c, #$0d, #$0e, #$0f,
      #$10, #$11, #$12, #$13, #$14, #$15, #$16, #$17,
      #$18, #$19, #$1a, #$1b, #$1c, #$1d, #$1e, #$1f,
      #$22, #$2a, #$2f, #$3a, #$3c, #$3e, #$3f, #$7c, #$7f: c := ReplaceChar;
      else
        c := s[si];
    end;
    Result[di] := c;
    Inc(di, 1);
  end;
  SetLength(Result, di - 1);
end;

function SelectDirCallback(Window: THandle; Message: UINT; LParam: LPARAM;
  Data: LPARAM): LRESULT; stdcall;
begin
  if (Message = BFFM_INITIALIZED) and (Data <> 0) then
  begin
    SendMessageW(Window, BFFM_SETSELECTION, 0, Data);
  end;
  Result := 0;
end;

function SelectDir(ParentWindow: THandle; var Directory: UTF8String): boolean;
var
  BI: TBrowseInfoW;
  Malloc: IMalloc;
  PIDL, InitialPIDL: PItemIDList;
  WS: WideString;
  ShellFolder: IShellFolder;
  eaten: ULong;
begin
  ShGetMalloc(Malloc);
  FillChar(BI, SizeOf(TBrowseInfoW), #0);
  BI.hwndOwner := ParentWindow;
  BI.lpfn := @SelectDirCallback;
  BI.lpszTitle := PWideChar(WideString('データ保存先の選択'));
  BI.pidlRoot := nil;
  BI.ulFlags := BIF_RETURNONLYFSDIRS or BIF_NEWDIALOGSTYLE or
    BIF_VALIDATE or BIF_EDITBOX;
  OleCheck(SHGetDesktopFolder(ShellFolder));
  try
    if Failed(ShellFolder.ParseDisplayName(0, nil, PWideChar(WideString(Directory)),
      eaten, InitialPIDL, eaten)) then
      InitialPIDL := nil;
  finally
    ShellFolder := nil;
  end;
  try
    BI.lParam := LPARAM(InitialPIDL);
    PIDL := SHBrowseForFolderW(BI);
  finally
    Malloc.Free(InitialPIDL);
  end;
  if PIDL = nil then
  begin
    Result := False;
    Exit;
  end;
  try
    SetLength(WS, MAX_PATH);
    SHGetPathFromIDListW(PIDL, @WS[1]);
    Directory := UTF8String(WideString(PWideChar(WS)));
    Result := True;
  finally
    Malloc.Free(PIDL);
  end;
end;

function GetLongPathNameW(ShortPathName: PWideChar; LongPathName: PWideChar;
  cchBuffer: integer): integer; stdcall; external kernel32 Name 'GetLongPathNameW';
function PathRelativePathToW(pszPath: PWideChar; pszFrom: PWideChar;
  dwAttrFrom: DWORD; pszTo: PWideChar; dwAtrTo: DWORD): longbool;
  stdcall; external 'shlwapi.dll' Name 'PathRelativePathToW';

function Contains(BasePath, Path: UTF8String): boolean;
var
  WS: WideString;
  S: UTF8String;
begin
  SetLength(WS, MAX_PATH);
  if GetLongPathNameW(PWideChar(WideString(Path)), @WS[1], MAX_PATH) = 0 then
    RaiseLastOSError();
  Path := UTF8String(WideString(PWideChar(WS)));
  Result := False;
  FillChar(WS[1], MAX_PATH * SizeOf(widechar), 0);
  if not PathRelativePathToW(@WS[1], PWideChar(WideString(BasePath)),
    FILE_ATTRIBUTE_DIRECTORY, PWideChar(WideString(Path)), 0) then
    Exit;
  S := UTF8String(WideString(PWideChar(WS)));
  if Path = S then
    Exit;
  if Copy(S, 1, 3) = '..' + DirectorySeparator then
    Exit;
  Result := True;
end;

function GetKnownFolderPath(const CSIDL: longint): UTF8String;
var
  WS: WideString;
begin
  SetLength(WS, MAX_PATH);
  OleCheck(SHGetFolderPathW(0, CSIDL, 0, SHGFP_TYPE_CURRENT, @WS[1]));
  Result := UTF8String(WideString(PWideChar(WS)));
end;

function GetDLLName(): WideString;
begin
  Result := GetOtherDLLName(hInstance);
end;

function GetOtherDLLName(const H: THandle): WideString;
begin
  SetLength(Result, MAX_PATH);
  GetModuleFileNameW(H, @Result[1], MAX_PATH);
  Result := PWideChar(Result);
end;

function CalcFileHash(FilePath: WideString): QWORD;
var
  WFS: TWideFileStream;
  Buffer: array[0..4095] of byte;
  Bytes: integer;
begin
  WFS := TWideFileStream.Create(FilePath, fmOpenRead);
  try
    Result := crc64(0, nil, 0);
    repeat
      Bytes := WFS.Read(Buffer[0], Length(Buffer));
      if Bytes > 0 then
        Result := crc64(Result, @Buffer[0], Bytes);
    until Bytes <> Length(Buffer);
  finally
    WFS.Free;
  end;
end;

{ TWideFileStream }

constructor TWideFileStream.Create(const FilePath: WideString; Mode: word);
var
  H: THandle;
begin
  if (Mode and fmCreate) = fmCreate then
    H := CreateFileW(PWideChar(FilePath), GENERIC_WRITE, FILE_SHARE_READ,
      nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0)
  else
    H := CreateFileW(PWideChar(FilePath), GENERIC_READ, FILE_SHARE_READ,
      nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL or FILE_FLAG_SEQUENTIAL_SCAN, 0);
  if H = INVALID_HANDLE_VALUE then
    raise Exception.Create('cannot open file');
  inherited Create(H);
  FFilePath := FilePath;
end;

constructor TWideFileStream.CreateUnique(const FilePath: WideString;
  const Ext: WideString);
var
  H: THandle;
  S: WideString;
  I: integer;
begin
  S := FilePath + Ext;
  for I := 0 to 10 do
  begin
    H := CreateFileW(PWideChar(S), GENERIC_WRITE, FILE_SHARE_READ,
      nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
    if H = INVALID_HANDLE_VALUE then
    begin
      S := FilePath + WideString(IntToStr(Random($ffffff))) + Ext;
      continue;
    end;
    inherited Create(H);
    FFilePath := S;
    Exit;
  end;
  raise Exception.Create('cannot create a unique file');
end;

destructor TWideFileStream.Destroy;
begin
  CloseHandle(Handle);
  inherited Destroy;
end;

end.
