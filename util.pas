unit Util;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Classes;

type
  THandleDynArray = array of THandle;

function DisableFamilyWindows(const Exclude: THandle): THandleDynArray;
procedure EnableFamilyWindows(const Handles: THandleDynArray);
function ReadAsTextFile(const Stream: TStream): UTF8String;
procedure EmulateDropOne(const Window: THandle; const Point: TPoint;
  const FileName: UTF8String);
procedure EmulateDrop(const Window: THandle; const Point: TPoint;
  const FileNames: array of UTF8String);
function Base32Encode(Source: string): string;
function ToUTF8(S: RawByteString): UTF8String;
function GenerateFilename(Stream: TStream; Extension: UTF8String): UTF8String;
function CleanFileName(const FileName: UTF8String): UTF8String;
function SelectDir(ParentWindow: THandle; var Directory: UTF8String): boolean;
function Contains(BasePath, Path: UTF8String): boolean;
function GetKnownFolderPath(const CSIDL: longint): UTF8String;
function CreateTempFile(const Extension: UTF8String): UTF8String;

implementation

uses
  Windows, ActiveX, SysUtils, ComObj, ShlObj, crc;

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

function ReadAsTextFile(const Stream: TStream): UTF8String;
var
  S: RawByteString;
begin
  SetLength(S, Stream.Size);
  Stream.ReadBuffer(S[1], Length(S));
  Result := ToUTF8(S);
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
function ToUTF8(S: RawByteString): UTF8String;
var
  Bin, UTF8, EUCJP, ShiftJIS, JIS: integer;
  I, Len: integer;
begin
  Len := Min(512, Length(S));
  if Len = 0 then
    Exit;
  // UTF-8 BOM
  if (Len >= 3) and (S[1] = #$ef) and (S[2] = #$bb) and (S[3] = #$bf) then
  begin
    SetCodePage(S, 65001, False);
    Result := UTF8String(Copy(S, 4, Length(S) - 3));
    Exit;
  end;
  // %PDF-.
  if (Len >= 5) and (S[1] = #$25) and (S[2] = #$50) and (S[3] = #$44) and
    (S[4] = #$46) and (S[5] = #$2d) then
  begin
    Result := UTF8String(S); // binary
    Exit;
  end;

  Bin := 0;
  UTF8 := 0;
  EUCJP := 0;
  ShiftJIS := 0;
  JIS := 0;

  I := 1;
  while I <= Len do
  begin
    if S[I] = #0 then
    begin
      Result := UTF8String(S); // binary
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
      if (I >= 32) and ((Bin * 100) / Len > 10) then
      begin
        Result := UTF8String(S); // binary
        Exit;
      end;
    end;
    Inc(I);
  end;

  if (Bin * 100) / Len > 10 then
  begin
    Result := UTF8String(S); // binary
    Exit;
  end;

  if (UTF8 = 0) and (EUCJP = 0) and (ShiftJIS = 0) and (JIS = 0) then
  begin
    SetCodePage(S, 65001, False);
    Result := UTF8String(S); // ascii
  end
  else if (UTF8 >= EUCJP) and (UTF8 >= ShiftJIS) and (UTF8 >= JIS) then
  begin
    SetCodePage(S, 65001, False);
    Result := UTF8String(S); // utf-8
  end
  else if (EUCJP >= UTF8) and (EUCJP >= ShiftJIS) and (EUCJP >= JIS) then
  begin
    SetCodePage(S, 20932, False);
    Result := UTF8String(S); // euc-jp
  end
  else if (ShiftJIS >= UTF8) and (ShiftJIS >= EUCJP) and (ShiftJIS >= JIS) then
  begin
    SetCodePage(S, 932, False);
    Result := UTF8String(S); // shift_jis
  end
  else if (JIS >= UTF8) and (JIS >= EUCJP) and (JIS >= ShiftJIS) then
  begin
    raise Exception.Create('cannot convert string to UTF-8 from ISO-2022-JP');
    SetCodePage(S, 50221, False);
    Result := UTF8String(S); // FIXME: Why it is not working?
  end
  else
  begin
    SetCodePage(S, 65001, False);
    Result := UTF8String(S); // ascii
  end;
end;

function GenerateFilename(Stream: TStream; Extension: UTF8String): UTF8String;
var
  Q: QWORD;
  Buffer: array[0..4095] of byte;
  Pos: int64;
  Bytes: integer;
begin
  Q := crc64(0, nil, 0);
  Pos := Stream.Position;
  repeat
    Bytes := Stream.Read(Buffer[0], Length(Buffer));
    if Bytes > 0 then
      Q := crc64(Q, @Buffer[0], Bytes);
  until Bytes <> 4096;
  Stream.Position := Pos;
  SetLength(Result, 8);
  Result[1] := char(Q shr 56);
  Result[2] := char(Q shr 48);
  Result[3] := char(Q shr 40);
  Result[4] := char(Q shr 32);
  Result[5] := char(Q shr 24);
  Result[6] := char(Q shr 16);
  Result[7] := char(Q shr 8);
  Result[8] := char(Q);
  Result := Base32Encode(Result);
  Result := StringReplace(Result, '=', '', [rfReplaceAll]);
  Result := Result + Extension;
end;

function CleanFileName(const FileName: UTF8String): UTF8String;
const
  Msk = $01;
  Table: array[byte] of byte = (
    Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk,
    Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk, Msk,
    $20, $21, Msk, $23, $24, $25, $26, $27, $28, $29, Msk, $2b, $2c, $2d, $2e, Msk,
    $30, $31, $32, $33, $34, $35, $36, $37, $38, $39, Msk, $3b, Msk, $3d, Msk, Msk,
    $40, $41, $42, $43, $44, $45, $46, $47, $48, $49, $4a, $4b, $4c, $4d, $4e, $4f,
    $50, $51, $52, $53, $54, $55, $56, $57, $58, $59, $5a, $5b, Msk, $5d, $5e, $5f,
    $60, $61, $62, $63, $64, $65, $66, $67, $68, $69, $6a, $6b, $6c, $6d, $6e, $6f,
    $70, $71, $72, $73, $74, $75, $76, $77, $78, $79, $7a, $7b, Msk, $7d, $7e, Msk,
    $80, $81, $82, $83, $84, $85, $86, $87, $88, $89, $8a, $8b, $8c, $8d, $8e, $8f,
    $90, $91, $92, $93, $94, $95, $96, $97, $98, $99, $9a, $9b, $9c, $9d, $9e, $9f,
    $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $a8, $a9, $aa, $ab, $ac, $ad, $ae, $af,
    $b0, $b1, $b2, $b3, $b4, $b5, $b6, $b7, $b8, $b9, $ba, $bb, $bc, $bd, $be, $bf,
    $c0, $c1, $c2, $c3, $c4, $c5, $c6, $c7, $c8, $c9, $ca, $cb, $cc, $cd, $ce, $cf,
    $d0, $d1, $d2, $d3, $d4, $d5, $d6, $d7, $d8, $d9, $da, $db, $dc, $dd, $de, $df,
    $e0, $e1, $e2, $e3, $e4, $e5, $e6, $e7, $e8, $e9, $ea, $eb, $ec, $ed, $ee, $ef,
    $f0, $f1, $f2, $f3, $f4, $f5, $f6, $f7, $f8, $f9, $fa, $fb, $fc, $fd, $fe, $ff
    );
var
  P, EndPos: PByte;
begin
  Result := FileName;
  P := @Result[1];
  EndPos := P;
  Inc(EndPos, Length(Result));
  while P < EndPos do
  begin
    P^ := Table[P^];
    Inc(P);
  end;
  Result := StringReplace(Result, #1, '', [rfReplaceAll]);
end;

function SelectDirCallback(Window: THandle; Message: UINT; LParam: LPARAM;
  Data: LPARAM): LRESULT; stdcall;
begin
  if Message = BFFM_INITIALIZED then
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
    OleCheck(ShellFolder.ParseDisplayName(0, nil, PWideChar(WideString(Directory)),
      eaten, InitialPIDL, eaten));
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

function CreateTempFile(const Extension: UTF8String): UTF8String;
var
  FileHandle: THandle;
  BasePath, FileName: WideString;
  I: integer;
begin
  SetLength(BasePath, MAX_PATH);
  GetTempPathW(MAX_PATH, @BasePath[1]);
  BasePath := PWideChar(BasePath);
  for I := 0 to 10 do
  begin
    FileName := BasePath + WideString('tmp' + IntToStr(Random($ffffff)) + Extension);
    FileHandle := CreateFileW(PWideChar(FileName), GENERIC_WRITE, 0,
      nil, CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or FILE_ATTRIBUTE_HIDDEN or
      FILE_ATTRIBUTE_NOT_CONTENT_INDEXED, 0);
    if FileHandle = INVALID_HANDLE_VALUE then
      continue;
    try
      Result := UTF8String(FileName);
      Exit;
    finally
      CloseHandle(FileHandle);
    end;
  end;
  raise Exception.Create('cannot create a temporary file');
end;

function GetKnownFolderPath(const CSIDL: longint): UTF8String;
var
  WS: WideString;
begin
  SetLength(WS, MAX_PATH);
  OleCheck(SHGetFolderPathW(0, CSIDL, 0, SHGFP_TYPE_CURRENT, @WS[1]));
  Result := UTF8String(WideString(PWideChar(WS)));
end;

end.
