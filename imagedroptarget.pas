unit ImageDropTarget;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, ActiveX, ShlObj, Classes;

type
  TSniffResult = UTF8String;
  TSniffer = function(const Stream: TStream; const FileName: UTF8String): TSniffResult of
    object;
  TDropSourceType = (dstNone, dstFileContents, dstHDROP, dstText);
  TImageDragDropEvent = procedure(Sender: TObject; const KeyState: DWORD;
    const Point: TPoint; const DST: TDropSourceType; const Stream: TStream;
    const FileName: UTF8String; const SR: TSniffResult; var Effect: DWORD) of object;
  TImageDragOverEvent = procedure(Sender: TObject; const KeyState: DWORD;
    const Point: TPoint; var Effect: DWORD) of object;

  { TImageDropTarget }

  TImageDropTarget = class(TInterfacedObject, IDropTarget)
  private
    FOnDragEnter: TImageDragDropEvent;
    FOnDragLeave: TNotifyEvent;
    FOnDragOver: TImageDragOverEvent;
    FOnDrop: TImageDragDropEvent;
    FSniffer: TSniffer;
  protected
    FEffect: DWORD;
    function DefaultSniffer(const Stream: TStream;
      const FileName: UTF8String): TSniffResult;
    function DragEnter(const DataObject: IDataObject; KeyState: DWORD;
      Point: TPoint; var Effect: DWORD): HResult; stdcall; virtual;
    function DragOver(KeyState: DWORD; Point: TPoint; var Effect: DWORD): HResult;
      stdcall; virtual;
    function DragLeave: HResult; stdcall; virtual;
    function Drop(const DataObject: IDataObject; KeyState: DWORD;
      Point: TPoint; var Effect: DWORD): HResult; stdcall; virtual;
  public
    constructor Create();
    property OnDragEnter: TImageDragDropEvent read FOnDragEnter write FOnDragEnter;
    property OnDragOver: TImageDragOverEvent read FOnDragOver write FOnDragOver;
    property OnDragLeave: TNotifyEvent read FOnDragLeave write FOnDragLeave;
    property OnDrop: TImageDragDropEvent read FOnDrop write FOnDrop;
    property Sniffer: TSniffer read FSniffer write FSniffer;
  end;

  TFileDescriptorWArray = array of TFileDescriptorW;
  TFileNames = array of UTF8String;

function GetData(out StgMedium: TStgMedium; const DataObject: IDataObject;
  const Format: word; const Index: integer = -1): HRESULT;
function ReadFromStgMedium(StgMedium: TStgMedium): TMemoryStream;
function GetFileDescriptors(const DataObject: IDataObject): TFileDescriptorWArray;
function ReadFileContents(const DataObject: IDataObject; const Index: integer): TStream;
function ReadURL(const DataObject: IDataObject): UTF8String;
function ReadTextUTF8(const DataObject: IDataObject): UTF8String;
function ReadHDROP(const DataObject: IDataObject): TFileNames;
function DataURISchemeToStream(const S: RawByteString): TStream;

implementation

uses
  SysUtils, ComObj, base64;

function ReadFromHGlobal(const HGlobal: HGLOBAL): TMemoryStream;
var
  P: Pointer;
  Bytes: DWORD;
begin
  P := GlobalLock(HGlobal);
  if P = nil then
    RaiseLastOSError();
  try
    Bytes := GlobalSize(HGlobal);
    if Bytes = 0 then
      RaiseLastOSError();
    Result := TMemoryStream.Create();
    try
      Result.Write(P^, Bytes);
    except
      Result.Free;
      raise;
    end;
  finally
    GlobalUnlock(HGlobal);
  end;
end;

function ReadFromIStream(const Stream: IStream): TMemoryStream;
var
  Buffer: array[0..4095] of byte;
  Bytes: DWORD;
  HR: HRESULT;
begin
  Result := TMemoryStream.Create();
  try
    repeat
      HR := Stream.Read(@Buffer[0], 4096, @Bytes);
      if HR = S_OK then
        Result.Write(Buffer[0], Bytes);
    until (HR <> S_OK) or (Bytes <> 4096);
    if HR <> S_FALSE then
      OleCheck(HR);
  except
    Result.Free;
    raise;
  end;
end;

function GetData(out StgMedium: TStgMedium; const DataObject: IDataObject;
  const Format: word; const Index: integer = -1): HRESULT;
var
  FormatEtc: TFormatEtc;
begin
  FormatEtc.cfFormat := Format;
  FormatEtc.ptd := nil;
  FormatEtc.dwAspect := DVASPECT_CONTENT;
  FormatEtc.lindex := Index;
  FormatEtc.tymed := TYMED_HGLOBAL;
  Result := DataObject.GetData(FormatEtc, StgMedium);
end;

function ReadFromStgMedium(StgMedium: TStgMedium): TMemoryStream;
begin
  try
    case StgMedium.Tymed of
      TYMED_HGLOBAL: Result := ReadFromHGlobal(StgMedium.hGlobal);
      TYMED_ISTREAM: Result := ReadFromIStream(IStream(StgMedium.pstm));
      else
        raise Exception.Create('unsupported tymed: ' + IntToStr(StgMedium.Tymed));
    end;
  finally
    ReleaseStgMedium(StgMedium);
  end;
end;

function GetFileDescriptors(const DataObject: IDataObject): TFileDescriptorWArray;
var
  StgMedium: TStgMedium;
  PFGDW: PFileGroupDescriptorW;
  PFGDA: PFileGroupDescriptorA;
  PFDW: PFileDescriptorW;
  PFDA: PFileDescriptorA;
  I: integer;
  WS: WideString;
  MS: TMemoryStream;
begin
  if Succeeded(GetData(StgMedium, DataObject,
    RegisterClipboardFormat('FileGroupDescriptorW'))) then
  begin
    MS := ReadFromStgMedium(StgMedium);
    try
      PFGDW := MS.Memory;
      SetLength(Result, PFGDW^.cItems);
      PFDW := @PFGDW^.fgd[0];
      for I := 0 to PFGDW^.cItems - 1 do
      begin
        Move(PFDW^, Result[I], SizeOf(TFileDescriptorW));
        Inc(PFDW);
      end;
    finally
      MS.Free;
    end;
    exit;
  end;

  if Succeeded(GetData(StgMedium, DataObject,
    RegisterClipboardFormat('FileGroupDescriptor'))) then
  begin
    MS := ReadFromStgMedium(StgMedium);
    try
      PFGDA := MS.Memory;
      SetLength(Result, PFGDA^.cItems);
      PFDA := @PFGDA^.fgd[0];
      for I := 0 to PFGDA^.cItems - 1 do
      begin
        Move(PFDA^, Result[I], SizeOf(TFileDescriptor) - MAX_PATH);
        WS := WideString(PChar(@PFDA^.cFileName[0])) + #0;
        Move(WS[1], Result[I].cFileName[0], Length(WS) * SizeOf(widechar));
        Inc(PFDA);
      end;
    finally
      MS.Free;
    end;
    exit;
  end;
end;

function ReadFileContents(const DataObject: IDataObject; const Index: integer): TStream;
var
  StgMedium: TStgMedium;
begin
  if Failed(GetData(StgMedium, DataObject, RegisterClipboardFormat('FileContents'),
    Index)) then
  begin
    Result := nil;
    exit;
  end;
  Result := ReadFromStgMedium(StgMedium);
end;

function ReadURL(const DataObject: IDataObject): UTF8String;
var
  StgMedium: TStgMedium;
  MS: TMemoryStream;
begin
  if Succeeded(GetData(StgMedium, DataObject,
    RegisterClipboardFormat('UniformResourceLocatorW'))) then
  begin
    MS := ReadFromStgMedium(StgMedium);
    try
      Result := UTF8String(PWideChar(MS.Memory));
    finally
      MS.Free;
    end;
    exit;
  end;

  if Succeeded(GetData(StgMedium, DataObject,
    RegisterClipboardFormat('UniformResourceLocator'))) then
  begin
    MS := ReadFromStgMedium(StgMedium);
    try
      Result := UTF8String(PChar(MS.Memory));
    finally
      MS.Free;
    end;
  end;
end;

function ReadTextUTF8(const DataObject: IDataObject): UTF8String;
var
  StgMedium: TStgMedium;
  MS: TMemoryStream;
begin
  if Succeeded(GetData(StgMedium, DataObject, CF_UNICODETEXT)) then
  begin
    MS := ReadFromStgMedium(StgMedium);
    try
      Result := UTF8String(PWideChar(MS.Memory));
    finally
      MS.Free;
    end;
    exit;
  end;
  if Succeeded(GetData(StgMedium, DataObject, CF_TEXT)) then
  begin
    MS := ReadFromStgMedium(StgMedium);
    try
      Result := UTF8String(PChar(MS.Memory));
    finally
      MS.Free;
    end;
    exit;
  end;
end;

function ReadHDROP(const DataObject: IDataObject): TFileNames;
var
  StgMedium: TStgMedium;
  MS: TMemoryStream;
  PDF: PDropFiles;
  PC: PChar;
  PWC: PWideChar;
  S: UTF8String;
begin
  if Failed(GetData(StgMedium, DataObject, CF_HDROP)) then
    exit;
  MS := ReadFromStgMedium(StgMedium);
  try
    PDF := MS.Memory;
    PC := PChar(PDF);
    Inc(PC, SizeOf(TDropFiles));
    if PDF^.fWide then
    begin
      PWC := PWideChar(PC);
      while PWC^ <> #0 do
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := UTF8String(PWC);
        Inc(PWC, StrLen(PWC) + 1);
      end;
      exit;
    end;

    while PC^ <> #0 do
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := UTF8String(PC);
      Inc(PC, StrLen(PC) + 1);
    end;
  finally
    MS.Free;
  end;
end;

function DecodePercentEncoding(const S: RawByteString): TStream;
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
  P, EndPos: PByte;
begin
  Result := TMemoryStream.Create();
  try
    P := @S[1];
    EndPos := P + Length(S);
    while (P < EndPos) do
    begin
      if (P^ = $35) and (P + 2 < EndPos) then
      begin
        Result.WriteByte((Table[(P + 1)^] shl 4) or Table[(P + 2)^]);
        Inc(P, 3);
      end
      else
      begin
        Result.WriteByte(P^);
        Inc(P);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function DecodeBase64(const S: RawByteString): TStream;
var
  Stream, Decoder: TStream;
begin
  Stream := TStringStream.Create(S);
  try
    Decoder := TBase64DecodingStream.Create(Stream, bdmMIME);
    try
      Result := TMemoryStream.Create();
      try
        Result.CopyFrom(Decoder, Decoder.Size);
      except
        FreeAndNil(Result);
        raise;
      end;
    finally
      Decoder.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function DataURISchemeToStream(const S: RawByteString): TStream;
var
  Left: integer;
begin
  Result := nil;
  if Copy(S, 1, 5) <> 'data:' then
    exit;
  Left := Pos(',', S) + 1;
  if Left = 1 then
    exit;
  if Pos('base64', Copy(S, 6, Left - 6)) = 0 then
    Result := DecodePercentEncoding(Copy(S, Left, Length(S) - Left))
  else
    Result := DecodeBase64(Copy(S, Left, Length(S) - Left));
end;

function ExtractFileNameFromBase64(const S: RawByteString;
  const FileNameLen: integer): UTF8String;
begin
  Result := Copy(S, Length(S) - FileNameLen, FileNameLen);
  Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '_', [rfReplaceAll]);
end;

function FindImage(const DataObject: IDataObject; Sniffer: TSniffer;
  AllowUnexistedFile: boolean; out DST: TDropSourceType; out FileName: UTF8String;
  out SR: TSniffResult): TStream;
var
  Stream: TStream;
  FDWA: TFileDescriptorWArray;
  FileNames: TFileNames;
  S: UTF8String;
begin
  FDWA := GetFileDescriptors(DataObject);
  if Length(FDWA) = 1 then
  begin
    Stream := ReadFileContents(DataObject, 0);
    if Assigned(Stream) then
    begin
      try
        Stream.Position := 0;
        S := UTF8String(PWideChar(@FDWA[0].cFileName[0]));

        SR := Sniffer(Stream, S);
        if SR <> '' then
        begin
          Stream.Position := 0;
          DST := dstFileContents;
          Result := Stream;
          FileName := S;
          Stream := nil;
          exit;
        end;
      finally
        FreeAndNil(Stream);
      end;
    end;
  end;

  FileNames := ReadHDROP(DataObject);
  if Length(FileNames) = 1 then
  begin
    S := FileNames[0];
    try
      Stream := TFileStream.Create(S, fmOpenRead or fmShareDenyNone);
    except
      if AllowUnexistedFile then
      begin
        SR := Sniffer(nil, S);
        if SR <> '' then
        begin
          Result := nil;
          DST := dstHDROP;
          FileName := S;
          exit;
        end;
      end;
    end;
    if Assigned(Stream) then
    begin
      try
        SR := Sniffer(Stream, S);
        if SR <> '' then
        begin
          Stream.Position := 0;
          Result := Stream;
          DST := dstHDROP;
          FileName := S;
          Stream := nil;
          exit;
        end;
      finally
        FreeAndNil(Stream);
      end;
    end;
  end;

  S := ReadTextUTF8(DataObject);
  Stream := DataURISchemeToStream(S);
  if Assigned(Stream) then
  begin
    try
      Stream.Position := 0;
      SR := Sniffer(Stream, '');
      if SR <> '' then
      begin
        FileName := ExtractFileNameFromBase64(S, 24) + SR;
        Stream.Position := 0;
        DST := dstText;
        Result := Stream;
        Stream := nil;
        exit;
      end;
    finally
      FreeAndNil(Stream);
    end;
  end;

  Result := nil;
  DST := dstNone;
  FileName := '';
  SR := '';
end;

function TImageDropTarget.DefaultSniffer(const Stream: TStream;
  const FileName: UTF8String): TSniffResult;
var
  PB: array[0..15] of byte;
begin
  if (Assigned(Stream)) then
  begin
    try
      Stream.ReadBuffer(PB, 16);
    except
      Result := '';
      exit;
    end;
    if (PB[0] = Ord('G')) and (PB[1] = Ord('I')) and (PB[2] = Ord('F')) and
      (PB[3] = Ord('8')) and (((PB[4] = Ord('7')) or (PB[4] = Ord('9')))) and
      (PB[5] = Ord('a')) then
      Result := '.gif'
    else if (PB[0] = $ff) and (PB[1] = $d8) and (PB[2] = $ff) then
      Result := '.jpg'
    else if (PB[0] = $89) and (PB[1] = Ord('P')) and (PB[2] = Ord('N')) and
      (PB[3] = Ord('G')) and (PB[4] = $0d) and (PB[5] = $0a) and
      (PB[6] = $1a) and (PB[7] = $0a) then
      Result := '.png'
    else if (PB[0] = Ord('R')) and (PB[1] = Ord('I')) and (PB[2] = Ord('F')) and
      (PB[3] = Ord('F')) and (PB[8] = Ord('W')) and (PB[9] = Ord('E')) and
      (PB[10] = Ord('B')) and (PB[11] = Ord('P')) then
      Result := '.webp'
    else
      Result := '';
    exit;
  end;
  if FileName <> '' then
  begin
    case LowerCase(ExtractFileExt(FileName)) of
      '.gif': Result := '.gif';
      '.png': Result := '.png';
      '.jpg', '.jpeg', '.jpe', '.jfif', '.jfi', '.jif': Result := '.jpg';
      '.webp': Result := '.webp';
      else
        Result := '';
    end;
    exit;
  end;
end;

function TImageDropTarget.DragEnter(const DataObject: IDataObject;
  KeyState: DWORD; Point: TPoint; var Effect: DWORD): HResult; stdcall;
var
  Stream: TStream;
  FileName: UTF8String;
  SR: TSniffResult;
  DST: TDropSourceType;
begin
  Result := S_OK;
  if not Assigned(FOnDrop) then
  begin
    FEffect := DROPEFFECT_NONE;
    Effect := FEffect;
    exit;
  end;

  Stream := FindImage(DataObject, FSniffer, True, DST, FileName, SR);
  try
    if SR = '' then
    begin
      FEffect := DROPEFFECT_NONE;
      Effect := FEffect;
      exit;
    end;

    FEffect := DROPEFFECT_COPY;
    if Assigned(FOnDragEnter) then
      FOnDragEnter(Self, KeyState, Point, DST, Stream, FileName, SR, FEffect);
  finally
    FreeAndNil(Stream);
  end;
  Effect := FEffect;
end;

function TImageDropTarget.DragOver(KeyState: DWORD; Point: TPoint;
  var Effect: DWORD): HResult; stdcall;
begin
  Result := S_OK;
  if FEffect = DROPEFFECT_NONE then
  begin
    Effect := FEffect;
    exit;
  end;
  if Assigned(FOnDragOver) then
    FOnDragOver(Self, KeyState, Point, FEffect);
  Effect := FEffect;
end;

function TImageDropTarget.DragLeave: HResult; stdcall;
begin
  Result := S_OK;
  if Assigned(FOnDragLeave) then
    FOnDragLeave(Self);
end;

function TImageDropTarget.Drop(const DataObject: IDataObject;
  KeyState: DWORD; Point: TPoint; var Effect: DWORD): HResult; stdcall;
var
  Stream: TStream;
  FileName: UTF8String;
  SR: TSniffResult;
  DST: TDropSourceType;
begin
  Result := S_OK;
  if not Assigned(FOnDrop) then
  begin
    FEffect := DROPEFFECT_NONE;
    Effect := FEffect;
    Result := S_OK;
    exit;
  end;

  Stream := FindImage(DataObject, FSniffer, False, DST, FileName, SR);
  try
    if SR = '' then
    begin
      FEffect := DROPEFFECT_NONE;
      Effect := FEffect;
      Result := S_OK;
      exit;
    end;

    FEffect := DROPEFFECT_COPY;
    FOnDrop(Self, KeyState, Point, DST, Stream, FileName, SR, FEffect);
  finally
    FreeAndNil(Stream);
  end;
  Effect := FEffect;
end;

constructor TImageDropTarget.Create;
begin
  inherited Create;
  FSniffer := @DefaultSniffer;
end;

end.
