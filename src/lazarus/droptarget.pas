unit DropTarget;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, ActiveX, ShlObj, Classes;

type
  TDraggingFileType = (dftFile, dftText);

  TDraggingFile = record
    Type_: TDraggingFileType;
    FilePathOrContent: UTF8String;
    DeleteOnFinish: boolean;
    MediaType: UTF8String;
  end;
  TDraggingFiles = array of TDraggingFile;

  TDragDropInfo = record
    Point: TPoint;
    KeyState: DWORD;
    DraggingFiles: TDraggingFiles;
    Effect: DWORD;
  end;
  PDragDropInfo = ^TDragDropInfo;

  TDragDropEvent = procedure(Sender: TObject; const PDDI: PDragDropInfo) of object;

  TSniffResult = UTF8String;
  TSniffer = function(const Stream: TStream): TSniffResult of object;

  TTextConverter = procedure(const Text: UTF8String; out DF: TDraggingFile) of object;

  { TDropTarget }

  TDropTarget = class(TInterfacedObject, IDropTarget)
  private
    FOnDragEnter: TDragDropEvent;
    FOnDragLeave: TNotifyEvent;
    FOnDragOver: TDragDropEvent;
    FOnDrop: TDragDropEvent;
    FSniffer: TSniffer;
    FEffect: DWORD;
    FDraggingFiles: TDraggingFiles;
    FTextConverter: TTextConverter;
    procedure SetSniffer(AValue: TSniffer);
    function DefaultSniffer(const Stream: TStream): TSniffResult;
    procedure DefaultTextConverter(const Text: UTF8String; out DF: TDraggingFile);
    function DragEnter(const DataObject: IDataObject; KeyState: DWORD;
      Point: TPoint; var Effect: DWORD): HResult; stdcall;
    function DragOver(KeyState: DWORD; Point: TPoint; var Effect: DWORD): HResult;
      stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const DataObject: IDataObject; KeyState: DWORD;
      Point: TPoint; var Effect: DWORD): HResult; stdcall;
    procedure Cleanup();
    procedure SetTextConverter(AValue: TTextConverter);
  public
    constructor Create();
    destructor Destroy(); override;
    property OnDragEnter: TDragDropEvent read FOnDragEnter write FOnDragEnter;
    property OnDragOver: TDragDropEvent read FOnDragOver write FOnDragOver;
    property OnDragLeave: TNotifyEvent read FOnDragLeave write FOnDragLeave;
    property OnDrop: TDragDropEvent read FOnDrop write FOnDrop;
    property Sniffer: TSniffer read FSniffer write SetSniffer;
    property TextConverter: TTextConverter read FTextConverter write SetTextConverter;
  end;

  { TTempFileStream }

  TTempFileStream = class(THandleStream)
  private
    FFilePath: UTF8String;
  public
    constructor Create(const BaseFileName: UTF8String);
    destructor Destroy; override;
    property FilePath: UTF8String read FFilePath;
  end;

implementation

uses
  SysUtils, ComObj, base64, UsedFiles;

type
  TFileDescriptorWArray = array of TFileDescriptorW;
  TFileNames = array of UTF8String;

{ TTempFileStream }

constructor TTempFileStream.Create(const BaseFileName: UTF8String);
var
  H: THandle;
  FileNameNoExt, Ext, BaseDir: UTF8String;
  S: WideString;
  I: integer;
begin
  Ext := ExtractFileExt(BaseFileName);
  FileNameNoExt := ChangeFileExt(BaseFileName, '');

  SetLength(BaseDir, MAX_PATH);
  GetTempPathW(MAX_PATH, @BaseDir[1]);
  BaseDir := IncludeTrailingPathDelimiter(UTF8String(PWideChar(BaseDir)));

  S := WideString(BaseDir + FileNameNoExt + Ext);
  for I := 0 to 10 do
  begin
    H := CreateFileW(PWideChar(S), GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ, nil, CREATE_NEW, FILE_ATTRIBUTE_TEMPORARY or
      FILE_ATTRIBUTE_NOT_CONTENT_INDEXED, 0);
    if H = INVALID_HANDLE_VALUE then
    begin
      S := WideString(BaseDir + FileNameNoExt + '.' +
        IntToStr(Random($ffffff)) + Ext);
      continue;
    end;
    inherited Create(H);
    FFilePath := UTF8String(S);
    Exit;
  end;
  raise Exception.Create('cannot create a temporary file');
end;

destructor TTempFileStream.Destroy;
begin
  CloseHandle(Handle);
  inherited Destroy;
end;

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
      if (P^ = $25) and (P + 2 < EndPos) then
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

function DataURISchemeToStream(const S: RawByteString;
  out MediaType: UTF8String): TStream;
var
  I, Left: integer;
  SL: TStringList;
  IsBase64: boolean;
begin
  Result := nil;
  if Copy(S, 1, 5) <> 'data:' then
    exit;
  Left := Pos(',', S) + 1;
  if Left = 1 then
    exit;
  SL := TStringList.Create;
  try
    SL.Delimiter := ';';
    SL.StrictDelimiter := True;
    SL.DelimitedText := Copy(S, 6, Left - 7);
    MediaType := '';
    for I := 0 to SL.Count - 1 do
    begin
      if SL[I] = 'base64' then
      begin
        IsBase64 := True;
        continue;
      end;
      MediaType := MediaType + SL[I] + ';';
    end;
    if Length(MediaType) > 0 then
      SetLength(MediaType, Length(MediaType) - 1);
  finally
    SL.Free;
  end;
  if IsBase64 then
    Result := DecodeBase64(Copy(S, Left, Length(S) - Left + 1))
  else
    Result := DecodePercentEncoding(Copy(S, Left, Length(S) - Left + 1));
end;

function ExtractFileNameLikeString(const S: UTF8String): UTF8String;
var
  Src, Pos: PChar;
begin
  Src := PChar(S);
  Pos := StrRScan(Src, '\');
  if Assigned(Pos) then
  begin
    Inc(Pos);
    Result := Pos;
    Exit;
  end;

  Pos := StrRScan(Src, '/');
  if Assigned(Pos) then
  begin
    Inc(Pos);
    Result := Pos;
    Exit;
  end;

  Result := S;
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

function SuggestDataURISchemeFileName(const S: RawByteString;
  const Stream: TStream; const MediaType: UTF8String;
  const Sniffer: TSniffer): UTF8String;
var
  I: integer;
  SL: TStringList;
  Mime: UTF8String;
  FNStream: TStream;
begin
  SL := TStringList.Create;
  try
    SL.Delimiter := ';';
    SL.StrictDelimiter := True;
    SL.DelimitedText := MediaType;
    for I := 0 to SL.Count - 1 do
    begin
      if (Result = '') and (Pos('filename=', SL[I]) = 1) then
      begin
        FNStream := DecodePercentEncoding(Trim(Copy(SL[I], 10, Length(SL[I]) - 9)));
        try
          SetLength(Result, FNStream.Size);
          FNStream.Position := 0;
          FNStream.ReadBuffer(Result[1], FNStream.Size);
          Result := ExtractFileNameLikeString(Result);
        finally
          FNStream.Free;
        end;
      end;
    end;
    Mime := LowerCase(Trim(SL[0]));
  finally
    SL.Free;
  end;
  if Result <> '' then
    Exit;

  if Length(S) > 24 then
  begin
    Result := Sanitize(Copy(S, Length(S) - 24, 24), '-');
    Result := StringReplace(Result, '+', '-', [rfReplaceAll]);
    Result := ExtractFileNameLikeString(Result);
  end
  else
    Result := 'noname';
  if Mime = '' then
    Mime := Sniffer(Stream);
  case Mime of
    'image/x-icon': Result := Result + '.ico';
    'image/vnd.microsoft.icon': Result := Result + '.ico';
    'image/bmp': Result := Result + '.bmp';
    'image/gif': Result := Result + '.gif';
    'image/webp': Result := Result + '.webp';
    'image/png': Result := Result + '.png';
    'image/jpeg': Result := Result + '.jpg';
    'audio/basic': Result := Result + '.snd';
    'audio/aiff': Result := Result + '.aiff';
    'audio/mpeg': Result := Result + '.mp3';
    'application/ogg': Result := Result + '.ogg';
    'audio/midi': Result := Result + '.mid';
    'video/avi': Result := Result + '.avi';
    'audio/wave': Result := Result + '.wav';
    'video/mp4': Result := Result + '.mp4';
    'video/webm': Result := Result + '.webm';
    'application/pdf': Result := Result + '.pdf';
    'text/plain': Result := Result + '.txt';
    else
      Result := Result + '.bin';
  end;
end;

procedure TDropTarget.SetSniffer(AValue: TSniffer);
begin
  if FSniffer = AValue then
    Exit;
  if AValue = nil then
    FSniffer := @DefaultSniffer
  else
    FSniffer := AValue;
end;

function TDropTarget.DefaultSniffer(const Stream: TStream): TSniffResult;
var
  PB: array[0..15] of byte;
begin
  if Stream.Size < 16 then
  begin
    Result := '';
    Exit;
  end;
  Stream.ReadBuffer(PB, 16);

  // https://mimesniff.spec.whatwg.org/#matching-a-mime-type-pattern
  // TODO: implement video/mp4, video/webm, audio/mpeg matcher
  if (PB[0] = Ord('G')) and (PB[1] = Ord('I')) and (PB[2] = Ord('F')) and
    (PB[3] = Ord('8')) and (((PB[4] = Ord('7')) or (PB[4] = Ord('9')))) and
    (PB[5] = Ord('a')) then
    Result := 'gif'
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
  else if (PB[0] = $00) and (PB[1] = $00) and (PB[2] = $01) and (PB[3] = $00) then
    Result := '.ico'
  else if (PB[0] = $00) and (PB[1] = $00) and (PB[2] = $02) and (PB[3] = $00) then
    Result := '.cur'
  else if (PB[0] = $42) and (PB[1] = $4d) then
    Result := '.bmp'
  else if (PB[0] = $2e) and (PB[1] = $73) and (PB[2] = $6e) and (PB[3] = $64) then
    Result := '.snd'
  else if (PB[0] = $46) and (PB[1] = $4f) and (PB[2] = $52) and
    (PB[3] = $4d) and (PB[8] = $41) and (PB[9] = $49) and (PB[10] = $46) and
    (PB[11] = $46) then
    Result := '.aiff'
  else if (PB[0] = $49) and (PB[1] = $44) and (PB[2] = $33) then
    Result := '.mp3'
  else if (PB[0] = $4f) and (PB[1] = $67) and (PB[2] = $67) and
    (PB[3] = $53) and (PB[4] = $00) then
    Result := '.ogg'
  else if (PB[0] = $4d) and (PB[1] = $54) and (PB[2] = $68) and
    (PB[3] = $64) and (PB[4] = $00) and (PB[5] = $00) and (PB[6] = $00) and
    (PB[7] = $06) then
    Result := '.mid'
  else if (PB[0] = $52) and (PB[1] = $49) and (PB[2] = $46) and
    (PB[3] = $46) and (PB[8] = $41) and (PB[9] = $56) and (PB[10] = $49) and
    (PB[11] = $20) then
    Result := '.avi'
  else if (PB[0] = $52) and (PB[1] = $49) and (PB[2] = $46) and
    (PB[3] = $46) and (PB[8] = $57) and (PB[9] = $41) and (PB[10] = $56) and
    (PB[11] = $45) then
    Result := '.wav'
  else if (PB[0] = $25) and (PB[1] = $50) and (PB[2] = $44) and
    (PB[3] = $46) and (PB[4] = $2D) then
    Result := '.pdf'
  else
    Result := '';
end;

procedure TDropTarget.DefaultTextConverter(const Text: UTF8String;
  out DF: TDraggingFile);
begin
  DF.Type_ := dftText;
  DF.FilePathOrContent := Text;
  DF.DeleteOnFinish := False;
  DF.MediaType := 'text/plain; charset=UTF-8';
end;

function TryReadFromDataURIScheme(const Text: UTF8String; const Sniffer: TSniffer;
  var DraggingFiles: TDraggingFiles): boolean;
var
  MediaType: UTF8String;
  Stream: TStream;
  FileName: UTF8String;
  I: integer;
  TFS: TTempFileStream;
begin
  Result := False;
  Stream := DataURISchemeToStream(Text, MediaType);
  if not Assigned(Stream) then
    Exit;
  try
    FileName := SuggestDataURISchemeFileName(Text, Stream, MediaType, Sniffer);
    Stream.Position := 0;

    TFS := TTempFileStream.Create(FileName);
    try
      TFS.CopyFrom(Stream, 0);
      TFS.Position := 0;

      I := Length(DraggingFiles);
      SetLength(DraggingFiles, I + 1);
      DraggingFiles[I].Type_ := dftFile;
      DraggingFiles[I].FilePathOrContent := TFS.FilePath;
      DraggingFiles[I].DeleteOnFinish := True;
      DraggingFiles[I].MediaType := MediaType;
      Result := True;
    finally
      TFS.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function TryReadFromFileContents(const DataObject: IDataObject;
  var DraggingFiles: TDraggingFiles): boolean;
var
  FDWA: TFileDescriptorWArray;
  I, J: integer;
  Stream: TStream;
  FileName: UTF8String;
  TFS: TTempFileStream;
  CreatedFiles: TStringList;
begin
  FDWA := GetFileDescriptors(DataObject);
  if Length(FDWA) = 0 then
  begin
    Result := False;
    Exit;
  end;

  CreatedFiles := TStringList.Create;
  try
    try
      for I := Low(FDWA) to High(FDWA) do
      begin
        FileName := ExtractFileNameLikeString(
          Sanitize(UTF8String(PWideChar(@FDWA[I].cFileName[0])), '-'));
        Stream := ReadFileContents(DataObject, I);
        if not Assigned(Stream) then
        begin
          Result := False;
          for J := CreatedFiles.Count - 1 downto 0 do
            AddUsedFile(CreatedFiles.Strings[J]);
          Exit;
        end;
        try
          Stream.Position := 0;
          TFS := TTempFileStream.Create(FileName);
          try
            CreatedFiles.Add(TFS.FilePath);

            TFS.CopyFrom(Stream, 0);
            TFS.Position := 0;

            J := Length(DraggingFiles);
            SetLength(DraggingFiles, J + 1);
            DraggingFiles[J].Type_ := dftFile;
            DraggingFiles[J].FilePathOrContent := TFS.FilePath;
            DraggingFiles[J].DeleteOnFinish := True;
          finally
            TFS.Free;
          end;
        finally
          FreeAndNil(Stream);
        end;
      end;
      Result := True;
    except
      for J := CreatedFiles.Count - 1 downto 0 do
        AddUsedFile(CreatedFiles.Strings[J]);
      raise;
    end;
  finally
    FreeAndNil(CreatedFiles);
  end;
end;

function TryReadFromHDROP(const DataObject: IDataObject;
  var DraggingFiles: TDraggingFiles): boolean;
var
  FileNames: TFileNames;
  I, J: integer;
begin
  FileNames := ReadHDROP(DataObject);
  if Length(FileNames) = 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  for I := Low(FileNames) to High(FileNames) do
  begin
    J := Length(DraggingFiles);
    SetLength(DraggingFiles, J + 1);
    DraggingFiles[J].Type_ := dftFile;
    DraggingFiles[J].FilePathOrContent := FileNames[I];
  end;
end;

function TDropTarget.DragEnter(const DataObject: IDataObject;
  KeyState: DWORD; Point: TPoint; var Effect: DWORD): HResult; stdcall;
var
  I: integer;
  Text: UTF8String;
  DDI: TDragDropInfo;
begin
  Result := S_OK;
  if not Assigned(FOnDrop) then
  begin
    FEffect := DROPEFFECT_NONE;
    Effect := FEffect;
    Exit;
  end;

  try
    Cleanup();
    Text := ReadTextUTF8(DataObject);
    if not TryReadFromDataURIScheme(Text, FSniffer, FDraggingFiles) then
      if not TryReadFromFileContents(DataObject, FDraggingFiles) then
        if not TryReadFromHDROP(DataObject, FDraggingFiles) then
        begin
          I := Length(FDraggingFiles);
          SetLength(FDraggingFiles, I + 1);
          FTextConverter(Text, FDraggingFiles[I]);
        end;
    if Assigned(FOnDragEnter) then
    begin
      DDI.Point := Point;
      DDI.KeyState := KeyState;
      DDI.DraggingFiles := FDraggingFiles;
      DDI.Effect := FEffect;
      FOnDragEnter(Self, @DDI);
      FEffect := DDI.Effect;
    end;
    Effect := FEffect;
  except
    Effect := DROPEFFECT_NONE;
    raise;
  end;
end;

function TDropTarget.DragOver(KeyState: DWORD; Point: TPoint;
  var Effect: DWORD): HResult; stdcall;
var
  DDI: TDragDropInfo;
begin
  Result := S_OK;
  if not Assigned(FOnDragOver) then
  begin
    Effect := FEffect;
    Exit;
  end;
  try
    if Assigned(FOnDragOver) then
    begin
      DDI.Point := Point;
      DDI.KeyState := KeyState;
      DDI.DraggingFiles := FDraggingFiles;
      DDI.Effect := FEffect;
      FOnDragOver(Self, @DDI);
      FEffect := DDI.Effect;
    end;
    Effect := FEffect;
  except
    Effect := DROPEFFECT_NONE;
    raise;
  end;
end;

function TDropTarget.DragLeave: HResult; stdcall;
begin
  Result := S_OK;
  if Assigned(FOnDragLeave) then
    FOnDragLeave(Self);
  Cleanup();
end;

function TDropTarget.Drop(const DataObject: IDataObject; KeyState: DWORD;
  Point: TPoint; var Effect: DWORD): HResult; stdcall;
var
  I: integer;
  Text: UTF8String;
  DDI: TDragDropInfo;
begin
  Result := S_OK;
  if not Assigned(FOnDrop) then
  begin
    FEffect := DROPEFFECT_NONE;
    Effect := FEffect;
    Cleanup();
    exit;
  end;

  try
    Cleanup();
    Text := ReadTextUTF8(DataObject);
    if not TryReadFromDataURIScheme(Text, FSniffer, FDraggingFiles) then
      if not TryReadFromFileContents(DataObject, FDraggingFiles) then
        if not TryReadFromHDROP(DataObject, FDraggingFiles) then
        begin
          I := Length(FDraggingFiles);
          SetLength(FDraggingFiles, I + 1);
          FTextConverter(Text, FDraggingFiles[I]);
        end;
    if Assigned(FOnDrop) then
    begin
      DDI.Point := Point;
      DDI.KeyState := KeyState;
      DDI.DraggingFiles := FDraggingFiles;
      DDI.Effect := FEffect;
      FOnDrop(Self, @DDI);
      FEffect := DDI.Effect;
    end;
    Effect := FEffect;
    Cleanup();
  except
    Cleanup();
    Effect := DROPEFFECT_NONE;
    raise;
  end;
end;

procedure TDropTarget.Cleanup;
var
  I: integer;
begin
  for I := Low(FDraggingFiles) to High(FDraggingFiles) do
    if (FDraggingFiles[I].FilePathOrContent <> '') and FDraggingFiles[I].DeleteOnFinish then
      AddUsedFile(FDraggingFiles[I].FilePathOrContent);
  SetLength(FDraggingFiles, 0);
end;

procedure TDropTarget.SetTextConverter(AValue: TTextConverter);
begin
  if FTextConverter = AValue then
    Exit;
  if AValue = nil then
    FTextConverter := @DefaultTextConverter
  else
    FTextConverter := AValue;
end;

constructor TDropTarget.Create;
begin
  inherited Create;
  FSniffer := @DefaultSniffer;
  FTextConverter := @DefaultTextConverter;
end;

destructor TDropTarget.Destroy;
begin
  Cleanup();
  inherited Destroy;
end;

end.
