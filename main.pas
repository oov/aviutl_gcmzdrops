unit Main;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, ActiveX, SysUtils, Classes, ComObj, ShlObj,
  Aviutl, ImageDropTarget;

type

  { TPlugin }

  TPlugin = class(TImageDropTarget)
  private
    FFont: THandle;
    FFontHeight: integer;
    FEditHWND, FModeHWND, FButtonHWND: THandle;
    FEntry: TFilterDLL;
    FExEditHWND: THandle;
    FKnownFolders: array of UTF8String;
    function GetMode(): integer;
    function GetSaveDir(): UTF8String;
    procedure RethrowFiles(const Streams: array of TStream;
      const FileNames: array of UTF8String; const KeyState: DWORD;
      const Point: TPoint);
    procedure OnDropImage(Sender: TObject; const KeyState: DWORD;
      const Point: TPoint; const DST: TDropSourceType; const Stream: TStream;
      const FileName: UTF8String; const SR: TSniffResult; var Effect: DWORD);
    function MainProc(Window: HWND; Message: UINT; WP: WPARAM;
      LP: LPARAM; Edit: Pointer; Filter: PFilter): integer;
    function ProjectLoad(Filter: PFilter; Edit: Pointer; Data: Pointer;
      Size: integer): integer;
    function ProjectSave(Filter: PFilter; Edit: Pointer; Data: Pointer;
      var Size: integer): integer;
    procedure SetMode(AValue: integer);
    procedure SetSaveDir(AValue: UTF8String);
  protected
    function DragEnter(const DataObject: IDataObject; KeyState: DWORD;
      Point: TPoint; var Effect: DWORD): HRESULT; stdcall; override;
    function Drop(const DataObject: IDataObject; KeyState: DWORD;
      Point: TPoint; var Effect: DWORD): HRESULT; stdcall; override;
  public
    constructor Create();
    destructor Destroy(); override;
    property Entry: TFilterDLL read FEntry;
    property SaveDir: UTF8String read GetSaveDir write SetSaveDir;
    property Mode: integer read GetMode write SetMode;
  end;

function GetFilterTable(): PFilterDLL; stdcall;

implementation

uses
  InputDialog, Util;

const
  PluginName = 'ごちゃまぜドロップス';
  PluginNameANSI = #$82#$b2#$82#$bf#$82#$e1#$82#$dc#$82#$ba#$83#$68#$83#$8d#$83#$62#$83#$76#$83#$58;
  PluginInfoANSI = PluginNameANSI + ' v0.1.5';
  ExEditNameANSI = #$8a#$67#$92#$a3#$95#$d2#$8f#$57; // '拡張編集'

var
  Plugin: TPlugin;

function GetFilterTable(): PFilterDLL; stdcall;
begin
  Result := @Plugin.Entry;
end;

function FilterWndProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): LRESULT; cdecl;
begin
  Result := Plugin.MainProc(Window, Message, WP, LP, Edit, Filter);
end;

function FilterProjectLoad(Filter: PFilter; Edit: Pointer; Data: Pointer;
  Size: integer): integer; cdecl;
begin
  Result := Plugin.ProjectLoad(Filter, Edit, Data, Size);
end;

function FilterProjectSave(Filter: PFilter; Edit: Pointer; Data: Pointer;
  var Size: integer): integer; cdecl;
begin
  Result := Plugin.ProjectSave(Filter, Edit, Data, Size);
end;

{ TPlugin }

function TPlugin.MainProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): integer;
var
  Label1, Label2: THandle;
  Y, Height: integer;
  NCM: TNonClientMetrics;
  DC: THandle;
  hs: THandleDynArray;
  S: UTF8String;
begin
  case Message of
    WM_FILTER_INIT:
    begin
      NCM.cbSize := SizeOf(NCM);
      SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NCM), @NCM, 0);
      FFont := CreateFontIndirect(NCM.lfMessageFont);
      DC := GetDC(Window);
      try
        FFontHeight := -MulDiv(NCM.lfMessageFont.lfHeight,
          GetDeviceCaps(DC, LOGPIXELSY), 72);
      finally
        ReleaseDC(Window, DC);
      end;

      Y := 8;

      Height := FFontHeight;
      Label1 := CreateWindowW('STATIC', PWideChar('処理モード:'),
        WS_CHILD or WS_VISIBLE or ES_LEFT, 8, Y, 400, Height,
        Window, 0, Filter^.DLLHInst, nil);
      SendMessageW(Label1, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Height := FFontHeight + GetSystemMetrics(SM_CYFIXEDFRAME) * 2;
      FModeHWND := CreateWindowW('COMBOBOX', nil, WS_CHILD or WS_TABSTOP or
        WS_VISIBLE or CBS_DROPDOWNLIST, 8, Y, 400, Height + 300,
        Window, 1, Filter^.DLLHInst, nil);
      SendMessageW(FModeHWND, CB_ADDSTRING, 0, LPARAM(PWideChar('自動判定')));
      SendMessageW(FModeHWND, CB_ADDSTRING, 0, LPARAM(PWideChar('コピーを作成')));
      SendMessageW(FModeHWND, CB_ADDSTRING, 0, LPARAM(PWideChar('直接読み込み')));
      SendMessageW(FModeHWND, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Inc(Y, 8);

      Height := FFontHeight;
      Label2 := CreateWindowW('STATIC', PWideChar('データ保存先:'),
        WS_CHILD or WS_VISIBLE or ES_LEFT, 8, Y, 400, Height,
        Window, 2, Filter^.DLLHInst, nil);
      SendMessageW(Label2, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Height := FFontHeight + GetSystemMetrics(SM_CYFIXEDFRAME) * 2;
      FEditHWND := CreateWindowExW(WS_EX_CLIENTEDGE, 'EDIT', '',
        WS_CHILD or WS_VISIBLE or WS_TABSTOP or ES_AUTOHSCROLL or
        ES_LEFT, 8, Y, 400, Height, Window, 3234, Filter^.DLLHInst, nil);
      SendMessageW(FEditHWND, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Height := FFontHeight + GetSystemMetrics(SM_CYEDGE) * 2;
      FButtonHWND := CreateWindowW('BUTTON', PWideChar('フォルダーの選択...'),
        WS_CHILD or WS_TABSTOP or WS_VISIBLE, 8 + 400 - 128, Y, 128,
        Height, Window, 4, Filter^.DLLHInst, nil);
      SendMessageW(FButtonHWND, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Inc(Y, GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFIXEDFRAME) * 2 + 8);
      SetWindowPos(Window, 0, 0, 0, 8 + 400 + 8 + GetSystemMetrics(SM_CXFIXEDFRAME) *
        2, Y, SWP_NOMOVE or SWP_NOZORDER);

      FExEditHWND := FindWindowExA(GetParent(Window), 0, 'AviUtl',
        PChar(ExEditNameANSI));
      if FExEditHWND <> 0 then
      begin
        try
          DragAcceptFiles(FExEditHWND, False);
          OleCheck(RegisterDragDrop(FExEditHWND, Self));
        except
          on E: Exception do
          begin
            EnableWindow(Label1, False);
            EnableWindow(FModeHWND, False);
            EnableWindow(Label2, False);
            EnableWindow(FEditHWND, False);
            EnableWindow(FButtonHWND, False);
            SetWindowTextW(Window,
              PWideChar(WideString(PluginName + ' - 初期化に失敗したため使用できません')));
            MessageBoxW(FExEditHWND,
              PWideChar(WideString(E.Message) + #13#10'なお、' +
              PluginName + ' は「字幕アシスト」と同時に使用することはできません。'#13#10 +
              'もし同時に使用していてこのメッセージが表示されている場合は、どちらかのプラグインを読み込まないようにしてください。'),
              PWideChar('初期化エラー - ' + PluginName), MB_ICONERROR);
          end;
        end;
      end
      else
        MessageBoxW(Window, PWideChar('「拡張編集」ウィンドウを見つけられませんでした。'),
          PWideChar('初期化エラー - ' + PluginName), MB_ICONERROR);

      Mode := 0;
      SaveDir := GetKnownFolderPath(CSIDL_DESKTOP);
      Result := 1;
    end;
    WM_FILTER_FILE_OPEN:
    begin
      Mode := 0;
      SaveDir := GetKnownFolderPath(CSIDL_DESKTOP);
    end;
    WM_FILTER_FILE_CLOSE:
    begin
      Mode := 0;
      SaveDir := GetKnownFolderPath(CSIDL_DESKTOP);
    end;
    WM_COMMAND:
    begin
      case LOWORD(WP) of
        4:
        begin
          try
            if LP <> LPARAM(FButtonHWND) then
              exit;
            S := SaveDir;
            hs := DisableFamilyWindows(Window);
            try
              if not SelectDir(Window, S) then
                exit;
              SetWindowTextW(FEditHWND, PWideChar(WideString(S)));
            finally
              EnableFamilyWindows(hs);
            end;
          except
            on E: Exception do
              MessageBoxW(FExEditHWND, PWideChar(WideString(E.Message)),
                PWideChar(WideString('エラー - ' + PluginName)), MB_ICONERROR);
          end;
        end;
        else;
      end;
      Result := 0;
    end;
    WM_FILTER_EXIT:
    begin
      DeleteObject(FFont);
      FFont := 0;
      if Plugin.FExEditHWND <> 0 then
      begin
        OleCheck(RevokeDragDrop(Plugin.FExEditHWND));
        DragAcceptFiles(FExEditHWND, True);
      end;
    end
    else
      Result := 0;
  end;
end;

function TPlugin.ProjectLoad(Filter: PFilter; Edit: Pointer; Data: Pointer;
  Size: integer): integer;
var
  SL: TStringList;
  MS: TMemoryStream;
  I: integer;
  S: RawByteString;
begin
  SL := TStringList.Create();
  try
    MS := TMemoryStream.Create();
    try
      MS.Write(Data^, Size);
      MS.Position := 0;
      SL.LoadFromStream(MS);
    finally
      MS.Free();
    end;
    I := SL.IndexOfName('mode');
    if I <> -1 then
      Mode := StrToIntDef(SL.ValueFromIndex[I], 0);
    I := SL.IndexOfName('savepath');
    if I <> -1 then
    begin
      S := SL.ValueFromIndex[I];
      SetCodePage(S, 65001, False);
      SaveDir := S;
    end;
    Result := AVIUTL_TRUE;
  finally
    SL.Free();
  end;
end;

function TPlugin.ProjectSave(Filter: PFilter; Edit: Pointer; Data: Pointer;
  var Size: integer): integer;
var
  SL: TStringList;
  MS: TMemoryStream;
  S: UTF8String;
begin
  MS := TMemoryStream.Create();
  try
    SL := TStringList.Create();
    try
      S := 'mode=' + IntToStr(Mode);
      SL.Add(S);
      S := 'savepath=' + SaveDir;
      SL.Add(S);
      SL.SaveToStream(MS);
    finally
      SL.Free();
    end;
    Size := MS.Size;
    if Assigned(Data) then
      Move(MS.Memory^, Data^, MS.Size);
    Result := AVIUTL_TRUE;
  finally
    MS.Free();
  end;
end;

procedure TPlugin.SetMode(AValue: integer);
begin
  SendMessageW(FModeHWND, CB_SETCURSEL, WPARAM(AValue), 0);
end;

procedure TPlugin.SetSaveDir(AValue: UTF8String);
begin
  SetWindowTextW(FEditHWND, PWideChar(WideString(AValue)));
end;

function TPlugin.GetMode: integer;
begin
  Result := SendMessageW(FModeHWND, CB_GETCURSEL, 0, 0);
end;

function TPlugin.GetSaveDir: UTF8String;
var
  WS: WideString;
begin
  SetLength(WS, 1024);
  GetWindowTextW(FEditHWND, @WS[1], Length(WS));
  Result := ExcludeTrailingPathDelimiter(UTF8String(PWideChar(WS)));
end;

procedure TPlugin.RethrowFiles(const Streams: array of TStream;
  const FileNames: array of UTF8String; const KeyState: DWORD; const Point: TPoint);

  function Exists(const Postfix: UTF8String): UTF8String;
  var
    S: UTF8String;
    SearchRec: TUnicodeSearchRec;
  begin
    S := SaveDir + DirectorySeparator + '*.' + Postfix;
    if FindFirst(UnicodeString(S), 0, SearchRec) = 0 then
    begin
      try
        Result := SaveDir + DirectorySeparator + UTF8String(SearchRec.Name);
        exit;
      finally
        FindClose(SearchRec);
      end;
    end;
    Result := '';
  end;

type
  ShiftJISString = type ansistring(932);
var
  I, J: integer;
  SJISStr: ShiftJISString;
  S, FileName, Prefix, Postfix: UTF8String;
  ThrowFileNames, DeleteFileNames, DeleteIfFailed: array of UTF8String;
  NeedCopy: array of boolean;
  hs: THandleDynArray;
  FS: TFileStream;
  Pt: TPoint;
begin
  SetLength(ThrowFileNames, Length(Streams));
  SetLength(DeleteFileNames, Length(Streams));
  SetLength(DeleteIfFailed, Length(Streams));
  SetLength(NeedCopy, Length(Streams));
  try
    try
      for I := Low(Streams) to High(Streams) do
      begin
        if Assigned(Streams[I]) then
        begin
          NeedCopy[I] := True;
          continue;
        end;
        case Mode of
          0:
          begin // auto
            NeedCopy[I] := False;
            for J := Low(FKnownFolders) to High(FKnownFolders) do
              if Contains(FKnownFolders[J], FileNames[I]) then
              begin
                NeedCopy[I] := True;
                break;
              end;
          end;
          1: NeedCopy[I] := True; // copy
          2: NeedCopy[I] := False; // direct
          else
            raise Exception.Create('unexpected mode number: ' + IntToStr(Mode));
        end;
      end;

      for I := Low(Streams) to High(Streams) do
      begin
        case LowerCase(ExtractFileExt(FileNames[I])) of
          '.exa', '.exo':
          begin
            if Assigned(Streams[I]) then
            begin
              ThrowFileNames[I] := CreateTempFile(ExtractFileExt(FileNames[I]));
              DeleteFileNames[I] := ThrowFileNames[I];
              FS := TFileStream.Create(ThrowFileNames[I], fmCreate);
              try
                FS.CopyFrom(Streams[I], 0);
              finally
                FS.Free;
              end;
            end
            else
            begin
              ThrowFileNames[I] := FileNames[I];
            end;
            NeedCopy[I] := False;
            continue;
          end;
          '.txt':
          begin
            // convert encoding
            if Assigned(Streams[I]) then
            begin
              SJISStr := ShiftJISString(ReadAsTextFile(Streams[I]));
            end
            else
            begin
              FS := TFileStream.Create(FileNames[I], fmOpenRead);
              try
                SJISStr := ShiftJISString(ReadAsTextFile(FS));
              finally
                FS.Free;
              end;
            end;
            ThrowFileNames[I] := CreateTempFile('.txt');
            DeleteFileNames[I] := ThrowFileNames[I];
            FS := TFileStream.Create(ThrowFileNames[I], fmOpenWrite);
            try
              if Length(SJISStr) > 0 then
                FS.WriteBuffer(SJISStr[1], Length(SJISStr));
            finally
              FS.Free;
            end;
            NeedCopy[I] := False;
            continue;
          end;
          else;
        end;
        case NeedCopy[I] of
          False: ThrowFileNames[I] := FileNames[I];
          True:
          begin
            // generate hashed filename
            if Assigned(Streams[I]) then
              Postfix := GenerateFilename(Streams[I], ExtractFileExt(FileNames[I]))
            else
            begin
              FS := TFileStream.Create(FileNames[I], fmOpenRead);
              try
                Postfix := GenerateFilename(FS, ExtractFileExt(FileNames[I]))
              finally
                FS.Free;
              end;
            end;

            S := Exists(Postfix);
            if S <> '' then
            begin
              ThrowFileNames[I] := S;
              NeedCopy[I] := False;
              continue;
            end;

            hs := DisableFamilyWindows(FExEditHWND);
            try
              Prefix := ChangeFileExt(ExtractFileName(FileNames[I]), '');
              case InputBox(FExEditHWND, 'ファイルにニックネームをつけてください - ' +
                  PluginName, not Assigned(Streams[I]), Prefix) of
                idOk:
                begin
                  S :=
                    SaveDir + DirectorySeparator + CleanFileName(Prefix) +
                    '.' + Postfix;
                  if Assigned(Streams[I]) then
                  begin
                    FS := TFileStream.Create(S, fmCreate);
                    try
                      FS.CopyFrom(Streams[I], 0);
                    finally
                      FS.Free;
                    end;
                  end
                  else
                  begin
                    if not CopyFileW(PWideChar(WideString(FileNames[I])),
                      PWideChar(WideString(S)), True) then
                      RaiseLastOSError();
                  end;
                  ThrowFileNames[I] := S;
                  DeleteIfFailed[I] := S;
                end;
                idCancel: raise EAbort.Create('キャンセルが押されたため中止しました');
                idIgnore:
                begin
                  ThrowFileNames[I] := FileNames[I];
                  NeedCopy[I] := False;
                end;
                else
                  raise Exception.Create('unexpected dialog return value');
              end;
            finally
              EnableFamilyWindows(hs);
            end;
          end;
          else;
        end;
      end;
      SetLength(FileName, 1024);
      if GetModuleFileName(0, @FileName[1], Length(FileName)) = 0 then
        RaiseLastOSError();
      FileName := IncludeTrailingPathDelimiter(ExtractFilePath(PChar(FileName))) +
        'aviutl.ini';
      SetLength(S, 1024);
      GetPrivateProfileString(PChar(ExEditNameANSI),
        'small_layer', '0', @S[1], Length(S), PChar(FileName));
      case StrToIntDef(PChar(S), 0) of
        0: J := 31;
        1: J := 26;
        2: J := 22;
        else
          J := 31;
      end;
      Pt := Point;
      for I := Low(ThrowFileNames) to High(ThrowFileNames) do
      begin
        EmulateDropOne(FExEditHWND, Pt, ThrowFileNames[I]);
        Inc(Pt.y, J);
      end;
    except
      on e: Exception do
      begin
        for I := Low(DeleteIfFailed) to High(DeleteIfFailed) do
          if DeleteIfFailed[I] <> '' then
            DeleteFileW(PWideChar(WideString(DeleteIfFailed[I])));
        if not (e is EAbort) then
          raise;
      end;
    end;
  finally
    for I := Low(DeleteFileNames) to High(DeleteFileNames) do
      if DeleteFileNames[I] <> '' then
        DeleteFileW(PWideChar(WideString(DeleteFileNames[I])));
  end;
end;

procedure TPlugin.OnDropImage(Sender: TObject; const KeyState: DWORD;
  const Point: TPoint; const DST: TDropSourceType; const Stream: TStream;
  const FileName: UTF8String; const SR: TSniffResult; var Effect: DWORD);
var
  Streams: array of TStream;
  FileNames: array of UTF8String;
begin
  try
    if DST = dstHDROP then
    begin
      // In this case, we need detect the processing mode accurately,
      // so we don't process it here.
      Effect := DROPEFFECT_NONE;
      Exit;
    end;
    SetLength(Streams, 1);
    SetLength(FileNames, 1);
    Streams[0] := Stream;
    FileNames[0] := fileName;
    RethrowFiles(Streams, FileNames, KeyState, Point);
  except
    on E: Exception do
      MessageBoxW(FExEditHWND, PWideChar(WideString(E.Message)),
        PWideChar(WideString('エラー - ' + PluginName)), MB_ICONERROR);
  end;
end;

function TPlugin.DragEnter(const DataObject: IDataObject; KeyState: DWORD;
  Point: TPoint; var Effect: DWORD): HRESULT; stdcall;
var
  FileNames: TFileNames;
  S: UTF8String;
  FDWA: TFileDescriptorWArray;
begin
  Result := inherited DragEnter(DataObject, KeyState, Point, Effect);
  if Effect <> DROPEFFECT_NONE then
    exit;

  FileNames := ReadHDROP(DataObject);
  if Length(FileNames) > 0 then
  begin
    FEffect := DROPEFFECT_COPY;
    Effect := FEffect;
    Result := S_OK;
    exit;
  end;

  FDWA := GetFileDescriptors(DataObject);
  if Length(FDWA) > 0 then
  begin
    FEffect := DROPEFFECT_COPY;
    Effect := FEffect;
    Result := S_OK;
    exit;
  end;

  S := ReadTextUTF8(DataObject);
  if S <> '' then
  begin
    FEffect := DROPEFFECT_COPY;
    Effect := FEffect;
    Result := S_OK;
    exit;
  end;
end;

function TPlugin.Drop(const DataObject: IDataObject; KeyState: DWORD;
  Point: TPoint; var Effect: DWORD): HRESULT; stdcall;
var
  I: integer;
  FNs: TFileNames;
  FileNames: array of UTF8String;
  Streams: array of TStream;
  S: UTF8String;
  FDWA: TFileDescriptorWArray;
begin
  try
    Result := inherited Drop(DataObject, KeyState, Point, Effect);
    if Effect <> DROPEFFECT_NONE then
      exit;

    FNs := ReadHDROP(DataObject);
    if Length(FNs) > 0 then
    begin
      SetLength(FileNames, Length(FNs));
      SetLength(Streams, Length(FNs));
      for I := Low(FNs) to High(FNs) do
      begin
        FileNames[I] := FNs[I];
        Streams[I] := nil;
      end;
      RethrowFiles(Streams, FileNames, KeyState, Point);
      FEffect := DROPEFFECT_COPY;
      Effect := FEffect;
      Result := S_OK;
      exit;
    end;

    FDWA := GetFileDescriptors(DataObject);
    if Length(FDWA) > 0 then
    begin
      SetLength(FileNames, Length(FDWA));
      SetLength(Streams, Length(FDWA));
      try
        for I := Low(FDWA) to High(FDWA) do
        begin
          FileNames[I] := UTF8String(WideString(PWideChar(@FDWA[0].cFileName[0])));
          Streams[I] := ReadFileContents(DataObject, I);
        end;
        RethrowFiles(Streams, FileNames, KeyState, Point);
      finally
        for I := Low(FDWA) to High(FDWA) do
          FreeAndNil(Streams[I]);
      end;
      FEffect := DROPEFFECT_COPY;
      Effect := FEffect;
      Result := S_OK;
      exit;
    end;

    S := ReadTextUTF8(DataObject);
    if S <> '' then
    begin
      SetLength(FileNames, 1);
      SetLength(Streams, 1);
      FileNames[0] := 'noname.txt';
      Streams[0] := TMemoryStream.Create;
      try
        // write UTF-8 BOM for auto encoding detection.
        Streams[0].WriteBuffer(#$ef#$bb#$bf[1], 3);
        Streams[0].WriteBuffer(S[1], Length(S));
        Streams[0].Position := 0;
        RethrowFiles(Streams, FileNames, KeyState, Point);
      finally
        FreeAndNil(Streams[0]);
      end;
      FEffect := DROPEFFECT_COPY;
      Effect := FEffect;
      Result := S_OK;
      exit;
    end;
  except
    on E: Exception do
      MessageBoxW(FExEditHWND, PWideChar(WideString(E.Message)),
        PWideChar(WideString('エラー - ' + PluginName)), MB_ICONERROR);
  end;
end;

constructor TPlugin.Create;
const
  CSIDLs: array[0..10] of longint = (
    CSIDL_APPDATA,
    CSIDL_LOCAL_APPDATA,
    CSIDL_COMMON_APPDATA,
    CSIDL_COOKIES,
    CSIDL_INTERNET_CACHE,
    CSIDL_PROGRAM_FILES,
    CSIDL_PROGRAM_FILES_COMMON,
    CSIDL_STARTMENU,
    CSIDL_PROGRAMS,
    CSIDL_WINDOWS,
    CSIDL_SYSTEM);
var
  I: integer;
begin
  inherited Create;
  SetLength(FKnownFolders, Length(CSIDLs));
  for I := Low(CSIDLs) to High(CSIDLs) do
    FKnownFolders[I] := GetKnownFolderPath(CSIDLs[I]);

  FillChar(FEntry, SizeOf(FEntry), 0);
  FEntry.Flag := FILTER_FLAG_ALWAYS_ACTIVE or FILTER_FLAG_DISP_FILTER or
    FILTER_FLAG_EX_INFORMATION;

  FEntry.Name := PChar(PluginNameANSI);
  FEntry.FuncWndProc := @FilterWndProc;
  FEntry.FuncProjectLoad := @FilterProjectLoad;
  FEntry.FuncProjectSave := @FilterProjectSave;
  FEntry.Information := PChar(PluginInfoANSI);

  OnDrop := @OnDropImage;
  Randomize();
end;

destructor TPlugin.Destroy;
begin
  inherited Destroy;
end;

initialization
  OleInitialize(nil);
  Plugin := TPlugin.Create();
  Plugin._AddRef();

finalization
  Plugin._Release();
  OleUninitialize();
end.
