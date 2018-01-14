unit Main;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, ActiveX, SysUtils, Classes, ComObj, ShlObj,
  AviUtl, DropTarget, LuaObj;

type
  { TGCMZDrops }

  TGCMZDrops = class
  private
    FGCMZDropsMessageId: UINT;
    FEntry: TFilterDLL;
    FExEdit, FCurrentFilterP: PFilter;
    FCurrentEditP: Pointer;
    FDropTarget: TDropTarget;
    FDropTargetIntf: IDropTarget;
    FLua, FSCDropperLua: TLua;
    FWindow, FFont: THandle;
    FFontHeight: integer;
    FSavePathEdit, FSaveMode, FFolderSelectButton, FRevertChangeButton: THandle;
    FKnownFolders: array of UTF8String;
    FDeleteOnFinishFileQueue: TStringList;
    FDeleteOnAbortFileQueue: TStringList;
    function GetEntry: PFilterDLL;
    function GetExEditLayerHeight: integer;
    function GetExEditWindow: THandle;
    function GetMode(): integer;
    procedure SetMode(AValue: integer);
    function GetSaveDir(): UTF8String;
    procedure SetSaveDir(AValue: UTF8String);

    procedure ProcessDeleteFileQueue(const Error: boolean);
    function InitProc(Filter: PFilter): boolean;
    function ExitProc(Filter: PFilter): boolean;
    function MainProc(Window: HWND; Message: UINT; WP: WPARAM;
      LP: LPARAM; Edit: Pointer; Filter: PFilter): integer;
    function ProjectLoadProc(Filter: PFilter; Edit: Pointer; Data: Pointer;
      Size: integer): boolean;
    function ProjectSaveProc(Filter: PFilter; Edit: Pointer; Data: Pointer;
      var Size: integer): boolean;

    procedure TextConvert(const Text: UTF8String; out DF: TDraggingFile);
    procedure OnDragEnter(Sender: TObject; const PDDI: PDragDropInfo);
    procedure OnDragOver(Sender: TObject; const PDDI: PDragDropInfo);
    procedure OnDragLeave(Sender: TObject);
    procedure OnDrop(Sender: TObject; const PDDI: PDragDropInfo);
    procedure OnPopupSCDropperMenu(Sender: TObject; const Pt: TPoint);
  public
    constructor Create();
    destructor Destroy(); override;
    property Entry: PFilterDLL read GetEntry;
    property SaveDir: UTF8String read GetSaveDir write SetSaveDir;
    property Mode: integer read GetMode write SetMode;
    property ExEditWindow: THandle read GetExEditWindow;
    property ExEditLayerHeight: integer read GetExEditLayerHeight;
    function NeedCopy(FilePath: UTF8String): boolean;
    function ExistsInGCMZDir(FilePath: UTF8String): boolean;
    function GetSavePath(): UTF8String;
    procedure RegisterDeleteOnFinish(S: UTF8String);
    procedure RegisterDeleteOnAbort(S: UTF8String);
    procedure GetAviUtlSysInfo(out SI: TSysInfo);
    procedure GetExEditFileInfo(out FI: TFileInfo);
    function IsEditing(): boolean;
    procedure GetFileInfo(out FI: TFileInfo; out Samples: integer;
      const FileName: UTF8String);
    function Prompt(const Caption: UTF8String; var Value: UTF8String): boolean;
    function Confirm(const Caption: UTF8String): boolean;
  end;

function GetFilterTableList(): PPFilterDLL; stdcall;

var
  GCMZDrops: TGCMZDrops;

implementation

uses
  InputDialog, ScriptableDropper, UsedFiles, Lua, Util, Ver;

const
  PluginName = 'ごちゃまぜドロップス';
  PluginNameANSI = #$82#$b2#$82#$bf#$82#$e1#$82#$dc#$82#$ba#$83#$68#$83#$8d#$83#$62#$83#$76#$83#$58;
  PluginInfoANSI = PluginNameANSI + ' ' + Version;
  ExEditNameANSI = #$8a#$67#$92#$a3#$95#$d2#$8f#$57; // '拡張編集'
  DefaultSaveDir = '%PROJECTDIR%\gcmz';

const
  BoolConv: array[boolean] of AviUtlBool = (AVIUTL_FALSE, AVIUTL_TRUE);

var
  FilterDLLList: array of PFilterDLL;

function GetFilterTableList(): PPFilterDLL; stdcall;
begin
  Result := @FilterDLLList[0];
end;

function FilterFuncInit(fp: PFilter): AviUtlBool; cdecl;
begin
  Result := BoolConv[GCMZDrops.InitProc(fp)];
end;

function FilterFuncExit(fp: PFilter): AviUtlBool; cdecl;
begin
  Result := BoolConv[GCMZDrops.ExitProc(fp)];
end;

function FilterFuncWndProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): LRESULT; cdecl;
begin
  Result := GCMZDrops.MainProc(Window, Message, WP, LP, Edit, Filter);
end;

function FilterFuncProjectLoad(Filter: PFilter; Edit: Pointer;
  Data: Pointer; Size: integer): AviUtlBool; cdecl;
begin
  Result := BoolConv[GCMZDrops.ProjectLoadProc(Filter, Edit, Data, Size)];
end;

function FilterFuncProjectSave(Filter: PFilter; Edit: Pointer;
  Data: Pointer; var Size: integer): AviUtlBool; cdecl;
begin
  Result := BoolConv[GCMZDrops.ProjectSaveProc(Filter, Edit, Data, Size)];
end;

{ TGCMZDrops }

function TGCMZDrops.MainProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): integer;
const
  ExEditVersion = ' version 0.92 ';
  ExTextNameANSI = #$8e#$9a#$96#$8b#$83#$41#$83#$56#$83#$58#$83#$67;
  // '字幕アシスト'
var
  Label1, Label2: THandle;
  Y, Height: integer;
  NCM: TNonClientMetrics;
  DC: THandle;
  hs: THandleDynArray;
  S: UTF8String;
  sinfo: TSysInfo;
  fp: PFilter;
  PDDI: PDragDropInfo;
begin
  case Message of
    WM_FILTER_INIT:
    begin
      FWindow := Window;
      if Filter^.ExFunc^.GetSysInfo(Edit, @sinfo) <> AVIUTL_FALSE then
      begin
        for Y := 0 to sinfo.FilterN - 1 do
        begin
          fp := Filter^.ExFunc^.GetFilterP(Y);
          if (fp = nil) or (fp^.Name <> ExEditNameANSI) then
            continue;
          FExEdit := fp;
          break;
        end;
      end;

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
      FSaveMode := CreateWindowW('COMBOBOX', nil, WS_CHILD or WS_TABSTOP or
        WS_VISIBLE or CBS_DROPDOWNLIST, 8, Y, 400, Height + 300,
        Window, 1, Filter^.DLLHInst, nil);
      SendMessageW(FSaveMode, CB_ADDSTRING, 0, {%H-}LPARAM(PWideChar('自動判定')));
      SendMessageW(FSaveMode, CB_ADDSTRING, 0,
        {%H-}LPARAM(PWideChar('コピーを作成')));
      SendMessageW(FSaveMode, CB_ADDSTRING, 0,
        {%H-}LPARAM(PWideChar('直接読み込み')));
      SendMessageW(FSaveMode, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Inc(Y, 8);

      Height := FFontHeight;
      Label2 := CreateWindowW('STATIC', PWideChar('データ保存先:'),
        WS_CHILD or WS_VISIBLE or ES_LEFT, 8, Y, 400, Height,
        Window, 2, Filter^.DLLHInst, nil);
      SendMessageW(Label2, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Height := FFontHeight + GetSystemMetrics(SM_CYFIXEDFRAME) * 2;
      FSavePathEdit := CreateWindowExW(WS_EX_CLIENTEDGE, 'EDIT', '',
        WS_CHILD or WS_VISIBLE or WS_TABSTOP or ES_AUTOHSCROLL or
        ES_LEFT, 8, Y, 400, Height, Window, 3234, Filter^.DLLHInst, nil);
      SendMessageW(FSavePathEdit, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Height := FFontHeight + GetSystemMetrics(SM_CYEDGE) * 2;
      FRevertChangeButton := CreateWindowW('BUTTON', PWideChar('標準設定に戻す'),
        WS_CHILD or WS_TABSTOP or WS_VISIBLE, 8 + 400 - 256, Y, 128,
        Height, Window, 5, Filter^.DLLHInst, nil);
      SendMessageW(FRevertChangeButton, WM_SETFONT, WPARAM(FFont), 0);
      FFolderSelectButton := CreateWindowW('BUTTON',
        PWideChar('フォルダーの選択...'), WS_CHILD or WS_TABSTOP or
        WS_VISIBLE, 8 + 400 - 128, Y, 128, Height, Window, 4, Filter^.DLLHInst, nil);
      SendMessageW(FFolderSelectButton, WM_SETFONT, WPARAM(FFont), 0);
      Inc(Y, Height);

      Inc(Y, GetSystemMetrics(SM_CYCAPTION) + GetSystemMetrics(SM_CYFIXEDFRAME) * 2 + 8);
      SetWindowPos(Window, 0, 0, 0, 8 + 400 + 8 + GetSystemMetrics(SM_CXFIXEDFRAME) *
        2, Y, SWP_NOMOVE or SWP_NOZORDER);

      try
        if not Assigned(FExEdit) then
          raise Exception.Create(
            '拡張編集プラグインが見つかりません。');

        for Y := 0 to sinfo.FilterN - 1 do
        begin
          fp := Filter^.ExFunc^.GetFilterP(Y);
          if fp = nil then
            continue;
          if StrPos(fp^.Name, ExTextNameANSI) <> nil then
            raise Exception.Create(PluginName +
              ' は「字幕テキスト」プラグインと同時に使用することはできません。');
        end;

        if StrPos(FExEdit^.Information, ExEditVersion) = nil then
          raise Exception.Create(PluginName + ' を使うには拡張編集' +
            ExEditVersion + 'が必要です。');
        if sinfo.Build < 10000 then
          raise Exception.Create(PluginName +
            ' を使うには AviUtl version 1.00 以降が必要です。');
        if not LuaLoaded() then
          raise Exception.Create('lua51.dll の読み込みに失敗しました。');
        DragAcceptFiles(FExEdit^.Hwnd, False);
        OleCheck(RegisterDragDrop(FExEdit^.Hwnd, FDropTarget));
        SCDropper.InstallHook(FExEdit^.Hwnd);
      except
        on E: Exception do
        begin
          EnableWindow(Label1, False);
          EnableWindow(FSaveMode, False);
          EnableWindow(Label2, False);
          EnableWindow(FSavePathEdit, False);
          EnableWindow(FFolderSelectButton, False);
          EnableWindow(FRevertChangeButton, False);
          SetWindowTextW(Window,
            PWideChar(WideString(PluginName +
            ' - 初期化に失敗したため使用できません')));
          MessageBoxW(FExEdit^.Hwnd,
            PWideChar(PluginName +
            ' の初期化中にエラーが発生しました。'#13#10#13#10 +
            WideString(E.Message)),
            PWideChar('初期化エラー - ' + PluginName), MB_ICONERROR);
        end;
      end;

      Mode := 0;
      SaveDir := DefaultSaveDir;
      Result := 1;
    end;
    WM_FILTER_FILE_OPEN:
    begin
      Mode := 0;
      SaveDir := DefaultSaveDir;
    end;
    WM_FILTER_FILE_CLOSE:
    begin
      Mode := 0;
      SaveDir := DefaultSaveDir;
    end;
    WM_COMMAND:
    begin
      case LOWORD(WP) of
        4:
        begin
          try
            if LP <> LPARAM(FFolderSelectButton) then
              exit;
            S := SaveDir;
            hs := DisableFamilyWindows(Window);
            try
              if not SelectDir(Window, S) then
                exit;
              SetWindowTextW(FSavePathEdit, PWideChar(WideString(S)));
            finally
              EnableFamilyWindows(hs);
            end;
          except
            on E: Exception do
              MessageBoxW(FExEdit^.Hwnd, PWideChar(WideString(E.Message)),
                PWideChar(WideString('エラー - ' + PluginName)), MB_ICONERROR);
          end;
        end;
        5:
        begin
          try
            if LP <> LPARAM(FRevertChangeButton) then
              exit;
            SaveDir := DefaultSaveDir;
          except
            on E: Exception do
              MessageBoxW(FExEdit^.Hwnd, PWideChar(WideString(E.Message)),
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
      if Assigned(FExEdit) then
      begin
        RevokeDragDrop(FExEdit^.Hwnd);
        DragAcceptFiles(FExEdit^.Hwnd, True);
        SCDropper.UninstallHook();
      end;
    end
    else
    begin
      if Message = FGCMZDropsMessageId then
      begin
        PDDI := {%H-}PDragDropInfo(LP);
        FCurrentFilterP := Filter;
        FCurrentEditP := Edit;
        try
          case WP of
            0:
            begin
              FreeAndNil(FLua);
              FLua := TLua.Create();
              if not FLua.CallDragEnter(PDDI^.DraggingFiles, PDDI^.Point,
                PDDI^.KeyState) then
                raise EAbort.Create('canceled ondragenter');
              PDDI^.Effect := DROPEFFECT_COPY;
            end;
            1:
            begin
              if Assigned(FLua) then
              begin
                if not FLua.CallDragOver(PDDI^.DraggingFiles,
                  PDDI^.Point, PDDI^.KeyState) then
                  raise EAbort.Create('canceled ondragover');
                PDDI^.Effect := DROPEFFECT_COPY;
              end;
            end;
            2:
            begin
              if Assigned(FLua) then
              begin
                FLua.CallDragLeave();
                FreeAndNil(FLua);
              end;
              ProcessDeleteFileQueue(True);
            end;
            3:
            begin
              if Assigned(FLua) then
              begin
                if not FLua.CallDrop(PDDI^.DraggingFiles, PDDI^.Point,
                  PDDI^.KeyState) then
                  raise EAbort.Create('canceled ondrop');
                PDDI^.Effect := DROPEFFECT_COPY;
                FreeAndNil(FLua);
              end;
              ProcessDeleteFileQueue(False);
            end;
            100:
            begin
              if IsEditing() then
              begin
                FreeAndNil(FSCDropperLua);
                FSCDropperLua := TLua.Create();
                FSCDropperLua.InitDropper();
                SCDropper.RecreateMenu(FSCDropperLua.State);
                SCDropper.Popup(FSCDropperLua.State, FExEdit^.Hwnd, {%H-}PPoint(LP)^);
                FreeAndNil(FSCDropperLua);
              end;
            end;
          end;
        except
          on E: EAbort do
          begin
            ODS('処理が中断されました: %s', [WideString(E.Message)]);
            FreeAndNil(FLua);
            FreeAndNil(FSCDropperLua);
            if Assigned(PDDI) then
              PDDI^.Effect := DROPEFFECT_NONE;
            ProcessDeleteFileQueue(True);
          end;
          on E: Exception do
          begin
            ODS('error: %s', [WideString(E.Message)]);
            MessageBoxW(FExEdit^.Hwnd,
              PWideChar('ドラッグ＆ドロップの処理中にエラーが発生しました。'#13#10#13#10 + WideString(E.Message)),
              PluginName, MB_ICONERROR);
            FreeAndNil(FLua);
            FreeAndNil(FSCDropperLua);
            if Assigned(PDDI) then
              PDDI^.Effect := DROPEFFECT_NONE;
            ProcessDeleteFileQueue(True);
          end;
        end;
        Result := 0;
      end
      else
        Result := 0;
    end;
  end;
end;

function TGCMZDrops.ProjectLoadProc(Filter: PFilter; Edit: Pointer;
  Data: Pointer; Size: integer): boolean;
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
      SetCodePage(S, CP_UTF8, False);
      SaveDir := S;
    end;
    Result := True;
  finally
    SL.Free();
  end;
end;

function TGCMZDrops.ProjectSaveProc(Filter: PFilter; Edit: Pointer;
  Data: Pointer; var Size: integer): boolean;
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
    Result := True;
  finally
    MS.Free();
  end;
end;

procedure TGCMZDrops.TextConvert(const Text: UTF8String; out DF: TDraggingFile);
var
  TFS: TTempFileStream;
  SJIS: ShiftJISString;
begin
  TFS := TTempFileStream.Create('gcmztmp.txt');
  try
    SJIS := ShiftJISString(Text);
    TFS.WriteBuffer(SJIS[1], Length(SJIS));
    DF.Type_ := dftFile;
    DF.FilePathOrContent := TFS.FilePath;
    DF.DeleteOnFinish := True;
    DF.MediaType := 'text/plain; charset=Shift_JIS';
  finally
    TFS.Free;
  end;
end;

procedure TGCMZDrops.SetMode(AValue: integer);
begin
  SendMessageW(FSaveMode, CB_SETCURSEL, WPARAM(AValue), 0);
end;

procedure TGCMZDrops.SetSaveDir(AValue: UTF8String);
begin
  SetWindowTextW(FSavePathEdit, PWideChar(WideString(AValue)));
end;

procedure TGCMZDrops.ProcessDeleteFileQueue(const Error: boolean);
var
  I: integer;
begin
  if Error then
    for I := 0 to FDeleteOnAbortFileQueue.Count - 1 do
      AddUsedFile(FDeleteOnAbortFileQueue.Strings[I]);
  for I := 0 to FDeleteOnFinishFileQueue.Count - 1 do
    AddUsedFile(FDeleteOnFinishFileQueue.Strings[I]);
  FDeleteOnFinishFileQueue.Clear;
  FDeleteOnAbortFileQueue.Clear;
  CleanupUsedFile();
end;

function TGCMZDrops.InitProc(Filter: PFilter): boolean;
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
  I, N: integer;
  S: WideString;
begin
  Result := False;
  try
    SetLength(S, MAX_PATH);
    GetTempPathW(MAX_PATH, @S[1]);
    SetLength(FKnownFolders, Length(CSIDLs) + 1);
    FKnownFolders[0] := UTF8String(S);
    N := 1;
    for I := Low(CSIDLs) to High(CSIDLs) do
    begin
      try
        FKnownFolders[N] := GetKnownFolderPath(CSIDLs[I]);
        Inc(N);
      except
        on E: Exception do
        begin
          ODS('#%d error: %s', [I, WideString(E.Message)]);
        end;
      end;
    end;
    SetLength(FKnownFolders, N);
    Result := True;
  except
    on E: Exception do
    begin
      ODS('error: %s', [WideString(E.Message)]);
      MessageBoxW(0, PWideChar(
        '初期化中にエラーが発生しました。'#13#10#13#10 +
        WideString(E.Message)),
        PluginName, MB_ICONERROR);
    end;
  end;
end;

function TGCMZDrops.ExitProc(Filter: PFilter): boolean;
begin
  Result := True;
end;

procedure TGCMZDrops.OnDragEnter(Sender: TObject; const PDDI: PDragDropInfo);
begin
  SendMessage(FWindow, FGCMZDropsMessageId, 0, {%H-}LPARAM(PDDI));
end;

procedure TGCMZDrops.OnDragOver(Sender: TObject; const PDDI: PDragDropInfo);
begin
  SendMessage(FWindow, FGCMZDropsMessageId, 1, {%H-}LPARAM(PDDI));
end;

procedure TGCMZDrops.OnDragLeave(Sender: TObject);
begin
  SendMessage(FWindow, FGCMZDropsMessageId, 2, 0);
end;

procedure TGCMZDrops.OnDrop(Sender: TObject; const PDDI: PDragDropInfo);
begin
  SendMessage(FWindow, FGCMZDropsMessageId, 3, {%H-}LPARAM(PDDI));
end;

procedure TGCMZDrops.OnPopupSCDropperMenu(Sender: TObject; const Pt: TPoint);
begin
  SendMessage(FWindow, FGCMZDropsMessageId, 100, {%H-}LPARAM(@Pt));
end;

function TGCMZDrops.GetMode: integer;
begin
  Result := SendMessageW(FSaveMode, CB_GETCURSEL, 0, 0);
end;

function TGCMZDrops.GetEntry: PFilterDLL;
begin
  Result := @FEntry;
end;

function TGCMZDrops.GetExEditLayerHeight: integer;
begin
  Result := 31;
  if not Assigned(FExEdit) then
    Exit;
  case FExEdit^.ExFunc^.IniLoadInt(FExEdit, 'small_layer', 0) of
    0: Result := 31;
    1: Result := 26;
    2: Result := 22;
  end;
end;

function TGCMZDrops.GetExEditWindow: THandle;
begin
  if not Assigned(FExEdit) then
  begin
    Result := 0;
    Exit;
  end;
  Result := FExEdit^.Hwnd;
end;

function TGCMZDrops.GetSaveDir: UTF8String;
var
  WS: WideString;
begin
  SetLength(WS, 1024);
  GetWindowTextW(FSavePathEdit, @WS[1], Length(WS));
  Result := ExcludeTrailingPathDelimiter(UTF8String(PWideChar(WS)));
end;

constructor TGCMZDrops.Create;
begin
  inherited Create;
  FGCMZDropsMessageId := RegisterWindowMessage('GCMZDrops');
  FDropTarget := TDropTarget.Create();
  FDropTarget.OnDragEnter := @OnDragEnter;
  FDropTarget.OnDragOver := @OnDragOver;
  FDropTarget.OnDragLeave := @OnDragLeave;
  FDropTarget.OnDrop := @OnDrop;
  FDropTarget.TextConverter := @TextConvert;
  FDropTargetIntf := FDropTarget;

  FillChar(FEntry, SizeOf(FEntry), 0);
  FEntry.Flag := FILTER_FLAG_ALWAYS_ACTIVE or FILTER_FLAG_EX_INFORMATION;
  FEntry.Name := PluginNameANSI;
  FEntry.Information := PluginInfoANSI;

  SCDropper.OnNotify := @OnPopupSCDropperMenu;

  FDeleteOnFinishFileQueue := TStringList.Create;
  FDeleteOnAbortFileQueue := TStringList.Create;
end;

destructor TGCMZDrops.Destroy;
begin
  ProcessDeleteFileQueue(False);
  FDeleteOnFinishFileQueue.Free;
  FDeleteOnAbortFileQueue.Free;
  FDropTargetIntf := nil;
  FDropTarget := nil;
  inherited Destroy;
end;

function TGCMZDrops.NeedCopy(FilePath: UTF8String): boolean;
var
  I: integer;
begin
  case LowerCase(ExtractFileExt(FilePath)) of
    '.txt', '.exo', '.exa':
    begin
      Result := False;
      Exit;
    end;
  end;

  case Mode of
    0:
    begin // auto
      Result := False;
      for I := Low(FKnownFolders) to High(FKnownFolders) do
      begin
        if not Contains(FKnownFolders[I], FilePath) then
          continue;
        Result := True;
        Exit;
      end;
    end;
    1: Result := True; // copy
    2:
    begin // direct
      // Do not accept if the file is contained in a temp path.
      Result := Contains(FKnownFolders[0], FilePath);
    end;
  end;
end;

function TGCMZDrops.ExistsInGCMZDir(FilePath: UTF8String): boolean;
begin
  Result := Contains(GetSavePath(), FilePath);
end;

function TGCMZDrops.GetSavePath(): UTF8String;
var
  SJIS: ShiftJISString;
  SDir, ProjFile: UTF8String;
  SI: TSysInfo;
  FI: TFileInfo;
  hs: THandleDynArray;
begin
  SDir := SaveDir;
  if Pos('%PROJECTDIR%', SDir) = 0 then
  begin
    Result := SDir;
    Exit;
  end;

  GetAviUtlSysInfo(SI);
  GetExEditFileInfo(FI);
  if (SI.ProjectName = nil) or (SI.ProjectName = '') or (FI.FrameN = 0) then
    raise Exception.Create(
      'The project directory is unknown, please save the project first.'#13#10 +
      'プロジェクトファイルがまだ保存されていないため、処理を続行できません。');
  SJIS := SI.ProjectName;
  ProjFile := SJIS;
  Result := StringReplace(SDir, '%PROJECTDIR%',
    ExcludeTrailingPathDelimiter(ExtractFilePath(ProjFile)), [rfReplaceAll]);
  Result := ExcludeTrailingPathDelimiter(Result);

  if not DirectoryExists(WideString(Result)) then
  begin
    hs := DisableFamilyWindows(FExEdit^.Hwnd);
    try
      if MessageBoxW(FExEdit^.Hwnd,
        PWideChar('ファイルの保存先フォルダーが存在しません。作成しますか？'#13#10 + WideString(Result)), PluginName, MB_ICONQUESTION or MB_OKCANCEL) =
        idCancel then
        raise EAbort.Create('destination directory is not exists, operation canceled');
    finally
      EnableFamilyWindows(hs);
    end;
    if not ForceDirectories(WideString(Result)) then
      raise Exception.Create(UTF8String(
        'cannot create a directory.'#13#10'フォルダーの作成に失敗しました。'#13#10) + Result);
  end;
end;

procedure TGCMZDrops.RegisterDeleteOnFinish(S: UTF8String);
begin
  FDeleteOnFinishFileQueue.Add(S);
end;

procedure TGCMZDrops.RegisterDeleteOnAbort(S: UTF8String);
begin
  FDeleteOnAbortFileQueue.Add(S);
end;

procedure TGCMZDrops.GetAviUtlSysInfo(out SI: TSysInfo);
begin
  FillChar(SI, SizeOf(SI), 0);
  if FCurrentFilterP^.ExFunc^.GetSysInfo(FCurrentEditP, @SI) = AVIUTL_FALSE then
    raise Exception.Create('cannot get system information from AviUtl');
end;

procedure TGCMZDrops.GetExEditFileInfo(out FI: TFileInfo);
begin
  FillChar(FI, SizeOf(FI), 0);
  if FCurrentFilterP^.ExFunc^.GetFileInfo(FCurrentEditP, @FI) = AVIUTL_FALSE then
    raise Exception.Create('cannot get project information from AviUtl');
  if (FI.AudioRate = 0) or (FI.AudioCh = 0) then
    raise Exception.Create('A project must be editing to get a project file info.'#13#10 +
      'プロジェクトが編集中ではないため処理を続行できません。');
end;

function TGCMZDrops.IsEditing(): boolean;
var
  fi: TFileInfo;
begin
  FillChar(FI, SizeOf(FI), 0);
  Result := (FCurrentFilterP^.ExFunc^.GetFileInfo(FCurrentEditP, @FI) <>
    AVIUTL_FALSE) and (FI.AudioRate <> 0) and (FI.AudioCh <> 0);
end;

procedure TGCMZDrops.GetFileInfo(out FI: TFileInfo; out Samples: integer;
  const FileName: UTF8String);
var
  SJIS: ShiftJISString;
  h: Pointer;
  Proj: TFileInfo;
begin
  FillChar(Proj, SizeOf(Proj), 0);
  if FCurrentFilterP^.ExFunc^.GetFileInfo(FCurrentEditP, @Proj) = AVIUTL_FALSE then
    raise Exception.Create('cannot get project information from AviUtl');
  if (Proj.AudioRate = 0) or (Proj.AudioCh = 0) then
    raise Exception.Create('A project must be editing to get a media file info.'#13#10 +
      'プロジェクトが編集中ではないため処理を続行できません。');

  SJIS := ShiftJISString(FileName);
  FillChar(FI, SizeOf(FI), 0);
  h := FCurrentFilterP^.ExFunc^.AVIFileOpen(PChar(SJIS), @FI, 0);
  if not Assigned(h) then
    raise Exception.Create('cannot open media file: ' + FileName);
  Samples := FCurrentFilterP^.ExFunc^.AVIFileSetAudioSampleRate(h,
    Proj.AudioRate, Proj.AudioCh);
  FCurrentFilterP^.ExFunc^.AVIFileClose(h);
end;

function TGCMZDrops.Prompt(const Caption: UTF8String; var Value: UTF8String): boolean;
var
  hs: THandleDynArray;
begin
  hs := DisableFamilyWindows(FExEdit^.Hwnd);
  try
    Result := InputDialog.InputBox(FExEdit^.Hwnd, Caption, PluginName, Value);
  finally
    EnableFamilyWindows(hs);
  end;
end;

function TGCMZDrops.Confirm(const Caption: UTF8String): boolean;
var
  hs: THandleDynArray;
begin
  hs := DisableFamilyWindows(FExEdit^.Hwnd);
  try
    Result := MessageBoxW(FExEdit^.Hwnd, PWideChar(WideString(Caption)),
      PluginName, MB_ICONQUESTION or MB_OKCANCEL) = idOk;
  finally
    EnableFamilyWindows(hs);
  end;
end;

initialization
  GCMZDrops := TGCMZDrops.Create();
  GCMZDrops.Entry^.FuncInit := @FilterFuncInit;
  GCMZDrops.Entry^.FuncExit := @FilterFuncExit;
  GCMZDrops.Entry^.FuncWndProc := @FilterFuncWndProc;
  GCMZDrops.Entry^.FuncProjectLoad := @FilterFuncProjectLoad;
  GCMZDrops.Entry^.FuncProjectSave := @FilterFuncProjectSave;

  SetLength(FilterDLLList, 2);
  FilterDLLList[0] := GCMZDrops.Entry;
  FilterDLLList[1] := nil;

finalization
  GCMZDrops.Free();

end.
