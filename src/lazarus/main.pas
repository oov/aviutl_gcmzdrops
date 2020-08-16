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
    FEntry: TFilterDLL;
    FExEdit, FCurrentFilterP: PFilter;
    FCurrentEditP: Pointer;
    FDropTarget: TDropTarget;
    FDropTargetIntf: IDropTarget;
    FLua, FAPILua, FSCDropperLua: TLua;
    FWindow, FFont, FMutex, FFMO: THandle;
    FFontHeight: integer;
    FSavePathEdit, FSaveMode, FFolderSelectButton, FRevertChangeButton: THandle;
    FKnownFolders: array of UTF8String;
    FDeleteOnFinishFileQueue: TStringList;
    FDeleteOnAbortFileQueue: TStringList;
    function GetEntry: PFilterDLL;
    function GetExEditLayerHeight: integer;
    function GetExEditWindow: THandle;
    function GetMode: integer;
    procedure SetMode(AValue: integer);
    function GetSaveDir: UTF8String;
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

    procedure TextConvert(const Text: UTF8String; out DF: TFile);
    procedure OnDragEnter(Sender: TObject; const PDDI: PDragDropInfo);
    procedure OnDragOver(Sender: TObject; const PDDI: PDragDropInfo);
    procedure OnDragLeave(Sender: TObject);
    procedure OnDrop(Sender: TObject; const PDDI: PDragDropInfo);
    procedure OnPopupSCDropperMenu(Sender: TObject; const Pt: TPoint);

    function CmdUpdateMappedData(const ReadFileInfo: boolean): integer;
    function CmdAdvanceFrame(const N: integer): integer;

    procedure GetZoomLevelAndCursorPos(const ZoomLevel: PInteger; const
      LayerHeight: PInteger; const CursorPos: PPoint);
    function GetZoomLevel: integer;
    procedure SetZoomLevel(const Level: integer);
    function GetCursorPos(const OldZoom, LayerHeight: PInteger): TPoint;
    procedure GetScrollBars(const HScroll, VScroll: PHandle);
    function ProcessCopyData(const Window: THandle; CDS: PCopyDataStruct): LRESULT;
    procedure UpdateMappedData(const ReadFileInfo: boolean);
  public
    constructor Create();
    destructor Destroy(); override;
    property Entry: PFilterDLL read GetEntry;
    property SaveDir: UTF8String read GetSaveDir write SetSaveDir;
    property Mode: integer read GetMode write SetMode;
    property ExEditWindow: THandle read GetExEditWindow;
    property ExEditLayerHeight: integer read GetExEditLayerHeight;
    property ZoomLevel: integer read GetZoomLevel write SetZoomLevel;
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
    function GetClipboard(): TFiles;
    procedure AdvanceFrame(const N: integer);
  end;

function GetFilterTableList(): PPFilterDLL; stdcall;

var
  GCMZDrops: TGCMZDrops;

implementation

uses
  fpjson, jsonparser, InputDialog, ScriptableDropper, UsedFiles, Lua, Util, Ver;

const
  PluginName = 'ごちゃまぜドロップス';
  PluginNameANSI = #$82#$b2#$82#$bf#$82#$e1#$82#$dc#$82#$ba#$83#$68#$83#$8d#$83#$62#$83#$76#$83#$58;
  PluginInfoANSI = PluginNameANSI + ' ' + Version;
  ExEditNameANSI = #$8a#$67#$92#$a3#$95#$d2#$8f#$57; // '拡張編集'
  DefaultSaveDir = '%PROJECTDIR%';

const
  BoolConv: array[boolean] of AviUtlBool = (AVIUTL_FALSE, AVIUTL_TRUE);

const
  WM_GCMZDROP = WM_APP + 1;
  WM_GCMZCOMMAND = WM_APP + 2;

  ZoomActiveRed = 96;
  ZoomDeactiveRed = 32;
  ZoomLeft = 5;
  ZoomTop = 32;
  ZoomMax = 26;
  TimelineLeft = 64;
  TimeLineHeaderTop = 13;
  TimeLineTop = 48;
  LayerLeft = 0;
  LayerTop = 42;
  LayerMax = 31;

var
  FilterDLLList: array of PFilterDLL;

type
  TMappedData = record
    Window: HWND;
    Width, Height, VideoRate, VideoScale: integer;
    AudioRate, AudioCh: integer;
    GCMZAPIVer: integer;
    ProjectFilePath: array[0..MAX_PATH-1] of WideChar;
  end;
  PMappedData = ^TMappedData;

  TGCMZDragDropInfo = record
    DDI: PDragDropInfo;
    FrameAdvance: integer;
  end;
  PGCMZDragDropInfo = ^TGCMZDragDropInfo;

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

function ParseExEditVersion(S: string): integer;
  function NumDot(S: string): integer;
  var
    i: integer;
  begin
    for i := Low(S) to High(S) do
      case S[i] of
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.':;
        else begin
          Result := i - 1;
          Exit;
        end;
      end;
    Result := Length(S);
  end;
const
  ExEditVersionString = ' version ';
var
  I: integer;
  e: Extended;
begin
  Result := 0;
  I := Pos(ExEditVersionString, S);
  if I = 0 then
     Exit;
  Delete(S, 1, I + Length(ExEditVersionString) - 1);
  I := NumDot(S);
  if I = 0 then
     Exit;
  Delete(S, I + 1, Length(S));
  E := StrToFloatDef(S, 0);
  if E = 0 then
     Exit;
  Result := Trunc(E * 10000);
end;

function SetClientSize(Window: THandle; Width, Height: integer): BOOL;
var
  WR, CR: TRect;
begin
  Result := False;
  if not GetWindowRect(Window, WR) then Exit;
  if not GetClientRect(Window, CR) then Exit;
  Result := SetWindowPos(Window, 0, 0, 0, WR.Width - CR.Width + Width, WR.Height - CR.Height + Height, SWP_NOMOVE or SWP_NOZORDER);
end;

{ TGCMZDrops }

function TGCMZDrops.MainProc(Window: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM; Edit: Pointer; Filter: PFilter): integer;
const
  ExTextNameANSI = #$8e#$9a#$96#$8b#$83#$41#$83#$56#$83#$58#$83#$67;
  // '字幕アシスト'
  AulsTransparenceNameANSI = #$8a#$67#$92#$a3#$95#$d2#$8f#$57#$82#$f0#$94#$bc#$93#$a7#$96#$be#$89#$bb;
  // '拡張編集を半透明化'
var
  Label1, Label2: THandle;
  Y, Height: integer;
  NCM: TNonClientMetrics;
  DC: THandle;
  hs: THandleDynArray;
  S: UTF8String;
  sinfo: TSysInfo;
  fp, AulsTransparence: PFilter;
  PDDI: PGCMZDragDropInfo;
begin
  FCurrentFilterP := Filter;
  FCurrentEditP := Edit;
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
      SetClientSize(Window, 8 + 400 + 8, Y + 8);

      try
        if not Assigned(FExEdit) then
          raise Exception.Create(
            '拡張編集プラグインが見つかりません。');

        AulsTransparence := nil;
        for Y := 0 to sinfo.FilterN - 1 do
        begin
          fp := Filter^.ExFunc^.GetFilterP(Y);
          if fp = nil then
            continue;
          if StrPos(fp^.Name, ExTextNameANSI) <> nil then
            raise Exception.Create(PluginName +
              ' は「字幕テキスト」プラグインと同時に使用することはできません。');
          if (StrPos(fp^.Name, PluginNameANSI) <> nil)and(ExtractFileName(GetOtherDLLName(fp^.DLLHInst)) = 'oledd.auf') then
          raise Exception.Create('oledd.auf が存在するため初期化に失敗しました。'#13#10'これは' + PluginName + ' の古いバージョンであり、共存はできません。');
          if StrPos(fp^.Name, AulsTransparenceNameANSI) <> nil then
            AulsTransparence := fp;
        end;

        Y := ParseExEditVersion(FExEdit^.Information);
        if Y = 0 then
           raise Exception.Create('拡張編集のバージョンナンバー解析に失敗しました。');
        if Y < 9200 then
           raise Exception.Create(PluginName + ' を使うには拡張編集 version 0.92 以降が必要です。');
        if sinfo.Build < 10000 then
          raise Exception.Create(PluginName +
            ' を使うには AviUtl version 1.00 以降が必要です。');
        if not LuaLoaded() then
          raise Exception.Create('lua51.dll の読み込みに失敗しました。');
        DragAcceptFiles(FExEdit^.Hwnd, False);
        OleCheck(RegisterDragDrop(FExEdit^.Hwnd, FDropTargetIntf));
        SCDropper.InstallHook(FExEdit^.Hwnd);
        UpdateMappedData(False);

        // Workaround for the window capture problem when disabled desktop composition on Vista/7.
        SetWindowLong(FExEdit^.Hwnd, GWL_EXSTYLE, GetWindowLong(FExEdit^.Hwnd, GWL_EXSTYLE) or WS_EX_LAYERED);
        if AulsTransparence = nil then
          SetLayeredWindowAttributes(FExEdit^.Hwnd, 0, 255, LWA_ALPHA);
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
      UpdateMappedData(True);
      Mode := 0;
      SaveDir := DefaultSaveDir;
    end;
    WM_FILTER_FILE_CLOSE:
    begin
      UpdateMappedData(False);
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
    WM_COPYDATA:  Result := ProcessCopyData(THandle(WP), {%H-}PCopyDataStruct(LP));
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
    end;
    WM_GCMZDROP:
    begin
      PDDI := {%H-}PGCMZDragDropInfo(LP);
      hs := DisableFamilyWindows(0);
      try
        try
          case WP of
            0:
            begin
              FreeAndNil(FLua);
              FLua := TLua.Create();
              if not FLua.CallDragEnter(PDDI^.DDI^.Files, PDDI^.DDI^.Point,
                PDDI^.DDI^.KeyState) then
                raise EAbort.Create('canceled ondragenter');
              PDDI^.DDI^.Effect := DROPEFFECT_COPY;
            end;
            1:
            begin
              if Assigned(FLua) then
              begin
                if not FLua.CallDragOver(PDDI^.DDI^.Files,
                  PDDI^.DDI^.Point, PDDI^.DDI^.KeyState) then
                  raise EAbort.Create('canceled ondragover');
                PDDI^.DDI^.Effect := DROPEFFECT_COPY;
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
                if not FLua.CallDrop(PDDI^.DDI^.Files, PDDI^.DDI^.Point,
                  PDDI^.DDI^.KeyState, PDDI^.FrameAdvance) then
                  raise EAbort.Create('canceled ondrop');
                PDDI^.DDI^.Effect := DROPEFFECT_COPY;
                FreeAndNil(FLua);
              end;
              ProcessDeleteFileQueue(False);
            end;
            10:
            begin
              FAPILua := TLua.Create();
              if not FAPILua.CallDropSimulated(PDDI^.DDI^.Files, PDDI^.DDI^.Point, PDDI^.DDI^.KeyState, PDDI^.FrameAdvance) then
                raise EAbort.Create('canceled ondropsimulated');
              FreeAndNil(FAPILua);
            end;
            100:
            begin
              if IsEditing() then
              begin
                FSCDropperLua := TLua.Create();
                FSCDropperLua.InitDropper();
                SCDropper.RecreateMenu(FSCDropperLua.State);
                SCDropper.Popup(FSCDropperLua.State, FExEdit^.Hwnd, {%H-}PPoint(LP)^);
                FreeAndNil(FSCDropperLua);
                ProcessDeleteFileQueue(False);
              end;
            end;
          end;
        except
          on E: EAbort do
          begin
            ODS('処理が中断されました: %s', [WideString(E.Message)]);
            FreeAndNil(FLua);
            FreeAndNil(FAPILua);
            FreeAndNil(FSCDropperLua);
            if (WP <> 100) and Assigned(PDDI) then
              PDDI^.DDI^.Effect := DROPEFFECT_NONE;
            ProcessDeleteFileQueue(True);
          end;
          on E: Exception do
          begin
            ODS('error: %s', [WideString(E.Message)]);
            MessageBoxW(FExEdit^.Hwnd,
              PWideChar('ドラッグ＆ドロップの処理中にエラーが発生しました。'#13#10#13#10 + WideString(E.Message)),
              PluginName, MB_ICONERROR);
            FreeAndNil(FLua);
            FreeAndNil(FAPILua);
            FreeAndNil(FSCDropperLua);
            if (WP <> 100) and Assigned(PDDI) then
              PDDI^.DDI^.Effect := DROPEFFECT_NONE;
            ProcessDeleteFileQueue(True);
          end;
        end;
      finally
        EnableFamilyWindows(hs);
      end;
      Result := 0;
    end;
    WM_GCMZCOMMAND: begin
      case WP of
        0: Result := CmdAdvanceFrame(integer(LP));
        1: Result := CmdUpdateMappedData(LP <> 0);
      end;
    end;
    else
      Result := 0;
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
  UpdateMappedData(True);
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

procedure TGCMZDrops.TextConvert(const Text: UTF8String; out DF: TFile);
var
  TFS: TTempFileStream;
  SJIS: ShiftJISString;
begin
  TFS := TTempFileStream.Create('gcmztmp.txt');
  try
    SJIS := ShiftJISString(Text);
    TFS.WriteBuffer(SJIS[1], Length(SJIS));
    DF.Type_ := ftFile;
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
var
  GDDI: TGCMZDragDropInfo;
begin
  GDDI.DDI := PDDI;
  GDDI.FrameAdvance := 0;
  SendMessage(FWindow, WM_GCMZDROP, 0, {%H-}LPARAM(@GDDI));
end;

procedure TGCMZDrops.OnDragOver(Sender: TObject; const PDDI: PDragDropInfo);
var
  GDDI: TGCMZDragDropInfo;
begin
  GDDI.DDI := PDDI;
  GDDI.FrameAdvance := 0;
  SendMessage(FWindow, WM_GCMZDROP, 1, {%H-}LPARAM(@GDDI));
end;

procedure TGCMZDrops.OnDragLeave(Sender: TObject);
begin
  SendMessage(FWindow, WM_GCMZDROP, 2, 0);
end;

procedure TGCMZDrops.OnDrop(Sender: TObject; const PDDI: PDragDropInfo);
var
  GDDI: TGCMZDragDropInfo;
begin
  GDDI.DDI := PDDI;
  GDDI.FrameAdvance := 0;
  SendMessage(FWindow, WM_GCMZDROP, 3, {%H-}LPARAM(@GDDI));
end;

procedure TGCMZDrops.OnPopupSCDropperMenu(Sender: TObject; const Pt: TPoint);
begin
  SendMessage(FWindow, WM_GCMZDROP, 100, {%H-}LPARAM(@Pt));
end;

function TGCMZDrops.CmdUpdateMappedData(const ReadFileInfo: boolean): integer;
var
  MD: TMappedData;
  P: PMappedData;
  SI: TSysInfo;
  FI: TFileInfo;
  WS: WideString;
begin
  Result := 0;
  if FMutex = 0 then
    Exit;
  if FFMO = 0 then
    Exit;

  FillChar(MD, SizeOf(TMappedData), 0);
  MD.Window := FWindow;
  MD.GCMZAPIVer := 1;
  if ReadFileInfo then begin
    FillChar(FI, SizeOf(FI), 0);
    if FCurrentFilterP^.ExFunc^.GetFileInfo(FCurrentEditP, @FI) = AVIUTL_FALSE then
      Exit;
    if (FI.AudioRate = 0) or (FI.AudioCh = 0) then
      Exit;
    MD.Width := FI.Width;
    MD.Height := FI.Height;
    MD.VideoRate := FI.VideoRate;
    MD.VideoScale := FI.VideoScale;
    MD.AudioRate := FI.AudioRate;
    MD.AudioCh := FI.AudioCh;
    FillChar(SI, SizeOf(SI), 0);
    GetAviUtlSysInfo(SI);
    WS := WideString(ShiftJISString(SI.ProjectName));
    if Length(WS) > 0 then
      Move(WS[1], MD.ProjectFilePath[0], Min(Length(WS), MAX_PATH-1)*2);
  end;

  P := MapViewOfFile(FFMO, FILE_MAP_WRITE, 0, 0, 0);
  if P = nil then
    ODS('MapViewOfFile failed', []);
  try
    WaitForSingleObject(FMutex, INFINITE);
    try
      Move(MD, P^, SizeOf(MD));
    finally
      ReleaseMutex(FMutex);
    end;
  finally
    if not UnmapViewOfFile(P) then
      ODS('UnmapViewOfFile failed', []);
  end;
end;

function TGCMZDrops.CmdAdvanceFrame(const N: integer): integer;
var
  F, Len, S, E: integer;
begin
  F := FCurrentFilterP^.ExFunc^.GetFrame(FCurrentEditP);
  Len := FCurrentFilterP^.ExFunc^.GetFrameN(FCurrentEditP);
  Inc(F, N);
  if F >= Len then begin
    FCurrentFilterP^.ExFunc^.SetFrameN(FCurrentEditP, F + 1);
    if FCurrentFilterP^.ExFunc^.GetSelectFrame(FCurrentEditP, S, E) = AVIUTL_TRUE then begin
      if E = Len - 1 then
        FCurrentFilterP^.ExFunc^.SetSelectFrame(FCurrentEditP, S, F);
    end;
  end;
  FCurrentFilterP^.ExFunc^.SetFrame(FCurrentEditP, F);
  Result := 1;
end;

procedure SaveDIB(const FileName: String; const BI: TBitmapInfo; const P: PByte);
var
  BFH: TBitmapFileHeader;
  fs: TFileStream;
begin
  BFH.bfType := $4D42;
  BFH.bfSize := 0;
  BFH.bfReserved1 := 0;
  BFH.bfReserved2 := 0;
  BFH.bfOffBits := SizeOf(TBitmapFileHeader) + SizeOf(TBitmapInfoHeader);
  fs := TFileStream.Create(FileName, fmCreate);
  try
    fs.Write(BFH, SizeOf(TBitmapFileHeader));
    fs.Write(BI.bmiHeader, SizeOf(TBitmapInfoHeader));
    fs.Write(P^, ((BI.bmiHeader.biWidth * 3 + 3) and (not 3)) * 72);
  finally
    fs.Free;
  end;
end;

const PW_CLIENTONLY = 1;
function PrintWindow(hwnd: HWND; hdcBlt: HDC; nFlags: UINT): BOOL; stdcall; external user32 name 'PrintWindow';

procedure TGCMZDrops.GetZoomLevelAndCursorPos(const ZoomLevel: PInteger; const LayerHeight: PInteger; const CursorPos: PPoint);
var
  LineBytes, I, J: integer;
  hExEditDC, hDesktop, hDesktopDC, hDC, hBitmap, hOldBitmap: THandle;
  Rect: TRect;
  BI: TBitmapInfo;
  P, PP: PByte;
  TLR, TLG, TLB: Byte;
begin
  if not IsWindowVisible(FExEdit^.Hwnd) then
    raise Exception.Create('ExEdit Window is currently invisible');
  if not GetClientRect(FExEdit^.Hwnd, Rect) then
    raise Exception.Create('GetWindowRect failed');
  hExEditDC := GetDC(FExEdit^.Hwnd);
  if hExEditDC = 0 then
    raise Exception.Create('GetDC failed');
  try
    hDesktop := GetDesktopWindow();
    hDesktopDC := GetDC(hDesktop);
    if hDesktopDC = 0 then
      raise Exception.Create('GetDC failed');
    try
      hDC := CreateCompatibleDC(hDesktopDC);
      if hDC = 0 then
        raise Exception.Create('CreateCompatibleDC failed');
      try
        FillChar(BI, SizeOf(TBitmapInfo), 0);
        BI.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
        BI.bmiHeader.biWidth := Rect.Width;
        BI.bmiHeader.biHeight := -72;
        BI.bmiHeader.biPlanes := 1;
        BI.bmiHeader.biBitCount := 24;
        BI.bmiHeader.biCompression := BI_RGB;
        hBitmap := CreateDIBSection(hDC, BI, DIB_RGB_COLORS, P, 0, 0);
        if hBitmap = 0 then
          raise Exception.Create('CreateDIBSection failed');
        try
          hOldBitmap := SelectObject(hDC, hBitmap);
          if hOldBitmap = 0 then
            raise Exception.Create('SelectObject failed');
          try
            if not PrintWindow(FExEdit^.Hwnd, hDC, PW_CLIENTONLY) then
              raise Exception.Create('PrintWindow failed');
            try
            LineBytes := (Rect.Width * 3 + 3) and (not 3);
            if ZoomLevel <> nil then begin
              J := 0;
              PP := P + ZoomTop * LineBytes + ZoomLeft * 3;
              for I := 0 to ZoomMax - 1 do begin
                if (PP + 2)^ = ZoomActiveRed then
                  Inc(J)
                else if (PP + 2)^ = ZoomDeactiveRed then
                  break
                else
                  raise Exception.Create('failed to detect timeline zoom level');
                Inc(PP, 2 * 3);
              end;
              ZoomLevel^ := J;
            end;

            if LayerHeight <> nil then begin
              J := 1;
              PP := P + LayerTop * LineBytes + LayerLeft * 3;
              TLR := (PP + 2)^;
              TLG := (PP + 1)^;
              TLB := (PP + 0)^;
              for I := 0 to LayerMax - 1 do begin
                if ((PP + 0)^ <> TLB)or((PP + 1)^ <> TLG)or((PP + 2)^ <> TLR) then
                  break;
                Inc(J);
                Inc(PP, LineBytes);
              end;
              if (J <> 31)and(J <> 26)and(J <> 22) then
                raise Exception.Create('failed to detect layer height');
              LayerHeight^ := J;
            end;

            if CursorPos <> nil then begin
              CursorPos^ := Point(-1, -1);
              PP := P + TimeLineHeaderTop * LineBytes + TimelineLeft * 3;
              TLR := (PP - 1)^;
              TLG := 255 - (PP - 2)^;
              TLB := 255 - (PP - 3)^;
              // If the background color is not same to the end line color(#a0a0a0)...
              if (TLR <> 160)or(TLG <> 255 - 160)or(TLB <> 255 - 160) then
                for I := 0 to Rect.Width - TimelineLeft - 1 do begin
                  if (((PP + 0)^ = TLB)and((PP + 1)^ = TLG)and((PP + 2)^ = TLR))
                    or(((PP + 0)^ = 255 - 160)and((PP + 1)^ = 255 - 160)and((PP + 2)^ = 160)) then begin
                    CursorPos^ := Point(TimelineLeft + I, TimeLineTop);
                    break;
                  end;
                  Inc(PP, 3);
                end
              else
                for I := 0 to Rect.Width - TimelineLeft - 1 do begin
                  if ((PP + 0)^ = TLB)and((PP + 1)^ = TLG)and((PP + 2)^ = TLR) then begin
                    CursorPos^ := Point(TimelineLeft + I, TimeLineTop);
                    break;
                  end;
                  Inc(PP, 3);
                end;
            end;
            except
              SaveDIB(FormatDateTime('"gcmz-apierr-"yyyymmdd-hhnnss".bmp"', Now()), BI, P);
              raise;
            end;
          finally
            SelectObject(hDC, hOldBitmap);
          end;
        finally
          DeleteObject(hBitmap);
        end;
      finally
        DeleteDC(hDC);
      end;
    finally
      ReleaseDC(hDesktop, hDesktopDC);
    end;
  finally
    ReleaseDC(FExEdit^.Hwnd, hExEditDC);
  end;
end;

function TGCMZDrops.GetZoomLevel: integer;
begin
  GetZoomLevelAndCursorPos(@Result, nil, nil);
end;

procedure TGCMZDrops.SetZoomLevel(const Level: integer);
var
  lp: LPARAM;
begin
  lp := MAKELONG(ZoomLeft + Level * 2 - 2, ZoomTop);
  SendMessage(FExEdit^.Hwnd, WM_LBUTTONDOWN, MK_LBUTTON, lp);
  SendMessage(FExEdit^.Hwnd, WM_LBUTTONUP, MK_LBUTTON, lp);
end;

function TGCMZDrops.GetCursorPos(const OldZoom, LayerHeight: PInteger): TPoint;
const
  MinZoomLevel = 19;
var
  Z: integer;
  HScrollBar: THandle;
  SI: TScrollInfo;
begin
  GetZoomLevelAndCursorPos(@Z, LayerHeight, @Result);
  if Z < MinZoomLevel then begin
    SetZoomLevel(MinZoomLevel);
    GetZoomLevelAndCursorPos(nil, nil, @Result);
  end;
  if (Result.x = -1)and(Result.y = -1) then begin
    GetScrollBars(@HScrollBar, nil);
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_PAGE;
    if not GetScrollInfo(HScrollBar, SB_CTL, SI) then
      raise Exception.Create('GetScrollInfo failed');

    SI.fMask := SIF_POS;
    SI.nPos := FCurrentFilterP^.ExFunc^.GetFrame(FCurrentEditP) - SI.nPage div 2;
    SI.nPos := SetScrollInfo(HScrollBar, SB_CTL, SI, True);
    SendMessage(FExEdit^.Hwnd, WM_HSCROLL, MAKELONG(SB_THUMBTRACK, SI.nPos), HScrollBar);
    SendMessage(FExEdit^.Hwnd, WM_HSCROLL, MAKELONG(SB_THUMBPOSITION, SI.nPos), HScrollBar);
    GetZoomLevelAndCursorPos(nil, nil, @Result);
  end;
  if (Result.x <> -1)and(Result.y <> -1) then begin
    if not ClientToScreen(FExEdit^.Hwnd, Result) then
      raise Exception.Create('ClientToScreen failed');
  end;
  if OldZoom <> nil then
    OldZoom^ := Z;
end;

procedure TGCMZDrops.GetScrollBars(const HScroll, VScroll: PHandle);
const
  ID_SCROLLBAR = 1004;
var
  h, hV, hH: THandle;
begin
  hV := 0;
  hH := 0;
  h := 0;
  while (hV = 0)or(hH = 0) do
  begin
    h := FindWindowExW(FExEdit^.Hwnd, h, 'ScrollBar', nil);
    if h = 0 then
      Exit;
    if GetWindowLong(h, GWL_ID) <> ID_SCROLLBAR then
      continue;
    if GetWindowLong(h, GWL_STYLE) and SBS_VERT <> SBS_VERT then
      hH := h
    else
      hV := h;
  end;
  if HScroll <> nil then
    HScroll^ := hH;
  if VScroll <> nil then
    VScroll^ := hV;
end;

function TGCMZDrops.ProcessCopyData(const Window: THandle; CDS: PCopyDataStruct
  ): LRESULT;
  procedure Process(const Layer, FrameAdv: integer; const Files: array of UTF8String);
  var
    VScrollBar: THandle;
    I, LayerHeight, OldZoom: integer;
    SI: TScrollInfo;
    DDI: TDragDropInfo;
    GDDI: TGCMZDragDropInfo;
  begin
    DDI.KeyState := 0;
    DDI.Point := GetCursorPos(@OldZoom, @LayerHeight);
    if (DDI.Point.x = -1)and(DDI.Point.y = -1) then
      raise Exception.Create('現在のカーソル位置の検出に失敗しました。');
    SetLength(DDI.Files, 0);

    GetScrollBars(nil, @VScrollBar);
    SI.cbSize := SizeOf(TScrollInfo);
    SI.fMask := SIF_POS;
    if not GetScrollInfo(VScrollBar, SB_CTL, SI) then
      raise Exception.Create('GetScrollInfo failed');
    case Layer of
      -100..-1: begin // relative
        I := Layer * - 1 - 1;
        Inc(DDI.Point.y, I*LayerHeight);
      end;
      1..100: begin // absolute
        I := Layer - 1;
        if SI.nPos > I then begin
          SI.nPos := I;
          SI.nPos := SetScrollInfo(VScrollBar, SB_CTL, SI, True);
          SendMessage(FExEdit^.Hwnd, WM_VSCROLL, MAKELONG(SB_THUMBTRACK, SI.nPos), VScrollBar);
          SendMessage(FExEdit^.Hwnd, WM_VSCROLL, MAKELONG(SB_THUMBPOSITION, SI.nPos), VScrollBar);
        end;
        Inc(DDI.Point.y, (I - SI.nPos)*LayerHeight);
      end;
    end;

    GDDI.FrameAdvance := FrameAdv;
    GDDI.DDI := @DDI;
    SetLength(DDI.Files, Length(Files));
    for I := Low(Files) to High(Files) do begin
      DDI.Files[I].DeleteOnFinish := False;
      DDI.Files[I].Type_ := ftFile;
      DDI.Files[I].MediaType := '';
      DDI.Files[I].FilePathOrContent := Files[I];
    end;
    SendMessage(FWindow, WM_GCMZDROP, 10, {%H-}LPARAM(@GDDI));
    SetZoomLevel(OldZoom);
    Result := 1;
  end;
  function API0(): LRESULT;
  var
    Layer, FrameAdv: integer;
    Files: array of UTF8String;
    WS, S: WideString;
    I: integer;
  begin
    SetLength(WS, CDS^.cbData div 2);
    Move(CDS^.lpData^, WS[1], CDS^.cbData);
    Layer := StrToIntDef(string(Token(#0, WS)), 0);
    FrameAdv := StrToIntDef(string(Token(#0, WS)), 0);
    I := 0;
    SetLength(Files, 0);
    while WS <> '' do begin
      S := Token(#0, WS);
      if S = '' then
        continue;
      SetLength(Files, I + 1);
      Files[I] := UTF8String(S);
      Inc(I);
    end;
    Process(Layer, FrameAdv, Files);
    Result := 1;
  end;
  function API1(): LRESULT;
  var
    S: UTF8String;
    JD, E: TJSONData;
    Layer, FrameAdv, I: integer;
    Files: array of UTF8String;
  begin
    Result := 0;
    SetLength(S, CDS^.cbData);
    Move(CDS^.lpData^, S[1], CDS^.cbData);
    JD := GetJSON(S);
    try
      E := JD.FindPath('layer');
      if not Assigned(E) then
        raise Exception.Create('layer が指定されていません。');
      Layer := E.AsInteger;

      E := JD.FindPath('frameAdvance');
      if Assigned(E) then
        FrameAdv := E.AsInteger;

      E := JD.FindPath('files');
      if not Assigned(E) then
        raise Exception.Create('files が指定されていません。');
      SetLength(Files, E.Count);
      for I := 0 to E.Count-1 do begin
        Files[I] := E.Items[I].AsString;
      end;
      Process(Layer, FrameAdv, Files);
      Result := 1;
    finally
      JD.Free;
    end;
  end;
begin
  try
    case CDS^.dwData of
      0: Result := API0();
      1: Result := API1();
      else raise Exception.Create('COPYDATASTRUCT 構造体の dwData に不正な値が渡されました。');
    end;
  except
    on E: Exception do begin
      ODS('error: %s', [WideString(E.Message)]);
      MessageBoxW(FExEdit^.Hwnd,
        PWideChar('外部 API 経由での処理中にエラーが発生しました。'#13#10#13#10 + WideString(E.Message)),
        PluginName, MB_ICONERROR);
      ProcessDeleteFileQueue(True);
    end;
  end;
end;

procedure TGCMZDrops.UpdateMappedData(const ReadFileInfo: boolean);
var
  LP: LPARAM;
begin
  if ReadFileInfo then
    LP := 1
  else
    LP := 0;
  PostMessage(FWindow, WM_GCMZCOMMAND, 1, LP);
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
  GetZoomLevelAndCursorPos(nil, @Result, nil);
  if (Result = 31)or(Result = 26)or(Result = 22) then
    Exit;

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

constructor TGCMZDrops.Create();
begin
  inherited Create;
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

  FMutex := CreateMutex(nil, FALSE, 'GCMZDropsMutex');
  if FMutex = 0 then begin
    ODS('failed to create mutex "GCMZDropsMutex".', []);
  end;
  FFMO := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE, 0, SizeOf(TMappedData), 'GCMZDrops');
  if (FFMO <> 0)and(GetLastError() = ERROR_ALREADY_EXISTS) then begin
    ODS('FileMappingObject "GCMZDrops" already exists.', []);
    CloseHandle(FFMO);
    FFMO := 0;
    // If the old version has been started, only Mutex creation will succeed.
    // This situation causes the API version to be misidentified, so Mutex must be closed.
    if FMutex <> 0 then begin
      CloseHandle(FMutex);
      FMutex := 0;
    end;
  end;
end;

destructor TGCMZDrops.Destroy();
begin
  if FMutex <> 0 then
    CloseHandle(FMutex);
  if FFMO <> 0 then
    CloseHandle(FFMO);
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
    if MessageBoxW(FExEdit^.Hwnd,
      PWideChar('ファイルの保存先フォルダーが存在しません。作成しますか？'#13#10 + WideString(Result)), PluginName, MB_ICONQUESTION or MB_OKCANCEL) =
      idCancel then
      raise EAbort.Create('destination directory is not exists, operation canceled');
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
begin
  Result := InputDialog.InputBox(FExEdit^.Hwnd, Caption, PluginName, Value);
end;

function TGCMZDrops.Confirm(const Caption: UTF8String): boolean;
begin
  Result := MessageBoxW(FExEdit^.Hwnd, PWideChar(WideString(Caption)),
    PluginName, MB_ICONQUESTION or MB_OKCANCEL) = idOk;
end;

function TGCMZDrops.GetClipboard(): TFiles;
var
  CB: TClipboard;
begin
  CB := TClipboard.Create();
  try
    CB.TextConverter := @TextConvert;
    Result := CB.Get();
  finally
    CB.Free;
  end;
end;

procedure TGCMZDrops.AdvanceFrame(const N: integer);
begin
  SendMessage(FWindow, WM_GCMZCOMMAND, 0, LPARAM(N));
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
