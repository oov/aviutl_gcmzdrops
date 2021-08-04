unit API;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, Classes;

const
  HWND_MESSAGE = HWND(-3);

type
  TAPICallParams = record
    Window: THandle;
    Layer: integer;
    FrameAdv: integer;
    Files: array of UTF8String;
  end;
  PAPICallParams = ^TAPICallParams;

  TOnCall = procedure (const Data: PAPICallParams) of object;

  { TGCMZAPIThread }

  TGCMZAPIThread = class(TThread)
  private
    FWindow: THandle;
    FEvent, FProcessed, FReady: THandle;
    FOnCall: TOnCall;
    procedure Parse(const Data: PAPICallParams; const CDS: PCopyDataStruct);
    function WindowProc(Window: THandle; Msg: UINT; WP: WPARAM; LP: LPARAM): LRESULT;
  protected
    procedure Execute; override;
  public
    procedure Processed();
    procedure Ready();
    procedure SetBusy();
    constructor Create();
    destructor Destroy(); override;
    property Window: THandle read FWindow;
    property OnCall: TOnCall read FOnCall write FOnCall;
  end;

implementation

uses
  fpjson, jsonparser, SysUtils, Util;

var
  APIThread: TGCMZAPIThread;

function gWindowProc(Window: THandle; Msg: UINT; WP: WPARAM;
  LP: LPARAM): LRESULT; stdcall;
begin
  Result := APIThread.WindowProc(Window, Msg, WP, LP);
end;

{ TGCMZAPIThread }

procedure TGCMZAPIThread.Parse(const Data: PAPICallParams; const CDS: PCopyDataStruct);
  procedure API0();
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
    Data^.Layer := Layer;
    Data^.FrameAdv := FrameAdv;
    Data^.Files := Files;
  end;
  procedure API1();
  var
    S: UTF8String;
    JD, E: TJSONData;
    Layer, FrameAdv, I: integer;
    Files: array of UTF8String;
  begin
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
        FrameAdv := E.AsInteger
      else
        FrameAdv := 0;

      E := JD.FindPath('files');
      if not Assigned(E) then
        raise Exception.Create('files が指定されていません。');
      SetLength(Files, E.Count);
      for I := 0 to E.Count-1 do begin
        Files[I] := E.Items[I].AsString;
      end;
      Data^.Layer := Layer;
      Data^.FrameAdv := FrameAdv;
      Data^.Files := Files;
    finally
      JD.Free;
    end;
  end;
begin
  try
    case CDS^.dwData of
      0: API0();
      1: API1();
      else raise Exception.Create('COPYDATASTRUCT 構造体の dwData に不正な値が渡されました。');
    end;
  except
    on E: Exception do ODS('External API Error: %s', [WideString(E.Message)]);
  end;
end;

function TGCMZAPIThread.WindowProc(Window: THandle; Msg: UINT;
  WP: WPARAM; LP: LPARAM): LRESULT;
var
  Data: TAPICallParams;
begin
  case Msg of
    WM_COPYDATA:
    begin
      WaitForSingleObject(FReady, INFINITE);
      if FOnCall <> nil then
      begin
        Data.Window := THandle(WP);
        Parse(@Data, {%H-}PCopyDataStruct(LP));
        FOnCall(@Data);
        WaitForSingleObject(FProcessed, INFINITE);
      end;
      Result := 1;
    end;
    WM_CLOSE:
    begin
      DestroyWindow(Window);
    end;
    WM_DESTROY:
    begin
      PostQuitMessage(0);
    end
    else
    begin
      Result := DefWindowProc(Window, Msg, WP, LP);
      Exit;
    end;
  end;
  Result := 0;
end;

procedure TGCMZAPIThread.Execute;
const
  WindowClassName = 'GCMZDropsAPI';
var
  msg: TMsg;
  wc: TWNDClassExW;
begin
  FillChar(wc, sizeof(TWNDClassExW), 0);
  wc.cbSize := sizeof(TWNDClassExW);
  wc.lpfnWndProc := @gWindowProc;
  wc.hInstance := hInstance;
  wc.lpszClassName := WindowClassName;
  if RegisterClassExW(wc) = 0 then
  begin
    ODS('failed to register message window class', []);
    SetEvent(FEvent);
    Exit;
  end;

  FWindow := CreateWindowEx(0, WindowClassName, nil, WS_OVERLAPPEDWINDOW,
    CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, HWND_MESSAGE, 0,
    hInstance, nil);
  if FWindow = 0 then
  begin
    ODS('failed to create api message window', []);
    SetEvent(FEvent);
    Exit;
  end;
  SetEvent(FEvent);

  while integer(GetMessage(msg, 0, 0, 0)) > 0 do
  begin
    TranslateMessage(msg);
    DispatchMessage(msg);
  end;
  SetEvent(FEvent);
end;

procedure TGCMZAPIThread.Processed();
begin
  SetEvent(FProcessed);
end;

procedure TGCMZAPIThread.Ready();
begin
  SetEvent(FReady);
end;

procedure TGCMZAPIThread.SetBusy();
begin
  ResetEvent(FReady);
end;

constructor TGCMZAPIThread.Create();
begin
  APIThread := Self;

  FEvent := CreateEvent(nil, False, False, nil);
  FReady := CreateEvent(nil, False, False, nil);
  FProcessed := CreateEvent(nil, False, False, nil);
  inherited Create(True);
  Start;
  WaitForSingleObject(FEvent, INFINITE);
end;

destructor TGCMZAPIThread.Destroy();
begin
  SendMessage(FWindow, WM_SYSCOMMAND, SC_CLOSE, 0);
  WaitForSingleObject(FEvent, INFINITE);
  CloseHandle(FProcessed);
  CloseHandle(FReady);
  CloseHandle(FEvent);
  inherited Destroy();
end;

end.
