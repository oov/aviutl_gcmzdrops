unit API;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, Classes;

const
  HWND_MESSAGE = HWND(-3);

type
  TOnCall = function(const Window: THandle; CDS: PCopyDataStruct): LRESULT of object;

  { TGCMZAPIThread }

  TGCMZAPIThread = class(TThread)
  private
    FWindow: THandle;
    FEvent, FProcessed: THandle;
    FOnCall: TOnCall;
    function WindowProc(Window: THandle; Msg: UINT; WP: WPARAM; LP: LPARAM): LRESULT;
  protected
    procedure Execute; override;
  public
    procedure Processed();
    constructor Create();
    destructor Destroy(); override;
    property Window: THandle read FWindow;
    property OnCall: TOnCall read FOnCall write FOnCall;
  end;

implementation

uses
  Util;

var
  APIThread: TGCMZAPIThread;

function gWindowProc(Window: THandle; Msg: UINT; WP: WPARAM;
  LP: LPARAM): LRESULT; stdcall;
begin
  Result := APIThread.WindowProc(Window, Msg, WP, LP);
end;

{ TGCMZAPIThread }

function TGCMZAPIThread.WindowProc(Window: THandle; Msg: UINT;
  WP: WPARAM; LP: LPARAM): LRESULT;
begin
  case Msg of
    WM_COPYDATA:
    begin
      if FOnCall <> nil then
      begin
        FOnCall(THandle(WP), PCopyDataStruct(LP));
        WaitForSingleObject(FProcessed, INFINITE);
      end;
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

constructor TGCMZAPIThread.Create();
begin
  APIThread := Self;

  FEvent := CreateEvent(nil, False, False, nil);
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
  CloseHandle(FEvent);
  inherited Destroy();
end;

end.
