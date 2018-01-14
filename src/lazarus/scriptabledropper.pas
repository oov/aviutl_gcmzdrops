unit ScriptableDropper;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows, Lua;

type
  TPopupEvent = procedure(Sender: TObject; const Pt: TPoint) of object;

  { TScriptableDropper }

  TScriptableDropper = class
  private
    FOnNotify: TPopupEvent;
    FTarget: THandle;
    FHook: THandle;
    FPopupMenu: THandle;
    FMenuIndices: array of LONG;
    function HookProc(nCode: integer; wp: WPARAM; lp: LPARAM): LRESULT;
    constructor Create();
  public
    destructor Destroy(); override;
    function InstallHook(const Handle: THandle): boolean;
    procedure UninstallHook();
    procedure RecreateMenu(L: Plua_state);
    procedure Popup(L: Plua_state; const hWnd: THandle; const Pt: TPoint);
    property OnNotify: TPopupEvent read FOnNotify write FOnNotify;
  end;

var
  SCDropper: TScriptableDropper;

implementation

uses
  SysUtils, Util;

function HookProcTrampoline(nCode: integer; wp: WPARAM; lp: LPARAM): LRESULT; stdcall;
begin
  Result := SCDropper.HookProc(nCode, wp, lp);
end;

{ TScriptableDropper }

function TScriptableDropper.HookProc(nCode: integer; wp: WPARAM; lp: LPARAM): LRESULT;
const
  MB_MBUTTONDOWN = $0207;
var
  Msg: PMSG absolute lp;
  Pt: TPoint;
begin
  if (nCode < 0) or (nCode <> HC_ACTION) or (Msg^.hwnd <> FTarget) then
  begin
    Result := CallNextHookEx(FHook, nCode, wp, lp);
    Exit;
  end;
  case Msg^.message of
    WM_RBUTTONDOWN:
    begin
      if ((Msg^.wParam and MK_SHIFT) = MK_SHIFT) and
        ((Msg^.wParam and MK_CONTROL) = MK_CONTROL) then
      begin
        Result := 0;
        Msg^.message := WM_NULL;
        Pt.x := Msg^.lParam and $ffff;
        Pt.y := (Msg^.lParam shr 16) and $ffff;
        FOnNotify(Self, Pt);
        Exit;
      end;
    end;
    MB_MBUTTONDOWN:
    begin
      Result := 0;
      Msg^.message := WM_NULL;
      Pt.x := Msg^.lParam and $ffff;
      Pt.y := (Msg^.lParam shr 16) and $ffff;
      FOnNotify(Self, Pt);
      Exit;
    end;
  end;
  Result := CallNextHookEx(FHook, nCode, wp, lp);
end;

constructor TScriptableDropper.Create;
begin
  inherited Create();
end;

destructor TScriptableDropper.Destroy;
begin
  UninstallHook();
  if FPopupMenu <> 0 then
    DestroyMenu(FPopupMenu);
  inherited Destroy;
end;

function TScriptableDropper.InstallHook(const Handle: THandle): boolean;
begin
  FTarget := Handle;
  FHook := SetWindowsHookEx(WH_GETMESSAGE, @HookProcTrampoline, hInstance, 0);
  Result := FHook <> 0;
end;

procedure TScriptableDropper.UninstallHook;
begin
  if FHook = 0 then
    Exit;
  UnhookWindowsHookEx(FHook);
  FHook := 0;
  FTarget := 0;
end;

procedure TScriptableDropper.RecreateMenu(L: Plua_state);
var
  I, J, N, M, IdxLen: integer;
  SJIS: ShiftJISString;
  SubMenu: THandle;
begin
  if FPopupMenu <> 0 then
    DestroyMenu(FPopupMenu);
  FPopupMenu := CreatePopupMenu();

  SetLength(FMenuIndices, 0);
  IdxLen := 0;

  lua_getfield(L, -1, 'getdroppermenuitems');
  if lua_pcall(L, 0, 1, 0) <> 0 then
    raise Exception.Create('problem occurred executing getdroppermenuitems:'#13#10 +
      UTF8String(ShiftJISString(lua_tostring(L, -1))));

  N := lua_objlen(L, -1);
  for I := 1 to N do
  begin
    lua_rawgeti(L, -1, I);
    if lua_isnil(L, -1) then
    begin
      lua_pop(L, 1);
      continue;
    end;

    if lua_isstring(L, -1) then
    begin
      SetLength(FMenuIndices, IdxLen + 1);
      FMenuIndices[IdxLen] := MAKELONG(I, 0);
      Inc(IdxLen);
      SJIS := lua_tostring(L, -1);
      AppendMenuW(FPopupMenu, MF_ENABLED or MF_STRING, IdxLen,
        PWideChar(WideString(SJIS)));
      lua_pop(L, 1);
      continue;
    end;

    if lua_istable(L, -1) then
    begin
      M := lua_objlen(L, -1);
      if M = 0 then
      begin
        lua_pop(L, 1);
        continue;
      end;

      SubMenu := CreatePopupMenu();
      for J := 1 to M do
      begin
        lua_rawgeti(L, -1, J);
        SetLength(FMenuIndices, IdxLen + 1);
        FMenuIndices[IdxLen] := MAKELONG(I, J);
        Inc(IdxLen);
        SJIS := lua_tostring(L, -1);
        AppendMenuW(SubMenu, MF_ENABLED or MF_STRING, IdxLen,
          PWideChar(WideString(SJIS)));
        lua_pop(L, 1);
      end;
      lua_getfield(L, -1, 'name');
      SJIS := lua_tostring(L, -1);
      AppendMenuW(FPopupMenu, MF_ENABLED or MF_STRING or MF_POPUP,
        SubMenu, PWideChar(WideString(SJIS)));
      lua_pop(L, 2);
    end;
  end;
  lua_pop(L, 1);
end;

procedure TScriptableDropper.Popup(L: Plua_state; const hWnd: THandle; const Pt: TPoint);
var
  I: integer;
  CPt: TPoint;
  hs: THandleDynArray;
begin
  CPt := Pt;
  ClientToScreen(hWnd, CPt);
  I := integer(TrackPopupMenu(FPopupMenu, TPM_TOPALIGN or TPM_LEFTALIGN or
    TPM_RETURNCMD or TPM_RIGHTBUTTON, CPt.x, CPt.y, 0, hWnd, nil));
  if I = 0 then
    Exit;
  lua_getfield(L, -1, 'selectdropper');
  I := FMenuIndices[I - 1];
  lua_pushinteger(L, LOWORD(I));
  lua_pushinteger(L, HIWORD(I));
  lua_newtable(L);
  lua_pushinteger(L, CPt.x);
  lua_setfield(L, -2, 'x');
  lua_pushinteger(L, CPt.y);
  lua_setfield(L, -2, 'y');
  lua_pushinteger(L, hWnd);
  lua_setfield(L, -2, 'parent');
  hs := DisableFamilyWindows(hWnd);
  try
    if lua_pcall(L, 3, 2, 0) <> 0 then
      raise Exception.Create('problem occurred executing selectdropper:'#13#10 +
        UTF8String(ShiftJISString(lua_tostring(L, -1))));
  finally
    EnableFamilyWindows(hs);
  end;
end;

initialization
  SCDropper := TScriptableDropper.Create();

finalization
  SCDropper.Free;

end.
