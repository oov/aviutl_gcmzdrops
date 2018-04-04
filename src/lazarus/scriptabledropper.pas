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
    FOriginalProc: WNDPROC;
    FPopupMenu: THandle;
    FMenuIndices: array of LONG;
    function SubClassProc(Hwnd: HWND; Message: UINT; WP: WPARAM; LP: LPARAM): LRESULT;
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

function SubClassProcTrampoline(Hwnd: HWND; Message: UINT; WP: WPARAM;
  LP: LPARAM): LRESULT; stdcall;
begin
  Result := SCDropper.SubClassProc(Hwnd, Message, WP, LP);
end;

{ TScriptableDropper }

function TScriptableDropper.SubClassProc(Hwnd: HWND; Message: UINT;
  WP: WPARAM; LP: LPARAM): LRESULT;
const
  MB_MBUTTONDOWN = $0207;
var
  Pt: TPoint;
begin
  case Message of
    WM_RBUTTONDOWN:
    begin
      if ((WP and MK_SHIFT) = MK_SHIFT) and
        ((WP and MK_CONTROL) = MK_CONTROL) then
      begin
        Result := 0;
        Pt.x := LP and $ffff;
        Pt.y := (LP shr 16) and $ffff;
        FOnNotify(Self, Pt);
        Exit;
      end;
    end;
    MB_MBUTTONDOWN:
    begin
      Result := 0;
      Pt.x := LP and $ffff;
      Pt.y := (LP shr 16) and $ffff;
      FOnNotify(Self, Pt);
      Exit;
    end;
  end;
  Result := CallWindowProc(FOriginalProc, Hwnd, Message, WP, LP);
end;

constructor TScriptableDropper.Create();
begin
  inherited Create();
end;

destructor TScriptableDropper.Destroy();
begin
  UninstallHook();
  if FPopupMenu <> 0 then
    DestroyMenu(FPopupMenu);
  inherited Destroy;
end;

function TScriptableDropper.InstallHook(const Handle: THandle): boolean;
begin
  FOriginalProc := WNDPROC(GetWindowLong(Handle, GWL_WNDPROC));
  SetWindowLong(Handle, GWL_WNDPROC, LONG(@SubClassProcTrampoline));
  FTarget := Handle;
  Result := True;
end;

procedure TScriptableDropper.UninstallHook();
begin
  if not Assigned(FOriginalProc) then
    Exit;
  SetWindowLong(FTarget, GWL_WNDPROC, LONG(FOriginalProc));
  FTarget := 0;
  FOriginalProc := nil;
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
    if lua_isboolean(L, -1) then
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
  if lua_pcall(L, 3, 2, 0) <> 0 then
    raise Exception.Create('problem occurred executing selectdropper:'#13#10 +
      UTF8String(ShiftJISString(lua_tostring(L, -1))));
end;

initialization
  SCDropper := TScriptableDropper.Create();

finalization
  SCDropper.Free;

end.
