unit LuaObj;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  SysUtils, Classes, DropTarget, lua;

type
  { TLua }

  TLua = class
  private
    FState: Plua_state;
    procedure PushState(const Pt: TPoint; const KeyState: DWORD);
  public
    constructor Create();
    destructor Destroy(); override;
    procedure InitDropper();
    function CallDragEnter(const Files: TFiles; const Pt: TPoint;
      const KeyState: DWORD): boolean;
    function CallDragOver(const Files: TFiles; const Pt: TPoint;
      const KeyState: DWORD): boolean;
    procedure CallDragLeave();
    function CallDrop(const Files: TFiles; const Pt: TPoint;
      const KeyState: DWORD): boolean;
    property State: Plua_state read FState write FState;
  end;

  procedure LuaPushFiles(const L: Plua_state; const Files: TFiles);

implementation

uses
  Windows, LuaFuncs, Util;

function LuaAllocator({%H-}ud, ptr: Pointer; {%H-}osize, nsize: size_t): Pointer; cdecl;
begin
  if nsize = 0 then
  begin
    if ptr <> nil then
      FreeMem(ptr);
    Result := nil;
    Exit;
  end;
  if ptr <> nil then
    Result := ReallocMem({%H-}ptr, nsize)
  else
    Result := GetMem(nsize);
end;


procedure LuaPushFiles(const L: Plua_state; const Files: TFiles);
var
  I: integer;
  SJIS: ShiftJISString;
begin
  lua_newtable(L);
  for I := Low(Files) to High(Files) do
  begin
    lua_newtable(L);
    case Files[I].Type_ of
      ftFile:
      begin
        SJIS := Files[I].FilePathOrContent;
        lua_pushlstring(L, @SJIS[1], Length(SJIS));
        lua_setfield(L, -2, 'filepath');
        if Files[I].MediaType <> '' then
        begin
          SJIS := Files[I].MediaType;
          lua_pushlstring(L, @SJIS[1], Length(SJIS));
          lua_setfield(L, -2, 'mediatype');
        end;
      end;
      ftText: raise Exception.Create('ftText cannot use in GCMZDrops');
    end;
    lua_rawseti(L, -2, I + 1);
  end;
end;

{ TLua }

procedure TLua.PushState(const Pt: TPoint; const KeyState: DWORD);
const
  MK_ALT = 32;
var
  L: Plua_state;
begin
  L := FState;
  lua_newtable(L);
  lua_pushinteger(L, Pt.x);
  lua_setfield(L, -2, 'x');
  lua_pushinteger(L, Pt.y);
  lua_setfield(L, -2, 'y');
  lua_pushboolean(L, (KeyState and MK_CONTROL) = MK_CONTROL);
  lua_setfield(L, -2, 'control');
  lua_pushboolean(L, (KeyState and MK_SHIFT) = MK_SHIFT);
  lua_setfield(L, -2, 'shift');
  lua_pushboolean(L, (KeyState and MK_ALT) = MK_ALT);
  lua_setfield(L, -2, 'alt');
  lua_pushboolean(L, (KeyState and MK_LBUTTON) = MK_LBUTTON);
  lua_setfield(L, -2, 'lbutton');
  lua_pushboolean(L, (KeyState and MK_MBUTTON) = MK_MBUTTON);
  lua_setfield(L, -2, 'mbutton');
  lua_pushboolean(L, (KeyState and MK_RBUTTON) = MK_RBUTTON);
  lua_setfield(L, -2, 'rbutton');
end;

constructor TLua.Create;
var
  I: integer;
  L: Plua_state;
  sr: TUnicodeSearchRec;
  BasePath: WideString;
  SJIS: ShiftJISString;
begin
  if not LuaLoaded() then
    raise Exception.Create('lua51.dll not found');
  inherited Create();

  L := lua_newstate(@LuaAllocator, nil);
  if L = nil then
    raise Exception.Create('lua intialization failed');
  try
    luaL_openlibs(L);
    LuaSetFuncs(L);
    BasePath := ExtractFilePath(GetDLLName()) + 'GCMZDrops\';
    lua_getglobal(L, 'package');
    SJIS := ShiftJISString(BasePath + '?.lua');
    lua_pushlstring(L, @SJIS[1], Length(SJIS));
    lua_setfield(L, -2, 'path');
    SJIS := ShiftJISString(BasePath + '?.dll');
    lua_pushlstring(L, @SJIS[1], Length(SJIS));
    lua_setfield(L, -2, 'cpath');
    lua_pop(L, 1);
    lua_getglobal(L, 'require');
    lua_pushstring(L, '_entrypoint');
    if lua_pcall(L, 1, 1, 0) <> 0 then
      raise Exception.Create('failed to execute entrypoint script: ' +
        UTF8String(ShiftJISString(lua_tostring(L, -1))));
    lua_getfield(L, -1, 'init');
    lua_newtable(L);
    I := 1;
    if FindFirst(BasePath + '*.lua', 0, sr) = 0 then
    begin
      repeat
        if (sr.Name = '..') or (sr.Name = '.') or
          ((sr.Attr and faDirectory) = faDirectory) then
          continue;
        if sr.Name = '_entrypoint.lua' then
          continue;
        SJIS := ShiftJISString(ChangeFileExt(sr.Name, WideString('')));
        lua_pushlstring(L, @SJIS[1], Length(SJIS));
        lua_rawseti(L, -2, I);
        Inc(I);
      until FindNext(sr) <> 0;
    end;
    if lua_pcall(L, 1, 0, 0) <> 0 then
      raise Exception.Create('failed to complete init execution: ' +
        UTF8String(ShiftJISString(lua_tostring(L, -1))));
    FState := L;
  except
    lua_close(L);
    raise;
  end;
end;

destructor TLua.Destroy;
begin
  if Assigned(FState) then
  begin
    lua_close(FState);
    FState := nil;
  end;
  inherited Destroy;
end;

procedure TLua.InitDropper;
var
  I: integer;
  L: Plua_state;
  sr: TUnicodeSearchRec;
  BasePath: WideString;
  SJIS: ShiftJISString;
begin
  L := FState;

  BasePath := ExtractFilePath(GetDLLName()) + 'GCMZDrops\dropper\';

  lua_getglobal(L, 'package');

  lua_getfield(L, -1, 'path');
  SJIS := lua_tostring(L, -1) + ShiftJISString(';' + BasePath + '?.lua');
  lua_pop(L, 1);
  lua_pushlstring(L, @SJIS[1], Length(SJIS));
  lua_setfield(L, -2, 'path');

  lua_getfield(L, -1, 'cpath');
  SJIS := lua_tostring(L, -1) + ShiftJISString(';' + BasePath + '?.dll');
  lua_pop(L, 1);
  lua_pushlstring(L, @SJIS[1], Length(SJIS));
  lua_setfield(L, -2, 'cpath');

  lua_pop(L, 1);

  lua_getfield(L, 1, 'initdropper');
  lua_newtable(L);
  I := 1;
  if FindFirst(BasePath + '*.lua', 0, sr) = 0 then
  begin
    repeat
      if (sr.Name = '..') or (sr.Name = '.') or
        ((sr.Attr and faDirectory) = faDirectory) then
        continue;
      SJIS := ShiftJISString('dropper\' + ChangeFileExt(sr.Name, WideString('')));
      lua_pushlstring(L, @SJIS[1], Length(SJIS));
      lua_rawseti(L, -2, I);
      Inc(I);
    until FindNext(sr) <> 0;
  end;
  if lua_pcall(L, 1, 0, 0) <> 0 then
    raise Exception.Create('failed to complete initdropper execution: ' +
      UTF8String(ShiftJISString(lua_tostring(L, -1))));
end;

function TLua.CallDragEnter(const Files: TFiles; const Pt: TPoint;
  const KeyState: DWORD): boolean;
var
  L: Plua_state;
begin
  L := FState;
  lua_getfield(L, 1, 'ondragenter');
  LuaPushFiles(L, Files);
  PushState(Pt, KeyState);
  if lua_pcall(L, 2, 1, 0) <> 0 then
    raise Exception.Create('problem occurred executing ondragenter:'#13#10 +
      UTF8String(ShiftJISString(lua_tostring(L, -1))));
  Result := lua_toboolean(L, -1);
  lua_pop(L, 1);
end;

function TLua.CallDragOver(const Files: TFiles; const Pt: TPoint;
  const KeyState: DWORD): boolean;
var
  L: Plua_state;
begin
  L := FState;
  lua_getfield(L, 1, 'ondragover');
  LuaPushFiles(L, Files);
  PushState(Pt, KeyState);
  if lua_pcall(L, 2, 1, 0) <> 0 then
    raise Exception.Create('problem occurred executing ondragover:'#13#10 +
      UTF8String(ShiftJISString(lua_tostring(L, -1))));
  Result := lua_toboolean(L, -1);
  lua_pop(L, 1);
end;

procedure TLua.CallDragLeave;
var
  L: Plua_state;
begin
  L := FState;
  lua_getfield(L, 1, 'ondragleave');
  if lua_pcall(L, 0, 0, 0) <> 0 then
    raise Exception.Create('problem occurred executing ondragleave:'#13#10 +
      UTF8String(ShiftJISString(lua_tostring(L, -1))));
end;

function TLua.CallDrop(const Files: TFiles; const Pt: TPoint;
  const KeyState: DWORD): boolean;
var
  L: Plua_state;
begin
  L := FState;
  lua_getfield(L, 1, 'ondrop');
  LuaPushFiles(L, Files);
  PushState(Pt, KeyState);
  if lua_pcall(L, 2, 1, 0) <> 0 then
    raise Exception.Create('problem occurred executing ondrop:'#13#10 +
      UTF8String(ShiftJISString(lua_tostring(L, -1))));
  Result := lua_toboolean(L, -1);
  lua_pop(L, 1);
end;

end.
