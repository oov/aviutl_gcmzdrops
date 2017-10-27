unit LuaIni;

{$mode objfpc}{$H+}

interface

uses
  Classes, Lua;

function LuaIniRegisterMetaTable(L: Plua_State): integer;
function LuaIniInit(L: Plua_State; const S: TStream): integer;

implementation

uses
  SysUtils, IniFiles;

const
  MetaTableName = 'gcmzini';

type
  TLuaIni = record
    Ini: TMemIniFile;
  end;
  PLuaIni = ^TLuaIni;

function LuaGetUserData(L: Plua_State; const Idx: integer;
  const MetaTableName: PChar): Pointer;
begin
  Result := lua_touserdata(L, Idx);
  if Assigned(Result) and (lua_getmetatable(L, Idx) > 0) then
  begin
    lua_getfield(L, LUA_REGISTRYINDEX, MetaTableName);
    if lua_rawequal(L, -1, -2) then
    begin
      lua_pop(L, 2);
      Exit;
    end;
  end;
  raise Exception.CreateFmt('index: %d is not %s', [Idx, string(MetaTableName)]);
end;

function LuaIniFinalizer(L: Plua_State): integer; cdecl;
var
  P: PLuaIni;
begin
  P := LuaGetUserData(L, 1, MetaTableName);
  FreeAndNil(P^.Ini);
  lua_pop(L, 1);
  Result := 0;
end;

function LuaIniToString(L: Plua_State): integer; cdecl;
var
  P: PLuaIni;
  SL: TStringList;
  S: string;
begin
  P := LuaGetUserData(L, 1, MetaTableName);
  SL := TStringList.Create;
  try
    P^.Ini.GetStrings(SL);
    S := SL.Text;
  finally
    SL.Free;
  end;
  lua_pop(L, 1);
  lua_pushlstring(L, @S[1], Length(S));
  Result := 1;
end;

function LuaIniGet(L: Plua_State): integer; cdecl;
var
  P: PLuaIni;
  S: string;
begin
  P := LuaGetUserData(L, 1, MetaTableName);
  S := P^.Ini.ReadString(lua_tostring(L, 2), lua_tostring(L, 3),
    lua_tostring(L, 4));
  lua_pop(L, 4);
  lua_pushlstring(L, @S[1], Length(S));
  Result := 1;
end;

function LuaIniSet(L: Plua_State): integer; cdecl;
var
  P: PLuaIni;
begin
  P := LuaGetUserData(L, 1, MetaTableName);
  P^.Ini.WriteString(
    lua_tostring(L, 2),
    lua_tostring(L, 3),
    lua_tostring(L, 4)
    );
  lua_pop(L, 4);
  Result := 0;
end;

function LuaIniDelete(L: Plua_State): integer; cdecl;
var
  P: PLuaIni;
begin
  P := LuaGetUserData(L, 1, MetaTableName);
  P^.Ini.DeleteKey(lua_tostring(L, 2), lua_tostring(L, 3));
  lua_pop(L, 3);
  Result := 0;
end;

function LuaIniDeleteSection(L: Plua_State): integer; cdecl;
var
  P: PLuaIni;
begin
  P := LuaGetUserData(L, 1, MetaTableName);
  P^.Ini.EraseSection(lua_tostring(L, 2));
  lua_pop(L, 2);
  Result := 0;
end;

function LuaIniSections(L: Plua_State): integer; cdecl;
var
  P: PLuaIni;
  SL: TStringList;
  S: string;
  I: integer;
begin
  P := LuaGetUserData(L, 1, MetaTableName);
  lua_pop(L, 1);
  lua_newtable(L);
  SL := TStringList.Create;
  try
    P^.Ini.ReadSections(SL);
    for I := 0 to SL.Count - 1 do
    begin
      S := SL.Strings[I];
      lua_pushlstring(L, @S[1], Length(S));
      lua_rawseti(L, -2, I + 1);
    end;
  finally
    SL.Free;
  end;
  Result := 1;
end;


function LuaIniKeys(L: Plua_State): integer; cdecl;
var
  P: PLuaIni;
  SL: TStringList;
  S: string;
  I: integer;
begin
  P := LuaGetUserData(L, 1, MetaTableName);
  S := lua_tostring(L, 2);
  lua_pop(L, 2);
  lua_newtable(L);
  SL := TStringList.Create;
  try
    P^.Ini.ReadSection(S, SL);
    for I := 0 to SL.Count - 1 do
    begin
      S := SL.Strings[I];
      lua_pushlstring(L, @S[1], Length(S));
      lua_rawseti(L, -2, I + 1);
    end;
  finally
    SL.Free;
  end;
  Result := 1;
end;

function LuaIniExists(L: Plua_State): integer; cdecl;
var
  P: PLuaIni;
  B: Boolean;
begin
  P := LuaGetUserData(L, 1, MetaTableName);
  B := P^.Ini.ValueExists(lua_tostring(L, 2), lua_tostring(L, 3));
  lua_pop(L, 3);
  lua_pushboolean(L, B);
  Result := 1;
end;

function LuaIniSectionExists(L: Plua_State): integer; cdecl;
var
  P: PLuaIni;
  B: Boolean;
begin
  P := LuaGetUserData(L, 1, MetaTableName);
  B := P^.Ini.SectionExists(lua_tostring(L, 2));
  lua_pop(L, 2);
  lua_pushboolean(L, B);
  Result := 1;
end;


function LuaIniRegisterMetaTable(L: Plua_State): integer;
begin
  lua_newtable(L);
  lua_pushvalue(L, -1);
  lua_setfield(L, LUA_REGISTRYINDEX, MetaTableName);

  lua_pushcfunction(L, @LuaIniFinalizer);
  lua_setfield(L, -2, '__gc');

  lua_pushcfunction(L, @LuaIniToString);
  lua_setfield(L, -2, '__tostring');

  lua_newtable(L);
  lua_pushcfunction(L, @LuaIniGet);
  lua_setfield(L, -2, 'get');
  lua_pushcfunction(L, @LuaIniSet);
  lua_setfield(L, -2, 'set');
  lua_pushcfunction(L, @LuaIniDelete);
  lua_setfield(L, -2, 'delete');
  lua_pushcfunction(L, @LuaIniDeleteSection);
  lua_setfield(L, -2, 'deletesection');
  lua_pushcfunction(L, @LuaIniSections);
  lua_setfield(L, -2, 'sections');
  lua_pushcfunction(L, @LuaIniKeys);
  lua_setfield(L, -2, 'keys');
  lua_pushcfunction(L, @LuaIniExists);
  lua_setfield(L, -2, 'exists');
  lua_pushcfunction(L, @LuaIniSectionExists);
  lua_setfield(L, -2, 'sectionexists');
  lua_setfield(L, -2, '__index');

  lua_pop(L, 1);
  Result := 0;
end;

function LuaIniInit(L: Plua_State; const S: TStream): integer;
var
  SL: TStringList;
  Ini: TMemIniFile;
  LI: PLuaIni;
begin
  Ini := TMemIniFile.Create('');
  try
    SL := TStringList.Create;
    try
      SL.LoadFromStream(S);
      Ini.SetStrings(SL);
    finally
      SL.Free;
    end;
    LI := lua_newuserdata(L, SizeOf(TLuaIni));
    if not Assigned(LI) then
      raise Exception.Create('cannot allocate userdata memory');
    LI^.Ini := Ini;
    lua_getfield(L, LUA_REGISTRYINDEX, MetaTableName);
    lua_setmetatable(L, -2);
    Result := 1;
  except
    Ini.Free;
    raise;
  end;
end;

end.
