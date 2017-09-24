library GCMZDrops;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

uses
  AviUtl,
  Main,
  DropTarget,
  InputDialog,
  Util, lua, LuaFuncs, LuaObj, ActiveX, UsedFiles;

exports
  GetFilterTable;

initialization
  OleInitialize(nil);
  Randomize();
  LoadLua();

finalization
  FreeLua();
  OleUninitialize();

end.
