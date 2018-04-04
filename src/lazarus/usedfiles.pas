unit UsedFiles;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

uses
  Windows;

procedure AddUsedFile(const FilePath: UTF8String; const TryDelete: boolean = True);
procedure CleanupUsedFile();

implementation

uses
  SysUtils, Classes;

var
  Files: TStringList;

function Delete(const FilePath: UTF8String): boolean;
var
  WS: WideString;
begin
  WS := WideString(FilePath);
  if DeleteFileW(PWideChar(WS)) then
  begin
    Result := True;
    Exit;
  end;
  if not FileExists(WS) then
  begin
    Result := True;
    Exit;
  end;
  if MoveFileExW(PWideChar(WS), nil, MOVEFILE_DELAY_UNTIL_REBOOT) then
  begin
    Result := True;
    Exit;
  end;
  Result := False;
end;

procedure AddUsedFile(const FilePath: UTF8String; const TryDelete: boolean);
begin
  if (not TryDelete) or (not Delete(FilePath)) then
    Files.Add(FilePath);
end;

procedure CleanupUsedFile();
var
  I: integer;
begin
  for I := Files.Count - 1 downto 0 do
    if Delete(Files.Strings[I]) then
      Files.Delete(I);
end;

initialization
  Files := TStringList.Create();

finalization
  CleanupUsedFile();
  Files.Free();

end.
