unit InputDialog;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

function InputBox(const Parent: THandle; const Caption: UTF8String;
  const Title: UTF8String; var Value: UTF8String): boolean;

implementation

uses
  Windows;

{$R *.res}

type
  TInputDlgVars = record
    Font: THandle;
    Title: PWideChar;
    Caption: PWideChar;
    Value: WideString;
  end;
  PInputDlgVars = ^TInputDlgVars;

var
  DlgVars: PInputDlgVars;

function DlgProc(Dialog: HWND; Message: UINT; WP: WPARAM; LP: LPARAM): LRESULT; stdcall;
var
  NCM: TNonClientMetrics;
begin
  Result := 0;
  case Message of
    WM_INITDIALOG:
    begin
      NCM.cbSize := SizeOf(NCM);
      SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NCM), @NCM, 0);
      DlgVars^.Font := CreateFontIndirect(NCM.lfMessageFont);
      SendMessageW(Dialog, WM_SETFONT, WPARAM(DlgVars^.Font), 0);
      SendMessageW(GetDlgItem(Dialog, 0), WM_SETFONT, WPARAM(DlgVars^.Font), 0);
      SendMessageW(GetDlgItem(Dialog, 1), WM_SETFONT, WPARAM(DlgVars^.Font), 0);
      SendMessageW(GetDlgItem(Dialog, 2), WM_SETFONT, WPARAM(DlgVars^.Font), 0);
      SendMessageW(GetDlgItem(Dialog, 3), WM_SETFONT, WPARAM(DlgVars^.Font), 0);
      SetWindowTextW(Dialog, DlgVars^.Title);
      SetWindowTextW(GetDlgItem(Dialog, 3), DlgVars^.Caption);
      SetWindowTextW(GetDlgItem(Dialog, 0), PWideChar(DlgVars^.Value));
      Result := 1;
    end;
    WM_COMMAND:
    begin
      case LOWORD(WP) of
        1:
        begin
          SetLength(DlgVars^.Value, GetWindowTextLengthW(GetDlgItem(Dialog, 0)) + 1);
          GetWindowTextW(GetDlgItem(Dialog, 0), @DlgVars^.Value[1],
            Length(DlgVars^.Value));
          EndDialog(Dialog, idOk);
          Result := 1;
        end;
        2:
        begin
          EndDialog(Dialog, idCancel);
          Result := 1;
        end;
        else;
      end;
    end;
    WM_DESTROY:
    begin
      DeleteObject(DlgVars^.Font);
    end;
    else;
  end;
end;

function InputBox(const Parent: THandle; const Caption: UTF8String;
  const Title: UTF8String; var Value: UTF8String): boolean;
var
  v: TInputDlgVars;
begin
  v.Title := PWideChar(WideString(Title));
  v.Caption := PWideChar(WideString(Caption));
  v.Value := WideString(Value);
  DlgVars := @v;
  Result := DialogBoxW(hInstance, 'INPUTDIALOG', Parent, @DlgProc) = idOk;
  if Result then
    Value := PChar(UTF8String(v.Value));
end;

end.
