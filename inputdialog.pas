unit InputDialog;

{$mode objfpc}{$H+}
{$CODEPAGE UTF-8}

interface

function InputBox(const Parent: THandle; const Caption: UTF8String;
  const CanIgnore: boolean; var Value: UTF8String): integer;

implementation

uses
  Windows;

{$R *.res}

var
  Caption: UTF8String;
  Value: UTF8String;
  CanIgnore: boolean;
  FontHandle: THandle;

function DlgProc(Dialog: HWND; Message: UINT; WP: WPARAM; LP: LPARAM): LRESULT; stdcall;
var
  NCM: TNonClientMetrics;
  WS: WideString;
begin
  Result := 0;
  case Message of
    WM_INITDIALOG:
    begin
      NCM.cbSize := SizeOf(NCM);
      SystemParametersInfo(SPI_GETNONCLIENTMETRICS, SizeOf(NCM), @NCM, 0);
      FontHandle := CreateFontIndirect(NCM.lfMessageFont);
      SendMessageW(Dialog, WM_SETFONT, WPARAM(FontHandle), 0);
      SendMessageW(GetDlgItem(Dialog, 0), WM_SETFONT, WPARAM(FontHandle), 0);
      SendMessageW(GetDlgItem(Dialog, 1), WM_SETFONT, WPARAM(FontHandle), 0);
      SendMessageW(GetDlgItem(Dialog, 2), WM_SETFONT, WPARAM(FontHandle), 0);
      SendMessageW(GetDlgItem(Dialog, 3), WM_SETFONT, WPARAM(FontHandle), 0);
      SetWindowTextW(Dialog, PWideChar(WideString(Caption)));
      SetWindowTextW(GetDlgItem(Dialog, 0), PWideChar(WideString(Value)));
      if CanIgnore then
        ShowWindow(GetDlgItem(Dialog, 3), SW_SHOW)
      else
        ShowWindow(GetDlgItem(Dialog, 3), SW_HIDE);
      Result := 1;
    end;
    WM_ACTIVATE: SetForegroundWindow(Dialog);
    WM_COMMAND:
    begin
      case LOWORD(WP) of
        1:
        begin
          SetLength(WS, GetWindowTextLengthW(GetDlgItem(Dialog, 0)) + 1);
          GetWindowTextW(GetDlgItem(Dialog, 0), @WS[1], Length(WS));
          Value := UTF8String(WideString(PWideChar(WS)));
          EndDialog(Dialog, idOk);
          Result := 1;
        end;
        2:
        begin
          EndDialog(Dialog, idCancel);
          Result := 1;
        end;
        3:
        begin
          EndDialog(Dialog, idIgnore);
          Result := 1;
        end;
        else;
      end;
    end;
    WM_DESTROY:
    begin
      DeleteObject(FontHandle);
    end;
    else;
  end;
end;

function InputBox(const Parent: THandle; const Caption: UTF8String;
  const CanIgnore: boolean; var Value: UTF8String): integer;
begin
  InputDialog.Caption := Caption;
  InputDialog.Value := Value;
  InputDialog.CanIgnore := CanIgnore;
  Result := DialogBoxW(hInstance, 'INPUTDIALOG', Parent, @DlgProc);
  if Result = idOk then
    Value := InputDialog.Value;
end;

end.
