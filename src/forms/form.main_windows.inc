{

satania-buddy
Copyright (C) 2022-2023 kagamma

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

{$if defined(WINDOWS)}

{$ifdef unit_declare_uses}
  Windows, DwmApi, UxTheme,
{$endif unit_declare_uses}

{$ifdef unit_public}
    procedure CreateParams(var Params: TCreateParams); override;
{$endif unit_public}

{$ifdef unit_implmentation}

procedure TFormMain.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Attrib: LongInt;
  B: TDWMBlurBehind;
  S: String;
  Console: HWND;
  OldStyle, NewStyle: LONG_PTR;
begin
  Color := clFuchsia;
  Attrib := GetWindowLongA(Handle, GWL_EXSTYLE);
  SetWindowLongA(Handle, GWL_EXSTYLE, Attrib or WS_EX_LAYERED or WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW);
  SetLayeredWindowAttributes(Handle, Color, 0, LWA_COLORKEY);
  InitDwmLibrary;
  DwmCompositionEnabled;
  B.dwFlags := DWM_BB_ENABLE or DWM_BB_BLURREGION;
  B.fEnable := true;
  B.hRgnBlur := CreateRectRgn(0, 0, -1, -1);
  B.fTransitionOnMaximized := true;
  DwmEnableBlurBehindWindow(Handle, @B);
  // Hide console
  Console := GetConsoleWindow;
  OldStyle := GetWindowLongPtr(Console, GWL_STYLE);
  NewStyle := OldStyle and not WS_SYSMENU;
  SetWindowLongPtr(Console, GWL_STYLE, NewStyle);
  ShowWindow(Console, SW_HIDE);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
end;

{$endif unit_implmentation}

{$endif}


