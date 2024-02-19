{

satania-buddy
Copyright (C) 2022-2024 kagamma

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

{$if defined(LINUX_X11)}

{$ifdef unit_declare_interface}
  sataniaqt, qtwidgets, xlib, xatom, x, form.ask, form.bubble,
{$endif unit_declare_interface}

{$ifdef unit_implmentation}
procedure TFormTouch.TimerTimer(Sender: TObject);
var
  Data: PCardinal;
  A, B, C, D: Cardinal;
begin
  // Try to raise window when virtual desktop's changed
  if XGetWindowProperty(
        XDisplay, XDefaultRootWindow(XDisplay),
        XInternAtom(XDisplay, '_NET_CURRENT_DESKTOP', False),
        0, 1, False, XA_CARDINAL, @A, @B, @C, @D, @Data
    ) = Success then
  begin
    if Data^ <> LastVirtualDesktop then
    begin
      LastVirtualDesktop := Data^;
      BringToFront;
      FormAsk.BringToFront;
      FormBubble.BringToFront;
    end;
    XFree(Data);
  end;
end;
{$endif}

{$endif}

