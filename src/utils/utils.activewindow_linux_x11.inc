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
  X, Xlib, glx, sataniaqt, qtwidgets,
{$endif unit_declare_interface}

{$ifdef unit_public}
{$endif unit_public}

{$ifdef unit_implmentation}

var
  PreviousWindow: TWindow;

function FindTopWindow(Window: TWindow): TWindow;
var
  Root, Parent: TWindow;
  Children: PWindow;
  I: Integer;
begin
  while true do
  begin
    if XQueryTree(XDisplay, Window, @Root, @Parent, @Children, @I) = 0 then
      Exit;
    if (Window = Root) or (Parent = Root) then
      break
    else
      Window := Parent;
  end;
  Result := Window;
end;

constructor TUtilActiveWindow.Create;
begin
  inherited;
end;

function TUtilActiveWindow.LookForSuitablePosition: TPoint;
var
  Window: TWindow;
  Attrib: TXWindowAttributes;
  I: Integer;
begin
  IsWindowChanged := False;
  Result := Point(-999999, -999999);
  if IgnoreHandleList.Contains(QWord(GetForegroundWindow)) then
    Exit;
  XGetInputFocus(XDisplay, @Window, @I);
  XGetWindowAttributes(XDisplay, Window, @Attrib);
  if Window = QWidget_winId(TQtMainWindow(FormTouch.Handle).Widget) then
    Exit;
  // Find the actual top window
  Window := FindTopWindow(Window);
  // A bit delay so that GetForegroundWindow has a chance to update
  if PreviousWindow <> Window then
  begin
    PreviousWindow := Window;
    IsWindowChanged := True;
    Exit;
  end;
  XGetWindowAttributes(XDisplay, Window, @Attrib);
  Result := Point(Max(Attrib.X, Attrib.X + Attrib.Width - Save.Settings.SitOnWindowRightMargin), Attrib.Y);
end;

{$endif unit_implmentation}

{$endif LINUX}

