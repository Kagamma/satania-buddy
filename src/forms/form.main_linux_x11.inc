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

{$ifdef unit_declare_uses}
  X, Xlib, xatom, sataniaqt, qtwidgets,
{$endif unit_declare_uses}

{$ifdef unit_declare_interface}
{$endif}

{$ifdef unit_implmentation}

procedure TFormMain.FormCreate(Sender: TObject);
var
  Attrib: LongInt;
begin
  XDisplay := QX11Info_display();
  {TQtWidget(TQtMainWindow(Handle).Widget).setAttribute(QtWA_TranslucentBackground);
  TQtWidget(TQtMainWindow(Handle).Widget).setAttribute(QtWA_NoSystemBackground);
  TQtWidget(TQtMainWindow(SataniaDisplay.Handle).Widget).setAttribute(QtWA_TranslucentBackground);
  TQtWidget(TQtMainWindow(SataniaDisplay.Handle).Widget).setAttribute(128);
  // TQtWidget(TQtMainWindow(Handle).Widget).setWindowFlags(QtWindowTransparentForInput or QtFramelessWindowHint);}
  // TODO: Qt5 event pass-through
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
end;

{$endif unit_implmentation}

{$endif}

