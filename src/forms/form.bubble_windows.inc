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

{$ifdef unit_declare_interface}
  Windows,
{$endif unit_declare_interface}

{$ifdef unit_implmentation}
procedure TFormBubble.FormCreate(Sender: TObject);
var
  Attrib: Longint;
begin
  Color := $000002EE;
  Attrib := GetWindowLongA(Handle, GWL_EXSTYLE);
  SetWindowLongA(Handle, GWL_EXSTYLE, Attrib or WS_EX_LAYERED);
  SetLayeredWindowAttributes(Handle, $000002EE, 0, 1);

  Panel.ControlStyle := Panel.ControlStyle - [csOpaque] + [csParentBackground];

  CreateCommons;
end;
{$endif}

{$endif}


