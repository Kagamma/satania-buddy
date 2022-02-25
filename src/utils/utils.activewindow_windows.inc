{

satania-buddy
Copyright (C) 2022-2022 kagamma

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

{$ifdef WINDOWS}

{$ifdef unit_declare_interface}
  Windows,
{$endif unit_declare_interface}

{$ifdef unit_implmentation}

var
  PreviousWindow: THandle;

constructor TUtilActiveWindow.Create;
begin
  inherited;
end;

function TUtilActiveWindow.LookForSuitablePosition: TPoint;
var
  Window: THandle;
  Rect: TRect;
begin
  IsWindowChanged := False;
  Result := Point(-999999, -999999);
  Window := GetForegroundWindow;
  if IgnoreHandleList.Contains(QWord(Window)) then
    Exit;   
  // A bit delay so that GetForegroundWindow has a chance to update
  if PreviousWindow <> Window then
  begin
    PreviousWindow := Window;
    IsWindowChanged := True;
    Exit;
  end;
  if Window <> Satania.Form.Handle then
  begin
    GetWindowRect(Window, Rect);
    Result := Point(Max(Rect.Location.X, Rect.Location.X + Rect.Width - Save.Settings.SitOnWindowRightMargin), Rect.Location.Y);
  end;
end;

{$endif}
{$endif}
