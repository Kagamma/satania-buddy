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

unit Utils.ActiveWindow;

{$I configs.inc}

interface

uses
  {$define unit_declare_interface}
  {$I utils.activewindow_windows.inc}
  {$I utils.activewindow_linux_x11.inc}
  {$undef unit_declare_interface}
  Classes, SysUtils, Process, Forms, LCLIntf, Math, Globals,
  CastleVectors, CastleWindow;

type
  TUtilActiveWindow = class
  protected
    // Platform-specific function to get suitable position to place
    // Satania on top of active window
    function LookForSuitablePosition: TPoint;
  public
    {$define unit_public}
    {$I utils.activewindow_windows.inc}
    {$I utils.activewindow_linux_x11.inc}
    {$undef unit_public}
    IsWindowChanged: Boolean;
    constructor Create;
    procedure Update;
  end;

var
  IgnoreHandleList: TQWordList;
  UtilActiveWindow: TUtilActiveWindow;

procedure AddFormToIgnoreHandleList(F: TForm);

{$define unit_declare_uses}
{$I utils.activewindow_windows.inc}
{$I utils.activewindow_linux_x11.inc}
{$undef unit_declare_uses}

implementation

uses
  form.touch,
  Utils.Coords,
  mcdowell;

procedure AddFormToIgnoreHandleList(F: TForm);
begin
  IgnoreHandleList.Add(QWord(F.Handle));
end;

{$define unit_implmentation}

{$I utils.activewindow_windows.inc}
{$I utils.activewindow_linux_x11.inc}

{$undef unit_implementation}

procedure TUtilActiveWindow.Update;
var
  Position: TPoint;
begin
  try
    Position := LookForSuitablePosition;
    if Position.X <> -999999 then
    begin
      Satania.Sprite.Translation := Vector3(ScreenCoordToUI(Vector2(Position.X, Position.Y)), 0);
      FormTouch.UpdateMonitor;
    end;
  except
    on E: Exception do
    begin
      DumpExceptionCallStack(E);
      Satania.Error(E.Message);
    end;
  end;
  //if IsWindowChanged then
  //  Satania.ActionFromFile(Save.Settings.DefaultEvilScheme);
end;

initialization
  IgnoreHandleList := TQwordList.Create;

finalization
  FreeAndNil(IgnoreHandleList);

end.

