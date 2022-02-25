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

{$undef unit_declare_uses}
{$I utils.activewindow_windows.inc}
{$I utils.activewindow_linux_x11.inc}
{$undef unit_declare_uses}

implementation

uses
  Form.Main, Mcdowell;

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
  Position := LookForSuitablePosition;
  if Position.X <> -999999 then
  begin
    Satania.Sprite.Translation := Vector3(ScreenCoordToUI(Vector2(Position.X, Position.Y)), 0);
  end;
  //if IsWindowChanged then
  //  Satania.ActionFromFile(Save.Settings.DefaultEvilScheme);
end;

initialization
  IgnoreHandleList := TQwordList.Create;

finalization
  FreeAndNil(IgnoreHandleList);

end.

