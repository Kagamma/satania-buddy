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

unit Form.Touch;

{$I configs.inc}

interface

uses               
  {$define unit_declare_interface}
  {$I form.touch_linux_x11.inc}           
  {$I form.touch_windows.inc}
  {$undef unit_declare_interface}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  CastleVectors;

type

  { TFormTouch }

  TFormTouch = class(TForm)
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    OldMousePosX, OldMousePosY, LastVirtualDesktop: Integer;
    IsMouseDown: Boolean;
  public

  end;

var
  FormTouch: TFormTouch;

implementation

{$R *.lfm}

uses
  Form.Main, Utils.ActiveWindow,
  Globals,
  Form.evilc.editor,
  Form.settings,
  Form.Chat,
  Mcdowell;

{ TFormTouch }

{$define unit_implmentation}
{$I form.touch_linux_x11.inc}  
{$I form.touch_windows.inc}
{$undef unit_implmentation}

procedure TFormTouch.FormCreate(Sender: TObject);
begin
  AlphaBlend := true;
  AlphaBlendValue := 1;
  AddFormToIgnoreHandleList(Self);
  LastVirtualDesktop := 1;
end;

procedure TFormTouch.FormDblClick(Sender: TObject);
begin
  FormChat.Show;
end;

procedure TFormTouch.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if (Button <> mbLeft) or (Save.SitOnWindow) or (Satania.IsBlocked) then Exit;
  P := ClientToScreen(Point(X, Y));
  OldMousePosX := P.X;
  OldMousePosY := P.Y;
  IsMouseDown := True;
end;

procedure TFormTouch.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var     
  P: TPoint;
  DeltaX, DeltaY: Integer;
begin
  if IsMouseDown then
  begin
    P := ClientToScreen(Point(X, Y));
    DeltaX := P.X - OldMousePosX;
    DeltaY := P.Y - OldMousePosY;
    Satania.Sprite.Translation := Satania.Sprite.Translation + Vector3(DeltaX, -DeltaY, 0);
    OldMousePosX := P.X;
    OldMousePosY := P.Y;
    Left := Left + DeltaX;
    Top := Top + DeltaY;
  end;
end;

procedure TFormTouch.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  IsMouseDown := False;
end;

procedure TFormTouch.FormShow(Sender: TObject);
begin
  // QWidget_setWindowFlags(TQtMainWindow(Handle).Widget, QtTool);
  // QWidget_setWindowFlags(TQtMainWindow(Handle).Widget, QtWindowStaysOnTopHint);
end;


end.

