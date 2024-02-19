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
    TimerBlinking: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure TimerBlinkingStartTimer(Sender: TObject);
    procedure TimerBlinkingStopTimer(Sender: TObject);
    procedure TimerBlinkingTimer(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    BlinkCount: Integer;
    OldMousePosX, OldMousePosY, LastVirtualDesktop: Integer;
    IsMouseDown: Boolean;
  public
    procedure UpdateMonitor;
  end;

var
  FormTouch: TFormTouch;

implementation

{$R *.lfm}

uses
  Form.Main, Utils.ActiveWindow,
  Globals,
  form.tool.evilceditor,
  Form.settings,
  Form.Chat,
  mcdowell.EvilC,
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
  Width := 40;
  Height := 40;
end;

procedure TFormTouch.UpdateMonitor;
begin
  if Monitor <> FormMain.Monitor then
  begin
    FormMain.Left := Monitor.Left;
    FormMain.Top := Monitor.Top;
    FormMain.MakeFullyVisible(Monitor);
  end;
end;

procedure TFormTouch.FormDblClick(Sender: TObject);
begin
  if not Save.Settings.TouchPanelOpenChatWebUI then
    FormChat.Show
  else
    FormChat.ShowWebUI;
end;

procedure TFormTouch.FormDropFiles(Sender: TObject;
  const FileNames: array of string);
var
  V: TSEValue;
  I: Integer;
begin
  // Do not cache drop-files.evil, as we will need to recompile the bytecode in
  // order to inject new constant
  ScriptCacheMap.ClearSingle(PATH_SCRIPTS + Save.Settings.Skin + '/' + 'system/drop-files.evil');
  GC.AllocMap(@V);
  for I := 0 to High(FileNames) do
    SEMapSet(V, I, FileNames[I]);
  Satania.Script.ConstMap.AddOrSetValue('drop_files', V);
  Satania.ActionFromFile('system/drop-files.evil', False);
end;

procedure TFormTouch.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if (Button <> mbLeft) or (Satania.IsBlocked) then Exit;
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
    if Save.SitOnWindow then
    begin
      Save.SitOnWindow := False;
      FormMain.MenuItemSitOnWindow.Checked := False;
    end;
    if Monitor <> FormMain.Monitor then
    begin
      FormMain.Left := Monitor.Left;
      FormMain.Top := Monitor.Top;
      FormMain.MakeFullyVisible(Monitor);
    end;
    UpdateMonitor;
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
  ShowInTaskBar := stNever;
end;

procedure TFormTouch.TimerBlinkingStartTimer(Sender: TObject);
begin
  BlinkCount := 12;
end;

procedure TFormTouch.TimerBlinkingStopTimer(Sender: TObject);
begin
  BlinkCount := 0;
  AlphaBlendValue := 1;
end;

procedure TFormTouch.TimerBlinkingTimer(Sender: TObject);
begin
  Dec(BlinkCount);
  if AlphaBlendValue = 1 then
    AlphaBlendValue := 255
  else
    AlphaBlendValue := 1;
  if BlinkCount <= 0 then
    TimerBlinking.Enabled := False;
end;


end.

