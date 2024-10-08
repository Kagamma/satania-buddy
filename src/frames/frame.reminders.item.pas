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

unit frame.reminders.item;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Menus, Buttons,
  DateTimePicker, LCLIntf, LCLType;

type

  { TFrameRemindersItem }

  TFrameRemindersItem = class(TFrame)
    CheckBoxSunday: TCheckBox;
    CheckBoxEnable: TCheckBox;
    CheckBoxMonday: TCheckBox;
    CheckBoxSaturday: TCheckBox;
    CheckBoxFriday: TCheckBox;
    CheckBoxThursday: TCheckBox;
    CheckBoxWednesday: TCheckBox;
    CheckBoxTuesday: TCheckBox;
    ComboBoxKind: TComboBox;
    DateTimePicker: TDateTimePicker;
    EditTag: TEdit;
    LabelAlarmTime: TLabel;
    LabelName: TLabel;
    LabelAlarmType: TLabel;
    MemoScript: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ButtonDelete: TSpeedButton;
    ButtonRun: TSpeedButton;
    PanelDate: TPanel;
    procedure ComboBoxKindChange(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
  private

  public

  end;

implementation

uses
  Mcdowell;

{$R *.lfm}

{ TFrameRemindersItem }

procedure TFrameRemindersItem.ComboBoxKindChange(Sender: TObject);
begin
  case ComboBoxKind.ItemIndex of
    0:
      begin
        DateTimePicker.Kind := dtkTime;
        PanelDate.Enabled := True;
      end;
    1:
      begin
        DateTimePicker.Kind := dtkDateTime;
        PanelDate.Enabled := False;
      end;
  end;
end;

procedure TFrameRemindersItem.ButtonRunClick(Sender: TObject);
begin
  Satania.Action('script', MemoScript.Lines.Text);
end;

end.

