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

unit form.reminders;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, StrUtils, DateTimePicker, LCLIntf;

type

  { TFormReminders }

  TFormReminders = class(TForm)
    ButtonCancel: TBitBtn;
    ButtonAddNew1: TBitBtn;
    ButtonCancel1: TBitBtn;
    ButtonSave: TBitBtn;
    Panel1: TPanel;
    ScrollBoxReminders: TScrollBox;
    procedure ButtonCancel1Click(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToolButtonAddNewClick(Sender: TObject);
  private
    procedure DoDelete(Sender: TObject);
    function AddFrame: TFrame;
  public

  end;

var
  FormReminders: TFormReminders;

implementation

{$R *.lfm}

uses
  globals,
  Utils.Encdec,
  mcdowell,
  frame.reminders.item;

{ TFormReminders }

function TFormReminders.AddFrame: TFrame;
var
  Frame: TFrameRemindersItem;
begin
  Frame := TFrameRemindersItem.Create(Self);
  Frame.Name := GUIDName;
  Frame.ButtonDelete.OnClick := @DoDelete;
  Frame.DateTimePicker.Date := Now;
  Frame.DateTimePicker.Time := EncodeTime(0, 0, 0, 0);
  {$ifndef WINDOWS}
  Frame.MemoScript.Font.Name := 'Liberation Mono';
  Frame.MemoScript.Font.Quality := fqAntialiased;
  Frame.MemoScript.Font.Height := 16;
  {$endif}
  ScrollBoxReminders.InsertControl(Frame);
  Frame.Align := alTop;
  Result := Frame;
end;

procedure TFormReminders.ToolButtonAddNewClick(Sender: TObject);
begin
  AddFrame;
end;

procedure TFormReminders.ButtonSaveClick(Sender: TObject);
var
  I: Integer;
  A, B, C, D: Word;
  Frame: TFrameRemindersItem;
  Item: TReminderCollectionItem;
begin
  Save.Reminders.Clear;
  for I := 0 to ScrollBoxReminders.ControlCount - 1 do
  begin
    Frame := ScrollBoxReminders.Controls[I] as TFrameRemindersItem;
    Item := Save.Reminders.Add as TReminderCollectionItem;
    Item.Kind := Frame.ComboBoxKind.ItemIndex;
    if Item.Kind = 1 then
    begin
      DecodeDate(Frame.DateTimePicker.Date, A, B, C);
      Item.Year := A;
      Item.Month := B;
      Item.Day := C;
    end;
    DecodeTime(Frame.DateTimePicker.Time, A, B, C, D);
    Item.Hour := A;
    Item.Minute := B;
    Item.Enabled := Frame.CheckBoxEnable.Checked;
    Item.Monday := Frame.CheckBoxMonday.Checked;
    Item.Tuesday := Frame.CheckBoxTuesday.Checked;
    Item.Wednesday := Frame.CheckBoxWednesday.Checked;
    Item.Thursday := Frame.CheckBoxThursday.Checked;
    Item.Friday := Frame.CheckBoxFriday.Checked;
    Item.Saturday := Frame.CheckBoxSaturday.Checked;
    Item.Sunday := Frame.CheckBoxSunday.Checked;
    Item.Name := GUIDName;
    Item.Script := Frame.MemoScript.Lines.Text;
    Save.SaveToFile('configs.json');
  end;
  Satania.UsedRemindersList.Clear;
end;

procedure TFormReminders.ButtonCancelClick(Sender: TObject);
begin
  Hide;
end;

procedure TFormReminders.ButtonCancel1Click(Sender: TObject);
begin
  OpenURL('https://github.com/Kagamma/satania-buddy/wiki/Alarms-and-Reminders');
end;

procedure TFormReminders.FormShow(Sender: TObject);
var
  I, J: Integer;
  Frame: TFrameRemindersItem;
  A: array of TReminderCollectionItem;
  Item, Item2: TReminderCollectionItem;
  S1, S2: QWord;
begin
  SetLength(A, Save.Reminders.Count);
  for I := 0 to Save.Reminders.Count - 1 do
    A[I] := TReminderCollectionItem(Save.Reminders.Items[I]);
  if Save.Reminders.Count > 1 then
  begin
    for I := 0 to Save.Reminders.Count - 2 do
    begin
      Item := A[I];
      for J := I + 1 to Save.Reminders.Count - 1 do
      begin
        Item2 := A[J];
        S1 := StrToQWord(Format('%d%.4d%.2d%.2d%.2d%.2d', [Item.Kind, Item.Year, Item.Month, Item.Day, Item.Hour, Item.Minute]));
        S2 := StrToQWord(Format('%d%.4d%.2d%.2d%.2d%.2d', [Item2.Kind, Item2.Year, Item2.Month, Item2.Day, Item2.Hour, Item2.Minute]));
        if S1 < S2 then
        begin
          Item := Item2;
          Item2 := A[J];
          A[J] := A[I];
          A[I] := Item2;
        end;
      end;
    end;
  end;
  for I := ScrollBoxReminders.ControlCount - 1 downto 0 do
  begin
    ScrollBoxReminders.RemoveControl(ScrollBoxReminders.Controls[I]);
  end;
  for I := 0 to Save.Reminders.Count - 1 do
  begin
    Frame := AddFrame as TFrameRemindersItem;
    Item := A[I];
    Frame.ComboBoxKind.ItemIndex := Item.Kind;
    if Item.Kind = 1 then
    begin
      Frame.DateTimePicker.Date := EncodeDate(Item.Year, Item.Month, Item.Day);
      Frame.DateTimePicker.Kind := dtkDateTime;
      Frame.PanelDate.Enabled := False;
    end;
    Frame.DateTimePicker.Time := EncodeTime(Item.Hour, Item.Minute, 0, 0);
    Frame.CheckBoxEnable.Checked := Item.Enabled;
    Frame.MemoScript.Lines.Text := Item.Script;

    Frame.CheckBoxMonday.Checked := Item.Monday;
    Frame.CheckBoxTuesday.Checked := Item.Tuesday;
    Frame.CheckBoxWednesday.Checked := Item.Wednesday;
    Frame.CheckBoxThursday.Checked := Item.Thursday;
    Frame.CheckBoxFriday.Checked := Item.Friday;
    Frame.CheckBoxSaturday.Checked := Item.Saturday;
    Frame.CheckBoxSunday.Checked := Item.Sunday;
  end;
end;

procedure TFormReminders.DoDelete(Sender: TObject);
begin
  ScrollBoxReminders.RemoveControl(TWinControl(Sender).Parent.Parent);
end;

end.

