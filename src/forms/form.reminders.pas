unit form.reminders;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, Buttons, StrUtils, DateTimePicker;

type

  { TFormReminders }

  TFormReminders = class(TForm)
    ButtonAddNew: TBitBtn;
    ButtonSave: TBitBtn;
    Panel1: TPanel;
    ScrollBoxReminders: TScrollBox;
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
  mcdowell,
  Frame.reminderitem;

{ TFormReminders }

function TFormReminders.AddFrame: TFrame;
var
  Frame: TFrameReminderItem;
begin
  Frame := TFrameReminderItem.Create(Self);
  Frame.Name := GUIDName;
  Frame.ButtonDelete.OnClick := @DoDelete;
  Frame.DateTimePicker.Date := Now;   
  Frame.DateTimePicker.Time := EncodeTime(0, 0, 0, 0);
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
  Frame: TFrameReminderItem;
  Item: TReminderCollectionItem;
begin
  Save.Reminders.Clear;
  for I := 0 to ScrollBoxReminders.ControlCount - 1 do
  begin
    Frame := ScrollBoxReminders.Controls[I] as TFrameReminderItem;
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
    Item.Name := GUIDName;
    Item.Script := Frame.EditScript.Text; 
    Save.SaveToFile('configs.json');
  end;     
  Satania.UsedRemindersList.Clear;
end;

procedure TFormReminders.FormShow(Sender: TObject);
var
  I, J: Integer;
  Frame: TFrameReminderItem;
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
    Frame := AddFrame as TFrameReminderItem;
    Item := A[I];
    Frame.ComboBoxKind.ItemIndex := Item.Kind;
    if Item.Kind = 1 then
    begin
      Frame.DateTimePicker.Date := EncodeDate(Item.Year, Item.Month, Item.Day);
      Frame.DateTimePicker.Kind := dtkDateTime;
    end;
    Frame.DateTimePicker.Time := EncodeTime(Item.Hour, Item.Minute, 0, 0);
    Frame.CheckBoxEnable.Checked := Item.Enabled;
    Frame.EditScript.Text := Item.Script;
  end;
end;

procedure TFormReminders.DoDelete(Sender: TObject);
begin
  ScrollBoxReminders.RemoveControl(TWinControl(Sender).Parent.Parent);
end;

end.

