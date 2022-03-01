unit frame.reminderitem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, Menus, Buttons,
  DateTimePicker, LCLIntf, LCLType;

type

  { TFrameReminderItem }

  TFrameReminderItem = class(TFrame)
    CheckBoxEnable: TCheckBox;
    ComboBoxKind: TComboBox;
    DateTimePicker: TDateTimePicker;
    EditScript: TEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ButtonDelete: TSpeedButton;
    ButtonRun: TSpeedButton;
    procedure ComboBoxKindChange(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
  private

  public

  end;

implementation

uses
  Mcdowell;

{$R *.lfm}

{ TFrameReminderItem }

procedure TFrameReminderItem.ComboBoxKindChange(Sender: TObject);
begin
  case ComboBoxKind.ItemIndex of
    0: DateTimePicker.Kind := dtkTime;
    1: DateTimePicker.Kind := dtkDateTime;
  end;
end;

procedure TFrameReminderItem.ButtonRunClick(Sender: TObject);
begin
  Satania.Action('script', EditScript.Text);
end;

end.

