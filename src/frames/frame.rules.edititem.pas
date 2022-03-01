unit frame.rules.edititem;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, Buttons;

type

  { TFrameRulesEditItem }

  TFrameRulesEditItem = class(TFrame)
    EditText: TEdit;
    ButtonDelete: TSpeedButton;
    ButtonRun: TSpeedButton;
    procedure ButtonRunClick(Sender: TObject);
  private

  public

  end;

implementation

uses
  Mcdowell;

{$R *.lfm}

{ TFrameRulesEditItem }

procedure TFrameRulesEditItem.ButtonRunClick(Sender: TObject);
begin
  Satania.Action('script', EditText.Text);
end;

end.

