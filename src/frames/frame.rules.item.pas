unit frame.rules.item;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, Buttons;

type

  { TFrameRulesItem }

  TFrameRulesItem = class(TFrame)
    EditTag: TEdit;
    GroupBoxPatterns: TGroupBox;
    GroupBoxResponses: TGroupBox;
    Label1: TLabel;
    Panel1: TPanel;
    ButtonAddPattern: TSpeedButton;
    ButtonAddResponse: TSpeedButton;
    ScrollBoxResponses: TScrollBox;
    ScrollBoxPatterns: TScrollBox;
    ButtonDelete: TSpeedButton;
    Splitter1: TSplitter;
    procedure ButtonAddPatternClick(Sender: TObject);
    procedure ButtonAddResponseClick(Sender: TObject);
  private
  public
    procedure DoDeletePattern(Sender: TObject);
    procedure DoDeleteResponse(Sender: TObject);
    procedure AddPattern(S: String = '');
    procedure AddResponse(S: String = 'talk("Hello!")');
  end;

implementation

{$R *.lfm}

uses
  Globals,
  Frame.Rules.EditItem;

{ TFrameRulesItem }

procedure TFrameRulesItem.DoDeletePattern(Sender: TObject);
begin
  ScrollBoxPatterns.RemoveControl(TWinControl(Sender).Parent);
end;

procedure TFrameRulesItem.DoDeleteResponse(Sender: TObject);
begin
  ScrollBoxResponses.RemoveControl(TWinControl(Sender).Parent);
end;

procedure TFrameRulesItem.AddPattern(S: String);
var
  Frame: TFrameRulesEditItem;
begin
  Frame := TFrameRulesEditItem.Create(Self);
  Frame.Name := GUIDName;
  Frame.ButtonRun.Hide;
  Frame.ButtonDelete.OnClick := @DoDeletePattern;
  Frame.EditText.Text := S;
  ScrollBoxPatterns.InsertControl(Frame);
end;

procedure TFrameRulesItem.AddResponse(S: String);
var
  Frame: TFrameRulesEditItem;
begin
  Frame := TFrameRulesEditItem.Create(Self);
  Frame.Name := GUIDName;
  Frame.ButtonDelete.OnClick := @DoDeleteResponse;
  Frame.EditText.Text := S;
  ScrollBoxResponses.InsertControl(Frame);
end;

procedure TFrameRulesItem.ButtonAddPatternClick(Sender: TObject);
begin
  AddPattern;
end;

procedure TFrameRulesItem.ButtonAddResponseClick(Sender: TObject);
begin
  AddResponse;
end;

end.

