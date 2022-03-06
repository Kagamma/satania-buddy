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

