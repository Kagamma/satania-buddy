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

unit form.rules;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  LCLIntf, StdCtrls;

type

  { TFormRules }

  TFormRules = class(TForm)
    ButtonAddRule: TBitBtn;
    ButtonHelp: TBitBtn;
    ButtonLearnRules: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonSave: TBitBtn;
    EditSearch: TEdit;
    Panel1: TPanel;
    ScrollBoxRules: TScrollBox;
    procedure ButtonAddRuleClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonHelpClick(Sender: TObject);
    procedure ButtonLearnRulesClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure EditSearchChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    IsSaved: Boolean;
    procedure DoDeleteRule(Sender: TObject);
  public

  end;

var
  FormRules: TFormRules;

implementation

{$R *.lfm}

uses
  globals,
  mcdowell,
  mcdowell.chatbot,  
  mcdowell.chatbot.train,
  frame.rules.item,
  frame.rules.edititem,
  fpjson, jsonparser,
  generics.collections;

procedure TFormRules.DoDeleteRule(Sender: TObject);
begin
  ScrollBoxRules.RemoveControl(TWinControl(Sender).Parent.Parent);
end;

procedure TFormRules.ButtonCancelClick(Sender: TObject);
begin
  Hide;
end;

procedure TFormRules.ButtonHelpClick(Sender: TObject);
begin
  OpenURL('https://github.com/Kagamma/satania-buddy/wiki/Rules');
end;

procedure TFormRules.ButtonLearnRulesClick(Sender: TObject);
begin
  ButtonSaveClick(Self);
  if IsSaved then
    RunTrain;
end;

procedure TFormRules.ButtonSaveClick(Sender: TObject);
var
  JSONArray, JSONArraySub: TJSONArray;
  JSONItem: TJSONObject;
  Frame: TFrameRulesItem;
  EditFrame: TFrameRulesEditItem;
  FS: TStringList;
  I, J: Integer;
begin     
  JSONArray := TJSONArray.Create;
  IsSaved := False;
  try
    try
      for J := 0 to ScrollBoxRules.ControlCount - 1 do
      begin
        Frame := TFrameRulesItem(ScrollBoxRules.Controls[J]);
        JSONItem := TJSONObject.Create;
        Frame.EditTag.Text := Trim(Frame.EditTag.Text);
        if Frame.EditTag.Text = '' then
          continue;
        JSONItem.Add('tag', Frame.EditTag.Text);
        JSONArraySub := TJSONArray.Create;
        for I := 0 to Frame.ScrollBoxPatterns.ControlCount - 1 do
        begin
          EditFrame := TFrameRulesEditItem(Frame.ScrollBoxPatterns.Controls[I]);
          JSONArraySub.Add(Trim(EditFrame.EditText.Text));
        end;
        JSONItem.Add('patterns', JSONArraySub);      
        JSONArraySub := TJSONArray.Create;
        for I := 0 to Frame.ScrollBoxResponses.ControlCount - 1 do
        begin
          EditFrame := TFrameRulesEditItem(Frame.ScrollBoxResponses.Controls[I]);
          JSONArraySub.Add(Trim(EditFrame.EditText.Text));
        end;
        JSONItem.Add('responses', JSONArraySub);
        JSONArray.Add(JSONItem);
      end;
      FS := TStringList.Create;
      FS.Text := JSONArray.AsJSON;
      FS.SaveToFile('data/nn/chatbot/rules.json');
      FS.Free;       
      ReadRules;
      IsSaved := True;
    except
      on E: Exception do
        Satania.Talk(E.Message);
    end;
  finally
    FreeAndNil(JSONArray);
  end;
end;

procedure TFormRules.EditSearchChange(Sender: TObject);
var
  I: Integer;
  S: String;
  Frame: TFrameRulesItem;
begin
  S := LowerCase(EditSearch.Text);
  for I := 0 to ScrollBoxRules.ControlCount - 1 do
  begin
    Frame := TFrameRulesItem(ScrollBoxRules.Controls[I]);
    if (S = '') or (LowerCase(Frame.EditTag.Text).IndexOf(S) >= 0) then
    begin
      Frame.Show;
    end else
    begin
      Frame.Hide;
    end;
  end;
end;

procedure TFormRules.FormShow(Sender: TObject);
var
  Frame: TFrameRulesItem;
  EditFrame: TFrameRulesEditItem;
  KeyList: TStringList;
  Rule: TRuleRec;
  I, J: Integer;
  key: String;
begin
  ReadRules;                    
  for I := ScrollBoxRules.ControlCount - 1 downto 0 do
    ScrollBoxRules.RemoveControl(ScrollBoxRules.Controls[I]);
  KeyList := TStringList.Create;
  KeyList.Sorted := True;
  for Key in RuleDict.Keys do
    KeyList.Add(Key);
  for I := KeyList.Count - 1 downto 0 do
  begin
    Key := KeyList[I];
    Rule := RuleDict[Key];
    Frame := TFrameRulesItem.Create(Self);
    Frame.Name := GUIDName;
    Frame.EditTag.Text := Key;
    Frame.ButtonDelete.OnClick := @DoDeleteRule;
    for J := 0 to Length(Rule.Patterns) - 1 do
    begin
      Frame.AddPattern(Rule.Patterns[J]);
    end;                       
    for J := 0 to Length(Rule.Responses) - 1 do
    begin
      Frame.AddResponse(Rule.Responses[J]);
    end;
    ScrollBoxRules.InsertControl(Frame);
  end;
  KeyList.Free;
end;

procedure TFormRules.ButtonAddRuleClick(Sender: TObject);
var
  Frame: TFrameRulesItem;
begin
  Frame := TFrameRulesItem.Create(Self);
  Frame.Name := GUIDName;         
  Frame.ButtonDelete.OnClick := @DoDeleteRule;
  ScrollBoxRules.InsertControl(Frame, 0);
end;

end.

