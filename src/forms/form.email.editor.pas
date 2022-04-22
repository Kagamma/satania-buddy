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

unit form.email.editor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Buttons, cssbase, CSSCtrls, kmemo, Types, StrUtils;

type
  { TFormEmailEditor }

  TFormEmailEditor = class(TForm)
    ButtonSend: TBitBtn;
    CSSShape: TCSSShape;
    EditSubject: TEdit;
    EditMailTo: TEdit;
    Memo: TKMemo;
    PanelSubject: TPanel;
    PanelBody: TPanel;
    PanelMain: TPanel;
    ToolBarMemo: TToolBar;
    procedure ButtonSendClick(Sender: TObject);
    procedure EditMailToKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FTagContainerMailTo: THtmlNode;
  public
    procedure RemoveInputResult(Sender: TObject);
    procedure AddInputResult(const AParent: THtmlNode; const AText: String);
  end;

var
  FormEmailEditor: TFormEmailEditor;

implementation

{$R *.lfm}

uses
  Mcdowell,
  Mcdowell.smtp;

procedure TFormEmailEditor.FormCreate(Sender: TObject);
var
  Dummy: THtmlNode;
begin
  DisableAlign;

  Dummy := HTMLDiv('display:flex;margin-bottom:4px')
    .AppendTo(CSSShape.Body);        
  FTagContainerMailTo := HTMLDiv('').AppendTo(Dummy);
  HTMLDiv('display:inline-block;padding-right:4px').AppendTo(FTagContainerMailTo).SetAlignControl(EditMailTo);
  HTMLDiv('').AppendTo(CSSShape.Body).SetAlignControl(PanelSubject);

  EnableAlign;
end;

procedure TFormEmailEditor.EditMailToKeyPress(Sender: TObject; var Key: char);
var
  S: String;
begin
  if (Key = #13) and (EditMailTo.Text <> '') then
  begin  
    for S in SplitString(EditMailTo.Text, ';') do
      AddInputResult(FTagContainerMailTo, S);
    EditMailTo.Text := '';
    CSSShape.Changed;
  end;
end;

procedure TFormEmailEditor.ButtonSendClick(Sender: TObject);
var
  I: Integer;
  MailTo: String;
  Node: THtmlNode;
  C: Char = #13;
  SataniaSMTP: TSataniaSMTP;
begin
  //
  if not TSataniaSMTP.IsEmailConfigured then
  begin
    Satania.Talk('Please config you email first in <font color="#0000ff">Settings</font>');
    Exit;
  end;
  // Adds remaining text to list
  EditMailToKeyPress(Self, C);
  MailTo := '';
  // Get all emails
  Node := FTagContainerMailTo.FirstChild;  
  Node := Node.GetNext(Node);
  while Node <> nil do
  begin
    MailTo := MailTo + Node.FirstChild.Text;
    Node := Node.GetNext(Node);
    if Node <> nil then
      MailTo := MailTo + ';';
  end;
  // Verify
  if (MailTo = '') or (EditSubject.Text = '') then
  begin
    Satania.Talk('Please make sure MailTo and Subject are filled');
    Exit;
  end;
  // Send    
  Satania.Talk('Sending...');
  SataniaSMTP := TSataniaSMTP.Create;
  SataniaSMTP.MailTo := MailTo;
  SataniaSMTP.Subject := EditSubject.Text;
  SataniaSMTP.Body := Memo.Text;
end;

procedure TFormEmailEditor.FormShow(Sender: TObject);
begin
  FTagContainerMailTo.Clear;       
  HTMLDiv('display:inline-block;padding-right:4px').AppendTo(FTagContainerMailTo).SetAlignControl(EditMailTo);
end;       

procedure TFormEmailEditor.RemoveInputResult(Sender: TObject);
begin
  THtmlNode(Sender).ParentNode.ParentNode.Delete(THtmlNode(Sender).ParentNode);
end;

procedure TFormEmailEditor.AddInputResult(const AParent: THtmlNode; const AText: String);
var
  Dummy: THtmlNode;
begin     
  Dummy := HTMLDiv('display:inline-block;padding-right:4px;')
    .AppendTo(AParent);
  HTMLSpan('padding:4px;color:black;background-color:#DFE3E8', AText)
    .AppendTo(Dummy);
  HTMLSpan('padding:4px;color:black;background-color:#C6CDD6;border-bottom-right-radius:4px', 'X')
    .SetHover('color:white;background-color:#EA5E60;')
    .AppendTo(Dummy)
    .SetOnClick(@RemoveInputResult);
end;

end.

