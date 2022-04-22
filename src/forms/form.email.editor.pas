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
    EditSubject: TEdit;
    EditMailTo: TEdit;
    Memo: TKMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel5: TPanel;
    PanelBody: TPanel;
    PanelHeader: TPanel;
    PanelMain: TPanel;
    ScrollBoxMailTo: TScrollBox;
    ToolBarMemo: TToolBar;
    procedure ButtonSendClick(Sender: TObject);
    procedure EditMailToKeyPress(Sender: TObject; var Key: char);
    procedure EditReplyToKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FTagContainerReplyTo,
    FTagContainerMailTo: THtmlNode;
    FShapeMailTo,
    FShapeReplyTo: TCSSShape;
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
begin
  DisableAlign;
  FShapeMailTo := TCSSShape.Create(Self);
  FShapeMailTo.Align := alLeft;
  FShapeMailTo.AutoSize := True;

  FShapeReplyTo := TCSSShape.Create(Self);
  FShapeReplyTo.Align := alLeft;
  FShapeReplyTo.AutoSize := True;

  FTagContainerMailTo := HTMLDiv('display:inline-block;padding-bottom:4px')
    .AppendTo(FShapeMailTo.Body);

  FTagContainerReplyTo := HTMLDiv('display:inline-block;padding-bottom:4px')
    .AppendTo(FShapeReplyTo.Body);

  Self.ScrollBoxMailTo.InsertControl(FShapeMailTo);
  Self.ScrollBoxReplyTo.InsertControl(FShapeReplyTo);
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
    FShapeMailTo.Changed;
  end;
end;

procedure TFormEmailEditor.ButtonSendClick(Sender: TObject);
var
  I: Integer;
  MailTos,
  ReplyTos: String;
  Node: THtmlNode;
  C: Char = #13;
begin
  // Adds remaining text to list
  EditMailToKeyPress(Self, C);
  EditReplyToKeyPress(Self, C);
  MailTos := '';
  ReplyTos := '';
  // Get all emails
  Node := FTagContainerMailTo.FirstChild;
  while Node <> nil do
  begin
    MailTos := MailTos + Node.Text;
    Node := Node.GetNext(Node);
    if Node <> nil then
      MailTos := MailTos + ';';
  end;                        
  // Get all emails
  Node := FTagContainerReplyTo.FirstChild;
  while Node <> nil do
  begin
    ReplyTos := ReplyTos + Node.Text;
    Node := Node.GetNext(Node);
    if Node <> nil then
      ReplyTos := ReplyTos + ';';
  end; 
  // Verify
  if (MailTos = '') or (EditSubject.Text = '') then
  begin
    Satania.Talk('Please make sure MailTo and Subject are filled');
    Exit;
  end;
  // Send    
  Satania.Talk(MailTos);
end;

procedure TFormEmailEditor.EditReplyToKeyPress(Sender: TObject; var Key: char);
var
  S: String;
begin
  if (Key = #13) and (EditReplyTo.Text <> '') then
  begin
    for S in SplitString(EditReplyTo.Text, ';') do
      AddInputResult(FTagContainerReplyTo, S);
    EditReplyTo.Text := ''; 
    FShapeReplyTo.Changed;
  end;
end;

procedure TFormEmailEditor.FormShow(Sender: TObject);
begin
end;       

procedure TFormEmailEditor.RemoveInputResult(Sender: TObject);
begin
  THtmlNode(Sender).ParentNode.Delete(THtmlNode(Sender));
end;

procedure TFormEmailEditor.AddInputResult(const AParent: THtmlNode; const AText: String);
var
  TagButton: THtmlNode;
begin
  TagButton := HTMLSpan('margin:2px;color:black;background-color:white;border:1px solid blue;border-radius:3px;padding-right:4px', AText)
    .SetHover('color:white;background-color:red;')
    .AppendTo(AParent)
    .SetOnClick(@RemoveInputResult);
end;

end.

