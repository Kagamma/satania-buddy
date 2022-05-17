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

unit Form.ChatBubble;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TFormChatBubble }

  TFormChatBubble = class(TForm)
    AskText: TLabel;
    Panel1: TPanel;
    Panel: TPanel;
    Panel3: TPanel;
    PanelButton: TPanel;
    procedure ButtonAnswerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PanelPaint(Sender: TObject);
  private

  public
    Answer: Integer;                         
    procedure ClearButtons;
    procedure AddButton(const ButtonName: String; const T: Integer);
  end;

var
  FormChatBubble: TFormChatBubble;

implementation

{$R *.lfm}

uses
  mcdowell,
  Utils.ActiveWindow;

{ TFormChatBubble }

procedure TFormChatBubble.FormCreate(Sender: TObject);
begin
  AddFormToIgnoreHandleList(Self);
end;

procedure TFormChatBubble.PanelPaint(Sender: TObject);
begin
  Panel.Canvas.Pen.Color := clBlack;
  Panel.Canvas.Rectangle(0, 0, Panel.Width - 1, Panel.Height - 1);
end;

procedure TFormChatBubble.ButtonAnswerClick(Sender: TObject);
begin
  Satania.IsBlocked := False;
  Self.Visible := False;         
  Satania.ChatText.Text.Text := '';
  Answer := (Sender as TButton).Tag;
end;

procedure TFormChatBubble.ClearButtons;
begin
  while PanelButton.ControlCount > 0 do
    PanelButton.RemoveControl(PanelButton.Controls[0]);
end;

procedure TFormChatBubble.AddButton(const ButtonName: String; const T: Integer);
var
  B: TButton;
begin
  B := TButton.Create(Self);
  B.Caption := ButtonName;
  B.Tag := T;
  B.Align := alLeft;
  B.BorderSpacing.Left := 4;
  B.OnClick := @ButtonAnswerClick;
  PanelButton.InsertControl(B);
end;

end.

