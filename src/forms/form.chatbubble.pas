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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  HtmlView, HTMLUn2, HtmlGlobals;

type

  { TFormChatBubble }

  TFormChatBubble = class(TForm)
    AskText: THtmlViewer;
    Panel: TPanel;
    Panel1: TPanel;
    procedure AskTextFormSubmit(Sender: TObject; const Act, Target, EncType,
      Method: ThtString; Results: ThtStringList);
    procedure AskTextHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure PanelPaint(Sender: TObject);
  private

  public
    Answer: String;
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
  Panel.Canvas.Rectangle(0, 0, Panel.Width, Panel.Height);
end;

procedure TFormChatBubble.AskTextHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: Boolean);
begin
  Satania.IsBlocked := False;
  Self.Visible := False;
  Satania.ChatText.Text.Text := '';
  Answer := SRC;
end;

procedure TFormChatBubble.AskTextFormSubmit(Sender: TObject; const Act,
  Target, EncType, Method: ThtString; Results: ThtStringList);
begin
  Satania.IsBlocked := False;
  Self.Visible := False;
  Satania.ChatText.Text.Text := '';
  Answer := Results.Text;
end;

end.

