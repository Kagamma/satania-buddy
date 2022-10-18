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

unit form.chat;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, Process, CastleControls, CastleUIControls,
  CastleURIUtils, LCLTranslator, kmemo;

type

  { TFormChat }

  TFormChat = class(TForm)
    BttonClear: TBitBtn;
    EditChat: TEdit;
    MemoChatLog: TKMemo;
    MemoChatLogOld: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelChatlog: TPanel;
    ButtonHistory: TSpeedButton;
    procedure BttonClearClick(Sender: TObject);
    procedure ButtonHistoryClick(Sender: TObject);
    procedure EditChatKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
  private
    IsShowHistory: Boolean;
  public
    procedure InsertLog(const LogName, Msg: String);
  end;

var
  FormChat: TFormChat;

implementation

{$R *.lfm}

uses
  Globals,
  Mcdowell;

{ TFormChat }

procedure TFormChat.EditChatKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin
    Satania.Log('(you)', FormChat.EditChat.Text);
    Satania.Chat(EditChat.Text);
    EditChat.Text := '';
  end;
  if Key = #27 then
  begin
    Hide;
  end;
end;

procedure TFormChat.FormCreate(Sender: TObject);
begin
  Height := 45;
  PanelChatlog.Visible := IsShowHistory;
end;

procedure TFormChat.ButtonHistoryClick(Sender: TObject);
begin
  IsShowHistory := not IsShowHistory;
  PanelChatlog.Visible := IsShowHistory;
  if IsShowHistory then
  begin
    Height := 320;
  end else
  begin
    Height := 45;
  end;
end;

procedure TFormChat.BttonClearClick(Sender: TObject);
begin
  MemoChatLog.Blocks.Clear;
end;

procedure TFormChat.InsertLog(const LogName, Msg: String);
var
  H, M, SS, MS: Word;
  TB: TKMemoTextBlock;
  Time: String;
begin
  DecodeTime(Now, H, M, SS, MS);
  Time := '[' + Format('%.*d', [2, H]) + ':' + Format('%.*d', [2, M]) + ':' + Format('%.*d', [2, SS]) + '] ';

  MemoChatLog.Blocks.AddParagraph(0);

  TB := MemoChatLog.Blocks.AddTextBlock(Msg + #10#13, 0);

  TB := MemoChatLog.Blocks.AddTextBlock(LogName + ': ', 0);
  TB.TextStyle.Font.Style := TB.TextStyle.Font.Style + [fsBold];
  case LogName of
    'System': TB.TextStyle.Font.Color := $008000;
    '(you)': TB.TextStyle.Font.Color := $000000;
    else TB.TextStyle.Font.Color := $000080;
  end;

  TB := MemoChatLog.Blocks.AddTextBlock(Time, 0);
  TB.TextStyle.Font.Style := TB.TextStyle.Font.Style + [fsItalic];
  TB.TextStyle.Font.Color := $800000;

  while FormChat.MemoChatLog.Blocks.Count > 10000 do
    FormChat.MemoChatLog.Blocks.Delete(10000);
end;

end.

