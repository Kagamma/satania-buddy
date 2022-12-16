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
  CastleURIUtils, LCLTranslator, kmemo, Types, StrUtils;

type

  { TFormChat }

  TFormChat = class(TForm)
    BttonClear: TBitBtn;
    EditChat: TEdit;
    MemoChatLog: TKMemo;
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
    Satania.Log('(You)', FormChat.EditChat.Text);
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
  MsgSplit: TStringDynArray;
  I: Integer;
begin
  DecodeTime(Now, H, M, SS, MS);
  Time := '[' + Format('%.*d', [2, H]) + ':' + Format('%.*d', [2, M]) + ':' + Format('%.*d', [2, SS]) + '] ';

  MemoChatLog.Blocks.AddParagraph(0);

  MsgSplit := SplitString(Msg, #10);
  for I := High(MsgSplit) downto 0 do
  begin
    MemoChatLog.Blocks.AddTextBlock(MsgSplit[I], 0);
    if I > 0 then
      MemoChatLog.Blocks.AddParagraph(0);
  end;

  TB := MemoChatLog.Blocks.AddTextBlock(LogName + ': ', 0);
  TB.TextStyle.Font.Style := TB.TextStyle.Font.Style + [fsBold];
  case LogName of
    'System': TB.TextStyle.Font.Color := $008000;
    '(You)': TB.TextStyle.Font.Color := $800000;
    else TB.TextStyle.Font.Color := $0000B0;
  end;

  TB := MemoChatLog.Blocks.AddTextBlock(Time, 0);
  TB.TextStyle.Font.Style := TB.TextStyle.Font.Style + [fsItalic];
  TB.TextStyle.Font.Color := $808080;

  while FormChat.MemoChatLog.Blocks.LineCount > 2000 do
    FormChat.MemoChatLog.Blocks.DeleteLine(2000);
end;

end.

