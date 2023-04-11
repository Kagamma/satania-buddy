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
    CheckBoxAlwaysOnTop: TCheckBox;
    EditChat: TMemo;
    MemoChatLog: TKMemo;
    Panel1: TPanel;
    PanelToolbar: TPanel;
    PanelEdit: TPanel;
    PanelChatlog: TPanel;
    Splitter1: TSplitter;
    ChatHistory: TStringList;
    procedure BttonClearClick(Sender: TObject);
    procedure CheckBoxAlwaysOnTopChange(Sender: TObject);
    procedure EditChatKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure EditChatKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure InsertLog(const LogName, Msg: String);
    procedure CalcHeights;
  end;

var
  FormChat: TFormChat;

implementation

{$R *.lfm}

uses
  Math,
  Globals,
  Mcdowell;

{ TFormChat }

procedure TFormChat.CalcHeights;
begin
  EditChat.Height := 46;
  PanelEdit.Height := 71;
end;

procedure TFormChat.FormCreate(Sender: TObject);
begin
  Self.CalcHeights;
  ChatHistory := TStringList.Create;
end;

procedure TFormChat.FormDestroy(Sender: TObject);
begin
  ChatHistory.Free;
end;

procedure TFormChat.FormShow(Sender: TObject);
begin
  EditChat.SetFocus;
end;

procedure TFormChat.EditChatKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  S: String;
begin
  Self.CalcHeights;
  if (Key = 13) and not (Shift = [ssShift]) and (FormChat.EditChat.Lines.Text<>'') then
  begin
    S := Trim(FormChat.EditChat.Lines.Text);
    Satania.Log('(You)', S);
    Satania.Chat(S);
    EditChat.Lines.Clear;
  end;
  if Key = 27 then
  begin
    Hide;
  end;
end;

procedure TFormChat.EditChatKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Self.CalcHeights;
  if Trim(FormChat.EditChat.Lines.Text) = '' then
    FormChat.EditChat.Clear;
end;

procedure TFormChat.BttonClearClick(Sender: TObject);
begin
  MemoChatLog.Blocks.Clear;
  ChatHistory.Clear;
end;

procedure TFormChat.CheckBoxAlwaysOnTopChange(Sender: TObject);
begin
  if CheckBoxAlwaysOnTop.Checked then
    Self.FormStyle := fsSystemStayOnTop
  else
    Self.FormStyle := fsStayOnTop;
end;

procedure TFormChat.InsertLog(const LogName, Msg: String);
  procedure ScrollToBottom;
  begin
    MemoChatLog.SelStart := MemoChatLog.GetTextLen;
    MemoChatLog.SelLength := 0;
    // 999999 should be more than enough to scroll it to the bottom
    MemoChatLog.ScrollBy(0, 999999, False);
    MemoChatLog.Refresh;
  end;

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
    'System': TB.TextStyle.Font.Color := $800000;
    '(You)': TB.TextStyle.Font.Color := $008000;
    else TB.TextStyle.Font.Color := $0000B0;
  end;

  TB := MemoChatLog.Blocks.AddTextBlock(Time);
  TB.TextStyle.Font.Style := TB.TextStyle.Font.Style + [fsItalic];
  TB.TextStyle.Font.Color := $808080;

  ChatHistory.Add(LogName + ': ' + Msg);

  while FormChat.MemoChatLog.Blocks.LineCount > 2000 do
    FormChat.MemoChatLog.Blocks.DeleteLine(0);
  while ChatHistory.Count > 2000 do
    ChatHistory.Delete(0);
  ScrollToBottom;
end;

end.

