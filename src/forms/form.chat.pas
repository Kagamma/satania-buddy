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
  CastleURIUtils, LCLTranslator, kmemo, Types, StrUtils, Generics.Collections;

type
  TChatSenderEnum = (
    cseSystem,
    cseSatania,
    cseUser
  );

  TChatHistory = record
    SenderType: TChatSenderEnum;
    Time,
    Message: String;
  end;

  TChatHistoryList = specialize TList<TChatHistory>;

  { TFormChat }

  TFormChat = class(TForm)

    ButtonClear: TBitBtn;
    CheckBoxAlwaysOnTop: TCheckBox;
    EditChat: TMemo;
    MemoChatLog: TKMemo;
    Panel1: TPanel;
    PanelToolbar: TPanel;
    PanelEdit: TPanel;
    PanelChatlog: TPanel;
    Splitter1: TSplitter;

    procedure ButtonClearClick(Sender: TObject);
    procedure CheckBoxAlwaysOnTopChange(Sender: TObject);
    procedure EditChatKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure EditChatKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    ChatHistoryList: TChatHistoryList;
    Typing: TKMemoTextBlock;
    procedure InsertLog(const LogName, Msg: String);
    procedure CalcHeights;
    procedure InsertTyping;
    procedure RemoveTyping;
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
  ChatHistoryList := TChatHistoryList.Create;
  ButtonClearClick(Self);
end;

procedure TFormChat.FormDestroy(Sender: TObject);
begin
  ChatHistoryList.Free;
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
    Satania.Log(Save.Settings.UserName, S);
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

procedure TFormChat.ButtonClearClick(Sender: TObject);
begin
  MemoChatLog.Blocks.Clear;
  ChatHistoryList.Clear;
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
  CH: TChatHistory;
begin
  RemoveTyping;
  DecodeTime(Now, H, M, SS, MS);
  Time := '[' + Format('%.*d', [2, H]) + ':' + Format('%.*d', [2, M]) + ':' + Format('%.*d', [2, SS]) + '] ';

  TB := MemoChatLog.Blocks.AddTextBlock(Time);
  TB.TextStyle.Font.Style := TB.TextStyle.Font.Style + [fsItalic];
  TB.TextStyle.Font.Color := $808080;

  TB := MemoChatLog.Blocks.AddTextBlock(LogName + ': ');
  TB.TextStyle.Font.Style := TB.TextStyle.Font.Style + [fsBold];
  if LogName = 'System' then
  begin
    TB.TextStyle.Font.Color := $800000;
    CH.SenderType := cseSystem;
  end else
  if LogName = Save.Settings.UserName then
  begin
    TB.TextStyle.Font.Color := $008000;
    CH.SenderType := cseSatania;
  end else
  begin
    TB.TextStyle.Font.Color := $0000B0;
    CH.SenderType := cseUser;
  end;

  MsgSplit := SplitString(Msg, #10);
  for I := 0 to High(MsgSplit) do
  begin
    MemoChatLog.Blocks.AddTextBlock(MsgSplit[I]);
    MemoChatLog.Blocks.AddParagraph;
  end;

  CH.Time := Time;
  CH.Message := Msg;

  ChatHistoryList.Add(CH);

  if LogName = Save.Settings.UserName then
    InsertTyping;

  while FormChat.MemoChatLog.Blocks.LineCount > 2000 do
    FormChat.MemoChatLog.Blocks.DeleteLine(0);
  while ChatHistoryList.Count > 2000 do
    ChatHistoryList.Delete(0);
  ScrollToBottom;
end;

procedure TFormChat.InsertTyping;
begin
  RemoveTyping;
  Typing := MemoChatLog.Blocks.AddTextBlock(Satania.Name + ' is typing...');
  Typing.TextStyle.Font.Style := Typing.TextStyle.Font.Style + [fsItalic];
  Typing.TextStyle.Font.Color := $808080;
end;

procedure TFormChat.RemoveTyping;
var
  I: Integer;
begin
  if Self.Typing = nil then
    Exit;
  for I := 0 to MemoChatLog.Blocks.Count - 1 do
  begin
    if Typing = MemoChatLog.Blocks[I] then
    begin
      MemoChatLog.Blocks.Delete(I);
      Break;
    end;
  end;
end;

end.

