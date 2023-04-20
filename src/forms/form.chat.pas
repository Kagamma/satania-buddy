{

satania-buddy
Copyright (C) 2022-2023 kagamma

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
  CastleURIUtils, LCLTranslator, kmemo, Types, StrUtils, Generics.Collections,
  kgraphics, FileUtil,
  Mcdowell.RichText;

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
    ComboBoxService: TComboBox;
    EditChat: TMemo;
    MemoChatLog: TKMemo;
    Panel1: TPanel;
    PanelToolbar: TPanel;
    PanelEdit: TPanel;
    PanelChatlog: TPanel;
    ButtonOpenService: TSpeedButton;
    Splitter1: TSplitter;

    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonOpenServiceClick(Sender: TObject);
    procedure CheckBoxAlwaysOnTopChange(Sender: TObject);
    procedure ComboBoxServiceChange(Sender: TObject);
    procedure EditChatKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure EditChatKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FRichText: TRichText;
    FStreamingPartCount: Integer;
    procedure ScrollToBottom;
  public
    ChatHistoryList: TChatHistoryList;
    Typing: TKMemoTextBlock;
    procedure EnableStreaming;
    procedure DisableStreaming;
    procedure Streaming(const S: String);
    procedure InsertLog(const LogName, Msg: String);
    procedure CalcHeights;
    procedure InsertTyping;
    procedure RemoveTyping;
    procedure LoadServiceList;
  end;

var
  FormChat: TFormChat;

implementation

{$R *.lfm}

uses
  Math,
  Globals,
  form.tool.evilceditor,
  Mcdowell;

{ TFormChat }

procedure TFormChat.EnableStreaming;
begin
  FRichText.IsStreaming := True;
  FStreamingPartCount := 0;
end;

procedure TFormChat.DisableStreaming;
begin
  if FRichText.IsStreaming then
  begin
    FRichText.IsStreaming := False;
    FStreamingPartCount := 1;
    RemoveTyping;
    MemoChatLog.Blocks.AddParagraph;
  end;
end;

procedure TFormChat.CalcHeights;
begin
  //EditChat.Height := 46;
  //PanelEdit.Height := 71;
end;

procedure TFormChat.FormCreate(Sender: TObject);
begin
  Self.CalcHeights;
  ChatHistoryList := TChatHistoryList.Create;
  ButtonClearClick(Self);
  FRichText := TRichText.Create;
  LoadServiceList;
end;

procedure TFormChat.FormDestroy(Sender: TObject);
begin
  ChatHistoryList.Free;
  FRichText.Free;
end;

procedure TFormChat.LoadServiceList;
var
  SL: TStringList;
  I : Integer;
  S : String;
begin
  ComboBoxService.Clear;
  ComboBoxService.Items.Add('None');
  ComboBoxService.ItemIndex := 0;

  SL := TStringList.Create;
  FindAllFiles(SL, 'data/scripts/' + Save.Settings.Skin + '/services', '*.evil', False);
  for I := 0 to SL.Count - 1 do
  begin
    S := ExtractFileName(SL[I]);
    ComboBoxService.Items.Add(S);
    if S.IndexOf(Save.Settings.LastServiceUsed) >= 0 then
      ComboBoxService.ItemIndex := I + 1;
  end;
  SL.Free;
end;

procedure TFormChat.FormShow(Sender: TObject);
begin
  EditChat.SetFocus;
  // Load list of services
  LoadServiceList;
  Self.ButtonOpenService.Enabled := Self.ComboBoxService.ItemIndex > 0;
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

procedure TFormChat.ButtonOpenServiceClick(Sender: TObject);
var
  I: Integer;
begin
  I := Self.ComboBoxService.ItemIndex;
  if I > 0 then
  begin
    FormEvilCEditor.Show;
    FormEvilCEditor.LoadFromFile('data/scripts/' + Save.Settings.Skin + '/services/' + Self.ComboBoxService.Items[I]);
  end;
end;

procedure TFormChat.CheckBoxAlwaysOnTopChange(Sender: TObject);
begin
  if CheckBoxAlwaysOnTop.Checked then
    Self.FormStyle := fsSystemStayOnTop
  else
    Self.FormStyle := fsStayOnTop;
end;

procedure TFormChat.ComboBoxServiceChange(Sender: TObject);
begin
  EditChat.SetFocus;
  if Self.ComboBoxService.ItemIndex > 0 then
  begin
    Self.ButtonOpenService.Enabled := True;
    Save.Settings.LastServiceUsed := Self.ComboBoxService.Items[Self.ComboBoxService.ItemIndex];
  end else
  begin
    Self.ButtonOpenService.Enabled := False;
    Save.Settings.LastServiceUsed := 'None';
  end;
  Save.SaveToFile('configs.json');
end;

procedure TFormChat.ScrollToBottom;
begin
  MemoChatLog.SelStart := MemoChatLog.GetTextLen;
  MemoChatLog.SelLength := 0;
  // 999999 should be more than enough to scroll it to the bottom
  MemoChatLog.ScrollBy(0, 999999, False);
  MemoChatLog.Refresh;
end;

procedure TFormChat.Streaming(const S: String);
begin
  InsertLog(Satania.Name, S);
end;

procedure TFormChat.InsertLog(const LogName, Msg: String);
var
  H, M, SS, MS: Word;
  TB: TKMemoTextBlock;
  Time: String;
  MsgSplit: TStringDynArray;
  CodeMode: Boolean = False;
  I: Integer;
  CH: TChatHistory;
begin
  DecodeTime(Now, H, M, SS, MS);
  Time := Format('%.*d', [2, H]) + ':' + Format('%.*d', [2, M]) + ':' + Format('%.*d', [2, SS]);

  RemoveTyping;
  if (FRichText.IsStreaming and (FStreamingPartCount = 0)) or (not FRichText.IsStreaming) then
  begin
    MemoChatLog.Blocks.AddParagraph;
    TB := MemoChatLog.Blocks.AddTextBlock(LogName);
    TB.TextStyle.Font.Style := TB.TextStyle.Font.Style + [fsBold];
    if LogName = 'System' then
    begin
      TB.TextStyle.Font.Color := $800000;
      CH.SenderType := cseSystem;
      TB := MemoChatLog.Blocks.AddTextBlock(' [' + Time + ']');
      TB.TextStyle.Font.Style := TB.TextStyle.Font.Style + [fsItalic];
      TB.TextStyle.Font.Color := $808080;
    end else
    if LogName = Save.Settings.UserName then
    begin
      TB.TextStyle.Font.Color := $008000;
      CH.SenderType := cseUser;
    end else
    begin
      TB.TextStyle.Font.Color := $0000B0;
      CH.SenderType := cseSatania;
    end;
    MemoChatLog.Blocks.AddParagraph;
    FRichText.Reset;
  end;

  FRichText.Source := Msg;
  FRichText.Lex;
  FRichText.NextTokenPos := FRichText.TokenList.Count - 1;
  FRichText.Parse(MemoChatLog);

  if (not FRichText.IsStreaming) or (FStreamingPartCount = 0) then
  begin
    CH.Time := Time;
    CH.Message := Msg;
    ChatHistoryList.Add(CH);
  end;
  if (FRichText.IsStreaming) and (FStreamingPartCount > 0) then
  begin
    CH := ChatHistoryList[ChatHistoryList.Count - 1];
    CH.Message := Msg;
    ChatHistoryList[ChatHistoryList.Count - 1] := CH;
  end;

  if not FRichText.IsStreaming then
    MemoChatLog.Blocks.AddParagraph;

  if (LogName = Save.Settings.UserName) or (FRichText.IsStreaming) then
  begin
    InsertTyping;
  end;

  while FormChat.MemoChatLog.Blocks.LineCount > 2000 do
    FormChat.MemoChatLog.Blocks.DeleteLine(0);
  while ChatHistoryList.Count > 2000 do
    ChatHistoryList.Delete(0);
  ScrollToBottom;
  Inc(FStreamingPartCount);
end;

procedure TFormChat.InsertTyping;
begin
  RemoveTyping;
  Typing := MemoChatLog.Blocks.AddTextBlock(' ' + Satania.Name + ' is typing...');
  Typing.TextStyle.Font.Style := Typing.TextStyle.Font.Style + [fsItalic];
  Typing.TextStyle.Font.Color := $818181;
end;

procedure TFormChat.RemoveTyping;
begin
  if Self.Typing = nil then
    Exit;
  if MemoChatLog.Blocks.Count <> 0 then
    MemoChatLog.Blocks.Delete(MemoChatLog.Blocks.Count - 1);
  Self.Typing := nil;
end;

end.

