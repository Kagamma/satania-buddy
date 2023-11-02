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
  ExtCtrls, Process, CastleControls, CastleUIControls, CastleURIUtils,
  LCLTranslator, ComCtrls, Menus, kmemo, Types, StrUtils, Generics.Collections,
  kgraphics, FileUtil, Mcdowell.RichText, Mcdowell.Chat.History, AnchorDocking,
  WebUI;

type
  { TFormChat }

  TFormChat = class(TForm)
    ButtonEditCancel: TBitBtn;

    ButtonEditSave: TBitBtn;
    ButtonRefreshService: TSpeedButton;
    CheckBoxAlwaysOnTop: TCheckBox;
    ComboBoxService: TComboBox;
    EditChat: TMemo;
    LabelEditMode: TLabel;
    MemoEdit: TMemo;
    MemoChatLog: TKMemo;
    MenuItemShowWebUI: TMenuItem;
    MenuItemStopGenerating: TMenuItem;
    MenuItemClearHistory: TMenuItem;
    MenuItemEditMode: TMenuItem;
    PageControl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelToolbar: TPanel;
    PanelEdit: TPanel;
    PanelChatlog: TPanel;
    ButtonOpenService: TSpeedButton;
    PopupMenuChat: TPopupMenu;
    Splitter1: TSplitter;
    TabSheetChatEdit: TTabSheet;
    TabSheetChat: TTabSheet;
    ToolBarChat: TToolBar;
    ButtonMenuChat: TToolButton;

    procedure ButtonEditCancelClick(Sender: TObject);
    procedure ButtonEditSaveClick(Sender: TObject);
    procedure ButtonClearClick(Sender: TObject);
    procedure ButtonEditClick(Sender: TObject);
    procedure ButtonOpenServiceClick(Sender: TObject);
    procedure ButtonRefreshServiceClick(Sender: TObject);
    procedure CheckBoxAlwaysOnTopChange(Sender: TObject);
    procedure ComboBoxServiceChange(Sender: TObject);
    procedure EditChatKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure EditChatKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemShowWebUIClick(Sender: TObject);
    procedure MenuItemStopGeneratingClick(Sender: TObject);
  private
    FRichText: TSataniaRichText;
    FStreamingPartCount: Integer;
    FIsWriteToHistoryLog: Boolean;
    procedure ScrollToBottom;
  public
    WebUIHandle: QWord;
    Typing: TKMemoTextBlock;
    ChatHistory: TSataniaChatHistory;
    procedure Send;
    procedure EnableStreaming;
    procedure DisableStreaming;
    procedure Streaming(const S: String);
    procedure InsertLog(const LogName, Msg: String; Time: String = '');
    procedure CalcHeights;
    procedure InsertTyping;
    procedure RemoveTyping;
    procedure LoadServiceList;
    procedure LoadChatHistoryFromFile;
    procedure ApplySettings;              
    procedure ShowWebUI;
    procedure StopGenerating;
    procedure SaveHistory(const HistoryText: String);
    procedure ClearHistory;
    property RichText: TSataniaRichText read FRichText;
  end;

var
  FormChat: TFormChat;

implementation

{$R *.lfm}

uses
  Math,
  Globals,
  form.tool.evilceditor,
  Form.Bubble,
  Utils.Colors,
  Utils.Encdec,
  Mcdowell.Data,
  Mcdowell;

{ TFormChat }

{$I form.chat_webui.inc}

procedure TFormChat.Send;
var
  S: String;
begin
  FormBubble.FinishedTyping := True;
  FormBubble.DisableStreaming;
  S := Trim(FormChat.EditChat.Lines.Text);
  Satania.Log(Save.Settings.UserName, S);
  Satania.Chat(S);
  EditChat.Lines.Clear;
end;

procedure TFormChat.EnableStreaming;
begin
  FRichText.IsStreaming := True;
  FStreamingPartCount := 0;
  Satania.TakeLocalBoundingBoxSnapshot;
  MenuItemStopGenerating.Enabled := True;
end;

procedure TFormChat.DisableStreaming;
begin
  if FRichText.IsStreaming then
  begin
    FRichText.IsStreaming := False;
    FStreamingPartCount := 1;
    RemoveTyping;
    MemoChatLog.Blocks.AddParagraph;
    ChatHistory.SaveLastestMessage;
    MenuItemStopGenerating.Enabled := False;
  end;
end;

procedure TFormChat.CalcHeights;
begin
  //EditChat.Height := 46;
  //PanelEdit.Height := 71;
end;

procedure TFormChat.LoadChatHistoryFromFile;
var
  I: Integer;
  CH: TChatHistoryRec;
begin
  MemoChatLog.Blocks.Clear;
  MemoChatLog.Blocks.LockUpdate;
  ChatHistory.LoadFromFile(GetOSLocalDir + PATH_CHAT_HISTORY + Save.Settings.Skin + '.txt');
  FIsWriteToHistoryLog := False;
  for I := 0 to Self.ChatHistory.List.Count - 1 do
  begin
    CH := Self.ChatHistory.List[I];
    if CH.SenderType = cseSatania then
      InsertLog(Satania.Name, CH.Message, CH.Time)
    else
      InsertLog(Save.Settings.UserName, CH.Message, CH.Time);
    RemoveTyping;
  end;
  FIsWriteToHistoryLog := True;
  MemoChatLog.Blocks.UnLockUpdate;
end;

procedure TFormChat.FormCreate(Sender: TObject);
begin
  Self.CalcHeights;
  ChatHistory := TSataniaChatHistory.Create;
  FRichText := TSataniaRichText.Create;
  LoadServiceList;
  LoadChatHistoryFromFile;
  FIsWriteToHistoryLog := True;
  ApplySettings;
end;

procedure TFormChat.FormDestroy(Sender: TObject);
begin
  FRichText.Free;
  ChatHistory.Free;
end;

procedure TFormChat.LoadServiceList;
var
  SL: TStringList;
  I : Integer;
  S : String;
begin
  PageControl.PageIndex := 0;
  ComboBoxService.Clear;
  ComboBoxService.Items.Add('None');
  ComboBoxService.ItemIndex := 0;

  SL := TStringList.Create;
  FindAllFiles(SL, GetPhysDirPath('data/scripts/' + Save.Settings.Skin + '/services'), '*.evil', False);
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
  PageControl.ShowTabs := False;
  PageControl.PageIndex := 0;
  EditChat.SetFocus;
  // Load list of services
  LoadServiceList;
  Self.ButtonOpenService.Enabled := Self.ComboBoxService.ItemIndex > 0;
end;

procedure TFormChat.MenuItemShowWebUIClick(Sender: TObject);
begin
  Self.ShowWebUI;
end;

procedure TFormChat.MenuItemStopGeneratingClick(Sender: TObject);
begin
  Self.StopGenerating;
end;

procedure TFormChat.EditChatKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Self.CalcHeights;
  if (Key = 13) and not (Shift = [ssShift]) and (FormChat.EditChat.Lines.Text<>'') then
  begin
    Send;
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
  if not FRichText.IsStreaming then
  begin
    if MessageDlg('', 'Clear chat history?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      Self.ClearHistory;
    end;
  end;
end;

procedure TFormChat.ButtonEditSaveClick(Sender: TObject);
begin
  Self.SaveHistory(Self.MemoEdit.Lines.Text);
end;

procedure TFormChat.ButtonEditCancelClick(Sender: TObject);
begin
  PageControl.PageIndex := 0;
end;

procedure TFormChat.ButtonEditClick(Sender: TObject);
begin
  if FRichText.IsStreaming then Exit;
  PageControl.PageIndex := 1;
  MemoEdit.Lines.Text := ChatHistory.ToEdit;
end;

procedure TFormChat.ButtonOpenServiceClick(Sender: TObject);
var
  I: Integer;
  Frm: TFormEvilCEditor;
begin
  I := Self.ComboBoxService.ItemIndex;
  if I > 0 then
  begin
    Frm := TFormEvilCEditor.Create(nil);
    DockMaster.MakeDockable(Frm);
    Frm.LoadFromFile(GetPhysFilePath('data/scripts/' + Save.Settings.Skin + '/services/' + Self.ComboBoxService.Items[I]));
  end;
end;

procedure TFormChat.ButtonRefreshServiceClick(Sender: TObject);
begin
  LoadServiceList;
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

procedure TFormChat.InsertLog(const LogName, Msg: String; Time: String = '');
var
  H, M, SS, MS: Word;
  TB: TKMemoTextBlock;
  CH: TChatHistoryRec;
  IsLog: Boolean = False;
begin
  DecodeTime(Now, H, M, SS, MS);
  if Time = '' then
  begin
    Time := Format('%.*d', [2, H]) + ':' + Format('%.*d', [2, M]) + ':' + Format('%.*d', [2, SS]);
    IsLog := True;
  end;

  RemoveTyping;
  if (FRichText.IsStreaming and (FStreamingPartCount = 0)) or (not FRichText.IsStreaming) then
  begin
    MemoChatLog.Blocks.AddParagraph;
    TB := MemoChatLog.Blocks.AddTextBlock(LogName);
    TB.TextStyle.Font.Style := TB.TextStyle.Font.Style + [fsBold];
    if LogName = 'System' then
    begin
      TB.TextStyle.Font.Color := CColor(Save.Settings.ChatWindowColorSystemName);
      CH.SenderType := cseSystem;
      TB := MemoChatLog.Blocks.AddTextBlock(' [' + Time + ']');
      TB.TextStyle.Font.Style := TB.TextStyle.Font.Style + [fsItalic];
      TB.TextStyle.Font.Color := CColor(Save.Settings.ChatWindowColorItalicText);
    end else
    if LogName = Save.Settings.UserName then
    begin
      TB.TextStyle.Font.Color := CColor(Save.Settings.ChatWindowColorUserName);
      CH.SenderType := cseUser;
    end else
    begin
      TB.TextStyle.Font.Color := CColor(Save.Settings.ChatWindowColorCharacterName);
      CH.SenderType := cseSatania;
    end;
    MemoChatLog.Blocks.AddParagraph;
    FRichText.Reset;
  end;

  // TODO: Optimize this horrible mess
  FRichText.Source := Msg;
  if LogName = 'System' then
    FRichText.Lex(False)
  else
    FRichText.Lex((LogName <> Save.Settings.UserName) or Save.Settings.EnableItalicForUserText);
  FRichText.NextTokenPos := FRichText.TokenList.Count - 1;
  if LogName = 'System' then
    FRichText.Parse(MemoChatLog, False)
  else
    FRichText.Parse(MemoChatLog, ((LogName <> Save.Settings.UserName) or Save.Settings.EnableItalicForUserText));

  if FIsWriteToHistoryLog then
  begin
    if (not FRichText.IsStreaming) or (FStreamingPartCount = 0) then
    begin
      CH.Time := Time;
      CH.Message := Msg;
      CH.GUID := GUID;
      ChatHistory.List.Add(CH);
    end;
    if (FRichText.IsStreaming) and (FStreamingPartCount > 0) then
    begin
      CH := ChatHistory.List[ChatHistory.List.Count - 1];
      CH.Message := Msg;   
      CH.GUID := GUID;
      ChatHistory.List[ChatHistory.List.Count - 1] := CH;
    end;
  end;

  if not FRichText.IsStreaming then
  begin
    MemoChatLog.Blocks.AddParagraph;
    if IsLog then
      ChatHistory.SaveLastestMessage;
  end;

  if (LogName = Save.Settings.UserName) or (FRichText.IsStreaming) then
  begin
    InsertTyping;
  end;
  ScrollToBottom;
  Inc(FStreamingPartCount);
end;

procedure TFormChat.InsertTyping;
begin
  RemoveTyping;
  Typing := MemoChatLog.Blocks.AddTextBlock(' ' + Satania.Name + ' is typing...');
  Typing.TextStyle.Font.Style := Typing.TextStyle.Font.Style + [fsItalic];
  Typing.TextStyle.Font.Color := CColor(Save.Settings.ChatWindowColorItalicText);
end;

procedure TFormChat.RemoveTyping;
begin
  if Self.Typing = nil then
    Exit;
  if MemoChatLog.Blocks.Count <> 0 then
    MemoChatLog.Blocks.Delete(MemoChatLog.Blocks.Count - 1);
  Self.Typing := nil;
end;

procedure TFormChat.ApplySettings;
begin
  MemoChatLog.Font.Name := Save.Settings.ChatWindowFont;
  MemoChatLog.Font.Size := Save.Settings.ChatWindowFontSize;
  MemoChatLog.Font.Color := Save.Settings.ChatWindowColorNormalText;
  MemoChatLog.Color := Save.Settings.ChatWindowColorBackground;  
  MemoChatLog.Colors.BkGnd := Save.Settings.ChatWindowColorBackground;
  if Save.Settings.ChatWindowClearType then
    MemoChatLog.Font.Quality := fqCleartype
  else
    MemoChatLog.Font.Quality := fqDefault;
  Self.RichText.ColorCodeBlockText := Save.Settings.ChatWindowColorCodeBlockText;
  Self.RichText.ColorItalicText := Save.Settings.ChatWindowColorItalicText;
  //MemoEdit.Color := Save.Settings.ChatWindowColorBackground;
  //MemoEdit.Font.Color := Save.Settings.ChatWindowColorNormalText;
  //LabelEditMode.Font.Color := Save.Settings.ChatWindowColorNormalText;
  //Self.Color := Save.Settings.ChatWindowColorBackground;
  LoadChatHistoryFromFile;
end;

procedure TFormChat.ShowWebUI;
var
  RootPath,
  AvatarPath: String;
begin
  if WebUIHandle <> 0 then
    webui_close(WebUIHandle);
  WebUIHandle := webui_new_window;
  // Try to set root at user config dir if exists
  RootPath := GetOSLocalDir + 'webui';
  if not DirectoryExists(RootPath) then
    RootPath := 'data/webui';
  webui_set_root_folder(WebUIHandle, PChar(RootPath));
  // Prepare avatar
  AvatarPath := GetOSLocalDir + 'sprites/' + Save.Settings.Skin + '/avatar.png';
  if not FileExists(AvatarPath) then
    AvatarPath := 'data/sprites/' + Save.Settings.Skin + '/avatar.png';
  if FileExists(RootPath + '/chat/avatar_1.png') then
    DeleteFile(RootPath + '/chat/avatar_1.png');
  if FileExists(AvatarPath) then
  begin
    CopyFile(AvatarPath, RootPath + '/chat/avatar_1.png');
  end;
  //
  webui_bind(WebUIHandle, 'chat_history_get', @WebUI_ChatHistoryGet);  
  webui_bind(WebUIHandle, 'chat_history_plaintext_get', @WebUI_ChatHistoryGetPlainText);
  webui_bind(WebUIHandle, 'chat_history_clear', @WebUI_ChatHistoryClear);
  webui_bind(WebUIHandle, 'chat_history_save', @WebUI_ChatHistorySave);
  webui_bind(WebUIHandle, 'chat_is_streaming', @WebUI_ChatIsStreaming);
  webui_bind(WebUIHandle, 'chat_send', @WebUI_ChatSend);
  webui_bind(WebUIHandle, 'chat_stop_generating', @WebUI_ChatStopGenerating);
  webui_bind(WebUIHandle, 'character_skin_get', @WebUI_CharacterSkinGet);
  webui_bind(WebUIHandle, 'character_name_get', @WebUI_CharacterNameGet);
  webui_show(WebUIHandle, 'chat/index.html');
end;

procedure TFormChat.StopGenerating;
begin
  // Stop worker script
  Satania.WorkerDelete('___worker');
  // Restore state
  DisableStreaming;
  FormBubble.DisableStreaming;
  //
  MenuItemStopGenerating.Enabled := False;
end;

procedure TFormChat.SaveHistory(const HistoryText: String);
begin
  PageControl.PageIndex := 0;
  ChatHistory.FromEdit(StringReplace(HistoryText, #13, '', [rfReplaceAll]));
  LoadChatHistoryFromFile;
end;

procedure TFormChat.ClearHistory;
begin
  MemoChatLog.Blocks.Clear;
  ChatHistory.Clear;
end;

end.

