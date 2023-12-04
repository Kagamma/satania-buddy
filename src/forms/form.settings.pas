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

unit form.settings;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  FileUtil, CopyDir,
  ExtCtrls, Buttons, Spin, MaskEdit, Menus, CastleApplicationProperties,
  Types, LCLTranslator, IniFiles, LCLIntf, WebUI;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    ButtonDeleteSkin: TBitBtn;
    ButtonOk: TBitBtn;
    ButtonCloneSkin: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonChatBubbleFont: TSpeedButton;
    ButtonApply: TBitBtn;
    ButtonSpeechRecognitionHelp: TBitBtn;
    CheckBoxChatSpeechBalloon: TCheckBox;
    CheckBoxChatBubbleCleartype: TCheckBox;
    CheckBoxDefaultChatWindow: TCheckBox;
    CheckBoxErrorMessage: TCheckBox;
    CheckBoxEmailUseSSL: TCheckBox;
    CheckBoxEmailSmtpUseSSL: TCheckBox;
    CheckBoxEnableItalicForUserText: TCheckBox;
    CheckBoxLewd: TCheckBox;
    CheckBoxDeveloperMode: TCheckBox;
    CheckBoxEmbeddedServerEnable: TCheckBox;
    CheckBoxChatWindowCleartype: TCheckBox;
    CheckBoxRules: TCheckBox;
    ColorButtonChatWindowCharacterName: TColorButton;
    ColorButtonChatWindowUserName: TColorButton;
    ColorButtonChatWindowSystemName: TColorButton;
    ColorButtonChatWindowBackground: TColorButton;
    ColorButtonChatWindowNormalText: TColorButton;
    ColorButtonChatWindowItalicText: TColorButton;
    ColorButtonChatWindowCodeBlockText: TColorButton;
    ComboBoxSTTVoskModel: TComboBox;
    ComboBoxSkin: TComboBox;
    ComboBoxSTTBackend: TComboBox;
    ComboBoxImageQuality: TComboBox;
    ComboBoxSTTWhisperModel: TComboBox;
    EditChatBubbleDelay: TSpinEdit;
    EditChatBubbleSizeX: TSpinEdit;
    EditChatBubbleSizeY: TSpinEdit;
    EditChatWindowFont: TEdit;
    EditChatBubbleFont: TEdit;
    EditTextSpeed: TSpinEdit;
    EditYourName: TEdit;
    EditDefaultEvilScheme: TEdit;
    EditEmailFetchFrom: TEdit;
    EditEmailPassword: TEdit;
    EditEmailSmtpPassword: TEdit;
    EditEmailPort: TSpinEdit;
    EditEmailSmtpPort: TSpinEdit;
    EditEmailServer: TEdit;
    EditEmailSmtpServer: TEdit;
    EditEmailUsername: TEdit;
    EditEmailSmtpUsername: TEdit;
    EditEmbeddedServerPort: TSpinEdit;
    EditFrameSkip: TSpinEdit;
    EditFPS: TSpinEdit;
    EditSoWRightMargin: TSpinEdit;
    EditBaseScaling: TFloatSpinEdit;
    FontDialog: TFontDialog;
    GroupBoxEmailIMAP: TGroupBox;
    GroupBoxEmailSMTP: TGroupBox;
    GroupBoxSTTVosk: TGroupBox;
    GroupBoxSTTWhisper: TGroupBox;
    Label1: TLabel;
    Label4: TLabel;
    LabelChatbotServer4: TLabel;
    LabelChatBubbleDelay: TLabel;
    LabelChatBubbleSize: TLabel;
    LabelChatSpeechBalloon: TLabel;
    LabelChatBubbleFont: TLabel;
    LabelChatBubbleCleartype: TLabel;
    LabelChatWindowColorCharacterName: TLabel;
    LabelChatWindowColorUserName: TLabel;
    LabelChatWindowColorSystemName: TLabel;
    LabelChatWindowColorBackground: TLabel;
    LabelChatWindowColorNormalText: TLabel;
    LabelChatWindowColorItalicText: TLabel;
    LabelChatWindowColorCodeBlockText: TLabel;
    LabelDefaultChatWindow: TLabel;
    LabelErrorMessage: TLabel;
    LabelEmbeddedServiceNotice: TLabel;
    LabelDeveloperMode: TLabel;
    LabelEmailPassword: TLabel;
    LabelEmailSmtpPassword: TLabel;
    LabelEmailPort: TLabel;
    LabelEmailSmtpPort: TLabel;
    LabelEmailServer: TLabel;
    LabelEmailSmtpServer: TLabel;
    LabelEmailUsername: TLabel;
    LabelEmailSmtpUsername: TLabel;
    LabelEmailUseSSL: TLabel;
    LabelEmailSmtpUseSSL: TLabel;
    LabelEnableItalicForUserText: TLabel;
    LabelFetchFrom: TLabel;
    LabelEmbeddedServerPort: TLabel;
    LabelEmbeddedServerEnable: TLabel;
    LabelChatWindowFont: TLabel;
    LabelChatWindowCleartype: TLabel;
    LabelRules: TLabel;
    LabelSTTModel2: TLabel;
    LabelTextSpeed: TLabel;
    LabelYourName: TLabel;
    LabelSTTBackend: TLabel;
    LabelBaseScaling: TLabel;
    LabelFontSkin: TLabel;
    LabelFrameSkip: TLabel;
    LabelLewd: TLabel;
    LabelFPS: TLabel;
    LabelSoWRightMargin: TLabel;
    LabelSTTModel1: TLabel;
    LabelDefaultEvilScheme: TLabel;
    LabelImageQuality: TLabel;
    ListBoxSettings: TListBox;
    PageControl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelButtons: TPanel;
    PanelChatWindow: TPanel;
    PanelSettings: TPanel;
    ButtonChatWindowFont: TSpeedButton;
    Splitter1: TSplitter;
    TabSheetChatBubble: TTabSheet;
    TabSheetGeneral: TTabSheet;
    TabSheetChatWindow: TTabSheet;
    TabSheetEmbeddedServer: TTabSheet;
    TabSheetOptimization: TTabSheet;
    TabSheetSpeechRecognition: TTabSheet;
    TabSheetIMAP: TTabSheet;
    TabSheetGraphics: TTabSheet;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonChatBubbleFontClick(Sender: TObject);
    procedure ButtonChatWindowFontClick(Sender: TObject);
    procedure ButtonApplyClick(Sender: TObject);
    procedure ButtonCloneSkinClick(Sender: TObject);
    procedure ButtonDeleteSkinClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ButtonSpeechRecognitionHelpClick(Sender: TObject);
    procedure ComboBoxSkinChange(Sender: TObject);
    procedure ComboBoxSTTBackendChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxSettingsSelectionChange(Sender: TObject; User: boolean);
  private
    EditChatWindowFontSize: Integer;
    EditChatBubbleFontSize: Integer;
  public
    procedure Apply;
  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.lfm}

uses
  Globals, Mcdowell, Mcdowell.imap, State.Main, Mcdowell.SpeechToText,
  Utils.Strings,
  form.bubble,
  Mcdowell.smtp,
  Mcdowell.sketch,         
  Mcdowell.Data,
  Form.chat,
  Form.Touch,
  com.Brokers,
  Utils.Files,
  Utils.Encdec;

{ TFormSettings }

procedure TFormSettings.FormShow(Sender: TObject);
var
  I, J: Integer;
  S: String;
  SL, SL2: TStringList;
begin
  PageControl.TabIndex := 0;
  PageControl.ShowTabs := False;
  ListboxSettings.ItemIndex := 0;
  ComboBoxSTTBackend.Items.Clear;
  ComboBoxSTTBackend.Items.Add('Vosk');
  ComboBoxSTTBackend.Items.Add('whisper.cpp');
  {$ifdef WINDOWS}
  ComboBoxSTTBackend.Items.Add('Microsoft Speech Object Library');
  {$endif}
  ComboBoxSTTBackend.ItemIndex := Save.Settings.STTBackend;
  ComboBoxSTTBackendChange(Self);

  CheckBoxLewd.Checked := Save.Settings.Lewd;
  CheckBoxDeveloperMode.Checked := Save.Settings.DeveloperMode;

  EditFPS.Value := Save.Settings.FPS;
  EditChatBubbleDelay.Value := Save.Settings.ChatBubbleDelay;
  EditSoWRightMargin.Value := Save.Settings.SitOnWindowRightMargin;
  EditTextSpeed.Value := Save.Settings.TextSpeed;
  EditDefaultEvilScheme.Text := Save.Settings.DefaultEvilScheme;
  EditBaseScaling.Value := Save.Settings.BaseScaling;
  EditFrameSkip.Value := Save.Settings.FrameSkip;
  EditYourName.Text := Save.Settings.UserName;
  CheckBoxRules.Checked := Save.Settings.Rules;
  CheckBoxErrorMessage.Checked := Save.Settings.SystemErrorMessage;
  CheckBoxChatSpeechBalloon.Checked := Save.Settings.ChatSpeechBalloon;
  CheckBoxEnableItalicForUserText.Checked := Save.Settings.EnableItalicForUserText;

  EditChatWindowFont.Text := Save.Settings.ChatWindowFont;
  EditChatWindowFontSize := Save.Settings.ChatWindowFontSize;
  ColorButtonChatWindowBackground.ButtonColor := Save.Settings.ChatWindowColorBackground;
  ColorButtonChatWindowUserName.ButtonColor := Save.Settings.ChatWindowColorUserName;
  ColorButtonChatWindowCharacterName.ButtonColor := Save.Settings.ChatWindowColorCharacterName;
  ColorButtonChatWindowSystemName.ButtonColor := Save.Settings.ChatWindowColorSystemName;
  ColorButtonChatWindowNormalText.ButtonColor := Save.Settings.ChatWindowColorNormalText;
  ColorButtonChatWindowItalicText.ButtonColor := Save.Settings.ChatWindowColorItalicText;
  ColorButtonChatWindowCodeBlockText.ButtonColor := Save.Settings.ChatWindowColorCodeBlockText;
  CheckBoxChatWindowCleartype.Checked := Save.Settings.ChatWindowClearType;

  EditChatBubbleFont.Text := Save.Settings.ChatBubbleFont;
  EditChatBubbleFontSize := Save.Settings.ChatBubbleFontSize;
  EditChatBubbleSizeX.Value := Save.Settings.ChatBubbleSizeX;
  EditChatBubbleSizeY.Value := Save.Settings.ChatBubbleSizeY;
  CheckBoxChatBubbleCleartype.Checked := Save.Settings.ChatBubbleClearType;          
  CheckBoxDefaultChatWindow.Checked := Save.Settings.TouchPanelOpenChatWebUI;

  ComboBoxSkin.Items.Clear;
  SL := TStringList.Create;
  SL2 := TStringList.Create;
  FindAllDirectories(SL, 'data/sprites', False);
  FindAllDirectories(SL, GetOSLocalDir + 'data/sprites', False);
  FindAllDirectories(SL2, 'data/scripts', False);
  FindAllDirectories(SL2, GetOSLocalDir + 'data/scripts', False);
  for I := 0 to SL.Count - 1 do
  begin
    SL[I] := ExtractFileName(SL[I]);
  end;
  for I := 0 to SL2.Count - 1 do
  begin
    SL2[I] := ExtractFileName(SL2[I]);
  end;
  for I := 0 to SL.Count - 1 do
  begin
    S := SL[I];
    for J := 0 to SL2.Count - 1 do
    begin
      if S = SL2[J] then
      begin
        ComboBoxSkin.Items.Add(S);
        if S = Save.Settings.Skin then
        begin
          ComboBoxSkin.ItemIndex := ComboBoxSkin.Items.Count - 1;
        end;
        break;
      end;
    end;
  end;
  SL.Free;
  SL2.Free;

  EditEmailServer.Text := Save.Settings.EmailServer;
  EditEmailPort.Value := Save.Settings.EmailPort;
  EditEmailUsername.Text := Save.Settings.EmailUsername;
  EditEmailFetchFrom.Text := Save.Settings.EmailFetchFrom;

  EditEmailSMTPServer.Text := Save.Settings.EmailSMTPServer;
  EditEmailSMTPPort.Value := Save.Settings.EmailSMTPPort;
  EditEmailSMTPUsername.Text := Save.Settings.EmailSMTPUsername;

  // Vosk loader
  ComboBoxSTTVoskModel.Clear;
  SL := TStringList.Create;
  FindAllDirectories(SL, 'data/nn/vosk', False);
  for I := 0 to SL.Count - 1 do
  begin
    S := ExtractFileName(SL[I]);
    ComboBoxSTTVoskModel.Items.Add(S);
    if S = Save.Settings.STTVoskModel then
    begin
      ComboBoxSTTVoskModel.ItemIndex := ComboBoxSTTVoskModel.Items.Count - 1;
    end;
  end;
  SL.Free;

  // whisper loader
  ComboBoxSTTWhisperModel.Clear;
  SL := TStringList.Create;
  FindAllFiles(SL, 'data/nn/whisper.cpp', '*.bin', False);
  for I := 0 to SL.Count - 1 do
  begin
    S := ExtractFileName(SL[I]);
    ComboBoxSTTWhisperModel.Items.Add(S);
    if S = Save.Settings.STTWhisperModel then
    begin
      ComboBoxSTTWhisperModel.ItemIndex := ComboBoxSTTWhisperModel.Items.Count - 1;
    end;
  end;
  SL.Free;

  SL := TStringList.Create;
  FindAllFiles(SL, 'data/fonts', '', False);
  for I := 0 to SL.Count - 1 do
  begin
    S := ExtractFileName(SL[I]);
    PageControl.ActivePageIndex := ListboxSettings.ItemIndex;
  end;
  SL.Free;

  CheckBoxEmbeddedServerEnable.Checked := Save.Settings.EmbeddedServerEnable;
  EditEmbeddedServerPort.Value := Save.Settings.EmbeddedServerPort;

  if Save.Settings.EmailPassword <> '' then
    EditEmailPassword.Text := Decrypt(Save.Settings.EmailPassword)
  else
    EditEmailPassword.Text := '';
  if Save.Settings.EmailSMTPPassword <> '' then
    EditEmailSMTPPassword.Text := Decrypt(Save.Settings.EmailSMTPPassword)
  else
    EditEmailSMTPPassword.Text := '';
  CheckBoxEmailUseSSL.Checked := Save.Settings.EmailUseSSL;
  CheckBoxEmailSMTPUseSSL.Checked := Save.Settings.EmailSMTPUseSSL;

  ComboBoxImageQuality.ItemIndex := 1;
  for I := 0 to ComboBoxImageQuality.Items.Count - 1 do
    if ComboBoxImageQuality.Items[I] = Save.Settings.ImageQuality then
    begin
      ComboBoxImageQuality.ItemIndex := I;
      break;
    end;
  //
  ButtonDeleteSkin.Enabled := DirectoryExists(GetOSLocalDir + 'data/sprites/' + Save.Settings.Skin);
end;

procedure TFormSettings.ListBoxSettingsSelectionChange(Sender: TObject;
  User: boolean);
begin
  PageControl.TabIndex := ListboxSettings.ItemIndex;
end;

procedure TFormSettings.Apply;
var
  IsSkinChanged: Boolean = False;
begin
  Save.Settings.Lewd := CheckBoxLewd.Checked;
  Save.Settings.DeveloperMode := CheckBoxDeveloperMode.Checked;
  if Save.Settings.DeveloperMode then
    Satania.CleanUpCache;
  Save.Settings.FPS := EditFPS.Value;
  Save.Settings.ChatBubbleDelay := EditChatBubbleDelay.Value;
  Save.Settings.SitOnWindowRightMargin := EditSoWRightMargin.Value;
  Save.Settings.TextSpeed := EditTextSpeed.Value;
  Save.Settings.BaseScaling := EditBaseScaling.Value;
  Save.Settings.DefaultEvilScheme := EditDefaultEvilScheme.Text;
  Save.Settings.ImageQuality := ComboBoxImageQuality.Items[ComboBoxImageQuality.ItemIndex];
  Save.Settings.ChatWindowFont := EditChatWindowFont.Text;
  Save.Settings.ChatWindowFontSize := EditChatWindowFontSize;
  Save.Settings.ChatWindowColorBackground := ColorButtonChatWindowBackground.Color;
  Save.Settings.ChatWindowColorUserName := ColorButtonChatWindowUserName.Color;
  Save.Settings.ChatWindowColorCharacterName := ColorButtonChatWindowCharacterName.Color;
  Save.Settings.ChatWindowColorSystemName := ColorButtonChatWindowSystemName.Color;
  Save.Settings.ChatWindowColorNormalText := ColorButtonChatWindowNormalText.Color;
  Save.Settings.ChatWindowColorItalicText := ColorButtonChatWindowItalicText.Color;
  Save.Settings.ChatWindowColorCodeBlockText := ColorButtonChatWindowCodeBlockText.Color;
  Save.Settings.FrameSkip := EditFrameSkip.Value;
  Save.Settings.Rules := CheckBoxRules.Checked;
  Save.Settings.ChatSpeechBalloon := CheckBoxChatSpeechBalloon.Checked;
  Save.Settings.UserName := EditYourName.Text;
  Save.Settings.SystemErrorMessage := CheckBoxErrorMessage.Checked;
  Save.Settings.EnableItalicForUserText := CheckBoxEnableItalicForUserText.Checked;
  // Clear sketch and workers if skin is changed
  Save.Settings.ChatWindowClearType := CheckBoxChatWindowCleartype.Checked;
  Save.Settings.EmailServer := EditEmailServer.Text;
  Save.Settings.EmailPort := EditEmailPort.Value;
  Save.Settings.EmailUsername := EditEmailUsername.Text;
  Save.Settings.EmailFetchFrom := EditEmailFetchFrom.Text;

  Save.Settings.EmailSMTPServer := EditEmailSMTPServer.Text;
  Save.Settings.EmailSMTPPort := EditEmailSMTPPort.Value;
  Save.Settings.EmailSMTPUsername := EditEmailSMTPUsername.Text;

  Save.Settings.EmbeddedServerPort := EditEmbeddedServerPort.Value;
  Save.Settings.EmbeddedServerEnable := CheckBoxEmbeddedServerEnable.Checked;

  Save.Settings.ChatWindowFont := EditChatWindowFont.Text;
  Save.Settings.ChatWindowFontSize := EditChatWindowFontSize;
  Save.Settings.ChatWindowColorBackground := ColorButtonChatWindowBackground.ButtonColor;
  Save.Settings.ChatWindowColorUserName := ColorButtonChatWindowUserName.ButtonColor;
  Save.Settings.ChatWindowColorCharacterName := ColorButtonChatWindowCharacterName.ButtonColor;
  Save.Settings.ChatWindowColorSystemName := ColorButtonChatWindowSystemName.ButtonColor;
  Save.Settings.ChatWindowColorNormalText := ColorButtonChatWindowNormalText.ButtonColor;
  Save.Settings.ChatWindowColorItalicText := ColorButtonChatWindowItalicText.ButtonColor;
  Save.Settings.ChatWindowColorCodeBlockText := ColorButtonChatWindowCodeBlockText.ButtonColor;
  Save.Settings.ChatWindowClearType := CheckBoxChatWindowCleartype.Checked;

  Save.Settings.ChatBubbleFont := EditChatBubbleFont.Text;
  Save.Settings.ChatBubbleFontSize := EditChatBubbleFontSize;
  Save.Settings.ChatBubbleSizeX := EditChatBubbleSizeX.Value;
  Save.Settings.ChatBubbleSizeY := EditChatBubbleSizeY.Value;
  Save.Settings.ChatBubbleClearType := CheckBoxChatBubbleCleartype.Checked;
  Save.Settings.TouchPanelOpenChatWebUI := CheckBoxDefaultChatWindow.Checked;

  if EditEmailPassword.Text <> '' then
    Save.Settings.EmailPassword := Encrypt(EditEmailPassword.Text)
  else
    Save.Settings.EmailPassword := '';
  if EditEmailSMTPPassword.Text <> '' then
    Save.Settings.EmailSMTPPassword := Encrypt(EditEmailSMTPPassword.Text)
  else
    Save.Settings.EmailSMTPPassword := '';

  Save.Settings.EmailUseSSL := CheckBoxEmailUseSSL.Checked;
  Save.Settings.EmailSMTPUseSSL := CheckBoxEmailSMTPUseSSL.Checked;
  if Save.Settings.Skin <> ComboBoxSkin.Items[ComboBoxSkin.ItemIndex] then
  begin
    Satania.SwitchCharacter(ComboBoxSkin.Items[ComboBoxSkin.ItemIndex]);
    IsSkinChanged := True;
  end;
  Save.SaveToFile('configs.json');
  FormChat.LoadServiceList;
  SataniaIMAP.Disconnect;
  //
  ApplicationProperties.LimitFPS := Save.Settings.FPS;
  FormBubble.TypingSpeed := Save.Settings.TextSpeed;
  Satania.SetImageQuality(Save.Settings.ImageQuality);

  if (ComboBoxSTTVoskModel.Items[ComboBoxSTTVoskModel.ItemIndex] <> Save.Settings.STTVoskModel)
    or (ComboBoxSTTWhisperModel.Items[ComboBoxSTTWhisperModel.ItemIndex] <> Save.Settings.STTWhisperModel)
    or (ComboBoxSTTBackend.ItemIndex <> Save.Settings.STTBackend) then
  begin
    Save.Settings.STTBackend := ComboBoxSTTBackend.ItemIndex;
    Save.Settings.STTVoskModel := ComboBoxSTTVoskModel.Items[ComboBoxSTTVoskModel.ItemIndex];     
    Save.Settings.STTWhisperModel := ComboBoxSTTWhisperModel.Items[ComboBoxSTTWhisperModel.ItemIndex];
    if Save.SpeechToText then
      SataniaSpeechToText.Enable;
  end;
  //
  ButtonDeleteSkin.Enabled := DirectoryExists(GetOSLocalDir + 'data/scripts/' + Save.Settings.Skin);
end;

procedure TFormSettings.ButtonApplyClick(Sender: TObject);
begin
  try
    Self.Apply;
  except
    on E: Exception do
    begin
      Satania.Error(E.Message);
      DumpExceptionCallStack(E);
    end;
  end;
end;

procedure TFormSettings.ButtonCloneSkinClick(Sender: TObject);
var
  CurrentSkinName, NewSkinName: String;
  I: Integer;
  CD: TCopyDir;
begin
  if InputQuery('Clone to new character, locate in "' + GetOSLocalDir + '"', 'Character''s directory name:', False, NewSkinName) then
  begin
    if NewSkinName = '' then
    begin
      MessageDlg('', 'Character''s name is empty!', mtError, [mbOk], 0);
      Exit;
    end;
    for I := 0 to ComboBoxSkin.Items.Count - 1 do
    begin
      if NewSkinName = ComboBoxSkin.Items[I] then
      begin
        MessageDlg('', 'Character with the same directory name is already existed!', mtError, [mbOk], 0);
        Exit;
      end;
    end;
    CurrentSkinName := ComboBoxSkin.Items[ComboBoxSkin.ItemIndex];
    CD := TCopyDir.Create(GetPhysDirPath('data/scripts/' + CurrentSkinName), GetOSLocalDir + 'data/scripts/' + NewSkinName);
    CD.PrintToTerminal := True;
    CD.Start;
    CD.Free;
    CD := TCopyDir.Create(GetPhysDirPath('data/sprites/' + CurrentSkinName), GetOSLocalDir + 'data/sprites/' + NewSkinName);
    CD.PrintToTerminal := True;
    CD.Start;
    CD.Free;  
    MessageDlg('', 'Character cloned!', mtInformation, [mbOk], 0);
    ComboBoxSkin.Items.Add(NewSkinName);
  end;
end;

procedure TFormSettings.ButtonDeleteSkinClick(Sender: TObject);
var
  CurrentSelectedSkin: String;
  DefaultString: String = 'satania (bloomers)';
  I: Integer;
begin
  CurrentSelectedSkin := ComboBoxSkin.Items[ComboBoxSkin.ItemIndex];
  if MessageDlg('Character deletion', 'Do you want to delete "' + CurrentSelectedSkin + '"?', mtConfirmation, [mbOk, mbCancel], 0) = mrOK then
  begin
    ComboBoxSkin.Items.Delete(ComboBoxSkin.ItemIndex);
    if Save.Settings.Skin = CurrentSelectedSkin then
    begin
      Satania.SwitchCharacter(DefaultString);
    end else
      DefaultString := Save.Settings.Skin;
    for I := 0 to ComboBoxSkin.Items.Count - 1 do
    begin
      if ComboBoxSkin.Items[I] = DefaultString then
      begin
        ComboBoxSkin.ItemIndex := I;
        break;
      end;
    end;
    DelDir(GetOSLocalDir + 'data/sprites/' + CurrentSelectedSkin);
    DelDir(GetOSLocalDir + 'data/scripts/' + CurrentSelectedSkin);
    MessageDlg('', 'Character "' + CurrentSelectedSkin + '" deleted!', mtInformation, [mbOk], 0)
  end;
end;

procedure TFormSettings.ButtonOkClick(Sender: TObject);
begin
  try
    Self.Apply;
    Hide;
  except
    on E: Exception do
    begin
      Satania.Error(E.Message);
      DumpExceptionCallStack(E);
    end;
  end;
end;

procedure TFormSettings.ButtonSpeechRecognitionHelpClick(Sender: TObject);
begin
  OpenURL('https://github.com/Kagamma/satania-buddy/wiki/Speech-Recognition');
end;

procedure TFormSettings.ComboBoxSkinChange(Sender: TObject);
begin
  ButtonDeleteSkin.Enabled := DirectoryExists(GetOSLocalDir + 'data/sprites/' + ComboBoxSkin.Items[ComboBoxSkin.ItemIndex]);
end;

procedure TFormSettings.ComboBoxSTTBackendChange(Sender: TObject);
begin
  GroupBoxSTTVosk.Visible := ComboBoxSTTBackend.ItemIndex = SPEECH_RECOGNIZER_BACKEND_VOSK;
  GroupBoxSTTWhisper.Visible := ComboBoxSTTBackend.ItemIndex = SPEECH_RECOGNIZER_BACKEND_WHISPER;
end;

procedure TFormSettings.ButtonCancelClick(Sender: TObject);
begin
  Hide;
end;

procedure TFormSettings.ButtonChatBubbleFontClick(Sender: TObject);
begin
  FontDialog.Font.Name := EditChatBubbleFont.Text;
  FontDialog.Font.Size := EditChatBubbleFontSize;
  if FontDialog.Execute then
  begin
    EditChatBubbleFont.Text := FontDialog.Font.Name;
    EditChatBubbleFontSize := FontDialog.Font.Size;
  end;
end;

procedure TFormSettings.ButtonChatWindowFontClick(Sender: TObject);
var
  ScriptFiles: TStringList;
begin
  FontDialog.Font.Name := EditChatWindowFont.Text;
  FontDialog.Font.Size := EditChatWindowFontSize;
  if FontDialog.Execute then
  begin
    EditChatWindowFont.Text := FontDialog.Font.Name;
    EditChatWindowFontSize := FontDialog.Font.Size;
  end;
  FindAllFiles(ScriptFiles, 'data/scripts/libs/menu', '*.evil', False);
end;

end.

