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

unit form.settings;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  FileUtil,
  ExtCtrls, Buttons, Spin, MaskEdit, Menus, CastleApplicationProperties,
  Types, LCLTranslator, IniFiles;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    ButtonCharsetAdd: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonOk: TBitBtn;
    CheckBoxEmailUseSSL: TCheckBox;
    CheckBoxEmailSmtpUseSSL: TCheckBox;
    CheckBoxLewd: TCheckBox;
    CheckBoxDeveloperMode: TCheckBox;
    CheckBoxEmbeddedServerEnable: TCheckBox;
    ComboBoxCustomBotScriptType: TComboBox;
    ComboBoxFont: TComboBox;
    ComboBoxSTTVoskModel: TComboBox;
    ComboBoxSkin: TComboBox;
    ComboBoxSTTBackend: TComboBox;
    ComboBoxImageQuality: TComboBox;
    EditBotServer: TEdit;
    EditCustomBotScript: TEdit;
    EditBotVolframAlphaAppID: TEdit;
    EditCharsetTo: TLabeledEdit;
    EditDefaultEvilScheme: TEdit;
    EditChatBubbleDelay: TSpinEdit;
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
    EditFontSize: TSpinEdit;
    EditTextSpeed: TSpinEdit;
    EditBaseScaling: TFloatSpinEdit;
    GroupBoxVolframAlpha: TGroupBox;
    GroupBoxChatbotServer: TGroupBox;
    GroupBoxCustomBotScript: TGroupBox;
    GroupBoxEmailIMAP: TGroupBox;
    GroupBoxEmailSMTP: TGroupBox;
    GroupBoxSTTVosk: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelCustomBotScript: TLabel;
    LabelEmbeddedServiceNotice: TLabel;
    LabelChatbotServer: TLabel;
    LabelChatbotServer1: TLabel;
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
    LabelFetchFrom: TLabel;
    LabelEmbeddedServerPort: TLabel;
    LabelEmbeddedServerEnable: TLabel;
    LabelSTTBackend: TLabel;
    LabelBaseScaling: TLabel;
    LabelFontSkin: TLabel;
    LabelFontSize: TLabel;
    LabelFontName: TLabel;
    LabelFontCharset: TLabel;
    LabelFrameSkip: TLabel;
    LabelLewd: TLabel;
    LabelFPS: TLabel;
    LabelChatBubbleDelay: TLabel;
    LabelSoWRightMargin: TLabel;
    LabelSTTModel1: TLabel;
    LabelTextSpeed: TLabel;
    LabelDefaultEvilScheme: TLabel;
    LabelImageQuality: TLabel;
    EditCharsetFrom: TLabeledEdit;
    ListBoxSettings: TListBox;
    ListBoxCharset: TListBox;
    MenuItemDeleteCharset: TMenuItem;
    OpenDialogEvilScript: TOpenDialog;
    PageControl: TPageControl;
    PanelButtons: TPanel;
    PanelSettings: TPanel;
    PopupMenuCharset: TPopupMenu;
    ButtonCustomBotScriptOpen: TSpeedButton;
    RadioButtonCustomScript: TRadioButton;
    RadioButtonChatbotServer: TRadioButton;
    RadioButtonWolframAlpha: TRadioButton;
    Splitter1: TSplitter;
    TabSheet1: TTabSheet;
    TabSheetEmbeddedServer: TTabSheet;
    TabSheetOptimization: TTabSheet;
    TabSheetSpeechRecognition: TTabSheet;
    TabSheetIMAP: TTabSheet;
    TabSheetGraphics: TTabSheet;
    TabSheetBot: TTabSheet;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonCharsetAddClick(Sender: TObject);
    procedure ButtonCustomBotScriptOpenClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ComboBoxSTTBackendChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBoxSettingsSelectionChange(Sender: TObject; User: boolean);
    procedure MenuItemDeleteCharsetClick(Sender: TObject);
    procedure RadioButtonWolframAlphaChange(Sender: TObject);
  private

  public

  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.lfm}

uses
  Globals, Mcdowell, Mcdowell.imap, State.Main, Mcdowell.SpeechToText,
  Utils.Strings,
  Mcdowell.smtp,
  Mcdowell.sketch,
  com.Brokers,
  Utils.Encdec;

{ TFormSettings }

procedure TFormSettings.FormShow(Sender: TObject);
var
  I, J, Tmp: Integer;
  L: TStringDynArray;
  S, S2: String;
  SL, SL2: TStringList;
begin
  PageControl.TabIndex := 0;
  PageControl.ShowTabs := False;
  ListboxSettings.ItemIndex := 0;
  ComboBoxSTTBackend.Items.Clear;
  ComboBoxSTTBackend.Items.Add('Vosk');
  {$ifdef WINDOWS}
  ComboBoxSTTBackend.Items.Add('Microsoft Speech Object Library');
  {$endif}
  ComboBoxSTTBackend.ItemIndex := Save.Settings.STTBackend;
  ComboBoxSTTBackendChange(Self);

  RadioButtonWolframAlpha.Checked := Save.Settings.ExternalServiceSelect = 0;
  RadioButtonChatbotServer.Checked := Save.Settings.ExternalServiceSelect = 1;
  RadioButtonCustomScript.Checked := Save.Settings.ExternalServiceSelect = 2;
  GroupBoxVolframAlpha.Enabled := RadioButtonWolframAlpha.Checked; 
  GroupBoxChatbotServer.Enabled := RadioButtonChatbotServer.Checked;
  GroupBoxCustomBotScript.Enabled := RadioButtonCustomScript.Checked;

  CheckBoxLewd.Checked := Save.Settings.Lewd;
  CheckBoxDeveloperMode.Checked := Save.Settings.DeveloperMode;
  ComboBoxCustomBotScriptType.ItemIndex := Save.Settings.CustomBotScriptType;
  EditCustomBotScript.Text := Save.Settings.CustomBotScript;
  EditBotServer.Text := Save.Settings.BotServer;
  EditBotVolframAlphaAppID.Text := Save.Settings.BotVolframAlphaAppID;
  EditFPS.Value := Save.Settings.FPS;
  EditChatBubbleDelay.Value := Save.Settings.ChatBubbleDelay;
  EditSoWRightMargin.Value := Save.Settings.SitOnWindowRightMargin;
  EditTextSpeed.Value := Save.Settings.TextSpeed;
  EditDefaultEvilScheme.Text := Save.Settings.DefaultEvilScheme;
  EditBaseScaling.Value := Save.Settings.BaseScaling;
  EditFrameSkip.Value := Save.Settings.FrameSkip;

  ComboBoxSkin.Items.Clear;
  SL := TStringList.Create;
  SL2 := TStringList.Create;
  FindAllDirectories(SL, 'data/sprites', False);
  FindAllDirectories(SL2, 'data/scripts', False);
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

  ComboBoxFont.Clear;
  SL := TStringList.Create;
  FindAllFiles(SL, 'data/fonts', '', False);
  for I := 0 to SL.Count - 1 do
  begin
    S := ExtractFileName(SL[I]);
    PageControl.ActivePageIndex := ListboxSettings.ItemIndex;
    ComboBoxFont.Items.Add(S);
    if S = Save.Settings.Font then
    begin
      ComboBoxFont.ItemIndex := ComboBoxFont.Items.Count - 1;
    end;
  end;
  SL.Free;

  CheckBoxEmbeddedServerEnable.Checked := Save.Settings.EmbeddedServerEnable;
  EditEmbeddedServerPort.Value := Save.Settings.EmbeddedServerPort;

  EditFontSize.Value := Save.Settings.FontSize;
  ListBoxCharset.Clear;
  EditCharsetFrom.Text := '';
  EditCharsetTo.Text := '';
  L := CharsetToSettings(Save.Settings.Charset);
  for S in L do
    if Trim(S) <> '' then
      ListBoxCharset.AddItem(S, nil);
  EditFontSize.Value := Save.Settings.FontSize;
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
end;

procedure TFormSettings.ListBoxSettingsSelectionChange(Sender: TObject;
  User: boolean);
begin
  PageControl.TabIndex := ListboxSettings.ItemIndex;
end;

procedure TFormSettings.MenuItemDeleteCharsetClick(Sender: TObject);
begin
  if ListBoxCharset.ItemIndex >= 0 then
    ListBoxCharset.Items.Delete(ListBoxCharset.ItemIndex);
end;

procedure TFormSettings.RadioButtonWolframAlphaChange(Sender: TObject);
begin
  GroupBoxVolframAlpha.Enabled := RadioButtonWolframAlpha.Checked;
  GroupBoxChatbotServer.Enabled := RadioButtonChatbotServer.Checked;
  GroupBoxCustomBotScript.Enabled := RadioButtonCustomScript.Checked;
end;

procedure TFormSettings.ButtonOkClick(Sender: TObject);
var
  IniFilePath: String;
begin
  try
    if RadioButtonWolframAlpha.Checked then
      Save.Settings.ExternalServiceSelect := 0
    else if RadioButtonChatbotServer.Checked then
      Save.Settings.ExternalServiceSelect := 1
    else if RadioButtonCustomScript.Checked then
      Save.Settings.ExternalServiceSelect := 2;

    Save.Settings.Lewd := CheckBoxLewd.Checked;
    Save.Settings.DeveloperMode := CheckBoxDeveloperMode.Checked; 
    Save.Settings.CustomBotScriptType := ComboBoxCustomBotScriptType.ItemIndex;
    Save.Settings.CustomBotScript := EditCustomBotScript.Text;
    Save.Settings.BotServer := EditBotServer.Text;
    Save.Settings.BotVolframAlphaAppID := EditBotVolframAlphaAppID.Text;
    Save.Settings.FPS := EditFPS.Value;
    Save.Settings.ChatBubbleDelay := EditChatBubbleDelay.Value;
    Save.Settings.SitOnWindowRightMargin := EditSoWRightMargin.Value;
    Save.Settings.TextSpeed := EditTextSpeed.Value;
    Save.Settings.BaseScaling := EditBaseScaling.Value;
    Save.Settings.DefaultEvilScheme := EditDefaultEvilScheme.Text;
    Save.Settings.ImageQuality := ComboBoxImageQuality.Items[ComboBoxImageQuality.ItemIndex];
    Save.Settings.FrameSkip := EditFrameSkip.Value;
    // Clear sketch and workers if skin is changed
    if Save.Settings.Skin <> ComboBoxSkin.Items[ComboBoxSkin.ItemIndex] then
    begin
      SataniaSketch.DeleteAll;
      Satania.BackgroundScriptClearAll;
    end;
    Save.Settings.Skin := ComboBoxSkin.Items[ComboBoxSkin.ItemIndex];
                                                                              Save.Settings.CustomBotScriptType := ComboBoxCustomBotScriptType.ItemIndex;
    Save.Settings.EmailServer := EditEmailServer.Text;
    Save.Settings.EmailPort := EditEmailPort.Value;
    Save.Settings.EmailUsername := EditEmailUsername.Text;
    Save.Settings.EmailFetchFrom := EditEmailFetchFrom.Text;

    Save.Settings.EmailSMTPServer := EditEmailSMTPServer.Text;
    Save.Settings.EmailSMTPPort := EditEmailSMTPPort.Value;
    Save.Settings.EmailSMTPUsername := EditEmailSMTPUsername.Text;

    Save.Settings.EmbeddedServerPort := EditEmbeddedServerPort.Value;     
    Save.Settings.EmbeddedServerEnable := CheckBoxEmbeddedServerEnable.Checked;

    Save.Settings.Font := ComboBoxFont.Items[ComboBoxFont.ItemIndex];
    Save.Settings.FontSize := EditFontSize.Value;
    Save.Settings.Charset := SettingsToCharset(TStringList(ListBoxCharset.Items));
    Save.Settings.FontSize := EditFontSize.Value;

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
    Save.SaveToFile('configs.json');
    SataniaIMAP.Disconnect;
    //
    ApplicationProperties.LimitFPS := Save.Settings.FPS;
    Satania.ChatText.TypingSpeed := Save.Settings.TextSpeed;
    Satania.SetImageQuality(Save.Settings.ImageQuality);
    Satania.UpdateMeta(Satania.Script);
    Satania.ActionFromFile(Save.Settings.DefaultEvilScheme);
    Satania.FontSystem.URL := PATH_FONT + Save.Settings.Font;
    Satania.FontSystem.OptimalSize := Save.Settings.FontSize;
    Satania.FontSystem.LoadCharacters := CharsetToCharacters(Save.Settings.Charset);
    Satania.ChatText.FontSize := Save.Settings.FontSize;
    Satania.SpriteAsSpine.AnimateSkipTicks := Save.Settings.FrameSkip;
    Satania.SpriteAsX3D.AnimateSkipTicks := Save.Settings.FrameSkip;
    Satania.UpdateMenuItems;

    // Load local flag
    Satania.LoadLocalFlags;

    if (ComboBoxSTTVoskModel.Items[ComboBoxSTTVoskModel.ItemIndex] <> Save.Settings.STTVoskModel)
      or (ComboBoxSTTBackend.ItemIndex <> Save.Settings.STTBackend) then
    begin
      Save.Settings.STTBackend := ComboBoxSTTBackend.ItemIndex;
      Save.Settings.STTVoskModel := ComboBoxSTTVoskModel.Items[ComboBoxSTTVoskModel.ItemIndex];
      if Save.SpeechToText then
        SataniaSpeechToText.Enable;
    end;

    Hide;
  except
    on E: Exception do
      Satania.Talk(E.Message);
  end;
end;

procedure TFormSettings.ComboBoxSTTBackendChange(Sender: TObject);
begin
  if ComboBoxSTTBackend.ItemIndex <> SPEECH_RECOGNIZER_BACKEND_VOSK then
    GroupBoxSTTVosk.Enabled := False
  else
    GroupBoxSTTVosk.Enabled := True;
end;

procedure TFormSettings.ButtonCancelClick(Sender: TObject);
begin
  Hide;
end;

procedure TFormSettings.ButtonCharsetAddClick(Sender: TObject);
var
  X, Y: Cardinal;
  S: String;
begin
  try
    X := StrToInt(EditCharsetFrom.Text);
    Y := StrToInt(EditCharsetTo.Text);
    S := IntToStr(X) + '..' + IntToStr(Y);
    ListBoxCharset.AddItem(S, nil);
  except
    on E: Exception do
      Satania.Talk(E.Message);
  end;
end;

procedure TFormSettings.ButtonCustomBotScriptOpenClick(Sender: TObject);
begin
  if OpenDialogEvilScript.Execute then
  begin
    Self.EditCustomBotScript.Text := OpenDialogEvilScript.FileName;
  end;
end;

end.

