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

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls, Buttons, Spin, MaskEdit, Menus, CastleApplicationProperties,
  Types, LCLTranslator;

type

  { TFormSettings }

  TFormSettings = class(TForm)
    ButtonCharsetAdd: TBitBtn;
    ButtonCancel: TBitBtn;
    ButtonOk: TBitBtn;
    CheckBoxEmailUseSSL: TCheckBox;
    CheckBoxLewd: TCheckBox;
    ComboBoxSTTBackend: TComboBox;
    ComboBoxImageQuality: TComboBox;
    EditBotServer: TEdit;
    EditCharsetTo: TLabeledEdit;
    EditDefaultEvilScheme: TEdit;
    EditChatBubbleDelay: TSpinEdit;
    EditSkin: TEdit;
    EditFrameSkip: TSpinEdit;
    EditFontName: TEdit;
    EditEmailFetchFrom: TEdit;
    EditEmailServer: TEdit;
    EditEmailUsername: TEdit;
    EditEmailPassword: TEdit;
    EditFPS: TSpinEdit;
    EditEmailPort: TSpinEdit;
    EditSoWRightMargin: TSpinEdit;
    EditFontSize: TSpinEdit;
    EditSTTDict: TEdit;
    EditSTTModel: TEdit;
    EditSTTNgram: TEdit;
    EditTextSpeed: TSpinEdit;
    EditBaseScaling: TFloatSpinEdit;
    GroupBoxSTTPocketSphinx: TGroupBox;
    LabelChatbotServer: TLabel;
    LabelSTTBackend: TLabel;
    LabelEmailUsername: TLabel;
    LabelEmailPassword: TLabel;
    LabelEmailUseSSL: TLabel;
    LabelFetchFrom: TLabel;
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
    LabelSTTDict: TLabel;
    LabelSTTModel: TLabel;
    LabelSTTNgram: TLabel;
    LabelTextSpeed: TLabel;
    LabelDefaultEvilScheme: TLabel;
    LabelImageQuality: TLabel;
    LabelEmailServer: TLabel;
    LabelEmailPort: TLabel;
    EditCharsetFrom: TLabeledEdit;
    ListBoxCharset: TListBox;
    MenuItemDeleteCharset: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PopupMenuCharset: TPopupMenu;
    TabSheet1: TTabSheet;
    TabSheetSpeechRecognition: TTabSheet;
    TabSheetEmail: TTabSheet;
    TabSheetGraphics: TTabSheet;
    TabSheetBot: TTabSheet;
    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonCharsetAddClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure ComboBoxSTTBackendChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemDeleteCharsetClick(Sender: TObject);
  private

  public

  end;

var
  FormSettings: TFormSettings;

implementation

{$R *.lfm}

uses
  Globals, Mcdowell, Mcdowell.imap, State.Main, Mcdowell.SpeechToText;

{ TFormSettings }

procedure TFormSettings.FormShow(Sender: TObject);
var
  I: Integer;
  L: TStringDynArray;
  S: String;
begin
  ComboBoxSTTBackend.Items.Clear;
  ComboBoxSTTBackend.Items.Add('PocketSphinx');   
  ComboBoxSTTBackend.Items.Add('Microsoft Speech Object Library');
  {$ifdef WINDOWS}
  ComboBoxSTTBackend.ItemIndex := Save.Settings.STTBackend;
  {$else}
  ComboBoxSTTBackend.ItemIndex := 0;
  ComboBoxSTTBackend.Enabled := False;
  {$endif}

  CheckBoxLewd.Checked := Save.Settings.Lewd;
  EditBotServer.Text := Save.Settings.BotServer;
  EditFPS.Value := Save.Settings.FPS;
  EditChatBubbleDelay.Value := Save.Settings.ChatBubbleDelay;
  EditSoWRightMargin.Value := Save.Settings.SitOnWindowRightMargin;
  EditTextSpeed.Value := Save.Settings.TextSpeed;
  EditDefaultEvilScheme.Text := Save.Settings.DefaultEvilScheme;
  EditBaseScaling.Value := Save.Settings.BaseScaling;
  EditFrameSkip.Value := Save.Settings.FrameSkip;
  EditSkin.Text := Save.Settings.Skin;

  EditEmailServer.Text := Save.Settings.EmailServer;
  EditEmailPort.Value := Save.Settings.EmailPort;
  EditEmailUsername.Text := Save.Settings.EmailUsername;
  EditEmailFetchFrom.Text := Save.Settings.EmailFetchFrom;

  EditSTTModel.Text := Save.Settings.STTModel;
  EditSTTDict.Text := Save.Settings.STTDict;
  EditSTTNgram.Text := Save.Settings.STTNgram;

  EditFontName.Text := Save.Settings.Font;
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
  CheckBoxEmailUseSSL.Checked := Save.Settings.EmailUseSSL;

  ComboBoxImageQuality.ItemIndex := 1;
  for I := 0 to ComboBoxImageQuality.Items.Count - 1 do
    if ComboBoxImageQuality.Items[I] = Save.Settings.ImageQuality then
    begin
      ComboBoxImageQuality.ItemIndex := I;
      break;
    end;
end;

procedure TFormSettings.MenuItemDeleteCharsetClick(Sender: TObject);
begin
  if ListBoxCharset.ItemIndex >= 0 then
    ListBoxCharset.Items.Delete(ListBoxCharset.ItemIndex);
end;

procedure TFormSettings.ButtonOkClick(Sender: TObject);
begin
  try
    Save.Settings.Lewd := CheckBoxLewd.Checked;
    Save.Settings.BotServer := EditBotServer.Text;
    Save.Settings.FPS := EditFPS.Value;
    Save.Settings.ChatBubbleDelay := EditChatBubbleDelay.Value;
    Save.Settings.SitOnWindowRightMargin := EditSoWRightMargin.Value;
    Save.Settings.TextSpeed := EditTextSpeed.Value;
    Save.Settings.BaseScaling := EditBaseScaling.Value;
    Save.Settings.DefaultEvilScheme := EditDefaultEvilScheme.Text;
    Save.Settings.ImageQuality := ComboBoxImageQuality.Items[ComboBoxImageQuality.ItemIndex];
    Save.Settings.FrameSkip := EditFrameSkip.Value;
    Save.Settings.Skin := EditSkin.Text;

    Save.Settings.EmailServer := EditEmailServer.Text;
    Save.Settings.EmailPort := EditEmailPort.Value;
    Save.Settings.EmailUsername := EditEmailUsername.Text;
    Save.Settings.EmailFetchFrom := EditEmailFetchFrom.Text;

    Save.Settings.Font := EditFontName.Text;
    Save.Settings.FontSize := EditFontSize.Value;
    Save.Settings.Charset := SettingsToCharset(TStringList(ListBoxCharset.Items));
    Save.Settings.FontSize := EditFontSize.Value;

    if EditEmailPassword.Text <> '' then
      Save.Settings.EmailPassword := Encrypt(EditEmailPassword.Text)
    else
      Save.Settings.EmailPassword := '';

    Save.Settings.EmailUseSSL := CheckBoxEmailUseSSL.Checked;
    Save.SaveToFile('configs.json');
    SataniaIMAP.Disconnect;
    //
    ApplicationProperties.LimitFPS := Save.Settings.FPS;
    Satania.ChatText.TypingSpeed := Save.Settings.TextSpeed;
    Satania.SetImageQuality(Save.Settings.ImageQuality);
    Satania.ActionFromFile(Save.Settings.DefaultEvilScheme);
    Satania.FontSystem.URL := PATH_FONT + Save.Settings.Font;
    Satania.FontSystem.OptimalSize := Save.Settings.FontSize;
    Satania.FontSystem.LoadCharacters := CharsetToCharacters(Save.Settings.Charset);
    Satania.ChatText.FontSize := Save.Settings.FontSize;
    Satania.Sprite.AnimateSkipTicks := Save.Settings.FrameSkip;
    Satania.UpdateMenuItems;

    if (EditSTTModel.Text <> Save.Settings.STTModel)
      or (EditSTTDict.Text <> Save.Settings.STTDict)
      or (EditSTTNgram.Text <> Save.Settings.STTNgram)
      or (ComboBoxSTTBackend.ItemIndex <> Save.Settings.STTBackend) then
    begin                             
      Save.Settings.STTBackend := ComboBoxSTTBackend.ItemIndex;
      Save.Settings.STTModel := EditSTTModel.Text;
      Save.Settings.STTDict := EditSTTDict.Text;
      Save.Settings.STTNgram := EditSTTNgram.Text;
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
  if ComboBoxSTTBackend.ItemIndex <> 0 then
    GroupBoxSTTPocketSphinx.Enabled := False
  else
    GroupBoxSTTPocketSphinx.Enabled := True;
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

end.

