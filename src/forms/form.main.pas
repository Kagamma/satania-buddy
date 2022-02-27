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

unit Form.Main;

{$I configs.inc}

interface

uses
  {$define unit_declare_uses}
  {$I form.main_windows.inc}
  {$I form.main_linux_x11.inc}
  {$undef unit_declare_uses}
  CastleApplicationProperties, CastleUIControls, CastleUIState, FileUtil,
  CastleSceneCore, CastleScene,
  CastleVectors, {$ifdef WINDOWS}CastleControl,{$else}OpenGLContext,{$endif} CastleWindow,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  LCLType, StdCtrls, PopupNotifier, Globals,
  Mcdowell, LCLTranslator;

type

  { TFormMain }
  {$ifdef LINUX_X11}
  TCastleControl = class(TCustomOpenGLControl)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Enabled;
    property OpenGLMajorVersion;
    property OpenGLMinorVersion;
    property MultiSampling;
    property AlphaBits;
    property DepthBits;
    property StencilBits;
    property AUXBuffers;
    property Options;
    property OnChangeBounds;
    property OnConstrainedResize;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEnter;
    property OnExit;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
    property TabOrder;
    property TabStop default true;
  end;
  {$endif}

  TFormMain = class(TForm)
    CastleControl: TCastleControl;
    MainMenu1: TMainMenu;
    MenuItemSilent: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemRefreshMenu: TMenuItem;
    MenuItemEditor: TMenuItem;
    MenuItemActions: TMenuItem;
    MenuItemLearn: TMenuItem;
    MenuItemHideShow: TMenuItem;
    MenuItemChatWithHer: TMenuItem;
    MenuItemSettings: TMenuItem;
    MenuItemSitOnWindow: TMenuItem;
    MenuItemQuit: TMenuItem;
    PopupNotifier: TPopupNotifier;
    Separator1: TMenuItem;
    TimerMainLoop: TTimer;
    TrayMenu: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemChatWithHerClick(Sender: TObject);
    procedure MenuItemEditorClick(Sender: TObject);
    procedure MenuItemHideShowClick(Sender: TObject);
    procedure MenuItemLearnClick(Sender: TObject);
    procedure MenuItemQuitClick(Sender: TObject);
    procedure MenuItemRefreshMenuClick(Sender: TObject);
    procedure MenuItemSettingsClick(Sender: TObject);
    procedure MenuItemSilentClick(Sender: TObject);
    procedure MenuItemSitOnWindowClick(Sender: TObject);
    procedure TimerMainLoopTimer(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
  private
    { private declarations }
    procedure InitCommon;
  public
    Ticks: QWord;
    {$define unit_public}
    {$I form.main_windows.inc}
    {$I form.main_linux_x11.inc}
    {$undef unit_public}
    procedure DoExecuteScriptFromMenu(Sender: TObject);
  end;

{$define unit_declare_interface}
{$I form.main_windows.inc}
{$I form.main_linux_x11.inc}
{$undef unit_declare_interface}

var
  FormMain: TFormMain;
  Window: TCastleWindow;

implementation

{$R *.lfm}

uses
  Utils.ActiveWindow,
  Form.Touch,
  Form.Settings,
  Form.Chat,
  Form.evilc.editor,
  Mcdowell.chatbot.train,
  State.Main;

{$define unit_implmentation}
{$I form.main_windows.inc}
{$I form.main_linux_x11.inc}
{$undef unit_implmentation}

{ TFormMain }

procedure TFormMain.DoExecuteScriptFromMenu(Sender: TObject);
begin
  Satania.ActionFromFile('menu/' + (Sender as TMenuItem).Caption + '.evil');
end;

procedure TFormMain.InitCommon;
var
  StateMain: TStateMain;
  S: String;
  I: Integer;
begin
  if UtilActiveWindow = nil then
  begin
    Ticks := GetTickCount64;
    UtilActiveWindow := TUtilActiveWindow.Create;
    Satania.Form := Self;
    Satania.FormTouch := FormTouch;

    MenuItemSitOnWindow.Checked := Save.SitOnWindow;
    MenuItemSilent.Checked := Save.Silent;

    {$if defined(WINDOWS)}
    TCastleControl.MainControl := CastleControl;
    CastleControl.Container.BackgroundColor := Vector4(0, 0, 0, 0);
    ScreenWidth := GetSystemMetrics(SM_CXSCREEN);
    ScreenHeight := GetSystemMetrics(SM_CYSCREEN);
    {$elseif defined(LINUX_X11)}
    Hide;
    RemoveControl(CastleControl);
    Window := TCastleWindow.Create(Application);
    CastleWindow.Application.MainWindow := Window;
    Window.Container.BackgroundColor := Vector4(0, 0, 0, 0);
    CastleWindow.Application.MainWindow.Open;
    ScreenWidth := CastleWindow.Application.ScreenWidth;
    ScreenHeight := CastleWindow.Application.ScreenHeight;
    {$endif}

    ApplicationProperties.LimitFPS := Save.Settings.FPS;
    // OptimizeExtensiveTransformations := True;
    DynamicBatching := True;
    StateMain := TStateMain.Create(Self);
    TUIState.Current := StateMain;

    TimerMainLoop.Enabled := true;
    FormTouch.Show;

    // Generate script menu
    Satania.UpdateMenuItems;
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  InitCommon;
end;

procedure TFormMain.MenuItemAboutClick(Sender: TObject);
begin
  Satania.Talk('Satania Buddy'#10'Homepage: https://kgm.itch.io/satania-buddy');
end;

procedure TFormMain.MenuItemChatWithHerClick(Sender: TObject);
begin
  FormChat.Show;
end;

procedure TFormMain.MenuItemEditorClick(Sender: TObject);
begin
  FormEvilCEditor.Show;
end;

procedure TFormMain.MenuItemHideShowClick(Sender: TObject);
begin
  if Satania.Sprite.Visible then
  begin
    MenuItemHideShow.Caption := 'Show';
    Satania.Sprite.Visible := False;
    FormTouch.Hide;
  end else
  begin
    MenuItemHideShow.Caption := 'Hide';
    Satania.Sprite.Visible := True;
    FormTouch.Show;
  end;
end;

procedure TFormMain.MenuItemLearnClick(Sender: TObject);
begin
  RunTrain;
end;

procedure TFormMain.MenuItemQuitClick(Sender: TObject);
begin
  Halt(0);
end;

procedure TFormMain.MenuItemRefreshMenuClick(Sender: TObject);
begin
  Satania.UpdateMenuItems;
end;

procedure TFormMain.MenuItemSettingsClick(Sender: TObject);
begin
  FormSettings.Show;
end;

procedure TFormMain.MenuItemSilentClick(Sender: TObject);
begin
  Save.Silent := not Save.Silent;
  MenuItemSilent.Checked := Save.Silent;
  Satania.ActionFromFile(Save.Settings.DefaultEvilScheme);
end;

procedure TFormMain.MenuItemSitOnWindowClick(Sender: TObject);
begin
  Save.SitOnWindow := not Save.SitOnWindow;
  MenuItemSitOnWindow.Checked := Save.SitOnWindow;
  if not Save.SitOnWindow then
    Satania.DefaultPosition;
end;

procedure TFormMain.TimerMainLoopTimer(Sender: TObject);
begin
  if Save.SitOnWindow and not Satania.IsBlocked then
    UtilActiveWindow.Update;
  {$ifdef LINUX_X11}
  CastleWindow.Application.DoRun;
  {$endif}
  // Hide satania bubble
  if (Satania.ChatText.FinishedTyping) and (Satania.ChatBubbleDelay > 0) then
  begin
    Dec(Satania.ChatBubbleDelay, GetTickCount64 - Ticks);
    if Satania.ChatBubbleDelay <= 0 then
    begin
      Satania.ChatText.Text.Text := '';
    end;
  end;
  Ticks := GetTickCount64;
end;

procedure TFormMain.TrayIconDblClick(Sender: TObject);
begin
  if not Satania.Sprite.Visible then
  begin
    MenuItemHideShow.Caption := 'Hide';
    Satania.Sprite.Visible := True;
    FormTouch.Show;
  end;
end;

initialization

end.
