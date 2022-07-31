unit State.Main;

{$I configs}

interface

uses Classes,
  Forms,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleTransform,
  X3DNodes, CastleBoxes, CastleRectangles, CastleTypingLabel, CastleViewport,
  CastleFonts, LCLTranslator, CastleSceneCore, CastleSpine,
  Globals;

type
  TStateMain = class(TUIState)
  private
    procedure UpdateTouchPanelPosition;
    procedure UpdateChatBubblePosition;
    procedure UpdateSataniaPositionBasedOnMonitor;
  public
    ChatText: TCastleTypingLabel;
    ChatBubble: TCastleRectangleControl;
    ChatBubbleArrow: TCastleImageControl;
    FontSystem: TCastleFont;
    SpriteTransform: TCastleTransform;
    SpriteAsX3D: TCastleScene;                   
    SpriteAsSpine: TCastleSpine;
    Sprite: TCastleSceneCore;
    Viewport: TCastleViewport;
    BubbleSideX, BubbleSideY,
    AskSideX, AskSideY: Integer;
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses
  SysUtils,
  CastleWindow,
  Form.Touch,
  Form.Main,
  Form.ChatBubble,
  Utils.Strings,
  Utils.Coords,
  Mcdowell;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/ui-main.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  ChatText := DesignedComponent('ChatText') as TCastleTypingLabel;
  ChatText.Text.Text := '';
  ChatBubble := DesignedComponent('ChatBubble') as TCastleRectangleControl;
  ChatBubble.Exists := False;
  ChatBubbleArrow := DesignedComponent('ChatBubbleArrow') as TCastleImageControl;
  ChatBubbleArrow.Exists := False;
  FontSystem := DesignedComponent('FontSystem') as TCastleFont;

  SpriteAsX3D := DesignedComponent('Sprite') as TCastleScene;     
  SpriteAsSpine := DesignedComponent('Spine') as TCastleSpine;
  Sprite := SpriteAsX3D;
  SpriteTransform := DesignedComponent('SpriteTransform') as TCastleTransform;
  Viewport := DesignedComponent('Viewport') as TCastleViewport;
  ChatText.TypingSpeed := Save.Settings.TextSpeed;

  //
  Satania.Sprite := Sprite;
  Satania.SpriteAsSpine := Self.SpriteAsSpine;
  Satania.SpriteAsX3D := Self.SpriteAsX3D;
  Satania.Viewport := Viewport;
  Satania.ChatText := ChatText;
  Satania.ChatBubble := ChatBubble;
  Satania.FontSystem := FontSystem;
  try
    BubbleSideY := 1;
    Satania.DefaultPosition;
    Satania.ActionFromFile(Save.Settings.DefaultEvilScheme);
    Satania.SetImageQuality(Save.Settings.ImageQuality);
    FontSystem.URL := PATH_FONT + Save.Settings.Font;
    FontSystem.OptimalSize := Save.Settings.FontSize;
    FontSystem.LoadCharacters := CharsetToCharacters(Save.Settings.Charset);
    ChatText.FontSize := Save.Settings.FontSize;
    Sprite.AnimateSkipTicks := Save.Settings.FrameSkip;
  except
    on E: Exception do
      Satania.Talk(E.Message);
  end;
end;

procedure TStateMain.UpdateSataniaPositionBasedOnMonitor;
var
  V: TVector3;
begin
  V := TVector3.Zero;
  if FormMain.Monitor.Left > 0 then
  begin
    V.X := V.X - FormMain.Monitor.Left;
  end;
  SpriteTransform.Translation := V;
end;

procedure TStateMain.UpdateTouchPanelPosition;
var
  Position: TVector3;
  ScreenPosition: TVector2Integer;
begin
  if Satania.TouchBone <> nil then
    Position := Satania.TouchBone.WorldTranslation
  else
    Position := Satania.Sprite.Translation;
  ScreenPosition := UIToScreenCoord(Position);
  FormTouch.Left := ScreenPosition.X - FormTouch.Width div 2 - Round(SpriteTransform.Translation.X);
  FormTouch.Top := ScreenPosition.Y - FormTouch.Height div 2;
end;

procedure TStateMain.UpdateChatBubblePosition;
var
  Box: TBox3D;
  R, RA: TFloatRectangle;
begin
  if ChatText.Text.Text <> '' then
  begin
    FormChatBubble.Visible := Satania.IsAsking;
    ChatBubble.Exists := True and Satania.Sprite.Visible and not Satania.IsAsking;
  end else
  begin
    FormChatBubble.Visible := False;
    ChatBubble.Exists := False;
  end;
  ChatBubbleArrow.Exists := ChatBubble.Exists and not Satania.IsAsking;
  if (not ChatBubble.Exists) and (not FormChatBubble.Visible) then Exit;

  Box := Satania.LocalBoundingBoxSnapshot;
  Box.Data[0] := Box.Data[0] + Satania.Sprite.Translation + SpriteTransform.Translation;
  Box.Data[1] := Box.Data[1] + Satania.Sprite.Translation + SpriteTransform.Translation;
  R := ChatBubble.EffectiveRect;
  RA := ChatBubbleArrow.EffectiveRect;
  case BubbleSideX of
    0:
      begin
        ChatBubble.Left := Box.Min.X - R.Width;
        ChatBubbleArrow.Left := ChatBubble.Left + R.Width - RA.Width - 4;
        ChatBubbleArrow.FlipHorizontal := False;
        if R.Left < 0 then
          BubbleSideX := 1;
      end;
    1:
      begin
        ChatBubble.Left := Box.Max.X;
        ChatBubbleArrow.Left := ChatBubble.Left + 4;
        ChatBubbleArrow.FlipHorizontal := True;
        if R.Left + R.Width > R.Width * 2 + (Box.Max.X - Box.Min.X) then
          BubbleSideX := 0;
      end;
  end;
  case BubbleSideY of
    0:
      begin
        ChatBubble.Bottom := Box.Min.Y - R.Height;
        ChatBubbleArrow.Bottom := ChatBubble.Bottom + R.Height - 4;
        ChatBubbleArrow.FlipVertical := True;
        if R.Bottom + R.Height * 2 + (Box.Max.Y - Box.Min.Y) < Application.ScreenHeight then
          BubbleSideY := 1;
      end;
    1:
      begin
        ChatBubble.Bottom := Box.Max.Y;
        ChatBubbleArrow.Bottom := ChatBubble.Bottom - RA.Height + 4;
        ChatBubbleArrow.FlipVertical := False;
        if R.Bottom + R.Height > Application.ScreenHeight then
          BubbleSideY := 0;
      end;
  end;
  case AskSideX of
    0:
      begin
        FormChatBubble.Left := Round(Box.Min.X - FormChatBubble.Width) + FormMain.Monitor.Left;
        if FormChatBubble.Left < FormMain.Monitor.Left then
          AskSideX := 1;
      end;
    1:
      begin
        FormChatBubble.Left := Round(Box.Max.X) + FormMain.Monitor.Left;
        if FormChatBubble.Left + FormChatBubble.Width > FormChatBubble.Width * 2 + (Box.Max.X - Box.Min.X) + FormMain.Monitor.Left then
          AskSideX := 0;
      end;
  end;
  case AskSideY of
    0:
      begin
        FormChatBubble.Top := UIToScreenCoord(Box.Max.Y) - FormChatBubble.Height;
        if FormChatBubble.Top < 0 then
          AskSideY := 1;
      end;
    1:
      begin
        FormChatBubble.Top := UIToScreenCoord(Box.Min.Y);
        if FormChatBubble.Top + FormChatBubble.Height > Application.ScreenHeight then
          AskSideY := 0;
      end;
  end;
  // Hide arrow if size is larger than bubble
  if ChatBubble.EffectiveWidth < ChatBubbleArrow.EffectiveWidth then
    ChatBubbleArrow.Exists := False
  else
    ChatBubbleArrow.Exists := ChatBubble.Exists and not Satania.IsAsking;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  // LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  try
    UpdateSataniaPositionBasedOnMonitor;
    UpdateTouchPanelPosition;
    UpdateChatBubblePosition;
    Satania.Update(SecondsPassed);
    Forms.Application.ProcessMessages;
  except
    on E: Exception do
      Satania.Log('System', E.Message);
  end;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;
end;

end.
