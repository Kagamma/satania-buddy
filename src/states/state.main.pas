unit State.Main;

{$I configs}

interface

uses Classes,
  Forms,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleTransform,
  X3DNodes, CastleBoxes, CastleRectangles, CastleViewport,
  CastleFonts, LCLTranslator, CastleSceneCore, CastleSpine, CastleSpineMixer,
  Globals;

type
  TStateMain = class(TUIState)
  private
    procedure UpdateTouchPanelPosition;
    procedure UpdateChatBubblePosition;
    procedure UpdateSataniaPositionBasedOnMonitor;
  public
    FontSystem: TCastleFont;
    SpriteTransform: TCastleTransform;
    Mixer: TCastleSpineMixerBehavior;
    SpriteAsX3D: TCastleScene;
    SpriteAsSpine: TCastleSpine;
    Sprite: TCastleSceneCore;
    SketchBefore,
    SketchAfter: TCastleTransform;
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
  form.ask,
  Form.Bubble,
  form.main,
  form.touch,
  form.chat,
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
  FontSystem := DesignedComponent('FontSystem') as TCastleFont;

  SpriteAsX3D := DesignedComponent('Sprite') as TCastleScene;
  SpriteAsSpine := DesignedComponent('Spine') as TCastleSpine;
  Mixer := DesignedComponent('Mixer') as TCastleSpineMixerBehavior;
  Sprite := SpriteAsX3D;
  SpriteTransform := DesignedComponent('SpriteTransform') as TCastleTransform;
  Viewport := DesignedComponent('Viewport') as TCastleViewport;
  SketchBefore := DesignedComponent('SketchBefore') as TCastleTransform;
  SketchAfter := DesignedComponent('SketchAfter') as TCastleTransform;

  //
  Satania.Sprite := Sprite;
  Satania.SpriteAsSpine := Self.SpriteAsSpine;
  Satania.SpriteAsX3D := Self.SpriteAsX3D;
  Satania.Viewport := Viewport;
  Satania.FontSystem := FontSystem;
  Satania.SketchBefore := Self.SketchBefore;
  Satania.SketchAfter := Self.SketchAfter;
  Satania.Mixer := Self.Mixer;
  try
    BubbleSideY := 1;
    Satania.DefaultPosition;
    Satania.ActionFromFile(Save.Settings.DefaultEvilScheme);
    Satania.SetImageQuality(Save.Settings.ImageQuality);
    Sprite.AnimateSkipTicks := Save.Settings.FrameSkip;
  except
    on E: Exception do
    begin
      Satania.Error(E.Message);
      DumpExceptionCallStack(E);
    end;
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
  if FormBubble.Text <> '' then
  begin
    FormAsk.Visible := Satania.IsAsking;
    if Save.Settings.ChatSpeechBalloon and FormChat.Visible and (FormChat.WindowState <> wsMinimized) then
      FormBubble.VisibleViaSize := False
    else
      FormBubble.VisibleViaSize := True and Satania.Sprite.Visible and not Satania.IsAsking;
  end else
  begin
    FormAsk.Visible := False;
    FormBubble.VisibleViaSize := False;
  end;
  if (not FormBubble.VisibleViaSize) and (not FormAsk.Visible) then Exit;

  Box := Satania.LocalBoundingBoxSnapshot;
  Box.Data[0] := Box.Data[0] + Satania.Sprite.Translation + SpriteTransform.Translation;
  Box.Data[1] := Box.Data[1] + Satania.Sprite.Translation + SpriteTransform.Translation;
  if FormAsk.IsPositionUpdated < 15 then
  begin
    case AskSideX of
      0:
        begin
          FormAsk.Left := Round(Box.Min.X - FormAsk.Width) + FormMain.Monitor.Left;
          if FormAsk.Left < FormMain.Monitor.Left then
            AskSideX := 1;
        end;
      1:
        begin
          FormAsk.Left := Round(Box.Max.X) + FormMain.Monitor.Left;
          if FormAsk.Left + FormAsk.Width > FormAsk.Width * 2 + (Box.Max.X - Box.Min.X) + FormMain.Monitor.Left then
            AskSideX := 0;
        end;
    end;
    case AskSideY of
      0:
        begin
          FormAsk.Top := UIToScreenCoord(Box.Max.Y) - FormAsk.Height;
          if FormAsk.Top < 0 then
            AskSideY := 1;
        end;
      1:
        begin
          FormAsk.Top := UIToScreenCoord(Box.Min.Y);
          if FormAsk.Top + FormAsk.Height > Application.ScreenHeight then
            AskSideY := 0;
        end;
    end;
    Inc(FormAsk.IsPositionUpdated);
  end;

  case BubbleSideX of
    0:
      begin
        FormBubble.Left := Round(Box.Min.X - FormBubble.Width) + FormMain.Monitor.Left;
        if FormBubble.Left < FormMain.Monitor.Left then 
        begin
          BubbleSideX := 1;
          FormBubble.Invalidate;
        end;
      end;
    1:
      begin
        FormBubble.Left := Round(Box.Max.X) + FormMain.Monitor.Left;
        if FormBubble.Left + FormBubble.Width > FormBubble.Width * 2 + (Box.Max.X - Box.Min.X) + FormMain.Monitor.Left then  
        begin
          BubbleSideX := 0;
          FormBubble.Invalidate;
        end;
      end;
  end;
  case BubbleSideY of
    0:
      begin
        FormBubble.Top := UIToScreenCoord(Box.Max.Y) - FormBubble.Height;
        if FormBubble.Top < 0 then  
        begin
          BubbleSideY := 1;
          FormBubble.Invalidate;
        end;
      end;
    1:
      begin
        FormBubble.Top := UIToScreenCoord(Box.Min.Y);
        FormBubble.Top := UIToScreenCoord(Box.Min.Y);
        if FormBubble.Top + FormBubble.Height > Application.ScreenHeight then    
        begin
          BubbleSideY := 0;
          FormBubble.Invalidate;
        end;
      end;
  end;
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
    Self.SketchBefore.Translation := Satania.Sprite.Translation;
    Self.SketchAfter.Translation := Satania.Sprite.Translation;
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
