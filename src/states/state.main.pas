unit State.Main;

{$I configs}

interface

uses Classes,
  Forms,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene,
  X3DNodes, CastleBoxes, CastleRectangles, CastleTypingLabel, CastleViewport,
  CastleFonts, LCLTranslator,
  Globals;

type
  TStateMain = class(TUIState)
  private
    procedure UpdateTouchPanelPosition;
    procedure UpdateChatBubblePosition;
  public
    ChatText: TCastleTypingLabel;
    ChatBubble: TCastleRectangleControl;  
    ChatBubbleArrow: TCastleImageControl;
    FontSystem: TCastleFont;
    Sprite: TCastleScene;
    Viewport: TCastleViewport;
    BubbleSideX, BubbleSideY: Integer;
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

  Sprite := DesignedComponent('Sprite') as TCastleScene;
  Viewport := DesignedComponent('Viewport') as TCastleViewport;
  ChatText.TypingSpeed := Save.Settings.TextSpeed;

  //
  Satania.Sprite := Sprite;
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

procedure TStateMain.UpdateTouchPanelPosition;
var
  Position: TVector3;
  ScreenPosition: TVector2Integer;
begin
  if Satania.TouchBone <> nil then
    Position := Sprite.WorldTransform.MultPoint(Satania.TouchBone.Translation)
  else
    Position := Sprite.Translation;
  ScreenPosition := UIToScreenCoord(Position); 
  FormTouch.Left := ScreenPosition.X - FormTouch.Width div 2;
  {$ifdef LINUX_X11}
  FormTouch.Top := ScreenPosition.Y - FormTouch.Height div 2;
  {$else}
  FormTouch.Top := ScreenPosition.Y - FormTouch.Height div 2;
  {$endif}
end;

procedure TStateMain.UpdateChatBubblePosition;
var
  Position: TVector3;
  Box: TBox3D;
  R, RA: TFloatRectangle;
begin  
  if ChatText.Text.Text <> '' then
    ChatBubble.Exists := True and Sprite.Visible
  else
    ChatBubble.Exists := False;
  ChatBubbleArrow.Exists := ChatBubble.Exists;
  if not ChatBubble.Exists then Exit;

  Box := Satania.LocalBoundingBoxSnapshot;
  Box.Data[0] := Box.Data[0] + Sprite.Translation; 
  Box.Data[1] := Box.Data[1] + Sprite.Translation;
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
  // Hide arrow if size is larger than bubble
  if ChatBubble.EffectiveWidth < ChatBubbleArrow.EffectiveWidth then
    ChatBubbleArrow.Exists := False
  else
    ChatBubbleArrow.Exists := True;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  // LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
  UpdateTouchPanelPosition;
  UpdateChatBubblePosition;
  Satania.Update(SecondsPassed);
  Forms.Application.ProcessMessages;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit;
end;

end.
