unit utils.sprites;

{$I configs.inc}

interface

uses
  Classes, SysUtils, CastleScene, CastleSceneCore, CastleRenderOptions, X3DNodes,
  CastleSpine, globals;

procedure SpriteSetFilter(const Sprite: TCastleSceneCore; const S: String);
procedure SpriteSetAnimationSpeed(const Sprite: TCastleSceneCore; const AnimName: String; const Speed: Single);
procedure SpriteStartAnimation(const Sprite: TCastleSceneCore; const AnimName: String; const IsRepeat: Boolean);
procedure SpriteStopAnimation(const Sprite: TCastleSceneCore; const AnimName: String);
procedure SpriteStopAllAnimations(const Sprite: TCastleSceneCore);

var
  TrackDict: TTrackDict;

implementation

uses
  Mcdowell;

procedure SpriteSetFilter(const Sprite: TCastleSceneCore; const S: String);
begin
  if Sprite is TCastleScene then
  begin
    case S of
      'Linear':
        begin
          TCastleScene(Sprite).RenderOptions.MinificationFilter := minLinear;
          TCastleScene(Sprite).RenderOptions.MagnificationFilter := magLinear;
        end;
      'Nicest':
        begin
          TCastleScene(Sprite).RenderOptions.MinificationFilter := minNicest;
          TCastleScene(Sprite).RenderOptions.MagnificationFilter := magNicest;
        end;
      else
        begin
          TCastleScene(Sprite).RenderOptions.MinificationFilter := minNearest;
          TCastleScene(Sprite).RenderOptions.MagnificationFilter := magNearest;
        end;
    end;
  end else
  if Sprite is TCastleSpine then
  begin
    case S of
      'Nearest':
        begin
          TCastleSpine(Sprite).SmoothTexture := False;
        end;
      else
        begin
          TCastleSpine(Sprite).SmoothTexture := True;
        end;
    end;
  end;
end;

procedure SpriteSetAnimationSpeed(const Sprite: TCastleSceneCore; const AnimName: String; const Speed: Single);
var
  TimeSensor: TTimeSensorNode;
begin     
  if Sprite is TCastleScene then
  begin
    try
      TimeSensor := Sprite.Node(AnimName) as TTimeSensorNode;
      TimeSensor.FdCycleInterval.Value := Speed;
    except
      on E: Exception do
        Satania.TalkWithoutBlock(E.Message);
    end;
  end else
  if Sprite is TCastleSpine then
  begin
    // TODO
  end;
end;    

procedure SpriteStartAnimation(const Sprite: TCastleSceneCore; const AnimName: String; const IsRepeat: Boolean);
var
  TimeSensor: TTimeSensorNode;
  Track: Integer;
begin
  if Sprite is TCastleScene then
  begin
    try
      TimeSensor := Sprite.Node(AnimName) as TTimeSensorNode;
      TimeSensor.Start(IsRepeat, True, 0);
      Sprite.ForceInitialAnimationPose;
    except
      on E: Exception do
        Satania.TalkWithoutBlock(E.Message);
    end;
  end else
  if Sprite is TCastleSpine then
  begin                                          
    if TrackDict.ContainsKey(AnimName) then
      Track := TrackDict[AnimName]
    else
    begin
      Track := TrackDict.Count;
      TrackDict.Add(AnimName, Track);
    end;
    TCastleSpine(Sprite).PlayAnimation(AnimName, IsRepeat, True, Track);
  end;
end;

procedure SpriteStopAnimation(const Sprite: TCastleSceneCore; const AnimName: String);
var
  TimeSensor: TTimeSensorNode;
begin
  if Sprite is TCastleScene then
  begin
    try
      TimeSensor := Sprite.Node(AnimName) as TTimeSensorNode;
      TimeSensor.Start(False, True, 0);
      TimeSensor.Stop;
    except
      on E: Exception do
        Satania.TalkWithoutBlock(E.Message);
    end;
  end else
  if Sprite is TCastleSpine then
  begin
    if TrackDict.ContainsKey(AnimName) then
      TCastleSpine(Sprite).StopAnimation(TrackDict[AnimName]);
  end;
end;

procedure SpriteStopAllAnimations(const Sprite: TCastleSceneCore);
  procedure StopButNotResetAnimation(AnimName: String);
  var
    TimeSensor: TTimeSensorNode;
  begin
    try
      TimeSensor := Sprite.Node(AnimName) as TTimeSensorNode;
      TimeSensor.Stop;
    except
      on E: Exception do
        Satania.TalkWithoutBlock(E.Message);
    end;
  end;
var
  S: String;
begin     
  if Sprite is TCastleScene then
  begin
    for S in Sprite.AnimationsList do
      StopButNotResetAnimation(S);
    Sprite.ResetAnimationState;
  end else
  if Sprite is TCastleSpine then
  begin
    TCastleSpine(Sprite).StopAnimation;
  end;
end;

initialization
  TrackDict := TTrackDict.Create;

finalization
  TrackDict.Free;

end.

