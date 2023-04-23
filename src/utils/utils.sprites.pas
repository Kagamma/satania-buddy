unit utils.sprites;

{$I configs.inc}

interface

uses
  Classes, SysUtils, CastleScene, CastleTransform, CastleRenderOptions, X3DNodes,
  CastleSpine, CastleSpineMixer, globals;

procedure SpriteSetFilter(const Sprite: TCastleTransform; const S: String);
procedure SpriteSetAnimationSpeed(const Sprite: TCastleTransform; const AnimName: String; const Speed: Single);
procedure SpriteStartAnimation(const Sprite: TCastleTransform; const Mixer: TCastleSpineMixerBehavior; const AnimName: String; const IsRepeat: Boolean);
procedure SpriteStopAnimation(const Sprite: TCastleTransform; const Mixer: TCastleSpineMixerBehavior; const AnimName: String);
procedure SpriteStopAllAnimations(const Sprite: TCastleTransform; const Mixer: TCastleSpineMixerBehavior);

var
  TrackDict: TTrackDict;

implementation

uses
  Mcdowell;

procedure SpriteSetFilter(const Sprite: TCastleTransform; const S: String);
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
      'Linear':
        begin
          TCastleSpine(Sprite).SmoothTexture := False;
          TCastleSpine(Sprite).Mipmap := False;
        end;
      'Nicest':
        begin
          TCastleSpine(Sprite).SmoothTexture := True;
          TCastleSpine(Sprite).Mipmap := True;
        end;
      else
        begin
          TCastleSpine(Sprite).SmoothTexture := True;
        end;
    end;
  end;
end;

procedure SpriteSetAnimationSpeed(const Sprite: TCastleTransform; const AnimName: String; const Speed: Single);
var
  TimeSensor: TTimeSensorNode;
begin     
  if Sprite is TCastleScene then
  begin
    try
      TimeSensor := TCastleScene(Sprite).Node(AnimName) as TTimeSensorNode;
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

procedure SpriteStartAnimation(const Sprite: TCastleTransform; const Mixer: TCastleSpineMixerBehavior; const AnimName: String; const IsRepeat: Boolean);
var
  TimeSensor: TTimeSensorNode;
  Track: TTrackRec;
begin
  if Sprite is TCastleScene then
  begin
    try
      TimeSensor := TCastleScene(Sprite).Node(AnimName) as TTimeSensorNode;
      TimeSensor.Start(IsRepeat, True, 0);
      TCastleScene(Sprite).ForceInitialAnimationPose;
    except
      on E: Exception do
        Satania.TalkWithoutBlock(E.Message);
    end;
  end else
  if Sprite is TCastleSpine then
  begin
    if (Mixer.Data <> nil) and (Mixer.Data.FindAnimation(AnimName) <> nil) then
    begin
      Mixer.PlayAnimation(AnimName, IsRepeat);
    end else
    begin
      if TrackDict.ContainsKey(AnimName) then
      begin
        Track := TrackDict[AnimName];
        if not Track.IsLooped then
        begin
          TCastleSpine(Sprite).PlayAnimation(AnimName, IsRepeat, True, Track.Indx);
          Track.IsLooped := IsRepeat;
          TrackDict[AnimName] := Track;
        end;
      end else
      begin
        Track.Indx := TrackDict.Count;
        Track.IsLooped := IsRepeat;
        TrackDict.Add(AnimName, Track);
        TCastleSpine(Sprite).PlayAnimation(AnimName, IsRepeat, True, Track.Indx);
      end;
    end;
  end;
end;

procedure SpriteStopAnimation(const Sprite: TCastleTransform; const Mixer: TCastleSpineMixerBehavior; const AnimName: String);
var
  TimeSensor: TTimeSensorNode;
  Track: TTrackRec;
begin
  if Sprite is TCastleScene then
  begin
    try
      TimeSensor := TCastleScene(Sprite).Node(AnimName) as TTimeSensorNode;
      TimeSensor.Start(False, True, 0);
      TimeSensor.Stop;
    except
      on E: Exception do
        Satania.TalkWithoutBlock(E.Message);
    end;
  end else
  if Sprite is TCastleSpine then
  begin
    if (Mixer.Data <> nil) and (Mixer.Data.FindAnimation(AnimName) <> nil) then
    begin
      Mixer.StopAnimation;
    end else
    if TrackDict.ContainsKey(AnimName) then
    begin
      Track := TrackDict[AnimName];
      TCastleSpine(Sprite).StopAnimation(Track.Indx);
      Track.IsLooped := False;
      TrackDict[AnimName] := Track;
    end;
  end;
end;

procedure SpriteStopAllAnimations(const Sprite: TCastleTransform; const Mixer: TCastleSpineMixerBehavior);
  procedure StopButNotResetAnimation(AnimName: String);
  var
    TimeSensor: TTimeSensorNode;
  begin
    try
      TimeSensor := TCastleScene(Sprite).Node(AnimName) as TTimeSensorNode;
      TimeSensor.Stop;
    except
      on E: Exception do
        Satania.TalkWithoutBlock(E.Message);
    end;
  end;
var
  S: String;
  Track: TTrackRec;
begin     
  if Sprite is TCastleScene then
  begin
    for S in TCastleScene(Sprite).AnimationsList do
      StopButNotResetAnimation(S);
    TCastleScene(Sprite).ResetAnimationState;
  end else
  if Sprite is TCastleSpine then
  begin
    TCastleSpine(Sprite).StopAnimation;
    Mixer.StopAnimation;
    for S in TrackDict.Keys do
    begin
      Track := TrackDict[S];
      Track.IsLooped := False;
      TrackDict[S] := Track;
    end;
  end;
end;

initialization
  TrackDict := TTrackDict.Create;

finalization
  TrackDict.Free;

end.

