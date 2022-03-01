unit Mcdowell.Sound;

interface

uses
  Sysutils, Classes,
  CastleScene, CastleTransform, CastleSoundEngine, CastleSoundBase, CastleBehaviors;

type
  TSataniaSoundBehavior = class(TCastleBehavior)
  public
    IsPlaying,
    IsLooped: Boolean;
    URL: String;
    SoundBuffer: TSoundBuffer;
    SoundSource: TInternalSoundSource;
    constructor Create(AOwner: TComponent);
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

implementation

uses
  Globals;

constructor TSataniaSoundBehavior.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TSataniaSoundBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  if not IsPlaying then
  begin
    IsPlaying := True;
    SoundBuffer := SoundEngine.LoadBuffer(PATH_SOUND + URL);
    SoundSource := SoundEngine.PlaySound(SoundBuffer);
  end;
  if not SoundSource.PlayingOrPaused then
  begin
    RemoveMe := rtRemoveAndFree;
    SoundEngine.FreeBuffer(SoundBuffer);
  end else
    RemoveMe := rtNone;
end;

end.
