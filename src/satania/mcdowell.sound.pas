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

unit Mcdowell.Sound;

{$I configs.inc}

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
