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

unit Mcdowell.SpeechToText;

{$I configs.inc}

interface

uses
  {$define unit_declare_interface}
  {$I mcdowell.speechtotext_windows.inc} 
  {$undef unit_declare_interface}
  Classes, SysUtils,
  ad, cmd_ln, ps_search, vosk, voskthread, voskbassaudiosource;

type
  TSataniaSpeechToText = class
  protected
    FVoskThread: TVoskThread;
    {$define unit_protected}
    {$I mcdowell.speechtotext_windows.inc}
    {$undef unit_protected}
    procedure OnVoskStateChange(Sender: TObject; AState: TVoskState);
    procedure OnVoskHypothesis(Sender: TObject; AScore: Integer; AHypothesis: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Disable;
    function Enable: Boolean;
    function IsLoaded: Boolean;
  end;

var
  SataniaSpeechToText: TSataniaSpeechToText;

implementation

uses
  Mcdowell,
  Globals;

{$define unit_implmentation}
{$I mcdowell.speechtotext_windows.inc}
{$undef unit_implmentation}

constructor TSataniaSpeechToText.Create;
begin
  inherited;
  FVoskThread := nil;
end;

destructor TSataniaSpeechToText.Destroy;
begin
  Disable;
  inherited;
end;

procedure TSataniaSpeechToText.OnVoskStateChange(Sender: TObject;
  AState: TVoskState);
begin
  case AState of
    rsNotInitialized: ;
    rsInitialized: Satania.Talk('I''m listening.');
    rsReady: ;
    rsListening: ;
    rsAnalyze:
      begin
        if not Satania.IsTalking then
          Satania.Talk('...');
      end;
  end;
end;

procedure TSataniaSpeechToText.OnVoskHypothesis(Sender: TObject;
  AScore: Integer; AHypothesis: String);
begin
  Satania.Log('(you)', AHypothesis);
  Satania.Chat(AHypothesis);
end;

procedure TSataniaSpeechToText.Disable;
begin
  if FVoskThread <> nil then
    FreeAndNil(FVoskThread);
  {$ifdef WINDOWS}
  SpDisable;
  {$endif}
end;

function TSataniaSpeechToText.Enable: Boolean;
begin
  if not IsLoaded then exit(False);
  Disable;
  {$ifdef WINDOWS}
  if Save.Settings.STTBackend = SPEECH_RECOGNIZER_BACKEND_SAPI then
  begin
    SpEnable;
  end else
  {$endif}
  if Save.Settings.STTBackend = SPEECH_RECOGNIZER_BACKEND_VOSK then
  begin
    FVoskThread := TVoskThread.Create;

    FVoskThread.OnStateChange := @OnVoskStateChange;
    FVoskThread.OnHypothesis := @OnVoskHypothesis;

    FVoskThread.ModelPath := PATH_VOSK + Save.Settings.STTVoskModel;

    FVoskThread.Init;
    if FVoskThread.State = rsInitialized then
    begin
      FVoskThread.AudioSource := TBassAudioSource.Create(GetMicrophoneDeviceIdx);
      FVoskThread.Active := True;
    end;
  end;
  exit(True);
end;

function TSataniaSpeechToText.IsLoaded: Boolean;
begin
  Result := True;
  if Save.Settings.STTBackend = SPEECH_RECOGNIZER_BACKEND_VOSK then
    Result := vosk.Lib <> 0;
end;

initialization
  SataniaSpeechToText := TSataniaSpeechToText.Create;

finalization
  if SataniaSpeechToText <> nil then
    FreeAndNil(SataniaSpeechToText);

end.
