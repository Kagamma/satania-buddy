{

satania-buddy
Copyright (C) 2022-2024 kagamma

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
  vosk, whisper, SttThread, BassAudioSource;

type
  TSataniaSpeechToText = class
  protected
    FVoskThread: TVoskThread;
    FWhisperThread: TWhisperThread;
    {$define unit_protected}
    {$I mcdowell.speechtotext_windows.inc}
    {$undef unit_protected}
    procedure OnSttStateChange(Sender: TObject; AState: TSttState);
    procedure OnSttHypothesis(Sender: TObject; AScore: Integer; AHypothesis: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Disable;
    function Enable: Boolean;
    function IsVoskLoaded: Boolean;    
    function IsWhisperLoaded: Boolean;
  end;

var
  SataniaSpeechToText: TSataniaSpeechToText;

implementation

uses
  Mcdowell,
  Globals,
  Form.bubble;

{$define unit_implmentation}
{$I mcdowell.speechtotext_windows.inc}
{$undef unit_implmentation}

constructor TSataniaSpeechToText.Create;
begin
  inherited;
end;

destructor TSataniaSpeechToText.Destroy;
begin
  Disable;
  inherited;
end;

procedure TSataniaSpeechToText.OnSttStateChange(Sender: TObject;
  AState: TSttState);
begin
  case AState of
    sttNotInitialized: ;
    sttInitialized: Satania.Log('I''m listening.');
    sttReady: ;
    sttListening: ;
    sttAnalyze:
      begin
        //if not Satania.IsTalking then
        //  Satania.Talk('...');
      end;
  end;
end;

procedure TSataniaSpeechToText.OnSttHypothesis(Sender: TObject;
  AScore: Integer; AHypothesis: String);
begin
  if (AHypothesis <> 'huh') and FormBubble.FinishedTyping then
  begin
    Satania.Log(Save.Settings.UserName, AHypothesis);
    Satania.Chat(AHypothesis);
  end;
end;

procedure TSataniaSpeechToText.Disable;
begin
  if FVoskThread <> nil then
  begin
    FVoskThread.Terminate;
    FVoskThread := nil;
  end;      
  if FWhisperThread <> nil then
  begin
    FWhisperThread.Terminate;
    FWhisperThread := nil;
  end;
  {$ifdef WINDOWS}
  SpDisable;
  {$endif}
end;

function TSataniaSpeechToText.Enable: Boolean;
begin
  Disable;
  {$ifdef WINDOWS}
  if Save.Settings.STTBackend = SPEECH_RECOGNIZER_BACKEND_SAPI then
  begin
    SpEnable;
  end else
  {$endif}
  if (Save.Settings.STTBackend = SPEECH_RECOGNIZER_BACKEND_VOSK) and Self.IsVoskLoaded then
  begin
    FVoskThread := TVoskThread.Create;

    FVoskThread.OnStateChange := @OnSttStateChange;
    FVoskThread.OnHypothesis := @OnSttHypothesis;

    FVoskThread.ModelPath := PATH_VOSK + Save.Settings.STTVoskModel;

    FVoskThread.Init;
    if FVoskThread.State = sttInitialized then
    begin
      FVoskThread.AudioSource := TBassAudioSource.Create(GetMicrophoneDeviceIdx);
      FVoskThread.Active := True;
    end;
  end else
  if (Save.Settings.STTBackend = SPEECH_RECOGNIZER_BACKEND_WHISPER) and Self.IsWhisperLoaded then
  begin
    FWhisperThread := TWhisperThread.Create;

    FWhisperThread.OnStateChange := @OnSttStateChange;
    FWhisperThread.OnHypothesis := @OnSttHypothesis;

    FWhisperThread.ModelPath := PATH_WHISPER + Save.Settings.STTWhisperModel;

    FWhisperThread.Init;
    if FWhisperThread.State = sttInitialized then
    begin
      FWhisperThread.AudioSource := TBassAudioSource.Create(GetMicrophoneDeviceIdx);
      FWhisperThread.Active := True;
    end;
  end;
  exit(True);
end;

function TSataniaSpeechToText.IsVoskLoaded: Boolean;
begin
  Result := vosk.Lib <> 0;
end;

function TSataniaSpeechToText.IsWhisperLoaded: Boolean;
begin
  Result := whisper.Lib <> 0;
end;

initialization
  SataniaSpeechToText := TSataniaSpeechToText.Create;

finalization
  if SataniaSpeechToText <> nil then
    FreeAndNil(SataniaSpeechToText);

end.
