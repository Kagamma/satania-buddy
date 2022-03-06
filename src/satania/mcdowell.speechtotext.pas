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
  Classes, SysUtils, uPocketSphinx, uPocketSphinxBassAudioSource,
  ad, cmd_ln, ps_search, pocketsphinx, vosk, voskthread;

type
  TSataniaSpeechToText = class
  protected
    FPocketSphinx: TPocketSphinx;
    FVoskThread: TVoskThread;
    {$define unit_protected}
    {$I mcdowell.speechtotext_windows.inc}
    {$undef unit_protected}
    procedure OnPocketSphinxStateChange(Sender: TObject; AState: TPocketSphinxState);
    procedure OnPocketSphinxHypothesis(Sender: TObject; AScore: Integer; AHypothesis: String);
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
  FPocketSphinx := nil;
  FVoskThread := nil;
end;

destructor TSataniaSpeechToText.Destroy;
begin
  Disable;
  inherited;
end;

procedure TSataniaSpeechToText.OnPocketSphinxStateChange(Sender: TObject;
  AState: TPocketSphinxState);
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
    rsError: Satania.Talk('Sphinx: Error - ' + (Sender as TPocketSphinx).LastErrorMsg);
  end;
end;

procedure TSataniaSpeechToText.OnPocketSphinxHypothesis(Sender: TObject;
  AScore: Integer; AHypothesis: String);
begin
  Satania.Log('(you)', AHypothesis);
  Satania.Chat(AHypothesis);
end;

procedure TSataniaSpeechToText.Disable;
begin
  if FPocketSphinx <> nil then
    FreeAndNil(FPocketSphinx);       
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
  {if Save.Settings.STTBackend = SPEECH_RECOGNIZER_BACKEND_POCKETSPHINX then
  begin
    FPocketSphinx := TPocketSphinx.Create;

    FPocketSphinx.OnStateChange := @OnPocketSphinxStateChange;
    FPocketSphinx.OnHypothesis := @OnPocketSphinxHypothesis;

    FPocketSphinx.AcousticModelPath := PATH_SPHINX + Save.Settings.STTSphinxModel;
    FPocketSphinx.Threshold := 0;

    FPocketSphinx.Init;
    if FPocketSphinx.State = rsInitialized then
    begin
      if FPocketSphinx.LoadDictionary(PATH_SPHINX + Save.Settings.STTSphinxDict) then
      begin
        FPocketSphinx.AddNgramSearch('ngram', PATH_SPHINX + Save.Settings.STTSphinxNgram);
        FPocketSphinx.ActiveSearch := 'ngram';
      end;
      FPocketSphinx.AudioSource := TBassAudioSource.Create(FPocketSphinx, GetMicrophoneDeviceIdx);

      FPocketSphinx.Active := True;
    end;
  end else}
  {$ifdef WINDOWS}
  if Save.Settings.STTBackend = SPEECH_RECOGNIZER_BACKEND_SAPI then
  begin
    SpEnable;
  end else
  {$endif}
  if Save.Settings.STTBackend = SPEECH_RECOGNIZER_BACKEND_VOSK then
  begin
    FVoskThread := TVoskThread.Create;

    FVoskThread.OnStateChange := @OnPocketSphinxStateChange;
    FVoskThread.OnHypothesis := @OnPocketSphinxHypothesis;

    FVoskThread.ModelPath := PATH_VOSK + Save.Settings.STTVoskModel;

    FVoskThread.Init;
    if FVoskThread.State = rsInitialized then
    begin
      FVoskThread.AudioSource := TBassAudioSource.Create(FPocketSphinx, GetMicrophoneDeviceIdx);
      FVoskThread.Active := True;
    end;
  end;
  exit(True);
end;

function TSataniaSpeechToText.IsLoaded: Boolean;
begin
  Result := True;
  {if Save.Settings.STTBackend = SPEECH_RECOGNIZER_BACKEND_POCKETSPHINX then
    Result := (ad.Lib <> 0) and (cmd_ln.Lib <> 0) and (ps_search.Lib <> 0) and (pocketsphinx.Lib <> 0)
  else}
  if Save.Settings.STTBackend = SPEECH_RECOGNIZER_BACKEND_VOSK then
    Result := vosk.Lib <> 0;
end;

initialization
  SataniaSpeechToText := TSataniaSpeechToText.Create;

finalization
  if SataniaSpeechToText <> nil then
    FreeAndNil(SataniaSpeechToText);

end.
