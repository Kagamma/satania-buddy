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
  Classes, SysUtils, uPocketSphinx, uPocketSphinxDefaultAudioSource,
  ad, cmd_ln, ps_search, pocketsphinx;

type
  TSataniaSpeechToText = class
  protected
    FPocketSphinx: TPocketSphinx;
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
  Mcdowell;

constructor TSataniaSpeechToText.Create;
begin
  inherited;
  FPocketSphinx := nil;
end;

destructor TSataniaSpeechToText.Destroy;
begin
  FreeAndNil(FPocketSphinx);
  inherited;
end;

procedure TSataniaSpeechToText.OnPocketSphinxStateChange(Sender: TObject;
  AState: TPocketSphinxState);
begin
  case AState of
    rsNotInitialized: ;
    rsInitialized: Satania.Talk('I''m listening');
    rsReady: ;
    rsListening: ;
    rsAnalyze: Satania.Talk('...');
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
end;

function TSataniaSpeechToText.Enable: Boolean;
begin
  if not IsLoaded then exit(False);
  Disable;
  FPocketSphinx := TPocketSphinx.Create;

  FPocketSphinx.OnStateChange := @OnPocketSphinxStateChange;
  FPocketSphinx.OnHypothesis := @OnPocketSphinxHypothesis;

  FPocketSphinx.AcousticModelPath := 'data/nn/sphinx/english/en-us';
  FPocketSphinx.Threshold := 0;

  FPocketSphinx.Init;
  if FPocketSphinx.State = rsInitialized then
  begin
    if FPocketSphinx.LoadDictionary('data/nn/sphinx/english/cmudict-en-us.dict') then
    begin
      FPocketSphinx.AddNgramSearch('ngram', 'data/nn/sphinx/english/en-us.lm.bin');
      FPocketSphinx.ActiveSearch := 'ngram';
    end;

    FPocketSphinx.AudioSource := TAudioSourceDefaultDevice.Create;

    FPocketSphinx.Active := True;
  end;
  exit(True);
end;

function TSataniaSpeechToText.IsLoaded: Boolean;
begin
  Result := (ad.Lib <> 0) and (cmd_ln.Lib <> 0) and (ps_search.Lib <> 0) and (pocketsphinx.Lib <> 0);
end;

initialization
  SataniaSpeechToText := TSataniaSpeechToText.Create;

finalization
  if SataniaSpeechToText <> nil then
    FreeAndNil(SataniaSpeechToText);

end.
