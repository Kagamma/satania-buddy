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
  Classes, SysUtils, uPocketSphinx, uPocketSphinxDefaultAudioSource,
  ad, cmd_ln, ps_search, pocketsphinx;

type
  TSataniaSpeechToText = class
  protected
    FPocketSphinx: TPocketSphinx; 
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
    rsInitialized: Satania.Talk('I''m listening.');
    rsReady: ;
    rsListening: ;
    rsAnalyze: ; // Satania.Talk('...');
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
  {$ifdef WINDOWS}
  SpDisable;
  {$endif}
end;

function TSataniaSpeechToText.Enable: Boolean;
begin
  if not IsLoaded then exit(False);
  Disable;
  {$ifdef WINDOWS}
  if Save.Settings.STTBackend = 0 then
  begin  
  {$endif}
    FPocketSphinx := TPocketSphinx.Create;

    FPocketSphinx.OnStateChange := @OnPocketSphinxStateChange;
    FPocketSphinx.OnHypothesis := @OnPocketSphinxHypothesis;

    FPocketSphinx.AcousticModelPath := PATH_SPHINX + Save.Settings.STTModel;
    FPocketSphinx.Threshold := 0;

    FPocketSphinx.Init;
    if FPocketSphinx.State = rsInitialized then
    begin
      if FPocketSphinx.LoadDictionary(PATH_SPHINX + Save.Settings.STTDict) then
      begin
        FPocketSphinx.AddNgramSearch('ngram', PATH_SPHINX + Save.Settings.STTNgram);
        FPocketSphinx.ActiveSearch := 'ngram';
      end;

      FPocketSphinx.AudioSource := TAudioSourceDefaultDevice.Create;

      FPocketSphinx.Active := True;
    end;
  {$ifdef WINDOWS}
  end else
  if Save.Settings.STTBackend = 1 then
  begin
    SpEnable;
  end;  
  {$endif}
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
