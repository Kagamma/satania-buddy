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

{$ifdef WINDOWS}

{$ifdef unit_declare_interface}
  Windows, ComObj, speechlib_5_4_tlb,
{$endif unit_declare_interface}

{$ifdef unit_protected}
    FSpGrammar: Variant;
    FSpSharedRecoContext: TAxcSpSharedRecoContext;
    procedure SpSharedRecoContextHypothesis(Sender: TObject; StreamNumber: Integer; StreamPosition: OleVariant; R: ISpeechRecoResult);
    procedure SpSharedRecoContextRecognition(Sender: TObject; StreamNumber: Integer; StreamPosition: OleVariant; RecognitionType: SpeechRecognitionType; R: ISpeechRecoResult);
    procedure SpEnable;
    procedure SpDisable;
{$endif unit_protected}

{$ifdef unit_implmentation}

procedure TSataniaSpeechToText.SpEnable;
begin
  FSpSharedRecoContext := TAxcSpSharedRecoContext.Create(nil);
  FSpSharedRecoContext.OleServer.EventInterests := SREAllEvents;
  FSpSharedRecoContext.OnHypothesis := @SpSharedRecoContextHypothesis;
  FSpSharedRecoContext.OnRecognition := @SpSharedRecoContextRecognition;
  FSpGrammar := FSpSharedRecoContext.OleServer.CreateGrammar(0);
  FSpGrammar.DictationSetState(SGDSActive);
end;

procedure TSataniaSpeechToText.SpDisable;
begin
  if FSpSharedRecoContext <> nil then
    FreeAndNil(FSpSharedRecoContext);
end;

procedure TSataniaSpeechToText.SpSharedRecoContextHypothesis(Sender: TObject; StreamNumber: Integer; StreamPosition: OleVariant; R: ISpeechRecoResult);
begin
  if not Satania.IsTalking then
    Satania.Talk('...');
end;

procedure TSataniaSpeechToText.SpSharedRecoContextRecognition(Sender: TObject; StreamNumber: Integer; StreamPosition: OleVariant; RecognitionType: SpeechRecognitionType; R: ISpeechRecoResult);
var
  S: String;
begin
  S := R.PhraseInfo.GetText(0, -1, True);
  Satania.Log('(you)', S);
  Satania.Chat(S);
end;

{$endif unit_implmentation}

{$endif WINDOWS}

