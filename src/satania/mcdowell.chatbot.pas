{

satania-buddy
Copyright (C) 2022-2023 kagamma

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

unit mcdowell.chatbot;

{$I configs.inc}

interface

uses
  Classes, SysUtils,
  StrUtils, types,
  neuralnetwork,
  neuralvolume,
  neuralfit,
  generics.collections,
  fpjson, jsonparser,
  globals;

procedure ReadRules;
procedure Reload;
function Inference(S: String): String;

var
  RuleDict: TRuleDict;

implementation

uses
  Mcdowell;

type
  TNeuralData = array of array of TNeuralFloat;

var
  TagList: TStringList;
  WordList: TStringList;
  OutputSize: Integer;
  NN: TNNet;
  Output: TNNetVolume;

procedure Prepare;
begin
  TagList := TStringList.Create;
  WordList := TStringList.Create;
  TagList.Sorted := True;
  WordList.Sorted := True;
  WordList.LoadFromFile('data/nn/chatbot/model.word');
  TagList.LoadFromFile('data/nn/chatbot/model.tag');

  OutputSize := TagList.Count;
  NN := TNNet.Create;
  NN.LoadFromFile('data/nn/chatbot/model.nn');
  Output := TNNetVolume.Create(OutputSize);
end;

procedure Cleanup;
begin
  if NN <> nil then
    FreeAndNil(NN);
  if Output <> nil then
    FreeAndNil(Output);
  if TagList <> nil then
    FreeAndNil(TagList);
  if WordList <> nil then
    FreeAndNil(WordList);
end;

function CreateWordMap(Sentence: String): TNeuralFloatDynArr;
var
  I, J: Integer;
  Words: TStringDynArray;
begin
  SetLength(Result, WordList.Count);
  Sentence := StringsReplace(LowerCase(Sentence), ['.', '?', '!', '  '], ['', '', '', ' '], [rfReplaceAll]);
  Words := SplitString(Sentence, ' ');
  for J := 0 to WordList.Count - 1 do
  begin
    Result[J] := 0;
    for I := 0 to Length(Words) - 1 do
    begin
      if Words[I] = WordList[J] then
      begin
        Result[J] := 1;
        break;
      end;
    end;
  end;
end;

procedure ReadRules;
var
  JSONArray, JSONArraySub: TJSONArray;
  JSONItem: TJSONObject;
  S: TStringList;
  Tag: String;
  Rule: TRuleRec;
  I, J: Integer;
begin
  RuleDict.Clear;
  S := TStringList.Create;
  S.LoadFromFile('data/nn/chatbot/rules.json');
  JSONArray := GetJSON(S.Text) as TJSONArray;
  for I := 0 to JSONArray.Count - 1 do
  begin
    SetLength(Rule.Patterns, 0);
    SetLength(Rule.Responses, 0);
    JSONItem := JSONArray[I] as TJSONObject;
    Tag := JSONItem['tag'].AsString;
    JSONArraySub := JSONItem['patterns'] as TJSONArray;
    SetLength(Rule.Patterns, JSONArraySub.Count);
    for J := 0 to JSONArraySub.Count - 1 do
    begin
      Rule.Patterns[J] := JSONArraySub[J].AsString;
    end;
    JSONArraySub := JSONItem['responses'] as TJSONArray;
    SetLength(Rule.Responses, JSONArraySub.Count);
    for J := 0 to JSONArraySub.Count - 1 do
    begin
      Rule.Responses[J] := JSONArraySub[J].AsString;
    end;
    RuleDict.Add(Tag, Rule);
  end;
  S.Free;
end;

function Inference(S: String): String;

  function CalcScore(OutPut: TNNetVolume): Integer;
  var
    F: Single = 0;
    I: Integer;
  begin
    Result := -1;
    for I := 0 to OutputSize - 1 do
    begin
      if F < Output.Raw[I] then
      begin
        F := Output.Raw[I];
        if F > 0.75 then
          Result := I;
      end;
    end;
  end;

var
  Tag: String;
  Map: TNeuralFloatDynArr;
  I, Score: Integer;
  Responses: TStringDynArray;

begin
  Map := CreateWordMap(S);
  NN.Compute(Map);
  NN.GetOutput(Output);
  Score := CalcScore(Output);
  if Score >= 0 then
  begin
    Tag := TagList[Score];
    Responses := RuleDict[Tag].Responses;
    Result := Responses[Random(Length(Responses))];
  end else
    Result := '';
end;

procedure Reload;
begin
  try
    Cleanup;
    Prepare;
    ReadRules;
  except
    on E: Exception do
      Satania.Talk(E.Message);
  end;
end;

initialization
  RuleDict := TRuleDict.Create;

finalization
  RuleDict.Free;
  Cleanup;

end.

