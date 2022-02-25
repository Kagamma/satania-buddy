unit mcdowell.chatbot;

{$mode ObjFPC}{$H+}

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

procedure Reload;
function Inference(S: String): String;

implementation

type
  TNeuralData = array of array of TNeuralFloat;

var
  TagList: TStringList;
  WordList: TStringList;
  RuleDict: TStringArrayDict;
  OutputSize: Integer;
  NN: TNNet;
  Output: TNNetVolume;

procedure Prepare;
begin
  TagList := TStringList.Create;
  WordList := TStringList.Create;
  RuleDict := TStringArrayDict.Create;
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
  NN.Free;
  Output.Free;
  TagList.Free;
  WordList.Free;
  RuleDict.Free;
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
  Responses: TStringDynArray;
  I, J: Integer;
begin
  S := TStringList.Create;
  S.LoadFromFile('data/nn/chatbot/rules.json');
  JSONArray := GetJSON(S.Text) as TJSONArray;
  for I := 0 to JSONArray.Count - 1 do
  begin
    JSONItem := JSONArray[I] as TJSONObject;
    Tag := JSONItem['tag'].AsString;
    JSONArraySub := JSONItem['responses'] as TJSONArray;
    SetLength(Responses, JSONArraySub.Count);
    for J := 0 to JSONArraySub.Count - 1 do
    begin
      Responses[J] := JSONArraySub[J].AsString;
    end;
    RuleDict.Add(Tag, Responses);
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
    Responses := RuleDict[Tag];
    Result := Responses[Random(Length(Responses))];
  end else
    Result := '';
end;

procedure Reload;
begin
  Cleanup;
  Prepare;
  ReadRules;
end;

initialization
  Prepare;
  ReadRules;

finalization
  Cleanup;

end.

