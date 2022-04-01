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

unit mcdowell.chatbot.train;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  StrUtils, types,
  neuralnetwork,
  neuralvolume,
  neuralfit,
  generics.collections,
  fpjson, jsonparser;

procedure RunTrain;

implementation

uses
  Mcdowell, Mcdowell.chatbot, Globals;

type
  TPair = record
    Sentence, Tag: String;
  end;

  TPairList = specialize TList<TPair>;
  TRuleData = record
    Tag: String;
    Patterns: array of String;
  end;
  PRuleData = ^TRuleData;

  TNeuralData = array of array of TNeuralFloat;

  TTrainThread = class(TThread)
    procedure Execute; override;
  end;

  TMessage = class
    class procedure NoMessageProc(const S: string);  
    class procedure EpochMessageProc(const S: string);
  end;

var
  TagList: TStringList;
  WordList: TStringList;
  PairList: TPairList;
  RuleArray: array of TRuleData;
  InputSize, OutputSize, HiddenSize: Integer;
  XData, YData: TNeuralData;  
  NFit: TNeuralFit;

class procedure TMessage.NoMessageProc(const S: string);
begin
end;

class procedure TMessage.EpochMessageProc(const S: string);
begin
  if (NFit.CurrentEpoch <> 0) and (NFit.CurrentEpoch mod 100 = 0) then
    Satania.Talk(IntToStr(NFit.CurrentEpoch) + ' epochs');
end;

procedure Prepare;
begin
  TagList := TStringList.Create;
  WordList := TStringList.Create;
  PairList := TPairList.Create;
  TagList.Sorted := True;
  WordList.Sorted := True;
end;

procedure Cleanup;
begin
  TagList.Free;
  WordList.Free;
  PairList.Free;
  SetLength(RuleArray, 0);
  SetLength(XData, 0);
  SetLength(YData, 0);
end;

procedure CreatePairsAndWords(Sentence, Tag: String);
var
  Words: TStringDynArray;
  Pair: TPair;
  V: Integer;
  S: String;
begin
  Sentence := StringsReplace(LowerCase(Sentence), ['.', '?', '!', '  '], ['', '', '', ' '], [rfReplaceAll]);
  Words := SplitString(Sentence, ' ');
  Pair.Sentence := Sentence;
  Pair.Tag := Tag;
  PairList.Add(Pair);
  for S in Words do
  begin
    if not WordList.Find(S, V) then
    begin
      WordList.Add(S);
    end;
  end;
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
  P: PRuleData;
  S: TStringList;
  I, J: Integer;
begin
  S := TStringList.Create;
  S.LoadFromFile('data/nn/chatbot/rules.json');
  JSONArray := GetJSON(S.Text) as TJSONArray;
  SetLength(RuleArray, JSONArray.Count);
  for I := 0 to JSONArray.Count - 1 do
  begin
    P := @RuleArray[I];
    JSONItem := JSONArray[I] as TJSONObject;
    P^.Tag := JSONItem['tag'].AsString;
    TagList.Add(P^.Tag);
    JSONArraySub := JSONItem['patterns'] as TJSONArray;
    SetLength(P^.Patterns, JSONArraySub.Count);
    for J := 0 to JSONArraySub.Count - 1 do
    begin
      P^.Patterns[J] := JSONArraySub[J].AsString;
      CreatePairsAndWords(P^.Patterns[J], P^.Tag);
    end;
  end;
  S.Free;
end;

procedure PrepareTrainingData;
var
  X, Y: Integer;
  Map: TNeuralFloatDynArr;
  Pair: TPair;
begin
  SetLength(XData, PairList.Count, WordList.Count);
  SetLength(YData, PairList.Count, TagList.Count);
  for Y := 0 to PairList.Count - 1 do
  begin
    Pair := PairList[Y];
    Map := CreateWordMap(Pair.Sentence);
    for X := 0 to WordList.Count - 1 do
      XData[Y, X] := Map[X];
    for X := 0 to TagList.Count - 1 do
    begin
      YData[Y, X] := 0;
    end;
    YData[Y, TagList.IndexOf(Pair.Tag)] := 1;
  end;
  InputSize := WordList.Count;
  OutputSize := TagList.Count;
  HiddenSize := (InputSize + OutputSize) * 3;
end;

procedure Train;
  function DebugErrors(NN: TNNet): String;
  var
    LayerCnt: integer;
  begin
    Result := '';
    with NN do
    if Layers.Count > 1 then
    begin
      for LayerCnt := 0 to GetLastLayerIdx() do
      begin
        Result := Result +
            'Layer ' + IntToStr(LayerCnt) +
            ' Max Error: ' + PointFloatToStr(Layers[LayerCnt].OutputError.GetMax()) +
            ' Min Error: ' + PointFloatToStr(Layers[LayerCnt].OutputError.GetMin()) +
            ' Max ErrorD: ' + PointFloatToStr(Layers[LayerCnt].OutputErrorDeriv.GetMax()) +
            ' Min ErrorD: ' + PointFloatToStr(Layers[LayerCnt].OutputErrorDeriv.GetMin()) +
            ' ' + Layers[LayerCnt].ClassName + ' ' +
            IntToStr(Layers[LayerCnt].Output.SizeX) + ',' +
            IntToStr(Layers[LayerCnt].Output.SizeY) + ',' +
            IntToStr(Layers[LayerCnt].Output.Depth) + #10;
      end;
    end;
  end;

var
  NN: TNNet;
  Y: Integer;
  TrainingPairs: TNNetVolumePairList;
  Ticks: QWord;
begin
  NN := TNNet.Create();
  NFit := TNeuralFit.Create();
  TrainingPairs := TNNetVolumePairList.Create();

  NN.AddLayer(TNNetInput.Create(InputSize));
  NN.AddLayer(TNNetFullConnectReLU.Create(HiddenSize));
  NN.AddLayer(TNNetFullConnectReLU.Create(HiddenSize));
  NN.AddLayer(TNNetFullConnectLinear.Create(OutputSize));

  Satania.ActionFromFile('system/loading-start.evil', False);
  Satania.Talk('I am learning. please wait...');
  Ticks := GetTickCount64;
  for Y := Low(XData) to High(XData) do
  begin
    TrainingPairs.Add(
      TNNetVolumePair.Create(
        TNNetVolume.Create(XData[Y]),
        TNNetVolume.Create(YData[Y])
      )
    );
  end;
  NFit.FileNameBase := 'data/nn/chatbot/model';
  NFit.InitialLearningRate := 0.001;
  NFit.LearningRateDecay := 0;
  NFit.L2Decay := 0;
  NFit.Verbose := False;
  NFit.InferHitFn := @MonopolarCompare;
  {$ifdef WINDOWS}
  // NFit.HideMessages;
  {$endif}
  NFit.MessageProc := @TMessage(nil).EpochMessageProc;
  NFit.ErrorProc := @TMessage(nil).NoMessageProc;
  NFit.Fit(NN, TrainingPairs, nil, nil, 16, 1000);

  Satania.ActionFromFile('system/loading-stop.evil', False);
  Satania.Talk(
    'Learning completed in ' + IntToStr((GetTickCount64 - Ticks) div 1000) + ' seconds!' + #10#10 +
    // DebugErrors(NN) + #13 +
    'See "' + NFit.FileNameBase + '.csv" for details.'
  );
  TagList.SaveToFile('data/nn/chatbot/model.tag');
  WordList.SaveToFile('data/nn/chatbot/model.word');
  TrainingPairs.Free;
  NFit.Free;
  NN.Free;
end;

procedure TTrainThread.Execute;
begin
  Satania.IsBlocked := True;    
  Prepare;
  try
    try
      ReadRules;
      PrepareTrainingData;
      Train;
    except
      on E: Exception do
        Satania.Talk(E.Message);
    end;
  finally
    Cleanup; 
    Reload;
  end;
  Satania.IsBlocked := False;
  Terminate;
end;

procedure RunTrain;
var
  Thread: TTrainThread;
begin
  if Satania.IsBlocked then Exit;
  Thread := TTrainThread.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.Start;
end;

end.


