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

unit voskthread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Vosk, uPocketSphinx, fpjson, jsonparser;

type
  TVoskThread = class(TThread)
  private            
    FOnStateChange: TOnPocketSphinxStateChange;
    FOnHypothesis: TOnPocketSphinxHypothesis;
    FState: TPocketSphinxState;
    FActive: Boolean;
    FAudioSource: TPocketSphinxAudioSource;
    FModel: PVoskModel;
    FRec: PVoskRecognizer;
    FRecentHypothesis: String;
    procedure SendHypothesis;   
    procedure SendState;   
    procedure SetState(S: TPocketSphinxState);
    procedure SetAudioSource(AudioSource: TPocketSphinxAudioSource);
    procedure SetActive(V: Boolean);
  public
    ModelPath: String;

    constructor Create;
    procedure Init;
    destructor Destroy; override;
    procedure Execute; override;

    property Active: Boolean read FActive write SetActive;
    property AudioSource: TPocketSphinxAudioSource read FAudioSource write SetAudioSource;
    property State: TPocketSphinxState read FState write SetState;
    property OnStateChange: TOnPocketSphinxStateChange read FOnStateChange write FOnStateChange;
    property OnHypothesis: TOnPocketSphinxHypothesis read FOnHypothesis write FOnHypothesis;
  end;

implementation

procedure TVoskThread.SendHypothesis;
begin
  if Assigned(FOnHypothesis) then
    OnHypothesis(Self, 0, FRecentHypothesis);
end;

procedure TVoskThread.SendState;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self, FState);
end;

constructor TVoskThread.Create;
begin
  inherited Create(True);
end;

destructor TVoskThread.Destroy;
begin
  if Assigned(FAudioSource) then
    FAudioSource.Free;   
  if FRec <> nil then
    vosk_recognizer_free(FRec);
  if FModel <> nil then
    vosk_model_free(FModel);
  inherited;
end;

procedure TVoskThread.Init;
begin
  FModel := vosk_model_new(PChar(ModelPath));
  FRec := vosk_recognizer_new(FModel, 16000);
  State := rsInitialized;
end;

procedure TVoskThread.SetActive(V: Boolean);
begin
  if V and not FActive then
  begin
    FActive := V;
    Start;
  end;
end;

procedure TVoskThread.Execute;
var
  Adbuf: array[0..1024*64-1] of SmallInt;
  NFrames, Final: Integer;
  S: PChar;
  Text: String;
  JSObject: TJSONObject;
begin
  while not Terminated do
  begin
    if FActive then
    begin
      if Assigned(FAudioSource) then
        if FAudioSource.Ready then
          NFrames := FAudioSource.GetData(@Adbuf[0], SizeOf(Adbuf));

      if NFrames > 0 then
      begin
        Final := vosk_recognizer_accept_waveform_s(FRec, @Adbuf[0], NFrames);
        if not Boolean(Final) then
          S := vosk_recognizer_partial_result(FRec)
        else
          S := vosk_recognizer_result(FRec);
        JSObject := GetJSON(S) as TJSONObject;
        try
          Text := JSObject['partial'].AsString;
        except
          on E: Exception do;
        end;
        if Text = '' then
          try
            Text := JSObject['text'].AsString;
          except
            on E: Exception do;
          end;
        if Text <> '' then
        begin
          FRecentHypothesis := Text;
          if State <> rsAnalyze then
            State := rsAnalyze;
        end else
        if (Text = '') and (FRecentHypothesis <> '') then
        begin
          Synchronize(@SendHypothesis);
          FRecentHypothesis := '';
          State := rsReady;
        end else
        begin
          State := rsListening;
        end;
        JSObject.Free;
      end else
      begin
        if (FRecentHypothesis <> '') then
        begin
          Synchronize(@SendHypothesis);
          FRecentHypothesis := '';
          State := rsReady;
        end;
      end;
    end;
    Sleep(256);
  end;
end;

procedure TVoskThread.SetAudioSource(AudioSource: TPocketSphinxAudioSource);
begin
  if Assigned(FAudioSource) then
    FAudioSource.Free;
  FAudioSource := AudioSource;
end;

procedure TVoskThread.SetState(S: TPocketSphinxState);
begin
  if FState <> S then
  begin
    FState := S;
    Synchronize(@SendState);
  end;
end;

end.

