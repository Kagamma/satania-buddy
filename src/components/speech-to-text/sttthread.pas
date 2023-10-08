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

unit SttThread;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Vosk, whisper, JsonTools;

type
  TSttState = (
    sttNotInitialized, // Initial state
    sttInitialized, // Indicates both decoder and audio device initialized successfully
    sttReady, // Waiting for audio input
    sttListening, // "In-speech"
    sttAnalyze, // Recognizer is analyzing provided audio data
    sttError // Error state, LastErrorMsg property stores the error message
  );

type
  TOnSttStateChange = procedure(Sender: TObject; AState: TSttState) of object;
  TOnSttHypothesis = procedure(Sender: TObject; AScore: Integer; AHypothesis: String) of object;

type
  TSttAudioSource = class
  protected
    FReady: Boolean;
  public
    property Ready: Boolean read FReady;
    function GetData(buffer: Pointer; nmax: Cardinal): Integer; virtual; abstract;
  end;

type
  TSttBaseThread = class(TThread)
  protected
    FOnStateChange: TOnSttStateChange;
    FOnHypothesis: TOnSttHypothesis;
    FState: TSttState;
    FActive: Boolean;
    FAudioSource: TSttAudioSource;
    FRecentHypothesis: String;
    procedure SendHypothesis;
    procedure SendState;
    procedure SetState(S: TSttState);
    procedure SetAudioSource(AudioSource: TSttAudioSource);
    procedure SetActive(V: Boolean);
  public
    ModelPath: String;

    property Active: Boolean read FActive write SetActive;
    property AudioSource: TSttAudioSource read FAudioSource write SetAudioSource;
    property State: TSttState read FState write SetState;
    property OnStateChange: TOnSttStateChange read FOnStateChange write FOnStateChange;
    property OnHypothesis: TOnSttHypothesis read FOnHypothesis write FOnHypothesis;
  end;

  TVoskThread = class(TSttBaseThread)
  protected
    FModel: PVoskModel;
    FRec: PVoskRecognizer;
  public
    constructor Create;
    procedure Init;
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TWhisperThread = class(TSttBaseThread)
  protected
    FContext: Pwhisper_context;
  public
    constructor Create;
    procedure Init;
    destructor Destroy; override; 
    procedure Execute; override;
  end;

implementation

uses
  Math;

var
  IsRunning: Boolean = False;

// ----- TSttBaseThread -----

procedure TSttBaseThread.SetAudioSource(AudioSource: TSttAudioSource);
begin
  if Assigned(FAudioSource) then
    FAudioSource.Free;
  FAudioSource := AudioSource;
end;

procedure TSttBaseThread.SendHypothesis;
begin
  if Assigned(Self.FOnHypothesis) then
    OnHypothesis(Self, 0, Self.FRecentHypothesis);
end;

procedure TSttBaseThread.SendState;
begin
  if Assigned(Self.FOnStateChange) then
    FOnStateChange(Self, Self.FState);
end;

procedure TSttBaseThread.SetState(S: TSttState);
begin
  if FState <> S then
  begin
    FState := S;
    Synchronize(@SendState);
  end;
end;

procedure TSttBaseThread.SetActive(V: Boolean);
begin
  if V and not FActive then
  begin
    FActive := V;
    Start;
  end;
end;

// ----- Vosk -----

constructor TVoskThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

destructor TVoskThread.Destroy;
begin
  if Assigned(FAudioSource) then
    FAudioSource.Free;   
  if FRec <> nil then
    vosk_recognizer_free(FRec);
  if FModel <> nil then
    vosk_model_free(FModel);
  IsRunning := False;
  inherited;
end;

procedure TVoskThread.Init;
begin
  FModel := vosk_model_new(PChar(ModelPath));
  FRec := vosk_recognizer_new(FModel, 16000);
  State := sttInitialized;
end;

procedure TVoskThread.Execute;
var
  Adbuf: array[0..1024*64-1] of SmallInt;
  NFrames, Final: Integer;
  S: PChar;
  Text: String;
  Json, N: TJsonNode;
begin
  while IsRunning do
  begin
    if Terminated then
      Exit;
    Sleep(256);
  end;
  IsRunning := True;
  NFrames := 0;
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
        Json := TJsonNode.Create;
        Json.TryParse(S);
        N := Json.Find('partial');
        if N <> nil then
          Text := N.AsString;
        if Text = '' then
        begin
          N := Json.Find('text');
          if N <> nil then
            Text := N.AsString;
        end;
        if Text <> '' then
        begin
          FRecentHypothesis := Text;
          if State <> sttAnalyze then
            State := sttAnalyze;
        end else
        if (Text = '') and (FRecentHypothesis <> '') then
        begin
          Synchronize(@SendHypothesis);
          FRecentHypothesis := '';
          State := sttReady;
        end else
        begin
          State := sttListening;
        end;
        Json.Free;
      end else
      begin
        if (FRecentHypothesis <> '') then
        begin
          Synchronize(@SendHypothesis);
          FRecentHypothesis := '';
          State := sttReady;
        end;
      end;
    end;
    Sleep(256);
  end;
end;


// ----- Whisper -----


constructor TWhisperThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
end;

destructor TWhisperThread.Destroy;
begin
  IsRunning := False;
  if Assigned(FAudioSource) then
    FAudioSource.Free;
  if Self.FContext <> nil then
    whisper_free(Self.FContext);
  inherited;
end; 

procedure TWhisperThread.Init;
begin
  Self.FContext := whisper_init_from_file(PChar(Self.ModelPath));

  State := sttInitialized;
end;

procedure TWhisperThread.Execute;
var
  Adbuf: array[0..1024*64*4-1] of SmallInt;
  AdbufFloat: array[0..1024*64*10-1] of Single;
  AdbufFloatCount: Integer;
  NFrames, I, NumSegments, Tmp: Integer;
  Params: Twhisper_full_params;
  Text, S: String;
begin
  while IsRunning do
  begin
    if Terminated then
      Exit;
    Sleep(256);
  end;
  IsRunning := True;
  AdbufFloatCount := 0;
  NFrames := 0;
  while not Terminated do
  begin
    if FActive then
    begin
      if Assigned(Self.FAudioSource) then
        if Self.FAudioSource.Ready then
          NFrames := Self.FAudioSource.GetData(@Adbuf[0], SizeOf(Adbuf));
      if NFrames > 0 then
      begin
        if AdbufFloatCount + NFrames >= Length(AdbufFloat) then
        begin
          AdbufFloatCount := 0;
        end;
        for I := 0 to NFrames - 1 do
          AdbufFloat[AdbufFloatCount + I] := Adbuf[I] / 64000;
        AdbufFloatCount := AdbufFloatCount + NFrames;

        Params := whisper_full_default_params(WHISPER_SAMPLING_GREEDY);
        Params.n_threads := 4;
        Params.print_realtime := false;
        Params.print_progress := false;
        Params.temperature_inc := 0;
        Params.prompt_n_tokens := 0;
        Params.prompt_tokens := nil;

        whisper_full(Self.FContext, Params, @AdbufFloat[0], AdbufFloatCount);
        NumSegments := whisper_full_n_segments(Self.FContext);
        Text := '';
        for I := 0 to NumSegments - 1 do
        begin
          S := Trim(whisper_full_get_segment_text(Self.FContext, I));
          Text := Text + ' ';
          if not (S[1] in ['(', '[', '<']) and not
            (S[Length(S)] in [')', ']', '>']) and
            (LowerCase(S) <> 'you') and
            (S.IndexOf('♪') < 0) then
            Text := Text + S;
        end;
        Text := Trim(Text);
        if Text <> '' then
        begin
          if Self.FRecentHypothesis = '' then
          begin
            Tmp := Min(AdbufFloatCount, NFrames + 2048);
            AdbufFloatCount := AdbufFloatCount - Tmp;
            for I := 0 to Tmp - 1 do
              AdbufFloat[I] := AdbufFloat[AdbufFloatCount + I];
            AdbufFloatCount := Tmp;
          end;
          if (Self.FRecentHypothesis <> '') and (Text = Self.FRecentHypothesis) then
          begin
            Writeln('> whisper.cpp - Final: ', Self.FRecentHypothesis);
            Synchronize(@SendHypothesis);
            FRecentHypothesis := '';
            AdbufFloatCount := 0;
            Self.State := sttReady;
          end else
          begin 
            Writeln('> whisper.cpp - Partial: ', Text);
            Self.FRecentHypothesis := Text;
            if Self.State <> sttAnalyze then
              Self.State := sttAnalyze;
          end;
        end else
        if (Text = '') and (Self.FRecentHypothesis <> '') then
        begin
          Writeln('> whisper.cpp - Final: ', Self.FRecentHypothesis);
          Synchronize(@SendHypothesis);
          FRecentHypothesis := '';
          AdbufFloatCount := 0;
          Self.State := sttReady;
        end else
        begin
          Self.State := sttListening;
        end;
      end else
      begin
        if (Self.FRecentHypothesis <> '') then
        begin
          Writeln('> whisper.cpp - Final: ', Self.FRecentHypothesis);
          Self.Synchronize(@Self.SendHypothesis);
          Self.FRecentHypothesis := '';
          AdbufFloatCount := 0;
          Self.State := sttReady;
        end;
      end;
    end;
    Sleep(1024);
  end;
end;

end.

