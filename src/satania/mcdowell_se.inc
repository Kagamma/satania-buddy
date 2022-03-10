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

{$ifdef unit_protected}
    function SELoadModel(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESetScale(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESetVisible(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetVisible(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SETalk(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SENotify(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SERun(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsRunning(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetRunResult(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsAnimationPlaying(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEStopAnimation(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEStopAllAnimations(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEStartAnimation(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESetAnimationSpeed(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsSoW(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsLewd(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsSilent(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsSpeechToText(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SELoadScheme(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEDelta(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SELoadEmails(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetUnseenEmailCount(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetEmailSender(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetEmailSubject(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetEmailBody(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsEmailLoading(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsEmailSuccess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsEmailConfigured(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEDefaultScheme(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESoundPlay(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEOpenURL(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEChatModeSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue; 
    function SEChatResultGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEURLGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue; 
    function SEURLIsSuccess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEURLGetResult(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEURLProcess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
{$endif}

{$ifdef unit_implementation}
function TSatania.SETalk(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if Length(Args) = 1 then
  begin
    Talk(Args[0]);
    VM.IsPaused := True;
  end else
  begin
    if Args[1] = 0 then
    begin
      TalkWithoutBlock(Args[0]);
    end else
    begin
      Talk(Args[0]);
      VM.IsPaused := True;
    end;
  end;
end;

function TSatania.SENotify(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Notify(Name, Args[0]);
end;

function TSatania.SERun(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Exec(Args[0]);
end;

function TSatania.SEIsRunning(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if RunList.IndexOf(Args[0]) >= 0 then
    Result := True
  else
    Result := False;
end;

function TSatania.SEGetRunResult(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := '';
  try
    Result := RunResultList[Args[0].VarString];
    RunResultList.Remove(Args[0].VarString);
  except
  end;
end;

function TSatania.SELoadModel(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  LoadModel(Args[0]);
end;

function TSatania.SEIsAnimationPlaying(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  TimeSensor: TTimeSensorNode;
begin
  try
    TimeSensor := Sprite.Node(Args[0]) as TTimeSensorNode;
    if (not TimeSensor.Loop) and (TimeSensor.Enabled) and (TimeSensor.StartTime > 0) and
       (TimeSensor.IsActive) and (not TimeSensor.IsPaused) then
      Result := 1
    else
      Result := 0;
  except
    on E: Exception do
      TalkWithoutBlock(E.Message);
  end;
end;

function TSatania.SEStartAnimation(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  StartAnimation(Args[0], Boolean(Round(Args[1].VarNumber)));
end;

function TSatania.SESetAnimationSpeed(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  SetAnimationSpeed(Args[0], Args[1].VarNumber);
end;

function TSatania.SEStopAnimation(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  StopAnimation(Args[0]);
end;

function TSatania.SEStopAllAnimations(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  StopAllAnimations;
end;

function TSatania.SELoadScheme(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Satania.ActionFromFile(Args[0]);
end;

function TSatania.SEDelta(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Delta;
end;

function TSatania.SELoadEmails(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  SataniaIMAP.GetMessages;
end;

function TSatania.SEGetUnseenEmailCount(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.MailList.Count;
end;

function TSatania.SEGetEmailSender(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.MailList[Round(Args[0].VarNumber)].Sender;
end;

function TSatania.SEGetEmailSubject(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.MailList[Round(Args[0].VarNumber)].Subject;
end;

function TSatania.SEGetEmailBody(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.MailList[Round(Args[0].VarNumber)].Body;
end;

function TSatania.SEIsEmailLoading(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.IsRunning;
end;

function TSatania.SEIsEmailSuccess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.IsSuccess;
end;

function TSatania.SEIsEmailConfigured(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.IsEmailConfigured;
end;

function TSatania.SEDefaultScheme(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Save.Settings.DefaultEvilScheme;
end;

function TSatania.SESoundPlay(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  B: TSataniaSoundBehavior;
begin
  B := TSataniaSoundBehavior.Create(Sprite);
  B.URL := Args[0];
  Sprite.AddBehavior(B);
end;

function TSatania.SEOpenURL(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  OpenURL(Args[0]);
end;

function TSatania.SEChatModeSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  ChatMode := Round(Args[0].VarNumber);
end;

function TSatania.SEChatResultGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := ChatResult;
  ChatResult := '';
end;

function TSatania.SEIsSoW(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Save.SitOnWindow
end;

function TSatania.SEIsLewd(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Save.Settings.Lewd;
end;

function TSatania.SEIsSilent(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Save.Silent
end;

function TSatania.SEIsSpeechToText(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Save.SpeechToText
end;

function TSatania.SESetScale(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  SetScale(Args[0].VarNumber);
end;

function TSatania.SESetVisible(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if Args[0].VarNumber = 0 then
    SetVisible(False)
  else
    SetVisible(True);
end;

function TSatania.SEGetVisible(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Sprite.Visible;
end;

function TSatania.SEURLGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Thread: TSataniaNetThread;
begin  
  RunList.Add(Args[0]);
  Thread := TSataniaNetThread.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.URL := Args[0];
  Thread.Start;
  Result := Args[0].VarString;
end;

function TSatania.SEURLIsSuccess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if RunList.IndexOf(Args[0]) >= 0 then
    Result := False
  else
    Result := True;
end;

function TSatania.SEURLGetResult(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := '';
  try
    Result := RunResultList[Args[0].VarString];
    RunResultList.Remove(Args[0].VarString);
  except
  end;
end;

function TSatania.SEURLProcess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  X: IXQValue;
  SL: TStrings;
  I: Integer;
  A: TSEValueArray;
begin
  SL := TStringList.Create;
  try
    for X in simpleinternet.process(Args[0], Args[1]) do
    begin
      SL.Add(X.ToString);
    end;
    Result.Kind := sevkArray;
    SetLength(A, SL.Count);
    for I := 0 to SL.Count - 1 do
      A[I] := SL[I];
    Result.VarArray := Pointer(A);
  finally
    SL.Free;
  end;
end;

{$endif}
