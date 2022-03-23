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
    function SESpriteTalkLoopSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpriteTalkFinishSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpriteLoad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpriteScaleGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpriteVisibleSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpriteVisibleGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SETalk(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SENotify(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEProcessRun(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEProcessIsRunning(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEProcessResultGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsAnimationPlaying(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEStopAnimation(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEStopAllAnimations(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEStartAnimation(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESetAnimationSpeed(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsSoW(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsLewd(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsSilent(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsSpeechToText(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESchemeLoad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEDelta(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SELoadEmails(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetUnseenEmailCount(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetEmailSender(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetEmailSubject(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetEmailBody(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsEmailLoading(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsEmailSuccess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsEmailConfigured(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESchemeDefault(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESoundPlay(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEOpenURL(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEChatModeSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue; 
    function SEChatResultGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEURLGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;        
    function SEURLPost(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEURLUpload(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEURLIsSuccess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEURLGetResult(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEURLProcess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SENumbers(const VM: TSEVM; const Args: array of TSEValue): TSEValue;     
    function SEMonthsToNumbers(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEReminderCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEReminderTodayGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue; 
    function SEClipboardGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEClipboardToFile(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEFileRead(const VM: TSEVM; const Args: array of TSEValue): TSEValue;   
    function SEFileWrite(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEFileExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEFileDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
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

function TSatania.SEProcessRun(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Exec(Args[0]);
end;

function TSatania.SEProcessIsRunning(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if RunList.IndexOf(Args[0]) >= 0 then
    Result := True
  else
    Result := False;
end;

function TSatania.SEProcessResultGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := '';
  try
    Result := RunResultList[Args[0].VarString];
    RunResultList.Remove(Args[0].VarString);
  except
  end;
end;

function TSatania.SESpriteLoad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  LoadModel(Args[0]);
end;

function TSatania.SESpriteTalkLoopSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  AnimTalkLoop := Args[0];
end;

function TSatania.SESpriteTalkFinishSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  AnimTalkFinish := Args[0];
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

function TSatania.SESchemeLoad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
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

function TSatania.SESchemeDefault(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
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

function TSatania.SESpriteScaleGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  SetScale(Args[0].VarNumber);
end;

function TSatania.SESpriteVisibleSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if Args[0].VarNumber = 0 then
    SetVisible(False)
  else
    SetVisible(True);
end;

function TSatania.SESpriteVisibleGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Sprite.Visible;
end;

function TSatania.SEURLGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Thread: TSataniaHttpGetThread;
begin  
  RunList.Add(Args[0]);
  Thread := TSataniaHttpGetThread.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.URL := Args[0];
  Thread.Start;
  Result := Args[0].VarString;
end;

function TSatania.SEURLPost(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Thread: TSataniaHttpPostThread;
  I: Integer;
begin
  RunList.Add(Args[0]);
  Thread := TSataniaHttpPostThread.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.URL := Args[0];
  for I := 0 to Length(Args[1].VarArray) do
  begin
    if Args[1].VarArray[I].Kind = sevkSingle then
      Thread.FormData.Add(FloatToStr(Args[1].VarArray[I].VarNumber))
    else
      Thread.FormData.Add(Args[1].VarArray[I].VarString);
  end;
  Thread.Start;
  Result := Args[0].VarString;
end;

function TSatania.SEURLUpload(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Thread: TSataniaHttpPostThread;
  I, P: Integer;
begin
  RunList.Add(Args[0]);
  Thread := TSataniaHttpPostThread.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.URL := Args[0];
  for I := 0 to Length(Args[1].VarArray) div 2 - 1 do
  begin
    P := I * 2;
    if Args[1].VarArray[I].Kind = sevkSingle then
      Thread.FormData.Values[Args[1].VarArray[P].VarString] := FloatToStr(Args[1].VarArray[P + 1].VarNumber)
    else
      Thread.FormData.Values[Args[1].VarArray[P].VarString] := Args[1].VarArray[P + 1].VarString;
  end;
  Thread.FieldName := Args[2];
  Thread.FileName := Args[3];
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

function TSatania.SENumbers(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  A: TDoubleDynArray;
  I: Integer;
begin
  A := WordsToNumbers(Args[0]);
  Result.Kind := sevkArray;
  SetLength(Result.VarArray, Length(A));
  for I := 0 to Length(A) - 1 do
  begin
    Result.VarArray[I] := A[I];
  end;
end;

function TSatania.SEMonthsToNumbers(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  A: TDoubleDynArray;
  I: Integer;
begin
  A := WordsToDates(Args[0]);
  Result.Kind := sevkArray;
  SetLength(Result.VarArray, Length(A));
  for I := 0 to Length(A) - 1 do
  begin
    Result.VarArray[I] := A[I];
  end;
end;

function TSatania.SEReminderCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Reminder: TReminderCollectionItem;
  DD, TT: TDateTime;
  Y, M, D, H, MM, S, MS: Word;
begin
  DD := Args[0].VarNumber;
  TT := Args[1].VarNumber;
  DecodeDate(DD, Y, M, D);    
  DecodeTime(TT, H, MM, S, MS);
  Reminder := Save.Reminders.Add as TReminderCollectionItem;
  Reminder.Kind := 1;
  Reminder.Enabled := True;
  Reminder.Year := Y;
  Reminder.Month := M;
  Reminder.Day := D;
  Reminder.Hour := H;
  Reminder.Minute := MM;
  Reminder.Script := Args[2];
  Save.SaveToFile('configs.json');
end;

function TSatania.SEReminderTodayGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Reminder: TReminderCollectionItem;
  Count, I, J: Integer;
  Y, M, D: Word;
  V: TSEValue;
begin
  Result.Kind := sevkArray;
  DecodeDate(Now, Y, M, D);
  Count := 0;
  for I := Save.Reminders.Count - 1 downto 0 do
  begin
    Reminder := TReminderCollectionItem(Save.Reminders.Items[I]);
    if (Reminder.Year = Y) and
      (Reminder.Month = M) and
      (Reminder.Day = D) then
    begin
      SetLength(V.VarArray, 5);    
      SetLength(Result.VarArray, Count + 1);
      Result.VarArray[Count].Kind := sevkArray;
      SetLength(Result.VarArray[Count].VarArray, 2);
      Result.VarArray[Count].VarArray[0] := Reminder.Hour;      
      Result.VarArray[Count].VarArray[1] := Reminder.Minute;
      Inc(Count);
    end;
  end;
end;

function TSatania.SEClipboardGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;  
begin
  if Clipboard.HasFormat(PredefinedClipboardFormat(pcfText)) then
    Result := StringReplace(Clipboard.AsText, 'file://', '', [rfReplaceAll])
  else
    Result := '';
end;    

function TSatania.SEClipboardToFile(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: String;
  Jpeg: TJPEGImage;
  FS: TFileStream;
  B: TBitmap;
begin
  if Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then
  begin
    Jpeg := TJPEGImage.Create;
    B := TBitmap.Create;
    try
      B.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));
      Jpeg.CompressionQuality := 80;
      Jpeg.Assign(B);
      Jpeg.SaveToFile(Args[0]);
    finally
      Jpeg.Free;
      B.Free;
    end;
  end else
  if Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then
  begin
    FS := TFileStream.Create(Args[0], fmCreate);
    try
      S := Clipboard.AsText;
      FS.WriteAnsiString(S);
    finally   
      FS.Free;
    end;
  end;
end;   

function TSatania.SEFileRead(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  SL: TStrings;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(Args[0]);
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TSatania.SEFileWrite(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  SL: TStrings;
begin
  SL := TStringList.Create;
  try
    SL.Text := Args[1]; 
    SL.SaveToFile(Args[0]);
  finally
    SL.Free;
  end;
end;

function TSatania.SEFileExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := FileExists(Args[0].VarString);
end;

function TSatania.SEFileDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue; 
begin
  DeleteFile(Args[0].VarString);
end;

{$endif}
