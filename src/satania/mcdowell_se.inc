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
    function SESpriteTalkSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpriteLoad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpriteScaleGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpriteVisibleSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpriteVisibleGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SETalk(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEAsk(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEAnswer(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
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
    function SEIsEmailSMTPConfigured(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEEmailSMTPSend(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
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
    function SEURLEncode(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEURLDecode(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
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
    function SEFileFindAll(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEDirectoryCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEDirectoryDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEDirectoryFindAll(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEDirectoryExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEJSONParse(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEJSONStringify(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SELocalFlagGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SELocalFlagSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESketchCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESketchLoadTexture(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESketchDrawTriangles(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESketchExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESketchClear(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESketchClearAll(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEWorkerCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEWorkerExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEWorkerDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEToolEvilCEditor(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEToolHexEditor(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
{$endif}

{$ifdef unit_implementation}

function SEValueToText(const Value: TSEValue; const IsRoot: Boolean = True): String;
var
  Key: String;
  IsValidArray: Boolean;
  I: Integer = 0;
begin
  case Value.Kind of
    sevkString:
      begin
        if IsRoot then
          Result := Value.VarString^
        else
          Result := '"' + Value.VarString^ + '"';
      end;
    sevkNumber:
      Result := PointFloatToStr(Value.VarNumber);
    sevkMap:
      begin
        Result := '[';
        IsValidArray := SEMapIsValidArray(Value);
        if IsValidArray then
        begin
          for I := 0 to TSEValueMap(Value.VarMap).List.Count - 1 do
          begin
            if I > 0 then
              Result := Result + ', ';
            Result := Result + SEValueToText(SEMapGet(Value, I), False);
          end;
        end else
        begin
          for Key in TSEValueMap(Value.VarMap).Keys do
          begin
            if I > 0 then
              Result := Result + ', ';
            Result := Result + '"' + Key + '": ' + SEValueToText(SEMapGet(Value, Key), False);
            Inc(I);
          end;
        end;
        Result := Result + ']'
      end;
    sevkNull:
      Result := 'null';
    else
      Result := Value;
  end;
end;

function TSatania.SETalk(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Key: String;
begin
  if Length(Args) = 1 then
  begin
    Talk(SEValueToText(Args[0]));
    VM.IsPaused := True;
  end else
  begin
    if Args[1] = 0 then
    begin
      TalkWithoutBlock(SEValueToText(Args[0]));
    end else
    begin
      Talk(SEValueToText(Args[0]));
      VM.IsPaused := True;
    end;
  end;
end;

function TSatania.SEAsk(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
begin
  FormAsk.Caption := Args[0].VarString^;
  if FormAsk.Caption = '' then
  begin
    FormAsk.BorderStyle := Controls.bsNone;
  end else
  begin
    FormAsk.BorderStyle := Controls.bsSizeable;
  end;
  FormAsk.WindowState := wsNormal;
  Ask(SEValueToText(Args[1]));
  if Length(Args) = 4 then
  begin
    FormAsk.Width := Args[2];
    FormAsk.Height := Args[3];
  end else
  begin
    FormAsk.Width := 314;
    FormAsk.Height := 148;
  end;
  VM.IsPaused := True
end;

function TSatania.SEAnswer(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
begin
  if FormAsk.IsAnswerText then
    GC.AllocString(@Result, FormAsk.AnswerText)
  else
  begin
    GC.AllocMap(@Result);
    for I := 0 to FormAsk.Answer.Count - 1 do
      SEMapSet(Result, FormAsk.Answer.Names[I], FormAsk.Answer.ValueFromIndex[I]);
  end;
  FormAsk.IsAnswerText := False;
end;

function TSatania.SENotify(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Notify(Name, SEValueToText(Args[0]));
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
  Result := SENull;
  try
    Result := RunProcessResultList[Args[0].VarString^];
    RunProcessResultList.Remove(Args[0].VarString^);
  except
  end;
end;

function TSatania.SESpriteLoad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  LoadModel(Args[0]);
end;

function TSatania.SESpriteTalkSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
begin
  AnimTalkLoop := Args[0];
  AnimTalkFinish := Args[1];
  Self.AnimTalkScriptList.Clear;
  if Length(Args) = 3 then
  begin
    for I := 0 to SESize(Args[2]) - 1 do
    begin
      Self.AnimTalkScriptList.Add(SEMapGet(Args[2], I).VarString^);
    end;
  end;
end;

function TSatania.SEIsAnimationPlaying(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  TimeSensor: TTimeSensorNode;
begin
  try
    TimeSensor := TCastleScene(Sprite).Node(Args[0]) as TTimeSensorNode;
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

function TSatania.SEIsEmailSMTPConfigured(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := TSataniaSMTP.IsEmailConfigured;
end;

function TSatania.SEEmailSMTPSend(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  SataniaSMTP: TSataniaSMTP;
  I: Integer;
begin
  SataniaSMTP := TSataniaSMTP.Create;
  SataniaSMTP.Sender := Args[0];
  SataniaSMTP.MailTo := Args[1];
  SataniaSMTP.ReplyTo := Args[2];
  SataniaSMTP.Subject := Args[3];
  SataniaSMTP.Body := Args[4];
  SetLength(SataniaSMTP.Attachments, SESize(Args[5]));
  for I := 0 to SESize(Args[5]) - 1 do
    SataniaSMTP.Attachments[I] := SEMapGet(Args[5], I);

  SataniaSMTP.Start;
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

function TSatania.SESpriteVisibleSet(const VM: TSEVM  ; const Args: array of TSEValue): TSEValue;
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
  I: Integer;
  S: String;
begin
  I := RunList.IndexOf(Args[0]);
  if I >= 0 then
    RunList.Delete(I);
  RunList.Add(Args[0]);
  Thread := TSataniaHttpGetThread.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.URL := Args[0];
  if Args[1].Kind = sevkMap then
    for S in TSEValueMap(Args[1].VarMap).Keys do
    begin
      Thread.HTTP.AddHeader(S, SEMapGet(Args[1], S));
    end;
  Thread.Start;
  Result := Args[0].VarString^;
end;

function TSatania.SEURLPost(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Thread: TSataniaHttpPostThread;
  Key: String;
  I: Integer;
  V: TSEValue;
  S: String;
begin
  I := RunList.IndexOf(Args[0]);
  if I >= 0 then
    RunList.Delete(I);
  RunList.Add(Args[0]);
  Thread := TSataniaHttpPostThread.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.URL := Args[0];
  if Args[1].Kind = sevkMap then
    for S in TSEValueMap(Args[1].VarMap).Keys do
    begin
      Thread.HTTP.AddHeader(S, SEMapGet(Args[1], S));
    end;
  case Args[2].Kind of
    sevkMap:
      Thread.FormData.Text := SEJSONStringify(VM, [Args[2]]);
    sevkString:
      Thread.FormData.Text := Args[2];
  end;
  Thread.Start;
  Result := Args[0].VarString^;
end;

function TSatania.SEURLUpload(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Thread: TSataniaHttpPostThread;
  I, P: Integer;
  Key: String;
  V: TSEValue;
begin
  I := RunList.IndexOf(Args[0]);
  if I >= 0 then
    RunList.Delete(I);
  RunList.Add(Args[0]);
  Thread := TSataniaHttpPostThread.Create(True);
  Thread.FreeOnTerminate := True;
  Thread.URL := Args[0];
  case Args[2].Kind of
    sevkMap:
      Thread.FormData.Text := SEJSONStringify(VM, [Args[2]]);
    sevkString:
      Thread.FormData.Text := Args[2];
  end;
  Thread.FieldName := Args[3];
  Thread.FileName := Args[4];
  Thread.Start;
  Result := Args[0].VarString^;
end;

function TSatania.SEURLIsSuccess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if RunList.IndexOf(Args[0]) >= 0 then
    Result := False
  else
    Result := True;
end;

function TSatania.SEURLGetResult(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  HttpResponse: THttpResponseRec;
begin
  Result := SENull;
  try
    GC.AllocMap(@Result);
    HttpResponse := RunHttpResultList[Args[0].VarString^];
    SEMapSet(Result, 'status', HttpResponse.Status);
    SEMapSet(Result, 'data', HttpResponse.Data);
    RunHttpResultList.Remove(Args[0].VarString^);
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
    GC.AllocMap(@Result);
    for I := 0 to SL.Count - 1 do
      SEMapSet(Result, I, SL[I]);
  finally
    SL.Free;
  end;
end;

function TSatania.SEURLEncode(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := EncodeURLElement(Args[0]);
end;

function TSatania.SEURLDecode(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := DecodeURLElement(Args[0]);
end;

function TSatania.SENumbers(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  A: TDoubleDynArray;
  I: Integer;
begin
  A := WordsToNumbers(Args[0]);
  GC.AllocMap(@Result);
  for I := 0 to Length(A) - 1 do
  begin
    SEMapSet(Result.VarMap, I, A[I]);
  end;
end;

function TSatania.SEMonthsToNumbers(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  A: TDoubleDynArray;
  I: Integer;
begin
  A := WordsToDates(Args[0]);
  GC.AllocMap(@Result);
  for I := 0 to Length(A) - 1 do
  begin
    SEMapSet(Result.VarMap, I, A[I]);
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
  HA, MA: array of Word;
  V: TSEValue;
begin
  DecodeDate(Now, Y, M, D);
  Count := 0;
  GC.AllocMap(@Result);
  for I := Save.Reminders.Count - 1 downto 0 do
  begin
    Reminder := TReminderCollectionItem(Save.Reminders.Items[I]);
    if (Reminder.Year = Y) and
      (Reminder.Month = M) and
      (Reminder.Day = D) then
    begin
      SetLength(HA, Count + 1);
      SetLength(MA, Count + 1);
      HA[Count] := Reminder.Hour;
      MA[Count] := Reminder.Minute;
      Inc(Count);
    end;
  end;
  GC.AllocMap(@Result);
  for I := 0 to Count - 1 do
  begin
    GC.AllocMap(@V);
    SEMapSet(V, 'hour', HA[I]);
    SEMapSet(V, 'minute', MA[I]);
    SEMapSet(Result, I, V);
  end;
end;

function TSatania.SEClipboardGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if Clipboard.HasFormat(PredefinedClipboardFormat(pcfText)) then
    Result := StringReplace(Clipboard.AsText, 'file://', '', [rfReplaceAll])
  else
    Result := SENull;
end;

function TSatania.SEClipboardToFile(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S, Ext: String;
  Jpeg: TJPEGImage;
  Png: TPortableNetworkGraphic;
  FS: TFileStream;
  B: TBitmap;
begin
  Ext := LowerCase((Args[0].VarString^));
  if Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) then
  begin
    B := TBitmap.Create;
    if Ext = '.png' then
    begin
      Png := TPortableNetworkGraphic.Create;
      try
        B.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));
        Png.Assign(B);
        Png.SaveToFile(Args[0]);
      finally
        Png.Free;
        B.Free;
      end;
    end else
    begin
      Jpeg := TJPEGImage.Create;
      try
        B.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));
        Jpeg.CompressionQuality := 80;
        Jpeg.Assign(B);
        Jpeg.SaveToFile(Args[0]);
      finally
        Jpeg.Free;
        B.Free;
      end;
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
    if FileExists(Args[0].VarString^) then
      SL.LoadFromFile(Args[0]);
    SL.Text := SL.Text + Args[1];
    SL.SaveToFile(Args[0]);
  finally
    SL.Free;
  end;
end;

function TSatania.SEFileExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := FileExists(Args[0].VarString^);
end;

function TSatania.SEFileDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  DeleteFile(Args[0].VarString^);
end;

function TSatania.SEFileFindAll(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    FindAllFiles(SL, Args[0], Args[1], Boolean(Round(Args[2].VarNumber)), Round(Args[3].VarNumber));
    GC.AllocMap(@Result);
    for I := 0 to SL.Count - 1 do
      SEMapSet(Result, I, SL[I]);
  finally
    SL.Free;
  end;
end;

function TSatania.SEDirectoryCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  CreateDir(Args[0].VarString^);
end;

function TSatania.SEDirectoryDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  DeleteDirectory(Args[0], False);
end;

function TSatania.SEDirectoryFindAll(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    FindAllDirectories(SL, Args[0], Args[1]);
    GC.AllocMap(@Result);
    for I := 0 to SL.Count - 1 do
      SEMapSet(Result, I, SL[I]);
  finally
    SL.Free;
  end;
end;

function TSatania.SEDirectoryExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := DirectoryExists(Args[0].VarString^);
end;

function TSatania.SEJSONParse(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
  procedure QueryForObject(out R: TSEValue; Data: TJSONData); forward;

  procedure QueryForArray(out R: TSEValue; Data: TJSONData);
  var
    I: Integer;
    D: TJSONData;
    Name: String;
    V: TSEValue;
  begin
    GC.AllocMap(@R);
    for I := 0 to Data.Count - 1 do
    begin
      D := Data.Items[I];
      case D.JSONType of
        jtArray:
          begin
            QueryForArray(V, D);
            SEMapSet(R, I, V);
          end;
        jtString:
          begin
            SEMapSet(R, I, D.AsString);
          end;
        jtNumber:
          begin
            SEMapSet(R, I, D.AsFloat);
          end;
        jtBoolean:
          begin
            SEMapSet(R, I, D.AsBoolean);
          end;
        jtNull:
          begin
            SEMapSet(R, I, SENull);
          end;
        jtObject:
          begin
            QueryForObject(V, D);
            SEMapSet(R, I, V);
          end;
      end;
    end;
  end;

  procedure QueryForObject(out R: TSEValue; Data: TJSONData);
  var
    I: Integer;
    D: TJSONData;
    Name: String;
    V: TSEValue;
  begin
    GC.AllocMap(@R);
    for I := 0 to Data.Count - 1 do
    begin
      Name := TJSONObject(Data).Names[I];
      D := Data.FindPath(Name);
      case D.JSONType of
        jtArray:
          begin
            QueryForArray(V, D);
            SEMapSet(R, Name, V);
          end;
        jtString:
          begin
            SEMapSet(R, Name, D.AsString);
          end;
        jtNumber:
          begin
            SEMapSet(R, Name, D.AsFloat);
          end;
        jtBoolean:
          begin
            SEMapSet(R, Name, D.AsBoolean);
          end;
        jtNull:
          begin
            SEMapSet(R, Name, SENull);
          end;
        jtObject:
          begin
            QueryForObject(V, D);
            SEMapSet(R, Name, V);
          end;
      end;
    end;
  end;

var
  JSON: TJSONData;
begin
  JSON := GetJSON(Args[0].VarString^);
  try
    if JSON.JSONType = jtArray then
      QueryForArray(Result, JSON)
    else
      QueryForObject(Result, JSON);
  finally
    JSON.Free;
  end;
end;

function TSatania.SEJSONStringify(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
  procedure DecodeJSONArray(var JSONStr: String; const Map: TSEValue); forward;
  procedure DecodeJSONObject(var JSONStr: String; const Map: TSEValue); forward;

  procedure Decide(var JSONStr: String; const Map: TSEValue);
  begin
    if SEMapIsValidArray(Map) then
      DecodeJSONArray(JSONStr, Map)
    else
      DecodeJSONObject(JSONStr, Map);
  end;

  procedure DecodeJSONArray(var JSONStr: String; const Map: TSEValue);
  var
    I: Integer = 0;
    V: TSEValue;
  begin
    JSONStr := JSONStr + '[';
    for I := 0 to TSEValueMap(Map.VarMap).List.Count - 1 do
    begin
      if (I > 0) then
        JSONStr := JSONStr + ',';
      V := SEMapGet(Map, I);
      case V.Kind of
        sevkString:
          JSONStr := JSONStr + '"' + StringToJSONString(V.VarString^) + '"';
        sevkNumber:
          JSONStr := JSONStr + PointFloatToStr(V.VarNumber);
        sevkMap:
          begin
            Decide(JSONStr, V);
          end;
        sevkNull:
          JSONStr := JSONStr + 'null';
      end;
    end;
    JSONStr := JSONStr + ']';
  end;

  procedure DecodeJSONObject(var JSONStr: String; const Map: TSEValue);
  var
    I: Integer = 0;
    V: TSEValue;
    Key: String;
  begin
    JSONStr := JSONStr + '{';
    for Key in TSEValueMap(Map.VarMap).Keys do
    begin
      if (I > 0) then
        JSONStr := JSONStr + ',';
      JSONStr := JSONStr + '"' + StringReplace(Key, '"', '\"', [rfReplaceAll]) + '":';
      V := SEMapGet(Map, Key);
      case V.Kind of
        sevkString:
          JSONStr := JSONStr + '"' + StringToJSONString(V.VarString^) + '"';
        sevkNumber:
          JSONStr := JSONStr + PointFloatToStr(V.VarNumber);
        sevkMap:
          begin
            Decide(JSONStr, V);
          end;
        sevkNull:
          JSONStr := JSONStr + 'null';
      end;
      Inc(I);
    end;
    JSONStr := JSONStr + '}';
  end;

var
  JSONStr: String = '';
begin
  if Args[0].Kind = sevkMap then
    Decide(JSONStr, Args[0]);
  Result := JSONStr;
end;

function TSatania.SELocalFlagGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := LocalFlagIni.ReadString('Flags', Args[0], '');
end;

function TSatania.SELocalFlagSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  LocalFlagIni.WriteString('Flags', Args[0], Args[1]);
end;

function TSatania.SESketchCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaSketch.CreateSketch(Args[0]);
end;

function TSatania.SESketchDrawTriangles(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Triangles: TSataniaSketchDataArray;
  I, V, C, ColorLen, TexCoordLen, VertexLen: Integer;
  ColorValue: Cardinal;
  Vertices, TexCoords, Colors: TSEValue;
begin
  Vertices := Args[1];
  VertexLen := SESize(Vertices);
  // Do not draw if vertex count <> color count, count = 0, or is not triangles
  if (VertexLen = 0) or ((VertexLen mod 6) <> 0) then
    Exit(0);

  TexCoords := Args[2];
  Colors := Args[3];
  ColorLen := SESize(Colors) - 1;
  TexCoordLen := SESize(TexCoords);

  SetLength(Triangles, VertexLen);
  V := 0;
  C := 0;
  for I := 0 to VertexLen div 2 - 1 do
  begin
    if C > ColorLen then
      ColorValue := Round(SEMapGet(Colors, ColorLen).VarNumber)
    else
      ColorValue := Round(SEMapGet(Colors, C).VarNumber);
    Triangles[I].Vertex := Vector2(
      SEMapGet(Vertices, V).VarNumber,
      SEMapGet(Vertices, V + 1).VarNumber
    );
    if (TexCoordLen > 0) and (TexCoordLen = VertexLen) then
    Triangles[I].TexCoord := Vector2(
      SEMapGet(TexCoords, V).VarNumber,
      SEMapGet(TexCoords, V + 1).VarNumber
    );
    Triangles[I].Color := Vector4(
      Byte(ColorValue) / 255,
      Byte(ColorValue shr 8) / 255,
      Byte(ColorValue shr 16) / 255,
      Byte(ColorValue shr 24) / 255
    );
    Inc(V, 2);
    Inc(C);
  end;
  SataniaSketch.DrawTriangles(Args[0], Triangles);
  Exit(1);
end;

function TSatania.SESketchLoadTexture(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if Args[1] = SENull then
    SataniaSketch.LoadTexture(Args[0], '')
  else
    SataniaSketch.LoadTexture(Args[0], Args[1]);
  Exit(1);
end;

function TSatania.SESketchExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaSketch.Find(Args[0]);
end;

function TSatania.SESketchClear(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaSketch.Delete(Args[0]);
end;

function TSatania.SESketchClearAll(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  SataniaSketch.DeleteAll;
end;

function TSatania.SEWorkerCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Item: TSataniaBackgroundScript;
  Key, ConstKey: String;
begin
  Key := Args[0];
  // We do not allow duplicate of the same worker
  if Self.BackgroundScriptDict.ContainsKey(Key) then
  begin
    Self.BackgroundScriptDict[Key].Script.Free;
    Self.BackgroundScriptDict.Remove(Key);
  end;
  Item.Script := TEvilC.Create;
  Self.RegisterFuncs(Item.Script, True);
  Self.UpdateMeta(Item.Script);
  Item.Script.Source := Args[1];
  if Length(Args) >= 3 then
    Item.Interval := Round(Args[2].VarNumber * 1000)
  else
    Item.Interval := 0;
  Item.LastTimestamp := GetTickCount64;
  if Length(Args) >= 4 then
  begin
    for ConstKey in TSEValueMap(Args[3].VarMap).Keys do
      Item.Script.ConstMap.AddOrSetValue(ConstKey, SEMapGet(Args[3], ConstKey));
  end;
  Self.BackgroundScriptDict.AddOrSetValue(Key, Item);
  Result := Key;
end;

function TSatania.SEWorkerExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Self.BackgroundScriptDict.ContainsKey(Args[0]);
end;

function TSatania.SEWorkerDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if Self.BackgroundScriptDict.ContainsKey(Args[0]) then
  begin
    Self.BackgroundScriptDict[Args[0]].Script.Free;
    Self.BackgroundScriptDict.Remove(Args[0]);
  end;
end;

function TSatania.SEToolEvilCEditor(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  FormEvilCEditor.Show;
  FormEvilCEditor.LoadFromFile(Args[0]);
end;

function TSatania.SEToolHexEditor(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  FormHexEditor.Show;
  FormHexEditor.LoadFromFile(Args[0]);
end;

{$endif}
