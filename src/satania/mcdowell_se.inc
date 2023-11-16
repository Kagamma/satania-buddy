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

{$ifdef unit_protected}
    function SESpriteTalkSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpriteLoad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpriteScaleGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpriteVisibleSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpriteVisibleGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESpritePositionSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SETalk(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SELog(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEStreamEnable(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEStreamDisable(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEStream(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEStreamPositionGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEAsk(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEAnswer(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SENotify(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEProcessRun(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEProcessPipeGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEProcessPipeSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEProcessTerminate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
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
    function SEChatHistoryGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEURLFetch(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEURLUpload(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEURLIsSuccess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEURLGetProgress(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
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
    function SEFileReadText(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEFileReadBinary(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEFileWriteText(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEFileWriteBinary(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEFileCopy(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEFileExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEFileDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEFileRename(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEFileFindAll(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEFileGetSize(const VM: TSEVM; const Args: array of TSEValue): TSEValue;  
    function SEDirectoryCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEDirectoryDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEDirectoryFindAll(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEDirectoryExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEDirectoryGetConfig(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEJSONParse(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEJSONStringify(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SELocalFlagGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SELocalFlagSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESketchCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESketchLoadFromText(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESketchExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESketchClear(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESketchClearAll(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEWorkerCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEWorkerSetPersistent(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEWorkerExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEWorkerDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEToolEvilCEditor(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEToolHexEditor(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
{$endif}

{$ifdef unit_implementation}

function TSatania.SETalk(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Key: String;
begin
  FormChat.DisableStreaming;
  Talk(SEValueToText(Args[0]));
  VM.IsPaused := True;
  Result := SENull;
end;

function TSatania.SELog(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: String;
begin
  S := SEValueToText(Args[0]);
  Log('System', S);
  Writeln(S);
  Result := SENull;
end;

function TSatania.SEStreamEnable(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  FormChat.EnableStreaming;
  FormBubble.EnableStreaming;
  Result := SENull;
end;

function TSatania.SEStreamDisable(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  FormChat.DisableStreaming;
  FormBubble.DisableStreaming;
  Result := SENull;
end;

function TSatania.SEStream(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  FormChat.Streaming(Args[0].VarString^);
  FormBubble.Streaming(Args[0].VarString^);
  Result := SENull;
end;

function TSatania.SEStreamPositionGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := FormChat.RichText.TokenList[FormChat.RichText.LastTokenPos].Pos;
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
  VM.IsPaused := True;
  Result := SENull;
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
  Result := SENull;
end;

function TSatania.SEProcessRun(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  T: TSataniaExecNonBlockThread;
begin
  T := TSataniaExecNonBlockThread.Create(True, GUID);
  T.Info.IsActive := True;
  T.RunName := Args[0];
  T.IsShowProcess := Args[1];
  Result := T.Key;
end;

function TSatania.SEProcessTerminate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Thread: TSataniaExecNonBlockThread;
begin
  {$ifdef WINDOWS}
  if ThreadDict.ContainsKey(Args[0].VarString^) then
  begin
    Thread := ThreadDict[Args[0].VarString^] as TSataniaExecNonBlockThread;
    ExecuteProcess(FindDefaultExecutablePath('taskkill'), '-f -im ' + ExtractFileName(Thread.ExeName), []);
  end;
  {$else}
  if ThreadDict.ContainsKey(Args[0].VarString^) then
  begin
    Thread := ThreadDict[Args[0].VarString^] as TSataniaExecNonBlockThread;
    Thread.IsForcedQuit := True;
  end;
  {$endif}
end;

function TSatania.SEProcessPipeGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Info: TNonBlockProcessRec;
begin
  Result := SENull;
  try
    if RunProcessNonBlockResultList.ContainsKey(Args[0].VarString^) then
    begin
      Info := RunProcessNonBlockResultList[Args[0].VarString^];
      if not Info.IsActive then
      begin
        RunProcessNonBlockResultList.Remove(Args[0].VarString^);
      end;
      GC.AllocMap(@Result);
      SEMapSet(Result, 'running', Info.IsActive);
      if Info.StdOut <> '' then
        SEMapSet(Result, 'output', Info.StdOut);
      SEMapSet(Result, 'process_id', Info.Process.ProcessID);
      if Info.IsActive then
      begin
        // Flush cache
        TSataniaExecNonBlockThread(Info.Thread).Info.StdOut := '';
        RunProcessNonBlockResultList[Args[0].VarString^] := TSataniaExecNonBlockThread(Info.Thread).Info;
      end;
    end;
  except
  end;
end;

function TSatania.SEProcessPipeSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Info: TNonBlockProcessRec;
begin
  Result := SENull;
  try
    if RunProcessNonBlockResultList.ContainsKey(Args[0].VarString^) then
    begin
      Info := RunProcessNonBlockResultList[Args[0].VarString^];
      TSataniaExecNonBlockThread(Info.Thread).StdIn := TSataniaExecNonBlockThread(Info.Thread).StdIn + Args[1].VarString^;
    end;
  except
  end;
end;

function TSatania.SESpriteLoad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  LoadModel(Args[0]);
  Result := SENull;
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
  Result := SENull;
end;

function TSatania.SESetAnimationSpeed(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  SetAnimationSpeed(Args[0], Args[1].VarNumber);
  Result := SENull;
end;

function TSatania.SEStopAnimation(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  StopAnimation(Args[0]);
  Result := SENull;
end;

function TSatania.SEStopAllAnimations(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  StopAllAnimations;
  Result := SENull;
end;

function TSatania.SESchemeLoad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Satania.ActionFromFile(Args[0]);
  Result := SENull;
end;

function TSatania.SEDelta(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Delta;
  Result := SENull;
end;

function TSatania.SELoadEmails(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  SataniaIMAP.GetMessages;
  Result := SENull;
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
  Result := SENull;
end;

function TSatania.SEOpenURL(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  OpenURL(Args[0]);
  Result := SENull;
end;

function TSatania.SEChatModeSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  ChatMode := Round(Args[0].VarNumber);
  Result := SENull;
end;

function TSatania.SEChatResultGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := ChatResult;
  ChatResult := '';
end;

function TSatania.SEChatHistoryGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
  CH: TChatHistoryRec;
  S: String;
  Item: TSEValue;
begin
  GC.AllocMap(@Result);
  for I := 0 to FormChat.ChatHistory.List.Count - 1 do
  begin
    CH := FormChat.ChatHistory.List[I];
    GC.AllocMap(@Item);
    if CH.SenderType = cseUser then
    begin
      SEMapSet(Item, 'name', Save.Settings.UserName);
    end else
    begin
      SEMapSet(Item, 'name', Satania.Name);
    end;
    SEMapSet(Item, 'message', CH.Message);
    SEMapSet(Item, 'timestamp', CH.Time);
    SEMapSet(Result, I, Item);
  end;
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
  Result := SENull;
end;

function TSatania.SESpriteVisibleSet(const VM: TSEVM  ; const Args: array of TSEValue): TSEValue;
begin
  if Args[0].VarNumber = 0 then
    SetVisible(False)
  else
    SetVisible(True);
  Result := SENull;
end;

function TSatania.SESpriteVisibleGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Sprite.Visible;
end;

function TSatania.SESpritePositionSet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  SetPosition(Round(Args[0].VarNumber), Round(Args[1].VarNumber));
  Result := SENull;
end;

function TSatania.SEURLFetch(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Thread: TSataniaHttpThread;
  I: Integer;
  S: String;
begin
  Thread := TSataniaHttpThread.Create(True, GUID);
  Thread.FreeOnTerminate := True;
  Thread.URL := Args[1];
  Thread.Method := UpperCase(Args[0].VarString^);
  if Args[2].Kind = sevkMap then
    for S in TSEValueMap(Args[2].VarMap).Keys do
    begin
      Thread.HTTP.AddHeader(S, SEMapGet(Args[2], S));
    end;
  case Args[3].Kind of
    sevkMap:
      Thread.FormData.Text := SEJSONStringify(VM, [Args[3]]);
    sevkString:
      Thread.FormData.Text := Args[3];
  end;
  Thread.Start;
  Result := Thread.Key;
end;

function TSatania.SEURLUpload(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Thread: TSataniaHttpThread;
begin
  Thread := TSataniaHttpThread.Create(True, GUID);
  Thread.FreeOnTerminate := True;
  Thread.URL := Args[0];
  Thread.Method := 'POST';
  case Args[2].Kind of
    sevkMap:
      Thread.FormData.Text := SEJSONStringify(VM, [Args[2]]);
    sevkString:
      Thread.FormData.Text := Args[2];
  end;
  Thread.FieldName := Args[3];
  Thread.FileName := Args[4];
  Thread.Start;
  Result := Thread.Key;
end;

function TSatania.SEURLIsSuccess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if ThreadDict.ContainsKey(Args[0]) then
    Result := False
  else
    Result := True;
end;

function TSatania.SEURLGetProgress(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Thread: TSataniaHttpThread;
begin
  GC.AllocMap(@Result);
  if ThreadDict.ContainsKey(Args[0]) then
  begin
    Thread := ThreadDict[Args[0]] as TSataniaHttpThread;
    SEMapSet(Result, 'position', Thread.CurrentPos);
    SEMapSet(Result, 'length', Thread.ContentLength);
  end;
end;

function TSatania.SEURLGetResult(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  HttpResponse: THttpResponseRec;
  Buf, Headers: TSEValue;
  Len: QWord;
  I: Integer;
begin
  Result := SENull;
  try
    GC.AllocMap(@Result);
    GC.AllocMap(@Headers);
    HttpResponse := RunHttpResultList[Args[0].VarString^];
    SEMapSet(Result, 'status', HttpResponse.Status);
    if (HttpResponse.IsBinary) or ((Length(Args) > 1) and (Args[1] = True)) then
    begin
      Len := Length(HttpResponse.Data);
      GC.AllocBuffer(@Buf, Len);
      Move(HttpResponse.Data[1], Buf.VarBuffer^.Base[1], Len);
      SEMapSet(Result, 'data', Buf);
    end else
      SEMapSet(Result, 'data', HttpResponse.Data);
    for I := 0 to Length(HttpResponse.HeaderKeys) - 1 do
    begin
      SEMapSet(Headers, HttpResponse.HeaderKeys[I], HttpResponse.HeaderValues[I]);
    end;
    SEMapSet(Result, 'headers', Headers);
    RunHttpResultList.Remove(Args[0].VarString^);
  except
    on E: Exception do
      Writeln(E.Message);
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
  // https://wiki.freepascal.org/URL_encoding/decoding
  function EncodeUrl(const url: string): string;
  var
    x: integer;
    sBuff: string;
  const
    SafeMask = ['A'..'Z', '0'..'9', 'a'..'z', '*', '@', '.', '_', '-'];
  begin
    //Init
    sBuff := '';

    for x := 1 to Length(url) do
    begin
      //Check if we have a safe char
      if url[x] in SafeMask then
      begin
        //Append all other chars
        sBuff := sBuff + url[x];
      end
      else if url[x] = ' ' then
      begin
        //Append space
        sBuff := sBuff + '+';
      end
      else
      begin
        //Convert to hex
        sBuff := sBuff + '%' + IntToHex(Ord(url[x]), 2);
      end;
    end;

    Result := sBuff;
  end;
begin
  Result := EncodeUrl(Args[0]);
end;

function TSatania.SEURLDecode(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
  // https://wiki.freepascal.org/URL_encoding/decoding
  function DecodeUrl(const url: string): string;
  var
    x: integer;
    ch: string;
    sVal: string;
    Buff: string;
  begin
    //Init
    Buff := '';
    x := 1;
    while x <= Length(url) do
    begin
      //Get single char
      ch := url[x];

      if ch = '+' then
      begin
        //Append space
        Buff := Buff + ' ';
      end
      else if ch <> '%' then
      begin
        //Append other chars
        Buff := Buff + ch;
      end
      else
      begin
        //Get value
        sVal := Copy(url, x + 1, 2);
        //Convert sval to int then to char
        Buff := Buff + char(StrToInt('$' + sVal));
        //Inc counter by 2
        Inc(x, 2);
      end;
      //Inc counter
      Inc(x);
    end;
    //Return result
    Result := Buff;
  end;
begin
  Result := DecodeUrl(Args[0]);
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
  Result := SENull;
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

function TSatania.SEFileReadText(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
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

function TSatania.SEFileReadBinary(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(Args[0], fmOpenRead);
  try
    GC.AllocBuffer(@Result, FS.Size);
    FS.Read(Result.VarBuffer^.Ptr^, FS.Size);
  finally
    FS.Free;
  end;
end;

function TSatania.SEFileWriteText(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  FS: TFileStream;
begin
  if FileExists(Args[0].VarString^) then
    FS := TFileStream.Create(Args[0], fmOpenWrite)
  else
    FS := TFileStream.Create(Args[0], fmCreate);
  try
    FS.Position := FS.Size;
    FS.Write(Args[1].VarString^[1], Length(Args[1].VarString^));
  finally
    FS.Free;
  end;
end;

function TSatania.SEFileWriteBinary(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  FS: TFileStream;
begin
  if FileExists(Args[0].VarString^) then
    FS := TFileStream.Create(Args[0], fmOpenWrite)
  else
    FS := TFileStream.Create(Args[0], fmCreate);
  try
    FS.Position := FS.Size;
    FS.Write(Args[1].VarBuffer^.Ptr^, Args[2]);
  finally
    FS.Free;
  end;
end;

function TSatania.SEFileCopy(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := False;
  if FileExists(Args[0].VarString^) then
  begin
    Result := CopyFile(Args[0].VarString^, Args[1], [cffOverwriteFile], False);
  end;
end;

function TSatania.SEFileExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := FileExists(Args[0].VarString^);
end;

function TSatania.SEFileDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  DeleteFile(Args[0].VarString^);
  Result := SENull;
end;

function TSatania.SEFileRename(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  RenameFile(Args[0].VarString^, Args[1].VarString^);
  Result := SENull;
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

function TSatania.SEFileGetSize(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  F: File of Byte;
begin
  Result := 0;
  if FileExists(Args[0].VarString^) then
  begin
    AssignFile(F, Args[0].VarString^);
    Reset(F);
    Result := FileSize(F);
    CloseFile(F);
  end;
end;

function TSatania.SEDirectoryCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  ForceDirectories(Args[0].VarString^);
  Result := SENull;
end;

function TSatania.SEDirectoryDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  DeleteDirectory(Args[0], False);
  Result := SENull;
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

function TSatania.SEDirectoryGetConfig(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := GetOSLocalDir;
end;

function TSatania.SEJSONParse(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
  procedure QueryForObject(out R: TSEValue; Data: TJsonNode); forward;

  procedure QueryForArray(out R: TSEValue; Data: TJsonNode);
  var
    I: Integer;
    D: TJsonNode;
    Name: String;
    V: TSEValue;
  begin
    GC.AllocMap(@R);
    for I := 0 to Data.Count - 1 do
    begin
      D := Data.Child(I);
      case D.Kind of
        nkArray:
          begin
            QueryForArray(V, D);
            SEMapSet(R, I, V);
          end;
        nkString:
          begin
            SEMapSet(R, I, D.AsString);
          end;
        nkNumber:
          begin
            SEMapSet(R, I, D.AsNumber);
          end;
        nkBool:
          begin
            SEMapSet(R, I, D.AsBoolean);
          end;
        nkNull:
          begin
            SEMapSet(R, I, SENull);
          end;
        nkObject:
          begin
            QueryForObject(V, D);
            SEMapSet(R, I, V);
          end;
      end;
    end;
  end;

  procedure QueryForObject(out R: TSEValue; Data: TJsonNode);
  var
    I: Integer;
    D: TJsonNode;
    V: TSEValue;
  begin
    GC.AllocMap(@R);
    for I := 0 to Data.Count - 1 do
    begin
      D := Data.Child(I);
      case D.Kind of
        nkArray:
          begin
            QueryForArray(V, D);
            SEMapSet(R, D.Name, V);
          end;
        nkString:
          begin
            SEMapSet(R, D.Name, D.AsString);
          end;
        nkNumber:
          begin
            SEMapSet(R, D.Name, D.AsNumber);
          end;
        nkBool:
          begin
            SEMapSet(R, D.Name, D.AsBoolean);
          end;
        nkNull:
          begin
            SEMapSet(R, D.Name, SENull);
          end;
        nkObject:
          begin
            QueryForObject(V, D);
            SEMapSet(R, D.Name, V);
          end;
      end;
    end;
  end;

var
  Json: TJsonNode;
  ErrorStr: String = '';
begin
  Result := SENull;
  Json := TJsonNode.Create;
  try
    try
      Json.Parse(Args[0].VarString^);
      if Json.Kind = nkArray then
        QueryForArray(Result, Json)
      else
        QueryForObject(Result, Json);
    except
      on E: Exception do
      begin
        ErrorStr := E.Message;
      end;
    end;
  finally
    Json.Free;
    if ErrorStr <> '' then
      raise Exception.Create(ErrorStr);
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
        sevkBoolean:
          JSONStr := JSONStr + BoolToStr(Boolean(Round(V.VarNumber)), 'true', 'false');
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
      JSONStr := JSONStr + '"' + StringToJSONString(Key) + '":';
      V := SEMapGet(Map, Key);
      case V.Kind of
        sevkString:
          JSONStr := JSONStr + '"' + StringToJSONString(V.VarString^) + '"';
        sevkNumber:
          JSONStr := JSONStr + PointFloatToStr(V.VarNumber);
        sevkBoolean:
          JSONStr := JSONStr + BoolToStr(Boolean(Round(V.VarNumber)), 'true', 'false');
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
  Result := SENull;
end;

function TSatania.SESketchCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaSketch.CreateSketch(Args[0]);
end;

function TSatania.SESketchLoadFromText(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaSketch.LoadFromText(Args[0], Args[1], Args[2]);
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
  Result := SENull;
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
  Item.IsPersistent := False;
  Item.Script := Self.CreateEvilC(True);
  Item.Script.IncludePathList.Add('data/scripts/' + Save.Settings.Skin + '/');
  Item.Script.IncludePathList.Add(GetOSLocalDir + 'data/scripts/' + Save.Settings.Skin + '/');
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

function TSatania.SEWorkerSetPersistent(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Item: TSataniaBackgroundScript;
begin
  Item := Self.BackgroundScriptDict[Args[0]];
  Item.IsPersistent := Args[1];
  Self.BackgroundScriptDict[Args[0]] := Item;
end;

function TSatania.SEWorkerExists(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Self.BackgroundScriptDict.ContainsKey(Args[0]);
end;

function TSatania.SEWorkerDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Self.WorkerDelete(Args[0]);
  Result := SENull;
end;

function TSatania.SEToolEvilCEditor(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Frm: TFormEvilCEditor;
begin
  Frm := TFormEvilCEditor.Create(nil);
  DockMaster.MakeDockable(Frm);
  Frm.LoadFromFile(Args[0]);
  Result := SENull;
end;

function TSatania.SEToolHexEditor(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  FormHexEditor.Show;
  FormHexEditor.LoadFromFile(Args[0]);
  Result := SENull;
end;

{$endif}
