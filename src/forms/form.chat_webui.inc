type
  TWebUIToNativeUIProc = procedure(S: String);

  // Use it when we want to update native ui from webui
  TWebUIToNativeUIThread = class(TThread)
  protected
    FProc: TWebUIToNativeUIProc;
    FS: String;
    procedure Update;
  public
    constructor Create(Proc: TWebUIToNativeUIProc; S: String);
    destructor Destroy; override;
    procedure Execute; override;
  end;

var
  WebUIHandle: QWord;
        
procedure TWebUIToNativeUIThread.Update;
begin
  FProc(FS);
end;

constructor TWebUIToNativeUIThread.Create(Proc: TWebUIToNativeUIProc; S: String);
begin
  FS := S;
  FProc := Proc;
  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TWebUIToNativeUIThread.Destroy;
begin
  inherited;
end;

procedure TWebUIToNativeUIThread.Execute;
begin
  Synchronize(@Update);
  Terminate;
end;

// -----

procedure WebUI_CharacterSkinGet(E: PWebUIEvent);
begin
  webui_return_string(E, PChar(Save.Settings.Skin));
end;

procedure WebUI_CharacterNameGet(E: PWebUIEvent);
begin
  webui_return_string(E, PChar(Satania.Name));
end;

procedure WebUI_ChatHistoryGet(E: PWebUIEvent);
begin
  webui_return_string(E, PChar(FormChat.ChatHistory.ToJSONString(0)));
end;

procedure WebUI_ChatHistoryGetPlainText(E: PWebUIEvent);
begin
  webui_return_string(E, PChar(FormChat.ChatHistory.ToEdit));
end;

procedure WebUI_ChatIsStreaming(E: PWebUIEvent);
begin
  webui_return_bool(E, FormChat.RichText.IsStreaming);
end;

procedure WebUI_ChatSendProc(S: String);
begin
  FormChat.EditChat.Lines.Text := S;
  FormChat.Send;
end;
procedure WebUI_ChatSend(E: PWebUIEvent);
var
  Thread: TWebUIToNativeUIThread;
begin
  Thread := TWebUIToNativeUIThread.Create(@WebUI_ChatSendProc, webui_get_string(E));
  Thread.Start;
  WaitForThreadTerminate(Thread.ThreadID, 5000);
end;

procedure WebUI_ChatStopGenerating(E: PWebUIEvent);
begin
  FormChat.StopGenerating;
end;

procedure WebUI_ChatHistoryClear(E: PWebUIEvent);
begin
  FormChat.ClearHistory;
end;

procedure WebUI_ChatHistorySave(E: PWebUIEvent);
begin
  FormChat.SaveHistory(webui_get_string(E));
end;

procedure WebUI_ChatServiceSetProc(S: String);
begin
  FormChat.ComboBoxService.ItemIndex := StrToInt(S);
  FormChat.ComboBoxServiceChange(nil);
end;
procedure WebUI_ChatServiceSet(E: PWebUIEvent);
var
  Thread: TWebUIToNativeUIThread;
begin
  Thread := TWebUIToNativeUIThread.Create(@WebUI_ChatServiceSetProc, webui_get_string(E));
  Thread.Start;
  WaitForThreadTerminate(Thread.ThreadID, 5000);
end;

procedure WebUI_ChatServiceGet(E: PWebUIEvent);
begin
  webui_return_string(E, PChar(IntToStr(FormChat.ComboBoxService.ItemIndex)));
end;

procedure WebUI_ChatServiceGetList(E: PWebUIEvent);
begin
  webui_return_string(E, PChar(FormChat.LoadServiceList));
end;
       
procedure WebUI_ChatServiceEditProc(S: String);
begin
  FormChat.ButtonOpenServiceClick(nil);
end;
procedure WebUI_ChatServiceEdit(E: PWebUIEvent);
var
  Thread: TWebUIToNativeUIThread;
begin
  Thread := TWebUIToNativeUIThread.Create(@WebUI_ChatServiceEditProc, '');
  Thread.Start;
  WaitForThreadTerminate(Thread.ThreadID, 5000);
end;
