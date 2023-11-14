const WEBUI_CHATICON =
'<?xml version="1.0" encoding="iso-8859-1"?>'+
'<!-- Uploaded to: SVG Repo, www.svgrepo.com, Generator: SVG Repo Mixer Tools -->'+
'<svg height="800px" width="800px" version="1.1" id="Layer_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"'+
'	 viewBox="0 0 511.999 511.999" xml:space="preserve">'+
'<path style="fill:#CEE8FA;" d="M497.847,428.814V219.932H241.394v208.882h130.583c15.255,30.395,47.302,47.583,79.916,44.968'+
'	c-0.848-1.681-1.664-3.39-2.408-5.152c-5.49-13.027-7.36-26.671-6.073-39.817h54.435V428.814z"/>'+
'<g>'+
'	<path style="fill:#2D527C;" d="M445.372,488.197c-33.645,0-64.185-17.122-81.671-45.229H241.394'+
'		c-7.816,0-14.154-6.337-14.154-14.154V219.932c0-7.817,6.338-14.154,14.154-14.154h256.452c7.816,0,14.154,6.337,14.154,14.154'+
'		v208.882c0,7.817-6.338,14.154-14.154,14.154h-40.373c0.643,6.908,2.342,13.728,5.056,20.168c0.532,1.262,1.168,2.62,2.001,4.273'+
'		c2.123,4.208,2.013,9.197-0.296,13.306c-2.308,4.11-6.511,6.801-11.208,7.177C450.49,488.095,447.916,488.197,445.372,488.197z'+
'		 M255.548,414.661h116.431c5.353,0,10.249,3.02,12.651,7.804c9.449,18.826,26.673,31.836,46.742,35.996'+
'		c-2.352-10.152-3.06-20.659-2.044-31.025c0.709-7.248,6.804-12.775,14.086-12.775h40.28V234.085H255.548V414.661z"/>'+
'	<path style="fill:#2D527C;" d="M54.74,394.847c-15.798,0-31.704-3.371-46.575-10.318c-4.27-1.994-7.268-5.983-7.994-10.639'+
'		c-0.729-4.657,0.91-9.37,4.368-12.571c1.632-1.51,2.926-2.781,4.072-3.997c21.707-23.035,28.248-55.764,17.071-85.413'+
'		c-0.375-0.995-0.633-2.018-0.776-3.052c-8.41-21.065-12.672-43.326-12.672-66.229c0-98.604,80.222-178.826,178.826-178.826'+
'		c67.998,0,129.241,37.758,159.829,98.539c3.513,6.982,0.702,15.491-6.281,19.006c-6.979,3.512-15.493,0.703-19.006-6.281'+
'		c-25.75-51.168-77.304-82.956-134.543-82.956c-82.997,0-150.518,67.523-150.518,150.518c0,20.37,3.997,40.123,11.879,58.709'+
'		c0.585,1.377,0.94,2.819,1.069,4.274c11.51,34.024,5.936,70.652-14.618,99.388c26.448,5.223,54.465-2.838,74.069-22.422'+
'		c4.172-4.17,10.492-5.315,15.865-2.87c15.394,7.003,31.764,11.322,48.657,12.833c7.786,0.696,13.532,7.574,12.835,15.36'+
'		c-0.696,7.786-7.565,13.541-15.36,12.835c-16.932-1.516-33.427-5.404-49.158-11.579C105.632,385.999,80.331,394.847,54.74,394.847z'+
'		"/>'+
'</g>'+
'</svg>';

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

procedure WebUI_ChatHistorySavePlainText(E: PWebUIEvent);
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

procedure WebUI_ShowWebUI;
var
  RootPath,
  AvatarPath: String;
begin
  if WebUIHandle <> 0 then
    webui_close(WebUIHandle);
  WebUIHandle := webui_new_window;
  // Try to set root at user config dir if exists
  RootPath := GetOSLocalDir + 'data/webui';
  if not DirectoryExists(RootPath) then
    RootPath := 'data/webui';
  webui_set_root_folder(WebUIHandle, PChar(RootPath));
  // Prepare avatar
  AvatarPath := GetPhysFilePath('data/sprites/' + Save.Settings.Skin + '/avatar.png');
  if FileExists(RootPath + '/chat/avatar_1.png') then
    DeleteFile(RootPath + '/chat/avatar_1.png');
  if FileExists(AvatarPath) then
  begin
    CopyFile(AvatarPath, RootPath + '/chat/avatar_1.png');
  end;
  //
  webui_set_port(WebUIHandle, Save.Settings.ChatWebUIPort);
  webui_bind(WebUIHandle, 'chat_history_get', @WebUI_ChatHistoryGet);
  webui_bind(WebUIHandle, 'chat_history_plaintext_get', @WebUI_ChatHistoryGetPlainText);
  webui_bind(WebUIHandle, 'chat_history_clear', @WebUI_ChatHistoryClear);
  webui_bind(WebUIHandle, 'chat_history_plaintext_save', @WebUI_ChatHistorySavePlainText);
  webui_bind(WebUIHandle, 'chat_is_streaming', @WebUI_ChatIsStreaming);
  webui_bind(WebUIHandle, 'chat_send', @WebUI_ChatSend);
  webui_bind(WebUIHandle, 'chat_stop_generating', @WebUI_ChatStopGenerating);
  webui_bind(WebUIHandle, 'chat_service_get', @WebUI_ChatServiceGet);
  webui_bind(WebUIHandle, 'chat_service_set', @WebUI_ChatServiceSet);
  webui_bind(WebUIHandle, 'chat_service_list_get', @WebUI_ChatServiceGetList);
  webui_bind(WebUIHandle, 'chat_service_edit', @WebUI_ChatServiceEdit);
  webui_bind(WebUIHandle, 'character_skin_get', @WebUI_CharacterSkinGet);
  webui_bind(WebUIHandle, 'character_name_get', @WebUI_CharacterNameGet);
  webui_set_icon(WebUIHandle, WEBUI_CHATICON, 'image/svg+xml');
  webui_show(WebUIHandle, 'chat/index.html');
end;
