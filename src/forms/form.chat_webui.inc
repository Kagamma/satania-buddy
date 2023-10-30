var
  WebUIHandle: QWord;

procedure WebUI_SkinGet(E: PWebUIEvent);
begin
  webui_return_string(E, PChar(Save.Settings.Skin));
end;

procedure WebUI_ChatHistoryGet(E: PWebUIEvent);
begin
  webui_return_string(E, PChar(FormChat.ChatHistory.ToJSONString(0)));
end;
