var
  WebUIHandle: QWord;

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

procedure WebUI_ChatSend(E: PWebUIEvent);
begin
  FormChat.EditChat.Lines.Text := webui_get_string(E);
  FormChat.Send;
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

procedure WebUI_ChatServiceSet(E: PWebUIEvent);
begin
  FormChat.ComboBoxService.ItemIndex := StrToInt(webui_get_string(E));
  FormChat.ComboBoxServiceChange(nil);
end;

procedure WebUI_ChatServiceGet(E: PWebUIEvent);
begin
  webui_return_string(E, PChar(IntToStr(FormChat.ComboBoxService.ItemIndex)));
end;

procedure WebUI_ChatServiceGetList(E: PWebUIEvent);
begin
  webui_return_string(E, PChar(FormChat.LoadServiceList));
end;

procedure WebUI_ChatServiceEdit(E: PWebUIEvent);
begin
  FormChat.ButtonOpenServiceClick(nil);
end;