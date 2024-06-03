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

unit Com.WebApp;

{$I configs}

interface

uses
  Classes, SysUtils, Globals,
  fphttpapp, httpdefs, httproute,
  CastleURIUtils;

implementation

uses
  form.chat,
  mcdowell.chat.history,
  Mcdowell;

procedure WebUI_API_DefaultAction(Req: TRequest; Res: TResponse);
var
  FS: TFileStream;
  Path: String = 'data/webui';
begin
  if Req.URI = '/' then
    Path := Path + '/chat/index.html'
  else
    Path := Path + Req.URI;
  Path := Path.Split('?')[0];
  if FileExists(Path) then
  begin
    FS := TFileStream.Create(Path, fmOpenRead);
    Res.ContentType := UriMimeType(Path);
    Res.ContentStream := FS;
    Res.FreeContentStream := True;
  end else
  begin
    Res.Code := 404;
    Res.Content := '404 Not Found.';
  end;
end;

procedure WebUI_API_ChatHistory(Req: TRequest; Res: TResponse);
begin
  Res.Content := FormChat.ChatHistory.ToJSONString(0);
end;

procedure WebUI_API_ChatHistoryPlainText(Req: TRequest; Res: TResponse);
begin
  Res.Content := FormChat.ChatHistory.ToEdit;
end;

procedure WebUI_API_ChatHistoryClear(Req: TRequest; Res: TResponse);
begin
  FormChat.ClearHistory;
end;

procedure WebUI_API_ChatHistorySavePlainText(Req: TRequest; Res: TResponse);
var
  Thread: TWebUIToNativeUIThread;
begin
  Thread := TWebUIToNativeUIThread.Create(@WebUI_ChatServiceSavePlainTextProc, Req.Content);
  Thread.Start;
  WaitForThreadTerminate(Thread.ThreadID, 5000);
end;

procedure WebUI_API_ChatIsStreaming(Req: TRequest; Res: TResponse);
begin
  Res.Content := BoolToStr(FormChat.RichText.IsStreaming, '1', '0');
end;

procedure WebUI_API_ChatSend(Req: TRequest; Res: TResponse);
var
  Thread: TWebUIToNativeUIThread;
begin
  Thread := TWebUIToNativeUIThread.Create(@WebUI_ChatSendProc, Req.Content);
  Thread.Start;
  WaitForThreadTerminate(Thread.ThreadID, 5000);
end;

procedure WebUI_API_ChatStopGenerating(Req: TRequest; Res: TResponse);
var
  Thread: TWebUIToNativeUIThread;
begin
  Thread := TWebUIToNativeUIThread.Create(@WebUI_ChatStopGeneratingProc, Req.Content);
  Thread.Start;
  WaitForThreadTerminate(Thread.ThreadID, 5000);
end;

procedure WebUI_API_ChatServiceGet(Req: TRequest; Res: TResponse);
var
  Thread: TWebUIToNativeUIThread;
begin
  Thread := TWebUIToNativeUIThread.Create(@WebUI_ChatServiceGetProc, Req.Content);
  Thread.Start;
  WaitForThreadTerminate(Thread.ThreadID, 5000);
  Res.Content := ChatServiceGetValue;
end;

procedure WebUI_API_ChatServiceSet(Req: TRequest; Res: TResponse);
var
  Thread: TWebUIToNativeUIThread;
begin
  Thread := TWebUIToNativeUIThread.Create(@WebUI_ChatServiceSetProc, Req.Content);
  Thread.Start;
  WaitForThreadTerminate(Thread.ThreadID, 5000);
end;

procedure WebUI_API_ChatServiceGetList(Req: TRequest; Res: TResponse);
var
  Thread: TWebUIToNativeUIThread;
begin
  Thread := TWebUIToNativeUIThread.Create(@WebUI_ChatServiceGetListProc, Req.Content);
  Thread.Start;
  WaitForThreadTerminate(Thread.ThreadID, 5000);
  Res.Content := ChatServiceGetListValue;
end;

procedure WebUI_API_ChatServiceEdit(Req: TRequest; Res: TResponse);
var
  Thread: TWebUIToNativeUIThread;
begin
  Thread := TWebUIToNativeUIThread.Create(@WebUI_ChatServiceEditProc, '');
  Thread.Start;
  WaitForThreadTerminate(Thread.ThreadID, 5000);
end;

procedure WebUI_API_ChatServiceGetType(Req: TRequest; Res: TResponse);
begin
  Res.Content := WebUI_ChatServiceGetTypeProc;
end;

procedure WebUI_API_ChatServiceGetSetting(Req: TRequest; Res: TResponse);
var
  S: String;
begin
  S := Satania.LocalFlagIni.ReadString('Flags', Save.Settings.LastServiceUsed, '☙❥');
  if S = '☙❥' then
    Res.Content := ''
  else
    Res.Content := S;
end;

procedure WebUI_API_ChatServiceSetSetting(Req: TRequest; Res: TResponse);
begin
  Satania.LocalFlagIni.WriteString('Flags', Save.Settings.LastServiceUsed, Req.Content);
end;

procedure WebUI_API_CharacterSkinGet(Req: TRequest; Res: TResponse);
begin
  Res.Content := Save.Settings.Skin;
end;

procedure WebUI_API_CharacterNameGet(Req: TRequest; Res: TResponse);
begin
  Res.Content := Satania.Name;
end;

initialization
  HTTPRouter.RegisterRoute('/api/v1/chat_history_get', @WebUI_API_ChatHistory);
  HTTPRouter.RegisterRoute('/api/v1/chat_history_plaintext_get', @WebUI_API_ChatHistoryPlainText);
  HTTPRouter.RegisterRoute('/api/v1/chat_history_clear', @WebUI_API_ChatHistoryClear);
  HTTPRouter.RegisterRoute('/api/v1/chat_history_plaintext_save', @WebUI_API_ChatHistorySavePlainText);
  HTTPRouter.RegisterRoute('/api/v1/chat_is_streaming', @WebUI_API_ChatIsStreaming);
  HTTPRouter.RegisterRoute('/api/v1/chat_send', @WebUI_API_ChatSend);
  HTTPRouter.RegisterRoute('/api/v1/chat_stop_generating', @WebUI_API_ChatStopGenerating);
  HTTPRouter.RegisterRoute('/api/v1/chat_service_get', @WebUI_API_ChatServiceGet);
  HTTPRouter.RegisterRoute('/api/v1/chat_service_set', @WebUI_API_ChatServiceSet);
  HTTPRouter.RegisterRoute('/api/v1/chat_service_list_get', @WebUI_API_ChatServiceGetList);
  HTTPRouter.RegisterRoute('/api/v1/chat_service_edit', @WebUI_API_ChatServiceEdit);
  HTTPRouter.RegisterRoute('/api/v1/chat_service_type_get', @WebUI_API_ChatServiceGetType);
  HTTPRouter.RegisterRoute('/api/v1/chat_service_settings_get', @WebUI_API_ChatServiceGetSetting);
  HTTPRouter.RegisterRoute('/api/v1/chat_service_settings_set', @WebUI_API_ChatServiceSetSetting);
  HTTPRouter.RegisterRoute('/api/v1/character_skin_get', @WebUI_API_CharacterSkinGet);
  HTTPRouter.RegisterRoute('/api/v1/character_name_get', @WebUI_API_CharacterNameGet);
  HTTPRouter.RegisterRoute('*', @WebUI_API_DefaultAction);

end.

