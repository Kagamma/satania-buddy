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

unit Com.Talk;

{$I configs}

interface

uses
  Classes, SysUtils, Globals,
  BrookAction;

implementation

uses
  jsontools,
  form.chat,
  mcdowell.chat.history,
  Mcdowell;

type
  TWebUIDefaultAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

  TWebUI_ChatHistory = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_ChatHistoryPlainText = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_ChatHistoryClear = class(TBrookAction)
  public
    procedure Post; override;
  end;            

  TWebUI_ChatHistorySavePlainText = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_ChatIsStreaming = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_ChatSend = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_ChatStopGenerating = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_ChatServiceGet = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_ChatServiceSet = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_ChatServiceGetList = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_ChatServiceEdit = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_ChatServiceGetType = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_ChatServiceGetSetting = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_ChatServiceSetSetting = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_CharacterSkinGet = class(TBrookAction)
  public
    procedure Post; override;
  end;

  TWebUI_CharacterNameGet = class(TBrookAction)
  public
    procedure Post; override;
  end;

procedure TWebUIDefaultAction.Get;
var
  FS: TFileStream;
  Path: String = 'data/webui';
begin
  if Self.HttpRequest.URI = '/' then
    Path := Path + '/chat/index.html'
  else
    Path := Path + Self.HttpRequest.URI;
  Path := Path.Split('?')[0];
  if FileExists(Path) then
  begin
    FS := TFileStream.Create(Path, fmOpenRead);
    case LowerCase(ExtractFileExt(Path)) of
      '.png':
        HttpResponse.ContentType := 'image/png';
      '.woff':
        HttpResponse.ContentType := 'application/x-font-woff';
      '.css':
        HttpResponse.ContentType := 'text/css';
    end;
    Self.HttpResponse.ContentStream := FS;
    Self.HttpResponse.FreeContentStream := True;
  end else
  begin
    HttpResponse.SetStatus(404);
    Write('404 Not Found.');
  end;
end;

procedure TWebUI_ChatHistory.Post;
begin
  Write(FormChat.ChatHistory.ToJSONString(0));
end;  

procedure TWebUI_ChatHistoryPlainText.Post;
begin
  Write(FormChat.ChatHistory.ToEdit);
end;

procedure TWebUI_ChatHistoryClear.Post;
begin
  FormChat.ClearHistory;
end;          

procedure TWebUI_ChatHistorySavePlainText.Post;
begin
  FormChat.SaveHistory(Self.HttpRequest.Content);
end;

procedure TWebUI_ChatIsStreaming.Post;
begin
  Write(BoolToStr(FormChat.RichText.IsStreaming, '1', '0'));
end;          

procedure TWebUI_ChatSend.Post;
var
  Thread: TWebUIToNativeUIThread;
begin
  Thread := TWebUIToNativeUIThread.Create(@WebUI_ChatSendProc, Self.HttpRequest.Content);
  Thread.Start;
  WaitForThreadTerminate(Thread.ThreadID, 5000);
end;

procedure TWebUI_ChatStopGenerating.Post;
begin
  FormChat.StopGenerating;
end;  

procedure TWebUI_ChatServiceGet.Post;
begin
  Write(IntToStr(FormChat.ComboBoxService.ItemIndex));
end;

procedure TWebUI_ChatServiceSet.Post;
var
  Thread: TWebUIToNativeUIThread;
begin
  Thread := TWebUIToNativeUIThread.Create(@WebUI_ChatServiceSetProc, Self.HttpRequest.Content);
  Thread.Start;
  WaitForThreadTerminate(Thread.ThreadID, 5000);
end;

procedure TWebUI_ChatServiceGetList.Post;
begin
  Write(FormChat.LoadServiceList);
end;

procedure TWebUI_ChatServiceEdit.Post;
var
  Thread: TWebUIToNativeUIThread;
begin
  Thread := TWebUIToNativeUIThread.Create(@WebUI_ChatServiceEditProc, '');
  Thread.Start;
  WaitForThreadTerminate(Thread.ThreadID, 5000);
end; 

procedure TWebUI_ChatServiceGetType.Post;
begin
  Write(WebUI_ChatServiceGetTypeProc);
end;      

procedure TWebUI_ChatServiceGetSetting.Post;
var
  S: String;
begin
  S := Satania.LocalFlagIni.ReadString('Flags', Save.Settings.LastServiceUsed, '☙❥');
  if S = '☙❥' then
    Write('')
  else
    Write(S);
end;

procedure TWebUI_ChatServiceSetSetting.Post;
begin
  Satania.LocalFlagIni.WriteString('Flags', Save.Settings.LastServiceUsed, Self.HttpRequest.Content);
end;      

procedure TWebUI_CharacterSkinGet.Post;
begin
  Write(Save.Settings.Skin);
end;

procedure TWebUI_CharacterNameGet.Post;
begin
  Write(Satania.Name);
end;

initialization
  TWebUI_ChatHistory.Register('/api/v1/chat_history_get');
  TWebUI_ChatHistoryPlainText.Register('/api/v1/chat_history_plaintext_get');
  TWebUI_ChatHistoryClear.Register('/api/v1/chat_history_clear');
  TWebUI_ChatHistorySavePlainText.Register('/api/v1/chat_history_plaintext_save');
  TWebUI_ChatIsStreaming.Register('/api/v1/chat_is_streaming');
  TWebUI_ChatSend.Register('/api/v1/chat_send');
  TWebUI_ChatStopGenerating.Register('/api/v1/chat_stop_generating');
  TWebUI_ChatServiceGet.Register('/api/v1/chat_service_get');
  TWebUI_ChatServiceSet.Register('/api/v1/chat_service_set');
  TWebUI_ChatServiceGetList.Register('/api/v1/chat_service_list_get');
  TWebUI_ChatServiceEdit.Register('/api/v1/chat_service_edit');
  TWebUI_ChatServiceGetType.Register('/api/v1/chat_service_type_get');
  TWebUI_ChatServiceGetSetting.Register('/api/v1/chat_service_settings_get');
  TWebUI_ChatServiceSetSetting.Register('/api/v1/chat_service_settings_set');
  TWebUI_CharacterSkinGet.Register('/api/v1/character_skin_get');
  TWebUI_CharacterNameGet.Register('/api/v1/character_name_get');
  TWebUIDefaultAction.Register('*');

end.

