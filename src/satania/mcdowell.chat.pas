unit mcdowell.chat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, internetaccess, CastleURIUtils,
  Process, LCLIntf;

type
  TSataniaChatThread = class(TThread)
  protected
    procedure SendChatSendToHer;
  public   
    ChatSend,
    ChatResponse,
    ChatType: String;
    procedure Execute; override;
  end;

  TSataniaExecThread = class(TThread)
  protected
    procedure SendChatSendToHer;
  public     
    ChatSend,
    ChatResponse: String;
    RunName: String;
    procedure Execute; override;
  end;

implementation

uses
  mcdowell, globals, mcdowell.chatbot;

procedure TSataniaChatThread.SendChatSendToHer;
begin
  if ChatType = '' then Exit;
  if ChatResponse = '' then
  begin
    ChatType := 'chat';
    ChatResponse := 'I couldn''t find any process with that name.';
  end;
  Satania.Action(ChatType, ChatResponse);
end;

procedure TSataniaChatThread.Execute;
var
  S, JsonString: String;
  JsonObject: TJSONObject;
  FormData: TStringList;
begin
  ChatResponse := '';
  S := ChatSend;
  if (Length(S) > 0) and (S[1] <> '>') then
  begin
    ChatResponse := Satania.Expression(S);
    if ChatResponse <> '' then
    begin
      ChatType := 'chat';
    end else
    if URIFileExists(PATH_SCRIPTS + Save.Settings.Skin + '/' + S) then
    begin
      ChatType := '';
      Satania.ActionFromFile(S);
    end else
    begin
      if Satania.ChatMode = CHATMODE_CHAT then
      begin
        ChatResponse := Inference(S);
        if ChatResponse = '' then
        begin
          if Save.Settings.BotServer <> '' then
          begin
            FormData := TStringList.Create;
            try
              try
                FormData.Add('message=' + S);
                JsonString := httpRequest(Save.Settings.BotServer, FormData);
                JsonObject := GetJSON(JsonString) as TJSONObject;
                ChatType := JsonObject['type'].AsString;
                ChatResponse := JsonObject['message'].AsString;
                FreeAndNil(JsonObject);
              except
                on E: Exception do
                begin
                  ChatResponse := E.Message;
                  ChatType := 'chat';
                end;
              end;
            finally
              FreeAndNil(FormData);
            end;
          end else
          begin
            if not Save.SpeechToText then
            begin
              ChatType := 'chat';
              ChatResponse := 'Sorry I don''t understand.';
            end else
              ChatType := '';
          end;
        end else
          ChatType := 'script';
      end else
      if Satania.ChatMode = CHATMODE_SEARCH then
      begin
        OpenURL(Format(Save.Settings.SearchEngine, [S]));
        ChatResponse := S;
        ChatType := '';
        Satania.ChatMode := CHATMODE_CHAT;
      end;
    end;
  end else
  if (Length(S) > 0) and ((Save.Settings.BotServer = '') or (S[1] = '>')) then
  begin
    Delete(S, 1, 1);
    ChatType := 'chat';
    RunCommand(S, ChatResponse);
  end;
  Synchronize(@SendChatSendToHer);
  freeThreadVars;
  Terminate;
end;

procedure TSataniaExecThread.SendChatSendToHer;
begin
  // Satania.Action('chat', ChatResponse);
  RunList.Delete(RunList.IndexOf(RunName));
  RunResultList.Add(RunName, ChatResponse);
end;

procedure TSataniaExecThread.Execute;
var
  S: String;
begin
  ChatResponse := '';
  S := ChatSend;
  if (Length(S) > 0) then
  begin
    RunCommand(S, ChatResponse);
  end;
  Synchronize(@SendChatSendToHer);
  Terminate;
end;

end.

