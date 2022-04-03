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

unit mcdowell.chat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, CastleURIUtils,
  Process, LCLIntf;

type
  TSataniaChatThread = class(TThread)
  protected
    procedure SendToHer;
  public   
    ChatSend,
    ChatResponse,
    ChatType: String;
    procedure Execute; override;
  end;

  TSataniaExecThread = class(TThread)
  protected
    procedure SendToHer;
  public     
    ChatSend,
    ChatResponse: String;
    RunName: String;
    procedure Execute; override;
  end;

implementation

uses
  mcdowell, globals, mcdowell.chatbot;

procedure TSataniaChatThread.SendToHer;
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
      ChatResponse := Inference(S);
      if ChatResponse = '' then
      begin
        if Save.Settings.BotServer <> '' then
        begin
          FormData := TStringList.Create;
          try
            try
              FormData.Add('message=' + S);
              JsonString := TFPHTTPClient.SimpleFormPost(Save.Settings.BotServer, FormData);
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
        if Save.Settings.BotVolframAlphaAppID <> '' then
        begin
          try
            JsonString := TFPHTTPClient.SimpleGet('https://api.wolframalpha.com/v1/result?appid=' + Save.Settings.BotVolframAlphaAppID + '&i=' + EncodeURLElement(S));
            ChatType := 'chat';
            ChatResponse := JsonString;
            FreeAndNil(JsonObject);
          except
            on E: Exception do
            begin
              if E.Message.IndexOf('status code: 501') >= 0 then
                ChatResponse := 'Sorry I don''t understand.'
              else
                ChatResponse := E.Message;
              ChatType := 'chat';
            end;
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
    end;
  end else
  if (Length(S) > 0) and ((Save.Settings.BotServer = '') or (S[1] = '>')) then
  begin
    Delete(S, 1, 1);
    ChatType := 'chat';
    RunCommand(S, ChatResponse);
  end;
  Synchronize(@SendToHer);
  Terminate;
end;

procedure TSataniaExecThread.SendToHer;
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
  Synchronize(@SendToHer);
  Terminate;
end;

end.

