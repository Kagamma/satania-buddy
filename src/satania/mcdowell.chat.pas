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

{$I configs.inc}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, CastleURIUtils,
  Process, LCLIntf, StrUtils;

type
  TSataniaChatThread = class(TThread)
  protected
    procedure SendToHer;
    procedure SpeakDontUnderstand;
    procedure ExecuteCustomEvilWorkerScript;
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
  utils.Encdec,
  mcdowell,
  globals,
  mcdowell.chatbot,
  Form.chat,
  Mcdowell.EvilC;

procedure TSataniaChatThread.SendToHer;
begin
  if ChatType = '' then Exit;
  if ChatResponse = '' then
  begin
   // ChatType := 'chat';
   // ChatResponse := 'I couldn''t find any process with that name.';
  end else
  begin
    Satania.Action(ChatType, ChatResponse);
  end;
end;

procedure TSataniaChatThread.SpeakDontUnderstand;
begin
  ChatResponse := 'Sorry I don''t understand.';
  ChatType := 'chat';
end;

procedure TSataniaChatThread.ExecuteCustomEvilWorkerScript;
var
  SL: TStringList;
  S: String;
  V: TSEValue;
  WorkerScriptType: Integer = 0; // Always 0 for now
begin
  if FormChat.ComboBoxService.ItemIndex > 0 then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile('data/scripts/' + Save.Settings.Skin + '/services/' + FormChat.ComboBoxService.Items[FormChat.ComboBoxService.ItemIndex]);
      if WorkerScriptType = 0 then
      begin
        GC.AllocMap(@V);
        S := 'chat_message';
        SEMapSet(V, S, Self.ChatSend);
        Satania.Worker('___worker', SL.Text, 0, V);
      end else
      begin
        Satania.Action('script', 'chat_message = "' + StringToJSONString(Self.ChatSend) + '" ' + SL.Text);
      end;
    finally
      SL.Free;
    end;
  end else
    SpeakDontUnderstand;
end;

procedure TSataniaChatThread.Execute;
var
  S, JsonString: String;
  JsonObject: TJSONObject;
  Client: TFPHTTPClient;

  procedure PerformCustomScriptRequest;
  begin
    Self.Synchronize(@Self.ExecuteCustomEvilWorkerScript);
  end;

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
      // Rules or not
      if Save.Settings.Rules then
        ChatResponse := Inference(S);
      if ChatResponse = '' then
      begin
        PerformCustomScriptRequest;
      end else
        ChatType := 'script';
    end;
  end else
  if (Length(S) > 0) or (S[1] = '>') then
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
  RunProcessResultList.Add(RunName, ChatResponse);
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

