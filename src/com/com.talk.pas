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

type
  TWebUIAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

  THistoryAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

  TChatAction = class(TBrookAction)
  public
    procedure Post; override;
  end;

implementation

uses
  jsontools,
  form.chat,
  Mcdowell;

procedure TWebUIAction.Get;
var
  FS: TFileStream;
  Path: String = 'data/webui';
begin
  if Self.HttpRequest.URI = '/' then
    Path := Path + '/index.html'
  else
    Path := Path + Self.HttpRequest.URI;
  if FileExists(Path) then
  begin
    FS := TFileStream.Create(Path, fmOpenRead);
    try
      Write(FS);
    finally
      FS.Free;
    end;
  end else
  begin
    HttpResponse.SetStatus(404);
    Write('404 Not Found.');
  end;
end;

procedure THistoryAction.Get;
var
  N,
  Json: TJsonNode;
  I: Integer;
  CH: TChatHistory;
begin 
  Json := TJsonNode.Create;
  try
    Json.Value := '[]';
    for I := 0 to FormChat.ChatHistoryList.Count - 1 do
    begin
      CH := FormChat.ChatHistoryList[I];
      N := Json.Add;
      N.Add('message', CH.Message);
      N.Add('time', CH.Time);
      case CH.SenderType of
        cseSatania:
          N.Add('sender', Satania.Name);
        cseUser:
          N.Add('sender', Save.Settings.UserName);
        else
          N.Add('sender', 'System');
      end;
    end;
    Write(Json.AsJson);
  finally
    Json.Free;
  end;
end;

procedure TChatAction.Post;
var
  Name, Value, Typ, Message: String;
  I: Integer;
  HC: Integer;
  Json: TJsonNode;
begin
  Typ := 'chat';
  for I := 0 to Fields.Count - 1 do
  begin
    Fields.GetNameValue(I, Name, Value);
    if Name = 'message' then
      Message := Value
    else
    if Name = 'type' then
      Typ := Value;
  end;
  HC := FormChat.ChatHistoryList.Count;
  if Message <> '' then
    Satania.Action(Typ, Message);
  while HC = FormChat.ChatHistoryList.Count do
    Sleep(100);
  Json := TJsonNode.Create;
  try
    Json.Add('message', FormChat.ChatHistoryList[FormChat.ChatHistoryList.Count - 1].Message);
    Write(Json.AsJson);
  finally
    Json.Free;
  end;
end;

initialization
  TChatAction.Register('/api/v1/chat');
  THistoryAction.Register('/api/v1/chat_history');
  TWebUIAction.Register('*');

end.

