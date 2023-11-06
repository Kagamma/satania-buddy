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

unit mcdowell.chat;

{$I configs.inc}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, fphttpclient, CastleURIUtils,
  Process, LCLIntf, StrUtils, FIleUtil, globals;

type
  TSataniaChatThread = class(TThread)
  protected
    procedure RemoveTyping;
    procedure SendToHer;
    procedure SpeakDontUnderstand;
    procedure ExecuteCustomEvilWorkerScript;
  public
    IsShowProcess: Boolean;
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
    ChatResponse : String;
    RunName : String;
    procedure Execute; override;
  end;

  TSataniaExecNonBlockThread = class(TThread)
  protected
    TmpStdOut: String;
    procedure SendToHer;
    procedure SendStdOutToHer;
  public
    IsForcedQuit : Boolean;
    IsShowProcess: Boolean;
    Key     : String;
    RunName : String;
    ExeName : String;
    StdIn   : RawByteString;
    Info    : TNonBlockProcessRec;
    Process : TProcess;
    constructor Create(CreateSuspended: Boolean; const AKey: String);
    procedure Execute; override;
    destructor Destroy; override;
  end;

implementation

uses
  utils.Encdec,
  mcdowell,
  mcdowell.chatbot,
  Form.chat,
  Mcdowell.Data,
  Mcdowell.EvilC;

procedure TSataniaChatThread.RemoveTyping;
begin
  FormChat.RemoveTyping;
end;

procedure TSataniaChatThread.SendToHer;
begin
  if (ChatType = '') or (ChatResponse = '') then
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
      SL.LoadFromFile(GetPhysFilePath('data/scripts/' + Save.Settings.Skin + '/services/' + FormChat.ComboBoxService.Items[FormChat.ComboBoxService.ItemIndex]));
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
  S: String;

  procedure PerformCustomScriptRequest;
  begin
    Self.Synchronize(@Self.ExecuteCustomEvilWorkerScript);
  end;

begin
  ChatResponse := '';
  S := ChatSend;
  if not ((Length(S) > 0) and (S[1] = '>')) then
  begin
    ChatResponse := Satania.Expression(S);
    if ChatResponse <> '' then
    begin
      ChatType := 'chat';
    end else
    // TODO: It's broken
    {if URIFileExists(PATH_SCRIPTS + Save.Settings.Skin + '/' + S) then
    begin
      ChatType := '';
      Satania.ActionFromFile(S);
    end else}
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
    Synchronize(@RemoveTyping);
    RunCommand(S, ChatResponse);
  end;
  Synchronize(@SendToHer);
  Terminate;
end;

procedure TSataniaExecThread.SendToHer;
begin
  // Satania.Action('chat', ChatResponse);
  ThreadDict.Remove(RunName);
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

constructor TSataniaExecNonBlockThread.Create(CreateSuspended: Boolean; const AKey: String);
begin
  inherited Create(FreeOnTerminate);
  Info.IsActive := True;
  Info.Thread := Self;
  Self.Key := AKey;
  RunProcessNonBlockResultList.AddOrSetValue(AKey, Info);
  ThreadDict.AddOrSetValue(AKey, Self);
end;

procedure TSataniaExecNonBlockThread.SendToHer;
begin
  RunProcessNonBlockResultList.AddOrSetValue(Key, Info);
end;

procedure TSataniaExecNonBlockThread.SendStdOutToHer;
begin
  Info.StdOut := Info.StdOut + Self.TmpStdOut;
end;

procedure TSataniaExecNonBlockThread.Execute;
const
  READ_BYTES = 16384;
var
  S: String;
  Commands: TStrings;
  I: Integer;

  procedure ReadFromPipes;
  var
    Buffer: array[0..READ_BYTES - 1] of Byte;
    S: RawByteString;
    BytesRead: Cardinal;
  begin
    BytesRead := Process.Output.Read(Buffer[0], READ_BYTES);
    if BytesRead > 0 then
    begin
      SetString(S, @Buffer[0], BytesRead);
      TmpStdOut := S;
      Synchronize(@Self.SendStdOutToHer);
    end;
  end;

  procedure WriteToPipes;
  var
    Buffer: RawByteString;
  begin
    if StdIn <> '' then
    begin
      Buffer := StdIn;
      StdIn := '';
      Process.Input.Write(Buffer[1], Length(Buffer));
    end;
  end;

begin
  S := RunName;
  Process := TProcess.Create(nil);
  Commands := TStringList.Create;
  Info.Process := Process;
  try
    if (Length(S) > 0) then
    begin
      CommandToList(S, Commands);
      Self.ExeName := Commands[0];
      if IsShowProcess then
        Process.ShowWindow := swoShowDefault
      else
        Process.ShowWindow := swoHIDE;
      Process.Options := Process.Options + [poUsePipes, poStderrToOutPut];
      Process.Executable := FindDefaultExecutablePath(Self.ExeName);
      for I := 1 to Commands.Count - 1 do
      begin
        Process.Parameters.Add(Commands[I]);
      end;
      Process.Execute;
      while Process.Running and not Self.IsForcedQuit do
      begin
        ReadFromPipes;
        WriteToPipes;
        Synchronize(@SendToHer);
        Sleep(100);
      end;
      ReadFromPipes;
      Writeln();
      if Process.Running and Self.IsForcedQuit then
      begin
        Process.Terminate(0);
      end;
    end;
  finally
    Info.IsActive := False;
    Synchronize(@SendToHer);
    Commands.Free;
  end;
  Terminate;
end;

destructor TSataniaExecNonBlockThread.Destroy;
begin
  if Process.Active then
  begin
    Process.Terminate(0);
  end;   
  Process.Free;
  ThreadDict.Remove(Self.Key);
  inherited;
end;

end.

