unit Mcdowell.Chat.History;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, StrUtils;

type
  TChatSenderEnum = (
    cseSystem,
    cseSatania,
    cseUser
  );

  TChatHistoryRec = record
    SenderType: TChatSenderEnum;
    Time,
    Message: String;
  end;
  TChatHistoryList = specialize TList<TChatHistoryRec>;

  TSataniaChatHistory = class
  private
    ChatHistoryList: TChatHistoryList;
    Path: String;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const HistoryName: String);
    procedure SaveLastestMessage;
    procedure Clear;

    property List: TChatHistoryList read ChatHistoryList;
  end;

implementation

uses
  Globals;

constructor TSataniaChatHistory.Create;
begin
  inherited;
  CreateDir(PATH_CHAT_HISTORY);
  Self.ChatHistoryList := TChatHistoryList.Create;
end;

destructor TSataniaChatHistory.Destroy;
begin
  FreeAndNil(Self.ChatHistoryList);
  inherited;
end;

procedure TSataniaChatHistory.LoadFromFile(const HistoryName: String);
var
  CH: TChatHistoryRec;
  F: TextFile;
begin
  ChatHistoryList.Clear;
  Path := HistoryName;
  if not FileExists(Path) then
  begin
    AssignFile(F, Path);
    Rewrite(F);
  end else
  begin
    AssignFile(F, Path);
    Reset(F);
  end;

  try
    while not EOF(F) do
    begin
      Readln(F, CH.Time);
      Readln(F, Integer(CH.SenderType));
      Readln(F, CH.Message);
      CH.Message := StringReplace(CH.Message, '\n', #10, [rfReplaceAll]);
      Self.ChatHistoryList.Add(CH);
    end;
  except
  end;
  CloseFile(F);
end;

procedure TSataniaChatHistory.SaveLastestMessage;
var
  CH: TChatHistoryRec;
  F: TextFile;
begin
  if ChatHistoryList.Count > 0 then
  begin
    CH := ChatHistoryList[ChatHistoryList.Count - 1];
    if CH.SenderType <> cseSystem then
    begin
      AssignFile(F, Path);
      Append(F);
      Writeln(F, CH.Time);
      Writeln(F, Integer(CH.SenderType));
      Writeln(F, StringsReplace(CH.Message, [#10#13, #10], ['\n', '\n'], [rfReplaceAll]));
      CloseFile(F);
    end;
  end;
end;

procedure TSataniaChatHistory.Clear;
var
  F: TextFile;
begin
  ChatHistoryList.Clear;
  AssignFile(F, Path);
  Rewrite(F);
  CloseFile(F);
end;

end.

