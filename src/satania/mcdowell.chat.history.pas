unit Mcdowell.Chat.History;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

const
  CHAT_HISTORY_VERSION = 1;

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
    ChatHistoryFile: TFileStream;
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
  FreeAndNil(Self.ChatHistoryFile);
  inherited;
end;

procedure TSataniaChatHistory.LoadFromFile(const HistoryName: String);
var
  Path: String;
  CH: TChatHistoryRec;
begin
  ChatHistoryList.Clear;
  if ChatHistoryFile <> nil then
    ChatHistoryFile.Free;
  Path := HistoryName;
  if not FileExists(Path) then
  begin
    ChatHistoryFile := TFileStream.Create(Path, fmCreate or fmShareDenyWrite);
    ChatHistoryFile.WriteDWord(CHAT_HISTORY_VERSION);
  end else
  begin
    ChatHistoryFile := TFileStream.Create(Path, fmOpenReadWrite or fmShareDenyWrite);
  end;

  ChatHistoryFile.Position := 0;
  if ChatHistoryFile.Position < ChatHistoryFile.Size then
    ChatHistoryFile.ReadDWord; // Version
  try
    while ChatHistoryFile.Position < ChatHistoryFile.Size do
    begin
      CH.Time := ChatHistoryFile.ReadAnsiString;
      CH.SenderType := TChatSenderEnum(ChatHistoryFile.ReadDWord);
      CH.Message := ChatHistoryFile.ReadAnsiString;
      Self.ChatHistoryList.Add(CH);
    end;
  except
  end;
end;

procedure TSataniaChatHistory.SaveLastestMessage;
var
  CH: TChatHistoryRec;
begin
  if ChatHistoryList.Count > 0 then
  begin
    CH := ChatHistoryList[ChatHistoryList.Count - 1];
    if CH.SenderType <> cseSystem then
    begin
      ChatHistoryFile.WriteAnsiString(CH.Time);
      ChatHistoryFile.WriteDWord(DWord(CH.SenderType));
      ChatHistoryFile.WriteAnsiString(CH.Message);
    end;
  end;
end;

procedure TSataniaChatHistory.Clear;
begin
  ChatHistoryList.Clear;
  ChatHistoryFile.Size := 0;
  ChatHistoryFile.WriteDWord(CHAT_HISTORY_VERSION);
end;

end.

