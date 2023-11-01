unit Mcdowell.Chat.History;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, StrUtils, LazUTF8;

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
    GUID: String;
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
    function ToEdit: String;       
    function ToJSONString(const Start: Integer): String;
    procedure FromEdit(Source: String);

    property List: TChatHistoryList read ChatHistoryList;
  end;

implementation

uses
  Globals, Mcdowell, Mcdowell.Data, fpjson, Utils.Encdec;

constructor TSataniaChatHistory.Create;
begin
  inherited;
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
      CH.GUID := GUID;
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

function TSataniaChatHistory.ToEdit: String;
var
  CH: TChatHistoryRec;
begin
  Result := '';
  for CH in ChatHistoryList do
  begin
    case CH.SenderType of
      cseSatania:
        Result := Result + #10 + Satania.Name + ': ';
      cseUser:
        Result := Result + #10 + Save.Settings.UserName + ': ';
      else
        Continue;
    end;
    Result := Result + CH.Message;
  end;
  Result := Trim(Result);
end;

function TSataniaChatHistory.ToJSONString(const Start: Integer): String;
var
  CH: TChatHistoryRec;
  I: Integer;
begin
  if ChatHistoryList.Count = 0 then
    Exit('[]');
  Result := '[';
  for I := Start to ChatHistoryList.Count - 1 do
  begin
    CH := ChatHistoryList[I];
    case CH.SenderType of
      cseSatania:
        Result := Result + '{"name":"' + StringToJSONString(Satania.Name) + '",';
      cseUser:
        Result := Result + '{"name":"' + StringToJSONString(Save.Settings.UserName) + '",';
      cseSystem:
        Result := Result + '{"name":"System",';
      else
        begin
          Continue;
        end;
    end;                                                       
    Result := Result + '"guid":"' + CH.GUID + '",';
    Result := Result + '"kind":' + IntToStr(Integer(CH.SenderType)) + ',';
    Result := Result + '"message":"' + StringToJSONString(CH.Message) + '"';
    Result := Result + '},';
  end;
  if Result = '[' then
    Result := '[]'
  else
    Result[Length(Result)] := ']';
end;

procedure TSataniaChatHistory.FromEdit(Source: String);
var
  CH    : TChatHistoryRec;
  C     : String;
  Buffer: String;
  SataniaToken,
  UserToken: String;
  I     : Integer;
begin
  Buffer := '';
  Source := Trim(Source);
  SataniaToken := Satania.Name + ': ';
  UserToken    := Save.Settings.UserName + ': ';
  Clear;
  CH.Message := '';
  CH.Time := '00:00:00';
  for I := 1 to UTF8Length(Source) do
  begin
    C := UTF8Copy(Source, I, 1);
    if C = #10 then
    begin
      CH.Message := CH.Message + Buffer + #10;
      Buffer := '';
    end else
      Buffer := Buffer + C;
    if (Buffer = SataniaToken) or (Buffer = UserToken) then
    begin
      CH.Message := Trim(CH.Message);
      if CH.Message <> '' then
      begin
        CH.GUID := GUID;
        ChatHistoryList.Add(CH);
        SaveLastestMessage;
        CH.Message := '';
      end;
      if Buffer = UserToken then
        CH.SenderType := cseUser
      else
        CH.SenderType := cseSatania; 
      Buffer := '';
    end;
  end;
  if (Buffer <> '') or (CH.Message <> '') then
  begin
    CH.Message := CH.Message + Buffer;
    CH.Message := Trim(CH.Message);
    if CH.Message <> '' then
    begin              
      CH.GUID := GUID;
      ChatHistoryList.Add(CH);
      SaveLastestMessage;
    end;
  end;
end;

end.

