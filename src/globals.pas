{

satania-buddy
Copyright (C) 2022-2024 kagamma

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

unit Globals;

{$I configs.inc}

interface

uses
  {$ifdef LINUX_X11}
  X, Xlib, glx,
  {$endif}
  Classes, SysUtils, Types, Generics.Collections, fpjsonrtti,
  CastleFonts, CastleStringUtils, CastleUnicode,
  CastleVectors, Mcdowell.EvilC, Process;

const
  PATH_SCRIPTS_RAW = 'data/scripts/';
  PATH_SCRIPTS = 's-data:/scripts/';
  PATH_SPRITES = 's-data:/sprites/';
  PATH_FONT = 's-data:/fonts/';
  PATH_SOUND = 's-data:/sounds/';
  PATH_CHAT_HISTORY = 'data/chat_history/';
  PATH_VOSK = 'data/nn/vosk/';                
  PATH_WHISPER = 'data/nn/whisper.cpp/';
  SECRET_KEY = 'satania_mcdowell';
  SPEECH_RECOGNIZER_BACKEND_VOSK = 0;
  SPEECH_RECOGNIZER_BACKEND_WHISPER = 1;
  SPEECH_RECOGNIZER_BACKEND_SAPI = 2;
  CHATMODE_SCRIPT = 1;
  CHATMODE_CHAT = 0;
  SETTINGS_VERSION = 1;

type
  TMailRec = record
    Sender,
    Subject,
    Body: String;
  end;

  TRuleRec = record
    Patterns: TStringDynArray;
    Response: String;
    SimilarityScore: Single;
    Confirmation: Boolean;
    Enable: Boolean;
  end;

  THttpResponseRec = record
    IsBinary: Boolean;
    Status: Integer;
    HeaderKeys: array of String;
    HeaderValues: array of String;
    Data: RawByteString;
  end;

  TTrackRec = record
    Indx: Integer;
    IsLooped: Boolean;
  end;

  TNonBlockProcessRec = record
    IsActive: Boolean;
    StdOut  : RawByteString;
    Thread  : TThread;
    Process : TProcess;
  end;

  TQWordList = specialize TList<QWord>;
  TStringDict = specialize TDictionary<String, String>;
  TNonBlockProcessDict = specialize TDictionary<String, TNonBlockProcessRec>;
  THttpResponseDict = specialize TDictionary<String, THttpResponseRec>;
  TStringArrayDict = specialize TDictionary<String, TStringDynArray>;
  TThreadDict = specialize TDictionary<String, TThread>;
  TTrackDict = specialize TDictionary<String, TTrackRec>;
  TMailList = specialize TList<TMailRec>;
  TRuleDict = specialize TDictionary<String, TRuleRec>;

  TSaveCollectionItem = class(TCollectionItem)
  private
    FName: String;
  published
    property Name: String read FName write FName;
  end;

  TSaveFlagCollectionItem = class(TSaveCollectionItem)
  private
    FValue: String;
  published
    property Value: String read FValue write FValue;
  end;

  TReminderCollectionItem = class(TSaveCollectionItem)
  private
    FKind,
    FYear,
    FMonth,
    FDay,
    FHour,
    FMinute: Integer;
    FScript: String;
    FMonday,
    FTuesday,
    FWednesday,
    FThursday,
    FFriday,
    FSaturday,
    FSunday,
    FEnabled: Boolean;
    FTag: String;
  public
    constructor Create;
  published
    property Kind: Integer read FKind write FKind;
    property Year: Integer read FYear write FYear;
    property Month: Integer read FMonth write FMonth;
    property Day: Integer read FDay write FDay;
    property Hour: Integer read FHour write FHour;
    property Minute: Integer read FMinute write FMinute;
    property Script: String read FScript write FScript;
    property Monday: Boolean read FMonday write FMonday default True;
    property Tuesday: Boolean read FTuesday write FTuesday default True;
    property Wednesday: Boolean read FWednesday write FWednesday default True;
    property Thursday: Boolean read FThursday write FThursday default True;
    property Friday: Boolean read FFriday write FFriday default True;
    property Saturday: Boolean read FSaturday write FSaturday default True;
    property Sunday: Boolean read FSunday write FSunday default True;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Tag: String read FTag write FTag;
  end;

  TSaveCollection = class(TCollection)
  public
    Name: String;
    function FindByName(const AName: String): TSaveCollectionItem;
    function FindOrAddByName(const AName: String): TSaveCollectionItem;
  end;

  TSaveSettings = class(TPersistent)
  protected
    FVersion: Cardinal;
    FExternalServiceSelect: Integer;
    FDefaultEvilScheme: String;
    FChatBubbleDelay: Integer;
    FFPS: Integer;
    FSitOnWindowRightMargin: Integer;
    FTextSpeed: Integer;
    FBaseScaling: Single;
    FImageQuality: String;
    FEmailServer,
    FEmailUsername,
    FEmailPassword,
    FEmailFetchFrom,
    FEmailSmtpServer,
    FEmailSmtpUsername,
    FEmailSmtpPassword: String;
    FFontSize: Integer;
    FEmbeddedServerEnable: Boolean;
    FEmbeddedServerPort,
    FEmailSmtpPort,
    FEmailPort: Word;
    FEmailSmtpUseSSL: Boolean;
    FEmailUseSSL: Boolean;
    FRules: Boolean;
    FFrameSkip: Integer;
    FSystemErrorMessage: Boolean;
    FDeveloperMode,
    FChatSpeechBalloon,
    FEnableItalicForUserText,
    FLewd: Boolean;
    FUserName,
    FLastServiceUsed,
    FSkin: String;
    FSTTVoskModel: String;
    FSTTWhisperModel: String;
    FSTTBackend: Integer;
    FChatWindowFont: String;
    FChatWindowFontSize: Integer;
    FChatBubbleFont: String;
    FChatBubbleFontSize: Integer;
    FChatBubbleSizeX,
    FChatBubbleSizeY: Integer;
    FChatWindowColorBackground,
    FChatWindowColorUserName,
    FChatWindowColorCharacterName,
    FChatWindowColorSystemName,
    FChatWindowColorNormalText,
    FChatWindowColorItalicText,
    FChatWindowColorCodeBlockText: Cardinal;
    FChatWindowClearType: Boolean;       
    FChatBubbleClearType: Boolean;
    FTouchPanelOpenChatWebUI: Boolean;
    FChatWebUIPort: Word;
  published
    property Version: Cardinal read FVersion write FVersion;
    property EmbeddedServerPort: Word read FEmbeddedServerPort write FEmbeddedServerPort default 8666;      
    property ChatWebUIPort: Word read FChatWebUIPort write FChatWebUIPort default 36663;
    property EmbeddedServerEnable: Boolean read FEmbeddedServerEnable write FEmbeddedServerEnable default False;
    property TextSpeed: Integer read FTextSpeed write FTextSpeed;
    property SitOnWindowRightMargin: Integer read FSitOnWindowRightMargin write FSitOnWindowRightMargin;
    property FPS: Integer read FFPS write FFPS;
    property ChatBubbleDelay: Integer read FChatBubbleDelay write FChatBubbleDelay;
    property BaseScaling: Single read FBaseScaling write FBaseScaling; 
    property ImageQuality: String read FImageQuality write FImageQuality;
    property DefaultEvilScheme: String read FDefaultEvilScheme write FDefaultEvilScheme;
    property EmailServer: String read FEmailServer write FEmailServer;
    property ChatSpeechBalloon: Boolean read FChatSpeechBalloon write FChatSpeechBalloon default False;
    property EmailPort: Word read FEmailPort write FEmailPort;
    property EmailUsername: String read FEmailUsername write FEmailUsername;
    property EmailPassword: String read FEmailPassword write FEmailPassword;
    property EmailFetchFrom: String read FEmailFetchFrom write FEmailFetchFrom;
    property EmailUseSSL: Boolean read FEmailUseSSL write FEmailUseSSL;
    property EmailSmtpServer: String read FEmailSmtpServer write FEmailSmtpServer;
    property EmailSmtpPort: Word read FEmailSmtpPort write FEmailSmtpPort;
    property EmailSmtpUsername: String read FEmailSmtpUsername write FEmailSmtpUsername;
    property EmailSmtpPassword: String read FEmailSmtpPassword write FEmailSmtpPassword;
    property EmailSmtpUseSSL: Boolean read FEmailSmtpUseSSL write FEmailSmtpUseSSL;
    property FrameSkip: Integer read FFrameSkip write FFrameSkip;
    property Lewd: Boolean read FLewd write FLewd default False;
    property DeveloperMode: Boolean read FDeveloperMode write FDeveloperMode default False;
    property Skin: String read FSkin write FSkin;
    property STTVoskModel: String read FSTTVoskModel write FSTTVoskModel; 
    property STTWhisperModel: String read FSTTWhisperModel write FSTTWhisperModel;
    property STTBackend: Integer read FSTTBackend write FSTTBackend default 0;
    property UserName: String read FUserName write FUserName;
    property Rules: Boolean read FRules write FRules default True;
    property SystemErrorMessage: Boolean read FSystemErrorMessage write FSystemErrorMessage default False;
    property LastServiceUsed: String read FLastServiceUsed write FLastServiceUsed;
    property EnableItalicForUserText: Boolean read FEnableItalicForUserText write FEnableItalicForUserText default False;
    property ChatWindowFont: String read FChatWindowFont write FChatWindowFont;
    property ChatWindowFontSize: Integer read FChatWindowFontSize write FChatWindowFontSize default 11;
    property ChatBubbleFont: String read FChatBubbleFont write FChatBubbleFont;
    property ChatBubbleFontSize: Integer read FChatBubbleFontSize write FChatBubbleFontSize default 11;
    property ChatBubbleSizeX: Integer read FChatBubbleSizeX write FChatBubbleSizeX default 300;
    property ChatBubbleSizeY: Integer read FChatBubbleSizeY write FChatBubbleSizeY default 200;
    property ChatWindowColorBackground: Cardinal read FChatWindowColorBackground write FChatWindowColorBackground default $FFFFFF;
    property ChatWindowColorUserName: Cardinal read FChatWindowColorUserName write FChatWindowColorUserName default $008000;
    property ChatWindowColorCharacterName: Cardinal read FChatWindowColorCharacterName write FChatWindowColorCharacterName default $0000B0;
    property ChatWindowColorSystemName: Cardinal read FChatWindowColorSystemName write FChatWindowColorSystemName default $800000;
    property ChatWindowColorNormalText: Cardinal read FChatWindowColorNormalText write FChatWindowColorNormalText default $000000;
    property ChatWindowColorItalicText: Cardinal read FChatWindowColorItalicText write FChatWindowColorItalicText default $808080;
    property ChatWindowColorCodeBlockText: Cardinal read FChatWindowColorCodeBlockText write FChatWindowColorCodeBlockText default $071330;
    property ChatWindowClearType: Boolean read FChatWindowClearType write FChatWindowClearType default False;
    property ChatBubbleClearType: Boolean read FChatBubbleClearType write FChatBubbleClearType default False;             
    property TouchPanelOpenChatWebUI: Boolean read FTouchPanelOpenChatWebUI write FTouchPanelOpenChatWebUI default False;
  end;

  TSave = class(TPersistent)
  protected
    FFlags,
    FReminders: TSaveCollection;
    FSitOnWindow: Boolean;
    FSpriteDefaultLocationX: Single;
    FSpriteDefaultLocationY: Single;
    FSettings: TSaveSettings;
    FSilent,
    FSpeechToText: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(FileName: String);
    procedure LoadFromFile(FileName: String);
    function SEGetFlag(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    function SESetFlag(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
  published
    property Flags: TSaveCollection read FFlags;
    property Reminders: TSaveCollection read FReminders;
    property Settings: TSaveSettings read FSettings;
    property SitOnWindow: Boolean read FSitOnWindow write FSitOnWindow default false;
    property SpriteDefaultLocationX: Single read FSpriteDefaultLocationX write FSpriteDefaultLocationX;
    property SpriteDefaultLocationY: Single read FSpriteDefaultLocationY write FSpriteDefaultLocationY;
    property Silent: Boolean read FSilent write FSilent default False;
    property SpeechToText: Boolean read FSpeechToText write FSpeechToText default False;
  end;

procedure DumpExceptionCallStack(E: Exception);

var
  OwnedWindowHandleList: TQWordList;
  Save: TSave;
  {$ifdef LINUX_X11}
  XDisplay: PDisplay;
  {$endif}

implementation

uses  
  Mcdowell.Data,
  Mcdowell;

procedure DumpExceptionCallStack(E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
begin
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);
  Writeln(Report);
end;

constructor TReminderCollectionItem.Create;
begin
  inherited;
  Monday := True;
  Tuesday := True;
  Wednesday := True;
  Thursday := True;
  Friday := True;
  Saturday := True;
  Sunday := True;
  Enabled := True;
end;

function TSaveCollection.FindByName(const AName: String): TSaveCollectionItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.Count - 1 do
    if TSaveCollectionItem(Self.Items[I]).Name = AName then
    begin
      Result := TSaveCollectionItem(Self.Items[I]);
      Exit;
    end;
end;

function TSaveCollection.FindOrAddByName(const AName: String): TSaveCollectionItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Self.Count - 1 do
    if TSaveCollectionItem(Self.Items[I]).Name = AName then
    begin
      Result := TSaveCollectionItem(Self.Items[I]);
      Exit;
    end;
  Result := TSaveCollectionItem(Self.Add);
  Result.Name := AName;
end;

constructor TSave.Create;
begin
  inherited;
  SitOnWindow := False;
  SpriteDefaultLocationX := -1;
  SpriteDefaultLocationY := -1;
  FFlags := TSaveCollection.Create(TSaveFlagCollectionItem);
  FReminders := TSaveCollection.Create(TReminderCollectionItem);
  FSilent := False;
  FSettings := TSaveSettings.Create; 
  FSettings.Version := SETTINGS_VERSION;
  FSettings.ChatBubbleDelay := 5000;
  FSettings.FPS := 16;
  FSettings.TextSpeed := 24;
  FSettings.BaseScaling := 1;
  FSettings.DefaultEvilScheme := 'main.evil';
  FSettings.ImageQuality := 'Nicest';
  FSettings.FrameSkip := 0;
  FSettings.Lewd := False;
  FSettings.Skin := 'satania (bloomers)';
  FSettings.STTWhisperModel := 'ggml-tiny.en-q5_1.bin';
  FSettings.STTVoskModel := 'english';
  FSettings.STTBackend := SPEECH_RECOGNIZER_BACKEND_VOSK;
  FSettings.FSitOnWindowRightMargin := 86;
  FSettings.FEmbeddedServerPort := 8666;
  FSettings.FEmbeddedServerEnable := False;
  FSettings.FRules := True;
  FSettings.FUserName := 'Anon';
  FSettings.FLastServiceUsed := 'WolframAlpha.evil';
  FSettings.FSystemErrorMessage := False;
  FSettings.FChatWindowFont := 'default';
  FSettings.FChatBubbleFont := 'default';
  FSettings.FChatBubbleSizeX := 300;     
  FSettings.FChatBubbleSizeY := 200;
  FSettings.ChatWindowColorBackground := $FFFFFF;
  FSettings.FChatWindowColorUserName := $008000;
  FSettings.FChatWindowColorCharacterName := $0000B0;
  FSettings.FChatWindowColorSystemName := $800000;
  FSettings.FChatWindowColorNormalText := $000000;
  FSettings.FChatWindowColorItalicText := $808080;
  FSettings.FChatWindowColorCodeBlockText := $071330;
  FSettings.FChatWindowFontSize := 11;
  FSettings.FChatBubbleFontSize := 11;
  FSettings.ChatWebUIPort := 36663;
end;

destructor TSave.Destroy;
begin
  inherited;
end;

procedure TSave.SaveToFile(FileName: String);
var
  FS: TFileStream;
  Streamer: TJSONStreamer;
  SS: TStringStream;
begin
  if FileExists(FileName) then
    DeleteFile(FileName);
  FileName := GetOSLocalDir + FileName;
  FS := TFileStream.Create(FileName, fmCreate);
  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.Options := Streamer.Options + [jsoUseFormatString];
    SS := TStringStream.Create(Streamer.ObjectToJSONString(Self));
    try
      SS.Position := 0;
      FS.CopyFrom(SS, SS.Size);
    finally
      FreeAndNil(SS);
    end;
  finally
    FreeAndNil(Streamer);
    FreeAndNil(FS);
  end;
end;

procedure TSave.LoadFromFile(FileName: String);
var
  FS: TFileStream;
  DeStreamer: TJSONDeStreamer;
  SS: TStringStream;
begin
  if not FileExists(FileName) then
  begin
    FileName := GetOSLocalDir + FileName;
    if not FileExists(FileName) then
      Exit;
  end;
  FS := TFileStream.Create(FileName, fmOpenRead);
  SS := TStringStream.Create('');
  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    FS.Position := 0;
    SS.CopyFrom(FS, FS.Size);
    DeStreamer.JSONToObject(SS.DataString, Self);
  finally
    FreeAndNil(DeStreamer);
    FreeAndNil(SS);
    FreeAndNil(FS);
  end;
end;

function TSave.SEGetFlag(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  Flag: TSaveFlagCollectionItem;
begin
  SEValidateType(@Args[0], sevkString, 1, {$I %CURRENTROUTINE%});
  Flag := TSaveFlagCollectionItem(Self.FFlags.FindByName(Args[0].VarString^));
  if Flag <> nil then
    GC.AllocString(@Result, Flag.Value)
  else
    Result := SENull;
end;

function TSave.SESetFlag(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  Flag: TSaveFlagCollectionItem;
begin            
  SEValidateType(@Args[0], sevkString, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkString, 2, {$I %CURRENTROUTINE%});
  Flag := TSaveFlagCollectionItem(Self.FFlags.FindByName(Args[0].VarString^));
  if Flag = nil then
  begin
    Flag := TSaveFlagCollectionItem(Self.FFlags.Add);
    Flag.Name := Args[0].VarString^;
  end;
  Flag.Value := Args[1].VarString^;
end;

initialization
  CreateDir(GetOSLocalDir);
  CreateDir(GetOSLocalDir + 'data');
  CreateDir(GetOSLocalDir + PATH_CHAT_HISTORY);
  OwnedWindowHandleList := TQWordList.Create;

finalization
  if Save <> nil then
    Save.SaveToFile('configs.json');
  FreeAndNil(OwnedWindowHandleList);

end.

