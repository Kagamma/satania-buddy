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

unit Globals;

{$I configs.inc}

interface

uses
  {$ifdef LINUX_X11}
  X, Xlib, glx,
  {$endif}
  Classes, SysUtils, Types, Generics.Collections, fpjsonrtti,
  CastleFonts, CastleStringUtils, CastleUnicode, StrUtils,
  CastleVectors, Mcdowell.EvilC, Blowfish, base64;

const
  PATH_SCRIPTS = 'castle-data:/scripts/';
  PATH_SPRITES = 'castle-data:/sprites/';
  PATH_FONT = 'castle-data:/fonts/';         
  PATH_SOUND = 'castle-data:/sounds/';
  PATH_VOSK = 'data/nn/vosk/';
  SECRET_KEY = 'satania_mcdowell';
  SPEECH_RECOGNIZER_BACKEND_VOSK = 0;
  SPEECH_RECOGNIZER_BACKEND_SAPI = 1;
  CHATMODE_SCRIPT = 1;
  CHATMODE_CHAT = 0;

type
  TMailRec = record
    Sender,
    Subject,
    Body: String;
  end;

  TRuleRec = record
    Patterns: TStringDynArray;
    Responses: TStringDynArray;
  end;

  TQWordList = specialize TList<QWord>;
  TStringDict = specialize TDictionary<String, String>;
  TStringArrayDict = specialize TDictionary<String, TStringDynArray>;
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
  end;

  TSaveCollection = class(TCollection)
  public
    Name: String;
    function FindByName(const AName: String): TSaveCollectionItem;
    function FindOrAddByName(const AName: String): TSaveCollectionItem;
  end;

  TSaveSettings = class(TPersistent)
  protected
    FDefaultEvilScheme: String;
    FBotVolframAlphaAppID,
    FBotServer: String;
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
    FCharset,
    FFont: String;
    FFontSize: Integer;
    FEmailPort: Word;
    FEmailUseSSL: Boolean;
    FFrameSkip: Integer;
    FDeveloperMode,
    FLewd: Boolean;
    FSkin: String;
    FSTTVoskModel: String;
    FSTTBackend: Integer;
  published
    property DefaultEvilScheme: String read FDefaultEvilScheme write FDefaultEvilScheme;
    property TextSpeed: Integer read FTextSpeed write FTextSpeed;
    property SitOnWindowRightMargin: Integer read FSitOnWindowRightMargin write FSitOnWindowRightMargin;   
    property BotServer: String read FBotServer write FBotServer;
    property BotVolframAlphaAppID: String read FBotVolframAlphaAppID write FBotVolframAlphaAppID;
    property FPS: Integer read FFPS write FFPS;
    property ChatBubbleDelay: Integer read FChatBubbleDelay write FChatBubbleDelay;
    property BaseScaling: Single read FBaseScaling write FBaseScaling;
    property ImageQuality: String read FImageQuality write FImageQuality;
    property EmailServer: String read FEmailServer write FEmailServer;
    property EmailPort: Word read FEmailPort write FEmailPort;
    property EmailUsername: String read FEmailUsername write FEmailUsername;
    property EmailPassword: String read FEmailPassword write FEmailPassword;
    property EmailFetchFrom: String read FEmailFetchFrom write FEmailFetchFrom;
    property EmailUseSSL: Boolean read FEmailUseSSL write FEmailUseSSL;
    property Charset: String read FCharset write FCharset;
    property Font: String read FFont write FFont;
    property FontSize: Integer read FFontSize write FFontSize;
    property FrameSkip: Integer read FFrameSkip write FFrameSkip;
    property Lewd: Boolean read FLewd write FLewd default False;
    property DeveloperMode: Boolean read FDeveloperMode write FDeveloperMode default False;
    property Skin: String read FSkin write FSkin;
    property STTVoskModel: String read FSTTVoskModel write FSTTVoskModel;
    property STTBackend: Integer read FSTTBackend write FSTTBackend default 0;
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
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String);
    function SEGetFlag(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESetFlag(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
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

  TMethod = procedure of object;
  TCommonThread = class(TThread)
    Method: TMethod;
    procedure Execute; override;
  end;

var
  OwnedWindowHandleList: TQWordList;
  Save: TSave;
  ScreenWidth, ScreenHeight: Integer;
  {$ifdef LINUX_X11}
  XDisplay: PDisplay;
  {$endif}

function PointStrToFloat(S: String): Double; inline;
function PointFloatToStr(X: Double): String; inline;
function UIToScreenCoord(const V: TVector2): TVector2Integer; overload; inline;
function UIToScreenCoord(const V: TVector3): TVector2Integer; overload; inline;

function ScreenCoordToUI(const V: TVector2): TVector2; inline;
procedure CommonThread(Method: TMethod); inline;

function Encrypt(S: String): String; inline;
function Decrypt(S: String): String; inline;
function GUID: String; inline;               
function GUIDName: String; inline;

function CharsetToSettings(S: String): TStringDynArray;
function SettingsToCharset(L: TStringList): String;
function CharsetToCharacters(S: String): String;
function Grep(Src, S: String): String;

implementation

uses
  CastleWindow, Mcdowell;

function PointStrToFloat(S: String): Double;
var
  fS: TFormatSettings;
begin
  FS := FormatSettings;
  fS.DecimalSeparator := '.';
  Result := StrToFloat(S, FS);
end;

function PointFloatToStr(X: Double): String;
var
  FS: TFormatSettings;
begin
  FS := FormatSettings;
  FS.DecimalSeparator := '.';
  Result := FloatToStr(X, FS);
end;

function UIToScreenCoord(const V: TVector2): TVector2Integer;
begin
  Result := Vector2Integer(Round(V.X), Round(Application.ScreenHeight - V.Y));
end;

function UIToScreenCoord(const V: TVector3): TVector2Integer;
begin
  Result := Vector2Integer(Round(V.X), Round(Application.ScreenHeight - V.Y));
end;

function ScreenCoordToUI(const V: TVector2): TVector2;
begin
  Result := Vector2(V.X, Application.ScreenHeight - V.Y);
end;

procedure CommonThread(Method: TMethod);
var
  T: TCommonThread;
begin
  T := TCommonThread.Create(True);
  T.FreeOnTerminate := True;
  T.Method := Method;
  T.Start;
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
  FSettings.ChatBubbleDelay := 5000;
  FSettings.FPS := 16;
  FSettings.TextSpeed := 24;
  FSettings.BaseScaling := 1;
  FSettings.DefaultEvilScheme := 'main.evil';
  FSettings.ImageQuality := 'Linear';
  FSettings.FontSize := 16;
  FSettings.Font := 'lightnovelpop.otf';
  FSettings.FrameSkip := 0;
  FSettings.Lewd := False;
  FSettings.Skin := 'satania';
  FSettings.STTVoskModel := 'english';
  FSettings.STTBackend := SPEECH_RECOGNIZER_BACKEND_VOSK;
  FSettings.FSitOnWindowRightMargin := 86;
end;

destructor TSave.Destroy;
begin
  inherited;
end;

procedure TSave.SaveToFile(const FileName: String);
var
  FS: TFileStream;
  Streamer: TJSONStreamer;
  SS: TStringStream;
begin
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

procedure TSave.LoadFromFile(const FileName: String);
var
  FS: TFileStream;
  DeStreamer: TJSONDeStreamer;
  SS: TStringStream;
begin
  if not FileExists(FileName) then
    Exit;
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

function TSave.SEGetFlag(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Flag: TSaveFlagCollectionItem;
begin
  Flag := TSaveFlagCollectionItem(Self.FFlags.FindByName(Args[0].VarString));
  if Flag <> nil then
    Result.VarString := Flag.Value
  else
    Result.VarString := '';
  Result.Kind := sevkString;
end;

function TSave.SESetFlag(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Flag: TSaveFlagCollectionItem;
begin
  Flag := TSaveFlagCollectionItem(Self.FFlags.FindByName(Args[0].VarString));
  if Flag = nil then
  begin
    Flag := TSaveFlagCollectionItem(Self.FFlags.Add);
    Flag.Name := Args[0].VarString;
  end;
  Flag.Value := Args[1].VarString;
end;

procedure TCommonThread.Execute;
begin
  try
    Method;
    Terminate;
  except
    on E: Exception do
      Satania.Talk(E.Message);
  end;
end;

function Encrypt(S: String): String;
var
  EncrytpStream: TBlowFishEncryptStream;
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  EncrytpStream := TBlowFishEncryptStream.Create(SECRET_KEY, StringStream);
  EncrytpStream.WriteAnsiString(S);
  EncrytpStream.Free;
  Result := EncodeStringBase64(StringStream.DataString);
  StringStream.Free;
end;

function Decrypt(S: String): String;
var
  DecrytpStream: TBlowFishDeCryptStream;
  StringStream: TStringStream;
begin
  S := DecodeStringBase64(S);
  StringStream := TStringStream.Create(S);
  DecrytpStream := TBlowFishDeCryptStream.Create(SECRET_KEY, StringStream);
  Result := DecrytpStream.ReadAnsiString;
  DecrytpStream.Free;
  StringStream.Free;
end;

function GUID: String;
var
  GUIDRec: TGUID;
begin
  CreateGUID(GUIDRec);
  Result := GUIDToString(GUIDRec);
end; 

function GUIDName: String;
begin
  Result := 'G' + StringsReplace(GUID, ['{', '}', '-'], ['', '', ''], [rfReplaceAll]);
end;

function CharsetToSettings(S: String): TStringDynArray;
begin
  Result := SplitString(S, #10#13);
end;

function SettingsToCharset(L: TStringList): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to L.Count - 1 do
  begin
    Result := Result + L[I];
    if I < L.Count - 1 then
      Result := Result + #10#13;
  end;
end;

function CharsetToCharacters(S: String): String;
var
  X, Y, I: Cardinal;
  W: String;
  SS: String = '';
  L, P: TStringDynArray;
begin
  L := CharsetToSettings(S);
  for S in L do
    if Trim(S) <> '' then
    begin
      P := SplitString(S, '..');
      if Length(P) = 3 then
      begin
        X := StrToInt(P[0]);
        Y := StrToInt(P[2]);
        W := '';
        for I := X to Y do
        begin
          W := W + UnicodeToUTF8(I);
        end;
        SS := SS + W;
      end;
    end;
  Result := SS;
end;

function Grep(Src, S: String): String;
var
  A: TStringDynArray;
  V: String;
begin
  Result := '';
  A := SplitString(Src, #10);
  for V in A do
    if V.IndexOf(S) >= 0 then
    begin
      if Result = '' then
        Result := V
      else
        Result := Result + #10 + V;
    end;
end;

initialization
  OwnedWindowHandleList := TQWordList.Create;

finalization
  FreeAndNil(OwnedWindowHandleList);

end.

