unit Globals;

{$I configs.inc}

interface

uses
  {$ifdef LINUX_X11}
  X, Xlib, glx,
  {$endif}
  Classes, SysUtils, Types, Generics.Collections, fpjsonrtti,
  CastleFonts, CastleStringUtils, CastleUnicode, StrUtils,
  CastleVectors, Mcdowell.EvilC, Blowfish;

const
  PATH_SCRIPTS = 'castle-data:/scripts/';
  PATH_SPRITES = 'castle-data:/sprites/';
  PATH_FONT = 'castle-data:/fonts/';
  SECRET_KEY = 'satania_mcdowell';

type 
  TMailRec = record
    Sender,
    Subject,
    Body: String;
  end;

  TQWordList = specialize TList<QWord>;    
  TStringDict = specialize TDictionary<String, String>;
  TStringArrayDict = specialize TDictionary<String, TStringDynArray>;
  TMailList = specialize TList<TMailRec>;

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

  TSaveCollection = class(TCollection)
  public
    Name: String;
    function FindByName(const AName: String): TSaveCollectionItem;
    function FindOrAddByName(const AName: String): TSaveCollectionItem;
  end;

  TSaveSettings = class(TPersistent)
  protected
    FDefaultEvilScheme: String;
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
    FLewd: Boolean;
    FSkin: String;
  published                                                        
    property DefaultEvilScheme: String read FDefaultEvilScheme write FDefaultEvilScheme;
    property TextSpeed: Integer read FTextSpeed write FTextSpeed;
    property SitOnWindowRightMargin: Integer read FSitOnWindowRightMargin write FSitOnWindowRightMargin;
    property BotServer: String read FBotServer write FBotServer;
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
    property Skin: String read FSkin write FSkin;
  end;

  TSave = class(TPersistent)
  protected
    FFlags: TSaveCollection;
    FSitOnWindow: Boolean;
    FSpriteDefaultLocationX: Single;
    FSpriteDefaultLocationY: Single;
    FSettings: TSaveSettings;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(const FileName: String);
    procedure LoadFromFile(const FileName: String); 
    function SEGetFlag(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESetFlag(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
  published     
    property Flags: TSaveCollection read FFlags;
    property Settings: TSaveSettings read FSettings;
    property SitOnWindow: Boolean read FSitOnWindow write FSitOnWindow default false;
    property SpriteDefaultLocationX: Single read FSpriteDefaultLocationX write FSpriteDefaultLocationX;
    property SpriteDefaultLocationY: Single read FSpriteDefaultLocationY write FSpriteDefaultLocationY;
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

function UIToScreenCoord(const V: TVector2): TVector2Integer; overload;
function UIToScreenCoord(const V: TVector3): TVector2Integer; overload;

function ScreenCoordToUI(const V: TVector2): TVector2;
procedure CommonThread(Method: TMethod);

function Encrypt(S: String): String;
function Decrypt(S: String): String;

{$ifdef LINUX_X11}
function FindTopWindow(Window: TWindow): TWindow;
{$endif}

function CharsetToSettings(S: String): TStringDynArray;
function SettingsToCharset(L: TStringList): String;
function CharsetToCharacters(S: String): String;
function Grep(Src, S: String): String;

implementation

uses
  CastleWindow;

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
  {$ifdef WINDOWS}
  FSettings.FSitOnWindowRightMargin := 256;
  {$else}                
  FSettings.FSitOnWindowRightMargin := 192;
  {$endif}
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

{$ifdef LINUX_X11}
function FindTopWindow(Window: TWindow): TWindow;
var
  Root, Parent: TWindow;
  Children: PWindow;
  I: Integer;
begin
  while true do
  begin
    if XQueryTree(XDisplay, Window, @Root, @Parent, @Children, @I) = 0 then
      Exit;
    if (Window = Root) or (Parent = Root) then
      break
    else
      Window := Parent;
  end;
  Result := Window;
end;
{$endif}

procedure TCommonThread.Execute;
begin
  Method;
  Terminate;
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
  Result := StringStream.DataString;
  StringStream.Free;
end;

function Decrypt(S: String): String;
var
  DecrytpStream: TBlowFishDeCryptStream;
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(S);
  DecrytpStream := TBlowFishDeCryptStream.Create(SECRET_KEY, StringStream);
  Result := DecrytpStream.ReadAnsiString;
  DecrytpStream.Free;
  StringStream.Free;
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
  W: WideString;
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
          W := W + WideChar(I);
        end;
        SS := SS + UTF8Encode(W);
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
  Save := TSave.Create;
  if FileExists('configs.json') then
    Save.LoadFromFile('configs.json');

finalization
  Save.SaveToFile('configs.json');
  FreeAndNil(OwnedWindowHandleList);
  FreeAndNil(Save);

end.
