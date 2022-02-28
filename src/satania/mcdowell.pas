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

unit Mcdowell;

{$I configs.inc}

interface

uses
  Classes, SysUtils,
  Forms, Menus, FileUtil,
  fphttpclient, fpjson, jsonparser, Process,
  CastleScene, CastleControls, CastleUIControls, CastleTypingLabel, CastleDownload,
  CastleVectors, X3DNodes, CastleBoxes, CastleFilesUtils, CastleURIUtils,
  CastleTransform, CastleRenderOptions, CastleViewport, CastleFonts,
  Mcdowell.EvilC, Globals;

type
  TSataniaChatThread = class(TThread)
  protected
    ChatSend,
    ChatResponse,
    ChatType: String;
    procedure SendChatSendToHer;
  public
    procedure Execute; override;
  end;

  TSataniaExecThread = class(TThread)
  protected
    ChatSend,
    ChatResponse: String;
    procedure SendChatSendToHer;
  public
    RunName: String;
    procedure Execute; override;
  end;

  TSatania = class
  protected
    function SELoadModel(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESetScale(const VM: TSEVM; const Args: array of TSEValue): TSEValue;   
    function SESetVisible(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetVisible(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SETalk(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SERun(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsRunning(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetRunResult(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsAnimationPlaying(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEStopAnimation(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEStopAllAnimations(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEStartAnimation(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SESetAnimationSpeed(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsSoW(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsLewd(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsSilent(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsSpeechToText(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SELoadScheme(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEDelta(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SELoadEmails(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetUnseenEmailCount(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetEmailSender(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetEmailSubject(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEGetEmailBody(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsEmailLoading(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsEmailSuccess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEIsEmailConfigured(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    function SEDefaultScheme(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
  public
    Delta: Single;
    MenuItems: array of TMenuItem;
    IsAction: Boolean;
    IsTalking: Boolean;
    IsBlocked: Boolean;
    Sprite: TCastleScene;
    Viewport: TCastleViewport;
    LocalBoundingBoxSnapshot: TBox3D;
    ChatText: TCastleTypingLabel;
    ChatBubble: TCastleUserInterface;
    FontSystem: TCastleFont;
    ChatBubbleDelay: Integer;
    Form,
    FormTouch: TForm;
    Name: String;
    Script: TEvilC;
    { Where we should move our touch panel to }
    TouchBone: TTransformNode;
    constructor Create;
    destructor Destroy; override;
    procedure DefaultPosition;
    procedure LoadModel(S: String);
    procedure SetAnimationSpeed(AnimName: String; Speed: Single);
    procedure StartAnimation(AnimName: String; IsRepeat: Boolean = True); overload;
    procedure StartAnimation(URL, AnimName: String; IsRepeat: Boolean = True); overload;
    procedure StopAnimation(AnimName: String);
    procedure StopAllAnimations;
    procedure Log(LogName, S: String);
    procedure Talk(S: String);
    procedure Notify(S: String);
    procedure TalkWithoutBlock(S: String);
    function Exec(S: String): String;
    procedure Chat(S: String);
    procedure Action(Typ, Message: String);
    procedure ActionFromFile(FileName: String);
    procedure SetScale(Scale: Single);
    procedure ResetScript;
    procedure Update(const Dt: Single);
    procedure SetImageQuality(S: String);
    function Expression(const S: String): String;
    procedure UpdateMenuItems;
    procedure SetVisible(const V: Boolean);
  end;

var
  Satania: TSatania;
  RunList: TStringList;
  RunResultList: TStringDict;
  CSAction: TRTLCriticalSection;

implementation

uses
  Form.Chat, Form.Main, mcdowell.chatbot, mcdowell.imap;

procedure TSataniaChatThread.SendChatSendToHer;
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
  Synchronize(@SendChatSendToHer);
  Terminate;
end;

procedure TSataniaExecThread.SendChatSendToHer;
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
  Synchronize(@SendChatSendToHer);
  Terminate;
end;

function TSatania.SETalk(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if Length(Args) = 1 then
  begin
    Satania.Talk(Args[0]);
    VM.IsPaused := True;
  end else
  begin
    if Args[1] = 0 then
    begin
      Satania.TalkWithoutBlock(Args[0]);
    end else
    begin
      Satania.Talk(Args[0]);
      VM.IsPaused := True;
    end;
  end;
end;

function TSatania.SERun(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Exec(Args[0]);
end;

function TSatania.SEIsRunning(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if RunList.IndexOf(Args[0]) >= 0 then
    Result := 1
  else
    Result := 0;
end;

function TSatania.SEGetRunResult(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := '';
  try
    Result := RunResultList[Args[0].VarString];
    RunResultList.Remove(Args[0].VarString);
  except
  end;
end;

function TSatania.SELoadModel(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  LoadModel(Args[0]);
end;

function TSatania.SEIsAnimationPlaying(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  TimeSensor: TTimeSensorNode;
begin
  try
    TimeSensor := Sprite.Node(Args[0]) as TTimeSensorNode;
    if (not TimeSensor.Loop) and (TimeSensor.Enabled) and (TimeSensor.StartTime > 0) and
       (TimeSensor.IsActive) and (not TimeSensor.IsPaused) then
      Result := 1
    else
      Result := 0;
  except
    on E: Exception do
      TalkWithoutBlock(E.Message);
  end;
end;

function TSatania.SEStartAnimation(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  StartAnimation(Args[0], Boolean(Round(Args[1].VarNumber)));
end;

function TSatania.SESetAnimationSpeed(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  SetAnimationSpeed(Args[0], Args[1].VarNumber);
end;

function TSatania.SEStopAnimation(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  StopAnimation(Args[0]);
end;

function TSatania.SEStopAllAnimations(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  StopAllAnimations;
end;

function TSatania.SELoadScheme(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Satania.ActionFromFile(Args[0]);
end;

function TSatania.SEDelta(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Delta;
end;

function TSatania.SELoadEmails(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  SataniaIMAP.GetMessages;
end;

function TSatania.SEGetUnseenEmailCount(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.MailList.Count;
end;

function TSatania.SEGetEmailSender(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.MailList[Round(Args[0].VarNumber)].Sender;
end;

function TSatania.SEGetEmailSubject(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.MailList[Round(Args[0].VarNumber)].Subject;
end;

function TSatania.SEGetEmailBody(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.MailList[Round(Args[0].VarNumber)].Body;
end;

function TSatania.SEIsEmailLoading(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.IsRunning;
end;

function TSatania.SEIsEmailSuccess(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.IsSuccess;
end;

function TSatania.SEIsEmailConfigured(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SataniaIMAP.IsEmailConfigured;
end;

function TSatania.SEDefaultScheme(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Save.Settings.DefaultEvilScheme;
end;

function TSatania.SEIsSoW(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Save.SitOnWindow
end;

function TSatania.SEIsLewd(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Save.Settings.Lewd;
end;

function TSatania.SEIsSilent(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Save.Silent
end;

function TSatania.SEIsSpeechToText(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Save.SpeechToText
end;

function TSatania.SESetScale(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  SetScale(Args[0].VarNumber);
end;   

function TSatania.SESetVisible(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if Args[0].VarNumber = 0 then
    SetVisible(False)
  else
    SetVisible(True);
end;

function TSatania.SEGetVisible(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Sprite.Visible;
end;

constructor TSatania.Create;
begin
  inherited;
  Name := 'Satania';
  Script := TEvilC.Create;
  Script.RegisterFunc('talk', @SETalk, -1);
  Script.RegisterFunc('process_run', @SERun, 1);
  Script.RegisterFunc('process_is_running', @SEIsRunning, 1);
  Script.RegisterFunc('process_result_get', @SEGetRunResult, 1);
  Script.RegisterFunc('sprite_visible_set', @SESetVisible, 1);    
  Script.RegisterFunc('sprite_visible_get', @SEGetVisible, 0);
  Script.RegisterFunc('sprite_animation_stop_all', @SEStopAllAnimations, 0);
  Script.RegisterFunc('sprite_load', @SELoadModel, 1);
  Script.RegisterFunc('sprite_animation_speed_set', @SESetAnimationSpeed, 2);
  Script.RegisterFunc('sprite_animation_play', @SEStartAnimation, 2);
  Script.RegisterFunc('sprite_animation_is_playing', @SEIsAnimationPlaying, 1);
  Script.RegisterFunc('sprite_animation_stop', @SEStopAnimation, 1);
  Script.RegisterFunc('is_sow', @SEIsSoW, 0);
  Script.RegisterFunc('is_lewd', @SEIsLewd, 0);
  Script.RegisterFunc('is_silent', @SEIsSilent, 0);
  Script.RegisterFunc('is_speech_to_text', @SEIsSpeechToText, 0);
  Script.RegisterFunc('sprite_scale_set', @SESetScale, 1);
  Script.RegisterFunc('flag_get', @Save.SEGetFlag, 1);
  Script.RegisterFunc('flag_set', @Save.SESetFlag, 2);
  Script.RegisterFunc('scheme_load', @SELoadScheme, 1);
  Script.RegisterFunc('scheme_default', @SEDefaultScheme, 0);
  Script.RegisterFunc('delta_time', @SEDelta, 0);
  Script.RegisterFunc('email_load', @SELoadEmails, 0);
  Script.RegisterFunc('email_unseen_count', @SEGetUnseenEmailCount, 0);
  Script.RegisterFunc('email_sender_get', @SEGetEmailSender, 1);
  Script.RegisterFunc('email_subject_get', @SEGetEmailSubject, 1);
  Script.RegisterFunc('email_body_get', @SEGetEmailBody, 1);
  Script.RegisterFunc('email_is_loading', @SEIsEmailLoading, 0);
  Script.RegisterFunc('email_is_success', @SEIsEmailSuccess, 0);
  Script.RegisterFunc('email_is_configured', @SEIsEmailConfigured, 0);
end;

destructor TSatania.Destroy;
begin
  inherited;
end;

procedure TSatania.DefaultPosition;
begin
  if (Save.SpriteDefaultLocationX = -1) and (Save.SpriteDefaultLocationY = -1) then
  begin
    Save.SpriteDefaultLocationX := ScreenWidth - 150;
    Save.SpriteDefaultLocationY := 150;
  end;
  Sprite.Translation := Vector3(Save.SpriteDefaultLocationX, Save.SpriteDefaultLocationY, 0);
end;

procedure TSatania.LoadModel(S: String);
begin
  S := PATH_SPRITES + Save.Settings.Skin + '/' + S;
  if S <> Sprite.URL then
  try
    begin
      Sprite.URL := '';
      Sprite.URL := S;
    end;
  except
    on E: Exception do
    begin
      Sprite.URL := PATH_SPRITES + 'fallback/skeleton.json';
      Talk(E.Message);
    end;
  end else
  begin
    StopAllAnimations;
  end;
  try
    TouchBone := nil;
    TouchBone := Sprite.RootNode.FindNode('Bone_touch') as TTransformNode;
  except
  end;
end;

procedure TSatania.SetAnimationSpeed(AnimName: String; Speed: Single);
var
  TimeSensor: TTimeSensorNode;
begin
  try
    TimeSensor := Sprite.Node(AnimName) as TTimeSensorNode;
    TimeSensor.FdCycleInterval.Value := Speed;
  except
    on E: Exception do
      TalkWithoutBlock(E.Message);
  end;
end;

procedure TSatania.StartAnimation(AnimName: String; IsRepeat: Boolean = True);
var
  TimeSensor: TTimeSensorNode;
  B: Boolean;
begin
  try
    TimeSensor := Sprite.Node(AnimName) as TTimeSensorNode;
    TimeSensor.Start(IsRepeat, True, 0);
    Sprite.ForceInitialAnimationPose;
  except
    on E: Exception do
      TalkWithoutBlock(E.Message);
  end;
end;

procedure TSatania.StartAnimation(URL, AnimName: String; IsRepeat: Boolean = True);
begin
  LoadModel(URL);
  StartAnimation(AnimName, IsRepeat);
end;

procedure TSatania.StopAnimation(AnimName: String);
var
  TimeSensor: TTimeSensorNode;
begin
  try
    TimeSensor := Sprite.Node(AnimName) as TTimeSensorNode;
    TimeSensor.Start(False, True, 0);
    TimeSensor.Stop;
  except
    on E: Exception do
      TalkWithoutBlock(E.Message);
  end;
end;

procedure TSatania.StopAllAnimations;
  procedure StopButNotResetAnimation(AnimName: String);
  var
    TimeSensor: TTimeSensorNode;
  begin
    try
      TimeSensor := Sprite.Node(AnimName) as TTimeSensorNode;
      TimeSensor.Stop;
    except
      on E: Exception do
        TalkWithoutBlock(E.Message);
    end;
  end;
var
  S: String;
begin
  for S in Sprite.AnimationsList do
    StopButNotResetAnimation(S);
  Sprite.ResetAnimationState;
end;

procedure TSatania.Action(Typ, Message: String);
begin
  EnterCriticalSection(CSAction);
  case Typ of
    'chat':
      Talk(Message);
    'script':
      begin
        ResetScript;
        Script.Source := Message;
        IsAction := True;
      end
    else
      begin
        raise Exception.Create('Type unknown "' + Typ + '"');
      end;
  end;
  LeaveCriticalSection(CSAction);
end;

procedure TSatania.ActionFromFile(FileName: String);
var
  FS: TFileStream;
  SS: TStringStream;
begin
  SS := TStringStream.Create('');
  try
    try
      FS := Download(PATH_SCRIPTS + Save.Settings.Skin + '/' + FileName) as TFileStream;
      FS.Position := 0;
      SS.CopyFrom(FS, FS.Size);
      Action('script', SS.DataString);
      FreeAndNil(FS);
    except
      on E: Exception do
        Talk(E.Message);
    end;
  finally
    FreeAndNil(SS);
  end;
end;

procedure TSatania.Talk(S: String);
begin
  LocalBoundingBoxSnapshot := Sprite.LocalBoundingBox;
  LocalBoundingBoxSnapshot.Data[0] := LocalBoundingBoxSnapshot.Data[0] * Sprite.Scale;
  LocalBoundingBoxSnapshot.Data[1] := LocalBoundingBoxSnapshot.Data[1] * Sprite.Scale;
  ChatText.ResetText;
  ChatText.MaxDisplayChars := 0;
  ChatText.Text.Text := S;
  if ChatText.Text.Count > 25 then
  begin
    ChatText.Text.Text := 'too much words... please check the history instead!';
  end;
  ChatBubbleDelay := 1;
  IsTalking := True;
  if S <> '' then
  begin
    Log(Name, S);
    Satania.StartAnimation('talk_loop');
    ChatBubbleDelay := Save.Settings.ChatBubbleDelay;
  end;
end;

procedure TSatania.TalkWithoutBlock(S: String);
begin
  LocalBoundingBoxSnapshot := Sprite.LocalBoundingBox;
  LocalBoundingBoxSnapshot.Data[0] := LocalBoundingBoxSnapshot.Data[0] * Sprite.Scale;
  LocalBoundingBoxSnapshot.Data[1] := LocalBoundingBoxSnapshot.Data[1] * Sprite.Scale;
  ChatText.ResetText;
  ChatText.MaxDisplayChars := 0;
  ChatText.Text.Text := S;
  if ChatText.Text.Count > 25 then
  begin
    ChatText.Text.Text := 'too much words... please check the history instead!';
  end;
  ChatBubbleDelay := 1;
  if S <> '' then
  begin
    Log(Name, S);
    ChatBubbleDelay := Save.Settings.ChatBubbleDelay;
  end;
  IsTalking := False;
end;

procedure TSatania.Log(LogName, S: String);
begin
  FormChat.MemoChatLog.Lines.BeginUpdate;
  FormChat.MemoChatLog.Text := LogName + ': ' + S + #13 + FormChat.MemoChatLog.Text;
  while FormChat.MemoChatLog.Lines.Count > 2000 do
    FormChat.MemoChatLog.Lines.Pop;
  FormChat.MemoChatLog.Lines.EndUpdate;
end;

procedure TSatania.Chat(S: String);
var
  ChatThread: TSataniaChatThread;
begin
  ChatThread := TSataniaChatThread.Create(True);
  ChatThread.ChatSend := S;
  ChatThread.FreeOnTerminate := True;
  ChatThread.Start;
end;

procedure TSatania.Update(const Dt: Single);
begin
  Delta := Dt;
  try
    if IsTalking and ChatText.FinishedTyping then
    begin
      Satania.StopAnimation('talk_loop');
      Satania.StartAnimation('talk_finish', False);
      IsTalking := False;
      Script.IsPaused := False;
    end;
    //
    if not Self.Script.IsDone then
    begin
      Script.Exec;
    end else
      IsAction := False;
  except
    on E: Exception do
    begin
      ResetScript;
      IsAction := False;
      Talk(E.Message);
    end;
  end;
end;

procedure TSatania.ResetScript;
begin
  Script.Reset;
  Script.Source := '';
end;

function TSatania.Exec(S: String): String;
var
  ExecThread: TSataniaExecThread;
begin
  ExecThread := TSataniaExecThread.Create(True);
  ExecThread.RunName := S;
  RunList.Add(ExecThread.RunName);
  ExecThread.ChatSend := S;
  ExecThread.FreeOnTerminate := True;
  ExecThread.Start;
  Result := ExecThread.RunName;
end;

procedure TSatania.SetScale(Scale: Single);
begin
  Sprite.Scale := Vector3(Scale, Scale, Scale) * Save.Settings.BaseScaling;
end;

procedure TSatania.SetImageQuality(S: String);
begin
  case S of
    'Linear':
      begin
        Sprite.RenderOptions.MinificationFilter := minLinear;
        Sprite.RenderOptions.MagnificationFilter := magLinear;
      end;
    'Nicest':
      begin
        Sprite.RenderOptions.MinificationFilter := minNicest;
        Sprite.RenderOptions.MagnificationFilter := magNicest;
      end;
    else
      begin
        Sprite.RenderOptions.MinificationFilter := minNearest;
        Sprite.RenderOptions.MagnificationFilter := magNearest;
      end;
  end;
end;

function TSatania.Expression(const S: String): String;
var
  SE: TEvilC;
  V: TSEValue;
begin
  Result := '';
  SE := TEvilC.Create;
  SE.Reset;
  SE.Source := 'result=' + S;
  try
    try
      V := SE.Exec;
      if V.Kind = sevkSingle then
        Result := FloatToStr(V.VarNumber)
      else
      if V.Kind = sevkString then
        Result := V.VarString;
    except
      on E: Exception do;
    end;
  finally
    SE.Free;
  end;
end;

procedure TSatania.UpdateMenuItems;
var
  I: Integer;
  MenuItem: TMenuItem;
  ScriptFiles: TStringList;
  S: String;
begin
  for I := 0 to Length(MenuItems) - 1 do
  begin
    MenuItems[I].Free;
  end;
  // Generate script menu
  ScriptFiles := TStringList.Create;
  try
    FindAllFiles(ScriptFiles, 'data/scripts/' + Save.Settings.Skin + '/menu', '*.evil', False);
    SetLength(MenuItems, ScriptFiles.Count);
    for I := 0 to ScriptFiles.Count - 1 do
    begin
      S := ExtractFileName(ScriptFiles[I]);
      S := StringReplace(S, ExtractFileExt(S), '', [rfReplaceAll]);
      MenuItem := TMenuItem.Create(FormMain);
      MenuItem.Caption := S;
      MenuItem.OnClick := @FormMain.DoExecuteScriptFromMenu;
      FormMain.MenuItemActions.Add(MenuItem);
      MenuItems[I] := MenuItem;
    end;
  finally
    ScriptFiles.Free;
  end;
end;

procedure TSatania.Notify(S: String);
begin
  FormMain.PopupNotifier.Text := S;
  FormMain.PopupNotifier.Visible := True;
  Log('System', S);
end;

procedure TSatania.SetVisible(const V: Boolean);
begin
  if V then
  begin                  
    FormMain.MenuItemHideShow.Caption := 'Hide';
    Satania.Sprite.Visible := True;
    FormTouch.Show;
  end else
  begin           
    FormMain.MenuItemHideShow.Caption := 'Show';
    Satania.Sprite.Visible := False;
    FormTouch.Hide;
  end;
end;

initialization
  InitCriticalSection(CSAction);
  Save := TSave.Create;
  if FileExists('configs.json') then
    Save.LoadFromFile('configs.json');
  Satania := TSatania.Create;
  RunList := TStringList.Create;
  RunResultList := TStringDict.Create;

finalization 
  Save.SaveToFile('configs.json');
  FreeAndNil(Save);
  FreeAndNil(Satania);
  FreeAndNil(RunList);
  FreeAndNil(RunResultList);
  DoneCriticalSection(CSAction);

end.

