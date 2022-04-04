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
  Forms, Menus, FileUtil, simpleinternet,
  fpjson, jsonparser, Process, LCLType, Types, LCLIntf, Graphics, syncobjs,
  CastleScene, CastleControls, CastleUIControls, CastleTypingLabel, CastleDownload,
  CastleVectors, X3DNodes, CastleBoxes, CastleFilesUtils, CastleURIUtils,
  CastleTransform, CastleRenderOptions, CastleViewport, CastleFonts,
  CastleBehaviors, Clipbrd, fphttpclient, LazUTF8,
  Mcdowell.EvilC, Mcdowell.Chat, Globals;

type
  TSatania = class
  protected
    {$define unit_protected}
    {$I mcdowell_se.inc}
    {$undef unit_protected}
  private
    PreviousMinute: Integer;
    PreviousDay: Integer;
  public
    Delta: Single;
    MenuItems: array of TMenuItem;
    IsAction: Boolean;
    IsTalking: Boolean;
    IsBlocked: Boolean;
    ChatResult: String;
    Sprite: TCastleScene;
    Viewport: TCastleViewport;
    LocalBoundingBoxSnapshot: TBox3D;
    ChatMode: Integer;
    ChatText: TCastleTypingLabel;
    ChatBubble: TCastleUserInterface;
    FontSystem: TCastleFont;
    ChatBubbleDelay: Integer;
    Form,
    FormTouch: TForm;
    AnimTalkLoop,
    AnimTalkFinish,
    Name: String;
    Script: TEvilC;     
    UsedRemindersList: TStringList;
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
    procedure Notify(C, S: String);
    procedure TalkWithoutBlock(S: String);
    function Exec(S: String): String;
    procedure Chat(S: String);
    procedure Action(Typ, Message: String);
    procedure ActionFromFile(FileName: String; IsChecked: Boolean = True);
    procedure SetScale(Scale: Single);
    procedure ResetScript;
    procedure Update(const Dt: Single);
    procedure SetImageQuality(S: String);
    function Expression(const S: String): String;
    procedure UpdateMenuItems;
    procedure SetVisible(const V: Boolean);
    procedure UpdateReminders;
    procedure UpdateMeta;
  end;

var
  Satania: TSatania;
  RunList: TStringList;
  RunResultList: TStringDict;
  CSAction,
  CSTalk: TCriticalSection;

implementation

uses
  form.reminders,
  form.chat,
  form.touch,
  mcdowell.chatbot,
  mcdowell.sound,
  mcdowell.net,
  mcdowell.numbers,
  form.main,
  mcdowell.imap;

{$define unit_implementation}
{$I mcdowell_se.inc}
{$undef unit_implementation}

constructor TSatania.Create;
begin
  inherited;
  Name := 'Satania';
  PreviousMinute := -1;
  PreviousDay := -1;
  UsedRemindersList := TStringList.Create;
  UsedRemindersList.Sorted := True;
  Script := TEvilC.Create;
  AnimTalkLoop := 'talk_loop';   
  AnimTalkFinish := 'talk_finish';            
  Script.RegisterFunc('buffer_length', @SEBufferLength, 1);
  Script.RegisterFunc('buffer_get', @SEBufferGet, 2);
  Script.RegisterFunc('numbers', @SENumbers, 1);     
  Script.RegisterFunc('months_to_numbers', @SEMonthsToNumbers, 1);
  Script.RegisterFunc('talk', @SETalk, -1);
  Script.RegisterFunc('notify', @SENotify, 1);
  Script.RegisterFunc('process_run', @SEProcessRun, 1);
  Script.RegisterFunc('process_is_running', @SEProcessIsRunning, 1);
  Script.RegisterFunc('process_result_get', @SEProcessResultGet, 1);
  Script.RegisterFunc('sprite_visible_set', @SESpriteVisibleSet, 1);    
  Script.RegisterFunc('sprite_visible_get', @SESpriteVisibleGet, 0);
  Script.RegisterFunc('sprite_animation_stop_all', @SEStopAllAnimations, 0);
  Script.RegisterFunc('sprite_load', @SESpriteLoad, 1);
  Script.RegisterFunc('sprite_animation_speed_set', @SESetAnimationSpeed, 2);
  Script.RegisterFunc('sprite_animation_play', @SEStartAnimation, 2);
  Script.RegisterFunc('sprite_animation_is_playing', @SEIsAnimationPlaying, 1);
  Script.RegisterFunc('sprite_animation_stop', @SEStopAnimation, 1); 
  Script.RegisterFunc('sprite_animation_talk_set', @SESpriteTalkSet, 2);
  Script.RegisterFunc('is_sow', @SEIsSoW, 0);
  Script.RegisterFunc('is_lewd', @SEIsLewd, 0);
  Script.RegisterFunc('is_silent', @SEIsSilent, 0);
  Script.RegisterFunc('is_speech_to_text', @SEIsSpeechToText, 0);
  Script.RegisterFunc('sprite_scale_set', @SESpriteScaleGet, 1);
  Script.RegisterFunc('flag_get', @Save.SEGetFlag, 1);
  Script.RegisterFunc('flag_set', @Save.SESetFlag, 2);
  Script.RegisterFunc('scheme_load', @SESchemeLoad, 1);
  Script.RegisterFunc('scheme_default', @SESchemeDefault, 0);
  Script.RegisterFunc('delta_time', @SEDelta, 0);
  Script.RegisterFunc('email_load', @SELoadEmails, 0);
  Script.RegisterFunc('email_unseen_count', @SEGetUnseenEmailCount, 0);
  Script.RegisterFunc('email_sender_get', @SEGetEmailSender, 1);
  Script.RegisterFunc('email_subject_get', @SEGetEmailSubject, 1);
  Script.RegisterFunc('email_body_get', @SEGetEmailBody, 1);
  Script.RegisterFunc('email_is_loading', @SEIsEmailLoading, 0);
  Script.RegisterFunc('email_is_success', @SEIsEmailSuccess, 0);
  Script.RegisterFunc('email_is_configured', @SEIsEmailConfigured, 0);
  Script.RegisterFunc('sound_play', @SESoundPlay, 1);          
  Script.RegisterFunc('url_open', @SEOpenURL, 1);
  Script.RegisterFunc('url_get', @SEURLGet, 1);    
  Script.RegisterFunc('url_post', @SEURLPost, 2);
  Script.RegisterFunc('url_upload', @SEURLUpload, 4);
  Script.RegisterFunc('url_is_success', @SEURLIsSuccess, 1); 
  Script.RegisterFunc('url_result_get', @SEURLGetResult, 1);
  Script.RegisterFunc('url_query', @SEURLProcess, 2);          
  Script.RegisterFunc('url_encode', @SEURLEncode, 1);
  Script.RegisterFunc('url_decode', @SEURLDecode, 1);
  Script.RegisterFunc('chat_mode_set', @SEChatModeSet, 1);      
  Script.RegisterFunc('chat_result_get', @SEChatResultGet, 0);         
  Script.RegisterFunc('reminder_create', @SEReminderCreate, 3);
  Script.RegisterFunc('reminder_today_get', @SEReminderTodayGet, 0);
  Script.RegisterFunc('clipboard_get', @SEClipboardGet, 0);  
  Script.RegisterFunc('clipboard_to_file', @SEClipboardToFile, 1);
  Script.RegisterFunc('fs_file_delete', @SEFileDelete, 1);    
  Script.RegisterFunc('fs_file_exists', @SEFileExists, 1);
  Script.RegisterFunc('fs_file_read', @SEFileRead, 1);
  Script.RegisterFunc('fs_file_write', @SEFileWrite, 2);
  UpdateMeta;
end;

destructor TSatania.Destroy;
begin
  UsedRemindersList.Free;
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
      Sprite.URL := PATH_SPRITES + 'template/sprites.plist';
      Talk(E.Message);
    end;
  end else
  begin
    StopAllAnimations;
  end;
  try
    TouchBone := nil;
    TouchBone := Sprite.RootNode.FindNode('Bone_touch') as TTransformNode;

    LocalBoundingBoxSnapshot := Sprite.LocalBoundingBox;
    LocalBoundingBoxSnapshot.Data[0] := LocalBoundingBoxSnapshot.Data[0] * Sprite.Scale;
    LocalBoundingBoxSnapshot.Data[1] := LocalBoundingBoxSnapshot.Data[1] * Sprite.Scale;

    AnimTalkLoop := 'talk_loop';
    AnimTalkFinish := 'talk_finish';
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
  CSAction.Enter;
  try
    case Typ of
      'chat':
        Talk(Message);
      'script':
        begin
          ResetScript;
          Script.Source := Message + ' scheme_load(scheme_default)';
          IsAction := True;
        end
      else
        begin
          raise Exception.Create('Type unknown "' + Typ + '"');
        end;
    end;
  finally
    CSAction.Leave;
  end;
end;

procedure TSatania.ActionFromFile(FileName: String; IsChecked: Boolean = True);
var
  FS: TFileStream;
  SS: TStringStream;
begin
  SS := TStringStream.Create('');
  try
    try
      if not IsChecked then
      begin
        if not FileExists('data/scripts/' + Save.Settings.Skin + '/' + FileName) then
        begin
          Exit;
        end;
      end;
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
  CSTalk.Enter;
  try
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
      if AnimTalkLoop <> '' then
        Satania.StartAnimation(AnimTalkLoop);
      ChatBubbleDelay := Save.Settings.ChatBubbleDelay;
    end;
  finally
    CSTalk.Leave;
  end;
end;

procedure TSatania.TalkWithoutBlock(S: String);
begin
  CSTalk.Enter;
  try
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
  finally
    CSTalk.Leave;
  end;
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
  if ChatMode = CHATMODE_CHAT then
  begin
    ChatThread := TSataniaChatThread.Create(True);
    ChatThread.ChatSend := S;
    ChatThread.FreeOnTerminate := True;
    ChatThread.Start;
  end else
  begin
    ChatResult := S;
  end;
end;

procedure TSatania.Update(const Dt: Single);
begin
  Delta := Dt;
  try
    if IsTalking and ChatText.FinishedTyping then
    begin
      if AnimTalkLoop <> '' then
      begin
        Satania.StopAnimation(AnimTalkLoop);
        Satania.StartAnimation(AnimTalkFinish, False);
      end;
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
  ChatMode := CHATMODE_CHAT;
  ChatResult := '';
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
  SE.Source := 'result=' + S;
  try
    try
      V := SE.Exec;
      if V.Kind = sevkSingle then
        Result := PointFloatToStr(V.VarNumber)
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
  ScriptFiles.Sorted := True;
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

procedure TSatania.Notify(C, S: String);
begin
  FormMain.PopupNotifier.Title := C;
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

procedure TSatania.UpdateReminders;
var
  Year, Month, Day, Hour, Minute, Second, Mili: Word;
  Current: TDateTime;
  I, L: Integer;
  Q1, Q2: QWord;
  Item: TReminderCollectionItem;
  IsAlarm: Boolean;
  DOW: Integer;
begin
  Current := Now;
  DecodeTime(Current, Hour, Minute, Second, Mili);
  if Minute <> PreviousMinute then
  begin
    PreviousMinute := Minute;
    DecodeDate(Current, Year, Month, Day);
    Q1 := StrToQWord(Format('%.4d%.2d%.2d%.2d%.2d', [Year, Month, Day, Hour, Minute]));
    if PreviousDay <> Day then
    begin
      UsedRemindersList.Clear;
      PreviousDay := Day;
    end;
    for I := Save.Reminders.Count - 1 downto 0 do
    begin
      Item := TReminderCollectionItem(Save.Reminders.Items[I]);
      IsAlarm := False;
      if (Item.Enabled) and (not UsedRemindersList.Find(Item.Name, L)) then
      begin
        case Item.Kind of
          0:
            begin
              DOW := DayOfWeek(Current);
              if (Item.Minute = Minute) and (Item.Hour = Hour) and
               (((DOW = 1) and (Item.Sunday)) or
                ((DOW = 2) and (Item.Monday)) or
                ((DOW = 3) and (Item.Tuesday)) or
                ((DOW = 4) and (Item.Wednesday)) or
                ((DOW = 5) and (Item.Thursday)) or
                ((DOW = 6) and (Item.Friday)) or
                ((DOW = 7) and (Item.Saturday))) then
                IsAlarm := True;
            end;
          1:
            begin
              Q2 := StrToQWord(Format('%.4d%.2d%.2d%.2d%.2d', [Item.Year, Item.Month, Item.Day, Item.Hour, Item.Minute]));
              if Q1 >= Q2 then
              begin
                IsAlarm := True;
              end;
            end;
        end;
      end;
      if IsAlarm then
      begin
        UsedRemindersList.Add(Item.Name);
        Satania.Action('script', Item.Script);
        if Item.Kind = 1 then
        begin
          Save.Reminders.Delete(I);
          Save.SaveToFile('configs.json');
        end;
      end;
    end;
  end;
end;

procedure TSatania.UpdateMeta;
var
  JSON: TJSONObject;
  SL: TStrings;
  MetaPath: String;
  IsNamed: Boolean = False;
  I: Integer;
begin
  Script.ConstMap.Clear;
  Script.AddDefaultConsts;
  Script.ConstMap.Add('CHATMODE_CHAT', CHATMODE_CHAT);
  Script.ConstMap.Add('CHATMODE_SCRIPT', CHATMODE_SCRIPT);
  MetaPath := 'data/scripts/' + Save.Settings.Skin + '/meta.json';
  Name := 'Satania';
  if FileExists(MetaPath) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(MetaPath);
      JSON := GetJSON(SL.Text) as TJSONObject;
      for I := 0 to JSON.Count - 1 do
      begin
        Script.ConstMap.Add(JSON.Names[I], JSON.Items[I].AsString);
        if JSON.Names[I] = 'name' then
        begin
          IsNamed := True;
          Name := JSON.Items[I].AsString;
        end;
      end;
      JSON.Free;
    finally
      SL.Free;
    end;
  end;
  if not IsNamed then
    Script.ConstMap.Add('name', Name);
end;

initialization
  CSAction := TCriticalSection.Create;
  CSTalk := TCriticalSection.Create;
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
  CSTalk.Free;
  CSAction.Free;

end.

