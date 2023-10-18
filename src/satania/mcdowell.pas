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

unit Mcdowell;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Generics.Collections, Controls,
  Forms, Menus, FileUtil, simpleinternet,
  fpjson, jsonparser, Process, LCLType, Types, LCLIntf, Graphics, syncobjs,
  CastleScene, CastleControls, CastleUIControls, CastleTypingLabel, CastleDownload,
  CastleVectors, X3DNodes, CastleBoxes, CastleFilesUtils, CastleURIUtils,
  CastleTransform, CastleRenderOptions, CastleViewport, CastleFonts,
  CastleSceneCore, CastleSpine, CastleSpineMixer, strutils,
  CastleBehaviors, Clipbrd, fphttpclient, LazUTF8, IniFiles, JsonTools,
  Mcdowell.EvilC, Mcdowell.Chat, Globals, AnchorDocking;

type
  TSataniaBackgroundScript = record
    Script: TEvilC;
    IsPersistent: Boolean;
    Interval,
    LastTimestamp: QWord;
  end;
  TSataniaBackgroundScriptDict = specialize TDictionary<String, TSataniaBackgroundScript>;

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
    IsAsking: Boolean;
    IsBlocked: Boolean;
    ChatResult: String;
    Sprite: TCastleTransform;
    SpriteAsX3D: TCastleScene;
    SpriteAsSpine: TCastleSpine;
    Mixer: TCastleSpineMixerBehavior;
    Viewport: TCastleViewport;
    LocalBoundingBoxSnapshot: TBox3D;
    ChatMode: Integer;
    FontSystem: TCastleFont;
    ChatBubbleDelay: Integer;
    Form,
    FormTouch: TForm;
    AnimTalkLoop,
    AnimTalkFinish,
    Name: String;
    Script: TEvilC;
    BackgroundScriptDict: TSataniaBackgroundScriptDict;
    AnimTalkScriptList: TStringList; // List of possible scripts to execute during talking
    UsedRemindersList: TStringList;
    { Where we should move our touch panel to }
    TouchBone: TCastleTransform;
    { The root contains all the sketches }
    SketchBefore,
    SketchAfter: TCastleTransform;
    { Store local config }
    LocalFlagIni: TIniFile;
    constructor Create;
    destructor Destroy; override;
    procedure RegisterFuncs(const S: TEvilC; const IsSafe: Boolean = False);
    procedure SetPosition(X, Y: Integer);
    procedure DefaultPosition;
    procedure LoadModel(S: String);
    procedure LoadLocalFlags;
    procedure TakeLocalBoundingBoxSnapshot;
    procedure SetAnimationSpeed(AnimName: String; Speed: Single);
    procedure StartAnimation(AnimName: String; IsRepeat: Boolean = True); overload;
    procedure StartAnimation(URL, AnimName: String; IsRepeat: Boolean = True); overload;
    procedure StopAnimation(AnimName: String);
    procedure StopAllAnimations;
    procedure Log(LogName, S: String); overload;
    procedure Log(S: String); overload;
    procedure Error(S: String);
    procedure Talk(S: String);
    procedure Ask(S: String);
    procedure TalkReset(S: String);
    procedure Notify(C, S: String);
    procedure TalkWithoutBlock(S: String);
    function Exec(S: String): String;
    procedure Chat(S: String);
    procedure Action(Typ, Message: String);
    procedure Worker(const AKey, AScript: String; const ATime: Single; const AArgs: TSEValue);
    procedure WorkerDelete(const AKey: String);      
    procedure ActionFromFileGlobal(FileName: String; IsChecked: Boolean = True);
    procedure ActionFromFile(FileName: String; IsChecked: Boolean = True);
    procedure SetScale(Scale: Single);
    procedure ResetScript;
    procedure Update(const Dt: Single);
    procedure SetImageQuality(S: String);
    function Expression(const S: String): String;
    procedure UpdateMenuItems;
    procedure SetVisible(const V: Boolean);
    procedure UpdateReminders;
    procedure UpdateMeta(const S: TEvilC);
    procedure CleanUpCache;
    procedure BackgroundScriptClearAll;
  end;

var
  Satania: TSatania;
  ThreadDict: TThreadDict;
  RunProcessResultList: TStringDict;
  RunProcessNonBlockResultList: TNonBlockProcessDict;
  RunHttpResultList: THttpResponseDict;
  CSAction,
  CSTalk: TCriticalSection;

implementation

uses
  Utils.coords,
  Utils.Encdec,
  Utils.Strings,
  Utils.Threads,
  utils.sprites,
  form.reminders,
  form.chat,
  form.touch,
  form.ask,
  form.bubble,
  form.tool.evilceditor,
  form.tool.hexeditor,
  mcdowell.chatbot,
  mcdowell.sound,
  mcdowell.net,
  mcdowell.numbers,
  form.main,
  mcdowell.imap,
  mcdowell.smtp,
  mcdowell.sketch,
  mcdowell.chat.history,
  com.Brokers;

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
  BackgroundScriptDict := TSataniaBackgroundScriptDict.Create;
  AnimTalkLoop := 'talk_loop';
  AnimTalkFinish := 'talk_finish';
  Self.AnimTalkScriptList := TStringList.Create;
  Self.RegisterFuncs(Self.Script);
  UpdateMeta(Self.Script);
  Self.LoadLocalFlags;
end;

destructor TSatania.Destroy;
begin
  UsedRemindersList.Free;
  AnimTalkScriptList.Free;
  Script.Free;
  BackgroundScriptClearAll;
  BackgroundScriptDict.Free;
  if LocalFlagIni <> nil then
    FreeAndNil(LocalFlagIni);
  inherited;
end;

procedure TSatania.RegisterFuncs(const S: TEvilC; const IsSafe: Boolean = False);
begin
  if not IsSafe then
  begin
    S.RegisterFunc('ask', @SEAsk, -1);
    S.RegisterFunc('scheme_load', @SESchemeLoad, 1);
  end;
  S.RegisterFunc('talk', @SETalk, 1);    
  S.RegisterFunc('talk_system', @SELog, 1);
  S.RegisterFunc('stream_enable', @SEStreamEnable, 0);
  S.RegisterFunc('stream_disable', @SEStreamDisable, 0);
  S.RegisterFunc('stream', @SEStream, 1);       
  S.RegisterFunc('stream_position_get', @SEStreamPositionGet, 0);
  S.RegisterFunc('numbers', @SENumbers, 1);
  S.RegisterFunc('months_to_numbers', @SEMonthsToNumbers, 1);
  S.RegisterFunc('answer', @SEAnswer, 0);
  S.RegisterFunc('notify', @SENotify, 1);
  S.RegisterFunc('process_run', @SEProcessRun, 2);
  S.RegisterFunc('process_pipe_get', @SEProcessPipeGet, 1);
  S.RegisterFunc('process_pipe_set', @SEProcessPipeSet, 2); 
  S.RegisterFunc('process_terminate', @SEProcessTerminate, 1);
  S.RegisterFunc('sprite_visible_set', @SESpriteVisibleSet, 1);
  S.RegisterFunc('sprite_visible_get', @SESpriteVisibleGet, 0);
  S.RegisterFunc('sprite_animation_stop_all', @SEStopAllAnimations, 0);
  S.RegisterFunc('sprite_load', @SESpriteLoad, 1);
  S.RegisterFunc('sprite_animation_speed_set', @SESetAnimationSpeed, 2);
  S.RegisterFunc('sprite_animation_play', @SEStartAnimation, 2);
  S.RegisterFunc('sprite_animation_is_playing', @SEIsAnimationPlaying, 1);
  S.RegisterFunc('sprite_animation_stop', @SEStopAnimation, 1);
  S.RegisterFunc('sprite_animation_talk_set', @SESpriteTalkSet, -1);
  S.RegisterFunc('sprite_position_set', @SESpritePositionSet, 2);
  S.RegisterFunc('is_sow', @SEIsSoW, 0);
  S.RegisterFunc('is_lewd', @SEIsLewd, 0);
  S.RegisterFunc('is_silent', @SEIsSilent, 0);
  S.RegisterFunc('is_speech_to_text', @SEIsSpeechToText, 0);
  S.RegisterFunc('sprite_scale_set', @SESpriteScaleGet, 1);
  S.RegisterFunc('flag_global_get', @Save.SEGetFlag, 1);
  S.RegisterFunc('flag_global_set', @Save.SESetFlag, 2);
  S.RegisterFunc('flag_local_get', @SELocalFlagGet, 1);
  S.RegisterFunc('flag_local_set', @SELocalFlagSet, 2);
  S.RegisterFunc('scheme_default', @SESchemeDefault, 0);
  S.RegisterFunc('delta_time', @SEDelta, 0);
  S.RegisterFunc('email_imap_load', @SELoadEmails, 0);
  S.RegisterFunc('email_imap_unseen_count', @SEGetUnseenEmailCount, 0);
  S.RegisterFunc('email_imap_sender_get', @SEGetEmailSender, 1);
  S.RegisterFunc('email_imap_subject_get', @SEGetEmailSubject, 1);
  S.RegisterFunc('email_imap_body_get', @SEGetEmailBody, 1);
  S.RegisterFunc('email_imap_is_loading', @SEIsEmailLoading, 0);
  S.RegisterFunc('email_imap_is_success', @SEIsEmailSuccess, 0);
  S.RegisterFunc('email_imap_is_configured', @SEIsEmailConfigured, 0);
  S.RegisterFunc('email_smtp_is_configured', @SEIsEmailSMTPConfigured, 0);
  S.RegisterFunc('email_smtp_send', @SEEmailSMTPSend, 6);
  S.RegisterFunc('sound_play', @SESoundPlay, 1);
  S.RegisterFunc('http_open', @SEOpenURL, 1);
  S.RegisterFunc('http_fetch', @SEURLFetch, 4);
  S.RegisterFunc('http_upload', @SEURLUpload, 5); 
  S.RegisterFunc('http_progress_get', @SEURLGetProgress, 1);
  S.RegisterFunc('http_is_success', @SEURLIsSuccess, 1);
  S.RegisterFunc('http_result_get', @SEURLGetResult, -1);
  S.RegisterFunc('http_query', @SEURLProcess, 2);
  S.RegisterFunc('url_encode', @SEURLEncode, 1);
  S.RegisterFunc('url_decode', @SEURLDecode, 1);
  S.RegisterFunc('chat_history_get', @SEChatHistoryGet, 0);
  S.RegisterFunc('chat_mode_set', @SEChatModeSet, 1);
  S.RegisterFunc('chat_result_get', @SEChatResultGet, 0);
  S.RegisterFunc('reminder_create', @SEReminderCreate, 3);
  S.RegisterFunc('reminder_today_get', @SEReminderTodayGet, 0);
  S.RegisterFunc('clipboard_get', @SEClipboardGet, 0);
  S.RegisterFunc('clipboard_to_file', @SEClipboardToFile, 1);
  S.RegisterFunc('fs_file_delete', @SEFileDelete, 1);
  S.RegisterFunc('fs_file_rename', @SEFileRename, 2);
  S.RegisterFunc('fs_file_exists', @SEFileExists, 1); 
  S.RegisterFunc('fs_file_read', @SEFileReadText, 1);
  S.RegisterFunc('fs_file_read_text', @SEFileReadText, 1);
  S.RegisterFunc('fs_file_read_binary', @SEFileReadBinary, 1); 
  S.RegisterFunc('fs_file_write', @SEFileWriteText, 2);
  S.RegisterFunc('fs_file_write_text', @SEFileWriteText, 2);
  S.RegisterFunc('fs_file_write_binary', @SEFileWriteBinary, 3);
  S.RegisterFunc('fs_file_copy', @SEFileCopy, 2);
  S.RegisterFunc('fs_file_size_get', @SEFileGetSize, 1);
  S.RegisterFunc('fs_file_find_all', @SEFileFindAll, 4);
  S.RegisterFunc('fs_directory_create', @SEDirectoryCreate, 1);
  S.RegisterFunc('fs_directory_delete', @SEDirectoryDelete, 1);
  S.RegisterFunc('fs_directory_find_all', @SEDirectoryFindAll, 2);
  S.RegisterFunc('fs_directory_exists', @SEDirectoryExists, 1);     
  S.RegisterFunc('fs_directory_config_get', @SEDirectoryGetConfig, 0);
  S.RegisterFunc('json_parse', @SEJSONParse, 1);
  S.RegisterFunc('json_stringify', @SEJSONStringify, 1);
  S.RegisterFunc('sprite_other_create', @SESketchCreate, 1);
  S.RegisterFunc('sprite_other_render', @SESketchLoadFromText, 3);
  S.RegisterFunc('sprite_other_exists', @SESketchExists, 1);
  S.RegisterFunc('sprite_other_delete', @SESketchClear, 1);
  S.RegisterFunc('sprite_other_delete_all', @SESketchClearAll, 0);
  S.RegisterFunc('worker_create', @SEWorkerCreate, -1);
  S.RegisterFunc('worker_persistent_set', @SEWorkerExists, 2);
  S.RegisterFunc('worker_exists', @SEWorkerExists, 1);
  S.RegisterFunc('worker_delete', @SEWorkerDelete, 1);
  S.RegisterFunc('tool_evilc_editor', @SEToolEvilCEditor, 1);
  S.RegisterFunc('tool_hex_editor', @SEToolHexEditor, 1);
end;

procedure TSatania.SetPosition(X, Y: Integer);
begin
  SpriteAsX3D.Translation := Vector3(X, ScreenHeight - Y, 0);
  SpriteAsSpine.Translation := Vector3(X, ScreenHeight - Y, 0);
end;

procedure TSatania.DefaultPosition;
begin
  if (Save.SpriteDefaultLocationX = -1) and (Save.SpriteDefaultLocationY = -1) then
  begin
    Save.SpriteDefaultLocationX := ScreenWidth - 150;
    Save.SpriteDefaultLocationY := 150;
  end;
  SpriteAsX3D.Translation := Vector3(Save.SpriteDefaultLocationX, Save.SpriteDefaultLocationY, 0);
  SpriteAsSpine.Translation := Vector3(Save.SpriteDefaultLocationX, Save.SpriteDefaultLocationY, 0);
end;

procedure TSatania.LoadModel(S: String);
var
  Ext: String;
  RM: TRemoveType = rtNone;
  ExposeTransforms: TStrings;
begin
  Ext := LowerCase(ExtractFileExt(S));
  S := PATH_SPRITES + Save.Settings.Skin + '/' + S;
  if (SpriteAsX3D.Exists and (S <> SpriteAsX3D.URL)) or (SpriteAsSpine.Exists and (S <> SpriteAsSpine.URL)) or Save.Settings.DeveloperMode then
  try
    begin
      // Clean up sprite's data
      SpriteAsX3D.URL := '';
      SpriteAsSpine.URL := '';
      // Hide the sprite
      Sprite.Exists := False;
      // Load new model, runtime based on ext
      case Ext of
        '.json':
          begin
            if Save.Settings.DeveloperMode then
              SpineDataCache.Clear; // Clear spine cache in case this is developer mode
            Sprite := Self.SpriteAsSpine;
            TCastleSpine(Sprite).URL := S;
          end
        else
          begin
            Sprite := Self.SpriteAsX3D;
            TCastleScene(Sprite).URL := S;
          end;
      end;
      Sprite.Exists := True;
    end;
    TrackDict.Clear; // For spine only
  except
    on E: Exception do
    begin
      TCastleScene(Sprite).URL := PATH_SPRITES + 'template/sprites.plist';
      Error(E.Message);
    end;
  end else
  begin
    StopAllAnimations;
  end;
  try
    AnimTalkLoop := 'talk_loop';
    AnimTalkFinish := 'talk_finish';
    Self.AnimTalkScriptList.Clear;

    TakeLocalBoundingBoxSnapshot;

    TouchBone := nil;
    ExposeTransforms := TStringList.Create;
    if Sprite = Self.SpriteAsX3D then
    begin
      if TCastleScene(Sprite).RootNode.FindNode('Bone_touch') <> nil then
      begin
        ExposeTransforms.Add('Bone_touch');
        TCastleScene(Sprite).ExposeTransforms := ExposeTransforms;
        TouchBone := Sprite.Items[0];
      end;
    end else
    begin
      ExposeTransforms.Add('touch');
      TCastleSpine(Sprite).ExposeTransforms := ExposeTransforms;
      if Sprite.Count > 0 then
        TouchBone := Sprite.Items[0];
    end;
    ExposeTransforms.Free;
  except
  end;
end;

procedure TSatania.LoadLocalFlags;
var
  IniFilePath: String;
begin
  if LocalFlagIni <> nil then
    FreeAndNil(LocalFlagIni);
  IniFilePath := PATH_SCRIPTS_RAW + Save.Settings.Skin +  '/flags.ini';
  LocalFlagIni := TIniFile.Create(IniFilePath);
end;

procedure TSatania.TakeLocalBoundingBoxSnapshot;
begin
  LocalBoundingBoxSnapshot := Sprite.LocalBoundingBox;
  LocalBoundingBoxSnapshot.Data[0] := LocalBoundingBoxSnapshot.Data[0] * Sprite.Scale;
  LocalBoundingBoxSnapshot.Data[1] := LocalBoundingBoxSnapshot.Data[1] * Sprite.Scale;
end;

procedure TSatania.SetAnimationSpeed(AnimName: String; Speed: Single);
begin
  SpriteSetAnimationSpeed(Self.Sprite, AnimName, Speed);
end;

procedure TSatania.StartAnimation(AnimName: String; IsRepeat: Boolean = True);
begin
  if AnimName <> '' then
    SpriteStartAnimation(Self.Sprite, Self.Mixer, AnimName, IsRepeat);
end;

procedure TSatania.StartAnimation(URL, AnimName: String; IsRepeat: Boolean = True);
begin
  if AnimName <> '' then
  begin
    LoadModel(URL);
    StartAnimation(AnimName, IsRepeat);
  end;
end;

procedure TSatania.StopAnimation(AnimName: String);
begin
  SpriteStopAnimation(Self.Sprite, Self.Mixer, AnimName);
end;

procedure TSatania.StopAllAnimations;
begin
  SpriteStopAllAnimations(Self.Sprite, Self.Mixer);
end;

procedure TSatania.Action(Typ, Message: String);
begin
  CSAction.Enter;
  try
    Self.Script.IncludePathList.Clear;
    Self.Script.IncludePathList.Add('data/scripts/' + Save.Settings.Skin + '/');  
    Self.Script.IncludePathList.Add(GetAppConfigDir(True) + 'data/scripts/' + Save.Settings.Skin + '/');
    case Typ of
      'chat':
        Talk(Message);
      'script':
        begin
          ResetScript;
          Script.Source := Message + ' scheme_load(scheme_default())';
          IsAction := True;
        end;
      else
        begin
          raise Exception.Create('Type unknown "' + Typ + '"');
        end;
    end;
  finally
    CSAction.Leave;
  end;
end;

procedure TSatania.Worker(const AKey, AScript: String; const ATime: Single; const AArgs: TSEValue);
begin
  Self.SEWorkerCreate(nil, [AKey, AScript, ATime, AArgs]);
end;

procedure TSatania.WorkerDelete(const AKey: String);
begin
  if Self.BackgroundScriptDict.ContainsKey(AKey) then
  begin
    Self.BackgroundScriptDict[AKey].Script.Free;
    Self.BackgroundScriptDict.Remove(AKey);
  end;
end;

procedure TSatania.ActionFromFile(FileName: String; IsChecked: Boolean = True);
var
  FS: TFileStream;
  SS: TStringStream;
  Path: String;
  IsContainKey: Boolean = False;
begin
  SS := TStringStream.Create('');
  try
    try
      if not IsChecked then
      begin
        if (not FileExists('data/scripts/' + Save.Settings.Skin + '/' + FileName)) and
           (not FileExists(GetAppConfigDir(True) + 'data/scripts/' + Save.Settings.Skin + '/' + FileName)) then
        begin
          Exit;
        end;
      end;
      Path := PATH_SCRIPTS + Save.Settings.Skin + '/' + FileName;
      // Clear the cache in developer mode
      if Save.Settings.DeveloperMode then
        Self.CleanUpCache;
      // We always load script in developer mode
      IsContainKey := ScriptCacheMap.ContainsKey(Path);
      if Save.Settings.DeveloperMode or (not IsContainKey) then
      begin
        FS := Download(Path) as TFileStream;
        FS.Position := 0;
        SS.CopyFrom(FS, FS.Size);
        FreeAndNil(FS);
        Action('script', SS.DataString);
        // Parse the source beforehand to store backup
        Self.Script.OptimizeAsserts := not Save.Settings.DeveloperMode;
        Self.Script.Lex;
        Self.Script.Parse;
        ScriptCacheMap.Add(Path, Self.Script.Backup);
      end else
      if IsContainKey then
      begin
        CSAction.Enter;
        try
          // Restore binaries from cache
          ResetScript;
          Self.Script.Restore(ScriptCacheMap[Path]);
          IsAction := True;
        finally
          CSAction.Leave;
        end;
      end;
    except
      on E: Exception do
        Error(E.Message);
    end;
  finally
    FreeAndNil(SS);
  end;
end;

procedure TSatania.ActionFromFileGlobal(FileName: String; IsChecked: Boolean = True);
var
  FS: TFileStream;
  SS: TStringStream;
  Path: String;
  IsContainKey: Boolean = False;
begin
  SS := TStringStream.Create('');
  try
    try
      if not IsChecked then
      begin
        if not FileExists(FileName) then
        begin
          Exit;
        end;
      end;
      Path := FileName;
      // Clear the cache in developer mode
      if Save.Settings.DeveloperMode then
        Self.CleanUpCache;
      // We always load script in developer mode
      IsContainKey := ScriptCacheMap.ContainsKey(Path);
      if Save.Settings.DeveloperMode or (not IsContainKey) then
      begin
        FS := Download(Path) as TFileStream;
        FS.Position := 0;
        SS.CopyFrom(FS, FS.Size);
        FreeAndNil(FS);
        Action('script', SS.DataString);
        // Parse the source beforehand to store backup
        Self.Script.OptimizeAsserts := not Save.Settings.DeveloperMode;
        Self.Script.Lex;
        Self.Script.Parse;
        ScriptCacheMap.Add(Path, Self.Script.Backup);
      end else
      if IsContainKey then
      begin
        CSAction.Enter;
        try
          // Restore binaries from cache
          ResetScript;
          Self.Script.Restore(ScriptCacheMap[Path]);
          IsAction := True;
        finally
          CSAction.Leave;
        end;
      end;
    except
      on E: Exception do
        Error(E.Message);
    end;
  finally
    FreeAndNil(SS);
  end;
end;

procedure TSatania.Talk(S: String);
begin
  CSTalk.Enter;
  FormChat.DisableStreaming;
  FormBubble.DisableStreaming;
  try
    TakeLocalBoundingBoxSnapshot;
    FormBubble.Text := S;
    ChatBubbleDelay := 1;
    IsTalking := True;
    IsAsking := False;
    if S <> '' then
    begin
      Log(Name, S);
      ChatBubbleDelay := Save.Settings.ChatBubbleDelay;
      if Self.AnimTalkScriptList.Count > 0 then
      begin
        // Play an animation randomly
        Satania.ActionFromFile(Self.AnimTalkScriptList.Strings[Random(Self.AnimTalkScriptList.Count)]);
      end;
    end;
  finally
    CSTalk.Leave;
  end;
end;

procedure TSatania.Ask(S: String);
begin
  CSTalk.Enter;
  try
    TakeLocalBoundingBoxSnapshot;

    FormAsk.Answer.Clear;
    FormAsk.AskText.Clear;
    FormAsk.Ask := S;
    FormBubble.Text := S;
    ChatBubbleDelay := 1;
    IsTalking := True;
    IsAsking := True;
   // IsBlocked := True;
    if S <> '' then
    begin
      ChatBubbleDelay := Save.Settings.ChatBubbleDelay;
    end;
  finally
    CSTalk.Leave;
  end;
end;

procedure TSatania.TalkReset(S: String);
begin
  Self.Talk(S);
  Satania.ActionFromFile(Save.Settings.DefaultEvilScheme);
end;

procedure TSatania.TalkWithoutBlock(S: String);
begin
  CSTalk.Enter;
  try
    TakeLocalBoundingBoxSnapshot;
    FormBubble.Text := S;
    ChatBubbleDelay := 1;
    if S <> '' then
    begin
      Log(Name, S);
      ChatBubbleDelay := Save.Settings.ChatBubbleDelay;
    end;
    IsTalking := False;
    IsAsking := False;
  finally
    CSTalk.Leave;
  end;
end;

procedure TSatania.Log(LogName, S: String);
begin
  FormChat.InsertLog(LogName, S);
end;

procedure TSatania.Log(S: String);
begin
  FormChat.InsertLog('System', S);
  Writeln(S);
end;

procedure TSatania.Error(S: String);
begin
  if Save.Settings.SystemErrorMessage then
    Self.Log(S)
  else
    Self.Talk(S);
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
var
  I: Integer;
  Key: String;
  BackgroundScript: TSataniaBackgroundScript;
begin
  Delta := Dt;
  try
    if IsTalking and FormBubble.FinishedTyping then
    begin
      IsTalking := False;
      Script.IsPaused := False;
    end;
    //
    if not Self.Script.IsDone then
    begin
      Script.Exec;
    end else
      IsAction := False;
    // Execute background scripts
    for I := Self.BackgroundScriptDict.Count - 1 downto 0 do
    begin
      Key := Self.BackgroundScriptDict.Keys.ToArray[I];
      BackgroundScript := Self.BackgroundScriptDict[Key];
      if not BackgroundScript.Script.IsDone then
      begin
        if (GetTickCount64 > BackgroundScript.LastTimestamp + BackgroundScript.Interval) then
        begin
          BackgroundScript.LastTimestamp := GetTickCount64;
          Self.BackgroundScriptDict[Key] := BackgroundScript;
          try
            BackgroundScript.Script.OptimizeAsserts := not Save.Settings.DeveloperMode;
            BackgroundScript.Script.Exec;
          except
            // Make sure runtime doesnt get blocked when an error occurs in background script
            on E: Exception do
            begin
              BackgroundScript.Script.Free;
              BackgroundScriptDict.Remove(Key);
              Error('Worker "' + Key + '": ' + E.Message);
            end;
          end;
        end;
      end else
      // Remove script if the job is completed
      begin
        BackgroundScript.Script.Free;
        BackgroundScriptDict.Remove(Key);
      end;
    end;
  except
    on E: Exception do
    begin
      ResetScript;
      IsAction := False;
      DumpExceptionCallStack(E);
      Error(E.Message);
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
  I: Integer;
begin
  ExecThread := TSataniaExecThread.Create(True);
  ExecThread.RunName := S;
  ThreadDict.AddOrSetValue(ExecThread.RunName, ExecThread);
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
  SpriteSetFilter(Self.SpriteAsSpine, S);
  SpriteSetFilter(Self.SpriteAsX3D, S);
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
      if V.Kind = sevkNumber then
        Result := PointFloatToStr(V.VarNumber)
      else
      if V.Kind = sevkString then
        Result := V.VarString^;
    except
      on E: Exception do;
    end;
  finally
    SE.Free;
  end;
end;

procedure TSatania.UpdateMenuItems;
var
  I, Anchor: Integer;
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
    // Global
    FindAllFiles(ScriptFiles, 'data/scripts/libs/menu', '*.evil', False);
    SetLength(MenuItems, ScriptFiles.Count + 1);
    for I := 0 to ScriptFiles.Count - 1 do
    begin
      S := ExtractFileName(ScriptFiles[I]);
      S := StringReplace(S, ExtractFileExt(S), '', [rfReplaceAll]);
      MenuItem := TMenuItem.Create(FormMain);
      MenuItem.Caption := S;
      MenuItem.OnClick := @FormMain.DoExecuteScriptFromMenuGlobal;
      FormMain.MenuItemActions.Add(MenuItem);
      MenuItems[I] := MenuItem;
    end;
    // Separator
    MenuItem := TMenuItem.Create(FormMain);
    MenuItem.Caption := '-';
    FormMain.MenuItemActions.Add(MenuItem);
    MenuItems[Length(MenuItems) - 1] := MenuItem;
    // Local
    ScriptFiles.Clear;
    FindAllFiles(ScriptFiles, 'data/scripts/' + Save.Settings.Skin + '/menu', '*.evil', False);
    Anchor := Length(MenuItems);
    SetLength(MenuItems, Anchor + ScriptFiles.Count);
    for I := 0 to ScriptFiles.Count - 1 do
    begin
      S := ExtractFileName(ScriptFiles[I]);
      S := StringReplace(S, ExtractFileExt(S), '', [rfReplaceAll]);
      MenuItem := TMenuItem.Create(FormMain);
      MenuItem.Caption := S;
      MenuItem.OnClick := @FormMain.DoExecuteScriptFromMenuLocal;
      FormMain.MenuItemActions.Add(MenuItem);
      MenuItems[Anchor + I] := MenuItem;
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

procedure TSatania.UpdateMeta(const S: TEvilC);
var
  Json, N: TJsonNode;
  SL: TStrings;
  MetaPath: String;
  IsNamed: Boolean = False;
  I: Integer;
begin
  S.ConstMap.Clear;
  S.AddDefaultConsts;
  S.ConstMap.AddOrSetValue('CHATMODE_CHAT', CHATMODE_CHAT);
  S.ConstMap.AddOrSetValue('CHATMODE_SCRIPT', CHATMODE_SCRIPT);
  S.ConstMap.AddOrSetValue('FA_DIRECTORY', faDirectory);
  S.ConstMap.AddOrSetValue('FA_READONLY', faReadOnly);
  S.ConstMap.AddOrSetValue('FA_NORMAL', faNormal);
  S.ConstMap.AddOrSetValue('FA_ENCRYPTED', faEncrypted);
  S.ConstMap.AddOrSetValue('FA_COMPRESSED', faCompressed);
  S.ConstMap.AddOrSetValue('FA_SYMLINK', faSymLink);
  S.ConstMap.AddOrSetValue('FA_SYSFILE', faSysFile);
  S.ConstMap.AddOrSetValue('FA_ANYFILE', faAnyFile);
  MetaPath := 'data/scripts/' + Save.Settings.Skin + '/meta.json';
  Name := 'Satania';
  if FileExists(MetaPath) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(MetaPath);
      Json := TJsonNode.Create;
      Json.TryParse(SL.Text);
      // We search for name
      for I := 0 to Json.Count - 1 do
      begin
        // Stop once we found name
        N := Json.Child(I);
        if N.Name = 'name' then
        begin
          IsNamed := True;
          Name := N.AsString;
          Break;
        end;
      end;
      Json.Free;
      // Create a new meta constant and map meta data there
      if SL.Text = '' then
        S.ConstMap.AddOrSetValue('meta', SEJSONParse(nil, ['{ "name": "' + Name + '" }']))
      else
        S.ConstMap.AddOrSetValue('meta', SEJSONParse(nil, [SL.Text]));
    finally
      SL.Free;
    end;
  end else
  begin
    S.ConstMap.AddOrSetValue('meta', SEJSONParse(nil, ['{ "name": "' + Name + '" }']))
  end;
  if not IsNamed then
  begin
    S.ConstMap.AddOrSetValue('charname', Name);
    S.ConstMap.AddOrSetValue('name', Name);
  end;
  S.ConstMap.AddOrSetValue('username', Save.Settings.UserName);
  S.ConstMap.AddOrSetValue('character', Save.Settings.Skin);
end;

procedure TSatania.CleanUpCache;
begin
  ScriptCacheMap.Clear;
end;

procedure TSatania.BackgroundScriptClearAll;
var
  Key: String;
begin
  for Key in Self.BackgroundScriptDict.Keys do
  begin
    if not BackgroundScriptDict[Key].IsPersistent then
      BackgroundScriptDict[Key].Script.Free;
  end;
  BackgroundScriptDict.Clear;
end;

procedure FreeLeftoverProcesses;
var
  Info  : TNonBlockProcessRec;
  Key   : String;
begin
  if RunProcessNonBlockResultList.Keys.Count > 0 then
  begin
    for Key in RunProcessNonBlockResultList.Keys do
    begin
      Info := RunProcessNonBlockResultList[Key];
      if Info.Process <> nil then
      begin
        Info.Process.Terminate(0);
        Info.Process.Free;
      end;
    end;
  end;
  FreeAndNil(RunProcessNonBlockResultList);
end; 

var
  Key: String;
  Thread: TThread;

initialization
  CSAction := TCriticalSection.Create;
  CSTalk := TCriticalSection.Create;
  Save := TSave.Create;
  Save.LoadFromFile('configs.json');

  Satania := TSatania.Create;
  RunHttpResultList := THttpResponseDict.Create;
  RunProcessResultList := TStringDict.Create;
  RunProcessNonBlockResultList := TNonBlockProcessDict.Create;
  ThreadDict := TThreadDict.Create;

finalization
  Save.SaveToFile('configs.json');
  FreeAndNil(Save);
  FreeAndNil(Satania);
  FreeAndNil(RunProcessResultList);
  for Key in ThreadDict.Keys do
  begin
    Thread := ThreadDict[Key];
    if Thread is TSataniaExecNonBlockThread then
      ExecuteProcess(FindDefaultExecutablePath('taskkill'), '-f -im ' + ExtractFileName(TSataniaExecNonBlockThread(Thread).ExeName), []);
  end;
  Sleep(1000);
  ThreadDict.Free;
  FreeLeftoverProcesses;
  CSTalk.Free;
  CSAction.Free;

end.

