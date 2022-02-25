program SataniaBuddy;

{$I configs.inc}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Form.Main, utils.activewindow,
  mcdowell, State.Main, Form.Touch, globals, form.settings, form.chat,
  mcdowell.chatbot, mcdowell.chatbot.train, 
  form.evilc.editor, mcdowell.imap;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;           
  Application.Scaled:=False;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormTouch, FormTouch);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormChat, FormChat);
  Application.CreateForm(TFormEvilCEditor, FormEvilCEditor);
  Application.Run;
end.

