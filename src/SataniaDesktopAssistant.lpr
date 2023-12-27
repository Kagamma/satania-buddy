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

program SataniaBuddy;

{$I configs.inc}

uses
  //mormot.core.fpcx64mm,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  LCLPlatformDef,
  Interfaces, // this includes the LCL widgetset
  FrameViewer09, opensslsockets, globals, Forms, datetimectrls, anchordockpkg,
  utils.activewindow, mcdowell, State.Main, form.settings, form.chat,
  form.tool.evilceditor,
  mcdowell.imap, Mcdowell.SpeechToText, frame.reminders.item, form.reminders,
  form.Main, form.touch, mcdowell.sound, form.rules, frame.rules.item,
  frame.rules.edititem, mcdowell.chat, mcdowell.net, mcdowell.numbers,
  utils.strings, utils.encdec, utils.coords, utils.threads, utils.files,
  mcdowell.smtp, utils.smartptr, 
Com.WebApp, Com.Brokers, form.ask, utils.sprites,
  mcdowell.sketch, form.tool.hexeditor, Mcdowell.RichText, Form.Bubble,
  Utils.Colors, CopyDir, Vosk, BassAudioSource, SttThread, whisper,
  Mcdowell.Data, Form.Tool.StackViewer;

{$R *.res}

begin
  Randomize;
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormTouch, FormTouch);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormBubble, FormBubble);
  Application.CreateForm(TFormChat, FormChat);
  Application.CreateForm(TFormReminders, FormReminders);
  Application.CreateForm(TFormRules, FormRules);
  Application.CreateForm(TFormAsk, FormAsk);
  Application.CreateForm(TFormHexEditor, FormHexEditor);
  Application.CreateForm(TFormStackViewer, FormStackViewer);
  Application.Run;
 end.

