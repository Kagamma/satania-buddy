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

program SataniaBuddy;

{$I configs.inc}

uses
  mormot.core.fpcx64mm,
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  opensslsockets, globals, Forms, datetimectrls, utils.activewindow, mcdowell,
  State.Main, form.settings, form.chat, mcdowell.chatbot,
  mcdowell.chatbot.train, form.evilc.editor, mcdowell.imap,
  Mcdowell.SpeechToText, frame.reminders.item, form.reminders, form.Main,
  form.touch, mcdowell.sound, form.rules, frame.rules.item,
  frame.rules.edititem, vosk, voskthread, voskbassaudiosource, mcdowell.chat,
  mcdowell.net, mcdowell.numbers, utils.strings, utils.encdec, utils.coords,
  utils.threads, utils.files, mcdowell.smtp, form.email.editor, utils.smartptr,
  form.chatbubble, utils.htmltext;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.CreateForm(TFormTouch, FormTouch);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormChat, FormChat);
  Application.CreateForm(TFormEvilCEditor, FormEvilCEditor);
  Application.CreateForm(TFormReminders, FormReminders);
  Application.CreateForm(TFormRules, FormRules);
  Application.CreateForm(TFormEmailEditor, FormEmailEditor);
  Application.CreateForm(TFormChatBubble, FormChatBubble);
  Application.Run;
end.

