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

unit mcdowell.smtp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, blcksock, smtpsend, mimemess, mimepart, globals, types,
  StrUtils;

type
  TSataniaSMTP = class(TThread)
  private
    Smtp: TSMTPSend;
    FSender,
    FReplyTo,
    FMailTo,
    FSubject,
    FBody: String;
    FTalk: String;
    procedure Talk;
    procedure Execute; override;
  public
    Attachments: TStringDynArray;
    constructor Create;
    destructor Destroy; override;
    class function IsEmailConfigured: Boolean;

    property Sender: String read FSender write FSender;
    property MailTo: String read FMailTo write FMailTo;
    property ReplyTo: String read FReplyTo write FReplyTo;
    property Subject: String read FSubject write FSubject;
    property Body: String read FBody write FBody;
  end;

implementation

uses
  Mcdowell,
  Utils.Threads,
  Utils.Encdec;

class function TSataniaSMTP.IsEmailConfigured: Boolean;
begin
  if (Save.Settings.EmailSMTPServer = '')
    or (Save.Settings.EmailSMTPPassword = '')
    or (Save.Settings.EmailSMTPUsername = '') then
    Exit(False);
  Exit(True);
end;

constructor TSataniaSMTP.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Self.Smtp := TSMTPSend.Create;
  Self.Smtp.TargetHost := Save.Settings.EmailSmtpServer;
  Self.Smtp.TargetPort := IntToStr(Save.Settings.EmailSmtpPort);
  Self.Smtp.UserName := Save.Settings.EmailSmtpUsername;
  Self.Smtp.Password := Decrypt(Save.Settings.EmailSmtpPassword);
  Self.Smtp.FullSSL := Save.Settings.EmailSmtpUseSSL;
end;

destructor TSataniaSMTP.Destroy;
begin
  Self.Smtp.Free;
  inherited;
end;

procedure TSataniaSMTP.Talk;
begin
  Satania.Talk(Self.FTalk);
end;

procedure TSataniaSMTP.Execute;
var
  SL: TStringList;
  Mime: TMimeMess;
  P : TMimePart;
  B : Boolean;
  S: String;
begin
  Mime := TMimeMess.Create;
  SL := TStringList.Create;
  try
    Mime.Header.ToList.Text := Self.FMailTo;
    Mime.Header.ReplyTo := Self.FReplyTo;
    Mime.Header.Subject := Self.FSubject;
    Mime.Header.From := Self.FSender;
    P := Mime.AddPartMultipart('mixed', nil);
    SL.Text := Self.FBody;
    Mime.AddPartHTML(SL, P);
    for S in Self.Attachments do
      Mime.AddPartBinaryFromFile(S, P);
    Mime.EncodeMessage;
    if not Self.Smtp.Login then
    begin
      FTalk := 'SMTP ERROR: Login: ' + Self.Smtp.EnhCodeString;
      Self.Synchronize(@Self.Talk);
      Exit;
    end;
    if not Self.Smtp.MailFrom(Save.Settings.EmailSmtpUsername, Length(Save.Settings.EmailSmtpUsername)) then
    begin
      FTalk := 'SMTP ERROR: MailFrom: ' + Self.Smtp.EnhCodeString;
      Self.Synchronize(@Self.Talk);
      Exit;
    end;
    for S in SplitString(Self.FMailTo, ';') do
      if not Self.Smtp.MailTo(S) then
      begin
        FTalk := 'SMTP ERROR: MailTo: ' + Self.Smtp.EnhCodeString;
        Self.Synchronize(@Self.Talk);
        Exit;
      end;
    if not Self.Smtp.MailData(Mime.Lines) then
    begin
      FTalk := 'SMTP ERROR: MailData: ' + Self.Smtp.EnhCodeString;
      Self.Synchronize(@Self.Talk);
      Exit;
    end;
    if not Self.Smtp.Logout then
    begin
      FTalk := 'SMTP ERROR: Logout: ' + Self.Smtp.EnhCodeString;
      Self.Synchronize(@Self.Talk);
      Exit;
    end;
    FTalk := 'E-mail sent OK';
    Self.Synchronize(@Self.Talk);
  finally
    Mime.Free;
    SL.Free;
  end;
end;

end.

