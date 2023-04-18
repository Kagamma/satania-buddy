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

unit mcdowell.imap;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Globals, imapsend, mimemess, mimepart, ssl_openssl,
  Generics.Collections;

type
  TSataniaIMAP = class
  private
    Imap: TIMAPSend;
    FolderList: TStringList;
    FIsRunning: Boolean;
    FIsSuccess: Boolean;
    procedure GetMessagesParallel;
  public
    MailList: TMailList;
    constructor Create;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure GetMessages;
    function IsEmailConfigured: Boolean;
    property IsRunning: Boolean read FIsRunning;
    property IsSuccess: Boolean read FIsSuccess;
  end;

var
  SataniaIMAP: TSataniaIMAP;

implementation

uses
  Utils.Threads,
  Utils.Encdec;

procedure TSataniaIMAP.GetMessagesParallel;
var
  I, J, Count, SubPartCount, Ind: Integer;
  Mail: TMailRec;
  MimePart: TMimePart;
  MimeMess: TMimeMess;
  MailUniqueList: TStringList;
  S: String;
  IsMixedHtml: Boolean = False;
begin
  FIsRunning := True;
  if Imap = nil then
    Connect;
  MailUniqueList := TStringList.Create;
  MailUniqueList.Sorted := True;
  if FIsSuccess then
  begin
    FolderList.Clear;
    MailList.Clear;
    Imap.List(Save.Settings.EmailFetchFrom, FolderList);
    for I := 0 to FolderList.Count - 1 do
    begin
      Count := Imap.StatusFolder(FolderList[I], 'UNSEEN');
      if Count > 0 then
      begin
        Imap.SelectROFolder(FolderList[I]);
        for J := Imap.SelectedCount - Imap.SelectedRecent to Imap.SelectedCount do
        begin
          MimeMess := TMimeMess.Create;
          Imap.FetchMess(J, MimeMess.Lines);
          try
            MimeMess.DecodeMessage;
            // Do not process duplicate emails
            if MailUniqueList.Find(MimeMess.Header.MessageID, Ind) then
              continue; 
            MailUniqueList.Add(MimeMess.Header.MessageID);
            Mail.Sender := MimeMess.Header.From;
            Mail.Subject := MimeMess.Header.Subject;
            Mail.Body := '';
            SubPartCount := MimeMess.MessagePart.GetSubPartCount();
            if SubPartCount > 0 then
            begin
              for Count := 0 to SubPartCount - 1 do
              begin
                MimePart := MimeMess.MessagePart.GetSubPart(Count);
                MimePart.DecodePart;
                if MimePart.Secondary = 'HTML' then
                  IsMixedHtml := True;
              end;
              for Count := 0 to SubPartCount - 1 do
              begin
                MimePart := MimeMess.MessagePart.GetSubPart(Count);
                if (IsMixedHtml and (MimePart.Secondary = 'HTML')) or (not IsMixedHtml) then
                begin
                  Setlength(S, MimePart.DecodedLines.Size);
                  MimePart.DecodedLines.Read(S[1], Length(S));
                  Mail.Body := Mail.Body + S + #10;
                end;
              end
            end else
              Mail.Body := MimeMess.MessagePart.Lines.Text;
          except
            on E: Exception do;
          end;
          MimeMess.Free;
          MailList.Add(Mail);
        end;
      end;
    end;
  end;
  MailUniqueList.Free;
  FIsRunning := False;
end;

constructor TSataniaIMAP.Create;
begin
  inherited;
  MailList := TMailList.Create;
  FolderList := TStringList.Create;
end;

destructor TSataniaIMAP.Destroy;
begin
  Disconnect;
  FreeAndNil(MailList);
  FreeAndNil(FolderList);
  inherited;
end;

procedure TSataniaIMAP.GetMessages;
begin
  if FIsRunning then
    Exit;
  FIsRunning := True;
  CommonThread(@GetMessagesParallel);
end;

procedure TSataniaIMAP.Connect;
begin
  FIsSuccess := False;
  Disconnect;
  if not IsEmailConfigured then
    Exit;
  Imap := TIMAPSend.create;

  Imap.TargetHost := Save.Settings.EmailServer;
  Imap.TargetPort := IntToStr(Save.Settings.EmailPort);
  Imap.UserName := Save.Settings.EmailUsername;
  Imap.Password := Decrypt(Save.Settings.EmailPassword);
  Imap.FullSSL := Save.Settings.EmailUseSSL;
  if Imap.Login then
  begin
    FIsSuccess := True;
  end;
end;

procedure TSataniaIMAP.Disconnect;
begin
  if Imap <> nil then
  begin
    FreeAndNil(Imap);
  end;
end;

function TSataniaIMAP.IsEmailConfigured: Boolean;
begin
  if (Save.Settings.EmailServer = '')
    or (Save.Settings.EmailPassword = '')
    or (Save.Settings.EmailUsername = '') then
    Exit(False);
  Exit(True);
end;

initialization
  SataniaIMAP := TSataniaIMAP.Create;

finalization
  FreeAndNil(SataniaIMAP);

end.

