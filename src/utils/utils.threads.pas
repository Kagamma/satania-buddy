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

unit Utils.Threads;

{$I configs.inc}

interface

uses
  Classes, SysUtils, fphttpclient;

type
  TMethod = procedure of object;
  TCommonThread = class(TThread)
  protected   
    ErrorMessage: String;
    procedure ResetToDefault;
  public
    Method: TMethod;
    procedure Execute; override;
  end;

  TDownloadThread = class(TThread)
  protected
    ErrorMessage: String;
    procedure ResetToDefault;
  public
    Stream: TStream;
    URL: String;
    procedure Execute; override;
  end;

procedure CommonThread(Method: TMethod); inline;     
procedure DownloadThread(URL: String; var Stream: TStream); inline;

implementation

uses
  Mcdowell;

// TCommonThread

procedure TCommonThread.ResetToDefault;
begin
  Satania.TalkReset(ErrorMessage);
end;

procedure TCommonThread.Execute;
begin
  try
    Method;
    Terminate;
  except
    on E: Exception do
    begin
      ErrorMessage := E.Message;
      Synchronize(@Self.ResetToDefault);
    end;
  end;
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

// TDownloadThread

procedure TDownloadThread.ResetToDefault;
begin
  Satania.TalkReset(ErrorMessage);
end;

procedure TDownloadThread.Execute;
var
  Client: TFPHTTPClient;
begin
  Client := TFPHTTPClient.Create(nil);
  try
    try
      Client.Get(URL, Stream);
      Terminate;
    except
      on E: Exception do
      begin
        ErrorMessage := E.Message;
        Synchronize(@Self.ResetToDefault);
      end;
    end;
  finally
    Client.Free;
  end;
end;

procedure DownloadThread(URL: String; var Stream: TStream);
var
  T: TDownloadThread;
begin
  Stream := TMemoryStream.Create;
  T := TDownloadThread.Create(True);
  T.FreeOnTerminate := True;
  T.URL := URL;
  T.Stream := Stream;
  T.Start;
end;

end.

