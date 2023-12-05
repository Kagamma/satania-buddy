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

unit Com.Brokers;

{$I configs.inc}

interface

uses
  Classes, SysUtils, fphttpapp;

type
  TWebAppThread = class(TThread)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;

procedure EmbeddedServerStart;

var
  WebAppThread: TWebAppThread;

implementation

uses
  Globals,
  Mcdowell;

procedure EmbeddedServerStart;
begin
  if WebAppThread = nil then
  begin
    Application.Port := Save.Settings.EmbeddedServerPort;
    Application.Threaded := False;
    Application.Initialize;
    WebAppThread := TWebAppThread.Create;
    WebAppThread.Start;
  end;
end;

procedure EmbeddedServerStop;
begin
  if WebAppThread <> nil then
    WebAppThread.Terminate;
  WebAppThread := nil;
end;

constructor TWebAppThread.Create;
begin
  inherited Create(False);
  FreeOnTerminate := true;
end;

procedure TWebAppThread.Execute;
begin
  try
    Application.Run;
  except
    on E: Exception do
    begin
      Satania.Error(E.Message);
      DumpExceptionCallStack(E);
    end;
  end;
end;

destructor TWebAppThread.Destroy;
begin
  Application.Terminate;
  inherited;
end;

initialization
  WebAppThread := nil;

finalization
  EmbeddedServerStop;

end.

