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
  Classes, SysUtils, BrookFCLHttpAppBroker, BrookUtils, BrookApplication;

type
  TBrookThread = class(TThread)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;

procedure EmbeddedServerStart;

var
  BrookThread: TBrookThread;

implementation

uses
  Globals,
  Mcdowell;

procedure EmbeddedServerStart;
begin
  BrookSettings.Port := Save.Settings.EmbeddedServerPort;
  TBrookHTTPApplication(BrookApp.Instance).Threaded := False;
  BrookThread := TBrookThread.Create;
  BrookThread.Start;
end;

constructor TBrookThread.Create;
begin
  inherited Create(False);
  FreeOnTerminate := true;
end;

procedure TBrookThread.Execute;
begin
  try
    BrookApp.Run;
  except
    on E: Exception do
      Satania.Error(E.Message);
  end;
end;

destructor TBrookThread.Destroy;
begin
  BrookApp.Terminate;
  inherited;
end;

initialization
  BrookThread := nil;

finalization
  if BrookThread <> nil then
    BrookThread.Terminate;

end.

