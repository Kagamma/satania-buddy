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

unit Com.Brokers;

{$I configs.inc}

interface

uses
  Classes, BrookFCLHttpAppBroker, BrookUtils, BrookApplication;

type
  TBrookThread = class(TThread)
  public
    constructor Create;
    procedure Execute; override;
  end;

var
  BrookThread: TBrookThread;

implementation

constructor TBrookThread.Create;
begin
  inherited Create(false);
  FreeOnTerminate := true;
end;

procedure TBrookThread.Execute;
begin
  BrookApp.Run;
end;

initialization
  BrookSettings.Port := 9999;
  TBrookHTTPApplication(BrookApp.Instance).Threaded := false;
  BrookThread := TBrookThread.Create;

end.

