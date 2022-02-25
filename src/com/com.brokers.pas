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

