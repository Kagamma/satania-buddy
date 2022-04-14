unit Utils.Threads;

{$I configs.inc}

interface

uses
  Classes, SysUtils;

type
  TMethod = procedure of object;
  TCommonThread = class(TThread)
    Method: TMethod;
    procedure Execute; override;
  end;

procedure CommonThread(Method: TMethod); inline;

implementation

uses
  Mcdowell;

procedure TCommonThread.Execute;
begin
  try
    Method;
    Terminate;
  except
    on E: Exception do
      Satania.Talk(E.Message);
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

end.

