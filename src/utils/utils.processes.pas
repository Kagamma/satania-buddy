unit Utils.Processes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  TermVT, UnTerminal;

procedure RunCommandWithoutAsciiEscapeCode(Cmd: String; var Res: String);

implementation

procedure RunCommandWithoutAsciiEscapeCode(Cmd: String; var Res: String);
var
  P: TConsoleProc;
begin
  P := TConsoleProc.Create(nil);
  try
    P.RunInLoop(Cmd, '', -1, Res);
  finally
    P.Free;
  end;
end;

end.

