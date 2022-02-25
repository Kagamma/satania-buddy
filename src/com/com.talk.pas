unit Com.Talk;

{$I configs}

interface

uses
  Classes, SysUtils,
  BrookAction;

type
  TTalkAction = class(TBrookAction)
  public
    function Process: string;
    procedure Get; override;
    procedure Post; override;
  end;

implementation

function TTalkAction.Process: string;
begin
  Result := 'Hello!';
end;

procedure TTalkAction.Get;
begin
  Write(Process);
end;

procedure TTalkAction.Post;
begin
  Write(Process);
end;

initialization
  TTalkAction.Register('/talk');

end.

