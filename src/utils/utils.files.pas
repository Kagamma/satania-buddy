unit Utils.Files;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Types, StrUtils;

function LookForFileInPath(const Name: String): String;

implementation      

function LookForFileInPath(const Name: String): String;
var
  Paths, Path, S: String;
  PathArray: TStringDynArray;
  I: Integer;
begin
  Paths := GetEnvironmentVariable('PATH');
  PathArray := SplitString(Paths, ';');
  for I := 0 to Length(PathArray) - 1 do
  begin
    S := StringReplace(PathArray[I], '\', '/', [rfReplaceAll]) + '/' + Name;
    S := StringReplace(S, '//', '/', [rfReplaceAll]);
    PathArray[I] := S;
  end;
  for Path in PathArray do
  begin
    if FileExists(S) then
    begin
      Exit(S);
    end;
    if FileExists(S + '.exe') then
    begin
      Exit(S);
    end;
  end;
  Exit(Name);
end;

end.

