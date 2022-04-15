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

