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

unit Utils.Files;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Types, StrUtils;

function LookForFileInPath(const Name: String): String;
function ReadFileAsString(const Name: String): String; overload;
procedure ReadFileAsString(const Name: String; var Str: String); overload;

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

function ReadFileAsString(const Name: String): String;
var
  MS: TMemoryStream;
begin
  if not FileExists(Name) then
    Exit;
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(Name);
    if MS.Size > 0 then
    begin
      SetLength(Result, MS.Size div SizeOf(Char));
      MS.ReadBuffer(Pointer(Result)^, MS.Size div SizeOf(Char));
    end;
  finally
    MS.Free;
  end;
end;

procedure ReadFileAsString(const Name: String; var Str: String);
var
  MS: TMemoryStream;
begin
  if not FileExists(Name) then
    Exit;
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(Name);
    if MS.Size > 0 then
    begin
      SetLength(Str, MS.Size div SizeOf(Char));
      MS.ReadBuffer(Pointer(Str)^, MS.Size div SizeOf(Char));
    end;
  finally
    MS.Free;
  end;
end;

end.

