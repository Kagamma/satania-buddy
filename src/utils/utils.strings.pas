{

satania-buddy
Copyright (C) 2022-2024 kagamma

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

unit Utils.Strings;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Types, StrUtils, CastleUnicode;

function PointStrToFloat(S: String): Double; inline;
function PointFloatToStr(X: Double): String; inline;

function CharsetToSettings(S: String): TStringDynArray;
function SettingsToCharset(L: TStringList): String;
function CharsetToCharacters(S: String): String;
function Grep(Src, S: String): String;

implementation   

function PointStrToFloat(S: String): Double;
var
  fS: TFormatSettings;
begin
  FS := FormatSettings;
  fS.DecimalSeparator := '.';
  Result := StrToFloat(S, FS);
end;

function PointFloatToStr(X: Double): String;
var
  FS: TFormatSettings;
begin
  FS := FormatSettings;
  FS.DecimalSeparator := '.';
  Result := FloatToStr(X, FS);
end;    

function CharsetToSettings(S: String): TStringDynArray;
begin
  Result := SplitString(S, #10#13);
end;

function SettingsToCharset(L: TStringList): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to L.Count - 1 do
  begin
    Result := Result + L[I];
    if I < L.Count - 1 then
      Result := Result + #10#13;
  end;
end;

function CharsetToCharacters(S: String): String;
var
  X, Y, I: Cardinal;
  W: String;
  SS: String = '';
  L, P: TStringDynArray;
begin
  L := CharsetToSettings(S);
  for S in L do
    if Trim(S) <> '' then
    begin
      P := SplitString(S, '..');
      if Length(P) = 3 then
      begin
        X := StrToInt(P[0]);
        Y := StrToInt(P[2]);
        W := '';
        for I := X to Y do
        begin
          W := W + UnicodeToUTF8(I);
        end;
        SS := SS + W;
      end;
    end;
  Result := SS;
end;

function Grep(Src, S: String): String;
var
  A: TStringDynArray;
  V: String;
begin
  Result := '';
  A := SplitString(Src, #10);
  for V in A do
    if V.IndexOf(S) >= 0 then
    begin
      if Result = '' then
        Result := V
      else
        Result := Result + #10 + V;
    end;
end;

end.

