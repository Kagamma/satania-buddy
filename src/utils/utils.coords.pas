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

unit Utils.Coords;

{$I configs.inc}

interface

uses
  Classes, SysUtils, CastleVectors;

var
  ScreenWidth, ScreenHeight: Integer;

function UIToScreenCoord(const V: TVector2): TVector2Integer; overload; inline;
function UIToScreenCoord(const V: TVector3): TVector2Integer; overload; inline;
function UIToScreenCoord(const S: Single): Integer; inline;
function ScreenCoordToUI(const V: TVector2): TVector2; inline;

implementation

uses
  CastleWindow;

function UIToScreenCoord(const V: TVector2): TVector2Integer;
begin
  Result := Vector2Integer(Round(V.X), Round(Application.ScreenHeight - V.Y));
end;

function UIToScreenCoord(const V: TVector3): TVector2Integer;
begin
  Result := Vector2Integer(Round(V.X), Round(Application.ScreenHeight - V.Y));
end; 

function UIToScreenCoord(const S: Single): Integer; inline;
begin
  Result := Round(Application.ScreenHeight - S);
end;

function ScreenCoordToUI(const V: TVector2): TVector2;
begin
  Result := Vector2(V.X, Application.ScreenHeight - V.Y);
end;

end.

