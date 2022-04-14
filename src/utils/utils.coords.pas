unit Utils.Coords;

{$I configs.inc}

interface

uses
  Classes, SysUtils, CastleVectors;

var
  ScreenWidth, ScreenHeight: Integer;

function UIToScreenCoord(const V: TVector2): TVector2Integer; overload; inline;
function UIToScreenCoord(const V: TVector3): TVector2Integer; overload; inline;
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

function ScreenCoordToUI(const V: TVector2): TVector2;
begin
  Result := Vector2(V.X, Application.ScreenHeight - V.Y);
end;

end.

