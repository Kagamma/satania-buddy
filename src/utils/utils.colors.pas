unit Utils.Colors;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleUtils, CastleColors, CastleLCLUtils, Graphics,
  CastleVectors;

function IsDarkTheme: Boolean;
function CColor(const Color: Cardinal): Cardinal;

implementation

function IsDarkTheme: Boolean; inline;
var
  Luminance: Single;
begin
  Luminance := GrayscaleValue(ColorToVector3(clForm));
  Result := Luminance < 180 / 255;
end;

function CColor(const Color: Cardinal): Cardinal; inline;
var
  C: TVector3Byte;
begin
  {if IsDarkTheme then
  begin
    C := ColorToVector3Byte(Color);
    C.X := 255 - C.X;
    C.Y := 255 - C.Y;
    C.Z := 255 - C.Z;
    Result := RGBToColor(C.X, C.Y, C.Z);
  end else}
    Result := Color;
end;

end.

