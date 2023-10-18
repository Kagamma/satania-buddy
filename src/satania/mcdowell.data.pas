unit Mcdowell.Data;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, CastleURIUtils;

type
  TSataniaDataClass = class
    class function Read(const Url: String; out MimeType: String): TStream;
    class function Write(const Url: String): TStream;
  end;

implementation

class function TSataniaDataClass.Read(const Url: String; out MimeType: String): TStream;
var
  F: TFileStream;
  RealUrl, PhysicalUrl: String;
begin
  RealUrl := StringReplace(Url, 's-data://', '', []);
  RealUrl := StringReplace(RealUrl, 's-data:/', '', []);
  PhysicalUrl := 'data/' + RealUrl;
  if not FileExists(PhysicalUrl) then
  begin
    PhysicalUrl := GetAppConfigDir(True) + PhysicalUrl;
    if not FileExists(PhysicalUrl) then
      raise Exception(Format('File "%s" not found!', [PhysicalUrl]));
  end;
  F := TFileStream.Create(PhysicalUrl, fmOpenRead);
  MimeType := URIMimeType(PhysicalUrl);
  Result := F;
end;

class function TSataniaDataClass.Write(const Url: String): TStream;
var
  FileName: String;
begin
  FileName := URIToFilenameSafe(URL);
  if FileName = '' then
    raise Exception.CreateFmt('Cannot convert URL to a filename: "%s"', [URL]);
  Result := TFileStream.Create(FileName, fmCreate);
end;

end.

