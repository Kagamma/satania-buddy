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

function GetPhysFilePath(S: String): String;     
function GetPhysDirPath(S: String): String;
function GetOSLocalDir: String;

implementation

function GetOSLocalDir: String; inline;
begin
  {$ifdef WINDOWS}
  Result := GetUserDir + 'AppData\Local\satania-buddy\';
  {$else}
  Result := GetUserDir + '.config/satania-buddy/';
  {$endif}
end;

function GetPhysFilePath(S: String): String;
begin
  if not FileExists(S) then
  begin
    S := GetOSLocalDir + S;
  end;
  Result := S;
end;               

function GetPhysDirPath(S: String): String;
begin
  if not DirectoryExists(S) then
  begin
    S := GetOSLocalDir + S;
  end;
  Result := S;
end;

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
    PhysicalUrl := GetOSLocalDir + PhysicalUrl;
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

