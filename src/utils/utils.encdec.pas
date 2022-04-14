unit Utils.Encdec;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Blowfish, base64, StrUtils;

function Encrypt(S: String): String; inline;
function Decrypt(S: String): String; inline;
function GUID: String; inline;
function GUIDName: String; inline;

implementation

uses
  globals;

function Encrypt(S: String): String;
var
  EncrytpStream: TBlowFishEncryptStream;
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  EncrytpStream := TBlowFishEncryptStream.Create(SECRET_KEY, StringStream);
  EncrytpStream.WriteAnsiString(S);
  EncrytpStream.Free;
  Result := EncodeStringBase64(StringStream.DataString);
  StringStream.Free;
end;

function Decrypt(S: String): String;
var
  DecrytpStream: TBlowFishDeCryptStream;
  StringStream: TStringStream;
begin
  S := DecodeStringBase64(S);
  StringStream := TStringStream.Create(S);
  DecrytpStream := TBlowFishDeCryptStream.Create(SECRET_KEY, StringStream);
  Result := DecrytpStream.ReadAnsiString;
  DecrytpStream.Free;
  StringStream.Free;
end;

function GUID: String;
var
  GUIDRec: TGUID;
begin
  CreateGUID(GUIDRec);
  Result := GUIDToString(GUIDRec);
end;

function GUIDName: String;
begin
  Result := 'G' + StringsReplace(GUID, ['{', '}', '-'], ['', '', ''], [rfReplaceAll]);
end;

end.

