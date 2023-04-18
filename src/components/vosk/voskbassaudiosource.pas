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

unit voskbassaudiosource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, voskthread, bass;

type

  { TBassAudioSource }

  TBassAudioSource = class(TVoskAudioSource)
  private
    FBassInitialized: Boolean;
    FChan: HRECORD;
    FInputSourceIdx: Integer;
    FInputSources: TStringList;
    function InitBASS(ADeviceIdx: Integer): Boolean;
    procedure FreeBASS;
    procedure ListInputSources;
    procedure SetInputSourceIdx(ANewIdx: Integer);
  public
    property InputSources: TStringList read FInputSources;
    property InputSourceIdx: Integer read FInputSourceIdx write SetInputSourceIdx;

    function GetData(buffer: Pointer; nmax: Cardinal): Integer; override;

    constructor Create(ADeviceIdx: Integer);
    destructor Destroy; override;
  end;

function GetMicrophoneDeviceIdx: Integer;

implementation

function HiWord(l: LongInt): Word;
begin
  Result := Word(((DWord(l)) shr 16) and $FFFF);
end;

{ TBassAudioSource }

function GetMicrophoneDeviceIdx: Integer;
var
  I: Integer;
  info: BASS_DEVICEINFO;
begin
  Result := -1;
  I := 0;
  while BASS_RecordGetDeviceInfo(I, info) do
  begin
    if ((info.flags and BASS_DEVICE_ENABLED) = BASS_DEVICE_ENABLED) and
       ((info.flags and BASS_DEVICE_TYPE_MASK) = BASS_DEVICE_TYPE_MICROPHONE) then
    begin
      Result := I;
      Break;
    end;

    Inc(I);
  end;
end;

function TBassAudioSource.InitBASS(ADeviceIdx: Integer): Boolean;
begin
  Result := False;

  // Check if correct version of BASS.DLL was loaded
  if (HiWord(BASS_GetVersion) <> BASSVERSION) then
    Exit;

  // Initialize audio device - 16000hz, mono, 16 bits
  Result := BASS_RecordInit(ADeviceIdx) and
    BASS_Init(ADeviceIdx, 16000, BASS_DEVICE_MONO or BASS_DEVICE_FREQ, {$ifdef WINDOWS}GetCurrentThreadID{$else}Pointer(GetCurrentThreadID){$endif}, nil);

  FBassInitialized := Result;
end;

procedure TBassAudioSource.FreeBASS;
begin
  BASS_Stop;
  BASS_RecordFree;
  BASS_Free;
  FBassInitialized := False;
end;

procedure TBassAudioSource.ListInputSources;
var
  I: Integer;
  inName: PAnsiChar;
  level: Single;
begin
  I := 0;
  inName := BASS_RecordGetInputName(I);
  while inName <> nil do
  begin
    FInputSources.Add(StrPas(inName));
    if (BASS_RecordGetInput(I, level) and BASS_INPUT_OFF) = 0 then
      FInputSourceIdx := I;
    Inc(I);
    inName := BASS_RecordGetInputName(I);
  end;
end;

procedure TBassAudioSource.SetInputSourceIdx(ANewIdx: Integer);
var
  I: Integer;
  R: Boolean;
begin
  FInputSourceIdx := ANewIdx;

  R := True;
  I := 0;
  while R do
  begin
    R := BASS_RecordSetInput(I, BASS_INPUT_OFF, -1);
    Inc(I);
  end;
  BASS_RecordSetInput(FInputSourceIdx, BASS_INPUT_ON, -1);
end;

function TBassAudioSource.GetData(buffer: Pointer; nmax: Cardinal): Integer;
begin
  Result := Integer(BASS_ChannelGetData(FChan, buffer, nmax * SizeOf(Int16)));

  if Result < 0 then
  begin
    if BASS_ErrorGetCode = BASS_ERROR_ENDED then
      Result := 0;
  end else
    Result := Result div 2;
end;

constructor TBassAudioSource.Create(ADeviceIdx: Integer);
begin
  FReady := False;
  FInputSourceIdx := -1;
  FInputSources := TStringList.Create;

  if InitBASS(ADeviceIdx) then
  begin
    ListInputSources;
    FChan := BASS_RecordStart(16000, 1, 0, nil, nil);
    FReady := FChan <> 0;
  end;
end;

destructor TBassAudioSource.Destroy;
begin
  FreeBASS;
  FInputSources.Free;
  inherited Destroy;
end;

end.


