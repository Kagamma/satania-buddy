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

unit mcdowell.sketch;

{$I configs.inc}

interface

uses
  Classes, SysUtils,
  CastleVectors, CastleTransform, CastleScene, CastleGLShaders, CastleApplicationProperties,
  CastleRenderContext, X3DLoad,
  globals, Utils.Encdec;

type
  TSataniaSketchItem = class(TCastleScene)
  public
    IsRemoved: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

  TSataniaSketch = class
  public
    { Create new sketch with triangles }
    function CreateSketch(const AName: String): TSataniaSketchItem;
    { Find a sketch by name, return nil if none is found }
    function Find(const AName: String): TSataniaSketchItem;
    { Load X3D }
    function LoadFromText(const AName, AText, AType: String): TSataniaSketchItem;
    { Delete sketch by name, return true if delete successfully }
    function Delete(const AName: String): Boolean;
    { Delete all sketches }
    procedure DeleteAll;
  end;

var
  SataniaSketch: TSataniaSketch;

implementation

uses
  mcdowell;

// ----- TSataniaSketchItem -----

constructor TSataniaSketchItem.Create(AOwner: TComponent);
begin
  inherited;
  Self.ProcessEvents := True;
end;

destructor TSataniaSketchItem.Destroy;
begin
  inherited;
end;

procedure TSataniaSketchItem.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  Self.Scale := Vector3(Globals.Save.Settings.BaseScaling, Globals.Save.Settings.BaseScaling, Globals.Save.Settings.BaseScaling);
  inherited;
  if not Self.Exists then
    Exit;
  if IsRemoved then
    RemoveMe := rtRemoveAndFree
  else
    RemoveMe := rtNone;
end;

// ----- TSataniaSketch -----

function TSataniaSketch.CreateSketch(const AName: String): TSataniaSketchItem;
begin
  Result := Self.Find(AName);
  if Result = nil then
  begin
    Result := TSataniaSketchItem.Create(Satania.SketchAfter);
    Result.Name := AName;
    Satania.SketchBefore.Add(Result);
  end;
end;

function TSataniaSketch.Find(const AName: String): TSataniaSketchItem;
begin
  Result := TSataniaSketchItem(Satania.SketchBefore.FindComponent(AName));
  if Result = nil then
    Result := TSataniaSketchItem(Satania.SketchAfter.FindComponent(AName));
end;

function TSataniaSketch.LoadFromText(const AName, AText, AType: String): TSataniaSketchItem;
var
  SS: TStringStream;
  S: String;
begin
  Result := CreateSketch(AName);
  SS := TStringStream.Create(AText);
  try
    try
      SS.Position := 0;
      S := 'castle-data:/temp' + GUIDName + '.' + AType;
      case LowerCase(AType) of
        'x3d':
          Result.Load(LoadNode(SS, S, 'model/x3d+xml'), True);
        'x3dv', 'wrl':
          Result.Load(LoadNode(SS, S, 'model/x3d+vrml'), True);
      end;
    except
      on E: Exception do
        Satania.Log(E.Message);
    end;
  finally
    SS.Free;
  end;
end;

function TSataniaSketch.Delete(const AName: String): Boolean;
var
  Item: TSataniaSketchItem;
begin
  Item := Self.Find(AName);
  if Item <> nil then
  begin
    Item.Free;
    Result := True;
  end else
    Result := False;
end;

procedure TSataniaSketch.DeleteAll;
var
  I: Integer;
begin
  for I := Satania.SketchBefore.Count - 1 downto 0 do
    Satania.SketchBefore.Items[I].Free;
  for I := Satania.SketchAfter.Count - 1 downto 0 do
    Satania.SketchAfter.Items[I].Free;
end;

initialization
  SataniaSketch := TSataniaSketch.Create;

finalization
  SataniaSketch.Free;

end.
