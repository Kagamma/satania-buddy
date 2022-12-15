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

unit mcdowell.sketch;

{$I configs.inc}

interface

uses
  Classes, SysUtils, GL, GLExt,
  CastleVectors, CastleTransform, CastleGLShaders, CastleApplicationProperties,
  CastleRenderContext, CastleGLImages,
  globals;

type
  TSataniaSketchData = record
    Vertex: TVector2;
    TexCoord: TVector2;
    Color: TVector4;
  end;
  TSataniaSketchDataArray = array of TSataniaSketchData;

  TSataniaSketchItem = class(TCastleTransform)
  private
    VBO: GLuint;
    FSketchData: array of TSataniaSketchData;
    FSketchDataPreviousLength: Integer;
    FTexture: String;
    FGLTexture: TGLuint;
    procedure SetSketchData(const Data: TSataniaSketchDataArray);
    procedure SetTexture(const AValue: String);
    procedure GLContextOpen;
  public
    IsRemoved: Boolean;
    procedure GLContextClose; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure LocalRender(const Params: TRenderParams); override;

    property SketchData: TSataniaSketchDataArray write SetSketchData;
    property Texture: String read FTexture write SetTexture;
  end;

  TSataniaSketch = class
  public
    { Create new sketch with triangles }
    function CreateSketch(const AName: String): TSataniaSketchItem;
    { Find a sketch by name, return nil if none is found }
    function Find(const AName: String): TSataniaSketchItem;
    { Load Texture }
    function LoadTexture(const AName, ATexName: String): TSataniaSketchItem;
    { Draw triangles }
    function DrawTriangles(const AName: String; const ATriangles: TSataniaSketchDataArray): TSataniaSketchItem;
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

const
  VertexShaderSource =
'attribute vec2 inVertex;'nl
'attribute vec2 inTexCoord;'nl
'attribute vec4 inColor;'nl

'varying vec2 fragTexCoord;'nl
'varying vec4 fragColor;'nl

'uniform mat4 mvMatrix;'nl
'uniform mat4 pMatrix;'nl

'void main() {'nl
'  fragTexCoord = inTexCoord;'nl
'  fragColor = inColor;'nl
'  vec4 p = mvMatrix * vec4(inVertex, 0.0, 1.0);'nl
'  gl_Position = pMatrix * p;'nl
'}';

  FragmentShaderSource: String =
'varying vec2 fragTexCoord;'nl
'varying vec4 fragColor;'nl

'void main() {'nl
'  gl_FragColor = fragColor;'nl
'}';

    FragmentTextureShaderSource: String =
'varying vec2 fragTexCoord;'nl
'varying vec4 fragColor;'nl

'uniform sampler2D baseColor;'nl

'void main() {'nl
'  gl_FragColor = texture2D(baseColor, fragTexCoord) * fragColor;'nl
'}';

var
  RenderProgram,
  RenderTextureProgram: TGLSLProgram;

{ Call when OpenGL context is closed }
procedure FreeGLContext;
begin
  if RenderProgram <> nil then
  begin
    FreeAndNil(RenderProgram);
    FreeAndNil(RenderTextureProgram);
  end;
end;

// ----- TSataniaSketchItem -----

procedure TSataniaSketchItem.SetSketchData(const Data: TSataniaSketchDataArray);
var
  Len: Integer;
begin
  Self.FSketchData := Data;
  Len := Length(Self.FSketchData);
  glBindBuffer(GL_ARRAY_BUFFER, Self.VBO);
  if Len <> Self.FSketchDataPreviousLength then
    glBufferData(GL_ARRAY_BUFFER, Len * SizeOf(TSataniaSketchData), @Self.FSketchData[0], GL_STATIC_DRAW)
  else
    glBufferSubData(GL_ARRAY_BUFFER, 0, Len * SizeOf(TSataniaSketchData), @Self.FSketchData[0]);
  glBindBuffer(GL_ARRAY_BUFFER, 0);
  Self.FSketchDataPreviousLength := Len;
end;

procedure TSataniaSketchItem.SetTexture(const AValue: String);
begin
  if Self.FGLTexture <> 0 then
    glFreeTexture(Self.FGLTexture);
  if AValue <> '' then
  begin
    Self.FGLTexture := LoadGLTexture(
      PATH_SPRITES + Save.Settings.Skin + '/' + Self.FTexture,
      TextureFilter(minLinear, magLinear),
      Texture2DClampToEdge
    );
  end;
  Self.FTexture := AValue;
end;

procedure TSataniaSketchItem.GLContextOpen;
begin
  if RenderProgram = nil then
  begin
    RenderProgram := TGLSLProgram.Create;
    RenderProgram.AttachVertexShader(VertexShaderSource);
    RenderProgram.AttachFragmentShader(FragmentShaderSource);
    RenderProgram.Link;
    RenderTextureProgram := TGLSLProgram.Create;
    RenderTextureProgram.AttachVertexShader(VertexShaderSource);
    RenderTextureProgram.AttachFragmentShader(FragmentTextureShaderSource);
    RenderTextureProgram.Link;
    ApplicationProperties.OnGLContextClose.Add(@FreeGLContext);
  end;
  if Self.VBO = 0 then
  begin
    glGenBuffers(1, @Self.VBO);
    glBindBuffer(GL_ARRAY_BUFFER, Self.VBO);
    glBufferData(GL_ARRAY_BUFFER, Length(Self.FSketchData) * SizeOf(TSataniaSketchData), @Self.FSketchData[0], GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
  end;
end;

procedure TSataniaSketchItem.GLContextClose;
begin
  if Self.VBO <> 0 then
  begin
    glDeleteBuffers(1, @Self.VBO);
    Self.VBO := 0;
  end;
  if Self.FGLTexture <> 0 then
    glFreeTexture(Self.FGLTexture);
  inherited;
end;

constructor TSataniaSketchItem.Create(AOwner: TComponent);
begin
  inherited;
  Self.GLContextOpen;
end;

destructor TSataniaSketchItem.Destroy;
begin
  inherited;
end;

procedure TSataniaSketchItem.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  if not Self.Exists then
    Exit;
  if IsRemoved then
    RemoveMe := rtRemoveAndFree
  else
    RemoveMe := rtNone;
end;

procedure TSataniaSketchItem.LocalRender(const Params: TRenderParams);
var
  I: Integer;
  PreviousProgram: TGLSLProgram;
begin
  if (not Self.Visible) or (not Self.Exists) or Params.InShadow or (not Params.Transparent) or (Params.StencilTest > 0) then
    Exit;
  Inc(Params.Statistics.ScenesRendered);
  Inc(Params.Statistics.ScenesVisible);
  Inc(Params.Statistics.ShapesVisible);

  PreviousProgram := RenderContext.CurrentProgram;

  glEnable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glDepthMask(GL_FALSE);
  glActiveTexture(GL_TEXTURE0);

  if Self.FGLTexture <> 0 then
  begin
    RenderTextureProgram.Enable;
    glBindTexture(GL_TEXTURE_2D, Self.FGLTexture);
  end else
  begin
    RenderProgram.Enable;
  end;
  RenderProgram.Uniform('mvMatrix').SetValue(Params.RenderingCamera.Matrix * Params.Transform^);
  RenderProgram.Uniform('pMatrix').SetValue(RenderContext.ProjectionMatrix);

  glBindBuffer(GL_ARRAY_BUFFER, Self.VBO);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, SizeOf(TSataniaSketchData), Pointer(0));
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, SizeOf(TSataniaSketchData), Pointer(8));
  glEnableVertexAttribArray(2);
  glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, SizeOf(TSataniaSketchData), Pointer(16));
  glDrawArrays(GL_TRIANGLES, 0, Length(Self.FSketchData));
  glBindBuffer(GL_ARRAY_BUFFER, 0);

  glDisable(GL_BLEND);
  glDisable(GL_DEPTH_TEST);
  glDepthMask(GL_TRUE);

  PreviousProgram.Enable;
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

function TSataniaSketch.LoadTexture(const AName, ATexName: String): TSataniaSketchItem;
begin
  Result := CreateSketch(AName);
  Result.Texture := ATexName;
end;

function TSataniaSketch.DrawTriangles(const AName: String; const ATriangles: TSataniaSketchDataArray): TSataniaSketchItem;
begin
  Result := CreateSketch(AName);
  Result.SketchData := ATriangles;
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
