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
  CastleRenderContext;

type
  TSataniaSketchItem = class(TCastleTransform)
  private
    FVertices: array of TVector2;
    FTexCoords: array of TVector2;
    FColors: array of TVector4;
    FShader: TGLSLProgram;
    procedure GLContextOpen;
  public
    IsRemoved: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure LocalRender(const Params: TRenderParams); override;
  end;

  TSataniaSketch = class
  public
    function Find(const AName: String): TSataniaSketchItem;
    procedure AddOrReplace(const AName: String; const AItem: TSataniaSketchItem);
    function Delete(const AName: String): Boolean;
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

'uniform sampler2D baseColor;'nl
'uniform vec4 color;'nl

'void main() {'nl
'  gl_FragColor = texture2D(baseColor, fragTexCoord) * fragColor * color;'nl
'}';

var
  RenderProgram: TGLSLProgram;

{ Call when OpenGL context is closed }
procedure FreeGLContext;
begin
  if RenderProgram <> nil then
  begin
    FreeAndNil(RenderProgram);
  end;
end;

// ----- TSataniaSketchItem -----

procedure TSataniaSketchItem.GLContextOpen;
begin
  if RenderProgram = nil then
  begin
    RenderProgram := TGLSLProgram.Create;
    RenderProgram.AttachVertexShader(VertexShaderSource);
    RenderProgram.AttachFragmentShader(FragmentShaderSource);
    RenderProgram.Link;
    ApplicationProperties.OnGLContextClose.Add(@FreeGLContext);
  end;
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
  RenderProgram.Enable;

  glEnable(GL_DEPTH_TEST);
  glEnable(GL_BLEND);
  glDepthMask(GL_FALSE);
  // glActiveTexture(GL_TEXTURE0);

  // TODO: Render goes here

  glDisable(GL_BLEND);
  glDisable(GL_DEPTH_TEST);
  glDepthMask(GL_TRUE);

  PreviousProgram.Enable;
end;

// ----- TSataniaSketch -----

function TSataniaSketch.Find(const AName: String): TSataniaSketchItem;
begin

end;

procedure TSataniaSketch.AddOrReplace(const AName: String; const AItem: TSataniaSketchItem);
begin

end;

function TSataniaSketch.Delete(const AName: String): Boolean;
begin

end;

initialization
  SataniaSketch := TSataniaSketch.Create;

finalization
  SataniaSketch.Free;

end.

