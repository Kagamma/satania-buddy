{
  Copyright (c) 2022-2024 kagamma.
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
}

unit CastleSpine;

{$mode objfpc}{$H+}
{$macro on}
{$define nl:=+ LineEnding +}

{$ifdef ANDROID}{$define GLES}{$endif}
{$ifdef iOS}{$define GLES}{$endif}

interface

uses
  Classes, SysUtils, Generics.Collections, fpjsonrtti, Spine, strutils,
  {$ifdef GLES}
  CastleGLES,
  {$else}
  GL, GLExt,
  {$endif}
  {$ifdef CASTLE_DESIGN_MODE}
  PropEdits, CastlePropEdits, CastleDebugTransform, Forms, Controls, Graphics, Dialogs,
  ButtonPanel, StdCtrls, ExtCtrls, CastleInternalExposeTransformsDialog,
  {$endif}
  CastleVectors, CastleApplicationProperties, CastleTransform, CastleComponentSerialize, CastleGLVersion,
  CastleBoxes, CastleUtils, CastleLog, CastleRenderContext, CastleGLShaders, CastleDownload, CastleURIUtils,
  CastleGLImages, X3DNodes, CastleColors, CastleClassUtils, CastleBehaviors, CastleRenderOptions;

type
  TCastleSpinePlayAnimationParameters = record
    Name: String;
    Loop: Boolean;
    Forward: Boolean;
    TimeScale: Single;
    TransitionDuration: Single;
    InitialTime: Single;
    Track: Integer;
  end;
  TCastleSpinePlayAnimationParametersList = specialize TList<TCastleSpinePlayAnimationParameters>;

  TCastleSpineEvent = record
    State: PspAnimationState;
    Typ: TSpEventType;
    Entry: PspTrackEntry;
    Event: PspEvent;
  end;

  TCastleSpineEventNotify = procedure(const Event: TCastleSpineEvent) of object;

  PCastleSpineVertex = ^TCastleSpineVertex;
  TCastleSpineVertex = packed record
    Vertex: TVector2;
    TexCoord: TVector2;
    Color: TVector4;
  end;

  TCastleSpineControlBone = record
    Bone: PspBone;
    X, Y, Rotation: Single;
  end;
  TCastleSpineControlBoneList = specialize TList<TCastleSpineControlBone>;

  PCastleSpineData = ^TCastleSpineData;
  TCastleSpineData = record
    Atlas: PspAtlas;
    SkeletonJson: PspSkeletonJson;
    SkeletonBinary: PspSkeletonBinary;
    SkeletonData: PspSkeletonData;
    AnimationStateData: PspAnimationStateData;
  end;

  TCastleSpineDataCacheBase = specialize TDictionary<String, PCastleSpineData>;
  TCastleSpineDataCache = class(TCastleSpineDataCacheBase)
  public
    // Clear the cache
    procedure Clear; override;
  end;

  TCastleSpineTransformBehavior = class(TCastleBehavior)
  private
    FControlBone: Boolean;
    FControlRootPersistent: TCastleVector3Persistent;
    FControlAreaPersistent: TCastleVector2Persistent;
    FControlRoot: TVector3;
    FControlArea: TVector2;
    FOldTranslation: TVector3;
    FOldRotation: Single;
    FOldData: TCastleSpineControlBone;
    FBone: PspBone;
    FBoneDefault: PspBone;
    procedure SetControlRootForPersistent(const AValue: TVector3);
    procedure SetControlAreaForPersistent(const AValue: TVector2);
    function GetControlRootForPersistent: TVector3;
    function GetControlAreaForPersistent: TVector2;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$ifdef CASTLE_DESIGN_MODE}
    function PropertySections(const PropertyName: String): TPropertySections; override;
    {$endif}
    procedure SetControlBone(const V: Boolean);
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    property Bone: PspBone read FBone write FBone;
    property BoneDefault: PspBone read FBoneDefault write FBoneDefault;
    property ControlRoot: TVector3 read FControlRoot write FControlRoot;
    property ControlArea: TVector2 read FControlArea write FControlArea;
  published
    property ControlBone: Boolean read FControlBone write SetControlBone default False;
    property ControlRootPersistent: TCastleVector3Persistent read FControlRootPersistent;
    property ControlAreaPersistent: TCastleVector2Persistent read FControlAreaPersistent;
  end;

  TCastleSpine = class(TCastleTransform)
  strict private
    VBO: GLuint; // Maybe all instances could share the same VBO?
    FURL: String;
    FIsNeedRefreshAnimation: Boolean;
    FParametersList: TCastleSpinePlayAnimationParametersList;
    FIsGLContextInitialized: Boolean;
    FspSkin: PspSkin;
    FspSkinArray: array of PspSkin;
    FspSkeleton: PspSkeleton;
    FspSkeletonDefault: array of TspBone;
    FspAnimationState: PspAnimationState;
    FspSkeletonBounds: PspSkeletonBounds;
    FspClipper: PspSkeletonClipping;
    FEnableFog: Boolean;
    FIsNeedRefreshBones: Boolean;
    FIsNeedRefresh: Boolean;
    FPreviousAnimation: String;
    FIsAnimationPlaying: Boolean;
    FAutoAnimations: TStrings;
    FAutoAnimationsLoop: Boolean;
    FSpineData: PCastleSpineData;
    FDistanceCulling: Single;
    FSecondsPassedAcc: Single; // Used by AnimationSkipTicks
    FTicks: Integer; // Used by AnimationSkipTicks
    FSmoothTexture: Boolean;
    FColor: TVector4;
    FExposeTransforms: TStrings;
    FExposeTransformsPrefix: String;
    FColorPersistent: TCastleColorPersistent;
    FOnEventNotify: TCastleSpineEventNotify; // Used by Spine's events
    FControlBoneList: TCastleSpineControlBoneList;
    FTimePlaying: Boolean;
    FTimePlayingSpeed: Single;
    FAnimateSkipTicks: Integer;
    FAnimateOnlyWhenVisible: Boolean;
    FDefaultAnimationTransition: Single;
    FAnimationsList: TStrings;
    FSkinsList: TStrings;
    FProcessEvents: Boolean;
    FShader: TGLSLProgram;
    FSkins: TStrings;
    FMipmap: Boolean;
    FIsCoreProfile: Boolean;
    { Cleanup Spine resource associate with this instance }
    procedure Cleanup;
    procedure InternalExposeTransformsChange;
    procedure ExposeTransformsChange(Sender: TObject);
    procedure GLContextOpen;
    procedure InternalLoadSpine;
    procedure SetAutoAnimations(const Value: TStrings);
    procedure SetAutoAnimationsLoop(const V: Boolean);
    procedure SetColorForPersistent(const AValue: TVector4);
    procedure SetExposeTransforms(const Value: TStrings);
    procedure SetExposeTransformsPrefix(const S: String);
    function GetColorForPersistent: TVector4;
    procedure SetSkins(const Value: TStrings);
  public
    { Keep track of track entries }
    TrackEntries: array[0..99] of PspTrackEntry;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$ifdef CASTLE_DESIGN_MODE}
    function PropertySections(const PropertyName: String): TPropertySections; override;
    {$endif}
    procedure LoadSpine(const AURL: String);
    procedure GLContextClose; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    procedure LocalRender(const Params: TRenderParams); override;
    function LocalBoundingBox: TBox3D; override;
    procedure InternalPlayAnimation;
    { Similar to PlayAnimation. The Track parameter tell Spine runtime which track we play the animation, allows to mix multiple animations }
    function PlayAnimation(const AnimationName: string; const Loop: boolean; const Forward: boolean = true; const Track: Integer = 0): boolean; overload;
    function PlayAnimation(const Parameters: TCastleSpinePlayAnimationParameters): boolean; overload;
    { Similar to StopAnimation. The Track parameter tell Spine runtime which track we stop the animation. If Track = -1, then we stop all animations }
    procedure StopAnimation(const Track: Integer = -1); overload;
    { Get the names of the bounding boxes attachment contains the point, or nil if point is not inside any bounding box }
    function ContainsPoint(const X, Y: Single): TStrings;
    { Get the names of the bounding boxes attachment contains the point, or nil if point is not inside any bounding box }
    function IntersectsSegment(const X1, Y1, X2, Y2: Single): TStrings;
    property Color: TVector4 read FColor write FColor;
    property ControlBoneList: TCastleSpineControlBoneList read FControlBoneList;
    property AnimationsList: TStrings read FAnimationsList;
    property SkinsList: TStrings read FSkinsList;
    property Shader: TGLSLProgram read FShader write FShader;
    property Skeleton: PspSkeleton read FspSkeleton;
    property Bounds: PspSkeletonBounds read FspSkeletonBounds;
    property IsGLContextInitialized: Boolean read FIsGLContextInitialized;
  published
    property ProcessEvents: Boolean read FProcessEvents write FProcessEvents;
    property DefaultAnimationTransition: Single read FDefaultAnimationTransition write FDefaultAnimationTransition default 0;
    property AnimateOnlyWhenVisible: Boolean read FAnimateOnlyWhenVisible write FAnimateOnlyWhenVisible;
    property AnimateSkipTicks: Integer read FAnimateSkipTicks write FAnimateSkipTicks default 0;
    property TimePlayingSpeed: Single read FTimePlayingSpeed write FTimePlayingSpeed default 1;
    property TimePlaying: Boolean read FTimePlaying write FTimePlaying default True;
    property Skins: TStrings read FSkins write SetSkins;
    property URL: String read FURL write LoadSpine;
    property AutoAnimations: TStrings read FAutoAnimations write SetAutoAnimations;
    property AutoAnimationsLoop: Boolean read FAutoAnimationsLoop write SetAutoAnimationsLoop default true;
    property EnableFog: Boolean read FEnableFog write FEnableFog default False;
    property ColorPersistent: TCastleColorPersistent read FColorPersistent;
    property Mipmap: Boolean read FMipmap write FMipmap default False;
    property SmoothTexture: Boolean read FSmoothTexture write FSmoothTexture default True;
    property DistanceCulling: Single read FDistanceCulling write FDistanceCulling default 0;
    property ExposeTransforms: TStrings read FExposeTransforms write SetExposeTransforms;
    property ExposeTransformsPrefix: String read FExposeTransformsPrefix write SetExposeTransformsPrefix;
    property OnEventNotify: TCastleSpineEventNotify read FOnEventNotify write FOnEventNotify;
  end;

var
  SpineDataCache: TCastleSpineDataCache;

implementation

uses
  Math;

const
  VertexShaderSourceLegacy: String =
'attribute vec2 inVertex;'nl
'attribute vec2 inTexCoord;'nl
'attribute vec4 inColor;'nl

'varying vec2 fragTexCoord;'nl
'varying vec4 fragColor;'nl
'varying float fragFogCoord;'nl

'uniform mat4 mvMatrix;'nl
'uniform mat4 pMatrix;'nl

'void main() {'nl
'  fragTexCoord = inTexCoord;'nl
'  fragColor = inColor;'nl
'  vec4 p = mvMatrix * vec4(inVertex, 0.0, 1.0);'nl
'  fragFogCoord = abs(p.z / p.w);'nl
'  gl_Position = pMatrix * p;'nl
'}';

  FragmentShaderSourceLegacy: String =
'varying vec2 fragTexCoord;'nl
'varying vec4 fragColor;'nl
'varying float fragFogCoord;'nl

'uniform sampler2D baseColor;'nl
'uniform int fogEnable;'nl
'uniform float fogEnd;'nl
'uniform vec3 fogColor;'nl
'uniform vec4 color;'nl

'void main() {'nl
'  gl_FragColor = texture2D(baseColor, fragTexCoord) * fragColor * color;'nl
'  if (fogEnable == 1) {'nl
'    float fogFactor = (fogEnd - fragFogCoord) / fogEnd;'nl
'    gl_FragColor.rgb = mix(fogColor, gl_FragColor.rgb, clamp(fogFactor, 0.0, 1.0));'nl
'  }'nl
'}';

  VertexShaderSource: String =
'#version 330'nl
'layout(location = 0) in vec2 inVertex;'nl
'layout(location = 1) in vec2 inTexCoord;'nl
'layout(location = 2) in vec4 inColor;'nl

'out vec2 fragTexCoord;'nl
'out vec4 fragColor;'nl
'out float fragFogCoord;'nl

'uniform mat4 mvMatrix;'nl
'uniform mat4 pMatrix;'nl

'void main() {'nl
'  fragTexCoord = inTexCoord;'nl
'  fragColor = inColor;'nl
'  vec4 p = mvMatrix * vec4(inVertex, 0.0, 1.0);'nl
'  fragFogCoord = abs(p.z / p.w);'nl
'  gl_Position = pMatrix * p;'nl
'}';

  FragmentShaderSource: String =
'#version 330'nl
'in vec2 fragTexCoord;'nl
'in vec4 fragColor;'nl
'in float fragFogCoord;'nl

'out vec4 outColor;'nl

'uniform sampler2D baseColor;'nl
'uniform int fogEnable;'nl
'uniform float fogEnd;'nl
'uniform vec3 fogColor;'nl
'uniform vec4 color;'nl

'void main() {'nl
'  outColor = texture2D(baseColor, fragTexCoord) * fragColor * color;'nl
'  if (fogEnable == 1) {'nl
'    float fogFactor = (fogEnd - fragFogCoord) / fogEnd;'nl
'    outColor.rgb = mix(fogColor, fragColor.rgb, clamp(fogFactor, 0.0, 1.0));'nl
'  }'nl
'}';

{$ifdef CASTLE_DESIGN_MODE}
type
  TExposeTransformsPropertyEditor = class(TStringsPropertyEditor)
  public
    procedure Edit; override;
  end;

  { Property editor to select an animation on TCastleSpine. }
  TSpineAutoAnimationPropertyEditor = class(TStringsPropertyEditor)
  public
    procedure Edit; override;
  end;

  TSpineSkinPropertyEditor = class(TStringsPropertyEditor)
  public
    procedure Edit; override;
  end;
{$endif}

var
  WorldVerticesPositions: array[0..(16384 * 3 - 1)] of Single;
  SpineVertices: array[0..(High(WorldVerticesPositions) div 3) - 1] of TCastleSpineVertex;
  RenderProgram: TGLSLProgram;
  RegionIndices: array[0..5] of Word = (0, 1, 2, 2, 3, 0);
  CurrentSpineInstance: TCastleSpine;

{ Call when OpenGL context is closed }
procedure FreeGLContext;
begin
  if RenderProgram <> nil then
  begin
    FreeAndNil(RenderProgram);
  end;
end;

{ Provide loader functions for Spine }
procedure LoaderLoad(FileName: PChar; var Data: Pointer; var Size: LongWord); cdecl;
var
  S: String;
  MS: TMemoryStream;
begin
  try
    S := FileName;
    MS := Download(S, [soForceMemoryStream]) as TMemoryStream;
    // Data is managed by spine-c, so we call spine-c mem functions instead
    Data := _spMalloc(Size, nil, 0);
    Size := MS.Size;
    // Copy data from MS to Data
    Move(MS.Memory^, Data^, Size);
    //
    MS.Free;
  except
    // We ignore exception, and return null instead
    on E: Exception do
    begin
      WritelnLog('Spine Error', 'LoaderLoad: ' + E.Message + ' while loading ' + S);
      Data := nil;
      Size := 0;
    end;
  end;
end;

procedure LoaderLoadTexture(FileName: PChar; var ObjPas: TObject; var Width, Height: Integer);
var
  Image: TDrawableImage;
begin
  Image := TDrawableImage.Create(FileName, True);
  ObjPas := Image;
  Width := Image.Image.Width;
  Height := Image.Image.Height;
end;

procedure LoaderFreeTexture(ObjPas: TObject);
begin
  ObjPas.Free;
end;

function CreateColorPersistent(const G: TGetVector4Event; const S: TSetVector4Event; const ADefaultValue: TVector4): TCastleColorPersistent;
begin
  Result := TCastleColorPersistent.Create(nil);
  Result.InternalGetValue := G;
  Result.InternalSetValue := S;
  Result.InternalDefaultValue := ADefaultValue;
end;

function CreateVec2Persistent(const G: TGetVector2Event; const S: TSetVector2Event; const ADefaultValue: TVector2): TCastleVector2Persistent;
begin
  Result := TCastleVector2Persistent.Create(nil);
  Result.InternalGetValue := G;
  Result.InternalSetValue := S;
  Result.InternalDefaultValue := ADefaultValue;
end;

function CreateVec3Persistent(const G: TGetVector3Event; const S: TSetVector3Event; const ADefaultValue: TVector3): TCastleVector3Persistent;
begin
  Result := TCastleVector3Persistent.Create(nil);
  Result.InternalGetValue := G;
  Result.InternalSetValue := S;
  Result.InternalDefaultValue := ADefaultValue;
end;

{ Naive implementation of util function that takes a bone name and convert to valid component name }
function ValidName(const S: String): String;
begin
  Result := StringsReplace(S, [' ', '-'], ['_', '_'], [rfReplaceAll]);
end;

{ Trigger when an Spine event is fired. CurrentSpineInstance is the instance where the event belong to }
procedure EventListener(State: PspAnimationState; Typ: TSpEventType; Entry: PspTrackEntry; Event: PspEvent); cdecl;
var
  E: TCastleSpineEvent;
begin
  if (CurrentSpineInstance <> nil) and (CurrentSpineInstance.OnEventNotify <> nil) then
  begin
    E.State := State;
    E.Typ := Typ;
    E.Entry := Entry;
    E.Event := Event;
    CurrentSpineInstance.OnEventNotify(E);
  end;
end;

{ ----- TCastleSpineTransformBehavior ----- }

procedure TCastleSpineTransformBehavior.SetControlRootForPersistent(const AValue: TVector3);
begin
  Self.FControlRoot := AValue;
end;

procedure TCastleSpineTransformBehavior.SetControlAreaForPersistent(const AValue: TVector2);
begin
  Self.FControlArea := AValue;
end;

function TCastleSpineTransformBehavior.GetControlRootForPersistent: TVector3;
begin
  Result := Self.FControlRoot;
end;

function TCastleSpineTransformBehavior.GetControlAreaForPersistent: TVector2;
begin
  Result := Self.FControlArea;
end;

constructor TCastleSpineTransformBehavior.Create(AOwner: TComponent);
begin
  inherited;
  Self.FControlRootPersistent := CreateVec3Persistent(
    @Self.GetControlRootForPersistent,
    @Self.SetControlRootForPersistent,
    Self.FControlRoot
  );
  Self.FControlAreaPersistent := CreateVec2Persistent(
    @Self.GetControlAreaForPersistent,
    @Self.SetControlAreaForPersistent,
    Self.FControlArea
  );
end;

destructor TCastleSpineTransformBehavior.Destroy;
begin
  Self.FControlRootPersistent.Free;
  Self.FControlAreaPersistent.Free;
  inherited;
end;

{$ifdef CASTLE_DESIGN_MODE}
function TCastleSpineTransformBehavior.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'ControlBone')
    or (PropertyName = 'ControlRootPersistent')
    or (PropertyName = 'ControlAreaPersistent') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;
{$endif}

procedure TCastleSpineTransformBehavior.SetControlBone(const V: Boolean);
begin
  Self.FControlBone := V;
  Self.FOldTranslation := TVector3.Zero;
end;

procedure TCastleSpineTransformBehavior.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  V: TVector4;
  D: TCastleSpineControlBone;

  procedure UpdateParentPosition; inline;
  begin
    Self.Parent.Translation := Vector3(Self.Bone^.worldX, Self.Bone^.worldY, 0);
    Self.Parent.Rotation := Vector4(0, 0, 1, spBone_getWorldRotationX(Self.Bone) * 0.017453);
    Self.Parent.Scale := Vector3(spBone_getWorldScaleX(Bone), spBone_getWorldScaleY(Bone), 1);
  end;

  procedure UpdateParentPositionBaseOnArea; inline;
  var
    V: Single;
    AX, AY: Single;
  begin
    // Only if area <> (0,0)
    if Self.FControlArea.X <> 0 then
    begin
      AX := Self.FControlArea.X * 0.5;
      V := Self.FControlRoot.X - Self.Parent.Translation.X;
      if Abs(V) > AX then
        Self.Parent.Translation := Vector3(Self.FControlRoot.X + (AX * (-(Sign(V)))), Self.Parent.Translation.Y, Self.Parent.Translation.Z);
    end;
    if Self.FControlArea.Y <> 0 then
    begin
      AY := Self.FControlArea.Y * 0.5;
      V := Self.FControlRoot.Y - Self.Parent.Translation.Y;
      if Abs(V) > AY then
        Self.Parent.Translation := Vector3(Self.Parent.Translation.X, Self.FControlRoot.Y + (AY * (-(Sign(V)))), Self.Parent.Translation.Z);
    end;
  end;

begin
  inherited;
  if Bone = nil then Exit;
  if not Self.FControlBone then
  begin
    UpdateParentPosition;
  end else
  begin
    UpdateParentPositionBaseOnArea;
    if (Self.FOldTranslation.X <> Self.Parent.Translation.X) or (Self.FOldTranslation.Y <> Self.Parent.Translation.Y) or (Self.FOldRotation <> Self.Parent.Rotation.W) then
    begin
      Bone^ := Self.FBoneDefault^;
      // TODO: Correctly handle rotation
      // TODO: Currently this expects "world", or "root", locate at (0,0)
      // TODO: Only IKs at root work correctly at the moment
      V := Self.Parent.Parent.WorldInverseTransform * (Self.Parent.WorldTransform * Vector4(Self.Parent.Translation, 1.0)) * 0.5;
      spBone_worldToLocal(Bone, V.X, V.Y, @D.X, @D.Y);
      D.Rotation := Self.Parent.Rotation.W * 57.29578;
      D.Bone := Bone;
      Self.FOldData := D;
    end;
    if Self.FOldData.Bone <> nil then
      TCastleSpine(Self.Parent.Parent).ControlBoneList.Add(Self.FOldData);
  end;
  Self.FOldTranslation := Self.Parent.Translation;
  Self.FOldRotation := Self.Parent.Rotation.W;
end;

{$ifdef CASTLE_DESIGN_MODE}

{ ----- TExposeTransformsPropertyEditor ----- }

procedure TExposeTransformsPropertyEditor.Edit;
var
  DialogSelection: TExposeTransformSelection;
  D: TExposeTransformsDialog;
  ValueStrings, SelectionList: TStrings;
  S: String;
  SelItem: TExposeTransformSelectionItem;
  Skeleton: PspSkeleton;
  Scene: TCastleSpine;
  Item: TExposeTransformSelectionItem;
  NamesList: TStrings;
  I: Integer;
begin
  D := TExposeTransformsDialog.Create(Application);
  try
    Scene := GetComponent(0) as TCastleSpine;
    Skeleton := Scene.Skeleton;
    D.Caption := 'Edit ' + Scene.Name + '.ExposeTransforms';
    D.Label1.Caption := '';

    DialogSelection := D.Selection;
    DialogSelection.Clear;

    // add to D.Selection all possible transforms from the scene
    if Skeleton <> nil then
    begin
      NamesList := TStringList.Create;
      try
        TstringList(NamesList).Sorted := True;
        for I := 0 to Skeleton^.bonesCount - 1 do
          NamesList.Add(Skeleton^.bones[I]^.data^.name);
        for I := 0 to NamesList.Count - 1 do
        begin
          if DialogSelection.FindName(NamesList[I]) = nil then
          begin
            Item := TExposeTransformSelectionItem.Create;
            Item.Name := NamesList[I];
            Item.ExistsInScene := true;
            Item.Selected := false; // may be changed to true later
            DialogSelection.Add(Item);
          end;
        end;
      finally
        NamesList.Free;
      end;
    end;

    // add/update in D.Selection all currently selected transforms
    ValueStrings := TStrings(GetObjectValue);
    for S in ValueStrings do
      if S <> '' then
      begin
        SelItem := D.Selection.FindName(S);
        if SelItem = nil then
        begin
          SelItem := TExposeTransformSelectionItem.Create;
          SelItem.Name := S;
          SelItem.ExistsInScene := false;
          DialogSelection.Add(SelItem);
        end;
        SelItem.Selected := true
      end;

    D.UpdateSelectionUi;
    if D.ShowModal = mrOK then
    begin
      SelectionList := DialogSelection.ToList;
      try
        SetPtrValue(SelectionList);
      finally FreeAndNil(SelectionList) end;
    end;
    Modified;
  finally FreeAndNil(D) end;
end;

{ ----- TSpineAutoAnimationPropertyEditor ----- }

procedure TSpineAutoAnimationPropertyEditor.Edit;
var
  DialogSelection: TExposeTransformSelection;
  D: TExposeTransformsDialog;
  ValueStrings, SelectionList: TStrings;
  S: String;
  SelItem: TExposeTransformSelectionItem;
  Scene: TCastleSpine;
  Item: TExposeTransformSelectionItem;
  I: Integer;
begin
  D := TExposeTransformsDialog.Create(Application);
  try
    Scene := GetComponent(0) as TCastleSpine;
    D.Caption := 'Edit ' + Scene.Name + '.AutoAnimations';
    D.Label1.Caption := '';

    DialogSelection := D.Selection;
    DialogSelection.Clear;

    // add to D.Selection all possible animations from the scene
    for I := 0 to Scene.AnimationsList.Count - 1 do
    begin
      if DialogSelection.FindName(Scene.AnimationsList[I]) = nil then
      begin
        Item := TExposeTransformSelectionItem.Create;
        Item.Name := Scene.AnimationsList[I];
        Item.ExistsInScene := true;
        Item.Selected := false; // may be changed to true later
        DialogSelection.Add(Item);
      end;
    end;

    // add/update in D.Selection all currently selected animations
    ValueStrings := TStrings(GetObjectValue);
    for S in ValueStrings do
      if S <> '' then
      begin
        SelItem := D.Selection.FindName(S);
        if SelItem = nil then
        begin
          SelItem := TExposeTransformSelectionItem.Create;
          SelItem.Name := S;
          SelItem.ExistsInScene := false;
          DialogSelection.Add(SelItem);
        end;
        SelItem.Selected := true
      end;

    D.UpdateSelectionUi;
    if D.ShowModal = mrOK then
    begin
      SelectionList := DialogSelection.ToList;
      try
        SetPtrValue(SelectionList);
      finally FreeAndNil(SelectionList) end;
    end;
    Modified;
  finally FreeAndNil(D) end;
end;

{ ----- TSpineSkinPropertyEditor ----- }

procedure TSpineSkinPropertyEditor.Edit;
var
  DialogSelection: TExposeTransformSelection;
  D: TExposeTransformsDialog;
  ValueStrings, SelectionList: TStrings;
  S: String;
  SelItem: TExposeTransformSelectionItem;
  Scene: TCastleSpine;
  Item: TExposeTransformSelectionItem;
  I: Integer;
begin
  D := TExposeTransformsDialog.Create(Application);
  try
    Scene := GetComponent(0) as TCastleSpine;
    D.Caption := 'Edit ' + Scene.Name + '.Skins';
    D.Label1.Caption := '';

    DialogSelection := D.Selection;
    DialogSelection.Clear;

    // add to D.Selection all possible skins from the scene
    for I := 0 to Scene.SkinsList.Count - 1 do
    begin
      if DialogSelection.FindName(Scene.SkinsList[I]) = nil then
      begin
        Item := TExposeTransformSelectionItem.Create;
        Item.Name := Scene.SkinsList[I];
        Item.ExistsInScene := true;
        Item.Selected := false; // may be changed to true later
        DialogSelection.Add(Item);
      end;
    end;

    // add/update in D.Selection all currently selected skins
    ValueStrings := TStrings(GetObjectValue);
    for S in ValueStrings do
      if S <> '' then
      begin
        SelItem := D.Selection.FindName(S);
        if SelItem = nil then
        begin
          SelItem := TExposeTransformSelectionItem.Create;
          SelItem.Name := S;
          SelItem.ExistsInScene := false;
          DialogSelection.Add(SelItem);
        end;
        SelItem.Selected := true
      end;

    D.UpdateSelectionUi;
    if D.ShowModal = mrOK then
    begin
      SelectionList := DialogSelection.ToList;
      try
        SetPtrValue(SelectionList);
      finally FreeAndNil(SelectionList) end;
    end;
    Modified;
  finally FreeAndNil(D) end;
end;
{$endif}

{ ----- TCastleSpineDataCache ----- }

procedure TCastleSpineDataCache.Clear;
var
  Key: String;
  SpineData: PCastleSpineData;
begin
  for Key in Self.Keys do
  begin
    SpineData := Self[Key];
    spAtlas_dispose(SpineData^.Atlas);
    if SpineData^.SkeletonJson <> nil then
      spSkeletonJson_dispose(SpineData^.SkeletonJson)
    else
    if SpineData^.SkeletonBinary <> nil then
      spSkeletonBinary_dispose(SpineData^.SkeletonBinary);
    spSkeletonData_dispose(SpineData^.SkeletonData);
    spAnimationStateData_dispose(SpineData^.AnimationStateData);
    Dispose(SpineData);
  end;
  inherited;
end;

{ ----- TCastleSpine ----- }

procedure TCastleSpine.GLContextOpen;
begin
  if not ApplicationProperties.IsGLContextOpen then Exit;
  if Self.FIsGLContextInitialized then Exit;
  if Spine_Load then
  begin
    Spine_Loader_RegisterLoadRoutine(@LoaderLoad);
    Spine_Loader_RegisterLoadTextureRoutine(@LoaderLoadTexture);
    Spine_Loader_RegisterFreeTextureRoutine(@LoaderFreeTexture);
  end;
  if RenderProgram = nil then
  begin
    RenderProgram := TGLSLProgram.Create;
    if GLVersion.AtLeast(3, 0) then
    begin
      Self.FIsCoreProfile := True;
      RenderProgram.AttachVertexShader(VertexShaderSource);
      RenderProgram.AttachFragmentShader(FragmentShaderSource);
    end else
    begin
      Self.FIsCoreProfile := False;
      RenderProgram.AttachVertexShader(VertexShaderSourceLegacy);
      RenderProgram.AttachFragmentShader(VertexShaderSourceLegacy);
    end;
    RenderProgram.Link;
    ApplicationProperties.OnGLContextClose.Add(@FreeGLContext);
  end;
  Self.FShader := RenderProgram;
  if VBO = 0 then
  begin
    glGenBuffers(1, @VBO);

    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBufferData(GL_ARRAY_BUFFER, Length(SpineVertices) * SizeOf(TCastleSpineVertex), @SpineVertices[0], GL_STATIC_DRAW);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
  end;
  Self.FIsGLContextInitialized := True;
end;

procedure TCastleSpine.GLContextClose;
begin
  if Self.FIsGLContextInitialized then
  begin
    if VBO <> 0 then
    begin
      glDeleteBuffers(1, @VBO);
      VBO := 0;
    end;
    if Self.FShader <> RenderProgram then
      Self.FShader.Free;
    Self.FIsGLContextInitialized := False;
  end;
  inherited;
end;

procedure TCastleSpine.InternalLoadSpine;
var
  Path: String;
  SkeletonFullPath,
  AtlasFullPath: String;
  MS: TMemoryStream;
  SS: TStringStream;
  SpineData: PCastleSpineData;
  I, J: Integer;
  HasCustomSkin: Boolean = False;
  IsBinary: Boolean = False;
begin
  Self.Cleanup;

  if Self.FURL = '' then
  begin
    Self.FIsNeedRefresh := False;
    Exit;
  end;
  CurrentSpineInstance := Self;

  {$ifdef CASTLE_DESIGN_MODE}
  SpineDataCache.Clear; // We don't cache spine data in castle-editor
  {$endif}

  if not SpineDataCache.ContainsKey(Self.FURL) then
  begin
    New(SpineData);
    SpineData^.Atlas := nil;

    Path := ExtractFilePath(Self.FURL);
    SkeletonFullPath := Self.FURL;
    IsBinary := LowerCase(ExtractFileExt(SkeletonFullPath)) = '.skel';
    AtlasFullPath := Path + StringReplace(ExtractFileName(Self.FURL), ExtractFileExt(Self.FURL), '', [rfReplaceAll]) + '.atlas';

    // Load atlas
    // TODO: There's an unknown segfault here on Linux system. We hide it for now...
    MS := Download(AtlasFullPath, [soForceMemoryStream]) as TMemoryStream;
    try
      try
        SpineData^.Atlas := spAtlas_create(MS.Memory, MS.Size, PChar(Path), nil);
      except
        on E: Exception do
          ;
      end;
      if SpineData^.Atlas = nil then
        raise Exception.Create('Failed to load spine atlas');
    finally
      MS.Free;
    end;

    // Load skeleton data
    MS := Download(SkeletonFullPath, [soForceMemoryStream]) as TMemoryStream;
    if not IsBinary then
    begin
      SS := TStringStream.Create('');
      try
        SS.CopyFrom(MS, MS.Size);
        SpineData^.SkeletonBinary := nil;
        SpineData^.SkeletonJson := spSkeletonJson_create(SpineData^.Atlas);
        if SpineData^.SkeletonJson = nil then
          raise Exception.Create('Failed to load Spine JSON model');
        SpineData^.SkeletonData := spSkeletonJson_readSkeletonData(SpineData^.SkeletonJson, PChar(SS.DataString));
      finally
        SS.Free;
      end;
    end else
    begin
      SpineData^.SkeletonJson := nil;
      SpineData^.SkeletonBinary := spSkeletonBinary_create(SpineData^.Atlas);
      if SpineData^.SkeletonBinary = nil then
        raise Exception.Create('Failed to load Spine Binary model');
      SpineData^.SkeletonData := spSkeletonBinary_readSkeletonData(SpineData^.SkeletonBinary, MS.Memory, MS.Size);
    end;
    MS.Free;

    // Prepare animation state data
    SpineData^.AnimationStateData := spAnimationStateData_create(SpineData^.SkeletonData);
  end else
    SpineData := SpineDataCache[Self.FURL];

  // Create animation state
  Self.FspAnimationState := spAnimationState_create(SpineData^.AnimationStateData);
  Self.FspAnimationState^.listener := @EventListener;

  // Create skeleton
  Self.FspSkeleton := spSkeleton_create(SpineData^.SkeletonData);
  spSkeleton_setToSetupPose(Self.FspSkeleton);
  SetLength(Self.FspSkeletonDefault, Self.FspSkeleton^.bonesCount);
  for I := 0 to Self.FspSkeleton^.bonesCount - 1 do
    Self.FspSkeletonDefault[I] := Self.FspSkeleton^.bones[I]^;

  // Create boundingbox
  Self.FspSkeletonBounds := spSkeletonBounds_create();
  spSkeletonBounds_update(Self.FspSkeletonBounds, Self.FspSkeleton, True);

  // Create clipper
  Self.FspClipper := spSkeletonClipping_create();

  // Create skin
  Self.FspSkin := spSkin_create('___cge_skin');

  // Load animation list
  Self.AnimationsList.Clear;
  for I := 0 to SpineData^.SkeletonData^.animationsCount - 1 do
  begin
    Self.AnimationsList.Add(SpineData^.SkeletonData^.animations[I]^.name);
  end;
  // Auto play animation
  for I := 0 to Self.FAutoAnimations.Count - 1 do
  begin
    Self.PlayAnimation(Self.FAutoAnimations[I], Self.FAutoAnimationsLoop, True, I);
  end;

  // Load skin
  Self.SkinsList.Clear;
  SetLength(FspSkinArray, SpineData^.SkeletonData^.skinsCount);
  for I := 0 to SpineData^.SkeletonData^.skinsCount - 1 do
  begin
    Self.SkinsList.Add(SpineData^.SkeletonData^.skins[I]^.name);
    Self.FspSkinArray[I] := SpineData^.SkeletonData^.skins[I];
    // Auto apply skin
    for J := 0 to Self.FSkins.Count - 1 do
    begin
      if (SpineData^.SkeletonData^.skins[I]^.name = Self.FSkins[J]) then
      begin
        spSkin_addSkin(Self.FspSkin, SpineData^.SkeletonData^.skins[I]);
        HasCustomSkin := True;
        break;
      end;
    end;
  end;
  if HasCustomSkin then
    spSkeleton_setSkin(Self.FspSkeleton, Self.FspSkin);

  // Expose bone list
  Self.ExposeTransformsChange(nil);

  Self.FIsNeedRefresh := False;
  Self.FSpineData := SpineData;
  CurrentSpineInstance := nil;
end;

procedure TCastleSpine.Cleanup;
begin
  CurrentSpineInstance := Self;
  if Self.FspAnimationState <> nil then
    spAnimationState_dispose(Self.FspAnimationState);
  if Self.FspSkeleton <> nil then
    spSkeleton_dispose(Self.FspSkeleton);
  if Self.FspSkeletonBounds <> nil then
    spSkeletonBounds_dispose(Self.FspSkeletonBounds);
  if Self.FspClipper <> nil then
    spSkeletonClipping_dispose(Self.FspClipper);
  if Self.FspSkin <> nil then
    spSkin_dispose(Self.FspSkin);
  Self.FspSkeleton := nil;
  Self.FspAnimationState := nil;
  Self.FspSkeletonBounds := nil;
  Self.FspClipper := nil;
  Self.FspSkin := nil;
  CurrentSpineInstance := nil;
end;

procedure TCastleSpine.ExposeTransformsChange(Sender: TObject);
begin
  Self.FIsNeedRefreshBones := True;
end;

procedure TCastleSpine.InternalExposeTransformsChange;
var
  T: TCastleTransform;
  B: TCastleSpineTransformBehavior;
  I, J, K, L: Integer;
  Bone: PspBone;
  OldTransformList: TCastleTransformList;
  TransformName: String;
begin
  if Self.FspSkeleton = nil then Exit;
  OldTransformList := TCastleTransformList.Create;
  try
    OldTransformList.OwnsObjects := False;
    for I := 0 to Self.Count - 1 do
    begin
      T := Self.Items[I];
      if T.FindBehavior(TCastleSpineTransformBehavior) <> nil then
        OldTransformList.Add(T);
    end;
    // Generate new transforms, skip if old transforms is found
    for I := 0 to Self.FspSkeleton^.bonesCount - 1 do
    begin
      Bone := Self.FspSkeleton^.bones[I];
      TransformName := ValidName(Self.FExposeTransformsPrefix + Bone^.data^.name);
      for J := 0 to Self.FExposeTransforms.Count - 1 do
      begin
        if Self.FExposeTransforms[J] = Bone^.data^.name then
        begin
          T := nil;
          for K := 0 to OldTransformList.Count - 1 do
            if OldTransformList[K].Name = TransformName then
            begin
              T := OldTransformList[K];
              OldTransformList.Delete(K);
              B := T.FindBehavior(TCastleSpineTransformBehavior) as TCastleSpineTransformBehavior;
              Break;
            end;
          if T = nil then
          begin
            T := TCastleTransform.Create(Self);
            T.Name := TransformName;
            Self.Add(T);
            B := TCastleSpineTransformBehavior.Create(T);
            B.Name := T.Name + '_Behavior';
            T.AddBehavior(B);
          end;
          // Not sure if necessary, since B should never be nil
          if B = nil then
          begin
            B := TCastleSpineTransformBehavior.Create(T);
            B.Name := T.Name + '_Behavior';
            T.AddBehavior(B);
          end;
          B.Bone := Bone;
          for L := 0 to Length(Self.FspSkeletonDefault) - 1 do
            if Self.FspSkeletonDefault[I].data^.name = Bone^.data^.name then
            begin
              B.BoneDefault := @Self.FspSkeletonDefault[I];
              Break;
            end;
        end;
      end;
    end;
    // Remove remaining old transforms
    for I := 0 to OldTransformList.Count - 1 do
      OldTransformList[I].Free;
    InternalCastleDesignInvalidate := True;
  finally
    OldTransformList.Free;
  end;
  Self.FIsNeedRefreshBones := False;
end;

procedure TCastleSpine.SetAutoAnimations(const Value: TStrings);
var
  I: Integer;
begin
  Self.FAutoAnimations.Assign(Value);
  if Value.Count > 0 then
  begin
    if Value.Count > 1 then
    begin
      Self.StopAnimation;
    end;
    for I := 0 to Self.FAutoAnimations.Count - 1 do
      Self.PlayAnimation(Self.FAutoAnimations[I], Self.FAutoAnimationsLoop, True, I);
  end else
  begin
    Self.StopAnimation;
    spSkeleton_setToSetupPose(Self.FspSkeleton);
  end;
end;

procedure TCastleSpine.SetAutoAnimationsLoop(const V: Boolean);
var
  I: Integer;
begin
  Self.FAutoAnimationsLoop := V;
  if Self.FAutoAnimations.Count > 0 then
  begin
    for I := 0 to Self.FAutoAnimations.Count - 1 do
      Self.PlayAnimation(Self.FAutoAnimations[I], Self.FAutoAnimationsLoop, True, I);
  end;
end;

procedure TCastleSpine.SetColorForPersistent(const AValue: TVector4);
begin
  Self.FColor := AValue;
end;

procedure TCastleSpine.SetExposeTransforms(const Value: TStrings);
begin
  Self.FExposeTransforms.Assign(Value);
end;

procedure TCastleSpine.SetExposeTransformsPrefix(const S: String);
var
  T: TCastleTransform;
  B: TCastleSpineTransformBehavior;
  I: Integer;
  Bone: PspBone;
begin
  Self.FExposeTransformsPrefix := S;
  // Rename transforms
  for I := 0 to Self.Count - 1 do
  begin
    T := Self.Items[I];
    B := T.FindBehavior(TCastleSpineTransformBehavior) as TCastleSpineTransformBehavior;
    if B <> nil then
    begin
      T.Name := ValidName(Self.FExposeTransformsPrefix + B.Bone^.data^.name);
      B.Name := T.Name + '_Behavior';
      InternalCastleDesignInvalidate := True;
    end;
  end;
end;

function TCastleSpine.GetColorForPersistent: TVector4;
begin
  Result := Self.FColor;
end;

procedure TCastleSpine.SetSkins(const Value: TStrings);
var
  I, J: Integer;
begin
  Self.FSkins.Assign(Value);
  if Self.FspSkeleton <> nil then
  begin
    spSkin_clear(Self.FspSkin);
    if Self.FSkins.Count > 0 then
    begin
      for J := 0 to Self.FSkinsList.Count - 1 do
      begin
        for I := 0 to Self.FSkins.Count - 1 do
        begin
          if Self.FSkinsList[J] = Self.FSkins[I] then
          begin
            spSkin_addSkin(Self.FspSkin, Self.FspSkinArray[J]);
            break;
          end;
        end;
      end;
      spSkeleton_setSkinByName(Self.FspSkeleton, nil);
      spSkeleton_setSlotsToSetupPose(Self.FspSkeleton);
      spSkeleton_setSkin(Self.FspSkeleton, Self.FspSkin);
      spSkeleton_setSlotsToSetupPose(Self.FspSkeleton);
    end else
      spSkeleton_setSkinByName(Self.FspSkeleton, nil);
  end;
end;

constructor TCastleSpine.Create(AOwner: TComponent);
begin
  inherited;
  Self.FColor := Vector4(1, 1, 1, 1);
  Self.FSmoothTexture := True;
  Self.ProcessEvents := True;
  Self.FParametersList := TCastleSpinePlayAnimationParametersList.Create;
  Self.FAutoAnimations := TStringList.Create;
  Self.FAutoAnimationsLoop := True;
  Self.FExposeTransforms := TStringList.Create;
  Self.FSkins := TStringList.Create;
  Self.FControlBoneList := TCastleSpineControlBoneList.Create;
  Self.FTimePlayingSpeed := 1;
  Self.FTimePlaying := True;
  Self.FAnimationsList := TstringList.Create;
  TStringList(Self.FAnimationsList).Sorted := True;
  Self.FSkinsList := TStringList.Create;
  TStringList(Self.FExposeTransforms).OnChange := @Self.ExposeTransformsChange;
  Self.FColorPersistent := CreateColorPersistent(
    @Self.GetColorForPersistent,
    @Self.SetColorForPersistent,
    Self.FColor
  );
  FillChar(Self.TrackEntries, SizeOf(PspTrackEntry) * Length(TrackEntries), 0);
end;

destructor TCastleSpine.Destroy;
begin
  Self.Cleanup;
  Self.FParametersList.Free;
  Self.FColorPersistent.Free;
  Self.FExposeTransforms.Free;
  Self.FSkins.Free;
  Self.FControlBoneList.Free;
  Self.FAnimationsList.Free;
  Self.FAutoAnimations.Free;
  Self.FSkinsList.Free;
  inherited;
end;

{$ifdef CASTLE_DESIGN_MODE}
function TCastleSpine.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'ExposeTransforms')
    or (PropertyName = 'AutoAnimations')
    or (PropertyName = 'AutoAnimationsLoop')
    or (PropertyName = 'DefaultAnimationTransition')
    or (PropertyName = 'ProcessEvents')
    or (PropertyName = 'TimePlaying')
    or (PropertyName = 'TimePlayingSpeed')
    or (PropertyName = 'Skins')
    or (PropertyName = 'URL') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;
{$endif}

procedure TCastleSpine.LoadSpine(const AURL: String);
begin
  Self.FURL := AURL;
  Self.FIsNeedRefresh := True;
  if Self.FIsGLContextInitialized then
    Self.InternalLoadSpine;
end;

procedure TCastleSpine.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  F: Single;
  D: TCastleSpineControlBone;
begin
  inherited;
  CurrentSpineInstance := Self;
  Self.GLContextOpen;

  if not Self.Exists then
    Exit;

  if Self.FIsNeedRefresh then
    Self.InternalLoadSpine;
  if Self.FIsNeedRefreshAnimation then
    Self.InternalPlayAnimation;
  if Self.FIsNeedRefreshBones then
    Self.InternalExposeTransformsChange;

  RemoveMe := rtNone;
  // Update
  if Self.FIsGLContextInitialized and (Self.FspAnimationState <> nil) then
  begin
    if Self.TimePlaying then
      F := SecondsPassed * Self.TimePlayingSpeed
    else
      F := SecondsPassed;
    Inc(Self.FTicks);
    Self.FSecondsPassedAcc := Self.FSecondsPassedAcc + F;
    if Self.ProcessEvents then
    begin
      if (Self.FTicks > Self.AnimateSkipTicks) and ((Self.AnimateOnlyWhenVisible and Self.Visible) or (not Self.AnimateOnlyWhenVisible)) then
      begin
        if Self.FIsAnimationPlaying then
        begin
          spAnimationState_update(Self.FspAnimationState, Self.FSecondsPassedAcc);
          spAnimationState_apply(Self.FspAnimationState, Self.FspSkeleton);
        end;
        // Override bone values
        if Self.FControlBoneList.Count > 0 then
        begin
          for D in Self.FControlBoneList do
          begin
            D.Bone^.x := D.X;
            D.Bone^.y := D.Y;
            D.Bone^.rotation := D.Rotation;
          end;
        end;
        spSkeleton_update(Self.FspSkeleton, SecondsPassed);
        spSkeleton_updateWorldTransform(Self.FspSkeleton, SP_PHYSICS_UPDATE);
        spSkeletonBounds_update(Self.FspSkeletonBounds, Self.FspSkeleton, True);
        Self.FControlBoneList.Clear;
        Self.FTicks := 0;
        Self.FSecondsPassedAcc := 0;
      end;
    end;
  end;
  CurrentSpineInstance := nil;
end;

procedure TCastleSpine.LocalRender(const Params: TRenderParams);

  procedure RenderSkeleton(const Skeleton: PspSkeleton);

    procedure AddVertex(const X, Y, U, V: Single; const Color: TVector4; var Indx: Cardinal); inline;
    var
      P: PCastleSpineVertex;
    begin
      P := @SpineVertices[Indx];
      P^.Vertex := Vector2(X, Y);
      P^.TexCoord := Vector2(U, V);
      P^.Color := Color;
      Inc(Indx);
    end;

  var
    I, J, Indx: Integer;
    Attachment: PspAttachment;
    RegionAttachment: PspRegionAttachment;
    MeshAttachment: PspMeshAttachment;
    ClipAttachment: PspClippingAttachment;
    Slot: PspSlot;
    TotalVertexCount: Cardinal;
    PreviousImage: TDrawableImage = nil;
    Image: TDrawableImage;
    PreviousBlendMode: Integer = -1;
    AttachmentColor: TspColor;
    Color: TVector4;
    VertexCount,
    IndexCount: Cardinal;
    VertexPtr: PSingle;
    IndexPtr: PWord;
    UVPtr: PSingle;

    procedure Render; inline;
    begin
      if TotalVertexCount = 0 then
        Exit;
      // Render result
      //if Image.SmoothScaling <> Self.FSmoothTexture then
      //  Image.SmoothScaling := Self.FSmoothTexture;

      glBindTexture(GL_TEXTURE_2D, Image.Texture);
      if Self.FSmoothTexture then
      begin
        if Self.FMipmap then
        begin
          glGenerateMipmap(GL_TEXTURE_2D);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
        end else
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      end else
      begin
        if Self.FMipmap then
        begin
          glGenerateMipmap(GL_TEXTURE_2D);
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST_MIPMAP_LINEAR);
        end else
          glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      end;

      glBufferSubData(GL_ARRAY_BUFFER, 0, TotalVertexCount * SizeOf(TCastleSpineVertex), @SpineVertices[0]);

      glDrawArrays(GL_TRIANGLES, 0, TotalVertexCount);

      TotalVertexCount := 0;
      //if not Self.ExcludeFromStatistics then
      begin
        Inc(Params.Statistics.ShapesRendered);
        Inc(Params.Statistics.ShapesVisible);
      end;
    end;

  begin
    TotalVertexCount := 0;

    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastleSpineVertex), Pointer(0));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, SizeOf(TCastleSpineVertex), Pointer(8));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 4, GL_FLOAT, GL_FALSE, SizeOf(TCastleSpineVertex), Pointer(16));

    for J := 0 to Skeleton^.slotsCount - 1 do
    begin
      Slot := Skeleton^.drawOrder[J];
      Attachment := Slot^.Attachment;

      if Attachment = nil then Continue;
      if (Slot^.color.a = 0) or (not Slot^.bone^.active) then
      begin
        spSkeletonClipping_clipEnd(Self.FspClipper, Slot);
        Continue;
      end;

      case Attachment^.type_ of
        SP_ATTACHMENT_REGION:
          begin
            RegionAttachment := PspRegionAttachment(Attachment);
            AttachmentColor := RegionAttachment^.color;
            if AttachmentColor.a = 0 then
            begin
              spSkeletonClipping_clipEnd(Self.FspClipper, Slot);
              Continue;
            end;
            Image := TDrawableImage(PspAtlasRegion(RegionAttachment^.rendererObject)^.page^.rendererObject);
            spRegionAttachment_computeWorldVertices(RegionAttachment, Slot, @WorldVerticesPositions[0], 0, 2);
            if PreviousImage = nil then
            begin
              PreviousImage := Image;
            end;
            VertexCount := 4;
            IndexCount := 6;
            VertexPtr := @WorldVerticesPositions[0];
            IndexPtr := @RegionIndices[0];
            UVPtr := RegionAttachment^.uvs;
          end;
        SP_ATTACHMENT_MESH:
          begin
            MeshAttachment := PspMeshAttachment(Attachment);
            AttachmentColor := MeshAttachment^.color;
            if (MeshAttachment^.super.worldVerticesLength > High(WorldVerticesPositions)) then continue;
            if AttachmentColor.a = 0 then
            begin
              spSkeletonClipping_clipEnd(Self.FspClipper, Slot);
              Continue;
            end;
            Image := TDrawableImage(PspAtlasRegion(MeshAttachment^.rendererObject)^.page^.rendererObject);
            spVertexAttachment_computeWorldVertices(@MeshAttachment^.super, Slot, 0, MeshAttachment^.Super.worldVerticesLength, @WorldVerticesPositions[0], 0, 2);
            if PreviousImage = nil then
            begin
              PreviousImage := Image;
            end;
            VertexCount := MeshAttachment^.super.worldVerticesLength shr 1;
            IndexCount := MeshAttachment^.trianglesCount;
            VertexPtr := @WorldVerticesPositions[0];
            IndexPtr := MeshAttachment^.triangles;
            UVPtr := MeshAttachment^.uvs;
          end;
        SP_ATTACHMENT_CLIPPING:
          begin
            ClipAttachment := PspClippingAttachment(Attachment);
            spSkeletonClipping_clipStart(Self.FspClipper, Slot, ClipAttachment);
            Continue;
          end;
        else
          Continue;
      end;

      // Flush the current pipeline if material change
      if (PreviousBlendMode <> Integer(Slot^.data^.blendMode)) or (PreviousImage <> Image) then
      begin
        Render;
        // Blend mode
        if Integer(Slot^.data^.blendMode) <> PreviousBlendMode then
        begin
          if Self.FSpineData^.Atlas^.pages^.pma <> 0 then
          begin
            case Slot^.data^.blendMode of
              SP_BLEND_MODE_ADDITIVE:
                RenderContext.BlendingEnable(bsOne, bdOne);
              else
                RenderContext.BlendingEnable(bsOne, bdOneMinusSrcAlpha);
            end;
          end else
          begin
            case Slot^.data^.blendMode of
              SP_BLEND_MODE_ADDITIVE:
                RenderContext.BlendingEnable(bsSrcAlpha, bdOne);
              else
                RenderContext.BlendingEnable(bsSrcAlpha, bdOneMinusSrcAlpha);
            end;
          end;
        end;
        PreviousBlendMode := Integer(Slot^.data^.blendMode);
        PreviousImage := Image;
      end;

      Color := Vector4(
        Skeleton^.color.r * Slot^.color.r * AttachmentColor.r,
        Skeleton^.color.g * Slot^.color.g * AttachmentColor.g,
        Skeleton^.color.b * Slot^.color.b * AttachmentColor.b,
        Skeleton^.color.a * Slot^.color.a * AttachmentColor.a
      );

      if spSkeletonClipping_isClipping(Self.FspClipper) then
      begin
        spSkeletonClipping_clipTriangles(Self.FspClipper, VertexPtr, VertexCount shl 1, IndexPtr, IndexCount, UVPtr, 2);
        VertexPtr := Self.FspClipper^.clippedVertices^.items;
        VertexCount := Self.FspClipper^.clippedVertices^.size shr 1;
        UVPtr := Self.FspClipper^.clippedUVs^.items;
        IndexPtr := Self.FspClipper^.clippedTriangles^.items;
        IndexCount := Self.FspClipper^.clippedTriangles^.size;
      end;

      // Build mesh
      // TODO: Separate indices / vertices to save bandwidth
      for I := 0 to IndexCount - 1 do
      begin
        Indx := IndexPtr[I] shl 1;
        AddVertex(VertexPtr[Indx], VertexPtr[Indx + 1],
            UVPtr[Indx], 1 - UVPtr[Indx + 1],
            Color, TotalVertexCount);
      end;

      spSkeletonClipping_clipEnd(Self.FspClipper, Slot);
    end;
    Render;
    PreviousImage := Image;
    PreviousBlendMode := Integer(Slot^.data^.blendMode);
    spSkeletonClipping_clipEnd2(Self.FspClipper);

    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindTexture(GL_TEXTURE_2D, 0);
    //if not Self.ExcludeFromStatistics then
    begin
      Inc(Params.Statistics.ScenesRendered);
    end;
  end;

var
  PreviousProgram: TGLSLProgram;
  Fog: TFogFunctionality;
  RenderCameraPosition: TVector3;
  RelativeBBox: TBox3D;
begin
  inherited;
  if Self.FspAnimationState = nil then
    Exit;
  if not Self.FIsGLContextInitialized then
    Exit;
  if (not Self.Visible) or (not Self.Exists) or Params.InShadow or (not Params.Transparent) or (Params.StencilTest > 0) then
    Exit;

  //if not Self.ExcludeFromStatistics then
  begin
    Inc(Params.Statistics.ScenesVisible);
  end;
  if DistanceCulling > 0 then
  begin
    RenderCameraPosition := Params.InverseTransform^.MultPoint(Params.RenderingCamera.Camera.Position);
    if RenderCameraPosition.Length > DistanceCulling + LocalBoundingBox.Radius then
      Exit;
  end;
  if Self.FspSkeletonBounds^.minX < Self.FspSkeletonBounds^.maxX then
  begin
    RelativeBBox := Box3D(
      Vector3(Self.FspSkeletonBounds^.minX, Self.FspSkeletonBounds^.minY, -0.0001),
      Vector3(Self.FspSkeletonBounds^.maxX, Self.FspSkeletonBounds^.maxY, 0.0001)
    );
    if not Params.Frustum^.Box3DCollisionPossibleSimple(RelativeBBox) then
      Exit;
  end;

  PreviousProgram := RenderContext.CurrentProgram;
  Self.FShader.Enable;

  Self.FShader.Uniform('mvMatrix').SetValue(Params.RenderingCamera.Matrix * Params.Transform^);
  Self.FShader.Uniform('pMatrix').SetValue(RenderContext.ProjectionMatrix);
  Self.FShader.Uniform('color').SetValue(Self.FColor);
  if Self.FEnableFog and (Params.GlobalFog <> nil) then
  begin
    Fog := (Params.GlobalFog as TFogNode).Functionality(TFogFunctionality) as TFogFunctionality;
    Self.FShader.Uniform('fogEnable').SetValue(1);
    Self.FShader.Uniform('fogEnd').SetValue(Fog.VisibilityRange);
    Self.FShader.Uniform('fogColor').SetValue(Fog.Color);
  end else
    Self.FShader.Uniform('fogEnable').SetValue(0);

  glEnable(GL_DEPTH_TEST);
  glDepthMask(GL_FALSE);
  glActiveTexture(GL_TEXTURE0);

  RenderSkeleton(Self.FspSkeleton);

  glDisable(GL_DEPTH_TEST);
  glDepthMask(GL_TRUE);

  PreviousProgram.Enable;
end;

function TCastleSpine.LocalBoundingBox: TBox3D;
begin
  if (Self.FspSkeletonBounds <> nil) and Exists then
  begin
    Result := Box3D(
      Vector3(Self.FspSkeletonBounds^.minX, Self.FspSkeletonBounds^.minY, -0.0001),
      Vector3(Self.FspSkeletonBounds^.maxX, Self.FspSkeletonBounds^.maxY, 0.0001)
    );
  end else
    Result := TBox3D.Empty;
  Result.Include(inherited LocalBoundingBox);
end;

function TCastleSpine.PlayAnimation(const Parameters: TCastleSpinePlayAnimationParameters): boolean;
begin
  Self.FParametersList.Add(Parameters);
  Self.FIsAnimationPlaying := True;
  Self.FIsNeedRefreshAnimation := True;
  Result := True;
end;

function TCastleSpine.PlayAnimation(const AnimationName: string; const Loop: boolean; const Forward: boolean; const Track: Integer = 0): boolean;
var
  Parameters: TCastleSpinePlayAnimationParameters;
begin
  Parameters.Name := AnimationName;
  Parameters.Loop := Loop;
  Parameters.Forward := Forward;
  Parameters.TransitionDuration := Self.DefaultAnimationTransition;
  Parameters.InitialTime := 0;
  Parameters.TimeScale := 1;
  Parameters.Track := Track;
  Self.FParametersList.Add(Parameters);
  Self.FIsAnimationPlaying := True;
  Self.FIsNeedRefreshAnimation := True;
  Result := True;
end;

procedure TCastleSpine.StopAnimation(const Track: Integer = -1);
begin
  Self.FIsAnimationPlaying := False;
  if Track < 0 then
  begin
    spAnimationState_clearTracks(Self.FspAnimationState);
    spSkeleton_setToSetupPose(Self.FspSkeleton);
    FillChar(Self.TrackEntries, SizeOf(PspTrackEntry) * Length(TrackEntries), 0);
  end else
  begin
    spAnimationState_clearTrack(Self.FspAnimationState, Track);
    if Track < Length(Self.TrackEntries) then
      Self.TrackEntries[Track] := nil;
  end;
end;

function TCastleSpine.ContainsPoint(const X, Y: Single): TStrings;
var
  I: Integer;
begin
  Result := TStringList.Create;
  if spSkeletonBounds_aabbContainsPoint(Self.FspSkeletonBounds, X, Y) then
  begin
    for I := 0 to Self.FspSkeletonBounds^.count - 1 do
      if spPolygon_containsPoint(Self.FspSkeletonBounds^.polygons[I], X, Y) then
        Result.Add(Self.FspSkeletonBounds^.boundingBoxes[I]^.super^.super.name);
  end;
  if Result.Count = 0 then
    FreeAndNil(Result);
end;

function TCastleSpine.IntersectsSegment(const X1, Y1, X2, Y2: Single): TStrings;
var
  I: Integer;
begin
  Result := TStringList.Create;
  if spSkeletonBounds_aabbIntersectsSegment(Self.FspSkeletonBounds, X1, Y1, X2, Y2) then
  begin
    for I := 0 to Self.FspSkeletonBounds^.count - 1 do
      if spPolygon_intersectsSegment(Self.FspSkeletonBounds^.polygons[I], X1, Y1, X2, Y2) then
        Result.Add(Self.FspSkeletonBounds^.boundingBoxes[I]^.super^.super.name);
  end;
  if Result.Count = 0 then
    FreeAndNil(Result);
end;

procedure TCastleSpine.InternalPlayAnimation;

  function IsAnimationExists(const Parameters: TCastleSpinePlayAnimationParameters): Boolean;
  var
    I: Integer;
  begin
    for I := 0 to Self.FSpineData^.SkeletonData^.animationsCount - 1 do
      if Self.FSpineData^.SkeletonData^.animations[I]^.Name = Parameters.Name then
        Exit(True);
    Exit(False);
  end;

var
  TrackEntry: PspTrackEntry;
  Parameters: TCastleSpinePlayAnimationParameters;

begin
  if Self.FspAnimationState = nil then Exit;
  CurrentSpineInstance := Self;
  for Parameters in Self.FParametersList do
  begin
    if IsAnimationExists(Parameters) then
    begin
      if (Self.FParametersList.Count = 1) then
      begin
        if (Self.FPreviousAnimation <> '') and (Self.FPreviousAnimation <> Parameters.Name) then
          spAnimationStateData_setMixByName(Self.FSpineData^.AnimationStateData, PChar(Self.FPreviousAnimation), PChar(Parameters.Name), Parameters.TransitionDuration);
        Self.FPreviousAnimation := Parameters.Name;
      end else
        Self.FPreviousAnimation := '';
      TrackEntry := spAnimationState_setAnimationByName(Self.FspAnimationState, Parameters.Track, PChar(Parameters.Name), Parameters.Loop);
      TrackEntry^.reverse := Integer(not Parameters.Forward);
      TrackEntry^.trackTime := Parameters.InitialTime;
      TrackEntry^.timeScale := Parameters.TimeScale;
      if Parameters.Track < Length(Self.TrackEntries) then
        Self.TrackEntries[Parameters.Track] := TrackEntry;
    end;
  end;
  Self.FParametersList.Clear;
  Self.FIsNeedRefreshAnimation := False;
  CurrentSpineInstance := nil;
end;

initialization
  RegisterSerializableComponent(TCastleSpine, 'Spine');
  RegisterSerializableComponent(TCastleSpineTransformBehavior, 'Spine Transform Behavior');
  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(TStrings), TCastleSpine, 'ExposeTransforms',
    TExposeTransformsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TCastleSpine, 'AutoAnimations',
    TSpineAutoAnimationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSpine, 'URL',
    TSceneURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TCastleSpine, 'Skins',
    TSpineSkinPropertyEditor);
  {$endif}
  SpineDataCache := TCastleSpineDataCache.Create;

finalization
  SpineDataCache.Free;

end.
