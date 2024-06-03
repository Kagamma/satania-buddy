unit Spine;

{$macro on}
{$mode delphi}
{$define SPINECALL:=cdecl}
{$if defined(windows)}
  {$define SPINELIB:='libspine-c.dll'}
{$elseif defined(darwin)}
  {$define SPINELIB:='libspine-c.dylib'}
{$else}
  {$define SPINELIB:='libspine-c.so'}
{$endif}

interface

uses
  Classes,
  ctypes, dynlibs;

type
  {$packenum 4}
  TspBlendMode = (
    SP_BLEND_MODE_NORMAL, SP_BLEND_MODE_ADDITIVE, SP_BLEND_MODE_MULTIPLY, SP_BLEND_MODE_SCREEN
  );
  TspMixBlend = (
    SP_MIX_BLEND_SETUP,
    SP_MIX_BLEND_FIRST,
    SP_MIX_BLEND_REPLACE,
    SP_MIX_BLEND_ADD
  );
  TspAttachmentType = (
    SP_ATTACHMENT_REGION,
    SP_ATTACHMENT_BOUNDING_BOX,
    SP_ATTACHMENT_MESH,
    SP_ATTACHMENT_LINKED_MESH,
    SP_ATTACHMENT_PATH,
    SP_ATTACHMENT_POINT,
    SP_ATTACHMENT_CLIPPING
  );
  TspAtlasFormat = (
    SP_ATLAS_UNKNOWN_FORMAT,
    SP_ATLAS_ALPHA,
    SP_ATLAS_INTENSITY,
    SP_ATLAS_LUMINANCE_ALPHA,
    SP_ATLAS_RGB565,
    SP_ATLAS_RGBA4444,
    SP_ATLAS_RGB888,
    SP_ATLAS_RGBA8888
  );
  TspAtlasFilter = (
    SP_ATLAS_UNKNOWN_FILTER,
    SP_ATLAS_NEAREST,
    SP_ATLAS_LINEAR,
    SP_ATLAS_MIPMAP,
    SP_ATLAS_MIPMAP_NEAREST_NEAREST,
    SP_ATLAS_MIPMAP_LINEAR_NEAREST,
    SP_ATLAS_MIPMAP_NEAREST_LINEAR,
    SP_ATLAS_MIPMAP_LINEAR_LINEAR
  );
  TspAtlasWrap = (
    SP_ATLAS_MIRROREDREPEAT,
    SP_ATLAS_CLAMPTOEDGE,
    SP_ATLAS_REPEAT
  );
  TspInherit = (
    SP_INHERIT_NORMAL,
    SP_INHERIT_ONLYTRANSLATION,
    SP_INHERIT_NOROTATIONORREFLECTION,
    SP_INHERIT_NOSCALE,
    SP_INHERIT_NOSCALEORREFLECTION
  );
  TspEventType = (
    SP_ANIMATION_START,
    SP_ANIMATION_INTERRUPT,
    SP_ANIMATION_END,
    SP_ANIMATION_COMPLETE,
    SP_ANIMATION_DISPOSE,
    SP_ANIMATION_EVENT
  );
  TspPhysics = (
    SP_PHYSICS_NONE,
    SP_PHYSICS_RESET,
    SP_PHYSICS_UPDATE,
    SP_PHYSICS_POSE
  );
  {$packenum 1}

  PspFloatArray = ^TspFloatArray;
  PspIntArray = ^TspIntArray;
  PspUnsignedShortArray = ^TspUnsignedShortArray;
  PspTrackEntryArray = ^TspTrackEntryArray;
  PspKeyValueArray = Pointer;
  PspAttachmentLoader = Pointer;
  PspSkeletonJson = ^TspSkeletonJson;
  PspSkeletonBinary = Pointer;
  PspBoneData = ^TspBoneData;
  PspSkin = ^TspSkin;
  PspTimelineArray = Pointer;
  PspPropertyIdArray = Pointer;
  PspBoundingBoxAttachment = ^TspBoundingBoxAttachment;
  PspPolygon = Pointer;
  PspClippingAttachment = Pointer;
  PspTriangulator = Pointer;
  PspSkeleton = ^TSpSkeleton;
  PspTrackEntry = ^TspTrackEntry;
  PspEvent = ^TspEvent;
  PspAnimationState = ^TspAnimationState;
  PspAnimationStateListener = Pointer;
  PspEventData = ^TspEventData;
  PspSkeletonBounds = ^TspSkeletonBounds;
  PspAnimation = ^TspAnimation;
  PspIkConstraintData = Pointer;
  PspTransformConstraintData = Pointer;
  PspPathConstraintData = Pointer;
  PspSkeletonData = ^TspSkeletonData;
  PspRegionAttachment = ^TspRegionAttachment;
  PspAtlas = ^TspAtlas;
  PspAtlasPage = ^TspAtlasPage;
  PspAtlasRegion = ^TspAtlasRegion;
  PspBone = ^TspBone;
  PPspBone = ^PspBone;
  PspIkConstraint = Pointer;
  PPspIkConstraint = ^PspIkConstraint;
  PspTransformConstraint = Pointer;
  PPspTransformConstraint = ^PspTransformConstraint;
  PspPathConstraint = Pointer;
  PspTextureRegion = ^TspTextureRegion;
  PPspPathConstraint = ^PspPathConstraint;
  PspPhysicsConstraint = Pointer;
  PPspPhysicsConstraint = ^PspPhysicsConstraint;
  PspSlot = ^TspSlot;
  PPspSlot = ^PspSlot;
  PspVertexAttachment = ^TspVertexAttachment;
  PspMeshAttachment = ^TspMeshAttachment;
  PspSequence = Pointer;
  PspBoneDataArray = Pointer;
  PspIkConstraintDataArray = Pointer;
  PspTransformConstraintDataArray = Pointer;
  PspPathConstraintDataArray = Pointer;
  PspPhysicsConstraintData = Pointer;
  PspPhysicsConstraintDataArray = Pointer;

  TspColor = record
    r, g, b, a: cfloat;
  end;
  PspColor = ^TspColor;

  TspIntArray = record
    size: cint;
    capacity: cint;
    items: pcint;
  end;

  TspFloatArray = record
    size: cint;
    capacity: cint;
    items: pcfloat;
  end;

  TspUnsignedShortArray = record
    size: cint;
    capacity: cint;
    items: pcushort;
  end;

  TspTrackEntryArray = record
    size: cint;
    capacity: cint;
    items: ^PspTrackEntry;
  end;

  TspSkeletonJson = record
    scale: cfloat;
    attachmentLoader: PspAttachmentLoader;
    error: PChar;
  end;

  TspBoundingBoxAttachment = record
    super: PspVertexAttachment;
    color: TspColor;
  end;

  TspSkin = record
    name: PChar;
    bones: PspBoneDataArray;
    ikConstraints: PspIkConstraintDataArray;
    transformConstraints: PspTransformConstraintDataArray;
    pathConstraints: PspPathConstraintDataArray;
    physicsConstraints: PspPhysicsConstraintDataArray;
    color: TspColor;
  end;

  TspBoneData = record
    index_: cint;
    name: PChar;
    parent: PspBoneData;
    length: cfloat;
    x, y, rotation, scaleX, scaleY, shearX, shearY: cfloat;
    inherit: TspInherit;
    skinRequired: cbool;
    color: TspColor;
    icon: PChar;
    visible: cint;
  end;

  TspTextureRegion = record
    rendererObject: Pointer;
    u, v, u2, v2: cfloat;
    degrees: cint;
    offsetX, offsetY: cfloat;
    width, height: cint;
    originalWidth, originalHeight: cint;
  end;

  PspSkeletonClipping = ^TspSkeletonClipping;
  TspSkeletonClipping = record
    triangulator: PspTriangulator;
    clippingPolygon: PspFloatArray;
    clipOutput: PspFloatArray;
    clippedVertices: PspFloatArray;
    clippedUVs: PspFloatArray;
    clippedTriangles: PspUnsignedShortArray;
    scratch: PspFloatArray;
    clipAttachment: PspClippingAttachment;
    clippingPolygons: ^PspFloatArray;
  end;

  TspSkeletonBounds = record
    count: cint;
    boundingBoxes: ^PspBoundingBoxAttachment;
    polygons: ^PspPolygon;
    minX, minY, maxX, maxY: cfloat;
  end;

  TspAnimation = record
    name: PChar;
    duration: cfloat;
    timelines: PspTimelineArray;
    timelineIds: PspPropertyIdArray;
  end;

  TspSlotData = record
    index_: cint;
    name: PChar;
    boneData: PspBoneData;
    attachmentName: PChar;
    color: TspColor;
    darkColor: PspColor;
    blendMode: TspBlendMode;
    visible: cint;
    path: PChar;
  end;
  PspSlotData = ^TspSlotData;

  TspSkeletonData = record
    version: PChar;
    hash: PChar;
    x, y, width, height: cfloat;
    referenceScale: cfloat;
    fps: cfloat;
    imagesPath: PChar;
    audioPath: PChar;
    stringsCount: cint;
    strings: ^PChar;
    bonesCount: cint;
    bones: ^PspBoneData;
    slotsCount: cint;
    slots: ^PspSlotData;
    skinsCount: cint;
    skins: ^PspSkin;
    defaultSkin: PspSkin;
    eventsCount: cint;
    events: ^PspEventData;
    animationsCount: cint;
    animations: ^PspAnimation;
    ikConstraintsCount: cint;
    ikConstraints: ^PspIkConstraintData;
    transformConstraintsCount: cint;
    transformConstraints: ^PspTransformConstraintData;
    pathConstraintsCount: cint;
    pathConstraints: ^PspPathConstraintData;
    physicsConstraintsCount: cint;
    physicsConstraints: ^PspPhysicsConstraintData;
  end;

  PspAnimationStateData = Pointer;
  TspAnimationState = record
    data: PspAnimationStateData;
    tracksCount: cint;
    tracks: PspTrackEntry;
    listener: PspAnimationStateListener;
    timeScale: cfloat;
    rendererObject: Pointer;
    userData: Pointer;
    unkeyedState: cint;
  end;

  TspAttachment = record
    name: Pchar;
    type_: TspAttachmentType;
    vtable: Pointer;
    refCount: cint;
    attachmentLoader: PspAttachmentLoader;
  end;
  PspAttachment = ^TspAttachment;

  TspRegionAttachment = record
    super: TspAttachment;
    path: PChar;
    x, y, scaleX, scaleY, rotation, width, height: cfloat;
    color: TspColor;
    rendererObject: Pointer;
    region: PspTextureRegion;
    sequence: PspSequence;
    offset: array[0..7] of cfloat;
    uvs: array[0..7] of cfloat;
  end;

  TspAtlasPage = record
    atlas: PspAtlas;
    name: PChar;
    format: TspAtlasFormat;
    minFilter, magFilter: TspAtlasFilter;
    uWrap, vWrap: TspAtlasWrap;
    rendererObject: Pointer;
    width, height: cint;
    pma: cint;
    next: PspAtlasPage;
  end;

  TspAtlasRegion = record
    super: TspTextureRegion;
    name: PChar;
    x, y: cint;
    index_: cint;
    splits: pcint;
    pads: pcint;
    keyValues: PspKeyValueArray;
    page: PspAtlasPage;
    next: PspAtlasRegion;
  end;

  TspAtlas = record
    pages: PspAtlasPage;
    regions: PspAtlasRegion;
    rendererObject: Pointer;
  end;

  TspBone = record
    data: PspBoneData;
    skeleton: PspSkeleton;
    parent: PspBone;
    childrenCount: cint;
    children: PPspBone;
    x, y, rotation, scaleX, scaleY, shearX, shearY: cfloat;
    ax, ay, arotation, ascaleX, ascaleY, ashearX, ashearY: cfloat;
    a, b, worldX: cfloat;
    c, d, worldY: cfloat;
    sorted: cbool;
    active: cbool;
    inherit: TspInherit;
  end;

  TspSlot = record
    data: PspSlotData;
    bone: PspBone;
    color: TspColor;
    darkColor: PspColor;
    attachment: PspAttachment;
    attachmentState: cint;
    deformCapacity: cint;
    deformCount: cint;
    deform: pcfloat;
    sequenceIndex: cint;
  end;

  TspSkeleton = record
    data: PspSkeletonData;

    bonesCount: cint;
    bones: PPspBone;
    root: PspBone;

    slotsCount: cint;
    slots: PPspSlot;
    drawOrder: PPspSlot;

    ikConstraintsCount: cint;
    ikConstraints: PPspIkConstraint;
    transformConstraintsCount: cint;
    transformConstraints: PPspTransformConstraint;
    pathConstraintsCount: cint;
    pathConstraints: PPspPathConstraint;
    physicsConstraintsCount: cint;
    physicsConstraints: PPspPhysicsConstraint;
    skin: PspSkin;
    color: TspColor;
    scaleX, scaleY: cfloat;
    x, y: cfloat;
    time: cfloat;
  end;

  TspVertexAttachment = record
    super: TspAttachment;
    bonesCount: cint;
    bones: pcint;
    verticesCount: cint;
    vertices: pcfloat;
    worldVerticesLength: cint;
    timelineAttachment: PspAttachment;
    id: cint;
  end;

  TspMeshAttachment = record
    super: TspVertexAttachment;
    rendererObject: Pointer;
    region: PspTextureRegion;
    sequence: PspSequence;
    path: PChar;
    regionUVs: pcfloat;
    uvs: pcfloat;
    trianglesCount: cint;
    triangles: pcushort;
    color: TspColor;
    hullLength: cint;
    parentMesh: PspMeshAttachment;
    edgesCount: cint;
    edges: pcushort;
    width, height: cfloat;
  end;

  TspTrackEntry = record
    animation: PspAnimation;
    previous, next, mixingFrom, mixingTo: PspTrackEntry;
    listener: Pointer;
    trackIndex: cint;
    loop, holdPrevious, reverse, shortestRotation: cint;
    eventThreshold, mixAttachmentThreshold, alphaAttachmentThreshold, mixDrawOrdertThreshold: cfloat;
    animationStart, animationEnd, animationLast, nextAnimationLast: cfloat;
    delay, trackTime, trackLast, nextTrackLast, trackEnd, timeScale: cfloat;
    alpha, mixTime, mixDuration, interruptAlpha, totalAlpha: cfloat;
    mixBlend: TspMixBlend;
    timelineMode: PspIntArray;
    timelineHoldMix: PspTrackEntryArray;
    timelinesRotation: pcfloat;
    timelinesRotationCount: cint;
    rendererObject: Pointer;
    userData: Pointer;
  end;

  TspEvent = record
    data: PspEventData;
    time: cfloat;
    intValue: cint;
    floatValue: cfloat;
    stringValue: PChar;
    volume: cfloat;
    balance: cfloat;
  end;

  TspEventData = record
    name: PChar;
    intValue: cint;
    floatValue: cfloat;
    stringValue: PChar;
    audioPath: PChar;
    volume: cfloat;
    balance: cfloat;
  end;

var
  // ----- Loader -----
  { FileName: PWideChar; Data: Pointer; var Size: cuint32 }
  Spine_Loader_RegisterLoadRoutine: procedure(Func: Pointer); SPINECALL;
  Spine_Loader_RegisterLoadTextureRoutine: procedure(Func: Pointer); SPINECALL;
  Spine_Loader_RegisterFreeTextureRoutine: procedure(Func: Pointer); SPINECALL;

  // Memory management
  Spine_MM_Malloc: procedure(Func: Pointer); SPINECALL;
  Spine_MM_ReAlloc: procedure(Func: Pointer); SPINECALL;
  Spine_MM_Free: procedure(Func: Pointer); SPINECALL;
  _spMalloc: function(Size: csize_t; F: PChar; Line: cint): Pointer;

  // Atlas
  spAtlas_create: function(Data: Pointer; Len: cint; Dir: PChar; rendererObject: Pointer): PspAtlas; SPINECALL;
  spAtlas_dispose: procedure(Atlas: PspAtlas); SPINECALL;

  // Skeleton
  spSkeletonBinary_create: function(Atlas: PspAtlas): PspSkeletonBinary; SPINECALL;
  spSkeletonBinary_readSkeletonData: function(SkeletonBinary: PspSkeletonBinary; Data: Pointer; Len: Integer): PspSkeletonBinary; SPINECALL;
  spSkeletonBinary_dispose: procedure(SkeletonBinary: PspSkeletonBinary); SPINECALL;
  spSkeletonJson_create: function(Atlas: PspAtlas): PspSkeletonJson; SPINECALL;
  spSkeletonJson_readSkeletonData: function(SkeletonJson: PspSkeletonJson; Data: PChar): PspSkeletonData; SPINECALL;
  spSkeletonJson_dispose: procedure(SkeletonJson: PspSkeletonJson); SPINECALL;
  spSkeleton_create: function(SkeletonData: PspSkeletonData): PspSkeleton; SPINECALL;
  spSkeleton_dispose: procedure(Skeleton: PspSkeleton); SPINECALL;
  spSkeletonData_dispose: procedure(SkeletonData: PspSkeletonData); SPINECALL;
  spSkeletonData_findSkin: function(SkeletonJson: PspSkeletonJson; Name: PChar): PspSkin; SPINECALL;
  spSkeleton_updateWorldTransform: procedure(Skeleton: PspSkeleton; Physics: TspPhysics); SPINECALL;
  spSkeleton_update: procedure(Skeleton: PspSkeleton; Delta: cfloat); SPINECALL;
  spSkeleton_findBone: function(Skeleton: PspSkeleton; Name: PChar): PspBone; SPINECALL;
  spSkeleton_setSkin: procedure(Skeleton: PspSkeleton; Skin: PspSkin); SPINECALL;
  spSkeleton_setSkinByName: function(Skeleton: PspSkeleton; Name: PChar): cint; SPINECALL;
  spSkeleton_setToSetupPose: procedure(Skeleton: PspSkeleton); SPINECALL;
  spSkeleton_setSlotsToSetupPose: procedure(Skeleton: PspSkeleton); SPINECALL;
  spSkeleton_setBonesToSetupPose: procedure(Skeleton: PspSkeleton); SPINECALL;

  // Polygon
  spPolygon_containsPoint: function(Polygon: PspPolygon; X, Y: cfloat): cbool; SPINECALL;
  spPolygon_intersectsSegment: function(Polygon: PspPolygon; X1, Y1, X2, Y2: cfloat): cbool; SPINECALL;

  // Skin
  spSkin_create: function(Name: PChar): PspSkin; SPINECALL;
  spSkin_dispose: procedure(Skin: PspSkin); SPINECALL;
  spSkin_addSkin: procedure(Skin, Other: PspSkin); SPINECALL;
  spSkin_clear: procedure(Skin: PspSkin); SPINECALL;

  // Bone
  spBone_getWorldRotationX: function(Bone: PspBone): cfloat; SPINECALL;
  spBone_getWorldRotationY: function(Bone: PspBone): cfloat; SPINECALL;
  spBone_getWorldScaleX: function(Bone: PspBone): cfloat; SPINECALL;
  spBone_getWorldScaleY: function(Bone: PspBone): cfloat; SPINECALL;
  spBone_localToWorld: procedure(Bone: PspBone; LocalX, LocalY: cfloat; WorldX, WorldY: pcfloat); SPINECALL;
  spBone_worldToLocal: procedure(Bone: PspBone; WorldX, WorldY: cfloat; LocalX, LocalY: pcfloat); SPINECALL;
  spBone_worldToLocalRotationX: function(Bone: PspBone): cfloat; SPINECALL;
  spBone_updateWorldTransform: procedure(Bone: PspBone); SPINECALL;
  spBone_setToSetupPose: procedure(Bone: PspBone); SPINECALL;

  // Animation
  spAnimationStateData_create: function(SkeletonData: PspSkeletonData): PspAnimationStateData; SPINECALL;
  spAnimationStateData_dispose: procedure(AnimationStateData: PspAnimationStateData); SPINECALL;
  spAnimationStateData_setMixByName: procedure(AnimationStateData: PspAnimationStateData; FromName, ToName: PChar; Duration: cfloat); SPINECALL;
  spAnimationState_setAnimationByName: function(AnimationState: PspAnimationState; TrackIndex: cint; AnimationName: PChar; Loop: cbool): PspTrackEntry; SPINECALL;
  spAnimationState_update: procedure(AnimationState: PspAnimationState; Delta: cfloat); SPINECALL;
  spAnimationState_apply: procedure(AnimationState: PspAnimationState; Skeleton: PspSkeleton); SPINECALL;
  spAnimationState_create: function(Data: PspAnimationStateData): PspAnimationState; SPINECALL;
  spAnimationState_dispose: procedure(Data: PspAnimationState); SPINECALL;
  spAnimationState_clearTrack: procedure(Data: PspAnimationState; TrackIndex: cint); SPINECALL;
  spAnimationState_clearTracks: procedure(Data: PspAnimationState); SPINECALL;

  // BoundingBox
  spSkeletonBounds_create: function: PspSkeletonBounds; SPINECALL;
  spSkeletonBounds_dispose: procedure(Bounds: PspSkeletonBounds); SPINECALL;
  spSkeletonBounds_update: procedure(Bounds: PspSkeletonBounds; Skeleton: PspSkeleton; updateAabb: cbool); SPINECALL;
  spSkeletonBounds_aabbContainsPoint: function(Bounds: PspSkeletonBounds; X, Y: cfloat): cbool; SPINECALL;
  spSkeletonBounds_containsPoint: function(Bounds: PspSkeletonBounds; X, Y: cfloat): PspBoundingBoxAttachment; SPINECALL;
  spSkeletonBounds_aabbIntersectsSegment: function(Bounds: PspSkeletonBounds; X1, Y1, X2, Y2: cfloat): cbool; SPINECALL;
  spSkeletonBounds_intersectsSegment: function(Bounds: PspSkeletonBounds; X1, Y1, X2, Y2: cfloat): PspBoundingBoxAttachment; SPINECALL;

  // Attachment
  spRegionAttachment_computeWorldVertices: procedure(This: PspRegionAttachment; Slot: PspSlot; Vertices: pcfloat; Offset, Stride: cint); SPINECALL;
  spVertexAttachment_computeWorldVertices: procedure(This: PspVertexAttachment; Slot: PspSlot; Start, Count: cint; Vertices: pcfloat; Offset, Stride: cint); SPINECALL;

  // Clip
  spSkeletonClipping_create: function: PspSkeletonClipping; SPINECALL;
  spSkeletonClipping_dispose: procedure(Clip: PspSkeletonClipping); SPINECALL;
  spSkeletonClipping_clipStart: function(This: PspSkeletonClipping; Slot: PspSlot; Attachment: PspClippingAttachment): cint; SPINECALL;
  spSkeletonClipping_clipTriangles: procedure(This: PspSkeletonClipping; vertices: pcfloat; vertLen: cint; triangles: pcushort; triLen: cint; uvs: pcfloat; stride: cint); SPINECALL;
  spSkeletonClipping_isClipping: function(This: PspSkeletonClipping): cbool; SPINECALL;
  spSkeletonClipping_clipEnd: procedure(This: PspSkeletonClipping; Slot: PspSlot); SPINECALL;
  spSkeletonClipping_clipEnd2: procedure(This: PspSkeletonClipping); SPINECALL;

function Spine_Load: Boolean;

implementation

var
  Lib: TLibHandle = dynlibs.NilHandle;

function SpAlloc(Size: csize_t): Pointer; SPINECALL;
begin
  Result := AllocMem(Size);
end;

function SpReAlloc(P: Pointer; Size: csize_t): Pointer; SPINECALL;
begin
  Result := ReAllocMem(P, Size);
end;

procedure SpFree(P: Pointer); SPINECALL;
begin
  FreeMem(P);
end;

function Spine_Load: Boolean;
begin;
  // library already loaded, subsequent calls to Spine_Load do nothing
  if Lib <> dynlibs.NilHandle then Exit(True);

  Lib := LoadLibrary(SPINELIB);
  if Lib = dynlibs.NilHandle then Exit(False);

  Spine_Loader_RegisterLoadRoutine := GetProcedureAddress(Lib, 'Spine_Loader_RegisterLoadRoutine');
  Spine_Loader_RegisterLoadTextureRoutine := GetProcedureAddress(Lib, 'Spine_Loader_RegisterLoadTextureRoutine');
  Spine_Loader_RegisterFreeTextureRoutine := GetProcedureAddress(Lib, 'Spine_Loader_RegisterFreeTextureRoutine');

  // Memory management
  Spine_MM_Malloc := GetProcedureAddress(Lib, 'Spine_MM_Malloc');
  Spine_MM_ReAlloc := GetProcedureAddress(Lib, 'Spine_MM_ReAlloc');
  Spine_MM_Free := GetProcedureAddress(Lib, 'Spine_MM_Free');
  _spMalloc := GetProcedureAddress(Lib, '_spMalloc');

  // Atlas
  spAtlas_create := GetProcedureAddress(Lib, 'spAtlas_create');
  spAtlas_dispose := GetProcedureAddress(Lib, 'spAtlas_dispose');

  // Skeleton
  spSkeletonBinary_create := GetProcedureAddress(Lib, 'spSkeletonBinary_create');
  spSkeletonBinary_readSkeletonData := GetProcedureAddress(Lib, 'spSkeletonBinary_readSkeletonData');
  spSkeletonBinary_dispose := GetProcedureAddress(Lib, 'spSkeletonBinary_dispose');
  spSkeletonJson_create := GetProcedureAddress(Lib, 'spSkeletonJson_create');
  spSkeletonJson_readSkeletonData := GetProcedureAddress(Lib, 'spSkeletonJson_readSkeletonData');
  spSkeletonJson_dispose := GetProcedureAddress(Lib, 'spSkeletonJson_dispose');
  spSkeleton_create := GetProcedureAddress(Lib, 'spSkeleton_create');
  spSkeleton_dispose := GetProcedureAddress(Lib, 'spSkeleton_dispose');
  spSkeletonData_dispose := GetProcedureAddress(Lib, 'spSkeletonData_dispose');
  spSkeletonData_findSkin := GetProcedureAddress(Lib, 'spSkeletonData_findSkin');
  spSkeleton_updateWorldTransform := GetProcedureAddress(Lib, 'spSkeleton_updateWorldTransform');
  spSkeleton_update := GetProcedureAddress(Lib, 'spSkeleton_update');
  spSkeleton_findBone := GetProcedureAddress(Lib, 'spSkeleton_findBone');
  spSkeleton_setSkin := GetProcedureAddress(Lib, 'spSkeleton_setSkin');
  spSkeleton_setSkinByName := GetProcedureAddress(Lib, 'spSkeleton_setSkinByName');
  spSkeleton_setToSetupPose := GetProcedureAddress(Lib, 'spSkeleton_setToSetupPose');
  spSkeleton_setSlotsToSetupPose := GetProcedureAddress(Lib, 'spSkeleton_setSlotsToSetupPose');
  spSkeleton_setBonesToSetupPose := GetProcedureAddress(Lib, 'spSkeleton_setBonesToSetupPose');

  // Polygon
  spPolygon_containsPoint := GetProcedureAddress(Lib, 'spPolygon_containsPoint');
  spPolygon_intersectsSegment := GetProcedureAddress(Lib, 'spPolygon_intersectsSegment');

  // Skin
  spSkin_create := GetProcedureAddress(Lib, 'spSkin_create');
  spSkin_dispose := GetProcedureAddress(Lib, 'spSkin_dispose');
  spSkin_addSkin := GetProcedureAddress(Lib, 'spSkin_addSkin');
  spSkin_clear := GetProcedureAddress(Lib, 'spSkin_clear');

  // Bone
  spBone_getWorldRotationX := GetProcedureAddress(Lib, 'spBone_getWorldRotationX');
  spBone_getWorldRotationY := GetProcedureAddress(Lib, 'spBone_getWorldRotationY');
  spBone_getWorldScaleX := GetProcedureAddress(Lib, 'spBone_getWorldScaleX');
  spBone_getWorldScaleY := GetProcedureAddress(Lib, 'spBone_getWorldScaleY');
  spBone_localToWorld := GetProcedureAddress(Lib, 'spBone_localToWorld');
  spBone_worldToLocal := GetProcedureAddress(Lib, 'spBone_worldToLocal');
  spBone_worldToLocalRotationX := GetProcedureAddress(Lib, 'spBone_worldToLocalRotationX');
  spBone_updateWorldTransform := GetProcedureAddress(Lib, 'spBone_updateWorldTransform');
  spBone_setToSetupPose := GetProcedureAddress(Lib, 'spBone_setToSetupPose');

  // Animation
  spAnimationStateData_create := GetProcedureAddress(Lib, 'spAnimationStateData_create');
  spAnimationStateData_dispose := GetProcedureAddress(Lib, 'spAnimationStateData_dispose');
  spAnimationStateData_setMixByName := GetProcedureAddress(Lib, 'spAnimationStateData_setMixByName');
  spAnimationState_setAnimationByName := GetProcedureAddress(Lib, 'spAnimationState_setAnimationByName');
  spAnimationState_update := GetProcedureAddress(Lib, 'spAnimationState_update');
  spAnimationState_apply := GetProcedureAddress(Lib, 'spAnimationState_apply');
  spAnimationState_create := GetProcedureAddress(Lib, 'spAnimationState_create');
  spAnimationState_dispose := GetProcedureAddress(Lib, 'spAnimationState_dispose');
  spAnimationState_clearTrack := GetProcedureAddress(Lib, 'spAnimationState_clearTrack');
  spAnimationState_clearTracks := GetProcedureAddress(Lib, 'spAnimationState_clearTracks');

  // BoundingBox
  spSkeletonBounds_create := GetProcedureAddress(Lib, 'spSkeletonBounds_create');
  spSkeletonBounds_dispose := GetProcedureAddress(Lib, 'spSkeletonBounds_dispose');
  spSkeletonBounds_update := GetProcedureAddress(Lib, 'spSkeletonBounds_update');
  spSkeletonBounds_aabbContainsPoint := GetProcedureAddress(Lib, 'spSkeletonBounds_aabbContainsPoint');
  spSkeletonBounds_containsPoint := GetProcedureAddress(Lib, 'spSkeletonBounds_containsPoint');
  spSkeletonBounds_aabbIntersectsSegment := GetProcedureAddress(Lib, 'spSkeletonBounds_aabbIntersectsSegment');
  spSkeletonBounds_intersectsSegment := GetProcedureAddress(Lib, 'spSkeletonBounds_intersectsSegment');

  // Attachment
  spRegionAttachment_computeWorldVertices := GetProcedureAddress(Lib, 'spRegionAttachment_computeWorldVertices');
  spVertexAttachment_computeWorldVertices := GetProcedureAddress(Lib, 'spVertexAttachment_computeWorldVertices');

  // Clip
  spSkeletonClipping_create := GetProcedureAddress(Lib, 'spSkeletonClipping_create');
  spSkeletonClipping_dispose := GetProcedureAddress(Lib, 'spSkeletonClipping_dispose');
  spSkeletonClipping_clipStart := GetProcedureAddress(Lib, 'spSkeletonClipping_clipStart');
  spSkeletonClipping_clipTriangles := GetProcedureAddress(Lib, 'spSkeletonClipping_clipTriangles');
  spSkeletonClipping_isClipping := GetProcedureAddress(Lib, 'spSkeletonClipping_isClipping');
  spSkeletonClipping_clipEnd := GetProcedureAddress(Lib, 'spSkeletonClipping_clipEnd');
  spSkeletonClipping_clipEnd2 := GetProcedureAddress(Lib, 'spSkeletonClipping_clipEnd2');

  Spine_MM_Malloc(@SpAlloc);
  Spine_MM_ReAlloc(@SpReAlloc);
  Spine_MM_Free(@SpFree);

  Exit(True);
end;

end.