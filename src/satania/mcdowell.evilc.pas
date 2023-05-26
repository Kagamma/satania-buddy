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

unit Mcdowell.EvilC;

{$mode objfpc}
{$ifdef CPUX86_64}
  {$asmmode intel}
{$endif}
{$H+}
{$macro on}
{$modeswitch nestedprocvars}
{$modeswitch advancedrecords}
// enable this if you want to handle UTF-8 strings (requires LCL)
{$define SE_STRING_UTF8}
// use computed goto instead of case of
{$define SE_COMPUTED_GOTO}

interface

uses
  SysUtils, Classes, Generics.Collections, StrUtils, Types, DateUtils, RegExpr
  {$ifdef SE_STRING_UTF8},LazUTF8{$endif}{$ifdef CPU64}, dynlibs{$endif};

type
  TSENumber = Double;

  TSEOpcode = (
    opPushConst,
    opPushGlobalVar,
    opPushLocalVar,
    opPushArrayPop,
    opPopConst,
    opPopFrame,
    opAssignGlobalVar,
    opAssignGlobalArray,
    opAssignLocalVar,
    opAssignLocalArray,
    opJumpEqual,
    opJumpUnconditional,
    opJumpEqualOrGreater,
    opJumpEqualOrLesser,

    opOperatorAdd2,
    opOperatorSub2,
    opOperatorMul2,
    opOperatorDiv2,

    opOperatorAdd,
    opOperatorSub,
    opOperatorMul,
    opOperatorDiv,
    opOperatorMod,
    opOperatorPow,
    opOperatorNegative,
    opOperatorLesser,
    opOperatorLesserOrEqual,
    opOperatorGreater,
    opOperatorGreaterOrEqual,
    opOperatorEqual,
    opOperatorNotEqual,
    opOperatorAnd,
    opOperatorOr,
    opOperatorXor,
    opOperatorNot,

    opCallRef,
    opCallNative,
    opCallScript,
    opCallImport,
    opYield,
    opHlt
  );
  TSEOpcodes = set of TSEOpcode;
  TSEOpcodeInfo = record
    Op: TSEOpcode;
    Pos: Integer;
    Binary: Pointer;
    Size: Integer;
  end;
  PSEOpcodeInfo = ^TSEOpcodeInfo;
  TSEOpcodeInfoListAncestor = specialize TList<TSEOpcodeInfo>;
  TSEOpcodeInfoList = class(TSEOpcodeInfoListAncestor)
  public
    function Ptr(const P: Integer): PSEOpcodeInfo;
  end;

  TSENestedProc = procedure is nested;

  TSEValueKind = (
    sevkNull,
    sevkNumber,
    sevkString,
    sevkMap,
    sevkBuffer,
    sevkPointer,
    sevkBoolean,
    sevkFunction
  );
  PSECommonString = ^String;
  TSEBuffer = record
    Base: RawByteString;
    Ptr: Pointer;
  end;
  PSEBuffer = ^TSEBuffer;

  TSEFuncKind = (sefkNative, sefkScript, sefkImport);

  {$mode delphi}
  PSEValue = ^TSEValue;
  TSEValue = record
    Size, Ref: Cardinal;
    case Kind: TSEValueKind of
      sevkNumber:
        (
          VarNumber: TSENumber;
        );
      sevkString:
        (
          VarString: PSECommonString;
        );
      sevkMap:
        (
          VarMap: TObject;
        );
      sevkBuffer:
        (
          VarBuffer: PSEBuffer;
        );
      sevkPointer:
        (
          VarPointer: Pointer;
        );
      sevkNull:
        (
          VarNull: Pointer;
        );
      sevkBoolean:
        (
          VarBoolean: Boolean;
        );
      sevkFunction:
        (
          VarFuncKind: TSEFuncKind;
          VarFuncIndx: QWord;
        );
  end;
  {$mode objfpc}
  TSEValueList = specialize TList<TSEValue>;
  TSEValueMap = class(specialize TDictionary<String, TSEValue>)
  private
    FIsValidArray: Boolean;
    FList: TSEValueList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Set2(const Key: String; const AValue: TSEValue);
    procedure Del2(const Key: String);
    function Get2(const Key: String): TSEValue;
    property List: TSEValueList read FList;
    property IsValidArray: Boolean read FIsValidArray;
  end;
  TSEValueArray = array of TSEValue;
  PPSEValue = ^PSEValue;

  TSEGCValue = record
    Garbage: Boolean;
    Value: TSEValue;
    Lock: Boolean;
  end;
  TSEGCValueList = specialize TList<TSEGCValue>;
  TSEGCValueAvailList = specialize TList<Integer>;

  TSEGarbageCollector = class
  private
    FAllocatedMem: Int64;
    FValueList: TSEGCValueList;
    FValueAvailList: TSEGCValueAvailList;
    FTicks: QWord;
    procedure Sweep;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddToList(const PValue: PSEValue);
    procedure CheckForGC;
    procedure GC;
    procedure AllocBuffer(const PValue: PSEValue; const Size: Integer);
    procedure AllocMap(const PValue: PSEValue);
    procedure AllocString(const PValue: PSEValue; const S: String);
    procedure Lock(const PValue: PSEValue);
    procedure Unlock(const PValue: PSEValue);
    property ValueList: TSEGCValueList read FValueList;
    property AllocatedMem: Int64 read FAllocatedMem write FAllocatedMem;
  end;

  TSEAtomKind = (
    seakVoid,
    seakI8,
    seakI16,
    seakI32,
    seakI64,
    seakU8,
    seakU16,
    seakU32,
    seakU64,
   // seakF32,
    seakF64,
    seakBuffer,
    seakWBuffer
  );
  TSEAtomKindArray = array of TSEAtomKind;

  TSEVM = class;
  TSEVMList = specialize TList<TSEVM>;
  TSEFunc = function(const VM: TSEVM; const Args: array of TSEValue): TSEValue of object;

  TSEFuncNativeInfo = record
    Name: String;
    Func: TSEFunc;
    ArgCount: Integer;
  end;
  PSEFuncNativeInfo = ^TSEFuncNativeInfo;

  TSEFuncScriptInfo = record
    Name: String;
    BinaryPos: Integer;
    ArgCount: Integer;
    VarCount: Integer;
  end;
  PSEFuncScriptInfo = ^TSEFuncScriptInfo;

  TSEFuncImportInfo = record
    Name: String;
    Func: Pointer;
    Args: TSEAtomKindArray;
    Return: TSEAtomKind;
  end;
  PSEFuncImportInfo = ^TSEFuncImportInfo;

  TSEFuncNativeListAncestor = specialize TList<TSEFuncNativeInfo>;
  TSEFuncNativeList = class(TSEFuncNativeListAncestor)
  public
    function Ptr(const P: Integer): PSEFuncNativeInfo;
  end;

  TSEFuncScriptListAncestor = specialize TList<TSEFuncScriptInfo>;
  TSEFuncScriptList = class(TSEFuncScriptListAncestor)
  public
    function Ptr(const P: Integer): PSEFuncScriptInfo;
  end;

  TSEFuncImportListAncestor = specialize TList<TSEFuncImportInfo>;
  TSEFuncImportList = class(TSEFuncImportListAncestor)
  public
    function Ptr(const P: Integer): PSEFuncImportInfo;
  end;

  TSEBinaryAncestor = specialize TList<TSEValue>;
  TSEBinary = class(TSEBinaryAncestor)
  public
    function Ptr(const P: Integer): PSEValue;
  end;

  TSEConstMap = specialize TDictionary<String, TSEValue>;
  TSEStack = TSEBinaryAncestor;
  TSEVarMap = TSEConstMap;
  TSEListStack = specialize TStack<TList>;
  TSEScopeStack = specialize TStack<Integer>;
  TIntegerList = specialize TList<Integer>;
  TSEFrame = record
    Code: Integer;
    Stack: PSEValue;
    Binary: Integer;
  end;
  PSEFrame = ^TSEFrame;

  TEvilC = class;
  TSEVM = class
  public
    IsPaused: Boolean;
    IsDone: Boolean;
    IsYielded: Boolean;
    Stack: array of TSEValue;
    Frame: array of TSEFrame;
    CodePtr: Integer;
    StackPtr: PSEValue;
    BinaryPtr: Integer;
    FramePtr: PSEFrame;
    StackSize: Integer;
    FrameSize: Integer;
    Parent: TEvilC;
    Binaries: array of TSEBinary;
    WaitTime: LongWord;

    constructor Create;
    destructor Destroy; override;
    function IsWaited: Boolean;
    procedure Reset;
    procedure Exec;
    procedure BinaryClear;
  end;

  TSECache = record
    Binaries: array of TSEBinary;
    GlobalVarCount: Cardinal;
    LineOfCodeList: TIntegerList;
    FuncScriptList: TSEFuncScriptList;
    FuncImportList: TSEFuncImportList;
  end;
  TSECacheMapAncestor = specialize TDictionary<String, TSECache>;
  TSECacheMap = class(TSECacheMapAncestor)
  public
    procedure ClearSingle(const AName: String);
    procedure Clear; override;
  end;

  TSETokenKind = (
    tkEOF,
    tkDot,
    tkAdd,
    tkSub,
    tkMul,
    tkDiv,
    tkMod,
    tkPow,
    tkOpAssign,
    tkEqual,
    tkNotEqual,
    tkSmaller,
    tkGreater,
    tkSmallerOrEqual,
    tkGreaterOrEqual,
    tkBegin,
    tkEnd,
    tkColon,
    tkBracketOpen,
    tkBracketClose,
    tkNegative,
    tkNumber,
    tkString,
    tkComma,
    tkIf,
    tkSwitch,
    tkCase,
    tkDefault,
    tkIdent,
    tkFunction,
    tkFunctionDecl,
    tkVariable,
    tkConst,
    tkUnknown,
    tkElse,
    tkWhile,
    tkBreak,
    tkContinue,
    tkYield,
    tkSquareBracketOpen,
    tkSquareBracketClose,
    tkAnd,
    tkOr,
    tkXor,
    tkNot,
    tkFor,
    tkIn,
    tkTo,
    tkDownto,
    tkReturn,
    tkAtom,
    tkImport,
    tkDo
  );
TSETokenKinds = set of TSETokenKind;

const TokenNames: array[TSETokenKind] of String = (
  'EOF', '.', '+', '-', '*', 'div', 'mod', '^', 'operator assign', '=', '!=', '<',
  '>', '<=', '>=', '{', '}', ':', '(', ')', 'neg', 'number', 'string',
  ',', 'if', 'switch', 'case', 'default', 'identity', 'function', 'fn', 'variable', 'const',
  'unknown', 'else', 'while', 'break', 'continue', 'yield',
  '[', ']', 'and', 'or', 'xor', 'not', 'for', 'in', 'to', 'downto', 'return',
  'atom', 'import', 'do'
);

type
  TSEIdentKind = (
    ikVariable,
    ikFunc
  );

  TSEIdent = record
    Kind: TSEIdentKind;
    Addr: Integer;
    IsUsed: Boolean;
    Local: Integer;
    Ln: Integer;
    Col: Integer;
    Name: String;
  end;
  PSEIdent = ^TSEIdent;

  TSEIdentListAncestor = specialize TList<TSEIdent>;
  TSEIdentList = class(TSEIdentListAncestor)
  public
    function Ptr(const P: Integer): PSEIdent;
  end;

  TSEToken = record
    Kind: TSETokenKind;
    BelongedFileName,
    Value: String;
    Ln, Col: Integer;
  end;
  PSEToken = ^TSEToken;
  TSETokenList = specialize TList<TSEToken>;

  TEvilC = class
  private
    FSource: String;
    FInternalIdentCount: QWord;
    procedure SetSource(V: String);
    function InternalIdent: String;
  public
    ErrorLn, ErrorCol: Integer;
    VM: TSEVM;
    IncludePathList,
    IncludeList: TStrings;
    TokenList: TSETokenList;
    OpcodeInfoList: TSEOpcodeInfoList;
    LocalVarCountList: TIntegerList;
    GlobalVarCount: Integer;
    VarList: TSEIdentList;
    FuncNativeList: TSEFuncNativeList;
    FuncScriptList: TSEFuncScriptList;
    FuncImportList: TSEFuncImportList;
    ConstMap: TSEConstMap;
    ScopeStack: TSEScopeStack;
    ScopeFunc: TSEScopeStack;
    LineOfCodeList: TIntegerList;
    IsLex,
    IsParsed: Boolean;
    IsDone: Boolean;
    FuncTraversal: Integer;
    CurrentFileList: TStrings;
    Binary: TSEBinary; // Current working binary
    constructor Create;
    destructor Destroy; override;
    procedure AddDefaultConsts;
    function IsWaited: Boolean;
    function GetIsPaused: Boolean;
    procedure SetIsPaused(V: Boolean);
    function IsYielded: Boolean;
    procedure Lex(const IsIncluded: Boolean = False);
    procedure Parse;
    procedure Reset;
    function Exec: TSEValue;
    procedure RegisterFunc(const Name: String; const Func: TSEFunc; const ArgCount: Integer);
    function RegisterScriptFunc(const Name: String; const ArgCount: Integer): PSEFuncScriptInfo;
    procedure RegisterImportFunc(const Name, ActualName, LibName: String; const Args: TSEAtomKindArray; const Return: TSEAtomKind);
    function Backup: TSECache;
    procedure Restore(const Cache: TSECache);

    property IsPaused: Boolean read GetIsPaused write SetIsPaused;
    property Source: String read FSource write SetSource;
  end;

function SEValueToText(const Value: TSEValue; const IsRoot: Boolean = True): String;
function SESize(constref Value: TSEValue): Cardinal; inline;
function SEMapGet(constref V: TSEValue; constref I: Integer): TSEValue; inline; overload;
function SEMapGet(constref V: TSEValue; constref S: String): TSEValue; inline; overload;
function SEMapGet(constref V, I: TSEValue): TSEValue; inline; overload;
procedure SEMapSet(constref V: TSEValue; constref I: Integer; const A: TSEValue); inline; overload;
procedure SEMapSet(constref V: TSEValue; constref S: String; const A: TSEValue); inline; overload;
procedure SEMapSet(constref V, I: TSEValue; const A: TSEValue); inline; overload;
function SEMapIsValidArray(constref V: TSEValue): Boolean; inline;

operator := (V: TSENumber) R: TSEValue;
operator := (V: String) R: TSEValue;
operator := (V: Boolean) R: TSEValue;
operator := (V: TSEValueArray) R: TSEValue;
operator := (V: Pointer) R: TSEValue;
operator := (V: TSEValue) R: Integer;
{$ifdef CPU64}
operator := (V: TSEValue) R: Int64;
{$endif}
operator := (V: TSEValue) R: Boolean;
operator := (V: TSEValue) R: TSENumber;
operator := (V: TSEValue) R: String;
operator := (V: TSEValue) R: TSEValueArray;
operator := (V: TSEValue) R: Pointer;
operator + (V1: TSEValue; V2: TSENumber) R: TSEValue;
operator + (V1: TSEValue; V2: String) R: TSEValue;
operator + (V1: TSEValue; V2: Pointer) R: TSEValue;
operator - (V1: TSEValue; V2: TSENumber) R: TSEValue;
operator - (V1: TSEValue; V2: Pointer) R: TSEValue;
operator * (V1: TSEValue; V2: TSENumber) R: TSEValue;
operator / (V1: TSEValue; V2: TSENumber) R: TSEValue;
operator + (V1, V2: TSEValue) R: TSEValue;
operator - (V1, V2: TSEValue) R: TSEValue;
operator - (V: TSEValue) R: TSEValue;
operator * (V1, V2: TSEValue) R: TSEValue;
operator / (V1, V2: TSEValue) R: TSEValue;
operator < (V1: TSEValue; V2: TSENumber) R: Boolean;
operator > (V1: TSEValue; V2: TSENumber) R: Boolean;
operator <= (V1: TSEValue; V2: TSENumber) R: Boolean;
operator >= (V1: TSEValue; V2: TSENumber) R: Boolean;
operator = (V1: TSEValue; V2: TSENumber) R: Boolean;
operator <> (V1: TSEValue; V2: String) R: Boolean;
operator < (V1, V2: TSEValue) R: Boolean;
operator > (V1, V2: TSEValue) R: Boolean;
operator <= (V1, V2: TSEValue) R: Boolean;
operator >= (V1, V2: TSEValue) R: Boolean;
operator = (V1, V2: TSEValue) R: Boolean;
operator <> (V1, V2: TSEValue) R: Boolean;

var
  ScriptVarMap: TSEVarMap;
  GC: TSEGarbageCollector;
  ScriptCacheMap: TSECacheMap;
  SENull: TSEValue;
  JumpTable: array[TSEOpcode] of Pointer;

implementation

uses
  Math, Strings;

type
  TBuiltInFunction = class
    class function SEBufferCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferLength(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferGetU8(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferGetU16(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferGetU32(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferGetU64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferGetI8(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferGetI16(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferGetI32(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferGetI64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferGetF64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferSetU8(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferSetU16(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferSetU32(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferSetU64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferSetI8(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferSetI16(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferSetI32(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferSetI64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferSetF64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringToBuffer(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEBufferToString(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEWBufferToString(const VM: TSEVM; const Args: array of TSEValue): TSEValue;

    class function SETypeOf(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEWrite(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEWriteln(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SERandom(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SERnd(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SERound(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEFloor(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SECeil(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SESet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEString(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SENumber(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEWait(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SELength(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEMapCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEMapDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEMapKeysGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEArrayResize(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SELerp(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SESLerp(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SESign(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SESin(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SECos(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SETan(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SECot(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SERange(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEMin(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEMax(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEPow(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringEmpty(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringGrep(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringSplit(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringFind(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringInsert(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringConcat(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringReplace(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringFormat(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringUpperCase(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringLowerCase(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringFindRegex(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringTrim(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringTrimLeft(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringTrimRight(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEEaseInQuad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEEaseOutQuad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEEaseInOutQuad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEEaseInCubic(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEEaseOutCubic(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEEaseInOutCubic(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEGetTickCount(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEDTNow(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEDTSetDate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEDTSetTime(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEDTDayAdd(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEDTMonthAdd(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEDTYearAdd(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEDTGetYear(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEDTGetMonth(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEDTGetDay(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEDTGetHour(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEDTGetMinute(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEGCObjectCount(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEGCUsed(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEGCCollect(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
  end;

  TDynlibMap = specialize TDictionary<String, TLibHandle>;

var
  DynlibMap: TDynlibMap;
  VMList: TSEVMList;

function PointStrToFloat(S: String): Double; inline;
var
  fS: TFormatSettings;
begin
  FS := FormatSettings;
  fS.DecimalSeparator := '.';
  Result := StrToFloat(S, FS);
end;

function PointFloatToStr(X: Double): String; inline;
var
  FS: TFormatSettings;
begin
  FS := FormatSettings;
  FS.DecimalSeparator := '.';
  Result := FloatToStr(X, FS);
end;

function GetOS: String; inline;
begin
  {$if defined(WINDOWS)}
  Result := 'windows';
  {$elseif defined(LINUX)}
  Result := 'linux';
  {$elseif defined(DARWIN)}
  Result := 'darwin';
  {$else}
  Result := 'unknown';
  {$endif}
end;

function StringIndexOf(S, P: String): Integer; inline;
begin
  {$ifdef SE_STRING_UTF8}
  Result := UTF8Pos(P, S);
  Dec(Result);
  {$else}
  Result := S.IndexOf(P);
  {$endif}
end;

function SEValueToText(const Value: TSEValue; const IsRoot: Boolean = True): String;
var
  Key, S: String;
  IsValidArray: Boolean;
  I: Integer = 0;
begin
  case Value.Kind of
    sevkString:
      begin
        if IsRoot then
          Result := Value.VarString^
        else
          Result := '"' + Value.VarString^ + '"';
      end;
    sevkNumber:
      Result := PointFloatToStr(Value.VarNumber);
    sevkBoolean:
      Result := BoolToStr(Boolean(Round(Value.VarNumber)), 'true', 'false');
    sevkMap:
      begin
        Result := '[';
        IsValidArray := SEMapIsValidArray(Value);
        if IsValidArray then
        begin
          for I := 0 to TSEValueMap(Value.VarMap).List.Count - 1 do
          begin
            if I > 0 then
              Result := Result + ', ';
            Result := Result + SEValueToText(SEMapGet(Value, I), False);
          end;
        end else
        begin
          for Key in TSEValueMap(Value.VarMap).Keys do
          begin
            if I > 0 then
              Result := Result + ', ';
            Result := Result + '"' + Key + '": ' + SEValueToText(SEMapGet(Value, Key), False);
            Inc(I);
          end;
        end;
        Result := Result + ']'
      end;
    sevkFunction:
      begin
        WriteStr(S, Value.VarFuncKind);
        Result := 'fn@' + S + ':' + IntToStr(Value.VarFuncIndx);
      end;
    sevkNull:
      Result := 'null';
    sevkBuffer:
      Result := 'buffer@' + IntToStr(QWord(Value.VarBuffer^.Ptr));
    else
      Result := Value;
  end;
end;

function SESize(constref Value: TSEValue): Cardinal; inline;
begin
  case Value.Kind of
    sevkMap:
      begin
        if SEMapIsValidArray(Value) then
          Result := TSEValueMap(Value.VarMap).List.Count
        else
          Result := TSEValueMap(Value.VarMap).Count;
      end;
    sevkBuffer:
      begin
        Result := Length(Value.VarBuffer^.Base);
      end;
    else
      Result := Value.Size;
  end;
end;

function SEMapGet(constref V: TSEValue; constref I: Integer): TSEValue; inline; overload;
begin
  try
    Result := TSEValueMap(V.VarMap).Get2(IntToStr(I));
  except
    Result := SENull;
  end;
end;

function SEMapGet(constref V: TSEValue; constref S: String): TSEValue; inline; overload;
begin
  try
    Result := TSEValueMap(V.VarMap).Get2(S);
  except
    Result := SENull;
  end;
end;

function SEMapGet(constref V, I: TSEValue): TSEValue; inline; overload;
var
  S: String;
begin
  try
    case I.Kind of
      sevkString:
        S := I.VarString^;
      sevkNumber, sevkBoolean:
        S := IntToStr(Round(I.VarNumber));
      else
        Exit(SENull);
    end;
    Result := TSEValueMap(V.VarMap).Get2(S);
  except
    Result := SENull;
  end;
end;

procedure SEMapSet(constref V: TSEValue; constref I: Integer; const A: TSEValue); inline; overload;
begin
  TSEValueMap(V.VarMap).Set2(IntToStr(I), A);
end;

procedure SEMapSet(constref V: TSEValue; constref S: String; const A: TSEValue); inline; overload;
begin
  TSEValueMap(V.VarMap).Set2(S, A);
end;

procedure SEMapSet(constref V, I: TSEValue; const A: TSEValue); inline; overload;
var
  S: String;
begin
  case I.Kind of
    sevkString:
      S := I.VarString^;
    sevkNumber, sevkBoolean:
      S := IntToStr(Round(I.VarNumber));
    else
      Exit;
  end;
  TSEValueMap(V.VarMap).Set2(S, A);
end;

function SEMapIsValidArray(constref V: TSEValue): Boolean; inline;
begin
  if V.Kind <> sevkMap then
    Exit(False);
  Result := TSEValueMap(V.VarMap).IsValidArray;
end;

function SEClone(constref V: TSEValue): TSEValue;
var
  S, Key: String;
begin
  case V.Kind of
    sevkNumber, sevkBoolean:
      begin
        Result.VarNumber := V.VarNumber;
        Result.Kind := V.Kind;
      end;
    sevkPointer:
      begin
        Result.VarPointer := V.VarPointer;
        Result.Kind := sevkPointer;
      end;
    sevkString:
      begin
        S := V.VarString^;
        GC.AllocString(@Result, S);
      end;
    sevkMap:
      begin
        GC.AllocMap(@Result);
        for Key in TSEValueMap(V.VarMap).Keys do
          SEMapSet(Result, Key, SEMapGet(V, Key))
      end;
  end;
end;

class function TBuiltInFunction.SEBufferCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  GC.AllocBuffer(@Result, Round(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEBufferLength(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := SESize(Args[0]);
end;

class function TBuiltInFunction.SEBufferGetU8(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkNumber;
  Result.VarNumber := Byte((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetU16(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkNumber;
  Result.VarNumber := Word((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetU32(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkNumber;
  Result.VarNumber := LongWord((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetU64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkNumber;
  Result.VarNumber := QWord((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetI8(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkNumber;
  Result.VarNumber := SmallInt((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetI16(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkNumber;
  Result.VarNumber := ShortInt((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetI32(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkNumber;
  Result.VarNumber := LongInt((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetI64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkNumber;
  Result.VarNumber := Int64((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetF64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkNumber;
  Result.VarNumber := TSENumber((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferSetU8(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Byte(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetU16(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Word(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetU32(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  LongWord(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetU64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  QWord(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetI8(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  ShortInt(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetI16(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  P: Pointer;
begin
  P := Pointer(Round(Args[0].VarNumber));
  SmallInt(P^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetI32(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  LongInt(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetI64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Int64(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetF64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  TSENumber(Args[0].VarBuffer^.Ptr^) := Args[1];
end;

class function TBuiltInFunction.SEStringToBuffer(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  GC.AllocBuffer(@Result, 1);
  Result.VarBuffer^.Base := Args[0].VarString^;
  Result.VarBuffer^.Ptr := PChar(Result.VarBuffer^.Base);
end;

class function TBuiltInFunction.SEBufferToString(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: String;
begin
  S := PChar(Args[0].VarBuffer^.Ptr);
  GC.AllocString(@Result, S);
end;

class function TBuiltInFunction.SEWBufferToString(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  WS: WideString;
  S: String;
begin
  WS := PWideChar(Args[0].VarBuffer^.Ptr);
  S := UTF8Encode(WS);
  GC.AllocString(@Result, S);
end;

class function TBuiltInFunction.SETypeOf(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  case Args[0].Kind of
    sevkMap:
      if SEMapIsValidArray(Args[0]) then
        Result := 'array'
      else
        Result := 'map';
    sevkNumber:
      Result := 'number';
    sevkBoolean:
      Result := 'boolean';
    sevkString:
      Result := 'string';
    sevkNull:
      Result := 'null';
    sevkPointer:
      Result := 'pointer';
    sevkBuffer:
      Result := 'buffer';
    sevkFunction:
      Result := 'function';
  end;
end;

class function TBuiltInFunction.SEWrite(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
begin
  for I := 0 to Length(Args) - 1 do
  begin
    Write(SEValueToText(Args[I]));
  end;
end;

class function TBuiltInFunction.SEWriteln(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
begin
  TBuiltInFunction.SEWrite(VM, Args);
  Writeln;
end;

class function TBuiltInFunction.SERandom(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(Random(Round(Args[0].VarNumber)));
end;

class function TBuiltInFunction.SERnd(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(Random);
end;

class function TBuiltInFunction.SERound(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(Round(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEFloor(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(Floor(Args[0].VarNumber));
end;

class function TBuiltInFunction.SECeil(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(Ceil(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if ScriptVarMap.ContainsKey(Args[0].VarString^) then
    Exit(ScriptVarMap[Args[0]])
  else
    Exit(SENull);
end;

class function TBuiltInFunction.SESet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  ScriptVarMap.AddOrSetValue(Args[0].VarString^, Args[1]);
end;

class function TBuiltInFunction.SEString(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(SEValueToText(Args[0]));
end;

class function TBuiltInFunction.SENumber(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(PointStrToFloat(Args[0]));
end;

class function TBuiltInFunction.SEWait(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  VM.WaitTime := GetTickCount64 + Round(Args[0].VarNumber * 1000);
end;

class function TBuiltInFunction.SELength(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  case Args[0].Kind of
    sevkString:
      {$ifdef SE_STRING_UTF8}
      Exit(UTF8Length(String(Args[0].VarString^)));
      {$else}
      Exit(Length(String(Args[0].VarString^)));
      {$endif}
    sevkMap:
      begin
        Exit(SESize(Args[0]));
      end;
    else
      Exit(0);
  end;
end;

class function TBuiltInFunction.SEMapCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  I: Integer = 0;
begin
  GC.AllocMap(@Result);
  while I < Length(Args) - 1 do
  begin
    SEMapSet(Result, Args[I].VarString^, Args[I + 1]);
    Inc(I, 2);
  end;
end;

class function TBuiltInFunction.SEMapDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Key: String;
  I, J: Integer;
begin
  GC.AllocMap(@Result);
  if SEMapIsValidArray(Args[0]) then
  begin
    J := 0;
    for I := 0 to TSEValueMap(Args[0].VarMap).List.Count - 1 do
    begin
      if I <> Args[1] then
      begin
        SEMapSet(Result, J, SEMapGet(Args[0], I));
        Inc(J);
      end;
    end;
  end else
  begin
    for Key in TSEValueMap(Args[0].VarMap).Keys do
    begin
      if Key <> Args[1] then
      begin
        SEMapSet(Result, Key, SEMapGet(Args[0], Key));
      end;
    end;
  end;
end;

class function TBuiltInFunction.SEMapKeysGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Key: String;
  I: Integer = 0;
begin
  GC.AllocMap(@Result);
  if not SEMapIsValidArray(Args[0]) then
  begin
    for Key in TSEValueMap(Args[0].VarMap).Keys do
    begin
      SEMapSet(Result, I, Key);
      Inc(I);
    end;
  end else
  begin
    for I := 0 to TSEValueMap(Args[0].VarMap).List.Count - 1 do
    begin
      SEMapSet(Result, I, I);
    end;
  end;
end;

class function TBuiltInFunction.SEArrayResize(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if SEMapIsValidArray(Args[0]) then
  begin
    TSEValueMap(Args[0].VarMap).List.Count := Args[1];
  end;
  Result := Args[0];
end;

class function TBuiltInFunction.SELerp(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  A, B, T: TSENumber;
begin
  A := Args[0];
  B := Args[1];
  T := Args[2];
  Exit(A + (B - A) * T);
end;

class function TBuiltInFunction.SESLerp(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  A, B, T, T2: TSENumber;
begin
  A := Args[0];
  B := Args[1];
  T := Args[2];
  T2 := (1 - Cos(T * PI)) * 0.5;
  Exit(A * (1 - T2) + B * T2);
end;

class function TBuiltInFunction.SESign(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(Sign(Args[0].VarNumber));
end;

class function TBuiltInFunction.SERange(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
  function EpsilonRound(V: TSENumber): TSENumber;
  begin
    if Abs(Frac(V)) < 1E-12 then
      Result := Round(V)
    else
      Result := V;
  end;

var
  V: TSENumber;
  I: Integer = 0;
begin
  GC.AllocMap(@Result);
  V := Args[0];
  if Length(Args) = 3 then
    TSEValueMap(Result.VarMap).List.Capacity := Round(Args[1].VarNumber * (1 / Args[2].VarNumber)) // Set capacity beforehand
  else
    TSEValueMap(Result.VarMap).List.Capacity := Round(Args[1].VarNumber); // Set capacity beforehand
  while EpsilonRound(V) <= Args[1].VarNumber do
  begin
    SEMapSet(Result, I, V);
    if Length(Args) = 3 then
      V := V + Args[2].VarNumber
    else
      V := V + 1;
    Inc(I);
  end;
end;

class function TBuiltInFunction.SEMin(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
begin
  for I := 0 to Length(Args) - 2 do
    if Args[I] < Args[I + 1] then
      Result := Args[I]
    else
      Result := Args[I + 1];
end;

class function TBuiltInFunction.SEMax(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
begin
  for I := 0 to Length(Args) - 2 do
    if Args[I] > Args[I + 1] then
      Result := Args[I]
    else
      Result := Args[I + 1];
end;

class function TBuiltInFunction.SEPow(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(Power(Args[0].VarNumber, Args[1].VarNumber));
end;

class function TBuiltInFunction.SEStringEmpty(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if Args[0].Kind = sevkString then
    Args[0].VarString^ := '';
end;

class function TBuiltInFunction.SEStringGrep(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
  A: TStringDynArray;
  V: String;
begin
  Result := '';
  A := SplitString(Args[0], #10);
  for V in A do
    for I := 0 to SESize(Args[1]) - 1 do
      if StringIndexOf(V, SEMapGet(Args[1], I).VarString^) >= 0 then
      begin
        if Result = '' then
          Result := V
        else
          Result := Result + #10 + V;
      end;
end;

class function TBuiltInFunction.SEStringSplit(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  D: TStringDynArray;
  I: Integer;
begin
  D := SplitString(Args[0], Args[1]);
  GC.AllocMap(@Result);
  for I := 0 to Length(D) - 1 do
    SEMapSet(Result, I, D[I]);
end;

class function TBuiltInFunction.SEStringFind(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := StringIndexOf(Args[0].VarString^, Args[1]);
end;

class function TBuiltInFunction.SEStringDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  {$ifdef SE_STRING_UTF8}
  UTF8Delete(Args[0].VarString^, Round(Args[1].VarNumber + 1), Round(Args[2].VarNumber));
  {$else}
  Delete(Args[0].VarString^, Round(Args[1].VarNumber + 1), Round(Args[2].VarNumber));
  {$endif}
  Result := Args[0].VarString^;
end;

class function TBuiltInFunction.SEStringConcat(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Args[0];
  // Since we mess with GC, manually update mem used
  GC.AllocatedMem := GC.AllocatedMem - ByteLength(Args[1].VarString^);
  Result.VarString^ := Args[1].VarString^ + Args[2].VarString^;
  GC.AllocatedMem := GC.AllocatedMem + ByteLength(Args[1].VarString^);
end;

class function TBuiltInFunction.SEStringInsert(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  {$ifdef SE_STRING_UTF8}
  UTF8Insert(Args[1].VarString^, Args[0].VarString^, Round(Args[2].VarNumber + 1));
  {$else}
  Insert(Args[1].VarString^, Args[0].VarString^, Round(Args[2].VarNumber + 1));
  {$endif}
  Result := Args[0].VarString^;
end;

class function TBuiltInFunction.SEStringReplace(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: String;
begin
  S := StringReplace(Args[0], Args[1], Args[2], [rfReplaceAll]);
  Result := S;
end;

class function TBuiltInFunction.SEStringFormat(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
  S, V: String;
  Value: TSEValue;
begin
  S := Args[0].VarString^;
  for I := 0 to SESize(Args[1]) - 1 do
  begin
    V := '';
    Value := SEMapGet(Args[1], I);
    V := SEValueToText(Value);
    S := StringReplace(S, '{' + IntToStr(I) + '}', V, [rfReplaceAll]);
  end;
  Result := S;
end;

class function TBuiltInFunction.SEStringUpperCase(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: String;
begin
  Result := '';
  case Args[0].Kind of
    sevkString: Result := UpperCase(Args[0].VarString^);
    sevkBoolean,
    sevkNumber: Result := UpperCase(Char(Round(Args[0].VarNumber)));
  end;
end;

class function TBuiltInFunction.SEStringLowerCase(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: String;
begin
  Result := '';
  case Args[0].Kind of
    sevkString: Result := LowerCase(Args[0].VarString^);
    sevkBoolean,
    sevkNumber: Result := LowerCase(Char(Round(Args[0].VarNumber)));
  end;
end;

class function TBuiltInFunction.SEStringFindRegex(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  R: TRegExpr;
  I: Integer;
  C: Integer = 0;
  V: TSEValue;
begin
  GC.AllocMap(@Result);
  R := TRegExpr.Create(Args[1].VarString^);
  if R.Exec(Args[0].VarString^) then
  repeat
    for I := 1 to R.SubExprMatchCount do
    begin
      GC.AllocMap(@V);
      SEMapSet(V, 0, R.Match[I]);
      SEMapSet(V, 1, R.MatchPos[I] - 1);
      SEMapSet(Result, C, V);
      Inc(C);
    end;
  until not R.ExecNext;
end;

class function TBuiltInFunction.SEStringTrim(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Trim(Args[0]);
end;

class function TBuiltInFunction.SEStringTrimLeft(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := TrimLeft(Args[0]);
end;

class function TBuiltInFunction.SEStringTrimRight(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := TrimRight(Args[0]);
end;

class function TBuiltInFunction.SESin(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(Sin(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SECos(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(Cos(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SETan(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(Tan(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SECot(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(Cot(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SEEaseInQuad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: TSENumber;
begin
  S := Args[0];
  Exit(S * S);
end;

class function TBuiltInFunction.SEEaseOutQuad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: TSENumber;
begin
  S := Args[0];
  Exit(S * (2 - S));
end;

class function TBuiltInFunction.SEEaseInOutQuad(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: TSENumber;
begin
  S := Args[0];
  if S < 0.5 then
    Exit(2 * S * S);
  Exit(-1 + (4 - 2 * S) * S);
end;

class function TBuiltInFunction.SEEaseInCubic(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: TSENumber;
begin
  S := Args[0];
  Exit(S * S * S);
end;

class function TBuiltInFunction.SEEaseOutCubic(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: TSENumber;
begin
  S := Args[0];
  S := S - 1;
  Exit(S * S * S + 1);
end;

class function TBuiltInFunction.SEEaseInOutCubic(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: TSENumber;
begin
  S := Args[0];
  if S < 0.5 then
    Exit(4 * S * S * S);
  Exit((S - 1) * (2 * S - 2) * (2 * S - 2) + 1);
end;

class function TBuiltInFunction.SEGetTickCount(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: TSENumber;
begin
  Exit(GetTickCount64);
end;

class function TBuiltInFunction.SEDTNow(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Now;
end;

class function TBuiltInFunction.SEDTSetDate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := EncodeDate(Round(Args[0].VarNumber), Round(Args[1].VarNumber), Round(Args[2].VarNumber));
end;

class function TBuiltInFunction.SEDTSetTime(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := EncodeTime(Round(Args[0].VarNumber), Round(Args[1].VarNumber), Round(Args[2].VarNumber), Round(Args[3].VarNumber));
end;

class function TBuiltInFunction.SEDTDayAdd(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := IncDay(Args[0].VarNumber, Round(Args[1].VarNumber));
end;

class function TBuiltInFunction.SEDTMonthAdd(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := IncMonth(Args[0].VarNumber, Round(Args[1].VarNumber));
end;

class function TBuiltInFunction.SEDTYearAdd(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := IncYear(Args[0].VarNumber, Round(Args[1].VarNumber));
end;

class function TBuiltInFunction.SEDTGetYear(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Y, M, D: Word;
begin
  DecodeDate(Args[0].VarNumber, Y, M, D);
  Result := Y;
end;

class function TBuiltInFunction.SEDTGetMonth(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Y, M, D: Word;
begin
  DecodeDate(Args[0].VarNumber, Y, M, D);
  Result := M;
end;

class function TBuiltInFunction.SEDTGetDay(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  Y, M, D: Word;
begin
  DecodeDate(Args[0].VarNumber, Y, M, D);
  Result := D;
end;

class function TBuiltInFunction.SEDTGetHour(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  H, M ,S, MS: Word;
begin
  DecodeTime(Args[0].VarNumber, H, M, S, MS);
  Result := H;
end;

class function TBuiltInFunction.SEDTGetMinute(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  H, M ,S, MS: Word;
begin
  DecodeTime(Args[0].VarNumber, H, M, S, MS);
  Result := M;
end;

class function TBuiltInFunction.SEGCObjectCount(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := GC.ValueList.Count - 1;
end;

class function TBuiltInFunction.SEGCUsed(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := GC.AllocatedMem;
end;

class function TBuiltInFunction.SEGCCollect(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  GC.GC;
end;

function TSEOpcodeInfoList.Ptr(const P: Integer): PSEOpcodeInfo; inline;
begin
  Result := @FItems[P];
end;

function TSEFuncNativeList.Ptr(const P: Integer): PSEFuncNativeInfo; inline;
begin
  Result := @FItems[P];
end;

function TSEFuncScriptList.Ptr(const P: Integer): PSEFuncScriptInfo; inline;
begin
  Result := @FItems[P];
end;

function TSEFuncImportList.Ptr(const P: Integer): PSEFuncImportInfo; inline;
begin
  Result := @FItems[P];
end;

function TSEIdentList.Ptr(const P: Integer): PSEIdent; inline;
begin
  Result := @FItems[P];
end;

function TSEBinary.Ptr(const P: Integer): PSEValue; inline;
begin
  Result := @FItems[P];
end;

// ----- Fast inline TSEValue operations -----

procedure SEValueAdd(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
var
  I, Len: Integer;
  Temp: TSEValue;
  Key, S: String;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber, sevkBoolean:
      begin
        R.Kind := sevkNumber;
        R.VarNumber := V1.VarNumber + V2.VarNumber;
      end;
    sevkString:
      begin
        GC.AllocString(@R, V1.VarString^ + V2.VarString^);
      end;
    sevkMap:
      begin
        GC.AllocMap(@Temp);
        if (not SEMapIsValidArray(V1)) and (not SEMapIsValidArray(V2)) then
        begin
          for S in TSEValueMap(V1.VarMap).Keys do
            SEMapSet(Temp, S, SEMapGet(V1, S));
          for S in TSEValueMap(V2.VarMap).Keys do
            SEMapSet(Temp, S, SEMapGet(V2, S));
        end else
        begin
          Len := SESize(V1);
          TSEValueMap(Temp.VarMap).List.Count := Len + SESize(V2);
          for I := 0 to Len - 1 do
            SEMapSet(Temp, I, SEMapGet(V1, I));
          for I := Len to Len + SESize(V2) - 1 do
            SEMapSet(Temp, I, SEMapGet(V2, I - Len));
        end;
        R := Temp;
      end;
    sevkPointer:
      begin
        R.Kind := sevkPointer;
        R.VarPointer := V1.VarPointer + V2.VarPointer;
      end;
  end
  else
    if (V1.Kind = sevkBuffer) and (V2.Kind in [sevkNumber, sevkBoolean]) then
    begin
      GC.AllocBuffer(@Temp, 1);
      Temp.VarBuffer^.Ptr := Pointer(QWord(V1.VarBuffer^.Ptr) + Round(V2.VarNumber));
      R := Temp;
    end;
end;

procedure SEValueSub(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
var
  Temp: TSEValue;
begin
  case V1.Kind of
    sevkNumber, sevkBoolean:
      begin
        R.Kind := sevkNumber;
        R.VarNumber := V1.VarNumber - V2.VarNumber;
      end;
    sevkPointer:
      begin
        R.Kind := sevkPointer;
        R.VarPointer := Pointer(V1.VarPointer - V2.VarPointer);
      end;
    sevkBuffer:
      begin
        GC.AllocBuffer(@Temp, 1);
        Temp.VarBuffer^.Ptr := Pointer(QWord(V1.VarBuffer^.Ptr) - Round(V2.VarNumber));
        R := Temp;
      end;
  end;
end;

procedure SEValueNot(out R: TSEValue; constref V: TSEValue); inline;
begin
  case V.Kind of
    sevkNumber, sevkBoolean:
      begin
        R := not (V.VarNumber <> 0);
      end;
    sevkNull:
      begin
        R := True;
      end;
    sevkString:
      begin
        R := False;
      end;
  end;
end;

procedure SEValueNeg(out R: TSEValue; constref V: TSEValue); inline;
begin
  R.VarNumber := -V.VarNumber;
end;

procedure SEValueMul(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber * V2.VarNumber;
end;

procedure SEValueDiv(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber / V2.VarNumber;
end;

procedure SEValueLesser(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R := V1.VarNumber < V2.VarNumber;
end;

procedure SEValueGreater(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R := V1.VarNumber > V2.VarNumber;
end;

procedure SEValueLesserOrEqual(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R := V1.VarNumber <= V2.VarNumber;
end;

procedure SEValueGreaterOrEqual(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R := V1.VarNumber >= V2.VarNumber;
end;

procedure SEValueEqual(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber, sevkBoolean:
      R := V1.VarNumber = V2.VarNumber;
    sevkString:
      R := V1.VarString^ = V2.VarString^;
    sevkFunction:
      R := (V1.VarFuncKind = V2.VarFuncKind) and (V1.VarFuncIndx = V2.VarFuncIndx);
    sevkNull:
      R := True;
  end else
  if (V1.Kind = sevkNumber) and (V2.Kind = sevkBoolean) then
    R := (V1.VarNumber <> 0) = V2
  else
    R := False;
end;

procedure SEValueNotEqual(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber, sevkBoolean:
      R := V1.VarNumber <> V2.VarNumber;
    sevkString:
      R := V1.VarString^ <> V2.VarString^;
    sevkFunction:
      R := (V1.VarFuncKind <> V2.VarFuncKind) or (V1.VarFuncIndx <> V2.VarFuncIndx);
    sevkNull:
      R := False;
  end else
  if (V1.Kind = sevkNumber) and (V2.Kind = sevkBoolean) then
    R := (V1.VarNumber <> 0) <> V2
  else
    R := True;
end;

function SEValueLesser(constref V1, V2: TSEValue): Boolean; inline; overload;
begin
  Result := V1.VarNumber < V2.VarNumber;
end;

function SEValueGreater(constref V1, V2: TSEValue): Boolean; inline; overload;
begin
  Result := V1.VarNumber > V2.VarNumber;
end;

function SEValueLesserOrEqual(constref V1, V2: TSEValue): Boolean; inline; overload;
begin
  Result := V1.VarNumber <= V2.VarNumber;
end;

function SEValueGreaterOrEqual(constref V1, V2: TSEValue): Boolean; inline; overload;
begin
  Result := V1.VarNumber >= V2.VarNumber;
end;

function SEValueEqual(constref V1, V2: TSEValue): Boolean; inline; overload;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber, sevkBoolean:
      Result := V1.VarNumber = V2.VarNumber;
    sevkString:
      Result := V1.VarString^ = V2.VarString^;
    sevkFunction:
      Result := (V1.VarFuncKind = V2.VarFuncKind) and (V1.VarFuncIndx = V2.VarFuncIndx);
    sevkNull:
      Result := True;
  end else
  if (V1.Kind = sevkNumber) and (V2.Kind = sevkBoolean) then
    Result := (V1.VarNumber <> 0) = V2
  else
    Result := False;
end;

function SEValueNotEqual(constref V1, V2: TSEValue): Boolean; inline; overload;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber, sevkBoolean:
      Result := V1.VarNumber <> V2.VarNumber;
    sevkString:
      Result := V1.VarString^ <> V2.VarString^;
    sevkFunction:
      Result := (V1.VarFuncKind <> V2.VarFuncKind) or (V1.VarFuncIndx <> V2.VarFuncIndx);
    sevkNull:
      Result := False;
  end else
  if (V1.Kind = sevkNumber) and (V2.Kind = sevkBoolean) then
    Result := (V1.VarNumber <> 0) <> V2
  else
    Result := True;
end;

// ----- TSEValue operator overloading

operator := (V: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V;
end;

operator := (V: String) R: TSEValue; inline;
begin
  FillChar(R, SizeOf(TSEValue), 0);
  GC.AllocString(@R, V);
end;

operator := (V: Boolean) R: TSEValue; inline;
begin
  R.Kind := sevkBoolean;
  R.VarNumber := Integer(V);
end;
operator := (V: TSEValueArray) R: TSEValue; inline;
var
  I: Integer;
begin
  GC.AllocMap(@R);
  for I := 0 to Length(V) - 1 do
    SEMapSet(R, I, V[I]);
end;
operator := (V: Pointer) R: TSEValue; inline;
begin
  R.Kind := sevkPointer;
  R.VarPointer := V;
end;

operator := (V: TSEValue) R: Integer; inline;
begin
  R := Round(V.VarNumber);
end;
{$ifdef CPU64}
operator := (V: TSEValue) R: Int64; inline;
begin
  R := Round(V.VarNumber);
end;
{$endif}
operator := (V: TSEValue) R: Boolean; inline;
begin
  R := Round(V.VarNumber) <> 0;
end;
operator := (V: TSEValue) R: TSENumber; inline;
begin
  R := V.VarNumber;
end;
operator := (V: TSEValue) R: String; inline;
begin
  if V.Kind = sevkString then
    R := V.VarString^
  else
    R := '';
end;
operator := (V: TSEValue) R: TSEValueArray; inline;
var
  Len, I: Integer;
begin
  if V.Kind <> sevkMap then
    Exit;
  Len := SESize(V.VarMap);
  SetLength(R, Len);
  for I := 0 to Len - 1 do
    R[I] := SEMapGet(V, I);
end;
operator := (V: TSEValue) R: Pointer; inline;
begin
  R := V.VarPointer;
end;
var
  I, Len: Integer;

operator + (V1: TSEValue; V2: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber + V2;
end;

operator + (V1: TSEValue; V2: String) R: TSEValue; inline;
var
  S: String;
begin
  S := V1.VarString^;
  R := S + V2;
end;

operator + (V1: TSEValue; V2: Pointer) R: TSEValue; inline;
begin
  R.Kind := sevkPointer;
  R.VarPointer := V1.VarPointer + V2;
end;

operator - (V1: TSEValue; V2: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber - V2;
end;
operator - (V1: TSEValue; V2: Pointer) R: TSEValue; inline;
begin
  R.Kind := sevkString;
  R.VarPointer := V1.VarPointer + V2;
end;

operator * (V1: TSEValue; V2: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber * V2;
end;

operator / (V1: TSEValue; V2: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber / V2;
end;

operator + (V1, V2: TSEValue) R: TSEValue; inline;
var
  I, Len: Integer;
  S: String;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber, sevkBoolean:
      begin
        R.Kind := sevkNumber;
        R.VarNumber := V1.VarNumber + V2.VarNumber;
      end;
    sevkString:
      begin
        GC.AllocString(@R, V1.VarString^ + V2.VarString^);
      end;
    sevkMap:
      begin
        GC.AllocMap(@R);
        if (not SEMapIsValidArray(V1)) and (not SEMapIsValidArray(V2)) then
        begin
          for S in TSEValueMap(V1.VarMap).Keys do
            SEMapSet(R, S, SEMapGet(V1, S));
          for S in TSEValueMap(V2.VarMap).Keys do
            SEMapSet(R, S, SEMapGet(V2, S));
        end else
        begin
          Len := SESize(V1);
          TSEValueMap(R.VarMap).List.Count := Len + SESize(V2);
          for I := 0 to Len - 1 do
            SEMapSet(R, I, SEMapGet(V1, I));
          for I := Len to Len + SESize(V2) - 1 do
            SEMapSet(R, I, SEMapGet(V2, I - Len));
        end;
      end;
    sevkBuffer:
      begin
        GC.AllocBuffer(@R, 1);
        R.VarBuffer^.Ptr := Pointer(QWord(V1.VarBuffer^.Ptr) + Round(V2.VarNumber));
      end;
    sevkPointer:
      begin
        R.Kind := sevkPointer;
        R.VarPointer := V1.VarPointer + V2.VarPointer;
      end;
  end else
    if (V1.Kind = sevkBuffer) and (V2.Kind in [sevkNumber, sevkBoolean]) then
    begin
      GC.AllocBuffer(@R, 1);
      R.VarBuffer^.Ptr := V1.VarBuffer^.Ptr + Pointer(Round(V2.VarNumber));
    end;
end;
operator - (V: TSEValue) R: TSEValue; inline;
begin
  R.VarNumber := -V.VarNumber;
end;
operator - (V1, V2: TSEValue) R: TSEValue; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber, sevkBoolean:
      begin
        R.Kind := sevkNumber;
        R.VarNumber := V1.VarNumber - V2.VarNumber;
      end;
    sevkPointer:
      begin
        R.Kind := sevkPointer;
        R.VarPointer := Pointer(V1.VarPointer - V2.VarPointer);
      end;
    sevkBuffer:
      begin
        GC.AllocBuffer(@R, 1);
        R.VarBuffer^.Ptr := Pointer(QWord(V1.VarBuffer^.Ptr) - Round(V2.VarNumber));
      end;
  end;
end;
operator * (V1, V2: TSEValue) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber * V2.VarNumber;
end;
operator / (V1, V2: TSEValue) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber / V2.VarNumber;
end;

operator < (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  R := V1.VarNumber < V2;
end;
operator > (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  R := V1.VarNumber > V2;
end;
operator <= (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  R := V1.VarNumber <= V2;
end;
operator >= (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  R := V1.VarNumber >= V2;
end;
operator = (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  R := V1.VarNumber = V2;
end;

operator = (V1: TSEValue; V2: String) R: Boolean; inline;
begin
  R := V1.VarString^ = V2;
end;

operator <> (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  R := V1.VarNumber <> V2;
end;

operator <> (V1: TSEValue; V2: String) R: Boolean; inline;
begin
  R := V1.VarString^ <> V2;
end;

operator < (V1, V2: TSEValue) R: Boolean; inline;
begin
  R := V1.VarNumber < V2.VarNumber;
end;
operator > (V1, V2: TSEValue) R: Boolean; inline;
begin
  R := V1.VarNumber > V2.VarNumber;
end;
operator <= (V1, V2: TSEValue) R: Boolean; inline;
begin
  R := V1.VarNumber <= V2.VarNumber;
end;
operator >= (V1, V2: TSEValue) R: Boolean; inline;
begin
  R := V1.VarNumber >= V2.VarNumber;
end;
operator = (V1, V2: TSEValue) R: Boolean; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber:
      R := V1.VarNumber = V2.VarNumber;
    sevkBoolean:
      R := Boolean(Round(V1.VarNumber)) = Boolean(Round(V2.VarNumber));
    sevkString:
      R := V1.VarString^ = V2.VarString^;
    sevkFunction:
      R := (V1.VarFuncKind = V2.VarFuncKind) and (V1.VarFuncIndx = V2.VarFuncIndx);
    sevkNull:
      R := True;
  end else
    R := False;
end;
operator <> (V1, V2: TSEValue) R: Boolean; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber:
      R := V1.VarNumber <> V2.VarNumber;
    sevkBoolean:
      R := Boolean(Round(V1.VarNumber)) <> Boolean(Round(V2.VarNumber));
    sevkString:
      R := V1.VarString^ <> V2.VarString^;
    sevkFunction:
      R := (V1.VarFuncKind <> V2.VarFuncKind) or (V1.VarFuncIndx <> V2.VarFuncIndx);
    sevkNull:
      R := False;
  end else
    R := True;
end;

constructor TSEValueMap.Create;
begin
  inherited;
  Self.FList := TSEValueList.Create;
  Self.FIsValidArray := True;
end;

destructor TSEValueMap.Destroy;
begin
  if Self.FIsValidArray then
    GC.AllocatedMem := GC.AllocatedMem - Self.FList.Count * SizeOf(TSEValue)
  else
    GC.AllocatedMem := GC.AllocatedMem - 1024;
  if Self.FList <> nil then
    Self.FList.Free;
  inherited;
end;

procedure TSEValueMap.Set2(const Key: String; const AValue: TSEValue);
var
  Index, I: Integer;
  IsNumber: Boolean;
begin
  IsNumber := TryStrToInt(Key, Index);
  if IsNumber and Self.FIsValidArray and (Index >= 0) then
  begin
    GC.AllocatedMem := GC.AllocatedMem - Self.FList.Count * SizeOf(TSEValue);
    if Index > Self.FList.Count - 1 then
      Self.FList.Count := Index + 1;
    Self.FList[Index] := AValue;
    GC.AllocatedMem := GC.AllocatedMem + Self.FList.Count * SizeOf(TSEValue);
  end else
  begin
    if Self.FIsValidArray then
    begin
      for I := 0 to Self.FList.Count - 1 do
        Self.AddOrSetValue(IntToStr(I), Self.FList[I]);
      GC.AllocatedMem := GC.AllocatedMem - Self.FList.Count * SizeOf(TSEValue) + 1024;
      FreeAndNil(Self.FList);
      Self.FIsValidArray := False;
    end;
  end;
  if not Self.IsValidArray then
  begin
    Self.AddOrSetValue(Key, AValue);
  end;
end;

procedure TSEValueMap.Del2(const Key: String);
var
  Index: Integer;
  IsNumber: Boolean;
begin
  IsNumber := TryStrToInt(Key, Index);
  if IsNumber and Self.FIsValidArray and (Index >= 0) then
  begin
    if Index <= Self.FList.Count - 1 then
    begin
      Self.FList.Delete(Index);
      GC.AllocatedMem := GC.AllocatedMem - SizeOf(TSEValue);
    end;
  end else
  begin
    Self.Remove(Key);
  end;
end;

function TSEValueMap.Get2(const Key: String): TSEValue;
var
  Index: Integer;
  IsNumber: Boolean;
begin
  IsNumber := TryStrToInt(Key, Index);
  if IsNumber and Self.FIsValidArray and (Index >= 0) then
  begin
    if Index <= Self.FList.Count - 1 then
      Result := Self.FList[Index]
    else
      Result := SENull;
  end else
  begin
    Result := Self[Key];
  end;
end;

constructor TSEGarbageCollector.Create;
var
  Ref0: TSEGCValue;
begin
  inherited;
  Self.FValueList := TSEGCValueList.Create;
  Self.FValueList.Capacity := 65536;
  Self.FValueList.Add(Ref0);
  Self.FValueAvailList := TSEGCValueAvailList.Create;;
  Self.FTicks := GetTickCount64;
  Self.FAllocatedMem := 0;
end;

destructor TSEGarbageCollector.Destroy;
var
  I: Integer;
  Value: TSEGCValue;
begin
  for I := 1 to Self.FValueList.Count - 1 do
  begin
    Value := Self.FValueList[I];
    Value.Garbage := True;
    Self.FValueList[I] := Value;
  end;
  Self.Sweep;
  Self.FValueList.Free;
  Self.FValueAvailList.Free;
  inherited;
end;

procedure TSEGarbageCollector.AddToList(const PValue: PSEValue); inline;
var
  Value: TSEGCValue;
begin
  if Self.FValueAvailList.Count = 0 then
  begin
    PValue^.Ref := Self.FValueList.Count;
    Value.Value := PValue^;
    Value.Lock := False;
    Self.FValueList.Add(Value);
  end else
  begin
    PValue^.Ref := Self.FValueAvailList[Self.FValueAvailList.Count - 1];
    Value.Value := PValue^;
    Value.Lock := False;
    Self.FValueList[PValue^.Ref] := Value;
    Self.FValueAvailList.Delete(Self.FValueAvailList.Count - 1);
  end;
end;

procedure TSEGarbageCollector.CheckForGC; inline;
begin
  if (GetTickCount64 - Self.FTicks > 1000 * 60 * 2) or (Self.FAllocatedMem > 1024 * 1024 * 128) then
  begin
    Self.GC;
    Self.FTicks := GetTickCount64;
  end;
end;

procedure TSEGarbageCollector.Sweep; inline;
var
  Value: TSEGCValue;
  I, J, MS: Integer;
begin
  for I := Self.FValueList.Count - 1 downto 1 do
  begin
    Value := Self.FValueList[I];
    if Value.Garbage then
    begin
      case Value.Value.Kind of
        sevkMap:
          begin
            if Value.Value.VarMap <> nil then
            begin
              Value.Value.VarMap.Free;
            end;
          end;
        sevkString:
          begin
            if Value.Value.VarString <> nil then
            begin
              MS := ByteLength(Value.Value.VarString^);
              Self.FAllocatedMem := Self.FAllocatedMem - MS;
              Value.Value.VarString^ := '';
              Dispose(Value.Value.VarString);
            end;
          end;
        sevkBuffer:
          begin
            if Value.Value.VarBuffer <> nil then
            begin
              MS := ByteLength(Value.Value.VarBuffer^.Base);
              Self.FAllocatedMem := Self.FAllocatedMem - MS;
              Value.Value.VarBuffer^.Base := '';
              Dispose(Value.Value.VarBuffer);
            end;
          end;
      end;
      Value.Value.Kind := sevkNumber;
      Self.FValueList[I] := Value;
      Self.FValueAvailList.Add(I);
    end;
  end;
end;

procedure TSEGarbageCollector.GC;
  procedure Mark(const PValue: PSEValue); inline;
  var
    Value: TSEGCValue;
    RValue: TSEValue;
    Key: String;
    I: Integer;
  begin
    if (PValue^.Kind <> sevkMap) and (PValue^.Kind <> sevkString) then
      Exit;
    Value := Self.FValueList[PValue^.Ref];
    if not Value.Garbage then
      Exit;
    case Value.Value.Kind of
      sevkMap:
        begin
          if SEMapIsValidArray(PValue^) then
          begin
            for I := 0 to TSEValueMap(PValue^.VarMap).List.Count - 1 do
            begin
              RValue := SEMapGet(PValue^, I);
              Mark(@RValue);
            end;
          end else
          begin
            for Key in TSEValueMap(PValue^.VarMap).Keys do
            begin
              RValue := SEMapGet(PValue^, Key);
              Mark(@RValue);
            end;
          end;
        end;
    end;
    Value.Garbage := False;
    Self.FValueList[PValue^.Ref] := Value;
  end;

var
  Value: TSEGCValue;
  P: PSEValue;
  V: TSEValue;
  VM: TSEVM;
  I, J, K: Integer;
  Key: String;
  Cache: TSECache;
  Binary: TSEBinary;
begin
  for I := 1 to Self.FValueList.Count - 1 do
  begin
    Value := Self.FValueList[I];
    Value.Garbage := not Value.Lock;
    Self.FValueList[I] := Value;
  end;
  for I := 0 to VMList.Count - 1 do
  begin
    VM := VMList[I];
    P := @VM.Stack[0];
    while QWord(P) <= QWord(VM.StackPtr)  do
    begin
      Mark(P);
      Inc(P);
    end;
    for J := 0 to High(VM.Binaries) do
    begin
      Binary := VM.Binaries[J];
      for K := 0 to Binary.Count - 1 do
      begin
        P := Binary.Ptr(K);
        Mark(P);
      end;
    end;
    for Key in VM.Parent.ConstMap.Keys do
    begin
      V := VM.Parent.ConstMap[Key];
      Mark(@V);
    end;
  end;
  for Cache in ScriptCacheMap.Values do
  begin;
    for J := 0 to High(Cache.Binaries) do
    begin
      Binary := Cache.Binaries[J];
      for K := 0 to Binary.Count - 1 do
      begin
        P := Binary.Ptr(K);
        Mark(P);
      end;
    end;
  end;
  for V in ScriptVarMap.Values do
  begin;
    Mark(@V);
  end;
  Sweep;
end;

procedure TSEGarbageCollector.AllocBuffer(const PValue: PSEValue; const Size: Integer);
begin
  PValue^.Kind := sevkBuffer;
  New(PValue^.VarBuffer);
  SetLength(PValue^.VarBuffer^.Base, Size);
  PValue^.VarBuffer^.Ptr := PChar(PValue^.VarBuffer^.Base);
  PValue^.Size := Size;
  Self.FAllocatedMem := Self.FAllocatedMem + Size;
  Self.AddToList(PValue);
end;

procedure TSEGarbageCollector.AllocMap(const PValue: PSEValue);
var
  Len: Integer;
begin
  PValue^.Kind := sevkMap;
  PValue^.VarMap := TSEValueMap.Create;
  PValue^.Size := 0;
  Self.AddToList(PValue);
end;

procedure TSEGarbageCollector.AllocString(const PValue: PSEValue; const S: String);
begin
  PValue^.Kind := sevkString;
  New(PValue^.VarString);
  PValue^.VarString^ := S;
  PValue^.Size := Length(S);
  Self.FAllocatedMem := Self.FAllocatedMem + ByteLength(PValue^.VarString^);
  Self.AddToList(PValue);
end;

procedure TSEGarbageCollector.Lock(const PValue: PSEValue);
var
  Value: TSEGCValue;
begin
  if (PValue^.Kind <> sevkMap) and (PValue^.Kind <> sevkString) then
    Exit;
  Value := Self.FValueList[PValue^.Ref];
  Value.Lock := True;
  Self.FValueList[PValue^.Ref] := Value;
end;

procedure TSEGarbageCollector.Unlock(const PValue: PSEValue);
var
  Value: TSEGCValue;
begin
  if (PValue^.Kind <> sevkMap) and (PValue^.Kind <> sevkString) then
    Exit;
  Value := Self.FValueList[PValue^.Ref];
  Value.Lock := False;
  Self.FValueList[PValue^.Ref] := Value;
end;

procedure TSEVM.BinaryClear;
var
  I: Integer;
begin
  for I := 0 to High(Self.Binaries) do
    FreeAndNil(Self.Binaries[I]);
  SetLength(Self.Binaries, 1);
  Self.Binaries[0] := TSEBinary.Create;
end;

constructor TSEVM.Create;
begin
  inherited;
  Self.CodePtr := 0;
  Self.IsPaused := False;
  Self.IsDone := True;
  Self.WaitTime := 0;
  Self.StackSize := 65536;
  Self.FrameSize := 1024;
  if VMList = nil then
    VMList := TSEVMList.Create;
  if GC = nil then
    GC := TSEGarbageCollector.Create;
  VMList.Add(Self);
  SetLength(Self.Binaries, 1);
  Self.Binaries[0] := TSEBinary.Create;
end;

destructor TSEVM.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(Self.Binaries) do
    FreeAndNil(Self.Binaries[I]);
  if VMList <> nil then
    VMList.Delete(VMList.IndexOf(Self));
  inherited;
end;

function TSEVM.IsWaited: Boolean;
begin
  Exit(GetTickCount64 < Self.WaitTime);
end;

procedure TSEVM.Reset;
begin
  Self.CodePtr := 0;
  Self.BinaryPtr := 0;
  Self.IsPaused := False;
  Self.IsDone := False;
  Self.Parent.IsDone := False;
  Self.WaitTime := 0;
  SetLength(Self.Stack, Self.StackSize);
  SetLength(Self.Frame, Self.FrameSize);
  FillChar(Self.Stack[0], Length(Self.Stack) * SizeOf(TSEValue), 0);
  FillChar(Self.Frame[0], Length(Self.Frame) * SizeOf(TSEFrame), 0);
  Self.FramePtr := @Self.Frame[0];
  Self.StackPtr := @Self.Stack[0];
  Self.StackPtr := Self.StackPtr + Self.Parent.GlobalVarCount + 64;
end;

procedure TSEVM.Exec;
var
  A, B, C, V,
  OA, OB, OC, OV: PSEValue;
  AA: array[0..15] of PSEValue;
  TV: TSEValue;
  S, S1, S2: String;
  WS, WS1, WS2: WideString;
  FuncNativeInfo: PSEFuncNativeInfo;
  FuncScriptInfo: PSEFuncScriptInfo;
  FuncImportInfo: PSEFuncImportInfo;
  I, J, ArgCountStack, ArgCount, ArgSize: Integer;
  Args: array of TSEValue;
  CodePtrLocal: Integer;
  StackPtrLocal: PSEValue;
  BinaryPtrLocal: Integer;
  BinaryLocal: TSEBinary;
  MMXCount, RegCount: QWord;
  ImportBufferIndex: array [0..31] of QWord;
  ImportBufferData: array [0..8*31] of Byte;
  ImportBufferString: array [0..31] of String;
  ImportBufferWideString: array [0..31] of WideString;
  ImportResult: QWord;
  ImportResultD: TSENumber;
  FuncImport, P, PP, PC: Pointer;
  BinaryLocalCountMinusOne: Integer;

  procedure Push(const Value: TSEValue); inline;
  begin
    StackPtrLocal^ := Value;
    Inc(StackPtrLocal);
  end;

  function Pop: PSEValue; inline;
  begin
    Dec(StackPtrLocal);
    Result := StackPtrLocal;
  end;

  procedure AssignGlobal(const I: Pointer; const Value: PSEValue); inline;
  begin
    Self.Stack[Integer(I)] := Value^;
  end;

  procedure AssignLocal(const I: Pointer; const F: Integer; const Value: PSEValue); inline;
  begin
    ((Self.FramePtr - F)^.Stack + Integer(I))^ := Value^;
  end;

  function GetGlobal(const I: Pointer): PSEValue; inline;
  begin
    Exit(@Self.Stack[Integer(I)]);
  end;

  function GetLocal(const I: Pointer; const F: Integer): PSEValue; inline;
  begin
    Exit((Self.FramePtr - F)^.Stack + Integer(I));
  end;

  function GetGlobalInt(const I: Integer): PSEValue; inline;
  begin
    Exit(@Self.Stack[Integer(I)]);
  end;

  function GetLocalInt(const I, F: Integer): PSEValue; inline;
  begin
    Exit((Self.FramePtr - F)^.Stack + Integer(I));
  end;

  procedure AssignGlobalInt(const I: Integer; const Value: PSEValue); inline;
  begin
    Self.Stack[Integer(I)] := Value^;
  end;

  procedure AssignLocalInt(const I: Integer; const F: Integer; const Value: PSEValue); inline;
  begin
    ((Self.FramePtr - F)^.Stack + Integer(I))^ := Value^;
  end;

{$ifdef SE_COMPUTED_GOTO}
  {$if defined(CPUX86_64) or defined(CPUi386)}
    {$define DispatchGoto :=
      if Self.IsPaused or Self.IsWaited then
      begin
        Self.CodePtr := CodePtrLocal;
        Self.StackPtr := StackPtrLocal;
        Self.BinaryPtr := BinaryPtrLocal;
        Exit;
      end;
      P := DispatchTable[TSEOpcode(Integer(BinaryLocal.Ptr(CodePtrLocal)^.VarPointer))];
      asm
        jmp P;
      end
    }
  {$elseif CPUARM}
    {$define DispatchGoto :=
      if Self.IsPaused or Self.IsWaited then
      begin
        Self.CodePtr := CodePtrLocal;
        Self.StackPtr := StackPtrLocal;
        Self.BinaryPtr := BinaryPtrLocal;
        Exit;
      end;
      P := DispatchTable[TSEOpcode(Integer(BinaryLocal.Ptr(CodePtrLocal)^.VarPointer))];
      asm
        b P;
      end
    }
  {$endif}
{$else}
  {$define DispatchGoto := ;}
{$endif}

label
  Loop, FinishLoop, LoopMMX, LoopMMXAlloc, AllocMMX6, AllocMMX5, AllocMMX4, AllocMMX3, AllocMMX2, AllocMMX1,
  AllocMMX0, LoopMMXFinishAlloc, LoopReg, LoopRegAlloc, AllocRDI, AllocRSI, AllocRDX, AllocRCX, AllocR8, AllocR9, LoopRegFinishAlloc,
  LoopFinishAlloc,
  CallScript, CallNative, CallImport
  {$ifdef SE_COMPUTED_GOTO},
  labelPushConst,
  labelPushGlobalVar,
  labelPushLocalVar,
  labelPushArrayPop,
  labelPopConst,
  labelPopFrame,
  labelAssignGlobalVar,
  labelAssignGlobalArray,
  labelAssignLocalVar,
  labelAssignLocalArray,
  labelJumpEqual,
  labelJumpUnconditional,
  labelJumpEqualOrGreater,
  labelJumpEqualOrLesser,

  labelOperatorAdd2,
  labelOperatorSub2,
  labelOperatorMul2,
  labelOperatorDiv2,

  labelOperatorAdd,
  labelOperatorSub,
  labelOperatorMul,
  labelOperatorDiv,
  labelOperatorMod,
  labelOperatorPow,
  labelOperatorNegative,
  labelOperatorLesser,
  labelOperatorLesserOrEqual,
  labelOperatorGreater,
  labelOperatorGreaterOrEqual,
  labelOperatorEqual,
  labelOperatorNotEqual,
  labelOperatorAnd,
  labelOperatorOr,
  labelOperatorXor,
  labelOperatorNot,

  labelCallRef,
  labelCallNative,
  labelCallScript,
  labelCallImport,
  labelYield,
  labelHlt
  {$endif};

{$ifdef SE_COMPUTED_GOTO}
var
  DispatchTable: array[TSEOpcode] of Pointer = (
    @labelPushConst,
    @labelPushGlobalVar,
    @labelPushLocalVar,
    @labelPushArrayPop,
    @labelPopConst,
    @labelPopFrame,
    @labelAssignGlobalVar,
    @labelAssignGlobalArray,
    @labelAssignLocalVar,
    @labelAssignLocalArray,
    @labelJumpEqual,
    @labelJumpUnconditional,
    @labelJumpEqualOrGreater,
    @labelJumpEqualOrLesser,

    @labelOperatorAdd2,
    @labelOperatorSub2,
    @labelOperatorMul2,
    @labelOperatorDiv2,

    @labelOperatorAdd,
    @labelOperatorSub,
    @labelOperatorMul,
    @labelOperatorDiv,
    @labelOperatorMod,
    @labelOperatorPow,
    @labelOperatorNegative,
    @labelOperatorLesser,
    @labelOperatorLesserOrEqual,
    @labelOperatorGreater,
    @labelOperatorGreaterOrEqual,
    @labelOperatorEqual,
    @labelOperatorNotEqual,
    @labelOperatorAnd,
    @labelOperatorOr,
    @labelOperatorXor,
    @labelOperatorNot,

    @labelCallRef,
    @labelCallNative,
    @labelCallScript,
    @labelCallImport,
    @labelYield,
    @labelHlt
  );
{$endif}

begin
  if Self.IsDone then
    Self.Reset;
  Self.IsYielded := False;
  if Self.IsPaused or Self.IsWaited then
    Exit;
  CodePtrLocal := Self.CodePtr;
  StackPtrLocal := Self.StackPtr;
  BinaryPtrLocal := Self.BinaryPtr;
  BinaryLocal := Self.Binaries[Self.BinaryPtr];
  BinaryLocalCountMinusOne := BinaryLocal.Count - 1;
  GC.CheckForGC;

  try
    DispatchGoto;
    while True do
    begin
      {$ifndef SE_COMPUTED_GOTO}
      case TSEOpcode(Integer(BinaryLocal.Ptr(CodePtrLocal)^.VarPointer)) of
      {$endif}
      {$ifdef SE_COMPUTED_GOTO}labelOperatorAdd{$else}opOperatorAdd{$endif}:
        begin
          B := Pop;
          A := Pop;
          SEValueAdd(StackPtrLocal^, A^, B^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorSub{$else}opOperatorSub{$endif}:
        begin
          B := Pop;
          A := Pop;
          SEValueSub(StackPtrLocal^, A^, B^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorMul{$else}opOperatorMul{$endif}:
        begin
          B := Pop;
          A := Pop;
          SEValueMul(StackPtrLocal^, A^, B^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorDiv{$else}opOperatorDiv{$endif}:
        begin
          B := Pop;
          A := Pop;
          SEValueDiv(StackPtrLocal^, A^, B^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorMod{$else}opOperatorMod{$endif}:
        begin
          B := Pop;
          A := Pop;
          Push(A^ - B^ * Int(TSENumber(A^ / B^)));
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorEqual{$else}opOperatorEqual{$endif}:
        begin
          B := Pop;
          A := Pop;
          SEValueEqual(StackPtrLocal^, A^, B^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorNotEqual{$else}opOperatorNotEqual{$endif}:
        begin
          B := Pop;
          A := Pop;
          SEValueNotEqual(StackPtrLocal^, A^, B^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorLesser{$else}opOperatorLesser{$endif}:
        begin
          B := Pop;
          A := Pop;
          SEValueLesser(StackPtrLocal^, A^, B^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorLesserOrEqual{$else}opOperatorLesserOrEqual{$endif}:
        begin
          B := Pop;
          A := Pop;
          SEValueLesserOrEqual(StackPtrLocal^, A^, B^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorGreater{$else}opOperatorGreater{$endif}:
        begin
          B := Pop;
          A := Pop;
          SEValueGreater(StackPtrLocal^, A^, B^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorGreaterOrEqual{$else}opOperatorGreaterOrEqual{$endif}:
        begin
          B := Pop;
          A := Pop;
          SEValueGreaterOrEqual(StackPtrLocal^, A^, B^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorAnd{$else}opOperatorAnd{$endif}:
        begin
          B := Pop;
          A := Pop;
          Push(Integer(A^) and Integer(B^));
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorOr{$else}opOperatorOr{$endif}:
        begin
          B := Pop;
          A := Pop;
          Push(Integer(A^) or Integer(B^));
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorXor{$else}opOperatorXor{$endif}:
        begin
          B := Pop;
          A := Pop;
          Push(Integer(A^) xor Integer(B^));
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorNot{$else}opOperatorNot{$endif}:
        begin
          A := Pop;
          SEValueNot(StackPtrLocal^, A^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorNegative{$else}opOperatorNegative{$endif}:
        begin
          A := Pop;
          SEValueNeg(StackPtrLocal^, A^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorAdd2{$else}opOperatorAdd2{$endif}:
        begin
          P := BinaryLocal.Ptr(CodePtrLocal + 3)^.VarPointer;
          if P = Pointer($FFFFFFFF) then
            SEValueAdd(StackPtrLocal^, GetGlobal(BinaryLocal.Ptr(CodePtrLocal + 1)^)^, GetGlobal(BinaryLocal.Ptr(CodePtrLocal + 2)^)^)
          else
            SEValueAdd(StackPtrLocal^, GetLocal(BinaryLocal.Ptr(CodePtrLocal + 1)^, Integer(P))^, GetLocal(BinaryLocal.Ptr(CodePtrLocal + 2)^, Integer(P))^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal, 4);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorSub2{$else}opOperatorSub2{$endif}:
        begin
          P := BinaryLocal.Ptr(CodePtrLocal + 3)^.VarPointer;
          if P = Pointer($FFFFFFFF) then
            SEValueSub(StackPtrLocal^, GetGlobal(BinaryLocal.Ptr(CodePtrLocal + 1)^)^, GetGlobal(BinaryLocal.Ptr(CodePtrLocal + 2)^)^)
          else
            SEValueSub(StackPtrLocal^, GetLocal(BinaryLocal.Ptr(CodePtrLocal + 1)^, Integer(P))^, GetLocal(BinaryLocal.Ptr(CodePtrLocal + 2)^, Integer(P))^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal, 4);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorMul2{$else}opOperatorMul2{$endif}:
        begin
          P := BinaryLocal.Ptr(CodePtrLocal + 3)^.VarPointer;
          if P = Pointer($FFFFFFFF) then
            SEValueMul(StackPtrLocal^, GetGlobal(BinaryLocal.Ptr(CodePtrLocal + 1)^)^, GetGlobal(BinaryLocal.Ptr(CodePtrLocal + 2)^)^)
          else
            SEValueMul(StackPtrLocal^, GetLocal(BinaryLocal.Ptr(CodePtrLocal + 1)^, Integer(P))^, GetLocal(BinaryLocal.Ptr(CodePtrLocal + 2)^, Integer(P))^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal, 4);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorDiv2{$else}opOperatorDiv2{$endif}:
        begin
          P := BinaryLocal.Ptr(CodePtrLocal + 3)^.VarPointer;
          if P = Pointer($FFFFFFFF) then
            SEValueDiv(StackPtrLocal^, GetGlobal(BinaryLocal.Ptr(CodePtrLocal + 1)^)^, GetGlobal(BinaryLocal.Ptr(CodePtrLocal + 2)^)^)
          else
            SEValueDiv(StackPtrLocal^, GetLocal(BinaryLocal.Ptr(CodePtrLocal + 1)^, Integer(P))^, GetLocal(BinaryLocal.Ptr(CodePtrLocal + 2)^, Integer(P))^);
          Inc(StackPtrLocal);
          Inc(CodePtrLocal, 4);
          DispatchGoto;
        end;

      {$ifdef SE_COMPUTED_GOTO}labelPushConst{$else}opPushConst{$endif}:
        begin
          Push(BinaryLocal.Ptr(CodePtrLocal + 1)^);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelPushGlobalVar{$else}opPushGlobalVar{$endif}:
        begin
          Push(GetGlobal(BinaryLocal.Ptr(CodePtrLocal + 1)^)^);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelPushLocalVar{$else}opPushLocalVar{$endif}:
        begin
          Push(GetLocal(BinaryLocal.Ptr(CodePtrLocal + 1)^, Integer(BinaryLocal.Ptr(CodePtrLocal + 2)^.VarPointer))^);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelPushArrayPop{$else}opPushArrayPop{$endif}:
        begin
          A := Pop;
          B := Pop;
          case B^.Kind of
            sevkString:
              {$ifdef SE_STRING_UTF8}
                Push(UTF8Copy(B^.VarString^, Integer(A^) + 1, 1));
              {$else}
                Push(B^.VarString^[Integer(A^) + 1]);
              {$endif}
            sevkMap:
              Push(SEMapGet(B^, A^));
            else
              Push(0);
          end;
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelPopConst{$else}opPopConst{$endif}:
        begin
          Dec(StackPtrLocal); // Pop;
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelJumpEqual{$else}opJumpEqual{$endif}:
        begin
          B := Pop;
          A := Pop;
          if SEValueEqual(A^, B^) then
            CodePtrLocal := Integer(BinaryLocal.Ptr(CodePtrLocal + 1)^.VarPointer)
          else
            Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelJumpUnconditional{$else}opJumpUnconditional{$endif}:
        begin
          CodePtrLocal := Integer(BinaryLocal.Ptr(CodePtrLocal + 1)^.VarPointer);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelJumpEqualOrGreater{$else}opJumpEqualOrGreater{$endif}:
        begin
          B := Pop;
          A := Pop;
          if SEValueGreaterOrEqual(A^, B^) then
            CodePtrLocal := Integer(BinaryLocal.Ptr(CodePtrLocal + 1)^.VarPointer)
          else
            Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelJumpEqualOrLesser{$else}opJumpEqualOrLesser{$endif}:
        begin
          B := Pop;
          A := Pop;
          if SEValueLesserOrEqual(A^, B^) then
            CodePtrLocal := Integer(BinaryLocal.Ptr(CodePtrLocal + 1)^.VarPointer)
          else
            Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelCallRef{$else}opCallRef{$endif}:
        begin
          A := Pop; // Ref or map
          case A^.Kind of
            sevkFunction:
              begin
                // Do nothing
              end;
            sevkMap:
              begin
                ArgCount := Integer(BinaryLocal.Ptr(CodePtrLocal + 1)^.VarPointer);
                if ArgCount = 0 then
                  raise Exception.Create('Not a function reference');
                for I := ArgCount - 1 downto 0 do
                  AA[I] := Pop;
                for I := 0 to ArgCount - 1 do
                begin
                  TV := SEMapGet(A^, AA[I]^);
                  A := @TV;
                end;
              end;
            else
              raise Exception.Create('Not a function reference');
          end;
          BinaryLocal.Ptr(CodePtrLocal + 1)^ := Pointer(A^.VarFuncIndx);
          case A^.VarFuncKind of
            sefkScript:
              begin
                goto CallScript;
              end;
            sefkImport:
              begin
                Dec(BinaryLocal.Ptr(CodePtrLocal + 2)^.VarPointer);
                Pop; // import has no this
                goto CallImport;
              end;
            sefkNative:
              begin
                Dec(BinaryLocal.Ptr(CodePtrLocal + 2)^.VarPointer);
                Pop; // native has no this
                goto CallNative;
              end;
          end;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelCallNative{$else}opCallNative{$endif}:
        begin
        CallNative:
          GC.CheckForGC;
          FuncNativeInfo := PSEFuncNativeInfo(BinaryLocal.Ptr(CodePtrLocal + 1)^.VarPointer);
          ArgCount := Integer(BinaryLocal.Ptr(CodePtrLocal + 2)^.VarPointer);
          SetLength(Args, ArgCount);
          for I := ArgCount - 1 downto 0 do
          begin
            Args[I] := Pop^;
          end;
          TV := FuncNativeInfo^.Func(Self, Args);
          if IsDone then
          begin
            Exit;
          end;
          Push(TV);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelCallScript{$else}opCallScript{$endif}:
        begin
        CallScript:
          GC.CheckForGC;
          ArgCount := Integer(BinaryLocal.Ptr(CodePtrLocal + 2)^.VarPointer);
          FuncScriptInfo := Self.Parent.FuncScriptList.Ptr(Integer(BinaryLocal.Ptr(CodePtrLocal + 1)^.VarPointer));
          Inc(Self.FramePtr);
          if Self.FramePtr >= @Self.Frame[Self.FrameSize] then
            raise Exception.Create('Too much recursion');
          Self.FramePtr^.Stack := StackPtrLocal - ArgCount;
          StackPtrLocal := StackPtrLocal + FuncScriptInfo^.VarCount;
          Self.FramePtr^.Code := CodePtrLocal + 3;
          Self.FramePtr^.Binary := BinaryPtrLocal;
          CodePtrLocal := 0;
          BinaryPtrLocal := FuncScriptInfo^.BinaryPos;
          BinaryLocal := Self.Binaries[BinaryPtrLocal];
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelCallImport{$else}opCallImport{$endif}:
        begin
        CallImport:
          GC.CheckForGC;
          FuncImportInfo := Self.Parent.FuncImportList.Ptr(Integer(BinaryLocal.Ptr(CodePtrLocal + 1)^.VarPointer));
          FuncImport := FuncImportInfo^.Func;
          if FuncImport = nil then
            raise Exception.Create(Format('Function "%s" is null', [FuncImportInfo^.Name]));
          ArgCount := Length(FuncImportInfo^.Args);
          ArgSize := ArgCount * 8;
          RegCount := 0;
          {$ifdef LINUX}
          MMXCount := 0;
          {$endif}

          for I := ArgCount - 1 downto 0 do
          begin
            case FuncImportInfo^.Args[I] of
              seakI8:
                begin
                  Int64((@ImportBufferData[I * 8])^) := ShortInt(Round(Pop^.VarNumber));
                  ImportBufferIndex[I] := 0;
                  Inc(RegCount);
                end;
              seakI16:
                begin
                  Int64((@ImportBufferData[I * 8])^) := SmallInt(Round(Pop^.VarNumber));
                  ImportBufferIndex[I] := 0;
                  Inc(RegCount);
                end;
              seakI32:
                begin
                  Int64((@ImportBufferData[I * 8])^) := LongInt(Round(Pop^.VarNumber));
                  ImportBufferIndex[I] := 0;
                  Inc(RegCount);
                end;
              seakI64:
                begin
                  Int64((@ImportBufferData[I * 8])^) := Int64(Round(Pop^.VarNumber));
                  ImportBufferIndex[I] := 0;
                  Inc(RegCount);
                end;
              seakU8:
                begin
                  QWord((@ImportBufferData[I * 8])^) := Byte(Round(Pop^.VarNumber));
                  ImportBufferIndex[I] := 0;
                  Inc(RegCount);
                end;
              seakU16:
                begin
                  QWord((@ImportBufferData[I * 8])^) := Word(Round(Pop^.VarNumber));
                  ImportBufferIndex[I] := 0;
                  Inc(RegCount);
                end;
              seakU32:
                begin
                  QWord((@ImportBufferData[I * 8])^) := LongWord(Round(Pop^.VarNumber));
                  ImportBufferIndex[I] := 0;
                  Inc(RegCount);
                end;
              seakU64:
                begin
                  QWord((@ImportBufferData[I * 8])^) := QWord(Round(Pop^.VarNumber));
                  ImportBufferIndex[I] := 0;
                  Inc(RegCount);
                end;
             { seakF32:
                begin
                  TSENumber((@ImportBufferData[I * 8])^) := Pop^.VarNumber;
                end;}
              seakF64:
                begin
                  TSENumber((@ImportBufferData[I * 8])^) := Pop^.VarNumber;
                  ImportBufferIndex[I] := 1;
                  {$ifdef WINDOWS}
                  Inc(RegCount);
                  {$else}
                  Inc(MMXCount);
                  {$endif}
                end;
              seakBuffer:
                begin
                  A := Pop;
                  if A^.Kind = sevkString then
                  begin
                    ImportBufferString[I] := A^.VarString^ + #0;
                    PChar((@ImportBufferData[I * 8])^) := PChar(ImportBufferString[I]);
                  end else
                  if A^.Kind = sevkBuffer then
                    PChar((@ImportBufferData[I * 8])^) := PChar(A^.VarBuffer^.Ptr)
                  else
                    QWord((@ImportBufferData[I * 8])^) := Round(A^.VarNumber);
                  ImportBufferIndex[I] := 0;
                  Inc(RegCount);
                end;
              seakWBuffer:
                begin
                  A := Pop;
                  if A^.Kind = sevkString then
                  begin
                    ImportBufferWideString[I] := UTF8Decode(A^.VarString^ + #0);
                    PChar((@ImportBufferData[I * 8])^) := PChar(ImportBufferWideString[I]);
                  end else
                  if A^.Kind = sevkBuffer then
                    PWideChar((@ImportBufferData[I * 8])^) := PWideChar(A^.VarBuffer^.Ptr)
                  else
                    QWord((@ImportBufferData[I * 8])^) := Round(A^.VarNumber);
                  ImportBufferIndex[I] := 0;
                  Inc(RegCount);
                end;
            end;
          end;
          P := @ImportBufferData[0];
          PP := @ImportBufferIndex[0];
          {$if defined(WINDOWS)}
          ArgCountStack := Max(0, Int64(RegCount) - 4);
          {$elseif defined(LINUX)}
          ArgCountStack := Max(0, Int64(MMXCount) - 8) + Max(0, Int64(RegCount) - 6);
          {$endif}
          {$ifdef CPUX86_64}
          {$if defined(WINDOWS)}
            asm
              xor  rax,rax
              mov  eax,ArgCount
              mov  r10,rax

              xor  rax,rax
              mov  eax,ArgSize
              mov  r14,rax

              mov  rbx,P
              add  rbx,r14
              mov  rax,PP
              add  rax,r14
              mov  r12,RegCount
            Loop:
              sub  rax,8
              sub  rbx,8
              mov  r13,[rbx]
              mov  r14,[rax]
            LoopReg:
                cmp  r12,4
                jle  LoopRegAlloc // Lower or equal: Register allocation, Higher: Push to stack
              // Push to stack
                push r13 // Always push ...
                jmp  LoopRegFinishAlloc
              LoopRegAlloc:
                cmp  r14,1 // MMX?
                je   LoopMMX

                cmp  r12,1
                je   AllocRCX
                cmp  r12,2
                je   AllocRDX
                cmp  r12,3
                je   AllocR8
              // R9
                mov  r9,r13
                jmp  LoopRegFinishAlloc
              AllocRCX:
                mov  rcx,r13
                jmp  LoopRegFinishAlloc
              AllocRDX:
                mov  rdx,r13
                jmp  LoopRegFinishAlloc
              AllocR8:
                mov  r8,r13
                jmp  LoopRegFinishAlloc

              LoopMMX:
                cmp  r12,1
                je   AllocMMX0
                cmp  r12,2
                je   AllocMMX1
                cmp  r12,3
                je   AllocMMX2
              // MMX3
                movsd xmm3,[rbx]
                jmp  LoopRegFinishAlloc
              AllocMMX0:
                movsd xmm0,[rbx]
                jmp  LoopRegFinishAlloc
              AllocMMX1:
                movsd xmm1,[rbx]
                jmp  LoopRegFinishAlloc
              AllocMMX2:
                movsd xmm2,[rbx]

              LoopRegFinishAlloc:
                dec  r12
            LoopFinishAlloc:
              dec  r10
              cmp  r10,0 // Still have arguments to take care of?
              jne  Loop
            FinishLoop:
              sub  rsp,32
              call [FuncImport]
              mov  ImportResult,rax
              movsd ImportResultD,xmm0
              xor  rax,rax
              mov  eax,ArgCountStack
              mov  ecx,8
              mul  ecx
              add  rsp,rax
              add  rsp,32
            end ['rax', 'rbx', 'rcx', 'rdx', 'r8', 'r9', 'r10', 'r11', 'r12', 'r13', 'r14', 'xmm0', 'xmm1', 'xmm2', 'xmm3'];
          {$elseif defined(LINUX)}
            asm
              xor  rax,rax
              mov  eax,ArgCount
              mov  r10,rax

              xor  rax,rax
              mov  eax,ArgSize
              mov  r14,rax

              mov  rbx,P
              add  rbx,r14
              mov  rax,PP
              add  rax,r14
              mov  r11,MMXCount
              mov  r12,RegCount
            Loop:
              sub  rax,8
              sub  rbx,8
              mov  r13,[rbx]
              mov  r14,[rax]
              cmp  r14,0 // Reg?
              je   LoopReg
            LoopMMX:
                cmp  r11,8
                jle  LoopMMXAlloc // Lower or equal: Register allocation, Higher: Push to stack
              // Push to stack
                push r13
                jmp  LoopMMXFinishAlloc
              LoopMMXAlloc:
                cmp  r11,1
                je   AllocMMX0
                cmp  r11,2
                je   AllocMMX1
                cmp  r11,3
                je   AllocMMX2
                cmp  r11,4
                je   AllocMMX3
                cmp  r11,5
                je   AllocMMX4
                cmp  r11,6
                je   AllocMMX5
                cmp  r11,7
                je   AllocMMX6
              // MMX7
                movsd xmm7,[rbx]
                jmp  LoopMMXFinishAlloc
              AllocMMX6:
                movsd xmm6,[rbx]
                jmp  LoopMMXFinishAlloc
              AllocMMX5:
                movsd xmm5,[rbx]
                jmp  LoopMMXFinishAlloc
              AllocMMX4:
                movsd xmm4,[rbx]
                jmp  LoopMMXFinishAlloc
              AllocMMX3:
                movsd xmm3,[rbx]
                jmp  LoopMMXFinishAlloc
              AllocMMX2:
                movsd xmm2,[rbx]
                jmp  LoopMMXFinishAlloc
              AllocMMX1:
                movsd xmm1,[rbx]
                jmp  LoopMMXFinishAlloc
              AllocMMX0:
                movsd xmm0,[rbx]
              LoopMMXFinishAlloc:
                dec  r11
                jmp  LoopFinishAlloc
            LoopReg:
                cmp  r12,6
                jle  LoopRegAlloc // Lower or equal: Register allocation, Higher: Push to stack
              // Push to stack
                push r13
                jmp  LoopRegFinishAlloc
              LoopRegAlloc:
                cmp  r12,1
                je   AllocRDI
                cmp  r12,2
                je   AllocRSI
                cmp  r12,3
                je   AllocRDX
                cmp  r12,4
                je   AllocRCX
                cmp  r12,5
                je   AllocR9
              // R8
                mov  r8,r13
                jmp  LoopRegFinishAlloc
              AllocRDI:
                mov  rdi,r13
                jmp  LoopRegFinishAlloc
              AllocRSI:
                mov  rsi,r13
                jmp  LoopRegFinishAlloc
              AllocRDX:
                mov  rdx,r13
                jmp  LoopRegFinishAlloc
              AllocRCX:
                mov  rcx,r13
                jmp  LoopRegFinishAlloc
              AllocR9:
                mov  r9,r13
              LoopRegFinishAlloc:
                dec  r12
            LoopFinishAlloc:
              dec  r10
              cmp  r10,0 // Still have arguments to take care of?
              jne  Loop
            FinishLoop:
              call [FuncImport]
              mov  ImportResult,rax
              movsd ImportResultD,xmm0
              xor  rax,rax
              mov  eax,ArgCountStack
              mov  ecx,8
              mul  ecx
              add  rsp,rax
            end ['rsi', 'rdi', 'rax', 'rbx', 'rcx', 'rdx', 'r8', 'r9', 'r10', 'r11', 'r12', 'r13', 'r14', 'xmm0', 'xmm1', 'xmm2', 'xmm3', 'xmm4', 'xmm5', 'xmm6', 'xmm7'];
          {$endif}
          {$else}
          raise Exception.Create('Import external function does not support this CPU architecture');
          {$endif} // CPUX86_64

          case FuncImportInfo^.Return of
            seakI8, seakI16, seakI32:
              begin
                TV := Int64(LongInt(ImportResult))
              end;
            seakI64:
              begin
                TV := Int64(ImportResult)
              end;
            seakU8, seakU16, seakU32:
              begin
                TV := QWord(LongWord(ImportResult))
              end;
            seakU64, seakBuffer, seakWBuffer:
              begin
                TV := QWord(ImportResult)
              end;
           // seakF32,
            seakF64:
              begin
                TV := ImportResultD;
              end;
          end;
          Push(TV);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelPopFrame{$else}opPopFrame{$endif}:
        begin
          CodePtrLocal := Self.FramePtr^.Code;
          StackPtrLocal := Self.FramePtr^.Stack;
          BinaryPtrLocal := Self.FramePtr^.Binary;
          BinaryLocal := Self.Binaries[BinaryPtrLocal];
          Dec(Self.FramePtr);
          if Self.FramePtr < @Self.Frame[0] then
            Break;
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelAssignGlobalVar{$else}opAssignGlobalVar{$endif}:
        begin
          AssignGlobal(BinaryLocal.Ptr(CodePtrLocal + 1)^, Pop);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelAssignLocalVar{$else}opAssignLocalVar{$endif}:
        begin
          AssignLocal(BinaryLocal.Ptr(CodePtrLocal + 1)^, Integer(BinaryLocal.Ptr(CodePtrLocal + 2)^.VarPointer), Pop);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelAssignGlobalArray{$else}opAssignGlobalArray{$endif}:
        begin
          A := BinaryLocal.Ptr(CodePtrLocal + 1);
          V := GetGlobalInt(Integer(A^));
          B := Pop;
          ArgCount := BinaryLocal.Ptr(CodePtrLocal + 2)^;
          if ArgCount = 1 then
            C := Pop
          else
          begin
            for I := ArgCount - 1 downto 0 do
              AA[I] := Pop;
            C := AA[0];
            for I := 1 to ArgCount - 1 do
            begin
              OC := C;
              OV := V;
              TV := SEMapGet(V^, C^);
              V := @TV;
              C := AA[I];
            end;
          end;
          case B^.Kind of
            sevkString:
              begin
                if V^.Kind = sevkString then
                begin
                  {$ifdef SE_STRING_UTF8}
                    S1 := V^.VarString^;
                    S2 := B^.VarString^;
                    UTF8Delete(S1, Integer(C^) + 1, 1);
                    S := UTF8Copy(S2, 1, 1);
                    UTF8Insert(S, S1, Integer(C^) + 1);
                    V^.VarString^ := S1;
                  {$else}
                    V^.VarString^[Integer(C^) + 1] := B^.VarString^[1];
                  {$endif}
                  // Self.Stack[A] := S;
                  if ArgCount >= 2 then
                    SEMapSet(OV^, OC^, V^);
                end else
                begin
                  SEMapSet(V^, C^, B^);
                  if ArgCount = 1 then
                    AssignGlobalInt(Integer(A^), V);
                end;
              end;
            sevkNumber, sevkBoolean:
              begin
                if V^.Kind = sevkString then
                begin
                  {$ifdef SE_STRING_UTF8}
                    S1 := V^.VarString^;
                    UTF8Delete(S1, Integer(C^) + 1, 1);
                    S := Char(Round(B^.VarNumber));
                    UTF8Insert(S, S1, Integer(C^) + 1);
                    V^.VarString^ := S1;
                  {$else}
                    V^.VarString^[Integer(C^) + 1] := Char(Round(B^.VarNumber));
                  {$endif}
                  // Self.Stack[A] := S;
                  if ArgCount >= 2 then
                    SEMapSet(OV^, OC^, V^);
                end else
                begin
                  SEMapSet(V^, C^, B^);
                  if ArgCount = 1 then
                    AssignGlobalInt(Integer(A^), V);
                end;
              end;
            else
              begin
                SEMapSet(V^, C^, B^);
                if ArgCount = 1 then
                  AssignGlobalInt(Integer(A^), V);
              end;
          end;
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelAssignLocalArray{$else}opAssignLocalArray{$endif}:
        begin
          A := BinaryLocal.Ptr(CodePtrLocal + 1);
          J := Integer(BinaryLocal.Ptr(CodePtrLocal + 3)^.VarPointer);
          V := GetLocalInt(Integer(A^), J);
          B := Pop;
          ArgCount := BinaryLocal.Ptr(CodePtrLocal + 2)^;
          if ArgCount = 1 then
            C := Pop
          else
          begin
            for I := ArgCount - 1 downto 0 do
              AA[I] := Pop;
            C := AA[0];
            for I := 1 to ArgCount - 1 do
            begin
              OC := C;
              OV := V;
              TV := SEMapGet(V^, C^);
              V := @TV;
              C := AA[I];
            end;
          end;
          case B^.Kind of
            sevkString:
              begin
                if V^.Kind = sevkString then
                begin
                  {$ifdef SE_STRING_UTF8}
                    S1 := V^.VarString^;
                    S2 := B^.VarString^;
                    UTF8Delete(S1, Integer(C^) + 1, 1);
                    S := UTF8Copy(S2, 1, 1);
                    UTF8Insert(S, S1, Integer(C^) + 1);
                    V^.VarString^ := S1;
                  {$else}
                    V^.VarString^[Integer(C^) + 1] := B^.VarString^[1];
                  {$endif}
                  // Self.Stack[A] := S;
                  if ArgCount >= 2 then
                    SEMapSet(OV^, OC^, V^);
                end else
                begin
                  SEMapSet(V^, C^, B^);
                  if ArgCount = 1 then
                    AssignLocalInt(Integer(A^), J, V);
                end;
              end;
            sevkNumber, sevkBoolean:
              begin
                if V^.Kind = sevkString then
                begin
                  {$ifdef SE_STRING_UTF8}
                    S1 := V^.VarString^;
                    UTF8Delete(S1, Integer(C^) + 1, 1);
                    S := Char(Round(B^.VarNumber));
                    UTF8Insert(S, S1, Integer(C^) + 1);
                    V^.VarString^ := S1;
                  {$else}
                    V^.VarString^[Integer(C^) + 1] := Char(Round(B^.VarNumber));
                  {$endif}
                  // Self.Stack[A] := S;
                  if ArgCount >= 2 then
                    SEMapSet(OV^, OC^, V^);
                end else
                begin
                  SEMapSet(V^, C^, B^);
                  if ArgCount = 1 then
                    AssignLocalInt(Integer(A^), J, V);
                end;
              end;
            else
              begin
                SEMapSet(V^, C^, B^);
                if ArgCount = 1 then
                  AssignLocalInt(Integer(A^), J, V);
              end;
          end;
          Inc(CodePtrLocal, 4);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelYield{$else}opYield{$endif}:
        begin
          Self.IsYielded := True;
          Inc(CodePtrLocal);
          Self.CodePtr := CodePtrLocal;
          Self.StackPtr := StackPtrLocal;
          Self.BinaryPtr := BinaryPtrLocal;
          Exit;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelOperatorPow{$else}opOperatorPow{$endif}:
        begin
          B := Pop;
          A := Pop;
          Push(Power(A^.VarNumber, B^.VarNumber));
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifdef SE_COMPUTED_GOTO}labelHlt{$else}opHlt{$endif}:
        begin
          Self.CodePtr := CodePtrLocal;
          Self.StackPtr := StackPtrLocal;
          Self.BinaryPtr := BinaryPtrLocal;
          Self.IsDone := True;
          Self.Parent.IsDone := True;
          Exit;
        end;
      {$ifndef SE_COMPUTED_GOTO}
      end;
      if Self.IsPaused or Self.IsWaited then
      begin
        Self.CodePtr := CodePtrLocal;
        Self.StackPtr := StackPtrLocal;
        Self.BinaryPtr := BinaryPtrLocal;
        Exit;
      end;
      {$endif}
    end;
  except
    on E: Exception do
    begin
      I := 0;
      while I <= Self.Parent.LineOfCodeList.Count - 1 do
      begin
        if CodePtrLocal <= Self.Parent.LineOfCodeList[I] then
          break;
        Inc(I);
      end;
      raise Exception.Create(Format('Runtime error %s: "%s" at line %d', [E.ClassName, E.Message, I + 1]));
    end;
  end;
  Self.CodePtr := CodePtrLocal;
  Self.StackPtr := StackPtrLocal;
  Self.BinaryPtr := BinaryPtrLocal;
  Self.IsDone := True;
  Self.Parent.IsDone := True;
end;

constructor TEvilC.Create;
begin
  inherited;
  Self.VM := TSEVM.Create;
  Self.TokenList := TSETokenList.Create;
  Self.OpcodeInfoList := TSEOpcodeInfoList.Create;
  Self.VarList := TSEIdentList.Create;
  Self.FuncNativeList := TSEFuncNativeList.Create;
  Self.FuncScriptList := TSEFuncScriptList.Create;
  Self.FuncImportList := TSEFuncImportList.Create;
  Self.ConstMap := TSEConstMap.Create;
  Self.ScopeStack := TSEScopeStack.Create;
  Self.ScopeFunc := TSEScopeStack.Create;
  Self.LineOfCodeList := TIntegerList.Create;
  Self.IncludeList := TStringList.Create;
  Self.IncludePathList := TStringList.Create;
  Self.CurrentFileList := TStringList.Create;
  Self.LocalVarCountList := TIntegerList.Create;
  //
  Self.TokenList.Capacity := 1024;
  Self.VarList.Capacity := 256;
  Self.FuncNativeList.Capacity := 64;
  Self.FuncScriptList.Capacity := 64;
  Self.FuncImportList.Capacity := 64;
  Self.ConstMap.Capacity := 64;
  Self.ScopeStack.Capacity := 16;
  Self.LineOfCodeList.Capacity := 1024;
  //
  Self.VM.Parent := Self;
  Self.RegisterFunc('buffer_create', @TBuiltInFunction(nil).SEBufferCreate, 1);
  Self.RegisterFunc('buffer_length', @TBuiltInFunction(nil).SEBufferLength, 1);
  Self.RegisterFunc('buffer_u8_get', @TBuiltInFunction(nil).SEBufferGetU8, 1);
  Self.RegisterFunc('buffer_u16_get', @TBuiltInFunction(nil).SEBufferGetU16, 1);
  Self.RegisterFunc('buffer_u32_get', @TBuiltInFunction(nil).SEBufferGetU32, 1);
  Self.RegisterFunc('buffer_u64_get', @TBuiltInFunction(nil).SEBufferGetU64, 1);
  Self.RegisterFunc('buffer_i8_get', @TBuiltInFunction(nil).SEBufferGetI8, 1);
  Self.RegisterFunc('buffer_i16_get', @TBuiltInFunction(nil).SEBufferGetI16, 1);
  Self.RegisterFunc('buffer_i32_get', @TBuiltInFunction(nil).SEBufferGetI32, 1);
  Self.RegisterFunc('buffer_i64_get', @TBuiltInFunction(nil).SEBufferGetI64, 1);
  Self.RegisterFunc('buffer_f64_get', @TBuiltInFunction(nil).SEBufferGetF64, 1);
  Self.RegisterFunc('buffer_u8_set', @TBuiltInFunction(nil).SEBufferSetU8, 2);
  Self.RegisterFunc('buffer_u16_set', @TBuiltInFunction(nil).SEBufferSetU16, 2);
  Self.RegisterFunc('buffer_u32_set', @TBuiltInFunction(nil).SEBufferSetU32, 2);
  Self.RegisterFunc('buffer_u64_set', @TBuiltInFunction(nil).SEBufferSetU64, 2);
  Self.RegisterFunc('buffer_i8_set', @TBuiltInFunction(nil).SEBufferSetI8, 2);
  Self.RegisterFunc('buffer_i16_set', @TBuiltInFunction(nil).SEBufferSetI16, 2);
  Self.RegisterFunc('buffer_i32_set', @TBuiltInFunction(nil).SEBufferSetI32, 2);
  Self.RegisterFunc('buffer_i64_set', @TBuiltInFunction(nil).SEBufferSetI64, 2);
  Self.RegisterFunc('buffer_f64_set', @TBuiltInFunction(nil).SEBufferSetF64, 2);
  Self.RegisterFunc('string_to_buffer', @TBuiltInFunction(nil).SEStringToBuffer, 1);
  Self.RegisterFunc('buffer_to_string', @TBuiltInFunction(nil).SEBufferToString, 1);
  Self.RegisterFunc('wbuffer_to_string', @TBuiltInFunction(nil).SEWBufferToString, 1);
  Self.RegisterFunc('typeof', @TBuiltInFunction(nil).SETypeOf, 1);
  Self.RegisterFunc('get', @TBuiltInFunction(nil).SEGet, 1);
  Self.RegisterFunc('set', @TBuiltInFunction(nil).SESet, 2);
  Self.RegisterFunc('string', @TBuiltInFunction(nil).SEString, 1);
  Self.RegisterFunc('number', @TBuiltInFunction(nil).SENumber, 1);
  Self.RegisterFunc('wait', @TBuiltInFunction(nil).SEWait, 1);
  Self.RegisterFunc('length', @TBuiltInFunction(nil).SELength, 1);
  Self.RegisterFunc('map_create', @TBuiltInFunction(nil).SEMapCreate, -1);
  Self.RegisterFunc('___map_create', @TBuiltInFunction(nil).SEMapCreate, -1);
  Self.RegisterFunc('map_delete', @TBuiltInFunction(nil).SEMapDelete, 2);
  Self.RegisterFunc('map_keys_get', @TBuiltInFunction(nil).SEMapKeysGet, 1);
  Self.RegisterFunc('array_resize', @TBuiltInFunction(nil).SEArrayResize, 2);
  Self.RegisterFunc('sign', @TBuiltInFunction(nil).SESign, 1);
  Self.RegisterFunc('min', @TBuiltInFunction(nil).SEMin, -1);
  Self.RegisterFunc('max', @TBuiltInFunction(nil).SEMax, 1);
  Self.RegisterFunc('range', @TBuiltInFunction(nil).SERange, -1);
  Self.RegisterFunc('pow', @TBuiltInFunction(nil).SEPow, 2);
  Self.RegisterFunc('string_empty', @TBuiltInFunction(nil).SEStringEmpty, 1);
  Self.RegisterFunc('string_grep', @TBuiltInFunction(nil).SEStringGrep, 2);
  Self.RegisterFunc('string_format', @TBuiltInFunction(nil).SEStringFormat, 2);
  Self.RegisterFunc('string_split', @TBuiltInFunction(nil).SEStringSplit, 2);
  Self.RegisterFunc('string_find', @TBuiltInFunction(nil).SEStringFind, 2);
  Self.RegisterFunc('string_delete', @TBuiltInFunction(nil).SEStringDelete, 3);
  Self.RegisterFunc('string_insert', @TBuiltInFunction(nil).SEStringInsert, 3);
  Self.RegisterFunc('string_replace', @TBuiltInFunction(nil).SEStringReplace, 3);
  Self.RegisterFunc('string_uppercase', @TBuiltInFunction(nil).SEStringUpperCase, 1);
  Self.RegisterFunc('string_lowercase', @TBuiltInFunction(nil).SEStringLowerCase, 1);
  Self.RegisterFunc('string_find_regex', @TBuiltInFunction(nil).SEStringFindRegex, 2);
  Self.RegisterFunc('string_concat', @TBuiltInFunction(nil).SEStringConcat, 3);
  Self.RegisterFunc('string_trim', @TBuiltInFunction(nil).SEStringTrim, 1);
  Self.RegisterFunc('string_trim_left', @TBuiltInFunction(nil).SEStringTrimLeft, 1);
  Self.RegisterFunc('string_trim_right', @TBuiltInFunction(nil).SEStringTrimRight, 1);
  Self.RegisterFunc('lerp', @TBuiltInFunction(nil).SELerp, 3);
  Self.RegisterFunc('slerp', @TBuiltInFunction(nil).SESLerp, 3);
  Self.RegisterFunc('write', @TBuiltInFunction(nil).SEWrite, -1);
  Self.RegisterFunc('writeln', @TBuiltInFunction(nil).SEWriteln, -1);
  Self.RegisterFunc('ease_in_quad', @TBuiltInFunction(nil).SEEaseInQuad, 1);
  Self.RegisterFunc('ease_out_quad', @TBuiltInFunction(nil).SEEaseOutQuad, 1);
  Self.RegisterFunc('ease_in_out_quad', @TBuiltInFunction(nil).SEEaseInOutQuad, 1);
  Self.RegisterFunc('ease_in_cubic', @TBuiltInFunction(nil).SEEaseInCubic, 1);
  Self.RegisterFunc('ease_out_cubic', @TBuiltInFunction(nil).SEEaseOutCubic, 1);
  Self.RegisterFunc('ease_in_out_cubic', @TBuiltInFunction(nil).SEEaseInOutQuad, 1);
  Self.RegisterFunc('ticks', @TBuiltInFunction(nil).SEGetTickCount, 0);
  Self.RegisterFunc('dt_now', @TBuiltInFunction(nil).SEDTNow, 0);
  Self.RegisterFunc('dt_year_get', @TBuiltInFunction(nil).SEDTGetYear, 1);
  Self.RegisterFunc('dt_month_get', @TBuiltInFunction(nil).SEDTGetMonth, 1);
  Self.RegisterFunc('dt_day_get', @TBuiltInFunction(nil).SEDTGetDay, 1);
  Self.RegisterFunc('dt_hour_get', @TBuiltInFunction(nil).SEDTGetHour, 1);
  Self.RegisterFunc('dt_minute_get', @TBuiltInFunction(nil).SEDTGetMinute, 1);
  Self.RegisterFunc('dt_date_set', @TBuiltInFunction(nil).SEDTSetDate, 3);
  Self.RegisterFunc('dt_time_set', @TBuiltInFunction(nil).SEDTSetTime, 4);
  Self.RegisterFunc('dt_day_add', @TBuiltInFunction(nil).SEDTDayAdd, 2);
  Self.RegisterFunc('dt_month_add', @TBuiltInFunction(nil).SEDTMonthAdd, 2);
  Self.RegisterFunc('dt_year_add', @TBuiltInFunction(nil).SEDTYearAdd, 2);
  Self.RegisterFunc('random', @TBuiltInFunction(nil).SERandom, 1);
  Self.RegisterFunc('rnd', @TBuiltInFunction(nil).SERnd, 0);
  Self.RegisterFunc('round', @TBuiltInFunction(nil).SERound, 1);
  Self.RegisterFunc('floor', @TBuiltInFunction(nil).SEFloor, 1);
  Self.RegisterFunc('ceil', @TBuiltInFunction(nil).SECeil, 1);
  Self.RegisterFunc('sin', @TBuiltInFunction(nil).SESin, 1);
  Self.RegisterFunc('cos', @TBuiltInFunction(nil).SECos, 1);
  Self.RegisterFunc('tan', @TBuiltInFunction(nil).SETan, 1);
  Self.RegisterFunc('cot', @TBuiltInFunction(nil).SECot, 1);
  Self.RegisterFunc('mem_object_count', @TBuiltInFunction(nil).SEGCObjectCount, 0);
  Self.RegisterFunc('mem_used', @TBuiltInFunction(nil).SEGCUsed, 0);
  Self.RegisterFunc('mem_gc', @TBuiltInFunction(nil).SEGCCollect, 0);
  Self.AddDefaultConsts;
  Self.Source := '';
end;

destructor TEvilC.Destroy;
begin
  FreeAndNil(Self.VM);
  FreeAndNil(Self.TokenList);
  FreeAndNil(Self.OpcodeInfoList);
  FreeAndNil(Self.VarList);
  FreeAndNil(Self.FuncNativeList);
  FreeAndNil(Self.FuncScriptList);
  FreeAndNil(Self.FuncImportList);
  FreeAndNil(Self.ConstMap);
  FreeAndNil(Self.ScopeStack);
  FreeAndNil(Self.ScopeFunc);
  FreeAndNil(Self.LineOfCodeList);
  FreeAndNil(Self.IncludeList);
  FreeAndNil(Self.IncludePathList);
  FreeAndNil(Self.CurrentFileList);
  FreeAndNil(Self.LocalVarCountList);
  inherited;
end;

procedure TEvilC.AddDefaultConsts;
begin
  Self.ConstMap.AddOrSetValue('PI', PI);
  Self.ConstMap.AddOrSetValue('true', True);
  Self.ConstMap.AddOrSetValue('false', False);
  Self.ConstMap.AddOrSetValue('null', SENull);
  Self.ConstMap.AddOrSetValue('os', GetOS);
end;

procedure TEvilC.SetSource(V: String);
begin
  Self.Reset;
  Self.FSource := V;
end;

function TEvilC.InternalIdent: String; inline;
begin
  Inc(Self.FInternalIdentCount);
  Result := IntToStr(FInternalIdentCount);
end;

function TEvilC.IsWaited: Boolean;
begin
  Exit(Self.VM.IsWaited);
end;

function TEvilC.GetIsPaused: Boolean;
begin
  Exit(Self.VM.IsPaused);
end;

procedure TEvilC.SetIsPaused(V: Boolean);
begin
  Self.VM.IsPaused := V;
end;

function TEvilC.IsYielded: Boolean;
begin
  Exit(Self.VM.IsYielded);
end;

procedure TEvilC.Lex(const IsIncluded: Boolean = False);
var
  Ln, Col: Integer;
  Pos: Integer = 0;
  Token: TSEToken;
  C, PC, NC: Char;

  function PeekAtNextChar: Char; inline;
  var
    P: Integer;
  begin
    P := Pos + 1;
    if P > Length(Self.Source) then
      Exit(#0);
    Exit(Self.Source[P]);
  end;

  function NextChar: Char; inline;
  begin
    Inc(Pos);
    Inc(Col);
    if Pos > Length(Self.Source) then
      Exit(#0);
    if Self.Source[Pos] = #10 then
    begin
      Inc(Ln);
      Col := 1;
    end;
    Exit(Self.Source[Pos]);
  end;

  procedure Error(const S: String; const N: String = '');
  begin
    ErrorLn := Ln;
    ErrorCol := Col;
    if N = '' then
      raise Exception.CreateFmt('[%d:%d] %s', [Ln, Col, S])
    else
      raise Exception.CreateFmt('[%s:%d:%d] %s', [N, Ln, Col, S]);
  end;

var
  IsLoopDone: Boolean;
  PrevQuote: Char;
  SL: TStrings;
  BackupSource: String;
  IsPathFound: Boolean;
  S,
  Path: String;
  IsString: Boolean = False;

label
  IsStringLabel, EndLabel;

begin
  Ln := 1;
  Col := 1;
  ErrorLn := -1;
  ErrorCol := -1;
  repeat
    Token.Value := '';
    repeat
      C := NextChar;
    until (not (C in [#1..#32])) and (C <> ';');
    Token.Ln := Ln;
    Token.Col := Col;
    if Self.CurrentFileList.Count > 0 then
      Token.BelongedFileName := Self.CurrentFileList[Self.CurrentFileList.Count - 1]
    else
      Token.BelongedFileName := '';
    case C of
      #0:
        if not IsIncluded then
          Token.Kind := tkEOF
        else
          continue;
      '.':
        Token.Kind := tkDot;
      '&':
        begin
          if PeekAtNextChar = '&' then
          begin
            NextChar;
          end;
          Token.Kind := tkAnd;
        end;
      '|':
        begin
          if PeekAtNextChar = '|' then
          begin
            NextChar;
          end;
          Token.Kind := tkOr;
        end;
      '~':
        begin
          if PeekAtNextChar = '~' then
          begin
            NextChar;
          end;
          Token.Kind := tkXor;
        end;
      '!':
        begin
          if PeekAtNextChar = '=' then
          begin
            NextChar;
            Token.Kind := tkNotEqual;
          end else
          if Pos > 1 then
          begin
            PC := Self.Source[Pos - 1];
            NC := PeekAtNextChar;
            if ((PC = ' ') or (PC = '(') or (PC = '=') or (PC = ',')) and (NC <> ' ') then
              Token.Kind := tkNot;
          end;
        end;
      ',':
        Token.Kind := tkComma;
      '(':
        Token.Kind := tkBracketOpen;
      ')':
        Token.Kind := tkBracketClose;
      '[':
        Token.Kind := tkSquareBracketOpen;
      ']':
        Token.Kind := tkSquareBracketClose;
      '{':
        Token.Kind := tkBegin;
      '}':
        begin
          if IsString then
            goto IsStringLabel;
          Token.Kind := tkEnd;
        end;
      ':':
        Token.Kind := tkColon;
      '''', '"':
        begin
          PrevQuote := C;
          Token.Kind := tkString;
          repeat
            IsLoopDone := False;
            C := NextChar;
            case C of
              #0:
                Error('Unterminated string literal', Token.BelongedFileName);
              '$':
                begin
                  if PeekAtNextChar = '{' then
                  begin
                    NextChar;
                    TokenList.Add(Token);
                    // Add a plus sign
                    Token.Value := '';
                    Token.Kind := tkAdd;
                    TokenList.Add(Token);
                    // Add string function
                    Token.Value := 'string';
                    Token.Kind := tkIdent;
                    TokenList.Add(Token);
                    Token.Value := '';
                    Token.Kind := tkBracketOpen;
                    TokenList.Add(Token);
                    //
                    IsString := True;
                    goto EndLabel;
                    //
                  IsStringLabel:
                    IsString := False;
                    Token.Value := '';
                    Token.Kind := tkBracketClose;
                    TokenList.Add(Token);
                    // Add a plus sign
                    Token.Kind := tkAdd;
                    TokenList.Add(Token);
                    Token.Kind := tkString;
                  end else
                    Token.Value := Token.Value + C;
                end;
              '\':
                begin
                  if PeekAtNextChar = 'n' then
                  begin
                    NextChar;
                    Token.Value := Token.Value + #10;
                  end else
                  if PeekAtNextChar <> #0 then
                  begin
                    Token.Value := Token.Value + NextChar;
                  end;
                end;
              else
                begin
                  if C = PrevQuote then
                    IsLoopDone := True
                  else
                    Token.Value := Token.Value + C;
                end;
            end;
          until IsLoopDone;
        end;
      '+':
        begin
          Token.Kind := tkAdd;
          if PeekAtNextChar = '=' then
          begin
            Token.Kind := tkOpAssign;
            Token.Value := C;
            NextChar;
          end;
        end;
      '^':
        begin
          Token.Kind := tkPow;
        end;
      '-':
        begin
          Token.Kind := tkSub;
          if PeekAtNextChar = '=' then
          begin
            Token.Kind := tkOpAssign;
            Token.Value := C;
            NextChar;
          end else
          if Pos > 1 then
          begin
            PC := Self.Source[Pos - 1];
            NC := PeekAtNextChar;
            if ((PC = ' ') or (PC = '(') or (PC = '=') or (PC = ',')) and (NC <> ' ') then
              Token.Kind := tkNegative;
          end;
        end;
      '*':
        begin
          Token.Kind := tkMul;
          if PeekAtNextChar = '=' then
          begin
            Token.Kind := tkOpAssign;
            Token.Value := C;
            NextChar;
          end;
        end;
      '/':
        begin
          Token.Kind := tkDiv;
          if PeekAtNextChar = '/' then
          begin
            repeat
              NextChar;
            until (PeekAtNextChar = #10) or (PeekAtNextChar = #0);
            continue;
          end else
          if PeekAtNextChar = '*' then
          begin
            repeat
              C := NextChar;
            until ((C = '*') and (PeekAtNextChar = '/')) or (C = #0);
            NextChar;
            continue;
          end else
          if PeekAtNextChar = '=' then
          begin
            Token.Kind := tkOpAssign;
            Token.Value := C;
            NextChar;
          end;
        end;
      '=':
        begin
          if PeekAtNextChar = '=' then
          begin
            NextChar;
          end;
          Token.Kind := tkEqual;
        end;
      '<':
        begin
          if PeekAtNextChar = '=' then
          begin
            NextChar;
            Token.Kind := tkSmallerOrEqual;
          end else
          if PeekAtNextChar = '>' then
          begin
            NextChar;
            Token.Kind := tkNotEqual;
          end else
            Token.Kind := tkSmaller;
        end;
      '>':
        begin
          if PeekAtNextChar = '=' then
          begin
            NextChar;
            Token.Kind := tkGreaterOrEqual;
          end else
            Token.Kind := tkGreater;
        end;
      '%':
        Token.Kind := tkMod;
      '0'..'9':
        begin
          Token.Kind := tkNumber;
          if (C = '0') and (PeekAtNextChar = 'x') then
          begin
            NextChar;
            while PeekAtNextChar in ['0'..'9', 'A'..'F', 'a'..'f'] do
            begin
              C := NextChar;
              Token.Value := Token.Value + C;
            end;
            Token.Value := IntToStr(Hex2Dec64(Token.Value));
          end else
          begin
            Token.Value := C;
            while PeekAtNextChar in ['0'..'9', '.'] do
            begin
              C := NextChar;
              Token.Value := Token.Value + C;
              if (C = '.') and not (PeekAtNextChar in ['0'..'9']) then
                Error('Invalid number');
            end;
          end;
        end;
      '#':
        begin
          C := PeekAtNextChar;
          while C in ['0'..'9', 'A'..'Z', 'a'..'z', '_'] do
          begin
            Token.Value := Token.Value + NextChar;
            C := PeekAtNextChar;
          end;
          case Token.Value of
            'require':
              begin
                Token.Value := '';
                C := PeekAtNextChar;
                while C = ' ' do
                begin
                  NextChar;
                  C := PeekAtNextChar;
                end;
                C := PeekAtNextChar;
                while C in ['0'..'9', 'A'..'Z', 'a'..'z', '_'] do
                begin
                  Token.Value := Token.Value + NextChar;
                  C := PeekAtNextChar;
                end;
                if Token.Value <> GetOS then
                begin
                  C := #0;
                  goto EndLabel;
                end else
                  Continue;
              end
            else
              Error('Unhandled directive ' + C);
          end;
        end;
      'A'..'Z', 'a'..'z', '_':
        begin
          Token.Value := C;
          C := PeekAtNextChar;
          while C in ['0'..'9', 'A'..'Z', 'a'..'z', '_'] do
          begin
            Token.Value := Token.Value + NextChar;
            C := PeekAtNextChar;
          end;
          case Token.Value of
            'using':
              begin
                C := PeekAtNextChar;
                while C = ' ' do
                begin
                  NextChar;
                  C := PeekAtNextChar;
                end;

                C := NextChar;
                if not (C in ['''', '"']) then
                  Error('Expected "''"');

                Token.Value := '';
                C := PeekAtNextChar;
                while (C <> '''') and (C <> '"') and (C <> #10) and (C <> #0) do
                begin
                  Token.Value := Token.Value + NextChar;
                  C := PeekAtNextChar;
                end;

                C := NextChar;
                if not (C in ['''', '"']) then
                  Error('Expected "''"');

                Token.Value := Trim(Token.Value);
                Path := Token.Value;
                if not FileExists(Path) then
                begin
                  IsPathFound := False;
                  for S in Self.IncludePathList do
                  begin
                    Path := S + Token.Value;
                    if FileExists(Path) then
                    begin
                      IsPathFound := True;
                      break;
                    end;
                  end;
                  if not IsPathFound then
                    Error(Format('"%s" not found', [Path]));
                end;
                if Self.IncludeList.IndexOf(Path) < 0 then
                begin
                  BackupSource := Source;
                  SL := TStringList.Create;
                  try
                    Self.CurrentFileList.Add(Path);
                    SL.LoadFromFile(Path);
                    FSource := SL.Text;
                    Self.Lex(True);
                    Self.CurrentFileList.Pop;
                  finally
                    SL.Free;
                  end;
                  FSource := BackupSource;
                  Self.IncludeList.Add(Path);
                end;
                C := PeekAtNextChar;
                continue;
              end;
            'if':
              Token.Kind := tkIf;
            'else':
              Token.Kind := tkElse;
            'for':
              Token.Kind := tkFor;
            'in':
              Token.Kind := tkIn;
            'to':
              Token.Kind := tkTo;
            'do':
              Token.Kind := tkDo;
            'downto':
              Token.Kind := tkDownto;
            'while':
              Token.Kind := tkWhile;
            'switch':
              Token.Kind := tkSwitch;
            'case':
              Token.Kind := tkCase;
            'default':
              Token.Kind := tkDefault;
            'continue':
              Token.Kind := tkContinue;
            'break':
              Token.Kind := tkBreak;
            'yield':
              Token.Kind := tkYield;
            'return':
              Token.Kind := tkReturn;
            'fn':
              Token.Kind := tkFunctionDecl;
            'void', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f64', 'buffer', 'wbuffer':
              Token.Kind := tkAtom;
            'import':
              Token.Kind := tkImport;
            else
              Token.Kind := tkIdent;
          end;
        end;
      else
        Error('Unhandled symbol ' + C);
    end;
    TokenList.Add(Token);
EndLabel:
  until C = #0;
  Self.IsLex := True;
end;

procedure TEvilC.Parse;
var
  Pos: Integer = -1;
  Token: TSEToken;
  ContinueStack: TSEListStack;
  BreakStack: TSEListStack;
  ReturnStack: TSEListStack;

  procedure Error(const S: String; const Token: TSEToken);
  begin
    ErrorLn := Token.Ln;
    ErrorCol := Token.Col;
    if Token.BelongedFileName = '' then
      raise Exception.CreateFmt('[%d:%d] %s', [Token.Ln, Token.Col, S])
    else
      raise Exception.CreateFmt('[%s:%d:%d] %s', [Token.BelongedFileName, Token.Ln, Token.Col, S]);
  end;

  function FindFunc(const Name: String): Pointer; inline; overload;
  var
    I: Integer;
  begin
    for I := Self.FuncScriptList.Count - 1 downto 0 do
    begin
      Result := Self.FuncScriptList.Ptr(I);
      if PSEFuncScriptInfo(Result)^.Name = Name then
        Exit(Result);
    end;
    for I := Self.FuncImportList.Count - 1 downto 0 do
    begin
      Result := Self.FuncImportList.Ptr(I);
      if PSEFuncImportInfo(Result)^.Name = Name then
        Exit(Result);
    end;
    for I := Self.FuncNativeList.Count - 1 downto 0 do
    begin
      Result := Self.FuncNativeList.Ptr(I);
      if PSEFuncNativeInfo(Result)^.Name = Name then
        Exit(Result);
    end;
    Exit(nil);
  end;

  function FindFuncNative(const Name: String; var Ind: Integer): PSEFuncNativeInfo; inline;
  var
    I: Integer;
  begin
    for I := Self.FuncNativeList.Count - 1 downto 0 do
    begin
      Result := Self.FuncNativeList.Ptr(I);
      if Result^.Name = Name then
      begin
        Ind := I;
        Exit(Result);
      end;
    end;
    Exit(nil);
  end;

  function FindFuncScript(const Name: String; var Ind: Integer): PSEFuncScriptInfo; inline;
  var
    I: Integer;
  begin
    for I := Self.FuncScriptList.Count - 1 downto 0 do
    begin
      Result := Self.FuncScriptList.Ptr(I);
      if Result^.Name = Name then
      begin
        Ind := I;
        Exit(Result);
      end;
    end;
    Exit(nil);
  end;

  function FindFuncImport(const Name: String; var Ind: Integer): PSEFuncImportInfo; inline;
  var
    I: Integer;
  begin
    for I := Self.FuncImportList.Count - 1 downto 0 do
    begin
      Result := Self.FuncImportList.Ptr(I);
      if Result^.Name = Name then
      begin
        Ind := I;
        Exit(Result);
      end;
    end;
    Exit(nil);
  end;

  function FindFunc(const Name: String; var Kind: TSEFuncKind; var Ind: Integer): Pointer; inline; overload;
  begin
    Result := FindFuncScript(Name, Ind);
    if Result = nil then
    begin
      Result := FindFuncNative(Name, Ind);
      if Result = nil then
      begin
        Result := FindFuncImport(Name, Ind);
        if Result <> nil then
          Kind := sefkImport;
      end else
        Kind := sefkNative;
    end else
      Kind := sefkScript;
  end;

  function FindVar(const Name: String): PSEIdent; inline;
  var
    I: Integer;
  begin
    for I := Self.VarList.Count - 1 downto 0 do
    begin
      Result := Self.VarList.Ptr(I);
      if Result^.Name = Name then
        Exit(Result);
    end;
    Exit(nil);
  end;

  function PeekAtNextToken: TSEToken; inline;
  var
    P: Integer;
  begin
    P := Pos + 1;
    if P >= Self.TokenList.Count then
      P := P - 1;
    Exit(Self.TokenList[P]);
  end;

  function PeekAtNextNextToken: TSEToken; inline;
  var
    P: Integer;
  begin
    P := Pos + 2;
    if P >= Self.TokenList.Count then
      P := P - 2;
    Exit(Self.TokenList[P]);
  end;

  function NextToken: TSEToken; inline;
  begin
    Pos := Pos + 1;
    if Pos >= Self.TokenList.Count then
      Pos := Pos - 1;
    Result := Self.TokenList[Pos];
    if Self.LineOfCodeList.Count + 1 < Result.Ln then
      Self.LineOfCodeList.Add({Self.VM.Binary.Count}0);
  end;

  function TokenTypeString(const Kinds: TSETokenKinds): String; inline;
  var
    Kind: TSETokenKind;
  begin
    Result := '';
    for Kind in Kinds do
      Result := Result + '"' + TokenNames[Kind] + '", ';
  end;

  function NextTokenExpected(const Expected: TSETokenKinds): TSEToken; inline;
  var
    Kind: TSETokenKind;
  begin
    Result := NextToken;
    for Kind in Expected do
      if Kind = Result.Kind then
        Exit;
    Error(Format('Expected %s but got %s', [TokenTypeString(Expected), TokenNames[Result.Kind]]), Result);
  end;

  function PeekAtNextTokenExpected(const Expected: TSETokenKinds): TSEToken; inline;
  var
    Kind: TSETokenKind;
  begin
    Result := PeekAtNextToken;
    for Kind in Expected do
      if Kind = Result.Kind then
        Exit;
    Error(Format('Expected %s but got "%s"', [TokenTypeString(Expected), TokenNames[Result.Kind]]), Result);
  end;

  function CreateIdent(const Kind: TSEIdentKind; const Token: TSEToken; const IsUsed: Boolean = False): TSEIdent; inline;
  begin
    Result.Kind := Kind;
    Result.Ln := Token.Ln;
    Result.Col := Token.Col;
    Result.Name := Token.Value;
    Result.Local := Self.FuncTraversal;
    Result.IsUsed := IsUsed;
    if Result.Local > 0 then
    begin
      Result.Addr := Self.LocalVarCountList.Last;
      Self.LocalVarCountList[Self.LocalVarCountList.Count - 1] := Self.LocalVarCountList.Last + 1;
    end else
    begin
      Result.Addr := Self.GlobalVarCount;
      Inc(Self.GlobalVarCount);
    end;
    Self.VarList.Add(Result);
  end;

  procedure Rewind(const StartAddr, Count: Integer); inline;
  var
    Addr, I, J: Integer;
    FuncScript: PSEFuncScriptInfo;
  begin
    for I := 0 to Count - 1 do
    begin
      Addr := StartAddr + I;
      Self.Binary.Add(Self.Binary[Addr]);
    end;
    Self.Binary.DeleteRange(StartAddr, Count);
  end;

  function Emit(const Data: array of TSEValue): Integer; inline;
  var
    I: Integer;
    OpcodeInfo: TSEOpcodeInfo;
  begin
    OpcodeInfo.Op := TSEOpcode(Integer(Data[0].VarPointer));
    OpcodeInfo.Pos := Self.Binary.Count;
    OpcodeInfo.Size := Length(Data);
    OpcodeInfo.Binary := Self.Binary;
    Self.OpcodeInfoList.Add(OpcodeInfo);
    for I := Low(Data) to High(Data) do
    begin
      Self.Binary.Add(Data[I]);
    end;
    Exit(Self.Binary.Count);
  end;

  function EmitPushVar(const Ident: TSEIdent): Integer; inline;
  begin
    if Ident.Local > 0 then
      Result := Emit([Pointer(opPushLocalVar), Pointer(Ident.Addr), Pointer(Self.FuncTraversal - Ident.Local)])
    else
      Result := Emit([Pointer(opPushGlobalVar), Pointer(Ident.Addr)]);
  end;

  function EmitAssignVar(const Ident: TSEIdent): Integer; inline;
  begin
    if Ident.Local > 0 then
      Result := Emit([Pointer(opAssignLocalVar), Pointer(Ident.Addr), Pointer(Self.FuncTraversal - Ident.Local)])
    else
      Result := Emit([Pointer(opAssignGlobalVar), Pointer(Ident.Addr)]);
  end;

  function EmitAssignArray(const Ident: TSEIdent; const ArgCount: Integer): Integer; inline;
  begin
    if Ident.Local > 0 then
      Result := Emit([Pointer(opAssignLocalArray), Ident.Addr, ArgCount, Pointer(Self.FuncTraversal - Ident.Local)])
    else
      Result := Emit([Pointer(opAssignGlobalArray), Ident.Addr, ArgCount]);
  end;

  procedure Patch(const Addr: Integer; const Data: TSEValue); inline;
  begin
    Self.Binary[Addr] := Data;
  end;

  function PatchRange(const Addr: Integer; const Data: array of TSEValue): Integer; inline;
  var
    I: Integer;
  begin
    for I := Low(Data) to High(Data) do
    begin
      Self.Binary[Addr + I] := Data[I];
    end;
    Exit(Addr + I + 1);
  end;

  function IdentifyIdent(const Ident: String): TSETokenKind; inline;
  begin
    if FindVar(Ident) <> nil then
      Exit(tkVariable);
    if FindFunc(Ident) <> nil then
      Exit(tkFunction);
    if Self.ConstMap.ContainsKey(Ident) then
      Exit(tkConst);
    Exit(tkUnknown);
  end;

  procedure ParseFuncCall(const Name: String); forward;
  procedure ParseFuncRefCallByRewind(const RewindStartAdd: Integer; const ThisRefIdent: PSEIdent = nil); forward;
  procedure ParseFuncRefCallByName(const Name: String); forward;
  procedure ParseBlock(const IsCase: Boolean = False); forward;
  procedure ParseArrayAssign; forward;
  procedure ParseFuncAnonDecl; forward;

  procedure ParseExpr;
  type
    TProc = TSENestedProc;
  var
    PushConstCount: Integer = 0;
    IsTailed: Boolean = False;

    procedure Logic; forward;

    procedure EmitExpr(const Data: array of TSEValue); inline;
    var
      Op: TSEOpcode;
      V1, V2, V: TSEValue;
      OpInfoPrev1,
      OpInfoPrev2: PSEOpcodeInfo;

      function PeekAtPrevOp(const Ind: Integer): PSEOpcodeInfo; inline;
      var
        I: Integer;
      begin
        I := Self.OpcodeInfoList.Count - 1 - Ind;
        if I >= 0 then
          Result := Self.OpcodeInfoList.Ptr(I)
        else
          Result := nil;
      end;

      function PeekAtPrevOpExpected(const Ind: Integer; const Expected: TSEOpcodes): PSEOpcodeInfo; inline;
      var
        Op: TSEOpcode;
      begin
        Result := PeekAtPrevOp(Ind);
        if Result <> nil then
          for Op in Expected do
            if Op = Result^.Op then
              Exit;
        Result := nil;
      end;

      function OpToOp2(const Op: TSEOpcode): TSEOpcode; inline;
      begin
        case Op of
          opOperatorAdd:
            Result := opOperatorAdd2;
          opOperatorSub:
            Result := opOperatorSub2;
          opOperatorMul:
            Result := opOperatorMul2;
          opOperatorDiv:
            Result := opOperatorDiv2;
        end;
      end;

      function PeepholeOptimization: Boolean; inline;
      var
        A, B: TSEValue;
        I: Integer;
        P: Pointer;
      begin
        Result := False;
        case Op of
          opOperatorAdd,
          opOperatorSub,
          opOperatorMul,
          opOperatorDiv:
            begin
              OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushGlobalVar]);
              OpInfoPrev2 := PeekAtPrevOpExpected(1, [opPushGlobalVar]);
              if (OpInfoPrev1 <> nil) and (OpInfoPrev2 <> nil) then
              begin
                if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) or (OpInfoPrev2^.Binary <> Pointer(Self.Binary)) then
                  Exit;
                B := Self.Binary[OpInfoPrev1^.Pos + 1];
                A := Self.Binary[OpInfoPrev2^.Pos + 1];
                Op := OpToOp2(Op);
                Self.Binary.DeleteRange(Self.Binary.Count - 4, 4);
                Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 2, 2);
                Emit([Pointer(Integer(Op)), A.VarPointer, B.VarPointer, Pointer($FFFFFFFF)]);
                Result := True;
                PushConstCount := 0;
              end else
              begin
                OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushLocalVar]);
                OpInfoPrev2 := PeekAtPrevOpExpected(1, [opPushLocalVar]);
                if (OpInfoPrev1 <> nil) and (OpInfoPrev2 <> nil) then
                begin
                  if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) or (OpInfoPrev2^.Binary <> Pointer(Self.Binary)) then
                    Exit;
                  if Self.Binary[OpInfoPrev1^.Pos + 2].VarPointer <> Self.Binary[OpInfoPrev2^.Pos + 2].VarPointer then
                    Exit;
                  P := Self.Binary[OpInfoPrev1^.Pos + 2].VarPointer;
                  B := Self.Binary[OpInfoPrev1^.Pos + 1];
                  A := Self.Binary[OpInfoPrev2^.Pos + 1];
                  Op := OpToOp2(Op);
                  Self.Binary.DeleteRange(Self.Binary.Count - 6, 6);
                  Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 2, 2);
                  Emit([Pointer(Integer(Op)), A.VarPointer, B.VarPointer, Pointer(P)]);
                  Result := True;
                  PushConstCount := 0;
                end;
              end;
            end;
        end;
      end;

      function ConstantFoldingOptimization: Boolean; inline;
        function SameKind: Boolean; inline;
        begin
          V2 := Self.Binary[Self.Binary.Count - 1];
          V1 := Self.Binary[Self.Binary.Count - 3];
          Result := V1.Kind = V2.Kind;
        end;

        procedure Pop2; inline;
        begin
          Self.Binary.DeleteRange(Self.Binary.Count - 4, 4);
          Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 2, 2);
          Dec(PushConstCount);
        end;
      begin
        Result := False;
        if (PushConstCount < 2) or (IsTailed) then Exit;
        OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushConst]);
        OpInfoPrev2 := PeekAtPrevOpExpected(1, [opPushConst]);
        if (OpInfoPrev1 <> nil) and (OpInfoPrev1 <> nil) and SameKind then
        begin
          Result := True;
          case Op of
            opOperatorAdd:
              begin
                Pop2;
                SEValueAdd(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorSub:
              begin
                Pop2;
                SEValueSub(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorMul:
              begin
                Pop2;
                SEValueMul(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorDiv:
              begin
                Pop2;
                SEValueDiv(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorMod:
              begin
                Pop2;
                Emit([Pointer(opPushConst), V1 - V2 * Int(TSENumber(V1 / V2))]);
              end;
            opOperatorAnd:
              begin
                Pop2;
                Emit([Pointer(opPushConst), Integer(V1) and Integer(V2)]);
              end;
            opOperatorOr:
              begin
                Pop2;
                Emit([Pointer(opPushConst), Integer(V1) or Integer(V2)]);
              end;
            opOperatorXor:
              begin
                Pop2;
                Emit([Pointer(opPushConst), Integer(V1) xor Integer(V2)]);
              end;
            opOperatorGreater:
              begin
                Pop2;
                SEValueGreater(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorGreaterOrEqual:
              begin
                Pop2;
                SEValueGreaterOrEqual(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorLesser:
              begin
                Pop2;
                SEValueLesser(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorLesserOrEqual:
              begin
                Pop2;
                SEValueLesserOrEqual(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorEqual:
              begin
                Pop2;
                SEValueEqual(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorNotEqual:
              begin
                Pop2;
                SEValueNotEqual(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            else
              begin
                PushConstCount := 0;
                Result := False;
              end;
          end;
        end;
      end;

    begin
      Op := TSEOpcode(Integer(Data[0].VarPointer));
      if Op = opPushConst then
      begin
        Emit(Data);
        Inc(PushConstCount)
      end else
      if PeepholeOptimization then
      else
      if ConstantFoldingOptimization then
      else
        Emit(Data);
    end;

    procedure BinaryOp(const Op: TSEOpcode; const Func: TProc; const IsString: Boolean = False); inline;
    begin
      NextToken;
      if IsString then
        PeekAtNextTokenExpected([tkBracketOpen, tkSquareBracketOpen, tkDot, tkNumber, tkString, tkNegative, tkIdent])
      else
        PeekAtNextTokenExpected([tkBracketOpen, tkSquareBracketOpen, tkDot, tkNumber, tkNegative, tkNot, tkIdent]);
      Func;
      EmitExpr([Pointer({$ifdef CPU64}Int64(Op){$else}Op{$endif})]);
    end;

    procedure Tail;
    var
      Token: TSEToken;
    begin
      case PeekAtNextToken.Kind of
        tkSquareBracketOpen:
          begin
            PushConstCount := 0;
            IsTailed := True;
            NextToken;
            ParseExpr;
            NextTokenExpected([tkSquareBracketClose]);
            EmitExpr([Pointer(opPushArrayPop)]);
            Tail;
          end;
        tkDot:
          begin
            PushConstCount := 0;
            IsTailed := True;
            NextToken;
            Token := NextTokenExpected([tkIdent]);
            EmitExpr([Pointer(opPushConst), Token.Value]);
            EmitExpr([Pointer(opPushArrayPop)]);
            Tail;
          end;
      end;
    end;

    procedure Factor;
    var
      Token, Token2, FuncRefToken: TSEToken;
      Ident: PSEIdent;
      FuncValue: TSEValue;
      Ind: Integer;
      P: Pointer;
      RewindStartAddr: Integer;
      FuncRefIdent: TSEIdent;

      procedure FuncTail;
      var
        IsFirst: Boolean = True;
      begin
        while PeekAtNextToken.Kind = tkBracketOpen do
        begin
          if FuncRefToken.Value <> '' then
            ParseFuncRefCallByRewind(RewindStartAddr, @FuncRefIdent)
          else
            ParseFuncRefCallByRewind(RewindStartAddr, Ident);
          IsFirst := True;
          while PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] do
          begin
            if IsFirst then
            begin
              if FuncRefToken.Value = '' then
              begin
                FuncRefToken.Value := '___f' + Self.InternalIdent;
                FuncRefToken.Kind := tkIdent;
                FuncRefIdent := CreateIdent(ikVariable, FuncRefToken, True);
              end;
              IsFirst := False;
              EmitAssignVar(FuncRefIdent);
              RewindStartAddr := Self.Binary.Count;
            end;
            EmitPushVar(FuncRefIdent);
            Tail;
          end;
        end;
      end;

    begin
      Token := PeekAtNextTokenExpected([
        tkBracketOpen, tkBracketClose, tkSquareBracketOpen, tkDot, tkNumber, tkEOF,
        tkNegative, tkNot, tkString, tkIdent, tkFunctionDecl]);
      case Token.Kind of
        tkBracketOpen:
          begin
            NextToken;
            PeekAtNextTokenExpected([tkNegative, tkNot, tkBracketOpen, tkNumber, tkIdent]);
            Logic();
            NextTokenExpected([tkBracketClose]);
          end;
        tkSquareBracketOpen:
          begin
            NextToken;
            ParseArrayAssign;
          end;
        tkNumber:
          begin
            NextToken;
            EmitExpr([Pointer(opPushConst), PointStrToFloat(Token.Value)]);
          end;
        tkString:
          begin
            NextToken;
            EmitExpr([Pointer(opPushConst), Token.Value]);
          end;
        tkFunctionDecl:
          begin
            PushConstCount := 0;
            IsTailed := True;
            NextToken;
            ParseFuncAnonDecl;
          end;
        tkIdent:
          begin
            case IdentifyIdent(Token.Value) of
              tkVariable:
                begin
                  NextToken;
                  if PeekAtNextToken.Kind = tkBracketOpen then // Likely function ref
                  begin
                    ParseFuncRefCallByName(Token.Value);
                  end else
                  begin
                    Ident := FindVar(Token.Value);
                    Ident^.IsUsed := True;
                    RewindStartAddr := Self.Binary.Count;
                    case PeekAtNextToken.Kind of
                      tkSquareBracketOpen:
                        begin
                          PushConstCount := 0;
                          IsTailed := True;
                          NextToken;
                          EmitPushVar(Ident^);
                          ParseExpr;
                          Emit([Pointer(opPushArrayPop)]);
                          NextTokenExpected([tkSquareBracketClose]);
                          Tail;
                          FuncTail;
                        end;
                      tkDot:
                        begin
                          PushConstCount := 0;
                          IsTailed := True;
                          NextToken;
                          Token2 := NextTokenExpected([tkIdent]);
                          EmitPushVar(Ident^);
                          EmitExpr([Pointer(opPushConst), Token2.Value]);
                          Emit([Pointer(opPushArrayPop)]);
                          Tail;
                          FuncTail;
                        end;
                      else
                        EmitPushVar(Ident^);
                    end;
                  end;
                end;
              tkConst:
                begin
                  NextToken;
                  EmitExpr([Pointer(opPushConst), Self.ConstMap[Token.Value]]);
                end;
              tkFunction:
                begin
                  NextToken;
                  if PeekAtNextToken.Kind <> tkBracketOpen then // Likely function ref
                  begin
                    P := FindFunc(Token.Value, FuncValue.VarFuncKind, Ind);
                    if P = nil then
                      Error(Format('Function "%s" not found', [Token.Value]), Token);
                    case FuncValue.VarFuncKind of
                      sefkScript, sefkImport:
                        FuncValue.VarFuncIndx := Ind;
                      sefkNative:
                        FuncValue.VarFuncIndx := QWord(P);
                    end;
                    FuncValue.Kind := sevkFunction;
                    PushConstCount := 0;
                    EmitExpr([Pointer(opPushConst), FuncValue]);
                  end else
                  begin
                    ParseFuncCall(Token.Value);
                  end;
                  if PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] then
                  begin
                    FuncRefToken.Value := '___f' + Self.InternalIdent;
                    FuncRefToken.Kind := tkIdent;
                    FuncRefIdent := CreateIdent(ikVariable, FuncRefToken, True);
                    EmitAssignVar(FuncRefIdent);
                    RewindStartAddr := Self.Binary.Count;
                    EmitPushVar(FuncRefIdent);
                    Tail;
                    FuncTail;
                  end;
                end;
              else
                Error(Format('Unknown identify "%s"', [Token.Value]), Token);
            end;
          end;
      end;
    end;

    procedure SignedFactor;
    var
      Token: TSEToken;
    begin
      Factor;
      while True do
      begin
        Token := PeekAtNextToken;
        case Token.Kind of
          tkNegative:
            begin
              NextToken;
              PeekAtNextTokenExpected([tkBracketOpen, tkNumber, tkIdent]);
              Factor;
              EmitExpr([Pointer(opOperatorNegative)]);
            end;
          tkNot:
            begin
              NextToken;
              PeekAtNextTokenExpected([tkBracketOpen, tkNumber, tkIdent]);
              Factor;
              EmitExpr([Pointer(opOperatorNot)]);
            end;
          else
            Exit;
        end;
      end;
    end;

    procedure Pow;
    var
      Token: TSEToken;
    begin
      SignedFactor;
      while True do
      begin
        Token := PeekAtNextToken;
        case Token.Kind of
          tkPow:
            BinaryOp(opOperatorPow, @SignedFactor, True);
          else
            Exit;
        end;
      end;
    end;

    procedure Term;
    var
      Token: TSEToken;
    begin
      Pow;
      while True do
      begin
        Token := PeekAtNextToken;
        case Token.Kind of
          tkMul:
            BinaryOp(opOperatorMul, @Pow, True);
          tkDiv:
            BinaryOp(opOperatorDiv, @Pow, True);
          tkMod:
            BinaryOp(opOperatorMod, @Pow, True);
          else
            Exit;
        end;
      end;
    end;

    procedure Expr;
    var
      Token: TSEToken;
    begin
      Term;
      while True do
      begin
        Token := PeekAtNextToken;
        case Token.Kind of
          tkAdd:
            BinaryOp(opOperatorAdd, @Term, True);
          tkSub:
            BinaryOp(opOperatorSub, @Term, True);
          else
            Exit;
        end;
      end;
    end;

    procedure Logic;
    var
      Token: TSEToken;
    begin
      Expr;
      while True do
      begin
        Token := PeekAtNextToken;
        case Token.Kind of
          tkEqual:
            BinaryOp(opOperatorEqual, @Expr, True);
          tkNotEqual:
            BinaryOp(opOperatorNotEqual, @Expr, True);
          tkGreater:
            BinaryOp(opOperatorGreater, @Expr, True);
          tkGreaterOrEqual:
            BinaryOp(opOperatorGreaterOrEqual, @Expr, True);
          tkSmaller:
            BinaryOp(opOperatorLesser, @Expr, True);
          tkSmallerOrEqual:
            BinaryOp(opOperatorLesserOrEqual, @Expr, True);
          tkAnd:
            BinaryOp(opOperatorAnd, @Expr, True);
          tkOr:
            BinaryOp(opOperatorOr, @Expr, True);
          tkXor:
            BinaryOp(opOperatorXor, @Expr, True);
          else
            Exit;
        end;
      end;
    end;
  begin
    Logic;
  end;

  procedure ParseFuncRefCallByMapRewind(const Ident: TSEIdent; const DeepCount, RewindStartAdd: Integer; const ThisRefIdent: PSEIdent = nil);
  var
    Token: TSEToken;
    ArgCount: Integer = 1;
    RewindCount: Integer;
    ThisAddr: Integer;
  begin
    RewindCount := Self.Binary.Count - RewindStartAdd;
    NextTokenExpected([tkBracketOpen]);
    // Allocate stack for result
    Emit([Pointer(opPushConst), SENull]);
    Token := PeekAtNextToken;
    if Token.Kind = tkBracketClose then
      NextToken;
    while not (Token.Kind = tkBracketClose) do
    begin
      ParseExpr;
      Inc(ArgCount);
      Token := NextTokenExpected([tkComma, tkBracketClose]);
    end;
    // Allocate stack for this
    if ThisRefIdent <> nil then
      EmitPushVar(ThisRefIdent^)
    else
      Emit([Pointer(opPushConst), SENull]);
    // Push map to stack
    Rewind(RewindStartAdd, RewindCount);
    EmitPushVar(Ident);
    Emit([Pointer(opCallRef), Pointer(DeepCount), Pointer(ArgCount)]);
  end;

  procedure ParseFuncRefCallByRewind(const RewindStartAdd: Integer; const ThisRefIdent: PSEIdent = nil);
  var
    Token: TSEToken;
    ArgCount: Integer = 1;
    RewindCount: Integer;
  begin
    RewindCount := Self.Binary.Count - RewindStartAdd;
    NextTokenExpected([tkBracketOpen]);
    // Allocate stack for result
    Emit([Pointer(opPushConst), SENull]);
    Token := PeekAtNextToken;
    if Token.Kind = tkBracketClose then
      NextToken;
    while not (Token.Kind = tkBracketClose) do
    begin
      ParseExpr;
      Inc(ArgCount);
      Token := NextTokenExpected([tkComma, tkBracketClose]);
    end;
    // Allocate stack for this
    if ThisRefIdent <> nil then
      EmitPushVar(ThisRefIdent^)
    else
      Emit([Pointer(opPushConst), SENull]);
    // Func def already exists in stack, rewind to access it
    Rewind(RewindStartAdd, RewindCount);
    Emit([Pointer(opCallRef), Pointer(0), Pointer(ArgCount)]);
  end;

  procedure ParseFuncRefCallByName(const Name: String);
  var
    Token: TSEToken;
    ArgCount: Integer = 1;
  begin
    NextTokenExpected([tkBracketOpen]);
    // Allocate stack for result
    Emit([Pointer(opPushConst), SENull]);
    Token := PeekAtNextToken;
    if Token.Kind = tkBracketClose then
      NextToken;
    while not (Token.Kind = tkBracketClose) do
    begin
      ParseExpr;
      Inc(ArgCount);
      Token := NextTokenExpected([tkComma, tkBracketClose]);
    end;
    // Allocate stack for this
    Emit([Pointer(opPushConst), SENull]);
    // We now push func def to stack
    EmitPushVar(FindVar(Name)^);
    Emit([Pointer(opCallRef), Pointer(0), Pointer(ArgCount)]);
  end;

  procedure ParseFuncCall(const Name: String);
  var
    FuncNativeInfo: PSEFuncNativeInfo = nil;
    FuncScriptInfo: PSEFuncScriptInfo = nil;
    FuncImportInfo: PSEFuncImportInfo = nil;
    I, Ind: Integer;
    DefinedArgCount: Integer;
    ArgCount: Integer = 0;
    Token: TSEToken;
  begin
    FuncNativeInfo := FindFuncNative(Name, Ind);
    if FuncNativeInfo <> nil then
      DefinedArgCount := FuncNativeInfo^.ArgCount
    else
    begin
      FuncScriptInfo := FindFuncScript(Name, Ind);
      if FuncScriptInfo <> nil then
        DefinedArgCount := FuncScriptInfo^.ArgCount
      else
      begin
        FuncImportInfo := FindFuncImport(Name, Ind);
        if FuncImportInfo <> nil then
          DefinedArgCount := Length(FuncImportInfo^.Args);
      end;
    end;
    if FuncScriptInfo <> nil then // Allocate stack for result
      Emit([Pointer(opPushConst), SENull]);
    if DefinedArgCount > 0 then
    begin
      NextTokenExpected([tkBracketOpen]);
      for I := 0 to DefinedArgCount - 1 do
      begin
        ParseExpr;
        if I < DefinedArgCount - 1 then
          NextTokenExpected([tkComma]);
        Inc(ArgCount);
      end;
      NextTokenExpected([tkBracketClose]);
    end else
    if DefinedArgCount < 0 then
    begin
      NextTokenExpected([tkBracketOpen]);
      repeat
        ParseExpr;
        Token := NextTokenExpected([tkComma, tkBracketClose]);
        Inc(ArgCount);
      until Token.Kind = tkBracketClose;
    end else
    begin
      NextTokenExpected([tkBracketOpen]);
      NextTokenExpected([tkBracketClose]);
    end;
    ArgCount := Max(0, ArgCount);
    if FuncNativeInfo <> nil then
      Emit([Pointer(opCallNative), Pointer(FuncNativeInfo), Pointer(ArgCount)])
    else
    if FuncScriptInfo <> nil then
    begin
      Emit([Pointer(opPushConst), SENull]); // this
      Inc(ArgCount);
      Emit([Pointer(opCallScript), Pointer(Ind), Pointer(ArgCount)])
    end
    else
      Emit([Pointer(opCallImport), Pointer(Ind), Pointer(0)]);
  end;

  function ParseFuncDecl(const IsAnon: Boolean = False): TSEToken;
  var
    Token: TSEToken;
    Name: String;
    ArgCount: Integer = 0;
    I, FuncIndex: Integer;
    ReturnList: TList;
    Func: PSEFuncScriptInfo;
    ParentBinary: TSEBinary;
  begin
    ReturnList := TList.Create;
    try
      ReturnStack.Push(ReturnList);
      if not IsAnon then
      begin
        Token := NextTokenExpected([tkIdent]);
        Name := Token.Value;
        if (FuncTraversal = 0) and (FindFunc(Name) <> nil) then
          Error(Format('Duplicate function declaration "%s"', [Token.Value]), Token);
      end else
      begin
        Token.Kind := tkIdent;
        Token.Value := '___fn' + Self.InternalIdent;
        Name := Token.Value;
      end;
      Result := Token;

      Token.Value := 'result';
      Token.Kind := tkIdent;
      CreateIdent(ikVariable, Token, True);

      NextTokenExpected([tkBracketOpen]);
      repeat
        if PeekAtNextToken.Kind = tkIdent then
        begin
          Token := NextTokenExpected([tkIdent]);
          CreateIdent(ikVariable, Token);
          Inc(ArgCount);
        end;
        Token := NextTokenExpected([tkComma, tkBracketClose]);
      until Token.Kind = tkBracketClose;

      Token.Value := 'self';
      Token.Kind := tkIdent;
      CreateIdent(ikVariable, Token, True);

      Func := RegisterScriptFunc(Name, ArgCount);
      FuncIndex := Self.FuncScriptList.Count - 1;
      ParentBinary := Self.Binary;
      Self.Binary := Self.VM.Binaries[Func^.BinaryPos];
      ParseBlock;

      ReturnList := ReturnStack.Pop;
      for I := 0 to ReturnList.Count - 1 do
        Patch(Integer(ReturnList[I]), Pointer(Self.Binary.Count));
      Emit([Pointer(opPopFrame)]);

      // The pointer may be changed due to reallocation, need to query for it again
      Func := Self.FuncScriptList.Ptr(FuncIndex);
      Func^.VarCount := Self.LocalVarCountList[Self.LocalVarCountList.Count - 1] - ArgCount;
      Self.Binary := ParentBinary;
    finally
      ReturnList.Free;
    end;
  end;

  procedure ParseFuncAnonDecl;
  var
    I, J: Integer;
    FuncValue: TSEValue;
    Ind: Integer;
    P: Pointer;
  begin
    Inc(FuncTraversal);
    Self.LocalVarCountList.Add(-1);
    Self.ScopeStack.Push(Self.VarList.Count);
    Self.ScopeFunc.Push(Self.FuncScriptList.Count + 1);
    Token := ParseFuncDecl(True);
    I := Self.ScopeStack.Pop;
    Self.VarList.DeleteRange(I, Self.VarList.Count - I);
    I := Self.ScopeFunc.Pop;
    for J := I to Self.FuncScriptList.Count - 1 do
    begin
      if Self.FuncScriptList.Ptr(J)^.Name.IndexOf('___fn') <> 0 then
        Self.FuncScriptList.Ptr(J)^.Name := '';
    end;
    Self.LocalVarCountList.Delete(Self.LocalVarCountList.Count - 1);
    Dec(FuncTraversal);
    //
    P := FindFunc(Token.Value, FuncValue.VarFuncKind, Ind);
    if P = nil then
      Error(Format('Function "%s" not found', [Token.Value]), Token);
    case FuncValue.VarFuncKind of
      sefkScript, sefkImport:
        FuncValue.VarFuncIndx := Ind;
      sefkNative:
        FuncValue.VarFuncIndx := QWord(P);
    end;
    FuncValue.Kind := sevkFunction;
    Emit([Pointer(opPushConst), FuncValue]);
  end;

  procedure ParseFuncImport;

    function GetAtom(const Token: TSEToken; const IsVoidForbid: Boolean = False): TSEAtomKind;
    begin
      case Token.Value of
        'void':
          begin
            if IsVoidForbid then
              Error('"void" type it not allowed as parameter', Token);
            Result := seakVoid;
          end;
        'u8':
          Result := seakU8;
        'u16':
          Result := seakU16;
        'u32':
          Result := seakU32;
        'u64':
          Result := seakU64;
        'i8':
          Result := seakI8;
        'i16':
          Result := seakI16;
        'i32':
          Result := seakI32;
        'i64':
          Result := seakI64;
        {'f32':
          Result := seakF32;}
        'f64':
          Result := seakF64;
        'buffer':
          Result := seakBuffer;
        'wbuffer':
          Result := seakWBuffer;
      end;
    end;

    procedure FuncImport(const Lib: String);
    var
      Token: TSEToken;
      Name, ActualName: String;
      Return: TSEAtomKind;
      Args: TSEAtomKindArray;
      I: Integer;
    begin
      NextTokenExpected([tkFunctionDecl]);
      Token := NextTokenExpected([tkIdent]);
      Name := Token.Value;

      if FindFunc(Name) <> nil then
        Error(Format('Duplicate function declaration "%s"', [Token.Value]), Token);

      NextTokenExpected([tkBracketOpen]);
      repeat
        if PeekAtNextToken.Kind = tkAtom then
        begin
          Token := NextTokenExpected([tkAtom]);
          SetLength(Args, Length(Args) + 1);
          Args[Length(Args) - 1] := GetAtom(Token, True);
        end;
        Token := NextTokenExpected([tkComma, tkBracketClose]);
      until Token.Kind = tkBracketClose;
      NextTokenExpected([tkColon]);
      Token := NextTokenExpected([tkAtom]);
      Return := GetAtom(Token);
      if PeekAtNextToken.Kind = tkString then
      begin
        Token := NextToken;
        ActualName := Token.Value;
      end else
        ActualName := Name;

      Self.RegisterImportFunc(Name, ActualName, Lib, Args, Return);
    end;

  var
    Token: TSEToken;
    Lib: String;
  begin
    Token := NextTokenExpected([tkString]);
    Lib := Token.Value;
    if PeekAtNextToken.Kind <> tkBegin then
      FuncImport(Lib)
    else
    begin
      NextToken;
      while True do
      begin
        FuncImport(Lib);
        if PeekAtNextTokenExpected([tkEnd, tkFunctionDecl]).Kind = tkEnd then
        begin
          NextToken;
          break;
        end;
      end;
    end;
  end;

  procedure ParseWhile;
  var
    StartBlock,
    EndBlock,
    JumpBlock,
    JumpEnd: Integer;
    BreakList,
    ContinueList: TList;
    I: Integer;
  begin
    ContinueList := TList.Create;
    BreakList := TList.Create;
    try
      ContinueStack.Push(ContinueList);
      BreakStack.Push(BreakList);
      StartBlock := Self.Binary.Count;
      ParseExpr;
      Emit([Pointer(opPushConst), False]);
      JumpEnd := Emit([Pointer(opJumpEqual), Pointer(0)]);
      ParseBlock;
      JumpBlock := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
      EndBlock := Self.Binary.Count;
      ContinueList := ContinueStack.Pop;
      BreakList := BreakStack.Pop;
      for I := 0 to ContinueList.Count - 1 do
        Patch(Integer(ContinueList[I]), Pointer(StartBlock));
      for I := 0 to BreakList.Count - 1 do
        Patch(Integer(BreakList[I]), Pointer(EndBlock));
      Patch(JumpBlock - 1, Pointer(StartBlock));
      Patch(JumpEnd - 1, Pointer(EndBlock));
    finally
      ContinueList.Free;
      BreakList.Free;
    end;
  end;

  procedure ParseDoWhile;
  var
    StartBlock,
    EndBlock,
    JumpBlock,
    JumpEnd: Integer;
    BreakList,
    ContinueList: TList;
    I: Integer;
  begin
    ContinueList := TList.Create;
    BreakList := TList.Create;
    try
      ContinueStack.Push(ContinueList);
      BreakStack.Push(BreakList);
      StartBlock := Self.Binary.Count;
      ParseBlock;
      NextTokenExpected([tkWhile]);
      ParseExpr;
      Emit([Pointer(opPushConst), False]);
      JumpEnd := Emit([Pointer(opJumpEqual), Pointer(0)]);
      JumpBlock := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
      EndBlock := Self.Binary.Count;
      ContinueList := ContinueStack.Pop;
      BreakList := BreakStack.Pop;
      for I := 0 to ContinueList.Count - 1 do
        Patch(Integer(ContinueList[I]), Pointer(StartBlock));
      for I := 0 to BreakList.Count - 1 do
        Patch(Integer(BreakList[I]), Pointer(EndBlock));
      Patch(JumpBlock - 1, Pointer(StartBlock));
      Patch(JumpEnd - 1, Pointer(EndBlock));
    finally
      ContinueList.Free;
      BreakList.Free;
    end;
  end;

 procedure ParseFor;
  var
    StartBlock,
    EndBlock,
    JumpBlock,
    JumpEnd: Integer;
    BreakList,
    ContinueList: TList;
    I: Integer;
    Token: TSEToken;
    VarIdent,
    VarHiddenCountIdent,
    VarHiddenArrayIdent: TSEIdent;
    VarHiddenCountName,
    VarHiddenArrayName: String;
    Ind: Integer;
  begin
    ContinueList := TList.Create;
    BreakList := TList.Create;
    try
      ContinueStack.Push(ContinueList);
      BreakStack.Push(BreakList);

      Token := NextTokenExpected([tkVariable, tkIdent]);
      // FIXME: tkVariable?
      if Token.Kind = tkIdent then
      begin
        VarIdent := CreateIdent(ikVariable, Token, True);
      end else
      begin
        VarIdent := FindVar(Token.Value)^;
      end;
      Token := NextTokenExpected([tkEqual, tkIn, tkComma]);

      if Token.Kind = tkEqual then
      begin
        ParseExpr;
        EmitAssignVar(VarIdent);

        Token := NextTokenExpected([tkTo, tkDownto]);
        StartBlock := Self.Binary.Count;
        EmitPushVar(VarIdent);
        ParseExpr;
        if Token.Kind = tkTo then
        begin
          Emit([Pointer(opPushConst), 1]);
          Emit([Pointer(opOperatorAdd)]);
          JumpEnd := Emit([Pointer(opJumpEqualOrGreater), Pointer(0)]);
        end else
        if Token.Kind = tkDownto then
        begin
          Emit([Pointer(opPushConst), 1]);
          Emit([Pointer(opOperatorSub)]);
          JumpEnd := Emit([Pointer(opJumpEqualOrLesser), Pointer(0)]);
        end;

        ParseBlock;

        EmitPushVar(VarIdent);
        if Token.Kind = tkTo then
        begin
          Emit([Pointer(opPushConst), 1]);
          Emit([Pointer(opOperatorAdd)]);
        end else
        if Token.Kind = tkDownto then
        begin
          Emit([Pointer(opPushConst), 1]);
          Emit([Pointer(opOperatorSub)]);
        end;
        EmitAssignVar(VarIdent);
        JumpBlock := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
        EndBLock := JumpBlock;
      end else
      begin
        if Token.Kind = tkComma then
        begin
          Token := NextTokenExpected([tkIdent]);
          VarHiddenCountName := Token.Value;
          NextTokenExpected([tkIn]);
        end else
          VarHiddenCountName := '___c' + VarIdent.Name;
        VarHiddenArrayName := '___a' + VarIdent.Name;
        Token.Value := VarHiddenCountName;
        VarHiddenCountIdent := CreateIdent(ikVariable, Token, True);
        Token.Value := VarHiddenArrayName;
        VarHiddenArrayIdent := CreateIdent(ikVariable, Token, True);

        ParseExpr;

        EmitAssignVar(VarHiddenArrayIdent);
        Emit([Pointer(opPushConst), 0]);
        EmitAssignVar(VarHiddenCountIdent);

        StartBlock := Self.Binary.Count;

        EmitPushVar(VarHiddenArrayIdent);
        Emit([Pointer(opCallNative), FindFuncNative('length', Ind), Pointer(1)]);
        EmitPushVar(VarHiddenCountIdent);
        JumpEnd := Emit([Pointer(opJumpEqualOrLesser), Pointer(0)]);

        EmitPushVar(VarHiddenArrayIdent);
        EmitPushVar(VarHiddenCountIdent);
        Emit([Pointer(opPushArrayPop)]);
        EmitAssignVar(VarIdent);

        ParseBlock;

        EmitPushVar(VarHiddenCountIdent);
        Emit([Pointer(opPushConst), 1]);
        Emit([Pointer(opOperatorAdd)]);
        EmitAssignVar(VarHiddenCountIdent);
        JumpBlock := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
        EndBLock := JumpBlock;
      end;

      ContinueList := ContinueStack.Pop;
      BreakList := BreakStack.Pop;
      for I := 0 to ContinueList.Count - 1 do
        Patch(Integer(ContinueList[I]), Pointer(StartBlock));
      for I := 0 to BreakList.Count - 1 do
        Patch(Integer(BreakList[I]), Pointer(EndBlock));
      Patch(JumpBlock - 1, Pointer(StartBlock));
      Patch(JumpEnd - 1, Pointer(EndBlock));
    finally
      ContinueList.Free;
      BreakList.Free;
    end;
  end;

  procedure ParseIf;
  var
    StartBlock1,
    StartBlock2,
    EndBlock2,
    JumpBlock1,
    JumpBlock2,
    JumpEnd: Integer;
  begin
    ParseExpr;
    Emit([Pointer(opPushConst), True]);
    JumpBlock1 := Emit([Pointer(opJumpEqual), Pointer(0)]);
    JumpBlock2 := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
    StartBlock1 := Self.Binary.Count;
    ParseBlock;
    JumpEnd := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
    StartBlock2 := Self.Binary.Count;
    if PeekAtNextToken.Kind = tkElse then
    begin
      NextToken;
      ParseBlock;
    end;
    EndBlock2 := Self.Binary.Count;
    Patch(JumpBlock1 - 1, Pointer(StartBlock1));
    Patch(JumpBlock2 - 1, Pointer(StartBlock2));
    Patch(JumpEnd - 1, Pointer(EndBlock2));
  end;

  procedure ParseSwitch;
  var
    Token: TSEToken;
    VarHiddenIdent: TSEIdent;
    BreakList: TList;
    JumpBlock1,
    JumpBlock2,
    StartCaseBlock,
    EndCaseBlock,
    JumpNextBlock,
    EndBlock: Integer;
  begin
    Token.Kind := tkIdent;
    Token.Value := '___s' + Self.InternalIdent;
    VarHiddenIdent := CreateIdent(ikVariable, Token, True);

    ParseExpr;
    EmitAssignVar(VarHiddenIdent);

    NextTokenExpected([tkBegin]);
    BreakList := TList.Create;
    JumpNextBlock := -1;
    try
      BreakStack.Push(BreakList);

      while PeekAtNextToken.Kind in [tkCase, tkDefault] do
      begin
        Token := NextToken;
        if Token.Kind = tkCase then
        begin
          ParseExpr;
          EmitPushVar(VarHiddenIdent);
          JumpBlock1 := Emit([Pointer(opJumpEqual), Pointer(0)]);
          JumpBlock2 := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
        end;
        StartCaseBlock := Self.Binary.Count;
        if JumpNextBlock <> -1 then
        begin
          Patch(JumpNextBlock - 1, Pointer(StartCaseBlock));
          JumpNextBlock := -1;
        end;
        PeekAtNextTokenExpected([tkColon]);
        ParseBlock(True);
        if Token.Kind = tkCase then
        begin
          JumpNextBlock := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
          EndCaseBlock := Self.Binary.Count;
          Patch(JumpBlock1 - 1, Pointer(StartCaseBlock));
          Patch(JumpBlock2 - 1, Pointer(EndCaseBlock));
        end else
          Break;
      end;
      NextTokenExpected([tkEnd]);
      EndBlock := Self.Binary.Count;

      BreakList := BreakStack.Pop;

      for I := 0 to BreakList.Count - 1 do
        Patch(Integer(BreakList[I]), Pointer(EndBlock));
    finally
      BreakList.Free;
    end;
  end;

  procedure ParseArrayAssign;
  var
    FuncNativeInfo: PSEFuncNativeInfo;
    I, Ind: Integer;
    ArgCount: Integer = 0;
    Token: TSEToken;
  begin
    I := 0;
    FuncNativeInfo := FindFuncNative('___map_create', Ind);
    repeat
      if PeekAtNextToken.Kind <> tkSquareBracketClose then
      begin
        if ((PeekAtNextToken.Kind = tkIdent) or (PeekAtNextToken.Kind = tkString)) and (PeekAtNextNextToken.Kind = tkColon) then
        begin
          Token := NextToken;
          Emit([Pointer(opPushConst), Token.Value]);
          NextToken;
          ParseExpr;
          Inc(ArgCount, 2);
        end else
        begin
          Emit([Pointer(opPushConst), IntToStr(I)]);
          ParseExpr;
          Inc(ArgCount, 2);
          Inc(I);
        end;
      end;
      Token := NextTokenExpected([tkComma, tkSquareBracketClose]);
    until Token.Kind = tkSquareBracketClose;
    Emit([Pointer(opCallNative), Pointer(FuncNativeInfo), Pointer(ArgCount)]);
  end;

  procedure ParseAssignTail;
  var
    RewindStartAddr: Integer;
    Token, FuncRefToken: TSEToken;
    FuncRefIdent: TSEIdent;
  begin
    while PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] do
    begin
      if FuncRefToken.Value = '' then
      begin
        FuncRefToken.Value := '___f' + Self.InternalIdent;
        FuncRefToken.Kind := tkIdent;
        FuncRefIdent := CreateIdent(ikVariable, FuncRefToken, True);
      end;
      EmitAssignVar(FuncRefIdent);
      RewindStartAddr := Self.Binary.Count;
      EmitPushVar(FuncRefIdent);
      while PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] do
      begin
        case PeekAtNextToken.Kind of
          tkSquareBracketOpen:
            begin
              NextToken;
              ParseExpr;
              NextTokenExpected([tkSquareBracketClose]);
              Emit([Pointer(opPushArrayPop)]);
            end;
          tkDot:
            begin
              NextToken;
              Token := NextTokenExpected([tkIdent]);
              Emit([Pointer(opPushConst), Token.Value]);
              Emit([Pointer(opPushArrayPop)]);
            end;
        end;
      end;
      if PeekAtNextToken.Kind = tkBracketOpen then
      begin
        ParseFuncRefCallByRewind(RewindStartAddr, @FuncRefIdent);
      end;
    end;
    Emit([Pointer(opPopConst)]);
  end;

  procedure ParseVarAssign(const Name: String; const IsNew: Boolean = False);
  var
    Ident: PSEIdent;
    Token, Token2: TSEToken;
    ArgCount: Integer = 0;
    I, J,
    RewindStartAddr,
    VarStartTokenPos,
    VarEndTokenPos: Integer;
  begin
    Ident := FindVar(Name);
    RewindStartAddr := Self.Binary.Count;
    VarStartTokenPos := Pos;
    while PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] do
    begin
      if IsNew then
        Error(Format('Variable "%s" is not an array / a map', [Name]), PeekAtNextToken);
      case PeekAtNextToken.Kind of
        tkSquareBracketOpen:
          begin
            NextToken;
            ParseExpr;
            NextTokenExpected([tkSquareBracketClose]);
          end;
        tkDot:
          begin
            NextToken;
            Token2 := NextTokenExpected([tkIdent]);
            Emit([Pointer(opPushConst), Token2.Value]);
          end;
      end;
      Inc(ArgCount);
    end;

    Token := PeekAtNextTokenExpected([tkEqual, tkOpAssign, tkBracketOpen]);
    case Token.Kind of
      tkEqual,
      tkOpAssign:
        begin
          VarEndTokenPos := Pos;
          NextToken;
          if Token.Kind = tkOpAssign then
          begin
            if ArgCount > 0 then
            begin
              J := Pos + 1;
              for I := VarStartTokenPos to VarEndTokenPos do
              begin
                Self.TokenList.Insert(J, Self.TokenList[I]);
                Inc(J);
              end;
              ParseExpr;
            end else
              EmitPushVar(Ident^);
          end;
          ParseExpr;
          if Token.Kind = tkOpAssign then
          begin
            case Token.Value of
              '+':
                Emit([Pointer(opOperatorAdd)]);
              '-':
                Emit([Pointer(opOperatorSub)]);
              '*':
                Emit([Pointer(opOperatorMul)]);
              '/':
                Emit([Pointer(opOperatorDiv)]);
            end;
          end;
          if ArgCount > 0 then
            EmitAssignArray(Ident^, ArgCount)
          else
            EmitAssignVar(Ident^);
        end;
      tkBracketOpen:
        begin
          if IsNew then
            Error(Format('Variable "%s" is not a function', [Name]), PeekAtNextToken);
          ParseFuncRefCallByMapRewind(Ident^, ArgCount, RewindStartAddr, Ident);
          ParseAssignTail;
        end;
    end;
  end;

  procedure ParseBlock(const IsCase: Boolean = False);
  var
    Token: TSEToken;
    Ident: TSEIdent;
    List: TList;
    I, J, RewindStartAddr: Integer;
  begin
    Token := PeekAtNextToken;
    case Token.Kind of
      tkIf:
        begin
          NextToken;
          ParseIf;
        end;
      tkFor:
        begin
          NextToken;
          ParseFor;
        end;
      tkDo:
        begin
          NextToken;
          ParseDoWhile;
        end;
      tkWhile:
        begin
          NextToken;
          ParseWhile;
        end;
      tkSwitch:
        begin
          NextToken;
          ParseSwitch;
        end;
      tkBreak:
        begin
          NextToken;
          if BreakStack.Count = 0 then
            Error('Not in loop but "break" found', Token);
          List := BreakStack.Peek;
          List.Add(Pointer(Emit([Pointer(opJumpUnconditional), Pointer(0)]) - 1));
        end;
      tkContinue:
        begin
          NextToken;
          if ContinueStack.Count = 0 then
            Error('Not in loop but "continue" found', Token);
          List := ContinueStack.Peek;
          List.Add(Pointer(Emit([Pointer(opJumpUnconditional), Pointer(0)]) - 1));
        end;
      tkReturn:
        begin
          NextToken;
          if FuncTraversal = 0 then
            Emit([Pointer(opHlt)])
          else
          begin
            List := ReturnStack.Peek;
            List.Add(Pointer(Emit([Pointer(opJumpUnconditional), Pointer(0)]) - 1));
          end;
        end;
      tkFunctionDecl:
        begin
          NextToken;
          Inc(FuncTraversal);
          Self.LocalVarCountList.Add(-1);
          Self.ScopeStack.Push(Self.VarList.Count);
          Self.ScopeFunc.Push(Self.FuncScriptList.Count + 1);
          ParseFuncDecl;
          I := Self.ScopeStack.Pop;
          Self.VarList.DeleteRange(I, Self.VarList.Count - I);
          I := Self.ScopeFunc.Pop;
          for J := I to Self.FuncScriptList.Count - 1 do
          begin
            if Self.FuncScriptList.Ptr(J)^.Name.IndexOf('___fn') <> 0 then
              Self.FuncScriptList.Ptr(J)^.Name := '';
          end;
          Self.LocalVarCountList.Delete(Self.LocalVarCountList.Count - 1);
          Dec(FuncTraversal);
        end;
      tkYield:
        begin
          NextToken;
          Emit([Pointer(opYield)]);
        end;
      tkColon:
        begin
          if not IsCase then
            Error('Invalid statement ' + TokenNames[Token.Kind], Token);
          Self.ScopeStack.Push(Self.VarList.Count);
          NextToken;
          Token := PeekAtNextToken;
          while not (Token.Kind in [tkEnd, tkCase, tkDefault]) do
          begin
            if Token.Kind = tkEOF then
              Error('Expected end, got EOF instead', Token);
            ParseBlock;
            Token := PeekAtNextToken;
          end;
          I := Self.ScopeStack.Pop;
          Self.VarList.DeleteRange(I, Self.VarList.Count - I);
        end;
      tkBegin:
        begin
          Self.ScopeStack.Push(Self.VarList.Count);
          NextToken;
          Token := PeekAtNextToken;
          while Token.Kind <> tkEnd do
          begin
            if Token.Kind = tkEOF then
              Error('Expected end, got EOF instead', Token);
            ParseBlock;
            Token := PeekAtNextToken;
          end;
          I := Self.ScopeStack.Pop;
          Self.VarList.DeleteRange(I, Self.VarList.Count - I);
          NextToken;
        end;
      tkIdent:
        begin
          case IdentifyIdent(Token.Value) of
            tkUnknown:
              begin
                NextToken;
                CreateIdent(ikVariable, Token);
                ParseVarAssign(Token.Value, True);
              end;
            tkVariable:
              begin
                NextToken;
                if PeekAtNextToken.Kind = tkBracketOpen then // Likely function ref
                begin
                  ParseFuncRefCallByName(Token.Value);
                  ParseAssignTail;
                end else
                  ParseVarAssign(Token.Value);
              end;
            tkFunction:
              begin
                NextToken;
                ParseFuncCall(Token.Value);
                ParseAssignTail;
              end;
            else
              Error('Invalid statement', Token);
          end;
        end;
      tkImport:
        begin
          NextToken;
          ParseFuncImport;
        end;
      tkEOF:
        Exit;
      else
        Error('Invalid statement ' + TokenNames[Token.Kind], Token);
    end;
  end;

begin
  ContinueStack := TSEListStack.Create;
  BreakStack := TSEListStack.Create;
  ReturnStack := TSEListStack.Create;
  try
    Self.LocalVarCountList.Clear;
    Self.Binary := Self.VM.Binaries[0];
    repeat
      ParseBlock;
    until PeekAtNextToken.Kind = tkEOF;
    Emit([Pointer(opHlt)]);
    Self.IsParsed := True;
  finally
    FreeAndNil(ContinueStack);
    FreeAndNil(BreakStack);
    FreeAndNil(ReturnStack);
  end;
end;

procedure TEvilC.Reset;
var
  Ident: TSEIdent;
  I: Integer;
begin
  Self.FuncScriptList.Clear;
  Self.FuncImportList.Clear;
  Self.CurrentFileList.Clear;
  Self.LocalVarCountList.Clear;
  Self.VM.Reset;

  Self.VM.BinaryClear;
  Self.VM.IsDone := True;
  Self.Vm.IsPaused := False;
  Self.IsDone := False;
  Self.IsParsed := False;
  Self.IsLex := False;
  Self.VarList.Clear;
  Self.TokenList.Clear;
  Self.OpcodeInfoList.Clear;
  Self.IncludeList.Clear;
  Self.ScopeFunc.Clear;
  Self.ScopeStack.Clear;
  Self.GlobalVarCount := 1;
  Self.VarList.Count := Self.GlobalVarCount; // Safeguard
  Ident.Kind := ikVariable;
  Ident.Addr := 0;
  Ident.Name := 'result';
  Ident.Local := 0;
  Self.VarList[0] := Ident;
  ErrorLn := -1;
  ErrorCol := -1;
  FuncTraversal := 0;
end;

function TEvilC.Exec: TSEValue;
begin
  if not Self.IsLex then
    Self.Lex;
  if not Self.IsParsed then
  begin
    Self.Parse;
  end;
  Self.VM.Exec;
  Exit(Self.VM.Stack[0])
end;

procedure TEvilC.RegisterFunc(const Name: String; const Func: TSEFunc; const ArgCount: Integer);
var
  FuncNativeInfo: TSEFuncNativeInfo;
begin
  FuncNativeInfo.ArgCount := ArgCount;
  FuncNativeInfo.Func := Func;
  FuncNativeInfo.Name := Name;
  Self.FuncNativeList.Add(FuncNativeInfo);
end;

function TEvilC.RegisterScriptFunc(const Name: String; const ArgCount: Integer): PSEFuncScriptInfo;
var
  FuncScriptInfo: TSEFuncScriptInfo;
begin
  SetLength(Self.VM.Binaries, Length(Self.VM.Binaries) + 1);
  Self.VM.Binaries[Length(Self.VM.Binaries) - 1] := TSEBinary.Create;
  FuncScriptInfo.ArgCount := ArgCount;
  FuncScriptInfo.BinaryPos := Length(Self.VM.Binaries) - 1;
  FuncScriptInfo.Name := Name;
  Self.FuncScriptList.Add(FuncScriptInfo);
  Result := Self.FuncScriptList.Ptr(Self.FuncScriptList.Count - 1);
end;

procedure TEvilC.RegisterImportFunc(const Name, ActualName, LibName: String; const Args: TSEAtomKindArray; const Return: TSEAtomKind);
var
  FuncImportInfo: TSEFuncImportInfo;
  Lib: TLibHandle;
begin
  if DynlibMap.ContainsKey(LibName) then
    Lib := DynlibMap[LibName]
  else
  begin
    Lib := LoadLibrary(LibName);
    DynlibMap.Add(LibName, Lib);
  end;

  FuncImportInfo.Args := Args;
  FuncImportInfo.Return := Return;
  FuncImportInfo.Name := Name;
  FuncImportInfo.Func := nil;
  if Lib <> nil then
  begin
    FuncImportInfo.Func := GetProcAddress(Lib, ActualName);
  end;
  Self.FuncImportList.Add(FuncImportInfo);
end;

function TEvilC.Backup: TSECache;
var
  I, J: Integer;
  BackupBinary, SrcBinary: TSEBinary;
begin
  Result.LineOfCodeList := TIntegerList.Create;
  Result.FuncScriptList := TSEFuncScriptList.Create;
  Result.FuncImportList := TSEFuncImportList.Create;
  SetLength(Result.Binaries, Length(Self.VM.Binaries));
  for J := 0 to High(Self.VM.Binaries) do
  begin
    BackupBinary := TSEBinary.Create;
    Result.Binaries[J] := BackupBinary;
    SrcBinary := Self.VM.Binaries[J];
    for I := 0 to SrcBinary.Count - 1 do
    begin
      BackupBinary.Add(SrcBinary[I]);
    end;
  end;
  for I := 0 to Self.LineOfCodeList.Count - 1 do
  begin
    Result.LineOfCodeList.Add(Self.LineOfCodeList[I]);
  end;
  for I := 0 to Self.FuncScriptList.Count - 1 do
  begin
    Result.FuncScriptList.Add(Self.FuncScriptList[I]);
  end;
  for I := 0 to Self.FuncImportList.Count - 1 do
  begin
    Result.FuncImportList.Add(Self.FuncImportList[I]);
  end;
  Result.GlobalVarCount := Self.GlobalVarCount;
end;

procedure TEvilC.Restore(const Cache: TSECache);
var
  I, J: Integer;
  BackupBinary, DstBinary: TSEBinary;
begin
  Self.VM.BinaryClear;
  Self.LineOfCodeList.Clear;
  for I := 0 to Cache.LineOfCodeList.Count - 1 do
    Self.LineOfCodeList.Add(Cache.LineOfCodeList[I]);
  SetLength(Self.VM.Binaries, Length(Cache.Binaries));
  for I := 0 to High(Cache.Binaries) do
  begin
    BackupBinary := Cache.Binaries[I];
    DstBinary := TSEBinary.Create;
    Self.VM.Binaries[I] := DstBinary;
    for J := 0 to BackupBinary.Count - 1 do
      DstBinary.Add(BackupBinary[J]);
  end;
  for I := 0 to Cache.FuncScriptList.Count - 1 do
    Self.FuncScriptList.Add(Cache.FuncScriptList[I]);
  for I := 0 to Cache.FuncImportList.Count - 1 do
    Self.FuncImportList.Add(Cache.FuncImportList[I]);
  Self.GlobalVarCount := Cache.GlobalVarCount;
  Self.IsParsed := True;
end;

procedure TSECacheMap.ClearSingle(const AName: String);
var
  Cache: TSECache;
  I: Integer;
begin
  try
    Cache := Self[AName];
    for I := 0 to High(Cache.Binaries) do
      Cache.Binaries[I].Free;
    Cache.LineOfCodeList.Free;
    Cache.FuncScriptList.Free;
    Cache.FuncImportList.Free;
    Self.Remove(AName);
  except
  end;
end;

procedure TSECacheMap.Clear;
var
  S: String;
  Cache: TSECache;
  I: Integer;
begin
  for S in Self.Keys do
  begin
    Cache := Self[S];
    for I := 0 to High(Cache.Binaries) do
      Cache.Binaries[I].Free;
    Cache.LineOfCodeList.Free;
    Cache.FuncScriptList.Free;
    Cache.FuncImportList.Free;
  end;
  inherited;
end;

initialization
  SENull.Kind := sevkNull;
  SENull.Ref := 0;
  SENull.VarNumber := Floor(0);
  DynlibMap := TDynlibMap.Create;
  ScriptVarMap := TSEVarMap.Create;
  GC := TSEGarbageCollector.Create;
  ScriptCacheMap := TSECacheMap.Create;

finalization
  FreeAndNil(ScriptVarMap);
  DynlibMap.Free;
  if VMList <> nil then
    VMList.Free;
  VMList := nil;
  GC.Free;
  ScriptCacheMap.Free;

end.
