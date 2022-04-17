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

unit Mcdowell.EvilC;

{$mode objfpc}
{$ifdef CPUX86_64}
  {$asmmode intel}
{$endif}
{$H+}
{$modeswitch nestedprocvars}
{$modeswitch advancedrecords}
// enable this if you want to perform string manipulation (concat, compare)
{$define SE_STRING}
// enable this if you want to handle UTF-8 strings
{$define SE_STRING_UTF8}
// enable this if you want precision (use Double instead of Single)
{$define SE_PRECISION}

interface

uses
  SysUtils, Classes, Generics.Collections, StrUtils, Types, DateUtils, RegExpr
  {$ifdef SE_STRING_UTF8},LazUTF8{$endif}, dynlibs;

type
  TSENumber = {$ifdef SE_PRECISION}Double{$else}Single{$endif};

  TSEOpcode = (
    opPushConst,
    opPushLocalVar,
    opPushLocalArray,
    opPushLocalArrayPop,
    opPopConst,   
    opPopFrame,
    opAssignLocal,
    opAssignLocalArray,
    opJumpEqual,
    opJumpUnconditional,  
    opJumpEqualOrGreater,
    opJumpEqualOrLesser,
    opOperatorAdd,
    opOperatorSub,
    opOperatorMul,
    opOperatorDiv,
    opOperatorMod,
    opOperatorNegative,
    opOperatorSmaller,
    opOperatorSmallerOrEqual,
    opOperatorGreater,
    opOperatorGreaterOrEqual,
    opOperatorEqual,
    opOperatorNotEqual,
    opOperatorAnd,
    opOperatorOr,
    opOperatorNot,
    opCallNative,
    opCallScript,      
    opCallImport,
    opPause,
    opYield
  );

  TSENestedProc = procedure is nested;

  TSEValueKind = (
    sevkSingle,
    sevkString,
    sevkArray,
    sevkPointer
  );
  {$mode delphi}
  TSEValue = record
    {$ifdef SE_STRING}
    VarString: String;
    {$endif}
    VarArray: array of TSEValue;
    case Kind: TSEValueKind of
      sevkSingle:
        (
          VarNumber: TSENumber;
        );
      sevkString:
        (
          {$ifdef SE_STRING}
          VarStringDummy: PChar;
          {$else}
          VarString: PChar;
          {$endif}
        );
      sevkArray:
        (
          VarArrayDummy: Pointer;
        );
      sevkPointer:
        (
          VarPointer: Pointer;
        );
  end;
  {$mode objfpc}
  TSEValueArray = array of TSEValue;
  PSEValue = ^TSEValue;

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
    seakChars,
    seakWChars
  );
  TSEAtomKindArray = array of TSEAtomKind;

  TSEVM = class;
  TSEFunc = function(const VM: TSEVM; const Args: array of TSEValue): TSEValue of object;

  TSEFuncKind = (sefkNative, sefkScript, sefkImport);

  TSEFuncNativeInfo = record
    Name: String;
    Func: TSEFunc;
    ArgCount: Integer;
  end;
  PSEFuncNativeInfo = ^TSEFuncNativeInfo;

  TSEFuncScriptInfo = record 
    Name: String;
    Addr,
    StackAddr: Integer; // Used by parameters
    ArgCount: Integer;
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

  TEvilC = class;
  TSEVM = class
  public
    IsPaused: Boolean;
    IsDone: Boolean;
    IsYielded: Boolean;
    Stack: array of TSEValue;
    Frame: array of Integer;
    CodePtr: Integer;
    StackPtr: PSEValue;
    FramePtr: Integer;
    StackWorkingSize: Integer; // not count memory need for local variables
    Parent: TEvilC;
    Binary: TSEBinary;
    WaitTime: LongWord;

    constructor Create;
    destructor Destroy; override;
    function IsWaited: Boolean;
    procedure Reset;
    procedure Exec;
  end;

  TSECache = record
    Binary: TSEBinary;
    LocalVarListCount: Cardinal;
    LineOfCodeList: TIntegerList;
    FuncScriptList: TSEFuncScriptList;
    FuncImportList: TSEFuncImportList;
  end;
  TSECacheMapAncestor = specialize TDictionary<String, TSECache>;
  TSECacheMap = class(TSECacheMapAncestor)
  public
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
    tkPause,
    tkYield,
    tkSquareBracketOpen,
    tkSquareBracketClose,
    tkAnd,
    tkOr,
    tkNot,
    tkFor,
    tkTo,
    tkDownto,
    tkReturn,
    tkAtom,
    tkImport
  );
TSETokenKinds = set of TSETokenKind;

const TokenNames: array[TSETokenKind] of String = (
  'EOF', '.', '+', '-', '*', 'div', 'mod', '=', '!=', '<',
  '>', '<=', '>=', '{', '}', ':', '(', ')', 'neg', 'number', 'string',
  ',', 'if', 'identity', 'function', 'fn', 'variable', 'const',
  'unknown', 'else', 'while', 'break', 'continue', 'pause', 'yield',
  '[', ']', 'and', 'or', 'not', 'for', 'to', 'downto', 'return',
  'atom', 'import'
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
    ArgCount: Integer;
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
    procedure SetSource(V: String);
  public
    ErrorLn, ErrorCol: Integer;
    VM: TSEVM;
    IncludeList: TStrings;
    TokenList: TSETokenList;
    LocalVarList: TSEIdentList;
    FuncNativeList: TSEFuncNativeList;
    FuncScriptList: TSEFuncScriptList;
    FuncImportList: TSEFuncImportList;
    ConstMap: TSEConstMap;
    ScopeStack: TSEScopeStack;
    LineOfCodeList: TIntegerList;
    IsParsed: Boolean;
    IsDone: Boolean;
    FuncTraversal: Integer;
    CurrentFileList: TStrings;
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
    procedure RegisterScriptFunc(const Name: String; const Addr, StackAddr, ArgCount: Integer);
    procedure RegisterImportFunc(const Name, ActualName, LibName: String; const Args: TSEAtomKindArray; const Return: TSEAtomKind);
    function Backup: TSECache;
    procedure Restore(const Cache: TSECache);

    property IsPaused: Boolean read GetIsPaused write SetIsPaused;
    property Source: String read FSource write SetSource;
  end;

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
{$ifdef SE_STRING}
operator + (V1: TSEValue; V2: String) R: TSEValue;
{$endif}
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
{$ifdef SE_STRING}
operator <> (V1: TSEValue; V2: String) R: Boolean;
{$endif}
operator < (V1, V2: TSEValue) R: Boolean;
operator > (V1, V2: TSEValue) R: Boolean;
operator <= (V1, V2: TSEValue) R: Boolean;
operator >= (V1, V2: TSEValue) R: Boolean;
operator = (V1, V2: TSEValue) R: Boolean;
operator <> (V1, V2: TSEValue) R: Boolean;

var
  ScriptVarMap: TSEVarMap;

implementation

uses
  Math, Strings, Globals, Utils.Strings;

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
    class function SEGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SESet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEString(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SENumber(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEWait(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SELength(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEArray(const VM: TSEVM; const Args: array of TSEValue): TSEValue;      
    class function SEArrayCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEArrayDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SELerp(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SESLerp(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SESign(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SESin(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SECos(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SETan(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SECot(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEMin(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEMax(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEPow(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringGrep(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringSplit(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringFind(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringInsert(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue; 
    class function SEStringReplace(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringFormat(const VM: TSEVM; const Args: array of TSEValue): TSEValue;     
    class function SEStringUpperCase(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEStringLowerCase(const VM: TSEVM; const Args: array of TSEValue): TSEValue;  
    class function SEStringFindRegex(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function SEOS(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
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
  end;

  TDynlibMap = specialize TDictionary<String, TLibHandle>;

var
  DynlibMap: TDynlibMap;

class function TBuiltInFunction.SEBufferCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkSingle;
  SetLength(Result.VarString, Round(Args[0].VarNumber));
  Result.VarNumber := QWord(@Result.VarString[1]);
end;

class function TBuiltInFunction.SEBufferLength(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Length(Args[0].VarString);
end;

class function TBuiltInFunction.SEBufferGetU8(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkSingle;
  Result.VarNumber := Byte(Pointer(Round(Args[0].VarNumber))^);
end;

class function TBuiltInFunction.SEBufferGetU16(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkSingle;
  Result.VarNumber := Word(Pointer(Round(Args[0].VarNumber))^);
end;

class function TBuiltInFunction.SEBufferGetU32(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkSingle;
  Result.VarNumber := LongWord(Pointer(Round(Args[0].VarNumber))^);
end;

class function TBuiltInFunction.SEBufferGetU64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkSingle;
  Result.VarNumber := QWord(Pointer(Round(Args[0].VarNumber))^);
end;

class function TBuiltInFunction.SEBufferGetI8(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkSingle;
  Result.VarNumber := SmallInt(Pointer(Round(Args[0].VarNumber))^);
end;

class function TBuiltInFunction.SEBufferGetI16(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkSingle;
  Result.VarNumber := ShortInt(Pointer(Round(Args[0].VarNumber))^);
end;

class function TBuiltInFunction.SEBufferGetI32(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkSingle;
  Result.VarNumber := LongInt(Pointer(Round(Args[0].VarNumber))^);
end;

class function TBuiltInFunction.SEBufferGetI64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkSingle;
  Result.VarNumber := Int64(Pointer(Round(Args[0].VarNumber))^);
end;

class function TBuiltInFunction.SEBufferGetF64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkSingle;
  Result.VarNumber := Double(Pointer(Round(Args[0].VarNumber))^);
end;

class function TBuiltInFunction.SEBufferSetU8(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  P: Pointer;
begin
  P := Pointer(Round(Args[0].VarNumber));
  Byte(P^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetU16(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  P: Pointer;
begin
  P := Pointer(Round(Args[0].VarNumber));
  Word(P^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetU32(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  P: Pointer;
begin
  P := Pointer(Round(Args[0].VarNumber));
  LongWord(P^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetU64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  P: Pointer;
begin
  P := Pointer(Round(Args[0].VarNumber));
  QWord(P^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetI8(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  P: Pointer;
begin
  P := Pointer(Round(Args[0].VarNumber));
  ShortInt(P^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetI16(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  P: Pointer;
begin
  P := Pointer(Round(Args[0].VarNumber));
  SmallInt(P^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetI32(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  P: Pointer;
begin
  P := Pointer(Round(Args[0].VarNumber));
  LongInt(P^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetI64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  P: Pointer;
begin
  P := Pointer(Round(Args[0].VarNumber));
  Int64(P^) := Round(Args[1].VarNumber);
end;

class function TBuiltInFunction.SEBufferSetF64(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  P: Pointer;
begin
  P := Pointer(Round(Args[0].VarNumber));
  Double(P^) := Args[1];
end;

class function TBuiltInFunction.SEStringToBuffer(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := QWord(@Args[0].VarString[1]);
end;

class function TBuiltInFunction.SEBufferToString(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if QWord(Args[0].VarString[1]) = Round(Args[0].VarNumber) then
    Result := Args[0].VarString
  else
    Result := PChar(Round(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEWBufferToString(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  WS: WideString;
begin
  WS := PWideChar(Round(Args[0].VarNumber));
  Result := UTF8Encode(WS);
end;

class function TBuiltInFunction.SETypeOf(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  case Args[0].Kind of
    sevkArray:
      Result := 'array';   
    sevkSingle:
      Result := 'number';
    sevkString:
      Result := 'string';
    sevkPointer:
      Result := 'pointer';
  end;
end;

class function TBuiltInFunction.SEWrite(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
begin
  for I := 0 to Length(Args) - 1 do
  begin
    case Args[I].Kind of
      sevkSingle:
        Write(Args[I].VarNumber);
      sevkString:
        Write(Args[I].VarString);
    end;
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

class function TBuiltInFunction.SEGet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(ScriptVarMap[Args[0]]);
end;

class function TBuiltInFunction.SESet(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  ScriptVarMap.AddOrSetValue(Args[0], Args[1]);
end;

class function TBuiltInFunction.SEString(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if Args[0].Kind = sevkSingle then
    Exit(PointFloatToStr(Args[0].VarNumber));
  Exit(Args[0].VarString);
end;

class function TBuiltInFunction.SENumber(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(PointStrToFloat(Args[0]));
end;

class function TBuiltInFunction.SEWait(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  VM.WaitTime := GetTickCount + Round(Args[0].VarNumber * 1000);
end;

class function TBuiltInFunction.SELength(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  A: TSEValueArray;
begin
  case Args[0].Kind of
    sevkString:
      {$ifdef SE_STRING_UTF8}     
      Exit(UTF8Length(String(Args[0].VarString)));
      {$else}
      Exit(Length(String(Args[0].VarString)));
      {$endif}
    else
      begin
        A := Args[0];
        Exit(Length(A));
      end;
  end;
end;

class function TBuiltInFunction.SEArray(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkArray;
  SetLength(Result.VarArray, Args[0]);
end;

class function TBuiltInFunction.SEArrayCreate(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
begin
  Result.Kind := sevkArray;
  SetLength(Result.VarArray, Length(Args));
  for I := 0 to Length(Args) - 1 do
  begin
    Result.VarArray[I] := Args[I];
  end;
end;

class function TBuiltInFunction.SEArrayDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result.Kind := sevkArray;
  Result.VarArray := Args[0].VarArray;
  Delete(Result.VarArray, Round(Args[1].VarNumber), Round(Args[2].VarNumber));
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

class function TBuiltInFunction.SEMin(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if Args[0] < Args[1] then
    Exit(Args[0]);
  Exit(Args[1]);
end;

class function TBuiltInFunction.SEMax(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  if Args[0] > Args[1] then
    Exit(Args[0]);
  Exit(Args[1]);
end;

class function TBuiltInFunction.SEPow(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(Power(Args[0].VarNumber, Args[1].VarNumber));
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
    for I := 1 to Length(Args) - 1 do
      if V.IndexOf(Args[I]) >= 0 then
      begin
        if Result = '' then
          Result.VarString := V
        else
          Result.VarString := Result.VarString + #10 + V;
      end;
end;

class function TBuiltInFunction.SEStringSplit(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  D: TStringDynArray;
  A: TSEValueArray;
  I: Integer;
begin
  D := SplitString(Args[0], Args[1]);
  Result.Kind := sevkArray;
  SetLength(A, Length(D));
  for I := 0 to Length(D) - 1 do
    A[I] := D[I];
  Result.VarArray := A;
end;

class function TBuiltInFunction.SEStringFind(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Args[0].VarString.IndexOf(Args[1]);
end;        

class function TBuiltInFunction.SEStringDelete(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Args[0].VarString;
  {$ifdef SE_STRING_UTF8}
  UTF8Delete(Result.VarString, Round(Args[1].VarNumber + 1), Round(Args[2].VarNumber));
  {$else}
  Delete(Result.VarString, Round(Args[1].VarNumber + 1), Round(Args[2].VarNumber));
  {$endif}
end;  

class function TBuiltInFunction.SEStringInsert(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Result := Args[0]; 
  {$ifdef SE_STRING_UTF8}
  UTF8Insert(Args[1].VarString, Result.VarString, Round(Args[2].VarNumber + 1));
  {$else}
  Insert(Args[1].VarString, Result.VarString, Round(Args[2] + 1));
  {$endif}
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
begin
  S := Args[0].VarString;
  for I := 0 to Length(Args[1].VarArray) - 1 do
  begin
    V := '';
    if Args[1].VarArray[I].Kind = sevkSingle then
      V := PointFloatToStr(Args[1].VarArray[I].VarNumber)
    else
    if Args[1].VarArray[I].Kind = sevkString then
      V := Args[1].VarArray[I].VarString;
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
    sevkString: Result := UpperCase(Args[0].VarString);
    sevkSingle: Result := UpperCase(Char(Round(Args[0].VarNumber)));
  end;
end;

class function TBuiltInFunction.SEStringLowerCase(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  S: String;
begin
  Result := '';
  case Args[0].Kind of
    sevkString: Result := LowerCase(Args[0].VarString);
    sevkSingle: Result := LowerCase(Char(Round(Args[0].VarNumber)));
  end;
end;

class function TBuiltInFunction.SEStringFindRegex(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
var
  R: TRegExpr;
  I: Integer;
  C: Integer = 0;
begin
  Result.Kind := sevkArray;
  R := TRegExpr.Create(Args[1].VarString);
  if R.Exec(Args[0]) then
  repeat
    SetLength(Result.VarArray, Length(Result.VarArray) + R.SubExprMatchCount);
    for I := 1 to R.SubExprMatchCount do
    begin
      Result.VarArray[C].Kind := sevkArray;
      SetLength(Result.VarArray[C].VarArray, 2);
      Result.VarArray[C].VarArray[0] := R.Match[I];  
      Result.VarArray[C].VarArray[1] := R.MatchPos[I] - 1;
      Inc(C);
    end;
  until not R.ExecNext;
end;

class function TBuiltInFunction.SEOS(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  {$if defined(WINDOWS)}
  Exit('windows');
  {$elseif defined(LINUX)}
  Exit('linux');
  {$elseif defined(DARWIN)}
  Exit('darwin');
  {$else}
  Exit('unknown');
  {$endif}
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

operator := (V: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkSingle;
  R.VarNumber := V;
end;
{$ifdef SE_STRING}
operator := (V: String) R: TSEValue; inline;
begin
  R.Kind := sevkString;
  R.VarString := V;
end;
{$else}
operator := (V: String) R: TSEValue; inline;
begin
  R.Kind := sevkString;
  R.VarString := PChar(V);
end;
{$endif}
operator := (V: Boolean) R: TSEValue; inline;
begin
  R.Kind := sevkSingle;
  R.VarNumber := Integer(V);
end;
operator := (V: TSEValueArray) R: TSEValue; inline;
begin
  R.Kind := sevkArray;
  R.VarArray := V;
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
  R := V.VarString;
end;
operator := (V: TSEValue) R: TSEValueArray; inline;
begin
  R := TSEValueArray(V.VarArray);
end;
operator := (V: TSEValue) R: Pointer; inline;
begin
  R := V.VarPointer;
end;

operator + (V1: TSEValue; V2: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkSingle;
  R.VarNumber := V1.VarNumber + V2;
end;
{$ifdef SE_STRING}
operator + (V1: TSEValue; V2: String) R: TSEValue; inline;
begin
  R.VarString := V2;
end;
{$endif}
operator + (V1: TSEValue; V2: Pointer) R: TSEValue; inline;
begin
  R.Kind := sevkPointer;
  R.VarPointer := V1.VarPointer + V2;
end;

operator - (V1: TSEValue; V2: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkSingle;
  R.VarNumber := V1.VarNumber - V2;
end;
operator - (V1: TSEValue; V2: Pointer) R: TSEValue; inline;
begin
  R.Kind := sevkString;
  R.VarPointer := V1.VarPointer + V2;
end;

operator * (V1: TSEValue; V2: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkSingle;
  R.VarNumber := V1.VarNumber * V2;
end;

operator / (V1: TSEValue; V2: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkSingle;
  R.VarNumber := V1.VarNumber / V2;
end;

operator + (V1, V2: TSEValue) R: TSEValue; inline;
var
  I, Len: Integer;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkSingle:
      begin
        R.Kind := sevkSingle;
        R.VarNumber := V1.VarNumber + V2.VarNumber;
      end;
    sevkPointer:
      begin
        R.Kind := sevkPointer;
        R.VarPointer := V1.VarPointer + V2.VarPointer;
      end;   
    sevkArray:
      begin
        R.Kind := sevkArray;
        SetLength(R.VarArray, Length(V1.VarArray) + Length(V2.VarArray));
        Len := Length(V1.VarArray);
        for I := 0 to Len - 1 do
          R.VarArray[I] := V1.VarArray[I];    
        for I := Len to Len + Length(V2.VarArray) - 1 do
          R.VarArray[I] := V2.VarArray[I - Len];
      end;
    {$ifdef SE_STRING}
    sevkString:
      begin
        R.Kind := sevkString;
        R.VarString := V1.VarString + V2.VarString;
      end;
    {$endif}
  end;
end;
operator - (V: TSEValue) R: TSEValue; inline;
begin
  case V.Kind of
    sevkSingle:
      R.VarNumber := -V.VarNumber;
  end;
end;
operator - (V1, V2: TSEValue) R: TSEValue; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkSingle:
      begin
        R.Kind := sevkSingle;
        R.VarNumber := V1.VarNumber - V2.VarNumber;
      end;
    sevkPointer:
      begin
        R.Kind := sevkPointer;
        R.VarPointer := Pointer(V1.VarPointer - V2.VarPointer);
      end;
  end;
end;
operator * (V1, V2: TSEValue) R: TSEValue; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkSingle:
      begin
        R.Kind := sevkSingle;
        R.VarNumber := V1.VarNumber * V2.VarNumber;
      end;
  end;
end;
operator / (V1, V2: TSEValue) R: TSEValue; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkSingle:
      begin
        R.Kind := sevkSingle;
        R.VarNumber := V1.VarNumber / V2.VarNumber;
      end;
  end;
end;

operator < (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  case V1.Kind of
    sevkSingle:
      R := V1.VarNumber < V2;
  end;
end;
operator > (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  case V1.Kind of
    sevkSingle:
      R := V1.VarNumber > V2;
  end;
end;
operator <= (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  case V1.Kind of
    sevkSingle:
      R := V1.VarNumber <= V2;
  end;
end;
operator >= (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  case V1.Kind of
    sevkSingle:
      R := V1.VarNumber >= V2;
  end;
end;
operator = (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  case V1.Kind of
    sevkSingle:
      R := V1.VarNumber = V2;
  end;
end;
{$ifdef SE_STRING}
operator = (V1: TSEValue; V2: String) R: Boolean; inline;
begin
  case V1.Kind of
    sevkString:
      R := V1.VarString = V2;
  end;
end;
{$endif}
operator <> (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  case V1.Kind of
    sevkSingle:
      R := V1.VarNumber <> V2;
  end;
end;
{$ifdef SE_STRING}
operator <> (V1: TSEValue; V2: String) R: Boolean; inline;
begin
  case V1.Kind of
    sevkString:
      R := V1.VarString <> V2;
  end;
end;
{$endif}

operator < (V1, V2: TSEValue) R: Boolean; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkSingle:
      R := V1.VarNumber < V2.VarNumber;
  end;
end;
operator > (V1, V2: TSEValue) R: Boolean; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkSingle:
      R := V1.VarNumber > V2.VarNumber;
  end;
end;
operator <= (V1, V2: TSEValue) R: Boolean; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkSingle:
      R := V1.VarNumber <= V2.VarNumber;
  end;
end;
operator >= (V1, V2: TSEValue) R: Boolean; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkSingle:
      R := V1.VarNumber >= V2.VarNumber;
  end;
end;
operator = (V1, V2: TSEValue) R: Boolean; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkSingle:
      R := V1.VarNumber = V2.VarNumber;
  {$ifdef SE_STRING}
    sevkString:
      R := V1.VarString = V2.VarString;
  {$endif}
  end;
end;
operator <> (V1, V2: TSEValue) R: Boolean; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkSingle:
      R := V1.VarNumber <> V2.VarNumber;
    {$ifdef SE_STRING}
    sevkString:
      R := V1.VarString <> V2.VarString;
    {$endif}
  end;
end;

constructor TSEVM.Create;
begin
  inherited;
  Self.Binary := TSEBinary.Create;
  Self.CodePtr := 0;
  Self.IsPaused := False;
  Self.IsDone := True;
  Self.WaitTime := 0;
  Self.StackWorkingSize := 256;
end;

destructor TSEVM.Destroy;
begin
  FreeAndNil(Self.Binary);
  inherited;
end;

function TSEVM.IsWaited: Boolean;
begin
  Exit(GetTickCount < Self.WaitTime);
end;

procedure TSEVM.Reset;
begin
  Self.CodePtr := 0;
  Self.IsPaused := False;
  Self.IsDone := False;
  Self.Parent.IsDone := False;
  Self.WaitTime := 0;
  SetLength(Self.Stack, Self.Parent.LocalVarList.Count + 64 + StackWorkingSize);
  SetLength(Self.Frame, 64);
  Self.FramePtr := 0;
  Self.StackPtr := @Self.Stack[0];
  Self.StackPtr := Self.StackPtr + Self.Parent.LocalVarList.Count + 64;
end;

procedure TSEVM.Exec;
var
  A, B, C: PSEValue;
  V: PSEValue;
  TV: TSEValue;
  {$ifdef SE_STRING}
  S: String;
  {$else}
  S: PChar;
  {$endif}
  WS: WideString;
  FuncNativeInfo: PSEFuncNativeInfo;
  FuncScriptInfo: PSEFuncScriptInfo;
  FuncImportInfo: PSEFuncImportInfo;
  I, J, {$ifdef LINUX}ArgCountStack, {$endif}ArgCount, ArgSize: Integer;
  Args: array of TSEValue;
  CodePtrLocal: Integer;
  StackPtrLocal: PSEValue;
  BinaryLocal: TSEBinary;
  MMXCount, RegCount: QWord;
  {$ifdef LINUX}
  ImportBufferIndex: array [0..31] of QWord;
  {$endif}
  ImportBufferData: array [0..8*31] of Byte;
  ImportBufferString: array [0..31] of String;     
  ImportBufferWideString: array [0..31] of WideString;
  ImportResult: QWord;                         
  ImportResultD: Double;
  FuncImport, P, PP: Pointer;

  procedure Push(const Value: TSEValue); inline;
  begin
    StackPtrLocal^ := Value;
    Inc(StackPtrLocal);
  end;

  function Pop: PSEValue; inline;
  var
    T: Integer;
  begin
    Dec(StackPtrLocal);
    Result := StackPtrLocal;
  end;

  procedure Assign(const I: Integer; const Value: PSEValue); inline;
  begin
    Self.Stack[I] := Value^;
  end;

  function Get(const I: Integer): PSEValue; inline;
  begin
    Exit(@Self.Stack[I]);
  end;

label
  Loop, FinishLoop, LoopMMX, LoopMMXAlloc, AllocMMX6, AllocMMX5, AllocMMX4, AllocMMX3, AllocMMX2, AllocMMX1,
  AllocMMX0, LoopMMXFinishAlloc, LoopReg, LoopRegAlloc, AllocRDI, AllocRSI, AllocRDX, AllocRCX, AllocR9, LoopRegFinishAlloc,
  LoopFinishAlloc;

begin
  if Self.IsDone then
    Self.Reset;
  Self.IsYielded := False;
  if Self.IsPaused or Self.IsWaited then
    Exit;
  CodePtrLocal := Self.CodePtr;
  StackPtrLocal := Self.StackPtr;
  BinaryLocal := Self.Binary;
  try
    while CodePtrLocal < BinaryLocal.Count do
    begin
      case TSEOpcode(Integer(BinaryLocal.Ptr(CodePtrLocal)^.VarPointer)) of
        opOperatorAdd:
          begin
            B := Pop;
            A := Pop;
            Push(A^ + B^);
            Inc(CodePtrLocal);
          end;
        opOperatorSub:
          begin
            B := Pop;
            A := Pop;
            Push(A^ - B^);
            Inc(CodePtrLocal);
          end;
        opOperatorMul:
          begin
            B := Pop;
            A := Pop;
            Push(A^ * B^);
            Inc(CodePtrLocal);
          end;
        opOperatorDiv:
          begin
            B := Pop;
            A := Pop;
            Push(A^ / B^);
            Inc(CodePtrLocal);
          end;
        opOperatorMod:
          begin
            B := Pop;
            A := Pop;
            Push(A^ - B^ * Int(TSENumber(A^ / B^)));
            Inc(CodePtrLocal);
          end;
        opOperatorEqual:
          begin
            B := Pop;
            A := Pop;
            Push(A^ = B^);
            Inc(CodePtrLocal);
          end;
        opOperatorNotEqual:
          begin
            B := Pop;
            A := Pop;
            Push(A^ <> B^);
            Inc(CodePtrLocal);
          end;
        opOperatorSmaller:
          begin
            B := Pop;
            A := Pop;
            Push(A^ < B^);
            Inc(CodePtrLocal);
          end;
        opOperatorSmallerOrEqual:
          begin
            B := Pop;
            A := Pop;
            Push(A^ <= B^);
            Inc(CodePtrLocal);
          end;
        opOperatorGreater:
          begin
            B := Pop;
            A := Pop;
            Push(A^ > B^);
            Inc(CodePtrLocal);
          end;
        opOperatorGreaterOrEqual:
          begin
            B := Pop;
            A := Pop;
            Push(A^ >= B^);
            Inc(CodePtrLocal);
          end;
        opOperatorAnd:
          begin
            B := Pop;
            A := Pop;
            Push(Integer(A^) and Integer(B^));
            Inc(CodePtrLocal);
          end;
        opOperatorOr:
          begin
            B := Pop;
            A := Pop;
            Push(Integer(A^) or Integer(B^));
            Inc(CodePtrLocal);
          end;
        opOperatorNot:
          begin
            A := Pop;
            Push(not Integer(A^));
            Inc(CodePtrLocal);
          end;
        opOperatorNegative:
          begin
            A := Pop;
            Push(-(A^));
            Inc(CodePtrLocal);
          end;
        opPushConst:
          begin
            Push(BinaryLocal.Ptr(CodePtrLocal + 1)^);
            Inc(CodePtrLocal, 2);
          end;
        opPushLocalVar:
          begin
            Push(Get(BinaryLocal.Ptr(CodePtrLocal + 1)^)^);
            Inc(CodePtrLocal, 2);
          end;
        opPushLocalArray:
          begin
            A := BinaryLocal.Ptr(CodePtrLocal + 1);
            B := Get(A^);
            case B^.Kind of
              sevkString:
                {$ifdef SE_STRING}
                {$ifdef SE_STRING_UTF8}
                  Push(UTF8Copy(B^.VarString, Integer(Pop^) + 1, 1));
                {$else}            
                  Push(B^.VarString[Integer(Pop^) + 1]);
                {$endif}
                {$else}
                Push(B^.VarString[Integer(Pop^)]);
                {$endif}
              else
                Push(TSEValueArray(B^.VarArray)[Integer(Pop^)]);
            end;
            Inc(CodePtrLocal, 2);
          end;
        opPushLocalArrayPop:
          begin
            A := Pop;
            B := Pop;
            case B^.Kind of
              sevkString:
                {$ifdef SE_STRING}   
                {$ifdef SE_STRING_UTF8}
                  Push(UTF8Copy(B^.VarString, Integer(A^) + 1, 1));
                {$else}
                  Push(B^.VarString[Integer(A^) + 1]);
                {$endif}
                {$else}
                Push(B^.VarString[Integer(A^)]);
                {$endif}
              else
                Push(TSEValueArray(B^.VarArray)[Integer(A^)]);
            end;
            Inc(CodePtrLocal);
          end;
        opPopConst:
          begin
            Dec(StackPtrLocal); // Pop;
            Inc(CodePtrLocal);
          end;
        opJumpEqual:
          begin
            B := Pop;
            A := Pop;
            if A^ = B^ then
              CodePtrLocal := BinaryLocal.Ptr(CodePtrLocal + 1)^
            else
              Inc(CodePtrLocal, 2);
          end;
        opJumpUnconditional:
          begin
            CodePtrLocal := BinaryLocal.Ptr(CodePtrLocal + 1)^
          end;  
        opJumpEqualOrGreater:
          begin
            B := Pop;
            A := Pop;
            if A^ >= B^ then
              CodePtrLocal := BinaryLocal.Ptr(CodePtrLocal + 1)^
            else
              Inc(CodePtrLocal, 2);
          end;        
        opJumpEqualOrLesser:
          begin
            B := Pop;
            A := Pop;
            if A^ <= B^ then
              CodePtrLocal := BinaryLocal.Ptr(CodePtrLocal + 1)^
            else
              Inc(CodePtrLocal, 2);
          end;
        opCallNative:
          begin
            FuncNativeInfo := PSEFuncNativeInfo(Pointer(BinaryLocal.Ptr(CodePtrLocal + 1)^));
            ArgCount := BinaryLocal.Ptr(CodePtrLocal + 2)^;
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
          end;
        opCallScript:
          begin
            Self.Frame[Self.FramePtr] := CodePtrLocal + 3;
            Inc(Self.FramePtr);
            FuncScriptInfo := Self.Parent.FuncScriptList.Ptr(BinaryLocal.Ptr(CodePtrLocal + 1)^);
            ArgCount := BinaryLocal.Ptr(CodePtrLocal + 2)^;
            J := FuncScriptInfo^.StackAddr + ArgCount;
            for I := ArgCount - 1 downto 0 do
            begin
              Self.Stack[J] := Pop^;
              Dec(J);
            end;
            CodePtrLocal := FuncScriptInfo^.Addr;
          end;    
        opCallImport:
          begin                                          
            FuncImportInfo := Self.Parent.FuncImportList.Ptr(BinaryLocal.Ptr(CodePtrLocal + 1)^);
            FuncImport := FuncImportInfo^.Func;
            if FuncImport = nil then
              raise Exception.Create(Format('Function "%s" is null', [FuncImportInfo^.Name]));
            ArgCount := Length(FuncImportInfo^.Args);
            ArgSize := ArgCount * 8;
            {$ifdef LINUX}
            MMXCount := 0;
            RegCount := 0;
            {$endif}

            for I := ArgCount - 1 downto 0 do
            begin
              case FuncImportInfo^.Args[I] of
                seakI8:
                  begin
                    Int64((@ImportBufferData[I * 8])^) := ShortInt(Round(Pop^.VarNumber));
                    {$ifdef LINUX}
                    ImportBufferIndex[I] := 0;
                    Inc(RegCount);
                    {$endif}
                  end;       
                seakI16:
                  begin
                    Int64((@ImportBufferData[I * 8])^) := SmallInt(Round(Pop^.VarNumber));
                    {$ifdef LINUX}
                    ImportBufferIndex[I] := 0;   
                    Inc(RegCount);    
                    {$endif}
                  end;
                seakI32:
                  begin
                    Int64((@ImportBufferData[I * 8])^) := LongInt(Round(Pop^.VarNumber));  
                    {$ifdef LINUX}
                    ImportBufferIndex[I] := 0;    
                    Inc(RegCount);  
                    {$endif}
                  end;    
                seakI64:
                  begin
                    Int64((@ImportBufferData[I * 8])^) := Int64(Round(Pop^.VarNumber)); 
                    {$ifdef LINUX}
                    ImportBufferIndex[I] := 0; 
                    Inc(RegCount);   
                    {$endif}
                  end;        
                seakU8:
                  begin
                    QWord((@ImportBufferData[I * 8])^) := Byte(Round(Pop^.VarNumber));  
                    {$ifdef LINUX}
                    ImportBufferIndex[I] := 0; 
                    Inc(RegCount);
                    {$endif}
                  end;
                seakU16:
                  begin
                    QWord((@ImportBufferData[I * 8])^) := Word(Round(Pop^.VarNumber));
                    {$ifdef LINUX}
                    ImportBufferIndex[I] := 0;  
                    Inc(RegCount); 
                    {$endif}
                  end;
                seakU32:
                  begin
                    QWord((@ImportBufferData[I * 8])^) := LongWord(Round(Pop^.VarNumber));
                    {$ifdef LINUX}
                    ImportBufferIndex[I] := 0;   
                    Inc(RegCount);  
                    {$endif}
                  end;
                seakU64:
                  begin
                    QWord((@ImportBufferData[I * 8])^) := QWord(Round(Pop^.VarNumber));
                    {$ifdef LINUX}
                    ImportBufferIndex[I] := 0;  
                    Inc(RegCount);
                    {$endif}
                  end;     
               { seakF32:
                  begin
                    Double((@ImportBufferData[I * 8])^) := Pop^.VarNumber;
                  end;}
                seakF64:
                  begin
                    Double((@ImportBufferData[I * 8])^) := Pop^.VarNumber;
                    {$ifdef LINUX}
                    ImportBufferIndex[I] := 1;
                    Inc(MMXCount); 
                    {$endif}
                  end;     
                seakChars:
                  begin
                    A := Pop;
                    if A^.Kind = sevkString then
                    begin
                      ImportBufferString[I] := A^.VarString + #0;
                      PChar((@ImportBufferData[I * 8])^) := PChar(ImportBufferString[I]);
                    end else
                      QWord((@ImportBufferData[I * 8])^) := Round(A^.VarNumber);
                    {$ifdef LINUX}
                    ImportBufferIndex[I] := 0;
                    Inc(RegCount);
                    {$endif}
                  end;
                seakWChars:
                  begin
                    A := Pop;
                    if A^.Kind = sevkString then
                    begin
                      ImportBufferWideString[I] := UTF8Decode(A^.VarString + #0);
                      PChar((@ImportBufferData[I * 8])^) := PChar(ImportBufferWideString[I]);
                    end else
                      QWord((@ImportBufferData[I * 8])^) := Round(A^.VarNumber);     
                    {$ifdef LINUX}
                    ImportBufferIndex[I] := 0;   
                    Inc(RegCount); 
                    {$endif}
                  end;
              end;
            end;
            P := @ImportBufferData[0];
            {$if defined(LINUX)}                           
            PP := @ImportBufferIndex[0];
            ArgCountStack := Max(0, Int64(MMXCount) - 8) + Max(0, Int64(RegCount) - 6);
            {$endif}
            {$ifdef CPUX86_64}
            {$if defined(WINDOWS)}
              asm
                mov  rbx,P
                mov  rcx,[rbx]
                mov  rdx,[rbx + 8]
                mov  r8,[rbx + 16]
                mov  r9,[rbx + 24]
                movsd xmm0,[rbx]
                movsd xmm1,[rbx + 8]
                movsd xmm2,[rbx + 16]
                movsd xmm3,[rbx + 24]
                xor  rax,rax
                mov  eax,ArgCount
                cmp  eax,4
                jle  FinishLoop
                add  ebx,ArgSize
                sub  eax,5
              Loop:         
                sub  rbx,8
                mov  r11,[rbx]
                push r11
                cmp  rax,0
                je   FinishLoop
                dec  rax
                jmp  Loop
              FinishLoop:
                sub  rsp,32
                call [FuncImport]
                mov  ImportResult,rax
                movsd ImportResultD,xmm0
                xor  rax,rax
                mov  eax,ArgCount
                mov  ecx,8
                mul  ecx
                add  rsp,rax
              end ['rax', 'rbx', 'rcx', 'rdx', 'r8', 'r9', 'r11', 'xmm0', 'xmm1', 'xmm2', 'xmm3'];
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
                sub  rsp,8
                call [FuncImport]
                mov  ImportResult,rax
                movsd ImportResultD,xmm0
                xor  rax,rax
                mov  eax,ArgCountStack
                mov  ecx,8
                mul  ecx
                add  rsp,rax
                add  rsp,8
              end ['rax', 'rbx', 'rcx', 'rdx', 'r8', 'r9', 'r10', 'r11', 'r12', 'r13', 'r14', 'xmm0', 'xmm1', 'xmm2', 'xmm3', 'xmm4', 'xmm5', 'xmm6', 'xmm7'];
            {$endif}
            {$else}
            throw Exception.Create('Import external function does not support this CPU architecture');
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
              seakU64, seakChars, seakWChars:
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
            Inc(CodePtrLocal, 2);
          end;
        opPopFrame:
          begin      
            Dec(Self.FramePtr);
            CodePtrLocal := Self.Frame[Self.FramePtr];
          end;
        opAssignLocal:
          begin
            Assign(BinaryLocal.Ptr(CodePtrLocal + 2)^, Pop);
            Inc(CodePtrLocal, 3);
          end;
        opAssignLocalArray:
          begin
            A := BinaryLocal.Ptr(CodePtrLocal + 2);
            B := Pop;
            C := Pop;
            V := @Self.Stack[Integer(A^)];
            case B^.Kind of
              sevkString:
                begin
                  if V^.Kind = sevkString then
                  begin
                    {$ifdef SE_STRING}
                    {$ifdef SE_STRING_UTF8}
                      UTF8Delete(V^.VarString, Integer(C^) + 1, 1);
                      S := UTF8Copy(B^.VarString, 1, 1);
                      UTF8Insert(S, V^.VarString, Integer(C^) + 1);
                    {$else}
                      V^.VarString[Integer(C^) + 1] := B^.VarString[1];
                    {$endif}
                    {$else}  
                    S := V^.VarString;
                    S[C^] := B^.VarString[0];
                    {$endif}
                    // Self.Stack[A] := S;
                  end else
                  begin
                    TSEValueArray(V^.VarArray)[Integer(C^)] := B^;
                    Self.Stack[Integer(A^)] := V^;
                  end;
                end;
              sevkSingle:
                begin
                  if V^.Kind = sevkString then
                  begin
                    {$ifdef SE_STRING} 
                    {$ifdef SE_STRING_UTF8}
                      UTF8Delete(V^.VarString, Integer(C^) + 1, 1);
                      S := Char(Round(B^.VarNumber));
                      UTF8Insert(S, V^.VarString, Integer(C^) + 1);
                    {$else}
                      V^.VarString[Integer(C^) + 1] := Char(Round(B^.VarNumber));
                    {$endif}
                    {$else}   
                    S := V^.VarString;
                    S[C^] := Char(Round(B^.VarNumber));
                    {$endif}
                    // Self.Stack[A] := S;
                  end else
                  begin
                    TSEValueArray(V^.VarArray)[Integer(C^)] := B^;
                    Self.Stack[Integer(A^)] := V^;
                  end;
                end;
              else
                begin
                  TSEValueArray(V^.VarArray)[Integer(C^)] := B^;
                  Self.Stack[Integer(A^)] := V^;
                end;
            end;
            Inc(CodePtrLocal, 3);
          end;
        opPause:
          begin
            Self.IsPaused := True;
            Inc(CodePtrLocal);
            Self.CodePtr := CodePtrLocal;
            Self.StackPtr := StackPtrLocal;
            Exit;
          end;
        opYield:
          begin
            Self.IsYielded := True;
            Inc(CodePtrLocal);
            Self.CodePtr := CodePtrLocal;
            Self.StackPtr := StackPtrLocal;
            Exit;
          end;
      end;
      if Self.IsPaused or Self.IsWaited then
      begin
        Self.CodePtr := CodePtrLocal;
        Self.StackPtr := StackPtrLocal;
        Exit;
      end;
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
  Self.IsDone := True;
  Self.Parent.IsDone := True;
end;

constructor TEvilC.Create;
begin
  inherited;
  Self.VM := TSEVM.Create;
  Self.TokenList := TSETokenList.Create;
  Self.LocalVarList := TSEIdentList.Create;
  Self.FuncNativeList := TSEFuncNativeList.Create;
  Self.FuncScriptList := TSEFuncScriptList.Create;
  Self.FuncImportList := TSEFuncImportList.Create;
  Self.ConstMap := TSEConstMap.Create;
  Self.ScopeStack := TSEScopeStack.Create;
  Self.LineOfCodeList := TIntegerList.Create;
  Self.IncludeList := TStringList.Create;
  Self.CurrentFileList := TStringList.Create;
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
  Self.RegisterFunc('get', @TBuiltInFunction(nil).SEGet, 2);
  Self.RegisterFunc('set', @TBuiltInFunction(nil).SESet, 2);
  Self.RegisterFunc('string', @TBuiltInFunction(nil).SEString, 1);
  Self.RegisterFunc('number', @TBuiltInFunction(nil).SENumber, 1);
  Self.RegisterFunc('wait', @TBuiltInFunction(nil).SEWait, 1);
  Self.RegisterFunc('length', @TBuiltInFunction(nil).SELength, 1);
  Self.RegisterFunc('array', @TBuiltInFunction(nil).SEArray, 1);  
  Self.RegisterFunc('array_create', @TBuiltInFunction(nil).SEArrayCreate, -1);     
  Self.RegisterFunc('array_delete', @TBuiltInFunction(nil).SEArrayDelete, 3);
  Self.RegisterFunc('sign', @TBuiltInFunction(nil).SESign, 1);
  Self.RegisterFunc('min', @TBuiltInFunction(nil).SEMin, 2);
  Self.RegisterFunc('max', @TBuiltInFunction(nil).SEMax, 2);
  Self.RegisterFunc('pow', @TBuiltInFunction(nil).SEPow, 2);
  Self.RegisterFunc('string_grep', @TBuiltInFunction(nil).SEStringGrep, -1);   
  Self.RegisterFunc('string_format', @TBuiltInFunction(nil).SEStringFormat, 2);
  Self.RegisterFunc('string_split', @TBuiltInFunction(nil).SEStringSplit, 2);
  Self.RegisterFunc('string_find', @TBuiltInFunction(nil).SEStringFind, 2);   
  Self.RegisterFunc('string_delete', @TBuiltInFunction(nil).SEStringDelete, 3);  
  Self.RegisterFunc('string_insert', @TBuiltInFunction(nil).SEStringInsert, 3);
  Self.RegisterFunc('string_replace', @TBuiltInFunction(nil).SEStringReplace, 3);
  Self.RegisterFunc('string_uppercase', @TBuiltInFunction(nil).SEStringUpperCase, 1);
  Self.RegisterFunc('string_lowercase', @TBuiltInFunction(nil).SEStringLowerCase, 1);      
  Self.RegisterFunc('string_find_regex', @TBuiltInFunction(nil).SEStringFindRegex, 2);
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
  Self.RegisterFunc('sin', @TBuiltInFunction(nil).SESin, 1);
  Self.RegisterFunc('cos', @TBuiltInFunction(nil).SECos, 1);
  Self.RegisterFunc('tan', @TBuiltInFunction(nil).SETan, 1);
  Self.RegisterFunc('cot', @TBuiltInFunction(nil).SECot, 1);
  Self.RegisterFunc('os', @TBuiltInFunction(nil).SEOS, 0);
  Self.AddDefaultConsts;
  Self.Source := '';
end;

destructor TEvilC.Destroy;
begin
  FreeAndNil(Self.VM);
  FreeAndNil(Self.TokenList);
  FreeAndNil(Self.LocalVarList);
  FreeAndNil(Self.FuncNativeList);
  FreeAndNil(Self.FuncScriptList);     
  FreeAndNil(Self.FuncImportList);
  FreeAndNil(Self.ConstMap);
  FreeAndNil(Self.ScopeStack);
  FreeAndNil(Self.LineOfCodeList);
  FreeAndNil(Self.IncludeList);
  FreeAndNil(Self.CurrentFileList);
  inherited;
end;

procedure TEvilC.AddDefaultConsts;
begin
  Self.ConstMap.Add('PI', PI);
  Self.ConstMap.Add('true', True);
  Self.ConstMap.Add('false', False);
end;

procedure TEvilC.SetSource(V: String);
begin
  Self.Reset;
  Self.FSource := V;
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
      raise Exception.CreateFmt('[%d,%d] %s', [Ln, Col, S])
    else
      raise Exception.CreateFmt('(%s) [%d,%d] %s', [N, Ln, Col, S]);
  end;

var
  IsLoopDone: Boolean;
  PrevQuote: Char;
  SL: TStrings;
  BackupSource: String;

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
        Token.Kind := tkEnd;
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
        Token.Kind := tkAdd;
      '-':
        begin
          Token.Kind := tkSub;
          if Pos > 1 then
          begin
            PC := Self.Source[Pos - 1];
            NC := PeekAtNextChar;
            if ((PC = ' ') or (PC = '(') or (PC = '=') or (PC = ',')) and (NC <> ' ') then
              Token.Kind := tkNegative;
          end;
        end;
      '*':
        Token.Kind := tkMul;
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
      '#':
        begin             
          Token.Value := '';
          C := PeekAtNextChar;
          while C in ['0'..'9', 'A'..'Z', 'a'..'z', '_'] do
          begin
            Token.Value := Token.Value + NextChar;
            C := PeekAtNextChar;
          end;

          if Token.Value <> 'include' then
            Error('Expected "include"');
                
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
          if not FileExists(Token.Value) then
          begin
            Error(Format('"%s" not found', [Token.Value]));
          end;
          if Self.IncludeList.IndexOf(Token.Value) < 0 then
          begin
            BackupSource := Source;
            SL := TStringList.Create;
            try
              Self.CurrentFileList.Add(Token.Value);
              SL.LoadFromFile(Token.Value);
              FSource := SL.Text;
              Self.Lex(True);   
              Self.CurrentFileList.Pop;
            finally
              SL.Free;
            end;
            FSource := BackupSource;
            Self.IncludeList.Add(Token.Value);
          end;
          continue;
        end;
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
      'A'..'Z', 'a'..'z', '_':
        begin
          Token.Value := C;
          C := PeekAtNextChar;
          while C in ['0'..'9', 'A'..'Z', 'a'..'z', '_'] do
          begin
            Token.Value := Token.Value + NextChar;
            C := PeekAtNextChar;
          end;
          C := 'H';
          case Token.Value of
            'if':
              Token.Kind := tkIf;
            'else':
              Token.Kind := tkElse;
            'for':
              Token.Kind := tkFor;      
            'to':
              Token.Kind := tkTo;
            'downto':
              Token.Kind := tkDownto;
            'while':
              Token.Kind := tkWhile;
            'continue':
              Token.Kind := tkContinue;
            'break':
              Token.Kind := tkBreak;
            'pause':
              Token.Kind := tkPause;
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
  until C = #0;
end;

procedure TEvilC.Parse;
var
  Pos: Integer = -1;
  Token: TSEToken;
  ContinueStack: TSEListStack;
  BreakStack: TSEListStack;

  procedure Error(const S: String; const Token: TSEToken);
  begin
    ErrorLn := Token.Ln;
    ErrorCol := Token.Col;
    if Token.BelongedFileName = '' then
      raise Exception.CreateFmt('[%d,%d] %s', [Token.Ln, Token.Col, S])
    else                                                                
      raise Exception.CreateFmt('(%s) [%d,%d] %s', [Token.BelongedFileName, Token.Ln, Token.Col, S]);
  end;

  function FindFunc(const Name: String): Pointer; inline;
  var
    I: Integer;
  begin
    for I := 0 to Self.FuncNativeList.Count - 1 do
    begin
      Result := Self.FuncNativeList.Ptr(I);
      if PSEFuncNativeInfo(Result)^.Name = Name then
        Exit(Result);
    end;      
    for I := 0 to Self.FuncScriptList.Count - 1 do
    begin
      Result := Self.FuncScriptList.Ptr(I);
      if PSEFuncScriptInfo(Result)^.Name = Name then
        Exit(Result);
    end;
    for I := 0 to Self.FuncImportList.Count - 1 do
    begin
      Result := Self.FuncImportList.Ptr(I);
      if PSEFuncImportInfo(Result)^.Name = Name then
        Exit(Result);
    end;
    Exit(nil);
  end;

  function FindFuncNative(const Name: String; var Ind: Integer): PSEFuncNativeInfo; inline;
  var
    I: Integer;
  begin
    for I := 0 to Self.FuncNativeList.Count - 1 do
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
    for I := 0 to Self.FuncScriptList.Count - 1 do
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
    for I := 0 to Self.FuncImportList.Count - 1 do
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

  function FindVar(const Name: String): PSEIdent; inline;
  var
    I: Integer;
  begin
    for I := Self.LocalVarList.Count - 1 downto 0 do
    begin
      Result := Self.LocalVarList.Ptr(I);
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

  function NextToken: TSEToken; inline;
  begin
    Pos := Pos + 1;
    if Pos >= Self.TokenList.Count then
      Pos := Pos - 1;
    Result := Self.TokenList[Pos];
    if Self.LineOfCodeList.Count + 1 < Result.Ln then
      Self.LineOfCodeList.Add(Self.VM.Binary.Count);
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

  function CreateIdent(const Kind: TSEIdentKind; const Token: TSEToken): TSEIdent; inline;
  begin
    Result.Kind := Kind;
    Result.Ln := Token.Ln;
    Result.Col := Token.Col;
    Result.Addr := Self.LocalVarList.Count - 1;
    Result.Name := Token.Value;
  end;

  function Emit(const Data: array of TSEValue): Integer; inline;
  var
    I: Integer;
  begin
    for I := Low(Data) to High(Data) do
      Self.VM.Binary.Add(Data[I]);
    Exit(Self.VM.Binary.Count);
  end;

  procedure Patch(const Addr: Integer; const Data: TSEValue); inline;
  begin
    Self.VM.Binary[Addr] := Data;
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
  procedure ParseBlock; forward;
  procedure ParseArrayAssign; forward;

  procedure ParseExpr;
  type
    TProc = TSENestedProc;
  var
    ExprStack: TList;
    IsFuncCalled: Boolean = False;

    procedure Logic; forward;

    procedure EmitExpr(const Data: array of TSEValue); inline;
    begin
      ExprStack.Add(Pointer(0));
      Emit(Data);
    end;

    procedure ValidateExpr;
    begin
      //if (ExprStack.Count = 0) and not IsFuncCalled then
      //  Error('Illegal expression', Self.TokenList[Pos]);
    end;

    procedure BinaryOp(const Op: TSEOpcode; const Func: TProc; const IsString: Boolean = False); inline;
    begin
      NextToken;
      if IsString then
        PeekAtNextTokenExpected([tkBracketOpen, tkSquareBracketOpen, tkNumber, tkString, tkNegative, tkIdent])
      else
        PeekAtNextTokenExpected([tkBracketOpen, tkSquareBracketOpen, tkNumber, tkNegative, tkNot, tkIdent]);
      Func;
      EmitExpr([Pointer({$ifdef CPU64}Int64(Op){$else}Op{$endif})]);
    end;

    procedure Tail;
    begin
      case PeekAtNextToken.Kind of
        tkSquareBracketOpen:
          begin
            NextToken;
            ParseExpr;
            NextTokenExpected([tkSquareBracketClose]);
            EmitExpr([Pointer(opPushLocalArrayPop)]);
            Tail;
          end;
      end;
    end;

    procedure Factor;
    var
      Token: TSEToken;
      Ident: PSEIdent;
    begin
      Token := PeekAtNextTokenExpected([
        tkBracketOpen, tkBracketClose, tkSquareBracketOpen, tkNumber, tkEOF,
        tkNegative, tkNot, tkString, tkIdent]);
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
        tkIdent:
          begin
            case IdentifyIdent(Token.Value) of
              tkVariable:
                begin
                  NextToken;
                  Ident := FindVar(Token.Value);
                  Ident^.IsUsed := True;
                  case PeekAtNextToken.Kind of
                    tkSquareBracketOpen:
                      begin
                        NextToken;
                        ParseExpr;
                        NextTokenExpected([tkSquareBracketClose]);
                        EmitExpr([Pointer(opPushLocalArray), Ident^.Addr]);
                        Tail;
                      end;
                    else
                      EmitExpr([Pointer(opPushLocalVar), Ident^.Addr]);
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
                  IsFuncCalled := True;
                  ParseFuncCall(Token.Value);
                  Tail;
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

    procedure Term;
    var
      Token: TSEToken;
    begin
      SignedFactor;
      while True do
      begin
        Token := PeekAtNextToken;
        case Token.Kind of
          tkMul:
            BinaryOp(opOperatorMul, @SignedFactor, True);
          tkDiv:
            BinaryOp(opOperatorDiv, @SignedFactor, True);
          tkMod:
            BinaryOp(opOperatorMod, @SignedFactor, True);
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
            BinaryOp(opOperatorSmaller, @Expr, True);
          tkSmallerOrEqual:
            BinaryOp(opOperatorSmallerOrEqual, @Expr, True);
          tkAnd:
            BinaryOp(opOperatorAnd, @Expr, True);
          tkOr:
            BinaryOp(opOperatorOr, @Expr, True);
          else
            Exit;
        end;
      end;
    end;
  begin
    begin
      ExprStack := TList.Create;
      try
        Logic;
        ValidateExpr;
      finally
        FreeAndNil(ExprStack);
      end;
    end;
  end;

  procedure ParseFuncCall(const Name: String);
  var
    FuncNativeInfo: PSEFuncNativeInfo;
    FuncScriptInfo: PSEFuncScriptInfo;        
    FuncImportInfo: PSEFuncImportInfo;
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
      if PeekAtNextToken.Kind = tkBracketOpen then
      begin
        NextTokenExpected([tkBracketOpen]);
        NextTokenExpected([tkBracketClose]);
      end;
    end;
    if FuncNativeInfo <> nil then
      Emit([Pointer(opCallNative), Pointer(FuncNativeInfo), ArgCount])
    else
    if FuncScriptInfo <> nil then
      Emit([Pointer(opCallScript), Ind, ArgCount])
    else
      Emit([Pointer(opCallImport), Ind]);
  end;

  procedure ParseFuncDecl;
  var
    Token: TSEToken;
    Name: String;
    ArgCount: Integer = 0;
    I,
    JumpBlock,
    Addr, StackAddr: Integer;
    BreakList: TList;
  begin           
    BreakList := TList.Create;
    try            
      BreakStack.Push(BreakList);
      Token := NextTokenExpected([tkIdent]);
      Name := Token.Value;
      if FindFunc(Name) <> nil then
        Error(Format('Duplicate function declaration "%s"', [Token.Value]), Token);

      StackAddr := Self.LocalVarList.Count - 1;

      Token.Value := 'result';
      Token.Kind := tkIdent;
      Self.LocalVarList.Add(CreateIdent(ikVariable, Token));

      if PeekAtNextToken.Kind = tkBracketOpen then
      begin
        NextTokenExpected([tkBracketOpen]);
        repeat
          if PeekAtNextToken.Kind = tkIdent then
          begin
            Token := NextTokenExpected([tkIdent]);
            Self.LocalVarList.Add(CreateIdent(ikVariable, Token));
            Inc(ArgCount);
          end;
          Token := NextTokenExpected([tkComma, tkBracketClose]);
        until Token.Kind = tkBracketClose;
      end;

      JumpBlock := Emit([Pointer(opJumpUnconditional), 0]);
      Addr := JumpBlock;
      ParseBlock;     

      BreakList := BreakStack.Pop;
      for I := 0 to BreakList.Count - 1 do
        Patch(Integer(BreakList[I]), Self.VM.Binary.Count);

      Emit([Pointer(opPushLocalVar), StackAddr]);
      Emit([Pointer(opPopFrame)]);
      Patch(JumpBlock - 1, Self.VM.Binary.Count);

      RegisterScriptFunc(Name, Addr, StackAddr, ArgCount);  
    finally
      BreakList.Free;
    end;
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
          Result := seakChars;
        'wbuffer':
          Result := seakWChars;
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

      if PeekAtNextToken.Kind = tkBracketOpen then
      begin
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
      end;
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
      StartBlock := Self.VM.Binary.Count;
      ParseExpr;
      Emit([Pointer(opPushConst), False]);
      JumpEnd := Emit([Pointer(opJumpEqual), 0]);
      ParseBlock;
      JumpBlock := Emit([Pointer(opJumpUnconditional), 0]);
      EndBlock := Self.VM.Binary.Count;
      ContinueList := ContinueStack.Pop;
      BreakList := BreakStack.Pop;
      for I := 0 to ContinueList.Count - 1 do
        Patch(Integer(ContinueList[I]), StartBlock);
      for I := 0 to BreakList.Count - 1 do
        Patch(Integer(BreakList[I]), EndBlock);
      Patch(JumpBlock - 1, StartBlock);
      Patch(JumpEnd - 1, EndBlock);
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
    VarName: String;
    VarAddr: Integer;
  begin
    ContinueList := TList.Create;
    BreakList := TList.Create;
    try
      ContinueStack.Push(ContinueList);
      BreakStack.Push(BreakList);

      Token := NextTokenExpected([tkVariable, tkIdent]);
      if Token.Kind = tkIdent then
      begin
        Self.LocalVarList.Add(CreateIdent(ikVariable, Token));
      end;
      VarName := Token.Value;
      VarAddr := FindVar(VarName)^.Addr;
      NextTokenExpected([tkEqual]);
      ParseExpr;
      Emit([Pointer(opAssignLocal), VarName, VarAddr]);

      Token := NextTokenExpected([tkTo, tkDownto]);
      StartBlock := Self.VM.Binary.Count; 
      Emit([Pointer(opPushLocalVar), VarAddr]);
      ParseExpr;
      if Token.Kind = tkTo then
      begin
        Emit([Pointer(opPushConst), 1]);
        Emit([Pointer(opOperatorAdd)]);  
        JumpEnd := Emit([Pointer(opJumpEqualOrGreater), 0]);
      end else
      if Token.Kind = tkDownto then
      begin
        Emit([Pointer(opPushConst), 1]);
        Emit([Pointer(opOperatorSub)]);     
        JumpEnd := Emit([Pointer(opJumpEqualOrLesser), 0]);
      end;

      ParseBlock;

      Emit([Pointer(opPushLocalVar), VarAddr]);
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
      Emit([Pointer(opAssignLocal), VarName, VarAddr]);
      JumpBlock := Emit([Pointer(opJumpUnconditional), 0]);
      EndBLock := JumpBlock;

      ContinueList := ContinueStack.Pop;
      BreakList := BreakStack.Pop;
      for I := 0 to ContinueList.Count - 1 do
        Patch(Integer(ContinueList[I]), StartBlock);
      for I := 0 to BreakList.Count - 1 do
        Patch(Integer(BreakList[I]), EndBlock);
      Patch(JumpBlock - 1, StartBlock);
      Patch(JumpEnd - 1, EndBlock);
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
    JumpBlock1 := Emit([Pointer(opJumpEqual), 0]);
    JumpBlock2 := Emit([Pointer(opJumpUnconditional), 0]);
    StartBlock1 := Self.VM.Binary.Count;
    ParseBlock;
    JumpEnd := Emit([Pointer(opJumpUnconditional), 0]);
    StartBlock2 := Self.VM.Binary.Count;
    if PeekAtNextToken.Kind = tkElse then
    begin
      NextToken;
      ParseBlock;
    end;
    EndBlock2 := Self.VM.Binary.Count;
    Patch(JumpBlock1 - 1, StartBlock1);
    Patch(JumpBlock2 - 1, StartBlock2);
    Patch(JumpEnd - 1, EndBlock2);
  end;    

  procedure ParseArrayAssign;
  var
    FuncNativeInfo: PSEFuncNativeInfo;
    I, Ind: Integer;
    ArgCount: Integer = 0;
    Token: TSEToken;
  begin
    FuncNativeInfo := FindFuncNative('array_create', Ind);
    repeat
      if PeekAtNextToken.Kind <> tkSquareBracketClose then
      begin
        ParseExpr;         
        Inc(ArgCount);
      end;
      Token := NextTokenExpected([tkComma, tkSquareBracketClose]);
    until Token.Kind = tkSquareBracketClose;
    Emit([Pointer(opCallNative), Pointer(FuncNativeInfo), ArgCount]);
  end;

  procedure ParseVarAssign(const Name: String);
  var
    Addr: Integer;
    Token: TSEToken;
    IsArrayAssign: Boolean = False;
  begin
    Addr := FindVar(Name)^.Addr;
    case PeekAtNextToken.Kind of
      tkSquareBracketOpen:
        begin
          IsArrayAssign := True;
          NextToken;
          ParseExpr;
          NextTokenExpected([tkSquareBracketClose]);
        end;
    end;
    Token := NextTokenExpected([tkEqual]);
    ParseExpr;
    if IsArrayAssign then
      Emit([Pointer(opAssignLocalArray), Name, Addr])
    else
      Emit([Pointer(opAssignLocal), Name, Addr]);
  end;

  procedure ParseBlock;
  var
    Token: TSEToken;
    Ident: TSEIdent;
    List: TList;
    I: Integer;
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
      tkWhile:
        begin
          NextToken;
          ParseWhile;
        end;
      tkBreak:
        begin
          NextToken;
          if BreakStack.Count = 0 then
            Error('Not in loop but "break" found', Token);
          List := BreakStack.Peek;
          List.Add(Pointer(Emit([Pointer(opJumpUnconditional), 0]) - 1));
        end;
      tkContinue:
        begin
          NextToken;
          if ContinueStack.Count = 0 then
            Error('Not in loop but "continue" found', Token);
          List := ContinueStack.Peek;
          List.Add(Pointer(Emit([Pointer(opJumpUnconditional), 0]) - 1));
        end;
      tkPause:
        begin
          NextToken;
          Emit([Pointer(opPause)]);
        end;
      tkReturn:
        begin                                 
          NextToken;
          if FuncTraversal = 0 then
            Error('Not in a function', Token);
          List := BreakStack.Peek;
          List.Add(Pointer(Emit([Pointer(opJumpUnconditional), 0]) - 1));
        end;
      tkFunctionDecl:
        begin
          Inc(FuncTraversal);
          NextToken;
          Self.ScopeStack.Push(Self.LocalVarList.Count);
          ParseFuncDecl;     
          I := Self.ScopeStack.Pop;
          Self.LocalVarList.DeleteRange(I, Self.LocalVarList.Count - I);
          Dec(FuncTraversal);
        end;
      tkYield:
        begin
          NextToken;
          Emit([Pointer(opYield)]);
        end;
      tkBegin:
        begin
          Self.ScopeStack.Push(Self.LocalVarList.Count);
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
          Self.LocalVarList.DeleteRange(I, Self.LocalVarList.Count - I);
          NextToken;
        end;
      tkIdent:
        begin
          case IdentifyIdent(Token.Value) of
            tkUnknown:
              begin
                NextToken;
                Self.LocalVarList.Add(CreateIdent(ikVariable, Token));
                ParseVarAssign(Token.Value);
              end;
            tkVariable:
              begin
                NextToken;
                ParseVarAssign(Token.Value);
              end;
            tkFunction:
              begin
                NextToken;
                ParseFuncCall(Token.Value);
                Emit([Pointer(opPopConst)]);
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
  try
    repeat
      ParseBlock;
    until PeekAtNextToken.Kind = tkEOF;
    Self.IsParsed := True;
  finally
    FreeAndNil(ContinueStack);
    FreeAndNil(BreakStack);
  end;
end;

procedure TEvilC.Reset;
var
  Ident: TSEIdent;
begin
  Self.FuncScriptList.Clear;   
  Self.FuncImportList.Clear;
  Self.CurrentFileList.Clear;
  Self.VM.Reset;
  Self.VM.Binary.Clear;
  Self.VM.IsDone := True;
  Self.Vm.IsPaused := False;
  Self.IsDone := False;
  Self.IsParsed := False;
  Self.LocalVarList.Clear;
  Self.TokenList.Clear;
  Self.IncludeList.Clear;
  Ident.Kind := ikVariable;
  Ident.Addr := 0;
  Ident.Name := 'result';
  Self.LocalVarList.Add(Ident);
  ErrorLn := -1;
  ErrorCol := -1;
  FuncTraversal := 0;
end;

function TEvilC.Exec: TSEValue;
begin
  if not Self.IsParsed then
  begin
    Self.Lex;
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

procedure TEvilC.RegisterScriptFunc(const Name: String; const Addr, StackAddr, ArgCount: Integer);
var
  FuncScriptInfo: TSEFuncScriptInfo;
begin
  FuncScriptInfo.ArgCount := ArgCount;
  FuncScriptInfo.Addr := Addr;
  FuncScriptInfo.StackAddr := StackAddr;
  FuncScriptInfo.Name := Name;
  Self.FuncScriptList.Add(FuncScriptInfo);
end;

procedure TEvilC.RegisterImportFunc(const Name, ActualName, LibName: String; const Args: TSEAtomKindArray; const Return: TSEAtomKind);
var
  FuncImportInfo: TSEFuncImportInfo;
  Lib: TLibHandle;
begin
  if DynlibMap.ContainsKey(LibName) then
    Lib := DynlibMap[LibName]
  else
    Lib := LoadLibrary(LibName);

  FuncImportInfo.Args := Args;
  FuncImportInfo.Return := Return;
  FuncImportInfo.Name := Name;
  if Lib <> nil then
    FuncImportInfo.Func := GetProcAddress(Lib, ActualName);
  Self.FuncImportList.Add(FuncImportInfo);
end;

function TEvilC.Backup: TSECache;
var
  I: Integer;
begin
  Result.Binary := TSEBinary.Create;
  Result.LineOfCodeList := TIntegerList.Create;
  Result.FuncScriptList := TSEFuncScriptList.Create;     
  Result.FuncImportList := TSEFuncImportList.Create;
  for I := 0 to Self.VM.Binary.Count - 1 do
  begin
    Result.Binary.Add(Self.VM.Binary[I]);
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
  Result.LocalVarListCount := Self.LocalVarList.Count;
end;

procedure TEvilC.Restore(const Cache: TSECache);
var
  I: Integer;
begin
  Self.VM.Binary.Clear;    
  Self.LineOfCodeList.Clear;
  for I := 0 to Cache.LineOfCodeList.Count - 1 do
    Self.LineOfCodeList.Add(Cache.LineOfCodeList[I]);
  for I := 0 to Cache.Binary.Count - 1 do
    Self.VM.Binary.Add(Cache.Binary[I]);               
  for I := 0 to Cache.FuncScriptList.Count - 1 do
    Self.FuncScriptList.Add(Cache.FuncScriptList[I]);
  for I := 0 to Cache.FuncImportList.Count - 1 do
    Self.FuncImportList.Add(Cache.FuncImportList[I]);
  Self.LocalVarList.Count := Cache.LocalVarListCount;
  Self.IsParsed := True;
end;

procedure TSECacheMap.Clear;
var
  S: String;
  Cache: TSECache;
begin
  for S in Self.Keys do
  begin
    Cache := Self[S];
    Cache.Binary.Free;
    Cache.LineOfCodeList.Free;
    Cache.FuncScriptList.Free;
    Cache.FuncImportList.Free;
  end;
  inherited;
end;

initialization
  DynlibMap := TDynlibMap.Create;
  ScriptVarMap := TSEVarMap.Create;

finalization
  FreeAndNil(ScriptVarMap);
  DynlibMap.Free;

end.
