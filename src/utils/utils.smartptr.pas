unit utils.smartptr;

{$mode Delphi}{$H+}

interface

uses SysUtils;

type
  TSmartPtr<T: class> = record
  public type
    SelfT = TSmartPtr<T>;
    SelfP = ^SelfT;
  strict private
    FClassInstance: T;
    FCriticalSection: TRTLCriticalSection;
  private
    class operator Initialize(var Val: SelfT); inline;
    class operator Finalize(var Val: SelfT); inline;
    class operator Copy(constref Left: SelfT; var Right: SelfT); inline;
  public
    function Lock: SelfP; inline;
    function Unlock: SelfP; inline;
    property Obj: T read FClassInstance;
  end;

implementation

class operator TSmartPtr<T>.Initialize(var Val: SelfT);
begin
  with Val do
  begin
    FClassInstance := T.Create();
    InitCriticalSection(FCriticalSection);
  end;
end;

class operator TSmartPtr<T>.Finalize(var Val: SelfT);
begin
  with Val do
  begin
    FClassInstance.Free();
    LeaveCriticalSection(FCriticalSection);
    DoneCriticalSection(FCriticalSection);
  end;
end;

class operator TSmartPtr<T>.Copy(constref Left: SelfT; var Right: SelfT);
begin
end;

function TSmartPtr<T>.Lock: SelfP;
begin
  EnterCriticalSection(FCriticalSection);
  Result := @Self;
end;

function TSmartPtr<T>.Unlock: SelfP;
begin
  LeaveCriticalSection(FCriticalSection);
  Result := @Self;
end;

end.

