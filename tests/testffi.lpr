library testffi;

{$mode objfpc}{$H+}

procedure TestString(S: PChar);
begin
  Writeln('TestString: ', S);
end;

function TestInt(A, B: Integer): Integer;
begin
  Result := A + B;
end;

function TestFloat(A, B: Single): Single;
begin
  Result := A + B;
end;  

function TestDouble(A, B: Double): Double;
begin
  Result := A + B;
end;

exports
  TestInt,
  TestFloat,
  TestDouble,
  TestString;

begin
end.

