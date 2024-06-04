program ffisample;

{$mode objfpc}{$H+}

uses
  cthreads,
  FFI;

var
  status: ffi_status;
  cif: ffi_cif;
  args: array[0..0] of pffi_type;
  values: array[0..0] of Pointer;
  s: PChar;
  rc: ffi_arg;

procedure Puts(S: PChar);
begin
  Writeln('Calling');
  Writeln('Puts: ', S);
end;

begin
  args[0] := @ffi_type_pointer;
  Writeln('FFI Sample Start');

  status := ffi_prep_cif(@cif, ffi_abi(2), 1, @ffi_type_sint, @args);
  Writeln('status: ', status);

  //if status = FFI_OK then
  begin
    s := 'Hello World!';
    values[0] := @s;
    ffi_call(@cif, ffi_fn(@Puts), @rc, @values);

    s := 'This is cool!';
    ffi_call(@cif, ffi_fn(@Puts), @rc, @values);
  end;

  Writeln('FFI Sample End');
end.

