{$mode ObjFPC}{$H+}
{$ifdef LINUX}
  {$define LINUX_X11}
{$endif}
{$ifdef CPUX86_64}
  {$asmmode intel}
{$endif}
{$macro on}
{$define nl:=+ LineEnding +}
