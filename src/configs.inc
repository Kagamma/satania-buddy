{$mode ObjFPC}{$H+}
{$ifdef LINUX}
  {$define LINUX_X11}
{$endif}
{$ifdef CPUX86_64}
  {$asmmode intel}
{$endif}
{$macro on}
{$define nl:=+ LineEnding +}
{$ifdef SATANIA_UI_QT6}
  {$define sataniaqt:= qt6}
{$else}
  {$define sataniaqt:= qt5}
{$endif}
