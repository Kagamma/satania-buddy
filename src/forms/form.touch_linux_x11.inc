{$if defined(LINUX_X11)}

{$ifdef unit_declare_interface}
  qt5, qtwidgets, xlib, xatom, x,
{$endif unit_declare_interface}

{$ifdef unit_implmentation}
procedure TFormTouch.TimerTimer(Sender: TObject);
var
  Data: PCardinal;
  A, B, C, D: Cardinal;
begin
  // Try to raise window when virtual desktop's changed
  if XGetWindowProperty(
        XDisplay, XDefaultRootWindow(XDisplay),
        XInternAtom(XDisplay, '_NET_CURRENT_DESKTOP', False),
        0, 1, False, XA_CARDINAL, @A, @B, @C, @D, @Data
    ) = Success then
  begin
    if Data^ <> LastVirtualDesktop then
    begin
      LastVirtualDesktop := Data^;
      Hide;
      Show;
    end;
    XFree(Data);
  end;
end;
{$endif}

{$endif}

