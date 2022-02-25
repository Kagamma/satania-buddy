{$if defined(LINUX_X11)}

{$ifdef unit_declare_interface}
  X, Xlib, glx, qt5,
{$endif unit_declare_interface}

{$ifdef unit_public}
{$endif unit_public}

{$ifdef unit_implmentation}

var
  PreviousWindow: TWindow;

constructor TUtilActiveWindow.Create;
begin
  inherited;
end;

function TUtilActiveWindow.LookForSuitablePosition: TPoint;
var
  Window: TWindow;
  Attrib: TXWindowAttributes;
  I: Integer;
begin
  IsWindowChanged := False;
  Result := Point(-999999, -999999);
  if IgnoreHandleList.Contains(QWord(GetForegroundWindow)) then
    Exit;
  XGetInputFocus(XDisplay, @Window, @I);
  XGetWindowAttributes(XDisplay, Window, @Attrib);
  // Find the actual top window
  Window := FindTopWindow(Window);
  // A bit delay so that GetForegroundWindow has a chance to update
  if PreviousWindow <> Window then
  begin
    PreviousWindow := Window;
    IsWindowChanged := True;
    Exit;
  end;
  XGetWindowAttributes(XDisplay, Window, @Attrib);
  Result := Point(Max(Attrib.X, Attrib.X + Attrib.Width - Save.Settings.SitOnWindowRightMargin), Attrib.Y);
end;

{$endif unit_implmentation}

{$endif LINUX}

