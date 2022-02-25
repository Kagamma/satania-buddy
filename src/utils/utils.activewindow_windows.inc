{$ifdef WINDOWS}

{$ifdef unit_declare_interface}
  Windows,
{$endif unit_declare_interface}

{$ifdef unit_implmentation}

var
  PreviousWindow: THandle;

constructor TUtilActiveWindow.Create;
begin
  inherited;
end;

function TUtilActiveWindow.LookForSuitablePosition: TPoint;
var
  Window: THandle;
  Rect: TRect;
begin
  IsWindowChanged := False;
  Result := Point(-999999, -999999);
  Window := GetForegroundWindow;
  if IgnoreHandleList.Contains(QWord(Window)) then
    Exit;   
  // A bit delay so that GetForegroundWindow has a chance to update
  if PreviousWindow <> Window then
  begin
    PreviousWindow := Window;
    IsWindowChanged := True;
    Exit;
  end;
  if Window <> Satania.Form.Handle then
  begin
    GetWindowRect(Window, Rect);
    Result := Point(Max(Rect.Location.X, Rect.Location.X + Rect.Width - Save.Settings.SitOnWindowRightMargin), Rect.Location.Y);
  end;
end;

{$endif}
{$endif}
