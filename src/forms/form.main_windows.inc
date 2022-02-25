{$if defined(WINDOWS)}

{$ifdef unit_declare_uses}
  Windows, DwmApi, UxTheme,
{$endif unit_declare_uses}

{$ifdef unit_public}
    procedure CreateParams(var Params: TCreateParams); override;
{$endif unit_public}

{$ifdef unit_implmentation}

procedure TFormMain.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_TRANSPARENT;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  Attrib: LongInt;
  B: TDWMBlurBehind;
begin
  Color := clFuchsia;
  Attrib := GetWindowLongA(Handle, GWL_EXSTYLE);
  SetWindowLongA(Handle, GWL_EXSTYLE, Attrib or WS_EX_LAYERED);
  SetLayeredWindowAttributes(Handle, Color, 0, LWA_COLORKEY);
  InitDwmLibrary;
  DwmCompositionEnabled;
  B.dwFlags := DWM_BB_ENABLE or DWM_BB_BLURREGION;
  B.fEnable := true;
  B.hRgnBlur := CreateRectRgn(0, 0, 1, 1);
  B.fTransitionOnMaximized := true;
  DwmEnableBlurBehindWindow(Handle, @B);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
end;

{$endif unit_implmentation}

{$endif}


