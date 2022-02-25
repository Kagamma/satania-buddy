{$if defined(LINUX_X11)}

{$ifdef unit_declare_uses}
  X, Xlib, xatom, gtk2, qt5, qtwidgets,
{$endif unit_declare_uses}

{$ifdef unit_declare_interface}
{$endif}

{$ifdef unit_implmentation}

procedure TFormMain.FormCreate(Sender: TObject);
var
  Attrib: LongInt;
begin
  XDisplay := QX11Info_display();
  {TQtWidget(TQtMainWindow(Handle).Widget).setAttribute(QtWA_TranslucentBackground);
  TQtWidget(TQtMainWindow(Handle).Widget).setAttribute(QtWA_NoSystemBackground);
  TQtWidget(TQtMainWindow(SataniaDisplay.Handle).Widget).setAttribute(QtWA_TranslucentBackground);
  TQtWidget(TQtMainWindow(SataniaDisplay.Handle).Widget).setAttribute(128);
  // TQtWidget(TQtMainWindow(Handle).Widget).setWindowFlags(QtWindowTransparentForInput or QtFramelessWindowHint);}
  // TODO: Qt5 event pass-through
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
end;

{$endif unit_implmentation}

{$endif}

