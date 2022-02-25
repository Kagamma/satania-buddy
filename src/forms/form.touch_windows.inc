{$if defined(WINDOWS)}

{$ifdef unit_declare_interface}
{$endif unit_declare_interface}

{$ifdef unit_implmentation}
procedure TFormTouch.TimerTimer(Sender: TObject);
begin
  Timer.Enabled := False;
end;
{$endif}

{$endif}


