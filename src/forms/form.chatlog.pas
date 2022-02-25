unit form.chatlog;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons;

type

  { TFormChatLog }

  TFormChatLog = class(TForm)
    BttonClear: TBitBtn;
    MemoChatLog: TMemo;
    Panel1: TPanel;
    procedure BttonClearClick(Sender: TObject);
  private

  public

  end;

var
  FormChatLog: TFormChatLog;

implementation

{$R *.lfm}

{ TFormChatLog }

procedure TFormChatLog.BttonClearClick(Sender: TObject);
begin
  MemoChatLog.Lines.Clear;
end;

end.

