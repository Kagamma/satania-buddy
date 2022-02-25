unit form.chat;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, Process, CastleControls, CastleUIControls,
  CastleURIUtils, LCLTranslator;

type

  { TFormChat }

  TFormChat = class(TForm)
    BttonClear: TBitBtn;
    ButtonHistory: TBitBtn;
    EditChat: TEdit;
    MemoChatLog: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelChatlog: TPanel;
    procedure BttonClearClick(Sender: TObject);
    procedure ButtonHistoryClick(Sender: TObject);
    procedure EditChatKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
  private
    IsShowHistory: Boolean;
  public

  end;

var
  FormChat: TFormChat;

implementation

{$R *.lfm}

uses
  Globals,
  Mcdowell;

{ TFormChat }

procedure TFormChat.EditChatKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
  begin       
    Satania.Log('(you)', FormChat.EditChat.Text);
    Satania.Chat(EditChat.Text);
    EditChat.Text := '';
  end;
  if Key = #27 then
  begin
    Hide;
  end;
end;

procedure TFormChat.FormCreate(Sender: TObject);
begin
  Height := 45; 
  PanelChatlog.Visible := IsShowHistory;
end;

procedure TFormChat.ButtonHistoryClick(Sender: TObject);
begin
  IsShowHistory := not IsShowHistory;
  PanelChatlog.Visible := IsShowHistory;
  if IsShowHistory then
  begin
    Height := 320;
  end else
  begin
    Height := 45;
  end;
end;

procedure TFormChat.BttonClearClick(Sender: TObject);
begin
  MemoChatLog.Lines.Clear;
end;

end.

