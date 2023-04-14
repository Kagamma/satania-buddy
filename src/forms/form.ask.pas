{

satania-buddy
Copyright (C) 2022-2022 kagamma

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

}

unit form.ask;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  HtmlView, HTMLUn2, HtmlGlobals, Mcdowell.EvilC;

type

  { TFormAsk }

  TFormAsk = class(TForm)
    AskText: THtmlViewer;
    Panel: TPanel;
    Panel1: TPanel;
    Timer: TTimer;
    procedure AskTextFormSubmit(Sender: TObject; const Act, Target, EncType,
      Method: ThtString; Results: ThtStringList);
    procedure AskTextHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: Boolean);
    procedure AskTextImageRequest(Sender: TObject; const SRC: ThtString;
      var Stream: TStream);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PanelPaint(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private

  public
    IsPositionUpdated: Integer;
    IsAnswerText: Boolean;
    AnswerText, Ask: String;
    Answer: TStringList;
  end;

var
  FormAsk: TFormAsk;

implementation

{$R *.lfm}

uses
  form.bubble,
  mcdowell,
  fphttpclient,
  Utils.Threads,
  Utils.ActiveWindow;

{ TFormAsk }

procedure TFormAsk.FormCreate(Sender: TObject);
begin
  AddFormToIgnoreHandleList(Self);
  Answer := TStringList.Create;
  Answer.StrictDelimiter := True;
  Answer.NameValueSeparator := '=';
end;

procedure TFormAsk.FormDestroy(Sender: TObject);
begin
  Answer.Free;
end;

procedure TFormAsk.FormShow(Sender: TObject);
begin
  AskText.Hide;
  IsPositionUpdated := 0;
  Timer.Enabled := True;
end;

procedure TFormAsk.PanelPaint(Sender: TObject);
begin
  Panel.Canvas.Pen.Color := clBlack;
  Panel.Canvas.Rectangle(0, 0, Panel.Width, Panel.Height);
end;

procedure TFormAsk.TimerTimer(Sender: TObject);
begin
  AskText.Show;
  Timer.Enabled := False;
  if Ask <> FormAsk.AskText.Text then
    FormAsk.AskText.Text := Ask;
end;

procedure TFormAsk.AskTextHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: Boolean);
begin
 // Satania.IsBlocked := False;
  Self.Visible := False;
  FormBubble.Text := '';
  IsAnswerText := True;
  AnswerText := SRC;
end;

procedure TFormAsk.AskTextImageRequest(Sender: TObject;
  const SRC: ThtString; var Stream: TStream);
var
  Client: TFPHTTPClient;
begin
  if (String(SRC).IndexOf('http:') >= 0) or (String(SRC).IndexOf('https:') >= 0) then
  begin
    Client := TFPHTTPClient.Create(nil);
    Stream := TMemoryStream.Create;
    try
      try
        Client.Get(SRC, Stream);
      except
        on E: Exception do
        begin
          Satania.Log('System', E.Message);
          FreeAndNil(Stream);
        end;
      end;
    finally
      Client.Free;
    end;
  end else
  begin
    if FileExists(SRC) then
      Stream := TFileStream.Create(SRC, fmOpenRead);
  end;
end;

procedure TFormAsk.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not IsAnswerText then
  begin
    FormBubble.Text := '';
    IsAnswerText := True;
    AnswerText := '_close';
  end;
end;

procedure TFormAsk.AskTextFormSubmit(Sender: TObject; const Act,
  Target, EncType, Method: ThtString; Results: ThtStringList);
var
  I: Integer;
begin
 // Satania.IsBlocked := False;
  Self.Visible := False;
  FormBubble.Text := '';
  Answer.Clear;
  IsAnswerText := False;
  for I := 0 to Results.Count - 1 do
  begin
    Answer.AddDelimitedtext(Results.Names[I] + '=' + Results.ValueFromIndex[I]);
  end;
end;

end.

