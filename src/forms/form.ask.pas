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
    procedure AskTextFormSubmit(Sender: TObject; const Act, Target, EncType,
      Method: ThtString; Results: ThtStringList);
    procedure AskTextHotSpotClick(Sender: TObject; const SRC: ThtString;
      var Handled: Boolean);
    procedure AskTextImageRequest(Sender: TObject; const SRC: ThtString;
      var Stream: TStream);
    procedure FormCreate(Sender: TObject);
    procedure PanelPaint(Sender: TObject);
  private

  public
    Answer: TSEValue;
  end;

var
  FormAsk: TFormAsk;

implementation

{$R *.lfm}

uses
  mcdowell,
  fphttpclient,
  Utils.Threads,
  Utils.ActiveWindow;

{ TFormAsk }

procedure TFormAsk.FormCreate(Sender: TObject);
begin
  AddFormToIgnoreHandleList(Self);
end;

procedure TFormAsk.PanelPaint(Sender: TObject);
begin
  Panel.Canvas.Pen.Color := clBlack;
  Panel.Canvas.Rectangle(0, 0, Panel.Width, Panel.Height);
end;

procedure TFormAsk.AskTextHotSpotClick(Sender: TObject;
  const SRC: ThtString; var Handled: Boolean);
begin
  Satania.IsBlocked := False;
  Self.Visible := False;
  Satania.ChatText.Text.Text := '';
  Answer := SRC;
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
          Satania.TalkReset(E.Message);
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

procedure TFormAsk.AskTextFormSubmit(Sender: TObject; const Act,
  Target, EncType, Method: ThtString; Results: ThtStringList);
var
  I: Integer;
begin
  Satania.IsBlocked := False;
  Self.Visible := False;
  Satania.ChatText.Text.Text := '';
  Answer.Kind := sevkArray;
  SetLength(Answer.VarArray, Results.Count);
  for I := 0 to Results.Count - 1 do
    Answer.VarArray[I] := Results[I];
end;

end.

