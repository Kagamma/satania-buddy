{

satania-buddy
Copyright (C) 2022-2023 kagamma

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

unit Form.Bubble;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, kmemo,
  {$define unit_declare_interface}
  {$I form.bubble_linux_x11.inc}
  {$I form.bubble_windows.inc}
  {$undef unit_declare_interface}
  Mcdowell.RichText;

type

  { TFormBubble }

  TFormBubble = class(TForm)
    KMemo: TKMemo;
    Panel1: TPanel;
    Panel: TPanel;
    Timer: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PanelPaint(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FRichText: TSataniaRichText;
    FIsStartTalking: Boolean;
    FText: String;
    FNumWordsDisplay: Single;
    FTypingSpeed: Single;
    FVisibleViaSize: Boolean;
    procedure CreateCommons;
    procedure SetText(S: String);
    procedure SetTypingSpeed(S: Single);
    procedure SetVisibleViaSize(B: Boolean);
  public
    IsPersistent: Boolean;
    FinishedTyping: Boolean;
    procedure EnableStreaming;
    procedure DisableStreaming;
    procedure Streaming(S: String);
    procedure ApplySettings;
    property Text: String read FText write SetText;
    property TypingSpeed: Single read FTypingSpeed write SetTypingSpeed;
    property VisibleViaSize: Boolean read FVisibleViaSize write SetVisibleViaSize;
  end;

var
  FormBubble: TFormBubble;

implementation

uses
  globals,
  Mcdowell,
  Form.chat,
  State.Main,
  Utils.ActiveWindow;

{$R *.lfm}

{ TFormBubble }

{$define unit_implmentation}
{$I form.bubble_linux_x11.inc}
{$I form.bubble_windows.inc}
{$undef unit_implmentation}

procedure TFormBubble.SetVisibleViaSize(B: Boolean);
begin
  if Self.FVisibleViaSize <> B then
    if B then
    begin
      if Self.Width = 1 then
        Self.Width := Save.Settings.ChatBubbleSizeX + 32;
      if Self.Height = 1 then
        Self.Height := Save.Settings.ChatBubbleSizeY + 32;
      AlphaBlendValue := 255;
    end else
    begin
      Self.Width := 1;
      Self.Height := 1; 
      AlphaBlendValue := 1;
    end;
  Self.FVisibleViaSize := B;
end;

procedure TFormBubble.EnableStreaming;
begin
  Self.IsPersistent := True;
  Self.FinishedTyping := False;
  Self.FText := '';
end;

procedure TFormBubble.DisableStreaming;
begin
  Self.IsPersistent := False;
  Satania.StopAnimation(Satania.AnimTalkFinish);
  Self.FIsStartTalking := False;
end;

procedure TFormBubble.Streaming(S: String);
begin
  if Self.FVisibleViaSize then
  begin
    Self.Width := Save.Settings.ChatBubbleSizeX;
    Self.Height := Save.Settings.ChatBubbleSizeY;
  end;
  if (Self.FText = '') and (Satania.AnimTalkLoop <> '') then
  begin
    Self.KMemo.Blocks.Clear;
    Satania.ChatBubbleDelay := Save.Settings.ChatBubbleDelay;
    Self.FRichText.Reset;
    Self.FNumWordsDisplay := 0;
  end;
  Satania.IsTalking := True;
  Satania.IsAsking := False;
  Self.FText := S;
  Self.FRichText.Source := S;
end;

procedure TFormBubble.SetTypingSpeed(S: Single);
begin
  Self.FTypingSpeed := S;
  Self.Timer.Interval := 48;
end;

procedure TFormBubble.SetText(S: String);
begin
  Self.DisableStreaming;
  Self.FText := S;
  if S = '' then
    FinishedTyping := True
  else
  begin
    Self.FNumWordsDisplay := 0;
    Self.FinishedTyping := False;
    Self.KMemo.Blocks.Clear;
    Self.FRichText.Reset;
    Self.FRichText.Source := S;
    if Self.FVisibleViaSize then
      {if (Length(S) < 100) and not (Self.IsPersistent) then
      begin
        Self.Width := 200;
        Self.Height := 100;
      end else}
      begin
        Self.Width := Save.Settings.ChatBubbleSizeX;
        Self.Height := Save.Settings.ChatBubbleSizeY;
      end;
  end;
end;

procedure TFormBubble.TimerTimer(Sender: TObject);
  procedure ScrollToBottom;
  begin
    KMemo.SelStart := KMemo.GetTextLen;
    KMemo.SelLength := 0;
    // 999999 should be more than enough to scroll it to the bottom
    KMemo.ScrollBy(0, 999999, False);
    KMemo.Refresh;
  end;
begin
  if (not Self.FinishedTyping) and (not Satania.IsAsking) then
  begin
    if Self.FRichText.TokenList.Count > Self.FNumWordsDisplay - 1 then
      Self.FNumWordsDisplay := Self.FNumWordsDisplay + Self.FTypingSpeed / 20;
    Self.FRichText.NextTokenPos := Round(Self.FNumWordsDisplay);
    Self.FRichText.Parse(Self.KMemo);
    if (Self.FRichText.TokenList.Count <= Self.FNumWordsDisplay - 1) and not Self.IsPersistent then
    begin
      Self.FinishedTyping := True;
    end;
    if (Self.FRichText.TokenList.Count <= Self.FNumWordsDisplay - 1)  then
    begin
      Satania.StopAnimation(Satania.AnimTalkLoop);
      Satania.StartAnimation(Satania.AnimTalkFinish, False);
    end else
    if not Self.FIsStartTalking then
    begin
      Self.FIsStartTalking := True;
      Satania.StopAnimation(Satania.AnimTalkFinish);
      Satania.StartAnimation(Satania.AnimTalkLoop);
    end;
    ScrollToBottom;
  end;
end;

procedure TFormBubble.CreateCommons;
begin
  Self.FRichText := TSataniaRichText.Create;
  Self.FRichText.IsStreaming := True;
  Self.FRichText.IsPerformance := False;
  AddFormToIgnoreHandleList(Self);
  Self.TypingSpeed := Save.Settings.TextSpeed;
  Self.Width := 1;
  Self.Height := 1;
  AlphaBlend := True;
  AlphaBlendValue := 1;
  ApplySettings;
end;

procedure TFormBubble.FormActivate(Sender: TObject);
begin
  ActiveControl := nil;
end;

procedure TFormBubble.FormDestroy(Sender: TObject);
begin
  Self.FRichText.Free;
end;

procedure TFormBubble.FormShow(Sender: TObject);
begin
  BringToFront;
end;

procedure TFormBubble.PanelPaint(Sender: TObject);
var
  C: TCanvas;
  P: array[0..2] of TPoint;
begin
  inherited;
  C := Panel.Canvas;
  C.Pen.Width := 1;
  C.Pen.Color := clBlack;
  C.Brush.Color := clWhite;
  C.RoundRect(8, 8, Panel.Width - 8, Panel.Height - 8, 16, 16);
  if StateMain = nil then
    Exit;
  if StateMain.BubbleSideY = 0 then
  begin
    if StateMain.BubbleSideX = 0 then
    begin
      P[0].X := Panel.Width - 38; P[0].Y := Panel.Height - 9;
      P[1].X := Panel.Width - 18; P[1].Y := Panel.Height - 9;
      P[2].X := Panel.Width -  8; P[2].Y := Panel.Height;
    end else
    begin
      P[0].X := 18; P[0].Y := Panel.Height - 9;
      P[1].X := 38; P[1].Y := Panel.Height - 9;
      P[2].X :=  8; P[2].Y := Panel.Height;
    end;
  end else
  begin
    if StateMain.BubbleSideX = 0 then
    begin
      P[0].X := Panel.Width - 38; P[0].Y := 9;
      P[1].X := Panel.Width - 18; P[1].Y := 9;
      P[2].X := Panel.Width -  8; P[2].Y := 0;
    end else
    begin
      P[0].X := 18; P[0].Y := 9;
      P[1].X := 38; P[1].Y := 9;
      P[2].X :=  8; P[2].Y := 0;
    end;
  end;
  C.Pen.Color := clBlack;
  C.Polygon(P);
  C.Pen.Color := clWhite;
  P[0].X := P[0].X + 1;
  P[1].X := P[1].X - 1;
  C.Line(P[0], P[1]);
end;

procedure TFormBubble.ApplySettings;
begin
  KMemo.Font.Name := Save.Settings.ChatBubbleFont;
  KMemo.Font.Size := Save.Settings.ChatBubbleFontSize;
  if Save.Settings.ChatBubbleClearType then
    KMemo.Font.Quality := fqCleartype
  else
    KMemo.Font.Quality := fqDefault;
end;

end.

