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
{$ifdef LINUX_X11}
  X, Xlib, xatom, qt5, qtwidgets,
{$endif}
  Mcdowell.RichText;

type

  { TFormBubble }

  TFormBubble = class(TForm)
    KMemo: TKMemo;
    Timer: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FRichText: TSataniaRichText;
    FText: String;
    FNumWordsDisplay: Integer;
    FTypingSpeed: Integer;
    FVisibleViaSize: Boolean;
    procedure SetText(S: String);
    procedure SetTypingSpeed(S: Integer);
    procedure SetVisibleViaSize(B: Boolean);
  public
    IsPersistent: Boolean;
    FinishedTyping: Boolean;
    procedure EnableStreaming;
    procedure DisableStreaming;
    procedure Streaming(S: String);
    procedure ApplySettings;
    property Text: String read FText write SetText;
    property TypingSpeed: Integer read FTypingSpeed write SetTypingSpeed;
    property VisibleViaSize: Boolean read FVisibleViaSize write SetVisibleViaSize;
  end;

var
  FormBubble: TFormBubble;

implementation

uses
  globals,
  Mcdowell,
  Form.chat,
  Utils.ActiveWindow;

{$R *.lfm}

{ TFormBubble }

procedure TFormBubble.SetVisibleViaSize(B: Boolean);
begin
  if Self.FVisibleViaSize <> B then
    if B then
    begin
      if Self.Width = 1 then
        Self.Width := Save.Settings.ChatBubbleSizeX;
      if Self.Height = 1 then
        Self.Height := Save.Settings.ChatBubbleSizeY;
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

procedure TFormBubble.SetTypingSpeed(S: Integer);
begin
  Self.FTypingSpeed := S;
  Self.Timer.Interval := 1000 div S;
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
      Inc(Self.FNumWordsDisplay);
    Self.FRichText.NextTokenPos := Self.FNumWordsDisplay;
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
      Satania.StartAnimation(Satania.AnimTalkLoop);
    ScrollToBottom;
    Self.Timer.Interval := 1000 div Self.FTypingSpeed;
  end;
end;

procedure TFormBubble.FormCreate(Sender: TObject);
begin
  {$ifdef LINUX_X11}
  Self.Enabled := False;       
  Self.KMemo.Enabled := False;
  TQtWidget(TQtMainWindow(Handle).Widget).setFocusPolicy(QtNoFocus);
  // TQtWidget(TQtMainWindow(Handle).Widget).setAttribute(QtWA_ShowWithoutActivating);
  {$endif}
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

