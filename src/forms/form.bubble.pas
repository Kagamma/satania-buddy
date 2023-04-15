unit Form.Bubble;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, kmemo,
  Mcdowell.RichText;

type

  { TFormBubble }

  TFormBubble = class(TForm)
    KMemo: TKMemo;
    Timer: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FRichText: TRichText;
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
      if Self.Width = 0 then
        Self.Width := 300;
      if Self.Height = 0 then
        Self.Height := 200;
    end else
    begin
      Self.Width := 0;
      Self.Height := 0;
    end;
  Self.FVisibleViaSize := B;
end;

procedure TFormBubble.EnableStreaming;
begin
  Self.IsPersistent := True;
  Self.FinishedTyping := False;
  Self.FNumWordsDisplay := 0;
  Self.FText := '';
end;

procedure TFormBubble.DisableStreaming;
begin
  Self.IsPersistent := False;
end;

procedure TFormBubble.Streaming(S: String);
begin
  Self.Width := 300;
  Self.Height := 200;
  if (Self.FText = '') and (Satania.AnimTalkLoop <> '') then
  begin
    Satania.StartAnimation(Satania.AnimTalkLoop);
    Self.KMemo.Blocks.Clear;
    Satania.ChatBubbleDelay := Save.Settings.ChatBubbleDelay;
    Self.FRichText.Reset;
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
    if (Length(S) < 100) and not (Self.IsPersistent) then
    begin
      Self.Width := 200;
      Self.Height := 100;
    end else
    begin
      Self.Width := 300;
      Self.Height := 200;
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
  if not Self.FinishedTyping then
  begin
    if Self.FRichText.TokenList.Count > Self.FNumWordsDisplay - 1 then
      Inc(Self.FNumWordsDisplay);
    Self.FRichText.NextTokenPos := Self.FNumWordsDisplay;
    Self.FRichText.Parse(Self.KMemo);
    if (Self.FRichText.TokenList.Count <= Self.FNumWordsDisplay - 1) and not Self.IsPersistent then
      Self.FinishedTyping := True;
    ScrollToBottom;
  end;
end;

procedure TFormBubble.FormCreate(Sender: TObject);
begin
  Self.FRichText := TRichText.Create;
  Self.FRichText.IsStreaming := True;
  AddFormToIgnoreHandleList(Self);
  Self.KMemo.TextStyle.Font.Size := Save.Settings.FontSize;
  Self.TypingSpeed := Save.Settings.TextSpeed;
  Self.Width := 0;
  Self.Height := 0;
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
  if FormChat.Visible then
    FormChat.MemoChatLog.SetFocus;
end;

end.

