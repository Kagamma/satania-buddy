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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FRichText: TRichText;
    FText: String;
    FNumWordsDisplay: Integer;
    FTypingSpeed: Integer;
    procedure SetText(S: String);
    procedure SetTypingSpeed(S: Integer);
  public
    FinishedTyping: Boolean;
    property Text: String read FText write SetText;
    property TypingSpeed: Integer read FTypingSpeed write SetTypingSpeed;
  end;

var
  FormBubble: TFormBubble;

implementation

uses
  globals,
  Utils.ActiveWindow;

{$R *.lfm}

{ TFormBubble }

procedure TFormBubble.SetTypingSpeed(S: Integer);
begin
  Self.FTypingSpeed := S;
  Self.Timer.Interval := 1000 div S;
end;

procedure TFormBubble.SetText(S: String);
begin
  Self.FText := S;
  if S = '' then
    FinishedTyping := True
  else
  begin
    Self.FNumWordsDisplay := 0;
    Self.FinishedTyping := False;
    Self.KMemo.Blocks.Clear;
    Self.FRichText.Source := S;
    if Length(S) < 100 then
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
    Inc(Self.FNumWordsDisplay);
    Self.FRichText.NextTokenPos := Self.FNumWordsDisplay;
    Self.FRichText.Parse(Self.KMemo);
    if Self.FRichText.TokenList.Count <= Self.FNumWordsDisplay - 1 then
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
end;

procedure TFormBubble.FormDestroy(Sender: TObject);
begin
  Self.FRichText.Free;
end;

end.

