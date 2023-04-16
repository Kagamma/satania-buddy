unit Mcdowell.RichText;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, kmemo;

type
  TRichTextKind = (rtkText, rtkNewLine, rtkState, rtkEOS);
  TRichTextState = (rtsNormal, rtsCode);

  TRichTextToken = record
    Kind : TRichTextKind;
    State: TRichTextState;
    Value: Char;
  end;
  TRichTextTokenList = specialize TList<TRichTextToken>;

  TRichText = class
  private
    FSource: String;
    FIsLexed: Boolean;
    FState: TRichTextState;
    procedure SetSource(S: String);
  public
    IsStreaming: Boolean;
    LastTokenPos,
    NextTokenPos: Integer;
    LastState: TRichTextState;
    LastKind: TRichTextKind;
    TokenList: TRichTextTokenList;
    constructor Create;
    destructor Destroy; override;
    procedure Lex;
    procedure Parse(const Memo: TKMemo);
    procedure Reset;

    property Source: String read FSource write SetSource;
  end;

implementation

uses
  Math;

procedure TRichText.SetSource(S: String);
begin
  if S <> Self.FSource then
  begin
    Self.FSource := S;
    Self.FIsLexed := False;
  end;
end;

procedure TRichText.Reset;
begin
  Self.LastTokenPos := 0;
  Self.NextTokenPos := 0;
  Self.FState := rtsNormal;
  Self.LastKind := rtkEOS;
  Self.LastState := rtsNormal;
end;

constructor TRichText.Create;
begin
  inherited;
  Self.TokenList := TRichTextTokenList.Create;
end;

procedure TRichText.Lex;
var
  Ln : Integer = 1;
  Col: Integer = 1;
  Pos: Integer = 0;

  function PeekAtNextChar(const Ind: Integer = 1): Char; inline;
  var
    P: Integer;
  begin
    P := Pos + Ind;
    if P > Length(Self.Source) then
      Exit(#0);
    Exit(Self.Source[P]);
  end;

  function NextChar: Char; inline;
  begin
    Inc(Pos);
    Inc(Col);
    if Pos > Length(Self.Source) then
      Exit(#0);
    if Self.Source[Pos] = #10 then
    begin
      Inc(Ln);
      Col := 1;
    end;
    Exit(Self.Source[Pos]);
  end;

label
  LB_Other;

var
  C    : Char;
  Token: TRichTextToken;

begin
  Self.TokenList.Clear;
  repeat
    C := NextChar;
    case C of
      #0:
        begin
          Token.Kind := rtkEOS;
        end;
      #10:
        begin
          Token.Kind := rtkNewLine;
        end;
      '`':
        begin
          if (Col = 2) and (PeekAtNextChar = '`') and (PeekAtNextChar(2) = '`') then
          begin
            repeat
              C := NextChar;
            until (C = #10) or (C = #0);
            if Self.FState = rtsNormal then
              Self.FState := rtsCode
            else
              Self.FState := rtsNormal;
            Token.Kind := rtkState;
            Token.State := Self.FState;
          end else
            goto LB_Other;
        end;
      else
        LB_Other:
        begin
          Token.Value := C;
          Token.Kind := rtkText;
        end;
    end;
    Self.TokenList.Add(Token);
  until C = #0;
  Self.FIsLexed := True;
end;

procedure TRichText.Parse(const Memo: TKMemo);
var
  I, IMin, IMax: Integer;
  Token: TRichTextToken;
  TB: TKMemoTextBlock;
begin
  if (not Self.FIsLexed) or (Self.TokenList.Count = 0) then
    Self.Lex;
  if not Self.IsStreaming then
  begin
    IMin := 0;
    IMax := Self.TokenList.Count - 1
  end else
  begin
    IMin := Self.LastTokenPos;
    IMax := Min(Self.NextTokenPos, Self.TokenList.Count - 1);
  end;
  for I := IMin to IMax do
  begin
    Token := Self.TokenList[I];
    case Token.Kind of
      rtkText:
        begin
          if (Self.LastKind = Token.Kind) and (Memo.Blocks.Count > 0) then
          begin
            TB := TKMemoTextBlock(Memo.Blocks[Memo.Blocks.Count - 1]);
            TB.Text := TB.Text + Token.Value;
          end else
            TB := Memo.Blocks.AddTextBlock(Token.Value);
          case Self.LastState of
            rtsCode:
              begin
                TB.TextStyle.Font.Color := $303030;
                {$ifdef WINDOWS}
                TB.TextStyle.Font.Name := 'Consolas';
                {$else}
                TB.TextStyle.Font.Name := 'Monospace';
                {$endif}
              end;
            else
              begin
                // Do nothing
              end;
          end;
        end;
      rtkNewLine:
        begin
          Memo.Blocks.AddParagraph;
        end;
      rtkState:
        begin
          Self.LastState := Token.State;
        end;
      rtkEOS:
        Exit;
    end;
    Self.LastKind := Token.Kind;
    Self.LastTokenPos := I + 1;
  end;
end;

destructor TRichText.Destroy;
begin
  Self.TokenList.Free;
  inherited;
end;

end.

