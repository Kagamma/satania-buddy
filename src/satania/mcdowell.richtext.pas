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

unit Mcdowell.RichText;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, kmemo, Graphics;

type
  TRichTextKind = (rtkText, rtkNewLine, rtkState, rtkEOS);
  TRichTextState = (rtsNormal, rtsCode, rtsThink);

  TRichTextToken = record
    Kind : TRichTextKind;
    State: TRichTextState;
    Value: Char;
    Pos  : Integer;        // Position in original string
  end;
  TRichTextTokenList = specialize TList<TRichTextToken>;

  TSataniaRichText = class
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
    IsPerformance: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Lex(const IsEmote: Boolean = True);
    procedure Parse(const Memo: TKMemo; const IsEmote: Boolean = True);
    procedure Reset;

    property Source: String read FSource write SetSource;
  end;

implementation

uses
  Globals,
  Math,
  Utils.Colors;

procedure TSataniaRichText.SetSource(S: String);
begin
  if S <> Self.FSource then
  begin
    Self.FSource := S;
    Self.FIsLexed := False;
  end;
end;

procedure TSataniaRichText.Reset;
begin
  Self.LastTokenPos := 0;
  Self.NextTokenPos := 0;
  Self.FState := rtsNormal;
  Self.LastKind := rtkEOS;
  Self.LastState := rtsNormal;
end;

constructor TSataniaRichText.Create;
begin
  inherited;
  Self.TokenList := TRichTextTokenList.Create;
  Self.IsPerformance := True;
end;

procedure TSataniaRichText.Lex(const IsEmote: Boolean = True);
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
  C,
  Bracket: Char;
  Token: TRichTextToken;

begin
  Self.TokenList.Clear;
  Self.FState := rtsNormal;
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
      '(':
        begin
          if IsEmote and (Self.FState = rtsNormal) and (PeekAtNextChar <> ' ') then
          begin
            Self.FState := rtsThink;
            Token.Kind := rtkState;
            Token.State := Self.FState;
            Bracket := '(';
          end else
            goto LB_Other;
        end;
      ')':
        begin
          if IsEmote and (Self.FState = rtsThink) and (Bracket = '(') then
          begin
            Self.FState := rtsNormal;
            Token.Kind := rtkState;
            Token.State := Self.FState;
          end else
            goto LB_Other;
        end;
      '[':
        begin
          if IsEmote and (Self.FState = rtsNormal) and (PeekAtNextChar <> ' ') then
          begin
            Self.FState := rtsThink;
            Token.Kind := rtkState;
            Token.State := Self.FState;
            Bracket := '[';
          end else
            goto LB_Other;
        end;
      ']':
        begin
          if IsEmote and (Self.FState = rtsThink) and (Bracket = '[') then
          begin
            Self.FState := rtsNormal;
            Token.Kind := rtkState;
            Token.State := Self.FState;
          end else
            goto LB_Other;
        end;
      '*':
        begin
          if IsEmote and (((Self.FState = rtsNormal) and (PeekAtNextChar <> ' ')) or (Self.FState = rtsThink)) then
          begin
            if Self.FState = rtsNormal then
              Self.FState := rtsThink
            else
              Self.FState := rtsNormal;
            Token.Kind := rtkState;
            Token.State := Self.FState;
          end else
            goto LB_Other;
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
    Token.Pos := Pos;
    Self.TokenList.Add(Token);
  until C = #0;
  Self.FIsLexed := True;
end;

procedure TSataniaRichText.Parse(const Memo: TKMemo; const IsEmote: Boolean = True);
var
  I, IMin, IMax: Integer;
  Token: TRichTextToken;
  TB: TKMemoTextBlock;
  S: String;

  procedure AddText;
  begin
    if (not IsPerformance) or (S = '') then Exit;
    TB := TKMemoTextBlock(Memo.Blocks[Memo.Blocks.Count - 1]);
    //
    if not (TB is TKMemoTextBlock) then Exit;
    TB.Text := TB.Text + S;
    S := '';
  end;

begin
  try
    if (not Self.FIsLexed) or (Self.TokenList.Count = 0) then
      Self.Lex(IsEmote);
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
              if IsPerformance then
                S := S + Token.Value
              else
                TB.Text := TB.Text + Token.Value;
            end else
            begin
              AddText;
              TB := Memo.Blocks.AddTextBlock(Token.Value);
              TB.TextStyle.Font.Color := Memo.Font.Color;
              TB.TextStyle.Font.Name := Memo.Font.Name;
              TB.TextStyle.Font.Size := Memo.Font.Size;
              TB.TextStyle.Font.Quality := fqCleartypeNatural;
            end;
            case Self.LastState of
              rtsCode:
                begin
                  TB.TextStyle.Font.Color := CColor(Save.Settings.ChatWindowColorCodeBlockText);
                  {$ifdef WINDOWS}
                  TB.TextStyle.Font.Name := 'Consolas';
                  {$else}
                  TB.TextStyle.Font.Name := 'Monospace';
                  {$endif}
                end;
              rtsThink:
                begin
                  TB.TextStyle.Font.Color := CColor(Save.Settings.ChatWindowColorItalicText);
                  TB.TextStyle.Font.Style := [fsItalic];
                end
              else
                begin
                  // Do nothing
                end;
            end;
          end;
        rtkNewLine:
          begin
            AddText;
            Memo.Blocks.AddParagraph;
          end;
        rtkState:
          begin
            if (Token.State = rtsCode) or ((Token.State = rtsNormal) and (Self.LastState = rtsCode)) then
            begin
              AddText;
              Memo.Blocks.AddParagraph;
            end;
            Self.LastState := Token.State;
          end;
        rtkEOS:
          begin
            AddText;
            Exit;
          end;
      end;
      Self.LastKind := Token.Kind;
      Self.LastTokenPos := I + 1;
    end;
  except
    on E: Exception do
    begin
      Writeln(E.Message);
    end;
  end;
end;

destructor TSataniaRichText.Destroy;
begin
  Self.TokenList.Free;
  inherited;
end;

end.

