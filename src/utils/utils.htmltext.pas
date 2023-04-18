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

// Unused at the moment

unit utils.htmltext;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FastHTMLParser, Generics.Collections;

type
  THtmlTextRec = record
    Text: String;
    Color: Cardinal;
  end;
  THtmlTextList = specialize TList<THtmlTextRec>;

  THtmlText = class
  protected
    FText: String;
    procedure SetText(S: String); 
    procedure DoTagFound(NoCaseTag, ActualTag: String);
    procedure DoTextFound(Text: String);
    procedure TextToHTMLChars;
  public  
    HtmlTextList: THtmlTextList;
    constructor Create;
    destructor Destroy; override;
    property Text: String write SetText;
  end;

implementation

procedure THtmlText.SetText(S: String);
begin
  Self.HtmlTextList.Clear;
  Self.FText := S;
  Self.TextToHTMLChars;
end;

procedure THtmlText.DoTagFound(NoCaseTag, ActualTag: String);
begin

end;

procedure THtmlText.DoTextFound(Text: String);
var
  TS: THtmlTextRec;
begin
  TS.Text := Text;
  TS.Color := 0;
  Self.HtmlTextList.Add(TS);
end;

procedure THtmlText.TextToHTMLChars;
var
  Parser: THTMLParser;
begin
  Parser := THTMLParser.Create(Self.FText);
  try
    Parser.Exec;
  finally
    Parser.Free;
  end;
end;

constructor THtmlText.Create;
begin
  inherited;
  Self.HtmlTextList := THtmlTextList.Create;
end;           

destructor THtmlText.Destroy;
begin
  Self.HtmlTextList.Free;
  inherited;
end;

end.

