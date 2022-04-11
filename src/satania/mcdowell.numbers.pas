unit mcdowell.numbers;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Types, StrUtils, Generics.Collections, Math;

function WordsToNumbers(const Sentence: String): TDoubleDynArray; 
function WordsToDates(const Sentence: String): TDoubleDynArray;

implementation

type
  TNumberDict = specialize TDictionary<String, QWord>;

var
  DateDict,
  NumberDict,
  AddDict,
  MagDict: TNumberDict;

function WordsToNumbers(const Sentence: String): TDoubleDynArray;
var
  Tokens: TStringDynArray;
  Token: String;
  T: QWord;
  Fragment: QWord = 0;
  Number: QWord = 0;
  IsAdd: Boolean = False;
  IsMag: Boolean = False;
  IsNumbered: Boolean = False;
  I: Integer;
begin
  try
    Tokens := SplitString(LowerCase(Sentence), ' ');
    for I := 0 to Length(Tokens) - 1 do
    begin
      Token := Tokens[I];
      if Token = 'and' then
        continue;
      if NumberDict.ContainsKey(Token) then
      begin
        IsMag := False;
        IsNumbered := True;
        T := NumberDict[Token];
        if IsAdd and (T <> 0) then
          Fragment := Fragment + T
        else
          Fragment := Fragment * 10 + T;
        IsAdd := False;
      end else 
      if AddDict.ContainsKey(Token) then
      begin       
        IsMag := False;
        IsNumbered := True;
        if IsAdd then
          Fragment := Fragment * 100 + AddDict[Token]
        else
          Fragment := Fragment + AddDict[Token];
        IsAdd := True;
      end else
      if MagDict.ContainsKey(Token) then
      begin
        IsNumbered := True;
        Fragment := Max(Fragment, 1);
        if not IsMag then
        begin
          Number := Number + Fragment * MagDict[Token];
        end else
        begin
          Number := Number * MagDict[Token];
        end;
        Fragment := 0; 
        IsAdd := False;
        IsMag := True;
      end else
      begin
        if IsNumbered then
        begin
          IsAdd := False;
          IsNumbered := False;
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1] := Number + Fragment;
          Number := 0;
          Fragment := 0;
        end;
      end;
    end;
    if IsNumbered then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := Number + Fragment;
    end;
  finally
  end;
end;

function WordsToDates(const Sentence: String): TDoubleDynArray;
var
  Tokens: TStringDynArray;
  Token: String;
  I: Integer;
begin
  try
    Tokens := SplitString(LowerCase(Sentence), ' ');
    for I := 0 to Length(Tokens) - 1 do
    begin
      Token := Tokens[I];
      if DateDict.ContainsKey(Token) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := DateDict[Token];
      end;
    end;
  finally
  end;
end;

initialization                   
  DateDict := TNumberDict.Create;
  NumberDict := TNumberDict.Create;
  AddDict := TNumberDict.Create;
  MagDict := TNumberDict.Create;

  DateDict.Add('january', 1);
  DateDict.Add('February', 2);
  DateDict.Add('march', 3);
  DateDict.Add('april', 4);
  DateDict.Add('may', 5);
  DateDict.Add('june', 6);
  DateDict.Add('july', 7);
  DateDict.Add('august', 8);
  DateDict.Add('september', 9);
  DateDict.Add('october', 10);
  DateDict.Add('november', 11);
  DateDict.Add('december', 12);
                     
  NumberDict.Add('zero', 0);
  NumberDict.Add('one', 1);     
  NumberDict.Add('two', 2);
  NumberDict.Add('three', 3);
  NumberDict.Add('four', 4);
  NumberDict.Add('five', 5);
  NumberDict.Add('six', 6);
  NumberDict.Add('seven', 7);
  NumberDict.Add('eight', 8);
  NumberDict.Add('nine', 9);

  NumberDict.Add('first', 1);
  NumberDict.Add('second', 2);
  NumberDict.Add('third', 3);
  NumberDict.Add('fourth', 4);
  NumberDict.Add('fifth', 5);
  NumberDict.Add('sixth', 6);
  NumberDict.Add('seventh', 7);
  NumberDict.Add('eighth', 8);
  NumberDict.Add('ninth', 9);   
  NumberDict.Add('tenth', 10);
  NumberDict.Add('eleventh', 11);
  NumberDict.Add('twelfth', 12);
  NumberDict.Add('thirteenth', 13);
  NumberDict.Add('fourteenth', 14);
  NumberDict.Add('fifteenth', 15);
  NumberDict.Add('sixteenth', 16);
  NumberDict.Add('seventeenth', 17);
  NumberDict.Add('eighteenth', 18);
  NumberDict.Add('nineteenth', 19);   
  NumberDict.Add('twentieth', 20);
  NumberDict.Add('thirtieth', 30);

  AddDict.Add('ten', 10);
  AddDict.Add('eleven', 11);
  AddDict.Add('twelve', 12);
  AddDict.Add('thirteen', 13);
  AddDict.Add('fourteen', 14);
  AddDict.Add('fifteen', 15);
  AddDict.Add('sixteen', 16);
  AddDict.Add('seventeen', 17);
  AddDict.Add('eighteen', 18);
  AddDict.Add('nineteen', 19);

  AddDict.Add('twenty', 20);
  AddDict.Add('thirty', 30);
  AddDict.Add('forty', 40);
  AddDict.Add('fifty', 50);
  AddDict.Add('sixty', 60);
  AddDict.Add('seventy', 70);
  AddDict.Add('eighty', 80);
  AddDict.Add('ninety', 90);

  MagDict.Add('hundred', 100);
  MagDict.Add('hundreds', 100);
  MagDict.Add('thousand', 1000);
  MagDict.Add('thousands', 1000);
  MagDict.Add('million', 1000000);
  MagDict.Add('millions', 1000000);
  MagDict.Add('billion', 1000000000);
  MagDict.Add('billions', 1000000000);
  MagDict.Add('trillion', 1000000000000);   
  MagDict.Add('trillions', 1000000000000);

finalization
  DateDict.Free;
  NumberDict.Free;
  AddDict.Free;
  MagDict.Free;

end.

