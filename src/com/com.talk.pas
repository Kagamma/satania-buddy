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

unit Com.Talk;

{$I configs}

interface

uses
  Classes, SysUtils,
  BrookAction;

type
  TGreetAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

  TTalkAction = class(TBrookAction)
  public
    procedure Get; override;
  end;

implementation

uses
  Mcdowell;

procedure TGreetAction.Get;
begin
  Write('satania-buddy<br /><li><a href="/talk">/talk</a></li>');
end;

procedure TTalkAction.Get;
var
  Name, Value, Typ, Message: String;
  I: Integer;
begin
  Typ := 'chat';
  for I := 0 to Params.Count - 1 do
  begin
    Params.GetNameValue(I, Name, Value);
    if Name = 'message' then
      Message := Value
    else
    if Name = 'type' then
      Typ := Value;
  end;
  if Message <> '' then
    Satania.Action(Typ, Message);
  Write('method=GET<br /><li>message=' + Message + '</li><li>type=' + Typ + '</li>');
end;

initialization
  TTalkAction.Register('/talk');  
  TGreetAction.Register('*');

end.

