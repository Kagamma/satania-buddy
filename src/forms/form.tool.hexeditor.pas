unit form.tool.hexeditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, khexeditor;

type

  { TFormHexEditor }

  TFormHexEditor = class(TForm)
    HexEditor: TKHexEditor;
  private

  public

  end;

var
  FormHexEditor: TFormHexEditor;

implementation

{$R *.lfm}

end.

