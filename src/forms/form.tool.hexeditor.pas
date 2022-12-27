unit form.tool.hexeditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, khexeditor;

type

  { TFormHexEditor }

  TFormHexEditor = class(TForm)
    HexEditor: TKHexEditor;
    ToolBar1: TToolBar;
    ButtonSave: TToolButton;
    ButtonSaveAs: TToolButton;
  private

  public

  end;

var
  FormHexEditor: TFormHexEditor;

implementation

{$R *.lfm}

end.

