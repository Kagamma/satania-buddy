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

unit form.tool.hexeditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, khexeditor;

type

  { TFormHexEditor }

  TFormHexEditor = class(TForm)
    HexEditor: TKHexEditor;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ToolBar1: TToolBar;
    ToolButtonOpen: TToolButton;
    ToolButtonSave: TToolButton;
    ToolButtonSaveAs: TToolButton;
    procedure ToolButtonOpenClick(Sender: TObject);
    procedure ToolButtonSaveAsClick(Sender: TObject);
    procedure ToolButtonSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    WorkingFile: String;
    procedure LoadFromFile(const AFileName: String);
  end;

var
  FormHexEditor: TFormHexEditor;

implementation

{$R *.lfm}

{ TFormHexEditor }

procedure TFormHexEditor.ToolButtonSaveClick(Sender: TObject);
begin
  if Self.WorkingFile <> '' then
    Self.HexEditor.SaveToFile(Self.WorkingFile);
end;

procedure TFormHexEditor.FormShow(Sender: TObject);
begin
  Self.ToolButtonSave.Enabled := Self.WorkingFile <> '';
end;

procedure TFormHexEditor.ToolButtonSaveAsClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    Self.Caption := ExtractFileName(SaveDialog.FileName);
    Self.WorkingFile := Self.SaveDialog.FileName;
    Self.HexEditor.SaveToFile(Self.WorkingFile);
    Self.ToolButtonSave.Enabled := True;
  end;
end;

procedure TFormHexEditor.ToolButtonOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Self.Caption := ExtractFileName(SaveDialog.FileName);
    Self.WorkingFile := Self.SaveDialog.FileName;
    Self.LoadFromFile(Self.WorkingFile);
    Self.ToolButtonSave.Enabled := True;
  end;
end;

procedure TFormHexEditor.LoadFromFile(const AFileName: String);
begin
  Self.Caption := ExtractFileName(AFileName);
  Self.WorkingFile := AFileName;
  Self.HexEditor.LoadFromFile(AFileName);
  Self.ToolButtonSave.Enabled := True;
end;

end.

