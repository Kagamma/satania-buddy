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

unit form.evilc.editor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  SynEdit, SynHighlighterCpp, SynEditMarkupSpecialLine, LCLTranslator;

type

  { TFormEvilCEditor }

  TFormEvilCEditor = class(TForm)
    ImageList: TImageList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    Editor: TSynEdit;
    StatusBar: TStatusBar;
    SynCppSyn: TSynCppSyn;
    ToolBar1: TToolBar;
    ToolButtonSeparator1: TToolButton;
    ToolButtonSaveAs: TToolButton;
    ToolButtonNew: TToolButton;
    ToolButtonSave: TToolButton;
    ToolButtonSeparator2: TToolButton;
    ToolButtonRun: TToolButton;
    ToolButtonOpen: TToolButton;
    procedure EditorChange(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ToolButtonNewClick(Sender: TObject);
    procedure ToolButtonOpenClick(Sender: TObject);
    procedure ToolButtonRunClick(Sender: TObject);
    procedure ToolButtonSaveAsClick(Sender: TObject);
    procedure ToolButtonSaveClick(Sender: TObject);
  private

  public
    ErrorPos: TPoint;
    WorkingFile: String;
  end;

var
  FormEvilCEditor: TFormEvilCEditor;

implementation

{$R *.lfm}

uses
  Math,
  Mcdowell;

{ TFormEvilCEditor }

procedure TFormEvilCEditor.ToolButtonNewClick(Sender: TObject);
begin
  WorkingFile := '';
  Caption := 'New Evil Scheme';
  Editor.Lines.Clear;
  StatusBar.Panels[1].Text := '';
end;

procedure TFormEvilCEditor.FormShow(Sender: TObject);
begin
  ToolButtonNewClick(Sender);
  {$ifdef WINDOWS}
  Editor.Font.Name := 'Consolas';
  {$endif}
end;

procedure TFormEvilCEditor.FormCreate(Sender: TObject);
begin
  ErrorPos.Y := -1;
end;

procedure TFormEvilCEditor.EditorSpecialLineColors(Sender: TObject;
  Line: integer; var Special: boolean; var FG, BG: TColor);
begin
  if Line = ErrorPos.Y then
  begin
    Special := True;
    BG := $00A5FF;
    Fg := clBlack;
  end;
end;

procedure TFormEvilCEditor.EditorChange(Sender: TObject);
begin
  ErrorPos.Y := -1;
  if (WorkingFile <> '') and (Caption[1] <> '*') then
    Caption := '*' + ExtractFileName(WorkingFile);
end;

procedure TFormEvilCEditor.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = $78 then
  begin
    ToolButtonRunClick(Self);
  end else
  if (Key = 83) and (Shift = [ssCtrl]) then
  begin
    ToolButtonSaveClick(Self);
  end;
end;

procedure TFormEvilCEditor.ToolButtonOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    Caption := ExtractFileName(OpenDialog.FileName);
    WorkingFile := OpenDialog.FileName;
    Editor.Lines.LoadFromFile(OpenDialog.FileName);
    StatusBar.Panels[1].Text := '';
  end;
end;

procedure TFormEvilCEditor.ToolButtonRunClick(Sender: TObject);
begin
  Satania.Action('script', Editor.Lines.Text);
  try
    StatusBar.Panels[1].Text := '';
    Satania.Script.Exec;
  except
    on E: Exception do
    begin
      ErrorPos := Point(Satania.Script.ErrorCol, Min(Satania.Script.ErrorLn, Editor.Lines.Count));
      Editor.CaretXY := ErrorPos;
      Editor.SetFocus;
      Editor.Invalidate;
      Satania.ResetScript;
      Satania.IsAction := False;
      Satania.Talk(E.Message);
      StatusBar.Panels[1].Text := E.Message;
    end;
  end;
end;

procedure TFormEvilCEditor.ToolButtonSaveAsClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    Caption := ExtractFileName(SaveDialog.FileName);
    WorkingFile := SaveDialog.FileName;
    Editor.Lines.SaveToFile(WorkingFile);
  end;
end;

procedure TFormEvilCEditor.ToolButtonSaveClick(Sender: TObject);
begin
  if WorkingFile <> '' then
  begin
    Editor.Lines.SaveToFile(WorkingFile);
    Caption := ExtractFileName(WorkingFile);
  end else
  if SaveDialog.Execute then
  begin
    Caption := ExtractFileName(SaveDialog.FileName);
    WorkingFile := SaveDialog.FileName;
    Editor.Lines.SaveToFile(WorkingFile);
  end;
end;

end.

