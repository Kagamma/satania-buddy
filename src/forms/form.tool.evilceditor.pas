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

unit form.tool.evilceditor;

{$I configs.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  SynEdit, SynHighlighterCpp, SynEditMarkupSpecialLine, SynCompletion,
  LCLTranslator, lclintf, Menus,
  Mcdowell.EvilC, Types, LCLType;

type

  { TFormEvilCEditor }

  TFormEvilCEditor = class(TForm)
    ImageList: TImageList;
    MenuItemEditorCut: TMenuItem;
    MenuItemEditorCopy: TMenuItem;
    MenuItemEditorPaste: TMenuItem;
    OpenDialog: TOpenDialog;
    PopupMenuEditor: TPopupMenu;
    SaveDialog: TSaveDialog;
    Editor: TSynEdit;
    StatusBar: TStatusBar;
    SynCompletion: TSynCompletion;
    SynCppSyn: TSynCppSyn;
    ToolBar1: TToolBar;
    ToolButtonHelp: TToolButton;
    ToolButton2: TToolButton;
    ToolButtonUndo: TToolButton;
    ToolButtonRedo: TToolButton;
    ToolButtonSeparator1: TToolButton;
    ToolButtonSaveAs: TToolButton;
    ToolButtonNew: TToolButton;
    ToolButtonSave: TToolButton;
    ToolButtonSeparator2: TToolButton;
    ToolButtonRun: TToolButton;
    ToolButtonOpen: TToolButton;
    ToolButtonSeparator3: TToolButton;
    procedure EditorChange(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemEditorCopyClick(Sender: TObject);
    procedure MenuItemEditorCutClick(Sender: TObject);
    procedure MenuItemEditorPasteClick(Sender: TObject);
    procedure SynCompletionBeforeExecute(ASender: TSynBaseCompletion;
      var ACurrentString: String; var APosition: Integer; var AnX,
      AnY: Integer; var AnResult: TOnBeforeExeucteFlags);
    procedure ToolButtonHelpClick(Sender: TObject);
    procedure ToolButtonNewClick(Sender: TObject);
    procedure ToolButtonOpenClick(Sender: TObject);
    procedure ToolButtonRedoClick(Sender: TObject);
    procedure ToolButtonRunClick(Sender: TObject);
    procedure ToolButtonSaveAsClick(Sender: TObject);
    procedure ToolButtonSaveClick(Sender: TObject);
    procedure OpenRules;
    procedure ToolButtonUndoClick(Sender: TObject);
  private
    // For autocomplete
    Script: TEvilC;
    procedure GenerateAutoComplete(const ASource: String = '');
  public
    ErrorPos: TPoint;
    WorkingFile: String;
    procedure LoadFromFile(const AFileName: String);
  end;

var
  FormEvilCEditor: TFormEvilCEditor;

implementation

{$R *.lfm}

uses
  Math,
  Mcdowell;

{ TFormEvilCEditor }

procedure TFormEvilCEditor.GenerateAutoComplete(const ASource: String = '');
var
  SL: TStringList;
  I: Integer;
  S: String;
  Token: TSEToken;
begin
  Self.SynCompletion.ItemList.Clear;
  SL := TStringList.Create;
  try
    SL.Sorted := True;
    SL.Duplicates := dupIgnore;
    try
      Self.Script.Source := ASource;
      Self.Script.Lex;
    except
      // There will be errors, obviously. We just need to ignore it
    end;
    // Add identity
    for I := 0 to Self.Script.TokenList.Count - 1 do
    begin
      Token := Self.Script.TokenList[I];
      if Token.Kind = tkIdent then
        SL.Add(Token.Value);
    end;
    // Add keywords
    SL.AddStrings([
      '#include',
      'if',
      'for',
      'while',
      'true',
      'false',
      'yield',
      'break',
      'continue',
      'pause',
      'in',
      'to',
      'fn',
      'import',
      'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f64', 'buffer', 'wbuffer'
    ]);
    // Transfer function names and constant names to completion
    for I := 0 to Self.Script.FuncNativeList.Count - 1 do
    begin
      SL.Add(Self.Script.FuncNativeList[I].Name);
    end;
    for S in Self.Script.ConstMap.Keys do
    begin
      SL.Add(S);
    end;
    for S in SL do
    begin
      Self.SynCompletion.ItemList.Add(S);
    end;
  finally
    SL.Free;
  end;
end;

procedure TFormEvilCEditor.ToolButtonNewClick(Sender: TObject);
begin
  WorkingFile := '';
  Caption := 'New Evil Scheme';
  Editor.Lines.Clear;
  StatusBar.Panels[1].Text := '';
end;

procedure TFormEvilCEditor.FormShow(Sender: TObject);
var
  SL: TStringList;
  I: Integer;
  S: String;
  Token: TSEToken;
begin
  ToolButtonNewClick(Sender);
  {$ifdef WINDOWS}
  Editor.Font.Name := 'Consolas';
  {$endif}
  Self.ToolButtonSave.Enabled := Self.WorkingFile <> '';
end;

procedure TFormEvilCEditor.MenuItemEditorCopyClick(Sender: TObject);
begin
  Editor.CopyToClipboard;
end;

procedure TFormEvilCEditor.MenuItemEditorCutClick(Sender: TObject);
begin
  Editor.CutToClipboard;
end;

procedure TFormEvilCEditor.MenuItemEditorPasteClick(Sender: TObject);
begin
  Editor.PasteFromClipboard;
end;

procedure TFormEvilCEditor.SynCompletionBeforeExecute(
  ASender: TSynBaseCompletion; var ACurrentString: String;
  var APosition: Integer; var AnX, AnY: Integer;
  var AnResult: TOnBeforeExeucteFlags);
begin
  Self.GenerateAutoComplete(Editor.Lines.Text);
end;

procedure TFormEvilCEditor.ToolButtonHelpClick(Sender: TObject);
begin
  OpenURL('https://github.com/Kagamma/satania-buddy/wiki/Scripting-Reference');
end;

procedure TFormEvilCEditor.FormCreate(Sender: TObject);
begin
  ErrorPos.Y := -1;
  Self.Script := TEvilC.Create;
  Satania.RegisterFuncs(Self.Script);
  Satania.UpdateMeta(Self.Script);
end;

procedure TFormEvilCEditor.FormDestroy(Sender: TObject);
begin
  Self.Script.Free;
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
    Self.LoadFromFile(OpenDialog.FileName);
    Self.ToolButtonSave.Enabled := True;
  end;
end;

procedure TFormEvilCEditor.ToolButtonRedoClick(Sender: TObject);
begin
  Editor.Redo;
end;

procedure TFormEvilCEditor.ToolButtonRunClick(Sender: TObject);
begin
  // Clean up cache
  Satania.CleanUpCache;
  //
  Satania.Action('script', Editor.Lines.Text);
  try
    StatusBar.Panels[1].Text := '';
    Satania.Script.Exec; // Force to execute the script immediately
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
    Self.ToolButtonSave.Enabled := True;
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

procedure TFormEvilCEditor.OpenRules;
begin
  Caption := ExtractFileName('data/nn/chatbot/rules.json');
  WorkingFile := 'data/nn/chatbot/rules.json';
  Editor.Lines.LoadFromFile('data/nn/chatbot/rules.json');
  StatusBar.Panels[1].Text := '';
end;

procedure TFormEvilCEditor.ToolButtonUndoClick(Sender: TObject);
begin
  Editor.Undo;
end;

procedure TFormEvilCEditor.LoadFromFile(const AFileName: String);
begin
  Caption := ExtractFileName(AFileName);
  WorkingFile := AFileName;
  Editor.Lines.LoadFromFile(AFileName);
  StatusBar.Panels[1].Text := '';
  ErrorPos.Y := -1;
  Self.ToolButtonSave.Enabled := True;
end;

end.

