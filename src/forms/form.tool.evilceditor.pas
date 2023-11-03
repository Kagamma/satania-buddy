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
  SynEdit, SynEditMarkupSpecialLine, SynCompletion,
  LCLTranslator, lclintf, Menus,
  Mcdowell.EvilC, Types, LCLType, ExtCtrls, SynFacilHighlighter, SynFacilBasic,
  SynEditTypes;

type

  { TFormEvilCEditor }

  TFormEvilCEditor = class(TForm)
    MenuItemEnableAssertions: TMenuItem;
    MenuItemEditorCut: TMenuItem;
    MenuItemEditorCopy: TMenuItem;
    MenuItemEditorPaste: TMenuItem;
    OpenDialog: TOpenDialog;
    PopupMenuCompilerFlags: TPopupMenu;
    PopupMenuEditor: TPopupMenu;
    SaveDialog: TSaveDialog;
    Editor: TSynEdit;
    StatusBar: TStatusBar;
    SynCompletion: TSynCompletion;
    ToolBar1: TToolBar;
    ToolButtonCompilerFlags: TToolButton;
    ToolButtonReplace: TToolButton;
    ToolButtonFind: TToolButton;
    ToolButton3: TToolButton;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItemEditorCopyClick(Sender: TObject);
    procedure MenuItemEditorCutClick(Sender: TObject);
    procedure MenuItemEditorPasteClick(Sender: TObject);
    procedure MenuItemEnableAssertionsClick(Sender: TObject);
    procedure SynCompletionBeforeExecute(ASender: TSynBaseCompletion;
      var ACurrentString: String; var APosition: Integer; var AnX,
      AnY: Integer; var AnResult: TOnBeforeExeucteFlags);
    procedure ToolButtonFindClick(Sender: TObject);
    procedure ToolButtonHelpClick(Sender: TObject);
    procedure ToolButtonNewClick(Sender: TObject);
    procedure ToolButtonOpenClick(Sender: TObject);
    procedure ToolButtonRedoClick(Sender: TObject);
    procedure ToolButtonReplaceClick(Sender: TObject);
    procedure ToolButtonRunClick(Sender: TObject);
    procedure ToolButtonSaveAsClick(Sender: TObject);
    procedure ToolButtonSaveClick(Sender: TObject);
    procedure OpenRules;
    procedure ToolButtonUndoClick(Sender: TObject);
  private
    // For autocomplete
    Script: TEvilC;
    Highlighter: TSynFacilSyn;
    procedure GenerateAutoComplete(const ASource: String = '');
    procedure LoadHighligher(const Ext: String);                    
    procedure FindNext;
  public
    EnbaleAssertions: Boolean;
    ErrorPos: TPoint;
    SearchText: String;
    WorkingFile: String;
    procedure LoadFromFile(const AFileName: String);
  end;

implementation

{$R *.lfm}

uses
  Globals,
  Math,
  Mcdowell.Data,
  Mcdowell;

{ TFormEvilCEditor }

procedure TFormEvilCEditor.GenerateAutoComplete(const ASource: String = '');
var
  SL: TStringList;
  I, P: Integer;
  S: String;
  Token: TSEToken;
begin
  Self.SynCompletion.ItemList.Clear;
  SL := TStringList.Create;
  try
    SL.Sorted := True;
    SL.Duplicates := dupIgnore;
    try
      Self.Script.IncludePathList.Clear;
      Self.Script.IncludePathList.Add('data/scripts/' + Save.Settings.Skin + '/');
      Self.Script.IncludePathList.Add(GetOSLocalDir + 'data/scripts/' + Save.Settings.Skin + '/');
      Self.Script.Source := ASource;
      Self.Script.Lex;
      Self.Script.Parse;
    except
      // There will be errors, obviously. We just need to ignore it
    end;
    // Add keywords
    SL.AddStrings([
      'using',
      'if',
      'for',
      'while',
      'do',
      'switch',
      'case',
      'default',
      'yield',
      'break',
      'continue',
      'return',
      'self',
      'in',
      'to',
      'fn',
      'import',
      'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f64', 'buffer', 'wbuffer'
    ]);
    // Transfer function names and constant names to completion
    for I := 0 to Self.Script.FuncNativeList.Count - 1 do
    begin
      SL.Add(Self.Script.FuncNativeList[I].Name + '()');
    end;
    for I := 0 to Self.Script.FuncScriptList.Count - 1 do
    begin
      SL.Add(Self.Script.FuncScriptList[I].Name + '()');
    end;
    for I := 0 to Self.Script.FuncImportList.Count - 1 do
    begin
      SL.Add(Self.Script.FuncImportList[I].Name + '()');
    end;
    for I := 0 to Self.Script.VarList.Count - 1 do
    begin
      SL.Add(Self.Script.VarList[I].Name);
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
  if Editor.Modified then
  begin
    if MessageDlg('', 'You have unsaved changes. Do you still want to reset?', mtInformation, [mbYes, mbNo], 0) = mrNo then
      Exit;
  end;
  WorkingFile := '';
  Caption := 'New Evil Scheme';
  Editor.Lines.Clear;
  StatusBar.Panels[1].Text := '';
  LoadHighligher('evil');
  Editor.Modified := False;
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

procedure TFormEvilCEditor.MenuItemEnableAssertionsClick(Sender: TObject);
begin
  Self.EnbaleAssertions := not Self.EnbaleAssertions;
  Self.MenuItemEnableAssertions.Checked := Self.EnbaleAssertions;
end;

procedure TFormEvilCEditor.SynCompletionBeforeExecute(
  ASender: TSynBaseCompletion; var ACurrentString: String;
  var APosition: Integer; var AnX, AnY: Integer;
  var AnResult: TOnBeforeExeucteFlags);
begin
  Self.GenerateAutoComplete(Editor.Lines.Text);
end;

procedure TFormEvilCEditor.ToolButtonFindClick(Sender: TObject);
begin
  SearchText := '';
  if InputQuery('Find', 'Search string', False, Self.SearchText) then
  begin
    Editor.SearchReplace(Self.SearchText, '', []);
  end;
end;

procedure TFormEvilCEditor.FindNext;
begin
  if SearchText <> '' then
  begin
    Editor.SearchReplace(Self.SearchText, '', [ssoFindContinue]);
  end;
end;

procedure TFormEvilCEditor.ToolButtonHelpClick(Sender: TObject);
begin
  OpenURL('https://github.com/Kagamma/satania-buddy/wiki/Scripting-Reference');
end;

procedure TFormEvilCEditor.LoadHighligher(const Ext: String);
var
  Path: String;
begin
  if Highlighter <> nil then
  begin
    Editor.Highlighter := nil;
    FreeAndNil(Highlighter);
  end;
  Path := 'data/highlighters/' + StringReplace(Ext, '.', '', [rfReplaceAll]) + '.xml';
  if not FileExists(Path) then
    Exit;
  Highlighter := TSynFacilSyn.Create(Self);
  Highlighter.LoadFromFile(Path);
  Editor.Highlighter := Highlighter;
end;

procedure TFormEvilCEditor.FormCreate(Sender: TObject);
begin
  ErrorPos.Y := -1;
  Self.Script := TEvilC.Create;
  Satania.RegisterFuncs(Self.Script);
  Satania.UpdateMeta(Self.Script);
  ToolButtonNewClick(Sender);
  {$ifdef WINDOWS}
  //Editor.Font.Name := 'Consolas';
  {$else}
  Editor.Font.Name := 'Liberation Mono';
  Editor.Font.Quality := fqAntialiased;
  Editor.Font.Height := 16;
  {$endif}
  Self.ToolButtonSave.Enabled := Self.WorkingFile <> '';
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

procedure TFormEvilCEditor.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TFormEvilCEditor.FormCloseQuery(Sender: TObject; var CanClose: Boolean
  );
begin
  CanClose := True;
  if Editor.Modified then
  begin
    if MessageDlg('', 'You have unsaved changes. Do you still want to close?', mtInformation, [mbYes, mbNo], 0) = mrYes then
      Editor.Modified := False
    else
      CanClose := False;
  end;
end;

procedure TFormEvilCEditor.EditorChange(Sender: TObject);
begin
  ErrorPos.Y := -1;
  if (WorkingFile <> '') and (Caption[1] <> '*') then
    Caption := '*' + WorkingFile;
end;

procedure TFormEvilCEditor.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = $78 then
  begin
    ToolButtonRunClick(Self);
  end else
  if (Key = VK_S) and (Shift = [ssCtrl]) then
  begin
    ToolButtonSaveClick(Self);
  end else
  if (Key = VK_F) and (Shift = [ssCtrl]) then
  begin
    Self.ToolButtonFindClick(Self);
  end else
  if (Key = VK_R) and (Shift = [ssCtrl]) then
  begin
    Self.ToolButtonReplaceClick(Self);
  end else
  if Key = VK_F3 then
  begin
    Self.FindNext;
  end;
end;

procedure TFormEvilCEditor.ToolButtonOpenClick(Sender: TObject);
begin
  if Editor.Modified then
  begin
    if MessageDlg('', 'You have unsaved changes. Do you still want to open another file?', mtInformation, [mbYes, mbNo], 0) = mrNo then
      Exit;
  end;
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

procedure TFormEvilCEditor.ToolButtonReplaceClick(Sender: TObject);
var
  ATexts: array of String;
begin
  SearchText := '';
  SetLength(ATexts, 2);
  if InputQuery('Replace All', ['Search string', 'Replace with'], ATexts) then
  begin
    SearchText := ATexts[0];
    Editor.SearchReplace(ATexts[0], ATexts[1], [ssoEntireScope, ssoReplaceAll]);
  end;
end;

procedure TFormEvilCEditor.ToolButtonRunClick(Sender: TObject);
begin
  // Clean up cache
  Satania.CleanUpCache;
  //
  Satania.Action('script', Editor.Lines.Text);
  try
    StatusBar.Panels[1].Text := '';
    Satania.Script.OptimizeAsserts := not Self.EnbaleAssertions;
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
      Satania.Log(E.Message);
      StatusBar.Panels[1].Text := E.Message;
    end;
  end;
end;

procedure TFormEvilCEditor.ToolButtonSaveAsClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    Caption := SaveDialog.FileName;
    WorkingFile := SaveDialog.FileName;
    Editor.Lines.SaveToFile(WorkingFile);
    Self.ToolButtonSave.Enabled := True;
    Editor.Modified := False;
  end;
end;

procedure TFormEvilCEditor.ToolButtonSaveClick(Sender: TObject);
begin
  if WorkingFile <> '' then
  begin
    Editor.Lines.SaveToFile(WorkingFile);
    Caption := WorkingFile;
    Editor.Modified := False;
  end else
  if SaveDialog.Execute then
  begin
    Caption := SaveDialog.FileName;
    WorkingFile := SaveDialog.FileName;
    Editor.Lines.SaveToFile(WorkingFile);
    Editor.Modified := False;
  end;
end;

procedure TFormEvilCEditor.OpenRules;
begin
  Caption := 'data/nn/chatbot/rules.json';
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
  Caption := AFileName;
  WorkingFile := AFileName;
  Editor.Lines.LoadFromFile(AFileName);
  StatusBar.Panels[1].Text := '';
  ErrorPos.Y := -1;
  Self.ToolButtonSave.Enabled := True;
  LoadHighligher(ExtractFileExt(AFileName));
  Editor.Modified := False;
end;

end.

