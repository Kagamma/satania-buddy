unit Form.Tool.StackViewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Menus, Buttons, Mcdowell.EvilC;

type

  { TFormStackViewer }

  TFormStackViewer = class(TForm)
    MenuItemExportJSON: TMenuItem;
    MenuItemExpandSelected: TMenuItem;
    MenuItemClearAll: TMenuItem;
    MenuItemCollapseAll: TMenuItem;
    MenuItemExpandAll: TMenuItem;
    Panel: TPanel;
    PopupMenuTree: TPopupMenu;
    ExportJsonDialog: TSaveDialog;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    TreeView: TTreeView;
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemClearAllClick(Sender: TObject);
    procedure MenuItemCollapseAllClick(Sender: TObject);
    procedure MenuItemExpandAllClick(Sender: TObject);
    procedure MenuItemExpandSelectedClick(Sender: TObject);
    procedure MenuItemExportJSONClick(Sender: TObject);
    procedure PopupMenuTreePopup(Sender: TObject);
  private
    function Export: String;
  public
    procedure GatherStackTraceInfo(Message: String; StackTraceArray: TSEStackTraceSymbolArray);
  end;

var
  FormStackViewer: TFormStackViewer;

implementation

{$R *.lfm}

uses
  fpjson,
  Mcdowell.Data,
  Utils.Strings;

{ TFormStackViewer }

function GetRootCount(Tree: TTreeView): Integer;
var
  Node: TTreeNode;
begin
  Result := 0;
  Node := Tree.Items.GetFirstNode;
  while Node <> nil do
  begin
    Inc(Result);
    Node := Node.GetNextSibling;
  end;
end;

function GetRootItem(Tree: TTreeView; Index: Integer): TTreeNode;
begin
  Result := Tree.Items.GetFirstNode;
  while (Result <> nil) and (Index > 0) do
  begin
    Result := Result.GetNextSibling;
    Dec(Index);
  end;
end;

procedure TFormStackViewer.FormShow(Sender: TObject);
begin
end;

procedure TFormStackViewer.FormDestroy(Sender: TObject);
var
  SL: TStrings;
begin
  SL := TStringList.Create;
  try
    SL.Text := Self.Export;
    SL.SaveToFile(GetOSLocalDir + 'gab-watcher-report.json');
  finally
    SL.Free;
  end;
end;

procedure TFormStackViewer.MenuItemClearAllClick(Sender: TObject);
begin
  Self.TreeView.Items.Clear;
end;

procedure TFormStackViewer.MenuItemCollapseAllClick(Sender: TObject);
begin
  Self.TreeView.FullCollapse;
end;

procedure TFormStackViewer.MenuItemExpandAllClick(Sender: TObject);
begin
  Self.TreeView.FullExpand;
end;

procedure TFormStackViewer.MenuItemExpandSelectedClick(Sender: TObject);
begin
  Self.TreeView.Selected.Expand(True);
end;

function TFormStackViewer.Export: String;
  procedure Decode(var JSONStr: String; Node: TTreeNode);
  var
    I: Integer = 0;
    V: TSEValue;
    Key: String;
  begin
    JSONStr := JSONStr + '{';
    while Node <> nil do
    begin
      if (I > 0) then
        JSONStr := JSONStr + ',';
      if Node.HasChildren then
      begin
        JSONStr := JSONStr + '"' + StringToJSONString(Node.Text) + '":';
        Decode(JSONStr, Node.GetFirstChild);
      end else
      begin
        JSONStr := JSONStr + '"' + StringToJSONString(Node.Text) + '":null';
      end;
      Node := Node.GetNextSibling;
      Inc(I);
    end;
    JSONStr := JSONStr + '}';
  end;
begin
  Decode(Result, Self.TreeView.Items.GetFirstNode);
end;

procedure TFormStackViewer.MenuItemExportJSONClick(Sender: TObject);
var
  JSONStr: String;
  SL: TStrings;
begin
  JSONStr := Self.Export;
  if Self.ExportJsonDialog.Execute then
  begin
    SL := TStringList.Create;
    try
      SL.Text := JSONStr;
      SL.SaveToFile(Self.ExportJsonDialog.FileName);
    finally
      SL.Free;
    end;
  end;
end;

procedure TFormStackViewer.PopupMenuTreePopup(Sender: TObject);
begin
  Self.MenuItemExpandSelected.Enabled := Self.TreeView.Selected <> nil;
end;

procedure TFormStackViewer.GatherStackTraceInfo(Message: String; StackTraceArray: TSEStackTraceSymbolArray);

  function AddNode(const Root: Boolean; const Parent: TTreeNode; const StackNode: PSEStackTraceSymbol): TTreeNode;
  var
    I, C: Integer;
    S: String;
  begin
    if StackNode^.Name.IndexOf('___') = 0 then
      Exit;
    S := StackNode^.Value;
    C := Length(S);
    if C > 64000 then
    begin
      SetLength(S, 64000);
      S := S + '...';
    end;
    if Root then
      Result := TreeView.Items.AddChild(Parent, StackNode^.Name)
    else
      Result := TreeView.Items.AddChild(Parent, StackNode^.Name + ' (' + ValueKindNames[StackNode^.Kind] + '): ' + S);
    C := Length(StackNode^.Childs);
    if C > 0 then
    begin
      for I := 0 to C - 1 do
        AddNode(False, Result, @StackNode^.Childs[I]);
    end;
  end;

var
  I: Integer;
  Node: TTreeNode;
begin
  TreeView.BeginUpdate;
  if GetRootCount(Self.TreeView) > 9 then
    TreeView.Items.Delete(GetRootItem(Self.TreeView, 9));
  if TreeView.Items.Count = 0 then
    Node := TreeView.Items.Add(nil, '[' + FormatDateTime('YYYY/MM/DD hh:mm:ss', Now) + '] ' + Message)
  else
    Node := TreeView.Items.Insert(TreeView.Items.GetFirstNode, '[' + FormatDateTime('YYYY/MM/DD hh:mm:ss', Now) + '] ' + Message);
  for I := 0 to Length(StackTraceArray) - 1 do
  begin
    AddNode(True, Node, @StackTraceArray[I]);
  end;
  TreeView.EndUpdate;
end;

end.

