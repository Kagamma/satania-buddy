unit Form.Tool.StackViewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Menus, Buttons, Mcdowell.EvilC;

type

  { TFormStackViewer }

  TFormStackViewer = class(TForm)
    MenuItemExpandSelected: TMenuItem;
    MenuItemClearAll: TMenuItem;
    MenuItemCollapseAll: TMenuItem;
    MenuItemExpandAll: TMenuItem;
    Panel: TPanel;
    PopupMenuTree: TPopupMenu;
    TreeView: TTreeView;
    procedure FormShow(Sender: TObject);
    procedure MenuItemClearAllClick(Sender: TObject);
    procedure MenuItemCollapseAllClick(Sender: TObject);
    procedure MenuItemExpandAllClick(Sender: TObject);
    procedure MenuItemExpandSelectedClick(Sender: TObject);
    procedure PopupMenuTreePopup(Sender: TObject);
  private
  public
    procedure GatherStackTraceInfo(Message: String; StackTraceArray: TSEStackTraceSymbolArray);
  end;

var
  FormStackViewer: TFormStackViewer;

implementation

{$R *.lfm}

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
    if C > 4096 then
      SetLength(S, 4096);
    if Root then
      Result := TreeView.Items.AddChild(Parent, StackNode^.Name + '()')
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
begin
  TreeView.BeginUpdate;
  if GetRootCount(Self.TreeView) > 9 then
    TreeView.Items.Delete(GetRootItem(Self.TreeView, 9));
  for I := 0 to Length(StackTraceArray) - 1 do
  begin
    if TreeView.Items.Count = 0 then
      AddNode(True, TreeView.Items.Add(nil, '[' + FormatDateTime('YYYY/MM/DD hh:mm:ss', Now) + '] ' + Message), @StackTraceArray[I])
    else
      AddNode(True, TreeView.Items.Insert(TreeView.Items.GetFirstNode, '[' + FormatDateTime('YYYY/MM/DD hh:mm:ss', Now) + '] ' + Message), @StackTraceArray[I]);
  end;
  TreeView.EndUpdate;
end;

end.

