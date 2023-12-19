unit Form.Tool.StackViewer;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Mcdowell.EvilC;

type

  { TFormStackViewer }

  TFormStackViewer = class(TForm)
    Panel: TPanel;
    TreeView: TTreeView;
    procedure FormShow(Sender: TObject);
  private
    FIsRendered: Boolean;
    FStackTraceArray: TSEStackTraceSymbolArray;
  public
    procedure RenderStackTraceInfo(StackTraceArray: TSEStackTraceSymbolArray);
    procedure GatherStackTraceInfo(StackTraceArray: TSEStackTraceSymbolArray);
  end;

var
  FormStackViewer: TFormStackViewer;

implementation

{$R *.lfm}

{ TFormStackViewer }

procedure TFormStackViewer.GatherStackTraceInfo(StackTraceArray: TSEStackTraceSymbolArray);
begin
  Self.FStackTraceArray := StackTraceArray;
  Self.FIsRendered := False;
  if Self.Visible then
    Self.RenderStackTraceInfo(Self.FStackTraceArray);
end;

procedure TFormStackViewer.FormShow(Sender: TObject);
begin
  if not Self.FIsRendered then
    Self.RenderStackTraceInfo(Self.FStackTraceArray);
end;

procedure TFormStackViewer.RenderStackTraceInfo(StackTraceArray: TSEStackTraceSymbolArray);

  function AddNode(const Parent: TTreeNode; const StackNode: PSEStackTraceSymbol): TTreeNode;
  var
    I, C: Integer;
    S: String;
  begin
    S := StackNode^.Value;
    C := Length(S);
    if C > 4096 then
      SetLength(S, 4096);
    if Parent = nil then
      Result := TreeView.Items.Add(Parent, StackNode^.Name + '()')
    else
      Result := TreeView.Items.AddChild(Parent, StackNode^.Name + ' (' + StackNode^.Kind + '): ' + S);
    C := Length(StackNode^.Childs);
    if C > 0 then
    begin
      for I := 0 to C - 1 do
        AddNode(Result, @StackNode^.Childs[I]);
    end;
  end;

var
  I: Integer;
begin
  TreeView.BeginUpdate;
  TreeView.Items.Clear;
  for I := 0 to Length(StackTraceArray) - 1 do
  begin
    AddNode(nil, @StackTraceArray[I]);
  end;
  TreeView.EndUpdate;
  Self.FIsRendered := True;
end;

end.

