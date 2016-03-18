unit dhEditFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Buttons, DesignIntf, Math, DesignWindows,
  DataHandler, DesignEditors, StdCtrls, Vcl.Menus;

type
  TDHEditForm = class(TDesignWindow)
    tvMain: TTreeView;
    btnAdd: TSpeedButton;
    btnDel: TSpeedButton;
    pmData: TPopupMenu;
    mnAdd: TMenuItem;
    mnDel: TMenuItem;
    procedure btnAddClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure mnAddClick(Sender: TObject);
    procedure mnDelClick(Sender: TObject);
    procedure tvMainChange(Sender: TObject; Node: TTreeNode);
    procedure tvMainEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tvMainMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pmDataPopup(Sender: TObject);
    procedure tvMainDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvMainDragOver(Sender, Source: TObject; X, Y: Integer; State:
        TDragState; var Accept: Boolean);
    procedure tvMainStartDrag(Sender: TObject; var DragObject: TDragObject);
  private
    FRoot: TCustomDataHandler;
    FDragDH: TSubDataHandler;
    procedure BuildTree(ANode: TTreeNode; ADataHandler: TCustomDataHandler);
    procedure EnableDelete(ANode: TTreeNode);
    procedure SetRoot(ADataHandler: TCustomDataHandler);
    procedure RefreshNode(ANode: TTreeNode);
    function NodeByDataHandler(ADataHandler: TCustomDataHandler): TTreeNode;
  protected
    procedure Activated; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure ItemsModified(const Designer: IDesigner); override;
    property DataHandler: TCustomDataHandler read FRoot write SetRoot;
  end;

implementation

{$R *.dfm}  

type
  TCustomDataHandlerCrack = class(TCustomDataHandler);

procedure TDHEditForm.Activated;
begin
  inherited;
  if Assigned(Designer) then begin
    Designer.Activate;
    if Assigned(tvMain.Selected) then
      Designer.SelectComponent(tvMain.Selected.Data)
  end
end;

procedure TDHEditForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FRoot) then
    Close
end;

procedure TDHEditForm.SetRoot(ADataHandler: TCustomDataHandler);
begin
  ADataHandler.FreeNotification(Self);
  FRoot := ADataHandler;
  tvMain.Items.Clear;
  BuildTree(nil, ADataHandler);
  tvMain.Items[0].Expand(True);
end;

procedure TDHEditForm.ItemsModified(const Designer: IDesigner);
var
  i: Integer;
begin
  for i := 0 to Pred(tvMain.Items.Count) do
    with tvMain.Items[i] do
      Text := TCustomDataHandler(Data).Caption;
end;

procedure TDHEditForm.ItemDeleted(const ADesigner: IDesigner;
  Item: TPersistent);
var
  i: Integer;
  ANode: TTreeNode;
begin
  if (ADesigner = Designer) and (Item is TCustomDataHandler) and (Item <> FRoot) then
    for i := 0 to Pred(tvMain.Items.Count) do begin
      ANode := tvMain.Items[i];
      if ANode.Data = Item then begin
        ANode.Delete;
        exit;
      end;
    end;
end;

procedure TDHEditForm.btnAddClick(Sender: TObject);
var
  ACurDH: TCustomDataHandler;
  ANewDH: TSubDataHandler;
  AName: string;
  i: Integer;
begin
  if Assigned(tvMain.Selected) then begin
    ACurDH := TCustomDataHandler(tvMain.Selected.Data);
    ANewDH := TSubDataHandler.Create(FRoot.Owner);
    AName := '';
    i := Succ(tvMain.Selected.Count);
    while (AName = '') or Assigned(FRoot.Owner.FindComponent(AName)) do begin
      inc(i);
      AName := Format('%s_%d', [ACurDH.Name, i]);
    end;

    ANewDH.Name := AName;
    ACurDH.Insert(ANewDH);
    Designer.Modified;

    tvMain.Items.AddChildObject(tvMain.Selected, ANewDH.Caption, ANewDH);
    tvMain.Selected.Expand(False);
  end;
end;

procedure TDHEditForm.btnDelClick(Sender: TObject);
begin
  if Assigned(tvMain.Selected) then begin
    TObject(tvMain.Selected.Data).Free;
    Designer.Modified;
  end;
end;

procedure TDHEditForm.BuildTree(ANode: TTreeNode; ADataHandler: TCustomDataHandler);

  procedure DoBuildTree(ANode: TTreeNode; ADH: TCustomDataHandler);
  var
    i: Integer;
  begin
    for i := 0 to Pred(ADH.Count) do
      DoBuildTree(
        tvMain.Items.AddChildObject(ANode, ADH.Children[i].Caption, ADH.Children[i]),
        ADH.Children[i])
  end;

begin
  if not Assigned(ANode) then
    ANode := tvMain.Items.AddObject(nil, DataHandler.Caption, ADataHandler);
  DoBuildTree(ANode, ADataHandler);
end;

procedure TDHEditForm.tvMainChange(Sender: TObject; Node: TTreeNode);
begin
  if Assigned(Node) and Assigned(Node.Data) then begin
    Designer.SelectComponent(Node.Data);
    EnableDelete(Node);
  end 
end;

procedure TDHEditForm.EnableDelete(ANode: TTreeNode);
begin
  btnDel.Enabled := not TCustomDataHandler(ANode.Data).IsRoot;
  mnDel.Enabled := btnDel.Enabled;
end;

procedure TDHEditForm.DesignerClosed(const Designer: IDesigner;
  AGoingDormant: Boolean);
begin
  if Designer = Self.Designer then
    Close;
end;

procedure TDHEditForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TDHEditForm.tvMainDragOver(Sender, Source: TObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
var
  ANode: TTreeNode;
begin
  ANode := tvMain.GetNodeAt(X, Y);
  Accept := (Sender = tvMain) and Assigned(ANode) and
    (TSubDataHandler(ANode.Data).Parent = FDragDH.Parent);
end;

procedure TDHEditForm.mnAddClick(Sender: TObject);
begin
  btnAdd.Click;
end;

procedure TDHEditForm.mnDelClick(Sender: TObject);
begin
  if btnDel.Enabled then
    btnDel.Click;
end;

procedure TDHEditForm.tvMainEdited(Sender: TObject; Node: TTreeNode; var S: string);
begin
  TCustomDataHandler(Node.Data).Caption := S;
  Designer.Modified;
end;

procedure TDHEditForm.tvMainMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ANode: TTreeNode;
begin
  if Button = mbRight then begin
    ANode := tvMain.GetNodeAt(X, Y);
    if Assigned(ANode) then
      ANode.Selected := True;
  end;
end;

procedure TDHEditForm.pmDataPopup(Sender: TObject);
begin
  if Assigned(tvMain.Selected) then
    EnableDelete(tvMain.Selected);
end;

procedure TDHEditForm.tvMainDragDrop(Sender, Source: TObject; X, Y: Integer);
  function GetIndex(ADH: TSubDataHandler): Integer;
  var
    I: Integer;
  begin
    for I := 0 to Pred(ADH.Parent.Count) do
      if ADH.Parent.Children[I] = ADH then Exit(I);
    Result := -1;
  end;

var
  ADH: TSubDataHandler;
  ANode, AParentNode: TTreeNode;
begin
  with tvMain.GetNodeAt(X, Y) do begin
    AParentNode := Parent;
    ADH := TSubDataHandler(Data);
  end;
  if Assigned(ADH) and Assigned(FDragDH) then begin
    TCustomDataHandlerCrack(FDragDH.Parent).FItems.Move(GetIndex(FDragDH), GetIndex(ADH));

//    FDragDH.Parent.SwapChildren(FDragDH, ADH);
//    if (ADH1.Index >= 0) and (ADH2.Index >= 0) then
//      FItems.Move(ADH1.Index, ADH2.Index);

    RefreshNode(AParentNode);
    Designer.Modified;
  end;
  ANode := NodeByDataHandler(FDragDH);
  if Assigned(ANode) then
    ANode.Selected := True;
end;

procedure TDHEditForm.tvMainStartDrag(Sender: TObject; var DragObject:
    TDragObject);
begin
  FDragDH := TSubDataHandler(tvMain.Selected.Data);
end;

procedure TDHEditForm.RefreshNode(ANode: TTreeNode);
var
  ADH: TCustomDataHandler;
begin
  ANode.DeleteChildren;
  ADH := TCustomDataHandler(ANode.Data);
  if Assigned(ADH) then begin
    BuildTree(ANode, ADH);
    ANode.Expand(False);
  end;
end;

function TDHEditForm.NodeByDataHandler(ADataHandler: TCustomDataHandler): TTreeNode;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Pred(tvMain.Items.Count) do
    if tvMain.Items[i].Data = ADataHandler then begin
      Result := tvMain.Items[i];
      exit;
    end;
end;

end.
