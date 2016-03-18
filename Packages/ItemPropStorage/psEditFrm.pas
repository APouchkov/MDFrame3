unit psEditFrm;

interface

uses
  TypInfo, Math, ImgList, ItemPropStorage, cxPropEditors,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, Buttons, DesignWindows, DB, dxBar, StdCtrls, ToolsAPI,
  CheckLst, DesignIntf, ExtCtrls, DesignConst;

type
  TpsEditForm = class(TDesignWindow)
    pnlCat: TPanel;
    lbCat: TListBox;
    lbItems: TCheckListBox;

    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lbCatClick(Sender: TObject);
    procedure lbItemsClick(Sender: TObject);
    procedure lbItemsClickCheck(Sender: TObject);
    procedure lbItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbItemsDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lbItemsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure lbItemsStartDrag(Sender: TObject; var DragObject: TDragObject);
  private
    FPropStorage: TdxItemPropStorage;
    FDragIndex: Integer;
    procedure Sort;
    procedure SetPropStorage(AEditor: TdxItemPropStorage);
    procedure SelectCategory(AIndex: Integer);
    function PropIndex(AItem: TdxBarItem): Integer;
    procedure SelectItem;
    procedure CatChanged(Sender: TObject);
    procedure RefreshCategory;
    procedure SelectActiveItem;
    procedure SwapItems(idx1, idx2: Integer);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Activated; override;
  public
    procedure DesignerClosed(const Designer: IDesigner; AGoingDormant: Boolean); override;
    procedure ItemDeleted(const ADesigner: IDesigner; Item: TPersistent); override;
    procedure ItemsModified(const Designer: IDesigner); override;
    property PropStorage: TdxItemPropStorage read FPropStorage write SetPropStorage;
  end;
  TCheckListBoxAccess = class(TCheckListBox);

implementation

{$R *.dfm}

resourcestring
  SErrSetBarManager = '”кажите BarManager';
  SElementsFormat = 'Ёлементы %s';

function TpsEditForm.PropIndex(AItem: TdxBarItem): Integer;
var
  APropItem: TdxBarPropItem;
begin
  APropItem := PropStorage.Properties.FindByBarItem(AItem);
  if Assigned(APropItem) then
    Result := APropItem.Index
  else
    Result := -1;
end;

procedure TpsEditForm.SetPropStorage(AEditor: TdxItemPropStorage);
begin
  if FPropStorage <> AEditor then begin
    if not Assigned(AEditor.BarManager) then
      raise Exception.Create(SErrSetBarManager);
    Caption := Format(SElementsFormat, [AEditor.BarManager.Name]);
    FPropStorage := AEditor;
    FPropStorage.FreeNotification(Self);
    FPropStorage.BarManager.FreeNotification(Self);
    if Assigned(FPropStorage.BarManager.Images) then
      lbItems.ItemHeight := FPropStorage.BarManager.Images.Height;
    CatChanged(Self);
  end;
end;

procedure TpsEditForm.lbItemsClickCheck(Sender: TObject);
begin
  SelectItem();
end;

procedure TpsEditForm.lbItemsDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  AItem: TdxBarItem;
  AShift: Integer;
begin
  if Index >= 0 then begin
    AItem := lbItems.Items.Objects[Index] as TdxBarItem;
    lbItems.Canvas.FillRect(Rect);
    if Assigned(PropStorage.BarManager.Images) and (AItem.ImageIndex >= 0) then begin
      PropStorage.BarManager.Images.Draw(lbItems.Canvas, Rect.Left, Rect.Top, AItem.ImageIndex);
      AShift := PropStorage.BarManager.Images.Width;
    end else
      AShift := 0;

    lbItems.Canvas.TextOut(Rect.Left + AShift, Rect.Top, Format('%s (%s)', [AItem.Name, AItem.Caption]));
  end;        
end;

procedure TpsEditForm.lbCatClick(Sender: TObject);
begin
  SelectCategory(lbCat.ItemIndex);
end;

procedure TpsEditForm.SelectCategory(AIndex: Integer);
var
  i: Integer;
  ItemIndex: Integer;
begin
  if lbCat.ItemIndex = AIndex then
    ItemIndex := lbItems.ItemIndex
  else
    ItemIndex := 0;

  lbItems.Clear;

  if PropStorage.BarManager.Categories.IndexOf(lbCat.Items[AIndex]) = -1 then begin
    lbCat.Items.Assign(PropStorage.BarManager.Categories);
    AIndex := 0;
  end;

  with PropStorage.BarManager do
    for i := 0 to Pred(ItemCount) do
      if Items[i].Category = AIndex then begin
        lbItems.AddItem('', Items[i]);
        lbItems.Checked[Pred(lbItems.Items.Count)] := PropIndex(Items[i]) >= 0;
      end;

  if lbItems.Count <= ItemIndex then
    ItemIndex := 0
  else
    ItemIndex := Max(ItemIndex, 0);

  if lbItems.Count > ItemIndex then begin
    lbItems.Selected[ItemIndex] := True;
    SelectActiveItem;
  end;
  
  if (lbCat.ItemIndex >= 0) and not lbCat.Selected[lbCat.ItemIndex] then
    lbCat.Selected[lbCat.ItemIndex] := True;
end;

procedure TpsEditForm.SelectItem;
var
  AIndex: Integer;
  AItem: TdxBarItem;
  AChecked: Boolean;
begin
  if lbItems.ItemIndex < 0 then
    Exit;

  AItem := lbItems.Items.Objects[lbItems.ItemIndex] as TdxBarItem;
  AChecked := lbItems.Checked[lbItems.ItemIndex];
  AIndex := PropIndex(AItem);

  if (AIndex = -1) = AChecked then begin
    if AChecked then
      (PropStorage.Properties.Add as TdxBarPropItem).Item := AItem
    else begin
      if Active then
        Designer.SelectComponent(PropStorage);
      PropStorage.Properties.Delete(AIndex);
    end;
    Sort;
    Designer.Modified;
  end;
end;

procedure TpsEditForm.lbItemsClick(Sender: TObject);
begin
  SelectActiveItem;
end;

procedure TpsEditForm.CatChanged(Sender: TObject);
begin
  lbCat.Items.Assign(PropStorage.BarManager.Categories);
  lbCat.ItemIndex := 0;
  RefreshCategory;
end;

procedure TpsEditForm.ItemsModified(const Designer: IDesigner);
begin
  RefreshCategory;
end;

procedure TpsEditForm.ItemDeleted(const ADesigner: IDesigner; Item: TPersistent);
var
  APropItem: TdxBarPropItem;
begin
  if Item is TdxBarItem then begin
    APropItem := PropStorage.Properties.FindByBarItem(TdxBarItem(Item));
    if Assigned(APropItem) then
      PropStorage.Properties.Delete(APropItem.Index);

    RefreshCategory;
  end;
end;

procedure TpsEditForm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then begin
    if Assigned(PropStorage) and
      ((AComponent = PropStorage) or (AComponent = PropStorage.BarManager))
    then
      close
    else if AComponent is TdxBarItem then
      RefreshCategory
  end
end;

procedure TpsEditForm.DesignerClosed(const Designer: IDesigner;
  AGoingDormant: Boolean);
begin
  inherited;
  if Designer = Self.Designer then
    close;
end;

procedure TpsEditForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree
end;

procedure TpsEditForm.lbItemsDragDrop(Sender, Source: TObject; X, Y:
    Integer);
begin
  SwapItems(FDragIndex, lbItems.ItemAtPos(Point(X, Y), True));
end;

procedure TpsEditForm.lbItemsDragOver(Sender, Source: TObject; X, Y:
    Integer; State: TDragState; var Accept: Boolean);
var
  CurPos: Integer;
begin
  Accept := (Sender = lbItems) and (lbItems.ItemAtPos(Point(X, Y), True) >= 0);
  if Accept and (Y div lbItems.ItemHeight < lbItems.Count) then begin
    CurPos := lbItems.ItemHeight * (Y div lbItems.ItemHeight);
    lbItems.Repaint;
    lbItems.Canvas.DrawFocusRect(Rect(TCheckListBoxAccess(lbItems).GetCheckWidth,
      CurPos, lbItems.Width, CurPos + lbItems.ItemHeight));
  end;
end;

procedure TpsEditForm.lbItemsStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  FDragIndex := lbItems.ItemIndex;
end;

procedure TpsEditForm.RefreshCategory;
begin
  if lbCat.ItemIndex >= 0 then
    SelectCategory(lbCat.ItemIndex);
end;

procedure TpsEditForm.SwapItems(idx1, idx2: Integer);
begin
  PropStorage.BarManager.MoveItem(
    (lbItems.Items.Objects[idx1] as TdxBarItem).Index,
    (lbItems.Items.Objects[idx2] as TdxBarItem).Index
  );

  Sort;
  Designer.Modified;
  lbItems.ItemIndex := idx2;
  SelectActiveItem;
end;

procedure TpsEditForm.Sort;
var
  i, Idx: Integer;
  AItem: TdxBarPropItem;
begin
  Idx := 0;
  PropStorage.Properties.BeginUpdate;
  try
    for i := 0 to Pred(lbItems.Count) do begin
      AItem := PropStorage.Properties.FindByBarItem(lbItems.Items.Objects[i] as TdxBarItem);
      if Assigned(AItem) then begin
        AItem.Index := Idx;
        inc (Idx);
      end;
    end;
  finally
    PropStorage.Properties.EndUpdate;
  end;
end;

procedure TpsEditForm.Activated;
begin
  inherited;
  CatChanged(nil)
end;

procedure TpsEditForm.SelectActiveItem;
var
  APropItem: TdxBarPropItem;
begin
  if (lbItems.ItemIndex >= 0) and lbItems.Checked[lbItems.ItemIndex] then begin
    APropItem := PropStorage.Properties.FindByBarItem(lbItems.Items.Objects[lbItems.ItemIndex] as TdxBarItem);
    if Assigned(APropItem) and Active then
      Designer.SelectComponent(APropItem)
  end else if Active then
    Designer.SelectComponent(PropStorage);
end;

end.
