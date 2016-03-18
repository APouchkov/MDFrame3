unit SBaseCxTreeForm;
{$I config.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SBaseXXViewForm, cxGraphics, ExtCtrls, DB, dxBar,
{$IFDEF FASTSCRIPT_RTTI}
  fs_iinterpreter,
{$ENDIF}
  cxControls, dxStatusBar, cxCustomData, cxStyles, cxTL, cxInplaceContainer,
  cxTLData, cxDBTL, NamedVariables, cxClasses, cxCalendar, cxBarEditItem, cxLookAndFeels,
  cxLookAndFeelPainters, cxTLdxBarBuiltInMenu, SBaseDataModule, cxContainer,
  cxEdit, cxSplitter, cxGroupBox, ImgList, System.Actions, ActnList, cxMDDBTreeList,
  LayoutPanel, dxMDBarButton, dxMDBar, dxMDStatusBar, dxSkinsCore,
  dxSkinsDefaultPainters;

const
  SBFSelectParents : String = 'SELECT#PARENTS';

type
  TcxDBTreeListDataControllerCrack = class(TcxDBTreeListDataController);

  TSBaseCxTreeFrm = class(TSBaseXXViewFrm)
    cxDBTreeList: TcxMDDBTreeList;
    mnNew2: TdxMDBarButton;
    mnNewX: TdxBarSubItem;

    procedure DataSetAfterOpen(DataSet: TDataSet); override;

    procedure cxDBTreeListFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    procedure cxDBTreeListCollapsing(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);

    procedure mnFitToWidthClick(Sender: TObject);
    procedure mnFitToHeightClick(Sender: TObject);
    procedure mnPropertiesClick(Sender: TObject);
    procedure mnExpandAllClick(Sender: TObject);
    procedure mnCollapseAllClick(Sender: TObject);
    procedure cxDBTreeListNodeAssignValues(Sender: TcxCustomDBTreeList; ANode: TcxTreeListNode);
    procedure cxDBTreeListDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure cxDBTreeListNodeExpanded(Sender: TcxCustomTreeList; ANode: TcxTreeListNode);

    procedure DataSetProviderOnBeginUpdate(ADataSet: TCustomSQLDataSetProvider; var ADataSourceStateBookmark: TDataSourceStateBookmark); override;
    procedure DataSetProviderOnEndUpdate(ADataSet: TCustomSQLDataSetProvider; const ADataSourceStateBookmark: TDataSourceStateBookmark); override;
  private
    { Private declarations }
    FSelectParents: Boolean;
  strict private
    FcxDBTreeListDragOver: TfsProcVariable;
  protected
    { Protected declarations }
    procedure InternalCreate; override;
    procedure InternalPrepareParams; override;

    procedure SetRecordsPrivileges(const Value: Word); override;

    procedure InternalSaveGridPosition(var ABookmark: TDataSourceBookmark); override;
    procedure InternalRestoreGridPosition(ABookmark: TDataSourceBookmark); override;
    procedure InternalReOpen; override;

    procedure InternalBeginUpdate; override;
    procedure InternalEndUpdate; override;

    function  InternalGetControlsEnabled: Boolean; override;

    procedure InternalSetGridFocus; override;
    procedure InternalSetGridFirstRow; override;
    function  InternalLocateByKey(AKeyValue: Variant): Boolean; override;

    function  InternalGetGrid: TComponent; override;
    function  InternalGetGridKeys: String; override;
    function  InternalFindRecord(AKeyFieldValues: Variant): Integer; override;
    function  InternalGetGridKeyValues: Variant; override;
    function  InternalGetGridRecordChecked(ARecordIndex: Integer): Boolean; override;
    procedure InternalSetGridRecordChecked(ARecordIndex: Integer); override;
    function  InternalGetGridFieldValues(const ARecordIndex: Integer; const AFieldNames: String; var AUnknownFields: String): Variant; override;
    function  InternalGetGridFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant; override;
    function  InternalGetGridRecordCount: Integer; override;

    function  InternalGetSelectedRowCount: Integer; override;
//    procedure InternalSetSelectedRow(ARecNo: Integer); override;

    procedure InternalExcelExport; override;

    procedure InternalAutoExpand;
    procedure ShowRecordCount; override;
  public
    { Public declarations }
    Expand   : Byte;
    Expanded : Byte;

    destructor Destroy; override;
  end;

implementation

uses
  SBaseFormDefinitions, SBaseForm, SBaseCxUtils, SBaseConstants, SBaseUtils,
  SBaseItemReactions;

type
  TcxCustomTreeListCrack = class(TcxCustomTreeList);

{$R *.dfm}

procedure TSBaseCxTreeFrm.InternalCreate;
begin
  Expand   := 0;
  Expanded := 0;

  mnNewX.Caption    := SBaseProperties.Text_MenuNew;
  mnNewX.ImageIndex := SBaseFormProperties.IconIndex_New;
  mnNew.Caption     := SBaseProperties.Text_MenuNewLevel;
  mnNew2.Caption    := SBaseProperties.Text_MenuNewSubLevel;
  mnNew2.ImageIndex := SBaseFormProperties.IconIndex_New;

  inherited;

  FcxDBTreeListDragOver := PrepareFunction('cxDBTreeListDragOver', False);

  if (SelectMode in [smMultiple, smMultipleRepeateAble]) then begin
    cxDBTreeList.OptionsView.CheckGroups := True;
    cxDBTreeList.OnNodeAssignValues := cxDBTreeListNodeAssignValues;
  end;

  if cxDBTreeList.OptionsBehavior.AlwaysExpandedLevel = -1 then begin
    mnExpandAll.Visible := ivNever;
    mnCollapseAll.Visible := ivNever;
  end;

  with ItemsReactions.FindOrCreateReaction(mnNewX) do begin
    LinkedComponent   := InternalGetGrid;
    OnClick.Reaction  := biraDataSourceActiveAndRecordSubMenu;
  end;

  FDataSourceControls.FindOrCreateGridControl(cxDBTreeList).AfterUpdate := GridAfterUpdateActions;
end;

procedure TSBaseCxTreeFrm.InternalPrepareParams;
var
  LIdx: Integer;
begin
  inherited;

  if SelectMode <> smNone then
    if CreateFormParams.Find(SBFSelectParents, LIdx) then begin
      FSelectParents := (CreateFormParams.Items[LIdx].Value = True);
      CreateFormParams.Delete(LIdx);
    end;
end;

destructor TSBaseCxTreeFrm.Destroy;
begin
  if cxDBTreeList <> nil then
    with cxDBTreeList.Customizing do
      if Form <> nil then
        Form.Close;

  inherited;
end;

procedure TSBaseCxTreeFrm.InternalAutoExpand;
begin
  if (Expand = 255) and (cxDBTreeList.AbsoluteCount > 0) then
    cxDBTreeList.FullExpand
end;

procedure TSBaseCxTreeFrm.InternalBeginUpdate;
begin
  cxDBTreeList.BeginUpdate;
end;

procedure TSBaseCxTreeFrm.InternalEndUpdate;
begin
//  TcxCustomTreeListCrack(cxDBTreeList).DataChanged;
  cxDBTreeList.EndUpdate;
end;

procedure TSBaseCxTreeFrm.InternalSaveGridPosition(var ABookmark: TDataSourceBookmark);
begin
  ABookmark := cxDBTreeList.GridBookmark
end;

procedure TSBaseCxTreeFrm.InternalRestoreGridPosition(ABookmark: TDataSourceBookmark);
begin
  cxDBTreeList.GridBookmark := ABookmark
end;

procedure TSBaseCxTreeFrm.InternalReOpen;
begin
  cxDBTreeList.RefreshDataSet;
end;

function TSBaseCxTreeFrm.InternalGetControlsEnabled: Boolean;
begin
  Result := (inherited InternalGetControlsEnabled); // and (cxDBTreeList.LockUpdate = 0)
end;

procedure TSBaseCxTreeFrm.InternalSetGridFocus;
begin
  cxDBTreeList.SetFocus
end;

procedure TSBaseCxTreeFrm.InternalSetGridFirstRow;
begin
  if (cxDBTreeList.TopNode <> nil) and (not DataSetProvider.ControlsDisabled) then
    cxDBTreeList.TopNode.Focused := True;
end;

function TSBaseCxTreeFrm.InternalLocateByKey(AKeyValue: Variant): Boolean;
begin
  Result := cxDBTreeList.DataController.LocateByKey(AKeyValue)
end;

function TSBaseCxTreeFrm.InternalGetGrid: TComponent;
begin
  Result := cxDBTreeList
end;

function TSBaseCxTreeFrm.InternalGetGridKeys: String;
begin
  Result := TcxDBTreeList(cxDBTreeList).DataController.KeyField
end;

function TSBaseCxTreeFrm.InternalFindRecord(AKeyFieldValues: Variant): Integer;
var
  LNode: TcxDBTreeListNode;
begin
  LNode := TcxDBTreeList(cxDBTreeList).FindNodeByKeyValue(AKeyFieldValues);
  if Assigned(LNode) then
    Result := LNode.AbsoluteIndex
  else
    Result := -1;
end;

function TSBaseCxTreeFrm.InternalGetGridKeyValues: Variant;
begin
  if (cxDBTreeList.FocusedNode <> nil) and (cxDBTreeList.DataController.KeyField <> '') then
    Result := TcxDBTreeListNode(cxDBTreeList.FocusedNode).KeyValue
  else
    Result := unAssigned
end;

function TSBaseCxTreeFrm.InternalGetGridFieldValues(const ARecordIndex: Integer; const AFieldNames: String; var AUnknownFields: String): Variant;
begin
  Result := cxDBTreeList.GetRecordValues(ARecordIndex, AFieldNames, @AUnknownFields);
end;

function TSBaseCxTreeFrm.InternalGetGridFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant;
begin
  if (cxDBTreeList.FocusedNode <> nil) then
    if cxDBTreeList.OptionsData.SyncMode then
      Result := cxDBTreeList.DataController.DataSet.Fields.GetValues(AFieldNames, @AUnknownFields)
    else
      Result := InternalGetGridFieldValues(cxDBTreeList.FocusedNode.AbsoluteIndex, AFieldNames, AUnknownFields)
  else begin
    Result          := unAssigned;
    AUnknownFields  := AFieldNames;
  end;
end;

function TSBaseCxTreeFrm.InternalGetGridRecordChecked(ARecordIndex: Integer): Boolean;
begin
  with cxDBTreeList.AbsoluteItems[ARecordIndex] do
    Result := Checked and ((FSelectParents and not Parent.Checked) or ((not FSelectParents) and (Count = 0)));
end;

procedure TSBaseCxTreeFrm.InternalSetGridRecordChecked(ARecordIndex: Integer);
begin
  with cxDBTreeList.AbsoluteItems[ARecordIndex] do
    Checked := True;
end;

function TSBaseCxTreeFrm.InternalGetGridRecordCount: Integer;
begin
  Result := cxDBTreeList.DataController.NodesCount
end;

function TSBaseCxTreeFrm.InternalGetSelectedRowCount: Integer;
begin
  Result := cxDBTreeList.SelectionCount
end;

//procedure TSBaseCxTreeFrm.InternalSetSelectedRow(ARecNo: Integer);
//begin
//  if DataSetProvider.ControlsDisabled then begin
//    DataSetProvider.EnableControls;
//    try
//      cxDBTreeList.Selections[ARecNo].Focused := True;
//    finally
//      DataSetProvider.DisableControls;
//    end;
//  end else
//    cxDBTreeList.Selections[ARecNo].Focused := True;
//end;

procedure TSBaseCxTreeFrm.InternalExcelExport;
begin
  ExportCxTreeToExcel(cxDBTreeList, Caption, ExportToExcelInNativeFormat)
end;

procedure TSBaseCxTreeFrm.DataSetAfterOpen(DataSet: TDataSet);
begin
  InternalAutoExpand;
  inherited;
end;

procedure TSBaseCxTreeFrm.DataSetProviderOnBeginUpdate(ADataSet: TCustomSQLDataSetProvider; var ADataSourceStateBookmark: TDataSourceStateBookmark);
begin
  cxDBTreeList.BeginUpdate(ADataSourceStateBookmark);
end;

procedure TSBaseCxTreeFrm.DataSetProviderOnEndUpdate(ADataSet: TCustomSQLDataSetProvider; const ADataSourceStateBookmark: TDataSourceStateBookmark);
begin
  cxDBTreeList.EndUpdate(ADataSourceStateBookmark, [sbrDisableCount], DataSourceBookmarkRestores_All);
end;

procedure TSBaseCxTreeFrm.SetRecordsPrivileges(const Value: Word);
begin
  inherited;

  if not Loading then begin
    if FRecordsPrivileges and Privilege_Insert_Bit = 0 then begin
      ItemsReactions.SetActive(mnNew2, False);
      mnNew2.Enabled := False;
    end else
      with ItemsReactions.FindOrCreateReaction(mnNew2, biraDataSourceInPlaceInsertChild) do begin
        LinkedComponent := InternalGetGrid;
        OnGetEnabled.Active := True;
        if OnClick.Reaction = biraNone then OnClick.Reaction := biraCustomDataSourceRecord;
      end;
  end;
end;

procedure TSBaseCxTreeFrm.ShowRecordCount;
begin
  with StatusBar.Panels[Pred(StatusBar.Panels.Count)] do begin
    Text := RecourdCountPrefix + IntToStr(cxDBTreeList.Count) + ' / ' + IntToStr(cxDBTreeList.AbsoluteCount);
    Width := PanelStyle.StatusBarControl.Canvas.TextWidth(Text) + 10;
  end;
end;

{ Собственные методы Гридов }

procedure TSBaseCxTreeFrm.cxDBTreeListDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  LDragNode, LSourceNode, LNode: TcxTreeListNode;
  LcxDBTreeList: TcxDBTreeList absolute Source;
  LParams: TNamedVariants;
begin
  Accept := (Sender = Source);
  if Accept then begin
    LSourceNode := LcxDBTreeList.GetNodeAt(X, Y);
    LDragNode := LcxDBTreeList.DragNode;
    Accept := Assigned(LSourceNode) and (LDragNode <> LSourceNode);
    if Accept and (LDragNode.Level < LSourceNode.Level) then begin
      LNode := LSourceNode;
      repeat
        LNode := LNode.Parent
      until (LNode.Level = LDragNode.Level);
      Accept := (LNode <> LDragNode);
    end;

{$IFDEF FASTSCRIPT_RTTI}
    if Accept and Assigned(FcxDBTreeListDragOver) then begin
      LParams.UnknownAsNull := True;
      LParams[SConst_DragNode + ';' + SConst_SelectedNode + ';' + SConst_State + ';' + SConst_Accept] := VarArrayOf([Integer(LDragNode), Integer(LSourceNode), State, True]);
      CallFunctionOut(FcxDBTreeListDragOver, LParams);
      Accept := (LParams[SConst_Accept] = True);
    end;
{$ENDIF}
  end;
end;

(*
procedure TSBaseCxTreeFrm.cxDBTreeListDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  LDragNode, LSourceNode: TcxTreeListNode;
  LcxDBTreeList: TcxDBTreeList absolute Source;
//  LcxDBTreeListDataController: TcxDBTreeListDataControllerCrack;
begin
  LSourceNode := LcxDBTreeList.GetNodeAt(X, Y);
  LDragNode := LcxDBTreeList.DragNode;

  if Assigned(LSourceNode) then begin
    LDragNode.Focused := True;
{
    LcxDBTreeListDataController := TcxDBTreeListDataControllerCrack(LcxDBTreeList.DataController);
    LcxDBTreeListDataController.Edit;
      LcxDBTreeListDataController.DBParentField.AsVariant := (LSourceNode as TcxDBTreeListNode).KeyValue;
    LcxDBTreeListDataController.Post(True);
}
    DataSetProvider.Edit;
      DataSetProvider.FieldByName(LcxDBTreeList.DataController.ParentField).AsVariant := (LSourceNode as TcxDBTreeListNode).KeyValue;
    DataSetProvider.Post;
  end;
end;
*)

procedure TSBaseCxTreeFrm.cxDBTreeListFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
begin
  DBGridAfterFocusedRecordChanged(True);
end;

procedure TSBaseCxTreeFrm.cxDBTreeListNodeAssignValues(Sender: TcxCustomDBTreeList; ANode: TcxTreeListNode);
begin
  if ANode.CheckGroupType = ncgNone then begin
    ANode.CheckGroupType := ncgCheckGroup;
    ANode.CheckState := cbsUnchecked;
  end;
end;

procedure TSBaseCxTreeFrm.cxDBTreeListNodeExpanded(Sender: TcxCustomTreeList; ANode: TcxTreeListNode);
begin
  if ANode.Count = 1 then
    ANode.Expand(False);
end;

procedure TSBaseCxTreeFrm.cxDBTreeListCollapsing(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
begin
  Allow := (ANode = nil) or (Expanded = 0) or (ANode.Level > Expanded);
end;

{ Собственные методы кнопок }

procedure TSBaseCxTreeFrm.mnFitToWidthClick(Sender: TObject);
begin
  cxDBTreeList.OptionsView.ColumnAutoWidth := mnFitToWidth.Down;
end;

procedure TSBaseCxTreeFrm.mnFitToHeightClick(Sender: TObject);
begin
  cxDBTreeList.OptionsView.CellAutoHeight := mnFitToHeight.Down;
end;

procedure TSBaseCxTreeFrm.mnPropertiesClick(Sender: TObject);
begin
  cxDBTreeList.Customizing.Visible := True;
end;

procedure TSBaseCxTreeFrm.mnExpandAllClick(Sender: TObject);
begin
  cxDBTreeList.FullExpand;
end;

procedure TSBaseCxTreeFrm.mnCollapseAllClick(Sender: TObject);
begin
  cxDBTreeList.FullCollapse;
end;

initialization
  RegisterClasses([TSBaseCxTreeFrm]);

finalization
  RegisterClasses([TSBaseCxTreeFrm]);

end.
