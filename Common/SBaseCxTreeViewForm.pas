unit SBaseCxTreeViewForm;
{$I config.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SBaseCxViewForm, cxGraphics, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, cxCalendar, ExtCtrls, dxBar, cxBarEditItem,
  cxClasses, cxGridLevel, SBaseForm, cxGridBandedTableView, cxGridDBBandedTableView,
  cxControls, SBaseDataModule, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  ImgList, NamedVariables, cxGridDBTableView, cxGrid, dxStatusBar, cxTL, cxTextEdit, Buttons,
  StdCtrls, cxInplaceContainer, cxDBTL, cxTLData, cxTLdxBarBuiltInMenu, cxMaskEdit, cxContainer,
  cxSplitter, cxGroupBox, cxLookAndFeels, cxLookAndFeelPainters, cxNavigator,
  System.Actions, ActnList, LayoutPanel, dxMDBarButton, cxMDGrid
{IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter, dxMDBar, dxMDStatusBar, dxSkinsCore, dxSkinsDefaultPainters,
  cxMDDBTreeList
{ENDIF}
{$IFDEF DELPHIXE3}
  , System.UITypes
{$ENDIF}
;

type
  TTreeViewExpandMode = (tvemCurrentNode, tvemAllChilds);

  TSBaseCxTreeViewFrm = class(TSBaseCxViewFrm)
    mnTree: TdxBarButton;
    mnTreeChildsIncluded: TdxBarButton;
    mnFindInTree: TdxBarButton;

    mnGroups: TdxBarListItem;
    PopupMenuGroups: TdxBarPopupMenu;

    PanelLeft: TcxGroupBox;
    PanelLeftCaption: TPanel;
    PanelLeftTopGray: TPanel;
    LbGroups: TLabel;
    LbTreeNode: TLabel;

    cxTree: TcxMDDBTreeList;
    cxTreeStandardColumn: TcxMDDBTreeListColumn;
    cxTreeFocusedImage: TcxMDDBTreeListColumn;

    btnTreeTypeChange: TSpeedButton;

    PanelMainCaption: TPanel;
    PanelMainTopGray: TPanel;

    SplitterLeft: TSplitter;

    TreeSource: TDataSource;
    TreeSetProvider: TSQLDataSetProvider;
    GroupSetProvider: TSQLDataSetProvider;

    procedure FormShow(Sender: TObject); override;

    procedure cxTreeCanFocusNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
    procedure cxTreeFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    procedure cxTreeCollapsing(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
    procedure cxTreeCustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);

    procedure mnTreeClick(Sender: TObject);
    function  mnFindInTreeAllowed(Sender: TObject): Boolean;
    procedure mnFindInTreeClick(Sender: TObject);
    procedure mnTreeChildsIncludedClick(Sender: TObject);
    procedure mnGroupsClick(Sender: TObject);
    procedure btnTreeTypeChangeClick(Sender: TObject);
    procedure PanelGridResize(Sender: TObject);
    procedure cxTreeGetNodeImageIndex(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
    procedure GroupSetProviderAfterOpen(DataSet: TDataSet);
  strict private
    FcxTreeCanFocusNode, FcxTreeFocusedNodeChanged: TfsProcVariable;
  protected
    { Protected declarations }
    procedure InternalCreate; override;
    procedure InternalInit; override;

    procedure LoadUserConfig(AStoreFormSettings: TFormItemsForSave); override;
    procedure SaveUserConfig(AStoreFormSettings: TFormItemsForSave); override;

    procedure InternalGetParameters(ALevel: UInt16; var AParameters: TNamedVariants; var ACollisions: Boolean); override;

    procedure OpenDataSet; override;
  private
    { Private declarations }
  public
    { Public declarations }
    Expand   : Byte;
    Expanded : Byte;
    CanSelect: Byte;
  end;

implementation

uses SBaseConstants, SBaseVariantFunctions, SBaseCxUtils, SBaseStringFunctions,
  SBaseUtils, SBaseItemReactions;

{$R *.dfm}

{ TSBaseCxTreeViewFrm }

procedure TSBaseCxTreeViewFrm.InternalCreate;
begin
  Expand    := 0;
  Expanded  := 0;
  CanSelect := 0;

  GroupSetProvider.ConnectionProvider := SQLConnectionProvider;
  TreeSetProvider.ConnectionProvider := SQLConnectionProvider;

  if not cxTree.Configured then begin
    if not SBaseFormProperties.Font_Inspector.IsEmpty then
      cxTree.Font.Name := SBaseFormProperties.Font_Inspector;
    cxTree.Configure := ScxMDTreeListInspectorConfigure;
  end;

  cxTree.Images := SBaseFormProperties.Icon16List;

  mnTree.Caption := SBaseProperties.Text_MenuShowTree;
  mnTree.ImageIndex := SBaseFormProperties.IconIndex_ShowTree;

  mnFindInTree.Caption := SBaseProperties.Text_MenuFindInTree;
  mnFindInTree.ImageIndex := SBaseFormProperties.IconIndex_FindInTree;

  inherited;

  if mnTreeChildsIncluded.Visible <> ivNever then begin
    with ItemsReactions.FindOrCreateReaction(mnFindInTree) do begin
      LinkedComponent := InternalGetGrid;
      OnGetEnabled.SystemFunction := mnFindInTreeAllowed;
      OnClick.Reaction := biraCustomDataSourceRecord;
    end;
  end;

  FcxTreeCanFocusNode       := PrepareFunction('cxTreeCanFocusNode', False);
  FcxTreeFocusedNodeChanged := PrepareFunction('cxTreeFocusedNodeChanged', False);
end;

procedure TSBaseCxTreeViewFrm.InternalInit;
begin
  inherited;

  with GroupSetProvider do
    if (SQL.Text <> '') and (not Active) then
      Open;
end;

procedure TSBaseCxTreeViewFrm.FormShow(Sender: TObject);
begin
//  if mnTree.Visible <> ivNever then
  mnTree.OnClick(mnTree);

  if mnTreeChildsIncluded.Visible <> ivNever then
    mnTreeChildsIncluded.OnClick(mnTreeChildsIncluded);

  inherited;
end;

procedure TSBaseCxTreeViewFrm.GroupSetProviderAfterOpen(DataSet: TDataSet);
var
  FGroup: String;
begin
  mnGroups.Items.Clear;
  while not GroupSetProvider.Eof do begin
    mnGroups.Items.Add(GroupSetProvider.FieldByName(SConst_Title).AsString);
    GroupSetProvider.Next
  end;

  if GroupSetProvider.RecordCount > 1 then begin
    FGroup := VarToStr(GetSetting(SConst_Group));
    if (FGroup = '') or not GroupSetProvider.Locate(SConst_Code, FGroup, [loCaseInsensitive]) then
      GroupSetProvider.RecNo := 1;

  end else begin
    btnTreeTypeChange.Visible := False;
    LbGroups.Width := LbGroups.Width + LbGroups.Left;
    LbGroups.Left  := 5;
  end;

  if GroupSetProvider.RecordCount > 0 then
    mnGroupsClick(nil);
end;

procedure TSBaseCxTreeViewFrm.LoadUserConfig(AStoreFormSettings: TFormItemsForSave);
begin
  inherited;

  if frsvUserDefined in AStoreFormSettings then begin
    PanelLeft.Width := VarToIntDef(GetSetting(PanelLeft.Name), PanelLeft.Width);
    if mnTree.Visible <> ivNever then
      mnTree.Down  := VarToBoolDef(GetSetting(SConst_Tree), mnTree.Down);
  end;
end;

procedure TSBaseCxTreeViewFrm.SaveUserConfig(AStoreFormSettings: TFormItemsForSave);
begin
  inherited;

  if frsvUserDefined in AStoreFormSettings then begin
    SetSetting(PanelLeft.Name , PanelLeft.Width);
    if mnTree.Visible <> ivNever then
      SetSetting(SConst_Tree , mnTree.Down);
  end
end;

procedure TSBaseCxTreeViewFrm.InternalGetParameters(ALevel: UInt16; var AParameters: TNamedVariants; var ACollisions: Boolean);
var
  LFocusedNode: TcxTreeListNode;
begin
  if ALevel > TreeSetProvider.Level then begin
    LFocusedNode := cxTree.FocusedNode;

    if TreeSetProvider.Active and Assigned(LFocusedNode) and (DataSetProvider.MasterFields <> '') then
      if cxTree.OptionsData.SyncMode then
        AParameters[DataSetProvider.DetailFields] := TreeSetProvider.Fields.GetValues(DataSetProvider.MasterFields)
      else
        AParameters[DataSetProvider.DetailFields] := cxTree.GetRecordValues(LFocusedNode.AbsoluteIndex, DataSetProvider.MasterFields);

    inherited;

    ACollisions := ACollisions or (LFocusedNode = nil) or (LFocusedNode.Level < CanSelect);
  end else
    inherited;
end;

procedure TSBaseCxTreeViewFrm.OpenDataSet;
begin
  if (cxTree.FocusedNode <> nil) and (cxTree.FocusedNode.Level >= CanSelect) then
    inherited;
end;

procedure TSBaseCxTreeViewFrm.cxTreeCanFocusNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
var
  LParams: TNamedVariants;
begin
  Allow := Loading or (ANode = nil) or (ANode.Level >= CanSelect);
  if Assigned(FcxTreeCanFocusNode) then begin
    LParams := TNamedVariants.Create
               (
                [
                  TNamedVariant.Create(SConst_Sender, CastAsVariant(Sender)),
                  TNamedVariant.Create(SConst_Node, CastAsVariant(ANode)),
                  TNamedVariant.Create(SConst_Allow, Allow)
                ],
                True
               );
    CallFunctionOut(FcxTreeCanFocusNode, LParams);
    Allow := (LParams[SConst_Allow] = True);
  end;
end;

procedure TSBaseCxTreeViewFrm.cxTreeFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
var
  Text: String;
  Node: TcxTreeListNode;
  LGridTableView: TcxGridTableView;
  LOptionsView: TcxGridTableOptionsView;
  LNotActiveInfoText: String;
begin
  if DataSetProvider.MasterFields <> '' then begin
    if AFocusedNode = nil then
      Text := 'Выбор папки не произведён'
    else begin
      Text := AFocusedNode.Texts[0];
      Node := AFocusedNode;
      while (Node.Level > 0) and (Node.Parent <> nil) do begin
        Node := Node.Parent;
        Text := Node.Texts[0] + ' :: ' + Text;
      end;
    end;
    LbTreeNode.Caption := Text;

    AfterFilterChanged;

    LGridTableView := TcxGridTableView(cxGrid.ActiveView);
{$IFDEF DEBUG}
    if Assigned(LGridTableView) then begin
{$ENDIF}
      if (AFocusedNode = nil) or (AFocusedNode.Level < CanSelect) then
        LNotActiveInfoText := SBaseProperties.Message_TreeFilterNotPresent
      else
        LNotActiveInfoText := SBaseProperties.Message_DataSetNotActive;

      LOptionsView := LGridTableView.OptionsView;
      if LOptionsView is TcxMDGridTableOptionsView then
        TcxMDGridTableOptionsView(LOptionsView).NotActiveInfoText := LNotActiveInfoText
      else if LOptionsView is TcxMDGridBandedTableOptionsView then
        TcxMDGridBandedTableOptionsView(LOptionsView).NotActiveInfoText := LNotActiveInfoText;
{$IFDEF DEBUG}
    end;
{$ENDIF}

    CallFunction
    (
      FcxTreeFocusedNodeChanged,
      TNamedVariants.Create
      (
        [
          TNamedVariant.Create(SConst_Sender, CastAsVariant(Sender)),
          TNamedVariant.Create(SConst_APrevFocusedNode, CastAsVariant(APrevFocusedNode)),
          TNamedVariant.Create(SConst_AFocusedNode, CastAsVariant(AFocusedNode))
        ],
        True
      )
    );
  end;
end;

procedure TSBaseCxTreeViewFrm.cxTreeGetNodeImageIndex(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
var
  LIndex: Variant;
begin
  if (AIndexType = tlitSelectedIndex) and (AIndex >= 0) and ANode.Focused then begin
    LIndex := cxTreeFocusedImage.Values[ANode];
    if VarIsOrdinal(LIndex) then
      AIndex := LIndex
  end;
end;

procedure TSBaseCxTreeViewFrm.mnTreeClick(Sender: TObject);
begin
  if not mnTree.Down then begin
    SplitterLeft.Visible := False;
    PanelLeft.Visible    := False;

    if cxTree.AbsoluteCount > 0 then
      cxTree.AbsoluteItems[0].Focused := True;
  end else begin
    SplitterLeft.Visible := True;
    PanelLeft.Visible    := True;
  end;
end;

procedure TSBaseCxTreeViewFrm.mnTreeChildsIncludedClick(Sender: TObject);
begin
//
end;

procedure TSBaseCxTreeViewFrm.mnFindInTreeClick(Sender: TObject);
begin
//
end;

function TSBaseCxTreeViewFrm.mnFindInTreeAllowed(Sender: TObject): Boolean;
begin
  Result := (mnTreeChildsIncluded.Visible <> ivNever) and mnTreeChildsIncluded.Down;
end;

procedure TSBaseCxTreeViewFrm.cxTreeCollapsing(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
begin
  Allow := Loading or (ANode = nil) or (Expanded = 0) or (ANode.Level > Expanded)
end;

procedure TSBaseCxTreeViewFrm.cxTreeCustomDrawDataCell(
  Sender: TcxCustomTreeList; ACanvas: TcxCanvas;
  AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
begin
  if AViewInfo.Node.Focused then
    ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline];
end;

procedure TSBaseCxTreeViewFrm.mnGroupsClick(Sender: TObject);
var
  LMasterFields, LDetailFields: String;
//  Index: Integer;
  Loading_Saved: Boolean;

  I: Integer;
  LNode: TcxTreeListNode;
begin
  if Sender <> nil then
    GroupSetProvider.RecNo := Succ(mnGroups.ItemIndex);

  Loading_Saved := Loading;
  Loading := True;

  try
    if not GroupSetProvider.IsEmpty then begin
//      TreeSetProvider.Close;

      LbGroups.Font.Size := 10;
      LbGroups.Caption  := GroupSetProvider.FieldByName(SConst_Title).AsString;
      if LbGroups.Canvas.TextWidth(LbGroups.Caption) > LbGroups.Width then
        LbGroups.Font.Size := 8;

      TreeSetProvider.SQL.Text := GroupSetProvider.FieldByName(SConst_SQL).AsString;

      Expand            := GroupSetProvider.FieldByName(SConst_Expand).AsInteger;
      Expanded          := GroupSetProvider.FieldByName(SConst_Expanded).AsInteger;
      CanSelect         := GroupSetProvider.FieldByName(SConst_CanSelect).AsInteger;

      // Временная заглушка
      if Assigned(GroupSetProvider.FindField(SConst_MasterFields)) then begin
        DataSetProvider.MasterFields := GroupSetProvider.FieldByName(SConst_MasterFields).AsString;
        DataSetProvider.DetailFields := GroupSetProvider.FieldByName(SConst_DetailFields).AsString;
      end else begin
        ExtractNameAndAlias(GroupSetProvider.FieldByName(SConst_Fields).AsString, @LDetailFields, @LMasterFields);
        DataSetProvider.MasterFields := LMasterFields;
        DataSetProvider.DetailFields := LDetailFields;
      end;

      SetSetting(SConst_Group, GroupSetProvider.FieldByName(SConst_Code).AsString);
    end;

    TreeSetProvider.ReOpen([rofOnlyIfChanged, rofCloseIfParamsRequired]);

    if cxTree.AbsoluteCount > 0 then // Call check changes
      if Expand = 255 then
        cxTree.FullExpand
      else if Expand > 0 then
        for I := Pred(cxTree.Count) downto 0 do with cxTree.Items[I] do // Возможно ошибка - Absolute* ?
          if (not Expanded) and (Level < Self.Expand) then
            Expand(False);

    LNode := cxTree.Root;
    if (LNode.Count = 1) then begin
      repeat
        LNode := LNode.Items[0];
      until (LNode.Count <> 1) or (LNode.Level >= CanSelect);
      LNode.Focused := True;
    end else begin
      LNode := cxTree.TopNode;
      if Assigned(LNode) then
        LNode.Focused := True;
    end;

  finally
    Loading := Loading_Saved;
  end;

  cxTreeFocusedNodeChanged(cxTree, nil, cxTree.FocusedNode);
end;

procedure TSBaseCxTreeViewFrm.btnTreeTypeChangeClick(Sender: TObject);
begin
  PopupMenuGroups.PopupFromCursorPos;
end;

procedure TSBaseCxTreeViewFrm.PanelGridResize(Sender: TObject);
begin
  inherited;
{
  if LabelTreeSelectRequire <> nil then with LabelTreeSelectRequire do begin
    Top  := (DBGrid.Height shr 1) - 10;
    Left := (DBGrid.Width - LabelTreeSelectRequire.Canvas.TextWidth(LabelTreeSelectRequire.Caption)) shr 1;
  end;
}
end;

initialization
  RegisterClass(TSBaseCxTreeViewFrm);

end.
