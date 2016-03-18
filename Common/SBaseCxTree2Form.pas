unit SBaseCxTree2Form;
{$I config.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SBaseCxTreeForm, cxGraphics, cxCustomData, cxStyles, cxTL, ImgList,
  cxCalendar, ExtCtrls, DB, dxBar, SBaseDataModule, Math,
  cxBarEditItem, cxClasses, cxInplaceContainer, cxTLData, cxDBTL, StdCtrls, NamedVariables,
  cxControls, dxStatusBar, cxTextEdit, Buttons, cxLookAndFeels,
  cxLookAndFeelPainters, cxTLdxBarBuiltInMenu, SBaseForm, cxMaskEdit,
  cxContainer, cxEdit, cxSplitter, cxGroupBox, System.Actions, ActnList,
  LayoutPanel, dxSkinsCore, dxSkinsDefaultPainters, dxMDBarButton, dxMDBar,
  cxMDDBTreeList, dxMDStatusBar
{$IFDEF DELPHIXE3}
  , System.UITypes
{$ENDIF}
;

type
  TSBaseCxTree2Frm = class(TSBaseCxTreeFrm)
    mnTree: TdxBarButton;
    mnTreeChildsIncluded: TdxBarButton;
    mnFindInTree: TdxBarButton;
    mnGroups: TdxBarListItem;
    PopupMenuGroups: TdxBarPopupMenu;

    PanelLeft: TcxGroupBox;
    cxTree: TcxMDDBTreeList;
    cxTreeStandardColumn: TcxMDDBTreeListColumn;
    cxTreeFocusedImage: TcxMDDBTreeListColumn;
    PanelLeftCaption: TPanel;
    LbGroups: TLabel;
    btnTreeTypeChange: TSpeedButton;
    PanelLeftTopGray: TPanel;
    SplitterLeft: TSplitter;
    PanelMainCaption: TPanel;
    LbTreeNode: TLabel;
    PanelMainTopGray: TPanel;
    TreeSource: TDataSource;
    TreeSetProvider: TSQLDataSetProvider;
    GroupSetProvider: TSQLDataSetProvider;

    procedure FormShow(Sender: TObject); override;
    procedure mnTreeClick(Sender: TObject);
    function  mnFindInTreeAllowed(Sender: TObject): Boolean;
    procedure mnFindInTreeClick(Sender: TObject);
    procedure mnTreeChildsIncludedClick(Sender: TObject);
    procedure cxTreeCanFocusNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
    procedure cxTreeFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    procedure cxTreeCollapsing(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
    procedure cxTreeCustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
    procedure mnGroupsClick(Sender: TObject);
    procedure btnTreeTypeChangeClick(Sender: TObject);
    procedure PanelGridResize(Sender: TObject);
    procedure cxTreeGetNodeImageIndex(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
    procedure GroupSetProviderAfterOpen(DataSet: TDataSet);
  private
    { Private declarations }
    LabelTreeSelectRequire: TLabel;
  protected
    { Protected declarations }
    procedure InternalCreate; override;
    procedure InternalInit; override;

    procedure LoadUserConfig(AStoreFormSettings: TFormItemsForSave); override;
    procedure SaveUserConfig(AStoreFormSettings: TFormItemsForSave); override;

    procedure InternalGetParameters(ALevel: UInt16; var AParameters: TNamedVariants; var ACollisions: Boolean); override;

    procedure OpenDataSet; override;
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

procedure TSBaseCxTree2Frm.InternalCreate;
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

  LabelTreeSelectRequire := TLabel.Create(cxDBTreeList);
  with LabelTreeSelectRequire do begin
    Visible       := False;
    Transparent   := True;
    Caption       := SBaseProperties.Message_TreeFilterNotPresent;
    Font.Color    := clRed;
    Font.Style    := [fsBold];
    Parent        := cxDBTreeList;
  end;

  inherited;

  if mnTreeChildsIncluded.Visible <> ivNever then begin
    with ItemsReactions.FindOrCreateReaction(mnFindInTree) do begin
      LinkedComponent := InternalGetGrid;
      OnGetEnabled.SystemFunction := mnFindInTreeAllowed;
      OnClick.Reaction := biraCustomDataSourceRecord;
    end;
  end;
end;

procedure TSBaseCxTree2Frm.InternalInit;
begin
  inherited;

  with GroupSetProvider do
    if (SQL.Text <> '') and (not Active) then
      Open;
end;

procedure TSBaseCxTree2Frm.FormShow(Sender: TObject);
begin
//  if mnTree.Visible <> ivNever then
  mnTree.OnClick(mnTree);

  if mnTreeChildsIncluded.Visible <> ivNever then
    mnTreeChildsIncluded.OnClick(mnTreeChildsIncluded);

  inherited;
end;

procedure TSBaseCxTree2Frm.GroupSetProviderAfterOpen(DataSet: TDataSet);
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

procedure TSBaseCxTree2Frm.LoadUserConfig(AStoreFormSettings: TFormItemsForSave);
begin
  inherited;

  if frsvUserDefined in AStoreFormSettings then begin
    PanelLeft.Width := VarToIntDef(GetSetting(PanelLeft.Name), PanelLeft.Width);
    if mnTree.Visible <> ivNever then
      mnTree.Down  := VarToBoolDef(GetSetting(SConst_Tree), mnTree.Down);
  end;
end;

procedure TSBaseCxTree2Frm.SaveUserConfig(AStoreFormSettings: TFormItemsForSave);
begin
  inherited;

  if frsvUserDefined in AStoreFormSettings then begin
    SetSetting(PanelLeft.Name, PanelLeft.Width);
    if mnTree.Visible <> ivNever then
      SetSetting(SConst_Tree , mnTree.Down);
  end
end;

procedure TSBaseCxTree2Frm.InternalGetParameters(ALevel: UInt16; var AParameters: TNamedVariants; var ACollisions: Boolean);
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

procedure TSBaseCxTree2Frm.OpenDataSet;
begin
  if (cxTree.FocusedNode <> nil) and (cxTree.FocusedNode.Level >= CanSelect) then
    inherited;
end;

procedure TSBaseCxTree2Frm.cxTreeCanFocusNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
begin
  Allow := Loading or (ANode = nil) or (ANode.Level >= CanSelect)
end;

procedure TSBaseCxTree2Frm.cxTreeFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
var
  Text: String;
  Node: TcxTreeListNode;
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
    LabelTreeSelectRequire.Visible := (AFocusedNode = nil) or (AFocusedNode.Level < CanSelect);
    CallFunction
    (
      'cxTreeFocusedNodeChanged',
      [
        TNamedVariant.Create(SConst_Sender, CastAsVariant(Sender)),
        TNamedVariant.Create(SConst_APrevFocusedNode, CastAsVariant(APrevFocusedNode)),
        TNamedVariant.Create(SConst_AFocusedNode, CastAsVariant(AFocusedNode))
      ],
      False
    );
  end;

end;

procedure TSBaseCxTree2Frm.cxTreeGetNodeImageIndex(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
var
  LIndex: Variant;
begin
  if (AIndexType = tlitSelectedIndex) and (AIndex >= 0) and ANode.Focused then begin
    LIndex := cxTreeFocusedImage.Values[ANode];
    if VarIsOrdinal(LIndex) then
      AIndex := LIndex
  end;
end;

procedure TSBaseCxTree2Frm.mnTreeClick(Sender: TObject);
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

function TSBaseCxTree2Frm.mnFindInTreeAllowed(Sender: TObject): Boolean;
begin
  Result := (mnTreeChildsIncluded.Visible <> ivNever) and mnTreeChildsIncluded.Down;
end;

procedure TSBaseCxTree2Frm.mnFindInTreeClick(Sender: TObject);
begin
//
end;

procedure TSBaseCxTree2Frm.mnTreeChildsIncludedClick(Sender: TObject);
begin
//
end;

procedure TSBaseCxTree2Frm.cxTreeCollapsing(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
begin
  Allow := Loading or (ANode = nil) or (Expanded = 0) or (ANode.Level > Expanded)
end;

procedure TSBaseCxTree2Frm.cxTreeCustomDrawDataCell(Sender: TcxCustomTreeList;
  ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo;
  var ADone: Boolean);
begin
  if AViewInfo.Node.Focused then
    ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline];
end;

procedure TSBaseCxTree2Frm.mnGroupsClick(Sender: TObject);
var
  LMasterFields, LDetailFields: String;
//  Index: Integer;
  Loading_Saved: Boolean;

  I: Integer;
  Node: TcxTreeListNode;
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
    end;

    TreeSetProvider.ReOpen([rofOnlyIfChanged, rofCloseIfParamsRequired]);

    if cxTree.AbsoluteCount > 0 then // Call check changes
      if Expand = 255 then
        cxTree.FullExpand
      else if Expand > 0 then
        for I := Pred(cxTree.Count) downto 0 do with cxTree.Items[I] do
          if (not Expanded) and (Level < Self.Expand) then
            Expand(False);

    Node := cxTree.Root;
    if (cxTree.Count > 0) and (Node.Count = 1) then begin
      repeat
        Node := Node.Items[0];
      until (Node.Count <> 1);
      Node.Focused := True;
    end;

  finally
    Loading := Loading_Saved;
  end;

  cxTreeFocusedNodeChanged(cxTree, nil, cxTree.FocusedNode);
end;

procedure TSBaseCxTree2Frm.btnTreeTypeChangeClick(Sender: TObject);
begin
  PopupMenuGroups.PopupFromCursorPos;
end;

procedure TSBaseCxTree2Frm.PanelGridResize(Sender: TObject);
begin
  inherited;

  if LabelTreeSelectRequire <> nil then with LabelTreeSelectRequire do begin
    Top  := Max((cxDBTreeList.Height shr 1) - 10, 0);
    Left := Max(cxDBTreeList.Width - LabelTreeSelectRequire.Canvas.TextWidth(LabelTreeSelectRequire.Caption), 0) shr 1;
  end;
end;

initialization
  RegisterClass(TSBaseCxTree2Frm);

finalization
  UnRegisterClass(TSBaseCxTree2Frm);

end.

