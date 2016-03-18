unit SBaseCxView2Form;
{$I config.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SBaseFormDefinitions, SBaseForm, SBaseCxViewForm, SBaseItemReactions,
  cxGraphics, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, ExtCtrls, dxBar,
  cxGridLevel, cxGridBandedTableView, cxGridDBBandedTableView,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, cxGridDBDataDefinitions,
  cxClasses, cxControls, DBConsts, dxStatusBar, NamedVariables, cxCalendar, cxBarEditItem,
  cxContainer, SBaseDataModule, cxSplitter, cxGroupBox, cxLookAndFeels, cxLookAndFeelPainters,
  cxNavigator, System.Actions, ActnList, LayoutPanel, dxMDBarButton, dxMDBar,
  cxMDGrid, dxMDStatusBar, dxSkinsCore, dxSkinsDefaultPainters;

type
  TSBaseCxView2Frm = class(TSBaseCxViewFrm)
    mnNew2: TdxBarButton;
    mnNewX: TdxBarSubItem;

    mnView2: TdxBarButton;
    mnViewX: TdxBarButton;

    mnEdit2: TdxBarButton;
    mnEditX: TdxBarButton;

    mnCopy2: TdxBarButton;
    mnCopyX: TdxBarButton;

    mnDelete2: TdxBarButton;
    mnDeleteX: TdxBarButton;

    mnMarkAsDeleted2: TdxBarButton;
    mnMarkAsDeletedX: TdxBarButton;

    mnRefreshRecord2: TdxBarButton;
    mnRefreshRecordX: TdxBarButton;

    cxGridLevel2: TcxMDGridLevel;
    DataSource2: TDataSource;
    DataSetProvider2: TSQLDataSetProvider;

    procedure GridAfterUpdateActions(Sender: TObject); override;
    procedure Grid2AfterUpdateActions(Sender: TObject);
    procedure CxGridAfterUpdateActions(Sender: TObject); dynamic;

    procedure DataSetBeforeOpen(DataSet: TDataSet); override;
//    procedure DataSetAfterClose(DataSet: TDataSet); override;

    procedure mnFitToHeightClick(Sender: TObject); override;
    procedure cxGridDBView2FocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);

    procedure mnViewXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); dynamic;
    procedure mnEditXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); dynamic;
    procedure mnCopyXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); dynamic;
    procedure mnDeleteXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); dynamic;
    procedure mnMarkAsDeletedXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); dynamic;
    procedure mnRefreshRecordXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); dynamic;
    procedure cxGridLevel2ViewChanged(Sender: TObject);
    procedure cxGridLevel2AddView(AView: TcxCustomGridView);
  private
    { Private declarations }
    FRecordsPrivileges2: Word;
    FRecordsPrivileges2Stored: Boolean;
    FDeletedCondition2: String;

    procedure SetRecordsPrivileges2(const Value: Word);
  protected
    { Protected declarations }
    procedure InternalCreate; override;
    procedure InternalInit; override;

    procedure CloseDataSet; override;
//    function  InternalGetGrid: TComponent; override; // Должен оставаться cxGridLevel.GridView
  public
    { Public declarations }
  published
    { Published declarations }
    { Битовая маска прав на записи SELECT / INSERT / UPDATE / DELETE }
    property RecordsPrivileges2: Word read FRecordsPrivileges2 write SetRecordsPrivileges2;
    property DeletedCondition2: String read FDeletedCondition2 write FDeletedCondition2;
  end;

implementation

uses SBaseXXViewForm, SBaseStringFunctions, SBaseConstants, SBaseVariantFunctions,
  SBaseDxUtils, SBaseUtils, SBaseDataSourceControls;

{$R *.dfm}

procedure TSBaseCxView2Frm.InternalCreate;
var
  LLinkedComponent2: TComponent;
begin
  mnNew.Caption   := SBaseProperties.Text_MenuNewElement;
  mnNew2.Caption  := SBaseProperties.Text_MenuNewSubElement;
  mnNewX.Caption  := SBaseProperties.Text_MenuNewElement;
  mnView2.Caption := SBaseProperties.Text_MenuView;
  mnViewX.Caption := SBaseProperties.Text_MenuView;
  mnEdit2.Caption := SBaseProperties.Text_MenuEdit;
  mnEditX.Caption := SBaseProperties.Text_MenuEdit;
  mnCopy2.Caption := SBaseProperties.Text_MenuCopy;
  mnCopyX.Caption := SBaseProperties.Text_MenuCopy;
  mnDelete2.Caption := SBaseProperties.Text_MenuDelete;
  mnDeleteX.Caption := SBaseProperties.Text_MenuDelete;
  mnMarkAsDeleted2.Caption := SBaseProperties.Text_MenuMarkAsDeleted;
  mnMarkAsDeleted2.ImageIndex := SBaseFormProperties.IconIndex_MarkAsDeleted;
  mnMarkAsDeletedX.Caption := SBaseProperties.Text_MenuMarkAsDeleted;
  mnRefreshRecord2.Caption := SBaseProperties.Text_MenuRefreshRecord;
  mnRefreshRecordX.Caption := SBaseProperties.Text_MenuRefreshRecord;

  inherited;

  if mnNew2.ImageIndex = -1 then
    mnNew2.ImageIndex := SBaseFormProperties.IconIndex_New;

  if mnNewX.ImageIndex = -1 then
    mnNewX.ImageIndex := SBaseFormProperties.IconIndex_New;
  if mnViewX.ImageIndex = -1 then
    mnViewX.ImageIndex := SBaseFormProperties.IconIndex_View;
  if mnEditX.ImageIndex = -1 then
    mnEditX.ImageIndex := SBaseFormProperties.IconIndex_Edit;
  if mnDeleteX.ImageIndex = -1 then
    mnDeleteX.ImageIndex := SBaseFormProperties.IconIndex_Delete;

  if not FRecordsPrivileges2Stored then
    FRecordsPrivileges2 := Self.FPrivileges and Privileges_ForRecord;

  LLinkedComponent2 := cxGridLevel2.GridView;

  with ItemsReactions.FindOrCreateReaction(mnRefreshRecord2) do begin
    LinkedComponent := LLinkedComponent2;
    OnClick.Reaction := biraDataSourceRefreshRecord;
  end;

  with ItemsReactions.FindOrCreateReaction(mnViewX) do begin
    //LinkedComponent := cxGrid;
    Default := True;
    OnClick.Reaction := biraCustom;
    OnClick.SystemProcedure := mnViewXClick;
  end;

  with ItemsReactions.FindOrCreateReaction(mnEditX) do begin
    //LinkedComponent := cxGrid;
    OnClick.Reaction := biraCustom;
    OnClick.SystemProcedure := mnEditXClick;
  end;

  with ItemsReactions.FindOrCreateReaction(mnCopyX) do begin
    //LinkedComponent := cxGrid;
    OnClick.Reaction := biraCustom;
    OnClick.SystemProcedure := mnCopyXClick;
  end;

  with ItemsReactions.FindOrCreateReaction(mnRefreshRecordX) do begin
    //LinkedComponent := cxGrid;
    OnClick.Reaction := biraCustom;
    OnClick.SystemProcedure := mnRefreshRecordXClick;
  end;

  with ItemsReactions.FindOrCreateReaction(mnMarkAsDeletedX) do begin
    //LinkedComponent := cxGrid;
    OnClick.Reaction := biraCustom;
    OnClick.SystemProcedure := mnMarkAsDeletedXClick;
  end;

  with ItemsReactions.FindOrCreateReaction(mnDeleteX) do begin
    //LinkedComponent := cxGrid;
    OnClick.Reaction := biraCustom;
    OnClick.SystemProcedure := mnDeleteXClick;
  end;

  if (not FDeletedCondition2.IsEmpty) then begin
//    mnMarkAsDeleted2.Caption := SBaseProperties.Text_MenuMarkAsDeleted;
    ItemsReactions.InitReaction(mnMarkAsDeleted2, LLinkedComponent2, biraCustomDataSourceRecord);
  end;

//  FDataSourceControls.FindOrCreateGridControl(cxGridLevel2.GridView).AfterUpdate := Grid2AfterUpdateActions;
  cxGridLevelViewChanged(cxGridLevel2);
  FDataSourceControls.FindOrCreateGridControl(cxGrid).AfterUpdate := CxGridAfterUpdateActions;

  if (DataSetProvider2.DataSetObject <> bdsoMemory) and (not Assigned(DataSetProvider2.ConnectionProvider)) then
    DataSetProvider2.ConnectionProvider := SQLConnectionProvider;

  TcxGridDBDataController(cxGridLevel2.GridView.DataController).DataSource := DataSource2;
end;

procedure TSBaseCxView2Frm.InternalInit;
begin
  RecordsPrivileges2 := RecordsPrivileges2;
  inherited;
end;

procedure TSBaseCxView2Frm.GridAfterUpdateActions(Sender: TObject);
begin
  inherited;

  mnNewX.CheckEnabled;

  if (cxGrid.ActiveView = cxGrid.FocusedView) then begin
    mnViewX.Enabled             := mnView.Enabled;
    mnEditX.Enabled             := mnEdit.Enabled;
    mnCopyX.Enabled             := mnCopy.Enabled;
    mnDeleteX.Enabled           := mnDelete.Enabled;

    mnRefreshRecordX.Enabled    := mnRefreshRecord.Enabled;

    mnMarkAsDeletedX.Enabled    := mnMarkAsDeleted.Enabled;
    mnMarkAsDeletedX.Caption    := mnMarkAsDeleted.Caption;
    mnMarkAsDeletedX.ImageIndex := mnMarkAsDeleted.ImageIndex;
    mnMarkAsDeletedX.Tag        := mnMarkAsDeleted.Tag;
    mnMarkAsDeletedX.ShortCut   := mnMarkAsDeleted.ShortCut;
  end;
end;

procedure TSBaseCxView2Frm.Grid2AfterUpdateActions(Sender: TObject);
var
  LFocusedRecord: TcxCustomGridRecord;
begin
  LFocusedRecord := TcxGridTableView(Sender).Controller.FocusedRecord;
  if FDeletedCondition2 <> '' then
    if (Sender = nil)
          or (LFocusedRecord = nil)
          or not (LFocusedRecord is TcxGridDataRow)
          or (not TcxGridDBDataController(TcxGridTableView(Sender).DataController).DataSet.EvaluateBoolean(FDeletedCondition2))
    then begin
      mnMarkAsDeleted2.Caption    := SBaseProperties.Text_MenuMarkAsDeleted;
      mnMarkAsDeleted2.ImageIndex := SBaseFormProperties.IconIndex_MarkAsDeleted;
      mnMarkAsDeleted2.Tag        := 1;
      mnMarkAsDeleted2.ShortCut   := ShortCut_Shift_Del;
    end else begin
      mnMarkAsDeleted2.Caption    := SBaseProperties.Text_MenuUnMarkFromDeleted;
      mnMarkAsDeleted2.ImageIndex := SBaseFormProperties.IconIndex_UnMarkFromDeleted;
      mnMarkAsDeleted2.Tag        := 0;
      mnMarkAsDeleted2.ShortCut   := 0;
    end;

  mnNewX.CheckEnabled;

  if (cxGridLevel2.GridView = cxGrid.FocusedView.PatternGridView) then begin
    mnViewX.Enabled             := mnView2.Enabled;
    mnEditX.Enabled             := mnEdit2.Enabled;
    mnCopyX.Enabled             := mnCopy2.Enabled;
    mnDeleteX.Enabled           := mnDelete2.Enabled;

    mnRefreshRecordX.Enabled    := mnRefreshRecord2.Enabled;

    mnMarkAsDeletedX.Enabled    := mnMarkAsDeleted2.Enabled;
    mnMarkAsDeletedX.Caption    := mnMarkAsDeleted2.Caption;
    mnMarkAsDeletedX.ImageIndex := mnMarkAsDeleted2.ImageIndex;
    mnMarkAsDeletedX.Tag        := mnMarkAsDeleted2.Tag;
    mnMarkAsDeletedX.ShortCut   := mnMarkAsDeleted2.ShortCut;
  end;
end;

procedure TSBaseCxView2Frm.CloseDataSet;
begin
  if DataSetProvider2 <> nil then begin
    DataSetProvider2.Close;
    DataSetProvider2.UnPrepare;
  end;

  inherited;
end;

procedure TSBaseCxView2Frm.CxGridAfterUpdateActions(Sender: TObject);
begin
  if (cxGrid.ActiveView = cxGrid.FocusedView) then
    GridAfterUpdateActions(cxGrid.FocusedView)
  else if (cxGrid.FocusedView.PatternGridView = cxGridLevel2.GridView) then
    Grid2AfterUpdateActions(cxGrid.FocusedView)
end;

{
procedure TSBaseCxView2Frm.DataSetAfterClose(DataSet: TDataSet);
begin
  inherited;

//  if not TcxGridDBDataController(cxGridLevel2.GridView.DataController).DataModeController.DetailInSQLMode then
  if DataSetProvider2.DataSetObject <> bdsoMemory then
    DataSetProvider2.Close;
end;
}

procedure TSBaseCxView2Frm.DataSetBeforeOpen(DataSet: TDataSet);
var
  I: Integer;
  LParam: TParam;
begin
  inherited;

//  if not TcxGridDBDataController(cxGridLevel2.GridView.DataController).DataModeController.DetailInSQLMode then begin

  if DataSetProvider2.DataSetObject <> bdsoMemory then begin
    for I := Pred(DataSetProvider2.Params.Count) downto 0 do with DataSetProvider2.Params[I] do begin
      LParam := DataSetProvider.Params.FindParam(Name);
      if LParam <> nil then
        Value := LParam.Value
      else
        Clear
    end;
    DataSetProvider2.Open;
  end;

//  end;
end;

procedure TSBaseCxView2Frm.mnViewXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  if cxGrid.FocusedView.PatternGridView = cxGridLevel2.GridView then
    mnView2.Click
  else if cxGrid.FocusedView = cxGridLevel.GridView then
    mnView.Click
end;

procedure TSBaseCxView2Frm.mnEditXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  if cxGrid.FocusedView.PatternGridView = cxGridLevel2.GridView then
    mnEdit2.Click
  else if cxGrid.FocusedView = cxGridLevel.GridView then
    mnEdit.Click
end;

procedure TSBaseCxView2Frm.mnRefreshRecordXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  if cxGrid.FocusedView.PatternGridView = cxGridLevel2.GridView then
    mnRefreshRecord2.Click
  else if cxGrid.FocusedView = cxGridLevel.GridView then
    mnRefreshRecord.Click
end;

procedure TSBaseCxView2Frm.mnMarkAsDeletedXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  if cxGrid.FocusedView.PatternGridView = cxGridLevel2.GridView then
    mnMarkAsDeleted2.Click
  else if cxGrid.FocusedView = cxGridLevel.GridView then
    mnMarkAsDeleted.Click
end;

procedure TSBaseCxView2Frm.mnCopyXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  if cxGrid.FocusedView.PatternGridView = cxGridLevel2.GridView then
    mnCopy2.Click
  else if cxGrid.FocusedView = cxGridLevel.GridView then
    mnCopy.Click
end;

procedure TSBaseCxView2Frm.mnDeleteXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  if cxGrid.FocusedView.PatternGridView = cxGridLevel2.GridView then
    mnDelete2.Click
  else if cxGrid.FocusedView = cxGridLevel.GridView then
    mnDelete.Click
end;

procedure TSBaseCxView2Frm.mnFitToHeightClick(Sender: TObject);
begin
  inherited;

  TcxGridTableView(cxGridLevel2.GridView).OptionsView.CellAutoHeight := mnFitToHeight.Down;
end;

procedure TSBaseCxView2Frm.cxGridDBView2FocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
begin
  if InternalGetControlsEnabled then begin
    ShowDetails;
  end;
end;

procedure TSBaseCxView2Frm.cxGridLevel2AddView(AView: TcxCustomGridView);
begin
  AView.PopupMenu := PopupMenu;
  with TcxGridTableView(AView) do begin
//    OnCanSelectRecord       := cxGridDBViewCanSelectRecord;
//    OnCellClick             := cxGridDBViewCellClick;
    OnFocusedRecordChanged  := cxGridDBView2FocusedRecordChanged;
    DataController.OnDataChanged := DBGridDataChanged;
  end;
end;

procedure TSBaseCxView2Frm.cxGridLevel2ViewChanged(Sender: TObject);
var
  LcxGridView: TcxCustomGridView;
begin
  LcxGridView := cxGridLevel2.GridView;
  if Assigned(LcxGridView) then
    FDataSourceControls.FindOrCreateGridControl(LcxGridView).AfterUpdate := Grid2AfterUpdateActions;
end;

procedure TSBaseCxView2Frm.SetRecordsPrivileges2(const Value: Word);
var
  LDataSourceControlProperties: TDataSourceControlProperties;
  LLinkedComponent: TComponent;
begin
  FRecordsPrivileges2 := Value;
  FRecordsPrivileges2Stored  := True;

  if not Loading then begin
    LLinkedComponent := cxGridLevel2.GridView;
    if FRecordsPrivileges2 and Privilege_Select_Bit = 0 then begin
      ItemsReactions.SetActive(mnView2, False);
      mnView2.Enabled := False;
    end else begin
      with ItemsReactions.FindOrCreateReaction(mnView2) do begin
        LinkedComponent := LLinkedComponent;
        Default := (SelectMode = smNone);
        OnGetEnabled.Active := True;
        OnClick.Reaction  := biraCustomDataSourceRecord;
      end;
    end;

    if FRecordsPrivileges2 and Privilege_Insert_Bit = 0 then begin
      ItemsReactions.SetActive(mnNew2, False);
      mnNew2.Enabled := False;
    end else
      with ItemsReactions.FindOrCreateReaction(mnNew2, biraDataSourceInPlaceInsertChild) do begin
        LinkedComponent := cxGridLevel.GridView;
        OnGetEnabled.Active := True;
        if OnClick.Reaction = biraNone then OnClick.Reaction := biraCustomDataSourceRecord;
        OnClick.Target := cxGridLevel2.GridView;
      end;

    if FRecordsPrivileges2 and Privilege_Update_Bit = 0 then begin
      ItemsReactions.SetActive(mnEdit2, False);
      mnEdit2.Enabled := False;
    end else
      with ItemsReactions.FindOrCreateReaction(mnEdit2, biraDataSourceInPlaceEdit) do begin
        LinkedComponent := LLinkedComponent;
        OnGetEnabled.Active := True;
        if OnClick.Reaction = biraNone then OnClick.Reaction := biraCustomDataSourceRecord;
      end;

    if FRecordsPrivileges2 and Privilege_Delete_Bit = 0 then begin
      ItemsReactions.SetActive(mnDelete2, False);
      mnDelete2.Enabled := False;
    end else begin
      with ItemsReactions.FindOrCreateReaction(mnDelete2, biraDataSourceInPlaceDelete) do begin
        LinkedComponent := LLinkedComponent;
        OnGetEnabled.Active := True;
        if OnClick.Confirmation.IsEmpty then
          OnClick.Confirmation := SDeleteRecordQuestion;
        if OnClick.Reaction = biraNone then OnClick.Reaction := biraCustomDataSourceRecord;
      end;
    end;

    ItemsReactions.DataSourceOnUpdate(InternalGetGrid, [dsusFocusedRecord]);
    ItemsReactions.DataSourceOnUpdate(cxGridLevel2.GridView, [dsusActive, dsusData, dsusFocusedRecord]);

    LDataSourceControlProperties := FDataSourceControls.FindGridControl(cxGridLevel2.GridView);
    if Assigned(LDataSourceControlProperties) and Assigned(LDataSourceControlProperties.AfterUpdate) then
      LDataSourceControlProperties.AfterUpdate(cxGridLevel2.GridView);
  end;
end;

initialization
  RegisterClass(TSBaseCxView2Frm);

finalization
  UnRegisterClass(TSBaseCxView2Frm);

end.
