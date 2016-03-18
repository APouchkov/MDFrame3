unit SBaseCxViewForm;
{$I config.inc}

interface

uses
  SBaseForm, SBaseXXViewForm, NamedVariables,

  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxGraphics, ExtCtrls, DB, dxBar, cxControls, dxStatusBar, cxStyles, cxCustomData,
  cxFilter, cxData, cxDataStorage,
  cxEdit, cxDBData, cxGridLevel, cxGridBandedTableView, cxGridDBBandedTableView,
  cxClasses,
  cxGrid, cxMDGrid,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView,
  cxGridDBDataDefinitions, cxCalendar, cxBarEditItem,
  SBaseDataModule, cxContainer, cxSplitter, cxGroupBox, cxLookAndFeels,
  cxLookAndFeelPainters, cxNavigator, dxBevel, System.Actions, ActnList,
  LayoutPanel, dxMDBarButton, dxMDBar, dxMDStatusBar, dxSkinsCore,
  dxSkinsDefaultPainters;

type
  TSBaseCxViewFrm = class(TSBaseXXViewFrm)
    cxGrid: TcxMDGrid;

    cxGridLevel: TcxMDGridLevel;
    procedure cxGridLevelOnAddView(AView: TcxCustomGridView);

    procedure cxGridFocusedViewChanged(Sender: TcxCustomGrid; APrevFocusedView, AFocusedView: TcxCustomGridView); dynamic;
    procedure cxGridDBViewFocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
    procedure cxGridDBViewDataControllerGroupingChanged(Sender: TObject);
    procedure cxGridDBViewCanSelectRecord(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; var AAllow: Boolean);
//    procedure cxGridDBViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);

    procedure mnFitToWidthClick(Sender: TObject);
    procedure mnFitToHeightClick(Sender: TObject); dynamic;
    procedure mnGroupingClearClick(Sender: TObject);
    procedure mnGroupBoxClick(Sender: TObject);

    procedure FormShow(Sender: TObject); override;
    procedure mnExpandAllClick(Sender: TObject); dynamic;
    procedure mnCollapseAllClick(Sender: TObject);
    procedure cxGridDBViewCellClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
    procedure cxGridActiveTabChanged(Sender: TcxCustomGrid; ALevel: TcxGridLevel);

    procedure DataSetProviderOnBeginUpdate(ADataSet: TCustomSQLDataSetProvider; var ADataSourceStateBookmark: TDataSourceStateBookmark); override;
    procedure DataSetProviderOnEndUpdate(ADataSet: TCustomSQLDataSetProvider; const ADataSourceStateBookmark: TDataSourceStateBookmark); override;
    procedure cxGridLevelViewChanged(Sender: TObject);
  private
    { Private declarations }
    FcxGridColumnChecked: TcxGridColumn;
  protected
    { Protected declarations }
    procedure InternalCreate; override;

    procedure LoadUserConfig(AStoreFormSettings: TFormItemsForSave); override;

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
    function  InternalGetGridKeyValues: Variant; override;
    function  InternalFindRecord(AKeyFieldValues: Variant): Integer; override;
    function  InternalGetGridRecordChecked(ARecordIndex: Integer): Boolean; override;
    procedure InternalSetGridRecordChecked(ARecordIndex: Integer); override;
    function  InternalGetGridFieldValues(const ARecordIndex: Integer; const AFieldNames: String; var AUnknownFields: String): Variant; override;
    function  InternalGetGridFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant; override;
    function  InternalGetGridRecordCount: Integer; override;

    function  InternalGetSelectedRowCount: Integer; override;
//    procedure InternalSetSelectedRow(ARecNo: Integer); override;

    procedure InternalExcelExport; override;
  public
    { Public declarations }
  end;

implementation

uses SBaseCxUtils, SBaseConstants, SBaseFormDefinitions;

{$R *.dfm}

procedure TSBaseCxViewFrm.InternalBeginUpdate;
begin
  cxGrid.BeginUpdate;
end;

procedure TSBaseCxViewFrm.InternalEndUpdate;
begin
  cxGrid.EndUpdate;
end;

procedure TSBaseCxViewFrm.InternalCreate;
var
  LcxGridView: TcxCustomGridView;
begin
  inherited;

  if SelectMode in [smMultiple, smMultipleRepeateAble] then begin
    FcxGridColumnChecked := TcxGridTableView(InternalGetGrid).CreateColumn;
    with FcxGridColumnChecked do begin
      Width := 20;
      DataBinding.ValueType := DelphiTypeBoolean;
      Options.Filtering   := False;
      Options.IncSearch   := False;
      Options.AutoWidthSizable := False;
      Options.GroupFooters:= False;
      Options.Grouping    := False;
      Options.HorzSizing  := False;
      Options.Moving      := False;
      Options.ShowCaption := False;
      Options.Sorting     := False;
      RepositoryItem := SBaseFormProperties.EditRepository_CheckBox;
    end;
  end;

//  TcxGridDBDataController(cxGridLevelDetails.GridView.DataController).DataSource := DetailSource;

  LcxGridView := cxGridLevel.GridView;
  if Assigned(LcxGridView) then begin
    TcxGridDBDataController(LcxGridView.DataController).DataSource := DataSource;
    cxGridLevelViewChanged(cxGridLevel);
  end;
end;

procedure TSBaseCxViewFrm.InternalSaveGridPosition(var ABookmark: TDataSourceBookmark);
begin
  ABookmark := TcxCustomGridTableView(cxGrid.ActiveView).GridBookmark;
end;

procedure TSBaseCxViewFrm.InternalRestoreGridPosition(ABookmark: TDataSourceBookmark);
begin
  TcxCustomGridTableView(cxGrid.ActiveView).GridBookmark := ABookmark;
end;

procedure TSBaseCxViewFrm.InternalReOpen;
begin
  try
    TcxGridTableView(cxGrid.ActiveView).RefreshDataSet;
  finally
    DBGridAfterFocusedRecordChanged(True);
  end;
end;

function TSBaseCxViewFrm.InternalGetControlsEnabled: Boolean;
begin
  Result := (inherited InternalGetControlsEnabled) and ((not Assigned(cxGrid) or (not cxGrid.UpdateLocked)));
end;

procedure TSBaseCxViewFrm.InternalSetGridFocus;
begin
  cxGrid.SetFocus
end;

procedure TSBaseCxViewFrm.InternalSetGridFirstRow;
begin
  if (cxGrid.ActiveView.DataController.FilteredRecordCount > 0) then
    TcxGridTableView(cxGrid.ActiveView).ViewData.Records[0].Focused := True;
end;

function TSBaseCxViewFrm.InternalLocateByKey(AKeyValue: Variant): Boolean;
begin
  Result := TcxDBDataController(TcxGridTableView(cxGrid.ActiveView).DataController).LocateByKey(AKeyValue)
end;

function TSBaseCxViewFrm.InternalGetGrid: TComponent;
begin
  Result := cxGridLevel.GridView
end;

function TSBaseCxViewFrm.InternalGetGridKeys: String;
begin
  Result := TcxGridDBDataController(TcxCustomGridTableView(cxGrid.ActiveView).DataController).KeyFieldNames;
end;

function TSBaseCxViewFrm.InternalFindRecord(AKeyFieldValues: Variant): Integer;
begin
  Result := TcxGridDBDataController(TcxCustomGridTableView(cxGrid.ActiveView).DataController).FindRecordIndexByKey(AKeyFieldValues)
end;

function TSBaseCxViewFrm.InternalGetGridKeyValues: Variant;
begin
  with TcxGridDBDataController(TcxCustomGridTableView(cxGrid.ActiveView).DataController) do
    if (FilteredRecordCount > 0) and (KeyFieldNames <> '') then
      Result := GetRecordId(FocusedRecordIndex)
    else
      Result := unAssigned
end;

function TSBaseCxViewFrm.InternalGetGridFieldValues(const ARecordIndex: Integer; const AFieldNames: String; var AUnknownFields: String): Variant;
begin
  Result := TcxGridDBDataController(cxGrid.FocusedView.DataController).GetRecordValues(ARecordIndex, AFieldNames, @AUnknownFields);
end;

function TSBaseCxViewFrm.InternalGetGridFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant;
var
  FGridView: TcxGridTableView;
  FFocusedRecord: TcxCustomGridRecord;
begin
  FGridView := TcxGridTableView(cxGrid.FocusedView);
  if FGridView <> nil then
    FFocusedRecord := FGridView.Controller.FocusedRecord
  else
    FFocusedRecord := nil;

  if (FFocusedRecord <> nil) and (FFocusedRecord is TcxGridDataRow) then
    if TcxGridDBDataController(FGridView.DataController).DataModeController.SyncMode then
      Result := TcxGridDBDataController(FGridView.DataController).DataSet.Fields.GetValues(AFieldNames, @AUnknownFields)
    else
      Result := InternalGetGridFieldValues(FFocusedRecord.RecordIndex, AFieldNames, AUnknownFields)
  else begin
    Result          := unAssigned;
    AUnknownFields  := AFieldNames;
  end;
end;

function TSBaseCxViewFrm.InternalGetGridRecordChecked(ARecordIndex: Integer): Boolean;
begin
  Result := TcxGridTableView(InternalGetGrid).DataController.Values[ARecordIndex, FcxGridColumnChecked.Index] = True
end;

procedure TSBaseCxViewFrm.InternalSetGridRecordChecked(ARecordIndex: Integer);
begin
  TcxGridTableView(InternalGetGrid).DataController.Values[ARecordIndex, FcxGridColumnChecked.Index] := True
end;

function TSBaseCxViewFrm.InternalGetGridRecordCount: Integer;
begin
  with TcxGridDBDataController(cxGrid.ActiveView.DataController) do
    if DataModeController.GridMode then
      if Assigned(DataSet) and DataSet.Active then
        Result := DataSet.RecordCount
      else
        Result := 0
    else
      Result := FilteredRecordCount
end;

function TSBaseCxViewFrm.InternalGetSelectedRowCount: Integer;
begin
  Result := TcxGridTableView(cxGrid.ActiveView).Controller.SelectedRecordCount;
end;

//procedure TSBaseCxViewFrm.InternalSetSelectedRow(ARecNo: Integer);
//begin
//  if DataSetProvider.ControlsDisabled then begin
//    DataSetProvider.EnableControls;
//    try
//      TcxGridTableView(cxGrid.ActiveView).Controller.FocusedRow := TcxGridTableView(cxGrid.ActiveView).Controller.SelectedRows[ARecNo];
//    finally
//      DataSetProvider.DisableControls;
//    end;
//  end else
//    TcxGridTableView(cxGrid.ActiveView).Controller.FocusedRow := TcxGridTableView(cxGrid.ActiveView).Controller.SelectedRows[ARecNo];
//end;

procedure TSBaseCxViewFrm.InternalExcelExport;
begin
  ExportCxGridToExcel(cxGrid, Caption, Caption, ExportToExcelInNativeFormat)
end;

procedure TSBaseCxViewFrm.LoadUserConfig(AStoreFormSettings: TFormItemsForSave);
begin
  inherited;

  if (frsvFormItems in AStoreFormSettings) or (frsvUserDefined in AStoreFormSettings) then begin
    if (mnGroupBox.Visible <> ivNever) and Assigned(mnGroupBox.OnClick) then
      mnGroupBox.Down := TcxGridTableView(cxGrid.ActiveView).OptionsView.GroupByBox;
  end;

  if (frsvFormItems in AStoreFormSettings) and (FcxGridColumnChecked <> nil) then
    FcxGridColumnChecked.Index := 0;
end;

procedure TSBaseCxViewFrm.FormShow(Sender: TObject);
begin
  inherited;

  if (cxGridLevel.Count > 0) or (TcxGridTableView(cxGrid.ActiveView).GroupedItemCount > 0) then begin
    mnExpandAll.Enabled     := True;
    mnExpandAll.Visible     := ivAlways;
    mnCollapseAll.Enabled   := True;
    mnCollapseAll.Visible   := ivAlways;
  end;

  mnGroupingClear.Visible := mnGroupBox.Visible;
  if mnGroupingClear.Visible <> ivNever then begin
    TcxGridTableView(cxGrid.ActiveView).DataController.OnGroupingChanged := cxGridDBViewDataControllerGroupingChanged;
    cxGridDBViewDataControllerGroupingChanged(cxGrid.ActiveView);
  end;
end;

{ Собственные методы Гридов }

procedure TSBaseCxViewFrm.cxGridLevelOnAddView(AView: TcxCustomGridView);
begin
  AView.PopupMenu := PopupMenu;
  with TcxGridTableView(AView) do begin
//    Synchronization = False
    OnCanSelectRecord       := cxGridDBViewCanSelectRecord;
    OnCellClick             := cxGridDBViewCellClick;
    OnFocusedRecordChanged  := cxGridDBViewFocusedRecordChanged;

    DataController.OnDataChanged := DBGridDataChanged;
  end;
end;

procedure TSBaseCxViewFrm.cxGridLevelViewChanged(Sender: TObject);
var
  LcxGridView: TcxCustomGridView;
begin
  LcxGridView := cxGridLevel.GridView;
  if Assigned(LcxGridView) then
    FDataSourceControls.FindOrCreateGridControl(LcxGridView).AfterUpdate := GridAfterUpdateActions;
end;

procedure TSBaseCxViewFrm.cxGridFocusedViewChanged(Sender: TcxCustomGrid; APrevFocusedView, AFocusedView: TcxCustomGridView);
begin
  if not Loading then
    DBGridAfterFocusedRecordChanged(True);
end;

procedure TSBaseCxViewFrm.DataSetProviderOnBeginUpdate(ADataSet: TCustomSQLDataSetProvider; var ADataSourceStateBookmark: TDataSourceStateBookmark);
begin
  TcxCustomGridTableView(cxGrid.ActiveView).BeginUpdate(ADataSourceStateBookmark);
end;

procedure TSBaseCxViewFrm.DataSetProviderOnEndUpdate(ADataSet: TCustomSQLDataSetProvider; const ADataSourceStateBookmark: TDataSourceStateBookmark);
begin
  TcxCustomGridTableView(cxGrid.ActiveView).EndUpdate(ADataSourceStateBookmark, [sbrDisableCount], DataSourceBookmarkRestores_All);
end;

procedure TSBaseCxViewFrm.cxGridDBViewFocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
begin
  if APrevFocusedRecord <> AFocusedRecord then
    DBGridAfterFocusedRecordChanged(False);
end;

procedure TSBaseCxViewFrm.cxGridDBViewCellClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  if (SelectMode in [smMultiple, smMultipleRepeateAble]) and (ACellViewInfo.Item = FcxGridColumnChecked) then
    with ACellViewInfo do
      if (Value = True) then
        Sender.DataController.Values[GridRecord.RecordIndex, FcxGridColumnChecked.Index] := Null
      else
        Sender.DataController.Values[GridRecord.RecordIndex, FcxGridColumnChecked.Index] := True;
end;

procedure TSBaseCxViewFrm.cxGridActiveTabChanged(Sender: TcxCustomGrid; ALevel: TcxGridLevel);
begin
  ShowRecordCount
end;

procedure TSBaseCxViewFrm.cxGridDBViewCanSelectRecord(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; var AAllow: Boolean);
begin
  AAllow := Assigned(ARecord) and ARecord.IsData;
end;

procedure TSBaseCxViewFrm.cxGridDBViewDataControllerGroupingChanged(Sender: TObject);
begin
  mnGroupingClear.Enabled := TcxGridTableView(cxGrid.ActiveView).GroupedItemCount > 0;
  if (cxGridLevel.Count = 0) then begin
    mnExpandAll.Enabled     := mnGroupingClear.Enabled;
    mnCollapseAll.Enabled   := mnGroupingClear.Enabled;
  end;
end;

//procedure TSBaseCxViewFrm.cxGridDBViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
//begin
//  if (ARecord <> nil)
//        and (Sender = TcxGrid(Sender.GetParentComponent).FocusedView)
//        and (ARecord is TcxGridDataRow)
//        and TcxGridDataRow(ARecord).Focused
//        and (SBaseFormProperties.dxDefaultStyles.HighlightRecord <> nil)
//  then
//    AStyle := SBaseFormProperties.dxDefaultStyles.HighlightRecord;
//end;

{ Собственные методы кнопок }

procedure TSBaseCxViewFrm.mnGroupBoxClick(Sender: TObject);
begin
  TcxGridTableView(cxGrid.ActiveView).OptionsView.GroupByBox := mnGroupBox.Down;
end;

procedure TSBaseCxViewFrm.mnGroupingClearClick(Sender: TObject);
begin
  TcxGridTableView(cxGrid.ActiveView).Controller.ClearGrouping;
end;

procedure TSBaseCxViewFrm.mnFitToWidthClick(Sender: TObject);
begin
  TcxGridTableView(cxGrid.ActiveView).OptionsView.ColumnAutoWidth := mnFitToWidth.Down;
end;

procedure TSBaseCxViewFrm.mnExpandAllClick(Sender: TObject);
begin
  TcxGridTableView(cxGrid.ActiveView).ViewData.Expand(True);
end;

procedure TSBaseCxViewFrm.mnCollapseAllClick(Sender: TObject);
begin
  with TcxGridTableView(cxGrid.ActiveView) do begin
    Focused := True;
    ViewData.Collapse(True);
  end;
end;

procedure TSBaseCxViewFrm.mnFitToHeightClick(Sender: TObject);
begin
  TcxGridTableView(cxGrid.ActiveView).OptionsView.CellAutoHeight := mnFitToHeight.Down;
end;

initialization
  RegisterClass(TSBaseCxViewFrm);

finalization
  UnRegisterClass(TSBaseCxViewFrm);

end.
