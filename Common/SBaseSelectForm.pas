unit SBaseSelectForm;
{$i config.inc}

interface

uses
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  SBaseFormDefinitions, SBaseForm, SBaseItemReactions, cxMDGrid,

{$IFDEF FASTSCRIPT_RTTI}
  fs_iinterpreter, fs_ievents, fs_iextctrlsrtti,
{$ENDIF}

  SBaseXXViewForm, NamedVariables, cxGraphics, cxControls,
  cxContainer, cxEdit, DB, SBaseDataModule, ExtCtrls, dxBar, cxClasses, cxSplitter,
  cxGroupBox, dxStatusBar, cxStyles, cxCustomData, cxFilter, cxData,
  cxDataStorage, cxDBData, cxTL, cxTLdxBarBuiltInMenu, cxInplaceContainer,
  cxTLData, cxDBTL, cxGridLevel, cxGridBandedTableView, cxGridDBBandedTableView,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView,
  cxGrid, cxGridDBDataDefinitions, cxLookAndFeels, cxLookAndFeelPainters,
  cxNavigator, System.Actions, ActnList, LayoutPanel, dxMDBarButton, dxMDBar,
  dxMDStatusBar, cxMDDBTreeList, dxSkinsCore, dxSkinsDefaultPainters;

const
  WM_NEXTLEVEL  = WM_USER + 1;
  WM_PRIORLEVEL = WM_USER + 2;

type
  TSBaseSelectedLevel = packed record
    Params  : TNamedVariants;
    Values  : TNamedVariants;

    GridBookmark: TDataSourceBookmark;
  end;
  PSBaseSelectedLevel = ^TSBaseSelectedLevel;

  TSBaseSelectFrm = class(TSBaseXXViewFrm)
    cxGrid: TcxMDGrid;
    cxGridLevel: TcxMDGridLevel;

    mnNext: TdxBarButton;
    mnPriorLevel: TdxBarButton;
    mnNextLevel: TdxBarButton;
    LevelChanged: TdxBarButton;

    procedure cxGridFocusedViewChanged(Sender: TcxCustomGrid; APrevFocusedView, AFocusedView: TcxCustomGridView); dynamic;
    procedure cxGridDBViewFocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
    procedure cxGridDBViewDataControllerGroupingChanged(Sender: TObject);
    procedure cxGridDBViewCanSelectRecord(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; var AAllow: Boolean);
//    procedure cxGridDBViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);

    procedure cxDBTreeListFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    procedure cxDBTreeListCollapsing(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);

    procedure mnPriorLevelClick(Sender: TObject);
    procedure WMPriorLevel(var Msg: TMessage); message WM_PRIORLEVEL;
    procedure mnNextClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
    procedure WMNextLevel(var Msg: TMessage); message WM_NEXTLEVEL;
    function  mnNextOnGetEnabled(Sender: TObject): Boolean;

    procedure cxDBTreeListNodeAssignValues(Sender: TcxCustomDBTreeList; ANode: TcxTreeListNode);

    procedure DataSetProviderOnBeginUpdate(ADataSet: TCustomSQLDataSetProvider; var ADataSourceStateBookmark: TDataSourceStateBookmark); override;
    procedure DataSetProviderOnEndUpdate(ADataSet: TCustomSQLDataSetProvider; const ADataSourceStateBookmark: TDataSourceStateBookmark); override;
  private
    { Private declarations }
    FActiveGrid: TComponent;

    procedure SetActiveGrid(const Value: TComponent);
    function GetLevelParams(Index: Integer; Name: String): Variant;
    procedure SetLevelParams(Index: Integer; Name: String; const Value: Variant);
    procedure LevelSuccess;
  protected
    { Protected declarations }
    FLevels: Array of TSBaseSelectedLevel;

    class function AlwaysSelect: Boolean; override;
    function  GetLevel: Integer;
    procedure SetLevel(Value: Integer);
    procedure InternalLevelChanged; dynamic;

    procedure InternalCreate; override;
    procedure InternalInit; override;

    procedure InternalSelect(ACloseForm: Boolean); override;

    procedure InternalSaveGridPosition(var ABookmark: TDataSourceBookmark); override;
    procedure InternalRestoreGridPosition(ABookmark: TDataSourceBookmark); override;
    procedure InternalReOpen; override;

    procedure InternalBeginUpdate; override;
    procedure InternalEndUpdate; override;

    function  InternalGetControlsEnabled: Boolean; override;

    procedure InternalClearGridColumns; dynamic;
    procedure InternalSetGridFocus; override;
    procedure InternalSetGridFirstRow; override;
    function  InternalLocateByKey(AKeyValue: Variant): Boolean; override;

    function  InternalGetGrid: TComponent; override;
    function  InternalGetGridKeys: String; override;
    function  InternalGetGridKeyValues: Variant; override;
    function  InternalGetGridFieldValues(const ARecordIndex: Integer; const AFieldNames: String; var AUnknownFields: String): Variant; override;
    function  InternalGetGridFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant; override;
    function  InternalGetGridRecordCount: Integer; override;

    function  InternalGetSelectedRowCount: Integer; override;
//    procedure InternalSetSelectedRow(ARecNo: Integer); override;

    procedure InternalGetParameters(ALevel: UInt16; var AParameters: TNamedVariants; var ACollisions: Boolean); override;
    procedure InternalGetResults(var AParameters: TNamedVariants; var ACollisions: Boolean); override;

    procedure InternalExcelExport; override;
  public
    { Public declarations }
//    Expand   : Byte;
//    Expanded : Byte;

    destructor Destroy; override;
//    function  GetFormParamValues(const AFieldNames: String; var AUnknownFields: String): Variant; override;

    property LevelParams[Index: Integer; Name: String]: Variant read GetLevelParams write SetLevelParams;
  published
    cxDBTreeList: TcxMDDBTreeList;

    property ActiveGrid: TComponent read FActiveGrid write SetActiveGrid;
    property Level: Integer read GetLevel write SetLevel;
  end;

{$IFDEF FASTSCRIPT_RTTI}
  TFunctions_TSBaseSelectFrm = class(TfsRTTIModule)
  private
    function Call_TSBaseSelectFrm_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

implementation

uses
  SBaseConstants, SBaseVariantFunctions, SBaseUtils, SBaseCxUtils;

{$R *.dfm}

procedure TSBaseSelectFrm.InternalCreate;
begin
  mnPriorLevel.Caption := SBaseProperties.Text_MenuBackward;
  mnNext.Caption := SBaseProperties.Text_MenuForward;

  inherited;

//    mnPriorLevel: TdxBarButton;
//    mnNext: TdxBarButton;
//    mnNextLevel: TdxBarButton;
//    LevelChanged: TdxBarButton;

  if mnPriorLevel.ImageIndex = -1 then begin
    mnPriorLevel.ImageIndex := SBaseFormProperties.IconIndex_Backward;
    mnNext.ImageIndex := SBaseFormProperties.IconIndex_Forward;
  end;

//  with ItemsReactions.FindOrCreateReaction(mnSelect) do begin
//    LinkedComponent := InternalGetGrid;
//    Default := False;
//    with OnClick do begin
//      Reaction        := biraCustomDataSourceRecord;
//      SystemProcedure := mnSelectClick;
//    end;
//  end;

  with ItemsReactions.FindOrCreateReaction(mnNext) do begin
    LinkedComponent := InternalGetGrid;
    Default := True;
    with OnGetEnabled do begin
      Priority := 1;
      SystemFunction := mnNextOnGetEnabled;
    end;
    with OnClick do begin
      Reaction        := biraCustomDataSourceRecord;
      SystemProcedure := mnNextClick;
    end;
  end;

  with ItemsReactions.FindOrCreateReaction(mnNextLevel) do begin
    LinkedComponent := InternalGetGrid;
    OnClick.Reaction := biraCustomDataSourceRecord;
  end;
  ItemsReactions.InitReaction(LevelChanged, nil, biraCustomForm);

  if (mnTake.Visible <> ivNever) then
    mnSelect.ShortCut := ShortCut_Ctrl_Del;
end;

procedure TSBaseSelectFrm.InternalInit;
begin
  Level := 0;
  InternalLevelChanged;
  inherited;
end;

procedure TSBaseSelectFrm.DataSetProviderOnBeginUpdate(ADataSet: TCustomSQLDataSetProvider; var ADataSourceStateBookmark: TDataSourceStateBookmark);
begin
//
end;

procedure TSBaseSelectFrm.DataSetProviderOnEndUpdate(ADataSet: TCustomSQLDataSetProvider; const ADataSourceStateBookmark: TDataSourceStateBookmark);
begin
//
end;

destructor TSBaseSelectFrm.Destroy;
begin
  if Assigned(cxDBTreeList) then
    with cxDBTreeList.Customizing do
      if Form <> nil then
        Form.Close;

  inherited;
end;

procedure TSBaseSelectFrm.InternalLevelChanged;
begin
  CallFunction('InternalLevelChanged', False);
  LevelChanged.Click;
  mnPriorLevel.Enabled := (Level > 0);
end;

function TSBaseSelectFrm.GetLevel: Integer;
begin
  Result := Length(FLevels) - 1;
end;

function TSBaseSelectFrm.GetLevelParams(Index: Integer; Name: String): Variant;
begin
  Result := FLevels[Index].Params[Name];
end;

procedure TSBaseSelectFrm.SetLevelParams(Index: Integer; Name: String; const Value: Variant);
begin
  FLevels[Index].Params[Name] := Value;
end;

procedure TSBaseSelectFrm.SetLevel(Value: Integer);
begin
  SetLength(FLevels, Value + 1);
end;

procedure TSBaseSelectFrm.WMPriorLevel(var Msg: TMessage);
var
  LGridBookmark: TDataSourceBookmark;
begin
  Level := Level - 1;
  DataSetProvider.DisableControls;
  try
    CloseDataSet;
    InternalClearGridColumns;
    InternalLevelChanged;
    AfterFilterChanged(True);
    with FLevels[Level] do begin
      Params.Clear;
      Values.Clear;
      LGridBookmark := GridBookmark;
    end;
  finally
    DataSetProvider.EnableControls;
  end;
  InternalRestoreGridPosition(LGridBookmark);
  InternalSetGridFocus;
end;

procedure TSBaseSelectFrm.mnPriorLevelClick(Sender: TObject);
begin
  PostMessage(Handle, WM_PRIORLEVEL, 0, 0)
end;

procedure TSBaseSelectFrm.InternalSelect(ACloseForm: Boolean);
begin
  LevelSuccess;
  Level := Level + 1;
  try
    inherited;
  finally
    Level := Level - 1;
  end;
end;

procedure TSBaseSelectFrm.WMNextLevel(var Msg: TMessage);
begin
  if not mnNextLevel.Enabled then
    mnSelect.Click
  else begin
    LevelSuccess;
    with FLevels[Level] do begin
      InternalSaveGridPosition(GridBookmark);

      Values.Clear;
      Values.UnknownAsNull := False;
      Values.AssignValues(DataSetProvider.Fields);
    end;
    Level := Level + 1;

    CloseDataSet;
    InternalClearGridColumns;
    InternalLevelChanged;
    AfterFilterChanged(True);
//    OpenDataSet;
  end;
end;

procedure TSBaseSelectFrm.mnNextClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  PostMessage(Handle, WM_NEXTLEVEL, 0, 0)
end;

function TSBaseSelectFrm.mnNextOnGetEnabled(Sender: TObject): Boolean;
begin
  Result := mnNextLevel.Enabled or mnSelect.Enabled;
end;

procedure TSBaseSelectFrm.SetActiveGrid(const Value: TComponent);
var
  I: Integer;
begin
  if FActiveGrid <> Value then begin
{$IFDEF DEBUG}
    Assert(Assigned(Value), 'Value = nil');
{$ENDIF}

    if Value = cxDBTreeList then begin
      cxGrid.Visible := False;
      if Assigned(cxGrid.ActiveView) then begin
        TcxGridDBDataController(cxGrid.ActiveView.DataController).DataSource := nil;
        while cxGrid.ActiveView.IsUpdateLocked do cxGrid.ActiveView.EndUpdate;
      end;

      if (SelectMode in [smMultiple, smMultipleRepeateAble]) then begin
        cxDBTreeList.OptionsView.CheckGroups := True;
        cxDBTreeList.OnNodeAssignValues := cxDBTreeListNodeAssignValues;
      end;

      cxDBTreeList.DataController.DataSource := DataSource;
      cxDBTreeList.Visible := True;
      if Visible then
        cxDBTreeList.SetFocus;
    end else if (Value is TcxGridTableView) then begin
      if Assigned(cxDBTreeList) then begin
        cxDBTreeList.Visible := False;
        cxDBTreeList.DataController.DataSource := nil;
        while cxDBTreeList.LockUpdate > 0 do cxDBTreeList.EndUpdate;
      end;

      if cxGridLevel.GridView <> Value then begin
        TcxGridDBDataController(cxGrid.ActiveView.DataController).DataSource := nil;
        cxGridLevel.GridView := TcxCustomGridView(Value);
        if SelectMode in [smMultiple, smMultipleRepeateAble] then begin
          TcxGridTableView(cxGrid.ActiveView).OptionsSelection.MultiSelect := True;
        end;
      end;
      TcxGridDBDataController(cxGrid.ActiveView.DataController).DataSource := DataSource;
      cxGrid.Visible := True;
      if Visible then
        cxGrid.SetFocus
    end else
      Assert(False);

    FActiveGrid := Value;
    for I := Pred(ItemsReactions.Count) downto 0 do
      with ItemsReactions.Items[I] do
        if OnClick.IsDataSourceReaction and OnGetEnabled.Active then
          LinkedComponent := FActiveGrid;

    FDataSourceControls.FindOrCreateGridControl(FActiveGrid).AfterUpdate := GridAfterUpdateActions;
  end;
end;

procedure TSBaseSelectFrm.cxGridFocusedViewChanged(Sender: TcxCustomGrid; APrevFocusedView, AFocusedView: TcxCustomGridView);
begin
  if not Loading then
    DBGridAfterFocusedRecordChanged(True);
end;

procedure TSBaseSelectFrm.cxGridDBViewFocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
begin
  DBGridAfterFocusedRecordChanged(True);
end;

procedure TSBaseSelectFrm.cxGridDBViewCanSelectRecord(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; var AAllow: Boolean);
begin
  AAllow := Assigned(ARecord) and ARecord.IsData;
end;

procedure TSBaseSelectFrm.cxGridDBViewDataControllerGroupingChanged(Sender: TObject);
begin
  mnGroupingClear.Enabled := TcxGridTableView(cxGrid.ActiveView).GroupedItemCount > 0;
end;

procedure TSBaseSelectFrm.cxDBTreeListFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
begin
  DBGridAfterFocusedRecordChanged(True);
end;

procedure TSBaseSelectFrm.cxDBTreeListNodeAssignValues(Sender: TcxCustomDBTreeList; ANode: TcxTreeListNode);
begin
  if ANode.CheckGroupType = ncgNone then begin
    ANode.CheckGroupType := ncgCheckGroup;
    ANode.CheckState := cbsUnchecked;
  end;
end;

class function TSBaseSelectFrm.AlwaysSelect: Boolean;
begin
  Result := True;
end;

procedure TSBaseSelectFrm.cxDBTreeListCollapsing(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
begin
  Allow := True; //(ANode = nil) or (Expanded = 0) or (ANode.Level > Expanded);
end;

procedure TSBaseSelectFrm.InternalBeginUpdate;
begin
{$IFDEF DEBUG}
    Assert(Assigned(FActiveGrid), 'FActiveGrid = nil');
{$ENDIF}

  if FActiveGrid = cxDBTreeList then
    cxDBTreeList.BeginUpdate
  else begin
{$IFDEF DEBUG}
    Assert(Assigned(cxGrid.ActiveView), 'cxGrid.ActiveView = nil');
{$ENDIF}
    cxGrid.ActiveView.BeginUpdate
  end;
end;

procedure TSBaseSelectFrm.InternalEndUpdate;
begin
{$IFDEF DEBUG}
    Assert(Assigned(FActiveGrid), 'FActiveGrid = nil');
{$ENDIF}

  if FActiveGrid = cxDBTreeList then
    cxDBTreeList.EndUpdate
  else begin
{$IFDEF DEBUG}
    Assert(Assigned(cxGrid.ActiveView), 'cxGrid.ActiveView = nil');
{$ENDIF}
    cxGrid.ActiveView.EndUpdate;
  end;
end;

procedure TSBaseSelectFrm.InternalClearGridColumns;
begin
  Assert(Assigned(FActiveGrid));
  if FActiveGrid = cxDBTreeList then
    cxDBTreeList.DeleteAllColumns
  else
    TcxGridTableView(cxGrid.ActiveView).ClearItems
end;

{ ќверрайды материнской формы }

procedure TSBaseSelectFrm.InternalExcelExport;
begin
{$IFDEF DEBUG}
  Assert(Assigned(FActiveGrid));
{$ENDIF}
  if FActiveGrid = cxDBTreeList then
    ExportCxTreeToExcel(cxDBTreeList, Caption, ExportToExcelInNativeFormat)
  else
    ExportCxGridToExcel(cxGrid, Caption, Caption, ExportToExcelInNativeFormat)
end;

function TSBaseSelectFrm.InternalGetGridFieldValues(const ARecordIndex: Integer; const AFieldNames: String; var AUnknownFields: String): Variant;
begin
{$IFDEF DEBUG}
  Assert(Assigned(FActiveGrid));
{$ENDIF}
  if FActiveGrid = cxDBTreeList then
    Result := cxDBTreeList.GetRecordValues(ARecordIndex, AFieldNames, @AUnknownFields)
  else
    Result := TcxGridDBDataController(cxGrid.FocusedView.DataController).GetRecordValues(ARecordIndex, AFieldNames, @AUnknownFields)
end;

function TSBaseSelectFrm.InternalGetGridFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant;
var
  FGridView: TcxGridTableView;
  FFocusedRecord: TcxCustomGridRecord;
begin
{$IFDEF DEBUG}
  Assert(Assigned(FActiveGrid));
{$ENDIF}
  if FActiveGrid = cxDBTreeList then begin
    if (cxDBTreeList.FocusedNode <> nil) then
      if cxDBTreeList.OptionsData.SyncMode then
        Result := cxDBTreeList.DataController.DataSet.Fields.GetValues(AFieldNames, @AUnknownFields)
      else
        Result := InternalGetGridFieldValues(cxDBTreeList.FocusedNode.AbsoluteIndex, AFieldNames, AUnknownFields)
    else begin
      Result          := unAssigned;
      AUnknownFields  := AFieldNames;
    end;
  end else begin
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
end;

function TSBaseSelectFrm.InternalGetControlsEnabled: Boolean;
begin
  Result := (inherited InternalGetControlsEnabled);
  if Result then begin
    Assert(Assigned(FActiveGrid));
    if FActiveGrid = cxDBTreeList then
      Result := (cxDBTreeList.LockUpdate = 0)
    else
      Result := cxGrid.UpdateLocked;
  end;
end;

function TSBaseSelectFrm.InternalGetGrid: TComponent;
begin
  Result := FActiveGrid;
end;

//function TSBaseSelectFrm.GetFormParamValues(const AFieldNames: String; var AUnknownFields: String): Variant;
//var
//  I: Integer;
//  LVarParams: TNamedVariants;
//  LFieldNames: String;
//begin
//  LVarParams.UnknownAsNull := True;
//
//  LFieldNames := AFieldNames;
//  for I := Level - 1 downto 0 do begin
//    LVarParams[AFieldNames] := FLevels[I].Params.GetValues(AFieldNames, @AUnknownFields);
//    LFieldNames := AUnknownFields;
//    if LFieldNames.IsEmpty then Break;
//  end;
//
//  if (not LFieldNames.IsEmpty) then
//    LVarParams[AFieldNames] := (inherited GetFormParamValues(LFieldNames, AUnknownFields));
//  Result := LVarParams[AFieldNames];
//end;

procedure TSBaseSelectFrm.InternalGetParameters(ALevel: UInt16; var AParameters: TNamedVariants; var ACollisions: Boolean);
var
  I: Integer;
begin
  for I := Level - 1 downto 0 do
    AParameters.AssignValues(FLevels[I].Params, amAppend);

  inherited;
end;

procedure TSBaseSelectFrm.InternalGetResults(var AParameters: TNamedVariants; var ACollisions: Boolean);
var
  I: Integer;
begin
  for I := Level - 1 downto 0 do
    AParameters.AssignValues(FLevels[I].Params, amAppend);

  inherited;
end;

function TSBaseSelectFrm.InternalGetGridKeys: String;
begin
  if FActiveGrid = nil then
    Result := ''
  else if FActiveGrid = cxDBTreeList then
    Result := cxDBTreeList.DataController.KeyField
  else
    Result := TcxDBDataController(cxGrid.ActiveView.DataController).KeyFieldNames;
end;

function TSBaseSelectFrm.InternalGetGridKeyValues: Variant;
begin
  Assert(Assigned(FActiveGrid));
  if FActiveGrid = cxDBTreeList then
    with cxDBTreeList.DataController do
      if (FilteredRecordCount > 0) and (KeyField <> '') then
        Result := GetRecordId(FocusedRecordIndex)
      else
        Result := unAssigned
  else
    with TcxGridDBDataController(TcxCustomGridTableView(cxGrid.ActiveView).DataController) do
      if (FilteredRecordCount > 0) and (KeyFieldNames <> '') then
        Result := GetRecordId(FocusedRecordIndex)
      else
        Result := unAssigned
end;

function TSBaseSelectFrm.InternalGetGridRecordCount: Integer;
begin
  Assert(Assigned(FActiveGrid));
  if FActiveGrid = cxDBTreeList then
    Result := cxDBTreeList.DataController.NodesCount
  else
    with TcxGridDBDataController(cxGrid.ActiveView.DataController) do
      if DataModeController.GridMode then
        if Assigned(DataSet) and DataSet.Active then
          Result := DataSet.RecordCount
        else
          Result := 0
      else
        Result := FilteredRecordCount
end;

function TSBaseSelectFrm.InternalGetSelectedRowCount: Integer;
begin
  Assert(Assigned(FActiveGrid));
  if FActiveGrid = cxDBTreeList then
    Result := cxDBTreeList.SelectionCount
  else
    Result := TcxGridTableView(cxGrid.ActiveView).Controller.SelectedRecordCount;
end;

procedure TSBaseSelectFrm.InternalReOpen;
begin
  Assert(Assigned(FActiveGrid));
  if FActiveGrid = cxDBTreeList then
    cxDBTreeList.RefreshDataSet
  else
    TcxGridTableView(cxGrid.ActiveView).RefreshDataSet;
end;

procedure TSBaseSelectFrm.InternalSaveGridPosition(var ABookmark: TDataSourceBookmark);
begin
  Assert(Assigned(FActiveGrid));
  if FActiveGrid = cxDBTreeList then
    ABookmark := cxDBTreeList.GridBookmark
  else
    ABookmark := TcxGridTableView(cxGrid.ActiveView).GridBookmark
end;

procedure TSBaseSelectFrm.InternalRestoreGridPosition(ABookmark: TDataSourceBookmark);
begin
  Assert(Assigned(FActiveGrid));
  if FActiveGrid = cxDBTreeList then
    cxDBTreeList.GridBookmark := ABookmark
  else
    TcxGridTableView(cxGrid.ActiveView).GridBookmark := ABookmark;
end;

procedure TSBaseSelectFrm.InternalSetGridFirstRow;
begin
{$IFDEF DEBUG}
  Assert(Assigned(FActiveGrid), 'ActiveGrid is empty');
{$ENDIF}
  if FActiveGrid = cxDBTreeList then begin
    if (cxDBTreeList.TopNode <> nil) and (not DataSetProvider.ControlsDisabled) then begin
      cxDBTreeList.TopNode.Focused := True;
  //    cxDBTreeList.FullCollapse;
     end;
  end else begin
    if (cxGrid.ActiveView.DataController.FilteredRecordCount > 0) then
      TcxGridTableView(cxGrid.ActiveView).ViewData.Records[0].Focused := True;
  end;
end;

function TSBaseSelectFrm.InternalLocateByKey(AKeyValue: Variant): Boolean;
begin
  Assert(Assigned(FActiveGrid));
  if FActiveGrid = cxDBTreeList then
    Result := cxDBTreeList.DataController.LocateByKey(AKeyValue)
  else
    Result := TcxGridDBDataController(TcxGridTableView(cxGrid.ActiveView).DataController).LocateByKey(AKeyValue)
end;

procedure TSBaseSelectFrm.InternalSetGridFocus;
begin
  Assert(Assigned(FActiveGrid));
  if FActiveGrid = cxDBTreeList then
    cxDBTreeList.SetFocus
  else
    cxGrid.SetFocus
end;

procedure TSBaseSelectFrm.LevelSuccess;
begin
  if Assigned(mnNextLevel.OnClick) then
    with FLevels[Level] do begin
      Params.Clear;
      Params.UnknownAsNull := True;
      FilterUnderConstruction := True;
      try
        mnNextLevel.OnClick(mnNextLevel);
      finally
        FilterUnderConstruction := False;
      end;
    end;
end;

{$IFDEF FASTSCRIPT_RTTI}
{ TFunctions_TSBaseXXViewFrm }

constructor TFunctions_TSBaseSelectFrm.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseSelectFrm, TSBaseSelectFrm.ClassParent.ClassName) do begin
    AddIndexProperty('LevelParams', 'Integer,String', DelphiTypeVariant, Call_TSBaseSelectFrm_Method);
  end;
end;

function TFunctions_TSBaseSelectFrm.Call_TSBaseSelectFrm_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'LEVELPARAMS.GET' then
    Result := TSBaseSelectFrm(Instance).LevelParams[Caller.Params[0], VarToStr(Caller.Params[1])]
  else if MethodName = 'LEVELPARAMS.SET' then
    TSBaseSelectFrm(Instance).LevelParams[Caller.Params[0], VarToStr(Caller.Params[1])] := Caller.Params[2];
end;
{$ENDIF}

initialization
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TFunctions_TSBaseSelectFrm);
{$ENDIF}

  RegisterClass(TSBaseSelectFrm);

finalization
  UnRegisterClass(TSBaseSelectFrm);

{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TFunctions_TSBaseSelectFrm);
{$ENDIF}

end.
