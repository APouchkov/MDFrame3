unit cxMDGrid;

interface

uses
  Classes, SysUtils, Graphics, Vcl.Controls,
  cxGridCommon, cxGrid, cxGridLevel, cxFilter, cxCustomData, cxControls,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView,
  cxGridBandedTableView, cxGridDBBandedTableView, cxDBData,
  cxGridDBDataDefinitions, cxStorage;

const
  ScxMDGridViewDefaultConfigure: String = 'Indicator,QuickCustomization,Filtering=Values+Sorting';

type
  TcxMDGrid = class(TcxGrid)
  private
    FAutoHeight: Boolean;

    FOnCellDblClick   : TcxGridCellClickEvent;
    FOnKeyDown        : TKeyEvent;
    FOnCellEditKeyDown: TcxGridEditKeyEvent;
{$IFNDEF PACKAGE}
  protected
    procedure DoAutoHeight(AView: TcxCustomGridView);
    procedure DoChange(AGridChange: TcxCustomGridChange); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
    //property BorderStyle default cxcbsNone; // Чаще надо
    property ParentFont default False;
    property AutoHeight: Boolean read FAutoHeight write FAutoHeight default False;

    property OnCellDblClick: TcxGridCellClickEvent read FOnCellDblClick write FOnCellDblClick;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnCellEditKeyDown: TcxGridEditKeyEvent read FOnCellEditKeyDown write FOnCellEditKeyDown;
  end;

  TcxCustomGridViewNotifyEvent = procedure(AView: TcxCustomGridView) of object;

  TcxMDGridLevel = class(TcxGridLevel)
  private
    FOnAddView: TcxCustomGridViewNotifyEvent;
    FOnViewChanged: TNotifyEvent;

    procedure DoOnAddView(AView: TcxCustomGridView);
    procedure DoOnViewChanged;

    procedure SetOnAddView(const AValue: TcxCustomGridViewNotifyEvent);
    procedure SetOnViewChanged(const AValue: TNotifyEvent);
  protected
{$IFDEF DEBUG}
    procedure SetName(const NewName: TComponentName); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnAddView: TcxCustomGridViewNotifyEvent read FOnAddView write SetOnAddView;
    property OnViewChanged: TNotifyEvent read FOnViewChanged write SetOnViewChanged;
  end;

{
  // HideBorder
  -- Indicator
  -- QuickCustomization
  -- Filtering = Filtered + Values
  -- Sorting
  -- AutoHeight
  -- AutoWidth
  -- SyncMode
  -- DetailInSQLMode

  // Inserting
  // Editing
  // Deleting

  // Categorized
}

  TcxMDGridDBDataController = class;

  TcxMDDBDataProvider = class(TcxDBDataProvider)
  private
    function GetDataController: TcxMDGridDBDataController;
  protected
    procedure EditingRecord; override;

    property DataController: TcxMDGridDBDataController read GetDataController;
  end;

  TcxMDGridDBDataController = class(TcxGridDBDataController)
  private
    SkipChanges: TcxDataControllerChanges;
  protected
    function GetDataProviderClass: TcxCustomDataProviderClass; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Options default [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoCaseInsensitive, dcoFocusTopRowAfterSorting];
  end;

  TcxMDGridTableOptionsBehavior = class(TcxGridTableOptionsBehavior)
  public
    constructor Create(AGridView: TcxCustomGridView); override;
  published
    property CellHints                 default True;
    property ImmediateEditor           default False;
    property IncSearch                 default True;
    property ExpandMasterRowOnDblClick default False;
    property RecordScrollMode          default rsmByPixel;
  end;

  TcxMDGridBandedTableOptionsBehavior = class(TcxGridBandedTableOptionsBehavior)
  public
    constructor Create(AGridView: TcxCustomGridView); override;
  published
    property CellHints                 default True;
    property ImmediateEditor           default False;
    property IncSearch                 default True;
    property ExpandMasterRowOnDblClick default False;
    property RecordScrollMode          default rsmByPixel;
  end;

  TcxMDGridTableOptionsData = class(TcxGridTableOptionsData)
  public
    constructor Create(AGridView: TcxCustomGridView); override;
  published
    property CancelOnExit         default False;
    property Inserting            default False;
    property Editing              default False;
    property Deleting             default False;
    property DeletingConfirmation default False;
  end;

  TcxMDGridTableOptionsSelection = class(TcxGridTableOptionsSelection)
  public
    constructor Create(AGridView: TcxCustomGridView); override;
  published
    // property HideFocusRectOnExit default True;
    // property HideSelection default False;
    property InvertSelect default False;
  end;

  TcxMDGridTableOptionsView = class(TcxGridTableOptionsView)
  private
    FNotActiveInfoText: String;

    function IsNotActiveInfoTextStored: Boolean;
    procedure SetNotActiveInfoText(const AValue: String);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
  published
    property CellEndEllipsis               default True;
    property ExpandButtonsForEmptyDetails  default False;
    property GridLineColor                 default clSilver;
    property GroupByBox                    default False;
    property HeaderEndEllipsis             default True;

    property NotActiveInfoText: String read FNotActiveInfoText write SetNotActiveInfoText stored IsNotActiveInfoTextStored;
  end;

  TcxMDGridBandedTableOptionsView = class(TcxGridBandedTableOptionsView)
  private
    FNotActiveInfoText: String;
    function IsNotActiveInfoTextStored: Boolean;
    procedure SetNotActiveInfoText(const AValue: String);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
  published
    property CellEndEllipsis               default True;
    property ExpandButtonsForEmptyDetails  default False;
    property GridLineColor                 default clSilver;
    property GroupByBox                    default False;
    property HeaderEndEllipsis             default True;

    property BandCaptionsInColumnAlternateCaption default True;
    property BandHeaderEndEllipsis default True;

    property NotActiveInfoText: String read FNotActiveInfoText write SetNotActiveInfoText stored IsNotActiveInfoTextStored;
  end;

  TcxMDGridTableOptionsCustomize = class(TcxGridTableOptionsCustomize)
  public
    constructor Create(AGridView: TcxCustomGridView); override;
  published
    property ColumnGrouping default False;
    property ColumnFiltering default False;
    property ColumnSorting default False;
  end;

  TcxMDGridBandedTableOptionsCustomize = class(TcxGridBandedTableOptionsCustomize)
  public
    constructor Create(AGridView: TcxCustomGridView); override;
  published
    property ColumnFiltering default False;
    property ColumnSorting default False;
    property BandSizing default False;
  end;

  TcxMDGridTableViewInfo = class(TcxGridTableViewInfo)
  protected
    function GetNoDataInfoText: string; override;
  end;

  TcxMDGridDBTableView = class(TcxGridDBTableView)
  private
    FVersion: Byte;

{$IFNDEF PACKAGE}
    procedure SetConfigure(const AValue: String);
{$ENDIF}
    function GetDataController: TcxMDGridDBDataController;
    procedure SetDataController(const Value: TcxMDGridDBDataController);
  protected
    FOnFocusedRecordChangedEx: TNotifyEvent;
    FOnKeyDown: TKeyEvent;

    function GetDataControllerClass: TcxCustomDataControllerClass; override;

    function GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass; override;
    function GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass; override;
    function GetOptionsDataClass: TcxCustomGridOptionsDataClass; override;
    function GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass; override;
    function GetOptionsViewClass: TcxCustomGridOptionsViewClass; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;

{$IFNDEF PACKAGE}
    procedure SetLevel(AValue: TComponent); override;

    procedure DoFocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex, APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer; ANewItemRecordFocusingChanged: Boolean); override;
    function DoCellDblClick(ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState): Boolean; override;

    function GetOnKeyDown: TKeyEvent; override;
    procedure SetOnKeyDown(Value: TKeyEvent); override;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure RestoreFrom(const AStorageName: string; AStream: TStream; AReaderClass: TcxCustomReaderClass;
      AChildrenCreating, AChildrenDeleting: Boolean;
      AOptions: TcxGridStorageOptions; const ARestoreViewName: string; const AOwnerName: string = ''); override;

    procedure StoreTo(const AStorageName: string; AStream: TStream;
      AWriterClass: TcxCustomWriterClass; AReCreate: Boolean;
      AOptions: TcxGridStorageOptions; const ASaveViewName: string;
      const AOwnerName: string = ''); override;

    procedure DoAssign(ASource: TcxCustomGridView); override;
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetParentComponent(AValue: TComponent); override;

    property OnFocusedRecordChangedEx: TNotifyEvent read FOnFocusedRecordChangedEx write FOnFocusedRecordChangedEx;
  published
    property Version: Byte read FVersion write FVersion default 0;
    property DataController: TcxMDGridDBDataController read GetDataController write SetDataController;

{$IFNDEF PACKAGE}
    property Configure: String write SetConfigure;
{$ENDIF}
  end;

  TcxMDGridColumnOptions = class(TcxGridColumnOptions)
  public
    constructor Create(AItem: TcxCustomGridTableItem); override;
  published
    property AutoWidthSizable default False;
  end;

  TcxMDGridDBColumn = class(TcxGridDBColumn)
  private
{$IFNDEF PACKAGE}
    procedure SetIndexAfter(const Value: TcxMDGridDBColumn);
{$ENDIF}
  protected
    function DefaultHeaderAlignmentHorz: TAlignment; override;
  public
    function GetOptionsClass: TcxCustomGridTableItemOptionsClass; override;
  published
    property Index Stored False;
{$IFNDEF PACKAGE}
    property IndexAfter: TcxMDGridDBColumn write SetIndexAfter;
{$ENDIF}
  end;

  TcxMDGridBandedTableController = class(TcxGridBandedTableController)
  protected
    function GetItemsCustomizationPopupClass: TcxCustomGridItemsCustomizationPopupClass; override;
  end;

  TcxGridBandedColumnsCustomizationPopup = class(TcxGridColumnsCustomizationPopup)
  private
    type
      TcxGridBandedTableViewCrack = class(TcxGridBandedTableView);
      TcxGridBandedColumnCrack = class(TcxGridBandedColumn);
  private
    function GridView: TcxGridBandedTableViewCrack; inline;
  protected
    procedure AddCheckListBoxItems; override;
  end;

  TcxMDGridDBBandedTableView = class(TcxGridDBBandedTableView)
  private
    FVersion: Byte;
{$IFNDEF PACKAGE}
    procedure SetConfigure(const AValue: String);
{$ENDIF}
    function GetDataController: TcxMDGridDBDataController;
    procedure SetDataController(const Value: TcxMDGridDBDataController);
  protected
    FOnFocusedRecordChangedEx: TNotifyEvent;
    FOnKeyDown: TKeyEvent;

    function GetDataControllerClass: TcxCustomDataControllerClass; override;

    function GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass; override;
    function GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass; override;
    function GetOptionsDataClass: TcxCustomGridOptionsDataClass; override;
    function GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass; override;
    function GetOptionsViewClass: TcxCustomGridOptionsViewClass; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;

{$IFNDEF PACKAGE}
    procedure SetLevel(AValue: TComponent); override;

    procedure DoFocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex, APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer; ANewItemRecordFocusingChanged: Boolean); override;
    function DoCellDblClick(ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState): Boolean; override;

    function GetOnKeyDown: TKeyEvent; override;
    procedure SetOnKeyDown(Value: TKeyEvent); override;
    procedure DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure RestoreFrom(const AStorageName: string; AStream: TStream; AReaderClass: TcxCustomReaderClass;
      AChildrenCreating, AChildrenDeleting: Boolean;
      AOptions: TcxGridStorageOptions; const ARestoreViewName: string; const AOwnerName: string = ''); override;

    procedure StoreTo(const AStorageName: string; AStream: TStream;
      AWriterClass: TcxCustomWriterClass; AReCreate: Boolean;
      AOptions: TcxGridStorageOptions; const ASaveViewName: string;
      const AOwnerName: string = ''); override;

    procedure DoAssign(ASource: TcxCustomGridView); override;
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetParentComponent(AValue: TComponent); override;
    property OnFocusedRecordChangedEx: TNotifyEvent read FOnFocusedRecordChangedEx write FOnFocusedRecordChangedEx;

  published
    property Version: Byte read FVersion write FVersion default 0;
    property DataController: TcxMDGridDBDataController read GetDataController write SetDataController;

{$IFNDEF PACKAGE}
    property Configure: String write SetConfigure;
{$ENDIF}
  end;

  TcxMDGridBandedColumnOptions = class(TcxGridBandedColumnOptions)
  public
    constructor Create(AItem: TcxCustomGridTableItem); override;
  published
    property AutoWidthSizable default False;
  end;

  TcxMDGridDBBandedColumn = class(TcxGridDBBandedColumn)
  protected
    function DefaultHeaderAlignmentHorz: TAlignment; override;
  public
    function GetOptionsClass: TcxCustomGridTableItemOptionsClass; override;
  end;

{$IFNDEF PACKAGE}
var
  cxGridViewOnCustomDrawCellEvent: TcxGridTableDataCellCustomDrawEvent;
  cxGridViewOptionsViewNotActiveInfoText: String;
{$ENDIF}

implementation

{ TcxMDGrid }

uses Forms, SBaseStringFunctions, Math, SBaseConstants;

const
  SConst_QuickCustomization: String = 'QuickCustomization';

type
  TcxCustomGridViewCrack = class(TcxCustomGridView);
  TcxControlCrack = class(TcxControl);

constructor TcxMDGrid.Create(AOwner: TComponent);
begin
  inherited;

  ParentFont := False;
end;

{$IFNDEF PACKAGE}
procedure TcxMDGrid.DoAutoHeight(AView: TcxCustomGridView);
var
  LViewInfo: TcxGridTableViewInfo;
  LHeight: Integer;
begin
  if Assigned(AView) and (ActiveView = AView) then begin
    LViewInfo := TcxGridTableViewInfo(AView.ViewInfo);
    LHeight := TcxControlCrack(AView.Site).GetScrollBar(sbVertical).GetData.Max
      + LViewInfo.ClientBounds.Top
//      + LViewInfo.HeaderViewInfo.Height
      + LViewInfo.FooterViewInfo.Height
      + LViewInfo.PartsBottomHeight
      + BorderWidth * 2
      + 1;

    if BorderStyle <> cxcbsNone then
      Inc(LHeight, 2);

    if (Constraints.MinHeight > 0) and (Constraints.MinHeight > LHeight) then
      LHeight := Constraints.MinHeight
    else if (Constraints.MaxHeight > 0) and (Constraints.MaxHeight < LHeight) then
      LHeight := Constraints.MaxHeight;

    if Height <> LHeight then
      Height := LHeight;
  end;
end;

procedure TcxMDGrid.DoChange(AGridChange: TcxCustomGridChange);
begin
  try
    if {IsLoading or - because of details} IsDestroying then Exit;
    with AGridChange do
    begin
      Control := Self;
      Execute;
      if FAutoHeight and (AGridChange is TcxGridSizeChange) then
        DoAutoHeight(TcxGridSizeChange(AGridChange).GridView)
    end;
  finally
    AGridChange.Free;
  end;

//  inherited;
end;
{$ENDIF}

{ TcxMDGridLevel }

constructor TcxMDGridLevel.Create(AOwner: TComponent);
begin
  inherited;

  Options.DetailFrameWidth := 0;
end;

{ TcxMDGridDB???TableView }
procedure cxMDGridTableViewCreate(AcxMDGridTableView: TcxGridTableView);
begin
  with AcxMDGridTableView do begin
    DataController.Filter.Options := [fcoCaseInsensitive];
    DataController.Options := [dcoCaseInsensitive, dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoFocusTopRowAfterSorting];

{$IFNDEF PACKAGE}
    if TMethod(cxGridViewOnCustomDrawCellEvent).Code <> nil then
      OnCustomDrawCell := cxGridViewOnCustomDrawCellEvent;
{$ENDIF}
  end;
end;

{$IFNDEF PACKAGE}
procedure cxMDGridTableViewSetConfigure(AcxMDGridTableView: TcxGridTableView; const AValue: String);
var
  LItem: TStringNameItem;
  LSyncMode: Boolean;
begin
  LSyncMode := False;

  with AcxMDGridTableView do begin
    for LItem in EnumStringNameItems(AValue, ',', False) do
      if SameText(LItem.NameLeft, SConst_Indicator) then
        OptionsView.Indicator := True
      else if SameText(LItem.NameLeft, SConst_AutoHeight) then
        OptionsView.CellAutoHeight := True
      else if SameText(LItem.NameLeft, SConst_AutoWidth) then
        OptionsView.ColumnAutoWidth := True
      else if SameText(LItem.NameLeft, SConst_Header) then
        OptionsView.Header := StrToBoolDef(LItem.NameRight, True)

      else if SameText(LItem.NameLeft, SConst_QuickCustomization) then begin
        OptionsCustomize.ColumnsQuickCustomization := True;
        OptionsCustomize.ColumnsQuickCustomizationReordering := qcrEnabled;
      end else if SameText(LItem.NameLeft, SConst_Filtering) then begin
        OptionsCustomize.ColumnFiltering := True;
        Filtering.ColumnAddValueItems     := InArray(LItem.NameRight, 'Values'  , '+');
        Filtering.ColumnFilteredItemsList := InArray(LItem.NameRight, 'Filtered', '+');
        Filtering.ColumnMRUItemsList      := InArray(LItem.NameRight, 'MRU'     , '+');
      end else if SameText(LItem.NameLeft, SConst_Sorting) then
        OptionsCustomize.ColumnSorting := True
      else if SameText(LItem.NameLeft, SConst_Grouping) then
        OptionsCustomize.ColumnGrouping := True
      else if SameText(LItem.NameLeft, SConst_AlwaysExpanded) then
        DataController.Options := DataController.Options + [dcoGroupsAlwaysExpanded]
      else if SameText(LItem.NameLeft, SConst_SyncMode) then
        LSyncMode := True
      else if SameText(LItem.NameLeft, 'DetailInSQLMode') then
        TcxGridDBDataController(DataController).DataModeController.DetailInSQLMode := True
      else
        Raise Exception.Create('TcxGridTableView.Configure.Option = ' + LItem.NameLeft + ' is unknown');
    ;
    TcxGridDBDataController(DataController).DataModeController.SyncMode := LSyncMode;
  end;
end;

procedure cxMDGridTableViewRestoreFrom(AGridView: TcxCustomGridView; const AStorageName: string; AStream: TStream; AReaderClass: TcxCustomReaderClass;
  AChildrenCreating, AChildrenDeleting: Boolean;
  AOptions: TcxGridStorageOptions; const ARestoreViewName: string; const AOwnerName: string = '');
var
  LStorage: TcxStorage;
  LModes: TcxStorageModes;
begin
  AGridView.StoringName := ARestoreViewName;
  LStorage := TcxStorage.Create(AStorageName, AStream);
  try
    LStorage.NamePrefix := AGridView.Owner.ClassName;

    AGridView.StorageOptions := AOptions;
    LModes := [];
    if AChildrenCreating then
      Include(LModes, smChildrenCreating);
    if AChildrenDeleting then
      Include(LModes, smChildrenDeleting);
    LStorage.Modes := LModes;
    LStorage.OnSetStoredPropertyValueError := TcxCustomGridViewCrack(AGridView).HandleSetStoredPropertyValueError;
    AGridView.BeginUpdate;
    try
      TcxCustomGridViewCrack(AGridView).IsRestoring := True;
      try
        LStorage.RestoreFrom(AGridView, AReaderClass);
      finally
        TcxCustomGridViewCrack(AGridView).IsRestoring := False;
      end;
    finally
      AGridView.EndUpdate;
    end;
  finally
    LStorage.Free;
  end;
end;

procedure cxMDGridTableViewStoreTo(AGridView: TcxCustomGridView; const AStorageName: string; AStream: TStream;
  AWriterClass: TcxCustomWriterClass; AReCreate: Boolean;
  AOptions: TcxGridStorageOptions; const ASaveViewName: string;
  const AOwnerName: string = '');
var
  LStorage: TcxStorage;
begin
  AGridView.StoringName := ASaveViewName;
  LStorage := TcxStorage.Create(AStorageName, AStream);
  try
    AGridView.StorageOptions := AOptions;
    LStorage.NamePrefix := AGridView.Owner.ClassName;

    LStorage.ReCreate := AReCreate;
    LStorage.StoreTo(AGridView, AWriterClass);
  finally
    LStorage.Free;
  end;
end;
{$ENDIF}

procedure cxMDGridTableViewDoFocusedRecordChanged
(
  AcxMDGridTableView: TcxGridTableView;
  APrevFocusedRecordIndex, AFocusedRecordIndex,
  APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer;
  ANewItemRecordFocusingChanged: Boolean
);
var
  LPrevFocusedRecord, LFocusedRecord: TcxCustomGridRecord;
  LOnFocusedRecordChangedEx: TNotifyEvent;
begin
  if AcxMDGridTableView.Control <> nil then
    TcxCustomGrid(AcxMDGridTableView.Control).SendNotifications(gnkFocusedRecordChanged);

  if AcxMDGridTableView is TcxMDGridDBTableView then
    LOnFocusedRecordChangedEx := TcxMDGridDBTableView(AcxMDGridTableView).OnFocusedRecordChangedEx
  else if AcxMDGridTableView is TcxMDGridDBBandedTableView then
    LOnFocusedRecordChangedEx := TcxMDGridDBBandedTableView(AcxMDGridTableView).OnFocusedRecordChangedEx
  else
    LOnFocusedRecordChangedEx := nil;

  if Assigned(AcxMDGridTableView.OnFocusedRecordChanged) or Assigned(LOnFocusedRecordChangedEx) then begin
    LFocusedRecord := AcxMDGridTableView.ViewData.GetRecordByIndex(AFocusedRecordIndex);
    if APrevFocusedRecordIndex = AFocusedRecordIndex then
      LPrevFocusedRecord := LFocusedRecord
    else
      LPrevFocusedRecord := AcxMDGridTableView.ViewData.GetRecordByIndex(APrevFocusedRecordIndex);

    if Assigned(AcxMDGridTableView.OnFocusedRecordChanged) then
      AcxMDGridTableView.OnFocusedRecordChanged(AcxMDGridTableView, LPrevFocusedRecord, LFocusedRecord, ANewItemRecordFocusingChanged);

    if Assigned(LOnFocusedRecordChangedEx) then
      LOnFocusedRecordChangedEx(AcxMDGridTableView);
  end;
end;

procedure cxMDGridTableViewDoKeyDown(AcxMDGridTableView: TcxGridTableView; var Key: Word; Shift: TShiftState);
begin
  if Assigned(AcxMDGridTableView.OnKeyDown) then
    AcxMDGridTableView.OnKeyDown(AcxMDGridTableView, Key, Shift);

  if Assigned(AcxMDGridTableView.Control) and (AcxMDGridTableView.Control is TcxMDGrid) then
    with TcxMDGrid(AcxMDGridTableView.Control) do
      if Assigned(FOnKeyDown) then
        FOnKeyDown(AcxMDGridTableView, Key, Shift);
end;

procedure TcxMDGridLevel.DoOnAddView(AView: TcxCustomGridView);
begin
  if Assigned(FOnAddView) and Assigned(AView) then
    FOnAddView(AView);
end;

procedure TcxMDGridLevel.DoOnViewChanged;
begin
  if Assigned(FOnViewChanged) then
    FOnViewChanged(Self);
end;

{$IFDEF DEBUG}
procedure TcxMDGridLevel.SetName(const NewName: TComponentName);
begin
  Assert(Assigned(Control), NewName + ' is not connected to TcxGrid');
  Assert(Control is TcxMDGrid, NewName + ' is connected to ' + Control.ClassName);
  inherited;
end;
{$ENDIF}

procedure TcxMDGridLevel.SetOnAddView(const AValue: TcxCustomGridViewNotifyEvent);
begin
  if TMethod(FOnAddView) <> TMethod(AValue) then begin
    FOnAddView := AValue;
    DoOnAddView(GridView);
  end;
end;

procedure TcxMDGridLevel.SetOnViewChanged(const AValue: TNotifyEvent);
begin
  if TMethod(FOnViewChanged) <> TMethod(AValue) then begin
    FOnViewChanged := AValue;
    DoOnViewChanged;
  end;
end;

{ TcxMDGridDBTableView }

constructor TcxMDGridDBTableView.Create(AOwner: TComponent);
begin
  inherited;
{$IFNDEF PACKAGE}
  inherited SetOnKeyDown(DoKeyDown);
{$ENDIF}
  cxMDGridTableViewCreate(Self);
end;

{$IFNDEF PACKAGE}
procedure TcxMDGridDBTableView.DoAssign(ASource: TcxCustomGridView);
begin
  if ASource is TcxMDGridDBTableView then
    with TcxMDGridDBTableView(ASource) do begin
      Self.OnFocusedRecordChangedEx := OnFocusedRecordChangedEx;
    end;
  inherited DoAssign(ASource);
end;

function TcxMDGridDBTableView.DoCellDblClick(ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := inherited DoCellDblClick(ACellViewInfo, AButton, AShift);

  if (not Result) and (Control is TcxMDGrid) then
    with TcxMDGrid(Control) do
      if Assigned(FOnCellDblClick) then
        FOnCellDblClick(Self, ACellViewInfo, AButton, AShift, Result);
end;

procedure TcxMDGridDBTableView.DoFocusedRecordChanged(APrevFocusedRecordIndex,
  AFocusedRecordIndex, APrevFocusedDataRecordIndex,
  AFocusedDataRecordIndex: Integer; ANewItemRecordFocusingChanged: Boolean);
begin
  cxMDGridTableViewDoFocusedRecordChanged(Self, APrevFocusedRecordIndex, AFocusedRecordIndex, APrevFocusedDataRecordIndex, AFocusedDataRecordIndex, ANewItemRecordFocusingChanged)
end;

procedure TcxMDGridDBTableView.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  cxMDGridTableViewDoKeyDown(Self, Key, Shift);
end;

function TcxMDGridDBTableView.GetOnKeyDown: TKeyEvent;
begin
  Result := FOnKeyDown;
end;
{$ENDIF}

function TcxMDGridDBTableView.GetDataController: TcxMDGridDBDataController;
begin
  Result := TcxMDGridDBDataController(FDataController)
end;

function TcxMDGridDBTableView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxMDGridDBDataController;
end;

function TcxMDGridDBTableView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxMDGridDBColumn
end;

function TcxMDGridDBTableView.GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass;
begin
  Result := TcxMDGridTableOptionsBehavior;
end;

function TcxMDGridDBTableView.GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass;
begin
  Result := TcxMDGridTableOptionsCustomize
end;

function TcxMDGridDBTableView.GetOptionsDataClass: TcxCustomGridOptionsDataClass;
begin
  Result := TcxMDGridTableOptionsData;
end;

function TcxMDGridDBTableView.GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass;
begin
  Result := TcxMDGridTableOptionsSelection;
end;

function TcxMDGridDBTableView.GetOptionsViewClass: TcxCustomGridOptionsViewClass;
begin
  Result := TcxMDGridTableOptionsView;
end;

function TcxMDGridDBTableView.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  Result := TcxMDGridTableViewInfo;
end;

{$IFNDEF PACKAGE}
procedure TcxMDGridDBTableView.RestoreFrom(const AStorageName: string;
  AStream: TStream; AReaderClass: TcxCustomReaderClass; AChildrenCreating,
  AChildrenDeleting: Boolean; AOptions: TcxGridStorageOptions;
  const ARestoreViewName, AOwnerName: string);
begin
  cxMDGridTableViewRestoreFrom(Self, AStorageName, AStream, AReaderClass, AChildrenCreating, AChildrenDeleting, AOptions, ARestoreViewName, AOwnerName)
end;

procedure TcxMDGridDBTableView.SetConfigure(const AValue: String);
begin
  cxMDGridTableViewSetConfigure(Self, AValue);
end;

procedure TcxMDGridDBTableView.SetLevel(AValue: TComponent);
begin
  inherited;

  if Assigned(AValue) and (AValue is TcxMDGridLevel) then
    TcxMDGridLevel(AValue).DoOnViewChanged;
end;

procedure TcxMDGridDBTableView.SetOnKeyDown(Value: TKeyEvent);
begin
  FOnKeyDown := Value;
end;

procedure TcxMDGridDBTableView.StoreTo(const AStorageName: string;
  AStream: TStream; AWriterClass: TcxCustomWriterClass; AReCreate: Boolean;
  AOptions: TcxGridStorageOptions; const ASaveViewName, AOwnerName: string);
begin
  cxMDGridTableViewStoreTo(Self, AStorageName, AStream, AWriterClass, AReCreate, AOptions, ASaveViewName, AOwnerName)
end;

procedure TcxMDGridDBTableView.UpdateControl(AInfo: TcxUpdateControlInfo);
begin
  if (dccUpdateRecord in DataController.SkipChanges) and (AInfo is TcxUpdateRecordInfo) then
    Exclude(DataController.SkipChanges, dccUpdateRecord)
  else
    inherited;
end;

{$ENDIF}

procedure TcxMDGridDBTableView.SetDataController(const Value: TcxMDGridDBDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxMDGridDBTableView.SetParentComponent(AValue: TComponent);
begin
  if Assigned(AValue) and (AValue is TcxMDGridLevel) then begin
    Control := TcxMDGridLevel(AValue).Control;
    with TcxMDGridLevel(AValue) do if GridView = nil then GridView := Self;
    TcxMDGridLevel(AValue).DoOnAddView(Self);
  end else
    inherited;
end;

{ TcxMDGridDBBandedTableView }

constructor TcxMDGridDBBandedTableView.Create(AOwner: TComponent);
begin
  inherited;
{$IFNDEF PACKAGE}
  inherited SetOnKeyDown(DoKeyDown);
{$ENDIF}

  cxMDGridTableViewCreate(Self);
end;

{$IFNDEF PACKAGE}
procedure TcxMDGridDBBandedTableView.DoAssign(ASource: TcxCustomGridView);
begin
  if ASource is TcxMDGridDBBandedTableView then
    with TcxMDGridDBBandedTableView(ASource) do begin
      Self.OnFocusedRecordChangedEx := OnFocusedRecordChangedEx;
    end;
  inherited DoAssign(ASource);
end;

function TcxMDGridDBBandedTableView.DoCellDblClick(ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState): Boolean;
begin
  Result := inherited DoCellDblClick(ACellViewInfo, AButton, AShift);

  if Control is TcxMDGrid then
    with TcxMDGrid(Control) do
      if Assigned(FOnCellDblClick) then
        FOnCellDblClick(Self, ACellViewInfo, AButton, AShift, Result);
end;

procedure TcxMDGridDBBandedTableView.DoFocusedRecordChanged(APrevFocusedRecordIndex, AFocusedRecordIndex, APrevFocusedDataRecordIndex, AFocusedDataRecordIndex: Integer; ANewItemRecordFocusingChanged: Boolean);
begin
  cxMDGridTableViewDoFocusedRecordChanged(Self, APrevFocusedRecordIndex, AFocusedRecordIndex, APrevFocusedDataRecordIndex, AFocusedDataRecordIndex, ANewItemRecordFocusingChanged)
end;

procedure TcxMDGridDBBandedTableView.DoKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  cxMDGridTableViewDoKeyDown(Self, Key, Shift);
end;

function TcxMDGridDBBandedTableView.GetOnKeyDown: TKeyEvent;
begin
  Result := FOnKeyDown
end;
{$ENDIF}

function TcxMDGridDBBandedTableView.GetDataController: TcxMDGridDBDataController;
begin
  Result := TcxMDGridDBDataController(FDataController)
end;

function TcxMDGridDBBandedTableView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxMDGridDBDataController;
end;

function TcxMDGridDBBandedTableView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxMDGridDBBandedColumn
end;

function TcxMDGridDBBandedTableView.GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass;
begin
  Result := TcxMDGridBandedTableOptionsBehavior
end;

function TcxMDGridDBBandedTableView.GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass;
begin
  Result := TcxMDGridBandedTableOptionsCustomize;
end;

function TcxMDGridDBBandedTableView.GetOptionsDataClass: TcxCustomGridOptionsDataClass;
begin
  Result := TcxMDGridTableOptionsData
end;

function TcxMDGridDBBandedTableView.GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass;
begin
  Result := TcxMDGridTableOptionsSelection
end;

function TcxMDGridDBBandedTableView.GetOptionsViewClass: TcxCustomGridOptionsViewClass;
begin
  Result := TcxMDGridBandedTableOptionsView
end;

function TcxMDGridDBBandedTableView.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  Result := (inherited GetViewInfoClass);
end;

{$IFNDEF PACKAGE}
procedure TcxMDGridDBBandedTableView.RestoreFrom(const AStorageName: string;
  AStream: TStream; AReaderClass: TcxCustomReaderClass; AChildrenCreating,
  AChildrenDeleting: Boolean; AOptions: TcxGridStorageOptions;
  const ARestoreViewName, AOwnerName: string);
begin
  cxMDGridTableViewRestoreFrom(Self, AStorageName, AStream, AReaderClass, AChildrenCreating, AChildrenDeleting, AOptions, ARestoreViewName, AOwnerName)
end;

procedure TcxMDGridDBBandedTableView.SetConfigure(const AValue: String);
begin
  cxMDGridTableViewSetConfigure(Self, AValue);

  if InArray(AValue, SConst_QuickCustomization, ',') then begin
    OptionsCustomize.BandsQuickCustomization := True;
    OptionsCustomize.BandsQuickCustomizationReordering := qcrEnabled;
  end;
end;

procedure TcxMDGridDBBandedTableView.SetLevel(AValue: TComponent);
begin
  inherited;

  if Assigned(AValue) and (AValue is TcxMDGridLevel) then
    TcxMDGridLevel(AValue).DoOnViewChanged;
end;

procedure TcxMDGridDBBandedTableView.SetOnKeyDown(Value: TKeyEvent);
begin
  FOnKeyDown := Value
end;

procedure TcxMDGridDBBandedTableView.StoreTo(const AStorageName: string;
  AStream: TStream; AWriterClass: TcxCustomWriterClass; AReCreate: Boolean;
  AOptions: TcxGridStorageOptions; const ASaveViewName, AOwnerName: string);
begin
  cxMDGridTableViewStoreTo(Self, AStorageName, AStream, AWriterClass, AReCreate, AOptions, ASaveViewName, AOwnerName)
end;

procedure TcxMDGridDBBandedTableView.UpdateControl(AInfo: TcxUpdateControlInfo);
begin
  if (dccUpdateRecord in DataController.SkipChanges) and (AInfo is TcxUpdateRecordInfo) then
    Exclude(DataController.SkipChanges, dccUpdateRecord)
  else
    inherited;
end;
{$ENDIF}

procedure TcxMDGridDBBandedTableView.SetDataController(const Value: TcxMDGridDBDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxMDGridDBBandedTableView.SetParentComponent(AValue: TComponent);
begin
  if Assigned(AValue) and (AValue is TcxMDGridLevel) then begin
    Control := TcxMDGridLevel(AValue).Control;
    with TcxMDGridLevel(AValue) do if GridView = nil then GridView := Self;
    TcxMDGridLevel(AValue).DoOnAddView(Self);
  end else
    inherited;
end;

{ TcxMDGridTableOptionsBehavior }

constructor TcxMDGridTableOptionsBehavior.Create(AGridView: TcxCustomGridView);
begin
  inherited;

  CellHints                 := True;
  ImmediateEditor           := False;
  IncSearch                 := True;
  ExpandMasterRowOnDblClick := False;
  RecordScrollMode          := rsmByPixel;
end;

{ TcxMDGridTableOptionsData }

constructor TcxMDGridTableOptionsData.Create(AGridView: TcxCustomGridView);
begin
  inherited;

  FCancelOnExit  := False;
  FInserting     := False;
  FEditing       := False;
  FDeleting      := False;
  FDeletingConfirmation := False;
end;

{ TcxMDGridTableOptionsSelection }

constructor TcxMDGridTableOptionsSelection.Create(AGridView: TcxCustomGridView);
begin
  inherited;

  // HideFocusRectOnExit := True;
  // HideSelection := False;
  InvertSelect  := False;
end;

{ TcxMDGridTableOptionsView }

constructor TcxMDGridTableOptionsView.Create(AGridView: TcxCustomGridView);
begin
  inherited;

  CellEndEllipsis               := True;
  ExpandButtonsForEmptyDetails  := False;
  GridLineColor                 := clSilver;
  GroupByBox                    := False;
  HeaderEndEllipsis             := True;

{$IFNDEF PACKAGE}
  FNotActiveInfoText := cxGridViewOptionsViewNotActiveInfoText;
{$ENDIF}
end;

function TcxMDGridTableOptionsView.IsNotActiveInfoTextStored: Boolean;
begin
{$IFNDEF PACKAGE}
  Result := (FNotActiveInfoText <> cxGridViewOptionsViewNotActiveInfoText)
{$ELSE}
  Result := (not FNotActiveInfoText.IsEmpty)
{$ENDIF}
end;

procedure TcxMDGridTableOptionsView.SetNotActiveInfoText(const AValue: String);
begin
  if FNotActiveInfoText <> AValue then begin
    FNotActiveInfoText := AValue;
    Changed(vcLayout);
  end;
end;

{ TcxMDGridBandedTableOptionsBehavior }

constructor TcxMDGridBandedTableOptionsBehavior.Create(AGridView: TcxCustomGridView);
begin
  inherited;

  CellHints                 := True;
  ImmediateEditor           := False;
  IncSearch                 := True;
  ExpandMasterRowOnDblClick := False;
  RecordScrollMode          := rsmByPixel;
end;

{ TcxMDGridBandedTableOptionsView }

constructor TcxMDGridBandedTableOptionsView.Create(AGridView: TcxCustomGridView);
begin
  inherited;

  CellEndEllipsis               := True;
  ExpandButtonsForEmptyDetails  := False;
  GridLineColor                 := clSilver;
  GroupByBox                    := False;
  HeaderEndEllipsis             := True;

  BandCaptionsInColumnAlternateCaption := True;
  BandHeaderEndEllipsis := True;

{$IFNDEF PACKAGE}
  FNotActiveInfoText := cxGridViewOptionsViewNotActiveInfoText;
{$ENDIF}
end;

function TcxMDGridBandedTableOptionsView.IsNotActiveInfoTextStored: Boolean;
begin
{$IFNDEF PACKAGE}
  Result := (FNotActiveInfoText <> cxGridViewOptionsViewNotActiveInfoText)
{$ELSE}
  Result := (not FNotActiveInfoText.IsEmpty)
{$ENDIF}
end;

procedure TcxMDGridBandedTableOptionsView.SetNotActiveInfoText(const AValue: String);
begin
  if FNotActiveInfoText <> AValue then begin
    FNotActiveInfoText := AValue;
    Changed(vcLayout);
  end;
end;

{ TcxMDGridBandedTableOptionsCustomize }

constructor TcxMDGridBandedTableOptionsCustomize.Create(AGridView: TcxCustomGridView);
begin
  inherited;

  ColumnFiltering := False;
  ColumnSorting := False;
  BandSizing := False;
end;

{ TcxMDGridTableViewInfo }

function TcxMDGridTableViewInfo.GetNoDataInfoText: String;
var
  LOptionsView: TcxGridTableOptionsView;
begin
  if GridView.DataController.Active then
    Result := (inherited GetNoDataInfoText)
  else begin
    LOptionsView := GridView.OptionsView;
    if LOptionsView is TcxMDGridTableOptionsView then
      Result := TcxMDGridTableOptionsView(LOptionsView).NotActiveInfoText
    else if LOptionsView is TcxMDGridBandedTableOptionsView then
      Result := TcxMDGridBandedTableOptionsView(LOptionsView).NotActiveInfoText
  end;
end;

{ TcxMDGridBandedTableController }

function TcxMDGridBandedTableController.GetItemsCustomizationPopupClass: TcxCustomGridItemsCustomizationPopupClass;
begin
  Result := TcxGridBandedColumnsCustomizationPopup;
end;

{ TcxGridBandedColumnsCustomizationPopup }

function CompareBandedCols(Item1, Item2: Pointer): Integer;
var
  APos1, APos2: TcxGridBandedColumnPosition;
  IsFirst: Boolean;
begin
  APos1 := TcxGridBandedColumn(Item1).Position;
  APos2 := TcxGridBandedColumn(Item2).Position;

  if Item1 = Item2 then
    Result := 0
  else
  begin
    IsFirst := (APos1.BandIndex < APos2.BandIndex)
     or
     ((APos1.BandIndex = APos2.BandIndex) and (APos1.RowIndex < APos2.RowIndex))
     or
     ((APos1.BandIndex = APos2.BandIndex) and (APos1.RowIndex = APos2.RowIndex)
        and (APos1.ColIndex < APos2.ColIndex));

    Result := IfThen(IsFirst, -1, 1);
 end;
end;

procedure TcxGridBandedColumnsCustomizationPopup.AddCheckListBoxItems;
var
  I: Integer;
  LList: TList;
  LColumn: TcxGridBandedColumnCrack;
begin
  LList := TList.Create();
  try
    LList.Assign(GridView.ItemsList);
    LList.Sort(CompareBandedCols);

    with CheckListBox.Items do begin
      BeginUpdate;
      try
        for I := 0 to LList.Count - 1 do begin
          LColumn := TcxGridBandedColumnCrack(LList[I]);
          if LColumn.VisibleInQuickCustomizationPopup then
            with Add do begin
              Checked     := LColumn.Visible;
              ItemObject  := LColumn;
              Text        := LColumn.GetAlternateCaption;
            end;
        end;
      finally
        EndUpdate;
      end;
    end;
  finally
    LList.Free;
  end;
end;

function TcxGridBandedColumnsCustomizationPopup.GridView: TcxGridBandedTableViewCrack;
begin
  Result := TcxGridBandedTableViewCrack(inherited GridView);
end;

{ TcxMDGridColumnOptions }

constructor TcxMDGridColumnOptions.Create(AItem: TcxCustomGridTableItem);
begin
  inherited;
  FAutoWidthSizable := False;

end;

{ TcxMDGridDBColumn }

function TcxMDGridDBColumn.DefaultHeaderAlignmentHorz: TAlignment;
begin
  Result := taCenter
end;

function TcxMDGridDBColumn.GetOptionsClass: TcxCustomGridTableItemOptionsClass;
begin
  Result := TcxMDGridColumnOptions
end;

{$IFNDEF PACKAGE}
procedure TcxMDGridDBColumn.SetIndexAfter(const Value: TcxMDGridDBColumn);
begin
  Index := Value.Index + 1
end;
{$ENDIF}

{ TcxMDGridDBBandedColumn }

function TcxMDGridDBBandedColumn.DefaultHeaderAlignmentHorz: TAlignment;
begin
  Result := taCenter
end;

function TcxMDGridDBBandedColumn.GetOptionsClass: TcxCustomGridTableItemOptionsClass;
begin
  Result := TcxMDGridBandedColumnOptions
end;

{ TcxMDGridBandedColumnOptions }

constructor TcxMDGridBandedColumnOptions.Create(AItem: TcxCustomGridTableItem);
begin
  inherited;
  FAutoWidthSizable := False;
end;

{ TcxMDGridTableOptionsCustomize }

constructor TcxMDGridTableOptionsCustomize.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  ColumnGrouping := False;
  ColumnFiltering := False;
  ColumnSorting := False;
end;

{ TcxMDGridDBDataController }

constructor TcxMDGridDBDataController.Create(AOwner: TComponent);
begin
  inherited;

  Options := [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoCaseInsensitive, dcoFocusTopRowAfterSorting];
end;

function TcxMDGridDBDataController.GetDataProviderClass: TcxCustomDataProviderClass;
begin
  Result := TcxMDDBDataProvider;
end;

{ TcxMDDBDataProvider }

procedure TcxMDDBDataProvider.EditingRecord;
begin
  Include(DataController.SkipChanges, dccUpdateRecord);

  inherited;
end;

function TcxMDDBDataProvider.GetDataController: TcxMDGridDBDataController;
begin
  Result := inherited DataController as TcxMDGridDBDataController;
end;

initialization
  RegisterClasses
  (
    [
      TcxMDGrid, TcxMDGridLevel,
      TcxMDGridDBDataController,
      TcxMDGridDBTableView, TcxMDGridDBColumn,
      TcxMDGridDBBandedTableView, TcxMDGridDBBandedColumn
    ]
  );

finalization
  UnRegisterClasses
  (
    [
      TcxMDGrid, TcxMDGridLevel,
      TcxMDGridDBDataController,
      TcxMDGridDBTableView, TcxMDGridDBColumn,
      TcxMDGridDBBandedTableView, TcxMDGridDBBandedColumn
    ]
  );

end.
