unit SBaseDataSourceControls;
{$i config.inc}

interface
uses
  Classes, Controls, Forms, DB, SysUtils, Messages, Windows, Variants,
  SBaseDataModule, FieldDataLink, cxCustomData,
  cxGrid, cxGridTableView, cxGridCustomTableView, cxGridCustomView, cxEdit,
  cxTL, cxDBTL, cxMDDBTreeList,
  cxVGrid, cxDBVGrid, cxMDVGrid,
  cxControls, cxGridDBDataDefinitions, dxBar;

type
//  TDataSourceChangeNotify = procedure() of object;
  TDataSourceControlPropertiesLink = class(TCollectionItem)
  private
    FTable: String;
    FFieldNames: String;
    FKeyField: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Table: String read FTable write FTable;
    property FieldNames: String read FFieldNames write FFieldNames;
    property KeyField: Boolean read FKeyField write FKeyField default False;

//    property OnChangeNotify: TNotifyEvent;
  end;

  TDataSourceControlPropertiesLinks = class(TCollection)
  private
    function GetItem(Index: Integer): TDataSourceControlPropertiesLink;
  public
    property Items[Index: Integer]: TDataSourceControlPropertiesLink read GetItem; default;
  end;

  TDataSourceUpdateState = (dsusActive, dsusVolume, dsusData, dsusFocusedRecord);
  TDataSourceUpdateStates = Set Of TDataSourceUpdateState;

  TDataSourceControls = class;
  TDataSourceControlItem = class;
  TDataSourceControlProperties = class;

  { --- TDataSourceControlsXXX --- }
  TDataSourceControlPropertiesClass = class of TDataSourceControlProperties;
  TDataSourceControlPropertiesClassReference = record
    ControlClass: TClass;
    ControlPropertiesClass: TDataSourceControlPropertiesClass;
  end;

  TPopupMenuKind = (pmClassic, pmDxBar);
  TPopupMenuKinds = set of TPopupMenuKind;

  TDataSourceControlProperties = class(TObject)
  strict private
    class var FClasses: Array of TDataSourceControlPropertiesClassReference;
  public
    class procedure RegisterClass(AControlClass: TClass; AControlPropertiesClass: TDataSourceControlPropertiesClass);
    class procedure ClearClasses;
  private
    FCollectionItem : TDataSourceControlItem;
    FGrid           : TComponent;
    FLinks          : TDataSourceControlPropertiesLinks;
    FAfterUpdate    : TNotifyEvent;

    FActive     : Boolean;
    FIsNotEmpty : Boolean;

    function Collection: TDataSourceControls;
    procedure SetAfterUpdate(const AValue: TNotifyEvent);
  protected
    procedure InternalCreate; dynamic; abstract;
//    procedure Loaded; override;
    function GetParams: TParams;
    function GetPopupMenu: TComponent; virtual;
    procedure SetPopupMenu(APopupMenu: TComponent); virtual;

    procedure DoDelete; dynamic; abstract;
    function  GetDataSource: TDataSource; virtual; abstract;
    function  GetDataSet(const Index: Integer): TDataSet; virtual; abstract;

    function  GetBookmark: TDataSourceBookmark; dynamic; abstract;
    procedure SetBookmark(Value: TDataSourceBookmark); dynamic; abstract;

    function  GetNoDataToDisplayInfoText: String; virtual;
    procedure SetNoDataToDisplayInfoText(const AValue: String); virtual;

    function  GetActive: Boolean; dynamic; abstract;
  public
    class function CreateControlProperties(ACollectionItem: TDataSourceControlItem; AGrid: TComponent): TDataSourceControlProperties;
    destructor Destroy; override;

    function  UpdateIsNotEmpty(AIsNotEmpty: Boolean): Boolean;
    procedure DoUpdate(AUpdateStates: TDataSourceUpdateStates; ADataSource: TComponent = nil);

    procedure BeginUpdate(var ADataSourceStateBookmark: TDataSourceStateBookmark); dynamic; abstract;
    procedure EndUpdate(const ADataSourceStateBookmark: TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores = DataSetStateBookmarkRestores_All; ADataSourceRestore: TDataSourceBookmarkRestores = DataSourceBookmarkRestores_All); dynamic; abstract;

    function  DefaultItemClick(AShortCutReturnTest: Boolean = True): Boolean;

    procedure SetInsertEnabled(AEnabled: Boolean); dynamic;
    procedure SetCopyEnabled(AEnabled: Boolean); dynamic;
    procedure SetEditEnabled(AEnabled: Boolean); dynamic;
    procedure SetDeleteEnabled(AEnabled: Boolean); dynamic;

    procedure Refresh; dynamic; abstract;
    procedure RefreshRecord; dynamic;

    function  CanModify : Boolean; dynamic; abstract;
    function  Inserted  : Boolean; dynamic; abstract;
    function  Editing   : Boolean; dynamic; abstract;

    procedure InPlaceInsert     ; dynamic; abstract;
    procedure InPlaceInsertChild(AChildComponent: TComponent); dynamic; abstract;
    procedure InPlaceEdit       ; dynamic; abstract;
    procedure Post    ; dynamic; abstract;
    procedure Cancel  ; dynamic; abstract;
    procedure InPlaceSyncEdit; dynamic; abstract;

    procedure Delete;

//    procedure Expand  ; dynamic; abstract;
//    procedure Collapse; dynamic; abstract;

    function ConfirmRecordAction(const AMessage: String): Boolean;

    function GetFormDataSourceParamValues(const AParamNames: String; var AUnknownParams: String): Variant;
    function GetFormDataSourceParamAndFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant; dynamic;

    function GetWinControl: TWinControl; dynamic;
    function CanAssignPopupMenu: TPopupMenuKinds; dynamic;

    function IsDataActive: Boolean; dynamic;
    function IsNotEmpty: Boolean; dynamic; abstract;
    function VisibleRecordCount: Integer; dynamic;
    function FocusedRecodIsData: Boolean; dynamic;

    function  SupportFolding: Boolean; dynamic;
    function  FoldingActive: Boolean; dynamic;
    procedure FullExpand; dynamic;
    procedure FullCollapse; dynamic;

    function KeyFieldNames : String ; dynamic;
    function KeyFieldValues: Variant; dynamic;

    function Evaluate(const AExpression: string; AConditionParser: PExprParser = nil): Variant; dynamic;
    function EvaluateBoolean(const AExpression: string; AConditionParser: PExprParser = nil; ADefaultValue: Boolean = False): Boolean;

    property DataSource: TDataSource read GetDataSource;
    property SourceDataSet: TDataSet index 1 read GetDataSet;
    property TargetDataSet: TDataSet index 0 read GetDataSet;
    property Params: TParams read GetParams;

    property PopupMenu: TComponent read GetPopupMenu write SetPopupMenu;
    property NoDataToDisplayInfoText: String read GetNoDataToDisplayInfoText write SetNoDataToDisplayInfoText;
    property AfterUpdate: TNotifyEvent read FAfterUpdate write SetAfterUpdate;
  end;

  TDataSourceControlItem = class(TCollectionItem)
  strict private
    FLoadedLinks: TDataSourceControlPropertiesLinks;
  private
    function GetGrid: TComponent;
    function GetLinks: TDataSourceControlPropertiesLinks;
    procedure SetLinks(const Value: TDataSourceControlPropertiesLinks);
  protected
    FProperties: TDataSourceControlProperties;

    procedure SetGrid(const AValue: TComponent); overload;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    property Properties: TDataSourceControlProperties read FProperties;
  published
    property Item: TComponent read GetGrid write SetGrid;
    property Links: TDataSourceControlPropertiesLinks read GetLinks write SetLinks;
  end;

  TDataSourceDefaultUpdateControlProperties = class(TDataSourceControlProperties)
    procedure DataSourceActiveChanged(Sender: TObject);
    procedure DataSourceRecordChanged(Sender: TObject);
  protected
    FDataSetLink: TDataSourceDataLink;

    procedure InternalCreate; override;

    procedure DoDelete; override;
    function  GetDataSource: TDataSource; override;
    function  GetDataSet(const Index: Integer): TDataSet; override;

    function  GetBookmark: TDataSourceBookmark; override;
    procedure SetBookmark(Value: TDataSourceBookmark); override;

    function  GetActive: Boolean; override;

    property DataSource: TDataSource read GetDataSource;
  public
    destructor Destroy; override;
    procedure Refresh; override;

    procedure BeginUpdate(var ADataSourceStateBookmark: TDataSourceStateBookmark); override;
    procedure EndUpdate(const ADataSourceStateBookmark: TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores); override;

    function  CanModify : Boolean; override;
    function  Inserted  : Boolean; override;
    function  Editing   : Boolean; override;

    function  IsNotEmpty: Boolean; override;

    procedure InPlaceInsert; override;
    procedure InPlaceInsertChild(AChildComponent: TComponent); override;
    procedure InPlaceEdit; override;
    procedure Post; override;
    procedure Cancel; override;
    procedure InPlaceSyncEdit; override;
  end;

  TDataSourceCxGridUpdateControlProperties = class;

  TSBaseDataSourceControlsCxGridNotification = class(TcxCustomGridNotification)
  private
    FOwner: TDataSourceCxGridUpdateControlProperties;
  protected
    function NotificationKinds: TcxGridNotificationKinds; override;
    procedure Notify(AKind: TcxGridNotificationKind; AData: TObject; var AHandled: Boolean); override;
  public
    constructor Create(AOwner: TDataSourceCxGridUpdateControlProperties);
  end;

  TDataSourceCxGridUpdateControlProperties = class(TDataSourceControlProperties)
    procedure cxGridTableViewDataControllerDataChanged(Sender: TObject);
    procedure cxGridTableViewFocusedRecordChanged;
    procedure cxGridCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
    procedure cxGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure cxGridFocusedTableViewChanged(Sender: TcxCustomGrid; APrevFocusedView, AFocusedView: TcxCustomGridView);
  private
    FcxGridNotification: TSBaseDataSourceControlsCxGridNotification;
  protected
    FcxGridFocusedTableViewChanged: TcxGridFocusedViewChangedEvent;
    FcxGridCellDblClick: TcxGridCellClickEvent;
    FcxGridKeyDown: TKeyEvent;

    procedure InternalCreate; override;
    function  GetPopupMenu: TComponent; override;
    procedure SetPopupMenu(APopupMenu: TComponent); override;
    procedure DoDelete; override;

    function  GetDataController: TcxCustomDataController;
    function  GetDataSource: TDataSource; override;
    function  GetDataSet(const Index: Integer): TDataSet; override;

    function  GetBookmark: TDataSourceBookmark; override;
    procedure SetBookmark(Value: TDataSourceBookmark); override;

    function  GetActive: Boolean; override;
  public
    destructor Destroy; override;

    procedure BeginUpdate(var ADataSourceStateBookmark: TDataSourceStateBookmark); override;
    procedure EndUpdate(const ADataSourceStateBookmark: TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores); override;

    procedure Refresh; override;
    function  IsNotEmpty: Boolean; override;
    function  VisibleRecordCount: Integer; override;
    function  FocusedRecodIsData: Boolean; override;

    function GetFormDataSourceParamAndFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant; override;

    function  CanModify : Boolean; override;
    function  Inserted: Boolean; override;
    function  Editing  : Boolean; override;

    function Evaluate(const AExpression: string; AConditionParser: PExprParser = nil): Variant; override;

    function CanAssignPopupMenu: TPopupMenuKinds; override;

    procedure InPlaceInsert; override;
    procedure InPlaceInsertChild(AChildComponent: TComponent); override;
    procedure InPlaceEdit; override;
    procedure Post; override;
    procedure Cancel; override;
    procedure InPlaceSyncEdit; override;

    function KeyFieldNames : String ; override;
    function KeyFieldValues: Variant; override;
  end;

  TDataSourceCxGridTableViewUpdateControlProperties = class(TDataSourceControlProperties)
    procedure cxGridTableViewDataControllerDataChanged(Sender: TObject);
    procedure cxGridTableViewFocusedRecordChanged(Sender: TObject);// APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
    procedure cxGridTableViewCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
    procedure cxGridTableViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure cxGridTableViewEditKeyDown(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem; AEdit: TcxCustomEdit; var Key: Word; Shift: TShiftState);
  protected
    FcxGridTableViewCellDblClick: TcxGridCellClickEvent;
    FcxGridTableViewKeyDown: TKeyEvent;
    FcxGridTableViewEditKeyDown: TcxGridEditKeyEvent;

    procedure InternalCreate; override;
    function  GetPopupMenu: TComponent; override;
    procedure SetPopupMenu(APopupMenu: TComponent); override;
    procedure DoDelete; override;

    function  GetDataController: TcxCustomDataController; inline;
    function  GetDataSource: TDataSource; override;
    function  GetDataSet(const Index: Integer): TDataSet; override;

    function GetBookmark: TDataSourceBookmark; override;
    procedure SetBookmark(Value: TDataSourceBookmark); override;

    function  GetNoDataToDisplayInfoText: String; override;
    procedure SetNoDataToDisplayInfoText(const AValue: String); override;

    function GetActive: Boolean; override;
  public
    function  SupportFolding: Boolean; override;
    function  GetIsGrouping: Boolean;
    function  FoldingActive: Boolean; override;
    procedure FullExpand; override;
    procedure FullCollapse; override;
  public
    destructor Destroy; override;
    procedure ReleaseEvents;

    procedure BeginUpdate(var ADataSourceStateBookmark: TDataSourceStateBookmark); override;
    procedure EndUpdate(const ADataSourceStateBookmark: TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores); override;

    procedure Refresh; override;
    function IsNotEmpty: Boolean; override;
    function  VisibleRecordCount: Integer; override;
    function  FocusedRecodIsData: Boolean; override;

    function GetFormDataSourceParamAndFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant; override;
    function GetWinControl: TWinControl; override;
    function CanAssignPopupMenu: TPopupMenuKinds; override;

    function  CanModify : Boolean; override;
    function  Inserted  : Boolean; override;
    function  Editing   : Boolean; override;

    procedure InPlaceInsert; override;
    procedure InPlaceInsertChild(AChildComponent: TComponent); override;
    procedure InPlaceEdit; override;
    procedure Post; override;
    procedure Cancel; override;
    procedure InPlaceSyncEdit; override;

//    procedure Expand; override;
//    procedure Collapse; override;

    function KeyFieldNames : String ; override;
    function KeyFieldValues: Variant; override;

    function Evaluate(const AExpression: string; AConditionParser: PExprParser = nil): Variant; override;
  end;

  TDataSourceCxMDDBTreeListUpdateControlProperties = class(TDataSourceControlProperties)
    procedure cxDBTreeListDataChanged(Sender: TObject);
    procedure cxDBTreeListFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    procedure cxDBTreeListDblClick(Sender: TObject);
    procedure cxDBTreeListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FAlwaysShowEditor: Boolean;
  protected
    FcxDBTreeListDataChanged: TNotifyEvent;
    FcxDBTreeListFocusedNodeChanged: TcxTreeListFocusedNodeChangedEvent;
    FcxDBTreeListDblClick: TNotifyEvent;
    FcxDBTreeListKeyDown: TKeyEvent;

    procedure InternalCreate; override;

    function  GetDataSource: TDataSource; override;
    function  GetDataSet(const Index: Integer): TDataSet; override;
    function  GetPopupMenu: TComponent; override;
    procedure SetPopupMenu(APopupMenu: TComponent); override;
    procedure DoDelete; override;

    function GetBookmark: TDataSourceBookmark; override;
    procedure SetBookmark(Value: TDataSourceBookmark); override;

    function GetActive: Boolean; override;
  public
    function  SupportFolding: Boolean; override;
    function  FoldingActive: Boolean; override;
    procedure FullExpand; override;
    procedure FullCollapse; override;
  public
    destructor Destroy; override;

    procedure BeginUpdate(var ADataSourceStateBookmark: TDataSourceStateBookmark); override;
    procedure EndUpdate(const ADataSourceStateBookmark: TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores); override;

    procedure Refresh; override;

    function GetFormDataSourceParamAndFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant; override;

    function CanAssignPopupMenu: TPopupMenuKinds; override;

    function  CanModify : Boolean; override;
    function  Inserted  : Boolean; override;
    function  Editing   : Boolean; override;

    procedure InPlaceInsert; override;
    procedure InPlaceInsertChild(AChildComponent: TComponent); override;
    procedure InPlaceEdit; override;
    procedure Post; override;
    procedure Cancel; override;
    procedure InPlaceSyncEdit; override;

    function KeyFieldNames : String ; override;
    function KeyFieldValues: Variant; override;

    function IsNotEmpty: Boolean; override;
    function VisibleRecordCount: Integer; override;
    function FocusedRecodIsData: Boolean; override;

    function Evaluate(const AExpression: string; AConditionParser: PExprParser = nil): Variant; override;
  end;

  TcxDataSourceMDDBVerticalGridUpdateControlProperties = class(TDataSourceControlProperties)
    procedure cxDBVerticalGridUpdateData(AControl: TObject; AInfo: TcxUpdateControlInfo);
//    procedure cxDBVerticalGridDataChanged(Sender: TObject);
//    procedure cxDBVerticalGridFocusedRecordChanged(Sender: TcxVirtualVerticalGrid; APrevFocusedRecord, AFocusedRecord: Integer);

    procedure cxDBVerticalGridDblClick(Sender: TObject);
    procedure cxDBVerticalGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FAlwaysShowEditor: Boolean;
  protected
//    FcxDBVerticalGridDataChanged: TNotifyEvent;
//    FcxDBVerticalGridFocusedRecordChanged: TcxVerticalGridFocusedRecordChangedEvent;
    FcxDBVerticalGridDblClick: TNotifyEvent;
    FcxDBVerticalGridKeyDown: TKeyEvent;

    procedure InternalCreate; override;
    function  GetDataSet(const Index: Integer): TDataSet; override;
    function  GetPopupMenu: TComponent; override;
    procedure SetPopupMenu(APopupMenu: TComponent); override;
    procedure DoDelete; override;

    function GetBookmark: TDataSourceBookmark; override;
    procedure SetBookmark(Value: TDataSourceBookmark); override;

    function GetActive: Boolean; override;
  public
    function  SupportFolding: Boolean; override;
    function  FoldingActive: Boolean; override;
    procedure FullExpand; override;
    procedure FullCollapse; override;
  public
    destructor Destroy; override;

    procedure BeginUpdate(var ADataSourceStateBookmark: TDataSourceStateBookmark); override;
    procedure EndUpdate(const ADataSourceStateBookmark: TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores); override;

    function CanAssignPopupMenu: TPopupMenuKinds; override;

    procedure Refresh; override;

    function  CanModify : Boolean; override;
    function  Inserted  : Boolean; override;
    function  Editing   : Boolean; override;

    procedure SetInsertEnabled(AEnabled: Boolean); override;
    procedure SetEditEnabled(AEnabled: Boolean); override;
    procedure SetDeleteEnabled(AEnabled: Boolean); override;

    function IsNotEmpty: Boolean; override;

    procedure InPlaceInsert; override;
    procedure InPlaceInsertChild(AChildComponent: TComponent); override;
    procedure InPlaceEdit; override;
    procedure Post; override;
    procedure Cancel; override;
    procedure InPlaceSyncEdit; override;

//    function KeyFieldNames : String ; override;
//    function KeyFieldValues: Variant; override;
  end;

  TDataSourceControls = class(TCollection)
  private
    function GetItem(Index: Integer): TDataSourceControlItem;
  protected
    FOwnerForm: TForm;
  public
    constructor Create(AOwner: TForm); reintroduce;

    function FindGridControl(const AGrid: TComponent): TDataSourceControlProperties;
    function CreateGridControl(const AGrid: TComponent): TDataSourceControlProperties;
    function FindOrCreateGridControl(const AGrid: TComponent): TDataSourceControlProperties;
    procedure DeleteGridControl(AGrid: TComponent);

    property Items[Index: Integer]: TDataSourceControlItem read GetItem; default;
  end;
  { --- /TDataSourceControlsXXX --- }

implementation

uses
  SBaseForm, SBaseVariantFunctions, SBaseConstants, SBaseUtils, hsDialogs,
  SBaseCxUtils, cxGridLevel, cxDBData, SBaseControls,
  cxGridDBTableView, cxGridDBBandedTableView, SBaseItemReactions,
  SBaseDataHelperInternal, cxMDGrid;

type
  TcxGridTableViewCrack = class(TcxGridTableView);
  TcxCustomDataControllerCrack = class(TcxCustomDataController);
  TcxCustomDataProviderCrack = class(TcxCustomDataProvider);
  TcxCustomGridRecordCrack = class(TcxCustomGridRecord);

{$REGION 'TDataSourceControlPropertiesLinks'}

{ TDataSourceControlPropertiesLinks }

function TDataSourceControlPropertiesLinks.GetItem(Index: Integer): TDataSourceControlPropertiesLink;
begin
  Result := TDataSourceControlPropertiesLink(inherited GetItem(Index));
end;
{$ENDREGION 'TDataSourceControlPropertiesLinks'}

{$REGION 'TDataSourceControlItem'}
{ TDataSourceControlItem }

constructor TDataSourceControlItem.Create(Collection: TCollection);
begin
  inherited;
  FLoadedLinks := TDataSourceControlPropertiesLinks.Create(TDataSourceControlPropertiesLink);
end;

destructor TDataSourceControlItem.Destroy;
begin
  inherited;
  FreeAndNil(FProperties);
  FreeAndNil(FLoadedLinks);
end;

function TDataSourceControlItem.GetGrid: TComponent;
begin
  if FProperties <> nil then
    Result := FProperties.FGrid
  else
    Result := nil;
end;

function TDataSourceControlItem.GetLinks: TDataSourceControlPropertiesLinks;
begin
  if FProperties <> nil then
    Result := FProperties.FLinks
  else
    Result := FLoadedLinks;
end;

procedure TDataSourceControlItem.SetGrid(const AValue: TComponent);
var
  I: Integer;
begin
  FProperties := nil;
  for I := Pred(Collection.Count) downto 0 do
    if (Index <> I) and (TDataSourceControlItem(Collection.Items[I]).Item = AValue) then begin
      FProperties := TDataSourceControlItem(Collection.Items[I]).FProperties;
      TDataSourceControlItem(Collection.Items[I]).FProperties := nil;
      Collection.Delete(I);

      FProperties.FCollectionItem := Self;
      Break;
    end;

  if not Assigned(FProperties) then
    FProperties := TDataSourceDefaultUpdateControlProperties.CreateControlProperties(Self, AValue);

  if Assigned(FLoadedLinks) then begin
    FProperties.FLinks.Assign(FLoadedLinks);
    FreeAndNil(FLoadedLinks);
  end;
end;

procedure TDataSourceControlItem.SetLinks(const Value: TDataSourceControlPropertiesLinks);
begin
  Links.Assign(Value);
end;
{$ENDREGION 'TDataSourceControlItem'}

{$REGION 'TDataSourceControlProperties'}
{ TDataSourceControlProperties }

class procedure TDataSourceControlProperties.RegisterClass(AControlClass: TClass; AControlPropertiesClass: TDataSourceControlPropertiesClass);
var
  I: Integer;
begin
  for I := Pred(Length(FClasses)) downto 0 do
    if FClasses[I].ControlClass = AControlClass then begin
      FClasses[I].ControlPropertiesClass := AControlPropertiesClass;
      Exit;
    end;

  I := Length(FClasses);
  SetLength(FClasses, Succ(I));

  with FClasses[I] do begin
    ControlClass := AControlClass;
    ControlPropertiesClass := AControlPropertiesClass;
  end;
end;

function TDataSourceControlProperties.SupportFolding: Boolean;
begin
  Result := False;
end;

function TDataSourceControlProperties.UpdateIsNotEmpty(AIsNotEmpty: Boolean): Boolean;
begin
  Result := (FIsNotEmpty <> AIsNotEmpty);
  FIsNotEmpty := AIsNotEmpty;
end;

function TDataSourceControlProperties.CanAssignPopupMenu: TPopupMenuKinds;
begin
  Result := [];
end;

class procedure TDataSourceControlProperties.ClearClasses;
begin
  SetLength(FClasses, 0);
end;

function TDataSourceControlProperties.GetParams: TParams;
var
  LDataSet: TDataSet;
begin
  LDataSet := GetDataSet(1);

  if LDataSet <> nil then
{$IFDEF DELPHIXE3}
    Result := IProviderSupportNG(LDataSet).PSGetParams
{$ELSE}
    Result := IProviderSupport(LDataSet).PSGetParams
{$ENDIF}
  else
    Result := nil;
end;

function TDataSourceControlProperties.GetFormDataSourceParamValues(const AParamNames: String; var AUnknownParams: String): Variant;
var
  LParams: TParams;
  LUnknownParams: String;
begin
  LParams := Params;
  if LParams <> nil then begin
    Result := LParams.GetValues(AParamNames, @AUnknownParams);
    if (not AUnknownParams.IsEmpty) then begin
      LUnknownParams := AUnknownParams;
      VarArrayOverWrite(
        AParamNames,
        Result,
        LUnknownParams,
        TSBaseFrm(Collection.FOwnerForm).GetFormParamValues(LUnknownParams, AUnknownParams)
      );
    end;
  end else
    Result := TSBaseFrm(Collection.FOwnerForm).GetFormParamValues(AParamNames, AUnknownParams);
end;

function TDataSourceControlProperties.GetNoDataToDisplayInfoText: String;
begin
  Result := ''
end;

procedure TDataSourceControlProperties.SetNoDataToDisplayInfoText(const AValue: String);
begin
  Assert(AValue.IsEmpty);
end;

procedure TDataSourceControlProperties.SetPopupMenu(APopupMenu: TComponent);
begin
  Assert(APopupMenu = nil);
end;

function TDataSourceControlProperties.GetFormDataSourceParamAndFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant;
var
  LDataSet: TDataSet;
  LUnknownParams: String;
begin
  LDataSet := TargetDataSet;
  if LDataSet = nil then
    Raise Exception.Create(SAbstractError)
  else begin
    Result := LDataSet.Fields.GetValues(AFieldNames, @AUnknownFields);
    if (not AUnknownFields.IsEmpty) then begin
      LUnknownParams := AUnknownFields;
      VarArrayOverWrite
      (
        AFieldNames,
        Result,
        LUnknownParams,
        GetFormDataSourceParamValues(LUnknownParams, AUnknownFields)
      );
    end;
  end;
end;

class function TDataSourceControlProperties.CreateControlProperties(ACollectionItem: TDataSourceControlItem; AGrid: TComponent): TDataSourceControlProperties;
var
  I: Integer;
begin
  I := Length(FClasses) - 1;
  while I >= 0 do
    if AGrid.InheritsFrom(FClasses[I].ControlClass) then
      Break
    else
      Dec(I);
  if(I < 0) then
    Raise Exception.Create('TDataSourceControlProperties.CreateControlProperties failed for class ' + AGrid.ClassName);
    

  Result := FClasses[I].ControlPropertiesClass.Create;
  with Result do try
    FGrid     := AGrid;
    FCollectionItem := ACollectionItem;
    FLinks := TDataSourceControlPropertiesLinks.Create(TDataSourceControlPropertiesLink);

    InternalCreate;
  except
    Free;
    Raise;
  end;
end;

destructor TDataSourceControlProperties.Destroy;
begin
  FreeAndNil(FLinks);
  inherited;
end;

//            if LPopupMenu is TPopupMenu then
//              LEnabled := TPopupMenu(LPopupMenu).IsShortCut(LMessage)
//            else
//            if Supports(LPopupMenu, IcxPopupMenu, AIcxPopupMenu) then
//              LEnabled := AIcxPopupMenu.IsShortCutKey(LMessage);
//            else

function TDataSourceControlProperties.DefaultItemClick(AShortCutReturnTest: Boolean): Boolean;
var
  I: Integer;
  LBaseItemReaction: TBaseItemReaction;
  LPopupMenu: TComponent;
//  AIcxPopupMenu: IcxPopupMenu;
  LMessage: TWMKey;
  LEnabled: Boolean;
begin
  Result := False;
  with TSBaseFrm(Collection.FOwnerForm).ItemsReactions do begin
    //I := Pred(Count);
    for I := 0 to Pred(Count) do begin
      LBaseItemReaction := Items[I];
      if (LBaseItemReaction.LinkedComponent = FGrid)
            and LBaseItemReaction.Enabled
            and LBaseItemReaction.Default
      then begin
        LEnabled := True;

        if AShortCutReturnTest then begin
          LPopupMenu := PopupMenu;
          if LPopupMenu <> nil then begin
            // Проверяем что на VK_RETURN не назначено обработчика
            LMessage.CharCode := VK_RETURN;
            if LPopupMenu is TdxBarCustomPopupMenu then
              LEnabled := TdxBarCustomPopupMenu(LPopupMenu).IsShortCutKey(LMessage);
          end;

          if not LEnabled then
            LEnabled := LBaseItemReaction.Enabled
          else
            LEnabled := False;
        end;

        if LEnabled then
          LBaseItemReaction.OnClickEvent(LBaseItemReaction.Item);

        Result := True;
        Break;
      end;
      //Dec(I);
    end;
  end;
end;

procedure TDataSourceControlProperties.Delete;
var
  LBookmark: TDataSourceBookmark;
begin
  LBookmark := GetBookmark;
  try
    DoDelete;
  finally
    SetBookmark(LBookmark);
  end;
end;

procedure TDataSourceControlProperties.DoUpdate(AUpdateStates: TDataSourceUpdateStates; ADataSource: TComponent);
begin
  if ADataSource = nil then
    ADataSource := FGrid;

  if FActive <> GetActive then begin
    FActive := not FActive;
    AUpdateStates := AUpdateStates + [dsusActive];
  end;

  TSBaseFrm(Collection.FOwnerForm).ItemsReactions.DataSourceOnUpdate(ADataSource, AUpdateStates);
  if (ADataSource = FGrid) and Assigned(FAfterUpdate) then
    FAfterUpdate(FGrid);
end;

function TDataSourceControlProperties.Evaluate(const AExpression: string; AConditionParser: PExprParser): Variant;
var
  LDataSet: TDataSet;
begin
  LDataSet := TargetDataSet;
  if Assigned(LDataSet) then
    Result := LDataSet.Evaluate(AExpression, False)
  else
    Result := unAssigned;
end;

function TDataSourceControlProperties.EvaluateBoolean(const AExpression: string; AConditionParser: PExprParser; ADefaultValue: Boolean): Boolean;
begin
  Result := SBaseVariantFunctions.IsEmpty(Evaluate(AExpression, AConditionParser), ADefaultValue) = True
end;

function TDataSourceControlProperties.FocusedRecodIsData: Boolean;
begin
  Result := (VisibleRecordCount > 0);
end;

function TDataSourceControlProperties.FoldingActive: Boolean;
begin
  Result := False;
end;

procedure TDataSourceControlProperties.FullCollapse;
begin
//
end;

procedure TDataSourceControlProperties.FullExpand;
begin
//
end;

function TDataSourceControlProperties.Collection: TDataSourceControls;
begin
  Result := TDataSourceControls(FCollectionItem.Collection)
end;

function TDataSourceControlProperties.ConfirmRecordAction(const AMessage: String): Boolean;
var
  LKeys       : String;
  LKeysValues : Variant;
begin
  Result := False;

  LKeys       := KeyFieldNames;
  LKeysValues := KeyFieldValues;

  with TControlRect.FindFocus(FGrid) do
    if hsDialogs.HSConfirmMessage(AMessage, Control, Target.X, Target.Y) then
      if VarIsPresent(LKeysValues) and
          (
            (LKeys <> KeyFieldNames)
            or
            (not VarIsEqual(LKeysValues, KeyFieldValues))
          )
      then
        hsDialogs.HSShowError(SRecordChangedWhileConfirming)
      else
        Result := True;
end;

function TDataSourceControlProperties.GetPopupMenu: TComponent;
begin
  Result := nil;
end;

function TDataSourceControlProperties.GetWinControl: TWinControl;
begin
  if FGrid is TWinControl then
    Result := TWinControl(FGrid)
  else
    Result := nil;
end;

function TDataSourceControlProperties.IsDataActive: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := GetDataSet(1);
  Result := Assigned(LDataSet) and LDataSet.Active;
end;

function TDataSourceControlProperties.KeyFieldNames: String;
var
  LDataSet: TDataSet;
begin
  LDataSet := SourceDataSet.Source;
  if Assigned(LDataSet) and (LDataSet is TCustomBaseDataSetProvider) then
    Result := TCustomBaseDataSetProvider(LDataSet).KeyFieldNames
  else
    Result := ''
end;

function TDataSourceControlProperties.KeyFieldValues: Variant;
var
  LDataSet: TDataSet;
begin
  LDataSet := TargetDataSet;
  if Assigned(LDataSet) {and (LDataSet is TCustomBaseDataSetProvider)} then
    Result := LDataSet.FieldValues[KeyFieldNames]
  else
    Result := Unassigned
end;

procedure TDataSourceControlProperties.RefreshRecord;
var
  LDataSet: TDataSet;
  LDataSetProvider: TSQLDataSetProvider absolute LDataSet;
begin
  LDataSet := SourceDataSet.Source;
  Assert(Assigned(LDataSet));
  LDataSetProvider.RefreshRecord;
end;

procedure TDataSourceControlProperties.SetAfterUpdate(const AValue: TNotifyEvent);
begin
  if TMethod(FAfterUpdate) <> TMethod(AValue) then begin
    FAfterUpdate := AValue;
    if Assigned(FAfterUpdate) then
      FAfterUpdate(FGrid);
  end;
end;

procedure TDataSourceControlProperties.SetCopyEnabled(AEnabled: Boolean);
begin
//
end;

procedure TDataSourceControlProperties.SetDeleteEnabled(AEnabled: Boolean);
begin
//
end;

procedure TDataSourceControlProperties.SetEditEnabled(AEnabled: Boolean);
begin
//
end;

procedure TDataSourceControlProperties.SetInsertEnabled(AEnabled: Boolean);
begin
//
end;

function TDataSourceControlProperties.VisibleRecordCount: Integer;
var
  LDataSet: TDataSet;
begin
  LDataSet := TargetDataSet;
  if Assigned(LDataSet) and LDataSet.Active then
    Result := LDataSet.RecordCount
  else
    Result := 0;
end;

{ TDataSourceDefaultUpdateControlProperties }

procedure TDataSourceDefaultUpdateControlProperties.InternalCreate;
begin
  Assert(FGrid.InheritsFrom(TDataSource));
  FDataSetLink := TDataSourceDataLink.Create;
  with FDataSetLink do begin
    DataSource      := Self.DataSource;
    OnActiveChanged := DataSourceActiveChanged;
    OnRecordChanged := DataSourceRecordChanged;
  end;

  inherited;
end;

procedure TDataSourceDefaultUpdateControlProperties.DataSourceActiveChanged(Sender: TObject);
begin
  DoUpdate([dsusActive, dsusFocusedRecord]);
end;

procedure TDataSourceDefaultUpdateControlProperties.DataSourceRecordChanged(Sender: TObject);
begin
  DoUpdate([dsusData, dsusFocusedRecord]);
end;

destructor TDataSourceDefaultUpdateControlProperties.Destroy;
begin
  FreeAndNil(FDataSetLink);
  inherited;
end;

function TDataSourceDefaultUpdateControlProperties.GetBookmark: TDataSourceBookmark;
begin
  Result := DataSource.DataSet.GridBookmark
end;

procedure TDataSourceDefaultUpdateControlProperties.SetBookmark(Value: TDataSourceBookmark);
begin
  DataSource.DataSet.GridBookmark := Value;
end;

function TDataSourceDefaultUpdateControlProperties.GetDataSet(const Index: Integer): TDataSet;
begin
  Result := DataSource.DataSet;
  if (Index = 1) then
    Result := Result.Source;
end;

function TDataSourceDefaultUpdateControlProperties.GetDataSource: TDataSource;
begin
  Result := TDataSource(FGrid)
end;

function TDataSourceDefaultUpdateControlProperties.GetActive: Boolean;
begin
  with DataSource do
    Result := Assigned(DataSet) and DataSet.Active
end;

function TDataSourceDefaultUpdateControlProperties.IsNotEmpty: Boolean;
begin
  with DataSource do
    Result := Assigned(DataSet) and (not DataSet.IsEmpty)
end;

function TDataSourceDefaultUpdateControlProperties.CanModify: Boolean;
begin
  with DataSource do
    Result := Assigned(DataSet) and DataSet.CanModify
end;

function TDataSourceDefaultUpdateControlProperties.Inserted: Boolean;
begin
  with DataSource do
    Result := Assigned(DataSet) and DataSet.Inserted
end;

function TDataSourceDefaultUpdateControlProperties.Editing: Boolean;
begin
  with DataSource do
    Result := Assigned(DataSet) and DataSet.Editing
end;

procedure TDataSourceDefaultUpdateControlProperties.BeginUpdate(var ADataSourceStateBookmark: TDataSourceStateBookmark);
begin
  DataSource.DataSet.BeginUpdate(ADataSourceStateBookmark)
end;

procedure TDataSourceDefaultUpdateControlProperties.EndUpdate(const ADataSourceStateBookmark: TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores);
begin
  DataSource.DataSet.Source.EndUpdate(ADataSourceStateBookmark, ADataSetStateRestore, ADataSourceRestore)
end;

procedure TDataSourceDefaultUpdateControlProperties.InPlaceEdit;
begin
  with DataSource do begin
    Assert(Assigned(DataSet));
    DataSet.Edit;
  end;
end;

procedure TDataSourceDefaultUpdateControlProperties.InPlaceInsert;
begin
  with DataSource do begin
    Assert(Assigned(DataSet));
    DataSet.Insert;
  end;
end;

procedure TDataSourceDefaultUpdateControlProperties.Cancel;
begin
  with DataSource do begin
    Assert(Assigned(DataSet));
    DataSet.Cancel;
  end;
end;

procedure TDataSourceDefaultUpdateControlProperties.Post;
begin
  with DataSource do begin
    Assert(Assigned(DataSet));
    DataSet.Post;
  end;
end;

procedure TDataSourceDefaultUpdateControlProperties.Refresh;
begin
  with DataSource do begin
    Assert(Assigned(DataSet));
    DataSet.Refresh;
  end;
end;

procedure TDataSourceDefaultUpdateControlProperties.DoDelete;
begin
  with DataSource do begin
    Assert(Assigned(DataSet));
    DataSet.Delete;
  end;
end;

procedure TDataSourceDefaultUpdateControlProperties.InPlaceInsertChild(AChildComponent: TComponent);
begin
  Assert(Assigned(AChildComponent) and (AChildComponent is TDataSet));
  with TSQLDataSetProvider(TDataSet(AChildComponent).Source) do begin
    Insert;
    // TODO: Пока только для LocalMasterDetail
    FieldValues[DetailFields] := DataSource.DataSet.FieldValues[MasterFields];
  end;
end;

procedure TDataSourceDefaultUpdateControlProperties.InPlaceSyncEdit;
begin
  //
end;
{$ENDREGION 'TDataSourceControlProperties'}

{$REGION 'TDataSourceCxGridUpdateControlProperties'}

{ TSBaseDataSourceControlsCxGridNotification }

constructor TSBaseDataSourceControlsCxGridNotification.Create(AOwner: TDataSourceCxGridUpdateControlProperties);
begin
  FOwner := AOwner;
end;

function TSBaseDataSourceControlsCxGridNotification.NotificationKinds: TcxGridNotificationKinds;
begin
  Result := [{gnkFocusedViewChanged,} gnkFocusedRecordChanged];
end;

procedure TSBaseDataSourceControlsCxGridNotification.Notify(AKind: TcxGridNotificationKind; AData: TObject; var AHandled: Boolean);
begin
  inherited;

  if AKind = gnkFocusedRecordChanged then
    FOwner.cxGridTableViewFocusedRecordChanged;
end;

{ TDataSourceCxGridUpdateControlProperties }

destructor TDataSourceCxGridUpdateControlProperties.Destroy;
var
//  I: Integer;
  LcxGridFocusedViewChangedEvent: TcxGridFocusedViewChangedEvent;
  LcxGridCellDblClick: TcxGridCellClickEvent;
  LcxGridKeyDown: TKeyEvent;
begin
  if Assigned(FcxGridNotification) then begin
    if not (csDestroying in TcxGrid(FGrid).ComponentState) then
      TcxGrid(FGrid).UnregisterNotification(FcxGridNotification);
    FcxGridNotification.Free;
  end;

  if (FGrid <> nil) and (FGrid.ComponentState = []) then begin
    with TcxMDGrid(FGrid) do begin
      LcxGridFocusedViewChangedEvent := cxGridFocusedTableViewChanged;
      if TMethod(OnFocusedViewChanged) = TMethod(LcxGridFocusedViewChangedEvent) then
        OnFocusedViewChanged := FcxGridFocusedTableViewChanged;

        LcxGridCellDblClick := Self.cxGridCellDblClick;
        if TMethod(OnCellDblClick) = TMethod(LcxGridCellDblClick) then
          OnCellDblClick := FcxGridCellDblClick;

        LcxGridKeyDown := Self.cxGridKeyDown;
        if TMethod(OnKeyDown) = TMethod(LcxGridKeyDown) then
          OnKeyDown := FcxGridKeyDown;
    end;
  end;

  inherited;
end;

procedure TDataSourceCxGridUpdateControlProperties.SetPopupMenu(APopupMenu: TComponent);
begin
  TcxGrid(FGrid).PopupMenu := APopupMenu  
end;

function TDataSourceCxGridUpdateControlProperties.VisibleRecordCount: Integer;
var
  LActiveGridView: TcxGridTableView;
begin
  LActiveGridView := TcxGridTableView(TcxGrid(FGrid).FocusedView.GetActualClone(False));
  if LActiveGridView <> nil then
    Result := LActiveGridView.DataController.FilteredRecordCount
  else
    Result := 0;
end;

function TDataSourceCxGridUpdateControlProperties.FocusedRecodIsData: Boolean;
var
  LActiveGridView: TcxGridTableView;
  LFocusedRecord: TcxCustomGridRecord;
begin
  Result := False;
  LActiveGridView := TcxGridTableView(TcxGrid(FGrid).FocusedView.GetActualClone(False));
  if LActiveGridView <> nil then begin
    LFocusedRecord := LActiveGridView.Controller.FocusedRecord;
    Result := LActiveGridView.Focused and Assigned(LFocusedRecord) and (LFocusedRecord is TcxGridDataRow);
  end;
end;

procedure TDataSourceCxGridUpdateControlProperties.InternalCreate;
begin
  Assert(FGrid.InheritsFrom(TcxGrid));

  inherited;

  FcxGridNotification := TSBaseDataSourceControlsCxGridNotification.Create(Self);
  TcxGrid(FGrid).RegisterNotification(FcxGridNotification);

  with TcxMDGrid(FGrid) do begin
    FcxGridFocusedTableViewChanged := OnFocusedViewChanged;
    OnFocusedViewChanged := cxGridFocusedTableViewChanged;

    FcxGridCellDblClick := OnCellDblClick;
    OnCellDblClick := cxGridCellDblClick;

    FcxGridKeyDown := OnKeyDown;
    OnKeyDown := cxGridKeyDown;
  end;
end;

procedure TDataSourceCxGridUpdateControlProperties.cxGridFocusedTableViewChanged(Sender: TcxCustomGrid; APrevFocusedView, AFocusedView: TcxCustomGridView);
var
  LPrevFocusedView: TcxCustomGridTableView absolute APrevFocusedView;
  LFocusedView: TcxCustomGridTableView absolute AFocusedView;
begin
  while (LFocusedView <> nil) and (LPrevFocusedView <> nil) and (LFocusedView.PatternGridView <> LPrevFocusedView.PatternGridView) do begin
    if (LPrevFocusedView.Level <> nil) and (TcxGridLevel(LFocusedView.Level).Level < TcxGridLevel(LPrevFocusedView.Level).Level) then begin
      DoUpdate([dsusActive, dsusData, dsusFocusedRecord], LPrevFocusedView.PatternGridView);
      DoUpdate([dsusFocusedRecord], LFocusedView.PatternGridView); // ??

      LPrevFocusedView := TcxCustomGridTableView(LPrevFocusedView.MasterGridView);
    end else begin
      DoUpdate([dsusFocusedRecord], LPrevFocusedView.PatternGridView); // ??
      DoUpdate([dsusActive, dsusData], LFocusedView.PatternGridView);
      DoUpdate([dsusFocusedRecord], LFocusedView.PatternGridView);

      LFocusedView := TcxCustomGridTableView(LFocusedView.MasterGridView);
    end;
  end;

  if Assigned(FAfterUpdate) then
    FAfterUpdate(FGrid);

  if Assigned(FcxGridFocusedTableViewChanged) then
    FcxGridFocusedTableViewChanged(Sender, APrevFocusedView, AFocusedView);
end;

procedure TDataSourceCxGridUpdateControlProperties.cxGridTableViewDataControllerDataChanged(Sender: TObject);
var
  LcxGridTableView: TcxGridTableView;
begin
  InPlaceSyncEdit;

  LcxGridTableView := TcxGridTableView(TcxDBDataController(Sender).GetOwner);
  if TcxGrid(FGrid).FocusedView = LcxGridTableView then
    DoUpdate([dsusData, dsusFocusedRecord]);
end;

procedure TDataSourceCxGridUpdateControlProperties.cxGridTableViewFocusedRecordChanged;
begin
  DoUpdate([dsusFocusedRecord]);
end;

procedure TDataSourceCxGridUpdateControlProperties.cxGridCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  AHandled := DefaultItemClick(True);
end;

procedure TDataSourceCxGridUpdateControlProperties.cxGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then begin
    if Editing then begin
      Post;
      Exit;
    end else if ([ssShift, ssAlt, ssCtrl] * Shift = []) then begin
      //if DefaultItemClick(False) then Exit;
      DefaultItemClick(False);
    end;
  end;
end;

function TDataSourceCxGridUpdateControlProperties.GetBookmark: TDataSourceBookmark;
var
  LcxCustomGridView: TcxCustomGridView;
begin
  LcxCustomGridView := TCxGrid(FGrid).FocusedView;
  if (LcxCustomGridView is TcxGridTableView) then
    Result := TcxGridTableView(LcxCustomGridView).GridBookmark
  else
    Result := TDataSourceBookmark.Create(True);
end;

procedure TDataSourceCxGridUpdateControlProperties.SetBookmark(Value: TDataSourceBookmark);
var
  LcxCustomGridView: TcxCustomGridView;
begin
  LcxCustomGridView := TCxGrid(FGrid).FocusedView;
  if (LcxCustomGridView is TcxGridTableView) then
    TcxGridTableView(LcxCustomGridView).GridBookmark := Value;
    // GridReOpen(TcxGridTableView(LcxCustomGridView), Value, False)
end;

function TDataSourceCxGridUpdateControlProperties.GetDataController: TcxCustomDataController;
var
  LcxCustomGridView: TcxCustomGridView;
begin
  LcxCustomGridView := TCxGrid(FGrid).FocusedView;
  if (LcxCustomGridView is TcxGridTableView) then
    Result := TcxGridTableView(LcxCustomGridView).DataController
  else
    Result := nil;
end;

function TDataSourceCxGridUpdateControlProperties.GetDataSource: TDataSource;
var
  LDataController: TcxCustomDataController;
begin
  LDataController := GetDataController;
  if Assigned(LDataController) and (LDataController is TcxGridDBDataController) then
    Result := TcxGridDBDataController(LDataController).DataSource
  else
    Result := nil;
end;

function TDataSourceCxGridUpdateControlProperties.GetDataSet(const Index: Integer): TDataSet;
var
  LDataController: TcxCustomDataController;
begin
  Result := nil;

  LDataController := GetDataController;
  if Assigned(LDataController) and (LDataController is TcxGridDBDataController) then
    if Index = 0 then
      Result := TcxGridDBDataController(LDataController).DataSet
    else if Assigned(TcxGridDBDataController(LDataController).DataSource) then
      Result := TcxGridDBDataController(LDataController).DataSource.DataSet.Source
end;

function TDataSourceCxGridUpdateControlProperties.GetFormDataSourceParamAndFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant;
var
  LUnknownParams: String;
  LActiveClone: TcxGridTableView;
begin
  LActiveClone := TcxGridTableView(TcxGridTableView(TCxGrid(FGrid).FocusedView).GetActualClone(False));
  Assert(Assigned(LActiveClone));
  Result := LActiveClone.GetFocusedRecordValues(AFieldNames, @AUnknownFields);
  // Result := TcxGridTableView(TCxGrid(FGrid).FocusedView).GetFocusedRecordValues(AFieldNames, AUnknownFields);

  if not AUnknownFields.IsEmpty then begin
    LUnknownParams := AUnknownFields;
    VarArrayOverWrite
    (
      AFieldNames,
      Result,
      LUnknownParams,
      GetFormDataSourceParamValues(LUnknownParams, AUnknownFields)
    );
  end;
end;

function TDataSourceCxGridUpdateControlProperties.GetActive: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := GetDataSet(1);
  Result := Assigned(LDataSet) and LDataSet.Active
end;

function TDataSourceCxGridUpdateControlProperties.IsNotEmpty: Boolean;
begin
  Result := (VisibleRecordCount > 0)
end;

function TDataSourceCxGridUpdateControlProperties.GetPopupMenu: TComponent;
begin
  Result := nil;

  if (FGrid <> nil) then
    if (TcxGrid(FGrid).FocusedView <> nil) then begin
      Result := TcxGrid(FGrid).FocusedView.PopupMenu;
      if Result = nil then
        Result := TcxGrid(FGrid).PopupMenu
    end
end;

procedure TDataSourceCxGridUpdateControlProperties.Refresh;
begin
  TcxGridTableView(TcxGrid(FGrid).FocusedView).RefreshDataSet;
end;

procedure TDataSourceCxGridUpdateControlProperties.DoDelete;
begin
  with TcxGridTableViewCrack(TcxGrid(FGrid).FocusedView.GetActualClone) do
    if GetDataControllerClass.InheritsFrom(TcxGridDBDataController) then
      TcxGridDBDataController(DataController).DeleteFocusedRow
    else
      DataController.DeleteFocused
end;

procedure TDataSourceCxGridUpdateControlProperties.InPlaceInsert;
begin
  TcxGridTableView(TcxGrid(FGrid).FocusedView).DataController.Append;
  TcxGridTableView(TcxGrid(FGrid).FocusedView).Controller.EditingController.ShowEdit;
  InPlaceSyncEdit;
end;

procedure TDataSourceCxGridUpdateControlProperties.InPlaceInsertChild(AChildComponent: TComponent);
begin
  with TcxGridTableView(TcxGridTableView(AChildComponent).GetActualClone) do begin
    DataController.Append;
    Controller.EditingController.ShowEdit;
  end;
  Collection.FindOrCreateGridControl(AChildComponent).InPlaceSyncEdit;
end;

procedure TDataSourceCxGridUpdateControlProperties.InPlaceEdit;
begin
  with TcxGridTableView(TcxGrid(FGrid).FocusedView.GetActualClone) do begin
    DataController.Edit;
    Controller.EditingController.ShowEdit;
  end;
  InPlaceSyncEdit;
end;

procedure TDataSourceCxGridUpdateControlProperties.BeginUpdate(var ADataSourceStateBookmark: TDataSourceStateBookmark);
begin
  TcxGridTableView(TcxGrid(FGrid).FocusedView).BeginUpdate(ADataSourceStateBookmark)
end;

procedure TDataSourceCxGridUpdateControlProperties.EndUpdate(const ADataSourceStateBookmark: TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores);
begin
  TcxGridTableView(TcxGrid(FGrid).FocusedView).EndUpdate(ADataSourceStateBookmark, ADataSetStateRestore, ADataSourceRestore)
end;

function TDataSourceCxGridUpdateControlProperties.Evaluate(const AExpression: string; AConditionParser: PExprParser): Variant;
var
  LcxGridTableView: TcxGridTableView;
  LcxCustomGridRow: TcxCustomGridRow;
begin
  LcxGridTableView := TcxGridTableView(TcxGridTableView(TcxGrid(FGrid).FocusedView).GetActualClone);
  if Assigned(LcxGridTableView) then begin
    if TcxGridTableViewCrack(LcxGridTableView).GetDataControllerClass.InheritsFrom(TcxDBDataController)
        and TcxDBDataController(TcxGridTableView(LcxGridTableView).DataController).DataModeController.SyncMode
    then
      Result := inherited Evaluate(AExpression, AConditionParser)
    else begin
      LcxCustomGridRow := LcxGridTableView.Controller.FocusedRow;
      if Assigned(LcxCustomGridRow) and (LcxCustomGridRow is TcxGridDataRow) then
        Exit(CxGridDataRowExpression(TcxGridDataRow(LcxCustomGridRow), AExpression, AConditionParser));
    end;
  end;
  Result := unAssigned;
end;

function TDataSourceCxGridUpdateControlProperties.CanAssignPopupMenu: TPopupMenuKinds;
begin
  Result := [pmClassic, pmDxBar]
end;

procedure TDataSourceCxGridUpdateControlProperties.Cancel;
begin
  TcxGridTableView(TcxGrid(FGrid).FocusedView).DataController.Cancel
end;

function TDataSourceCxGridUpdateControlProperties.CanModify: Boolean;
begin
  Result := TcxCustomDataProviderCrack
            (
              TcxCustomDataControllerCrack
              (
                TcxGridTableView
                (
                  TcxGrid(FGrid).FocusedView
                ).DataController
              ).Provider
            ).CanModify
end;

procedure TDataSourceCxGridUpdateControlProperties.Post;
begin
  TcxGridTableView(TcxGrid(FGrid).FocusedView).DataController.Post
end;

procedure TDataSourceCxGridUpdateControlProperties.InPlaceSyncEdit;
var
  LEditing: Boolean;
  LcxGridTableView: TcxGridTableView;
begin
  LcxGridTableView := TcxGridTableView(TcxGrid(FGrid).FocusedView.GetActualClone);
  if LcxGridTableView = nil then Exit;
  LEditing := Editing;
  with LcxGridTableView do begin
    OptionsBehavior.ImmediateEditor   := LEditing;
    OptionsBehavior.AlwaysShowEditor  := LEditing;
    OptionsData.Editing               := LEditing;
  end;
end;

function TDataSourceCxGridUpdateControlProperties.Editing: Boolean;
begin
  Result := TcxGridTableView(TcxGrid(FGrid).FocusedView).DataController.EditState * [dceInsert, dceEdit] <> [];
end;

function TDataSourceCxGridUpdateControlProperties.Inserted: Boolean;
begin
  Result := dceInsert in TcxGridTableView(TcxGrid(FGrid).FocusedView).DataController.EditState;
end;

function TDataSourceCxGridUpdateControlProperties.KeyFieldNames: String;
begin
  if (TcxGrid(FGrid).FocusedView <> nil) and (TcxGrid(FGrid).FocusedView is TcxGridDBTableView) then begin
    Result := TcxGridDBTableView(TcxGrid(FGrid).FocusedView).DataController.KeyFieldNames;
    if Result = '' then
      Result := (inherited KeyFieldNames);
  end else
    Result := unAssigned;
end;

function TDataSourceCxGridUpdateControlProperties.KeyFieldValues: Variant;
begin
  Result := unAssigned;

  if (TcxGrid(FGrid).FocusedView <> nil) and (TcxGrid(FGrid).FocusedView is TcxGridDBTableView) then
    with TcxGridDBTableView(TcxGrid(FGrid).FocusedView).DataController do
      if (FocusedRecordIndex >= 0) then
        if (KeyFieldNames <> '') then
          Result := GetRecordId(FocusedRecordIndex)
        else
          Result := (inherited KeyFieldValues)
end;
{$ENDREGION 'TDataSourceCxGridUpdateControlProperties'}

{$REGION 'TDataSourceCxGridTableViewUpdateControlProperties'}
{ TDataSourceCxGridTableViewUpdateControlProperties }

destructor TDataSourceCxGridTableViewUpdateControlProperties.Destroy;
begin
  ReleaseEvents;

  inherited;
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.ReleaseEvents;
var
  LcxGridTableViewDataControllerDataChanged: TNotifyEvent;
  LcxGridTableViewCellDblClick: TcxGridCellClickEvent;
  LcxGridTableViewKeyDown: TKeyEvent;
  LcxGridTableViewEditKeyDown: TcxGridEditKeyEvent;
begin
  if (FGrid <> nil) and (FGrid.ComponentState = []) then
    with TcxGridTableView(FGrid) do begin
      LcxGridTableViewDataControllerDataChanged := cxGridTableViewDataControllerDataChanged;
      TcxDBDataController(DataController).RemoveDataChangedListener(Self, cxGridTableViewDataControllerDataChanged);

      if FGrid is TcxMDGridDBTableView then
        TcxMDGridDBTableView(FGrid).OnFocusedRecordChangedEx := nil
      else if FGrid is TcxMDGridDBBandedTableView then
        TcxMDGridDBBandedTableView(FGrid).OnFocusedRecordChangedEx := nil;

      LcxGridTableViewCellDblClick := cxGridTableViewCellDblClick;
      if TMethod(OnCellDblClick) = TMethod(LcxGridTableViewCellDblClick) then
        OnCellDblClick := FcxGridTableViewCellDblClick;

      LcxGridTableViewKeyDown := cxGridTableViewKeyDown;
      if TMethod(OnKeyDown) = TMethod(LcxGridTableViewKeyDown) then
        OnKeyDown := FcxGridTableViewKeyDown;

      LcxGridTableViewEditKeyDown := cxGridTableViewEditKeyDown;
      if TMethod(OnEditKeyDown) = TMethod(LcxGridTableViewEditKeyDown) then
        OnEditKeyDown := FcxGridTableViewEditKeyDown;
    end;
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.cxGridTableViewDataControllerDataChanged(Sender: TObject);
var
  LcxGridTableView: TcxGridTableView;
begin
  InPlaceSyncEdit;

  LcxGridTableView := TcxGridTableView(TcxDBDataController(Sender).GetOwner);

  if LcxGridTableView.Focused then
    DoUpdate([dsusData, dsusFocusedRecord]);
end;


procedure TDataSourceCxGridTableViewUpdateControlProperties.cxGridTableViewFocusedRecordChanged(Sender: TObject);
begin
  InPlaceSyncEdit;
  DoUpdate([dsusFocusedRecord]);
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.InternalCreate;
//var
//  LcxGrid: TcxGrid;
//  LcxDsUcProp: TDataSourceCxGridUpdateControlProperties;
begin
  Assert(FGrid.InheritsFrom(TcxGridTableView));
  inherited;

//  LcxGrid := TcxGrid(TcxGridTableView(FGrid).GetParentComponent);
//  LcxDsUcProp := TDataSourceCxGridUpdateControlProperties(Collection.FindGridControl(LcxGrid));
//  if LcxDsUcProp = nil then
//    TDataSourceControlItem(Collection.Add).SetGrid(LcxGrid);

//  if (LcxDsUcProp = nil) then begin

  if FGrid is TcxMDGridDBTableView then
    TcxMDGridDBTableView(FGrid).OnFocusedRecordChangedEx := cxGridTableViewFocusedRecordChanged
  else if FGrid is TcxMDGridDBBandedTableView then
    TcxMDGridDBBandedTableView(FGrid).OnFocusedRecordChangedEx := cxGridTableViewFocusedRecordChanged;

  with TcxGridTableView(FGrid) do begin
    TcxDBDataController(DataController).AddDataChangedListener(Self, cxGridTableViewDataControllerDataChanged);

    FcxGridTableViewCellDblClick  := OnCellDblClick;
    OnCellDblClick                := cxGridTableViewCellDblClick;

    FcxGridTableViewKeyDown       := OnKeyDown;
    OnKeyDown                     := cxGridTableViewKeyDown;

    FcxGridTableViewEditKeyDown   := OnEditKeyDown;
    OnEditKeyDown                 := cxGridTableViewEditKeyDown;
  end

//  end;
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.cxGridTableViewCellDblClick(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
begin
  AHandled := DefaultItemClick; //TODO: Не знаю правильно ли

  if (not AHandled) and Assigned(FcxGridTableViewCellDblClick) then
    FcxGridTableViewCellDblClick(Sender, ACellViewInfo, AButton, AShift, AHandled);
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.cxGridTableViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then begin
    if Editing then begin
      Post;
      Exit;
    end else if ([ssShift, ssAlt, ssCtrl] * Shift = []) then begin
      if DefaultItemClick(False) then Exit;
    end;
  end;

  if Assigned(FcxGridTableViewKeyDown) then
    FcxGridTableViewKeyDown(Sender, Key, Shift);
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.cxGridTableViewEditKeyDown(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem;
  AEdit: TcxCustomEdit; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and Editing then begin
    Post;
    Key := 0;
  end else if Assigned(FcxGridTableViewEditKeyDown) then
    FcxGridTableViewEditKeyDown(Sender, AItem, AEdit, Key, Shift);
end;

function TDataSourceCxGridTableViewUpdateControlProperties.GetBookmark: TDataSourceBookmark;
begin
  Result := TcxGridTableView(FGrid).GridBookmark
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.SetBookmark(Value: TDataSourceBookmark);
begin
  TcxGridTableView(FGrid).GridBookmark := Value
end;

function TDataSourceCxGridTableViewUpdateControlProperties.VisibleRecordCount: Integer;
var
  LActiveGridView: TcxGridTableView;
begin
  LActiveGridView := TcxGridTableView(TcxGridTableView(FGrid).GetActualClone(False));
  if LActiveGridView <> nil then
    Result := LActiveGridView.DataController.FilteredRecordCount
  else
    Result := 0;
end;

function TDataSourceCxGridTableViewUpdateControlProperties.FocusedRecodIsData: Boolean;
var
  LActiveGridView: TcxGridTableView;
  LFocusedRecord: TcxCustomGridRecord;
begin
  Result := False;
  LActiveGridView := TcxGridTableView(TcxGridTableView(FGrid).GetActualClone(False));
  if LActiveGridView <> nil then begin
    LFocusedRecord := LActiveGridView.Controller.FocusedRecord;
    Result := LActiveGridView.Focused and Assigned(LFocusedRecord) and (LFocusedRecord is TcxGridDataRow);
  end;
end;

function TDataSourceCxGridTableViewUpdateControlProperties.GetIsGrouping: Boolean;
begin
  Result := TcxGridTableView(FGrid).OptionsCustomize.ColumnGrouping
        //and (TcxGridTableView(FGrid).DataController.RecordCount > 0)
        and (TcxGridTableView(FGrid).DataController.Groups.GroupingItemCount > 0)
        and not (dcoGroupsAlwaysExpanded in TcxGridTableView(FGrid).DataController.Options)
end;

function TDataSourceCxGridTableViewUpdateControlProperties.GetNoDataToDisplayInfoText: String;
begin
  Result := TcxGridTableView(FGrid).OptionsView.NoDataToDisplayInfoText
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.SetNoDataToDisplayInfoText(const AValue: String);
begin
  TcxGridTableView(FGrid).OptionsView.NoDataToDisplayInfoText := AValue;
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.SetPopupMenu(APopupMenu: TComponent);
begin
  TcxGridTableView(FGrid).PopupMenu := APopupMenu;
end;

function TDataSourceCxGridTableViewUpdateControlProperties.SupportFolding: Boolean;
begin
  Result := TcxGridTableView(FGrid).OptionsCustomize.ColumnGrouping
end;

function TDataSourceCxGridTableViewUpdateControlProperties.FoldingActive: Boolean;
begin
  Result := GetIsGrouping or TcxGridTableView(FGrid).IsMaster
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.FullCollapse;
var
  LActiveGridView: TcxGridTableView;
begin
  if TcxGridTableView(FGrid).IsMaster then
    TcxGridTableView(FGrid).ViewData.Collapse(True);

  if GetIsGrouping then begin
    LActiveGridView := TcxGridTableView(TcxGridTableView(FGrid).GetActualClone(False));
    if LActiveGridView <> nil then
      LActiveGridView.DataController.Groups.FullCollapse;
  end;
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.FullExpand;
var
  LActiveGridView: TcxGridTableView;
begin
  if TcxGridTableView(FGrid).IsMaster then
    TcxGridTableView(FGrid).ViewData.Expand(True);

  if GetIsGrouping then begin
    LActiveGridView := TcxGridTableView(TcxGridTableView(FGrid).GetActualClone(False));
    if LActiveGridView <> nil then
      LActiveGridView.DataController.Groups.FullExpand;
  end;
end;

function TDataSourceCxGridTableViewUpdateControlProperties.GetDataController: TcxCustomDataController;
begin
  Result := TcxGridTableView(FGrid).DataController;
end;

function TDataSourceCxGridTableViewUpdateControlProperties.GetDataSource: TDataSource;
begin
  if (GetDataController is TcxGridDBDataController) then
    Result := TcxGridDBDataController(GetDataController).DataSource
  else
    Result := nil;
end;

function TDataSourceCxGridTableViewUpdateControlProperties.GetDataSet(const Index: Integer): TDataSet;
var
  LFocusedView: TcxCustomGridView;
  LcxCustomDataController: TcxCustomDataController;
begin
  Result := nil;

  LcxCustomDataController := GetDataController;
  if (LcxCustomDataController is TcxGridDBDataController) then
    if Index = 0 then begin
      LFocusedView := TcxCustomGridView(FGrid).GetActualClone(False);
      if LFocusedView <> nil then
        Result := TcxGridDBDataController(LFocusedView.DataController).DataSet;
    end else with TcxGridDBDataController(LcxCustomDataController) do
      if Assigned(DataSource) then
        Result := DataSource.DataSet.Source
end;

function TDataSourceCxGridTableViewUpdateControlProperties.GetFormDataSourceParamAndFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant;
var
  LUnknownParams: String;
  LActiveClone: TcxGridTableView;
begin
  LActiveClone := TcxGridTableView(TcxGridTableView(FGrid).GetActualClone(False));
  Assert(Assigned(LActiveClone));
  Result := LActiveClone.GetFocusedRecordValues(AFieldNames, @AUnknownFields);
//  Result := TcxGridTableView(FGrid).GetFocusedRecordValues(AFieldNames, AUnknownFields);

  if (not AUnknownFields.IsEmpty) then begin
    LUnknownParams := AUnknownFields;
    VarArrayOverWrite
    (
      AFieldNames,
      Result,
      LUnknownParams,
      GetFormDataSourceParamValues(LUnknownParams, AUnknownFields)
    );
  end;
end;

function TDataSourceCxGridTableViewUpdateControlProperties.GetActive: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := GetDataSet(1);
  Result := Assigned(LDataSet) and LDataSet.Active
end;

function TDataSourceCxGridTableViewUpdateControlProperties.IsNotEmpty: Boolean;
begin
  Result := (VisibleRecordCount > 0)
end;

function TDataSourceCxGridTableViewUpdateControlProperties.GetPopupMenu: TComponent;
begin
  if (FGrid <> nil) then
    Result := TcxGridTableView(FGrid).PopupMenu
  else
    Result := nil
end;

function TDataSourceCxGridTableViewUpdateControlProperties.GetWinControl: TWinControl;
begin
  Result := TcxGridTableView(FGrid).Site;
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.Refresh;
begin
  TcxGridTableView(FGrid).RefreshDataSet;
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.InPlaceInsert;
var
  LcxCustomGridRecord: TcxCustomGridRecord;
begin
  with TcxGridTableView(TcxGridTableView(FGrid).GetActualClone) do begin
    LcxCustomGridRecord := MasterGridRecord;
    if LcxCustomGridRecord <> nil then
      MasterGridRecord.Expanded := True;
    // DataController.Append;
    Controller.CreateNewRecord(True);
    Controller.EditingController.ShowEdit;
    Focused := True;
  end;
  InPlaceSyncEdit;
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.InPlaceInsertChild(AChildComponent: TComponent);
var
  LIsDBDataController: Boolean;
  LGrid: TcxGridTableView;
begin
  Assert
  (
    Assigned(AChildComponent)
    and (FGrid <> AChildComponent)
    and (TcxGridTableView(AChildComponent).MasterGridView = TcxCustomGridView(FGrid))
  );
  LIsDBDataController := (TcxGridTableView(FGrid).DataController.InheritsFrom(TcxGridDBDataController));
  Assert(LIsDBDataController = (TcxGridTableView(AChildComponent).DataController.InheritsFrom(TcxGridDBDataController)));
  if LIsDBDataController then
    Assert(TcxGridDBDataController(TcxGridTableView(FGrid).DataController).DataModeController.SyncMode and TcxGridDBDataController(TcxGridTableView(AChildComponent).DataController).DataModeController.SyncMode, 'Sync mode');

  LGrid := TcxGridTableView(TcxGridTableView(FGrid).GetActualClone);
  TcxCustomGridRecordCrack(LGrid.Controller.FocusedRecord).DoExpand(False);

  with TcxCustomGridTableController(LGrid.Controller.FocusedRow.AsMasterDataRow.ActiveDetailGridView.Controller) do begin
    CreateNewRecord(True);
    EditingController.ShowEdit;
  end;

  Collection.FindGridControl(AChildComponent).InPlaceSyncEdit;
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.InPlaceEdit;
begin
  with TcxGridTableView(TcxGridTableView(FGrid).GetActualClone) do begin
    DataController.Edit;
    Controller.EditingController.ShowEdit;
  end;
  InPlaceSyncEdit;
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.BeginUpdate(var ADataSourceStateBookmark: TDataSourceStateBookmark);
begin
  TcxGridTableView(FGrid).BeginUpdate(ADataSourceStateBookmark)
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.EndUpdate(const ADataSourceStateBookmark: TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores);
begin
  TcxGridTableView(FGrid).EndUpdate(ADataSourceStateBookmark, ADataSetStateRestore, ADataSourceRestore)
end;

function TDataSourceCxGridTableViewUpdateControlProperties.Evaluate(const AExpression: string; AConditionParser: PExprParser): Variant;
var
  LcxCustomGridRow: TcxCustomGridRow;
begin
  if (TcxGridTableViewCrack(FGrid).GetDataControllerClass.InheritsFrom(TcxGridDBDataController)) and TcxGridDBDataController(TcxGridTableView(FGrid).DataController).DataModeController.SyncMode then
    Result := inherited Evaluate(AExpression, AConditionParser)
  else begin
    LcxCustomGridRow := TcxGridTableView(FGrid).Controller.FocusedRow;
    if Assigned(LcxCustomGridRow) and (LcxCustomGridRow is TcxGridDataRow) then
      Exit(CxGridDataRowExpression(TcxGridDataRow(LcxCustomGridRow), AExpression, AConditionParser));
    Result := unAssigned;
  end;
end;

function TDataSourceCxGridTableViewUpdateControlProperties.CanAssignPopupMenu: TPopupMenuKinds;
begin
  Result := [pmClassic, pmDxBar]
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.Cancel;
begin
  TcxGridTableView(FGrid).DataController.Cancel;
end;

function TDataSourceCxGridTableViewUpdateControlProperties.CanModify: Boolean;
begin
  Result := TcxCustomDataProviderCrack(TcxCustomDataControllerCrack(TcxGridTableView(FGrid).DataController).Provider).CanModify
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.Post;
begin
  TcxGridTableView(FGrid).DataController.Post;
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.DoDelete;
begin
//  TcxGridTableView(TcxGridTableView(FGrid).GetActualClone).DataController.DeleteFocused
  with TcxGridTableViewCrack(TcxGridTableView(FGrid).GetActualClone) do
    if GetDataControllerClass.InheritsFrom(TcxGridDBDataController) then
      TcxGridDBDataController(DataController).DeleteFocusedRow
    else
      DataController.DeleteFocused
end;

procedure TDataSourceCxGridTableViewUpdateControlProperties.InPlaceSyncEdit;
var
  LEditing: Boolean;
  LcxGridTableView: TcxGridTableView;
begin
  LcxGridTableView := TcxGridTableView(TcxGridTableView(FGrid).GetActualClone(False));
  if LcxGridTableView = nil then Exit;
  LEditing := Editing;
  with LcxGridTableView do begin
    OptionsBehavior.ImmediateEditor   := LEditing;
    OptionsBehavior.AlwaysShowEditor  := LEditing;
    OptionsData.Editing               := LEditing;
  end;
end;

function TDataSourceCxGridTableViewUpdateControlProperties.Editing: Boolean;
begin
  Result := TcxGridTableView(FGrid).DataController.EditState * [dceInsert, dceEdit] <> [];
end;

function TDataSourceCxGridTableViewUpdateControlProperties.Inserted: Boolean;
begin
  Result := dceInsert in TcxGridTableView(FGrid).DataController.EditState;
end;

function TDataSourceCxGridTableViewUpdateControlProperties.KeyFieldNames: String;
{
var
  LActiveCxView: TcxCustomGridView;
}
begin
{
  LActiveCxView := TcxCustomGridView(FGrid).GetActualClone(False);

  if (LActiveCxView <> nil) and
                            (
                              (LActiveCxView is TcxGridDBTableView)
                              or
                              (LActiveCxView is TcxGridDBBandedTableView)
                            )
  then begin
    Result := TcxGridDBDataController(TcxGridTableView(LActiveCxView).DataController).KeyFieldNames;
    if Result = '' then
      Result := (inherited KeyFieldNames)
  end else
    Result := '';
}

  if (
        (FGrid is TcxGridDBTableView)
        or
        (FGrid is TcxGridDBBandedTableView)
      )
  then
    Result := TcxGridDBDataController(TcxGridTableView(FGrid).DataController).KeyFieldNames;
    if Result = '' then
      Result := (inherited KeyFieldNames)
end;

function TDataSourceCxGridTableViewUpdateControlProperties.KeyFieldValues: Variant;
var
  LActiveCxView: TcxCustomGridView;
begin
  Result := unAssigned;

  LActiveCxView := TcxCustomGridView(FGrid).GetActualClone(False);
  if (LActiveCxView <> nil) and
                            (
                              (LActiveCxView is TcxGridDBTableView)
                              or
                              (LActiveCxView is TcxGridDBBandedTableView)
                            )
  then
    with TcxGridDBDataController(TcxGridTableView(LActiveCxView).DataController) do
      if (FocusedRecordIndex >= 0) and (KeyFieldNames <> '') then
        Result := GetRecordId(FocusedRecordIndex)
end;
{$ENDREGION 'TDataSourceCxGridTableViewUpdateControlProperties'}

{$REGION 'TDataSourceCxMDDBTreeListUpdateControlProperties'}
{ TDataSourceCxMDDBTreeListUpdateControlProperties }

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.InternalCreate;
begin
  Assert(FGrid.InheritsFrom(TcxMDDBTreeList));
  inherited;

  with TcxDBTreeList(FGrid) do begin
    FAlwaysShowEditor               := OptionsBehavior.AlwaysShowEditor;

//    FcxDBTreeListDataChanged        := OnDataChanged;
//    OnDataChanged                   := cxDBTreeListDataChanged;
    FcxDBTreeListDataChanged        := OnLayoutChanged;
    OnLayoutChanged                 := cxDBTreeListDataChanged;

    FcxDBTreeListFocusedNodeChanged := OnFocusedNodeChanged;
    OnFocusedNodeChanged            := cxDBTreeListFocusedNodeChanged;

    FcxDBTreeListDblClick           := OnDblClick;
    OnDblClick                      := cxDBTreeListDblClick;

    FcxDBTreeListKeyDown            := OnKeyDown;
    OnKeyDown                       := cxDBTreeListKeyDown;
  end;
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.cxDBTreeListDataChanged(Sender: TObject);
begin
  InPlaceSyncEdit;

  DoUpdate([dsusData, dsusFocusedRecord]);
  if Assigned(FcxDBTreeListDataChanged) then
    FcxDBTreeListDataChanged(Sender);
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.cxDBTreeListFocusedNodeChanged(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
begin
  if Assigned(AFocusedNode) then begin
    InPlaceSyncEdit;
    DoUpdate([dsusFocusedRecord]);
  end;

  if Assigned(FcxDBTreeListFocusedNodeChanged) then
    FcxDBTreeListFocusedNodeChanged(Sender, APrevFocusedNode, AFocusedNode);
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.cxDBTreeListDblClick(Sender: TObject);
begin
  with TcxDBTreeList(Sender).HitTest do
    if HitAtColumn and (not HitAtColumnHeader) and (not DefaultItemClick) and Assigned(FcxDBTreeListDblClick) then
      FcxDBTreeListDblClick(Sender);
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.cxDBTreeListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then begin
    if Editing then begin
      Post;
      Exit;
    end else if ([ssShift, ssAlt, ssCtrl] * Shift = []) then begin
      if DefaultItemClick(False) then Exit;
    end;
  end;

  if Assigned(FcxDBTreeListKeyDown) then
    FcxDBTreeListKeyDown(Sender, Key, Shift);
end;

destructor TDataSourceCxMDDBTreeListUpdateControlProperties.Destroy;
var
  LcxDBTreeListDataChanged: TNotifyEvent;
  LcxDBTreeListFocusedNodeChanged: TcxTreeListFocusedNodeChangedEvent;
  LcxDBTreeListDblClick: TNotifyEvent;
  LcxDBTreeListKeyDown: TKeyEvent;
begin
  if (FGrid <> nil) and (FGrid.ComponentState = []) then
    with TcxDBTreeList(FGrid) do begin
      LcxDBTreeListDataChanged := DataController.OnDataChanged;
      if TMethod(DataController.OnDataChanged) = TMethod(LcxDBTreeListDataChanged) then
        DataController.OnDataChanged := FcxDBTreeListDataChanged;

      LcxDBTreeListFocusedNodeChanged := cxDBTreeListFocusedNodeChanged;
      if TMethod(OnFocusedNodeChanged) = TMethod(LcxDBTreeListFocusedNodeChanged) then
        OnFocusedNodeChanged := FcxDBTreeListFocusedNodeChanged;

      LcxDBTreeListDblClick := cxDBTreeListDblClick;
      if TMethod(OnDblClick) = TMethod(LcxDBTreeListDblClick) then
        OnDblClick := FcxDBTreeListDblClick;

      LcxDBTreeListKeyDown := cxDBTreeListKeyDown;
      if TMethod(OnKeyDown) = TMethod(LcxDBTreeListKeyDown) then
        OnKeyDown := FcxDBTreeListKeyDown;
    end;

  inherited;
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.GetBookmark: TDataSourceBookmark;
begin
  Result := TcxDBTreeList(FGrid).GridBookmark
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.SetBookmark(Value: TDataSourceBookmark);
begin
  TcxDBTreeList(FGrid).GridBookmark := Value
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.SetPopupMenu(APopupMenu: TComponent);
begin
  TcxDBTreeList(FGrid).PopupMenu := APopupMenu;
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.GetDataSource: TDataSource;
begin
  Result := TcxDBTreeList(FGrid).DataController.DataSource;
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.GetDataSet(const Index: Integer): TDataSet;
var
  LcxCustomDataController: TcxDBTreeListDataController;
begin
  LcxCustomDataController := TcxDBTreeList(FGrid).DataController;
  if Index = 0 then
    Result := LcxCustomDataController.DataSet
  else if Assigned(LcxCustomDataController.DataSource) then
    Result := LcxCustomDataController.DataSource.DataSet.Source
  else
    Result := nil;
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.GetFormDataSourceParamAndFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant;
var
  LUnknownParams: String;
begin
  Result := TcxDBTreeList(FGrid).GetFocusedRecordValues(AFieldNames, AUnknownFields);

  if (not AUnknownFields.IsEmpty) then begin
    LUnknownParams := AUnknownFields;
    VarArrayOverWrite
    (
      AFieldNames,
      Result,
      LUnknownParams,
      GetFormDataSourceParamValues(LUnknownParams, AUnknownFields)
    );
  end;
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.GetActive: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := GetDataSet(1);
  Result := Assigned(LDataSet) and LDataSet.Active
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.IsNotEmpty: Boolean;
begin
  Result := (TcxDBTreeList(FGrid).VisibleCount > 0);
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.Refresh;
begin
  TcxDBTreeList(FGrid).RefreshDataSet
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.GetPopupMenu: TComponent;
begin
  if (FGrid <> nil) then
    Result := TcxDBTreeList(FGrid).PopupMenu
  else
    Result := nil
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.DoDelete;
var
  LBookmark: TDataSourceBookmark;
begin
//  TcxDBTreeList(FGrid).DataController.DeleteFocused
  with TcxDBTreeList(FGrid) do
    if FocusedNode <> nil then begin
      LBookmark := TcxDBTreeList(FGrid).GridBookmark;
      DataController.DataSet.Delete;
      TcxDBTreeList(FGrid).GridBookmark := LBookmark;
    end;
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.InPlaceInsert;
begin
  with TcxDBTreeList(FGrid) do begin
    DataController.Append;
//    if (not DataController.ImageIndexField.IsEmpty) then with DataController.DataSet.FieldByName(DataController.ImageIndexField) do
//      if IsNull then
//        AsInteger := -1;

    ShowEdit;
  end;
  InPlaceSyncEdit;
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.InPlaceInsertChild(AChildComponent: TComponent);
var
  LParentFieldValue: Variant;
begin
  with TcxDBTreeList(FGrid) do begin
    with DataController do begin
      //BeginUpdate;
      DataSet.DisableControls;
      try
        LParentFieldValue := DataSet.FieldValues[KeyField];
        Append;
        DataSet.FieldValues[ParentField] := LParentFieldValue;
//        if (not DataController.ImageIndexField.IsEmpty) then with DataSet.FieldByName(DataController.ImageIndexField) do
//          if IsNull then
//            AsInteger := -1;
      finally
        DataSet.EnableControls;
        //EndUpdate;
      end;
    end;

    ShowEdit;
  end;
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.InPlaceEdit;
begin
  TcxDBTreeList(FGrid).DataController.Edit;
  TcxDBTreeList(FGrid).ShowEdit;
  InPlaceSyncEdit;
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.BeginUpdate(var ADataSourceStateBookmark: TDataSourceStateBookmark);
begin
  TcxDBTreeList(FGrid).BeginUpdate(ADataSourceStateBookmark);
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.EndUpdate(const ADataSourceStateBookmark: TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores);
begin
  TcxDBTreeList(FGrid).EndUpdate(ADataSourceStateBookmark, ADataSetStateRestore, ADataSourceRestore);
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.Evaluate(const AExpression: string; AConditionParser: PExprParser): Variant;
begin
  if TcxDBTreeList(FGrid).OptionsData.SyncMode then
    Result := inherited Evaluate(AExpression, AConditionParser)
  else
    Result := CxDBTreeListNodeExpression(TcxDBTreeListNode(TcxDBTreeList(FGrid).FocusedNode), AExpression, AConditionParser)
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.FocusedRecodIsData: Boolean;
begin
  Result := TcxDBTreeList(FGrid).FocusedNode <> nil
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.SupportFolding: Boolean;
begin
  Result := TcxMDDBTreeList(FGrid).OptionsBehavior.AlwaysExpandedLevel > -1;
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.VisibleRecordCount: Integer;
begin
  Result := TcxDBTreeList(FGrid).VisibleCount;
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.FoldingActive: Boolean;
begin
  with TcxDBTreeList(FGrid) do
    Result := (Count > 0) and (DataController.NodesCount > Count)
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.FullCollapse;
begin
  TcxDBTreeList(FGrid).FullCollapse
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.FullExpand;
begin
  TcxDBTreeList(FGrid).FullExpand
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.CanAssignPopupMenu: TPopupMenuKinds;
begin
  Result := [pmClassic, pmDxBar]
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.Cancel;
begin
  TcxDBTreeList(FGrid).DataController.Cancel;
  InPlaceSyncEdit;
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.CanModify: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := GetDataSet(1);
  Result := Assigned(LDataSet) and LDataSet.CanModify;
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.Post;
begin
  TcxDBTreeList(FGrid).DataController.Post;
  InPlaceSyncEdit;
end;

procedure TDataSourceCxMDDBTreeListUpdateControlProperties.InPlaceSyncEdit;
var
  LEditing: Boolean;
begin
  LEditing := Editing;
  if LEditing or (not FAlwaysShowEditor) then
    with TcxDBTreeList(FGrid) do begin
      OptionsBehavior.ImmediateEditor   := LEditing;
      OptionsBehavior.AlwaysShowEditor  := LEditing;
      OptionsData.Editing               := LEditing;
    end;
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.Editing: Boolean;
begin
  Result := TcxDBTreeList(FGrid).DataController.EditState * [dceInsert, dceEdit] <> [];
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.Inserted: Boolean;
begin
  Result := dceInsert in TcxDBTreeList(FGrid).DataController.EditState;
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.KeyFieldNames: String;
begin
  Result := TcxDBTreeList(FGrid).DataController.KeyField;
end;

function TDataSourceCxMDDBTreeListUpdateControlProperties.KeyFieldValues: Variant;
begin
  with TcxDBTreeList(FGrid).DataController do
    if (KeyFieldNames <> '') and (DataSet <> nil) and (DataSet.RecordCount > 0) then
      Result := DataSet.Fields.GetValues(KeyFieldNames)
end;
{$ENDREGION 'TDataSourceCxMDDBTreeListUpdateControlProperties'}

{$REGION 'TcxDataSourceMDDBVerticalGridUpdateControlProperties'}
{ TcxDataSourceMDDBVerticalGridUpdateControlProperties }

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.InternalCreate;
begin
  Assert(FGrid.InheritsFrom(TcxMDDBVerticalGrid));
  inherited;

  with TcxMDDBVerticalGrid(FGrid) do begin
    FAlwaysShowEditor                     := OptionsBehavior.AlwaysShowEditor;

    OnUpdateData                          := cxDBVerticalGridUpdateData;

//    FcxDBVerticalGridDataChanged          := OnLayoutChanged;
//    OnLayoutChanged                       := cxDBVerticalGridDataChanged;
//
//    FcxDBVerticalGridFocusedRecordChanged := OnFocusedRecordChanged;
//    OnFocusedRecordChanged                := cxDBVerticalGridFocusedRecordChanged;

//    DataController.OnRecordChanged        := cxDBVerticalGridDataControllerRecordChanged;

    FcxDBVerticalGridDblClick             := OnDblClick;
    OnDblClick                            := cxDBVerticalGridDblClick;

    FcxDBVerticalGridKeyDown              := OnKeyDown;
    OnKeyDown                             := cxDBVerticalGridKeyDown;
  end;
end;

destructor TcxDataSourceMDDBVerticalGridUpdateControlProperties.Destroy;
var
//  LcxDBVerticalGridDataChanged: TNotifyEvent;
//  LcxDBVerticalGridFocusedRecordChanged: TcxVerticalGridFocusedRecordChangedEvent;
  LcxDBVerticalGridDblClick: TNotifyEvent;
  LcxDBVerticalGridKeyDown: TKeyEvent;
begin
  if (FGrid <> nil) and (FGrid.ComponentState = []) then
    with TcxMDDBVerticalGrid(FGrid) do begin
      OnUpdateData := nil;

//      LcxDBVerticalGridDataChanged := DataController.OnDataChanged;
//      if TMethod(DataController.OnDataChanged) = TMethod(LcxDBVerticalGridDataChanged) then
//        DataController.OnDataChanged := FcxDBVerticalGridDataChanged;
//
//      LcxDBVerticalGridFocusedRecordChanged := cxDBVerticalGridFocusedRecordChanged;
//      if TMethod(OnFocusedRecordChanged) = TMethod(LcxDBVerticalGridFocusedRecordChanged) then
//        OnFocusedRecordChanged := FcxDBVerticalGridFocusedRecordChanged;

      LcxDBVerticalGridDblClick := cxDBVerticalGridDblClick;
      if TMethod(OnDblClick) = TMethod(LcxDBVerticalGridDblClick) then
        OnDblClick := FcxDBVerticalGridDblClick;

      LcxDBVerticalGridKeyDown := cxDBVerticalGridKeyDown;
      if TMethod(OnKeyDown) = TMethod(LcxDBVerticalGridKeyDown) then
        OnKeyDown := FcxDBVerticalGridKeyDown;
    end;

  inherited;
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.BeginUpdate(var ADataSourceStateBookmark: TDataSourceStateBookmark);
begin
  TcxDBVerticalGrid(FGrid).BeginUpdate(ADataSourceStateBookmark);
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.EndUpdate(const ADataSourceStateBookmark: TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores);
begin
  TcxDBVerticalGrid(FGrid).EndUpdate(ADataSourceStateBookmark, ADataSetStateRestore, ADataSourceRestore);
end;

function TcxDataSourceMDDBVerticalGridUpdateControlProperties.FoldingActive: Boolean;
begin
  Result := True
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.FullCollapse;
begin
  TcxDBVerticalGrid(FGrid).FullCollapse
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.FullExpand;
begin
  TcxDBVerticalGrid(FGrid).FullExpand
end;

function TcxDataSourceMDDBVerticalGridUpdateControlProperties.GetBookmark: TDataSourceBookmark;
begin
  Result := TcxDBVerticalGrid(FGrid).GridBookmark
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.SetBookmark(Value: TDataSourceBookmark);
begin
  TcxDBVerticalGrid(FGrid).GridBookmark := Value
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.SetInsertEnabled(AEnabled: Boolean);
begin
  with TcxDBVerticalGrid(FGrid).Navigator do if Visible then with Buttons do begin
    with Insert do if Visible then Enabled := AEnabled;
    with Append do if Visible then Enabled := AEnabled;
  end;
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.SetPopupMenu(APopupMenu: TComponent);
begin
  TcxDBVerticalGrid(FGrid).PopupMenu := APopupMenu
end;

function TcxDataSourceMDDBVerticalGridUpdateControlProperties.SupportFolding: Boolean;
begin
  Result := True
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.SetEditEnabled(AEnabled: Boolean);
begin
  with TcxDBVerticalGrid(FGrid).Navigator do
    with Buttons.Edit do if Visible then Enabled := AEnabled
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.SetDeleteEnabled(AEnabled: Boolean);
begin
  with TcxDBVerticalGrid(FGrid).Navigator do
    with Buttons.Delete do if Visible then Enabled := AEnabled
end;

function TcxDataSourceMDDBVerticalGridUpdateControlProperties.CanAssignPopupMenu: TPopupMenuKinds;
begin
  Result := [pmClassic, pmDxBar]
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.Cancel;
begin
  TcxDBVerticalGrid(FGrid).DataController.Cancel;
end;

function TcxDataSourceMDDBVerticalGridUpdateControlProperties.CanModify: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := GetDataSet(1);
  Result := Assigned(LDataSet) and LDataSet.CanModify;
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.cxDBVerticalGridUpdateData(AControl: TObject; AInfo: TcxUpdateControlInfo);
var
  LUpdateStates: TDataSourceUpdateStates;
begin
  InPlaceSyncEdit;

  if AInfo is TcxDataChangedInfo then
    LUpdateStates := [dsusActive]
  else
    LUpdateStates := [dsusData, dsusFocusedRecord];

  DoUpdate(LUpdateStates);
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.cxDBVerticalGridDblClick(Sender: TObject);
begin
  with TcxDBVerticalGrid(Sender).HitTest do
    if HitAtValue and (not DefaultItemClick) and Assigned(FcxDBVerticalGridDblClick) then
      FcxDBVerticalGridDblClick(Sender);
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.cxDBVerticalGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then begin
    if Editing then begin
      Post;
      Exit;
    end else if ([ssShift, ssAlt, ssCtrl] * Shift = []) then begin
      if DefaultItemClick(False) then Exit;
    end;
  end;

  if Assigned(FcxDBVerticalGridKeyDown) then
    FcxDBVerticalGridKeyDown(Sender, Key, Shift);
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.DoDelete;
//var
//  LBookmark: TDataSourceBookmark;
begin
  TcxDBVerticalGrid(FGrid).DataController.DeleteFocused
{
  with TcxDBVerticalGrid(FGrid) do
    if FocusedRecodIsData then begin
      LBookmark := TcxDBVerticalGrid(FGrid).GridBookmark;
      DataController.DeleteFocused;
      TcxDBVerticalGrid(FGrid).GridBookmark := LBookmark;
    end;
}
end;

function TcxDataSourceMDDBVerticalGridUpdateControlProperties.Editing: Boolean;
begin
  Result := TcxDBVerticalGrid(FGrid).DataController.EditState * [dceInsert, dceEdit] <> [];
end;

function TcxDataSourceMDDBVerticalGridUpdateControlProperties.GetActive: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := GetDataSet(1);
  Result := Assigned(LDataSet) and LDataSet.Active
end;

function TcxDataSourceMDDBVerticalGridUpdateControlProperties.GetDataSet(const Index: Integer): TDataSet;
var
  LcxCustomDataController: TcxDBVerticalGridDataController;
begin
  LcxCustomDataController := TcxDBVerticalGrid(FGrid).DataController;
  if Index = 0 then
    Result := LcxCustomDataController.DataSet
  else if Assigned(LcxCustomDataController.DataSource) then
    Result := LcxCustomDataController.DataSource.DataSet.Source
  else
    Result := nil;
end;

function TcxDataSourceMDDBVerticalGridUpdateControlProperties.IsNotEmpty: Boolean;
begin
  Result := (TcxDBVerticalGrid(FGrid).RecordCount > 0);
end;

function TcxDataSourceMDDBVerticalGridUpdateControlProperties.GetPopupMenu: TComponent;
begin
  if (FGrid <> nil) then
    Result := TcxDBVerticalGrid(FGrid).PopupMenu
  else
    Result := nil
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.InPlaceEdit;
begin
  TcxDBVerticalGrid(FGrid).DataController.Edit;
  TcxDBVerticalGrid(FGrid).ShowEdit;
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.InPlaceInsert;
begin
  with TcxDBVerticalGrid(FGrid) do begin
    DataController.Append;
    ShowEdit;
  end;
  InPlaceSyncEdit;
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.InPlaceInsertChild(AChildComponent: TComponent);
begin
  Assert(False);
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.InPlaceSyncEdit;
var
  LEditing: Boolean;
begin
  LEditing := Editing;
  if LEditing or (not FAlwaysShowEditor) then
    with TcxDBVerticalGrid(FGrid) do begin
      OptionsBehavior.ImmediateEditor   := LEditing;
      OptionsBehavior.AlwaysShowEditor  := LEditing;
      OptionsData.Editing               := LEditing;
    end
{
  else if (not LEditing) and FAlwaysShowEditor then
    with TcxDBVerticalGrid(FGrid).DataController do
      if (not DataSet.IsEmpty) and (not DataSet.Editing) and DataSet.CanModify then
        Edit;
}
end;

function TcxDataSourceMDDBVerticalGridUpdateControlProperties.Inserted: Boolean;
begin
  Result := dceInsert in TcxDBVerticalGrid(FGrid).DataController.EditState;
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.Post;
begin
  TcxDBVerticalGrid(FGrid).DataController.Post;
end;

procedure TcxDataSourceMDDBVerticalGridUpdateControlProperties.Refresh;
begin
  TcxDBVerticalGrid(FGrid).RefreshDataSet
end;
{$ENDREGION 'TcxDataSourceMDDBVerticalGridUpdateControlProperties'}

{$REGION 'TDataSourceControls'}
{ TDataSourceControls }

constructor TDataSourceControls.Create(AOwner: TForm);
begin
  FOwnerForm := TSBaseFrm(AOwner);
  inherited Create(TDataSourceControlItem);
end;

function TDataSourceControls.GetItem(Index: Integer): TDataSourceControlItem;
begin
  Result := TDataSourceControlItem(inherited GetItem(Index));
end;

function TDataSourceControls.CreateGridControl(const AGrid: TComponent): TDataSourceControlProperties;
var
  LItem: TDataSourceControlItem;
begin
  LItem := TDataSourceControlItem(Add);
  LItem.Item := AGrid;
  Result := LItem.FProperties;
end;

procedure TDataSourceControls.DeleteGridControl(AGrid: TComponent);
var
  I: Integer;
begin
  for I := Pred(Count) downto 0 do
    if Items[I].Item = AGrid then begin
      Delete(I);
      Exit;
    end;
end;

function TDataSourceControls.FindGridControl(const AGrid: TComponent): TDataSourceControlProperties;
var
  I: Integer;
begin
  Result := nil;
  for I := Pred(Count) downto 0 do
    if Items[I].Item = AGrid then begin
      Result := Items[I].FProperties;
      Exit;
    end;
end;

function TDataSourceControls.FindOrCreateGridControl(const AGrid: TComponent): TDataSourceControlProperties;
begin
  Result := FindGridControl(AGrid);
  if Result = nil then
    Result := CreateGridControl(AGrid);
end;
{$ENDREGION 'TDataSourceControls'}

{$REGION 'TDataSourceControlPropertiesLink'}
{ TDataSourceControlPropertiesLink }

procedure TDataSourceControlPropertiesLink.AssignTo(Dest: TPersistent);
begin
  Assert(Dest is TDataSourceControlPropertiesLink);

  with TDataSourceControlPropertiesLink(Dest) do begin
    Table       := Self.Table;
    FieldNames  := Self.FieldNames;
    FKeyField   := Self.FKeyField;
  end;
end;
{$ENDREGION 'TDataSourceControlPropertiesLink'}

initialization
  TDataSourceControlProperties.RegisterClass(TDataSource, TDataSourceDefaultUpdateControlProperties);

  TDataSourceControlProperties.RegisterClass(TcxGrid, TDataSourceCxGridUpdateControlProperties);
  TDataSourceControlProperties.RegisterClass(TcxGridTableView, TDataSourceCxGridTableViewUpdateControlProperties);
  TDataSourceControlProperties.RegisterClass(TcxMDDBTreeList, TDataSourceCxMDDBTreeListUpdateControlProperties);
  TDataSourceControlProperties.RegisterClass(TcxMDDBVerticalGrid, TcxDataSourceMDDBVerticalGridUpdateControlProperties);

finalization
  TDataSourceControlProperties.ClearClasses;

end.





