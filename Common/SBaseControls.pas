unit SBaseControls;
{$I config.inc}

interface

uses
  Types, SBaseDataModule, SBaseExpression, NamedVariables, SBaseCxUtils, Generics.Collections,

  Windows, Messages, SysUtils, StrUtils, Variants, Classes, Graphics, Controls, PropFilerEh,
  Forms, TypInfo, ComCtrls, Mask, ExtCtrls, StdCtrls, RegularExpressions,
  SysConst, Buttons, Clipbrd, ImgList, cxLookAndFeels, Dialogs, Math, dxBar, dxStatusBar, cxButtonEdit
{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter, fs_ievents, fsFunction_TcxControls
{$ENDIF}
  , DB, DBCommon, DSIntf,
  SBaseFormDefinitions, FieldDataLink,

  cxControls, cxGrid, cxGridLevel, cxGridTableView, cxGridDBTableView, cxEdit, cxMDLookupTreeComboBox,
  cxDBEdit, cxTL, cxDBTL, cxGridCustomView, cxGridCustomTableView, cxGridDBBandedTableView,
  cxGridDBDataDefinitions, cxInplaceContainer, cxBarEditItem, cxDBLookupComboBox, cxGroupBox,
  cxCustomData, cxDBData, cxContainer, cxStyles, cxMaskEdit, cxCheckBox, cxMultiFieldsDBDataBinding,
  cxGraphics, cxDropDownEdit, cxVGrid, cxDBVGrid, System.Actions, ActnList, dxMDBarButton;

type
{$REGION 'ControlStateRules - Definition'}
  TControlStateRules = class(TComponent)
  private
    FControlModifyState         : TControlDefaultModifyState;
    FDisableTabStopWhenReadOnly : Boolean;
    FDataSource                 : TDataSource;
    FFieldName                  : String;
    FCreatedByLinkedControl     : Boolean;
    FParent                     : TComponent;
    FParentOwner                : Boolean;

    function GetDataSet: TDataSet;
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure SetParentComponent(AComponent: TComponent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadParentOwner(Reader: TReader);
    procedure AssignTo(Dest: TPersistent); override;
    procedure Loaded; override;
  public
    class function ComponentName: String;

    class function GetRules(AControl: TComponent): TControlStateRules;
    class function CreateRules(AControl: TComponent; ACreatedByLinkedControl: Boolean = False): TControlStateRules;

    property DataSet: TDataSet read GetDataSet;
    property CreatedByLinkedControl: Boolean read FCreatedByLinkedControl;
    property ReadOnly: Boolean write SetReadOnly;
  published
    property ModifyState: TControlDefaultModifyState read FControlModifyState write FControlModifyState;
    property DisableTabStopWhenReadOnly: Boolean read FDisableTabStopWhenReadOnly write FDisableTabStopWhenReadOnly;
    property DataSource: TDataSource read FDataSource write FDataSource;
    property FieldName: String read FFieldName write FFieldName;
  end;

  TWinControlStateRulesHelper = class Helper for TWinControl
  strict private
    const Const_LabelPermissionDenied: String = 'LabelPermissionDenied';
  public
    function StateRules: TControlStateRules;
    function AccessDenied(AException: String): Boolean;
    procedure AccessGranted;
  end;

  TcxStyleHelper = class Helper for TcxStyle
    procedure AssignStyleTo(ACanvas: TcxCanvas);
  end;

  TCustomControlInterfaceClass = class of TCustomControlInterface;
  TControlInterfaceRecord = record
    ControlClass: TClass;
    InterfaceClass: TCustomControlInterfaceClass;
  end;

  TCustomControlInterface = class
  strict private
    class var FInterfaces: Array of TControlInterfaceRecord;
  private
    FControl: TComponent;

    function  GetReadOnly: Boolean; virtual;
    procedure SetReadOnly(const Value: Boolean); virtual;

    procedure SetColor(const AValue: TcxStyle); virtual;

    function  GetVisible: Boolean; virtual;
    procedure SetVisible(const Value: Boolean); virtual;

    function  GetDataSource: TDataSource; virtual;
  public
    class procedure RegisterClass(AControlClass: TClass; AInterfaceClass: TCustomControlInterfaceClass);

    class function CreateInterface(AControl: TComponent): TCustomControlInterface;
    constructor Create(AControl: TComponent); reintroduce; virtual;

    procedure DrawState(AReadOnly: Boolean; const AColor: TcxStyle); virtual;
    function DataSetDepended: Boolean; dynamic;
    function FieldDepended: Boolean; dynamic;
    function DataSourceCanModify: Boolean;
//    function DataSetCanModify: Boolean; dynamic;
    function FieldCanModify: Boolean; dynamic;

    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Color: TcxStyle {read GetColor} write SetColor;
    property Visible: Boolean read GetVisible write SetVisible;

    property DataSource: TDataSource read GetDataSource;

//    property Field: TField read GetField;
  end;

  TcxContainerInterface = class(TCustomControlInterface)
  private
    procedure SetColor(const AValue: TcxStyle); override;
  end;

  TcxCustomGridViewInterface = class(TCustomControlInterface)
  private
    FDataController: TcxCustomDataController;

    procedure SetColor(const AValue: TcxStyle); override;

    function  GetVisible: Boolean; override;
    procedure SetVisible(const Value: Boolean); override;

    function  GetReadOnly: Boolean; override;

    function  GetDataSource: TDataSource; override;
  public
    function DataSetDepended: Boolean; override;
//    function DataSetCanModify: Boolean; override;
//    function FieldDepended: Boolean; override;
    constructor Create(AControl: TComponent); override;
  end;

  TcxCustomCheckBoxInterface = class(TCustomControlInterface)
  private
    FDataBinding: TcxDBEditDataBinding;

    function GetProperties: TcxCustomCheckBoxProperties;

    function  GetReadOnly: Boolean; override;
    procedure SetReadOnly(const Value: Boolean); override;

    function  GetDataSource: TDataSource; override;
  public
    constructor Create(AControl: TComponent); override;

    procedure DrawState(AReadOnly: Boolean; const AColor: TcxStyle); override;
    function FieldDepended: Boolean; override;
    function FieldCanModify: Boolean; override;
  end;

  TcxCustomEditInterface = class(TCustomControlInterface)
  private
    [Weak]FDataBinding: TcxEditDataBinding;

    function GetProperties: TcxCustomEditProperties;

    function  GetReadOnly: Boolean; override;
    procedure SetReadOnly(const Value: Boolean); override;

    procedure SetColor(const AValue: TcxStyle); override;

    function  GetDataSource: TDataSource; override;
  public
    constructor Create(AControl: TComponent); override;

    function FieldDepended: Boolean; override;
    function FieldCanModify: Boolean; override;
  end;

  TcxCustomDropDownEditInterface = class(TcxCustomEditInterface)
  public
    procedure DrawState(AReadOnly: Boolean; const AColor: TcxStyle); override;
  end;

  TCustomButtonInterface = class(TCustomControlInterface)
  public
    procedure DrawState(AReadOnly: Boolean; const AColor: TcxStyle); override;
  end;

  TSynEditInterface = class(TCustomControlInterface)
  private
    function  GetReadOnly: Boolean; override;
    procedure SetReadOnly(const Value: Boolean); override;
  end;

  TDBSynEditInterface = class(TSynEditInterface)
  private
    function GetDataSource: TDataSource; override;
  public
    function FieldDepended: Boolean; override;
    function FieldCanModify: Boolean; override;
  end;

  TcxCustomTreeListInterface = class(TCustomControlInterface)
  private
    FDataController: TcxCustomDataController;

    procedure SetColor(const AValue: TcxStyle); override;
    function  GetDataSource: TDataSource; override;
  public
    function DataSetDepended: Boolean; override;
//    function DataSetCanModify: Boolean; override;
//    function FieldDepended: Boolean; override;
    constructor Create(AControl: TComponent); override;
  end;

  TcxCustomVerticalGridInterface = class(TCustomControlInterface)
  private
    FDataController: TcxCustomDataController;

    procedure SetColor(const AValue: TcxStyle); override;

    function  GetVisible: Boolean; override;
    procedure SetVisible(const Value: Boolean); override;

    function  GetDataSource: TDataSource; override;
  public
    function DataSetDepended: Boolean; override;
//    function DataSetCanModify: Boolean; override;
//    function FieldDepended: Boolean; override;
    constructor Create(AControl: TComponent); override;
  end;

{
  TcxCustomEditorRowPropertiesInterface = class(TCustomControlInterface)
  private
    FDataBinding: TcxEditDataBinding;

    function GetProperties: TcxCustomEditProperties;

    function  GetReadOnly: Boolean; override;
    procedure SetReadOnly(const Value: Boolean); override;

    procedure SetColor(const AValue: TcxStyle); override;

    function  GetDataSource: TDataSource; override;
  public
    constructor Create(AControl: TComponent); override;

    function FieldDepended: Boolean; override;
    function FieldCanModify: Boolean; override;
  end;
}
{$ENDREGION 'ControlStateRules - Definition'}

{$REGION 'GridExprEvaluators - Definition'}
  TcxGridDataRowExprEvaluator = class(TCanExprEvaluator)
  private
    FcxGridDataRow: TcxGridDataRow;
  protected
    function DoGetFieldValue(FieldNo: Word; const AFieldName: string; var FldType, FldLen: Integer): Variant; override;
  public
    constructor Create(AcxGridDataRow: TcxGridDataRow);
  end;
  PcxGridDataRowExprEvaluator = ^TcxGridDataRowExprEvaluator;

  TcxDBTreeListExprEvaluator = class(TCanExprEvaluator)
  private
    FcxDBTreeListNode: TcxDBTreeListNode;
  protected
    function DoGetFieldValue(FieldNo: Word; const FieldName: string; var FldType, FldLen: Integer): Variant; override;
  public
    constructor Create(AcxDBTreeListNode: TcxDBTreeListNode);
  end;
  PcxDBTreeListExprEvaluator = ^TcxDBTreeListExprEvaluator;

  function CxGridDataRowExpression(AcxGridDataRow: TcxGridDataRow; const AExpression: string; AConditionParser: PExprParser = nil): Variant;
  function CxDBTreeListNodeExpression(AcxDBTreeListNode: TcxDBTreeListNode; const AExpression: string; AConditionParser: PExprParser = nil): Variant;
{$ENDREGION 'GridExprEvaluators - Definition'}

{$REGION 'GridColorer - Definition'}
type
  TColorGridCondition = class(TCollectionItem)
  private
    procedure SetCondition(const Value: String);
  protected
    FCondition          : String;
    FColor              : TcxStyle;
  public
    FConditionParser    : TExprParser;
//    FConditionEvaluator : TCanExprEvaluator;

    destructor Destroy; override;
  published
    property Condition: String read FCondition write SetCondition;
    property Color: TcxStyle read FColor write FColor;
  end;

  TColorGridConditions = class(TCollection)
  private
    function GetItem(Index: Integer): TColorGridCondition;
  public
    constructor Create; reintroduce;
    property Items[Index: Integer]: TColorGridCondition read GetItem; default;
  end;

  TBaseColorGridInterpritatorClass = class of TBaseColorGridInterpritator;
  TBaseColorGridInterpritatorRecord = record
    ControlClass: TClass;
    InterfaceClass: TBaseColorGridInterpritatorClass;
  end;

  TColorGridItem = class;

  TBaseColorGridInterpritator = class(TObject)
  strict private
    class var FInterfaces: Array of TBaseColorGridInterpritatorRecord;
  protected
    [weak] FOwner: TColorGridItem;
    FMethod: TMethod;
  public
    class procedure RegisterClass(AControlClass: TClass; AInterfaceClass: TBaseColorGridInterpritatorClass);
    class function Create(AOwner: TColorGridItem): TBaseColorGridInterpritator;
    constructor InternalCreate(AOwner: TColorGridItem); dynamic;
  end;

  TcxGridTableViewColorGridInterpritator = class(TBaseColorGridInterpritator)
  private
    procedure cxGridTableViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
  protected
    FCachedCxGridDataRow  : TcxGridDataRow;
    FCachedCxGridTableItem: TcxCustomGridTableItem;
    FCachedCxGridStyle    : TcxStyle;
  public
    constructor InternalCreate(AOwner: TColorGridItem); override;
    destructor Destroy; override;
  end;

  TcxDBTreeListColorGridInterpritator = class(TBaseColorGridInterpritator)
  private
    procedure cxDBTreeListStylesGetContentStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
  protected
    FCachedCxTLNode     : TcxTreeListNode;
    FCachedCxTLItem     : TcxTreeListColumn;
    FCachedCxTLStyle    : TcxStyle;
  public
    constructor InternalCreate(AOwner: TColorGridItem); override;
    destructor Destroy; override;
  end;

  TColorGridItem = class(TCollectionItem)
  private
    [Weak] FGrid: TComponent;
    FConditions: TColorGridConditions;
    FInterpritator: TBaseColorGridInterpritator;

    procedure SetGrid(const Value: TComponent);
  public
    destructor Destroy; override;
    constructor Create(Collection: TCollection); override;
  published
    property Grid: TComponent read FGrid write SetGrid;
    property Conditions: TColorGridConditions read FConditions write FConditions;
  end;

  TGridsColorsCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TColorGridItem;
  public
    constructor Create; reintroduce;
    property Items[Index: Integer]: TColorGridItem read GetItem; default;
  end;

  TGridsColors = class(TComponent)
  private
    FCollection: TGridsColorsCollection;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Grids: TGridsColorsCollection read FCollection write FCollection;
  end;
{$ENDREGION 'GridColorer - Definition'}

{$REGION 'TSBaseCustomSQLMenuController - Definition'}
  TSBaseCustomSQLMenuButton = class(TdxMDBarButton)
  private
    FId: Variant;
    [Weak] FParent : TSBaseCustomSQLMenuButton;
    [Weak] FDefault: TSBaseCustomSQLMenuButton;
  end;

  TSBaseCustomSQLMenuController = class(TComponent)
    procedure OnPopup(Sender: TObject);
  strict private
    [Weak] FSBaseFrm: TForm;
    [Weak] FParent: TComponent;
    FRoot: TSBaseCustomSQLMenuButton;

    FMenuComponent: TdxBarComponent;
    FItemLinks: TdxBarItemLinks;

    FClickableCount: Integer;

    FSQL: TStrings;
    FActive: Boolean;
    FConfigured: Boolean;

    FParams: TNamedVariants;
    FItems: TObjectList<TdxBarItem>;
    FItemReactionSelectNotify: TCallBackSelectFunction;
    FOnSQLChanged: TNotifyEvent;

    procedure PrivateCreate;

    procedure SQLChange(Sender: TObject);
  private
    function GetSQL: TStrings;
    procedure SetSQL(AValue: TStrings);
    procedure SetMenuComponent(const AValue: TdxBarComponent);
    function GetActive: Boolean;
    procedure SetActive(const AValue: Boolean);
  protected
    procedure SetParentComponent(AValue: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CustomCreate(AOwner: TForm);

    destructor Destroy; override;

    procedure UnConfigure;
    procedure Configure(AReconfigure: Boolean = False; ARaiseIfNotReady: Boolean = True);

    function DefaultItem: TSBaseCustomSQLMenuButton;

    property Root: TSBaseCustomSQLMenuButton read FRoot;
    property ClickableCount: Integer read FClickableCount;
    property OnSQLChanged: TNotifyEvent read FOnSQLChanged write FOnSQLChanged;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property SQL: TStrings read GetSQL write SetSQL;
    property ItemReactionSelectNotify: TCallBackSelectFunction read FItemReactionSelectNotify write FItemReactionSelectNotify;
    property MenuComponent: TdxBarComponent read FMenuComponent write SetMenuComponent;
  end;
{$ENDREGION 'TSBaseCustomSQLMenuController - Definition'}

{$REGION 'Lookup Controls - Definition'}
  { Базовый класс контроллера }
  TSBaseCustomLookupComponentControl = class(TComponent)
    procedure ButtonOnClick(Sender: TObject);
    procedure ButtonOnClickCx(Sender: TObject; AButtonIndex: Integer);

    procedure OnSelectNotify(ASelectAction: TCallBackSelectAction; const AResultValues: TNamedVariants);
  strict private
    FInParamValueChanged: Boolean;
    FHasSubControls: Boolean;
    FSelectMenuConfigured: Boolean;
    FSelectMenuSQLController: TSBaseCustomSQLMenuController;

    procedure SetOnButtonClickEvent;
  private
    FControl      : TComponent;
    FSubControlIndex : SmallInt;

    FcxCustomEditProperties: TcxCustomEditProperties;
    FcxCustomObject: TObject;
    FReadOnly: Boolean;

    FSavedMethod      : TMethod;
    FSelectButtonIndex: SmallInt;
    FClearButtonIndex : SmallInt;
    FViewButtonIndex  : SmallInt;
    FViewButton       : TObject;

    FParamName : String;
    FParamCount: Byte;
    FParamValueLocal: Boolean;

    FExtendedParamValues: TNamedVariants;

    FSelectMenu: TdxBarPopupMenu;

    FExtractSQL   : String;
    FExtractScript: String;

    FOnParamValueChanged: TNotifyEvent;
    FOnLookupSelect: TNotifyEvent;

    FOnOtherButtonClick: TcxEditButtonClickEvent;

    procedure SetControl(const Value: TComponent);
    procedure SetSubControlIndex(const AValue: SmallInt);
    procedure SetSelectButtonIndex(const Value: SmallInt);
    procedure SetClearButtonIndex(const Value: SmallInt);

    function  GetSelectMenu: TdxBarPopupMenu;
    procedure SetSelectMenu(const AValue: TdxBarPopupMenu);

    procedure ConfigureSelectMenu;

    procedure SetParamName(const AValue: String);

    procedure Select;
    procedure Clear;
    procedure View;

    function  GetCanModify: Boolean;
    procedure SetReadOnly(const Value: Boolean); virtual;

    function  GetParamValues(Index: String): Variant;
    procedure SetParamValues(Index: String; const Value: Variant);

    function  GetParamValue: Variant;
    function  GetDataSourceActive: Boolean;

    procedure SelectMenuSQLChanged(ASender: TObject);
  private
    class function DataSourceRequired: Boolean; dynamic;
    procedure InitControl; dynamic;
    function  GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);

    function  GetDataField: String;
    procedure SetDataField(const Value: String);

    procedure DataChanged(Sender: TObject);

    function  GetDataFieldsCount: Integer;

    procedure DataLinkChanged;
    function  GetOnDataFieldChanged: TNotifyEvent;
    procedure SetOnDataFieldChanged(const Value: TNotifyEvent);

    procedure DoParamNameChanged; dynamic;
    procedure ParamValueChanged; dynamic;
    procedure SetParamValue(const AValue: Variant);

    function  GetDataLinkEnabled: Boolean;
    procedure SetDataLinkEnabled(const Value: Boolean);

    function  GetOrCreateSelectMenuSQLController: TSBaseCustomSQLMenuController;
    function  GetSelectMenuSQL: TStrings;
    procedure SetSelectMenuSQL(AValue: TStrings);

    procedure SetExtractSQL(const Value: String);
    procedure SetViewButtonIndex(const Value: SmallInt);
    function GetViewButton: TObject;
  protected
    FInSelectNotify: Boolean;
    FSkipExtractParamValues: Boolean;

    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ExtractSQLChanged; dynamic;
    procedure DoOnSelect(const AResultValues: TNamedVariants); dynamic;
    procedure DoAfterExtractParamValues(var AParamValues: TNamedVariants); dynamic;
    function  SelectMenuSQLExist: Boolean; inline;

    property DataFieldsCount: Integer read GetDataFieldsCount;
  protected
    FDataLink: TMDFieldDataLink;
    FSBaseFrm: TForm;
  protected
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataLinkEnabled: Boolean read GetDataLinkEnabled write SetDataLinkEnabled default True;

    property OnDataFieldChanged: TNotifyEvent read GetOnDataFieldChanged write SetOnDataFieldChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ExtractParamValues; dynamic;

    property ParamValue: Variant read GetParamValue write SetParamValue;
    property ParamValues[Index: String]: Variant read GetParamValues write SetParamValues;
    property CanModify: Boolean read GetCanModify;
  published
    property Control: TComponent read FControl write SetControl;
    property ViewButton: TObject read GetViewButton;

    property SubControlIndex: SmallInt read FSubControlIndex write SetSubControlIndex default -1;
    property SelectButtonIndex: SmallInt read FSelectButtonIndex write SetSelectButtonIndex default -1;
    property SelectMenu: TdxBarPopupMenu read GetSelectMenu write SetSelectMenu;
    property SelectMenuSQL: TStrings read GetSelectMenuSQL write SetSelectMenuSQL;
    property ClearButtonIndex: SmallInt read FClearButtonIndex write SetClearButtonIndex default -1;
    property ViewButtonIndex: SmallInt read FViewButtonIndex write SetViewButtonIndex default -1;

    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;

    property DataField: String read GetDataField write SetDataField;
    property ParamName: String read FParamName write SetParamName;

    property ExtractSQL: String read FExtractSQL write SetExtractSQL;
    property ExtractScript: String read FExtractScript write FExtractScript;

    property OnParamValueChanged: TNotifyEvent read FOnParamValueChanged write FOnParamValueChanged;
    property OnLookupSelect: TNotifyEvent read FOnLookupSelect write FOnLookupSelect;
    property OnOtherButtonClick: TcxEditButtonClickEvent read FOnOtherButtonClick write FOnOtherButtonClick;

    function IsParamPresent: Boolean;
  end;

  { Класс контроллера без собственной реализации отображения }
  TSBaseCustomDBLookupComponentControl = class(TSBaseCustomLookupComponentControl)
  private
    class function DataSourceRequired: Boolean; override;
  protected
    procedure ExtractSQLChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property DataLinkEnabled default False;
    property DataSource;
    property DataField;
    property OnDataFieldChanged;
  end;

  { Класс контроллера с реализацией через открытие датасета }

  TSBaseLookupDataSetComponentControl = class(TSBaseCustomLookupComponentControl)
  private
    FLookupSource: TDataSource;
    FLookupAlwaysActive: Boolean;

    procedure SetLookupSource(const Value: TDataSource);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure ParamValueChanged; override;
    function  GetLookupDataSet: TDataSet;
  public
    property ParamValue;
  published
    property DataField;
    property LookupSource: TDataSource read FLookupSource write SetLookupSource;
    property LookupAlwaysActive: Boolean read FLookupAlwaysActive write FLookupAlwaysActive default False;
  end;

  { Класс контроллера с реализацией через ExtractSQL/ExtractScript }

  TSBaseLookupComponentControl = class(TSBaseCustomLookupComponentControl)
  strict private
    FDisplayText: String;
  private
    FEmptyString  : String;
    FSkipExtractDisplayText: Boolean;

    procedure SetDisplayText(const AValue: String; AUserAction: Boolean = True);
    procedure SetEmptyString(const Value: String);
    procedure InitControl; override;
  protected
    procedure DoOnSelect(const AResultValues: TNamedVariants); override;
    procedure DoAfterExtractParamValues(var AParamValues: TNamedVariants); override;

    procedure ParamValueChanged; override;
    procedure DoParamNameChanged; override;
  public
    procedure ExtractDisplayText;

    property ParamValue;
  published
    property DataField;

    property EmptyString: String read FEmptyString write SetEmptyString;
  end;

  { DB-Класс контроллера с реализацией через открытие датасета }

  TSBaseDBLookupComponentControl = class(TSBaseLookupComponentControl)
  private
    class function DataSourceRequired: Boolean; override;
  public
    property ParamValue;
  published
    property DataLinkEnabled default True;
    property DataSource;
    property DataField;
    property OnDataFieldChanged;
  end;

  { DB-Класс контроллера с реализацией через ExtractSQL/ExtractScript }

  TSBaseDBLookupDataSetComponentControl = class(TSBaseLookupDataSetComponentControl)
  private
    class function DataSourceRequired: Boolean; override;
  public
    property ParamValue;
//    constructor Create(AOwner: TComponent); override;
  published
    property DataLinkEnabled default True;
    property DataSource;
//    property DataField;
    property OnDataFieldChanged;
  end;
{$ENDREGION 'Lookup Controls - Definition'}

{$REGION 'FASTSCRIPT_RTTI - Definition'}
{$IFDEF FASTSCRIPT_RTTI}
  TfsFunctions_SBaseControls = class(TfsRTTIModule)
  private
    function  Call_TSBaseCustomSQLMenuController_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Call_TSBaseCustomLookupComponentControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Get_TSBaseCustomLookupComponentControl_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TSBaseCustomLookupComponentControl_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Call_TSBaseLookupComponentControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}
{$ENDREGION 'FASTSCRIPT_RTTI - Definition'}

implementation

uses
  Menus, SBaseConstants, SBaseForm, SBaseStringFunctions, SBaseVariantFunctions,
  SBaseDataHelperInternal, SBaseItemReactions, SBaseUtils, SynEdit, SynDBEdit,
  cxButtons
{$IFDEF DATAHANDLER}
  , DataHandler
{$ENDIF}
;

type
  TControlCrack = class(TControl);
  TcxContainerCrack = class(TcxContainer);
  TcxCustomEditCrack = class(TcxCustomEdit);
  TcxCustomGridViewCrack = class(TcxCustomGridView);
  TdxBarItemCrack = class(TdxBarItem);

{$REGION 'ControlStateRules'}
{ TControlStateRules }

procedure TControlStateRules.AssignTo(Dest: TPersistent);
begin
  Assert(Dest is TControlStateRules);

  with TControlStateRules(Dest) do begin
    ModifyState                 := Self.ModifyState;
    DisableTabStopWhenReadOnly  := Self.DisableTabStopWhenReadOnly;
    DataSource                  := Self.DataSource;
    FieldName                   := Self.FieldName;
  end;
end;

class function TControlStateRules.ComponentName: String;
begin
  Result := 'ControlStateRules'
end;

class function TControlStateRules.GetRules(AControl: TComponent): TControlStateRules;
begin
  Result := TControlStateRules(AControl.FindComponent(ComponentName));
end;

procedure TControlStateRules.Loaded;
begin
  inherited;
  if FParentOwner then Free;
end;

procedure TControlStateRules.SetParentComponent(AComponent: TComponent);
begin
  inherited;
  if FParent <> AComponent then begin
    Assert(Assigned(AComponent));
    FParent := AComponent;
  end;
end;

procedure TControlStateRules.SetReadOnly(const Value: Boolean);
begin
  if Value then
    ModifyState := bfcsReadOnly
  else
    ModifyState := bfcsWriteAble;

  if FParent is TcxCustomEdit then
    TcxCustomEdit(FParent).ActiveProperties.ReadOnly := Value
end;

class function TControlStateRules.CreateRules(AControl: TComponent; ACreatedByLinkedControl: Boolean = False): TControlStateRules;
begin
  Result := GetRules(AControl);
  if Result = nil then begin
    Result := TControlStateRules.Create(AControl);
    Result.Name := ComponentName;
    Result.FCreatedByLinkedControl := ACreatedByLinkedControl;
    Result.DisableTabStopWhenReadOnly := False;
  end;
end;

procedure TControlStateRules.DefineProperties(Filer: TFiler);
{
  function DoWrite: Boolean;
  begin
    Result := False;
  end;
}
begin
  Filer.DefineProperty('ParentOwner', ReadParentOwner, nil, False);
end;

procedure TControlStateRules.ReadParentOwner(Reader: TReader);
var
  LNew: TControlStateRules;
begin
  if Reader.ReadBoolean then begin
    Assert(Assigned(FParent));
    LNew := TControlStateRules.CreateRules(FParent, FCreatedByLinkedControl);
    AssignTo(LNew);
    FParentOwner := True;
  end;
end;

function TControlStateRules.GetDataSet: TDataSet;
begin
  if (FDataSource <> nil) then
    Result := FDataSource.DataSet
  else
    Result := nil;
end;

{ TComponentStateRulesHelper }

function TWinControlStateRulesHelper.StateRules: TControlStateRules;
begin
  Result := TControlStateRules.CreateRules(Self)
end;

{$IFNDEF PACKAGE}
function TWinControlStateRulesHelper.AccessDenied(AException: String): Boolean;
var
  LLabel: TComponent;
begin
  Result :=
    TRegEx.Create('(SELECT|EXEC|EXECUTE) (Pseudo |)permission (was |)denied on (the |)object', [roIgnoreCase]).Match(AException).Success
    or (Pos('прав доступа', AException) > 0)
    or (Pos('Invalid object name', AException) > 0)
    or (Pos('Could not find stored procedure', AException) > 0)
  ;

  if Result and Assigned(Self) then begin
    LLabel := FindComponent(Const_LabelPermissionDenied);
    if not Assigned(LLabel) or not (LLabel is TLabel) then
      with TLabel.Create(Self) do begin
        Name        := Const_LabelPermissionDenied;
        Parent      := Self;
        AutoSize    := False;
        Transparent := True;
        Font.Color  := clRed;
        Font.Style  := [];

        Left    := 0;
        Top     := Self.Height div 2 - 7;
        Width   := Self.Width;
        Height  := 18;

        Alignment := taCenter;
        //Align := alClient;
        Anchors := [akLeft, akTop, akRight];
        Caption   := SBaseProperties.Message_ViewAccessDenied;
     end
  end;
end;

procedure TWinControlStateRulesHelper.AccessGranted;
var
  LLabel: TComponent;
begin
  if not Assigned(Self) then Exit;

  LLabel := FindComponent(Const_LabelPermissionDenied);
  if Assigned(LLabel) and (LLabel is TLabel) then
    LLabel.Free;
end;
{$ENDIF}

{ TCustomControlInterface }

constructor TCustomControlInterface.Create(AControl: TComponent);
begin
  FControl := AControl;
  inherited Create;
end;

class function TCustomControlInterface.CreateInterface(AControl: TComponent): TCustomControlInterface;
var
  I: Integer;
  LInterface: TControlInterfaceRecord;
begin
  Result := nil;
  for I := 0 to Length(FInterfaces) - 1 do begin
    LInterface := FInterfaces[I];
    if AControl.InheritsFrom(LInterface.ControlClass) then begin
      if Assigned(LInterface.InterfaceClass) then begin
        Result := LInterface.InterfaceClass.Create(AControl);
      end;
      Exit;
    end;
  end;
end;

//function TCustomControlInterface.DataSetCanModify: Boolean;
//begin
//  Result := False;
//end;

function TCustomControlInterface.DataSetDepended: Boolean;
begin
  Result := False;
end;

function TCustomControlInterface.DataSourceCanModify: Boolean;
var
  LDataSource: TDataSource;
  LDataSet: TDataSet;
begin
  LDataSource := GetDataSource;
  Result := Assigned(LDataSource);
  if Result then begin
    LDataSet := LDataSource.DataSet;
    Result := Assigned(LDataSet) and LDataSet.CanModify
{$IFDEF DATAHANDLER}
      and (not (LDataSource is TCustomDataHandler) or TCustomDataHandler(LDataSource).CanModify)
{$ENDIF}
    ;
  end;
end;

procedure TCustomControlInterface.DrawState(AReadOnly: Boolean; const AColor: TcxStyle);
begin
  SetColor(AColor);
end;

function TCustomControlInterface.FieldCanModify: Boolean;
begin
  Result := False;
end;

function TCustomControlInterface.FieldDepended: Boolean;
begin
  Result := False;
end;

function TCustomControlInterface.GetDataSource: TDataSource;
begin
  Result := nil;
end;

function TCustomControlInterface.GetReadOnly: Boolean;
begin
  Result := (not TControlCrack(FControl).Enabled)
end;

procedure TCustomControlInterface.SetReadOnly(const Value: Boolean);
begin
  TControlCrack(FControl).Enabled := not Value;
end;

function TCustomControlInterface.GetVisible: Boolean;
begin
  Result := TControl(FControl).Visible
end;

class procedure TCustomControlInterface.RegisterClass(AControlClass: TClass; AInterfaceClass: TCustomControlInterfaceClass);
var
  L: Integer;
begin
  L := Length(FInterfaces);
  SetLength(FInterfaces, L + 1);
  with FInterfaces[L] do begin
    ControlClass := AControlClass;
    InterfaceClass := AInterfaceClass;
  end;
end;

procedure TCustomControlInterface.SetVisible(const Value: Boolean);
begin
  TControl(FControl).Visible := Value
end;

procedure TCustomControlInterface.SetColor(const AValue: TcxStyle);
begin
  TControlCrack(FControl).Color := AValue.Color;
end;

{ TcxContainerInterface }

procedure TcxContainerInterface.SetColor(const AValue: TcxStyle);
begin
  TcxContainerCrack(FControl).Style.Color := AValue.Color;
end;

{ TcxCustomCheckBoxInterface }

function TcxCustomCheckBoxInterface.GetProperties: TcxCustomCheckBoxProperties;
begin
  Result := TcxCustomCheckBoxProperties(TcxCustomCheckBox(FControl).Properties)
end;

function TcxCustomCheckBoxInterface.GetDataSource: TDataSource;
begin
  if FDataBinding <> nil then
    Result := FDataBinding.DataSource
  else
    Result := nil
end;

function TcxCustomCheckBoxInterface.GetReadOnly: Boolean;
begin
  Result := GetProperties.ReadOnly;
end;

procedure TcxCustomCheckBoxInterface.SetReadOnly(const Value: Boolean);
begin
  GetProperties.ReadOnly := Value;
end;

constructor TcxCustomCheckBoxInterface.Create(AControl: TComponent);
begin
  inherited;
  if FControl is TcxDBCheckBox then
    FDataBinding := TcxDBCheckBox(FControl).DataBinding
  else
    FDataBinding := nil
end;

procedure TcxCustomCheckBoxInterface.DrawState(AReadOnly: Boolean; const AColor: TcxStyle);
begin
  TcxCustomCheckBox(FControl).Enabled := (not AReadOnly);
end;

function TcxCustomCheckBoxInterface.FieldCanModify: Boolean;
begin
  if FDataBinding <> nil then
    Result := FDataBinding.DataLink.CanModify
  else
    Result := False
end;

function TcxCustomCheckBoxInterface.FieldDepended: Boolean;
begin
  Result := (FDataBinding <> nil)
end;

{ TcxCustomEditInterface }

function TcxCustomEditInterface.GetProperties: TcxCustomEditProperties;
begin
  Result := TcxCustomEditCrack(FControl).Properties;
end;

function TcxCustomEditInterface.GetReadOnly: Boolean;
begin
  Result := GetProperties.ReadOnly
end;

procedure TcxCustomEditInterface.SetReadOnly(const Value: Boolean);
begin
  GetProperties.ReadOnly := Value;
end;

procedure TcxCustomEditInterface.SetColor(const AValue: TcxStyle);
begin
  TcxCustomEdit(FControl).Style.Color := AValue.Color;
end;

constructor TcxCustomEditInterface.Create(AControl: TComponent);
begin
  inherited;

  if IsPublishedProp(FControl, SConst_DataBinding) then begin
    FDataBinding := TcxEditDataBinding(GetObjectProp(FControl, SConst_DataBinding));
    if not ((FDataBinding is TcxDBEditDataBinding) or (FDataBinding is TcxMDDBEditDataBinding)) then
      FDataBinding := nil;
  end else
    FDataBinding := nil
end;

function TcxCustomEditInterface.FieldCanModify: Boolean;
begin
  Result := False;
  if FDataBinding <> nil then
    if FDataBinding is TcxDBEditDataBinding then
      Result := TcxDBEditDataBinding(FDataBinding).DataLink.CanModify
    else if FDataBinding is TcxMDDBEditDataBinding then
      Result := TcxMDDBEditDataBinding(FDataBinding).DataLink.CanModify
end;

function TcxCustomEditInterface.FieldDepended: Boolean;
begin
  Result := (FDataBinding <> nil)
end;

function TcxCustomEditInterface.GetDataSource: TDataSource;
begin
  Result := nil;
  if FDataBinding <> nil then
    if FDataBinding is TcxDBEditDataBinding then
      Result := TcxDBEditDataBinding(FDataBinding).DataSource
    else if FDataBinding is TcxMDDBEditDataBinding then
      Result := TcxMDDBEditDataBinding(FDataBinding).DataSource
end;

{ TcxCustomDropDownEditInterface }

procedure TcxCustomDropDownEditInterface.DrawState(AReadOnly: Boolean; const AColor: TcxStyle);
begin
  inherited;
  with TcxCustomDropDownEditProperties(GetProperties) do
    ImmediateDropDownWhenActivated := (Buttons.Count > 0) and Buttons[0].Visible and (not AReadOnly);
end;

{ TcxCustomGridViewInterface }

procedure TcxCustomGridViewInterface.SetColor(const AValue: TcxStyle);
begin
  with TcxGridTableViewStyles(TcxCustomGridViewCrack(FControl).Styles) do begin
    Background := AValue;
    Content    := AValue;
  end;
end;

function TcxCustomGridViewInterface.GetVisible: Boolean;
begin
  Result := TcxCustomGridViewCrack(FControl).Visible
end;

procedure TcxCustomGridViewInterface.SetVisible(const Value: Boolean);
begin
  Assert(GetVisible = Value);
end;

constructor TcxCustomGridViewInterface.Create(AControl: TComponent);
begin
  inherited;
  FDataController := TcxCustomDataController(GetObjectProp(FControl, SConst_DataController));
end;

//function TcxCustomGridViewInterface.DataSetCanModify: Boolean;
//var
//  LDataSet: TDataSet;
//begin
//  Result := DataSetDepended;
//  if Result then begin
//    LDataSet := TcxDBDataController(FDataController).DataSet;
//    Result := Assigned(LDataSet) and LDataSet.CanModify
//  end;
//end;

function TcxCustomGridViewInterface.DataSetDepended: Boolean;
begin
  Result := (FDataController is TcxDBDataController)
end;

function TcxCustomGridViewInterface.GetDataSource: TDataSource;
begin
  if FDataController is TcxDBDataController then
    Result := TcxDBDataController(FDataController).DataSource
  else
    Result := nil
end;

function TcxCustomGridViewInterface.GetReadOnly: Boolean;
begin
  Result := False
end;

{ TcxCustomTreeListInterface }

procedure TcxCustomTreeListInterface.SetColor(const AValue: TcxStyle);
begin
  TcxCustomTreeList(FControl).Styles.Background := AValue;
  TcxCustomTreeList(FControl).Styles.Content    := AValue;
end;

constructor TcxCustomTreeListInterface.Create(AControl: TComponent);
begin
  inherited;

  FDataController := TcxCustomDataController(GetObjectProp(FControl, SConst_DataController));
end;

//function TcxCustomTreeListInterface.DataSetCanModify: Boolean;
//var
//  LDataSet: TDataSet;
//begin
//  Result := DataSetDepended;
//  if Result then begin
//    LDataSet := TcxDBDataController(FDataController).DataSet;
//    Result := Assigned(LDataSet) and LDataSet.CanModify
//  end;
//end;

function TcxCustomTreeListInterface.DataSetDepended: Boolean;
begin
  Result := FDataController is TcxDBDataController
end;

function TcxCustomTreeListInterface.GetDataSource: TDataSource;
begin
  if FDataController is TcxDBDataController then
    Result := TcxDBDataController(FDataController).DataSource
  else
    Result := nil;
end;

{ TcxCustomVerticalGridInterface }

constructor TcxCustomVerticalGridInterface.Create(AControl: TComponent);
begin
  inherited;

  FDataController := TcxCustomDataController(GetObjectProp(FControl, SConst_DataController));
end;

//function TcxCustomVerticalGridInterface.DataSetCanModify: Boolean;
//var
//  LDataSet: TDataSet;
//begin
//  Result := DataSetDepended;
//  if Result then begin
//    LDataSet := TcxDBDataController(FDataController).DataSet;
//    Result := Assigned(LDataSet) and LDataSet.CanModify
//  end;
//end;

function TcxCustomVerticalGridInterface.DataSetDepended: Boolean;
begin
  Result := FDataController is TcxDBDataController
end;

function TcxCustomVerticalGridInterface.GetDataSource: TDataSource;
begin
  if FDataController is TcxDBDataController then
    Result := TcxDBDataController(FDataController).DataSource
  else
    Result := nil;
end;

function TcxCustomVerticalGridInterface.GetVisible: Boolean;
begin
  Result := TcxCustomVerticalGrid(FControl).Visible
end;

procedure TcxCustomVerticalGridInterface.SetVisible(const Value: Boolean);
begin
  TcxCustomVerticalGrid(FControl).Visible := Value;
end;

procedure TcxCustomVerticalGridInterface.SetColor(const AValue: TcxStyle);
begin
  TcxCustomVerticalGrid(FControl).Styles.Content := AValue;
  TcxCustomVerticalGrid(FControl).Styles.Background := AValue;
  TcxCustomVerticalGrid(FControl).Styles.NavigatorInfoPanel := AValue;
end;
{$ENDREGION 'ControlStateRules'}

{$REGION 'GridExprEvaluators'}
{ TcxGridDataRowExprEvaluator }

constructor TcxGridDataRowExprEvaluator.Create(AcxGridDataRow: TcxGridDataRow);
begin
  FcxGridDataRow := AcxGridDataRow;
end;

function TcxGridDataRowExprEvaluator.DoGetFieldValue(FieldNo: Word; const AFieldName: string; var FldType, FldLen: Integer): Variant;
var
  LUnknownItemNames: String;
begin
  LUnknownItemNames := '';
  Result := TcxGridDBDataController(FcxGridDataRow.GridView.DataController).GetRecordValues(FcxGridDataRow.RecordIndex, AFieldName, @LUnknownItemNames);

  if not LUnknownItemNames.IsEmpty then
    Raise Exception.Create('Field '#171 + LUnknownItemNames + #187' not found.');
end;

{ TcxDBTreeListExprEvaluator }

constructor TcxDBTreeListExprEvaluator.Create(AcxDBTreeListNode: TcxDBTreeListNode);
begin
  FcxDBTreeListNode := AcxDBTreeListNode;
end;

function TcxDBTreeListExprEvaluator.DoGetFieldValue(FieldNo: Word; const FieldName: string; var FldType, FldLen: Integer): Variant;
var
  Idx: Integer;
begin
  if SameText(TcxDBTreeList(FcxDBTreeListNode.TreeList).DataController.KeyField, FieldName) then
    Result := FcxDBTreeListNode.KeyValue
  else if SameText(TcxDBTreeList(FcxDBTreeListNode.TreeList).DataController.ParentField, FieldName) then
    Result := FcxDBTreeListNode.ParentKeyValue
  else begin
    Idx := Pred(TcxDBTreeList(FcxDBTreeListNode.TreeList).ColumnCount);
    while Idx >= 0 do begin
      if SameText(TcxDBTreeListColumn(TcxDBTreeList(FcxDBTreeListNode.TreeList).Columns[Idx]).DataBinding.FieldName, FieldName) then begin
        Result := TcxDBTreeList(FcxDBTreeListNode.TreeList).Columns[Idx].Values[FcxDBTreeListNode];
        Exit;
      end;
      Dec(Idx);
    end;

    Raise Exception.Create('Field '#171 + FieldName + #187' not found.');
  end;
end;

function CxGridDataRowExpression(AcxGridDataRow: TcxGridDataRow; const AExpression: string; AConditionParser: PExprParser = nil): Variant;
var
  LExprEval : TcxGridDataRowExprEvaluator;
  LParser   : TExprParser;
begin
  if AExpression.IsEmpty then
    Exit(unAssigned);

  if (AConditionParser <> nil) and (AConditionParser^ <> nil) then
    LParser := AConditionParser^
  else begin
    LParser := TExprParser.Create(TcxGridDBDataController(AcxGridDataRow.GridView.DataController).DataSet, AExpression, [], [poExtSyntax], '', nil, FieldTypeMap);
    if AConditionParser <> nil then
      AConditionParser^ := LParser;
  end;

  LExprEval := TcxGridDataRowExprEvaluator.Create(AcxGridDataRow);

  try
    LExprEval.SetData(Pointer(LParser.FilterData));
    Result := LExprEval.Evaluate;
  finally
    if AConditionParser = nil then
      LParser.Free;

    LExprEval.Free;
  end;
end;

function CxDBTreeListNodeExpression(AcxDBTreeListNode: TcxDBTreeListNode; const AExpression: string; AConditionParser: PExprParser = nil): Variant;
var
  LExprEval : TcxDBTreeListExprEvaluator;
  LParser   : TExprParser;
begin
  if AExpression.IsEmpty then
    Exit(unAssigned);

  if (AConditionParser <> nil) and (AConditionParser^ <> nil) then
    LParser := AConditionParser^
  else begin
    LParser := TExprParser.Create(TcxDBTreeList(AcxDBTreeListNode.TreeList).DataController.DataSet, AExpression, [], [poExtSyntax], '', nil, FieldTypeMap);
    if AConditionParser <> nil then
      AConditionParser^ := LParser;
  end;

  LExprEval := TcxDBTreeListExprEvaluator.Create(AcxDBTreeListNode);

  try
    LExprEval.SetData(Pointer(LParser.FilterData));
    Result := LExprEval.Evaluate;
  finally
    if AConditionParser = nil then
      LParser.Free;

    LExprEval.Free;
  end;
end;
{$ENDREGION 'GridExprEvaluators'}

{$REGION 'GridColorer'}
{ TColorGridConditions }

constructor TColorGridConditions.Create;
begin
  inherited Create(TColorGridCondition);
end;

function TColorGridConditions.GetItem(Index: Integer): TColorGridCondition;
begin
  Result := TColorGridCondition(inherited GetItem(Index));
end;

{ TColorGridItem }

constructor TColorGridItem.Create(Collection: TCollection);
begin
  inherited;
  FConditions := TColorGridConditions.Create;
end;

destructor TColorGridItem.Destroy;
begin
  FreeAndNil(FInterpritator);
  FreeAndNil(FConditions);

  inherited;
end;

procedure TColorGridItem.SetGrid(const Value: TComponent);
var
  I: Integer;
begin
  FGrid := Value;

  FreeAndNil(FInterpritator);

  for I := Pred(Collection.Count) downto 0 do
    if (Index <> I) and (TColorGridItem(Collection.Items[I]).Grid = Value) then
      Collection.Delete(I);

  FInterpritator := TBaseColorGridInterpritator.Create(Self);
end;

{ TBaseColorGridInterpritator }

class function TBaseColorGridInterpritator.Create(AOwner: TColorGridItem): TBaseColorGridInterpritator;
var
  I: Integer;
begin
  for I := 0 to Length(FInterfaces) - 1 do with FInterfaces[I] do
    if AOwner.Grid.InheritsFrom(ControlClass) then begin
      Result := InterfaceClass.InternalCreate(AOwner);
      Exit;
    end;

  Raise Exception.Create(SAbstractError);
end;

constructor TBaseColorGridInterpritator.InternalCreate(AOwner: TColorGridItem);
begin
  FOwner := AOwner;
  inherited Create;
end;

class procedure TBaseColorGridInterpritator.RegisterClass(AControlClass: TClass; AInterfaceClass: TBaseColorGridInterpritatorClass);
var
  L: Integer;
begin
  L := Length(FInterfaces);
  SetLength(FInterfaces, L + 1);
  with FInterfaces[L] do begin
    ControlClass := AControlClass;
    InterfaceClass := AInterfaceClass;
  end;
end;

{ TcxGridTableViewColorGridInterpritator }

constructor TcxGridTableViewColorGridInterpritator.InternalCreate(AOwner: TColorGridItem);
begin
  inherited;

  FMethod := TMethod(TcxGridTableView(FOwner.FGrid).Styles.OnGetContentStyle);
  TcxGridTableView(FOwner.FGrid).Styles.OnGetContentStyle := cxGridTableViewStylesGetContentStyle;
end;

destructor TcxGridTableViewColorGridInterpritator.Destroy;
var
  LcxGridGetCellStyleEvent: TcxGridGetCellStyleEvent;
begin
  if not (csDestroying in FOwner.FGrid.ComponentState) then begin
    LcxGridGetCellStyleEvent := cxGridTableViewStylesGetContentStyle;
    if TMethod(TcxGridTableView(FOwner.FGrid).Styles.OnGetContentStyle) = TMethod(LcxGridGetCellStyleEvent) then
      TcxGridTableView(FOwner.FGrid).Styles.OnGetContentStyle := TcxGridGetCellStyleEvent(FMethod);
  end;

  inherited;
end;

procedure TcxGridTableViewColorGridInterpritator.cxGridTableViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
var
  I: Integer;
begin
  if FMethod.Code <> nil then
    TcxGridGetCellStyleEvent(FMethod)(Sender, ARecord, AItem, AStyle);

  if (AItem <> nil) {and (AStyle = nil) and (ARecord is TcxGridDataRow)} and (not Sender.IsUpdateLocked) then begin
    if (FCachedCxGridDataRow <> nil) and (FCachedCxGridDataRow = ARecord) and (FCachedCxGridTableItem.VisibleIndex < AItem.VisibleIndex) then
      AStyle := FCachedCxGridStyle
    else begin
      for I := 0 to Pred(FOwner.Conditions.Count) do
        with FOwner.Conditions[I] do
          if CxGridDataRowExpression(TcxGridDataRow(ARecord), Condition, @FConditionParser) = True then begin
            AStyle := Color;
            Break;
          end;

      FCachedCxGridDataRow   := TcxGridDataRow(ARecord);
      FCachedCxGridTableItem := AItem;
      FCachedCxGridStyle     := AStyle;
    end;
  end;
end;


{ TcxDBTreeListColorGridInterpritator }

constructor TcxDBTreeListColorGridInterpritator.InternalCreate(AOwner: TColorGridItem);
begin
  inherited;

  FMethod := TMethod(TcxDBTreeList(FOwner.FGrid).Styles.OnGetContentStyle);
  TcxDBTreeList(FOwner.FGrid).Styles.OnGetContentStyle := cxDBTreeListStylesGetContentStyle;
end;

destructor TcxDBTreeListColorGridInterpritator.Destroy;
var
  LcxTLOnGetContentStyleEvent: TcxTreeListGetContentStyleEvent;
begin
  if not (csDestroying in FOwner.FGrid.ComponentState) then begin
    LcxTLOnGetContentStyleEvent := cxDBTreeListStylesGetContentStyle;
    if TMethod(TcxDBTreeList(FOwner.FGrid).Styles.OnGetContentStyle) = TMethod(LcxTLOnGetContentStyleEvent) then
      TcxDBTreeList(FOwner.FGrid).Styles.OnGetContentStyle := TcxTreeListGetContentStyleEvent(FMethod);
  end;

  inherited;
end;

procedure TcxDBTreeListColorGridInterpritator.cxDBTreeListStylesGetContentStyle(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn; ANode: TcxTreeListNode; var AStyle: TcxStyle);
var
  I: Integer;
begin                      // TcxDBTreeListColumn
  if FMethod.Code <> nil then
    TcxTreeListGetContentStyleEvent(FMethod)(Sender, AColumn, ANode, AStyle);

  if (AColumn <> nil) and (AStyle = nil) then begin
    if (FCachedCxTLNode <> nil) and (FCachedCxTLNode = ANode) and (FCachedCxTLItem.VisibleIndex < TcxDBTreeListColumn(AColumn).VisibleIndex) then
      AStyle := FCachedCxTLStyle
    else begin
      for I := 0 to Pred(FOwner.Conditions.Count) do
        with FOwner.Conditions[I] do
          if CxDBTreeListNodeExpression(TcxDBTreeListNode(ANode), Condition, @FConditionParser) = True then begin
            AStyle := Color;
            Break;
          end;

      FCachedCxTLNode   := ANode;
      FCachedCxTLItem   := AColumn;
      FCachedCxTLStyle  := AStyle;
    end;
  end;
end;

{ TGridsColorsCollection }

constructor TGridsColorsCollection.Create;
begin
  inherited Create(TColorGridItem);
end;

function TGridsColorsCollection.GetItem(Index: Integer): TColorGridItem;
begin
  Result := TColorGridItem(inherited GetItem(Index));
end;


{ TGridsColors }

constructor TGridsColors.Create(AOwner: TComponent);
begin
  FCollection := TGridsColorsCollection.Create;
  inherited;
end;

destructor TGridsColors.Destroy;
begin
  FreeAndNil(FCollection);
  inherited;
end;

procedure TGridsColors.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;

  if (Operation = opRemove) then begin
    if (FCollection <> nil) and (FCollection.Count > 0) then
      for I := 0 to FCollection.Count - 1 do
        if FCollection.Items[I].FGrid = AComponent then begin
          FCollection.Delete(I);
          Break;
        end;
  end;
end;

{ TColorGridCondition }

destructor TColorGridCondition.Destroy;
begin
  FreeAndNil(FConditionParser);
//  FreeAndNil(FConditionEvaluator);

  inherited;
end;

procedure TColorGridCondition.SetCondition(const Value: String);
begin
  FreeAndNil(FConditionParser);
//  FreeAndNil(FConditionEvaluator);

  FCondition := Value;
end;
{$ENDREGION 'GridColorer'}

{$REGION 'Lookup Controls'}
{ TSBaseCustomLookupComponentControl }

constructor TSBaseCustomLookupComponentControl.Create(AOwner: TComponent);
begin
  FSelectButtonIndex  := -1;
  FClearButtonIndex   := -1;
  FViewButtonIndex    := -1;
  FSubControlIndex    := -1;
  FParamValueLocal    := True;
//  FParamValuePrior    := unAssigned;

  FExtendedParamValues.UnknownAsNull := True;

  if Assigned(AOwner) and (AOwner is TSBaseFrm) then
    FSBaseFrm := TForm(AOwner)
  else begin
    Assert((AOwner.Owner <> nil) and (AOwner.Owner is TSBaseFrm));
    FSBaseFrm := TForm(AOwner.Owner);
  end;

  inherited;

  FDataLink             := TMDFieldDataLink.Create;
  with FDataLink do begin
    Control         := Self;
    MultiFields     := True;
    OnDataChanged   := Self.DataChanged;
//    OnFieldChanged  := Self.OnFieldChanged;
  end;
end;

(*
procedure TSBaseCustomLookupComponentControl.DataChange(Sender: TObject);
begin
  if FDataLink.DataSetActive and (DataField <> '') then
    SetParamValue(FDataLink.DataSet.FieldValues[DataField])
  else
    SetParamValue(Null)
end;
*)

procedure TSBaseCustomLookupComponentControl.DataLinkChanged;
var
  LControlStateRules: TControlStateRules;
begin
  if not (csLoading in ComponentState) and Assigned(FControl) then begin
//  if Assigned(FControl) then begin
    LControlStateRules := TControlStateRules.CreateRules(FControl, True);
    if LControlStateRules.CreatedByLinkedControl then
      with LControlStateRules do begin
        DataSource := Self.DataSource;
        FieldName  := Self.DataField;

        if Self.ReadOnly then
          ModifyState := bfcsReadOnly
        else
          ModifyState := bfcsDefaultWriteAble;
      end;

//    if Assigned(DataSource) and (not DataField.IsEmpty) then begin
//    end else if Assigned(LControlStateRules) and LControlStateRules.CreatedByLinkedControl then
//      LControlStateRules.Free;
  end;
end;

class function TSBaseCustomLookupComponentControl.DataSourceRequired: Boolean;
begin
  Result := False;
end;

destructor TSBaseCustomLookupComponentControl.Destroy;
begin
//  if Assigned(FcxCustomDataController) and (not DataSourceRequired) then
//    FcxCustomDataController.RemoveDataChangedListener(Self, ValueChanged);

  FreeAndNil(FViewButton);
  FreeAndNil(FSelectMenuSQLController);
  FreeAndNil(FDataLink);

  if Assigned(FSelectMenu) and (FSelectMenu.Owner = Self) then
    FreeAndNil(FSelectMenu);

  inherited;
end;

procedure TSBaseCustomLookupComponentControl.DoAfterExtractParamValues(var AParamValues: TNamedVariants);
begin
//
end;

procedure TSBaseCustomLookupComponentControl.DoOnSelect(const AResultValues: TNamedVariants);
begin
  ParamValue := AResultValues[FParamName];
  if Assigned(FOnLookupSelect) then
    FOnLookupSelect(Self);
end;

procedure TSBaseCustomLookupComponentControl.DoParamNameChanged;
begin
//
end;

procedure TSBaseCustomLookupComponentControl.ExtractParamValues;
var
  LParamValues, LVarItems: TNamedVariants;
begin
  if FExtractSQL.IsEmpty and FExtractScript.IsEmpty then Exit;

  LParamValues.UnknownAsNull := True;
  if (not FExtractSQL.IsEmpty) then begin
    LVarItems := TSBaseFrm(FSBaseFrm).ExecSQL(FExtractSQL, TNamedVariants.Create(FParamName, ParamValue, True), TSBaseFrm(FSBaseFrm).GetFormParamValues, Self);
    if LVarItems.Count > 0 then
      DoAfterExtractParamValues(LVarItems);

    LParamValues.Clear;
    LParamValues[FParamName] := ParamValue;
    LParamValues.AssignValues(LVarItems, amOverride);
  end else
    LParamValues[FParamName] := ParamValue;

  if (not FExtractScript.IsEmpty) then begin
    LVarItems := LParamValues;
    TSBaseFrm(FSBaseFrm).CallFunctionOut(FExtractScript, LVarItems);
    if LVarItems.Count > 0 then begin
      DoAfterExtractParamValues(LVarItems);
      LParamValues.AssignValues(LVarItems, amOverride);
    end;
  end;

  ParamValue := LParamValues[FParamName];
end;

procedure TSBaseCustomLookupComponentControl.ExtractSQLChanged;
begin
//
end;

procedure TSBaseCustomLookupComponentControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) then begin
    if (FControl <> nil) and (AComponent = FControl) then begin
      FControl := nil;
      FcxCustomEditProperties := nil;
//      FParamValueLocal := True;
    end else if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil
  end;
end;

procedure TSBaseCustomLookupComponentControl.SetControl(const Value: TComponent);
var
  LcxCustomEditProperties: TcxCustomEditProperties;
  LcxCustomObject: TObject;
  LParamValueLocal: Boolean;
  LHasSubControls: Boolean;
begin
  if FControl = Value then Exit;

  LcxCustomEditProperties := nil;
  LcxCustomObject         := nil;
  LParamValueLocal        := False;
  LHasSubControls         := False;

  if (FControl = nil) and (Value <> nil) then begin
    if (Value is TcxCustomMaskEdit) then begin
      LcxCustomEditProperties := TcxCustomMaskEdit(Value).Properties;
      LParamValueLocal := True;
    end else if (Value is TcxBarEditItem) then begin
      LcxCustomEditProperties := TcxBarEditItem(Value).Properties;
      LParamValueLocal := True;
    end else if Value is TcxGridColumn then
      LcxCustomEditProperties := TcxGridColumn(Value).Properties
    else if Value is TcxTreeListColumn then
      LcxCustomEditProperties := TcxTreeListColumn(Value).Properties
    else if Value is TcxDBEditorRow then begin
      LcxCustomObject := TcxDBEditorRow(Value).Properties;
      LcxCustomEditProperties := TcxDBEditorRowProperties(LcxCustomObject).EditProperties;
    end
    else if Value is TcxEditRepositoryItem then begin
      LcxCustomEditProperties := TcxEditRepositoryItem(Value).Properties;
      LParamValueLocal := True;
    end
    else if Value is TcxCustomMultiEditorRow then begin
      LHasSubControls := True;
    // Not needed
    end
    else if Value is TcxCustomEditorRow then begin
    // Not needed
    end
    else if Value is TcxButton then begin
    // Not needed
      TcxButton(Value).OnClick := ButtonOnClick;
    end

    else
      Raise Exception.Create('TSBaseCustomLookupComponentControl.SetControl failed: invalid value class '#171 + Value.ClassName + #187);
  end else
    Raise Exception.Create('Invald call TSBaseCustomLookupComponentControl.SetControl method');

//  Assert(Assigned(LcxCustomEditProperties));

  FControl                := Value;
  FHasSubControls         := LHasSubControls;
  FcxCustomEditProperties := LcxCustomEditProperties;
  FcxCustomObject         := LcxCustomObject;
  FParamValueLocal        := LParamValueLocal;

  if FHasSubControls and (FSubControlIndex >= 0) then
    SetSubControlIndex(FSubControlIndex)
  else if not (csLoading in ComponentState) then
    InitControl;
end;

procedure TSBaseCustomLookupComponentControl.InitControl;
begin
  if (not FHasSubControls) or (FSubControlIndex >= 0) then begin
    if FSelectButtonIndex >= 0 then
      SetSelectButtonIndex(FSelectButtonIndex);

    if FClearButtonIndex >= 0 then
      SetClearButtonIndex(FClearButtonIndex);

    if FViewButtonIndex >= 0 then
      SetViewButtonIndex(FViewButtonIndex);
  end;
end;

procedure TSBaseCustomLookupComponentControl.SetSubControlIndex(const AValue: SmallInt);
begin
  Assert(AValue >= -1);
  FSubControlIndex := AValue;

  if Assigned(FControl) and (AValue >= 0) then begin
    Assert(FHasSubControls);
    FcxCustomObject := TcxDBMultiEditorRow(FControl).Properties.Editors[AValue];
    FcxCustomEditProperties := TcxEditorRowItemProperties(FcxCustomObject).EditProperties;
    if not (csLoading in ComponentState) then
      InitControl;
  end;
end;

procedure TSBaseCustomLookupComponentControl.SetOnButtonClickEvent;
var
  LSavedMethod: TMethod;
begin
  LSavedMethod := TMethod(FcxCustomEditProperties.OnButtonClick);
  if LSavedMethod.Code = @TSBaseCustomLookupComponentControl.ButtonOnClickCx then Exit;

  if (FSavedMethod.Code = nil) or (LSavedMethod <> FSavedMethod) then
    FSavedMethod := LSavedMethod;

  FcxCustomEditProperties.OnButtonClick := ButtonOnClickCx;
end;

procedure TSBaseCustomLookupComponentControl.SetSelectButtonIndex(const Value: SmallInt);
begin
  Assert(Value >= -1);
  FSelectButtonIndex := Value;

  if Assigned(FcxCustomEditProperties) then begin
    SetOnButtonClickEvent;
    if (Value >= 0) then
      with FcxCustomEditProperties do begin
        while Buttons.Count <= Value do
          Buttons.Add;
        Buttons[Value].Kind := bkEllipsis;
      end;
  end;
end;

procedure TSBaseCustomLookupComponentControl.SetClearButtonIndex(const Value: SmallInt);
begin
  Assert(Value >= -1);
  FClearButtonIndex := Value;

  if Assigned(FcxCustomEditProperties) then begin
    SetOnButtonClickEvent;
    if (Value >= 0) then
      with FcxCustomEditProperties do begin
        while Buttons.Count <= Value do
          Buttons.Add;
        with Buttons[Value] do begin
          Kind := bkText;
          Caption := 'X';
        end;
      end;
  end;
end;

procedure TSBaseCustomLookupComponentControl.SetViewButtonIndex(const Value: SmallInt);
begin
  Assert(Value >= -1);
  FViewButtonIndex := Value;

  if Assigned(FcxCustomEditProperties) then begin
    SetOnButtonClickEvent;
    if (Value >= 0) then
      with FcxCustomEditProperties do begin
        while Buttons.Count <= Value do
          Buttons.Add;
        with Buttons[Value] do begin
          Kind := bkText;
          Caption := '->';
        end;
      end;
  end;
end;

procedure TSBaseCustomLookupComponentControl.View;
var
  LProperties: TBaseItemReaction;
begin
//  if CanModify then begin

  LProperties := TSBaseFrm(FSBaseFrm).ItemsReactions.FindItemProperties(ViewButton);
//  if not Assigned(LProperties) then LProperties := TSBaseFrm(FSBaseFrm).ItemsReactions.FindItemProperties(FControl);
  if Assigned(LProperties) then begin
    LProperties.Execute(TSBaseFrm(FSBaseFrm).GetFormParamValues, TSBaseFrm(FSBaseFrm).ConfirmMessage, Self);
  end;
//  end;
end;

procedure TSBaseCustomLookupComponentControl.AssignTo(Dest: TPersistent);
begin
  Assert(Dest is TSBaseCustomLookupComponentControl);

  with TSBaseCustomLookupComponentControl(Dest) do begin
    ReadOnly            := Self.ReadOnly;
    SelectButtonIndex   := Self.SelectButtonIndex;
    SelectMenu          := Self.SelectMenu;
    ClearButtonIndex    := Self.ClearButtonIndex;
    ViewButtonIndex     := Self.ViewButtonIndex;
    ParamName           := Self.ParamName;
    ParamValue          := Self.ParamValue;
    OnParamValueChanged := Self.OnParamValueChanged;
    OnOtherButtonClick  := Self.OnOtherButtonClick;

    DataLinkEnabled     := Self.DataLinkEnabled;
    DataSource          := Self.DataSource;
    DataField           := Self.DataField;
    OnDataFieldChanged  := Self.OnDataFieldChanged;
  end;
end;

procedure TSBaseCustomLookupComponentControl.ButtonOnClick(Sender: TObject);
begin
  Select;
end;

procedure TSBaseCustomLookupComponentControl.ButtonOnClickCx(Sender: TObject; AButtonIndex: Integer);
begin
  if AButtonIndex = FSelectButtonIndex then begin
    Select;
    TcxCustomEditCrack(Sender).DoHideEdit(True);
  end else if AButtonIndex = FClearButtonIndex then begin
    Clear;
    TcxCustomEditCrack(Sender).DoHideEdit(True);
  end else if AButtonIndex = FViewButtonIndex then begin
    View;
    TcxCustomEditCrack(Sender).DoHideEdit(True);
  end else begin
    if FSavedMethod.Code <> nil then
      TcxEditButtonClickEvent(FSavedMethod)(Sender, AButtonIndex);
    if Assigned(FOnOtherButtonClick) then
      FOnOtherButtonClick(Sender, AButtonIndex);
  end;
end;

procedure TSBaseCustomLookupComponentControl.SetSelectMenu(const AValue: TdxBarPopupMenu);
begin
  if SelectMenu <> AValue then begin
    if Assigned(FSelectMenu) and (FSelectMenu.Owner = Self) then
      FreeAndNil(FSelectMenu);

    FSelectMenu := AValue;
    FSelectMenuConfigured := False;
  end;
end;

procedure TSBaseCustomLookupComponentControl.SelectMenuSQLChanged(ASender: TObject);
begin
  FSelectMenuConfigured := False;
end;

function TSBaseCustomLookupComponentControl.GetOrCreateSelectMenuSQLController: TSBaseCustomSQLMenuController;
begin
  if not Assigned(FSelectMenuSQLController) then begin
    FSelectMenuSQLController := TSBaseCustomSQLMenuController.Create(Self);
    FSelectMenuSQLController.ItemReactionSelectNotify := OnSelectNotify;
    FSelectMenuSQLController.OnSQLChanged := SelectMenuSQLChanged;
  end;
  Result := FSelectMenuSQLController;
end;

function TSBaseCustomLookupComponentControl.GetSelectMenuSQL: TStrings;
begin
  Result := GetOrCreateSelectMenuSQLController.SQL;
end;

function TSBaseCustomLookupComponentControl.GetViewButton: TObject;
begin
  if not Assigned(FViewButton) then
    FViewButton := TObject.Create;
  Result := FViewButton
end;

procedure TSBaseCustomLookupComponentControl.SetSelectMenuSQL(AValue: TStrings);
begin
  if not FSelectMenuSQLController.SQL.IsEqualStrings(AValue) then
    GetOrCreateSelectMenuSQLController.SQL.AssignStrings(AValue);
end;

procedure TSBaseCustomLookupComponentControl.SetParamName(const AValue: String);
var
  Idx: Integer;
  LParamValue: Variant;
begin
  if FParamName <> AValue then begin
    FParamName := AValue;
    FExtendedParamValues.Clear;

    if FParamName.IsEmpty then begin
      FParamCount := 0;
      if FDataLink.DataIndepended then
        ParamValue := Null;
    end else begin
      FParamCount := 0;
      Idx := 0;
      repeat
        Inc(FParamCount);
        Idx := PosEx(';', FParamName, Succ(Idx));
      until Idx = 0;

      if FDataLink.DataIndepended and FParamValueLocal then
        if FParamCount = 1 then
          ParamValue := Null
        else begin
          LParamValue := VarArrayCreate([0, Pred(FParamCount)], varVariant);
          for Idx := 0 to Pred(FParamCount) do LParamValue[Idx] := Null;
          ParamValue := LParamValue;
        end;
    end;

    DoParamNameChanged;
  end;
end;

function TSBaseCustomLookupComponentControl.GetDataSourceActive: Boolean;
begin
  if (not FDataLink.DataIndepended) or FParamValueLocal or (DataFieldsCount = 0) then
    Result := FDataLink.Active
  else if FControl is TcxGridColumn then
    with TcxGridColumn(FControl) do
      Result := GridView.GetActive
  else if (FControl is TcxDBTreeListColumn) then
    with TcxDBTreeListColumn(FControl) do
      Result := TcxDBTreeList(TreeList).GetActive
  else if (FControl is TcxCustomRow) then
    with TcxDBVerticalGrid(TcxCustomRow(FControl).VerticalGrid) do
      Result := GetActive
  else
    Result := False
end;

function TSBaseCustomLookupComponentControl.GetParamValue: Variant;
var
  LParamItem: TStringItem;
begin
  if FParamCount = 0 then Exit(Null);

  if (not FDataLink.DataIndepended) or FParamValueLocal or (DataFieldsCount = 0) then
    Result := FDataLink.Value
  else if FControl is TcxGridColumn then
    with TcxGridColumn(FControl) do
      Result := GridView.FocusedRecordValues[FDataLink.FieldName]
  else if (FControl is TcxDBTreeListColumn) then
    with TcxDBTreeListColumn(FControl) do
      Result := TcxDBTreeList(TreeList).FocusedRecordValues[FDataLink.FieldName]
  else if (FControl is TcxCustomRow) then
    with TcxDBVerticalGrid(TcxCustomRow(FControl).VerticalGrid) do
      Result := FocusedRecordValues[FDataLink.FieldName];
//  else
//    Assert(False);

  if (DataFieldsCount = 0) or (FDataLink.FieldNameCount = FParamCount) then Exit;

  if FDataLink.FieldNameCount = 1 then
    Result := VarArrayOf([Result]);

  VarArrayRedim(Result, FParamCount - 1);
  for LParamItem in EnumStringItems(ParamName, ';') do
    if LParamItem.Index >= ParamCount then
      Result[LParamItem.Index] := FExtendedParamValues[LParamItem.Value];
end;

procedure TSBaseCustomLookupComponentControl.SetParamValue(const AValue: Variant);
var
  LParamItem: TStringItem;
  LParamValue: Variant;
  LValue: Variant;
begin
  if FParamCount = 0 then
    Raise Exception.Create('Cannot set ParamValue property while ParamCount = 0')
  else if DataFieldsCount > FParamCount then
    Raise Exception.Create('Cannot set ParamValue property while DataFieldsCount > ParamsCount')
  else begin
    if (DataFieldsCount > 0) and (DataFieldsCount < FParamCount) then begin
      LValue := VarArrayGetRangeByIndexes(AValue, 0, DataFieldsCount - 1);

      for LParamItem in EnumStringItems(ParamName, ';') do
        if LParamItem.Index >= DataFieldsCount then
          FExtendedParamValues[LParamItem.Value] := AValue[LParamItem.Index];
    end else
      LValue := AValue;


    if (not FDataLink.DataIndepended) or FParamValueLocal or (DataFieldsCount = 0) then begin
      if not VarIsEqual(FDataLink.Value, LValue) then begin
        FDataLink.Edit;
        FDataLink.Value := LValue;
//        if FControl is TcxEditRepositoryItem then
//          TcxEditRepositoryItem(FControl).Properties.RefreshNonShareable
      end;
    end else if FControl is TcxGridColumn then begin
      with TcxGridColumn(FControl).GridView do begin
        LParamValue := FocusedRecordValues[FDataLink.FieldName];
        if not VarIsEqual(LParamValue, AValue) then begin
          FocusedRecordValues[FDataLink.FieldName] := AValue;
          FDataLink.DoOnValueChanged;
        end;
      end;
    end else if (FControl is TcxDBTreeListColumn) then
      with TcxDBTreeList(TcxDBTreeListColumn(FControl).TreeList) do begin
        LParamValue := FocusedRecordValues[FDataLink.FieldName];
        if not VarIsEqual(LParamValue, AValue) then begin
          FocusedRecordValues[FDataLink.FieldName] := AValue;
          FDataLink.DoOnValueChanged;
        end;
      end
    else if (FControl is TcxCustomRow) then
      with TcxDBVerticalGrid(TcxCustomRow(FControl).VerticalGrid) do begin
        LParamValue := FocusedRecordValues[FDataLink.FieldName];
        if not VarIsEqual(LParamValue, AValue) then begin
          FocusedRecordValues[FDataLink.FieldName] := AValue;
          FDataLink.DoOnValueChanged;
        end;
      end;
  end;
end;

function TSBaseCustomLookupComponentControl.IsParamPresent: Boolean;
begin
  Result := GetDataSourceActive and VarArrayIsPresent(ParamValue)
end;

procedure TSBaseCustomLookupComponentControl.Loaded;
begin
  inherited;
  InitControl;
  DataLinkChanged;
end;

procedure TSBaseCustomLookupComponentControl.Select;
var
  LProperties: TBaseItemReaction;
  function IsItemLinksEmpty(AItemLinks: TdxBarItemLinks): Boolean;
  begin
    Result := (AItemLinks = nil) or (AItemLinks.Count = 0)
  end;
begin
  if CanModify then begin
    if SelectMenuSQLExist and (not Assigned(FSelectMenu)) then begin
      FSelectMenu := TdxBarPopupMenu.Create(Self);
      FSelectMenu.BarManager := TSBaseFrm(FSBaseFrm).BarManager;
    end;

    if Assigned(FSelectMenu) then begin
      ConfigureSelectMenu;

      if FSelectMenu.ItemLinks.Count = 0 then
        Abort
      else if (FSelectMenu.Owner = Self) and (FSelectMenu.ItemLinks.Count = 1) and IsItemLinksEmpty(TdxBarItemCrack(FSelectMenu.ItemLinks[0].Item).GetItemLinks) then
        FSelectMenu.ItemLinks[0].Item.Click
      else
        TdxBarPopupMenu(FSelectMenu).PopupFromCursorPos
    end else begin
      LProperties := TSBaseFrm(FSBaseFrm).ItemsReactions.FindItemProperties(Self);
//      if not Assigned(LProperties) then LProperties := TSBaseFrm(FSBaseFrm).ItemsReactions.FindItemProperties(FControl);
      if Assigned(LProperties) then begin
        LProperties.OnSelect.Notify := OnSelectNotify;
        LProperties.Execute(TSBaseFrm(FSBaseFrm).GetFormParamValues, TSBaseFrm(FSBaseFrm).ConfirmMessage);
      end;
    end;
  end;
end;

function TSBaseCustomLookupComponentControl.SelectMenuSQLExist: Boolean;
begin
  Result := Assigned(FSelectMenuSQLController) and (FSelectMenuSQLController.SQL.Count > 0);
end;

procedure TSBaseCustomLookupComponentControl.Clear;
begin
  if CanModify then
    if FParamCount <= 1 then
      ParamValue := Null
    else
      ParamValue := VarArrayGetRangeByIndexes(unAssigned, 0, FParamCount - 1, vaieReturnNull);
end;

procedure TSBaseCustomLookupComponentControl.ConfigureSelectMenu;
  procedure CheckMenuItem(const AItemLinks: TdxBarItemLinks);
  var
    LItemLinks: TdxBarItemLinks;
    LProperties: TBaseItemReaction;
    I: Integer;
    LItem: TdxBarItem;
  begin
    for I := 0 to AItemLinks.Count - 1 do begin
      LItem := AItemLinks[I].Item;
      if Assigned(LItem) then begin
        LItemLinks := TdxBarItemCrack(LItem).GetItemLinks;
        if Assigned(LItemLinks) and (LItemLinks.Count > 0) then
          CheckMenuItem(LItemLinks);

        LProperties := TSBaseFrm(FSBaseFrm).ItemsReactions.FindItemProperties(LItem);
        if Assigned(LProperties) then
          LProperties.OnSelect.Notify := OnSelectNotify;
      end;
    end;
  end;

begin
  if SelectMenuSQLExist then begin
    FSelectMenuSQLController.MenuComponent := FSelectMenu;
    FSelectMenuSQLController.Configure;
  end else begin
    CheckMenuItem(TdxBarPopupMenu(FSelectMenu).ItemLinks);
    FSelectMenuConfigured := True;
  end;
end;

procedure TSBaseCustomLookupComponentControl.OnSelectNotify(ASelectAction: TCallBackSelectAction; const AResultValues: TNamedVariants);
begin
  FInSelectNotify := True;
  try
    DoOnSelect(AResultValues);
  finally
    FInSelectNotify := False;
  end;
end;

procedure TSBaseCustomLookupComponentControl.ParamValueChanged;
begin
  if (not FInParamValueChanged) and not (csDestroying in ComponentState) then begin
    FInParamValueChanged := True;
    try
      if FInSelectNotify and (not FSkipExtractParamValues) then
        ExtractParamValues;

      if Assigned(FOnParamValueChanged) then
        FOnParamValueChanged(Self);
    finally
      FInParamValueChanged := False;
    end;
  end;
end;

function TSBaseCustomLookupComponentControl.GetCanModify: Boolean;
var
  LDataBinding: TcxGridItemDataBinding;

  function InheritedGetCanModify(ADataSource: TDataSource): Boolean;
  begin
    Result := ADataSource.GetInternalCanModify(False) and GetFieldsCanModify(ADataSource.DataSet, FDataLink.FieldName)
  end;
var
  LVerticalGrid: TcxCustomVerticalGrid;      
begin
  Result := (not FReadOnly) and (FParamCount > 0);
  if Result then begin
    if DataSourceRequired then
      Result := FDataLink.CanModify and FDataLink.DataSource.GetInternalCanModify(False)
    else if Result then
      if (FControl is TcxGridColumn) then begin
        LDataBinding := TcxGridColumn(FControl).DataBinding;
        if LDataBinding is TcxGridItemDBDataBinding then
          Result := InheritedGetCanModify(TcxGridItemDBDataBinding(LDataBinding).DataController.DataSource);
      end else if (FControl is TcxDBTreeListColumn) then begin
        Result := InheritedGetCanModify(TcxDBTreeListColumn(FControl).DataBinding.DataController.DataSource);
      end else if (FControl is TcxCustomRow) then begin
        LVerticalGrid := TcxCustomRow(FControl).VerticalGrid;
        if LVerticalGrid is TcxDBVerticalGrid then
          Result := InheritedGetCanModify(TcxDBVerticalGrid(LVerticalGrid).DataController.DataSource);
      end;
  end;
end;

function TSBaseCustomLookupComponentControl.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource
end;

procedure TSBaseCustomLookupComponentControl.SetDataSource(const Value: TDataSource);
begin
  if DataSource <> Value then begin
    FDataLink.DataSource := Value;
    DataLinkChanged;
  end;
end;

procedure TSBaseCustomLookupComponentControl.SetExtractSQL(const Value: String);
begin
  if FExtractSQL <> Value then begin
    FExtractSQL := Value;
    ExtractSQLChanged;
  end;
end;

procedure TSBaseCustomLookupComponentControl.DataChanged(Sender: TObject);
begin
  ParamValueChanged;
end;

function TSBaseCustomLookupComponentControl.GetDataField: String;
begin
  Result := FDataLink.FieldName;
end;

procedure TSBaseCustomLookupComponentControl.SetDataField(const Value: String);
begin
  if FDataLink.FieldName <> Value then begin
    FDataLink.FieldName := Value;
    FExtendedParamValues.Clear;
    DataLinkChanged;
  end;
end;

function TSBaseCustomLookupComponentControl.GetOnDataFieldChanged: TNotifyEvent;
begin
  Result := FDataLink.OnFieldChanged
end;

procedure TSBaseCustomLookupComponentControl.SetOnDataFieldChanged(const Value: TNotifyEvent);
begin
  FDataLink.OnFieldChanged := Value;
end;

function TSBaseCustomLookupComponentControl.GetDataFieldsCount: Integer;
begin
  Result := FDataLink.FieldNameCount;
end;

procedure TSBaseCustomLookupComponentControl.SetDataLinkEnabled(const Value: Boolean);
begin
  FDataLink.Enabled := Value
end;

function TSBaseCustomLookupComponentControl.GetDataLinkEnabled: Boolean;
begin
  Result := FDataLink.Enabled;
end;

procedure TSBaseCustomLookupComponentControl.SetReadOnly(const Value: Boolean);
begin
  if (FReadOnly <> Value) then begin
    FReadOnly := Value;
    if not FDataLink.DataIndepended then
      DataLinkChanged;
  end;
end;

function TSBaseCustomLookupComponentControl.GetParamValues(Index: String): Variant;
begin
  Result := VarArrayRead(ParamName, ParamValue, Index)
end;

function TSBaseCustomLookupComponentControl.GetSelectMenu: TdxBarPopupMenu;
begin
  if Assigned(FSelectMenu) and (FSelectMenu.Owner = Self) then
    Result := nil
  else
    Result := FSelectMenu
end;

procedure TSBaseCustomLookupComponentControl.SetParamValues(Index: String; const Value: Variant);
var
  LParamValue: Variant;
begin
  LParamValue := ParamValue;
  VarArrayOverWrite(ParamName, LParamValue, Index, Value);
  ParamValue := LParamValue;
end;

{ TSBaseLookupComponentControl }

procedure TSBaseLookupComponentControl.DoAfterExtractParamValues(var AParamValues: TNamedVariants);
var
  LIdx: Integer;
begin
  inherited;

  if AParamValues.Find(SConst_DisplayText, LIdx) then begin
    FDisplayText := VarToStr(AParamValues.Items[LIdx].Value);
    AParamValues.Delete(LIdx);
  end;
end;

procedure TSBaseLookupComponentControl.DoOnSelect(const AResultValues: TNamedVariants);
var
  LIdx:Integer;
begin
  if AResultValues.Find(SConst_DisplayText, LIdx) then begin
    SetDisplayText(VarToStr(AResultValues.Items[LIdx].Value));
    FSkipExtractDisplayText := True;
  end;

  try
    inherited;
  finally
    FSkipExtractDisplayText := False;
  end;
end;

procedure TSBaseLookupComponentControl.SetDisplayText(const AValue: String; AUserAction: Boolean);
begin
  if FControl <> nil then begin
    if (FControl is TcxCustomButtonEdit) then
      TcxCustomButtonEdit(FControl).Text := AValue
    else if (FControl is TcxBarEditItem) then
      TcxBarEditItem(FControl).EditValue := AValue
    else if (FControl is TcxGridColumn) then begin
      if AUserAction then
        with TcxGridColumn(FControl) do begin
          //DataBinding.DataController.Edit;
          FocusedValue := AValue;
        end
    end else if (FControl is TcxDBTreeListColumn) then begin
      if AUserAction then
        with TcxDBTreeListColumn(FControl) do begin
          //DataBinding.DataController.Edit;
          FocusedValue := AValue;
        end
    end else if (FControl is TcxCustomRow) then begin
      if AUserAction then
        if FcxCustomObject is TcxCustomEditorRowProperties then
          TcxCustomEditorRowProperties(FcxCustomObject).FocusedValue := AValue;
    end else
      Raise Exception.Create(SAbstractError);
  end;
end;

procedure TSBaseLookupComponentControl.ExtractDisplayText;
begin
  FDisplayText := EmptyString;
  if IsParamPresent then
    ExtractParamValues;

  SetDisplayText(FDisplayText);
end;

procedure TSBaseLookupComponentControl.InitControl;
begin
  inherited;
  if not IsParamPresent then
    SetDisplayText(EmptyString, False);
end;

procedure TSBaseLookupComponentControl.SetEmptyString(const Value: String);
begin
  FEmptyString := Value;
  if (not IsParamPresent) then
    SetDisplayText(EmptyString, False);
end;

procedure TSBaseLookupComponentControl.DoParamNameChanged;
begin
  inherited;
  if not IsParamPresent then
    SetDisplayText(EmptyString, False);
end;

procedure TSBaseLookupComponentControl.ParamValueChanged;
begin
  if not (csDestroying in ComponentState) then
    try
      if not (FSkipExtractDisplayText) then
        ExtractDisplayText;
    finally
      FSkipExtractParamValues := True;
      try
        inherited;
      finally
        FSkipExtractParamValues := True;
      end;
    end;
end;

{ TSBaseLookupDataSetComponentControl }

procedure TSBaseLookupDataSetComponentControl.AssignTo(Dest: TPersistent);
begin
  Assert(Dest is TSBaseLookupDataSetComponentControl);

  inherited;
  with TSBaseCustomLookupComponentControl(Dest) do begin
    LookupSource  := Self.LookupSource;
  end;
end;

function TSBaseLookupDataSetComponentControl.GetLookupDataSet: TDataSet;
begin
  Result := nil;
  if Assigned(FLookupSource) then begin
    Result := FLookupSource.DataSet;
    if Assigned(Result) then
      Result := Result.Source;
  end;
end;

procedure TSBaseLookupDataSetComponentControl.ParamValueChanged;
var
  LDataSet: TDataSet;
  LDataSetProvider: TSQLDataSetProvider absolute LDataSet;
  LStateBookmark: TDataSourceStateBookmark;
  LUnknownFields: String;
  LParamValues: Variant;
begin
  try
    LDataSet := GetLookupDataSet {.Source};
    if Assigned(LDataSet) then begin
      if (not FParamName.IsEmpty) then begin
        LParamValues := ParamValue;
        if VarArrayIsPresent(LParamValues) then begin
          LDataSet.Fields.GetValues(FParamName, @LUnknownFields);
          if LUnknownFields.IsEmpty and (not LDataSet.IsEmpty) and LDataSet.Locate(FParamName, LParamValues, []) then Exit;

          LDataSet.BeginUpdate(LStateBookmark);
          try
            LDataSet.Close;
            LDataSetProvider.Params.SetValues(FParamName, LParamValues);
            LDataSet.Open;
          finally
            LDataSet.EndUpdate(LStateBookmark, [sbrDisableCount], [sbrKeyFieldValues, sbrRecordIndex]);
          end;
        end else if not FLookupAlwaysActive then
          LDataSet.Close;
      end;
    end;
  finally
    inherited;
  end;
end;

procedure TSBaseLookupDataSetComponentControl.SetLookupSource(const Value: TDataSource);
begin
  FLookupSource := Value;
end;

{ TSBaseCustomDBLookupComponentControl }

constructor TSBaseCustomDBLookupComponentControl.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink.Enabled := False;
end;

class function TSBaseCustomDBLookupComponentControl.DataSourceRequired: Boolean;
begin
  Result := True
end;

procedure TSBaseCustomDBLookupComponentControl.ExtractSQLChanged;
begin
  inherited;
  FDataLink.Enabled := (not FExtractSQL.IsEmpty);
end;

{ TSBaseDBLookupComponentControl }

class function TSBaseDBLookupComponentControl.DataSourceRequired: Boolean;
begin
  Result := True;
end;

{ TSBaseDBLookupDataSetComponentControl }

class function TSBaseDBLookupDataSetComponentControl.DataSourceRequired: Boolean;
begin
  Result := True
end;
{$ENDREGION 'Lookup Controls'}

{$REGION 'FASTSCRIPT_RTTI'}
{$IFDEF FASTSCRIPT_RTTI}
{ TfsFunctions_SBaseControls }

constructor TfsFunctions_SBaseControls.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseCustomSQLMenuController, TSBaseCustomSQLMenuController.ClassParent.ClassName) do begin
    AddMethod('procedure Configure', Call_TSBaseCustomSQLMenuController_Method);
  end;

  with AScript.AddClass(TSBaseCustomLookupComponentControl, TSBaseCustomLookupComponentControl.ClassParent.ClassName) do begin
    AddIndexProperty('ParamValues', DelphiTypeString, DelphiTypeVariant, Call_TSBaseCustomLookupComponentControl_Method);
    AddMethod('procedure ExtractParamValues', Call_TSBaseCustomLookupComponentControl_Method);

    AddProperty('CanModify', DelphiTypeVariant, Get_TSBaseCustomLookupComponentControl_Property);
    AddProperty('ParamValue', DelphiTypeVariant, Get_TSBaseCustomLookupComponentControl_Property, Set_TSBaseCustomLookupComponentControl_Property);

    AddEvent('OnParamValueChanged', TfsNotifyEvent);
    AddEvent('OnLookupSelect', TfsNotifyEvent);
    AddEvent('OnOtherButtonClick', TfsCxEditButtonClickEvent);
    AddEvent('OnDataFieldValueChanged', TfsNotifyEvent);
  end;

  with AScript.AddClass(TSBaseLookupComponentControl, TSBaseLookupComponentControl.ClassParent.ClassName) do begin
    AddMethod('procedure ExtractDisplayText', Call_TSBaseLookupComponentControl_Method);
  end;
end;

function TfsFunctions_SBaseControls.Call_TSBaseCustomSQLMenuController_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'CONFIGURE' then
    TSBaseCustomSQLMenuController(Instance).Configure;
end;

function TfsFunctions_SBaseControls.Call_TSBaseCustomLookupComponentControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'PARAMVALUES.GET' then
    Result := TSBaseCustomLookupComponentControl(Instance).ParamValues[Caller.Params[0]]
  else if MethodName = 'PARAMVALUES.SET' then
    TSBaseCustomLookupComponentControl(Instance).ParamValues[Caller.Params[0]] := Caller.Params[1]
  else if MethodName = 'EXTRACTPARAMVALUES' then
    TSBaseCustomLookupComponentControl(Instance).ExtractParamValues
end;

function TfsFunctions_SBaseControls.Get_TSBaseCustomLookupComponentControl_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'PARAMVALUE' then
    Result := TSBaseLookupComponentControl(Instance).ParamValue
  else if PropName = 'CANMODIFY' then
    Result := TSBaseLookupComponentControl(Instance).CanModify
end;

procedure TfsFunctions_SBaseControls.Set_TSBaseCustomLookupComponentControl_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
  if PropName = 'PARAMVALUE' then
    TSBaseLookupComponentControl(Instance).ParamValue := Value
end;

function TfsFunctions_SBaseControls.Call_TSBaseLookupComponentControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'EXTRACTDISPLAYTEXT' then
    TSBaseLookupComponentControl(Instance).ExtractDisplayText
end;

{$ENDIF}
{$ENDREGION 'FASTSCRIPT_RTTI'}

{ TcxStyleHelper }

{$WARNINGS OFF}
procedure TcxStyleHelper.AssignStyleTo(ACanvas: TcxCanvas);
var
  LTextColor: TColor;
begin
  if svFont in AssignedValues then begin
    LTextColor := ACanvas.Font.Color;
    ACanvas.Font := Font;
  end;

  if svTextColor in AssignedValues then
    ACanvas.Font.Color := TextColor
  else if svFont in AssignedValues then
    ACanvas.Font.Color := LTextColor;

  if svColor in AssignedValues then
    ACanvas.Brush.Color := Color;
end;
{$WARNINGS ON}

{ TSBaseCustomSQLMenuController }

procedure TSBaseCustomSQLMenuController.UnConfigure;
begin
  FClickableCount := 0;

  FConfigured := False;
  FParams.Clear;

  FRoot.ItemLinks.Clear;
  FRoot.Enabled := True;

  if Assigned(FItemLinks) then
    FItemLinks.Clear;

  if Assigned(FItems) then
    FItems.Clear;
end;

procedure TSBaseCustomSQLMenuController.Configure(AReconfigure: Boolean; ARaiseIfNotReady: Boolean);
var
  LQuery: TDataSet;
  LQueryInterface: IBaseDataSetProvider;
  LIdField: TField;
  LBeginGroupField: TField;

  LEnabledField, LDefaultField: TField;
  LCaptionField, LImageIndexField, LShortCutField: TField;

  LConfirmationField, LSQLTextField: TField;
  LScriptProcedureField, LScriptConstantsField, LScriptParamsField: TField;
  LClassNameField, LClassConstantsField, LClassParamsField: TField;
  LOnSelectSQLTextField, LOnSelectScriptProcedureField: TField;

  LClickableItem : TSBaseCustomSQLMenuButton;

  procedure CreateMenuLevel(AItemLinks: TdxBarItemLinks; AParent: TSBaseCustomSQLMenuButton);
  var
    LEnabled, LFound: Boolean;
    LdxBarItem: TSBaseCustomSQLMenuButton;
    I: Integer;
    LParent: Variant;
    LDefault: TSBaseCustomSQLMenuButton;

    procedure AddItemReaction;
    begin
      with TSBaseFrm(FSBaseFrm).ItemsReactions.Add do begin
        Item := LdxBarItem;
        OnClick.Reaction := biraCustomForm;
        if Assigned(LConfirmationField)     then OnClick.Confirmation := LConfirmationField.AsString;
        if Assigned(LSQLTextField)          then OnClick.SQL.Text := LSQLTextField.AsString;
        if Assigned(LScriptProcedureField)  then OnClick.ScriptProcedure := LScriptProcedureField.AsString;
        if Assigned(LScriptConstantsField)  then OnClick.ScriptConstants.AsClrTParams := LScriptConstantsField.AsString;
        if Assigned(LScriptParamsField)     then OnClick.ScriptParams := LScriptParamsField.AsString;

        if Assigned(LClassNameField)        then OnClick.ClassName := LClassNameField.AsString;
        if Assigned(LClassConstantsField)   then OnClick.ClassConstants.AsClrTParams := LClassConstantsField.AsString;
        if Assigned(LClassParamsField)      then OnClick.ClassParams := LClassParamsField.AsString;

        if Assigned(LOnSelectSQLTextField) then OnSelect.SQL.Text := LOnSelectSQLTextField.AsString;
        if Assigned(LOnSelectScriptProcedureField) then OnSelect.ScriptProcedure := LOnSelectScriptProcedureField.AsString;
        OnSelect.Notify := FItemReactionSelectNotify;
      end;

      LClickableItem := LdxBarItem;
    end;
  begin
//    if Assigned(AParent) then
//    else
//      LParent := Null;

    LEnabled := False;
    LParent  := AParent.FId;
    LDefault := nil;

    LFound := LQuery.Locate(SConst_Parent_Id, LParent, []);
    while LFound do begin
      LdxBarItem := TSBaseCustomSQLMenuButton.Create(Self);
      FItems.Add(LdxBarItem);

      LdxBarItem.FId := LIdField.AsVariant;
      LdxBarItem.FParent := AParent;
      LdxBarItem.Caption := LCaptionField.AsString;
      if Assigned(LImageIndexField) then LdxBarItem.ImageIndex := LImageIndexField.AsInteger;
      if Assigned(LShortCutField) and (not LShortCutField.IsNull) then LdxBarItem.ShortCut := TextToShortCut(LShortCutField.AsString);

      if Assigned(LEnabledField) then LdxBarItem.Enabled := LEnabledField.AsBoolean;
      if LdxBarItem.Enabled then
        LEnabled := True;

      if VarIsPresent(LQuery.Lookup(SConst_Parent_Id, LIdField.AsVariant, SConst_Id)) then begin
        LdxBarItem.Tag := LQuery.RecNo;
      end else begin
        if LdxBarItem.Enabled then
          AddItemReaction;
        Inc(FClickableCount);
      end;

      if Assigned(LDefaultField) and LDefaultField.AsBoolean then
        LDefault := LdxBarItem;

      LdxBarItem.BarManager := AItemLinks.BarManager;
      with AItemLinks.Add do begin
        Item := LdxBarItem;
        BeginGroup := Assigned(LBeginGroupField) and LBeginGroupField.AsBoolean;
      end;

      LFound := LQueryInterface.LocateEx(SConst_Parent_Id, LParent, [bloNext]);
    end;

    for I := 0 to AItemLinks.Count - 1 do
      with AItemLinks.Items[I] do
        if TSBaseCustomSQLMenuButton(Item).Tag > 0 then begin
          if TSBaseCustomSQLMenuButton(Item).Enabled then begin
            LQuery.RecNo := TSBaseCustomSQLMenuButton(Item).Tag;
            CreateMenulevel(TSBaseCustomSQLMenuButton(Item).ItemLinks, TSBaseCustomSQLMenuButton(Item));
          end else
            TSBaseCustomSQLMenuButton(Item).ListItems.Add(SConst_Default);
        end;

    if Assigned(LDefault) and LDefault.Enabled and (Assigned(LDefault.FDefault) or (LDefault.Tag = 0)) then
      AParent.FDefault := LDefault
    else if (not LEnabled) then
      AParent.Enabled := False;
  end;

var
  LParams: TNamedVariants;
  LCached: Boolean;
begin
  if not (Assigned(FItemLinks) and (FSQL.Count > 0)) then
    if ARaiseIfNotReady then
      Raise Exception.Create(IsEmpty(Name, ClassName) + ' is not completly defined')
    else
      Exit;

  LParams := TSBaseFrm(FSBaseFrm).PrepareParameters
             (
              ParseSQLParams(FSQL.Text),
              TNamedVariants.Create(False),
              TSBaseFrm(FSBaseFrm).GetFormParamValues,
              FParent
             );

  if AReconfigure or (not FConfigured) or (LParams <> FParams) then begin
    UnConfigure;

    FParams     := LParams;
    if not Assigned(FItems) then
      FItems := TObjectList<TdxBarItem>.Create(True);

    LCached := TSQLStrings(FSQL).Cached;
    if LCached then
      LQuery := TSBaseFrm(FSBaseFrm).OpenSQLPublicQuery(FSQL.Text, FParams)
    else
      LQuery := TSBaseFrm(FSBaseFrm).OpenSQLQuery(FSQL.Text, FParams);

    LQueryInterface := (LQuery as IBaseDataSetProvider);
    try
      if LQuery.IsEmpty then Abort;

      LIdField              := LQuery.FieldByName(SConst_Id);

      LCaptionField         := LQuery.FieldByName(SConst_Caption);
      LImageIndexField      := LQuery.FindField(SConst_ImageIndex);
      LShortCutField        := LQuery.FindField(SConst_ShortCut);

      LEnabledField         := LQuery.FindField(SConst_Enabled);
      LDefaultField         := LQuery.FindField(SConst_Default);

      LBeginGroupField      := LQuery.FindField(SConst_BeginGroup);
      LConfirmationField    := LQuery.FindField(SConst_Confirmation);
      LSQLTextField         := LQuery.FindField(SConst_SQLText);
      LScriptProcedureField := LQuery.FindField(SConst_ScriptProcedure);
      LScriptConstantsField := LQuery.FindField(SConst_ScriptConstants);
      LScriptParamsField    := LQuery.FindField(SConst_ScriptParams);
      LClassNameField       := LQuery.FindField(SConst_ClassName);
      LClassConstantsField  := LQuery.FindField(SConst_ClassConstants);
      LClassParamsField     := LQuery.FindField(SConst_ClassParams);

      LOnSelectSQLTextField := LQuery.FindField(SConst_OnSelect + '.' + SConst_SQLText);
      LOnSelectScriptProcedureField := LQuery.FindField(SConst_OnSelect + '.' + SConst_ScriptProcedure);

      LClickableItem := nil;
      CreateMenulevel(FItemLinks, FRoot);
      if FClickableCount = 1 then
        FRoot.FDefault := LClickableItem;

      FConfigured := True;
    finally
      if LCached then
        TSBaseFrm(FSBaseFrm).CloseSQLPublicQuery
      else
        TSBaseFrm(FSBaseFrm).CloseSQLQuery;
    end;
  end;
end;

constructor TSBaseCustomSQLMenuController.Create(AOwner: TComponent);
begin
  Assert(Assigned(AOwner));
  FParent := AOwner;

  PrivateCreate;
  inherited;
end;

constructor TSBaseCustomSQLMenuController.CustomCreate(AOwner: TForm);
begin
  Assert(Assigned(AOwner));
  FParent := AOwner;

  PrivateCreate;
  inherited Create(nil);
end;

procedure TSBaseCustomSQLMenuController.PrivateCreate;
begin
  FRoot := TSBaseCustomSQLMenuButton.Create(Self);
  FSBaseFrm := TForm(FParent.FindOwner(TSBaseFrm));

  FSQL := TSQLStrings.Create;
  TStringList(FSQL).OnChange := SQLChange;
end;

destructor TSBaseCustomSQLMenuController.Destroy;
begin
  UnConfigure;
  FItems.Free;
  FSQL.Free;
  inherited;
end;

function TSBaseCustomSQLMenuController.GetActive: Boolean;
begin
  Result := FActive or FConfigured;
end;

procedure TSBaseCustomSQLMenuController.SetActive(const AValue: Boolean);
begin
  if FActive <> AValue then begin
    FActive := AValue;
    if FActive then
      Configure(True, False)
    else
      UnConfigure;
  end;
end;

function TSBaseCustomSQLMenuController.GetSQL: TStrings;
begin
  if Assigned(Self) then
    Result := FSQL
  else
    Result := nil
end;

procedure TSBaseCustomSQLMenuController.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FMenuComponent) then
    MenuComponent := nil;
end;

function TSBaseCustomSQLMenuController.DefaultItem: TSBaseCustomSQLMenuButton;
begin
  Result := FRoot.FDefault;
end;

procedure TSBaseCustomSQLMenuController.OnPopup(Sender: TObject);
begin
  Configure
end;

procedure TSBaseCustomSQLMenuController.SetSQL(AValue: TStrings);
begin
  FSQL.AssignStringsIfDifferent(AValue);
end;

procedure TSBaseCustomSQLMenuController.SQLChange(Sender: TObject);
begin
  FConfigured := False;
  FParams.Clear;
  if Assigned(FOnSQLChanged) then
    FOnSQLChanged(Self);
  if FActive then
    Configure(True, False)
end;

procedure TSBaseCustomSQLMenuController.SetMenuComponent(const AValue: TdxBarComponent);
//var
//  LPopupMenu: TComponent;
begin
  if AValue <> FMenuComponent then begin
    if Assigned(AValue) then begin
      // TdxBarCustomPopupMenu
      if (AValue is TdxBarCustomPopupMenu) then begin
        FMenuComponent := AValue;
        FItemLinks := TdxBarCustomPopupMenu(AValue).ItemLinks;
      end
      // TdxBarItem
      else if ((AValue is TdxBarItem) and (TdxBarItemCrack(AValue).GetItemLinks <> nil)) then begin
        FMenuComponent := AValue;
        FItemLinks     := TdxBarItemCrack(AValue).GetItemLinks;
      end
      // Unknown
      else begin
        FMenuComponent  := nil;
        FItemLinks      := nil;
      end;
    end else begin
      FMenuComponent  := nil;
      FItemLinks      := nil;
    end;
    FConfigured := False;

    if FActive then
      Configure(True, False)
  end;
end;

procedure TSBaseCustomSQLMenuController.SetParentComponent(AValue: TComponent);
begin
  if FParent <> AValue then begin
    //FConfigured := False;
    if Assigned(AValue) then begin
      FParent := AValue;
      if (not Assigned(FMenuComponent)) and (AValue is TdxBarComponent) then
        MenuComponent := TdxBarComponent(AValue);
    end else
      FParent := Owner;
  end;
end;

{ TSynEditInterface }

function TSynEditInterface.GetReadOnly: Boolean;
begin
  Result := TCustomSynEdit(FControl).ReadOnly
end;

procedure TSynEditInterface.SetReadOnly(const Value: Boolean);
begin
  TCustomSynEdit(FControl).ReadOnly := Value;
end;

{ TDBSynEditInterface }

function TDBSynEditInterface.FieldCanModify: Boolean;
begin
  Result := Assigned(TDBSynEdit(FControl).Field) and TDBSynEdit(FControl).Field.CanModify;
end;

function TDBSynEditInterface.FieldDepended: Boolean;
begin
  Result := True
end;

function TDBSynEditInterface.GetDataSource: TDataSource;
begin
  Result := TDBSynEdit(FControl).DataSource;
end;

{ TCustomButtonInterface }

procedure TCustomButtonInterface.DrawState(AReadOnly: Boolean; const AColor: TcxStyle);
begin
  TCustomButton(FControl).Enabled := (not AReadOnly);
end;

initialization
  RegisterClass(TGridsColors);
  TBaseColorGridInterpritator.RegisterClass(TcxGridTableView, TcxGridTableViewColorGridInterpritator);
  TBaseColorGridInterpritator.RegisterClass(TcxDBTreeList, TcxDBTreeListColorGridInterpritator);

  RegisterClass(TControlStateRules);

  RegisterClass(TSBaseCustomSQLMenuController);
  RegisterClass(TSBaseCustomDBLookupComponentControl);
  RegisterClasses([TSBaseLookupComponentControl, TSBaseDBLookupComponentControl]);
  RegisterClasses([TSBaseLookupDataSetComponentControl, TSBaseDBLookupDataSetComponentControl]);

  TCustomControlInterface.RegisterClass(TcxCustomGridView, TcxCustomGridViewInterface);
  TCustomControlInterface.RegisterClass(TcxCustomTreeList, TcxCustomTreeListInterface);
  TcxCustomVerticalGridInterface.RegisterClass(TcxDBVerticalGrid, TcxCustomVerticalGridInterface);
  TCustomControlInterface.RegisterClass(TcxCustomCheckBox, TcxCustomCheckBoxInterface);
  TCustomControlInterface.RegisterClass(TcxCustomGroupBox, nil);
  TCustomControlInterface.RegisterClass(TcxCustomComboBox, TcxCustomDropDownEditInterface);
  TCustomControlInterface.RegisterClass(TcxCustomMDLookupTreePopupEdit, TcxCustomDropDownEditInterface);
  TCustomControlInterface.RegisterClass(TcxCustomEdit, TcxCustomEditInterface);
  TCustomControlInterface.RegisterClass(TcxContainer, TcxContainerInterface);

  TCustomControlInterface.RegisterClass(TCustomButton, TCustomButtonInterface);

  TCustomControlInterface.RegisterClass(TSynEdit, TSynEditInterface);
  TCustomControlInterface.RegisterClass(TDBSynEdit, TDBSynEditInterface);

  dxBarRegisterItem(TSBaseCustomSQLMenuButton, TdxMDBarButtonControl, True);

{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_SBaseControls);
{$ENDIF}

finalization
  UnRegisterClass(TGridsColors);
  UnRegisterClass(TControlStateRules);

  UnRegisterClass(TSBaseCustomSQLMenuController);
  UnRegisterClass(TSBaseCustomDBLookupComponentControl);
  UnRegisterClasses([TSBaseLookupComponentControl, TSBaseDBLookupComponentControl]);
  UnRegisterClasses([TSBaseLookupDataSetComponentControl, TSBaseDBLookupDataSetComponentControl]);

  dxBarUnregisterItem(TSBaseCustomSQLMenuButton);

{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TfsFunctions_SBaseControls);
{$ENDIF}

end.




