unit SBaseXXViewForm;
{$I config.inc}

interface

uses
  Types, Generics.Collections,
  SBaseForm, SBaseItemReactions, SBaseControls,
  NamedVariables, SBaseProgress, SBaseSelectDatesPeriodForm,
  SBaseDataModule, SBaseDataHelperInternal, SBaseDataSourceControls, SBaseDataChanges,

{$IFDEF FASTSCRIPT_RTTI}
  fs_iinterpreter, fs_ievents, fs_iextctrlsrtti,
{$ENDIF}

  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, dxBar, DB,
  StrUtils, FMTBcd, LayoutPanel,
  ExtCtrls, DBConsts, DateUtils, dxStatusBar, dxMDStatusBar, cxGraphics, cxControls,
  dxBarExtDBItems, cxEdit, cxSpinEdit, cxButtonEdit, StdCtrls, cxClasses, cxLabel,
  cxBarEditItem, cxDBLookupComboBox, cxCheckBox, cxUserCheckBox, cxGroupBox, cxRadioGroup,
  cxTextEdit, cxTimeEdit, cxCalendar, Buttons, cxTL, cxDBTL, ImgList, cxMDLookupTreeComboBox,
  cxCheckLookupComboBox, cxContainer, cxSplitter, cxLookAndFeels,
  cxLookAndFeelPainters, cxFileEdit, dxBevel, System.Actions, ActnList,
  dxMDBarButton, dxMDBar, dxSkinsCore, dxSkinsDefaultPainters;

type
  TSBaseViewFilter = class;
  TSBaseViewFilterDate = class;
  TSBaseViewFilterText = class;
  TSBaseViewFilterDatesPeriod = class;

  TSBaseXXViewFrm = class(TSBaseFrm)
    StatusBar: TdxMDStatusBar;

    mnSelect: TdxBarButton;
    mnTake: TdxBarButton;

    mnNew: TdxMDBarButton;
    mnView: TdxMDBarButton;
    mnEdit: TdxMDBarButton;
    mnCopy: TdxMDBarButton;
    mnMarkAsDeleted: TdxBarButton;
    mnDelete: TdxBarButton;

    PopupMenu: TdxBarPopupMenu;
    mnFitToWidth: TdxBarButton;
    mnRefresh: TdxBarButton;
    mnProperties: TdxBarButton;
    DataSource: TDataSource;
    mnFitToScreen: TdxBarButton;
    PanelGrid: TcxGroupBox;
    mnFilter: TdxBarButton;
    mnDetails: TdxBarButton;
    PanelDetails: TcxGroupBox;
    SplitterDetails: TcxSplitter;
    DetailSource: TDataSource;
    mnSubmit: TdxMDBarButton;
    RefreshTimer: TTimer;
    mnFitToHeight: TdxBarButton;
    mnPrint: TdxBarButton;
    TimerDetails: TTimer;
    ToolBar: TdxBar;
    mnRefreshRecord: TdxBarButton;
    mnFormClose: TdxBarButton;
    mnGroupBox: TdxBarButton;
    mnGroupingClear: TdxBarButton;
    mnExpandAll: TdxBarButton;
    mnCollapseAll: TdxBarButton;
    DataSetProvider: TSQLDataSetProvider;
    DetailSetProvider: TSQLDataSetProvider;
    PanelMain: TcxGroupBox;
    PopupMenuDetailsPosition: TdxBarPopupMenu;
    mnDetailsBottom: TdxBarButton;
    mnDetailsRight: TdxBarButton;
    PanelData: TcxGroupBox;
    FilterBar: TdxBar;
    dxBarDockControl: TdxMDBarDockControl;

    procedure WMDatesPeriod(var Msg: TMessage); message SBaseForm.WM_DATESPERIOD;
    procedure WMFilter(var Msg: TMessage); message SBaseForm.WM_FILTER;
    procedure WMPrint(var Msg: TMessage); message SBaseForm.WM_PRINT;
    procedure WMProperties(var Msg: TMessage); message SBaseForm.WM_PROPERTIES;

    procedure mnRefreshClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
    procedure FormShow(Sender: TObject); override;
    procedure mnFitToScreenClick(Sender: TObject);
    procedure mnFilterClick(Sender: TObject);
    procedure mnSubmitClick(Sender: TObject);
    procedure DoCancelRequest(Sender: TObject = nil);
    procedure mnDetailsClick(Sender: TObject);

    procedure mnSelectClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); dynamic;

    function  mnTakeOnGetEnabled(Sender: TObject): Boolean;
    procedure mnTakeClick(Sender: TObject); dynamic;

    procedure DataSetBeforeOpen(DataSet: TDataSet); virtual;
    procedure DataSetAfterOpen(DataSet: TDataSet); virtual;
    procedure DataSetAfterRefresh(DataSet: TDataSet); virtual;
    procedure DataSetAfterClose(DataSet: TDataSet); virtual;
    procedure DataSetNewRecord(DataSet: TDataSet); dynamic;

    procedure DBGridDataChanged(Sender: TObject);
//    procedure DBGridXXKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure mnExcelClick(Sender: TObject);
    procedure TimerTimerDetails(Sender: TObject);
    procedure mnFormCloseClick(Sender: TObject);

    procedure DataSetProviderThreadOnPrint(ADataSetProvider: TCustomSQLDataSetProvider; const AMessages: TStringList);
//    procedure DataSetProviderThreadOnValidate(ADataSetProvider: TCustomSQLDataSetProvider; ADataSet: TInternalQuery; var AValid: Boolean);
    procedure DataSetProviderThreadOnValidate(ADataSetProvider: TCustomSQLDataSetProvider; ADataSet: TInternalQuery; AMultiDataSetIndex: Word; var AValid: Boolean);
    procedure DataSetProviderThreadAfterComplete(ADataSetProvider: TCustomSQLDataSetProvider; ATerminateReason: TDataSetThreadResult);
    procedure DataSetProviderThreadOnError(ADataSetProvider: TCustomSQLDataSetProvider; const AException: string; var ADone: Boolean);
    procedure DataSetProviderOnBeginUpdate(ADataSet: TCustomSQLDataSetProvider; var ADataSourceStateBookmark: TDataSourceStateBookmark); dynamic; abstract;
    procedure DataSetProviderOnEndUpdate(ADataSet: TCustomSQLDataSetProvider; const ADataSourceStateBookmark: TDataSourceStateBookmark); dynamic; abstract;

    procedure mnDetailsRightClick(Sender: TObject);
    procedure mnDetailsBottomClick(Sender: TObject);
    procedure PanelDetailsResize(Sender: TObject);
    procedure RefreshTimerTimer(Sender: TObject);
  strict private
    FDataSetActive: Boolean;
{$IFDEF FASTSCRIPT_RTTI}
    FBeforeOpenDataSet, FDBGridAfterDataSetOpen, FDBGridAfterDataSetClose, FDBGridAfterFocusedRecordChanged,
    FOpenDetails, FCloseDetails, FInternalSetFilter: TfsProcVariable;
{$ENDIF}
  private
    { Private declarations }
    FAutoFilterItems          : TList<TSBaseViewFilter>;
    FAutoLocalFilter          : Boolean;
    FFilterUnderConstruction  : Boolean;
    FCollisions               : Boolean;

    FRecordCountPrefix          : String;
    FExportToExcelInNativeFormat : Boolean;

    FThreadOpen               : Boolean;

    FLoadedRecordsPrivileges  : Word;
    FRecordsPrivilegesStored  : Boolean;

//    FOpenWithReInit    : Boolean;
    FAutoRefresh       : Boolean;
    FRefreshTimeout    : Word;
    FBeforOpenTime     : TTime;

    FInitGridKeyValues : Variant;

    FDetailsWidth   : Integer;
    FDetailsHeight  : Integer;
    FDetailsUpdating: Boolean;

    procedure SetDataSetActive(const Value: Boolean);
    procedure SetFilterChanged(const Value: Boolean);
    procedure SetCollisions(const Value: Boolean);
  protected
    FRecordsPrivileges  : Word;
    FOnSelectFilter     : TSBaseViewFilter;
    FDeletedCondition   : String;
    FDetailsOpened      : Boolean;

    class function AlwaysSelect: Boolean; dynamic;
    procedure SetRecordsPrivileges(const AValue: Word); virtual;

    procedure InternalCreate  ; override;
    procedure InternalPrepareParams; override;
    procedure InternalInit    ; override;
    procedure InternalPostInit; override;
    procedure InternalClose(AFinalization: Boolean); override;

    procedure InternalGetParameters(ALevel: UInt16; var AParameters: TNamedVariants; var ACollisions: Boolean); override;
    procedure InternalGetResults(var AParameters: TNamedVariants; var ACollisions: Boolean); dynamic;

    function  InternalGetLocalFilter(var AFilter: TNamedVariants): String; dynamic;
    procedure InternalSetFilter; dynamic;

    procedure InternalSelect(ACloseForm: Boolean); dynamic;

    procedure GetCaptionParamByName(const AParamName: String; var ADisplayText: String; var AFound: Boolean); override;
    procedure SetMDIToolButtons(AValue: TMDIToolButtons); override;

    procedure AfterSkinChanged; override;
    procedure AfterDataChanged(const ATablesChanges: TTablesChanges); override;

    { Предлагаю открывать ДатаСет только через эту процедуру }
    procedure OpenDataSet; dynamic;
    { А Закрывать через эту }
    procedure CloseDataSet; dynamic;

    { Стандартное действие при изменении условий фильтрации }
    procedure AfterFilterChanged(AOpenIfPosible: Boolean = False); dynamic;
    { Покажем кол-во записей }
    procedure ShowRecordCount; virtual;

    { Что-то происходит при смене фокусированной записи }
    procedure DBGridAfterFocusedRecordChanged(ARefresh: Boolean);
    { Откроем Detail-DataSet }
    procedure ShowDetails(ARefresh: Boolean = False); dynamic;
    procedure CloseDetails;

    procedure LoadUserConfig(AStoreFormSettings: TFormItemsForSave); override;
    procedure SaveUserConfig(AStoreFormSettings: TFormItemsForSave); override;
    function  GetBusy: Boolean; override;

    { Для обязательного переопределения непосредственными наследниками }
    procedure InternalSaveGridPosition(var ABookmark: TDataSourceBookmark); dynamic; abstract;
    procedure InternalRestoreGridPosition(ABookmark: TDataSourceBookmark); dynamic; abstract;
    procedure InternalReOpen; dynamic; abstract;

    procedure InternalBeginUpdate; dynamic; abstract;
    procedure InternalEndUpdate; dynamic; abstract;

//    procedure InternalDisableControls; dynamic;
    function  InternalGetControlsEnabled: Boolean; virtual;
    procedure InternalEnableControls; dynamic;

    procedure InternalSetGridFocus; dynamic; abstract;
    procedure InternalSetGridFirstRow; dynamic; abstract;
    function  InternalLocateByKey(AKeyValue: Variant): Boolean; dynamic; abstract;

    function  InternalGetGrid: TComponent; dynamic; abstract;
    function  InternalGetGridKeys: String; dynamic; abstract;
    function  InternalGetGridKeyValues: Variant; dynamic; abstract;
    function  InternalFindRecord(AKeyFieldValues: Variant): Integer; dynamic; abstract;
    function  InternalGetGridRecordChecked(ARecordIndex: Integer): Boolean; dynamic; abstract;
    procedure InternalSetGridRecordChecked(ARecordIndex: Integer); dynamic; abstract;
    function  InternalGetGridFieldValues(const ARecordIndex: Integer; const AFieldNames: String; var AUnknownFields: String): Variant; overload; dynamic; abstract;
    function  InternalGetGridFieldValues(const AFieldNames: String; var AUnknownFields: String): Variant; overload; dynamic; abstract;
    function  InternalGetGridRecordCount: Integer; dynamic; abstract;

    procedure GridAfterUpdateActions(Sender: TObject); virtual;
    procedure InternalGetGridFilter(AFilter: PNamedVariants); dynamic;

    function  InternalGetSelectedRowCount: Integer; dynamic; abstract;
//    procedure InternalSetSelectedRow(ARecNo: Integer); dynamic; abstract;

    procedure InternalExcelExport; dynamic; abstract;

    procedure Loaded; override;
    procedure SBaseFormLoaded; override;
  public
    destructor Destroy; override;
    { Через эту функцию проверить - не работает ли сейчас потоковое открытие }
    procedure RegisterFilterItem(AItem: TSBaseViewFilter);
    function  GetFormParamValues(const AFieldNames: String; var AUnknownFields: String): Variant; override;
  published
    mnDatesPeriod: TSBaseViewFilterDatesPeriod;
  published
    { Битовая маска прав на записи SELECT / INSERT / UPDATE / DELETE }
    property RecordsPrivileges: Word read FRecordsPrivileges write SetRecordsPrivileges default 0;

    { Прификс для вывода кол-ва записей }
    property RecordCountPrefix: String read FRecordCountPrefix write FRecordCountPrefix;
    { Использует "родной" для Excel формат данных }
    property ExportToExcelInNativeFormat: Boolean read FExportToExcelInNativeFormat write FExportToExcelInNativeFormat Default True;

    { Если время открытия журнала превышает заданный интервал, то автоматического обновления не будет }
    property RefreshTimeout: Word read FRefreshTimeout write FRefreshTimeout default 0; { секунды }

    { Открытие датасета в потоке (не забудьте включить кнопку "Вывести") }
    property ThreadOpen: Boolean read FThreadOpen write FThreadOpen default false;
    { Автоматическое обновление журнала по таймеру }
    property AutoRefresh: Boolean read FAutoRefresh write FAutoRefresh default false;

    property DataSetActive: Boolean write SetDataSetActive;

    property AutoLocalFilter: Boolean read FAutoLocalFilter write FAutoLocalFilter default False;
    property FilterUnderConstruction: Boolean read FFilterUnderConstruction write FFilterUnderConstruction default False;
    property FilterChanged: Boolean write SetFilterChanged;
    property Collisions: Boolean read FCollisions write SetCollisions;
    property OnSelectFilter: TSBaseViewFilter read FOnSelectFilter write FOnSelectFilter;

    property DeletedCondition: String read FDeletedCondition write FDeletedCondition;
  end;

{$IFDEF FASTSCRIPT_RTTI}
  TFunctions_TSBaseXXViewFrm = class(TfsRTTIModule)
  private
    function Call_TSBaseXXViewFrm_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TSBaseViewFilter_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TSBaseViewFilter_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
    function  Call_TSBaseViewFilter_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

  TSBaseViewFilter = class(TComponent)
    procedure OnViewFilterItemChanged(Sender: TObject); dynamic;
  private
    FLevel          : UInt16;
    FInternalEnabled: Boolean;
    FInternalVisible: Boolean;
    FInternalUpdate : Boolean;

    FParamName      : String;
    FAllowNull      : Boolean;
    FStore          : Boolean;

    FBaseXXViewForm : TSBaseXXViewFrm;
    FParent         : TComponent;
    FControl        : TComponent;
    FDisplayFormat  : String;

    FOnFilterValueChanged: TNotifyEvent;

    function  InternalGetEnabled: Boolean;
    procedure InternalSetEnabled(const Value: Boolean);
    function  InternalGetVisible: Boolean;
    procedure InternalSetVisible(const Value: Boolean);

    procedure SetParamName(const Value: String); virtual;
    function  GetControlProperties: TcxCustomEditProperties; virtual;
    procedure SetControlProperties(const Value: TcxCustomEditProperties);
    function  GetParamValues(Index: String): Variant;
    procedure SetParamValues(AName: String; const AValue: Variant);
    function  GetEditControl: TComponent;
  protected
    procedure SetParentComponent(Value: TComponent); override;

    procedure InternalCreate; dynamic; abstract;

    function  InternalGetParamValue: Variant; virtual; abstract;
    procedure InternalSetParamValue(const AValue: Variant); virtual; abstract;
    function  InternalGetCaption: String; virtual;

    procedure InternalSaveConfig; dynamic;
    procedure InternalLoadConfig; dynamic;

    procedure InternalSetAllowNull(const AValue: Boolean); virtual;
    property  AllowNull: Boolean read FAllowNull write InternalSetAllowNull Default True;
  public
    constructor Create(AOwner: TComponent); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;

    property ParamValue: Variant read InternalGetParamValue write InternalSetParamValue;
    property ParamValues[Index: String]: Variant read GetParamValues write SetParamValues;
    property DisplayText: String read InternalGetCaption;
  published
    property Level: UInt16 read FLevel write FLevel default 0;

    property Control: TComponent read FControl;
    property EditControl: TComponent read GetEditControl;
    property EditControlProperties: TcxCustomEditProperties read GetControlProperties write SetControlProperties;

    property ParamName : String  read FParamName write SetParamName;
    property Enabled: Boolean read InternalGetEnabled write InternalSetEnabled;
    property Visible: Boolean read InternalGetVisible write InternalSetVisible;
    property Store: Boolean read FStore write FStore;
    property DisplayFormat: String read FDisplayFormat write FDisplayFormat;

    property OnFilterValueChanged: TNotifyEvent read FOnFilterValueChanged write FOnFilterValueChanged;
  end;

  TSBaseViewFilterCxEdit = class(TSBaseViewFilter)
  protected
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; dynamic; abstract;
    class function GetEditClass: TcxCustomEditClass; dynamic; abstract;
    function GetProperties: TcxCustomEditProperties;

    procedure InternalCreate; override;
    procedure InternalInitProperties; dynamic;

    function  InternalGetParamValue: Variant; override;
    procedure InternalSetParamValue(const AValue: Variant); override;
    function  InternalGetCaption: String; override;
  published
    property  AllowNull;
  end;

  TSBaseViewFilterText = class(TSBaseViewFilterCxEdit)
  protected
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    class function GetEditClass: TcxCustomEditClass; override;

    function  InternalGetParamValue: Variant; override;
  end;

  TSBaseViewFilterSpin = class(TSBaseViewFilterCxEdit)
  protected
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    class function GetEditClass: TcxCustomEditClass; override;
  end;

  TSBaseViewFilterDate = class(TSBaseViewFilterCxEdit)
  protected
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    class function GetEditClass: TcxCustomEditClass; override;

    procedure InternalInitProperties; override;
    function InternalGetCaption: String; override;
  end;

  TSBaseViewFilterDateTime = class(TSBaseViewFilterDate)
  protected
    procedure InternalInitProperties; override;
  end;

  TSBaseViewFilterDatesPeriod = class(TSBaseViewFilter)
    procedure ButtonClick(Sender: TObject);
  private
    FDateBegin, FDateEnd: TDate;
    FDatesPeriod: TDatesPeriod;
    FEmptyValue: Boolean;
  protected
    procedure InternalCreate; override;

    procedure InternalSetAllowNull(const AValue: Boolean); override;
    function  InternalGetParamValue: Variant; override;
    procedure InternalSetParamValue(const AValue: Variant); override;
    function  InternalGetCaption: String; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property  AllowNull;
  end;

  TSBaseViewFilterTime = class(TSBaseViewFilterCxEdit)
  protected
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    class function GetEditClass: TcxCustomEditClass; override;

    procedure InternalInitProperties; override;
  end;

  TSBaseViewFilterLookupCxEdit = class(TSBaseViewFilterCxEdit)
  protected
    procedure InternalSetAllowNull(const AValue: Boolean); override;
  end;

  TSBaseViewFilterLookupComboBox = class(TSBaseViewFilterLookupCxEdit)
  protected
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    class function GetEditClass: TcxCustomEditClass; override;

    procedure InternalInitProperties; override;
  end;

  TSBaseViewFilterLookupTreeComboBox = class(TSBaseViewFilterLookupCxEdit)
  protected
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    class function GetEditClass: TcxCustomEditClass; override;

    procedure InternalInitProperties; override;
  end;

  TSBaseViewFilterCheckLookupTreeComboBox = class(TSBaseViewFilterLookupCxEdit)
  protected
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    class function GetEditClass: TcxCustomEditClass; override;
    procedure InternalInitProperties; override;
  end;

  TSBaseViewFilterCheckLookupComboBox = class(TSBaseViewFilterLookupCxEdit)
  protected
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    class function GetEditClass: TcxCustomEditClass; override;
    procedure InternalInitProperties; override;
  end;

  TSBaseViewFilterCheckBox = class(TSBaseViewFilterCxEdit)
  protected
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    class function GetEditClass: TcxCustomEditClass; override;
    procedure InternalInitProperties; override;
    function  InternalGetCaption: String; override;
  end;

  TSBaseViewFilterRadioGroup = class(TSBaseViewFilterCxEdit)
  protected
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    class function GetEditClass: TcxCustomEditClass; override;
  end;

  TSBaseViewFilterLookupComponent = class(TSBaseViewFilterCxEdit)
  private
    FLookupControl: TSBaseLookupComponentControl;

    procedure SetMenu(const Value: TdxBarPopupMenu);
    procedure SetParamName(const Value: String); override;
    procedure SetExtractScript(const Value: String);
    procedure SetExtractSQL(const Value: String);
  protected
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    class function GetEditClass: TcxCustomEditClass; override;

    procedure InternalInitProperties; override;
  protected
    procedure InternalSetAllowNull(const AValue: Boolean); override;
    function  InternalGetParamValue: Variant; override;
    procedure InternalSetParamValue(const AValue: Variant); override;
  public
    destructor Destroy; override;
  published
    property LookupControl: TSBaseLookupComponentControl read FLookupControl;

    property ExtractSQL: String write SetExtractSQL;
    property ExtractScript: String write SetExtractScript;

    property Menu: TdxBarPopupMenu write SetMenu;
  end;

  TSBaseViewFilterFile = class(TSBaseViewFilterCxEdit)
  private
    function GetButtonClear: Boolean;
    function GetButtonDir: Boolean;
    function GetButtonFile: Boolean;
    procedure SetButtonClear(const Value: Boolean);
    procedure SetButtonDir(const Value: Boolean);
    procedure SetButtonFile(const Value: Boolean);
  protected
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    class function GetEditClass: TcxCustomEditClass; override;

    procedure InternalInitProperties; override;
    procedure InternalSetAllowNull(const AValue: Boolean); override;
  published
    property ButtonDir: Boolean read GetButtonDir write SetButtonDir default False;
    property ButtonFile: Boolean read GetButtonFile write SetButtonFile default True;
    property ButtonClear: Boolean read GetButtonClear write SetButtonClear default False;
  end;

  TdxBarButtonEx = class(TdxBarButton)
  private
    FValueChecked  : Variant;
    FValueUnchecked: Variant;
    procedure SetEditValue(const Value: Variant);
  public
    constructor Create(AOwner: TComponent); override;
    function GetEditValue: Variant;
  published
    property ValueChecked: Variant read FValueChecked write FValueChecked;
    property ValueUnchecked: Variant read FValueUnchecked write FValueUnchecked;

    property EditValue: Variant read GetEditValue write SetEditValue;
  end;

  TSBaseViewFilterButton = class(TSBaseViewFilter)
  private
  protected
    procedure InternalCreate; override;

    function  InternalGetParamValue: Variant; override;
    procedure InternalSetParamValue(const AValue: Variant); override;
    function  InternalGetCaption: String; override;
  end;

implementation

uses
  SBaseStringFunctions, SBaseConstants, SBaseVariantFunctions, SBaseDxUtils,
  SBaseCxUtils, SBaseUtils, SBaseFormDefinitions, DataModule;

type
  TComponentCrack = class(TComponent);
  TdxBarItemCrack = class(TdxBarItem);
  TcxCustomEditCrack = class(TcxCustomEdit);

{$R *.dfm}

procedure TSBaseXXViewFrm.InternalClose(AFinalization: Boolean);
begin
  FFilterUnderConstruction := True;
  try
    CloseDataSet;
    inherited;
  except
    FFilterUnderConstruction := False;
    Raise;
  end;
end;

procedure TSBaseXXViewFrm.InternalCreate;
var
  LLinkedComponent: TComponent;
begin
  FCollisions               := True;
  FFilterUnderConstruction  := True;
//  FRefreshTimeout         := 0;
  FRecordCountPrefix     := ' ' + SBaseProperties.Text_RecordCount + ': ';

//  FRecordsPrivileges      := 0;

  FThreadOpen             := False;

  FExportToExcelInNativeFormat := True;

  FAutoFilterItems        := TList<TSBaseViewFilter>.Create;

  mnView.Caption := SBaseProperties.Text_MenuView;
  mnEdit.Caption := SBaseProperties.Text_MenuEdit;
  if TdxBarItemCrack(mnNew).FCaption = '' then
    mnNew.Caption := SBaseProperties.Text_MenuNew;
  mnDelete.Caption := SBaseProperties.Text_MenuDelete;
  mnCopy.Caption := SBaseProperties.Text_MenuCopy;
  mnMarkAsDeleted.Caption := SBaseProperties.Text_MenuMarkAsDeleted;
  mnRefresh.Caption := SBaseProperties.Text_MenuRefresh;
  mnFilter.Caption := SBaseProperties.Text_MenuFilterBox;
  mnGroupBox.Caption := SBaseProperties.Text_MenuGroupBox;
  mnGroupingClear.Caption := SBaseProperties.Text_MenuGroupingClear;
  mnExpandAll.Caption := SBaseProperties.Text_MenuExpandAll;
  mnCollapseAll.Caption := SBaseProperties.Text_MenuCollapseAll;
  mnDetails.Caption := SBaseProperties.Text_MenuDetails;
  mnDetailsBottom.Caption := SBaseProperties.Text_BottomPlace;
  mnDetailsRight.Caption := SBaseProperties.Text_RightPlace;
  mnProperties.Caption := SBaseProperties.Text_MenuProperties;
  mnFitToWidth.Caption := SBaseProperties.Text_MenuFitToWidth;
  mnFitToHeight.Caption := SBaseProperties.Text_MenuFitToHeight;
  mnFitToScreen.Caption := SBaseProperties.Text_MenuFitToScreen;
  mnPrint.Caption := SBaseProperties.Text_MenuPrint;
  mnExcel.Caption := SBaseProperties.Text_MenuExportTo + ' Excel';
  mnSubmit.Caption := SBaseProperties.Text_MenuDisplay;
  mnFormClose.Caption := SBaseProperties.Text_MenuClose;
  mnSelect.Caption := SBaseProperties.Text_MenuSelect;
  mnTake.Caption := SBaseProperties.Text_MenuTake;

  inherited;

  LLinkedComponent := InternalGetGrid;
{$IFDEF DEBUG}
//  Assert(Assigned(LLinkedComponent), 'InternalGetGrid is nil');
{$ENDIF}

{$IFDEF FASTSCRIPT_RTTI}
  FBeforeOpenDataSet                := PrepareFunction('BeforeOpenDataSet', False);
  FDBGridAfterDataSetOpen           := PrepareFunction('DBGridAfterDataSetOpen', False);
  FDBGridAfterDataSetClose          := PrepareFunction('DBGridAfterDataSetClose', False);
  FDBGridAfterFocusedRecordChanged  := PrepareFunction('DBGridAfterFocusedRecordChanged', False);
  FOpenDetails                      := PrepareFunction('OpenDetails', False);
  FCloseDetails                     := PrepareFunction('CloseDetails', False);
  FInternalSetFilter                := PrepareFunction('InternalSetFilter', False);
{$ENDIF}

  FAutoRefresh          := RefreshTimer.Enabled;
  RefreshTimer.Enabled  := False;

  if BarManager.ImageOptions.Images = SBaseFormProperties.Icon16List then begin
    mnView.ImageIndex := SBaseFormProperties.IconIndex_View;
    mnEdit.ImageIndex := SBaseFormProperties.IconIndex_Edit;
    mnNew.ImageIndex := SBaseFormProperties.IconIndex_New;
    mnDelete.ImageIndex := SBaseFormProperties.IconIndex_Delete;
    mnCopy.ImageIndex := SBaseFormProperties.IconIndex_Copy;
    mnMarkAsDeleted.ImageIndex := SBaseFormProperties.IconIndex_MarkAsDeleted;
    mnRefresh.ImageIndex := SBaseFormProperties.IconIndex_Refresh;
    mnFilter.ImageIndex := SBaseFormProperties.IconIndex_FilterBox;
    mnGroupBox.ImageIndex := SBaseFormProperties.IconIndex_GroupBox;
    mnGroupingClear.ImageIndex := SBaseFormProperties.IconIndex_GroupingClear;
    mnExpandAll.ImageIndex := SBaseFormProperties.IconIndex_ExpandAll;
    mnCollapseAll.ImageIndex := SBaseFormProperties.IconIndex_CollapseAll;
    mnDetails.ImageIndex := SBaseFormProperties.IconIndex_Details;
    mnDetailsBottom.ImageIndex := SBaseFormProperties.IconIndex_DetailsBottom;
    mnDetailsRight.ImageIndex := SBaseFormProperties.IconIndex_DetailsRight;
    mnProperties.ImageIndex := SBaseFormProperties.IconIndex_Properties;
    mnFitToWidth.ImageIndex := SBaseFormProperties.IconIndex_FitToWidth;
    mnFitToHeight.ImageIndex := SBaseFormProperties.IconIndex_FitToHeight;
    mnFitToScreen.ImageIndex := SBaseFormProperties.IconIndex_FitToScreen;
    mnPrint.ImageIndex := SBaseFormProperties.IconIndex_Printer;
    mnExcel.ImageIndex := SBaseFormProperties.IconIndex_Excel;
    mnSubmit.ImageIndex := SBaseFormProperties.IconIndex_Submit;
    mnFormClose.ImageIndex := SBaseFormProperties.IconIndex_WindowClose;
    mnSelect.ImageIndex := SBaseFormProperties.IconIndex_Select;
    mnTake.ImageIndex := SBaseFormProperties.IconIndex_Take;

    mnDetailsBottom.ImageIndex := SBaseFormProperties.IconIndex_DetailsBottom;
    mnDetailsRight.ImageIndex := SBaseFormProperties.IconIndex_DetailsRight;
  end;

  if Assigned(LLinkedComponent) then
    ResultFields := InternalGetGridKeys;

  // В нужном ли месте?
  if SelectMode <> smNone then
    with ItemsReactions.FindOrCreateReaction(mnSelect) do begin
      LinkedComponent := LLinkedComponent;
      Default := (not AlwaysSelect) and (SelectMode <> smMultiple);
      OnGetEnabled.Active := True;
      OnClick.Reaction        := biraCustomDataSourceRecord;
      OnClick.SystemProcedure := mnSelectClick;
    end;

  if not FRecordsPrivilegesStored then
    RecordsPrivileges := Self.FPrivileges and Privileges_ForRecord;

  with ItemsReactions.FindOrCreateReaction(mnRefresh) do begin
    LinkedComponent := LLinkedComponent;
    with OnClick do begin
      Reaction        := biraCustomDataSourceActive;
      SystemProcedure := mnRefreshClick;
    end;
  end;

  with ItemsReactions.FindOrCreateReaction(mnRefreshRecord) do begin
    LinkedComponent := LLinkedComponent;
    OnClick.Reaction  := biraDataSourceRefreshRecord;
  end;

  if (not FDeletedCondition.IsEmpty) then begin
    mnMarkAsDeleted.Caption := SBaseProperties.Text_MenuMarkAsDeleted;
    ItemsReactions.InitReaction(mnMarkAsDeleted, InternalGetGrid, biraCustomDataSourceRecord);
  end;

  if SelectMode <> smNone then begin
    mnSelect.Enabled    := True;
    mnSelect.Visible    := ivAlways;
    if SelectMode in [smSingleRepeateAble, smMultipleRepeateAble] then begin
      mnTake.Visible    := ivAlways;
      with ItemsReactions.FindOrCreateReaction(mnTake) do begin
        LinkedComponent := LLinkedComponent;
        with OnClick do begin
          OnGetEnabled.Priority := 1;
          OnGetEnabled.SystemFunction := mnTakeOnGetEnabled;
          Reaction        := biraCustomDataSourceRecord;
          SystemProcedure := mnRefreshClick;
        end;
      end;
    end;

    mnFormClose.Enabled := True;
    mnFormClose.Visible := ivAlways;

    if Assigned(FOnSelectFilter) then begin
      FOnSelectFilter.AllowNull := False;
      ClickItem := FOnSelectFilter;
    end;
  end;

  with ToolBar.ItemLinks do begin
    BeginUpdate;
    try
      if SelectMode <> smNone then begin
        if not ToolBar.ItemLinks.HasItem(mnSelect) then
          with Add do begin
            Item        := mnSelect;
            BeginGroup  := True;
            Visible     := True;
            Align       := TdxBarItemAlign.iaCenter;
          end;

        if (SelectMode in [smSingleRepeateAble, smMultipleRepeateAble]) and not ToolBar.ItemLinks.HasItem(mnTake) then
          with Add do begin
            Item        := mnTake;
            //BeginGroup  := True;
            Visible     := True;
            Align       := mnTake.Align;
          end;

        if not ToolBar.ItemLinks.HasItem(mnFormClose) then
          with Add do begin
            Item        := mnFormClose;
            Visible     := True;
            Align       := TdxBarItemAlign.iaCenter;
          end;
      end;

      if not ToolBar.ItemLinks.HasItem(mnSubmit) then
        with Add do begin
          Item        := mnSubmit;
          Visible     := True;
          Align       := mnSubmit.Align;
        end;

      ToolBar.ItemLinks.FindByItem(mnSubmit).Index := ToolBar.ItemLinks.Count - 1;
    finally
      EndUpdate;
    end;

    if (BarControl <> nil) then
      BarControl.RepaintBar;
  end;

  if not Assigned(DataSetProvider.ConnectionProvider) then
    DataSetProvider.ConnectionProvider  := SQLConnectionProvider;

  if not Assigned(DetailSetProvider.ConnectionProvider) then
    DetailSetProvider.ConnectionProvider := SQLConnectionProvider;

  mnDetails.OnClick(mnDetails);
end;

//procedure TSBaseXXViewFrm.InternalDisableControls;
//begin
//  DataSetProvider.DisableControls;
//end;

procedure TSBaseXXViewFrm.InternalEnableControls;
begin
  if DataSetProvider.ControlsDisabled then
    DataSetProvider.EnableControls;
end;

function TSBaseXXViewFrm.InternalGetControlsEnabled: Boolean;
begin
  Result := (not DataSetProvider.ControlsDisabled);
end;

destructor TSBaseXXViewFrm.Destroy;
begin
  FFilterUnderConstruction := True;
  if Created then
    CloseDataSet;

  inherited;

  FreeAndNil(FAutoFilterItems);
end;

procedure TSBaseXXViewFrm.RefreshTimerTimer(Sender: TObject);
var
  LDone: Boolean;
  LVars: TNamedVariants;
begin
  LVars := TNamedVariants.Create(True);
  LDone := False;
  mnRefreshClick(nil, LVars, LDone);
end;

procedure TSBaseXXViewFrm.RegisterFilterItem(AItem: TSBaseViewFilter);
begin
  FAutoFilterItems.Add(AItem);
end;

procedure TSBaseXXViewFrm.InternalPrepareParams;
var
  I, LIdx: Integer;
  LItem: TStringItem;
  LFound: Boolean;
  LParamValue: Variant;
begin
  if CreateFormParams.Find(SConst_DataSetActive, LIdx) then begin
    DataSetActive := (CreateFormParams.Items[LIdx].Value = True);
    CreateFormParams.Delete(LIdx);
  end;

  for I := Pred(FAutoFilterItems.Count) downto 0 do
    with FAutoFilterItems[I] do
      if Enabled and (not ParamName.IsEmpty) then begin
        LFound := False;
        for LItem in EnumStringItems(ParamName) do
          if CreateFormParams.Find(LItem.Value, LIdx) then begin
            if not LFound then begin
              Store := False;
              LFound := True;
              LParamValue := ParamValue;
            end;
            VarArrayOverWrite(ParamName, LParamValue, LItem.Value, CreateFormParams.Items[LIdx].Value);
            CreateFormParams.Delete(LIdx);
          end;
        if LFound then
          ParamValue := LParamValue;
      end;

  inherited;
end;

procedure TSBaseXXViewFrm.InternalInit;
var
  LGridKeys, LUnknownGridKeys: String;
begin
  if (SelectMode <> smNone) then begin
    LGridKeys := InternalGetGridKeys;
    if (not LGridKeys.IsEmpty) then begin
      FInitGridKeyValues := CreateFormParams.ReadValues(LGridKeys, LUnknownGridKeys, False);
      if (not LUnknownGridKeys.IsEmpty) then begin
        LGridKeys := '';
        FInitGridKeyValues := Unassigned;
      end else
        CreateFormParams[LGridKeys] := unAssigned;
    end;
  end else
    LGridKeys := '';

  FFilterUnderConstruction := False;
  inherited;

  if (not DataSetProvider.Active) and (not DataSetProvider.Thread.Active) then
    AfterFilterChanged(FDataSetActive);
end;

procedure TSBaseXXViewFrm.InternalPostInit;
var
  LGridKeys: String;
  I, J: Integer;
  LKeyCount, LInitGridKeyValuesHighBound: Integer;
  LInitGridKeyValues: Variant;
  LRecordIndex: Integer;
  LItem: TStringItem;
  LHighBound: Integer;
begin
  LGridKeys := InternalGetGridKeys;
  if (mnSubmit.Visible <> ivNever) and (not DataSetProvider.Active) then
    mnSubmit.ShortCut := ShortCut_Enter
  else if (not LGridKeys.IsEmpty) and VarArrayIsPresent(FInitGridKeyValues) then
    if (SelectMode in [smSingle, smSingleRepeateAble]) then
      InternalLocateByKey(FInitGridKeyValues)
    else begin
      LKeyCount := 0;
      LInitGridKeyValuesHighBound := 0;
      for LItem in SBaseStringFunctions.EnumStringItems(LGridKeys) do begin
        if (LItem.Index = 0) and LItem.Eof then begin
          LKeyCount := 1;
          LInitGridKeyValuesHighBound := VarArrayHighBound(LInitGridKeyValues, 1);
        end else begin
          LHighBound := VarArrayHighBound(LInitGridKeyValues[0], 1);
          if (LItem.Index > 0) and (LInitGridKeyValuesHighBound <> LHighBound) then
            LKeyCount:= 0
          else begin
            LKeyCount:= LItem.Index;
            if (LItem.Index = 0) then
              LInitGridKeyValuesHighBound := LHighBound;
          end;
        end;
      end;

      if (LKeyCount > 0) then begin
        if LKeyCount > 1 then
          LInitGridKeyValues := VarArrayCreate([0, LKeyCount-1], varVariant);
      end;
      for I := 0 to LInitGridKeyValuesHighBound do begin
        if (LKeyCount = 1) then
          LRecordIndex := InternalFindRecord(FInitGridKeyValues[I])
        else begin
          for J := 0 to LKeyCount - 1 do
            LInitGridKeyValues[J] := FInitGridKeyValues[J][I];
          LRecordIndex := InternalFindRecord(LInitGridKeyValues)
        end;
        if LRecordIndex >= 0 then
          InternalSetGridRecordChecked(LRecordIndex);
      end;
    end;

  inherited;
end;

procedure TSBaseXXViewFrm.SetMDIToolButtons(AValue: TMDIToolButtons);
begin
  if Assigned(mnDatesPeriod) then
    mnDatesPeriod.Enabled := (mtbDatesPeriod in AValue)
  else
    AValue := AValue - [mtbDatesPeriod];

  inherited;

  if (mtbProperties in MDIToolButtons) then
    mnProperties.Visible := ivAlways
  else
    mnProperties.Visible := ivNever;

  if (mtbFilter in MDIToolButtons) then
    mnFilter.Visible := ivAlways
  else
    mnFilter.Visible := ivNever;
end;

procedure TSBaseXXViewFrm.Loaded;
begin
  inherited;

  if not Loading then
    SBaseFormLoaded;
end;

procedure TSBaseXXViewFrm.SBaseFormLoaded;
begin
  if FLoadedRecordsPrivileges <> FRecordsPrivileges then
    RecordsPrivileges := FLoadedRecordsPrivileges;
end;

procedure TSBaseXXViewFrm.LoadUserConfig(AStoreFormSettings: TFormItemsForSave);
var
  I, FInt: Integer;
begin
  inherited;

  if frsvUserDefined in AStoreFormSettings then begin
    if mnFitToWidth.Visible <> ivNever then
      mnFitToWidth.Down  := VarToBoolDef(GetSetting(SConst_FitToWidth), mnFitToWidth.Down);

    if mnFitToHeight.Visible <> ivNever then
      mnFitToHeight.Down := VarToBoolDef(GetSetting(SConst_FitToHeight), mnFitToHeight.Down);

    if mnDetails.Visible <> ivNever then begin
      FDetailsWidth   := VarToIntDef(GetSetting(PanelDetails.Name + '.' + SConst_Width),  PanelDetails.ExplicitWidth);
      FDetailsHeight  := VarToIntDef(GetSetting(PanelDetails.Name + '.' + SConst_Height), PanelDetails.ExplicitHeight);
      FInt := VarToIntDef(GetSetting(PanelDetails.Name + '.' + SConst_State), -1);
      case FInt of
        0:
        begin
          mnDetails.Down := False;
          mnDetailsClick(Self);
        end;

        1: mnDetailsBottom.Click;
        2: mnDetailsRight.Click;
      end;
    end;

    for I := Pred(FAutoFilterItems.Count) downto 0 do
      with FAutoFilterItems[I] do
        if Enabled and Store then
          InternalLoadConfig;
  end;

  if (frsvFormItems in AStoreFormSettings) or (frsvUserDefined in AStoreFormSettings) then begin
    if (mnFitToWidth.Visible <> ivNever) and Assigned(mnFitToWidth.OnClick) then
      mnFitToWidth.OnClick(mnFitToWidth);

    if (mnFitToHeight.Visible <> ivNever) and Assigned(mnFitToHeight.OnClick) then
      mnFitToHeight.OnClick(mnFitToHeight);
  end;
end;

procedure TSBaseXXViewFrm.SaveUserConfig(AStoreFormSettings: TFormItemsForSave);
var
  I, LInt: Integer;
begin
  inherited;

  if frsvUserDefined in AStoreFormSettings then begin
    if mnFitToWidth.Visible <> ivNever then
      SetSetting(SConst_FitToWidth , mnFitToWidth.Down);

    if mnFitToHeight.Visible <> ivNever then
      SetSetting(SConst_FitToHeight, mnFitToHeight.Down);

    if (mnDetails.Visible <> ivNever) then begin
      SetSetting(PanelDetails.Name + '.' + SConst_Height, FDetailsHeight);
      SetSetting(PanelDetails.Name + '.' + SConst_Width, FDetailsWidth);

      if mnDetails.Down then begin
        if PanelDetails.Align = alBottom then
          LInt := 1
        else
          LInt := 2;
      end
      else
        LInt := 0;

      SetSetting(PanelDetails.Name + '.' + SConst_State, LInt);
    end;

    for I := Pred(FAutoFilterItems.Count) downto 0 do
      with FAutoFilterItems[I] do
        if Enabled and Store then
          InternalSaveConfig;
  end;
end;

procedure TSBaseXXViewFrm.WMDatesPeriod(var Msg: TMessage);
begin
  if (not Busy) and Assigned(mnDatesPeriod) then
    mnDatesPeriod.ButtonClick(nil);
end;

procedure TSBaseXXViewFrm.AfterDataChanged(const ATablesChanges: TTablesChanges);
begin
  if (not Busy) then
    if ATablesChanges.FullRefresh then
      mnRefresh.Click
    else
      inherited;
end;

procedure TSBaseXXViewFrm.WMPrint(var Msg: TMessage);
begin
  mnPrint.Click
end;

procedure TSBaseXXViewFrm.WMFilter(var Msg: TMessage);
begin
  if (not Busy) then
    mnFilter.Click;
end;

procedure TSBaseXXViewFrm.WMProperties(var Msg: TMessage);
begin
  if (not Busy) then
    mnProperties.Click;
end;

procedure TSBaseXXViewFrm.ShowRecordCount;
begin
  with StatusBar.Panels[Pred(StatusBar.Panels.Count)] do begin
    Text := RecordCountPrefix + IntToStr(InternalGetGridRecordCount);
    Width := PanelStyle.StatusBarControl.Canvas.TextWidth(Text) + 10;
  end;
end;

procedure TSBaseXXViewFrm.mnRefreshClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  ADone := True;

  if Collisions then
    CloseDataSet
  else if FThreadOpen then begin
    if not DataSetProvider.Thread.Active then
      if DataSetProvider.Active then
        DataSetProvider.Thread.Refresh
      else
        OpenDataSet
  end else begin
    FBeforOpenTime := Now;
    InternalReOpen
  end
end;

procedure TSBaseXXViewFrm.DataSetProviderThreadOnPrint(ADataSetProvider: TCustomSQLDataSetProvider; const AMessages: TStringList);
var
  I: Integer;
begin
  for I := 0 to Pred(AMessages.Count) do
    SBaseMainFormMDIInterface.Log(lgInfo, AMessages.Strings[I]);
end;

procedure TSBaseXXViewFrm.DataSetProviderThreadOnError(ADataSetProvider: TCustomSQLDataSetProvider; const AException: string; var ADone: Boolean);
begin
  ADone := True;
  SBaseMainFormMDIInterface.Log(lgError, AException);
end;

procedure TSBaseXXViewFrm.DataSetProviderThreadAfterComplete(ADataSetProvider: TCustomSQLDataSetProvider; ATerminateReason: TDataSetThreadResult);
begin
  HideProgress;
  case ATerminateReason of
    bdstrOpened:
      begin
        InternalSetGridFirstRow;
        if Visible and (not Assigned(FClickItems) or (FClickItems.Count = 0)) then
          InternalSetGridFocus;
      end;
//    bdstrRefreshed:
//      begin
//      end;
  end;
end;

procedure TSBaseXXViewFrm.DataSetProviderThreadOnValidate(ADataSetProvider: TCustomSQLDataSetProvider; ADataSet: TInternalQuery; AMultiDataSetIndex: Word; var AValid: Boolean);
begin
  with ADataSet do
    if (FieldCount = 2) and SameText(Fields[0].FieldName, SConst_Log_Type) and SameText(Fields[1].FieldName, SConst_Log_Message)
    then begin
      if not IsEmpty then begin
        First;
        while not Eof do begin
          SBaseMainFormMDIInterface.Log(Fields[0].AsString[1], Fields[1].AsString);
          Next;
        end;
      end;
      AValid := False;
    end;
end;

procedure TSBaseXXViewFrm.OpenDataSet;
begin
  CloseDataSet;

  if not Collisions then begin
    FBeforOpenTime := Now;
    InternalBeginUpdate;
    try
      if FThreadOpen then begin
        ShowProgress(DoCancelRequest);
        try
          //DataSetBeforeOpen;
          DataSetProvider.Thread.Open;
        except
          HideProgress;
          Raise;
        end;
      end else begin
        //DataSetBeforeOpen;
        DataSetProvider.Open;
        InternalSetGridFirstRow;
      end;
    finally
      InternalEndUpdate
    end;
  end;
end;

procedure TSBaseXXViewFrm.PanelDetailsResize(Sender: TObject);
begin
  if not FDetailsUpdating then begin
    case PanelDetails.Align of
      alRight:
        FDetailsWidth := PanelDetails.Width;
      alBottom:
        FDetailsHeight := PanelDetails.Height;
    end;
  end;
end;

procedure TSBaseXXViewFrm.SetDataSetActive(const Value: Boolean);
begin
  if Value then begin
    if Loading then
      FDataSetActive := True
    else
      OpenDataSet
  end else begin
    FDataSetActive := False;
    CloseDataSet;
  end;
end;

procedure TSBaseXXViewFrm.SetCollisions(const Value: Boolean);
begin
  if FCollisions <> Value then begin
    FCollisions := Value;
  end;

  if mnSubmit.Visible <> ivNever then
    mnSubmit.Enabled := (not FCollisions);
end;

procedure TSBaseXXViewFrm.SetFilterChanged(const Value: Boolean);
begin
  AfterFilterChanged(Value);
end;

procedure TSBaseXXViewFrm.DataSetAfterOpen(DataSet: TDataSet);
begin
  if (mnSubmit.Visible <> ivNever) then
    mnSubmit.ShortCut := 0;

  RefreshTimer.Enabled := FAutoRefresh
                            and DataSet.Active
                            and ( (RefreshTimeout = 0) or (SecondsBetween(FBeforOpenTime, Now) < RefreshTimeout) );

{$IFDEF FASTSCRIPT_RTTI}
  CallFunction(FDBGridAfterDataSetOpen);
{$ENDIF}
end;

procedure TSBaseXXViewFrm.DataSetAfterRefresh(DataSet: TDataSet);
begin
  DBGridAfterFocusedRecordChanged(True);
end;

procedure TSBaseXXViewFrm.DataSetBeforeOpen(DataSet: TDataSet);
begin
{$IFDEF FASTSCRIPT_RTTI}
  CallFunction(FBeforeOpenDataSet);
{$ENDIF}
end;

procedure TSBaseXXViewFrm.DataSetNewRecord(DataSet: TDataSet);
var
  LVariables: TNamedVariants;
  LLevel: UInt16;
  LCollisions: Boolean;
begin
  DataSet := DataSet.Source;
  if DataSet is TCustomBaseDataSetProvider then
    LLevel := TCustomBaseDataSetProvider(DataSet).Level
  else
    LLevel := 0;

  LVariables.UnknownAsNull := True;
  LCollisions := False;
  InternalGetParameters(LLevel, LVariables, LCollisions);
  Assert(not LCollisions);
  DataSet.Fields.Load(LVariables, lvcIfReceive);
end;

procedure TSBaseXXViewFrm.DataSetAfterClose(DataSet: TDataSet);
begin
  if (csDestroying in ComponentState) then Exit;

  RefreshTimer.Enabled := False;
  if (mnSubmit.Visible <> ivNever) then
    mnSubmit.ShortCut := ShortCut_Enter;

  {  if ToolBar.Visible then }
  DBGridAfterFocusedRecordChanged(True);

  StatusBar.Panels[Pred(StatusBar.Panels.Count)].Text := '';
//  CloseDetails;

{$IFDEF FASTSCRIPT_RTTI}
  CallFunction(FDBGridAfterDataSetClose);
{$ENDIF}
end;

procedure TSBaseXXViewFrm.CloseDataSet;
begin
  if DataSetProvider <> nil then with DataSetProvider do begin
    Thread.Stop;
    Close;
    UnPrepare;
  end;
end;

procedure TSBaseXXViewFrm.AfterFilterChanged(AOpenIfPosible: Boolean);
begin
  if FFilterUnderConstruction then Exit;
  UpdateCaption;

  if (not AOpenIfPosible) and (mnSubmit.Visible <> ivNever) then
    CloseDataSet;

  InternalSetFilter;

  if AOpenIfPosible and (mnSubmit.Visible <> ivNever) and (not DataSetProvider.Active) then
    OpenDataSet;
end;

procedure TSBaseXXViewFrm.AfterSkinChanged;
var
  LDefaultStyle: Boolean;
  I: Integer;
begin
  inherited;

  LDefaultStyle := (SBaseMainFormMDIInterface = nil) or SBaseMainFormMDIInterface.dxDefaultStyle;
  with PanelMain.Style.LookAndFeel do
    if LDefaultStyle then begin
      Kind        := lfOffice11;
      NativeStyle := False;
    end else
      AssignedValues := [];

  if Assigned(FAutoFilterItems) then
    for I := Pred(FAutoFilterItems.Count) downto 0 do
      with FAutoFilterItems[I] do
        if (EditControl is TcxCustomEdit) then
          if LDefaultStyle then begin
            with TcxCustomEdit(FAutoFilterItems[I].EditControl).Style.LookAndFeel do begin
              Kind        := lfOffice11;
              NativeStyle := False;
            end;
          end else
            TcxCustomEdit(FAutoFilterItems[I].EditControl).Style.LookAndFeel.AssignedValues := []
end;

class function TSBaseXXViewFrm.AlwaysSelect: Boolean;
begin
  Result := False
end;

procedure TSBaseXXViewFrm.FormShow(Sender: TObject);
begin
  inherited;

//  StatusBar.Panels[1].Width := StatusBar.Canvas.TextWidth(FRecordCountPrefix + '0000000');
  mnFilter.OnClick(mnFilter);
end;

procedure TSBaseXXViewFrm.mnFitToScreenClick(Sender: TObject);
begin
  ShowMDIChildMaximized;
end;

procedure TSBaseXXViewFrm.mnFilterClick(Sender: TObject);
var
  FVisible: Boolean;
begin
  FVisible := (mnFilter.Visible <> ivNever) and mnFilter.Down;
//  SplitterFilter.Visible := FVisible;
  FilterBar.Visible := FVisible;
end;

function TSBaseXXViewFrm.GetBusy: Boolean;
begin
  Result := (inherited GetBusy) or (Assigned(DataSetProvider) and DataSetProvider.Thread.Active);
end;

procedure TSBaseXXViewFrm.mnSubmitClick(Sender: TObject);
begin
  if DataSetProvider.Thread.Active then
    CloseDataSet
  else
    OpenDataSet
end;

procedure TSBaseXXViewFrm.DoCancelRequest(Sender: TObject);
begin
//  DataSetProvider.Thread.Stop;
//    MainDataModule.ConnectionTerminate(DataSetProvider.ConnectionProvider);
  CloseDataSet;
end;

procedure TSBaseXXViewFrm.mnDetailsClick(Sender: TObject);
var
  FVisible: Boolean;
begin
  FVisible := (mnDetails.Visible <> ivNever) and mnDetails.Down;

  if FVisible <> PanelDetails.Visible then begin
    PanelDetails.Visible    := FVisible;
    SplitterDetails.Visible := FVisible;

    if FVisible then begin
      if (mnDetails.DropDownMenu <> nil) and (Sender <> nil) then
        if PanelDetails.Align = alRight then
          mnDetailsRight.Down := True
        else
          mnDetailsBottom.Down := True;

      ShowDetails;
    end else
      CloseDetails;
  end;
end;

procedure TSBaseXXViewFrm.mnDetailsBottomClick(Sender: TObject);
begin
  try
    PanelDetails.DisableAlign;
    FDetailsUpdating := True;

    PanelDetails.Align            := alBottom;
    PanelDetails.Height           := FDetailsHeight;
    SplitterDetails.AlignSplitter := salBottom;
    SplitterDetails.Cursor        := crVSplit;

    mnDetails.Down := True;
    mnDetailsClick(nil);
  finally
    FDetailsUpdating := False;
    PanelDetails.EnableAlign;
  end;
end;

procedure TSBaseXXViewFrm.mnDetailsRightClick(Sender: TObject);
begin
  try
    PanelDetails.DisableAlign;
    FDetailsUpdating := True;

    PanelDetails.Align            := alRight;
    PanelDetails.Width            := FDetailsWidth;
    SplitterDetails.AlignSplitter := salRight;
    SplitterDetails.Cursor        := crHSplit;

    mnDetails.Down := True;
    mnDetailsClick(nil);
  finally
    FDetailsUpdating := False;
    PanelDetails.EnableAlign;
  end;
end;

procedure TSBaseXXViewFrm.DBGridAfterFocusedRecordChanged(ARefresh: Boolean);
begin
  ShowDetails(ARefresh);
{$IFDEF FASTSCRIPT_RTTI}
  CallFunction(FDBGridAfterFocusedRecordChanged);
{$ENDIF}
end;

procedure TSBaseXXViewFrm.ShowDetails(ARefresh: Boolean = False);
var
  LUnknownFields: String;
  LValues: Variant;
begin
  if Assigned(PanelDetails) and PanelDetails.Visible then begin
    if InternalGetGridRecordCount > 0 then begin
      LValues := InternalGetGridFieldValues(DetailSetProvider.MasterFields, LUnknownFields);

      if not VarArrayIsPresent(LValues) then
        CloseDetails
      else if ARefresh
                  or (not FDetailsOpened)
                  or (not VarIsEqual(DetailSetProvider.Params.ParamValues[DetailSetProvider.DetailFields], LValues))
      then begin
        CloseDetails;
        DetailSetProvider.Params.ParamValues[DetailSetProvider.DetailFields] := LValues;

        if TimerDetails.Interval > 0 then
          TimerDetails.Enabled := True
        else
          TimerDetails.OnTimer(TimerDetails);
      end;
    end else
      CloseDetails;
  end;
end;

procedure TSBaseXXViewFrm.TimerTimerDetails(Sender: TObject);
begin
  TimerDetails.Enabled := False;
{$IFDEF FASTSCRIPT_RTTI}
  if not Assigned(FOpenDetails) then
    DetailSetProvider.Open
  else
    FOpenDetails.Value;
{$ELSE}
  DetailSetProvider.Open;
{$ENDIF}

  FDetailsOpened := True;
end;

procedure TSBaseXXViewFrm.CloseDetails;
begin
  TimerDetails.Enabled := False;
  FDetailsOpened := False;
{$IFDEF FASTSCRIPT_RTTI}
  if not Assigned(FCloseDetails) then
    DetailSetProvider.Close
  else
    FCloseDetails.Value;
{$ELSE}
  DetailSetProvider.Close;
{$ENDIF}
end;

procedure TSBaseXXViewFrm.GetCaptionParamByName(const AParamName: String; var ADisplayText: String; var AFound: Boolean);
var
  I: Integer;
begin
  if Assigned(FAutoFilterItems) then
    for I := 0 to FAutoFilterItems.Count - 1 do
      with FAutoFilterItems[I] do
        if SameText(AParamName, FParamName) then begin
          if Enabled then
            ADisplayText := InternalGetCaption
          else
            ADisplayText := '';
          AFound := True;
          Exit;
        end;

  inherited;
end;

procedure TSBaseXXViewFrm.mnSelectClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  InternalSelect(True)
end;

procedure TSBaseXXViewFrm.mnTakeClick(Sender: TObject);
begin
  InternalSelect(False);
end;

procedure TSBaseXXViewFrm.InternalSelect(ACloseForm: Boolean);
var
  I, J, K: Integer;
  LResultFields, LUnknownResultFields: String;
  LVarParams: TNamedVariants;
  LResultValues: TNamedVariants;
  LCollisions: Boolean;
  LCallBackMode: Boolean;
  LResultCount: Integer;
begin
  ResultValues.Clear;
  ResultValues.UnknownAsNull := False;

  LVarParams.UnknownAsNull := True;
  LCollisions := False;
  InternalGetResults(LVarParams, LCollisions);
  Assert(not LCollisions);

  if SelectMode in [smMultiple, smMultipleRepeateAble] then begin
//    Assert((@FSelectCallBackProc <> nil) or (FSelectCallBackScript <> ''));
    LCallBackMode := (@FSelectCallBackProc <> nil) or (FSelectCallBackScript <> '');
    LResultValues.UnknownAsNull := False;
//    LResultValues.Clear;

//    InternalDisableControls;
//    try

      if LCallBackMode then
        ExecuteSelectCallBackProcedures(cbsaMultipleBegin, LResultValues { Empty Set }, Owner, FSelectCallBackProc, FSelectCallBackScript);
      LResultCount := 0;

      for I := 0 to InternalGetGridRecordCount - 1 do
        if InternalGetGridRecordChecked(I) then begin
          LResultValues.Clear;
          LResultValues.WriteValues(ResultFields, InternalGetGridFieldValues(I, ResultFields, LUnknownResultFields), True);
          if (not LUnknownResultFields.IsEmpty) then begin
            LResultFields := LUnknownResultFields;
            LResultValues.WriteValues(LResultFields, LVarParams.ReadValues(LResultFields, LUnknownResultFields, True), True);
          end;

          if LCallBackMode then
            ExecuteSelectCallBackProcedures(cbsaMultipleRecord, LResultValues, Owner, FSelectCallBackProc, FSelectCallBackScript)
          else begin
            for J := 0 to LResultValues.Count - 1 do
              with LResultValues.Items[J] do
                if LResultCount = 0 then
                  ResultValues[Name] := VarArrayOf([Value])
                else begin
                  Assert(ResultValues.Find(Name, K));
                  ResultValues.SetValue(K, VarArraysConcat(ResultValues.Items[K].Value, Value), amOverride);
                end;
            Inc(LResultCount);
          end;
        end;
      if LCallBackMode then
        ExecuteSelectCallBackProcedures(cbsaMultipleEnd, LResultValues { Empty set }, Owner, FSelectCallBackProc, FSelectCallBackScript);

//    finally
//      InternalEnableControls;
//    end;
  end else begin
    ResultValues.WriteValues(ResultFields, InternalGetGridFieldValues(ResultFields, LUnknownResultFields), True);
    if (not LUnknownResultFields.IsEmpty) then begin
      LResultFields := LUnknownResultFields;
      ResultValues.WriteValues(LResultFields, LVarParams.ReadValues(LResultFields, LUnknownResultFields, True), True);
    end;

    if (@FSelectCallBackProc <> nil) or (not FSelectCallBackScript.IsEmpty) then begin
      if (@FSelectCallBackProc <> nil) then
        FSelectCallBackProc(cbsaSingleRecord, ResultValues);

      if (not FSelectCallBackScript.IsEmpty) then begin
        ResultValues.SetValue(SConst_Action, cbsaSingleRecord);
{$IFDEF FASTSCRIPT_RTTI}
        CallFunction(FSelectCallBackScript, ResultValues);
{$ENDIF}
      end;

      ResultValues.Clear;
    end;
  end;

  if ACloseForm then
    ModalResult := mrOk;
end;

function TSBaseXXViewFrm.GetFormParamValues(const AFieldNames: String; var AUnknownFields: String): Variant;
var
  LVarParams: TNamedVariants;
  LCollisions: Boolean;
begin
  LVarParams.UnknownAsNull := True;

  { Получим параметры формы }
  LCollisions := False;
  InternalGetParameters(255, LVarParams, LCollisions);
//  Assert(not LCollisions);
  Result := LVarParams.ReadValues(AFieldNames, AUnknownFields, True);
end;

procedure TSBaseXXViewFrm.GridAfterUpdateActions(Sender: TObject);
begin
  if (not FDeletedCondition.IsEmpty) then
    if (InternalGetGridRecordCount = 0) or (not DataSetProvider.ActiveDataSet.EvaluateBoolean(FDeletedCondition)) then begin
      mnMarkAsDeleted.Caption     := SBaseProperties.Text_MenuMarkAsDeleted;
      mnMarkAsDeleted.ImageIndex  := SBaseFormProperties.IconIndex_MarkAsDeleted;
      mnMarkAsDeleted.Tag         := 1;
      mnMarkAsDeleted.ShortCut    := ShortCut_Shift_Del;
    end else begin
      mnMarkAsDeleted.Caption     := SBaseProperties.Text_MenuUnMarkFromDeleted;
      mnMarkAsDeleted.ImageIndex  := SBaseFormProperties.IconIndex_UnMarkFromDeleted;
      mnMarkAsDeleted.Tag         := 0;
      mnMarkAsDeleted.ShortCut    := 0;
    end;
end;

procedure TSBaseXXViewFrm.DBGridDataChanged(Sender: TObject);
begin
  if not Loading then begin
    DBGridAfterFocusedRecordChanged(False);
    ShowRecordCount
  end;
end;

//procedure TSBaseXXViewFrm.DBGridXXKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
//begin
//    if (SelectMode = smNone) then
//      mnView.Click
//    else if mnSelect.Enabled then begin
//      if (SelectMode = smSingleRepeateAble) and (mnSelect.Visible <> ivNever) and (ssCtrl in Shift) then
//        mnSelectClick(nil)
//      else
//        mnSelect.Click
//    end;
//end;

procedure TSBaseXXViewFrm.InternalGetParameters(ALevel: UInt16; var AParameters: TNamedVariants; var ACollisions: Boolean);
  procedure SetParamValue(const AName: String; const AValue: Variant);
  var
    Idx: Integer;
  begin
    if (AParameters.Find(AName, Idx) and not VarSameValue(AParameters.Items[Idx].Value, AValue)) then
      ACollisions := True
    else
      AParameters[AName] := AValue;
  end;

var
  I, Idx: Integer;
  LParamName: String;
  LParamValue, LParamItemValue: Variant;
begin
//  Collisions        := True;
//  LCollisionsFound  := False;

  { 1. }
  inherited;

  { 2. Дополнительные фильтры }
  for I := Pred(FAutoFilterItems.Count) downto 0 do
    with FAutoFilterItems[I] do
      if Enabled and (not ParamName.IsEmpty) and (Level < ALevel) then begin
        LParamName := ParamName;
        LParamValue:= ParamValue;

        if (not AllowNull) and (not VarArrayIsPresent(LParamValue)) then
          ACollisions := True
        else if not VarIsEmpty(LParamValue) then begin
          if Pos(';', LParamName) = 0 then
            SetParamValue(LParamName, LParamValue)
          else begin
            Idx := 0;
            LParamItemValue := Null;

            while (LParamName <> '') do begin
              if VarIsArray(LParamValue) then
                LParamItemValue := LParamValue[Idx];

              SetParamValue(ExtractAndRemoveValue(LParamName, ';', ftString), LParamItemValue);
              Inc(Idx);
            end;
          end;
        end;
      end;

// 3.
  if ALevel > DataSetProvider.Level then
    InternalGetGridFilter(@AParameters);
end;

procedure TSBaseXXViewFrm.InternalGetResults(var AParameters: TNamedVariants; var ACollisions: Boolean);
begin
  InternalGetParameters(DataSetProvider.Level, AParameters, ACollisions);
end;

procedure TSBaseXXViewFrm.InternalGetGridFilter(AFilter: PNamedVariants);
begin
  { Abstract }
end;

function TSBaseXXViewFrm.InternalGetLocalFilter(var AFilter: TNamedVariants): String;
var
  LField: TField;
  LValue: Variant;
  LName: String;
  LFilterLiteral, LFilterValue: String;
  LFilterLiteralChar: Char;
begin
  Result := '';
  if not FAutoLocalFilter then Exit;

  { Построение локального фильтра }
  while AFilter.Count > 0 do begin
    with AFilter.Items[0] do begin
      LName  := Name;
      LValue := Value;
    end;

    if not Assigned(DataSetProvider.Params.FindParam(LName)) and not Assigned(DataSetProvider.Params.FindParam(SConst_Param + '_' + LName)) then begin
      LField := DataSetProvider.FindField(LName);
      if Assigned(LField) then begin
        Result := ConcatIfNotEmpty(Result, ' ' + SConst_AND + ' ', DataSetFieldQuoteName(LName));

        LFilterValue := VarToStr(LValue);
        LFilterLiteral := LeftStr(LFilterValue, 2);

        LFilterLiteralChar := #0;
        if LFilterLiteral <> '' then
          if (CompareStr(LFilterLiteral, '<>') = 0) or (CompareStr(LFilterLiteral, '!=') = 0) then begin
            LFilterLiteralChar := '!';
            Delete(LFilterValue, 1, 2);
            Result := Result + '<>';
          end else if CompareStr(LFilterLiteral, '~=') = 0 then begin
            LFilterLiteralChar := '~';
            Delete(LFilterValue, 1, 2);
            Result := Result + ' ' + SConst_LIKE + ' ';
          end else if CharInSet(LFilterLiteral[1], ['>', '<', '=']) then begin
            LFilterLiteralChar := LFilterLiteral[1];
            Delete(LFilterValue, 1, 1);
            Result := Result + LFilterLiteralChar;
          end else if LField.DataType in [ftString, ftWideString] then
            Result := Result + ' ' + SConst_LIKE + ' '
          else
            Result := Result + '=';

        case LField.DataType of
          ftString, ftWideString:
            begin
              if (LFilterLiteralChar = '~') or (Pos('%', LFilterValue) > 0) then
                LFilterValue := StringReplace(LFilterValue, '%', '*', [rfReplaceAll, rfIgnoreCase])
              else if Pos('*', LFilterValue) = 0 then
                LFilterValue := LFilterValue + '*';

              Result := Result + QuotedStr(LFilterValue);
            end;
          ftSmallint, ftWord, ftInteger, ftLargeint:
            begin
              Result := Result + IntToStr(StrToInt(LFilterValue));
            end;
          ftFloat, ftExtended, ftCurrency:
            begin
              Result := Result + FloatToStr(StrToFloat(LFilterValue));
            end;
          ftBCD, ftFMTBCD:
            begin
              Result := Result + BcdToStr(StrToBcd(LFilterValue));
            end
          else
            Result := Result + LFilterValue;
        end;
      end;
    end;
    AFilter.Delete(0);
  end;
end;

procedure TSBaseXXViewFrm.InternalSetFilter;
var
  I, Ix: Integer;
  LFilter: TNamedVariants;
  LParam: TParam;
  LServerFilterChanged, LOpenRequired: Boolean;
  LValue: Variant;
  LName: String;
  LLocalFilter: String;
  LCollisions: Boolean;
{ !!! Test !!!
  LPositionSaved: Boolean;
  LBookmark: TDataSourceBookmark;
}
begin
  { Возможно неверное место }
{$IFDEF FASTSCRIPT_RTTI}
  CallFunction(FInternalSetFilter);
{$ENDIF}

  LFilter.UnknownAsNull := True;

  { Получаем фильтр. Внимание !!! ВСЕ параметры DataSource.DataSet не пришедшие в LFilter будут обнулены !!! }
  LCollisions := False;
  InternalGetParameters(DataSetProvider.Level, LFilter, LCollisions);
  Collisions := LCollisions;

  if Collisions then
    { Если были коллизии, то датасет открыт не будет }
    CloseDataSet
  else begin
    LServerFilterChanged  := False;

    { Реинициализация параметров DataSetProvider }
    for I := Pred(DataSetProvider.Params.Count) downto 0 do with DataSetProvider.Params[I] do if (ParamType <> ptOutput) then begin
      LName := RemovePrefixIfPresent(Name, SConst_Param + '_');
      LValue:= Value;
      if LFilter.Find(LName, Ix) then begin
        if not VarIsEqual(LValue, LFilter.Items[Ix].Value) then begin
          Value := LFilter.Items[Ix].Value;
          LServerFilterChanged := True;
        end;
        LFilter.Delete(Ix);
      end else begin
        if not IsNull then begin
          Clear;
          LServerFilterChanged := True;
        end;
      end;
    end;

    { Реинициализация параметров DetailSetProvider }
    for I := Pred(DetailSetProvider.Params.Count) downto 0 do
      with DetailSetProvider.Params[I] do
       if (ParamType <> ptOutput) and (not InArray(DetailSetProvider.DetailFields, Name, ';')) then begin
          LParam := DataSetProvider.Params.FindParam(Name);
          if LParam = nil then begin
            LName := RemovePrefixIfPresent(Name, SConst_Param + '_');
            LParam := DataSetProvider.Params.FindParam(LName);
            if (LParam = nil) and (LName = Name) then
              LParam := DataSetProvider.Params.FindParam(SConst_Param + '_' + Name);
          end;

          if LParam = nil then
            Clear
          else
            Value := LParam.Value
    end;

    DataSetProvider.DisableControls;
    try
      if LServerFilterChanged then begin
        LOpenRequired := DataSetProvider.Active or ((not Loading) and (mnSubmit.Visible = ivNever));
        CloseDataSet;
      end else
        LOpenRequired := (not Loading) and (not DataSetProvider.Active) and (mnSubmit.Visible = ivNever);

  { !!! Test !!!
      if LOpenRequired and (InternalGetGridRecordCount > 0) then begin
        InternalSaveGridPosition(LBookmark);
        LPositionSaved := True;
      end;
  }
      if LOpenRequired then begin
        DataSetProvider.Filtered := False;
        OpenDataSet;
      end;

      LLocalFilter := InternalGetLocalFilter(LFilter);

      if LLocalFilter.IsEmpty then begin
        DataSetProvider.Filtered := False;
        DataSetProvider.Filter   := '';
      end else begin
        DataSetProvider.Filter   := LLocalFilter;
        DataSetProvider.Filtered := True;
      end;
    finally
      DataSetProvider.EnableControls;
    end;

    if (InternalGetGridRecordCount > 0) then
      InternalSetGridFirstRow

    else if not LOpenRequired then // TO DO: ???
      ShowRecordCount;

{ !!! Test !!!
    else if LPositionSaved and (InternalGetGridRecordCount > 0) then
      InternalRestoreGridPosition(LBookmark);
}
  end;
end;

procedure TSBaseXXViewFrm.mnFormCloseClick(Sender: TObject);
begin
  if (mnSelect.Visible <> ivNever) then Close;
end;

function TSBaseXXViewFrm.mnTakeOnGetEnabled(Sender: TObject): Boolean;
begin
  Result := mnSelect.Enabled;
end;

procedure TSBaseXXViewFrm.mnExcelClick(Sender: TObject);
begin
  if (not Busy) and (InternalGetGridRecordCount > 0) then
    InternalExcelExport;
end;

procedure TSBaseXXViewFrm.SetRecordsPrivileges(const AValue: Word);
var
  LDataSourceControlProperties: TDataSourceControlProperties;
  LLinkedComponent: TComponent;
begin
  FLoadedRecordsPrivileges := AValue;
  if Loading or (csReading in ComponentState) then begin
    FRecordsPrivilegesStored := True;
    Exit;
  end;

  FRecordsPrivilegesStored := True;
  FRecordsPrivileges := AValue;

  LLinkedComponent := InternalGetGrid;

  if FRecordsPrivileges and Privilege_Select_Bit = 0 then begin
    ItemsReactions.SetActive(mnView, False);
    mnView.Enabled := False;
  end else
    with ItemsReactions.FindOrCreateReaction(mnView) do begin
      LinkedComponent := LLinkedComponent;
      Default := (SelectMode = smNone);
      OnGetEnabled.Active := True;
      OnClick.Reaction  := biraCustomDataSourceRecord;
    end;

  if FRecordsPrivileges and Privilege_Insert_Bit = 0 then begin
    ItemsReactions.SetActive(mnNew, False);
    mnNew.Enabled   := False;
    ItemsReactions.SetActive(mnCopy, False);
    mnCopy.Enabled  := False;
  end else begin
    with ItemsReactions.FindOrCreateReaction(mnNew, biraDataSourceInPlaceInsert) do begin
      LinkedComponent := LLinkedComponent;
      OnGetEnabled.Active := True;
      if OnClick.Reaction = biraNone then OnClick.Reaction := biraCustomDataSourceActive;
    end;
    with ItemsReactions.FindOrCreateReaction(mnCopy, biraDataSourceInPlaceCopy) do begin
      LinkedComponent := LLinkedComponent;
      OnGetEnabled.Active := True;
      if OnClick.Reaction = biraNone then OnClick.Reaction := biraCustomDataSourceRecord;
    end;
  end;

  if FRecordsPrivileges and Privilege_Update_Bit = 0 then begin
    ItemsReactions.SetActive(mnEdit, False);
    mnEdit.Enabled  := False;
  end else begin
    with ItemsReactions.FindOrCreateReaction(mnEdit, biraDataSourceInPlaceEdit) do begin
      LinkedComponent := LLinkedComponent;
      OnGetEnabled.Active := True;
      if OnClick.Reaction = biraNone then OnClick.Reaction := biraCustomDataSourceRecord;
    end;
  end;

  if FRecordsPrivileges and Privilege_Delete_Bit = 0 then begin
    ItemsReactions.SetActive(mnDelete, False);
    mnDelete.Enabled := False;
  end else begin
    with ItemsReactions.FindOrCreateReaction(mnDelete, biraDataSourceInPlaceDelete) do begin
      LinkedComponent := LLinkedComponent;
      OnGetEnabled.Active := True;
      if OnClick.Confirmation.IsEmpty then
        OnClick.Confirmation := SDeleteRecordQuestion;
      if OnClick.Reaction = biraNone then OnClick.Reaction := biraCustomDataSourceRecord;
    end;
  end;

  ItemsReactions.DataSourceOnUpdate(InternalGetGrid, [dsusActive, dsusData, dsusFocusedRecord]);
  LDataSourceControlProperties := FDataSourceControls.FindGridControl(InternalGetGrid);
  if Assigned(LDataSourceControlProperties) and Assigned(LDataSourceControlProperties.AfterUpdate) then
    LDataSourceControlProperties.AfterUpdate(InternalGetGrid);
//  end;
end;

{ Class TSBaseViewFilter }

constructor TSBaseViewFilter.Create(AOwner: TComponent);
begin
  FInternalEnabled  := True;
  FInternalVisible  := True;
  FAllowNull        := True;
  FStore            := False;

  if AOwner is TSBaseXXViewFrm then
    FBaseXXViewForm := TSBaseXXViewFrm(AOwner)
  else if AOwner.Owner is TSBaseXXViewFrm then
    FBaseXXViewForm := TSBaseXXViewFrm(AOwner.Owner)
  else
    Raise Exception.Create('Owner or Parent Owner Form must be TSBaseXXViewFrm.');

  inherited
end;

procedure TSBaseViewFilter.SetParentComponent(Value: TComponent);
begin
  if FParent <> nil then
    Raise Exception.Create('Parent component changing is not allowed');

  inherited;
  FParent := Value;

  if Assigned(FParent) then begin
    InternalCreate;

    if FControl <> nil then begin
      InternalSetEnabled(FInternalEnabled);
      InternalSetVisible(FInternalVisible);
      FBaseXXViewForm.RegisterFilterItem(Self);
    end;
  end;
end;

function TSBaseViewFilter.GetParentComponent: TComponent;
begin
  Result := FParent
end;

function TSBaseViewFilter.HasParent: Boolean;
begin
  Result := Assigned(FParent)
end;

procedure TSBaseViewFilter.OnViewFilterItemChanged(Sender: TObject);
begin
  if (not FInternalUpdate) and Assigned(FOnFilterValueChanged) then
    FOnFilterValueChanged(Self);

  FBaseXXViewForm.AfterFilterChanged;
end;

function TSBaseViewFilter.GetControlProperties: TcxCustomEditProperties;
begin
  if FControl is TEditLayoutPanel then
    Result := TcxCustomEditCrack(TEditLayoutPanel(FControl).EditControl).Properties
  else if (FControl is TcxBarEditItem) then with TcxBarEditItem(FControl) do
    Result := TcxBarEditItem(FControl).Properties
  else
    Result := nil;

  //  Assert(Assigned(Result));
end;

function TSBaseViewFilter.GetEditControl: TComponent;
begin
  if FControl is TEditLayoutPanel then
    Result := TEditLayoutPanel(FControl).EditControl
  else if (FControl is TdxBarItem) then
    Result := FControl
  else
    Result := nil;

  Assert(Assigned(Result));
end;

function TSBaseViewFilter.GetParamValues(Index: String): Variant;
var
  LUnknownNames: String;
begin
  Result := VarArrayRead(FParamName, ParamValue, Index, LUnknownNames);
  if (not LUnknownNames.IsEmpty) then
    Raise Exception.Create(Name + ': Unknown filter parameters "' + LUnknownNames + '"');
end;

procedure TSBaseViewFilter.SetParamValues(AName: String; const AValue: Variant);
var
  LUnknownNames: String;
  LParamValue: Variant;
begin
  LUnknownNames := ArraysAntiJoin(AName, FParamName, ';');
  if (not LUnknownNames.IsEmpty) then
    Raise Exception.Create(Name + ': Unknown filter parameters "' + LUnknownNames + '"');

  LParamValue := ParamValue;
  VarArrayWrite(FParamName, LParamValue, AName, AValue);
  ParamValue := LParamValue;
end;

procedure TSBaseViewFilter.SetControlProperties(const Value: TcxCustomEditProperties);
begin
  GetControlProperties.Assign(Value);
end;

function TSBaseViewFilter.InternalGetCaption: String;
var
  LParamValue: Variant;
  LIdx: Integer;
begin
  LParamValue := ParamValue;
  LIdx := Pos('|', FDisplayFormat);

  if not VarArrayIsPresent(LParamValue) then begin
    if LIdx > 0 then
      Result := Copy(FDisplayFormat, LIdx + 1, MAXSHORT)
    else
      Result := '';
  end else begin
    if VarIsArray(LParamValue) then
      Result := '(Array)'
    else
      Result := VarToStr(LParamValue);

    if LIdx > 1 then
      Result := Format(Copy(FDisplayFormat, 1, LIdx - 1), [Result]);
  end;
end;

function TSBaseViewFilter.InternalGetEnabled: Boolean;
begin
  if FControl = nil then
    Result := FInternalEnabled
  else if FControl is TdxBarItem then
    Result := TdxBarItem(FControl).Enabled
  else
    Result := TControl(FControl).Enabled
end;

procedure TSBaseViewFilter.InternalSetEnabled(const Value: Boolean);
begin
  if FControl = nil then
    FInternalEnabled := Value
  else begin
    if FControl is TdxBarItem then
      TdxBarItem(FControl).Enabled := Value
    else
      TControl(FControl).Enabled := Value;
  end;
end;

function TSBaseViewFilter.InternalGetVisible: Boolean;
begin
  if FControl = nil then
    Result := FInternalVisible
  else if FControl is TdxBarItem then
    Result := TdxBarItem(FControl).Visible <> ivNever
  else
    Result := TControl(FControl).Visible
end;

procedure TSBaseViewFilter.InternalSetVisible(const Value: Boolean);
begin
  if FControl = nil then
    FInternalVisible := Value
  else begin
    if FControl is TdxBarItem then
      with TdxBarItem(FControl) do
        if Value then
          Visible := ivAlways
        else
          Visible := ivNever
    else
      TControl(FControl).Visible := Value;
  end;
end;

procedure TSBaseViewFilter.InternalSaveConfig;
var
  I: Integer;
  LParamNames: String;
  LParamValues: Variant;
begin
  I := 0;
  LParamNames  := ParamName;
  LParamValues := ParamValue;

  while LParamNames <> '' do begin
    FBaseXXViewForm.SetSetting(Name + '.' + ExtractAndRemoveValue(LParamNames, ';'), VarArrayGetValueByIndex(LParamValues, I));
    Inc(I);
  end;
end;

procedure TSBaseViewFilter.InternalLoadConfig;
var
  I: Integer;
  LParamNames: String;
  LParamValue, LParamValues: Variant;
begin
  I := 0;
  LParamNames := ParamName;
  LParamValues := Null;

  while LParamNames <> '' do begin
    LParamValue := FBaseXXViewForm.GetSetting(Name + '.' + ExtractAndRemoveValue(LParamNames, ';'));

    if (I = 0) then
      LParamValues := LParamValue
    else if (I = 1) then
      LParamValues := VarArrayOf([LParamValues, LParamValue])
    else begin
      VarArrayRedim(LParamValues, I);
      LParamValues[I] := LParamValue;
    end;

    Inc(I);
  end;

  if VarArrayIsPresent(LParamValues) then
    ParamValue := LParamValues;
end;

procedure TSBaseViewFilter.InternalSetAllowNull(const AValue: Boolean);
begin
  FAllowNull := AValue;
end;

procedure TSBaseViewFilter.SetParamName(const Value: String);
var
  Idx, LCount: Integer;
begin
  if FParamName <> Value then try
    FInternalUpdate := True;

    FParamName := Value;
    Idx := Pos(';', FParamName);
    if Idx = 0 then
      InternalSetParamValue(Null)
    else begin
      LCount := 0;
      while Idx > 0 do begin
        Inc(LCount);
        Idx := PosEx(';', FParamName, Idx + 1);
      end;
      InternalSetParamValue( VarArrayCreate([0, LCount], varVariant) );
    end;
  finally
    FInternalUpdate := False;
  end;
end;

{ TSBaseViewFilterCxEdit }

procedure TSBaseViewFilterCxEdit.InternalCreate;
begin
  if FParent is TdxBarManager then begin
    FControl := TcxBarEditItem.Create(FBaseXXViewForm);
    with TcxBarEditItem(FControl) do begin
      PropertiesClass := Self.GetPropertiesClass;
      ShowCaption     := True;
      OnChange        := OnViewFilterItemChanged;
    end;
  end else begin
    FControl := TEditLayoutPanel.Create(Self);
    TEditLayoutPanel(FControl).EditControl := Self.GetEditClass.Create(FBaseXXViewForm);
    with TcxCustomEditCrack(TEditLayoutPanel(FControl).EditControl) do begin
      Properties.OnEditValueChanged := OnViewFilterItemChanged;
      Parent := TLayoutPanel(FControl);
      Align := alClient;
    end;
  end;

  InternalInitProperties;

  TComponentCrack(FControl).SetParentComponent(FParent);
end;

function TSBaseViewFilterCxEdit.InternalGetCaption: String;
var
  LcxCustomEditProperties: TcxCustomEditProperties;
begin
  LcxCustomEditProperties := GetProperties;
  if LcxCustomEditProperties <> nil then
    Result := LcxCustomEditProperties.GetDisplayText(ParamValue, True, False)
  else
    Result := '';
end;

function TSBaseViewFilterCxEdit.InternalGetParamValue: Variant;
begin
  if FControl <> nil then begin
    if FControl is TcxBarEditItem then
      Result := TcxBarEditItem(FControl).EditValue
    else
      Result := TcxCustomEdit(TEditLayoutPanel(FControl).EditControl).EditValue;
  end else
    Result := unAssigned;
end;

function TSBaseViewFilterCxEdit.GetProperties: TcxCustomEditProperties;
begin
  if FControl <> nil then begin
    if FControl is TcxBarEditItem then
      Result := TcxBarEditItem(FControl).Properties
    else
      Result := TcxCustomEditCrack(TEditLayoutPanel(FControl).EditControl).Properties
  end else
    Result := nil;
end;

procedure TSBaseViewFilterCxEdit.InternalInitProperties;
begin
//
end;

procedure TSBaseViewFilterCxEdit.InternalSetParamValue(const AValue: Variant);
begin
  if FControl <> nil then
    if FControl is TcxBarEditItem then
      TcxBarEditItem(FControl).EditValue := AValue
    else
      TcxCustomEdit(TEditLayoutPanel(FControl).EditControl).SetAndPostEditValue(AValue);
end;

{ TSBaseViewFilterText }

class function TSBaseViewFilterText.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxTextEdit
end;

class function TSBaseViewFilterText.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxTextEditProperties
end;

function TSBaseViewFilterText.InternalGetParamValue: Variant;
begin
  Result := (inherited InternalGetParamValue);
  if VarIsStr(Result) and (Result = '') then
    Result := Null;
end;

{ TSBaseViewFilterSpin }

class function TSBaseViewFilterSpin.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxSpinEdit
end;

class function TSBaseViewFilterSpin.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxSpinEditProperties
end;

{ TSBaseViewFilterDate }

class function TSBaseViewFilterDate.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxDateEdit
end;

class function TSBaseViewFilterDate.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxDateEditProperties
end;

procedure TSBaseViewFilterDate.InternalInitProperties;
begin
  inherited;

  with TcxDateEditProperties(GetProperties) do begin
    DateOnError := deNull;
    SaveTime    := False;
    ShowTime    := False;
    Kind        := ckDate;
    DateButtons := [btnClear, btnToday, btnOk];
  end;

  if FControl is TcxBarEditItem then
    TcxBarEditItem(FControl).Width := 80
  else
    TcxCustomEdit(FControl).Width := 85
end;

function TSBaseViewFilterDate.InternalGetCaption: String;
var
  LParamValue: Variant;
begin
  LParamValue := ParamValue;
  if VarIsType(LParamValue, varDate) then
    Result := DateToStr(VarToDateTime(LParamValue))
  else
    Result := '<Дата не выбрана>'
end;

{ TSBaseViewFilterDateTime }

procedure TSBaseViewFilterDateTime.InternalInitProperties;
begin
  with TcxDateEditProperties(GetProperties) do begin
    DateOnError := deNull;
    SaveTime    := True;
    ShowTime    := True;
    Kind        := ckDateTime;
    DateButtons := [btnClear, btnNow, btnToday, btnOk];
  end;

  if FControl is TcxBarEditItem then
    TcxBarEditItem(FControl).Width := 125
  else
    TcxCustomEdit(FControl).Width := 130
end;

{ TSBaseViewFilterDatesPeriod }

constructor TSBaseViewFilterDatesPeriod.Create(AOwner: TComponent);
begin
  FEmptyValue   := True;
  inherited;
end;

procedure TSBaseViewFilterDatesPeriod.InternalSetAllowNull(const AValue: Boolean);
begin
  inherited;
  if FAllowNull and FEmptyValue then begin
    FDateBegin    := Date;
    FDateEnd      := Date;
    FDatesPeriod  := sdkDay;
    FEmptyValue   := False;
  end;
end;

procedure TSBaseViewFilterDatesPeriod.InternalCreate;
begin
  if FParent is TdxBarManager then begin
    FControl := TdxBarButton.Create(FBaseXXViewForm);
    with TdxBarButton(FControl) do begin
      ImageIndex        := SBaseFormProperties.IconIndex_DatesPeriod;
      PaintStyle        := psStandard;
      OnClick           := ButtonClick;
    end;
  end else begin
    FControl := TSpeedButton.Create(FBaseXXViewForm);
    with TSpeedButton(FControl) do begin
      if Assigned(SBaseFormProperties.Icon16List) and (SBaseFormProperties.IconIndex_DatesPeriod <> -1) then
        SBaseFormProperties.Icon16List.GetBitmap(SBaseFormProperties.IconIndex_DatesPeriod, Glyph);
      OnClick           := ButtonClick;
    end;
  end;

  TComponentCrack(FControl).SetParentComponent(FParent);
end;

function TSBaseViewFilterDatesPeriod.InternalGetCaption: String;
var
  LIdx1, LIdx2: Integer;
begin
  LIdx1 := Pos('|', FDisplayFormat);
  if LIdx1 > 0 then
    LIdx2 := PosEx('|', FDisplayFormat, LIdx1 + 1)
  else
    LIdx2 := 0;

  if FEmptyValue then begin
    if LIdx2 > 0 then
      Result := Copy(FDisplayFormat, LIdx2 + 1, MAXSHORT)
    else
      Result := ''
  end else if FDateBegin = FDateEnd then begin
    Result := DateToStr(FDateBegin);
    if LIdx1 > 1 then
      Result := Format(Copy(FDisplayFormat, 1, LIdx1 - 1), [Result]);
  end else begin
    if LIdx1 > 1 then begin
      if LIdx2 = 0 then
        LIdx2 := MAXSHORT;
      Result := Format(Copy(FDisplayFormat, LIdx1 + 1, LIdx2 - LIdx1 - 1), [DateToStr(FDateBegin), DateToStr(FDateEnd)]);
    end else
      Result := DateToStr(FDateBegin) + '-' + DateToStr(FDateEnd)
  end;
end;

function TSBaseViewFilterDatesPeriod.InternalGetParamValue: Variant;
begin
  if FEmptyValue then
    Result := VarArrayOf([Null, Null])
  else
    Result := VarArrayOf([VarFromDateTime(FDateBegin), VarFromDateTime(FDateEnd)]);
end;

procedure TSBaseViewFilterDatesPeriod.InternalSetParamValue(const AValue: Variant);
begin
  if not (VarIsArray(AValue)) or (VarArrayLowBound(AValue, 1) <> 0) or (VarArrayHighBound(AValue, 1) <> 1) then
    Assert(False);
    //Raise Exception.Create(SError_ControlHaveIncorrentValue)

  if not VarIsPresent(AValue[0]) or not VarIsPresent(AValue[1]) then
    FEmptyValue := True
  else begin
    if not VarIsType(AValue[0], varDate) or not VarIsType(AValue[1], varDate) then
      Assert(False);
      //Raise Exception.Create(SError_ControlHaveIncorrentValue)

    FEmptyValue:= False;
    FDateBegin := VarToDateTime(AValue[0]);
    FDateEnd   := VarToDateTime(AValue[1]);
    if FDateBegin = FDateEnd then
      FDatesPeriod := sdkDay
    else
      FDatesPeriod := sdkInterval
  end;

  OnViewFilterItemChanged(nil);
end;

procedure TSBaseViewFilterDatesPeriod.ButtonClick(Sender: TObject);
begin
  if Assigned(Sender) then
    if SBaseSelectDatesPeriodForm.SelectDatesPeriod(FDatesPeriod, FDateBegin, FDateEnd) then
      OnViewFilterItemChanged(nil);
end;

{ TSBaseViewFilterTime }

class function TSBaseViewFilterTime.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxTimeEdit
end;

class function TSBaseViewFilterTime.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxTimeEditProperties
end;

procedure TSBaseViewFilterTime.InternalInitProperties;
begin
  inherited;

  if FControl is TcxBarEditItem then
    TcxBarEditItem(FControl).Width := 65
  else
    TcxCustomEdit(FControl).Width := 65
end;

{ TSBaseViewFilterLookupCxEdit }

procedure TSBaseViewFilterLookupCxEdit.InternalSetAllowNull(const AValue: Boolean);
begin
  inherited;

  Assert(Assigned(FControl) or (not AValue));
  with TcxCustomEditProperties(GetProperties) do
    if AValue then
      ClearKey := ShortCut_Del
    else
      ClearKey := 0
end;

{ Class TSBaseViewFilterLookupComboBox }

class function TSBaseViewFilterLookupComboBox.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxLookupComboBox
end;

class function TSBaseViewFilterLookupComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxLookupComboBoxProperties
end;

procedure TSBaseViewFilterLookupComboBox.InternalInitProperties;
begin
  inherited;
  with TcxLookupComboBoxProperties(GetProperties) do begin
    with ListOptions do begin
      ShowHeader              := False;
      GridLines               := glNone;
      ColumnSorting           := False;
      FocusRowOnMouseMove     := False;
    end;
    ImmediateDropDownWhenActivated := True;
  end;
end;

{ TSBaseViewFilterLookupTreeComboBox }

class function TSBaseViewFilterLookupTreeComboBox.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxMDLookupTreeComboBox
end;

class function TSBaseViewFilterLookupTreeComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMDLookupTreeComboBoxProperties
end;

procedure TSBaseViewFilterLookupTreeComboBox.InternalInitProperties;
begin
  inherited;
  with TcxMDLookupTreeComboBoxProperties(GetProperties) do begin
    ImmediateDropDownWhenActivated := True;
  end;
end;

{ TSBaseViewFilterCheckLookupTreeComboBox }

class function TSBaseViewFilterCheckLookupTreeComboBox.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxMDCheckLookupTreeComboBox
end;

class function TSBaseViewFilterCheckLookupTreeComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMDCheckLookupTreeComboBoxProperties
end;

procedure TSBaseViewFilterCheckLookupTreeComboBox.InternalInitProperties;
begin
  inherited;
  with TcxMDCheckLookupTreeComboBoxProperties(GetProperties) do begin
    ImmediateDropDownWhenActivated := True;
  end;
end;

{ Class TSBaseViewFilterCheckLookupComboBox }

class function TSBaseViewFilterCheckLookupComboBox.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxCheckLookupComboBox
end;

class function TSBaseViewFilterCheckLookupComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckLookupComboBoxProperties
end;

procedure TSBaseViewFilterCheckLookupComboBox.InternalInitProperties;
begin
  inherited;
  with TcxCheckLookupComboBoxProperties(GetProperties) do begin
    ImmediateDropDownWhenActivated := True;
  end;
end;

{ TdxBarButtonEx }

constructor TdxBarButtonEx.Create(AOwner: TComponent);
begin
  inherited;

  FValueChecked   := True;
  FValueUnchecked := False;
end;

function TdxBarButtonEx.GetEditValue: Variant;
begin
  if Down then
    Result := FValueChecked
  else if GroupIndex = 0 then
    Result := FValueUnchecked
  else
    Result := unAssigned
end;

procedure TdxBarButtonEx.SetEditValue(const Value: Variant);
begin
  Down := (Value = FValueChecked)
end;

{ TSBaseViewFilterButton }

procedure TSBaseViewFilterButton.InternalCreate;
begin
  if FParent is TdxBarManager then begin
    FControl := TdxBarButtonEx.Create(FBaseXXViewForm);
    with TdxBarButtonEx(FControl) do begin
      ButtonStyle       := bsChecked;
      PaintStyle        := psCaptionGlyph;
      OnClick           := OnViewFilterItemChanged;
    end;
  end else begin
    FControl := TSpeedButton.Create(FBaseXXViewForm);
    with TSpeedButton(FControl) do begin
      AllowAllUp        := True;
      GroupIndex        := -Integer(FControl);
      OnClick           := OnViewFilterItemChanged;
    end;
  end;

  TComponentCrack(FControl).SetParentComponent(FParent);
end;

procedure TSBaseViewFilterButton.InternalSetParamValue(const AValue: Variant);
begin
  if FControl <> nil then
    if FControl is TdxBarButton then
      TdxBarButtonEx(FControl).EditValue := AValue
    else
      TSpeedButton(FControl).Down := (AValue = True);
end;

function TSBaseViewFilterButton.InternalGetParamValue: Variant;
begin
  if FControl <> nil then
    if FControl is TdxBarButtonEx then begin
      Result := TdxBarButtonEx(FControl).EditValue
    end else
      Result := TSpeedButton(FControl).Down
  else
    Result := unAssigned
end;

function TSBaseViewFilterButton.InternalGetCaption: String;
var
  LDown: Boolean;
  LIdx: Integer;
begin
  if FControl is TdxBarButtonEx then begin
    LDown := TdxBarButtonEx(FControl).Down
  end else
    LDown := TSpeedButton(FControl).Down;

  LIdx := Pos('|', FDisplayFormat);
  if LDown then begin
    if LIdx <= 0 then
      Result := FDisplayFormat
    else
      Result := LeftStr(FDisplayFormat, Pred(LIdx));
  end else if LIdx > 0 then
    Result := Copy(FDisplayFormat, Succ(LIdx), MAXSHORT);
end;

{ TSBaseViewFilterCheckBox }

class function TSBaseViewFilterCheckBox.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxUserCheckBox;
end;

class function TSBaseViewFilterCheckBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxUserCheckBoxProperties
end;

function TSBaseViewFilterCheckBox.InternalGetCaption: String;
var
  LIdx: Integer;
begin
  LIdx := Pos('|', FDisplayFormat);

  if ParamValue = TcxUserCheckBoxProperties(GetProperties).ValueChecked then begin
    if LIdx <= 0 then
      Result := FDisplayFormat
    else
      Result := LeftStr(FDisplayFormat, Pred(LIdx));
  end else if LIdx > 0 then
    Result := Copy(FDisplayFormat, Succ(LIdx), MAXSHORT);
end;

procedure TSBaseViewFilterCheckBox.InternalInitProperties;
begin
  inherited;
  with TcxUserCheckBoxProperties(GetProperties) do begin
    ImmediatePost := True;
  end;
end;

{ TSBaseViewFilterRadioGroup }

class function TSBaseViewFilterRadioGroup.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxRadioGroup
end;

class function TSBaseViewFilterRadioGroup.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxRadioGroupProperties
end;

{ Class TSBaseViewFilterLookupComponent }

class function TSBaseViewFilterLookupComponent.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxButtonEdit
end;

class function TSBaseViewFilterLookupComponent.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxButtonEditProperties
end;

procedure TSBaseViewFilterLookupComponent.InternalSetAllowNull(const AValue: Boolean);
begin
  inherited;
{$IFDEF DEBUG}
  Assert(Assigned(FControl) or (not AValue));
{$ENDIF}
  TcxCheckLookupComboBoxProperties(GetProperties).Buttons[1].Visible := AValue;
end;

procedure TSBaseViewFilterLookupComponent.InternalSetParamValue(const AValue: Variant);
begin
  FLookupControl.ParamValue := AValue;
end;

function TSBaseViewFilterLookupComponent.InternalGetParamValue: Variant;
begin
  Result := FLookupControl.ParamValue
end;

procedure TSBaseViewFilterLookupComponent.InternalInitProperties;
begin
  inherited;
  with TcxButtonEditProperties(GetProperties) do begin
    ReadOnly := True;
  end;

  FLookupControl := TSBaseLookupComponentControl.Create(Owner);
  FLookupControl.Control := EditControl;
  FLookupControl.SelectButtonIndex := 0;
  FLookupControl.ClearButtonIndex  := 1;
  FLookupControl.OnParamValueChanged := OnViewFilterItemChanged;
end;

destructor TSBaseViewFilterLookupComponent.Destroy;
begin
  if (Owner = nil) or (csDestroying in Owner.ComponentState) then
    FLookupControl := nil
  else
    FreeAndNil(FLookupControl);

  inherited;
end;

procedure TSBaseViewFilterLookupComponent.SetMenu(const Value: TdxBarPopupMenu);
begin
  FLookupControl.SelectMenu := Value;
end;

procedure TSBaseViewFilterLookupComponent.SetParamName(const Value: String);
begin
  FLookupControl.ParamName := Value;
  inherited;
end;

procedure TSBaseViewFilterLookupComponent.SetExtractSQL(const Value: String);
begin
  FLookupControl.ExtractSQL := Value
end;

procedure TSBaseViewFilterLookupComponent.SetExtractScript(const Value: String);
begin
  FLookupControl.ExtractScript := Value;
end;

{ TSBaseViewFilterFile }

function TSBaseViewFilterFile.GetButtonClear: Boolean;
begin
  Result := TcxFileEditProperties(GetProperties).Buttons[2].Visible
end;

function TSBaseViewFilterFile.GetButtonDir: Boolean;
begin
  Result := TcxFileEditProperties(GetProperties).Buttons[0].Visible
end;

function TSBaseViewFilterFile.GetButtonFile: Boolean;
begin
  Result := TcxFileEditProperties(GetProperties).Buttons[1].Visible
end;

class function TSBaseViewFilterFile.GetEditClass: TcxCustomEditClass;
begin
  Result := TcxFileEdit
end;

class function TSBaseViewFilterFile.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxFileEditProperties
end;

procedure TSBaseViewFilterFile.InternalInitProperties;
begin
  inherited;

  with TcxFileEditProperties(GetProperties) do begin
    if Buttons.Count = 0 then Buttons.Add;
    with Buttons[0] do begin
      Visible     := False;
      Kind        := bkText;
      Caption     := '\';
      Hint        := SBaseProperties.Text_SubMenuFolder;
      EditAction  := feaDir
    end;

    if Buttons.Count = 1 then Buttons.Add;
    with Buttons[1] do begin
      Default     := True;
      Kind        := bkEllipsis;
      Hint        := SBaseProperties.Text_SubMenuFile;
      EditAction  := feaFile
    end;

    if Buttons.Count = 2 then Buttons.Add;
    with Buttons[2] do begin
      Visible     := False;
      Kind        := bkText;
      Caption     := 'X';
      Hint        := SBaseProperties.Text_MenuClear;
      EditAction  := feaClear;
    end;
  end;
end;

procedure TSBaseViewFilterFile.InternalSetAllowNull(const AValue: Boolean);
begin
  inherited;
{$IFDEF DEBUG}
  Assert(Assigned(FControl) or (not AValue));
{$ENDIF}
  if (not AValue) then
    TcxFileEditProperties(GetProperties).Buttons[2].Visible := False;
end;

procedure TSBaseViewFilterFile.SetButtonClear(const Value: Boolean);
begin
  TcxFileEditProperties(GetProperties).Buttons[2].Visible := Value
end;

procedure TSBaseViewFilterFile.SetButtonDir(const Value: Boolean);
begin
  TcxFileEditProperties(GetProperties).Buttons[0].Visible := Value
end;

procedure TSBaseViewFilterFile.SetButtonFile(const Value: Boolean);
begin
  TcxFileEditProperties(GetProperties).Buttons[1].Visible := Value
end;

{$IFDEF FASTSCRIPT_RTTI}
{ TFunctions_TSBaseXXViewFrm }

constructor TFunctions_TSBaseXXViewFrm.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseXXViewFrm, TSBaseXXViewFrm.ClassParent.ClassName) do begin
    AddMethod('function InternalGetGridKeyValues: Variant', Call_TSBaseXXViewFrm_Method);
    AddMethod('function InternalGetGridFieldValues(const AFieldNames: String; var AUnknownFields: String = ''): Variant', Call_TSBaseXXViewFrm_Method);
  end;

  with AScript.AddClass(TSBaseViewFilter, TSBaseViewFilter.ClassParent.ClassName) do begin
    AddEvent('OnFilterValueChanged', TfsNotifyEvent);
    AddProperty('ParamValue', DelphiTypeVariant, Get_TSBaseViewFilter_Property, Set_TSBaseViewFilter_Property);
    AddIndexProperty('ParamValues', DelphiTypeString, DelphiTypeVariant, Call_TSBaseViewFilter_Method, False);
    AddMethod('function DisplayText: String', Call_TSBaseViewFilter_Method);
  end;
end;

function TFunctions_TSBaseXXViewFrm.Call_TSBaseXXViewFrm_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  LUnknownFields: String;
begin
  if MethodName = 'INTERNALGETGRIDFIELDVALUES' then begin
    Result := TSBaseXXViewFrm(Instance).InternalGetGridFieldValues(VarToStr(Caller.Params[0]), LUnknownFields);
    if Caller.ParamCount > 1 then
      Caller.Params[1] := LUnknownFields;
  end else if MethodName = 'INTERNALGETGRIDKEYVALUES' then
    Result := TSBaseXXViewFrm(Instance).InternalGetGridKeyValues
end;

function TFunctions_TSBaseXXViewFrm.Get_TSBaseViewFilter_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'PARAMVALUE' then
    Result := TSBaseViewFilter(Instance).ParamValue
end;

procedure TFunctions_TSBaseXXViewFrm.Set_TSBaseViewFilter_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
  if PropName = 'PARAMVALUE' then
    TSBaseViewFilter(Instance).ParamValue := Value
end;

function TFunctions_TSBaseXXViewFrm.Call_TSBaseViewFilter_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'PARAMVALUES.GET' then
    Result := TSBaseViewFilter(Instance).ParamValues[VarToStr(Caller.Params[0])]
  else if MethodName = 'PARAMVALUES.SET' then
    TSBaseViewFilter(Instance).ParamValues[VarToStr(Caller.Params[0])] := Caller.Params[1]
  else if MethodName = 'DISPLAYTEXT' then
    Result := TSBaseViewFilter(Instance).DisplayText;
end;

{$ENDIF}

initialization
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TFunctions_TSBaseXXViewFrm);
{$ENDIF}

  dxBarRegisterItem(TdxBarButtonEx, TdxBarButtonControl, True);

  RegisterClasses
  (
    [
      TSBaseViewFilterText, TSBaseViewFilterSpin,
      TSBaseViewFilterDate, TSBaseViewFilterDateTime, TSBaseViewFilterTime, TSBaseViewFilterDatesPeriod,
      TSBaseViewFilterCheckBox,
      TSBaseViewFilterButton,
      TSBaseViewFilterRadioGroup,
      TSBaseViewFilterLookupComboBox, TSBaseViewFilterLookupTreeComboBox, TSBaseViewFilterCheckLookupTreeComboBox,
      TSBaseDBLookupComponentControl,
      TSBaseViewFilterCheckLookupComboBox,
      TSBaseViewFilterLookupComponent,
      TSBaseViewFilterFile,
      TTimer
    ]
  );

finalization
  UnRegisterClasses
  (
    [
      TSBaseViewFilterText, TSBaseViewFilterSpin,
      TSBaseViewFilterDate, TSBaseViewFilterDateTime, TSBaseViewFilterTime, TSBaseViewFilterDatesPeriod,
      TSBaseViewFilterCheckBox,
      TSBaseViewFilterButton,
      TSBaseViewFilterRadioGroup,
      TSBaseViewFilterLookupComboBox, TSBaseViewFilterLookupTreeComboBox, TSBaseViewFilterCheckLookupTreeComboBox,
      TSBaseDBLookupComponentControl,
      TSBaseViewFilterCheckLookupComboBox,
      TSBaseViewFilterLookupComponent,
      TSBaseViewFilterFile,
      TTimer
    ]
  );

{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TFunctions_TSBaseXXViewFrm);
{$ENDIF}

end.
