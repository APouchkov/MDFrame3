{ ****************************************************************************** }
{ DataHandler v1.1 }
{ }
{ }
{ build 2009-01-13 }
{ ****************************************************************************** }

{
  TODO:
  - Clone
  - DataHandler.Link (?)
  - Required fields (??)
  - Auto refresh (??)
}

unit DataHandler;
{$I config.inc}

interface

uses
  SysUtils, Classes, Controls, DB, Contnrs, Generics.Collections,
  ItemPropStorage, NamedVariables, FieldDataLink,
  cxStyles, dxBar, Types, SBaseDataModule, Math
{$IFNDEF PACKAGE}
  , SBaseDataSourceControls
  , SBaseForm
{$ENDIF}
  ;

type
  EDataHandler = class(Exception);

  TdhActionMode  = (amDefault, amInplace, amKeyFields, amKeyFieldsModal, amDataSet, amDataSetSelect);
  TdhMessageMode = (mInfo, mAsk, mAskCancel, mWarn, mErr);
  TdhMenuItemPos = (ipFirst, ipMiddle, ipLast);

  TdhActionKind  = (dhakCustom, dhakView, dhakNew, dhakEdit, dhakCopy);
  TdhActionDependence = (dhdActive, dhdData, dhdFocus);
  TdhActionDependencies = set of TdhActionDependence;

  TdhApplyAction = (dhaaNone, dhaaAppendRecord, dhaaUpdateRecord, dhaaAbort);

  TdhFieldNames = type string;

  TCustomDataHandler = class;
  TDataHandler = class;
  TSubDataHandler = class;
  TdhPropItem = class;
  TdhMultiNewItem = class;
  TdhAction = class;

  TCustomDataHandlerSupportClass = class of TCustomDataHandlerSupport;
  TCustomDataHandlerSupport = class abstract(TPersistent)
  strict private
    class var FSupports: TArray<TCustomDataHandlerSupportClass>;
  protected
    [weak] FOwner: TComponent;

//    procedure SetOwner(const Value: TComponent); dynamic; abstract;
//    procedure DoGenerateFirstAutoKeyField(ADataHandler: TCustomDataHandler); dynamic; abstract;
    procedure DoPostEditValues(ADataHandler: TCustomDataHandler); dynamic; abstract;
//    procedure DoApply(ADataHandler: TCustomDataHandler); dynamic; abstract;
    procedure DoCancel(ADataHandler: TCustomDataHandler); dynamic; abstract;
    procedure DoOnBeforeClose(ADataHandler: TCustomDataHandler); dynamic; abstract;
    procedure DoOnAfterClose(ADataHandler: TCustomDataHandler); dynamic; abstract;
//    procedure DoCommit(ADataHandler: TCustomDataHandler); dynamic; abstract;
    procedure DoOnAfterCommit(ADataHandler: TCustomDataHandler); dynamic; abstract;
//    procedure DoRestore(ADataHandler: TCustomDataHandler); dynamic; abstract;
//    procedure DoOnRowChanged(ADataHandler: TCustomDataHandler; IsAfterScroll: Boolean); dynamic; abstract;
    procedure DoNewObject(ADataHandler: TCustomDataHandler); dynamic; abstract;

    procedure DoView(ADataHandler: TSubDataHandler; const AVariables : TNamedVariants); dynamic; abstract;
    procedure DoNew(ADataHandler: TSubDataHandler; AItem: TdhMultiNewItem; var AVariables: TNamedVariants); dynamic; abstract;
    procedure DoEdit(ADataHandler: TSubDataHandler; var AVariables: TNamedVariants); dynamic; abstract;
    procedure DoCopy(ADataHandler: TSubDataHandler; var AVariables: TNamedVariants); dynamic; abstract;
//    procedure DoDelete(ADataHandler: TSubDataHandler; ACascade: Boolean); dynamic; abstract;

//    function  DoGetReadOnly(ADataHandler: TCustomDataHandler): Boolean; dynamic; abstract;
//    procedure DoSetReadOnly(ADataHandler: TCustomDataHandler; AReadOnly: Boolean); dynamic; abstract;
    procedure UpdateReadOnly(ADataHandler: TCustomDataHandler); dynamic; abstract;

    procedure DoOpen(ADataHandler: TCustomDataHandler); dynamic; abstract;
    procedure DoOnAfterOpen(ADataHandler: TCustomDataHandler); dynamic; abstract;
//    procedure DoRefresh(ADataHandler: TCustomDataHandler; ARecordOnly: Boolean); dynamic; abstract;
//    procedure DoResync(ADataHandler: TCustomDataHandler); dynamic; abstract;
//    procedure DoRollback(ADataHandler: TCustomDataHandler); dynamic; abstract;
    procedure DoValidate(ADataHandler: TCustomDataHandler); dynamic; abstract;
    procedure DoOptionsMenuNewItemsChanged(ADataHandler: TCustomDataHandler); dynamic; abstract;
//    procedure DoOptionsMenuChanged(ADataHandler: TCustomDataHandler); dynamic; abstract;

    procedure DoPerformAction(APropItem: TdhPropItem); dynamic; abstract;
    function DoShowMessage(const AText: string; AMode: TdhMessageMode; AControl: TObject = nil): TModalResult; dynamic; abstract;
//    function GetDataSetParams(ADataHandler: TCustomDataHandler): TParams; dynamic; abstract;
    function GetGrid(ADataHandler: TCustomDataHandler): TComponent; dynamic; abstract;
    function GetObjectValue(AName: string): Variant; dynamic; abstract;
//    function IsDataChanged(ADataHandler: TCustomDataHandler): Boolean; dynamic; abstract;
  public
    class procedure RegisterClass(AClass: TCustomDataHandlerSupportClass);
    class function RegisteredClassCount: Integer; inline;
    class function GetRegisteredClass(AIndex: Integer): TCustomDataHandlerSupportClass; inline;
    class function GetRegisteredClassDescription(AIndex: Integer): String; inline;
//    class procedure UnRegisterClass(AClass: TCustomDataHandlerSupportClass);
    class procedure ReleaseClasses;

    constructor Create(AOwner: TComponent); reintroduce; virtual;

    property Owner: TComponent read FOwner;
  end;

  TdhPersistent = class(TPersistent)
  private
    FDataHandler: TCustomDataHandler;
  protected
    function GetOwner: TPersistent; override;
  public
    property DataHandler: TCustomDataHandler read FDataHandler;
    constructor Create(ADataHandler: TCustomDataHandler); reintroduce; virtual;
  end;

  TdhCustomActionProperties = class;

  TdhParamEdit = class(TStringList)
  private
    FActionProperties : TdhCustomActionProperties;
//    FDataHandler      : TCustomDataHandler;
    FVarList          : TNamedVariants;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function VarItem(const AItem: String): TNamedVariant; dynamic; abstract;
  public
    constructor Create(AActionProperties : TdhCustomActionProperties);
    procedure Assign(Source: TPersistent); override;

    procedure AssignParams(const AValues: Array of TNamedVariant); overload;
    procedure AssignParams(const AValues: String); overload;

    procedure Add(const AName: String; const AValue: Variant); reintroduce;
    function DataHandler: TSubDataHandler; inline;
    function VarItems: TNamedVariants;
  end;

  TdhConstantParamEdit = class(TdhParamEdit)
  protected
    function VarItem(const AItem: String): TNamedVariant; override;
  end;

  TdhClassMenuActionProperties = class;
  TdhDynamicParamEdit = class(TdhParamEdit)
  protected
    function VarItem(const AItem: String): TNamedVariant; override;
  end;

  TdhAction = class(TdhPersistent)
  private
    function GetDataHandler: TSubDataHandler; inline;
  public
    constructor Create(ADataHandler: TSubDataHandler); reintroduce; virtual;
    property DataHandler: TSubDataHandler read GetDataHandler;
  end;

  TdhCustomMenuActionProperties = class;
  TdhGetEnabledFunction = function(AProperties: TdhCustomMenuActionProperties): Boolean of object;

  TdhCustomActionProperties = class(TdhPersistent)
  private
    function GetDataHandler: TSubDataHandler; inline;
  public
    function ScanFields: Boolean; dynamic; abstract;
    property DataHandler: TSubDataHandler read GetDataHandler;
  end;

  TdhCustomMenuActionProperties = class(TdhCustomActionProperties)
  private
    FEnabled: Boolean;
    FVisible: Boolean;
  protected
    function GetActionKind: TdhActionKind; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ADataHandler: TSubDataHandler); reintroduce; virtual;
    function Active: Boolean;
    function InternalVisible: Boolean; dynamic;

    property ActionKind: TdhActionKind read GetActionKind;
    property Visible: Boolean read FVisible write FVisible;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
  end;
  TdhCustomMenuActionPropertiesClass = type of TdhCustomMenuActionProperties;

  TdhOptionsMenuActionPropertiesOnGetEnabled = class(TPersistent)
  private
    FCondition      : String;
    FScriptFunction : String;
    FSystemFunction : TdhGetEnabledFunction;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property SystemFunction: TdhGetEnabledFunction read FSystemFunction write FSystemFunction;
  published
    property Condition      : String read FCondition      write FCondition;
    property ScriptFunction : String read FScriptFunction write FScriptFunction;
  end;

  TdhOptionsMenuActionProperties = class(TdhCustomMenuActionProperties)
  private
    FOnGetEnabled: TdhOptionsMenuActionPropertiesOnGetEnabled;
    procedure SetOnGetEnabled(const AValue: TdhOptionsMenuActionPropertiesOnGetEnabled);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ADataHandler: TSubDataHandler); override;
    destructor Destroy; override;
  published
    property OnGetEnabled: TdhOptionsMenuActionPropertiesOnGetEnabled read FOnGetEnabled write SetOnGetEnabled;
  end;

  TdhClassMenuActionProperties = class(TdhOptionsMenuActionProperties)
  private
    FParams     : TdhDynamicParamEdit;
    FConstants  : TdhConstantParamEdit;
    FActionMode : TdhActionMode;
    FClassName  : String;

    procedure SetConstants(const Value: TdhConstantParamEdit);
    procedure SetParams(const Value: TdhDynamicParamEdit);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ASubDataHandler: TSubDataHandler); override;
    destructor Destroy; override;

    function GetItems: TNamedVariants; dynamic;
    function GetClassName: string; dynamic;
    function GetActionMode: TdhActionMode; dynamic;
  published
{$WARN HIDING_MEMBER OFF}
    property ClassName: string read FClassName write FClassName;
{$WARN HIDING_MEMBER ON}

    property ActionMode : TdhActionMode read FActionMode write FActionMode default amDefault;
    property Params     : TdhDynamicParamEdit   read FParams    write SetParams;
    property Constants  : TdhConstantParamEdit  read FConstants write SetConstants;
  end;

  TdhMultiNewItems = class;

  TdhMultiNewActionProperties = class(TdhClassMenuActionProperties)
  private
    FdhMultiNewItem: TdhMultiNewItem;
  public
    function InternalVisible: Boolean; override;
  protected
    function GetActionKind: TdhActionKind; override;
  public
    constructor Create(AdhMultiNewItem: TdhMultiNewItem); reintroduce;

    function ScanFields: Boolean; override;
    function GetItems: TNamedVariants; override;
    function GetClassName: string; override;
    function GetActionMode: TdhActionMode; override;
    function MultiNewItems: TdhMultiNewItems; inline;
  published
    property Visible default True;
  end;

  TdhMultiNewItem = class(TCollectionItem)
  private
    FCaption: string;
    FOnClick: TNotifyEvent;

    FDependencies : TdhActionDependencies;
    FProperties   : TdhMultiNewActionProperties;

    FItems: TdhMultiNewItems;
    FTag  : Variant;

    procedure SetProperties(const Value: TdhMultiNewActionProperties);
    procedure SetItems(const Value: TdhMultiNewItems);

    function DefaultDependencies: TdhActionDependencies;
    function IsDependenciesStored: Boolean;
    procedure SetDependencies(const AValue: TdhActionDependencies);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function  GetDisplayName: String; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor  Destroy; override;
  published
    property Properties: TdhMultiNewActionProperties read FProperties write SetProperties;
    property Caption: String read FCaption write FCaption;
    property Dependencies: TdhActionDependencies read FDependencies write SetDependencies stored IsDependenciesStored;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property Items: TdhMultiNewItems read FItems write SetItems;
    property Tag: Variant read FTag write FTag;
  end;

  TdhMultiNewItems = class(TCollection)
  private
    FDataHandler: TSubDataHandler;
    FParentItem : TdhMultiNewItem;
  protected
    function  GetItem(AIndex: Integer): TdhMultiNewItem;
    procedure SetItem(AIndex: Integer; const AValue: TdhMultiNewItem);
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(ADataHandler: TSubDataHandler; AParentItem: TdhMultiNewItem); reintroduce;
    function Add: TdhMultiNewItem;

    property Items[Index: Integer]: TdhMultiNewItem read GetItem write SetItem; default;
  end;

  TdhDeleteActionProperties = class(TdhOptionsMenuActionProperties)
  private
    FConfirmText: string;
    function IsModifiedConfirmText: Boolean;
  public
    constructor Create(ADataHandler: TSubDataHandler); override;
  public
    function ScanFields: Boolean; override;
    function InternalVisible: Boolean; override;
  published
    property ConfirmText: string read FConfirmText write FConfirmText stored IsModifiedConfirmText;

    property Visible default True;
  end;

  TdhMultiNewItemsLoadMode = (dhlmAsField, dhlmAsSimpleObject);
  TdhNewActionProperties = class(TdhClassMenuActionProperties)
  private
    FItems: TdhMultiNewItems;
    FDependencies: TdhActionDependencies;

    procedure SetItems(const Value: TdhMultiNewItems);
    function  IsItemsStored: Boolean;
    procedure SetDependencies(const AValue: TdhActionDependencies);
  protected
    function GetActionKind: TdhActionKind; override;
  public
    function ScanFields: Boolean; override;
    function InternalVisible: Boolean; override;
  public
    constructor Create(ADataHandler: TSubDataHandler); override;
    destructor Destroy; override;

    procedure Load(ADataSet: TSQLDataSetProvider; const AFieldName: String; AMode: TdhMultiNewItemsLoadMode);
  published
    property Items: TdhMultiNewItems read FItems write SetItems stored IsItemsStored;
    property Dependencies: TdhActionDependencies read FDependencies write SetDependencies default [dhdActive];

    property Visible default True;
  end;

  TdhViewActionProperties = class(TdhClassMenuActionProperties)
  protected
    function GetActionKind: TdhActionKind; override;
  public
    function ScanFields: Boolean; override;
    function InternalVisible: Boolean; override;
  published
    property Visible default True;
  end;

  TdhEditActionProperties = class(TdhClassMenuActionProperties)
  protected
    function GetActionKind: TdhActionKind; override;
  public
    function ScanFields: Boolean; override;
    function InternalVisible: Boolean; override;
  published
    property Visible default True;
  end;

  TdhCopyActionProperties = class(TdhClassMenuActionProperties)
  protected
    function GetActionKind: TdhActionKind; override;
  public
    constructor Create(ASubDataHandler: TSubDataHandler); override;
  public
    function ScanFields: Boolean; override;
    function InternalVisible: Boolean; override;
  published
    property Visible default False;
  end;

  TdhOptionsMenu = class(TdhCustomActionProperties)
  private
    FClassName      : String;
    FConstants      : TdhConstantParamEdit;
    FParams         : TdhDynamicParamEdit;

    FOptionsNew     : TdhNewActionProperties;
    FOptionsView    : TdhViewActionProperties;
    FOptionsEdit    : TdhEditActionProperties;
    FOptionsCopy    : TdhCopyActionProperties;
    FOptionsDelete  : TdhDeleteActionProperties;

    FKeyFieldsMode  : Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(ADataHandler: TSubDataHandler); reintroduce;
    destructor Destroy; override;

    function ScanFields: Boolean; override;
  published
    property KeyFieldsMode: Boolean read FKeyFieldsMode write FKeyFieldsMode Default False;

{$WARN HIDING_MEMBER OFF}
    property ClassName: String read FClassName write FClassName;
{$WARN HIDING_MEMBER ON}

    property Constants: TdhConstantParamEdit  read FConstants write FConstants;
    property Params   : TdhDynamicParamEdit   read FParams    write FParams;

    property OptionsNew   : TdhNewActionProperties    read FOptionsNew    write FOptionsNew;
    property OptionsView  : TdhViewActionProperties   read FOptionsView   write FOptionsView;
    property OptionsEdit  : TdhEditActionProperties   read FOptionsEdit   write FOptionsEdit;
    property OptionsCopy  : TdhCopyActionProperties   read FOptionsCopy   write FOptionsCopy;
    property OptionsDelete: TdhDeleteActionProperties read FOptionsDelete write FOptionsDelete;
  end;

  TdhRequiredFields = class;
  TdhGenerateKeyFieldMethod = (dhGkfNever, dhGkfInc, dhGkfDec, dhGkfIncFromMin, dhGkfDecFromMax, dhGkfAutoInc, dhGkfNewGUId);

  TdhSettings = class(TdhPersistent)
  private
//    FAcceptNotification: Boolean;
//    FNotifyChanges: Boolean;

    FGenerateKeyField: TdhGenerateKeyFieldMethod;
    FCascadeDelete: Boolean;
    FConfirmApplyRequiredText: String;
    function IsModifiedApplyReqdText: Boolean;

    procedure SetGenerateKeyField(const AValue: TdhGenerateKeyFieldMethod);
  public
    constructor Create(ADataHandler: TCustomDataHandler); override;

//    property AcceptNotification: Boolean read FAcceptNotification write FAcceptNotification default False; // reserved
//    property NotifyChanges: Boolean read FNotifyChanges write FNotifyChanges default True; // reserved
  published
    property GenerateKeyField: TdhGenerateKeyFieldMethod read FGenerateKeyField write SetGenerateKeyField default dhGkfNever;
    property CascadeDelete: Boolean read FCascadeDelete write FCascadeDelete default True;
    property ConfirmApplyRequiredText: string read FConfirmApplyRequiredText write FConfirmApplyRequiredText stored IsModifiedApplyReqdText;
  end;

  TdhDefaultImages = class(TPersistent)
  private
    FApply    : Integer;
    FCollapse : Integer;
    FDelete   : Integer;
    FRestore  : Integer;
    FEdit     : Integer;
    FCopy     : Integer;
    FExpand   : Integer;
    FNew      : Integer;
    FRefresh  : Integer;
    FView     : Integer;
  public
    constructor Create;
  published
    property Apply    : Integer read FApply     write FApply    default -1;
    property Collapse : Integer read FCollapse  write FCollapse default -1;
    property Delete   : Integer read FDelete    write FDelete   default -1;
    property Restore  : Integer read FRestore   write FRestore  default -1;
    property Edit     : Integer read FEdit      write FEdit     default -1;
    property Copy     : Integer read FCopy      write FCopy     default -1;
    property Expand   : Integer read FExpand    write FExpand   default -1;
    property New      : Integer read FNew       write FNew      default -1;
    property Refresh  : Integer read FRefresh   write FRefresh  default -1;
    property View     : Integer read FView      write FView     default -1;
  end;

  TdhPropItem = class(TdxBarPropItem)
  private
    FBeginMenuGroup: Boolean;
    FPosition: TdhMenuItemPos;
    FDataHandler: TCustomDataHandler;
    procedure SetDataHandler(const Value: TCustomDataHandler);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property DataHandler: TCustomDataHandler read FDataHandler write SetDataHandler;
    property BeginMenuGroup: Boolean read FBeginMenuGroup write FBeginMenuGroup default False;
    property Position: TdhMenuItemPos read FPosition write FPosition default ipMiddle;
  end;

  TdhItemPropStorage = class(TdxItemPropStorage)
  private
    FDefaultImages: TdhDefaultImages;
  protected
    function BarPropsClass: TdxBarPropsClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function FindByBarItem(AdxBarItem: TdxBarItem): TdhPropItem;
  published
    property DefaultImages: TdhDefaultImages read FDefaultImages write FDefaultImages;
  end;

  TdhProps = class(TdxBarProps)
  protected
    function BarPropItemClass: TdxBarPropItemClass; override;
  end;

  TCustomDataHandlerNotifyEvent           = procedure(ADataHandler: TCustomDataHandler) of object;
  TCustomDataHandlerNotifyVariabledEvent  = procedure(ADataHandler: TCustomDataHandler; Variables: TNamedVariants) of object;
  TCustomDataHandlerNotifyDataSetEvent    = procedure(ADataHandler: TCustomDataHandler; ADataSet: TDataSet) of object;

  TCustomDataHandlerMemoryPrepareEvent  = procedure(ADataHandler: TCustomDataHandler; ADataSet: TSQLDataSetProvider) of object;
  TCustomDataHandlerMemoryApplyEvent    = procedure(ADataHandler: TCustomDataHandler; ADataSet: TDataSet; var AApplyAction: TdhApplyAction) of object;

  TCustomDataHandlerRowChangedEvent     = procedure(ADataHandler: TCustomDataHandler; IsAfterScroll: Boolean) of object;
  TCustomDataHandlerAllowEvent          = procedure(ADataHandler: TCustomDataHandler; var ADone: Boolean) of object;
  TCustomDataHandlerAllowVariabledEvent = procedure(ADataHandler: TCustomDataHandler; var AVariables: TNamedVariants; var ADone: Boolean) of object;
  TCustomDataHandlerAllowProc           = procedure(ADataHandler: TCustomDataHandler; var ADone: Boolean; const ARezerved: Array of Pointer);

  TCustomDataHandlerBeforeNewEvent      = procedure(ADataHandler: TCustomDataHandler; AItem: TdhMultiNewItem; var AVariables: TNamedVariants; var ADone: Boolean) of object;
  TCustomDataHandlerBeforeActionEvent   = procedure(AAction: TdhAction; AProps: TdhCustomMenuActionProperties; var ADone: Boolean) of object;
  TCustomDataHandlerAfterActionEvent    = procedure(AAction: TdhAction; AProps: TdhCustomMenuActionProperties) of object;

  TDataSetChangeEvent = procedure(ADataHandler: TCustomDataHandler; ANewDS: TDataSet) of object;

  TCustomDataHandlerLockErrorEvent = procedure(ADataHandler: TCustomDataHandler; const ALockErrorMessage: String) of object;

  TdhLockOptions = class(TPersistent)
  private
    FMessageFieldName: String;
    FKeyFieldName: String;
  published
    property KeyFieldName: String read FKeyFieldName write FKeyFieldName;
    property MessageFieldName: String read FMessageFieldName write FMessageFieldName;
  end;

  TdhRequiredField = class(TCollectionItem)
  private
    FFieldName: String;
    FControl: TComponent;
    FSubControlIndex: SmallInt;
  public
    constructor Create(Collection: TCollection); override;
  published
    property FieldName: String read FFieldName write FFieldName;
    property Control: TComponent read FControl write FControl;
    property SubControlIndex: SmallInt read FSubControlIndex write FSubControlIndex default -1;
  end;

  TdhRequiredFields = class(TCollection)
  private
    function GetItem(AIndex: Integer): TdhRequiredField;
  public
    constructor Create; reintroduce;
    property Items[Index: Integer]: TdhRequiredField read GetItem; default;
  end;

  TDataHandlerList = TObjectList<TCustomDataHandler>;
//  TDataHandlerList = class(TComponentList)
//  protected
//    function GetItems(AIndex: Integer): TCustomDataHandler;
//    procedure SetItems(AIndex: Integer; AComponent: TCustomDataHandler);
//  public
//    property Items[Index: Integer]: TCustomDataHandler read GetItems write SetItems; default;
//  end;

  TDataHandlerEditMode = (dhemReadOnly, dhemPostOnly, dhemCommitable);

  TCustomDataHandler = class(TDataSource)
  private
    FActive   : Boolean;
    FModified : Boolean;

    FIntIdentity: Largeint;
  protected
    function GetModified: Boolean;

    procedure InternalApply;
  private
    procedure SetModified(const AValue: Boolean);

    property Modified: Boolean read GetModified write SetModified;
  private
    FDataLink       : TDataSourceDataLink;

    FCaption        : String;
    FEnabled        : Boolean;
    FReadOnly       : Boolean;
    FIsBusy         : Boolean;
    FLockOptions    : TdhLockOptions;
    FKeyFieldNames  : TdhFieldNames;
    FSettings       : TdhSettings;
    FRequiredFields : TdhRequiredFields;
    FBackup         : Array of Variant;
    FDataSetStates  : TStack<TDataSourceStateBookmark>;

    // -- События

    FOnBeforeApply  : TCustomDataHandlerAllowEvent;
    //FOnBeforeCancel : TCustomDataHandlerAllowEvent;
    FOnBeforeCommit : TCustomDataHandlerAllowEvent;
    FOnBeforeRestore: TCustomDataHandlerAllowEvent;

    FOnBeforeNew    : TCustomDataHandlerBeforeNewEvent;
    FOnBeforeView   : TCustomDataHandlerAllowVariabledEvent;
    FOnBeforeEdit   : TCustomDataHandlerAllowVariabledEvent;
    FOnBeforeCopy   : TCustomDataHandlerAllowVariabledEvent;
    FOnBeforeDelete : TCustomDataHandlerAllowEvent;
//    FOnGetClassName : TCustomDataHandlerGetClassNameEvent;

    FOnBeforeRollBack : TCustomDataHandlerAllowEvent;
    FOnBeforeValidate : TCustomDataHandlerAllowEvent;

    FOnAfterNew     : TCustomDataHandlerNotifyVariabledEvent;
    FOnAfterEdit    : TCustomDataHandlerNotifyVariabledEvent;
//    FOnAfterClose   : TCustomDataHandlerNotifyEvent;
    FOnAfterCommit  : TCustomDataHandlerNotifyEvent;
    FOnAfterDelete  : TCustomDataHandlerNotifyEvent;
    FOnAfterRestore : TCustomDataHandlerNotifyEvent;
//    FOnAfterOpen    : TCustomDataHandlerNotifyEvent;

    FOnLockError      : TCustomDataHandlerLockErrorEvent;

    procedure DataSetActiveChanged(Sender: TObject);
    procedure DataSetReOpened(Sender: TObject);

    procedure DataSetDataChanged(Sender: TObject);

    procedure SetInternalReadOnly(const Value: Boolean);
    procedure SetReadOnly(AReadOnly: Boolean);

    procedure CheckPrivileges(APriv: TdhCustomMenuActionProperties);
    procedure CheckActive;
//    function GetIndex: Integer;
    function GetKeyFieldValues: Variant;
{$IFNDEF PACKAGE}
    function GetAutoEdit: Boolean;
    procedure SetAutoEdit(const Value: Boolean);
{$ENDIF}
    function IsRequiredFieldsStored: Boolean;
    function GetDataSetProvider: TSQLDataSetProvider;
    function GetCanModify: Boolean; virtual;
    function GetInternalCanModify: Boolean;
  protected
    FItems: TList<TSubDataHandler>;
    procedure SetDataSetProvider(ADataSet: TSQLDataSetProvider); overload;
{$IFNDEF PACKAGE}
{$WARNINGS OFF}
    procedure SetDataSet(ADataSet: TDataSet); override;
{$WARNINGS ON}
{$ENDIF}
    procedure InternalEnableControls;
  protected
    FInternalReadOnly: Boolean;
//    EventsFreezed: Boolean;

    procedure InternalClearBackup;
    procedure InternalDetachDataSet; virtual;
    procedure InternalAttachDataSet; virtual;
    procedure InternalOpen;
    procedure InternalAfterOpen;
    procedure InternalPrepareIdentity;
    procedure DataSetOnNewRecord(Sender: TDataSet); dynamic;
    procedure InternalAfterClose;

    function ChildByItemIndex(i: Integer): TSubDataHandler;

//    procedure DoResync;
    function ExecCustomAllowEvent(AEvent: TCustomDataHandlerAllowEvent): Boolean;
    function ExecCustomAllowVariabledEvent(AEvent: TCustomDataHandlerAllowVariabledEvent; var AVariables : TNamedVariants): Boolean;
    procedure ExecCustomEvent(AEvent: TCustomDataHandlerNotifyEvent); overload;
    procedure ExecCustomEvent(AEvent: TCustomDataHandlerNotifyVariabledEvent; AVariables: TNamedVariants); overload;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function GetItemCount: Integer;

    function CheckDataHandlerSupport: TCustomDataHandlerSupport;
//    procedure Resync;

//    procedure RowChanged(IsAfterScroll: Boolean = False);
    procedure SyncActive; virtual;
  public
    class function IsRoot: Boolean; dynamic; abstract;

{$IFNDEF PACKAGE}
    function SBaseFrm: TSBaseFrm; inline;
{$ENDIF}

    function GetIsBusy: Boolean; dynamic; abstract;
    function GetRoot: TDataHandler; dynamic; abstract;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetDataHandlerSupport: TCustomDataHandlerSupport; dynamic; abstract;

    function ParentByDataName(const AName: String): TCustomDataHandler;

    // -- Для работы с Backup-ом в памяти
    function  DataSetBackupedRowsCount: Integer;         // -- Кол-во строк в Backup-е
    procedure DataSetBackup(ARecursive: Boolean);       // -- Формируем Backup в памяти (Array of Variant). Работаем с наложенным фильтром
    procedure DataSetRestore(ARecursive: Boolean);      // -- Добавляем записи к текущему DataSet из Backup-а
    procedure DataSetClear(ARecursive: Boolean);        // -- Удаляем все строки из DataSet
    procedure DataSetBackupClear(ARecursive: Boolean);  // -- Освобождаем память от Backup-а

    procedure Cancel;
    procedure Close;
    procedure Commit;

    function  NewObject: Boolean;
    procedure Apply; dynamic; abstract;
    procedure ApplyReadOnly;

    procedure Open;
    procedure Post;
//    procedure Refresh1(ARecordOnly: Boolean = False);
    procedure RollBack;
    procedure Validate;
//    procedure Update;
    procedure Restore;

    // -- NormalOrder = True: Сначала Children, затем Self
    procedure DisableControls(ARecursive: Boolean); virtual;
    // -- NormalOrder = True: Сначала Self, затем Children
    procedure EnableControls(ARecursive: Boolean); virtual;
    procedure FullEnableControls;

    function HasChildren: Boolean;
    function IsDataChanged: Boolean;
//    function Editing: Boolean;
    function Inserting: Boolean;

    procedure EnumChildren(AProc: TCustomDataHandlerAllowProc; AIncludeSelf: Boolean; const ARezerved: Array of Pointer);
    procedure FillChildrenList(AList: TDataHandlerList; AIncludeOwner: Boolean = True; AOwnerFirst: Boolean = True);
    function Childrens(AIncludeOwner: Boolean = True; AOwnerFirst: Boolean = True): TDataHandlerList;

//    function GetDataSetObject(const AFullFieldName: string; out AField: TField; out AParam: TParam): Boolean;
    function GetDataSetObjectValue(const AFullFieldName: String; AFieldsScan: Boolean = True): Variant;
    function DataSetActive: Boolean; inline;

    procedure Insert(ADataHandler: TSubDataHandler);
    procedure Remove(ADataHandler: TSubDataHandler);

//    procedure FreezeEvents;
//    procedure UnFreezeEvents;
    function  CachedUpdates: Boolean;

    property Count: Integer read GetItemCount;
    property Children[I: Integer]: TSubDataHandler read ChildByItemIndex;

    property Active: Boolean read FActive;

    property KeyFieldValues: Variant read GetKeyFieldValues;
    property InternalReadOnly: Boolean read FInternalReadOnly write SetInternalReadOnly default False;

    property InternalCanModify: Boolean read GetInternalCanModify;
    property CanModify: Boolean read GetCanModify;
  published
{$IFNDEF PACKAGE}
    property AutoEdit: Boolean read GetAutoEdit write SetAutoEdit default True;
{$ENDIF}
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Caption: string read FCaption write FCaption;
    property DataSet: TSQLDataSetProvider read GetDataSetProvider write SetDataSetProvider;
    property KeyFieldNames: TdhFieldNames read FKeyFieldNames write FKeyFieldNames;
    property Settings: TdhSettings read FSettings write FSettings;
    property RequiredFields: TdhRequiredFields read FRequiredFields write FRequiredFields stored IsRequiredFieldsStored; // reserved;
    // -- Параметры управления блокировками
    property LockOptions: TdhLockOptions read FLockOptions write FLockOptions;

    // -- События

    property OnBeforeNew      : TCustomDataHandlerBeforeNewEvent        read FOnBeforeNew       write FOnBeforeNew;
    property OnBeforeView     : TCustomDataHandlerAllowVariabledEvent   read FOnBeforeView      write FOnBeforeView;
    property OnBeforeEdit     : TCustomDataHandlerAllowVariabledEvent   read FOnBeforeEdit      write FOnBeforeEdit;
    property OnBeforeCopy     : TCustomDataHandlerAllowVariabledEvent   read FOnBeforeCopy      write FOnBeforeCopy;
    property OnBeforeDelete   : TCustomDataHandlerAllowEvent            read FOnBeforeDelete    write FOnBeforeDelete;

    property OnBeforeValidate : TCustomDataHandlerAllowEvent            read FOnBeforeValidate  write FOnBeforeValidate;
    property OnBeforeApply    : TCustomDataHandlerAllowEvent            read FOnBeforeApply     write FOnBeforeApply;
    property OnBeforeCommit   : TCustomDataHandlerAllowEvent            read FOnBeforeCommit    write FOnBeforeCommit;
    property OnBeforeRollBack : TCustomDataHandlerAllowEvent            read FOnBeforeRollBack  write FOnBeforeRollBack;

    property OnAfterNew       : TCustomDataHandlerNotifyVariabledEvent  read FOnAfterNew        write FOnAfterNew;
    property OnAfterEdit      : TCustomDataHandlerNotifyVariabledEvent  read FOnAfterEdit       write FOnAfterEdit;
    property OnAfterDelete    : TCustomDataHandlerNotifyEvent           read FOnAfterDelete     write FOnAfterDelete;

    property OnAfterCommit    : TCustomDataHandlerNotifyEvent           read FOnAfterCommit     write FOnAfterCommit;
//    property OnBeforeCancel   : TCustomDataHandlerAllowEvent            read FOnBeforeCancel    write FOnBeforeCancel;

//    property OnGetClassName   : TCustomDataHandlerGetClassNameEvent   read FOnGetClassName    write FOnGetClassName;
    property OnLockError: TCustomDataHandlerLockErrorEvent read FOnLockError write FOnLockError;
  end;
  PCustomDataHandler = ^TCustomDataHandler;

  TInterfacedObjectClass = class of TInterfacedObject;

  TDataHandler = class(TCustomDataHandler)
  private
    FOnModifiedChanged: TNotifyEvent;
    FOnApplyUpdates   : TCustomDataHandlerAllowEvent;
    FTotalModified    : Boolean;

    procedure ModifiedChanged;
  private
    FDataHandlerSupport: TCustomDataHandlerSupport;
    FEditMode: TDataHandlerEditMode;

    FIsOpening: Boolean;
    FPropStorage: TdhItemPropStorage;

    procedure SetPropStorage(const APropStorage: TdhItemPropStorage);

    function GetSupportClassName: String;
    procedure SetSupportClassName(const AValue: String);

    function GetSupportClass: TCustomDataHandlerSupportClass;
    procedure SetSupportClass(const AValue: TCustomDataHandlerSupportClass);
    procedure SetSupport(const Value: TCustomDataHandlerSupport);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function HasParent: Boolean; override;
    function  GetParentComponent: TComponent; override;
    procedure SetParentComponent(AParent: TComponent); override;
    function GetDataHandlerSupport: TCustomDataHandlerSupport; override;
  public
    class function IsRoot: Boolean; override;

    function GetRoot: TDataHandler; override;
    function GetIsBusy: Boolean; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  CheckApplied: Boolean;
    procedure ApplyUpdates;
    procedure Apply; override;

    property SupportClass: TCustomDataHandlerSupportClass read GetSupportClass write SetSupportClass;
    property EditMode: TDataHandlerEditMode read FEditMode write FEditMode default dhemCommitable;
    property Modified: Boolean read GetModified;
  published
    property PropStorage: TdhItemPropStorage read FPropStorage write SetPropStorage;

    property SupportClassName: String read GetSupportClassName write SetSupportClassName;
    property Support: TCustomDataHandlerSupport read FDataHandlerSupport write SetSupport;

    property OnModifiedChanged: TNotifyEvent                  read FOnModifiedChanged write FOnModifiedChanged;
    property OnApplyUpdates   : TCustomDataHandlerAllowEvent  read FOnApplyUpdates    write FOnApplyUpdates;
  end;

  TSubDataHandlerXMLPostOnBeforeApply = procedure(ASubDataHandler: TSubDataHandler; var AXML: String; var ADone: Boolean) of object;

  TSubDataHandlerXMLPostProperties = class(TPersistent)
  private
    [Weak] FDataHandler : TCustomDataHandler;
    FOnlyIfModified     : Boolean;
    FUpdateStatusFilter : TUpdateStatusSet;

    FFieldName          : String;
    FDataTag            : String;
    FFilter             : String;
    FExcludedFields     : String;
    FStatusAttributeName: String;

    FOnBeforeApply: TSubDataHandlerXMLPostOnBeforeApply;

    function IsDataTagStored: Boolean;
  protected
    ExcludedInProcessFields: String;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadAddExcludedFields(Reader: TReader);
  public
    constructor Create(AOwner: TCustomDataHandler);
    procedure AddExcludedFields(const AFields: String);
    function AllExcludedFields: String;
  published
    property FieldName            : String            read FFieldName           write FFieldName;
    property OnlyIfModified       : Boolean           read FOnlyIfModified      write FOnlyIfModified       default True;

    property DataTag              : String            read FDataTag             write FDataTag              stored IsDataTagStored;

    property UpdateStatusFilter   : TUpdateStatusSet  read FUpdateStatusFilter  write FUpdateStatusFilter   default [];
    property StatusAttributeName  : String            read FStatusAttributeName write FStatusAttributeName;
    property Filter               : String            read FFilter              write FFilter;

    property ExcludedFields       : String            read FExcludedFields      write FExcludedFields;
  end;

  TSubDataHandler = class(TCustomDataHandler)
  private
    FMasterKeyFieldNames: TdhFieldNames;
    FDetailKeyFieldNames: TdhFieldNames;

    FOptionsMenu  : TdhOptionsMenu;
    FXMLPost      : TSubDataHandlerXMLPostProperties;

    FParent : TCustomDataHandler;
    FGrid   : TComponent;

    FMemoryDataSet  : TDataSet;
    FOnMemoryPrepare: TCustomDataHandlerMemoryPrepareEvent;
    FOnMemoryApply  : TCustomDataHandlerMemoryApplyEvent;
{$IFNDEF PACKAGE}
//    FGridControl: TDataSourceControlProperties;
{$ENDIF}

    function  GetMasterKeyFieldNames: TdhFieldNames;
    procedure SetMasterKeyFieldNames(const Value: TdhFieldNames);

    function  GetDetailKeyFieldNames: TdhFieldNames;
    procedure SetDetailKeyFieldNames(const AValue: TdhFieldNames);
    procedure DetailKeyFieldNamesUpdated;

    function  GetXMLPostOnBeforeApply: TSubDataHandlerXMLPostOnBeforeApply;
    procedure SetXMLPostOnBeforeApply(const Value: TSubDataHandlerXMLPostOnBeforeApply);

    procedure SetParent(ADataHandler: TCustomDataHandler); inline;
  protected
{$IFNDEF PACKAGE}
    procedure SetDataSet(ADataSet: TDataSet); override;
{$ENDIF}
    procedure InternalDetachDataSet; override;
    procedure InternalAttachDataSet; override;
    procedure InitializeMasterDetail;
    procedure DataSetOnNewRecord(Sender: TDataSet); override;

    procedure Loaded; override;

    procedure MemoryDataSetProviderBeforePost(ADataSet: TDataSet);
    property  MemoryDataSet: TDataSet read FMemoryDataSet write FMemoryDataSet;
  public
    function  HasParent: Boolean; override;
    function  GetParentComponent: TComponent; override;
    procedure SetParentComponent(AParent: TComponent); override;

    property Parent: TCustomDataHandler read FParent;
  public
    class function IsRoot: Boolean; override;

    function GetRoot: TDataHandler; override;
    function GetIsBusy: Boolean; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetDataHandlerSupport: TCustomDataHandlerSupport; override;

    procedure AssignParams;
    procedure View;
    procedure UpdateActive;

    function GetCanModify: Boolean; override;

    function New(AItem: TdhMultiNewItem = nil): Boolean;
    function Edit(ARaiseIfFalse: Boolean): Boolean;
    function Copy: Boolean;
    function Delete: Boolean;
    function DoDeleteDataSet(ACascade: Boolean = False): Boolean;

    procedure Apply; override;
  published
    property Grid: TComponent read FGrid write FGrid;

    property MasterKeyFieldNames  : TdhFieldNames                     read GetMasterKeyFieldNames write SetMasterKeyFieldNames;
    property DetailKeyFieldNames  : TdhFieldNames                     read GetDetailKeyFieldNames write SetDetailKeyFieldNames;

    property OptionsMenu: TdhOptionsMenu read FOptionsMenu write FOptionsMenu;

    property XMLPost              : TSubDataHandlerXMLPostProperties  read FXMLPost               write FXMLPost;

    property OnMemoryPrepare      : TCustomDataHandlerMemoryPrepareEvent  read FOnMemoryPrepare         write FOnMemoryPrepare;
    property OnMemoryApply        : TCustomDataHandlerMemoryApplyEvent    read FOnMemoryApply           write FOnMemoryApply;
    property XMLPostOnBeforeApply : TSubDataHandlerXMLPostOnBeforeApply   read GetXMLPostOnBeforeApply  write SetXMLPostOnBeforeApply;
  end;

implementation

uses
  StrUtils, Variants, TypInfo, DBConsts,
  SBaseStringFunctions, SBaseConstants, SBaseVariantFunctions, SBaseUtils,
  SBaseDataHelperInternal;

resourcestring
//  SConfirmApplyReqd = 'Для выполнения этой операции данные должны быть сохранены. Сохранить?';
  SConfirmDel = 'Удалить текущую запись?';
  SErrDataSetExists = 'Данный DataSet уже привязан к %s';
  SErrNoDataSet = 'У %s не задан DataSet';
  SErrNotActive = 'Cannot perform this operation on closed DataHandler';
  SErrIvalidParamList = 'Параметры должны быть заданы в виде ''Param=Value''';
//  SErrFormIsNotAppliable = 'Продолжение операции невозможно, т.к. форма в текущий момент не может быть сохранена';

type
  TFieldListAccess = class(TFieldList);

function CheckDependencies(const AValue: TdhActionDependencies): TdhActionDependencies;
begin
  Assert(AValue <> []);

  Result := AValue;

  if [dhdActive, dhdData] * Result = [dhdActive, dhdData] then
    Result := Result - [dhdActive];

  if [dhdData, dhdFocus] * Result = [dhdData, dhdFocus] then
    Result := Result - [dhdData];
end;

{ ***************************** TCustomDataHandler ***************************** }

constructor TCustomDataHandler.Create(AOwner: TComponent);
begin
{$IFNDEF PACKAGE}
  Assert(Assigned(AOwner) and (AOwner is TSBaseFrm));
{$ENDIF}

  inherited;

  FDataLink := TDataSourceDataLink.Create;
  FDataLink.DataSource := Self;
  FDataLink.OnActiveChanged   := DataSetActiveChanged;
  FDataLink.OnDataSetReOpened := DataSetReOpened;

  FDataLink.OnRecordChanged := DataSetDataChanged;

  FItems := TList<TSubDataHandler>.Create;
  FSettings := TdhSettings.Create(Self);
  FRequiredFields := TdhRequiredFields.Create;
  FLockOptions := TdhLockOptions.Create;

  FEnabled := True;
  FDataSetStates := TStack<TDataSourceStateBookmark>.Create;
//  FDataSetStates := TStack<TStack<TDataSourceStateBookmark>>.Create;
end;

destructor TCustomDataHandler.Destroy;
var
  I: Integer;
  LChild: TSubDataHandler;
begin
  InternalClearBackup;
  FreeAndNil(FDataLink);

  for I := Pred(Count) downto 0 do
  begin
    LChild := Children[I];
    FItems[I] := nil;
    FreeAndNil(LChild);
  end;

  FreeAndNil(FDataSetStates);
  FreeAndNil(FLockOptions);
  FreeAndNil(FRequiredFields);
  FreeAndNil(FSettings);
  FreeAndNil(FItems);

  inherited;
end;

function TCustomDataHandler.GetDataSetProvider: TSQLDataSetProvider;
begin
  Result := TSQLDataSetProvider((inherited DataSet).Source);
end;

procedure TCustomDataHandler.Cancel;
var
  I: Integer;
begin
  CheckDataHandlerSupport.DoCancel(Self);

  for I := 0 to Pred(Count) do
    with Children[I] do
      if Enabled and FActive then Cancel;
end;

procedure TCustomDataHandler.ApplyReadOnly;
var
  LDataHandlerSupport: TCustomDataHandlerSupport;
begin
  LDataHandlerSupport := GetDataHandlerSupport;
  if Assigned(LDataHandlerSupport) then
    LDataHandlerSupport.UpdateReadOnly(Self);
end;

function TCustomDataHandler.CachedUpdates: Boolean;
begin
  Result := (GetRoot.EditMode = dhemCommitable) and DataSet.CachedUpdates;
end;

function TCustomDataHandler.ParentByDataName(const AName: String): TCustomDataHandler;
var
  LDataSet: TDataSet;
begin
  LDataSet := DataSet;
  if SameText(Name, AName) or (Assigned(LDataSet) and SameText(LDataSet.Name, AName)) then
    Result := Self
  else begin
    if IsRoot then
      Result := nil
    else begin
      Result := TCustomDataHandler(GetParentComponent);
      if Assigned(Result) then
        Result := Result.ParentByDataName(AName)
    end;
  end;
end;

function TCustomDataHandler.ChildByItemIndex(I: Integer): TSubDataHandler;
begin
  if FItems.Count > I then
    Result := FItems[I]
  else
    Result := nil;
end;

function TCustomDataHandler.Childrens(AIncludeOwner, AOwnerFirst: Boolean): TDataHandlerList;

  procedure Fill(ADataHandler: TCustomDataHandler; AIncludeOwner: Boolean);
  var
    i: Integer;
  begin
    if (AIncludeOwner or (Self <> ADataHandler)) and (AOwnerFirst) then
      Result.Add(ADataHandler);

    for i := 0 to Pred(ADataHandler.Count) do
      Fill(ADataHandler.Children[i], True);

    if (AIncludeOwner or (Self <> ADataHandler)) and (not AOwnerFirst) then
      Result.Add(ADataHandler);
  end;

begin
  Result := TDataHandlerList.Create(False);
  Fill(Self, AIncludeOwner);
end;

procedure TCustomDataHandler.Close;
var
  I: Integer;
begin
  if FInternalReadOnly then begin
    ReadOnly := False;
    FInternalReadOnly := False;
  end;

  if (not Enabled) or (not FActive) then Exit;

  CheckDataHandlerSupport.DoOnBeforeClose(Self);

//  if not ExecCustomAllowEvent(FOnBeforeClose) then
//  begin
    // -- Закрываем сначала всех детей

    for I := 0 to Pred(Count) do
      Children[I].Close;

    DataSet.Close;
    SyncActive;
//  end;
end;

procedure TCustomDataHandler.Commit;
var
  I: Integer;
begin
  if not Enabled then Exit;
  CheckActive;

  FIsBusy := True;
  try
    if not ExecCustomAllowEvent(FOnBeforeCommit) then begin
      if CachedUpdates then DataSet.CommitUpdates;
      CheckDataHandlerSupport.DoOnAfterCommit(Self);

      ExecCustomEvent(FOnAfterCommit);
    end;

    for I := 0 to Pred(Count) do
      with Children[I] do
        if FActive then Commit;
  finally
    FIsBusy := False;
  end;
  DataSetDataChanged(nil);
end;

procedure TCustomDataHandler.Restore;
begin
  if not Enabled then
    exit;
  CheckActive;
  FIsBusy := True;
  try
    if not ExecCustomAllowEvent(FOnBeforeRestore) then begin
      DataSet.RevertRecord;
      //CheckDataHandlerSupport.DoRestore(Self);
      ExecCustomEvent(FOnAfterRestore);
    end;
  finally
    FIsBusy := False;
  end;
end;

procedure TCustomDataHandler.DataSetBackup(ARecursive: Boolean);
var
  i: Integer;
  j: Integer;
  n: Integer;
  LFieldsCount: Integer;
  LVar: Variant;
  LRecNo: Integer;
begin
  InternalClearBackup;

  if ARecursive then begin
    n := Pred(Count);
    for i := n downto 0 do
      Children[i].DataSetBackup(ARecursive);
  end;

  if (not Enabled) or (not DataSet.Active) then
    exit;

  LRecNo := -1;
  DataSet.DisableControls;
  try
    LRecNo := DataSet.RecNo;

    SetLength(FBackup, DataSet.RecordCount);
    i := 0;
    LFieldsCount := Pred(DataSet.FieldCount);
    DataSet.First;
    while not DataSet.Eof do begin
      LVar := VarArrayCreate([0, LFieldsCount], varVariant);
      for j := 0 to LFieldsCount do begin
        LVar[j] := DataSet.Fields[j].AsVariant;
      end;
      FBackup[i] := LVar;
      DataSet.Next;
      Inc(i);
    end;
  finally
    if LRecNo > 0 then begin
      DataSet.RecNo := LRecNo;
    end;
    DataSet.EnableControls;
  end;
end;

procedure TCustomDataHandler.DataSetBackupClear(ARecursive: Boolean);
var
  i: Integer;
  n: Integer;
begin
  if ARecursive then
  begin
    n := Pred(Count);
    for i := n downto 0 do
      Children[i].DataSetBackupClear(ARecursive);
  end;

  InternalClearBackup;
end;

function TCustomDataHandler.DataSetBackupedRowsCount: Integer;
begin
  Result := Length(FBackup);
end;

procedure TCustomDataHandler.DataSetClear(ARecursive: Boolean);
var
  I: Integer;
begin
  if ARecursive then
    for I := Pred(Count) downto 0 do
      Children[I].DataSetClear(ARecursive);

  if (not Enabled) or (not DataSet.Active) or DataSet.IsEmpty then
    Exit;

  DataSet.DisableControls;
  try
    DataSet.First;
    while not DataSet.Eof do
      DataSet.Delete;
  finally
    DataSet.EnableControls;
  end;
end;

procedure TCustomDataHandler.DataSetReOpened(Sender: TObject);
begin
  SyncActive;
end;

procedure TCustomDataHandler.DataSetRestore(ARecursive: Boolean);
var
  i             : Integer;
  n             : Integer;
  j             : Integer;
  LRowsCount    : Integer;
  LFieldsCount  : Integer;

  LVar          : Variant;

  LDataSet: TSQLDataSetProvider;
  LDetailFields : String;
  LModifiAbleFieldsFieldIndex: Integer;

  procedure DoCopyField(AIndex: Integer);
  var
    LField: TField;
    LValue: Variant;
  begin
    LField := LDataSet.Fields[AIndex];
    LValue := LVar[AIndex];

    if (not LField.ReadOnly) and (LField.IsNull) and VarIsPresent(LValue)
    then
      LField.AsVariant := LValue;
  end;
begin
  LDataSet := DataSet;
  if Enabled and LDataSet.Active then begin
    LRowsCount := Pred(Length(FBackup));
    LFieldsCount := Pred(LDataSet.FieldCount);
//    LKeyFields := KeyFieldNames;

    if Self is TSubDataHandler then
      LDetailFields := TSubDataHandler(Self).DetailKeyFieldNames;

    with LDataSet.FieldsAutomations do
      if ModifiAbleFieldsFieldName.IsEmpty then
        LModifiAbleFieldsFieldIndex := -1
      else
        LModifiAbleFieldsFieldIndex := LDataSet.Fields.FieldByName(ModifiAbleFieldsFieldName).Index;

    LDataSet.DisableControls;
    try
      for i := 0 to LRowsCount do begin
        LDataSet.Append;
        LVar := FBackup[i];

        if LModifiAbleFieldsFieldIndex <> -1 then begin
          DoCopyField(LModifiAbleFieldsFieldIndex);
          LDataSet.DoDataSetFieldsUpdateAble;
        end;

        for j := 0 to LFieldsCount do
          if j <> LModifiAbleFieldsFieldIndex then begin
            DoCopyField(j);
          end;

        if not IsRoot and LDataSet.Editing then
          LDataSet.Post;
      end;

    finally
      LDataSet.EnableControls;
    end;
  end;

  if ARecursive then begin
    n := Pred(Count);
    for i := 0 to n do
      Children[i].DataSetRestore(ARecursive);
  end;
end;

function TCustomDataHandler.DataSetActive: Boolean;
begin
  Result := (Assigned(DataSet) and DataSet.Active);
end;

procedure TCustomDataHandler.DataSetActiveChanged(Sender: TObject);
begin
  SyncActive;
end;

procedure TCustomDataHandler.SyncActive;
var
  LActive: Boolean;
begin
  LActive := DataSetActive;
  if FActive <> LActive then begin
    FActive := LActive;

    if FActive then
      InternalAfterOpen
    else
      CheckDataHandlerSupport.DoOnAfterClose(Self)
  end;
end;

procedure TCustomDataHandler.DataSetDataChanged(Sender: TObject);
var
  LDataSet: TSQLDataSetProvider;
begin
  if not GetIsBusy then begin
    LDataSet := DataSet;
    Modified := Assigned(LDataSet)
                and
                (
                  (LDataSet.State in [dsInsert, dsEdit])
                  or
                  ((GetRoot.EditMode = dhemCommitable) and LDataSet.UpdatesPending)
                );
  end;
end;

procedure TCustomDataHandler.DataSetOnNewRecord(Sender: TDataSet);
var
  LField: TField;
  LMethod: TdhGenerateKeyFieldMethod;
begin
  LMethod := Settings.GenerateKeyField;
  if CachedUpdates and (LMethod <> dhGkfNever) then begin
    LField := DataSet.FieldByName(KeyFieldNames);
    if LMethod = dhGkfNewGUId then
      LField.AsString := NewGUIdString
    else begin
      LField.AsInteger := FIntIdentity;
      if LMethod in [dhGkfInc, dhGkfIncFromMin, dhGkfAutoInc] then
        Inc(FIntIdentity)
      else
        Dec(FIntIdentity);
    end;
  end;
end;

//function TCustomDataHandler.Editing: Boolean;
//begin
//  Result := Assigned(DataSet) and (DataSet.State in dsEditModes)
//end;

function TCustomDataHandler.Inserting: Boolean;
begin
  Result := Assigned(DataSet) and DataSet.Inserted
end;

procedure TCustomDataHandler.InternalClearBackup;
var
  I: Integer;
begin
  for I := Pred(Length(FBackup)) downto 0 do
    FBackup[I] := Unassigned;
  SetLength(FBackup, 0);
end;

procedure TCustomDataHandler.InternalOpen;
begin
  with DataSet.FieldsAutomations do begin
    AddServiceFields(LockOptions.KeyFieldName);
    AddServiceFields(LockOptions.MessageFieldName);
    if Settings.GenerateKeyField <> dhGkfNever then
      AddServiceFields(KeyFieldNames);
  end;

//  if not ExecCustomAllowEvent(FOnBeforeOpen) then
  CheckDataHandlerSupport.DoOpen(Self);
  if FActive and CachedUpdates then
    InternalPrepareIdentity;
end;

procedure TCustomDataHandler.InternalAfterOpen;
begin
  DataSetDataChanged(nil);
  CheckDataHandlerSupport.DoOnAfterOpen(Self);
end;

procedure TCustomDataHandler.InternalApply;
var
  LDataSet: TSQLDataSetProvider;
  I: Integer;
begin
  LDataSet := DataSet;
  if LDataSet.Editing then Post;
  if CachedUpdates and LDataSet.UpdatesPending then LDataSet.ApplyUpdates;

  for I := 0 to Pred(Count) do
    with Children[I] do
      if Enabled and FActive then Apply;
end;

procedure TCustomDataHandler.InternalDetachDataSet;
begin
  with DataSet do
    OnNewRecord := nil;
end;

procedure TCustomDataHandler.InternalAttachDataSet;
begin
  with DataSet do
    OnNewRecord := DataSetOnNewRecord;
end;

procedure TCustomDataHandler.InternalAfterClose;
var
  LDataSet: TSQLDataSetProvider;
begin
  LDataSet := DataSet;
  if Assigned(LDataSet) and not (csDestroying in LDataSet.ComponentState) then begin
    LDataSet.OnNewRecord := nil; //FOnNewRecord;
  end;
end;

procedure TCustomDataHandler.InternalPrepareIdentity;
var
//  LKeyName: String;
  LKeyField: TField;
  LBookMark: TDataSourceStateBookmark;
  LDataSet, LMasterDataSet: TSQLDataSetProvider;
begin
  LDataSet := DataSet;
  if Settings.GenerateKeyField in [dhGkfNever, dhGkfNewGUId] then Exit;

  LKeyField := LDataSet.FindField(KeyFieldNames);
  Assert(Assigned(LKeyField) and LKeyField.IsOrdinal, 'Unsupported key field name/type for auto generate procedure.');

  case Settings.GenerateKeyField of
    dhGkfInc: FIntIdentity := 1;
    dhGkfDec: FIntIdentity := -1;
    dhGkfAutoInc:
      begin
        if Assigned(LDataSet.MasterSource) then begin
          LMasterDataSet := TSQLDataSetProvider(LDataSet.MasterSource.DataSet.Source);
          Assert(Assigned(LMasterDataSet));
          Assert(((LMasterDataSet.State = dsInsert) and (LMasterDataSet.RecordCount = 0)) or ((LMasterDataSet.State <> dsInsert) and (LMasterDataSet.RecordCount = 1)), 'Cannot prepare Auto Identity while MasterSource has more then one record');
        end;

        if LDataSet.IsEmpty then
          FIntIdentity := 1
        else if ((LDataSet.State = dsInsert) and (LDataSet.RecordCount = 0)) or ((LDataSet.State <> dsInsert) and (LDataSet.RecordCount = 1)) then
          FIntIdentity := LKeyField.AsInteger + 1
        else begin
          Assert(LDataSet.Editing, 'Cannot prepare Auto Identity while DataSet in Edit mode');
          FIntIdentity := 1;
          LDataSet.BeginUpdate(LBookMark);
          try
            LDataSet.Filtered := False;
            LDataSet.First;
            while not LDataSet.Eof do begin
              FIntIdentity := Max(FIntIdentity, LKeyField.AsInteger + 1);
              LDataSet.Next;
            end;
          finally
            LDataSet.EndUpdate(LBookMark, [sbrFiltered, sbrDisableCount], [sbrRecordIndex]);
          end;
        end;
      end;
    else begin
      FIntIdentity := Pred(Cardinal(1 shl (Pred(8 * LKeyField.DataSize)))); // MaxInt - 1
      if Settings.GenerateKeyField = dhGkfIncFromMin then
        FIntIdentity := -FIntIdentity;
    end;
  end;
end;

//procedure TCustomDataHandler.Update;
//var
//  I: Integer;
//begin
//  RowChanged(False);
//  for I := 0 to Pred(Count) do
//    with Children[I] do
//      if Active then Update;
//end;

procedure TCustomDataHandler.EnumChildren(AProc: TCustomDataHandlerAllowProc; AIncludeSelf: Boolean; const ARezerved: Array of Pointer);
var
  I: Integer;
  LDone: Boolean;
  LList: TDataHandlerList;
begin
  LList := TDataHandlerList.Create(False);
  try
    FillChildrenList(LList, AIncludeSelf);
    LDone := False;
    I := 0;
    while not LDone and (I < LList.Count) do begin
      AProc(LList[I], LDone, ARezerved);
      Inc(I);
    end;
  finally
    LList.Free
  end;
end;

procedure TCustomDataHandler.DisableControls(ARecursive: Boolean);

  procedure InternalDisableControls;
  var
    LDataSetState : TDataSourceStateBookmark;
    LDataSet      : TDataSet;
  begin
    LDataSet := DataSet;
    if LDataSet <> nil then begin
      LDataSet.BeginUpdate(LDataSetState);
      FDataSetStates.Push(LDataSetState);
    end;
  end;

var
  I, LCount : Integer;
begin
  LCount := Pred(Count);

  if ARecursive then
    for I := 0 to LCount do
      Children[I].DisableControls(ARecursive);

  InternalDisableControls;
end;

procedure TCustomDataHandler.InternalEnableControls;
var
  LDataSetState : TDataSourceStateBookmark;
  LDataSet      : TDataSet;
begin
  LDataSet := DataSet;
  if LDataSet <> nil then begin
    LDataSetState := FDataSetStates.Pop;
    LDataSet.EndUpdate(LDataSetState);
  end;
end;

procedure TCustomDataHandler.EnableControls(ARecursive: Boolean);
var
  I, LCount : Integer;
begin
  LCount := Pred(Count);

  InternalEnableControls;
  if ARecursive then
    for I := LCount downto 0 do
      Children[I].EnableControls(ARecursive);
end;

procedure TCustomDataHandler.FullEnableControls;
var
  I: Integer;
  LDataSet: TDataSet;
begin
  for I := 0 to Pred(Count) do
    Children[I].FullEnableControls;

  LDataSet := DataSet;
  if LDataSet <> nil then
    while LDataSet.ControlsDisabled do InternalEnableControls;
end;

function TCustomDataHandler.ExecCustomAllowEvent
  (AEvent: TCustomDataHandlerAllowEvent): Boolean;
begin
  Result := False;
  if {not EventsFreezed and} Assigned(AEvent) then
    AEvent(Self, Result);
end;

function TCustomDataHandler.ExecCustomAllowVariabledEvent(AEvent: TCustomDataHandlerAllowVariabledEvent; var AVariables: TNamedVariants): Boolean;
begin
  Result := False;
  if {not EventsFreezed and} Assigned(AEvent) then
    AEvent(Self, AVariables, Result);
end;

procedure TCustomDataHandler.ExecCustomEvent(AEvent: TCustomDataHandlerNotifyEvent);
begin
  if {(not EventsFreezed) and} Assigned(AEvent) then
    AEvent(Self);
end;

procedure TCustomDataHandler.ExecCustomEvent(AEvent: TCustomDataHandlerNotifyVariabledEvent; AVariables: TNamedVariants);
begin
  if {(not EventsFreezed) and} Assigned(AEvent) then
    AEvent(Self, AVariables);
end;

procedure TCustomDataHandler.FillChildrenList(AList: TDataHandlerList; AIncludeOwner: Boolean = True; AOwnerFirst: Boolean = True);

  procedure Fill(ADataHandler: TCustomDataHandler; AIncludeOwner: Boolean);
  var
    i: Integer;
  begin
    if (AIncludeOwner or (Self <> ADataHandler)) and (AOwnerFirst) then
      AList.Add(ADataHandler);

    for i := 0 to Pred(ADataHandler.Count) do
      Fill(ADataHandler.Children[i], True);

    if (AIncludeOwner or (Self <> ADataHandler)) and (not AOwnerFirst) then
      AList.Add(ADataHandler);
  end;

begin
  if Assigned(AList) then begin
    AList.OwnsObjects := False;
    Fill(Self, AIncludeOwner);
  end;
end;

procedure TdhParamEdit.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TdhParamEdit then
  begin
    TdhParamEdit(Dest).FVarList.AssignValues(FVarList);
  end;
end;

{$IFNDEF PACKAGE}
function TCustomDataHandler.GetAutoEdit: Boolean;
begin
  Result := FAutoEdit;
end;

function TCustomDataHandler.SBaseFrm: TSBaseFrm;
begin
  Result := TSBaseFrm(Owner);
end;

procedure TCustomDataHandler.SetAutoEdit(const Value: Boolean);
begin
  if FAutoEdit <> Value then begin
    Assert((not Value) or (not FReadOnly));
    FAutoEdit := Value;
  end;
end;
{$ENDIF}

procedure TCustomDataHandler.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Pred(FItems.Count) do
    Proc(FItems[i]);
end;

function TCustomDataHandler.GetDataSetObjectValue(const AFullFieldName: String; AFieldsScan: Boolean): Variant;
var
  LField: TField;
  LParam: TParam;

var
  LPos: Integer;
  LDsName: String;
  LFlName: String;

  LDH: TCustomDataHandler;
  LDS: TSQLDataSetProvider;
  LParams: TParams;
begin
  LDH := Self;

  if FindPos('.', AFullFieldName, LPos) then begin
    LFlName := RightStr(AFullFieldName, Length(AFullFieldName) - LPos);
    LDsName := LeftStr(AFullFieldName, Pred(LPos));

    LDH   := ParentByDataName(LDsName);
  end else
    LFlName := AFullFieldName;

  LDS := LDH.DataSet;
  LField := nil;
  LParam := nil;
  if Assigned(LDS) then begin
    if AFieldsScan or (LDH <> Self) then
      LField := LDS.FindField(LFlName);

    if not Assigned(LField) then begin
      LParams := LDS.Params;
      LParam := LParams.FindParam(LFlName);
      if not Assigned(LParam) then
        LParam := LParams.FindParam(SConst_PARAM_ + LFlName);
    end;
  end;

  if Assigned(LField) then
    Result := LField.AsVariant
  else if Assigned(LParam) then
    Result := LParam.Value
  else
    Result := CheckDataHandlerSupport.GetObjectValue(AFullFieldName);
end;

function TCustomDataHandler.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TCustomDataHandler.GetKeyFieldValues: Variant;
begin
  Result := DataSet.FieldValues[KeyFieldNames];
end;

function TCustomDataHandler.GetModified: Boolean;
var
  I: Integer;
begin
  Result := FModified;
  if not Result then
    for I := 0 to FItems.Count - 1 do
      if FItems[I].Modified then
        Exit(True);
end;

procedure TCustomDataHandler.SetInternalReadOnly(const Value: Boolean);
begin
  FInternalReadOnly := Value;
{$IFNDEF PACKAGE}
  FAutoEdit := not (FReadOnly or FInternalReadOnly);
{$ENDIF}
end;

procedure TCustomDataHandler.SetModified(const AValue: Boolean);
begin
  if FModified <> AValue then begin
    FModified := AValue;
    GetRoot.ModifiedChanged;
  end;
end;

function TCustomDataHandler.GetInternalCanModify: Boolean;
begin
  Result := (not FInternalReadOnly) and (not FReadOnly);
end;

function TCustomDataHandler.GetCanModify: Boolean;
var
  LDataSet: TSQLDataSetProvider;
begin
  LDataSet := DataSet;
  Result := (not FInternalReadOnly)
          and (not FReadOnly)
          and Assigned(LDataSet)
          and LDataSet.CanModify
end;

procedure TCustomDataHandler.SetReadOnly(AReadOnly: Boolean);
begin
  if FReadOnly <> AReadOnly then begin
    FReadOnly := AReadOnly;
    SetInternalReadOnly(FInternalReadOnly);
    ApplyReadOnly;
  end;
end;

function TCustomDataHandler.HasChildren: Boolean;
begin
  Result := FItems.Count > 0;
end;

procedure TCustomDataHandler.Insert(ADataHandler: TSubDataHandler);
begin
  FItems.Add(ADataHandler);
  ADataHandler.SetParent(Self);
end;

function TCustomDataHandler.IsDataChanged: Boolean;
var
  I: Integer;
  LDataSetProvider: TSQLDataSetProvider;
begin
  LDataSetProvider := DataSet;
  if (not Enabled) or (LDataSetProvider = nil) then Exit(False);

  Result := FActive and (LDataSetProvider.Editing or (CachedUpdates and LDataSetProvider.UpdatesPending));

  if not Result then
    for I := 0 to Count - 1 do
      if Children[I].IsDataChanged then
        Exit(True);
end;

function TCustomDataHandler.IsRequiredFieldsStored: Boolean;
begin
  Result := (FRequiredFields.Count > 0)
end;

function TCustomDataHandler.NewObject: Boolean;
var
  LDataSet: TDataSet;
  LDone   : Boolean;
  I       : Integer;
begin
  Result := False;
  if not Enabled then
    Exit;

  LDataSet := DataSet;
  if not Assigned(LDataSet) then
    raise EDataHandler.CreateFmt(SErrNoDataSet, [Name]);

  DisableControls(True);
  try
    FIsBusy := True;
    SyncActive; //TODO: Зачем это здесь???
    LDone := False;

    if IsRoot then
      TDataHandler(Self).FIsOpening := True;

    InternalOpen;

    for I := 0 to Pred(Count) do
      Children[I].NewObject;

    if (not LDone) and (IsRoot or (not TSubDataHandler(Self).OptionsMenu.KeyFieldsMode)) then
      CheckDataHandlerSupport.DoNewObject(Self);
    Result := True;
  finally
    EnableControls(True);
    if IsRoot then
      TDataHandler(Self).FIsOpening := False;
    FIsBusy := False;
  end;
  DataSetDataChanged(nil);
end;

procedure TCustomDataHandler.Open;
var
  LDataHandlerSupport: TCustomDataHandlerSupport;
  LDataSetProvider: TSQLDataSetProvider;
  i: Integer;
begin
  if FInternalReadOnly then begin
    ReadOnly := True;
  end;

  if (not Enabled) or (FActive) then
    Exit;

  LDataSetProvider := DataSet;
  if not Assigned(LDataSetProvider) then
    raise EDataHandler.CreateFmt(SErrNoDataSet, [Name]);

  LDataHandlerSupport := GetDataHandlerSupport;
{$IFDEF DEBUG}
  Assert(LDataHandlerSupport <> nil, 'Support interface not selected');
{$ENDIF}

//  if FReadOnly then
//    CheckDataHandlerSupport.DoSetReadOnly(Self, True);

  LDataSetProvider.DisableControls;
  try
    FIsBusy := True;
    //SyncActive; //TODO: Зачем это?

    if IsRoot then
      TDataHandler(Self).FIsOpening := True;

    InternalOpen;

    for I := 0 to Pred(Count) do begin
      Children[i].Open;
    end;
  finally
    LDataSetProvider.EnableControls;
//    SyncActive;
    if IsRoot then
      TDataHandler(Self).FIsOpening := False;
    FIsBusy := False;
  end;

  DataSetDataChanged(nil);
end;

procedure TCustomDataHandler.Post;
begin
  if not Enabled then
    Exit;
  CheckActive;
  if Assigned(DataSet) then
    DataSet.Post;
end;

//procedure TCustomDataHandler.Refresh1(ARecordOnly: Boolean);
//var
//  I: Integer;
//begin
//  if not Enabled then Exit;
//
//  CheckActive;
//
//  if ARecordOnly then
//    DataSet.RefreshRecord
//  else begin
//    DataSet.ReOpen([rofSavePosition]);
//    for I := 0 to Pred(Count) do
//      Children[I].Refresh1(False);
//  end;
//end;

procedure TCustomDataHandler.Remove(ADataHandler: TSubDataHandler);
begin
  FItems.Remove(ADataHandler);
end;

procedure TCustomDataHandler.RollBack;
var
  I: Integer;
begin
  if not Enabled then Exit;

  CheckActive;
  if not ExecCustomAllowEvent(FOnBeforeRollBack) then begin
    if CachedUpdates then DataSet.RestoreUpdates;
//    CheckDataHandlerSupport.DoRollback(Self);
//    ExecCustomEvent(FOnAfterRollback);
  end;

  for I := 0 to Pred(Count) do
    Children[I].RollBack;
end;

procedure TCustomDataHandler.SetDataSetProvider(ADataSet: TSQLDataSetProvider);
begin
  inherited DataSet := ADataSet;
end;

{$IFNDEF PACKAGE}
procedure TCustomDataHandler.SetDataSet(ADataSet: TDataSet);
var
  LOldDataSet, LNewDataSet: TDataSet;
begin
  LOldDataSet := (inherited DataSet).Source;
  LNewDataSet := ADataSet.Source;
  if (LOldDataSet <> ADataSet) and ((LNewDataSet = nil) or (LNewDataSet is TSQLDataSetProvider)) then begin
    if LOldDataSet <> nil then
      InternalDetachDataSet;

    inherited;

    if ((inherited DataSet) <> nil) then
      InternalAttachDataSet;
  end;
end;
{$ENDIF}

procedure TCustomDataHandler.Validate;
var
  I: Integer;
begin
  if not Enabled then
    Exit;
  CheckActive;
  if not ExecCustomAllowEvent(FOnBeforeValidate) then
  begin
    CheckDataHandlerSupport.DoValidate(Self);
//    ExecCustomEvent(FOnAfterValidate);
  end;

  for I := 0 to Pred(Count) do
    with Children[I] do
      if FActive then Validate;
end;

function TCustomDataHandler.CheckDataHandlerSupport: TCustomDataHandlerSupport;
begin
  Result := GetDataHandlerSupport;
  if not Assigned(Result) then
    Raise Exception.Create('Property '#171'SupportClass'#187' is not set');
end;

procedure TCustomDataHandler.CheckActive;
begin
  if (not FActive) and (State in [dsInactive]) then
    Raise EDataHandler.Create(SErrNotActive);
end;

procedure TCustomDataHandler.CheckPrivileges(APriv: TdhCustomMenuActionProperties);
begin
  Assert(APriv.Active);
end;

//procedure TCustomDataHandler.FreezeEvents;
//begin
//  EventsFreezed := True;
//end;

//procedure TCustomDataHandler.UnFreezeEvents;
//begin
//  EventsFreezed := False;
//end;

{ ******************************** TDataHandler ******************************** }

procedure TDataHandler.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FPropStorage) then
    FPropStorage := nil;
end;

procedure TDataHandler.Apply;
var
  LDataHandlerSupport: TCustomDataHandlerSupport;
  LList: TDataHandlerList;
  LListItem: Pointer;
begin
  if not ExecCustomAllowEvent(FOnBeforeApply) then begin
    LDataHandlerSupport := CheckDataHandlerSupport;
    LList := TDataHandlerList.Create(False);
    try
      FillChildrenList(LList, True, False);
      for LListItem in LList do if TCustomDataHandler(LListItem) is TSubDataHandler then with TSubDataHandler(LListItem) do
        if Enabled and FActive then
          XMLPost.ExcludedInProcessFields := '';

      for LListItem in LList do with TCustomDataHandler(LListItem) do
        if Enabled and FActive then
          LDataHandlerSupport.DoPostEditValues(TCustomDataHandler(LListItem));
    finally
      LList.Free
    end;

    InternalApply;
  end;
end;

function TDataHandler.GetDataHandlerSupport: TCustomDataHandlerSupport;
begin
  Result := FDataHandlerSupport;
end;

function TDataHandler.GetIsBusy: Boolean;
begin
  Result := FIsBusy
end;

procedure TDataHandler.SetPropStorage(const APropStorage: TdhItemPropStorage);
begin
  if FPropStorage <> APropStorage then
  begin
    if Assigned(FPropStorage) then
      FPropStorage.RemoveFreeNotification(Self);
    FPropStorage := APropStorage;
    if Assigned(FPropStorage) then
      FPropStorage.FreeNotification(Self);
  end;
end;

procedure TDataHandler.ApplyUpdates;
begin
  if not ExecCustomAllowEvent(FOnApplyUpdates) then
    Apply;
end;

function TDataHandler.CheckApplied: Boolean;
begin
  Assert(Enabled);

  Result := not Inserting;
  if not Result then begin
//    LText := Settings.ConfirmApplyRequiredText;
//    if (not LText.IsEmpty) then
//      FormatFieldsText(LText, ':');

    if Settings.ConfirmApplyRequiredText.IsEmpty
          or (CheckDataHandlerSupport.DoShowMessage(Settings.ConfirmApplyRequiredText, mAsk) = mrOk)
    then
      ApplyUpdates
    else
      Exit(False);

    Result := True;
  end;
end;

constructor TDataHandler.Create(AOwner: TComponent);
begin
  FEditMode := dhemCommitable;
  inherited;
end;

destructor TDataHandler.Destroy;
begin
  FreeAndNil(FDataHandlerSupport);
  inherited;
end;

function TDataHandler.HasParent: Boolean;
begin
  Result := False;
end;

function TDataHandler.GetParentComponent: TComponent;
begin
  Result := nil;
end;

function TDataHandler.GetRoot: TDataHandler;
begin
  Result := Self;
end;

function TDataHandler.GetSupportClass: TCustomDataHandlerSupportClass;
begin
  if Assigned(FDataHandlerSupport) then
    Result := TCustomDataHandlerSupportClass(FDataHandlerSupport.ClassType)
  else
    Result := nil;
end;

function TDataHandler.GetSupportClassName: String;
begin
  if Assigned(FDataHandlerSupport) then
    Result := FDataHandlerSupport.ClassName
  else
    Result := '';
end;

procedure TDataHandler.SetParentComponent(AParent: TComponent);
begin
//
end;

class function TDataHandler.IsRoot: Boolean;
begin
  Result := True;
end;

procedure TDataHandler.ModifiedChanged;
var
  LModified: Boolean;
begin
  LModified := Modified;
  if LModified <> FTotalModified then begin
    FTotalModified := LModified;
    if Assigned(FOnModifiedChanged) then
      FOnModifiedChanged(Self);
  end;
end;

procedure TDataHandler.SetSupport(const Value: TCustomDataHandlerSupport);
begin
  FDataHandlerSupport.Assign(Value);
end;

procedure TDataHandler.SetSupportClass(const AValue: TCustomDataHandlerSupportClass);
begin
  if SupportClass = AValue then Exit;

  FreeAndNil(FDataHandlerSupport);
  FDataHandlerSupport := AValue.Create(Owner);
end;

procedure TDataHandler.SetSupportClassName(const AValue: String);
var
  I: Integer;
begin
  if GetSupportClassName = AValue then Exit;

  if (not AValue.IsEmpty) then begin
    I := TCustomDataHandlerSupport.RegisteredClassCount - 1;
    while (I >= 0) and not SameText(TCustomDataHandlerSupport.GetRegisteredClassDescription(I), AValue) do Dec(I);
    if I < 0 then
      Raise Exception.Create('DataHandler can''t accept '#171 + AValue + #187' support class');

    SupportClass := TCustomDataHandlerSupport.GetRegisteredClass(I);
  end else
    SupportClass := nil;
end;

{ ******************************** TSubDataHandler ******************************** }

procedure TSubDataHandler.View;
var
  LVariables  : TNamedVariants;
begin
  if not Enabled then
    exit;
  CheckActive;
  CheckPrivileges(OptionsMenu.OptionsView);

  LVariables := OptionsMenu.OptionsView.GetItems;
  if not ExecCustomAllowVariabledEvent(FOnBeforeView, LVariables) then
    CheckDataHandlerSupport.DoView(Self, LVariables);
end;

function TSubDataHandler.New(AItem: TdhMultiNewItem): Boolean;
var
  LDone       : Boolean;
  LVariables  : TNamedVariants;
begin
  Result := False;
  if not Enabled then
    Exit;

  CheckActive;
  CheckPrivileges(OptionsMenu.OptionsNew);
  if OptionsMenu.KeyFieldsMode and (not GetRoot.CheckApplied) then
    Abort;

  if Assigned(AItem) then
    LVariables := AItem.Properties.GetItems
  else
    LVariables := OptionsMenu.OptionsNew.GetItems;

  LDone := False;
  if Assigned(FOnBeforeNew) {and (not EventsFreezed)} then
    FOnBeforeNew(Self, AItem, LVariables, LDone);

  if not LDone then begin
    CheckDataHandlerSupport.DoNew(Self, AItem, LVariables);
    ExecCustomEvent(FOnAfterNew, LVariables);
  end;

//  RowChanged;
  Result := True;
end;

function TSubDataHandler.Edit(ARaiseIfFalse: Boolean): Boolean;
var
  LVariables: TNamedVariants;
begin
  //if not Enabled then Exit(False);

  CheckActive;
  CheckPrivileges(OptionsMenu.OptionsEdit);

  LVariables := OptionsMenu.OptionsEdit.GetItems;
  Result := True;
  try
    if not ExecCustomAllowVariabledEvent(FOnBeforeEdit, LVariables) then begin
      CheckDataHandlerSupport.DoEdit(Self, LVariables);
      ExecCustomEvent(FOnAfterEdit, LVariables);
    end;
//    RowChanged;
  except on E:Exception do begin
    if (not ARaiseIfFalse) and (E is EAbort) then
      Result := False
    else
      Raise
  end end;
end;

function TSubDataHandler.Copy: Boolean;
var
  LVariables: TNamedVariants;
begin
  if not Enabled then Exit(False);

  CheckActive;
  CheckPrivileges(OptionsMenu.OptionsCopy);

  LVariables := OptionsMenu.OptionsCopy.GetItems;
  Result := True;
//  try

  if not ExecCustomAllowVariabledEvent(FOnBeforeCopy, LVariables) then begin
    CheckDataHandlerSupport.DoCopy(Self, LVariables);
    ExecCustomEvent(FOnAfterNew, LVariables);
  end;

//    RowChanged;
//  except on E:Exception do begin
//    if (not ARaiseIfFalse) and (E is EAbort) then
//      Result := False
//    else
//      Raise
//  end end;
end;

function TSubDataHandler.Delete: Boolean;
begin
  Result := False;
  if not Enabled then
    Exit;
  FIsBusy := True;
  try
    Result := DoDeleteDataSet;
  finally
    FIsBusy := False;
  end;
  DataSetDataChanged(nil);
end;

function TSubDataHandler.DoDeleteDataSet(ACascade: Boolean = False): Boolean;
// ACascade - удаление инициированно по каскаду
var
  LDataSet: TSQLDataSetProvider;
  LDataHandlerSupport: TCustomDataHandlerSupport;

  procedure InternalDoDeleteDataSet;
  var
    LDone: Boolean;
    I: Integer;
    LKeyFieldValues: Variant;
  begin
    LDone := False;
    if {(not EventsFreezed) and} Assigned(FOnBeforeDelete) then
      FOnBeforeDelete(Self, LDone);

    if not LDone then begin
      if (Count > 0) then begin
        LKeyFieldValues := KeyFieldValues;
        for I := 0 to Pred(Count) do
          Children[I].DoDeleteDataSet(True);
        Assert(VarIsEqual(LKeyFieldValues, KeyFieldValues));
      end;
      LDataSet.Delete;
    end;
  end;
begin
  LDataSet := DataSet;
  if (not FActive) or LDataSet.IsEmpty then Exit(False);

  LDataHandlerSupport := CheckDataHandlerSupport;
  if (not ACascade) and (not OptionsMenu.OptionsDelete.ConfirmText.IsEmpty) then begin
    if LDataHandlerSupport.DoShowMessage(OptionsMenu.OptionsDelete.ConfirmText, mAsk, LDataHandlerSupport.GetGrid(Self)) <> mrOk then
      Exit(False);
  end;

  if ACascade then begin
    while (not LDataSet.IsEmpty) do
      InternalDoDeleteDataSet;
  end else
    InternalDoDeleteDataSet;

  ExecCustomEvent(FOnAfterDelete);

  Result := True;
end;

procedure TSubDataHandler.Apply;
begin
  if not ExecCustomAllowEvent(FOnBeforeApply) then
    InternalApply;
end;

procedure TSubDataHandler.AssignParams;
var
  LParams, MParams: TParams;
  LParam: TParam;
  LField: TField;
  I: Integer;
  LDataHandler: TCustomDataHandler;
begin
  LParams := DataSet.Params;
  if not Assigned(LParams) then
    Exit;

  for I := 0 to Pred(LParams.Count) do begin
    LDataHandler := Parent;
    while Assigned(LDataHandler) do begin
      LField := LDataHandler.DataSet.FindField(RemovePrefixIfPresent(LParams[I].Name, SConst_PARAM_));
      if Assigned(LField) then begin
        LParams[I].Value := LField.AsVariant;
        Break;
      end;

      MParams := LDataHandler.DataSet.Params;
      if Assigned(MParams) then begin
        LParam := MParams.FindParam(LParams[I].Name);
        if not Assigned(LParam) then
          LParam := MParams.FindParam(SConst_PARAM_ + LParams[I].Name);

        if Assigned(LParam) then begin
          LParams[I].Value := LParam.Value;
          Break;
        end;
      end;

      if LDataHandler.IsRoot then Break;
      LDataHandler := TSubDataHandler(LDataHandler).Parent;
    end;
  end;
end;

constructor TSubDataHandler.Create(AOwner: TComponent);
begin
  FXMLPost := TSubDataHandlerXMLPostProperties.Create(Self);
  FOptionsMenu  := TdhOptionsMenu.Create(Self);
  inherited;
end;

destructor TSubDataHandler.Destroy;
begin
  FreeAndNil(FXMLPost);
  FreeAndNil(FOptionsMenu);
  inherited;

  if Assigned(Parent) then
    Parent.Remove(TSubDataHandler(Self));
end;

function TSubDataHandler.GetCanModify: Boolean;
var
  LDataSet: TDataSet;
begin
  if not Assigned(FParent) then
    Exit(False)
  else if OptionsMenu.KeyFieldsMode then
    Exit(True);

  Result := (inherited GetCanModify);
  if Result then begin
    LDataSet := FParent.DataSet;
    Result := Assigned(LDataSet)
              and
              (XMLPost.FieldName.IsEmpty or FParent.DataSet.FieldByName(XMLPost.FieldName).CanModify);
  end;
end;

function TSubDataHandler.GetDataHandlerSupport: TCustomDataHandlerSupport;
var
  LRoot: TDataHandler;
begin
  LRoot := GetRoot;
  if Assigned(LRoot) then
    Result := LRoot.GetDataHandlerSupport
  else
    Result := nil
end;

function TSubDataHandler.GetDetailKeyFieldNames: TdhFieldNames;
var
  LDataSet: TDataSet;
  LDataSetProvider: TSQLDataSetProvider absolute LDataSet;
begin
  LDataSet := DataSet;
  if Assigned(LDataSet) then
    Result := LDataSetProvider.DetailFields
  else
    Result := '';
end;

function TSubDataHandler.GetIsBusy: Boolean;
begin
  Result := FIsBusy or (Assigned(FParent) and FParent.GetIsBusy);
end;

function TSubDataHandler.GetMasterKeyFieldNames: TdhFieldNames;
var
  LDataSet: TDataSet;
  LDataSetProvider: TSQLDataSetProvider absolute LDataSet;
begin
  LDataSet := DataSet;
  if Assigned(LDataSet) then
    Result := LDataSetProvider.MasterFields
  else
    Result := '';
end;

function TSubDataHandler.GetXMLPostOnBeforeApply: TSubDataHandlerXMLPostOnBeforeApply;
begin
  Result := FXMLPost.FOnBeforeApply
end;

function TSubDataHandler.HasParent: Boolean;
begin
  Result := Assigned(FParent);
end;

procedure TSubDataHandler.InternalDetachDataSet;
begin
  with DataSet do begin
    MasterSource := nil;
    MasterFields := '';
    DetailFields := ''
  end;

  inherited;
end;

procedure TSubDataHandler.InternalAttachDataSet;
begin
  inherited;
  InitializeMasterDetail;
end;

procedure TSubDataHandler.InitializeMasterDetail;
begin
  with DataSet do begin
    MasterSource := Parent;
    MasterFields := FMasterKeyFieldNames;
  end;
  DetailKeyFieldNamesUpdated;
end;

class function TSubDataHandler.IsRoot: Boolean;
begin
  Result := False;
end;

procedure TSubDataHandler.DataSetOnNewRecord(Sender: TDataSet);
begin
  inherited;
  DataSet.FieldValues[DetailKeyFieldNames] := Parent.DataSet.FieldValues[MasterKeyFieldNames];
end;

procedure TSubDataHandler.SetXMLPostOnBeforeApply(const Value: TSubDataHandlerXMLPostOnBeforeApply);
begin
  FXMLPost.FOnBeforeApply := Value;
end;

procedure TSubDataHandler.UpdateActive;
var
  LDataHandlerSupport: TCustomDataHandlerSupport;
begin
  LDataHandlerSupport := GetDataHandlerSupport;
  if Assigned(LDataHandlerSupport) then
    LDataHandlerSupport.DoOnAfterOpen(Self);
end;

procedure TSubDataHandler.SetMasterKeyFieldNames(const Value: TdhFieldNames);
var
  LSQLDataSetProvider: TSQLDataSetProvider;
begin
  FMasterKeyFieldNames := Value;
  LSQLDataSetProvider := DataSet;
  if Assigned(LSQLDataSetProvider) then
    LSQLDataSetProvider.MasterFields := Value;
end;

procedure TSubDataHandler.DetailKeyFieldNamesUpdated;
var
  LSQLDataSetProvider: TSQLDataSetProvider;
begin
  LSQLDataSetProvider := DataSet;
  if Assigned(LSQLDataSetProvider) then begin
    LSQLDataSetProvider.DetailFields := FDetailKeyFieldNames;
    LSQLDataSetProvider.FieldsAutomations.AddServiceFields(FDetailKeyFieldNames);
  end;
end;

procedure TSubDataHandler.SetDetailKeyFieldNames(const AValue: TdhFieldNames);
begin
  if FDetailKeyFieldNames <> AValue then begin
    FDetailKeyFieldNames := AValue;
    DetailKeyFieldNamesUpdated;
  end;
end;

procedure TSubDataHandler.SetParent(ADataHandler: TCustomDataHandler);
begin
  FParent := ADataHandler;
  if Assigned(FParent) and ((inherited DataSet) <> nil) then
    InitializeMasterDetail;
end;

procedure TSubDataHandler.SetParentComponent(AParent: TComponent);
begin
  if (AParent is TCustomDataHandler) then begin
    if (AParent <> FParent) then
      TCustomDataHandler(AParent).Insert(Self)
  end;
end;

function TSubDataHandler.GetParentComponent: TComponent;
begin
  Result := FParent
end;

function TSubDataHandler.GetRoot: TDataHandler;
var
  LResult: TCustomDataHandler;
begin
  LResult := Parent;
  while True do begin
    Assert(Assigned(LResult));
    if LResult.IsRoot then Break;
    LResult := TSubDataHandler(LResult).Parent;
  end;

  Result := TDataHandler(LResult);
end;

{$IFNDEF PACKAGE}
procedure TSubDataHandler.SetDataSet(ADataSet: TDataSet);
begin
  Assert((ADataSet = nil) or (ADataSet.Source is TSQLDataSetProvider));
  inherited;
end;
{$ENDIF}

procedure TSubDataHandler.Loaded;
begin
  inherited;
  if (DetailKeyFieldNames = '') and (MasterKeyFieldNames <> '') then
    DetailKeyFieldNames := MasterKeyFieldNames
end;

procedure TSubDataHandler.MemoryDataSetProviderBeforePost(ADataSet: TDataSet);
var
  LApplyAction: TdhApplyAction;
begin
  if ADataSet.State = dsInsert then
    LApplyAction := dhaaAppendRecord
  else
    LApplyAction := dhaaUpdateRecord;

  if Assigned(FOnMemoryApply) then
    FOnMemoryApply(Self, ADataSet, LApplyAction);

  case LApplyAction of
    dhaaAppendRecord:
      with DataSet do begin
        Append;
          FieldValues[DetailKeyFieldNames] := Parent.DataSet.FieldValues[MasterKeyFieldNames];
          LoadFields(ADataSet.Fields, lvcIfNotPresent);
        Post;
      end;
    dhaaUpdateRecord:
      with DataSet do begin
        Edit;
          Fields.Load(ADataSet.Fields, lvcAlways);
        Post;
      end;
    dhaaAbort: Abort;
  end;
end;

{ ******************************** TdhPersistent ******************************* }

constructor TdhPersistent.Create(ADataHandler: TCustomDataHandler);
begin
  inherited Create;
  FDataHandler := ADataHandler;
end;

function TdhPersistent.GetOwner: TPersistent;
begin
  Result := DataHandler;
end;

{ ********************************* TdhParamEdit ******************************* }

constructor TdhParamEdit.Create(AActionProperties : TdhCustomActionProperties);
begin
  FActionProperties := AActionProperties;
  inherited Create;

  FVarList := TNamedVariants.Create(True);
end;

function TdhParamEdit.DataHandler: TSubDataHandler;
begin
  Result := FActionProperties.DataHandler
end;

procedure TdhParamEdit.Add(const AName: String; const AValue: Variant);
begin
  FVarList.SetValue(AName, AValue);
end;

procedure TdhParamEdit.Assign(Source: TPersistent);
var
  AStrings: TStrings absolute Source;
  i: Integer;
begin
  if Source is TStrings then
    for i := 0 to Pred(AStrings.Count) do
      if (AStrings[i].Trim <> '') and (Pos('=', AStrings[i]) = 0) then
        Raise EDataHandler.Create(SErrIvalidParamList);
  inherited;
end;

procedure TdhParamEdit.AssignParams(const AValues: Array of TNamedVariant);
begin
  FVarList.AssignValues(AValues);
end;

procedure TdhParamEdit.AssignParams(const AValues: String);
begin
  FVarList.AsClrTParams := AValues;
end;

function TdhParamEdit.VarItems: TNamedVariants;
var
  I: Integer;
  LLine: String;
begin
  Result := TNamedVariants.Create(True);
  for I := 0 to Pred(Count) do begin
    LLine := Strings[i];
    if not LLine.IsEmpty then
      with VarItem(LLine) do
        Result[Name] := Value
  end;

  Result.AssignValues(FVarList);
end;

{ TdhConstantParamEdit }

function TdhConstantParamEdit.VarItem(const AItem: String): TNamedVariant;
begin
  Result := TNamedVariant.Create(AItem)
end;

{ TdhDynamicParamEdit }

function TdhDynamicParamEdit.VarItem(const AItem: String): TNamedVariant;
var
  LStrVal: string;
  LPos: Integer;
begin
  LPos := Pos('=', AItem);
  Assert(LPos > 1);

  Result.Name := LeftStr(AItem, Pred(LPos)).TrimRight;
  LStrVal     := RightStr(AItem, Length(AItem) - LPos).TrimLeft;

  if RightStr(LStrVal, 2) = '()' then begin
    LStrVal := LeftStr(LStrVal, LStrVal.Length - 2);
    if SameText(LStrVal, SConst_Today) then
      Result.Value := Date
    else if SameText(LStrVal, SConst_Now) then
      Result.Value := Now
    else if SameText(LStrVal, SConst_NewGUId) then
      Result.Value := NewGUIdString
    else
      Raise Exception.CreateFmt(MFunctionNotFound, [LStrVal]);
  end else begin//if Assigned(FDataHandler) then
    if LStrVal[1] = ':' then
      LStrVal := RightStr(LStrVal, Pred(Length(LStrVal)));

    Result.Value := DataHandler.GetDataSetObjectValue(LStrVal, FActionProperties.ScanFields);
  end;
//  else
//    Result.Value := Null;
end;

{ ******************************* TdhOptionsMenu ******************************* }

procedure TdhOptionsMenu.AssignTo(Dest: TPersistent);
begin
  if Dest is TdhOptionsMenu then begin
    TdhOptionsMenu(Dest).ClassName := ClassName;
    TdhOptionsMenu(Dest).Constants := Constants;
    TdhOptionsMenu(Dest).Params    := Params;

    TdhOptionsMenu(Dest).OptionsView    := OptionsView;
    TdhOptionsMenu(Dest).OptionsEdit    := OptionsEdit;
    TdhOptionsMenu(Dest).OptionsNew     := OptionsNew;
  end;
end;

constructor TdhOptionsMenu.Create(ADataHandler: TSubDataHandler);
begin
  inherited Create(ADataHandler);

  FConstants := TdhConstantParamEdit.Create(Self);
  FParams    := TdhDynamicParamEdit.Create(Self);

  FOptionsNew     := TdhNewActionProperties.Create(ADataHandler);
  FOptionsView    := TdhViewActionProperties.Create(ADataHandler);
  FOptionsEdit    := TdhEditActionProperties.Create(ADataHandler);
  FOptionsCopy    := TdhCopyActionProperties.Create(ADataHandler);
  FOptionsDelete  := TdhDeleteActionProperties.Create(ADataHandler);
end;

destructor TdhOptionsMenu.Destroy;
begin
  FreeAndNil(FConstants);
  FreeAndNil(FParams);

  FreeAndNil(FOptionsNew);
  FreeAndNil(FOptionsView);
  FreeAndNil(FOptionsEdit);
  FreeAndNil(FOptionsCopy);
  FreeAndNil(FOptionsDelete);
  //FreeAndNil(FOptionsRefresh);

  inherited;
end;

function TdhOptionsMenu.ScanFields: Boolean;
begin
  Result := False;
end;

{ ********************************* TdhSettings ******************************** }

constructor TdhSettings.Create(ADataHandler: TCustomDataHandler);
begin
  inherited;

//  FNotifyChanges := True;

  FGenerateKeyField := dhGkfNever;
  FCascadeDelete := True;
{$IFNDEF PACKAGE}
  FConfirmApplyRequiredText := SBaseProperties.Confirmation_ApplyRequired;
{$ENDIF}
end;

function TdhSettings.IsModifiedApplyReqdText: Boolean;
begin
  Result := ConfirmApplyRequiredText <> {$IFNDEF PACKAGE}SBaseProperties.Confirmation_ApplyRequired{$ELSE}''{$ENDIF}
end;

procedure TdhSettings.SetGenerateKeyField(const AValue: TdhGenerateKeyFieldMethod);
begin
  if FGenerateKeyField <> AValue then begin
    FGenerateKeyField := AValue;
    if (AValue <> dhGkfNever) and DataHandler.FActive then
      DataHandler.InternalPrepareIdentity;
  end;
end;

{ ****************************** TdhMultiNewItem ******************************* }

constructor TdhMultiNewItem.Create(ACollection: TCollection);
var
  LCollection: TdhMultiNewItems absolute ACollection;
begin
  inherited;

  FProperties   := TdhMultiNewActionProperties.Create(Self);
  FItems        := TdhMultiNewItems.Create(LCollection.FDataHandler, Self);

  FDependencies := DefaultDependencies;
end;

function TdhMultiNewItem.DefaultDependencies: TdhActionDependencies;
var
  LParentItem: TdhMultiNewItem;
begin
  LParentItem := TdhMultiNewItems(Collection).FParentItem;
  if LParentItem = nil then
    Result := TdhMultiNewItems(Collection).FDataHandler.OptionsMenu.OptionsNew.Dependencies
  else
    Result := LParentItem.Dependencies
end;

destructor TdhMultiNewItem.Destroy;
begin
  //FreeAndNil(FAction);
  FreeAndNil(FProperties);
  FreeAndNil(FItems);

  inherited;
end;

procedure TdhMultiNewItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TdhMultiNewItem then begin
    //TdhMultiNewItem(Dest).Action := Action;
    TdhMultiNewItem(Dest).Properties    := Properties;
    TdhMultiNewItem(Dest).Caption       := Caption;
    TdhMultiNewItem(Dest).Dependencies  := Dependencies;
    TdhMultiNewItem(Dest).OnClick       := OnClick;
  end else
    inherited;
end;

//procedure TdhMultiNewItem.SetAction(const Value: TdhAction);
//begin
//  FAction.Assign(Value);
//end;

procedure TdhMultiNewItem.SetDependencies(const AValue: TdhActionDependencies);
begin
  FDependencies := CheckDependencies(AValue)
end;

procedure TdhMultiNewItem.SetItems(const Value: TdhMultiNewItems);
begin
  if Value = nil then
    FItems.Clear
  else
    FItems.Assign(Value);
end;

procedure TdhMultiNewItem.SetProperties(const Value: TdhMultiNewActionProperties);
begin
  FProperties.Assign(Value);
end;

function TdhMultiNewItem.GetDisplayName: string;
begin
  if Caption = '' then
    Result := inherited GetDisplayName
  else
    Result := Caption;
end;

function TdhMultiNewItem.IsDependenciesStored: Boolean;
begin
  Result := (FDependencies <> DefaultDependencies)
end;

{ ******************************** TdhMultiNewItems ********************************* }

constructor TdhMultiNewItems.Create(ADataHandler: TSubDataHandler; AParentItem: TdhMultiNewItem);
begin
  FDataHandler := ADataHandler;
  FParentItem  := AParentItem;
  inherited Create(TdhMultiNewItem);
end;

function TdhMultiNewItems.Add: TdhMultiNewItem;
begin
  Result := TdhMultiNewItem(inherited Add);
end;

function TdhMultiNewItems.GetItem(AIndex: Integer): TdhMultiNewItem;
begin
  Result := TdhMultiNewItem(inherited GetItem(AIndex));
end;

procedure TdhMultiNewItems.SetItem(AIndex: Integer; const AValue: TdhMultiNewItem);
begin
  inherited SetItem(AIndex, AValue);
end;

procedure TdhMultiNewItems.Update(Item: TCollectionItem);
var
  LDataHandlerSupport: TCustomDataHandlerSupport;
begin
  inherited;

  if Assigned(FDataHandler) then
    with FDataHandler do
      if FActive and GetRoot.FActive and (OptionsMenu.OptionsNew.Items.UpdateCount = 0) then begin
        LDataHandlerSupport := GetDataHandlerSupport;
        if Assigned(LDataHandlerSupport) then
          LDataHandlerSupport.DoOptionsMenuNewItemsChanged(FDataHandler);
      end;
end;

{ ***************************** TdhItemPropStorage ***************************** }

constructor TdhItemPropStorage.Create(AOwner: TComponent);
begin
  FDefaultImages := TdhDefaultImages.Create;
  inherited;
end;

destructor TdhItemPropStorage.Destroy;
begin
  FreeAndNil(FDefaultImages);
  inherited;
end;

function TdhItemPropStorage.BarPropsClass: TdxBarPropsClass;
begin
  Result := TdhProps;
end;

//procedure TdhItemPropStorage.SetBarManager(const Value: TdxBarManager);
//begin
//  if Value <> BarManager then
//    FDefaultImages.BarManager := Value;
//  inherited;
//end;

function TdhItemPropStorage.FindByBarItem(AdxBarItem: TdxBarItem): TdhPropItem;
begin
  Result := Properties.FindByBarItem(AdxBarItem) as TdhPropItem
end;

{ ****************************** TdhDefaultImages ****************************** }

constructor TdhDefaultImages.Create;
begin
  inherited;
  FNew := -1;
  FEdit := -1;
  FView := -1;
  FDelete := -1;
  FRestore := -1;
  FRefresh := -1;
  FCollapse := -1;
  FExpand := -1;
  FApply := -1;
end;

{ ******************************** TdhPropItem ********************************* }

constructor TdhPropItem.Create(Collection: TCollection);
begin
  inherited;

  FPosition := ipMiddle;

//  FAction := TdhCommandAction.Create(nil);
//  FProperties := TdhCommandActionProperties.Create(nil);
end;

//destructor TdhPropItem.Destroy;
//begin
//  FreeAndNil(FAction);
//  FreeAndNil(FProperties);
//  inherited;
//end;

procedure TdhPropItem.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TdhPropItem then
  begin
    TdhPropItem(Dest).BeginMenuGroup := BeginMenuGroup;
    TdhPropItem(Dest).Position := Position;
  end
end;

procedure TdhPropItem.SetDataHandler(const Value: TCustomDataHandler);
begin
  if FDataHandler <> Value then
  begin
    FDataHandler := Value;
//    Action.FDataHandler := Value;
//    Properties.FDataHandler := Value;
//    Properties.FParams.FDataHandler := Value;
  end;
end;

function TdhProps.BarPropItemClass: TdxBarPropItemClass;
begin
  Result := TdhPropItem;
end;

function TdhCustomActionProperties.GetDataHandler: TSubDataHandler;
begin
  Result := TSubDataHandler(FDataHandler)
end;

constructor TdhAction.Create(ADataHandler: TSubDataHandler);
begin
  inherited Create(ADataHandler);
//  FModal := True;
//  FActionMode := amDefault;
end;

function TdhAction.GetDataHandler: TSubDataHandler;
begin
  Result := TSubDataHandler(FDataHandler);
end;

{ TdhMultiNewActionProperties }

constructor TdhMultiNewActionProperties.Create(AdhMultiNewItem: TdhMultiNewItem);
begin
  inherited Create(TdhMultiNewItems(AdhMultiNewItem.Collection).FDataHandler);
  FdhMultiNewItem := AdhMultiNewItem;
//  FActionKind := dhakNew;
end;

function TdhMultiNewActionProperties.GetActionKind: TdhActionKind;
begin
  Result := dhakNew;
end;

function TdhMultiNewActionProperties.GetActionMode: TdhActionMode;
begin
  if FActionMode = amDefault then
    if MultiNewItems.FParentItem = nil then
      Result := DataHandler.OptionsMenu.OptionsNew.GetActionMode
    else
      Result := MultiNewItems.FParentItem.Properties.GetActionMode
  else
    Result := FActionMode
end;

function TdhMultiNewActionProperties.GetClassName: string;
begin
  Result := FdhMultiNewItem.Properties.ClassName;
  if Result.IsEmpty then
    if (MultiNewItems.FParentItem <> nil) then
      Result := MultiNewItems.FParentItem.Properties.GetClassName
    else begin
      Result := DataHandler.OptionsMenu.OptionsNew.ClassName;
      if Result.IsEmpty then
        Result := DataHandler.OptionsMenu.ClassName;
    end
end;

function TdhMultiNewActionProperties.GetItems: TNamedVariants;
begin
  if DataHandler <> nil then begin
    if MultiNewItems.FParentItem <> nil then
      Result := MultiNewItems.FParentItem.Properties.GetItems
    else begin
      Result := inherited GetItems;
      Result.AssignValues(DataHandler.OptionsMenu.OptionsNew.Params.VarItems);
      Result.AssignValues(DataHandler.OptionsMenu.OptionsNew.Constants.VarItems);
    end;

    Result.AssignValues(Params.VarItems);
    Result.AssignValues(Constants.VarItems);
  end else
    Result := inherited GetItems;
end;

function TdhMultiNewActionProperties.InternalVisible: Boolean;
begin
  Result := (inherited InternalVisible)
    and (DataHandler.OptionsMenu.KeyFieldsMode or (DataHandler.GetRoot.EditMode <> dhemReadOnly));
end;

function TdhMultiNewActionProperties.MultiNewItems: TdhMultiNewItems;
begin
  Result := TdhMultiNewItems(FdhMultiNewItem.Collection);
end;

function TdhMultiNewActionProperties.ScanFields: Boolean;
begin
  Result := (dhdFocus in FdhMultiNewItem.Dependencies)
end;

{ *************************** TdhNewActionProperties *************************** }

constructor TdhNewActionProperties.Create(ADataHandler: TSubDataHandler);
begin
  inherited;

  FItems := TdhMultiNewItems.Create(ADataHandler, nil);
//  FActionKind := dhakNew;
  FDependencies := [dhdActive];
end;

destructor TdhNewActionProperties.Destroy;
begin
  inherited;
  FreeAndNil(FItems);
end;

function TdhNewActionProperties.GetActionKind: TdhActionKind;
begin
  Result := dhakNew;
end;

function TdhNewActionProperties.InternalVisible: Boolean;
begin
  Result := (inherited InternalVisible)
          and (DataHandler.OptionsMenu.KeyFieldsMode or (DataHandler.GetRoot.EditMode <> dhemReadOnly));
end;

function TdhNewActionProperties.IsItemsStored: Boolean;
begin
  Result := FItems.Count > 0
end;

procedure TdhNewActionProperties.Load(ADataSet: TSQLDataSetProvider; const AFieldName: String; AMode: TdhMultiNewItemsLoadMode);
var
  LIdField: TField;
  //LBeginGroupField: TField;

  LEnabledField     : TField;
  LCaptionField     : TField;
  LConstantsField   : TField;
  LDependenciesField: TField;

  //LConfirmationField, LSQLTextField, LImageIndexField: TField;
  //LScriptProcedureField, LScriptParamsField: TField;
  //LClassNameField, LClassConstantsField, LClassParamsField: TField;
  //LOnSelectSQLTextField, LOnSelectScriptProcedureField: TField;

  function StrToDependencies(const AText: String): TdhActionDependencies;
  var
    LTypeInfo: PTypeInfo;
    LItem: TStringItem;
  begin
    LTypeInfo := TypeInfo(TdhActionDependence);
    for LItem in EnumStringItems(AText, ',') do
      Include(Result, TdhActionDependence(GetEnumValue(LTypeInfo, 'dhd' + LItem.Value.Trim)));
    Result := [];
  end;

  procedure CreateMenuLevel(AItemLinks: TdhMultiNewItems);
  var
    LEnabled, LFound: Boolean;
    LdhMenuItem: TdhMultiNewItem;
    LItem: TCollectionItem;
    LParent: Variant;

    procedure AddItemReaction;
    begin
        if Assigned(LConstantsField)  then LdhMenuItem.Properties.Constants.AssignParams(LConstantsField.AsString);
//
//        if Assigned(LClassNameField)        then OnClick.ClassName := LClassNameField.AsString;
//        if Assigned(LClassConstantsField)   then OnClick.ClassConstants.AsClrTParams := LClassConstantsField.AsString;
//        if Assigned(LClassParamsField)      then OnClick.ClassParams := LClassParamsField.AsString;
    end;
  begin
    LEnabled := False;
    if AItemLinks.FParentItem = nil then
      LParent  := Null
    else
      LParent  := AItemLinks.FParentItem.FTag;

//    LDefault := nil;

    LFound := ADataSet.Locate(SConst_Parent_Id, LParent, []);
    while LFound do begin
      LdhMenuItem := AItemLinks.Add;
      LdhMenuItem.Caption := LCaptionField.AsString;

      if Assigned(LEnabledField) then LdhMenuItem.Properties.Enabled := LEnabledField.AsBoolean;
//      if Assigned(LImageIndexField) then LdxBarItem.ImageIndex := LImageIndexField.AsInteger;
      if Assigned(LDependenciesField) and (not LDependenciesField.IsNull) then LdhMenuItem.Dependencies := StrToDependencies(LDependenciesField.AsString);


      if LdhMenuItem.Properties.Enabled then
        LEnabled := True;

      if VarIsPresent(ADataSet.Lookup(SConst_Parent_Id, LIdField.AsVariant, SConst_Id)) then
        LdhMenuItem.Tag := LIdField.AsVariant
      else begin
        if LdhMenuItem.Properties.Enabled then
          AddItemReaction;
      end;

      // BeginGroup := Assigned(LBeginGroupField) and LBeginGroupField.AsBoolean;

      LFound := ADataSet.LocateEx(SConst_Parent_Id, LParent, [bloNext]);
    end;

    if LEnabled then begin
      for LItem in AItemLinks do with TdhMultiNewItem(LItem) do
        if Properties.Enabled and VarIsPresent(Tag) then begin
          Items.BeginUpdate;
          try
            CreateMenulevel(Items);
          finally
            Items.EndUpdate;
          end;
        end;
    end else if Assigned(AItemLinks.FParentItem) then
      AItemLinks.FParentItem.Properties.Enabled := False;

    if not Assigned(AItemLinks.FParentItem) then
      AItemLinks.FDataHandler.OptionsMenu.OptionsNew.Enabled := LEnabled;
  end;
begin
  Items.BeginUpdate;
  try
    Items.Clear;
    if ADataSet.IsEmpty then begin
      Enabled := False;
      Exit;
    end;

    ADataSet.DisableControls;
    try
      LIdField              := ADataSet.FieldByName(SConst_Id);

      LCaptionField         := ADataSet.FindField(SConst_Name);
      if (LCaptionField = nil) then
        LCaptionField := ADataSet.FieldByName(SConst_Caption);

//      LImageIndexField      := ADataSet.FindField(SConst_ImageIndex);

      LEnabledField         := ADataSet.FindField(SConst_Enabled);
      LConstantsField       := ADataSet.FindField(SConst_Constants);
      LDependenciesField    := ADataSet.FindField(SConst_Dependencies);

//      LBeginGroupField      := ADataSet.FindField(SConst_BeginGroup);

//      LClassNameField       := ADataSet.FindField(SConst_ClassName);
//      LClassConstantsField  := ADataSet.FindField(SConst_ClassConstants);
//      LClassParamsField     := ADataSet.FindField(SConst_ClassParams);

      CreateMenulevel(Items);
    finally
      ADataSet.EnableControls;
    end;
  finally
    Items.EndUpdate;
  end;
end;

function TdhNewActionProperties.ScanFields: Boolean;
begin
  Result := (dhdFocus in FDependencies)
end;

procedure TdhNewActionProperties.SetDependencies(const AValue: TdhActionDependencies);
begin
  FDependencies := CheckDependencies(AValue)
end;

procedure TdhNewActionProperties.SetItems(const Value: TdhMultiNewItems);
begin
  if Value = nil then
    FItems.Clear
  else
    FItems.Assign(Value);
end;

{ ************************** TdhDeleteActionProperties ************************* }

constructor TdhDeleteActionProperties.Create(ADataHandler: TSubDataHandler);
begin
  inherited;
  FConfirmText := SConfirmDel;
end;

function TdhDeleteActionProperties.InternalVisible: Boolean;
begin
  Result := (inherited InternalVisible)
          and (DataHandler.OptionsMenu.KeyFieldsMode or (DataHandler.GetRoot.EditMode <> dhemReadOnly));
end;

function TdhDeleteActionProperties.IsModifiedConfirmText: Boolean;
begin
  Result := ConfirmText <> SConfirmDel
end;

function TdhDeleteActionProperties.ScanFields: Boolean;
begin
  Result := True;
end;

{ TSubDataHandlerXMLPostProperties }

procedure TSubDataHandlerXMLPostProperties.AddExcludedFields(const AFields: String);
begin
  if not AFields.IsEmpty then
    ExcludedFields := ArraysMerge([ExcludedFields, AFields], ',');
end;

function TSubDataHandlerXMLPostProperties.AllExcludedFields: String;
begin
  Result := ArraysMerge
            (
              [
                ExcludedFields,
                ExcludedInProcessFields,
                FDataHandler.LockOptions.MessageFieldName,
                FDataHandler.DataSet.FieldsAutomations.ModifiAbleFieldsFieldName
              ],
              ','
            )
end;

constructor TSubDataHandlerXMLPostProperties.Create(AOwner: TCustomDataHandler);
begin
  FDataHandler := AOwner;
  FOnlyIfModified := True;
  FDataTag := XMLConst_RECORD;
//  FSummaryTag := Default_XMLSummaryTag;

  inherited Create;
end;

function TSubDataHandlerXMLPostProperties.IsDataTagStored: Boolean;
begin
  Result := not SameText(FDataTag, XMLConst_RECORD)
end;

procedure TSubDataHandlerXMLPostProperties.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineProperty('AddExcludedFields', ReadAddExcludedFields, nil, False);
end;

procedure TSubDataHandlerXMLPostProperties.ReadAddExcludedFields(Reader: TReader);
begin
  AddExcludedFields(Reader.ReadString);
end;

{ TdhViewActionProperties }

//constructor TdhViewActionProperties.Create(ASubDataHandler: TSubDataHandler);
//begin
//  inherited;
//  FActionKind := dhakView;
//end;

function TdhViewActionProperties.GetActionKind: TdhActionKind;
begin
  Result := dhakView;
end;

function TdhViewActionProperties.InternalVisible: Boolean;
begin
  Result := (inherited InternalVisible)
            and (GetActionMode in [amKeyFields, amKeyFieldsModal, amDataSet]);
end;

function TdhViewActionProperties.ScanFields: Boolean;
begin
  Result := True;
end;

{ TdhEditActionProperties }

//constructor TdhEditActionProperties.Create(ASubDataHandler: TSubDataHandler);
//begin
//  inherited;
//  FActionKind := dhakEdit;
//end;

function TdhEditActionProperties.GetActionKind: TdhActionKind;
begin
  Result := dhakEdit;
end;

function TdhEditActionProperties.InternalVisible: Boolean;
begin
  Result := (inherited InternalVisible)
          and (DataHandler.OptionsMenu.KeyFieldsMode or (DataHandler.GetRoot.EditMode <> dhemReadOnly));
end;

function TdhEditActionProperties.ScanFields: Boolean;
begin
  Result := True;
end;

{ TdhRequiredFields }

constructor TdhRequiredFields.Create;
begin
  inherited Create(TdhRequiredField);
end;

function TdhRequiredFields.GetItem(AIndex: Integer): TdhRequiredField;
begin
  Result := TdhRequiredField(inherited GetItem(AIndex));
end;

{ TdhRequiredField }

constructor TdhRequiredField.Create(Collection: TCollection);
begin
  inherited;
  FSubControlIndex := -1;
end;

{ TCustomDataHandlerSupport }

constructor TCustomDataHandlerSupport.Create(AOwner: TComponent);
begin
  FOwner := AOwner;
  inherited Create;
end;

class function TCustomDataHandlerSupport.GetRegisteredClass(AIndex: Integer): TCustomDataHandlerSupportClass;
begin
  Result := FSupports[AIndex]
end;

class function TCustomDataHandlerSupport.GetRegisteredClassDescription(AIndex: Integer): String;
begin
  Result := FSupports[AIndex].ClassName
end;

class procedure TCustomDataHandlerSupport.RegisterClass(AClass: TCustomDataHandlerSupportClass);
var
  I: Integer;
begin
  Assert(AClass <> nil);
  for I := 0 to RegisteredClassCount - 1 do
    if FSupports[I] = AClass then Exit;
  I := RegisteredClassCount;
  SetLength(FSupports, I + 1);
  FSupports[I] := AClass;
end;

class function TCustomDataHandlerSupport.RegisteredClassCount: Integer;
begin
  Result := Length(FSupports)
end;

class procedure TCustomDataHandlerSupport.ReleaseClasses;
begin
  SetLength(FSupports, 0);
end;

{ TdhCustomMenuActionProperties }

function TdhCustomMenuActionProperties.Active: Boolean;
begin
  Result := InternalVisible and Enabled;
end;

procedure TdhCustomMenuActionProperties.AssignTo(Dest: TPersistent);
begin
  if Dest is TdhCustomMenuActionProperties then begin
    TdhCustomMenuActionProperties(Dest).Enabled := Enabled;
    TdhCustomMenuActionProperties(Dest).Visible := Visible;
  end else
    inherited;
end;

constructor TdhCustomMenuActionProperties.Create(ADataHandler: TSubDataHandler);
begin
  FEnabled := True;
  FVisible := True;

  inherited Create(ADataHandler);
end;

function TdhCustomMenuActionProperties.GetActionKind: TdhActionKind;
begin
  Result := dhakCustom;
end;

function TdhCustomMenuActionProperties.InternalVisible: Boolean;
begin
  Result := FVisible
end;

procedure TdhClassMenuActionProperties.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TdhClassMenuActionProperties then
  begin
    TdhClassMenuActionProperties(Dest).Params := FParams;
    TdhClassMenuActionProperties(Dest).Constants := FConstants;
  end;
end;

constructor TdhClassMenuActionProperties.Create(ASubDataHandler: TSubDataHandler);
begin
  inherited Create(ASubDataHandler);

  FActionMode := amDefault;
  FParams     := TdhDynamicParamEdit.Create(Self);
  FConstants  := TdhConstantParamEdit.Create(Self);
end;

destructor TdhClassMenuActionProperties.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FConstants);

  inherited;
end;

function TdhClassMenuActionProperties.GetActionMode: TdhActionMode;
begin
  if FActionMode = amDefault then begin
    if DataHandler.OptionsMenu.KeyFieldsMode then
      Result := amKeyFields
    else if GetClassName.IsEmpty then
      Result := amInplace
    else
      Result := amDataSet
  end else
    Result := FActionMode
end;

function TdhClassMenuActionProperties.GetClassName: string;
begin
  if FClassName.IsEmpty then
    Result := TSubDataHandler(FDataHandler).OptionsMenu.ClassName
  else
    Result := FClassName
end;

function TdhClassMenuActionProperties.GetItems: TNamedVariants;
begin
  Result := TNamedVariants.Create(DataHandler.OptionsMenu.Params.VarItems, True);
  Result.AssignValues(Params.VarItems);
  Result.AssignValues(Constants.VarItems);

  Result.AssignValues(DataHandler.OptionsMenu.Constants.VarItems);
end;

procedure TdhClassMenuActionProperties.SetConstants(const Value: TdhConstantParamEdit);
begin
  FConstants.Assign(Value);
end;

procedure TdhClassMenuActionProperties.SetParams(const Value: TdhDynamicParamEdit);
begin
  FParams.Assign(Value);
end;

{ TdhCopyActionProperties }

constructor TdhCopyActionProperties.Create(ASubDataHandler: TSubDataHandler);
begin
  inherited;
//  FActionKind := dhakCopy;
  FVisible    := False;
end;

function TdhCopyActionProperties.GetActionKind: TdhActionKind;
begin
  Result := dhakCopy;
end;

function TdhCopyActionProperties.InternalVisible: Boolean;
begin
  Result := (inherited InternalVisible)
          and (DataHandler.OptionsMenu.KeyFieldsMode or (DataHandler.GetRoot.EditMode <> dhemReadOnly));
end;

function TdhCopyActionProperties.ScanFields: Boolean;
begin
  Result := True;
end;

{ TdhOptionsMenuActionProperties }

procedure TdhOptionsMenuActionProperties.AssignTo(Dest: TPersistent);
begin
  if Dest is TdhOptionsMenuActionProperties then begin
    TdhOptionsMenuActionProperties(Dest).OnGetEnabled := OnGetEnabled;
  end else
    inherited;
end;

constructor TdhOptionsMenuActionProperties.Create(ADataHandler: TSubDataHandler);
begin
  inherited;
  FOnGetEnabled := TdhOptionsMenuActionPropertiesOnGetEnabled.Create;
end;

destructor TdhOptionsMenuActionProperties.Destroy;
begin
  FreeAndNil(FOnGetEnabled);
  inherited;
end;

procedure TdhOptionsMenuActionProperties.SetOnGetEnabled(const AValue: TdhOptionsMenuActionPropertiesOnGetEnabled);
begin
  if Assigned(AValue) then
    FOnGetEnabled.Assign(AValue);
end;

{ TdhOptionsMenuActionPropertiesOnGetEnabled }

procedure TdhOptionsMenuActionPropertiesOnGetEnabled.AssignTo(Dest: TPersistent);
begin
  if Dest is TdhOptionsMenuActionPropertiesOnGetEnabled then
  begin
    TdhOptionsMenuActionPropertiesOnGetEnabled(Dest).Condition      := Condition;
    TdhOptionsMenuActionPropertiesOnGetEnabled(Dest).SystemFunction := SystemFunction;
    TdhOptionsMenuActionPropertiesOnGetEnabled(Dest).ScriptFunction := ScriptFunction;
  end
  else
    inherited;
end;

initialization
  RegisterClasses
  (
    [
      TSubDataHandler,
      TdhItemPropStorage,
      TdhCustomMenuActionProperties,
      TdhOptionsMenuActionPropertiesOnGetEnabled,
      TdhClassMenuActionProperties
    ]
  );

finalization
  TCustomDataHandlerSupport.ReleaseClasses;
  UnRegisterClasses
  (
    [
      TSubDataHandler,
      TdhItemPropStorage,
      TdhCustomMenuActionProperties,
      TdhOptionsMenuActionPropertiesOnGetEnabled,
      TdhClassMenuActionProperties
    ]
  );

end.
