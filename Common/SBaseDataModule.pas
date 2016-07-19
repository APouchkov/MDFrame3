unit SBaseDataModule;
{$I config.inc}

interface

uses
  Types, NamedVariables
{$IFDEF PACKAGE}
  , DesignIntf
  , SBaseEventPropEditor
{$ELSE}
  , VirtualTable
  , SBaseExpression
{$ENDIF}

{$IFNDEF PACKAGE}
{$IFNDEF REPORT_EXTERNAL}
  , Forms
{$ENDIF}
{$ENDIF}

{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter, fs_ievents
{$ENDIF}
  , ComObj, SysUtils, Classes
{$IFNDEF PACKAGE}
  , Controls
{$ENDIF}
  , Windows, Messages, DB
  , DateUtils, FieldDataLink, ActiveX, SysConst
  , Variants, StrUtils, DBCommon, DSIntf, Math
{$IFDEF DELPHIXE3}
  , System.Generics.Collections
{$ENDIF}
  , SBaseConstants, SBaseDataHelperInternal, SBaseUtils
  ;

const
  WM_BaseDataSetThreadFinished = WM_USER + $3000;
  Default_UpdateStatusFilter = [usUnmodified, usModified, usInserted];
  All_UpdateStatusFilter = [usUnmodified, usModified, usInserted, usDeleted];

  SConnectionProviderNotDefined = 'Connection provider is not defined';
  SSQLTextNotDefined = 'SQL Text is not defined';

type
  PExprParser = ^TExprParser;
  TDataSetClass = class of TDataSet;
  TDataSetCrack = class(TDataSet);

  TDataSetStateBookmark = packed record
  private
    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
  public
    NotNull           : Boolean;
    Filtered          : Boolean;
    Filter            : String;
    UpdateStatusFilter: TUpdateStatusSet;
//    ControlsDisabled  : Boolean;
    procedure Clear;
    property AsVariant : Variant read GetAsVariant write SetAsVariant;
  end;
  PDataSetStateBookmark = ^TDataSetStateBookmark;

  TDataSourceBookmark = packed record
    KeyFieldValues: Variant;
    RecordIndex   : Integer;
    RowIndex      : Integer;
    Reserved      : Variant;
  private
    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
  public
    constructor Create(AClear: Boolean);
    procedure Clear;
    property AsVariant : Variant read GetAsVariant write SetAsVariant;
  end;

  TDataSourceStateBookmark = packed record
    DataSource  : TDataSourceBookmark;
    DataSetState: TDataSetStateBookmark;
  private
    function GetAsVariant: Variant;
    procedure SetAsVariant(const Value: Variant);
  public
    procedure Clear;
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  end;

  TDataSetStateBookmarkRestore = (sbrUpdateStatusFilter, sbrFiltered, sbrFilter, sbrDisableCount);
  TDataSetStateBookmarkRestores = set of TDataSetStateBookmarkRestore;
  TDataSourceBookmarkRestore = (sbrKeyFieldValues, sbrRecordIndex, sbrRowIndex);
  TDataSourceBookmarkRestores = set of TDataSourceBookmarkRestore;
  TDataSetReOpenFlag = (rofSavePosition, rofOnlyIfChanged, rofCloseIfParamsRequired);
  TDataSetReOpenFlags = set of TDataSetReOpenFlag;
  TDataSetInitDetail = (dsidChanged, dsidParamsRequired);
  TDataSetInitDetails = set of TDataSetInitDetail;

const
  DataSetStateBookmarkRestores_All = [sbrUpdateStatusFilter, sbrFiltered, sbrFilter, sbrDisableCount];
  SDataSetStateBookmarkRestores_All: String = '[sbrUpdateStatusFilter, sbrFiltered, sbrFilter, sbrDisableCount]';
  DataSourceBookmarkRestores_All = [sbrKeyFieldValues, sbrRecordIndex, sbrRowIndex];
  SDataSourceBookmarkRestores_All: String = '[sbrKeyFieldValues, sbrRecordIndex, sbrRowIndex]';

type
  TCustomBaseDataSetProvider = class;
  TCustomSQLDataSetProvider = class;

  TBaseDataSetObject = (bdsoOriginal, bdsoCDS, bdsoCDSCached, bdsoCDSPublic, bdsoMemory);
{
  bdsoOriginal          - Определённый провайдером DataSet
  bdsoCDS               - DataSet, не закрывающийся при разрыве соединения
  bdsoCDSCached         - Тоже самое, только если уже есть открытый экземпляр запроса - то вернётся ссылка на него
  bdsoCDSPublic         - Тоже самое, только шарится один ДатаСет на всех. Не поддерживает фильтрацию.
  bdsoMemory            - Датасет в памяти
}
  TSQLDataSetProvider = class;

  TPublicDataSet = class(TInternalPublicDataSet)
  end;

  TBasePublicDataSet = class
  private
    [Weak] FSQLConnectionProvider: TComponent;
    FCommandText: String;
    FParams: TParams;
  public
    DataSet     : TPublicDataSet;

    Owners      : TList<TComponent>;
    ReleaseTime : TDateTime;
  public
    constructor Create(ASQLConnectionProvider: TComponent; ACommandText: String; AParams: TParams); reintroduce;
    destructor Destroy; override;

    function  Active(ALiteMode: Boolean): Boolean;
    procedure Open(ALiteMode: Boolean);

    property SQLConnectionProvider: TComponent read FSQLConnectionProvider;
    property CommandText: String read FCommandText;
    property Params: TParams read FParams;
  end;

  TBaseSQLStatementType = (bstQuery, bstInsert, bstUpdate, bstDelete, bstLock, bstRefresh);
  TBaseSQLStatementTypes = set of TBaseSQLStatementType;

  TBaseDataSetUpdateExecuteEvent = procedure(ASender: TDataSet; StatementTypes: TBaseSQLStatementTypes; AParams: TParams) of object;

  TCustomConnectionClass = class of TCustomConnection;
  TSQLConnectionProviderClass = class of TSQLConnectionProvider;
  TSQLConnectionProvider = class;
  TObjectMethod = procedure of object;
  TSQLConnectionProviderEvent = procedure(Sender: TSQLConnectionProvider) of object;

  TSQLConnectionProvider = class(TComponent)
  strict private
    FSessionVariables: TNamedVariants;
    FSQLSessionVariables: String;

{$IFNDEF PACKAGE}
    FLoadedSessionVariables: Boolean;
{$ENDIF}
  private
    { Private declarations }
    FAfterConnect   : TSQLConnectionProviderEvent;
    FAfterDisconnect: TSQLConnectionProviderEvent;
    FOnInfoMessage  : TInternalConnectionOnInfoMessage;
{$IFDEF SERVICEBROKER_SUPPORT}
    FServiceBrokerProviders: TList<TCustomSQLServiceBrokerProvider>;
{$ENDIF}

{$IFNDEF PACKAGE}
    FDataSet: TCustomInternalQuery;
    FPublicDataSet: TUserMemoryDataSet;
{$ENDIF}
    FSQLDataSetProviders: TList<TCustomSQLDataSetProvider>;
    FPublicDataSets: TObjectList<TBasePublicDataSet>;
    FConnection: TInternalConnection;

    function EqualParams(AParams: TParams; ASource: TParams): Boolean;

    function  IsActualDataSet(ADataSet: TBasePublicDataSet; const ACommandText: String; AParams: TParams): Boolean;
    function  CreatePublicDataSet(AOwner: TComponent; const ACommandText: String; AParams: TParams; ALiteMode: Boolean): TBasePublicDataSet;
    procedure ClosePublicDataSet(AOwner: TComponent; ADataSet: TBasePublicDataSet);

{$IFNDEF PACKAGE}
    procedure ConnectionAfterConnect(Sender: TObject);
    procedure ConnectionAfterDisconnect(Sender: TObject);
    procedure GetSessionVariables;
{$ENDIF}

    procedure SetConnection(const AValue: TInternalConnection);
    procedure SetOnInfoMessage(const Value: TInternalConnectionOnInfoMessage);
    procedure SetSQLSessionVariables(const AValue: String);
  protected
{$IFNDEF PACKAGE}
    procedure Loaded; override;
{$ENDIF}

    procedure AddSQLDataSetProvider(ASQLDataSetProvider: TCustomSQLDataSetProvider);
    procedure RemoveSQLDataSetProvider(ASQLDataSetProvider: TCustomSQLDataSetProvider);

{$IFDEF SERVICEBROKER_SUPPORT}
    procedure AddSQLServiceBrokerProvider(ASQLServiceBrokerProvider: TCustomSQLServiceBrokerProvider);
    procedure RemoveSQLServiceBrokerProvider(ASQLServiceBrokerProvider: TCustomSQLServiceBrokerProvider);
    procedure SQLServiceBrokerProvidersNotifyConnectionChanged;
{$ENDIF}
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function Clone(AOwner: TComponent): TSQLConnectionProvider;

    procedure CheckConnection;
    procedure BeginTran;
    procedure CommitTran(AIfPresent: Boolean = False);
    procedure RollbackTran(AIfPresent: Boolean = False);
    function  TranCount: Integer;

    function VarTypeToDataBaseType(AVarType: TVarType): String;
    function VariantToDataBaseString(const AValue: Variant; const ABinaryStringFormat: Boolean = False): String;

{$IFNDEF PACKAGE}
    function  GetSQLQuery: TCustomInternalQuery; overload;
    function  GetSQLQuery(const ACommandText: String; const AParams: TNamedVariants): TCustomInternalQuery; overload;

    function  OpenSQLQuery(const ACommandText: String; const AParams: TParams): TCustomInternalQuery; overload;
    function  OpenSQLQuery(const ACommandText: String; const AParams: TNamedVariants): TCustomInternalQuery; overload;
    procedure CloseSQLQuery;

    function  OpenSQLPublicQuery(const ACommandText: String; AParams: TParams): TUserMemoryDataSet;
    procedure CloseSQLPublicQuery;

    function  ExecSQL(const ACommandText: String; const AParams: TNamedVariants; AUseMultipleResults: Boolean = False): TNamedVariants; overload;
    function  ExecSQL(const ACommandText: String; const AParams: Array of TNamedVariant; AUseMultipleResults: Boolean = False): TNamedVariants; overload;
    function  ExecSQL(const ACommandText: String; const AParamsNames: String; const AParamValues: Variant; AUseMultipleResults: Boolean = False): TNamedVariants; overload;
{$ENDIF}

//    function  LookUpPublicDataSet(const AQuery: String; const AParams: TNamedVariants; const KeyFiledNames: String; const KeyFieldValues: Variant; const LookupFieldNames: String): Variant;

{$IFNDEF PACKAGE}
    property DataSet: TCustomInternalQuery read FDataSet;
{$ENDIF}
//    property DataSetHelper: TBaseDataSetHelper read FDataSetHelper;
    function GetConnected: Boolean;
    procedure SetConnected(AValue: Boolean);
    function ConnectionInfo: String;

    property Connected: Boolean read GetConnected write SetConnected;
    property SessionVariables: TNamedVariants read FSessionVariables;
  published
    { Published declarations }
    property Connection: TInternalConnection read FConnection write SetConnection;

    property SQLSessionVariables: String read FSQLSessionVariables write SetSQLSessionVariables;

    property AfterConnect: TSQLConnectionProviderEvent read FAfterConnect write FAfterConnect;
    property AfterDisconnect: TSQLConnectionProviderEvent read FAfterDisconnect write FAfterDisconnect;
    property OnInfoMessage: TInternalConnectionOnInfoMessage read FOnInfoMessage write SetOnInfoMessage;
  end;

  TDataSourceAccess = class(TDataSource);

  TBaseExecuteMethod = (bemExecute, bemOpen, bemOpenNext, bemOpenFill, bemRefresh);
{
  bemExecute  = DataSet.Execute;
  bemOpen     = DataSet.Open;
  bemOpenNext = DataSet.OpenNext;
  bemOpenFill = Fill array of DataSets (In Thread). Look at TBaseDataSetThread.Fill method;
  bemRefresh  = In Memory Open and Change Active DataSet;
}

  TBaseDataSetFieldAutomations = class(TPersistent)
  private
    FUpdateCount: Word;
    FUpdated: Boolean;
    FDataSetProvider: TCustomBaseDataSetProvider;

    FModifiedFieldsFieldName: String;
//    FAllFieldsModifiedIfChanged: String;
    FModifiAbleFieldsFieldName: String;
    FServiceFields: String;
    FInsertAbleFields: String;
    FUpdateAbleFields: String;
    FInitializeAbleFields: String;

    FTrimRightFields: String;
    FTrimLeftFields : String;
    FNullIfEmptyFields: String;

    FUpdateAbleAlgorithmActive: Boolean;

    procedure SetInitializeAbleFields(const Value: String);
    procedure SetInsertAbleFields(const Value: String);
    procedure SetUpdateAbleFields(const Value: String);
    function IsInsertAbleFieldsStored: Boolean;
    function IsUpdateAbleFieldsStored: Boolean;
    procedure SetModifiAbleFieldsFieldName(const Value: String);
  protected
    function GetOwner: TPersistent; override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadAddServiceFields(Reader: TReader);
  public
    constructor Create(ADataSetProvider: TCustomBaseDataSetProvider);

    function IsUpdateAbleAlgorithmActive: Boolean;
    function AllUpdateableFields: String;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure AddServiceFields(const AFields: String);

    property UpdateAbleAlgorithmActive: Boolean read FUpdateAbleAlgorithmActive;
  published
    property ModifiedFieldsFieldName: String read FModifiedFieldsFieldName write FModifiedFieldsFieldName;
//    property AllFieldsModifiedIfChanged: String read FAllFieldsModifiedIfChanged write FAllFieldsModifiedIfChanged;
    property ServiceFields: String read FServiceFields write FServiceFields;

    property ModifiAbleFieldsFieldName: String read FModifiAbleFieldsFieldName write SetModifiAbleFieldsFieldName;
    property InsertAbleFields: String read FInsertAbleFields write SetInsertAbleFields Stored IsInsertAbleFieldsStored;
    property UpdateAbleFields: String read FUpdateAbleFields write SetUpdateAbleFields Stored IsUpdateAbleFieldsStored;
    property InitializeAbleFields: String read FInitializeAbleFields write SetInitializeAbleFields;

    property TrimRightFields  : String read FTrimRightFields    write FTrimRightFields;
    property TrimLeftFields   : String read FTrimLeftFields     write FTrimLeftFields;
    property NullIfEmptyFields: String read FNullIfEmptyFields  write FNullIfEmptyFields;
  end;

//  TDataSetFieldsAutomationsOnGet

{$WARN SYMBOL_DEPRECATED OFF}
  TCustomBaseDataSetProvider = class(TDataSet, IBaseDataSetProvider)
//  strict private
//    FLoadedActive: Boolean;
  strict private
  { Новые скрытые переменные }
    FKeyFieldNames, FIndexFieldNames: String;
    FDetailFields, FMasterFields: String;
    FMasterSource: TDataSource;
    FFieldsAutomations: TBaseDataSetFieldAutomations;
    FBeforePostState: TDataSetState;
    FReadOnly: Boolean;
    FUpdateStatusFilter: TUpdateStatusSet;
    FLevel: UInt16;

    procedure SetFieldsAutomations(const Value: TBaseDataSetFieldAutomations);
  private
    FCachedUpdates: Boolean;
  protected
    procedure DataSetBeforePost;
//    procedure DataSetAfterPost; virtual;
    procedure DataSetAfterScroll;
  protected
  { Новые защищённые переменные }
    FDataSource: TDataSource;
    FBeforeOpenTime: TTime;
    FOpenDuration: Integer;

    function GetActiveDataSet: TDataSet; inline;
    function GetBaseDataSetProviderSupport: IBaseDataSetProvider;

{$IFNDEF PACKAGE}
    function GetDataSources: TList<TDataSource>; override;
{$ENDIF}
  public
  { Унаследованные публичные методы базового класса TDataSet }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
  { Собственные защищенные методы }
    procedure SetDataSet(const AValue: TDataSet);
    procedure InternalSetDataSet(const AFrom, ATo: TDataSet); virtual;

    procedure RaiseDataSetException(const AException: String);
    procedure DataSetNotSet(const AMethod: String);
    function  CheckDataSet(const AMethod: String): TDataSetCrack;
    function  CheckBaseDataSetProviderSupport(const AMethod: String): IBaseDataSetProvider;

    procedure SetParentComponent(AParent: TComponent); override;
  protected
    function SupportedMethods: TBaseDataSetSupportedActions; dynamic;

    function GetMasterSource: TDataSource;
    procedure SetMasterSource(AValue: TDataSource);

    function GetMasterFields: String;
    procedure SetMasterFields(const AValue: String);

    function GetDetailFields: String;
    procedure SetDetailFields(const AValue: String);

    function GetIndexFieldNames: String;
    procedure SetIndexFieldNames(const AValue: String);

//    function GetKeyFieldNames: String;
    procedure SetKeyFieldNames(const AValue: String);

    function GetReadOnly: Boolean;
    procedure SetReadOnly(const AValue: Boolean);

    function GetCachedUpdates: Boolean;
    procedure SetCachedUpdates(const AValue: Boolean); virtual;

    function  GetUpdateStatusFilter: TUpdateStatusSet;
    procedure SetUpdateStatusFilter(AValue: TUpdateStatusSet);

    procedure InternalApplyUpdates;
    procedure InternalCommitUpdates;
  public
    function VariantToFilterString(const AValue: Variant): String; virtual;
    procedure RefreshDetail;

    function  UpdatesPending: Boolean;
    procedure ApplyUpdates; virtual;
    procedure CommitUpdates; virtual;
    procedure CancelUpdates;
    procedure RestoreUpdates;
    procedure RevertRecord;
  protected
    { IProviderSupport }
    function  PSGetKeyFields: String; override;
    function  IBaseDataSetProvider.GetKeyFieldNames = PSGetKeyFields;
  protected
  { Новые защищённые методы }
    procedure AttachDataSetOptions(const ADataSet: TDataSet); virtual;
    procedure DetachDataSetOptions(const ADataSet: TDataSet); virtual;
    procedure InternalSetActive(Value: Boolean); virtual;
    procedure InternalSetCachedUpdates(AValue: Boolean); virtual;
//  protected
//    procedure Loaded; override;
  protected
  { Унаследованные защищённые методы базового класса TDataSet }
    procedure SetActive(Value: Boolean); override;
    procedure SetBlockReadSize(Value: Integer); override;
    function  GetCanModify: Boolean; override;
    function  GetRecordCount: Integer; override;
    function  GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function  GetRecordSize: Word; override;
    procedure BlockReadNext; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;

    function GetActive: Boolean; {$IFNDEF PACKAGE} override; {$ENDIF}
    function GetAggFields: TFields; {$IFNDEF PACKAGE} override; {$ENDIF}
    function GetAutoCalcFields: Boolean; {$IFNDEF PACKAGE} override; {$ENDIF}
    procedure SetAutoCalcFields(const Value: Boolean); {$IFNDEF PACKAGE} override; {$ENDIF}
    function GetBlockReadSize: Integer; {$IFNDEF PACKAGE}override; {$ENDIF}
    function GetBOF: Boolean; {$IFNDEF PACKAGE}override; {$ENDIF}
    function GetEOF: Boolean; {$IFNDEF PACKAGE}override; {$ENDIF}
    function GetIsUniDirectional: Boolean; {$IFNDEF PACKAGE}override; {$ENDIF}

    function  GetFound: Boolean; {$IFNDEF PACKAGE} override; {$ENDIF}
    function  GetModified: Boolean; {$IFNDEF PACKAGE} override; {$ENDIF}

    function GetFieldDefList: TFieldDefList; {$IFNDEF PACKAGE} override; {$ENDIF}
    function ReadFieldList: TFieldList; {$IFNDEF PACKAGE} override; {$ENDIF} // GetFieldDefs
    function GetFieldDefs: TFieldDefs; {$IFNDEF PACKAGE} override; {$ENDIF}

    function GetFields: TFields; {$IFNDEF PACKAGE} override; {$ENDIF}

    function GetFiltered: Boolean; {$IFNDEF PACKAGE} override; {$ENDIF}
    function GetFilterOptions: TFilterOptions; {$IFNDEF PACKAGE} override; {$ENDIF}
    function GetFilterText: string; {$IFNDEF PACKAGE} override; {$ENDIF}
    function GetState: TDataSetState; {$IFNDEF PACKAGE} override; {$ENDIF}

    function GetObjectView: Boolean; {$IFNDEF PACKAGE} override; {$ENDIF}
    procedure SetObjectView(const Value: Boolean); {$IFNDEF PACKAGE} override; {$ENDIF}
    function GetSparseArrays: Boolean; {$IFNDEF PACKAGE} override; {$ENDIF}
    procedure SetSparseArrays(Value: Boolean); {$IFNDEF PACKAGE} override; {$ENDIF}
  protected
  { Переопределённые защищённые методы базового класса TDataSet }
{$IFNDEF PACKAGE}
    procedure AddDataSource(DataSource: TDataSource); override;
{$ENDIF}
  protected
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    function  IsCursorOpen: Boolean; override;
    procedure InternalClose; override;
    procedure InternalOpen; override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function IsFilterTextStored: Boolean;
  public
  { Переопределённые публичные методы базового класса TDataSet, необходимость реализации которых под сомнением }
//    function DefaultFields: Boolean; {$IFNDEF PACKAGE}override; {$ENDIF}
    function MoveBy(Distance: Integer): Integer; override;
    function GetCurrentRecord(Buffer: TRecordBuffer): Boolean; override;

    function GetBookmark: TBookmark; override;
//    function GetBookmarkStr: TBookmarkStr; reintroduce;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    procedure FreeBookmark(Bookmark: TBookmark); override;

{$IFDEF DELPHIXE3}
    procedure InternalGotoBookmark(Bookmark: TBookmark); override;
    procedure GetDetailDataSets(List: TList<TDataSet>); overload; override;
    procedure GetDetailLinkFields(MasterFields, DetailFields: TList<TField>); overload; override;
{$ENDIF}
{$IFNDEF NEXTGEN}
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure GetDetailDataSets(AList: TList); overload; override;
    procedure GetDetailLinkFields(AMasterFields, ADetailFields: TList); overload; override;
{$ENDIF}
  public
  { Переопределённые публичные методы базового класса TDataSet }
{$IFDEF PACKAGE}
    property Active: Boolean read GetActive write SetActive Default False;
    property AggFields: TFields read GetAggFields;
    property AutoCalcFields: Boolean read GetAutoCalcFields write SetAutoCalcFields Default True;

    property FieldDefList: TFieldDefList read GetFieldDefList;
    property FieldList: TFieldList read ReadFieldList;
    property FieldDefs: TFieldDefs read GetFieldDefs; // write SetFieldDefs;
    property BlockReadSize: Integer read GetBlockReadSize write SetBlockReadSize;
    property IsUniDirectional: Boolean read GetIsUniDirectional;
    property Found: Boolean read GetFound;
    property Filter: string read GetFilterText write SetFilterText stored IsFilterTextStored;
    property Filtered: Boolean read GetFiltered write SetFiltered default False;
    property FilterOptions: TFilterOptions read GetFilterOptions write SetFilterOptions default [];
    property State: TDataSetState read GetState;

    property Bof: Boolean read GetBOF;
    property Eof: Boolean read GetEOF;
//    property Modified: Boolean read GetModified;
    property ObjectView: Boolean read GetObjectView write SetObjectView;
    property SparseArrays: Boolean read GetSparseArrays write SetSparseArrays;
{$ENDIF}

    procedure Insert; {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};
    procedure InsertRecord(const Values: array of const); {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};
    procedure Append; {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};
    procedure AppendRecord(const Values: array of const); {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};
    procedure Edit; {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};
    procedure Delete; {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};
    procedure Post; override;
    procedure Cancel; override;

    procedure EnableControls; {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};
    procedure DisableControls; {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};
    procedure Refresh; {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};

    function  ControlsDisabled: Boolean; {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};
    procedure CheckBrowseMode; reintroduce;
    procedure ClearFields; reintroduce;
    procedure UpdateRecord; reintroduce;
    function IsEmpty: Boolean; {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};
    function IsLinkedTo(DataSource: TDataSource): Boolean; {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};
    procedure CursorPosChanged; reintroduce;
    procedure UpdateCursorPos; reintroduce;

    function FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure First; {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};
    procedure Last; {$IFNDEF PACKAGE}override{$ELSE}reintroduce{$ENDIF};
    procedure GotoBookmark(Bookmark: TBookmark); reintroduce;

    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    procedure Resync(Mode: TResyncMode); override;
    function UpdateStatus: TUpdateStatus; override;
    procedure GetFieldNames(List: TStrings); override;
    function IsSequenced: Boolean; override;
  public
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
    function LocateEx(const KeyFields: string; const KeyValues: Variant; Options: TBaseLocateOptions): Boolean; virtual;
    procedure AppendEx(const KeyFields: string; const KeyValues: Variant);

    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
  public
  { Переопределённые публичные свойства базового класса TDataSet }
    property RecordSize: Word read GetRecordSize;

//    property DataSource: TDataSource read GetDataSource;
  private
    FUpdateableFields, FInitializeableFields: String;

    procedure DataSourceDataChange(Sender: TObject; Field: TField);
    procedure DataSourceStateChange(Sender: TObject);
  public
    function Editing: Boolean;
    function KeyFieldValues: Variant;
    property OpenDuration: Integer read FOpenDuration{$IFDEF DEBUG} write FOpenDuration stored False{$ENDIF};

{$IFNDEF PACKAGE}
    function GetXML: String;
{$ENDIF}
    procedure DoDataSetFieldsUpdateAble(AForce: Boolean = False);

    property ActiveDataSet: TDataSet read GetActiveDataSet;
    property KeyFieldNames: String read PSGetKeyFields write SetKeyFieldNames;
    property IndexFieldNames: String read GetIndexFieldNames write SetIndexFieldNames;

    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;
    property MasterFields: String read GetMasterFields write SetMasterFields;
    property DetailFields: String read GetDetailFields write SetDetailFields;
    property CachedUpdates: Boolean read GetCachedUpdates write SetCachedUpdates Default True;
    property UpdateStatusFilter: TUpdateStatusSet read GetUpdateStatusFilter write SetUpdateStatusFilter Default Default_UpdateStatusFilter;

    property FieldsAutomations: TBaseDataSetFieldAutomations read FFieldsAutomations write SetFieldsAutomations;
  published
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Level: UInt16 read FLevel write FLevel default 0;
  end;

{$WARN SYMBOL_DEPRECATED ON}

  TStreamArray<T> = record
  strict private
    FItems  : TArray<T>;
    FIndex  : Integer;
    FOutOfBoundAllowed: Boolean;
  public
    constructor Create(const AItems: Array of T; AOutOfBoundAllowed: Boolean);
    function  CurrentItem: T;
    procedure Next(ARaiseIfOutbound: Boolean = True);
    function  Count: Integer;
    function  OutOfBound: Boolean;
    function  Eof: Boolean;

    property  OutOfBoundAllowed: Boolean read FOutOfBoundAllowed;
  end;
  TStreamArrayOfTDataSet = TStreamArray<TDataSet>;
  PStreamArrayOfTDataSet = ^TStreamArrayOfTDataSet;

  TCustomSQLDataSetProviderEvent = procedure(ADataSetProvider: TCustomSQLDataSetProvider) of object;

  TBaseDataSetThread = class;
  TBaseDataSetThreadProcess = class(TThread)
  private
    [weak] FOwner: TBaseDataSetThread;
    FExecuteMethod: TBaseExecuteMethod;
    FTarget: PStreamArrayOfTDataSet;

    FOnInfoMessage: TInternalConnectionOnInfoMessage;
    procedure SQLConnectionProviderInfoMessage(AConnection: TInternalConnection; const AInfoMessage: string);
  protected
    FDataSet: TCustomInternalQuery;
    function InternalQuery: TInternalQuery; inline;
    procedure Execute; override;
//    procedure CheckThreadError(Success: Boolean); override;
  public
    constructor Create(AOwner: TBaseDataSetThread; AExecuteMethod: TBaseExecuteMethod; ATarget: PStreamArrayOfTDataSet); overload;
    constructor Create(AOwner: TBaseDataSetThread; ADataSet: TInternalQuery; AExecuteMethod: TBaseExecuteMethod; ATarget: PStreamArrayOfTDataSet); overload;
    destructor Destroy; override;
  end;

  TDataSetThreadResult = (bdstrCancel, bdstrError, bdstrOpened, bdstrRefreshed, bdstrExecuted);
  TDataSetThreadOnErrorEvent = procedure(ADataSetProvider: TCustomSQLDataSetProvider; const AException: String; var ADone: Boolean) of object;
  TDataSetThreadOnPrintEvent = procedure(ADataSetProvider: TCustomSQLDataSetProvider; AMessages: TStringList) of object;
  TDataSetThreadOnValidateEvent = procedure(ADataSetProvider: TCustomSQLDataSetProvider; ADataSet: TInternalQuery; AMultiDataSetIndex: Word; var AValid: Boolean) of object;
  TDataSetThreadAfterCompleteEvent = procedure(ADataSetProvider: TCustomSQLDataSetProvider; ATerminateReason: TDataSetThreadResult) of object;

{$IFDEF PACKAGE}
  TSQLDataSetProviderTreadEvents = class(TNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  public
    function GetName: String; override;
  end;
{$ENDIF}

  TBaseDataSetThread = class(TPersistent)
  private
    [weak] FOwner: TCustomSQLDataSetProvider;

    FThreadProcess    : TBaseDataSetThreadProcess;
    FTerminating      : Boolean;

    FReservedDataSet  : TInternalQuery;
    FMultiDataSetIndex: Integer;

    FRaiseException : Boolean;

    FOnError        : TDataSetThreadOnErrorEvent;
    FOnPrint        : TDataSetThreadOnPrintEvent;
    FOnValidate     : TDataSetThreadOnValidateEvent;
    FAfterComplete  : TDataSetThreadAfterCompleteEvent;

    function GetMessages: TStringList;
    procedure InternalExecute(AExecuteMethod: TBaseExecuteMethod; ADataSetArrayPointer: PStreamArrayOfTDataSet = nil);
    procedure InternalEndExecute;
    procedure InternalEndTerminating;

//    procedure MessageThreadFinished(var AMessage: TMessage);
  protected
//    FHandle: HWND;
    FInfoMessages: TStringList;
    FErrorMessage: String;
  public
    constructor Create(AOwner: TCustomSQLDataSetProvider); reintroduce;
    destructor Destroy; override;

    procedure Execute;
    procedure Open;
    procedure OpenNext;
    procedure Fill(const ADataSets: Array of TDataSet; AKeepLastRecordSet: Boolean = True);
    procedure Refresh;
    procedure Stop;

    function Active: Boolean;
    procedure Wait;

    property LastError: String read FErrorMessage;
    property LastMessages: TStringList read GetMessages;
  published
    property OnError: TDataSetThreadOnErrorEvent read FOnError write FOnError;
    property OnPrint: TDataSetThreadOnPrintEvent read FOnPrint write FOnPrint;
    property OnValidate: TDataSetThreadOnValidateEvent read FOnValidate write FOnValidate;
    property AfterComplete: TDataSetThreadAfterCompleteEvent read FAfterComplete write FAfterComplete;

    property RaiseException: Boolean read FRaiseException write FRaiseException default True;
  end;

  TSQLDataSetProviderApplyMethod = (damDefault, damTempTable, damXML);
  TSQLDataSetProviderApplyOptions = class(TPersistent)
  strict private
    [weak] FOwner: TCustomSQLDataSetProvider;
  private
    FMethod: TSQLDataSetProviderApplyMethod;
//    FAutoPostCount: Integer;
//    FDeleteAfterDump: Boolean;
    FTempTable: String;
    FXMLTag: String;
    FStatusFieldName: String;
    FCommitSQL: TStrings;

    FInternalTempTable: String;
    FTempDumpActive: Boolean;

    procedure SetApplyMethod(const AValue: TSQLDataSetProviderApplyMethod);
    procedure SetCommitSQL(const AValue: TStrings);
    function IsXMLTagStored: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;

    function GetInterpreter: TInternalProviderInterpreterClass;

    procedure ApplyUpdates;
    procedure InternalTempBeginDump;
    procedure InternalTempEndDump;
  public
    constructor Create(AOwner: TCustomSQLDataSetProvider);
    destructor Destroy; override;

    procedure TempBeginDump;
    procedure TempPostDump(AUpdateStatusFilter: TUpdateStatusSet = Default_UpdateStatusFilter; ADeleteAfterDump: Boolean = False);
    procedure TempCommitDump;
    procedure TempEndDump;

    property InternalTempTable: String read FInternalTempTable;
    property TempDumpActive: Boolean read FTempDumpActive;
  published
    property Method: TSQLDataSetProviderApplyMethod read FMethod write SetApplyMethod default damDefault;
//    property AutoPostAfter: Integer read FAutoPostCount write FAutoPostCount default 0;
    property TempTable: String read FTempTable write FTempTable;
    property XMLTag: String read FXMLTag write FXMLTag stored IsXMLTagStored;

    property StatusFieldName: String read FStatusFieldName write FStatusFieldName;
    property CommitSQL: TStrings read FCommitSQL write SetCommitSQL;
  end;

  TDataSetProviderParam = class(TParam)
  private
    FRequired: Boolean;
  published
    property Required: Boolean read FRequired write FRequired default False;
  end;
  TDataSetProviderParamClass = class of TDataSetProviderParam;

  TDataSetProviderParams = class(TParams)
  private
    function GetItem(Index: Integer): TDataSetProviderParam;
    procedure SetItem(Index: Integer; const Value: TDataSetProviderParam);
  protected
    function GetParamClass: TParamClass; override;
  public
    function FindParam(const Value: string): TDataSetProviderParam;

    property Items[Index: Integer]: TDataSetProviderParam read GetItem write SetItem; default;
  end;

  TCustomSQLDataSetProviderOnBeginUpdate = procedure(ADataSet: TCustomSQLDataSetProvider; var ADataSourceStateBookmark: TDataSourceStateBookmark) of object;
  TCustomSQLDataSetProviderOnEndUpdate = procedure(ADataSet: TCustomSQLDataSetProvider; const ADataSourceStateBookmark: TDataSourceStateBookmark) of object;

  TCustomSQLDataSetProvider = class(TCustomBaseDataSetProvider, IBaseDataSetProvider, IBaseSQLDataSetProvider)
  private
    FSQLConnectionProvider: TSQLConnectionProvider;
    FSQLConnectionProviderChanged: Boolean;

    FSQLUpdating: Boolean;
    FSQL: TStringList;
    FParams: TDataSetProviderParams;
    FSQLRefresh: TStringList;
    FSQLInsert: TStringList;
    FSQLUpdate: TStringList;
    FSQLDelete: TStringList;
    FRefreshOptions: TBaseRefreshOptions;
    FSQLApplyOptions: TSQLDataSetProviderApplyOptions;

    FAutoGenerateUpdateScripts: Boolean;
    FReadOnly: Boolean;
    FDataSetObject: TBaseDataSetObject;

    FThread: TBaseDataSetThread;

    FMainDataSet: TDataSet;
    FBasePublicDataSet: TBasePublicDataSet;
    FOnPrepareParams: TCustomSQLDataSetProviderEvent;

    FBeforeUpdateExecute: TBaseDataSetUpdateExecuteEvent;
    FAfterUpdateExecute: TBaseDataSetUpdateExecuteEvent;

    FOnBeginUpdate: TCustomSQLDataSetProviderOnBeginUpdate;
    FOnEndUpdate  : TCustomSQLDataSetProviderOnEndUpdate;

    function GetBaseSQLDataSetProviderSupport: IBaseSQLDataSetProvider;
    function CheckBaseSQLDataSetProviderSupport(const AMethod: String): IBaseSQLDataSetProvider;

//    function GetSQL: TStringList;
    procedure SetSQL(const AValue: TStringList);

//    function GetSQLRefresh: TStringList;
    procedure SetSQLRefresh(const AValue: TStringList);

//    function GetSQLInsert: TStringList;
    procedure SetSQLInsert(const AValue: TStringList);

//    function GetSQLUpdate: TStringList;
    procedure SetSQLUpdate(const AValue: TStringList);

//    function GetSQLDelete: TStringList;
    procedure SetSQLDelete(const AValue: TStringList);

    procedure SQLChange(Sender: TObject);
    procedure SQLRefreshChange(Sender: TObject);
    procedure SQLInsertChange(Sender: TObject);
    procedure SQLUpdateChange(Sender: TObject);
    procedure SQLDeleteChange(Sender: TObject);

    procedure SetDataSetObject(const AValue: TBaseDataSetObject);
    procedure SetParams(const Value: TDataSetProviderParams);
    procedure SetThread(const Value: TBaseDataSetThread);

    function  GetThreadEvents: TNotifyEvent;
    procedure SetThreadEvents(const Value: TNotifyEvent);
    procedure SetSQLApplyOptions(const Value: TSQLDataSetProviderApplyOptions);
  protected
    function LogicalyActive: Boolean;
    procedure SetActive(Value: Boolean); override;
    procedure InternalSetActive(AValue: Boolean); override;
    procedure InternalCreatePublicDataSet;
    procedure InternalSetCachedUpdates(Value: Boolean); override;
  protected
    function  GetRefreshOptions: TBaseRefreshOptions;
    procedure SetRefreshOptions(AValue: TBaseRefreshOptions);

    function  GetAutoGenerateUpdateScripts: Boolean;
    procedure SetAutoGenerateUpdateScripts(const AValue: Boolean);

    procedure InternalSQLConnectionProviderChanged;
    procedure SQLConnectionProviderChanged;
    procedure SetSQLConnectionProvider(const Value: TSQLConnectionProvider);

    procedure Loaded; override;
  protected
  { Дополненные защищённые методы }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure AttachDataSetOptions(const ADataSet: TDataSet); override;
    procedure DetachDataSetOptions(const ADataSet: TDataSet); override;
    procedure InternalSetDataSet(const AFrom, ATo: TDataSet); override;
    function  InitDataSetParams(ADataSet: TDataSet): TDataSetInitDetails;

    procedure ActivateDefaultDataSet;
  protected
    { IProviderSupport }
    function  PSGetParams: TParams; override;
    procedure PSSetParams(AParams: TParams); override;
    procedure PSSetCommandText(const AText: String); override;
    function  PSIsSQLBased: Boolean; override;
    function  PSIsSQLSupported: Boolean; override;
    procedure PSExecute; override;
{
    procedure PSEndTransaction(Commit: Boolean); override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer = nil): Integer; override;
    procedure PSGetAttributes(List: TList); override;
    function PSGetDefaultOrder: TIndexDef; override;
    function PSGetQuoteChar: string; override;
    function PSGetTableName: string; override;
    function PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs; override;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError; override;
    function PSInTransaction: Boolean; override;
    function PSIsSQLBased: Boolean; override;
    function PSIsSQLSupported: Boolean; override;
    procedure PSReset; override;
    procedure PSStartTransaction; override;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean; override;
}
  protected
    procedure SyncronizeOutputParameters(Sender: TDataSet);
//    procedure DataSetAfterPost; override;
  public
    class function ParseSQLParams(const ASQLText: String; AParams: TParams = nil): String;
  public
    function SupportedMethods: TBaseDataSetSupportedActions; override;
  protected
    function  GetConnection: TCustomConnection;
    procedure SetConnection(AValue: TCustomConnection);
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
  public
    procedure RefreshRecord;
    procedure ApplyUpdates; override;

    procedure Execute;
    procedure OpenNext;
//    function  !asNextResultSet: Boolean;
    procedure ClearNextResultSets;
    procedure BreakExec;
    procedure UnPrepare;

    function  ReOpen(AReOpenFlag: TDataSetReOpenFlags = []): Boolean;

    procedure LoadFields(const AVariables: TNamedVariants; ALoadValueCondition: TLoadValueCondition; AExcludedFields: String = ''); overload;
    procedure LoadFields(AFields: TFields; ALoadValueCondition: TLoadValueCondition; AExcludedFields: String = ''; AOnlyIfCanModify: Boolean = False); overload;

    procedure CopyRecord(const AExcludedFields: String; AConstants: TNamedVariants); overload;
    procedure CopyRecord(const AExcludedFields: String; const AConstants: Array of TNamedVariant); overload;
    procedure CopyRecord(const AExcludedFields: String = ''); overload;

    function  IGetSQLDelete: String;
    function  IBaseSQLDataSetProvider.GetSQLDelete = IGetSQLDelete;
    procedure ISetSQLDelete(const Value: String);
    procedure IBaseSQLDataSetProvider.SetSQLDelete = ISetSQLDelete;

    function  IGetSQLInsert: String;
    function  IBaseSQLDataSetProvider.GetSQLInsert = IGetSQLInsert;
    procedure ISetSQLInsert(const Value: String);
    procedure IBaseSQLDataSetProvider.SetSQLInsert = ISetSQLInsert;

    function  IGetSQLRefresh: String;
    function  IBaseSQLDataSetProvider.GetSQLRefresh = IGetSQLRefresh;
    procedure ISetSQLRefresh(const Value: String);
    procedure IBaseSQLDataSetProvider.SetSQLRefresh = ISetSQLRefresh;

    function  IGetSQLUpdate: String;
    function  IBaseSQLDataSetProvider.GetSQLUpdate = IGetSQLUpdate;
    procedure ISetSQLUpdate(const Value: String);
    procedure IBaseSQLDataSetProvider.SetSQLUpdate = ISetSQLUpdate;
  public
    property ConnectionProvider: TSQLConnectionProvider read FSQLConnectionProvider write SetSQLConnectionProvider;
//    property DataSetHelper: TBaseDataSetHelper read FDataSetHelper;
    property Thread: TBaseDataSetThread read FThread write SetThread;
    property ThreadEvents: TNotifyEvent read GetThreadEvents write SetThreadEvents;

    property DataSetObject: TBaseDataSetObject read FDataSetObject write SetDataSetObject Default bdsoOriginal;

    property ReadOnly: Boolean read GetReadOnly write SetReadOnly Default False;
    property RefreshOptions: TBaseRefreshOptions read GetRefreshOptions write SetRefreshOptions Default [];

    property SQL: TStringList read FSQL write SetSQL;
    property Params: TDataSetProviderParams read FParams write SetParams;
//    property PreparedParams: String read FPreparedParams write FPreparedParams;

    property AutoGenerateUpdateScripts: Boolean read GetAutoGenerateUpdateScripts write SetAutoGenerateUpdateScripts Default False;
    property SQLApplyOptions: TSQLDataSetProviderApplyOptions read FSQLApplyOptions write SetSQLApplyOptions;
    property SQLInsert: TStringList read FSQLInsert write SetSQLInsert;
    property SQLUpdate: TStringList read FSQLUpdate write SetSQLUpdate;
    property SQLDelete: TStringList read FSQLDelete write SetSQLDelete;
    property SQLRefresh: TStringList read FSQLRefresh write SetSQLRefresh;

    property OnPrepareParams: TCustomSQLDataSetProviderEvent read FOnPrepareParams write FOnPrepareParams;

    property BeforeUpdateExecute: TBaseDataSetUpdateExecuteEvent read FBeforeUpdateExecute write FBeforeUpdateExecute;
    property AfterUpdateExecute: TBaseDataSetUpdateExecuteEvent read FAfterUpdateExecute write FAfterUpdateExecute;

    property OnBeginUpdate: TCustomSQLDataSetProviderOnBeginUpdate read FOnBeginUpdate write FOnBeginUpdate;
    property OnEndUpdate: TCustomSQLDataSetProviderOnEndUpdate read FOnEndUpdate write FOnEndUpdate;
  end;

  TSQLDataSetProvider = class(TCustomSQLDataSetProvider)
  published
    property Active;
{$IFDEF DEBUG}
    property OpenDuration;
{$ENDIF}

    property AutoCalcFields;
    property Params;
//    property Fields;
    property Filter;
    property FilterOptions;
    property Filtered;

    property ConnectionProvider;
    property AutoGenerateUpdateScripts;
    property CachedUpdates;
    property UpdateStatusFilter;
    property ReadOnly;
    property DataSetObject;

    property KeyFieldNames;
    property MasterSource;
    property MasterFields;
    property DetailFields;
    property IndexFieldNames;
    property FieldsAutomations;
    property RefreshOptions;

    property SQLApplyOptions;
    property SQL;
    property SQLInsert;
    property SQLUpdate;
    property SQLDelete;
    property SQLRefresh;

    property Thread;
  published
    property BeforeOpen;
    property AfterOpen;

    property BeforeClose;
    property AfterClose;

    property BeforeInsert;
    property AfterInsert;

    property BeforeEdit;
    property AfterEdit;

    property BeforePost;
    property AfterPost;

    property BeforeCancel;
    property AfterCancel;

    property BeforeDelete;
    property AfterDelete;

    property BeforeScroll;
    property AfterScroll;

    property BeforeRefresh;
    property AfterRefresh;

    property OnNewRecord;
    property OnEditError;
    property OnPostError;
    property OnDeleteError;

    property OnCalcFields;
    property OnFilterRecord;

    property OnPrepareParams;

    property ThreadEvents;

    property BeforeUpdateExecute;
    property AfterUpdateExecute;

    property OnBeginUpdate;
    property OnEndUpdate;
  end;

var
  BaseSQLConnectionProvider: TSQLConnectionProvider;

type
  TStorageBaseSQLConnectionProvider = class(TSQLConnectionProvider)
  private
    FIndepended: Boolean;
    FOwners: TList<TComponent>;
  public
    destructor Destroy; override;
    property Independed: Boolean read FIndepended write FIndepended;

    function  IsLinkedFrom(AOwner: TComponent): Boolean;
    function  LinkCount(ANotEqualToOwner: TComponent = nil): Integer;
    procedure CreateLink(AOwner: TComponent);
    procedure ReleaseLink(AOwner: TComponent);
  end;

  TSQLConnectionProviderStorage = class(TComponent)
  private
    FCommandText: String;
    FBaseDataModules: Array of TStorageBaseSQLConnectionProvider;
  protected
    { Отслеживание удаляющихся владельцев }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TSQLConnectionProvider); reintroduce;

{$IFNDEF PACKAGE}
    procedure GetConnection(AOwner: TComponent; AParams: TNamedVariants; AIndepended: Boolean; ASQLDataSetProvider: TSQLDataSetProvider);
    function  AssignConnection(AOwner: TComponent; AParams: TNamedVariants; AIndepended: Boolean; AUsedConnectionProvider: TSQLConnectionProvider = nil): TSQLConnectionProvider;

    procedure ResetConnection(AOwner: TComponent; ASQLDataSetProvider: TSQLDataSetProvider);
    procedure ReleaseConnection(AOwner: TComponent; AStorageDataModule: TSQLConnectionProvider);
{$ENDIF}
  published
    { SQL текст процедуры, отвечающей за распределение нагрузки }
    property CommandText: String read FCommandText write FCommandText;
  end;

var
  BaseSQLConnectionProviderStorage: TSQLConnectionProviderStorage = nil;

type
{$IFDEF FASTSCRIPT_RTTI}
  TfsSQLConnectionProviderInfoMessageEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(AConnection: TCustomConnection; const AInfoMessage: String);
    function GetMethod: Pointer; override;
  end;

  TFunctions_TBaseDataModule = class(TfsRTTIModule)
  private
    function Call_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Call_TBaseDataModule_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Call_TBaseDataModuleStorage_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Call_TSQLDataSetProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function Call_TSQLDataSetProviderApplyOptions_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function Get_TMasterDetailDataSetControl_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function Get_TMasterDetailDataSetControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

{$ENDIF}

(*
{$IFNDEF PACKAGE}
  TDataSetExpression = class(TCustomDataSetExpression)
  private
//    FDataSet: TDataSet;
    FOldValues: Boolean;
  protected
    function DoGetFieldValue(FieldNo: Word; const FieldName: string; var FldType, FldLen: Integer): Variant; override;
  public
    constructor Create(ADataSet: TDataSet; const AExpression: String); reintroduce;
    function Evaluate(AOldValues: Boolean = False): Variant; overload; virtual;
  end;
{$ENDIF}
*)

  TFieldReadOnly = class(TComponent)
  private
    FReadOnlyOrigin: Boolean;
  public
    class function ComponentName: String;
    constructor Create(AOwner: TField); reintroduce;

    class procedure DisableEdit(AField: TField);
    class procedure RestoreEdit(AField: TField);
  end;

  TMasterDetailFilterMethod = (mdfmFilter, mdfmParams, mdfmParamsEx);
  TMasterDetailFilterHelper = (mdfhClearIfNotInList, mdfhSetWhenOneRecord, mdfhSetIfDefaultLocated);
  TMasterDetailFilterHelpers = set of TMasterDetailFilterHelper;

  TMasterDetailDataSetControl = class(TComponent)
  private
    FActive: Boolean;
    FEnabled: Boolean;
    FEnabledCondition: String;

    FDetailSourceLink : TDataSourceDataLink;

    FDetailFieldsList : TStringList;
    FMethod           : TMasterDetailFilterMethod;
{$IFNDEF PACKAGE}
    FDetailFilter     : String;
{$ENDIF}
    FDetailFilterFormat : String;

    FCondition: String;

    FDataHelpers: TMasterDetailFilterHelpers;
    FDataFieldsDataLink: TMDFieldDataLink;

//    FDataFieldsList: TStringList;
    FListField: String;

    FMasterFieldsDataLink: TMDFieldDataLink;
    FConditionChanged: Boolean;

    FAfterActivate: TNotifyEvent;
    FAfterDeactivate: TNotifyEvent;

    FOnMasterDataChanged: TNotifyEvent;
    FOnMasterFieldsChanged: TNotifyEvent;

    function  GetMasterSource: TDataSource;
    procedure SetMasterSource(const Value: TDataSource);

    function  GetMasterFields: String;
    procedure SetMasterFields(const Value: String);

    procedure SetDetailFields(const Value: String);
    function  GetDetailFields: String;

    function  GetDetailSource: TDataSource;
    procedure SetDetailSource(const Value: TDataSource);

    procedure SetMethod(const Value: TMasterDetailFilterMethod);
    procedure SetCondition(const Value: String);

{$IFNDEF PACKAGE}
    procedure DataChanged(Sender: TObject);
    procedure MasterFieldsChanged(Sender: TObject);

    procedure DetailDataSetChanged(Sender: TObject);
    procedure DoAfterActivate;
    procedure DoAfterDeActivate;
{$ENDIF}

    function  GetMasterDataSet: TDataSet;
    function  GetDetailDataSet: TCustomSQLDataSetProvider;

    function  GetDataSet: TDataSet;
    function  GetDataField: String;
    procedure SetDataField(const Value: String);

{$IFNDEF PACKAGE}
    procedure ActivateDetailDataSet;
    procedure DeactivateDetailDataSet;
    procedure SetDetailFieldValues;
    procedure ClearFields;
{$ENDIF}
    procedure SetEnabled(const Value: Boolean);
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);

//  protected
//    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ConditionChanged(AForce: Boolean = False);

    property MasterDataSet: TDataSet read GetMasterDataSet;
    property DetailDataSet: TCustomSQLDataSetProvider read GetDetailDataSet;
    property DataSet: TDataSet read GetDataSet;
  published
    { Условия срабатывания Open/Close }
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property EnabledCondition: String read FEnabledCondition write FEnabledCondition;

    property Method: TMasterDetailFilterMethod read FMethod write SetMethod default mdfmFilter;
    property DetailFilterFormat: String read FDetailFilterFormat write FDetailFilterFormat;

    { Условие активности(открытия) }
    property Condition: String read FCondition write SetCondition;

    { Датасорс и поле условия набора DetailSource }
    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;
    property MasterFields: String read GetMasterFields write SetMasterFields;

    { Датасорс и параметры листа }
    property DetailSource: TDataSource read GetDetailSource write SetDetailSource;
    property DetailFields: String read GetDetailFields write SetDetailFields;

    { Помощник (Действия по-умолчанию) }
    property DataHelpers: TMasterDetailFilterHelpers read FDataHelpers write FDataHelpers;
    { Датасорс и поле для установки при срабатывании DataHelpers  }
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataField: String read GetDataField write SetDataField;
    { Поле значения(й) которое(ые) будет(ут) установлено(ы) при срабатывании DataHelpers }
    property ListField: String read FListField write FListField;

    property AfterActivate: TNotifyEvent read FAfterActivate write FAfterActivate;
    property AfterDeactivate: TNotifyEvent read FAfterDeactivate write FAfterDeactivate;

    property OnMasterDataChanged: TNotifyEvent read FOnMasterDataChanged write FOnMasterDataChanged;
    property OnMasterFieldsChanged: TNotifyEvent read FOnMasterFieldsChanged write FOnMasterFieldsChanged;
  end;

  TDateTimeOffsetFieldControl = class(TComponent)
  private
    FFieldDataLink: TMDFieldDataLink;
    FDataFieldDateTimeOffset: String;

    function GetDataFieldDateTime: String;
    function GetDataFieldOffset: String;
    function GetDataSource: TDataSource;
    procedure SetDataFieldDateTime(const Value: String);
    procedure SetDataFieldOffset(const Value: String);
    procedure SetDataSource(const Value: TDataSource);
    procedure FieldDataLinkOnDataChanged(Sender: TObject);
  public
    constructor Create(AOnwer: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataFieldDateTime: String read GetDataFieldDateTime write SetDataFieldDateTime;
    property DataFieldOffset: String read GetDataFieldOffset write SetDataFieldOffset;
    property DataFieldDateTimeOffset: String read FDataFieldDateTimeOffset write FDataFieldDateTimeOffset;
  end;

  TDataSourceHelper = class Helper for TDataSource
  public
    function GetInternalCanModify(ANilValue: Boolean = False): Boolean;
    function GetCanModify: Boolean;
  end;

  TAllFields = set of (afReadOnly, afData);
    // afReadOnly - Только для чтения
    // afData - Только Дата филды

  TDataSetHelper = class Helper for TDataSet
  private
    function GetGridBookmark: TDataSourceBookmark;
    function GetFieldOldValues(const FieldNames: String): Variant;
  protected
    procedure SetGridBookmark(const AValue: TDataSourceBookmark; ARestores: TDataSourceBookmarkRestores); overload;
    procedure SetGridBookmark(const AValue: TDataSourceBookmark); overload;
  public
    function Inserted: Boolean;
    function Modified: Boolean;
    function Editing: Boolean;

    function Source: TDataSet;
    procedure EmptyTable;

    function CloneDataSet: TDataSet;

    procedure Load(ADataSet: TDataSet; ARecordCount: Integer = -1; const AExcludedFields: String = '');

    function NonEmptyFieldsExits(const AFieldNames: String): Boolean;

    property GridBookmark: TDataSourceBookmark read GetGridBookmark write SetGridBookmark;
    property FieldOldValues[const FieldNames: String]: Variant read GetFieldOldValues;

{$IFNDEF PACKAGE}
    function Evaluate(const AExpression: string; AOldValues: Boolean = False; AConditionParser: PExprParser = nil): Variant;
    function EvaluateBoolean(const AExpression: string; AOldValues: Boolean = False; ADefaultValue: Boolean = False): Boolean;
{$ENDIF}

    procedure BeginUpdate(var AStateBookmark : TDataSourceStateBookmark);
    procedure EndUpdate(const AStateBookmark : TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores = DataSetStateBookmarkRestores_All; ADataSourceRestore: TDataSourceBookmarkRestores = DataSourceBookmarkRestores_All);
  end;

{$IFNDEF PACKAGE}
  TMDFieldDataLinkHelper = class Helper for TMDFieldDataLink
    function Evaluate(const AExpression: string; AConditionParser: PExprParser = nil): Variant;
    function EvaluateBoolean(const AExpression: string; ADefaultValue: Boolean = False): Boolean;
  end;
{$ENDIF}


  TParamsHelper = class Helper for TParams
    procedure Load(AVariables: TNamedVariants; ALoadValueCondition: TLoadValueCondition = lvcIfReceive; const AExcludedParams: String = ''); overload;
    function GetValue(const AParamName: String; const AFound: PBoolean = nil): Variant;
    function GetValues(const AParamNames: String; const AUnknownParams: PString = nil): Variant;
    procedure SetValues(const AParamNames: string; const AValues: Variant);
  end;

  TFieldsEditMode = (femAutoEdit, femAllOrRaise);
  TFieldsEditModes = set of TFieldsEditMode;
  TFieldsHelper = class Helper for TFields
  private
    function GetFieldValue(const FieldName: string): Variant;
    procedure SetFieldValue(const FieldName: string; const Value: Variant);
  public
    procedure CreateField(const AName, AType: String; ALength: Word = 0; AScale: Byte = 0);

    procedure Load(const AVariables: TNamedVariants; ALoadValueCondition: TLoadValueCondition; const AExcludedFields: String = ''); overload;
    procedure Load(AFields: TFields; ALoadValueCondition: TLoadValueCondition; const AExcludedFields: String = ''; AOnlyIfCanModify: Boolean = False); overload;

    function  FindFieldValue(AFieldName: String): Variant;

    procedure Save(AVariables: TNamedVariants; ASaveValueCondition: TLoadValueCondition; const AExcludedFields: String = ''); overload;

    function GetValue(const AFieldName: String; const AFound: PBoolean = nil): Variant;
    function GetValues(const AFieldNames: String; const AUnknownFields: PString = nil): Variant;
    procedure SetValues(const AFieldNames: String; const AValue: Variant);

    function ClearFields(const AFields: String; AEditModes: TFieldsEditModes): Boolean;
    function SetFields(const AFields: String; const AValues: Variant; AEditModes: TFieldsEditModes): Boolean;

    function  All(AFilter: TAllFields = [afData]): String;

    procedure SetAllEditAble;
    function  IsAllEditAble: Boolean;

    property Values[const FieldName: string]: Variant read GetFieldValue write SetFieldValue;
  end;

  TFieldHelper = class Helper for TField
    function Clone(AOwner: TComponent = nil): TField;
    function IsOrdinal: Boolean;
    function IsNullOrWhiteSpace: Boolean;
  end;

  TParamHelper = class Helper for TParam
  private
    function GetAsVariant: Variant;
    procedure SetAsVariant(const AValue: Variant);
  published
    property AsVariant: Variant read GetAsVariant write SetAsVariant;
  end;

{$IFNDEF PACKAGE}
  TMDFieldDataLinkExprEvaluator = class(TCanExprEvaluator)
  private
    FMDFieldDataLink: TMDFieldDataLink;
  protected
    function DoGetFieldValue(FieldNo: Word; const FieldName: string; var FldType, FldLen: Integer): Variant; override;
  public
    constructor Create(AFieldDataLink: TMDFieldDataLink);
  end;
  PMDFieldDataLinkExprEvaluator = ^TMDFieldDataLinkExprEvaluator;
{$ENDIF}

//  function BooleanFieldAsInteger(AField: TBooleanField): ShortInt;
//  function BooleanFieldAsStringInteger(AField: TBooleanField): String;
  function GetFieldsCanModify(const ADataSet: TDataSet; const ANames: String): Boolean;

//  function DataSetForeignIndex(ADataSet: TDataSet; AField: TField; AKey: Variant; AForInsertOnly: Boolean = False): Boolean;
  function DataSetFieldQuoteName(const AName: String): String; //Inline;

  function SetDataSetFieldsReadOnly(ADataSet: TDataSet; AWriteableFields: string; AWriteableIfEmptyFields: string = ''): Boolean;

{$IFNDEF PACKAGE}
  procedure ReplaceDataSet(const ADataSet, ADataSetSource: TDataSet);
//  function FieldsMetaDataToXML(AFields: TFields; AExcludedFields: String = ''): String;

  // фомирование XML представления строки из датасета на которой стоит курсор
  //function DataSetFieldsToXML(AFields: TFields; AExcludedFields: String = ''): String; // AExcludedFields = with ',' as delimeter

  // фомирование XML представления датасета
  //   формирует XML со строки на которой стоит курсор в датасете
  //
  // параметры, созданные для избежания возникновения ошибки "Out of memory"
  // при конвертации "больших" датасетов в XML
  //   AMaxRowCount  - макс. кол-во строк из датасета преобразуемых за один вызов в XML
  //   AMaxBlockSize - макс. непрерывный размер памяти, выделяемый для формирования строки XML
  function DataSetToXML(ADataSet: TDataSet; ATag: String = 'row'; AKeyFieldNames: String = ''; AStatusFieldName: String = ''; AExcludedFields: String = ''; AMaxRowCount: Integer = -1): String;

  function ParseSQLParams(const ASQL: String): String;
{$ENDIF}

{$IFDEF PACKAGE}
procedure Register;
{$ENDIF}

implementation

uses
  SBaseStringFunctions, SBaseVariantFunctions
{$IFNDEF PACKAGE}
{$IFDEF DATAHANDLER}
  , DataHandler
{$ENDIF}
{$IFNDEF SBASEFORM_DISABLED}
  , SBaseForm
{$ENDIF}
{$IFDEF CLDS_SUPPORT}
  , SBaseDataHelperCDS
{$ENDIF}
{$ENDIF}
{$IFDEF FASTSCRIPT_RTTI}
  , fsFunction_TDataSet, fsFunction_Other
{$ENDIF}
  , DAConsts
;

const
  SConst_MainDataSet    : String = 'MainDataSet';
  SConst_ReservedDataSet: String = 'ReservedDataSet';

{$IFDEF SERVICEBROKER_SUPPORT}
type
  TCustomSQLServiceBrokerProviderCrack = class(TCustomSQLServiceBrokerProvider);
{$ENDIF}

{$IFDEF PACKAGE}
procedure Register;
begin
  RegisterComponents('MD Project Tools', [TInternalConnection, TInternalConnectForm, TSQLConnectionProvider]);

  RegisterComponents('MD Project Tools', [TUserMemoryDataSet, TSQLDataSetProvider]);
  RegisterComponents('MD Project Tools', [TMasterDetailDataSetControl, TDateTimeOffsetFieldControl]);

  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TSQLDataSetProvider, 'ThreadEvents', TSQLDataSetProviderTreadEvents);
end;
{$ENDIF}

{ Data Aware Functions }

{$IFNDEF PACKAGE}
procedure ReplaceDataSet(const ADataSet, ADataSetSource: TDataSet);
var
  LOwner                : TComponent;
  LDAddress, LFieldAddress  : Pointer;

//  LDataSourceCount      : Integer;
//  LDataSources          : Array of TReplaceDataSetDataSource;

//  LFieldCount           : Integer;
//  LFields               : Array of TReplaceDataSetField;

  LEnabledDataSourceCount : Integer;
  LEnabledDataSources     : Array of TDataSource;

  I: Integer;

  LField: TField;
  LDataSource: TDataSource;
begin
  LOwner := ADataSet.Owner;
  LDAddress := LOwner.FieldAddress(ADataSet.Name);

{$IFDEF DEBUG}
  Assert(Assigned(LDAddress));
{$ENDIF}

  ADataSetSource.DisableControls;

  try
//    LFieldCount := 0;
//    SetLength(LFields, ADataSet.Fields.Count);

    for I := 0 to Pred(ADataSet.Fields.Count) do
      with ADataSet.Fields[I] do
        if (Name <> '') and (FieldName <> '') then begin
          LFieldAddress := LOwner.FieldAddress(Name);
          if Assigned(LFieldAddress) then begin
            LField := ADataSetSource.FieldByName(Name);
            TField(LFieldAddress^) := LField;
          end;

//          if (LFAddress <> nil) then begin
//            LFields[LFieldCount].Address   := LFAddress;
//            LFields[LFieldCount].FieldName := FieldName;
//            Inc(LFieldCount);
//          end;
        end;

//    LDataSourceCount := ;
//    SetLength(LDataSources, LDataSourceCount);
//    for I := 0 to Pred(LDataSourceCount) do
//      with LDataSources[I] do begin
//        DataSource := ADataSet.DataSources[I];
//        Enabled := DataSource.Enabled;
//      end;

    LEnabledDataSourceCount := 0;
    for I := Pred(ADataSet.DataSources.Count) downto 0 do begin
      LDataSource := ADataSet.DataSources[I];
      if (LDataSource.Owner <> ADataSet) then begin
        if LDataSource.Enabled then begin
          Inc(LEnabledDataSourceCount);
          SetLength(LEnabledDataSources, LEnabledDataSourceCount);
          LEnabledDataSources[Pred(LEnabledDataSourceCount)] := LDataSource;
          LDataSource.Enabled := False;
        end;
        LDataSource.DataSet := ADataSetSource;
      end;
    end;

    ADataSet.Free;
    TObject(LDAddress^) := ADataSetSource;

//    for I := 0 to Pred(LFieldCount) do begin
//      LField := ADataSetSource.FindField(LFields[I].FieldName);
//      if Assigned(LField) then
//        TObject(LFields[I].Address^) := LField;
//    end;

//    for I := 0 to Pred(LDataSourceCount) do begin
//      LDataSource := LDataSources[I].DataSource;
//      if LDataSource.Enabled then
//        LDataSource.Enabled := False;
//      LDataSource.DataSet := ADataSetSource;
//    end;

//    for I := 0 to Pred(LDataSourceCount) do
//      if LDataSources[I].Enabled then
//        LDataSources[I].DataSource.Enabled := True;

    for I := 0 to Pred(LEnabledDataSourceCount) do
      LEnabledDataSources[I].Enabled := True;
  finally
    ADataSetSource.EnableControls;
  end;
end;

function DataSetFieldsToXML(AFields: TFields; AExcludedFields: String = ''): String; // AExcludedFields = with ',' as delimeter
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AFields.Count - 1 do
    with AFields[I] do
      if (not IsNull) and (not InArray(AExcludedFields, FieldName, ',')) { and (Pos(':', FieldName) = -1)} then begin
        Result := Result + ' ' + FieldName + '="' + SBaseVariantFunctions.XMLEncode(AsVariant) + '"';
      end;
end;

function DataSetToXML(ADataSet: TDataSet; ATag: String; AKeyFieldNames: String; AStatusFieldName: String; AExcludedFields: String; AMaxRowCount: Integer): String;
var
  LFirstRowNo: Integer;
  LRowXML: String;
  LRowXMLLenght, LXMLLength, LXMLActualLength: Integer;
begin
  LXMLLength := 0;
  SetLength(Result, 0);
  LXMLActualLength := 0;

  LFirstRowNo := ADataSet.RecNo;
  ADataSet.DisableControls;
  try
    while not (ADataSet.Eof) and ((AMaxRowCount < 0) or (ADataSet.RecNo - LFirstRowNo < AMaxRowCount)) do begin
      LRowXML := '<' + ATag;
      if (not AStatusFieldName.IsEmpty) and (ADataSet.UpdateStatus <> usUnmodified) then begin
        LRowXML := LRowXML + ' ' + AStatusFieldName + '="';
        case ADataSet.UpdateStatus of
          usInserted: LRowXML := LRowXML + 'I';
          usModified: LRowXML := LRowXML + 'U';
          usDeleted : LRowXML := LRowXML + 'D';
        end;
        LRowXML := LRowXML + '"';
      end;

      if (ADataSet.UpdateStatus = usDeleted) and (not AKeyFieldNames.IsEmpty) and (not AStatusFieldName.IsEmpty){?} then
        LRowXML := LRowXML + ' ' + SBaseVariantFunctions.XMLEncode(AKeyFieldNames, ADataSet.FieldValues[AKeyFieldNames])
      else
        LRowXML := LRowXML + DataSetFieldsToXML(ADataSet.Fields, AExcludedFields);

      LRowXML := LRowXML + '/>' + SConst_CR;
      LRowXMLLenght := System.Length(LRowXML);

      if LRowXMLLenght > 0 then begin
        if LXMLLength - LXMLActualLength < LRowXMLLenght then begin
//          if (LXMLLength + LRowXMLLenght) > AMaxBlockSize then
//            LXMLLength := (AMaxBlockSize - LRowXMLLenght) shl 1;
          LXMLLength := LXMLActualLength + LRowXMLLenght + LXMLLength shr 1;
          SetLength(Result, LXMLLength);
        end;

        Move(LRowXML[1], Result[LXMLActualLength + 1], LRowXMLLenght * SizeOf(Char));
        Inc(LXMLActualLength, LRowXMLLenght);
      end;
      ADataSet.Next;
    end;
  finally
    ADataSet.EnableControls;
  end;

  SetLength(Result, LXMLActualLength);
end;

function ParseSQLParams(const ASQL: String): String;
begin
  Result := TCustomSQLDataSetProvider.ParseSQLParams(ASQL);
end;

{$ENDIF}

{$IFNDEF PACKAGE}
{ TMDFieldDataLinkExprEvaluator }

constructor TMDFieldDataLinkExprEvaluator.Create(AFieldDataLink: TMDFieldDataLink);
begin
  FMDFieldDataLink := AFieldDataLink
end;

function TMDFieldDataLinkExprEvaluator.DoGetFieldValue(FieldNo: Word; const FieldName: string; var FldType, FldLen: Integer): Variant;
begin
  Result := FMDFieldDataLink.FieldByName(FieldName).AsVariant;
end;
{$ENDIF}

{ TDataSourceBookmark }

procedure TDataSourceBookmark.Clear;
begin
  KeyFieldValues  := unAssigned;
  RecordIndex     := -1;
  RowIndex        := -1;
  Reserved        := unAssigned;
end;

constructor TDataSourceBookmark.Create(AClear: Boolean);
begin
  if AClear then
    Clear;
end;

function TDataSourceBookmark.GetAsVariant: Variant;
begin
  if RecordIndex = -1 then
    Result := unAssigned
  else
    Result := VarArrayOf([KeyFieldValues, RecordIndex, RowIndex, Reserved])
end;

procedure TDataSourceBookmark.SetAsVariant(const Value: Variant);
begin
  if VarIsEmpty(Value) then
    RecordIndex := -1
  else begin
    KeyFieldValues  := Value[0];
    RecordIndex     := Value[1];
    RowIndex        := Value[2];
    Reserved        := Value[3];
  end;
end;

{ TDataSourceStateBookmark }

procedure TDataSourceStateBookmark.Clear;
begin
  DataSource.Clear;
//  DataSet.Clear;
end;

function TDataSourceStateBookmark.GetAsVariant: Variant;
begin
  Result := VarArrayOf([DataSetState.AsVariant, DataSource.AsVariant])
end;

procedure TDataSourceStateBookmark.SetAsVariant(const Value: Variant);
begin
  if not VarIsArray(Value) then begin
    DataSetState.Clear;
    DataSource.Clear;
  end else begin
    DataSetState.AsVariant := Value[0];
    DataSource.AsVariant   := Value[1];
    DataSetState.NotNull   := True;
  end;
end;


{ TDataSourceHelper }

function TDataSourceHelper.GetCanModify: Boolean;
begin
  if (Self = nil) or (DataSet = nil) or (not DataSet.Active) then
    Exit(False);

{$IFNDEF PACKAGE}
{$IFDEF DATAHANDLER}
  if Self is TCustomDataHandler then
    Result := TCustomDataHandler(Self).CanModify
  else
{$ENDIF}
{$ENDIF}
    Result := DataSet.CanModify
end;

function TDataSourceHelper.GetInternalCanModify(ANilValue: Boolean): Boolean;
begin
  if (Self = nil) then
    Exit(ANilValue)
  else if (DataSet = nil) then
    Exit(False);

{$IFNDEF PACKAGE}
{$IFDEF DATAHANDLER}
  if Self is TCustomDataHandler then
    Result := TCustomDataHandler(Self).InternalCanModify
  else
{$ENDIF}
{$ENDIF}
    Result := True;
end;

{ TDataSetHelper }

function TDataSetHelper.GetGridBookmark: TDataSourceBookmark;
var
  LKeyFields: String;
begin
  if (Self = nil) or IsEmpty then
    Result.RecordIndex := -1
  else begin
    if Self is TSQLDataSetProvider then
      LKeyFields := TSQLDataSetProvider(Self).KeyFieldNames
    else
      LKeyFields := '';

    if LKeyFields.IsEmpty then
      Result.KeyFieldValues := unAssigned
    else
      Result.KeyFieldValues := FieldValues[LKeyFields];

    Result.RecordIndex := RecNo;
    Result.Reserved    := Unassigned;
  end;
end;

procedure TDataSetHelper.SetGridBookmark(const AValue: TDataSourceBookmark; ARestores: TDataSourceBookmarkRestores);
var
  LKeyFields: String;
begin
  if (AValue.RecordIndex <= 0) or (IsEmpty) or (ARestores = []) then Exit;

  DisableControls;
  try
    if Self is TSQLDataSetProvider then
      LKeyFields := TSQLDataSetProvider(Self).KeyFieldNames
    else
      LKeyFields := '';

    if (not (sbrKeyFieldValues in ARestores)) or (LKeyFields.IsEmpty) or not VarIsPresent(AValue.KeyFieldValues) or not Locate(LKeyFields, AValue.KeyFieldValues, [loCaseInsensitive]) then
      if sbrRecordIndex in ARestores then
        if (AValue.RecordIndex > RecordCount) then
          Last
        else
          RecNo := AValue.RecordIndex;
  finally
    EnableControls
  end;
end;

procedure TDataSetHelper.SetGridBookmark(const AValue: TDataSourceBookmark);
begin
  SetGridBookmark(AValue, DataSourceBookmarkRestores_All);
end;

function TDataSetHelper.GetFieldOldValues(const FieldNames: String): Variant;
var
  LItem : TStringItem;
  LValue: Variant;
begin
  Result := unAssigned;

  for LItem in SBaseStringFunctions.EnumStringItems(FieldNames) do begin
    LValue := FieldByName(LItem.Value).OldValue;
    if (LItem.Index = 0) and LItem.Eof then
      Result  := LValue
    else begin
      if LItem.Index = 0 then
        Result := VarArrayOf([LValue])
      else begin
        VarArrayRedim(Result, LItem.Index);
        Result[LItem.Index] := LValue;
      end;
    end;
  end;
end;

function TDataSetHelper.Inserted: Boolean;
begin
  Result := (State = dsInsert) or (UpdateStatus = usInserted)
end;

function TDataSetHelper.Modified: Boolean;
begin
  Result := (State <> dsBrowse) or (UpdateStatus <> usUnmodified)
end;

function TDataSetHelper.NonEmptyFieldsExits(const AFieldNames: String): Boolean;
var
  LDataSourceStateBookmark: TDataSourceStateBookmark;
begin
  BeginUpdate(LDataSourceStateBookmark);
  try
    Filtered := False;
    First;
    while not Eof do begin
      if VarArrayIsPresent(FieldValues[AFieldNames]) then Exit(True);
      Next;
    end;
  finally
    EndUpdate(LDataSourceStateBookmark, [sbrFiltered, sbrDisableCount]);
  end;
  Result := False;
end;

procedure TDataSetHelper.Load(ADataSet: TDataSet; ARecordCount: Integer = -1; const AExcludedFields: String = '');
var
  LRecordCount: Integer;
begin
  if ADataSet.IsEmpty and Self.Active then
    Exit;

  LRecordCount := 0;
  ADataSet.DisableControls;
  try
    DisableControls;
    Fields.SetAllEditAble;
    try
      if (Active = False) then begin
        if (Fields.Count <= 0) then
        // Надо скопировать поля из источника
          FieldDefs.Assign(ADataSet.FieldDefs);
        Active := True;
      end;

      if ARecordCount < 0 then
        ADataSet.First;

      while ((ARecordCount < 0) or (LRecordCount < ARecordCount)) and (not ADataSet.Eof) do begin
        Append;
          Fields.Load(ADataSet.Fields, lvcIfReceive, AExcludedFields);
        Post;
        ADataSet.Next;
        Inc(LRecordCount);
      end;
    finally
      EnableControls;
    end;
  finally
    ADataSet.EnableControls;
  end;
end;


function TDataSetHelper.Source: TDataSet;
begin
  if Self = nil then
    Result := nil
  else if Self is TInternalMemoryDataSet then
    Result := TInternalMemoryDataSet(Self).Provider
  else if Self is TInternalQuery then
    Result := TInternalQuery(Self).Provider
  else
    Result := Self
end;

procedure TDataSetHelper.BeginUpdate(var AStateBookmark: TDataSourceStateBookmark);
begin
  if Self is TSQLDataSetProvider then
    AStateBookmark.DataSetState.UpdateStatusFilter := TSQLDataSetProvider(Self).UpdateStatusFilter
  else
    AStateBookmark.DataSetState.UpdateStatusFilter := Default_UpdateStatusFilter;

  AStateBookmark.DataSetState.Filtered  := Filtered;
  AStateBookmark.DataSetState.Filter    := Filter;
  AStateBookmark.DataSource             := GridBookmark;

//  AStateBookmark.DataSetState.ControlsDisabled   := True;
  AStateBookmark.DataSetState.NotNull   := True;
  DisableControls;
end;

procedure TDataSetHelper.EndUpdate(const AStateBookmark : TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores);
begin
  if AStateBookmark.DataSetState.NotNull then begin
    if  (sbrUpdateStatusFilter in ADataSetStateRestore) and (Self is TCustomBaseDataSetProvider)
    then
      TCustomBaseDataSetProvider(Self).UpdateStatusFilter := AStateBookmark.DataSetState.UpdateStatusFilter;

    if (sbrFilter in ADataSetStateRestore) then
      Filter := AStateBookmark.DataSetState.Filter;

    if (sbrFiltered in ADataSetStateRestore) then
      Filtered := AStateBookmark.DataSetState.Filtered;
  end;

  SetGridBookmark(AStateBookmark.DataSource, ADataSourceRestore);

  if AStateBookmark.DataSetState.NotNull and (sbrDisableCount in ADataSetStateRestore) then
    EnableControls;
end;

function TDataSetHelper.CloneDataSet: TDataSet;
begin
  Result := TDataSetClass(ClassType).Create(Owner);

  try
    Result.Assign(Self);
  except
    Result.Free;
    Raise
  end;
end;

function TDataSetHelper.Editing: Boolean;
begin
  Result := {(Self <> nil) and} (State in [dsInsert, dsEdit]);
end;

procedure TDataSetHelper.EmptyTable;
begin
  DisableControls;
  try
    while not IsEmpty do
      Delete;
  finally
    EnableControls
  end;
end;

{$IFNDEF PACKAGE}
function TDataSetHelper.Evaluate(const AExpression: string; AOldValues: Boolean; AConditionParser: PExprParser): Variant;
var
  LExprEval : TDataSetExprEvaluator;
  LParser   : TExprParser;
begin
  if IsEmpty or AExpression.IsEmpty then
    Exit(unAssigned);

  if (AConditionParser <> nil) and (AConditionParser^ <> nil) then
    LParser := AConditionParser^
  else begin
    LParser := TExprParser.Create(Self, AExpression, [], [poExtSyntax], '', nil, FieldTypeMap);
    if AConditionParser <> nil then
      AConditionParser^ := LParser;
  end;

  LExprEval := TDataSetExprEvaluator.Create(Self, AOldValues);

  try
    LExprEval.Data := Pointer(LParser.FilterData);
    Result := LExprEval.Evaluate;
  finally
    if AConditionParser = nil then
      LParser.Free;

    LExprEval.Free;
  end;
end;

function TDataSetHelper.EvaluateBoolean(const AExpression: string; AOldValues: Boolean; ADefaultValue: Boolean): Boolean;
begin
  Result := SBaseVariantFunctions.IsEmpty(Evaluate(AExpression, AOldValues), ADefaultValue) = True
end;
{$ENDIF}

{ TParamsHelper }

function TParamsHelper.GetValue(const AParamName: String; const AFound: PBoolean = nil): Variant;
var
  Param: TParam;
  i: Integer;
begin
  if Self <> nil then begin
    Param := FindParam(AParamName);

    if Param = nil then begin
      i := Pos('=', AParamName);
      if I > 0 then
        Param := Self.FindParam(Copy(AParamName, Succ(i), MaxInt))
      else begin
        Param := Self.FindParam(SConst_PARAM_ + AParamName);
        if Param = nil then begin
          Param := FindParam(SConst_PARAM_OUT_ + AParamName);
          if Param = nil then
            Param := FindParam(SConst_PARAM_INOUT_ + AParamName);
        end;
      end;
    end;
  end else
    Param := nil;

  if Param <> nil then begin
    Result := Param.Value;
    if AFound <> nil then AFound^ := True;
  end else begin
    Result := Null;
    if AFound <> nil then AFound^ := False;
  end;
end;

function TParamsHelper.GetValues(const AParamNames: String; const AUnknownParams: PString = nil): Variant;
var
  LItem   : TStringItem;
  LValue  : Variant;
  LFound  : Boolean;
begin
  if AUnknownParams <> nil then
    AUnknownParams^ := '';

  for LItem in SBaseStringFunctions.EnumStringItems(AParamNames) do begin
    LValue := GetValue(LItem.Value, @LFound);
    if (not LFound) and (AUnknownParams <> nil) then
      AUnknownParams^ := ConcatIfNotEmpty(AUnknownParams^, ';', LItem.Value);

    if (LItem.Index = 0) and LItem.Eof then
      Result  := LValue
    else begin
      if LItem.Index = 0 then
        Result := VarArrayOf([LValue])
      else begin
        VarArrayRedim(Result, LItem.Index);
        Result[LItem.Index] := LValue;
      end;
    end;
  end;
end;

procedure TParamsHelper.Load(AVariables: TNamedVariants; ALoadValueCondition: TLoadValueCondition; const AExcludedParams: String);
var
  I, LIdx: Integer;
  LValue: Variant;
  LReceive: Boolean;
begin
  for I := 0 to Count - 1 do
    with Items[I] do
      if not InArray(AExcludedParams, Name, ';') then begin
        LReceive := AVariables.Find(Name, LIdx);
        if LReceive then
          LValue := AVariables.Items[LIdx].Value;

        if LReceive then begin
          if (ALoadValueCondition <> lvcIfNotPresent) or (IsNull) then
            Value := LValue;
        end else if ALoadValueCondition = lvcAlways then
          Clear
      end;
end;

procedure TParamsHelper.SetValues(const AParamNames: string; const AValues: Variant);
var
  LItem : TStringItem;
  LParam: TParam;
  I     : Integer;
begin
  for LItem in SBaseStringFunctions.EnumStringItems(AParamNames) do begin
    LParam := FindParam(LItem.Value);
    if not Assigned(LParam) then begin
      I := Pos('=', LItem.Value);
      if I > 0 then
        LParam := FindParam(LeftStr(LItem.Value, Pred(I)))
      else begin
        LParam := FindParam(SConst_PARAM_ + LItem.Value);
        if LParam = nil then
          LParam := FindParam(SConst_PARAM_INOUT_ + LItem.Value);
      end;
    end;

    if not Assigned(LParam) then
      if (Owner <> nil) and (Owner is TDataSet) then
        Raise Exception.CreateFmt(MError_DataSetParamNameNotFound, [TDataSet(Owner).Name, LItem.Value])
      else
        Raise Exception.CreateFmt(MError_ParamNameNotFound, [LItem.Value]);

    if (LItem.Index = 0) and LItem.Eof then
      LParam.Value := AValues
    else
      LParam.Value := AValues[LItem.Index]
  end;
end;

{ TFieldsHelper }

function TFieldsHelper.All(AFilter: TAllFields): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do with Fields[I] do
    if ((not (afData in AFilter)) or (FieldKind = fkData))
      and
       ((not (afReadOnly in AFilter)) or ReadOnly)
    then
      Result := ConcatIfNotEmpty(Result, ';', FieldName)
end;

function TFieldsHelper.ClearFields(const AFields: String; AEditModes: TFieldsEditModes): Boolean;
var
  LStringItem: TStringItem;
  LField: TField;
  LFields: TArray<TField>;
  LFieldCount: Integer;
begin
  Assert(DataSet.Active and ((femAutoEdit in AEditModes) or DataSet.Editing));
  Result := False;

  LFieldCount := 0;

  for LStringItem in SBaseStringFunctions.EnumStringItems(AFields, ';') do begin
    LField := FieldByName(LStringItem.Value);
    with LField do
      if not IsNull then
        if CanModify then begin
          if femAllOrRaise in AEditModes then begin
            SetLength(LFields, Succ(LFieldCount));
            LFields[LFieldCount] := LField;
            Inc(LFieldCount);
          end else begin
            DataSet.Edit;
            Clear;
            Result := True;
          end;
        end else if (femAllOrRaise in AEditModes) then
          Raise Exception.Create('Field "' + LStringItem.Value + '" cannot be modified');
  end;

  if (femAllOrRaise in AEditModes) then begin
    Result := True;
    DataSet.Edit;
    while (LFieldCount > 0) do begin
      Dec(LFieldCount);
      LFields[LFieldCount].Clear;
    end;
  end;
end;

procedure TFieldsHelper.SetAllEditAble;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    with Fields[I] do
      if ReadOnly then
        ReadOnly := False;
end;

function TFieldsHelper.SetFields(const AFields: String; const AValues: Variant; AEditModes: TFieldsEditModes): Boolean;
var
  LStringItem: TStringItem;
  LField: TField;
  LFieldValue: Variant;
  LFields: TArray<TField>;
  LFieldValues: TArray<Variant>;
  LFieldCount: Integer;
begin
  Assert(DataSet.Active and ((femAutoEdit in AEditModes) or DataSet.Editing));

  Result := False;
  LFieldCount := 0;

  for LStringItem in SBaseStringFunctions.EnumStringItems(AFields, ';') do begin
    LField := FieldByName(LStringItem.Value);
    if LStringItem.Eof and (LStringItem.Index = 0) then
      LFieldValue := AValues
    else
      LFieldValue := AValues[LStringItem.Index];

    with LField do
      if not VarIsEqual(AsVariant, LFieldValue) then
        if CanModify then begin
          if femAllOrRaise in AEditModes then begin
            SetLength(LFields, Succ(LFieldCount));
            SetLength(LFieldValues, Succ(LFieldCount));
            LFields[LFieldCount] := LField;
            LFieldValues[LFieldCount] := LFieldValue;
            Inc(LFieldCount);
          end else begin
            DataSet.Edit;
            AsVariant := LFieldValue;
            Result := True;
          end;
        end else if (femAllOrRaise in AEditModes) then
          Raise Exception.Create('Field "' + LStringItem.Value + '" cannot be modified');
  end;

  if (femAllOrRaise in AEditModes) then begin
    DataSet.Edit;
    Result := True;
    while (LFieldCount > 0) do begin
      Dec(LFieldCount);
      LFields[LFieldCount].AsVariant := LFieldValues[LFieldCount];
    end;
  end;
end;

procedure TFieldsHelper.CreateField(const AName, AType: String; ALength: Word; AScale: Byte);
var
//  LDataType: TFieldType;
  LFieldClass: TFieldClass;
  LField: TField;
begin
  if SameText(AType, DelphiTypeString) then
    LFieldClass := TStringField
  else if SameText(AType, SQLtypeTinyInt) then
    LFieldClass := TSmallIntField
  else if SameText(AType, DelphiTypeWord) then
    LFieldClass := TWordField
  else if SameText(AType, DelphiTypeInteger) then
    LFieldClass := TIntegerField
  else if SameText(AType, SQLtypeBigInt) then
    LFieldClass := TLargeintField
  else if SameText(AType, DelphiTypeBigInteger) then
    LFieldClass := TLargeintField
  else if SameText(AType, DelphiTypeSmallInt) then
    LFieldClass := TSmallintField
  else if SameText(AType, DelphiTypeBoolean) then
    LFieldClass := TBooleanField
  else if SameText(AType, DelphiTypeFloat) then
    LFieldClass := TFloatField
  else if SameText(AType, DelphiTypeExtended) then
    LFieldClass := TExtendedField
  else if SameText(AType, DelphiTypeBcd) then
    LFieldClass := TBCDField
  else if SameText(AType, DelphiTypeFMTBcd) then
    LFieldClass := TFMTBCDField
  else if SameText(AType, DelphiTypeDateTime) then
    LFieldClass := TDateTimeField
  else if SameText(AType, DelphiTypeDate) then
    LFieldClass := TDateField
  else if SameText(AType, DelphiTypeTime) then
    LFieldClass := TTimeField
  else if SameText(AType, DelphiTypeGUId) then
    LFieldClass := TGuidField
  else if SameText(AType, DelphiTypeVariant) then
    LFieldClass := TVariantField
  else
    Raise Exception.Create('Unknown field data type '#171 + AType + #187);

  LField := LFieldClass.Create(nil);
  LField.FieldName := AName;

  try
    case LField.DataType of
      ftString, ftWideString, ftMemo, ftWideMemo:
        LField.Size := ALength;
      ftFloat:
        with TFloatField(LField) do begin
          Precision := ALength;
  //        Scale := AScale;
        end;
      ftExtended:
        with TExtendedField(LField) do begin
          Precision := ALength;
  //        Scale := AScale;
        end;
      ftBcd:
        with TBCDField(LField) do begin
          Precision := ALength;
          Size      := AScale;
        end;
      ftFMTBCD:
        with TFMTBCDField(LField) do begin
          Precision := ALength;
          Size      := AScale;
        end;
    end;
  except
    LField.Free;
    Raise;
  end;
  Add(LField);
end;

procedure TFieldsHelper.Load(const AVariables: TNamedVariants; ALoadValueCondition: TLoadValueCondition; const AExcludedFields: String);
var
  I, LIdx: Integer;
begin
  for I := 0 to Count - 1 do
    with Fields[I] do
      if (FieldKind = fkData) and CanModify and (not InArray(AExcludedFields, FieldName, ';')) then begin
        if AVariables.Find(FieldName, LIdx) then begin
          if (ALoadValueCondition <> lvcIfNotPresent) or (IsNull) then
{$IFNDEF PACKAGE}
{$IFDEF DEBUG}
          try
{$ENDIF}
{$ENDIF}
            AsVariant := AVariables.Items[LIdx].Value;
{$IFNDEF PACKAGE}
{$IFDEF DEBUG}
          except on E:Exception do begin
            Raise Exception.Create(DataSet.Source.GetDisplayName + ':' + FieldName + ': ' + E.Message);
          end end;
{$ENDIF}
{$ENDIF}
        end else if ALoadValueCondition = lvcAlways then
          Clear;
      end;
end;

procedure TFieldsHelper.Save(AVariables: TNamedVariants; ASaveValueCondition: TLoadValueCondition; const AExcludedFields: String);
var
  I, LIdx: Integer;
  LValue: Variant;
  LReceive: Boolean;
begin
  for I := 0 to Count - 1 do
    with Fields[I] do
      if (FieldKind = fkData) and (not InArray(AExcludedFields, FieldName, ';')) then begin
        LReceive := (not IsNull);
        if LReceive then
          LValue := AsVariant;

        if LReceive then begin
          if (ASaveValueCondition <> lvcIfNotPresent) or (not AVariables.Find(FieldName, LIdx)) then
            AVariables.SetValue(FieldName, LValue);
        end else if ASaveValueCondition = lvcAlways then
          AVariables.Delete(LIdx);
      end;
end;

function TFieldsHelper.FindFieldValue(AFieldName: String): Variant;
var
  LField: TField;
begin
  LField := FindField(AFieldName);

  if not Assigned(LField) then
    Result := Unassigned
  else if LField.IsNull then
    Result := Null
  else
    Result := LField.AsVariant;
(*
    case LField.DataType of
      ftVarBytes: Result := LField.AsAnsiString;
      ftBlob    : Result := LField.AsAnsiString;
      {
      begin
        LByteStream := TBytesStream.Create();
        with LByteStream do try
          AField.SaveToStream(LByteStream);
          Result := AnsiString(Bytes);
        finally
          Free
        end;
      end;
      }
      else
        Result := LField.AsVariant;
  end;
*)
end;

function TFieldsHelper.GetFieldValue(const FieldName: string): Variant;
begin
  Result := DataSet.FieldValues[FieldName]
end;

procedure TFieldsHelper.SetFieldValue(const FieldName: string; const Value: Variant);
begin
  DataSet.FieldValues[FieldName] := Value;
end;

function TFieldsHelper.GetValue(const AFieldName: String; const AFound: PBoolean = nil): Variant;
var
  LField: TField;
  i: Integer;
begin
  if Self <> nil then
    LField := FindField(AFieldName)
  else
    LField := nil;

  if LField = nil then begin
    i := Pos('=', AFieldName);
    if I > 0 then
      LField := FindField(Copy(AFieldName, Succ(i), MaxInt));
  end;

  if AFound <> nil then
    AFound^ := (LField <> nil) or (AFieldName = '@');

  if LField <> nil then
    Result := LField.AsVariant
  else if AFieldName = '@' then
    Result := Integer(DataSet)
  else
    Result := Null;
end;

function TFieldsHelper.GetValues(const AFieldNames: String; const AUnknownFields: PString = nil): Variant;
var
  LItem : TStringItem;
  LValue: Variant;
  LFound: Boolean;
begin
  if AUnknownFields <> nil then
    AUnknownFields^ := '';

  for LItem in SBaseStringFunctions.EnumStringItems(AFieldNames) do begin
    LValue := GetValue(LItem.Value, @LFound);
    if (not LFound) and (AUnknownFields <> nil) then
      AUnknownFields^ := ConcatIfNotEmpty(AUnknownFields^, ';', LItem.Value);

    if (LItem.Index = 0) and LItem.Eof then
      Result  := LValue
    else begin
      if LItem.Index = 0 then
        Result := VarArrayOf([LValue])
      else begin
        VarArrayRedim(Result, LItem.Index);
        Result[LItem.Index] := LValue;
      end;
    end;
  end;
end;

function TFieldsHelper.IsAllEditAble: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := Count - 1 downto 0 do
    if not Fields[I].CanModify then begin
      Result := False;
      Exit;
    end;
end;

procedure TFieldsHelper.SetValues(const AFieldNames: String; const AValue: Variant);
var
  LItem : TStringItem;
  LIndex: Integer;
  LName : String;
  LValue: Variant;
begin
  for LItem in SBaseStringFunctions.EnumStringItems(AFieldNames) do begin
    if FindPos('=', LItem.Value, LIndex) then
      LName := LeftStr(LItem.Value, Pred(LIndex))
    else
      LName := LItem.Value;

    if (LItem.Index = 0) and LItem.Eof then
      LValue := AValue
    else
      LValue := AValue[LItem.Index];

    FieldByName(LName).AsVariant := LValue;
  end;
end;

procedure TFieldsHelper.Load(AFields: TFields; ALoadValueCondition: TLoadValueCondition; const AExcludedFields: String; AOnlyIfCanModify: Boolean);
var
  I: Integer;
  LField: TField;
begin
  for I := 0 to Count - 1 do
    with Fields[I] do
      if (FieldKind = fkData) and CanModify and (not InArray(AExcludedFields, FieldName, ';')) then begin
        LField := AFields.FindField(FieldName);
        if AOnlyIfCanModify and (not Assigned(LField) or not LField.CanModify) then Continue;

        if Assigned(LField) then begin
          if (ALoadValueCondition <> lvcIfNotPresent) or (IsNull and not LField.IsNull) then
            AsVariant := LField.AsVariant;
        end else if (ALoadValueCondition = lvcAlways) and (not IsNull) then
          Clear;

//        else if ANotFoundAction <> nfNone then
//          Raise Exception.Create('Field '#171 + FieldName + #187' not found in source fields collection');
      end;
end;

{ TFieldHelper }

function TFieldHelper.Clone(AOwner: TComponent = nil): TField;
begin
  Result := TFieldClass(ClassType).Create(AOwner);
  Result.FieldName  := FieldName;
  Result.Size       := Size;

  if Self is TFloatField then
    with TFloatField(Self) do begin
      TFloatField(Result).Precision := Precision;
    end
  else if Self is TExtendedField then
    with TExtendedField(Self) do begin
      TExtendedField(Result).Precision := Precision;
    end
  else if Self is TBCDField then
    with TBCDField(Self) do begin
      TBCDField(Result).Precision := Precision;
      TBCDField(Result).Size := Size;
    end
  else if Self is TFMTBCDField then
    with TFMTBCDField(Self) do begin
      TFMTBCDField(Result).Precision := Precision;
      TFMTBCDField(Result).Size := Size;
    end
end;

function TFieldHelper.IsNullOrWhiteSpace: Boolean;
begin
  if IsNull then Exit(True);
  if DataType in [ftString, ftWideString, ftMemo, ftWideMemo] then
    Exit(String.IsNullOrWhiteSpace(AsString));
  Result := False;
end;

function TFieldHelper.IsOrdinal: Boolean;
begin
  Result := DataType in [ftByte, ftShortint, ftSmallint, ftInteger, ftLargeint, ftWord, ftLongWord];
end;

{ TParamHelper }

function TParamHelper.GetAsVariant: Variant;
begin
  Result := Value;
end;

procedure TParamHelper.SetAsVariant(const AValue: Variant);
begin
  Value := AValue;
end;

{ Class TSQLConnectionProvider }

constructor TSQLConnectionProvider.Create(AOwner: TComponent);
begin
  FSQLDataSetProviders := TList<TCustomSQLDataSetProvider>.Create;

  inherited;

//  FClientDataSetProvider := TDataSetProvider.Create(Self);
//  with FClientDataSetProvider do begin
//    Name := 'ClientDataSetProvider';
//    Options := [poReadOnly, poAllowCommandText];
//  end;
end;

destructor TSQLConnectionProvider.Destroy;
var
  I: Integer;
begin
  FreeAndNil(FPublicDataSets);

  for I := Pred(FSQLDataSetProviders.Count) downto 0 do
    FSQLDataSetProviders[I].ConnectionProvider := nil;

  FreeAndNil(FSQLDataSetProviders);
{$IFDEF SERVICEBROKER_SUPPORT}
  FreeAndNil(FServiceBrokerProviders);
{$ENDIF}

  inherited;
end;

{
procedure TSQLConnectionProvider.AssignTo(Dest: TPersistent);
begin
  inherited;

  TSQLConnectionProvider(Dest).OnInfoMessage    := OnInfoMessage;
  TSQLConnectionProvider(Dest).AfterConnect     := AfterConnect;
  TSQLConnectionProvider(Dest).AfterDisconnect  := AfterDisconnect;
end;
}

procedure TSQLConnectionProvider.BeginTran;
begin
  FConnection.StartTransaction
end;

procedure TSQLConnectionProvider.CommitTran(AIfPresent: Boolean);
begin
  with FConnection do
    if (not AIfPresent) or InTransaction then
      Commit
end;

function TSQLConnectionProvider.GetConnected: Boolean;
begin
  Result := Assigned(FConnection) and FConnection.Connected;
end;

procedure TSQLConnectionProvider.SetConnected(AValue: Boolean);
begin
  Assert((not AValue) or Assigned(FConnection));
  FConnection.Connected := AValue;
end;

{$IFNDEF PACKAGE}
procedure TSQLConnectionProvider.ConnectionAfterConnect(Sender: TObject);
begin
  GetSessionVariables;
{$IFDEF SERVICEBROKER_SUPPORT}
  SQLServiceBrokerProvidersNotifyConnectionChanged;
{$ENDIF}

  if Assigned(FAfterConnect) then
    FAfterConnect(Self)
end;

procedure TSQLConnectionProvider.ConnectionAfterDisconnect(Sender: TObject);
begin
{$IFDEF SERVICEBROKER_SUPPORT}
  SQLServiceBrokerProvidersNotifyConnectionChanged;
{$ENDIF}

  if Assigned(FAfterDisconnect) then
    FAfterDisconnect(Self);
end;

procedure TSQLConnectionProvider.GetSessionVariables;
begin
  if not FSQLSessionVariables.IsEmpty then begin
    if (csReading in ComponentState) then begin
      FLoadedSessionVariables := True;
      Exit;
    end else begin
      FSessionVariables := ExecSQL(FSQLSessionVariables, TNamedVariants.Create(True));
      FSessionVariables.ClearUndefined;
    end;
  end else
    FSessionVariables.Clear;

  FLoadedSessionVariables := False;
end;
{$ENDIF}

function TSQLConnectionProvider.ConnectionInfo: String;
begin
  if Assigned(FConnection) then
    with FConnection do
      Result := SConst_Server + '=' + Server + ';' + SConst_DataBase + '=' + Database + ';' + SConst_User + '=' + Username
  else
    Result := '';
end;

procedure TSQLConnectionProvider.RollbackTran(AIfPresent: Boolean);
begin
  FConnection.RollbackTran(AIfPresent);
//    if (not AIfPresent) or InTransaction then
//      Rollback
end;

function TSQLConnectionProvider.TranCount: Integer;
begin
  Result := FConnection.TransactionCount
end;

function TSQLConnectionProvider.VariantToDataBaseString(const AValue: Variant; const ABinaryStringFormat: Boolean): String;
begin
  Assert(Assigned(FConnection) and Assigned(FConnection.Interpreter));
  Result := FConnection.Interpreter.VariantToDataBaseString(AValue, ABinaryStringFormat)
end;

function TSQLConnectionProvider.VarTypeToDataBaseType(AVarType: TVarType): String;
begin
  Assert(Assigned(FConnection) and Assigned(FConnection.Interpreter));
  Result := FConnection.Interpreter.VarTypeToDataBaseType(AVarType)
end;

procedure TSQLConnectionProvider.CheckConnection;
begin
  if (FConnection = nil) then
    Raise Exception.Create(SConnectionNotDefined);
  if not FConnection.Connected then
    FConnection.Connect;
end;

function TSQLConnectionProvider.Clone(AOwner: TComponent): TSQLConnectionProvider;
var
  LConnection: TInternalConnection;
begin
  if Connection <> nil then begin
    Result := TSQLConnectionProviderClass(ClassType).Create(AOwner);
    try
      Result.OnInfoMessage        := nil;

      LConnection := TInternalConnectionClass(Connection.ClassType).Create(Result);
      try
        LConnection.Assign(Connection);
        LConnection.LoginPrompt := False;

        LConnection.AfterConnect    := nil;
        LConnection.AfterDisconnect := nil;

        LConnection.Connected   := True;

        Result.Connection := TInternalConnection(LConnection);
        Result.SQLSessionVariables  := SQLSessionVariables;
      except on E:Exception do begin
        LConnection.Free;
        Raise;
      end end
    except on E:Exception do begin
      Result.Free;
      Raise;
    end end
  end else
    Raise Exception.Create('Connection could not be cloned.');
end;

{$IFNDEF PACKAGE}
function TSQLConnectionProvider.OpenSQLQuery(const ACommandText: String; const AParams: TParams): TCustomInternalQuery;
var
  Idx: Integer;
  LParams: TParams;
begin
  Result := GetSQLQuery;
  with Result do begin
    LParams := Params;

    { Bug Fix }
    if Assigned(LParams) then
      LParams.Clear;

    SQL.Text := ACommandText;

    if Assigned(LParams) then
      for Idx := 0 to LParams.Count - 1 do with LParams[Idx] do
        Value := AParams.GetValue(Name);

    try
      Execute;
    except
      Result.Close; Result.UnPrepare;
      Raise;
    end;
  end;
end;

function TSQLConnectionProvider.GetSQLQuery: TCustomInternalQuery;
begin
  CloseSQLQuery;
  Connected := True;
  Result := FDataSet;
end;

function TSQLConnectionProvider.GetSQLQuery(const ACommandText: String; const AParams: TNamedVariants): TCustomInternalQuery;
var
  Idx: Integer;
  LParams: TParams;
begin
  Result := GetSQLQuery;

  with Result do begin
    LParams := Params;

    { Bug Fix }
    if Assigned(LParams) then
      LParams.Clear;

    SQL.Text := ACommandText;

    if Assigned(LParams) then
      for Idx := 0 to LParams.Count - 1 do with LParams[Idx] do
        Value := AParams[Name];
  end;
end;

function TSQLConnectionProvider.OpenSQLQuery(const ACommandText: String; const AParams: TNamedVariants): TCustomInternalQuery;
begin
  Result := GetSQLQuery(ACommandText, AParams);
  try
    Result.Execute;
  except
    Result.Close; Result.UnPrepare;
    Raise;
  end;
end;

procedure TSQLConnectionProvider.CloseSQLQuery;
begin
  with FDataSet do begin
    Close; UnPrepare;
    SQL.Clear;

    Filtered  := False;
    Filter    := '';
  end;
end;

function TSQLConnectionProvider.OpenSQLPublicQuery(const ACommandText: String; AParams: TParams): TUserMemoryDataSet;
begin
  CloseSQLPublicQuery;
  if not Assigned(FPublicDataSet) then
    FPublicDataSet := TUserMemoryDataSet.Create(Self);

  CreatePublicDataSet(Self, ACommandText, AParams, False).DataSet.Save(FPublicDataSet);
  Result := FPublicDataSet;
end;

procedure TSQLConnectionProvider.CloseSQLPublicQuery;
begin
  if Assigned(FPublicDataSet) then with FPublicDataSet do begin
    Close;
    Filtered  := False;
    Filter    := '';
  end;
end;

function TSQLConnectionProvider.ExecSQL(const ACommandText: String; const AParams: TNamedVariants; AUseMultipleResults: Boolean): TNamedVariants;
var
  LDataSet: TCustomInternalQuery;
begin
  LDataSet := OpenSQLQuery(ACommandText, AParams);
  Result := TNamedVariants.Create(False);
  try
    while LDataSet.Active do begin
      if (not LDataSet.IsEmpty) then
        Result.AssignValues(LDataSet.Fields, amAggregate);
      if AUseMultipleResults then
        LDataSet.OpenNext
      else
        Break;
    end;
  finally
    CloseSQLQuery;
  end
end;

function TSQLConnectionProvider.ExecSQL(const ACommandText: String; const AParams: Array of TNamedVariant; AUseMultipleResults: Boolean): TNamedVariants;
begin
  Result := ExecSQL(ACommandText, TNamedVariants.Create(AParams, True));
end;

function TSQLConnectionProvider.ExecSQL(const ACommandText, AParamsNames: String; const AParamValues: Variant; AUseMultipleResults: Boolean): TNamedVariants;
begin
  Result := ExecSQL(ACommandText, TNamedVariants.Create(AParamsNames, AParamValues, True));
end;
{$ENDIF}

procedure TSQLConnectionProvider.AddSQLDataSetProvider(ASQLDataSetProvider: TCustomSQLDataSetProvider);
begin
  if FSQLDataSetProviders.IndexOf(ASQLDataSetProvider) < 0 then
    FSQLDataSetProviders.Add(ASQLDataSetProvider);

  FreeNotification(ASQLDataSetProvider);

  ASQLDataSetProvider.FSQLConnectionProvider := Self;
end;

procedure TSQLConnectionProvider.RemoveSQLDataSetProvider(ASQLDataSetProvider: TCustomSQLDataSetProvider);
begin
  FSQLDataSetProviders.Remove(ASQLDataSetProvider);
  ASQLDataSetProvider.FSQLConnectionProvider := nil;
  RemoveFreeNotification(ASQLDataSetProvider);
end;

{$IFDEF SERVICEBROKER_SUPPORT}
procedure TSQLConnectionProvider.AddSQLServiceBrokerProvider(ASQLServiceBrokerProvider: TCustomSQLServiceBrokerProvider);
begin
  if not Assigned(FServiceBrokerProviders) then
    FServiceBrokerProviders := TList<TCustomSQLServiceBrokerProvider>.Create;
  FServiceBrokerProviders.Add(ASQLServiceBrokerProvider);
end;

procedure TSQLConnectionProvider.RemoveSQLServiceBrokerProvider(ASQLServiceBrokerProvider: TCustomSQLServiceBrokerProvider);
begin
  if Assigned(FServiceBrokerProviders) then
    FServiceBrokerProviders.Remove(ASQLServiceBrokerProvider);
end;

procedure TSQLConnectionProvider.SQLServiceBrokerProvidersNotifyConnectionChanged;
var
  I: Integer;
begin
  if Assigned(FServiceBrokerProviders) then
    for I := 0 to FServiceBrokerProviders.Count - 1 do
      TCustomSQLServiceBrokerProviderCrack(FServiceBrokerProviders[I]).ConnectionChanged;
end;
{$ENDIF}

procedure TSQLConnectionProvider.SetConnection(const AValue: TInternalConnection);
{$IFNDEF PACKAGE}
var
  I: Integer;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  if FConnection = AValue then
    Exit
  else if (FConnection <> nil) then
    with FConnection do begin
      AfterConnect    := nil;
      AfterDisconnect := nil;
    end;

  FSessionVariables.Clear;

  if AValue = nil then
    FConnection  := nil
  else if (FConnection = nil) or (FConnection <> AValue) then
    FConnection  := AValue
  else
    Exit;

  if (FConnection <> nil) then
    with FConnection do begin
      AfterConnect    := ConnectionAfterConnect;
      AfterDisconnect := ConnectionAfterDisconnect;
    end;

  FreeAndNil(FDataSet);
  FDataSet              := TCustomInternalQuery.Create(Self);
  FDataSet.LocalUpdate  := True;
  FDataSet.Connection   := FConnection;
//  FDataSet.ReadOnly     := True;

  for I := 0 to Pred(FSQLDataSetProviders.Count) do
    TSQLDataSetProvider(FSQLDataSetProviders.Items[I]).SQLConnectionProviderChanged;

{$IFDEF SERVICEBROKER_SUPPORT}
  SQLServiceBrokerProvidersNotifyConnectionChanged;
{$ENDIF}

{$ELSE}
  FConnection := AValue;
{$ENDIF}
end;

procedure TSQLConnectionProvider.SetOnInfoMessage(const Value: TInternalConnectionOnInfoMessage);
begin
  FOnInfoMessage := Value;
  if FConnection <> nil then
    FConnection.OnInfoMessage := FOnInfoMessage
end;

procedure TSQLConnectionProvider.SetSQLSessionVariables(const AValue: String);
begin
  if FSQLSessionVariables <> AValue then begin
    FSQLSessionVariables := AValue;
{$IFNDEF PACKAGE}
    if Connected then
      GetSessionVariables;
{$ENDIF}
  end;
end;

function TSQLConnectionProvider.EqualParams(AParams: TParams; ASource: TParams): Boolean;
var
  I: Integer;
  LSourceValue: Variant;
begin
  Result := False;

  for I := Pred(AParams.Count) downto 0 do begin
    LSourceValue := ASource.GetValue(AParams[I].Name);
    if ( AParams[I].IsNull
            and ( not (VarType(LSourceValue) in [varEmpty, varNull]) )
       )
       or
       (
          (not AParams[I].IsNull)
          and
          (
            AParams[I].Value <> LSourceValue
          )
       )
    then
      Exit;
  end;

  Result := True;
end;

function TSQLConnectionProvider.IsActualDataSet(ADataSet: TBasePublicDataSet; const ACommandText: String; AParams: TParams): Boolean;
begin
  Result := SameText(ADataSet.CommandText, ACommandText) and EqualParams(ADataSet.Params, AParams)
end;

{$IFNDEF PACKAGE}
procedure TSQLConnectionProvider.Loaded;
begin
  inherited;
  if FLoadedSessionVariables then
    GetSessionVariables;
end;
{$ENDIF}

function TSQLConnectionProvider.CreatePublicDataSet(AOwner: TComponent; const ACommandText: String; AParams: TParams; ALiteMode: Boolean): TBasePublicDataSet;
var
  LBasePublicDataSet: TBasePublicDataSet;
begin
  Result := nil;

  if FPublicDataSets = nil then
    FPublicDataSets := TObjectList<TBasePublicDataSet>.Create(True)
  else
    for LBasePublicDataSet in FPublicDataSets do
      if IsActualDataSet(LBasePublicDataSet, ACommandText, AParams) then begin
        Result := LBasePublicDataSet;
        Break;
      end;

  if (Result = nil) then begin
    Result := TBasePublicDataSet.Create(Self, ACommandText, AParams);
    FPublicDataSets.Add(Result);
  end;

  if Result.Owners.IndexOf(AOwner) = -1 then begin
    Result.Owners.Add(AOwner);
    Result.ReleaseTime := 0;
  end;

  if not Result.DataSet.Active(ALiteMode) then
    Result.Open(ALiteMode);
end;

procedure TSQLConnectionProvider.ClosePublicDataSet(AOwner: TComponent; ADataSet: TBasePublicDataSet);
begin
{$IFDEF DEBUG}
  Assert(Assigned(AOwner) and Assigned(ADataSet) and (ADataSet.SQLConnectionProvider = Self));
{$ENDIF}

  ADataSet.Owners.Remove(AOwner);
  if (ADataSet.Owners.Count = 0) then
    ADataSet.ReleaseTime := IncMinute(Now);
end;

{ TStorageBaseSQLConnectionProvider }

destructor TStorageBaseSQLConnectionProvider.Destroy;
begin
  FreeAndNil(FOwners);
  inherited;
end;

procedure TStorageBaseSQLConnectionProvider.CreateLink(AOwner: TComponent);
begin
  Assert(Assigned(AOwner));

  if not Assigned(FOwners) then
    FOwners := TList<TComponent>.Create
  else if FOwners.IndexOf(AOwner) >= 0 then
    Exit;

  FOwners.Add(AOwner);
end;

function TStorageBaseSQLConnectionProvider.IsLinkedFrom(AOwner: TComponent): Boolean;
begin
  Result := Assigned(FOwners) and (FOwners.IndexOf(AOwner) >= 0)
end;

function TStorageBaseSQLConnectionProvider.LinkCount(ANotEqualToOwner: TComponent): Integer;
begin
  if (not Assigned(FOwners)) then
    Result := 0
  else
    Result := FOwners.Count - IfThen(FOwners.IndexOf(ANotEqualToOwner) >= 0, 1, 0);
end;

procedure TStorageBaseSQLConnectionProvider.ReleaseLink(AOwner: TComponent);
begin
  if Assigned(FOwners) then
    FOwners.Remove(AOwner);
end;

{ TSQLConnectionProviderStorage }

constructor TSQLConnectionProviderStorage.Create(AOwner: TSQLConnectionProvider);
begin
  Assert(Assigned(AOwner)); //  and Assigned(AOwner.FConnectionHelper.Connection)
  inherited Create(AOwner);
end;

procedure TSQLConnectionProviderStorage.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;

  if (Operation = opRemove) then
    for I := 0 to Pred(Length(FBaseDataModules)) do
      FBaseDataModules[I].ReleaseLink(AComponent);
end;

{$IFNDEF PACKAGE}
procedure TSQLConnectionProviderStorage.GetConnection(AOwner: TComponent; AParams: TNamedVariants; AIndepended: Boolean; ASQLDataSetProvider: TSQLDataSetProvider);
begin
  Assert(Assigned(ASQLDataSetProvider));

  try
    ASQLDataSetProvider.ConnectionProvider := AssignConnection(AOwner, AParams, AIndepended, ASQLDataSetProvider.ConnectionProvider);
  except
    ASQLDataSetProvider.ConnectionProvider := nil;
    Raise;
  end;
end;

function TSQLConnectionProviderStorage.AssignConnection(AOwner: TComponent; AParams: TNamedVariants; AIndepended: Boolean; AUsedConnectionProvider: TSQLConnectionProvider = nil): TSQLConnectionProvider;
var
  LServer, LDataBase, LCached: String;
  LParams: TNamedVariants;
  LResult: TNamedVariants;

  LConnection: TInternalConnection;

  procedure AcceptConnection;
  begin
    with TStorageBaseSQLConnectionProvider(Result) do begin
      CreateLink(AOwner);

      if AIndepended then begin
        Independed := True;
        Result.OnInfoMessage := nil;
      end else begin
        Independed := False;

        Result.OnInfoMessage := TSQLConnectionProvider(Self.Owner).OnInfoMessage;
      end;

      if not SameText(FConnection.DataBase, LDataBase) then
        FConnection.DataBase := LDataBase;

      if not FConnection.Connected then
        FConnection.Connected := True;
    end;
  end;

var
  I, FCount: Smallint;
begin
  { Обязательная идентификация по владельцу }
  Assert(Assigned(AOwner));

  { Начнем с нуля }
  LCached           := '';

  { Если текущий ДатаСорс уже имеет некий источник данных, то ... }
  if Assigned(AUsedConnectionProvider) then
    if (AUsedConnectionProvider = TSQLConnectionProvider(Owner))
          or (AUsedConnectionProvider.Connection = nil)
          or not (AUsedConnectionProvider is TStorageBaseSQLConnectionProvider)
    then
      AUsedConnectionProvider := nil
    else with TStorageBaseSQLConnectionProvider(AUsedConnectionProvider) do begin
      if (LinkCount(AOwner) = 0) or (not AIndepended) then
        LCached := FConnection.Server + IfThen(LinkCount(AOwner) > 0, '\' + FConnection.DataBase, '')
    end;

{ ------------------------------------------------------------------ }
  for I := Pred(Length(FBaseDataModules)) downto 0 do
    if (AUsedConnectionProvider <> FBaseDataModules[i]) then
      with FBaseDataModules[I] do
        if (LinkCount = 0) then
          LCached := ArraysMerge([LCached, FConnection.Server], ',')
        else if ( (not AIndepended) and (not Independed) ) then
          LCached := ArraysMerge([LCached, FConnection.Server + '\' + FConnection.DataBase], ',');

  LParams := AParams;
  if LCached <> '' then
    LParams['Connected'] := LCached;

  LResult := TSQLConnectionProvider(Owner).ExecSQL(FCommandText, LParams);
  LResult.UnknownAsNull := True;

{ ------------------------------------------------------------------ }

  LServer   := VarToStr(LResult[SConst_Server]);
  LDataBase := VarToStrDef(LResult[SConst_DataBase], TSQLConnectionProvider(Owner).FConnection.DataBase);

  if (not AIndepended) and (LServer = '') then begin
    Result := TSQLConnectionProvider(Owner);

    if (AUsedConnectionProvider <> nil) then
      ReleaseConnection(AOwner, AUsedConnectionProvider);
  end else begin
    { Если подсказки не пришло, то используем параметры основного подключения }
    if LServer = '' then
      LServer := TSQLConnectionProvider(Owner).FConnection.Server;

    if (AUsedConnectionProvider <> nil) then begin
      { Если выбранное соединение верно то на Выход }
      with TStorageBaseSQLConnectionProvider(AUsedConnectionProvider) do
        if SameText(FConnection.Server, LServer)
            and
            (
              (LinkCount(AOwner) = 0)
              or
              ( (not AIndepended) and (not Independed) and SameText(FConnection.DataBase, LDataBase) )
            )
        then begin
          Result := TStorageBaseSQLConnectionProvider(AUsedConnectionProvider);

          AcceptConnection;
          Exit;
        end;

      { Если неверно то отключимся от него }
      ReleaseConnection(AOwner, AUsedConnectionProvider);
    end;

    FCount := Length(FBaseDataModules);

    { Ищем подходящий коннекшин }
    for I := Pred(FCount) downto 0 do with FBaseDataModules[I] do
      if SameText(FConnection.Server, LServer)
        and
         (
           (LinkCount = 0)
           or
           ( (not AIndepended) and (not Independed) and SameText(FConnection.DataBase, LDataBase) )
         )
      then begin
        Result := FBaseDataModules[I];

        AcceptConnection;
        Exit
      end;

    { Создаем подключение }

    Result := TStorageBaseSQLConnectionProvider.Create(Self);
    try
      LConnection := TInternalConnectionClass(GetClass(TSQLConnectionProvider(Owner).Connection.ClassName)).Create(Result);
      LConnection.Assign(TSQLConnectionProvider(Owner).Connection);

      Result.Connection := LConnection;

      with Result.Connection do begin
        LoginPrompt   := False;
        ConnectDialog := nil;
        Server        := LServer;
      end;

      AcceptConnection;
    except
      Result.Free;

      Raise
    end;

    // Запомним подключение
    SetLength(FBaseDataModules, Succ(FCount));
    FBaseDataModules[FCount] := TStorageBaseSQLConnectionProvider(Result);
  end;
end;

procedure TSQLConnectionProviderStorage.ResetConnection(AOwner: TComponent; ASQLDataSetProvider: TSQLDataSetProvider);
var
  LStorageDataModule: TSQLConnectionProvider;
begin
  if (ASQLDataSetProvider = nil) or (ASQLDataSetProvider.ConnectionProvider = nil) or not (ASQLDataSetProvider.ConnectionProvider is TStorageBaseSQLConnectionProvider) then
    Exit;

  LStorageDataModule := ASQLDataSetProvider.ConnectionProvider;
  ASQLDataSetProvider.ConnectionProvider := nil;

  ReleaseConnection(AOwner, LStorageDataModule);
end;

procedure TSQLConnectionProviderStorage.ReleaseConnection(AOwner: TComponent; AStorageDataModule: TSQLConnectionProvider);
var
  I, LCount: Smallint;
begin
  LCount := Pred(Length(FBaseDataModules));

  if not Assigned(AStorageDataModule) or not (AStorageDataModule is TStorageBaseSQLConnectionProvider) or (LCount < 0) then
    Exit;

  { Ищем соединение в списке }
  I := LCount;
  while (I >= 0) and (FBaseDataModules[I] <> TStorageBaseSQLConnectionProvider(AStorageDataModule)) do
    Dec(I);

  { Нашли ? }
  if I >= 0 then with FBaseDataModules[I] do begin
    { А если соединение больше никому не нужно? }
    if LinkCount(AOwner) = 0 then begin
      { Самостоятельно разорвём соединение }
      Connection.Connected := False;

      { Удалим ДатаМодуль }
      Free;

      { Сдвиг списка }
      while I < LCount do begin
        FBaseDataModules[I] := FBaseDataModules[Succ(I)];
        Inc(I)
      end;

      // Уменьшение размеров массива
      SetLength(FBaseDataModules, LCount)
    end else
      ReleaseLink(AOwner);
  end else
    AStorageDataModule.Free;
end;
{$ENDIF}

{ TBaseDataSetThreadProcess }

constructor TBaseDataSetThreadProcess.Create(AOwner: TBaseDataSetThread; AExecuteMethod: TBaseExecuteMethod; ATarget: PStreamArrayOfTDataSet);
var
  LDataSet: TInternalQuery;
  LOldActive: Boolean;
  LDataSetProvider: TCustomSQLDataSetProvider;
begin
  FExecuteMethod  := AExecuteMethod;
  FTarget         := ATarget;
  FOwner := AOwner;
  LDataSetProvider  := FOwner.FOwner;

  LOldActive := FOwner.FOwner.Active;

  if FExecuteMethod <> bemRefresh then begin
    LDataSetProvider.SetDataSet(nil);
    LDataSet := TInternalQuery(LDataSetProvider.FMainDataSet);
    if LOldActive and Assigned(LDataSetProvider.AfterClose) then
      LDataSetProvider.AfterClose(LDataSetProvider);
  end else if LDataSetProvider.ActiveDataSet = LDataSetProvider.FMainDataSet then begin
    if FOwner.FReservedDataSet = nil then begin
      FOwner.FReservedDataSet := TInternalQuery(LDataSetProvider.FMainDataSet.CloneDataSet);
{$IFDEF DEBUG}
      FOwner.FReservedDataSet.Name := SConst_ReservedDataSet;
{$ENDIF}
    end;
    LDataSet := FOwner.FReservedDataSet;
  end else
    LDataSet := TInternalQuery(LDataSetProvider.FMainDataSet);

  FDataSet := LDataSet;
  FDataSet.Connection := LDataSetProvider.ConnectionProvider.Connection;
  LDataSetProvider.AttachDataSetOptions(FDataSet);
  LDataSetProvider.InitDataSetParams(FDataSet);

  FreeOnTerminate := True;
//  FreeOnTerminate := False; //TEST(1)

  inherited Create(False);

  //Start;
end;

//procedure TBaseDataSetThreadProcess.CheckThreadError(Success: Boolean);
//begin
//  inherited CheckThreadError(Success or (FOwner = nil) or FOwner.FTerminating);
//end;

constructor TBaseDataSetThreadProcess.Create(AOwner: TBaseDataSetThread; ADataSet: TInternalQuery; AExecuteMethod: TBaseExecuteMethod; ATarget: PStreamArrayOfTDataSet);
begin
  FExecuteMethod  := AExecuteMethod;
  FDataSet        := ADataSet;
  FTarget         := ATarget;
  FOwner := AOwner;

  FreeOnTerminate := True;
//  FreeOnTerminate := False; //TEST(1)

  inherited Create(True);

  Start;
end;

destructor TBaseDataSetThreadProcess.Destroy;
begin
  if not Finished then
    FDataSet.BreakExec;

  inherited;
end;

procedure TBaseDataSetThreadProcess.Execute;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  FOnInfoMessage := FOwner.FOwner.ConnectionProvider.OnInfoMessage;
  try
    FOwner.FOwner.ConnectionProvider.OnInfoMessage := SQLConnectionProviderInfoMessage;

    case FExecuteMethod of
      bemExecute  :
        FDataSet.Execute;

      bemOpen     :
        FDataSet.Open;

      bemOpenNext, bemOpenFill:
        FDataSet.OpenNext;

      bemRefresh  :
        begin
          FDataSet.Close;
          FDataSet.Open;
        end;
    end;

  except on E:Exception do
    FOwner.FErrorMessage := E.Message
  end;

  FOwner.FOwner.ConnectionProvider.OnInfoMessage := FOnInfoMessage;

  CoUninitialize;

//  if (not Finished) then begin
//    Terminate;
//  if (FOwner.FHandle <> 0) then
  if (FOwner.FTerminating) then
    Synchronize(FOwner.InternalEndTerminating)
  else
    Synchronize(FOwner.InternalEndExecute);
//    PostMessage(FOwner.FHandle, WM_BaseDataSetThreadFinished, Integer(Self), 0);
//  end
end;

function TBaseDataSetThreadProcess.InternalQuery: TInternalQuery;
begin
  Result := TInternalQuery(FDataSet);
end;

procedure TBaseDataSetThreadProcess.SQLConnectionProviderInfoMessage(AConnection: TInternalConnection; const AInfoMessage: string);
begin
  FOwner.FInfoMessages.Add(AInfoMessage);
end;

{ TBaseDataSetFieldAutomations }

procedure TBaseDataSetFieldAutomations.AddServiceFields(const AFields: String);
begin
  if not AFields.IsEmpty then
    ServiceFields := ArraysMerge([ServiceFields, AFields], ',');
end;

function TBaseDataSetFieldAutomations.AllUpdateableFields: String;
var
  I: Integer;
  LServiceFields: String;
begin
  LServiceFields := ArraysMerge([FServiceFields, FModifiedFieldsFieldName, FModifiAbleFieldsFieldName, StringReplace(LServiceFields, ',', ';', [rfReplaceAll, rfIgnoreCase])], ',');
  Result := '';
  for I := 0 to FDataSetProvider.FieldCount - 1 do
    with FDataSetProvider.Fields[I] do
      if not InArray(LServiceFields, FieldName, ';') then
        Result := ConcatIfNotEmpty(Result, ';', FieldName)
end;

procedure TBaseDataSetFieldAutomations.AssignTo(Dest: TPersistent);
begin
  TBaseDataSetFieldAutomations(Dest).BeginUpdate;
  try
//    TBaseDataSetFieldAutomations(Dest).AllFieldsModifiedIfChanged := AllFieldsModifiedIfChanged;
    TBaseDataSetFieldAutomations(Dest).InitializeAbleFields       := InitializeAbleFields;
    TBaseDataSetFieldAutomations(Dest).InsertAbleFields           := InsertAbleFields;
    TBaseDataSetFieldAutomations(Dest).ModifiAbleFieldsFieldName  := ModifiAbleFieldsFieldName;
    TBaseDataSetFieldAutomations(Dest).ModifiedFieldsFieldName    := ModifiedFieldsFieldName;
    TBaseDataSetFieldAutomations(Dest).UpdateAbleFields           := UpdateAbleFields;
    TBaseDataSetFieldAutomations(Dest).ServiceFields              := ServiceFields;

    TBaseDataSetFieldAutomations(Dest).TrimRightFields            := TrimRightFields;
    TBaseDataSetFieldAutomations(Dest).TrimLeftFields             := TrimLeftFields;
    TBaseDataSetFieldAutomations(Dest).NullIfEmptyFields          := NullIfEmptyFields;
  finally
    TBaseDataSetFieldAutomations(Dest).EndUpdate;
  end;
end;

constructor TBaseDataSetFieldAutomations.Create(ADataSetProvider: TCustomBaseDataSetProvider);
begin
  FDataSetProvider := ADataSetProvider;
  FInsertAbleFields := '*';
  FUpdateAbleFields := '*';

  inherited Create;
end;

procedure TBaseDataSetFieldAutomations.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineProperty('AddServiceFields', ReadAddServiceFields, nil, False);
end;

procedure TBaseDataSetFieldAutomations.ReadAddServiceFields(Reader: TReader);
begin
  AddServiceFields(Reader.ReadString);
end;

procedure TBaseDataSetFieldAutomations.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TBaseDataSetFieldAutomations.EndUpdate;
begin
  if FUpdateCount > 0 then begin
    Dec(FUpdateCount);
    if (FUpdateCount = 0) and FUpdated and FDataSetProvider.Active
    then
      FDataSetProvider.DoDataSetFieldsUpdateAble(True);
  end;
end;

function TBaseDataSetFieldAutomations.GetOwner: TPersistent;
begin
  Result := FDataSetProvider;
end;

function TBaseDataSetFieldAutomations.IsInsertAbleFieldsStored: Boolean;
begin
  Result := FInsertAbleFields <> '*';
end;

function TBaseDataSetFieldAutomations.IsUpdateAbleFieldsStored: Boolean;
begin
  Result := FUpdateAbleFields <> '*';
end;

function TBaseDataSetFieldAutomations.IsUpdateAbleAlgorithmActive: Boolean;
begin
  Result := IsInsertAbleFieldsStored
         or IsUpdateAbleFieldsStored
         or (not FInitializeAbleFields.IsEmpty)
         or (not FModifiAbleFieldsFieldName.IsEmpty);
end;

procedure TBaseDataSetFieldAutomations.SetInsertAbleFields(const Value: String);
begin
  if FInsertAbleFields = Value then Exit;

  FInsertAbleFields := Value;
  FUpdateAbleAlgorithmActive := IsUpdateAbleAlgorithmActive;
  if FDataSetProvider.Active and (FDataSetProvider.Inserted) then
    FDataSetProvider.DoDataSetFieldsUpdateAble(True);
end;

procedure TBaseDataSetFieldAutomations.SetModifiAbleFieldsFieldName(const Value: String);
begin
  if FModifiAbleFieldsFieldName = Value then Exit;

  FModifiAbleFieldsFieldName := Value;
  if (not FModifiAbleFieldsFieldName.IsEmpty) then begin
    if (not IsInsertAbleFieldsStored)
        and (not IsUpdateAbleFieldsStored)
        and (FInitializeAbleFields.IsEmpty)
    then begin
      FInsertAbleFields := '';
      FUpdateAbleFields := '';
    end;
  end;

  FUpdateAbleAlgorithmActive := IsUpdateAbleAlgorithmActive;
  if FDataSetProvider.Active then
    FDataSetProvider.DoDataSetFieldsUpdateAble(True);
end;

procedure TBaseDataSetFieldAutomations.SetInitializeAbleFields(const Value: String);
begin
  if FInitializeAbleFields = Value then Exit;

  FInitializeAbleFields := Value;
  FUpdateAbleAlgorithmActive := IsUpdateAbleAlgorithmActive;
  if FDataSetProvider.Active and (not FDataSetProvider.Inserted) then
    FDataSetProvider.DoDataSetFieldsUpdateAble(True);
end;

procedure TBaseDataSetFieldAutomations.SetUpdateAbleFields(const Value: String);
begin
  if FUpdateAbleFields = Value then Exit;

  FUpdateAbleFields := Value;
  FUpdateAbleAlgorithmActive := IsUpdateAbleAlgorithmActive;
  if FDataSetProvider.Active and (not FDataSetProvider.Inserted) then
    FDataSetProvider.DoDataSetFieldsUpdateAble(True);
end;

{ TCustomBaseDataSetProvider }

constructor TCustomBaseDataSetProvider.Create(AOwner: TComponent);
begin
  FCachedUpdates      := True;
  FUpdateStatusFilter := Default_UpdateStatusFilter;
  FFieldsAutomations  := TBaseDataSetFieldAutomations.Create(Self);

  FDataSource := TDataSource.Create(Self);
  FDataSource.OnDataChange := DataSourceDataChange;
  FDataSource.OnStateChange:= DataSourceStateChange;

  inherited;
end;

function TCustomBaseDataSetProvider.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := CheckDataSet('CreateBlobStream').CreateBlobStream(Field, Mode);
end;

destructor TCustomBaseDataSetProvider.Destroy;
begin
  Close;
  inherited;
  FreeAndNil(FFieldsAutomations);
end;

procedure TCustomBaseDataSetProvider.InternalSetDataSet(const AFrom, ATo: TDataSet);
begin
//
end;

procedure TCustomBaseDataSetProvider.SetDataSet(const AValue: TDataSet);
{$IFNDEF PACKAGE}
var
  LDataSet, LDataSetFrom, LDataSetTo: TDataSet;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  LDataSet := FDataSource.DataSet;

  if LDataSet <> AValue then begin
    if (AValue <> nil) and (AValue is TCustomBaseDataSetProvider) then
      Raise Exception.Create(SUnSupportedObject);

    if LDataSet <> nil then begin
      LDataSetFrom := LDataSet;

      DetachDataSetOptions(LDataSetFrom);
    end else begin
      LDataSetFrom := Self;
    end;

    if AValue <> nil then begin
      LDataSetTo := AValue;

      AttachDataSetOptions(LDataSetTo);
    end else begin
      LDataSetTo := Self;
    end;

    FDataSource.DataSet := AValue;
    if (not LDataSetFrom.Active) and (not LDataSetTo.Active) then begin
      if TDataSetCrack(LDataSetFrom).GetFFields.Count > 0 then begin
        TDataSetCrack(LDataSetTo).GetFFields.Clear;
        with TDataSetCrack(LDataSetFrom).GetFFields do
          while Count > 0 do
            Fields[0].DataSet := LDataSetTo;
      end;

      if TDataSetCrack(LDataSetFrom).GetFAggFields.Count > 0 then begin
        TDataSetCrack(LDataSetTo).GetFAggFields.Clear;
        with TDataSetCrack(LDataSetFrom).GetFAggFields do
          while Count > 0 do
            AggFields[0].DataSet := LDataSetTo;
      end;
    end;

    InternalSetDataSet(LDataSet, AValue);

    while TDataSetCrack(LDataSetFrom).FDataSources.Count > 0 do
      TDataSetCrack(LDataSetFrom).FDataSources[0].DataSet := LDataSetTo;
  end;
{$ELSE}
  FDataSource.DataSet := AValue;
{$ENDIF}
end;

procedure TCustomBaseDataSetProvider.AttachDataSetOptions(const ADataSet: TDataSet);
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  with ADataSet do begin
    if GetUpdateStatusFilter <> FUpdateStatusFilter then
      SetUpdateStatusFilter(FUpdateStatusFilter);

    if Filter <> (inherited {$IFNDEF PACKAGE}GetFilterText{$ELSE}Filter{$ENDIF}) then
      Filter         := inherited {$IFNDEF PACKAGE}GetFilterText{$ELSE}Filter{$ENDIF};

    if FilterOptions <> (inherited {$IFNDEF PACKAGE}GetFilterOptions{$ELSE}FilterOptions{$ENDIF}) then
      FilterOptions  := inherited {$IFNDEF PACKAGE}GetFilterOptions{$ELSE}FilterOptions{$ENDIF};
{$IFNDEF PACKAGE}
    if TMethod(OnFilterRecord) <> TMethod(inherited OnFilterRecord) then
      OnFilterRecord := inherited OnFilterRecord;
{$ENDIF}
    if Filtered <> (inherited {$IFNDEF PACKAGE}GetFiltered{$ELSE}Filtered{$ENDIF}) then
      Filtered       := inherited {$IFNDEF PACKAGE}GetFiltered{$ELSE}Filtered{$ENDIF};
  end;

  Supports(ADataSet, IBaseDataSetProvider, LBaseDataSetProvider);
  with LBaseDataSetProvider do begin
    if GetMasterSource <> FMasterSource then
      SetMasterSource(FMasterSource);

    if GetMasterFields <> FMasterFields then
      SetMasterFields(FMasterFields);

    if GetDetailFields <> FDetailFields then
      SetDetailFields(FDetailFields);

    if GetIndexFieldNames <> FIndexFieldNames then
      SetIndexFieldNames(FIndexFieldNames);

    if GetKeyFieldNames <> FKeyFieldNames then
      SetKeyFieldNames(FKeyFieldNames);

    if GetReadOnly <> FReadOnly then
      SetReadOnly(FReadOnly);

    if GetCachedUpdates <> FCachedUpdates then
      SetCachedUpdates(FCachedUpdates);
  end;
end;

procedure TCustomBaseDataSetProvider.DetachDataSetOptions(const ADataSet: TDataSet);
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  with ADataSet do begin
    inherited SetFilterText(Filter);
    inherited SetFilterOptions(FilterOptions);
    inherited SetOnFilterRecord(OnFilterRecord);
    inherited SetFiltered(Filtered);
  end;

  Supports(ADataSet, IBaseDataSetProvider, LBaseDataSetProvider);
  with LBaseDataSetProvider do begin
    FMasterSource       := GetMasterSource;
    FMasterFields       := GetMasterFields;
    FDetailFields       := GetDetailFields;
    FIndexFieldNames    := GetIndexFieldNames;
    FKeyFieldNames      := GetKeyFieldNames;
    FCachedUpdates      := CachedUpdates;
    FUpdateStatusFilter := UpdateStatusFilter;
  end;
end;

{ Новые приватные методы обработки свойств }

function TCustomBaseDataSetProvider.GetActive: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;
  Result := (LDataSet <> nil) and LDataSet.Active;
end;

procedure TCustomBaseDataSetProvider.InternalSetActive(Value: Boolean);
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if Assigned(LDataSet) then begin
    if Value then
      FBeforeOpenTime := Now();
    LDataSet.Active := Value;
    if LDataSet.Active then
      FOpenDuration := MilliSecondsBetween(FBeforeOpenTime, Now());
  end else if Value then
    DataSetNotSet('SetActive(True)');
end;

procedure TCustomBaseDataSetProvider.InternalSetCachedUpdates(AValue: Boolean);
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    FCachedUpdates := AValue
  else
    LBaseDataSetProvider.SetCachedUpdates(AValue);
end;

procedure TCustomBaseDataSetProvider.SetActive(Value: Boolean);
begin
  if (csReading in ComponentState) then
    inherited
  else if (GetActive <> Value) then
    InternalSetActive(Value);
end;

function TCustomBaseDataSetProvider.GetAggFields: TFields;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := inherited {$IFNDEF PACKAGE}GetAggFields{$ELSE}AggFields{$ENDIF}
  else
    Result := LDataSet.AggFields;
end;

function TCustomBaseDataSetProvider.GetAutoCalcFields: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := inherited {$IFNDEF PACKAGE}GetAutoCalcFields{$ELSE}AutoCalcFields{$ENDIF}
  else
    Result := LDataSet.AutoCalcFields;
end;

procedure TCustomBaseDataSetProvider.SetAutoCalcFields(const Value: Boolean);
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    inherited {$IFNDEF PACKAGE}SetAutoCalcFields(Value){$ELSE}AutoCalcFields := Value{$ENDIF}
  else
    LDataSet.AutoCalcFields := Value;
end;

function TCustomBaseDataSetProvider.GetFieldDefList: TFieldDefList;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := inherited {$IFNDEF PACKAGE}GetFieldDefList{$ELSE}FieldDefList{$ENDIF}
  else
    Result := LDataSet.FieldDefList;
end;

function TCustomBaseDataSetProvider.GetFieldDefs: TFieldDefs;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := inherited {$IFNDEF PACKAGE}GetFieldDefs{$ELSE}FieldDefs{$ENDIF}
  else
    Result := LDataSet.FieldDefs;
end;

function TCustomBaseDataSetProvider.GetFields: TFields;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := inherited {$IFNDEF PACKAGE}GetFields{$ELSE}Fields{$ENDIF}
  else
    Result := LDataSet.Fields;
end;

function TCustomBaseDataSetProvider.GetIndexFieldNames: String;
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    Result := FIndexFieldNames
  else
    Result := LBaseDataSetProvider.GetIndexFieldNames;
end;

procedure TCustomBaseDataSetProvider.SetIndexFieldNames(const AValue: String);
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    FIndexFieldNames := AValue
  else
    LBaseDataSetProvider.SetIndexFieldNames(AValue);
end;

function TCustomBaseDataSetProvider.GetIsUniDirectional: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := inherited {$IFNDEF PACKAGE}GetIsUniDirectional{$ELSE}IsUniDirectional{$ENDIF}
  else
    Result := LDataSet.IsUniDirectional;
end;

{
function TCustomBaseDataSetProvider.GetKeyFieldNames: String;
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    Result := FKeyFieldNames
  else
    Result := LBaseDataSetProvider.GetKeyFieldNames
end;
}

procedure TCustomBaseDataSetProvider.SetKeyFieldNames(const AValue: String);
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    FKeyFieldNames := AValue
  else
    LBaseDataSetProvider.SetKeyFieldNames(AValue)
end;

function TCustomBaseDataSetProvider.GetCachedUpdates: Boolean;
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    Result := FCachedUpdates
  else
    Result := LBaseDataSetProvider.GetCachedUpdates;
end;

function TCustomBaseDataSetProvider.GetCanModify: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := False
  else
    Result := (not FReadOnly) and LDataSet.CanModify;
end;

function TCustomBaseDataSetProvider.GetBlockReadSize: Integer;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := inherited {$IFNDEF PACKAGE}GetBlockReadSize{$ELSE}BlockReadSize{$ENDIF}
  else
    Result := LDataSet.BlockReadSize;
end;

procedure TCustomBaseDataSetProvider.SetBlockReadSize(Value: Integer);
begin
  CheckDataSet('SetBlockReadSize').BlockReadSize := Value;
end;

procedure TCustomBaseDataSetProvider.SetCachedUpdates(const AValue: Boolean);
begin
  if CachedUpdates <> AValue then
    InternalSetCachedUpdates(AValue);
end;

function TCustomBaseDataSetProvider.GetBOF: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := True
  else
    Result := LDataSet.BOF;
end;

function TCustomBaseDataSetProvider.GetEOF: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := True
  else
    Result := LDataSet.EOF;
end;

function TCustomBaseDataSetProvider.GetActiveDataSet: TDataSet;
begin
  Result := FDataSource.DataSet;
end;

//function TCustomBaseDataSetProvider.DefaultFields: Boolean;
//var
//  LDataSet: TDataSet;
//begin
//  LDataSet := ActiveDataSet;
//
//  if LDataSet = nil then
//    Result := inherited DefaultFields
//  else
//    Result := LDataSet.DefaultFields;
//end;

function TCustomBaseDataSetProvider.GetFiltered: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := inherited {$IFNDEF PACKAGE}GetFiltered{$ELSE}Filtered{$ENDIF}
  else
    Result := LDataSet.Filtered;
end;

procedure TCustomBaseDataSetProvider.SetFieldsAutomations(const Value: TBaseDataSetFieldAutomations);
begin
  FFieldsAutomations.Assign(Value);
end;

procedure TCustomBaseDataSetProvider.SetFiltered(Value: Boolean);
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    inherited SetFiltered(Value)
  else
    LDataSet.Filtered := Value;
end;

function TCustomBaseDataSetProvider.GetFilterOptions: TFilterOptions;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := inherited {$IFNDEF PACKAGE}GetFilterOptions{$ELSE}FilterOptions{$ENDIF}
  else
    Result := LDataSet.FilterOptions;
end;

procedure TCustomBaseDataSetProvider.SetFilterOptions(Value: TFilterOptions);
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    inherited SetFilterOptions(Value)
  else
    LDataSet.FilterOptions := Value;
end;

function TCustomBaseDataSetProvider.GetFilterText: string;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := inherited {$IFNDEF PACKAGE}GetFilterText{$ELSE}Filter{$ENDIF}
  else
    Result := LDataSet.Filter;
end;

procedure TCustomBaseDataSetProvider.SetFilterText(const Value: string);
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    inherited SetFilterText(Value)
  else
    LDataSet.Filter := Value;
end;

function TCustomBaseDataSetProvider.GetFound: Boolean;
begin
  Result := CheckDataSet('GetFound').Found;
end;

function TCustomBaseDataSetProvider.GetDetailFields: String;
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    Result := FDetailFields
  else
    Result := LBaseDataSetProvider.GetDetailFields
end;

function TCustomBaseDataSetProvider.GetMasterFields: String;
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    Result := FMasterFields
  else
    Result := LBaseDataSetProvider.GetMasterFields
end;

function TCustomBaseDataSetProvider.GetMasterSource: TDataSource;
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    Result := FMasterSource
  else
    Result := LBaseDataSetProvider.GetMasterSource
end;

procedure TCustomBaseDataSetProvider.SetDetailFields(const AValue: String);
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    FDetailFields := AValue
  else
    LBaseDataSetProvider.SetDetailFields(AValue);
end;

procedure TCustomBaseDataSetProvider.SetMasterFields(const AValue: String);
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    FMasterFields := AValue
  else
    LBaseDataSetProvider.SetMasterFields(AValue)
end;

procedure TCustomBaseDataSetProvider.SetMasterSource(AValue: TDataSource);
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    FMasterSource := AValue
  else
    LBaseDataSetProvider.SetMasterSource(AValue);
end;

function TCustomBaseDataSetProvider.GetModified: Boolean;
begin
  Result := CheckDataSet('GetModified').Modified;
end;

function TCustomBaseDataSetProvider.GetObjectView: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := inherited {$IFNDEF PACKAGE}GetObjectView{$ELSE}ObjectView{$ENDIF}
  else
    Result := LDataSet.ObjectView;
end;

procedure TCustomBaseDataSetProvider.SetObjectView(const Value: Boolean);
begin
  CheckDataSet('SetObjectView').ObjectView := Value;
end;

procedure TCustomBaseDataSetProvider.SetOnFilterRecord(const Value: TFilterRecordEvent);
var
  LDataSet: TDataSet;
begin
  inherited;

  LDataSet := ActiveDataSet;
  if Assigned(LDataSet) then
    LDataSet.OnFilterRecord := Value;
end;

procedure TCustomBaseDataSetProvider.SetParentComponent(AParent: TComponent);
begin
  if AParent is TDataSource then
    TDataSource(AParent).DataSet := Self;
end;

function TCustomBaseDataSetProvider.GetSparseArrays: Boolean;
begin
  Result := CheckDataSet('GetSparseArrays').SparseArrays;
end;

procedure TCustomBaseDataSetProvider.SetSparseArrays(Value: Boolean);
begin
  CheckDataSet('SetSparseArrays').SparseArrays := Value;
end;

procedure TCustomBaseDataSetProvider.SetUpdateStatusFilter(AValue: TUpdateStatusSet);
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    FUpdateStatusFilter := AValue
  else
    LBaseDataSetProvider.SetUpdateStatusFilter(AValue);
end;

function TCustomBaseDataSetProvider.SupportedMethods: TBaseDataSetSupportedActions;
begin
  Result := [bdssaKeyFields];
end;

function TCustomBaseDataSetProvider.GetState: TDataSetState;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := dsInactive
  else
    Result := LDataSet.State;
end;

function TCustomBaseDataSetProvider.GetUpdateStatusFilter: TUpdateStatusSet;
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    Result := FUpdateStatusFilter
  else
    Result := LBaseDataSetProvider.GetUpdateStatusFilter
end;

{$IFNDEF PACKAGE}
function TCustomBaseDataSetProvider.GetXML: String;
var
  LBookmark: TDataSourceStateBookmark;
begin
  if IsEmpty then
    Result := ''
  else begin
    BeginUpdate(LBookmark);
    try
      First;
      Result := DataSetToXML(Self)
    finally
      EndUpdate(LBookmark, [sbrDisableCount], [sbrRecordIndex]);
    end;
  end;
end;
{$ENDIF}

function TCustomBaseDataSetProvider.GetReadOnly: Boolean;
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    Result := FReadOnly
  else
    Result := LBaseDataSetProvider.GetReadOnly
end;

function TCustomBaseDataSetProvider.GetRecNo: Longint;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := 0
  else
    Result := LDataSet.RecNo;
end;

procedure TCustomBaseDataSetProvider.SetReadOnly(const AValue: Boolean);
var
  LBaseDataSetProvider: IBaseDataSetProvider;
begin
  LBaseDataSetProvider := GetBaseDataSetProviderSupport;
  if LBaseDataSetProvider = nil then
    FReadOnly := AValue
  else
    LBaseDataSetProvider.SetReadOnly(AValue)
end;

procedure TCustomBaseDataSetProvider.SetRecNo(Value: Integer);
begin
  CheckDataSet('SetRecNo').RecNo := Value;
end;

function TCustomBaseDataSetProvider.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
begin
  Result := grError;
end;

function TCustomBaseDataSetProvider.GetRecordCount: Longint;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := 0
  else
    Result := LDataSet.RecordCount;
end;

function TCustomBaseDataSetProvider.MoveBy(Distance: Integer): Integer;
begin
  Result := CheckDataSet('MoveBy').MoveBy(Distance);
end;

function TCustomBaseDataSetProvider.GetRecordSize: Word;
begin
  Result := CheckDataSet('GetRecordSize').RecordSize;
end;

{ abstract methods required for all datasets }

procedure TCustomBaseDataSetProvider.InternalHandleException;
begin
{$IFNDEF PACKAGE}
{$IFNDEF REPORT_EXTERNAL}
  Application.HandleException(Self);
{$ENDIF}
{$ENDIF}
end;

procedure TCustomBaseDataSetProvider.InternalInitFieldDefs;
begin
//
end;

procedure TCustomBaseDataSetProvider.InternalOpen;
begin
//
end;

{$IFDEF DELPHIXE3}
procedure TCustomBaseDataSetProvider.InternalGotoBookmark(Bookmark: TBookmark);
begin
  CheckDataSet('InternalGotoBookmark').InternalGotoBookmark(Bookmark);
end;
{$ENDIF}
{$IFNDEF NEXTGEN}
{$WARN SYMBOL_DEPRECATED OFF}
procedure TCustomBaseDataSetProvider.InternalClose;
begin
//
end;

procedure TCustomBaseDataSetProvider.InternalGotoBookmark(Bookmark: Pointer);
begin
  CheckDataSet('InternalGotoBookmark').InternalGotoBookmark(Bookmark);
end;
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}

function TCustomBaseDataSetProvider.IsCursorOpen: Boolean;
begin
  Result := False;
end;

function TCustomBaseDataSetProvider.GetBaseDataSetProviderSupport: IBaseDataSetProvider;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;
  if Assigned(LDataSet) then
    Supports(LDataSet, IBaseDataSetProvider, Result)
  else
    Result := nil
end;

{ Собственные защищённые методы }

//procedure TCustomBaseDataSetProvider.DataSetAfterPost;
//begin
//
//end;

procedure TCustomBaseDataSetProvider.DataSetAfterScroll;
begin
  if FFieldsAutomations.UpdateAbleAlgorithmActive then
    DoDataSetFieldsUpdateAble(False);
end;

procedure TCustomBaseDataSetProvider.DoDataSetFieldsUpdateAble(AForce: Boolean);
var
  I: Integer;
  LUpdateable, LInitializeable: String;
  LInserted, LReadOnly: Boolean;
  LDataSet: TDataSet;

  label LABEL_DONE;
begin
  LDataSet := ActiveDataSet;
  if (LDataSet = nil) or (LDataSet.IsEmpty) or (not LDataSet.CanModify)
            or not (AForce or FFieldsAutomations.UpdateAbleAlgorithmActive)
  then
    Exit;

  if FFieldsAutomations.FUpdateCount > 0 then begin
    FFieldsAutomations.FUpdated := True;
    Exit;
  end;

  LInserted := LDataSet.Inserted;
  if FFieldsAutomations.ModifiAbleFieldsFieldName <> '' then begin
    with FieldByName(FFieldsAutomations.ModifiAbleFieldsFieldName) do
      // TODO: А правильно ли это?
      // Да. При копировании в New так срабатывает DoNewObject
//      if IsNull and (LDataSet.State = dsInsert) then
//        LUpdateable := '*'
//      else
        LUpdateable := AsString;
  end else
    LUpdateable := '';

  if LInserted then begin
    LUpdateable := ArraysMerge([LUpdateable, FFieldsAutomations.InsertAbleFields], ',');
    LInitializeable := '';
  end else begin
    LUpdateable := ArraysMerge([LUpdateable, FFieldsAutomations.UpdateAbleFields], ',');
    LInitializeable := FFieldsAutomations.InitializeAbleFields;
  end;

  if (not AForce) and (LUpdateable = FUpdateableFields) and (LInitializeable = FInitializeableFields) then
    goto LABEL_DONE;

  FUpdateableFields     := LUpdateable;
  FInitializeableFields := LInitializeable;

  for I := 0 to LDataSet.FieldCount - 1 do
    with LDataSet.Fields[I] do
      if (not SameText(FieldName, FFieldsAutomations.ModifiedFieldsFieldName))
        and (not SameText(FieldName, FFieldsAutomations.ModifiAbleFieldsFieldName))
        and (not InArray(FFieldsAutomations.ServiceFields, FieldName, ','))
      then begin
        LReadOnly := not InArray(LUpdateable, FieldName, ',');
        if LReadOnly and (not LInserted) and IsNull and (not LInitializeable.IsEmpty) then
          LReadOnly := not InArray(LInitializeable, FieldName, ',');
        ReadOnly := LReadOnly;
      end;

LABEL_DONE:
  FFieldsAutomations.FUpdated := False;
end;

procedure TCustomBaseDataSetProvider.DataSetBeforePost;
var
  LDataSet: TDataSet;
  LFields, LServiceFields: String;
  LModified: Boolean;
  I: Integer;
  LOldValue: Variant;

  LItem: TStringItem;
  LString: String;
  LChanged: Boolean;
begin
  LDataSet := ActiveDataSet;
  FBeforePostState := LDataSet.State;

  LFields := ArraysMerge
            (
              [FFieldsAutomations.TrimLeftFields, FFieldsAutomations.TrimRightFields, FFieldsAutomations.NullIfEmptyFields],
              ','
            );

  if not LFields.IsEmpty then
    for LItem in EnumStringItems(LFields, ',') do begin
      with LDataSet.FieldByName(LItem.Value) do if (not IsNull) and ((DataType <> ftVariant) or VarIsStr(Value)) then begin
        LString := AsString;
        LChanged := False;
        if (LString.Length > 0) and InArray(FFieldsAutomations.TrimRightFields, LItem.Value, ',') and (LString[Length(LString)] < ' ') then begin
          LString := SBaseStringFunctions.TrimRight(LString, ' ');
          LChanged := True;
        end;

        if (LString.Length > 0) and InArray(FFieldsAutomations.TrimLeftFields, LItem.Value, ',') and (LString[1] < ' ') then begin
          LString := SBaseStringFunctions.TrimLeft(LString, ' ');
          LChanged := True;
        end;

        if LString.IsEmpty and InArray(FFieldsAutomations.NullIfEmptyFields, LItem.Value, ',') then
          Clear
        else if LChanged then
          AsString := LString;
      end;
    end;


  if (not FFieldsAutomations.ModifiedFieldsFieldName.IsEmpty) then begin
    LFields := '';

    LServiceFields := ArraysMerge
                      (
                        [
                          FFieldsAutomations.ServiceFields,
                          FFieldsAutomations.ModifiedFieldsFieldName,
                          FFieldsAutomations.ModifiAbleFieldsFieldName
                        ],
                        ','
                      );
    for I := 0 to LDataSet.FieldCount - 1 do
      with LDataSet.Fields[I] do
        if (not InArray(LServiceFields, FieldName, ',')) and CanModify then begin
          LOldValue := OldValue;
          LModified := not VarIsEqual(LOldValue, AsVariant);

          if LModified then
            LFields := ConcatIfNotEmpty(LFields, ',', FieldName);
        end;

    with LDataSet.FieldByName(FFieldsAutomations.ModifiedFieldsFieldName) do
      if AsString <> LFields then
        if (not LFields.IsEmpty) then
          AsString := LFields
        else
          Clear;
  end;
end;

procedure TCustomBaseDataSetProvider.RaiseDataSetException(const AException: String);
begin
  Raise Exception.Create(Name + ': ' + AException);
end;

procedure TCustomBaseDataSetProvider.DataSetNotSet(const AMethod: String);
begin
  RaiseDataSetException('Method ' + AMethod + ' is not available, while DataSet property is not set.');
end;

procedure TCustomBaseDataSetProvider.DataSourceDataChange(Sender: TObject; Field: TField);
begin
  if (FFieldsAutomations.ModifiAbleFieldsFieldName <> '')
        and
        (
          (Field = nil)
          or
          SameText(Field.FieldName, FFieldsAutomations.ModifiAbleFieldsFieldName)
        )
  then
    DoDataSetFieldsUpdateAble(Field = nil);
end;

procedure TCustomBaseDataSetProvider.DataSourceStateChange(Sender: TObject);
begin
  if (State = dsBrowse) then
    DoDataSetFieldsUpdateAble(True);
end;

function TCustomBaseDataSetProvider.CheckDataSet(const AMethod: String): TDataSetCrack;
begin
  Result := TDataSetCrack(ActiveDataSet);
  if Result = nil then
    DataSetNotSet(AMethod);
end;

function TCustomBaseDataSetProvider.CheckBaseDataSetProviderSupport(const AMethod: String): IBaseDataSetProvider;
var
  LDataSet: TDataSetCrack;
begin
  LDataSet := CheckDataSet(AMethod);
  if Assigned(LDataSet) then begin
    Supports(LDataSet, IBaseDataSetProvider, Result);
    if Result = nil then
      RaiseDataSetException('Method ' + AMethod + ' is not available, because DataSet does not support IBaseDataSetProvider interface.');
  end;
end;

{ Переопределённые защищённые методы базового класса TDataSet }

{$IFNDEF PACKAGE}
procedure TCustomBaseDataSetProvider.AddDataSource(DataSource: TDataSource);
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if (DataSource <> FDataSource) and (LDataSet <> nil) then
    DataSource.DataSet := LDataSet
  else
    inherited;
end;
{$ENDIF}

{ Переопределённые публичные методы базового класса TDataSet, необходимость реализации которых под сомнением }

{$WARNINGS OFF}
function TCustomBaseDataSetProvider.GetCurrentRecord(Buffer: TRecordBuffer): Boolean;
begin
  Result := CheckDataSet('GetCurrentRecord').GetCurrentRecord(Buffer);
end;
{$WARNINGS ON}

{
function TCustomBaseDataSetProvider.GetBookmarkStr: TBookmarkStr;
begin
  CheckDataSet('GetBookmarkStr');
  Result := FDataSet.GetBookmarkStr;
end;
}

{$WARNINGS OFF}
procedure TCustomBaseDataSetProvider.BlockReadNext;
begin
  CheckDataSet('BlockReadNext').BlockReadNext;
end;

function TCustomBaseDataSetProvider.BookmarkValid(Bookmark: TBookmark): Boolean;
{$WARNINGS ON}
begin
  Result := CheckDataSet('BookmarkValid').BookmarkValid(Bookmark);
end;

{$WARNINGS OFF}
procedure TCustomBaseDataSetProvider.CommitUpdates;
begin
  InternalCommitUpdates;
end;

procedure TCustomBaseDataSetProvider.InternalCommitUpdates;
begin
  CheckBaseDataSetProviderSupport('CommitUpdates').CommitUpdates;
end;

function TCustomBaseDataSetProvider.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
{$WARNINGS ON}
begin
  Result := CheckDataSet('CompareBookmarks').CompareBookmarks(Bookmark1, Bookmark2);
end;

{$WARNINGS OFF}
procedure TCustomBaseDataSetProvider.FreeBookmark(Bookmark: TBookmark);
{$WARNINGS ON}
begin
  CheckDataSet('FreeBookmark').FreeBookmark(Bookmark);
end;

{ Переопределённые публичные методы базового класса TDataSet }

{$IFDEF DELPHIXE3}
procedure TCustomBaseDataSetProvider.GetDetailDataSets(List: TList<TDataSet>);
begin
  CheckDataSet('GetDetailDataSets').GetDetailDataSets(List);
end;
{$ENDIF}

{$IFNDEF NEXTGEN}
{$WARN SYMBOL_DEPRECATED OFF}
{$IFNDEF PACKAGE}
function TCustomBaseDataSetProvider.GetDataSources: TList<TDataSource>;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;
  if Assigned(LDataSet) then
    Result := LDataSet.DataSources
  else
    Result := FDataSources
end;
{$ENDIF}

procedure TCustomBaseDataSetProvider.GetDetailDataSets(AList: TList);
begin
  CheckDataSet('GetDetailDataSets').GetDetailDataSets(AList);
end;
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}

{$IFDEF DELPHIXE3}
procedure TCustomBaseDataSetProvider.GetDetailLinkFields(MasterFields, DetailFields: TList<TField>);
begin
  CheckDataSet('GetDetailLinkFields').GetDetailLinkFields(MasterFields, DetailFields);
end;
{$ENDIF}
{$IFNDEF NEXTGEN}
{$WARN SYMBOL_DEPRECATED OFF}
procedure TCustomBaseDataSetProvider.GetDetailLinkFields(AMasterFields, ADetailFields: TList);
begin
  CheckDataSet('GetDetailLinkFields').GetDetailLinkFields(AMasterFields, ADetailFields);
end;
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}

procedure TCustomBaseDataSetProvider.RestoreUpdates;
begin
  CheckBaseDataSetProviderSupport('RestoreUpdates').RestoreUpdates;
end;

procedure TCustomBaseDataSetProvider.Resync(Mode: TResyncMode);
begin
  CheckDataSet('Resync').Resync(Mode);
end;

procedure TCustomBaseDataSetProvider.RevertRecord;
begin
  CheckBaseDataSetProviderSupport('RevertRecord').RevertRecord;
end;

function TCustomBaseDataSetProvider.UpdateStatus: TUpdateStatus;
begin
  Result := CheckDataSet('UpdateStatus').UpdateStatus;
end;

function TCustomBaseDataSetProvider.VariantToFilterString(const AValue: Variant): String;
//var
//  LDate    : Extended;
begin
  case VarType(AValue) of
    varNull, varEmpty:
      Result := SConst_NULL;
    varBoolean           : Result := String(AnsiChar(Byte('0') + Integer(Boolean(AValue))));

    varByte, varSmallint, varShortInt, varWord, varInteger, varLongWord, varInt64,
    varSingle, varDouble, varCurrency:
      Result := VarToStr(AValue);

//                         : Result := IntToStr(AValue);
//                         : Result := FloatToStr(AValue);
//    varDate              :
//      begin
//        LDate := Int(AValue);
//        if LDate = 0.0 then
//          Result := DataBaseTimeFormat(AValue, not ABinaryStringFormat)
//        else if LDate <> AValue then
//          Result := DataBaseDateTimeFormat(AValue, not ABinaryStringFormat)
//        else
//          Result := DataBaseDateFormat(AValue, not ABinaryStringFormat);
//      end;

    varDate:
      Result := QuotedStr(VarToStr(AValue));
    varString, varOleStr, varUString :
      Result := QuotedStr(AValue);
    else
      Raise Exception.Create('Cannot convert variant to FilterString');
  end;
end;

procedure TCustomBaseDataSetProvider.GetFieldNames(List: TStrings);
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    inherited GetFieldNames(List)
  else
    LDataSet.GetFieldNames(List);
end;

function TCustomBaseDataSetProvider.IsSequenced: Boolean;
begin
  Result := CheckDataSet('IsSequenced').IsSequenced;
end;

function TCustomBaseDataSetProvider.KeyFieldValues: Variant;
var
  LDataSet: TDataSet;
begin
  LDataSet := CheckDataSet('KeyFieldValues');
  if (FKeyFieldNames.IsEmpty) or (LDataSet.IsEmpty) then
    Result := Unassigned
  else
    Result := LDataSet.FieldValues[FKeyFieldNames];
end;

//procedure TCustomBaseDataSetProvider.Loaded;
//begin
//  inherited;
//  Active := FLoadedActive;
//end;

function TCustomBaseDataSetProvider.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  Result := CheckDataSet('Locate').Locate(KeyFields, KeyValues, Options);
end;

function TCustomBaseDataSetProvider.LocateEx(const KeyFields: string; const KeyValues: Variant; Options: TBaseLocateOptions): Boolean;
begin
  Result := CheckBaseDataSetProviderSupport('LocateEx').LocateEx(KeyFields, KeyValues, Options)
end;

function TCustomBaseDataSetProvider.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
begin
  Result := CheckDataSet('Lookup').Lookup(KeyFields, KeyValues, ResultFields);
end;

(*
function TCustomBaseDataSetProvider.GetBlobFieldData(FieldNo: Integer; var Buffer: TBlobByteData): Integer;
begin
  Result := CheckDataSet('GetBlobFieldData').GetBlobFieldData(FieldNo, Buffer);
end;
*)

procedure TCustomBaseDataSetProvider.Insert;
begin
  CheckDataSet('Insert').Insert;
end;

procedure TCustomBaseDataSetProvider.Append;
begin
  CheckDataSet('Append').Append;
end;

procedure TCustomBaseDataSetProvider.AppendEx(const KeyFields: string; const KeyValues: Variant);
var
  LDataSourceStateBookmark: TDataSourceStateBookmark;
begin
  BeginUpdate(LDataSourceStateBookmark);
  try
    UpdateStatusFilter := All_UpdateStatusFilter;
    if Locate(KeyFields, KeyValues, []) then
      if UpdateStatus = usDeleted then
        Edit
      else
        Raise Exception.Create('Primary key violation detected at AppendEx method')
    else
      Append;
  finally
    EndUpdate(LDataSourceStateBookmark, [sbrUpdateStatusFilter, sbrDisableCount], []);
  end;
end;

procedure TCustomBaseDataSetProvider.InsertRecord(const Values: array of const);
begin
  CheckDataSet('InsertRecord').InsertRecord(Values);
end;

procedure TCustomBaseDataSetProvider.AppendRecord(const Values: array of const);
begin
  CheckDataSet('AppendRecord').AppendRecord(Values);
end;

procedure TCustomBaseDataSetProvider.InternalApplyUpdates;
begin
  CheckBaseDataSetProviderSupport('ApplyUpdates').ApplyUpdates;
end;

procedure TCustomBaseDataSetProvider.ApplyUpdates;
begin
  InternalApplyUpdates;
end;

procedure TCustomBaseDataSetProvider.Edit;
begin
  CheckDataSet('Edit').Edit;
end;

function TCustomBaseDataSetProvider.Editing: Boolean;
var
  LDataSet: TDataSet;
begin
//  if Self = nil then Exit(False);
  LDataSet := ActiveDataSet;
  Result := Assigned(LDataSet) and LDataSet.Editing
end;

procedure TCustomBaseDataSetProvider.Delete;
begin
  CheckDataSet('Delete').Delete;
end;

procedure TCustomBaseDataSetProvider.Post;
begin
  CheckDataSet('Post').Post;
end;

function TCustomBaseDataSetProvider.PSGetKeyFields: string;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet <> nil then
    Result := TDataSetCrack(LDataSet).PSGetKeyFields
  else
    Result := FKeyFieldNames
end;

procedure TCustomBaseDataSetProvider.Cancel;
begin
  CheckDataSet('Cancel').Cancel
end;

procedure TCustomBaseDataSetProvider.CancelUpdates;
begin
  CheckBaseDataSetProviderSupport('CancelUpdates').CancelUpdates;
end;

function TCustomBaseDataSetProvider.ReadFieldList: TFieldList;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := inherited {$IFNDEF PACKAGE}ReadFieldList{$ELSE}FieldList{$ENDIF}
  else
    Result := LDataSet.FieldList;
end;

procedure TCustomBaseDataSetProvider.Refresh;
begin
  CheckDataSet('Refresh').Refresh;
end;

procedure TCustomBaseDataSetProvider.RefreshDetail;
begin
  CheckBaseDataSetProviderSupport('RefreshDetail').RefreshDetail;
end;

procedure TCustomBaseDataSetProvider.EnableControls;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet <> nil then
    LDataSet.EnableControls;
end;

procedure TCustomBaseDataSetProvider.DisableControls;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet <> nil then
    LDataSet.DisableControls;
end;

function TCustomBaseDataSetProvider.ControlsDisabled: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  Result := (LDataSet <> nil) and LDataSet.ControlsDisabled;
end;

procedure TCustomBaseDataSetProvider.CheckBrowseMode;
begin
  CheckDataSet('CheckBrowseMode').CheckBrowseMode;
end;

procedure TCustomBaseDataSetProvider.ClearFields;
begin
  CheckDataSet('ClearFields').ClearFields;
end;

procedure TCustomBaseDataSetProvider.UpdateRecord;
begin
  CheckDataSet('UpdateRecord').UpdateRecord;
end;

function TCustomBaseDataSetProvider.UpdatesPending: Boolean;
begin
  Result := CheckBaseDataSetProviderSupport('UpdatesPending').UpdatesPending;
end;

function TCustomBaseDataSetProvider.IsEmpty: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := True
  else
    Result := LDataSet.IsEmpty;
end;

function TCustomBaseDataSetProvider.IsFilterTextStored: Boolean;
begin
  Result := (GetFilterText <> '');
end;

function TCustomBaseDataSetProvider.IsLinkedTo(DataSource: TDataSource): Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if LDataSet = nil then
    Result := inherited IsLinkedTo(DataSource)
  else
    Result := LDataSet.IsLinkedTo(DataSource);
end;

procedure TCustomBaseDataSetProvider.CursorPosChanged;
begin
  CheckDataSet('CursorPosChanged').CursorPosChanged;
end;

procedure TCustomBaseDataSetProvider.UpdateCursorPos;
begin
  CheckDataSet('UpdateCursorPos').UpdateCursorPos;
end;

function TCustomBaseDataSetProvider.FindRecord(Restart, GoForward: Boolean): Boolean;
begin
  Result := CheckDataSet('FindRecord').FindRecord(Restart, GoForward);
end;

procedure TCustomBaseDataSetProvider.First;
begin
  CheckDataSet('First').First;
end;

procedure TCustomBaseDataSetProvider.Last;
begin
  CheckDataSet('Last').Last;
end;

{$WARNINGS OFF}
function TCustomBaseDataSetProvider.GetBookmark: TBookmark;
{$WARNINGS ON}
begin
  Result := CheckDataSet('GetBookmark').GetBookmark;
end;

{$WARNINGS OFF}
procedure TCustomBaseDataSetProvider.GotoBookmark(Bookmark: TBookmark);
{$WARNINGS ON}
begin
  CheckDataSet('GotoBookmark').GotoBookmark(Bookmark);
end;

{ TStreamArray }

constructor TStreamArray<T>.Create(const AItems: Array of T; AOutOfBoundAllowed: Boolean);
var
  I, L: Integer;
begin
  L := Length(AItems);
  SetLength(FItems, L);
  for I := 0 to L - 1 do
   FItems[I] := AItems[I];

  FOutOfBoundAllowed := AOutOfBoundAllowed;
  FIndex := 0;
end;

function TStreamArray<T>.Count: Integer;
begin
  Result := Length(FItems);
end;

function TStreamArray<T>.CurrentItem: T;
begin
  Assert(not OutOfBound);
  Result := FItems[FIndex]
end;

function TStreamArray<T>.OutOfBound: Boolean;
begin
  Result := (FIndex >= Count)
end;

function TStreamArray<T>.Eof: Boolean;
begin
  Result := (FIndex >= Count - 1)
end;

procedure TStreamArray<T>.Next(ARaiseIfOutbound: Boolean = True);
begin
  Assert(not (ARaiseIfOutbound and Eof));
  Inc(FIndex);
end;

{ TBaseDataSetThread }

constructor TBaseDataSetThread.Create(AOwner: TCustomSQLDataSetProvider);
begin
  FOwner := AOwner;
  FRaiseException := True;

  inherited Create; //(AOwner);

//  FHandle := Classes.AllocateHWnd(MessageThreadFinished);
end;

destructor TBaseDataSetThread.Destroy;
begin
  Stop;
  FreeAndNil(FInfoMessages);
//  Classes.DeallocateHWnd(FHandle);
  inherited;
end;

//procedure TBaseDataSetThread.MessageThreadFinished(var AMessage: TMessage);
//begin
//  with AMessage do
//    if (Msg = WM_BaseDataSetThreadFinished) and (Integer(FThreadProcess) = Integer(wParam)) then
//      try
//        InternalEndExecute
//      except
//        Application.HandleException(Self);
//      end
//    else
//      DefWindowProc(FHandle, Msg, wParam, lParam);
//end;

function TBaseDataSetThread.GetMessages: TStringList;
begin
  if FInfoMessages = nil then
    FInfoMessages := TStringList.Create;
  Result := FInfoMessages;
end;

procedure TBaseDataSetThread.InternalExecute(AExecuteMethod: TBaseExecuteMethod; ADataSetArrayPointer: PStreamArrayOfTDataSet);
begin
  if FOwner.FMainDataSet = nil then
    FOwner.DataSetNotSet('Thread.InternalExecute');

  if FOwner.DataSetObject in [bdsoCDSCached, bdsoMemory, bdsoCDSPublic] then
    Raise Exception.Create('Cannot call Thread.InternalExecute in memory stored mode');

  Stop;
  FTerminating := False;

  if AExecuteMethod <> bemOpenNext then
    FMultiDataSetIndex := 0;

  case AExecuteMethod of
    bemOpen:
      if FOwner.Active then
        Exit;
    bemOpenNext:
      if not FOwner.Active then
        AExecuteMethod := bemOpen
      else if Assigned(FOwner.BeforeClose) then
        FOwner.BeforeClose(FOwner.ActiveDataSet);
    bemOpenFill:
      if FOwner.Active and Assigned(FOwner.BeforeClose) then
        FOwner.BeforeClose(FOwner.ActiveDataSet);
    bemRefresh:
      if Assigned(FOwner.BeforeRefresh) then
        FOwner.BeforeRefresh(FOwner.ActiveDataSet);
    bemExecute:
      FOwner.Close;
  end;

  LastMessages.Clear;
  FErrorMessage  := '';
  FThreadProcess := TBaseDataSetThreadProcess.Create(Self, AExecuteMethod, ADataSetArrayPointer);
end;

procedure TBaseDataSetThread.InternalEndExecute;
var
  LDone: Boolean;
  LDataSet: TInternalQuery;
  LKeyFieldNames: String;
  LDataSetThreadResult: TDataSetThreadResult;
  LExecuteMethod: TBaseExecuteMethod;
  LTarget: PStreamArrayOfTDataSet;
  LDataSourceStateBookmark: TDataSourceStateBookmark;
begin
  try
    if FInfoMessages.Count > 0 then begin
      if Assigned(FOnPrint) then
        FOnPrint(TCustomSQLDataSetProvider(FOwner), FInfoMessages);
//      FInfoMessages.Clear;
    end;

    if not FErrorMessage.IsEmpty then begin
      LDone := False;

      if Assigned(FOnError) then
        FOnError(TCustomSQLDataSetProvider(FOwner), FErrorMessage, LDone);

      if LDone then
        Abort
      else
        Raise Exception.Create(FErrorMessage);
    end;

    LDataSet := FThreadProcess.InternalQuery;
    if FTerminating then begin
      LDataSet.Close;
      FTerminating := False;
      Exit;
    end;

    LExecuteMethod := FThreadProcess.FExecuteMethod;

    if LDataSet.Active then begin
      LDone := True;
      Inc(FMultiDataSetIndex);
      if Assigned(FOnValidate) then
        FOnValidate(TCustomSQLDataSetProvider(FOwner), LDataSet, FMultiDataSetIndex, LDone);

      if LDone then begin
//        if LDataSet.!asNextResultSet then
//        LDataSet.ClearNextResultSets;
        with FOwner do begin
          if (LExecuteMethod = bemOpenFill) then begin
            LTarget := FThreadProcess.FTarget;
            if (not LTarget.OutOfBound) then begin
              with LTarget.CurrentItem do begin
                Close;
                Load(LDataSet);
              end;
              FreeAndNil(FThreadProcess);
              LTarget.Next(False);
              FThreadProcess := TBaseDataSetThreadProcess.Create(Self, LDataSet, bemOpenFill, LTarget);
              Exit;
            end else if (not LTarget.OutOfBoundAllowed) {or LDataSet.!asNextResultSet} then
              Raise Exception.Create('Fill method failed: Too many recordsets returned');
          end;

          if FOwner.Active then begin
            if Assigned(FOwner.BeforeScroll) then
              FOwner.BeforeScroll(FOwner);

            LKeyFieldNames := KeyFieldNames;
            if (LKeyFieldNames.IsEmpty) or (not LDataSet.IsEmpty and not LDataSet.Locate(LKeyFieldNames, FOwner.FDataSource.DataSet.FieldValues[LKeyFieldNames], []))
            then
              if LDataSet.RecordCount > RecNo then
                LDataSet.RecNo := RecNo
              else
                LDataSet.Last;

            if Assigned(FOwner.AfterScroll) then
              FOwner.AfterScroll(LDataSet);

            LDataSetThreadResult := bdstrRefreshed;
          end else begin
            LDataSetThreadResult := bdstrOpened;

            if Assigned(BeforeOpen) then
              BeforeOpen(FOwner);
          end;
        end;

        if (LExecuteMethod = bemRefresh) then
          if Assigned(FOwner.FOnBeginUpdate) then
            FOwner.FOnBeginUpdate(FOwner, LDataSourceStateBookmark);
//          else
//            FOwner.ActiveDataSet.DisableControls;
        try
          TCustomSQLDataSetProvider(FOwner).SetDataSet(LDataSet);
        finally
          if (LExecuteMethod = bemRefresh) then
            if Assigned(FOwner.FOnEndUpdate) then
              FOwner.FOnEndUpdate(FOwner, LDataSourceStateBookmark)
//            else if not Assigned(FOwner.FOnBeginUpdate) then
//              FOwner.ActiveDataSet.EnableControls;
        end;

        if LDataSetThreadResult <> bdstrRefreshed then
          FOwner.SyncronizeOutputParameters(LDataSet);

//        if LDataSetThreadResult = bdstrRefreshed then begin
//          if Assigned(FOwner.AfterRefresh) then
//            FOwner.AfterRefresh(LDataSet);
//        end else begin
//          FOwner.SyncronizeOutputParameters(LDataSet);
//          if Assigned(FOwner.AfterOpen) then
//            FOwner.AfterOpen(LDataSet);
//        end;
      end else begin
        if LExecuteMethod <> bemOpenFill then
          LExecuteMethod := bemOpenNext;
        LTarget := FThreadProcess.FTarget;
        FreeAndNil(FThreadProcess);
        FThreadProcess := TBaseDataSetThreadProcess.Create(Self, LDataSet, LExecuteMethod, LTarget);
        Exit;
      end;
    end else if (LExecuteMethod = bemOpenFill) then
      Raise Exception.Create('Fill method failed: Too few recordsets returned')
    else
      LDataSetThreadResult := bdstrExecuted;
  except on E:Exception do begin
    LDataSetThreadResult := bdstrError;
    if (E.Message <> SOperationAborted) and FErrorMessage.IsEmpty then
      FErrorMessage := E.Message;
  end end;

  FThreadProcess := nil; // AUTOREF
  if Assigned(FAfterComplete) then
    FAfterComplete(TCustomSQLDataSetProvider(FOwner), LDataSetThreadResult);

  if (LDataSetThreadResult in [bdstrOpened, bdstrRefreshed]) and FOwner.ActiveDataSet.Active then
    FOwner.FOpenDuration := MilliSecondsBetween(FOwner.FBeforeOpenTime, Now());

  case LDataSetThreadResult of
    bdstrOpened:
      begin
        if Assigned(TCustomSQLDataSetProvider(FOwner).AfterOpen) then
          FOwner.AfterOpen(TCustomSQLDataSetProvider(FOwner));
      end;
    bdstrRefreshed:
      begin
        if Assigned(TCustomSQLDataSetProvider(FOwner).AfterRefresh) then
          TCustomSQLDataSetProvider(FOwner).AfterRefresh(TCustomSQLDataSetProvider(FOwner));
      end;
  end;
end;

procedure TBaseDataSetThread.InternalEndTerminating;
begin
  FThreadProcess := nil; // AUTOREF

  if Assigned(FAfterComplete) then
    FAfterComplete(TCustomSQLDataSetProvider(FOwner), bdstrCancel);
end;

procedure TBaseDataSetThread.Execute;
begin
  InternalExecute(bemExecute);
end;

procedure TBaseDataSetThread.Fill(const ADataSets: Array of TDataSet; AKeepLastRecordSet: Boolean);
var
  I: Integer;
  LDataSets: TStreamArrayOfTDataSet;
begin
  for I := 0 to Length(ADataSets) - 1 do
    ADataSets[I].DisableControls;
  try
    try
      LDataSets.Create(ADataSets, AKeepLastRecordSet);
      InternalExecute(bemOpenFill, @LDataSets);
      Wait;
    except
      for I := 0 to Length(ADataSets) - 1 do
        ADataSets[I].Close;
      FOwner.Close;
      Raise;
    end;
  finally
    for I := 0 to Length(ADataSets) - 1 do
      ADataSets[I].EnableControls;
  end;
end;

procedure TBaseDataSetThread.Open;
begin
  FOwner.FBeforeOpenTime := Now();
  InternalExecute(bemOpen);
end;

procedure TBaseDataSetThread.OpenNext;
begin
  FOwner.FBeforeOpenTime := Now();
  InternalExecute(bemOpenNext);
end;

procedure TBaseDataSetThread.Refresh;
begin
  FOwner.FBeforeOpenTime := Now();
  InternalExecute(bemRefresh);
end;

procedure TBaseDataSetThread.Stop;
begin
  if Assigned(FThreadProcess) then begin
    FTerminating := True;
    try
      try
        FreeAndNil(FThreadProcess);
      except
      end;
    finally
      FTerminating := False;
    end;
  end;
end;

procedure TBaseDataSetThread.Wait;
begin
{$IFNDEF PACKAGE}
  while Active do
  {$IFDEF SBASEFORM_DISABLED}
  try
    Application.HandleMessage
  except
    Application.HandleException(Application);
  end;
  {$ELSE}
  HandleMessage;
  {$ENDIF}
//  if Assigned(FInfoMessages) and (FInfoMessages.Count > 0) then
  if Length(FErrorMessage) > 0 then
    Raise Exception.Create(FErrorMessage);
{$ENDIF}
end;

function TBaseDataSetThread.Active: Boolean;
begin
  Result := (FThreadProcess <> nil); // and (not FThreadProcess.Finished);
end;

{ TCustomSQLDataSetProvider }

constructor TCustomSQLDataSetProvider.Create(AOnwer: TComponent);
begin
  FAutoGenerateUpdateScripts := False;
  FReadOnly := False;
  FDataSetObject := bdsoOriginal;

  FSQL := TStringList.Create;
  FSQL.OnChange := SQLChange;

  FParams := TDataSetProviderParams.Create(Self);
  FSQLApplyOptions := TSQLDataSetProviderApplyOptions.Create(Self);

  FSQLRefresh := TStringList.Create;
  FSQLRefresh.OnChange := SQLRefreshChange;

  FSQLInsert  := TStringList.Create;
  FSQLInsert.OnChange := SQLInsertChange;

  FSQLUpdate  := TStringList.Create;
  FSQLUpdate.OnChange := SQLUpdateChange;

  FSQLDelete  := TStringList.Create;
  FSQLDelete.OnChange := SQLDeleteChange;

  FThread := TBaseDataSetThread.Create(Self);

  inherited;
end;

//procedure TCustomSQLDataSetProvider.DataSetAfterPost;
//begin
//  inherited;
//  if FSQLApplyOptions.FTempDumpActive
//      and (FSQLApplyOptions.FDeleteAfterDump)
//      and (FSQLApplyOptions.FAutoPostCount > 0)
//      and (FSQLApplyOptions.FAutoPostCount < ActiveDataSet.RecordCount)
//  then
//    FSQLApplyOptions.TempPostDump;
//end;

destructor TCustomSQLDataSetProvider.Destroy;
begin
  FreeAndNil(FMainDataSet);
  ConnectionProvider := nil;
  inherited;

  FSQLApplyOptions.Free;
  FSQL.Free;
  FParams.Free;
  FSQLRefresh.Free;
  FSQLInsert.Free;
  FSQLUpdate.Free;
  FSQLDelete.Free;

  FThread.Free;
end;

procedure TCustomSQLDataSetProvider.RefreshRecord;
begin
  CheckBaseSQLDataSetProviderSupport('RefreshRecord').RefreshRecord;
end;

function TCustomSQLDataSetProvider.ReOpen(AReOpenFlag: TDataSetReOpenFlags): Boolean;
var
  LDataSourceStateBookmark: TDataSourceStateBookmark;
  LDataSourceRestore: TDataSourceBookmarkRestores;
  LDataSetInitDetails: TDataSetInitDetails;
  I: Integer;
begin
  if rofSavePosition in AReOpenFlag then
    LDataSourceRestore := [sbrKeyFieldValues, sbrRecordIndex]
  else
    LDataSourceRestore := [];

  BeginUpdate(LDataSourceStateBookmark);
  try
    if (AReOpenFlag * [rofOnlyIfChanged, rofCloseIfParamsRequired] <> []) and (FDataSetObject <> bdsoMemory)
    then begin
      Result := Active;
      case FDataSetObject of

        bdsoOriginal, bdsoCDS:
          begin
            LDataSetInitDetails := InitDataSetParams(ActiveDataSet);

            if Result then
              if
              (
                not (rofOnlyIfChanged in AReOpenFlag)
                or (dsidChanged in LDataSetInitDetails)
                or ((rofCloseIfParamsRequired in AReOpenFlag) and (dsidParamsRequired in LDataSetInitDetails))
              )
              then
                inherited InternalSetActive(False)
              else
                Result := False;

            if (rofCloseIfParamsRequired in AReOpenFlag) and (dsidParamsRequired in LDataSetInitDetails) then
              Exit(Result);

            if Result and (rofOnlyIfChanged in AReOpenFlag) and not (dsidChanged in LDataSetInitDetails) then
              Exit(False);

            inherited InternalSetActive(True);
          end;

        bdsoCDSCached, bdsoCDSPublic:
          begin
            if Assigned(FOnPrepareParams) then
              FOnPrepareParams(Self);

            if Result then begin
              if ConnectionProvider.IsActualDataSet(FBasePublicDataSet, FSQL.Text, FParams) then Exit(False);
              inherited InternalSetActive(False);
            end;

            if (rofCloseIfParamsRequired in AReOpenFlag) then
              for I := FParams.Count - 1 downto 0 do
                with FParams[I] do if Required and IsNull then
                  Exit(Result);

            if not Active then
              InternalSetActive(True)
            else
              InternalCreatePublicDataSet;
          end;
      end;
    end else begin
      Close; Open;
    end;
  finally
    EndUpdate(LDataSourceStateBookmark, [sbrDisableCount], LDataSourceRestore)
  end;
  Result := True;
end;

procedure TCustomSQLDataSetProvider.Execute;
begin
  ActivateDefaultDataSet;
  with CheckBaseSQLDataSetProviderSupport('Execute') do begin
    InitDataSetParams(ActiveDataSet);
    Execute;
  end;
end;

procedure TCustomSQLDataSetProvider.OpenNext;
begin
  with CheckBaseSQLDataSetProviderSupport('OpenNext') do
    if not Active then
      Open
    else
      OpenNext;
end;

//function TCustomSQLDataSetProvider.!asNextResultSet: Boolean;
//begin
//  Result := CheckBaseSQLDataSetProviderSupport('!asNextResultSet').!asNextResultSet;
//end;

procedure TCustomSQLDataSetProvider.ClearNextResultSets;
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider <> nil then
    LBaseSQLDataSetProvider.ClearNextResultSets;
end;

class function TCustomSQLDataSetProvider.ParseSQLParams(const ASQLText: String; AParams: TParams = nil): String;
var
  I: Integer;
  LParamsAdded: TList;
  LParam: TParam;
  LSQLParamItem: TSQLParamItem;
begin
  Result := '';

  if (AParams <> nil) and (AParams.Count > 0) then
    LParamsAdded := TList.Create
  else
    LParamsAdded := nil;

  try
    for LSQLParamItem in EnumSQLParamItems(ASQLText) do
      if (not LSQLParamItem.Value.IsEmpty) then begin
        if (AParams <> nil) then begin
          LParam := AParams.FindParam(LSQLParamItem.Value);
          if LParam = nil then begin
            LParam      := AParams.AddParameter;
            LParam.Name := LSQLParamItem.Value;
            if SameText(LeftStr(LSQLParamItem.Value, 10), SConst_PARAM_OUT_) then
              LParam.ParamType := ptOutput
            else if SameText(LeftStr(LSQLParamItem.Value, 12), SConst_PARAM_INOUT_) then
              LParam.ParamType := ptInputOutput;

            if LParamsAdded <> nil then
              LParamsAdded.Add(LParam);
          end else if (LParamsAdded <> nil) and (LParamsAdded.IndexOf(LParam) = -1) then
            LParamsAdded.Add(LParam);
        end;

        if Result.IsEmpty then
          Result := LSQLParamItem.Value
        else if Pos(';' + UpperCase(LSQLParamItem.Value) + ';', ';' + UpperCase(Result) + ';') = 0 then
          Result := Result + ';' + LSQLParamItem.Value;
      end;

    if LParamsAdded <> nil then
      for I := AParams.Count - 1 downto 0 do begin
        LParam := AParams[I];
        if LParamsAdded.IndexOf(LParam) = -1 then
          AParams.Delete(I)
        else if LParam.ParamType = ptOutput then
          LParam.Clear
      end;
  finally
    LParamsAdded.Free;
  end;
end;

procedure TCustomSQLDataSetProvider.PSExecute;
begin
  Execute
end;

function TCustomSQLDataSetProvider.PSGetParams: TParams;
begin
  Result := FParams;
end;

function TCustomSQLDataSetProvider.PSIsSQLBased: Boolean;
begin
  Result := True
end;

function TCustomSQLDataSetProvider.PSIsSQLSupported: Boolean;
begin
  Result := True
end;

procedure TCustomSQLDataSetProvider.PSSetCommandText(const AText: String);
begin
  FSQL.SetTextIfDifferent(AText);
end;

procedure TCustomSQLDataSetProvider.PSSetParams(AParams: TParams);
begin
  FParams.Assign(AParams)
end;

function TCustomSQLDataSetProvider.GetBaseSQLDataSetProviderSupport: IBaseSQLDataSetProvider;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;
  if Assigned(LDataSet) then
    Supports(LDataSet, IBaseSQLDataSetProvider, Result)
  else
    Result := nil
end;

function TCustomSQLDataSetProvider.CheckBaseSQLDataSetProviderSupport(const AMethod: String): IBaseSQLDataSetProvider;
var
  LDataSet: TDataSetCrack;
begin
  LDataSet := CheckDataSet(AMethod);
  if Assigned(LDataSet) then begin
    Supports(LDataSet, IBaseSQLDataSetProvider, Result);
    if Result = nil then
      Raise Exception.Create('Method ' + AMethod + ' is not available, because DataSet does not support IBaseSQLDataSetProvider interface.');
  end;
end;

procedure TCustomSQLDataSetProvider.ActivateDefaultDataSet;
begin
  FThread.Stop;
  SetDataSet(FMainDataSet);
end;

procedure TCustomSQLDataSetProvider.UnPrepare;
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider <> nil then
    LBaseSQLDataSetProvider.UnPrepare;
end;

procedure TCustomSQLDataSetProvider.ApplyUpdates;
begin
  if FSQLApplyOptions.Method = damDefault then
    inherited
  else
    FSQLApplyOptions.ApplyUpdates
end;

procedure TCustomSQLDataSetProvider.AttachDataSetOptions(const ADataSet: TDataSet);
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  inherited;

  Supports(ADataSet, IBaseSQLDataSetProvider, LBaseSQLDataSetProvider);
  if LBaseSQLDataSetProvider <> nil then
    with LBaseSQLDataSetProvider do begin
      if GetRefreshOptions <> FRefreshOptions then
        SetRefreshOptions(FRefreshOptions);

      if GetAutoGenerateUpdateScripts <> FAutoGenerateUpdateScripts then
        SetAutoGenerateUpdateScripts(FAutoGenerateUpdateScripts);

      if {$IFDEF DELPHIXE3}IProviderSupportNG{$ELSE}IProviderSupport{$ENDIF}(ADataSet).PSGetCommandText <> FSQL.Text then
        {$IFDEF DELPHIXE3}IProviderSupportNG{$ELSE}IProviderSupport{$ENDIF}(ADataSet).PSSetCommandText(FSQL.Text);

      if GetSQLRefresh <> FSQLRefresh.Text then
        SetSQLRefresh(FSQLRefresh.Text);

      if GetSQLInsert <> FSQLInsert.Text then
        SetSQLInsert(FSQLInsert.Text);

      if GetSQLUpdate <> FSQLUpdate.Text then
        SetSQLUpdate(FSQLUpdate.Text);

      if GetSQLDelete <> FSQLDelete.Text then
        SetSQLDelete(FSQLDelete.Text);
    end;
end;

procedure TCustomSQLDataSetProvider.BreakExec;
begin
  CheckBaseSQLDataSetProviderSupport('BreakExec').BreakExec;
end;

procedure TCustomSQLDataSetProvider.DetachDataSetOptions(const ADataSet: TDataSet);
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  inherited;

  Supports(ADataSet, IBaseSQLDataSetProvider, LBaseSQLDataSetProvider);
  if LBaseSQLDataSetProvider <> nil then with LBaseSQLDataSetProvider do begin
    FRefreshOptions             := GetRefreshOptions;
    FAutoGenerateUpdateScripts  := AutoGenerateUpdateScripts;

    FSQLUpdating := True;
    try
      FSQL.SetTextIfDifferent({$IFDEF DELPHIXE3}IProviderSupportNG{$ELSE}IProviderSupport{$ENDIF}(ADataSet).PSGetCommandText);
      FSQLRefresh.SetTextIfDifferent(GetSQLRefresh);
      FSQLInsert.SetTextIfDifferent(GetSQLInsert);
      FSQLUpdate.SetTextIfDifferent(GetSQLUpdate);
      FSQLDelete.SetTextIfDifferent(GetSQLDelete);
    finally
      FSQLUpdating := False;
    end;
  end;
end;

procedure TCustomSQLDataSetProvider.SetSQLConnectionProvider(const Value: TSQLConnectionProvider);
begin
  if FSQLConnectionProvider <> Value then begin
    if FSQLConnectionProvider <> nil then FSQLConnectionProvider.RemoveSQLDataSetProvider(Self);
    if Value <> nil then Value.AddSQLDataSetProvider(Self);
    SQLConnectionProviderChanged;
  end;
end;

function TCustomSQLDataSetProvider.GetRefreshOptions: TBaseRefreshOptions;
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider <> nil then
    Result := LBaseSQLDataSetProvider.GetRefreshOptions
  else
    Result := FRefreshOptions
end;

procedure TCustomSQLDataSetProvider.SetRefreshOptions(AValue: TBaseRefreshOptions);
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider <> nil then
    LBaseSQLDataSetProvider.SetRefreshOptions(AValue)
  else
    FRefreshOptions := AValue
end;

procedure TCustomSQLDataSetProvider.InternalSQLConnectionProviderChanged;
var
  LMainDataSet: TDataSet;
begin
  FSQLConnectionProviderChanged := False;

  FThread.Stop;
  LMainDataSet := FMainDataSet;

  if (FDataSetObject <> bdsoMemory) and ((FSQLConnectionProvider = nil) or (FSQLConnectionProvider.Connection = nil)) then
    FMainDataSet := nil
  else begin
    case FDataSetObject of
      bdsoOriginal, bdsoCDS:
        begin
          FMainDataSet := TInternalQuery.Create(Self);
          with TInternalQuery(FMainDataSet) do begin
            Disconnected := (FDataSetObject = bdsoCDS);
            Connection   := FSQLConnectionProvider.Connection;
          end;
        end;
      bdsoCDSCached, bdsoMemory:
        FMainDataSet := TInternalMemoryDataSet.Create(Self);
//      bdsoCDSPublic:
    end;
  end;

  SetDataSet(FMainDataSet);
  LMainDataSet.Free;
{$IFDEF DEBUG}
  if Assigned(FMainDataSet) then
    FMainDataSet.Name := SConst_MainDataSet;
{$ENDIF}

  if FThread.FReservedDataSet <> nil then
    FreeAndNil(FThread.FReservedDataSet);
end;

procedure TCustomSQLDataSetProvider.SQLConnectionProviderChanged;
begin
  if csReading in ComponentState then
    FSQLConnectionProviderChanged := True
  else
    InternalSQLConnectionProviderChanged;
end;

procedure TCustomSQLDataSetProvider.InternalCreatePublicDataSet;
begin
  FBasePublicDataSet := ConnectionProvider.CreatePublicDataSet(Self, FSQL.Text, FParams, (FDataSetObject = bdsoCDSPublic));
  if FDataSetObject = bdsoCDSPublic then
    SetDataSet(FBasePublicDataSet.DataSet.PublicQuery)
  else
    FBasePublicDataSet.DataSet.Save(TInternalMemoryDataSet(ActiveDataSet));

  if Assigned(AfterOpen) then
    AfterOpen(Self);
end;

procedure TCustomSQLDataSetProvider.SetActive(Value: Boolean);
begin
  if (csReading in ComponentState) then
    inherited
  else begin
    if (FDataSetObject = bdsoCDSPublic) and (not Value) and LogicalyActive then begin
      ConnectionProvider.ClosePublicDataSet(Self, FBasePublicDataSet);
      SetDataSet(nil);
    end else
      inherited;
  end;
end;

procedure TCustomSQLDataSetProvider.InternalSetActive(AValue: Boolean);
begin
  ActivateDefaultDataSet;

  if FDataSetObject <> bdsoMemory then
    if AValue then begin
      if not Assigned(ConnectionProvider) then
        Raise Exception.Create(SConnectionProviderNotDefined);
{$IFDEF DEBUG}
      if FSQL.Count = 0 then
        RaiseDataSetException(SSQLTextNotDefined);
{$ENDIF}

      case FDataSetObject of
        bdsoOriginal, bdsoCDS:
          InitDataSetParams(ActiveDataSet);
        bdsoCDSCached, bdsoCDSPublic:
          begin
            if Assigned(FOnPrepareParams) then
              FOnPrepareParams(Self);

            FBeforeOpenTime := Now();
            InternalCreatePublicDataSet;
            if ActiveDataSet.Active then
              FOpenDuration := MilliSecondsBetween(FBeforeOpenTime, Now());
            Exit;
          end;
      end;
    end else begin // if not AValue
      // Never call with bdsoCDSPublic
      if Assigned(ConnectionProvider) and (FDataSetObject in [bdsoCDSCached]) then begin
        ConnectionProvider.ClosePublicDataSet(Self, FBasePublicDataSet);
        //if FDataSetObject = bdsoCDSPublic then
      end;
    end;

  inherited;
end;

procedure TCustomSQLDataSetProvider.InternalSetCachedUpdates(Value: Boolean);
begin
  Assert(FSQLApplyOptions.Method = damDefault);
  inherited;
end;

function TCustomSQLDataSetProvider.GetAutoGenerateUpdateScripts: Boolean;
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider = nil then
    Result := FAutoGenerateUpdateScripts
  else
    Result := LBaseSQLDataSetProvider.GetAutoGenerateUpdateScripts;
end;

procedure TCustomSQLDataSetProvider.SetAutoGenerateUpdateScripts(const AValue: Boolean);
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider = nil then
    FAutoGenerateUpdateScripts := AValue
  else
    LBaseSQLDataSetProvider.SetAutoGenerateUpdateScripts(AValue);
end;

function TCustomSQLDataSetProvider.GetConnection: TCustomConnection;
begin
  if Assigned(FSQLConnectionProvider) then
    Result := FSQLConnectionProvider.Connection
  else
    Result := nil
end;

procedure TCustomSQLDataSetProvider.SetConnection(AValue: TCustomConnection);
begin
  Assert(Assigned(FSQLConnectionProvider) and (FSQLConnectionProvider.Connection <> AValue));
end;

procedure TCustomSQLDataSetProvider.SetDataSetObject(const AValue: TBaseDataSetObject);
begin
  if AValue = FDataSetObject then Exit;

//  if (FDataSetObject = bdsoCDSPublic) then begin
//    if Assigned(FMainDataSet) then
//      Raise Exception.Create('Cannot change DataSetObject = bdsoCDSPublic property');
//  end else

  if Active then
    Raise Exception.Create('Cannot change DataSetObject property when DataSet is Active');

  FDataSetObject := AValue;
  case FDataSetObject of
    bdsoCDSCached, bdsoMemory:
      begin
        FAutoGenerateUpdateScripts := False;
        // FCachedUpdates := True;
      end;
    bdsoCDSPublic:
      begin
        FAutoGenerateUpdateScripts := False;
        FCachedUpdates := False;
      end;
  end;

  SQLConnectionProviderChanged;
end;

procedure TCustomSQLDataSetProvider.SetParams(const Value: TDataSetProviderParams);
begin
  FParams.Assign(Value);
end;

procedure TCustomSQLDataSetProvider.SetSQL(const AValue: TStringList);
begin
  SQL.AssignStringsIfDifferent(AValue);
end;

procedure TCustomSQLDataSetProvider.SetSQLApplyOptions(const Value: TSQLDataSetProviderApplyOptions);
begin
  FSQLApplyOptions.Assign(Value);
end;

procedure TCustomSQLDataSetProvider.SetSQLRefresh(const AValue: TStringList);
begin
  SQLRefresh.AssignStringsIfDifferent(AValue);
end;

procedure TCustomSQLDataSetProvider.SetSQLInsert(const AValue: TStringList);
begin
  SQLInsert.AssignStringsIfDifferent(AValue);
end;

function TCustomSQLDataSetProvider.GetThreadEvents: TNotifyEvent;
begin
  Result := nil;
end;

function TCustomSQLDataSetProvider.InitDataSetParams(ADataSet: TDataSet): TDataSetInitDetails;
var
  I: Integer;
  LParams: TParams;
  LParam: TDataSetProviderParam;
begin
  LParams := {$IFDEF DELPHIXE3}IProviderSupportNG{$ELSE}IProviderSupport{$ENDIF}(ADataSet).PSGetParams;
  Assert(Assigned(LParams));

  if Assigned(FOnPrepareParams) then
    FOnPrepareParams(Self);

  Result := [];
  for I := Pred(LParams.Count) downto 0 do
    with LParams[I] do begin
      LParam := Self.FParams.FindParam(Name);
      if (LParam = nil) then begin
{ -=MD=-: ParamCheck = False
        LParams.Delete(I);
        Include(Result, dsidChanged);
        Continue;
}
{ -=MD=-: ParamCheck = True }
        if not IsNull then begin
          Clear;
          Include(Result, dsidChanged);
        end
      end else begin
        if LParam.Required and LParam.IsNull then
          Include(Result, dsidParamsRequired);

        if DataType <> LParam.DataType then begin
          DataType  := LParam.DataType;
          Include(Result, dsidChanged);
        end;

        if ParamType <> LParam.ParamType then begin
          ParamType := LParam.ParamType;
          Include(Result, dsidChanged);
        end;

        if not VarIsEqual(Value, LParam.Value) then begin
          Value  := LParam.Value;
          Include(Result, dsidChanged);
        end;
      end;
    end;

{ -=MD=-: ParamCheck = False }
//  for I := Pred(Self.FParams.Count) downto 0 do begin
//    LParam := Self.FParams[I];
//    if (LParams.FindParam(LParam.Name) = nil) then begin
//      with TParam(LParams.Add) do begin
//        Name       := LParam.Name;
//        DataType   := LParam.DataType;
//        ParamType  := LParam.ParamType;
//        if not LParam.IsNull then
//          Value := LParam.Value
//        else if LParam.Required then
//          Include(Result, dsidParamsRequired);
//        Include(Result, dsidChanged);
//      end;
//    end;
//  end;
end;

procedure TCustomSQLDataSetProvider.InternalSetDataSet(const AFrom, ATo: TDataSet);
begin
{$IFNDEF PACKAGE}
  if Assigned(AFrom) and Assigned(ATo) then begin
    while AFrom.DisableCount > ATo.DisableCount do ATo.DisableControls;
    while AFrom.DisableCount < ATo.DisableCount do ATo.EnableControls;
  end;
{$ENDIF}

  inherited;

  if Assigned(AFrom) then begin
    while AFrom.ControlsDisabled do AFrom.EnableControls;
    if AFrom is TInternalMemoryDataSet then
      TInternalMemoryDataSet(AFrom).Focused := False
    else
      TInternalQuery(AFrom).Focused := False;
  end;

  if Assigned(ATo) then
    if ATo is TInternalMemoryDataSet then
      TInternalMemoryDataSet(ATo).Focused := True
    else if ATo is TInternalQuery then
      TInternalQuery(ATo).Focused := True
{$IFDEF DEBUG}
    else if not (ATo is TInternalPublicQuery) then
      Assert(False, 'Invalid Value for call SetMainDataSet method')
{$ENDIF};
end;

function TCustomSQLDataSetProvider.IGetSQLDelete: String;
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider <> nil then
    Result := LBaseSQLDataSetProvider.GetSQLDelete
  else
    Result := FSQLDelete.Text
end;

procedure TCustomSQLDataSetProvider.ISetSQLDelete(const Value: String);
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider <> nil then
    LBaseSQLDataSetProvider.SetSQLDelete(Value)
  else
    FSQLDelete.Text := Value
end;

function TCustomSQLDataSetProvider.IGetSQLInsert: String;
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider <> nil then
    Result := LBaseSQLDataSetProvider.GetSQLInsert
  else
    Result := FSQLInsert.Text
end;

procedure TCustomSQLDataSetProvider.ISetSQLInsert(const Value: String);
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider <> nil then
    LBaseSQLDataSetProvider.SetSQLInsert(Value)
  else
    FSQLInsert.Text := Value
end;

function TCustomSQLDataSetProvider.IGetSQLRefresh: String;
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider <> nil then
    Result := LBaseSQLDataSetProvider.GetSQLRefresh
  else
    Result := FSQLRefresh.Text
end;

procedure TCustomSQLDataSetProvider.ISetSQLRefresh(const Value: String);
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider <> nil then
    LBaseSQLDataSetProvider.SetSQLRefresh(Value)
  else
    FSQLRefresh.Text := Value
end;

function TCustomSQLDataSetProvider.IGetSQLUpdate: String;
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider <> nil then
    Result := LBaseSQLDataSetProvider.GetSQLUpdate
  else
    Result := FSQLUpdate.Text
end;

procedure TCustomSQLDataSetProvider.ISetSQLUpdate(const Value: String);
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
begin
  LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
  if LBaseSQLDataSetProvider <> nil then
    LBaseSQLDataSetProvider.SetSQLUpdate(Value)
  else
    FSQLUpdate.Text := Value
end;

procedure TCustomSQLDataSetProvider.Loaded;
begin
  if FSQLConnectionProviderChanged then InternalSQLConnectionProviderChanged;

  inherited;
end;

procedure TCustomSQLDataSetProvider.LoadFields(AFields: TFields; ALoadValueCondition: TLoadValueCondition; AExcludedFields: String; AOnlyIfCanModify: Boolean);
var
  LField: TField;
begin
  with FieldsAutomations do
    if (not FModifiAbleFieldsFieldName.IsEmpty) then begin
      with FieldByName(FModifiAbleFieldsFieldName) do begin
        LField := AFields.FindField(FModifiAbleFieldsFieldName);
        if Assigned(LField) then begin
          if (ALoadValueCondition <> lvcIfNotPresent) or (IsNull) then
            AsVariant := LField.AsVariant;
        end else if ALoadValueCondition = lvcAlways then
          Clear;
      end;

      DoDataSetFieldsUpdateAble(True);
      AExcludedFields := ArraysMerge([AExcludedFields, FModifiAbleFieldsFieldName], ',');
    end;

  Fields.Load(AFields, ALoadValueCondition, AExcludedFields, AOnlyIfCanModify);
end;

function TCustomSQLDataSetProvider.LogicalyActive: Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := ActiveDataSet;

  if (FDataSetObject = bdsoCDSPublic) then
    Result := Assigned(LDataSet)
  else
    Result := (LDataSet <> nil) and LDataSet.Active;
end;

procedure TCustomSQLDataSetProvider.LoadFields(const AVariables: TNamedVariants; ALoadValueCondition: TLoadValueCondition; AExcludedFields: String);
var
  LIndex: Integer;
begin
  with FieldsAutomations do
    if (not FModifiAbleFieldsFieldName.IsEmpty) then begin
      with FieldByName(FModifiAbleFieldsFieldName) do
        if AVariables.Find(FModifiAbleFieldsFieldName, LIndex) then begin
          if (ALoadValueCondition <> lvcIfNotPresent) or (IsNull) then
            AsVariant := AVariables.Items[LIndex].Value;
        end else if ALoadValueCondition = lvcAlways then
          Clear;

      DoDataSetFieldsUpdateAble(True);
      AExcludedFields := ArraysMerge([AExcludedFields, FModifiAbleFieldsFieldName], ',');
    end;

  Fields.Load(AVariables, ALoadValueCondition, AExcludedFields);
end;

procedure TCustomSQLDataSetProvider.CopyRecord(const AExcludedFields: String; AConstants: TNamedVariants);
var
  I, LIdx: Integer;
begin
  for I := 0 to FieldCount - 1 do
    with Fields[I] do
      if (not InArray(AExcludedFields, FieldName, ';')) and (not AConstants.Find(FieldName, LIdx)) then
        AConstants[FieldName] := AsVariant;

  DisableControls;
  try
    Append;
    LoadFields(AConstants, lvcIfReceive, AExcludedFields);
  finally
    EnableControls;
  end;
end;

procedure TCustomSQLDataSetProvider.CopyRecord(const AExcludedFields: String; const AConstants: Array of TNamedVariant);
begin
  CopyRecord(AExcludedFields, TNamedVariants.Create(AConstants, False));
end;

procedure TCustomSQLDataSetProvider.CopyRecord(const AExcludedFields: String);
begin
  CopyRecord(AExcludedFields, TNamedVariants.Create(True));
end;

procedure TCustomSQLDataSetProvider.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FMainDataSet then
      FMainDataSet := nil
    else if AComponent = FSQLConnectionProvider then
      ConnectionProvider := nil
end;

procedure TCustomSQLDataSetProvider.SetThread(const Value: TBaseDataSetThread);
begin
  FThread.Assign(Value);
end;

procedure TCustomSQLDataSetProvider.SetThreadEvents(const Value: TNotifyEvent);
begin
end;

procedure TCustomSQLDataSetProvider.SetSQLUpdate(const AValue: TStringList);
begin
  SQLUpdate.AssignStringsIfDifferent(AValue);
end;

procedure TCustomSQLDataSetProvider.SetSQLDelete(const AValue: TStringList);
begin
  SQLDelete.AssignStringsIfDifferent(AValue);
end;

procedure TCustomSQLDataSetProvider.SQLChange(Sender: TObject);
var
  LDataSet: TDataSet;
begin
  if (not FSQLUpdating) then begin
    ParseSQLParams(FSQL.Text, FParams);
    LDataSet := ActiveDataSet;
    if (LDataSet <> nil) and (LDataSet is TInternalQuery) then
      TInternalQuery(LDataSet).SQL.AssignStringsIfDifferent(FSQL);
  end;
end;

procedure TCustomSQLDataSetProvider.SQLRefreshChange(Sender: TObject);
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
  LSQL: String;
begin
  if (not FSQLUpdating) and (FSQLApplyOptions.Method = damDefault) then begin
    LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
    if (LBaseSQLDataSetProvider <> nil) then begin
      LSQL := FSQLRefresh.Text;
      if (LSQL <> LBaseSQLDataSetProvider.GetSQLRefresh) then
        LBaseSQLDataSetProvider.SetSQLRefresh(LSQL);
    end;
  end;
end;

procedure TCustomSQLDataSetProvider.SQLInsertChange(Sender: TObject);
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
  LSQL: String;
begin
  if (not FSQLUpdating) and (FSQLApplyOptions.Method = damDefault) then begin
    LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
    if (LBaseSQLDataSetProvider <> nil) then begin
      LSQL := FSQLInsert.Text;
      if (LSQL <> LBaseSQLDataSetProvider.GetSQLInsert) then
        LBaseSQLDataSetProvider.SetSQLInsert(LSQL);
    end;
  end;
end;

procedure TCustomSQLDataSetProvider.SQLUpdateChange(Sender: TObject);
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
  LSQL: String;
begin
  if (not FSQLUpdating) and (FSQLApplyOptions.Method = damDefault) then begin
    LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
    if (LBaseSQLDataSetProvider <> nil) then begin
      LSQL := FSQLUpdate.Text;
      if (LSQL <> LBaseSQLDataSetProvider.GetSQLUpdate) then
        LBaseSQLDataSetProvider.SetSQLUpdate(LSQL);
    end;
  end;
end;

procedure TCustomSQLDataSetProvider.SQLDeleteChange(Sender: TObject);
var
  LBaseSQLDataSetProvider: IBaseSQLDataSetProvider;
  LSQL: String;
begin
  if (not FSQLUpdating) and (FSQLApplyOptions.Method = damDefault) then begin
    LBaseSQLDataSetProvider := GetBaseSQLDataSetProviderSupport;
    if (LBaseSQLDataSetProvider <> nil) then begin
      LSQL := FSQLDelete.Text;
      if (LSQL <> LBaseSQLDataSetProvider.GetSQLDelete) then
        LBaseSQLDataSetProvider.SetSQLDelete(LSQL);
    end;
  end;
end;

function TCustomSQLDataSetProvider.SupportedMethods: TBaseDataSetSupportedActions;
begin
  Result := [bdssaSQL, bdssaKeyFields, bdssaRefreshAfterInsert, bdssaRefreshAfterUpdate, bdssaRefreshBeforeEdit];
end;

procedure TCustomSQLDataSetProvider.SyncronizeOutputParameters(Sender: TDataSet);
var
  I: Integer;
  LDataSet: TDataSet;
  LParams: TParams;
  LParam: Tparam;
begin
  // FSQLApplyOptions.FInternalTempTable := '';

  LDataSet := ActiveDataSet;
  if (Sender = nil) or (LDataSet = nil) or (LDataSet <> Sender) then Exit;
  LParams := {$IFDEF DELPHIXE3}IProviderSupportNG{$ELSE}IProviderSupport{$ENDIF}(LDataSet).PSGetParams;
  Assert(Assigned(LParams));
  for I := 0 to LParams.Count - 1 do with LParams[I] do begin
    LParam := FParams.FindParam(Name);
    if (LParam <> nil) and (LParam.ParamType in [ptOutput, ptInputOutput]) then begin
      LParam.Value := Value;
      if ParamType = ptOutput then
        Clear;
    end;
  end;
end;

{ TFieldReadonly }

class function TFieldReadOnly.ComponentName: String;
begin
  Result := 'ReadOnlyOriginStorage'
end;

constructor TFieldReadOnly.Create(AOwner: TField);
begin
  inherited Create(AOwner);

  Name := ComponentName;
  FReadOnlyOrigin := AOwner.ReadOnly;
end;

class procedure TFieldReadOnly.DisableEdit(AField: TField);
var
  LFieldReadOnly: TFieldReadOnly;
begin
  LFieldReadOnly := TFieldReadOnly(AField.FindComponent(ComponentName));
  if LFieldReadOnly = nil then
    {LFieldReadOnly := }TFieldReadOnly.Create(AField);

  AField.ReadOnly := True;
end;

class procedure TFieldReadOnly.RestoreEdit(AField: TField);
var
  LFieldReadOnly: TFieldReadOnly;
begin
  LFieldReadOnly := TFieldReadOnly(AField.FindComponent(ComponentName));
  if LFieldReadOnly = nil then
    Exit;

  AField.ReadOnly := LFieldReadOnly.FReadOnlyOrigin;
end;

{ TMasterDetailDataSetControl }

{$IFNDEF PACKAGE}
procedure TMasterDetailDataSetControl.ClearFields;
begin
  FDataFieldsDataLink.Clear;
end;
{$ENDIF}

constructor TMasterDetailDataSetControl.Create(AOwner: TComponent);
begin
  FEnabled           := True;
  FActive            := False;
//  FDetailFieldValues := unAssigned;
  FConditionChanged  := True;

  FDetailFieldsList  := TStringList.Create;
  FDetailFieldsList.Delimiter := ';';

  inherited;

  FMasterFieldsDataLink := TMDFieldDataLink.Create;
  FDataFieldsDataLink   := TMDFieldDataLink.Create;
  FDetailSourceLink     := TDataSourceDataLink.Create;

  FMasterFieldsDataLink.Control      := Self;
  FMasterFieldsDataLink.MultiFields  := True;

{$IFNDEF PACKAGE}
  FDataFieldsDataLink.OnDataChanged     := DataChanged;
  FMasterFieldsDataLink.OnDataChanged   := DataChanged;

  FMasterFieldsDataLink.OnFieldChanged  := MasterFieldsChanged;

  FDetailSourceLink.OnDataSetChanged := DetailDataSetChanged;
{$ENDIF}
end;

destructor TMasterDetailDataSetControl.Destroy;
begin
  FreeAndNil(FMasterFieldsDataLink);
  FreeAndNil(FDetailSourceLink);
  FreeAndNil(FDataFieldsDataLink);

  FreeAndNil(FDetailFieldsList);

  inherited;
end;

procedure TMasterDetailDataSetControl.ConditionChanged(AForce: Boolean);
{$IFNDEF PACKAGE}
var
  LDataSet, LMasterDataSet: TDataSet;
{$ENDIF}
begin
  if AForce then FConditionChanged := True;

{$IFNDEF PACKAGE}
  LDataSet        := DataSet;
  LMasterDataSet  := MasterDataSet;

  if Assigned(LMasterDataSet)
      //and (not LMasterDataSet.IsEmpty)
      and (not MasterFields.IsEmpty)
      and Assigned(DetailSource)
      and
      (
        ((Method = mdfmFilter) and (not FDetailFilterFormat.IsEmpty))
        or
        (not DetailFields.IsEmpty)
      )
      and FEnabled and LMasterDataSet.EvaluateBoolean(FEnabledCondition, False, True)
  then begin
    if (not LMasterDataSet.IsEmpty) and LMasterDataSet.EvaluateBoolean(FCondition, False, True) then begin
      SetDetailFieldValues;
      if FConditionChanged then
        ActivateDetailDataSet;
    end else begin
      DeactivateDetailDataSet;
      if (mdfhClearIfNotInList in FDataHelpers) and Assigned(LDataSet) and LDataSet.Editing
            and (not ListField.IsEmpty) and (not DataField.IsEmpty) and GetFieldsCanModify(LDataSet, DataField)
      then
        ClearFields;
    end;

    if Assigned(FOnMasterDataChanged) then
      FOnMasterDataChanged(Self);
  end;
{$ENDIF}
end;

{$IFNDEF PACKAGE}
procedure TMasterDetailDataSetControl.DataChanged(Sender: TObject);
begin
  ConditionChanged;
end;
{$ENDIF}

function TMasterDetailDataSetControl.GetMasterSource: TDataSource;
begin
  Result := FMasterFieldsDataLink.DataSource;
end;

{
procedure TMasterDetailDataSetControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FMainDataSet then
      FMainDataSet := nil
    else if AComponent = FSQLConnectionProvider then
      ConnectionProvider := nil
end;
}

procedure TMasterDetailDataSetControl.SetMasterSource(const Value: TDataSource);
begin
  FMasterFieldsDataLink.DataSource := Value;
end;

function TMasterDetailDataSetControl.GetMasterFields: String;
begin
  Result := FMasterFieldsDataLink.FieldName;
end;

procedure TMasterDetailDataSetControl.SetMasterFields(const Value: String);
begin
  FMasterFieldsDataLink.FieldName := Value;
end;

procedure TMasterDetailDataSetControl.SetDetailSource(const Value: TDataSource);
begin
  if FDetailSourceLink.DataSource <> Value then begin
    FDetailSourceLink.DataSource := Value;
    ConditionChanged
  end;
end;

procedure TMasterDetailDataSetControl.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    FMasterFieldsDataLink.Enabled := FEnabled;
    FDataFieldsDataLink.Enabled := FEnabled;
    ConditionChanged;
  end;
end;

function TMasterDetailDataSetControl.GetDetailFields: String;
begin
  Result := FDetailFieldsList.DelimitedText;
end;

function TMasterDetailDataSetControl.GetDetailSource: TDataSource;
begin
  Result := FDetailSourceLink.DataSource
end;

procedure TMasterDetailDataSetControl.SetDetailFields(const Value: String);
begin
  if DetailFields <> Value then begin
    FDetailFieldsList.DelimitedText := Value;

    ConditionChanged;
  end;
end;

function TMasterDetailDataSetControl.GetMasterDataSet: TDataSet;
begin
  Result := FMasterFieldsDataLink.DataSet
end;

{$IFNDEF PACKAGE}
procedure TMasterDetailDataSetControl.ActivateDetailDataSet;
var
  LDetailDataSet: TCustomSQLDataSetProvider;
begin
  LDetailDataSet := DetailDataSet;

  if FEnabled and Assigned(LDetailDataSet) then
    with DetailDataSet do begin
      DisableControls;
      try
        if FMethod = mdfmFilter then begin
          Filter   := FDetailFilter;
          Filtered := True;
          Open;
        end else begin
          ReOpen([rofOnlyIfChanged, rofCloseIfParamsRequired, rofSavePosition]);
        end;
        FActive := False;
      finally
        EnableControls
      end;
    end;

  FConditionChanged := False;
end;

procedure TMasterDetailDataSetControl.DetailDataSetChanged(Sender: TObject);
var
  LDetailDataSet: TCustomSQLDataSetProvider;
begin
  LDetailDataSet  := DetailDataSet;
  if (not FActive) and Assigned(LDetailDataSet) and LDetailDataSet.Active then begin
    FActive := True;
    DoAfterActivate;
  end else if (FActive) and (not Assigned(LDetailDataSet) or not LDetailDataSet.Active) then begin
    FActive := False;
    DoAfterDeActivate
  end;
end;

procedure TMasterDetailDataSetControl.DoAfterActivate;
var
  LDataSet: TDataSet;
  LDetailDataSet: TCustomSQLDataSetProvider;
begin
  LDetailDataSet  := DetailDataSet;
  if FActive and Assigned(LDetailDataSet) and LDetailDataSet.Active then begin
    if (FDataHelpers <> []) then begin
      LDataSet := DataSet;
      if Assigned(LDataSet) and LDataSet.Editing and (ListField <> '') and (DataField <> '') and GetFieldsCanModify(LDataSet, DataField) then
        if (mdfhSetWhenOneRecord in FDataHelpers) and (LDetailDataSet.RecordCount = 1) then
          LDataSet.FieldValues[DataField] := LDetailDataSet.FieldValues[ListField]
        else if (mdfhSetIfDefaultLocated in FDataHelpers) and (LDetailDataSet.Locate(SConst_Default, True, [])) then
          LDataSet.FieldValues[DataField] := LDetailDataSet.FieldValues[ListField]
        else if (mdfhClearIfNotInList in FDataHelpers) and (not LDetailDataSet.Locate(ListField, LDataSet.FieldValues[DataField], [])) then
          ClearFields
    end;

    if Assigned(FAfterActivate) then
      FAfterActivate(Self);
  end;
end;

procedure TMasterDetailDataSetControl.DoAfterDeActivate;
begin
  if (not FActive) and Assigned(FAfterDeactivate) then
    FAfterDeactivate(Self);
end;

procedure TMasterDetailDataSetControl.MasterFieldsChanged(Sender: TObject);
begin
  if Assigned(FOnMasterFieldsChanged) then
    FOnMasterFieldsChanged(Sender);
end;

procedure TMasterDetailDataSetControl.DeactivateDetailDataSet;
var
  LDetailDataSet: TCustomSQLDataSetProvider;
  LActive: Boolean;
begin
  LActive := FActive;
  LDetailDataSet  := DetailDataSet;

  if Enabled and Assigned(LDetailDataSet) then begin
    FActive := False;
    with LDetailDataSet do
      if FMethod = mdfmFilter then begin
        Filter := SFilterFalseCondition
      end else begin
        Close;
      end;
  end;

  FConditionChanged := True;
  if LActive and (not FActive) then
    DoAfterDeActivate;
end;

procedure TMasterDetailDataSetControl.SetDetailFieldValues;
var
  LDetailFilter: String;
  LDetailFilterParamItem: TStringParamItem;
  I: Integer;
  LFieldValues: Variant;
  LDetailDataSet: TCustomSQLDataSetProvider;
begin
  LDetailDataSet := DetailDataSet;
  if FMethod = mdfmFilter then begin
    if FDetailFilterFormat.IsEmpty then begin
      LDetailFilter := '';
      for I := 0 to Pred(FMasterFieldsDataLink.FieldsCount) do
        LDetailFilter := ConcatIfNotEmpty
                         (
                          LDetailFilter,
                          ' ' + SConst_AND + ' ',
                          FDetailFieldsList[I]
                          + '=' + LDetailDataSet.VariantToFilterString(FMasterFieldsDataLink.Fields[I].AsVariant)
                         );
    end else begin
      LDetailFilter := '';
      for LDetailFilterParamItem in EnumStringParamItems(FDetailFilterFormat, []) do begin
        LDetailFilter := LDetailFilter + LDetailFilterParamItem.Gap;
        if not LDetailFilterParamItem.Value.IsEmpty then
          LDetailFilter := LDetailFilter + FMasterFieldsDataLink.FieldByName(LDetailFilterParamItem.Value).AsString;
      end;
    end;

    FConditionChanged := FConditionChanged or (FDetailFilter <> LDetailFilter);
    FDetailFilter := LDetailFilter;
  end else with LDetailDataSet.Params do begin
    LFieldValues := MasterDataSet.FieldValues[MasterFields];
    FConditionChanged := FConditionChanged or not VarIsEqual(GetValues(DetailFields), LFieldValues);
    if FConditionChanged then
      SetValues(DetailFields, LFieldValues);

    if FMethod = mdfmParamsEx then begin
      LFieldValues := FDataFieldsDataLink.Value;
      if not VarIsEqual(GetValues(ListField), LFieldValues) then begin
        SetValues(ListField, LFieldValues);
        if (not FDataFieldsDataLink.DataSetActive) or (not FDataFieldsDataLink.DataSet.Locate(ListField, LFieldValues, [])) then
          FConditionChanged := True;
      end;
    end;

  end;
end;
{$ENDIF}

procedure TMasterDetailDataSetControl.SetMethod(const Value: TMasterDetailFilterMethod);
begin
  if FMethod <> Value then begin
    FMethod := Value;
    FDataFieldsDataLink.Enabled := (FMethod = mdfmParamsEx);

    ConditionChanged;
  end;
end;

procedure TMasterDetailDataSetControl.SetCondition(const Value: String);
begin
  if FCondition <> Value then begin
    FCondition := Value;
    ConditionChanged;
  end;
end;

function TMasterDetailDataSetControl.GetDetailDataSet: TCustomSQLDataSetProvider;
var
  LDataSet: TDataSet;
begin
  if Assigned(FDetailSourceLink.DataSource) then begin
    LDataSet := FDetailSourceLink.DataSource.DataSet.Source;
    Assert(not Assigned(LDataSet) or (LDataSet is TCustomSQLDataSetProvider));
    Result := LDataSet as TCustomSQLDataSetProvider;
  end else
    Result := nil;
end;

function TMasterDetailDataSetControl.GetDataSet: TDataSet;
begin
  Result := FDataFieldsDataLink.DataSet.Source
end;

function TMasterDetailDataSetControl.GetDataSource: TDataSource;
begin
  Result := FDataFieldsDataLink.DataSource
end;

function TMasterDetailDataSetControl.GetDataField: String;
begin
  Result := FDataFieldsDataLink.FieldName
end;

procedure TMasterDetailDataSetControl.SetDataField(const Value: String);
begin
  FDataFieldsDataLink.FieldName := Value;
end;

procedure TMasterDetailDataSetControl.SetDataSource(const Value: TDataSource);
begin
  FDataFieldsDataLink.DataSource := Value
end;

{ TDateTimeOffsetFieldControl }

constructor TDateTimeOffsetFieldControl.Create(AOnwer: TComponent);
begin
  inherited;
  FFieldDataLink := TMDFieldDataLink.Create;
  FFieldDataLink.Control     := Self;
  FFieldDataLink.MultiFields := True;
  FFieldDataLink.OnDataChanged := FieldDataLinkOnDataChanged;
end;

destructor TDateTimeOffsetFieldControl.Destroy;
begin
  FreeAndNil(FFieldDataLink);
  inherited;
end;

function TDateTimeOffsetFieldControl.GetDataSource: TDataSource;
begin
  Result := FFieldDataLink.DataSource
end;

procedure TDateTimeOffsetFieldControl.SetDataSource(const Value: TDataSource);
begin
  FFieldDataLink.DataSource := Value
end;

function TDateTimeOffsetFieldControl.GetDataFieldDateTime: String;
var
  LFieldNames: String;
  I: Integer;
begin
  LFieldNames := FFieldDataLink.FieldName;
  I := Pos(';', LFieldNames);
  if (I > 2) or ((I = 2) and (LFieldNames[1] <> '-')) then
    Result := LeftStr(LFieldNames, I - 1)
  else
    Result := '';
end;

procedure TDateTimeOffsetFieldControl.SetDataFieldDateTime(const Value: String);
begin
  FFieldDataLink.FieldName := IfThen(Value = '', '-', Value) + ';' + DataFieldOffset
end;

function TDateTimeOffsetFieldControl.GetDataFieldOffset: String;
var
  LFieldNames: String;
  I, L: Integer;
begin
  LFieldNames := FFieldDataLink.FieldName;
  I := Pos(';', LFieldNames);
  L := Length(LFieldNames);
  if (I < L - 1) or ((I = L - 1) and (LFieldNames[L] <> '-')) then
    Result := Copy(LFieldNames, I + 1, L - I)
  else
    Result := '';
end;

procedure TDateTimeOffsetFieldControl.SetDataFieldOffset(const Value: String);
begin
  FFieldDataLink.FieldName := DataFieldDateTime + ';' + IfThen(Value = '', '-', Value)
end;

procedure TDateTimeOffsetFieldControl.FieldDataLinkOnDataChanged(Sender: TObject);
var
  LDateTimeField, LOffsetField, LDateTimeOffsetField: TField;
  LDateTimeOffsetValue: String;
  LOffset: Int16;
begin
  if (FFieldDataLink.DataIndepended)
      or (not FFieldDataLink.Editing)
      or (FFieldDataLink.FieldsCount <> 2)
      or (Length(FDataFieldDateTimeOffset) = 0)
  then
    Exit;

  LDateTimeOffsetField := FFieldDataLink.DataSet.FindField(FDataFieldDateTimeOffset);
  if (not Assigned(LDateTimeOffsetField)) or (not LDateTimeOffsetField.CanModify) then Exit;

  LDateTimeField := FFieldDataLink.Fields[0];
  if not Assigned(LDateTimeField) then Exit;
  if LDateTimeField.IsNull then
    LDateTimeOffsetField.Clear
  else begin
    LDateTimeOffsetValue := SBaseVariantFunctions.XMLEncode(LDateTimeField.AsVariant);
    LOffsetField := FFieldDataLink.Fields[1];
    if Assigned(LOffsetField) and (not LOffsetField.IsNull) then begin
      LOffset := LOffsetField.AsInteger;
      if LOffset < 0 then begin
        LDateTimeOffsetValue := LDateTimeOffsetValue + '-';
        LOffset := -LOffset;
      end else
        LDateTimeOffsetValue := LDateTimeOffsetValue + '+';
      LDateTimeOffsetField.AsString := LDateTimeOffsetValue + SBaseStringFunctions.ZeroPredicatedUInt(LOffset div 60, 2) + ':' + SBaseStringFunctions.ZeroPredicatedUInt(LOffset mod 60, 2);
    end;
  end;
end;

{ Simple Functions }

//function BooleanFieldAsInteger(AField: TBooleanField): ShortInt;
//begin
//  if AField.AsBoolean then
//    Result := 1
//  else
//    Result := 0
//end;

//function BooleanFieldAsStringInteger(AField: TBooleanField): String;
//begin
//  if AField.IsNull then
//    Result := 'NULL'
//  else if AField.AsBoolean then
//    Result := '1'
//  else
//    Result := '0'
//end;

function GetFieldsCanModify(const ADataSet: TDataSet; const ANames: String): Boolean;
var
  I: Integer;
  Fields: {$IFDEF DELPHIXE3}TList<TField>{$ELSE}TList{$ENDIF};
begin
  if (ADataSet = nil) or ((not ADataSet.Active) and (ADataSet.FieldCount = 0)) then
    Result := False
  else if Pos(';', ANames) <> 0 then begin
    Result := True;
    Fields := {$IFDEF DELPHIXE3}TList<TField>{$ELSE}TList{$ENDIF}.Create;
    try
      ADataSet.GetFieldList(Fields, ANames);
      for I := 0 to Fields.Count - 1 do
        if not TField(Fields[I]).CanModify then begin
          Result := False;
          Exit
        end;
    finally
      Fields.Free;
    end;
  end else
    Result := ADataSet.FieldByName(ANames).CanModify;
end;

function SetDataSetFieldsReadOnly(ADataSet: TDataSet; AWriteableFields: string; AWriteableIfEmptyFields: string = ''): Boolean;
var
  I: integer;
begin
  Result := True;

  AWriteableFields        := UpperCase(AWriteableFields);
  AWriteableIfEmptyFields := UpperCase(AWriteableIfEmptyFields);

  for I := 0 to Pred(ADataSet.FieldCount) do
    with ADataSet.Fields[I] do
      if InArray(AWriteableFields, UpperCase(FieldName), ',') then begin
        ReadOnly := False;
        Result   := False;
      end else if (AWriteableIfEmptyFields <> '') and IsNull and InArray(AWriteableIfEmptyFields, UpperCase(FieldName), ',') then begin
        ReadOnly := False;
        Result   := False;
      end else
        ReadOnly := True;
end;

(*
function DataSetForeignIndex(ADataSet: TDataSet; AField: TField; AKey: Variant; AForInsertOnly: Boolean = False): Boolean;
var
  FFiltered: Boolean;
  LDataSetHelper: TBaseDataSetHelper;
begin
  Result := False;
  if ADataSet.Active then begin
    LDataSetHelper := ADataSet.Helper;
    if ADataSet.Editing then ADataSet.Post;
    if LDataSetHelper.CachedUpdates then begin
      ADataSet.DisableControls;
      FFiltered := ADataSet.Filtered;
      try
        ADataSet.Filtered := False;
        ADataSet.Last;
        while not ADataSet.Bof do begin
          if ((not AForInsertOnly) or AField.DataSet.Inserted) and (AField.AsInteger <> AKey) then begin
            ADataSet.Edit;
            AField.AsInteger := AKey;
            ADataSet.Post
          end;
          ADataSet.Prior
        end;
      finally
        if FFiltered then ADataSet.Filtered := True;
        ADataSet.EnableControls
      end;
      Result := LDataSetHelper.UpdatesPending;
    end;
  end;
end;
*)

function DataSetFieldQuoteName(const AName: String): String;
begin
  Result := QuoteString(AName, '[')
end;

{ TDataSetStateBookmark }

procedure TDataSetStateBookmark.Clear;
begin
  NotNull           := False;
//  Filtered          := False;
  Filter            := '';;
//  UpdateStatusFilter:= [];
//  ControlsDisabled  : Boolean;
end;

function TDataSetStateBookmark.GetAsVariant: Variant;
begin
  if NotNull then
    Result := VarArrayOf
              (
                [
                  VarFromSet(UpdateStatusFilter, SizeOf(UpdateStatusFilter)),
                  Filtered,
                  Filter
                ]
              )
  else
    Result := unAssigned
end;

procedure TDataSetStateBookmark.SetAsVariant(const Value: Variant);
begin
  if not VarIsArray(Value) then
    Clear
  else begin
    VarToSet(Value[0], UpdateStatusFilter, SizeOf(UpdateStatusFilter));
    Filtered            := Value[1];
    Filter              := Value[2];
    NotNull             := True;
  end;
end;

{$IFDEF FASTSCRIPT_RTTI}

{ TfsMSConnectionInfoMessageEvent }

function TfsSQLConnectionProviderInfoMessageEvent.GetMethod: Pointer;
begin
  Result := @TfsSQLConnectionProviderInfoMessageEvent.DoEvent;
end;

procedure TfsSQLConnectionProviderInfoMessageEvent.DoEvent(AConnection: TCustomConnection; const AInfoMessage: String);
begin
  CallHandler([AConnection, AInfoMessage]);
end;

{ TFunctions_TBaseDataModule }

constructor TFunctions_TBaseDataModule.Create(AScript: TfsScript);
const
  SCategory: String = '';
begin
  inherited;

  with AScript do begin
    AddMethod('function SetDataSetFieldsReadOnly(ADataSet: TDataSet; AWriteableFields: String; AWriteableIfEmptyFields: String = ''): Boolean', Call_Method);
  end;

//  AScript.AddEnum('TNotFoundAction', 'nfClear, nfNone, nfError');
  AScript.AddEnum('TDataSetStateBookmarkRestores', 'sbrUpdateStatusFilter, sbrFiltered, sbrFilter, sbrDisableCount');
  AScript.AddEnum('TDataSourceBookmarkRestores', 'sbrKeyFieldValues, sbrRecordIndex, sbrRowIndex');
  AScript.AddEnum('TDataSetReOpenFlag', 'rofSavePosition, rofOnlyIfChanged, rofCloseIfParamsRequired');
  AScript.AddEnum('TBaseDataSetObject', 'bdsoOriginal, bdsoCDS, bdsoCDSCached, bdsoCDSPublic, bdsoMemory');

  with AScript.AddClass(TSQLConnectionProvider, TSQLConnectionProvider.ClassParent.ClassName) do begin
    AddMethod('function ExecSQL(ASQLText: String; Params: Variant = nil): Variant', Call_TBaseDataModule_Method);

    AddMethod('function OpenSQLQuery(ASQLText: String; Params: Variant = nil): TDataSet', Call_TBaseDataModule_Method);
    AddMethod('procedure CloseSQLQuery', Call_TBaseDataModule_Method);

    AddMethod('function Clone(AOwner: TComponent): TSQLConnectionProvider', Call_TBaseDataModule_Method);

    AddEvent('AfterConnect', TfsNotifyEvent);
    AddEvent('AfterDisconnect', TfsNotifyEvent);
    AddEvent('OnInfoMessage', TfsSQLConnectionProviderInfoMessageEvent);
  end;

  with AScript.AddClass(TSQLConnectionProviderStorage, TSQLConnectionProviderStorage.ClassParent.ClassName) do begin
    AddMethod('function AssignConnection(AOwner: TComponent; AParams: TNamedVariants; AIndepended: Boolean; AUsedBaseSQLConnectionProvider: TSQLConnectionProvider = nil): TSQLConnectionProvider', Call_TBaseDataModuleStorage_Method);
    AddMethod('procedure ReleaseConnection(AOwner: TComponent; AStorageDataModule: TSQLConnectionProvider)', Call_TBaseDataModuleStorage_Method);

    AddMethod('procedure GetConnection(AOwner: TComponent; AParams: TNamedVariants; AIndepended: Boolean; ASQLDataSetProvider: TSQLDataSetProvider)', Call_TBaseDataModuleStorage_Method);
    AddMethod('procedure ResetConnection(AOwner: TComponent; ABaseDataSource: ASQLDataSetProvider)', Call_TBaseDataModuleStorage_Method);
  end;

  with AScript.AddClass(TSQLDataSetProvider, TSQLDataSetProvider.ClassParent.ClassName) do begin
    AddMethod('function ParamByName(AName: String): TParam', Call_TSQLDataSetProvider_Method);
    AddMethod('procedure OpenNext', Call_TSQLDataSetProvider_Method);
    AddMethod('function GetXML: String', Call_TSQLDataSetProvider_Method);
    AddMethod('procedure ApplyUpdates', Call_TSQLDataSetProvider_Method);
    AddMethod('function ReOpen(AReOpenFlag: Variant = nil): Boolean', Call_TSQLDataSetProvider_Method);
    AddMethod('procedure CopyRecord(const AExcludedFields: String; const AConstants: Variant)', Call_TSQLDataSetProvider_Method);
  end;

  with AScript.AddClass(TFieldReadOnly, TFieldReadOnly.ClassParent.ClassName) do begin
    AddMethod('procedure DisableEdit(AField: TField)', Call_TBaseDataModule_Method);
    AddMethod('procedure RestoreEdit(AField: TField)', Call_TBaseDataModule_Method);
  end;

  with AScript.AddClass(TMasterDetailDataSetControl, TMasterDetailDataSetControl.ClassParent.ClassName) do begin
    AddProperty('MasterDataSet', 'TDataSet', Get_TMasterDetailDataSetControl_Property);
    AddProperty('DetailDataSet', 'TDataSet', Get_TMasterDetailDataSetControl_Property);

    AddMethod('procedure ConditionChanged(AForce: Boolean = False)', Get_TMasterDetailDataSetControl_Method);

    AddEvent('AfterActivate', TfsNotifyEvent);
    AddEvent('AfterDeactivate', TfsNotifyEvent);
//    AddEvent('OnMasterDataChanged', TfsNotifyEvent);
    AddEvent('OnMasterFieldsChanged', TfsNotifyEvent);
  end;

  with AScript.AddClass(TSQLDataSetProviderApplyOptions, TSQLDataSetProviderApplyOptions.ClassParent.ClassName) do begin
    AddMethod('procedure TempBeginDump', Call_TSQLDataSetProviderApplyOptions_Method);
    AddMethod('procedure TempPostDump(AUpdateStatusFilter: Variant = 0; ADeleteAfterDump: Boolean = True)', Call_TSQLDataSetProviderApplyOptions_Method);
  end;

  with AScript do begin
    if BaseSQLConnectionProvider <> nil then
      AddComponent(BaseSQLConnectionProvider, 'DB');

    if BaseSQLConnectionProviderStorage <> nil then
      AddComponent(BaseSQLConnectionProviderStorage, 'DBStorage');

    AddMethod('function DataSetFieldsToXML(AFields: TFields): String;', Call_TBaseDataModule_Method, 'ctConv');
    AddMethod('function DataSetToXML(ADataSet: TDataSet; ATag: String = ''row''; AKeyFieldNames: String = ''; AStatusFieldName: String = ''''; AExcludedFields: String = ''''; AMaxRowCount: Integer = -1): String;', Call_TBaseDataModule_Method, 'ctConv');
  end;
end;

function TFunctions_TBaseDataModule.Call_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'SETDATASETFIELDSREADONLY' then
    Result := SetDataSetFieldsReadOnly(VarToPointer(Caller.Params[0]), Caller.Params[1], VarToStr(Caller.Params[2]))
{
  else if MethodName = 'DATASETINSERTING' then
    Result := DataSetInserting(VarToPointer(Caller.Params[0]))
  else if MethodName = 'DATASETEDITING' then
    Result := DataSetEditing(VarToPointer(Caller.Params[0]))
}
end;

function TFunctions_TBaseDataModule.Call_TSQLDataSetProviderApplyOptions_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  LUpdateStatusSet: TUpdateStatusSet;
begin
  if MethodName = 'TEMPBEGINDUMP' then
    TSQLDataSetProviderApplyOptions(Instance).TempBeginDump
  else if MethodName = 'TEMPPOSTDUMP' then begin
    if VarIsPresent(Caller.Params[0]) then
      VarToSet(Caller.Params[0], LUpdateStatusSet, SizeOf(LUpdateStatusSet))
    else
      LUpdateStatusSet := Default_UpdateStatusFilter;
    TSQLDataSetProviderApplyOptions(Instance).TempPostDump(LUpdateStatusSet, VarToBoolDef(Caller.Params[1], True));
  end;
end;

function TFunctions_TBaseDataModule.Call_TBaseDataModule_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if ClassType = nil then begin
    if MethodName = 'DATASETFIELDSTOXML' then
      Result := DataSetFieldsToXML(VarToPointer(Caller.Params[0]), VarToStr(Caller.Params[1]))
    else if MethodName = 'DATASETTOXML' then
      Result := DataSetToXML
                (
                  VarToPointer(Caller.Params[0]),
                  VarToStrDef(Caller.Params[1], 'row'),
                  VarToStr(Caller.Params[2]),
                  VarToStr(Caller.Params[3]),
                  VarToStr(Caller.Params[4]),
                  VarToIntDef(Caller.Params[5], -1)
                )
  end else if ClassType.InheritsFrom(TSQLConnectionProvider) then begin
    if MethodName = 'CLONE' then
      Result := Integer(TSQLConnectionProvider(Instance).Clone(VarToPointer(Caller.Params[0])))
    else if MethodName = 'EXECSQL' then
      Result := TSQLConnectionProvider(Instance).ExecSQL(Caller.Params[0], TNamedVariants.Create(Caller.Params[1], True)).AsVariant
    else if MethodName = 'OPENSQLQUERY' then
      Result := Integer(TSQLConnectionProvider(Instance).OpenSQLQuery(Caller.Params[0], TNamedVariants.Create(Caller.Params[1], True)))
    else if MethodName = 'CLOSESQLQUERY' then
      TSQLConnectionProvider(Instance).CloseSQLQuery

  end else if ClassType = TFieldReadOnly then begin
    if MethodName = 'DISABLEEDIT' then
      TFieldReadOnly.DisableEdit(VarToPointer(Caller.Params[0]))
    else if MethodName = 'RESTOREEDIT' then
      TFieldReadOnly.RestoreEdit(VarToPointer(Caller.Params[0]))
  end;
end;

function TFunctions_TBaseDataModule.Call_TBaseDataModuleStorage_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'ASSIGNCONNECTION' then
    Result := Integer(TSQLConnectionProviderStorage(Instance).AssignConnection(TComponent(VarToPointer(Caller.Params[0])), TNamedVariants.Create(Caller.Params[1], True), Caller.Params[2] = True, TSQLConnectionProvider(VarToPointer(Caller.Params[3]))))
  else if MethodName = 'RELEASECONNECTION' then
    TSQLConnectionProviderStorage(Instance).ReleaseConnection(TComponent(VarToPointer(Caller.Params[0])), TSQLConnectionProvider(VarToPointer(Caller.Params[1])))

  else if MethodName = 'GETCONNECTION' then
    TSQLConnectionProviderStorage(Instance).GetConnection(TComponent(VarToPointer(Caller.Params[0])), TNamedVariants.Create(Caller.Params[1], True), Caller.Params[2] = True, TSQLDataSetProvider(VarToPointer(Caller.Params[3])))
  else if MethodName = 'RESETCONNECTION' then
    TSQLConnectionProviderStorage(Instance).ResetConnection(TComponent(VarToPointer(Caller.Params[0])), TSQLDataSetProvider(VarToPointer(Caller.Params[1])))
end;

function TFunctions_TBaseDataModule.Call_TSQLDataSetProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  LDataSetReOpenFlags: TDataSetReOpenFlags;
begin
  if MethodName = 'PARAMBYNAME' then
    Result := Integer(TSQLDataSetProvider(Instance).Params.ParamByName(Caller.Params[0]))
  else if MethodName = 'OPENNEXT' then
    TSQLDataSetProvider(Instance).OpenNext
//  else if MethodName = '!ASNEXTRESULTSET' then
//    Result := TSQLDataSetProvider(Instance).!asNextResultSet
  else if MethodName = 'GETXML' then
    Result := TSQLDataSetProvider(Instance).GetXML
  else if MethodName = 'APPLYUPDATES' then
    TSQLDataSetProvider(Instance).ApplyUpdates

  else if MethodName = 'REOPEN' then begin
    if VarisPresent(Caller.Params[0]) then
      VarToSet(Caller.Params[0], LDataSetReOpenFlags, SizeOf(LDataSetReOpenFlags))
    else
      LDataSetReOpenFlags := [];

    TSQLDataSetProvider(Instance).ReOpen(LDataSetReOpenFlags)
  end else if MethodName = 'COPYRECORD' then
    TSQLDataSetProvider(Instance).CopyRecord(VarToStr(Caller.Params[0]), TNamedVariants.Create(Caller.Params[0], False))
{$IFDEF DEBUG}
  else
    Assert(False)
{$ENDIF}
end;

function TFunctions_TBaseDataModule.Get_TMasterDetailDataSetControl_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'MASTERDATASET' then
    Result := Integer(TMasterDetailDataSetControl(Instance).MasterDataSet)
  else if PropName = 'DETAILDATASET' then
    Result := Integer(TMasterDetailDataSetControl(Instance).DetailDataSet)
end;

function TFunctions_TBaseDataModule.Get_TMasterDetailDataSetControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'CONDITIONCHANGED' then
    TMasterDetailDataSetControl(Instance).ConditionChanged(Caller.Params[0] = True);
end;
{$ENDIF}

{$IFDEF PACKAGE}
{ TSQLDataSetProviderTreadEvents }

function TSQLDataSetProviderTreadEvents.GetInstance: TPersistent;
begin
  Result := TCustomSQLDataSetProvider(GetComponent(0)).Thread;
end;

function TSQLDataSetProviderTreadEvents.GetName: String;
begin
  Result := 'Thread';
end;
{$ENDIF}

{ TSQLDataSetProviderApplyOptions }

procedure TSQLDataSetProviderApplyOptions.ApplyUpdates;
{$IFNDEF PACKAGE}
var
  LBookmark: TDataSourceStateBookmark;
  LInterpreter: TInternalProviderInterpreterClass;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  FTempDumpActive := False;

  if FOwner.Editing then FOwner.Post;
  if not FOwner.UpdatesPending then Exit;
  LInterpreter := GetInterpreter;

  FOwner.BeginUpdate(LBookmark);
  try
    if FCommitSQL.Count > 0 then begin
      Assert(not FStatusFieldName.IsEmpty);
      FOwner.UpdateStatusFilter := [usDeleted, usInserted, usModified];
      if not FOwner.IsEmpty then begin
        InternalTempBeginDump;
        try
          FOwner.First;
          if FMethod = damTempTable then begin
            while not FOwner.Eof do
              FOwner.ConnectionProvider.Connection.ExecSQL(LInterpreter.GenerateInsertTableScript(FOwner, FInternalTempTable, FStatusFieldName));
            FOwner.ConnectionProvider.Connection.ExecSQL(FCommitSQL.Text, [FInternalTempTable]);
          end else // if FMethod = damXML then
            FOwner.ConnectionProvider.Connection.ExecSQL(FCommitSQL.Text, [DataSetToXML(FOwner, XMLTag, FOwner.KeyFieldNames, FStatusFieldName)]);
        finally
          InternalTempEndDump;
        end;
      end;
    end else begin
      if FMethod = damTempTable then InternalTempBeginDump;
      try
        // DELETE
        if FOwner.SQLDelete.Count > 0 then begin
          FOwner.UpdateStatusFilter := [usDeleted];
          if not FOwner.IsEmpty then begin
            FOwner.First;
            if FMethod = damTempTable then begin
              while not FOwner.Eof do
                FOwner.ConnectionProvider.Connection.ExecSQL(LInterpreter.GenerateInsertTableScript(FOwner, FInternalTempTable));
              FOwner.ConnectionProvider.Connection.ExecSQL(FOwner.SQLDelete.Text + SConst_CR + LInterpreter.GenerateClearTableScript(FInternalTempTable), [FInternalTempTable]);
            end else // if FMethod = damXML then
              FOwner.ConnectionProvider.Connection.ExecSQL(FOwner.SQLDelete.Text, [DataSetToXML(FOwner, XMLTag, FOwner.KeyFieldNames)]);
          end;
        end;

        // UPDATE
        if FOwner.SQLUpdate.Count > 0 then begin
          FOwner.UpdateStatusFilter := [usModified];
          if not FOwner.IsEmpty then begin
            FOwner.First;
            if FMethod = damTempTable then begin
              while not FOwner.Eof do
                FOwner.ConnectionProvider.Connection.ExecSQL(LInterpreter.GenerateInsertTableScript(FOwner, FInternalTempTable));
              FOwner.ConnectionProvider.Connection.ExecSQL(FOwner.SQLUpdate.Text + SConst_CR + LInterpreter.GenerateClearTableScript(FInternalTempTable), [FInternalTempTable]);
            end else // if FMethod = damXML then
              FOwner.ConnectionProvider.Connection.ExecSQL(FOwner.SQLUpdate.Text, [DataSetToXML(FOwner, XMLTag, FOwner.KeyFieldNames)]);
          end;
        end;

        // INSERT
        if FOwner.SQLInsert.Count > 0 then begin
          FOwner.UpdateStatusFilter := [usInserted];
          if not FOwner.IsEmpty then begin
            FOwner.First;
            if FMethod = damTempTable then begin
              while not FOwner.Eof do
                FOwner.ConnectionProvider.Connection.ExecSQL(LInterpreter.GenerateInsertTableScript(FOwner, FInternalTempTable));
              FOwner.ConnectionProvider.Connection.ExecSQL(FOwner.SQLInsert.Text{ + SConst_CR + LInterpreter.GenerateClearTableScript(FInternalTempTable)}, [FInternalTempTable]);
            end else // if FMethod = damXML then
              FOwner.ConnectionProvider.Connection.ExecSQL(FOwner.SQLInsert.Text, [DataSetToXML(FOwner, XMLTag, FOwner.KeyFieldNames)]);
          end;
        end;
      finally
        if FMethod = damTempTable then InternalTempEndDump;
      end;
    end;
    FOwner.InternalApplyUpdates;
    FOwner.InternalCommitUpdates;
  finally
    FOwner.EndUpdate(LBookmark, [sbrUpdateStatusFilter, sbrDisableCount], []);
  end;
{$ENDIF}
end;

procedure TSQLDataSetProviderApplyOptions.AssignTo(Dest: TPersistent);
begin
  TSQLDataSetProviderApplyOptions(Dest).Method    := Method;
//  TSQLDataSetProviderApplyOptions(Dest).RowCount  := RowCount;
  TSQLDataSetProviderApplyOptions(Dest).TempTable := TempTable;
  TSQLDataSetProviderApplyOptions(Dest).XMLTag    := XMLTag;
  TSQLDataSetProviderApplyOptions(Dest).StatusFieldName := StatusFieldName;
  TSQLDataSetProviderApplyOptions(Dest).CommitSQL := CommitSQL;
end;

procedure TSQLDataSetProviderApplyOptions.InternalTempBeginDump;
var
  LCommandText: String;
begin
  FTempDumpActive := False;

  if FTempTable.IsEmpty then
    FInternalTempTable := NewGUIdString
  else
    FInternalTempTable := FTempTable;

  LCommandText := GetInterpreter.GenerateCreateTableScript(FOwner, FInternalTempTable, FStatusFieldName);
  FOwner.ConnectionProvider.Connection.ExecSQL(LCommandText);

  FTempDumpActive := True;
end;

procedure TSQLDataSetProviderApplyOptions.TempBeginDump;
begin
//  FDeleteAfterDump := ADeleteAfterDump;
//  if FTempDumpActive then InternalTempEndDump;
  InternalTempBeginDump;
end;

procedure TSQLDataSetProviderApplyOptions.TempPostDump(AUpdateStatusFilter: TUpdateStatusSet; ADeleteAfterDump: Boolean);
var
  LInterpreter: TInternalProviderInterpreterClass;
  LDataSet: TDataSet;
  LDataSourceStateBookmark: TDataSourceStateBookmark;
  LDataSourceRestore: TDataSourceBookmarkRestores;
begin
  Assert(FTempDumpActive);
  if FOwner.Editing then FOwner.Post;

  LDataSet := FOwner.ActiveDataSet;
  if ADeleteAfterDump then
    LDataSourceRestore := []
  else
    LDataSourceRestore := [sbrRecordIndex, sbrKeyFieldValues];
  FOwner.BeginUpdate(LDataSourceStateBookmark);
  try
    FOwner.UpdateStatusFilter := AUpdateStatusFilter;
    if not LDataSet.IsEmpty then begin
      LInterpreter := GetInterpreter;
      LDataSet := FOwner.ActiveDataSet;

        LDataSet.First;
        while not LDataSet.Eof do
          FOwner.ConnectionProvider.Connection.ExecSQL(LInterpreter.GenerateInsertTableScript(LDataSet, FInternalTempTable, FStatusFieldName));

        if ADeleteAfterDump and (AUpdateStatusFilter <> [usDeleted]) then
          with LDataSet do
            while (not IsEmpty) and (not (usDeleted in AUpdateStatusFilter) or (FOwner.UpdateStatus <> usDeleted)) do Delete;
    end;
  finally
    FOwner.EndUpdate(LDataSourceStateBookmark, [sbrUpdateStatusFilter, sbrDisableCount], LDataSourceRestore);
  end;
end;

procedure TSQLDataSetProviderApplyOptions.TempCommitDump;
begin
  if FCommitSQL.Count > 0 then
    FOwner.ConnectionProvider.Connection.ExecSQL(FCommitSQL.Text, [FInternalTempTable]);
  // FOwner.InternalCommitUpdates;
end;

procedure TSQLDataSetProviderApplyOptions.InternalTempEndDump;
var
  LCommandText: String;
begin
  if FInternalTempTable.IsEmpty then Exit;

  LCommandText := GetInterpreter.GenerateDropTableScript(FInternalTempTable);
  FOwner.ConnectionProvider.Connection.ExecSQL(LCommandText);

  FInternalTempTable := '';
end;

procedure TSQLDataSetProviderApplyOptions.TempEndDump;
begin
  FTempDumpActive := False;
  InternalTempEndDump;
end;

function TSQLDataSetProviderApplyOptions.GetInterpreter: TInternalProviderInterpreterClass;
begin
  Assert
  (
    Assigned(FOwner)
    and Assigned(FOwner.ConnectionProvider)
    and Assigned(FOwner.ConnectionProvider.Connection)
  );
  Result := FOwner.ConnectionProvider.Connection.Interpreter;
  Assert(Assigned(Result));
end;

function TSQLDataSetProviderApplyOptions.IsXMLTagStored: Boolean;
begin
  Result := FXMLTag <> XMLConst_RECORD;
end;

constructor TSQLDataSetProviderApplyOptions.Create(AOwner: TCustomSQLDataSetProvider);
begin
  Assert(Assigned(AOwner));
  FOwner := AOwner;
  FCommitSQL := TStringList.Create;
  FXMLTag := XMLConst_RECORD;

  inherited Create;
end;

destructor TSQLDataSetProviderApplyOptions.Destroy;
begin
  FCommitSQL.Free;
  inherited;
end;

procedure TSQLDataSetProviderApplyOptions.SetApplyMethod(const AValue: TSQLDataSetProviderApplyMethod);
var
  LDataSet: TDataSet;
begin
  if FMethod <> AValue then begin
    if AValue <> damDefault then begin
      FOwner.CachedUpdates := True;
      if FMethod = damDefault then begin
        LDataSet := FOwner.ActiveDataSet;
        if (LDataSet <> nil) and (LDataSet is TInternalQuery) then with TInternalQuery(LDataSet) do begin
          SQL.Clear;
          SQLInsert.Clear;
          SQLUpdate.Clear;
          SQLRefresh.Clear;
          SQLDelete.Clear;
        end;
      end;
    end;
    FMethod := AValue;
  end;
end;

procedure TSQLDataSetProviderApplyOptions.SetCommitSQL(const AValue: TStrings);
begin
  if FCommitSQL.Text <> AValue.Text then begin
    FCommitSQL.BeginUpdate;
    try
      FCommitSQL.Clear;
      FCommitSQL.AddStrings(AValue);
    finally
      FCommitSQL.EndUpdate;
    end;
  end;
end;

{ TDataSetProviderParams }

function TDataSetProviderParams.GetParamClass: TParamClass;
begin
  Result := TDataSetProviderParam;
end;

function TDataSetProviderParams.FindParam(const Value: string): TDataSetProviderParam;
begin
  Result := TDataSetProviderParam(inherited FindParam(Value))
end;

function TDataSetProviderParams.GetItem(Index: Integer): TDataSetProviderParam;
begin
  Result := TDataSetProviderParam(inherited Items[Index]);
end;

procedure TDataSetProviderParams.SetItem(Index: Integer; const Value: TDataSetProviderParam);
begin
  inherited Items[Index] := Value
end;

{$IFNDEF PACKAGE}
{ TMDFieldDataLinkHelper }

function TMDFieldDataLinkHelper.Evaluate(const AExpression: string; AConditionParser: PExprParser): Variant;
var
  LExprEval : TMDFieldDataLinkExprEvaluator;
  LParser   : TExprParser;
begin
  if DataSet.IsEmpty or AExpression.IsEmpty then
    Exit(unAssigned);

  if (AConditionParser <> nil) and (AConditionParser^ <> nil) then
    LParser := AConditionParser^
  else begin
    LParser := TExprParser.Create(DataSet, AExpression, [], [poExtSyntax], '', nil, FieldTypeMap);
    if AConditionParser <> nil then
      AConditionParser^ := LParser;
  end;

  LExprEval := TMDFieldDataLinkExprEvaluator.Create(Self);

  try
    LExprEval.Data := Pointer(LParser.FilterData);
    Result := LExprEval.Evaluate;
  finally
    if AConditionParser = nil then
      LParser.Free;

    LExprEval.Free;
  end;
end;

function TMDFieldDataLinkHelper.EvaluateBoolean(const AExpression: string; ADefaultValue: Boolean): Boolean;
begin
  Result := SBaseVariantFunctions.IsEmpty(Evaluate(AExpression), ADefaultValue) = True
end;
{$ENDIF}

{ TBasePublicDataSet }

function TBasePublicDataSet.Active(ALiteMode: Boolean): Boolean;
begin
  Result := DataSet.Active(ALiteMode)
end;

constructor TBasePublicDataSet.Create(ASQLConnectionProvider: TComponent; ACommandText: String; AParams: TParams);
begin
{$IFDEF DEBUG}
  Assert(Assigned(ASQLConnectionProvider) and (ASQLConnectionProvider is TSQLConnectionProvider));
{$ENDIF}
  FSQLConnectionProvider  := ASQLConnectionProvider;
  FCommandText            := ACommandText;

  FParams                 := TParams.Create(nil);
  FParams.Assign(AParams);

  DataSet := TPublicDataSet.Create;
  Owners  := TList<TComponent>.Create;
end;

destructor TBasePublicDataSet.Destroy;
begin
  FreeAndNil(Owners);
  FreeAndNil(DataSet);
  FreeAndNil(FParams);

  inherited;
end;

procedure TBasePublicDataSet.Open(ALiteMode: Boolean);
begin
  DataSet.Open(FSQLConnectionProvider, FCommandText, FParams, ALiteMode)
end;

initialization
  BaseSQLConnectionProvider := TSQLConnectionProvider.Create(nil);
  BaseSQLConnectionProviderStorage := TSQLConnectionProviderStorage.Create(BaseSQLConnectionProvider);

  RegisterClasses
  (
    [
      TDataSetProviderParam,
      TDataSetProviderParams,
      TSQLConnectionProvider,
      TSQLConnectionProviderStorage,
      TCustomSQLDataSetProvider,
      TSQLDataSetProvider,
      TBaseDataSetFieldAutomations,
      TMasterDetailDataSetControl,
      TSQLDataSetProviderApplyOptions
    ]
  );
//  TBaseDataSetHelper.RegisterHelperClass(TSQLDataSetProvider, TSQLDataSetProviderHelper);

{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TFunctions_TBaseDataModule);
{$ENDIF}

finalization
  UnRegisterClasses
  (
    [
      TDataSetProviderParam,
      TDataSetProviderParams,
      TSQLConnectionProvider,
      TSQLConnectionProviderStorage,
      TCustomSQLDataSetProvider,
      TSQLDataSetProvider,
      TBaseDataSetFieldAutomations,
      TMasterDetailDataSetControl,
      TSQLDataSetProviderApplyOptions
    ]
  );
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TFunctions_TBaseDataModule);
{$ENDIF}

  FreeAndNil(BaseSQLConnectionProvider);

end.
