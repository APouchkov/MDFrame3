unit SBaseDataHelperInternal;
{$I config.inc}

interface

uses
  Classes, DB, SysUtils, TypInfo,
  MemDS, DBAccess, Uni, UniProvider, UniDacVcl, CRAccess,
  OLEDBCUni, OLEDBAccessUni, VirtualTable, MemData, UniConnectForm, CRThreadUni

{$IFDEF SERVICEBROKER_SUPPORT}
//  , SBaseDataServiceBroker
{$ENDIF}

{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter, fs_idbrtti, fs_itools
{$ENDIF}

  //, Controls, Forms, System.UITypes
  ;

type
  PWordSet = PWord;

  TInternalConnectDialog = class(TUniConnectDialog)
  private
    FDisableLoginChange: Boolean;
    FDisableConnectionChange: Boolean;

    FHidePort: Boolean;
  published
    property DisableLoginChange: Boolean read FDisableLoginChange write FDisableLoginChange;
    property DisableConnectionChange: Boolean read FDisableConnectionChange write FDisableConnectionChange;

    property HidePort: Boolean read FHidePort write FHidePort;
  end;

  TInternalConnection = class;
  TInternalConnectionClass = class of TInternalConnection;

  TInternalConnectionOnInfoMessage = procedure(AConnection: TInternalConnection; const AInfoMessage: String) of object;

  TInternalProviderInterpreter = class;
  TInternalProviderInterpreterClass = class of TInternalProviderInterpreter;
  TInternalProviderInterpreterRecord = record
    ProviderName: String;
    Interpreter: TInternalProviderInterpreterClass;
  end;

  TInternalProviderInterpreter = class
  strict private
    class var FInterpreters: TArray<TInternalProviderInterpreterRecord>;
  protected
    class procedure Release;
  public
    class procedure RegisterInterpreter(const AProviderName: String; AInterpreter: TInternalProviderInterpreterClass);
    class function  GetInterpreter(const AProviderName: String): TInternalProviderInterpreterClass;

    class function VarTypeToDataBaseType(AVarType: TVarType): String; dynamic; abstract;
    class function VariantToDataBaseString(const AValue: Variant; const ABinaryStringFormat: Boolean = False) : String; dynamic; abstract;
    class function FieldTypeToDataBaseType(AField: TField): String; dynamic; abstract;

    class function DataBaseDateFormat(ADate: Variant; AQuoted: Boolean = True): String; dynamic; abstract;
    class function DataBaseTimeFormat(ADateTime: Variant; AQuoted: Boolean = True): String; dynamic; abstract;
    class function DataBaseDateTimeFormat(ADateTime: Variant; AQuoted: Boolean = True): String; dynamic; abstract;

    class function GenerateCreateTableScript(ADataSet: TDataSet; const ATableName: String; AStatusFieldName: String = ''): String; dynamic; abstract;
    class function GenerateInsertTableScript(ADataSet: TDataSet; const ATableName: String; AStatusFieldName: String = ''): String; dynamic; abstract;
    class function GenerateClearTableScript(const ATableName: String): String; dynamic; abstract;
    class function GenerateDropTableScript(const ATableName: String): String; dynamic; abstract;

    class function GenerateGetTranCountScript: String; dynamic; abstract;
    class function GenerateRollBackTranScript: String; dynamic; abstract;
    class function GenerateRollBackTranIfPresentScript: String; dynamic; abstract;

//    procedure CreateTempTable;
    class function GenerateReceiveScript(const Queue: string; const TimeoutMSec: Integer): String; dynamic; abstract;
  end;

  TInternalConnection = class(TUniConnection)
  strict private
    FInterpreter: TInternalProviderInterpreterClass;
    FOnInfoMessage: TInternalConnectionOnInfoMessage;

    procedure ConnectionInfoMessage(E: Exception);
    procedure SetOnInfoMessage(AValue: TInternalConnectionOnInfoMessage);
    procedure DoSetOnInfoMessage;
//    procedure EventOnConnectionLost(Sender: TObject; Component: TComponent; ConnLostCause: TConnLostCause; var RetryMode: TRetryMode);

//    property OnConnectionLost;
  private
    function GetProviderName: string;
    procedure SetProviderName(const Value: string);
  protected
    procedure SetIConnection(Value: TCRConnection); override;
    procedure AssignTo(Dest: TPersistent); override;

//    function GetInTransaction: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;

    class function IsConnectionError(E: Exception): Boolean;
    class function IsBreakExecError(E: Exception): Boolean;

    procedure DoDefaultError(E: Exception; var Fail: boolean);
    procedure RollbackTran(AIfPresent: Boolean = False);

    property OnInfoMessage: TInternalConnectionOnInfoMessage read FOnInfoMessage write SetOnInfoMessage;
    property Interpreter: TInternalProviderInterpreterClass read FInterpreter;
  published
    property ProviderName: string read GetProviderName write SetProviderName;
  end;

  TInternalConnectForm = class(TUniConnectForm)
  strict private
    FHeightOffset: Integer;
    FHeightPostFix: Integer;
  protected
    procedure DoInit; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TInternalThreadWrapper = class(TCRThreadWrapper)
  protected
    procedure AfterCreate; dynamic; abstract;
  public
    constructor Create(ForceStartTimer: boolean = False);
  end;

//  TInternalThread = class(TCRThread);

{$IFDEF SERVICEBROKER_SUPPORT}
  TCustomSQLServiceBrokerProvider = class(TComponent)
  protected
    procedure ConnectionChanged; dynamic; abstract;
  end;
{$ENDIF}

  TBaseDataSetSupportedAction = (bdssaSQL, bdssaKeyFields, bdssaRefreshAfterInsert, bdssaRefreshAfterUpdate, bdssaRefreshBeforeEdit);
  TBaseDataSetSupportedActions = set of TBaseDataSetSupportedAction;

  TBaseLocateOption = (bloCaseInsensitive, bloPartialKey, bloNearest, bloNext, bloUp, bloPartialCompare{, blxCharCompare});
  TBaseLocateOptions = set of TBaseLocateOption;

  IBaseDataSetProvider = interface
    ['{BF23A93D-DF02-4B3C-846A-D6D8EF4FBF76}']

    function SupportedMethods: TBaseDataSetSupportedActions;

    function GetMasterSource: TDataSource;
    procedure SetMasterSource(AValue: TDataSource);

    function GetMasterFields: String;
    procedure SetMasterFields(const AValue: String);

    function GetDetailFields: String;
    procedure SetDetailFields(const AValue: String);

    function GetIndexFieldNames: String;
    procedure SetIndexFieldNames(const AValue: String);

    function GetKeyFieldNames: String;
    procedure SetKeyFieldNames(const AValue: String);

    function GetCachedUpdates: Boolean;
    procedure SetCachedUpdates(const Value: Boolean);

    function GetUpdateStatusFilter: TUpdateStatusSet;
    procedure SetUpdateStatusFilter(AValue: TUpdateStatusSet);

    function GetReadOnly: Boolean;
    procedure SetReadOnly(const AValue: Boolean);

    procedure RefreshDetail;

    function UpdatesPending: Boolean;
    procedure ApplyUpdates;
    procedure CommitUpdates;
    procedure CancelUpdates;
    procedure RestoreUpdates;
    procedure RevertRecord;

    function LocateEx(const KeyFields: string; const KeyValues: Variant; Options: TBaseLocateOptions): Boolean;
  end;

  TBaseRefreshOption = (broAfterInsert, broAfterUpdate, broBeforeEdit);
  TBaseRefreshOptions = set of TBaseRefreshOption;

  IBaseSQLDataSetProvider = interface
    ['{5C8268F4-A470-4FCB-A87A-8B35413D3764}']

    function SupportedMethods: TBaseDataSetSupportedActions;

    function GetConnection: TCustomConnection;
    procedure SetConnection(AValue: TCustomConnection);

//    function !asNextResultSet: Boolean;
    procedure ClearNextResultSets;

    procedure BreakExec;

    procedure OpenNext;
    procedure RefreshRecord;
    procedure UnPrepare;

    function GetAutoGenerateUpdateScripts: Boolean;
    procedure SetAutoGenerateUpdateScripts(const Value: Boolean);

    function GetSQLDelete: String;
    procedure SetSQLDelete(const Value: String);

    function GetSQLInsert: String;
    procedure SetSQLInsert(const Value: String);

    function GetSQLRefresh: String;
    procedure SetSQLRefresh(const Value: String);

    function GetSQLUpdate: String;
    procedure SetSQLUpdate(const Value: String);

    function  GetRefreshOptions: TBaseRefreshOptions;
    procedure SetRefreshOptions(AValue: TBaseRefreshOptions);
  end;

  TUserMemoryDataSet = class(TVirtualTable, IBaseDataSetProvider)
  private
    FKeyFieldNames: String;
  protected
    function SupportedMethods: TBaseDataSetSupportedActions;

    function GetMasterSource: TDataSource;
    procedure SetMasterSource(AValue: TDataSource);

    function GetMasterFields: String;
    procedure SetMasterFields(const AValue: String);

    function GetDetailFields: String;
    procedure SetDetailFields(const AValue: String);

    function GetIndexFieldNames: String;
    procedure SetIndexFieldNames(const AValue: String); override;

    function GetKeyFieldNames: String;
    procedure SetKeyFieldNames(const AValue: String);

    function GetReadOnly: Boolean;
    procedure SetReadOnly(const Value: Boolean);

    function IGetCachedUpdates: Boolean;
    function IBaseDataSetProvider.GetCachedUpdates = IGetCachedUpdates;

    procedure ISetCachedUpdates(const Value: Boolean);
    procedure IBaseDataSetProvider.SetCachedUpdates = ISetCachedUpdates;

    function GetUpdateStatusFilter: TUpdateStatusSet;
    procedure SetUpdateStatusFilter(AValue: TUpdateStatusSet);

    function ILocateEx(const KeyFields: string; const KeyValues: Variant; Options: TBaseLocateOptions): Boolean;
    function IBaseDataSetProvider.LocateEx = ILocateEx;

    procedure SetFilterText(const AValue: string); override;
    procedure SetFiltered(AValue: Boolean); override;

    procedure IRefreshDetail;
    procedure IBaseDataSetProvider.RefreshDetail = IRefreshDetail;

    function IUpdatesPending: Boolean;
    function IBaseDataSetProvider.UpdatesPending = IUpdatesPending;

    procedure IApplyUpdates;
    procedure IBaseDataSetProvider.ApplyUpdates = IApplyUpdates;

    procedure ICommitUpdates;
    procedure IBaseDataSetProvider.CommitUpdates = ICommitUpdates;

    procedure ICancelUpdates;
    procedure IBaseDataSetProvider.CancelUpdates = ICancelUpdates;

    procedure IRestoreUpdates;
    procedure IBaseDataSetProvider.RestoreUpdates = IRestoreUpdates;

    procedure IRevertRecord;
    procedure IBaseDataSetProvider.RevertRecord = IRevertRecord;
  protected
    procedure SetActive(AValue: Boolean); override;
    procedure SetParentComponent(AParent: TComponent); override;

    procedure BeginLoad; virtual;
    procedure EndLoad; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property KeyFieldNames: String read FKeyFieldNames write FKeyFieldNames;
  end;

  TInternalMemoryDataSet = class(TUserMemoryDataSet)
  private
    FFocused: Boolean;
  protected
    procedure DoAfterCancel; override;
    procedure DoAfterClose; override;
    procedure DoAfterDelete; override;
    procedure DoAfterEdit; override;
    procedure DoAfterInsert; override;
    procedure DoAfterOpen; override;
    procedure DoAfterPost; override;
    procedure DoAfterRefresh; override;
    procedure DoAfterScroll; override;
    procedure DoBeforeCancel; override;
    procedure DoBeforeClose; override;
    procedure DoBeforeDelete; override;
    procedure DoBeforeEdit; override;
    procedure DoBeforeInsert; override;
    procedure DoBeforeOpen; override;
    procedure DoBeforePost; override;
    procedure DoBeforeRefresh; override;
    procedure DoBeforeScroll; override;
    procedure DoOnCalcFields; override;
    procedure DoOnNewRecord; override;
  protected
    procedure BeginLoad; override;
    procedure EndLoad; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Provider: TDataSet; inline;

    property Focused: Boolean read FFocused write FFocused;
  end;

  TCustomInternalQuery = class(TCustomUniDataSet, IBaseDataSetProvider, IBaseSQLDataSetProvider)
  private
    function SupportedMethods: TBaseDataSetSupportedActions;

    function GetMasterSource: TDataSource;
    procedure SetMasterSource(AValue: TDataSource);

    function GetMasterFields: String;
    procedure SetMasterFields(const AValue: String);

    function GetDetailFields: String;
    procedure SetDetailFields(const AValue: String);

    function GetKeyFieldNames: String;
    procedure SetKeyFieldNames(const AValue: String);

    function  IGetReadOnly: Boolean;
    function  IBaseDataSetProvider.GetReadOnly = IGetReadOnly;

    procedure ISetReadOnly(const AValue: Boolean);
    procedure IBaseDataSetProvider.SetReadOnly = ISetReadOnly;

    procedure IRefreshDetail;
    procedure IBaseDataSetProvider.RefreshDetail = IRefreshDetail;

    function IUpdatesPending: Boolean;
    function IBaseDataSetProvider.UpdatesPending = IUpdatesPending;

    procedure IApplyUpdates;
    procedure IBaseDataSetProvider.ApplyUpdates = IApplyUpdates;

    procedure ICommitUpdates;
    procedure IBaseDataSetProvider.CommitUpdates = ICommitUpdates;

    procedure ICancelUpdates;
    procedure IBaseDataSetProvider.CancelUpdates = ICancelUpdates;

    procedure IRestoreUpdates;
    procedure IBaseDataSetProvider.RestoreUpdates = IRestoreUpdates;

    procedure IRevertRecord;
    procedure IBaseDataSetProvider.RevertRecord = IRevertRecord;
  private
    function  GetConnection: TCustomConnection;
    procedure SetConnection(AValue: TCustomConnection);

    function  IGetRefreshOptions: TBaseRefreshOptions;
    function  IBaseSQLDataSetProvider.GetRefreshOptions = IGetRefreshOptions;

    procedure ISetRefreshOptions(AValue: TBaseRefreshOptions);
    procedure IBaseSQLDataSetProvider.SetRefreshOptions = ISetRefreshOptions;

//    function IHasNextResultSet: Boolean;
//    function IBaseSQLDataSetProvider.!asNextResultSet = I!asNextResultSet;

    procedure IClearNextResultSets;
    procedure IBaseSQLDataSetProvider.ClearNextResultSets = IClearNextResultSets;

    procedure IBreakExec;
    procedure IBaseSQLDataSetProvider.BreakExec = IBreakExec;

    procedure IOpenNext;
    procedure IBaseSQLDataSetProvider.OpenNext = IOpenNext;

    procedure IRefreshRecord; virtual;
    procedure IBaseSQLDataSetProvider.RefreshRecord = IRefreshRecord;

    procedure IUnPrepare;
    procedure IBaseSQLDataSetProvider.UnPrepare = IUnPrepare;

    function GetAutoGenerateUpdateScripts: Boolean;
    procedure SetAutoGenerateUpdateScripts(const AValue: Boolean);

    function GetCachedUpdates: Boolean;
    procedure SetCachedUpdates(const AValue: Boolean);

    function GetUpdateStatusFilter: TUpdateStatusSet;
    procedure SetUpdateStatusFilter(AValue: TUpdateStatusSet);

    function GetSQLDelete: String;
    procedure SetSQLDelete(const AValue: String);

    function GetSQLInsert: String;
    procedure SetSQLInsert(const AValue: String);

    function GetSQLRefresh: String;
    procedure SetSQLRefresh(const AValue: String);

    function GetSQLUpdate: String;
    procedure SetSQLUpdate(const AValue: String);
  protected // override !!!
{$IFNDEF PACKAGE}
    procedure SetActive(AValue: Boolean); override;
{$ENDIF}

    procedure AssignTo(Dest: TPersistent); override;
    function PSGetCommandText: string; override;
    function PSGetKeyFields: String; override;
    procedure DoAfterOpen; override;
    procedure SetFilterText(const AValue: string); override;
    procedure SetFiltered(AValue: Boolean); override;

    function GetIndexFieldNames: String;
    procedure SetIndexFieldNames(const AValue: String); override;
  public
    constructor Create(AOwner: TComponent); override;

    procedure CheckConnection; override;
    function DataSetProvider: TDataSet;

    function ILocateEx(const KeyFields: string; const KeyValues: Variant; Options: TBaseLocateOptions): Boolean;
    function IBaseDataSetProvider.LocateEx = ILocateEx;
{$IFNDEF PACKAGE}
    procedure Refresh; override;
{$ENDIF}

//  published
//    property Connection: TCustomConnection read GetConnection write SetConnection;
//    property UpdateStatusFilter: TUpdateStatusSet read GetUpdateStatusFilter write SetUpdateStatusFilter;
  end;

  TInternalQuery = class(TCustomInternalQuery)
  private
    FFocused: Boolean;
  private
    procedure IRefreshRecord; override;
  protected
    procedure DoAfterCancel; override;
    procedure DoAfterClose; override;
    procedure DoAfterDelete; override;
    procedure DoAfterEdit; override;
    procedure DoAfterInsert; override;
    procedure DoAfterOpen; override;
    procedure DoAfterPost; override;
    procedure DoAfterRefresh; override;
    procedure DoAfterScroll; override;
    procedure DoBeforeCancel; override;
    procedure DoBeforeClose; override;
    procedure DoBeforeDelete; override;
    procedure DoBeforeEdit; override;
    procedure DoBeforeInsert; override;
    procedure DoBeforeOpen; override;
    procedure DoBeforePost; override;
    procedure DoBeforeRefresh; override;
    procedure DoBeforeScroll; override;
    procedure DoOnCalcFields; override;
    procedure DoOnNewRecord; override;
  public
{$IFDEF DEBUG}
    constructor Create(AOwner: TComponent); override;
{$ENDIF}

    function Provider: TDataSet; inline;
    property Focused: Boolean read FFocused write FFocused;
  published
    property Params;
  end;

  TInternalPublicQuery = class(TCustomInternalQuery)
  protected
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: string); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TInternalPublicDataSet = class
    FMemoryStream : TMemoryStream;
    FPublicQuery  : TInternalPublicQuery;
  public
    destructor Destroy; override;

    procedure Open(ASQLConnectionProvider: TComponent; ACommandText: String; AParams: TParams; ALiteMode: Boolean);
    function Active(ALiteMode: Boolean): Boolean;
    procedure Save(ADataSet: TUserMemoryDataSet);

    property PublicQuery: TInternalPublicQuery read FPublicQuery;
  end;

{$IFDEF FASTSCRIPT_RTTI}
  TFunctions_DHI = class(TfsRTTIModule)
  private
    function  Get_TCustomDAConnection_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function  Call_TCustomDAConnection_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Call_TUniDataSet_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

  TfsUniConnectionInfoMessageEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject; E: EMSError);
    function GetMethod: Pointer; override;
  end;

  TfsDAConnectionErrorEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject; E: EDAError; var Fail: boolean);
    function GetMethod: Pointer; override;
  end;
{$ENDIF}

var
  FDisableSQLCursorCount: Integer = 0;
  FDisabledCursor: Integer;

  procedure DisableSQLCursor(AGlobalCursor: Integer);
  procedure EnableSQLCursor;

implementation

uses
  SBaseConstants, SBaseDataModule, DacVcl, CRFunctions
  ;

type
  TCustomUniDataSetCrack = class(TCustomUniDataSet);
  TCRConnectionCrack = class(TCRConnection);
  TCustomSQLDataSetProviderCrack = class(TSQLDataSetProvider);
  TCustomInternalQueryCrack = class(TCustomInternalQuery);
  TSQLDataSetProviderCrack = class(TSQLDataSetProvider);

procedure DisableSQLCursor(AGlobalCursor: Integer);
begin
  if CRFunctions.IsMainThread then begin
    Inc(FDisableSQLCursorCount);
    if FDisableSQLCursorCount = 1 then begin
      FDisabledCursor := GetScreenCursor;
      DBAccess.SetCursor(AGlobalCursor);
    end;
    DBAccess.ChangeCursor := False;
  end;
end;

procedure EnableSQLCursor;
begin
  if CRFunctions.IsMainThread and (FDisableSQLCursorCount > 0) then begin
    Dec(FDisableSQLCursorCount);
    if FDisableSQLCursorCount = 0 then begin
      DBAccess.ChangeCursor := True;
      DBAccess.SetCursor(FDisabledCursor);
    end;
  end;
end;

{ TInternalConnection }

procedure TInternalConnection.AssignTo(Dest: TPersistent);
begin
  if Dest is TInternalConnection then
    TInternalConnection(Dest).ProviderName := ProviderName;

  inherited;
end;

procedure TInternalConnection.ConnectionInfoMessage(E: Exception);
begin
  if Assigned(FOnInfoMessage) then
    FOnInfoMessage(Self, E.Message);
end;

constructor TInternalConnection.Create(AOwner: TComponent);
begin
  inherited;

//  Options.LocalFailover := True;
//  OnConnectionLost := EventOnConnectionLost;
end;

procedure TInternalConnection.DoDefaultError(E: Exception; var Fail: boolean);
begin
  TCRConnectionCrack(TDBAccessUtils.GetIConnection(Self)).DoError(E, Fail);
end;

procedure TInternalConnection.DoSetOnInfoMessage;
var
  LIConnection: TCRConnection;
begin
  LIConnection := TDBAccessUtils.GetIConnection(Self);
  if LIConnection <> nil then
    TCRConnectionCrack(LIConnection).SetOnInfoMessage(ConnectionInfoMessage, Assigned(FOnInfoMessage));
end;

//procedure TInternalConnection.EventOnConnectionLost(Sender: TObject; Component: TComponent; ConnLostCause: TConnLostCause; var RetryMode: TRetryMode);
//begin
//  if ConnLostCause in [clExecute] then
//    RetryMode := rmReconnect;

//  if Assigned(AfterDisconnect) then
//    AfterDisconnect(Self)
//end;

procedure TInternalConnection.SetIConnection(Value: TCRConnection);
begin
  inherited;
  DoSetOnInfoMessage;
end;

procedure TInternalConnection.SetOnInfoMessage(AValue: TInternalConnectionOnInfoMessage);
begin
  FOnInfoMessage := AValue;
  DoSetOnInfoMessage;
end;

//function TInternalConnection.GetInTransaction: boolean;
//begin
//  Result := Connected //and FInterpreter.
//end;

function TInternalConnection.GetProviderName: string;
begin
  Result := (inherited ProviderName);
end;

class function TInternalConnection.IsBreakExecError(E: Exception): Boolean;
begin
  if (E is EOLEDBError) and (EOLEDBError(E).ErrorCode = DB_E_CANCELED) then
    Result := True
  else
    Result := False;
end;

class function TInternalConnection.IsConnectionError(E: Exception): Boolean;
begin
  Result := (E is EOLEDBError);
end;

procedure TInternalConnection.RollbackTran(AIfPresent: Boolean);
begin
  if AIfPresent then begin
    if Connected then
      ExecSQL(FInterpreter.GenerateRollBackTranIfPresentScript);
  end else
    ExecSQL(FInterpreter.GenerateRollBackTranScript);
end;

procedure TInternalConnection.SetProviderName(const Value: string);
begin
  inherited ProviderName := Value;
  FInterpreter := TInternalProviderInterpreter.GetInterpreter(inherited ProviderName)
end;

{ TUserMemoryDataSet }

constructor TUserMemoryDataSet.Create(AOwner: TComponent);
begin
  inherited;
  Options := Options - [voPersistentData];
  if Assigned(AOwner) then
    SetSubComponent(True);
end;

procedure TUserMemoryDataSet.BeginLoad;
begin
//
end;

procedure TUserMemoryDataSet.EndLoad;
begin
//
end;

function TUserMemoryDataSet.GetDetailFields: String;
begin
  Result := DetailFields
end;

function TUserMemoryDataSet.GetIndexFieldNames: String;
begin
  Result := IndexFieldNames
end;

function TUserMemoryDataSet.GetKeyFieldNames: String;
begin
  Result := KeyFieldNames
end;

function TUserMemoryDataSet.GetMasterFields: String;
begin
  Result := MasterFields
end;

function TUserMemoryDataSet.GetMasterSource: TDataSource;
begin
  Result := MasterSource
end;

function TUserMemoryDataSet.GetReadOnly: Boolean;
begin
  Result := False;
end;

function TUserMemoryDataSet.GetUpdateStatusFilter: TUpdateStatusSet;
var
  LUpdateRecordTypes: TUpdateRecordTypes;
begin
  if CachedUpdates then begin
    Result := [];
    LUpdateRecordTypes := UpdateRecordTypes;
    if rtUnmodified in LUpdateRecordTypes then
      Include(Result, usUnmodified);
    if rtModified in LUpdateRecordTypes then
      Include(Result, usModified);
    if rtInserted in LUpdateRecordTypes then
      Include(Result, usInserted);
    if rtDeleted in LUpdateRecordTypes then
      Include(Result, usDeleted);
  end else
    Result := Default_UpdateStatusFilter;
end;

procedure TUserMemoryDataSet.IApplyUpdates;
begin
  ApplyUpdates
end;

procedure TUserMemoryDataSet.ICancelUpdates;
begin
  CancelUpdates
end;

procedure TUserMemoryDataSet.ICommitUpdates;
begin
  CommitUpdates
end;

function TUserMemoryDataSet.IGetCachedUpdates: Boolean;
begin
  Result := CachedUpdates
end;

procedure TUserMemoryDataSet.ISetCachedUpdates(const Value: Boolean);
begin
  CachedUpdates := Value;
end;

function TUserMemoryDataSet.IUpdatesPending: Boolean;
begin
  Result := UpdatesPending
end;

function TUserMemoryDataSet.ILocateEx(const KeyFields: string; const KeyValues: Variant; Options: TBaseLocateOptions): Boolean;
begin
  Result := LocateEx(KeyFields, KeyValues, TLocateExOptions(Options));
end;

procedure TUserMemoryDataSet.IRefreshDetail;
begin
  MasterRecordChanged
end;

procedure TUserMemoryDataSet.IRestoreUpdates;
begin
  RestoreUpdates
end;

procedure TUserMemoryDataSet.IRevertRecord;
begin
  RevertRecord
end;

procedure TUserMemoryDataSet.SetActive(AValue: Boolean);
begin
  if Active = AValue then Exit;

  inherited;

//  if not Active then
//    Clear
//  else
{$IFNDEF PACKAGE}
  if Active and ControlsDisabled then
    Include(FEnableEvents, deDataSetChange);
{$ENDIF}
end;

procedure TUserMemoryDataSet.SetDetailFields(const AValue: String);
begin
  DetailFields := AValue
end;

procedure TUserMemoryDataSet.SetFiltered(AValue: Boolean);
begin
  if AValue <> Filtered then
    inherited;
end;

procedure TUserMemoryDataSet.SetFilterText(const AValue: string);
begin
  if AValue <> Filter then
    inherited;
end;

procedure TUserMemoryDataSet.SetIndexFieldNames(const AValue: String);
begin
  inherited;
  //IndexFieldNames := AValue
end;

procedure TUserMemoryDataSet.SetKeyFieldNames(const AValue: String);
begin
  KeyFieldNames := AValue
end;

procedure TUserMemoryDataSet.SetMasterFields(const AValue: String);
begin
  MasterFields := AValue
end;

procedure TUserMemoryDataSet.SetMasterSource(AValue: TDataSource);
begin
  MasterSource := AValue
end;

procedure TUserMemoryDataSet.SetParentComponent(AParent: TComponent);
begin
  if AParent is TDataSource then
    TDataSource(AParent).DataSet := Self;
end;

procedure TUserMemoryDataSet.SetReadOnly(const Value: Boolean);
begin
  Assert(not Value);
end;

procedure TUserMemoryDataSet.SetUpdateStatusFilter(AValue: TUpdateStatusSet);
var
  LUpdateRecordTypes: TUpdateRecordTypes;
begin
  if CachedUpdates then begin
    LUpdateRecordTypes := [];
    if usUnmodified in AValue then
      Include(LUpdateRecordTypes, rtUnmodified);
    if usModified in AValue then
      Include(LUpdateRecordTypes, rtModified);
    if usInserted in AValue then
      Include(LUpdateRecordTypes, rtInserted);
    if usDeleted in AValue then
      Include(LUpdateRecordTypes, rtDeleted);

    if UpdateRecordTypes <> LUpdateRecordTypes then
      UpdateRecordTypes := LUpdateRecordTypes;
  end else
    Assert((AValue = []) or (AValue = Default_UpdateStatusFilter))
end;

function TUserMemoryDataSet.SupportedMethods: TBaseDataSetSupportedActions;
begin
  Result := []
end;

{ TInternalMemoryDataSet }

procedure TInternalMemoryDataSet.BeginLoad;
begin
  FFocused := True;
  inherited;
end;

constructor TInternalMemoryDataSet.Create(AOwner: TComponent);
begin
  Assert(AOwner is TCustomSQLDataSetProvider);
  inherited;
end;

procedure TInternalMemoryDataSet.DoAfterCancel;
begin
  if FFocused and Assigned(Provider.AfterCancel) then
    Provider.AfterCancel(Provider);
end;

procedure TInternalMemoryDataSet.DoAfterClose;
begin
  if FFocused and Assigned(Provider.AfterClose) then
    Provider.AfterClose(Provider);
end;

procedure TInternalMemoryDataSet.DoAfterDelete;
begin
  if FFocused and Assigned(Provider.AfterDelete) then
    Provider.AfterDelete(Provider);
end;

procedure TInternalMemoryDataSet.DoAfterEdit;
begin
  if FFocused and Assigned(Provider.AfterEdit) then
    Provider.AfterEdit(Provider);
end;

procedure TInternalMemoryDataSet.DoAfterInsert;
begin
  if FFocused and Assigned(Provider.AfterInsert) then
    Provider.AfterInsert(Provider);
end;

procedure TInternalMemoryDataSet.DoAfterOpen;
begin
  if FFocused and Assigned(Provider.AfterOpen) then
    Provider.AfterOpen(Provider);
  if not IsEmpty then DoAfterScroll;
end;

procedure TInternalMemoryDataSet.DoAfterPost;
begin
  if FFocused then begin
//    TCustomSQLDataSetProviderCrack(Owner).DataSetAfterPost;
//    if (broAfterPost in Provider.RefreshOptions) then
//      RefreshRecord;
    if Assigned(Provider.AfterPost) then
      Provider.AfterPost(Provider);
  end;
end;

procedure TInternalMemoryDataSet.DoAfterRefresh;
begin
  if FFocused and Assigned(Provider.AfterRefresh) then
    Provider.AfterRefresh(Provider);
end;

procedure TInternalMemoryDataSet.DoAfterScroll;
begin
  TCustomSQLDataSetProviderCrack(Owner).DataSetAfterScroll;
  if FFocused and Assigned(Provider.AfterScroll) then
    Provider.AfterScroll(Provider);
end;

procedure TInternalMemoryDataSet.DoBeforeCancel;
begin
  if FFocused and Assigned(Provider.BeforeCancel) then
    Provider.BeforeCancel(Provider);
end;

procedure TInternalMemoryDataSet.DoBeforeClose;
begin
  if FFocused and Assigned(Provider.BeforeClose) then
    Provider.BeforeClose(Provider);
end;

procedure TInternalMemoryDataSet.DoBeforeDelete;
begin
  if FFocused and Assigned(Provider.BeforeDelete) then
    Provider.BeforeDelete(Provider);
end;

procedure TInternalMemoryDataSet.DoBeforeEdit;
begin
  if FFocused then begin
//    if (broBeforeEdit in TSQLDataSetProvider(Owner).RefreshOptions) then
//      RefreshRecord;
    if Assigned(Provider.BeforeEdit) then
      Provider.BeforeEdit(Provider);
  end;
end;

procedure TInternalMemoryDataSet.DoBeforeInsert;
begin
  if FFocused and Assigned(Provider.BeforeInsert) then
    Provider.BeforeInsert(Provider);
end;

procedure TInternalMemoryDataSet.DoBeforeOpen;
begin
  if FFocused and Assigned(Provider.BeforeOpen) then
    Provider.BeforeOpen(Provider);
end;

procedure TInternalMemoryDataSet.DoBeforePost;
begin
  TCustomSQLDataSetProviderCrack(Owner).DataSetBeforePost;
  if FFocused and Assigned(Provider.BeforePost) then
    Provider.BeforePost(Provider);
end;

procedure TInternalMemoryDataSet.DoBeforeRefresh;
begin
  if FFocused and Assigned(Provider.BeforeRefresh) then
    Provider.BeforeRefresh(Provider);
end;

procedure TInternalMemoryDataSet.DoBeforeScroll;
begin
  if FFocused and Assigned(Provider.BeforeScroll) then
    Provider.BeforeScroll(Provider);
end;

procedure TInternalMemoryDataSet.DoOnCalcFields;
begin
  if FFocused and Assigned(Provider.OnCalcFields) then
    Provider.OnCalcFields(Provider);
end;

procedure TInternalMemoryDataSet.DoOnNewRecord;
begin
  if FFocused and Assigned(Provider.OnNewRecord) then
    Provider.OnNewRecord(Provider);
end;

procedure TInternalMemoryDataSet.EndLoad;
begin
  FFocused := False;
  inherited;
end;

function TInternalMemoryDataSet.Provider: TDataSet;
begin
  Result := TDataSet(Owner);
end;

{ TCustomInternalQuery }

function TCustomInternalQuery.GetDetailFields: String;
begin
  Result := DetailFields
end;

function TCustomInternalQuery.GetIndexFieldNames: String;
begin
  Result := IndexFieldNames
end;

function TCustomInternalQuery.GetKeyFieldNames: String;
begin
  Result := KeyFields
end;

function TCustomInternalQuery.GetMasterFields: String;
begin
  Result := MasterFields
end;

function TCustomInternalQuery.GetMasterSource: TDataSource;
begin
  Result := MasterSource
end;

function TCustomInternalQuery.IGetRefreshOptions: TBaseRefreshOptions;
begin
  Result := TBaseRefreshOptions(RefreshOptions);
end;

procedure TCustomInternalQuery.IOpenNext;
begin
  OpenNext;
end;

procedure TCustomInternalQuery.ISetRefreshOptions(AValue: TBaseRefreshOptions);
begin
  RefreshOptions := DBAccess.TRefreshOptions(AValue)
end;

function TCustomInternalQuery.IGetReadOnly: Boolean;
begin
  Result := ReadOnly
end;

procedure TCustomInternalQuery.ISetReadOnly(const AValue: Boolean);
begin
  ReadOnly := AValue
end;

function TCustomInternalQuery.GetSQLDelete: String;
begin
  Result := SQLDelete.Text
end;

function TCustomInternalQuery.GetSQLInsert: String;
begin
  Result := SQLInsert.Text
end;

function TCustomInternalQuery.GetSQLRefresh: String;
begin
  Result := SQLRefresh.Text
end;

function TCustomInternalQuery.GetSQLUpdate: String;
begin
  Result := SQLUpdate.Text
end;

procedure TCustomInternalQuery.SetDetailFields(const AValue: String);
begin
  DetailFields := AValue
end;

procedure TCustomInternalQuery.SetFiltered(AValue: Boolean);
begin
  if AValue <> Filtered then
    inherited;
end;

procedure TCustomInternalQuery.SetFilterText(const AValue: string);
begin
  if AValue <> Filter then
    inherited;
end;

procedure TCustomInternalQuery.SetIndexFieldNames(const AValue: String);
begin
//  IndexFieldNames := AValue
  inherited;
end;

procedure TCustomInternalQuery.SetKeyFieldNames(const AValue: String);
begin
  KeyFields := AValue
end;

procedure TCustomInternalQuery.SetMasterFields(const AValue: String);
begin
  MasterFields := AValue
end;

procedure TCustomInternalQuery.SetMasterSource(AValue: TDataSource);
begin
  MasterSource := AValue
end;

procedure TCustomInternalQuery.IApplyUpdates;
begin
  ApplyUpdates
end;

procedure TCustomInternalQuery.IBreakExec;
begin
  BreakExec
end;

procedure TCustomInternalQuery.ICancelUpdates;
begin
  CancelUpdates
end;

procedure TCustomInternalQuery.ICommitUpdates;
begin
  CommitUpdates
end;

procedure TCustomInternalQuery.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TCustomInternalQuery then begin
    TCustomInternalQuery(Dest).ReadOnly := ReadOnly;
  end;
end;

procedure TCustomInternalQuery.CheckConnection;
begin
  inherited;
end;

constructor TCustomInternalQuery.Create(AOwner: TComponent);
begin
  inherited;
{$IFDEF DEBUG}
  SetSubComponent(True);
{$ENDIF}

  CachedUpdates := False;
  LocalUpdate   := True;
  EnableUniSQL  := False;

  Options.ReturnParams      := True;
  Options.RemoveOnRefresh   := True;
  Options.StrictUpdate      := False;
  Options.LocalMasterDetail := True;
  Options.SetFieldsReadOnly := False;
end;

function TCustomInternalQuery.DataSetProvider: TDataSet;
begin
  if Owner is TSQLDataSetProvider then
    Result := TSQLDataSetProvider(Owner)
  else
    Result := nil
end;

procedure TCustomInternalQuery.DoAfterOpen;
var
  LDataSetProvider: TSQLDataSetProviderCrack;
begin
  LDataSetProvider := TSQLDataSetProviderCrack(DataSetProvider);
  if LDataSetProvider <> nil then
    LDataSetProvider.SyncronizeOutputParameters(Self);

  inherited;
end;

function TCustomInternalQuery.GetAutoGenerateUpdateScripts: Boolean;
begin
  Result := not LocalUpdate
end;

function TCustomInternalQuery.GetCachedUpdates: Boolean;
begin
  Result := CachedUpdates
end;

procedure TCustomInternalQuery.SetSQLDelete(const AValue: String);
begin
  SQLDelete.Text := AValue
end;

procedure TCustomInternalQuery.SetSQLInsert(const AValue: String);
begin
  SQLInsert.Text := AValue
end;

procedure TCustomInternalQuery.SetSQLRefresh(const AValue: String);
begin
  SQLRefresh.Text := AValue
end;

procedure TCustomInternalQuery.SetSQLUpdate(const AValue: String);
begin
  SQLUpdate.Text := AValue
end;

function TCustomInternalQuery.GetConnection: TCustomConnection;
begin
  Result := inherited Connection
end;

{$IFNDEF PACKAGE}
procedure TCustomInternalQuery.SetActive(AValue: Boolean);
begin
  inherited;

  if Active and ControlsDisabled then
    Include(FEnableEvents, deDataSetChange);
end;
{$ENDIF}

procedure TCustomInternalQuery.SetAutoGenerateUpdateScripts(const AValue: Boolean);
begin
  LocalUpdate := not AValue
end;

procedure TCustomInternalQuery.SetCachedUpdates(const AValue: Boolean);
begin
  CachedUpdates := AValue
end;

procedure TCustomInternalQuery.SetConnection(AValue: TCustomConnection);
begin
  Assert((AValue = nil) or (AValue is TInternalConnection));
  inherited SetConnection(TInternalConnection(AValue));
end;

function TCustomInternalQuery.GetUpdateStatusFilter: TUpdateStatusSet;
var
  LUpdateRecordTypes: TUpdateRecordTypes;
begin
  if CachedUpdates then begin
    Result := [];
    LUpdateRecordTypes := UpdateRecordTypes;
    if rtUnmodified in LUpdateRecordTypes then
      Include(Result, usUnmodified);
    if rtModified in LUpdateRecordTypes then
      Include(Result, usModified);
    if rtInserted in LUpdateRecordTypes then
      Include(Result, usInserted);
    if rtDeleted in LUpdateRecordTypes then
      Include(Result, usDeleted);
  end else
    Result := Default_UpdateStatusFilter;
end;

//function TCustomInternalQuery.IHasNextResultSet: Boolean;
//begin
//  Result := !asNextResultSet
//end;

procedure TCustomInternalQuery.IClearNextResultSets;
begin
  ClearNextResultSets
end;

function TCustomInternalQuery.ILocateEx(const KeyFields: string; const KeyValues: Variant; Options: TBaseLocateOptions): Boolean;
begin
  Result := LocateEx(KeyFields, KeyValues, TLocateExOptions(Options));
end;

procedure TCustomInternalQuery.IRefreshDetail;
begin
  MasterRecordChanged
end;

procedure TCustomInternalQuery.IRefreshRecord;
begin
  RefreshRecord;
end;

procedure TCustomInternalQuery.IRestoreUpdates;
begin
  RestoreUpdates
end;

procedure TCustomInternalQuery.IRevertRecord;
begin
  RevertRecord
end;

procedure TCustomInternalQuery.SetUpdateStatusFilter(AValue: TUpdateStatusSet);
var
  LUpdateRecordTypes: TUpdateRecordTypes;
begin
  if CachedUpdates then begin
    LUpdateRecordTypes := [];
    if usUnmodified in AValue then
      Include(LUpdateRecordTypes, rtUnmodified);
    if usModified in AValue then
      Include(LUpdateRecordTypes, rtModified);
    if usInserted in AValue then
      Include(LUpdateRecordTypes, rtInserted);
    if usDeleted in AValue then
      Include(LUpdateRecordTypes, rtDeleted);

    if UpdateRecordTypes <> LUpdateRecordTypes then
      UpdateRecordTypes := LUpdateRecordTypes;
  end else
    Assert((AValue = []) or (AValue = Default_UpdateStatusFilter))
end;

function TCustomInternalQuery.SupportedMethods: TBaseDataSetSupportedActions;
begin
  Result := [bdssaSQL, bdssaKeyFields, bdssaRefreshAfterInsert, bdssaRefreshAfterUpdate, bdssaRefreshBeforeEdit];
end;

procedure TCustomInternalQuery.IUnPrepare;
begin
  UnPrepare
end;

function TCustomInternalQuery.IUpdatesPending: Boolean;
begin
  Result := UpdatesPending
end;

function TCustomInternalQuery.PSGetCommandText: string;
begin
  Result := SQL.Text
end;

function TCustomInternalQuery.PSGetKeyFields: String;
begin
  Result := KeyFields
end;

{$IFNDEF PACKAGE}
procedure TCustomInternalQuery.Refresh;
begin
  inherited;
  if ControlsDisabled then
    Include(FEnableEvents, deDataSetChange);
end;
{$ENDIF}

{ TInternalQuery }

{$IFDEF DEBUG}
constructor TInternalQuery.Create(AOwner: TComponent);
begin
  Assert(AOwner is TCustomSQLDataSetProvider);
  inherited;
//  ParamCheck    := False; //TODO: Подправить передачу параметров
end;
{$ENDIF}

procedure TInternalQuery.DoAfterCancel;
begin
  if FFocused and Assigned(Provider.AfterCancel) then
    Provider.AfterCancel(Provider);
end;

procedure TInternalQuery.DoAfterClose;
begin
  if FFocused and Assigned(Provider.AfterClose) then
    Provider.AfterClose(Provider);
end;

procedure TInternalQuery.DoAfterDelete;
begin
  if FFocused and Assigned(Provider.AfterDelete) then
    Provider.AfterDelete(Provider);
end;

procedure TInternalQuery.DoAfterEdit;
begin
  if FFocused and Assigned(Provider.AfterEdit) then
    Provider.AfterEdit(Provider);
end;

procedure TInternalQuery.DoAfterInsert;
begin
  if FFocused and Assigned(Provider.AfterInsert) then
    Provider.AfterInsert(Provider);
end;

procedure TInternalQuery.DoAfterOpen;
begin
  inherited;
  if FFocused and Assigned(Provider.AfterOpen) then
    Provider.AfterOpen(Provider);
  if not IsEmpty then DoAfterScroll;
end;

procedure TInternalQuery.DoAfterPost;
begin
  if FFocused then begin
//    TCustomSQLDataSetProviderCrack(Owner).DataSetAfterPost;
//    if (broAfterPost in Provider.RefreshOptions) then
//      RefreshRecord;
    if Assigned(Provider.AfterPost) then
      Provider.AfterPost(Provider);
  end;
end;

procedure TInternalQuery.DoAfterRefresh;
begin
  if FFocused and Assigned(Provider.AfterRefresh) then
    Provider.AfterRefresh(Provider);
end;

procedure TInternalQuery.DoAfterScroll;
begin
  TSQLDataSetProviderCrack(Provider).DataSetAfterScroll;
  if FFocused and Assigned(Provider.AfterScroll) then
    Provider.AfterScroll(Provider);
end;

procedure TInternalQuery.DoBeforeCancel;
begin
  if FFocused and Assigned(Provider.BeforeCancel) then
    Provider.BeforeCancel(Provider);
end;

procedure TInternalQuery.DoBeforeClose;
begin
  if FFocused and Assigned(Provider.BeforeClose) then
    Provider.BeforeClose(Provider);
end;

procedure TInternalQuery.DoBeforeDelete;
begin
  if FFocused and Assigned(Provider.BeforeDelete) then
    Provider.BeforeDelete(Provider);
end;

procedure TInternalQuery.DoBeforeEdit;
begin
  if FFocused then begin
//    if (broBeforeEdit in Provider.RefreshOptions) then
//      RefreshRecord;
    if Assigned(Provider.BeforeEdit) then
      Provider.BeforeEdit(Provider);
  end;
end;

procedure TInternalQuery.DoBeforeInsert;
begin
  if FFocused and Assigned(Provider.BeforeInsert) then
    Provider.BeforeInsert(Provider);
end;

procedure TInternalQuery.DoBeforeOpen;
begin
  if FFocused and Assigned(Provider.BeforeOpen) then
    Provider.BeforeOpen(Provider);
end;

procedure TInternalQuery.DoBeforePost;
begin
  TSQLDataSetProviderCrack(Provider).DataSetBeforePost;
  if FFocused and Assigned(Provider.BeforePost) then
    Provider.BeforePost(Provider);
end;

procedure TInternalQuery.DoBeforeRefresh;
begin
  if FFocused and Assigned(Provider.BeforeRefresh) then
    Provider.BeforeRefresh(Provider);
end;

procedure TInternalQuery.DoBeforeScroll;
begin
  if FFocused and Assigned(Provider.BeforeScroll) then
    Provider.BeforeScroll(Provider);
end;

procedure TInternalQuery.DoOnCalcFields;
begin
  if FFocused and Assigned(Provider.OnCalcFields) then
    Provider.OnCalcFields(Provider);
end;

procedure TInternalQuery.DoOnNewRecord;
begin
  TSQLDataSetProviderCrack(Provider).DataSetAfterScroll;
  if FFocused and Assigned(Provider.OnNewRecord) then
    Provider.OnNewRecord(Provider);
end;

procedure TInternalQuery.IRefreshRecord;
begin
  inherited;
  if Assigned(Provider.AfterRefresh) then
    Provider.AfterRefresh(Provider);
end;

function TInternalQuery.Provider: TDataSet;
begin
  Result := TDataSet(Owner);
end;

{ TInternalPublicDataSet }

//constructor TInternalPublicDataSet.Create(ASQLConnectionProvider: TComponent; ACommandText: String; AParams: TParams);
//begin
//  Assert(Assigned(ASQLConnectionProvider) and (ASQLConnectionProvider is TSQLConnectionProvider));
//  FSQLConnectionProvider := ASQLConnectionProvider;
//  FCommandText := ACommandText;
//  FParams := TParams.Create(nil);
//  FParams.Assign(AParams);
//  inherited Create;
//end;

destructor TInternalPublicDataSet.Destroy;
begin
  FreeAndNil(FMemoryStream);
  FreeAndNil(FPublicQuery);
  inherited;
end;

function TInternalPublicDataSet.Active(ALiteMode: Boolean): Boolean;
begin
  Result := Assigned(FPublicQuery) or ((not ALiteMode) and Assigned(FMemoryStream) and (FMemoryStream.Size > 0));
end;

procedure TInternalPublicDataSet.Save(ADataSet: TUserMemoryDataSet);
begin
  ADataSet.BeginLoad;
  try
    if Assigned(FMemoryStream) then begin
{$IFDEF DEBUG}
      Assert(FMemoryStream.Size > 0);
{$ENDIF}
      ADataSet.LoadFromStream(FMemoryStream, True);
    end else begin
{$IFDEF DEBUG}
      Assert(Assigned(FPublicQuery));
{$ENDIF}
      ADataSet.Load(FPublicQuery);
    end;
  finally
    ADataSet.EndLoad;
  end;
end;

procedure TInternalPublicDataSet.Open(ASQLConnectionProvider: TComponent; ACommandText: String; AParams: TParams; ALiteMode: Boolean);
{$IFNDEF PACKAGE}
var
  LCustomInternalQuery: TCustomInternalQuery;
  LDataSet: TVirtualTable;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  if ALiteMode or Assigned(FPublicQuery) then begin
    if not Assigned(FPublicQuery) then begin
      FPublicQuery := TInternalPublicQuery.Create(nil);
      FPublicQuery.Connection := TSQLConnectionProvider(ASQLConnectionProvider).Connection;
      FPublicQuery.ReadOnly := True;
      FPublicQuery.SQL.Text := ACommandText;
      FPublicQuery.Params.Assign(AParams);
      FreeAndNil(FMemoryStream);
    end;
    FPublicQuery.Open;
  end else begin
    LCustomInternalQuery := TSQLConnectionProvider(ASQLConnectionProvider).OpenSQLQuery(ACommandText, AParams);
    try
      LDataSet := TVirtualTable.Create(nil);
      try
        LDataSet.Assign(LCustomInternalQuery);
        if Assigned(FMemoryStream) then
          FMemoryStream.Clear
        else
          FMemoryStream := TMemoryStream.Create;

        LDataSet.SaveToStream(FMemoryStream, True);
      finally
        LDataSet.Free;
      end;
    finally
      TSQLConnectionProvider(ASQLConnectionProvider).CloseSQLQuery;
    end;
  end;
{$ENDIF}
end;

{ TInternalConnectForm }

constructor TInternalConnectForm.Create(AOwner: TComponent);
begin
  inherited;
  FHeightOffset := lbUsername.Top - lbProvider.Top;
  FHeightPostFix := Height - lbDatabase.Top;
end;

procedure TInternalConnectForm.DoInit;
var
  LLabelTop, LEditorTop: Integer;
begin
  inherited;

//  LLabelTop := lbUsername.Top; LEditorTop := edUserName.Top;
  LLabelTop := lbProvider.Top; LEditorTop := edProvider.Top;
  lbProvider.Visible := False; edProvider.Visible := False;

  if edUserName.Enabled then begin
    lbUsername.Top := LLabelTop; edUserName.Top := LEditorTop;
    Inc(LLabelTop, FHeightOffset); Inc(LEditorTop, FHeightOffset);
    lbUsername.Visible := True; edUserName.Visible := True;
    edUserName.Enabled := (not TInternalConnectDialog(ConnectDialog).DisableLoginChange);
  end else begin
    lbUsername.Visible := False; edUserName.Visible := False;
  end;

  if edPassword.Enabled then begin
    lbPassword.Top := LLabelTop; edPassword.Top := LEditorTop;
    Inc(LLabelTop, FHeightOffset); Inc(LEditorTop, FHeightOffset);
    lbPassword.Visible := True; edPassword.Visible := True;
    edPassword.Enabled := (not TInternalConnectDialog(ConnectDialog).DisableLoginChange);
  end else begin
    lbPassword.Visible := False; edPassword.Visible := False;
  end;

  if edServer.Enabled then begin
    lbServer.Top := LLabelTop; edServer.Top := LEditorTop;
    Inc(LLabelTop, FHeightOffset); Inc(LEditorTop, FHeightOffset);
    lbServer.Visible := True; edServer.Visible := True;
    edServer.Enabled := (not TInternalConnectDialog(ConnectDialog).DisableConnectionChange);
  end else begin
    lbServer.Visible := False; edServer.Visible := False;
  end;

  if edPort.Enabled and (not TInternalConnectDialog(ConnectDialog).HidePort) then begin
    lbPort.Top := LLabelTop; edPort.Top := LEditorTop;
    Inc(LLabelTop, FHeightOffset); Inc(LEditorTop, FHeightOffset);
    lbPort.Visible := True; edPort.Visible := True;
    edPort.Enabled := (not TInternalConnectDialog(ConnectDialog).DisableConnectionChange);
  end else begin
    lbPort.Visible := False; edPort.Visible := False;
  end;

  if edDatabase.Enabled then begin
    lbDatabase.Top:= LLabelTop; edDatabase.Top := LEditorTop;
    Inc(LLabelTop, FHeightOffset); //Inc(LEditorTop, FHeightOffset);
    lbDatabase.Visible := True; edDatabase.Visible := True;
    edDatabase.Enabled := (not TInternalConnectDialog(ConnectDialog).DisableConnectionChange);
  end else begin
    lbDatabase.Visible := False; edDatabase.Visible := False;
  end;

  Height := LLabelTop + FHeightPostFix - FHeightOffset;
end;

{$IFDEF FASTSCRIPT_RTTI}

{ TfsUniConnectionInfoMessageEvent }

function TfsUniConnectionInfoMessageEvent.GetMethod: Pointer;
begin
  Result := @TfsUniConnectionInfoMessageEvent.DoEvent;
end;

procedure TfsUniConnectionInfoMessageEvent.DoEvent(Sender: TObject; E: EMSError);
begin
  CallHandler([Sender, E]);
end;

{ TfsDAConnectionErrorEvent }

function TfsDAConnectionErrorEvent.GetMethod: Pointer;
begin
  Result := @TfsDAConnectionErrorEvent.DoEvent;
end;

procedure TfsDAConnectionErrorEvent.DoEvent(Sender: TObject; E: EDAError; var Fail: boolean);
begin
  CallHandler([Sender, E, Fail]);
  Fail := Handler.Params[2].Value = True;
end;

{ TFunctions_DHI }

constructor TFunctions_DHI.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TCustomDAConnection, TCustomDAConnection.ClassParent.ClassName) do begin
    AddProperty('InTransaction', DelphiTypeBoolean, Get_TCustomDAConnection_Property, nil);
    AddMethod('procedure RollBack', Call_TCustomDAConnection_Method);
  end;

  with AScript.AddClass(TUniConnection, TUniConnection.ClassParent.ClassName) do begin
    AddEvent('OnInfoMessage', TfsUniConnectionInfoMessageEvent);
    AddEvent('OnError', TfsDAConnectionErrorEvent);
  end;

  with AScript.AddClass(TCustomUniDataSet, TCustomUniDataSet.ClassParent.ClassName) do begin
    AddMethod('procedure OpenNext', Call_TUniDataSet_Method);
    AddMethod('procedure BreakExec', Call_TUniDataSet_Method);
//    AddMethod('function !asNextResultSet: Boolean', Call_TUniDataSet_Method);
  end;
end;

function TFunctions_DHI.Get_TCustomDAConnection_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'INTRANSACTION' then
    Result := TCustomDAConnection(Instance).InTransaction
end;

function TFunctions_DHI.Call_TCustomDAConnection_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'ROLLBACK' then
    TCustomDAConnection(Instance).Rollback;
end;

function TFunctions_DHI.Call_TUniDataSet_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'OPENNEXT' then
    TCustomUniDataSet(Instance).OpenNext
  else if MethodName = 'BREAKEXEC' then
    TCustomUniDataSet(Instance).BreakExec

//  else if MethodName = '!ASNEXTRESULTSET' then
//    Result := TCustomUniDataSet(Instance).!asNextResultSet
end;

{$ENDIF}

{ TInternalProviderInterpreter }

class function TInternalProviderInterpreter.GetInterpreter(const AProviderName: String): TInternalProviderInterpreterClass;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Length(FInterpreters) - 1 do with FInterpreters[I] do
    if SameText(ProviderName, AProviderName) then
      Exit(Interpreter);
end;

class procedure TInternalProviderInterpreter.RegisterInterpreter(const AProviderName: String; AInterpreter: TInternalProviderInterpreterClass);
var
  I: Integer;
begin
  I := Length(FInterpreters);
  SetLength(FInterpreters, I + 1);
  with FInterpreters[I] do begin
    ProviderName := AProviderName;
    Interpreter := AInterpreter;
  end;
end;

class procedure TInternalProviderInterpreter.Release;
begin
  SetLength(FInterpreters, 0);
end;

{ TInternalThreadWrapper }

constructor TInternalThreadWrapper.Create(ForceStartTimer: boolean);
begin
  inherited Create(ForceStartTimer);
  AfterCreate;
end;

{ TInternalPublicQuery }

constructor TInternalPublicQuery.Create(AOwner: TComponent);
begin
  inherited;
  Disconnected := True;
end;

procedure TInternalPublicQuery.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  if not (Event in [deRecordChange, deDataSetChange, deDataSetScroll, deUpdateRecord, deCheckBrowseMode, deFocusControl])
  then
    inherited;
end;

procedure TInternalPublicQuery.SetFiltered(Value: Boolean);
begin
  Assert(not Value);
end;

procedure TInternalPublicQuery.SetFilterText(const Value: string);
begin
  Assert(Value.IsEmpty);
end;

initialization
  RegisterClasses
  (
    [
      TInternalConnectForm,
      TUniQuery,
      TInternalConnectDialog,
      TVirtualTable,
      TUserMemoryDataSet, TInternalMemoryDataSet,
      TInternalConnection, TInternalQuery
    ]
  );

{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TFunctions_DHI);
{$ENDIF}

finalization
  UnRegisterClasses
  (
    [
      TInternalConnectForm,
      TUniQuery,
      TInternalConnectDialog,
      TVirtualTable,
      TUserMemoryDataSet, TInternalMemoryDataSet,
      TInternalConnection, TInternalQuery, TInternalPublicQuery
    ]
  );

  TInternalProviderInterpreter.Release;

end.
