unit SBaseDataHelperSdac;
{$I config.inc}

interface

uses
  Classes, DB, MemDS, DBAccess, MSAccess, OLEDBAccess, VirtualTable, MemData, SdacVcl, MSServiceBroker

{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter, fs_idbrtti, fs_itools
{$ENDIF}

  , SBaseDataModule;

type
  PWordSet = PWord;

  TMSQueryProvidedDataSet = class(TMSQuery)
  protected
    procedure DoAfterOpen; override;
  end;

  TMSConnectionHelper = class(TBaseConnectionHelper)
  private
    procedure OnInfoMessage(Sender: TObject; E: EMSError);
  protected
    class function GetDataSetHelperClass: TBaseDataSetHelperClass; override;

    function  GetServer: String; override;
    procedure SetServer(const Value: String); override;
    function  GetDataBase: String; override;
    procedure SetDataBase(const Value: String); override;

    function  GetConnectDialog: TComponent; override;
    procedure SetConnectDialog(const Value: TComponent); override;

    procedure SetOnInfoMessage(AValue: TBaseConnectionOnInfoMessage); override;
  public
    function CreateDataSet(AOwner: TComponent): TDataSet; override;
    function ConnectionInfo: String; override;

    procedure BeginTran; override;
    procedure CommitTran(AIfPresent: Boolean); override;
    procedure RollbackTran(AIfPresent: Boolean); override;
    function  TranCount: Integer; override;
  end;

  TMSDataSetHelper = class(TBaseDataSetHelper)
  private
    FBeforeUpdateExecute: TMSUpdateExecuteEvent;
    FAfterUpdateExecute: TMSUpdateExecuteEvent;

    function StatementTypes(AStatementTypes: TStatementTypes): TBaseSQLStatementTypes;
    procedure DataSetBeforeUpdateExecute(ASender: TCustomMSDataSet; AStatementTypes: TStatementTypes; AParams: TMSParams);
    procedure DataSetAfterUpdateExecute(ASender: TCustomMSDataSet; AStatementTypes: TStatementTypes; AParams: TMSParams);
  protected
    function  GetConnection: TCustomConnection; override;
    procedure SetConnection(const Value: TCustomConnection); override;
    function  GetCachedUpdates: Boolean; override;
    procedure SetCachedUpdates(const Value: Boolean); override;
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(const Value: Boolean); override;
    function GetUpdateStatusFilter: TUpdateStatusSet; override;
    procedure SetUpdateStatusFilter(Value: TUpdateStatusSet); override;
    function  GetAutoGenerateUpdateScripts: Boolean; override;
    procedure SetAutoGenerateUpdateScripts(const Value: Boolean); override;

    function GetKeyFields: String; override;
    procedure SetKeyFields(const Value: String); override;

    function GetDetailFields: String; override;
    procedure SetDetailFields(const Value: String); override;

    function GetMasterFields: String; override;
    procedure SetMasterFields(const Value: String); override;

    function GetMasterSource: TDataSource; override;
    procedure SetMasterSource(const Value: TDataSource); override;

    function GetIndexFieldNames: String; override;
    procedure SetIndexFieldNames(const Value: String); override;

    function GetSQL: String; override;

    function GetSQLInsert: String; override;
    function GetSQLUpdate: String; override;
    function GetSQLDelete: String; override;
    function GetSQLRefresh: String; override;

    procedure SetSQLInsert(const Value: String); override;
    procedure SetSQLUpdate(const Value: String); override;
    procedure SetSQLDelete(const Value: String); override;
    procedure SetSQLRefresh(const Value: String); override;

    procedure SetBeforeUpdateExecute(const Value: TBaseDataSetUpdateExecuteEvent); override;
    procedure SetAfterUpdateExecute(const Value: TBaseDataSetUpdateExecuteEvent); override;
  public
    class function Supports: TBaseDataSetSupportedActions; override;
    constructor Create(ADataSet: TDataSet); override;

    procedure OpenNext; override;
    function  HasNextResultSet: Boolean; override;
    procedure UnPrepare; override;
    procedure RefreshRecord; override;
    procedure ApplyUpdates; override;
    procedure CommitUpdates; override;
    procedure CancelUpdates; override;
    procedure RestoreUpdates; override;
    procedure RevertRecord; override;
    function  UpdatesPending: Boolean; override;

    procedure BreakExec; override;
    procedure RefreshDetail; override;
  end;

  TMSSQLServiceBrokerProvider = class(TSQLServiceBrokerProvider)
  private
    FMSServiceBroker: TMSServiceBroker;
    procedure OnMessageEvent(Sender: TObject);
  protected
    function GetStartTimeout: Integer; override;
    procedure SetAsyncNotification(const Value: Boolean); override;
    procedure SetStartTimeout(const Value: Integer); override;
    function GetAsyncNotification: Boolean; override;
    function GetMessageType: String; override;
    function GetMessageBody: String; override;
    function GetConnection: TCustomConnection; override;
    procedure SetConnection(const Value: TCustomConnection); override;
    function GetQueue: String; override;
    function GetService: String; override;
    procedure SetQueue(const Value: String); override;
    procedure SetService(const Value: String); override;
  public
    class function SEndDialogType: String; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Receive: Boolean; override;
  end;

{$IFDEF FASTSCRIPT_RTTI}
  TFunctions_Sdac = class(TfsRTTIModule)
  private
    function  Call_TDADataSet_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Get_TMemDataSet_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function  Call_TMemDataSet_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TCustomDAConnection_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function  Call_TCustomDAConnection_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TMacro_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TMacro_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Call_TMSDataSet_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

  TfsMSUpdateExecuteEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TCustomMSDataSet; StatementTypes: TStatementTypes; Params: TMSParams);
    function GetMethod: Pointer; override;
  end;

  TfsMSConnectionInfoMessageEvent = class(TfsCustomEvent)
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


implementation

uses
  SysUtils, SBaseConstants;

type
  TCustomSQLDataSetProviderCrack = class(TCustomSQLDataSetProvider);
  TCustomMSDataSetCrack = class(TCustomMSDataSet);

{ TMSQueryProvidedDataSet }

procedure TMSQueryProvidedDataSet.DoAfterOpen;
begin
  if Owner is TCustomSQLDataSetProvider then
    TCustomSQLDataSetProviderCrack(Owner).SyncronizeOutputParameters(Self);

  inherited;
end;

{ TMSConnectionHelper }

class function TMSConnectionHelper.GetDataSetHelperClass: TBaseDataSetHelperClass;
begin
  Result := TMSDataSetHelper
end;

procedure TMSConnectionHelper.SetOnInfoMessage(AValue: TBaseConnectionOnInfoMessage);
begin
  inherited;

  with TMSConnection(Connection) do
    if Assigned(FOnInfoMessage) then
      OnInfoMessage := Self.OnInfoMessage
    else
      OnInfoMessage := nil
end;

procedure TMSConnectionHelper.OnInfoMessage(Sender: TObject; E: EMSError);
begin
  if Assigned(FOnInfoMessage) then
    FOnInfoMessage(Connection, E.Message);
end;

procedure TMSConnectionHelper.BeginTran;
begin
  TMSConnection(Connection).ExecSQL('BEGIN TRAN', []);
end;

procedure TMSConnectionHelper.CommitTran(AIfPresent: Boolean);
var
  LSQL: String;
begin
  if AIfPresent then
    LSQL := 'IF @@TRANCOUNT > 0 '
  else
    LSQL := '';

  TMSConnection(Connection).ExecSQL(LSQL + 'COMMIT TRAN', []);
end;

procedure TMSConnectionHelper.RollbackTran(AIfPresent: Boolean);
var
  LSQL: String;
begin
  if AIfPresent then
    LSQL := 'IF @@TRANCOUNT > 0 '
  else
    LSQL := '';

  TMSConnection(Connection).ExecSQL(LSQL + 'ROLLBACK TRAN', []);
end;

function TMSConnectionHelper.TranCount: Integer;
begin
  Result := Integer(TMSConnection(Connection).ExecSQL('SET :Result = @@TRANCOUNT', []));
end;

function TMSConnectionHelper.ConnectionInfo: String;
begin
  with TMSConnection(Connection) do
    Result := SConst_Server + '=' + Server + ';' + SConst_DataBase + '=' + Database + ';' + SConst_UserName + '=' + Username
end;

function TMSConnectionHelper.CreateDataSet(AOwner: TComponent): TDataSet;
begin
  Result := TMSQueryProvidedDataSet.Create(AOwner);
  with TMSQuery(Result) do begin
    Connection := TMSConnection(Self.Connection);

    CachedUpdates := False;
    LocalUpdate   := True;

    Options.ReturnParams      := True;
    Options.RemoveOnRefresh   := True;
    Options.StrictUpdate      := False;
    Options.LocalMasterDetail := True;
  end;
end;

function TMSConnectionHelper.GetServer: String;
begin
  Result := TMSConnection(Connection).Server
end;

procedure TMSConnectionHelper.SetServer(const Value: String);
begin
  TMSConnection(Connection).Server := Value
end;

function TMSConnectionHelper.GetDataBase: String;
begin
  Result := TMSConnection(Connection).Database
end;

procedure TMSConnectionHelper.SetDataBase(const Value: String);
begin
  TMSConnection(Connection).Database := Value
end;

function TMSConnectionHelper.GetConnectDialog: TComponent;
begin
  Result := TMSConnection(Connection).ConnectDialog
end;

procedure TMSConnectionHelper.SetConnectDialog(const Value: TComponent);
begin
  Assert((Value = nil) or (Value is DBAccess.TCustomConnectDialog));
  TMSConnection(Connection).ConnectDialog := DBAccess.TCustomConnectDialog(Value);
end;

{ TMSDataSetHelper }

constructor TMSDataSetHelper.Create(ADataSet: TDataSet);
begin
  if ADataSet is TCustomMSDataSet then
    inherited
  else
    Raise Exception.Create('Class ' + ADataSet.ClassName + ' is not inherited from TCustomMSDataSet');
end;

function TMSDataSetHelper.StatementTypes(AStatementTypes: TStatementTypes): TBaseSQLStatementTypes;
begin
  Result := [];

  if stInsert in AStatementTypes then
    Include(Result, bstInsert);

  if stUpdate in AStatementTypes then
    Include(Result, bstUpdate);

  if stDelete in AStatementTypes then
    Include(Result, bstDelete);

  if stRefresh in AStatementTypes then
    Include(Result, bstRefresh);
end;

procedure TMSDataSetHelper.DataSetBeforeUpdateExecute(ASender: TCustomMSDataSet; AStatementTypes: TStatementTypes; AParams: TMSParams);
begin
  if Assigned(FAfterUpdateExecute) then
    FBeforeUpdateExecute(ASender, AStatementTypes, AParams);

  BeforeUpdateExecute(ASender, StatementTypes(AStatementTypes), AParams);
end;

procedure TMSDataSetHelper.DataSetAfterUpdateExecute(ASender: TCustomMSDataSet; AStatementTypes: TStatementTypes; AParams: TMSParams);
begin
  if Assigned(FAfterUpdateExecute) then
    FAfterUpdateExecute(ASender, AStatementTypes, AParams);

  AfterUpdateExecute(ASender, StatementTypes(AStatementTypes), AParams);
end;

function TMSDataSetHelper.GetConnection: TCustomConnection;
begin
  Result := TCustomMSDataSet(DataSet).Connection
end;

function TMSDataSetHelper.GetDetailFields: String;
begin
  Result :=  TCustomMSDataSet(DataSet).DetailFields
end;

function TMSDataSetHelper.GetIndexFieldNames: String;
begin
  Result :=  TCustomMSDataSet(DataSet).IndexFieldNames;
end;

procedure TMSDataSetHelper.SetIndexFieldNames(const Value: String);
begin
  TCustomMSDataSet(DataSet).IndexFieldNames := Value;
end;

function TMSDataSetHelper.GetKeyFields: String;
begin
  Result :=  TCustomMSDataSet(DataSet).KeyFields
end;

procedure TMSDataSetHelper.SetKeyFields(const Value: String);
begin
  TCustomMSDataSet(DataSet).KeyFields := Value
end;

function TMSDataSetHelper.GetMasterFields: String;
begin
  Result :=  TCustomMSDataSet(DataSet).MasterFields
end;

function TMSDataSetHelper.GetMasterSource: TDataSource;
begin
  Result :=  TCustomMSDataSet(DataSet).MasterSource
end;

procedure TMSDataSetHelper.SetDetailFields(const Value: String);
begin
  TCustomMSDataSet(DataSet).DetailFields := Value
end;

procedure TMSDataSetHelper.SetMasterFields(const Value: String);
begin
  TCustomMSDataSet(DataSet).MasterFields := Value
end;

procedure TMSDataSetHelper.SetMasterSource(const Value: TDataSource);
begin
  TCustomMSDataSet(DataSet).MasterSource := Value
end;

procedure TMSDataSetHelper.SetConnection(const Value: TCustomConnection);
begin
  Assert(Value is TCustomMSConnection);
  TCustomMSDataSet(DataSet).Connection := TCustomMSConnection(Value);
end;

function TMSDataSetHelper.GetAutoGenerateUpdateScripts: Boolean;
begin
  Result := not TCustomMSDataSet(DataSet).LocalUpdate
end;

procedure TMSDataSetHelper.SetBeforeUpdateExecute(const Value: TBaseDataSetUpdateExecuteEvent);
begin
  inherited;

  if Assigned(Value) then begin
    if @TCustomMSDataSet(DataSet).BeforeUpdateExecute <> @TMSDataSetHelper.DataSetBeforeUpdateExecute then begin
      FBeforeUpdateExecute := TCustomMSDataSet(DataSet).BeforeUpdateExecute;
      TCustomMSDataSet(DataSet).BeforeUpdateExecute := DataSetBeforeUpdateExecute
    end;
  end else begin
    if @TCustomMSDataSet(DataSet).BeforeUpdateExecute = @TMSDataSetHelper.DataSetBeforeUpdateExecute then
      TCustomMSDataSet(DataSet).BeforeUpdateExecute := FBeforeUpdateExecute;
    FBeforeUpdateExecute := nil;
  end;
end;

procedure TMSDataSetHelper.SetAfterUpdateExecute(const Value: TBaseDataSetUpdateExecuteEvent);
begin
  inherited;

  if Assigned(Value) then begin
    if @TCustomMSDataSet(DataSet).AfterUpdateExecute <> @TMSDataSetHelper.DataSetAfterUpdateExecute then begin
      FAfterUpdateExecute := TCustomMSDataSet(DataSet).AfterUpdateExecute;
      TCustomMSDataSet(DataSet).AfterUpdateExecute := DataSetAfterUpdateExecute;
    end
  end else begin
    if @TCustomMSDataSet(DataSet).AfterUpdateExecute = @TMSDataSetHelper.DataSetAfterUpdateExecute then
      TCustomMSDataSet(DataSet).AfterUpdateExecute := FAfterUpdateExecute;
    FAfterUpdateExecute := nil;
  end;
end;

procedure TMSDataSetHelper.SetAutoGenerateUpdateScripts(const Value: Boolean);
begin
  TCustomMSDataSet(DataSet).LocalUpdate := not Value;
end;

function TMSDataSetHelper.GetCachedUpdates: Boolean;
begin
  Result := TCustomMSDataSet(DataSet).CachedUpdates
end;

procedure TMSDataSetHelper.SetCachedUpdates(const Value: Boolean);
begin
  TCustomMSDataSet(DataSet).CachedUpdates := Value
end;

procedure TMSDataSetHelper.OpenNext;
begin
  TCustomMSDataSet(DataSet).OpenNext
end;

function TMSDataSetHelper.HasNextResultSet: Boolean;
begin
  Result := TCustomMSDataSet(DataSet).HasNextResultSet
end;

procedure TMSDataSetHelper.UnPrepare;
begin
  TCustomMSDataSet(DataSet).UnPrepare
end;

function TMSDataSetHelper.UpdatesPending: Boolean;
begin
  Result := TCustomMSDataSet(DataSet).UpdatesPending
end;

procedure TMSDataSetHelper.RefreshDetail;
begin
  TCustomMSDataSetCrack(DataSet).RefreshParams;
end;

procedure TMSDataSetHelper.RefreshRecord;
begin
  TCustomMSDataSet(DataSet).RefreshRecord
end;

procedure TMSDataSetHelper.RestoreUpdates;
begin
  TCustomMSDataSet(DataSet).RestoreUpdates
end;

procedure TMSDataSetHelper.RevertRecord;
begin
  TCustomMSDataSet(DataSet).RevertRecord
end;

procedure TMSDataSetHelper.ApplyUpdates;
begin
  TCustomMSDataSet(DataSet).ApplyUpdates
end;

procedure TMSDataSetHelper.BreakExec;
begin
  TCustomMSDataSet(DataSet).BreakExec
end;

procedure TMSDataSetHelper.CommitUpdates;
begin
  TCustomMSDataSet(DataSet).CommitUpdates
end;

procedure TMSDataSetHelper.CancelUpdates;
begin
  TCustomMSDataSet(DataSet).CancelUpdates
end;

function TMSDataSetHelper.GetReadOnly: Boolean;
begin
  Result := TCustomMSDataSet(DataSet).ReadOnly
end;

procedure TMSDataSetHelper.SetReadOnly(const Value: Boolean);
begin
  TCustomMSDataSet(DataSet).ReadOnly := Value;
end;

function TMSDataSetHelper.GetSQL: String;
begin
  Result := TCustomMSDataSet(DataSet).SQL.Text
end;

function TMSDataSetHelper.GetSQLInsert: String;
begin
  Result := TCustomMSDataSet(DataSet).SQLInsert.Text
end;

function TMSDataSetHelper.GetSQLUpdate: String;
begin
  Result := TCustomMSDataSet(DataSet).SQLUpdate.Text
end;

function TMSDataSetHelper.GetUpdateStatusFilter: TUpdateStatusSet;
var
  LUpdateRecordTypes: TUpdateRecordTypes;
begin
  if TCustomMSDataSet(DataSet).CachedUpdates then begin
    Result := [];
    LUpdateRecordTypes := TCustomMSDataSet(DataSet).UpdateRecordTypes;
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

procedure TMSDataSetHelper.SetUpdateStatusFilter(Value: TUpdateStatusSet);
var
  LUpdateRecordTypes: TUpdateRecordTypes;
begin
  if TCustomMSDataSet(DataSet).CachedUpdates then begin
    LUpdateRecordTypes := [];
    if usUnmodified in Value then
      Include(LUpdateRecordTypes, rtUnmodified);
    if usModified in Value then
      Include(LUpdateRecordTypes, rtModified);
    if usInserted in Value then
      Include(LUpdateRecordTypes, rtInserted);
    if usDeleted in Value then
      Include(LUpdateRecordTypes, rtDeleted);

    TCustomMSDataSet(DataSet).UpdateRecordTypes := LUpdateRecordTypes;
  end else
    Assert((Value = []) or (Value = Default_UpdateStatusFilter))
end;

function TMSDataSetHelper.GetSQLDelete: String;
begin
  Result := TCustomMSDataSet(DataSet).SQLDelete.Text
end;

function TMSDataSetHelper.GetSQLRefresh: String;
begin
  Result := TCustomMSDataSet(DataSet).SQLRefresh.Text
end;

procedure TMSDataSetHelper.SetSQLInsert(const Value: String);
begin
  TCustomMSDataSet(DataSet).SQLInsert.Text := Value
end;

procedure TMSDataSetHelper.SetSQLUpdate(const Value: String);
begin
  TCustomMSDataSet(DataSet).SQLUpdate.Text := Value
end;

class function TMSDataSetHelper.Supports: TBaseDataSetSupportedActions;
begin
  Result := [bdssaSQL, bdssaKeyFields];
end;

procedure TMSDataSetHelper.SetSQLDelete(const Value: String);
begin
  TCustomMSDataSet(DataSet).SQLDelete.Text := Value
end;

procedure TMSDataSetHelper.SetSQLRefresh(const Value: String);
begin
  TCustomMSDataSet(DataSet).SQLRefresh.Text := Value
end;

{ TMSSQLServiceBrokerProvider }

constructor TMSSQLServiceBrokerProvider.Create(AOwner: TComponent);
begin
  inherited;

  FMSServiceBroker := TMSServiceBroker.Create(Self);
  FMSServiceBroker.OnMessage := OnMessageEvent;
end;

destructor TMSSQLServiceBrokerProvider.Destroy;
begin
  FreeAndNil(FMSServiceBroker);
  inherited;
end;

function TMSSQLServiceBrokerProvider.GetAsyncNotification: Boolean;
begin
  Result := FMSServiceBroker.AsyncNotification
end;

function TMSSQLServiceBrokerProvider.GetConnection: TCustomConnection;
begin
  Result := FMSServiceBroker.Connection
end;

procedure TMSSQLServiceBrokerProvider.SetConnection(const Value: TCustomConnection);
begin
  if (Value <> nil) and not (Value is TMSConnection) then
    Raise Exception.Create(SAbstractError);

  FMSServiceBroker.Connection := TMSConnection(Value)
end;

function TMSSQLServiceBrokerProvider.GetQueue: String;
begin
  Result := FMSServiceBroker.Queue
end;

procedure TMSSQLServiceBrokerProvider.SetQueue(const Value: String);
begin
{$IFNDEF PACKAGE}
  FMSServiceBroker.Queue := Value
{$ENDIF}
end;

function TMSSQLServiceBrokerProvider.GetService: String;
begin
  Result := FMSServiceBroker.Service
end;

procedure TMSSQLServiceBrokerProvider.SetService(const Value: String);
begin
  FMSServiceBroker.Service := Value
end;

function TMSSQLServiceBrokerProvider.GetMessageBody: String;
begin
  Result := FMSServiceBroker.CurrentMessage.AsString;
end;

function TMSSQLServiceBrokerProvider.GetMessageType: String;
begin
  Result := FMSServiceBroker.CurrentMessage.MessageType;
end;

function TMSSQLServiceBrokerProvider.GetStartTimeout: Integer;
begin
  Result := FMSServiceBroker.StartTimeout;
end;

class function TMSSQLServiceBrokerProvider.SEndDialogType: String;
begin
  Result := MSServiceBroker.SEndDialogType
end;

procedure TMSSQLServiceBrokerProvider.SetAsyncNotification(const Value: Boolean);
begin
  FMSServiceBroker.AsyncNotification := Value;
end;

procedure TMSSQLServiceBrokerProvider.SetStartTimeout(const Value: Integer);
begin
  FMSServiceBroker.StartTimeout := Value;
end;

function TMSSQLServiceBrokerProvider.Receive: Boolean;
begin
  Result := FMSServiceBroker.Receive
end;

procedure TMSSQLServiceBrokerProvider.OnMessageEvent(Sender: TObject);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self)
end;

{$IFDEF FASTSCRIPT_RTTI}
{ TFunctions_Sdac }

constructor TFunctions_Sdac.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TCustomDAConnection, TCustomDAConnection.ClassParent.ClassName) do begin
    AddProperty('InTransaction', DelphiTypeBoolean, Get_TCustomDAConnection_Property, nil);
    AddMethod('procedure RollBack', Call_TCustomDAConnection_Method);
  end;

  AScript.AddClass(TCustomMSConnection, TCustomMSConnection.ClassParent.ClassName);

  with AScript.AddClass(TMSConnection, TMSConnection.ClassParent.ClassName) do begin
    AddEvent('OnInfoMessage', TfsMSConnectionInfoMessageEvent);
    AddEvent('OnError', TfsDAConnectionErrorEvent);
  end;

  with AScript.AddClass(TMacro, TMacro.ClassParent.ClassName) do begin
    AddProperty('AsString', DelphiTypeString, Get_TMacro_Property, Set_TMacro_Property);
    AddProperty('AsInteger', DelphiTypeInteger, Get_TMacro_Property, Set_TMacro_Property);
  end;

  with AScript.AddClass(TCustomDADataSet, TCustomDADataSet.ClassParent.ClassName) do begin
    AddMethod('procedure Execute', Call_TDADataSet_Method);
    AddMethod('procedure RefreshRecord', Call_TDADataSet_Method);
    AddMethod('procedure Prepare', Call_TDADataSet_Method);
    AddMethod('procedure UnPrepare', Call_TDADataSet_Method);
    AddMethod('procedure ApplyUpdates', Call_TDADataSet_Method);

    AddMethod('function ParamByName(const ParamName: String): TDAParam', Call_TDADataSet_Method);
    AddMethod('function MacroByName(const Value: String): TMacro', Call_TDADataSet_Method);
  end;

  AScript.AddEnumSet('TLocateExOptions', 'lxCaseInsensitive, lxPartialKey, lxNearest, lxNext, lxUp, lxPartialCompare');

  with AScript.AddClass(TMemDataSet, TMemDataSet.ClassParent.ClassName) do begin
    AddProperty('UpdatesPending', DelphiTypeBoolean, Get_TMemDataSet_Property);
    AddMethod('function LocateEx(const KeyFields: string; const KeyValues: Variant;Options: TLocateExOptions): Boolean', Call_TMemDataSet_Method);
  end;

  with AScript.AddClass(TCustomMSDataSet, TCustomMSDataSet.ClassParent.ClassName) do begin
    AddMethod('procedure OpenNext', Call_TMSDataSet_Method);
    AddMethod('procedure BreakExec', Call_TMSDataSet_Method);
  end;
end;

function TFunctions_Sdac.Call_TDADataSet_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'EXECUTE' then
    TCustomDADataSet(Instance).Execute
  else if MethodName = 'REFRESHRECORD' then
    TCustomDADataSet(Instance).RefreshRecord
  else if MethodName = 'PREPARE' then
    TCustomDADataSet(Instance).Prepare
  else if MethodName = 'UNPREPARE' then
    TCustomDADataSet(Instance).UnPrepare
  else if MethodName = 'APPLYUPDATES' then
    TCustomDADataSet(Instance).ApplyUpdates
  else if MethodName = 'PARAMBYNAME' then
    Result := Integer(TCustomDADataSet(Instance).ParamByName(Caller.Params[0]))
  else if MethodName = 'MACROBYNAME' then
    Result := Integer(TCustomDADataSet(Instance).MacroByName(Caller.Params[0]))
end;

function TFunctions_Sdac.Call_TMemDataSet_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  function IntToLocateExOptions(I: Integer): TLocateExOptions;
  begin
    PByte(@Result)^ := I;
  end;

begin
  if MethodName = 'LOCATEEX' then
    Result := TMemDataSet(Instance).LocateEx(Caller.Params[0], Caller.Params[1], IntToLocateExOptions(Caller.Params[2]))
end;

function TFunctions_Sdac.Call_TMSDataSet_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'OPENNEXT' then
    TCustomMSDataSet(Instance).OpenNext
  else if MethodName = 'BREAKEXEC' then
    TCustomMSDataSet(Instance).BreakExec
end;

function TFunctions_Sdac.Get_TCustomDAConnection_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'INTRANSACTION' then
    Result := TCustomDAConnection(Instance).InTransaction
end;

function TFunctions_Sdac.Call_TCustomDAConnection_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'ROLLBACK' then
    TCustomDAConnection(Instance).Rollback;
end;

function TFunctions_Sdac.Get_TMacro_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'ASSTRING' then
    Result := TMacro(Instance).AsString
  else if PropName = 'ASINTEGER' then
    Result := TMacro(Instance).AsInteger
end;

function TFunctions_Sdac.Get_TMemDataSet_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'UPDATESPENDING' then
    Result := TMemDataSet(Instance).UpdatesPending
end;

procedure TFunctions_Sdac.Set_TMacro_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
  if PropName = 'ASSTRING' then
    TMacro(Instance).AsString := Value
  else if PropName = 'ASINTEGER' then
    TMacro(Instance).AsInteger := Value
end;

{ TfsMSUpdateExecuteEvent }

function TfsMSUpdateExecuteEvent.GetMethod: Pointer;
begin
  Result := @TfsMSUpdateExecuteEvent.DoEvent;
end;

procedure TfsMSUpdateExecuteEvent.DoEvent(Sender: TCustomMSDataSet; StatementTypes: TStatementTypes; Params: TMSParams);
var
  LTypes: Integer;
begin
  LTypes := Integer(PWordSet(@StatementTypes)^);

  CallHandler([Sender, LTypes, Params]);
end;

{ TfsMSConnectionInfoMessageEvent }

function TfsMSConnectionInfoMessageEvent.GetMethod: Pointer;
begin
  Result := @TfsMSConnectionInfoMessageEvent.DoEvent;
end;

procedure TfsMSConnectionInfoMessageEvent.DoEvent(Sender: TObject; E: EMSError);
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

{$ENDIF}

initialization
  TBaseConnectionHelper.RegisterHelperClass(TMSConnection, TMSConnectionHelper);
  TBaseDataSetHelper.RegisterHelperClass(TCustomMSDataSet, TMSDataSetHelper);
  TSQLServiceBrokerProvider.RegisterProviderClass(TMSConnection, TMSSQLServiceBrokerProvider);

  RegisterClasses(
    [
      TMSQuery, TMSConnection, TMSConnectDialog,
      TMSParams, TDAParam,
      TMacro,

      TVirtualTable
    ]);

{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TFunctions_Sdac);
{$ENDIF}

finalization
  UnRegisterClasses(
    [
      TMSQuery, TMSConnection, TMSConnectDialog,
      TMSParams, TDAParam,
      TMacro,

      TVirtualTable
    ]);

end.
