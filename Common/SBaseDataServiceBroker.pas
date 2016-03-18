//////////////////////////////////////////////////
//  -=MD=- Data Access Components
//  Data - ServiceBroker
//////////////////////////////////////////////////

unit SBaseDataServiceBroker;

interface

uses
  SysUtils, Classes, DB,{$IFDEF VER6P} Variants,{$ENDIF}
  SBaseDataModule, SBaseDataHelperInternal, NamedVariables;

const
  SConst_Queue = 'Queue';
  SConst_WaitTimeout = 'WaitTimeout';

type
  TQueryListenThreadWrapper = class(TInternalThreadWrapper)
  protected
    FConnection : TInternalConnection;
    FQuery      : TCustomInternalQuery;
    FReleaseSQL : String;

    FMsgs: TThreadList;

    procedure AfterCreate; override;
    procedure InternalExecute; override;
  public
    destructor Destroy; override;
  end;

  TBaseBrokerMessageType = (bmtBeginDialog, bmtMessage, bmtEndDialog);

  TSQLServiceBrokerProvider = class;

  TSBaseDataMessage = class;
  TSBaseDataMessageClass = class of TSBaseDataMessage;

  TSBaseDataMessage = class
  protected
    FText   : String;
    FParams : TNamedVariants;
    FXML    : String;
//    FBinary : TBytes;
    FBinaryStream : TStream;

    FMessageType: TBaseBrokerMessageType;
  protected
    procedure Fill(Source: TCustomInternalQuery);
  public
    destructor Destroy; override;

    property MessageType: TBaseBrokerMessageType read FMessageType;
    property Text   : String read FText;
    property Params : TNamedVariants read FParams;
    property XML    : String read FXML;
    property Binary : TStream read FBinaryStream;

//    property IsEmpty: boolean read GetIsEmpty;
  end;

  TSBaseDataMessageEvent = procedure(Sender: TObject) of object;

  TSQLServiceBrokerProvider = class(TCustomSQLServiceBrokerProvider)
  strict private
    FSynchronizeConnectedStatus: Boolean;

    FConnectionProvider: TSQLConnectionProvider;
    FOriginalConnection: TInternalConnection;
    FOriginalConnectionActive: Boolean;
  protected
//    FQuery: TCustomInternalQuery;

//    FInitParams: TNamedVariants;

    FInitSQL   : String; // Запускается на соединении источника. По замыслу создаёт "Ухо"
    FReceiveSQL: String; // Запускается в соединении брокера. Получает сообщения. Может быть <Empty>
    FReleaseSQL: String; // Запускается на соединении источника. По замыслу удаляет "Ухо"

    FQueue          : String;
    FWaitTimeout    : Integer;

    FListener: TQueryListenThreadWrapper;

    FMsgs: TThreadList;
    FReceivePrev: boolean;

    // Async mode
    FOnMessage: TSBaseDataMessageEvent;
    FAsyncNotification: boolean;
    FStreamedAsyncNotification: boolean;
    FStopProcessing: boolean;

    function GetCurrentMessage: TSBaseDataMessage;

    procedure SetConnectionProvider(const AValue: TSQLConnectionProvider);
    procedure ConnectionChanged; override;

    procedure SetWaitTimeout(Value: integer);
    procedure SetOnMessage(Value: TSBaseDataMessageEvent);
//    procedure ConnectChange(Sender: TObject; Connecting: Boolean);

    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure CheckInactive;
    procedure BeginConnection;
    procedure EndConnection;

    class function ReceiveFromServer(Query: TCustomInternalQuery; Msgs: TThreadList): Integer; // Received messages count

    // Async mode
    procedure DoThreadMessage(Sender: TObject; Event: Pointer);

    procedure Start;
    function  AsyncActive: Boolean; inline;
    procedure Stop;
    procedure Release;

    procedure SetAsyncNotification(const AValue: boolean);
  protected
    procedure DoException(Sender: TObject; E: Exception; var Fail: boolean);
    procedure DoStop;
    procedure DoStart;
    procedure DoInit(AQuery: TCustomInternalQuery; AWaitTimeout: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property CurrentMessage: TSBaseDataMessage read GetCurrentMessage;

    function Receive: boolean;
    property ConnectionProvider: TSQLConnectionProvider read FConnectionProvider write SetConnectionProvider;

    property Queue: string read FQueue write FQueue;
//    property Connection: TInternalConnection read GetConnection;
  published
    property InitSQL   : String read FInitSQL write FInitSQL;
    property ReceiveSQL: String read FReceiveSQL write FReceiveSQL;
    property ReleaseSQL: String read FReleaseSQL write FReleaseSQL;

    property SynchronizeConnectedStatus: Boolean read FSynchronizeConnectedStatus write FSynchronizeConnectedStatus default False;
    property AsyncNotification: boolean read FAsyncNotification write SetAsyncNotification default False;
    property WaitTimeout: Integer read FWaitTimeout write SetWaitTimeout default -1;
//    property StartTimeout: Integer read FStartTimeout write FStartTimeout default 0;

    property OnMessage: TSBaseDataMessageEvent read FOnMessage write SetOnMessage;
  end;

//  TQueryListenThread = class(TInternalThread);

implementation

uses
  Windows, ComObj, ActiveX, SBaseStringFunctions;

const
  SServiceBrokerAsync = 'Cannot perform this operation in AsyncNotification mode';

type
  TSQLConnectionProviderCrack = class(TSQLConnectionProvider);

{ TQueryListenThreadWrapper }

procedure TQueryListenThreadWrapper.AfterCreate;
begin
  FConnection       := TInternalConnection.Create(nil);

  FQuery            := TCustomInternalQuery.Create(nil);
  FQuery.Connection := FConnection;
  FQuery.ReadOnly   := True;
//  FQuery.LocalUpdate := True;
end;

destructor TQueryListenThreadWrapper.Destroy;
begin
  TryTerminate;
  WaitFor;

  FQuery.Free;
  FConnection.Free;

  inherited;
end;

procedure TQueryListenThreadWrapper.InternalExecute;
var
  MsgCount: integer;
begin
  OleCheck(CoInitializeEx(nil, {$IFNDEF CLR}COINIT_APARTMENTTHREADED{$ELSE}COINIT_MULTITHREADED{$ENDIF}));
  try
    FQuery.Connection.Connect;
    //FQuery.CheckConnection;
    try
      while not Terminated do begin
        MsgCount := TSQLServiceBrokerProvider.ReceiveFromServer(FQuery, FMsgs);
        PostEvent(TObject(MsgCount));
      end;
    finally
      if (not FReleaseSQL.IsEmpty) and Assigned(FQuery.Connection) and FQuery.Connection.Connected then begin
        FQuery.UnPrepare;
        FQuery.SQL.Text := FReleaseSQL;
        FQuery.Execute;
      end;
    end;
  finally
    CoUninitialize;
  end;
end;

{ TSBaseDataMessage }

destructor TSBaseDataMessage.Destroy;
begin
  inherited;
end;

procedure TSBaseDataMessage.Fill(Source: TCustomInternalQuery);
var
  LField: TField;
  LString: String;

//  Blob: MemData.TBlob;
//  Handle: IntPtr;
begin
  LField := Source.FindField('message_type');
  FMessageType := bmtMessage;
  if Assigned(LField) and (not LField.IsNull) then begin
    LString := LField.AsString;
    if LString.Length = 1 then
      case LString[1] of
        'B': FMessageType := bmtBeginDialog;
        'E': FMessageType := bmtEndDialog;
      end;
  end;

  LField := Source.FindField('message_body_text');
  if (LField = nil) or LField.IsNull then
    FText := ''
  else
    FText := LField.AsString;

  LField := Source.FindField('message_body_params');
  if (LField = nil) or LField.IsNull then
    FParams.Clear
  else
    FParams.AsClrTParams := LField.AsString;

  LField := Source.FindField('message_body_xml');
  if (LField = nil) or LField.IsNull then
    FXML := ''
  else
    FXML := LField.AsString;

  LField := Source.FindField('message_body_binary');
  if (LField = nil) or LField.IsNull then
    FreeAndNil(FBinaryStream)
  else
    FBinaryStream := Source.CreateBlobStream(LField, bmRead);
end;

{ TSQLServiceBrokerProvider }

constructor TSQLServiceBrokerProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FWaitTimeout := -1;
  FMsgs := TThreadList.Create;
end;

destructor TSQLServiceBrokerProvider.Destroy;
var
  I: integer;
  LMsgList: TList;
begin
  ConnectionProvider := nil;
//  Stop;

  if Assigned(FMsgs) then begin
    LMsgList := FMsgs.LockList;
    try
      for I := 0 to LMsgList.Count - 1 do
        TObject(LMsgList[I]).Free;
    finally
      FMsgs.UnlockList;
      FMsgs.Free;
    end;
  end;

  inherited;
end;

procedure TSQLServiceBrokerProvider.Loaded;
begin
  inherited;

//  FDesignCreate := False;
  try
    if FStreamedAsyncNotification then
      AsyncNotification := True;
  except
    if csDesigning in ComponentState then
      ApplicationHandleException(Self)
    else
      raise;
  end;
end;

procedure TSQLServiceBrokerProvider.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FConnectionProvider then
      ConnectionProvider := nil
end;

procedure TSQLServiceBrokerProvider.AssignTo(Dest: TPersistent);
begin
  if Dest is TSQLServiceBrokerProvider then begin
//    TSQLServiceBrokerProvider(Dest).Connection  := Connection;
    TSQLServiceBrokerProvider(Dest).OnMessage   := OnMessage;
  end
  else
    inherited;
end;

function TSQLServiceBrokerProvider.AsyncActive: Boolean;
begin
  Result := (FListener <> nil)
end;

procedure TSQLServiceBrokerProvider.BeginConnection;
begin
  if FConnectionProvider = nil then
    Raise Exception.Create(SConnectionProviderNotDefined);
  FConnectionProvider.CheckConnection;
end;

procedure TSQLServiceBrokerProvider.EndConnection;
begin
//  if Connection <> nil then
//    Connection.Disconnect;
end;

procedure TSQLServiceBrokerProvider.CheckInactive;
begin
  if AsyncNotification then
    if ([csUpdating, csDesigning] * ComponentState) <> [] then
      Stop
    else
      DatabaseError(SServiceBrokerAsync, Self);
end;

//procedure TSQLServiceBrokerProvider.ConnectChange(Sender: TObject; Connecting: Boolean);
//begin
//  if not Connecting then begin
//    Stop;
//  end;
//end;

//function TSQLServiceBrokerProvider.GenerateReceiveSQL(const AQueue: String; const ATimeoutMSec: Integer): string;
//begin
//  if FReceiveSQL.IsEmpty then
//    Result := ConnectionProvider.Connection.Interpreter.GenerateReceiveScript(AQueue, ATimeoutMSec)
//  else
//    Result := FReceiveSQL
//end;

//function TSQLServiceBrokerProvider.GetConnection: TInternalConnection;
//begin
//  Result := ConnectionProvider.Connection;
//end;

function TSQLServiceBrokerProvider.GetCurrentMessage: TSBaseDataMessage;
var
  MsgList: TList;
begin
  MsgList := FMsgs.LockList;
  try
    if MsgList.Count > 0 then
      Result := TSBaseDataMessage(MsgList[0])
    else
      Result := nil;
  finally
    FMsgs.UnlockList;
  end;
end;

procedure TSQLServiceBrokerProvider.ConnectionChanged;
var
  LNewConnection: TInternalConnection;
  LNewConnectionActive: Boolean;
begin
  if Assigned(FConnectionProvider) then
    LNewConnection := FConnectionProvider.Connection
  else
    LNewConnection := nil;
  LNewConnectionActive := Assigned(LNewConnection) and LNewConnection.Connected;

  if (
      (FOriginalConnection <> LNewConnection)
      or
      (FSynchronizeConnectedStatus and (FOriginalConnectionActive <> LNewConnectionActive))
     )
        and AsyncActive
  then
    Stop;

  if FOriginalConnection <> LNewConnection then begin
    // Может надо что?
    FOriginalConnection := LNewConnection;
  end;

  if FOriginalConnectionActive <> LNewConnectionActive then begin
    // Может надо что?
    FOriginalConnectionActive := LNewConnectionActive;
  end;

  if LNewConnectionActive and FStreamedAsyncNotification and (not AsyncActive) then
    Start;
end;

procedure TSQLServiceBrokerProvider.SetWaitTimeout(Value: integer);
begin
  FWaitTimeout := Value;
end;

procedure TSQLServiceBrokerProvider.SetOnMessage(Value: TSBaseDataMessageEvent);
begin
  CheckInactive;
  FOnMessage := Value;
end;

class function TSQLServiceBrokerProvider.ReceiveFromServer(Query: TCustomInternalQuery; Msgs: TThreadList): integer; // Received messages count
var
  LMsg: TSBaseDataMessage;
  LMsgList: TList;
begin
{$IFDEF DEBUG}
  Assert(Query <> nil);
  Assert(Query.Connection <> nil);
{$ENDIF}

  Result := 0;

  Query.CheckConnection;
  Query.Execute;

  LMsgList := Msgs.LockList;
  try
    while not Query.Eof do begin
      LMsg := TSBaseDataMessage.Create;
      try
        LMsg.Fill(Query);
      except
        LMsg.Free;
        raise;
      end;
      LMsgList.Add(LMsg);
      Inc(Result);

      Query.Next;
    end;
  finally
    Msgs.UnlockList;
  end;
  Query.Close;
end;

procedure TSQLServiceBrokerProvider.Release;
begin
//
end;

function TSQLServiceBrokerProvider.Receive: boolean;
var
  LMsgList: TList;

  procedure RemoveCurrentMessage;
  var
    LMsg: TSBaseDataMessage;
  begin
    LMsg := TSBaseDataMessage(LMsgList[0]);
    LMsg.Free;
    LMsgList.Delete(0);
  end;

var
  LTimeout: Integer;
  LQuery: TCustomInternalQuery;
begin
  LMsgList := FMsgs.LockList;
  try
    if (LMsgList.Count > 0) and FReceivePrev then
      RemoveCurrentMessage;

    if (not AsyncNotification) and (LMsgList.Count = 0) then begin
//      DoInit(lt.FQuery);
//      lt.FQuery.Params.Load(FInitParams);

      if WaitTimeout > 0 then
        LTimeout := WaitTimeout
      else
        LTimeout := MaxInt;

      LQuery := FConnectionProvider.GetSQLQuery;
      DoInit(LQuery, LTimeout);
      ReceiveFromServer(LQuery, FMsgs);
      FConnectionProvider.CloseSQLQuery;
    end;

    Result := LMsgList.Count > 0;
    FReceivePrev := Result;
  finally
    FMsgs.UnlockList;
  end;
end;

procedure TSQLServiceBrokerProvider.DoException(Sender: TObject; E: Exception; var Fail: boolean);
var
  LConnection: TInternalConnection;
begin
  if FStopProcessing then begin
    Fail := False;
    Exit;
  end;

  if not TInternalConnection.IsConnectionError(E) then
    Exit;

  if TInternalConnection.IsBreakExecError(E) then
    Fail := False
  else begin
    LConnection := FListener.FConnection;
{$IFDEF DEBUG}
    Assert(LConnection <> nil);
{$ENDIF}

    LConnection.DoDefaultError(E, Fail);
  end;

  // TODO: Вообще непонятно тут всё. Куда ошибку то девать? И что с ней делать?
  Fail := False;

  if Fail then
    Stop;
end;

procedure TSQLServiceBrokerProvider.DoInit(AQuery: TCustomInternalQuery; AWaitTimeout: Integer);
var
  LInitParams: TNamedVariants;
  LQueue: String;
  LIdx: Integer;
begin
//  AQuery.Close;
  LInitParams.Clear;
  if not FInitSQL.IsEmpty then begin
    AQuery.SQL.Text := FInitSQL;
    AQuery.Params.Load(FConnectionProvider.SessionVariables);
    AQuery.Execute;
    if not AQuery.IsEmpty then begin
      LInitParams.AssignValues(AQuery.Fields);
    end;
    AQuery.Close; AQuery.UnPrepare;
  end;

  if FReceiveSQL.IsEmpty then begin
    if FQueue.IsEmpty then begin
      Assert(LInitParams.Find(SConst_Queue, LIdx), SConst_Queue);
      LQueue := LInitParams.Items[LIdx].Value;
    end else
      LQueue := FQueue;
    AQuery.SQL.Text := FConnectionProvider.Connection.Interpreter.GenerateReceiveScript(LQueue, AWaitTimeout);
  end else begin
    if not FQueue.IsEmpty then
      LInitParams[SConst_Queue] := FQueue;
    if AWaitTimeout > 0 then
      LInitParams[SConst_WaitTimeout] := AWaitTimeout;

    AQuery.SQL.Text := FReceiveSQL;
    AQuery.Params.Load(LInitParams);
  end;
end;

procedure TSQLServiceBrokerProvider.DoStart;
begin
  Assert(FListener = nil);
  FListener := TQueryListenThreadWrapper.Create;
  Assert(FListener.FConnection <> nil);

  with FListener.FConnection do begin
    Assign(FConnectionProvider.Connection);
    LoginPrompt     := False;

    AfterConnect    := nil;
    AfterDisconnect := nil;
    OnError         := nil;
  end;

  DoInit(FListener.FQuery, WaitTimeout);

  FListener.FReleaseSQL := FReleaseSQL;
  FListener.FMsgs := FMsgs;

  FListener.OnPostEvent := DoThreadMessage;
  FListener.OnException := DoException;
  FListener.Resume;
end;

procedure TSQLServiceBrokerProvider.DoStop;
begin
  FListener.FQuery.BreakExec;
  FListener.Free;
  FListener := nil;
end;

procedure TSQLServiceBrokerProvider.DoThreadMessage(Sender: TObject; Event: Pointer);
var
  i: integer;
begin
  if Assigned(FOnMessage) then
    for i := 0 to Integer(Event) - 1 do
      FOnMessage(Self);
end;

procedure TSQLServiceBrokerProvider.Start;
begin
  if not FAsyncNotification then begin
    BeginConnection;

    DoStart;

    FAsyncNotification := True;
  end;
end;

procedure TSQLServiceBrokerProvider.Stop;
begin
  if FStopProcessing then
    Exit;

  if FAsyncNotification then begin
    FStopProcessing := True;
    try
      DoStop;
      FAsyncNotification := False;
    finally
      FStopProcessing := False;
      EndConnection;
    end;
  end;
end;

procedure TSQLServiceBrokerProvider.SetAsyncNotification(const AValue: boolean);
begin
  FStreamedAsyncNotification := AValue;
  if not (csReading in ComponentState) then begin
    if AValue <> FAsyncNotification then
      if AValue then
        Start
      else
        Stop;
  end;
end;

procedure TSQLServiceBrokerProvider.SetConnectionProvider(const AValue: TSQLConnectionProvider);
begin
  if FConnectionProvider <> AValue then begin
    if FConnectionProvider <> nil then
      TSQLConnectionProviderCrack(FConnectionProvider).RemoveSQLServiceBrokerProvider(Self);

    FConnectionProvider := AValue;

    if FConnectionProvider <> nil then
      TSQLConnectionProviderCrack(FConnectionProvider).AddSQLServiceBrokerProvider(Self);
    ConnectionChanged;
  end;
end;

end.
