{******************************************************************************}
{                         -= Система уведомлений =-                            }
{                               v2.1 (С) 2010                                  }
{                                                                              }
{******************************************************************************}

{$I CONFIG.INC}
{$DEFINE FASTSCRIPT_RTTI}

unit SBaseEvents;

interface

uses Types, Classes, Generics.Collections, NamedVariables, SBaseDataModule, SBaseUtils;

type
  TSBaseEventMessage = class
  private
//    FText   : String;
    FProps  : TNamedVariants;
    FParams : TNamedVariants;
  protected
    procedure LoadFromStream(AStream: TStream);
  public
    constructor Create; virtual;

//    property Text: String read FText write FText;
    property Params: TNamedVariants read FParams;
    property Props: TNamedVariants read FProps;
  end;


  TSBaseNotificationTrap = class;
  TBaseNotificationEvent = procedure (ASender: TSBaseNotificationTrap; AMessage: TSBaseEventMessage) of object;


  TCustomSBaseNotificationTrap = class(TComponent)
  strict private
    FEnabled: Boolean;

    function GetConnectionProvider: TSQLConnectionProvider;

    procedure SetEnabled(const Value: Boolean);
  protected
    procedure SetActive; dynamic;
    function GetIdentifiersEnumerator: TBaseEnumeratorFactory<TNamedVariantsCustomEnumerator>; dynamic; abstract;

    function  CheckIdentifier(AMessage: TSBaseEventMessage): Boolean; virtual;
    procedure DoNotification(AMessage: TSBaseEventMessage); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ConnectionProvider: TSQLConnectionProvider read GetConnectionProvider;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
  end;


  TSBaseNotificationTrap = class(TCustomSBaseNotificationTrap)
  strict private
    FIdentifiers: TNamedVariants;
  private
    function GetIdentifiers(const AName: String): Variant;
    procedure SetIdentifiers(const AName: String; const Value: Variant);
  protected
    FOnNotification: TBaseNotificationEvent;

    function GetIdentifiersEnumerator: TBaseEnumeratorFactory<TNamedVariantsCustomEnumerator>; override;
    procedure DoNotification(AMessage: TSBaseEventMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Identifiers[const AName: String]: Variant read GetIdentifiers write SetIdentifiers; default;
  published
    property Enabled;

//    property Identifiers: TNamedVariants read FIdentifiers write SetIdentifiers;

    property OnNotification: TBaseNotificationEvent read FOnNotification write FOnNotification;
  end;

  // Процедура для инициализации окна уведомлений
  procedure NotificationInit;

  // Процедура для деинициализации окна уведомлений, должно быть установленно
  // соединение с сервером и существовать главное окно приложения.
  // Нужно вызывать обязательно !!! Например на OnClose главной формы.
//  procedure NotificationFree;

implementation

uses
  SysUtils, Variants, Windows, Forms, SBaseForm, SBaseConstants,
  CLRClasses, SBaseXml, MainMDIForm, SBaseNotificationMessage,
  SBaseDataServiceBroker, SBaseVariantFunctions;

resourcestring

  // procedure names
  SConst_Proc_SessionBegin  = '[Notifications].[Session@Begin]';
  SConst_Proc_SessionEnd    = '[Notifications].[Session@End]';
  SConst_Proc_SessionInfo   = '[Notifications].[Session@Info]';

type
  TSBaseEvents = class(TComponent)
  private
    FEnabled: Boolean;
    FList: TList<TCustomSBaseNotificationTrap>;
    [Weak] FConnectionProvider: TSQLConnectionProvider;
    FServiceBrokerProvider: TSQLServiceBrokerProvider;

    function GetNotificationCount: Integer;
    function GetNotification(AIndex: Integer): TCustomSBaseNotificationTrap;
    function GetConnectionProvider: TSQLConnectionProvider;
    procedure SetConnectionProvider(const AValue: TSQLConnectionProvider);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure ServiceBrokerMessage(Sender: TObject);
    procedure ProcessMessage(AMessage: TSBaseEventMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InternalRefresh;

    property Items[Index: Integer]: TCustomSBaseNotificationTrap read GetNotification;
    property ItemCount: Integer read GetNotificationCount;
    property ConnectionProvider: TSQLConnectionProvider read GetConnectionProvider write SetConnectionProvider;
  end;

var
  BaseEvents: TSBaseEvents = nil;

procedure NotificationInit;
begin
  BaseEvents.ConnectionProvider := BaseSQLConnectionProvider;

  SBaseNotificationMessage.NotificationInit;
end;

{$REGION 'TSBaseEventMessage'}

constructor TSBaseEventMessage.Create;
begin
  FProps  := TNamedVariants.Create(True);
  FParams := TNamedVariants.Create(True);
end;

procedure TSBaseEventMessage.LoadFromStream(AStream: TStream);
var
  LReader: TXMLTextReader;
  LParams: TNamedVariants;

  procedure ReadNode(AAttributes: PNamedVariants; AText: PString);
  var
    I: Integer;
  begin
    if Assigned(AAttributes) and LReader.HasAttributes then
      for I := 0 to Pred(LReader.AttributeCount) do
        AAttributes^[LReader.Names[I]] := LReader.Items[I];

    if LReader.NodeType = xntElement then begin
      if Assigned(AText) then
        AText^ := '';
    end else begin
      if Assigned(AText) then
        AText^ := LReader.CurrElementText;

      while LReader.Read do begin
        if (LReader.NodeType = xntEndElement) then
          Break
        else if (LReader.NodeType = xntStartElement) then
          ReadNode(nil, nil);
      end;
    end;
  end;

  procedure ReadParamsNode(AParams: PNamedVariants);
  var
    LType   : String;
    SValue  : String;
    LValue  : Variant;
  begin
//    if LReader.NodeType = xntStartElement then
      while LReader.Read and (LReader.NodeType = xntElement) do begin
       // LParams.Clear;

        SValue := LReader.Values[SConst_Value];
        LReader.TryGetValue(SConst_Type, LType);
        if LType.IsEmpty then
          LValue := SValue
        else
          LValue := CastAs(SValue, LType);

        AParams^[LReader.Values[SConst_Name]] := LValue;
      end;
  end;

  function IsSameNode(const ANodeName: String): Boolean;
  begin
    Result := (LReader.NodeType in [xntElement, xntStartElement]) and SameText(LReader.Name, ANodeName);
  end;

begin
//  FText := '';

  FProps.Clear;
  FParams.Clear;
  LParams.UnknownAsNull := True;

  LReader := TXMLTextReader.Create(AStream);
  try
    if LReader.Read and IsSameNode(SConst_Message) then
      while LReader.Read do begin
        if LReader.NodeType in [xntElement, xntStartElement] then begin
          // Header
          if IsSameNode(SConst_Header) then
            ReadNode(@FProps, nil)
          // Params
          else if IsSameNode(SConst_Params) then
            ReadParamsNode(@FParams)
          // Unknown
          else
            ReadNode(@FProps, nil)
        end;
      end;
  finally
    LReader.Free;
//    LParams.Clear;
  end
end;


{$ENDREGION}


{$REGION 'TSBaseEvents'}

constructor TSBaseEvents.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := False;
  FList := TList<TCustomSBaseNotificationTrap>.Create;
end;

destructor TSBaseEvents.Destroy;
begin
  FreeAndNil(FServiceBrokerProvider);
//  FreeAndNil(FConnectionProvider);
  FreeAndNil(FList);

  inherited;
end;

procedure TSBaseEvents.InternalRefresh;
var
  I: Integer;
  LEnabled: Boolean;
begin
  if FServiceBrokerProvider = nil then
    Exit;
  
  LEnabled := False;
  for I := Pred(ItemCount) downto 0 do
  begin
    LEnabled := Items[0].Enabled;
    if LEnabled then Break;
  end;

  if LEnabled = FEnabled then exit;
  FEnabled := LEnabled;

  if FEnabled then begin
    with FServiceBrokerProvider do begin
      InitSQL     := SConst_Proc_SessionBegin;
      ReleaseSQL  := SConst_Proc_SessionEnd;

      OnMessage         := ServiceBrokerMessage;
      AsyncNotification := True;
    end;
  end else begin
    FServiceBrokerProvider.AsyncNotification := False;
  end;
end;

procedure TSBaseEvents.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then
    if AComponent = FConnectionProvider then
      ConnectionProvider := nil
end;

procedure TSBaseEvents.ProcessMessage(AMessage: TSBaseEventMessage);
var
  i: Integer;
begin
  for i := Pred(FList.Count) downto 0 do
//    if NotifyMyself or (FList[i] <> ASender) then
      with FList[i] do
        if Enabled and CheckIdentifier(AMessage) then
          DoNotification(AMessage);
end;

procedure TSBaseEvents.ServiceBrokerMessage(Sender: TObject);
var
  LStream: TStringStream;
  LMessage: TSBaseEventMessage;
begin
  while FServiceBrokerProvider.Receive do
    if FServiceBrokerProvider.CurrentMessage.MessageType <> bmtEndDialog then begin
      LMessage := TSBaseEventMessage.Create;
      try
        LStream := TStringStream.Create(Utf8Encode(FServiceBrokerProvider.CurrentMessage.XML)); // ???? TODO: ????
        try
          LMessage.LoadFromStream(LStream);
        finally
          LStream.Free;
        end;
        ProcessMessage(LMessage);
      finally
        LMessage.Free;
      end;
    end;
end;

function TSBaseEvents.GetNotificationCount: Integer;
begin
  Result := FList.Count;
end;

function TSBaseEvents.GetNotification(AIndex: Integer): TCustomSBaseNotificationTrap;
begin
  Result := FList.Items[AIndex];
end;

function TSBaseEvents.GetConnectionProvider: TSQLConnectionProvider;
begin
  Result := FConnectionProvider;
  Assert(Assigned(Result));
end;

procedure TSBaseEvents.SetConnectionProvider(const AValue: TSQLConnectionProvider);
begin
  if Assigned(FConnectionProvider) or (FConnectionProvider = AValue) then
    Exit;

  if FConnectionProvider <> nil then
    RemoveFreeNotification(FConnectionProvider);

  FreeAndNil(FServiceBrokerProvider);
  FConnectionProvider := AValue;

  if FConnectionProvider <> nil then begin
    FreeNotification(FConnectionProvider);
    FServiceBrokerProvider := TSQLServiceBrokerProvider.Create(Self);
    with FServiceBrokerProvider do begin
      ConnectionProvider  := FConnectionProvider;
      WaitTimeout         := 30 * 60 * 1000;
    end;
  end;
end;

{$ENDREGION}

{$REGION 'TCustomSBaseNotificationTrap'}

constructor TCustomSBaseNotificationTrap.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := False;
  BaseEvents.FList.Add(Self);
end;

destructor TCustomSBaseNotificationTrap.Destroy;
begin
  BaseEvents.FList.Remove(Self);
  BaseEvents.InternalRefresh;
  inherited;
end;

function TCustomSBaseNotificationTrap.GetConnectionProvider: TSQLConnectionProvider;
begin
  Result := BaseEvents.FConnectionProvider;
end;

function TCustomSBaseNotificationTrap.CheckIdentifier(AMessage: TSBaseEventMessage): Boolean;
var
  LItem: TNamedVariant;
begin
  for LItem in GetIdentifiersEnumerator do
    if not VarIsEqual(AMessage.Params[LItem.Name], LItem.Value) then
      Exit(False);

  Result := True;
end;

procedure TCustomSBaseNotificationTrap.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then
    Exit;

  FEnabled := Value;
  if FEnabled then begin
    BaseEvents.InternalRefresh;
    SetActive;
  end;
end;

procedure TCustomSBaseNotificationTrap.SetActive;
begin
end;

//procedure TCustomSBaseNotificationTrap.SetIdentifier(const Value: TNamedVariants);
//begin
//  FIdentifier.AssignValues(Value);
//end;

//function TCustomSBaseNotificationTrap.GetIdents(const AName: String): Variant;
//begin
//  Result := FIdentifier[AName];
//end;
//
//procedure TCustomSBaseNotificationTrap.SetIdents(const AName: String; const Value: Variant);
//begin
//  FIdentifier[AName] := Value;
//end;

{$ENDREGION}

{$REGION 'TSBaseNotificationTrap'}

constructor TSBaseNotificationTrap.Create(AOwner: TComponent);
begin
  FIdentifiers.UnknownAsNull := True;
  inherited;
end;

destructor TSBaseNotificationTrap.Destroy;
begin
  FIdentifiers.Clear;
  inherited;
end;

procedure TSBaseNotificationTrap.DoNotification(AMessage: TSBaseEventMessage);
begin
  inherited;

  if Assigned(FOnNotification) then
    FOnNotification(Self, AMessage);
end;
{$ENDREGION}

function TSBaseNotificationTrap.GetIdentifiers(const AName: String): Variant;
begin
  Result := FIdentifiers[AName]
end;

function TSBaseNotificationTrap.GetIdentifiersEnumerator: TBaseEnumeratorFactory<TNamedVariantsCustomEnumerator>;
begin
  Result := TBaseEnumeratorFactory<TNamedVariantsCustomEnumerator>.
              Create
              (
                TNamedVariantsEnumerator.Create(@FIdentifiers)
              )
end;

procedure TSBaseNotificationTrap.SetIdentifiers(const AName: String; const Value: Variant);
begin
  FIdentifiers[AName] := Value
end;

initialization
  Classes.RegisterClasses([TSBaseNotificationTrap]);
  BaseEvents := TSBaseEvents.Create(nil);

finalization
  FreeAndNil(BaseEvents);
  Classes.UnRegisterClasses([TSBaseNotificationTrap]);

end.
