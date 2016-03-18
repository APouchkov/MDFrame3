unit SBaseItemReactions;
{$I config.inc}

interface
uses
  Classes, DBCommon, Forms, SysUtils, Types, DB, dxBar, Controls, Variants, Math,
  SBaseDataSourceControls, NamedVariables, SBaseFormDefinitions, dxMDBarButton
{IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter
{ENDIF}
  ;

type
  TConfirmMessageFunction  = function(const AMessage: String): Boolean of object;
  TBooleanObjectRequest = function(ASender: TObject): Boolean of object;

  TSQLStrings = class(TStringList)
  private
    FThread: Boolean;
    FCached: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Cached: Boolean read FCached write FCached default False;
    property Thread: Boolean read FThread write FThread default False;
  end;

  TBaseItemReaction = class;

{$REGION 'ItemReactions - Definition'}
  TBaseItemReactionNotify = class(TPersistent)
  private
    FHasUserAction        : Boolean;

    FConfirmation         : String;

    FSQL                  : TStrings;
    FScriptParams         : String;
    FScriptConstants      : TNamedVariants;
    FScriptProcedure      : String;

    FSystemMethod         : TMethod;

    FClassName            : String;
    FClassParams          : String;
    FClassConstants       : TNamedVariants;

    procedure SetSQL(const AValue: TStrings);
    procedure SQLChange(Sender: TObject); dynamic;

    procedure AddScriptConstantItem(const AItem: String);
    procedure AddClassConstantItem(const AItem: String);

    procedure SetClassName(const AValue: String);
    procedure SetScriptProcedure(const AValue: String);
    procedure SetSystemMethod(const AValue: TMethod);

    procedure CheckUserAction;
    function  IsClassNameStored: Boolean;
//  protected
//    property SystemMethod: TMethod read FSystemMethod write SetSystemMethod;
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadClassConstants(Reader: TReader);
  public
    constructor Create; //reintroduce;
    destructor Destroy; override;

    { Признак того, что реакция пользователя задана }
    property HasUserAction: Boolean read FHasUserAction;

    property ClassConstants : TNamedVariants read FClassConstants write FClassConstants;
  published
  {
    Приоритет:
      1. SQL.Text
      2. Script
      3. Notify
      4. ClassName
  }
    { Запрос подтверждения операции }
    property Confirmation: String read FConfirmation write FConfirmation;

    { SQL - код, который будет выполняться }
    property SQL: TStrings read FSQL write SetSQL;

    { Процедура / Функция FastScript из общего кода формы - будет выполняться при Click. Параметры подставляются автоматически. }
    property ScriptProcedure : String read FScriptProcedure write SetScriptProcedure;
    { Удобный интерфейс добавления одной константы в Массив Constants в текстовом представлении }
    property ScriptConstant  : String write AddScriptConstantItem;
    { Константы, которые будут переданы параметрами в ScriptProcedure }
    property ScriptConstants : TNamedVariants read FScriptConstants write FScriptConstants;
    { Параметры, которые будут переданы параметрами в ScriptProcedure }
    property ScriptParams    : String read FScriptParams write FScriptParams;

{$WARNINGS OFF}
    { Имя класса, который будет создан. Может возвращаться при выполнении SQL.Text'а }
    property ClassName: String read FClassName write SetClassName stored IsClassNameStored;
{$WARNINGS ON}
    { Удобный интерфейс добавления одной константы в Массив Constants в текстовом представлении }
    property ClassConstant  : String write AddClassConstantItem;
    { Параметры, которые будут переданны из владельца в экземпляр ClassName }
    property ClassParams    : String read FClassParams write FClassParams;
  end;


{$REGION 'TBaseItemReactionKind - Definition'}
  TBaseItemReactionKind =
  (
    biraNone,

//    biraUser,                           // Реакция, определённая пользователем
//    biraUserForm,                       // Реакция, определённая пользователем
//                                        // + Способна собирать данные с формы

    biraCustom,                           // Пользовательская реакция
    biraCustomForm,                       // Пользовательская реакция
                                          // + Способна собирать данные с формы

    biraSubMenu,                          // Рут-элемент с меню, ни от чего не зависящий. При OnPopup срабатывает ItemReaction.

    biraDataSourceActiveSubMenu,          // Рут-элемент с меню, обновляемый в зависимости от Активности набора данных
    biraDataSourceVolumeSubMenu,          // Рут-элемент с меню, обновляемый при изменении наличия набора данных
    biraDataSourceSubMenu,                // Рут-элемент с меню, обновляемый при изменении содержимого набора данных.
    biraDataSourceRecordSubMenu,          // Рут-элемент с меню, обновляемый в зависимости от Фокуса текущей записи

    biraDataSourceActiveAndRecordSubMenu, // Рут-элемент с меню, обновляемый при изменении Активности или Фокуса набора данных.

    biraDataSourceInPlaceInsertSubMenu,
    biraDataSourceInPlaceInsertChildSubMenu,
    biraDataSourceInPlaceInsertComboSubMenu,
    biraDataSourceInPlaceInsertDataSubMenu,


    biraCustomDataSourceActive,           // Пользовательская обработка активного грида
                                          // + Способна собирать данные с параметров источника, с формы

    biraCustomDataSourceVolume,           // Пользовательская обработка данных не пустого грида
                                          // + Способна собирать данные с параметров источника, с формы

    biraCustomDataSource,                 // Пользовательская обработка данных(!!!) активного грида
                                          // + Способна собирать данные с параметров источника, с формы

    biraCustomDataSourceRecord,           // Пользовательская обработка текущей записи
                                          // + Способна собирать данные с полей и параметров источника, с формы

    // biraCustomDataSourceActiveAndRecord,  // Пользовательская обработка Активности или Фокуса набора данных
                                          // + Способна собирать данные с параметров источника, с формы

    biraDataSourceRefresh,                // Обновление ДатаСета методом DataSet.Refresh
    biraDataSourceRefreshRecord,          // Обновление записи методом DataSet.RefreshRecord

    biraDataSourceExpand,                 //
    biraDataSourceCollapse,               //

    biraDataSourceInPlaceInsert,          // Добавить запись с редактированием "в гриде"
                                          // + Способна собирать данные с параметров источника, с формы

    biraDataSourceInPlaceInsertData,      // Добавить запись с редактированием "в гриде"
                                          // + Реагирует на изменение содержимого

    biraDataSourceInPlaceCopy,            // Копирует текущую запись в новую с редктированием "в гриде"
                                          // + Способна собирать данные с полей и параметров источника, с формы

    biraDataSourceInPlaceInsertChild,     // Для деревьев - вставляет чилд к текущей записи с редактированием "в гриде"
                                          // + Способна собирать данные с полей и параметров источника, с формы

    biraDataSourceInPlaceEdit,            // Редактирование "в гриде"
    biraDataSourceInPlacePost,            // Постит запись "в грид"
    biraDataSourceInPlaceDelete           // Удаляет текущую запись с ДатаСета
  );
{$ENDREGION 'TBaseItemReactionKind - Definition'}

//  TOnClickProcedure = procedure(Sender: TObject; var ADone: Boolean) of object;
  TBaseItemReactionOnClickProcedure = procedure(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean) of object;

  TBaseItemReactionOnClick = class(TBaseItemReactionNotify)
  strict private
    FItemReaction     : TBaseItemReaction;
  private
    FReaction         : TBaseItemReactionKind;

    FTarget           : TComponent;
    FInPlaceConstants : TNamedVariants;
    FInPlaceParams    : String;

    FClassParentOwner : Boolean;

    procedure SetReaction(AValue: TBaseItemReactionKind);

    function  GetSystemProcedure: TBaseItemReactionOnClickProcedure; inline;
    procedure SetSystemProcedure(const AValue: TBaseItemReactionOnClickProcedure);

    procedure AddInPlaceConstantItem(const AItem: String);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AItemReaction: TBaseItemReaction); overload;

    procedure AddClassConstant(const AName: String; const AValue: Variant);

    { Признак того, что реакция зависит от связанного набора }
    function IsDataSourceReaction: Boolean;
  published
    { Стандартная реакция нажатия на кнопку }
    property Reaction: TBaseItemReactionKind read FReaction write SetReaction default biraNone;
    { Иногда целью является иной компонент, тогда СТАНДАРТНАЯ реакция будет производиться над ним }
    property Target: TComponent read FTarget write FTarget;

    { Локальная реакция Click'а }
    property SystemProcedure: TBaseItemReactionOnClickProcedure read GetSystemProcedure write SetSystemProcedure;

{$WARNINGS OFF}
    { Имя класса, который будет создан при Click. Может возвращаться при выполнении SQL.Text'а }
    property ClassName;
{$WARNINGS ON}
    { Владельцем нового компонента является владелец элемента или сам элемент, вызвавший реакцию }
    property ClassParentOwner : Boolean read FClassParentOwner write FClassParentOwner Default False;

    { Константы, которые будут переданы параметрами в ScriptProcedure }
    property InPlaceConstants : TNamedVariants read FInPlaceConstants write FInPlaceConstants;
    { Удобный интерфейс добавления одной константы в Массив Constants в текстовом представлении }
    property InPlaceConstant  : String write AddInPlaceConstantItem;
    { Параметры, которые будут переданы параметрами в ScriptProcedure }
    property InPlaceParams    : String read FInPlaceParams write FInPlaceParams;
  end;

  TBaseItemReactonOnSelectNotify = class(TBaseItemReactionNotify)
  private
    FGetVarValuesFunction : TGetVarValuesFunction;

    function GetNotify: TCallBackSelectFunction; inline;
    procedure SetNotify(const AValue: TCallBackSelectFunction);
  published
    property Notify: TCallBackSelectFunction read GetNotify write SetNotify;
  end;

  TBaseItemReactionPropertiesOnGetEnabled = class(TPersistent)
  strict private
    FForm: TForm;
  private
    FCondition        : String;
    FConditionParser  : TExprParser;

    FScriptConstants  : TNamedVariants;
    FSystemFunction   : TBooleanObjectRequest;

    FPriority         : Word;
    FScriptFunction   : String;
    FScriptProcVar    : TfsProcVariable;

    FActive           : Boolean;
//    FSynonym          : TComponent;

//    FOnChange         : TNotifyEvent;
    FChanged          : Boolean;

    procedure AddScriptConstantItem(const AItem: String);
    procedure SetScriptFunction(const Value: String);
    procedure SetCondition(const Value: String);
    procedure SetActive(const Value: Boolean);
//    procedure SetSynonym(const Value: TComponent);
    procedure SetPriority(const Value: Word);
    procedure SetSystemFunction(const Value: TBooleanObjectRequest);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoChanged;
  public
    constructor Create(AForm: TForm); reintroduce;
    destructor Destroy; override;

    function ScriptProcVar: TfsProcVariable;

    procedure AddConstant(const AName: String; const AValue: Variant);

    property ConditionParser: TExprParser     read FConditionParser;
    property ScriptConstants: TNamedVariants  read FScriptConstants;

//    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Active : Boolean     read FActive  write SetActive Default True;
//    property Synonym: TComponent  read FSynonym write SetSynonym;

    // 1.
    property SystemFunction : TBooleanObjectRequest read FSystemFunction write SetSystemFunction;
    // 2.
    property Condition: String read FCondition write SetCondition;
    // 3.
    property ScriptConstant : String write AddScriptConstantItem;
    property ScriptFunction : String read FScriptFunction write SetScriptFunction;

    { Priority for depended items }
    property Priority: Word read FPriority write SetPriority Default 0;
  end;

  TBaseItemReaction = class(TCollectionItem)
  private
    FItem           : TObject;
    FDefault        : Boolean;
    FLinkedComponent: TComponent;

    FOnClick        : TBaseItemReactionOnClick;
    FOnGetEnabled   : TBaseItemReactionPropertiesOnGetEnabled;
    FSelect         : TBaseItemReactonOnSelectNotify;

    FDataSourceControlProperties: TDataSourceControlProperties;

    procedure SetItem(const AValue: TObject);

    function  GetEnabled: Boolean;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetLinkedComponent(const AValue: TComponent);

    procedure SelectProc(ASelectAction: TCallBackSelectAction; const AResultValues: TNamedVariants);
    procedure SetOnClick(const Value: TBaseItemReactionOnClick);
    procedure SetOnGetEnabled(const Value: TBaseItemReactionPropertiesOnGetEnabled);
    procedure SetSelect(const Value: TBaseItemReactonOnSelectNotify);
    procedure InternalInitOnClickEvent;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function  GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    function GetForm: TForm; inline;
    function GetOnClickEvent: TNotifyEvent;
    procedure SetOnClickEvent(AValue: TNotifyEvent);
    procedure SetOnPopupEvent(AValue: TNotifyEvent);

    procedure ItemClick(Sender: TObject);
    function Execute(AGetVarValuesFunction: TGetVarValuesFunction; AConfirmMessageFunction: TConfirmMessageFunction): Integer;
    function SubItemCount: Integer;
    function HasEnabledSubItem: Boolean;
    function SubItemEnabled(Index: Integer): Boolean;

    property OnClickEvent: TNotifyEvent read GetOnClickEvent;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property DataSourceControlProperties: TDataSourceControlProperties read FDataSourceControlProperties;
  published
    { Элемент }
    property Item: TObject read FItem write SetItem;
    { Действие по-умолчанию }
    property Default: Boolean read FDefault write FDefault Default False;

    { Условия доступности }
    property OnGetEnabled: TBaseItemReactionPropertiesOnGetEnabled read FOnGetEnabled write SetOnGetEnabled;

    { Действия при Click }
    property OnClick: TBaseItemReactionOnClick read FOnClick write SetOnClick;

    { Действие после получения значения из справочника }
    property OnSelect: TBaseItemReactonOnSelectNotify read FSelect write SetSelect;

    { Связанный компонент }
    property LinkedComponent: TComponent read FLinkedComponent write SetLinkedComponent;
  end;

  TBaseItemReactons = class(TCollection)
  private
    function GetItem(Index: Integer): TBaseItemReaction;
  protected
    FOwnerForm: TForm;
    FDataSourceControls: TDataSourceControls;
  public
    constructor Create(AOwner: TForm); reintroduce;
    function  Add: TBaseItemReaction;
    function  FindItemProperties(AItem: TObject): TBaseItemReaction;
    procedure DeleteItemProperties(AItem: TObject);

    procedure SetActive(AItem: TComponent; AActive: Boolean);
    procedure InitReaction(AItem: TComponent; ALinkedComponent: TComponent; ADefaultReaction: TBaseItemReactionKind);
    function  FindOrCreateReaction(const AItem: TObject; ADefaultItemReaction: TBaseItemReactionKind = biraNone; ADefaultOnClickSystemProcedure: TBaseItemReactionOnClickProcedure = nil): TBaseItemReaction;


    { Универсальный обработчик события обновления зависимых контролов ItemReactions при обновлении источника данных }
    procedure DataSourceOnUpdate(const ADataSource: TComponent; AUpdateStates: TDataSourceUpdateStates);

    property Items[Index: Integer]: TBaseItemReaction read GetItem; default;
  end;
{$ENDREGION 'ItemReactions - Definition'}

{$REGION 'TfsFunctions_SBaseItemReactions - Definition'}
{$IFDEF FASTSCRIPT_RTTI}
  TfsFunctions_SBaseItemReactions = class(TfsRTTIModule)
  private
    function Call_TBaseItemsReactons_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}
{$ENDREGION 'TfsFunctions_SBaseItemReactions - Definition'}

  function IfThen(ATrueFalse: Boolean; ATrue: TBaseItemReactionKind; AFalse: TBaseItemReactionKind): TBaseItemReactionKind; overload;

implementation

uses
  TypInfo,
  SBaseForm, SBaseConstants, SBaseDfmView, SBaseStringFunctions,
  SBaseDataModule, SBaseDataChanges, SBaseVariantFunctions, SBaseUtils;

type
  TControlCrack = class(TControl);

const
  MError_UnkOrUndefLinkedComponent: String = 'Unknown or undefined ItemReaction (Item = %s) LinkedComponent';
  SMessage_InvalidItemReactionCall: String = 'Invalid ItemReaction OnClick method call';

function IfThen(ATrueFalse: Boolean; ATrue: TBaseItemReactionKind; AFalse: TBaseItemReactionKind): TBaseItemReactionKind;
begin
  if ATrueFalse then
    Result := ATrue
  else
    Result := AFalse
end;

{$REGION 'TBaseItemReactionPropertiesOnGetEnabled'}
{ TBaseItemReactionPropertiesOnGetEnabled }

procedure TBaseItemReactionPropertiesOnGetEnabled.AddConstant(const AName: String; const AValue: Variant);
begin
  FScriptConstants[AName] := AValue;
end;

procedure TBaseItemReactionPropertiesOnGetEnabled.AddScriptConstantItem(const AItem: String);
begin
  with TNamedVariant.Create(AItem) do
    FScriptConstants[Name] := Value;
  DoChanged
end;

procedure TBaseItemReactionPropertiesOnGetEnabled.AssignTo(Dest: TPersistent);
begin
  if Dest is TBaseItemReactionPropertiesOnGetEnabled then begin
    TBaseItemReactionPropertiesOnGetEnabled(Dest).Active          := Active;
//    TBaseItemReactionPropertiesOnGetEnabled(Dest).Synonym         := Synonym;
    TBaseItemReactionPropertiesOnGetEnabled(Dest).Condition       := Condition;
    TBaseItemReactionPropertiesOnGetEnabled(Dest).FScriptConstants:= ScriptConstants;
    TBaseItemReactionPropertiesOnGetEnabled(Dest).ScriptFunction  := ScriptFunction;
    TBaseItemReactionPropertiesOnGetEnabled(Dest).SystemFunction  := SystemFunction;
    TBaseItemReactionPropertiesOnGetEnabled(Dest).Priority        := Priority;
  end;
end;

constructor TBaseItemReactionPropertiesOnGetEnabled.Create(AForm: TForm);
begin
  Assert(Assigned(AForm) and (AForm is TSBaseFrm));
  FActive := True;
  FForm   := AForm;

  inherited Create;
end;

destructor TBaseItemReactionPropertiesOnGetEnabled.Destroy;
begin
  FreeAndNil(FConditionParser);
  inherited;
end;

procedure TBaseItemReactionPropertiesOnGetEnabled.DoChanged;
begin
  FChanged := True;
//  if Assigned(FOnChange) then
//    FOnChange(Self);
end;

function TBaseItemReactionPropertiesOnGetEnabled.ScriptProcVar: TfsProcVariable;
begin
  if FScriptFunction.IsEmpty then
    Result := nil
  else begin
    if not Assigned(FScriptProcVar) then
      FScriptProcVar := TSBaseFrm(FForm).PrepareFunction(FScriptFunction, True);
    Result := FScriptProcVar;
  end;
end;

procedure TBaseItemReactionPropertiesOnGetEnabled.SetActive(const Value: Boolean);
begin
  if FActive <> Value then begin
    FActive := Value;
    DoChanged;
  end;
end;

procedure TBaseItemReactionPropertiesOnGetEnabled.SetCondition(const Value: String);
begin
  if FCondition <> Value then begin
    FCondition := Value;
    FreeAndNil(FConditionParser);
    DoChanged
  end;
end;

procedure TBaseItemReactionPropertiesOnGetEnabled.SetPriority(const Value: Word);
begin
  if FPriority <> Value then begin
    FPriority := Value;
    DoChanged
  end;
end;

procedure TBaseItemReactionPropertiesOnGetEnabled.SetScriptFunction(const Value: String);
begin
  if FScriptFunction <> Value then begin
    FScriptFunction := Value;
    FScriptProcVar  := nil;
    DoChanged
  end;
end;

//procedure TBaseItemReactionPropertiesOnGetEnabled.SetSynonym(const Value: TComponent);
//begin
//  if FSynonym <> Value then begin
//    FSynonym := Value;
//    DoChanged
//  end;
//end;

procedure TBaseItemReactionPropertiesOnGetEnabled.SetSystemFunction(const Value: TBooleanObjectRequest);
begin
  if TMethod(FSystemFunction) <> TMethod(Value) then begin
    FSystemFunction := Value;
    DoChanged
  end;
end;

{$ENDREGION 'TBaseItemReactionPropertiesOnGetEnabled'}

{$REGION 'TBaseItemReaction'}
{ TBaseItemReaction }

procedure TBaseItemReactionOnClick.AddInPlaceConstantItem(const AItem: String);
begin
  with TNamedVariant.Create(AItem) do
    FInPlaceConstants[Name] := Value;
end;

procedure TBaseItemReactionOnClick.AssignTo(Dest: TPersistent);
begin
  Assert(Dest is TBaseItemReactionOnClick);

  with TBaseItemReactionOnClick(Dest) do begin
    FReaction        := Self.FReaction;
    FTarget          := Self.FTarget;

    FClassParentOwner:= Self.FClassParentOwner;
    if (FReaction <> biraNone) and Assigned(FItemReaction) then
      FItemReaction.InternalInitOnClickEvent;
  end;

  inherited;
end;

constructor TBaseItemReactionOnClick.Create(AItemReaction: TBaseItemReaction);
begin
  FItemReaction := AItemReaction;

  inherited Create;
end;

function TBaseItemReactionOnClick.GetSystemProcedure: TBaseItemReactionOnClickProcedure;
begin
  Result := TBaseItemReactionOnClickProcedure(FSystemMethod);
end;

function TBaseItemReactionOnClick.IsDataSourceReaction: Boolean;
begin
  Result := FReaction in
    [
      biraDataSourceActiveSubMenu,
      biraDataSourceVolumeSubMenu,
      biraDataSourceSubMenu,
      biraDataSourceRecordSubMenu,
      biraDataSourceActiveAndRecordSubMenu,
      biraDataSourceInPlaceInsertSubMenu,
      biraDataSourceInPlaceInsertChildSubMenu,
      biraDataSourceInPlaceInsertComboSubMenu,

      biraDataSourceRefresh,
      biraCustomDataSourceActive,
      biraCustomDataSourceVolume,
      biraCustomDataSourceRecord,
      //biraCustomDataSourceActiveAndRecord,

      biraDataSourceRefreshRecord,
      biraDataSourceExpand,
      biraDataSourceCollapse,
      biraDataSourceInPlaceInsert,
      biraDataSourceInPlaceCopy,
      biraDataSourceInPlaceInsertChild,
      biraDataSourceInPlaceEdit,
      biraDataSourceInPlacePost,
      biraDataSourceInPlaceDelete
    ]
end;

procedure TBaseItemReactionOnClick.SetReaction(AValue: TBaseItemReactionKind);
begin
  if FReaction <> AValue then begin
    FReaction := AValue;
    if (FReaction <> biraNone) and Assigned(FItemReaction) then
      FItemReaction.InternalInitOnClickEvent;
  end;
end;

procedure TBaseItemReactionOnClick.SetSystemProcedure(const AValue: TBaseItemReactionOnClickProcedure);
begin
  SetSystemMethod(TMethod(AValue));
end;

procedure TBaseItemReactionOnClick.AddClassConstant(const AName: String; const AValue: Variant);
begin
  FClassConstants[AName] := AValue;
end;

procedure TBaseItemReaction.SetItem(const AValue: TObject);
var
  I: Integer;
begin
  if FItem <> AValue then begin
    if Assigned(FItem) and (FItem is TComponent) then
      TComponent(FItem).RemoveFreeNotification(GetForm);

    FItem := AValue;

    if Assigned(FItem) and (FItem is TComponent) then
      TComponent(FItem).FreeNotification(GetForm);

    for I := Pred(Collection.Count) downto 0 do
      if (Index <> I) and (TBaseItemReaction(Collection.Items[I]).Item = AValue) then begin
        Collection.Delete(I);
        Break;
      end;

    if Assigned(FItem) then begin
      if (OnClick.Reaction <> biraNone) then
        InternalInitOnClickEvent;
    end;
  end;
end;

function TBaseItemReaction.GetOnClickEvent: TNotifyEvent;
begin
  if (Item is TdxBarItem) then
    Result := TdxBarItem(Item).OnClick
  else if (Item is TControl) then
    Result := TControlCrack(Item).OnClick
end;

function TBaseItemReaction.HasEnabledSubItem: Boolean;
var
  I: Integer;
begin
  for I := SubItemCount - 1 downto 0 do
    if SubItemEnabled(I) then
      Exit(True);

  Result := False;
end;

procedure TBaseItemReaction.InternalInitOnClickEvent;
begin
  case OnClick.Reaction of
    biraNone, biraSubMenu,
    biraDataSourceActiveSubMenu, biraDataSourceVolumeSubMenu, biraDataSourceSubMenu, biraDataSourceRecordSubMenu,
    biraDataSourceActiveAndRecordSubMenu,
    biraDataSourceInPlaceInsertSubMenu, biraDataSourceInPlaceInsertChildSubMenu, biraDataSourceInPlaceInsertComboSubMenu, biraDataSourceInPlaceInsertDataSubMenu
    :
      SetOnClickEvent(nil);
    else
      SetOnClickEvent(ItemClick);
  end;

  case OnClick.Reaction of
    biraNone, biraSubMenu,
    biraDataSourceActiveSubMenu, biraDataSourceVolumeSubMenu, biraDataSourceSubMenu, biraDataSourceRecordSubMenu,
    biraDataSourceActiveAndRecordSubMenu,
    biraDataSourceInPlaceInsertSubMenu, biraDataSourceInPlaceInsertChildSubMenu, biraDataSourceInPlaceInsertComboSubMenu, biraDataSourceInPlaceInsertDataSubMenu
    :
      SetOnPopupEvent(ItemClick)
    else
      SetOnPopupEvent(nil);
  end;

  if not (OnClick.Reaction in [biraNone, biraCustom, biraCustomForm, biraSubMenu]) then
    SetEnabled(False);
end;

procedure TBaseItemReaction.ItemClick(Sender: TObject);
var
  LGetVarValuesFunction: TGetVarValuesFunction;
begin
  case OnClick.Reaction of
    biraCustomForm,
    biraSubMenu:
      LGetVarValuesFunction := TSBaseFrm(GetForm).GetFormParamValues;

    biraDataSourceActiveSubMenu, biraDataSourceInPlaceInsertSubMenu,
    biraDataSourceVolumeSubMenu,
    biraDataSourceSubMenu, biraDataSourceInPlaceInsertDataSubMenu,
    biraDataSourceActiveAndRecordSubMenu, biraDataSourceInPlaceInsertComboSubMenu,
        biraCustomDataSourceActive, biraCustomDataSourceVolume, biraCustomDataSource,
        biraDataSourceInPlaceInsert, biraDataSourceInPlaceInsertData, biraDataSourceRefresh:
      begin
        if not Assigned(FDataSourceControlProperties) then
          Raise Exception.CreateFmt(MError_UnkOrUndefLinkedComponent, [TComponent(Sender).GetDisplayName]);

        LGetVarValuesFunction := FDataSourceControlProperties.GetFormDataSourceParamValues;
      end;

    biraDataSourceRecordSubMenu, biraDataSourceInPlaceInsertChildSubMenu,
        biraCustomDataSourceRecord,
        biraDataSourceInPlaceCopy, biraDataSourceInPlaceInsertChild, biraDataSourceInPlaceEdit, biraDataSourceInPlacePost, biraDataSourceInPlaceDelete,
        biraDataSourceRefreshRecord:
      begin
        if not Assigned(FDataSourceControlProperties) then
          Raise Exception.CreateFmt(MError_UnkOrUndefLinkedComponent, [TComponent(Sender).GetDisplayName]);

        LGetVarValuesFunction := FDataSourceControlProperties.GetFormDataSourceParamAndFieldValues;
      end;

    biraCustom,
    biraDataSourceExpand,
    biraDataSourceCollapse
    :
      LGetVarValuesFunction := nil;
    else
    {
      biraNone, biraCustom
    }
      Raise Exception.Create(SMessage_InvalidItemReactionCall);
  end;

  Execute(LGetVarValuesFunction, TSBaseFrm.ConfirmMessage)
end;

function TBaseItemReaction.GetForm: TForm;
begin
  Result := TBaseItemReactons(Collection).FOwnerForm
end;

function TBaseItemReaction.SubItemCount: Integer;
begin
  if (Item is TdxBarSubItem) then begin
    if TdxBarSubItem(Item).ItemLinks = nil then
      Result := 0
    else
      Result := TdxBarSubItem(Item).ItemLinks.VisibleItemCount
  end else if (Item is TdxBarButton) and (TdxBarButton(Item).ButtonStyle = bsDropDown) then
    Result := TdxBarButton(Item).DropDownMenu.ItemLinks.VisibleItemCount
  else
    Result := 0
end;

function TBaseItemReaction.SubItemEnabled(Index: Integer): Boolean;
begin
  if (Item is TdxBarSubItem) then
    Result := TdxBarSubItem(Item).ItemLinks.VisibleItems[Index].Item.Enabled
  else if (Item is TdxBarButton) and (TdxBarButton(Item).ButtonStyle = bsDropDown) then
    Result := TdxBarButton(Item).DropDownMenu.ItemLinks.VisibleItems[Index].Item.Enabled
  else
    Result := False
end;

function TBaseItemReaction.GetDisplayName: string;
begin
  if Assigned(FItem) then
    if (FItem is TComponent) then begin
      Result := TComponent(FItem).Name;
      if not Result.IsEmpty then Exit;
      Result := VarToStr(GetPropValue(FItem, SConst_Caption, True, True));
      if not Result.IsEmpty then Exit;
    end;

  Result := inherited;
end;

function TBaseItemReaction.GetEnabled: Boolean;
begin
  if (Item is TdxBarItem) then begin
    Result := TdxBarItem(Item).Enabled;
  end else if (Item is TControl) then begin
    Result := TControlCrack(Item).Enabled;
  end else
    Result := False;
end;

procedure TBaseItemReaction.SetEnabled(const AValue: Boolean);
begin
  if (Item is TdxBarItem) then begin
    TdxBarItem(Item).Enabled := AValue;
  end else if (Item is TControl) then begin
    TControlCrack(Item).Enabled := AValue;
  end;
end;

procedure TBaseItemReaction.SelectProc(ASelectAction: TCallBackSelectAction; const AResultValues: TNamedVariants);
var
  LVarResult: TNamedVariants;
  LVarParams: TNamedVariants;

  I: Integer;
  Idx: Integer;
  LDFM: String;
  LComponent: TComponent;
  LParams, LParamName, LParamAlias: String;
  LParamItem: TStringItem;
  LfsFunction: TfsProcVariable;
begin
  LVarResult := AResultValues;
  LVarResult.UnknownAsNull := False;

  if (FSelect.SQL.Count > 0) then begin
    LVarParams := TSBaseFrm(GetForm).ExecSQL(FSelect.SQL.Text, LVarResult, FSelect.FGetVarValuesFunction, FItem, TSQLStrings(FSelect.SQL).Thread);

    LVarParams.UnknownAsNull := True;
    LVarParams.ClearUndefined;

    if LVarParams.Find(SConst_DFM, Idx) then begin
      LDFM := LVarParams.Items[Idx].Value;
      LVarParams.Delete(Idx);
      TSBaseFrm(GetForm).LoadDFM(LDFM);
    end;
    LVarResult.AssignValues(LVarParams, amOverride);
  end;

  if (not FSelect.ScriptProcedure.IsEmpty) then begin
    LfsFunction := TSBaseFrm(GetForm).PrepareFunction(FSelect.ScriptProcedure);
    LParams     := FSelect.ScriptParams;
    if LParams.IsEmpty then
      with LfsFunction do begin
        for I := 0 to ParamCount - 1 do
          LParams := ConcatIfNotEmpty(LParams, ';', Params[I].Name);
      end;

    if (not LParams.IsEmpty) then begin
      LVarParams := TSBaseFrm(GetForm).PrepareParameters(LParams, LVarResult, FSelect.FGetVarValuesFunction, FItem);
      LVarParams.ClearUndefined;
    end else
      LVarParams.Clear;

    LVarParams.AssignValues(FSelect.ScriptConstants, amRaiseIfRepeated);

    TSBaseFrm(GetForm).CallFunctionOut(FSelect.ScriptProcedure, LVarParams); // TODO: Список констант
    if (not LVarParams.IsEmpty) then begin
      if (not FSelect.ScriptParams.IsEmpty) then
        for LParamItem in EnumStringItems(FSelect.ScriptParams, ';') do begin
          ExtractNameAndAlias(LParamItem.Value, @LParamName, @LParamAlias);
          if LParamName <> LParamAlias then
            LVarParams.ReName(LParamName, LParamAlias);
        end;
      LVarResult.AssignValues(LVarParams, amOverride);
    end;
  end;

  if (FSelect.FSystemMethod.Code <> nil) then
    FSelect.Notify(ASelectAction, LVarResult)
  else if (not FSelect.ClassName.IsEmpty) then begin
    LVarParams := TSBaseFrm(GetForm).PrepareParameters(FSelect.ClassParams, LVarResult, FSelect.FGetVarValuesFunction, FItem);
    LVarParams.AssignValues(FSelect.ClassConstants, amRaiseIfRepeated);

    LComponent := SBaseFormDefinitions.ICreateByParams(FSelect.ClassName, TBaseItemReactons(Collection).FOwnerForm, LVarParams);
    if Assigned(LComponent) then
      (LComponent as ISBaseInterface).IShow;
  end;
end;

procedure TBaseItemReaction.AssignTo(Dest: TPersistent);
begin
  inherited;

  if Dest is TBaseItemReaction then with TBaseItemReaction(Dest) do begin
    Default         := Self.Default;
    OnGetEnabled    := Self.OnGetEnabled;
    OnClick         := Self.OnClick;
    OnSelect        := Self.OnSelect;
    LinkedComponent := Self.LinkedComponent;
    FItem           := Self.FItem; // Только последним !!!!
  end;
end;

constructor TBaseItemReaction.Create(Collection: TCollection);
begin
//  FBaseItemReaction := biraNone;

  inherited Create(Collection);

  FOnClick          := TBaseItemReactionOnClick.Create(Self);
  FOnGetEnabled     := TBaseItemReactionPropertiesOnGetEnabled.Create(GetForm);
  FSelect           := TBaseItemReactonOnSelectNotify.Create;
end;

destructor TBaseItemReaction.Destroy;
begin
  FreeAndNil(FOnClick);
  FreeAndNil(FOnGetEnabled);
  FreeAndNil(FSelect);

  inherited;
end;

function TBaseItemReaction.Execute(AGetVarValuesFunction: TGetVarValuesFunction; AConfirmMessageFunction: TConfirmMessageFunction): Integer;
var
  J, LIdx: Integer;
  LDone: Boolean;

  LVarParams    : TNamedVariants;
  LVarCollection: TNamedVariants;

  LClassName  : String;
  LClassParams: String;

  LParams, LParamName, LParamAlias: String;
  LParamItem: TStringItem;
  LValue: Variant;

  LTargetDataSourceControlProperties: TDataSourceControlProperties;
  LDataSet: TDataSet;
  LDataSetProvider: TSQLDataSetProvider absolute LDataSet;

  LComponent: TComponent;
  LfsFunction: TfsProcVariable;
begin
  Result := 0;
//  if Self = nil then Exit;

  with OnClick do
    if (not Confirmation.IsEmpty) and (not AConfirmMessageFunction(Confirmation)) then
      Abort;

  try
    { 1. Если задан SQL.Text то сначала обработаем его }
    if (OnClick.SQL.Count > 0) then begin
      LVarCollection := TSBaseFrm(GetForm).ExecSQL(OnClick.SQL.Text, TNamedVariants.Create(False), AGetVarValuesFunction, FItem, TSQLStrings(OnClick.SQL).Thread);
      LVarCollection.UnknownAsNull := False;

      if LVarCollection.Find(SConst_NotificationTableChangesUpdatedObjects, J) then begin
        LValue := LVarCollection.Items[J].Value;
        LVarCollection.Delete(J);
        if VarIsStr(LValue) then
          SBaseMainFormMDIInterface.BroadCastDataChanges
          (
            TTablesChanges.Create(TNamedVariants.Create(VarToStr(LValue), True))
          );
      end;

      if LVarCollection.Find(SConst_Confirmation, J) then begin
        LValue := LVarCollection.Items[J].Value;
        LVarCollection.Delete(J);

        if VarIsStr(LValue) then
          if not AConfirmMessageFunction(LValue) then Abort;
      end;

      if LVarCollection.Find(SConst_DFM, J) then begin
        LValue := LVarCollection.Items[J].Value;
        LVarCollection.Delete(J);
        GetForm.LoadDFM(LValue);
      end;

      if LVarCollection.Find(SConst_Return_Value, J) then begin
        Result := LVarCollection.Items[J].Value;
        LVarCollection.Delete(J);
      end;

      //TODO: А почему здесь?
//      if (LVarCollection.Count > 0) and (OnClick.Reaction in [biraDataSourceInPlaceInsert, biraDataSourceInPlaceCopy]) then
//        FDataSourceControlProperties.SourceDataSet.Fields.Load(LVarCollection, lvcIfReceive);
    end else
      LVarCollection := TNamedVariants.Create(False);

    { 2. Если задан Script то обработаем и его }
    if (not OnClick.ScriptProcedure.IsEmpty) then begin
      LfsFunction := TSBaseFrm(GetForm).PrepareFunction(OnClick.ScriptProcedure);

      LParams     := OnClick.ScriptParams;
      // Если нету OnClick.ScriptParams, то соберём сами из параметров ф-ии
      if LParams.IsEmpty then
        with LfsFunction do begin
          for J := 0 to ParamCount - 1 do with Params[J] do
            if not OnClick.ScriptConstants.Find(Name, LIdx) then
              LParams := ConcatIfNotEmpty(LParams, ';', Name);
        end;

      // Получим значения параметров, если они есть
      if (not LParams.IsEmpty) then begin
        LVarParams := TSBaseFrm(GetForm).PrepareParameters(LParams, LVarCollection, AGetVarValuesFunction, FItem);
        LVarParams.ClearUndefined;
      end else
        LVarParams := TNamedVariants.Create(True);

      // Добавим константы
      LVarParams.AssignValues(OnClick.ScriptConstants, amRaiseIfRepeated);

      // Вызовем функцию
      LValue := TSBaseFrm(GetForm).CallFunctionOut(LfsFunction, LVarParams); // На выходе ТОЛЬКО Var-параметры
      if VarIsOrdinal(LValue) then
        Result := LValue;

      // Если вернулись параметры
      if (not LVarParams.IsEmpty) then begin
        // Если они были запрошены, то попробуем переименовать их, если надо
        if (not OnClick.ScriptParams.IsEmpty) then
          for LParamItem in EnumStringItems(OnClick.ScriptParams, ';') do begin
            ExtractNameAndAlias(LParamItem.Value, @LParamName, @LParamAlias);
            if LParamName <> LParamAlias then
              LVarParams.ReName(LParamName, LParamAlias);
          end;

        // Confirmation
        if LVarParams.Find(SConst_Confirmation, J) then begin
          LValue := LVarParams.Items[J].Value;
          LVarParams.Delete(J);

          if VarIsStr(LValue) then
            if not AConfirmMessageFunction(LValue) then Abort;
        end;

        // Добавим в коллекцию
        LVarCollection.AssignValues(LVarParams, amOverride);
      end;
    end;

    LClassName   := OnClick.ClassName;
    LClassParams := OnClick.ClassParams;

    // Соберём параметры для Class-вызова
    if (not LClassParams.IsEmpty) then begin
      LVarParams := TSBaseFrm(GetForm).PrepareParameters(LClassParams, LVarCollection, AGetVarValuesFunction, FItem);
      LVarParams.ClearUndefined;

      // Вернулся ClassName?
      if (LClassName.IsEmpty) and LVarParams.Find(SConst_ClassName, J) then begin
        LClassName := LVarParams.Items[J].Value;
        LVarParams.Delete(J);
      end;
    end else begin
      { Если не указан ClassName, но он был собран предыдущими этапами, то извлечём его }
      if (LClassName.IsEmpty) and LVarCollection.Find(SConst_ClassName, J) then begin
        LClassName := LVarCollection.Items[J].Value;
        LVarCollection.Delete(J);
      end;
      LVarParams := LVarCollection;
    end;

    { Нужно для работы Select'а }
    if (not Assigned(OnClick.SystemProcedure))
                      and (LClassName.IsEmpty)
                      and (OnClick.Reaction = biraNone)
    then
      Exit(-1);

    { 3. Если есть ClassName }
    if (not LClassName.IsEmpty) then begin
      LVarParams.AssignValues(OnClick.ClassConstants, amRaiseIfRepeated);

      { Отображение экземпляра ClassName }
      if FSelect.HasUserAction then begin
        FSelect.FGetVarValuesFunction := AGetVarValuesFunction;
        LVarParams := SBaseFormDefinitions.ISelectByParams
                      (
                        LClassName,
                        TComponent(IfThen(OnClick.ClassParentOwner, Integer(GetForm.Owner), Integer(GetForm))),
                        LVarParams,
                        SelectProc
                      );
        // Добавим в коллекцию
        LVarCollection.AssignValues(LVarParams, amOverride);
      end else begin
        LComponent := SBaseFormDefinitions.ICreateByParams
                      (
                        LClassName,
                        TComponent(IfThen(OnClick.ClassParentOwner, Integer(GetForm.Owner), Integer(GetForm))),
                        LVarParams
                      );
        if Assigned(LComponent) then
          (LComponent as ISBaseInterface).IShow;
      end;
      { // }

    end else if Assigned(OnSelect.Notify) then begin
      OnSelect.Notify(cbsaSingleRecord, OnClick.ClassConstants); // TODO: А чё только OnClick.ClassConstants?
    end;


    { 4. Если есть OnClickProcedure }
    if Assigned(OnClick.SystemProcedure) then begin
      LDone := False;
      OnClick.SystemProcedure(Self, LVarCollection, LDone);
      if LDone then
        Exit;
    end;

    case OnClick.Reaction of
      biraDataSourceRefresh:
        FDataSourceControlProperties.Refresh;
      biraDataSourceRefreshRecord:
        FDataSourceControlProperties.RefreshRecord;
      biraDataSourceInPlaceEdit:
        FDataSourceControlProperties.InPlaceEdit;
      biraDataSourceInPlacePost:
        FDataSourceControlProperties.Post;
      biraDataSourceInPlaceDelete:
        FDataSourceControlProperties.Delete;

      biraDataSourceExpand:
        FDataSourceControlProperties.FullExpand;
      biraDataSourceCollapse:
        FDataSourceControlProperties.FullCollapse;

    { ------------ InPlaceInsert ------------ }
    biraDataSourceInPlaceInsert, biraDataSourceInPlaceInsertData:
      with FDataSourceControlProperties do begin
        LDataSet := SourceDataSet;
        if LDataSet.Editing then LDataSet.Post;

        LDataSet.DisableControls; //TODO: Раньше убирал из-за чего-то... Не перепродумывалось в дереве...
        try
          LVarParams := TSBaseFrm(GetForm).PrepareParameters
                        (
                          OnClick.InPlaceParams,
                          LVarCollection,
                          GetFormDataSourceParamValues,
                          FItem
                        );

          InPlaceInsert;
          LDataSetProvider.LoadFields(OnClick.ClassConstants, lvcIfReceive);

          if (not OnClick.ClassParams.IsEmpty) then
            LDataSet.Fields.Load(LVarParams, lvcIfReceive);
        finally
          LDataSet.EnableControls;
        end;
      end;

    { ------------ InPlaceInsertChild ------------ }
    biraDataSourceInPlaceInsertChild:
      begin
        if Assigned(OnClick.Target) then
          LTargetDataSourceControlProperties := TBaseItemReactons(Collection).FDataSourceControls.FindOrCreateGridControl(OnClick.Target)
        else
          LTargetDataSourceControlProperties := FDataSourceControlProperties;

        LDataSet := LTargetDataSourceControlProperties.SourceDataSet;
        if LDataSet.Editing then LDataSet.Post;

        LDataSet.DisableControls;
        try
          if (not OnClick.InPlaceParams.IsEmpty) then
            LVarParams := TSBaseFrm(GetForm).PrepareParameters
                          (
                            OnClick.InPlaceParams,
                            LVarCollection,
                            FDataSourceControlProperties.GetFormDataSourceParamAndFieldValues,
                            FItem
                          );

            //LValue := FDataSourceControlProperties.GetFormDataSourceParamAndFieldValues(OnClick.InPlaceParams, LUnknownParams);

          FDataSourceControlProperties.InPlaceInsertChild(OnClick.Target);
          LDataSetProvider.LoadFields(OnClick.InPlaceConstants + LVarParams, lvcIfReceive);
//          LDataSet.Fields.Load(LVarParams, lvcIfReceive);

          //if (not OnClick.InPlaceParams.IsEmpty) then
          //  LDataSet.Fields.SetValues(OnClick.InPlaceParams, LValue);

        finally
          LDataSet.EnableControls;
        end;
      end;

    { ------------ InPlaceCopy ------------ }
    biraDataSourceInPlaceCopy:
      with FDataSourceControlProperties do begin
        LVarParams := TSBaseFrm(GetForm).PrepareParameters
                      (
                        OnClick.InPlaceParams,
                        LVarCollection,
                        FDataSourceControlProperties.GetFormDataSourceParamAndFieldValues,
                        FItem
                      );

        //LValue := GetFormDataSourceParamAndFieldValues(OnClick.InPlaceParams, LUnknownParams);

        LDataSet := SourceDataSet;
        LDataSet.DisableControls;
        try
          InPlaceInsert;

          LDataSetProvider.LoadFields(OnClick.InPlaceConstants + LVarParams, lvcIfReceive);
          //LDataSet.Fields.Load(LVarParams, lvcIfReceive);
          //LDataSet.Fields.SetValues(OnClick.InPlaceParams, LValue);
        finally
          LDataSet.EnableControls;
        end;
      end;
    end;
  except
    if (OnClick.Reaction in [biraDataSourceInPlaceInsert, biraDataSourceInPlaceCopy])
          and FDataSourceControlProperties.Editing
    then
      FDataSourceControlProperties.Cancel;
    Raise;
  end;
end;

procedure TBaseItemReaction.SetLinkedComponent(const AValue: TComponent);
begin
  if FLinkedComponent <> AValue then begin
    FLinkedComponent := AValue;

    if Assigned(FLinkedComponent) then
      FDataSourceControlProperties := TBaseItemReactons(Collection).FDataSourceControls.FindOrCreateGridControl(FLinkedComponent);
  end;
end;

procedure TBaseItemReaction.SetOnClick(const Value: TBaseItemReactionOnClick);
begin
  FOnClick.Assign(Value);
end;

procedure TBaseItemReaction.SetOnClickEvent(AValue: TNotifyEvent);
begin
  if (Item is TdxBarItem) then
    TdxBarItem(Item).OnClick := AValue
  else if (Item is TControl) then
    TControlCrack(Item).OnClick := AValue
end;

procedure TBaseItemReaction.SetOnPopupEvent(AValue: TNotifyEvent);
begin
  if (Item is TdxBarSubItem) then
    TdxBarSubItem(Item).OnPopup := AValue
  else if (Item is TdxBarPopupMenu) then
    TdxBarPopupMenu(Item).OnPopup := AValue
  else if (Item is TdxMDBarButton) then
    TdxMDBarButton(Item).OnPopup := AValue
end;

procedure TBaseItemReaction.SetOnGetEnabled(const Value: TBaseItemReactionPropertiesOnGetEnabled);
begin
  FOnGetEnabled.Assign(Value);
end;

procedure TBaseItemReaction.SetSelect(const Value: TBaseItemReactonOnSelectNotify);
begin
  FSelect.Assign(Value);
end;
{$ENDREGION 'TBaseItemReaction'}

{$REGION 'TBaseItemReactons'}
{ TBaseItemReactons }

constructor TBaseItemReactons.Create(AOwner: TForm);
begin
  Assert(Assigned(AOwner) and (AOwner is TSBaseFrm));

  FOwnerForm := AOwner;
  FDataSourceControls := TSBaseFrm(AOwner).DataSourceControls;
  inherited Create(TBaseItemReaction);
end;

function TBaseItemReactons.Add: TBaseItemReaction;
begin
  Result := TBaseItemReaction(inherited Add);
end;

function TBaseItemReactons.GetItem(Index: Integer): TBaseItemReaction;
begin
  Result := TBaseItemReaction(inherited GetItem(Index));
end;

procedure TBaseItemReactons.SetActive(AItem: TComponent; AActive: Boolean);
var
  LItemReaction: TBaseItemReaction;
begin
  LItemReaction := FindItemProperties(AItem);
  if Assigned(LItemReaction) then
    LItemReaction.OnGetEnabled.Active := AActive;
end;

procedure TBaseItemReactons.InitReaction(AItem: TComponent; ALinkedComponent: TComponent; ADefaultReaction: TBaseItemReactionKind);
var
  LItemReaction: TBaseItemReaction;
begin
  LItemReaction := FindItemProperties(AItem);
  if Assigned(LItemReaction) then begin
    LItemReaction.LinkedComponent := ALinkedComponent;
    if (LItemReaction.OnClick.Reaction = biraNone) then
      LItemReaction.OnClick.Reaction := ADefaultReaction;
  end;
end;

function TBaseItemReactons.FindItemProperties(AItem: TObject): TBaseItemReaction;
var
  I: Integer;
begin
  Result := nil;
  for I := Pred(Count) downto 0 do
    if Items[I].FItem = AItem then begin
      Result := Items[I];
      Exit;
    end;
end;

function TBaseItemReactons.FindOrCreateReaction(const AItem: TObject; ADefaultItemReaction: TBaseItemReactionKind; ADefaultOnClickSystemProcedure: TBaseItemReactionOnClickProcedure): TBaseItemReaction;
begin
  Result := FindItemProperties(AItem);
  if Result = nil then begin
    Result      := TBaseItemReaction.Create(Self);
    with Result do begin
      Item := AItem;
      with OnClick do begin
        Reaction := ADefaultItemReaction;
        OnClick.SystemProcedure := ADefaultOnClickSystemProcedure;
      end;
    end;
  end;
end;

procedure TBaseItemReactons.DataSourceOnUpdate(const ADataSource: TComponent; AUpdateStates: TDataSourceUpdateStates);
var
  LOnGetEnabledParams: TNamedVariants;

  procedure FillOnGetEnabledParams(ASender: TObject; AOnGetEnabled: TBaseItemReactionPropertiesOnGetEnabled);
  begin
    LOnGetEnabledParams := AOnGetEnabled.ScriptConstants;
    LOnGetEnabledParams[SConst_Sender] := Integer(ASender);
  end;

var
  LCollectionItem: TCollectionItem;
  LItemReaction: TBaseItemReaction absolute LCollectionItem;
  LActive, LEnabled, LNotEmpty, LCanModify, LFocusedRecordIsData: Boolean;
  LCurrentPriority, LNextPriority: Word;

  LGridControl: TDataSourceControlProperties;
  LDataSet: TDataSet;
  LDataSetProvider: TSQLDataSetProvider absolute LDataSet;
begin
  LGridControl := FDataSourceControls.FindGridControl(ADataSource);
  if (not Assigned(LGridControl)) then Exit;

  LActive             := LGridControl.IsDataActive;

  if LActive then
    LNotEmpty := LGridControl.IsNotEmpty
  else
    LNotEmpty := False;
  LFocusedRecordIsData := LNotEmpty and LGridControl.FocusedRecodIsData;

  LCanModify := LActive and LGridControl.CanModify;
  if LCanModify then begin
    LDataSet := LGridControl.SourceDataSet.Source;
    if not (LDataSet is TSQLDataSetProvider) then
      LDataSet := nil;
  end else
    LDataSet := nil;

  if LGridControl.UpdateIsNotEmpty(LNotEmpty) then
    AUpdateStates := AUpdateStates + [dsusVolume];

  if dsusActive in AUpdateStates then
    AUpdateStates := AUpdateStates + [dsusData, dsusFocusedRecord]
  else if dsusData in AUpdateStates then
    AUpdateStates := AUpdateStates + [dsusFocusedRecord];

  LCurrentPriority := 1; // Фиктивное значение чтобы цикл стартовал
  LNextPriority    := 0; // Фактическое значение начала цикла
  while LNextPriority <> LCurrentPriority do begin
    LCurrentPriority := LNextPriority;
    for LCollectionItem in Self do with LItemReaction do
      if (LinkedComponent = ADataSource) then
        if (OnGetEnabled.Priority > LCurrentPriority) then begin
          if (LNextPriority = LCurrentPriority) or (OnGetEnabled.Priority < LNextPriority) then
            LNextPriority := OnGetEnabled.Priority;
        end else if OnGetEnabled.Active and (OnGetEnabled.Priority = LCurrentPriority) then begin
          case OnClick.Reaction of
            // 1. Всякая Активная хрень
            biraDataSourceActiveSubMenu, biraDataSourceInPlaceInsertSubMenu,
            biraCustomDataSourceActive, biraDataSourceInPlaceInsert, biraDataSourceRefresh:
              begin
                if (dsusActive in AUpdateStates) then begin
                  LEnabled := LActive
                              and (not (OnClick.Reaction in [biraDataSourceInPlaceInsert, biraDataSourceInPlaceInsertSubMenu]) or LCanModify)
                              and
                              (
                                not Assigned(OnGetEnabled.SystemFunction)
                                or OnGetEnabled.SystemFunction(Item)
                              );

                  if LEnabled and (OnGetEnabled.ScriptProcVar <> nil) then begin
                    FillOnGetEnabledParams(Item, OnGetEnabled);
                    LEnabled := (TSBaseFrm(FOwnerForm).CallFunction(OnGetEnabled.ScriptProcVar, LOnGetEnabledParams) = True);
                  end;

                  if LEnabled
                        and (OnClick.Reaction in [biraDataSourceActiveSubMenu, biraDataSourceInPlaceInsertSubMenu])
                        and (not OnClick.HasUserAction)
                  then
                    LEnabled := HasEnabledSubItem;

                  Enabled := LEnabled;
                end;
              end;

            // 2. Всякая содержательная хрень
            biraDataSourceVolumeSubMenu,
            biraCustomDataSourceVolume, biraDataSourceExpand, biraDataSourceCollapse:
              begin
                if (dsusVolume in AUpdateStates) or OnGetEnabled.FChanged then begin
                  LEnabled := LNotEmpty
                              and
                              (
                                not Assigned(OnGetEnabled.SystemFunction)
                                or OnGetEnabled.SystemFunction(Item)
                              );

                  if LEnabled and (OnGetEnabled.ScriptProcVar <> nil) then begin
                    FillOnGetEnabledParams(Item, OnGetEnabled);
                    LEnabled := (TSBaseFrm(FOwnerForm).CallFunction(OnGetEnabled.ScriptProcVar, LOnGetEnabledParams) = True);
                  end;

                  if LEnabled then
                    case OnClick.Reaction of
                      biraDataSourceVolumeSubMenu:
                        if (not OnClick.HasUserAction) then
                          LEnabled := HasEnabledSubItem;
                      biraDataSourceExpand, biraDataSourceCollapse:
                        LEnabled := LGridControl.FoldingActive;
                    end;

                  Enabled := LEnabled;
                  OnGetEnabled.FChanged := False;
                end;
              end;

            // 3. От данных
            biraDataSourceSubMenu,
            biraCustomDataSource, biraDataSourceInPlaceInsertData:
              begin
                if (dsusData in AUpdateStates) or OnGetEnabled.FChanged then begin
                  LEnabled := LActive
                              and (not LGridControl.Editing)
                              and
                              (
                                not Assigned(OnGetEnabled.SystemFunction)
                                or OnGetEnabled.SystemFunction(Item)
                              );

                  if LEnabled and (OnGetEnabled.ScriptProcVar <> nil) then begin
                    FillOnGetEnabledParams(Item, OnGetEnabled);
                    LEnabled := (TSBaseFrm(FOwnerForm).CallFunction(OnGetEnabled.ScriptProcVar, LOnGetEnabledParams) = True);
                  end;

                  if LEnabled
                          and (OnClick.Reaction = biraDataSourceSubMenu)
                          and (not OnClick.HasUserAction)
                  then
                    LEnabled := HasEnabledSubItem;

                  Enabled := LEnabled;
                  OnGetEnabled.FChanged := False;
                end;
              end;

            // 4. Хрень, зависимая от текущей записи (фокуса)
            biraDataSourceRecordSubMenu, biraDataSourceInPlaceInsertChildSubMenu,
            biraCustomDataSourceRecord,
            biraDataSourceRefreshRecord,
            biraDataSourceInPlaceEdit, biraDataSourceInPlaceCopy, biraDataSourceInPlaceDelete,
            biraDataSourceInPlaceInsertChild:
              begin
                if (dsusFocusedRecord in AUpdateStates) then begin
                  // 1 - All record
                  LEnabled := LNotEmpty and LFocusedRecordIsData;
                  if LEnabled then begin
                    // 2 - InPlace
                    if OnClick.Reaction in
                          [
                            biraDataSourceInPlaceInsertChild, biraDataSourceInPlaceInsertChildSubMenu,
                            biraDataSourceInPlaceEdit,
                            biraDataSourceInPlaceCopy,
                            biraDataSourceInPlaceDelete
                          ] then
                    begin
                      // 2.1 - Not Editing and not usDeleted
                      LEnabled := LCanModify
                                    and (LDataSetProvider <> nil)
                                    and (not LDataSetProvider.Editing)
                                    and (LDataSetProvider.UpdateStatus <> usDeleted);
                      // 2.2 - Insert, Edit, Copy
                      if LEnabled then begin
                        case OnClick.Reaction of
                          biraDataSourceInPlaceEdit:
                            begin
                              with LDataSetProvider.FieldsAutomations do
                                LEnabled :=
                                            (not UpdateAbleFields.IsEmpty) or (not InitializeAbleFields.IsEmpty)
                                            or
                                            (
                                              (not ModifiAbleFieldsFieldName.IsEmpty)
                                              and
                                              // TODO: Может когда-нибудь это и не сработает... Типа возможен InPlace без SyncMode...
                                              (LDataSetProvider.FieldByName(ModifiAbleFieldsFieldName).AsString <> '')
                                            );
                            end;
                          biraDataSourceInPlaceInsert, biraDataSourceInPlaceInsertChild, biraDataSourceInPlaceInsertChildSubMenu, biraDataSourceInPlaceCopy:
                            begin
                              with LDataSetProvider.FieldsAutomations do
                                LEnabled := (not InsertAbleFields.IsEmpty) or (not ModifiAbleFieldsFieldName.IsEmpty);
                            end;
                        end;
                      end;
                    end;

                    // 3 - All
                    if LEnabled then
                      LEnabled := LGridControl.EvaluateBoolean(OnGetEnabled.Condition, @OnGetEnabled.ConditionParser, True);

                    // 4 - All
                    if LEnabled and Assigned(OnGetEnabled.SystemFunction) then
                      LEnabled := OnGetEnabled.SystemFunction(Item);

                    // 5 - All
                    if LEnabled and (OnGetEnabled.ScriptProcVar <> nil) then begin
                      FillOnGetEnabledParams(Item, OnGetEnabled);
                      LEnabled := (TSBaseFrm(FOwnerForm).CallFunction(OnGetEnabled.ScriptProcVar, LOnGetEnabledParams) = True);
                    end;

                    // 6 - SubMenu
                    if LEnabled
                            and (OnClick.Reaction in [biraDataSourceRecordSubMenu, biraDataSourceInPlaceInsertChildSubMenu])
                            and (not OnClick.HasUserAction)
                    then
                      LEnabled := HasEnabledSubItem;
                  end;

                  Enabled := LEnabled;
                end;
              end;
            // 4.1
            biraDataSourceInPlacePost:
              if (dsusFocusedRecord in AUpdateStates) then
                Enabled := LGridControl.Editing;

            // 5. От активных или сфокусированных данных
            biraDataSourceActiveAndRecordSubMenu, biraDataSourceInPlaceInsertComboSubMenu
            //biraCustomDataSourceActiveAndRecord
            :
              begin
                if (dsusFocusedRecord in AUpdateStates) or OnGetEnabled.FChanged then begin
                  LEnabled := LActive
                              and
                              (
                                not Assigned(OnGetEnabled.SystemFunction)
                                or OnGetEnabled.SystemFunction(Item)
                              );

                  if LEnabled and (OnGetEnabled.ScriptProcVar <> nil) then begin
                    FillOnGetEnabledParams(Item, OnGetEnabled);
                    LEnabled := (TSBaseFrm(FOwnerForm).CallFunction(OnGetEnabled.ScriptProcVar, LOnGetEnabledParams) = True);
                  end;

                  if LEnabled
                          and (OnClick.Reaction = biraDataSourceActiveAndRecordSubMenu)
                          and (not OnClick.HasUserAction)
                  then
                    LEnabled := HasEnabledSubItem;

                  Enabled := LEnabled;
                  OnGetEnabled.FChanged := False;
                end;
              end;
          end;
        end;
  end;
end;

procedure TBaseItemReactons.DeleteItemProperties(AItem: TObject);
var
  I: Integer;
begin
  for I := Pred(Count) downto 0 do
    if Items[I].FItem = AItem then begin
      Delete(I);
      Exit;
    end;
end;

{$ENDREGION 'TBaseItemReactons'}

{$REGION 'TBaseItemReactonOnSelectNotify'}
{ TBaseItemReactonOnSelectNotify }

function TBaseItemReactonOnSelectNotify.GetNotify: TCallBackSelectFunction;
begin
  Result := TCallBackSelectFunction(FSystemMethod);
end;

procedure TBaseItemReactonOnSelectNotify.SetNotify(const AValue: TCallBackSelectFunction);
begin
  if TMethod(FSystemMethod) <> TMethod(AValue) then begin
    FSystemMethod := TMethod(AValue);
    CheckUserAction;
  end;
end;
{$ENDREGION 'TBaseItemReactonOnSelectNotify'}

{$REGION 'TBaseItemReactionNotify'}
{ TBaseItemReactionNotify }

procedure TBaseItemReactionNotify.AddScriptConstantItem(const AItem: String);
begin
  with TNamedVariant.Create(AItem) do
    FScriptConstants[Name] := Value;
end;

procedure TBaseItemReactionNotify.AddClassConstantItem(const AItem: String);
begin
  with TNamedVariant.Create(AItem) do
    FClassConstants[Name] := Value;
end;

procedure TBaseItemReactionNotify.AssignTo(Dest: TPersistent);
begin
  Assert(Dest is TBaseItemReactionNotify);

  with TBaseItemReactionNotify(Dest) do begin

    FConfirmation    := Self.FConfirmation;
    FSQL.Assign(Self.SQL);

    ScriptProcedure := Self.ScriptProcedure;
    ScriptConstants := Self.ScriptConstants;
    ScriptParams    := Self.ScriptParams;

    FSystemMethod   := Self.FSystemMethod;

    ClassName       := Self.ClassName;
    ClassConstants  := Self.ClassConstants;
    ClassParams     := Self.ClassParams;
  end;

  CheckUserAction;
end;

procedure TBaseItemReactionNotify.CheckUserAction;
begin
  FHasUserAction := not
                    (
                          (FSQL.Count = 0)
                      and FScriptProcedure.IsEmpty
                      and not Assigned(FSystemMethod.Code)
                      and FClassName.IsEmpty
                    )
end;

constructor TBaseItemReactionNotify.Create;
begin
  FSQL := TSQLStrings.Create;
  TSQLStrings(FSQL).OnChange := SQLChange;
  inherited;
end;

procedure TBaseItemReactionNotify.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty(SConst_ClassConstants, ReadClassConstants, nil, False);
end;

destructor TBaseItemReactionNotify.Destroy;
begin
  inherited;
  FSQL.Free;
end;

function TBaseItemReactionNotify.IsClassNameStored: Boolean;
begin
  Result := (not FClassName.IsEmpty);
end;

procedure TBaseItemReactionNotify.ReadClassConstants(Reader: TReader);
begin
  FClassConstants.AsClrTParams := Reader.ReadString
end;

procedure TBaseItemReactionNotify.SetClassName(const AValue: String);
begin
  if FClassName <> AValue then begin
    FClassName := AValue;
    CheckUserAction;
  end;
end;

procedure TBaseItemReactionNotify.SetScriptProcedure(const AValue: String);
begin
  if FScriptProcedure <> AValue then begin
    FScriptProcedure := AValue;
    CheckUserAction;
  end;
end;

procedure TBaseItemReactionNotify.SetSQL(const AValue: TStrings);
begin
  FSQL.Assign(AValue);
end;

procedure TBaseItemReactionNotify.SetSystemMethod(const AValue: TMethod);
begin
  if FSystemMethod <> AValue then begin
    FSystemMethod := AValue;
    CheckUserAction;
  end;
end;

procedure TBaseItemReactionNotify.SQLChange(Sender: TObject);
begin
  CheckUserAction;
end;

{$ENDREGION 'TBaseItemReactionNotify'}

{$REGION 'TfsFunctions_SBaseItemReactions'}
{$IFDEF FASTSCRIPT_RTTI}
constructor TfsFunctions_SBaseItemReactions.Create(AScript: TfsScript);
begin
  inherited;

  AScript.AddEnum('TDataSourceUpdateStates', 'dsusActive, dsusData, dsusFocusedRecord');
  with AScript.AddClass(TBaseItemReactons, TBaseItemReactons.ClassParent.ClassName) do begin
    AddMethod('procedure DataSourceOnUpdate(const ADataSource: TComponent; AUpdateStates: TDataSourceUpdateStates)', Call_TBaseItemsReactons_Method);
  end;
end;

function TfsFunctions_SBaseItemReactions.Call_TBaseItemsReactons_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  LUpdateStates: TDataSourceUpdateStates;
begin
  if MethodName = 'DATASOURCEONUPDATE' then begin
    VarToSet(Caller.Params[1], LUpdateStates, SizeOf(TDataSourceUpdateStates));
    TBaseItemReactons(Instance).DataSourceOnUpdate(TComponent(VarToPointer(Caller.Params[0])), LUpdateStates);
  end;
end;
{$ENDIF}
{$ENDREGION 'TfsFunctions_SBaseItemReactions'}

{ TSQLStrings }

procedure TSQLStrings.AssignTo(Dest: TPersistent);
begin
  if Dest is TSQLStrings then begin
    TSQLStrings(Dest).Cached := Cached;
    TSQLStrings(Dest).Thread := Thread;
  end;

  if Dest is TStrings then
    TStrings(Dest).AssignStringsIfDifferent(Self);
end;

initialization
  RegisterClasses([TBaseItemReactionNotify, TBaseItemReaction, TBaseItemReactons]);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_SBaseItemReactions);
{$ENDIF}

finalization
  UnRegisterClasses([TBaseItemReactionNotify, TBaseItemReaction, TBaseItemReactons]);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TfsFunctions_SBaseItemReactions);
{$ENDIF}

end.
