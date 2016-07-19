unit DBPeriodicTabControl;
{$I config.inc}

interface

uses
  SysUtils, Classes, Controls, cxControls, cxPC, DB, Variants, Types, StrUtils, dxBar
{$IFNDEF PACKAGE}
  , SBaseStringFunctions
  , SBaseVariantFunctions
  , SBaseForm
  , NamedVariables
  , SBaseSelectDateTimeForm
  , SBaseDataModule
{$ENDIF}
  , SBasePeriodicUtils
  , DataHandler
  , Dialogs
  , FieldDataLink

{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter, fs_ievents
  , fsFunction_TDataHandler
{$ENDIF}
  ;

const
  SMessageOriginalRecordNotFoundError : String = 'Редактируемая запись не найдена';
  SMessageKeyCannotBeChanged          : String = 'Ключевое поле записи не может быть изменено';
  SMessageKeyNotPresentError          : String = 'Сохранение невозможно, т.к. ключевое поле записи не может быть пустым';

  SConst_PlusCaption      : String = '+  ';

type
  TDBPeriodicTabControl = class;

  TDBPeriodicTabControlNotifyAllowEvent = procedure (Sender: TDBPeriodicTabControl; var AValid: Boolean) of object;

  TDBPeriodicTabControlProperties = class(TcxTabControlProperties)
  private
    function GetControl: TDBPeriodicTabControl;
  protected
    function DoCanChange(NewTabIndex: Integer): Boolean; override;
    property Control: TDBPeriodicTabControl read GetControl;
  end;

  TPeriodicDefaultDate        = (pddNone, pddEmptyPage, pddEmptyDataPage);
  TPeriodicFilling            = (pfRandom, pfEveryDate, pfFromBirth);
  TPeriodicKeyChangeAction    = (pkcaError, pkcaMoveRecord, pkcaMarkOldValueAsDeleted);

  TDBPeriodicTabControlPeriodic = class(TPersistent)
  private
    [weak] FOwner: TDBPeriodicTabControl;
    FDataLink: TDataSourceDataLink;
    FAgeFieldDataLink: TMDFieldDataLink;

    FDateFieldName      : String;

    FKeyFieldNames      : String;
    FUniqueFieldNames   : String;
    FNonUniqueDataFieldNames: String;
    FIdentityFieldName  : String;

    FActiveRecordSign   : TPeriodicActiveRecordSign;
    FActiveFieldName    : String;

    FStaticFieldNames   : String;
    FPrivateFieldNames  : String;

    FDefaultDate        : TPeriodicDefaultDate;
    FKeyChangeAction    : TPeriodicKeyChangeAction;
    FFilling            : TPeriodicFilling;
    FPostAllFields      : Boolean;

    FFuture             : Boolean;

    FExternalFilter     : String;

    FModifiAbleFieldsFieldName  : String;
    FDeletablePrivilegeFieldName: String;

    procedure SetDateFieldName(const Value: String);
    procedure SetKeyFieldNames(const Value: String);
    procedure SetStaticFieldNames(const Value: String);
    function  GetDataHandler: TSubDataHandler;
    procedure SetDataHandler(const Value: TSubDataHandler);
    procedure SetExternalFilter(const Value: String);

    //-- Проверка вводимых пропертей
    procedure CheckProperties;
    procedure SetUniqueFieldNames(const Value: String);
    procedure SetIdentityFieldName(const Value: String);

{$IFNDEF PACKAGE}
    procedure DataSetActiveChanged(ASender: TObject);
    procedure AgeFieldChanged(ASender: TObject);
{$ENDIF}
    function GetAgeFieldName: String;
    procedure SetAgeFieldName(const Value: String);
    procedure SetPrivateFieldNames(const Value: String);
  public
{$IFNDEF PACKAGE}
    function DefaultPeriodic: Boolean; inline;

    function ObservedStaticFieldNames: String;
    function ObservedNonPeriodicFieldNames: String;
    function BasicServiceFieldNames: String;
    function AllExcludedFieldNames: String;
{$ENDIF}
  published
    constructor Create(AOwner: TDBPeriodicTabControl); reintroduce;
    destructor Destroy; override;

    // Работает только с ДатаХандлером
    property DataHandler        : TSubDataHandler   read GetDataHandler       write SetDataHandler;

    // Поле с датой периодики (обязательное)
    property DateFieldName      : String            read FDateFieldName       write SetDateFieldName;

    // Поле (у Parent'а) с возврастом записи (необязательное)
    property AgeFieldName       : String            read GetAgeFieldName      write SetAgeFieldName;

    // Поля(ключи) распознавания записи периодики (обязательные)
    property KeyFieldNames      : String            read FKeyFieldNames       write SetKeyFieldNames;

    // Уникальные поля распознавания дубликата записи (необязательные).
    // Например в уполномоченных представителях с проставленным Link_Id (ссылка на персону)
    // Если имена заданы, но значения пустые, то идёт вставка "неуникальной записи" с заполнением данных в "NonUniqueDataFieldNames"
    property UniqueFieldNames   : String            read FUniqueFieldNames    write SetUniqueFieldNames;

    // Поля данных для неуникальных записей (VarIsPresent(UniqueFieldValues) = False)
    // Например в уполномоченных представителях без Link_Id к заполнению обязательны ФИО и т.п.
    property NonUniqueDataFieldNames: String        read FNonUniqueDataFieldNames write FNonUniqueDataFieldNames;

    // Поле (одинарное) идентификации записи.
    // Для тех случаев когда в дополнении к ключу идёт и Identity-поле.
    // Часто само Identity-поле и есть ключ - тогда тут ничего не указываем.
    // Нужно для автозаполнения при совпадении уникальности.
    property IdentityFieldName  : String            read FIdentityFieldName  write SetIdentityFieldName;

    // Поле-признак активной или удалённой записи.
    property ActiveFieldName    : String            read FActiveFieldName     write FActiveFieldName;
    // Способ хранения вышеуказанного признака
    property ActiveRecordSign   : TPeriodicActiveRecordSign read FActiveRecordSign write FActiveRecordSign Default parsDeletedTrueNull;

    // Список непериодических полей
    property StaticFieldNames   : String            read FStaticFieldNames    write SetStaticFieldNames;
    // Список полей, приватных для каждой даты периодики (не копируются при добавлении новой закладки)
    property PrivateFieldNames  : String            read FPrivateFieldNames   write SetPrivateFieldNames;

    // Значение Даты периодики в пустом Таб-контроле по-умолчанию
    property DefaultDate        : TPeriodicDefaultDate read FDefaultDate      write FDefaultDate Default pddNone;

    // Что делать при дублировании записи по KeyFieldValues?
    property KeyChangeAction    : TPeriodicKeyChangeAction read FKeyChangeAction  write FKeyChangeAction Default pkcaError;

    // Уровень заполнения последующих закладок периодики
    property Filling            : TPeriodicFilling  read FFilling             write FFilling Default pfEveryDate;

    // Признак отправки всех полей (а не только изменившихся) на сервер.
    property PostAllFields      : Boolean           read FPostAllFields       write FPostAllFields Default False;

    // Допускает периодику будущим числом.
    property Future             : Boolean           read FFuture              write FFuture Default False;

    property ExternalFilter     : String            read FExternalFilter      write SetExternalFilter;

    // Привилегии
    property ModifiAbleFieldsFieldName: String read FModifiAbleFieldsFieldName write FModifiAbleFieldsFieldName;
    property DeletablePrivilegeFieldName: String read FDeletablePrivilegeFieldName write FDeletablePrivilegeFieldName;
  end;

  TSBasePeriodicTabControlProperties = class(TComponent)
  private
    FErrorDublicateDateFmt    : String;
    FErrorDublicateKeysFmt    : String;
    FErrorNearestPeriodFmt    : String;
    FErrorEarlierThenAgeFmt   : String;
    FErrorFutureDateFmt: String;

    FConfirmDateDeleteFmt     : String;

    FMenuDateAdd              : String;
    FMenuDateChange           : String;
    FMenuDateDelete           : String;

//    procedure SetMessageDublicateError(const Value: String);
//    procedure SetQuestionDelete(const Value: String);
//    procedure SetCaptionMenuAdd(const Value: String);
//    procedure SetCaptionMenuChangeDate(const Value: String);
//    procedure SetCaptionMenuDelete(const Value: String);
//  public
//    constructor Create;
  published
    property ErrorDublicateDateFmt  : String read FErrorDublicateDateFmt  write FErrorDublicateDateFmt;
    property ErrorDublicateKeysFmt  : String read FErrorDublicateKeysFmt  write FErrorDublicateKeysFmt;
    property ErrorNearestPeriodFmt  : String read FErrorNearestPeriodFmt  write FErrorNearestPeriodFmt;
    property ErrorEarlierThenAgeFmt : String read FErrorEarlierThenAgeFmt write FErrorEarlierThenAgeFmt;
    property ErrorFutureDateFmt     : String read FErrorFutureDateFmt     write FErrorFutureDateFmt;

    property ConfirmDateDeleteFmt: String read FConfirmDateDeleteFmt write FConfirmDateDeleteFmt;

    property MenuDateAdd    : String read FMenuDateAdd    write FMenuDateAdd;
    property MenuDateChange : String read FMenuDateChange write FMenuDateChange;
    property MenuDateDelete : String read FMenuDateDelete write FMenuDateDelete;
  end;

  TDBPeriodicTabControlAgeChanging = procedure(APeriodicControl: TDBPeriodicTabControl; AAge: TDate; var AAllow) of object;
  TDBPeriodicTabControlAfterApply = procedure(APeriodicControl: TDBPeriodicTabControl; AFields: TFields; AApplyAction: TdhApplyAction) of object;

  TDBPeriodicTabControl = class(TcxCustomTabControl)
  private
    function GetProperties: TDBPeriodicTabControlProperties;
    procedure SetProperties(Value: TDBPeriodicTabControlProperties);
  protected
    function GetPropertiesClass: TcxCustomTabControlPropertiesClass; override;
  strict private
{$IFNDEF PACKAGE}
    FActive: Boolean;
    FBeforeEditKeyFieldValues: Variant;
    FBeforeEditDate: TDate;

    FOnGetEnabledNewEventFired,
    FOnGetEnabledEditEventFired,
    FOnGetEnabledDeleteEventFired: Boolean;

    FOnGetEnabledNew    : TdhGetEnabledFunction;
    FOnGetEnabledEdit   : TdhGetEnabledFunction;
    FOnGetEnabledDelete : TdhGetEnabledFunction;

    FOnBeforeNew   : TCustomDataHandlerBeforeNewEvent;
    FOnBeforeEdit  : TCustomDataHandlerAllowVariabledEvent;
    FOnBeforeDelete: TCustomDataHandlerAllowEvent;

    FOnMemoryPrepare: TCustomDataHandlerMemoryPrepareEvent;
    FOnMemoryApply  : TCustomDataHandlerMemoryApplyEvent;
{$ENDIF}
    FAgeChanging: TDBPeriodicTabControlAgeChanging;
    FAfterApply: TDBPeriodicTabControlAfterApply;

{$IFNDEF PACKAGE}
    function CheckDeleted(AActiveField: TField): Boolean;
    procedure UpdateDeleted(AActiveField: TField; ADeleted: Boolean);
{$ENDIF}
  private
    FPeriodic: TDBPeriodicTabControlPeriodic;

    FDateList: TDateList;
{$IFNDEF PACKAGE}
    FActiveDate: TDate;
{$ENDIF}

    FActiveTabIndex: Integer;

    FPopupMenu: TdxBarPopupMenu;
{$IFNDEF PACKAGE}
    FMenuItemAddDate: TdxBarButton;
    FMenuItemChangeDate: TdxBarButton;
    FMenuItemDeleteDate: TdxBarButton;
{$ENDIF}

//    FMessages: TSBasePeriodicTabControlProperties;
    FInternalReadOnly: Boolean;
    FReadOnly: Boolean;

{$IFNDEF PACKAGE}
    FActiveChanging: Boolean;
{$ENDIF}

    FOnDataCommitEventLocks : Integer;
    FOnValidate    : TDBPeriodicTabControlNotifyAllowEvent;
    FOnActiveDateChanged: TNotifyEvent;

{$IFNDEF PACKAGE}
    procedure TabControlOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MenuItemAddDateOnClick(Sender: TObject);
    procedure MenuItemChangeDateOnClick(Sender: TObject);
    procedure MenuItemDeleteDateOnClick(Sender: TObject);
    function GetNearestDate(ADate: TDate): TDate;

    function DataHandlerOnGetEnabledNew(AProperties: TdhCustomMenuActionProperties): Boolean;
    function DataHandlerOnGetEnabledEdit(AProperties: TdhCustomMenuActionProperties): Boolean;
    function DataHandlerOnGetEnabledDelete(AProperties: TdhCustomMenuActionProperties): Boolean;

    procedure DataHandlerOnMemoryPrepare(ADataHandler: TCustomDataHandler; ADataSet: TSQLDataSetProvider);

    procedure DataHandlerOnBeforeNew(ADataHandler: TCustomDataHandler; AItem: TdhMultiNewItem; var AVariables: TNamedVariants; var ADone: Boolean);
    procedure DataHandlerOnBeforeEdit(ADataHandler: TCustomDataHandler; var AVariables: TNamedVariants; var ADone: Boolean);
    procedure DataHandlerOnBeforeDelete(ADataHandler: TCustomDataHandler; var ADone: Boolean);

    procedure DataHandlerOnMemoryApply(ADataHandler: TCustomDataHandler; ADataSet: TDataSet; var AApplyAction: TdhApplyAction);
    procedure DataHandlerXMLPostOnBeforeApply(ASubDataHandler: TSubDataHandler; var AXML: String; var ADone: Boolean);

    function GetDate(ATabIndex: Integer): TDate;
    function KeepEditMode: Boolean;

    function GetActiveDate: Variant;
{$ENDIF}

    function InternalCanModify: Boolean;
    procedure UpdateReadOnly;
    procedure SetReadOnly(const Value: Boolean);

    procedure SetOnDataCommit(const Value: TDBPeriodicTabControlNotifyAllowEvent);
    procedure SetPeriodic(const Value: TDBPeriodicTabControlPeriodic);
//    procedure SetMessages(const Value: TSBasePeriodicTabControlProperties);
    function  GetDatesCount: Integer; inline;
    procedure SetDatesItem(Index: Integer; const Value: TDate);
    function  GetDatesItem(Index: Integer): TDate;

{$IFNDEF PACKAGE}
    procedure InternalOpen;
    procedure InternalClose(AInternalCall: Boolean);
    procedure SetActiveDate(const Value: Variant);
{$ENDIF}
  protected
    procedure Loaded; override;

    function DataValidated: Boolean; virtual;
    function CanModify: Boolean;

    procedure CheckPeriodicAgeField; virtual;
    procedure Refresh; virtual;
    procedure Change; override;

    function DataSet: {$IFNDEF PACKAGE}TSQLDataSetProvider{$ELSE}TDataSet{$ENDIF};
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

{$IFNDEF PACKAGE}
    //-- Добавление новой закладки
    procedure AddDate;
{$ENDIF}

    //-- Изменения даты закладки
    procedure ChangeDate;
    //-- Удаление закладки
    procedure DeleteDate;
    procedure SetTabIndex(ATabIndex: Integer);
    //-- Вызывается после создания новой записи в датасете и размножает данную запись по всем датам периодики, большей текущей
    // procedure NewRecord(AKeyValue: Variant);

    //-- Вызывается после изменения записи в датасете и обновляет все непериодические (статические) поля PeriodicStaticFieldNames
    //-- Также можно указать дополнительные непериодические поля UnPeriodicFields
    // procedure UpdateRecord(AKeyValue: Variant; const AUnPeriodicFields: String = '');


{$IFNDEF PACKAGE}
    procedure ApplyRecord(AFields: TFields; AApplyAction: TdhApplyAction);
    procedure DeleteRecord;

    procedure ReOpen;
{$ENDIF}

    //-- Вызывается после удаления записи в датасете и удаляет всю периодику по данному PeriodicKeyField
    procedure DeleteRecordHistory(AToDate: TDate);

    // -- Блокируется вызов OnValidate
    procedure LockOnDataCommitEvent;
    // -- Деблокируется вызов OnValidate
    procedure UnLockOnDataCommitEvent;

    // -- Кол-во дат (закладка с Плюсом не в счет)
    property DatesCount : Integer read GetDatesCount;
    // -- Доступ к датам по индексу. При "write" вызывается EditDate()
    property Dates[Index : Integer] : TDate read GetDatesItem write SetDatesItem;

    // -- Редактирует дату AOldDate на новую дату ANewDate!
    // -- AWithCheck - проверяеются права
    function EditDate(AOldDate: TDate; ANewDate: TDate; AWithCheck: Boolean = True): Boolean;
{$IFNDEF PACKAGE}
    property ActiveDate: Variant read GetActiveDate write SetActiveDate;
{$ENDIF}
    function KeyValue: Variant;
  published
    property AutoSize default False;
    property Align;
    property TabStop;
    property TabOrder;
//    property Padding; // Не работает всё равно

    property Properties: TDBPeriodicTabControlProperties read GetProperties write SetProperties;
    property Periodic: TDBPeriodicTabControlPeriodic  read FPeriodic write SetPeriodic;

//    property Messages   : TSBasePeriodicTabControlProperties read FMessages write SetMessages;
    property ReadOnly   : Boolean                       read FReadOnly write SetReadOnly Default False;

    property OnValidate: TDBPeriodicTabControlNotifyAllowEvent read FOnValidate write SetOnDataCommit;

    property OnCanChangeAge: TDBPeriodicTabControlAgeChanging read FAgeChanging write FAgeChanging;
    property OnActiveDateChanged: TNotifyEvent read FOnActiveDateChanged write FOnActiveDateChanged;

    property AfterApply: TDBPeriodicTabControlAfterApply read FAfterApply write FAfterApply;
  end;

{$IFDEF FASTSCRIPT_RTTI}
   TFunctions_TDBPeriodicTabControl = class(TfsRTTIModule)
  private
    function  Get_TDBPeriodicTabControl_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TDBPeriodicTabControl_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function Call_TDBPeriodicTabControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

  TfsDBPeriodicTabControlAllowEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(APeriodicControl: TDBPeriodicTabControl; var AAllow: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsDBPeriodicTabControlAllowDateEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(APeriodicControl: TDBPeriodicTabControl; ADate: TDate; var AAllow: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsDBPeriodicTabControlAfterApply = class(TfsCustomEvent)
  public
    procedure DoEvent(APeriodicControl: TDBPeriodicTabControl; AFields: TFields; AApplyAction: TdhApplyAction);
    function GetMethod: Pointer; override;
  end;
{$ENDIF}

procedure Register;

implementation

{$IFNDEF PACKAGE}
uses Forms, SBaseConstants, SBaseDataHelperInternal, SBaseUtils, Math;

type
  TSubDataHandlerXMLPostPropertiesCrack = class(TSubDataHandlerXMLPostProperties);
var
  SBasePeriodicTabControlProperties: TSBasePeriodicTabControlProperties;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('MD Project Tools', [TDBPeriodicTabControl]);
end;

{ TDBPeriodicTabControlProperties }

function TDBPeriodicTabControlProperties.DoCanChange(NewTabIndex: Integer): Boolean;
begin
{$IFNDEF PACKAGE}
  if (Control.FActiveChanging)
    or (Assigned(Control.Owner) and (Control.Owner.InheritsFrom(TSBaseFrm)) and (TSBaseFrm(Control.Owner).Loading)) then
  begin
    Result := inherited DoCanChange(NewTabIndex);
    Exit;
  end;

  Result := Control.DataValidated and (inherited DoCanChange(NewTabIndex));
{$ELSE}
  Result := True;
{$ENDIF}
end;

function TDBPeriodicTabControlProperties.GetControl: TDBPeriodicTabControl;
begin
  Result := GetOwner as TDBPeriodicTabControl;
end;

{ TDBPeriodicTabControlPeriodic }

constructor TDBPeriodicTabControlPeriodic.Create(AOwner: TDBPeriodicTabControl);
begin
  FDefaultDate := pddNone;
  FKeyChangeAction := pkcaError;
  FFilling := pfEveryDate;
//  FDeleteMethod := pdmDeleteRecord;
  FActiveRecordSign := parsDeletedTrueNull;
//  FDeleteEmptyRecords := pderNever;

//  FStyle := ptbsDefault;
  FOwner := AOwner;

  FDataLink := TDataSourceDataLink.Create;
{$IFNDEF PACKAGE}
  FDataLink.OnActiveChanged := DataSetActiveChanged;
  FDataLink.OnDataSetReOpened := DataSetActiveChanged;
{$ENDIF}

  FAgeFieldDataLink := TMDFieldDataLink.Create;
{$IFNDEF PACKAGE}
  FAgeFieldDataLink.OnFieldChanged := AgeFieldChanged;
{$ENDIF}

  inherited Create;
end;

{$IFNDEF PACKAGE}
procedure TDBPeriodicTabControlPeriodic.DataSetActiveChanged(ASender: TObject);
var
  LActive: Boolean;
  LActiveDate: Variant;
  LActiveTabIndex, LTabIndex: Integer;
begin
  LActiveDate := FOwner.ActiveDate;
  LActiveTabIndex := FOwner.TabIndex;

  LActive := Assigned(FDataLink.DataSet) and (FDataLink.DataSet.Active);
  FOwner.Properties.BeginUpdate;
  FOwner.LockOnDataCommitEvent;
  try
    try
      FOwner.FActiveChanging := True;
      try
        if LActive then begin
          FOwner.InternalOpen;
        end else begin
          FOwner.InternalClose(False);
        end;
      finally
        FOwner.FActiveChanging := False;
      end;

      if VarIsPresent(LActiveDate) then begin
        LTabIndex := FOwner.FDateList.IndexOf(VarToDateTime(LActiveDate));
        if LTabIndex = -1 then
          LTabIndex := Min(LActiveTabIndex, Pred(FOwner.DatesCount));
      end else
        LTabIndex := -1;

      if LTabIndex = -1 then
        LTabIndex := FOwner.Tabs.Count - 2;
      FOwner.SetTabIndex(LTabIndex);
    finally
      FOwner.Properties.EndUpdate;
    end;
  finally
    FOwner.UnLockOnDataCommitEvent;
  end;
end;

procedure TDBPeriodicTabControlPeriodic.AgeFieldChanged(ASender: TObject);
begin
  if FOwner.DatesCount > 0 then
    try
      FOwner.Dates[0] := FAgeFieldDataLink.Value;
    except
      FAgeFieldDataLink.Value := FOwner.Dates[0];
      Raise;
    end;
end;

function TDBPeriodicTabControlPeriodic.DefaultPeriodic: Boolean;
begin
  Result := FKeyFieldNames.IsEmpty
end;
{$ENDIF}

destructor TDBPeriodicTabControlPeriodic.Destroy;
begin
  FreeAndNil(FDataLink);
  FreeAndNil(FAgeFieldDataLink);
  inherited;
end;

function TDBPeriodicTabControlPeriodic.GetAgeFieldName: String;
begin
  Result := FAgeFieldDataLink.FieldName
end;

procedure TDBPeriodicTabControlPeriodic.SetAgeFieldName(const Value: String);
begin
  FAgeFieldDataLink.FieldName := Value;
end;

function TDBPeriodicTabControlPeriodic.GetDataHandler: TSubDataHandler;
begin
  Result := TSubDataHandler(FDataLink.DataSource);
end;

procedure TDBPeriodicTabControlPeriodic.SetDataHandler(const Value: TSubDataHandler);
begin
  FDataLink.DataSource := Value;
end;

procedure TDBPeriodicTabControlPeriodic.SetDateFieldName(const Value: String);
begin
  if FDateFieldName <> Value then begin
    FDateFieldName := Value;
    CheckProperties;
  end;
end;

procedure TDBPeriodicTabControlPeriodic.SetKeyFieldNames(const Value: String);
begin
  if FKeyFieldNames <> Value then begin
    FKeyFieldNames := Value;
    CheckProperties;
  end;
end;

procedure TDBPeriodicTabControlPeriodic.SetPrivateFieldNames(const Value: String);
begin
  if FPrivateFieldNames <> Value then begin
    FPrivateFieldNames := Value;
    CheckProperties;
  end;
end;

procedure TDBPeriodicTabControlPeriodic.SetStaticFieldNames(const Value: String);
begin
  if FStaticFieldNames <> Value then begin
    FStaticFieldNames := Value;
    CheckProperties;
  end;
end;

procedure TDBPeriodicTabControlPeriodic.SetUniqueFieldNames(const Value: String);
begin
  if FUniqueFieldNames <> Value then begin
    FUniqueFieldNames := Value;
    CheckProperties;
  end;
end;

procedure TDBPeriodicTabControlPeriodic.SetExternalFilter(const Value: String);
begin
  if (Value <> FExternalFilter) then begin
    FExternalFilter := Value;
    FOwner.Change;
  end;
end;

procedure TDBPeriodicTabControlPeriodic.SetIdentityFieldName(const Value: String);
begin
  if FIdentityFieldName <> Value then begin
    FIdentityFieldName := Value;
    CheckProperties;
  end;
end;

{$IFNDEF PACKAGE}
function TDBPeriodicTabControlPeriodic.ObservedStaticFieldNames: String;
begin
  Result := ConcatIfNotEmpty
            (
              [
                StaticFieldNames,
                IdentityFieldName,
                UniqueFieldNames
              ],
              ';'
            );
end;

function TDBPeriodicTabControlPeriodic.AllExcludedFieldNames: String;
begin
  Result := ConcatIfNotEmpty
            (
              [
                StringReplace(DataHandler.XMLPost.ExcludedFields, ',', ';', [rfReplaceAll, rfIgnoreCase]),
                DataHandler.DetailKeyFieldNames,
                ModifiAbleFieldsFieldName,
                DeletablePrivilegeFieldName
              ],
              ';'
            );
end;

function TDBPeriodicTabControlPeriodic.ObservedNonPeriodicFieldNames: String;
begin
  Result := ConcatIfNotEmpty
            (
              [
                ObservedStaticFieldNames,
                KeyFieldNames,
                DateFieldName
              ],
              ';'
            );
end;

function TDBPeriodicTabControlPeriodic.BasicServiceFieldNames: String;
begin
  Result := ConcatIfNotEmpty
            (
              [
                KeyFieldNames,
                UniqueFieldNames,
                IdentityFieldName,
                DataHandler.DetailKeyFieldNames,
                DateFieldName
              ],
              ';'
            );
end;
{$ENDIF}

procedure TDBPeriodicTabControlPeriodic.CheckProperties;
begin
{$IFNDEF PACKAGE}
  if csLoading in FOwner.ComponentState then Exit;

// FStaticFieldNames

  if InArray(FStaticFieldNames, FDateFieldName, ';') then
    raise Exception.Create('Значение свойства Periodic.StaticFieldNames не может содержать поле из DateFieldName!');

  if InArray(FStaticFieldNames, FIdentityFieldName, ';') then
    raise Exception.Create('Значение свойства Periodic.StaticFieldNames не может содержать поле из IdentityFieldName!');

  if ArraysPositiveJoin(FStaticFieldNames, FKeyFieldNames, ';') then
    raise Exception.Create('Значение свойства Periodic.StaticFieldNames не может содержать поля из KeyFieldNames!');

  if ArraysPositiveJoin(FStaticFieldNames, FUniqueFieldNames, ';') then
    raise Exception.Create('Значение свойства Periodic.StaticFieldNames не может содержать поля из UniqueFieldNames!');

// FPrivateFieldNames

  if InArray(FPrivateFieldNames, FDateFieldName, ';') then
    raise Exception.Create('Значение свойства Periodic.PrivateFieldNames не может содержать поле из DateFieldName!');

  if InArray(FPrivateFieldNames, FIdentityFieldName, ';') then
    raise Exception.Create('Значение свойства Periodic.PrivateFieldNames не может содержать поле из IdentityFieldName!');

  if ArraysPositiveJoin(FPrivateFieldNames, FStaticFieldNames, ';') then
    raise Exception.Create('Значение свойства Periodic.PrivateFieldNames не может содержать поля из StaticFieldNames!');

  if ArraysPositiveJoin(FPrivateFieldNames, FKeyFieldNames, ';') then
    raise Exception.Create('Значение свойства Periodic.PrivateFieldNames не может содержать поля из KeyFieldNames!');

  if ArraysPositiveJoin(FPrivateFieldNames, FUniqueFieldNames, ';') then
    raise Exception.Create('Значение свойства Periodic.PrivateFieldNames не может содержать поля из UniqueFieldNames!');
{$ENDIF}
end;

{ TDBPeriodicTabControl }

constructor TDBPeriodicTabControl.Create(AOwner: TComponent);
begin
{$IFNDEF PACKAGE}
  Assert(Assigned(AOwner) and (AOwner is TSBaseFrm));
{$ENDIF}
  FPeriodic := TDBPeriodicTabControlPeriodic.Create(Self);
//  FMessages := TSBasePeriodicTabControlProperties.Create;

  FActiveTabIndex := -1;
  FDateList.Clear;

{$IFNDEF PACKAGE}
  FPopupMenu := TdxBarPopupMenu.Create(Self);
  FPopupMenu.BarManager := TSBaseFrm(AOwner).BarManager;

  FMenuItemAddDate := TdxBarButton.Create(FPopupMenu);
  with FMenuItemAddDate do begin
    Caption := SBasePeriodicTabControlProperties.MenuDateAdd;
    ImageIndex := SBaseFormProperties.IconIndex_New;
    OnClick := MenuItemAddDateOnClick;
  end;

  FMenuItemChangeDate := TdxBarButton.Create(FPopupMenu);
  with FMenuItemChangeDate do begin
    Caption := SBasePeriodicTabControlProperties.MenuDateChange;
    ImageIndex := SBaseFormProperties.IconIndex_Edit;
    OnClick := MenuItemChangeDateOnClick;
  end;

  FMenuItemDeleteDate := TdxBarButton.Create(FPopupMenu);
  with FMenuItemDeleteDate do begin
    Caption := SBasePeriodicTabControlProperties.MenuDateDelete;
    ImageIndex := SBaseFormProperties.IconIndex_Delete;
    OnClick := MenuItemDeleteDateOnClick;
  end;

  FPopupMenu.ItemLinks.Add.Item := FMenuItemAddDate;
  FPopupMenu.ItemLinks.Add.Item := FMenuItemChangeDate;
  FPopupMenu.ItemLinks.Add.Item := FMenuItemDeleteDate;
{$ENDIF}

  inherited;
  ParentFont := False;
  TabStop := False;
  SetReadOnly(False);
end;

destructor TDBPeriodicTabControl.Destroy;
begin
  if Assigned(FPeriodic.DataHandler) then
    FPeriodic.DataHandler.XMLPostOnBeforeApply := nil;
  FreeAndNil(FPeriodic);

//  FreeAndNil(FMessages);
  FDateList.Clear;
  FreeAndNil(FPopupMenu);

  inherited;
end;

function TDBPeriodicTabControl.EditDate(AOldDate, ANewDate: TDate; AWithCheck: Boolean = True): Boolean;
{$IFNDEF PACKAGE}
var
  LIndex          : Integer;
  LCheckIndex     : Integer;
//  LOldDateString  : String;
//  LNewDateString  : String;
  LDataSet        : TSQLDataSetProvider;
  LKeyValue       : Variant;

  LAllFieldsEditable  : Boolean;
  LParentDataSet      : TSQLDataSetProvider;
  LAgeChangeEnabled   : Boolean;
  LXMLAgeField        : TField;
{$ENDIF}
begin
{$IFDEF PACKAGE}
  Result := False
{$ELSE}
  Result := False;
  if AOldDate = ANewDate then
    Exit;

  LDataSet := DataSet;
  LDataSet.DisableControls;
  try
    //-- Получаем индекс даты, которую надо поменять
    LIndex := FDateList.IndexOf(AOldDate);
    if LIndex < 0 then Exit;

    // -- Проверка прав
    if AWithCheck then begin
      if not CanModify then
        //Exit;
        SBaseProperties.RaiseTechnicalError('TDBPeriodicTabControl cannot be modified');

      LAllFieldsEditable  := LDataSet.Fields.IsAllEditAble;
      LParentDataSet      := FPeriodic.DataHandler.Parent.DataSet;

      if TabIndex = 0 then begin
        LAgeChangeEnabled := True;
        if (FPeriodic.AgeFieldName <> '') then begin
          if Assigned(LParentDataSet) and not LParentDataSet.IsEmpty then
            LXMLAgeField := LParentDataSet.FindField(FPeriodic.AgeFieldName)
          else
            LXMLAgeField := nil;

          LAgeChangeEnabled := Assigned(LXMLAgeField) and LXMLAgeField.CanModify;
        end;
      end else
        LAgeChangeEnabled := LAllFieldsEditable;

      if (LIndex = 0) and (not LAgeChangeEnabled) then
        Exit;
      if (LIndex > 0) and (not LAllFieldsEditable) then
        Exit;
    end;

    //-- Проверяем нет ли уже даты ANewDate!
    LCheckIndex := FDateList.IndexOf(ANewDate);
    if (LCheckIndex >= 0) and (LCheckIndex <> LIndex) then
      TSBaseFrm(nil).WarningMessage(Format(SBasePeriodicTabControlProperties.ErrorDublicateDateFmt, [DateToStr(ANewDate)]), Self);
      // Abort!

    // Проверяем, чтобы не было перскоков дат
    if ( (LIndex > 0) and (Dates[LIndex - 1] > ANewDate ) )
    or ( (LIndex < DatesCount - 1) and (Dates[LIndex + 1] < ANewDate) ) then
      TSBaseFrm(nil).WarningMessage(SBasePeriodicTabControlProperties.ErrorNearestPeriodFmt, Self);

    //-- Обновляем список закладок
    FDateList[LIndex] := ANewDate;

    //-- Обновляем дату у записей

    //-- Если указано ключевое поле, то запоминаем ключевое значение
    if FPeriodic.KeyFieldNames <> '' then
      LKeyValue := LDataSet.FieldValues[FPeriodic.KeyFieldNames];

    LDataSet.Filter := FPeriodic.DateFieldName + ' = ''' + DateToStr(AOldDate) + ''' ';

    while not LDataSet.Eof do begin
      LDataSet.Edit;
        LDataSet.FieldByName(FPeriodic.DateFieldName).AsDateTime := ANewDate;
      LDataSet.Post;
      Result := True;
    end;

    LockOnDataCommitEvent;
    try
      //-- Обновляем закладки
      Refresh;
      //-- Контроль за полем PeriodicAgeFieldName
      CheckPeriodicAgeField;
      //-- Позиционируемся на текущей
      SetTabIndex(LIndex);

      //-- Позиционируемся на ключевое значение (если оно есть)
      if (not FPeriodic.KeyFieldNames.IsEmpty) and VarIsPresent(LKeyValue) then
        LDataSet.Locate(FPeriodic.KeyFieldNames, LKeyValue, []);
    finally
      UnLockOnDataCommitEvent;
    end;
  finally
    LDataSet.EnableControls;
  end;
{$ENDIF}
end;

{$IFNDEF PACKAGE}
procedure TDBPeriodicTabControl.AddDate;
var
  LVariant      : TNamedVariants;
//  LDateString   : String;
  LTabIndex, I  : Integer;
  LDate, MDate  : TDate;
  V             : Variant;
  LFieldNames   : String;
  LDataSet      : TDataSet;
  LKeyValue     : Variant;
  LKeepEditMode : Boolean;
begin
  //-- Должна быть как минимум одна закладка
//  if not DataCommited then Exit;
  Assert((FPeriodic.DefaultDate = pddNone) or (DatesCount > 0));

  LockOnDataCommitEvent;
  LDataSet := DataSet;
  LDataSet.DisableControls;
  try
    LVariant := TSBaseSelectDateFrm.Select(Self, []);
    if (LVariant.Count = 0) then
      SetTabIndex(FActiveTabIndex)
    else try
      LDate := VarToDateTime(LVariant[SConst_Value]);
      //-- Проверяем на наличие такой даты
//      LDateString := DateToDateString(LDate);

      LTabIndex := FDateList.IndexOf(LDate);
      if LTabIndex <> -1 then
        TSBaseFrm(nil).WarningMessage(Format(SBasePeriodicTabControlProperties.ErrorDublicateDateFmt, [DateToStr(LDate)]), Self);

      if Date < LDate then
        TSBaseFrm(nil).WarningMessage(SBasePeriodicTabControlProperties.ErrorFutureDateFmt, Self);

      if (DatesCount > 0) then begin
        if (FDateList[0] < LDate) then begin
          //-- Находим ближайшую дату для копирования данных
          MDate := GetNearestDate(LDate);

          if not FPeriodic.KeyFieldNames.IsEmpty then
            LKeyValue := LDataSet.FieldValues[FPeriodic.KeyFieldNames];

          //-- Копируем данные из ближайшей даты
          LDataSet.Filter := FPeriodic.DateFieldName + ' = ''' + DateToStr(MDate) + ''' ';
          LKeepEditMode := KeepEditMode;

          //-- Буфер, в который записываем строчки
          V := VarArrayCreate([0, LDataSet.RecordCount - 1], varVariant);

          //-- Список полей
          for I := 0 to LDataSet.FieldCount - 1 do
            LFieldNames := ConcatIfNotEmpty(LFieldNames, ';', LDataSet.Fields[I].FieldName);

          I := 0;
          LDataSet.First;
          while not LDataSet.Eof do begin
            V[I] := LDataSet.FieldValues[LFieldNames];
            I := I + 1;
            LDataSet.Next;
          end;

          for I := 0 to VarArrayHighBound(V, 1) do begin
            LDataSet.Append;
              LDataSet.Fields.SetAllEditable;

              LDataSet.FieldValues[LFieldNames] := V[I];
              LDataSet.FieldByName(FPeriodic.DateFieldName).AsDateTime := LDate;
            LDataSet.Post;
          end;

          //-- Позиционируемся на ключевое значение (если оно есть)
          if LKeepEditMode then
            LDataSet.Edit
          else if (not FPeriodic.KeyFieldNames.IsEmpty) and VarIsPresent(LKeyValue) then
            LDataSet.Locate(FPeriodic.KeyFieldNames, LKeyValue, []);

        end else if (Periodic.Filling = pfEveryDate) then
          // В режиме pfEveryDate вы не можете добавить дату меньше чем первая так запросто
          // Пользуйтесь переносом первой даты, для модифицирования более ранних периодов
          TSBaseFrm(nil).WarningMessage(SBasePeriodicTabControlProperties.ErrorEarlierThenAgeFmt, Self);
      end;

      //-- Добавляем новую дату в список
      FDateList.Append(LDate);
      //-- Обновляем закладки
      Refresh;
      //-- Контроль за полем PeriodicAgeFieldName
      CheckPeriodicAgeField;
      //-- Позиционируемся на новой закладке
      SetTabIndex(FDateList.IndexOf(LDate));
    except
      SetTabIndex(FActiveTabIndex);
      Raise;
    end;
  finally
    LDataSet.EnableControls;
    UnLockOnDataCommitEvent;
  end;
end;
{$ENDIF}

function TDBPeriodicTabControl.InternalCanModify: Boolean;
begin
  Result := Assigned(FPeriodic.DataHandler) and FPeriodic.DataHandler.CanModify;
end;

function TDBPeriodicTabControl.CanModify: Boolean;
begin
  Result := (not FInternalReadOnly) and (not FReadOnly) and InternalCanModify;
end;

procedure TDBPeriodicTabControl.Change;
//-- Наследуем перещелкивание закладок
{$IFNDEF PACKAGE}
var
  LDataSet  : TDataSet;
  LDate     : TDate;
  LKeyValue : Variant;
  LFilter   : String;
  LActiveDateChanged: Boolean;
{$ENDIF}
begin
  inherited;

{$IFNDEF PACKAGE}
  LDataSet := DataSet;
  if not Assigned(LDataSet)
        or (FPeriodic.DateFieldName.IsEmpty)
        or (TabIndex = -1)
        or (not LDataSet.Active)
        or (DatesCount = 0)
  then
    Exit;

  if LDataSet.Editing then
    LDataSet.Post;

  LActiveDateChanged := False;
  LDataSet.DisableControls;
  try
    //-- Если указано ключевое поле, то запоминаем ключевое значение
    if (not FPeriodic.KeyFieldNames.IsEmpty) then
      LKeyValue := LDataSet.FieldValues[FPeriodic.KeyFieldNames];

    //-- Если активная закладка не последняя
    if (TabIndex < Tabs.Count - 1) then begin
      LDate := FDateList[TabIndex];
      //if LDate = FActiveDate then Exit;
      LActiveDateChanged := (LDate <> FActiveDate);
      FActiveDate := LDate;
      LFilter :=  '(' + FPeriodic.DateFieldName + ' IS NULL OR ' + FPeriodic.DateFieldName + ' = ''' + DateToStr(LDate) + ''')';
      if FPeriodic.ExternalFilter <> '' then
        LFilter := LFilter + ' AND (' + FPeriodic.ExternalFilter + ')';
      LDataSet.Filter  := LFilter;
    end else if FActiveChanging then
      Exit
    else if (Tabs.Count > 1) then
      AddDate;

    //-- Для возвращения к предыдущей закладке, в случае отмены создания новой
    FActiveTabIndex := TabIndex;

    //-- Позиционируемся на ключевое значение (если оно есть)
    if (not FPeriodic.KeyFieldNames.IsEmpty) and VarIsPresent(LKeyValue) then
      LDataSet.Locate(FPeriodic.KeyFieldNames, LKeyValue, []);
  finally
    LDataSet.EnableControls;
  end;

  if LActiveDateChanged then begin
    Periodic.DataHandler.UpdateActive;
    if Assigned(FOnActiveDateChanged) then
      FOnActiveDateChanged(Self);
  end;
{$ENDIF}
end;

function TDBPeriodicTabControl.DataSet: {$IFNDEF PACKAGE}TSQLDataSetProvider{$ELSE}TDataSet{$ENDIF};
begin
{$IFNDEF PACKAGE}
  Result := TSQLDataSetProvider(FPeriodic.FDataLink.DataSet.Source)
{$ELSE}
  Result := FPeriodic.FDataLink.DataSet
{$ENDIF};
end;

//-- Удаление закладки
procedure TDBPeriodicTabControl.DeleteDate;
{$IFNDEF PACKAGE}
var
  LDate       : TDate;
//  LDateString : String;
  LKeyValue   : Variant;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  //-- Текущая дата
//  LDateString := FDateList[TabIndex];
  LDate := FDateList[TabIndex];

  //-- Если осталась последняя закладка - удаление невозможно
  Assert((DatesCount > 1) or (FPeriodic.DefaultDate = pddNone));

  if not TSBaseFrm.ConfirmMessage(Format(SBasePeriodicTabControlProperties.ConfirmDateDeleteFmt, [DateToStr(LDate)]), Self) then
    Exit;

  DataSet.DisableControls;
  try
    with DataSet do begin
      //-- Если указано ключевое поле, то запоминаем ключевое значение
      if FPeriodic.KeyFieldNames <> '' then
        LKeyValue := FieldValues[FPeriodic.KeyFieldNames];

      Filter := FPeriodic.DateFieldName + ' = ''' + DateToStr(LDate) + ''' ';

      while not Eof do
        Delete;
    end;

    LockOnDataCommitEvent;
    try
      //-- Удаляем из списка
      FDateList.Delete(TabIndex);
      //-- Обновляем закладки
      Refresh;
      //-- Контроль за полем PeriodicAgeFieldName
      CheckPeriodicAgeField;
      //-- Находим ближайшую дату
      LDate := GetNearestDate(LDate);
      //-- Позиционируемся
      SetTabIndex(FDateList.IndexOf(LDate));

      //-- Позиционируемся на ключевое значение (если оно есть)
      if (FPeriodic.KeyFieldNames <> '') and  VarIsPresent(LKeyValue) then
         DataSet.Locate(FPeriodic.KeyFieldNames, LKeyValue, []);

    finally
      UnLockOnDataCommitEvent;
    end;
  finally
    DataSet.EnableControls;
  end;
{$ENDIF}
end;

procedure TDBPeriodicTabControl.DeleteRecordHistory(AToDate: TDate);
{$IFNDEF PACKAGE}
var
  LKeyFieldValues: Variant;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  with DataSet do begin
    LKeyFieldValues := DataSet[Periodic.KeyFieldNames];
    DisableControls;
    Filtered := False;
    try
      while Locate(FPeriodic.KeyFieldNames, LKeyFieldValues, []) and
      (
        (AToDate = 0)
        or
        (FieldByName(FPeriodic.DateFieldName).AsDateTime < AToDate)
      )
      do
        Delete;
    finally
      Filtered := True;
      EnableControls;
    end;
  end;
{$ENDIF}
end;

{$IFNDEF PACKAGE}
function TDBPeriodicTabControl.GetNearestDate(ADate: TDate): TDate;
var
  I: Integer;
begin
  FDateList.Sort;
  Result := 0;

  for I := 0 to Pred(DatesCount) do begin
    if ADate < FDateList[I] then begin
      if I = 0 then
        Result := FDateList[I];
      Break;
    end;
    Result := FDateList[I];
  end;
end;
{$ENDIF}

{$IFNDEF PACKAGE}
function TDBPeriodicTabControl.GetDate(ATabIndex: Integer): TDate;
begin
//  if (ATabIndex >= 0) and (ATabIndex < DatesCount) then
  Result := FDateList[ATabIndex]
//  else
//    Result := Null;
end;
{$ENDIF}

function TDBPeriodicTabControl.GetDatesCount: Integer;
begin
  Result := FDateList.Count;
end;

function TDBPeriodicTabControl.GetDatesItem(Index: Integer): TDate;
begin
  if (Index >= 0) and (Index < DatesCount) then
    Result := FDateList[Index]
  else
    Result := 0;
end;

{$IFNDEF PACKAGE}
function TDBPeriodicTabControl.GetActiveDate: Variant;
begin
  if FActiveDate = 0 then
    Result := Null
  else
    Result := VarFromDateTime(FActiveDate)
end;
{$ENDIF}

function TDBPeriodicTabControl.GetProperties: TDBPeriodicTabControlProperties;
begin
  Result := TDBPeriodicTabControlProperties(inherited Properties);
end;

procedure TDBPeriodicTabControl.SetProperties(Value: TDBPeriodicTabControlProperties);
begin
  inherited Properties := Value;
end;

function TDBPeriodicTabControl.GetPropertiesClass: TcxCustomTabControlPropertiesClass;
begin
  Result := TDBPeriodicTabControlProperties;
end;

{$IFNDEF PACKAGE}
function TDBPeriodicTabControl.KeepEditMode: Boolean;
begin
  // Автоматическое определение режима AlwaysEdit
  Result := (Periodic.DefaultDate = pddEmptyDataPage)
                    and (Periodic.Filling = pfEveryDate)
                    and (DataSet.RecordCount = 1)
                    and (not Periodic.DataHandler.OptionsMenu.KeyFieldsMode);
end;
{$ENDIF}

function TDBPeriodicTabControl.KeyValue: Variant;
begin
  if FPeriodic.KeyFieldNames = '' then
    Result := Unassigned
  else
    Result := DataSet.FieldValues[FPeriodic.KeyFieldNames];
end;

procedure TDBPeriodicTabControl.Loaded;
begin
  inherited;
  FPeriodic.CheckProperties;
end;

procedure TDBPeriodicTabControl.LockOnDataCommitEvent;
begin
  Inc(FOnDataCommitEventLocks);
end;

{$IFNDEF PACKAGE}
procedure TDBPeriodicTabControl.MenuItemAddDateOnClick(Sender: TObject);
begin
  AddDate;
end;

procedure TDBPeriodicTabControl.MenuItemChangeDateOnClick(Sender: TObject);
begin
  ChangeDate;
end;

procedure TDBPeriodicTabControl.MenuItemDeleteDateOnClick(Sender: TObject);
begin
  DeleteDate;
end;
{$ENDIF}

{$IFNDEF PACKAGE}
procedure TDBPeriodicTabControl.InternalOpen;
var
  LDataSet          : TDataSet;
  LDataSetProvider  : TSQLDataSetProvider absolute LDataSet;
  LDate       : TDate;
  LDateField  : TField;
  LAgeField   : TField;
  LDataSourceStateBookmark: TDataSourceStateBookmark;
begin
  InternalClose(True);

  FPeriodic.DataHandler.XMLPostOnBeforeApply  := DataHandlerXMLPostOnBeforeApply;
  FOnBeforeDelete := FPeriodic.DataHandler.OnBeforeDelete; FPeriodic.DataHandler.OnBeforeDelete := DataHandlerOnBeforeDelete;

  with FPeriodic.DataHandler do if (not OptionsMenu.KeyFieldsMode) then begin
    XMLPost.AddExcludedFields(FPeriodic.FModifiAbleFieldsFieldName);
    if not FPeriodic.FDeletablePrivilegeFieldName.IsEmpty then begin
      OptionsMenu.OptionsNew.Constants.Add(FPeriodic.FDeletablePrivilegeFieldName, True);
      DataSet.FieldsAutomations.AddServiceFields(FPeriodic.FDeletablePrivilegeFieldName);
      XMLPost.AddExcludedFields(FPeriodic.FDeletablePrivilegeFieldName);
    end;

    if not OptionsMenu.KeyFieldsMode then begin
      with OptionsMenu.OptionsNew do begin
        FOnGetEnabledNewEventFired := InternalVisible and Enabled and (GetActionMode in [amDataSet, amDataSetSelect]);
        if FOnGetEnabledNewEventFired then begin
          FOnGetEnabledNew            := OnGetEnabled.SystemFunction;
          OnGetEnabled.SystemFunction := DataHandlerOnGetEnabledNew;

          FOnBeforeNew                      := FPeriodic.DataHandler.OnBeforeNew;
          FPeriodic.DataHandler.OnBeforeNew := DataHandlerOnBeforeNew;
        end;
      end;

      with OptionsMenu.OptionsEdit do begin
        FOnGetEnabledEditEventFired := InternalVisible and Enabled and (GetActionMode in [amDataSet, amDataSetSelect]);
        if FOnGetEnabledEditEventFired then begin
          FOnGetEnabledEdit           := OnGetEnabled.SystemFunction;
          OnGetEnabled.SystemFunction := DataHandlerOnGetEnabledEdit;

          FOnBeforeEdit                       := FPeriodic.DataHandler.OnBeforeEdit;
          FPeriodic.DataHandler.OnBeforeEdit  := DataHandlerOnBeforeEdit;
        end;
      end;

      with OptionsMenu.OptionsDelete do begin
        FOnGetEnabledDeleteEventFired := InternalVisible and Enabled;
        if FOnGetEnabledDeleteEventFired then begin
          FOnGetEnabledDelete         := OnGetEnabled.SystemFunction;
          OnGetEnabled.SystemFunction := DataHandlerOnGetEnabledDelete;
        end;
      end;

      if FOnGetEnabledNewEventFired or FOnGetEnabledEditEventFired then begin
        FOnMemoryPrepare  := FPeriodic.DataHandler.OnMemoryPrepare; FPeriodic.DataHandler.OnMemoryPrepare := DataHandlerOnMemoryPrepare;
        FOnMemoryApply    := FPeriodic.DataHandler.OnMemoryApply  ; FPeriodic.DataHandler.OnMemoryApply   := DataHandlerOnMemoryApply;
      end;
    end;
  end;

  FActive := True;
  LDataSet := DataSet;
  Assert(Assigned(LDataSet) and (not FPeriodic.DateFieldName.IsEmpty));

  FPeriodic.FAgeFieldDataLink.DataSource := FPeriodic.DataHandler.Parent;

  Assert(Assigned(FPeriodic.DataHandler) and Assigned(FPeriodic.DataHandler.Parent) and Assigned(FPeriodic.DataHandler.Parent.DataSet));
  if FPeriodic.AgeFieldName.IsEmpty then
    LAgeField := nil
  else begin
    LAgeField := FPeriodic.DataHandler.Parent.DataSet.FieldByName(FPeriodic.AgeFieldName);
    Assert((FPeriodic.DefaultDate <> pddNone) or (LDataSet.IsEmpty and (not LAgeField.IsNull)));
  end;

  LockOnDataCommitEvent;
  try
    //-- Заполняем список дат из датасета
    LDataSet.BeginUpdate(LDataSourceStateBookmark);
    try
      LDataSetProvider.UpdateStatusFilter := Default_UpdateStatusFilter;
      LDataSet.Filtered := False;
      if LDataSet.IsEmpty then begin
        if (FPeriodic.DefaultDate = pddEmptyDataPage) and LDataSet.CanModify then begin
          LDataSet.Append;
            with LDataSet.FieldByName(FPeriodic.DateFieldName) do
              if Assigned(LAgeField) and not LAgeField.IsNull then
                AsDateTime := LAgeField.AsDateTime
              else
                AsDateTime := Date;
          LDataSet.Post;
        end
        //else if FPeriodic.DefaultPeriodic then
        //  Exit;
      end else
        LDataSet.First;

      if Assigned(LAgeField) and not LAgeField.IsNull then
        FDateList.Append(LAgeField.AsDateTime);

      LDateField := LDataSet.FieldByName(FPeriodic.DateFieldName);
      while not LDataSet.Eof do begin
        if not LDateField.IsNull then begin
          LDate := LDateField.AsDateTime;
          if FDateList.IndexOf(LDate)= -1 then
            FDateList.Append(LDate);
        end;
        LDataSet.Next;
      end;

      //-- если записи в датасете есть, но нет ни одной даты - создаем текущую дату
      if (DatesCount = 0) and (FPeriodic.DefaultDate = pddEmptyPage) then
        FDateList.Append(Date);

//        if (DatesCount = 0) and (not (FPeriodic.Style <> ptbsDefault)) then
//          Raise Exception.Create(Self.Name + ': В наборе данных Периодики не обнаружены записи с данными!');

      LDataSet.Filtered := True;

      //-- Контроль за полем PeriodicAgeFieldName
      CheckPeriodicAgeField;
      if (not LDataSet.IsEmpty) and (not LDataSet.Editing) then
        LDataSet.First;

      // -- Строим закладки на TabControl'е
      Refresh;
      // Change;
    finally
      LDataSet.EndUpdate(LDataSourceStateBookmark, [sbrUpdateStatusFilter, sbrDisableCount]);
    end;
  finally
    UnLockOnDataCommitEvent;
  end;
end;

procedure TDBPeriodicTabControl.InternalClose(AInternalCall: Boolean);
var
  LActiveDatePresent: Boolean;
begin
  if FActive then begin
    FActive := False;
    FPeriodic.DataHandler.OnBeforeDelete := FOnBeforeDelete;

    if FOnGetEnabledNewEventFired or FOnGetEnabledEditEventFired then begin
      FPeriodic.DataHandler.OnMemoryPrepare := FOnMemoryPrepare;
      FPeriodic.DataHandler.OnMemoryApply   := FOnMemoryApply;
    end;

    with FPeriodic.DataHandler.OptionsMenu do begin
      if FOnGetEnabledNewEventFired then begin
        OptionsNew.OnGetEnabled.SystemFunction := FOnGetEnabledNew;
        FPeriodic.DataHandler.OnBeforeNew := FOnBeforeNew;
      end;
      if FOnGetEnabledEditEventFired then begin
        OptionsEdit.OnGetEnabled.SystemFunction := FOnGetEnabledEdit;
        FPeriodic.DataHandler.OnBeforeEdit := FOnBeforeEdit;
      end;
      if FOnGetEnabledDeleteEventFired then
        OptionsDelete.OnGetEnabled.SystemFunction := FOnGetEnabledDelete;
    end;

    FDateList.Clear;
    LActiveDatePresent := (FActiveDate <> 0);
    FActiveDate := 0;
    FActiveTabIndex := -1;

    if not AInternalCall then Refresh;

    FPeriodic.FAgeFieldDataLink.DataSource := nil;

    if LActiveDatePresent and Assigned(FOnActiveDateChanged) then
      FOnActiveDateChanged(Self);
  end;
end;
{$ENDIF}

{$IFNDEF PACKAGE}
procedure TDBPeriodicTabControl.TabControlOnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LDataSet, LParentDataSet: TSQLDataSetProvider;
  LXMLAgeField: TField;
  LAllFieldsEditable, LAgeChangeEnabled: Boolean;
  LTabRect: TRect;
begin
  if (TabIndex < 0) or (Button <> mbRight) then
    Exit;

  LTabRect := Tabs[TabIndex].VisibleRect;

  if not ((X >= LTabRect.Left) and (X <= LTabRect.Right) and (Y >= LTabRect.Top) and (Y <= LTabRect.Bottom)) then
    Exit;

  LDataSet := DataSet;
  if (not CanModify)
      or (LDataSet.IsEmpty and FPeriodic.DefaultPeriodic)
      or FPeriodic.DataHandler.OptionsMenu.KeyFieldsMode
  then
    Exit;

  LAllFieldsEditable  := LDataSet.Fields.IsAllEditAble;
  LParentDataSet      := FPeriodic.DataHandler.Parent.DataSet;

  if TabIndex = 0 then begin
    LAgeChangeEnabled := True;
    if (FPeriodic.AgeFieldName <> '') then begin
      if Assigned(LParentDataSet) and not LParentDataSet.IsEmpty then
        LXMLAgeField := LParentDataSet.FindField(FPeriodic.AgeFieldName)
      else
        LXMLAgeField := nil;

      LAgeChangeEnabled := Assigned(LXMLAgeField) and LXMLAgeField.CanModify;
    end;
  end else
    LAgeChangeEnabled := LAllFieldsEditable;

  FMenuItemChangeDate.Enabled := LAgeChangeEnabled and (DatesCount > 0);
  FMenuItemAddDate.Enabled := LAllFieldsEditable; // and ((DatesCount > 0) or (not FPeriodic.DefaultPeriodic));
  FMenuItemDeleteDate.Enabled := (DatesCount > 0)
                                    and ((FPeriodic.DefaultDate = pddNone) or (DatesCount > 1))
                                    and
                                    (
                                      (TabIndex = 0) and LAgeChangeEnabled)
                                      or ((TabIndex > 0) and LAllFieldsEditable
                                    );

  FPopupMenu.PopupFromCursorPos;
end;
{$ENDIF}

//-- Процедура построения закладок по списку дат
procedure TDBPeriodicTabControl.Refresh;
var
  I           : Integer;
  LDateString : String;
  LReadOnly   : Boolean;
begin
//  if Assigned(FPeriodic.DataHandler) then
//    LReadOnly := (not FPeriodic.DataHandler.CanModify)
//                  or
//                 FPeriodic.DataHandler.OptionsMenu.KeyFieldsMode
//  else
//    LReadOnly := False;

  LReadOnly := (not InternalCanModify) or FPeriodic.DataHandler.OptionsMenu.KeyFieldsMode;

  //-- Сортируем список
  FDateList.Sort;

  Properties.Tabs.BeginUpdate;
  try
    //-- Очищаем все закладки
    Tabs.Clear;
    //-- Создаем закладки
    for I := 0 to Pred(DatesCount) do begin
      LDateString := DateToStr(FDateList[I]); // FormatDateTime('dd.mm.yyyy', FDateList[I]);
      if Tabs.IndexOf(LDateString) = -1 then
        Properties.Tabs.Add(LDateString);
    end;

     //-- Добавляем еще одну с ' + '
    Properties.Tabs.Add(SConst_PlusCaption);
  finally
    Properties.Tabs.EndUpdate;
  end;

  //-- Приводим свое ReadOnly в соответсвие с PeriodicDataHandler
  FInternalReadOnly := LReadOnly;
  UpdateReadOnly;
end;

{$IFNDEF PACKAGE}
procedure TDBPeriodicTabControl.ReOpen;
begin
  DataSet.ReOpen;
  Periodic.DataSetActiveChanged(nil);
end;

procedure TDBPeriodicTabControl.SetActiveDate(const Value: Variant);
begin
  SetTabIndex(FDateList.IndexOf(VarToDateTime(Value)));
end;
{$ENDIF}

procedure TDBPeriodicTabControl.SetDatesItem(Index: Integer; const Value: TDate);
var
  LOldDate : TDate;
begin
  if (Index >= 0) and (Index < DatesCount) then begin
    LOldDate := Dates[Index];
    EditDate(LOldDate, Value);
  end;
end;

procedure TDBPeriodicTabControl.SetOnDataCommit(const Value: TDBPeriodicTabControlNotifyAllowEvent);
begin
  FOnValidate := Value;
end;

procedure TDBPeriodicTabControl.SetPeriodic(const Value: TDBPeriodicTabControlPeriodic);
begin
  FPeriodic.Assign(Value);
end;

procedure TDBPeriodicTabControl.UpdateReadOnly;
var
  LCanModify: Boolean;
begin
  LCanModify := CanModify;
  if Tabs.Count > 0 then
    with Periodic.DataHandler.OptionsMenu do
      Tabs[Pred(Tabs.Count)].Visible := LCanModify or not (OptionsNew.Visible or OptionsCopy.Visible);

{$IFNDEF PACKAGE}
  if LCanModify then
    OnMouseDown := TabControlOnMouseDown
  else
    OnMouseDown := nil;
{$ENDIF}
end;

procedure TDBPeriodicTabControl.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then begin
    FReadOnly := Value;
    UpdateReadOnly;
  end;
end;

procedure TDBPeriodicTabControl.SetTabIndex(ATabIndex: Integer);
begin
  if (ATabIndex < 0) then
    if (Tabs.Count = 1) then
      ATabIndex := 0
    else
      Assert(Tabs.Count = 0);

//  if TabIndex <> ATabIndex then begin // А если закладка создаётся на томже месте где был TabIndex?
    TabIndex := ATabIndex;
    Change;
//  end;
end;

procedure TDBPeriodicTabControl.UnLockOnDataCommitEvent;
begin
  if FOnDataCommitEventLocks > 0 then
    Dec(FOnDataCommitEventLocks);
end;

{$IFNDEF PACKAGE}
procedure TDBPeriodicTabControl.UpdateDeleted(AActiveField: TField; ADeleted: Boolean);
begin
  DataSet.Edit;
  case Periodic.ActiveRecordSign of
    parsDeletedTrueNull : if ADeleted then AActiveField.AsBoolean := True else AActiveField.Clear;
    parsDeletedTrueFalse: if ADeleted then AActiveField.AsBoolean := True else AActiveField.AsBoolean := False;
    parsActiveTrueNull  : if ADeleted then AActiveField.Clear else AActiveField.AsBoolean := True;
    parsActiveTrueFalse : if ADeleted then AActiveField.AsBoolean := False else AActiveField.AsBoolean := True;
    parsDeletedNull     : if ADeleted then AActiveField.Clear else if AActiveField.IsNull then Raise Exception.Create('UpdateDeleted(True) method failed for parsDeletedNull mode');
  end;
end;
{$ENDIF}

function TDBPeriodicTabControl.DataValidated: Boolean;
begin
  Result := True;
  if (FOnDataCommitEventLocks > 0) then Exit;
    
  if Assigned(FOnValidate) then
    FOnValidate(Self, Result);
end;

{$IFNDEF PACKAGE}
procedure TDBPeriodicTabControl.DataHandlerOnBeforeNew(ADataHandler: TCustomDataHandler; AItem: TdhMultiNewItem; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  if Assigned(FOnBeforeNew) then
    FOnBeforeNew(ADataHandler, AItem, AVariables, ADone);

  if not ADone then begin
    AVariables[Periodic.DateFieldName] := ActiveDate;
  end;
end;

function TDBPeriodicTabControl.DataHandlerOnGetEnabledNew(AProperties: TdhCustomMenuActionProperties): Boolean;
begin
  Result := VarIsPresent(ActiveDate);
  if Result and Assigned(FOnGetEnabledNew) then
    Result := FOnGetEnabledNew(AProperties)
end;

function TDBPeriodicTabControl.DataHandlerOnGetEnabledEdit(AProperties: TdhCustomMenuActionProperties): Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := FPeriodic.DataHandler.DataSet;
  Result := (not LDataSet.FieldByName(FPeriodic.DateFieldName).IsNull)
              and
              (
                FPeriodic.FModifiAbleFieldsFieldName.IsEmpty
                or
                (LDataSet.FieldByName(FPeriodic.FModifiAbleFieldsFieldName).AsString <> '')
              );
  if Result and Assigned(FOnGetEnabledEdit) then
    Result := FOnGetEnabledEdit(AProperties)
end;

function TDBPeriodicTabControl.DataHandlerOnGetEnabledDelete(AProperties: TdhCustomMenuActionProperties): Boolean;
var
  LDataSet: TDataSet;
begin
  LDataSet := FPeriodic.DataHandler.DataSet;
  Result := (not LDataSet.FieldByName(FPeriodic.DateFieldName).IsNull)
              and
              (
                FPeriodic.FDeletablePrivilegeFieldName.IsEmpty
                or
                LDataSet.FieldByName(FPeriodic.FDeletablePrivilegeFieldName).AsBoolean
              );
  if Result and Assigned(FOnGetEnabledDelete) then
    Result := FOnGetEnabledDelete(AProperties)
end;

procedure TDBPeriodicTabControl.DeleteRecord;
var
  LBookmark: TDataSourceStateBookmark;
  LActiveField: TField;
  LDataHandlerDataSetProvider: TSQLDataSetProvider;
  LKeyFieldValues: Variant;
  LPeriodicDataFieldNames: String;
  I: Integer;
begin
  LDataHandlerDataSetProvider := Periodic.DataHandler.DataSet;
  LDataHandlerDataSetProvider.BeginUpdate(LBookmark);
  try
    if Periodic.DataHandler.Settings.CascadeDelete then begin
      LKeyFieldValues := LDataHandlerDataSetProvider[Periodic.KeyFieldNames];
      LDataHandlerDataSetProvider.UpdateStatusFilter := Default_UpdateStatusFilter;
      LDataHandlerDataSetProvider.Filtered := False;
      if LDataHandlerDataSetProvider.Locate(Periodic.KeyFieldNames, LKeyFieldValues, []) then
        repeat
          LDataHandlerDataSetProvider.Delete;
        until LDataHandlerDataSetProvider.IsEmpty or (not LDataHandlerDataSetProvider.Locate(Periodic.KeyFieldNames, LKeyFieldValues, []));
    end else begin
      if not Periodic.ActiveFieldName.IsEmpty then begin
        LActiveField := LDataHandlerDataSetProvider.FieldByName(Periodic.ActiveFieldName);
        Assert(not CheckDeleted(LActiveField));
        UpdateDeleted(LActiveField, True);
      end else begin
        LPeriodicDataFieldNames :=
          ArraysAntiJoin
          (
            LDataHandlerDataSetProvider.Fields.All,
            [
              Periodic.ObservedNonPeriodicFieldNames,
              FPeriodic.ModifiAbleFieldsFieldName,
              FPeriodic.DeletablePrivilegeFieldName
            ],
            ';'
          );
        LDataHandlerDataSetProvider.Edit;
        for I := 0 to LDataHandlerDataSetProvider.FieldCount - 1 do
          with LDataHandlerDataSetProvider.Fields[I] do
            if CanModify and InArray(LPeriodicDataFieldNames, FieldName, ';') then
              Clear;
      end;
      LDataHandlerDataSetProvider.Post;
    end;
  finally
    LDataHandlerDataSetProvider.EndUpdate(LBookmark, [sbrFiltered, sbrFilter, sbrDisableCount], [sbrRecordIndex]);
  end;
end;

procedure TDBPeriodicTabControl.DataHandlerOnBeforeDelete(ADataHandler: TCustomDataHandler; var ADone: Boolean);
begin
  if Assigned(FOnBeforeDelete) then
    FOnBeforeDelete(ADataHandler, ADone);

  if not ADone then begin
    DeleteRecord;
    ADone := True;
  end;
end;

procedure TDBPeriodicTabControl.DataHandlerOnBeforeEdit(ADataHandler: TCustomDataHandler; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  if Assigned(FOnBeforeEdit) then
    FOnBeforeEdit(ADataHandler, AVariables, ADone);

  if not ADone then begin
    FBeforeEditKeyFieldValues := ADataHandler.DataSet.FieldValues[Periodic.KeyFieldNames];
    FBeforeEditDate := ADataHandler.DataSet.FieldByName(Periodic.DateFieldName).AsDateTime;
  end;
end;

function TDBPeriodicTabControl.CheckDeleted(AActiveField: TField): Boolean;
begin
  // parsDeletedTrueNull, parsDeletedTrueFalse, parsActiveTrueNull, parsActiveTrueFalse, parsDeletedNull
  if Periodic.ActiveRecordSign = parsDeletedNull then
    Result := AActiveField.IsNull
  else
    Result := ((Periodic.ActiveRecordSign in [parsDeletedTrueNull, parsDeletedTrueFalse]) = AActiveField.AsBoolean);
end;

procedure TDBPeriodicTabControl.ApplyRecord(AFields: TFields; AApplyAction: TdhApplyAction);
type
  TLocalAction = (laAsIs, laInsertBefore, laInsertUpdate, laUpdateWithMarkAsDeleted, laInsertBeforeWithMarkAsDeleted, laUpdateMove);
var
  LDataHandlerDataSetProvider: TSQLDataSetProvider;
  LBookmark: TDataSourceStateBookmark;
  LUnPeriodicFields: String;
  LAppendPrivateFieldNames, LEditPrivateFieldNames: String;

  DKeyFieldValues, LKeyFieldValues: Variant;
  LUniqueFieldValues: Variant;
  DIdentityFieldValues, LIdentityFieldValues: Variant;
  LUnPeriodicFieldValues: Variant;

  DPeriodicDate: TDate;

  LKeyValuesUpdated, LUnPeriodicFieldValuesUpdated, LUniqueValuesFound, LUniqueValuesPresent: Boolean;
  I: Integer;
  LDateField, LActiveField, DActiveField: TField;
  LAction: TLocalAction;

{
  GUId - KeyField
  Id - Identity
  Unique - Link_Id
}

  procedure Step1_FindUniqueValues;
  var
    LRestoredFieldNames: String;

  begin
    LUniqueFieldValues := AFields.Values[Periodic.UniqueFieldNames];
    // Если заданы непустые значения уникальных полей (Link_Id)
    if VarArrayIsPresent(LUniqueFieldValues) then begin
      LUniqueValuesPresent := True;
      if not Periodic.NonUniqueDataFieldNames.IsEmpty then
        // Стираем значения
        AFields.Values[Periodic.NonUniqueDataFieldNames] := TNamedVariants.Create(True)[Periodic.NonUniqueDataFieldNames];

      // Если есть похожие записи в периодике
      LUniqueValuesFound := LDataHandlerDataSetProvider.Locate(Periodic.UniqueFieldNames, LUniqueFieldValues, []);
      if LUniqueValuesFound then begin
        LKeyFieldValues := LDataHandlerDataSetProvider.FieldValues[Periodic.KeyFieldNames];
        AFields.Values[Periodic.KeyFieldNames] := LKeyFieldValues;

        if not Periodic.IdentityFieldName.IsEmpty then begin
          LIdentityFieldValues := LDataHandlerDataSetProvider.FieldValues[Periodic.IdentityFieldName];
          AFields.Values[Periodic.IdentityFieldName] := LIdentityFieldValues;
        end;

        if not Periodic.DeletablePrivilegeFieldName.IsEmpty then
          AFields.FieldByName(Periodic.DeletablePrivilegeFieldName).AsBoolean := LDataHandlerDataSetProvider.FieldByName(Periodic.DeletablePrivilegeFieldName).AsBoolean;

        if not Periodic.ModifiAbleFieldsFieldName.IsEmpty then
          AFields.FieldByName(Periodic.ModifiAbleFieldsFieldName).AsVariant := LDataHandlerDataSetProvider.FieldByName(Periodic.ModifiAbleFieldsFieldName).AsVariant;

        LRestoredFieldNames := ArraysAntiJoin
                              (
                                AFields.All([afData, afReadOnly]),
                                Periodic.BasicServiceFieldNames,
                                ';'
                              );

        if not LRestoredFieldNames.IsEmpty then begin
          with TSQLDataSetProvider(AFields.DataSet.Source) do begin
            FieldsAutomations.BeginUpdate;
            try
              AFields.SetAllEditable;
              FieldValues[LRestoredFieldNames] := LDataHandlerDataSetProvider.FieldValues[LRestoredFieldNames];
            finally
              FieldsAutomations.EndUpdate;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure Step2_AppendWhenKeyFieldValuesFound;
  begin
    if (not LUniqueValuesFound) and (not Periodic.IdentityFieldName.IsEmpty) then begin
      LIdentityFieldValues := LDataHandlerDataSetProvider.FieldValues[Periodic.IdentityFieldName];
      AFields.Values[Periodic.IdentityFieldName] := LIdentityFieldValues;
    end;

    repeat
    {
      Abstract(Periodic.IdentityFieldName.IsEmpty or VarIsEqual(LIdentityFieldValues, LDataHandlerDataSetProvider.FieldValues[Periodic.IdentityFieldName]), 'IdentityFieldValues difference')
    }

      if (AApplyAction = dhaaAppendRecord) then begin
        // Вставка невозможна, если дубликаты запрещены,
        // либо на текущую дату идёт попытка объединения с активной записью или попытка вставить неактивную запись
        if
        (
          // (Periodic.KeyChangeAction in [pkcaError, pkcaMoveRecord])
          (LDateField.AsDateTime < DPeriodicDate) and Periodic.ActiveFieldName.IsEmpty
          or
          ((LDateField.AsDateTime = DPeriodicDate) and (Periodic.ActiveFieldName.IsEmpty or not CheckDeleted(LActiveField) or CheckDeleted(DActiveField)))
        )
        then
          Exception.CreateFmt(SBasePeriodicTabControlProperties.ErrorDublicateKeysFmt, [DateToStr(LDateField.AsDateTime)]);

        if LDateField.AsDateTime <= DPeriodicDate then
          LAction := laInsertUpdate
        else if LAction = laAsIs then
          LAction := laInsertBefore
      end else begin { if (AApplyAction = dhaaUpdateRecord) }
        // Апдейт невозможен, если поменялись KeyValues
        // и он проходит в вариации pdaError, pdaMoveRecord
        // либо на текущую дату идёт попытка объединения с активной записью или попытка вставить неактивную запись
        if
        (
          (Periodic.KeyChangeAction in [pkcaError, pkcaMoveRecord])
          or
          ((LDateField.AsDateTime = DPeriodicDate) and (not CheckDeleted(LActiveField) or CheckDeleted(DActiveField)))
        )
        then
          Raise Exception.CreateFmt(SBasePeriodicTabControlProperties.ErrorDublicateKeysFmt, [DateToStr(LDateField.AsDateTime)]);

        if LDateField.AsDateTime <= DPeriodicDate then
          LAction := laUpdateWithMarkAsDeleted
      end;
    until (not LDataHandlerDataSetProvider.LocateEx(Periodic.KeyFieldNames, LKeyFieldValues, [bloNext]));
  end;

begin
{$IFDEF DEBUG}
  // Не актуально для "базовой периодики главной записи"
  Assert((not Periodic.DefaultPeriodic) and Assigned(FPeriodic.DataHandler));
  // Не совместимые значения настроек
  Assert((Periodic.KeyChangeAction <> pkcaMarkOldValueAsDeleted) or (Periodic.Filling <> pfRandom));
{$ENDIF}

  LDataHandlerDataSetProvider := FPeriodic.DataHandler.DataSet;
  LDateField := LDataHandlerDataSetProvider.FieldByName(Periodic.DateFieldName);
  DPeriodicDate := AFields.FieldByName(Periodic.DateFieldName).AsDateTime;

  LAppendPrivateFieldNames := ConcatIfNotEmpty
                        (
                          [
                            LDataHandlerDataSetProvider.DetailFields,
                            FPeriodic.PrivateFieldNames
                          ],
                          ';'
                        );

  LEditPrivateFieldNames := ConcatIfNotEmpty
                        (
                          [
                            LAppendPrivateFieldNames,
                            FPeriodic.ModifiAbleFieldsFieldName,
                            FPeriodic.DeletablePrivilegeFieldName
                          ],
                          ';'
                        );

{$IFDEF DEBUG}
  Assert(DPeriodicDate = ActiveDate);
  Assert((AApplyAction = dhaaAppendRecord) or ((AApplyAction = dhaaUpdateRecord) and (DPeriodicDate = FBeforeEditDate)));
  Assert(not ArraysPositiveJoin(LDataHandlerDataSetProvider.DetailFields, Periodic.StaticFieldNames), 'DetailFields * StaticFieldNames <> []');
{$ENDIF}

  if not Periodic.ActiveFieldName.IsEmpty then begin
    LActiveField := LDataHandlerDataSetProvider.FieldByName(Periodic.ActiveFieldName);
    DActiveField := AFields.FieldByName(Periodic.ActiveFieldName);
  end else begin
    Assert(Periodic.KeyChangeAction <> pkcaMarkOldValueAsDeleted);
    LActiveField := nil;
    DActiveField := nil;
  end;

  DKeyFieldValues := AFields.Values[Periodic.KeyFieldNames];
  if not Periodic.IdentityFieldName.IsEmpty then
    DIdentityFieldValues := AFields.Values[Periodic.IdentityFieldName]
  else
    DIdentityFieldValues := Null;

  LAction := laAsIs;

  LDataHandlerDataSetProvider.BeginUpdate(LBookmark);
  try try
    LDataHandlerDataSetProvider.UpdateStatusFilter := Default_UpdateStatusFilter;
    LDataHandlerDataSetProvider.Filtered := False;

    if (AApplyAction = dhaaUpdateRecord)
        and not LDataHandlerDataSetProvider.Locate(Periodic.KeyFieldNames + ';' + Periodic.DateFieldName, VarArraysConcat(FBeforeEditKeyFieldValues, DPeriodicDate), [])
    then
      SBaseProperties.RaiseTechnicalError(SMessageOriginalRecordNotFoundError);

    // Step1: Если есть уникальные поля
    LUniqueValuesFound := False;
    LUniqueValuesPresent := False;
    if not Periodic.UniqueFieldNames.IsEmpty then
      Step1_FindUniqueValues;

    // Если дубликатов нету то берём собственный ключ
    if not LUniqueValuesFound then begin
      LKeyFieldValues := AFields.Values[Periodic.KeyFieldNames];
      if not Periodic.IdentityFieldName.IsEmpty then
        LIdentityFieldValues := AFields.Values[Periodic.IdentityFieldName]
    end;

    // Без уникального ключа, хотябы временного - жизни нет
    if not VarIsPresent(LKeyFieldValues) then
      SBaseProperties.RaiseTechnicalError(SMessageKeyNotPresentError);

    LKeyValuesUpdated := (AApplyAction = dhaaUpdateRecord) and not VarIsEqual(FBeforeEditKeyFieldValues, LKeyFieldValues);
    if LKeyValuesUpdated then
      case Periodic.KeyChangeAction of
        pkcaError: SBaseProperties.RaiseTechnicalError(SMessageKeyCannotBeChanged);
        pkcaMarkOldValueAsDeleted: LAction := laInsertBeforeWithMarkAsDeleted;
      end;

    // Step2: Ищем другие дубликаты при добавлении или изменении KeyFieldValues
    if ((AApplyAction = dhaaAppendRecord) or LKeyValuesUpdated)
        and LDataHandlerDataSetProvider.Locate(Periodic.KeyFieldNames, LKeyFieldValues, [])
    then
      Step2_AppendWhenKeyFieldValuesFound;

    LUnPeriodicFields := ArraysMerge([FPeriodic.StaticFieldNames, FPeriodic.IdentityFieldName], ';');
    if (not FPeriodic.NonUniqueDataFieldNames.IsEmpty) and (not LUniqueValuesPresent) then
      LUnPeriodicFields := ArraysMerge([LUnPeriodicFields, FPeriodic.NonUniqueDataFieldNames], ';');

    if not LUnPeriodicFields.IsEmpty then
      LUnPeriodicFieldValues := AFields.Values[LUnPeriodicFields];

    if LKeyValuesUpdated and (Periodic.KeyChangeAction = pkcaMoveRecord) then begin
      // LAction = laUpdateMove
      LDataHandlerDataSetProvider.Locate(Periodic.KeyFieldNames, FBeforeEditKeyFieldValues, []);
      repeat
        LDataHandlerDataSetProvider.Edit;
        if LDateField.AsDateTime = DPeriodicDate then begin
          LDataHandlerDataSetProvider.Fields.Load(AFields, lvcAlways, LEditPrivateFieldNames, True);
        end else begin
          LDataHandlerDataSetProvider.FieldValues[Periodic.KeyFieldNames] := LKeyFieldValues;
          if not LUnPeriodicFields.IsEmpty then
            LDataHandlerDataSetProvider.FieldValues[LUnPeriodicFields] := LUnPeriodicFieldValues;
        end;

        LDataHandlerDataSetProvider.Post;
      until (not LDataHandlerDataSetProvider.LocateEx(Periodic.KeyFieldNames, FBeforeEditKeyFieldValues, [bloNext]));
      AApplyAction := dhaaNone;
    end else if LAction = laInsertUpdate then begin
      LAction := laAsIs;
      AApplyAction := dhaaUpdateRecord;
      FBeforeEditKeyFieldValues := LKeyFieldValues;
    end;

    if AApplyAction = dhaaUpdateRecord then begin
      Assert(LDataHandlerDataSetProvider.Locate(Periodic.KeyFieldNames + ';' + Periodic.DateFieldName, VarArraysConcat(FBeforeEditKeyFieldValues, DPeriodicDate), []));
      if LAction = laUpdateWithMarkAsDeleted then with LDataHandlerDataSetProvider do begin
        UpdateDeleted(LActiveField, True); Post;
        FBeforeEditKeyFieldValues := LKeyFieldValues;
        Assert(LDataHandlerDataSetProvider.Locate(Periodic.KeyFieldNames + ';' + Periodic.DateFieldName, VarArraysConcat(LKeyFieldValues, DPeriodicDate), []));
      end else if LAction = laInsertBeforeWithMarkAsDeleted then with LDataHandlerDataSetProvider do begin
        UpdateDeleted(LActiveField, True); Post;
        LAction := laInsertBefore;
      end;
    end;

    if LAction = laInsertBefore then begin
      for I := TabIndex to Tabs.Count - 2 do begin
        if LDataHandlerDataSetProvider.Locate(Periodic.KeyFieldNames + ';' + Periodic.DateFieldName, VarArraysConcat(LKeyFieldValues, Dates[I]), []) then begin
          if (not LUnPeriodicFields.IsEmpty) then with LDataHandlerDataSetProvider do begin
            Edit;
              FieldValues[LUnPeriodicFields] := LUnPeriodicFieldValues;
            Post;
          end;
        end else with LDataHandlerDataSetProvider do begin
          Append;
            LDateField.AsDateTime := Dates[I];
            Fields.Load(AFields, lvcIfNotPresent, LAppendPrivateFieldNames);
            // В режиме Filling = pfRandom нужно сделать так, чтобы записи не виднелись на других закладках
            if (Periodic.Filling = pfRandom) and (I > TabIndex) then UpdateDeleted(LActiveField, True);
          Post;
        end;
      end;
      AApplyAction := dhaaNone;
    end;

    case AApplyAction of
      dhaaAppendRecord:
        with LDataHandlerDataSetProvider do begin
          for I := TabIndex to Tabs.Count - 2 do begin
            Append;
              LDateField.AsDateTime := Dates[I];
              Fields.Load(AFields, lvcIfNotPresent, LAppendPrivateFieldNames);
            Post;
          end;
          // AApplyAction := dhaaNone; // Конец!
        end;
      dhaaUpdateRecord:
        with LDataHandlerDataSetProvider do begin
          LUnPeriodicFieldValuesUpdated := (not LUnPeriodicFields.IsEmpty) and not VarIsEqual(LUnPeriodicFieldValues, FieldValues[LUnPeriodicFields]);
          Edit;
            Fields.Load(AFields, lvcAlways, LEditPrivateFieldNames);
          Post;
          if LUnPeriodicFieldValuesUpdated then begin
            Assert(Locate(Periodic.KeyFieldNames, LKeyFieldValues, []));
            repeat
              if LDateField.AsDateTime <> DPeriodicDate then begin
                Edit;
                  FieldValues[Periodic.KeyFieldNames] := LKeyFieldValues;
                  FieldValues[LUnPeriodicFields] := LUnPeriodicFieldValues;
                Post;
              end;
            until (not LDataHandlerDataSetProvider.LocateEx(Periodic.KeyFieldNames, LKeyFieldValues, [bloNext]));
          end;
          // AApplyAction := dhaaNone; // Конец!
        end;
    end;
  except
    AFields.Values[Periodic.KeyFieldNames] := DKeyFieldValues;
    if not Periodic.IdentityFieldName.IsEmpty then
      AFields.Values[Periodic.IdentityFieldName] := DIdentityFieldValues;
    Raise;
  end
  finally
    LDataHandlerDataSetProvider.EndUpdate(LBookmark, [sbrFiltered, sbrFilter, sbrDisableCount], [sbrKeyFieldValues, sbrRecordIndex]);
  end;
end;

procedure TDBPeriodicTabControl.DataHandlerOnMemoryApply(ADataHandler: TCustomDataHandler; ADataSet: TDataSet; var AApplyAction: TdhApplyAction);
begin
  if Assigned(FOnMemoryApply) then
    FOnMemoryApply(ADataHandler, ADataSet, AApplyAction);

  ApplyRecord(ADataSet.Fields, AApplyAction);
  if Assigned(FAfterApply) then begin
    FAfterApply(Self, ADataSet.Fields, AApplyAction);
  end;
  AApplyAction := dhaaNone;
end;

procedure TDBPeriodicTabControl.DataHandlerOnMemoryPrepare(ADataHandler: TCustomDataHandler; ADataSet: TSQLDataSetProvider);
begin
  if not FPeriodic.FModifiAbleFieldsFieldName.IsEmpty then
    ADataSet.FieldsAutomations.ModifiAbleFieldsFieldName := FPeriodic.FModifiAbleFieldsFieldName;
  ADataSet.FieldsAutomations.AddServiceFields(FPeriodic.DeletablePrivilegeFieldName);
end;

procedure TDBPeriodicTabControl.DataHandlerXMLPostOnBeforeApply(ASubDataHandler: TSubDataHandler; var AXML: String; var ADone: Boolean);
var
  I: Integer;
  LDataHandler        : TSubDataHandler;
  LParentDataHandler  : TcustomDataHandler;
  LDataSet            : TDataSet;
  LDataSetProvider    : TSQLDataSetProvider absolute LDataSet;
  LParentDataSet      : TDataSet;
  LParentDataSetProvider: TSQLDataSetProvider absolute LParentDataSet;
  LDataSourceStateBookmark: TDataSourceStateBookmark;
  LIdentityField      : TField;
  LDateField          : TField;
  LOldRows, LNewRows  : TPeriodicRows;
  LNewRow, LOldRow    : PPeriodicRow;
//  LKeyFieldNames      : String;
  LExcludedKeyFieldNames: String;
  LExcludedFields     : String;
  LStoredStaticFields   : String;
  LStoredPeriodicFields : String;
  LXMLPostUpdateStatusFilter: TUpdateStatusSet;
begin
  ADone := True;
  AXML  := '';

  LDataHandler        := FPeriodic.DataHandler;
  LParentDataHandler  := LDataHandler.Parent;
  if (LParentDataHandler is TSubDataHandler) and FPeriodic.DefaultPeriodic then
    with TSubDataHandlerXMLPostPropertiesCrack(TSubDataHandler(LParentDataHandler).XMLPost) do
      ExcludedInProcessFields :=
        ArraysMerge
        (
          [
            ExcludedInProcessFields,
            StringReplace(FPeriodic.StaticFieldNames, ';', ',', [rfReplaceAll, rfIgnoreCase])
          ],
          ','
        );

  LDataSet        := LDataHandler.DataSet;
  LParentDataSet  := LParentDataHandler.DataSet;
  if LParentDataSet.UpdateStatus = usDeleted then Exit;

  if LDataHandler.XMLPost.OnlyIfModified then
    if not
      (
        LDataSet.Editing
        or
        LDataSetProvider.UpdatesPending
      )
    then with LParentDataSetProvider do
      if (not FPeriodic.DefaultPeriodic)
          or
          not (
                Editing
                or LDataSetProvider.UpdatesPending
                or ((FPeriodic.StaticFieldNames <> '') and UpdatesPending)
              )
      then
        Exit;

  LOldRows.Clear;
  LNewRows.Clear;

  LExcludedFields         := FPeriodic.AllExcludedFieldNames;
  LExcludedKeyFieldNames  := ArraysJoin(FPeriodic.KeyFieldNames, LExcludedFields, ';');

  LStoredStaticFields   := ArraysAntiJoin(FPeriodic.ObservedStaticFieldNames, LExcludedFields, ';');
  LStoredPeriodicFields := ArraysAntiJoin(LDataSet.Fields.All, [FPeriodic.ObservedNonPeriodicFieldNames, LExcludedFields], ';');

  LXMLPostUpdateStatusFilter := LDataHandler.XMLPost.UpdateStatusFilter;
  if LXMLPostUpdateStatusFilter = [] then
    LXMLPostUpdateStatusFilter := Default_UpdateStatusFilter;

  //-- Заполняем список дат из датасета
  LDataSet.BeginUpdate(LDataSourceStateBookmark);
  try
{
    if ASubDataHandler.XMLPost.Filter <> '' then begin
      LDataSet.Filter := ASubDataHandler.XMLPost.Filter;
      LDataSet.Filtered := True;
    end else
} // Реализовано через EvaluateBoolean
    LDataSet.Filtered := False;
                                              
    LDataSetProvider.UpdateStatusFilter := All_UpdateStatusFilter;

    LDataSet.First;
    LDateField := LDataSet.FieldByName(FPeriodic.DateFieldName);
    if not FPeriodic.IdentityFieldName.IsEmpty then
      LIdentityField := LDataSet.FieldByName(FPeriodic.IdentityFieldName)
    else
      LIdentityField := nil;

    if FPeriodic.DefaultPeriodic and (not LStoredStaticFields.IsEmpty) then begin
      if (not LParentDataSet.Inserted) then
        LOldRows.FindRow(KeyValue, True).StaticFieldValues[LStoredStaticFields] := LParentDataSet.FieldOldValues[LStoredStaticFields];
      LNewRows.FindRow(KeyValue, True).StaticFieldValues[LStoredStaticFields] := LParentDataSet.FieldValues[LStoredStaticFields];
      LStoredStaticFields := '';
    end;

    while not LDataSet.Eof do begin
      if (LDataSet.UpdateStatus <> usInserted) then begin
        Assert
        (
          not Assigned(LIdentityField)
          or VarIsEqual(LIdentityField.OldValue, LIdentityField.AsVariant)
          // not VarIsPresent(LIdentityField.OldValue) or (not LIdentityField.IsNull)
          , 'IdentityField cannot be changed'
        );

        if
          // (not LParentDataSet.Inserted) and
          (not Assigned(LIdentityField) or VarIsPresent(LIdentityField.OldValue))
          and VarIsPresent(LDateField.OldValue)
          and LDataSet.EvaluateBoolean(LDataHandler.XMLPost.Filter, True, True)
        then
          LOldRows.Load(LDataSet.Fields, LDateField, FPeriodic.KeyFieldNames, LStoredStaticFields, LStoredPeriodicFields, True);
      end;


      if (LDataSet.UpdateStatus <> usDeleted)
        and (not LDateField.IsNull)
        and LDataSet.EvaluateBoolean(LDataHandler.XMLPost.Filter, False, True)
      then
        LNewRows.Load(LDataSet.Fields, LDateField, FPeriodic.KeyFieldNames, LStoredStaticFields, LStoredPeriodicFields, False);
      LDataSet.Next;
    end;
  finally
    LDataSet.EndUpdate(LDataSourceStateBookmark);
  end;

  if (not FPeriodic.ActiveFieldName.IsEmpty) then begin
    LOldRows.DeleteEmptyRecords(FPeriodic.ActiveFieldName, FPeriodic.ActiveRecordSign);
    LNewRows.DeleteEmptyRecords(FPeriodic.ActiveFieldName, FPeriodic.ActiveRecordSign);
  end;

  for I := 0 to LNewRows.Count - 1 do begin
    LNewRow := LNewRows.Rows[I];
    LOldRow := LOldRows.FindRow(LNewRow.KeyValue);

    AXML := ConcatIfNotEmpty
            (
              AXML,
              SConst_CR,
              LNewRow.UpdateXML
              (
                LOldRow,
                FPeriodic.KeyFieldNames,
                FPeriodic.IdentityFieldName,
                LExcludedKeyFieldNames,
                LDataHandler.XMLPost.StatusAttributeName,
                LXMLPostUpdateStatusFilter,
                FPeriodic.PostAllFields
              )
            );
  end;

  if (LDataHandler.XMLPost.StatusAttributeName <> '') and (usDeleted in LXMLPostUpdateStatusFilter) then
    for I := 0 to LOldRows.Count - 1 do begin
      LOldRow := LOldRows.Rows[I];
      if LNewRows.FindRow(LOldRow.KeyValue) = nil then
        AXML := ConcatIfNotEmpty(AXML, SConst_CR, LOldRow.DeleteXML(FPeriodic.KeyFieldNames, FPeriodic.IdentityFieldName, LExcludedKeyFieldNames, LDataHandler.XMLPost.StatusAttributeName));
    end;
end;
{$ENDIF}

//-- Процедура изменения даты закладки
procedure TDBPeriodicTabControl.ChangeDate;
{$IFNDEF PACKAGE}
var
  LVariant: TNamedVariants;
  LDate, LOldDate: TDate;
  LKeyValue: Variant;
  LAllow, LKeepEditMode: Boolean;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  DataSet.DisableControls;
  try
    //-- Текущая дата
    LOldDate := FDateList[TabIndex];

    //-- Выбираем новую дату
    LVariant := TSBaseSelectDateFrm.Select
                (
                  Self,
                  [
                    TNamedVariant.Create(SConst_Default, LOldDate)
                  ]
                );

    // Положительный выбор
    if (LVariant.Count > 0) then begin
      LDate := VarToDateTime(LVariant[SConst_Value]);
      if LOldDate = LDate then Exit;

      if FDateList.IndexOf(LDate) <> -1 then
        TSBaseFrm(nil).WarningMessage(Format(SBasePeriodicTabControlProperties.ErrorDublicateDateFmt, [DateToStr(LDate)]), Self);

      if (
          (TabIndex > 0) and (FDateList[TabIndex - 1] > LDate)
          or
          (TabIndex < Tabs.Count - 2) and (FDateList[TabIndex + 1] < LDate)
         )
      then
        TSBaseFrm(nil).WarningMessage(SBasePeriodicTabControlProperties.ErrorNearestPeriodFmt, Self);

      if (TabIndex = 0) then
        if Assigned(FAgeChanging) then begin
          LAllow := True;
          FAgeChanging(Self, LDate, LAllow);
          if not LAllow then Exit;;
        end;

      //LOldDate := FDateList[TabIndex];
      //-- Обновляем список закладок
      FDateList[TabIndex] := LDate;
      //-- Обновляем дату у записей
      with DataSet do begin
        //-- Если указано ключевое поле, то запоминаем ключевое значение
        if not FPeriodic.KeyFieldNames.IsEmpty then
          LKeyValue := FieldValues[FPeriodic.KeyFieldNames];

        Filter := FPeriodic.DateFieldName + ' = ''' + DateToStr(LOldDate) + '''';
        LKeepEditMode := KeepEditMode;

        while not Eof do begin
          Edit;
            FieldByName(FPeriodic.DateFieldName).AsDateTime := LDate;
          Post;
        end;
      end;

      LockOnDataCommitEvent;
      try
        //-- Обновляем закладки
        Refresh;
        //-- Контроль за полем PeriodicAgeFieldName
        CheckPeriodicAgeField;
        //-- Позиционируемся на текущей
        SetTabIndex(FDateList.IndexOf(LDate));

        //-- Позиционируемся на ключевое значение (если оно есть)
        if LKeepEditMode then
          DataSet.Edit
        else if (not FPeriodic.KeyFieldNames.IsEmpty) and VarIsPresent(LKeyValue) then
          DataSet.Locate(FPeriodic.KeyFieldNames, LKeyValue, []);
      finally
        UnLockOnDataCommitEvent;
      end;
    end;
  finally
    DataSet.EnableControls;
  end;
{$ENDIF}
end;

procedure TDBPeriodicTabControl.CheckPeriodicAgeField;
{$IFNDEF PACKAGE}
var
  LDataSet  : TDataSet;
  LField    : TField;
  LDate     : TDate;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  if (FPeriodic.AgeFieldName.IsEmpty) or (DatesCount = 0) then
    Exit;

  LDate := GetDate(0);

  LDataSet := FPeriodic.DataHandler.Parent.DataSet;
  LField := LDataSet.FieldByName(FPeriodic.AgeFieldName);

  if (LField.CanModify) and ((not VarIsPresent(LField.Value)) or (LField.AsDateTime <> LDate)) then begin
    LDataSet.Edit;
    LField.AsDateTime := LDate;
  end;
{$ENDIF}
end;

{ TSBasePeriodicTabControlProperties }

//constructor TSBasePeriodicTabControlProperties.Create;
//begin
//  inherited Create;
//
//  FMessageDublicateError  := SMessageFormatDublicateError;
//  FMessagePeriodError     := SMessagePeriodError;
//  FMessageFirstDateError  := SMessageEarlierThenAgeError;
//  FMessageFutureDateError := SMessageFutureDateError;
//
//  FQuestionDelete         := SQuestionFormatDelete;
//
//  FCaptionMenuAdd         := SCaptionMenuAdd;
//  FCaptionMenuChangeDate  := SCaptionMenuChangeDate;
//  FCaptionMenuDelete      := SCaptionMenuDelete;
//end;

//procedure TSBasePeriodicTabControlProperties.SetCaptionMenuAdd(const Value: String);
//begin
//  if Value = ''  then
//    raise Exception.Create('Не указано значение')
//  else
//    FCaptionMenuAdd := Value;
//end;
//
//procedure TSBasePeriodicTabControlProperties.SetCaptionMenuChangeDate(const Value: String);
//begin
//  if Value = ''  then
//    raise Exception.Create('Не указано значение')
//  else
//    FCaptionMenuChangeDate := Value;
//end;
//
//procedure TSBasePeriodicTabControlProperties.SetCaptionMenuDelete(const Value: String);
//begin
//  if Value = ''  then
//    raise Exception.Create('Не указано значение')
//  else
//    FCaptionMenuDelete := Value;
//end;
//
//procedure TSBasePeriodicTabControlProperties.SetMessageDublicateError(const Value: String);
//begin
//  if Pos('%s', Value) = 0  then
//    raise Exception.Create('Значение должно содержать выражение "%s", так как является форматированной строкой')
//  else
//    FMessageDublicateError := Value;
//end;
//
//procedure TSBasePeriodicTabControlProperties.SetQuestionDelete(const Value: String);
//begin
//  if Pos('%s', Value) = 0  then
//    raise Exception.Create('Значение должно содержать выражение "%s", так как является форматированной строкой')
//  else
//    FQuestionDelete := Value;
//end;

{$IFDEF FASTSCRIPT_RTTI}
{ TFunctions_TDBPeriodicTabControl }

constructor TFunctions_TDBPeriodicTabControl.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TDBPeriodicTabControl, TDBPeriodicTabControl.ClassParent.ClassName) do begin
    AddProperty('DatesCount', DelphiTypeInteger, Get_TDBPeriodicTabControl_Property);
    AddProperty('ActiveDate', DelphiTypeVariant, Get_TDBPeriodicTabControl_Property, Set_TDBPeriodicTabControl_Property);
    AddProperty('LastDateActive', DelphiTypeBoolean, Get_TDBPeriodicTabControl_Property);

{
    AddMethod('procedure Open', Call_TDBPeriodicTabControl_Method);
    AddMethod('procedure AddDate', Call_TDBPeriodicTabControl_Method);
    AddMethod('procedure ChangeDate', Call_TDBPeriodicTabControl_Method);
    AddMethod('procedure DeleteDate', Call_TDBPeriodicTabControl_Method);
    AddMethod('procedure UpdateRecord(AKeyValue: Variant; AUnPeriodicFields: String = '''')', Call_TDBPeriodicTabControl_Method);
    AddMethod('procedure NewRecord(AKeyValue: Variant)', Call_TDBPeriodicTabControl_Method);
}

    AddMethod('procedure ApplyRecord(AFields: TFields; AApplyAction: TdhApplyAction)', Call_TDBPeriodicTabControl_Method);
    AddMethod('procedure DeleteRecord', Call_TDBPeriodicTabControl_Method);
    AddMethod('procedure DeleteRecordHistory(AToDate: TDate)', Call_TDBPeriodicTabControl_Method);
    AddMethod('procedure EditDate(AOldDate : TDate; ANewDate : TDate; AWithACheck : Boolean := True)', Call_TDBPeriodicTabControl_Method);
    AddMethod('procedure ReOpen', Call_TDBPeriodicTabControl_Method);

    AddEvent('OnValidate', TfsDBPeriodicTabControlAllowEvent);
    AddEvent('OnCanChangeAge', TfsDBPeriodicTabControlAllowDateEvent);
    AddEvent('OnActiveDateChanged', TfsNotifyEvent);
    AddEvent('AfterApply', TfsDBPeriodicTabControlAfterApply);

    AddIndexProperty('Dates', DelphiTypeInteger, 'TDate', Call_TDBPeriodicTabControl_Method, False);
  end;
end;

function  TFunctions_TDBPeriodicTabControl.Get_TDBPeriodicTabControl_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'ACTIVEDATE' then
    Result := TDBPeriodicTabControl(Instance).ActiveDate
  else if PropName = 'DATESCOUNT' then
    Result := TDBPeriodicTabControl(Instance).DatesCount
  else if PropName = 'LASTDATEACTIVE' then
    with TDBPeriodicTabControl(Instance) do
      Result := (TabIndex = (Tabs.Count - 2));
end;

procedure TFunctions_TDBPeriodicTabControl.Set_TDBPeriodicTabControl_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
  if PropName = 'ACTIVEDATE' then
    TDBPeriodicTabControl(Instance).ActiveDate := Value;
end;

function TFunctions_TDBPeriodicTabControl.Call_TDBPeriodicTabControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if ClassType = TDBPeriodicTabControl then begin
{
    if MethodName = 'OPEN' then
      TDBPeriodicTabControl(Instance).Open
    else if MethodName = 'ADDDATE' then
      TDBPeriodicTabControl(Instance).AddDate
    else if MethodName = 'CHANGEDATE' then
      TDBPeriodicTabControl(Instance).ChangeDate
    else if MethodName = 'DELETEDATE' then
      TDBPeriodicTabControl(Instance).DeleteDate
    else if MethodName = 'NEWRECORD' then
      TDBPeriodicTabControl(Instance).NewRecord(Caller.Params[0])
    else
}
    if MethodName = 'APPLYRECORD' then
      TDBPeriodicTabControl(Instance).ApplyRecord(VarToPointer(Caller.Params[0]), Caller.Params[1])
    else if MethodName = 'DELETERECORD' then
      TDBPeriodicTabControl(Instance).DeleteRecord
    else if MethodName = 'DELETERECORDHISTORY' then
      TDBPeriodicTabControl(Instance).DeleteRecordHistory(VarToDateTime(Caller.Params[0]))
    else if MethodName = 'DATES.GET' then
      Result := TDBPeriodicTabControl(Instance).Dates[Caller.Params[0]]
    else if MethodName = 'DATES.SET' then
      TDBPeriodicTabControl(Instance).Dates[Caller.Params[0]] := Caller.Params[1]
    else if MethodName = 'EDITDATE' then
      TDBPeriodicTabControl(Instance).EditDate(Caller.Params[0], Caller.Params[1], Caller.Params[2])
    else if MethodName = 'REOPEN' then
      TDBPeriodicTabControl(Instance).ReOpen
  end;
end;

{ TfsDBPeriodicTabControlAllowEvent }
procedure TfsDBPeriodicTabControlAllowEvent.DoEvent(APeriodicControl: TDBPeriodicTabControl; var AAllow: Boolean);
begin
  CallHandler([APeriodicControl, AAllow]);
  AAllow := (Handler.Params[1].Value = True);
end;

function TfsDBPeriodicTabControlAllowEvent.GetMethod: Pointer;
begin
  Result := @TfsDBPeriodicTabControlAllowEvent.DoEvent;
end;

{ TfsDBPeriodicTabControlAllowDateEvent }
procedure TfsDBPeriodicTabControlAllowDateEvent.DoEvent(APeriodicControl: TDBPeriodicTabControl; ADate: TDate; var AAllow: Boolean);
begin
  CallHandler([APeriodicControl, ADate, AAllow]);
  AAllow := (Handler.Params[2].Value = True);
end;

function TfsDBPeriodicTabControlAllowDateEvent.GetMethod: Pointer;
begin
  Result := @TfsDBPeriodicTabControlAllowDateEvent.DoEvent;
end;

{ TfsDBPeriodicTabControlAfterApply }
procedure TfsDBPeriodicTabControlAfterApply.DoEvent(APeriodicControl: TDBPeriodicTabControl; AFields: TFields; AApplyAction: TdhApplyAction);
begin
  CallHandler([APeriodicControl, Integer(AFields), Variant(AApplyAction)]);
end;

function TfsDBPeriodicTabControlAfterApply.GetMethod: Pointer;
begin
  Result := @TfsDBPeriodicTabControlAfterApply.DoEvent;
end;
{$ENDIF}

initialization
{$IFNDEF PACKAGE}
  SBasePeriodicTabControlProperties := TSBasePeriodicTabControlProperties.Create(Application);
  SBasePeriodicTabControlProperties.Name := 'SBasePeriodicTabControlProperties';
{$ENDIF}

  RegisterClasses([TSBasePeriodicTabControlProperties, TDBPeriodicTabControl, TDBPeriodicTabControlPeriodic]);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TFunctions_TDBPeriodicTabControl);
{$ENDIF}


finalization
  UnRegisterClasses([TSBasePeriodicTabControlProperties, TDBPeriodicTabControlPeriodic, TDBPeriodicTabControl]);

end.


