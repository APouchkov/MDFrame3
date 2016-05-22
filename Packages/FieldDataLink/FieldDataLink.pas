unit FieldDataLink;
{$i config.inc}

interface

{
Х deFieldChange     Ч изменилось значение пол€ = TField.DataChanged
Х deRecordChange    Ч изменилось содержимое текущей записи = Edit, ClearFields
Х deDataSetChange   Ч изменилось содержимое набора данных (вставлена или удалена запись) = TField.PropertyChanged, EnableControls, Resync, First, Last, MoveBy, EndInsertAppend
Х deDataSetScroll   Ч был прокручен набор отображаемых записей;
Х deLayoutChange    Ч изменилось представление данных в компоненте;
Х deUpdateRecord    Ч произошло подтверждение изменени€ текущей записи = UpdateRecord(Post, CheckBrowseMode)
Х deUpdateState     Ч изменилось состо€ние набора данных;
Х deCheckBrowseMode Ч измен€етс€ состо€ние набора данных;
Х dePropertyChange  Ч изменилось свойство набора данных или одно из его полей;
Х deFieldListChange Ч изменилс€ список полей набора данных;
Х deFocusControl    Ч измен€етс€ фокус ввода;
Х deParentScroll    Ч прокручен главный набор данных (текущий набор Ч детальный);
Х deConnectChange   Ч изменилось соединение с удаленной базой данных;
Х deReconcileError  Ч устранена ошибка подтверждени€ изменений.
}

uses
  Types, Classes, Windows, SysUtils, DB, DBConsts, Controls, Contnrs, Variants
{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter, fs_ievents
{$ENDIF}
  , System.Generics.Collections
  , SBaseUtils, SBaseExpression
  ;

type
  TFieldsArray = TArray<TField>;
  TFieldsList = TList<TField>;

  TDataSourceDataLink = class(TDataLink)
//  strict private
//    FActive           : Boolean;
//    FActiveWasChanged : Boolean;
  private
    FOnActiveChanged    : TNotifyEvent;
    FOnDataSetReOpened  : TNotifyEvent;

    FOnDataSetChanged: TNotifyEvent;
    FOnFieldListChanged: TNotifyEvent;
    FOnRecordChanged: TNotifyEvent;
    FOnDataSetScrolled: TNotifyEvent;
    FOnEditingChanged: TNotifyEvent;

    FRecordCount: Integer;
    FOnRecordCountChanged: TNotifyEvent;

    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure CheckRecordCountChanged;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure FieldListChanged;
    procedure RecordChanged(Field: TField); override;
    procedure EditingChanged; override;
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
  public
    property DataSource: TDataSource read GetDataSource write SetDataSource;

    property OnActiveChanged: TNotifyEvent read FOnActiveChanged write FOnActiveChanged;
    property OnDataSetReOpened: TNotifyEvent read FOnDataSetReOpened write FOnDataSetReOpened;

    property OnDataSetChanged: TNotifyEvent read FOnDataSetChanged write FOnDataSetChanged;             // „то-то было (deDataSetChange, dsDataSetScrolled, dsLayoutChanged)
    property OnFieldListChanged: TNotifyEvent read FOnFieldListChanged write FOnFieldListChanged;       // »зменилс€ список полей
    property OnRecordChanged: TNotifyEvent read FOnRecordChanged write FOnRecordChanged;                // “оже что-то было
    property OnEditingChanged: TNotifyEvent read FOnEditingChanged write FOnEditingChanged;             // »зменилось состо€ние редактировани€
    property OnRecordCountChanged: TNotifyEvent read FOnRecordCountChanged write FOnRecordCountChanged; //  ол-во записей в датасете
  end;

  TMDFieldDataLink = class(TDataLink)
  strict private
    FPriorEvent         : TDataEvent;
    FFieldsInitialized  : Boolean;
  private
    FFreeNotifier   : TMDFreeNotificator;
    FDataIndependentValue: Variant;
    FFieldName      : String;
    FFieldNameCount : Integer;
    FControl        : TComponent;

    FEnabled            : Boolean;
    FMultiFields        : Boolean;
    FDataIndepended     : Boolean;
    FEditing            : Boolean;

    FOnDataChanged    : TNotifyEvent;
    FOnFieldChanged   : TNotifyEvent;
    FOnEditingChanged : TNotifyEvent;
    FOnUpdateData     : TNotifyEvent;
    FOnActiveChanged  : TNotifyEvent;

    procedure FieldFreeNotification(Sender: TComponent);
    function GetActive: Boolean;
    function GetCanModify: Boolean;
    function GetDataSetActive: Boolean;
    function GetDataSource: TDataSource;
    function GetFieldsCount: Integer;
    function GetFieldsField(Index: Integer): TField;

    procedure SetDataSource(const Value: TDataSource);
    procedure SetEditing(Value: Boolean);
    procedure ClearFields;
    procedure SetField(AValue: TFieldsList);
    procedure SetFieldName(const Value: string);
    procedure SetMultiFields(const Value: Boolean);

    function GetValue: Variant;
    function GetIsNull: Boolean;
    function GetText: String;
    procedure SetEnabled(const Value: Boolean);
  protected
    FFields   : TFieldsList;
    FModified : Boolean;

    function GetField: TField;
    function GetIsRefreshDisabled: Boolean; dynamic;
    function FieldFound(AField: TField): Boolean;

    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure DataSetChanged; override;
    procedure LayoutChanged; override;

    procedure RecordChanged(AField: TField); override;

    procedure DoOnFieldListChanged; virtual;
    procedure DoOnFieldChanged(AField: TField); virtual;
    procedure DoOnDataChanged; virtual;

    procedure FocusControl(Field: TFieldRef); override;
    procedure UpdateData; override;
    procedure UpdateDataIndepended;
    procedure UpdateField; virtual;

    procedure ResetModified;
    function  CheckValueChaged: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function Edit: Boolean;
    function FieldByName(const AFieldName: String): TField;

    procedure Modified;
    procedure DoOnValueChanged;
    procedure SetModified(Value: Boolean);
    procedure SetText(AText: String);
    procedure SetValue(AValue: Variant);
    procedure Reset;
    procedure UpdateRightToLeft;
    procedure Clear;

    property Enabled: Boolean read FEnabled write SetEnabled;
    property Active: Boolean read GetActive;
    property CanModify: Boolean read GetCanModify;
    property Control: TComponent read FControl write FControl;
    property DataIndepended: Boolean read FDataIndepended;
    property DataSetActive: Boolean read GetDataSetActive;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property Editing: Boolean read FEditing;

    property Field: TField read GetField;
    property FieldName: String read FFieldName write SetFieldName;

    property MultiFields: Boolean read FMultiFields write SetMultiFields;
    property FieldNameCount: Integer read FFieldNameCount;
    property Fields[Index: Integer]: TField read GetFieldsField;
    property FieldsCount: Integer read GetFieldsCount;

    property IsNull: Boolean read GetIsNull;

    property OnActiveChanged: TNotifyEvent read FOnActiveChanged write FOnActiveChanged;
    property OnEditingChanged: TNotifyEvent read FOnEditingChanged write FOnEditingChanged;
    property OnUpdateData: TNotifyEvent read FOnUpdateData write FOnUpdateData;

    property OnDataChanged: TNotifyEvent read FOnDataChanged write FOnDataChanged;
    property OnFieldChanged: TNotifyEvent read FOnFieldChanged write FOnFieldChanged; // ¬озможно изменили поле

    property Value: Variant read GetValue write SetValue;
    property Text: String read GetText write SetText;
  end;

  TMDFieldDataLinks = class(TObjectList<TMDFieldDataLink>)
  private
    procedure SetOnDataChanged(const Value: TNotifyEvent);
    procedure SetOnFieldChanged(const Value: TNotifyEvent);
  public
    procedure AddField(ADataSource: TDataSource; const AFieldName: String);

    property OnDataChanged: TNotifyEvent write SetOnDataChanged;
    property OnFieldChanged: TNotifyEvent write SetOnFieldChanged;
  end;

  TDataSourceDataLinkController = class(TComponent)
  strict private
    FDataLink: TDataSourceDataLink;
  private
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    function GetOnRecordChanged: TNotifyEvent;
    procedure SetOnRecordChanged(const Value: TNotifyEvent);
    function GetOnActiveChanged: TNotifyEvent;
    function GetOnDataSetChanged: TNotifyEvent;
    function GetOnFieldListChanged: TNotifyEvent;
    procedure SetOnActiveChanged(const Value: TNotifyEvent);
    procedure SetOnDataSetChanged(const Value: TNotifyEvent);
    procedure SetOnFieldListChanged(const Value: TNotifyEvent);
    function GetOnEditingChanged: TNotifyEvent;
    function GetOnRecordCountChanged: TNotifyEvent;
    procedure SetOnEditingChanged(const Value: TNotifyEvent);
    procedure SetOnRecordCountChanged(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;

    property OnActiveChanged: TNotifyEvent read GetOnActiveChanged write SetOnActiveChanged;
    property OnDataSetChanged: TNotifyEvent read GetOnDataSetChanged write SetOnDataSetChanged;
    property OnRecordChanged: TNotifyEvent read GetOnRecordChanged write SetOnRecordChanged;
    property OnFieldListChanged: TNotifyEvent read GetOnFieldListChanged write SetOnFieldListChanged;
    property OnEditingChanged: TNotifyEvent read GetOnEditingChanged write SetOnEditingChanged;
    property OnRecordCountChanged: TNotifyEvent read GetOnRecordCountChanged write SetOnRecordCountChanged;
  end;

  TFieldDataLinkController = class(TComponent)
  strict private
    FFieldDataLink: TMDFieldDataLink;
  private
    function GetDataField: String;
    function GetDataSource: TDataSource;

    function GetOnFieldChanged: TNotifyEvent;
    procedure SetOnFieldChanged(const Value: TNotifyEvent);

    procedure SetDataField(const Value: String);
    procedure SetDataSource(const Value: TDataSource);

    function GetOnDataChanged: TNotifyEvent;
    procedure SetOnDataChanged(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property FieldNames: String read GetDataField write SetDataField;

    property OnDataChanged: TNotifyEvent read GetOnDataChanged write SetOnDataChanged;
    property OnFieldChanged: TNotifyEvent read GetOnFieldChanged write SetOnFieldChanged;
  end;

{ Common functions definitions }

  procedure GetFieldsProperty(AList: TFieldsList; ADataSet: TDataSet; AControl: TComponent; const AFieldNames: String; ARaiseIfNotFound: Boolean = True); overload;
  function GetFieldsProperty(DataSet: TDataSet; Control: TComponent; const FieldNames: String): TFieldsArray; overload;
  function DBUseRightToLeftAlignment(AControl: TControl; AField: TField): Boolean;
  function OkToChangeFieldAlignment(AField: TField; Alignment: TAlignment): Boolean;

  procedure Register;

implementation

uses
  SBaseVariantFunctions, SBaseStringFunctions;

{$IFDEF FASTSCRIPT_RTTI}
type
  TFunctions_MDDataLink = class(TfsRTTIModule)
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('MD Project Tools', [TDataSourceDataLinkController, TFieldDataLinkController]);
end;

{ Common functions }

procedure GetFieldsProperty(AList: TFieldsList; ADataSet: TDataSet; AControl: TComponent; const AFieldNames: String; ARaiseIfNotFound: Boolean);
var
  LPos: Integer;
  LField: TField;
  LFieldName: String;
begin
  LPos := 1;
  while LPos <= AFieldNames.Length do begin
    LFieldName := ExtractFieldName(AFieldNames, LPos);
    LField     := ADataSet.FindField(LFieldName);
    if LField = nil then begin
      if ARaiseIfNotFound then
        DatabaseErrorFmt(SFieldNotFound, [LFieldName], AControl)
      else begin
        AList.Clear;
        Exit;
      end;
    end else if Assigned(AList) then
      AList.Add(LField);
  end;
end;

function GetFieldsProperty(DataSet: TDataSet; Control: TComponent; const FieldNames: String): TFieldsArray;
var
  FieldList: TFieldsList;
  i: Integer;
begin
  FieldList := TFieldsList.Create;
  try
    GetFieldsProperty(FieldList, DataSet, Control, FieldNames);
    SetLength(Result, FieldList.Count);
    for i := 0 to FieldList.Count - 1 do
      Result[i] := TField(FieldList[i]);
  finally
    FieldList.Free;
  end;
end;

function DBUseRightToLeftAlignment(AControl: TControl; AField: TField): Boolean;
var
  AAlignment: TAlignment;
begin
  if Assigned(AField) then
    AAlignment := AField.Alignment
  else
    AAlignment := taLeftJustify;
  Result := (SysLocale.MiddleEast) and (AControl.BiDiMode = bdRightToLeft) and
    (OkToChangeFieldAlignment(AField, AAlignment));
end;

function OkToChangeFieldAlignment(AField: TField; Alignment: TAlignment): Boolean;
begin
  { dont change the alignment for these fields:
    ftSmallInt     ftInteger      ftWord         ftFloat        ftCurrency
    ftBCD          ftDate         ftTime         ftDateTime     ftAutoInc
    ftTimeStamp    ftFMTBcd}
  if Assigned(AField) then with AField do
    Result := (DataType < ftSmallInt) or
              (DataType = ftBoolean) or
              ((DataType > ftDateTime) and (DataType <> ftAutoInc)
              and (DataType <> ftFMTBcd))
  else
    Result := Alignment <> taCenter;
end;

{ TDataSourceDataLink }

procedure TDataSourceDataLink.ActiveChanged;
begin
  inherited;

  if Assigned(FOnActiveChanged) then
    FOnActiveChanged(Self);

  if Active and Assigned(FOnDataSetReOpened) then
    FOnDataSetReOpened(Self);
end;

procedure TDataSourceDataLink.CheckRecordCountChanged;
var
  LRecordCount: Integer;
  LDataSet: TDataSet;
begin
  if not Assigned(FOnRecordCountChanged) then Exit;

  LDataSet := DataSet;
  if Assigned(LDataSet) and LDataSet.Active then begin
    LRecordCount := LDataSet.RecordCount;
//    if LDataSet.State = dsInsert then
//      Inc(LRecordCount);
  end else
    LRecordCount := 0;

  if LRecordCount <> FRecordCount then begin
    FRecordCount := LRecordCount;
    FOnRecordCountChanged(Self);
  end;
end;

procedure TDataSourceDataLink.DataEvent(Event: TDataEvent; Info: NativeInt);
{$IFNDEF PACKAGE}
var
  LEnableEvents: TDataEvents;
{$ENDIF}
begin
  inherited;
{$IFNDEF PACKAGE}
  if (Event in [deDataSetChange, deLayoutChange]) and (Info <> 0) then begin
    LEnableEvents := [];
    PWord(@LEnableEvents)^ := Info;

    if deFieldListChange in LEnableEvents then
      FieldListChanged;

    if (deDataSetChange in LEnableEvents) and Active and Assigned(FOnDataSetReOpened) then
      FOnDataSetReOpened(Self)
  end;
{$ENDIF}
end;

procedure TDataSourceDataLink.DataSetChanged;
begin
  inherited;

  if Assigned(FOnDataSetChanged) then
    FOnDataSetChanged(Self);
  CheckRecordCountChanged;
end;

procedure TDataSourceDataLink.DataSetScrolled(Distance: Integer);
begin
  inherited;

  if Assigned(FOnDataSetScrolled) then
    FOnDataSetScrolled(Self)
end;

procedure TDataSourceDataLink.EditingChanged;
begin
  inherited;

  if Assigned(FOnEditingChanged) then
    FOnEditingChanged(Self);

//  CheckRecordCountChanged;
end;

procedure TDataSourceDataLink.FieldListChanged;
begin
  if Assigned(FOnFieldListChanged) then
    FOnFieldListChanged(Self)
end;

function TDataSourceDataLink.GetDataSource: TDataSource;
begin
  Result := (inherited DataSource);
  if (Result <> nil) and (csDestroying in Result.ComponentState) then
    Result := nil;
end;

procedure TDataSourceDataLink.SetDataSource(const Value: TDataSource);
begin
  inherited DataSource := Value
end;

{
procedure TDataSourceDataLink.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent <> nil) then
    if AComponent = DataSource then
      DataSource := nil
end;
}

procedure TDataSourceDataLink.RecordChanged(Field: TField);
begin
  inherited;

  if Assigned(FOnRecordChanged) then
    FOnRecordChanged(Self)
end;

{ TMDFieldDataLink }

constructor TMDFieldDataLink.Create;
begin
  FEnabled := True;
  FFields := TFieldsList.Create;
  FFreeNotifier := TMDFreeNotificator.Create(nil);
  FFreeNotifier.OnFreeNotification := FieldFreeNotification;
  FPriorEvent := deCheckBrowseMode;

  inherited Create;

  VisualControl := True;
  FDataIndepended := True;
  FDataIndependentValue := Null;
end;

procedure TMDFieldDataLink.Assign(Source: TPersistent);
begin
  if Source is TMDFieldDataLink then begin
    FieldName  := TMDFieldDataLink(Source).FieldName;
    DataSource := TMDFieldDataLink(Source).DataSource;
//    DataChanged;
  end;

  inherited Assign(Source);
end;

function TMDFieldDataLink.CheckValueChaged: Boolean;
var
  LValue: Variant;
begin
  Result := not FDataIndepended;
  if Result then begin
    if FEnabled then begin
      LValue := GetValue;
      Result := not VarIsEqual(FDataIndependentValue, LValue);
      if Result then begin
        FDataIndependentValue := LValue;
        DoOnDataChanged;
      end;
    end else begin
      Result := False;
      // FDataIndependentValue := GetValue;
    end;
  end;
end;

procedure TMDFieldDataLink.DoOnDataChanged;
begin
  if FEnabled and Assigned(FOnDataChanged) then
    FOnDataChanged(Self)
end;

function TMDFieldDataLink.Edit: Boolean;
begin
  if DataIndepended then begin
    if not Editing and not ReadOnly then begin
      FEditing := True;
      FModified := False;
      if FEnabled and Assigned(OnEditingChanged) then OnEditingChanged(Self);
    end;
  end else if CanModify then
    inherited Edit;

  Result := (not ReadOnly) and FEditing;
end;

function TMDFieldDataLink.GetActive: Boolean;
begin
  if DataIndepended then
    Result := True
  else
    Result := inherited Active and (Field <> nil);
end;

function TMDFieldDataLink.GetDataSetActive: Boolean;
begin
  Result := (DataSource <> nil) and (DataSource.DataSet <> nil) and DataSource.DataSet.Active;
end;

function TMDFieldDataLink.GetCanModify: Boolean;
var
  I: Integer;
  LDataSet: TDataSet;
begin
  Result := DataIndepended;

  if (not Result) then begin
    LDataSet := DataSet;
    if Assigned(LDataSet) and LDataSet.Active and LDataSet.CanModify then
      if MultiFields then begin
        for I := 0 to Pred(FieldsCount) do
          if not Fields[I].CanModify then Exit;

        Result := FieldsCount > 0;
      end else
        Result := ((Field <> nil) and Field.CanModify);
  end;
end;

function TMDFieldDataLink.GetDataSource: TDataSource;
begin
  Result := inherited DataSource;
end;

procedure TMDFieldDataLink.Modified;
begin
  FModified := True;
end;

procedure TMDFieldDataLink.DoOnFieldChanged(AField: TField);
begin
  if FEnabled and Assigned(FOnFieldChanged) then
    if Assigned(AField) then
      FOnFieldChanged(AField)
    else
      FOnFieldChanged(Self)
end;

procedure TMDFieldDataLink.SetDataSource(const Value: TDataSource);
begin
  if Value <> inherited DataSource then
  begin
    inherited DataSource := Value;
    UpdateDataIndepended;
  end;
end;

procedure TMDFieldDataLink.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName := Value;
    if Length(FFieldName) = 0 then
      FFieldNameCount := 0
    else if FMultiFields then
      FFieldNameCount := CharCount(';', FFieldName) + 1
    else
      FFieldNameCount := 1;

    UpdateField;
    UpdateDataIndepended;
  end;
end;

procedure TMDFieldDataLink.SetText(AText: String);
begin
  if DataIndepended then begin
    if FDataIndependentValue <> AText then begin
      FDataIndependentValue := Text;
      RecordChanged(nil);
    end;
  end else begin
    Assert(FieldsCount = 1);
    if (Field is TMemoField) then
      Field.AsString := Text
    else
      Field.Text := Text;
  end;
end;

procedure TMDFieldDataLink.SetValue(AValue: Variant);
var
  I: Integer;
  LValueChanged: Boolean;
  LField: TField;
begin
  if DataIndepended then begin
    LValueChanged := not VarIsEqual(FDataIndependentValue, AValue);
    if LValueChanged then begin
      FDataIndependentValue := AValue;
      DoOnDataChanged;
    end;
  end else if FieldsCount > 1 then begin
    LValueChanged := False;
    // DataSet.DisableControls;
    try
      if VarIsPresent(AValue) then begin
        for I := 0 to FieldsCount - 1 do
          with Fields[I] do
            if not VarIsEqual(AsVariant, AValue[I]) then begin
              if not LValueChanged then begin
                DataSet.DisableControls;
                LValueChanged := True;
              end;
              AsVariant := AValue[I];
            end;
      end else begin
        for I := 0 to FieldsCount - 1 do
          with Fields[I] do
            if not IsNull then begin
              if not LValueChanged then begin
                DataSet.DisableControls;
                LValueChanged := True;
              end;
              Clear;
            end;
      end;
    finally
      if LValueChanged then DataSet.EnableControls;
    end;
  end else begin
    LField := Field;
    Assert(Assigned(LField));
    LValueChanged := not VarIsEqual(LField.AsVariant, AValue);
    if LValueChanged then
      LField.AsVariant := AValue;
  end;
end;

procedure TMDFieldDataLink.UpdateData;
begin
  if DataIndepended then begin
    if FModified then
      if FEnabled and Assigned(FOnUpdateData) then FOnUpdateData(Self);
    FEditing := False;
    if not GetIsRefreshDisabled then
      FModified := False;
  end else if FModified then begin
    if FEnabled and (FieldsCount <> 0) and Assigned(FOnUpdateData) then FOnUpdateData(Self);
    if not GetIsRefreshDisabled then
      FModified := False;
  end;
end;

procedure TMDFieldDataLink.UpdateDataIndepended;
var
  LOldDataIndepended: Boolean;
begin
  LOldDataIndepended := FDataIndepended;
  FDataIndepended := (DataSource = nil) or (FieldName.IsEmpty);
  if FDataIndepended <> LOldDataIndepended then begin
    FDataIndependentValue := Null;

//    if not FDataIndepended then
//      FEditing := (inherited Editing);

    LayoutChanged;
    if not LOldDataIndepended and FDataIndepended then
      RecordChanged(nil);
  end;
end;

procedure TMDFieldDataLink.ActiveChanged;
begin
  try
    UpdateField;
  finally
    FModified := False;
    if FEnabled and Assigned(FOnActiveChanged) then FOnActiveChanged(Self);
  end;
end;

procedure TMDFieldDataLink.EditingChanged;
begin
  SetEditing(inherited Editing and CanModify);
end;

{$WARNINGS OFF}
function TMDFieldDataLink.FieldByName(const AFieldName: String): TField;
var
  I: Integer;
//  LField: TField;

  procedure FieldNotFound;
  begin
    DatabaseErrorFmt(SFieldNotFound, [AFieldName], DataSet);
  end;
begin
  if not DataSetActive then
    FieldNotFound;

  if (not FFieldsInitialized) then UpdateField;

  for I := FFields.Count - 1 downto 0 do begin
    Result := FFields[I];
    if SameText(Result.FieldName, AFieldName) then
      Exit;
  end;

  FieldNotFound;
end;
{$WARNINGS ON}

function TMDFieldDataLink.FieldFound(AField: TField): Boolean;
var
  i: Integer;
begin
  Result := False;
  for I := 0 to FieldsCount - 1 do
    if FFields[i] = AField then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TMDFieldDataLink.FieldFreeNotification(Sender: TComponent);
begin
  ClearFields;
end;

procedure TMDFieldDataLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and FieldFound(Field^) and (FControl is TWinControl) then
    if TWinControl(FControl).CanFocus then
    begin
      Field^ := nil;
      TWinControl(FControl).SetFocus;
    end;
end;

function TMDFieldDataLink.GetField: TField;
begin
  if (not FFieldsInitialized) and DataSetActive then UpdateField;

  if FieldsCount = 0 then
    Result := nil
  else
    Result := FFields[0];
end;

function TMDFieldDataLink.GetFieldsCount: Integer;
begin
  Result := FFields.Count;
end;

function TMDFieldDataLink.GetFieldsField(Index: Integer): TField;
begin
  if (not FFieldsInitialized) and DataSetActive then UpdateField;

  if FieldsCount = 0 then
    Result := nil
  else
    Result := FFields[Index];
end;

function TMDFieldDataLink.GetIsNull: Boolean;
var
  I: Integer;
begin
  if FDataIndepended then
    if FieldNameCount > 1 then
      Result := not VarArrayIsPresent(FDataIndependentValue)
    else
      Result := not VarIsPresent(FDataIndependentValue)
  else begin
    Result := True;
    if Assigned(DataSet) and DataSet.Active then begin
      for I := 0 to FieldsCount - 1 do begin
        Result := Fields[I].IsNull;
        if not Result then Exit;
      end;
    end;
  end;
end;

function TMDFieldDataLink.GetIsRefreshDisabled: Boolean;
begin
  Result := False;
end;

function TMDFieldDataLink.GetText: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FieldsCount - 1 do begin
    if I > 0 then
      Result := Result + ';';
    Result := Result + Fields[I].Text;
  end;
end;

function TMDFieldDataLink.GetValue: Variant;
var
  I: Integer;
begin
  if FDataIndepended then
    Result := FDataIndependentValue
  else if Assigned(DataSet) and DataSet.Active then
    Result := DataSet.FieldValues[FFieldName]
  else if FieldsCount <= 1 then
    Result := Null
  else begin
    Result := VarArrayCreate([0, FieldsCount - 1], varVariant);
    for I := 0 to FieldsCount - 1 do
      Result[I] := Null;
  end;
end;

procedure TMDFieldDataLink.LayoutChanged;
begin
  UpdateField;
end;

procedure TMDFieldDataLink.RecordChanged(AField: TField);
begin
  if (not Assigned(AField)) or FieldFound(AField) then begin
    if CheckValueChaged then begin
      if (not DataIndepended)
          and FEditing
          and (FieldsCount > 0)
          and
          (
            (FPriorEvent <> deUpdateState)
            or
            (DataSet.State <> dsInsert)
            or
            (not IsNull)
          )
      then
        DoOnFieldChanged(AField);
    end;
    if GetIsRefreshDisabled then
      FModified := False;
  end;
end;

procedure TMDFieldDataLink.Reset;
begin
  DoOnDataChanged;
end;

procedure TMDFieldDataLink.ResetModified;
begin
  FModified := False;
end;

procedure TMDFieldDataLink.SetMultiFields(const Value: Boolean);
begin
  if FMultiFields <> Value then begin
    FMultiFields := Value;
    UpdateField;
  end;
end;

procedure TMDFieldDataLink.UpdateField;
var
  LFieldList: TFieldsList;
begin
  LFieldList := TFieldsList.Create;
  try
    if (not FFieldName.IsEmpty) and DataSetActive then
      GetFieldsProperty(LFieldList, DataSet, Control, FFieldName, False);
    SetField(LFieldList);
  finally
    LFieldList.Free;
  end;
end;

procedure TMDFieldDataLink.UpdateRightToLeft;
var
  LIsRightAligned: Boolean;
  LUseRightToLeftAlignment: Boolean;
begin
  if Assigned(FControl) and (FControl is TWinControl) then
    with FControl as TWinControl do
      if IsRightToLeft then begin
        LIsRightAligned := (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_RIGHT) = WS_EX_RIGHT;
        LUseRightToLeftAlignment := DBUseRightToLeftAlignment(TControl(FControl), Field);
        if (LIsRightAligned and (not LUseRightToLeftAlignment)) or ((not LIsRightAligned) and LUseRightToLeftAlignment) then
          Perform(CM_RECREATEWND, 0, 0);
      end;
end;

procedure TMDFieldDataLink.SetEditing(Value: Boolean);
begin
  if FEditing <> Value then
  begin
    FEditing := Value;
    if not GetIsRefreshDisabled then
      FModified := False;
    if FEnabled and Assigned(FOnEditingChanged) then FOnEditingChanged(Self);
  end;
end;

procedure TMDFieldDataLink.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    if FEnabled then
      CheckValueChaged
  end;
end;

procedure TMDFieldDataLink.Clear;
begin
  Value := Null; // Emulate clear
end;

procedure TMDFieldDataLink.ClearFields;
var
  I: Integer;
begin
  FFieldsInitialized := False;
  for I := FieldsCount - 1 downto 0 do begin
    if Assigned(FFreeNotifier) then
      FFreeNotifier.RemoveSender(FFields[I]);

    FFields.Delete(I);
  end;
end;

procedure TMDFieldDataLink.SetField(AValue: TFieldsList);

  function CompareFieldsAndList: Boolean; //TODO: Create it
  var
    I: Integer;
  begin
    if AValue = nil then
      Exit(FieldsCount > 0);

    Result := (FieldsCount <> AValue.Count);
    if not Result then
      for I := FieldsCount - 1 downto 0 do begin
        Result := (FFields[I] <> TField(AValue[I]));
        if Result then Exit;
      end;
  end;

var
  I: Integer;
begin
  if CompareFieldsAndList then begin
    ClearFields;

    if Assigned(AValue) then
      for I := 0 to AValue.Count - 1 do begin
        FFields.Add(AValue[I]);
        FFreeNotifier.AddSender(AValue[I]);
      end;

    DoOnFieldListChanged;
    EditingChanged;
    RecordChanged(nil);
    UpdateRightToLeft;
  end;
  FFieldsInitialized := (FFields.Count > 0) or DataSetActive;
end;

procedure TMDFieldDataLink.SetModified(Value: Boolean);
begin
  FModified := Value;
end;

procedure TMDFieldDataLink.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  try
    case Event of
      deDisabledStateChange:
        try
          if Boolean(Info) then
            UpdateField
          else
            SetField(nil);
        finally
          FModified := False;
        end;
      else
        inherited DataEvent(Event, Info);
    end;
  finally
    FPriorEvent := Event;
  end;
end;

{
var
  LEnableEvents: TDataEvents;

    deDataSetChange: // deLayoutChange
    begin
      if (Info <> 0) then begin
        LEnableEvents := [];
        PWord(@LEnableEvents)^ := Info;
        if FEditing and (deFieldChange in LEnableEvents) then
          RecordChanged(nil)
      end;
      inherited DataEvent(Event, Info);
    end;
}

procedure TMDFieldDataLink.DataSetChanged;
begin
  EditingChanged;
  RecordChanged(nil);
end;

destructor TMDFieldDataLink.Destroy;
begin
  FreeAndNil(FFreeNotifier);
  FreeAndNil(FFields);
  inherited;
end;

procedure TMDFieldDataLink.DoOnFieldListChanged;
begin
//
end;

procedure TMDFieldDataLink.DoOnValueChanged;
begin
  DoOnDataChanged;
  DoOnFieldChanged(nil);
end;

{ TFieldDataLinkController }

constructor TFieldDataLinkController.Create(AOwner: TComponent);
begin
  FFieldDataLink := TMDFieldDataLink.Create;
  FFieldDataLink.MultiFields := True;

  inherited;
end;

destructor TFieldDataLinkController.Destroy;
begin
  FreeAndNil(FFieldDataLink);
  inherited;
end;

function TFieldDataLinkController.GetDataField: String;
begin
  Result := FFieldDataLink.FieldName
end;

function TFieldDataLinkController.GetDataSource: TDataSource;
begin
  Result := FFieldDataLink.DataSource
end;

function TFieldDataLinkController.GetOnFieldChanged: TNotifyEvent;
begin
  Result := FFieldDataLink.OnFieldChanged
end;

procedure TFieldDataLinkController.SetOnFieldChanged(const Value: TNotifyEvent);
begin
  FFieldDataLink.OnFieldChanged := Value
end;

procedure TFieldDataLinkController.SetDataField(const Value: String);
begin
  FFieldDataLink.FieldName := Value
end;

procedure TFieldDataLinkController.SetDataSource(const Value: TDataSource);
begin
  FFieldDataLink.DataSource := Value;
end;

function TFieldDataLinkController.GetOnDataChanged: TNotifyEvent;
begin
  Result := FFieldDataLink.OnDataChanged
end;

procedure TFieldDataLinkController.SetOnDataChanged(const Value: TNotifyEvent);
begin
  FFieldDataLink.OnDataChanged := Value
end;

{ TMDFieldDataLinks }

procedure TMDFieldDataLinks.AddField(ADataSource: TDataSource; const AFieldName: String);
var
  LIndex: Integer;
begin
  if (not Assigned(ADataSource)) or AFieldName.IsEmpty then Exit;

  LIndex := Count - 1;
  while (LIndex >= 0) do
    if Items[LIndex].DataSource = ADataSource then with Items[LIndex] do begin
      FieldName := ArraysMerge([FieldName, AFieldName], ';');
      Exit;
    end else
      Dec(LIndex);

  LIndex := Add(TMDFieldDataLink.Create);
  with Items[LIndex] do begin
    DataSource := ADataSource;
    FieldName  := AFieldName;
  end;
end;

procedure TMDFieldDataLinks.SetOnDataChanged(const Value: TNotifyEvent);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].OnDataChanged := Value;
end;

procedure TMDFieldDataLinks.SetOnFieldChanged(const Value: TNotifyEvent);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].OnFieldChanged := Value;
end;

{ TDataSourceDataLinkController }

constructor TDataSourceDataLinkController.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TDataSourceDataLink.Create
end;

destructor TDataSourceDataLinkController.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited;
end;

function TDataSourceDataLinkController.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource
end;

procedure TDataSourceDataLinkController.SetDataSource(const Value: TDataSource);
begin
  FDataLink.DataSource := Value
end;

function TDataSourceDataLinkController.GetOnActiveChanged: TNotifyEvent;
begin
  Result := FDataLink.OnActiveChanged
end;

function TDataSourceDataLinkController.GetOnDataSetChanged: TNotifyEvent;
begin
  Result := FDataLink.OnDataSetChanged
end;

function TDataSourceDataLinkController.GetOnEditingChanged: TNotifyEvent;
begin
  Result := FDataLink.OnEditingChanged
end;

function TDataSourceDataLinkController.GetOnFieldListChanged: TNotifyEvent;
begin
  Result := FDataLink.OnFieldListChanged
end;

function TDataSourceDataLinkController.GetOnRecordChanged: TNotifyEvent;
begin
  Result := FDataLink.OnRecordChanged
end;

function TDataSourceDataLinkController.GetOnRecordCountChanged: TNotifyEvent;
begin
  Result := FDataLink.OnRecordCountChanged
end;

procedure TDataSourceDataLinkController.SetOnActiveChanged(const Value: TNotifyEvent);
begin
  FDataLink.OnActiveChanged := Value
end;

procedure TDataSourceDataLinkController.SetOnDataSetChanged(const Value: TNotifyEvent);
begin
  FDataLink.OnDataSetChanged := Value
end;

procedure TDataSourceDataLinkController.SetOnEditingChanged(const Value: TNotifyEvent);
begin
  FDataLink.OnEditingChanged := Value
end;

procedure TDataSourceDataLinkController.SetOnFieldListChanged(const Value: TNotifyEvent);
begin
  FDataLink.OnFieldListChanged := Value
end;

procedure TDataSourceDataLinkController.SetOnRecordChanged(const Value: TNotifyEvent);
begin
  FDataLink.OnRecordChanged := Value
end;

procedure TDataSourceDataLinkController.SetOnRecordCountChanged(const Value: TNotifyEvent);
begin
  FDataLink.OnRecordCountChanged := Value
end;

{$IFDEF FASTSCRIPT_RTTI}
constructor TFunctions_MDDataLink.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TDataSourceDataLinkController, TDataSourceDataLinkController.ClassParent.ClassName) do begin
    AddEvent('OnActiveChanged', TfsNotifyEvent);
    AddEvent('OnDataSetChanged', TfsNotifyEvent);
    AddEvent('OnRecordChanged', TfsNotifyEvent);
    AddEvent('OnFieldListChanged', TfsNotifyEvent);
    AddEvent('OnEditingChanged', TfsNotifyEvent);
    AddEvent('OnRecordCountChanged', TfsNotifyEvent);
  end;
end;
{$ENDIF}

initialization
  RegisterClasses([TDataSourceDataLinkController, TFieldDataLinkController]);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TFunctions_MDDataLink);
{$ENDIF}

finalization
  UnRegisterClasses([TDataSourceDataLinkController, TFieldDataLinkController]);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TFunctions_MDDataLink);
{$ENDIF}

end.
