unit cxMultiFieldsDBDataBinding;

interface

uses
  Classes, SysUtils, Types, Variants, DB, FmtBcd, FieldDataLink,
  dxCoreClasses, dxCore, cxDB, cxEdit, cxDBEdit, cxDataUtils, cxCalc;

type
  TcxMDDBEditDataBinding = class;

  TcxMDEditFieldDataLink = class(TMDFieldDataLink)
  private
    FControl: TComponent;
    FDataBinding: TcxMDDBEditDataBinding;
//    FModified: Boolean;

//    FDataField: TField;
//    FFieldName: string;
//    FMasterField: TField;
//    FPropertiesField: TField;

//    procedure InternalSetField(ADataField, APropertiesField: TField);
//    procedure UpdateMasterField;
  protected
    function GetIsRefreshDisabled: Boolean; override;
    procedure DataEvent(Event: TDataEvent; Info: TdxNativeInt); override;
    procedure DoOnFieldListChanged; override;
//    procedure DoOnFieldChanged; override;
    procedure UpdateData; override;

    procedure ActiveChanged; override;
    procedure EditingChanged; override;
    procedure DoOnDataChanged; override;

    property DataField: TField read GetField;
    property PropertiesField: TField read GetField;
  public
    constructor Create(ADataBinding: TcxMDDBEditDataBinding);
    destructor Destroy; override;

    property Control: TComponent read FControl write FControl;
  end;

  TcxCustomMDDBEditDefaultValuesProvider = class(TcxCustomDBEditDefaultValuesProvider)
  private
    FFields: TFieldsList;
    function FieldCount: Integer; inline;
    procedure SetFields(const Value: TFieldsList);
  protected
    function IsDefaultDataAvailable: Boolean; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;

    function DefaultCanModify: Boolean; override;
    function DefaultReadOnly: Boolean; override;
    function DefaultRequired: Boolean; override;
    function IsDataAvailable: Boolean; override;
  public
    property Fields: TFieldsList read FFields write SetFields;
  end;

  TcxMDDBEditDataBinding = class(TcxEditDataBinding)
  private
    FDataLink: TcxMDEditFieldDataLink;
    FRefreshCount: Integer;

    function GetDataFieldName: string;
    function GetDataSource: TDataSource;
    function GetDefaultValuesProvider: TcxCustomMDDBEditDefaultValuesProvider;
    procedure SetDataFieldName(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    function GetOnDataFieldChanged: TNotifyEvent;
    procedure SetOnDataFieldChanged(const Value: TNotifyEvent);
    function GetField: TField;
  protected
    procedure DefaultValuesChanged; override;
    function GetEditing: Boolean; override;
    function GetIsDataAvailable: Boolean; override;
    function GetModified: Boolean; override;
    function GetStoredValue: TcxEditValue; override;
    function IsNull: Boolean; override;
    function IsInnerControlHaveToBeReadOnly: Boolean; override;
    procedure Reset; override;
    procedure SetDisplayValue(const Value: TcxEditValue); override;
    function SetEditMode: Boolean; override;
    procedure SetStoredValue(const Value: TcxEditValue); override;

    procedure DisableRefresh;
    procedure EnableRefresh;
    function IsRefreshDisabled: Boolean;
    procedure DataChanged; virtual;

    procedure DataSetChange; virtual;
    procedure EditingChanged; virtual;
    function IsLookupControl: Boolean; virtual;
    procedure UpdateData; virtual;

    property DefaultValuesProvider: TcxCustomMDDBEditDefaultValuesProvider read GetDefaultValuesProvider;
    property ProperiesField: TField read GetField;
  public
    constructor Create(AEdit: TcxCustomEdit); override;
    destructor Destroy; override;

    function CanCheckEditorValue: Boolean; override;
    function CanModify: Boolean; override;
    function CanPostEditorValue: Boolean; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    class function GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass; override;
    procedure SetModified; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure UpdateDisplayValue; override;
    procedure UpdateNotConnectedDBEditDisplayValue; override;

    property DataLink: TcxMDEditFieldDataLink read FDataLink;
    property Field: TField read GetField;
  published
    property DataField: string read GetDataFieldName write SetDataFieldName;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
{ -=MD=- }
    property OnDataFieldChanged: TNotifyEvent read GetOnDataFieldChanged write SetOnDataFieldChanged;
  end;

  { TcxDBTextEditDataBinding }

  TcxMDDBTextEditDataBinding = class(TcxMDDBEditDataBinding)
  protected
    procedure SetDisplayValue(const Value: TcxEditValue); override;
  public
    procedure UpdateNotConnectedDBEditDisplayValue; override;
  end;

implementation

uses SBaseVariantFunctions;

type
  TcxCustomEditAccess = class(TcxCustomEdit);
  TcxDataSetAccess = class(TDataSet);

{ TcxMDDBEditDataBinding }

constructor TcxMDDBEditDataBinding.Create(AEdit: TcxCustomEdit);
begin
  inherited Create(AEdit);
  FDataLink := TcxMDEditFieldDataLink.Create(Self);
  if AEdit.InnerControl <> nil then
    FDataLink.Control := AEdit.InnerControl
  else
    FDataLink.Control := AEdit;
end;

destructor TcxMDDBEditDataBinding.Destroy;
begin
  FreeAndNil(FDataLink);
  inherited Destroy;
end;

function TcxMDDBEditDataBinding.CanCheckEditorValue: Boolean;
begin
  Result := False;
end;

function TcxMDDBEditDataBinding.CanModify: Boolean;
begin
  Result := inherited CanModify and DataLink.CanModify;
end;

function TcxMDDBEditDataBinding.CanPostEditorValue: Boolean;
begin
  Result := Editing and DataLink.FModified;
end;

function TcxMDDBEditDataBinding.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := FDataLink.ExecuteAction(Action);
end;

class function TcxMDDBEditDataBinding.GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxCustomMDDBEditDefaultValuesProvider;
end;

procedure TcxMDDBEditDataBinding.SetModified;
begin
  if Editing then
    DataLink.Modified;
end;

procedure TcxMDDBEditDataBinding.SetOnDataFieldChanged(const Value: TNotifyEvent);
begin
  FDataLink.OnFieldChanged := Value
end;

function TcxMDDBEditDataBinding.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := FDataLink.UpdateAction(Action);
end;

procedure TcxMDDBEditDataBinding.UpdateDisplayValue;
begin
  Edit.LockClick(True);
  try
    if IsDataAvailable then
      FDataLink.Reset
    else
      inherited UpdateDisplayValue;
  finally
    Edit.LockClick(False);
  end;
end;

procedure TcxMDDBEditDataBinding.DefaultValuesChanged;
begin
  DefaultValuesProvider.Field   := ProperiesField;
  DefaultValuesProvider.Fields  := FDataLink.FFields;
  inherited DefaultValuesChanged;
end;

procedure TcxMDDBEditDataBinding.DisableRefresh;
begin
  Inc(FRefreshCount);
end;

procedure TcxMDDBEditDataBinding.EnableRefresh;
begin
  if FRefreshCount > 0 then
    Dec(FRefreshCount);
end;

function TcxMDDBEditDataBinding.GetEditing: Boolean;
begin
  Result := IsDataAvailable and FDataLink.Editing;
end;

function TcxMDDBEditDataBinding.GetField: TField;
begin
  Result := FDataLink.Field
end;

function TcxMDDBEditDataBinding.GetIsDataAvailable: Boolean;
begin
  Result := inherited GetIsDataAvailable and not FDataLink.DataIndepended;
end;

function TcxMDDBEditDataBinding.GetModified: Boolean;
begin
  Result := GetEditing and FDataLink.FModified;
end;

function TcxMDDBEditDataBinding.GetOnDataFieldChanged: TNotifyEvent;
begin
  Result := FDataLink.OnFieldChanged
end;

function TcxMDDBEditDataBinding.GetStoredValue: TcxEditValue;

  function GetNullValue(AEditValueSource: TcxDataEditValueSource): Variant;
  var
    I: Integer;
  begin
    if not IsDataAvailable or (AEditValueSource <> evsKey) or (FDataLink.FieldsCount = 0) then
      Result := Null
    else begin
      Result := VarArrayCreate([0, FDataLink.FieldsCount - 1], varVariant);
      for I := 0 to FDataLink.FieldsCount - 1 do
        Result[I] := Null;
    end;
  end;

var
  LEditValueSource: TcxDataEditValueSource;
  LField: TField;
  LFieldCount: Integer;
begin
  LEditValueSource := Edit.ActiveProperties.GetEditValueSource(Edit.InternalFocused);
  LFieldCount := FDataLink.FieldsCount;
  LField := FDataLink.Field;

  if (not IsDataAvailable)
    or
    (
      IsNull
      and
      (
        (LEditValueSource <> evsText)
        or
        (LFieldCount <> 1)
        or
        not Assigned(LField.OnGetText))
      )
    then
      Result := GetNullValue(LEditValueSource)
  else begin
    case LEditValueSource of
      evsKey:
        if (LFieldCount = 1) and (LField.KeyFields <> '') then
          Result := LField.DataSet.FieldValues[LField.KeyFields]
        else
          Result := FDataLink.Value;
      evsText:
        if Edit.Focused and FDataLink.CanModify then
          Result := FDataLink.Text
        else if (LFieldCount = 1) and LField.IsBlob then
          Result := LField.AsString
        else if (LFieldCount = 1) and (not Edit.ActiveProperties.IsValueFormattedByProperties) then
          Result := LField.DisplayText
        else
          Result := FDataLink.Value;
      evsValue:
        Result := FDataLink.Value;
      else
        Result := Null;
    end;
  end;
end;

function TcxMDDBEditDataBinding.IsNull: Boolean;
begin
  if (FDataLink.FieldsCount = 1) and (FDataLink.Field is TAggregateField) then
    Result := VarIsNull(TcxDataSetAccess(DataLink.DataSet).GetAggregateValue(FDataLink.Field))
  else
    Result := FDataLink.DataIndepended or FDataLink.IsNull;
end;

function TcxMDDBEditDataBinding.IsInnerControlHaveToBeReadOnly: Boolean;
begin
  Result := not Editing;
end;

function TcxMDDBEditDataBinding.IsRefreshDisabled: Boolean;
begin
  Result := FRefreshCount > 0;
end;

procedure TcxMDDBEditDataBinding.Reset;
begin
  if IsDataAvailable then
  begin
    FDataLink.Reset;
    Edit.SelectAll;
  end;
end;

procedure TcxMDDBEditDataBinding.SetDisplayValue(const Value: TcxEditValue);
begin
  if IsDataAvailable then
    SetInternalDisplayValue(Value)
  else
    SetInternalDisplayValue('');
end;

function TcxMDDBEditDataBinding.SetEditMode: Boolean;
begin
  Result := IDefaultValuesProvider.DefaultCanModify;
  if not Result then
    Exit;

  DisableRefresh;
  try
    FDatalink.Edit;
    Result := FDatalink.Editing;
    if Result then
      FDatalink.Modified;
  finally
    EnableRefresh;
  end;
end;

procedure TcxMDDBEditDataBinding.SetStoredValue(const Value: TcxEditValue);

  procedure SetFieldValue(AField: TField; const AValue: TcxEditValue);
  begin
    if VarIsStr(AValue) and (AValue = '') and
      not (AField.DataType in [ftString, ftWideString]) then
        AField.Value := Null
    else
      if (AField is TFMTBCDField) and VarIsType(AValue, varDouble) then
        AField.AsString := BcdToStrF(VarToBcd(AValue), ffFixed, TFMTBCDField(AField).Precision, cxMaxFloatToStrfDigits)

      else
        AField.Value := AValue;
  end;

var
  LEditValueSource: TcxDataEditValueSource;
  LField: TField;
  LFieldCount: Integer;
begin
  if IsDataAvailable then begin
    DisableRefresh;
    try
      if FDataLink.Edit then begin
        LFieldCount := FDataLink.FieldsCount;
        LField := FDataLink.Field;

        LEditValueSource := Edit.ActiveProperties.GetEditValueSource(True(*Edit.InternalFocused*));
        if (LEditValueSource = evsText) and VarIsPresent(Value) then
          FDataLink.Text := VarToStr(Value)
        else if (LFieldCount = 1) and (LEditValueSource = evsKey) and (LField.KeyFields <> '') then
          LField.DataSet.FieldValues[LField.KeyFields] := Value
        else
          FDataLink.Value := Value;

        FDataLink.ResetModified;
      end;
    finally
      EnableRefresh;
    end;
  end;
end;

procedure TcxMDDBEditDataBinding.UpdateNotConnectedDBEditDisplayValue;
begin
  if not IsDataAvailable then
    Edit.EditValue := Null;
end;

procedure TcxMDDBEditDataBinding.DataChanged;
begin
  if IsRefreshDisabled then
    Exit;
  if Edit.IsDesigning and not IsDataAvailable then
    UpdateNotConnectedDBEditDisplayValue
  else
  begin
    if not TcxCustomEditAccess(Edit).Focused and
      Edit.ActiveProperties.IsValueFormattedByProvider then
    begin
      if not IsDataAvailable or IsNull then
        TcxCustomEditAccess(Edit).FEditValue := Null
      else
        TcxCustomEditAccess(Edit).FEditValue := FDataLink.Value;
      Edit.LockClick(True);
      try
        SetInternalDisplayValue(StoredValue);
      finally
        Edit.LockClick(False);
      end;
    end
    else
      Edit.EditValue := StoredValue;
  end;
end;

procedure TcxMDDBEditDataBinding.DataSetChange;
begin
  DefaultValuesChanged;
end;

procedure TcxMDDBEditDataBinding.EditingChanged;
begin
  TcxCustomEditAccess(Edit).EditingChanged;
end;

function TcxMDDBEditDataBinding.IsLookupControl: Boolean;
begin
  Result := False;
end;

procedure TcxMDDBEditDataBinding.UpdateData;
begin
  if IsDataAvailable then
  begin
    if TcxCustomEditAccess(Edit).InternalValidateEdit then
      StoredValue := Edit.EditValue;
  end;
end;

function TcxMDDBEditDataBinding.GetDataFieldName: string;
begin
  Result := FDataLink.FieldName;
end;

function TcxMDDBEditDataBinding.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TcxMDDBEditDataBinding.GetDefaultValuesProvider: TcxCustomMDDBEditDefaultValuesProvider;
begin
  Result := TcxCustomMDDBEditDefaultValuesProvider(IDefaultValuesProvider.GetInstance);
end;

procedure TcxMDDBEditDataBinding.SetDataFieldName(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TcxMDDBEditDataBinding.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and Edit.IsLoading) then
  begin
    FDataLink.DataSource := Value;
    DefaultValuesProvider.DataSource := Value;
  end;
end;

{ TcxMDEditFieldDataLink }

procedure TcxMDEditFieldDataLink.ActiveChanged;
begin
  FDataBinding.DefaultValuesProvider.DataSource := DataSource;
  FDataBinding.DisableRefresh;
  try
    inherited;
    FDataBinding.DataSetChange;
  finally
    FDataBinding.EnableRefresh;
    FDataBinding.DataChanged;
  end;
end;

constructor TcxMDEditFieldDataLink.Create(ADataBinding: TcxMDDBEditDataBinding);
begin
  inherited Create;
  MultiFields := True;
  FDataBinding := ADataBinding;
end;

procedure TcxMDEditFieldDataLink.DataEvent(Event: TDataEvent; Info: TdxNativeInt);
begin
  inherited DataEvent(Event, Info);

  case Event of
    deDataSetChange:
    begin
      FDataBinding.DisableRefresh;
      try
        FDataBinding.DataSetChange;
      finally
        FDataBinding.EnableRefresh;
      end;
    end;
  end;
end;

destructor TcxMDEditFieldDataLink.Destroy;
begin
  inherited Destroy;

  inherited;
end;

procedure TcxMDEditFieldDataLink.EditingChanged;
begin
  inherited;
  FDataBinding.EditingChanged;
end;

function TcxMDEditFieldDataLink.GetIsRefreshDisabled: Boolean;
begin
  Result := Assigned(FDataBinding) and FDataBinding.IsRefreshDisabled
end;

procedure TcxMDEditFieldDataLink.DoOnDataChanged;
begin
  FDataBinding.DataChanged;
  inherited;
end;

procedure TcxMDEditFieldDataLink.DoOnFieldListChanged;
begin
  FDataBinding.DefaultValuesChanged;
  inherited;
end;

procedure TcxMDEditFieldDataLink.UpdateData;
begin
  if FModified and Assigned(DataField) then
    FDataBinding.UpdateData;

  inherited;
end;

{
procedure TcxMDEditFieldDataLink.UpdateMasterField;
begin
  if DataField = nil then
    FMasterField := nil
  else
    if not FDataBinding.IsLookupControl or (DataField.FieldKind <> fkLookup) or
      (Pos(';', DataField.KeyFields) > 0) then
        FMasterField := DataField
    else
      if Assigned(FControl) then
        FMasterField := GetFieldProperty(DataSource.DataSet, FControl, DataField.KeyFields)
      else
        FMasterField := DataSource.DataSet.FieldByName(DataField.KeyFields);
end;
}

{ TcxCustomMDDBEditDefaultValuesProvider }

constructor TcxCustomMDDBEditDefaultValuesProvider.Create(AOwner: TPersistent);
begin
  FFields := TFieldsList.Create;
  inherited;
end;

function TcxCustomMDDBEditDefaultValuesProvider.DefaultCanModify: Boolean;
var
  I: Integer;
begin
  Result := (inherited DefaultCanModify);
  if Result then
    for I := 1 to FieldCount - 1 do begin
      Result := Result and (FFields[I].CanModify or (FFields[I].Lookup and CanModifyLookupField(FFields[I])));
      if not Result then Exit;
    end;
end;

function TcxCustomMDDBEditDefaultValuesProvider.DefaultReadOnly: Boolean;
var
  I: Integer;
begin
  Result := (inherited DefaultReadOnly);
  if not Result then
    for I := 1 to FieldCount - 1 do begin
      Result := Result or FFields[I].ReadOnly;
      if Result then Exit;
    end;
end;

function TcxCustomMDDBEditDefaultValuesProvider.DefaultRequired: Boolean;
var
  I: Integer;
begin
  Result := (inherited DefaultRequired);
  if not Result then
    for I := 1 to FieldCount - 1 do begin
      Result := Result or FFields[I].Required;
      if Result then Exit;
    end;
end;

destructor TcxCustomMDDBEditDefaultValuesProvider.Destroy;
begin
  FreeAndNil(FFields);
  inherited;
end;

function TcxCustomMDDBEditDefaultValuesProvider.FieldCount: Integer;
begin
  Result := FFields.Count;
end;

function TcxCustomMDDBEditDefaultValuesProvider.IsDataAvailable: Boolean;
var
  I: Integer;
begin
  Result := (inherited IsDataAvailable);
  if Result then
    for I := 1 to FieldCount - 1 do begin
      Result := Result and cxDB.IsDataAvailable(FFields[I]);
      if not Result then Exit;
    end;
end;

function TcxCustomMDDBEditDefaultValuesProvider.IsDefaultDataAvailable: Boolean;
begin
  Result := FieldCount > 0;
end;

procedure TcxCustomMDDBEditDefaultValuesProvider.SetFields(const Value: TFieldsList);
var
  I: Integer;
begin
  FFields.Clear;
  for I := 0 to Value.Count - 1 do
    FFields.Add(Value[I]);
end;

{ TcxMDDBTextEditDataBinding }

procedure TcxMDDBTextEditDataBinding.SetDisplayValue(const Value: TcxEditValue);
begin
  if IsDataAvailable then
    SetInternalDisplayValue(Value)
  else
    if Edit.IsDesigning then
      SetInternalDisplayValue(Edit.Name)
    else
      SetInternalDisplayValue('');
end;

procedure TcxMDDBTextEditDataBinding.UpdateNotConnectedDBEditDisplayValue;
begin
  if not IsDataAvailable then
  begin
    Edit.LockClick(True);
    try
      DisplayValue := '';
    finally
      Edit.LockClick(False);
    end;
  end;
end;

end.







