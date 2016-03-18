unit fsFunction_TDataSet;

interface
{$I config.inc}

uses
  Types, SysUtils, Classes, DateUtils, frxClass, Variants, DB, TypInfo, fs_ievents,
  fs_iinterpreter, fs_idbrtti, fs_itools, SBaseUtils, NamedVariables
{$IFDEF DBF_WITH_HALCYON}
  , Halcn6db
{$ENDIF}
{$IFDEF DBF_WITH_UNIDAC}
  , Uni
  , DBFUniProvider
{$ENDIF}
  , SBaseDataModule, SBaseDataHelperInternal, FieldDataLink
  ;

type
  TfsFunctions_TDataSet = class(TfsRTTIModule)
  private
    function  Get_TConnection_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TConnection_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Call_TField_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Call_TFields_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TDataSet_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TDataSet_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
    function  Call_TDataSet_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TParam_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TParam_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
    function  Call_TParams_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Call_TParam_Method(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
{$IFDEF DBF_WITH_UNIDAC}
    function Call_TUniConnection_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
{$ENDIF}
  public
    constructor Create(AScript: TfsScript); override;
  end;

implementation

uses SBaseConstants, SBaseVariantFunctions;

type
  TfsBaseDataSetThreadAfterCompleteEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ADataSetProvider: TCustomSQLDataSetProvider; ATerminateReason: TDataSetThreadResult);
    function GetMethod: Pointer; override;
  end;

  TfsBaseDataSetThreadOnErrorEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ADataSetProvider: TCustomSQLDataSetProvider; const AException: String; var ADone: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsBaseDataSetThreadOnValidateEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ADataSetProvider: TCustomSQLDataSetProvider; ADataSet: TInternalQuery; AMultyDataSetIndex: Word; var AValid: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsBaseDataSetThreadOnPrintEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ADataSetProvider: TCustomSQLDataSetProvider; AMessages: TStringList);
    function GetMethod: Pointer; override;
  end;

{ TfsFunctions_TDataSet }

function TfsFunctions_TDataSet.Call_TDataSet_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  LStateBookmark                : TDataSourceStateBookmark;
  LDataSourceBookmarkRestores   : TDataSourceBookmarkRestores;
  LDataSetStateBookmarkRestores : TDataSetStateBookmarkRestores;
  LEditModes: TFieldsEditModes;
begin
  if MethodName = 'REFRESH' then
    TDataSet(Instance).Refresh
  else if MethodName = 'LOOKUP' then
    Result := TDataSet(Instance).Lookup(Caller.Params[0], Caller.Params[1], Caller.Params[2])
  else if MethodName = 'FIELDVALUES.GET' then
    Result := TDataSet(Instance).FieldValues[Caller.Params[0]]
  else if MethodName = 'FIELDVALUES.SET' then
    TDataSet(Instance).FieldValues[Caller.Params[0]] := Caller.Params[1]
  else if MethodName = 'FINDFIELD' then
    Result := frxInteger(TDataSet(Instance).FindField(Caller.Params[0]))
  else if MethodName = 'CANMODIFY' then
    Result := TDataSet(Instance).CanModify
  else if MethodName = 'MODIFIED' then
    Result := TDataSet(Instance).Modified
  else if MethodName = 'UPDATESTATUS' then
    Result := TDataSet(Instance).UpdateStatus
  else if MethodName = 'EMPTYTABLE' then
    TDataSet(Instance).EmptyTable
  else if MethodName = 'EDITING' then
    Result := TDataSet(Instance).Editing
  else if MethodName = 'INSERTED' then
    Result := TDataSet(Instance).Inserted
  else if MethodName = 'BEGINUPDATE' then begin
    TDataSet(Instance).BeginUpdate(LStateBookmark);
    Caller.Params[0] := LStateBookmark.AsVariant;
  end else if MethodName = 'ENDUPDATE' then begin
    if VarIsPresent(Caller.Params[1]) then
      VarToSet(Caller.Params[1], LDataSetStateBookmarkRestores, SizeOf(LDataSetStateBookmarkRestores))
    else
      LDataSetStateBookmarkRestores := DataSetStateBookmarkRestores_All;
    if VarIsPresent(Caller.Params[2]) then
      VarToSet(Caller.Params[2], LDataSourceBookmarkRestores, SizeOf(LDataSourceBookmarkRestores))
    else
      LDataSourceBookmarkRestores := DataSourceBookmarkRestores_All;
    LStateBookmark.AsVariant := Caller.Params[0];
    TDataSet(Instance).EndUpdate(LStateBookmark, LDataSetStateBookmarkRestores, LDataSourceBookmarkRestores);
  end else if MethodName = 'CONTROLSDISABLED' then
    Result := TDataSet(Instance).ControlsDisabled
  else if MethodName = 'LOAD' then
    TDataSet(Instance).Load(TDataSet(VarToPointer(Caller.Params[0])), VarToIntDef(Caller.Params[1], -1), VarToStr(Caller.Params[2]))
  else if MethodName = 'CLEARFIELDS' then begin
    if VarIsPresent(Caller.Params[1]) then
      VarToSet(Caller.Params[1], LEditModes, SizeOf(LEditModes))
    else
      LEditModes := [];
    Result := TDataSet(Instance).Fields.ClearFields(Caller.Params[0], LEditModes);
  end else if MethodName = 'SETFIELDS' then begin
    if VarIsPresent(Caller.Params[2]) then
      VarToSet(Caller.Params[2], LEditModes, SizeOf(LEditModes))
    else
      LEditModes := [];
    Result := TDataSet(Instance).Fields.SetFields(Caller.Params[0], Caller.Params[1], LEditModes);
  end else if MethodName = 'NONEMPTYFIELDSEXITS' then
    Result := TDataSet(Instance).NonEmptyFieldsExits(Caller.Params[0])
end;

function TfsFunctions_TDataSet.Call_TFields_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  LEditModes: TFieldsEditModes;
  LString: String;
begin
  if MethodName = 'FINDFIELD' then
    Result := frxInteger(TFields(Instance).FindField(Caller.Params[0]))
  else if MethodName = 'FIELDBYNAME' then
    Result := frxInteger(TFields(Instance).FieldByName(Caller.Params[0]))

  else if MethodName = 'LOAD' then
    TFields(Instance).Load(PNamedVariants(Integer(Caller.Params[0]))^, TLoadValueCondition(Caller.Params[1]))
  else if MethodName = 'LOADFROMFIELDS' then
    TFields(Instance).Load(TFields(VarToPointer(Caller.Params[0])), Caller.Params[1])
  else if MethodName = 'CLEARFIELDS' then begin
    if VarIsPresent(Caller.Params[1]) then
      VarToSet(Caller.Params[1], LEditModes, SizeOf(LEditModes))
    else
      LEditModes := [];
    Result := TFields(Instance).ClearFields(Caller.Params[0], LEditModes);
  end else if MethodName = 'SETFIELDS' then begin
    if VarIsPresent(Caller.Params[2]) then
      VarToSet(Caller.Params[2], LEditModes, SizeOf(LEditModes))
    else
      LEditModes := [];
    Result := TFields(Instance).SetFields(Caller.Params[0], Caller.Params[1], LEditModes);
  end else if MethodName = 'GETVALUES' then
    Result := TFields(Instance).GetValues(Caller.Params[0])
  else if MethodName = 'GETVALUESEX' then begin
    Result := TFields(Instance).GetValues(Caller.Params[0], @LString);
    Caller.Params[1] := LString;
  end else if MethodName = 'SETVALUES' then
    TFields(Instance).SetValues(Caller.Params[0], Caller.Params[1])
end;

function TfsFunctions_TDataSet.Call_TField_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'CLEAR' then
    TField(Instance).Clear
  else if MethodName = 'CANMODIFY' then
    Result := TField(Instance).CanModify
end;

{$IFDEF DBF_WITH_UNIDAC}
function TfsFunctions_TDataSet.Call_TUniConnection_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'OPEN' then
    TUniConnection(Instance).Open
  else if MethodName = 'CLOSE' then
    TUniConnection(Instance).Close;
end;
{$ENDIF}

function TfsFunctions_TDataSet.Call_TParams_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'PARAMVALUES.GET' then
    Result := TParams(Instance).ParamValues[Caller.Params[0]]
  else if MethodName = 'PARAMVALUES.SET' then
    TParams(Instance).ParamValues[Caller.Params[0]] := Caller.Params[1]
end;

function TfsFunctions_TDataSet.Call_TParam_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'LOADFROMFILE' then
    TParam(Instance).LoadFromFile(Caller.Params[0], Caller.Params[1]);
end;

constructor TfsFunctions_TDataSet.Create(AScript: TfsScript);
begin
  inherited Create(AScript);

  AScript.AddClass(TFMTBCDField, TFMTBCDField.ClassParent.ClassName);

  with AScript.AddClass(TCustomConnection, TCustomConnection.ClassParent.ClassName) do begin
    AddProperty('Connected', DelphiTypeBoolean, Get_TConnection_Property, Set_TConnection_Property);
    AddProperty('LoginPrompt', DelphiTypeBoolean, Get_TConnection_Property, Set_TConnection_Property);

    AddEvent('AfterConnect', TfsNotifyEvent);
    AddEvent('AfterDisconnect', TfsNotifyEvent);
  end;

  with AScript.AddClass(TField, TField.ClassParent.ClassName) do begin
    AddMethod('function CanModify: Boolean', Call_TField_Method);
    AddMethod('procedure Clear', Call_TField_Method);

    AddEvent('OnChange', TfsNotifyEvent);
  end;

  AScript.AddEnum('TFieldsEditMode', 'femAutoEdit, femAllOrRaise');
  with AScript.AddClass(TFields, TFields.ClassParent.ClassName) do begin
    AddMethod('function FieldByName(const FieldName: String): TField', Call_TFields_Method);
    AddMethod('function FindField(const FieldName: string): TField', Call_TFields_Method);

    AddMethod('procedure Load(AVariables: TNamedVariants; ALoadValueCondition: TLoadValueCondition)', Call_TFields_Method);
    AddMethod('procedure LoadFromFields(AFields: TFields; ALoadValueCondition: TLoadValueCondition)', Call_TFields_Method);

    AddMethod('function ClearFields(AFields: String; AEditModes: Variant = nil): Boolean', Call_TFields_Method);
    AddMethod('function SetFields(AFields: String; AValues: Variant; AEditModes: Variant = nil): Boolean', Call_TFields_Method);

    AddMethod('function GetValues(const AFieldNames: String): Variant', Call_TFields_Method);
    AddMethod('function GetValuesEx(const AFieldNames: String; var AUnkwnownFields: String): Variant', Call_TFields_Method);
    AddMethod('procedure SetValues(const AFieldNames: String; const AValue: Variant)', Call_TFields_Method);
  end;

  with AScript.AddClass(TParam, TParam.ClassParent.ClassName) do begin
    AddProperty('AsVariant', DelphiTypeVariant, Get_TParam_Property, Set_TParam_Property);
    AddMethod('procedure LoadFromFile(FileName: String; BlobType: TFieldType)', Call_TParam_Method);
  end;

  AScript.AddEnum('TUpdateStatus', 'usUnmodified, usModified, usInserted, usDeleted');
  AScript.AddEnum('TDataSetState', 'dsInactive, dsBrowse, dsEdit, dsInsert, dsSetKey, dsCalcFields, dsFilter,'
                                 + 'dsNewValue, dsOldValue, dsCurValue, dsBlockRead, dsInternalCalc, dsOpening');
  AScript.AddEnum('TDataSetThreadResult', 'bdstrError, bdstrOpened, bdstrRefreshed, bdstrExecuted');


  with AScript.AddClass(TDataSet, TDataSet.ClassParent.ClassName) do begin
    AddIndexProperty('FieldValues', DelphiTypeString, DelphiTypeVariant, Call_TDataSet_Method, False);
    AddProperty('RecordCount', DelphiTypeInteger, Get_TDataSet_Property);
    AddProperty('RecNo', DelphiTypeInteger, Get_TDataSet_Property, Set_TDataSet_Property);
    AddProperty('State', 'TDataSetState', Get_TDataSet_Property);

    AddMethod('procedure ReOpen(AReOpenFlag: Integer = 0)', Call_TDataSet_Method);
    AddMethod('procedure EmptyTable', Call_TDataSet_Method);
    AddMethod('function CanModify: Boolean', Call_TDataSet_Method);
    AddMethod('function Modified: Boolean', Call_TDataSet_Method);
    AddMethod('function UpdateStatus: TUpdateStatus', Call_TDataSet_Method);
    AddMethod('procedure Refresh', Call_TDataSet_Method);
    AddMethod('function Lookup(FieldName: String; FieldValue: Variant; LookupFieldName: String): Variant', Call_TDataSet_Method);
    AddMethod('function FindField(const FieldName: string): TField', Call_TDataSet_Method);
    AddMethod('function ClearFields(AFields: String; AEditModes: Variant = nil): Boolean', Call_TDataSet_Method);
    AddMethod('function SetFields(AFields: String; AValues: Variant; AEditModes: Variant = nil): Boolean', Call_TDataSet_Method);

    AddMethod('function Editing: Boolean', Call_TDataSet_Method);
    AddMethod('function Inserted: Boolean', Call_TDataSet_Method);
    AddMethod('function NonEmptyFieldsExits(const AFieldNames: String): Boolean', Call_TDataSet_Method);

    AddMethod('procedure BeginUpdate(var AStateBookMark: Variant)', Call_TDataSet_Method);
    AddMethod
    (
      'procedure EndUpdate(const AStateBookMark: Variant; ADataSetStateRestore: Integer = '
                + IntToStr(IntFromSet(DataSetStateBookmarkRestores_All, SizeOf(TDataSetStateBookmarkRestores)))
                + '; ADataSourceRestore: Integer = '
                + IntToStr(IntFromSet(DataSourceBookmarkRestores_All, SizeOf(TDataSourceBookmarkRestores)))
                + ')',
      Call_TDataSet_Method
    );
    AddMethod('function ControlsDisabled: Boolean', Call_TDataSet_Method);
    AddMethod('procedure Load(ADataSet: TDataSet; ARecordCount: Integer = -1; AExcludedFields: String = '')', Call_TDataSet_Method);
  end;

  with AScript.AddClass(TBaseDataSetThread, TBaseDataSetThread.ClassParent.ClassName) do begin
    AddEvent('AfterComplete', TfsBaseDataSetThreadAfterCompleteEvent);
    AddEvent('OnError', TfsBaseDataSetThreadOnErrorEvent);
    AddEvent('OnPrint', TfsBaseDataSetThreadOnPrintEvent);
    AddEvent('OnValidate', TfsBaseDataSetThreadOnValidateEvent);
  end;

  with AScript.AddClass(TParams, TParams.ClassParent.ClassName) do begin
    AddIndexProperty('ParamValues', DelphiTypeString, DelphiTypeVariant, Call_TParams_Method, False);
  end;

  AScript.AddEnum('TUpdateKind', 'ukModify, ukInsert, ukDelete');
//  AScript.AddEnumSet('TStatementTypes', 'stQuery, stInsert, stUpdate, stDelete, stLock, stRefresh, stCheck, stCustom, stRefreshQuick, stRefreshCheckDeleted, stBatchUpdate');

{$IFDEF DBF_WITH_HALCYON}
  { Halcyon }
  AScript.AddClass(THalcyonDataSet, THalcyonDataSet.ClassParent.ClassName);
{$ENDIF}

  with AScript.AddClass(TFieldDataLinkController, TFieldDataLinkController.ClassParent.ClassName) do begin
    AddEvent('OnDataChanged', TfsNotifyEvent);
    AddEvent('OnFieldChanged', TfsNotifyEvent);
  end;

{$IFDEF DBF_WITH_UNIDAC}
  with AScript.AddClass(TUniConnection, TUniConnection.ClassParent.ClassName) do begin
    AddMethod('procedure Open', Call_TUniConnection_Method);
    AddMethod('procedure Close', Call_TUniConnection_Method);
  end;
{$ENDIF}
end;

function TfsFunctions_TDataSet.Get_TConnection_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'CONNECTED' then
    Result := TCustomConnection(Instance).Connected
  else if PropName = 'LOGINPROMPT' then
    Result := TCustomConnection(Instance).Connected
end;

function TfsFunctions_TDataSet.Get_TDataSet_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'RECORDCOUNT' then
    Result := TDataSet(Instance).RecordCount
  else if PropName = 'RECNO' then
    Result := TDataSet(Instance).RecNo
  else if PropName = 'STATE' then
    Result := TDataSet(Instance).State
end;

function TfsFunctions_TDataSet.Get_TParam_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'ASVARIANT' then
    Result := TParam(Instance).AsVariant;
end;

procedure TfsFunctions_TDataSet.Set_TParam_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
  if PropName = 'ASVARIANT' then
    TParam(Instance).AsVariant := Value
end;

procedure TfsFunctions_TDataSet.Set_TConnection_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
  if PropName = 'CONNECTED' then
    TCustomConnection(Instance).Connected := Value
  else if PropName = 'LOGINPROMPT' then
    TCustomConnection(Instance).LoginPrompt := Value
end;

procedure TfsFunctions_TDataSet.Set_TDataSet_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
  if PropName = 'RECNO' then
    TDataSet(Instance).RecNo := Value
end;

{ TfsBaseDataSetThreadOnValidateEvent }

procedure TfsBaseDataSetThreadOnValidateEvent.DoEvent(ADataSetProvider: TCustomSQLDataSetProvider; ADataSet: TInternalQuery; AMultyDataSetIndex: Word; var AValid: Boolean);
begin
  CallHandler([ADataSetProvider, ADataSet, AMultyDataSetIndex, AValid]);
  AValid := Handler.Params[3].Value;
end;

function TfsBaseDataSetThreadOnValidateEvent.GetMethod: Pointer;
begin
  Result := @TfsBaseDataSetThreadOnValidateEvent.DoEvent;
end;

{ TfsBaseDataSetThreadAfterCompleteEvent }

procedure TfsBaseDataSetThreadAfterCompleteEvent.DoEvent(
  ADataSetProvider: TCustomSQLDataSetProvider;
  ATerminateReason: TDataSetThreadResult);
begin
  CallHandler([ADataSetProvider, Integer(ATerminateReason)]);
end;

function TfsBaseDataSetThreadAfterCompleteEvent.GetMethod: Pointer;
begin
  Result := @TfsBaseDataSetThreadAfterCompleteEvent.DoEvent;
end;

{ TfsBaseDataSetThreadOnErrorEvent }

procedure TfsBaseDataSetThreadOnErrorEvent.DoEvent(ADataSetProvider: TCustomSQLDataSetProvider; const AException: String; var ADone: Boolean);
begin
  CallHandler([ADataSetProvider, AException, ADone]);
  ADone := Handler.Params[2].Value;
end;

function TfsBaseDataSetThreadOnErrorEvent.GetMethod: Pointer;
begin
  Result := @TfsBaseDataSetThreadOnErrorEvent.DoEvent;
end;

{ TfsBaseDataSetThreadOnPrintEvent }

procedure TfsBaseDataSetThreadOnPrintEvent.DoEvent(
  ADataSetProvider: TCustomSQLDataSetProvider; AMessages: TStringList);
begin
  CallHandler([ADataSetProvider, AMessages]);
end;

function TfsBaseDataSetThreadOnPrintEvent.GetMethod: Pointer;
begin
  Result := @TfsBaseDataSetThreadOnPrintEvent.DoEvent;
end;

initialization
  RegisterClass(TParam);
  RegisterClasses(
    [
      TStringField, TNumericField, TLargeintField, TIntegerField, TSmallIntField, TWordField, TGuidField,
      TAutoIncField, TFloatField, TExtendedField, TCurrencyField, TNumericField, TBooleanField, TDateTimeField,
      TDateField, TTimeField, TBinaryField, TBytesField, TVarBytesField, TBCDField, TFMTBCDField, TVariantField,

      TDataSource
{$IFDEF DBF_WITH_HALCYON}
      , THalcyonDataSet
{$ENDIF}
//      , TADODataSet, TCustomADODataSet
    ]);

  fsRTTIModules.Add(TfsFunctions_SBaseUtils);
  fsRTTIModules.Add(TfsFunctions_TDataSet);

finalization
  UnRegisterClass(TParam);
  UnRegisterClasses(
    [
      TStringField, TNumericField, TLargeintField, TIntegerField, TSmallIntField, TWordField, TGuidField,
      TAutoIncField, TFloatField, TExtendedField, TCurrencyField, TNumericField, TBooleanField, TDateTimeField,
      TDateField, TTimeField, TBinaryField, TBytesField, TVarBytesField, TBCDField, TFMTBCDField, TVariantField,

      TDataSource
{$IFDEF DBF_WITH_HALCYON}
      , THalcyonDataSet
{$ENDIF}
//      , TADODataSet, TCustomADODataSet
    ]);

  fsRTTIModules.Remove(TfsFunctions_SBaseUtils);
  fsRTTIModules.Remove(TfsFunctions_TDataSet);

end.
