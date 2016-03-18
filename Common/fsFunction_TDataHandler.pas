unit fsFunction_TDataHandler;

interface

uses
  Types, Classes;

implementation

uses fs_iinterpreter, DataHandler, SBaseVariantFunctions, fsFunction_Other, NamedVariables, DB, Variants,
  SBaseDataModule, SBaseStringFunctions, SBaseConstants;

type
  TFunctions_TDataHandler = class(TfsRTTIModule)
  private
  public
    constructor Create(AScript: TfsScript); override;

    function Call_TCustomDataHandler_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Call_TDataHandler_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Call_TSubDataHandler_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Call_TdhNewActionProperties_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Call_TdhMultiNewItems_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  end;

  TfsDataHandlerEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ADataHandler: TCustomDataHandler);
    function GetMethod: Pointer; override;
  end;

  TfsDataHandlerVariabledEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ADataHandler: TCustomDataHandler; AVariables: TNamedVariants);
    function GetMethod: Pointer; override;
  end;

  TfsSubDataHandlerMemoryApplyEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ADataHandler: TCustomDataHandler; ADataSet: TDataSet; var AApplyAction: TdhApplyAction);
    function GetMethod: Pointer; override;
  end;

  TfsDataHandlerAllowEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ADataHandler: TCustomDataHandler; var ADone: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsDataHandlerAllowVariabledEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ADataHandler: TCustomDataHandler; var AVariables : TNamedVariants; var ADone: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsDataHandlerDeleteEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ADataHandler: TCustomDataHandler; ACascade: Boolean; var ADone: Boolean);
    function GetMethod: Pointer; override;
  end;

//  TfsDataHandlerRowChangedEvent = class(TfsCustomEvent)
//  public
//    procedure DoEvent(ADataHandler: TCustomDataHandler; IsAfterScroll: Boolean);
//    function GetMethod: Pointer; override;
//  end;
{
  TfsDataHandlerGetClassNameEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ADataHandler: TCustomDataHandler; const AVariables: TNamedVariants; var AClassName: String);
    function GetMethod: Pointer; override;
  end;
}
  TfsDataHandlerBeforeNewEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ADataHandler: TCustomDataHandler; AItem: TdhMultiNewItem; var AVariables : TNamedVariants; var ADone: Boolean);
    function GetMethod: Pointer; override;
  end;

//  TfsdhGetEnabledNotify = class(TfsCustomEvent)
//  public
//    procedure DoEvent(AProperties: TdhCustomMenuActionProperties; var AEnabled: Boolean);
//    function GetMethod: Pointer; override;
//  end;

{ Public procedures }

{ TFunctions_TDataHandler }

constructor TFunctions_TDataHandler.Create(AScript: TfsScript);
begin
  inherited;
  AScript.AddEnum('TdhApplyAction', 'dhaaNone, dhaaAppendRecord, dhaaUpdateRecord, dhaaAbort');
  AScript.AddEnum('TdhGenerateKeyFieldMethod', 'dhGkfNever, dhGkfInc, dhGkfDec, dhGkfIncFromMin, dhGkfDecFromMax, dhGkfAutoInc, dhGkfNewGUId');

  with AScript.AddClass(TCustomDataHandler, TCustomDataHandler.ClassParent.ClassName) do begin
//    AddEvent('OnGetClassName', TfsDataHandlerGetClassNameEvent);

    AddEvent('OnBeforeView', TfsDataHandlerAllowVariabledEvent);

    AddEvent('OnBeforeNew', TfsDataHandlerBeforeNewEvent);
    AddEvent('OnAfterNew', TfsDataHandlerEvent);

    AddEvent('OnBeforeEdit', TfsDataHandlerAllowVariabledEvent);
    AddEvent('OnAfterEdit', TfsDataHandlerEvent);

    AddEvent('OnBeforeApply', TfsDataHandlerAllowEvent);
//    AddEvent('OnBeforeOpen', TfsDataHandlerAllowEvent);

    AddEvent('OnBeforeDelete', TfsDataHandlerAllowEvent);
    AddEvent('OnAfterDelete', TfsDataHandlerEvent);

//    AddEvent('OnAfterCommit', TfsDataHandlerEvent);

    AddEvent('OnBeforeValidate', TfsDataHandlerAllowEvent);
    AddEvent('OnBeforeDelete', TfsDataHandlerDeleteEvent);

//    AddEvent('OnAfterOpen', TfsDataHandlerEvent);
//    AddEvent('OnRowChanged', TfsDataHandlerRowChangedEvent);

    AddMethod('procedure Cancel', Call_TCustomDataHandler_Method);
    AddMethod('procedure Close', Call_TCustomDataHandler_Method);

    AddMethod('procedure Open', Call_TCustomDataHandler_Method);
    AddMethod('procedure Post', Call_TCustomDataHandler_Method);
//    AddMethod('procedure Refresh(ARecordOnly: Boolean = False)', Call_TCustomDataHandler_Method);
    AddMethod('procedure Validate', Call_TCustomDataHandler_Method);
//    AddMethod('procedure Update', Call_TCustomDataHandler_Method);
    AddMethod('procedure Restore', Call_TCustomDataHandler_Method);

    AddMethod('function CanModify: Boolean', Call_TCustomDataHandler_Method);
    AddMethod('function HasChildren: Boolean', Call_TCustomDataHandler_Method);
    // AddMethod('function IsRoot: Boolean', Call_TCustomDataHandler_Method);
    AddMethod('function IsDataChanged: Boolean', Call_TCustomDataHandler_Method);
    // AddMethod('function Editing: Boolean', Call_TCustomDataHandler_Method);
    // AddMethod('function Inserting: Boolean', Call_TCustomDataHandler_Method);

    AddMethod('procedure DisableControls(ARecursive: Boolean)', Call_TCustomDataHandler_Method);
    AddMethod('procedure EnableControls(ARecursive: Boolean)', Call_TCustomDataHandler_Method);
  end;

  with AScript.AddClass(TDataHandler, TDataHandler.ClassParent.ClassName) do begin
    AddMethod('function CheckApplied: Boolean', Call_TDataHandler_Method);
  end;

  AScript.AddClass(TSubDataHandlerXMLPostProperties, TSubDataHandlerXMLPostProperties.ClassParent.ClassName);

  with AScript.AddClass(TSubDataHandler, TSubDataHandler.ClassParent.ClassName) do begin
    AddMethod('function Parent: TCustomDataHandler', Call_TSubDataHandler_Method);
//    AddMethod('function Delete: Boolean', Call_TCustomDataHandler_Method);
    AddMethod('function Edit(ARaiseIfFalse: Boolean = True): Boolean', Call_TSubDataHandler_Method);
//    AddMethod('function NewObject: Boolean', Call_TCustomDataHandler_Method);
    AddMethod('function New(AItem: TdhMultiNewItem = nil): Boolean', Call_TSubDataHandler_Method);
//    AddMethod('procedure View', Call_TCustomDataHandler_Method);

    AddEvent('OnMemoryApply', TfsSubDataHandlerMemoryApplyEvent);
  end;


  AScript.AddEnum('TdhActionMode', 'amDefault, amInplace, amKeyFields, amKeyFieldsModal, amDataSet, amDataSetSelect');

  AScript.AddClass(TdhOptionsMenu, TdhOptionsMenu.ClassParent.ClassName);

  AScript.AddEnum('TdhMultiNewItemsLoadMode', 'dhlmAsField, dhlmAsSimpleObject');
  with AScript.AddClass(TdhNewActionProperties, TdhNewActionProperties.ClassParent.ClassName) do begin
    AddMethod('procedure Load(ADataSet: TSQLDataSetProvider; const AFieldName: String; AMode: TdhMultiNewItemsLoadMode)', Call_TdhNewActionProperties_Method);
  end;
  AScript.AddClass(TdhViewActionProperties, TdhViewActionProperties.ClassParent.ClassName);
  AScript.AddClass(TdhEditActionProperties, TdhEditActionProperties.ClassParent.ClassName);

  AScript.AddClass(TdhAction, TdhAction.ClassParent.ClassName);
  with AScript.AddClass(TdhCustomMenuActionProperties, TdhCustomMenuActionProperties.ClassParent.ClassName) do begin
  end;

  AScript.AddClass(TdhMultiNewActionProperties, TdhMultiNewActionProperties.ClassParent.ClassName);
  AScript.AddClass(TdhDeleteActionProperties, TdhDeleteActionProperties.ClassParent.ClassName);

  AScript.AddClass(TdhMultiNewItem, TdhMultiNewItem.ClassParent.ClassName);
  with AScript.AddClass(TdhMultiNewItems, TdhMultiNewItems.ClassParent.ClassName) do begin
    AddDefaultProperty(SConst_Items, DelphiTypeInteger, TdhMultiNewItem.ClassName, Call_TdhMultiNewItems_Method, False);
  end;

  AScript.AddClass(TdhSettings, TdhSettings.ClassParent.ClassName);
end;

function TFunctions_TDataHandler.Call_TCustomDataHandler_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TCustomDataHandler);
{$ENDIF}
  if MethodName = 'CANCEL' then
    TCustomDataHandler(Instance).Cancel
  else if MethodName = 'CLOSE' then
    TCustomDataHandler(Instance).Close
//  else if MethodName = 'COMMIT' then
//    TCustomDataHandler(Instance).Commit
//  else if MethodName = 'APPLY' then
//    TCustomDataHandler(Instance).Apply
  else if MethodName = 'OPEN' then
    TCustomDataHandler(Instance).Open
  else if MethodName = 'POST' then
    TCustomDataHandler(Instance).Post
//  else if MethodName = 'REFRESH' then
//    TCustomDataHandler(Instance).Refresh(Caller.Params[0])
//  else if MethodName = 'VIEW' then
//    TCustomDataHandler(Instance).View
//  else if MethodName = 'ROLLBACK' then
//    TCustomDataHandler(Instance).RollBack
  else if MethodName = 'VALIDATE' then
    TCustomDataHandler(Instance).Validate
//  else if MethodName = 'UPDATE' then
//    TCustomDataHandler(Instance).Update
  else if MethodName = 'RESTORE' then
    TCustomDataHandler(Instance).Restore
  else if MethodName = 'DISABLECONTROLS' then
    TCustomDataHandler(Instance).DisableControls(Caller.Params[0])
  else if MethodName = 'ENABLECONTROLS' then
    TCustomDataHandler(Instance).EnableControls(Caller.Params[0])

  else if MethodName = 'CANMODIFY' then
    Result := TCustomDataHandler(Instance).CanModify
  else if MethodName = 'HASCHINDREN' then
    Result := TCustomDataHandler(Instance).HasChildren
  else if MethodName = 'ISDATACHANGED' then
    Result := TCustomDataHandler(Instance).IsDataChanged

{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TDataHandler.Call_TDataHandler_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TCustomDataHandler);
{$ENDIF}
  if MethodName = 'CHECKAPPLIED' then
    Result := TDataHandler(Instance).CheckApplied
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TDataHandler.Call_TSubDataHandler_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TCustomDataHandler);
{$ENDIF}
  if MethodName = 'PARENT' then
    Result := Integer(TSubDataHandler(Instance).Parent)
  else if MethodName = 'EDIT' then
    Result := TSubDataHandler(Instance).Edit(VarToBoolDef(Caller.Params[0], False))
  else if MethodName = 'NEW' then
    Result := TSubDataHandler(Instance).New(TdhMultiNewItem(Integer(Caller.Params[0])))
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TDataHandler.Call_TdhMultiNewItems_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TdhMultiNewItems);
{$ENDIF}
  if MethodName = 'ITEMS.GET' then
    Result := Integer(TdhMultiNewItems(Instance).Items[Caller.Params[0]])
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TDataHandler.Call_TdhNewActionProperties_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TdhNewActionProperties);
{$ENDIF}
  if MethodName = 'LOAD' then
    TdhNewActionProperties(Instance).Load(VarToPointer(Caller.Params[0]), Caller.Params[1], Caller.Params[2])
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

procedure TfsDataHandlerAllowEvent.DoEvent(ADataHandler: TCustomDataHandler; var ADone: Boolean);
begin
  CallHandler([ADataHandler, ADone]);
  ADone := Handler.Params[1].Value = True;
end;

function TfsDataHandlerAllowEvent.GetMethod: Pointer;
begin
  Result := @TfsDataHandlerAllowEvent.DoEvent;
end;

procedure TfsDataHandlerEvent.DoEvent(ADataHandler: TCustomDataHandler);
begin
  CallHandler([ADataHandler]);
end;

function TfsDataHandlerEvent.GetMethod: Pointer;
begin
  Result := @TfsDataHandlerEvent.DoEvent;
end;


//{ TfsDataHandlerRowChangedEvent }
//
//procedure TfsDataHandlerRowChangedEvent.DoEvent(ADataHandler: TCustomDataHandler; IsAfterScroll: Boolean);
//begin
//  CallHandler([ADataHandler, IsAfterScroll]);
//end;
//
//function TfsDataHandlerRowChangedEvent.GetMethod: Pointer;
//begin
//  Result := @TfsDataHandlerRowChangedEvent.DoEvent;
//end;

{ TfsDataHandlerDeleteEvent }

procedure TfsDataHandlerDeleteEvent.DoEvent(ADataHandler: TCustomDataHandler; ACascade: Boolean; var ADone: Boolean);
begin
  CallHandler([ADataHandler, ACascade, ADone]);
  ADone := Handler.Params[2].Value;
end;

function TfsDataHandlerDeleteEvent.GetMethod: Pointer;
begin
  Result := @TfsDataHandlerDeleteEvent.DoEvent;
end;

{ TfsDataHandlerBeforeNewEvent }

procedure TfsDataHandlerBeforeNewEvent.DoEvent(ADataHandler: TCustomDataHandler; AItem: TdhMultiNewItem; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  CallHandler([ADataHandler, AItem, Integer(@AVariables), ADone]);
  ADone := Handler.Params[3].Value;
end;

function TfsDataHandlerBeforeNewEvent.GetMethod: Pointer;
begin
  Result := @TfsDataHandlerBeforeNewEvent.DoEvent;
end;

{ TfsDataHandlerAllowVariabledEvent }

procedure TfsDataHandlerAllowVariabledEvent.DoEvent(ADataHandler: TCustomDataHandler; var AVariables : TNamedVariants; var ADone: Boolean);
begin
  CallHandler([ADataHandler, Integer(@AVariables), ADone]);
  ADone := Handler.Params[2].Value;
end;

function TfsDataHandlerAllowVariabledEvent.GetMethod: Pointer;
begin
  Result := @TfsDataHandlerAllowVariabledEvent.DoEvent;
end;

{ TfsDataHandlerVariabledEvent }

procedure TfsDataHandlerVariabledEvent.DoEvent(ADataHandler: TCustomDataHandler; AVariables: TNamedVariants);
begin
  CallHandler([ADataHandler, Integer(@AVariables)]);
end;

function TfsDataHandlerVariabledEvent.GetMethod: Pointer;
begin
  Result := @TfsDataHandlerVariabledEvent.DoEvent;
end;

{ TfsSubDataHandlerMemoryApplyEvent }

procedure TfsSubDataHandlerMemoryApplyEvent.DoEvent(ADataHandler: TCustomDataHandler; ADataSet: TDataSet; var AApplyAction: TdhApplyAction);
begin
  CallHandler([ADataHandler, ADataSet, Variant(AApplyAction)]);
  AApplyAction := Handler.Params[2].Value;
end;

function TfsSubDataHandlerMemoryApplyEvent.GetMethod: Pointer;
begin
  Result := @TfsSubDataHandlerMemoryApplyEvent.DoEvent;
end;

{ TfsDataHandlerGetClassNameEvent

procedure TfsDataHandlerGetClassNameEvent.DoEvent(ADataHandler: TCustomDataHandler; const AVariables: TNamedVariants; var AClassName: String);
begin
  CallHandler([ADataHandler, Integer(@AVariables), AClassName]);
  AClassName := VarToStr(Handler.Params[2].Value);
end;

function TfsDataHandlerGetClassNameEvent.GetMethod: Pointer;
begin
  Result := @TfsDataHandlerGetClassNameEvent.DoEvent;
end;
}

initialization
  RegisterClasses([TDataHandler, TdhParamEdit, TdhConstantParamEdit, TdhDynamicParamEdit, TdhMultiNewItems, TdhMultiNewItem]);
  fsRTTIModules.Add(TFunctions_TDataHandler);

finalization
  UnRegisterClasses([TDataHandler, TdhParamEdit, TdhConstantParamEdit, TdhDynamicParamEdit, TdhMultiNewItems, TdhMultiNewItem]);
  fsRTTIModules.Remove(TFunctions_TDataHandler);

end.



