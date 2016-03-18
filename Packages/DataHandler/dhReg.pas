unit dhReg;

interface

procedure Register;

implementation

uses
  DataHandler, dhDesignEditors, Classes, DesignEditors, DesignIntf, DHEditFrm,
  ItemPropStorage, dxBar, psDesignEditors;

resourcestring
  STabName = 'MD Project Tools';

{ TcxCustomGridTableItemPropertiesProperty }

type
  TDataHandlerSupportClassProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

function TDataHandlerSupportClassProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
//  Result := False;
  for I := 0 to PropCount - 1 do
  begin
    Result := TDataHandler(GetComponent(I)).Support <> nil;
    if not Result then Exit;
  end;
  Result := True;
end;

function TDataHandlerSupportClassProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable, paVolatileSubProperties];
end;

function TDataHandlerSupportClassProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := TDataHandler(GetComponent(0)).SupportClassName
  else
    Result := '';
end;

procedure TDataHandlerSupportClassProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  LDesc: String;
begin
  for I := 0 to TCustomDataHandlerSupport.RegisteredClassCount - 1 do
  begin
    LDesc := TCustomDataHandlerSupport.GetRegisteredClassDescription(I);
    if LDesc <> '' then
      Proc(LDesc);
  end;
end;

procedure TDataHandlerSupportClassProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
    TDataHandler(GetComponent(I)).SupportClassName := Value;

//  TDataHandler(GetComponent(0)).SupportClassName := Value;
  Modified;
end;

procedure Register;
begin
  RegisterNoIcon([TSubDataHandler]);
  RegisterComponents(STabName, [TDataHandler, TdhItemPropStorage]);
  RegisterComponentEditor(TDataHandler, TDHComponentEditor);
  RegisterComponentEditor(TdhItemPropStorage, TItemComponentEditor);

  RegisterPropertyEditor(TypeInfo(string), TDataHandler, 'SupportClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomDataHandlerSupport), TDataHandler, 'Support', TDataHandlerSupportClassProperty);

  RegisterPropertyEditor(TypeInfo(string), TdhAction, 'ClassName', TDHEditFormProperty);
  RegisterPropertyEditor(TypeInfo(TdxBarItem), TdxItemPropStorage, 'Item', TdxItemProperty);
  RegisterPropertyEditor(TypeInfo(TDHFieldNames), TCustomDataHandler, 'KeyFieldNames', TDHFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(TDHFieldNames), TCustomDataHandler, 'DetailKeyFieldNames', TDHFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(TDHFieldNames), TSubDataHandler, 'MasterKeyFieldNames', TDHMasterFieldNameProperty);
//  RegisterPropertyEditor(TypeInfo(TdxImageIndex), TdhDefaultImages, '', TdxImageIndexPropery);

end;                                        

end.
