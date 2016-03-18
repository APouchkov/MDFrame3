unit SBaseEventPropEditor;

interface

uses
  DesignIntf,
  DesignEditors,
  Messages, Classes, TypInfo;

type
  TNestedEventProperty = class(TMethodProperty)
  protected
    function GetInstance: TPersistent; virtual; abstract;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetName: string; override;
    procedure GetProperties(Proc: TGetPropProc); override;
    function GetValue: string; override;
    property Instance: TPersistent read GetInstance;
  end;

  TNestedParentElementEventProperty = class(TNestedProperty)
  private
    FOwner: TPersistent;
    FPropInfo: PPropInfo;
    FParent: TPropertyEditor;
    function GetInstance: TPersistent;
  protected
    constructor Create(Parent: TPropertyEditor; AOwner: TPersistent; APropInfo: PPropInfo); reintroduce;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetProperties(Proc: TGetPropProc); override;
    function GetValue: string; override;
    function GetName: string; override;
    property Instance: TPersistent read GetInstance;
  end;

  TNestedElementEventProperty = class(TNestedProperty)
  private
    FParent: TPropertyEditor;
    FPropInfo: PPropInfo;
    function GetTrimmedEventName: string;
    function GetPersistentClassNames: string;
    function GetInstance: TPersistent;
  protected
    constructor Create(Parent: TPropertyEditor; APropInfo: PPropInfo); reintroduce;
    function GetFormMethodName: string; virtual;
    property Instance: TPersistent read GetInstance;
  public
    function AllEqual: Boolean; override;
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetEditLimit: Integer; override;
    function GetName: string; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const AValue: string); override;
  end;

implementation

uses
  Windows, SysUtils,
  DB, StrUtils,
  DesignConst
{$IFNDEF PACKAGE}
  Contnrs, Controls, Forms,
{$ENDIF}
  ;

function NestedEventProperty_GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly, paRevertable];
end;

procedure icNestedEventProperty_GetProperties(APropertyEditor: TPropertyEditor;
  AInstance: TPersistent; Proc: TGetPropProc);
var
  APropList: TPropList;
  I, ACount : Integer;
  AClassType: TClass;
begin
  if AInstance = nil then Exit;
  ACount := GetPropList(AInstance.ClassInfo, [tkMethod], @APropList);
  for I := 0 to ACount - 1 do
    Proc(TNestedElementEventProperty.Create(APropertyEditor, APropList[I]));
  ACount := GetPropList(AInstance.ClassInfo, [tkClass], @APropList);
  for I := 0 to ACount - 1 do
  begin
    AClassType := GetTypeData(APropList[i].PropType^)^.ClassType;
    if not (AClassType.InheritsFrom(TComponent)) and
      (AClassType.InheritsFrom(TPersistent)) and
      (GetPropList(AClassType.ClassInfo, [tkMethod], nil) > 0) then
      Proc(TNestedParentElementEventProperty.Create(APropertyEditor, AInstance, APropList[I]));
  end;
end;

function icNestedEventProperty_GetValue(AInstance: TPersistent): String;
begin
  if AInstance = nil then
    Result := '(None)'
  else
    Result := '(' + AInstance.ClassName + ')';
end;

{ TNestedEventProperty }

function TNestedEventProperty.GetAttributes: TPropertyAttributes;
begin
  Result := NestedEventProperty_GetAttributes;
end;

function TNestedEventProperty.GetName: string;
begin
  Result := inherited GetName;
  Result := Copy(Result, 1, Pos('Events', Result) - 1);
end;

procedure TNestedEventProperty.GetProperties(Proc: TGetPropProc);
begin
  icNestedEventProperty_GetProperties(Self, Instance, Proc);
end;

function TNestedEventProperty.GetValue: string;
begin
  Result := icNestedEventProperty_GetValue(Instance);
end;

{ TNestedParentElementEventProperty }

constructor TNestedParentElementEventProperty.Create(Parent: TPropertyEditor;
  AOwner: TPersistent; APropInfo: PPropInfo);
begin
  inherited Create(Parent);
  FParent := Parent;
  FOwner := AOwner;
  FPropInfo := APropInfo;
end;

function TNestedParentElementEventProperty.GetInstance: TPersistent;
begin
  Result := TPersistent(GetObjectProp(FOwner, FPropInfo));
end;

function TNestedParentElementEventProperty.GetAttributes: TPropertyAttributes;
begin
  Result := NestedEventProperty_GetAttributes;
end;

procedure TNestedParentElementEventProperty.GetProperties(Proc: TGetPropProc);
begin
  icNestedEventProperty_GetProperties(Self, Instance, Proc);
end;

function TNestedParentElementEventProperty.GetValue: String;
begin
  Result := icNestedEventProperty_GetValue(Instance);
end;

function TNestedParentElementEventProperty.GetName: String;
begin
  Result := String(FPropInfo.Name);
end;

{ TNestedElementEventProperty }

constructor TNestedElementEventProperty.Create(Parent: TPropertyEditor; APropInfo: PPropInfo);
begin
  inherited Create(Parent);
  FPropInfo := APropInfo;
  FParent := Parent;
end;

function TNestedElementEventProperty.AllEqual: Boolean;
var
  I: Integer;
  V, T: TMethod;
begin
  Result := False;
  if PropCount > 1 then
  begin
    V := GetMethodValue;
    for I := 1 to PropCount - 1 do
    begin
      T := GetMethodValueAt(I);
      if (T.Code <> V.Code) or (T.Data <> V.Data) then Exit;
    end;
  end;
  Result := True;
end;

procedure TNestedElementEventProperty.Edit;
var
  FormMethodName: string;
begin
  FormMethodName := GetValue;
  if (FormMethodName = '') or
    Designer.MethodFromAncestor(GetMethodValue) then
  begin
    if FormMethodName = '' then
      FormMethodName := GetFormMethodName;
    if FormMethodName = '' then
      raise EPropertyError.CreateRes(@SCannotCreateName);
    SetValue(FormMethodName);
  end;
  Designer.ShowMethod(FormMethodName);
end;

function TNestedElementEventProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paSortList, paRevertable];
end;

function TNestedElementEventProperty.GetEditLimit: Integer;
begin
  Result := MaxIdentLength;
end;

function TNestedElementEventProperty.GetName: String;
begin
  Result := String(FPropInfo.Name);
end;

function TNestedElementEventProperty.GetValue: String;
begin
  Result := Designer.GetMethodName(GetMethodProp(Instance, FPropInfo));
end;

procedure TNestedElementEventProperty.GetValues(Proc: TGetStrProc);
begin
  Designer.GetMethods(GetTypeData(FPropInfo.PropType^), Proc);
end;

procedure TNestedElementEventProperty.SetValue(const AValue: string);

  procedure CheckChainCall(const MethodName: string; Method: TMethod);
  var
    Persistent: TPersistent;
    Component: TComponent;
    InstanceMethod: string;
    Instance: TComponent;
  begin
    Persistent := GetComponent(0);
    if Persistent is TComponent then
    begin
      Component := TComponent(Persistent);
      if (Component.Name <> '') and (Method.Data <> Designer.GetRoot) and
        (TObject(Method.Data) is TComponent) then
      begin
        Instance := TComponent(Method.Data);
        InstanceMethod := Instance.MethodName(Method.Code);
        if InstanceMethod <> '' then
          Designer.ChainCall(MethodName, Instance.Name, InstanceMethod,
            GetTypeData(GetPropType));
      end;
    end;
  end;

var
  NewMethod: Boolean;
  CurValue: string;
  OldMethod: TMethod;
begin
  CurValue:= GetValue;
  if (CurValue <> '') and (AValue <> '') and ((CompareText(CurValue, AValue) = 0) or
    not Designer.MethodExists(AValue)) and not Designer.MethodFromAncestor(GetMethodValue) then
    Designer.RenameMethod(CurValue, AValue)
  else
  begin
    NewMethod := (AValue <> '') and not Designer.MethodExists(AValue);
    OldMethod := GetMethodValue;
    SetMethodProp(Instance, FPropInfo, Designer.CreateMethod(AValue, GetTypeData(FPropInfo.PropType^)));
    if NewMethod then
    begin
      if (PropCount = 1) and (OldMethod.Data <> nil) and (OldMethod.Code <> nil) then
        CheckChainCall(AValue, OldMethod);
      Designer.ShowMethod(AValue);
    end;
    Designer.Modified;
  end;
end;

function TNestedElementEventProperty.GetFormMethodName: string;
var
  I: Integer;
begin
  if GetComponent(0) = Designer.GetRoot then
  begin
    Result := Designer.GetRootClassName;
    if (Result <> '') and (Result[1] = 'T') then
      Delete(Result, 1, 1);
  end
  else
  begin
    Result := Designer.GetObjectName(GetComponent(0));
    for I := Length(Result) downto 1 do
      if CharInSet(Result[I], ['.', '[', ']', '-', '>']) then
        Delete(Result, I, 1);
  end;
  if Result = '' then
    raise EPropertyError.CreateRes(@SCannotCreateName);
  Result := Result + GetPersistentClassNames + GetTrimmedEventName;
end;

function TNestedElementEventProperty.GetTrimmedEventName: string;
begin
  Result := GetName;
  if (Length(Result) >= 2) and CharInSet(Result[1], ['O', 'o']) and
    CharInSet(Result[2], ['N', 'n']) then
    Delete(Result, 1, 2);
end;

function TNestedElementEventProperty.GetPersistentClassNames: string;
var
  APropertyEditor: TPropertyEditor;
begin
  Result := '';
  APropertyEditor := self;
  while APropertyEditor <> nil do
  begin
    if APropertyEditor is TNestedParentElementEventProperty then
      APropertyEditor := TNestedParentElementEventProperty(APropertyEditor).FParent
    else
      if APropertyEditor is TNestedElementEventProperty then
        APropertyEditor := TNestedElementEventProperty(APropertyEditor).FParent
      else APropertyEditor := nil;
    if APropertyEditor <> nil then
      Result := APropertyEditor.GetName + Result;
  end;
end;

function TNestedElementEventProperty.GetInstance: TPersistent;
begin
  Result := nil;
  if FParent is TNestedEventProperty then
    Result := TNestedEventProperty(FParent).Instance;
  if FParent is TNestedParentElementEventProperty then
    Result := TNestedParentElementEventProperty(FParent).Instance;
end;

end.
