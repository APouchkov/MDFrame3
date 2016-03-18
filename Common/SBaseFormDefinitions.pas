unit SBaseFormDefinitions;

interface
uses
  Classes, NamedVariables, cxStyles;

const
  SBFSelectMode: String = 'SELECT#MODE';

  SErrorUnsupportedISBaseInterface: String = 'Class '#171'%s'#187' unregistred or does not support ISBaseInterface.';
  SErrorClassHasNoPublishedMethod: String = 'Class '#171'%s'#187' has no published method %s.';

type
  TSelectMode = (smNone, smSingle, smSingleRepeateAble, smMultiple, smMultipleRepeateAble);
  TCallBackSelectAction = (cbsaSingleRecord, cbsaMultipleRecords, cbsaMultipleBegin, cbsaMultipleRecord, cbsaMultipleEnd);
  TCallBackSelectFunction = procedure(ASelectAction: TCallBackSelectAction; const AResultValues: TNamedVariants) of object;

  ISBaseInterface = interface
    ['{FD6E2DB7-BD1B-4833-BDCD-81AC2B3DD894}']
    function  IVirtualClassOwner(var AVirtualClassName: String; var AParams: TNamedVariants): TPersistentClass;

    function  ICreateByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants): TComponent; overload;
    function  ISelectByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants; ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants;
    procedure IShow;

    function  IGetCreateParams: TNamedVariants;
    property  CreateParams: TNamedVariants read IGetCreateParams;
  end;
  TIVirtualClassOwner = function (var AVirtualClassName: String; var AParams: TNamedVariants): TPersistentClass of Object;
  TICreateByParams = function (const AClass: TPersistentClass; const AOwner: TComponent; const AParams: TNamedVariants): TComponent of Object;
  TISelectByParams = function (const AClass: TPersistentClass; const AOwner: TComponent; const AParams: TNamedVariants; const ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants of Object;

  TControlColors = packed record
    WriteAble, ReadOnly, HighlightRecord, DeletedRecord: TcxStyle;
  end;

  TControlDefaultModifyState = (bfcsDefaultWriteAble, bfcsWriteAbleIfEditing, bfcsWriteAble, bfcsReadOnly);
  TControlModifyState = (mdstNotSpecified, mdstDefaultWriteAble, mdstWriteAble, mdstReadOnly);

  ISBaseReadOnlyInterface = interface
    ['{25832973-AF62-4D00-B5FC-A20DAAFC8288}']
    procedure IUpdateState(AColors: TControlColors);
  end;

var
  NilCallBackSelectFunction: TCallBackSelectFunction = nil;

  procedure RegisterVirtualClassOwner(AClass: TPersistentClass);

  function IFindClassMethod(AClassName: String; const AMethodName: String; out AClass: TPersistentClass; var AParams: TNamedVariants): TMethod;
  function IFindClassCreateMethod(const AClassName: String; out AClass: TPersistentClass; var AParams: TNamedVariants): TMethod;
  function IFindClassSelectMethod(const AClassName: String; out AClass: TPersistentClass; var AParams: TNamedVariants): TMethod;

  function ICreateByParams(const AClassName: String; const AOwner: TComponent; AParams: TNamedVariants): TComponent; overload;
  function ICreateByParams(const AClassName: String; const AOwner: TComponent; const AParams: Array of TNamedVariant): TComponent; overload;
  function ISelectByParams(const AClassName: String; const AOwner: TComponent; AParams: TNamedVariants; const ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants;

implementation

uses
  SysUtils, SBaseForm, SBaseConstants, SBaseUtils, Generics.Collections;

var
  SBaseInterfaceVirtualClassOwners: TList<TPair<TPersistentClass, Pointer>>;

function GetMethodAddress(AClass: TPersistentClass; const AMethod: String): Pointer;
begin
  Result := AClass.MethodAddress(AMethod);
  if Result = nil then
    Raise Exception.CreateFmt(SErrorClassHasNoPublishedMethod, [AClass.ClassName, AMethod]);
end;

procedure RegisterVirtualClassOwner(AClass: TPersistentClass);
var
  LPair: TPair<TPersistentClass, Pointer>;
begin
  LPair.Key := AClass;
  LPair.Value := GetMethodAddress(AClass, 'IVirtualClassOwner');

  if SBaseInterfaceVirtualClassOwners = nil then
    SBaseInterfaceVirtualClassOwners := TList<TPair<TPersistentClass, Pointer>>.Create;

  SBaseInterfaceVirtualClassOwners.Add(LPair);
end;

function IFindClassMethod(AClassName: String; const AMethodName: String; out AClass: TPersistentClass; var AParams: TNamedVariants): TMethod;
var
//  I: Integer;
  LPersistentClass: TPersistentClass;
  LPair: TPair<TPersistentClass, Pointer>;
begin
  AClass := nil;
  Result.Data := nil;
  Result.Code := nil;

  LPersistentClass := GetClass(AClassName);
  if (LPersistentClass = nil) then begin
    for LPair in SBaseInterfaceVirtualClassOwners do begin
      Result.Code := LPair.Value;
      AClass := TIVirtualClassOwner(Result)(AClassName, AParams);
      if Assigned(AClass) then begin
        if not SameText(AClass.ClassName, AClassName) then
          AParams.SetValue(SConst_ClassName, AClassName, amRaiseIfRepeated);
        Break;
      end;
    end
  end else if Supports(LPersistentClass, ISBaseInterface) then
    AClass := LPersistentClass;

  if AClass = nil then
    Raise Exception.CreateFmt(SErrorUnsupportedISBaseInterface, [AClassName]);

  Result.Code := GetMethodAddress(AClass, AMethodName);
end;

function IFindClassCreateMethod(const AClassName: String; out AClass: TPersistentClass; var AParams: TNamedVariants): TMethod;
begin
  Result := IFindClassMethod(AClassName, 'ICreateByParams', AClass, AParams);
end;

function ICreateByParams(const AClassName: String; const AOwner: TComponent; AParams: TNamedVariants): TComponent;
var
  LPersistentClass: TPersistentClass;
  LMethod: TMethod;
begin
  LMethod := IFindClassCreateMethod(AClassName, LPersistentClass, AParams);
  Result  := TICreateByParams(LMethod)(LPersistentClass, AOwner, AParams);
end;

function ICreateByParams(const AClassName: String; const AOwner: TComponent; const AParams: Array of TNamedVariant): TComponent;
begin
  Result := ICreateByParams(AClassName, AOwner, TNamedVariants.Create(AParams, True));
end;

function IFindClassSelectMethod(const AClassName: String; out AClass: TPersistentClass; var AParams: TNamedVariants): TMethod;
begin
  Result := IFindClassMethod(AClassName, 'ISelectByParams', AClass, AParams);
end;

function ISelectByParams(const AClassName: String; const AOwner: TComponent; AParams: TNamedVariants; const ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants;
var
  LPersistentClass: TPersistentClass;
  LMethod: TMethod;
begin
  //Result := TNamedVariants.Create(False);

  LMethod := IFindClassSelectMethod(AClassName, LPersistentClass, AParams);
  Result := TISelectByParams(LMethod)(LPersistentClass, AOwner, AParams, ACallBackProcedure);
end;

initialization

finalization
  FreeAndNil(SBaseInterfaceVirtualClassOwners);

end.
