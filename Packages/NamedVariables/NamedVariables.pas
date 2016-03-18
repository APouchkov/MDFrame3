unit NamedVariables;
{$I config.inc}

interface

uses
  SysUtils, Classes, Types, Contnrs, Variants, StrUtils, FMTBcd, DB,
  SBaseUtils, SBaseConstants,
  SBaseVariantFunctions
{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter
{$ENDIF}
  ;

type
  TNamedVariant = record
  private
    FName : String;
    FValue: Variant;
  public
    const TypeName: String = 'TNamedVariant';

    constructor Create(const AName: String; const AValue: Variant); overload;
    constructor Create(const AItem: String); overload;

    property Name : String read FName write FName;
    property Value: Variant read FValue write FValue;
    function IsNull: Boolean;
    function AsClrTParam: String; inline;
  end;

//  TNamedVariant = TNamedVariable<Variant>;
  PNamedVariant = ^TNamedVariant;

  TNamedVariantsCustomEnumerator = class
  protected
    FPointer: Pointer;

    function GetCurrent: TNamedVariant; virtual; abstract;
  public
    constructor Create(AValues: Pointer);

    function MoveNext: Boolean; dynamic; abstract;
    property Current: TNamedVariant read GetCurrent;
  end;

  TNamedVariantsEnumerator = class(TNamedVariantsCustomEnumerator)
  strict private
    FIndex  : Integer;
  public
    constructor Create(AValues: Pointer);

    function GetCurrent: TNamedVariant; override;
    function MoveNext: Boolean; override;
  end;

  TNamedVariants = record
    class operator Add(A, B: TNamedVariants): TNamedVariants;
    class operator Equal(A, B: TNamedVariants): Boolean;
    class operator NotEqual(A, B: TNamedVariants): Boolean;

    function GetEnumerator: TNamedVariantsCustomEnumerator;
  private
    FValues: Array of TNamedVariant;
    FUnknownAsNull: Boolean;

    function  GetItem(Index: Integer): TNamedVariant;
    procedure SetItem(Index: Integer; const Value: TNamedVariant);

    function  GetAsVariant: Variant;
    procedure SetAsVariant(const AValue: Variant);

    function  GetNamesString: String;
    procedure SetUnknownAsNull(const Value: Boolean);
  public
    const TypeName: String = 'TNamedVariants';

    constructor Create(AUnknownAsNull: Boolean); overload;
    constructor Create(const AItems: Array Of TNamedVariant; AUnknownAsNull: Boolean = False); overload;
    constructor Create(const AItems: TNamedVariants; AUnknownAsNull: Boolean = False); overload;
    constructor Create(const AItems1: TNamedVariants; const AItems2: Array Of TNamedVariant; AUnknownAsNull: Boolean = False); overload;
    constructor Create(const ANames: String; const AValues: Variant; AUnknownAsNull: Boolean = False); overload;
    constructor Create(const AStringArray: String; AUnknownAsNull: Boolean = False); overload;
    constructor Create(const AVariant: Variant; AUnknownAsNull: Boolean = False); overload;

    procedure Clear;
    procedure ClearUndefined;

    function AddValues(AValues: TNamedVariants; AAppendMode: TAppendMode = amOverride): TNamedVariants; overload;

    procedure AssignValues(AValues: Array of TNamedVariant; AAppendMode: TAppendMode = amOverride); overload;
    procedure AssignValues(AValues: TNamedVariants; AAppendMode: TAppendMode = amOverride); overload;
    procedure AssignValues(AParams: TParams; AAppendMode: TAppendMode = amOverride); overload;
    procedure AssignValues(AFields: TFields; AAppendMode: TAppendMode = amOverride); overload;
    procedure AssignValues(ANames: String; const AValues: Variant; AAppendMode: TAppendMode = amOverride; AUseSynonym: Boolean = False); overload;

    function  Count: Integer; inline;
    function  IsEmpty: Boolean; inline;
    function  IsValueEqual(const AName: String; const AValue: Variant): Boolean;

    function  Find(const AName: string; var AIndex: Integer): Boolean; overload;
    function  Find(const AName: string; var AItem: TNamedVariant): Boolean; overload;

    function  ReadValues(AVarNames: string; var AUnknownVarNames: String; AUseSynonym: Boolean): Variant;
    procedure WriteValues(AVarNames: String; const AValues: Variant; AUseSynonym: Boolean);

    function  GetValues(AVarNames: String): Variant;
    procedure SetValues(AVarNames: String; AValues: Variant);
    property  Values[VarNames: string]: Variant read GetValues write SetValues; default;

    procedure Extract(var ANames: String; var AValues: Variant); overload;
    function  Copy(const ANames: String; AUseSynonym: Boolean = False): TNamedVariants; overload;

//    function  ExtractValue(const AVarName: String): Variant;
    function  GetValue(const AVarName: String; out AFound: Boolean): Variant; overload;
    function  GetValue(const AVarName: String): Variant; overload;
    procedure SetValue(AVarName: String; AValue: Variant; AAppendMode: TAppendMode = amOverride); overload;
    procedure SetValue(AVarIndex: Integer; AValue: Variant; AAppendMode: TAppendMode); overload;

    procedure ReName(const AOldVarName, ANewVarName: string);
    procedure Delete(Index: integer);
    function  EqualVariables(const AVarList: TNamedVariants; const AVarNames: String): Boolean;
    function  Contains(const AVarList: TNamedVariants): Boolean;

    property  UnknownAsNull: Boolean read FUnknownAsNull write SetUnknownAsNull;
    property  Items[Index: Integer]: TNamedVariant read GetItem write SetItem;

    property  AsVariant: Variant read GetAsVariant write SetAsVariant;
    property  NamesString: String read GetNamesString;

    function  Differences(const ATarget: TNamedVariants): String;
    function  AsXML(AXMLMode: TXMLMode; AVarTypeToStringFunction: TVarTypeToStringFunction = nil; const ATag: String = ''): String;

    function  GetAsClrTParams: String; overload;
    function  GetAsClrTParams(AItems: String): String; overload;

    procedure SetAsClrTParams(const AValue: String);
    property  AsClrTParams: String read GetAsClrTParams write SetAsClrTParams;
  end;

  PNamedVariants = ^TNamedVariants;

  TNamedVariantsObject = class
  private
  public
    Variables: TNamedVariants;
    constructor Create(AVariables: TNamedVariants);
    //property Variables: TNamedVariants read FVariables;
  end;

  TVariantArrayOfNamedValues = record
  private
    FUnknownAsNull: Boolean;
    FValues: Variant;
    function ReadValues(VarNames: string): Variant;
  public
    constructor Create(AUnknownAsNull: Boolean);
    class function GetValueByName(AValues: Variant; const AName: String; AUnknownAsNull: Boolean = False): Variant; static;

    property Values[VarNames: string]: Variant read ReadValues; default;
  end;


  TGetVarValuesFunction = function(const AFieldNames: String; var AUnknownFields: String): Variant of object;

{$IFDEF FASTSCRIPT_RTTI}
  TfsFunctions_NamedVariants = class(TfsRTTIModule)
  private
    FfsNamedVariants: TfsRecords<TNamedVariants>;

    function Call_TNamedVariant_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function Call_TNamedVariants_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Call_TVariantArrayOfNamedValues_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function Get_TNamedVariants_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TNamedVariants_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
  public
//    class function IsNamedVariants(APointer: Pointer): Boolean;

    constructor Create(AScript: TfsScript); override;
//    destructor Destroy; override;
  end;
{$ENDIF}

  function CastAsVariant(AValue: TMethod): Variant; overload;
  function CastAsVariant(AValue: TObject): Variant; overload;
  function CastAsClrTParam(const AName: String; AValue: Variant): String; overload;

implementation

uses
  SBaseStringFunctions
{$IFDEF DATETIMEOFFSET_SUPPORT}
  , DateTimeOffset
{$ENDIF}
;

{ TNamedVariant }

constructor TNamedVariant.Create(const AName: String; const AValue: Variant);
begin
  FName  := AName;
  FValue := AValue;
end;

function TNamedVariant.AsClrTParam: String;
begin
  Result := CastAsClrTParam(Name, Value)
end;

constructor TNamedVariant.Create(const AItem: String);
const
  SError_InvalidPacked: String = 'Invalid TNamedVariant Packed String: ';
var
  LEQPosition, LCastAsPosition: Integer;
  LName, LCastAs, LValue: String;
begin
  if AItem.IsEmpty then
    Raise Exception.Create('Empty TNamedVariant Packed String present');

  if AItem[1] <> '[' then begin
    LEQPosition := Pos('=', AItem);
    if (LEQPosition <= 1) then
      Raise Exception.Create(SError_InvalidPacked + AItem);

    LName := System.Copy(AItem, 1, LEQPosition - 1);
//    FPosition := LEQPosition + 1;
    LCastAsPosition := PosExRevert(':', LName);
    if LCastAsPosition > 0 then begin
      LCastAs := System.Copy(LName, LCastAsPosition + 1, LName.Length - LCastAsPosition);
      LName := System.Copy(LName, 1, LCastAsPosition - 1);
    end else
      LCastAs := '';
  end else begin
    LEQPosition := 2;
    if (not ParseEOQ(']', AItem, LEQPosition, LName, [':', '='])) then
      Raise Exception.Create(SError_InvalidPacked + AItem);

    if (AItem[LEQPosition] = ':') then begin
      LCastAsPosition := LEQPosition;
      LEQPosition := PosEx('=', AItem, LCastAsPosition + 1);
      if (LEQPosition = 0) or (LEQPosition = LCastAsPosition + 1) then
        Raise Exception.Create(SError_InvalidPacked + AItem);
      LCastAs := System.Copy(AItem, LCastAsPosition + 1, LEQPosition - LCastAsPosition - 1);
    end else
      LCastAs := '';
  end;

  LValue := System.Copy(AItem, LEQPosition + 1, AItem.Length - LEQPosition);

  FName := LName;
  if LCastAs.IsEmpty then
    FValue := LValue
  else
    FValue := CastAs(LValue, LCastAs);
end;

function TNamedVariant.IsNull: Boolean;
begin
  Result := VarType(FValue) in [varEmpty, varNull]
end;

{ TNamedVariants }

constructor TNamedVariants.Create(AUnknownAsNull: Boolean);
begin
  FUnknownAsNull := AUnknownAsNull;
  Clear;
end;

constructor TNamedVariants.Create(const AItems: array of TNamedVariant; AUnknownAsNull: Boolean = False);
var
  I: Integer;
begin
  Create(AUnknownAsNull);

  I := Length(AItems);
  SetLength(FValues, I);
  while I > 0 do begin
    Dec(I);
    FValues[I] := AItems[I];
  end;
end;

constructor TNamedVariants.Create(const AItems: TNamedVariants; AUnknownAsNull: Boolean = False);
var
  I: Integer;
begin
  Create(AUnknownAsNull);

  I := AItems.Count;
  SetLength(FValues, I);
  while I > 0 do begin
    Dec(I);
    FValues[I] := AItems.Items[I];
  end;
end;

constructor TNamedVariants.Create(const AItems1: TNamedVariants; const AItems2: Array Of TNamedVariant; AUnknownAsNull: Boolean = False);
begin
  Create(AItems1, AUnknownAsNull);
  AssignValues(AItems2);
end;


constructor TNamedVariants.Create(const AVariant: Variant; AUnknownAsNull: Boolean = False);
begin
  Create(AUnknownAsNull);

  if VarIsOrdinal(AVariant) then begin
    if Integer(AVariant) <> 0 then
      AssignValues(PNamedVariants(Pointer(Integer(AVariant)))^)
  end else
    AsVariant := AVariant;
end;

constructor TNamedVariants.Create(const AStringArray: String; AUnknownAsNull: Boolean);
begin
  Create(AUnknownAsNull);

  SetAsClrTParams(AStringArray);
end;

constructor TNamedVariants.Create(const ANames: String; const AValues: Variant; AUnknownAsNull: Boolean);
begin
  Create(AUnknownAsNull);

  AssignValues(ANames, AValues);
end;

procedure TNamedVariants.Clear;
begin
  SetLength(FValues, 0)
end;

procedure TNamedVariants.ClearUndefined;
var
  I: Integer;
begin
  for I := Count-1 downto 0 do
    if VarType(FValues[I].Value) in [varEmpty, varNull] then
      Delete(I);
end;

procedure TNamedVariants.SetUnknownAsNull(const Value: Boolean);
var
  I: Integer;
begin
  FUnknownAsNull := Value;
  if FUnknownAsNull then
    for I := Pred(Count) downto 0 do
      if VarIsEmpty(FValues[I].Value) then
        Delete(I);
end;

procedure TNamedVariants.AssignValues(AValues: array of TNamedVariant; AAppendMode: TAppendMode);
var
  I: Smallint;
begin
  for I := Pred(Length(AValues)) downto 0 do
    SetValue(AValues[I].Name, AValues[I].Value, AAppendMode);
end;

function TNamedVariants.AddValues(AValues: TNamedVariants; AAppendMode: TAppendMode = amOverride): TNamedVariants;
begin
  Result := Self;
  Result.AssignValues(AValues, AAppendMode);
end;

procedure TNamedVariants.AssignValues(AValues: TNamedVariants; AAppendMode: TAppendMode);
var
  I: Smallint;
begin
  for I := Pred(AValues.Count) downto 0 do
    with AValues.Items[I] do
      SetValue(Name, Value, AAppendMode);
end;

procedure TNamedVariants.AssignValues(AParams: TParams; AAppendMode: TAppendMode = amOverride);
var
  I: Integer;
begin
  for I := Pred(AParams.Count) downto 0 do
    with AParams[I] do
      SetValue(Name, Value, AAppendMode);
end;

procedure TNamedVariants.AssignValues(AFields: TFields; AAppendMode: TAppendMode);
var
  I: Integer;
begin
  for I := Pred(AFields.Count) downto 0 do
    with AFields[I] do
      SetValue(FieldName, AsVariant, AAppendMode);
end;

class operator TNamedVariants.Add(A, B: TNamedVariants): TNamedVariants;
begin
  Result := A;
  Result.FUnknownAsNull := A.UnknownAsNull and B.UnknownAsNull;
  Result.AssignValues(B, amRaiseIfRepeated);
end;

procedure TNamedVariants.AssignValues(ANames: String; const AValues: Variant; AAppendMode: TAppendMode; AUseSynonym: Boolean);
var
  LItem     : TStringItem;
  LNameItem : TStringNameItem;
  LValue: Variant;
  LValueIsNull: Boolean;
begin
  LValueIsNull := VarType(AValues) in [varNull, varEmpty];

  if AUseSynonym then
    for LNameItem in EnumStringNameItems(ANames, ';', True) do begin
      if LValueIsNull or ((LNameItem.Index = 0) and LNameItem.Eof) then
        LValue := AValues
      else
        LValue := AValues[LNameItem.Index];

      if not LNameItem.CastAs.IsEmpty then
        LValue := CastAs(LValue, LNameItem.CastAs);

      SetValue(LNameItem.NameLeft, LValue, AAppendMode);
    end
  else
    for LItem in EnumStringItems(ANames, ';', False) do begin
      if LValueIsNull or ((LItem.Index = 0) and LItem.Eof) then
        LValue := AValues
      else
        LValue := AValues[LItem.Index];

      SetValue(LItem.Value, LValue, AAppendMode)
    end
end;

function TNamedVariants.GetAsClrTParams(AItems: String): String;
var
  I: integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    with FValues[I] do
      if (not IsNull) and InArray(AItems, Name, ';') then
        Result := ConcatIfNotEmpty(Result, ';',  AsClrTParam);
end;

function TNamedVariants.AsXML(AXMLMode: TXMLMode; AVarTypeToStringFunction: TVarTypeToStringFunction; const ATag: String): String;
var
  I: integer;
begin
  Assert((AXMLMode = xmlmAttributes) or (ATag <> ''));
  Result := '';
  if (AXMLMode = xmlmRecords) and not Assigned(AVarTypeToStringFunction) then
    AVarTypeToStringFunction := VarTypeToString;

  for I := 0 to Count - 1 do
    with FValues[I] do
      if (not IsNull) then
        case AXMLMode of
          xmlmAttributes:
            Result := ConcatIfNotEmpty(Result, ' ', FName + '="' + SBaseVariantFunctions.XMLEncode(FValue) + '"');
          xmlmRecords:
            Result := ConcatIfNotEmpty
                      (
                        Result,
                        SConst_CR,
                        '<' + ATag + ' '
                            + XMLConst_NAME + '="' + FName + '" '
                            + XMLConst_VALUE + '="' + SBaseVariantFunctions.XMLEncode(FValue) + '" '
                            + XMLConst_TYPE + '="' + AVarTypeToStringFunction(VarType(FValue)) + '"'
                        + '/>'
                      );
        end;

  if (AXMLMode = xmlmAttributes) and (ATag <> '') then
    Result := '<' + ATag + ' ' + Result + '/>';
end;

function TNamedVariants.Count: Integer;
begin
  Result := Length(FValues);
end;

function TNamedVariants.IsEmpty: Boolean;
begin
  Result := (Count = 0);
end;

function TNamedVariants.IsValueEqual(const AName: String; const AValue: Variant): Boolean;
var
  LItem: TNamedVariant;
begin
  Result := Find(AName, LItem);
  if Result then
    Result := VarIsEqual(AValue, LItem.Value)
end;

function TNamedVariants.Find(const AName: string; var AIndex: Integer): Boolean;
var
  I: Integer;
begin
  for I := Pred(Count) downto 0 do
    if AnsiSameText(FValues[I].Name, AName) then begin
      AIndex  := I;
      Exit(True);
    end;
  Result := False;
end;

function TNamedVariants.Find(const AName: string; var AItem: TNamedVariant): Boolean;
var
  I: Integer;
begin
  for I := Pred(Count) downto 0 do
    if AnsiSameText(FValues[I].Name, AName) then begin
      AItem  := FValues[I];
      Exit(True);
    end;
  Result := False;
end;

function TNamedVariants.GetItem(Index: Integer): TNamedVariant;
begin
  Result := FValues[Index]
end;

procedure TNamedVariants.SetItem(Index: Integer; const Value: TNamedVariant);
begin
  FValues[Index] := Value;
end;

function TNamedVariants.GetAsClrTParams: String;
begin
  Result := GetAsClrTParams('*')
end;

procedure TNamedVariants.SetAsClrTParams(const AValue: String);
var
  LItem: TStringNamedItem;
const
  SError_InvalidString: String = 'Invalid TParams(Clr) String: ';
begin
  Clear;

  for LItem in EnumStringNamedItems(AValue, ';', True) do begin
    SetLength(FValues, LItem.Index + 1);
    FValues[LItem.Index] :=
      TNamedVariant.Create
      (
        LItem.Name,
        CastAs(LItem.Value, LItem.CastAs)
      );
  end;
end;

procedure TNamedVariants.ReName(const AOldVarName, ANewVarName: string);
var
  Idx, I: integer;
begin
  for Idx := 0 to Count - 1 do
    if SameText(FValues[Idx].Name, AOldVarName) then begin
      if Find(ANewVarName, I) then
        Raise Exception.Create('Duplicate name was found while processing rename operation.')
      else
        FValues[Idx].Name := ANewVarName;
      Break;
    end;
end;

function TNamedVariants.GetValue(const AVarName: String; out AFound: Boolean): Variant;
var
  Idx: integer;
begin
  AFound := Find(AVarName, Idx);
  if AFound then
    Result := FValues[Idx].Value
  else begin
    if FUnknownAsNull then
      Result := Null
    else
      Result := unAssigned;
  end;
end;

function TNamedVariants.GetValue(const AVarName: String): Variant;
var
  LFound: Boolean;
begin
  Result := GetValue(AVarName, LFound)
end;

function TNamedVariants.GetValues(AVarNames: String): Variant;
var
  LParamItem: TStringItem;
  LValue: Variant;
  LFound: Boolean;
begin
{$IFDEF NAMED_VARIANTS_DISABLE_EQ_AT_NAME}
  Assert(Pos('=', AVarNames) = 0);
{$ENDIF}

  for LParamItem in EnumStringItems(AVarNames, ';') do begin
    LValue := GetValue(LParamItem.Value, LFound);
    if (LParamItem.Index = 0) then
      Result := LValue
    else if LParamItem.Index = 1 then
      Result := VarArrayOf([Result, LValue])
    else begin
      VarArrayRedim(Result, LParamItem.Index);
      Result[LParamItem.Index] := LValue;
    end;
  end;
end;

function TNamedVariants.ReadValues(AVarNames: String; var AUnknownVarNames: String; AUseSynonym: Boolean): Variant;
var
  LParamItem: TStringNameItem;
  LValue: Variant;
  LFound: Boolean;
begin
  AUnknownVarNames := '';

  for LParamItem in EnumStringNameItems(AVarNames, ';', True) do begin
    LValue := GetValue(LParamItem.NameRight, LFound);
    if (LParamItem.Index = 0) then
      Result := LValue
    else if LParamItem.Index = 1 then
      Result := VarArrayOf([Result, LValue])
    else begin
      VarArrayRedim(Result, LParamItem.Index);
      Result[LParamItem.Index] := LValue;
    end;
    if (not LFound) then
      AUnknownVarNames := ConcatIfNotEmpty(AUnknownVarNames, ';', LParamItem.NameFull);
  end;
end;

procedure TNamedVariants.WriteValues(AVarNames: string; const AValues: Variant; AUseSynonym: Boolean);
var
  LParamItem: TStringNameItem;
  LValue: Variant;
begin
  if not AUseSynonym then
    AssignValues(AVarNames, AValues, amOverride)
  else
    for LParamItem in EnumStringNameItems(AVarNames, ';', True) do begin
      if (LParamItem.Index = 0) and LParamItem.Eof then
        LValue := AValues
      else
        LValue := AValues[LParamItem.Index];

      if not LParamItem.CastAs.IsEmpty then
        LValue := CastAs(LValue, LParamItem.CastAs);

      SetValue(LParamItem.NameLeft, LValue, amOverride);
    end;
end;

procedure TNamedVariants.SetValues(AVarNames: string; AValues: Variant);
begin
{$IFDEF NAMED_VARIANTS_DISABLE_EQ_AT_NAME}
  Assert(Pos('=', AVarNames) = 0);
{$ENDIF}
  AssignValues(AVarNames, AValues, amOverride);
end;

procedure TNamedVariants.SetValue(AVarName: String; AValue: Variant; AAppendMode: TAppendMode);
var
  Idx: integer;
  LVarType : TVarType;
  LVarValue: Variant;
begin
  LVarType := VarType(AValue);
  if Find(AVarName, Idx) then
    if AAppendMode in [amRaiseIfRepeated, amRaiseIfChanged] then begin
      if (AAppendMode = amRaiseIfRepeated) or (not VarIsEqual(FValues[Idx].Value, AValue)) then
        Raise Exception.Create('Variable '#171 + AVarName + #187' already exists');
    end else if (LVarType = varEmpty) or (FUnknownAsNull and (LVarType = varNull)) then begin
      if AAppendMode = amOverride then
        Delete(Idx)
    end else begin
      LVarValue := FValues[Idx].Value;

      if AAppendMode = amAggregate then
        if VarType(LVarValue) in [varEmpty, varNull] then
          FValues[Idx].Value := AValue
        else if not (VarType(AValue) in [varEmpty, varNull]) then
          FValues[Idx].Value := LVarValue + AValue;

      if (AAppendMode = amOverride) or (VarType(LVarValue) in [varEmpty, varNull]) then
        FValues[Idx].Value := AValue
    end
  else if not ((LVarType = varEmpty) or (FUnknownAsNull and (LVarType = varNull))) then begin
    Idx := Count;
    SetLength(FValues, Idx + 1);
    with FValues[Idx] do begin
      Name := AVarName;
      Value:= AValue;
    end;
  end;
end;

procedure TNamedVariants.SetValue(AVarIndex: Integer; AValue: Variant; AAppendMode: TAppendMode);
var
  LVarType : TVarType;
  LVarValue: Variant;
begin
  LVarType := VarType(AValue);

  if (LVarType = varEmpty) or (FUnknownAsNull and (LVarType = varNull)) then begin
    if AAppendMode = amOverride then
      Delete(AVarIndex)
  end else begin
    LVarValue := FValues[AVarIndex].Value;

    if AAppendMode = amAggregate then
      if VarType(LVarValue) in [varEmpty, varNull] then
        FValues[AVarIndex].Value := AValue
      else
        FValues[AVarIndex].Value := LVarValue + AValue;

    if (AAppendMode = amOverride) or (VarType(LVarValue) in [varEmpty, varNull]) then
      FValues[AVarIndex].Value := AValue
  end
end;

procedure TNamedVariants.Extract(var ANames: String; var AValues: Variant);
var
  LLength: Integer;
begin
  LLength := Count;

  if LLength = 0 then begin
    ANames  := '';
    AValues := unAssigned;
  end else if LLength = 1 then begin
    with FValues[0] do begin
      ANames  := Name;
      AValues := Value;
    end;
  end else begin
    ANames  := '';
    AValues := VarArrayCreate([0, Pred(LLength)], varVariant);

    repeat
      Dec(LLength);
      with FValues[LLength] do begin
        if ANames = '' then
          ANames := Name
        else
          ANames := Name + ';' + ANames;

        AValues[LLength] := Value;
      end;
    until LLength = 0;
  end;
end;

procedure TNamedVariants.Delete(Index: integer);
var
  L: Integer;
begin
  L := Count;
  if Index >= L then
    Raise Exception.Create('TNamedVariants: Invalid index');

  Dec(L);
  while Index < L do begin
    FValues[Index] := FValues[Succ(Index)];
    Inc(Index);
  end;

  SetLength(FValues, L);
end;

function TNamedVariants.Differences(const ATarget: TNamedVariants): String;
var
  I, LIndex: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
    with FValues[I] do
      if not VarIsEqual(FValue, ATarget[FName]) then
        Result := ConcatIfNotEmpty(Result, ';', FName);

  for I := 0 to ATarget.Count - 1 do
    with ATarget.Items[I] do
      if (not IsNull) and (not Find(Name, LIndex)) then
        Result := ConcatIfNotEmpty(Result, ';', Name);
end;

function TNamedVariants.GetAsVariant: Variant;
var
  LItem : Variant;
  I, LCount: Integer;
begin
  LCount := Pred(Count);
  if LCount >= 0 then begin
    Result := VarArrayCreate([0, LCount], varVariant);
    LItem  := VarArrayCreate([0, 1], varVariant);

    for I := 0 to LCount do begin
      with FValues[I] do begin
        LItem[0] := Name;
        LItem[1] := Value;
      end;
      Result[I] := LItem;
    end;
  end else
    Result := unAssigned;
end;

procedure TNamedVariants.SetAsVariant(const AValue: Variant);
var
  LItem : Variant;
  I, LCount: Integer;
begin
  if not VarIsArray(AValue) then
    Clear
  else begin
    LCount := VarArrayHighBound(AValue, 1);
    if LCount >= 0 then begin
      SetLength(FValues, Succ(LCount));

      for I := 0 to LCount do begin
        LItem := AValue[I];
        with FValues[I] do begin
          Name  := LItem[0];
          Value := LItem[1];
        end;
      end;
    end else
      SetLength(FValues, 0);
  end;
end;

function TNamedVariants.Contains(const AVarList: TNamedVariants): Boolean;
var
  I: Integer;
begin
  for I := 0 to AVarList.Count - 1 do with AVarList.FValues[I] do
    if not VarIsEqual(Values[Name], Value) then Exit(False);
  Result := True;
end;

function TNamedVariants.EqualVariables(const AVarList: TNamedVariants; const AVarNames: String): Boolean;

  function ExtractName(const ANames: String; var Pos: Integer): String;
  var
    I: Integer;
  begin
    I := Pos;
    while (I <= Length(ANames)) and (ANames[I] <> ';') do Inc(I);
    Result := System.Copy(ANames, Pos, I - Pos).Trim;
    if (I <= Length(ANames)) and (ANames[I] = ';') then Inc(I);
    Pos := I;
  end;

var
  i: Integer;
  SelfCount, OtherCount: Integer;
  AName: String;
begin
  SelfCount   := Count;
  OtherCount  := AVarList.Count;

  if (SelfCount = 0) and (OtherCount = 0) then
    Result := True
  else if Pos(';', AVarNames) <> 0 then begin
    I := 1;
    Result := True;
    while I <= Length(AVarNames) do begin
      AName  := ExtractName(AVarNames, I);
      Result := VarIsEqual(Values[AName], AVarList.Values[AName]);
      if not Result then Break;
    end;
  end else
    Result := VarIsEqual(Values[AVarNames], AVarList.Values[AVarNames]);
end;

function TNamedVariants.GetEnumerator: TNamedVariantsCustomEnumerator;
begin
  Result := TNamedVariantsEnumerator.Create(@Self);
end;

class operator TNamedVariants.Equal(A, B: TNamedVariants): Boolean;
var
  I: Integer;
begin
  Result := (A.Count = B.Count);

  if Result and (A.Count > 0) then
    for I := Pred(A.Count) downto 0 do
      with A.Items[I] do begin
        Result := VarIsEqual(Value, B.Values[Name]);
        if not Result then break;
      end
end;

class operator TNamedVariants.NotEqual(A, B: TNamedVariants): Boolean;
begin
  Result := not (A = B)
end;

function TNamedVariants.Copy(const ANames: String; AUseSynonym: Boolean): TNamedVariants;
var
  LParamItem: TStringItem;
  LParamNameItem: TStringNameItem;
begin
  Result := TNamedVariants.Create(UnknownAsNull);

  if AUseSynonym then
    for LParamNameItem in EnumStringNameItems(ANames, ';', True) do
      Result.SetValue(LParamNameItem.NameLeft, CastAs(GetValue(LParamNameItem.NameRight), LParamNameItem.CastAs), amRaiseIfRepeated)
  else
    for LParamItem in EnumStringItems(ANames, ';') do
      Result.SetValue(LParamItem.Value, GetValue(LParamItem.Value), amRaiseIfRepeated)
end;

//function TNamedVariants.ExtractValue(const AVarName: String): Variant;
//var
//  LIdx: Integer;
//begin
//  if Find(AVarName, LIdx) then begin
//    Result := FValues[LIdx].Value;
//    Delete(LIdx)
//  end else if UnknownAsNull then
//    Result := Null
//  else
//    Result := UnAssigned
//end;

function TNamedVariants.GetNamesString: String;
var
  I: Integer;
begin
  Result := '';

  for I := 0 to Count - 1 do
    if Result.IsEmpty then
      Result := FValues[I].Name
    else
      Result := Result + ';' + FValues[I].Name
end;

{ TNamedVariantsObject }

constructor TNamedVariantsObject.Create(AVariables: TNamedVariants);
begin
  Variables := AVariables;
  inherited Create;
end;

{ TVariantItems }

constructor TVariantArrayOfNamedValues.Create(AUnknownAsNull: Boolean);
begin
  FUnknownAsNull := AUnknownAsNull;
end;

class function TVariantArrayOfNamedValues.GetValueByName(AValues: Variant; const AName: String; AUnknownAsNull: Boolean = False): Variant;
var
  I: Integer;
begin
  if VarIsArray(AValues) then
    for I := VarArrayLowBound(AValues, 1) to VarArrayHighBound(AValues, 1) do
      if SameText(AValues[I][0], AName) then begin
        Result := AValues[I][1];
        Exit;
      end;

  if AUnknownAsNull then
    Result := Null
  else
    Result := unAssigned
end;

function TVariantArrayOfNamedValues.ReadValues(VarNames: string): Variant;
begin
  Result := GetValueByName(FValues, VarNames, FUnknownAsNull);
end;

{$IFDEF FASTSCRIPT_RTTI}
{ TfsFunctions_NamedVariants }

//class function TfsFunctions_NamedVariants.IsNamedVariants(APointer: Pointer): Boolean;
//begin
//  if APointer = nil then Exit(False);
//  if FfsNamedVariants.fi then
//end;

constructor TfsFunctions_NamedVariants.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddRecordType(TNamedVariant.TypeName) do begin
    AddProperty(SConst_Name, DelphiTypeString, Word(@PNamedVariant(nil).Name));
    AddProperty(SConst_Value, DelphiTypeVariant, Word(@PNamedVariant(nil).Value));
    AddMethod('function AsClrTParam: String', Call_TNamedVariant_Method);
    AddMethod('function CreateClrTParam(Name: String; Value: Variant): String', Call_TNamedVariant_Method);
  end;

  with AScript.AddRecordType('TVariantArrayOfNamedValues') do begin
    AddMethod('function GetValueByName(AValues: Variant; const AName: String; AUnknownAsNull: Boolean = False): Variant', Call_TVariantArrayOfNamedValues_Method);
  end;

  FfsNamedVariants := TfsRecords<TNamedVariants>.Register(AScript, TNamedVariants.TypeName);
  with FfsNamedVariants do begin
    AddConstructor('constructor Create(AUnknownAsNull: Boolean = False)', Call_TNamedVariants_Method);
    AddConstructor('constructor CreateFromArray(AItems: Variant; AUnknownAsNull: Boolean = False)', Call_TNamedVariants_Method);

    AddMethod('procedure Free', Call_TNamedVariants_Method);
    AddMethod('procedure Clear', Call_TNamedVariants_Method);
    AddMethod('function Count: Integer', Call_TNamedVariants_Method);
    AddMethod('procedure AssignValues(AValues: TNamedVariants; AAppendMode: TAppendMode = amOverride)', Call_TNamedVariants_Method);
    AddMethod('procedure WriteValues(AVarNames: String; const AValues: Variant; AUseSynonym: Boolean = False)', Call_TNamedVariants_Method);

    AddDefaultProperty(SConst_Values, DelphiTypeString, DelphiTypeVariant, Call_TNamedVariants_Method, False);
    AddIndexProperty(SConst_Items, DelphiTypeInteger, TNamedVariant.TypeName, Call_TNamedVariants_Method, True);
    AddProperty('AsClrTParams', DelphiTypeString, Get_TNamedVariants_Property, Set_TNamedVariants_Property);

    AddMethod('function Find(const VarName: String; var Idx: Integer): Boolean', Call_TNamedVariants_Method);
    AddMethod('procedure Delete(const Index: Integer)', Call_TNamedVariants_Method);
    AddMethod('function GetAsClrTParams(AItems: String): String', Call_TNamedVariants_Method);
  end;
end;

//destructor TfsFunctions_NamedVariants.Destroy;
//begin
//  inherited;
//  FreeAndNil(FfsNamedVariants);
//end;

function TfsFunctions_NamedVariants.Get_TNamedVariants_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'ASCLRTPARAMS' then
    Result := PNamedVariants(Instance)^.AsClrTParams
end;

procedure TfsFunctions_NamedVariants.Set_TNamedVariants_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
  if PropName = 'ASCLRTPARAMS' then
    PNamedVariants(Instance)^.AsClrTParams := Value
end;

function TfsFunctions_NamedVariants.Call_TNamedVariant_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'ASCLRTPARAM' then
    Result := PNamedVariant(Instance)^.AsClrTParam
  else if MethodName = 'CREATECLRTPARAM' then
    Result := TNamedVariant.Create(Caller.Params[0], Caller.Params[1]).AsClrTParam;
end;

function TfsFunctions_NamedVariants.Call_TNamedVariants_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  Idx: Integer;
  LNamedVariants: TNamedVariants;
begin
  if MethodName = 'CREATE' then begin
    PNamedVariants(Instance)^.UnknownAsNull := Caller.Params[0] = True;
    Result := Integer(Instance);
  end else if MethodName = 'CREATEFROMARRAY' then begin
    PNamedVariants(Instance)^.UnknownAsNull := Caller.Params[1] = True;
    PNamedVariants(Instance)^.AsVariant := Caller.Params[0];
    Result := Integer(Instance);
  end else if MethodName = 'ASSIGNVALUES' then
    if VarIsOrdinal(Caller.Params[0]) then
      PNamedVariants(Instance)^.AssignValues(PNamedVariants(VarToPointer(Caller.Params[0]))^, Caller.Params[1])
    else begin
      LNamedVariants := TNamedVariants.Create(False);
      LNamedVariants.AsVariant := Caller.Params[0];
      PNamedVariants(Instance)^.AssignValues(LNamedVariants, Caller.Params[1]);
    end
  else if MethodName = 'FREE' then
    FfsNamedVariants.Delete(Pointer(Instance))
  else if MethodName = 'CLEAR' then
    PNamedVariants(Instance)^.Clear
  else if MethodName = 'COUNT' then
    Result := PNamedVariants(Instance)^.Count
  else if MethodName = 'VALUES.GET' then
    Result := PNamedVariants(Instance)^.Values[Caller.Params[0]]
  else if MethodName = 'VALUES.SET' then
    PNamedVariants(Instance)^.Values[Caller.Params[0]] := Caller.Params[1]
  else if MethodName = 'ITEMS.GET' then
    Result := Integer(@(PNamedVariants(Instance)^.FValues[Integer(Caller.Params[0])]))
  else if MethodName = 'FIND' then begin
    Result := PNamedVariants(Instance)^.Find(Caller.Params[0], Idx);
    Caller.Params[1] := Idx;
  end else if MethodName = 'DELETE' then
    PNamedVariants(Instance)^.Delete(Caller.Params[0])
  else if MethodName = 'GETASCLRTPARAMS' then
    Result := PNamedVariants(Instance)^.GetAsClrTParams(Caller.Params[0])
  else if MethodName = 'WRITEVALUES' then
    PNamedVariants(Instance)^.WriteValues(Caller.Params[0], Caller.Params[1], VarToBoolDef(Caller.Params[2], False))
end;

function TfsFunctions_NamedVariants.Call_TVariantArrayOfNamedValues_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'GETVALUEBYNAME' then
    Result := TVariantArrayOfNamedValues.GetValueByName(Caller.Params[0], Caller.Params[1], Caller.Params[2] = True)
end;

{$ENDIF}

{ Procedures and functions }

function CastAsVariant(AValue: TMethod): Variant;
begin
  Result := VarArrayOf([Integer(AValue.Code), Integer(AValue.Data)])
end;

function CastAsVariant(AValue: TObject): Variant;
begin
  Result := Integer(Pointer(AValue))
end;

function CastAsClrTParam(const AName: String; AValue: Variant): String;
var
  LVarType: TVarType;
begin
  LVarType := VarType(AValue);
  if LVarType in [varEmpty, varNull] then
    Result := ''
  else begin
    Result := QuoteString(AName, '[');
    case LVarType of
      varBoolean: Result := Result + ':' + SQLtypeBit + '=' + IfThen(AValue = True, '1', '0');

      varByte: Result := Result + ':' + SQLtypeTinyInt + '=' + VarToStr(AValue);

      varShortInt,
      varSmallint: Result := Result + ':' + SQLtypeSmallInt + '=' + VarToStr(AValue);

      varWord,
      varInteger: Result := Result + ':' + SQLtypeInt + '=' + VarToStr(AValue);

      varLongWord,
      varInt64: Result := Result + ':' + SQLtypeBigInt + '=' + VarToStr(AValue);

      varSingle,
      varDouble,
      varCurrency,
      varDecimal: Result := Result + ':' + SQLtypeFloat + '=' + VarToStr(AValue);

      varOleStr,
      varString,
      varUString:  Result := Result + '=' + QuoteString(VarToStr(AValue), '"');

      varDate:  Result := Result + ':' + SQLtypeDateTime + '=' + QuoteString(TParamDateTimeFormat(AValue), '"');

      else
        if LVarType = VarFMTBcd then
          Result := Result + ':' + SQLtypeDecimal + '=' + BcdToStr(VarToBcd(AValue))
{$IFDEF DATETIMEOFFSET_SUPPORT}
        else if LVarType = VarDateTimeOffset then
          Result := Result + ':' + SQLtypeDateTimeOffset + '=' + QuoteString(VarToStr(AValue), '"')
{$ENDIF}
        else
          Raise Exception.Create('Cannot convert variant type '#171 + VarToStr(LVarType) + #187' into TParam variable format');
    end;
  end;
end;

{ TNamedVariantsCustomEnumerator }

constructor TNamedVariantsCustomEnumerator.Create(AValues: Pointer);
begin
  FPointer := AValues;
end;

{ TNamedVariantsEnumerator }

constructor TNamedVariantsEnumerator.Create(AValues: Pointer);
begin
  inherited;

  FIndex   := -1;
end;

function TNamedVariantsEnumerator.GetCurrent: TNamedVariant;
begin
  Result := PNamedVariants(FPointer).FValues[FIndex]
end;

function TNamedVariantsEnumerator.MoveNext: Boolean;
begin
  if FIndex < PNamedVariants(FPointer).Count - 1 then begin
    Inc(FIndex);
    Result := True;
  end else
    Result := False;
end;

initialization
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_NamedVariants);
{$ENDIF}

finalization
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TfsFunctions_NamedVariants);
{$ENDIF}

end.
