unit SBaseVariantFunctions;
{$I config.inc}

interface

uses
  StrUtils, Variants, SysUtils
{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter
{$ENDIF}
{$IFNDEF PACKAGE}
  , DB
{$ENDIF}
  , Classes, FmtBCD, Math;

const
{ закрыто в System.pas }
  varDecimal  = $000E; { vt_decimal     14 } {UNSUPPORTED as of v6.x code base}
  XMLEncodeDateFormat: String = 'yyyy-mm-dd';

type
  TVarArrayIndexException = (vaieExcept, vaieReturnUnassigned, vaieReturnNull);
  TEmptyVariantString = (evsUnassigned, evsNull, evsEmptyString);
  TVarTypeToStringFunction = function(AVarType: TVarType): String;

  function VarTypeToString(AVarType: TVarType): String;
  function XMLEncode(const AValue: Variant): String; overload;
  function XMLEncode(const ANames: String; const AValues: Variant; const ATag: String = ''; const AEncodedNames: PString = nil): String; overload;

  // формирование XML представления вариантного массива
  // пример:
  // VarArrayToXMLRow('F1,F2,F3,F4,F5',
  //                   VarArrayOf([1.0, 'str', 12.1212, StrToDate('01.01.2009'), StrToDateTime('12.01.2009 10:00:01')])));
  // вернет:
  // <row F1="False" F2="string" F3="12.1212" F4="20090101" F5="20090112T10:00:01000" />
  function  VarArrayToXMLRow(const VarArrayNames: String; const VarArrayValues: Variant): String;

  function CastAs(const AValue: Variant; const AVarType: String): Variant;

  function  VarIsPresent(const V: Variant): Boolean;
  function  VarArrayIsPresent(const V: Variant): Boolean;
  function  VarArrayLength(const AVariant: Variant; ADim: Integer = 1): Integer;

  function  IsNull(const V1, V2: Variant): Variant;
  function  NullIf(const Value1, Value2: Variant): Variant;
  function  IsEmpty(const V1, V2: Variant): Variant;
  function  IsNullOrEmpty(const V1, V2: Variant): Variant;

  function  VarIsEqual(const AParam1, AParam2: Variant): Boolean;

  function  VarArrayRead(const AVarNames: String; const AVarArray: Variant; const AReadNames: String; var AUnknownNames: String): Variant; overload;
  function  VarArrayRead(const AVarNames: String; const AVarArray: Variant; const AReadNames: String): Variant; overload;
  function  VarArrayExclude(const AVarNames: String; const AVarArray: Variant; const AExcludedNames: String): Variant;

  procedure VarArrayWrite(var AVarNames: String; var AVarArray: Variant; const AWriteNames: String; const AWriteValues: Variant);
  procedure VarArrayOverWrite(const AVarNames: String; var AVarArray: Variant; const AWriteNames: String; const AWriteValues: Variant);
  function  FnVarArrayOverWrite(const AVarNames: String; const AVarArray: Variant; const AWriteNames: String; const AWriteValues: Variant): Variant;

  procedure VarArrayAppend(var AVarArray: Variant; const AAppendValue: Variant);

  function  VarArrayGetValueByIndex(const AVarArray: Variant; AIndex: Integer; const AIndexException: TVarArrayIndexException = vaieExcept): Variant;
  function  VarArrayGetRangeByIndexes(const AVarArray: Variant; AIndexFrom, AIndexTo: Integer; const AIndexException: TVarArrayIndexException = vaieExcept): Variant;
  function  VarArraysConcat(const AVarArray1, AVarArray2: Variant; ASkipWhenEmpty: Boolean = False): Variant;

{$IFNDEF PACKAGE}
  function  VarArrayToString(AValues: Variant; ADelimeter: Char = ';'): string;
  function  VarFromStringArray(AValues: String; ADelimeter: Char = ','; AEmptyValue: TEmptyVariantString = evsEmptyString): Variant;
{$ENDIF}

  function  IntFromSet(const ASet; const ASize: Byte): Int64;
  function  VarFromSet(const ASet; const ASize: Byte): Variant;
  function  VarFromArrayOfObjects(AArray: TArray<TObject>): Variant;
  function  VarFromArrayReplicate(const AValue: Variant; ACount: Integer): Variant;

  procedure VarToSet(const AVariant: Variant; var ASet; const ASize: Byte);
  function  VarToChar(const V: Variant): Char;
  function  VarToCharDef(const V: Variant; ADefault: Char): Char;
  function  VarToArrayOfChar(const V: Variant): TArray<Char>;
  function  VarToPointer(const AVariant: Variant): Pointer;
  function  VarToBoolDef(const AVariant: Variant; const ADefault: Boolean): Boolean;
  function  VarToIntDef(const AVariant: Variant; const ADefault: Integer): Integer;
  function  VarToExtendedDef(const AVariant: Variant; const ADefault: Extended): Extended;
  function  VarToStrDef(const V: Variant; const ADefault: String): String;
  function  VarToDateTimeDef(const AVariant: Variant; const ADefault: TDateTime): TDateTime;
  function  VarFromDate(const AVariant: TDateTime): Variant; inline;

{$IFDEF FASTSCRIPT_RTTI}
type
  TfsFunctions_Variant = class(TfsRTTIModule)
  private
    function Call_Variant_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

implementation

uses
  SBaseStringFunctions, SBaseConstants;

function VarTypeToString(AVarType: TVarType): String;
begin
  case AVarType of
    varByte
        : Result := 'Int8';
    varSmallint
    , varShortInt
        : Result := 'Int16';

    varInteger
    , varWord
        : Result := 'Int32';

      varInt64
    , varLongWord
        : Result := 'Int64';

      varSingle
    , varDouble
    , varCurrency
    , varDecimal
        : Result := 'Decimal';

      varBoolean
        : Result := 'Bit';

      varDate
        : Result := 'DateTime';

      varString
    , varOleStr
        : Result := 'AnsiString';
      varUString
        : Result := 'WideString';
    else
      Result := '';
  end;
end;

function XMLEncode(const AValue: Variant): String;
var
  Hour, Min, Sec, MSec: Word;
begin
  case VarType(AValue) of
    varBoolean:
      if AValue = True then
        Result := '1'
      else
        Result := '0';
    varDate:
      begin
        Result := FormatDateTime(XMLEncodeDateFormat, VarToDateTime(AValue));
        DecodeTime(VarToDateTime(AValue), Hour, Min, Sec, MSec);

        if (Hour <> 0) or (Min <> 0) or (Sec <> 0) or (MSec <> 0) then
          Result := Result + 'T' + ZeroPredicatedUInt(Hour, 2) + ':' + ZeroPredicatedUInt(Min, 2) + ':' + ZeroPredicatedUInt(Sec, 2);
          if MSec > 0 then
            Result := Result + '.' + ZeroPredicatedUInt(MSec, 3);
      end;
    else
      Result := VarToStr(AValue);
  end;

  Result := SBaseStringFunctions.XMLEncode(Result)
end;

function XMLEncode(const ANames: String; const AValues: Variant; const ATag: String; const AEncodedNames: PString): String;
var
  LItem : TStringItem;
  LValue: Variant;
begin
  Result := '';
  if AEncodedNames <> nil then
    AEncodedNames^ := '';

  for LItem in SBaseStringFunctions.EnumStringItems(ANames) do begin
    if (LItem.Index = 0) and LItem.Eof then
      LValue := AValues
    else
      LValue := AValues[LItem.Index];

    if VarIsPresent(LValue) then begin
      Result := ConcatIfNotEmpty(Result, ' ', LItem.Value + '="' + XMLEncode(LValue) + '"');
      if AEncodedNames <> nil then
        AEncodedNames^ := ConcatIfNotEmpty(AEncodedNames^, ';', LItem.Value);
    end;
  end;

  if ATag <> '' then
    Result := '<' + ATag + ' ' + Result + '/'
end;

function VarArrayToXMLRow(const VarArrayNames: String; const VarArrayValues: Variant): String;
var
  LFieldId, LPos, LPosPred : Integer;
  LFieldName : String;
  LFieldValue: Variant;
begin
  Result := '<row ';

  LPos      := 1;
  LPosPred  := 1;
  LFieldId  := 0;

  while LPos <> 0 do begin
    LPos := PosEx(';', VarArrayNames, LPosPred);
    if LPos = 0 then
      LFieldName := Copy(VarArrayNames, LPosPred, Length(VarArrayNames))
    else
      LFieldName := Copy(VarArrayNames, LPosPred, LPos - LPosPred);

    LPosPred := Succ(LPos);

    if (LFieldId = 0) and (LPos = 0) then
      LFieldValue := VarArrayValues
    else
      LFieldValue := VarArrayValues[LFieldId];

    if not VarIsNull(LFieldValue) then
      Result := Result + LFieldName + '="' + SBaseVariantFunctions.XMLEncode(LFieldValue) + '" ';

    Inc(LFieldId);
  end;

  Result := Result + '/>' + SConst_CR;
end;

function CastAs(const AValue: Variant; const AVarType: String): Variant;
type
  TDateTimeKind = (dtkDate, dtkTime, dtkDateTime, dtkTTime, dtkTDateTime);
var
  LGetVarType, LSetVarType: TVarType;
  LText: String;
  LTextLength: Integer;
  LDateTimeKind: TDateTimeKind;

  function ExtractDate: TDate;
  begin
    if CharInSet(LText[3], ['.', '-', '/']) then      // DD.MM.YYYY
      Result := EncodeDate
                (
                  StrToInt(Copy(LText, 7, 4)),
                  StrToInt(Copy(LText, 4, 2)),
                  StrToInt(Copy(LText, 1, 2))
                )
    else if CharInSet(LText[5], ['.', '-', '/']) then // YYYY.MM.DD
      Result := EncodeDate
                (
                  StrToInt(Copy(LText, 1, 4)),
                  StrToInt(Copy(LText, 6, 2)),
                  StrToInt(Copy(LText, 9, 2))
                )
    else                                              // YYYYMMDD
      Result := EncodeDate
                (
                  StrToInt(Copy(LText, 1, 4)),
                  StrToInt(Copy(LText, 5, 2)),
                  StrToInt(Copy(LText, 7, 2))
                )
  end;

  function ExtractTime(AStartPos: Integer): TDate;
  var
    LMilliSeconds: Word;
  begin
    if LDateTimeKind = dtkDate then
      Exit(0);

    if not (LDateTimeKind in [dtkTTime, dtkTDateTime]) and (LTextLength > AStartPos + 8) and (LText[AStartPos + 8] = '.') then
      LMilliSeconds := StrToInt(Copy(LText, AStartPos + 9, LTextLength - AStartPos - 8))
    else
      LMilliSeconds := 0;
    Result := EncodeTime
              (
                StrToInt(Copy(LText, AStartPos, 2)),
                StrToInt(Copy(LText, AStartPos + 3, 2)),
                StrToInt(Copy(LText, AStartPos + 6, 2)),
                LMilliSeconds
              )

  end;
begin
  LGetVarType := VarType(AValue);

  if (LGetVarType in [varNull, varEmpty]) or AVarType.IsEmpty then
    Exit(AValue)
  else if SameText(AVarType, DelphiTypeString) or SameText(AVarType, SQLTypeNVarChar) then
    LSetVarType := varUString
  else if SameText(AVarType, SQLTypeVarChar) then
    LSetVarType := varString
  else if SameText(AVarType, DelphiTypeShortInt) then
    LSetVarType := varShortInt
  else if SameText(AVarType, SQLtypeTinyInt) or SameText(AVarType, DelphiTypeTinyByte) then
    LSetVarType := varByte
  else if SameText(AVarType, DelphiTypeSmallInt) then
    LSetVarType := varSmallint
  else if SameText(AVarType, DelphiTypeWord) then
    LSetVarType := varWord
  else if SameText(AVarType, DelphiTypeInteger) or SameText(AVarType, SQLtypeInt) then
    LSetVarType := varInteger
  else if SameText(AVarType, DelphiTypeLongWord) then
    LSetVarType := varLongWord
  else if SameText(AVarType, 'Int64') or SameText(AVarType, SQLtypeBigInt) or SameText(AVarType, 'LargeInt') then
    LSetVarType := varInt64
  else if SameText(AVarType, 'Decimal') then
    LSetVarType := varFMTBcd
  else if SameText(AVarType, DelphiTypeBoolean) or SameText(AVarType, SQLtypeBit) then
    LSetVarType := varBoolean
  else if SameText(AVarType, 'Null') then
    LSetVarType := varNull
  else if SameText(AVarType, DelphiTypeDate) then begin
    LSetVarType   := varDate;
    LDateTimeKind := dtkDate;
  end else if SameText(AVarType, DelphiTypeDateTime) then begin
    LSetVarType   := varDate;
    LDateTimeKind := dtkDateTime;
  end else if SameText(AVarType, SQLtypeTime) then begin
    LSetVarType   := varDate;
    LDateTimeKind := dtkTime;
  end else if SameText(AVarType, 'TTime') then begin
    LSetVarType   := varDate;
    LDateTimeKind := dtkTTime;
  end else if SameText(AVarType, 'TDateTime') then begin
    LSetVarType   := varDate;
    LDateTimeKind := dtkTDateTime;
  end else if SameText(AVarType, 'SysName') then begin
    LText := VarToStr(AValue);
    if LText.IsEmpty then
      Exit(LText)
    else
      Exit(QuoteString(LText, '['))
  end else if AVarType = '-' then
    Exit(-AValue)
  else
    Raise Exception.Create('Invalid CastAs(Type): ' + AVarType);

  if (LGetVarType = LSetVarType) then
    Result := AValue
  else if (LGetVarType = varString) or (LGetVarType = varUString) then begin
    LText := AValue;
    LTextLength := Length(LText);
    case LSetVarType of
      varDate:
        try
          if LTextLength = 8 then
            if LText[3] = ':' then
              Result := VarFromDateTime(ExtractTime(1))
            else
              Result := VarFromDateTime(ExtractDate)
          else if LTextLength = 10 then
            Result := VarFromDateTime(ExtractDate)
          else if (LTextLength >= 17) and CharInSet(LText[9], [' ', 'T']) and (LText[12] = ':') then
            Result := VarFromDateTime(ExtractDate + ExtractTime(10))
          else if (LTextLength >= 19) and CharInSet(LText[11], [' ', 'T']) and (LText[14] = ':') then
            Result := VarFromDateTime(ExtractDate + ExtractTime(12))
          else
            Raise Exception.Create('')
        except
          Raise Exception.Create('Invalid Date Value: '#171 + LText + #187);
        end;
      else
        Result := VarAsType(AValue, LSetVarType)
    end;
  end else if (LGetVarType = varBoolean) and (LSetVarType = varInteger) then
    Result := Integer(AValue = True)
  else
    Result := VarAsType(AValue, LSetVarType);
end;

function VarIsPresent(const v: Variant): Boolean;
begin
  Result := not (VarType(v) in [varEmpty, varNull])
end;

function VarArrayIsPresent(const V: Variant): Boolean; //TODO: Многомерные массивы
var
  I: Integer;
begin
  if VarIsArray(V) then begin
    Result := True;
    for I := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
      if VarArrayIsPresent(V[I]) then
        Exit;
    Result := False;
  end else
    Result := VarIsPresent(V);
end;

function VarArrayLength(const AVariant: Variant; ADim: Integer = 1): Integer;
begin
  Result := VarArrayHighBound(AVariant, ADim) - VarArrayLowBound(AVariant, ADim) + 1;
end;

function IsNull(const v1,v2: Variant): Variant;
begin
  if VarType(v1) in [varEmpty, varNull] then
    Result := v2
  else
    Result := v1
end;

function NullIf(const Value1, Value2: Variant): Variant;
begin
  if VarIsEqual(Value1, Value2) then
    Result := Null
  else
    Result := Value1;
end;

function IsEmpty(const V1, V2: Variant): Variant;
begin
  if VarIsEmpty(v1) then
    Result := v2
  else
    Result := v1
end;

function IsNullOrEmpty(const v1,v2: Variant): Variant;
begin
  if VarType(v1) in [varEmpty, varNull] then
    Result := v2
  else
    Result := v1
end;

function VarIsEqual(const AParam1, AParam2: Variant): Boolean;
var
  I: Integer;
  AArray, BArray: Boolean;
  AEmpty, BEmpty: Boolean;
begin
  Result := False;

  AArray := VarIsArray(AParam1);
  BArray := VarIsArray(AParam2);

  if (AArray and BArray)
          and (VarArrayHighBound(AParam1, 1) = VarArrayHighBound(AParam2, 1))
  then begin
    for I := VarArrayHighBound(AParam1, 1) downto 0 do
      if not VarIsEqual(AParam1[I], AParam2[I]) then Exit;
    Result := True;
  end else if ( not AArray ) and ( not BArray ) then begin
    AEmpty := not VarIsPresent(AParam1);
    BEmpty := not VarIsPresent(AParam2);

    try
      if (AEmpty and BEmpty)
              or
         ( (not AEmpty) and (not BEmpty) and (AParam1 = AParam2) )
      then
        Result := True;
    except end;
  end;
end;

function VarArrayRead(const AVarNames: String; const AVarArray: Variant; const AReadNames: String; var AUnknownNames: String): Variant;
var
  LReadValue: Variant;
  LArrayNameItem, LReadNameItem: TStringNameItem;
  LFound: Boolean;
begin
  Result := unAssigned;
  AUnknownNames := '';

  if AReadNames.IsEmpty then Exit;

  for LReadNameItem in EnumStringNameItems(AReadNames, ';', True) do begin
    LReadValue := unAssigned;
    LFound := False;
    for LArrayNameItem in EnumStringNameItems(AVarNames, ';', True) do
      if SameText(LArrayNameItem.NameLeft, LReadNameItem.NameRight) then begin
        if (LArrayNameItem.Index = 0) and LArrayNameItem.Eof then
          LReadValue := AVarArray
        else
          LReadValue := AVarArray[LArrayNameItem.Index];
        LFound := True;
        Break;
      end;

    if (LReadNameItem.Index = 0) then
      Result := LReadValue
    else if LReadNameItem.Index = 1 then
      Result := VarArrayOf([Result, LReadValue])
    else begin
      VarArrayRedim(Result, LReadNameItem.Index);
      Result[LReadNameItem.Index] := LReadValue;
    end;
    if (not LFound) then
      AUnknownNames := ConcatIfNotEmpty(AUnknownNames, ';', LReadNameItem.NameRight)
  end;
end;

function VarArrayRead(const AVarNames: String; const AVarArray: Variant; const AReadNames: String): Variant; overload;
var
  LUnknownNames: String;
begin
  Result := VarArrayRead(AVarNames, AVarArray, AReadNames, LUnknownNames);
end;

function VarArrayExclude(const AVarNames: String; const AVarArray: Variant; const AExcludedNames: String): Variant;
var
  LItem : TStringItem;
  LValue: Variant;
  I: Integer;
begin
  Result := Null;
  I := -1;
  for LItem in SBaseStringFunctions.EnumStringItems(AVarNames) do if not InArray(AExcludedNames, LItem.Value, ';') then begin
    Inc(I);
    if (LItem.Index = 0) and LItem.Eof then
      LValue := AVarArray
    else
      LValue := AVarArray[LItem.Index];

    if (I = 0) then
      Result := LValue
    else begin
      if I = 1 then
        Result := VarArrayOf([Result, LValue])
      else begin
        VarArrayRedim(Result, I);
        Result[I] := LValue;
      end;
    end;
  end;
end;

procedure VarArrayWrite_(AVarNames: PString; var AVarArray: Variant; const AWriteNames: String; const AWriteValues: Variant; const AAppendNewVariables: Boolean);
var
  LWriteValue: Variant;
  LArrayNameItem, LWriteNameItem: TStringNameItem;
begin
  if AWriteNames.IsEmpty then Exit;

  for LWriteNameItem in EnumStringNameItems(AWriteNames, ';', True) do begin
    if (LWriteNameItem.Index = 0) and LWriteNameItem.Eof then
      LWriteValue := AWriteValues
    else
      LWriteValue := AWriteValues[LWriteNameItem.Index];
    for LArrayNameItem in EnumStringNameItems(AVarNames^, ';', True) do
      if SameText(LArrayNameItem.NameLeft, LWriteNameItem.NameLeft) then begin
        if not LArrayNameItem.CastAs.IsEmpty then
          LWriteValue := CastAs(LWriteValue, LArrayNameItem.CastAs);

        if (LArrayNameItem.Index = 0) and LArrayNameItem.Eof then
          AVarArray := LWriteValue
        else
          AVarArray[LArrayNameItem.Index] := LWriteValue;
        Break;
      end;
  end;
end;

procedure VarArrayWrite(var AVarNames: String; var AVarArray: Variant; const AWriteNames: String; const AWriteValues: Variant);
begin
  VarArrayWrite_(@AVarNames, AVarArray, AWriteNames, AWriteValues, True);
end;

procedure VarArrayOverWrite(const AVarNames: String; var AVarArray: Variant; const AWriteNames: String; const AWriteValues: Variant);
begin
  VarArrayWrite_(@AVarNames, AVarArray, AWriteNames, AWriteValues, False);
end;

function FnVarArrayOverWrite(const AVarNames: String; const AVarArray: Variant; const AWriteNames: String; const AWriteValues: Variant): Variant;
begin
  Result := AVarArray;
  VarArrayWrite_(@AVarNames, Result, AWriteNames, AWriteValues, False);
end;

procedure VarArrayAppend(var AVarArray: Variant; const AAppendValue: Variant);
var
  I: Integer;
begin
  if VarIsEmpty(AVarArray) then
    AVarArray := AAppendValue
  else if not VarIsArray(AVarArray) then
    AVarArray := VarArrayOf([AVarArray, AAppendValue])
  else begin
    I := VarArrayHighBound(AVarArray, 1);
    Inc(I);
    VarArrayRedim(AVarArray, I);
    AVarArray[I] := AAppendValue;
  end;
end;

function VarArrayGetValueByIndex(const AVarArray: Variant; AIndex: Integer; const AIndexException: TVarArrayIndexException = vaieExcept): Variant;
var
  LException: Boolean;
begin
  LException := False;

  if VarIsArray(AVarArray) then begin
    if (AIndex < VarArrayLowBound(AVarArray, 1)) or (AIndex > VarArrayHighBound(AVarArray, 1)) then
      LException := True
    else
      Result := AVarArray[AIndex];
  end else if AIndex <> 0 then
    LException := True
  else
    Result := AVarArray;

  if LException then
    case AIndexException of
      vaieReturnUnassigned: Result := Unassigned;
      vaieReturnNull      : Result := Null;
      else Raise Exception.Create('Invalid variant array index');
    end;
end;

function VarArrayGetRangeByIndexes(const AVarArray: Variant; AIndexFrom, AIndexTo: Integer; const AIndexException: TVarArrayIndexException = vaieExcept): Variant;
var
  I: Integer;
begin
  if AIndexFrom = AIndexTo then
    Result := VarArrayGetValueByIndex(AVarArray, AIndexFrom, AIndexException)
  else if AIndexFrom > AIndexTo then
    Raise Exception.Create('Invalid variant array range indexes')
  else begin
    Result := Variants.VarArrayCreate([0, AIndexTo - AIndexFrom], varVariant);
    for I := AIndexFrom to AIndexTo do
      Result[I - AIndexFrom] := VarArrayGetValueByIndex(AVarArray, I, AIndexException);
  end;
end;

function VarArraysConcat(const AVarArray1, AVarArray2: Variant; ASkipWhenEmpty: Boolean): Variant;
var
  LHighBound: Integer;
  LLowBound2, LDim2: Integer;
  I: Integer;
begin
  if ASkipWhenEmpty then begin
    if VarIsEmpty(AVarArray1) then
      Result := AVarArray2
    else if VarIsEmpty(AVarArray2) then
      Result := AVarArray1
  end;

  if VarIsArray(AVarArray1) then
    Result := AVarArray1
  else
    Result := VarArrayOf([AVarArray1]);

  LHighBound := VarArrayHighBound(Result, 1);

  if not VarIsArray(AVarArray2) then begin
    VarArrayRedim(Result, LHighBound + 1);
    Result[LHighBound + 1] := AVarArray2;
  end else begin
    LLowBound2 := VarArrayLowBound(AVarArray2, 1);
    LDim2 := VarArrayHighBound(AVarArray2, 1) - LLowBound2;
    VarArrayRedim(Result, LHighBound + 1 + LDim2);
    for I := 0 to LDim2 do
      Result[LHighBound + 1 + I] := AVarArray2[LLowBound2 + I];
  end;
end;

{$IFNDEF PACKAGE}
function VarArrayToString(AValues: Variant; ADelimeter: Char = ';'): string;
var
  LDims     : Integer;
  LLows     : Array of Integer;
  LHighs    : Array of Integer;
  LIndexes  : Array of Integer;
  LVal      : Variant;

  function _VarArrayToStr(ADim : Integer) : string;
  var
    i         : Integer;
    n1, n2    : Integer;
  begin
    n1 := LLows[ADim-1];
    n2 := LHighs[ADim-1];
    Result := '';
    for i := n1 to n2 do begin
      LIndexes[ADim-1] := i;
      if ADim = LDims then begin
        //-- Мы на последнем уровне
        LVal := VarArrayGet(AValues, LIndexes);
        Result := Result + VarArrayToString(LVal, ADelimeter);
      end else begin
        //-- Не на последнем уровне; поэтому складываем подмассивы
        Result := Result + _VarArrayToStr(ADim + 1);
      end;
      if i < n2 then
        Result := Result + ADelimeter;
    end;
    Result := '[' + Result + ']';
  end;

var
  i         : Integer;
  n1, n2    : Integer;
begin
  Result := '';
  if VarIsEmpty(AValues) or VarIsNull(AValues) then
    Exit;

  if VarIsArray(AValues) then begin
    //-- Читаем кол-во измерений
    LDims := VarArrayDimCount(AValues);
    if LDims = 1 then begin
      //-- Если это одномерный массив, то реализуем простой алгоритм
      n1 := VarArrayLowBound(AValues, 1);
      n2 := VarArrayHighBound(AValues, 1);
      for i := n1 to n2 do begin
        LVal := VarArrayGetValueByIndex(AValues, i);
        Result := Result + VarArrayToString(LVal, ADelimeter);
        if i < n2 then
          Result := Result + ADelimeter;
      end;
      Result := '[' + Result + ']';
    end else begin
      //-- Готовим масивы, в которых будем хранить размерности измерений
      SetLength(LLows, LDims);
      SetLength(LHighs, LDims);
      //-- А также готовим массив индексов
      SetLength(LIndexes, LDims);
      //-- Заполняем Диапазоны и Массив индексов
      for i := 1 to LDims do begin
        n1 := VarArrayLowBound(AValues, i);
        n2 := VarArrayHighBound(AValues, i);
        LLows[i-1] := n1;
        LHighs[i-1] := n2;
        LIndexes[i-1] := n1;
      end;

      Result := _VarArrayToStr(1);
    end;
  end else begin
    //-- Если это не массив, то просто:
    Result := VarToStr(AValues);
  end;
end;

function VarFromStringArray(AValues: String; ADelimeter: Char = ','; AEmptyValue: TEmptyVariantString = evsEmptyString): Variant;
var
  LValue: String;
  LDim: Integer;
begin
  LValue := VarToStr(ExtractAndRemoveValue(AValues, ADelimeter, ftString));
  if AValues = '' then
    if LValue = '' then
      case AEmptyValue of
        evsUnassigned: Result := unAssigned;
        evsNull: Result := Null
        else Result := ''
      end
    else
      Result := LValue
  else begin
    Result := VarArrayOf([LValue]);
    LDim := 0;
    repeat
      LValue := ExtractAndRemoveValue(AValues, ADelimeter, ftString);
      Inc(LDim);
      VarArrayRedim(Result, LDim);
      Result[LDim] := LValue;
    until (AValues = '');
  end;
end;
{$ENDIF}

function IntFromSet(const ASet; const ASize: Byte): Int64;
begin
  Assert( ASize <= SizeOf(Int64) );

  Result := 0;
  Move(ASet, Result, ASize);
end;

function VarFromSet(const ASet; const ASize: Byte): Variant;
var
  LInt64: Int64;
begin
  Assert( ASize <= SizeOf(Int64) );

  LInt64 := 0;
  Move(ASet, LInt64, ASize);

  if ASize = SizeOf(Byte) then
    Result := Byte(LInt64)
  else if ASize = SizeOf(Word) then
    Result := Word(LInt64)
  else if ASize = SizeOf(Integer) then
    Result := Integer(LInt64)
  else
    Result := LInt64;
end;

function VarFromArrayOfObjects(AArray: TArray<TObject>): Variant;
var
  I: Integer;
begin
  I := Pred(Length(AArray));
  Result := VarArrayCreate([0, I], varVariant);
  while I >= 0 do begin
    Result[I] := Integer(AArray[I]);
    Dec(I);
  end;
end;

function VarFromArrayReplicate(const AValue: Variant; ACount: Integer): Variant;
begin
  Dec(ACount);
  Result := VarArrayCreate([0, ACount], varVariant);
  while ACount >= 0 do begin
    Result[ACount] := AValue;
    Dec(ACount);
  end;
end;

procedure VarToSet(const AVariant: Variant; var ASet; const ASize: Byte);
var
  I: Integer;
  LInt64: Int64;
begin
  if not VarIsPresent(AVariant) then
    LInt64 := 0
  else if VarIsArray(AVariant) then begin
    LInt64 := 0;
    for I := VarArrayLowBound(AVariant, 1) to VarArrayHighBound(AVariant, 1) do
      LInt64 := LInt64 or (1 shl Integer(AVariant[I]))
  end else begin
    Assert( VarIsOrdinal(AVariant) );

    LInt64 := AVariant;
  end;

  Move(LInt64, ASet, ASize);
end;

function VarTypeIsOrdinal(const AVarType: TVarType): Boolean;
begin
  Result := AVarType in [varSmallInt, varInteger, varBoolean, varShortInt,
                         varByte, varWord, varLongWord, varInt64, varUInt64];
end;

function VarTypeIsStr(const AVarType: TVarType): Boolean;
begin
  Result := (AVarType = varOleStr) or (AVarType = varString) or (AVarType = varUString);
end;

function VarToBoolDef(const AVariant: Variant; const ADefault: Boolean): Boolean;
var
  LStr    : String;
  LVarType: TVarType;
begin
  LVarType := VarType(AVariant);

  if LVarType = varBoolean then
    Result := (AVariant)
  else if VarTypeIsOrdinal(LVarType) then
    Result := (AVariant <> 0)
  else if VarTypeIsStr(LVarType) then begin
    LStr := VarToStr(AVariant);
    Result := (Length(LStr) > 0) and (SameText(LStr, SConst_Tree) or (LStr = '1'));
  end else if LVarType in [varEmpty, varNull] then
    Result := ADefault
  else
    Raise Exception.Create('VarToBoolDef failed for value '#171 + VarToStr(AVariant) + #187);
end;

function VarToChar(const V: Variant): Char;
begin
  Result := CharFromString(VarToStr(V))
end;

function VarToCharDef(const V: Variant; ADefault: Char): Char;
begin
  if VarType(V) in [varEmpty, varNull] then
    Result := ADefault
  else
    Result := CharFromString(VarToStr(V))
end;

function VarToArrayOfChar(const V: Variant): TArray<Char>;
var
  SValue: String;
  I: Integer;
begin
  if VarType(V) in [varEmpty, varNull] then
    Result := []
  else if VarIsArray(V) then
    Result := V
  else if VarIsStr(V) then begin
    SValue := V;
    SetLength(Result, SValue.Length);
    for I := 1 to SValue.Length do
      Result[Pred(I)] := SValue[I];
  end else
    Raise Exception.Create('Invalid VarToArrayOfChar conversion');
end;

function VarToPointer(const AVariant: Variant): Pointer;
begin
  if VarIsOrdinal(AVariant) then
    Result := Pointer(Integer(AVariant))
  else
    Result := nil
end;

function VarToIntDef(const AVariant: Variant; const ADefault: Integer): Integer;
begin
  if VarIsPresent(AVariant) then try
    Result := Integer(AVariant);
  except
    Result := ADefault;
  end else
    Result := ADefault;
end;

function VarToExtendedDef(const AVariant: Variant; const ADefault: Extended): Extended;
begin
  if VarIsPresent(AVariant) then try
    Result := Extended(AVariant);
  except
    Result := ADefault;
  end
  else
    Result := ADefault;
end;

function VarToStrDef(const V: Variant; const ADefault: String): String;
begin
  if VarIsPresent(V) then
    Result := V
  else
    Result := ADefault;
end;

function VarToDateTimeDef(const AVariant: Variant; const ADefault: TDateTime): TDateTime;
begin
  if VarIsPresent(AVariant) then
    Result := VarToDateTime(AVariant)
  else
    Result := ADefault;
end;

function VarFromDate(const AVariant: TDateTime): Variant;
begin
  Result := VarFromDateTime(Floor(AVariant));
end;

{$IFDEF FASTSCRIPT_RTTI}
{ TfsFunctions_Variant }

constructor TfsFunctions_Variant.Create(AScript: TfsScript);
const
  SCategory: String = 'Variants';
begin
  inherited;

  with AScript do begin
    AddEnum('TEmptyVariantString', 'evsUnassigned, evsNull, evsEmptyString');

    AddMethod('function VarToBoolDef(const AVariant: Variant; const ADefault: Boolean): Boolean', Call_Variant_Method, SCategory);
    AddMethod('function VarToIntDef(const Value: Variant; const Default: Integer): Integer', Call_Variant_Method, SCategory);
    AddMethod('function VarIsEmpty(const Value: Variant): Boolean', Call_Variant_Method, SCategory);
    AddMethod('function VarIsNull(const Value: Variant): Boolean', Call_Variant_Method, SCategory);
    AddMethod('function VarIsPresent(const Value: Variant): Boolean', Call_Variant_Method, SCategory);
    AddMethod('function VarArrayIsPresent(const Value: Variant): Boolean', Call_Variant_Method, SCategory);
    AddMethod('function VarIsArray(const Value: Variant): Boolean', Call_Variant_Method, SCategory);
    AddMethod('function VarIsOrdinal(const Value: Variant): Boolean', Call_Variant_Method, SCategory);
    AddMethod('function VarIsEqual(const AValue1, AValue2: Variant): Boolean', Call_Variant_Method, SCategory);

    AddMethod('function VarIsFloat(const AValue: Variant): Boolean', Call_Variant_Method, SCategory);

    AddMethod('function VarToStrDef(AValue: Variant; ADefault: String): String', Call_Variant_Method, SCategory);
    AddMethod('function VarFromDateTime(AValue: TDateTime): Variant', Call_Variant_Method, SCategory);
    AddMethod('function VarToDateTime(AValue: Variant): TDateTime', Call_Variant_Method, SCategory);
    AddMethod('function VarFMTBcdCreate(AValue: Variant): Variant', Call_Variant_Method, SCategory);

    AddMethod('function VarFromStringArray(AValues: String; ADelimeter: Char = '',''; AEmptyValue: TEmptyVariantString = evsEmptyString): Variant', Call_Variant_Method, SCategory);

    AddMethod('function IsNull(const AValue, AReplaceValue: Variant): Variant', Call_Variant_Method, SCategory);
    AddMethod('function IfThen(const ACondition: Boolean; ATrue, AFalse: Variant): Variant', Call_Variant_Method, SCategory);

//    AddMethod('procedure VarArrayRedim(var A: Variant; HighBound: Integer)', Call_Variant_Method, SCategory);
    AddMethod('function VarArrayHighBound(const A: Variant; Dim: Integer): Integer', Call_Variant_Method, SCategory);
    AddMethod('function VarArrayLowBound(const A: Variant; Dim: Integer): Integer', Call_Variant_Method, SCategory);

    AddMethod('function VarArrayRead(VarArrayNames: String; VarArrayValues: Variant; ReadNames: String): Variant', Call_Variant_Method, SCategory);
    AddMethod('function VarArrayToXMLRow(VarArrayNames: String; VarArrayValues: Variant): String', Call_Variant_Method, SCategory);

    AddMethod('function VarType(const V: Variant): TVarType', Call_Variant_Method, SCategory);
    AddMethod('function VarAsType(const V: Variant; AVarType: TVarType): Variant', Call_Variant_Method, SCategory);
    AddMethod('function VarIsType(const V: Variant; AVarType: TVarType): Boolean', Call_Variant_Method, SCategory);
    AddMethod('function VarIsByRef(const V: Variant): Boolean', Call_Variant_Method, SCategory);

    AddMethod('function VarIsEmpty(const V: Variant): Boolean', Call_Variant_Method, SCategory);
    AddMethod('procedure VarCheckEmpty(const V: Variant)', Call_Variant_Method, SCategory);
    AddMethod('function VarIsNull(const V: Variant): Boolean', Call_Variant_Method, SCategory);
    AddMethod('function VarIsClear(const V: Variant): Boolean', Call_Variant_Method, SCategory);

    AddMethod('function VarIsCustom(const V: Variant): Boolean', Call_Variant_Method, SCategory);
    AddMethod('function VarIsOrdinal(const V: Variant): Boolean', Call_Variant_Method, SCategory);
    AddMethod('function VarIsFloat(const V: Variant): Boolean', Call_Variant_Method, SCategory);
    AddMethod('function VarIsNumeric(const V: Variant): Boolean', Call_Variant_Method, SCategory);
    AddMethod('function VarIsStr(const V: Variant): Boolean', Call_Variant_Method, SCategory);

    AddMethod('function VarFromSet(V: Array Of Variant): Variant', Call_Variant_Method, SCategory);
  end;
end;

function TfsFunctions_Variant.Call_Variant_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  LValue: Variant;
  LInt64: Int64;
begin
  if MethodName = 'VARTOINTDEF' then
    Result := VarToIntDef(Caller.Params[0], Caller.Params[1])
  else if MethodName = 'VARTOBOOLDEF' then
    Result := VarToBoolDef(Caller.Params[0], Caller.Params[1])
  else if MethodName = 'VARISEMPTY' then
    Result := VarIsEmpty(Caller.Params[0])
  else if MethodName = 'VARISNULL' then
    Result := VarIsNull(Caller.Params[0])
  else if MethodName = 'VARISPRESENT' then
    Result := VarIsPresent(Caller.Params[0])
  else if MethodName = 'VARARRAYISPRESENT' then
    Result := VarArrayIsPresent(Caller.Params[0])
  else if MethodName = 'VARISARRAY' then
    Result := VarIsArray(Caller.Params[0])
  else if MethodName = 'VARISORDINAL' then
    Result := VarIsOrdinal(Caller.Params[0])
  else if MethodName = 'VARISEQUAL' then
    Result := VarIsEqual(Caller.Params[0], Caller.Params[1])
  else if MethodName = 'VARFROMSTRINGARRAY' then
    Result := VarFromStringArray(Caller.Params[0], VarToStr(Caller.Params[1])[1], Caller.Params[3])
  else if MethodName = 'VARTOSTRDEF' then
    Result := VarToStrDef(Caller.Params[0], Caller.Params[1])
  else if MethodName = 'VARFROMDATETIME' then
    Result := VarFromDateTime(Caller.Params[0])
  else if MethodName = 'VARTODATETIME' then
    Result := VarToDateTime(Caller.Params[0])
  else if MethodName = 'VARFMTBCDCREATE' then begin
    LValue := Caller.Params[0];
    if (not VarIsPresent(LValue)) or (VarIsStr(LValue) and VarToStr(LValue).IsEmpty) then
      Result := NULL
    else
      Result := VarFMTBcdCreate(StrToBcd(VarToStr(Caller.Params[0])))
  end else if MethodName = 'ISNULL' then
    if VarIsPresent(Caller.Params[0]) then
      Result := Caller.Params[0]
    else
      Result := Caller.Params[1]
  else if MethodName = 'IFTHEN' then begin
    if Caller.Params[0] = True then
      Result := Caller.Params[1]
    else
      Result := Caller.Params[2]
  end else if MethodName = 'VARARRAYHIGHBOUND' then
    Result := VarArrayHighBound(Caller.Params[0], Caller.Params[1])
  else if MethodName = 'VARARRAYLOWBOUND' then
    Result := VarArrayLowBound(Caller.Params[0], Caller.Params[1])
  else if MethodName = 'VARARRAYREAD' then begin
    Result := VarArrayRead(Caller.Params[0], Caller.Params[1], Caller.Params[2]);
  end else if MethodName = 'VARARRAYTOXMLROW' then
    Result := VarArrayToXMLRow(Caller.Params[0], Caller.Params[1])

  else if MethodName = 'VARTYPE' then
    Result := VarType(Caller.Params[0])
  else if MethodName = 'VARASTYPE' then
    Result := VarAsType(Caller.Params[0], TVarType(Caller.Params[0]))
  else if MethodName = 'VARISTYPE' then
    Result := VarIsType(Caller.Params[0], TVarType(Caller.Params[0]))
  else if MethodName = 'VARISBYREF' then
    Result := VarIsByRef(Caller.Params[0])
  else if MethodName = 'VARISEMPTY' then
    Result := VarIsEmpty(Caller.Params[0])
  else if MethodName = 'VARCHECKEMPTY' then
    VarCheckEmpty(Caller.Params[0])
  else if MethodName = 'VARISNULL' then
    Result := VarIsNull(Caller.Params[0])
  else if MethodName = 'VARISCLEAR' then
    Result := VarIsClear(Caller.Params[0])
  else if MethodName = 'VARISCUSTOM' then
    Result := VarIsCustom(Caller.Params[0])
  else if MethodName = 'VARISORDINAL' then
    Result := VarIsOrdinal(Caller.Params[0])
  else if MethodName = 'VARISFLOAT' then
    Result := VarIsFloat(Caller.Params[0])
  else if MethodName = 'VARISNUMERIC' then
    Result := VarIsNumeric(Caller.Params[0])
  else if MethodName = 'VARISSTR' then
    Result := VarIsStr(Caller.Params[0])
  else if MethodName = 'VARFROMSET' then begin
    LInt64 := 0;
    VarToSet(Caller.Params[0], LInt64, SizeOf(LInt64));
    Result := LInt64;
  end;
end;

{$ENDIF}

initialization
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_Variant);
{$ENDIF}

end.
