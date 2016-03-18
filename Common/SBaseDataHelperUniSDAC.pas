unit SBaseDataHelperUniSDAC;
{$I config.inc}

interface
uses
  Classes, SBaseDataHelperInternal, DB, StrUtils;

type
  TSQLServerUniProviderInterpreter = class(TInternalProviderInterpreter)
  public
    class function VarTypeToDataBaseType(AVarType: TVarType): String; override;
    class function VariantToDataBaseString(const AValue: Variant; const ABinaryStringFormat: Boolean = False) : String; override;

    class function FieldTypeToDataBaseType(AField: TField): String; override;
    class function DataBaseDateFormat(ADate: Variant; AQuoted: Boolean = True): String; override;
    class function DataBaseTimeFormat(ADateTime: Variant; AQuoted: Boolean = True): String; override;
    class function DataBaseDateTimeFormat(ADateTime: Variant; AQuoted: Boolean = True): String; override;

    class function GenerateCreateTableScript(ADataSet: TDataSet; const ATableName: String; AStatusFieldName: String = ''): String; override;
    class function GenerateInsertTableScript(ADataSet: TDataSet; const ATableName: String; AStatusFieldName: String = ''): String; override;
    class function GenerateClearTableScript(const ATableName: String): String; override;
    class function GenerateDropTableScript(const ATableName: String): String; override;
    class function GenerateReceiveScript(const Queue: string; const TimeoutMSec: Integer): String; override;

    class function GenerateGetTranCountScript: String; override;
    class function GenerateRollBackTranScript: String; override;
    class function GenerateRollBackTranIfPresentScript: String; override;
  end;

implementation

uses SBaseVariantFunctions, SQLServerUniProvider, OLEDBAccessUni, MSClassesUni,
  SBaseConstants, Variants, SysUtils, DateUtils, SBaseStringFunctions, FmtBcd;

const
  SConst_SQLDateFormat    : String = 'yyyymmdd';
  SConst_SQLTimeFormat    : String = 'hh:nn:ss.zzzzzzz';

{ TSQLServerUniProviderInterpreter }

class function TSQLServerUniProviderInterpreter.DataBaseDateFormat(ADate: Variant; AQuoted: Boolean): String;
begin
  case VarType(ADate) of
    varNull, varEmpty:
      begin
        Result := SConst_NULL;
        Exit;
      end;
    varDate:
      Result := FormatDateTime(SConst_SQLDateFormat, VarToDateTime(ADate))
    else
      Result := FormatDateTime(SConst_SQLDateFormat, ADate);
  end;

  if AQuoted then
    Result := '''' + Result + ''''
end;

class function TSQLServerUniProviderInterpreter.DataBaseDateTimeFormat(ADateTime: Variant; AQuoted: Boolean): String;
var
  LDateTime: TDateTime;
begin
  case VarType(ADateTime) of
    varNull, varEmpty:
      begin
        Result := SConst_NULL;
        Exit;
      end;
    varDate:
      LDateTime := VarToDateTime(ADateTime)
    else
      LDateTime := ADateTime;
  end;

  if TimeOf(LDateTime) > 0.0 then
    Result := TrimRight(TrimRight(' ' + FormatDateTime(SConst_SQLTimeFormat, LDateTime), '0'), '.')
  else
    Result := '';

  Result := FormatDateTime(SConst_SQLDateFormat, LDateTime) + Result;

  if AQuoted then
    Result := '''' + Result + ''''
end;

class function TSQLServerUniProviderInterpreter.DataBaseTimeFormat(ADateTime: Variant; AQuoted: Boolean): String;
begin
  if VarIsNull(ADateTime) or (ADateTime <= 0) then
    Result := SConst_NULL
  else begin
    Result := TrimRight(TrimRight(FormatDateTime(SConst_SQLTimeFormat, ADateTime), '0'), '.');

    if AQuoted then
      Result := '''' + Result + ''''
  end
end;

class function TSQLServerUniProviderInterpreter.GenerateClearTableScript(const ATableName: String): String;
begin
  Result := 'TRUNCATE TABLE ' + QuoteString('#' + ATableName, '[')
end;

class function TSQLServerUniProviderInterpreter.GenerateCreateTableScript(ADataSet: TDataSet; const ATableName: String; AStatusFieldName: String): String;
var
  I: Integer;
  LField: TField;
begin
  Result := GenerateDropTableScript(ATableName) + SConst_CR + 'CREATE TABLE ' + QuoteString('#' + ATableName, '[') + '(';

  for I := 0 to ADataSet.FieldCount - 1 do begin
    if I > 0 then Result := Result + ',';
    LField := ADataSet.Fields[I];
    Result := Result + QuoteString(LField.FieldName, '[') + ' ' + FieldTypeToDataBaseType(LField);
    if LField.Required then Result := Result + ' NOT NULL';
  end;
  if not AStatusFieldName.IsEmpty then
    Result := Result + ',' + QuoteString(AStatusFieldName, '[') + ' Char(1)';
  Result := Result + ')';
end;

class function TSQLServerUniProviderInterpreter.GenerateDropTableScript(const ATableName: String): String;
begin
  Result := 'IF OBJECT_ID(' + QuotedStr('tempdb..' + QuoteString('#' + ATableName, '[')) + ') IS NOT NULL DROP TABLE ' + QuoteString('#' + ATableName, '[')
end;

class function TSQLServerUniProviderInterpreter.GenerateInsertTableScript(ADataSet: TDataSet; const ATableName: String; AStatusFieldName: String): String;
var
  I, F: Integer;
begin
  if ADataSet.Eof then Exit('');
  Result := 'INSERT INTO ' + QuoteString('#' + ATableName, '[') + ' (';
  for F := 0 to ADataSet.FieldCount - 1 do begin
    if F > 0 then Result := Result + ',';
    Result := Result + QuoteString(ADataSet.Fields[F].FieldName, '[');
  end;
  if not AStatusFieldName.IsEmpty then
    Result := Result + ',' + QuoteString(AStatusFieldName, '[');
  Result := Result + ') VALUES';

  I := 0;
  while (I < 1000) and (not ADataSet.Eof) do begin
    Inc(I);
    if I > 1 then Result := Result + ',';
    Result := Result + SConst_CR + '(';
    for F := 0 to ADataSet.FieldCount - 1 do begin
      if F > 0 then Result := Result + ',';
      with ADataSet.Fields[F] do
        if IsNull then
          Result := Result + SConst_NULL
        else
          Result := Result + VariantToDataBaseString(AsVariant);
    end;

    if not AStatusFieldName.IsEmpty then begin
      Result := Result + ',';
      case ADataSet.UpdateStatus of
        usInserted: Result := Result + '''I''';
        usModified: Result := Result + '''U''';
        usDeleted : Result := Result + '''D''';
        else Result := Result + SConst_NULL;
      end;
      end;
    Result := Result + ')';

    ADataSet.Next;
  end;
end;

class function TSQLServerUniProviderInterpreter.GenerateReceiveScript(const Queue: string; const TimeoutMSec: Integer): String;
begin
  Result := 'RECEIVE'
              + ' CASE WHEN [message_type_id] = 2 THEN ''E'' END AS [message_type]'
              + ', CASE WHEN [validation] = ''X'' THEN CAST([message_body] AS XML) END AS [message_body_xml]'
              + ', CASE WHEN [validation] = ''N'' THEN [message_body] END AS [message_body_binary]'
              + ', status, queuing_order, message_sequence_number, service_contract_name'
              + ', service_contract_id, message_type_name, validation'
              + ' FROM ' + MSSQLInfo.NormalizeName(Queue);

  if TimeoutMSec > 0 then
    Result := 'WAITFOR (' + Result + '), TIMEOUT ' + IntToStr(TimeoutMSec);
end;

class function TSQLServerUniProviderInterpreter.GenerateGetTranCountScript: String;
begin
  Result := 'SELECT @@TRANCOUNT'
end;

class function TSQLServerUniProviderInterpreter.GenerateRollBackTranScript: String;
begin
  Result := 'ROLLBACK'
end;

class function TSQLServerUniProviderInterpreter.GenerateRollBackTranIfPresentScript: String;
begin
  Result := 'IF @@TRANCOUNT > 0 ROLLBACK'
end;

class function TSQLServerUniProviderInterpreter.VariantToDataBaseString(const AValue: Variant; const ABinaryStringFormat: Boolean): String;
var
  LWideChar: Boolean;
  LDate    : Extended;
  LVarType : TVarType;
begin
  LVarType := VarType(AValue);
  LWideChar := False;
  case LVarType of
    varNull, varEmpty     : Result := SConst_NULL;
    varBoolean            : Result := String(AnsiChar(Byte('0') + Integer(Boolean(AValue))));

    varByte, varSmallint, varShortInt, varWord, varInteger, varLongWord, varInt64
                         : Result := IntToStr(AValue);

    varSingle, varDouble, varCurrency
                         : Result := FloatToStr(AValue);

    varDate              :
      begin
        LDate := Int(AValue);
        if LDate = 0.0 then
          Result := DataBaseTimeFormat(AValue, not ABinaryStringFormat)
        else if LDate <> AValue then
          Result := DataBaseDateTimeFormat(AValue, not ABinaryStringFormat)
        else
          Result := DataBaseDateFormat(AValue, not ABinaryStringFormat);
      end;

    varString, varOleStr, varUString :
      begin
        if ABinaryStringFormat then
          Result := AValue
        else
          Result := QuotedStr(AValue);
        LWideChar := (LVarType = varUString);
      end;

    else
      if LVarType = VarFMTBcd then
        Result := VarToStr(AValue)
      else
        Raise Exception.Create('Cannot convert variant to DataBase String');
  end;

  if ABinaryStringFormat then begin
    if Length(Result) = 0 then
      Result := '0x'
    else if LWideChar then
      Result := '0x' + WideStringToHex(Result)
    else
      Result := '0x' + AnsiStringToHex(AnsiString(Result));
  end;
end;

class function TSQLServerUniProviderInterpreter.VarTypeToDataBaseType(AVarType: TVarType): String;
begin
  case AVarType of
    varByte
        : Result := 'TinyInt';
    varSmallint
    , varShortInt
        : Result := 'SmallInt';

    varInteger
    , varWord
        : Result := 'Int';

      varInt64
    , varLongWord
        : Result := 'BigInt';

      varSingle
    , varDouble
    , varCurrency
    , varDecimal
        : Result := 'numeric(32,12)';

      varBoolean
        : Result := 'Bit';

      varDate
        : Result := 'DateTime';

      varString
    , varOleStr
        : Result := 'VarChar(Max)';
      varUString
        : Result := 'NVarChar(Max)';
    else
      Result := '';
  end;
end;

class function TSQLServerUniProviderInterpreter.FieldTypeToDataBaseType(AField: TField): String;
begin
  case AField.DataType of
    ftBoolean
      : Result := 'Bit';

    ftByte
      : Result := 'TinyInt';
    ftSmallint
    , ftShortint
      : Result := 'SmallInt';
    ftInteger
    , ftWord
      : Result := 'Int';
    ftLargeint
    , ftLongWord
      : Result := 'BigInt';

    ftSingle
    , ftExtended
    , ftFloat
      : Result := 'Float';
    ftCurrency
      : Result := 'Money';

    ftBCD
      : Result := 'numeric(' + IntToStr(TBCDField(AField).Precision) + ',' + IntToStr(TBCDField(AField).Size) + ')';
    ftFMTBcd
      : Result := 'numeric(' + IntToStr(TFMTBCDField(AField).Precision) + ',' + IntToStr(TFMTBCDField(AField).Size) + ')';

    ftDate
      : Result := 'Date';
    ftTime
      : Result := 'Time';
    ftDateTime
      : Result := 'DateTime';

    ftFixedChar
      : Result := 'Char(' + IntToStr(AField.Size) + ')';
    ftString
      : Result := 'VarChar(' + IntToStr(AField.Size) + ')';
    ftMemo
      : Result := 'VarChar(Max)';
    ftFixedWideChar
      : Result := 'NChar(' + IntToStr(AField.Size) + ')';
    ftWideString
      : Result := 'NVarChar(' + IntToStr(AField.Size) + ')';
    ftWideMemo
      : Result := 'NVarChar(Max)';

    ftGuid
      : Result := 'UniqueIdentifier';
    ftTimeStamp
      : Result := 'TimeStamp';
    ftVariant
      : Result := 'SQL_Variant';
    ftBytes
      : Result := 'Binary(' + IntToStr(AField.Size) + ')';
    ftVarBytes
      : Result := 'VarBinary(' + IntToStr(AField.Size) + ')';
    ftBlob
      : Result := 'VarBinary(Max)';
    else
      Raise Exception.Create('Cannot convert TFieldType to SQL type');
  end;
end;

initialization
  RegisterClasses([TSQLServerUniProvider]);
  TInternalProviderInterpreter.RegisterInterpreter(TSQLServerUniProvider.GetProviderName, TSQLServerUniProviderInterpreter);

finalization
  UnRegisterClasses([TSQLServerUniProvider]);

end.
