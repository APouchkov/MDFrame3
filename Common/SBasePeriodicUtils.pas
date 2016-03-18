unit SBasePeriodicUtils;

interface
uses
  SysUtils, Classes, Variants, Types, StrUtils, DB,
  NamedVariables, SBaseStringFunctions, SBaseVariantFunctions, SBaseDataModule;

const
  SConst_AGE  : String = 'AGE';

type
  TPeriodicActiveRecordSign   = (parsDeletedTrueNull, parsDeletedTrueFalse, parsActiveTrueNull, parsActiveTrueFalse, parsDeletedNull);

  TDateList = packed record
  strict private
    FSorted: Boolean;
    FDates: Array of TDate;
    function GetDates(Index: Integer): TDate;
    procedure SetDates(Index: Integer; const Value: TDate);
  public
    procedure Clear;
    procedure Insert(AIndex: Integer; ADate: TDate);
    procedure Append(ADate: TDate);
    procedure Delete(AIndex: Integer);
    procedure Sort;

    function Count: Integer;
    function IndexOf(ADate: TDate): Integer;
    function NearestDateIndex(ADate: TDate; ADecrease: Boolean = True; AEqual: Boolean = True): Integer;

    property Dates[Index: Integer]: TDate read GetDates write SetDates; default;
  end;

  TPeriodicFieldValue = packed record
    Date: TDate;
    Value: Variant;
  end;

  PPeriodicField = ^TPeriodicField;
  TPeriodicField = packed record
  strict private
    FValues: Array of TPeriodicFieldValue;
    function GetValues(ADate: TDate): Variant;
    procedure SetValues(ADate: TDate; const AValue: Variant);
  private
    function GetDates(Index: Integer): TDate; inline;
    procedure SetDates(Index: Integer; const Value: TDate);
    function GetItems(Index: Integer): TPeriodicFieldValue;
  public
    FieldName: String;

    procedure Delete(AIndex: Integer);
    procedure Insert(AIndex: Integer);
    function IsEqual(ATargetField: PPeriodicField; ANewAge: PDate = nil): Boolean;

    property Items[Index: Integer]: TPeriodicFieldValue read GetItems;
    property Values[Date: TDate]: Variant read GetValues write SetValues; default;
    property Dates[Index: Integer]: TDate read GetDates write SetDates;
    function CountOfValues: Integer;
  end;

  PPeriodicRow = ^TPeriodicRow;
  TPeriodicRow = packed record
  strict private
    FStaticFields: TNamedVariants;
    FPeriodicFields: Array of TPeriodicField;
    function GetPeriodicFieldValues(AFieldNames: String; ADate: TDate): Variant;
    procedure SetPeriodicFieldValues(AFieldNames: String; ADate: TDate; const AValues: Variant);

    function GetStaticFieldValues(AFieldName: String): Variant;
    procedure SetStaticFieldValues(AFieldName: String; const Value: Variant);
    function GetPeriodicFields(Index: Integer): TPeriodicField;
  public
    KeyValue: Variant;
    procedure Init(const AKeyValue: Variant);

    function StaticFieldCount: Integer;
    property StaticFieldValues[AFieldName: String]: Variant read GetStaticFieldValues write SetStaticFieldValues;
    function StaticFieldsDifferences(ATarget: TPeriodicRow): String;

    function FindPeriodicField(AFieldName: String; ACreateIfNotCreated: Boolean = False): PPeriodicField;
    function PeriodicFieldCount: Integer;
    property PeriodicFields[Index: Integer]: TPeriodicField read GetPeriodicFields;
    property PeriodicFieldValues[AFieldNames: String; Date: TDate]: Variant read GetPeriodicFieldValues write SetPeriodicFieldValues;
    function PeriodicAge: TDate;
    function PeriodicXML(AFieldNames: String = ''): String;

    function UpdateXML(const AOldRow: PPeriodicRow; const AKeyFieldNames: String = ''; const AIdentityFieldName: String = ''; const AExcludedFieldNames: String = ''; const AStatusAttributeName: String = ''; AUpdateStatusFilter: TUpdateStatusSet = [usInserted, usModified]; APostAllFields: Boolean = False): String;
    function DeleteXML(const AKeyFieldNames: String = ''; const AIdentityFieldName: String = ''; const AExcludedFieldNames: String = ''; const AStatusAttributeName: String = ''): String;
    function UpgradeAge(AActiveFieldName: String; AActiveRecordSign: TPeriodicActiveRecordSign): Boolean;
  end;

  TPeriodicRows = packed record
  strict private
    FRows: Array of TPeriodicRow;
    function GetRows(Index: Integer): PPeriodicRow;
  public
    procedure Clear;
    procedure Delete(Index: Integer);
    function  Count: Integer;
    function  FindRow(AKeyValue: Variant; ACreateIfNotCreated: Boolean = False): PPeriodicRow;
    property  Rows[Index: Integer]: PPeriodicRow read GetRows;
    procedure Load(AFields: TFields; ADateField: TField; const AKeyFieldName: String; const AStaticFieldNames: String; const APeriodicFieldNames: String; AOldValues: Boolean = False); overload;
    procedure DeleteEmptyRecords(AActiveFieldName: String; AActiveRecordSign: TPeriodicActiveRecordSign);
  end;

implementation

uses SBaseConstants;

{ TDateList }

procedure TDateList.Append(ADate: TDate);
var
  LIndex: Integer;
begin
  LIndex := Count;
  SetLength(FDates, LIndex + 1);
  FDates[LIndex] := ADate;
  FSorted := False;
end;

procedure TDateList.Clear;
begin
  SetLength(FDates, 0);
  FSorted := True;
end;

function TDateList.Count: Integer;
begin
  Result := Length(FDates)
end;

procedure TDateList.Delete(AIndex: Integer);
var
  LCount, I: Integer;
begin
  LCount := Count - 1;
  for I := AIndex to LCount - 1 do
    FDates[I] := FDates[I + 1];
  SetLength(FDates, LCount);
  FSorted := False;
end;

function TDateList.IndexOf(ADate: TDate): Integer;
begin
  for Result := 0 to Count - 1 do
    if FDates[Result] = ADate then
      Exit;

  Result := -1;
end;

procedure TDateList.Insert(AIndex: Integer; ADate: TDate);
var
  I, L: Integer;
begin
  L := Length(FDates);
  SetLength(FDates, L + 1);
  for I := L downto AIndex + 1 do
    FDates[I] := FDates[I - 1];

  FDates[AIndex] := ADate;
  FSorted := False;
end;

function TDateList.NearestDateIndex(ADate: TDate; ADecrease, AEqual: Boolean): Integer;
var
  I: Integer;
  LResultDate: TDate;
begin
  Result := -1;
  LResultDate := 0;
  for I := 0 to Count - 1 do
    if AEqual and (FDates[I] = ADate) then begin
      Result := I;
      Break;
    end else if ADecrease and (FDates[I] < ADate) and ((Result = -1) or (LResultDate < FDates[I])) then begin
      Result := I;
      LResultDate := FDates[I];
    end else if (not ADecrease) and (FDates[I] > ADate) and ((Result = -1) or (LResultDate > FDates[I])) then begin
      Result := I;
      LResultDate := FDates[I];
    end
end;

function TDateList.GetDates(Index: Integer): TDate;
begin
  Result := FDates[Index]
end;

procedure TDateList.SetDates(Index: Integer; const Value: TDate);
begin
  FDates[Index] := Value;
  FSorted := False;
end;

procedure TDateList.Sort;
var
  I, J: Integer;
  LCount: Integer;
  LDate: TDate;
begin
  if FSorted then Exit;

  LCount := Count;
  for I := 0 to LCount - 2 do
    for J := I + 1 to LCount - 1 do
      if FDates[I] > FDates[J] then begin
        LDate := FDates[I];
        FDates[I] := FDates[J];
        FDates[J] := LDate;
      end;

  FSorted := True;
end;

{ TPeriodicField }

function TPeriodicField.CountOfValues: Integer;
begin
  Result := Length(FValues)
end;

procedure TPeriodicField.Delete(AIndex: Integer);
var
  I, LDim, LInc: Integer;
begin
  LDim := Length(FValues) - 1;
  Assert((AIndex >= 0) and (AIndex <= LDim));

  if (AIndex < LDim) and VarIsEqual(FValues[AIndex].Value, FValues[AIndex + 1].Value) then begin
    Dec(LDim);
    LInc := 2;
  end else
    LInc := 1;

  for I := AIndex to LDim - LInc do
    FValues[I] := FValues[I + LInc];

  SetLength(FValues, LDim);
end;

procedure TPeriodicField.Insert(AIndex: Integer);
var
  I, LDim: Integer;
begin
  LDim := Length(FValues);
  Assert((AIndex >= 0) and (AIndex <= LDim));
  SetLength(FValues, LDim + 1);

  for I := LDim downto AIndex + 1 do
    FValues[I] := FValues[I - 1];
end;

function TPeriodicField.IsEqual(ATargetField: PPeriodicField; ANewAge: PDate = nil): Boolean;
var
  I, LCount: Integer;
begin
  LCount := Length(FValues);
  if ANewAge <> nil then
    if (LCount > 0) and ((ANewAge^ = 0) or (FValues[0].Date < ANewAge^)) then
      ANewAge^ := FValues[0].Date;

  if (ATargetField = nil) then
    Exit((LCount = 1) and not VarIsPresent(FValues[0].Value));

  Result := (ATargetField <> nil) and (LCount = Length(ATargetField.FValues));
  if Result then
    for I := 0 to LCount - 1 do
      if
        (
          ((ANewAge = nil) or (I > 0))
          and
          (FValues[I].Date <> ATargetField.FValues[I].Date)
        )
        or
        (not VarIsEqual(FValues[I].Value, ATargetField.FValues[I].Value))
      then begin
        Result := False;
        Exit;
      end;
end;

function TPeriodicField.GetItems(Index: Integer): TPeriodicFieldValue;
begin
  Result := FValues[Index]
end;

function TPeriodicField.GetDates(Index: Integer): TDate;
begin
  Result := FValues[Index].Date
end;

procedure TPeriodicField.SetDates(Index: Integer; const Value: TDate);
begin
  FValues[Index].Date := Value
end;

function TPeriodicField.GetValues(ADate: TDate): Variant;
var
  I: Integer;
begin
  I := Length(FValues) - 1;
  while I > 0 do
    if FValues[I].Date > ADate then Dec(I) else Break;

  if I >= 0 then
    Result := FValues[I].Value
  else
    Result := Null;
end;

procedure TPeriodicField.SetValues(ADate: TDate; const AValue: Variant);
var
  L, I: Integer;
begin
  L := Length(FValues) - 1;
  I := L;
  while I >= 0 do
    if FValues[I].Date > ADate then Dec(I) else Break;

  // 1. ≈сли текущее значение равно устанавливаемому.
  if (I >= 0) and VarIsEqual(FValues[I].Value, AValue) then
    Exit
  // ≈сли это (не "последнее значение") и (это "новое первое" или "дата значени€ меньше текущей") и ("значение тоже что и последущее")
  else if (I < L) and ((I < 0) or (FValues[I].Date < ADate)) and VarIsEqual(FValues[I + 1].Value, AValue) then
    FValues[I + 1].Date := ADate
  // ≈сли это "новое первое"
  else if (I < 0) then begin
    Insert(0);
    with FValues[0] do begin
      Date  := ADate;
      Value := AValue;
    end;
  end
  // ≈сли "дата значени€" равна "текущей дате"
  else if (FValues[I].Date = ADate) then begin
    // ≈сли "последущее значение" равно "новому значению"
    if (I < L) and VarIsEqual(AValue, FValues[I + 1].Value) then
      Delete(I)
    else
      FValues[I].Value := AValue;
  end
  // »ное: нова€ дата и новое значение
  else begin
    Inc(I);
    Insert(I);
    with FValues[I] do begin
      Date  := ADate;
      Value := AValue;
    end;
  end;
end;

{ TPeriodicRow }

function TPeriodicRow.PeriodicAge: TDate;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to PeriodicFieldCount - 1 do
    with FPeriodicFields[I] do
      if (CountOfValues > 0) and ((Result = 0) or (Result > Dates[0])) then
        Result := Dates[0];
end;

function TPeriodicRow.PeriodicFieldCount: Integer;
begin
  Result := Length(FPeriodicFields);
end;

function TPeriodicRow.PeriodicXML(AFieldNames: String): String;
var
  I, J: Integer;
  LDateList: TDateList;
  LAge, LDate: TDate;
begin
  LDateList.Clear;
  LAge := 0;

  for I := 0 to PeriodicFieldCount - 1 do
    with FPeriodicFields[I] do
      if InArray(AFieldNames, FieldName, ';') then
        for J := 0 to CountOfValues - 1 do begin
          LDate := Dates[J];
          if (LDateList.IndexOf(LDate) = -1) then
            LDateList.Append(LDate);
        end else if (CountOfValues > 0) then begin
          LDate := Dates[0];
          if (LAge = 0) or (LAge > LDate) then
            LAge := LDate;
        end;

  if LDateList.Count = 0 then
    Result := '<PERIODIC DATE="' + FormatDateTime(XMLEncodeDateFormat, LAge) + '"/>'
  else begin
    LDateList.Sort;
    if (LAge <> 0) and (LDateList[0] > LAge) then
      LDateList.Insert(0, LAge);

    Result := '';
    for I := 0 to LDateList.Count - 1 do
      Result := ConcatIfNotEmpty
                (
                  Result,
                  SConst_CR,
                  '<PERIODIC DATE="' + FormatDateTime(XMLEncodeDateFormat, VarToDateTime(LDateList[I])) + '" '
                    + SBaseVariantFunctions.XMLEncode(AFieldNames, PeriodicFieldValues[AFieldNames, LDateList[I]])
                    + '/>'
                );
  end;
end;

function TPeriodicRow.UpdateXML
(
  const AOldRow             : PPeriodicRow;
  const AKeyFieldNames      : String;
  const AIdentityFieldName  : String;
  const AExcludedFieldNames : String;
  const AStatusAttributeName: String;
        AUpdateStatusFilter : TUpdateStatusSet;
        APostAllFields      : Boolean
): String;
var
  I: Integer;
  LDate                 : TDate;
  LAllPeriodicFields    : String;
  LUpdatedPeriodicFields: String;
  LUpdatedOtherFields   : String;
  LPeriodicField        : PPeriodicField;
  LKeyFieldNames        : String;
begin
  LKeyFieldNames         := '';
  LUpdatedPeriodicFields := '';

  LDate          := 0;
  LPeriodicField := nil;

  for I := 0 to PeriodicFieldCount - 1 do
    with FPeriodicFields[I] do begin
      LAllPeriodicFields := ConcatIfNotEmpty(LAllPeriodicFields, ';', FieldName);
      if AOldRow <> nil then
        LPeriodicField := AOldRow.FindPeriodicField(FieldName);
      if not IsEqual(LPeriodicField, @LDate) then
        LUpdatedPeriodicFields := ConcatIfNotEmpty(LUpdatedPeriodicFields, ',', FieldName);
    end;

  Result := '<RECORD';

  if (not AKeyFieldNames.IsEmpty) then
    Result := Result + ' ' + SBaseVariantFunctions.XMLEncode(ArraysAntiJoin(AKeyFieldNames, AExcludedFieldNames, ';'), VarArrayExclude(AKeyFieldNames, KeyValue, AExcludedFieldNames), '', @LKeyFieldNames);

  if APostAllFields then begin
    if FStaticFields.Count <> 0 then
      Result := Result + ' ' + FStaticFields.AsXML(xmlmAttributes)
  end else if (not AIdentityFieldName.IsEmpty) and FStaticFields.Find(AIdentityFieldName, I) then
    Result := Result + ' ' + AIdentityFieldName + '="' + SBaseVariantFunctions.XMLEncode(FStaticFields.Items[I].Value) + '"';

  if (FStaticFields.Count <> 0) or ((AOldRow <> nil) and (AOldRow.FStaticFields.Count <> 0)) then begin
    if AOldRow <> nil then
      LUpdatedOtherFields := FStaticFields.Differences(AOldRow.FStaticFields)
    else
      LUpdatedOtherFields := ArraysAntiJoin(FStaticFields.NamesString, AIdentityFieldName, ';');
    Assert(AIdentityFieldName.IsEmpty or not InArray(LUpdatedOtherFields, AIdentityFieldName, ';'));

    if (not LUpdatedOtherFields.IsEmpty) then begin
      if (not APostAllFields) then
        Result := Result + ' ' + SBaseVariantFunctions.XMLEncode(LUpdatedOtherFields, FStaticFields[LUpdatedOtherFields]);

      LUpdatedOtherFields := StringReplace(LUpdatedOtherFields, ';', ',', [rfReplaceAll, rfIgnoreCase]);
    end;
  end else
    LUpdatedOtherFields := '';

  if (AOldRow = nil) or (LDate <> AOldRow.PeriodicAge) then begin
    LUpdatedOtherFields := ConcatIfNotEmpty(LUpdatedOtherFields, ',', SConst_AGE);
    Result := Result
                + ' ' + SConst_AGE + '="' + FormatDateTime(SBaseVariantFunctions.XMLEncodeDateFormat, LDate) + '"';
  end;

  if (AStatusAttributeName <> '') and ((AOldRow = nil) or (LUpdatedPeriodicFields <> '') or (LUpdatedOtherFields <> '')) then
    Result := Result
                + ' ' + AStatusAttributeName + '="' + IfThen(AOldRow = nil, 'I', 'U') + '"';

  if (not APostAllFields) and ((AOldRow = nil) or (LUpdatedPeriodicFields <> '') or (LUpdatedOtherFields <> '')) then
    Result := Result
                + ' FIELDS="'
                + ConcatIfNotEmpty
                  (
                    ConcatIfNotEmpty
                    (
                      LUpdatedOtherFields,
                      ',',
                      LUpdatedPeriodicFields
                    ),
                    ',',
                    IfThen
                    (
                      (AOldRow = nil),
                      StringReplace(LKeyFieldNames, ';', ',', [rfReplaceAll, rfIgnoreCase]),
                      ''
                    )
                  )
                + '"';

  if APostAllFields
      and
      (
        (usUnmodified in AUpdateStatusFilter)
        or (LUpdatedPeriodicFields <> '')
        or (LUpdatedOtherFields <> '') // and (LUpdatedOtherFields <> SConst_AGE))
      )
  then
    Result := Result
                + '>' + SConst_CR
                + PeriodicXML(LAllPeriodicFields)
                + SConst_CR + '</RECORD>'
  else if (AOldRow = nil) or (LUpdatedPeriodicFields <> '') then
    Result := Result
                + '>' + SConst_CR
                + PeriodicXML(StringReplace(LUpdatedPeriodicFields, ',', ';', [rfReplaceAll, rfIgnoreCase]))
                + SConst_CR + '</RECORD>'
  else if (LUpdatedOtherFields <> '') then
    Result := Result
                + '/>'
  else
    Result := '';
end;

function TPeriodicRow.UpgradeAge(AActiveFieldName: String; AActiveRecordSign: TPeriodicActiveRecordSign): Boolean;
var
  LNewAge: TDate;

  function CheckValue(APeriodicFieldValue: TPeriodicFieldValue): Boolean;
  begin
    // ѕровер€ет значение на VarIsPresent
    case AActiveRecordSign of
      parsDeletedTrueNull, parsDeletedTrueFalse:
        Result := not (APeriodicFieldValue.Value = True);
      parsActiveTrueNull, parsActiveTrueFalse:
        Result := (APeriodicFieldValue.Value = True);
      else
      { parsDeletedNull: }
        Result := VarIsPresent(APeriodicFieldValue.Value);
    end;

    if Result then begin
      // ≈сли возраст еще не установлен, или возраст ранее обнаруженного,
      // то устанавливаем новый возраст
      if ((LNewAge = 0) or (APeriodicFieldValue.Date < LNewAge)) then
        LNewAge := APeriodicFieldValue.Date;
    end;
  end;

var
  I: Integer;
begin
  // True = ѕериодика пуста
  Result := (PeriodicFieldCount = 0);
  if Result then Exit;

  LNewAge := 0; //FPeriodicFields[0].FValues[0].Date;
  // ѕробега€ по всем пол€м по всем первым или вторым значени€м (если первое Empty)
  for I := 0 to PeriodicFieldCount - 1 do
    with FPeriodicFields[I] do
      if SameText(FieldName, AActiveFieldName) and (not CheckValue(Items[0])) and (CountOfValues > 1) then
        CheckValue(Items[1]);

  // True = ѕериодика пуста
  Result := (LNewAge = 0);
  if Result then Exit;

  // ќп€ть бежим по всем пол€м
  for I := 0 to PeriodicFieldCount - 1 do
    with FPeriodicFields[I] do
      // ≈сли первое поле пусто и возраст его ранее общего возраста
      // то это либо стартова€ пустышка, либо полна€.
      // —читаетс€ что изначально все записи корректны.
      if (not VarIsPresent(Items[0].Value)) and (Dates[0] < LNewAge) then
        // ≈сли есть изменени€ в периодике и возраст второй записи = ќбщему возрасту
        // то удал€ем первую пустую запись
        if (CountOfValues > 1) and (Dates[1] = LNewAge) then
          Delete(0)
        else
        // »наче просто выравниваем возраст первой пустой записи (???)
          Dates[0] := LNewAge;
end;

function TPeriodicRow.DeleteXML(const AKeyFieldNames: String; const AIdentityFieldName: String; const AExcludedFieldNames: String; const AStatusAttributeName: String): String;
begin
  if AKeyFieldNames = '' then
    Result := ''
  else begin
    if AIdentityFieldName.IsEmpty then
      Result := ''
    else
      Result := SBaseVariantFunctions.XMLEncode(AIdentityFieldName, FStaticFields[AIdentityFieldName]);

    Result := ConcatIfNotEmpty('<RECORD', ' ', Result)
              + ' ' + SBaseVariantFunctions.XMLEncode(ArraysAntiJoin(AKeyFieldNames, AExcludedFieldNames, ';'), VarArrayExclude(AKeyFieldNames, KeyValue, AExcludedFieldNames))
              + IfThen(AStatusAttributeName <> '', ' ' + AStatusAttributeName + '="D"')
              + '/>'
 end;
end;

function TPeriodicRow.FindPeriodicField(AFieldName: String; ACreateIfNotCreated: Boolean): PPeriodicField;
var
  I: Integer;
begin
  Result := nil;
  if @Self = nil then Exit;

  for I := 0 to Length(FPeriodicFields) - 1 do
    if SameText(FPeriodicFields[I].FieldName, AFieldName) then begin
      Result := @FPeriodicFields[I];
      Exit;
    end;

  if ACreateIfNotCreated then begin
    I := Length(FPeriodicFields);
    SetLength(FPeriodicFields, I + 1);
    Result := @FPeriodicFields[I];
    Result^.FieldName := AFieldName;
  end;
end;

function TPeriodicRow.GetPeriodicFields(Index: Integer): TPeriodicField;
begin
  Result := FPeriodicFields[Index];
end;

function TPeriodicRow.GetPeriodicFieldValues(AFieldNames: String; ADate: TDate): Variant;
var
  LItem                 : TStringItem;
  LPeriodicFieldHistory : PPeriodicField;
  LValue                : Variant;
begin
  for LItem in SBaseStringFunctions.EnumStringItems(AFieldNames) do begin
    LPeriodicFieldHistory := FindPeriodicField(LItem.Value);
    if LPeriodicFieldHistory <> nil then
      LValue := LPeriodicFieldHistory^.Values[ADate]
    else
      LValue := Null;

    if (LItem.Index = 0) and LItem.Eof then
      Result  := LValue
    else begin
      if LItem.Index = 0 then
        Result := VarArrayOf([LValue])
      else begin
        VarArrayRedim(Result, LItem.Index);
        Result[LItem.Index] := LValue;
      end;
    end;
  end;
end;

procedure TPeriodicRow.SetPeriodicFieldValues(AFieldNames: String; ADate: TDate; const AValues: Variant);
var
  LItem                 : TStringItem;
  LPeriodicFieldHistory : PPeriodicField;
  LValue                : Variant;
begin
  for LItem in SBaseStringFunctions.EnumStringItems(AFieldNames) do begin
    LPeriodicFieldHistory := FindPeriodicField(LItem.Value, True);
    if (LItem.Index = 0) and LItem.Eof then
      LValue := AValues
    else
      LValue := AValues[LItem.Index];

    LPeriodicFieldHistory^.Values[ADate] := LValue
  end;
end;

function TPeriodicRow.GetStaticFieldValues(AFieldName: String): Variant;
begin
  Result := FStaticFields[AFieldName];
end;

procedure TPeriodicRow.Init(const AKeyValue: Variant);
begin
  FStaticFields.UnknownAsNull := True;
  KeyValue := AKeyValue;
end;

procedure TPeriodicRow.SetStaticFieldValues(AFieldName: String; const Value: Variant);
begin
  FStaticFields[AFieldName] := Value;
end;

function TPeriodicRow.StaticFieldCount: Integer;
begin
  Result := FStaticFields.Count;
end;

function TPeriodicRow.StaticFieldsDifferences(ATarget: TPeriodicRow): String;
begin
  Result := FStaticFields.Differences(ATarget.FStaticFields)
end;

{ TPeriodicRows }

procedure TPeriodicRows.Clear;
begin
  SetLength(FRows, 0);
end;

function TPeriodicRows.Count: Integer;
begin
  Result := Length(FRows);
end;

procedure TPeriodicRows.Delete(Index: Integer);
var
  L, I: Integer;
begin
  L := Count - 1;
  for I := Index to L - 1 do
    FRows[I] := FRows[I + 1];
  SetLength(FRows, L);
end;

procedure TPeriodicRows.DeleteEmptyRecords(AActiveFieldName: String; AActiveRecordSign: TPeriodicActiveRecordSign);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    with FRows[I] do
      // ≈сли периодика полностью пуста
      if UpgradeAge(AActiveFieldName, AActiveRecordSign) then
        // ≈сли разрешено удал€ть по пустой периодики или не разрешено и статика тоже пуста
        Self.Delete(I);
end;

function TPeriodicRows.FindRow(AKeyValue: Variant; ACreateIfNotCreated: Boolean = False): PPeriodicRow;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to Length(FRows) - 1 do
    if VarIsEqual(FRows[I].KeyValue, AKeyValue) then begin
      Result := @FRows[I];
      Exit;
    end;

  if ACreateIfNotCreated then begin
    I := Length(FRows);
    SetLength(FRows, I + 1);
    Result := @FRows[I];
    Result^.Init(AKeyValue);
  end;
end;

function TPeriodicRows.GetRows(Index: Integer): PPeriodicRow;
begin
  Result := @FRows[Index];
end;

procedure TPeriodicRows.Load(AFields: TFields; ADateField: TField; const AKeyFieldName, AStaticFieldNames, APeriodicFieldNames: String; AOldValues: Boolean);
var
  LDataSet  : TDataSet;
  LKeyValue : Variant;
  LRow      : PPeriodicRow;
begin
  LDataSet := AFields.DataSet;
  if (AOldValues) and (LDataSet.UpdateStatus = usInserted) then Exit;
  if (not AOldValues) and (LDataSet.UpdateStatus = usDeleted) then Exit;

  if AKeyFieldName = '' then
    LKeyValue := unAssigned
  else if AOldValues then
    LKeyValue := LDataSet.FieldOldValues[AKeyFieldName]
  else
    LKeyValue := LDataSet.FieldValues[AKeyFieldName];

  LRow := FindRow(LKeyValue, True);
  if (AStaticFieldNames <> '') then
    if AOldValues then
      LRow.StaticFieldValues[AStaticFieldNames] := LDataSet.FieldOldValues[AStaticFieldNames]
    else
      LRow.StaticFieldValues[AStaticFieldNames] := LDataSet.FieldValues[AStaticFieldNames];

  if AOldValues then
    LRow.PeriodicFieldValues[APeriodicFieldNames, ADateField.OldValue] := LDataSet.FieldOldValues[APeriodicFieldNames]
  else
    LRow.PeriodicFieldValues[APeriodicFieldNames, ADateField.AsDateTime] := LDataSet.FieldValues[APeriodicFieldNames];
end;

end.

