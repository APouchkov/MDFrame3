//Build 2008-06-17

unit dhUtils;

interface
uses
  DB, Classes, SysUtils, Variants, Types;

  type
    TFieldArray = array of TField;
    TParseTextCallBack =
      procedure(var Text: string; var CurPos: integer; TokenPos: Integer; const Token: string); 

  procedure ForceAssignFields(ADataSet: TDataSet; const AFieldNames: string; AFieldValues: Variant); overload;
  procedure ForceAssignFields(AFields: array of TField; AFieldValues: Variant); overload;
  function GetFieldText(AField: TField): string;
  function IsDesigning(AComponent: TComponent): Boolean;
  function FindOwnerClass(AInstance: TPersistent; AOwnerClass: TPersistentClass): TPersistent;
  procedure ParseText(const AText, ASep: string; SubProc: TParseTextCallBack);

implementation
type
  TPersistentAccess = class(TPersistent);

function GetFieldText(AField: TField): string;
begin
  case AField.DataType of
    ftString, ftFixedChar, ftWideString, ftGUID:
      Result := QuotedStr(VarToStr(AField.Value));
    ftDate, ftTime, ftDateTime, ftTimeStamp:
      Result := Format('''%s''', [VarToStr(AField.Value)]);
    else{ftSmallint, ftInteger, ftWord, ftAutoInc, ftBoolean, ftFloat, ftCurrency, ftBCD, ftLargeInt, ftFMTBcd:}
      Result := VarToStr(AField.Value);
  end;
end;

procedure ForceAssignFields(ADataSet: TDataSet; const AFieldNames: string; AFieldValues: Variant); overload;
var
  AField: TField;
  AFields: TFieldArray;
  APos: Integer;
  AName: string;
begin
  APos := 1;
  repeat
    AName := ExtractFieldName(AFieldNames, APos);
    AField := ADataSet.FindField(AName);
    if Assigned(AField) then begin
      SetLength(AFields, Succ(Length(AFields)));
      AFields[High(AFields)] := AField;
    end;
  until AName = '';

  ForceAssignFields(AFields, AFieldValues);
end;

procedure ForceAssignFields(AFields: array of TField; AFieldValues: Variant); overload;
var
  i: integer;
  AReadOnly, AIsArr: Boolean;
begin
  Assert (
   (Length(AFields) = 1) or
   (
     (VarArrayDimCount(AFieldValues) = 1) and (Length(AFields) > 1) and
     ((VarArrayHighBound(AFieldValues, 1) - VarArrayLowBound(AFieldValues, 1))
       = (High(AFields) - Low(AFields)))
   ),
  'Illegal ForceAssignFields call');

  AIsArr := VarIsArray(AFieldValues);
  for i := Low(AFields) to High(AFields) do begin
    AReadOnly := AFields[i].ReadOnly;
    try
      if not (AFields[i].DataSet.State in dsEditModes) then
        AFields[i].DataSet.Edit;
      AFields[i].ReadOnly := False;
      if AIsArr then
        AFields[i].AsVariant := AFieldValues[i]
      else
        AFields[i].AsVariant := AFieldValues;
    finally
      AFields[i].ReadOnly := AReadOnly;
    end;
  end;
end;

function IsDesigning(AComponent: TComponent): Boolean;
begin
  Result := [csDesigning, csLoading] * AComponent.ComponentState = [csDesigning]
end;

function FindOwnerClass(AInstance: TPersistent; AOwnerClass: TPersistentClass): TPersistent;
var
  AOwner: TPersistent;
begin
  if AInstance.InheritsFrom(AOwnerClass) then
    Result := AInstance
  else begin
    Result := nil;
    AOwner := TPersistentAccess(AInstance).GetOwner;
    if Assigned(AOwner) then
      Result := FindOwnerClass(AOwner, AOwnerClass);
  end;
end;

procedure ParseText(const AText, ASep: string; SubProc: TParseTextCallBack);
const
  Name = ['0'..'9', 'a'..'z', 'A'..'Z', '_'];
var
  i, Len, TokenPos: Integer;
  AToken, NewText: string;
begin
  NewText := AText;
  if Pos(ASep, NewText) = 0 then
    exit;
  i := 1;
  Len := Length(NewText);
  while i <= Len do begin
    if Copy(NewText, i, Length(ASep)) = ASep then begin
      AToken := '';
      TokenPos := i;
      Inc(i, Length(ASep));
      while CharInSet(NewText[i], Name) and (i <= Len) do begin
        AToken := AToken + NewText[i];
        Inc(i);
      end;
      asm
        lea eax, NewText
        lea edx, i
        mov ecx, TokenPos
        push [ebp].dword
        push AToken
        call SubProc
        add esp, 4
      end; 
     Len := Length(NewText);
    end;
    Inc(i)
  end;
end;

end.
