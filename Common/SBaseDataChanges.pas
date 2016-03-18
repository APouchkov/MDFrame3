unit SBaseDataChanges;

interface

uses
  DB, NamedVariables, SysUtils, StrUtils, Variants, SBaseDataModule;

type
  TRecordAction = packed record
    KeyValue: Variant;
    Action: Char;
  end;

  TTableChanges = packed record
    FKeyFieldType: TFieldType;
    FFullRefresh: Boolean;
  public
    Table: String;
    Params: TNamedVariants;
    Changes: Array of TRecordAction;

    constructor Create(ATable: String; AString: String);
    procedure Add(AKeyValue: String; AAction: Char);
    function ChangeCount: Integer;

    property KeyFieldType: TFieldType read FKeyFieldType;
    property FullRefresh: Boolean read FFullRefresh;
  end;

  TTablesChanges = packed record
  private
    FExternalEvent: Boolean;
    FNotify: Largeint;
    FTables: Array of TTableChanges;

    function GetTables(Index: Integer): TTableChanges;
    function GetFullRefresh: Boolean;
    function GetNotify: Largeint;
  public
    constructor Create(AFullRefresh: Boolean); overload;
    constructor Create(const AParams: TNamedVariants); overload;

    function TableCount: Integer;
    property Tables[Index: Integer]: TTableChanges read GetTables;
    property ExternalEvent: Boolean read FExternalEvent;
    property FullRefresh: Boolean read GetFullRefresh;
    property Notify: Largeint read GetNotify;
  end;

  TDataSetAppliedChanges = packed record
  private
    FFullRefresh: Boolean;
    FRows: Array of Variant;
    DataSet: TSQLDataSetProvider;
  public
    property FullRefresh: Boolean read FFullRefresh;

    procedure Refresh;
    function ApplyChange(AKeyValue: Variant): Boolean;
  end;
  PDataSetAppliedChanges = ^TDataSetAppliedChanges;

  TDataSetsAppliedChanges = packed record
  private
    FTables: Array of TDataSetAppliedChanges;
  public
    function RegisterDataSet(ADataSet: TSQLDataSetProvider): PDataSetAppliedChanges;
  end;

  TChangeNotificationsCach = packed record
  private
    FNotifies: Array of Largeint;
    FIndex: Integer;
  public
    function Accept(ANotify: Largeint): Boolean;
  end;

implementation

{ TTableChanges }

uses SBaseConstants, SBaseStringFunctions, SBaseVariantFunctions;

function TTableChanges.ChangeCount: Integer;
begin
  Result := Length(Changes)
end;

constructor TTableChanges.Create(ATable: String; AString: String);
var
  LString: String;
  LAction: Char;
  I: Integer;
begin
  SetLength(Changes, 0);

//  if ATable.IsEmpty then Exit;
  if ATable[ATable.Length] = ')' then begin
    I := PosExRevert('(', ATable, -1);
    if I <= 0 then
      Raise Exception.Create('Invalid table changes name: ' + ATable);
    Params := TNamedVariants.Create(StringReplace(Copy(ATable, I + 1, ATable.Length - I - 1), ',', ';', [rfReplaceAll, rfIgnoreCase]), False);
    ATable := LeftStr(ATable, I - 1);
  end;
  Table := ATable;

  FFullRefresh := (AString = '*');
  if (not FFullRefresh) then begin
    I := Pos('@', AString);
    if (I > 0) then begin
      LString := LeftStr(AString, I - 1);
      if SameText(LString, SQLtypeTinyInt)
        or SameText(LString, SQLtypeSmallInt)
        or SameText(LString, SQLtypeInt)
        or SameText(LString, SQLtypeBigInt)
      then
        FKeyFieldType := ftInteger
      else
        FKeyFieldType := ftString;

      Delete(AString, 1, I);
    end else
      FKeyFieldType := ftString;

    while not AString.IsEmpty do begin
      LString := VarToStr(ExtractAndRemoveValue(AString, '/', ftString));
      if (Length(LString) <= 2) or (LString[2] <> ':') then Continue;
      LAction := LString[1];
      Delete(LString, 1, 2);
      while not LString.IsEmpty do
        Add(ExtractAndRemoveValue(LString, ','), LAction);
    end;
  end;
end;

procedure TTableChanges.Add(AKeyValue: String; AAction: Char);
var
  LIndex: Integer;
begin
  if (AKeyValue = '*') or (AAction = '*') then begin
    FFullRefresh := True;
    SetLength(Changes, 0);
  end else begin
    LIndex := Length(Changes);
    SetLength(Changes, LIndex + 1);
    with Changes[LIndex] do begin
      Action := AAction;
      if FKeyFieldType = ftInteger then
        KeyValue := StrToInt(AKeyValue)
      else
        KeyValue := AKeyValue
    end;
  end;
end;

{ TTablesChanges }

constructor TTablesChanges.Create(AFullRefresh: Boolean);
begin
  if AFullRefresh then
    FNotify := -1
  else
    FNotify := 0;
end;

constructor TTablesChanges.Create(const AParams: TNamedVariants);
var
  I, L: Integer;
begin
  if AParams.Find(SConst_Id, I) then begin
    FNotify := AParams.Items[I].Value;
    AParams.Delete(I);
  end else
    FNotify := 0;

  L := 0;
  for I := 0 to AParams.Count - 1 do begin
    with AParams.Items[I] do begin
      SetLength(FTables, L + 1);
      FTables[I] := TTableChanges.Create(Name, Value);
      Inc(L);
    end;
  end;
end;


function TTablesChanges.GetFullRefresh: Boolean;
begin
  Result := (FNotify = -1)
end;

function TTablesChanges.GetNotify: Largeint;
begin
  if FNotify >= 0 then
    Result := FNotify
  else
    Result := 0;
end;

function TTablesChanges.GetTables(Index: Integer): TTableChanges;
begin
  Result := FTables[Index];
end;

function TTablesChanges.TableCount: Integer;
begin
  Result := Length(FTables);
end;

{ TDataSetAppliedChanges }

function TDataSetAppliedChanges.ApplyChange(AKeyValue: Variant): Boolean;
var
  I: Integer;
begin
  Result := (not FullRefresh);
  if Result then
    for I := 0 to Length(FRows)-1 do
      if VarIsEqual(FRows[I], AKeyValue) then begin
        Result := False;
        Exit;
      end;
  I := Length(FRows);
  SetLength(FRows, I + 1);
  FRows[I] := AKeyValue;
end;

procedure TDataSetAppliedChanges.Refresh;
begin
  if FFullRefresh then Exit;

  if Assigned(DataSet.OnBeginUpdate) then
    DataSet.Thread.Refresh
  else
    DataSet.ReOpen;
  SetLength(FRows, 0);
  FFullRefresh := True;
end;

{ TDataSetsAppliedChanges }

function TDataSetsAppliedChanges.RegisterDataSet(ADataSet: TSQLDataSetProvider): PDataSetAppliedChanges;
var
  I: Integer;
begin
  for I := 0 to Length(FTables)-1 do begin
    Result := @FTables[I];
    if Result^.DataSet = ADataSet then
      Exit;
  end;

  I := Length(FTables);
  SetLength(FTables, I + 1);
  Result := @FTables[I];
  Result^.DataSet := ADataSet;
end;

{ TChangeNotificationsCach }

function TChangeNotificationsCach.Accept(ANotify: Largeint): Boolean;
const
  CACH_SIZE = 100;
var
  I, L: Integer;
begin
  Result := (ANotify <= 0);
  if Result then Exit;

  L := Length(FNotifies);
  for I := 0 to L - 1 do
    if FNotifies[I] = ANotify then
      Exit;

  Result := True;
  Inc(FIndex);
  if FIndex >= L then
    if L = CACH_SIZE then
      FIndex := 0
    else begin
      FIndex := L;
      SetLength(FNotifies, L + 1);
      FNotifies[FIndex] := ANotify;
    end;
end;

end.
