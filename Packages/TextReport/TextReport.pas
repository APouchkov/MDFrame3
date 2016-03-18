unit TextReport;

interface

uses
  SysUtils, Classes, System.Contnrs, Variants;

type
  TTRProp=class
  public
    Name: string;
    Value: variant;
    constructor Create(const aName: string; const aValue: variant);
  end;

  TTRPropList = class(TObjectList)
    private
      function GetItems(index: integer): Variant;
      function GetValues(index: string): Variant;
      procedure SetValues(index: string; const Value: Variant);
      function GetNames(index: integer): String;
    public
      property Names[index: integer]: String read GetNames;
      property Items[index: integer]: Variant read GetItems;
      property Values[index: string]: Variant read GetValues write SetValues; default;
      function Add(aRecord: TTRProp):integer;
      procedure Insert(Index: integer; aRecord: TTRProp);
    private
      Sorted: boolean;
    public
      function Find(const aItem:string ; var Index: integer): boolean;
      procedure Sort;
    end;

  TTextReport = class(TComponent)
  private
    { Private declarations }
    FDataModule: TComponent;

    FTemplate: TFileName;
    FFileName: TFileName;
    FValues: TTRPropList;

    procedure SetDataModule(const Value: TComponent);
//    procedure SetBuildActions(const Value: TOPWordReportBuildActions);
  protected
    { Protected declarations }
  public
    { Public declarations }
    property Values: TTRPropList read FValues write FValues;

    constructor Create(AOwner:TComponent);override;
    destructor Destroy;override;

    procedure Run;
  published
    { Published declarations }
    property DataModule: TComponent read FDataModule write SetDataModule;
    property Template: TFileName read FTemplate write FTemplate;
    property FileName: TFileName read FFileName write FFileName;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Data Controls', [TTextReport]);
end;

// ---- prop class

constructor TTRProp.Create(const aName: string; const aValue: variant);
begin
  inherited Create;
  Name:=aName;
  Value:=aValue;
end;

function TTRPropList.GetValues(index: string): variant;
var
  idx: integer;
begin
  if Find(index, idx) then Result:= (inherited items[idx] as TTRProp).Value
  else Result := unassigned;
end;

procedure TTRPropList.SetValues(index: string; const Value: variant);
var
  idx: integer;
begin
  if Find(index, idx) then
    if VarIsEmpty(Value) then delete(idx) else (inherited items[idx] as TTRProp).Value:=Value
  else
    if not VarIsEmpty(Value) then Add(TTRProp.Create(UpperCase(index), Value));
end;

function TTRPropList.GetItems(index: integer): Variant;
begin
  if (index>=0) and (index<Count) then Result:= (inherited items[index] as TTRProp).Value
  else Result := '';
end;

function TTRPropList.GetNames(index: integer): String;
begin
  if (index>=0) and (index<Count) then Result:= (inherited items[index] as TTRProp).Name
  else Result := '';
end;

function TTRPropList.Add(aRecord: TTRProp):integer;
begin
  Result:=inherited Add(aRecord);
  Sorted:=false;  //When we add the list gets unsorted
end;

procedure TTRPropList.Insert(Index: Integer; ARecord:TTRProp);
begin
  inherited Insert(Index, ARecord);
  // We assume that when we insert, we respect the order, so we dont set Sorted=true
end;

function CompareTPWRProps(Item1, Item2: Pointer): Integer;
begin
  if TTRProp(Item1).Name < TTRProp(Item2).Name then Result:=-1 else if TTRProp(Item1).Name > TTRProp(Item2).Name then Result:=1 else Result:=0;
end;

procedure TTRPropList.Sort;
begin
  inherited Sort(CompareTPWRProps);
  Sorted:=true;
end;

function TTRPropList.Find(const aItem:string ; var Index: integer): boolean;
Var
 L, H, I, C: Integer; a: String;
begin
  if not Sorted then Sort;
  a := UpperCase(aItem);
  Result := False;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    if (inherited items[i] as TTRProp).Name < a then C:=-1 else if (inherited items[i] as TTRProp).Name > a then C:=1 else C:=0;
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      If C = 0 Then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

// ----- end of prop class

constructor TTextReport.Create(AOwner: TComponent);
begin
  inherited;
  FValues :=  TTRPropList.Create;
  FDataModule:=AOwner;
end;

destructor TTextReport.Destroy;
begin
  inherited;
end;

procedure TTextReport.SetDataModule(const Value: TComponent);
begin
  if Value=nil then FDataModule := Owner else FDataModule:=Value;
end;



procedure TTextReport.Run;
type
  TReportTable = record
    table: TRdXP.Table;
    dataset: TDataSet;
    datarow: Integer;
    cols: array of string;
  end;
const
  VarCharacter = '#';
  VarStatic = '.';
  VarDataSetField = '#';
  VarDataSetDelimeter: String = '##';
  VarAnyChar: String = 'A-Za-z0-9_#';
  VarExtraChar = '#';
var
  OpWord: TOpWord;
  Doc : TOpWordDocument;
  i,L,di,de: Integer;
  table_id: Integer;
  table: TRdXP.Table;

  dataset_name: String;
  dataset: TDataSet;
  datarow: integer;

  tablecount: Integer;
  tables: array of TReportTable;

  cell, value: String;
  oleVar: OleVariant;

  DtSets: array of TDataSet;
  function GetDataSetByName(DsName: String): TDataSet;
  var
    DtSetsCount: Integer;
    obj: TComponent;
    j: Integer;
  begin
    j := 0; DtSetsCount := Length(DtSets);
    while (j<DtSetsCount) and (CompareText(DtSets[j].Name,DsName)<>0) do Inc(j);
    if j<DtSetsCount then
      Result := DtSets[j]
    else begin
      obj := FDataModule.FindComponent(DsName);
      if (obj is TDataSet) then begin
        SetLength(DtSets,Succ(DtSetsCount));
        DtSets[DtSetsCount] := (obj as TDataSet);
        Result := (obj as TDataSet);
      end else
        Result := nil;
    end;
  end;
  function GetDataSetFileByName(DsName,FdName: String): String;
  var
    Ds: TDataSet;
    Fd: TField;
  begin
    Result := VarCharacter + VarDataSetField + DsName + VarDataSetDelimeter + FdName;
    Ds := GetDataSetByName(DsName);
    if Ds <> nil then begin
      Fd := Ds.FindField(FdName);
      if Fd <> nil then Result := Fd.DisplayText;
    end;
  end;
  function GetValue(VarName: String): String;
  var
    i,L: Integer;
  begin
    L := Length(VarName);
    if VarName[2] = VarStatic then
      Result := Values[Copy(VarName,3,L-2)]
    else begin
      Result := VarName;
      i := PosEx(VarDataSetDelimeter,VarName,3);
      if i>0 then
        Result := GetDataSetFileByName(Copy(VarName,3,i-3),Copy(VarName,i+2,L-i));
    end;
  end;
begin
  OpWord := TOpWord.Create(Owner);
  OpWord.Connected := True;
  try try
    Doc := OpWord.OpenDocument(FTemplate);

    tablecount := 0;
    for table_id := Doc.AsDocument.Tables.Count downto 1 do begin
      table := Doc.AsDocument.Tables.Item(table_id);
      cell := table.Range.Text;
      cell := Trim(table.Cell(1,1).Range.Text);
      if (length(cell)>=12) and (cell[1]='{') and (cell[2]='$') and (CompareText(Copy(cell,3,8),'DATASET=')=0) then begin
        i := pos('}',cell);
        if i>0 then begin
          table.Cell(1,1).Range.Text := Trim(Copy(cell,i+1,Length(cell)-i));
          cell := Copy(cell,11,i-11);
          i := Pos(',',cell);
          if i>0 then begin
            dataset_name := Copy(cell,1,i-1);
            cell := Copy(cell,i+1,Length(cell)-i);
            if CompareText(Copy(cell,1,4),'ROW=')=0 then begin
              datarow := StrToInt(Copy(cell,5,Length(cell)-4));
            end else
              datarow := 1;
          end else
            dataset_name := cell;

          dataset := GetDataSetByName(dataset_name);
          if (not Assigned(dataset)) or (datarow<1) or (datarow>table.Rows.Count) then
          // датасет или строка не найден(а)
          else if dataset.IsEmpty then
          // датасет пуст
            for i := table.Columns.Count downto 1 do table.Cell(2,i).Range.Text := '-'
          else begin
            SetLength(tables,Succ(tablecount));
            tables[tablecount].table := table;
            tables[tablecount].dataset := dataset;
            tables[tablecount].datarow := datarow;
            SetLength(tables[tablecount].cols,table.Columns.Count);
            for i := table.Columns.Count downto 1 do tables[tablecount].cols[Pred(i)] := table.cell(datarow,i).Range.Text;
            Inc(tablecount);
          end;
        end;
      end;
    end;

    if Doc.Find(VarCharacter+'['+VarStatic+VarDataSetField+']['+VarAnyChar+']{1;90}>',True,False,False,True) then repeat
      Doc.FindRange.Text := GetValue(Doc.FindRange.Text);
      Doc.FindRange.Start := Doc.FindRange.End_+1;
    until not Doc.FindNext;

    for table_id := Pred(tablecount) downto 0 do begin
      table := tables[table_id].table;
      datarow := tables[table_id].datarow;
      dataset := tables[table_id].dataset;
//      row := VarArrayCreate([1,table.Columns.Count],varVariant);
//      for i := table.Columns.Count downto 1 do row[i] := Trim(table.Cell(2,i).Range.Text);
      dataset.First;
      repeat
        if dataset.RecNo<dataset.RecordCount then begin
          oleVar := table.rows.item(datarow + dataset.RecNo - 1);
          table.Rows.Add(oleVar);
        end;
        for i := table.Columns.Count downto 1 do begin
          cell := tables[table_id].cols[Pred(i)];
          di := 0; L := Length(cell);
          repeat
            di := PosEx(VarCharacter,cell,di);
            if (di>0) and (di<L) and (cell[Succ(di)] in [VarStatic,VarDataSetField]) then begin
              de := di + 2;
              while (de<=L) and (cell[de] in ['0'..'9','A'..'Z','a'..'z','_',VarExtraChar]) do Inc(de);
              value := GetValue(Copy(cell,di,de-di));
              cell := copy(cell,1,Pred(di)) + value + copy(cell,de,L-de+1);
              L := L + Length(value) - (de - di);
              di := di + Length(value);
            end;
          until (di=0) or (di>=L);
          table.Cell(datarow+dataset.RecNo-1,i).Range.Text := cell
        end;
        dataset.Next;
      until dataset.Eof;
    end;

    OpWord.Visible := TRShow in FBuildActions;
  finally
    if not OpWord.Visible then Doc.Free;
  end except
    OpWord.Connected := False;
    OpWord.Free
  end;
end;


end.
 