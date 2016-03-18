unit WordReport;
{$I config.inc}

interface

uses
  SysUtils, Classes, Contnrs, Variants, DB, StrUtils, ComObj, NamedVariables;

const
  wdNoProtection        = $FFFFFFFF;
  wdAllowOnlyRevisions  = $00000000;
  wdAllowOnlyComments   = $00000001;
  wdAllowOnlyFormFields = $00000002;
  
type
  TWordReportBuildAction = (opwrSave,opwrShow);
  TWordReportBuildActions = set of TWordReportBuildAction;

  TWordReport = class(TComponent)
  private
    { Private declarations }
    FDataModule: TComponent;

    FTemplate: TFileName;
    FFileName: TFileName;
    FValues  : TNamedVariants;
    FBuildActions: TWordReportBuildActions;

    procedure SetDataModule(const Value: TComponent);
    procedure SetBuildActions(const Value: TWordReportBuildActions);
  protected
    { Protected declarations }
    function GetDataSet(const DataSetName: String): TComponent; virtual;
  public
    { Public declarations }
    property Values: TNamedVariants read FValues write FValues;

    constructor Create(AOwner:TComponent);override;

    procedure Run(DataSets: Array of TDataSet; AProtectionType: Longword = wdNoProtection; APassword: String = '');
  published
    { Published declarations }
    property DataModule: TComponent read FDataModule write SetDataModule;
    property Template: TFileName read FTemplate write FTemplate;
    property FileName: TFileName read FFileName write FFileName;
    property BuildActions: TWordReportBuildActions read FBuildActions write SetBuildActions;

  end;

  const
    DataSetRecNo       : String = 'RecNo';
    DataSetRecordCount : String = 'RecordCount';

    STrue : String = 'Да';
    SFalse: String = 'Нет';

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Data Controls', [TWordReport]);
end;

constructor TWordReport.Create(AOwner: TComponent);
begin
  inherited;
  FValues.UnknownAsNull := False;
  FDataModule   := AOwner;
  FBuildActions := [opwrShow];
end;

procedure TWordReport.SetDataModule(const Value: TComponent);
begin
  if Value = nil then FDataModule := Owner else FDataModule := Value;
end;

procedure TWordReport.SetBuildActions(const Value: TWordReportBuildActions);
begin
  if Value = [] then
    FBuildActions := [opwrShow]
  else
    FBuildActions := Value
end;

function TWordReport.GetDataSet(const DataSetName: String): TComponent;
begin
  Result := FDataModule.FindComponent(DataSetName);
end;

procedure TWordReport.Run(DataSets: Array of TDataSet; AProtectionType: Longword = wdNoProtection; APassword: String = ''); // Use Aliased DBs as wrDBxx
type
  TReportTable = record
    table: OLEVariant;
    dataset: TDataSet;
    datarow: Integer;
    cols: array of string;
  end;
  TReportFormat = (wrfText, wrfCheck, wrfCross, wrfRadio);
const
  wdFindContinue = 1;
  wdReplaceOne = 1;
  wdReplaceAll = 2;
  wdDoNotSaveChanges = 0;

  wdCharacter = $00000001;
  wdWord = $00000002;
  wdSentence = $00000003;
  wdParagraph = $00000004;
  wdLine = $00000005;
  wdStory = $00000006;
  wdScreen = $00000007;
  wdSection = $00000008;
  wdColumn = $00000009;
  wdRow = $0000000A;
  wdWindow = $0000000B;
  wdCell = $0000000C;
  wdCharacterFormatting = $0000000D;
  wdParagraphFormatting = $0000000E;
  wdTable = $0000000F;
  wdItem = $00000010;

const
  VarCharacter = '#';
  VarStatic = '.';
  VarDataSetField = '#';
  VarDataSetDelimeter: String = '##';
  VarAnyChar: String = 'A-Za-z0-9_#';
  VarExtraChar = '#';
  VarAliasDB: String = 'wrDB';

  VarFormatCheck : String  = 'Check';
  VarFormatCross : String  = 'Cross';
  VarFormatRadio : String  = 'Radio';
var
  WordApp: OLEVariant;
  Doc : OLEVariant;
  table_id: Integer;
  table: OLEVariant;

  i,L,di,de: Integer;

  dataset_name: String;
  dataset: TDataSet;
  datarow: integer;

  tablecount: Integer;
  tables: array of TReportTable;

  cell: String;
  oleVar: OleVariant;

  DtSets: array of TDataSet;

  AVarText: String;
  AVarInteger: Integer;
  AVarFormat: TReportFormat;
  function GetDataSetByName(const DsName: String): TDataSet;
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
      obj := nil;
      if CompareText(Copy(DsName,1,Pred(Length(Dsname))),VarAliasDB)=0 then begin
        j := Byte(DsName[Length(Dsname)]) - Byte('0');
        if j<=Length(DataSets) then obj := DataSets[Pred(j)];
      end;
      if obj=nil then obj := GetDataSet(DsName);
      if (obj is TDataSet) then begin
        SetLength(DtSets,Succ(DtSetsCount));
        DtSets[DtSetsCount] := (obj as TDataSet);
        Result := (obj as TDataSet);
      end else
        Result := nil;
    end;
  end;
  function GetDataSetFieldByName(const DsName, FdName: String): String;
  var
    Ds: TDataSet;
    Fd: TField;
  begin
    Result := VarCharacter + VarDataSetField + DsName + VarDataSetDelimeter + FdName;
    Ds := GetDataSetByName(DsName);
    if Ds <> nil then begin
      Fd := Ds.FindField(FdName);
      if Fd <> nil then
        Result := Fd.DisplayText
      else if CompareText(FdName,DataSetRecNo)=0 then
        Result := IntToStr(Ds.RecNo)
      else if CompareText(FdName,DataSetRecordCount)=0 then
        Result := IntToStr(Ds.RecordCount);
    end;
  end;
  function GetValue(const AVarName: String; var AVarFormat: TReportFormat): String;
  var
    i,L: Integer;
    function GetFormat(const AVarName: String; var AVarFormat: TReportFormat): String;
    var
      i: Integer;
      FFormat: String;
    begin
      AVarFormat := wrfText;
      i := Pos(VarExtraChar, AVarName);
      if i > 0 then begin
        Result := LeftStr(AVarName, Pred(i));
        FFormat := Copy(AVarName, Succ(i), Length(AVarName) - i);
        if FFormat = VarFormatCheck then
          AVarFormat := wrfCheck
        else if FFormat = VarFormatCross then
          AVarFormat := wrfCross
        else if FFormat = VarFormatRadio then
          AVarFormat := wrfRadio;

      end else
        Result := AVarName
    end;
  begin
    L := Length(AVarName);
    if AVarName[2] = VarStatic then
      Result := Values[GetFormat(Copy(AVarName, 3, L-2), AVarFormat)]
    else begin
      Result := AVarName;
      i := PosEx(VarDataSetDelimeter, AVarName, 3);
      if i > 0 then begin
        Result := GetDataSetFieldByName(Copy(AVarName, 3, i-3), GetFormat(Copy(AVarName, i+2, L-i), AVarFormat));
      end;
    end;
  end;
begin
  WordApp := CreateOLEObject('Word.Application');
  WordApp.Visible := False;

  try
    Doc := WordApp.Documents.Add(FTemplate);
  try

    tablecount := 0;
    for table_id := Doc.Tables.Count downto 1 do begin
      table := Doc.Tables.Item(table_id);
      cell := table.Range.Text;
      cell := table.Cell(1,1).Range.Text.Trim;
      if (length(cell) >= 12) and (cell[1] = '{') and (cell[2] = '$') and (CompareText(Copy(cell, 3, 8), 'DATASET=') = 0) then begin
        i := pos('}', cell);
        if i>0 then begin
          table.Cell(1, 1).Range.Text := Copy(cell, i+1, Length(cell) - i).Trim;
          cell := Copy(cell, 11, i - 11);
          i := Pos(',',cell);
          if i>0 then begin
            dataset_name := Copy(cell,1,i-1);
            cell := Copy(cell,i+1,Length(cell)-i);
            if CompareText(Copy(cell,1,4),'ROW=')=0 then begin
              datarow := StrToInt(Copy(cell,5,Length(cell)-4));
            end else
              datarow := 1;
          end else begin
            dataset_name := cell;
            datarow := 1;
          end;

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
            SetLength(tables[tablecount].cols,Integer(table.Columns.Count));
            for i := table.Columns.Count downto 1 do tables[tablecount].cols[Pred(i)] := table.cell(datarow,i).Range.Text.Trim;
            Inc(tablecount);
          end;
        end;
      end;
    end;

    WordApp.Selection.Find.ClearFormatting;
    WordApp.Selection.Find.Text := VarCharacter + '[' + VarStatic + VarDataSetField + '][' + VarAnyChar + ']{1;90}>';
//    WordApp.Selection.Find.Replace := False;
    WordApp.Selection.Find.Replacement.Text := '';
    WordApp.Selection.Find.Forward := True;
    WordApp.Selection.Find.Wrap := wdFindContinue;
    WordApp.Selection.Find.Format := False;
    WordApp.Selection.Find.MatchCase := False;
    WordApp.Selection.Find.MatchWholeWord := False;
    WordApp.Selection.Find.MatchWildcards := True;
    WordApp.Selection.Find.MatchSoundsLike := False;
    WordApp.Selection.Find.MatchAllWordForms := False;

    WordApp.Selection.Start := 1;
    WordApp.Selection.Find.Execute;
    while WordApp.Selection.Find.Found do begin
      AVarText := GetValue(WordApp.Selection.Text, AVarFormat);
      case AVarFormat of
        wrfText:
            WordApp.Selection.Text := AVarText;
        wrfCheck, wrfCross, wrfRadio:
            begin
              if (AVarText = '') or (AVarText = '0') or (CompareText(AVarText, DefaultFalseBoolStr) = 0) then
                if AVarFormat = wrfRadio then
                  AVarInteger := -3933
                else
                  AVarInteger := -3928
              else if AVarFormat = wrfCheck then
                AVarInteger := -3842
              else if AVarFormat = wrfCross then
                AVarInteger := -3843
              else
                AVarInteger := -3932;

              WordApp.Selection.InsertSymbol(Font := 'Wingdings', CharacterNumber := AVarInteger, Unicode := True);
            end;
      end;
      WordApp.Selection.Start := WordApp.Selection.End + 1;
      WordApp.Selection.Find.Execute;
    end;

    for table_id := Pred(tablecount) downto 0 do begin
      table := tables[table_id].table;
      datarow := tables[table_id].datarow;
      dataset := tables[table_id].dataset;
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
            if (di>0) and (di<L) and
{$IFDEF DELPHIXE}
            CharInSet(cell[Succ(di)], [VarStatic, VarDataSetField])
{$ELSE}
            (Cell[Succ(di)] in [VarStatic, VarDataSetField])
{$ENDIF}
            then begin
              de := di + 2;
              while (de<=L) and
{$IFDEF DELPHIXE}
              CharInSet(cell[de], ['0'..'9','A'..'Z','a'..'z','_', VarExtraChar])
{$ELSE}
              (cell[de] in ['0'..'9','A'..'Z','a'..'z','_', VarExtraChar])
{$ENDIF}
              do Inc(de);

              AVarText := GetValue(Copy(cell, di, de-di), AVarFormat);
              case AVarFormat of
                wrfCheck, wrfCross:
                    begin
                      if (AVarText = '') or (AVarText = '0') or (CompareText(AVarText, DefaultFalseBoolStr) = 0) then
                        AVarText := '_'
                      else if AVarFormat = wrfCheck then
                        AVarText := 'v'
                      else
                        AVarText := 'x';

                      AVarText := '[' + AVarText + ']';
                    end;
              end;


              cell := copy(cell, 1, Pred(di)) + AVarText + copy(cell, de, L-de+1);
              L := L + Length(AVarText) - (de - di);
              di := di + Length(AVarText);
            end;
          until (di = 0) or (di >= L);
          table.Cell(datarow + dataset.RecNo - 1, i).Range.Text := cell
        end;
        dataset.Next;
      until dataset.Eof;
    end;

    WordApp.Selection.HomeKey(wdStory);

    if (AProtectionType <> wdNoProtection) and (APassword <> '') then
      WordApp.ActiveDocument.Protect( Password := APassword, Type := AProtectionType );

    if (opwrSave in FBuildActions) and (Length(FFileName)>0) then Doc.SaveAs(FFileName);
    WordApp.Visible := opwrShow in FBuildActions;
  finally
    if not WordApp.Visible then begin
      Doc.Close(wdDoNotSaveChanges);
      WordApp.Quit
    end;
  end except
    WordApp.Quit;
    Raise
  end;
end;


end.