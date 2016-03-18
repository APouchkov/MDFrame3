{******************************************************************************}
{                       -= Модуль печати документов =-                         }
{                                                                              }
{                  -= Шаблоны Excel Large (TFlexCelReport) =-                  }
{         ( с поддержкой QuickMasterDetail (только в режиме MaxRowCount )      }
{                                                                              }
{                            v4.1 (С) 2007 - 2014                              }
{                                                                              }
{                                                                              }
{******************************************************************************}
{******************************************************************************}
{ Параметры:                                                                   }
{   MaxPageRowCount - Разбиение отчета на файлы по кол-ву строк                }
{******************************************************************************}
unit SBaseReportExcel;
{$I config.inc}                                     

interface

uses
  SBaseReportInterface, NamedVariables, FileUtil;

{$IFDEF PATTERN_EXTERNAL}
procedure RegisterFactoryPattern(Report: IReport; var FactoryPattern: IFactoryPattern);
{$ENDIF}

implementation

uses
  Classes, DB, SysUtils, StrUtils, Variants,
  {$IFNDEF PATTERN_EXTERNAL}
  SBaseReport,
  {$ENDIF}
  {$IFNDEF REPORT_EXTERNAL}
  Controls, ComObj, Forms,
  {$ENDIF}
  UXlsDB, UXlsTDataSet, UFlexCelBands, XLSXAdapter, UFlxMessages, UCustomFlexCelReport,
  SBaseVariantFunctions;

type
  TSBaseReportExcel = class(TCustomFlexCelReport)
  private
    FMainBand: TExcelBand;
    FPattern: TCustomPattern;
    FMaxRowCount: Integer;
    FFilePageBreak: Boolean;
    FPageFooter: Boolean;
    FFilePageBreakPageHeaderStart: Integer;
    FFilePageBreakPageHeaderEnd: Integer;
    FFilePageBreakDataSets: TList;
    FFilePageBandSuccess: TStrings;
    FDataSetList: TStringList;
    FBandPageHeader: TExcelBand;
    FBandPageFooter: TExcelBand;
  protected
    function RecordCount(const DataSet: TDataSet): integer;
    procedure BandMoveFirst(const Ds: TDataSetArray); override;
    procedure BandMoveNext(const Ds: TDataSetArray); override;
    procedure FillBandData(const Band: TExcelBand; var RowOffset: Integer; const MainBandRow: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run(const FileNames: TStrings; MaxRowCount: Integer = 0); overload;
    function DesignDataModule: TComponent; override;
    function GetDataSet(const DataSetName: String): IXlsDataSet;  override;
    function GetPagesDataSet: IXlsDataSet; override;

    property Pattern: TCustomPattern read FPattern write FPattern;
  end;

  TExcelDataSet = class(TXlsTDataSet)
  private
    FDataSet: IReportDataSet;
  public
    function Eof: boolean; override;
    procedure Next; override;
    procedure First; override;
  end;

  TReportThread = class(TThread)
  protected
    FileNames: TStrings;
    MaxRowCount: Integer;
    Pattern: TCustomPattern;
    Template: String;
    FileName: String;
    FProgressText: String;
    FProgressPos: Integer;
    FProgressCount: Integer;
    FCancel: Boolean;
    FDone: Boolean;
    FError: String;
    Variables: TNamedVariants;
  protected
    procedure Execute; override;
    procedure DoProgress;
    procedure Progress(Sender: TObject; const Progress: TFlxProgress; var Cancel: boolean);
  public
    constructor Create;
  end;

  TPatternExcel = class(TCustomPattern)
  private
    FExcel    : Variant;
    FFileNames: TStrings;
    FReport   : TReportThread;
  protected
    class function GetCode: String; override;
    class function ScriptSupport: Boolean; override;
  public
    constructor Create(Report: IReport); override;
    destructor Destroy; override;
    function ResultFileExt: String; override;

    function GetVariables(Name: String): Variant; override;
    procedure PrepareReport(var APatternStorage: TPatternStorage; const ADataSets: Array of TDataSet; const AVariables: TNamedVariants); override;
    function SaveReport(const FileName: String): String; override;
{$IFNDEF REPORT_EXTERNAL}
    procedure OpenReport(const AFileName, ACaption: String); override;
    procedure ShowReport(Owner: TForm; Caption: String; ShowModal: Boolean); override;
    procedure DesignReport(Owner: TForm); override;
{$ENDIF}
  end;

  TFactoryPatterns = class(TCustomFactoryPatterns)
  protected
    function GetPatternClass(Index: Integer): TCustomPatternClass; override;
  end;

type
  TInterfacedObjectAccess = class(TInterfacedObject);

const
  SPatternCode          = 'EXCEL';
  SPatternOLEExcel      = 'Excel.Application';
  SPatternResultFileExt = 'xls';

resourcestring
  SMainBand = 'MAIN';
  SPageBand = 'PAGE';
  SPageNo = 'PAGENO';
  SPageHeader = 'HEADER';
  SPageFooter = 'FOOTER';
  SError_NoDataSet = 'Не найден набор данных ''%s''';
  SParams_MaxRowCount = 'MaxPageRowCount';

{ TSBaseReportExcel }
procedure TSBaseReportExcel.BandMoveFirst(const Ds: TDataSetArray);
var
  i: integer;
begin
  for i := 0 to Pred(Length(Ds)) do
  if (Ds[i] <> nil) and (FFilePageBreakDataSets.IndexOf(Pointer(Ds[i])) < 0) then
    Ds[i].First;
end;

procedure TSBaseReportExcel.BandMoveNext(const Ds: TDataSetArray);
var
  i, AIndex: integer;
begin
  inherited;
  for i := 0 to Pred(Length(Ds)) do
  if (Ds[i] <> nil) then
  begin
    AIndex := FFilePageBreakDataSets.IndexOf(Pointer(Ds[i]));
    if AIndex >= 0 then FFilePageBreakDataSets.Delete(AIndex);
  end;
end;

constructor TSBaseReportExcel.Create(AOwner: TComponent);
begin
  inherited;
  FPattern := nil;
  FMaxRowCount := 0;
  FFilePageBreak := False;
  FFilePageBreakDataSets := TList.Create;
  FFilePageBandSuccess := TStringList.Create;
  FDataSetList := TStringList.Create;
  FDataSetList.Sorted  := True;
end;

function TSBaseReportExcel.DesignDataModule: TComponent;
begin
  Result := FPattern.FScript;
end;

destructor TSBaseReportExcel.Destroy;
var
  I: Integer;
  LObject: TObject;
  LInterface: IInterface;
begin
  FreeAndNil(FFilePageBandSuccess);
  FreeAndNil(FFilePageBreakDataSets);

  if Assigned(FDataSetList) then begin
    for I := 0 to FDataSetList.Count - 1 do begin
      LObject := FDataSetList.Objects[I];
      if (LObject <> nil) then begin
        if Supports(LObject, IInterface, LInterface) then
          LInterface._Release
        else
          LObject.Free;
        FDataSetList.Objects[I] := nil;
      end;
    end;
    FreeAndNil(FDataSetList);
  end;

  inherited;
end;

procedure TSBaseReportExcel.FillBandData(const Band: TExcelBand; var RowOffset: Integer; const MainBandRow: integer);

  function FindBand(Band: TExcelBand; const BandName: String): TExcelBand;
  var
    i: Integer;
  begin
    if SameText(Workbook.RangeName[Band.RangePos], BandName) then
    begin
      Result := Band;
      exit;
    end;
    Result := nil;    
    for i := 0 to Pred(Band.SubBands.Count) do
    begin
      Result := FindBand(Band.SubBands[i], BandName);
      if Assigned(Result) then exit;
    end;
  end;

  procedure FilePageBreak(Row: Integer);
  var
    i: Integer;
    FirstRecord: Boolean;
  begin
    for i := Pred(Length(Band.DataSet)) downto 0 do
      if Assigned(Band.DataSet[i]) then FFilePageBreakDataSets.Add(Pointer(Band.DataSet[i]));
    if not SameText(Workbook.RangeName[Band.RangePos], '__' + SPageBand + '__' + SPageHeader + '__') and
       not SameText(Workbook.RangeName[Band.RangePos], '__' + SPageBand + '__' + SPageFooter + '__') and
       (BandIsNil(Band.DataSet) or BandEof(Band.DataSet, FirstRecord)) then
        FFilePageBandSuccess.Add(Workbook.RangeName[Band.RangePos] + '=' + IntToStr(Row));
    FFilePageBreak := True;
  end;

{ TODO: ??? For full support, but very slowly
  function BandRecordCount(const Band: TExcelBand): Integer;
  var
    i, ACount: Integer;
  begin
    if DeleteEmptyRows then Result := 0 else Result := 1;
    for i := 0 to Pred(Length(Band.DataSet)) do
    begin
      LazyDbList.Check(Band.DataSet[i]);
      if Band.DataSet[i] = nil then ACount := 1 else ACount := Band.DataSet[i].RecordCount;
      if ACount > Result then Result := ACount;
    end;
  end;
}

var
  i, j, Row, AStartRow, AIndex: Integer;
  FirstRecord: Boolean;
  SubBand: TExcelBand;
  CV: TXlsCellValue;
  RTFRuns: TRTFRunList;
begin
  if FMaxRowCount = 0 then
    inherited FillBandData(Band, RowOffset, MainBandRow)
  else
  try
    if not Assigned(FMainBand) then
    begin
      FMainBand := Band;
      RowOffset := RowOffset + Band.EndRow + 1;
      // Find Page Header & Footer 
      FBandPageHeader := FindBand(Band, '__' + SPageBand + '__' + SPageHeader + '__');
      FBandPageFooter := FindBand(Band, '__' + SPageBand + '__' + SPageFooter + '__');
      { TODO: ??? For full support, but very slowly
      Workbook.InsertAndCopyRows(Band.StartRow, Band.EndRow, RowOffset, 1, False);
      }
    end;
    if (FFilePageBreakPageHeaderStart < 0) and (Band = FBandPageHeader) then
    begin
      FFilePageBreakPageHeaderStart := Band.StartRow;
      FFilePageBreakPageHeaderEnd := Band.EndRow;
    end;
    BandMoveFirst(Band.DataSet);
    FirstRecord := True;
    { TODO: ??? For full support, but very slowly
    if BandRecordCount(Band) = 0 then
      Workbook.DeleteRows(RowOffset, Band.EndRow - Band.StartRow + 1)
    else
    }
    AIndex := FFilePageBandSuccess.IndexOfName(Workbook.RangeName[Band.RangePos]);
    if AIndex >= 0 then
      if (Band = FMainBand) and (FFilePageBreakPageHeaderStart >= 0) and (FFilePageBreakPageHeaderStart < StrToInt(FFilePageBandSuccess.ValueFromIndex[AIndex])) then
        AStartRow := FFilePageBreakPageHeaderStart
      else
        AStartRow := StrToInt(FFilePageBandSuccess.ValueFromIndex[AIndex])
    else
      AStartRow := Band.StartRow;
    while BandIsNil(Band.DataSet) or (not BandEof(Band.DataSet, FirstRecord)) do
    begin
      Row := AStartRow;
      { TODO: ??? For full support, but very slowly
      if not FirstRecord then
        Workbook.InsertAndCopyRows(Band.StartRow, Band.EndRow, RowOffset, 1, False);
      }
      if not FPageFooter and (RowOffset + Band.EndRow - Band.StartRow >= FMaxRowCount) then
      begin
        FilePageBreak(Row);
        exit;
      end;
      while Row <= Band.EndRow do
      begin
        SubBand := nil;
        for i := 0 to Pred(Band.SubBands.Count) do
        if Band.SubBands[i].StartRow = Row then
        begin
          SubBand := Band.SubBands[i];
          break;
        end;
        if Assigned(SubBand) then
        begin
          FillBandData(SubBand, RowOffset, MainBandRow);
          if not FPageFooter and FFilePageBreak then
          begin
            FilePageBreak(Row);
            exit;
          end;
          Row := SubBand.EndRow;
        end
        else
        { TODO: ??? For full support, but very slowly
        if (Band <> FMainBand) and BandIsNil(Band.DataSet) or (not BandEof(Band.DataSet, FirstRecord)) then
        }
        begin
          if not FPageFooter and (RowOffset > Pred(FMaxRowCount)) then
          begin
            FilePageBreak(Row);
            exit;
          end;
          for j := 0 to Pred(Length(ValueCache[Row - 1].Cols)) do
          begin
            CV.Value := GetValue(ValueCache[Row - 1].Cols[j], Row - 1, ValueCache[Row - 1].Cols[j][0].ColOffset, RTFRuns);
            CV.XF := ValueCache[Row - 1].Cols[j][0].Value.XF;
            { TODO: ??? For full support, but very slowly
            if not (ValueCache[Row - 1].Cols[j][0].ValueType in [flv_FullDataSet, flv_Const]) then
            }
            Workbook.AssignCellDataX2(RowOffset, ValueCache[Row - 1].Cols[j][0].ColOffset, CV, RTFRuns);
          end;
          RowOffset := RowOffset + 1;
        end;
        if (AIndex >= 0) and (Band = FMainBand) and (FFilePageBreakPageHeaderStart >= 0) and (Row = FFilePageBreakPageHeaderEnd) then
          Row := StrToInt(FFilePageBandSuccess.ValueFromIndex[AIndex])
        else
          Row := Row + 1;
      end;
      if not BandIsNil(Band.DataSet) then BandMoveNext(Band.DataSet) else break;
      FirstRecord := False;
    end;
    if (AIndex < 0) and (Band <> FBandPageHeader) and (Band <> FBandPageFooter) then
      FFilePageBandSuccess.Add(Workbook.RangeName[Band.RangePos] + '=' + IntToStr(Band.EndRow));
  finally
    if (Band = FMainBand) then
    begin
      if FFilePageBreak and Assigned(FBandPageFooter) then
      try
        FPageFooter := True;
        FillBandData(FBandPageFooter, RowOffset, MainBandRow);
      finally
        FPageFooter := False;
      end;
      Workbook.DeleteRows(Band.StartRow, Band.EndRow - Band.StartRow + 1);
    end;
  end;
end;

function TSBaseReportExcel.GetDataSet(const DataSetName: String): IXlsDataSet;
var
  LIndex: Integer;
  LDataSet: TDataSet;
  LXlsTDataSet: TExcelDataSet;
begin
  LIndex := FDataSetList.IndexOf(UpperCase(DataSetName));
  if LIndex >= 0 then
    Result := TExcelDataSet(FDataSetList.Objects[LIndex])
  else
  begin
    LDataSet := FPattern.FindDataSetByName(DataSetName);
    if Assigned(LDataSet) and not Supports(LDataSet, IXlsDataSet, Result) then
    begin
      LXlsTDataSet := TExcelDataSet.Create(LDataSet, RecordCount, UseDisplayNames, UseHiddenFields);
      Supports(LDataSet, IReportDataSet, LXlsTDataSet.FDataSet);
    end
    else
      LXlsTDataSet := nil;
    Result := LXlsTDataSet;
    LIndex := FDataSetList.Add(UpperCase(DataSetName));
    if Assigned(LXlsTDataSet) then
    begin
      TInterfacedObjectAccess(LXlsTDataSet)._AddRef;
      FDataSetList.Objects[LIndex] := LXlsTDataSet;
    end;
  end;
  if not Assigned(Result) and SameText(Copy(DataSetName, 1, Length(SReportDataSetPrefix)), SReportDataSetPrefix) then
      raise EPatternError.CreateFmt(SError_NoDataSet, [DataSetName]);
end;

function TSBaseReportExcel.GetPagesDataSet: IXlsDataSet;
var
  ADataSet: TDataSet;
begin
  if Assigned(FPattern) then
    ADataSet := FPattern.PageDataSet
  else
    ADataSet := nil;
  if Assigned(ADataSet) then
    Result := TXlsTDataSet.Create(ADataSet, RecordCount, UseDisplayNames, UseHiddenFields)
  else
    Result := nil;
end;

function TSBaseReportExcel.RecordCount(const DataSet: TDataSet): integer;
begin
  if Assigned(DataSet) then
    Result := DataSet.RecordCount
  else
    Result := 1;
end;

procedure TSBaseReportExcel.Run(const FileNames: TStrings; MaxRowCount: Integer = 0);
var
  i, AIndex: Integer;
  AFileMask: String;
  AReportDataSet: IReportDataSet;
  AReportModeRows: TFlxReportMode;
begin
  FMaxRowCount := MaxRowCount;
  AReportModeRows := ReportModeRows;
  FFilePageBreakPageHeaderStart := -1;
  FFilePageBreakPageHeaderEnd := -1;
  if FMaxRowCount > 0 then
  begin
    ReportModeRows := frm_DontCopyAndDontInsert;
    for i := Pred(FPattern.DataSetCount) downto 0 do
      if Supports(FPattern.DataSets[i], IReportDataSet, AReportDataSet) then AReportDataSet.SetQuickMasterDetail;
  end;
  try
    AIndex := 1;
    AFileMask := FileName;
    repeat
      FFilePageBreak := False;
      FPageFooter := False;
      if FMaxRowCount > 0 then Values[SPageNo] := AIndex;
      FMainBand := nil;
      Run;
      if Assigned(FileNames) then FileNames.Add(FileName);
      FileName := ChangeFileExt(AFileMask, '(' + IntToStr(AIndex) + ')' + SysUtils.ExtractFileExt(AFileMask));
      AIndex := AIndex + 1;
    until not FFilePageBreak;
  finally
    FFilePageBreakDataSets.Clear;
    FMaxRowCount := 0;
    ReportModeRows := AReportModeRows;
  end;
end;

{ TExcelDataSet }
function TExcelDataSet.Eof: boolean;
begin
  if Assigned(FDataSet) then
    Result := FDataSet.EOF
  else
    Result := inherited Eof;
end;

procedure TExcelDataSet.First;
begin
  if Assigned(FDataSet) then
    FDataSet.First
  else
    inherited;
end;

procedure TExcelDataSet.Next;
begin
  if Assigned(FDataSet) then
    FDataSet.Next
  else
    inherited;
end;

{ TReportThread }
constructor TReportThread.Create;
begin
  inherited Create(True);

  Variables.UnknownAsNull := True;
  FDone := False;
  FProgressPos := 0;
  FProgressCount := 0;
  FCancel := False;
end;

procedure TReportThread.Execute;
var
  i: Integer;
  FReport: TSBaseReportExcel;
begin
  FError := '';
  try
    FReport := TSBaseReportExcel.Create(nil);
    try
      FReport.Pattern := Pattern;
      FReport.Template := Template;
      FReport.FileName := FileName;
      FReport.PagesDataField := FReport.FPattern.PageFieldName;
      FReport.OnProgress := Progress;

      for I := Pred(Variables.Count) downto 0 do
        with Variables.Items[I] do
          FReport.Values[Name] := Value;

      with FReport do begin
        Pattern := FPattern;
        UseDisplayNames := True;
        Adapter := TXLSXAdapter.Create(FReport);
        Run(FileNames, MaxRowCount);
      end;
    finally
      FreeAndNil(FReport);
      FDone := True;
    end;
  except
    on E: Exception do
    begin
      FError := E.Message;
      raise;
    end;
  end;
end;

procedure TReportThread.DoProgress;
begin
  Pattern.Progress(FProgressText, FProgressPos, FProgressCount, FCancel);
  Pattern.Progress('', 0, 0, FCancel);
end;

procedure TReportThread.Progress(Sender: TObject; const Progress: TFlxProgress; var Cancel: Boolean);
begin
{$IFNDEF REPORT_EXTERNAL}
  Synchronize(DoProgress);
{$ENDIF}
  Cancel := FCancel;
end;

{ TPatternExcel }
constructor TPatternExcel.Create(Report: IReport);
begin
  inherited;
  FExcel := Null;
  FFileNames := TStringList.Create;
  FFileNames.Delimiter := ';';
  FReport := TReportThread.Create;
end;

destructor TPatternExcel.Destroy;
begin
  FreeAndNil(FFileNames);
  FreeAndNil(FReport);
  inherited;
end;

class function TPatternExcel.GetCode: String;
begin
  Result := SPatternCode;
end;

function TPatternExcel.GetVariables(Name: String): Variant;
begin
  Result := FReport.Variables[Name]
end;

procedure TPatternExcel.PrepareReport(var APatternStorage: TPatternStorage; const ADataSets: array of TDataSet; const AVariables: TNamedVariants);
begin
  inherited;
  FReport.Variables := AVariables;
  FReport.Template  := APatternStorage.PatternFileName;
end;

function TPatternExcel.ResultFileExt: String;
begin
  Result := SPatternResultFileExt;
end;

function TPatternExcel.SaveReport(const FileName: String): String;
begin
  {$IFNDEF REPORT_EXTERNAL}
  Screen.Cursor := crHourglass;
  DisableDataControls;
  try
  {$ENDIF}
    FReport.FileName := FileName;
    with FReport do begin
      Pattern     := Self;
      FileNames   := FFileNames;
      MaxRowCount := VarToIntDef(ReportProperty[SParams_MaxRowCount], 0);

      if MultiThread then begin
        Start;
        while not FDone do
        {$IFNDEF REPORT_EXTERNAL}
        try
          Application.HandleMessage;
        except
          Application.HandleException(Application);
        end;
        {$ENDIF}
        Terminate;
        if not FError.IsEmpty then
          Raise Exception.Create(FError);
      end else
        FReport.Execute;
    end;
  {$IFNDEF REPORT_EXTERNAL}
  finally
    EnableDataControls;
    Screen.Cursor := crDefault;
  end;
  {$ENDIF}
  Result := FFileNames.DelimitedText;
end;

class function TPatternExcel.ScriptSupport: Boolean;
begin
  Result := True;
end;

{$IFNDEF REPORT_EXTERNAL}
procedure TPatternExcel.OpenReport(const AFileName, ACaption: String);
begin
  if VarIsNull(FExcel) then FExcel := CreateOleObject(SPatternOLEExcel);
  FExcel.Visible := False;
  FExcel.WorkBooks.Open(String(AFileName));
  FExcel.Visible := True;
end;

procedure TPatternExcel.ShowReport(Owner: TForm; Caption: String; ShowModal: Boolean);
var
  I: Integer;
begin
  SaveReport(TSBaseReport.CreateTempFileName(Caption, ResultFileExt));
  if Terminated then Exit;

  if FFileNames.Count > 0 then begin
    if VarIsNull(FExcel) then FExcel := CreateOleObject(SPatternOLEExcel);
    FExcel.Visible := False;
    for I := Pred(FFileNames.Count) downto 0 do
      FExcel.WorkBooks.Open(FFileNames[I]);

    I := 1;
    while True do try
    // Если в Excel-сводной таблице поставить "обновить перед открытием" - вызов Visible := True должен дать время на обновление, иначе Exception
      FExcel.Visible := True;
      Break;
    except
      if (I = 3) then
        Raise;
      Sleep(400);
      Inc(I);
    end;
  end;
end;

procedure TPatternExcel.DesignReport(Owner: TForm);
begin
  OpenReport(FReport.Template, '');
end;
{$ENDIF}

{ TFactoryPatterns }
function TFactoryPatterns.GetPatternClass(Index: Integer): TCustomPatternClass;
begin
  Result := TPatternExcel;
end;

initialization
{$IFNDEF PATTERN_EXTERNAL}
  RegisterFactoryPatterns(TFactoryPatterns.Create as IFactoryPatterns);
{$ENDIF}

end.
