{******************************************************************************}
{                       -= Модуль печати документов =-                         }
{                                                                              }
{                      -= Шаблоны Word (TReportWord) =-                        }
{                                                                              }
{                             v4.1 (С) 2007 - 2014                             }
{                                                                              }
{                                                                              }
{******************************************************************************}
unit SBaseReportWord;
{$I config.inc}

interface

uses
  SBaseReportInterface;

{$IFDEF PATTERN_EXTERNAL}
procedure RegisterFactoryPattern(Report: IReport; var FactoryPattern: IFactoryPattern);
{$ENDIF}

implementation

uses
  Classes, DB, SysUtils, Variants,
  {$IFNDEF PATTERN_EXTERNAL}
  SBaseReport,
  {$ENDIF}
  {$IFNDEF REPORT_EXTERNAL}
  Controls, ComObj, Forms,
  {$ENDIF}
  WordReport, SBaseVariantFunctions, NamedVariables, SBaseConstants;

const
  SRepSysParam_WordProtectionType = 'ProtectionType';

type
  TPatternWord = class;
  TSBaseReportWord = class(TWordReport)
    FPattern: TPatternWord;
  public
    function GetDataSet(const DataSetName: String): TComponent; override;
  end;

  TPatternWord = class(TCustomPattern)
  private
    FReport: TSBaseReportWord;
    FProtectionType: Cardinal;
    FPassword: String;
  protected
    class function GetCode: String; override;
  public
    constructor Create(Report: IReport); override;
    destructor Destroy; override;
    function ResultFileExt: String; override;
    procedure PrepareReport(var APatternStorage: TPatternStorage; const DataSets: Array of TDataSet; const Variables: TNamedVariants); override;
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

var
  FWord: Variant;

const
  SPatternCode = 'WORD';
  SPatternOLEWord = 'Word.Application';
  SPatternResultFileExt = 'doc';

{ TSBaseReportWord }
function TSBaseReportWord.GetDataSet(const DataSetName: String): TComponent;
begin
  Result := FPattern.FindDataSetByName(DataSetName);
end;

{ TPatternWord }
constructor TPatternWord.Create(Report: IReport);
begin
  inherited;
  FReport := TSBaseReportWord.Create(nil);
  FReport.FPattern := Self;
  FProtectionType := wdNoProtection;
  FPassword := '';
end;

destructor TPatternWord.Destroy;
begin
  FReport.Free;
  inherited;
end;

class function TPatternWord.GetCode: String;
begin
  Result := SPatternCode;
end;

procedure TPatternWord.PrepareReport(var APatternStorage: TPatternStorage; const DataSets: Array of TDataSet; const Variables: TNamedVariants);
var
  I: Integer;
begin
  inherited;
  for I := Pred(Variables.Count) downto 0 do
    with Variables.Items[I] do
      FReport.Values[Name] := Value;

  FReport.Template := APatternStorage.PatternFileName;
  if not VarIsNull(ReportProperty[SRepSysParam_WordProtectionType]) then
    FProtectionType := ReportProperty[SRepSysParam_WordProtectionType];
  if not VarIsNull(ReportProperty[SConst_Password]) then
    FPassword := ReportProperty[SConst_Password];
end;

function TPatternWord.ResultFileExt: String;
begin
  Result := SPatternResultFileExt;
end;

function TPatternWord.SaveReport(const FileName: String): String;
begin
  {$IFNDEF REPORT_EXTERNAL}
  Screen.Cursor := crHourglass;
  DisableDataControls;
  try
  {$ENDIF}
    FReport.FileName := FileName;
    FReport.BuildActions := [opwrSave];
    FReport.Run([], FProtectionType, FPassword);
  {$IFNDEF REPORT_EXTERNAL}
  finally
    EnableDataControls;
    Screen.Cursor := crDefault;
  end;
  {$ENDIF}
  Result := FileName;
end;

{$IFNDEF REPORT_EXTERNAL}
procedure TPatternWord.OpenReport(const AFileName, ACaption: String);
begin
  if VarIsNull(FWord) then FWord := CreateOleObject(SPatternOLEWord);
  FWord.Visible := False;
  FWord.Documents.Add(String(AFileName));
  FWord.Visible := True;
end;

procedure TPatternWord.ShowReport(Owner: TForm; Caption: String; ShowModal: Boolean);
begin
  FReport.FileName := TSBaseReport.CreateTempFileName(Caption + '.', ResultFileExt);
  FReport.BuildActions := [opwrShow];
  FReport.Run([], FProtectionType, FPassword);
end;

procedure TPatternWord.DesignReport(Owner: TForm);
begin
  OpenReport(FReport.Template, '');
end;
{$ENDIF}

{ TFactoryPatterns }
function TFactoryPatterns.GetPatternClass(Index: Integer): TCustomPatternClass;
begin
  Result := TPatternWord;
end;

initialization
  FWord := Null;
{$IFNDEF PATTERN_EXTERNAL}
  RegisterFactoryPatterns(TFactoryPatterns.Create as IFactoryPatterns);
{$ENDIF}

end.
