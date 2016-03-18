{******************************************************************************}
{                       -= Модуль печати документов =-                         }
{                                                                              }
{                     -= Шаблоны DBF (THalcyonDataSet) =-                      }
{                                                                              }
{                             v4.1 (С) 2007 - 2014                             }
{                                                                              }
{                                                                              }
{******************************************************************************}
unit SBaseReportDBF;
{$I config.inc}

interface

uses
  SBaseReportInterface, NamedVariables;

{$IFDEF PATTERN_EXTERNAL}
procedure RegisterFactoryPattern(Report: IReport; var FactoryPattern: IFactoryPattern);
{$ENDIF}

implementation

uses
  Classes, DB, SysUtils, Variants
  {$IFNDEF PATTERN_EXTERNAL}
  , SBaseReport
  {$ENDIF}
  {$IFNDEF REPORT_EXTERNAL}
  , Controls, ComObj, Forms
  {$ENDIF}
  , FileUtil
{$IFDEF DBF_WITH_HALCYON}
  , Halcn6db
{$ENDIF}
{$IFDEF DBF_WITH_UNIDAC}
  , Uni
  , DBFUniProvider
{$ENDIF}
  , SBaseConstants, SBaseStringFunctions;

type
  TPatternDBF = class(TCustomPattern)
  private
    FPatternFileName: String;
{$IFDEF DBF_WITH_HALCYON}
    FDataSet: THalcyonDataSet;
{$ENDIF}
{$IFDEF DBF_WITH_UNIDAC}
    FConnection: TUniConnection;
    FDataSet: TUniTable;
{$ENDIF}
  protected
    class function GetCode: String; override;
  public
    constructor Create(Report: IReport); override;
    destructor Destroy; override;
    function ResultFileExt: String; override;
    procedure PrepareReport(var APatternStorage: TPatternStorage; const DataSets: Array of TDataSet; const Variables: TNamedVariants); override;
    function SaveReport(const AFileName: String): String; override;
  end;

  TFactoryPatterns = class(TCustomFactoryPatterns)
  protected
    function GetPatternClass(Index: Integer): TCustomPatternClass; override;
  end;

const
  SPatternCode = 'DBF';
  SPatternResultFileExt = 'dbf';

{ TPatternDBF }
constructor TPatternDBF.Create(Report: IReport);
begin
  inherited;

{$IFDEF DBF_WITH_HALCYON}
  FDataSet := THalcyonDataSet.Create(nil);
  with FDataSet do begin
    AutoCalcFields  := False;
    AutoFlush       := False;
    Exclusive       := False;
    LargeIntegerAs  := asLargeInt;
    TranslateASCII  := True;
    UseDeleted      := True;
  end;
{$ENDIF}
{$IFDEF DBF_WITH_UNIDAC}
  FConnection := TUniConnection.Create(nil);
  FConnection.LoginPrompt := False;
  FConnection.ProviderName := TDBFUniProvider.GetProviderName;
  FDataSet := TUniTable.Create(nil);
  FDataSet.UniDirectional := True;
  FDataSet.Connection := FConnection;
{$ENDIF}
end;

destructor TPatternDBF.Destroy;
begin
  FDataSet.Free;
{$IFDEF DBF_WITH_UNIDAC}
  FConnection.Free;
{$ENDIF}
  inherited;
end;

class function TPatternDBF.GetCode: String;
begin
  Result := SPatternCode;
end;

function TPatternDBF.ResultFileExt: String;
begin
  Result := SPatternResultFileExt;
end;

procedure TPatternDBF.PrepareReport(var APatternStorage: TPatternStorage; const DataSets: array of TDataSet; const Variables: TNamedVariants);
begin
  inherited;
  FPatternFileName := APatternStorage.PatternFileName;
end;

function TPatternDBF.SaveReport(const AFileName: String): String;
var
  I, LRecNo: Integer;
begin
  LRecNo := 0;
{$IFNDEF REPORT_EXTERNAL}
  Screen.Cursor := crHourglass;
  DisableDataControls;
  try
{$ENDIF}
    FileUtil.CopyFile(FPatternFileName, AFileName, nil);
    FDataSet.Close;
{$IFDEF DBF_WITH_HALCYON}
    FDataSet.DatabaseName := ExtractFileDir(AFileName);
    FDataSet.TableName    := ExtractFileName(AFileName);
{$ENDIF}
{$IFDEF DBF_WITH_UNIDAC}
    FConnection.Close;
    FConnection.Database := ExtractFileDir(AFileName);
    FConnection.Open;
    FDataSet.TableName := ExtractFileName(AFileName);
    //FDataSet.SQL.Text := Format(SQLQuerySelectFrom, ['*', QuoteString(ExtractFileName(AFileName), '[')]);
{$ENDIF}
    FDataSet.Open;
    try
      with DataSets[0] do
      try
        LRecNo := RecNo;
        DisableControls;
        First;
        while not Eof do
        begin
          FDataSet.Append;
          for i := Pred(FDataSet.FieldCount) downto 0 do
          if Assigned(FindField(FDataSet.Fields[i].FieldName)) then
            FDataSet.Fields[i].AsVariant := FieldByName(FDataSet.Fields[i].FieldName).AsVariant;
          FDataSet.Post;
          Next;
        end;
      finally
        RecNo := LRecNo;
        EnableControls;
      end;
    finally
      FDataSet.Close;
    end;
{$IFNDEF REPORT_EXTERNAL}
  finally
    EnableDataControls;
    Screen.Cursor := crDefault;
  end;
{$ENDIF}
  Result := AFileName;
end;

{ TFactoryPatterns }
function TFactoryPatterns.GetPatternClass(Index: Integer): TCustomPatternClass;
begin
  Result :=  TPatternDBF;
end;

{$IFNDEF PATTERN_EXTERNAL}
initialization
  RegisterFactoryPatterns(TFactoryPatterns.Create as IFactoryPatterns);
{$ENDIF}

end.
