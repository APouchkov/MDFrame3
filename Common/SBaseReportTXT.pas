{******************************************************************************}
{                       -= Модуль печати документов =-                         }
{                                                                              }
{                             -= Шаблоны TXT =-                                }
{                                                                              }
{                            v4.1.1 (С) 2007 - 2014                            }
{                                                                              }
{                                                                              }
{******************************************************************************}
{******************************************************************************}
{  Датасет: <##Имя датасета##>...</##Имя датасета##Строка если датасет пуст>   }
{  Поле: <##Имя датасета##Имя поля##Формат>                                    }
{  Переменная: <#.Имя переменной##Формат>                                      }
{  Выражение: <#[Выражение]##Формат>                                           }
{******************************************************************************}
{ * Формат (необязательный параметр): Тип поля + Формат поля                   }
{   Тип поля:                                                                  }
{    n - числовой формат:                                                      }
{      #N%x.x или #Nyyyyy - числовое представление. x.x - длина числа/число    }
{      разрядов после запятой; yyyyy - строка типа #,##0.00 (более подробную   }
{      информацию можно получить в справочной системе Delphi, раздел           }
{      formatting strings). Если строка x.x или yyyyy содержит символы         }
{      ".", ",", "-", то последний такой символ будет использован в качестве   }
{      разделителя целой и дробной части форматированного числа.               }
{        Пример: n%2,2 или n##,.00###                                          }
{    d - формат даты/времени (используется функция  FormatDateTime Delphi)     }
{    b - логический формат: Истина,Ложь. Пример: bTrue,False                   }
{    s - строка (используется функция Format Delphi)                           }
{  Примеры:                                                                    }
{     <##DB1##Sum##ddd-mm-yyyy>, Результат: 10.10.2010                         }
{     <##DB1##Active##bMONEY,DEPO>, Результат: MONEY или DEPO                  }
{     <##DB1##Sum##nSum = %0,2f;>, Результат: Sum = 12345,00;                  }
{     <##DB1##Sum##nSum = %0.2n>, Результат: Sum = 12 345.00                   }
{     <##DB1##Sum##n#,.00###>, Результат: 12 345.00                            }
{     <##DB1##Name##s/<Name/> = '%s'>, Результат: <Name> = 'Иванов И.И.'       }
{******************************************************************************}
unit SBaseReportTXT;
{$I config.inc}

interface

uses
  SBaseReportInterface, SBaseConstants, NamedVariables;

{$IFDEF PATTERN_EXTERNAL}
procedure RegisterFactoryPattern(Report: IReport; var FactoryPattern: IFactoryPattern);
{$ENDIF}

implementation

uses
  Classes, DB, SysUtils, Variants, StrUtils, Windows,
  {$IFNDEF PATTERN_EXTERNAL}
  SBaseReport,
  {$ENDIF}
  {$IFNDEF REPORT_EXTERNAL}
  Controls, ComObj, Forms,
  {$ENDIF}
  fs_iinterpreter, SBaseUtils, SBasePreviewTxtForm, SBaseVariantFunctions;

type
  TSBaseReportTXT = class;

  TPatternTXT = class(TCustomPattern)
  private
    FEncoding: String;
    FVariables: TNamedVariants;
    FPatternStream: TStream;
  protected
    class function GetCode: String; override;
//    function GetCodePage(const Value: Variant): TCodePage;
  public
    constructor Create(Report: IReport); override;

    function ResultFileExt: String; override;
    procedure SetEncoding(const AEncoding: String); override;
    procedure PrepareReport(var APatternStorage: TPatternStorage; const DataSets: Array of TDataSet; const Variables: TNamedVariants); override;
    procedure SaveReport(AStream: TStream); override;
{$IFNDEF REPORT_EXTERNAL}
    procedure ShowReport(Owner: TForm; Caption: String; ShowModal: Boolean); override;
//    procedure DesignReport(Owner: TForm); override;
{$ENDIF}
  end;

  TFactoryPatterns = class(TCustomFactoryPatterns)
  protected
    function GetPatternClass(Index: Integer): TCustomPatternClass; override;
  end;

  TRSDataSet = class(TStringList)
  public
    DataSet: TDataSet;
    Parent: TRSDataSet;
    EmptyString: String;
    constructor Create(AParent: TRSDataSet);
    destructor Destroy; override;
    procedure Clear; override;
  end;

  TSBaseReportTxtStructure = class(TStringList)
  private
    FIndex: Integer;
    FReport: TSBaseReportTXT;
    FRoot, FItems: TRSDataSet;
    FSkipDataSetRow: Boolean;
    FLineDatasets: Array of TDataSet;
  protected
    procedure Build(ADataSet: TRSDataSet);
  public
    constructor Create(Report: TSBaseReportTXT);
    destructor Destroy; override;
    function IsEmpty: Boolean;
    procedure CheckEmpty;
    procedure BeginDataSet(const Name: String);
    procedure EndDataSet(const Name, EmptyString: String);
    procedure AddText(const AText: String);
    procedure AddField(const DataSetName, FieldName, Format: String);
    procedure AddVariable(const Name, Format: String);
    procedure AddScript(const Text, Format: String);
  end;

  TFormatCache = class(TObject)
  private
    FFormats: TStringList;
    FFormatSettings: TFormatSettings;
  public
    constructor Create(AReport: TSBaseReportTXT);
    destructor Destroy; override;
    procedure Clear;
    function Format(const AValue: Variant; const AFormatStr: String): String;
  end;

  TExpressionCache = class(TObject)
  private
    FExpressions: TStringList;
    FScript: TfsScript;
  public
    constructor Create(AReport: TSBaseReportTXT);
    destructor Destroy; override;
    procedure Clear;
    function Prepare(const Expression: String): TfsProcVariable;
    function Calc(Proc: TfsProcVariable): Variant;
  end;

  TFormatKind = (fkText, fkNumeric, fkFloat, fkDateTime, fkBoolean);
  TFormatData = record
    Kind      : TFormatKind;
    Format    : String;
    TrueValue : String;
    FalseValue: String;
    Settings  : TFormatSettings;
  end;
  PFormatData = ^TFormatData;

  TSBaseReportTXT = class(TThread)
  private
    FPattern: TPatternTXT;
    FStream: TStream;
    FPatternStream: TStream;

    FDone: Boolean;
    FError: String;
    Structure: TSBaseReportTxtStructure;
    FCurrentChar, FPredChar: Char;

    FPatternStreamReader: TStreamReader;
    FStreamWriter: TStreamWriter;
    FText: String;

    FFormatCache: TFormatCache;
    FExpressionCache: TExpressionCache;

    procedure ParseText;
    procedure ClearText(ExcludeCurrent: Boolean = False);
    function NextChar(SpecChar: Boolean = True): Char;

    procedure WriteString(const AStr: String);
    procedure Flush;

    function GetVariableFS(VarName: String; VarTyp: TfsVarType; OldValue: Variant): Variant;
    function PrepareExpression(const Expression: String): TfsProcVariable;
    function CalcExpression(Proc: TfsProcVariable): Variant;
    function FormatData(const AValue: Variant; const AFormatStr: String): String;
  protected
    function GetDataSet(const Name: String): TDataSet; overload;
    function GetVariable(const Name: String): Variant; overload;
    procedure Run;
    procedure Execute; override;
  public
    constructor Create(AOwner: TPatternTXT);
    destructor Destroy; override;
    property PatternStream: TStream read FPatternStream write FPatternStream;
    property Stream: TStream read FStream write FStream;
//    property CodePageFrom: TCodePage read FCodePageFrom write FCodePageFrom;
//    property CodePageTo: TCodePage read FCodePageTo write FCodePageTo;
  end;

const
  SPatternCode = 'TXT';
  SPatternResultFileExt = 'txt';

resourcestring
//  SPatternParam_CodePageFrom = 'CodePageFrom';
//  SPatternParam_CodePageTo = 'CodePageTo';

  SErrorPattern_NotFound = 'Не найден шаблон для отчета ''%s''';
  SErrorPattern_UnknownCodePage = 'Неизвестная кодировка ''%s''';
  SErrorPattern_NoDataSet = 'Не найден датасет ''%s''';
  SErrorPattern_NoBeginDataSet = 'Не найден открывающий тег для датасета ''%s''';
  SErrorPattern_NoEndDataSet = 'Не найден закрывающий тег для датасета ''%s''';
  SErrorPattern_Format = 'Неверно задана строка форматирования ''%s''';
  SErrorPattern_ExpressionCalc = 'Ошибка в выражении ''%s'': %s';

//const
//  PatternCodePages: Array [TCodePage] of String = (SPatternCodePage_Windows, SPatternCodePage_Dos, SPatternCodePage_Unicode, SPatternCodePage_KOI8);

{ TFormatCache }
constructor TFormatCache.Create(AReport: TSBaseReportTXT);
var
  i: Integer;
begin
  FFormats := TStringList.Create;
  FFormats.Sorted := True;
  FFormatSettings.CurrencyFormat := FormatSettings.CurrencyFormat;
  FFormatSettings.NegCurrFormat := FormatSettings.NegCurrFormat;
  FFormatSettings.ThousandSeparator := FormatSettings.ThousandSeparator;
  FFormatSettings.DecimalSeparator := FormatSettings.DecimalSeparator;
  FFormatSettings.CurrencyDecimals := FormatSettings.CurrencyDecimals;
  FFormatSettings.DateSeparator := FormatSettings.DateSeparator;
  FFormatSettings.ShortDateFormat := FormatSettings.ShortDateFormat;
  FFormatSettings.LongDateFormat := FormatSettings.LongDateFormat;
  FFormatSettings.TimeSeparator := FormatSettings.TimeSeparator;
  FFormatSettings.TimeAMString := FormatSettings.TimeAMString;
  FFormatSettings.TimePMString := FormatSettings.TimePMString;
  FFormatSettings.ShortTimeFormat := FormatSettings.ShortTimeFormat;
  FFormatSettings.LongTimeFormat := FormatSettings.LongTimeFormat;
  for i := 1 to Length(FormatSettings.ShortMonthNames) do
    FFormatSettings.ShortMonthNames[i] := FormatSettings.ShortMonthNames[i];
  for i := 1 to Length(FormatSettings.LongMonthNames)  do
    FFormatSettings.LongMonthNames[i] := FormatSettings.LongMonthNames[i];
  for i := 1 to Length(FormatSettings.ShortDayNames)  do
    FFormatSettings.ShortDayNames[i] := FormatSettings.ShortDayNames[i];
  for i := 1 to Length(FormatSettings.LongDayNames)  do
    FFormatSettings.LongDayNames[i] := FormatSettings.LongDayNames[i];
  FFormatSettings.TwoDigitYearCenturyWindow := FormatSettings.TwoDigitYearCenturyWindow;
  FFormatSettings.ListSeparator := FormatSettings.ListSeparator;
end;

destructor TFormatCache.Destroy;
var
  i: Integer;
begin
  for i := Pred(FFormats.Count) downto 0 do
    Dispose(PFormatData(FFormats.Objects[i]));
  FFormats.Free;
  inherited;
end;

procedure TFormatCache.Clear;
begin
  FFormats.Clear;
end;

function TFormatCache.Format(const AValue: Variant; const AFormatStr: String): String;

  function InternalFormat(const Value: Variant; const FormatData: TFormatData): String;
  begin
    try
      case FormatData.Kind of
        fkText     : Result := SysUtils.Format(FormatData.Format, [String(Value)], FormatData.Settings);
        fkFloat    : Result := FormatFloat(FormatData.Format, Extended(Value), FormatData.Settings);
        fkNumeric  : Result := SysUtils.Format(FormatData.Format, [Extended(Value)], FormatData.Settings);
        fkDateTime : Result := FormatDateTime(FormatData.Format, Value, FormatData.Settings);
        fkBoolean  : if (Value = True) or (Value = 1)  then
                       Result := FormatData.TrueValue
                     else
                       Result := FormatData.FalseValue;
        else Result := Value;
      end;
    except
      Result := VarToStr(Value);
    end;
  end;

  function CreateFormatDate(const Format: String): PFormatData;
  begin
    New(Result);
    Result.TrueValue :=  DefaultTrueBoolStr;
    Result.FalseValue := DefaultFalseBoolStr;
    Result.Settings := FFormatSettings;
    Result.Format := Format;
  end;

var
  i, LPos: Integer;
  LFormatData: PFormatData;
begin
  if not VarIsPresent(AValue) then
    Exit('');

  if AFormatStr.IsEmpty then
    Exit(VarToStr(AValue));

  i := FFormats.IndexOf(AFormatStr);
  if i >= 0 then
    LFormatData := PFormatData(FFormats.Objects[i])
  else begin
    LFormatData := CreateFormatDate(Copy(AFormatStr, 2, MaxInt));
    case AFormatStr[1] of
      'S', 's': LFormatData.Kind := fkText;
      'N', 'n': begin
                  LFormatData.Kind := fkNumeric;
                  if CharInSet(LFormatData.Format[1], ['0', '#']) then
                    LFormatData.Kind := fkFloat;
                  LPos := 0;
                  for i := 1 to Length(LFormatData.Format) do
                  begin
                    if (LFormatData.Kind = fkFloat) and (LFormatData.Format[i] = ';') then break;
                    if CharInSet(LFormatData.Format[i], ['.', ',', '-']) then
                    begin
                      LFormatData.Settings.DecimalSeparator := LFormatData.Format[i];
                      LPos := i;
                    end;
                  end;
                  if (LPos > 0) and (LFormatData.Settings.DecimalSeparator <> '.') then LFormatData.Format[LPos] := '.';
                end;
      'D', 'T', 'd', 't': LFormatData.Kind := fkDateTime;
      'B', 'b': begin
                  LFormatData.Kind := fkBoolean;
                  if LFormatData.Format <> '' then
                  begin
                    LFormatData.TrueValue := Copy(LFormatData.Format, 1, Pos(',', LFormatData.Format) - 1);
                    LFormatData.FalseValue := Copy(LFormatData.Format, Pos(',', LFormatData.Format) + 1, MaxInt);
                  end;
                end;
      else
        raise EPatternError.CreateFmt(SErrorPattern_Format, [AFormatStr])
    end;
    FFormats.AddObject(AFormatStr, TObject(LFormatData));
  end;
  Result := InternalFormat(AValue, LFormatData^);
end;

{ TExpressionCache }
constructor TExpressionCache.Create(AReport: TSBaseReportTXT);
var
  i: Integer;
begin
  FExpressions := TStringList.Create;
  FExpressions.Sorted := True;
  FScript := TfsScript.Create(nil);
  FScript.Parent := fsGlobalUnit;
  FScript.AutoDeclareClasses := True;
  FScript.OnGetVarValue := AReport.GetVariableFS;
  for i := 0 to Pred(AReport.FPattern.DataSetCount) do
    FScript.AddComponent(AReport.FPattern.DataSets[i]);
end;

destructor TExpressionCache.Destroy;
begin
  FExpressions.Free;
  FScript.Free;
  inherited;
end;

procedure TExpressionCache.Clear;
begin
  FExpressions.Clear;
  FScript.Clear;
end;

function TExpressionCache.Prepare(const Expression: String): TfsProcVariable;
var
  AIndex: Integer;
begin
  AIndex := FExpressions.IndexOf(Expression);
  if AIndex < 0 then
  begin
    AIndex := FExpressions.Count;
    FScript.Lines.Text := 'function SBaseReportTXT' + IntToStr(AIndex) + ': Variant; begin Result := ' + Expression + ' end; begin end.';
    if FScript.Compile then
      Result := TfsProcVariable(FScript.Find('SBaseReportTXT' + IntToStr(AIndex)))
    else
      raise EPatternError.CreateFmt(SErrorPattern_ExpressionCalc, [Expression, FScript.ErrorMsg]);
    FExpressions.AddObject(Expression, Result);
  end
  else
    Result := TfsProcVariable(FExpressions.Objects[AIndex]);
end;

function TExpressionCache.Calc(Proc: TfsProcVariable): Variant;
begin
  FScript.Parent.MainProg := False;
  try
    Result := Proc.Value;
  finally
    FScript.Parent.MainProg := True;
  end;
end;

{ TRSDataSet }
constructor TRSDataSet.Create(AParent: TRSDataSet);
begin
  Parent := AParent;
  if Assigned(Parent) then Parent.AddObject('', Self);
end;

destructor TRSDataSet.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TRSDataSet.Clear;
var
  i: Integer;
begin
  for i := Pred(Count) downto 0 do
    if Objects[i] is TRSDataSet then Objects[i].Free;
  DataSet := nil;
  inherited;
end;

{ TSBaseReportTxtStructure }
constructor TSBaseReportTxtStructure.Create(Report: TSBaseReportTXT);
begin
  inherited Create;
  FSkipDataSetRow := False;
  FReport := Report;
  FRoot := TRSDataSet.Create(nil);
  FItems := nil;
  FIndex := -1;
end;

destructor TSBaseReportTxtStructure.Destroy;
begin
  FRoot.Free;
  inherited;
end;

function TSBaseReportTxtStructure.IsEmpty: Boolean;
begin
  Result := not Assigned(FItems);
end;

procedure TSBaseReportTxtStructure.CheckEmpty;
begin
  if (Length(FLineDatasets) > 0) then
    raise EPatternError.CreateFmt(SErrorPattern_NoEndDataSet, [FLineDatasets[Pred(Length(FLineDatasets))].Name]);
  if Assigned(FItems) then
    raise EPatternError.CreateFmt(SErrorPattern_NoEndDataSet, [FItems.DataSet.Name]);
end;

procedure TSBaseReportTxtStructure.Build(ADataSet: TRSDataSet);
var
  i: Integer;
  AObject: TObject;
begin
  if ADataSet.DataSet.IsEmpty then
{$WARNINGS OFF}
    FReport.WriteString(ADataSet.EmptyString)
{$WARNINGS ON}
  else begin
    ADataSet.DataSet.First;
    while not ADataSet.DataSet.Eof do begin
      for i := 0 to Pred(ADataSet.Count) do begin
        AObject := ADataSet.Objects[i];
        if Assigned(AObject) then begin
          if AObject is TField then
{$WARNINGS OFF}
            FReport.WriteString(FReport.FormatData(TField(AObject).AsVariant, ADataSet[i]))
          else if AObject is TfsProcVariable then
            FReport.WriteString(FReport.FormatData(FReport.CalcExpression(TfsProcVariable(AObject)), ADataSet[i]))
          else if AObject is TRSDataSet then
            Build(TRSDataSet(AObject));
        end else
          FReport.WriteString(ADataSet[i]);
{$WARNINGS ON}
      end;
      ADataSet.DataSet.Next;
    end;
  end;
end;

procedure TSBaseReportTxtStructure.BeginDataSet(const Name: String);
var
  LIndex: Integer;
  LDataSet: TDataSet;
begin
  LDataSet := FReport.GetDataSet(Name);
  FSkipDataSetRow := LDataSet.IsEmpty;
  if IsEmpty and (LDataSet.RecordCount <= 1) then begin
    LIndex := Length(FLineDatasets);
    SetLength(FLineDatasets, Succ(LIndex));
    FLineDatasets[LIndex] := LDataSet;
    if not LDataSet.IsEmpty then
      LDataSet.First;
  end else begin
    if not Assigned(FItems) then
      FItems := FRoot
    else
      FItems := TRSDataSet.Create(FItems);
    FItems.DataSet := LDataSet;
  end;
end;

procedure TSBaseReportTxtStructure.EndDataSet(const Name, EmptyString: String);
var
  LIndex: Integer;
  LDataSet: TDataSet;
begin
  LDataSet := FReport.GetDataSet(Name);
  if IsEmpty then begin
    LIndex := Pred(Length(FLineDatasets));
    if (LIndex < 0) or (FLineDatasets[LIndex] <> LDataSet) then
      raise EPatternError.CreateFmt(SErrorPattern_NoBeginDataSet, [Name]);
    SetLength(FLineDatasets, LIndex);
    FSkipDataSetRow := (LIndex > 0) and (FLineDatasets[LIndex].IsEmpty);
{$WARNINGS OFF}
    if LDataSet.IsEmpty then FReport.WriteString(EmptyString);
{$WARNINGS ON}
  end else begin
    if LDataSet <> FItems.DataSet then
      raise EPatternError.CreateFmt(SErrorPattern_NoBeginDataSet, [LDataSet.Name]);
    FItems.EmptyString := EmptyString;
    FSkipDataSetRow := False;
    if FItems = FRoot then begin
      Build(FItems);
      FRoot.Clear;
      FItems := nil;
    end
    else FItems := FItems.Parent;
  end;
end;

procedure TSBaseReportTxtStructure.AddText(const AText: String);
begin
  if not IsEmpty then
    FItems.Add(AText)
  else
    if not FSkipDataSetRow then FReport.WriteString(AText);
end;

procedure TSBaseReportTxtStructure.AddField(const DataSetName, FieldName, Format: String);
begin
  if not IsEmpty then
    FItems.AddObject(Format, FReport.GetDataSet(DataSetName).FieldByName(FieldName))
  else
{$WARNINGS OFF}
    if not FSkipDataSetRow then FReport.WriteString(FReport.FormatData(FReport.GetDataSet(DataSetName).FieldByName(FieldName).AsVariant, Format));
{$WARNINGS ON}
end;

procedure TSBaseReportTxtStructure.AddVariable(const Name, Format: String);
begin
  if not IsEmpty then
    FItems.Add(FReport.FormatData(FReport.GetVariable(Name), Format))
  else
{$WARNINGS OFF}
    if not FSkipDataSetRow then FReport.WriteString(FReport.FormatData(FReport.GetVariable(Name), Format));
{$WARNINGS ON}
end;

procedure TSBaseReportTxtStructure.AddScript(const Text, Format: String);
begin
  if not IsEmpty then
    FItems.AddObject(Format, FReport.PrepareExpression(Text))
  else
{$WARNINGS OFF}
    if not FSkipDataSetRow then FReport.WriteString(FReport.FormatData(FReport.CalcExpression(FReport.PrepareExpression(Text)), Format));
{$WARNINGS ON}
end;

{ TSBaseReportTXT }
constructor TSBaseReportTXT.Create(AOwner: TPatternTXT);
begin
  inherited Create(True);
  FDone := False;
  FPattern := AOwner;
  FFormatCache := TFormatCache.Create(Self);
  FExpressionCache := TExpressionCache.Create(Self);
//  FCodePageFrom := cpWindows;
//  FCodePageTo := cpWindows;
end;

destructor TSBaseReportTXT.Destroy;
begin
  FFormatCache.Free;
  FExpressionCache.Free;
  inherited;
end;

function TSBaseReportTXT.GetDataSet(const Name: String): TDataSet;
begin
  Result := FPattern.FindDataSetByName(Name);
  if not Assigned(Result) then
    raise EPatternError.CreateFmt(SErrorPattern_NoDataSet, [Name]);
end;

function TSBaseReportTXT.GetVariable(const Name: String): Variant;
begin
  Result := FPattern.FVariables[Name];
end;

function TSBaseReportTXT.GetVariableFS(VarName: String; VarTyp: TfsVarType; OldValue: Variant): Variant;
begin
  Result := FPattern.FVariables[VarName];
end;

function TSBaseReportTXT.PrepareExpression(const Expression: String): TfsProcVariable;
begin
  Result := FExpressionCache.Prepare(Expression);
end;

function TSBaseReportTXT.CalcExpression(Proc: TfsProcVariable): Variant;
begin
  Result := FExpressionCache.Calc(Proc);
end;

procedure TSBaseReportTXT.Flush;
begin
  FStreamWriter.Flush;
end;

function TSBaseReportTXT.FormatData(const AValue: Variant; const AFormatStr: String): String;
begin
  Result := FFormatCache.Format(AValue, AFormatStr);
end;

//procedure TSBaseReportTXT.WriteBuffer(const ABuffer: TCharArray; ACount: Integer);
//begin
//  if ACount = 0 then Exit;
//  FStreamWriter.Write(ABuffer, 0, ACount);
//end;

procedure TSBaseReportTXT.WriteString(const AStr: String);
begin
  FStreamWriter.Write(AStr); // WriteBuffer(PChar(AStr), Length(AStr));
end;

function TSBaseReportTXT.NextChar(SpecChar: Boolean = True): Char;
var
  LPeek: Integer;
begin
  FPredChar := FCurrentChar;

  LPeek := FPatternStreamReader.Read;
  if LPeek = -1 then
    Result := #0
  else
    Result := Char(LPeek);

  if Result <> #0 then begin
    if SpecChar and CharInSet(Result, ['<', '>', '#']) and (FPredChar = '/') then begin
      FText[FText.Length] := Result;
      Result := '/';
    end else
      FText := FText + Result;
  end;
  FCurrentChar := Result;
end;

procedure TSBaseReportTXT.ClearText(ExcludeCurrent: Boolean);
begin
  if ExcludeCurrent then begin
    if not FText.IsEmpty then
      SetLength(FText, Pred(FText.Length));
  end;

  if not FText.IsEmpty then
    Structure.AddText(FText);

  if ExcludeCurrent then begin
    FText := FCurrentChar;
//    FTextPos := 0;
//    FText[0] := FCurrentChar;
  end else
    SetLength(FText, 0);
end;

procedure TSBaseReportTXT.ParseText;
var
  T: Char;
  F, FF: Integer;
  LFormat: String;
  LCloseDataSet: Boolean;
begin
  ClearText(True);
  FF := 0;
  NextChar(False);
  LCloseDataSet := FCurrentChar = '/';
  if LCloseDataSet then NextChar(False);
  if (FCurrentChar = '#') and CharInSet(NextChar, ['#', '.', '[']) then begin
    T := FCurrentChar;
    while not CharInSet(NextChar, [#0, '<', '>']) and not ((FPredChar = '#') and (FCurrentChar = '#')) do ;

    if ((FPredChar = '#') and (FCurrentChar = '#')) or (FCurrentChar = '>') then begin
      if (FCurrentChar = '>') and (T = '#') then exit;
      if FCurrentChar = '>' then
        F := FText.Length - 4
      else begin
        F := FText.Length - 5;
        if (T = '#') and not LCloseDataSet then begin
          while not CharInSet(NextChar, [#0, '<', '>']) and not ((FPredChar = '#') and (FCurrentChar = '#')) do ;
          if (FPredChar = '#') and (FCurrentChar = '#') then
            if FCurrentChar = '>' then
              FF := FText.Length - 3
            else
              FF := FText.Length - 4;
        end;
      end;

      if FCurrentChar <> '>' then
        while not CharInSet(NextChar, [#0, '<', '>']) do ;

      if FCurrentChar = '>' then begin
        if (F < FText.Length - 6) then
          if FF > 0 then
            LFormat := Copy(FText, F + 6, FF - F - 2)
          else
            LFormat := Copy(FText, F + 6, FText.Length - F - 6)
        else
          LFormat := '';

        case T of
          '#': if LCloseDataSet then
                 Structure.EndDataSet(Copy(FText, 5, F - 1), LFormat)
               else if FText.Length = F + 6 then
                 Structure.BeginDataSet(Copy(FText, 4, F))
               else if FF > 0 then
                 Structure.AddField(Copy(FText, 4, F), LFormat, Copy(FText, FF + 6, FText.Length - FF - 5))
               else
                 Structure.AddField(Copy(FText, 4, F), LFormat, '');
          '.': Structure.AddVariable(Copy(FText, 4, F), LFormat);
          '[': if FText[F + 2] = ']' then
                 Structure.AddScript(Copy(FText, 4, F - 1), LFormat);
        end;
        SetLength(FText, 0); // ClearBuffer
      end;
    end;
  end;
end;

procedure TSBaseReportTXT.Run;
var
  LPageDataSet: TDataSet;
  LPreamble: TBytes;
  LEncoding: TEncoding;
begin
  FPredChar := #0;
  SetLength(FText, 0);

  if FPattern.FEncoding.IsEmpty then
    LEncoding := nil
  else
    LEncoding := TEncoding.GetEncoding(FPattern.FEncoding);

//  try
    FStreamWriter := TStreamWriter.Create(FStream);
    try
      LPreamble := FStreamWriter.Encoding.GetPreamble;
      if Length(LPreamble) > 0 then
        Stream.WriteBuffer(LPreamble, Length(LPreamble));

      FPatternStreamReader := TStreamReader.Create(FPatternStream, LEncoding, True); // TEncoding.ANSI
      try
        Structure := TSBaseReportTxtStructure.Create(Self);
        try
          LPageDataSet := FPattern.Report.GetPageDataSet;
          if Assigned(LPageDataSet) then Structure.BeginDataSet(LPageDataSet.Name);
          NextChar(False);
          while FCurrentChar <> #0 do begin
            if FCurrentChar = '<' then ParseText;
            //if FTextPos = Pred(FBufSize) then ClearText;
            NextChar(False);
          end;
          ClearText;
          if Assigned(LPageDataSet) then
            Structure.EndDataSet(LPageDataSet.Name, '');
          ClearText;
          Flush;
          Structure.CheckEmpty;
        finally
          FreeAndNil(FPatternStreamReader);
          SetLength(FText, 0);
        end;
      finally
        Structure.Free;
      end;
    finally
      FreeAndNil(FStreamWriter);
    end;
//  finally
//    if Assigned(LEncoding) and not TEncoding.IsStandardEncoding(LEncoding) then
//      LEncoding.Free;
//    LEncoding := nil;
//  end;
end;

procedure TSBaseReportTXT.Execute;
begin
  FError := '';
  try
    try
      Run;
    finally
      FDone := True;
    end;
  except on E: Exception do begin
    FError := E.Message;
    raise;
  end; end;
end;

{ TPatternTXT }
constructor TPatternTXT.Create(Report: IReport);
begin
  inherited;
  FVariables.UnknownAsNull := True;
end;

class function TPatternTXT.GetCode: String;
begin
  Result := SPatternCode;
end;

function TPatternTXT.ResultFileExt: String;
begin
  Result := SPatternResultFileExt;
end;

//function TPatternTXT.GetCodePage(const Value: Variant): TCodePage;
//var
//  ACodeStr: String;
//begin
//  ACodeStr := VarToStr(Value);
//  if ACodeStr = '' then
//    Result := cpWindows
//  else
//  begin
//    for Result := Low(PatternCodePages) to High(PatternCodePages) do
//      if SameText(PatternCodePages[Result], ACodeStr) then exit;
//    raise ERepError.CreateFmt(SErrorPattern_UnknownCodePage, [ACodeStr]);
//  end;
//end;

procedure TPatternTXT.PrepareReport(var APatternStorage: TPatternStorage; const DataSets: Array of TDataSet; const Variables: TNamedVariants);
begin
  inherited;

  FVariables     := Variables;
  FPatternStream := APatternStorage.PatternStream;
end;

procedure TPatternTXT.SaveReport(AStream: TStream);
var
  LReport: TSBaseReportTXT;
begin
  DisableDataControls;
  try
    LReport := TSBaseReportTXT.Create(Self);
    try
//      FReport.CodePageFrom := GetCodePage(ReportProperty[SPatternParam_CodePageFrom]);
//      LReport.CodePageTo := GetCodePage(ReportProperty[SPatternParam_CodePageTo]);

      LReport.FPattern := Self;
      LReport.PatternStream := FPatternStream;
      LReport.Stream := AStream;
      if MultiThread then begin
        LReport.Start;
{$IFNDEF REPORT_EXTERNAL}
        while not LReport.FDone do try
          Application.HandleMessage;
        except
          Application.HandleException(Application);
        end;
{$ENDIF}
        LReport.Terminate;
        if LReport.FError <> '' then
          raise Exception.Create(LReport.FError);
      end else
        LReport.Execute;
    finally
      LReport.Free;
    end;
  finally
    EnableDataControls;
  end;
end;

procedure TPatternTXT.SetEncoding(const AEncoding: String);
begin
  FEncoding := AEncoding;
end;

{$IFNDEF REPORT_EXTERNAL}
procedure TPatternTXT.ShowReport(Owner: TForm; Caption: String; ShowModal: Boolean);
var
  LOwner: TForm;
  LStream: TStringStream;
begin
  LStream := TStringStream.Create;
  try
    SaveReport(LStream);
    LStream.Position := 0;

    if Assigned(Owner) then
      LOwner := Owner
    else
      LOwner := Application.MainForm;

    with TSBasePreviewTxtFrm.CreateByParams(LOwner, TNamedVariants.Create([TNamedVariant.Create(SConst_Stream, Integer(LStream))], True)) do begin
      Caption := Caption;
      //FDefaultSaveName := Caption;
      Show;
    end;
  finally
    LStream.Free;
  end;
end;
{$ENDIF}

{ TFactoryPatterns }
function TFactoryPatterns.GetPatternClass(Index: Integer): TCustomPatternClass;
begin
  Result := TPatternTXT;
end;

{$IFNDEF PATTERN_EXTERNAL}

initialization
  RegisterFactoryPatterns(TFactoryPatterns.Create as IFactoryPatterns);
{$ENDIF}

end.
