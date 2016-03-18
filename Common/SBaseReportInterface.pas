{******************************************************************************}
{                       -= Модуль печати документов =-                         }
{                                                                              }
{                          -= Интерфейсы шаблонов =-                           }
{                                                                              }
{                           v4.1.1 (С) 2007 - 2014                             }
{                                                                              }
{                                                                              }
{******************************************************************************}
unit SBaseReportInterface;
{$I config.inc}

interface

uses
{$IFNDEF REPORT_EXTERNAL}
  Forms,
{$ENDIF}
  Types, Classes, Generics.Collections, Windows, Contnrs, SysUtils, DB, DBConsts, NamedVariables,
  fs_iinterpreter, SBaseFileUtils, StrUtils;

type
  TReportFileSignature = array[1..4] of Char;
  EReportError = class(Exception);
  ERepError = EReportError;

  IReport = interface(IInterface)
    ['{AB4C1827-4469-43AE-B665-B3F220ED93B5}']
    function GetOwner: TPersistent;
    function SupportProperty(Name: String): Boolean;
    function GetProperty(Name: String): Variant;
    function GetPageDataSet: TDataSet;
    function GetPageDataSetFieldName: String;
//    function GetTempFileName(Name, Ext: String; var FileName: String): Integer;
    procedure Terminate;
    procedure Progress(const AText: String; Pos, Count: Integer; var Cancel: Boolean);
  end;

  IReportDataSet = interface(IInterface)
    ['{A68EC65E-D908-41AB-9584-E605585BE926}']
    function  EOF: Boolean;
    procedure First;
    procedure Next;
    procedure SetQuickMasterDetail;
  end;

  TPatternStorage = record
  strict private
    FPatternFileName, FScriptFileName: String;
    FPatternStream: TStream;
    FPatternStreamCreated, FPatternTempFileCreated: Boolean;

    FScriptStream: TStream;
    FScriptStreamCreated: Boolean;
  private
    function GetPatternFileName: String;
    procedure SetPatternFileName(const Value: String);

    function  GetPatternStream: TStream;
    procedure SetPatternStream(const Value: TStream);

    function GetScriptStream: TStream;
    procedure SetScriptStream(const Value: TStream);
//    function GetScriptFileName: String;
//    procedure SetScriptFileName(const Value: String);
  public
    procedure Clear; // First init ?
    procedure Release;
    procedure CheckExists;
    function  StoragePath: String;

    property PatternFileName: String read GetPatternFileName write SetPatternFileName;
    property PatternStream: TStream read GetPatternStream write SetPatternStream;

    property ScriptFileName: String read FScriptFileName write FScriptFileName;
    property ScriptStream: TStream read GetScriptStream write SetScriptStream;
  end;

  IPattern = interface(IInterface)
    ['{AF7CE590-550B-449A-9893-2985361EAD37}']
    function  PatternCode: String;
    function  ResultFileExt: String;
    function  SupportProperty(AName: String): Boolean;
    function  GetProperty(AName: String): Variant;
    procedure SetProperty(AName: String; Value: Variant);
    procedure PrepareReport(var APatternStorage: TPatternStorage; const ADataSets: Array of TDataSet; const AVariables: TNamedVariants);
    procedure PrintReport(APrinter: String);
    procedure OpenReport(const AFileName, ACaption: String);

    procedure SetEncoding(const AEncoding: String);
    function  SaveReport(const AFileName: String): String; overload;
    procedure SaveReport(AStream: TStream); overload;
{$IFNDEF REPORT_EXTERNAL}
    procedure ShowReport(AForm: TForm; ACaption: String; AShowModal: Boolean);
    procedure DesignReport(AForm: TForm);
{$ENDIF}
  end;

  IPatternMerge = interface(IInterface)
    ['{A5E07DE3-BF7D-46E4-B1E5-DF4138DBC0F4}']
  end;

  IFactoryPatterns = interface(IInterface)
    ['{A3D1C159-1DC3-4CC4-BA89-45ADC24DC3EC}']
    function PatternCount: Integer;
    function PatternCode(Index: Integer): String;
    function SupportProperty(Name: String): Boolean;
    function GetProperty(Name: String): Variant;
    procedure SetProperty(Name: String; Value: Variant);
    function GetPattern(Report: IReport; Index: Integer): IPattern;
  end;

  EPatternError = class(Exception);
  TCustomFactoryPatterns = class;
  TCustomPattern = class;

  TReportScript = class(TfsScript)
  strict private
    FPattern: TCustomPattern;
  public
    constructor Create(APattern: TCustomPattern); reintroduce;
    function CallFunction(const AName, AParams: String): Variant; overload;

//    property Lines;
//    property SyntaxType;
//    property OnGetILUnit;
//    property OnGetUnit;
//    property OnRunLine;
//    property OnGetVarValue;
  published
    function FS(const AData: Array of Variant): Variant;
  end;

  TCustomPattern = class(TInterfacedObject, IPattern)
  private
    FDataSets: TList<TDataSet>;
    FChildObjects: TList<TObject>;
    FFactoryPatterns: TCustomFactoryPatterns;
    FTerminated: Boolean;

    function GetDataSetCount: Integer;
    function GetDataSets(Index: Integer): TDataSet;
    function GetMultiThread: Boolean;
    procedure SetTerminated(const Value: Boolean);
  protected
    Report: IReport;

    class function GetCode: String; virtual; abstract;
    class function ScriptSupport: Boolean; dynamic;

    function PatternCode: String;
    function ResultFileExt: String; virtual; abstract;
    function GetPageDataSet: TDataSet;
    function GetPageDataSetFieldName: String;
    function GetReportProperty(const Name: String): Variant;

    function GetDataSetPrefix: String; virtual;

    procedure DisableDataControls;
    procedure EnableDataControls;
    procedure AddChildObject(AObject: TObject);
    procedure RemoveChildObject(AObject: TObject);
    function  FindChildComponentByName(const AName: String): TComponent;

    property FactoryPatterns: TCustomFactoryPatterns read FFactoryPatterns;
  public
    FScript: TReportScript;

    constructor Create(Report: IReport); virtual;
    destructor Destroy; override;

    function SupportProperty(Name: String): Boolean;
    function GetProperty(Name: String): Variant;
    procedure SetProperty(Name: String; Value: Variant);
    procedure SetEncoding(const AEncoding: String); dynamic; abstract;

    function GetVariables(Name: String): Variant; dynamic; abstract;

    procedure PrepareReport(var APatternStorage: TPatternStorage; const ADataSets: Array of TDataSet; const AVariables: TNamedVariants); virtual;
    procedure PrintReport(Printer: String); virtual;
    procedure OpenReport(const FileName, Caption: String); virtual;
{$IFNDEF REPORT_EXTERNAL}
    procedure ShowReport(Form: TForm; Caption: String; ShowModal: Boolean); virtual;
    procedure DesignReport(Form: TForm); virtual;
{$ENDIF}

    function  SaveReport(const AFileName: String): String; overload; virtual;
    procedure SaveReport(AStream: TStream); overload; virtual;

    function FindDataSetByName(const DataSetName: String): TDataSet;
    function GetDataSetByName(const ADataSetName: String): TDataSet;

    procedure Progress(Text: String; Pos, Count: Integer; var Cancel: Boolean);

    property ReportProperty[const Name: String]: Variant read GetReportProperty;
    property PageDataSet: TDataSet read GetPageDataSet;
    property PageFieldName: String read GetPageDataSetFieldName;
    property MultiThread: Boolean read GetMultiThread;
    property DataSets[Index: Integer]: TDataSet read GetDataSets;
    property DataSetCount: Integer read GetDataSetCount;
    property Terminated: Boolean read FTerminated write SetTerminated;
  end;
  TCustomPatternClass = class of TCustomPattern;

  TCustomFactoryPatterns = class(TInterfacedObject, IFactoryPatterns)
  protected
    FFileNames: TStrings;
    function GetPatternClass(Index: Integer): TCustomPatternClass; virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function PatternCount: Integer; virtual;
    function PatternCode(Index: Integer): String; virtual;
    function SupportProperty(Name: String): Boolean;
    function GetProperty(Name: String): Variant;
    procedure SetProperty(Name: String; Value: Variant);
    function GetPattern(Report: IReport; Index: Integer): IPattern;
  end;

const
  SReportDataSetPrefix = 'DB';
  SReportFileSignature: TReportFileSignature = 'RLP0';
  SReportError_PatternFileNotExists = 'Не найден файл шаблона ''%s''';

implementation

uses
  Variants, TypInfo, SBaseConstants, SBaseStringFunctions;

resourcestring
  SErrorPatternNotSupported = 'Операция не поддерживается шаблоном ''%s''';

{ TCustomPattern }
constructor TCustomPattern.Create(Report: IReport);
begin
  inherited Create;
  FTerminated := False;
//  FDataSets := nil;
  FChildObjects := TList<TObject>.Create;
  Self.Report := Report;
end;

destructor TCustomPattern.Destroy;
var
  I: Integer;
begin
  FreeAndNil(FDataSets);
  FreeAndNil(FScript);

  for I := Pred(FChildObjects.Count) downto 0 do
    FChildObjects[I].Free;
  FChildObjects.Free;
  inherited;
end;

function TCustomPattern.PatternCode: String;
begin
  Result := GetCode;
end;

function TCustomPattern.GetProperty(Name: String): Variant;
begin
  Result := GetPropValue(Self, Name);
end;

procedure TCustomPattern.PrepareReport(var APatternStorage: TPatternStorage; const ADataSets: array of TDataSet; const AVariables: TNamedVariants);
var
  I: Integer;
begin
  if not Assigned(FDataSets) then
    FDataSets := TList<TDataSet>.Create
  else
    FDataSets.Clear;

  if ScriptSupport and Assigned(APatternStorage.ScriptStream) then begin
    FScript := TReportScript.Create(Self);
    with FScript do begin
      AutoDeclareClasses        := True;
      Parent                    := fsGlobalUnit;
      Parent.AutoDeclareClasses := True;

      Lines.LoadFromStream(APatternStorage.ScriptStream);
    end;
  end;

  for I := 0 to Pred(Length(ADataSets)) do begin
    FDataSets.Add(ADataSets[I]);
    if Assigned(FScript) then
      FScript.AddComponent(ADataSets[I], SReportDataSetPrefix + IntToStr(Succ(I)));
  end;

  if Assigned(FScript) then
    if not FScript.Compile then begin
      Raise Exception.Create('[' + FScript.ErrorPos + ']: ' + FScript.ErrorMsg);
    end;
end;

function TCustomPattern.SaveReport(const AFileName: String): String;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveReport(LStream);
  finally
    LStream.Free
  end;
  Result := AFileName;
end;

procedure TCustomPattern.SaveReport(AStream: TStream);
begin
  raise EPatternError.CreateFmt(SErrorPatternNotSupported, [GetCode]);
end;

class function TCustomPattern.ScriptSupport: Boolean;
begin
  Result := False
end;

procedure TCustomPattern.SetProperty(Name: String; Value: Variant);
begin
  SetPropValue(Self, Name, Value);
end;

{$IFNDEF REPORT_EXTERNAL}
procedure TCustomPattern.ShowReport(Form: TForm; Caption: String; ShowModal: Boolean);
begin
  raise EPatternError.CreateFmt(SErrorPatternNotSupported, [GetCode]);
end;

procedure TCustomPattern.DesignReport(Form: TForm);
begin
  raise EPatternError.CreateFmt(SErrorPatternNotSupported, [GetCode])
end;
{$ENDIF}

procedure TCustomPattern.PrintReport(Printer: String);
begin
  raise EPatternError.CreateFmt(SErrorPatternNotSupported, [GetCode]);
end;

procedure TCustomPattern.OpenReport(const FileName, Caption: String);
begin
  raise EPatternError.CreateFmt(SErrorPatternNotSupported, [GetCode]);
end;

function TCustomPattern.SupportProperty(Name: String): Boolean;
begin
  Result := IsPublishedProp(Self, Name);
end;

procedure TCustomPattern.AddChildObject(AObject: TObject);
begin
  FChildObjects.Add(AObject);
end;

procedure TCustomPattern.RemoveChildObject(AObject: TObject);
begin
  FChildObjects.Remove(AObject);
end;

function TCustomPattern.FindChildComponentByName(const AName: String): TComponent;
var
  I: Integer;
begin
  for I := 0 to FChildObjects.Count - 1 do
    if FChildObjects[I] is TComponent then with TComponent(FChildObjects[I]) do
      if Name = AName then
        Exit(TComponent(FChildObjects[I]));

  Result := nil;
end;

function TCustomPattern.GetDataSetCount: Integer;
begin
  if Assigned(FDataSets) then
    Result := FDataSets.Count
  else
    Result := 0;
end;

function TCustomPattern.GetDataSets(Index: Integer): TDataSet;
begin
  if Assigned(FDataSets) then
    Result := FDataSets[Index]
  else
    Result := nil;
end;

function TCustomPattern.GetDataSetPrefix: String;
begin
  Result := SReportDataSetPrefix;
end;

function TCustomPattern.GetPageDataSet: TDataSet;
begin
  Result := Report.GetPageDataSet;
end;

function TCustomPattern.GetPageDataSetFieldName: String;
begin
  Result := Report.GetPageDataSetFieldName;
end;

function TCustomPattern.FindDataSetByName(const DataSetName: String): TDataSet;
var
  AIndex: Integer;
  ADataSetPrefix: String;
begin
  ADataSetPrefix := GetDataSetPrefix;
  Result := nil;
  if not Assigned(FDataSets) then Exit;
  if SameText(SReportDataSetPrefix, Copy(DataSetName, 1, Length(ADataSetPrefix))) then begin
    if TryStrToInt(Copy(DataSetName, Succ(Length(ADataSetPrefix)), MaxInt), AIndex) then
      if (AIndex > 0) and (AIndex <= FDataSets.Count) then
        Result := FDataSets[Pred(AIndex)];
  end;
end;

function TCustomPattern.GetDataSetByName(const ADataSetName: String): TDataSet;
begin
  Result := FindDataSetByName(ADataSetName);
  if Result = nil then
    DatabaseErrorFmt(MDataSetNotFound, [ADataSetName]);
end;

procedure TCustomPattern.DisableDataControls;
var
  I: Integer;
begin
  if not Assigned(FDataSets) then exit;
  for I := Pred(FDataSets.Count) downto 0 do
    FDataSets[I].DisableControls;
end;

procedure TCustomPattern.EnableDataControls;
var
  I: Integer;
begin
  if not Assigned(FDataSets) then Exit;
  for I := Pred(FDataSets.Count) downto 0 do
    FDataSets[I].EnableControls;
end;

function TCustomPattern.GetReportProperty(const Name: String): Variant;
begin
  if Report.SupportProperty(Name) then
    Result := Report.GetProperty(Name)
  else
    Result := Null;
end;

function TCustomPattern.GetMultiThread: Boolean;
begin
  Result := (GetReportProperty('MultiThread') = True); // Do not localize
end;

procedure TCustomPattern.Progress(Text: String; Pos, Count: Integer; var Cancel: Boolean);
begin
  Report.Progress(Text, Pos, Count, Cancel);
  FTerminated := Cancel;
end;

procedure TCustomPattern.SetTerminated(const Value: Boolean);
begin
  FTerminated := Value;
  if FTerminated then Report.Terminate;
end;

{ TCustomFactoryPatterns }
constructor TCustomFactoryPatterns.Create;
begin
  inherited;
end;

destructor TCustomFactoryPatterns.Destroy;
begin
  inherited;
end;

function TCustomFactoryPatterns.GetPattern(Report: IReport; Index: Integer): IPattern;
var
  APattern: TCustomPattern;
  APatternClass: TCustomPatternClass;
begin
  APatternClass := GetPatternClass(Index);
  APattern := APatternClass.Create(Report);
  APattern.FFactoryPatterns := Self;
  Result := APattern;
end;

function TCustomFactoryPatterns.GetProperty(Name: String): Variant;
begin
  Result := GetPropValue(Self, Name);
end;

function TCustomFactoryPatterns.PatternCode(Index: Integer): String;
begin
  Result := GetPatternClass(Index).GetCode;
end;

function TCustomFactoryPatterns.PatternCount: Integer;
begin
  Result := 1;
end;

procedure TCustomFactoryPatterns.SetProperty(Name: String; Value: Variant);
begin
  SetPropValue(Self, Name, Value);
end;

function TCustomFactoryPatterns.SupportProperty(Name: String): Boolean;
begin
  Result := IsPublishedProp(Self, Name);
end;

{ TPattern }

procedure TPatternStorage.CheckExists;
begin
  if (FPatternStream = nil) and (FPatternFileName.IsEmpty or not FileExists(FPatternFileName)) then
    Raise ERepError.CreateFmt(SReportError_PatternFileNotExists, [FPatternFileName]);
end;

procedure TPatternStorage.Clear;
begin
  FPatternStream   := nil;
  FPatternFileName := '';
  FPatternStreamCreated    := False;
  FPatternTempFileCreated  := False;

  FScriptStream   := nil;
  FScriptFileName := '';
  FScriptStreamCreated := False;
end;

procedure TPatternStorage.Release;
begin
  if FPatternStreamCreated then
    FreeAndNil(FPatternStream);

  if FScriptStreamCreated then
    FreeAndNil(FScriptStream);

  if FPatternTempFileCreated and (not FPatternFileName.IsEmpty) and FileExists(FPatternFileName) then
    SysUtils.DeleteFile(FPatternFileName);

  Clear;
//  FTempFileCreated := False;
//  FFileName := '';
end;

function TPatternStorage.GetPatternFileName: String;
begin
  if FPatternStream = nil then
    Result := FPatternFileName
  else begin
    Result := GetTempFile();
    with TFileStream.Create(Result, fmCreate) do try
      CopyFrom(FPatternStream, FPatternStream.Size);
      FPatternFileName := Result;
    finally
      Free
    end;
  end;
end;

//function TPatternStorage.GetScriptFileName: String;
//begin
//  if FScriptStream = nil then
//    Result := FScriptFileName
//  else begin
//    Result := GetTempFile();
//    with TFileStream.Create(Result, fmCreate) do try
//      CopyFrom(FScriptStream, FScriptStream.Size);
//      FScriptFileName := Result;
//    finally
//      Free
//    end;
//  end;
//end;

function TPatternStorage.GetScriptStream: TStream;
begin
  if (FScriptStream = nil) and (FScriptFileName.IsEmpty) then
    Exit(nil)
  else if FScriptStream = nil then begin
    FScriptStream := TFileStream.Create(FScriptFileName, fmOpenRead or fmShareDenyWrite);
    FScriptStreamCreated := True;
  end;
  Result := FScriptStream;
end;

function TPatternStorage.GetPatternStream: TStream;
begin
  if (FPatternStream = nil) and (FPatternFileName.IsEmpty) then
    Exit(nil)
  else if FPatternStream = nil then begin
    FPatternStream := TFileStream.Create(FPatternFileName, fmOpenRead or fmShareDenyWrite);
    FPatternStreamCreated := True;
  end;
  Result := FPatternStream;
end;

procedure TPatternStorage.SetPatternFileName(const Value: String);
begin
  if FPatternFileName <> Value then begin
    Release;
    FPatternFileName := Value;
  end;
end;

procedure TPatternStorage.SetScriptStream(const Value: TStream);
begin
  if FScriptStream <> Value then begin
    FScriptStream := Value;
  end;
end;

procedure TPatternStorage.SetPatternStream(const Value: TStream);
begin
  if FPatternStream <> Value then begin
    FPatternStream := Value;
  end;
end;

function TPatternStorage.StoragePath: String;
begin
  if Assigned(FPatternStream) then
    if FPatternStream is TFileStream then
      Result := TFileStream(FPatternStream).FileName
    else
      Result := '(Stream)'
  else
    Result := FPatternFileName
end;

{ TReportScript }

constructor TReportScript.Create(APattern: TCustomPattern);
begin
  FPattern := APattern;
  inherited Create(nil);
end;

function TReportScript.CallFunction(const AName, AParams: String): Variant;
var
  LVariable: TfsCustomVariable;
  LItem: TStringItem;
  LParam: String;
  LIdx: Integer;
begin
  if Assigned(Self) then
    LVariable := FindLocal(AName)
  else
    LVariable := nil;

  if (LVariable = nil) or not (LVariable is TfsProcVariable) then
    Raise Exception.Create('The function '#171 + AName + #187' does not exists.');

  for LItem in EnumStringItems(AParams, ',', False, [esmTrim{, esmSkipEmpty}]) do begin
    LParam := LItem.Value;
    if LParam.IsEmpty then
      Raise Exception.Create('Parameter #' + IntToStr(Succ(LItem.Index)) + ' of function '#171 + AName + #187' cannot be empty.');

    if LParam[1] = '@' then begin
      LParam := Copy(LParam, 2, MaxInt);
      LIdx := Pos('.', LParam);
      if (LIdx > 0) then
        LVariable.Params[LItem.Index].Value := FPattern.GetDataSetByName(LeftStr(LParam, LIdx - 1)).FieldByName(Copy(LParam, LIdx + 1, MaxInt)).AsVariant
      else
        LVariable.Params[LItem.Index].Value := FPattern.GetVariables(LParam);
    end else
      LVariable.Params[LItem.Index].Value := LParam;
  end;


  Result := LVariable.Value;
end;

function TReportScript.FS(const AData: Array of Variant): Variant;
var
  LParams, LFormat: String;
  I: Integer;
begin
  I := Length(AData);
  if I > 3 then
    Raise Exception.Create('Method '#171'FS'#187' cannot be called with more then 3 parameters')
  else if I = 3 then
    LFormat := VarToStr(AData[2])
  else
    LFormat := '';

  if I > 1 then
    LParams := VarToStr(AData[1])
  else
    LParams := '';

  Result := CallFunction(VarToStr(AData[0]), LParams);
end;

end.
