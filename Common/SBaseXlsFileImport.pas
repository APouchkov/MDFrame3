unit SBaseXlsFileImport;

interface

uses
  Classes, SysUtils, SBaseImport, XLSXAdapter, UFlexCelImport, FlexCel.XlsAdapter,
  RegularExpressions, Variants, Types
  , fs_iinterpreter
  , fsFunction_Other
  ;

type
  TSBaseXlsFileImportProvider = class(TSBaseImportProvider)
  private
    FAdapter: TXLSXAdapter;
    FFile: TFlexCelImport;
    FTempFile: String;

    FRecordNo: Integer;

    function GetEOF: Boolean;
    function GetSheetCount: Integer;
    function GetActiveSheetNo: Integer;
    procedure SetActiveSheetNo(const Value: Integer);
    function GetLineValues(AIndex: Integer): Variant;
    function GetActiveSheetName: String;
    procedure SetActiveSheetName(const Value: String);
  protected
    function FileMode: TSBaseImportFileMode; override;
    function RecordNo: Integer; override;
    procedure InternalExecute; override;
  public
    class function Code: String; override;
    class function FastScriptClass: TfsRTTIModuleClass; override;

    constructor Create(AOwner: TSBaseImport); override;
    destructor Destroy; override;
    procedure Open(const AFileName: String); override;
    procedure Close; override;

    procedure Next;
    property LineValues[AIndex: Integer]: Variant read GetLineValues;
    function FindValueByName(const AName: String): Integer;
    function FindValueByRegExpr(const ARegExpr: String): Integer;
  published
    //property RecNo: Integer read FRecordNo write FRecordNo;
    property ActiveSheetNo: Integer read GetActiveSheetNo write SetActiveSheetNo;
    property ActiveSheetName: String read GetActiveSheetName write SetActiveSheetName;
    property SheetCount: Integer read GetSheetCount;
    property EOF: Boolean read GetEOF;
  end;

  TfsFunctions_XlsFileImport = class(TfsRTTIModule)
  private
    function Call_TSBaseXlsFileImportProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

  TfsFunctions_FlexCelImport = class(TfsRTTIModule)
  private
    function Call_TFlexCelImport_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Get_TFlexCelImport_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TFlexCelImport_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
  public
    constructor Create(AScript: TfsScript); override;
  end;

implementation

uses SBaseConstants, SBaseFileUtils;

{ TSBaseXlsFileImportProvider }

class function TSBaseXlsFileImportProvider.Code: String;
begin
  Result := SFileExt_Xls
end;

constructor TSBaseXlsFileImportProvider.Create(AOwner: TSBaseImport);
begin
  FAdapter := TXLSXAdapter.Create(nil);
  FAdapter.AllowOverwritingFiles := True;

  FFile := TFlexCelImport.Create(nil);
  FFile.Adapter := FAdapter;

  inherited;
end;

destructor TSBaseXlsFileImportProvider.Destroy;
begin
  FreeAndNil(FFile);
  FreeAndNil(FAdapter);
  inherited;
end;

procedure TSBaseXlsFileImportProvider.Open(const AFileName: String);
begin
  try
    FFile.OpenFile(AFileName);
  except on E:EFlexCelXlsAdapterException do begin
    FTempFile := ExcelFileReSave(AFileName);
    FFile.OpenFile(FTempFile);
  end end;
  FRecordNo := 1;
end;

function TSBaseXlsFileImportProvider.FindValueByName(const AName: String): Integer;
begin
  for Result := 1 to FFile.MaxCol do
    if FFile.CellValue[FRecordNo, Result] = AName then
      Exit;

  Result := -1;
end;

function TSBaseXlsFileImportProvider.FindValueByRegExpr(const ARegExpr: String): Integer;
begin
  for Result := 0 to FFile.MaxCol do
    if TRegEx.IsMatch(VarToStr(FFile.CellValue[FRecordNo, Result]), ARegExpr, []) then
      Exit;

  Result := -1;
end;

class function TSBaseXlsFileImportProvider.FastScriptClass: TfsRTTIModuleClass;
begin
  Result := TfsFunctions_XlsFileImport;
end;

procedure TSBaseXlsFileImportProvider.Close;
begin
  FFile.CloseFile;
  if not FTempFile.IsEmpty then begin
    DeleteFile(FTempFile);
    FTempFile := '';
  end;
end;

function TSBaseXlsFileImportProvider.FileMode: TSBaseImportFileMode;
begin
  Result := bifmFile
end;

procedure TSBaseXlsFileImportProvider.InternalExecute;
begin
  inherited;
  Owner.ScriptExecute;
end;

function TSBaseXlsFileImportProvider.GetEOF: Boolean;
begin
  Result := (FRecordNo > FFile.MaxRow);
end;

function TSBaseXlsFileImportProvider.GetLineValues(AIndex: Integer): Variant;
begin
  if (AIndex <= 0) or (AIndex > FFile.MaxCol) then
    Result := Null
  else
    Result := FFile.CellValue[FRecordNo, AIndex]
end;

function TSBaseXlsFileImportProvider.GetSheetCount: Integer;
begin
  Result := FFile.SheetCount
end;

function TSBaseXlsFileImportProvider.GetActiveSheetNo: Integer;
begin
  Result := FFile.ActiveSheet
end;

procedure TSBaseXlsFileImportProvider.SetActiveSheetNo(const Value: Integer);
begin
  FFile.ActiveSheet := Value;
end;

procedure TSBaseXlsFileImportProvider.Next;
begin
  Inc(FRecordNo);
end;

function TSBaseXlsFileImportProvider.RecordNo: Integer;
begin
  Result := FRecordNo;
end;

function TSBaseXlsFileImportProvider.GetActiveSheetName: String;
begin
  Result := FFile.ActiveSheetName
end;

procedure TSBaseXlsFileImportProvider.SetActiveSheetName(const Value: String);
var
  I: Integer;
begin
  for I := 1 to SheetCount do begin
    FFile.ActiveSheet := I;
    if (FFile.ActiveSheetName = Value) then
      Exit;
  end;
  Raise Exception.Create('SetActiveSheetName command failed: Cannot find sheet named as "' + Value + '"');
end;

{ TfsFunctions_XlsFileImport }

constructor TfsFunctions_XlsFileImport.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseXlsFileImportProvider, TSBaseXlsFileImportProvider.ClassParent.ClassName) do begin
    AddMethod('procedure Next', Call_TSBaseXlsFileImportProvider_Method);
    AddIndexProperty('LineValues', DelphiTypeInteger, DelphiTypeVariant, Call_TSBaseXlsFileImportProvider_Method, True);
    AddMethod('function FindValueByName(const AName: String): Integer', Call_TSBaseXlsFileImportProvider_Method);
    AddMethod('function FindValueByRegExpr(const ARegExpr: String): Integer', Call_TSBaseXlsFileImportProvider_Method);
  end;
end;

function TfsFunctions_XlsFileImport.Call_TSBaseXlsFileImportProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'NEXT' then
    TSBaseXlsFileImportProvider(Instance).Next
  else if MethodName = 'LINEVALUES.GET' then
    Result := TSBaseXlsFileImportProvider(Instance).LineValues[Caller.Params[0]]

  else if MethodName = 'FINDVALUEBYNAME' then
    Result := TSBaseXlsFileImportProvider(Instance).FindValueByName(Caller.Params[0])
  else if MethodName = 'FINDVALUEBYREGEXPR' then
    Result := TSBaseXlsFileImportProvider(Instance).FindValueByRegExpr(Caller.Params[0])
end;

{ TfsFunctions_FlexCelImport }

constructor TfsFunctions_FlexCelImport.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TFlexCelImport, TFlexCelImport.ClassParent.ClassName) do begin
    AddMethod('procedure OpenFile(const FileName: String)', Call_TFlexCelImport_Method);
    AddMethod('procedure CloseFile', Call_TFlexCelImport_Method);

    AddProperty('SheetCount', 'Integer', Get_TFlexCelImport_Property);
    AddProperty('ActiveSheet', 'Integer', Get_TFlexCelImport_Property, Set_TFlexCelImport_Property );
    AddProperty('ActiveSheetName', 'String', Get_TFlexCelImport_Property, Set_TFlexCelImport_Property );
  end;
end;

function TfsFunctions_FlexCelImport.Call_TFlexCelImport_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TFlexCelImport);
{$ENDIF}
  if MethodName = 'OPENFILE' then
    TFlexCelImport(Instance).OpenFile(Caller.Params[0])
  else if MethodName = 'CLOSEFILE' then
  TFlexCelImport(Instance).CloseFile
  else if MethodName = 'SHEETCOUNT' then
    Result := TFlexCelImport(Instance).SheetCount
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TfsFunctions_FlexCelImport.Get_TFlexCelImport_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TFlexCelImport);
{$ENDIF}
  if PropName = 'SHEETCOUNT' then
    Result := TFlexCelImport(Instance).SheetCount
  else if PropName = 'ACTIVESHEET' then
    Result := TFlexCelImport(Instance).ActiveSheet
  else if PropName = 'ACTIVESHEETNAME' then
    Result := TFlexCelImport(Instance).ActiveSheetName
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TfsFunctions_FlexCelImport.Set_TFlexCelImport_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TFlexCelImport);
{$ENDIF}
  if PropName = 'ACTIVESHEET' then
    TFlexCelImport(Instance).ActiveSheet := Integer(Value)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

initialization
  RegisterClass(TSBaseXlsFileImportProvider);
  SBaseImportProperties.RegisterFormat(TSBaseXlsFileImportProvider);

  RegisterClass(TFlexCelImport);
  fsRTTIModules.Add(TfsFunctions_FlexCelImport);
  RegisterClass(TXLSXAdapter);

finalization
  SBaseImportProperties.UnRegisterFormat(TSBaseXlsFileImportProvider);
  UnRegisterClass(TSBaseXlsFileImportProvider);

  fsRTTIModules.Remove(TfsFunctions_FlexCelImport);
  UnRegisterClass(TFlexCelImport);
  UnRegisterClass(TXLSXAdapter);

end.

