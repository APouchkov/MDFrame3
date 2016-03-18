unit SBaseADOFileImport;

interface

uses
  Classes, SysUtils, SBaseFileImport, DB, ADODB
  , fs_iinterpreter
;

type
  TSBaseADOFileImportProvider = class(TSBaseFileImportProvider)
  private
    FADOConnection: TADOConnection;
    FADODataSet: TADODataSet;

    function GetEOF: Boolean;
//    function GetRecNo: Integer;
    function GetDataSet: TDataSet;

    procedure DataSetAfterScroll(DataSet: TDataSet);
//    function GetRecNo: Integer;
  protected
    function RecordNo: Integer; override;
    function RecordCount: Integer; override;
  public
    class function Extension: String; override;
    class function FastScriptClass: TfsRTTIModuleClass; override;

    constructor Create(AOwner: TSBaseFileImport); override;
    destructor Destroy; override;
    procedure FileOpen(const AFileName: String); override;
    procedure Execute; override;
    procedure FileClose; override;
  published
    property DataSet: TDataSet read GetDataSet;
//    property RecNo: Integer read GetRecNo;
    property EOF: Boolean read GetEOF;
  end;

{
  TFunctions_ADOFileImport = class(TfsRTTIModule)
  private
    function Call_TSBaseADOFileImportProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
}

implementation

uses SBaseConstants, SBaseFileUtils, SBaseStringFunctions;

{ TSBaseADOFileImportProvider }

class function TSBaseADOFileImportProvider.Extension: String;
begin
  Result := 'ADO'
end;

constructor TSBaseADOFileImportProvider.Create(AOwner: TSBaseFileImport);
begin
  FADOConnection := TADOConnection.Create(nil);

  FADODataSet := TADODataSet.Create(nil);
  FADODataSet.AfterScroll := DataSetAfterScroll;
  //FADODataSet.ReadOnly := True;

  inherited;

  FADODataSet.CommandText := Owner.Environment[SConst_CommandText];
  FADODataSet.Connection  := FADOConnection;
end;

procedure TSBaseADOFileImportProvider.DataSetAfterScroll(DataSet: TDataSet);
begin
  UpdateProgress;
end;

destructor TSBaseADOFileImportProvider.Destroy;
begin
  FreeAndNil(FADODataSet);
  FreeAndNil(FADOConnection);
  inherited;
end;

procedure TSBaseADOFileImportProvider.FileOpen(const AFileName: String);
var
  I: Integer;
begin
  FileClose;

  FADOConnection.ConnectionString := Owner.Environment[SConst_ConnectionString] + QuoteString(AFileName, '"');
  FADOConnection.LoginPrompt      := (Owner.Environment[SConst_LoginPrompt] = True);
  FADOConnection.Connected        := True;

  try
    FADODataSet.Parameters.Refresh;
    for I := 0 to FADODataSet.Parameters.Count - 1 do
      with FADODataSet.Parameters[I] do
        Value := Owner.CreateParams[TrimLeft(Name, '@')];

    FADODataSet.Open;
  except
    FADOConnection.Close;
    Raise;
  end;
end;

class function TSBaseADOFileImportProvider.FastScriptClass: TfsRTTIModuleClass;
begin
  Result := nil;
end;

procedure TSBaseADOFileImportProvider.FileClose;
begin
  FADODataSet.Close;
  FADOConnection.Close;
end;

procedure TSBaseADOFileImportProvider.Execute;
begin
  inherited;
  Owner.ScriptExecute;
end;

function TSBaseADOFileImportProvider.GetDataSet: TDataSet;
begin
  Result := FADODataSet
end;

function TSBaseADOFileImportProvider.GetEOF: Boolean;
begin
  Result := FADODataSet.Eof;
end;

(*
function TSBaseADOFileImportProvider.GetRecNo: Integer;
begin
  Result := FADODataSet.RecNo;
end;
*)

function TSBaseADOFileImportProvider.RecordCount: Integer;
begin
  Result := FADODataSet.RecordCount
end;

function TSBaseADOFileImportProvider.RecordNo: Integer;
begin
  Result := FADODataSet.RecNo
end;

initialization
  RegisterClass(TSBaseADOFileImportProvider);
  SBaseFileImportProperties.RegisterFormat(TSBaseADOFileImportProvider);

finalization
  SBaseFileImportProperties.UnRegisterFormat(TSBaseADOFileImportProvider);
  UnRegisterClass(TSBaseADOFileImportProvider);

end.

