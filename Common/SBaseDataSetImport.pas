unit SBaseDataSetImport;
{$I config.inc}

interface

uses
  SBaseImport, DB;

type
  TSBaseDataSetImportProvider = class abstract(TSBaseImportProvider)
  private
    function GetEOF: Boolean;
    function GetDataSet: TDataSet;

    procedure DataSetAfterScroll(DataSet: TDataSet);
  protected
    FDataSet: TDataSet;

    function RecordNo: Integer; override;
    function RecordCount: Integer; override;

    procedure InternalExecute; override;
  public
    class function FastScriptClass: TfsRTTIModuleClass; override;

    constructor Create(AOwner: TSBaseImport); override;
//    destructor Destroy; override;

    procedure Close; override;
  published
    property DataSet: TDataSet read GetDataSet;
    property EOF: Boolean read GetEOF;
  end;

implementation

{ TSBaseDataSetImportProvider }

uses SBaseDataModule, NamedVariables, fs_iinterpreter, SBaseUtils;

constructor TSBaseDataSetImportProvider.Create(AOwner: TSBaseImport);
begin
  inherited;
  FDataSet.AfterScroll := DataSetAfterScroll;
end;

procedure TSBaseDataSetImportProvider.DataSetAfterScroll(DataSet: TDataSet);
begin
  UpdateProgress;
end;

procedure TSBaseDataSetImportProvider.InternalExecute;
var
  LTarget: TDataSet;
begin
  inherited;
  if Owner.ScriptExist then
    Owner.ScriptExecute
  else begin
    LTarget := Owner.DataSets[0];
    if not FDataSet.IsEmpty then FDataSet.First;
    while not FDataSet.Eof do begin
      LTarget.Append;
        LTarget.Fields.Load(FDataSet.Fields, lvcIfReceive);
      LTarget.Post;
      FDataSet.Next;
    end;

  end;
end;

class function TSBaseDataSetImportProvider.FastScriptClass: TfsRTTIModuleClass;
begin
  Result := nil;
end;

procedure TSBaseDataSetImportProvider.Close;
begin
  FDataSet.Close;
end;

function TSBaseDataSetImportProvider.GetDataSet: TDataSet;
begin
  Result := FDataSet
end;

function TSBaseDataSetImportProvider.GetEOF: Boolean;
begin
  Result := FDataSet.Eof;
end;

function TSBaseDataSetImportProvider.RecordCount: Integer;
begin
  Result := FDataSet.RecordCount
end;

function TSBaseDataSetImportProvider.RecordNo: Integer;
begin
  Result := FDataSet.RecNo
end;

end.
