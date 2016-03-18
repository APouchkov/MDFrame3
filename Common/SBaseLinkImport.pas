unit SBaseLinkImport;

interface

uses
  Classes, SysUtils, SBaseImport, SBaseDataSetImport, Variants, DB
  , SBaseDataHelperInternal, SBaseDataModule
  , fs_iinterpreter
;

type
  TSBaseLinkImportProvider = class(TSBaseDataSetImportProvider)
  private
    FSQLConnectionProvider: TSQLConnectionProvider;
    function SQLDataSetProvider: TSQLDataSetProvider; inline;
  protected
    procedure InternalExecute; override;
  public
    class function Code: String; override;

    constructor Create(AOwner: TSBaseImport); override;
    destructor Destroy; override;

    procedure Open(const AFileName: String); override;
    procedure Close; override;
    procedure Disconnect; override;
  end;

implementation

uses SBaseConstants, SBaseFileUtils, SBaseStringFunctions,
  SBaseVariantFunctions;

{ TSBaseLinkImportProvider }

class function TSBaseLinkImportProvider.Code: String;
begin
  Result := 'LINK'
end;

constructor TSBaseLinkImportProvider.Create(AOwner: TSBaseImport);
begin
  FSQLConnectionProvider := BaseSQLConnectionProvider.Clone(nil);

  FDataSet := TSQLDataSetProvider.Create(nil);
  with SQLDataSetProvider do begin
    ReadOnly := True;
    CachedUpdates := False;
    //ReadOnly := True;
    SQL.Text := SBaseImportProperties.SQLLinkExec;
    ConnectionProvider  := FSQLConnectionProvider;
  end;

  inherited;
end;

destructor TSBaseLinkImportProvider.Destroy;
begin
  inherited;
  FreeAndNil(FSQLConnectionProvider);
end;

procedure TSBaseLinkImportProvider.Disconnect;
begin
  FSQLConnectionProvider.Connected := False;
  inherited;
end;

procedure TSBaseLinkImportProvider.Open(const AFileName: String);
begin
  FSQLConnectionProvider.Connected := True;

  with SQLDataSetProvider.Params, Self.Owner do begin
    ParamByName(SConst_Id).AsInteger    := Environment[SConst_Id];
    ParamByName(SConst_Params).AsString := CreateParams.GetAsClrTParams(UserParamNames);
  end;
end;

procedure TSBaseLinkImportProvider.InternalExecute;
begin
  SQLDataSetProvider.Open;
  inherited;
end;

function TSBaseLinkImportProvider.SQLDataSetProvider: TSQLDataSetProvider;
begin
  Result := TSQLDataSetProvider(FDataSet);
end;

procedure TSBaseLinkImportProvider.Close;
begin
  inherited;
  FSQLConnectionProvider.Connected := False;
end;

initialization
  RegisterClass(TSBaseLinkImportProvider);
  SBaseImportProperties.RegisterFormat(TSBaseLinkImportProvider);

finalization
  SBaseImportProperties.UnRegisterFormat(TSBaseLinkImportProvider);
  UnRegisterClass(TSBaseLinkImportProvider);

end.

