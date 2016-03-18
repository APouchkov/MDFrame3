unit SBaseDbfFileImport;

interface
{$I config.inc}

uses
  Classes, SysUtils, SBaseImport, SBaseDataSetImport, DB
{$IFDEF DBF_WITH_HALCYON}
  , Halcn6db
{$ENDIF}
{$IFDEF DBF_WITH_UNIDAC}
  , Uni
  , DBFUniProvider
{$ENDIF}
  , fs_iinterpreter
;

type
  TSBaseDbfFileImportProvider = class(TSBaseDataSetImportProvider)
  private
{$IFDEF DBF_WITH_HALCYON}
    function HalcyonDataSet: THalcyonDataSet; inline;
{$ENDIF}
{$IFDEF DBF_WITH_UNIDAC}
    FPath: TUniConnection;
    function UniDataSet: TUniQuery; inline;
{$ENDIF}
  protected
    function FileMode: TSBaseImportFileMode; override;
  public
    class function Code: String; override;

    constructor Create(AOwner: TSBaseImport); override;
    destructor Destroy; override;

    procedure Open(const AFileName: String); override;
{$IFDEF DBF_WITH_UNIDAC}
    procedure Close; override;
    procedure Disconnect; override;
{$ENDIF}
  end;

implementation

uses SBaseConstants, SBaseFileUtils, SBaseStringFunctions;

{ TSBaseDbfFileImportProvider }

class function TSBaseDbfFileImportProvider.Code: String;
begin
  Result := SFileExt_Dbf
end;

{$IFDEF DBF_WITH_HALCYON}
function TSBaseDbfFileImportProvider.HalcyonDataSet: THalcyonDataSet;
begin
  Result := THalcyonDataSet(FDataSet)
end;
{$ENDIF}

{$IFDEF DBF_WITH_UNIDAC}
function TSBaseDbfFileImportProvider.UniDataSet: TUniQuery;
begin
  Result := TUniQuery(FDataSet)
end;
{$ENDIF}

constructor TSBaseDbfFileImportProvider.Create(AOwner: TSBaseImport);
begin
{$IFDEF DBF_WITH_HALCYON}
  FDataSet := THalcyonDataSet.Create(nil);
  HalcyonDataSet.ReadOnly := True;
{$ENDIF}
{$IFDEF DBF_WITH_UNIDAC}
  FPath := TUniConnection.Create(nil);
  FPath.LoginPrompt := False;
  FPath.ProviderName := TDBFUniProvider.GetProviderName;

  FDataSet := TUniQuery.Create(nil);
  UniDataSet.ReadOnly := True;
//  UniDataSet.UniDirectional := True;
  UniDataSet.Connection := FPath;
{$ENDIF}

  inherited;
end;

destructor TSBaseDbfFileImportProvider.Destroy;
begin
  inherited;
{$IFDEF DBF_WITH_UNIDAC}
  FreeAndNil(FPath);
{$ENDIF}
end;

function TSBaseDbfFileImportProvider.FileMode: TSBaseImportFileMode;
begin
  Result := bifmFile
end;

procedure TSBaseDbfFileImportProvider.Open(const AFileName: String);
begin
{$IFDEF DBF_WITH_HALCYON}
  HalcyonDataSet.DatabaseName := ExtractFileDir(AFileName);
  HalcyonDataSet.TableName := ExtractFileName(AFileName);
  HalcyonDataSet.Open;
{$ENDIF}
{$IFDEF DBF_WITH_UNIDAC}
  FPath.Database := ExtractFilePath(AFileName);
  FPath.SpecificOptions.Text := 'DBF.UseUnicode=' + BoolToStr(SameText(Owner.Environment[SConst_Encoding], SConst_Unicode), True);
  FPath.Open;
  UniDataSet.SQL.Text := Format(SQLQuerySelectFrom, ['*', QuoteString(ExtractFileName(ExtractShortPathName(AFileName)), '[')]);
  UniDataSet.Open;
{$ENDIF}
end;

{$IFDEF DBF_WITH_UNIDAC}
procedure TSBaseDbfFileImportProvider.Close;
begin
  inherited;
  FPath.Close;
end;

procedure TSBaseDbfFileImportProvider.Disconnect;
begin
  FPath.Disconnect;
  inherited;
end;
{$ENDIF}

initialization
  RegisterClass(TSBaseDbfFileImportProvider);
  SBaseImportProperties.RegisterFormat(TSBaseDbfFileImportProvider);

finalization
  SBaseImportProperties.UnRegisterFormat(TSBaseDbfFileImportProvider);
  UnRegisterClass(TSBaseDbfFileImportProvider);

end.

