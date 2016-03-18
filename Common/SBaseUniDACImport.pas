unit SBaseUniDACImport;

interface

uses
  Classes, SysUtils, SBaseImport, SBaseDataSetImport, Variants, DB,
  MemDS, DBAccess, Uni, UniProvider, UniDacVcl, CRAccess, OLEDBAccessUni, MemData, UniConnectForm
  , SBaseDataHelperInternal
  , fs_iinterpreter
;

type
  TSBaseUniDACImportProvider = class(TSBaseDataSetImportProvider)
  private
    FUniConnectionDialog: TInternalConnectDialog;
    FUniConnection: TUniConnection;

    function UniDataSet: TUniQuery; inline;
  protected
    function FileMode: TSBaseImportFileMode; override;
  public
    class function Code: String; override;

    constructor Create(AOwner: TSBaseImport); override;
    destructor Destroy; override;

    procedure Connect(const AConnectionString: String); override;
    procedure Open(const AFileName: String); override;
    procedure Close; override;
    procedure Disconnect; override;
  end;

implementation

uses SBaseConstants, SBaseFileUtils, SBaseStringFunctions,
  SBaseVariantFunctions;

{ TSBaseUniDACImportProvider }

class function TSBaseUniDACImportProvider.Code: String;
begin
  Result := 'UNI'
end;

procedure TSBaseUniDACImportProvider.Connect(const AConnectionString: String);
var
  LConnectionPrompt : Boolean;
  LLoginPrompt      : Boolean;
  LFileMode         : TSBaseImportFileMode;
  LMacro            : TMacro;
  LString           : String;
begin
  inherited;

  FUniConnection.ProviderName := VarToStr(Owner.Environment[SConst_Provider]);
  FUniConnection.Server       := VarToStr(Owner.Environment[SConst_Server]);

  LFileMode := FileMode;
  if LFileMode = bifmNone then begin
    FUniConnection.Database     := VarToStr(Owner.Environment[SConst_DataBase]);
    LConnectionPrompt           := (Owner.Environment[SConst_ConnectionPrompt] = True);
  end else if LFileMode = bifmDir then begin
    FUniConnection.Database     := AConnectionString;
    LConnectionPrompt := False;
  end else begin
    FUniConnection.Database     := ExtractFileDir(AConnectionString);
    LString := ExtractFileName(ExtractShortPathName(AConnectionString));

    LMacro := UniDataSet.Macros.FindMacro(SConst_FileName);
    if Assigned(LMacro) then
      LMacro.Value := QuoteString(LString, '[')
    else begin
      LMacro := UniDataSet.Macros.FindMacro(SConst_File);
      if Assigned(LMacro) then
        LMacro.Value := QuoteString(ChangeFileExt(LString, ''), '[');
    end;
    LConnectionPrompt := False;
  end;

  FUniConnection.Port         := VarToIntDef(Owner.Environment[SConst_Port], 0);

  FUniConnection.Username     := VarToStr(Owner.Environment[SConst_User]);
  FUniConnection.Password     := VarToStr(Owner.Environment[SConst_Password]);

  LLoginPrompt                := (Owner.Environment[SConst_LoginPrompt] = True);
  FUniConnection.LoginPrompt  := (LConnectionPrompt or LLoginPrompt);

  LString := VarToStr(Owner.Environment[SConst_ConnectionString]);
  if not LString.IsEmpty then
    FUniConnection.ConnectString := ConcatIfNotEmpty(FUniConnection.ConnectString, ';', LString);


  if FUniConnection.LoginPrompt and not Assigned(FUniConnectionDialog) then begin
    FUniConnectionDialog := TInternalConnectDialog.Create(nil);
    with FUniConnectionDialog do begin
      DialogClass     := TInternalConnectForm.ClassName;
      StoreLogInfo    := False;
      DisableLoginChange      := (not LLoginPrompt);
      DisableConnectionChange := (not LConnectionPrompt);
    end;
    FUniConnection.ConnectDialog := FUniConnectionDialog;
  end;

  FUniConnection.Options.EnableFMTBCD := True;
  FUniConnection.Connected        := True;
end;

constructor TSBaseUniDACImportProvider.Create(AOwner: TSBaseImport);
begin
  FUniConnection := TUniConnection.Create(nil);

  FDataSet := TUniQuery.Create(nil);
  //FUniDataSet.ReadOnly := True;

  inherited;

  with UniDataSet do begin
    Options.UseMacros := True;
    SQL.Text := Self.Owner.Environment[SConst_CommandText];
    Connection  := FUniConnection;
  end;
end;

destructor TSBaseUniDACImportProvider.Destroy;
begin
  inherited;
  FreeAndNil(FUniConnection);
end;

procedure TSBaseUniDACImportProvider.Disconnect;
begin
  FUniConnection.Disconnect;
  inherited;
end;

procedure TSBaseUniDACImportProvider.Open(const AFileName: String);
var
  I: Integer;
begin
//  try

    for I := 0 to UniDataSet.Params.Count - 1 do
      with UniDataSet.Params[I] do
        Value := Owner.CreateParams[Name];

    UniDataSet.Open;

//  except
//    FUniConnection.Close;
//    Raise;
//  end;
end;

procedure TSBaseUniDACImportProvider.Close;
begin
  inherited;
  FUniConnection.Close;
end;

function TSBaseUniDACImportProvider.FileMode: TSBaseImportFileMode;
var
  LFileSource: String;
begin
  LFileSource := VarToStr(Owner.Environment[SConst_FileSource]);
  if LFileSource.IsEmpty then
    Result := bifmNone
  else if SameText(LFileSource, SConst_File) then
    Result := bifmFile
  else if SameText(LFileSource, SConst_Dir) then
    Result := bifmDir
  else
    Raise Exception.Create('Неверное значение параметра файлового источника ' + SConst_FileSource + ' = '#171 + LFileSource + #187);
end;

function TSBaseUniDACImportProvider.UniDataSet: TUniQuery;
begin
  Result := TUniQuery(FDataSet);
end;

initialization
  RegisterClass(TSBaseUniDACImportProvider);
  SBaseImportProperties.RegisterFormat(TSBaseUniDACImportProvider);

finalization
  SBaseImportProperties.UnRegisterFormat(TSBaseUniDACImportProvider);
  UnRegisterClass(TSBaseUniDACImportProvider);

end.

