unit DataModule;
{$I config.inc}

interface

uses
  SysUtils, Classes, Forms, Dialogs, HSDialogs, IniFiles, IOUtils, SBaseForm,
  SBaseDataModule, DB, fs_iinterpreter, fs_ipascal, NamedVariables, SysConst, SBaseSplashForm,
  RxVerInf, Variants, SynHighlighterPas, SynEditHighlighter, SynHighlighterDfm,
  SynHighlighterXML, SynEditMiscClasses, SynEditSearch;

const
  SConst_ClassVersion   : String = 'ClassVersion';
  SConst_ClassIdentity  : String = 'ClassIdentity';
  SConst_VariableName   : String = 'VariableName';
  SConst_VariableVersion: String = 'VariableVersion';
  SConst_VariableValue  : String = 'VariableValue';
  SConst_VariableType   : String = 'VariableType';

type
  TMainDataModule = class(TDataModule)
    FileOpenDialog: TFileOpenDialog;
    FileSaveDialog: TFileSaveDialog;
    SynDfmSyn: TSynDfmSyn;
    SynPasSyn: TSynPasSyn;
    SynXMLSyn: TSynXMLSyn;
    SynEditSearch: TSynEditSearch;
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
    FApplicationName: String;
//    FSession_Id: Int64;
//    FExportsEnabled: Boolean;
//    FLanguage: String;

    FVersionInfo: TVersionInfo;
    FScript: TfsScript;

    FSQLText_DataModule: String;

    FSQLText_UserVariables_GetByClass_AsRows: String;
    FSQLText_UserVariables_GetAll_AsXML: String;
    FSQLText_UserVariable_Set: String;
    FSQLText_UserVariables_SetAll_FromXML: String;
    FSQLText_UserVariables_Clear: String;

//    FSQLText_LockRecord: String;
    FSQLText_UnLockRecord: String;
    FSQLText_ConnectionTerminate: String;

    FSynDfmCompletions: TStrings;
    FSynPasCompletions: TStrings;

    function GetSQLConnectionProvider: TSQLConnectionProvider;
    function GetSQLConnectionProviderStorage: TSQLConnectionProviderStorage;
  public
    { Public declarations }
    ApplicationDate: TDateTime;
    ExceptedApplicationDate: TDateTime;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function  GetUserClassVariables(const AClassName: String; const AClassVersion: Byte; const AClassIdentity: String): TSBaseFrmUserVariables;
    procedure ClearUserClassVariables(const AClassName: String; const AClassVersion: Variant; const AClassIdentity: Variant);
    procedure SetUserClassVariable(const AClassName: String; const AClassVersion: Byte; const AClassIdentity: String; const AVariableName: String; const AVariableVersion: Byte; AVariableValue: Variant);

    // function LockRecord(const AObject: String; AKey: String; ReRaise: Boolean = False): Boolean;
    procedure UnLockRecord(const Id: Int64);
    procedure ConnectionTerminate(ASQLConnectionProvider: TSQLConnectionProvider);

    function CallPublicFunction(const AFunction: String; AParams: TNamedVariants): Variant; overload;
    function CallPublicFunction(const AFunction: String; AParams: Array of TNamedVariant): Variant; overload;

    function CallPublicFunctionOut(const AFunction: String; var AParams: TNamedVariants): Variant; overload;
  published
    { Published declarations }
    Connection: TCustomConnection;
    SplashForm: TSBaseSplashFrm;

    property SynDfmCompletions: TStrings read FSynDfmCompletions write FSynDfmCompletions;
    property SynPasCompletions: TStrings read FSynPasCompletions write FSynPasCompletions;

    property SQLConnectionProvider: TSQLConnectionProvider read GetSQLConnectionProvider;
    property SQLConnectionProviderStorage: TSQLConnectionProviderStorage read GetSQLConnectionProviderStorage;

    property ApplicationName: String read FApplicationName write FApplicationName;
    property VersionInfo: TVersionInfo read FVersionInfo;

//    property Session_Id: Int64 read FSession_Id write FSession_Id;
//    property ExportsEnabled: Boolean read FExportsEnabled write FExportsEnabled;
//    property Language: String read FLanguage write FLanguage;

    property SQLText_DataModule: String read FSQLText_DataModule write FSQLText_DataModule;

    property SQLText_UserVariables_GetByClass_AsRows: String read FSQLText_UserVariables_GetByClass_AsRows write FSQLText_UserVariables_GetByClass_AsRows;
    property SQLText_UserVariables_GetAll_AsXML: String read FSQLText_UserVariables_GetAll_AsXML write FSQLText_UserVariables_GetAll_AsXML;

    property SQLText_UserVariable_Set: String read FSQLText_UserVariable_Set write FSQLText_UserVariable_Set;
    property SQLText_UserVariables_SetAll_FromXML: String read FSQLText_UserVariables_SetAll_FromXML write FSQLText_UserVariables_SetAll_FromXML;
    property SQLText_UserVariables_Clear: String read FSQLText_UserVariables_Clear write FSQLText_UserVariables_Clear;

//    property SQLText_LockRecord: String read FSQLText_LockRecord write FSQLText_LockRecord;
    property SQLText_UnLockRecord: String read FSQLText_UnLockRecord write FSQLText_UnLockRecord;
    property SQLText_ConnectionTerminate: String read FSQLText_ConnectionTerminate write FSQLText_ConnectionTerminate;
  end;

var
  MainDataModule: TMainDataModule;

implementation

uses
  SBaseConstants, SBaseFileUtils, MainMDIForm, SBaseEvents, SBaseVariantFunctions,
  SBaseDfmView, SBaseUtils, SBaseStringFunctions;

{$R *.dfm}

function TMainDataModule.GetSQLConnectionProvider: TSQLConnectionProvider;
begin
  Result := SBaseDataModule.BaseSQLConnectionProvider;
end;

function TMainDataModule.GetSQLConnectionProviderStorage: TSQLConnectionProviderStorage;
begin
  Result := SBaseDataModule.BaseSQLConnectionProviderStorage;
end;

function TMainDataModule.GetUserClassVariables(const AClassName: String; const AClassVersion: Byte; const AClassIdentity: String): TSBaseFrmUserVariables;
var
  LQuery: TDataSet;
begin
  Result := TSBaseFrmUserVariables.Create(True);

  if not FSQLText_UserVariables_GetByClass_AsRows.IsEmpty then begin
    LQuery := SBaseDataModule.BaseSQLConnectionProvider.OpenSQLQuery
             (
              FSQLText_UserVariables_GetByClass_AsRows,
              TNamedVariants.Create
              (
                [
                  TNamedVariant.Create(SConst_ClassName, AClassName),
                  TNamedVariant.Create(SConst_ClassVersion, AClassVersion),
                  TNamedVariant.Create(SConst_ClassIdentity, AClassIdentity)
                ],
                True
              )
             );
    with LQuery do try
      if Active and (RecordCount > 0) then begin
        SetLength(Result.Variables, RecordCount);
        while not Eof do begin
          Result.Variables[RecNo-1].Name := FieldByName(SConst_Name).AsString;
          Result.Variables[RecNo-1].Version := FieldByName(SConst_Version).AsInteger;

          with FieldByName(SConst_Value) do
            if IsNull then begin
              with FieldByName(SConst_AnsiString) do
                if IsNull then begin
                  with FieldByName(SConst_WideString) do
                    if not IsNull then
                      Result.Variables[RecNo-1].Value := AsString
                end else
                  Result.Variables[RecNo-1].Value := String(AsAnsiString);
            end else
              Result.Variables[RecNo-1].Value := AsVariant;

          Next;
        end;

      end;
    finally
      SBaseDataModule.BaseSQLConnectionProvider.CloseSQLQuery;
    end
  end;
end;

procedure TMainDataModule.ClearUserClassVariables(const AClassName: String; const AClassVersion: Variant; const AClassIdentity: Variant);
begin
  if not FSQLText_UserVariables_Clear.IsEmpty then
    SBaseDataModule.BaseSQLConnectionProvider.ExecSQL
    (
      FSQLText_UserVariables_Clear,
      [
        TNamedVariant.Create(SConst_ClassName, AClassName),
        TNamedVariant.Create(SConst_ClassVersion, AClassVersion),
        TNamedVariant.Create(SConst_ClassIdentity, AClassIdentity)
      ]
    );
end;

procedure TMainDataModule.SetUserClassVariable(const AClassName: String; const AClassVersion: Byte; const AClassIdentity, AVariableName: String; const AVariableVersion: Byte; AVariableValue: Variant);
begin
  if (not FSQLText_UserVariable_Set.IsEmpty) then with SBaseDataModule.BaseSQLConnectionProvider do
    ExecSQL
    (
      FSQLText_UserVariable_Set,
      [
        TNamedVariant.Create(SConst_ClassName, AClassName),
        TNamedVariant.Create(SConst_ClassVersion, AClassVersion),
        TNamedVariant.Create(SConst_ClassIdentity, AClassIdentity),
        TNamedVariant.Create(SConst_VariableName, AVariableName),
        TNamedVariant.Create(SConst_VariableVersion, AVariableVersion),

        TNamedVariant.Create(SConst_VariableValue, VariantToDataBaseString(AVariableValue, True)),
        TNamedVariant.Create(SConst_VariableType, VarTypeToDataBaseType(VarType(AVariableValue)))
      ]
    );
end;

procedure TMainDataModule.UnLockRecord(const Id: Int64);
begin
  try
    BaseSQLConnectionProvider.ExecSQL
    (
      FSQLText_UnLockRecord,
      [
        TNamedVariant.Create(SConst_Id, Id)
      ]
    );
  except on E:Exception do begin
    SBaseMainFormMDIInterface.Log(lgError, E.Message);
  end end;
end;

procedure TMainDataModule.ConnectionTerminate(ASQLConnectionProvider: TSQLConnectionProvider);
begin
  if ASQLConnectionProvider.Connected then
    if (not FSQLText_ConnectionTerminate.IsEmpty) then
      SBaseDataModule.BaseSQLConnectionProvider.ExecSQL(FSQLText_ConnectionTerminate, ASQLConnectionProvider.SessionVariables)
end;

constructor TMainDataModule.Create(AOwner: TComponent);
begin
  inherited;

  FSynDfmCompletions := TStringList.Create;
  FSynPasCompletions := TStringList.Create;
end;

function TMainDataModule.CallPublicFunction(const AFunction: String; AParams: TNamedVariants): Variant;
begin
  Result := TfsProcVariable(FScript.FindLocal(AFunction)).Value
end;

function TMainDataModule.CallPublicFunction(const AFunction: String; AParams: array of TNamedVariant): Variant;
begin
  Result := CallPublicFunction(AFunction, TNamedVariants.Create(AParams, True));
end;

function TMainDataModule.CallPublicFunctionOut(const AFunction: String; var AParams: TNamedVariants): Variant;
var
  LFunction: TfsProcVariable;
  I: Integer;
begin
  LFunction := TfsProcVariable(FScript.FindLocal(AFunction));

  Result := LFunction.Value;

  for I := 0 to LFunction.ParamCount-1 do
    with LFunction.Params[I] do
      AParams[Name] := Value;
end;

procedure TMainDataModule.DataModuleCreate(Sender: TObject);
var
  LIniFile: String;
//  LParam: String;
  LVariables: TNamedVariants;
//  I, J,
  LIdx: Integer;
  LItem: TStringItem;
begin
//  FLanguage := 'RU';

  SBaseForm.SBaseGetUserClassVariablesFunction := GetUserClassVariables;
  SBaseForm.SBaseSetUserClassVariableProcedure := SetUserClassVariable;

//  Session_Id := -1;

  try
    FVersionInfo := TVersionInfo.Create(Application.ExeName);
    SplashForm := TSBaseSplashFrm.Create(Self);
    SplashForm.Name := 'SplashForm';

    ApplicationDate := TFile.GetLastWriteTime(Application.ExeName);
    ExceptedApplicationDate := ApplicationDate;

    LIniFile := ChangeFileExt(ApplicationExeName, '.df');
    LoadDFM(String(SBaseFileUtils.ReadTextFile(LIniFile)));

    LIniFile := ChangeFileExt(ApplicationExeName, '.ds');
    FScript := TfsScript.Create(Self);
    with FScript do begin
      AutoDeclareClasses        := True;

      Parent                    := fsGlobalUnit;
      Parent.AutoDeclareClasses := True;

      AddComponent(Self, SConst_Self);

      Lines.Text := String(SBaseFileUtils.ReadTextFile(LIniFile));
      if not Compile then
        Raise Exception.Create(LIniFile + ': ' + ErrorMsg);
      Execute;
    end;

    with SplashForm do begin
      Show;
      Update;
    end;

    CallPublicFunctionOut('PrepareParams', LVariables);
    LVariables := BaseSQLConnectionProvider.ExecSQL(SQLText_DataModule, LVariables);
    LVariables.ClearUndefined;

    // Чтение DFM датамодуля
    if LVariables.Find(SConst_DFM, LIdx) then begin
      LoadDFM(VarToStr(LVariables.Items[LIdx].Value));
      LVariables.Delete(LIdx);
    end;

    // Чтение регистрации классов проекта
    if LVariables.Find(SConst_Classes, LIdx) then begin
      for LItem in EnumStringItems(LVariables.Items[LIdx].Value, #13, True) do
        TSBaseFrm.RegisterClass(LItem.Value.Trim);

      LVariables.Delete(LIdx);
    end;

    SBaseFormProperties.InterfaceVariables := LVariables;

    MainForm := TMainForm.CreateByParams(Application, TNamedVariants.Create(True));
    MainForm.Name := 'MainForm';
    MainForm.HandleNeeded;
    Application.MainForm := MainForm;
    MainForm.WindowState := wsMaximized;
    MainForm.Show;

  except on E:Exception do begin
    FreeAndNil(FScript);

    if E.Message <> SOperationAborted then
      HSMessageDlg(E.Message, mtError, [mbOK], 0);

    if not Application.Terminated then
      Application.Terminate;
  end end;
end;

destructor TMainDataModule.Destroy;
begin
  FreeAndNil(FVersionInfo);
  FreeAndNil(FSynDfmCompletions);
  FreeAndNil(FSynPasCompletions);

  inherited;
end;

initialization
  RegisterClasses([TApplication, TMainDataModule]);

  IsMultiThread     := True;

  Application.UpdateFormatSettings := False;

  FormatSettings.ThousandSeparator := ' ';
  FormatSettings.DecimalSeparator  := '.';

  FormatSettings.DateSeparator     := '.';
  FormatSettings.ShortDateFormat   := 'dd.mm.yyyy';
  FormatSettings.LongDateFormat    := 'dd.mm.yyyy';

  FormatSettings.TimeSeparator     := ':';
  FormatSettings.ShortTimeFormat   := 'hh:nn:ss';
  FormatSettings.LongTimeFormat    := 'hh:nn:ss';

finalization
  UnRegisterClasses([TApplication, TMainDataModule]);

end.
