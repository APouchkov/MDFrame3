unit SBaseTxtFilesImport;

interface

uses
  Classes, SBaseImport, SBaseTxtFileImport, Variants
  , fs_iinterpreter
  ;

type
  TSBaseTxtFilesImportProvider = class(TSBaseTxtFileImportProvider)
  protected
    FFileIndex: Integer;
    FFiles: TArray<TSBaseFilesImportItem>;

    function FileMode: TSBaseImportFileMode; override;

    function GetEOS: Boolean;
    function GetFileIndex   : Integer;
    function GetFileCode    : String;
    function GetFileMask    : String;
    function GetFileName    : String;
  public
    class function Code: String; override;
    class function FastScriptClass: TfsRTTIModuleClass; override;

    procedure Open(const AFileName: String); override;
    procedure Close; override;
    procedure NextFile;
//    destructor Destroy; override;
  published
    property EOS: Boolean read GetEOS;

    property FileIndex  : Integer read GetFileIndex;
    property FileCode   : String  read GetFileCode;
    property FileMask   : String  read GetFileMask;
    property FileName   : String  read GetFileName;
  end;

  TfsFunctions_TxtFilesImport = class(TfsRTTIModule)
  private
    function Call_TSBaseTxtFilesImportProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

implementation

uses
  SysUtils, SBaseStringFunctions, SBaseConstants, NamedVariables,
  SBaseFileUtils;

{ TSBaseTxtFilesImportProvider }

procedure TSBaseTxtFilesImportProvider.Close;
begin
  inherited;
  SetLength(FFiles, 0);
end;

class function TSBaseTxtFilesImportProvider.Code: String;
begin
  Result := 'TXTs'
end;

class function TSBaseTxtFilesImportProvider.FastScriptClass: TfsRTTIModuleClass;
begin
  Result := TfsFunctions_TxtFilesImport
end;

function TSBaseTxtFilesImportProvider.FileMode: TSBaseImportFileMode;
begin
  Result := bifmDir
end;

function TSBaseTxtFilesImportProvider.GetEOS: Boolean;
begin
  Result := (FFileIndex >= Length(FFiles));
end;

function TSBaseTxtFilesImportProvider.GetFileMask: String;
begin
  Result := FFiles[FFileIndex].Mask
end;

function TSBaseTxtFilesImportProvider.GetFileName: String;
begin
  Result := FFiles[FFileIndex].Name
end;

function TSBaseTxtFilesImportProvider.GetFileCode: String;
begin
  Result := FFiles[FFileIndex].Code
end;

function TSBaseTxtFilesImportProvider.GetFileIndex: Integer;
begin
  Result := Succ(FFileIndex)
end;

procedure TSBaseTxtFilesImportProvider.NextFile;
begin
  if Canceled then Abort;
  if GetEOS then Exit;

  Inc(FFileIndex);
  if not GetEOS then
    inherited Open(GetFileName);
end;

procedure TSBaseTxtFilesImportProvider.Open(const AFileName: String);
var
  LItem: TStringNamedItem;
  LFileCount: Integer;
  LParams: TNamedVariants;
  LStrings: TStrings;
  LFiles: TArray<String>;
  I: Integer;
  LFile: String;
begin
  LFileCount := 0;
  LStrings   := TStringList.Create;

  LParams := Owner.CreateParams;
  LParams.UnknownAsNull := True;

  try
    for LItem in EnumStringNamedItems(Owner.Environment[SConst_FileNames]) do begin
      LFiles := GetFileList(AFileName, FormatString(LItem.Value, LParams, '%'));
      SetLength(FFiles, LFileCount + Length(LFiles));
      for I := 0 to Length(LFiles) - 1 do begin
        with FFiles[LFileCount] do begin
          LFile := LFiles[I];

          Code  := LItem.Name;
          Mask  := LItem.Value;
          Name  := LFile;
        end;
        Inc(LFileCount);
      end;
    end;
  finally
    LStrings.Free
  end;

  FFileIndex := -1;
  NextFile;
end;

{ TfsFunctions_TxtFilesImport }

constructor TfsFunctions_TxtFilesImport.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseTxtFilesImportProvider, TSBaseTxtFilesImportProvider.ClassParent.ClassName) do begin
    AddMethod('procedure NextFile', Call_TSBaseTxtFilesImportProvider_Method);
  end;
end;

function TfsFunctions_TxtFilesImport.Call_TSBaseTxtFilesImportProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'NEXTFILE' then
    TSBaseTxtFilesImportProvider(Instance).NextFile
end;

initialization
  RegisterClass(TSBaseTxtFilesImportProvider);
  SBaseImportProperties.RegisterFormat(TSBaseTxtFilesImportProvider);

finalization
  SBaseImportProperties.UnRegisterFormat(TSBaseTxtFilesImportProvider);
  UnRegisterClass(TSBaseTxtFilesImportProvider);

end.
