unit SBaseXmlFilesImport;

interface

uses
  SBaseImport, SBaseXmlFileImport, Variants
  , fs_iinterpreter;

type
  TSBaseXmlFilesImportProvider = class(TSBaseXmlFileImportProvider)
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
  published
    property EOS: Boolean read GetEOS;

    property FileIndex  : Integer read GetFileIndex;
    property FileCode   : String  read GetFileCode;
    property FileMask   : String  read GetFileMask;
    property FileName   : String  read GetFileName;
  end;

  TfsFunctions_XmlFilesImport = class(TfsRTTIModule)
  private
    function Call_TSBaseXmlFilesImportProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

implementation

uses
  Classes, SysUtils, SBaseStringFunctions, SBaseConstants,
  NamedVariables, SBaseFileUtils;

{ TSBaseXmlFilesImportProvider }

class function TSBaseXmlFilesImportProvider.Code: String;
begin
  Result := 'XMLs'
end;

procedure TSBaseXmlFilesImportProvider.Close;
begin
  inherited;
  SetLength(FFiles, 0);
end;

class function TSBaseXmlFilesImportProvider.FastScriptClass: TfsRTTIModuleClass;
begin
  Result := TfsFunctions_XmlFilesImport
end;

function TSBaseXmlFilesImportProvider.FileMode: TSBaseImportFileMode;
begin
  Result := bifmDir
end;

function TSBaseXmlFilesImportProvider.GetEOS: Boolean;
begin
  Result := (FFileIndex >= Length(FFiles));
end;

function TSBaseXmlFilesImportProvider.GetFileCode: String;
begin
  Result := FFiles[FFileIndex].Code
end;

function TSBaseXmlFilesImportProvider.GetFileIndex: Integer;
begin
  Result := Succ(FFileIndex)
end;

function TSBaseXmlFilesImportProvider.GetFileMask: String;
begin
  Result := FFiles[FFileIndex].Mask
end;

function TSBaseXmlFilesImportProvider.GetFileName: String;
begin
  Result := FFiles[FFileIndex].Name
end;

procedure TSBaseXmlFilesImportProvider.NextFile;
begin
  if Canceled then Abort;
  if GetEOS then Exit;

  Inc(FFileIndex);
  if not GetEOS then
    inherited Open(GetFileName);
end;

procedure TSBaseXmlFilesImportProvider.Open(const AFileName: String);
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

{ TfsFunctions_XmlFilesImport }

constructor TfsFunctions_XmlFilesImport.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseXmlFilesImportProvider, TSBaseXmlFilesImportProvider.ClassParent.ClassName) do begin
    AddMethod('procedure NextFile', Call_TSBaseXmlFilesImportProvider_Method);
  end;
end;

function TfsFunctions_XmlFilesImport.Call_TSBaseXmlFilesImportProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'NEXTFILE' then
    TSBaseXmlFilesImportProvider(Instance).NextFile
end;

initialization
  RegisterClass(TSBaseXmlFilesImportProvider);
  SBaseImportProperties.RegisterFormat(TSBaseXmlFilesImportProvider);

finalization
  SBaseImportProperties.UnRegisterFormat(TSBaseXmlFilesImportProvider);
  UnRegisterClass(TSBaseXmlFilesImportProvider);

end.
