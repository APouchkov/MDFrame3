unit SBaseXmlFileImport;

interface

uses
  Classes, SysUtils, SBaseImport, NamedVariables, DB, Types,
  SBaseStringFunctions, SBaseUtils, SBaseXml
  , fs_iinterpreter
;

type
  TSBaseXmlFileImportProvider = class;

  TSBaseXmlFileImportProvider = class(TSBaseImportProvider)
  private
    FEncoding: TEncoding;
    FStream: TStream;

//    FRecordNo: Integer;
    FValues: TNamedVariants;
    function GetAttribute(const AName: String): String;
    function GetEof: Boolean;
    function GetNodeType: TXmlNodeType;
//    FPostDataSet     : TDataSet;
  protected
    FFile: TXmlTextReader;

    function FileMode: TSBaseImportFileMode; override;
//    function RecordNo: Integer; override;

    procedure InternalExecute; override;
    procedure InternalOpen(const AFileName: String); dynamic;
    procedure InternalClose;
  public
    class function Code: String; override;
    class function FastScriptClass: TfsRTTIModuleClass; override;

    constructor Create(AOwner: TSBaseImport); override;
    destructor Destroy; override;

    procedure Open(const AFileName: String); override;
    procedure Close; override;

    function ReadNode: Boolean;
    property NodeType: TXmlNodeType read GetNodeType;

    function  ElementName: String;
    procedure BeginElementText;
    function  ElementText: String;

    function  ReadElement(AElementText: TXmlElementText = xetSkip): String;
    function  FindElement(const ATagNames: String): Boolean;

    property  Attributes[const AName: String]: String read GetAttribute;

    procedure FillValues(const ARequiredNames: String; const APossibleNames: String);
    property  Values: TNamedVariants read FValues write FValues;
  published
    property EOF: Boolean read GetEof;
  end;

  TfsFunctions_XmlFileImport = class(TfsRTTIModule)
  private
    function Get_TSBaseXmlFileImportProvider_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function Call_TSBaseXmlFileImportProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

implementation

uses
  SBaseConstants, SBaseFileUtils, Variants, SBaseDataModule, IOUtils,
  SBaseVariantFunctions;

{ TSBaseXmlFileImportProvider }

class function TSBaseXmlFileImportProvider.Code: String;
begin
  Result := SFileExt_Xml
end;

constructor TSBaseXmlFileImportProvider.Create(AOwner: TSBaseImport);
var
  LIdx: Integer;
begin
  if AOwner.Environment.Find(SConst_Encoding, LIdx) then
    FEncoding := TEncoding.GetEncoding(VarToStr(AOwner.Environment.Items[LIdx].Value))
  else
    FEncoding := TEncoding.ANSI;

  inherited;
end;

destructor TSBaseXmlFileImportProvider.Destroy;
begin
  if Assigned(FEncoding) and not TEncoding.IsStandardEncoding(FEncoding) then
    FreeAndNil(FEncoding);

  inherited;
end;

procedure TSBaseXmlFileImportProvider.Open(const AFileName: String);
begin
  InternalOpen(AFileName);
end;

function TSBaseXmlFileImportProvider.ReadNode: Boolean;
begin
  Result := FFile.Read
end;

function TSBaseXmlFileImportProvider.ReadElement(AElementText: TXmlElementText): String;
begin
  Result := FFile.ReadElement(AElementText);
end;

procedure TSBaseXmlFileImportProvider.InternalOpen(const AFileName: String);
begin
  InternalClose;

  FStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  FFile   := TXmlTextReader.Create(FStream);
end;

function TSBaseXmlFileImportProvider.ElementName: String;
begin
  Result := FFile.CurrElementName
end;

function TSBaseXmlFileImportProvider.ElementText: String;
begin
  Result := FFile.CurrElementText
end;

//function TSBaseXmlFileImportProvider.RecordNo: Integer;
//begin
//  Result := FRecordNo;
//end;

class function TSBaseXmlFileImportProvider.FastScriptClass: TfsRTTIModuleClass;
begin
  Result := TfsFunctions_XmlFileImport;
end;

procedure TSBaseXmlFileImportProvider.BeginElementText;
begin
  FFile.BeginElementText
end;

procedure TSBaseXmlFileImportProvider.Close;
begin
  InternalClose;
end;

function TSBaseXmlFileImportProvider.FileMode: TSBaseImportFileMode;
begin
  Result := bifmFile
end;

procedure TSBaseXmlFileImportProvider.FillValues(const ARequiredNames: String; const APossibleNames: String);
var
  LItem: TStringNameItem;
  LIndex: Integer;
begin
  for LItem in EnumStringNameItems(ARequiredNames, ';', True) do begin
    LIndex := FFile.Names.IndexOf(LItem.NameRight);
    if LIndex < 0 then
      Raise Exception.Create('FillValues failed on Attribute '#171 + LItem.NameRight + #187);

    if LItem.CastAs.IsEmpty then
      FValues.SetValue(LItem.NameLeft, FFile.Items[LIndex])
    else
      FValues.SetValue(LItem.NameLeft, CastAs(FFile.Items[LIndex], LItem.CastAs))
  end;

  if not APossibleNames.IsEmpty then
    for LItem in EnumStringNameItems(APossibleNames, ';', True) do begin
      LIndex := FFile.Names.IndexOf(LItem.NameRight);
      if LIndex < 0 then Continue;

      if LItem.CastAs.IsEmpty then
        FValues.SetValue(LItem.NameLeft, FFile.Items[LIndex])
      else
        FValues.SetValue(LItem.NameLeft, CastAs(FFile.Items[LIndex], LItem.CastAs))
    end;
end;

function TSBaseXmlFileImportProvider.FindElement(const ATagNames: String): Boolean;
begin
  Result := FFile.FindElement(ATagNames)
end;

function TSBaseXmlFileImportProvider.GetAttribute(const AName: String): String;
begin
  Result := FFile.Values[AName]
end;

function TSBaseXmlFileImportProvider.GetEof: Boolean;
begin
  Result := FFile.Eof
end;

function TSBaseXmlFileImportProvider.GetNodeType: TXmlNodeType;
begin
  Result := FFile.NodeType
end;

procedure TSBaseXmlFileImportProvider.InternalClose;
begin
  FreeAndNil(FFile);
  FreeAndNil(FStream);
end;

procedure TSBaseXmlFileImportProvider.InternalExecute;
begin
  inherited;
  Owner.ScriptExecute;
end;

{ TfsFunctions_XmlFileImport }

constructor TfsFunctions_XmlFileImport.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseXmlFileImportProvider, TSBaseXmlFileImportProvider.ClassParent.ClassName) do begin
    AddIndexProperty('Attriutes', DelphiTypeString, DelphiTypeString, Call_TSBaseXmlFileImportProvider_Method, True);

    AddProperty('Values', TNamedVariants.TypeName, Get_TSBaseXmlFileImportProvider_Property);
    AddProperty('EOF', DelphiTypeBoolean, Get_TSBaseXmlFileImportProvider_Property);
    AddProperty('NodeType', 'TXmlNodeType', Get_TSBaseXmlFileImportProvider_Property);
    AddProperty('ElementName', DelphiTypeString, Get_TSBaseXmlFileImportProvider_Property);
    AddProperty('ElementText', DelphiTypeString, Get_TSBaseXmlFileImportProvider_Property);

    AddMethod('function ReadNode: Boolean', Call_TSBaseXmlFileImportProvider_Method);
    AddMethod('function ReadElement(AElementText: TXmlElementText = xetSkip): String', Call_TSBaseXmlFileImportProvider_Method);
    AddMethod('procedure BeginElementText', Call_TSBaseXmlFileImportProvider_Method);
    AddMethod('function FindElement(const ATagNames: String = ''''): Boolean', Call_TSBaseXmlFileImportProvider_Method);
    AddMethod('procedure FillValues(const ARequiredNames: String; const APossibleNames: String = '''')', Call_TSBaseXmlFileImportProvider_Method);
  end;
end;

function TfsFunctions_XmlFileImport.Get_TSBaseXmlFileImportProvider_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'VALUES' then
    Result := Integer(@TSBaseXmlFileImportProvider(Instance).Values)
  else if PropName = 'EOF' then
    Result := TSBaseXmlFileImportProvider(Instance).EOF
  else if PropName = 'NODETYPE' then
    Result := TSBaseXmlFileImportProvider(Instance).NodeType
  else if PropName = 'ELEMENTNAME' then
    Result := TSBaseXmlFileImportProvider(Instance).ElementName
  else if PropName = 'ELEMENTTEXT' then
    Result := TSBaseXmlFileImportProvider(Instance).ElementText
end;

function TfsFunctions_XmlFileImport.Call_TSBaseXmlFileImportProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'READNODE' then
    Result := TSBaseXmlFileImportProvider(Instance).ReadNode
  else if MethodName = 'READELEMENT' then
    Result := TSBaseXmlFileImportProvider(Instance).ReadElement(Caller.Params[0])
  else if MethodName = 'FINDELEMENT' then
    Result := TSBaseXmlFileImportProvider(Instance).FindElement(VarToStr(Caller.Params[0]))
  else if MethodName = 'FILLVALUES' then
    TSBaseXmlFileImportProvider(Instance).FillValues(Caller.Params[0], VarToStr(Caller.Params[1]))
  else if MethodName = 'BEGINELEMENTTEXT' then
    TSBaseXmlFileImportProvider(Instance).BeginElementText
end;

initialization
  RegisterClass(TSBaseXmlFileImportProvider);
  SBaseImportProperties.RegisterFormat(TSBaseXmlFileImportProvider);

finalization
  SBaseImportProperties.UnRegisterFormat(TSBaseXmlFileImportProvider);
  UnRegisterClass(TSBaseXmlFileImportProvider);

end.

