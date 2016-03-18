unit SBaseTxtFileImport;
{$I config.inc}

interface

uses
  Classes, SysUtils, Variants, SBaseImport, RegularExpressions, Generics.Collections,
  NamedVariables, SBaseStringFunctions
  , fs_iinterpreter
;

type
  PGroupCollection = ^TGroupCollection;

  TSBaseTxtFileImportProvider = class(TSBaseImportProvider)
  private
    FEncoding: TEncoding;
    FLineValues: TArray<String>;
    FNames: TDictionary<String, TImportValueSource>;
    FValues: TNamedVariants;

    function GetLineValues(AIndex: Integer): String;
    function GetName(Name: String): Integer;
    function GetLineValueCount: Integer; inline;
  protected
    FFile: TStreamReader;
    FRecordNo: Integer;
    FLine: String;

    FEof: Boolean;
    function GetEOF: Boolean;

    function FileMode: TSBaseImportFileMode; override;
    function RecordNo: Integer; override;
//    function RecordCount: Integer; override;

    procedure InternalExecute; override;
    procedure InternalClose;
  public
    class function Code: String; override;
    class function FastScriptClass: TfsRTTIModuleClass; override;

    constructor Create(AOwner: TSBaseImport); override;
    destructor Destroy; override;

    procedure Open(const AFileName: String); override;
    procedure Close; override;

    procedure ReadLine;
    function  FindLineByRegExpr(const ARegExpr: String; AGroups: PGroupCollection = nil): Boolean;
    procedure ParseLine(ADelimeter: Char; ATrimSpaces: Boolean = False);

    function FindValueByName(const AName: String): Integer; inline;
    function FindValueByRegExpr(const ARegExpr: String): Integer;

    procedure FindNames(const ARequiredNames: String; const APossibleNames: String = '');
    procedure FillValues;

    property Names[Name: String]: Integer read GetName;
    property Values: TNamedVariants read FValues write FValues;

    property LineValues[AIndex: Integer]: String read GetLineValues;
  published
//    property RecNo: Integer read FRecordNo write FRecordNo;
    property EOF: Boolean read FEof;

    property Line: String read FLine; //GetLine;
    property LineValueCount: Integer read GetLineValueCount; // write SetLineValueCount;
  end;

  TfsFunctions_TxtFileImport = class(TfsRTTIModule)
  private
    function Get_TSBaseTxtFileImportProvider_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function Call_TSBaseTxtFileImportProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

implementation

uses SBaseConstants, SBaseFileUtils, SBaseVariantFunctions;

{ TSBaseTxtFileImportProvider }

class function TSBaseTxtFileImportProvider.Code: String;
begin
  Result := SFileExt_Txt
end;

constructor TSBaseTxtFileImportProvider.Create(AOwner: TSBaseImport);
var
  LIdx: Integer;
begin
  if AOwner.Environment.Find(SConst_Encoding, LIdx) then
    FEncoding := TEncoding.GetEncoding(VarToStr(AOwner.Environment.Items[LIdx].Value))
  else
    FEncoding := TEncoding.ANSI;

  FValues.UnknownAsNull := True;
  inherited;
end;

destructor TSBaseTxtFileImportProvider.Destroy;
begin
  FreeAndNil(FFile);
  FreeAndNil(FNames);
  if Assigned(FEncoding) and not TEncoding.IsStandardEncoding(FEncoding) then
    FreeAndNil(FEncoding);

  inherited;
end;

procedure TSBaseTxtFileImportProvider.Open(const AFileName: String);
begin
  InternalClose;

  FFile := TStreamReader.Create(AFileName, FEncoding, True, 8192);
  FEof  := False;
  ReadLine;
end;

class function TSBaseTxtFileImportProvider.FastScriptClass: TfsRTTIModuleClass;
begin
  Result := TfsFunctions_TxtFileImport;
end;

procedure TSBaseTxtFileImportProvider.InternalClose;
begin
  FRecordNo := -1;
  FEof      := True;

  FreeAndNil(FFile);

  SetLength(FLineValues, 0);
  if Assigned(FNames) then
    FNames.Clear;
  FValues.Clear;
end;

procedure TSBaseTxtFileImportProvider.Close;
begin
  InternalClose;
end;

function TSBaseTxtFileImportProvider.FileMode: TSBaseImportFileMode;
begin
  Result := bifmFile
end;

procedure TSBaseTxtFileImportProvider.FillValues;
var
  LIndex: Integer;
  LStringValue: String;
  LPair: TPair<String, TImportValueSource>;
begin
  FValues.Clear;
  for LPair in FNames do with LPair do begin
    LIndex := LPair.Value.Index;
    if LIndex < LineValueCount then begin
      LStringValue := FLineValues[LIndex];
      if not LStringValue.IsEmpty then
        if LPair.Value.CastAs.IsEmpty then
          FValues.SetValue(LPair.Key, LStringValue)
        else
          FValues.SetValue(LPair.Key, CastAs(LStringValue, LPair.Value.CastAs))
    end;
  end;
end;

procedure TSBaseTxtFileImportProvider.InternalExecute;
begin
  inherited;
  Owner.ScriptExecute;
end;

function TSBaseTxtFileImportProvider.GetEOF: Boolean;
begin
  Result := (FFile = nil) or FFile.EndOfStream;
//  Result := (FRecordNo >= FFile.Count);
end;

function TSBaseTxtFileImportProvider.GetLineValueCount: Integer;
begin
  Result := Length(FLineValues)
end;

function TSBaseTxtFileImportProvider.GetLineValues(AIndex: Integer): String;
begin
  if (AIndex < 0) or (AIndex >= GetLineValueCount) then
    Result := ''
  else
    Result := FLineValues[AIndex]
end;

function TSBaseTxtFileImportProvider.GetName(Name: String): Integer;
var
  LValue: TImportValueSource;
begin
  if not Assigned(FNames) then
    Exit(-1);
  if FNames.TryGetValue(Name, LValue) then
    Result := LValue.Index
  else
    Result := -1;
end;

procedure TSBaseTxtFileImportProvider.ReadLine;
begin
  if Canceled then Abort;

  if not FEof then begin
    Inc(FRecordNo);
    FEof := GetEOF;

    if FEof then
      FLine := ''
    else
      FLine := FFile.ReadLine;
  end;
end;

procedure TSBaseTxtFileImportProvider.ParseLine(ADelimeter: Char; ATrimSpaces: Boolean);
var
  LLine: String;
  LItem: TStringItem;
  LOLDCount: Integer;
begin
  LLine := Line;
  if ATrimSpaces then
    if (ADelimeter = ' ') then
      Raise Exception.Create('"TrimSpaces" parameter cannot be specified when "Delimeter" = " "')
    else
      LLine := LLine.Trim;

  LOLDCount := LineValueCount;
  for LItem in EnumStringItems(LLine, ADelimeter) do begin
    if LItem.Eof or (LItem.Index >= LOLDCount) then
      SetLength(FLineValues, Succ(LItem.Index));
    FLineValues[LItem.Index] := LItem.Value;
  end;
end;

function TSBaseTxtFileImportProvider.RecordNo: Integer;
begin
  Result := Succ(FRecordNo)
end;

procedure TSBaseTxtFileImportProvider.FindNames(const ARequiredNames: String; const APossibleNames: String);
var
  LItem: TStringNameItem;
  LIndex: Integer;
begin
  if not Assigned(FNames) then
    FNames := TDictionary<String, TImportValueSource>.Create
  else
    FNames.Clear;

  for LItem in EnumStringNameItems(ARequiredNames, ';', True) do begin
    LIndex := FindValueByName(LItem.NameRight);
    if LIndex < 0 then
      raise Exception.Create('FindNames failed on Field '#171 + LItem.NameRight + #187);
    FNames.Add(LItem.NameLeft, TImportValueSource.Create(LIndex, LItem.CastAs))
  end;

  if not APossibleNames.IsEmpty then
    for LItem in EnumStringNameItems(APossibleNames, ';', True) do begin
      LIndex := FindValueByName(LItem.NameRight);
      if LIndex >= 0 then
        FNames.Add(LItem.NameLeft, TImportValueSource.Create(LIndex, LItem.CastAs))
    end;
end;

function TSBaseTxtFileImportProvider.FindValueByName(const AName: String): Integer;
begin
  Result := LineValueCount - 1;
  while (Result >= 0) do
    if FLineValues[Result] = AName then
      Exit
    else
      Dec(Result);
end;

function TSBaseTxtFileImportProvider.FindLineByRegExpr(const ARegExpr: String; AGroups: PGroupCollection): Boolean;
var
  LRegEx: TRegEx;
  LMatch: TMatch;
begin
  Result := False;
  LRegEx := TRegEx.Create(ARegExpr, [roIgnoreCase]);
  while (not FEof) do begin
    LMatch := LRegEx.Match(FLine);
    if LMatch.Success then begin
      if Assigned(AGroups) then
        AGroups^ := LMatch.Groups;
      Exit(True);
    end else
      ReadLine;
  end;
end;

function TSBaseTxtFileImportProvider.FindValueByRegExpr(const ARegExpr: String): Integer;
begin
  for Result := LineValueCount - 1 downto 0 do
    if TRegEx.IsMatch(FLineValues[Result], ARegExpr, []) then
      Exit;

  Result := -1;
end;

//procedure TSBaseTxtFileImportProvider.SetLineValueCount(const Value: Integer);
//begin
//  FLineValueCount := Value;
//end;

{ TfsFunctions_TxtFileImport }

constructor TfsFunctions_TxtFileImport.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseTxtFileImportProvider, TSBaseTxtFileImportProvider.ClassParent.ClassName) do begin
    AddMethod('procedure ParseLine(ADelimeter: String; ATrimSpaces: Boolean = False)', Call_TSBaseTxtFileImportProvider_Method);
    AddMethod('procedure ReadLine', Call_TSBaseTxtFileImportProvider_Method);
    AddMethod('function FindLineByRegExpr(const ARegExpr: String): Boolean', Call_TSBaseTxtFileImportProvider_Method);
    AddMethod('function FindLineByRegExprOut(const ARegExpr: String; var AGroups: Variant): Boolean', Call_TSBaseTxtFileImportProvider_Method);

    AddIndexProperty('LineValues', DelphiTypeInteger, DelphiTypeString, Call_TSBaseTxtFileImportProvider_Method, True);
    AddIndexProperty('Names', DelphiTypeString, DelphiTypeInteger, Call_TSBaseTxtFileImportProvider_Method, True);
    AddProperty('Values', TNamedVariants.TypeName, Get_TSBaseTxtFileImportProvider_Property);

    AddMethod('function FindValueByName(AName: String): Integer', Call_TSBaseTxtFileImportProvider_Method);
    AddMethod('function FindValueByRegExpr(ARegExpr: String): Integer', Call_TSBaseTxtFileImportProvider_Method);
    AddMethod('procedure FindNames(ARequiredNames: String; APossibleNames: String = '''')', Call_TSBaseTxtFileImportProvider_Method);
    AddMethod('procedure FillValues', Call_TSBaseTxtFileImportProvider_Method);
  end;
end;

function TfsFunctions_TxtFileImport.Get_TSBaseTxtFileImportProvider_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'VALUES' then
    Result := Integer(@TSBaseTxtFileImportProvider(Instance).Values)
end;

function TfsFunctions_TxtFileImport.Call_TSBaseTxtFileImportProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  LGroups: TGroupCollection;
  LVGroups: Variant;
  I: Integer;
begin
  if MethodName = 'PARSELINE' then
    TSBaseTxtFileImportProvider(Instance).ParseLine(VarToStr(Caller.Params[0])[1], (Caller.Params[1] = True))
  else if MethodName = 'READLINE' then
    TSBaseTxtFileImportProvider(Instance).ReadLine
  else if MethodName = 'FINDLINEBYREGEXPR' then
    Result := TSBaseTxtFileImportProvider(Instance).FindLineByRegExpr(Caller.Params[0])
  else if MethodName = 'FINDLINEBYREGEXPROUT' then begin
    Result := TSBaseTxtFileImportProvider(Instance).FindLineByRegExpr(Caller.Params[0], @LGroups);
    if Result then begin
      LVGroups := VarArrayCreate([0, LGroups.Count - 1], varVariant);
      for I := 0 to Pred(LGroups.Count) do
        LVGroups[I] := LGroups[I].Value;
      Caller.Params[1] := LVGroups;
    end else
      Caller.Params[1] := unAssigned;
  end

  else if MethodName = 'LINEVALUES.GET' then
    Result := TSBaseTxtFileImportProvider(Instance).LineValues[Caller.Params[0]]

  else if MethodName = 'FINDVALUEBYNAME' then
    Result := TSBaseTxtFileImportProvider(Instance).FindValueByName(Caller.Params[0])
  else if MethodName = 'FINDVALUEBYREGEXPR' then
    Result := TSBaseTxtFileImportProvider(Instance).FindValueByRegExpr(Caller.Params[0])

  else if MethodName = 'FINDNAMES' then
    TSBaseTxtFileImportProvider(Instance).FindNames(Caller.Params[0], VarToStr(Caller.Params[1]))
  else if MethodName = 'FILLVALUES' then
    TSBaseTxtFileImportProvider(Instance).FillValues

  else if MethodName = 'NAMES.GET' then
    Result := TSBaseTxtFileImportProvider(Instance).Names[Caller.Params[0]]
end;

initialization
  RegisterClass(TSBaseTxtFileImportProvider);
  SBaseImportProperties.RegisterFormat(TSBaseTxtFileImportProvider);

finalization
  SBaseImportProperties.UnRegisterFormat(TSBaseTxtFileImportProvider);
  UnRegisterClass(TSBaseTxtFileImportProvider);

end.
