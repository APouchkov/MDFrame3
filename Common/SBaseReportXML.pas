{******************************************************************************}
{                       -= Модуль печати документов =-                         }
{                                                                              }
{                              -= Шаблоны XML =-                               }
{                                                                              }
{                             v4.1 (С) 2007 - 2014                             }
{                                                                              }
{                                                                              }
{******************************************************************************}
unit SBaseReportXML;
{$I config.inc}

interface

uses
  SBaseReportInterface;

{$IFDEF PATTERN_EXTERNAL}
procedure RegisterFactoryPattern(Report: IReport; var FactoryPattern: IFactoryPattern);
{$ENDIF}

implementation

uses
  Classes, DB, SysUtils, Variants, StrUtils,
  {$IFNDEF PATTERN_EXTERNAL}
  SBaseReport,
  {$ENDIF}
  {$IFNDEF REPORT_EXTERNAL}
  Controls, ComObj, Forms,
  {$ENDIF}
  XMLDoc, XMLIntf, NamedVariables, SBaseConstants, SBaseVariantFunctions,
  SBaseFileUtils;

type
  TPatternXML = class;

  TSBaseReportXML = class(TComponent)
  private
    FPattern: TPatternXML;
    FTemplateStream: TStream;
    FVariables: TNamedVariants;
    FStream: TStream;
  protected
    XMLTeamplate: TXMLDocument;
    XMLDocument: TXMLDocument;
    FDataSets: Array of TDataSet;

    function FormatText(const AText: String): String;
    function GetDataSet(const DataSetName: String): TComponent; virtual;
    function GetDataSetFieldByName(const DataSetName, FieldName: String): String;
    function WriteNode(curXMLFrom, startXMLNodeTo: IXMLNode; DataSet: TDataSet = nil): IXMLNode;
    procedure PrepareNode(XMLFrom, XMLTo: IXMLNode; DataSet: TDataSet = nil);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run(ADataSets: Array of TDataSet);

    property Variables: TNamedVariants read FVariables write FVariables;
  published
//    property Template: TFileName read FTemplate write FTemplate;
    property TemplateStream: TStream read FTemplateStream write FTemplateStream;
//    property FileName: TFileName read FFileName write FFileName;
    property Stream: TStream read FStream write FStream;
  end;

  TPatternXML = class(TCustomPattern)
  private
    FReport: TSBaseReportXML;
  protected
    class function GetCode: String; override;
  public
    class function ScriptSupport: Boolean; override;

    constructor Create(Report: IReport); override;
    destructor Destroy; override;

    function ResultFileExt: String; override;
    function GetVariables(Name: String): Variant; override;
    procedure PrepareReport(var APatternStorage: TPatternStorage; const ADataSets: Array of TDataSet; const AVariables: TNamedVariants); override;

    procedure SaveReport(AStream: TStream); override;
    procedure ShowReport(Owner: TForm; Caption: String; ShowModal: Boolean); override;
  end;

  TFactoryPatterns = class(TCustomFactoryPatterns)
  protected
    function GetPatternClass(Index: Integer): TCustomPatternClass; override;
  end;

const
  SPatternCode          = 'XML';
  SPatternResultFileExt = 'xml';

resourcestring
  SSpecCharacter  = '#';
  SSpecDataSet    = 'DATASET';
  SSpecVisible    = 'VISIBLE';

  SVariable       = '#.';
  SDataSet_Field  = '##';
  SFunction       = '#@';

  SMulti_Begin    = '[#';
  SMulti_End      = ']';

{ TSBaseReportXML }
constructor TSBaseReportXML.Create(AOwner: TComponent);
begin
  inherited;
  FVariables.UnknownAsNull := True;
  XMLTeamplate := TXMLDocument.Create(Self);
  XMLDocument  := TXMLDocument.Create(Self);
end;

destructor TSBaseReportXML.Destroy;
begin
  if Assigned(XMLTeamplate) then XMLTeamplate.Destroy;
  if Assigned(XMLDocument)  then XMLDocument.Destroy;
  inherited;
end;

function TSBaseReportXML.GetDataSet(const DataSetName: String): TComponent;
begin
  Result := FPattern.FindDataSetByName(DataSetName);
end;

function TSBaseReportXML.GetDataSetFieldByName(const DataSetName, FieldName: String): String;
var
  AField: TField;
  ADataSet: TDataSet;
begin
  Result := SDataSet_Field + DataSetName + SDataSet_Field + FieldName;
  ADataSet := GetDataSet(DataSetName) as TDataSet;
  if ADataSet <> nil then begin
    AField := ADataSet.FindField(FieldName);
    if AField <> nil then
      Result := AField.DisplayText
    else if SameText(FieldName, SConst_RecNo) then
      Result := IntToStr(ADataSet.RecNo)
    else if SameText(FieldName, SConst_RecordCount) then
      Result := IntToStr(ADataSet.RecordCount);
  end;
end;

function TSBaseReportXML.FormatText(const AText: String): String;
type
  TReportValueType = (rvtText, rvtVariable, rvtDataSetField, rvtFunction);
  TReportValueInfo = record
    ValueType: TReportValueType;
    Name: String;
    Param: String;
    DisplayText: String;
  end;

  function GetValueInfo(const AValue: String): TReportValueInfo;
    procedure ExtractDisplayText(var AText: String);
    var
      LPos: Integer;
    begin
      LPos := PosEx(SDataSet_Field, AText, 4);
      if LPos = 0 then
        Result.DisplayText := ''
      else begin
        Result.DisplayText  := Copy(AText, LPos + 2, AText.Length);
        AText               := LeftStr(AText, LPos - 1);
      end;
    end;

  var
    LPrefix: String;
    LIndex, LIndex2: Integer;
  begin
    LPrefix := LeftStr(AValue, 2);
    if LPrefix = SVariable then begin
      Result.ValueType  := rvtVariable;
      Result.Name       := Copy(AValue, 3, AValue.Length);
    end else if LPrefix = SDataSet_Field then begin
      LIndex := PosEx(SDataSet_Field, AValue, 4);
      if LIndex = 0 then begin
        Result.ValueType := rvtText;
        Exit;
      end;

      Result.ValueType  := rvtDataSetField;
      Result.Name       := Copy(AValue, 3, LIndex - 3);
      Result.Param      := Copy(AValue, LIndex + 2, AValue.Length);
      ExtractDisplayText(Result.Param);
    end else if LPrefix = SFunction then begin
      Result.ValueType := rvtFunction;
      LIndex := PosEx('(', AValue, 4);
      if LIndex = 0 then begin
        Result.Name         := Copy(AValue, 3, AValue.Length);
        Result.Param        := '';
        Result.DisplayText  := '';
      end else begin
        LIndex2 := PosEx(')' + SDataSet_Field, AValue, 4);
        if (LIndex2 = 0) and (AValue[AValue.Length] = ')') then
          LIndex2 := AValue.Length;

        if LIndex2 = 0 then
          Result.ValueType  := rvtText
        else begin
          Result.Name         := Copy(AValue, 3, LIndex - 3);
          Result.Param        := Copy(AValue, LIndex + 1, LIndex2 - LIndex - 1);
          Result.DisplayText  := Copy(AValue, LIndex2 + 1, AValue.Length);
        end;
      end;

    end else begin
      Result.ValueType := rvtText;
    end;
  end;

  function GetSubValue(const ASubVarName: String): String;
  var
    LValueInfo: TReportValueInfo;
  begin
    LValueInfo := GetValueInfo(ASubVarName);
    if LValueInfo.ValueType = rvtText then
      Result := ASubVarName
    else begin
      if LValueInfo.ValueType = rvtVariable then
        Result := VarToStr(Variables[LValueInfo.Name])
      else if LValueInfo.ValueType = rvtDataSetField then
        Result := GetDataSetFieldByName(LValueInfo.Name, LValueInfo.Param)
      else if LValueInfo.ValueType = rvtFunction then
        Result := VarToStr(FPattern.FScript.CallFunction(LValueInfo.Name, LValueInfo.Param))
    end;
  end;

var
  LPos, LBegin, LEnd: Integer;
begin
  LBegin := PosEx(SMulti_Begin, AText, 1) + 1;
  if LBegin = 1 then
    Result := GetSubValue(AText)
  else begin
    LPos   := 1;
    Result := '';
    repeat
      Result := Result + Copy(AText, LPos, LBegin - LPos - 1);
      LEnd := PosEx(SMulti_End, AText, LBegin + 2) + 1;
      if LEnd = 1 then
        Break
      else begin
        Result := Result + GetSubValue(Copy(AText, LBegin, LEnd - LBegin - 1));
        LPos := LEnd;
      end;

      LBegin := PosEx(SMulti_Begin, AText, LPos) + 1;
    until (LBegin = 1);

    Result := Result + Copy(AText, LPos, AText.Length);
  end;
end;

procedure TSBaseReportXML.PrepareNode(XMLFrom, XMLTo: IXMLNode; DataSet: TDataSet = nil);
var
  I: Integer;
  LNodeName, LNodeValue: String;
  LXMLNode: IXMLNode;
begin
  for I := 0 to Pred(XMLFrom.AttributeNodes.Count) do begin
    LXMLNode := XMLFrom.AttributeNodes.Nodes[I];
    with LXMLNode do begin
      LNodeName := NodeName;
      if (Pos(':', LNodeName) = 0)
        and not SameText(LNodeName, SSpecDataSet)
        and not SameText(LNodeName, SSpecVisible)
      then begin
        LNodeValue := VarToStr(NodeValue);
        if (Pos(SDataSet_Field, LNodeValue) > 0) or (Pos(SVariable, LNodeValue) > 0) then
          XMLTo.Attributes[LNodeName] := FormatText(LNodeValue)
        else
          XMLTo.Attributes[LNodeName] := NodeValue;
      end;
    end;
  end;
end;

function TSBaseReportXML.WriteNode(curXMLFrom, startXMLNodeTo: IXMLNode; DataSet: TDataSet = nil): IXMLNode;
var
  curXMLTo: IXMLNode;
begin
  curXMLTo := startXMLNodeTo.AddChild(curXMLFrom.NodeName);
  if curXMLFrom.IsTextElement then begin
    if (Pos(SSpecCharacter, curXMLFrom.Text) > 0) then
      curXMLTo.NodeValue := FormatText(curXMLFrom.Text)
    else
      curXMLTo.NodeValue := curXMLFrom.NodeValue;
  end;
  PrepareNode(curXMLFrom, curXMLTo, DataSet);
  Result := curXMLTo;
end;

procedure TSBaseReportXML.Run(ADataSets: Array of TDataSet);

  procedure DoReport(XMLFrom, XMLNodeTo: IXMLNode);
  var
    DataSet: TDataSet;
    I, ARecNo, AIndex: Integer;
    curXMLFrom, curXMLTo: IXMLNode;
    AValue, ADatasetName: String;
  begin
    for I := 0 to XMLFrom.ChildNodes.Count - 1 do
    begin
      curXMLFrom := XMLFrom.ChildNodes[I];
      if curXMLFrom.NodeType = ntElement then begin
        // Visible
        if (curXMLFrom.Attributes[SSpecVisible] <> Null) and (curXMLFrom.Attributes[SSpecVisible] <> '') then begin
           AValue := FormatText(curXMLFrom.Attributes[SSpecVisible]);
           AIndex := Pos('=', AValue);
           if AIndex > 0 then
              AValue := BoolToStr(SameText(FormatText(Copy(AValue, 1, Pred(AIndex))), FormatText(Copy(AValue, Succ(AIndex), MaxInt))), True);
           if (AValue <> 'True') and (AValue <> '1') then continue;
           curXMLTo := WriteNode(curXMLFrom, XMLNodeTo);
           DoReport(curXMLFrom, curXMLTo);
        end else
        // DataSet
        if (curXMLFrom.Attributes[SSpecDataSet] <> Null) and (curXMLFrom.Attributes[SSpecDataSet] <> '') then begin
          ADatasetName := curXMLFrom.Attributes[SSpecDataSet];
          DataSet := GetDataSet(ADatasetName) as TDataSet;
          if Assigned(DataSet) and not DataSet.IsEmpty then begin
            ARecNo := DataSet.RecNo;
            DataSet.DisableControls;
            try
              DataSet.First;
              while not DataSet.Eof do begin
                curXMLTo := WriteNode(curXMLFrom, XMLNodeTo, DataSet);
                DoReport(curXMLFrom, curXMLTo);
                DataSet.Next;
              end;
            finally
              DataSet.RecNo := ARecNo;
              DataSet.EnableControls;
            end;
          end;
        end else begin
          curXMLTo := WriteNode(curXMLFrom, XMLNodeTo);
          DoReport(curXMLFrom, curXMLTo);
        end;
      end;
    end;
  end;

var
  I: Integer;
begin
  SetLength(FDataSets, Length(ADataSets));
  for I := 0 to Pred(Length(ADataSets)) do FDataSets[I] := ADataSets[I];

  XMLTeamplate.LoadFromStream(FTemplateStream);
  XMLDocument.Active := True;
  XMLDocument.DocumentElement := XMLTeamplate.DocumentElement.CloneNode(False);

  PrepareNode(XMLTeamplate.DocumentElement, XMLDocument.DocumentElement);
  DoReport(XMLTeamplate.DocumentElement, XMLDocument.DocumentElement);

  XMLDocument.Encoding := XMLTeamplate.Encoding;
  XMLDocument.SaveToStream(Stream);
end;

{ TPatternXML }
constructor TPatternXML.Create(Report: IReport);
begin
  inherited;
  FReport := TSBaseReportXML.Create(nil);
  FReport.FPattern := Self;
end;

destructor TPatternXML.Destroy;
begin
  FReport.Free;
  inherited;
end;

class function TPatternXML.GetCode: String;
begin
  Result := SPatternCode;
end;

function TPatternXML.GetVariables(Name: String): Variant;
begin
  Result := FReport.Variables[Name]
end;

function TPatternXML.ResultFileExt: String;
begin
  Result := SPatternResultFileExt;
end;

procedure TPatternXML.PrepareReport(var APatternStorage: TPatternStorage; const ADataSets: Array of TDataSet; const AVariables: TNamedVariants);
begin
  inherited;

  with FReport do begin
    FTemplateStream := APatternStorage.PatternStream;
    Variables := AVariables;
  end;
end;

procedure TPatternXML.SaveReport(AStream: TStream);
begin
  {$IFNDEF REPORT_EXTERNAL}
  Screen.Cursor := crHourglass;
  DisableDataControls;
  try
  {$ENDIF}
    FReport.Stream := AStream;
    FReport.Run([]);
  {$IFNDEF REPORT_EXTERNAL}
  finally
    EnableDataControls;
    Screen.Cursor := crDefault;
  end;
  {$ENDIF}
end;

procedure TPatternXML.ShowReport(Owner: TForm; Caption: String; ShowModal: Boolean);
var
  LFileName: String;
begin
  LFileName := TSBaseReport.CreateTempFileName(Caption, ResultFileExt);
  SaveReport(LFileName);
  if Terminated then Exit;
  OpenFileWithShell(LFileName);
end;

class function TPatternXML.ScriptSupport: Boolean;
begin
  Result := True;
end;

{ TFactoryPatterns }
function TFactoryPatterns.GetPatternClass(Index: Integer): TCustomPatternClass;
begin
  Result :=  TPatternXML;
end;

{$IFNDEF PATTERN_EXTERNAL}
initialization
  RegisterFactoryPatterns(TFactoryPatterns.Create as IFactoryPatterns);
{$ENDIF}

end.
