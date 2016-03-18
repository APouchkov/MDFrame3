unit SBaseXml;
{$I config.inc}

interface

uses
  Classes, SysUtils,
  Generics.Collections,
  CLRClasses, CRTypes
{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter
{$ENDIF}
  ;

type
  TXmlException = class(Exception);

  TXmlNodeType    = (xntNone, xntElement, xntStartElement, xntEndElement, xntComment, xntDeclaration, xntDocumentType);
  TXmlReadState   = (xrsInitial, xrsInteractive, xrsError, xrsEndOfFile{, xrsClosed});
  TXmlElementText = (xetSkip, xetLTrim, xetRTrim, xetTrim);

  TXmlTextReader = class
  strict private
    FElementNameStack: TStack<String>;
    FElementTextStack: TStack<PString>;
  private
    FBlocks: TStringList;
    FStream: TStream;
    FTmpBlock: TBytes;
    FStreamPosition: int64;// When this class created through constructor Create(str: string), FStreamPosition = FFullSize
    FBlockSize: integer;
    FFullSize: int64;
    FMaxNumBlock: integer;
    FCurPosition: integer;
    FActualPosition: integer;
    FLastBlockSize: integer;
    FPrefix: string;
    FNodeType: TXmlNodeType;

    FName: string;
    FValue: string;

    FAttrNames: TStringList;
    FAttrPrefix: TStringList;
    FAttrValues: TStringList;

    FState: TXmlReadState;

    FCurrElementName: String;
    FCurrElementText: PString;

    function GetHasAttributes: Boolean;
    function GetDepth: integer; inline;
    function GetAttributeCount: integer;
    procedure GetXMLNodeAttributes(const Node: string; AttrNames, AttrValues: TStrings);
    procedure InitInstance;
    function GetEof: Boolean;
    function LoadNextBlock(Count: integer): boolean;
    procedure FreeLastBlocks(Count: integer);
    function ReadTo(const SubStr: String; out ResultStr: string; const AdvLenth: integer = 0): Boolean; overload;
    function IsToken(const SubStr: String): Boolean;
    function GetNextSymbol: char;
    function MoveTo(const Lexem: String): Boolean;
    function GetCurrElementText: String;
  public
    constructor Create(Stream: TStream); overload;
    constructor Create(const Str: String {UTF8}); overload; // Parameter "Str" mast content only XML Text, Url for XML not supported
    destructor Destroy; override;

    procedure MoveToAttribute(I: integer); overload;
    function  MoveToAttribute(const name: String): Boolean; overload;

    function  Read: Boolean;

    procedure BeginElementText;
    function ReadElement(AElementText: TXmlElementText = xetSkip): String;
    function  FindElement(const ATagNames: String): Boolean;

    function GetValue(const AttrName: String): String;
    function TryGetValue(const AttrName: String; out AValue: String): Boolean;

    property CurrElementName: String read FCurrElementName;
    property CurrElementText: String read GetCurrElementText;

    property NodeType: TXmlNodeType read FNodeType;
    property Name: String read FName;
    property Prefix: String read FPrefix;
    property Value: String read FValue;

    property Names: TStringList read FAttrNames;
    property Items: TStringList read FAttrValues;
    property Values[const Name: String]: String read GetValue; default;

    property AttributeCount: integer read GetAttributeCount;
    property Depth: integer read GetDepth;
    property ReadState: TXmlReadState read FState;
    property Eof: Boolean read GetEof;
    property HasAttributes: Boolean read GetHasAttributes;
  end;

function XMLEncode(const AStr: WideString): WideString;
function XMLDecode(const AStr: string): string;

{$IFDEF FASTSCRIPT_RTTI}
type
  TfsFunctions_Xml = class(TfsRTTIModule)
  private
//    function Call_String_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

implementation

uses
  Math,
  CRFunctions, CRParser, MemUtils, SBaseStringFunctions, StrUtils,
  SBaseDataModule;

const
  XMLLineSeparator = #13#10;
  SInvalidXML = 'Invalid XML';

type
  TXmlParser = class(TParser)
  public
    constructor Create(const Text: string); override;
    function GetNextIdent(out Lexem: string): integer;
  end;

var
  XmlSymbolLexems, XmlKeywordLexems: TIntValueStringList;
  XmlSymbolChars: TCharSet;

{ TXmlTextReader }

function TXmlTextReader.LoadNextBlock(Count: integer): boolean;
var
  Str: string;
begin
  Result := False;
  if FStream = nil then
    Exit;

  if FBlocks.Count >= Count then begin
    Result := True;
    Exit;
  end;

  FLastBlockSize := FStream.Read(FTmpBlock[0], FBlockSize);
  FStreamPosition := FStream.Position;
  Result := FLastBlockSize <> 0;
  if Result then begin
    Str := Encoding.Default.GetString(FTmpBlock, 0, FLastBlockSize);
    FBlocks.Add(Str);
    FCurPosition := 1;
  end;
end;

procedure TXmlTextReader.FreeLastBlocks(Count: integer);
begin
  while FBlocks.Count > Count do
    FBlocks.Delete(0);
end;

function TXmlTextReader.GetEof: Boolean;
begin
  Result := (((FStreamPosition - FLastBlockSize) + FActualPosition) > FFullSize) or
            ((FCurPosition - 1) > FLastBlockSize);
end;

function TXmlTextReader.IsToken(const SubStr: string): boolean;
var
  i, BlockPos, BlockNo: integer;
begin
  BlockPos := FActualPosition;
  BlockNo := 0;

  for i := 1 to Length(SubStr) do begin
    if BlockPos > Length(FBlocks[BlockNo]) then begin
      Inc(BlockNo);
      if not LoadNextBlock(BlockNo + 1) then begin
        Result := False;
        Exit;
      end;
      BlockPos := 1;
    end;

    if FBlocks[BlockNo][BlockPos] <> SubStr[i] then begin
      Result := False;
      Exit;
    end;
    Inc(BlockPos);
  end;

  FActualPosition := BlockPos;
  FCurPosition := BlockPos;

  FreeLastBlocks(1);
  if (FActualPosition > Length(FBlocks[0])) and (FStream <> nil) then begin
    FreeLastBlocks(0);
    LoadNextBlock(1);
    FActualPosition := 1;
  end;

  Result := True;
end;

function TXmlTextReader.MoveTo(const Lexem: string): boolean;
begin
  Result := False;
  while (FActualPosition <= FFullSize) and not Eof do begin
    if IsToken(Lexem) then begin
      Result := True;
      Exit;
    end;

    Inc(FActualPosition);

    if (FActualPosition > Length(FBlocks[0])) and (FStream <> nil) then begin
      FreeLastBlocks(0);
      LoadNextBlock(1);
      FActualPosition := 1;
    end;
  end;
end;

function TXmlTextReader.GetNextSymbol: Char;
begin
  if (FCurPosition > Length(FBlocks[0])) and (FStream <> nil) then begin
    FreeLastBlocks(0);
    LoadNextBlock(1);
    FActualPosition := 1;
  end;

  Result := FBlocks[0][FCurPosition];
  Inc(FCurPosition);
end;

function TXmlTextReader.ReadTo(const SubStr: string; out ResultStr: string; const AdvLenth: integer): boolean;
var
  i, BlockNo, FoundBlockNo, SubStrIndex, SubStrLen,
  CurPos, StartPos, FoundPos, Len, Offset, l: integer;
begin
  BlockNo := 0;
  FoundBlockNo := 0;
  SubStrIndex := 1;
  StartPos := FActualPosition;
  FoundPos := -1;
  SubStrLen := Length(SubStr);
  Result := False;
  CurPos := StartPos;

  while BlockNo <= (FMaxNumBlock - 1) do begin
    while CurPos <= Length(FBlocks[BlockNo]) do begin

      if FBlocks[BlockNo][CurPos] <> SubStr[SubStrIndex] then begin
        SubStrIndex := 1;
        FoundPos := -1;
        Inc(CurPos);
        Continue;
      end;

      if FoundPos = -1 then begin
        FoundPos := CurPos;
        FoundBlockNo := BlockNo;
      end;

      if SubStrIndex = SubStrLen then begin
        Len := FoundPos - StartPos + FBlockSize * FoundBlockNo + AdvLenth;
        SetLength(ResultStr, Len);

        if Len > FBlockSize - StartPos + 1 then
          Len := FBlockSize - StartPos + 1;
        Move(FBlocks[0][StartPos], ResultStr[1], Len * sizeof(char));
        Offset := Len + 1;

        i := 1;
        Len := Length(ResultStr) - Offset + 1;
        while Len > 0 do begin
          if i >= FBlocks.Count then
            LoadNextBlock(FBlocks.Count + 1);

          l := Min(Len, FBlockSize);
          Move(FBlocks[i][1], ResultStr[Offset], l * sizeof(char));
          Inc(Offset, l);
          Dec(Len, l);
          Inc(i);
        end;

        FreeLastBlocks(FBlocks.Count - FoundBlockNo);

        FCurPosition := FoundPos;
        FActualPosition := FoundPos;
        Result := True;
        Exit;
      end;

      Inc(CurPos);
      Inc(SubStrIndex);
    end;

    Inc(BlockNo);
    CurPos := 1;
    if not LoadNextBlock(BlockNo + 1) then
      Exit;
  end;
end;

function TXmlTextReader.TryGetValue(const AttrName: string; out AValue: String): Boolean;
var
  LIndex: Integer;
begin
  LIndex := FAttrNames.IndexOf(AttrName);
  Result := (LIndex >= 0);
  if Result then
    AValue := FAttrValues[LIndex]
  else
    AValue := '';
end;

constructor TXmlTextReader.Create(Stream: TStream);
begin
  inherited Create;

  FBlocks := TStringList.Create;
  FStream := Stream;
  FStream.Position := 0;
  FLastBlockSize := 0;
  FStreamPosition := 0;
  FFullSize := FStream.Size;
  FBlockSize := 8192;
  SetLength(FTmpBlock, FBlockSize);
  FMaxNumBlock := (FFullSize div FBlockSize) + 1;
  FActualPosition := 1;
  FCurPosition := 1;
  LoadNextBlock(1);

  InitInstance;
end;

procedure TXmlTextReader.BeginElementText;
begin
  if FNodeType = xntStartElement then
    if FCurrElementText = nil then begin
      New(FCurrElementText);
      //FCurrElementText := NewStr('');
    end;
end;

constructor TXmlTextReader.Create(const Str: string);
begin
  inherited Create;

  FBlocks := TStringList.Create;
  FBlocks.Add(Str); // Count of blocks always  = 1
  FStream := nil; // When  FStream = nil  procedure NormalizeBuffer not working
  FFullSize := Length(Str);
  FLastBlockSize := FFullSize;
  FStreamPosition := FFullSize;
  FBlockSize := FFullSize;
  FMaxNumBlock := 1;
  FActualPosition := 1;
  FCurPosition := 1;
  InitInstance;
end;

destructor TXmlTextReader.Destroy;
begin
  FBlocks.Free;

  FElementNameStack.Free;
  FElementTextStack.Free;
  FAttrNames.Free;
  FAttrPrefix.Free;
  FAttrValues.Free;

  inherited;
end;

procedure TXmlTextReader.InitInstance;
begin
  FState := xrsInitial;
  FNodeType := xntNone;

  FElementNameStack := TStack<String>.Create;
  FElementTextStack := TStack<PString>.Create;

  FAttrNames := TStringList.Create;
  FAttrPrefix := TStringList.Create;
  FAttrValues := TStringList.Create;

  FCurrElementName := '';
end;

function TXmlTextReader.Read: Boolean;
var
  LValue: string;
//  a: char;
  LEof: Boolean;

  procedure ReadText;
  begin
    if (not IsToken('<')) and (not Eof) then begin
      Inc(FActualPosition);
      ReadTo('<', LValue);
      FCurrElementText^ := FCurrElementText^ + XMLDecode(CRFunctions.UTF8Decode(AnsiString(LValue)));
    end;
  end;

begin
  Result := False;

  FAttrNames.Clear;
  FAttrValues.Clear;
  FAttrPrefix.Clear;

  if FState in [xrsInitial, xrsInteractive] then begin
    if (FNodeType = xntStartElement) then begin
      if (FCurrElementText <> nil) then
        ReadText;
    end else if (FNodeType = xntEndElement) then begin
      FCurrElementText := FElementTextStack.Pop;
    end;

    if not MoveTo('<') then begin
      LEof := Eof;
      if LEof and (FElementNameStack.Count = 0) then begin
        FState := xrsEndOfFile;
        Exit;
      end;
      FState := xrsError;
      if LEof then
        TXmlException.Create('End element with the name '#171 + FElementNameStack.Pop + #187' is missing')
      else
        TXmlException.Create('Root element missing');
    end;

    if IsToken('?') then
      FNodeType := xntDeclaration
    else if IsToken('!--') then
      FNodeType := xntComment
    else if IsToken('!DOCTYPE') then
      FNodeType := xntDocumentType
    else if IsToken('/') then begin
      FNodeType := xntEndElement;
    end else
      FNodeType := xntStartElement;

    FState := xrsInteractive;
  end;

  FName   := '';
  FValue  := '';
  FPrefix := '';

  case FNodeType of
    xntDeclaration:
      begin
        ReadTo(' ', LValue);
        FName := LValue.Trim;
        if not ReadTo('?>', LValue) then begin
          FState := xrsError;
          raise TXmlException.Create('Invalid declaration tag');
        end;
        FValue := LValue.Trim;
      end;

    xntComment:
      begin
        if not ReadTo('-->', LValue) then begin
          FState := xrsError;
          raise TXmlException.Create('Invalid comment tag');
        end;
        FValue := LValue.Trim;
      end;

    xntDocumentType:
      begin
        if not ReadTo('[<', LValue) then begin
          FState := xrsError;
          raise TXmlException.Create('Invalid Document type tag');
        end;

        FName := LValue.Trim;
        ReadTo('>]', LValue);
        FValue := LValue.Trim
      end;

    xntEndElement:
      begin
        if not ReadTo('>', LValue) then begin
          FState := xrsError;
          raise TXmlException.Create('Unclosed EndElement tag');
        end;

        FName := LValue.Trim;

        if (FElementNameStack.Count = 0) then
          Raise TXmlException.Create('EndTag element '#171 + FName + #187' found, but never one opened');
        FCurrElementName := FElementNameStack.Pop;
        if (FCurrElementName <> FName) then
          Raise TXmlException.Create('Invalid EndTag element '#171 + FName + #187': expected '#171 + FCurrElementName + #187);
        //FCurrElementText := FElementTextStack.Pop;
      end;

    xntStartElement:
      begin
        if not ReadTo('>', LValue, 1) then begin
          FState := xrsError;
          raise TXmlException.Create('Unclosed Element tag');
        end;

        // Skip ">".
        GetNextSymbol;

        FName := Copy(LValue, 1, Pos(' ', LValue) - 1).Trim;
        if FName.IsEmpty then
          FName := Copy(LValue, 1, Pos('>', LValue) - 1).Trim;

        Delete(LValue, 1, Length(FName));

        if (LValue.Length > 1) and (LValue[LValue.Length - 1] = '/') then begin
          FNodeType := xntElement;
          if FName[FName.Length] = '/' then
            Delete(FName, Length(FName), 1);
        end else begin
          FElementNameStack.Push(FName);
          FElementTextStack.Push(FCurrElementText);
          FCurrElementText := nil;
        end;

        GetXMLNodeAttributes(CRFunctions.UTF8Decode(AnsiString(LValue)), FAttrNames, FAttrValues);
        FCurrElementName := FName;
      end;

    else begin
      FState := xrsError;
      Assert(False);
    end;
  end;

  Result := True;
end;

function TXmlTextReader.ReadElement(AElementText: TXmlElementText): String;

  procedure LocalReadElement;
  begin
    while Read do begin
      if NodeType = xntStartElement then
        LocalReadElement
      else if NodeType = xntEndElement then
        Exit;
    end;
  end;
begin
  if NodeType = xntElement then
    Exit('')
  else if NodeType <> xntStartElement then
    Raise TXmlException.Create('Invalid function call '#171'ReadElement'#187': NodeType <> xntStartElement');

  if AElementText <> xetSkip then
    BeginElementText;

  LocalReadElement;
  Result := CurrElementText;
  case AElementText of
    xetLTrim: Result := Result.TrimLeft;
    xetRTrim: Result := Result.TrimRight;
    xetTrim : Result := Result.Trim;
  end;
end;

function TXmlTextReader.FindElement(const ATagNames: String): Boolean;
begin
//  if (FState = xrsInitial) then
//  Read;

  while Read do begin
    if NodeType in [xntElement, xntStartElement] then begin
      if ATagNames.IsEmpty or InArray(ATagNames, FCurrElementName, ';') then Exit(True);
      if NodeType = xntStartElement then
        ReadElement;
    end else if (NodeType = xntEndElement) then
      Break;
  end;

  Result := False;
end;

procedure TXmlTextReader.GetXMLNodeAttributes(const Node: string; AttrNames, AttrValues: TStrings);
var
  Parser: TXmlParser;
  Code: integer;
  Lexem, AttrName, AttrValue, AttrPrefix: string;
  WithColon: boolean;
begin
  Parser := TXmlParser.Create(Node);
  try
    while True do begin
      Code := Parser.GetNextIdent(Lexem);
      case Code of
        lcEnd, lxLess, lxMore: // '<', '>'
          break;
        lcIdent: begin
          WithColon := False;
          AttrName := Lexem;
          AttrPrefix := '';
          Code := Parser.GetNextIdent(Lexem);
          if Code = lcIdent then begin
            AttrName := Lexem;
            Code := Parser.GetNextIdent(Lexem);
          end;
          if Code in [lxColon, lxEqual] then begin
            if Code = lxColon then begin // ':'
              Code := Parser.GetNextIdent(Lexem);
              if Code <> lcIdent then
                raise TXmlException.Create(SInvalidXML);
              AttrPrefix := AttrName;
              AttrName := AttrPrefix + ':' + Lexem;
              Code := Parser.GetNextIdent(Lexem);
              WithColon := True;
            end;

            if Code = lxEqual then begin // '='
              Code := Parser.GetNext(Lexem);
              if (Code <> lcIdent) and (Code <> lcString) then
                raise TXmlException.Create(SInvalidXML);
              AttrValue := XMLDecode(Lexem);
              if WithColon and (LowerCase(AttrName) = 'name') then
                AttrName := ':' + AttrName;
              AttrNames.Add(AttrName);
              AttrValues.Add(AttrValue);
              FAttrPrefix.Add(AttrPrefix);
            end;
          end;
        end;
      end;
    end;
  finally
    Parser.Free;
  end;
  Assert(AttrNames.Count = AttrValues.Count);
end;

function XMLEncode(const AStr: WideString): WideString;
var
  sb: WideStringBuilder;
begin
  sb := WideStringBuilder.Create(AStr, Length(AStr));
  try
    sb.Replace('&', '&amp;');
    sb.Replace('''', '&#x27;');
    sb.Replace('"', '&quot;');
    sb.Replace('<', '&lt;');
    sb.Replace('>', '&gt;');
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function XMLDecode(const AStr: string): string;
var
  sb: StringBuilder;
begin
  sb := StringBuilder.Create(AStr, Length(AStr));
  try
    sb.Replace('&#x27;', '''');
    sb.Replace('&#x39;', '''');
    sb.Replace('&#x92;', '''');
    sb.Replace('&#x96;', '''');
    sb.Replace('&#x22;', '"');
    sb.Replace('&#x3c;', '<');
    sb.Replace('&#x3e;', '>');
    sb.Replace('&#x26;', '&');
    sb.Replace('&amp;', '&');
    sb.Replace('&quot;', '"');
    sb.Replace('&lt;', '<');
    sb.Replace('&gt;', '>');
    sb.Replace('&#13;', #13);
    sb.Replace('&#10;', #10);
//      sb.Replace('&#x0A;', #10);
//      sb.Replace('&#x0D;', #13);

    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TXmlTextReader.GetValue(const AttrName: String): String;
begin
  Result := FAttrValues[FAttrNames.IndexOf(AttrName)];
end;

function TXmlTextReader.GetAttributeCount: integer;
begin
  Result := FAttrNames.Count;
end;

function TXmlTextReader.GetCurrElementText: String;
begin
  if FCurrElementText = nil then
    Result := ''
  else
    Result := FCurrElementText^;
end;

procedure TXmlTextReader.MoveToAttribute(i: integer);
begin
  try
    FName := FAttrNames[i];
    FValue := FAttrValues[i];
    FPrefix := FAttrPrefix[i];
  except
    raise TXmlException.Create(Format('Attribute not found (%d)', [i]));
  end;
end;

function TXmlTextReader.MoveToAttribute(const name: string): Boolean;
begin
  try
    MoveToAttribute(FAttrNames.IndexOf(name));
    Result := True;
  except
    Result := False;
  end;
end;

function TXmlTextReader.GetDepth: integer;
begin
  Result := FElementNameStack.Count;
end;

function TXmlTextReader.GetHasAttributes: Boolean;
begin
  Result := FAttrNames.Count > 0;
end;

{ TXmlParser }

constructor TXmlParser.Create(const Text: string);
begin
  inherited;

  FSymbolLexems := XmlSymbolLexems;
  FSymbolChars := XmlSymbolChars;

  FKeywordLexems := XmlKeywordLexems;
  FOmitKeywords := True; // XmlKeywordLexems.Count = 0
end;

function TXmlParser.GetNextIdent(out Lexem: string): integer;
var
  i: integer;
begin
{$IFDEF PERF_COUNTER}
  PerfCounters[2].Start;//}
  try
{$ENDIF}
  Result := InternalGetNext; // Result is not in KeywordLexems

  if Result = lcIdent then begin// Optimize
    if Result > 0 then begin
      Assert(FLexem <> '');
      Lexem := FLexem
    end
    else begin
      if FLexemLength = 0 then
        Lexem := ''
      else
      begin
        Assert(FLexemPos > 0);
        Lexem := CopyText(FLexemPos, FLexemLength);
      end;
    end;
  end;

  if not FOmitKeywords and (Result = lcIdent) then begin
    Assert(FLexemPos > 0);
    Assert(FLexemLength > 0);
    i := FindLexemIndex(FLexemPos, FLexemLength, FKeywordLexems);
    if i <> -1 then begin
      Result := FKeywordLexems.Values[i];
      Assert(Result > 0);
      if Uppered then
        Lexem := AnsiUpperCase(Lexem);  //WAR problem with macros as key words
    end;
  end;

  Assert(Result <> - MaxInt);
{$IFDEF PERF_COUNTER}
  finally
    PerfCounters[2].Stop;
  end;
{$ENDIF}
end;

{$IFDEF FASTSCRIPT_RTTI}
{ TfsFunctions_Xml }

constructor TfsFunctions_Xml.Create(AScript: TfsScript);
const
  SCategory: String = 'XML функции';
begin
  inherited;

  AScript.AddEnum('TXmlNodeType', 'xntNone, xntElement, xntStartElement, xntEndElement, xntComment, xntDeclaration, xntDocumentType');
  AScript.AddEnum('TXmlElementText', 'xetSkip, xetLTrim, xetRTrim, xetTrim');

//  with AScript do begin
//  end;
end;

//function TfsFunctions_Xml.Call_String_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
//begin
//end;
{$ENDIF}

var
  i: integer;

initialization
  XmlSymbolLexems  := TIntValueStringList.Create;
  XmlKeywordLexems := TIntValueStringList.Create;

  XmlSymbolLexems.Add(':', lxColon);
  XmlSymbolLexems.Add('<', lxLess);
  XmlSymbolLexems.Add('=', lxEqual);
  XmlSymbolLexems.Add('>', lxMore);
  XmlSymbolLexems.Sort;

  // Performance optimization
  XmlSymbolChars := [];
  for i := 0 to XmlSymbolLexems.Count - 1 do
    XmlSymbolChars := XmlSymbolChars + [AnsiChar(XmlSymbolLexems[i][1])];

finalization
  XmlSymbolLexems.Free;
  XmlKeywordLexems.Free;

end.
