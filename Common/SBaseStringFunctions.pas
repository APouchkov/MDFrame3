unit SBaseStringFunctions;
{$I config.inc}

interface

uses
  Classes, SysUtils, StrUtils, Variants, FMTBcd, DateUtils, Math, SBaseUtils,
  SBaseVariantFunctions
{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter, fs_idbrtti
{$ENDIF}
{$IFNDEF PACKAGE}
  , NamedVariables
  , DB, HTTPApp, NetEncoding
{$ENDIF}
  ;

const
  CConst_LineBreaks: Array of Char = [#13, #10];

type
  TStringsHelper = class Helper for TStrings
  public
    function  IsEqualStrings(ATarget: TStrings): Boolean;
    procedure AssignStrings(ATarget: TStrings);

    function AssignStringsIfDifferent(ATarget: TStrings): Boolean;
    function SetTextIfDifferent(const AText: String): Boolean;
  end;

  TStringArray = Array of String;
  PStringArray = ^TStringArray;

  TEnumStringMode = (esmTrim, esmSkipEmpty);
  TEnumStringModes = set of TEnumStringMode;

  TStringItem = record
    Index: Integer;
    Value: String;
    Eof  : Boolean;
  end;

  TStringItemsEnumerator = record
  private
    FDelimiter    : Char;
    FQuotedItems  : Boolean;
    FString       : String;
    FModes        : TEnumStringModes;

    FPosition     : Integer;
    FCurrent      : TStringItem;

    constructor Create(const AString: String; const ADelimiter: Char; AQuotedItems: Boolean; AModes: TEnumStringModes);
    function GetCurrent: TStringItem; inline;

    procedure SkipBlanks;
  public
    function MoveNext: Boolean;
    property Current: TStringItem read GetCurrent;
  end;

  TStringNamedItem = record
    Index : Integer;
    Name  : String;
    CastAs: String;
    Value : String;
    Eof   : Boolean;
  end;

  TStringNamedItemsEnumerator = record
  private
    FDelimiter    : Char;
    FString       : String;

    FPosition     : Integer;
    FCurrent      : TStringNamedItem;
    FWithCastAs   : Boolean;

    constructor Create(const AString: String; const ADelimiter: Char; AWithCastAs: Boolean = False);
    function GetCurrent: TStringNamedItem; inline;
  public
    function MoveNext: Boolean;
    property Current: TStringNamedItem read GetCurrent;
  end;

  TStringNameItem = record
    Index     : Integer;
    NameLeft  : String;
    CastAs    : String;
    NameRight : String;
    Eof       : Boolean;
    NameFull  : String;
  end;

  TStringNameItemsEnumerator = record
  private
    FDelimiter    : Char;
    FString       : String;

    FPosition     : Integer;
    FCurrent      : TStringNameItem;
    FWithCastAs   : Boolean;

    constructor Create(const AString: String; const ADelimiter: Char; AWithCastAs: Boolean = False);
    function GetCurrent: TStringNameItem; inline;
  public
    function MoveNext: Boolean;
    property Current: TStringNameItem read GetCurrent;
  end;

  // Строка параметров "xxx :[yyy] zzz"
  TStringParamItem = record
    Gap   : String;
    Value : String;
    Format: String;
  end;

  TEnumParamsMode = (epmSkipGap, epmFormat);
  TEnumParamsModes = set of TEnumParamsMode;

  TStringParamItemsEnumerator = record
  private
    FString       : String;
    FModes        : TEnumParamsModes;
    FLength       : Integer;
    FPosition     : Integer;

    FCurrent      : TStringParamItem;

    constructor Create(const AString: String; AModes: TEnumParamsModes);
    function GetCurrent: TStringParamItem; inline;
  public
    function MoveNext: Boolean;
    property Current: TStringParamItem read GetCurrent;
  end;

  TScriptCommentMethod = (scmNone, scmLattice, scmDoubleMinus, scmDoubleSlash, scmBraces, scmSlashRange, scmBracketRange);
  TScriptCommentMethods = Set of scmLattice .. scmBracketRange;
  TSQLParamItem = record
    Gap  : String;
    Quote: Char;
    Value: String;
    Eof  : Boolean;
  end;

  TSQLParamItemsEnumerator = record
  private
    FString       : String;
    FPrefix       : Char;
    FComments     : TScriptCommentMethods;
    FQuotes       : TSysCharSet;
    FLiterals     : TSysCharSet;

    FCurrChar     : Char;
    FNextChar     : Char;

    FLength       : Integer;
    FPosition     : Integer;

    FCurrent      : TSQLParamItem;

    constructor Create
                (
            const ACommandText: String;
                  APrefix     : Char;
                  AComments   : TScriptCommentMethods;
                  AQuotes     : TSysCharSet;
                  ALiterals   : TSysCharSet
                );
    function GetCurrent: TSQLParamItem; inline;
    procedure MoveToNextChar(AIncPosition: Boolean = True);
  public
    function MoveNext: Boolean;
    property Current: TSQLParamItem read GetCurrent;
  end;

  { Строки (возможно квотированные), разделённые делителем }
  function EnumStringItems(const AParamString: String; const ADelimiter: Char = ';'; AQuotedItems: Boolean = False; AModes: TEnumStringModes = []): TBaseEnumeratorFactory<TStringItemsEnumerator>;

  { Именованные значения (возможно квотированные), разделённые делителем }
  function EnumStringNamedItems(const AParamString: String; const ADelimiter: Char = ';'; AWithCastAs: Boolean = False): TBaseEnumeratorFactory<TStringNamedItemsEnumerator>;

  { Имена (возможно квотированные) с возможностью ссылок на другие имена, разделённые делителем }
  function EnumStringNameItems(const AParamString: String; const ADelimiter: Char = ';'; AWithCastAs: Boolean = False): TBaseEnumeratorFactory<TStringNameItemsEnumerator>;

  function EnumStringParamItems(const AParamString: String; AModes: TEnumParamsModes = [epmSkipGap]): TBaseEnumeratorFactory<TStringParamItemsEnumerator>;
  function EnumSQLParamItems(const ACommandText: String; APrefix: Char = ':'; AComments: TScriptCommentMethods = [scmDoubleMinus, scmSlashRange]; AQuotes: TSysCharSet = ['[']; ALiterals: TSysCharSet = ['''']): TBaseEnumeratorFactory<TSQLParamItemsEnumerator>;

{$IFNDEF PACKAGE}
  function FormatString(const AFormat: String; const AVariables: TNamedVariants; AMetaCharacter: Char = '%'): String;
{$ENDIF}

  function TrimLeft(const S: string; Letter: Char): string; overload;
  function TrimRight(const S: string; Letter: Char): string; overload;
{
  function  HexToAnsiString(const AHexString: AnsiString): AnsiString; overload;
  function  HexToAnsiString(const AHexString: String): AnsiString; overload;
}
  function  AnsiStringToHex(const AAnsiString: AnsiString): String;
  function  WideStringToHex(const AWideString: WideString): String;

  function  XMLEncode(const AValue: String): String;
  function  GetRightQuote(ALeftQuote: Char): Char; overload;
  function  GetRightQuote(ALeftQuote: Char; const AAllowedQuotes: TSysCharSet): Char; overload;

  function  QuoteString(const AString: String; AQuote: Char; AOnlyIfNeeded: Boolean = False): String;
  function  UnQuoteString(const AString: String; AQuote: Char; AOnlyIfNeeded: Boolean = False): String;

  function  ZeroPredicatedUInt(AValue: UInt64; ADigits: UInt8; ARaiseIfOverflow: Boolean = False): String;

  function  TParamDateTimeFormat(ADateTime: Variant): String;

//  function  IsAbsolutePath(const APath: String): Boolean;

  function  NewGUId: TGUID;
  function  NewGUIdString: String; inline;

//  function  QuotedPosEx(const AChar: Char; const AText: String; AIndex: Integer): Integer;
  function  FindPos(const AChar: Char; const AString: String; var AIndex: Integer): Boolean;
  function  FindPosEx(const AChar: Char; const AString: String; AOffset: Integer; var AIndex: Integer): Boolean;
  function  ParseEOQ(const ARightQuote: Char; const AString: String; var AOffset: Integer; out AValue: String; ANextChars: TSysCharSet = []): Boolean;

  function  PosExRevert(const AChar: Char; const AString: String; AOffset: Integer = -1): Integer;
  function  CharCount(const AChar: Char; const AString: String): Integer;
  function  CharFromString(const AString: String): Char;

  function  IsEmptyString(const V1, V2: String): String;
//  function  ConcatIfNotEmpty(const String1, String2: String): String; overload;
  function  ConcatIfNotEmpty(const AString1, ASeparator, AString2: String): String; overload;
  function  ConcatIfNotEmpty(const AStrings: Array of String; const ASeparator: String): String; overload;

  function  RemovePrefixIfPresent(const AString: String; const APrefix: String): String;

{$IFNDEF PACKAGE}
  function  ExtractAndRemoveValue(var Values: String; const Delimiter: Char = ','; const Field_Type: TFieldType = ftString; const AField_Name: String = ''): Variant;
  function  ExtractAndRemoveParamValue(var Values: String; const Delimiter: Char; const Field_Type: TFieldType; var AField_Name: String): Variant;
{$ENDIF}

{ Работа со строкой перечисления значений }
  procedure ExtractNameAndAlias(const ANameString: String; AAlias: PString = nil; AName: PString = nil);
  function  InArray(const SArray, SElement: String; SDelimiter: Char = ','): Boolean; overload;
  function  InArray(const SArray: String; IElement: Integer; SDelimiter: Char = ','): Boolean; overload;

  function  AnsiSubStringPosition(const ASubString, AString: String): Integer;

  function  ArraysMerge(const AArrays: Array of String; SDelimiter: Char = ','): String;

  function  ArraysJoin(const AArrays: Array of String; SDelimiter: Char = ','): String; overload;
  function  ArraysJoin(const SArray1: String; const SArray2: String; SDelimiter: Char = ','): String; overload; inline;

  function  ArraysAntiJoin(SFromArray: String; SThisArrays: Array of String; SDelimiter: Char = ','): String; overload;
  function  ArraysAntiJoin(const SFromArray: String; SThisArray: String; SDelimiter: Char = ','): String; overload; inline;

  function  ArraysPositiveJoin(S1Array: String; S2Array: String; SDelimiter: Char = ','): Boolean;

  function  ArrayQuote(const AArray: String; SDelimiter: Char = ','; AQuote: Char = ','): String;

{
  TBigStringBuilder = class
  protected
    FString: string;
    FMaxBlockSize: Integer;
    FActualLength: integer;
  public
    constructor Create(const MaxBlockSize: Integer);
    procedure Append(const value: string); overload;
    function ToString: string;
  end;
}

{$IFDEF FASTSCRIPT_RTTI}
type
  TfsFunctions_String = class(TfsRTTIModule)
  private
    function Call_String_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

implementation

uses SBaseConstants;

const
  SConst_TParamDateFormat : String = 'yyyy-mm-dd';
  SConst_TParamTimeFormat : String = 'hh:nn:ss.zzz';

(*
procedure EnumStringItems(AFunction: TEnumStringItemFoundFunction; const AString: String; const AReserved: Array of Pointer; const ADelimiter: Char; const ASingleItemIndex: Integer);
var
  iStart, iEnd: Integer;
  LSizeOf: Word;
begin
  iEnd := Pos(ADelimiter, AString);
  if iEnd = 0 then
    AFunction(AString, ASingleItemIndex, AReserved)
  else begin
    LSizeOf := 0;
    iStart := 1;
    repeat
      if AFunction(Copy(AString, iStart, iEnd - iStart), LSizeOf, AReserved) then Exit;
      iStart := Succ(iEnd);
      iEnd := PosEx(ADelimiter, AString, iStart);
      Inc(LSizeOf);
    until iEnd = 0;
    AFunction(Copy(AString, iStart, Length(AString) - iStart + 1), LSizeOf, AReserved);
  end;
end;
*)

function EnumStringItems(const AParamString: String; const ADelimiter: Char; AQuotedItems: Boolean; AModes: TEnumStringModes): TBaseEnumeratorFactory<TStringItemsEnumerator>;
begin
  Result := TBaseEnumeratorFactory<TStringItemsEnumerator>.
              Create
              (
                TStringItemsEnumerator.Create(AParamString, ADelimiter, AQuotedItems, AModes)
              );
end;

function EnumStringNamedItems(const AParamString: String; const ADelimiter: Char; AWithCastAs: Boolean): TBaseEnumeratorFactory<TStringNamedItemsEnumerator>;
begin
  Result := TBaseEnumeratorFactory<TStringNamedItemsEnumerator>.Create(TStringNamedItemsEnumerator.Create(AParamString, ADelimiter, AWithCastAs));
end;

function EnumStringNameItems(const AParamString: String; const ADelimiter: Char; AWithCastAs: Boolean): TBaseEnumeratorFactory<TStringNameItemsEnumerator>;
begin
  Result := TBaseEnumeratorFactory<TStringNameItemsEnumerator>.Create(TStringNameItemsEnumerator.Create(AParamString, ADelimiter, AWithCastAs));
end;

function EnumStringParamItems(const AParamString: String; AModes: TEnumParamsModes): TBaseEnumeratorFactory<TStringParamItemsEnumerator>;
begin
  Result := TBaseEnumeratorFactory<TStringParamItemsEnumerator>.Create(TStringParamItemsEnumerator.Create(AParamString, AModes));
end;

function EnumSQLParamItems(const ACommandText: String; APrefix: Char; AComments: TScriptCommentMethods; AQuotes: TSysCharSet; ALiterals: TSysCharSet): TBaseEnumeratorFactory<TSQLParamItemsEnumerator>;
begin
  Result := TBaseEnumeratorFactory<TSQLParamItemsEnumerator>.Create(TSQLParamItemsEnumerator.Create(ACommandText, APrefix, AComments, AQuotes, ALiterals));
end;

{$IFNDEF PACKAGE}
{
  %String
  %Date(Date:dd.mm.yyyy)
  %Float(Float:0,.00####)
}
function FormatString(const AFormat: String; const AVariables: TNamedVariants; AMetaCharacter: Char): String;
var
  LOldPos, LNewPos, LLength: Integer;
  LVarName, LFormatType, LFormatMask, LFormatNull: String;

  function NameDelimiter(AChar: Char): Boolean;
  begin
    Result := (AChar <= #32) or CharInSet(AChar, [',', ';', '(', ')'{, '['}, ']'{, '{'}, '}', '|', '%', '&', '!', '-', '+', '=']);
  end;

  procedure FindMetaCharacter;
  var
    LStreamCharacter: Char;
  begin
    while True do begin
      LNewPos := PosEx(AMetaCharacter, AFormat, LOldPos);
      if (LNewPos = 0) or (LNewPos = LLength) then begin
        Result := Result + Copy(AFormat, LOldPos, LLength - LOldPos + 1);
        LOldPos := 0;
        Break;
      end else if AFormat[LNewPos + 1] = AMetaCharacter then begin
        Result := Result + Copy(AFormat, LOldPos, LNewPos - LOldPos + 1);
        LOldPos := LNewPos + 2;
      end else if NameDelimiter(AFormat[LNewPos + 1]) then begin
        Result := Result + Copy(AFormat, LOldPos, LNewPos - LOldPos + 2);
        LOldPos := LNewPos + 2;
      end else begin
        Result := Result + Copy(AFormat, LOldPos, LNewPos - LOldPos);
        LOldPos := LNewPos + 1;

        LStreamCharacter := AFormat[LOldPos];
        case LStreamCharacter of
          '[': LStreamCharacter := ']';
          '{': LStreamCharacter := '}';
          '''', '"': ;
          else LStreamCharacter := #0;
        end;
        if LStreamCharacter <> #0 then begin
          LNewPos := PosEx(LStreamCharacter, AFormat, LOldPos + 1);
          if LNewPos = 0 then begin
            Result := Result + Copy(AFormat, LOldPos, LLength - LOldPos + 1);
            LOldPos := 0;
            Break;
          end;
          LVarName := Copy(AFormat, LOldPos + 1, LNewPos - LOldPos - 1);
          Inc(LNewPos);
        end else begin
          LNewPos := LOldPos + 1;
          while (LNewPos <= LLength) and (not NameDelimiter(AFormat[LNewPos])) do Inc(LNewPos);
          LVarName := Copy(AFormat, LOldPos, LNewPos - LOldPos);
        end;

        LOldPos := LNewPos;
        Break;
      end;
    end;
  end;

  procedure ExtractVarData;
  begin
    LFormatType := '';
    LFormatMask := '';
    LFormatNull := '';

    LNewPos := Pos('|', LVarName);
    if LNewPos > 0 then begin
      LFormatType := Copy(LVarName, LNewPos + 1, Length(LVarName) - LNewPos);
      LVarName    := Copy(LVarName, 1, LNewPos - 1);
      LNewPos := Pos('|', LFormatType);
      if LNewPos > 0 then begin
        LFormatNull := Copy(LFormatType, LNewPos + 1, Length(LFormatType) - LNewPos);
        LFormatType := Copy(LFormatType, 1, LNewPos - 1);
      end;
      LNewPos := Pos('=', LFormatType);
      if LNewPos > 0 then begin
        LFormatMask := Copy(LFormatType, LNewPos + 1, Length(LFormatType) - LNewPos);
        LFormatType := Copy(LFormatType, 1, LNewPos - 1);
      end;
    end;
  end;
begin
  Result := '';
  LLength := Length(AFormat);
  if LLength = 0 then Exit;

  LOldPos := 1;
  while LOldPos > 0 do begin
    FindMetaCharacter;
    if LOldPos = 0 then Break;

    ExtractVarData;
    if LFormatType.IsEmpty then
      Result := Result + VarToStr(AVariables[LVarName])
    else if SameText(LFormatType, DelphiTypeDate) or SameText(LFormatType, DelphiTypeDateTime) then
      Result := Result + FormatDateTime(LFormatMask, VarToDateTime(AVariables[LVarName]))
    else if SameText(LFormatType, DelphiTypeFloat) then
      Result := Result + FormatFloat(LFormatMask, Extended(AVariables[LVarName]))
    else
      Raise Exception.CreateFmt(MError_UnknownFormatType, [LFormatType]);
  end;
end;
{$ENDIF}

//function Trim(const S: string; Letter: Char): string;
//var
//  I, L: Integer;
//begin
//  L := Length(S);
//  I := 1;
//  while (I <= L) and ((S[I] = Letter) or ((Letter = ' ') and (S[I] < ' '))) do Inc(I);
//  if I > L then
//    Result := ''
//  else begin
//    while (S[L] = Letter) or ((Letter = ' ') and (S[L] < ' ')) do Dec(L);
//    Result := Copy(S, I, L - I + 1);
//  end;
//end;

function TrimLeft(const S: string; Letter: Char): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;

  while (I <= L)
    and
    (
      (S[I] = Letter)
      or ((Letter = ' ') and (S[I] < ' '))
      or (CharInSet(Letter, [#13, #10]) and CharInSet(S[I], [#13, #10]))
    )
    do Inc(I);

  if I > L then
    Result := ''
  else if I > 1 then
    Result := Copy(S, I, L - I + 1)
  else
    Result := S
end;

function TrimRight(const S: string; Letter: Char): string;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0)
    and
    (
      (S[I] = Letter)
      or ((Letter = ' ') and (S[I] < ' '))
      or (CharInSet(Letter, [#13, #10]) and CharInSet(S[I], [#13, #10]))
    )
    do Dec(I);

  Result := Copy(S, 1, I);
end;

{
function HexToAnsiString(const AHexString: AnsiString): AnsiString;
var
  LLength: Word;
begin
  LLength := Length(AHexString) shr 1;
  SetLength(Result, LLength);
  HexToBin(PAnsiChar(@AHexString[1]), PAnsiChar(@Result[1]), LLength);
end;

function HexToAnsiString(const AHexString: String): AnsiString;
var
  LLength: Word;
begin
  LLength := Length(AHexString) shr 1;
  SetLength(Result, LLength);
  HexToBin(PChar(@AHexString[1]), PAnsiChar(@Result[1]), LLength);
end;
}

function AnsiStringToHex(const AAnsiString: AnsiString): String;
var
  LLength: Word;
begin
  LLength := Length(AAnsiString);
  SetLength(Result, LLength shl 1);
  BinToHex(PAnsiChar(@AAnsiString[1]), PChar(@Result[1]), LLength);
end;

function WideStringToHex(const AWideString: WideString): String;
var
  LLength: Word;
begin
  LLength := Length(AWideString) shl 1;
  SetLength(Result, LLength shl 1);
  BinToHex(Pointer(@AWideString[1]), PChar(@Result[1]), LLength);
end;

function XMLEncode(const AValue: String): String;
begin
  Result := AnsiReplaceStr(AValue, '&', '&amp;');
  Result := AnsiReplaceStr(Result, '"', '&quot;');
  Result := AnsiReplaceStr(Result, '<', '&lt;');
  Result := AnsiReplaceStr(Result, '>', '&gt;');
  Result := AnsiReplaceStr(Result, char(10), '&#x0A;');
  Result := AnsiReplaceStr(Result, char(13), '&#x0D;');
end;

function GetRightQuote(ALeftQuote: Char): Char;
begin
  case ALeftQuote of
    '('  : Result := ')';
    '['  : Result := ']';
    '{'  : Result := '}';
    else Result := ALeftQuote;
  end;
end;

function GetRightQuote(ALeftQuote: Char; const AAllowedQuotes: TSysCharSet): Char;
begin
  if CharInSet(ALeftQuote, AAllowedQuotes) then
    Result := GetRightQuote(ALeftQuote)
  else
    Result := #0;
end;

function QuoteString(const AString: String; AQuote: Char; AOnlyIfNeeded: Boolean): String;
var
  LQuote: Char;
begin
  if AOnlyIfNeeded and (Pos(AQuote, AString) = 0) then
    Result := AString
  else begin
    LQuote := GetRightQuote(AQuote);
    if LQuote = #0 then
      Raise Exception.Create('Unknown Quotation method = '#171 + AQuote + #187);
    Result := AQuote + StringReplace(AString, LQuote, LQuote + LQuote, [rfReplaceAll, rfIgnoreCase]) +LQuote;
  end;
end;

function UnQuoteString(const AString: String; AQuote: Char; AOnlyIfNeeded: Boolean = False): String;
const
  SError_InvalidQuotedString: String = 'Invalid Quoted String: ';
var
  LLength: Integer;
  LQuote: Char;
begin
  LLength := Length(AString);
  if (LLength <= 1) or ((AQuote <> #0) and (AString[1] <> AQuote)) then
    if AOnlyIfNeeded then
      Exit(AString)
    else
      Raise Exception.Create(SError_InvalidQuotedString + AString);

  LQuote := GetRightQuote(AString[1], ['"', '[', '''']);
  if LQuote = #0 then
    if AOnlyIfNeeded then
      Exit(AString)
    else
      Raise Exception.Create(SError_InvalidQuotedString + AString);

  if AString[LLength] <> LQuote then
    if AOnlyIfNeeded then
      Exit(AString)
    else
      Raise Exception.Create(SError_InvalidQuotedString + AString);

  Result := StringReplace(Copy(AString, 2, LLength - 2), LQuote + LQuote, LQuote, [rfReplaceAll, rfIgnoreCase]);
end;

function ZeroPredicatedUInt(AValue: UInt64; ADigits: UInt8; ARaiseIfOverflow: Boolean): String;
var
  I, L: Integer;
begin
  Result := UIntToStr(AValue);
  L := Length(Result);
  if ADigits = L then Exit;
  Assert((not ARaiseIfOverflow) or (L < ADigits));
  for I := L to ADigits - 1 do
    Result := '0' + Result;
end;

{
function IsAbsolutePath(const APath: String): Boolean;
begin
  Result := False;
end;
}

function TParamDateTimeFormat(ADateTime: Variant): String;
var
  LDateTime: TDateTime;
begin
  case VarType(ADateTime) of
    varDate:
      LDateTime := VarToDateTime(ADateTime)
    else
      LDateTime := ADateTime;
  end;

  if TimeOf(LDateTime) > 0.0 then
    Result := TrimRight(TrimRight(' ' + FormatDateTime(SConst_TParamTimeFormat, LDateTime), '0'), '.')
  else
    Result := '';

  Result := FormatDateTime(SConst_TParamDateFormat, LDateTime) + Result;
end;

{}

function NewGUId: TGUID;
begin
  Assert(CreateGUID(Result) = 0);
end;

function NewGUIdString: String;
begin
  Result := GUIDToString(NewGUId);
end;

{$IFNDEF PACKAGE}
{}

function ExtractAndRemoveValue(var Values: String; const Delimiter: Char = ','; const Field_Type: TFieldType = ftString; const AField_Name: String = ''): Variant;
var
  LField_Name: String;
begin
  LField_Name := AField_Name;
  Result := ExtractAndRemoveParamValue(Values, Delimiter, Field_Type, LField_Name)
end;

function ExtractAndRemoveParamValue(var Values: String; const Delimiter: Char; const Field_Type: TFieldType; var AField_Name: String): Variant;
var
  iBeg, iEnd: Integer;
  ResultText: String;
begin
  if (Values = '') then begin
    ResultText := '';
    if (AField_Name = '?') then
      AField_Name := '';
  end else begin
    if (AField_Name = '') or (AField_Name = '?') then begin
      iEnd := Pos(Delimiter, Values);
      if AField_Name = '?' then begin
        iBeg := Pos('=', Values);
        if (iEnd > 0) and (iBeg > iEnd) then
          AField_Name := ''
        else begin
          AField_Name := Copy(Values, 1, Pred(iBeg));
          Delete(Values, 1, iBeg);
          if iEnd > 0 then
            Dec(iEnd, iBeg);
        end;
      end;
      iBeg := 1;
    end else begin
      iBeg := AnsiSubStringPosition(Delimiter + AField_Name + '=', Delimiter + Values);
      if iBeg > 0 then begin
        Delete(Values, iBeg, Succ(Length(AField_Name)));
        iEnd := PosEx(Delimiter, Values, iBeg)
      end else
        iEnd := 0;
    end;

    if (iBeg = 1) and (iEnd = 0) then begin
      ResultText := Values;
      Values := ''
    end else if (iBeg > 1) and (iEnd = 0) then begin
      ResultText := Copy( Values, iBeg, Length(Values) - iBeg + 1 );
      Values := Copy( Values, 1, iBeg - 2);
    end else if iBeg <= iEnd then begin
      ResultText := Copy( Values, iBeg, iEnd - iBeg );
      Delete( Values, iBeg, Succ(iEnd - iBeg) );
    end;
  end;

  Result := Null;
  if (ResultText <> '') and (not SameText(ResultText, SConst_NULL)) then try
    case Field_Type of
      ftBoolean           : Result := ResultText = '1';
      ftInteger           : Result := StrToInt(ResultText);
      ftFloat, ftExtended : Result := StrToFloat(ResultText);
      ftBCD, ftFMTBCD     : Result := VarFMTBcdCreate(StrToBcd(ResultText));
      ftDate:
        begin
          ResultText := ResultText.Trim(['''']);
          Result := EncodeDate
                    (
                      StrToInt(Copy(ResultText, 1, 4)),
                      StrToInt(Copy(ResultText, 5, 2)),
                      StrToInt(Copy(ResultText, 7, 2))
                    )
        end;
      else
{$WARNINGS OFF}
        Result := String(HTTPDecode(ResultText));
{$WARNINGS ON}
    end;
  except
  end;
end;
{$ENDIF}

function FindPos(const AChar: Char; const AString: String; var AIndex: Integer): Boolean;
begin
  AIndex := Pos(AChar, AString);
  Result := (AIndex > 0);
end;

function FindPosEx(const AChar: Char; const AString: String; AOffset: Integer; var AIndex: Integer): Boolean;
begin
  AIndex := PosEx(AChar, AString, AOffset);
  Result := (AIndex > 0);
end;

function PosExRevert(const AChar: char; const AString: string; AOffset: Integer = -1): Integer;
var
  Len: Integer;
begin
  Len := Length(AString);
  if (AOffset = 0) or (AOffset > Len) then
    AOffset := Len
  else if AOffset < 0 then
    Inc(AOffset, Len + 1);

  while AOffset > 0 do
    begin
      if AString[AOffset] = AChar then begin
        Result := AOffset;
        Exit;
      end;
      Dec(AOffset);
    end;

  Result := 0;
end;

function ParseEOQ(const ARightQuote: Char; const AString: String; var AOffset: Integer; out AValue: String; ANextChars: TSysCharSet): Boolean;
var
  LPos, LNextPos: Integer;
  LNextChar: Char;
begin
  AValue := '';
  LPos := AOffset;
  while True do begin
    LNextPos := PosEx(ARightQuote, AString, LPos);
    if LNextPos = 0 then Exit(False);

    AValue := AValue + Copy(AString, LPos, LNextPos - LPos);
    LPos := LNextPos + 1;
    if (LPos > AString.Length) then
      LNextChar := #0
    else
      LNextChar := AString[LPos];

    if (LNextChar = ARightQuote) then begin
      Inc(LPos);
      AValue := AValue + ARightQuote;
    end else if (ANextChars = []) or CharInSet(LNextChar, ANextChars) then
      Break
    else
      Exit(False)
  end;

  AOffset := LPos;
  Result := True;
end;

function CharCount(const AChar: Char; const AString: String): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to Length(AString) do
    if AString[I] = AChar then
      Inc(Result)
end;

function CharFromString(const AString: String): Char;
begin
  if AString.IsEmpty then
    Result := #0
  else
    Result := AString[1]
end;

function IsEmptyString(const v1,v2: String): String;
begin
  if v1 = '' then
    Result := v2
  else
    Result := v1
end;

{
function ConcatIfNotEmpty(const String1, String2: String): String;
begin
  if String1 = '' then
    Result := ''
  else
    Result := String1 + String2
end;
}

function ConcatIfNotEmpty(const AString1, ASeparator, AString2: String): String;
begin
  if AString1 = '' then
    Result := AString2
  else if AString2 = '' then
    Result := AString1
  else
    Result := AString1 + ASeparator + AString2
end;

function ConcatIfNotEmpty(const AStrings: Array of String; const ASeparator: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := Low(AStrings) to High(AStrings) do
    Result := ConcatIfNotEmpty(Result, ASeparator, AStrings[I])
end;

function RemovePrefixIfPresent(const AString: String; const APrefix: String): String;
var
  S, P: Integer;
begin
  S := Length(AString);
  P := Length(APrefix);

  if (P > 0) and (S >= P) and SameText(Copy(AString, 1, P), APrefix) then
    Result := Copy(AString, Succ(P), Succ(S - P))
  else
    Result := AString;
end;

function AnsiSubStringPosition(const ASubString, AString: String): Integer;
begin
  Result := Pos(AnsiLowerCase(ASubString), AnsiLowerCase(AString));
end;

procedure ExtractNameAndAlias(const ANameString: String; AAlias: PString = nil; AName: PString = nil);
var
  LIdx: Integer;
begin
  LIdx := Pos('=', ANameString);
  if LIdx > 0 then begin
    if AAlias <> nil then
      AAlias^ := Copy(ANameString, 1, Pred(LIdx));
    if AName <> nil then
      AName^ := Copy(ANameString, Succ(LIdx), MaxInt);
  end else begin
    if (AAlias <> nil) then
      AAlias^ := ANameString;
    if (AName <> nil) then
      AName^ := ANameString;
  end;
end;

{
function GetNames(const ANames: String; AReadNames: Boolean): String;
var
  I, J: Integer;
  LName: String;
begin
  Result := '';
  I := 1;

  while I > 0 do begin
    J := PosEx(';', ANames, I);

    if J = 0 then begin
      LName := Copy(ANames, I, MaxInt);
      I := 0;
    end else begin
      LName := Copy(ANames, I, J - I);
      I := Succ(J);
    end;


    J := Pos('=', LName);
    if J > 0 then begin
      if AReadNames then
        LName := Copy(LName, Succ(J), MaxInt)
      else begin
        LName := Copy(LName, 1, Pred(J));
        if (LeftStr(LName, 1) = '[') then begin
          if RightStr(LName, 1) = ']' then
            LName := Copy(LName, 2, Length(LName) - 2)
          else begin
            J := Pos(']:', LName);
            if J > 0 then
              LName := Copy(LName, 2, J - 2);
          end;
        end else begin
          J := Pos(':', LName);
          if J > 0 then
            LName := Copy(LName, 1, Pred(J));
        end;
      end;
    end;

    Result := ConcatIfNotEmpty(Result, ';', LName);
  end;
end;

function FindNameByAlias(ANames, AReadName: String; var AWriteName: String): Boolean;
var
  I, L: Integer;
begin
  Result := False;

  ANames    := ANames + ';';

  L := Length(AReadName);
  AReadName := AReadName + ';';

  I := 0;
  repeat
    I := PosEx(AReadName, ANames, Succ(I));
    if I = 0 then Exit;

    if ((I = 1) or CharInSet(ANames[Pred(I)], [';', '=']))
    then begin
      if (I = 1) or (ANames[I] = ';') then
        AWriteName := Copy(AReadName, 1, Pred(L))
      else begin
        Dec(I);
        L := -1;
        while (I > 0) and (ANames[I] <> ';') do begin
          Dec(I); Inc(L);
        end;
        AWriteName := Copy(ANames, Succ(I), L);
      end;
      Result := True;
      Exit;
    end;
  until False;
end;
}

function InArray(const SArray, SElement: String; SDelimiter: Char): Boolean;
begin
	if SArray.IsEmpty or SElement.IsEmpty or (SArray = SDelimiter) then
    Result := False
  else if SArray = '*' then
    Result := True
  else
    Result := AnsiSubStringPosition(SDelimiter + SElement + SDelimiter, SDelimiter + SArray + SDelimiter) > 0
end;

function InArray(const SArray: String; IElement: Integer; SDelimiter: Char): Boolean;
begin
  Result := InArray(SArray, IntToStr(IElement), SDelimiter);
end;

function ArraysMerge(const AArrays: Array of String; SDelimiter: Char): String;
var
  LItem: TStringItem;
  L1Array, L2Array: String;
  I, L: Integer;
begin
  L := Length(AArrays) - 1;
  if L < 0 then Exit('');
  Result := AArrays[0];
  if (Result = '*') or (L = 0) then Exit;

  Result := Result.Trim(SDelimiter);
  for I := 1 to L do begin
    L2Array := AArrays[I];
    if (L2Array = '*') then Exit('*');

    L2Array := L2Array.Trim(SDelimiter);

    if (L2Array = '') then
    else if (Result = '') then
      Result := L2Array
    else begin
      L1Array := SDelimiter + Result + SDelimiter;
      for LItem in EnumStringItems(L2Array, SDelimiter) do
        if (not LItem.Value.IsEmpty) and (AnsiSubStringPosition(SDelimiter + LItem.Value + SDelimiter, L1Array) = 0) then
          Result := ConcatIfNotEmpty(Result, SDelimiter, LItem.Value);
    end;
  end;
end;

function ArraysJoin(const AArrays: Array of String; SDelimiter: Char = ','): String;
var
  LItem: TStringItem;
  I, L: Integer;
  L1Array, L2Array: String;
begin
  L := Length(AArrays) - 1;
  if L < 0 then Exit('');
  Result := AArrays[0].Trim(SDelimiter);
  if (L = 0) then Exit;

  for I := 1 to L do begin
    L2Array := AArrays[I].Trim(SDelimiter);
    if Result = '*' then
      Result := L2Array
    else if L2Array <> '*' then begin
      if (Result.IsEmpty) or (L2Array.IsEmpty) then
        Exit('')
      else begin
        L1Array := Result;
        L2Array := SDelimiter + L2Array + SDelimiter;
        Result := '';
        for LItem in EnumStringItems(L2Array, SDelimiter) do
          if AnsiSubStringPosition(SDelimiter + LItem.Value + SDelimiter, L2Array) > 0 then
            Result := ConcatIfNotEmpty(Result, SDelimiter, LItem.Value);
      end;
    end;
  end;
end;

function ArraysJoin(const SArray1: String; const SArray2: String; SDelimiter: Char = ','): String;
begin
  Result := ArraysJoin([SArray1, SArray2], SDelimiter);
end;

function ArraysAntiJoin(SFromArray: String; SThisArrays: Array of String; SDelimiter: Char): String;
var
  LItem: TStringItem;
  LThisArray, LThisArrayEx: String;
begin
  if SFromArray.IsEmpty or (SFromArray = SDelimiter) then
    Exit('');
  Result := SFromArray;

  for LThisArray in SThisArrays do begin
    if (LThisArray = '*') then
      Exit('')
    else if (Result = '*') or (LThisArray.IsEmpty) or (LThisArray = SDelimiter) then
      Continue
    else begin
      SFromArray := Result;
      Result     := '';
      LThisArrayEx := SDelimiter + LThisArray.Trim(SDelimiter) + SDelimiter;

      for LItem in EnumStringItems(SFromArray, SDelimiter) do
        if (not LItem.Value.IsEmpty) and (AnsiSubStringPosition(SDelimiter + LItem.Value + SDelimiter, LThisArrayEx) = 0) then
          Result := ConcatIfNotEmpty(Result, SDelimiter, LItem.Value);

      if Result.IsEmpty then Exit;
    end
  end;
end;

function ArraysAntiJoin(const SFromArray: String; SThisArray: String; SDelimiter: Char): String;
begin
  Result := ArraysAntiJoin(SFromArray, [SThisArray], SDelimiter)
end;

function ArraysPositiveJoin(S1Array: String; S2Array: String; SDelimiter: Char): Boolean;
var
  LItem: TStringItem;
begin
  if S1Array <> '*' then S1Array := S1Array.Trim(SDelimiter);
  if S2Array <> '*' then S2Array := S2Array.Trim(SDelimiter);

  if (S1Array.IsEmpty) or (S2Array.IsEmpty) then Exit(False);
  if (S1Array = '*') or (S2Array = '*') then Exit(True);

  S1Array := SDelimiter + S1Array + SDelimiter;
  for LItem in EnumStringItems(S2Array, SDelimiter) do
    if (not LItem.Value.IsEmpty) and (AnsiSubStringPosition(SDelimiter + LItem.Value + SDelimiter, S1Array) > 0) then
      Exit(True);
  Result := False;
end;

function ArrayQuote(const AArray: String; SDelimiter: Char; AQuote: Char): String;
var
  LItem: TStringItem;
begin
  if AArray.IsEmpty or (AArray = '*') then
    Exit(AArray);

  Result := '';
  for LItem in EnumStringItems(AArray, SDelimiter) do
    if (not LItem.Value.IsEmpty) then begin
      if not Result.IsEmpty then
        Result := Result + SDelimiter;
      Result := Result + QuoteString(LItem.Value, AQuote);
    end;
end;

{ TBigStringBuilder

constructor TBigStringBuilder.Create(const MaxBlockSize: Integer);
begin
  FMaxBlockSize := MaxBlockSize;
  FActualLength := 0;
end;

procedure TBigStringBuilder.Append(const value: string);
var
  l, ls: integer;
begin
  ls := System.Length(value);
  if ls = 0 then
    Exit;

  l := System.Length(FString);
  if l - FActualLength < ls then begin
    if (l + ls) > FMaxBlockSize then
      l := (FMaxBlockSize - ls) shl 1;
    SetLength(FString, FActualLength + ls + l shr 1);
  end;
  Move(PChar(value)^, PChar(@FString[FActualLength + 1])^, ls);
  Inc(FActualLength, ls);
end;

function TBigStringBuilder.ToString: string;
begin
  Result := Copy(FString, 1, FActualLength);
end;
}

{$REGION 'TStringItemsEnumerator'}
{ TStringItemsEnumerator }

constructor TStringItemsEnumerator.Create(const AString: string; const ADelimiter: Char; AQuotedItems: Boolean; AModes: TEnumStringModes);
begin
  FString       := AString;
  FDelimiter    := ADelimiter;
  FQuotedItems  := AQuotedItems;
  FModes        := AModes;

  FPosition     := 0;
  SkipBlanks;

  with FCurrent do begin
    Index := -1;
    Value := '';
    Eof   := (FPosition >= FString.Length);
  end;
end;

function TStringItemsEnumerator.GetCurrent: TStringItem;
begin
  Result := FCurrent;
end;

procedure TStringItemsEnumerator.SkipBlanks;
var
  LNextChar: Char;
begin
  while (FPosition < FString.Length) do begin
    LNextChar := FString[Succ(FPosition)];
    if (esmTrim in FModes) then
      if (LNextChar <> FDelimiter) and (LNextChar <= ' ') then begin
        Inc(FPosition);
        Continue;
      end;

    if (LNextChar <> FDelimiter) or (not (esmSkipEmpty in FModes)) then
      Break;
    Inc(FPosition);
  end;
end;

function TStringItemsEnumerator.MoveNext: Boolean;
var
  LNewPosition: Integer;
  LChar, LQuote: Char;

const
  SError_InvalidString: String = 'Invalid string item quotation: ';

begin
  if FCurrent.Eof then Exit(False);

  Inc(FCurrent.Index);

  LNewPosition := Succ(FPosition);
  if LNewPosition > FString.Length then begin
    with FCurrent do begin
      Eof   := True;
      Value := '';
    end;
    Exit(not (esmSkipEmpty in FModes));
  end;

  if FQuotedItems then begin
    LQuote := GetRightQuote(FString[LNewPosition], ['"', '''']);
    if LQuote <> #0 then begin
      FPosition     := Succ(LNewPosition);
      LNewPosition  := FPosition;
    end;
  end else
    LQuote := #0;

  if LQuote <> #0 then begin
    FCurrent.Value := '';
    while LNewPosition <= FString.Length do begin
      LChar := FString[LNewPosition];
      if (LChar = LQuote) then begin
        if (LNewPosition < FString.Length) then begin
          LChar := FString[Succ(LNewPosition)];
          if LChar = LQuote then begin
            FCurrent.Value := FCurrent.Value + Copy(FString, FPosition, LNewPosition - FPosition + 1);
            Inc(LNewPosition);
            FPosition := Succ(LNewPosition);
          end else if LChar <> FDelimiter then
            Raise Exception.Create(SError_InvalidString + Copy(FString, FPosition + 1, LNewPosition - FPosition))
        end;
        Break;
      end;
      Inc(LNewPosition);
    end;

    with FCurrent do begin
      Value := Copy(FString, FPosition, LNewPosition - FPosition);
      Eof   := (LNewPosition > FString.Length);
    end;
  end else begin
    LNewPosition := PosEx(FDelimiter, FString, LNewPosition);
    with FCurrent do begin
      if LNewPosition = 0 then begin
        Eof   := True;
        Value := Copy(FString, FPosition + 1, FString.Length - FPosition);
      end else begin
        Value := Copy(FString, FPosition + 1, LNewPosition - FPosition - 1);
      end;
    end;
  end;

  if (esmTrim in FModes) then
    FCurrent.Value := FCurrent.Value.TrimRight;

  FPosition := LNewPosition;
  if (not FCurrent.Eof) then begin
    SkipBlanks;
    FCurrent.Eof := (FPosition >= FString.Length);
  end;

  Result := True;
end;

{$ENDREGION}

{$IFDEF FASTSCRIPT_RTTI}
{ TfsFunctions_String }

constructor TfsFunctions_String.Create(AScript: TfsScript);
const
  SCategory: String = 'Строковые массивы';
begin
  inherited;

  with AScript do begin
    AddMethod('function ConcatIfNotEmpty(AStrings: Array of String; ASeparator: String): String', Call_String_Method, SCategory);

    AddMethod('function InArray(SArray, SElement: String; SDelimiter: Char = '',''): Boolean', Call_String_Method, SCategory);
    AddMethod('function ArraysMerge(AArrays: Array of String; SDelimiter: Char = '',''): String', Call_String_Method, SCategory);
    AddMethod('function ArraysJoin(S1Array: String; S2Array: String; SDelimiter: Char = '',''): String', Call_String_Method, SCategory);
    AddMethod('function ArraysAntiJoin(SFromArray: String; SThisArray: String; SDelimiter: Char = '',''): String', Call_String_Method, SCategory);
    AddMethod('function ArrayQuote(const AArray: String; SDelimiter: Char = '',''; AQuote: Char = ''''''): String', Call_String_Method, SCategory);

    AddMethod('function ExtractValue(const Values: String; Delimiter: Char = '',''; const Field_Type: TFieldType = ftString; const Field_Name: String = ''''): Variant', Call_String_Method, SCategory);
    AddMethod('function ExtractAndRemoveValue(var Values: String; Delimiter: Char = '',''; const Field_Type: TFieldType = ftString; const Field_Name: String = ''''): Variant', Call_String_Method, SCategory);

    AddMethod('function LeftStr(AString: String; ACount: Integer): String;', Call_String_Method, SCategory);
    AddMethod('function RightStr(AString: String; ACount: Integer): String;', Call_String_Method, SCategory);

    AddMethod('function SameText(const S1, S2: String): Boolean', Call_String_Method, SCategory);
    AddMethod('function PosEx(const ASubString: String; const AString: string; AOffset: Cardinal = 1): Integer', Call_String_Method, SCategory);
    AddMethod('function PosExRevert(const ASubString: String; const AString: String; AOffset: Cardinal = -1): Integer', Call_String_Method, SCategory);
    AddMethod('function QuotedStr(const AString: String): String;', Call_String_Method, SCategory);
    AddMethod('function QuoteString(const AString: String; AQuote: Char; AOnlyIfNeeded: Boolean = False): String;', Call_String_Method, SCategory);
    AddMethod('function Replicate(const AString: String; Count: Integer): String;', Call_String_Method, SCategory);

    AddEnum('TReplaceFlags', 'rfReplaceAll, rfIgnoreCase');
    AddMethod('function StringReplace(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): String', Call_String_Method, SCategory);

    AddMethod('function FormatString(AFormat: String; AVariables: TNamedVariants; AMetaCharacter: String): String;', Call_String_Method, SCategory);

    AddMethod('function Trim(S: String; C: String = ''''): String', Call_String_Method, 'ctString');
    AddMethod('function TrimRight(S: String; C: String = ''''): String', Call_String_Method, 'ctString');

    AddMethod('function NewGUIdString: String', Call_String_Method, 'ctString');
{$IFNDEF NEXTGEN}
    AddMethod('function RoundAt(AValue: String; APosition: Integer): String', Call_String_Method, 'ctString');
{$ENDIF}
  end;
end;

function TfsFunctions_String.Call_String_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  LString: String;
  LStrings: Array of String;
  LChars: TArray<Char>;
  LReplaceFlags: TReplaceFlags;
begin
  if MethodName = 'CONCATIFNOTEMPTY' then begin
    LStrings := Caller.Params[0];
    Result := ConcatIfNotEmpty(LStrings, VarToStr(Caller.Params[1]))
  end else if MethodName = 'INARRAY' then
    Result := InArray(VarToStr(Caller.Params[0]), VarToStr(Caller.Params[1]), VarToCharDef(Caller.Params[2], ','))
  else if MethodName = 'ARRAYSMERGE' then begin
    LStrings := Caller.Params[0];
    Result := ArraysMerge(LStrings, VarToCharDef(Caller.Params[1], ','))
  end else if MethodName = 'ARRAYSJOIN' then
    Result := ArraysJoin(VarToStr(Caller.Params[0]), VarToStr(Caller.Params[1]), VarToCharDef(Caller.Params[2], ','))
  else if MethodName = 'ARRAYSANTIJOIN' then
    Result := ArraysAntiJoin(VarToStr(Caller.Params[0]), VarToStr(Caller.Params[1]), VarToCharDef(Caller.Params[2], ','))
  else if MethodName = 'ARRAYQUOTE' then
    Result := ArrayQuote(VarToStr(Caller.Params[0]), VarToCharDef(Caller.Params[1], ','), VarToCharDef(Caller.Params[2], ''''))

  else if MethodName = 'EXTRACTVALUE' then begin
    LString := VarToStr(Caller.Params[0]);
    Result  := ExtractAndRemoveValue(LString, VarToCharDef(Caller.Params[1], ','), Caller.Params[2], VarToStr(Caller.Params[3]));
  end else if MethodName = 'EXTRACTANDREMOVEVALUE' then begin
    LString := VarToStr(Caller.Params[0]);
    Result  := ExtractAndRemoveValue(LString, VarToCharDef(Caller.Params[1], ','), Caller.Params[2], VarToStr(Caller.Params[3]));
    Caller.Params[0] := LString;

  end else if MethodName = 'LEFTSTR' then
    Result := LeftStr(Caller.Params[0], Caller.Params[1])
  else if MethodName = 'RIGHTSTR' then
    Result := RightStr(Caller.Params[0], Caller.Params[1])

  else if MethodName = 'SAMETEXT' then
    Result := SameText(Caller.Params[0], Caller.Params[1])
  else if MethodName = 'POSEX' then
    Result := PosEx(Caller.Params[0], Caller.Params[1], Caller.Params[2])
  else if MethodName = 'POSEXREVERT' then
    Result := PosExRevert(VarToChar(Caller.Params[0]), Caller.Params[1], IsNullOrEmpty(Caller.Params[2], -1))
  else if MethodName = 'QUOTEDSTR' then
    Result := QuotedStr(Caller.Params[0])
  else if MethodName = 'QUOTESTRING' then
    Result := QuoteString(Caller.Params[0], VarToChar(Caller.Params[1]), (Caller.Params[2] = True))
  else if MethodName = 'STRINGREPLACE' then begin
    VarToSet(Caller.Params[3], LReplaceFlags, SizeOf(LReplaceFlags));
    Result := StringReplace(Caller.Params[0], Caller.Params[1], Caller.Params[2], LReplaceFlags);
  end else if MethodName = 'FORMATSTRING' then begin
    Result := FormatString(Caller.Params[0], TNamedVariants.Create(Caller.Params[1], True), VarToStr(Caller.Params[2])[1]);
  end else if MethodName = 'TRIM' then begin
    LChars := VarToArrayOfChar(Caller.Params[1]);
    if Length(LChars) = 0 then
      Result := VarToStr(Caller.Params[0]).Trim
    else
      Result := VarToStr(Caller.Params[0]).Trim(LChars)
  end else if MethodName = 'TRIMRIGHT' then begin
    LChars := VarToArrayOfChar(Caller.Params[1]);
    if Length(LChars) = 0 then
      Result := VarToStr(Caller.Params[0]).TrimRight
    else
      Result := VarToStr(Caller.Params[0]).TrimRight(LChars)
  end else if MethodName = 'NEWGUIDSTRING' then
    Result := NewGUIdString
  else if MethodName = 'REPLICATE' then
    Result := DupeString(Caller.Params[0], Caller.Params[1])
{$IFNDEF NEXTGEN}
{$WARN SYMBOL_DEPRECATED OFF}
  else if MethodName = 'ROUNDAT' then
    Result := RoundAt(Caller.Params[0], Caller.Params[1])
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}
end;

{$ENDIF}

{ TStringParamItemsEnumerator }

constructor TStringParamItemsEnumerator.Create(const AString: String; AModes: TEnumParamsModes);
begin
  FString   := AString;
  FModes    := AModes;

  FPosition := 1;
  FLength   := FString.Length;

  with FCurrent do begin
    Gap   := '';
    Value := '';
    //Eof   := FString.IsEmpty;
  end;
end;

function TStringParamItemsEnumerator.GetCurrent: TStringParamItem;
begin
  Result := FCurrent
end;

function TStringParamItemsEnumerator.MoveNext: Boolean;
var
  NEW_Idx: Integer;
  LChar, LEndChar: Char;
begin
  if (FPosition > FLength) then
    Exit(False);

  FCurrent.Gap    := '';
  FCurrent.Format := '';
  while True do begin
    NEW_Idx := PosEx(':', FString, FPosition);
    if (NEW_Idx = 0) or (NEW_Idx = FLength) then begin
      //FCurrent.Eof    := True;

      if not (epmSkipGap in FModes) then
        FCurrent.Gap := FCurrent.Gap + Copy(FString, FPosition, FLength - FPosition + 1);

      FPosition := Succ(FLength);
      if epmSkipGap in FModes then Exit(False);

      FCurrent.Value  := '';
      Exit(True);
    end;

    Inc(NEW_Idx);
    LChar := FString[NEW_Idx];
    if LChar = ':' then
      Inc(NEW_Idx);

    if not (epmSkipGap in FModes) then
      FCurrent.Gap := FCurrent.Gap + Copy(FString, FPosition, NEW_Idx - FPosition - 1);

    FPosition := NEW_Idx;

    if LChar <> ':' then begin
      LEndChar := GetRightQuote(LChar, ['"', '[', '''', '{']);
      if LEndChar <> #0 then begin
        NEW_Idx := PosEx(LEndChar, FString, FPosition);
        if NEW_Idx = 0 then begin
          FPosition := Succ(FLength);
          if epmSkipGap in FModes then Exit(False);

          FCurrent.Gap   := FCurrent.Gap + Copy(FString, FPosition - 1, FLength - FPosition + 2);
          FCurrent.Value := '';
          Exit(True);
        end else begin
          Inc(NEW_Idx);
          if NEW_Idx = FPosition + 2 then begin
            if not (epmSkipGap in FModes) then
              FCurrent.Gap := FCurrent.Gap + ':' + LChar + LEndChar;
          end else begin
            FCurrent.Value  := Copy(FString, FPosition + 1, NEW_Idx - FPosition - 2);
            FPosition       := NEW_Idx;
            if (epmFormat in FModes) and FindPos('|', FCurrent.Value, NEW_Idx) then begin
              FCurrent.Format := Copy(FCurrent.Value, Succ(NEW_Idx), FCurrent.Value.Length);
              FCurrent.Value  := LeftStr(FCurrent.Value, Pred(NEW_Idx));
            end;
            Exit(True);
          end;
        end;
      end else begin
        while (NEW_Idx <= FLength) and CharInSet(LChar, ['a'..'z', 'A'..'Z', '0'..'9', '_']) do begin
          Inc(NEW_Idx);
          if (NEW_Idx <= FLength) then
            LChar := FString[NEW_Idx];
        end;

        if FPosition = NEW_Idx then begin
          if not (epmSkipGap in FModes) then
            FCurrent.Gap := FCurrent.Gap + ':' + LChar;
          Inc(NEW_Idx);
        end else begin
          FCurrent.Value  := Copy(FString, FPosition, NEW_Idx - FPosition);
          FPosition       := NEW_Idx;
          Exit(True);
        end;
      end;
    end;

    FPosition := NEW_Idx;
  end;
end;

{ TSQLParamItemsEnumerator }

constructor TSQLParamItemsEnumerator.Create(const ACommandText: String; APrefix: Char; AComments: TScriptCommentMethods; AQuotes: TSysCharSet; ALiterals: TSysCharSet);
begin
  FString       := ACommandText;
  FPrefix       := APrefix;
  FComments     := AComments;
  FQuotes       := AQuotes;
  FLiterals     := ALiterals;

  FPosition := 0;
  FLength   := FString.Length;
  MoveToNextChar;

  with FCurrent do begin
//    Index := -1;
    Gap   := '';
    Quote := #0;
    Value := '';
//    Eof   := FString.IsEmpty;
  end;
end;

function TSQLParamItemsEnumerator.GetCurrent: TSQLParamItem;
begin
  Result := FCurrent
end;

procedure TSQLParamItemsEnumerator.MoveToNextChar(AIncPosition: Boolean = True);
begin
  if AIncPosition then
    Inc(FPosition);

  if FPosition <= FLength then
    FCurrChar     := FString[FPosition]
  else begin
    FCurrChar     := #0;
    FCurrent.Eof  := True;
  end;

  if FPosition < FLength then
    FNextChar := FString[Succ(FPosition)]
  else
    FNextChar := #0
end;

function TSQLParamItemsEnumerator.MoveNext: Boolean;
var
  LPosition: Integer;
  LCurrComment: TScriptCommentMethod;

  function NameDelimiter(AChar: Char): Boolean;
  begin
    Result := CharInSet(AChar, [' ', '!', '$', '?', ')', '<', '>', '=', '+', '-', '*', '/', '\', '%', '^', '&', '|', ',', ';', '''', '"', '`', #13, #10, #0]);
  end;

  procedure SkipText(AIncludeCurrent: Boolean);
  begin
    FCurrent.Gap := FCurrent.Gap + Copy(FString, LPosition, FPosition - LPosition + IfThen(AIncludeCurrent, 1, 0));
    LPosition := Succ(FPosition);
  end;

var
  LEndChar : Char;
begin
  if FCurrent.Eof then
    Exit(False);

  FCurrent.Gap  := '';
  LPosition     := FPosition;
  LCurrComment  := scmNone;

  while True {(FPosition <= FLength)} do begin
    case LCurrComment of
      scmLattice, scmDoubleMinus, scmDoubleSlash:
        begin
          if CharInSet(FCurrChar, [#10, #13, #0]) then
            LCurrComment := scmNone;
          MoveToNextChar;
          Continue;
        end;
      scmSlashRange:
        begin
          if (FCurrChar = '*') and (FNextChar = '/') then begin
            LCurrComment := scmNone;
            Inc(FPosition);
          end;
          MoveToNextChar;
          Continue;
        end;
      scmBracketRange:
        begin
          if (FCurrChar = '*') and (FNextChar = ')') then begin
            LCurrComment := scmNone;
            Inc(FPosition);
          end;
          MoveToNextChar;
          Continue;
        end;
      scmBraces:
        begin
          if FCurrChar = '}' then
            LCurrComment := scmNone;
          MoveToNextChar;
          Continue;
        end;
    end;

    case FCurrChar of
      '#':
        if scmLattice in FComments then
        begin
          LCurrComment := scmLattice;
          MoveToNextChar;
          Continue;
        end;
      '{':
        if (scmBraces in FComments) then
        begin
          LCurrComment := scmBraces;
          MoveToNextChar;
          Continue;
        end;
      '-':
        if (scmDoubleMinus in FComments) and (FNextChar = '-') then
        begin
          LCurrComment := scmDoubleMinus;
          Inc(FPosition);
          MoveToNextChar;
          Continue;
        end;
      '/':
        if (scmDoubleSlash in FComments) and (FNextChar = '/') then
        begin
          LCurrComment := scmDoubleSlash;
          Inc(FPosition);
          MoveToNextChar;
          Continue;
        end else if (scmSlashRange in FComments) and (FNextChar = '*') then
        begin
          LCurrComment := scmSlashRange;
          Inc(FPosition);
          MoveToNextChar;
          Continue;
        end;
      '(':
        if (scmBracketRange in FComments) and (FNextChar = '*') then
        begin
          LCurrComment := scmBracketRange;
          Inc(FPosition);
          MoveToNextChar;
          Continue;
        end;
      #0:
        begin
          SkipText(False);
          FCurrent.Quote := #0;
          FCurrent.Value := '';
          FCurrent.Eof   := True;
          Exit(True);
        end
      else
        if FCurrChar = FPrefix then
          if (FNextChar = FPrefix) then begin
            Inc(FPosition);
            SkipText(False);
            MoveToNextChar;
            Continue;
          end else begin
            SkipText(False);
            MoveToNextChar;
            FCurrent.Quote := GetRightQuote(FCurrChar, FQuotes);
            if (FCurrent.Quote <> #0) then begin
              Inc(FPosition);
              if ParseEOQ(FCurrent.Quote, FString, FPosition, FCurrent.Value, []) then begin
                if FCurrent.Value.IsEmpty then begin
                  Dec(LPosition);
                  SkipText(False);
                end else begin
                  LPosition     := FPosition;
                  FCurrent.Eof  := (FPosition > FLength);
                  if not FCurrent.Eof then
                    MoveToNextChar(False);
                  Exit(True);
                end;
              end else begin
                Dec(LPosition);
                FPosition := FLength;
                SkipText(True);
                FCurrent.Quote  := #0;
                FCurrent.Value  := '';
                FCurrent.Eof    := True;
                Exit(True);
              end;
            end else begin
              while not NameDelimiter(FCurrChar) do
                MoveToNextChar;
              if LPosition = FPosition then
                Dec(LPosition)
              else begin
                FCurrent.Value  := Copy(FString, LPosition, FPosition - LPosition);
                FCurrent.Eof    := (FPosition > FLength);
                Exit(True);
              end;
            end;
          end;
        // end if
      // end case else
    end;

    if CharInSet(FCurrChar, FLiterals) then begin
      LEndChar   := GetRightQuote(FCurrChar);
      repeat
        MoveToNextChar;
        if FCurrChar = LEndChar then
          if FNextChar = LEndChar then begin
            MoveToNextChar;
//            SkipText(False);
//            Inc(FPosition)
          end else begin
            MoveToNextChar;
            Break;
          end;
      until (FPosition > FLength);
    end else
      MoveToNextChar;
  end;
end;

{ TStringNamedItemsEnumerator }

constructor TStringNamedItemsEnumerator.Create(const AString: String; const ADelimiter: Char; AWithCastAs: Boolean);
begin
  FString       := AString;
  FDelimiter    := ADelimiter;
  FWithCastAs   := AWithCastAs;

  FPosition     := 1;
  with FCurrent do begin
    Index   := -1;
    Name    := '';
    CastAs  := '';
    Value   := '';
    Eof     := FString.IsEmpty;
  end;
end;

function TStringNamedItemsEnumerator.GetCurrent: TStringNamedItem;
begin
  Result := FCurrent;
end;

function TStringNamedItemsEnumerator.MoveNext: Boolean;
var
  LQuote: Char;
  LCastAsPosition, LEQPosition, LDelimiterPosition: Integer;
  LNextChars: TSysCharSet;
const
  SError_InvalidString: String = 'Invalid named variables string: ';
begin
  if FCurrent.Eof then
    Exit(False);
  Inc(FCurrent.Index);

  LQuote := GetRightQuote(FString[FPosition], ['"', '[', '''', '{']);
  if LQuote = #0 then begin
    LEQPosition := PosEx('=', FString, FPosition);
    if (LEQPosition = 0) or (LEQPosition = FPosition) then
      Raise Exception.Create(SError_InvalidString + FString);
    FCurrent.Name := Copy(FString, FPosition, LEQPosition - FPosition);
    FPosition := LEQPosition + 1;
    if FWithCastAs then begin
      LCastAsPosition := PosExRevert(':', FCurrent.Name);
      if LCastAsPosition > 0 then begin
        FCurrent.CastAs := Copy(FCurrent.Name, LCastAsPosition + 1, FCurrent.Name.Length - LCastAsPosition);
        FCurrent.Name := Copy(FCurrent.Name, 1, LCastAsPosition - 1);
      end else
        FCurrent.CastAs := '';
    end;
  end else begin
    if FWithCastAs then
      LNextChars := [':', '=']
    else
      LNextChars := ['='];
    Inc(FPosition);
    if not ParseEOQ(LQuote, FString, FPosition, FCurrent.Name, LNextChars) then
      Raise Exception.Create(SError_InvalidString + FString);

    if FWithCastAs and (FPosition <= FString.Length) and (FString[FPosition] = ':') then begin
      LEQPosition := PosEx('=', FString, FPosition + 1);
      if (LEQPosition = 0) then
        Raise Exception.Create(SError_InvalidString + FString);
      FCurrent.CastAs := Copy(FString, FPosition + 1, LEQPosition - FPosition - 1);
      FPosition := LEQPosition + 1;
    end else begin
      Inc(FPosition);
      FCurrent.CastAs := '';
    end;
  end;

  if (FPosition <= FString.Length) then
    LQuote := GetRightQuote(FString[FPosition], ['"', '[', '''', '{'])
  else
    LQuote := #0;

  if LQuote = #0 then begin
    LDelimiterPosition := PosEx(FDelimiter, FString, FPosition);
    if (LDelimiterPosition = 0) then begin
      LDelimiterPosition := FString.Length + 1;
      FCurrent.Eof := True;
    end;
    FCurrent.Value := Copy(FString, FPosition, LDelimiterPosition - FPosition);
    FPosition := LDelimiterPosition + 1;
  end else begin
    Inc(FPosition);
    if not ParseEOQ(LQuote, FString, FPosition, FCurrent.Value, [FDelimiter, #0]) then
      Raise Exception.Create(SError_InvalidString + FString);
    Inc(FPosition);
    FCurrent.Eof := (FPosition > FString.Length);
  end;

  Result := True;
end;

{ TStringNameItemsEnumerator }

constructor TStringNameItemsEnumerator.Create(const AString: String; const ADelimiter: Char; AWithCastAs: Boolean);
begin
  FString       := AString;
  FDelimiter    := ADelimiter;
  FWithCastAs   := AWithCastAs;

  FPosition     := 1;
  with FCurrent do begin
    Index     := -1;
    NameLeft  := '';
    CastAs    := '';
    NameRight := '';
    Eof       := FString.IsEmpty;
  end;
end;

function TStringNameItemsEnumerator.GetCurrent: TStringNameItem;
begin
  Result := FCurrent;
end;

{$WARNINGS OFF}
function TStringNameItemsEnumerator.MoveNext: Boolean;
var
  LCastAsPosition, LEQPosition, LDelimiterPosition: Integer;
  LNextChars: TSysCharSet;
  LPosition: Integer;

  procedure FindDelimeter;
  begin
    LDelimiterPosition := PosEx(FDelimiter, FString, FPosition);
    if (LDelimiterPosition = 0) then begin
      LDelimiterPosition := FString.Length + 1;
      FCurrent.Eof := True;
    end;
  end;
const
  SError_InvalidString: String = 'Invalid names string: ';
label
  LB_EXIT;
begin
  if FCurrent.Eof then
    Exit(False);
  Inc(FCurrent.Index);
  LPosition := FPosition;

  if FString[FPosition] <> '[' then begin
    LDelimiterPosition := PosEx(FDelimiter, FString, FPosition);
    FindDelimeter;

    LEQPosition := PosEx('=', FString, FPosition);
    if (LEQPosition = FPosition) or (LEQPosition = LDelimiterPosition - 1) then
      Raise Exception.Create(SError_InvalidString + FString);

    if (LEQPosition > 0) and (LEQPosition < LDelimiterPosition) then begin
      FCurrent.NameLeft := Copy(FString, FPosition, LEQPosition - FPosition);
      FPosition := LEQPosition + 1;
      if FWithCastAs then begin
        LCastAsPosition := PosExRevert(':', FCurrent.NameLeft);
        if LCastAsPosition > 0 then begin
          FCurrent.CastAs := Copy(FCurrent.NameLeft, LCastAsPosition + 1, FCurrent.NameLeft.Length - LCastAsPosition);
          FCurrent.NameLeft := Copy(FCurrent.NameLeft, 1, LCastAsPosition - 1);
        end else
          FCurrent.CastAs := '';
      end;
    end else begin
      FCurrent.NameLeft   := Copy(FString, FPosition, LDelimiterPosition - FPosition);
      FCurrent.NameRight  := FCurrent.NameLeft;
      FCurrent.CastAs     := '';
      FPosition := LDelimiterPosition + 1;
      goto LB_EXIT;
    end;
  end else begin
    if FWithCastAs then
      LNextChars := [':', '=', FDelimiter, #0]
    else
      LNextChars := ['=', FDelimiter, #0];
    Inc(FPosition);
    if not ParseEOQ(']', FString, FPosition, FCurrent.NameLeft, LNextChars) then
      Raise Exception.Create(SError_InvalidString + FString);

    FCurrent.Eof := (FPosition > FString.Length);
    if FCurrent.Eof or (FString[FPosition] = FDelimiter) then begin
      FCurrent.NameRight  := FCurrent.NameLeft;
      FCurrent.CastAs     := '';
      Inc(FPosition);
      goto LB_EXIT;
    end;

    if FWithCastAs and (FString[FPosition] = ':') then begin
      FindDelimeter;
      LEQPosition := PosEx('=', FString, FPosition + 1);
      if (LEQPosition = 0) or (LEQPosition = FPosition + 1) or (LEQPosition > LDelimiterPosition) or (LEQPosition = LDelimiterPosition - 1) then
        Raise Exception.Create(SError_InvalidString + FString);
      FCurrent.CastAs := Copy(FString, FPosition + 1, LEQPosition - FPosition - 1);
      FPosition := LEQPosition + 1;
    end else begin
      Inc(FPosition);
      FCurrent.CastAs := '';
      if (not FCurrent.Eof) and (FString[FPosition] <> '[') then
        FindDelimeter;
    end;
  end;

  if (FPosition > FString.Length) or (FString[FPosition] <> '[') then begin
    FCurrent.NameRight := Copy(FString, FPosition, LDelimiterPosition - FPosition);
    FPosition := LDelimiterPosition + 1;
  end else begin
    Inc(FPosition);
    if not ParseEOQ(']', FString, FPosition, FCurrent.NameRight, [FDelimiter, #0]) then
      Raise Exception.Create(SError_InvalidString + FString);
    Inc(FPosition);
    FCurrent.Eof := (FPosition > FString.Length);
  end;

LB_EXIT:
  FCurrent.NameFull := Copy(FString, LPosition, FPosition - LPosition - 1);
  Result := True;
end;
{$WARNINGS ON}

//{ TStringHelper }
//
//function TStringHelper.GetLength: Integer;
//begin
//  Result := System.Length(Self);
//end;
//
//function TStringHelper.IsEmpty: Boolean;
//begin
//  Result := (System.Length(Self) = 0)
//end;

{ TStringsHelper }

procedure TStringsHelper.AssignStrings(ATarget: TStrings);
begin
  BeginUpdate;
  try
    Clear;
    if Assigned(ATarget) then
      AddStrings(ATarget);
  finally
    EndUpdate;
  end;
end;

function TStringsHelper.AssignStringsIfDifferent(ATarget: TStrings): Boolean;
begin
  Result := not IsEqualStrings(ATarget);
  if Result then
    AssignStrings(ATarget)
end;

function TStringsHelper.IsEqualStrings(ATarget: TStrings): Boolean;
var
  I: Integer;
  LCount: Integer;
  LTargetCount: Integer;
begin
  if Assigned(Self) then
    LCount := Count
  else
    LCount := 0;

  if Assigned(ATarget) then
    LTargetCount := ATarget.Count
  else
    LTargetCount := 0;

  if LCount <> LTargetCount then Exit(False);
  for I := 0 to LCount - 1 do
    if Strings[I] <> ATarget.Strings[I] then
      Exit(False);

  Result := True;
end;

function TStringsHelper.SetTextIfDifferent(const AText: String): Boolean;
begin
  Result := (Text <> AText);
  if Result then
    Text := AText;
end;

initialization
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_String);
{$ENDIF}

end.
