unit SBaseExpression;
{$I config.inc}

interface

uses DB, DBCommon, DSIntf
//  , MidasLib
{$IFDEF WIN64}
  , Windows
{$ENDIF}
;

{ Linear exression tree}

const
  CANEXPRVERSION     = 2;

{ Field Types (Logical) - Originally from BDE.PAS }

  fldUNKNOWN         = 0;
  fldZSTRING         = 1;               { Null terminated string }
  fldDATE            = 2;               { Date     (32 bit) }
  fldBLOB            = 3;               { Blob }
  fldBOOL            = 4;               { Boolean  (16 bit) }
  fldINT16           = 5;               { 16 bit signed number }
  fldINT32           = 6;               { 32 bit signed number }
  fldFLOAT           = 7;               { 64 bit floating point }
  fldBCD             = 8;               { BCD }
  fldBYTES           = 9;               { Fixed number of bytes }
  fldTIME            = 10;              { Time        (32 bit) }
  fldTIMESTAMP       = 11;              { Time-stamp  (64 bit) }
  fldUINT16          = 12;              { Unsigned 16 bit integer }
  fldUINT32          = 13;              { Unsigned 32 bit integer }
  fldFLOATIEEE       = 14;              { 80-bit IEEE float }
  fldVARBYTES        = 15;              { Length prefixed var bytes }
  fldLOCKINFO        = 16;              { Look for LOCKINFO typedef }
  fldCURSOR          = 17;              { For Oracle Cursor type }
  fldINT64           = 18;              { 64 bit signed number }
  fldUINT64          = 19;              { Unsigned 64 bit integer }
  fldADT             = 20;              { Abstract datatype (structure) }
  fldARRAY           = 21;              { Array field type }
  fldREF             = 22;              { Reference to ADT }
  fldTABLE           = 23;              { Nested table (reference) }
  fldDATETIME        = 24;              { Datetime structure for DBExpress }
  fldFMTBCD          = 25;              { BCD Variant type: required by Midas, same as BCD for DBExpress}
  fldWIDESTRING      = 26;              { UCS2 null terminated string }

  MAXLOGFLDTYPES     = 27;              { Number of logical fieldtypes }

{ Additional (non-BDE fieldtypes }
  fldUNICODE         = $1007;          { Unicode }
  fldINT8            = 28;           { 8 bit signed number as defined in alctypes.h }
  fldUINT8           = 29;           { Unsigned 8 bit integer (Byte) as defined in alctypes.h }
  fldSINGLE          = 27;              { 32 bit floating point }
  fldDATETIMEOFFSET  = 36;              { DatetimeOffset structure for DBExpress }


{ Sub Types (Logical) }

{ fldFLOAT subtype }

  fldstMONEY         = 21;              { Money }

{ fldBLOB subtypes }

  fldstMEMO          = 22;              { Text Memo }
  fldstBINARY        = 23;              { Binary data }
  fldstFMTMEMO       = 24;              { Formatted Text }
  fldstOLEOBJ        = 25;              { OLE object (Paradox) }
  fldstGRAPHIC       = 26;              { Graphics object }
  fldstDBSOLEOBJ     = 27;              { dBASE OLE object }
  fldstTYPEDBINARY   = 28;              { Typed Binary data }
  fldstACCOLEOBJ     = 30;              { Access OLE object }
  fldstHMEMO         = 33;              { CLOB }
  fldstHBINARY       = 34;              { BLOB }
  fldstADTNESTEDTABLE= 35;              { ADT nested table }
  fldstBFILE         = 36;              { BFILE }

{ fldZSTRING subtype }

  fldstPASSWORD      = 1;               { Password }
  fldstFIXED         = 31;              { CHAR type }
  fldstUNICODE       = 32;              { Unicode }
  fldstGUID          = 38;              { GUID }

type
  ppCANExpr = ^pCANExpr;
  pCANExpr  = ^CANExpr;
  CANExpr   = packed record             { Expression Tree }
    iVer            : Word;             { Version tag of expression. }
    iTotalSize      : Word;             { Size of this structure }
    iNodes          : Word;             { Number of nodes }
    iNodeStart      : Word;             { Starting offet of Nodes in this }
    iLiteralStart   : Word;             { Starting offset of Literals in this }
  end;

  TDateTimeAlias = type TDateTime;
  TDateTimeRec = packed record
    case Integer of
      0: (Date: Integer);
      1: (Time: Integer);
      2: (DateTime: TDateTimeAlias);
  end;

{ TCanExprEvaluator }

  TEvalFunction = (efUnknown, efSum, efMin, efMax, efAvg, efCount, efUpper,
    efLower, efSubString, efTrim, efTrimLeft, efTrimRight, efYear, efMonth,
    efDay, efHour, efMinute, efSecond, efGetDate, efDate, efTime, efIIF);

  TCanExprEvaluator = class
  private
    FExpr: PCanExpr;
    FOpList: Variant;

    { Compare }
    function CompareOperandsEx(const Op1, Op2: Variant; CaseIns: Boolean; PartialLen: Word; CompareLike: Boolean): Integer;
    function CompareOperandsEx2(var Op1, Op2: Variant; var iFldType1, iFldLen1, iFldType2, iFldLen2: Integer): Integer;
    function CompareOperands(const Op1, Op2: Variant): Integer;
    { Existense }
    function Exists(const Op1, Op2: Variant): Boolean;
    { Operand extraction }
    function GetConstantValue(DataType, Size, Offset: Word; var iFldType, iFldLen: Integer): Variant;
    function GetFieldValue(FieldNo: Word; Offset: Word; var iFldType, iFldLen: Integer): Variant;
    function GetFuncValue(Func: TEvalFunction; Offset: Word; var iFldType, iFldLen: Integer): Variant;
    function GetFuncValueInternal(Func: TEvalFunction; Args: Variant; var iFldType, iFldLen: Integer): Variant;
    function GetOperandValue(Offset: Word; var iFldType, iFldLen: Integer): Variant;
  protected
    { Abstract functions }
    function DoGetFieldValue(FieldNo: Word; const FieldName: string; var iFldType, iFldLen: Integer): Variant; virtual;
    procedure DoFirst; virtual;
    procedure DoNext; virtual;
    function DoGetEOF: Boolean; virtual;
    procedure SetData(AValue: PCanExpr); virtual;
  public
    destructor Destroy; override;

    //Elohim Meth
    function IsSimpleExpression(var AFieldNo: Integer; var AValue: Variant; var ACaseInsensitive: Boolean): Boolean;
    //Elohim Meth
    property Data: PCanExpr read FExpr write SetData;
    function Evaluate: Variant;
  end;

  PCanExprEvaluator = ^TCanExprEvaluator;

  TDataSetExprEvaluator = class(TCanExprEvaluator)
  private
    FDataSet: TDataSet;
    FOldValues: Boolean;
  protected
    function DoGetFieldValue(FieldNo: Word; const FieldName: string; var FldType, FldLen: Integer): Variant; override;
  public
    constructor Create(ADataSet: TDataSet; AOldValues: Boolean = False);
  end;
  PDataSetExprEvaluator = ^TDataSetExprEvaluator;

const
  efAggregates  = [efSum, efMin, efMax, efAvg, efCount];

implementation

uses
  SysUtils, AnsiStrings, Math, Variants, FMTBcd, SBaseStringFunctions, SBaseVariantFunctions;

type

{ NODE definitions including misc data structures }

  CANOp = Integer;
  NODECls = Integer;

  pCANHdr = ^CANHdr;
  CANHdr = packed record                { Header part common to all     (*) }
    nodeClass       : NODECls;
    canOp           : CANOp;
  end;

  pCANUnary = ^CANUnary;
  CANUnary = packed record              { Unary Node                    (*) }
    nodeClass       : NODECls;
    canOp           : CANOp;
    iOperand1       : Word;             { Byte offset of Operand node }
  end;

  pCANBinary = ^CANBinary;
  CANBinary = packed record             { Binary Node                   (*) }
    nodeClass       : NODECls;
    canOp           : CANOp;
    iOperand1       : Word;             { Byte offset of Op1 }
    iOperand2       : Word;             { Byte offset of Op2 }
  end;

  pCANField = ^CANField;
  CANField = packed record              { Field }
    nodeClass       : NODECls;
    canOp           : CANOp;
    iFieldNum       : Word;
    iNameOffset     : Word;             { Name offset in Literal pool }
  end;

  pCANConst = ^CANConst;
  CANConst = packed record              { Constant }
    nodeClass       : NODECls;
    canOp           : CANOp;
    iType           : Word;             { Constant type. }
    iSize           : Word;             { Constant size. (in bytes) }
    iOffset         : Word;             { Offset in the literal pool. }
  end;

  pCANTuple = ^CANTuple;
  CANTuple = packed record              { Tuple (record) }
    nodeClass       : NODECls;
    canOp           : CANOp;
    iSize           : Word;             { Record size. (in bytes) }
  end;

  pCANContinue = ^CANContinue;
  CANContinue = packed record           { Break Node                    (*) }
    nodeClass       : NODECls;
    canOp           : CANOp;
    iContOperand    : Word;             { Continue if operand is true. }
  end;

  pCANCompare = ^CANCompare;
  CANCompare = packed record            { Extended compare Node (text fields) (*) }
    nodeClass       : NODECls;
    canOp           : CANOp;            { canLIKE, canEQ }
    bCaseInsensitive : WordBool;        { 3 val: UNKNOWN = "fastest", "native" }
    iPartialLen     : Word;             { Partial fieldlength (0 is full length) }
    iOperand1       : Word;             { Byte offset of Op1 }
    iOperand2       : Word;             { Byte offset of Op2 }
  end;

  pCANFunc = ^CANFunc;
  CANFunc = packed record               { Function }
    nodeClass       : NODECls;
    canOp           : CANOp;
    iNameOffset     : Word;             { Name offset in Literal pool }
    iElemOffset     : Word;             { Offset of first List Element in Node pool }
  end;

  pCANListElem = ^CANListElem;
  CANListElem = packed record           { List Element }
    nodeClass       : NODECls;
    canOp           : CANOp;
    iOffset         : Word;             { Arg offset in Node pool }
    iNextOffset     : Word;             { Offset in Node pool of next ListElem or 0 if end of list }
  end;

{This is the node to be used to pass User defined functions }

const
  iLangSQL           = 0;               { Common SQL dialect }
  iDbaseExpr         = 2;               { This is also the driver ID for dBASE }

type
  pCANUdf = ^CANUdf;
  CANUdf = packed record                { A user defined function }
    nodeClass       : NODECls;
    canOp           : CANOp;
    iOffSzFuncName  : Word;             { Offset in literal pool to Function Name string(0 terminated) }
    iOperands       : Word;             { Byte offset of Operands (concatenated using canCONT) }
    iDrvDialect     : Word;             { Driver Dialect ID for UDF string supplied }
    iOffSzUDF       : Word;             { Offset in literal pool to UDF string (0 terminated) }
  end;

  pCANList = ^CANList;
  CANList = packed record           { List of Constants }
    nodeClass       : NODECls;
    canOp           : CANOp;
    iType           : Word;            { Constant type. }
    iTotalSize      : Word;            { Total list size; }
    iElemSize       : Word;            { Size of each elem for fix-width types }
    iElems          : Word;            { Number of elements in list }
    iOffset         : Word;            { Offset in the literal pool to first elem. }
  end;

  pCANNode = ^CANNode;
  CANNode = packed record
    case Integer of
      0: (canHdr      : CANHdr);
      1: (canUnary    : CANUnary);
      2: (canBinary   : CANBinary);
      3: (canField    : CANField);
      4: (canConst    : CANConst);
      5: (canTuple    : CANTuple);
      6: (canContinue : CANContinue);
      7: (canCompare  : CANCompare);
      8: (canList     : CANList);
      9: (canFunc     : CANFunc);
     10: (canListElem : CANListElem);
  end;

procedure ZeroMem(pBuff: Pointer; Count: Integer);
{$IFDEF WIN64}
asm
  mov rcx,rdx
  sar rcx,4
  js @@done

  xchg rdx,rdi
  // push rdi
  mov rdi,rax
  xor rax,rax
  rep stosq
  mov rcx,rdi
  and rcx,7
  rep stosb
  xchg rdx,rdi
  // pop rdi
@@done:
end;
{$ELSE}
{$IFDEF WIN32}
asm
  mov ecx,edx
  sar ecx,2
  js @@done

  push edi
  mov edi,eax
  xor eax,eax
  rep stosd
  mov ecx,edx
  and ecx,3
  rep stosb
  pop edi
@@done:
end;
{$ENDIF}
{$ENDIF}

procedure NotSupported;
begin
  raise Exception.Create('NotSupported');
end;

function IsInteger(DataType: Integer): Boolean;
begin
  Result := DataType in [fldINT16, fldUINT16, fldINT32, fldUINT32, fldINT64, fldUINT64];
end;

function ValuePtrToDateTime(AFieldType: Integer; AValue: Pointer): TDateTime;
var
  TS: TTimeStamp;
begin
  case AFieldType of
    fldDATE:
      begin
        TS.Time := 0;
        TS.Date := PInteger(AValue)^;
      end;
    fldTIME:
      begin
        TS.Time := PInteger(AValue)^;
        TS.Date := DateDelta;
      end;
    fldTIMESTAMP:
      try
        TS := MSecsToTimeStamp(PDouble(AValue)^);
      except
        TS.Time := 0;
        TS.Date := 0;
      end;
    else
      raise Exception.Create('DBERR_NOTSUPPORTED');
  end;
  Result := TimeStampToDateTime(TS);
end;


function IsNumeric(DataType: Integer): Boolean;
begin
  Result := DataType in [
    fldINT16, fldUINT16, fldINT32, fldUINT32, fldINT64,
    fldUINT64, fldFLOAT, fldBCD, fldFMTBCD];
end;

function IsString(DataType: Integer): Boolean;
begin
  Result := (DataType = fldZSTRING) or (DataType = fldWIDESTRING) or (DataType = fldUNICODE);
end;

function IsBCD(DataType: Integer): Boolean;
begin
  Result := DataType in [fldBCD, fldFMTBCD]; 
end;

const
  NumericLen    = 10;

function StrToEvalFunction(const Func: string): TEvalFunction;
begin
  if (CompareText(Func, 'SUM') = 0) then
    Result := efSum
  else if (CompareText(Func, 'MIN') = 0) then
    Result := efMin
  else if (CompareText(Func, 'MAX') = 0) then
    Result := efMax
  else if (CompareText(Func, 'AVG') = 0) then
    Result := efAvg
  else if (CompareText(Func, 'COUNT') = 0) then
    Result := efCount
  else if (CompareText(Func, 'COUNT(*)') = 0) then
    Result := efCount
  else if (CompareText(Func, 'UPPER') = 0) then
    Result := efUpper
  else if (CompareText(Func, 'LOWER') = 0) then
    Result := efLower
  else if (CompareText(Func, 'SUBSTRING') = 0) then
    Result := efSubString
  else if (CompareText(Func, 'TRIM') = 0) then
    Result := efTrim
  else if (CompareText(Func, 'TRIMLEFT') = 0) then
    Result := efTrimLeft
  else if (CompareText(Func, 'TRIMRIGHT') = 0) then
    Result := efTrimRight
  else if (CompareText(Func, 'YEAR') = 0) then
    Result := efYear
  else if (CompareText(Func, 'MONTH') = 0) then
    Result := efMonth
  else if (CompareText(Func, 'DAY') = 0) then
    Result := efDay
  else if (CompareText(Func, 'HOUR') = 0) then
    Result := efHour
  else if (CompareText(Func, 'MINUTE') = 0) then
    Result := efMinute
  else if (CompareText(Func, 'SECOND') = 0) then
    Result := efSecond
  else if (CompareText(Func, 'GETDATE') = 0) then
    Result := efGetDate
  else if (CompareText(Func, 'DATE') = 0) then
    Result := efDate
  else if (CompareText(Func, 'TIME') = 0) then
    Result := efTime
  else if (CompareText(Func, 'IIF') = 0) then
    Result := efIIF
  else
    Result := efUnknown;
end;

procedure PreferInteger(iFldType1, iFldLen1, iFldType2, iFldLen2: Integer;
  var iFldType, iFldLen: Integer);
begin
  if IsInteger(iFldType1) then
  begin
    iFldType := iFldType1;
    iFldLen  := iFldLen1;
  end
  else
  begin
    iFldType := iFldType2;
    iFldLen  := iFldLen2;
  end
end;

procedure PreferNumeric(iFldType1, iFldLen1, iFldType2, iFldLen2: Integer; var iFldType, iFldLen: Integer);
begin
  if IsNumeric(iFldType1) and not IsInteger(iFldType1) then begin
    iFldType := iFldType1;
    iFldLen  := iFldLen1;
  end else if IsNumeric(iFldType2) and not IsInteger(iFldType2) then begin
    iFldType := iFldType2;
    iFldLen  := iFldLen2;
  end else
    PreferInteger(iFldType1, iFldLen1, iFldType2, iFldLen2,  iFldType, iFldLen);
end;

procedure PreferString(iFldType1, iFldLen1, iFldType2, iFldLen2: Integer; var iFldType, iFldLen: Integer);
begin
  if IsString(iFldType1) then
  begin
    iFldType := iFldType1;
    if IsString(iFldType2) then
      iFldLen  := Max(iFldLen1, iFldLen2)
    else
      iFldLen  := Max(iFldLen1, NumericLen);
  end
  else if IsString(iFldType2) then
  begin
    iFldType := iFldType2;
    if IsString(iFldType1) then
      iFldLen  := Max(iFldLen1, iFldLen2)
    else
      iFldLen  := Max(iFldLen2, NumericLen);
  end
  else
    PreferNumeric(iFldType1, iFldLen1, iFldType2, iFldLen2,  iFldType, iFldLen);
end;

procedure CheckNotBCD(var Operand: Variant; var iFldType, iFldLen: Integer);
var
  C: Currency;
begin
  if IsBCD(iFldType) then
  begin
    if VarIsPresent(Operand) then
    begin
      BCDToCurr(PBcd(TVarData(Operand).VString)^, C);
      Operand := C;
    end;
    iFldType := fldFLOAT;
    iFldLen  := SizeOf(Double);
  end;
end;

procedure CheckBCD(var Operand: Variant; var iFldType, iFldLen: Integer);
var
  B: TBcd;
  C: Currency;
  S: string;
begin
  if not IsBCD(iFldType) then
  begin
    if VarIsPresent(Operand) then
    begin
      if VarIsStr(Operand) then
        B := StrToBcd(string(TVarData(Operand).VString))
      else
      begin
        C := Operand;
        CurrToBCD(C, B, 32, 4);
      end;
      SetString(S, PChar(@B), SizeOf(TBcd));
      Operand := S;
    end;
    iFldType := fldBCD;
    iFldLen  := SizeOf(TBcd);
  end;
end;

function CustomLike(p1: PChar; l1: Integer; p2: PChar; l2: Integer; percent_char, underline_char, escape_char: Char): Boolean;
var
  c: Char;
  AEscapeFlag: Boolean;
begin
  AEscapeFlag := False;
  repeat
    Dec(l2);
    if l2 < 0 then Break;
    c := p2^;
    p2 := p2 + 1;
    if (escape_char <> #0) and not AEscapeFlag and (c = escape_char) then
    begin
      AEscapeFlag := True;
      Continue;
    end;
    if not AEscapeFlag and (c = percent_char) then
    begin
      if l2 = 0 then
      begin
        Result := True;
        Exit;
      end;
      while l1 > 0 do
      begin
        if CustomLike(p1, l1, p2, l2, percent_char, underline_char, escape_char) then
        begin
          Result := True;
          Exit;
        end;
        p1 := p1 + 1;
        Dec(l1);
      end;
      Result := False;
      Exit;
    end;
    Dec(l1);
    if l1 < 0 then
    begin
      Result := False;
      Exit;
    end;
    if (AEscapeFlag or (c <> underline_char)) and (c <> p1^) then
    begin
      Result := False;
      Exit;
    end;
    AEscapeFlag := False;
    p1 := p1 + 1;
  until False;
  Result := l1 = 0;
end;

function Like(const S, Pattern: string): Boolean;
begin
  Result := CustomLike(PChar(S), Length(S), PChar(Pattern), Length(Pattern), '%', '_', #0);
end;

{ TCanExprEvaluator }

destructor TCanExprEvaluator.Destroy;
begin
  FOpList := Unassigned;
  SetData(nil);
  inherited;
end;

procedure TCanExprEvaluator.SetData(AValue: PCanExpr);
begin
  FreeMem(FExpr);
  FExpr := nil;
  if AValue <> nil then
  begin
    GetMem(FExpr, AValue^.iTotalSize);
    Move(AValue^, FExpr^, AValue^.iTotalSize);
  end;
end;

//Elohim Meth
function TCanExprEvaluator.IsSimpleExpression(var AFieldNo: Integer; var AValue: Variant; var ACaseInsensitive: Boolean): Boolean;
var
  CanHdr: pCANHdr;
  CanHdr1, CanHdr2: pCANHdr;
  TmpType, TmpLen: Integer;
begin
  Result := False;
  if FExpr <> nil then
  begin
    CanHdr := pCanHdr(PChar(FExpr) + SizeOf(CanExpr));
    if (NODEClass(CanHdr^.nodeClass) = nodeBINARY) then
    begin
      if TCANOperator(pCanBinary(CanHdr)^.canOp) = coEQ then
      begin
        with pCanBinary(CanHdr)^ do
        begin
          ACaseInsensitive := False;
          CanHdr1 := pCanHdr(PChar(FExpr) + SizeOf(CanExpr) + iOperand1);
          CanHdr2 := pCanHdr(PChar(FExpr) + SizeOf(CanExpr) + iOperand2);
        end;
        if (NODEClass(CanHdr1^.nodeClass) = nodeFIELD) and
           (NODEClass(CanHdr2^.nodeClass) = nodeCONST) then
        begin
          AFieldNo := pCanField(CanHdr1)^.iFieldNum;
          with pCanConst(CanHdr2)^ do
            AValue := GetConstantValue(iType, iSize, iOffset, TmpType, TmpLen);
          Result := True;
        end;
      end;
    end
    else if (NODEClass(CanHdr^.nodeClass) = nodeCOMPARE) then
    begin
      if TCANOperator(pCanCompare(CanHdr)^.canOp) = coEQ then
      begin
        with pCanCompare(CanHdr)^ do
        begin
          if iPartialLen <> 0 then
            Exit;
          ACaseInsensitive := bCaseInsensitive;
          CanHdr1 := pCanHdr(PChar(FExpr) + SizeOf(CanExpr) + iOperand1);
          CanHdr2 := pCanHdr(PChar(FExpr) + SizeOf(CanExpr) + iOperand2);
        end;
        if (NODEClass(CanHdr1^.nodeClass) = nodeFIELD) and
           (NODEClass(CanHdr2^.nodeClass) = nodeCONST) then
        begin
          AFieldNo := pCanField(CanHdr1)^.iFieldNum;
          with pCanConst(CanHdr2)^ do
            AValue := GetConstantValue(iType, iSize, iOffset, TmpType, TmpLen);
          Result := True;
        end;
      end;
    end;
  end;
end;
//Elohim Meth

function TCanExprEvaluator.Evaluate: Variant;
var
  iFldType  :integer;
  iFldLen   :integer;
begin
  if FExpr <> nil then
    Result := GetOperandValue(0, iFldType, iFldLen)
  else
    Result := Null;
end;

function TCanExprEvaluator.CompareOperandsEx(const Op1, Op2: Variant;
  CaseIns: Boolean; PartialLen: Word; CompareLike: Boolean): Integer;
begin
  try
    if CompareLike then
    begin
      if CaseIns then
        Result := Ord(not Like(AnsiUpperCase(Op1), AnsiUpperCase(Op2)))
      else
        Result := Ord(not Like(Op1, Op2));
    end
    else
    begin
      if CaseIns then
      begin
        if PartialLen = 0 then
          Result := AnsiCompareText(string(Op1), string(Op2))
        else
          Result := AnsiStrLIComp(PChar(string(Op1)), PChar(string(Op2)), PartialLen);
      end
      else
      begin
        if PartialLen = 0 then
          Result := CompareOperands(Op1, Op2)
        else
          Result := AnsiStrLComp(PChar(string(Op1)), PChar(string(Op2)), PartialLen);
      end;
    end;
  except
    on EVariantError do
      Result := 1;
  end;
end;

function TCanExprEvaluator.CompareOperandsEx2(var Op1, Op2: Variant; var iFldType1, iFldLen1, iFldType2, iFldLen2: Integer): Integer;
begin
  try
    if IsBCD(iFldType1) or IsBCD(iFldType2) then
    begin
      CheckBCD(Op1, iFldType1, iFldLen1);
      CheckBCD(Op2, iFldType2, iFldLen2);
      Result := BcdCompare(PBcd(TVarData(Op1).VString)^, PBcd(TVarData(Op2).VString)^);
    end
    else
      Result := CompareOperands(Op1, Op2);
  except
    on EVariantError do Result := 1;
  end;
end;

function TCanExprEvaluator.CompareOperands(const Op1, Op2: Variant): Integer;
begin
  try
    case VarCompareValue(Op1, Op2) of
      vrEqual: Result := 0;
      vrLessThan: Result := -1;
      else {vrGreaterThan, vrNotEqual}
        Result := 1;
    end;
  except
    on EVariantError do Result := 1;
  end;
end;

function TCanExprEvaluator.Exists(const Op1, Op2: Variant): Boolean;
var
  I: Integer;
begin
  for I := 0 to VarArrayHighBound(Op2, 1) do
    if CompareOperands(Op1, Op2[I][0]) = 0 then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function TCanExprEvaluator.GetOperandValue(Offset: Word; var iFldType, iFldLen: Integer): Variant;

var
  iFldType1, iFldType2, iFldLen1, iFldLen2: Integer;

  procedure SetResult(FldType, FldLen: Integer);
  begin
    iFldType := FldType;
    iFldLen  := FldLen;
  end;

var
  I: Integer;
  S: string;
  CanHdr: pCanHdr;
  BCD: TBcd;
  P: PBcd;
  Op1, Op2: Variant;
  Func: TEvalFunction;
begin
  Result := Null;
  CanHdr := pCanHdr(PAnsiChar(FExpr) + SizeOf(CanExpr) + Offset);
  case NODEClass(CanHdr^.nodeClass) of
    nodeNULL:
      SetResult(fldUNKNOWN, 0);

    nodeUNARY:                          
      with pCanUnary(CanHdr)^ do
      begin
        Op1 := GetOperandValue(iOperand1, iFldType1, iFldLen1);
        case TCANOperator(canOp) of
          coISBLANK,
          coNOTBLANK:
          begin
            Result := VarIsNull(Op1) xor (TCANOperator(canOp) = coNOTBLANK);
            SetResult(fldBOOL, SizeOf(WordBool));
          end;

          coNOT:
          begin
            if VarIsPresent(Op1) then
              Result := not Op1;
            SetResult(fldBOOL, SizeOf(WordBool));
          end;

          coMINUS:
          begin
            if VarIsPresent(Op1) then
            begin
              if IsBCD(iFldType1) then
              begin
                P := PBcd(TVarData(Op1).VString);
                P^.SignSpecialPlaces := P^.SignSpecialPlaces xor $80;
                Result := Op1;
              end
              else
                Result := - Op1;
            end;
            SetResult(iFldType1, iFldLen1);
          end;

          coUPPER:
          begin
            if VarIsPresent(Op1) then
              Result := AnsiUpperCase(Op1);
            SetResult(iFldType1, iFldLen1);
          end;

          coLOWER:
          begin
            if VarIsPresent(Op1) then
              Result := AnsiLowerCase(Op1);
            SetResult(iFldType1, iFldLen1);
          end;
        else
          NotSupported;
        end;
      end;

    nodeBINARY:
      with pCanBinary(CanHdr)^ do
      begin
        Op1 := GetOperandValue(iOperand1, iFldType1, iFldLen1);
        if canOp = Integer(coASSIGN) then
        begin
          Result := Op1;
          SetResult(iFldType1, iFldLen1);
        end
        else
        begin
          Op2 := GetOperandValue(iOperand2, iFldType2, iFldLen2);
          case TCANOperator(canOp) of
            coEQ, coNE:
            begin
              if VarIsPresent(Op1) and VarIsPresent(Op2) then
              begin
                Result := (CompareOperandsEx2(Op1, Op2, iFldType1, iFldLen1, iFldType2, iFldLen2) = 0);
                Result := Result xor (TCANOperator(canOp) = coNE);
              end;
              SetResult(fldBOOL, SizeOf(WordBool));
            end;

            coGT:
            begin
              if VarIsPresent(Op1) and VarIsPresent(Op2) then
                Result := CompareOperandsEx2(Op1, Op2, iFldType1, iFldLen1, iFldType2, iFldLen2) > 0;
              SetResult(fldBOOL, SizeOf(WordBool));
            end;

            coLT:
            begin
              if VarIsPresent(Op1) and VarIsPresent(Op2) then
                Result := CompareOperandsEx2(Op1, Op2, iFldType1, iFldLen1, iFldType2, iFldLen2) < 0;
              SetResult(fldBOOL, SizeOf(WordBool));
            end;

            coGE:
            begin
              if VarIsPresent(Op1) and VarIsPresent(Op2) then
                Result := CompareOperandsEx2(Op1, Op2, iFldType1, iFldLen1, iFldType2, iFldLen2) >= 0;
              SetResult(fldBOOL, SizeOf(WordBool));
            end;

            coLE:
            begin
              if VarIsPresent(Op1) and VarIsPresent(Op2) then
                Result := CompareOperandsEx2(Op1, Op2, iFldType1, iFldLen1, iFldType2, iFldLen2) <= 0;
              SetResult(fldBOOL, SizeOf(WordBool));
            end;

            coAND:
            begin
              if VarIsNull(Op1) or VarIsNull(Op2) then
                Result := False
              else
                Result := Op1 and Op2;
              SetResult(fldBOOL, SizeOf(WordBool));
            end;

            coOR:
            begin
              if VarIsNull(Op1) or VarIsNull(Op2) then
                Result := VarSameValue(Op1, True) or VarSameValue(Op2, True)
              else
                Result := Op1 or Op2;
              SetResult(fldBOOL, SizeOf(WordBool));
            end;

            coADD:
            begin
              if IsBCD(iFldType1) or IsBCD(iFldType2) then
              begin
                CheckBCD(Op1, iFldType1, iFldLen1);
                CheckBCD(Op2, iFldType2, iFldLen2);
                if VarIsPresent(Op1) and VarIsPresent(Op2) then
                begin
                  BcdAdd(PBcd(TVarData(Op1).VString)^, PBcd(TVarData(Op2).VString)^, BCD);
                  SetString(S, PAnsiChar(@BCD), SizeOf(TBcd));
                  Result := S;
                end;
                SetResult(fldBCD, SizeOf(TBcd));
              end
              else
              begin
                CheckNotBCD(Op1, iFldType1, iFldLen1);
                CheckNotBCD(Op2, iFldType2, iFldLen2);
                PreferString(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
                if VarIsPresent(Op1) and VarIsPresent(Op2) then
                begin
                  if (iFldType = fldZSTRING) then
                    Result := string(Op1) + string(Op2)
                  else if (iFldType = fldUNICODE) then
                    Result := WideString(Op1) + WideString(Op2)
                  else
                    Result := Op1 + Op2;
                end;

                if IsString(iFldType) then
                  iFldLen := iFldLen1 + iFldLen2;
              end;
            end;

            coSUB:
            begin
              if IsBCD(iFldType1) or IsBCD(iFldType2) then
              begin
                CheckBCD(Op1, iFldType1, iFldLen1);
                CheckBCD(Op2, iFldType2, iFldLen2);
                if VarIsPresent(Op1) and VarIsPresent(Op2) then
                begin
                  BcdSubtract(PBcd(TVarData(Op1).VString)^, PBcd(TVarData(Op2).VString)^, BCD);
                  SetString(S, PAnsiChar(@BCD), SizeOf(TBcd));
                  Result := S;
                end;
                SetResult(fldBCD, SizeOf(TBcd));
              end
              else
              begin
                CheckNotBCD(Op1, iFldType1, iFldLen1);
                CheckNotBCD(Op2, iFldType2, iFldLen2);
                if VarIsPresent(Op1) and VarIsPresent(Op2) then
                  Result := Op1 - Op2;
                PreferNumeric(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
              end;
            end;

            coMUL:
            begin
              if IsBCD(iFldType1) or IsBCD(iFldType2) then
              begin
                CheckBCD(Op1, iFldType1, iFldLen1);
                CheckBCD(Op2, iFldType2, iFldLen2);
                if VarIsPresent(Op1) and VarIsPresent(Op2) then
                begin
                  BcdMultiply(PBcd(TVarData(Op1).VString)^, PBcd(TVarData(Op2).VString)^, BCD);
                  SetString(S, PAnsiChar(@BCD), SizeOf(TBcd));
                  Result := S;
                end;
                SetResult(fldBCD, SizeOf(TBcd));
              end
              else
              begin
                CheckNotBCD(Op1, iFldType1, iFldLen1);
                CheckNotBCD(Op2, iFldType2, iFldLen2);
                if VarIsPresent(Op1) and VarIsPresent(Op2) then
                  Result := Op1 * Op2;
                PreferNumeric(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
              end;
            end;

            coDIV:
            begin
              if IsBCD(iFldType1) or IsBCD(iFldType2) then
              begin
                CheckBCD(Op1, iFldType1, iFldLen1);
                CheckBCD(Op2, iFldType2, iFldLen2);
                if VarIsPresent(Op1) and VarIsPresent(Op2) then
                begin
                  BcdDivide(PBcd(TVarData(Op1).VString)^, PBcd(TVarData(Op2).VString)^, BCD);
                  SetString(S, PAnsiChar(@BCD), SizeOf(TBcd));
                  Result := S;
                end;
                SetResult(fldBCD, SizeOf(TBcd));
              end
              else
              begin
                CheckNotBCD(Op1, iFldType1, iFldLen1);
                CheckNotBCD(Op2, iFldType2, iFldLen2);
                if VarIsPresent(Op1) and VarIsPresent(Op2) then
                  Result := Op1 / Op2;
                SetResult(fldFLOAT, SizeOf(Double));
              end;
            end;

            coMOD:
            begin
              CheckNotBCD(Op1, iFldType1, iFldLen1);
              CheckNotBCD(Op2, iFldType2, iFldLen2);
              if VarIsPresent(Op1) and VarIsPresent(Op2) then
                Result := Op1 mod Op2;
              PreferInteger(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
            end;

            coREM:
            begin
              CheckNotBCD(Op1, iFldType1, iFldLen1);
              CheckNotBCD(Op2, iFldType2, iFldLen2);
              if VarIsPresent(Op1) and VarIsPresent(Op2) then
                Result := Op1 - Op1 div Op2 * Op2;
              PreferInteger(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
            end;

            coMIN:
            begin
              CheckNotBCD(Op1, iFldType1, iFldLen1);
              CheckNotBCD(Op2, iFldType2, iFldLen2);
              if VarIsPresent(Op1) and VarIsPresent(Op2) then
              begin
                if (CompareOperands(Op1, Op2) < 0) then
                  Result := Op1
                else
                  Result := Op2;
              end;
              PreferNumeric(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
            end;

            coMAX:
            begin
              CheckNotBCD(Op1, iFldType1, iFldLen1);
              CheckNotBCD(Op2, iFldType2, iFldLen2);
              if VarIsPresent(Op1) and VarIsPresent(Op2) then
              begin
                if (CompareOperands(Op1, Op2) > 0) then
                  Result := Op1
                else
                  Result := Op2;
              end;
              PreferString(iFldType1, iFldLen1, iFldType2, iFldLen2, iFldType, iFldLen);
            end;

            coAVG:
            begin
              CheckNotBCD(Op1, iFldType1, iFldLen1);
              CheckNotBCD(Op2, iFldType2, iFldLen2);
              if VarIsPresent(Op1) and VarIsPresent(Op2) then
                Result := (Op1 + Op2) / 2;
              SetResult(fldFLOAT, SizeOf(Double));
            end;

            coLIKE:
            begin
              if VarIsPresent(Op1) and VarIsPresent(Op2) then
                Result := (CompareOperandsEx(Op1, Op2, False, 0, True) = 0);
              SetResult(fldBOOL, SizeOf(WordBool));
            end;

            coIN:                        
            begin
              CheckNotBCD(Op1, iFldType1, iFldLen1);
              CheckNotBCD(Op2, iFldType2, iFldLen2);
              if VarIsPresent(Op1) and VarIsPresent(Op2) then
                Result := Exists(Op1, Op2);
              SetResult(fldBOOL, SizeOf(WordBool));
            end;
            else
              NotSupported;
          end;
        end;
      end;

    nodeCOMPARE:
      with pCanCompare(CanHdr)^ do
        case TCANOperator(canOp) of
          coEQ, coNE, coLIKE:
          begin
            Op1 := GetOperandValue(iOperand1, iFldType1, iFldLen1);
            Op2 := GetOperandValue(iOperand2, iFldType2, iFldLen2);
            if VarIsPresent(Op1) and VarIsPresent(Op2) then
              Result := (CompareOperandsEx(Op1, Op2, bCaseInsensitive, iPartialLen,
                TCANOperator(canOp) = coLIKE) = 0) xor (canOp = Integer(coNE));

            SetResult(fldBOOL, SizeOf(WordBool));
          end;
        end;
        
    nodeFIELD:                          
      with pCanField(CanHdr)^ do
        Result := GetFieldValue(iFieldNum, iNameOffset, iFldType, iFldLen);
        
    nodeCONST:                          
      with pCanConst(CanHdr)^ do
        Result := GetConstantValue(iType, iSize, iOffset, iFldType, iFldLen);

    nodeCONTINUE:
    begin
      Result := True;
      SetResult(fldBOOL, SizeOf(WordBool));
    end;

    nodeLIST:
    begin
      with pCanList(CanHdr)^ do
        Result := GetOperandValue(iOffset, iFldType1, iFldLen1);
      SetResult(fldUNKNOWN, 0);
    end;

    nodeFUNC:
    with pCANFunc(CanHdr)^ do
    begin
      S := GetConstantValue(fldZSTRING, 0, iNameOffset, iFldType1, iFldLen1);
      Func := StrToEvalFunction(S);
      Result := GetFuncValue(Func, iElemOffset, iFldType, iFldLen);
    end;

    nodeLISTELEM:
    begin
      if not VarIsArray(FOpList) then
        FOpList := VarArrayCreate([0, 9], varVariant);
      I := 0;
      repeat
        if I > VarArrayHighBound(FOpList, 1) then
           VarArrayRedim(FOpList, I + 10);

        with pCanListElem(CanHdr)^ do
        begin
          FOpList[I] := VarArrayOf([GetOperandValue(iOffset, iFldType1, iFldLen1), iFldType1, iFldLen1]);
          if iNextOffset = 0 then
          begin
            VarArrayRedim(FOpList, I);
            Result := FOpList;
            SetResult(fldUNKNOWN, 0);
            Exit;
          end;
          CanHdr := pCanHdr(PAnsiChar(FExpr) + SizeOf(CanExpr) + iNextOffset);
          Inc(I);
        end;
      until False;
    end;
    else
      NotSupported;
  end;
end;

function TCanExprEvaluator.GetConstantValue(DataType, Size, Offset: Word;
  var iFldType, iFldLen: Integer): Variant;

  procedure SetResult(FldType, FldLen: Integer);
  begin
    iFldType := FldType;
    iFldLen  := FldLen;
  end;

var
  AnsiStr: AnsiString;
  Str: String;
  CanHdr: pCanHdr;
begin
  CanHdr := pCanHdr(PAnsiChar(FExpr) + FExpr.iLiteralStart + Offset);

  case DataType of
    fldINT16:
    begin
      Result := PSmallInt(CanHdr)^;
      SetResult(DataType, SizeOf(SmallInt));
    end;
    fldUINT16:
    begin
      Result := PWord(CanHdr)^;
      SetResult(DataType, SizeOf(SmallInt));
    end;
    fldINT32:
    begin
      Result := PInteger(CanHdr)^;
      SetResult(DataType, SizeOf(Integer));
    end;
    fldUINT32:
    begin
      Result := PLongWord(CanHdr)^;
      SetResult(DataType, SizeOf(Integer));
    end;
    fldINT64, fldUINT64:
    begin
      Result := PInt64(CanHdr)^;
      SetResult(DataType, SizeOf(Int64));
    end;
    fldFLOAT:
    begin
      Result := PDouble(CanHdr)^;
      SetResult(DataType, SizeOf(Double));
    end;
    fldZSTRING:
    begin
      if Size > 0 then
        SetString(AnsiStr, PAnsiChar(CanHdr), Size - 1)
      else
      begin
        AnsiStr := System.AnsiStrings.StrPas(PAnsiChar(CanHdr));
        Size := Length(AnsiStr) + 1;
      end;
      Result := AnsiStr;
      SetResult(DataType, Size);
    end;
    fldUNICODE:
    begin
      if Size > 0 then
        SetString(Str, PChar(CanHdr) + 1, Size div 2 - 1)
      else begin
        Str  := String(PChar(CanHdr));
        Size := Length(Str) * 2 + 1;
      end;
      Result := Str;
      SetResult(DataType, Size);
    end;

    fldDATE:
    begin
      Result := ValuePtrToDateTime(DataType, CanHdr);
      SetResult(DataType, SizeOf(TDateTimeRec));
    end;
    fldTIME:
    begin
      Result := ValuePtrToDateTime(DataType, CanHdr);
      SetResult(DataType, SizeOf(TDateTimeRec));
    end;
    fldTIMESTAMP: 
    begin
      Result := ValuePtrToDateTime(DataType, CanHdr);
      SetResult(DataType, SizeOf(TDateTimeRec));
    end;
    fldBOOL:
    begin
      Result := PWordBool(CanHdr)^;
      SetResult(DataType, SizeOf(WordBool));
    end;
    fldBCD, fldFMTBCD:
    begin
      SetLength(AnsiStr, SizeOf(TBcd));
      ZeroMem(Pointer(AnsiStr), SizeOf(TBcd));
//      ZeroMemory(Pointer(S), SizeOf(TBcd));
      Move(CanHdr^, Pointer(AnsiStr)^, 18);
      Result := AnsiStr;
      SetResult(DataType, SizeOf(TBcd));
    end;
    else
      NotSupported;
  end;
end;

function TCanExprEvaluator.GetFieldValue(FieldNo, Offset: Word; var iFldType, iFldLen: Integer): Variant;
var
  FieldName: PAnsiChar;
begin
  FieldName := PAnsiChar(FExpr) + FExpr.iLiteralStart + Offset;
  Result := DoGetFieldValue(FieldNo, String(System.AnsiStrings.StrPas(FieldName)), iFldType, iFldLen);
end;

function TCanExprEvaluator.GetFuncValueInternal(Func: TEvalFunction;
  Args: Variant; var iFldType, iFldLen: Integer): Variant;

  procedure SetResult(FldType, FldLen: Integer);
  begin
    iFldType := FldType;
    iFldLen  := FldLen;
  end;

  procedure SetResultStr(Arg: Variant);
  var
    iFieldType: Integer;
  begin
    iFieldType := Arg[1];
    if iFieldType = fldZSTRING then
      SetResult(fldZSTRING, Arg[2])
    else
      SetResult(fldZSTRING, NumericLen);
  end;

var
  Tmp: Variant;
  S: string;
  Year, Month, Day: Word;
  Hour, Min, Sec, MSec: Word;
  Start, Count: Integer;
  TimeStamp: TDateTime;
begin
  case Func of
    efUpper, efLower:
      begin
        Tmp := Args[0][0];
        SetResultStr(Args[0]);
        if VarIsNull(Tmp) then
          Result := Null
        else
        begin
          S := Tmp;
          if Func = efUpper then
            Result := AnsiUpperCase(S)
          else
            Result := AnsiLowerCase(S);
        end;
      end;
    efSubString:
      begin
        Tmp := Args[0][0];
        SetResultStr(Args[0]);
        if VarIsNull(Tmp) then
          Result := Null
        else
        begin
          S := Tmp;
          Start  := Args[1][0];
          Count  := Args[2][0];
          Result := Copy(S, Start, Count);
        end;
      end;
    efTrim, efTrimLeft, efTrimRight:
      begin
        Tmp := Args[0][0];
        SetResultStr(Args[0]);
        if VarIsNull(Tmp) then
          Result := Null
        else begin
          S := Tmp;
          if Func = efTrim then
            Result := Trim(S)
          else if Func = efTrimLeft then
            Result := TrimLeft(S)
          else
            Result := TrimRight(S);
        end;
      end;
    efYear, efMonth, efDay:
      begin
        if VarIsNull(Args) then
          TimeStamp := Date else
          TimeStamp := Args[0][0];
        DecodeDate(TimeStamp, Year, Month, Day);
        SetResult(fldUINT16, SizeOf(Word));
        if Func = efYear then
          Result := Year
        else if Func = efMonth then
          Result := Month
        else
          Result := Day;
      end;
    efHour, efMinute, efSecond:
      begin
        if VarIsNull(Args) then
          TimeStamp := Time else
          TimeStamp := Args[0][0];
        DecodeTime(TimeStamp, Hour, Min, Sec, MSec);
        SetResult(fldUINT16, SizeOf(Word));
        if Func = efHour then
          Result := Hour
        else if Func = efMinute then
          Result := Min
        else
          Result := Sec;
      end;
    efGetDate:
      begin
        Result := Now;
        SetResult(fldTIMESTAMP, SizeOf(TDateTime));
      end;
    efDate, efTime:
      begin
        SetResult(fldTIMESTAMP, SizeOf(TDateTime));
        Tmp := Args[0][0];
        if VarIsNull(Tmp) then
          Result := Null
        else begin
          if Func = efDate then
            TimeStamp := Int(Tmp) else
            TimeStamp := Frac(Tmp);
          Result := TimeStamp;
        end;
      end;
    efIIF:
      begin
        SetResult(fldBOOL, SizeOf(WordBool));
        Tmp := Args[0][0];
        if VarIsNull(Tmp) then
          Result := Null
        else begin
          if Tmp then
            Result := Args[1][0] else
            Result := Args[2][0];
        end;
      end;
  end;
end;

function TCanExprEvaluator.GetFuncValue(Func: TEvalFunction; Offset: Word; var iFldType, iFldLen: Integer): Variant;

  procedure SetResult(FldType, FldLen: Integer);
  begin
    iFldType := FldType;
    iFldLen  := FldLen;
  end;

  function GetOperand: Variant;
  var
    iFldType1, iFldLen1: Integer;
  begin
    if Offset > 0 then
      Result := GetOperandValue(Offset, iFldType1, iFldLen1)
    else
      Result := VarArrayOf([Null]);
  end;

var
  Count, Cmp: Integer;
  V: Variant;
  B: TBcd;
  S: string;
begin
  case Func of
    efUnknown:
      NotSupported;

    efSum:
    begin
      DoFirst;
      V := GetOperand[0];
      if VarIsNull(V) then
      begin
        SetResult(fldUNKNOWN, 0);
        Result := Null;
      end
      else
      begin
        SetResult(V[1], V[2]);
        Result := V[0];
        DoNext;
        while not DoGetEOF do
        begin
          V := GetOperand[0][0];
          if VarIsPresent(V) then
          begin
            if VarIsPresent(Result) then
            begin
              if (iFldType = fldBCD) or (iFldType = fldFMTBCD) then
              begin
                BcdAdd(PBcd(TVarData(Result).VString)^, PBcd(TVarData(V).VString)^, B);
                SetString(S, PAnsiChar(@B), SizeOf(TBcd));
                Result := S;
              end
              else
                Result := Result + V;
            end
            else
              Result := V;
          end;
          DoNext;
        end;
      end;
    end;

    efMin, efMax:
    begin
      DoFirst;
      V := GetOperand[0];
      if VarIsNull(V) then
      begin
        SetResult(fldUNKNOWN, 0);
        Result := Null;
      end
      else
      begin
        SetResult(V[1], V[2]);
        Result := V[0];
        DoNext;
        while not DoGetEOF do
        begin
          V := GetOperand[0][0];
          if VarIsPresent(V) then
          begin
            if VarIsPresent(Result) then
            begin
              Cmp := CompareOperandsEx2(Result, V, iFldType, iFldLen, iFldType, iFldLen);
              if ((Func = efMin) and (Cmp > 0)) or ((Func = efMax) and (Cmp < 0)) then
                Result := V;
            end
            else
              Result := V;
          end;
          DoNext;
        end;
      end;
    end;

    efAvg:
    begin
      DoFirst;
      V := GetOperand[0];
      if VarIsNull(V) then
      begin
        SetResult(fldUNKNOWN, 0);
        Result := Null;
      end
      else
      begin
        SetResult(V[1], V[2]);
        Result := V[0];
        Count := 1;
        DoNext;
        while not DoGetEOF do
        begin
          V := GetOperand[0][0];
          if VarIsPresent(V) then
          begin
            if VarIsPresent(Result) then
            begin
              if (iFldType = fldBCD) or (iFldType = fldFMTBCD) then
              begin
                BcdAdd(PBcd(TVarData(Result).VString)^, PBcd(TVarData(V).VString)^, B);
                SetString(S, PAnsiChar(@B), SizeOf(TBcd));
                Result := S;
              end
              else
                Result := Result + V;
            end
            else
              Result := V;
            Inc(Count);
          end;
          DoNext;
        end;

        if (iFldType = fldBCD) or (iFldType = fldFMTBCD) then
        begin
          if VarIsPresent(Result) then
          begin
{$IFDEF DELPHIXE2}
            BcdDivide(PBcd(TVarData(Result).VString)^, TBcd(Count), B);
{$ELSE}
            BcdDivide(PBcd(TVarData(Result).VString)^, Count, B);
{$ENDIF}
            SetString(S, PAnsiChar(@B), SizeOf(TBcd));
            Result := S;
          end;
        end
        else
        begin
          SetResult(fldFLOAT, SizeOf(Double));
          if VarIsPresent(Result) then
          begin
            Result := VarAsType(Result, varDouble);
            Result := Result / Count;
          end;
        end;
      end;
    end;

    efCount:
    begin
      Count := 0;
      DoFirst;
      while not DoGetEOF do
      begin
        V := GetOperand[0];
        if VarIsNull(V) or VarIsPresent(V[0]) then
          Inc(Count);
        DoNext;
      end;
      SetResult(fldINT32, SizeOf(Integer));
      Result := Count;
    end;

    Succ(efCount)..High(TEvalFunction):
      Result := GetFuncValueInternal(Func, GetOperand, iFldType, iFldLen);
  end;
end;

function TCanExprEvaluator.DoGetFieldValue(FieldNo: Word; const FieldName: string; var iFldType, iFldLen: Integer): Variant;
begin
  Result   := Null;
  iFldType := fldUNKNOWN;
  iFldLen  := 0;
end;

procedure TCanExprEvaluator.DoFirst;
begin
end;

procedure TCanExprEvaluator.DoNext;
begin
end;

function TCanExprEvaluator.DoGetEOF: Boolean;
begin
  Result := True;
end;

{ TDataSetExprEvaluator }

constructor TDataSetExprEvaluator.Create(ADataSet: TDataSet; AOldValues: Boolean);
begin
  FDataSet := ADataSet;
  FOldValues := AOldValues;
end;

function TDataSetExprEvaluator.DoGetFieldValue(FieldNo: Word; const FieldName: string; var FldType, FldLen: Integer): Variant;
begin
  with FDataSet.FieldByName(FieldName) do
    if FOldValues then
      Result := OldValue
    else
      Result := AsVariant
end;

end.


