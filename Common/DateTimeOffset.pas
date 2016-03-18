unit DateTimeOffset;

interface

uses
  SysUtils, Variants, DateUtils, StrUtils;

type
  TDateTimeOffset = packed record
    DateTimeUTC : TDateTime;
    Offset      : Int16;
  private
    procedure InternalCreateFromString(AValue: String; const Format: TFormatSettings);
  public
    constructor Create(ADateTime: TDateTime; AOffset: Int16; ADateTimeInUTC: Boolean = True); overload;
    constructor Create(const AValue: String); overload;
    constructor Create(const AValue: String; const Format: TFormatSettings); overload;
  end;

  EDateTimeOffsetException = class(Exception);

{ TDateTimeOffset variant creation utils }
  procedure VarDateTimeOffsetCreate(var ADest: Variant; const ADateTimeOffset: TDateTimeOffset); overload;
  function VarDateTimeOffsetCreate: Variant; overload;
  function VarDateTimeOffsetCreate(const AValue: string): Variant; overload;
  function VarDateTimeOffsetCreate(const ADateTime: TDateTime; AOffset: Int16 = 0; ADateTimeInUTC: Boolean = True): Variant; overload;
  function VarDateTimeOffsetCreate(const ADateTimeOffset: TDateTimeOffset): Variant; overload;
  function VarIsDateTimeOffset(const AValue: Variant): Boolean; overload;
  function VarDateTimeOffset: TVarType;
  function VarToDateTimeOffset(const AValue: Variant): TDateTimeOffset;

const
  NullDateTimeOffset: TDateTimeOffset = (DateTimeUTC: 0; Offset: 0);

implementation

uses
  VarUtils, SysConst, TypInfo, Math, Classes, SBaseStringFunctions
{$IFNDEF PACKAGE}
{$ENDIF}
  ;

type
{ TDateTimeOffsetVariantType }

  TDateTimeOffsetVariantType = class(TPublishableVariantType)
  protected
    function GetInstance(const V: TVarData): TObject; override;
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
    procedure Cast(var Dest: TVarData; const Source: TVarData); override;
    procedure CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType); override;
//    procedure BinaryOp(var Left: TVarData; const Right: TVarData; const Operator: TVarOp); override;
    procedure Compare(const Left, Right: TVarData; var Relationship: TVarCompareResult); override;
  end;

var
{ DateTimeOffset that the complex variant points to }
  DateTimeOffsetVariantType: TDateTimeOffsetVariantType = nil;

type
{ TDateTimeOffsetData }

  TDateTimeOffsetData = class(TPersistent)
  private
    FDateTimeOffset: TDateTimeOffset;

    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(const Value: TDateTime);
  public
    constructor Create(const AText: string); overload;
    constructor Create(const ADateTime: TDateTime; const AOffset: Int16; ADateTimeInUTC: Boolean = True); overload;
    constructor Create(const ADateTimeOffset: TDateTimeOffset); overload;
    constructor Create(const ASource: TDateTimeOffsetData); overload;

    property DateTimeOffset: TDateTimeOffset read FDateTimeOffset write FDateTimeOffset;

    function Compare(const Value: TDateTimeOffsetData): TVarCompareResult;
{
    procedure DoAdd(const Value: TDateTimeOffset); overload;
    procedure DoAdd(const ADateTimeOffset: TDateTimeOffsetData); overload;
    procedure DoSubtract(const Value: TDateTimeOffset); overload;
    procedure DoSubtract(const ADateTimeOffset: TDateTimeOffsetData); overload;
    procedure DoMultiply(const ABcdData: TDateTimeOffsetData); overload;
    procedure DoDivide(const ABcdData: TDateTimeOffsetData); overload;
}
  published
    { Conversion }
    property AsString: string read GetAsString write SetAsString;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
  end;

{ Helper record that helps crack open TDateTimeOffsetObject }

  TDateTimeOffsetVarData = packed record
    VType: TVarType;
    Reserved1, Reserved2, Reserved3: Word;

    case Integer of
{$IFDEF AUTOREFCOUNT}
    0: (VDateTimeOffset: Pointer);
{$ELSE}
    0: (VDateTimeOffset: TDateTimeOffsetData);
{$ENDIF AUTOREFCOUNT}
    1: (VLargest: TLargestVarData); // to match size as Variant/TVarData type
  end;

procedure VarDateTimeOffsetCreate(var ADest: Variant; const ADateTimeOffset: TDateTimeOffset); overload;
begin
  VarClear(ADest);
  TDateTimeOffsetVarData(ADest).VType := DateTimeOffsetVariantType.VarType;
  TDateTimeOffsetVarData(ADest).VDateTimeOffset := TDateTimeOffsetData.Create(ADateTimeOffset);
end;

function VarDateTimeOffsetCreate: Variant; overload;
begin
  VarDateTimeOffsetCreate(Result, NullDateTimeOffset);
end;

function VarDateTimeOffsetCreate(const AValue: string): Variant; overload;
begin
  VarClear(Result);
  TDateTimeOffsetVarData(Result).VType := DateTimeOffsetVariantType.VarType;
  TDateTimeOffsetVarData(Result).VDateTimeOffset := TDateTimeOffsetData.Create(AValue);
end;

function VarDateTimeOffsetCreate(const ADateTime: TDateTime; AOffset: Int16; ADateTimeInUTC: Boolean): Variant; overload;
begin
  VarClear(Result);
  TDateTimeOffsetVarData(Result).VType := DateTimeOffsetVariantType.VarType;
  TDateTimeOffsetVarData(Result).VDateTimeOffset := TDateTimeOffsetData.Create(ADateTime, AOffset, ADateTimeInUTC);
end;

function VarDateTimeOffsetCreate(const ADateTimeOffset: TDateTimeOffset): Variant; overload;
begin
  VarClear(Result);
  TDateTimeOffsetVarData(Result).VType := DateTimeOffsetVariantType.VarType;
  TDateTimeOffsetVarData(Result).VDateTimeOffset := TDateTimeOffsetData.Create(ADateTimeOffset);
end;

function VarIsDateTimeOffset(const AValue: Variant): Boolean; overload;
begin
  Result := (TVarData(AValue).VType = DateTimeOffsetVariantType.VarType);
end;

function VarDateTimeOffset: TVarType;
begin
  Result := DateTimeOffsetVariantType.VarType
end;

function VarToDateTimeOffset(const AValue: Variant): TDateTimeOffset;
begin
  if VarType(AValue) = DateTimeOffsetVariantType.VarType then
    Result := TDateTimeOffsetVarData(AValue).VDateTimeOffset.DateTimeOffset
  else
    case VarType(AValue) of
      varDate:
        Result := TDateTimeOffset.Create(VarToDateTime(AValue), 0)
      else
        Result := TDateTimeOffset.Create(VarToStr(AValue));
    end;
end;

{ TDateTimeOffset }

constructor TDateTimeOffset.Create(ADateTime: TDateTime; AOffset: Int16; ADateTimeInUTC: Boolean);
begin
  if ADateTimeInUTC then
    DateTimeUTC := ADateTime
  else
    DateTimeUTC := IncMinute(ADateTime, -Offset);

  Offset := AOffset
end;

constructor TDateTimeOffset.Create(const AValue: String);
begin
  InternalCreateFromString(AValue, FormatSettings);
end;

constructor TDateTimeOffset.Create(const AValue: String; const Format: TFormatSettings);
begin
  InternalCreateFromString(AValue, Format);
end;

procedure TDateTimeOffset.InternalCreateFromString(AValue: String; const Format: TFormatSettings);
var
  LLength: Integer;
begin
  LLength := Length(AValue);
  if (LLength > 6) and (AValue[LLength-2] = ':') and CharInSet(AValue[LLength-5], ['-', '+']) then begin
    Offset := StrToInt(Copy(AValue, LLength - 4, 2)) * 60 + StrToInt(Copy(AValue, LLength - 1, 2));
    if AValue[LLength-5] = '-' then
      Offset := -Offset;
    AValue := TrimRight(Copy(AValue, 1, LLength - 6), ' ');
  end else
    Offset := 0;

  DateTimeUTC := IncMinute(StrToDateTime(AValue, FormatSettings), -Offset);
end;

{ TDateTimeOffsetVariantType }

procedure TDateTimeOffsetVariantType.Cast(var Dest: TVarData; const Source: TVarData);
var
  LSource, LTemp: TVarData;
begin
  VarDataInit(LSource);
  try
    VarDataCopyNoInd(LSource, Source);
    if VarDataIsStr(LSource) then
      TDateTimeOffsetVarData(Dest).VDateTimeOffset := TDateTimeOffsetData.Create(VarDataToStr(LSource))
    else begin
      VarDataInit(LTemp);
      try
        VarDataCastTo(LTemp, LSource, varDate);
        TDateTimeOffsetVarData(Dest).VDateTimeOffset := TDateTimeOffsetData.Create(LTemp.VDate, 0);
      finally
        VarDataClear(LTemp);
      end;
    end;
    Dest.VType := VarType;
  finally
    VarDataClear(LSource);
  end;
end;

procedure TDateTimeOffsetVariantType.CastTo(var Dest: TVarData; const Source: TVarData; const AVarType: TVarType);
var
  LTemp: TVarData;
begin
  if Source.VType = VarType then
    case AVarType of
      varOleStr:
        VarDataFromOleStr(Dest, TDateTimeOffsetVarData(Source).VDateTimeOffset.AsString);
      varString:
        VarDataFromLStr(Dest, AnsiString(TDateTimeOffsetVarData(Source).VDateTimeOffset.AsString));
      varUString:
        VarDataFromStr(Dest, TDateTimeOffsetVarData(Source).VDateTimeOffset.AsString);
      else
        VarDataInit(LTemp);
        try
          LTemp.VType := varDate;
          LTemp.VDate := TDateTimeOffsetVarData(Source).VDateTimeOffset.DateTimeOffset.DateTimeUTC;
          VarDataCastTo(Dest, LTemp, AVarType);
        finally
          VarDataClear(LTemp);
        end;
      end
  else
    inherited;
end;

procedure TDateTimeOffsetVariantType.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  FreeAndNil(TDateTimeOffsetVarData(V).VDateTimeOffset);
end;

procedure TDateTimeOffsetVariantType.Compare(const Left, Right: TVarData; var Relationship: TVarCompareResult);
begin
  Relationship := TDateTimeOffsetVarData(Left).VDateTimeOffset.Compare(TDateTimeOffsetVarData(Right).VDateTimeOffset);
end;

procedure TDateTimeOffsetVariantType.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    with TDateTimeOffsetVarData(Dest) do
    begin
      VType := VarType;
      VDateTimeOffset := TDateTimeOffsetData.Create(TDateTimeOffsetVarData(Source).VDateTimeOffset);
    end;
end;

function TDateTimeOffsetVariantType.GetInstance(const V: TVarData): TObject;
begin
  Result := TDateTimeOffsetVarData(V).VDateTimeOffset;
end;

{ TDateTimeOffsetData }

constructor TDateTimeOffsetData.Create(const ADateTimeOffset: TDateTimeOffset);
begin
  inherited Create;
  Move(ADateTimeOffset, FDateTimeOffset, SizeOf(TDateTimeOffset));
end;

constructor TDateTimeOffsetData.Create(const ADateTime: TDateTime; const AOffset: Int16; ADateTimeInUTC: Boolean);
begin
  inherited Create;
  if ADateTimeInUTC then
    FDateTimeOffset.DateTimeUTC := ADateTime
  else
    FDateTimeOffset.DateTimeUTC := IncMinute(ADateTime, -AOffset);

  FDateTimeOffset.Offset := AOffset;
end;

constructor TDateTimeOffsetData.Create(const AText: string);
begin
  inherited Create;
  FDateTimeOffset := TDateTimeOffset.Create(AText)
end;

constructor TDateTimeOffsetData.Create(const ASource: TDateTimeOffsetData);
begin
  Create(aSource.DateTimeOffset);
end;

function TDateTimeOffsetData.Compare(const Value: TDateTimeOffsetData): TVarCompareResult;
begin
  Result := TVarCompareResult(CompareDateTime(Self.DateTimeOffset.DateTimeUTC, Value.DateTimeOffset.DateTimeUTC))
end;

function TDateTimeOffsetData.GetAsDateTime: TDateTime;
begin
  Result := FDateTimeOffset.DateTimeUTC
end;

function TDateTimeOffsetData.GetAsString: string;
var
  LOffset: UInt16;
begin
  Result := DateTimeToStr(IncMinute(FDateTimeOffset.DateTimeUTC, FDateTimeOffset.Offset)) + IfThen((FDateTimeOffset.Offset < 0), '-', '+');
  LOffset := abs(FDateTimeOffset.Offset);
  Result := Result + ZeroPredicatedUInt(LOffset div 60, 2) + ':' + ZeroPredicatedUInt(LOffset mod 60, 2);
end;

procedure TDateTimeOffsetData.SetAsDateTime(const Value: TDateTime);
begin
  FDateTimeOffset.DateTimeUTC := Value;
  FDateTimeOffset.Offset := 0;
end;

procedure TDateTimeOffsetData.SetAsString(const Value: string);
begin
  FDateTimeOffset := TDateTimeOffset.Create(Value)
end;

initialization
  DateTimeOffsetVariantType := TDateTimeOffsetVariantType.Create;

finalization
  FreeAndNil(DateTimeOffsetVariantType);

end.
