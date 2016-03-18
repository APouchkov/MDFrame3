unit SBaseMathFunctions;
{$I config.inc}

interface

uses
  Math, FMTBcd
{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter
{$ENDIF}
;

type
  TFPURoundingModeEx = (rmExNearestMath, rmExNearestBank, rmExDown, rmExUp, rmExTruncate);

  function RoundToEx(const AValue: Extended; const ADigit: ShortInt; const ARoundMode: TFPURoundingModeEx = rmExNearestMath): Extended;
  function TDateTimeToTDate(AValue: TDateTime): TDate; inline;

{$IFDEF FASTSCRIPT_RTTI}
type
  TfsFunctions_Math = class(TfsRTTIModule)
  private
    function Call_Math_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

implementation

function RoundToEx(const AValue: Extended; const ADigit: ShortInt; const ARoundMode: TFPURoundingModeEx = rmExNearestMath): Extended;
var
  LPower, LPowerResult, LLossResult: Extended;
begin
  if ARoundMode = rmExNearestBank then
    Result := Math.RoundTo(AValue, ADigit)
  else begin
    LPower := IntPower(10, ADigit);
    LPowerResult := AValue / LPower;
    Result := Int(LPowerResult);
    LLossResult := LPowerResult - Result;

    if LLossResult <> 0.0 then
      case ARoundMode of
        rmExNearestMath:
          if LLossResult >= 0.5 then
            Result := Result + 1.0
          else if LLossResult <= -0.5 then
            Result := Result - 1.0;
        rmExDown:
          if LPowerResult < 0 then
            Result := Result - 1.0;
        rmExUp:
          if LPowerResult > 0 then
            Result := Result + 1.0;
  {
        rmTruncate:
  }
      end;

    Result := Result * LPower;
  end;
end;

function TDateTimeToTDate(AValue: TDateTime): TDate;
begin
  Result := Floor(AValue)
end;

{$IFDEF FASTSCRIPT_RTTI}
{ TfsFunctions_Math }

constructor TfsFunctions_Math.Create(AScript: TfsScript);
const
  SCategory: String = 'Математические функции';
begin
  inherited;

  with AScript do begin
    AddEnum('TFPURoundingModeEx', 'rmExNearestMath, rmExNearestBank, rmExDown, rmExUp, rmExTruncate');

    AddMethod('function Floor(e: Extended): Integer', Call_Math_Method, SCategory);
    AddMethod('function RoundTo(e: Extended; const ADigit: Integer): Integer', Call_Math_Method, SCategory);
    AddMethod('function RoundToEx(const AValue: Extended; const ADigit: ShortInt; const ARoundMode: TFPURoundingModeEx = rmExNearestMath): Extended', Call_Math_Method, SCategory);
{$IFDEF DELPHIXE3}
    AddMethod('function BcdRoundTo(AValue: Variant; APosition: Integer): String', Call_Math_Method, SCategory);
{$ENDIF}
  end;
end;

function TfsFunctions_Math.Call_Math_Method(Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'FLOOR' then
    Result := Floor(Caller.Params[0])
  else if MethodName = 'ROUNDTO' then
    Result := RoundTo(Caller.Params[0], Caller.Params[1])
  else if MethodName = 'ROUNDTOEX' then
    Result := RoundToEx(Caller.Params[0], Caller.Params[1], Caller.Params[2])
{$IFDEF DELPHIXE3}
  else if MethodName = 'BCDROUNDTO' then
    Result := VarFMTBcdCreate(BcdRoundTo(VarToBcd(Caller.Params[0]), Caller.Params[1]))
{$ENDIF}
end;

{$ENDIF}

initialization
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_Math);
{$ENDIF}

end.
