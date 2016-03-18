
{******************************************}
{                                          }
{             FastScript v1.8              }
{            Extended functions            }
{                                          }
{             Fast Reports Inc             }
{                                          }
{                                          }
{******************************************}

unit fsFunctions_Ru;

interface

uses
  Types, Windows;

type
  TGenders = (gndMale, gndFemale, gndNeutral);

var
  Months:  array [1..12] of String = ('январь', '‘евраль', 'ћарт', 'јпрель',
    'ћай', '»юнь', '»юль', 'јвгуст', '—ент€брь', 'ќкт€брь', 'Ќо€брь', 'ƒекабрь');

function  Month_r(AMonth: Byte): String;
function  GetStrDate(Date: TDate; isNice: Boolean = False): String;
function  GetStrInt(N: Int64; Gender: TGenders = gndMale): String;
function  GetStrFloat(F: Extended; Gender: TGenders = gndMale; const W: String = ''; const eSingle: String = ''; const eSome: String = ''; const eMulti: String = ''): String;
function  GetRusEnding(n: Integer; o: Array of String): String;

implementation

uses
  SysUtils, Classes, DateUtils, frxClass, fs_iinterpreter, fs_itools,
  SBaseConstants;

type
  TFunctions_Ru = class(TfsRTTIModule)
  private
    function CallMethod(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

var
  Decimals1: array [0..39] of String = ('ноль','один','два','три','четыре','п€ть','шесть','семь','восемь','дев€ть','дес€ть','одиннадцать','двенадцать','тринадцать','четырнадцать','п€тнадцать','шестнадцать','семнадцать','восемнадцать','дев€тнадцать','двадцать','тридцать','сорок','п€тьдес€т','шестьдес€т','семьдес€т','восемьдес€т','дев€носто','сто','двести','триста','четыреста','п€тьсот','шестьсот','семьсот','восемьсот','дев€тьсот','тыс€ч','миллион','миллиард');
  Decimals2: array [1..2] of String = ('одна','две');
  Decimals3: array [1..2] of String = ('одно','два');
  Decimals4: array [1..6] of String = ('дес€т','сот','тыс€чн','дес€титыс€чн','стотыс€чн','миллионн');
  DecEnds  : array [0..1] of String = ('а€','ых');

function Month_r(AMonth: Byte): String;
begin
  Result := Months[AMonth];
  if AMonth in [3,8] then
    Result := Result + 'а'
  else
    Result[Length(Result)] := '€';
end;

function GetStrDate(Date: TDate; isNice: Boolean = False): String;
begin
  Result := IntToStr(DayOf(Date));
  if isNice then Result := #171 + Result + #187;
  Result := Result + ' ' + AnsiLowerCase(Month_r(MonthOf(Date)));
  Result := Result + ' ' + IntToStr(YearOf(Date)) + ' г.'
end;

function GetStrInt(n: Int64; Gender: TGenders = gndMale): String;
const
  _dec_Des      = 18;
  _dec_Sot      = 27;
  _dec_Tis      = 37;
  _dec_Million  = 38;
  _dec_Milliard = 39;
var
  nn: Integer;
  s: String;
begin
  if n<=0 then begin
    Result := Decimals1[0];
    CharUpperBuff(@Result[1], 1)
  end else begin
      Result := '';
        nn := n mod 1000;
        if nn>=100 then begin Result := Decimals1[_dec_Sot+(nn div 100)]; nn := nn mod 100 end;
        if nn>20 then begin Result := Result + ' ' + Decimals1[_dec_Des+(nn div 10)]; nn := nn mod 10 end;
        if nn>2 then Result := Result + ' ' + Decimals1[nn]
        else if nn>0 then begin
          case Gender of
            gndMale: Result := Result + ' ' + Decimals1[nn];
            gndFemale: Result := Result + ' ' + Decimals2[nn];
            gndNeutral: Result := Result + ' ' + Decimals3[nn];
          end;
        end;
        Result := TrimLeft(Result);

      n := n div 1000;
      nn := n mod 1000;
      if nn>0 then begin
        s := GetStrInt(nn,gndFemale) + ' ' + Decimals1[_dec_Tis] + GetRusEnding(nn,['а','и','']);
        Result := s + ' ' + Result;
      end;

      n := n div 1000;
      nn := n mod 1000;
      if nn>0 then begin
        s := GetStrInt(nn) + ' ' + Decimals1[_dec_Million] + GetRusEnding(nn,['','а','ов']);
        Result := s + ' ' + Result;
      end;

      n := n div 1000;
      nn := n mod 1000;
      if nn>0 then begin
        s := GetStrInt(nn) + ' ' + Decimals1[_dec_Milliard] + GetRusEnding(nn,['','а','ов']);
        Result := s + ' ' + Result;
      end;

      Result := Result.Trim;
      CharUpperBuff(@Result[1], 1);
   end;
end;

function GetStrFloat(f: Extended; Gender: TGenders = gndMale; const W: String = ''; const eSingle: String = ''; const eSome: String = ''; const eMulti: String = ''): String;
var
  i,j: Integer;
begin
  i := Trunc(f);
  f := f - i; j := 0;

  while (j<=5) and (f-Trunc(f)<>0) do begin Inc(j); f := f * 10.0 end;
  if j>0 then begin
    Result := GetStrInt(i,gndFemale) + ' цел' + GetRusEnding(i,DecEnds) + ' ';
    i := Round(f); while (i>0) and ((i mod 10)=0) do begin i := i div 10; Dec(j) end;
    Result := Result + IntToStr(i) + ' ' + Decimals4[j] + GetRusEnding(i,DecEnds);
  end else Result := GetStrInt(i,Gender);

  if W <> '' then
    Result := Result + ' ' + W + GetRusEnding(i,[eSingle,eSome,eMulti]);
end;

function GetRusEnding(n: Integer; o: Array of String): String;
var
  d,e: Integer;
begin
  d := n mod 100;
  e := d mod 10;
  if Length(o)>2 then Result := o[2] else Result := o[1];
  if (d<10) or (d>20) then begin
    if e = 1 then Result := o[0]
      else if (e>1) and (e<5) then Result := o[1]
  end
end;

{ TFunctions_Ru }

constructor TFunctions_Ru.Create(AScript: TfsScript);
const
  SCategory: String = '–усификаци€';
begin
  inherited Create(AScript);

  with AScript do begin
    AddConst('gndMale', DelphiTypeInteger, 0);
    AddConst('gndFemale', DelphiTypeInteger, 1);
    AddConst('gndNeutral', DelphiTypeInteger, 2);

    AddMethod('function IntToStrRus(i: Integer; Gender: Integer): String', CallMethod, SCategory);
    AddMethod('function FloatToStrRus(e: Extended; Gender: Integer; W: String; eSingle: String; eSome: String; eMulti: String): String', CallMethod, SCategory);
    AddMethod('function DateRus(Date: TDateTime): String', CallMethod, SCategory);
  end;
end;

function TFunctions_Ru.CallMethod(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'INTTOSTRRUS' then
    Result := GetStrInt(Caller.Params[0], TGenders(Caller.Params[1]))

  else if MethodName = 'FLOATTOSTRRUS' then
    Result := GetStrFloat(Caller.Params[0], Caller.Params[1], Caller.Params[2], Caller.Params[3], Caller.Params[4], Caller.Params[5])

  else if MethodName = 'DATERUS' then
    Result := GetStrDate(Caller.Params[0], True)
end;

initialization
  fsRTTIModules.Add(TFunctions_Ru);

end.
