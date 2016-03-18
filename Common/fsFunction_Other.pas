unit fsFunction_Other;

interface

uses
  Types, fs_iinterpreter, Variants, DateUtils, Math, frxDBSet;

{$IFDEF DEBUG}
  //-- Проверяет Instance на соответствие типу данных
  //-- Если не соответствует, то выдается ошибка!
  procedure FSCheckInstanceMethod(CallerInstance: TObject; CallerClassType: TClass; const CallerMethodName: String; Caller: TfsMethodHelper; CheckClass : TClass);
  procedure FSCheckInstanceProperty(CallerInstance: TObject; CallerClassType: TClass; const CallerPropertyName: String; CheckClass : TClass);

  //-- Сообщение об ошибке в случае если не определена реализация метода
  procedure RaiseFSNoMethod(CallerInstance: TObject; CallerClassType: TClass; const CallerMethodName: String; Caller: TfsMethodHelper);
  //-- Сообщение об ошибке в случае если не определена реализация свойства
  procedure RaiseFSNoProperty(CallerInstance: TObject; CallerClassType: TClass; const CallerPropertyName: String);
{$ENDIF}

implementation

uses
  Classes, Forms, frxClass, fs_itools, Windows, SysUtils, NamedVariables, SBaseVariantFunctions,
  ClipBrd, SBaseConstants, FMTBcd, SBaseFileUtils, SBaseUtils;

type
  TComponentCrack = class(TComponent);

{$HINTS OFF}
procedure _DebugPoint(AInstance: TObject; AClassType: TClass; ACaller: TfsMethodHelper; AParams: Array of Variant);
var
  LParamsCount  : Integer;
  LObjects      : array of TObject;
  I             : Integer;
  LParam        : Variant;
begin
{$IFDEF DEBUG}
  LParamsCount := Length(AParams);
  SetLength(LObjects, LParamsCount);
  for i := 0 to Pred(LParamsCount) do begin
    LParam := AParams[I];
    if VarType(LParam) = varInteger then
      LObjects[I] := TObject(Integer(LParam));
  end;
{$ENDIF}
end; // ... А здесь собственно и удобно поставить BreakPoint!
{$HINTS ON}

type
  TfsFunctions_Other = class(TfsRTTIModule)
  private
    function Call_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Get_Exception_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function Call_TApplication_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Get_TCollectionItem_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function Call_TCollection_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Get_TComponent_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function Call_TComponent_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function Get_TClipboard_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TClipboard_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
    function Call_TClipboard_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function Get_TApplication_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TApplication_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function Get_TStrings_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TStrings_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function Call_TfrxReport_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

function SafeClassName(AClass : TClass): String;
begin
  if Assigned(AClass) then
    Result := AClass.ClassName
  else
    Result := '/Class = NULL/';
end;

{$IFDEF DEBUG}
procedure FSCheckInstanceMethod(CallerInstance: TObject; CallerClassType: TClass; const CallerMethodName: String; Caller: TfsMethodHelper; CheckClass : TClass);
var
  LError : String;
begin
  if CallerInstance = nil then
    LError := 'Instance = NULL.'
  else if not CallerInstance.InheritsFrom(CheckClass) then
    LError := 'Класс "' + CallerInstance.ClassName + '" объекта Instance не соотвествует классу "' + SafeClassName(CheckClass) + '".'
  else
    Exit;

  raise Exception.Create('Вызов метода "' + CallerMethodName + '" для объекта (CallerClass = "' + SafeClassName(CallerClassType) + '") с несоответсвующим типом!' + SConst_CR
                       + 'Объект = 0x' + IntToHex(Integer(CallerInstance), 8) + SConst_CR
                       + LError + SConst_CR
                       + Caller.SourceUnit + SConst_CR
                       + Caller.SourcePos + SConst_CR);
end;

procedure FSCheckInstanceProperty(CallerInstance: TObject; CallerClassType: TClass; const CallerPropertyName: String; CheckClass : TClass);
var
  LError : String;
begin
  if CallerInstance = nil then
    LError := 'Instance = NULL.'
  else if not CallerInstance.InheritsFrom(CheckClass) then
    LError := 'Класс "' + CallerInstance.ClassName + '" объекта Instance не соотвествует классу "' + CheckClass.ClassName + '".'
  else
    Exit;

  raise Exception.Create('Обращение к свойству "' + CallerPropertyName + '" для объекта (CallerClass = "' + SafeClassName(CallerClassType) + '") с несоответсвующим типом!' + SConst_CR
                       + 'Объект = 0x' + IntToHex(Integer(CallerInstance), 8) + SConst_CR
                       + LError + SConst_CR);
end;

procedure RaiseFSNoMethod(CallerInstance: TObject; CallerClassType: TClass; const CallerMethodName: String; Caller: TfsMethodHelper);
begin
  raise Exception.Create('Вызов неподдерживаемого метода "' + CallerMethodName + '" для объекта (CallerClass = "' + SafeClassName(CallerClassType) + '") с типом (' + SafeClassName(CallerInstance.ClassType) + ')!' + SConst_CR
                       + 'Объект = 0x' + IntToHex(Integer(CallerInstance), 8) + SConst_CR
                       + Caller.SourcePos + SConst_CR);
end;

procedure RaiseFSNoProperty(CallerInstance: TObject; CallerClassType: TClass; const CallerPropertyName: String);
begin
  raise Exception.Create('Обращение к неподдерживаемому свойству "' + CallerPropertyName + '" для объекта (CallerClass = "' + SafeClassName(CallerClassType) + '") с типом (' + SafeClassName(CallerInstance.ClassType) + ')!' + SConst_CR
                       + 'Объект = 0x' + IntToHex(Integer(CallerInstance), 8));
end;
{$ENDIF}

{ TfsFunctions_Other }

constructor TfsFunctions_Other.Create(AScript: TfsScript);
const
  SCategory: String = 'Прочие функции';
  FCatDate: String = 'ctDate';
  FCatStr: String = 'ctString';
  FCatMath: String = 'ctMath';
  FCatOther: String = 'ctOther';
begin
  inherited Create(AScript);

  with AScript.AddRecordType('TPoint') do begin
    AddProperty('X', 'LongInt', Word(@PPoint(nil).X));
    AddProperty('Y', 'LongInt', Word(@PPoint(nil).Y));
  end;
  AScript.AddType('VPoint', fvtVariant);
  AScript.AddConst(SConst_Default, DelphiTypeVariant, Null);

  AScript.AddEnum('TDatesPeriod', 'sdkDay, sdkInterval, sdkStartMonth, sdkStartQuad, sdkStartYear, sdkMonth, sdkQuad, sdkHalfYear, sdkYear');

  AScript.AddConst('CF_TEXT', 'Word', 1);
  AScript.AddConst('CF_BITMAP', 'Word', 2);
  AScript.AddConst('CF_METAFILEPICT', 'Word', 3);
  AScript.AddConst('CF_SYLK', 'Word', 4);
  AScript.AddConst('CF_DIF', 'Word', 5);
  AScript.AddConst('CF_TIFF', 'Word', 6);
  AScript.AddConst('CF_OEMTEXT', 'Word', 7);
  AScript.AddConst('CF_DIB', 'Word', 8);
  AScript.AddConst('CF_PALETTE', 'Word', 9);
  AScript.AddConst('CF_PENDATA', 'Word', 10);
  AScript.AddConst('CF_RIFF', 'Word', 11);
  AScript.AddConst('CF_WAVE', 'Word', 12);
  AScript.AddConst('CF_UNICODETEXT', 'Word', 13);
  AScript.AddConst('CF_ENHMETAFILE', 'Word', 14);
  AScript.AddConst('CF_HDROP', 'Word', 15);
  AScript.AddConst('CF_LOCALE', 'Word', $10);
  AScript.AddConst('CF_MAX', 'Word', 17);
  AScript.AddConst('CF_DIBV5', 'Word', 17);
  AScript.AddConst('CF_MAX_XP', 'Word', 18);
  AScript.AddConst('CF_OWNERDISPLAY', 'Word', 128);
  AScript.AddConst('CF_DSPTEXT', 'Word', 129);
  AScript.AddConst('CF_DSPBITMAP', 'Word', 130);
  AScript.AddConst('CF_DSPMETAFILEPICT', 'Word', 131);
  AScript.AddConst('CF_DSPENHMETAFILE', 'Word', 142);
  AScript.AddConst('CF_PRIVATEFIRST', 'Word', $200);
  AScript.AddConst('CF_PRIVATELAST', 'Word', 767);
  AScript.AddConst('CF_GDIOBJFIRST', 'Word', 768);
  AScript.AddConst('CF_GDIOBJLAST', 'Word', 1023);

  with AScript do begin
    AddMethod('porcedure _DebugPoint(AParams : array of Variant)', Call_Method);

    AddMethod('function ParamCount: Integer', Call_Method);
    AddMethod('function ParamStr(Index: Integer): String', Call_Method);
    AddMethod('procedure SetApplicationExeName(AExeName: String)', Call_Method);

    AddMethod('function Clipboard: TClipboard', Call_Method);

    AddMethod('function IncHour(Date: TDateTime; const Hours: Integer = 1): TDateTime', Call_Method, FCatDate);
    AddMethod('function IncMinute(Date: TDateTime; const Minutes: Integer = 1): TDateTime', Call_Method, FCatDate);
    AddMethod('function IncSecond(Date: TDateTime; const Seconds: Integer = 1): TDateTime', Call_Method, FCatDate);
    AddMethod('function IncDay(Date: TDateTime; const Days: Integer = 1): TDateTime', Call_Method, FCatDate);
    AddMethod('function IncMonth(Date: TDateTime; const Months: Integer = 1): TDateTime', Call_Method, FCatDate);
    AddMethod('function IncYear(Date: TDateTime; const Years: Integer = 1): TDateTime', Call_Method, FCatDate);

    AddMethod('function YearsBetween(const ANow, AThen: TDateTime): Integer', Call_Method, FCatDate);
    AddMethod('function MonthsBetween(const ANow, AThen: TDateTime): Integer', Call_Method, FCatDate);
    AddMethod('function WeeksBetween(const ANow, AThen: TDateTime): Integer', Call_Method, FCatDate);
    AddMethod('function DaysBetween(const ANow, AThen: TDateTime): Integer', Call_Method, FCatDate);
    AddMethod('function HoursBetween(const ANow, AThen: TDateTime): Int64', Call_Method, FCatDate);
    AddMethod('function MinutesBetween(const ANow, AThen: TDateTime): Int64', Call_Method, FCatDate);
    AddMethod('function SecondsBetween(const ANow, AThen: TDateTime): Int64', Call_Method, FCatDate);
    AddMethod('function MilliSecondsBetween(const ANow, AThen: TDateTime): Int64', Call_Method, FCatDate);
    AddMethod('function RecodeTime(const AValue: TDateTime; const AHour, AMinute: Word; ASecond: Word = 0; AMilliSecond: Word = 0): TDateTime', Call_Method, FCatDate);

    AddMethod('function StartMonth(const ADateTime: TDateTime): TDate', Call_Method, FCatDate);

    AddMethod('procedure Abort', Call_Method, FCatOther);
    AddMethod('procedure FreeAndNil(var Obj: Pointer)', Call_Method, FCatOther);
  end;

  with AScript.AddClass(Exception, Exception.ClassParent.ClassName) do begin
    AddProperty('Message', DelphiTypeString, Get_Exception_Property);
  end;

  with AScript.AddClass(TApplication, TApplication.ClassParent.ClassName) do begin
    AddProperty('Title', DelphiTypeString, Get_TApplication_Property, Set_TApplication_Property);
    AddMethod('procedure Terminate', Call_TApplication_Method);
  end;

  with AScript.AddClass(TStrings, TStrings.ClassParent.ClassName) do begin
    AddProperty('Delimiter', DelphiTypeChar, Get_TStrings_Property, Set_TStrings_Property);
    AddProperty('DelimitedText', DelphiTypeString, Get_TStrings_Property, Set_TStrings_Property);
  end;

  with AScript.AddClass(TCollectionItem, TCollectionItem.ClassParent.ClassName) do begin
    AddProperty('Id', DelphiTypeInteger, Get_TCollectionItem_Property, nil);
    AddProperty('Index', DelphiTypeInteger, Get_TCollectionItem_Property, nil);
  end;

  with AScript.AddClass(TCollection, TCollection.ClassParent.ClassName) do begin
    AddMethod('procedure Clear', Call_TCollection_Method);
    AddMethod('function Add: TCollectionItem', Call_TCollection_Method);
    AddMethod('procedure Delete(Index: Integer)', Call_TCollection_Method);
    AddMethod('procedure BeginUpdate', Call_TCollection_Method);
    AddMethod('procedure EndUpdate', Call_TCollection_Method);
  end;

  AScript.AddClass(TOwnedCollection, TOwnedCollection.ClassParent.ClassName);

  with AScript.AddClass(TComponent, 'TPersistent') do begin
    AddProperty('ComponentCount', DelphiTypeInteger, Get_TComponent_Property, nil);
    AddIndexProperty('Components', DelphiTypeInteger, 'TComponent', Call_TComponent_Method, True);
    AddMethod('function FindComponent(AComponentName: String): TComponent', Call_TComponent_Method);
    AddMethod('procedure SetParentComponent(AComponent: TComponent)', Call_TComponent_Method);
  end;

  with AScript.AddClass(TClipboard, TClipboard.ClassParent.ClassName) do begin
    AddMethod('procedure Assign(Source: TPersistent)', Call_TClipboard_Method);
    AddMethod('procedure Clear', Call_TClipboard_Method);
    AddMethod('procedure Close', Call_TClipboard_Method);
    AddMethod('function GetComponent(Owner, Parent: TComponent): TComponent', Call_TClipboard_Method);
    AddMethod('function GetAsHandle(Format: Word): THandle', Call_TClipboard_Method);
    AddMethod('function HasFormat(Format: Word): Boolean', Call_TClipboard_Method);
    AddMethod('procedure Open', Call_TClipboard_Method);
    AddMethod('procedure SetComponent(Component: TComponent)', Call_TClipboard_Method);
    AddMethod('procedure SetAsHandle(Format: Word; Value: THandle)', Call_TClipboard_Method);

    AddProperty('AsText', DelphiTypeString, Get_TClipboard_Property, Set_TClipboard_Property);
    AddProperty('FormatCount', DelphiTypeInteger, Get_TClipboard_Property, nil);
    AddIndexProperty('property Formats', DelphiTypeInteger, 'Word', Call_TClipboard_Method, True);
  end;

  with AScript.AddClass(TfrxReport, TfrxReport.ClassParent.ClassName) do begin
    AddMethod('function DataSetLookup(ADataSetName, AKeyFieldName: String; AKeyValue: Variant; AResultField: String): Variant', Call_TfrxReport_Method);
    AddMethod('function FindDataSet(const DSName: String): TfrxDataSet', Call_TfrxReport_Method);
  end;
end;

function TfsFunctions_Other.Get_TApplication_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'TITLE' then
    Result := TApplication(Instance).Title
end;

function TfsFunctions_Other.Get_Exception_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'MESSAGE' then
    Result := Exception(Instance).Message;
end;

function TfsFunctions_Other.Get_TClipboard_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TClipboard);
{$ENDIF}
  if PropName = 'ASTEXT' then
    if TClipboard(Instance).HasFormat(CF_TEXT) then
      Result := TClipboard(Instance).AsText
    else
      Result := Null
  else if PropName = 'FORMATCOUNT' then
    Result := TClipboard(Instance).FormatCount
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TfsFunctions_Other.Get_TCollectionItem_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'ID' then
    Result := TCollectionItem(Instance).ID
  else if PropName = 'INDEX' then
    Result := TCollectionItem(Instance).Index
end;

function TfsFunctions_Other.Get_TComponent_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'COMPONENTCOUNT' then
    Result := TComponent(Instance).ComponentCount
end;

function TfsFunctions_Other.Get_TStrings_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'DELIMITER' then
    Result := TStrings(Instance).Delimiter
  else if PropName = 'DELIMITEDTEXT' then
    Result := TStrings(Instance).DelimitedText;
end;

procedure TfsFunctions_Other.Set_TApplication_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
  if PropName = 'TITLE' then
    TApplication(Instance).Title := Value
end;

procedure TfsFunctions_Other.Set_TClipboard_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TClipboard);
{$ENDIF}
  if PropName = 'ASTEXT' then
    TClipboard(Instance).AsText := PropName
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TfsFunctions_Other.Set_TStrings_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TStrings);
{$ENDIF}
  if PropName = 'DELIMITER' then
    TStrings(Instance).Delimiter := VarToStr(Value)[1]
  else if PropName = 'DELIMITEDTEXT' then
    TStrings(Instance).DelimitedText := VarToStr(Value)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TfsFunctions_Other.Call_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  LParams: Array of Variant;
begin
  if MethodName = 'PARAMCOUNT' then
    Result := Length(SBaseFileUtils.ApplicationParams)
  else if MethodName = 'PARAMSTR' then
    Result := SBaseFileUtils.ApplicationParams[Integer(Caller.Params[0])]
  else if MethodName = 'SETAPPLICATIONEXENAME' then
    SBaseFileUtils.SetApplicationExeName(Caller.Params[0])

  else if MethodName = 'CLIPBOARD' then
    Result := Integer(Pointer(Clipboard))

  else if MethodName = 'INCHOUR' then
    Result := IncHour(Caller.Params[0], VarToIntDef(Caller.Params[1], 1))
  else if MethodName = 'INCMINUTE' then
    Result := IncMinute(Caller.Params[0], VarToIntDef(Caller.Params[1], 1))
  else if MethodName = 'INCSECOND' then
    Result := IncSecond(Caller.Params[0], VarToIntDef(Caller.Params[1], 1))

  else if MethodName = 'INCDAY' then
    Result := IncDay(Caller.Params[0], VarToIntDef(Caller.Params[1], 1))
  else if MethodName = 'INCMONTH' then
    Result := IncMonth(Caller.Params[0], VarToIntDef(Caller.Params[1], 1))
  else if MethodName = 'INCYEAR' then
    Result := IncYear(Caller.Params[0], VarToIntDef(Caller.Params[1], 1))

  else if MethodName = 'YEARSBETWEEN' then
    Result := YearsBetween(VarToDateTime(Caller.Params[0]), VarToDateTime(Caller.Params[1]))
  else if MethodName = 'MONTHSBETWEEN' then
    Result := MonthsBetween(VarToDateTime(Caller.Params[0]), VarToDateTime(Caller.Params[1]))
  else if MethodName = 'WEEKSBETWEEN' then
    Result := WeeksBetween(VarToDateTime(Caller.Params[0]), VarToDateTime(Caller.Params[1]))
  else if MethodName = 'DAYSBETWEEN' then
    Result := DaysBetween(VarToDateTime(Caller.Params[0]), VarToDateTime(Caller.Params[1]))

  else if MethodName = 'HOURSBETWEEN' then
    Result := HoursBetween(VarToDateTime(Caller.Params[0]), VarToDateTime(Caller.Params[1]))
  else if MethodName = 'MINUTESBETWEEN' then
    Result := MinutesBetween(VarToDateTime(Caller.Params[0]), VarToDateTime(Caller.Params[1]))
  else if MethodName = 'SECONDSBETWEEN' then
    Result := SecondsBetween(VarToDateTime(Caller.Params[0]), VarToDateTime(Caller.Params[1]))
  else if MethodName = 'MILLISECONDSBETWEEN' then
    Result := MilliSecondsBetween(VarToDateTime(Caller.Params[0]), VarToDateTime(Caller.Params[1]))
  else if MethodName = 'RECODETIME' then
    Result := RecodeTime(VarToDateTime(Caller.Params[0]), Caller.Params[1], Caller.Params[2], VarToIntDef(Caller.Params[3], 0), VarToIntDef(Caller.Params[4], 0))

  else if MethodName = 'ABORT' then
    Abort
  else if MethodName = 'FREEANDNIL' then begin
    if VarIsOrdinal(Caller.Params[0]) then
      TObject(Integer(Caller.Params[0])).Free;
    Caller.Params[0] := 0;
  end

  else if MethodName = 'STARTMONTH' then
    Result := VarFromDate(StartMonth(TDateTime(Caller.Params[0])))

  else if MethodName = '_DEBUGPOINT' then begin
    LParams := Caller.Params[0];
    _DebugPoint(Instance, ClassType, Caller, LParams);
  end;
end;

function TfsFunctions_Other.Call_TApplication_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'TERMINATE' then
    TApplication(Instance).Terminate
end;

function TfsFunctions_Other.Call_TClipboard_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, MethodName, TClipboard);
{$ENDIF}
  if MethodName = 'ASSIGN' then
    TClipboard(Instance).Assign( TPersistent(Pointer(Integer(Caller.Params[0]))) )
  else if MethodName = 'CLEAR' then
    TClipboard(Instance).Clear
  else if MethodName = 'CLOSE' then
    TClipboard(Instance).Close
  else if MethodName = 'GETCOMPONENT' then
    Result := Integer(Pointer( TClipboard(Instance).GetComponent( TComponent(Pointer(Integer(Caller.Params[0]))), TComponent(Pointer(Integer(Caller.Params[1]))) ) ))
  else if MethodName = 'GETASHANDLE' then
    Result := TClipboard(Instance).GetAsHandle( Word(Caller.Params[0]) )
  else if MethodName = 'HASFORMAT' then
    Result := TClipboard(Instance).HasFormat( Word(Caller.Params[0]) )
  else if MethodName = 'OPEN' then
    TClipboard(Instance).Open
  else if MethodName = 'SETCOMPONENT' then
    TClipboard(Instance).SetComponent( TComponent(Pointer(Integer(Caller.Params[0]))) )
  else if MethodName = 'SETASHANDLE' then
    TClipboard(Instance).SetAsHandle( Word(Caller.Params[0]), THandle(Caller.Params[1]) )
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, MethodName);
{$ENDIF}
end;

function TfsFunctions_Other.Call_TCollection_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, MethodName, TCollection);
{$ENDIF}
  if MethodName = 'ADD' then
    Result := Integer(TCollection(Instance).Add)
  else if MethodName = 'CLEAR' then
    TCollection(Instance).Clear
  else if MethodName = 'DELETE' then
    TCollection(Instance).Delete(Caller.Params[0])
  else if MethodName = 'BEGINUPDATE' then
    TCollection(Instance).BeginUpdate
  else if MethodName = 'ENDUPDATE' then
    TCollection(Instance).EndUpdate
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, MethodName);
{$ENDIF}
end;

function TfsFunctions_Other.Call_TComponent_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'COMPONENTS.GET' then
    Result := Integer(TComponent(Instance).Components[Integer(Caller.Params[0])])
  else if MethodName = 'FINDCOMPONENT' then
    Result := Integer(TComponent(Instance).FindComponent(Caller.Params[0]))
  else if MethodName = 'SETPARENTCOMPONENT' then
    TComponentCrack(Instance).SetParentComponent(VarToPointer(Caller.Params[0]))
end;

function TfsFunctions_Other.Call_TfrxReport_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  LfrxDataSet: TfrxComponent;
begin
  if MethodName = 'DATASETLOOKUP' then begin
    LfrxDataSet := TfrxReport(Instance).FindDataSet(nil, Caller.Params[0]);
    if Assigned(LfrxDataSet) and (LfrxDataSet is TfrxDBDataSet) then
      Result := TfrxDBDataSet(LfrxDataSet).DataSet.Lookup(Caller.Params[1], Caller.Params[2], Caller.Params[3])
    else
      Result := NULL;
  end else if MethodName = 'FINDDATASET' then
    Result := Integer(TfrxReport(Instance).FindDataSet(nil, Caller.Params[0]));
end;

initialization
  fsRTTIModules.Add(TfsFunctions_Other);

finalization
  fsRTTIModules.Remove(TfsFunctions_Other);

end.
