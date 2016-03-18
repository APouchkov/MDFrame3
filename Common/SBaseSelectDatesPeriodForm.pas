unit SBaseSelectDatesPeriodForm;
{$I config.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ComCtrls, ExtCtrls, DateUtils, StrUtils, NamedVariables

{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter
{$ENDIF}

  , SBaseForm, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxTextEdit, cxMaskEdit,
  cxDropDownEdit, cxCalendar, dxBar, dxCore, cxDateUtils, System.Actions,
  ActnList, cxClasses, dxSkinsCore, dxSkinsDefaultPainters, dxMDBar;

type
  TDatesPeriod  = (sdkDay, sdkInterval, sdkStartMonth, sdkStartQuad, sdkStartYear, sdkMonth, sdkQuad, sdkHalfYear, sdkYear);
  TSelectDatesPeriodType  = (sdtYear, sdtHalfYear, sdtQuad, sdtMonth, sdtDay);
  TSelectDatesPeriodTypes = Set Of TSelectDatesPeriodType;
  
  TSBaseSelectDatesPeriodFrm = class(TSBaseFrm)
    tInfo: TPanel;
    t1: TRadioButton;
    t2: TRadioButton;
    t3: TRadioButton;
    t4: TRadioButton;
    f1: TEdit;
    f2: TEdit;
    UD1: TUpDown;
    UD2: TUpDown;
    btnOk: TButton;
    btnCancel: TButton;
    t5: TRadioButton;
    t6: TRadioButton;
    t7: TRadioButton;
    t8: TRadioButton;
    f8: TEdit;
    ud8: TUpDown;
    t9: TRadioButton;
    f9: TEdit;
    ud9: TUpDown;
    f3: TcxDateEdit;
    f41: TcxDateEdit;
    f42: TcxDateEdit;
    procedure FormShow(Sender: TObject); override;
    procedure ReDraw();
    procedure tXClick(Sender: TObject);
    procedure t1Show();
    procedure t2Show();
    procedure t8Show();
    procedure t9Show();
    procedure UD1Click(Sender: TObject; Button: TUDBtnType);
    procedure UD2Click(Sender: TObject; Button: TUDBtnType);
    procedure UD8Click(Sender: TObject; Button: TUDBtnType);
    procedure UD9Click(Sender: TObject; Button: TUDBtnType);
    procedure ShowCaption();
    procedure f3Change(Sender: TObject);
    procedure f41Change(Sender: TObject);
    procedure f42Change(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure f1Click(Sender: TObject);
    procedure f2Click(Sender: TObject);
    procedure f3Click(Sender: TObject);
    procedure f41Click(Sender: TObject);
    procedure f42Click(Sender: TObject);
  private
    { Private declarations }
    dT1,dT2,dT3,dT41,dT42,dT5,dT6,dT7,dT8,dT9: TDate;
  protected
    procedure InternalCreate; override;
  public
    { Public declarations }
    DatesPeriod: TDatesPeriod;
    DateBegin, DateEnd: TDate;
  end;

{$IFDEF FASTSCRIPT_RTTI}
  TFunctions_TSelectDate = class(TfsRTTIModule)
  private
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

function SelectDatesPeriod(var ADatesPeriod: TDatesPeriod; var ADateBegin, ADateEnd: TDate): Boolean;
function SelectDatesPeriodLimited(SelectDateTypes: TSelectDatesPeriodTypes; var ADatesPeriod: TDatesPeriod; var ADateBegin, ADateEnd: TDate): Boolean;

implementation

uses
  SBaseVariantFunctions, SBaseConstants, fsFunctions_Ru;

{$R *.dfm}

function SelectDatesPeriod(var ADatesPeriod: TDatesPeriod; var ADateBegin, ADateEnd: TDate): Boolean;
var
  LResult: TNamedVariants;
begin
  LResult := TSBaseSelectDatesPeriodFrm.Select
  (
    Application.MainForm,
    [
      TNamedVariant.Create(SConst_DateBegin , ADateBegin),
      TNamedVariant.Create(SConst_DateEnd   , ADateEnd),
      TNamedVariant.Create(SConst_DatesPeriod, ADatesPeriod)
    ]
  );

  if LResult.Count > 0 then begin
    ADateBegin    := LResult[SConst_DateBegin];
    ADateEnd      := LResult[SConst_DateEnd];
    ADatesPeriod  := LResult[SConst_DatesPeriod];

    Result := True;
  end else
    Result := False;
end;

function SelectDatesPeriodLimited(SelectDateTypes: TSelectDatesPeriodTypes; var ADatesPeriod: TDatesPeriod; var ADateBegin, ADateEnd: TDate): Boolean;
var
  LResult: TNamedVariants;
begin
  LResult := TSBaseSelectDatesPeriodFrm.Select
  (
    Application.MainForm,
    [
      TNamedVariant.Create(SConst_Limit, VarFromSet(SelectDateTypes, SizeOf(TSelectDatesPeriodTypes))),
      TNamedVariant.Create(SConst_DatesPeriod, ADatesPeriod),
      TNamedVariant.Create(SConst_DateBegin, ADateBegin),
      TNamedVariant.Create(SConst_DateEnd, ADateEnd)
    ]
  );
  
  if LResult.Count > 0 then begin
    Result := True;

    ADateBegin    := LResult[SConst_DateBegin];
    ADateEnd      := LResult[SConst_DateEnd];
    ADatesPeriod  := LResult[SConst_DatesPeriod];
  end else
    Result := False;
end;

procedure TSBaseSelectDatesPeriodFrm.InternalCreate;
var
  MonthBegin, MonthEnd, MonthNow: Byte;
  Year, Month, Day : Word;
  Idx: Integer;
  LSelectDateTypes: TSelectDatesPeriodTypes;
begin
  inherited;
  ResultFields := SConst_DateBegin + ';' + SConst_DateEnd + ';' + SConst_DatesPeriod;

  DatesPeriod := IsNull(CreateFormParams[SConst_DatesPeriod], sdkDay);
  if CreateFormParams.Find(SConst_DateBegin, Idx) then begin
    DateBegin := CreateFormParams.Items[Idx].Value;
    if CreateFormParams.Find(SConst_DateEnd, Idx) then
      DateEnd := CreateFormParams.Items[Idx].Value;
  end else
    DateBegin := Date;

  if (DatesPeriod = sdkInterval) and
     (YearOf(DateBegin) = YearOf(DateEnd)) and
     (DayOf(DateBegin) = 1) and (DayOf(DateEnd + 1) = 1)
  then begin
    MonthBegin := MonthOf(DateBegin);
    MonthEnd   := MonthOf(DateEnd);
    MonthNow   := MonthOf(Now);

    if MonthBegin = MonthEnd then
      if MonthBegin = MonthNow then
        DatesPeriod := sdkStartMonth
      else
        DatesPeriod := sdkMonth
    else if (MonthBegin = 1) and (MonthEnd = 12) then
      if YearOf(DateBegin) = YearOf(Now) then
        DatesPeriod := sdkStartYear
      else begin
      end
    else if ((Pred(MonthBegin) div 3)=(Pred(MonthEnd) div 3)) and ((MonthBegin mod 3) = 1) and ((MonthEnd mod 3) = 0) then
      if (Pred(MonthBegin) div 3)=(Pred(MonthNow) div 3) then
        DatesPeriod := sdkStartQuad
      else
        DatesPeriod := sdkQuad
  end;

  dT3  := Today;
  dT41 := dT3; dT42 := dT3;
  DecodeDate(dT3,Year,Month,Day);
  dT2 := EncodeDate(Year, Month, 1); dT7 := dT2;
  dT1 := EncodeDate(Year, ((Month-1) div 3) * 3 + 1, 1); dT6 := dT1;
  dT5 := EncodeDate(Year, 1, 1);
  dT8 := EncodeDate(Year, 1, 1);
  dT9 := EncodeDate(Year, ((Month-1) div 6) * 6 + 1, 1);

  DecodeDate(DateBegin, Year, Month, Day);
  DateBegin := RecodeTime(DateBegin, 0, 0, 0, 0);

  case DatesPeriod of
    sdkYear: begin DateBegin := RecodeDate(DateBegin, YearOf(DateBegin), 1, 1); dT8 := DateBegin; DateEnd := IncYear(DateBegin) end;
    sdkHalfYear: begin DateBegin := RecodeDate(DateBegin,Year,((Month-1) div 6)*6+1,1); dT9 := DateBegin; DateEnd := IncMonth(DateBegin,6) end;
    sdkQuad: begin DateBegin := RecodeDate(DateBegin,Year,((Month-1) div 3)*3+1,1); dT1 := DateBegin; DateEnd := IncMonth(DateBegin,3) end;
    sdkMonth: begin DateBegin := RecodeDay(DateBegin,1); dT2 := DateBegin; DateEnd := IncMonth(DateBegin) end;
    sdkDay: begin dT3 := DateBegin; DateEnd := IncDay(DateBegin); end;
    sdkInterval: begin dT41:= DateBegin; dT42 := DateEnd; end;
    sdkStartYear: begin DateBegin := dT5; DateEnd := IncYear(dT5) end;
    sdkStartQuad: begin DateBegin := dT6; DateEnd := IncMonth(dT6,3) end;
    sdkStartMonth: begin DateBegin := dT7; DateEnd := IncMonth(dT7) end;
    else begin DateBegin := dT3; DateEnd := IncDay(DateBegin); DatesPeriod := sdkDay; end;
  end;

  t1Show();
  t2Show();
  t8Show();
  t9Show();
  f3.EditValue := dT3;
  f41.EditValue:= dT41; f42.EditValue := dT42;


  if CreateFormParams.Find(SConst_Limit, Idx) then begin
    VarToSet( CreateFormParams.Items[Idx].Value, LSelectDateTypes, SizeOf(TSelectDatesPeriodTypes) );

    t8.Visible := True; f8.Visible := True; ud8.Visible := True; t8.Left := t1.Left; t8.Top := t1.Top; f8.Left := f1.Left; f8.Top := f1.Top; ud8.Left := ud1.Left; ud8.Top := ud1.Top;
    t8.Enabled := sdtYear in LSelectDateTypes; t8.Checked := DatesPeriod = sdkYear;
    t9.Visible := True; f9.Visible := True; ud9.Visible := True; t9.Left := t2.Left; t9.Top := t2.Top; f9.Left := f2.Left; f9.Top := f2.Top; ud9.Left := ud2.Left; ud9.Top := ud2.Top;
    t9.Enabled := sdtHalfYear in LSelectDateTypes; t9.Checked := DatesPeriod = sdkHalfYear;
    t1.Enabled := sdtQuad  in LSelectDateTypes; t1.Checked := DatesPeriod = sdkQuad ; t1.Top := t1.Top + 50; f1.Top := f1.Top + 50; ud1.Top := ud1.Top + 50;
    t2.Enabled := sdtMonth in LSelectDateTypes; t2.Checked := DatesPeriod = sdkMonth; t2.Top := t2.Top + 50; f2.Top := f2.Top + 50; ud2.Top := ud2.Top + 50;
    t3.Enabled := sdtDay   in LSelectDateTypes; t3.Checked := DatesPeriod = sdkDay  ; t3.Top := t3.Top + 50; f3.Top := f3.Top + 50;
    t4.Visible := False; f41.Visible := False; f42.Visible := False;
    t5.Enabled := False;
    t6.Enabled := False;
    t7.Enabled := False;
  end;
end;

procedure TSBaseSelectDatesPeriodFrm.ReDraw();
begin
    t1.Checked := (DatesPeriod=sdkQuad);     UD1.Enabled  :=     t1.Checked; if t1.Checked   then f1.Color := clWindow  else f1.Color := clBtnFace;
    t2.Checked := (DatesPeriod=sdkMonth);    UD2.Enabled  :=     t2.Checked; if t2.Checked   then f2.Color := clWindow  else f2.Color := clBtnFace;
    t3.Checked := (DatesPeriod=sdkDay);      f3.Properties.ReadOnly := not t3.Checked;          if f3.Properties.ReadOnly  then f3.Style.Color := clBtnFace else f3.Style.Color := clWindow;
    t4.Checked := (DatesPeriod=sdkInterval); f41.Properties.ReadOnly:= not t4.Checked;          if f41.Properties.ReadOnly then f41.Style.Color:= clBtnFace else f41.Style.Color:= clWindow;
                                             f42.Properties.ReadOnly:= f41.Properties.ReadOnly; if f42.Properties.ReadOnly then f42.Style.Color:= clBtnFace else f42.Style.Color:= clWindow;
    t5.Checked := (DatesPeriod=sdkStartYear);
    t6.Checked := (DatesPeriod=sdkStartQuad);
    t7.Checked := (DatesPeriod=sdkStartMonth);
    t8.Checked := (DatesPeriod=sdkYear);     UD8.Enabled  :=     t8.Checked; if t8.Checked   then f8.Color := clWindow  else f8.Color := clBtnFace;
    t9.Checked := (DatesPeriod=sdkHalfYear); UD9.Enabled  :=     t9.Checked; if t9.Checked   then f9.Color := clWindow  else f9.Color := clBtnFace;
end;

procedure TSBaseSelectDatesPeriodFrm.tXClick(Sender: TObject);
begin
    case (Sender AS TWinControl).Tag of
      1: begin DateBegin := dT1; DateEnd := IncMonth(DateBegin,3); DatesPeriod := sdkQuad; if Visible then f1.SetFocus end;
      2: begin DateBegin := dT2; DateEnd := IncMonth(DateBegin); DatesPeriod := sdkMonth; if Visible then f2.SetFocus end;
      3: begin DateBegin := dT3; DateEnd := IncDay(dT3); DatesPeriod := sdkDay; if Visible then f3.SetFocus end;
      4: begin DateBegin := dT41;DateEnd := IncDay(dT42); DatesPeriod := sdkInterval; if Visible then f41.SetFocus end;
      5: begin DateBegin := dT5; DateEnd := IncYear(dT5); DatesPeriod := sdkStartYear end;
      6: begin DateBegin := dT6; DateEnd := IncMonth(dT6,3); DatesPeriod := sdkStartQuad end;
      7: begin DateBegin := dT7; DateEnd := IncMonth(dT7); DatesPeriod := sdkStartMonth end;
      8: begin DateBegin := dT8; DateEnd := IncYear(DateBegin); DatesPeriod := sdkYear; if Visible then f8.SetFocus end;
      9: begin DateBegin := dT9; DateEnd := IncMonth(DateBegin,6); DatesPeriod := sdkHalfYear; if Visible then f9.SetFocus end;
      else Exit;
    end;
    ReDraw();
    ShowCaption()
end;

procedure TSBaseSelectDatesPeriodFrm.t1Show();
var
  Year,Month,Day: Word;
begin
  DecodeDate(dT1,Year,Month,Day);
  f1.Text := IntToStr((Month-1) div 3+1) + ' Квартал ' + IntToStr(Year) + ' г.'
end;

procedure TSBaseSelectDatesPeriodFrm.UD1Click(Sender: TObject; Button: TUDBtnType);
begin
  if Button=btNext then dT1 := IncMonth(dT1,3) else dT1 := IncMonth(dT1,-3);
  DateBegin := dT1; DateEnd := IncMonth(dT1,3);
  t1Show();
  ShowCaption()
end;

procedure TSBaseSelectDatesPeriodFrm.t2Show();
var
  Year,Month,Day: Word;
begin
  DecodeDate(dT2,Year,Month,Day);
  f2.Text := Months[month] + ' ' + IntToStr(Year) + ' г.'
end;

procedure TSBaseSelectDatesPeriodFrm.UD2Click(Sender: TObject; Button: TUDBtnType);
begin
  if Button = btNext then
    dT2 := IncMonth(dT2, 1)
  else
    dT2 := IncMonth(dT2, -1);
  DateBegin := dT2;
  DateEnd := IncMonth(dT2);
  t2Show();
  ShowCaption()
end;

procedure TSBaseSelectDatesPeriodFrm.t8Show();
begin
  f8.Text := IntToStr(YearOf(dT8)) + ' г.'
end;

procedure TSBaseSelectDatesPeriodFrm.UD8Click(Sender: TObject; Button: TUDBtnType);
begin
  if Button=btNext then dT8 := IncYear(dT8,1) else dT8 := IncYear(dT8,-1);
  DateBegin := dT8; DateEnd := IncMonth(dT8);
  t8Show();
  ShowCaption()
end;

procedure TSBaseSelectDatesPeriodFrm.t9Show();
var
  Year,Month,Day: Word;
begin
  DecodeDate(dT9,Year,Month,Day);
  f9.Text := IntToStr((Month-1) div 6+1) + '-е ' + IntToStr(Year) + ' г.'
end;

procedure TSBaseSelectDatesPeriodFrm.UD9Click(Sender: TObject; Button: TUDBtnType);
begin
  if Button = btNext then dT9 := IncMonth(dT9,6) else dT9 := IncMonth(dT9,-6);
  DateBegin := dT9; DateEnd := IncMonth(dT9,6);
  t9Show();
  ShowCaption()
end;

procedure TSBaseSelectDatesPeriodFrm.ShowCaption();
begin
  if (DateBegin = 0) or (DateEnd = 0) or (DateBegin >= DateEnd) then begin
    tInfo.Font.Color := clRed;
    tInfo.Caption := 'Неверный диапазон дат';
    btnOk.Enabled := False
  end else begin
    tInfo.Font.Color := clWindowText;
    tInfo.Caption := 'Выбран период с ' + DateToStr(DateBegin) + ' по ' + DateToStr(IncDay(DateEnd,-1));
    btnOk.Enabled := True
  end
end;

procedure TSBaseSelectDatesPeriodFrm.f3Change(Sender: TObject);
begin
  if Visible then begin
    dT3 := IsNull((Sender AS TcxDateEdit).EditValue, 0);
    DateBegin := dT3; DateEnd := IncDay(dT3);
    ShowCaption()
  end
end;

procedure TSBaseSelectDatesPeriodFrm.f41Change(Sender: TObject);
begin
  if Visible then begin
    dT41 := IsNull((Sender AS TcxDateEdit).EditValue, 0);
    DateBegin := dT41; DateEnd := IncDay(dT42);
    ShowCaption()
  end
end;

procedure TSBaseSelectDatesPeriodFrm.f42Change(Sender: TObject);
begin
  if Visible then begin              
    dT42 := IsNull((Sender AS TcxDateEdit).EditValue, 0);
    DateBegin := dT41; DateEnd := IncDay(dT42);
    ShowCaption()
  end
end;

procedure TSBaseSelectDatesPeriodFrm.FormShow(Sender: TObject);
begin
  if t3.Checked then f3.SetFocus;

  ReDraw();
  ShowCaption()
end;

procedure TSBaseSelectDatesPeriodFrm.btnOkClick(Sender: TObject);
var
  LResultValues: TNamedVariants;
  LUnknownFields: String;
begin
  if not btnOk.Focused then
    btnOk.SetFocus;

  LResultValues := TNamedVariants.Create
  (
    SConst_DateBegin + ';' + SConst_DateEnd + ';' + SConst_DatesPeriod,
    VarArrayOf
    (
      [
        VarFromDateTime(RecodeTime(DateBegin, 0, 0, 0, 0)),
        VarFromDateTime(RecodeTime(IncDay(DateEnd, - 1), 0, 0, 0, 0)),
        DatesPeriod
      ]
    ),
    False
  );

  if (not ResultFields.IsEmpty) then
    ResultValues.WriteValues(ResultFields, LResultValues.ReadValues(ResultFields, LUnknownFields, True), True)
  else
    ResultValues := LResultValues;

  ModalResult := mrOk;
end;

{$IFDEF FASTSCRIPT_RTTI}
{ TFunctions_TSelectDate }

constructor TFunctions_TSelectDate.Create(AScript: TfsScript);
begin
  inherited;

  with AScript do begin
    AddEnum('TDatesPeriod', 'sdkDay, sdkInterval, sdkStartMonth, sdkStartQuad, sdkStartYear, sdkMonth, sdkQuad, sdkHalfYear, sdkYear');
  end;
end;
{$ENDIF}

procedure TSBaseSelectDatesPeriodFrm.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ActiveControl = f1 then
    UD1Click(f1, btNext)
  else
  if ActiveControl = f2 then
    UD2Click(f2, btNext);
  Handled := true;
end;

procedure TSBaseSelectDatesPeriodFrm.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if ActiveControl = f1 then
    UD1Click(f1, btPrev)
  else
  if ActiveControl = f2 then
    UD2Click(f2, btPrev);
  Handled := true;
end;

procedure TSBaseSelectDatesPeriodFrm.f1Click(Sender: TObject);
begin
  if not t1.Checked then tXClick(t1)
end;

procedure TSBaseSelectDatesPeriodFrm.f2Click(Sender: TObject);
begin
  if not t2.Checked then tXClick(t2)
end;

procedure TSBaseSelectDatesPeriodFrm.f3Click(Sender: TObject);
begin
  if not t3.Checked then tXClick(t3)
end;

procedure TSBaseSelectDatesPeriodFrm.f41Click(Sender: TObject);
begin
  if not T4.Checked then tXClick(t4)
end;

procedure TSBaseSelectDatesPeriodFrm.f42Click(Sender: TObject);
begin
  f41Click(Self);
  f42.SetFocus;
end;

initialization
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TFunctions_TSelectDate);
{$ENDIF}

  RegisterClass(TSBaseSelectDatesPeriodFrm);

finalization
  UnRegisterClass(TSBaseSelectDatesPeriodFrm);

end.
