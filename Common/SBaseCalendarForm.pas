unit SBaseCalendarForm;

interface

uses
  Messages, Classes, Controls, Types, SysUtils, Graphics, Forms;

type

{ TnltCustomMonthCalendar }

  EnltCalendarError = class(Exception);

  TnltCustomMonthCalendar = class(TCustomControl)
  private
    FCellHeight: Integer;
    FCellWidth: Integer;
    FTitleColor: TColor;
    FWeekTextColor: TColor;
    FSelectedColor: TColor;
    FHolydayTextColor: TColor;
    FBackgroundColor: TColor;
    FYear: Word;
    FMonth: Word;
    FShowToday: Boolean;
    FShowToDayFocus: Boolean;
    FShowPrevMonth: Boolean;
    FShowNextMonth: Boolean;
    FCurrentPos: Integer;
    FCurrentCount: Integer;
    FSelectedX: Integer;
    FSelectedY: Integer;
    FHolydays: LongWord;
    FOnSelectedChanged: TNotifyEvent;
    FOnNextMonthClick: TNotifyEvent;
    FOnPrevMonthClick: TNotifyEvent;
    FOnTodayClick: TNotifyEvent;
    procedure DrawTitle;
    procedure DrawWeek;
    procedure DrawToDay;
    procedure DrawContent;
    procedure DrawBackground;
    procedure DrawTodayCircle(Rect: TRect; Clear: Boolean);
    procedure DrawCells(Rect: TRect; X, Y: Integer; const Text: String; Selected: Boolean);
    function GetCellRect(Rect: TRect; X, Y: Integer; FullRect: Boolean): TRect;
    function GetCellX(X: LongInt): Integer;
    function GetCellY(Y: LongInt): Integer;
    procedure SetYear(const Value: Word);
    function GetHolydays(const Day: Byte): Boolean;
    procedure SetHolydays(const Day: Byte; const Value: Boolean);
    function GetSelected: Variant;
    procedure SetSelected(Value: Variant);
    procedure InternalSelected(X, Y: Integer); // TODO: Remove then multiselect
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
  protected
    function IsSelected(X, Y: Integer): Boolean;
    function GetTitleHeight: Integer;
    function GetWeekHeight: Integer;
    function GetTodayHeight: Integer;
    function GetContentRect: TRect;
    procedure Paint; override;
    procedure RefreshContent;
    procedure FontChanged(Sender: TObject);
    procedure DoPrevMonth;
    procedure DoNextMonth;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    property OnPrevMonthClick: TNotifyEvent read FOnPrevMonthClick write FOnPrevMonthClick;
    property OnNextMonthClick: TNotifyEvent read FOnNextMonthClick write FOnNextMonthClick;
    property OnTodayClick: TNotifyEvent read FOnTodayClick write FOnTodayClick;
    property OnSelectedChanged: TNotifyEvent read FOnSelectedChanged write FOnSelectedChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearSelected;
    procedure ClearHolydays(Holyday: Boolean = False);
    property Year: Word read FYear write SetYear;
    property Holydays[const Day: Byte]: Boolean read GetHolydays write SetHolydays;
    property Selected: Variant read GetSelected write SetSelected;
  end;

{ TnltYearCalendar }

  TnltMonths = Array [1..12] of TnltCustomMonthCalendar;
  TnltMonthData = Array [1..12] of DWord;
  // TODO: if datetime then TnltYearData = Array [1753..9999] of TnltMonthData;
  TnltYearData = Array [1900..2078] of TnltMonthData;

  TOnHolydaysChanged = procedure(Sender: TObject; Date: TDate; Holyday: Boolean) of object;
  TOnYearChanging = procedure(Sender: TObject; OldYear, NewYear: Word; var Allow: Boolean) of object;

  TnltYearCalendar = class(TScrollingWinControl)
  private
    FItems: TnltMonths;
    FMonthWidth: Integer;
    FMonthHeight: Integer;
    FYear: Word;
    FData: TnltYearData;
    FUpdating: Boolean;
    FUpdatingCount: Integer;
    FOnYearChanged: TNotifyEvent;
    FOnSelectedChanged: TNotifyEvent;
    FSelected: Variant;
    FOnYearChanging: TOnYearChanging;
    procedure SetYear(const Value: Word);
    function GetHolydays(const Date: TDate): Boolean;
    procedure SetHolydays(const Date: TDate; const Value: Boolean);
    procedure SetSelected(const Value: Variant);
  protected
    function CanChange(Value: Word): Boolean;
    procedure Resize; override;
    procedure DoClick(Sender: TObject);
    procedure DoTodayClick(Sender: TObject);
    procedure DoPrevMonthClick(Sender: TObject);
    procedure DoNextMonthClick(Sender: TObject);
    procedure DoSelectedChanged(Sender: TObject);
    property Items: TnltMonths read FItems;
    property MonthHeight: Integer read FMonthHeight;
    property MonthWidth: Integer read FMonthWidth;
    procedure WMMouseWheel(var Message: TWMMouseWheel);  message WM_MOUSEWHEEL;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ClearHolydays(Holyday: Boolean = False);
    property Holydays[const Date: TDate]: Boolean read GetHolydays write SetHolydays;
    property Year: Word read FYear write SetYear;
    property OnYearChanged: TNotifyEvent read FOnYearChanged write FOnYearChanged;
    property OnYearChanging: TOnYearChanging read FOnYearChanging write FOnYearChanging;
    property OnSelectedChanged: TNotifyEvent read FOnSelectedChanged write FOnSelectedChanged;
    property Selected: Variant read FSelected write SetSelected;
  published
    property OnClick;
    property PopupMenu;
  end;

implementation

uses
  Windows, DateUtils, Variants, SBaseVariantFunctions;

resourcestring
  SToday = 'Сегодня: ';
  EDayError = 'Неверный день месяца %d из %d';

var
  FLocaleFirstDayOfWeek: Byte;
  FLocaleFormatSettings: TFormatSettings;

function LocaleFirstDayOfWeek: Byte;
var
  fdw: Integer;
  c: array[0..1] of Char;
begin
  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_IFIRSTDAYOFWEEK, c, SizeOf(c));
  fdw := Ord(c[0]) - Ord('0');
  Result := (fdw + 2) mod 7;
end;

{ TnltCustomMonthCalendar }

procedure TnltCustomMonthCalendar.ClearHolydays(Holyday: Boolean);
begin
  if Holyday then
    FHolydays := High(LongWord)
  else
    FHolydays := 0;
end;

procedure TnltCustomMonthCalendar.ClearSelected;
var
  R: TRect;
  ADay, AToDay, AYear, AMonth: Word;
begin
  if (FSelectedX >= 0) and (FSelectedY >= 0) then
  begin
    Canvas.Brush.Color := FBackgroundColor;
    R := GetCellRect(GetContentRect, FSelectedX, FSelectedY, False);
    R.Left := R.Left - 7;
    R.Right := R.Right + 7;
    R.Bottom := R.Bottom + 1;
    Canvas.FillRect(R);
    ADay := FSelectedY * 7 + FSelectedX - FCurrentPos;
    if FHolydays and (1 shl (ADay - 1)) = 0 then
      Canvas.Font.Color := Font.Color
    else
      Canvas.Font.Color := FHolydayTextColor;
    DrawCells(GetContentRect, FSelectedX, FSelectedY, IntToStr(ADay), False);
    DecodeDate(Date, AYear, AMonth, AToDay);
    if FShowToDayFocus and (AYear = FYear) and (AMonth = FMonth) and (ADay = AToDay) then
      DrawTodayCircle(GetCellRect(GetContentRect, FSelectedX, FSelectedY, True), False);
    FSelectedX := -1;
    FSelectedY := -1;
  end;
end;

constructor TnltCustomMonthCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSelectedX := -1;
  FSelectedY := -1;
  FCellHeight := 15;
  FCellWidth := 15;
  Height := 154;
  Width := 191;
  FShowPrevMonth := True;
  FShowNextMonth := True;
  FMonth := MonthOf(Date);
  FYear := YearOf(Date);
  FCurrentCount := DaysInAMonth(FYear, FMonth);
  FShowToday := True;
  FShowToDayFocus := True;
  FSelectedColor := clActiveCaption;
  FTitleColor := clHighlight;
  FWeekTextColor := clActiveCaption;
  FBackgroundColor := clWindow;
  FHolydayTextColor := clRed;
end;

destructor TnltCustomMonthCalendar.Destroy;
begin
  inherited;
end;

procedure TnltCustomMonthCalendar.DoNextMonth;
begin
  if FShowNextMonth and Assigned(OnNextMonthClick) then OnNextMonthClick(Self);
end;

procedure TnltCustomMonthCalendar.DoPrevMonth;
begin
  if FShowPrevMonth and Assigned(OnPrevMonthClick) then OnPrevMonthClick(Self);
end;

procedure TnltCustomMonthCalendar.DrawBackground;
var
  R: TRect;
begin
  Canvas.Brush.Color := FBackgroundColor;
  R := GetContentRect;
  if not FShowToday then
    R.Bottom := R.Bottom + GetTodayHeight;
  Canvas.FillRect(R);
end;

procedure TnltCustomMonthCalendar.DrawCells(Rect: TRect; X, Y: Integer; const Text: String; Selected: Boolean);
var
  R: TRect;
  AColor: TColor;
begin
  if Selected then
    begin
      R := GetCellRect(Rect, x, y, True);;
      R.Left := R.Left + 3;
      R.Top := R.Top + 1;
      R.Right := R.Right - 4;
      R.Bottom := R.Bottom - 3;
      AColor := Canvas.Pen.Color;
      Canvas.Pen.Color := FSelectedColor;
      Canvas.Brush.Color := FSelectedColor;
      Canvas.Ellipse(R);
      Canvas.Pen.Color := AColor;
      Canvas.Brush.Style := bsClear;
      R := GetCellRect(Rect, x, y, False);
      AColor := Canvas.Font.Color;
      if Canvas.Font.Color <> FHolydayTextColor then Canvas.Font.Color := FBackgroundColor;
      DrawText(Canvas.Handle, PChar(Text), Length(Text), R, DT_SINGLELINE + DT_CENTER	+ DT_VCENTER);
      Canvas.Font.Color := AColor;
      Canvas.Brush.Style := bsSolid;
    end
  else
    begin
      R := GetCellRect(Rect, x, y, False);
      if y < 0 then
      begin
        R.Left := R.Left - 5;
        R.Right := R.Right + 5;
      end;
      DrawText(Canvas.Handle, PChar(Text), Length(Text), R, DT_SINGLELINE + DT_CENTER	+ DT_VCENTER);
    end;
end;

procedure TnltCustomMonthCalendar.DrawContent;
var
  R: TRect;
  i, AToDay, APos, AMonth, AYear, ACount: Word;
  ADate, APrevDate: TDate;
begin
  Canvas.Font.Assign(Font);
  DecodeDate(Date, AYear, AMonth, AToDay);
  ADate := EncodeDate(FYear, FMonth, 1);
  APos := DayOfWeek(EncodeDate(FYear, FMonth, 1));
  R := GetContentRect;
  if FLocaleFirstDayOfWeek < APos then
    APos := APos - FLocaleFirstDayOfWeek
  else
    APos := 7 - FLocaleFirstDayOfWeek + APos;
  if (APos > 1) and FShowPrevMonth then
  begin
    Canvas.Font.Color := clGradientInactiveCaption;
    APrevDate := IncMonth(ADate, -1);
    ACount := DaysInAMonth(YearOf(APrevDate), MonthOf(APrevDate));
    i := ACount - APos + 1; APos := 0;
    for i := i to ACount do
    begin
      DrawCells(R, APos mod 7 + 1, APos div 7, IntToStr(i), False);
      Inc(APos);
    end;
  end;
  FCurrentPos := APos;
  FCurrentCount := DaysInAMonth(FYear, FMonth);
  for i := 1 to FCurrentCount do
  begin
    if FHolydays and (1 shl (i - 1)) = 0 then
      Canvas.Font.Color := Font.Color
    else
      Canvas.Font.Color := FHolydayTextColor;
    DrawCells(R, APos mod 7 + 1, APos div 7, IntToStr(i), isSelected(APos mod 7 + 1, APos div 7));
    if FShowToDayFocus and (AYear = FYear) and (AMonth = FMonth) and (AToDay = i) then
      DrawTodayCircle(GetCellRect(R, APos mod 7 + 1, APos div 7, True), False);
    Inc(APos);
  end;
  if FShowNextMonth then
  begin
    Canvas.Font.Color := clGradientInactiveCaption;
    for i := 1 to 42 - APos do
    begin
      DrawCells(R, APos mod 7 + 1, APos div 7, IntToStr(i), False);
      Inc(APos);
    end;
  end;
end;

procedure TnltCustomMonthCalendar.DrawTitle;
var
  R: TRect;
  AStyle: set of TFontStyle;
  AMonthName: String;
begin
  R := ClientRect;
  R.Bottom := GetTitleHeight;
  AStyle := Canvas.Font.Style;
  Canvas.Font.Assign(Font);
  Canvas.Brush.Color := FTitleColor;
  Canvas.FillRect(R);
  Canvas.Font.Style := [fsBold];
  Canvas.Font.Color := clWhite;
  AMonthName := FormatSettings.LongMonthNames[FMonth];
  DrawText(Canvas.Handle, PChar(AMonthName), Length(AMonthName), R, DT_SINGLELINE + DT_CENTER	+ DT_VCENTER);
end;

procedure TnltCustomMonthCalendar.DrawToDay;
var
  R: TRect;
  ARight: Integer;
  AToDayText: String;
begin
  if FShowToday then
  begin
    R := ClientRect;
    R.Top := R.Bottom - GetTodayHeight;
    ARight := R.Right;
    R.Right := R.Left + ClientWidth div 7 + 3;
    DrawTodayCircle(R, True);
    R.Left := R.Right;
    R.Right := ARight;
    Canvas.Font.Assign(Font);
    Canvas.Brush.Color := FBackgroundColor;
    Canvas.FillRect(R);
    AToDayText := SToday + DateTimeToStr(Date, FLocaleFormatSettings);
    Canvas.Font.Style := [fsBold];
    DrawText(Canvas.Handle, PChar(AToDayText), Length(AToDayText), R, DT_SINGLELINE + DT_VCENTER);
  end;
end;

function Min(x,y: Integer): Integer;
begin
  if x > y then Result := x else Result := y;
end;

procedure TnltCustomMonthCalendar.DrawTodayCircle(Rect: TRect; Clear: Boolean);
var
  R: TRect;
  AWidth, AHeight, AIndent: Integer;
begin
  Canvas.Brush.Color := FBackgroundColor;
  Canvas.Pen.Width := 2;
  Canvas.Pen.Color := clRed;
  AWidth := Rect.Right - Rect.Left;
  AHeight := Rect.Bottom - Rect.Top;
  AIndent := Min(AWidth, AHeight) div 10;
  if AIndent < 3 then AIndent := 3;
  if Clear then Canvas.FillRect(Rect);
  R := Rect;
  R.Top := R.Top + AIndent;
  R.Left := R.Left + AIndent;
  R.Right := R.Right - AIndent;
  R.Bottom := R.Bottom - AIndent;
  AWidth := R.Right - R.Left;
  AHeight := R.Bottom - R.Top;
  Canvas.Arc(R.Left, R.Top, R.Right, R.Bottom, R.Left + AWidth div 2, R.Top, R.Right, R.Top + AHeight div 2);
  R.Top := R.Top - AIndent;
  R.Bottom := R.Bottom + AIndent;
  AHeight := R.Bottom - R.Top;
  Canvas.Arc(R.Left, R.Top, R.Right, R.Bottom, R.Right, R.Top + AHeight div 2, R.Left + AWidth div 2, R.Top);
  Canvas.MoveTo(R.Left + AWidth div 2, R.Top);
  Canvas.LineTo(R.Left, R.Top + AIndent div 2);
  Canvas.Pen.Width := 1;
end;

procedure TnltCustomMonthCalendar.DrawWeek;
var
  R: TRect;
  i: Integer;
begin
  R := ClientRect;
  R.Top := GetTitleHeight;
  R.Bottom := R.Top + GetWeekHeight;
  Canvas.Brush.Color := FBackgroundColor;
  Canvas.FillRect(R);
  Canvas.Pen.Color := Font.Color;
  Canvas.MoveTo(3, R.Bottom - 1);
  Canvas.LineTo(Width - 3, R.Bottom - 1);
  Canvas.Font.Assign(Font);
  Canvas.Font.Color := FWeekTextColor;
  for i := 1 to 7 do
    DrawCells(R, i, -1, FormatSettings.ShortDayNames[(i + FLocaleFirstDayOfWeek - 2) mod 7 + 1], False);
end;

procedure TnltCustomMonthCalendar.FontChanged(Sender: TObject);
begin
  Invalidate;
end;

function TnltCustomMonthCalendar.GetCellRect(Rect: TRect; x, y: Integer; FullRect: Boolean): TRect;
var
  AHeight, AWidth: Integer;
begin
  Result := Rect;
  AWidth := Result.Right - Result.Left;
  if FullRect then
    begin
      Result.Left := Result.Left + AWidth div 7 * (x - 1);
      Result.Right := Result.Left + FCellWidth + AWidth div 10 - 2;
    end
  else
    begin
      Result.Left := Result.Left + AWidth div 7 * (x - 1) + AWidth div 20;
      Result.Right := Result.Left + FCellWidth - 2;
    end;
  if y >= 0 then
  begin
    AHeight := (Result.Bottom - Result.Top) div 7;
    Result.Top := Result.Top + (AHeight + 2) * y + 1;
    Result.Bottom := Result.Top + AHeight;
  end;
  if FullRect then
  begin
    Result.Top := Result.Top - 1;
    Result.Bottom := Result.Bottom + 4;
  end;
end;

function TnltCustomMonthCalendar.GetCellX(X: LongInt): Integer;
var
  R: TRect;
  AWidth: Integer;
begin
  R := GetContentRect;
  AWidth := R.Right - R.Left;
  Result := (X - R.Left - AWidth div 40) div (AWidth div 7) + 1;
end;

function TnltCustomMonthCalendar.GetCellY(Y: LongInt): Integer;
var
  R: TRect;
  AHeight: Integer;
begin
  R := GetContentRect;
  AHeight := R.Bottom - R.Top;
  Result := (Y - R.Top) div (AHeight div 7 + 2);
  if Result > 5 then Result := 5;
end;

function TnltCustomMonthCalendar.GetContentRect: TRect;
begin
  Result := ClientRect;
  Result.Top := GetTitleHeight + GetWeekHeight;
  Result.Bottom := Result.Bottom - GetTodayHeight - ClientWidth div 20;;
end;

function TnltCustomMonthCalendar.GetHolydays(const Day: Byte): Boolean;
begin
  if (Day > 0) and (Day <= FCurrentCount) then
    Result := FHolydays and (1 shl (Day - 1)) <> 0
  else
    raise EnltCalendarError.CreateFmt(EDayError, [Day, FCurrentCount]);
end;

function TnltCustomMonthCalendar.GetSelected: Variant;
begin
  if (FSelectedX < 1) or (FSelectedY < 0) then
    Result := Null
  else
    Result := FSelectedY * 7 + FSelectedX - FCurrentPos;
end;

function TnltCustomMonthCalendar.GetTitleHeight: Integer;
begin
  Result := 2 * (Height div 10);
end;

function TnltCustomMonthCalendar.GetTodayHeight: Integer;
begin
  Result := Height div 10;
end;

function TnltCustomMonthCalendar.GetWeekHeight: Integer;
begin
  Result := Height div 10;
end;

function TnltCustomMonthCalendar.IsSelected(X, Y: Integer): Boolean;
begin
  Result := (X = FSelectedX) and (Y = FSelectedY)
end;

procedure TnltCustomMonthCalendar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
  APosX, APosY: Integer;
begin
  inherited;
  R := GetContentRect;
  if (Y > R.Top) and (Y < R.Bottom) and (X > R.Left) and (X < R.Right) then
  begin
    APosX := GetCellX(X);
    APosY := GetCellY(Y);
    if (APosY = 0) and (APosX <= FCurrentPos) then
      DoPrevMonth
    else
    if (APosX + APosY * 7 > FCurrentCount + FCurrentPos) then
      DoNextMonth
    else
      InternalSelected(APosX, APosY);
  end
  else
  if (Y > R.Bottom) and FShowToday and Assigned(OnTodayClick)then
    OnTodayClick(Self);
end;

procedure TnltCustomMonthCalendar.Paint;
begin
  DrawTitle;
  DrawWeek;
  DrawBackground;
  DrawToDay;
  DrawContent;
  inherited;
end;

procedure TnltCustomMonthCalendar.RefreshContent;
begin
  if HandleAllocated then
  begin
    DrawBackground;
    DrawContent;
  end;
end;

procedure TnltCustomMonthCalendar.SetHolydays(const Day: Byte; const Value: Boolean);
begin
  if (Day > 0) and (Day <= FCurrentCount) then
    if Value then
      FHolydays := FHolydays or (1 shl (Day - 1))
    else
      FHolydays := FHolydays and not (1 shl (Day - 1))
  else
    raise EnltCalendarError.CreateFmt(EDayError, [Day, FCurrentCount]);
end;

procedure TnltCustomMonthCalendar.InternalSelected(X, Y: Integer);
begin
  if not IsSelected(X, Y) then
  begin
    ClearSelected;
    FSelectedX := X;
    FSelectedY := Y;
    DrawContent;
    if Assigned(OnSelectedChanged) then OnSelectedChanged(Self);
  end;
end;

procedure TnltCustomMonthCalendar.SetSelected(Value: Variant);
var
  APos: Word;
begin
  if VarIsNull(Value) then
    ClearSelected
  else
    begin
      APos := DayOfWeek(EncodeDate(FYear, FMonth, 1));
      if FLocaleFirstDayOfWeek < APos then
        APos := APos - FLocaleFirstDayOfWeek
      else
        APos := 7 - FLocaleFirstDayOfWeek + APos;
      APos := APos + Value;
      InternalSelected(APos mod 7, APos div 7);
    end;
end;

procedure TnltCustomMonthCalendar.SetYear(const Value: Word);
begin
  if FYear <> Value then
  begin
    FYear := Value;
    ClearHolydays;
    ClearSelected;
    RefreshContent;
  end;
end;

procedure TnltCustomMonthCalendar.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
end;

procedure TnltCustomMonthCalendar.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  self.SetFocus;
end;

{ TnltYearCalendar }

procedure TnltYearCalendar.BeginUpdate;
begin
  Inc(FUpdatingCount); 
end;

function TnltYearCalendar.CanChange(Value: Word): Boolean;
begin
  Result := True;
  if Assigned(FOnYearChanging) then OnYearChanging(Self, FYear, Value, Result);
end;

procedure TnltYearCalendar.ClearHolydays(Holyday: Boolean);
var
  i, j: Integer;
begin
  for i := 0 to 12 do Items[i].ClearHolydays(Holyday);
  for i := Low(FData) to High(FData) do
   for j := 1 to 12 do
     if Holyday then
       FData[i, j] := High(LongWord)
     else
       FData[i, j] := 0;
end;

constructor TnltYearCalendar.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;
  FUpdating := False;
  FUpdatingCount := 0;
  FSelected := Null;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csDoubleClicks];
  Color := clNone;
  Color := clWindow;
  for i := 1 to 12 do
  begin
    FItems[i] := TnltCustomMonthCalendar.Create(Self);
    FItems[i].FMonth := i;
    FItems[i].FShowPrevMonth := i = 1;
    FItems[i].FShowNextMonth := i = 12;
    FItems[i].FShowToday := i = 12;
    FItems[i].Parent := Self;
    FItems[i].OnSelectedChanged := DoSelectedChanged;
    FItems[i].OnPrevMonthClick := DoPrevMonthClick;
    FItems[i].OnNextMonthClick := DoNextMonthClick;
    FItems[i].OnTodayClick := DoTodayClick;
    FItems[i].OnClick := DoClick;
  end;
  FMonthWidth := Items[1].Width;
  FMonthHeight := Items[1].Height;
  Width := (FMonthWidth) * 4 + 15;
  Height := (FMonthHeight) * 3 + 10;
  Year := YearOf(Date);
end;

destructor TnltYearCalendar.Destroy;
begin
  inherited;
end;

procedure TnltYearCalendar.DoClick(Sender: TObject);
begin
  if Assigned(OnClick) then OnClick(Self);
  SetFocus; //  TODO: ????
end;

procedure TnltYearCalendar.DoNextMonthClick(Sender: TObject);
begin
  Year := Year + 1;
end;

procedure TnltYearCalendar.DoPrevMonthClick(Sender: TObject);
begin
  Year := Year - 1;
end;

procedure TnltYearCalendar.DoSelectedChanged(Sender: TObject);
var
  i: Integer;
begin
  if FUpdating then exit;
  FUpdating := True;
  try
    with TnltCustomMonthCalendar(Sender) do
    begin
      if VarIsNull(Selected) then
        FSelected := Null
      else
        FSelected := EncodeDate(FYear, FMonth, Selected);
    end;
    for i := 1 to 12 do
    if Items[i] <> Sender then
      Items[i].ClearSelected;
  finally
    FUpdating := False;
  end;
  if Assigned(FOnSelectedChanged) then OnSelectedChanged(Self);
end;

procedure TnltYearCalendar.DoTodayClick(Sender: TObject);
begin
  Selected := Date;
  ScrollInView(Items[MonthOf(Date)]);
end;

procedure TnltYearCalendar.EndUpdate;
var
  i: Integer;
begin
  Dec(FUpdatingCount);
  for i := 1 to 12 do Items[i].RefreshContent;
end;

function TnltYearCalendar.GetHolydays(const Date: TDate): Boolean;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(Date, AYear, AMonth, ADay);
  Result := FData[AYear, AMonth] and (1 shl (ADay - 1)) <> 0;
end;

procedure TnltYearCalendar.Resize;
var
  i, ACount: Integer;
begin
  ACount := ClientWidth div MonthWidth;
  if ACount = 0 then ACount := 1;
  for i := 0 to 11 do
  begin
    Items[i + 1].Left := (i mod ACount) * (MonthWidth + 5) - HorzScrollBar.Position;
    Items[i + 1].Top := (i div ACount) * (MonthHeight + 5) - VertScrollBar.Position;
  end;
  inherited;
end;

procedure TnltYearCalendar.SetHolydays(const Date: TDate; const Value: Boolean);
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(Date, AYear, AMonth, ADay);
  if Value then
    FData[AYear, AMonth] := FData[AYear, AMonth] or (1 shl (ADay - 1))
  else
    FData[AYear, AMonth] := FData[AYear, AMonth] and not (1 shl (ADay - 1));
  if AYear = FYear then
  begin
    Items[AMonth].FHolydays := FData[AYear, AMonth];
    if FUpdatingCount = 0 then Items[AMonth].RefreshContent;
  end;
end;

procedure TnltYearCalendar.SetSelected(const Value: Variant);
begin
  FSelected := Value;
  Year := YearOf(FSelected);
  Items[MonthOf(FSelected)].Selected := DayOf(FSelected);
end;

procedure TnltYearCalendar.SetYear(const Value: Word);
var
  i: Integer;
begin
  if (FYear <> Value) and CanChange(Value) then
  begin
    FYear := Value;
    for i := 1 to 12 do
    begin
      Items[i].FYear := FYear;
      Items[i].FHolydays := FData[FYear, i];
      Items[i].FSelectedX := -1;
      Items[i].FSelectedY := -1;
      Items[i].RefreshContent;
    end;
    if Assigned(FOnYearChanged) then OnYearChanged(Self);
  end;
end;

procedure TnltYearCalendar.WMMouseWheel(var Message: TWMMouseWheel);
begin
  inherited;
  if VertScrollBar.Visible then
    if Message.WheelDelta < 0 then
      Perform(WM_VScroll, SB_LINEDOWN, 0)
    else
      Perform(WM_VScroll, SB_LINEUP, 0)
  else
  if HorzScrollBar.Visible then
    if Message.WheelDelta < 0 then
      Perform(WM_HScroll, SB_LINEDOWN, 0)
    else
      Perform(WM_HScroll, SB_LINEUP, 0);
end;

initialization

  FLocaleFirstDayOfWeek := LocaleFirstDayOfWeek;
//  GetLocaleFormatSettings(GetThreadLocale, );
  FLocaleFormatSettings := TFormatSettings.Create(GetThreadLocale);

end.
