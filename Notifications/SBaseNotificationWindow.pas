(*
      Негаш А.
      Разные нестандартные окна:
      - TSBaseNotifyWindow - простая форма - уведомление
      - TSBaseNotifyMessageWindow, TSBaseDBNotifyMessageWindow - окно содержит
      коолекцию сообщений и может их пролистывать вперед-назад
*)
unit SBaseNotificationWindow;
{$I config.inc}

interface

uses
  Classes, Messages, Windows, Controls, StdCtrls, ExtCtrls,
  Graphics, DB, SBaseForm, Types, ImgList
  {$IFDEF DELPHIXE3}
  , System.UITypes
  {$ENDIF};

type
  TSBaseNotifyWindow = class;
  TSBaseNotifyMessageWindow = class;

  /// Состояния кнопки
  TDrawButtonState  = (dbsNone, dbsHover, dbsClick, dbsDisable);

  /// Тип рисунка на кнопке
  TDrawType         = (dtClose, dtLeft, dtRight, dtImage);

  /// Способ выравнивания кнопки
  TAlignType        = (atTopLeft, atTopRight, atBottomRight, atBottomLeft);

  /// Как окно проявляется
  TWindowFadeType   = (ftHide, ftShow);

  /// Хранит положение кнопки на форме
  TButtonPosition = class(TPersistent)
  strict private
    FWidth:       Integer;
    FHeight:      Integer;
    FDistanceX:   Integer;
    FDistanceY:   Integer;
    FAlignType:   TAlignType;

    FOnChange:    TNotifyEvent;

    procedure SetAlignType(const AValue: TAlignType);
    procedure SetDistanceX(const AValue: Integer);
    procedure SetDistanceY(const AValue: Integer);
    procedure SetHeight(const AValue: Integer);
    procedure SetWidth(const AValue: Integer);

    procedure Change;
  public
    function GetRect(AOwner: TControl): TRect;
  published
    property Width: Integer         read FWidth       write SetWidth;
    property Height: Integer        read FHeight      write SetHeight;
    property DistanceX: Integer     read FDistanceX   write SetDistanceX;
    property DistanceY: Integer     read FDistanceY   write SetDistanceY;
    property AlignType: TAlignType  read FAlignType   write SetAlignType;
    property OnChange: TNotifyEvent read FOnChange    write FOnChange;
  end;


  /// Кнопка, не имеющая собственного окна(Window Handler)
  TCustomDrawButton = class(TComponent)
  strict private
    FRect:            TRect;
    FHoverColor:      TColor;
    FClickColor:      TColor;

    FState:           TDrawButtonState;
    FDrawType:        TDrawType;
    FEnable:          Boolean;
    FVisible:         Boolean;

    FOnClick:         TNotifyEvent;

    FPosition:        TButtonPosition;

    FHint:            String;

    FImage:           TBitmap;
    FImageIndex:      Integer;
    FImageList:       TCustomImageList;

    procedure UpdateView;

    procedure SetEnable(const AValue: Boolean);
    procedure SetState(const AValue: TDrawButtonState);
    procedure SetVisible(const AValue: Boolean);
    procedure SetImage(const AValue: TBitmap);
    procedure SetDrawType(const AValue: TDrawType);

    procedure DoImageChage(ASender: TObject);
    procedure ChangeImageIndex;

    procedure SetImageIndex(const AValue: Integer);
    procedure SetImageList(const AValue: TCustomImageList);
  private
    procedure Draw(ACanvas: TCanvas);
    procedure DoPositionChange(ASender: TObject);

  public
    constructor Create(AParent: TComponent); override;
    destructor Destroy; override;

    property ClientRect:  TRect             read FRect;
    property ClickColor:  TColor            read FClickColor  write FClickColor;
    property HoverColor:  TColor            read FHoverColor  write FHoverColor;
    property DrawType:    TDrawType         read FDrawType    write SetDrawType;

    property Enable:      Boolean           read FEnable      write SetEnable;
    property State:       TDrawButtonState  read FState       write SetState;
    property Visible:     Boolean           read FVisible     write SetVisible;

    property Image:       TBitmap           read FImage       write SetImage;

    property OnClick:     TNotifyEvent      read FOnClick     write FOnClick;
    property Position:    TButtonPosition   read FPosition    write FPosition;

    property Hint:        String            read FHint        write FHint;

    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property ImageList: TCustomImageList read FImageList write SetImageList;
  end;


  TBaseDrawButton = class(TCustomDrawButton)
  published
    property ClickColor;
    property DrawType;
    property Enable;
    property Hint;
    property HoverColor;
    property Position;
    property Visible;

    property Image;
    property ImageIndex;
    property ImageList;

    property OnClick;
  end;


  /// Элемент окна сообщений (т.е одно сообщение)
  TSBaseNotifyMessageItem = class(TCollectionItem)
  strict private
    FCaption: string;
    FText: string;
    FId: Integer;
  published
    property Caption: string read FCaption write FCaption;
    property Text: string read FText write FText;
    property Id: Integer read FId write FId;
  end;

  /// Коллекция оконных сообщений
  TBaseNotifyMessageCollection = class(TCollection)
  strict private
    FWindow: TSBaseNotifyMessageWindow;
  public
    constructor Create(AMessageWindow: TSBaseNotifyMessageWindow); reintroduce;
  end;

  /// Простое окно уведомлений
  TSBaseNotifyWindow = class(TSBaseFrm)
  strict private
    FPictureName:   string;

    /// для плавного скрытия - раскрытия
    FFadeThread:    TThread;
    FFadeType:      TWindowFadeType;
    FFadeValue:     Byte;

    /// для перетаскивания окна
    FIsDrag:        Boolean;
    FDragOldX:      Integer;
    FDragOldY:      Integer;

    /// цвета градиента
    FBeginColor:    TColor;
    FEndColor:      TColor;

    FBorderColor:   TColor;

    FCurrentButton: TBaseDrawButton;

    FButtonHintTimer: TTimer;
    FHintWindow:      THintWindow;

    procedure WMNCHitTest(var AMessage: TWMNCHitTest);      message WM_NCHITTEST;

    procedure WMNCMouseLeave(var AMessage: TMessage);       message WM_NCMOUSELEAVE;
    procedure CMMouseLeave(var AMessage: TMessage);         message CM_MOUSELEAVE;

    procedure FadeWindow(AType: TWindowFadeType);
    procedure DoFadeTerminate(ASender: TObject);
    procedure TerminateThread;

    procedure DoButtonHint(ASender: TObject);
    procedure ClearHint;
  strict protected
    function GetButtonAt(APoint: TPoint): TBaseDrawButton;

    function IsDragArea(APoint: TPoint): Boolean; virtual;

    procedure InternalRefresh; virtual;

    procedure RefreshButton(AButton: TBaseDrawButton); virtual;

    procedure DoMouseLeave; virtual;
  protected
    procedure Paint; override;
    procedure CreateParams(var Params: TCreateParams); override;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    procedure ShowAt(AX, AY: Integer); virtual;
    procedure InternalCreate; override;

    destructor Destroy; override;
  published
    property PictureName: string read FPictureName write FPictureName;
    property BeginColor: TColor read FBeginColor write FBeginColor;
    property EndColor: TColor read FEndColor write FEndColor;
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property FadeValue: Byte read FFadeValue write FFadeValue   default 150;
  end;


  /// Окно уведомления с несколькими сообщениями
  TSBaseNotifyMessageWindow = class(TSBaseNotifyWindow)
  strict private
    FMessages: TBaseNotifyMessageCollection;

    FItemIndex: Integer;
    FUpdating: Boolean;
    FOnCloseMessage: TNotifyEvent;

    procedure DisplayMessage(AIndex: Integer);

    function GetCount: Integer;
    function GetCurrentItem: TSBaseNotifyMessageItem;
    procedure SetItemIndex(const AValue: Integer); protected

    function FindById(AId: Integer): Integer;

  strict protected
    property UpdatingItems: Boolean read FUpdating write FUpdating;
    procedure Clear;

    function IsDragArea(APoint: TPoint): Boolean; override;
    procedure InternalRefresh; override;
    procedure RefreshButton(AButton: TBaseDrawButton); override;

    procedure RefreshData; virtual;
  protected
    procedure Paint; override;
    procedure Resize; override;
  public
    function Add(const ACaption, AText: string; const AId: Integer): TSBaseNotifyMessageItem;
    function DeleteById(AId: Integer): Boolean;

    procedure InternalCreate; override;
    procedure Show; override;

    destructor Destroy; override;
  published
    FCaptionLabel:    TLabel;
    FNumeratorLabel:  TLabel;
    FTextLabel:       TLabel;

    FCloseButton:     TBaseDrawButton;
    FNextButton:      TBaseDrawButton;
    FPrevButton:      TBaseDrawButton;

    procedure DoCloseClick(ASender: TObject);
    procedure DoNextClick(ASender: TObject);
    procedure DoPrevClick(ASender: TObject);

    property ItemIndex: Integer read FItemindex write SetItemIndex;
    property CurrentItem: TSBaseNotifyMessageItem read GetCurrentItem;
    property Count: Integer read GetCount;
    property Items: TBaseNotifyMessageCollection read FMessages;

    property OnCloseMessage: TNotifyEvent read FOnCloseMessage write FOnCloseMessage;
  end;


  /// окно с возможностью загрузки сообщений из DataSet-а
  TSBaseDBNotifyMessageWindow = class(TSBaseNotifyMessageWindow)
  strict private
    FDataSource: TDataSource;
    FCaptionFieldName: string;
    FTextFieldName: string;
    FIdFieldName: string;

    procedure SetIdFieldName(AValue: string);
    procedure SetCaptionFieldName(AValue: string);
    procedure SetDataSource(const AValue: TDataSource);
    procedure SetTextFieldName(AValue: string);

    procedure DoDataSetChande(ASender: TObject);

    procedure LoadFromDataSet;

    function CanLoadFromDataSet: Boolean;
  strict protected
    procedure RefreshData; override;
  public
    procedure ReOpen;
  published
    property MessagesDataSource: TDataSource read FDataSource write SetDataSource;
    property CaptionFieldName: string read FCaptionFieldName write SetCaptionFieldName;
    property TextFieldName: string read FTextFieldName write SetTextFieldName;
    property IdFieldName: string read FIdFieldName write SetIdFieldName;
  end;

implementation

uses
  Forms, SysUtils, Math, fs_ievents, fs_iinterpreter, SBaseItemReactions;

/// Файлы настройки внешенего вида компонентов
{$RESOURCE Notifications\SBaseNotifyWindow.dfm}
{$RESOURCE Notifications\SBaseNotifyMessageWindow.dfm}

const
  C_NumericLabelWidth   = 60;
  C_HintInterval        = 700;

type
  Tfs_TSBaseNotifyMessageWindow = class(TfsRTTIModule)
  strict private
    function Call_TSBaseNotifyMessageWindowMethod(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Call_TSBaseDBNotifyMessageWindowMethod(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

  // Поток для плавного скрытия-проявления окна
  TFadeWindowThread = class(TThread)
  strict private
    FWindow: TSBaseNotifyWindow;
    FType: TWindowFadeType;
  {$IFDEF DEBUG}
  private
    FDebugCurrentCounter: Integer;
    class var
      FDebugCounter: Integer;
  {$ENDIF}
  protected
    procedure Execute; override;
  public
    constructor Create(AWindow: TSBaseNotifyWindow; AType: TWindowFadeType); overload;
  end;

procedure PrintDebugString(const AText: string);
begin
  Windows.OutputDebugString(PWideChar(AText));
end;

{$REGION 'Рисовальные процедуры'}

/// <summary>
/// Создаем черно-белое изображение
/// </summary>
/// <param name="ABitmap">Изображение</param>
procedure BitmapToGrayscale(ABitmap: TBitmap; AApply: Boolean);
  procedure Grayscale(ABuffer: PInteger; ACount: Integer);
  const
    LAlpha = 120;
  var
    LColor: Integer;
  begin;
    repeat;
      LColor:=ABuffer^;
      if AApply then
        ABuffer^ := ((LColor * (LAlpha * 256) + (LColor and $00FF00FF) * (29 * 256 * 256 - LAlpha * 256 + 77)) shr 24) * $00010101
      else
        ABuffer^ := LColor and $00FFFFFF;

      Dec(ACount);
      Inc(ABuffer);
    until ACount <= 0;
  end;

var
  LLine, LFirstLine: PInteger;
begin;
  if ABitmap.Height = 0 then
    Exit;

  with ABitmap do begin;
    LLine := ScanLine[Height - 1];

    LFirstLine := ScanLine[0];
    if Cardinal(LLine) > Cardinal(LFirstLine) then
      LLine:=LFirstLine;

    Grayscale(LLine, Height * Width);
  end;
end;

/// <summary>
/// Процедура ограничивает положение формы в пределх родительского окна
/// </summary>
/// <param name="AHandle">Родительское окно</param>
/// <param name="ABounds">Положение формы в экранных координатах</param>
procedure AdjustRectToWindow(const AHandle: HWND; var ABounds: TRect);
var
  LRect: TRect;
  LPoint: TPoint;
begin
  Windows.GetClientRect(AHandle, LRect);

  LPoint := Point(0, 0);
  Windows.ClientToScreen(AHandle, LPoint);  // Экранные координаты верхнего левого угла
  OffsetRect(LRect, LPoint.X, LPoint.Y);    // Окно в экранных координатах

  if ABounds.Left < LRect.Left then
     OffsetRect(ABounds, LRect.Left - ABounds.Left, 0);
  if ABounds.Top < LRect.Top then
    OffsetRect(ABounds, 0, LRect.Top - ABounds.Top);
  if ABounds.Right > LRect.right then
    OffsetRect(ABounds, LRect.Right - ABounds.Right, 0);
  if ABounds.Bottom > LRect.Bottom then
    OffsetRect(ABounds, 0, LRect.Bottom - ABounds.Bottom);
end;


/// <summary>
/// Рисует линию, сдвигая все координаты на значение APoint
/// </summary>
/// <param name="APoint">Сдвиг координат</param>
procedure _Line(AX1, AY1, AX2, AY2: Integer; APoint: TPoint; ACanvas: TCanvas);
begin
  ACanvas.MoveTo(AX1 + APoint.X, AY1 + APoint.Y);
  ACanvas.LineTo(AX2 + APoint.X, AY2 + APoint.Y);
end;


/// <summary>
/// Градиентная заливка
/// </summary>
/// <param name="AFromColor">Начальный цвет</param>
/// <param name="AToColor">Конечный цвет</param>
/// <param name="ASteps">Количество шагов - чем больше - тем плавнее</param>
/// <param name="ARect">Область заливки</param>
/// <param name="ADirection">Тип - True = вертикальный, False = горизонтальный</param>
procedure DrawGradient(ACanvas: TCanvas; AFromColor, AToColor: TColor; ASteps: Integer; ARect: TRect; ADirection: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;
begin
  if ASteps = 0 then
    ASteps := 1;

  AFromColor := ColorToRGB(AFromColor);
  AToColor := ColorToRGB(AToColor);

  startr := (AFromColor and $0000FF);
  startg := (AFromColor and $00FF00) shr 8;
  startb := (AFromColor and $FF0000) shr 16;
  endr := (AToColor and $0000FF);
  endg := (AToColor and $00FF00) shr 8;
  endb := (AToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / ASteps;
  rstepg := diffg / ASteps;
  rstepb := diffb / ASteps;

  if ADirection then
    rstepw := (ARect.Right - ARect.Left) / ASteps
  else
    rstepw := (ARect.Bottom - ARect.Top) / ASteps;

  with ACanvas do begin
    for i := 0 to ASteps - 1 do begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if ADirection then
        Rectangle(ARect.Left + stepw, ARect.Top, ARect.Left + stepw + Round(rstepw) + 1, ARect.Bottom)
      else
        Rectangle(ARect.Left, ARect.Top + stepw, ARect.Right, ARect.Top + stepw + Round(rstepw) + 1);
    end;
  end;
end;


/// <summary>
/// Рисует узор в области перетаскивания
/// </summary>
/// <param name="AForm">Форма</param>
procedure DrawTopBar(AForm: TSBaseFrm; AColor, AColorTo: TColor);
var
  PS:       Integer;
  i:        Integer;
  LRect:    TRect;
  LCanvas:  TCanvas;
begin
  LCanvas := AForm.Canvas;
  LRect   := Rect(0, 0, AForm.Width, 7);
  DrawGradient(LCanvas, AColor, AColorTo, 16, LRect, False);

  LCanvas.Pen.Color     := clWhite;
  LCanvas.Brush.Color   := clWhite;
  LRect.Top             := LRect.Top + 2;
  PS                    := (AForm.Width - 34) div 2;
  LRect.Left            := PS + 1;

  for i := 1 to 9 do begin
    LCanvas.Rectangle(LRect.Left, LRect.Top, LRect.Left + 2, LRect.Top + 2);
    LRect.Left := LRect.Left + 4;
  end;

  LCanvas.Pen.Color     := clBlack;
  LCanvas.Brush.Color   := clBlack;
  LRect.Top             := LRect.Top - 1;
  LRect.Left            := PS;
  for i := 1 to 9 do begin
    LCanvas.Rectangle(LRect.Left, LRect.Top, LRect.Left + 2, LRect.Top + 2);
    LRect.Left := LRect.Left + 4;
  end;

  LRect.Top             := LRect.Top + 1;
  LRect.Left            := PS + 1;
  for i := 1 to 9 do begin
    LCanvas.Pixels[LRect.Left, LRect.Top] := AColorTo;
    LRect.Left := LRect.Left + 4;
  end;
end;


procedure _DrawButtonImage(AButton: TCustomDrawButton; ACanvas: TCanvas; AImage: TBitmap);
begin
  if AImage = nil then
    AImage := AButton.Image;

  ACanvas.Brush.Style := bsClear;

  ACanvas.Draw
  (
    AButton.ClientRect.Left + 2,
    AButton.ClientRect.Top + 2,
    AImage
  );
end;


procedure _DrawCloseButton(AButton: TCustomDrawButton; ACanvas: TCanvas);
var
  LPoint: TPoint;
begin
  ACanvas.Pen.Color := clBlack;
  ACanvas.Pen.Width := 1;
  LPoint := Point(AButton.ClientRect.Left + 2, AButton.ClientRect.Top + 2);

  _Line(0, 7, 8, 0, LPoint, ACanvas);
  _Line(1, 7, 7, 0, LPoint, ACanvas);

  _Line(0, 1, 8, 8, LPoint, ACanvas);
  _Line(1, 1, 7, 8, LPoint, ACanvas);
end;


function _PrepareDrawNextPrevButtons(AButton: TCustomDrawButton; ACanvas: TCanvas): TPoint;
var
  LColor: TColor;
begin
  case AButton.State of
    dbsNone:
      LColor := clBlack;
    dbsHover, dbsClick:
      LColor := clBlue;
    //dbsDisable:
    else
      LColor := clGray;
  end;

  ACanvas.Pen.Color := LColor;
  ACanvas.Pen.Width := 1;
  Result := Point(AButton.ClientRect.Left + 6, AButton.ClientRect.Top + 2);
end;


procedure _DrawNextButton(AButton: TCustomDrawButton; ACanvas: TCanvas);
var
  LPoint: TPoint;
begin
  LPoint := _PrepareDrawNextPrevButtons(AButton, ACanvas);

  _Line(0, 1, 0, 12,  LPoint, ACanvas);
  _Line(1, 2, 1, 11,  LPoint, ACanvas);
  _Line(2, 3, 2, 10,  LPoint, ACanvas);
  _Line(3, 4, 3, 9,   LPoint, ACanvas);
  _Line(4, 5, 4, 8,   LPoint, ACanvas);
  _Line(5, 6, 5, 7,   LPoint, ACanvas);
end;


procedure _DrawPrevButton(AButton: TCustomDrawButton; ACanvas: TCanvas);
var
  LPoint: TPoint;
begin
  LPoint := _PrepareDrawNextPrevButtons(AButton, ACanvas);

  _Line(5, 1, 5, 12,  LPoint, ACanvas);
  _Line(4, 2, 4, 11,  LPoint, ACanvas);
  _Line(3, 3, 3, 10,  LPoint, ACanvas);
  _Line(2, 4, 2, 9,   LPoint, ACanvas);
  _Line(1, 5, 1, 8,   LPoint, ACanvas);
  _Line(0, 6, 0, 7,   LPoint, ACanvas);
end;


{$ENDREGION}

{$REGION 'TSBaseNotifyWindow'}


procedure TSBaseNotifyWindow.ClearHint;
begin
  FreeAndNil(FHintWindow);
end;


procedure TSBaseNotifyWindow.CMMouseLeave(var AMessage: TMessage);
begin
  DoMouseLeave;
end;


procedure TSBaseNotifyWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited;
  Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;
end;


destructor TSBaseNotifyWindow.Destroy;
begin
  TerminateThread;

  inherited;
end;

procedure TSBaseNotifyWindow.TerminateThread;
begin
  while Assigned(FFadeThread) do begin
    FFadeThread.Terminate;
    Application.ProcessMessages;
  end;
end;


procedure TSBaseNotifyWindow.DoButtonHint(ASender: TObject);
var
  LHintWindow: THintWindow;
  LRect: TRect;
  LPoint: TPoint;
begin
  FButtonHintTimer.Enabled := False;
  if not Assigned(FCurrentButton) then
    Exit;

  FreeAndNil(FHintWindow);

  LHintWindow         := HintWindowClass.Create(Self);
  try
    LHintWindow.Color   := clInfoBk;

    LRect               := LHintWindow.CalcHintRect(250, FCurrentButton.Hint, nil);
    LPoint              := ClientToScreen(FCurrentButton.ClientRect.TopLeft);
    LRect.TopLeft       := Point(LPoint.X - 5, LPoint.Y - 5);
    LRect.Top           := LRect.Top - LRect.Bottom;
    LRect.Right         := LRect.Right + LRect.Left;
    LRect.Bottom        := LRect.Bottom + LRect.Top;

    LHintWindow.ActivateHint(LRect, FCurrentButton.Hint);
    FHintWindow         := LHintWindow;
  except
    LHintWindow.Free;
    raise;
  end;
end;


procedure TSBaseNotifyWindow.DoFadeTerminate(ASender: TObject);
begin
  Assert(Pointer(FFadeThread) = Pointer(ASender), 'Fade thread termination error');
  FFadeThread := nil;
end;


procedure TSBaseNotifyWindow.DoMouseLeave;
begin
  if Assigned(FCurrentButton) then begin
    FCurrentButton.State := dbsNone;
    FCurrentButton := nil;
  end;

  if not PtInRect(ClientRect, ScreenToClient(Mouse.CursorPos)) then
    FadeWindow(ftHide);
end;


procedure TSBaseNotifyWindow.FadeWindow(AType: TWindowFadeType);
begin
  TerminateThread;

  if FFadeType <> AType then begin
    FFadeType   := AType;
    FFadeThread := TFadeWindowThread.Create(Self, AType);
    FFadeThread.OnTerminate := DoFadeTerminate;
  end;
end;


function TSBaseNotifyWindow.GetButtonAt(APoint: TPoint): TBaseDrawButton;
var
  LButton: TBaseDrawButton;
  i: Integer;
begin
  Result := nil;

  for i := 0 to ComponentCount - 1 do
    if Components[i] is TBaseDrawButton then begin
      LButton := Components[i] as TBaseDrawButton;

      if PtInRect(LButton.ClientRect, APoint) then begin
        Result := LButton;
        Break;
      end;
    end;
end;


procedure TSBaseNotifyWindow.InternalCreate;
begin
  FButtonHintTimer          := TTimer.Create(Self);
  FButtonHintTimer.OnTimer  := DoButtonHint;
  FButtonHintTimer.Interval := C_HintInterval;
  FButtonHintTimer.Enabled  := False;

  FFadeValue  := 150;

  inherited;

  AlphaBlend  := True;
end;


procedure TSBaseNotifyWindow.InternalRefresh;
var
  i: Integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TBaseDrawButton then
      RefreshButton(Components[i] as TBaseDrawButton);
end;


function TSBaseNotifyWindow.IsDragArea(APoint: TPoint): Boolean;
begin
  Result := True;
end;


procedure TSBaseNotifyWindow.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  ClearHint;
  FButtonHintTimer.Enabled := False;

  if (Button = mbLeft) then begin
    if IsDragArea(Point(X, Y)) then begin
      FIsDrag := True;
      FDragOldX := X;
      FDragOldY := Y;
      Cursor := crHandPoint;
      SetCapture(Handle);
      Exit;
    end;

    if Assigned(FCurrentButton) then begin
      FCurrentButton.State := dbsClick;
      Invalidate;
    end;
  end;

  if FIsDrag then begin
    ReleaseCapture;
    FIsDrag := False;
    Cursor := crDefault
  end;
end;


procedure TSBaseNotifyWindow.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  LRect: TRect;
  LButton: TBaseDrawButton;
begin
  inherited;

  if FIsDrag then begin
    // рамка окна в экранных координатах
    GetWindowRect(Handle, LRect);

    // сдвиг Drag`n`Drop-а
    OffsetRect(LRect, X - FDragOldX, Y - FDragOldY);

    // Правка положения окна: чтобы не выходило за границу Application.MainForm
    // Получаем LRect в экранных координатах
    AdjustRectToWindow(Application.MainForm.ClientHandle, LRect);

    // новое положение - в родительских координатах
    // SetWindowPos использует клиентские координаты родителя
    // т.к. окно без родителя, перевод Screen -> Client можно не делать
    Windows.SetWindowPos
    (
      Handle,
      HWND_TOPMOST,
      LRect.Left, LRect.Top,
      Width, Height,
      0
    );
  end
  else
    if PtInRect(ClientRect, Point(X, Y)) then begin

      LButton := GetButtonAt(Point(X, Y));

      if (LButton = nil) or (LButton <> FCurrentButton) then
        ClearHint;

      if LButton <> FCurrentButton then begin
        FButtonHintTimer.Enabled := False;

        if Assigned(FCurrentButton) then
          FCurrentButton.State := dbsNone;

        if Assigned(LButton) then begin
          LButton.State := dbsHover;

          if (not String.IsNullOrWhiteSpace(LButton.Hint)) and (Shift = []) then
            FButtonHintTimer.Enabled := True;
        end;

        FCurrentButton := LButton;
      end;

      if FFadeType = ftHide then
        FadeWindow(ftShow);
    end;
end;


procedure TSBaseNotifyWindow.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  LItemReaction: TBaseItemReaction;
begin
  inherited;

  if FIsDrag then begin
    ReleaseCapture;
    FIsDrag := False;
    Cursor := crDefault;
  end
  else
    if Assigned(FCurrentButton) then begin
      if FCurrentButton.State = dbsClick then begin
        FCurrentButton.State := dbsHover;

        if Assigned(FCurrentButton.OnClick) then
          FCurrentButton.OnClick(FCurrentButton)
        else begin
          LItemReaction := ItemsReactions.FindItemProperties(FCurrentButton);
          if Assigned(LItemReaction) then
            LItemReaction.Execute(GetFormParamValues, ConfirmMessage);
        end;
      end;
    end;
end;


procedure TSBaseNotifyWindow.Paint;
var
  LPicture: TPicture;
  i: Integer;
begin
  inherited;

  DrawGradient(Canvas, BeginColor, EndColor, 80, ClientRect, False);

  if FPictureName <> '' then begin
    LPicture := SBaseFormProperties.ImageList.FindPicture(FPictureName);
    Canvas.Draw(10, 15, LPicture.Graphic);
  end
  else if Assigned(Icon) then
    Canvas.Draw(10, 15, Icon);

  Canvas.Brush.Color := BorderColor;
  Canvas.FrameRect(ClientRect);

  for i := 0 to ComponentCount - 1 do
    if Components[i] is TBaseDrawButton then
      (Components[i] as TBaseDrawButton).Draw(Canvas);
end;


procedure TSBaseNotifyWindow.RefreshButton(AButton: TBaseDrawButton);
begin
end;


procedure TSBaseNotifyWindow.ShowAt(AX, AY: Integer);
begin
  Left  := AX;
  Top   := AY;

  Windows.ShowWindow(Handle, SW_SHOWNOACTIVATE);

  if PtInRect(ClientRect, ScreenToClient(Mouse.CursorPos)) then begin
    FFadeType := ftHide;
    FadeWindow(ftShow);
  end
  else begin
    FFadeType := ftShow;
    FadeWindow(ftHide);
  end;
end;


procedure TSBaseNotifyWindow.WMNCHitTest(var AMessage: TWMNCHitTest);
begin
  AMessage.Result := HTCLIENT;
end;


procedure TSBaseNotifyWindow.WMNCMouseLeave(var AMessage: TMessage);
begin
  DoMouseLeave;
end;


{$ENDREGION}

{$REGION 'TSBaseNotifyMessageWindow'}


function TSBaseNotifyMessageWindow.Add(const ACaption, AText: string; const AId: Integer): TSBaseNotifyMessageItem;
begin
  Result := nil;
  if FindById(AId) <> -1 then
    Exit;

  Result          := FMessages.Add as TSBaseNotifyMessageItem;
  Result.Id       := AId;
  Result.Caption  := ACaption;
  Result.Text     := AText;

  if ItemIndex = -1 then
    ItemIndex := Count - 1
  else
    InternalRefresh;
end;


procedure TSBaseNotifyMessageWindow.Clear;
begin
  if Assigned(FMessages) then
    FMessages.Clear;
end;


function TSBaseNotifyMessageWindow.DeleteById(AId: Integer): Boolean;
var
  i, LCurrentItemIndex: Integer;
begin
  Result := False;
  i := FindById(AId);
  if i <> -1 then begin
    LCurrentItemIndex := ItemIndex;

    if (LCurrentItemIndex > i) or ((LCurrentItemIndex = i) and (i > 0)) then
      LCurrentItemIndex := LCurrentItemIndex - 1;

    FMessages.Delete(i);
    ItemIndex := LCurrentItemIndex;
    Result := True;
  end;
end;


destructor TSBaseNotifyMessageWindow.Destroy;
begin
  Clear;
  FreeAndNil(FMessages);

  inherited;
end;


procedure TSBaseNotifyMessageWindow.DisplayMessage(AIndex: Integer);
var
  LMessage: TSBaseNotifyMessageItem;
begin
  if AIndex = -1 then begin
    FCaptionLabel.Caption := '';
    FTextLabel.Caption := '';
    FNumeratorLabel.Caption := '';

    Exit;
  end;

  LMessage := FMessages.Items[AIndex] as TSBaseNotifyMessageItem;
  FCaptionLabel.Caption := LMessage.Caption;
  FTextLabel.Caption := LMessage.Text;
  FNumeratorLabel.Caption := Format('%d/%d', [AIndex + 1, Count]);
  Invalidate;
end;


procedure TSBaseNotifyMessageWindow.DoCloseClick(ASender: TObject);
begin
  if Assigned(FOnCloseMessage) then
    FOnCloseMessage(Self)
  else
    Close;
end;


procedure TSBaseNotifyMessageWindow.DoNextClick(ASender: TObject);
begin
  ItemIndex := ItemIndex + 1
end;


procedure TSBaseNotifyMessageWindow.DoPrevClick(ASender: TObject);
begin
  ItemIndex := ItemIndex - 1
end;


function TSBaseNotifyMessageWindow.FindById(AId: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count -1 do begin
    if TSBaseNotifyMessageItem(FMessages.Items[i]).Id = AId then begin
      Result := i;
      Break;
    end;
  end;
end;


function TSBaseNotifyMessageWindow.GetCount: Integer;
begin
  Result := FMessages.Count;
end;


function TSBaseNotifyMessageWindow.GetCurrentItem: TSBaseNotifyMessageItem;
begin
  if (Count > ItemIndex) and (ItemIndex <> -1) then
    Result := FMessages.Items[ItemIndex] as TSBaseNotifyMessageItem
  else
    Result := nil;
end;


procedure TSBaseNotifyMessageWindow.InternalCreate;
begin
  FMessages       := TBaseNotifyMessageCollection.Create(Self);
  FUpdating       := False;
  FItemIndex      := -1;

  inherited;

  InternalRefresh;
end;


procedure TSBaseNotifyMessageWindow.InternalRefresh;
var
  LCount: Integer;
begin
  if FUpdating then
    Exit;

  inherited;

  LCount := Count;
  FNumeratorLabel.Visible := LCount > 1;
  DisplayMessage(ItemIndex);
end;


function TSBaseNotifyMessageWindow.IsDragArea(APoint: TPoint): Boolean;
begin
  Result := APoint.Y <= 7;
end;


procedure TSBaseNotifyMessageWindow.Paint;
begin
  inherited;

  DrawTopBar
  (
    Self,
    RGB(83, 129, 210),
    RGB(6, 51, 154)
  );

  Canvas.Pen.Color := $00A78F87;
  Canvas.MoveTo(3, Height - 34);
  Canvas.LineTo(Width - 3, Height - 34);
end;


procedure TSBaseNotifyMessageWindow.RefreshButton(AButton: TBaseDrawButton);
begin
  inherited;

  if AButton = FNextButton then begin
    AButton.Visible := Count > 1;
    AButton.Enable := ItemIndex < (Count - 1);
  end else if AButton = FPrevButton then begin
    AButton.Visible := Count > 1;
    AButton.Enable := ItemIndex > 0;
  end;
end;


procedure TSBaseNotifyMessageWindow.RefreshData;
begin
end;


procedure TSBaseNotifyMessageWindow.Resize;
begin
  inherited;

  FNumeratorLabel.Top     := Height - FNumeratorLabel.Height - 12;
  FNumeratorLabel.Left    := (Width - C_NumericLabelWidth) div 2;
  FNumeratorLabel.Width   :=  C_NumericLabelWidth;

  FCloseButton.DoPositionChange(Self);
  FNextButton.DoPositionChange(Self);
  FPrevButton.DoPositionChange(Self);

  Invalidate;
end;


procedure TSBaseNotifyMessageWindow.SetItemIndex(const AValue: Integer);
begin
  if Count = 0 then
    FItemIndex := -1
  else begin
    if InRange(AValue, 0, Count - 1) then
      FItemIndex := AValue;
  end;

  InternalRefresh;
end;


procedure TSBaseNotifyMessageWindow.Show;
var
  LRect: TRect;
  LPoint: TPoint;
begin
  if Visible then
    FormShow(Self);

  {$REGION 'Отобразить в правом нижнем углу'}
  Left  := 0;
  Top   := 0;

  LPoint := Point(0, 0);
  Windows.ClientToScreen(Handle, LPoint);
  Windows.GetWindowRect(Application.MainForm.ClientHandle, LRect);
  Windows.OffsetRect(LRect,  - LPoint.X, - LPoint.Y);
  LPoint := Point(LRect.Right - Width - 5, LRect.Bottom - Height - 5);

  ShowAt(LPoint.X, LPoint.Y);
  {$ENDREGION}
end;


{$ENDREGION}

{$REGION 'TBaseNotifyMessageCollection'}


constructor TBaseNotifyMessageCollection.Create(AMessageWindow: TSBaseNotifyMessageWindow);
begin
  inherited Create(TSBaseNotifyMessageItem);

  FWindow := AMessageWindow;
end;


{$ENDREGION}

{$REGION 'TCustomDrawButton'}

procedure TCustomDrawButton.ChangeImageIndex;
begin
  if Assigned(FImageList) and (FImageIndex >= 0) then begin
    FImageList.GetBitmap(FImageIndex, FImage);
    DoImageChage(nil);
  end;
end;

constructor TCustomDrawButton.Create(AParent: TComponent);
begin
  Assert(AParent is TSBaseNotifyWindow);

  inherited;

  FVisible                  := False;
  FEnable                   := True;
  FState                    := dbsNone;

  FClickColor               := $00A78F87;
  FHoverColor               := $00BBA9A2;
  FDrawType                 := dtClose;

  FImage                    := TBitmap.Create;
  //FImage.OnChange           := DoImageChage;

  FPosition                 := TButtonPosition.Create;
  FPosition.OnChange        := DoPositionChange;
end;

destructor TCustomDrawButton.Destroy;
begin
  FPosition.Free;
  FImage.Free;

  inherited;
end;

procedure TCustomDrawButton.DoImageChage(ASender: TObject);
begin
  if not FImage.Empty then begin
    if FImage.Width > (Position.Width - 2) then
      FImage.Width := Position.Width - 2;

    if FImage.Height > (Position.Height - 2) then
      FImage.Height := Position.Height - 2;
  end;
end;

procedure TCustomDrawButton.DoPositionChange(ASender: TObject);
begin
  FRect := Position.GetRect(Owner as TControl);
end;

{$WARNINGS OFF}
procedure TCustomDrawButton.Draw(ACanvas: TCanvas);
var
  LDisabledImage: TBitmap;
  LStream: TMemoryStream;
begin
  if not Visible then
    Exit;

  LDisabledImage := nil;

  try
    if FDrawType = dtImage then begin
      LDisabledImage := TBitmap.Create;
      LDisabledImage.PixelFormat      := pf32bit;
      LDisabledImage.HandleType       := bmDIB;
      LDisabledImage.Transparent      := True;
      LDisabledImage.TransparentMode  := tmAuto;

      LStream := TMemoryStream.Create;
      try
        Image.SaveToStream(LStream);
        LStream.Position := 0;
        LDisabledImage.LoadFromStream(LStream);
      finally
        FreeAndNil(LStream);
      end;

      BitmapToGrayscale(LDisabledImage, State = dbsDisable);
    end;

    if State in [dbsHover, dbsClick] then begin
      case State of
        dbsHover: ACanvas.Brush.Color := FHoverColor;
        dbsClick: ACanvas.Brush.Color := FClickColor;
      end;

      ACanvas.Pen.Color := clBlack;
      ACanvas.Rectangle(ClientRect);
    end;

    case FDrawType of
      dtClose: _DrawCloseButton(Self, ACanvas);
      dtRight: _DrawNextButton(Self, ACanvas);
      dtLeft:  _DrawPrevButton(Self, ACanvas);
      dtImage: _DrawButtonImage(Self, ACanvas, LDisabledImage);
    end;
  finally
    FreeAndNil(LDisabledImage);
  end;
end;
{$WARNINGS ON}

procedure TCustomDrawButton.SetDrawType(const AValue: TDrawType);
begin
  if FDrawType <> AValue then begin
    FDrawType := AValue;
    UpdateView;
  end;
end;

procedure TCustomDrawButton.SetEnable(const AValue: Boolean);
begin
  if AValue <> FEnable then begin
    FEnable := AValue;

    if not FEnable then
      FState := dbsDisable
    else
      FState := dbsNone;

    UpdateView;
  end;
end;

procedure TCustomDrawButton.SetImage(const AValue: TBitmap);
begin
  FImage.Assign(AValue);
end;

procedure TCustomDrawButton.SetImageIndex(const AValue: Integer);
begin
  if FImageIndex <> AValue then begin
    FImageIndex := AValue;
    ChangeImageIndex;
  end;
end;

procedure TCustomDrawButton.SetImageList(const AValue: TCustomImageList);
begin
  if FImageList <> AValue then begin
    FImageList := AValue;
    ChangeImageIndex;
  end;
end;

procedure TCustomDrawButton.SetState(const AValue: TDrawButtonState);
begin
  if (FState <> AValue) and Enable then begin
    FState := AValue;
    UpdateView;
  end;
end;

procedure TCustomDrawButton.SetVisible(const AValue: Boolean);
begin
  if FVisible <> AValue then begin
    FVisible := AValue;
    UpdateView;
  end;
end;

procedure TCustomDrawButton.UpdateView;
begin
  (Owner as TWinControl).Invalidate;
end;

{$ENDREGION}

{$REGION 'TSBaseDBNotifyMessageWindow'}


function TSBaseDBNotifyMessageWindow.CanLoadFromDataSet: Boolean;
begin
  Result := Assigned(FDataSource)
    and Assigned(FDataSource.DataSet)
    and FDataSource.DataSet.Active
    and (FCaptionFieldName <> '')
    and (FTextFieldName <> '')
    and (FIdFieldName <> '');
end;


procedure TSBaseDBNotifyMessageWindow.DoDataSetChande(ASender: TObject);
begin
  RefreshData;
end;


procedure TSBaseDBNotifyMessageWindow.RefreshData;
begin
  Clear;
  if CanLoadFromDataSet then
    LoadFromDataSet;

  ItemIndex := Count - 1;
end;


procedure TSBaseDBNotifyMessageWindow.ReOpen;
var
  LHasOldPos: Boolean;
  LOldId, LIndex: Integer;
  LDataSet: TDataSet;
begin
  LDataSet := MessagesDataSource.DataSet;
  if not Assigned(LDataSet) then
    Exit;

  // Запомнить позицию
  LOldId := -1;
  if (ItemIndex > -1) and (Count > 0) then begin
    LOldId      := CurrentItem.Id;
    LHasOldPos  := True;
  end
  else
    LHasOldPos  := False;

  // Обновить датасет
  UpdatingItems := True;
  try
    LDataSet.Close;
    LDataSet.Open;
  finally
    UpdatingItems := False;
  end;

  // Загрузить данные
  LoadFromDataSet;

  // Восстановить позицию
  if LHasOldPos then begin
    LIndex := FindById(LOldId);
    if LIndex = -1 then
      LIndex := Count - 1;
  end
  else
    LIndex := Count - 1;

  ItemIndex := LIndex;
end;


procedure TSBaseDBNotifyMessageWindow.LoadFromDataSet;
var
  LTextField: TField;
  LCaptionField: TField;
  LIdField: TField;
  LDataSet: TDataSet;
begin
  if UpdatingItems then
    Exit;

  UpdatingItems := True;
  try
    LDataSet := FDataSource.DataSet;
    LCaptionField := LDataSet.FieldByName(FCaptionFieldName);
    LTextField := LDataSet.FieldByName(FTextFieldName);
    LIdField := LDataSet.FindField(FIdFieldName);

    LDataSet.First;
    while not LDataSet.Eof do begin
      Add(LCaptionField.AsString, LTextField.AsString, LIdField.AsInteger);
      LDataSet.Next;
    end;
  finally
    UpdatingItems := False;
  end;
end;


procedure TSBaseDBNotifyMessageWindow.SetCaptionFieldName(AValue: string);
begin
  AValue := AValue.Trim;
  if FCaptionFieldName <> AValue then begin
    FCaptionFieldName := AValue;
    RefreshData;
  end;
end;


procedure TSBaseDBNotifyMessageWindow.SetDataSource(const AValue: TDataSource);
begin
  if FDataSource <> AValue then begin
    FDataSource := AValue;

    if Assigned(FDataSource) then
      FDataSource.OnStateChange := DoDataSetChande;

    RefreshData;
  end;
end;


procedure TSBaseDBNotifyMessageWindow.SetIdFieldName(AValue: string);
begin
  AValue := AValue.Trim;
  if FIdFieldName <> AValue then begin
    FIdFieldName := AValue;
    RefreshData;
  end;
end;


procedure TSBaseDBNotifyMessageWindow.SetTextFieldName(AValue: string);
begin
  AValue := AValue.Trim;
  if FTextFieldName <> AValue then begin
    FTextFieldName := AValue;
    RefreshData;
  end;
end;


{$ENDREGION}

{$REGION 'TButtonPosition'}


procedure TButtonPosition.Change;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;


function TButtonPosition.GetRect(AOwner: TControl): TRect;
var
  LX, LY: Integer;
begin
  Result := Rect(0, 0, Width, Height);

  LX := Max(AOWner.Width - DistanceX - Width, 0);
  LY := Max(AOWner.Height - DistanceY - Height, 0);

  case AlignType of
    atTopLeft:      OffsetRect(Result, DistanceX, DistanceY);
    atTopRight:     OffsetRect(Result, LX, DistanceY);
    atBottomLeft:   OffsetRect(Result, DistanceX, LY);
    atBottomRight:  OffsetRect(Result, LX, LY);
  end;
end;


procedure TButtonPosition.SetAlignType(const AValue: TAlignType);
begin
  if AValue <> FAlignType then begin
    FAlignType := AValue;
    Change;
  end;
end;


procedure TButtonPosition.SetDistanceX(const AValue: Integer);
begin
  if AValue <> FDistanceX then begin
    FDistanceX := AValue;
    Change;
  end;
end;


procedure TButtonPosition.SetDistanceY(const AValue: Integer);
begin
  if AValue <> FDistanceY then begin
    FDistanceY := AValue;
    Change;
  end;
end;


procedure TButtonPosition.SetHeight(const AValue: Integer);
begin
  if AValue <> FHeight then begin
    FHeight := AValue;
    Change;
  end;
end;


procedure TButtonPosition.SetWidth(const AValue: Integer);
begin
  if AValue <> FWidth then begin
    FWidth := AValue;
    Change;
  end;
end;


{$ENDREGION}

{$REGION 'Tfs_TSBaseNotifyMessageWindow'}


function Tfs_TSBaseNotifyMessageWindow.Call_TSBaseDBNotifyMessageWindowMethod
(
  Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper
): Variant;
begin
  Assert(Instance is TSBaseDBNotifyMessageWindow);
  if MethodName = 'REOPEN' then
    (Instance as TSBaseDBNotifyMessageWindow).ReOpen;
end;


function Tfs_TSBaseNotifyMessageWindow.Call_TSBaseNotifyMessageWindowMethod
(
  Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper
): Variant;
begin
  Assert(Instance is TSBaseNotifyMessageWindow);
  if MethodName = 'DELETEBYID' then
    Result := TSBaseNotifyMessageWindow(Instance).DeleteById(Caller.Params[0])
  else if MethodName = 'ADDMESSAGE' then
    Result := Integer(Pointer(TSBaseNotifyMessageWindow(Instance).Add(Caller.Params[0], Caller.Params[1], Caller.Params[2])));
end;


constructor Tfs_TSBaseNotifyMessageWindow.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseNotifyMessageWindow, TSBaseNotifyMessageWindow.ClassParent.ClassName) do begin
    AddEvent('OnCloseMessage', TfsNotifyEvent);

    AddMethod('function DeleteById(AId: Integer): Boolean', Call_TSBaseNotifyMessageWindowMethod);
    AddMethod('function AddMessage(const ACaption, AText: string; const AId: Integer = -1): TSBaseNotifyMessageItem', Call_TSBaseNotifyMessageWindowMethod);
  end;

  with AScript.AddClass(TSBaseDBNotifyMessageWindow, TSBaseDBNotifyMessageWindow.ClassParent.ClassName) do begin
    AddMethod('procedure ReOpen', Call_TSBaseDBNotifyMessageWindowMethod);
  end;
end;


{$ENDREGION}

{$REGION 'TWindowShowThread'}


constructor TFadeWindowThread.Create(AWindow: TSBaseNotifyWindow; AType: TWindowFadeType);
begin
  FWindow := AWindow;
  FType := AType;

  FreeOnTerminate := True;
  inherited Create(False);

  Priority := tpHigher;

  {$IFDEF DEBUG}
  Inc(FDebugCounter);
  FDebugCurrentCounter := FDebugCounter;

  PrintDebugString('TFadeWindowThread.Create [' + IntToStr(FDebugCurrentCounter) + ']');
  {$ENDIF}
end;


procedure TFadeWindowThread.Execute;
var
  LCounter, LEndValue: Byte;
begin
  inherited;

{$IFDEF DEBUG}
  PrintDebugString('TFadeWindowThread.Execute [' + IntToStr(FDebugCurrentCounter) + ']: begin');
{$ENDIF}

  if FType = ftShow then begin
    LCounter    := FWindow.FadeValue;
    LEndValue   := 255;
  end
  else begin
    LCounter    := 255;
    LEndValue   := FWindow.FadeValue;
  end;

{$IFDEF DEBUG}
  PrintDebugString('TFadeWindowThread.Execute [' + IntToStr(FDebugCurrentCounter) + ']: to: ' + IntToStr(LEndValue));
{$ENDIF}

  while LCounter <> LEndValue do begin
    if FType = ftShow then
      Inc(LCounter)
    else
      Dec(LCounter);

{$IFDEF DEBUG}
    PrintDebugString('TFadeWindowThread.Execute [' + IntToStr(FDebugCurrentCounter) + ']: loop ' + IntToStr(LCounter));
{$ENDIF}

    Synchronize(procedure begin FWindow.AlphaBlendValue := LCounter end);

    Sleep(3);

    if Finished then begin
      Synchronize(procedure begin FWindow.AlphaBlendValue := LEndValue end);
{$IFDEF DEBUG}
      PrintDebugString('TFadeWindowThread.Execute [' + IntToStr(FDebugCurrentCounter) + ']: break');
{$ENDIF}
      Break;
    end;
  end;

{$IFDEF DEBUG}
  PrintDebugString('TFadeWindowThread.Execute [' + IntToStr(FDebugCurrentCounter) + ']: end');
{$ENDIF}
end;


{$ENDREGION}

initialization
{$IFDEF DEBUG}
  TFadeWindowThread.FDebugCounter := 0;
{$ENDIF}

  Classes.RegisterClasses
  (
    [
      TBaseDrawButton,
      TSBaseNotifyWindow,
      TSBaseNotifyMessageItem,
      TSBaseNotifyMessageWindow,
      TSBaseDBNotifyMessageWindow
    ]
  );

  fsRTTIModules.Add(Tfs_TSBaseNotifyMessageWindow);

finalization
  Classes.UnRegisterClasses
  (
    [
      TBaseDrawButton,
      TSBaseNotifyWindow,
      TSBaseNotifyMessageItem,
      TSBaseNotifyMessageWindow,
      TSBaseDBNotifyMessageWindow
    ]
  );

  fsRTTIModules.Remove(Tfs_TSBaseNotifyMessageWindow);

end.

