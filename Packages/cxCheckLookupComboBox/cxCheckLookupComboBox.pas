unit cxCheckLookupComboBox;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI6}
  Variants,
{$ENDIF}
  Types, DB, Windows, Messages, SysUtils, Classes, Controls, Graphics, StdCtrls,
  Forms, Math, dxCore, cxClasses, cxControls, cxContainer, cxGraphics,
  cxDataStorage, cxDataUtils, cxVariants, cxEdit, cxEditUtils,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox, cxGeometry,
  cxExtEditUtils, cxCheckBox, cxLookAndFeels, cxExtEditConsts,
  cxGroupBox, cxLookAndFeelPainters, cxDBEdit, dxCoreClasses;

const
  cxCheckLookupComboKeysSeparator = ',';

type
  TDataEvents = set of TDataEvent;

  { TcxCheckLookupComboBoxItem }
  TcxCheckLookupComboBoxItem = class(TcxButtonGroupItem)
  private
    FShortDescription: TCaption;
    FId: TcxEditValue;
    function GetDescription: TCaption;
    procedure SetDescription(const Value: TCaption);
    procedure SetShortDescription(const Value: TCaption);
  protected
    function GetDisplayDescription: string; virtual;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Id: TcxEditValue read FId write FId;
    property Description: TCaption read GetDescription write SetDescription;
    property Enabled;
    property ShortDescription: TCaption read FShortDescription write SetShortDescription;
    property Tag;
  end;

  { TcxCheckLookupComboBoxItems }

  TcxCheckLookupComboBoxItems = class(TcxButtonGroupItems)
  private
    function GetItems(Index: Integer): TcxCheckLookupComboBoxItem;
    procedure SetItems(Index: Integer; const Value: TcxCheckLookupComboBoxItem);
  public
    function Add: TcxCheckLookupComboBoxItem;
    function AddCheckItem(const ADescription: TCaption; const AShortDescription: TCaption = ''): TcxCheckLookupComboBoxItem;
    property Items[Index: Integer]: TcxCheckLookupComboBoxItem read GetItems write SetItems; default;
  end;

  { TcxCustomCheckLookupComboBoxViewData }

  TcxCustomCheckLookupComboBoxProperties = class;

  TcxCustomCheckLookupComboBoxViewData = class(TcxCustomDropDownEditViewData)
  protected
    function GetProperties: TcxCustomCheckLookupComboBoxProperties;
    function InternalEditValueToDisplayText(AEditValue: TcxEditValue): string; override;
    function IsComboBoxStyle: Boolean; override;
  public
    property Properties: TcxCustomCheckLookupComboBoxProperties read GetProperties;
  end;

  { TcxCheckLookupComboBoxListBox }

  TcxCustomCheckLookupComboBox = class;

  TcxCustomCheckLookupComboBoxListBox = class(TcxCustomComboBoxListBox, IUnknown, IcxMouseTrackingCaller)
  private
    FCapturedCheckIndex: Integer;
    FCheckBorderOffset: Integer;
    FCheckSize: TSize;
    FHotCheckIndex: Integer;
    FInternalUpdate: Boolean;
    FPressedCheckIndex: Integer;

    function GetEdit: TcxCustomCheckLookupComboBox;
    procedure LBGetTextLen(var Message: TMessage); message LB_GETTEXTLEN;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    // IcxMouseTrackingCaller
    procedure IcxMouseTrackingCaller.MouseLeave = MouseTrackingMouseLeave;
    procedure MouseTrackingMouseLeave;

    procedure CheckHotTrack;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure FullRepaint;
    function GetCheckAt(X, Y: Integer): Integer;
    function GetCheckRect(const R: TRect; AReturnFullRect: Boolean): TRect;
    procedure InternalMouseMove(Shift: TShiftState; X, Y: Integer);
    procedure InvalidateCheck(Index: Integer);
    procedure KeyPress(var Key: Char); override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure RecreateWindow; override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure SynchronizeCheckStates(ANewHotCheckIndex, ANewPressedIndex: Integer);
    procedure UpdateItemState(const AIndex: Integer); virtual;
    property Edit: TcxCustomCheckLookupComboBox read GetEdit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetItemHeight(AIndex: Integer = -1): Integer; override;
    function GetItemWidth(AIndex: Integer): Integer; override;
  end;

  { TcxCheckLookupComboBoxLookupData }

  TcxCheckLookupComboBoxLookupData = class(TcxComboBoxLookupData)
  protected
    function GetListBoxClass: TcxCustomEditListBoxClass; override;
    function GetItem(Index: Integer): string; override;
    function GetItemCount: Integer; override;
  public
    procedure TextChanged; override;
  end;

  { TcxCheckPaintHelper }

  TcxCheckPaintHelper = class(TcxOwnedPersistent)
  private
    FStyle: TcxCustomEditStyle;
    FGlyph: TBitmap;
    FGlyphCount: Integer;
    procedure SetStyle(Value: TcxCustomEditStyle);
  protected
    function GetNextState(Value: TcxCheckBoxState): TcxCheckBoxState; virtual;
    property Style: TcxCustomEditStyle read FStyle write SetStyle;
    property Glyph: TBitmap read FGlyph write FGlyph;
    property GlyphCount: Integer read FGlyphCount write FGlyphCount;
    function CalcTextRect(const ARect: TRect; const ACheckState: TcxCheckBoxState): TRect; virtual;
    function CalcCheckPoint(const ARect: TRect; const ACheckState: TcxCheckBoxState): TPoint; virtual;
    function IsClickInCheck(const AItemRect: TRect; const ACheckState: TcxCheckBoxState; X, Y: Integer; const AUseRightToLeftAlignment: Boolean = False): Boolean;
    function GetCheckWidth: Integer; virtual;
  public
    constructor Create(AOwner : TPersistent); override;
    destructor Destroy; override;
  end;

  { TcxCustomCheckLookupComboBoxProperties }

  TcxCheckLookupComboClickCheckEvent = procedure(Sender: TObject; ItemIndex: Integer; var AllowToggle: Boolean) of object;

  TcxCustomCheckLookupComboBoxPropertiesListLink = class(TDataLink)
  private
    FBusy: Boolean;
    FProperties: TcxCustomCheckLookupComboBoxProperties;
  protected
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    procedure ActiveChanged; override;
    procedure UpdateData; override;
    procedure DataSetRecordsChanged;
  public
    constructor Create(AProperties: TcxCustomCheckLookupComboBoxProperties); reintroduce;
  end;

  TcxCustomCheckLookupComboBoxProperties = class(TcxCustomComboBoxProperties)
  private
    FCheckPaintHelper : TcxCheckPaintHelper;

    FKeysSeparator: Char;
    FValuesSeparator: string;

    FEmptySelectionText: string;
    FGlyph: TBitmap;
    FGlyphCount: Integer;

    FListLink: TDataLink;
    FKeyField: String;
    FListField: String;
    FDisplayField: String;

    FItems: TcxCheckLookupComboBoxItems;

    FOnClickCheck: TcxCheckLookupComboClickCheckEvent;
    FOnEditValueToStates: TcxValueToCheckStatesEvent;
    FOnStatesToEditValue: TcxCheckStatesToValueEvent;

    function GetGlyph: TBitmap;
    procedure GlyphChanged(Sender: TObject);
    function IsEmptySelectionTextStored: Boolean;
    procedure SetGlyph(Value: TBitmap);
    procedure SetGlyphCount(Value: Integer);
    procedure SetEmptySelectionText(Value: string);

//    function GetKeysSeparator: string;
    procedure SetKeysSeparator(const AValue: Char);
//    function IsKeysSeparatorStored: Boolean;

    procedure SetValuesSeparator(Value: string);
    function IsValuesSeparatorStored: Boolean;

    procedure SetListSource(const Value: TDataSource);
    procedure SetKeyField(const Value: String);
    procedure SetListField(const Value: String);
    procedure SetDisplayField(const Value: String);
    procedure ListLinkChanged;
    function GetListSource: TDataSource;
  protected
    class function GetLookupDataClass: TcxInterfacedPersistentClass; override;
    class function GetPopupWindowClass: TcxCustomEditPopupWindowClass; override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;

    function DropDownOnClick: Boolean; override;
    function HasDisplayValue: Boolean; override;
    procedure CalculateCheckStatesByEditValue(Sender: TObject; const AEditValue: TcxEditValue; var ACheckStates: TcxCheckStates); virtual;
    function CalculateDisplayValueByCheckStates(const ACheckStates: TcxCheckStates): string; virtual;
    function CalculateEditValueByCheckStates(Sender: TObject; const ACheckStates: TcxCheckStates): TcxEditValue; virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function GetSupportedOperations: TcxEditSupportedOperations; override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;
    function IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean; override;
    // !!!
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 6;
    property KeysSeparator: Char read FKeysSeparator write SetKeysSeparator default cxCheckLookupComboKeysSeparator;
    property ValuesSeparator: string read FValuesSeparator write SetValuesSeparator stored IsValuesSeparatorStored;
    property EmptySelectionText : string read FEmptySelectionText write SetEmptySelectionText stored IsEmptySelectionTextStored;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property KeyField: String read FKeyField write SetKeyField;
    property ListField: String read FListField write SetListField;
    property DisplayField: String read FDisplayField write SetDisplayField;

    property OnClickCheck: TcxCheckLookupComboClickCheckEvent read FOnClickCheck write FOnClickCheck;
    property OnEditValueToStates: TcxValueToCheckStatesEvent read FOnEditValueToStates write FOnEditValueToStates;
    property OnStatesToEditValue: TcxCheckStatesToValueEvent read FOnStatesToEditValue write FOnStatesToEditValue;
  end;

  { TcxCheckLookupComboBoxProperties }

  TcxCheckLookupComboBoxProperties = class(TcxCustomCheckLookupComboBoxProperties)
  published
    property AssignedValues;
    property KeysSeparator;
    property ValuesSeparator;
    property EmptySelectionText;
    property Alignment;
    property BeepOnError;
    property ButtonGlyph;
    property CharCase;
    property ClearKey;
    property DropDownAutoWidth;
    property DropDownRows;
    property DropDownSizeable;
    property DropDownWidth;
    property Glyph;
    property GlyphCount;
    property ImeMode;
    property ImeName;
    property ImmediateDropDownWhenActivated;
//    property ImmediateDropDown;
//    property ImmediatePopup;
    property ImmediatePost;
    property ListSource;
    property KeyField;
    property ListField;
    property DisplayField;
    property PopupAlignment;
    property ReadOnly;
//    property ValidateOnEnter;
    property ValidationOptions;
    property OnChange;
    property OnClickCheck;
    property OnCloseUp;
    property OnEditValueChanged;
    property OnEditValueToStates;
    property OnInitPopup;
    property OnPopup;
    property OnStatesToEditValue;
  end;

  { TcxCheckLookupComboBoxPopupWindow }

  TcxCheckLookupComboBoxPopupWindow = class(TcxComboBoxPopupWindow)
  public
    property ViewInfo;
    property SysPanelStyle;
  end;

  { TcxCustomCheckLookupComboBox }

  TcxCustomCheckLookupComboBox = class(TcxCustomComboBox)
  private
    FCheckBorderStyle: TcxEditCheckBoxBorderStyle;
    FNativeStyle: Boolean;
    FStates: TcxCheckStates;
    function GetActiveProperties: TcxCustomCheckLookupComboBoxProperties;
    function GetLookupData: TcxCheckLookupComboBoxLookupData;
    function GetProperties: TcxCustomCheckLookupComboBoxProperties;
    procedure SetProperties(Value: TcxCustomCheckLookupComboBoxProperties);
  protected
    procedure CalculateDrawCheckParams;
    procedure PopupControlsLookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure PopupWindowShowed(Sender: TObject); override;
    procedure PropertiesChanged(Sender: TObject); override;
    function DoRefreshContainer(const P: TPoint; Button: TcxMouseButton; Shift: TShiftState; AIsMouseEvent: Boolean): Boolean; override;
    procedure SetItemIndex(Value: Integer); override;
    procedure SynchronizeDisplayValue; override;
    procedure SynchronizeEditValue; override;
    procedure UpdateDrawValue; override;
    function ClickCheck(AItemIndex: Integer): Boolean; virtual;
    function DoClickCheck(AItemIndex: Integer): Boolean;
    function GetEditingValue: TcxEditValue; override;
    procedure HandleSelectItem(Sender: TObject); override;
    procedure Initialize; override;
    procedure InitializePopupWindow; override;
    function InternalGetText: string; override;
    function InternalSetText(const Value: string): Boolean; override;
    procedure InternalValidateDisplayValue(const ADisplayValue: TcxEditValue); override;
    procedure KeyPress(var Key: Char); override;
    property LookupData: TcxCheckLookupComboBoxLookupData read GetLookupData;
    procedure CloseUp(AReason: TcxEditCloseUpReason); override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCustomCheckLookupComboBoxProperties read GetActiveProperties;
    property Properties: TcxCustomCheckLookupComboBoxProperties read GetProperties write SetProperties;
  end;

  { TcxCheckLookupComboBox }

  TcxCheckLookupComboBox = class(TcxCustomCheckLookupComboBox)
  private
    function GetActiveProperties: TcxCheckLookupComboBoxProperties;
    function GetProperties: TcxCheckLookupComboBoxProperties;
    procedure SetProperties(Value: TcxCheckLookupComboBoxProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCheckLookupComboBoxProperties read GetActiveProperties;
  published
    property Properties: TcxCheckLookupComboBoxProperties read GetProperties write SetProperties;

    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property EditValue; // to be after StatesItems (B34627)
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
{$IFDEF DELPHI5}
    property OnContextPopup;
{$ENDIF}
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TcxDBCheckLookupComboBox = class(TcxCustomCheckLookupComboBox)
  private
    function GetActiveProperties: TcxCheckLookupComboBoxProperties;
    function GetDataBinding: TcxDBTextEditDataBinding;
    function GetProperties: TcxCheckLookupComboBoxProperties;
    procedure SetDataBinding(Value: TcxDBTextEditDataBinding);
    procedure SetProperties(Value: TcxCheckLookupComboBoxProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCheckLookupComboBoxProperties read GetActiveProperties;
  published
    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
    property Properties: TcxCheckLookupComboBoxProperties read GetProperties write SetProperties;

    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
//    property EditValue; // to be after StatesItems (B34627)
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
{$IFDEF DELPHI5}
    property OnContextPopup;
{$ENDIF}
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TcxEditRepositoryCheckLookupComboBoxItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxCheckLookupComboBoxProperties;
    procedure SetProperties(Value: TcxCheckLookupComboBoxProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxCheckLookupComboBoxProperties read GetProperties write SetProperties;
  end;

  procedure Register;

const
  scxSEditRepositoryCheckLookupComboBox: String = 'CheckLookupComboBox|Represents a CheckLookupComboBox';
  cxCheckLookupComboValuesSeparator = '; ';

implementation

uses
  dxThemeManager, SBaseStringFunctions;

procedure Register;
begin
  RegisterComponents ('MD Project Tools', [TcxCheckLookupComboBox, TcxDBCheckLookupComboBox]);
end;

{ TcxCheckLookupComboBoxItem }

procedure TcxCheckLookupComboBoxItem.Assign(Source: TPersistent);
begin
  if Source is TcxCheckLookupComboBoxItem then
    ShortDescription := TcxCheckLookupComboBoxItem(Source).ShortDescription;
  inherited Assign(Source);
end;

function TcxCheckLookupComboBoxItem.GetDisplayDescription: string;
begin
  if ShortDescription <> '' then
    Result := ShortDescription
  else
    Result := Description;
end;

function TcxCheckLookupComboBoxItem.GetDescription: TCaption;
begin
  Result := Caption;
end;

procedure TcxCheckLookupComboBoxItem.SetDescription(const Value: TCaption);
begin
  Caption := Value;
end;

procedure TcxCheckLookupComboBoxItem.SetShortDescription(const Value: TCaption);
begin
  if Value <> FShortDescription then
  begin
    FShortDescription := Value;
    DoChanged(Collection, copChanged);
  end;
end;

{ TcxCheckLookupComboBoxItems }

function TcxCheckLookupComboBoxItems.GetItems(Index: Integer): TcxCheckLookupComboBoxItem;
begin
  Result := TcxCheckLookupComboBoxItem(inherited Items[Index]);
end;

procedure TcxCheckLookupComboBoxItems.SetItems(Index: Integer;const Value: TcxCheckLookupComboBoxItem);
begin
  inherited Items[Index] := Value;
end;

function TcxCheckLookupComboBoxItems.Add: TcxCheckLookupComboBoxItem;
begin
  Result := TcxCheckLookupComboBoxItem(inherited Add);
end;

function TcxCheckLookupComboBoxItems.AddCheckItem(const ADescription: TCaption;
  const AShortDescription: TCaption = ''): TcxCheckLookupComboBoxItem;
begin
  Result := Add;
  Result.Description := ADescription;
  Result.ShortDescription := AShortDescription;
end;

{ TcxCustomCheckLookupComboBoxViewData }

function TcxCustomCheckLookupComboBoxViewData.GetProperties: TcxCustomCheckLookupComboBoxProperties;
begin
  Result := TcxCustomCheckLookupComboBoxProperties(FProperties);
end;

function TcxCustomCheckLookupComboBoxViewData.InternalEditValueToDisplayText(AEditValue: TcxEditValue): string;
var
  ASender: TObject;
  LCheckStates: TcxCheckStates;
begin
  if IsInplace then
    ASender := nil
  else
    ASender := Edit;

  Properties.CalculateCheckStatesByEditValue(ASender, AEditValue, LCheckStates);
  Result := Properties.CalculateDisplayValueByCheckStates(LCheckStates);
  Properties.DisplayValueToDisplayText(Result);
end;

function TcxCustomCheckLookupComboBoxViewData.IsComboBoxStyle: Boolean;
begin
  Result := False; // TODO: ≈сли поставить True - будет какой-то серенький контрол. ’от€ может это и правильно ...;
end;

{ TcxCustomCheckLookupComboBoxListBox }

constructor TcxCustomCheckLookupComboBoxListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCapturedCheckIndex := -1;
  FHotCheckIndex := -1;
  FInternalUpdate := False;
  FPressedCheckIndex := -1;
  Style := lbOwnerDrawFixed;
end;

destructor TcxCustomCheckLookupComboBoxListBox.Destroy;
begin
  EndMouseTracking(Self);
  inherited Destroy;
end;

function TcxCustomCheckLookupComboBoxListBox.GetItemHeight(AIndex: Integer = -1): Integer;
begin
  if Edit.ActiveProperties.ItemHeight > 0 then
    Result := Edit.ActiveProperties.ItemHeight
  else
    Result := inherited GetItemHeight(AIndex);
  if Result < FCheckSize.cy + 2 then
    Result := FCheckSize.cy + 2;
end;

function TcxCustomCheckLookupComboBoxListBox.GetItemWidth(AIndex: Integer): Integer;
begin
  Result := inherited GetItemWidth(AIndex);
  Inc(Result, ItemHeight);
end;

procedure TcxCustomCheckLookupComboBoxListBox.MouseTrackingMouseLeave;
begin
  InternalMouseMove([], -1, -1);
  EndMouseTracking(Self);
end;

procedure TcxCustomCheckLookupComboBoxListBox.CheckHotTrack;
var
  P: TPoint;
begin
  P := ScreenToClient(GetMouseCursorPos);
  InternalMouseMove(KeyboardStateToShiftState, P.X, P.Y);
end;

function TcxCustomCheckLookupComboBoxListBox.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  CheckHotTrack;
end;

procedure TcxCustomCheckLookupComboBoxListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);

  function GetCheckState(AIsItemEnabled: Boolean): TcxEditCheckState;
  begin
    if not AIsItemEnabled then
      Result := ecsDisabled
    else
      if FHotCheckIndex = Index then
        Result := ecsHot
      else
        if FPressedCheckIndex = Index then
          Result := ecsPressed
        else
          Result := ecsNormal;
  end;

  function GetCheckBorderStyle(ACheckState: TcxEditCheckState): TcxEditCheckBoxBorderStyle;
  begin
    if not Edit.FNativeStyle and (Edit.FCheckBorderStyle = ebsFlat) and
        (ACheckState in [ecsHot, ecsPressed]) then
      Result := ebs3D
    else
      Result := Edit.FCheckBorderStyle;
  end;

var
  ACheckState: TcxEditCheckState;
  AFlags: Longint;
  AText: string;
  ATextRect : TRect;
begin
  if DoDrawItem(Index, Rect, State) then
    Exit;

  with Edit.ActiveProperties do
  begin
    Canvas.FillRect(Rect);
    FCheckPaintHelper.Glyph := Glyph;
    FCheckPaintHelper.GlyphCount := GlyphCount;
    ATextRect := FCheckPaintHelper.CalcTextRect(Rect, Edit.FStates[Index]);
    AText := GetItem(Index);
    AFlags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_LEFT or DT_VCENTER or DT_NOPREFIX);

    if not FItems[Index].Enabled then
      Canvas.Font.Color := Edit.StyleDisabled.TextColor;
    cxDrawText(Canvas.Handle, AText, ATextRect, AFlags);

    if Rect.Top >= 0 then
    begin
      ACheckState := GetCheckState(FItems[Index].Enabled);
      SaveCanvasParametersForFocusRect;
      DrawEditCheck(Canvas, GetCheckRect(Rect, True), Edit.FStates[Index],
        ACheckState, Glyph, GlyphCount, GetCheckBorderStyle(ACheckState),
        Edit.FNativeStyle, clBtnText, Color, False, False, False, False,
        Edit.Style.LookAndFeel.SkinPainter);
      RestoreCanvasParametersForFocusRect;
    end;
  end;
end;

procedure TcxCustomCheckLookupComboBoxListBox.FullRepaint;
var
  R: TRect;
begin
  if HandleAllocated then
  begin
    R := GetControlRect(Self);
    InvalidateRect(Handle, @R, True);
  end;
end;

function TcxCustomCheckLookupComboBoxListBox.GetCheckAt(X, Y: Integer): Integer;
begin
  Result := ItemAtPos(Point(X, Y), True);
end;

function TcxCustomCheckLookupComboBoxListBox.GetCheckRect(const R: TRect; AReturnFullRect: Boolean): TRect;
begin
// ToDo RightToLeftAlignment
  with R do
  begin
    Result.Top := (Bottom - Top - FCheckSize.cy) div 2;
    Result.Left := Result.Top;
    Inc(Result.Top, Top);
    Result.Bottom := Result.Top + FCheckSize.cy;
    if Result.Left < 1 then
      Result.Left := 1;
    Result.Right := Result.Left + FCheckSize.cx;
  end;
  if AReturnFullRect then
    InflateRect(Result, FCheckBorderOffset, FCheckBorderOffset);
end;

procedure TcxCustomCheckLookupComboBoxListBox.InternalMouseMove(Shift: TShiftState; X, Y: Integer);
var
  ANewHotCheckIndex, ANewPressedCheckIndex: Integer;
begin
  ANewHotCheckIndex := FHotCheckIndex;
  ANewPressedCheckIndex := FPressedCheckIndex;
  if FCapturedCheckIndex <> -1 then
  begin
    ANewHotCheckIndex := -1;
    if GetCheckAt(X, Y) = FCapturedCheckIndex then
      ANewPressedCheckIndex := FCapturedCheckIndex
    else
      ANewPressedCheckIndex := -1;
  end;
  if (GetCaptureControl <> Self) and cxShiftStateMoveOnly(Shift) then
    ANewHotCheckIndex := GetCheckAt(X, Y);
  SynchronizeCheckStates(ANewHotCheckIndex, ANewPressedCheckIndex);
end;

procedure TcxCustomCheckLookupComboBoxListBox.InvalidateCheck(Index: Integer);
var
  R: TRect;
begin
  R := ItemRect(Index);
  R := GetCheckRect(R, False);
  InvalidateRect(Handle, @R, False);
end;

procedure TcxCustomCheckLookupComboBoxListBox.KeyPress(var Key: Char);
begin
  if (ItemIndex <> -1) and Assigned(Edit) then
  begin
    if (Key = Char(VK_SPACE)) then
    begin
      if not (Edit.ActiveProperties.FItems[ItemIndex].Enabled) then Exit;
      FInternalUpdate := True;
      UpdateItemState(ItemIndex);
      FInternalUpdate := False;
      Key := #0;
    end;
  end
  else
    inherited KeyPress(Key);
end;

procedure TcxCustomCheckLookupComboBoxListBox.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  RecreateWindow;
end;

procedure TcxCustomCheckLookupComboBoxListBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ANewPressedCheckIndex: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    ANewPressedCheckIndex := GetCheckAt(X, Y);
    FCapturedCheckIndex := ANewPressedCheckIndex;
  end
  else
    ANewPressedCheckIndex := -1;
  SynchronizeCheckStates(-1, ANewPressedCheckIndex);
end;

procedure TcxCustomCheckLookupComboBoxListBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  APopupMouseMoveLocked: Boolean;
begin
  APopupMouseMoveLocked := Edit.PopupMouseMoveLocked;
  inherited MouseMove(Shift, X, Y);
  if not APopupMouseMoveLocked then
  begin
    InternalMouseMove(Shift, X, Y);
    BeginMouseTracking(Self, GetControlRect(Self), Self);
  end;
end;

procedure TcxCustomCheckLookupComboBoxListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AItemIndex: Integer;
  ANewHotCheckIndex, ANewPressedCheckIndex: Integer;
begin
  inherited MouseUp(Button, Shift, X, Y);
  ANewPressedCheckIndex := FPressedCheckIndex;
  if Button = mbLeft then
  begin
    SetCaptureControl(nil);
    AItemIndex := ItemAtPos(Point(X, Y), True);
    if (AItemIndex <> -1) and Edit.ActiveProperties.FItems[ItemIndex].Enabled then
    begin
      Edit.LookupData.InternalSetCurrentKey(AItemIndex);
      if AItemIndex = FPressedCheckIndex then
        UpdateItemState(AItemIndex);
    end;
    ANewPressedCheckIndex := -1;
  end;
  if cxShiftStateMoveOnly(Shift) then
    ANewHotCheckIndex := GetCheckAt(X, Y)
  else
    ANewHotCheckIndex := -1;
  SynchronizeCheckStates(ANewHotCheckIndex, ANewPressedCheckIndex);
end;

procedure TcxCustomCheckLookupComboBoxListBox.RecreateWindow;
begin
  with Edit do begin
    FCheckSize := GetEditCheckSize(Self.Canvas, PopupControlsLookAndFeel.NativeStyle,
      ActiveProperties.Glyph, ActiveProperties.GlyphCount, PopupControlsLookAndFeel.SkinPainter);

    FCheckBorderOffset := GetEditCheckBorderOffset(TcxContainerBorderStyle(FCheckBorderStyle),
      FNativeStyle, IsGlyphAssigned(ActiveProperties.Glyph) and (ActiveProperties.GlyphCount > 0),
      LookAndFeel.SkinPainter);
  end;
  FCheckSize.cx := FCheckSize.cx - FCheckBorderOffset * 2;
  FCheckSize.cy := FCheckSize.cy - FCheckBorderOffset * 2;

  ItemHeight := GetItemHeight;
  if Edit.IsOnMeasureItemEventAssigned then
    Style := lbOwnerDrawVariable
  else
    Style := lbOwnerDrawFixed;
end;

procedure TcxCustomCheckLookupComboBoxListBox.SetItemIndex(const Value: Integer);
begin
  if not FInternalUpdate then
    inherited SetItemIndex(Value);
end;

procedure TcxCustomCheckLookupComboBoxListBox.SynchronizeCheckStates(ANewHotCheckIndex,
  ANewPressedIndex: Integer);
begin
  if ANewHotCheckIndex <> FHotCheckIndex then
  begin
    InvalidateCheck(FHotCheckIndex);
    FHotCheckIndex := ANewHotCheckIndex;
    InvalidateCheck(FHotCheckIndex);
  end;
  if ANewPressedIndex <> FPressedCheckIndex then
  begin
    InvalidateCheck(FPressedCheckIndex);
    FPressedCheckIndex := ANewPressedIndex;
    InvalidateCheck(FPressedCheckIndex);
  end;
end;

procedure TcxCustomCheckLookupComboBoxListBox.UpdateItemState(const AIndex: Integer);
var
  ACurrentState: TcxCheckBoxState;
begin
  if Edit.ClickCheck(AIndex) then
  begin
    with Edit.ActiveProperties do
    begin
      ACurrentState := Edit.FStates[AIndex];
      Edit.FStates[AIndex] := FCheckPaintHelper.GetNextState(ACurrentState);
      Edit.SynchronizeEditValue;
      Edit.ModifiedAfterEnter := True;
    end;
    if Edit.ActiveProperties.ImmediatePost and Edit.CanPostEditValue then
      Edit.InternalPostEditValue;
    InvalidateCheck(AIndex);
  end;
end;

function TcxCustomCheckLookupComboBoxListBox.GetEdit: TcxCustomCheckLookupComboBox;
begin
  Result := TcxCustomCheckLookupComboBox(inherited Edit);
end;

procedure TcxCustomCheckLookupComboBoxListBox.LBGetTextLen(var Message: TMessage);
begin
  Message.Result := 0;
end;

procedure TcxCustomCheckLookupComboBoxListBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  BeginMouseTracking(Self, GetControlRect(Self), Self);
end;

procedure TcxCustomCheckLookupComboBoxListBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  InternalMouseMove([], -1, -1);
  EndMouseTracking(Self);
end;

{ TcxCheckLookupComboBoxLookupData }

procedure TcxCheckLookupComboBoxLookupData.TextChanged;
begin
end;

function TcxCheckLookupComboBoxLookupData.GetListBoxClass: TcxCustomEditListBoxClass;
begin
  Result := TcxCustomCheckLookupComboBoxListBox;
end;

function TcxCheckLookupComboBoxLookupData.GetItem(Index: Integer): string;
begin
  with TcxCustomCheckLookupComboBox(Edit).ActiveProperties do
    if (Index > -1) and (Index < FItems.Count) then
      Result := FItems[Index].Description
    else
      Result := ''
end;

function TcxCheckLookupComboBoxLookupData.GetItemCount: Integer;
begin
  Result := TcxCustomCheckLookupComboBox(Edit).ActiveProperties.FItems.Count;
end;

{ TcxCheckLookupComboBox }

class function TcxCheckLookupComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckLookupComboBoxProperties;
end;

function TcxCheckLookupComboBox.GetActiveProperties: TcxCheckLookupComboBoxProperties;
begin
  Result := TcxCheckLookupComboBoxProperties(InternalGetActiveProperties);
end;

function TcxCheckLookupComboBox.GetProperties: TcxCheckLookupComboBoxProperties;
begin
  Result := TcxCheckLookupComboBoxProperties(FProperties);
end;

procedure TcxCheckLookupComboBox.SetProperties(Value: TcxCheckLookupComboBoxProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxCheckPaintHelper }

constructor TcxCheckPaintHelper.Create(AOwner : TPersistent);
begin
  inherited;
  FStyle := TcxCustomEditStyle.Create(Self, False);
end;

destructor TcxCheckPaintHelper.Destroy;
begin
  FreeAndNil(FStyle);
  inherited Destroy;
end;

procedure TcxCheckPaintHelper.SetStyle(Value: TcxCustomEditStyle);
begin
  FStyle.Assign(Value);
end;

function TcxCheckPaintHelper.GetNextState(Value: TcxCheckBoxState): TcxCheckBoxState;
begin
  case Value of
    cbsChecked:
      Result := cbsUnchecked;
    cbsGrayed:
      Result := cbsUnchecked;
    else
      Result := cbsChecked;
  end;
end;

function TcxCheckPaintHelper.GetCheckWidth: Integer;
begin
  Result := 13;
end;

function TcxCheckPaintHelper.CalcTextRect(const ARect: TRect; const ACheckState: TcxCheckBoxState): TRect;
begin
  Result := ARect;
  Inc(Result.Left, cxRectHeight(ARect) + 2);
end;

function TcxCheckPaintHelper.CalcCheckPoint(const ARect: TRect;
  const ACheckState: TcxCheckBoxState): TPoint;
var
  FCenterPos : Integer;
begin
  FCenterPos := CalcCenterPosHeight(ARect, GetCheckWidth);
  Result.X := ARect.Left + FCenterPos;
  Result.Y := ARect.Top + FCenterPos;
end;

function TcxCheckPaintHelper.IsClickInCheck(const AItemRect: TRect;
  const ACheckState: TcxCheckBoxState; X, Y: Integer;
  const AUseRightToLeftAlignment: Boolean = False): Boolean;
var
  FCheckRect : TRect;
  FCenterPos : Integer;
begin
  FCenterPos := CalcCenterPosHeight(AItemRect, GetCheckWidth);
  if not AUseRightToLeftAlignment then
    FCheckRect := Bounds(AItemRect.Left + FCenterPos, AItemRect.Top + FCenterPos,
      GetCheckWidth, GetCheckWidth)
  else
    FCheckRect := Bounds(AItemRect.Right - (FCenterPos * 3), AItemRect.Top + FCenterPos,
      GetCheckWidth, GetCheckWidth);
  Result := (X >= FCheckRect.Left) and (X <= FCheckRect.Right) and
            (Y >= FCheckRect.Top) and (Y <= FCheckRect.Bottom);
end;

{ TcxCustomCheckLookupComboBoxProperties }

constructor TcxCustomCheckLookupComboBoxProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);

  DropDownListStyle := lsFixedList;

  FKeysSeparator := cxCheckLookupComboKeysSeparator;
  FValuesSeparator := cxCheckLookupComboValuesSeparator;

  FEmptySelectionText := cxGetResourceString(@cxSCheckComboBoxEmptySelectionText);
  FCheckPaintHelper := TcxCheckPaintHelper.Create(nil);
  FItems := TcxCheckLookupComboBoxItems.Create(Self, TcxCheckLookupComboBoxItem);
  FGlyphCount := 6;

  FListLink := TcxCustomCheckLookupComboBoxPropertiesListLink.Create(Self);
end;

destructor TcxCustomCheckLookupComboBoxProperties.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FCheckPaintHelper);
  FreeAndNil(FListLink);
  FreeAndNil(FGlyph);
  inherited;
end;

function TcxCustomCheckLookupComboBoxProperties.DropDownOnClick: Boolean;
begin
  Result := False;
end;

function TcxCustomCheckLookupComboBoxProperties.GetGlyph: TBitmap;
begin
  if FGlyph = nil then
  begin
    FGlyph := TBitmap.Create;
    FGlyph.OnChange := GlyphChanged;
  end;
  Result := FGlyph;
end;

{
function TcxCustomCheckLookupComboBoxProperties.GetKeysSeparator: string;
begin
  if FKeysSeparator = #0 then
    Result := ''
  else
    Result := String(FKeysSeparator);
end;
}

procedure TcxCustomCheckLookupComboBoxProperties.GlyphChanged(Sender: TObject);
begin
  Changed;
end;

procedure TcxCustomCheckLookupComboBoxProperties.ListLinkChanged;
var
  LKeyField: TField;
  LListField: TField;
  LDisplayField: TField;
  LBookmark: TBookmark;
begin
//  if ChangedLocked then Exit;
  
  BeginUpdate;
//  LockUpdate(True);
//  FItems.BeginUpdate;
  try
    FItems.Clear;
    if (FListLink.DataSet <> nil) and (FListLink.DataSet.RecordCount > 0) and (FKeyField <> '') and (FListField <> '') then
      with FListLink.DataSet do begin
        LKeyField := FieldByName(FKeyField);
        LListField := FieldByName(FListField);
        LDisplayField := FindField(FDisplayField);
        DisableControls;
        LBookmark := Bookmark;
        try
          First;
          while not Eof do begin
            with FItems.Add do begin
              Id := LKeyField.AsVariant;
              Description := LListField.AsString;
              if LDisplayField <> nil then
                ShortDescription := LDisplayField.AsString
            end;
            Next;
          end;
        finally
          Bookmark := LBookmark;
          EnableControls;
        end;
      end;
  finally
//    FItems.EndUpdate;
//    LockUpdate(False);
    EndUpdate;
//    Changed;
  end;
end;

function TcxCustomCheckLookupComboBoxProperties.IsEmptySelectionTextStored: Boolean;
begin
  Result := FEmptySelectionText <> cxGetResourceString(@cxSCheckComboBoxEmptySelectionText);
end;

procedure TcxCustomCheckLookupComboBoxProperties.SetGlyph(Value: TBitmap);
begin
  if Value = nil then
    FreeAndNil(FGlyph)
  else
    Glyph.Assign(Value);
  Changed;
end;

procedure TcxCustomCheckLookupComboBoxProperties.SetGlyphCount(Value: Integer);
begin
  if FGlyphCount <> Value then
  begin
    FGlyphCount := Value;
    if FGlyph <> nil then
      Changed;
  end;
end;

procedure TcxCustomCheckLookupComboBoxProperties.SetKeysSeparator(const AValue: Char);
begin
  if FKeysSeparator <> AValue then begin
    FKeysSeparator := AValue;
    Changed;
  end;
end;
{
function TcxCustomCheckLookupComboBoxProperties.IsKeysSeparatorStored: Boolean;
begin
  Result := FKeysSeparator <> cxCheckLookupComboKeysSeparator;
end;
}

procedure TcxCustomCheckLookupComboBoxProperties.SetValuesSeparator(Value: string);
begin
  if Value = '' then
    raise Exception.Create('Invalid ValuesSeparator value = ''' + Value + '''');

  if FValuesSeparator <> Value then
  begin
    FValuesSeparator := Value;
    Changed;
  end;
end;

function TcxCustomCheckLookupComboBoxProperties.IsValuesSeparatorStored: Boolean;
begin
  Result := FValuesSeparator <> cxCheckLookupComboValuesSeparator;
end;

procedure TcxCustomCheckLookupComboBoxProperties.Assign(Source: TPersistent);
begin
  if Source is TcxCustomCheckLookupComboBoxProperties then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      with TcxCustomCheckLookupComboBoxProperties(Source) do
      begin
        Self.KeysSeparator := KeysSeparator;
        Self.ValuesSeparator := ValuesSeparator;
        Self.ListSource := ListSource;
        Self.KeyField := KeyField;
        Self.ListField := ListField;
        Self.DisplayField := DisplayField;
        Self.Glyph := Glyph;
        Self.GlyphCount := GlyphCount;
        Self.EmptySelectionText := EmptySelectionText;
        Self.OnClickCheck := OnClickCheck;
        Self.OnEditValueToStates := OnEditValueToStates;
        Self.OnStatesToEditValue := OnStatesToEditValue;
      end;
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

class function TcxCustomCheckLookupComboBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxCheckLookupComboBox;
end;

function TcxCustomCheckLookupComboBoxProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  Result := evsValue;
end;

function TcxCustomCheckLookupComboBoxProperties.GetSupportedOperations: TcxEditSupportedOperations;
begin
  Result := inherited GetSupportedOperations;
  Include(Result, esoSortingByDisplayText);
end;

class function TcxCustomCheckLookupComboBoxProperties.GetLookupDataClass: TcxInterfacedPersistentClass;
begin
  Result := TcxCheckLookupComboBoxLookupData;
end;

class function TcxCustomCheckLookupComboBoxProperties.GetPopupWindowClass: TcxCustomEditPopupWindowClass;
begin
  Result := TcxCheckLookupComboBoxPopupWindow;
end;

class function TcxCustomCheckLookupComboBoxProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxCustomCheckLookupComboBoxViewData;
end;

function TcxCustomCheckLookupComboBoxProperties.HasDisplayValue: Boolean;
begin
  Result := False;
end;

function TcxCustomCheckLookupComboBoxProperties.GetListSource: TDataSource;
begin
  Result := FListLink.DataSource;
end;

procedure TcxCustomCheckLookupComboBoxProperties.SetListSource(const Value: TDataSource);
begin
  FListLink.DataSource := Value;
end;

procedure TcxCustomCheckLookupComboBoxProperties.SetKeyField(const Value: String);
begin
  if FKeyField <> Value then begin
    FKeyField := Value;
    ListLinkChanged;
  end;
end;

procedure TcxCustomCheckLookupComboBoxProperties.SetDisplayField(const Value: String);
begin
  if FDisplayField <> Value then begin
    FDisplayField := Value;
    ListLinkChanged;
  end;
end;

procedure TcxCustomCheckLookupComboBoxProperties.SetListField(const Value: String);
begin
  if FListField <> Value then begin
    FListField := Value;
    ListLinkChanged;
  end;
end;

procedure TcxCustomCheckLookupComboBoxProperties.SetEmptySelectionText(Value: string);
begin
  if FEmptySelectionText <> Value then
  begin
    FEmptySelectionText := Value;
    Changed;
  end;
end;

(*
function InArray(const SArray, SElement: String; SDelimeter: String = ','): Boolean;
begin
	if (SArray <> '') and (SElement <> '') and (SArray <> SDelimeter) then
    Result := Pos(SDelimeter + SElement + SDelimeter, SDelimeter + SArray + SDelimeter) > 0
  else
    Result := False;
end;
*)

procedure TcxCustomCheckLookupComboBoxProperties.CalculateCheckStatesByEditValue(Sender: TObject; const AEditValue: TcxEditValue; var ACheckStates: TcxCheckStates);
var
  I: Integer;
  LEditValue: String;
  LInArray: Boolean;
begin
  SetLength(ACheckStates, FItems.Count);

  if Assigned(OnEditValueToStates) then
  begin
    OnEditValueToStates(Sender, AEditValue, ACheckStates)
  end else begin
    LEditValue := VarToStr(AEditValue);
    for I := 0 to FItems.Count - 1 do begin
      if FKeysSeparator = #0 then
        LInArray := Pos(VarToStr(FItems[I].Id), LEditValue) > 0
      else
        LInArray := InArray(LEditValue, VarToStr(FItems[I].Id), FKeysSeparator);

      if LInArray then
        ACheckStates[I] := cbsChecked
      else
        ACheckStates[I] := cbsUnChecked
    end;
  end;
end;

function TcxCustomCheckLookupComboBoxProperties.CalculateDisplayValueByCheckStates(const ACheckStates: TcxCheckStates): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FItems.Count - 1 do
  begin
    if ACheckStates[I] <> cbsChecked then
      Continue;

    if Result <> '' then
        Result := Result + FValuesSeparator;

    Result := Result + FItems[I].GetDisplayDescription;
  end;

  if (Result = '') then
    Result := EmptySelectionText;
end;

function TcxCustomCheckLookupComboBoxProperties.CalculateEditValueByCheckStates(Sender: TObject; const ACheckStates: TcxCheckStates): TcxEditValue;
var
  LResult: String;
  I: Integer;
begin
  if Assigned(OnStatesToEditValue) then
    OnStatesToEditValue(Sender, ACheckStates, Result)
  else begin
    LResult := '';

    for I := 0 to FItems.Count - 1 do
    begin
      if ACheckStates[I] <> cbsChecked then
        Continue;

      if (FKeysSeparator <> #0) and (not LResult.IsEmpty) then
        LResult := LResult + FKeysSeparator;

      LResult := LResult + VarToStr(FItems[I].Id);
    end;

    if LResult.IsEmpty then
      Result := Null
    else
      Result := LResult;
  end;
end;

function TcxCustomCheckLookupComboBoxProperties.IsEditValueValid(var EditValue: TcxEditValue; AEditFocused: Boolean): Boolean;
begin
  Result := True;
end;

procedure TcxCustomCheckLookupComboBoxProperties.PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean);
var
  LCheckStates: TcxCheckStates;
begin
  CalculateCheckStatesByEditValue(nil, AEditValue, LCheckStates);
  DisplayValue := CalculateDisplayValueByCheckStates(LCheckStates);
end;

{ TcxCustomCheckLookupComboBox }

class function TcxCustomCheckLookupComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomCheckLookupComboBoxProperties;
end;

function TcxCustomCheckLookupComboBox.ClickCheck(AItemIndex: Integer): Boolean;
begin
  Result := CanModify and DoClickCheck(AItemIndex) and DoEditing;
end;

function TcxCustomCheckLookupComboBox.DoClickCheck(AItemIndex: Integer): Boolean;

  function InternalDoClickCheck(AProperties: TcxCustomCheckLookupComboBoxProperties;
    AItemIndex: Integer): Boolean;
  begin
    Result := True;
    if Assigned(AProperties.OnClickCheck) then
      AProperties.OnClickCheck(Self, AItemIndex, Result);
  end;

var
  AAllowToggle1, AAllowToggle2: Boolean;
begin
  AAllowToggle1 := InternalDoClickCheck(Properties, AItemIndex);
  if RepositoryItem <> nil then
    AAllowToggle2 := InternalDoClickCheck(
      TcxCustomCheckLookupComboBoxProperties(RepositoryItem.Properties), AItemIndex)
  else
    AAllowToggle2 := True;
  Result := AAllowToggle1 and AAllowToggle2;
end;

function TcxCustomCheckLookupComboBox.GetEditingValue: TcxEditValue;
begin
  Result := EditValue;
end;

procedure TcxCustomCheckLookupComboBox.HandleSelectItem(Sender: TObject);
begin
end;

procedure TcxCustomCheckLookupComboBox.Initialize;
begin
  inherited Initialize;
  FEditValue := 0;
  ControlStyle := ControlStyle - [csClickEvents];
  CalculateDrawCheckParams;
end;

procedure TcxCustomCheckLookupComboBox.InitializePopupWindow;
begin
  inherited InitializePopupWindow;
  (PopupWindow as TcxCheckLookupComboBoxPopupWindow).SysPanelStyle := ActiveProperties.PopupSizeable;
end;

function TcxCustomCheckLookupComboBox.InternalGetText: string;
begin
  if IsDestroying then
    Result := ''
  else
    Result := ViewInfo.Text;
end;

function TcxCustomCheckLookupComboBox.InternalSetText(const Value: string): Boolean;
begin
  Result := False;
end;

procedure TcxCustomCheckLookupComboBox.InternalValidateDisplayValue(const ADisplayValue: TcxEditValue);
begin
end;

procedure TcxCustomCheckLookupComboBox.KeyPress(var Key: Char);
begin
  if (Key = Char(VK_SPACE)) and HasPopupWindow then
  begin
    if Assigned(OnKeyPress) then
      OnKeyPress(Self, Key);
    (LookupData.ActiveControl as TcxCustomCheckLookupComboBoxListBox).KeyPress(Key)
  end
  else
    inherited KeyPress(Key);
end;

procedure TcxCustomCheckLookupComboBox.CloseUp(AReason: TcxEditCloseUpReason);
begin
  if HasPopupWindow then
  begin
    FCloseUpReason := AReason;  
    PopupWindow.CloseUp;
    if not ActiveProperties.AlwaysPostEditValue and ActiveProperties.ImmediatePost and CanPostEditValue and ValidateEdit(True) then
      InternalPostEditValue;
  end;
end;

function TcxCustomCheckLookupComboBox.GetProperties: TcxCustomCheckLookupComboBoxProperties;
begin
  Result := TcxCustomCheckLookupComboBoxProperties(FProperties);
end;

function TcxCustomCheckLookupComboBox.GetActiveProperties: TcxCustomCheckLookupComboBoxProperties;
begin
  Result := TcxCustomCheckLookupComboBoxProperties(InternalGetActiveProperties);
end;

function TcxCustomCheckLookupComboBox.GetLookupData: TcxCheckLookupComboBoxLookupData;
begin
  Result := TcxCheckLookupComboBoxLookupData(FLookupData);
end;

procedure TcxCustomCheckLookupComboBox.SetProperties(Value: TcxCustomCheckLookupComboBoxProperties);
begin
  FProperties.Assign(Value);
end;

procedure TcxCustomCheckLookupComboBox.CalculateDrawCheckParams;
const
  ABorderStyleMap: array[TcxLookAndFeelKind] of TcxEditCheckBoxBorderStyle =
    (ebsFlat, ebs3D, ebsUltraFlat, ebsOffice11);
begin
  with PopupControlsLookAndFeel do
  begin
    FNativeStyle := NativeStyle and AreVisualStylesAvailable([totButton, totComboBox]);
    if not FNativeStyle then
      FCheckBorderStyle := ABorderStyleMap[Kind];
  end;
end;

procedure TcxCustomCheckLookupComboBox.PopupControlsLookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited PopupControlsLookAndFeelChanged(Sender, AChangedValues);
  if not IsDestroying then
    CalculateDrawCheckParams;
end;

procedure TcxCustomCheckLookupComboBox.PopupWindowShowed(Sender: TObject);
begin
  inherited PopupWindowShowed(Sender);
  TcxCustomCheckLookupComboBoxListBox(ILookupData.ActiveControl).CheckHotTrack;
end;

procedure TcxCustomCheckLookupComboBox.PropertiesChanged(Sender: TObject);
begin
  with ActiveProperties.FItems do
    if ItemChanged and (ChangedItemOperation = copDelete) and (ChangedItemIndex < Length(FStates) - 1) then
      Move(FStates[ChangedItemIndex + 1], FStates[ChangedItemIndex],
        (ActiveProperties.FItems.Count - 1 - ChangedItemIndex) * SizeOf(TcxCheckBoxState));

  SetLength(FStates, ActiveProperties.FItems.Count);
  ActiveProperties.CalculateCheckStatesByEditValue(GetStandaloneEventSender(Self), EditValue, FStates);

  inherited PropertiesChanged(Sender);

  if ModifiedAfterEnter then
    UpdateDrawValue
  else
    if IsDBEdit then
      SynchronizeDisplayValue
    else
      EditValue := ActiveProperties.CalculateEditValueByCheckStates(GetStandaloneEventSender(Self), FStates);
end;

function TcxCustomCheckLookupComboBox.DoRefreshContainer(const P: TPoint;
  Button: TcxMouseButton; Shift: TShiftState; AIsMouseEvent: Boolean): Boolean;
begin
  Result := inherited DoRefreshContainer(P, Button, Shift, AIsMouseEvent);
  ActiveProperties.FCheckPaintHelper.Style.Assign(ActiveStyle);
end;

procedure TcxCustomCheckLookupComboBox.SetItemIndex(Value: Integer);
begin
  LookupData.InternalSetCurrentKey(Value);
end;

procedure TcxCustomCheckLookupComboBox.SynchronizeDisplayValue;
begin
  ActiveProperties.CalculateCheckStatesByEditValue(GetStandaloneEventSender(Self), EditValue, FStates);
  UpdateDrawValue;
end;

procedure TcxCustomCheckLookupComboBox.SynchronizeEditValue;
begin
  InternalEditValue := ActiveProperties.CalculateEditValueByCheckStates(GetStandaloneEventSender(Self), FStates);
end;

procedure TcxCustomCheckLookupComboBox.UpdateDrawValue;
begin
  TcxCustomTextEditViewInfo(ViewInfo).SelLength := 0;
  TcxCustomTextEditViewInfo(ViewInfo).Text := ActiveProperties.CalculateDisplayValueByCheckStates(FStates);
  with TcxCustomTextEditViewInfo(ViewInfo) do
    Self.ActiveProperties.DisplayValueToDisplayText(Text);
  with TcxCustomTextEditViewInfo(ViewInfo) do
    TextOutData.Initialized := False;
  InvalidateRect(TcxCustomTextEditViewInfo(ViewInfo).ClientRect, False);
end;

{ TcxCustomCheckLookupComboBoxPropertiesListLink }

constructor TcxCustomCheckLookupComboBoxPropertiesListLink.Create(AProperties: TcxCustomCheckLookupComboBoxProperties);
begin
  FProperties := AProperties;
  inherited Create;
end;

procedure TcxCustomCheckLookupComboBoxPropertiesListLink.DataEvent(Event: TDataEvent; Info: NativeInt);
//var
//  LEnableEvents: TDataEvents;
begin
  inherited;
  if (Event in [deDataSetChange, deLayoutChange]) then
    DataSetRecordsChanged;
{
  if (Event in [deDataSetChange, deLayoutChange]) and (Info <> 0) then begin
    LEnableEvents := [];
    PWord(@LEnableEvents)^ := Info;
    if (LEnableEvents * [deFieldChange, deFieldListChange]) <> [] then
      DataSetRecordsChanged;
  end;
}
end;

procedure TcxCustomCheckLookupComboBoxPropertiesListLink.DataSetRecordsChanged;
begin
  if (not FBusy) and (not Editing) then begin
    FBusy := True;
    try
      FProperties.ListLinkChanged;
    finally
      FBusy := False;
    end;
  end;
end;

procedure TcxCustomCheckLookupComboBoxPropertiesListLink.UpdateData;
begin
  DataSetRecordsChanged;
end;

procedure TcxCustomCheckLookupComboBoxPropertiesListLink.ActiveChanged;
begin
  DataSetRecordsChanged;
end;

{ TcxDBCheckLookupComboBox }

procedure TcxDBCheckLookupComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(GetcxDBEditDataLink(Self));
end;

function TcxDBCheckLookupComboBox.GetActiveProperties: TcxCheckLookupComboBoxProperties;
begin
  Result := TcxCheckLookupComboBoxProperties(InternalGetActiveProperties);
end;

function TcxDBCheckLookupComboBox.GetDataBinding: TcxDBTextEditDataBinding;
begin
  Result := TcxDBTextEditDataBinding(FDataBinding);
end;

class function TcxDBCheckLookupComboBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBTextEditDataBinding;
end;

function TcxDBCheckLookupComboBox.GetProperties: TcxCheckLookupComboBoxProperties;
begin
  Result := TcxCheckLookupComboBoxProperties(FProperties);
end;

class function TcxDBCheckLookupComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckLookupComboBoxProperties;
end;

procedure TcxDBCheckLookupComboBox.SetDataBinding(Value: TcxDBTextEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBCheckLookupComboBox.SetProperties(Value: TcxCheckLookupComboBoxProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxEditRepositoryCheckLookupComboBoxItem }

class function TcxEditRepositoryCheckLookupComboBoxItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCheckLookupComboBoxProperties
end;

function TcxEditRepositoryCheckLookupComboBoxItem.GetProperties: TcxCheckLookupComboBoxProperties;
begin
  Result := inherited Properties as TcxCheckLookupComboBoxProperties
end;

procedure TcxEditRepositoryCheckLookupComboBoxItem.SetProperties(Value: TcxCheckLookupComboBoxProperties);
begin
  inherited Properties := Value;
end;

initialization
  RegisterClasses([TcxCheckLookupComboBoxProperties, TcxEditRepositoryCheckLookupComboBoxItem]);
  GetRegisteredEditProperties.Register(TcxCheckLookupComboBoxProperties, scxSEditRepositoryCheckLookupComboBox);

finalization
  GetRegisteredEditProperties.Unregister(TcxCheckLookupComboBoxProperties);
  UnRegisterClasses([TcxCheckLookupComboBoxProperties, TcxEditRepositoryCheckLookupComboBoxItem]);

end.
