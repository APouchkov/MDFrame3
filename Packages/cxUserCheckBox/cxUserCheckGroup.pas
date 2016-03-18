
{********************************************************************}
{                                                                    }
{       Developer Express Visual Component Library                   }
{       ExpressEditors                                               }
{                                                                    }
{       Copyright (c) 1998-2013 Developer Express Inc.               }
{       ALL RIGHTS RESERVED                                          }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit cxUserCheckGroup;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Graphics, Messages, Controls,
  dxCore, cxContainer, cxGraphics, cxLookAndFeels,cxEdit, cxGroupBox,
  cxCheckBox, cxFilterControlUtils, cxCheckComboBox,
  cxLookAndFeelPainters;

const
  cxCheckGroupKeysSeparator = ',';

type
  { TcxUserCheckGroupItem }

  TcxUserCheckGroupItem = class(TcxButtonGroupItem)
  private
    FValue: TcxEditValue;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Value: TcxEditValue read FValue write FValue;

    property Caption;
    property Enabled;
    property Tag;
  end;

  { TcxUserCheckGroupItems }

  TcxUserCheckGroupItems = class(TcxButtonGroupItems)
  private
    function GetItem(Index: Integer): TcxUserCheckGroupItem;
    procedure SetItem(Index: Integer; Value: TcxUserCheckGroupItem);
  public
    function Add: TcxUserCheckGroupItem;
    property Items[Index: Integer]: TcxUserCheckGroupItem read GetItem write SetItem; default;
  end;

  { TcxUserCheckGroupButtonViewInfo }

  TcxUserCheckGroupButtonViewInfo = class(TcxButtonGroupButtonViewInfo)
  public
    Checked: Boolean;
    function GetGlyphRect(const AGlyphSize: TSize; AAlignment: TLeftRight; AIsPaintCopy: Boolean): TRect; override;
  end;

  TcxCustomUserCheckGroup = class;

  { TcxUserCheckGroupViewInfo }

  TcxUserCheckGroupViewInfo = class(TcxButtonGroupViewInfo)
  private
    function GetEdit: TcxCustomUserCheckGroup;
  protected
    procedure DrawButtonCaption(ACanvas: TcxCanvas; AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect); override;
    procedure DrawButtonGlyph(ACanvas: TcxCanvas; AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect); override;
    function GetButtonViewInfoClass: TcxEditButtonViewInfoClass; override;
    function IsButtonGlypthTransparent(AButtonViewInfo: TcxGroupBoxButtonViewInfo): Boolean; override;

    procedure SetOnDrawBackground(AValue: TcxEditDrawBackgroundEvent); override;
  public
    CheckBorderStyle: TcxEditCheckBoxBorderStyle;
    property Edit: TcxCustomUserCheckGroup read GetEdit;
  end;

  { TcxUserCheckGroupViewData }

  TcxUserCheckGroupViewData = class(TcxButtonGroupViewData)
  protected
    function GetCaptionRectExtent: TRect; override;
    procedure GetEditMetrics(AAutoHeight: Boolean; ACanvas: TcxCanvas; out AMetrics: TcxEditMetrics); override;
  public
    procedure Calculate(ACanvas: TcxCanvas; const ABounds: TRect; const P: TPoint;
      Button: TcxMouseButton; Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo;
      AIsMouseEvent: Boolean); override;
    procedure EditValueToDrawValue(const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo); override;
  end;

  { TcxCustomUserCheckGroupProperties }

  TcxCustomUserCheckGroupProperties = class(TcxCustomButtonGroupProperties)
  private
//    FAllowGrayed: Boolean;
    FKeysSeparator: Char;
    FGlyph: TBitmap;
    FGlyphCount: Integer;
    FItemAlignment: TLeftRight;
//    FOnEditValueToStates: TcxValueToCheckStatesEvent;
//    FOnStatesToEditValue: TcxCheckStatesToValueEvent;

    function GetGlyph: TBitmap;
    function GetItems: TcxUserCheckGroupItems;
    procedure GlyphChanged(Sender: TObject);
//    procedure SetAllowGrayed(Value: Boolean);
    procedure SetGlyph(Value: TBitmap);
    procedure SetGlyphCount(Value: Integer);
    procedure SetItemAlignment(Value: TLeftRight);
    procedure SetItems(Value: TcxUserCheckGroupItems);

//    function GetKeysSeparator: string;
    procedure SetKeysSeparator(const AValue: Char);
//    function IsKeysSeparatorStored: Boolean;

    procedure ItemsChanged(Sender: TObject; AItem: TCollectionItem);
  protected
    function CreateItems: TcxButtonGroupItems; override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;
    function HasDisplayValue: Boolean; override;

    procedure CalculateCheckStatesByEditValue(Sender: TObject; const AEditValue: TcxEditValue; var ACheckStates: TArray<Boolean>); virtual;
    procedure CalculateEditValueByCheckStates(Sender: TObject; const ACheckStates: TArray<Boolean>; out AEditValue: TcxEditValue); virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    function CompareDisplayValues(const AEditValue1, AEditValue2: TcxEditValue): Boolean; override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetDisplayText(const AEditValue: TcxEditValue; AFullText: Boolean = False; AIsInplace: Boolean = True): WideString; override;
    function GetSpecialFeatures: TcxEditSpecialFeatures; override;
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
    // !!!
//    property AllowGrayed: Boolean read FAllowGrayed write SetAllowGrayed default False;
//    property KeysSeparator: string read GetKeysSeparator write SetKeysSeparator stored IsKeysSeparatorStored;
    property KeysSeparator: Char read FKeysSeparator write SetKeysSeparator default cxCheckGroupKeysSeparator;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property GlyphCount: Integer read FGlyphCount write SetGlyphCount default 6;
    property ItemAlignment: TLeftRight read FItemAlignment write SetItemAlignment default taLeftJustify;
    property Items: TcxUserCheckGroupItems read GetItems write SetItems;
//    property OnEditValueToStates: TcxValueToCheckStatesEvent read FOnEditValueToStates write FOnEditValueToStates;
//    property OnStatesToEditValue: TcxCheckStatesToValueEvent read FOnStatesToEditValue write FOnStatesToEditValue;
  end;

  { TcxUserCheckGroupProperties }

  TcxUserCheckGroupProperties = class(TcxCustomUserCheckGroupProperties)
  published
//    property AllowGrayed;
    property AssignedValues;
    property ClearKey;
    property Columns;
    property KeysSeparator;
    property Glyph;
    property GlyphCount;
    property ImmediatePost;
    property ItemAlignment;
    property Items;
    property ReadOnly;
    property ShowEndEllipsis;
    property WordWrap;
    property OnChange;
    property OnEditValueChanged;
//    property OnEditValueToStates;
//    property OnStatesToEditValue;
  end;

  { TcxUserCheckGroupCheckButtonViewInfo }

  TcxUserCheckGroupCheckButtonViewInfo = class(TcxCustomCheckBoxViewInfo)
  protected
    procedure InternalPaint(ACanvas: TcxCanvas); override;
  end;

  { TcxUserCheckGroupButtonProperties }

  TcxUserCheckGroupButtonProperties = class(TcxCheckBoxProperties)
  protected
    function InternalGetGlyph: TBitmap; override;
    function IsEmbeddedEdit: Boolean; override;
  public
    class function GetViewInfoClass: TcxContainerViewInfoClass; override;
  end;

  { TcxUserCheckGroupButton }

  TcxUserCheckGroupButton = class(TcxCustomCheckBox, IcxContainerInnerControl)
  private
    FColumn: Integer;
    FRow: Integer;
    function GetCheckGroup: TcxCustomUserCheckGroup;
  protected
    function CanAutoSize: Boolean; override;
    function CanFocusOnClick: Boolean; override;
    procedure DoExit; override;
    procedure Initialize; override;
    function IsNativeBackground: Boolean; override;
    function IsNativeStyle: Boolean; override;
    function IsTransparent: Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure WndProc(var Message: TMessage); override;

    // IcxContainerInnerControl
    function GetControl: TWinControl;
    function GetControlContainer: TcxContainer;

    procedure CheckTransparentBorder;
    property CheckGroup: TcxCustomUserCheckGroup read GetCheckGroup;
  public
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  end;

  { TcxCustomUserCheckGroup }

  TcxCustomUserCheckGroup = class(TcxCustomButtonGroup)
  private
    FButtonStatesChanging: Boolean;
    FFocusedItemIndex: Integer;
    FStatesItems: Boolean;
    procedure DoButtonChange(Sender: TObject);
    procedure DoButtonEditing(Sender: TObject; var CanEdit: Boolean);
    procedure DoButtonFocusChanged(Sender: TObject);
    function GetActiveProperties: TcxCustomUserCheckGroupProperties;
    function GetButton(Index: Integer): TcxUserCheckGroupButton;
    function GetProperties: TcxCustomUserCheckGroupProperties;
    function GetState(Index: Integer): TcxCheckBoxState;
    procedure SetProperties(Value: TcxCustomUserCheckGroupProperties);
    procedure SetState(Index: Integer; Value: TcxCheckBoxState);
  protected
    procedure ArrangeButtons; override;
    procedure DoSetFocusWhenActivate; override;
    function GetButtonDC(AButtonIndex: Integer): THandle; override;
    function GetButtonInstance: TWinControl; override;
    procedure Initialize; override;
    procedure InternalSetEditValue(const Value: TcxEditValue; AValidateEditValue: Boolean); override;
    procedure InternalValidateDisplayValue(const ADisplayValue: TcxEditValue); override;
    function IsEditValueStored: Boolean; override;
    procedure ParentBackgroundChanged; override;
    procedure PropertiesChanged(Sender: TObject); override;
    procedure SynchronizeButtonsStyle; override;
    procedure SynchronizeDisplayValue; override;
    procedure SynchronizeModifiedAfterEnter; virtual;
    procedure UpdateButtons; override;
    function WantNavigationKeys: Boolean; override;
    function GetFirstEnabledItemIndex(AStartIndex: Integer): Integer;
    property Buttons[Index: Integer]: TcxUserCheckGroupButton read GetButton;
    property InternalButtons;
    property StatesItems: Boolean read FStatesItems write FStatesItems stored False;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    procedure GetTabOrderList(List: TList); override;
    procedure SetFocus; override;
    property ActiveProperties: TcxCustomUserCheckGroupProperties read GetActiveProperties;
    property Properties: TcxCustomUserCheckGroupProperties read GetProperties write SetProperties;
    property States[Index: Integer]: TcxCheckBoxState read GetState write SetState; default;
    property Transparent;
  end;

  { TcxUserCheckGroup }

  TcxUserCheckGroup = class(TcxCustomUserCheckGroup)
  private
    function GetActiveProperties: TcxUserCheckGroupProperties;
    function GetProperties: TcxUserCheckGroupProperties;
    procedure SetProperties(Value: TcxUserCheckGroupProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxUserCheckGroupProperties read GetActiveProperties;
  published
    property Alignment;
    property Anchors;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EditValue;
    property Enabled;
    property ParentBackground;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties: TcxUserCheckGroupProperties read GetProperties write SetProperties;
    property ShowHint;
    property StatesItems;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFocusChanged;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

(*
  { TcxFilterUserCheckGroupHelper }

  TcxFilterUserCheckGroupHelper = class(TcxFilterChecksHelper)
  protected
    class function GetEditValueFormat(AEditProperties: TcxCustomEditProperties): TcxCheckStatesValueFormat; override;
    class function GetItems(AEditProperties: TcxCustomEditProperties): IcxCheckItems; override;
    class procedure InitializeItems(AProperties, AEditProperties: TcxCustomEditProperties); override;
  end;
*)

const
  scxSEditRepositoryUserCheckGroup: String = 'UserCheckGroup|Represents an User CheckGroup';

implementation

uses
  Variants, Types, SysUtils, StrUtils, cxClasses, cxControls, cxVariants,
  cxExtEditConsts, cxEditUtils, dxThemeManager, cxEditPaintUtils,
  SBaseStringFunctions;


{ TcxUserCheckGroupItems }

function TcxUserCheckGroupItems.Add: TcxUserCheckGroupItem;
begin
  Result := TcxUserCheckGroupItem(inherited Add);
end;

function TcxUserCheckGroupItems.GetItem(Index: Integer): TcxUserCheckGroupItem;
begin
  Result := TcxUserCheckGroupItem(inherited Items[Index]);
end;

procedure TcxUserCheckGroupItems.SetItem(Index: Integer; Value: TcxUserCheckGroupItem);
begin
  inherited Items[Index] := Value;
end;

{ TcxUserCheckGroupButtonViewInfo }

function TcxUserCheckGroupButtonViewInfo.GetGlyphRect(const AGlyphSize: TSize; AAlignment: TLeftRight; AIsPaintCopy: Boolean): TRect;

  procedure CorrectCheckRect(var ACheckRect: TRect);
  begin
    if AIsPaintCopy then
      if AAlignment = taRightJustify then
        OffsetRect(ACheckRect, 2, 0)
      else
        OffsetRect(ACheckRect, -2, 0);
  end;

begin
  case AAlignment of
    taLeftJustify:
      AAlignment := taRightJustify;
    taRightJustify:
      AAlignment := taLeftJustify;
  end;
  Result := inherited GetGlyphRect(AGlyphSize, AAlignment, AIsPaintCopy);
  CorrectCheckRect(Result);
end;

{ TcxUserCheckGroupViewInfo }

procedure TcxUserCheckGroupViewInfo.DrawButtonCaption(ACanvas: TcxCanvas;
  AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect);

  function GetCaptionRect(const ACheckRect: TRect): TRect;
  begin
    Result := AButtonViewInfo.Bounds;
    if Alignment = taLeftJustify then
    begin
      Result.Left := ACheckRect.Right + CaptionExtent.Left;
      Dec(Result.Right, CaptionExtent.Right);
    end
    else
    begin
      Result.Right := ACheckRect.Left - CaptionExtent.Right;
      Inc(Result.Left, CaptionExtent.Left);
    end;
  end;

begin
  DrawCheckBoxText(ACanvas, AButtonViewInfo.Caption, Font, TextColor,
    GetCaptionRect(AGlyphRect), DrawTextFlags, Enabled);
end;

procedure TcxUserCheckGroupViewInfo.DrawButtonGlyph(ACanvas: TcxCanvas;
  AButtonViewInfo: TcxGroupBoxButtonViewInfo; const AGlyphRect: TRect);
const
  ACheckStateByButtonState: array[TcxEditButtonState] of TcxEditCheckState =
    (ecsDisabled, ecsNormal, ecsPressed, ecsHot);

  function GetSkinPainter: TcxCustomLookAndFeelPainter;
  begin
    if UseSkins then
      Result := Painter
    else
      Result := nil;
  end;

  function CanDrawBackground: Boolean;
  begin
    Result := not IsBackgroundTransparent and (IsInplace or (GetSkinPainter = nil));
  end;

var
  ACheckBorderStyle: TcxEditBorderStyle;
  AEditProperties: TcxCustomUserCheckGroupProperties;
  LcxCheckBoxState: TcxCheckBoxState;
begin
  if (CheckBorderStyle = ebsFlat) and (AButtonViewInfo.Data.State = ebsSelected) then
    ACheckBorderStyle := ebs3D
  else
    ACheckBorderStyle := CheckBorderStyle;
  AEditProperties := TcxCustomUserCheckGroupProperties(EditProperties);

  if TcxUserCheckGroupButtonViewInfo(AButtonViewInfo).Checked then
    LcxCheckBoxState := cbsChecked
  else
    LcxCheckBoxState := cbsUnChecked;

  DrawEditCheck(ACanvas, AGlyphRect, LcxCheckBoxState,
    ACheckStateByButtonState[AButtonViewInfo.Data.State], AEditProperties.Glyph,
    AEditProperties.GlyphCount, ACheckBorderStyle, AButtonViewInfo.Data.NativeStyle,
    clBtnText, BackgroundColor, CanDrawBackground, IsDesigning, False, True,
    GetSkinPainter);
end;

function TcxUserCheckGroupViewInfo.GetButtonViewInfoClass: TcxEditButtonViewInfoClass;
begin
  Result := TcxUserCheckGroupButtonViewInfo;
end;

function TcxUserCheckGroupViewInfo.IsButtonGlypthTransparent(AButtonViewInfo: TcxGroupBoxButtonViewInfo): Boolean;
begin
  Result := IsBackgroundTransparent;
end;

procedure TcxUserCheckGroupViewInfo.SetOnDrawBackground(AValue: TcxEditDrawBackgroundEvent);
var
  I: Integer;
begin
  inherited SetOnDrawBackground(AValue);
  if Edit <> nil then
    for I := 0 to Edit.InternalButtons.Count - 1 do
      TcxUserCheckGroupButton(Edit.InternalButtons[I]).ViewInfo.OnDrawBackground := AValue;
end;

function TcxUserCheckGroupViewInfo.GetEdit: TcxCustomUserCheckGroup;
begin
  Result := TcxCustomUserCheckGroup(FEdit);
end;

{ TcxUserCheckGroupViewData }

procedure TcxUserCheckGroupViewData.Calculate(ACanvas: TcxCanvas;
  const ABounds: TRect; const P: TPoint; Button: TcxMouseButton;
  Shift: TShiftState; AViewInfo: TcxCustomEditViewInfo; AIsMouseEvent: Boolean);
const
  ABorderStyle: array[TcxLookAndFeelKind] of TcxEditCheckBoxBorderStyle =
    (ebsFlat, ebs3D, ebsUltraFlat, ebsOffice11);

  function GetSkinPainter: TcxCustomLookAndFeelPainter;
  begin
    if AViewInfo.UseSkins then
      Result := AViewInfo.Painter
    else
      Result := nil;
  end;

begin
  inherited Calculate(ACanvas, ABounds, P, Button, Shift, AViewInfo,
    AIsMouseEvent);
  with TcxCustomUserCheckGroupProperties(Properties) do
  begin
    TcxUserCheckGroupViewInfo(AViewInfo).GlyphSize :=
      GetEditCheckSize(ACanvas, IsButtonNativeStyle(Style.LookAndFeel), Glyph,
      GlyphCount, GetSkinPainter);
    TcxUserCheckGroupViewInfo(AViewInfo).Alignment := ItemAlignment;
  end;
  TcxUserCheckGroupViewInfo(AViewInfo).CheckBorderStyle :=
    ABorderStyle[Style.LookAndFeel.Kind];
  AViewInfo.BackgroundColor := Style.Color;
end;

procedure TcxUserCheckGroupViewData.EditValueToDrawValue(const AEditValue: TcxEditValue; AViewInfo: TcxCustomEditViewInfo);
var
  ASender: TObject;
  I: Integer;
  LCheckStates: TArray<Boolean>;
begin
  with TcxCustomUserCheckGroupProperties(Properties) do
  begin
    if IsInplace then
      ASender := nil
    else
      ASender := Edit;
    TcxUserCheckGroupViewInfo(AViewInfo).SetButtonCount(Items.Count);
    if PreviewMode then
    begin
      if Items.Count > 0 then
        TcxUserCheckGroupButtonViewInfo(AViewInfo.ButtonsInfo[0]).Checked := True
    end
    else
    begin
      CalculateCheckStatesByEditValue(ASender, AEditValue, LCheckStates);
      for I := 0 to Items.Count - 1 do
        TcxUserCheckGroupButtonViewInfo(AViewInfo.ButtonsInfo[I]).Checked := LCheckStates[I];
    end;
  end;
  if epoAutoHeight in PaintOptions then
    Include(AViewInfo.PaintOptions, epoAutoHeight);
end;

function TcxUserCheckGroupViewData.GetCaptionRectExtent: TRect;
begin
  Result.Top := 0;
  Result.Bottom := 0;
  if TcxCustomUserCheckGroupProperties(Properties).ItemAlignment = taLeftJustify then
  begin
    Result.Left := 3;
    Result.Right := 2;
  end
  else
  begin
    Result.Right := 2;
    Result.Left := 2;
  end;
end;

procedure TcxUserCheckGroupViewData.GetEditMetrics(AAutoHeight: Boolean;
  ACanvas: TcxCanvas; out AMetrics: TcxEditMetrics);
begin
  AMetrics.ClientLeftBoundCorrection := 6 - (5 +
    Integer(TcxCustomUserCheckGroupProperties(Properties).ItemAlignment =
    taRightJustify)) * Integer(IsInplace);
  AMetrics.ClientWidthCorrection := 5 * Integer(IsInplace) - 6;
  AMetrics.ColumnOffset := 6;
  if ACanvas = nil then
    Exit;

  AMetrics.ButtonSize := GetEditCheckSize(ACanvas,
    IsButtonNativeStyle(Style.LookAndFeel),
    TcxCustomUserCheckGroupProperties(Properties).FGlyph,
    TcxCustomUserCheckGroupProperties(Properties).GlyphCount,
    Style.LookAndFeel.Painter);
  with GetCaptionRectExtent do
    AMetrics.AutoHeightColumnWidthCorrection := Left + Right;
  AMetrics.ColumnWidthCorrection := AMetrics.AutoHeightColumnWidthCorrection;
  AMetrics.WidthCorrection := 6 - 5 * Integer(IsInplace);
  AMetrics.AutoHeightWidthCorrection := AMetrics.ClientWidthCorrection;
end;

{ TcxCustomUserCheckGroupProperties }

constructor TcxCustomUserCheckGroupProperties.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FKeysSeparator := cxCheckGroupKeysSeparator;
  FItemAlignment := taLeftJustify;
  FGlyphCount := 6;

  Items.OnChange := ItemsChanged;
end;

destructor TcxCustomUserCheckGroupProperties.Destroy;
begin
  if FGlyph <> nil then
    FreeAndNil(FGlyph);
  inherited Destroy;
end;

function TcxCustomUserCheckGroupProperties.CompareDisplayValues(
  const AEditValue1, AEditValue2: TcxEditValue): Boolean;
var
  ACheckStates1, ACheckStates2: TArray<Boolean>;
begin
  CalculateCheckStatesByEditValue(nil, AEditValue1, ACheckStates1);
  CalculateCheckStatesByEditValue(nil, AEditValue2, ACheckStates2);
  Result := (Length(ACheckStates1) = Length(ACheckStates2)) and
    CompareMem(@ACheckStates1[0], @ACheckStates2[0], SizeOf(TcxCheckBoxState) * Length(ACheckStates1));
end;

class function TcxCustomUserCheckGroupProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxUserCheckGroup;
end;

function TcxCustomUserCheckGroupProperties.GetDisplayText(const AEditValue: TcxEditValue;
  AFullText: Boolean = False; AIsInplace: Boolean = True): WideString;
var
  ACheckStateNames: array[Boolean] of WideString;

  procedure InitCheckStateNames;
  begin
    ACheckStateNames[False] := cxGetResourceString(@cxSEditCheckGroupUnchecked);
    ACheckStateNames[True] := cxGetResourceString(@cxSEditCheckGroupChecked);
//    ACheckStateNames[cbsGrayed] := cxGetResourceString(@cxSEditCheckGroupGrayed);
  end;

var
  I: Integer;
  ACheckStates: TArray<Boolean>;
begin
  CalculateCheckStatesByEditValue(nil, AEditValue, ACheckStates);
  InitCheckStateNames;

  Result := '';
  for I := 0 to Items.Count - 1 do
  begin
    Result := Result + Items[I].Caption +
      cxGetResourceString(@cxSEditCheckGroupCaptionStateDelimiter) +
      ACheckStateNames[ACheckStates[I]];
    if I < Items.Count - 1 then
      Result := Result + cxGetResourceString(@cxSEditCheckGroupFilterColumnSeparator);
  end;
end;

function TcxCustomUserCheckGroupProperties.GetSpecialFeatures: TcxEditSpecialFeatures;
begin
  Result := inherited GetSpecialFeatures + [esfMultiRow];
end;

class function TcxCustomUserCheckGroupProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxUserCheckGroupViewInfo;
end;

function TcxCustomUserCheckGroupProperties.CreateItems: TcxButtonGroupItems;
begin
  Result := TcxUserCheckGroupItems.Create(Self, TcxUserCheckGroupItem);
end;

procedure TcxCustomUserCheckGroupProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  inherited;
  if AProperties is TcxCustomUserCheckGroupProperties then
    with TcxCustomUserCheckGroupProperties(AProperties) do
    begin
//      Self.AllowGrayed := AllowGrayed;
      Self.ItemAlignment := ItemAlignment;
      Self.WordWrap := WordWrap;
      Self.Glyph := Glyph;
      Self.GlyphCount := GlyphCount;
      Self.KeysSeparator := KeysSeparator;
//      Self.OnEditValueToStates := OnEditValueToStates;
//      Self.OnStatesToEditValue := OnStatesToEditValue;
    end;
end;

class function TcxCustomUserCheckGroupProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxUserCheckGroupViewData;
end;

function TcxCustomUserCheckGroupProperties.HasDisplayValue: Boolean;
begin
  Result := True;
end;

procedure TcxCustomUserCheckGroupProperties.CalculateCheckStatesByEditValue(Sender: TObject; const AEditValue: TcxEditValue; var ACheckStates: TArray<Boolean>);
var
  I: Integer;
  LEditValue: String;
  LInArray: Boolean;
begin
  SetLength(ACheckStates, Items.Count);

//  if Assigned(OnEditValueToStates) then
//  begin
//    OnEditValueToStates(Sender, AEditValue, ACheckStates)
//  end else begin
    LEditValue := VarToStr(AEditValue);
    for I := 0 to Items.Count - 1 do begin
      if FKeysSeparator = #0 then
        LInArray := Pos(VarToStr(Items[I].Value), LEditValue) > 0
      else
        LInArray := InArray(LEditValue, VarToStr(Items[I].Value), FKeysSeparator);

      ACheckStates[I] := LInArray;
{
      if LInArray then
        ACheckStates[I] := True
      else
        ACheckStates[I] := False
}
    end;
//  end;
end;

procedure TcxCustomUserCheckGroupProperties.CalculateEditValueByCheckStates(Sender: TObject; const ACheckStates: TArray<Boolean>; out AEditValue: TcxEditValue);
var
  LResult: String;
  I: Integer;
begin
//  if Assigned(OnStatesToEditValue) then
//    OnStatesToEditValue(Sender, ACheckStates, Result)
//  else begin
    LResult := '';

    for I := 0 to Items.Count - 1 do
    begin
      if not ACheckStates[I] then
        Continue;

      if (FKeysSeparator <> #0) and (not LResult.IsEmpty) then
        LResult := LResult + FKeysSeparator;

      LResult := LResult + VarToStr(Items[I].Value);
    end;

    if LResult.IsEmpty then
      AEditValue := Null
    else
      AEditValue := LResult;
//  end;
end;

function TcxCustomUserCheckGroupProperties.GetGlyph: TBitmap;
begin
  if FGlyph = nil then begin
    FGlyph := TBitmap.Create;
    FGlyph.OnChange := GlyphChanged;
  end;
  Result := FGlyph;
end;

function TcxCustomUserCheckGroupProperties.GetItems: TcxUserCheckGroupItems;
begin
  Result := TcxUserCheckGroupItems(inherited Items);
end;

{
function TcxCustomUserCheckGroupProperties.GetKeysSeparator: string;
begin
  if FKeysSeparator = #0 then
    Result := ''
  else
    Result := String(FKeysSeparator);
end;
}

procedure TcxCustomUserCheckGroupProperties.SetKeysSeparator(const AValue: Char);
begin
  if FKeysSeparator <> AValue then begin
    FKeysSeparator := AValue;
    Changed;
  end;
end;

{
function TcxCustomUserCheckGroupProperties.IsKeysSeparatorStored: Boolean;
begin
  Result := FKeysSeparator <> cxCheckGroupKeysSeparator;
end;
}

procedure TcxCustomUserCheckGroupProperties.GlyphChanged(Sender: TObject);
begin
  Changed;
end;

{
procedure TcxCustomUserCheckGroupProperties.SetAllowGrayed(Value: Boolean);
begin
  if Value <> FAllowGrayed then
  begin
    FAllowGrayed := Value;
    Changed;
  end;
end;
}

procedure TcxCustomUserCheckGroupProperties.SetGlyph(Value: TBitmap);
begin
  if Value = nil then
    FreeAndNil(FGlyph)
  else
    Glyph.Assign(Value);
  Changed;
end;

procedure TcxCustomUserCheckGroupProperties.SetGlyphCount(Value: Integer);
begin
  if FGlyphCount <> Value then
  begin
    FGlyphCount := Value;
    if FGlyph <> nil then
      Changed;
  end;
end;

procedure TcxCustomUserCheckGroupProperties.SetItemAlignment(Value: TLeftRight);
begin
  if Value <> FItemAlignment then
  begin
    FItemAlignment := Value;
    Changed;
  end;
end;

procedure TcxCustomUserCheckGroupProperties.SetItems(Value: TcxUserCheckGroupItems);
begin
  inherited Items.Assign(Value);
end;

procedure TcxCustomUserCheckGroupProperties.ItemsChanged(Sender: TObject; AItem: TCollectionItem);
begin
//  CheckEditValueFormat;
end;

{ TcxUserCheckGroupCheckButtonViewInfo }

procedure TcxUserCheckGroupCheckButtonViewInfo.InternalPaint(ACanvas: TcxCanvas);
var
  ARect: TRect;
  ABitmap: TBitmap;
  ABitmapCanvas: TcxCanvas;
begin
  if UseSkins then
  begin
    ARect := ClientRect;
    ABitmap := cxCreateBitmap(ARect, pf32bit);
    ABitmapCanvas := TcxCanvas.Create(ABitmap.Canvas);
    try
      Painter.DrawGroupBoxBackground(ABitmapCanvas, ARect,
        Rect(0, 0, ABitmap.Width, ABitmap.Height));
      inherited InternalPaint(ABitmapCanvas);
      ACanvas.Draw(0, 0, ABitmap); 
    finally
      ABitmapCanvas.Free;
      ABitmap.Free;
    end;
  end
  else
    inherited InternalPaint(ACanvas);
end;

{ TcxUserCheckGroupButtonProperties }

function TcxUserCheckGroupButtonProperties.InternalGetGlyph: TBitmap;
begin
  Result :=
   (TcxUserCheckGroupButton(Owner).Owner as TcxCustomUserCheckGroup).ActiveProperties.Glyph;
end;

function TcxUserCheckGroupButtonProperties.IsEmbeddedEdit: Boolean;
begin
  Result := True;
end;

class function TcxUserCheckGroupButtonProperties.GetViewInfoClass: TcxContainerViewInfoClass;
begin
  Result := TcxUserCheckGroupCheckButtonViewInfo;
end;

{ TcxUserCheckGroupButton }

destructor TcxUserCheckGroupButton.Destroy;
begin
  TcxCustomUserCheckGroup(GetOwner).InternalButtons.Remove(Self);
  inherited Destroy;
end;

function TcxUserCheckGroupButton.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
    CheckGroup.DataBinding.ExecuteAction(Action);
end;

class function TcxUserCheckGroupButton.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxUserCheckGroupButtonProperties;
end;

function TcxUserCheckGroupButton.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
    CheckGroup.DataBinding.UpdateAction(Action);
end;

function TcxUserCheckGroupButton.CanAutoSize: Boolean;
begin
  Result := False;
end;

function TcxUserCheckGroupButton.CanFocusOnClick: Boolean;
begin
  Result := inherited CanFocusOnClick and CheckGroup.NeedFocusOnClick;
end;

procedure TcxUserCheckGroupButton.DoExit;
begin
  inherited DoExit;
  CheckGroup.FFocusedItemIndex := CheckGroup.InternalButtons.IndexOf(Self);
end;

procedure TcxUserCheckGroupButton.Initialize;
begin
  inherited Initialize;
  CheckGroup.InternalButtons.Add(Self);
  Style.LookAndFeel.MasterLookAndFeel := CheckGroup.LookAndFeel;
  Keys := Keys + [kArrows];
  CheckTransparentBorder;
end;

function TcxUserCheckGroupButton.IsNativeBackground: Boolean;
begin
  Result := CheckGroup.IsNativeBackground;
end;

function TcxUserCheckGroupButton.IsNativeStyle: Boolean;
begin
  Result := CheckGroup.IsButtonNativeStyle;
end;

function TcxUserCheckGroupButton.IsTransparent: Boolean;
begin
  Result := (Style.LookAndFeel.SkinPainter <> nil) or CheckGroup.IsTransparent;
end;

procedure TcxUserCheckGroupButton.KeyDown(var Key: Word; Shift: TShiftState);

  procedure FocusNearestItem(ADown: Boolean);
  var
    AIndex, I, ADelta: Integer;
  begin
    with CheckGroup do
    begin
      AIndex := InternalButtons.IndexOf(Self);
      ADelta := Integer(ADown) - Integer(not ADown);
      I := AIndex;
      repeat
        I := I + ADelta;
        if I = InternalButtons.Count then
          I := 0
        else
          if I < 0 then
            I := InternalButtons.Count - 1;
        if Buttons[I].Enabled then
          Break;
      until I = AIndex;
      if I <> AIndex then
        Buttons[I].SetFocus;
    end;
  end;
var
  APrevKeyDownEvent: TKeyEvent;
begin
  _TcxContainerAccess.KeyDown(CheckGroup, Key, Shift);
  if Key = 0 then
    Exit;
  if not CheckGroup.IsInplace then
    if Key in [VK_UP, VK_LEFT] then
      FocusNearestItem(False)
    else
      if Key in [VK_DOWN, VK_RIGHT] then
        FocusNearestItem(True);
  APrevKeyDownEvent := OnKeyDown;
  OnKeyDown := nil;
  inherited KeyDown(Key, Shift);
  OnKeyDown := APrevKeyDownEvent;
end;

procedure TcxUserCheckGroupButton.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  CheckTransparentBorder;
end;

procedure TcxUserCheckGroupButton.WndProc(var Message: TMessage);
begin
  with Message do
    if ((Msg = WM_LBUTTONDOWN) or (Msg = WM_LBUTTONDBLCLK)) and
      (CheckGroup.DragMode = dmAutomatic) and not(csDesigning in CheckGroup.ComponentState) then
    begin
      _TcxContainerAccess.BeginAutoDrag(CheckGroup);
      Exit;
    end;
  inherited WndProc(Message);
end;

function TcxUserCheckGroupButton.GetControl: TWinControl;
begin
  Result := Self;
end;

function TcxUserCheckGroupButton.GetControlContainer: TcxContainer;
begin
  Result := CheckGroup;
end;

procedure TcxUserCheckGroupButton.CheckTransparentBorder;
begin
  if not CheckGroup.IsInplace then
    Style.TransparentBorder := IsNativeStyle;
end;

function TcxUserCheckGroupButton.GetCheckGroup: TcxCustomUserCheckGroup;
begin
  Result := TcxCustomUserCheckGroup(Owner);
end;

{ TcxCustomUserCheckGroup }

class function TcxCustomUserCheckGroup.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomUserCheckGroupProperties;
end;

procedure TcxCustomUserCheckGroup.GetTabOrderList(List: TList);
var
  AFocusedItemIndex: Integer;
begin
  inherited GetTabOrderList(List);
  List.Remove(Self);
  if Focused then
    AFocusedItemIndex := GetFocusedButtonIndex
  else
    AFocusedItemIndex := GetFirstEnabledItemIndex(FFocusedItemIndex);
  if (AFocusedItemIndex <> -1) and TabStop then
    List.Add(InternalButtons[AFocusedItemIndex]);
end;

procedure TcxCustomUserCheckGroup.SetFocus;
begin
  inherited SetFocus;
  FFocusedItemIndex := GetFirstEnabledItemIndex(FFocusedItemIndex);
  if FFocusedItemIndex <> -1 then
    TWinControl(InternalButtons[FFocusedItemIndex]).SetFocus;
end;

procedure TcxCustomUserCheckGroup.ArrangeButtons;
var
  AButtonViewInfo: TcxGroupBoxButtonViewInfo;
  I: Integer;
begin
  inherited ArrangeButtons;
  for I := 0 to InternalButtons.Count - 1 do
    with Buttons[I] do
    begin
      AButtonViewInfo := TcxGroupBoxButtonViewInfo(Self.ViewInfo.ButtonsInfo[I]);
      FColumn := AButtonViewInfo.Column;
      FRow := AButtonViewInfo.Row;
    end;
end;

procedure TcxCustomUserCheckGroup.DoSetFocusWhenActivate;
begin
  FFocusedItemIndex := 0;
  inherited;
end;

function TcxCustomUserCheckGroup.GetButtonDC(AButtonIndex: Integer): THandle;
begin
  Result := Buttons[AButtonIndex].Canvas.Handle;
end;

function TcxCustomUserCheckGroup.GetButtonInstance: TWinControl;
begin
  Result := TcxUserCheckGroupButton.Create(Self, IsInplace);
end;

procedure TcxCustomUserCheckGroup.Initialize;
begin
  inherited Initialize;
  ControlStyle := ControlStyle - [csAcceptsControls];
  FFocusedItemIndex := -1;
  InternalEditValue := 0;
end;

procedure TcxCustomUserCheckGroup.InternalSetEditValue(const Value: TcxEditValue; AValidateEditValue: Boolean);
begin
  inherited InternalSetEditValue(Value, AValidateEditValue);
  SynchronizeModifiedAfterEnter;
end;

procedure TcxCustomUserCheckGroup.InternalValidateDisplayValue(const ADisplayValue: TcxEditValue);
var
  APrevEditValue: TcxEditValue;
  I: Integer;
  ACheckStates: TArray<Boolean>;
begin
  APrevEditValue := FEditValue;
  SetLength(ACheckStates, InternalButtons.Count);
  for I := 0 to Length(ACheckStates) - 1 do
    ACheckStates[I] := (Buttons[I].State = cbsChecked);
  with ActiveProperties do
    CalculateEditValueByCheckStates(GetStandaloneEventSender(Self), ACheckStates, FEditValue);
  if not InternalVarEqualsExact(APrevEditValue, FEditValue) then
    DoEditValueChanged;
end;

function TcxCustomUserCheckGroup.IsEditValueStored: Boolean;
begin
  Result := VarToStr(EditValue) <> '';
end;

procedure TcxCustomUserCheckGroup.ParentBackgroundChanged;
var
  I: Integer;
begin
  for I := 0 to InternalButtons.Count - 1 do
    Buttons[I].ParentBackground := ParentBackground;
end;

procedure TcxCustomUserCheckGroup.PropertiesChanged(Sender: TObject);
begin
  inherited PropertiesChanged(Sender);
  InternalValidateDisplayValue(Null);
  DataBinding.UpdateDisplayValue;
end;

procedure TcxCustomUserCheckGroup.SynchronizeButtonsStyle;
var
  AButton: TcxUserCheckGroupButton;
  I: Integer;
begin
  inherited SynchronizeButtonsStyle;
  for I := 0 to InternalButtons.Count - 1 do
  begin
    AButton := Buttons[I];
    AButton.Style.Assign(Style);
    AButton.StyleDisabled.Assign(StyleDisabled);
    AButton.StyleFocused.Assign(StyleFocused);
    AButton.StyleHot.Assign(StyleHot);
    AButton.CheckTransparentBorder;
    AButton.Transparent := Transparent; // to repaint button
    AButton.ParentFont := True;
  end;
end;

procedure TcxCustomUserCheckGroup.SynchronizeDisplayValue;
var
  I: Integer;
  ACheckStates: TArray<Boolean>;
begin
  if ActiveProperties.Items.ItemChanged then
    Exit;
  with ActiveProperties do
    CalculateCheckStatesByEditValue(GetStandaloneEventSender(Self), EditValue, ACheckStates);
{
  if not ActiveProperties.AllowGrayed and Focused then
    for I := 0 to Length(ACheckStates) - 1 do
      if ACheckStates[I] = cbsGrayed then
        ACheckStates[I] := cbsUnchecked;
}

  FButtonStatesChanging := True;
  try
    for I := 0 to InternalButtons.Count - 1 do
      with Buttons[I] do
        if ACheckStates[I] then
          State := cbsChecked
        else
          State := cbsUnChecked;
  finally
    FButtonStatesChanging := False;
  end;
end;

procedure TcxCustomUserCheckGroup.SynchronizeModifiedAfterEnter;

    function NeedSynchronize: Boolean;
    begin
      Result := not ModifiedAfterEnter or ActiveProperties.ImmediatePost;
    end;

var
  I: Integer;
begin
  if NeedSynchronize then
    for I := 0 to InternalButtons.Count - 1 do
      Buttons[I].ModifiedAfterEnter := False;
end;

procedure TcxCustomUserCheckGroup.UpdateButtons;
const
  ALookAndFeelKindMap: array [TcxEditButtonStyle] of TcxLookAndFeelKind =
    (lfStandard, lfStandard, lfFlat, lfStandard, lfStandard,
    lfUltraFlat, lfOffice11);
var
 AButton: TcxUserCheckGroupButton;
 I: Integer;
begin
  inherited UpdateButtons;
  if FFocusedItemIndex >= InternalButtons.Count then
    FFocusedItemIndex := -1;
  for I := 0 to InternalButtons.Count - 1 do
  begin
    AButton := Buttons[I];
    AButton.Properties.BeginUpdate;
    try
      AButton.Properties.Alignment := ActiveProperties.ItemAlignment;
      AButton.Properties.AllowGrayed := False; //ActiveProperties.AllowGrayed;
      AButton.Caption := ActiveProperties.Items[I].Caption;
      AButton.Properties.ShowEndEllipsis := ActiveProperties.ShowEndEllipsis;
      AButton.Properties.MultiLine := ActiveProperties.WordWrap;
      AButton.Properties.GlyphCount := ActiveProperties.GlyphCount;
      AButton.Properties.ReadOnly := ActiveProperties.ReadOnly and
        DataBinding.IsDataAvailable;
      if Length(ViewInfo.ButtonsInfo) > 0 then
        AButton.LookAndFeel.Kind := ALookAndFeelKindMap[ViewInfo.ButtonsInfo[0].Data.Style];
    finally
      AButton.Properties.EndUpdate;
      AButton.OnEditing := DoButtonEditing;
      AButton.OnFocusChanged := DoButtonFocusChanged;
      AButton.Properties.OnChange := DoButtonChange;
    end;
  end;
//  SynchronizeDisplayValue;
end;

function TcxCustomUserCheckGroup.WantNavigationKeys: Boolean;
begin
  Result := not IsInplace;
end;

function TcxCustomUserCheckGroup.GetFirstEnabledItemIndex(AStartIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  if AStartIndex = -1 then
    AStartIndex := 0;
  for I := 0 to InternalButtons.Count - 1 do
    if TWinControl(InternalButtons[(I + AStartIndex) mod InternalButtons.Count]).CanFocus then
    begin
      Result := (I + AStartIndex) mod InternalButtons.Count;
      Break;
    end;
end;

procedure TcxCustomUserCheckGroup.DoButtonChange(Sender: TObject);
begin
  if FButtonStatesChanging then
    Exit;
  LockChangeEvents(True);
  try
    InternalValidateDisplayValue(Null);
    ChangeHandler(Self);
    if ActiveProperties.ImmediatePost and CanPostEditValue and InternalValidateEdit then
    begin
      InternalPostEditValue;
      SynchronizeModifiedAfterEnter;      
    end;
  finally
    LockChangeEvents(False);
  end;
end;

procedure TcxCustomUserCheckGroup.DoButtonEditing(Sender: TObject; var CanEdit: Boolean);
begin
  CanEdit := DoEditing;
end;

procedure TcxCustomUserCheckGroup.DoButtonFocusChanged(Sender: TObject);
begin
  FocusChanged;
end;

function TcxCustomUserCheckGroup.GetActiveProperties: TcxCustomUserCheckGroupProperties;
begin
  Result := TcxCustomUserCheckGroupProperties(InternalGetActiveProperties);
end;

function TcxCustomUserCheckGroup.GetButton(Index: Integer): TcxUserCheckGroupButton;
begin
  Result := TcxUserCheckGroupButton(InternalButtons[Index]);
end;

function TcxCustomUserCheckGroup.GetProperties: TcxCustomUserCheckGroupProperties;
begin
  Result := TcxCustomUserCheckGroupProperties(FProperties);
end;

function TcxCustomUserCheckGroup.GetState(Index: Integer): TcxCheckBoxState;
begin
  Result := Buttons[Index].State;
end;

procedure TcxCustomUserCheckGroup.SetProperties(Value: TcxCustomUserCheckGroupProperties);
begin
  FProperties.Assign(Value);
end;

procedure TcxCustomUserCheckGroup.SetState(Index: Integer; Value: TcxCheckBoxState);
begin
  Buttons[Index].State := Value;
end;

{ TcxUserCheckGroup }

class function TcxUserCheckGroup.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxUserCheckGroupProperties;
end;

function TcxUserCheckGroup.GetActiveProperties: TcxUserCheckGroupProperties;
begin
  Result := TcxUserCheckGroupProperties(InternalGetActiveProperties);
end;

function TcxUserCheckGroup.GetProperties: TcxUserCheckGroupProperties;
begin
  Result := TcxUserCheckGroupProperties(FProperties);
end;

procedure TcxUserCheckGroup.SetProperties(Value: TcxUserCheckGroupProperties);
begin
  FProperties.Assign(Value);
end;

(*
{ TcxFilterUserCheckGroupHelper }

class function TcxFilterUserCheckGroupHelper.GetEditValueFormat(AEditProperties: TcxCustomEditProperties): TcxCheckStatesValueFormat;
begin
  Result := TcxCustomUserCheckGroupProperties(AEditProperties).EditValueFormat;
end;

class function TcxFilterUserCheckGroupHelper.GetItems(AEditProperties: TcxCustomEditProperties): IcxCheckItems;
begin
  Result := TcxCustomUserCheckGroupProperties(AEditProperties).Items;
end;

class procedure TcxFilterUserCheckGroupHelper.InitializeItems(AProperties, AEditProperties: TcxCustomEditProperties);
begin
  inherited InitializeItems(AProperties, AEditProperties);
  TcxCustomCheckComboBoxProperties(AProperties).Glyph := TcxCustomUserCheckGroupProperties(AEditProperties).Glyph;
  TcxCustomCheckComboBoxProperties(AProperties).GlyphCount := TcxCustomUserCheckGroupProperties(AEditProperties).GlyphCount;
end;
*)

{ TcxUserCheckGroupItem }

procedure TcxUserCheckGroupItem.Assign(Source: TPersistent);
begin
  if Source is TcxUserCheckGroupItem then
    Self.Value := TcxUserCheckGroupItem(Source).Value;
  inherited Assign(Source);
end;

initialization
  GetRegisteredEditProperties.Register(TcxUserCheckGroupProperties, scxSEditRepositoryUserCheckGroup);
//  FilterEditsController.Register(TcxUserCheckGroupProperties, TcxFilterUserCheckGroupHelper);

finalization
//  FilterEditsController.Unregister(TcxUserCheckGroupProperties, TcxFilterUserCheckGroupHelper);
  GetRegisteredEditProperties.Unregister(TcxUserCheckGroupProperties);

end.
