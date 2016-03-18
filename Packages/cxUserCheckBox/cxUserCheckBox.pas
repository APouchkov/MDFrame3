unit cxUserCheckBox;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI6}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Controls, Graphics, StdCtrls,
  Forms, Math, cxClasses, cxControls, cxContainer, cxGraphics,
  cxDataStorage, cxDataUtils, cxVariants, cxEdit, cxEditUtils,
  cxTextEdit, cxMaskEdit, cxLookAndFeels, cxLookAndFeelPainters, DB, cxDBEdit,
  cxCheckBox, dxCoreClasses;

type
  TcxUserCheckBoxStyle = class(TcxCheckBoxStyle)
  private
    FIsTextColorStored: Boolean;
    function IsTextColorStored: Boolean;
    procedure SetTextColor(const Value: TColor);

    function GetAssignedValues: TcxEditStyleValues;
    procedure SetAssignedValues(const Value: TcxEditStyleValues);
  protected
    function GetTextColor: TColor; override;
    function DefaultTextColor: TColor; override;
  public
    constructor Create(AOwner: TPersistent; ADirectAccessMode: Boolean; AParentStyle: TcxContainerStyle = nil; AState: TcxContainerStateItem = csNormal); override;
  published
    property TextColor: TColor read GetTextColor write SetTextColor stored IsTextColorStored;
    property AssignedValues: TcxEditStyleValues read GetAssignedValues write SetAssignedValues stored False;
  end;

  TcxCustomUserCheckBoxProperties = class(TcxCustomCheckBoxProperties)
  private
    class var FDefaultGlyph: TBitMap;
  public
    class procedure LoadDefaultGlyph(const APicture: TPicture);
    class procedure UnLoadDefaultGlyph;
  public
    class function GetContainerClass: TcxContainerClass; override;
    class function GetStyleClass: TcxCustomEditStyleClass; override;

    constructor Create(AOwner: TPersistent); override;
  end;

  { TcxUserCheckBoxProperties }

  TcxUserCheckBoxProperties = class(TcxCustomUserCheckBoxProperties)
  published
    property Alignment;
    property AllowGrayed;
    property AssignedValues;
    property Caption; // obsolete
    property ClearKey;
    property DisplayChecked;
    property DisplayUnchecked;
    property DisplayGrayed;
    property FullFocusRect;
//    property Glyph;
//    property GlyphCount;
    property ImmediatePost;
    property MultiLine;
    property NullStyle;
    property ReadOnly;
    property ShowEndEllipsis;
    property UseAlignmentWhenInplace;
    property ValueChecked;
    property ValueGrayed;
    property ValueUnchecked;
    property OnChange;
    property OnEditValueChanged;
    property OnValidate;
  end;

  { TcxCustomUserCheckBox }

  TcxCustomUserCheckBox = class(TcxCustomCheckBox)
  private
    FGroupIndex: Integer;

    function GetStyle: TcxUserCheckBoxStyle;
    procedure SetStyle(Value: TcxUserCheckBoxStyle);

    function GetProperties: TcxUserCheckBoxProperties;
    procedure SetProperties(Value: TcxUserCheckBoxProperties);
    function GetActiveProperties: TcxUserCheckBoxProperties;
    function GetStyleDisabled: TcxUserCheckBoxStyle;
    procedure SetStyleDisabled(const Value: TcxUserCheckBoxStyle);
  protected
    procedure SetState(Value: TcxCheckBoxState); override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxUserCheckBoxProperties read GetActiveProperties;
    function IsRepositoryItemAcceptable(ARepositoryItem: TcxEditRepositoryItem): Boolean; override;
  published
    property Properties: TcxUserCheckBoxProperties read GetProperties write SetProperties;
    property Style: TcxUserCheckBoxStyle read GetStyle write SetStyle;
    property StyleDisabled: TcxUserCheckBoxStyle read GetStyleDisabled write SetStyleDisabled;

    property GroupIndex: Integer read FGroupIndex write FGroupIndex default 0;
  end;

  TcxUserCheckBox = class(TcxCustomUserCheckBox)
  published
    property Action;
    property Anchors;
    property AutoSize;
    property Caption;
    property Checked;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBackground;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties;
    property ShowHint;
    property State;
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

  TcxDBUserCheckBox = class(TcxCustomUserCheckBox)
  private
    function GetDataBinding: TcxDBEditDataBinding;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    property Checked;
  published
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;

    property Action;
    property Anchors;
    property AutoSize;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBackground;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
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
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

implementation

uses
  ActnList, dxThemeManager, SBaseStringFunctions;

const
  scxSEditRepositoryUserCheckBox: String = 'UserCheckBox|Represents an User predefined CheckBox edit';

type
  TcxCustomEditStyleCrack = class(TcxCustomEditStyle);
  TcxEditDataBindingCrack = class(TcxEditDataBinding);

{ TcxCustomUserCheckBoxProperties }

constructor TcxCustomUserCheckBoxProperties.Create(AOwner: TPersistent);
begin
  inherited;
  if FDefaultGlyph <> nil then begin
    if FDefaultGlyph.Width >= FDefaultGlyph.Height then
      GlyphCount := FDefaultGlyph.Width div FDefaultGlyph.Height
    else
      GlyphCount := FDefaultGlyph.Height div FDefaultGlyph.Width;

    Glyph := FDefaultGlyph;
  end;
end;

class function TcxCustomUserCheckBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxCustomUserCheckBox
end;

class function TcxCustomUserCheckBoxProperties.GetStyleClass: TcxCustomEditStyleClass;
begin
  Result := TcxUserCheckBoxStyle
end;

class procedure TcxCustomUserCheckBoxProperties.LoadDefaultGlyph(const APicture: TPicture);
begin
  if Assigned(APicture) then begin
    if not Assigned(FDefaultGlyph) then
      FDefaultGlyph := TBitmap.Create;

    with FDefaultGlyph do begin
      Width   := APicture.Width;
      Height  := APicture.Height;
      Canvas.Draw(0, 0, APicture.Graphic);
    end;
  end;
end;

class procedure TcxCustomUserCheckBoxProperties.UnLoadDefaultGlyph;
begin
  FreeAndNil(FDefaultGlyph)
end;

{ TcxCustomUserCheckBox }

function TcxCustomUserCheckBox.GetActiveProperties: TcxUserCheckBoxProperties;
begin
  Result := TcxUserCheckBoxProperties(InternalGetActiveProperties);
end;

function TcxCustomUserCheckBox.GetProperties: TcxUserCheckBoxProperties;
begin
  Result := TcxUserCheckBoxProperties(FProperties);
end;

class function TcxCustomUserCheckBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxUserCheckBoxProperties;
end;

function TcxCustomUserCheckBox.GetStyle: TcxUserCheckBoxStyle;
begin
  Result := TcxUserCheckBoxStyle(FStyles.Style);
end;

function TcxCustomUserCheckBox.GetStyleDisabled: TcxUserCheckBoxStyle;
begin
  Result := TcxUserCheckBoxStyle(FStyles.StyleDisabled);
end;

function TcxCustomUserCheckBox.IsRepositoryItemAcceptable(ARepositoryItem: TcxEditRepositoryItem): Boolean;
begin
  Result := (inherited IsRepositoryItemAcceptable(ARepositoryItem))
        or
        ARepositoryItem.Properties.InheritsFrom(TcxCustomCheckBoxProperties);
end;

procedure TcxCustomUserCheckBox.SetProperties(Value: TcxUserCheckBoxProperties);
begin
  FProperties.Assign(Value);
end;

procedure TcxCustomUserCheckBox.SetState(Value: TcxCheckBoxState);
  procedure TurnSiblingsOff;
  var
    I: Integer;
    LSibling: TControl;
    LcxUserCheckBox: TcxCustomUserCheckBox absolute LSibling;
    LAction: TCustomAction;
  begin
    if Parent <> nil then
      for I := 0 to Parent.ControlCount - 1 do begin
        LSibling := Parent.Controls[I];
        if (LSibling <> Self) and (LSibling is TcxCustomUserCheckBox) and (LcxUserCheckBox.GroupIndex = GroupIndex) and LcxUserCheckBox.Checked then begin
          if Assigned(LcxUserCheckBox.Action) and (LcxUserCheckBox.Action is TCustomAction) then begin
            LAction := TCustomAction(LcxUserCheckBox.Action);
            if LAction.AutoCheck then
              LAction.Checked := False;
          end;
          //LcxUserCheckBox.Checked := False;
          if TcxEditDataBindingCrack(LcxUserCheckBox.DataBinding).SetEditMode then begin
            LcxUserCheckBox.EditValue := False;
            LcxUserCheckBox.PostEditValue;
          end;

        end;
      end;
  end;

begin
  if State <> Value then begin
    if (FGroupIndex <> 0) and (Value <> cbsUnchecked) then
      TurnSiblingsOff;
    inherited;
  end;
end;

procedure TcxCustomUserCheckBox.SetStyle(Value: TcxUserCheckBoxStyle);
begin
  FStyles.Style := Value;
end;

procedure TcxCustomUserCheckBox.SetStyleDisabled(const Value: TcxUserCheckBoxStyle);
begin
  FStyles.StyleDisabled := Value;
end;

{ TcxDBUserCheckBox }

procedure TcxDBUserCheckBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

function TcxDBUserCheckBox.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

class function TcxDBUserCheckBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

procedure TcxDBUserCheckBox.SetDataBinding(Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

{ TcxUserCheckBoxStyle }

constructor TcxUserCheckBoxStyle.Create(AOwner: TPersistent; ADirectAccessMode: Boolean; AParentStyle: TcxContainerStyle; AState: TcxContainerStateItem);
begin
  inherited Create(AOwner, ADirectAccessMode, AParentStyle, AState);
  if AState = csDisabled then begin
    StyleData.FontColor   := clWindowText;
    Include(FAssignedValues, csvTextColor);
  end;
end;

function TcxUserCheckBoxStyle.DefaultTextColor: TColor;
begin
  if State = csDisabled then
    Result := clWindowText
  else
    if IsBaseStyle then
      Result := StyleData.Font.Color
    else
      Result := ParentStyle.TextColor;
end;

function TcxUserCheckBoxStyle.GetAssignedValues: TcxEditStyleValues;
begin
  Result := (inherited AssignedValues);
  if (State = csDisabled) and (not FIsTextColorStored) then
    Exclude(Result, csvTextColor);
end;

procedure TcxUserCheckBoxStyle.SetAssignedValues(const Value: TcxEditStyleValues);
begin
  if (State = csDisabled) then begin
    inherited AssignedValues := Value + [csvTextColor];
    FIsTextColorStored := (csvTextColor in Value);
  end else
    inherited AssignedValues := Value;
end;

function TcxUserCheckBoxStyle.GetTextColor: TColor;
begin
  Result := (inherited GetTextColor);
end;

function TcxUserCheckBoxStyle.IsTextColorStored: Boolean;
begin
  if (State = csDisabled) then
    Result := FIsTextColorStored
  else
    Result := (csvTextColor in (inherited AssignedValues));
end;

procedure TcxUserCheckBoxStyle.SetTextColor(const Value: TColor);
begin
  inherited TextColor := Value;
  if (State = csDisabled) then
    FIsTextColorStored := True;
end;

initialization
  RegisterClasses([TcxUserCheckBoxProperties]);
  GetRegisteredEditProperties.Register(TcxUserCheckBoxProperties, scxSEditRepositoryUserCheckBox);

finalization
  GetRegisteredEditProperties.Unregister(TcxUserCheckBoxProperties);
  UnRegisterClasses([TcxUserCheckBoxProperties]);

  TcxUserCheckBoxProperties.UnLoadDefaultGlyph;

end.
