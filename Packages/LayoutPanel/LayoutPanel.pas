unit LayoutPanel;

interface

{$I cxVer.inc}

{DEFINE USE_CX_PANEL}

uses
  Windows, Messages, SysUtils, Classes, Controls, Graphics, StdCtrls, ExtCtrls,
  Forms, Math, cxGroupBox, cxSplitter
{$IFDEF USE_CX_PANEL}
  , cxEdit
{$ENDIF}
  ;

type
{$IFDEF USE_CX_PANEL}
  TCustomInternalPanel = class(TcxCustomGroupBox)
{$ELSE}
  TCustomInternalPanel = class(TCustomPanel)
{$ENDIF}
  private
    FAutoSizePercents: Integer;
    procedure SetAutoSizePercents(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
//    procedure RequestAlign; override;

    property OnResize;
    property AutoSizePercents: Integer read FAutoSizePercents write SetAutoSizePercents default -1;
  end;

  TLayoutGroupStyle =
    (
      lgsFormInner, lgsDialogFormInner,
      lgsTabSheetInner, lgsTabSheetCustomInner, lgsLayoutLine,
      lgsCustomLeft, lgsCustomRight, lgsCustomTop, lgsCustomBottom, lgsCustomClient
    );

  TGroupPanel = class(TCustomInternalPanel)
  strict private
    FLoadedStyle: TLayoutGroupStyle;
    FStyle: TLayoutGroupStyle;

    procedure SetLayoutGroupStyle(const Value: TLayoutGroupStyle);
    procedure InternalSetLayoutGroupStyle;
  public
    constructor Create(AOwner: TComponent); override;
  protected
    procedure Loaded; override;
  published
    property AutoSizePercents;

    property Style: TLayoutGroupStyle read FStyle write SetLayoutGroupStyle default lgsLayoutLine;
    property Height Default 23;
//    property AutoSize;

//    property Caption;
//    property Font;
//    property Align;
//    property Anchors;
//    property Constraints;
    property Enabled;
//    property ParentBackground;
//    property ParentColor;
//    property ParentFont default False;
    property ParentShowHint;
//    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

    property OnResize;
  end;

  TcxCustomLayoutGroupBox = class(TcxCustomGroupBox)
  private
    FAutoSizePercents: Integer;

    function GetCaption: TCaption;
    procedure SetCaption(const Value: TCaption);

    procedure SetAutoSizePercents(const Value: Integer);
//    function IsAutoSizePercentsStored: Boolean;

    function GetAlign: TAlign;
    procedure SetAlign(const Value: TAlign);
  public
    constructor Create(AOwner: TComponent); override;

    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property Caption: TCaption read GetCaption write SetCaption;
    property Align: TAlign read GetAlign write SetAlign default alTop;
    property AutoSizePercents: Integer read FAutoSizePercents write SetAutoSizePercents default -1;
  end;

{$IFDEF PACKAGE}
  TcxLayoutGroupBoxPadding = class(TPadding)
  protected
    class procedure InitDefaults(AMargins: TMargins); override;
  published
    property Left   default 5;
    property Top    default 0;
    property Right  default 5;
    property Bottom default 5;
  end;
{$ENDIF}

//  TcxLayoutGroupBoxContainer = (lgbcLayoutPanels, lgbcGrid, lgbcPageControl);

  TcxLayoutGroupBox = class(TcxCustomLayoutGroupBox)
  private
{$IFDEF PACKAGE}
    FPadding: TcxLayoutGroupBoxPadding;
    procedure DoPaddingChange(Sender: TObject);
    procedure SetPadding(const Value: TcxLayoutGroupBoxPadding);
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
{$IFDEF PACKAGE}
    destructor Destroy; override;
{$ENDIF}
    //procedure RequestAlign; override;
  published
{$IFDEF PACKAGE}
    property Padding: TcxLayoutGroupBoxPadding read FPadding write SetPadding;
{$ELSE}
    property Padding;
{$ENDIF}

    property Caption;
    property AutoSizePercents;
//    property AutoSize;
//    property Alignment;
    property Anchors;
    property Constraints;
    property Enabled;
    property PanelStyle;
    property ParentBackground;
    property ParentColor;
//    property ParentFont default False;
//    property ParentShowHint;
//    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;

    property OnResize;
  end;

  TCustomLayoutPanel = class;

  TCustomLayoutPanelLabelProperties = class(TPersistent)
  const
    IDefault_Left  = 5;
    IDefault_Width = 50;
    IDefault_Right = 0;
  private
    FOwner: TCustomLayoutPanel;
    function GetText: String;
    function GetWidth: Integer;
    procedure SetText(const Value: String);
    procedure SetWidth(const Value: Integer);
    //function IsTextStored: Boolean;
    function GetLeft: Integer;
    procedure SetLeft(const Value: Integer);
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);
    function GetAutoWidth: Boolean;
    procedure SetAutoWidth(const AValue: Boolean);
    function IsWidthStored: Boolean;
    function GetRight: Integer;
    procedure SetRight(const Value: Integer);
  public
    constructor Create(AOwner: TCustomLayoutPanel);
  published
    property AutoWidth: Boolean read GetAutoWidth write SetAutoWidth Default False;

    property Text: String read GetText write SetText;
    property Left: Integer read GetLeft write SetLeft Default IDefault_Left;
    property Right: Integer read GetRight write SetRight Default IDefault_Right;
    property Width: Integer read GetWidth write SetWidth Stored IsWidthStored;
    property Font: TFont read GetFont write SetFont;
  end;

{$IFDEF PACKAGE}
  TLayoutPanelPadding = class(TPadding)
  protected
    class procedure InitDefaults(AMargins: TMargins); override;
  published
    property Left   default 0;
    property Top    default 1;
    property Right  default 0;
    property Bottom default 1;
  end;
{$ENDIF}

  TCustomLayoutPanel = class(TCustomInternalPanel)
{$IFDEF PACKAGE}
  private
    FPadding: TLayoutPanelPadding;
    procedure DoPaddingChange(Sender: TObject);
    procedure SetPadding(const Value: TLayoutPanelPadding);
{$ENDIF}
  private
    FControlLabel: TLabel;
    FControlLabelProperties: TCustomLayoutPanelLabelProperties;
    procedure SetControlLabelProperties(const Value: TCustomLayoutPanelLabelProperties);

    function GetAlign: TAlign;
    procedure SetAlign(const Value: TAlign);
    function IsAlignStored: Boolean;
    class function DefaultAlign: TAlign; dynamic;
{$IFDEF PACKAGE}
    procedure CMControlChanged(var Message: TCMControlChange); message CM_CONTROLCHANGE;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Align: TAlign read GetAlign write SetAlign stored IsAlignStored;
    property Caption: TCustomLayoutPanelLabelProperties read FControlLabelProperties write SetControlLabelProperties;
{$IFDEF PACKAGE}
    property Padding: TLayoutPanelPadding read FPadding write SetPadding;
{$ENDIF}
  end;

  { TcxCheckLookupComboBox }

  TLayoutPanel = class(TCustomLayoutPanel)
  protected
    class function DefaultAlign: TAlign; override;
//  public
//    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Caption;
    property Height Default 23;
    property AutoSizePercents;
    property Padding;

    property Font;
    property Anchors;
    property Constraints;
    property Enabled;
// TODO: Expired
    property ParentColor default False;
    property ParentFont default False;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
  end;

  TEditLayoutPanel = class(TLayoutPanel)
  private
    FEditControl: TControl;
    procedure SetEditControl(const Value: TControl);
  published
    property EditControl: TControl read FEditControl write SetEditControl;
  end;

  procedure Register;

implementation

const
  SConst_Label: String = 'Label';

procedure Register;
begin
//  RegisterComponents ('MD Project Tools', [TLayoutPanel, TEditLayoutPanel]);
  RegisterComponents ('MD Project Tools', [TGroupPanel, TcxLayoutGroupBox, TLayoutPanel, TEditLayoutPanel]);
end;

{ TCustomLayoutPanel }

{$IFDEF PACKAGE}
procedure TCustomLayoutPanel.CMControlChanged(var Message: TCMControlChange);
begin
  if (Message.Inserting) and (Message.Control <> FControlLabel) then
    with Message.Control do
      if (Align <> alClient) and (not AlignWithMargins) then begin
        Align := alClient;
      end;
end;
{$ENDIF}

constructor TCustomLayoutPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];

  FControlLabel := TLabel.Create(Self);
  FControlLabel.Transparent := True;
  FControlLabelProperties := TCustomLayoutPanelLabelProperties.Create(Self);

  BevelOuter := bvNone;
  Height := 23;

{$IFDEF PACKAGE}
  FPadding := TLayoutPanelPadding.Create(Self);
  FPadding.OnChange := DoPaddingChange;
  DoPaddingChange(Self);
{$ELSE}
  Padding.SetBounds(0, 1, 0, 1);
{$ENDIF}
  Align := DefaultAlign;
end;

class function TCustomLayoutPanel.DefaultAlign: TAlign;
begin
  Result := alNone
end;

destructor TCustomLayoutPanel.Destroy;
begin
  inherited;
  FControlLabelProperties.Free;
{$IFDEF PACKAGE}
  FPadding.Free;
{$ENDIF}
end;

function TCustomLayoutPanel.GetAlign: TAlign;
begin
  Result := (inherited Align)
end;

function TCustomLayoutPanel.IsAlignStored: Boolean;
begin
  Result := (inherited Align <> DefaultAlign);
end;

procedure TCustomLayoutPanel.SetAlign(const Value: TAlign);
begin
  if (inherited Align <> Value) then begin
    if Value = alClient then
      AutoSizePercents := -1;
    inherited Align := Value;
  end;
end;

{$IFDEF PACKAGE}
procedure TCustomLayoutPanel.DoPaddingChange(Sender: TObject);
begin
  inherited Padding := FPadding;
end;

procedure TCustomLayoutPanel.SetPadding(const Value: TLayoutPanelPadding);
begin
  FPadding.Assign(Value);
end;
{$ENDIF}

procedure TCustomLayoutPanel.SetControlLabelProperties(const Value: TCustomLayoutPanelLabelProperties);
begin
  FControlLabelProperties.Assign(Value);
end;

{ TCustomLayoutPanelLabelProperties }

constructor TCustomLayoutPanelLabelProperties.Create(AOwner: TCustomLayoutPanel);
begin
  FOwner := AOwner;
  inherited Create;
  Width := IDefault_Width;
  //Text := {$IFDEF PACKAGE}SConst_Label{$ELSE}''{$ENDIF};
  with FOwner.FControlLabel do begin
    AutoSize          := False;
    Margins.SetBounds(IDefault_Left, 0, IDefault_Right, 0);

    Parent := FOwner;

    AlignWithMargins  := True;
    Align             := alLeft;
    Layout            := tlCenter;
    WordWrap          := True;
  end;
end;

function TCustomLayoutPanelLabelProperties.GetText: String;
begin
  Result := FOwner.FControlLabel.Caption
end;

procedure TCustomLayoutPanelLabelProperties.SetText(const Value: String);
begin
  FOwner.FControlLabel.Caption := Value;
  if FOwner.FControlLabel.Caption <> Value then
    Raise Exception.Create('Not set');
end;

function TCustomLayoutPanelLabelProperties.GetAutoWidth: Boolean;
begin
  Result := FOwner.FControlLabel.AutoSize
end;

procedure TCustomLayoutPanelLabelProperties.SetAutoWidth(const AValue: Boolean);
begin
  FOwner.FControlLabel.WordWrap := (not AValue);
  FOwner.FControlLabel.AutoSize := AValue;
end;

function TCustomLayoutPanelLabelProperties.GetFont: TFont;
begin
  Result := FOwner.FControlLabel.Font
end;

procedure TCustomLayoutPanelLabelProperties.SetFont(const Value: TFont);
begin
  FOwner.FControlLabel.Font.Assign(Value);
end;

function TCustomLayoutPanelLabelProperties.GetLeft: Integer;
begin
  Result := FOwner.FControlLabel.Margins.Left
end;

procedure TCustomLayoutPanelLabelProperties.SetLeft(const Value: Integer);
begin
  FOwner.FControlLabel.Margins.Left := Value;
end;

function TCustomLayoutPanelLabelProperties.GetRight: Integer;
begin
  Result := FOwner.FControlLabel.Margins.Right
end;

procedure TCustomLayoutPanelLabelProperties.SetRight(const Value: Integer);
begin
  FOwner.FControlLabel.Margins.Right := Value;
end;

function TCustomLayoutPanelLabelProperties.GetWidth: Integer;
begin
  Result := FOwner.FControlLabel.Width
end;

function TCustomLayoutPanelLabelProperties.IsWidthStored: Boolean;
begin
  Result := (not AutoWidth) and (Width <> IDefault_Width);
end;

procedure TCustomLayoutPanelLabelProperties.SetWidth(const Value: Integer);
begin
  FOwner.FControlLabel.Width := Value;
end;

{$IFDEF PACKAGE}
{ TLayoutPanelPadding }

class procedure TLayoutPanelPadding.InitDefaults(AMargins: TMargins);
begin
  AMargins.SetBounds(0, 1, 0, 1);
end;
{$ENDIF}

{ TEditLayoutPanel }

procedure TEditLayoutPanel.SetEditControl(const Value: TControl);
begin
  if (FEditControl = Value) then Exit;

  if Assigned(FEditControl) then
    FEditControl.Parent := nil;

  FEditControl := Value;

  if Assigned(FEditControl) then
    FEditControl.Parent := Self;
end;

{ TCustomInternalPanel }

constructor TCustomInternalPanel.Create(AOwner: TComponent);
begin
  inherited;
  FAutoSizePercents := -1;
  ControlStyle := ControlStyle - [csSetCaption];

  ParentFont := False;
  ParentColor := False;
{$IFDEF USE_CX_PANEL}
  PanelStyle.Active := True;
  Style.BorderStyle := ebsNone;
{$ELSE}
  BevelOuter := bvNone;
{$ENDIF}
end;

procedure TCustomInternalPanel.SetAutoSizePercents(const Value: Integer);
begin
  if (FAutoSizePercents <> Value) and (Value > -1) then begin
    FAutoSizePercents := Value;
    AutoSize := (FAutoSizePercents = 0);
  end;
end;

procedure TCustomInternalPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if Parent <> nil then begin
    if FAutoSizePercents > 0 then
      case Align of
        alTop, alBottom:
          AHeight := Parent.Height * FAutoSizePercents div 100;
        alLeft, alRight:
          AWidth := Parent.Width * FAutoSizePercents div 100;
      end;
  end;

  inherited;
end;

//procedure TCustomInternalPanel.RequestAlign;
//begin
//  if Parent <> nil then begin
//    if FAutoSizePercents > 0 then
//      case Align of
//        alTop, alBottom:
//          Height := Parent.Height * FAutoSizePercents div 100;
//      end;
//    inherited;
//  end;
//end;

{ TGroupPanel }

constructor TGroupPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];

  Height := 23;

  //FStyle := lgsCustomClient;
  FLoadedStyle := lgsLayoutLine;
  InternalSetLayoutGroupStyle;
end;

procedure TGroupPanel.SetLayoutGroupStyle(const Value: TLayoutGroupStyle);
begin
  FLoadedStyle := Value;
  //if not (csReading in ComponentState) and (FStyle <> FLoadedStyle) then
  if (FStyle <> FLoadedStyle) then
    InternalSetLayoutGroupStyle;
end;

procedure TGroupPanel.InternalSetLayoutGroupStyle;
begin
  FStyle := FLoadedStyle;
  case FStyle of
    lgsFormInner:
      begin
        BevelOuter := bvLowered;
        Padding.SetBounds(5, 5, 5, 1);
        Align := alClient;
      end;
    lgsDialogFormInner:
      begin
        BevelOuter := bvLowered;
        Padding.SetBounds(5, 1, 5, 0);
        Align := alClient;
      end;
    lgsTabSheetCustomInner, lgsTabSheetInner:
      begin
        BevelOuter := bvLowered;
        if FStyle = lgsTabSheetCustomInner then
          Padding.SetBounds(0, 0, 0, 0)
        else
          Padding.SetBounds(5, 2, 5, 5);
        Align := alClient;
      end;
    lgsCustomLeft, lgsCustomRight, lgsCustomTop, lgsCustomBottom, lgsCustomClient, lgsLayoutLine:
      begin
        BevelOuter := bvNone;
        Padding.SetBounds(0, 0, 0, 0);
        case FStyle of
          lgsCustomLeft:   Align := alLeft;
          lgsCustomRight:  Align := alRight;
          lgsCustomTop,
          lgsLayoutLine:   Align := alTop;
          lgsCustomBottom: Align := alBottom;

          lgsCustomClient: Align := alClient;
        end;
//        if (Align <> alClient) and (FAutoSizePercents = -1) then
//          AutoSizePercents := 0;
      end;
  end;
end;

procedure TGroupPanel.Loaded;
begin
  if (FStyle <> FLoadedStyle) then
    InternalSetLayoutGroupStyle;
  inherited;
end;

{ TcxCustomLayoutGroupBox }

constructor TcxCustomLayoutGroupBox.Create(AOwner: TComponent);
begin
  inherited;

  ParentFont := False;
  Style.Font.Style := [fsBold];
  Align := alTop;
  FAutoSizePercents := -1;
end;

function TcxCustomLayoutGroupBox.GetAlign: TAlign;
begin
  Result := (inherited Align)
end;

procedure TcxCustomLayoutGroupBox.SetAlign(const Value: TAlign);
begin
  if (inherited Align <> Value) then begin
    if Value = alClient then
      AutoSizePercents := -1;
    inherited Align := Value;
  end;
end;

function TcxCustomLayoutGroupBox.GetCaption: TCaption;
begin
  Result := (inherited Caption);
  if (Length(Result) > 2) and (Result[1] = ' ') and (Result[Length(Result)] = ' ') then
    Result := Copy(Result, 2, Length(Result) - 2)
end;

//function TcxCustomLayoutGroupBox.IsAutoSizePercentsStored: Boolean;
//var
//  LDefault: Integer;
//begin
//  if Align in [alClient] then
//    LDefault := -1
//  else
//    LDefault := 0;
//
//  Result := (FAutoSizePercents <> LDefault)
//end;

procedure TcxCustomLayoutGroupBox.SetAutoSizePercents(const Value: Integer);
begin
  if (FAutoSizePercents <> Value) and (Value > -1) then begin
    FAutoSizePercents := Value;
    AutoSize := (FAutoSizePercents = 0);
  end;
end;

procedure TcxCustomLayoutGroupBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if Parent <> nil then begin
    if FAutoSizePercents > 0 then
      case Align of
        alTop, alBottom:
          AHeight := Parent.Height * FAutoSizePercents div 100;
        alLeft, alRight:
          AWidth := Parent.Width * FAutoSizePercents div 100;
      end;
  end;

  inherited;
end;

procedure TcxCustomLayoutGroupBox.SetCaption(const Value: TCaption);
begin
  if Value = '' then
    inherited Caption := Value
  else
    inherited Caption := ' ' + Value + ' '
end;

{$IFDEF PACKAGE}
{ TcxLayoutGroupBoxPadding }

class procedure TcxLayoutGroupBoxPadding.InitDefaults(AMargins: TMargins);
begin
  AMargins.SetBounds(5, 0, 5, 5);
end;
{$ENDIF}

{ TcxLayoutGroupBox }

constructor TcxLayoutGroupBox.Create(AOwner: TComponent);
begin
  inherited;

{$IFDEF PACKAGE}
  FPadding := TcxLayoutGroupBoxPadding.Create(Self);
  FPadding.OnChange := DoPaddingChange;
  DoPaddingChange(Self);
{$ELSE}
  Padding.SetBounds(5, 0, 5, 5);
{$ENDIF}
end;

{$IFDEF PACKAGE}
destructor TcxLayoutGroupBox.Destroy;
begin
  FreeAndNil(FPadding);
  inherited;
end;

procedure TcxLayoutGroupBox.DoPaddingChange(Sender: TObject);
begin
  inherited Padding := FPadding;
end;

procedure TcxLayoutGroupBox.SetPadding(const Value: TcxLayoutGroupBoxPadding);
begin
  FPadding.Assign(Value);
end;
{$ENDIF}

{ TLayoutPanel }

//constructor TLayoutPanel.Create(AOwner: TComponent);
//begin
//  inherited;
//  Align := alTop;
//end;

class function TLayoutPanel.DefaultAlign: TAlign;
begin
  Result := alTop
end;

initialization
  RegisterClasses([TGroupPanel, TcxLayoutGroupBox, TLayoutPanel, TEditLayoutPanel]);

finalization
  UnRegisterClasses([TGroupPanel, TcxLayoutGroupBox, TLayoutPanel, TEditLayoutPanel]);


end.
