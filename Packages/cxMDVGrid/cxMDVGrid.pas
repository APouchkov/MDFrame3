unit cxMDVGrid;

interface

uses
  Classes, SysUtils, Graphics, Controls,
  cxVGrid, cxDBVGrid, cxInplaceContainer, cxControls, cxCustomData, cxStyles;

const
  ScxMDVGridViewDefaultConfigure: String = 'Indicator';

type
  TcxMDvgMultiRecordsOptionsData = class(TcxvgMultiRecordsOptionsData)
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property CancelOnExit default False;

    property Appending default False;
    property Inserting default False;
    property Editing default False;
    property Deleting default False;
    property DeletingConfirmation default False;
  end;

  TcxMDvgMultiRecordsOptionsView = class(TcxvgMultiRecordsOptionsView)
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property CellEndEllipsis  default True;
    property GridLineColor    default clSilver;
  end;

  TcxMDvgMultiRecordsOptionsBehavior = class(TcxvgMultiRecordsOptionsBehavior)
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property CellHints        default True;
    property ImmediateEditor  default False;
    property RecordScrollMode default rsmByPixel;
  end;

  TcxMDControlUpdateDataEvent = procedure(AControl: TObject; AInfo: TcxUpdateControlInfo) of object;

  TcxMDDBVerticalGrid = class(TcxDBVerticalGrid)
  strict private
    FAutoHeightParentFirstCall: Boolean;
  private
    FOnUpdateData: TcxMDControlUpdateDataEvent;
    FAutoHeight: Boolean;
  protected
    function GetOptionsDataClass: TcxControlOptionsDataClass; override;
    function GetOptionsViewClass: TcxControlOptionsViewClass; override;
    function GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass; override;

    procedure ControlUpdateData(AInfo: TcxUpdateControlInfo); override;
    procedure DoLayoutChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoHeight: Boolean read FAutoHeight write FAutoHeight default True;
    property OnUpdateData: TcxMDControlUpdateDataEvent read FOnUpdateData write FOnUpdateData;
  end;

type
  TControlCrack = class(TControl);
  TcxExtEditingControlCrack = class(TcxExtEditingControl);
  TcxExtEditingControlViewInfoCrack = class(TcxExtEditingControlViewInfo);

{$IFNDEF PACKAGE}
var
  cxVGridStyleHeader: TcxStyle = nil;
{$ENDIF}

implementation

{ TcxMDDBVerticalGrid }

constructor TcxMDDBVerticalGrid.Create(AOwner: TComponent);
begin
  FAutoHeight := True;
  inherited;

{$IFNDEF PACKAGE}
  Styles.Header     := cxVGridStyleHeader;
  Styles.Selection  := cxVGridStyleHeader;
  Styles.Inactive   := cxVGridStyleHeader;
{$ENDIF}
end;

procedure TcxMDDBVerticalGrid.DoLayoutChanged;
var
  I, LHeight, LChilds: Integer;
  LcxCustomRow: TcxCustomRow;
begin
  if csUpdating in ComponentState then Exit;
  if FAutoHeight then begin
    LHeight := Controller.Scroller.ScrollStrategy.ViewInfo.HorzLineWidth;

    if Navigator.Visible then
      with TcxExtEditingControlViewInfoCrack(ViewInfo) do begin
        CalculateNavigator;
        LHeight := LHeight + NavigatorSiteVisibleRect.Height;
      end;

    for I := 0 to Rows.Count - 1 do begin
      LcxCustomRow := Rows[I];
      if LcxCustomRow.Visible and (LcxCustomRow.Parent = nil) then
        LHeight := LHeight + Controller.Scroller.ScrollStrategy.GetFullRowHeight(Rows[I], LChilds);
    end;

    if ClientHeight <> LHeight then begin
      ClientHeight := LHeight;

      if (not FAutoHeightParentFirstCall) and Assigned(Parent) and TControlCrack(Parent).AutoSize and (Parent.Height <> Height) then
        TControlCrack(Parent).AdjustSize;
    end;
    FAutoHeightParentFirstCall := True;
  end;
  inherited;
end;

procedure TcxMDDBVerticalGrid.ControlUpdateData(AInfo: TcxUpdateControlInfo);
begin
  inherited;

  if Assigned(FOnUpdateData) then
    FOnUpdateData(Self, AInfo);
end;

function TcxMDDBVerticalGrid.GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass;
begin
  Result := TcxMDvgMultiRecordsOptionsBehavior
end;

function TcxMDDBVerticalGrid.GetOptionsDataClass: TcxControlOptionsDataClass;
begin
  Result := TcxMDvgMultiRecordsOptionsData;
end;

function TcxMDDBVerticalGrid.GetOptionsViewClass: TcxControlOptionsViewClass;
begin
  Result := TcxMDvgMultiRecordsOptionsView
end;

{ TcxMDvgMultiRecordsOptionsData }

constructor TcxMDvgMultiRecordsOptionsData.Create(AOwner: TPersistent);
begin
  inherited;

  FCancelOnExit   := False;
  FAppending      := False;
  FInserting      := False;
  FEditing        := False;
  FDeleting       := False;
  FDeletingConfirmation := False;
end;

{ TcxMDvgMultiRecordsOptionsView }

constructor TcxMDvgMultiRecordsOptionsView.Create(AOwner: TPersistent);
begin
  inherited;

  CellEndEllipsis := True;
  GridLineColor   := clSilver;
end;

{ TcxMDvgMultiRecordsOptionsBehavior }

constructor TcxMDvgMultiRecordsOptionsBehavior.Create(AOwner: TPersistent);
begin
  inherited;

  CellHints                 := True;
  ImmediateEditor           := False;
  RecordScrollMode          := rsmByPixel;
end;

initialization
  RegisterClasses
  (
    [
      TcxMDDBVerticalGrid
    ]
  );

finalization
  UnRegisterClasses
  (
    [
      TcxMDDBVerticalGrid
    ]
  );


end.
