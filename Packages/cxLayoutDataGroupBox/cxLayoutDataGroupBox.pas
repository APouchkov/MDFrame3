unit cxLayoutDataGroupBox;

interface

uses
  LayoutPanel, Classes, cxRadioGroup, cxGroupBox, cxLookAndFeels, Controls, Messages, Windows;

const
  IConst_FilterPanelOffset = 120;
  WM_LookAndFeelChanged = WM_USER + 1;

type
  TcxLayoutDataGroupBox = class;

  TcxLayoutDataGroupBoxRadioButton = class(TCollectionItem)
  private
    FCaption: String;
    FValue  : String;
    procedure SetCaption(const Value: String);
  protected
    FRadionButton: TcxRadioButton;
  public
    procedure Release; override;
  published
    property Caption: String read FCaption write SetCaption;
    property Value: String read FValue write FValue;
  end;

  TcxLayoutDataGroupBoxRadioButtons = class(TCollection)
  private
    function GetItem(Index: Integer): TcxLayoutDataGroupBoxRadioButton;
  protected
    FOwner: TcxLayoutDataGroupBox;

    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TcxLayoutDataGroupBox); reintroduce;
    function  Add: TcxLayoutDataGroupBoxRadioButton;

    property Items[Index: Integer]: TcxLayoutDataGroupBoxRadioButton read GetItem; default;
  end;

  TDataFilterMethod = (dfFilter, dfParams);

  TcxLayoutDataGroupBox = class(TcxLayoutGroupBox)
  strict private
    FSkinUsed: Boolean;
    FTop: Integer;
    FHorizontalSpacing: Integer;

    procedure RecalcSkinUsed;
    procedure RecalcTop(AReSetPositions: Boolean = True);
    procedure WMLookAndFeelChanged(var Msg: TMessage); message WM_LookAndFeelChanged;
  private
    FFilters: TcxLayoutDataGroupBoxRadioButtons;
    FFilterMethod: TDataFilterMethod;
    FFilterComponent: TComponent;
    FFilterIndex: Integer;
//    FFilterPanel: TcxGroupBox;
    FFilterPanelOffset: Integer;

    procedure ReSetPositions;

    function  IsFiltersStored: Boolean;
    procedure SetFilters(const Value: TcxLayoutDataGroupBoxRadioButtons);
    procedure SetFilterPanelOffset(const Value: Integer);
    procedure SetFilterIndex(const Value: Integer);
  protected
    procedure Loaded; override;

    procedure FilterButtonsUpdated;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure SetParent(AParent: TWinControl); override;

    procedure OnRadioCheckChanged(ASender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

  published
    property FilterComponent: TComponent read FFilterComponent write FFilterComponent;
    property Filters: TcxLayoutDataGroupBoxRadioButtons read FFilters write SetFilters stored IsFiltersStored;
    property FilterIndex: Integer read FFilterIndex write SetFilterIndex default -1;
    property FilterMethod: TDataFilterMethod read FFilterMethod write FFilterMethod default dfFilter;
    property FilterPanelOffset: Integer read FFilterPanelOffset write SetFilterPanelOffset default IConst_FilterPanelOffset;
  end;

implementation

uses
  SysUtils, cxEdit, DB
{$IFNDEF PACKAGE}
  , DBPeriodicTabControl
{$ENDIF}
  ;

{ TcxLayoutDataGroupBoxRadioButtons }

function TcxLayoutDataGroupBoxRadioButtons.Add: TcxLayoutDataGroupBoxRadioButton;
begin
  Result := TcxLayoutDataGroupBoxRadioButton(inherited Add);
end;

constructor TcxLayoutDataGroupBoxRadioButtons.Create(AOwner: TcxLayoutDataGroupBox);
begin
  FOwner := AOwner;
  inherited Create(TcxLayoutDataGroupBoxRadioButton);
end;

function TcxLayoutDataGroupBoxRadioButtons.GetItem(Index: Integer): TcxLayoutDataGroupBoxRadioButton;
begin
  Result := TcxLayoutDataGroupBoxRadioButton(inherited GetItem(Index));
end;

procedure TcxLayoutDataGroupBoxRadioButtons.Update(Item: TCollectionItem);
begin
  FOwner.FilterButtonsUpdated;
end;

{ TcxLayoutDataGroupBox }

constructor TcxLayoutDataGroupBox.Create(AOwner: TComponent);
begin
  inherited;

  FFilterIndex := -1;
  FFilterPanelOffset := IConst_FilterPanelOffset;
  FFilterMethod := dfFilter;
  FFilters := TcxLayoutDataGroupBoxRadioButtons.Create(Self);

//  LookAndFeel.OnMasterEndChange := LookAndFeelOnMasterEndChange;

//  FFilterPanel := TcxGroupBox.Create(Self);
//  with FFilterPanel do begin
//    PanelStyle.Active := True;
//    Style.BorderStyle := ebsNone;
//
//    Padding.Left := 5;
//    Padding.Right := 5;
//    Height := 13;
//    AutoSize := True;
//  end;
  RecalcSkinUsed;
  RecalcTop(False);
end;

destructor TcxLayoutDataGroupBox.Destroy;
begin
//  FFilterPanel := nil;
  FreeAndNil(FFilters);
//  FreeAndNil(FFilterPanel);
  inherited;
end;

procedure TcxLayoutDataGroupBox.FilterButtonsUpdated;
var
  LCollectionItem: TCollectionItem;
  LButton: TcxLayoutDataGroupBoxRadioButton absolute LCollectionItem;
  LLeft: Integer;
begin
  if csReading in ComponentState then Exit;

  LLeft := 0;
  for LCollectionItem in FFilters do begin
    if not Assigned(LButton.FRadionButton) then begin
      LButton.FRadionButton := TcxRadioButton.Create(nil);
      with LButton.FRadionButton do begin
        Padding.SetBounds(3, 0, 3, 0);
//        Margins.SetBounds(3, 0, 3, 0);
//        AlignWithMargins := True;

        GroupIndex  := Integer(Self);
        Left        := LLeft;
        Top         := FTop;
        Transparent := FSkinUsed;
        Caption     := LButton.FCaption; // + SConst_Spaces;
        Parent      := Self.Parent;
        Checked     := (LButton.Index = FFilterIndex);
        OnClick := OnRadioCheckChanged;
      end;
    end;

    with LButton.FRadionButton do
      LLeft := Left + Width + FHorizontalSpacing;
  end;
end;

function TcxLayoutDataGroupBox.IsFiltersStored: Boolean;
begin
  Result := (FFilters.Count > 0)
end;

procedure TcxLayoutDataGroupBox.Loaded;
begin
  inherited;

//  RecalcTop;
  FilterButtonsUpdated;
  ReSetPositions;
//  LookAndFeelChanged(Style.LookAndFeel, []);
end;

procedure TcxLayoutDataGroupBox.WMLookAndFeelChanged(var Msg: TMessage);
begin
  ReSetPositions;
end;

procedure TcxLayoutDataGroupBox.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited;

  RecalcSkinUsed;
  RecalcTop(False);
  PostMessage(Handle, WM_LookAndFeelChanged, 0, 0);
end;

procedure TcxLayoutDataGroupBox.OnRadioCheckChanged(ASender: TObject);
var
  LCollectionItem: TCollectionItem;
  LButton: TcxLayoutDataGroupBoxRadioButton absolute LCollectionItem;
begin
  if TcxRadioButton(ASender).Checked then
    for LCollectionItem in FFilters do
      if LButton.FRadionButton = ASender then begin
        FilterIndex := LCollectionItem.Index;
        Break;
      end;
end;

procedure TcxLayoutDataGroupBox.RecalcSkinUsed;
begin
  FSkinUsed := (not Style.LookAndFeel.NativeStyle) and (Style.LookAndFeel.SkinName <> '');
  if FSkinUsed then
    FHorizontalSpacing := 2
  else
    FHorizontalSpacing := 0;

//  FFilterPanel.Transparent := FSkinUsed;
end;

procedure TcxLayoutDataGroupBox.RecalcTop(AReSetPositions: Boolean);
var
  LTop: Integer;
begin
  LTop := Top;
  if FSkinUsed then
    Inc(LTop, 2)
  else
    Dec(LTop, 1);

  if LTop <> FTop then begin
    FTop := LTop;
    if AReSetPositions then
      ReSetPositions;
  end;
end;

procedure TcxLayoutDataGroupBox.ReSetPositions;
var
  LLeft: Integer;

  LCollectionItem: TCollectionItem;
  LButton: TcxLayoutDataGroupBoxRadioButton absolute LCollectionItem;
begin

  LLeft := Left + FFilterPanelOffset;

  if Assigned(FFilters) then
  for LCollectionItem in FFilters do begin
    if Assigned(LButton.FRadionButton) then with LButton.FRadionButton do begin
      Top  := FTop;
      Left := LLeft;
      Transparent := FSkinUsed;
      Inc(LLeft, Width + FHorizontalSpacing);
    end;
  end;

  inherited;
end;

procedure TcxLayoutDataGroupBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;

  RecalcSkinUsed;
  RecalcTop(False);
  ReSetPositions;
end;

//procedure TcxLayoutDataGroupBox.Resize;
//begin
//  RecalcSkinUsed;
//  RecalcTop;
//  ReSetPositions;
//
//  inherited;
//end;

procedure TcxLayoutDataGroupBox.SetFilterIndex(const Value: Integer);
var
  LButton: TcxLayoutDataGroupBoxRadioButton;

  procedure ProcessDataSet(ADataSet: TDataSet);
  begin
    if LButton.FValue.IsEmpty then begin
      ADataSet.Filtered := False;
      ADataSet.Filter   := '';
    end else begin
      ADataSet.Filter := LButton.FValue;
      ADataSet.Filtered := True;
    end;
  end;
begin
  if FFilterIndex <> Value then begin
    FFilterIndex := Value;
    if {Assigned(FFilters) and} (FFilters.Count > Value) then begin
      LButton := FFilters[FFilterIndex];
      with LButton do begin
        if Assigned(FRadionButton) then
          FRadionButton.Checked := True;

        if Assigned(FFilterComponent) then begin
          if FFilterComponent is TDataSet then
            ProcessDataSet(TDataSet(FFilterComponent))
          else if (FFilterComponent is TDataSource) and Assigned(TDataSource(FFilterComponent).DataSet) then
            ProcessDataSet(TDataSource(FFilterComponent).DataSet)
{$IFNDEF PACKAGE}
          else if FFilterComponent is TDBPeriodicTabControl then
            TDBPeriodicTabControl(FFilterComponent).Periodic.ExternalFilter := LButton.FValue
{$ENDIF}

        end;
      end;
    end;
  end;
end;

procedure TcxLayoutDataGroupBox.SetFilterPanelOffset(const Value: Integer);
begin
  if FFilterPanelOffset <> Value then begin
    FFilterPanelOffset := Value;
  end;
end;

procedure TcxLayoutDataGroupBox.SetFilters(const Value: TcxLayoutDataGroupBoxRadioButtons);
begin
  FFilters.Assign(Value)
end;

procedure TcxLayoutDataGroupBox.SetParent(AParent: TWinControl);
var
  LCollectionItem: TCollectionItem;
  LButton: TcxLayoutDataGroupBoxRadioButton absolute LCollectionItem;
begin
  inherited;

  if Assigned(FFilters) then
    for LCollectionItem in FFilters do begin
      if Assigned(LButton.FRadionButton) then
        LButton.FRadionButton.Parent := Parent;
    end;
end;

{ TcxLayoutDataGroupBoxRadioButton }

procedure TcxLayoutDataGroupBoxRadioButton.Release;
begin
  inherited;
  if Assigned(FRadionButton) and not (csDestroying in FRadionButton.ComponentState) then
    FreeAndNil(FRadionButton);
end;

procedure TcxLayoutDataGroupBoxRadioButton.SetCaption(const Value: String);
begin
  FCaption := Value;
  if Assigned(FRadionButton) then begin
    FRadionButton.Caption := FCaption; // + SConst_Spaces;
  end;
  TcxLayoutDataGroupBoxRadioButtons(Collection).FOwner.ReSetPositions;
end;

initialization
  RegisterClasses([TcxLayoutDataGroupBox]);

finalization
  UnRegisterClasses([TcxLayoutDataGroupBox]);

end.
