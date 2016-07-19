unit cxMDDBTreeList;

interface

uses
  Classes, SysUtils, Graphics, cxInplaceContainer, cxLookAndFeelPainters,
  cxControls, cxTL, cxDBTL, cxStorage;

const
  ScxMDTreeListDefaultConfigure: String = 'Indicator,QuickCustomization';
  ScxMDTreeListInspectorConfigure: String = 'SyncMode,Headers=False,HideFocusRect,HideSelection,AutoWidth';

type
  TcxMDTreeListOptionsBehavior = class(TcxTreeListOptionsBehavior)
  private
    FAlwaysExpandedLevel: SmallInt;

    procedure SetAlwaysExpandedLevel(AValue: SmallInt);
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(ASource: TPersistent); override;
  published
    property AlwaysExpandedLevel: SmallInt read FAlwaysExpandedLevel write SetAlwaysExpandedLevel default 0;

    property CellHints          default True;
    property ConfirmDelete      default False;
    property ExpandOnDblClick   default False;
    property ExpandOnIncSearch  default True;
    property HeaderHints        default True;
    property ImmediateEditor    default False;
    property IncSearch          default True;

    property Sorting            default False;
//  property MultiSort          default False;

    property RecordScrollMode   default rsmByPixel;
    property ShowHourGlass      default False;
  end;

  TcxMDTreeListOptionsCustomizing = class(TcxTreeListOptionsCustomizing)
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property BandVertSizing   default False;
    property BandMoving       default False;
    property ColumnMoving     default False;
    property ColumnVertSizing default False;
  end;

  TcxMDTreeListOptionsData = class(TcxTreeListOptionsData)
  public
    constructor Create(AOwner: TPersistent); override;
  published
//    property Inserting        default False;
    property Editing          default False;
    property Deleting         default False;

    property CancelOnExit     default False;
  end;

  TcxMDDBTreeListOptionsData = class(TcxDBTreeListOptionsData)
  public
    constructor Create(AOwner: TPersistent); override;
  published
//    property Inserting        default False;
    property Editing          default False;
    property Deleting         default False;
    property SyncMode         default False;

    property CancelOnExit     default False;
    property CheckHasChildren default False;
  end;

  TcxMDTreeListOptionsView = class(TcxTreeListOptionsView)
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property CellEndEllipsis             default True;
    property GridLines                   default tlglBoth;
    property GridLineColor               default clSilver;
    property SimpleCustomizeBox          default True;
//    property UseImageIndexForSelected    default False;
  end;

  TcxMDTreeListOptionsSelection = class(TcxTreeListOptionsSelection)
  public
    constructor Create(AOwner: TPersistent); override;
  published
    property HideFocusRect default False;
    property InvertSelect  default False;
  end;

  TcxMDDBTreeListNode = class(TcxDBTreeListNode)
  protected
//    procedure SetParentFor(AValue: TcxTreeListNode; AValidateIndexes: Boolean = True); override;
    procedure RestoreStateAfterRefresh; override;
  public
    constructor Create(AOwner: TcxCustomTreeList); override;
  end;

  TcxMDTreeList = class(TcxTreeList)
  strict private
    FVersion: Byte;
  private
{$IFNDEF PACKAGE}
    procedure SetConfigure(const AValue: String);
{$ENDIF}

    function GetOptionsBehavior: TcxMDTreeListOptionsBehavior; inline;
    procedure SetOptionsBehavior(const Value: TcxMDTreeListOptionsBehavior);
    function GetOptionsCustomizing: TcxMDTreeListOptionsCustomizing;
    procedure SetOptionsCustomizing(const Value: TcxMDTreeListOptionsCustomizing);
    function GetOptionsData: TcxMDTreeListOptionsData;
    procedure SetOptionsData(const Value: TcxMDTreeListOptionsData);
    function GetOptionsView: TcxMDTreeListOptionsView;
    procedure SetOptionsView(const Value: TcxMDTreeListOptionsView);
    function GetOptionsSelection: TcxMDTreeListOptionsSelection;
    procedure SetOptionsSelection(const Value: TcxMDTreeListOptionsSelection);
  protected
{$IFNDEF PACKAGE}
    procedure StoreTo(AStorageType: TcxStorageType; const AStorageName: string; AStorageStream: TStream; AReCreate: Boolean; const ASaveTreeListName: string); override;
    procedure RestoreFrom(AStorageType: TcxStorageType; const AStorageName: string; AStorageStream: TStream; ACreateChildren, ADeleteChildren: Boolean; const ARestoreTreeListName: string); override;
{$ENDIF}

    function GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass; override;
    function GetOptionsCustomizingClass: TcxTreeListOptionsCustomizingClass; override;
    function GetOptionsDataClass: TcxControlOptionsDataClass; override;
    function GetOptionsViewClass: TcxControlOptionsViewClass; override;
    function GetOptionsSelectionClass: TcxTreeListOptionsSelectionClass; override;

    function InternalCanNodeCollapse(ANode: TcxTreeListNode): Boolean;
    function DoCanNodeCollapse(ANode: TcxTreeListNode): Boolean; override;
{$IFNDEF PACKAGE}
  public
    constructor Create(AOwner: TComponent); override;
{$ENDIF}
  published
    property Version: Byte read FVersion write FVersion Default 0;
{$IFNDEF PACKAGE}
    property Configure: String write SetConfigure;
{$ENDIF}
    property ParentFont default False;

    property OptionsBehavior: TcxMDTreeListOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsCustomizing: TcxMDTreeListOptionsCustomizing read GetOptionsCustomizing write SetOptionsCustomizing;
    property OptionsData: TcxMDTreeListOptionsData read GetOptionsData write SetOptionsData;
    property OptionsView: TcxMDTreeListOptionsView read GetOptionsView write SetOptionsView;
    property OptionsSelection: TcxMDTreeListOptionsSelection read GetOptionsSelection write SetOptionsSelection;
  end;

  TcxMDTreeListColumn = class(TcxTreeListColumn)
  protected
    function GetCaptionClass: TcxTreeListCaptionClass; override;
  end;

  TcxMDDBTreeList = class(TcxDBTreeList)
  strict private
    FVersion    : Byte;
    FConfigured : Boolean;

    FAutoHeight : Boolean;
    FAutoHeightNeedCheck: Boolean;

  private
{$IFNDEF PACKAGE}
    procedure SetConfigure(const AValue: String);
{$ENDIF}

    function  GetOptionsBehavior: TcxMDTreeListOptionsBehavior; inline;
    procedure SetOptionsBehavior(const Value: TcxMDTreeListOptionsBehavior);
    function  GetOptionsCustomizing: TcxMDTreeListOptionsCustomizing;
    procedure SetOptionsCustomizing(const Value: TcxMDTreeListOptionsCustomizing);
    function  GetOptionsData: TcxMDDBTreeListOptionsData;
    procedure SetOptionsData(const Value: TcxMDDBTreeListOptionsData);
    function  GetOptionsView: TcxMDTreeListOptionsView;
    procedure SetOptionsView(const Value: TcxMDTreeListOptionsView);
    function  GetOptionsSelection: TcxMDTreeListOptionsSelection;
    procedure SetOptionsSelection(const Value: TcxMDTreeListOptionsSelection);

    function IsRootValueStored: Boolean;
    function IsBandsStored: Boolean;
  protected
{$IFNDEF PACKAGE}
    procedure StoreTo(AStorageType: TcxStorageType; const AStorageName: string; AStorageStream: TStream; AReCreate: Boolean; const ASaveTreeListName: string); override;
    procedure RestoreFrom(AStorageType: TcxStorageType; const AStorageName: string; AStorageStream: TStream; ACreateChildren, ADeleteChildren: Boolean; const ARestoreTreeListName: string); override;
{$ENDIF}
    procedure DoDataChangedEvent(Sender: TObject); override;
    procedure AfterLayoutChanged; override;

    function GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass; override;
    function GetOptionsCustomizingClass: TcxTreeListOptionsCustomizingClass; override;
    function GetOptionsDataClass: TcxControlOptionsDataClass; override;
    function GetOptionsViewClass: TcxControlOptionsViewClass; override;
    function GetOptionsSelectionClass: TcxTreeListOptionsSelectionClass; override;

    function InternalCanNodeCollapse(ANode: TcxTreeListNode): Boolean;
    function DoCanNodeCollapse(ANode: TcxTreeListNode): Boolean; override;
    function CreateNode: TcxTreeListNode; override;

    property OnDataChanged;
{$IFNDEF PACKAGE}
  public
    constructor Create(AOwner: TComponent); override;
    procedure DoEndUpdate; override;
{$ENDIF}
    property Configured: Boolean read FConfigured;
  published
    property Version: Byte read FVersion write FVersion Default 0;
    property AutoHeight: Boolean read FAutoHeight write FAutoHeight Default False;

{$IFNDEF PACKAGE}
    property Configure: String write SetConfigure;
{$ENDIF}
    property ParentFont default False;
    property Bands stored IsBandsStored;
    property RootValue stored IsRootValueStored;

    property OptionsBehavior: TcxMDTreeListOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsCustomizing: TcxMDTreeListOptionsCustomizing read GetOptionsCustomizing write SetOptionsCustomizing;
    property OptionsData: TcxMDDBTreeListOptionsData read GetOptionsData write SetOptionsData;
    property OptionsView: TcxMDTreeListOptionsView read GetOptionsView write SetOptionsView;
    property OptionsSelection: TcxMDTreeListOptionsSelection read GetOptionsSelection write SetOptionsSelection;
  end;

  TcxMDTreeListCaption = class(TcxTreeListCaption)
  public
    constructor Create(AOwner: TPersistent); override;
  end;

  TcxMDDBTreeListColumn = class(TcxDBTreeListColumn)
  protected
    function GetCaptionClass: TcxTreeListCaptionClass; override;
  end;

{$IFNDEF PACKAGE}
var
  cxTreeListCustomDrawDataCellEvent: TcxTreeListCustomDrawEditCellEvent;
{$ENDIF}

implementation

uses
  Math, SBaseStringFunctions, SBaseConstants, SBaseVariantFunctions;

type
  TcxCustomTreeListCrack = class(TcxCustomTreeList);
  TcxTreeListViewInfoCrack = class(TcxTreeListViewInfo);

{ TcxMDDBTreeList }

{$IFNDEF PACKAGE}
procedure cxTreeListStoreTo(AcxTreeList: TcxCustomTreeList; AStorageType: TcxStorageType; const AStorageName: string; AStorageStream: TStream; AReCreate: Boolean; const ASaveTreeListName: string);
var
  LStorage: TcxStorage;
begin
  TcxCustomTreeListCrack(AcxTreeList).StoringName := ASaveTreeListName;
  LStorage := TcxStorage.Create(AStorageName, AStorageStream);
  try
    LStorage.UseInterfaceOnly := True;
    if ASaveTreeListName = '' then
      LStorage.NamePrefix := AcxTreeList.ClassName;
    LStorage.ReCreate := ARecreate;
    case AStorageType of
      stIniFile: LStorage.StoreToIni(AcxTreeList);
      stRegistry: LStorage.StoreToRegistry(AcxTreeList);
      stStream: LStorage.StoreToStream(AcxTreeList);
    end;
    LStorage.ReCreate := False;
  finally
    LStorage.Free;
  end;
end;

procedure cxTreeListRestoreFrom(AcxTreeList: TcxCustomTreeList; AStorageType: TcxStorageType; const AStorageName: string;
  AStorageStream: TStream; ACreateChildren, ADeleteChildren: Boolean;
  const ARestoreTreeListName: string);
var
  LStorage: TcxStorage;
  LModes: TcxStorageModes;
begin
  TcxCustomTreeListCrack(AcxTreeList).StoringName := ARestoreTreeListName;
  LStorage := TcxStorage.Create(AStorageName, AStorageStream);
  try
    LModes := [];
    if ACreateChildren then
      Include(LModes, smChildrenCreating);
    if ADeleteChildren then
      Include(LModes, smChildrenDeleting);
    LStorage.Modes := LModes;
    LStorage.UseInterfaceOnly := True;
    if ARestoreTreeListName = '' then
      LStorage.NamePrefix := AcxTreeList.ClassName;

    AcxTreeList.BeginUpdate;
    try
      TcxCustomTreeListCrack(AcxTreeList).IsRestoring := True;
      try
        case AStorageType of
          stIniFile: LStorage.RestoreFromIni(AcxTreeList);
          stRegistry: LStorage.RestoreFromRegistry(AcxTreeList);
          stStream: LStorage.RestoreFromStream(AcxTreeList);
        end;
      finally
        TcxCustomTreeListCrack(AcxTreeList).IsRestoring := False;
      end;
    finally
      AcxTreeList.EndUpdate;
    end;
  finally
    LStorage.Free;
  end;
end;

procedure cxTreeListSetConfigure(AcxTreeList: TcxCustomTreeList; const AValue: String);
var
  LItem: TStringNameItem;
  LAutoWidthFound: Boolean;
begin
  LAutoWidthFound := False;

  for LItem in EnumStringNameItems(AValue, ',', False) do
    if SameText(LItem.NameLeft, SConst_Bands) then
      AcxTreeList.OptionsView.Bands := StrToBoolDef(LItem.NameRight, True)
    else if SameText(LItem.NameLeft, SConst_Headers) then
      AcxTreeList.OptionsView.Headers := StrToBoolDef(LItem.NameRight, True)
    else if SameText(LItem.NameLeft, SConst_Indicator) then
      AcxTreeList.OptionsView.Indicator := True
    else if SameText(LItem.NameLeft, SConst_AutoHeight) then
      AcxTreeList.OptionsView.CellAutoHeight := True
    else if SameText(LItem.NameLeft, SConst_AutoWidth) then begin
      AcxTreeList.OptionsView.ColumnAutoWidth := True;
      LAutoWidthFound := True;
    end

    else if SameText(LItem.NameLeft, SConst_QuickCustomization) then begin
      AcxTreeList.OptionsCustomizing.BandsQuickCustomization   := True;
      AcxTreeList.OptionsCustomizing.ColumnsQuickCustomization := True;
    end else if SameText(LItem.NameLeft, SConst_Sorting) then
      AcxTreeList.OptionsBehavior.Sorting := True

    else if SameText(LItem.NameLeft, SConst_AlwaysExpandedLevel) then begin
      if AcxTreeList is TcxMDDBTreeList then
        TcxMDDBTreeList(AcxTreeList).OptionsBehavior.AlwaysExpandedLevel := StrToInt(LItem.NameRight)
      else
        TcxMDTreeList(AcxTreeList).OptionsBehavior.AlwaysExpandedLevel := StrToInt(LItem.NameRight)
    end

    else if SameText(LItem.NameLeft, SConst_HideFocusRect) then
      AcxTreeList.OptionsSelection.HideFocusRect := True
    else if SameText(LItem.NameLeft, SConst_HideSelection) then
      AcxTreeList.OptionsSelection.HideSelection := True

    else if SameText(LItem.NameLeft, SConst_SyncMode) then begin
      if AcxTreeList is TcxMDDBTreeList then
        TcxMDDBTreeList(AcxTreeList).OptionsData.SyncMode := True
    end

    else if SameText(LItem.NameLeft, SConst_Categorized) then
      AcxTreeList.OptionsView.PaintStyle := tlpsCategorized

    else
      Raise Exception.Create('cxTreeListSetConfigure.Configure.Option = ' + LItem.NameLeft + ' is unknown');
  ;

  if (AcxTreeList.Bands.Count = 0) then
    AcxTreeList.Bands.Add.Options.AutoWidthSizable := LAutoWidthFound;
end;

constructor TcxMDDBTreeList.Create(AOwner: TComponent);
begin
  inherited;

  ParentFont := False;

  if TMethod(cxTreeListCustomDrawDataCellEvent).Code <> nil then
    OnCustomDrawDataCell := cxTreeListCustomDrawDataCellEvent;
end;

procedure TcxMDDBTreeList.DoEndUpdate;
begin
  inherited;
end;

procedure TcxMDDBTreeList.RestoreFrom(AStorageType: TcxStorageType; const AStorageName: string; AStorageStream: TStream; ACreateChildren, ADeleteChildren: Boolean; const ARestoreTreeListName: string);
begin
  cxTreeListRestoreFrom(Self, AStorageType, AStorageName, AStorageStream, ACreateChildren, ADeleteChildren, ARestoreTreeListName);
end;

procedure TcxMDDBTreeList.StoreTo(AStorageType: TcxStorageType;
  const AStorageName: string; AStorageStream: TStream; AReCreate: Boolean;
  const ASaveTreeListName: string);
begin
  cxTreeListStoreTo(Self, AStorageType, AStorageName, AStorageStream, AReCreate, ASaveTreeListName);
end;
{$ENDIF}

function TcxMDDBTreeList.CreateNode: TcxTreeListNode;
begin
  Result := TcxMDDBTreeListNode.Create(Self);
  TcxMDDBTreeListNode(Result).OriginalIndex := DataController.NodesCount;
end;

function TcxMDDBTreeList.InternalCanNodeCollapse(ANode: TcxTreeListNode): Boolean;
var
  LAlwaysExpandedLevel: SmallInt;
begin
  LAlwaysExpandedLevel := OptionsBehavior.AlwaysExpandedLevel;
  Result := (LAlwaysExpandedLevel = 0)
            or
            (
              (LAlwaysExpandedLevel > 0)
              and
              (LAlwaysExpandedLevel <= ANode.Level)
            );
end;

function TcxMDDBTreeList.IsBandsStored: Boolean;
begin
  Result := (Bands.Count > 0)
end;

function TcxMDDBTreeList.IsRootValueStored: Boolean;
begin
  Result := not VarIsEqual(RootValue, Integer(-1));
end;

function TcxMDDBTreeList.DoCanNodeCollapse(ANode: TcxTreeListNode): Boolean;
begin
  Result := InternalCanNodeCollapse(ANode);
  if Result then
    Result := inherited DoCanNodeCollapse(ANode);
end;

function TcxMDDBTreeList.GetOptionsBehavior: TcxMDTreeListOptionsBehavior;
begin
  Result := TcxMDTreeListOptionsBehavior(inherited OptionsBehavior);
end;

function TcxMDDBTreeList.GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass;
begin
  Result := TcxMDTreeListOptionsBehavior;
end;

function TcxMDDBTreeList.GetOptionsCustomizing: TcxMDTreeListOptionsCustomizing;
begin
  Result := TcxMDTreeListOptionsCustomizing(inherited OptionsCustomizing);
end;

function TcxMDDBTreeList.GetOptionsCustomizingClass: TcxTreeListOptionsCustomizingClass;
begin
  Result := TcxMDTreeListOptionsCustomizing
end;

function TcxMDDBTreeList.GetOptionsData: TcxMDDBTreeListOptionsData;
begin
  Result := TcxMDDBTreeListOptionsData(inherited OptionsData);
end;

function TcxMDDBTreeList.GetOptionsDataClass: TcxControlOptionsDataClass;
begin
  Result := TcxMDDBTreeListOptionsData
end;

function TcxMDDBTreeList.GetOptionsSelection: TcxMDTreeListOptionsSelection;
begin
  Result := TcxMDTreeListOptionsSelection(inherited OptionsSelection)
end;

function TcxMDDBTreeList.GetOptionsSelectionClass: TcxTreeListOptionsSelectionClass;
begin
  Result := TcxMDTreeListOptionsSelection;
end;

function TcxMDDBTreeList.GetOptionsView: TcxMDTreeListOptionsView;
begin
  Result := TcxMDTreeListOptionsView(inherited OptionsView)
end;

function TcxMDDBTreeList.GetOptionsViewClass: TcxControlOptionsViewClass;
begin
  Result := TcxMDTreeListOptionsView;
end;

{$IFNDEF PACKAGE}
procedure TcxMDDBTreeList.SetConfigure(const AValue: String);
begin
  FConfigured := True;
  cxTreeListSetConfigure(Self, AValue);
end;
{$ENDIF}

procedure TcxMDDBTreeList.DoDataChangedEvent(Sender: TObject);
begin
  inherited;
  FAutoHeightNeedCheck := True;
end;

procedure TcxMDDBTreeList.AfterLayoutChanged;
var
  LHeight: Integer;
  LViewInfo: TcxTreeListViewInfoCrack;

  function GetAbsoluteHeight: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to AbsoluteVisibleCount - 1 do
      Result := Result + LViewInfo.GetNodePixelScrollSize(AbsoluteVisibleItems[I]);
  end;
begin
  inherited;

  if FAutoHeight and FAutoHeightNeedCheck
      and (LockUpdate = 0)
      and (OptionsBehavior.RecordScrollMode = rsmByPixel)
  then begin
    FAutoHeightNeedCheck := False;

    LViewInfo := TcxTreeListViewInfoCrack(ViewInfo);
    LHeight := GetAbsoluteHeight + LViewInfo.HeadersHeight + LViewInfo.FooterHeight
                    - ClientBounds.Bottom + ClientBounds.Top;

    if LHeight <> 0 then begin
      Inc(LHeight, Height);
      if (Constraints.MinHeight > 0) and (Constraints.MinHeight > LHeight) then
        LHeight := Constraints.MinHeight
      else if (Constraints.MaxHeight > 0) and (Constraints.MaxHeight < LHeight) then
        LHeight := Constraints.MaxHeight;

      if Height <> LHeight then
        Height := LHeight;
    end;
  end;
end;

procedure TcxMDDBTreeList.SetOptionsBehavior(const Value: TcxMDTreeListOptionsBehavior);
begin
  inherited;
end;

procedure TcxMDDBTreeList.SetOptionsCustomizing(const Value: TcxMDTreeListOptionsCustomizing);
begin
  inherited;
end;

procedure TcxMDDBTreeList.SetOptionsData(const Value: TcxMDDBTreeListOptionsData);
begin
  inherited;
end;

procedure TcxMDDBTreeList.SetOptionsSelection(const Value: TcxMDTreeListOptionsSelection);
begin
  inherited;
end;

procedure TcxMDDBTreeList.SetOptionsView(const Value: TcxMDTreeListOptionsView);
begin
  inherited;
end;

{ TcxMDTreeListOptionsBehavior }

procedure TcxMDTreeListOptionsBehavior.Assign(ASource: TPersistent);
begin
  if ASource is TcxMDTreeListOptionsBehavior then begin
    FAlwaysExpandedLevel := TcxMDTreeListOptionsBehavior(ASource).AlwaysExpandedLevel;
  end;
  inherited Assign(ASource);
end;

constructor TcxMDTreeListOptionsBehavior.Create(AOwner: TPersistent);
begin
//  FAlwaysExpandedLevel := 0;
  inherited;

  CellHints               := True;
  ConfirmDelete           := False;
  ExpandOnDblClick        := False;
  ExpandOnIncSearch       := True;
  HeaderHints             := True;
  ImmediateEditor         := False;
  IncSearch               := True;
  Sorting                 := False;
//  MultiSort               := False;
  RecordScrollMode        := rsmByPixel;
  ShowHourGlass           := False;
end;

procedure TcxMDTreeListOptionsBehavior.SetAlwaysExpandedLevel(AValue: SmallInt);
begin
  if AValue < -1 then AValue := -1;

  if FAlwaysExpandedLevel <> AValue then begin
    FAlwaysExpandedLevel := AValue;
    if (FAlwaysExpandedLevel = 0) and (TreeList.Count > 0) then
      TreeList.FullExpand
  end;
end;

{ TcxMDDBTreeListNode }

constructor TcxMDDBTreeListNode.Create(AOwner: TcxCustomTreeList);
begin
  inherited;

  if TcxMDDBTreeList(TreeList).OptionsBehavior.AlwaysExpandedLevel = -1 then
    State := State - [nsCollapsed];
end;

procedure TcxMDDBTreeListNode.RestoreStateAfterRefresh;
begin
  inherited;
  if (nsCollapsed in State) and not TcxMDDBTreeList(TreeList).InternalCanNodeCollapse(Self) then
    State := State - [nsCollapsed];
end;

{ TcxMDTreeListOptionsCustomizing }

constructor TcxMDTreeListOptionsCustomizing.Create(AOwner: TPersistent);
begin
  inherited;

  BandVertSizing            := False;
  BandMoving                := False;
  ColumnMoving              := False;
  ColumnVertSizing          := False;
end;

{ TcxMDDBTreeListOptionsData }

constructor TcxMDDBTreeListOptionsData.Create(AOwner: TPersistent);
begin
  inherited;

//  Inserting         := False;
  FEditing          := False;
  FDeleting         := False;

  FSyncMode         := False;

  CancelOnExit      := False;
  CheckHasChildren  := False;
end;

{ TcxMDTreeListOptionsView }

constructor TcxMDTreeListOptionsView.Create(AOwner: TPersistent);
begin
  inherited;
  CellEndEllipsis             := True;
  GridLines                   := tlglBoth;
  GridLineColor               := clSilver;
  SimpleCustomizeBox          := True;
//  UseImageIndexForSelected    := False;
end;

{ TcxMDTreeList }

{$IFNDEF PACKAGE}
constructor TcxMDTreeList.Create(AOwner: TComponent);
begin
  inherited;

  ParentFont := False;

  if TMethod(cxTreeListCustomDrawDataCellEvent).Code <> nil then
    OnCustomDrawDataCell := cxTreeListCustomDrawDataCellEvent;
end;

procedure TcxMDTreeList.RestoreFrom(AStorageType: TcxStorageType; const AStorageName: string; AStorageStream: TStream; ACreateChildren, ADeleteChildren: Boolean; const ARestoreTreeListName: string);
begin
  cxTreeListRestoreFrom(Self, AStorageType, AStorageName, AStorageStream, ACreateChildren, ADeleteChildren, ARestoreTreeListName);
end;

procedure TcxMDTreeList.StoreTo(AStorageType: TcxStorageType;
  const AStorageName: string; AStorageStream: TStream; AReCreate: Boolean;
  const ASaveTreeListName: string);
begin
  cxTreeListStoreTo(Self, AStorageType, AStorageName, AStorageStream, AReCreate, ASaveTreeListName);
end;
{$ENDIF}

function TcxMDTreeList.DoCanNodeCollapse(ANode: TcxTreeListNode): Boolean;
begin
  Result := InternalCanNodeCollapse(ANode);
  if Result then
    Result := inherited DoCanNodeCollapse(ANode);
end;

function TcxMDTreeList.GetOptionsBehavior: TcxMDTreeListOptionsBehavior;
begin
  Result := TcxMDTreeListOptionsBehavior(inherited OptionsBehavior);
end;

function TcxMDTreeList.GetOptionsBehaviorClass: TcxControlOptionsBehaviorClass;
begin
  Result := TcxMDTreeListOptionsBehavior;
end;

function TcxMDTreeList.GetOptionsCustomizing: TcxMDTreeListOptionsCustomizing;
begin
  Result := TcxMDTreeListOptionsCustomizing(inherited OptionsCustomizing);
end;

function TcxMDTreeList.GetOptionsCustomizingClass: TcxTreeListOptionsCustomizingClass;
begin
  Result := TcxMDTreeListOptionsCustomizing
end;

function TcxMDTreeList.GetOptionsData: TcxMDTreeListOptionsData;
begin
  Result := TcxMDTreeListOptionsData(inherited OptionsData);
end;

function TcxMDTreeList.GetOptionsDataClass: TcxControlOptionsDataClass;
begin
  Result := TcxMDTreeListOptionsData;
end;

function TcxMDTreeList.GetOptionsSelection: TcxMDTreeListOptionsSelection;
begin
  Result := TcxMDTreeListOptionsSelection(inherited OptionsSelection)
end;

function TcxMDTreeList.GetOptionsSelectionClass: TcxTreeListOptionsSelectionClass;
begin
  Result := TcxMDTreeListOptionsSelection;
end;

function TcxMDTreeList.GetOptionsView: TcxMDTreeListOptionsView;
begin
  Result := TcxMDTreeListOptionsView(inherited OptionsView)
end;

function TcxMDTreeList.GetOptionsViewClass: TcxControlOptionsViewClass;
begin
  Result := TcxMDTreeListOptionsView;
end;

function TcxMDTreeList.InternalCanNodeCollapse(ANode: TcxTreeListNode): Boolean;
var
  LAlwaysExpandedLevel: SmallInt;
begin
  LAlwaysExpandedLevel := OptionsBehavior.AlwaysExpandedLevel;
  Result := (LAlwaysExpandedLevel = 0)
            or
            (
              (LAlwaysExpandedLevel > 0)
              and
              (LAlwaysExpandedLevel <= ANode.Level)
            );
end;

{$IFNDEF PACKAGE}
procedure TcxMDTreeList.SetConfigure(const AValue: String);
begin
  cxTreeListSetConfigure(Self, AValue);
end;
{$ENDIF}

procedure TcxMDTreeList.SetOptionsBehavior(const Value: TcxMDTreeListOptionsBehavior);
begin
  inherited;
end;

procedure TcxMDTreeList.SetOptionsCustomizing(const Value: TcxMDTreeListOptionsCustomizing);
begin
  inherited;
end;

procedure TcxMDTreeList.SetOptionsData(const Value: TcxMDTreeListOptionsData);
begin
  inherited;
end;

procedure TcxMDTreeList.SetOptionsSelection(const Value: TcxMDTreeListOptionsSelection);
begin
  inherited;
end;

procedure TcxMDTreeList.SetOptionsView(const Value: TcxMDTreeListOptionsView);
begin
  inherited;
end;

{ TcxMDTreeListOptionsData }

constructor TcxMDTreeListOptionsData.Create(AOwner: TPersistent);
begin
  inherited;

//  Inserting         := False;
  FEditing          := False;
  FDeleting         := False;

  CancelOnExit      := False;
//  CheckHasChildren  := False;
end;

{ TcxMDTreeListOptionsSelection }

constructor TcxMDTreeListOptionsSelection.Create(AOwner: TPersistent);
begin
  inherited;

  HideFocusRect := False;
  InvertSelect  := False;
end;

{ TcxMDTreeListCaption }

constructor TcxMDTreeListCaption.Create(AOwner: TPersistent);
begin
  inherited;
  FAlignHorz := taCenter;
end;

{ TcxMDDBTreeListColumn }

function TcxMDDBTreeListColumn.GetCaptionClass: TcxTreeListCaptionClass;
begin
  Result := TcxMDTreeListCaption
end;

{ TcxMDTreeListColumn }

function TcxMDTreeListColumn.GetCaptionClass: TcxTreeListCaptionClass;
begin
  Result := TcxMDTreeListCaption
end;

initialization
  RegisterClasses([TcxMDDBTreeList, TcxMDTreeListColumn, TcxMDDBTreeListColumn]);

finalization
  UnRegisterClasses([TcxMDDBTreeList, TcxMDTreeListColumn, TcxMDDBTreeListColumn]);

end.
