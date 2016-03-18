unit dxMDBarButton;

interface

uses
  dxCore, dxBar, Classes, Windows, ImgList, cxClasses, dxBarStrs, cxControls,
  SysUtils, Math, dxBarAccessibility, cxLookAndFeels, Contnrs;

type
  TdxMDBarListItem = class;

{$WARNINGS OFF}
  TdxMDBarButton = class(TdxBarButton)
  strict private
    FDropDownMenu: TdxBarPopupMenu;
    FListItem: TdxMDBarListItem;
    FItemLinks: TdxBarItemLinks;

//    FUseMenu: Boolean;
    procedure ItemLinksChanged(AItemLinks: TObject);
    procedure ListItemsChanged(AItemLinks: TObject);
    procedure ListItemClick(ASender: TObject);
  protected
    function  InternalGetItemLinks: TdxBarItemLinks;
    function  InternalIsItemLinksStored: Boolean;
    procedure InternalSetItemLinks(const Value: TdxBarItemLinks);
    function  InternalGetDropDownMenu: TdxBarPopupMenu;
    function  InternalGetListItem: TdxMDBarListItem;

    // IdxBarLinksOwner
    function CanContainItem(AItem: TdxBarItem; out AErrorText: string): Boolean; override;
  private
    function GetAllowAllUp: Boolean;
    function GetButtonStyle: TdxBarButtonStyle;
    function GetCloseSubMenuOnClick: Boolean;
    function GetDown: Boolean;
    function GetDropDownEnabled: Boolean;
    function GetDropDownMenu: TdxBarCustomPopupMenu;
    function GetGroupIndex: Integer;
    function GetLowered: Boolean;
    function GetUnclickAfterDoing: Boolean;
//    procedure SetDropDownMenu(const Value: TdxBarCustomPopupMenu);

    function GetListItems: TStrings;
    procedure SetListItems(const Value: TStrings);
    function IsListItemsStored: Boolean;
    function GetItemIndex: Integer;
    function GetOnClick: TNotifyEvent;
    procedure SetOnClick(const Value: TNotifyEvent);
    function GetOnPopup: TNotifyEvent;
    procedure SetOnPopup(const Value: TNotifyEvent);
  protected
    procedure Loaded; override;
    procedure SetBarManager(Value: TdxBarManager); override;
    function GetItemLinks: TdxBarItemLinks; override;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoClick; override;
  published
    property AllowAllUp: Boolean read GetAllowAllUp;
    property ButtonStyle: TdxBarButtonStyle read GetButtonStyle;
    property CloseSubMenuOnClick: Boolean read GetCloseSubMenuOnClick;
    property DropDownEnabled: Boolean read GetDropDownEnabled;
    property GroupIndex: Integer read GetGroupIndex;
    property Down: Boolean read GetDown;
    property DropDownMenu: TdxBarCustomPopupMenu read GetDropDownMenu;
    property Lowered: Boolean read GetLowered;
    property UnclickAfterDoing: Boolean read GetUnclickAfterDoing;

    property ItemLinks: TdxBarItemLinks read FItemLinks write InternalSetItemLinks stored InternalIsItemLinksStored;
    property ListItems: TStrings read GetListItems write SetListItems stored IsListItemsStored;
    property ItemIndex: Integer read GetItemIndex;

    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;
    property OnPopup: TNotifyEvent read GetOnPopup write SetOnPopup;
  end;
{$WARNINGS ON}

  TdxMDBarButtonControl = class(TdxBarButtonControl)
  protected
    procedure ControlUnclick(ByMouse: Boolean); override;
  end;

  TdxMDBarListItem = class(TCustomdxBarContainerItem)
  private
    FShowValues: Boolean;
    FItemIndex: Integer;
    FItemList: TComponentList;
    FItems: TStrings;
    FShowCheck: Boolean;
    FOnItemsChanged: TNotifyEvent;

    function GetDataIndex: Integer;
    function GetItemList: TList;
    procedure SetDataIndex(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure ClickItem(Sender: TObject);
    procedure MDItemsChanged(AItemLinks: TObject);
  protected
    procedure ClearItemList; override;
    function CreateItem(AIndex, ACurIndex: Integer): TdxBarButton; virtual;
    function GetDisplayHint(const AText: string): string; virtual;
    function GetDisplayText(AItemIndex: Integer): string; overload; virtual;
    function GetDisplayText(const AText: string): string; overload; virtual;
    function GetItemClass: TdxBarButtonClass; virtual;
    function InternalActuallyVisible: Boolean; override;
    procedure PopulateListedItemLinks(AItemLinks: TdxBarItemLinks; ALinkData: TObject; AIndex: Integer); override;
//    property OnItemsChanged: TNotifyEvent read FOnItemsChanged write FOnItemsChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure DirectClick; override;

    property ItemList: TList read GetItemList;
  published
    property DataIndex: Integer read GetDataIndex write SetDataIndex stored False;
    property ItemIndex: Integer read FItemIndex write FItemIndex default -1;
    property Items: TStrings read FItems write SetItems;
    property LargeGlyph;
    property LargeImageIndex;
    property ShowCheck: Boolean read FShowCheck write FShowCheck default False;
    property ShowValues: Boolean read FShowValues write FShowValues default False;
  end;

implementation

const
  ListItemSeparator = '-';

type
  TdxBarItemLinkCrack = class(TdxBarItemLink);
  TdxBarItemLinksCrack = class(TdxBarItemLinks);
  PdxBarManager = ^TdxBarManager;
  TdxBarCustomPopupComponentCrack = class(TdxBarCustomPopupComponent);

{ TdxMDBarButton }

function TdxMDBarButton.CanContainItem(AItem: TdxBarItem; out AErrorText: string): Boolean;
var
  LDropDownMenu: TdxBarCustomPopupMenu;
begin
  LDropDownMenu := (inherited DropDownMenu);
  if Assigned(LDropDownMenu) then
    Result := TdxBarCustomPopupComponentCrack(LDropDownMenu).CanContainItem(AItem, AErrorText)
  else
    Result := True
end;

constructor TdxMDBarButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FItemLinks := TdxBarSubMenuControlItemLinks.Create(BarManager, Self);
  FItemLinks.OnChange := ItemLinksChanged;
end;

destructor TdxMDBarButton.Destroy;
begin
  FreeAndNil(FItemLinks);
  FreeAndNil(FDropDownMenu);
  FreeAndNil(FListItem);

  inherited;
end;

procedure TdxMDBarButton.DoClick;
begin
  if Assigned(FListItem) then
    FListItem.ItemIndex := -1;
  inherited;
end;

function TdxMDBarButton.GetAllowAllUp: Boolean;
begin
  Result := (inherited AllowAllUp)
end;

function TdxMDBarButton.GetButtonStyle: TdxBarButtonStyle;
begin
  Result := (inherited ButtonStyle)
end;

function TdxMDBarButton.GetCloseSubMenuOnClick: Boolean;
begin
  Result := (inherited CloseSubMenuOnClick)
end;

function TdxMDBarButton.GetDown: Boolean;
begin
  Result := (inherited Down)
end;

function TdxMDBarButton.GetDropDownEnabled: Boolean;
begin
  Result := (inherited DropDownEnabled)
end;

function TdxMDBarButton.GetDropDownMenu: TdxBarCustomPopupMenu;
begin
  Result := nil;
{
  Result := (inherited DropDownMenu);
  if Result = FDropDownMenu then
    Result := nil;
}
end;

function TdxMDBarButton.GetGroupIndex: Integer;
begin
  Result := (inherited GroupIndex)
end;

function TdxMDBarButton.GetItemLinks: TdxBarItemLinks;
begin
  Result := FItemLinks
end;

function TdxMDBarButton.GetListItems: TStrings;
begin
  Result := InternalGetListItem.Items
end;

procedure TdxMDBarButton.SetListItems(const Value: TStrings);
begin
  InternalGetListItem.Items := Value
end;

function TdxMDBarButton.IsListItemsStored: Boolean;
begin
  Result := Assigned(FListItem) and (FListItem.Items.Count > 0);
end;

function TdxMDBarButton.GetLowered: Boolean;
begin
  Result := (inherited Lowered)
end;

function TdxMDBarButton.GetOnClick: TNotifyEvent;
begin
  Result := (inherited OnClick);
end;

function TdxMDBarButton.GetOnPopup: TNotifyEvent;
begin
  if FDropDownMenu <> nil then
    Result := FDropDownMenu.OnPopup
  else
    Result := nil
end;

procedure TdxMDBarButton.SetOnClick(const Value: TNotifyEvent);
begin
  inherited OnClick := Value;
  inherited CloseSubMenuOnClick := (not Assigned(inherited DropDownMenu)) or Assigned(inherited OnClick)
end;

procedure TdxMDBarButton.SetOnPopup(const Value: TNotifyEvent);
begin
  if TMethod(Value) <> TMethod(OnPopup) then
    InternalGetDropDownMenu.OnPopup := Value
end;

function TdxMDBarButton.GetUnclickAfterDoing: Boolean;
begin
  Result := (inherited UnclickAfterDoing)
end;

procedure TdxMDBarButton.SetBarManager(Value: TdxBarManager);
begin
  inherited;

  if Assigned(FDropDownMenu) then
    FDropDownMenu.BarManager := BarManager;

  if Assigned(FListItem) then
    FListItem.BarManager := BarManager;

  PdxBarManager(@(FItemLinks.BarManager))^ := BarManager;
  ItemLinksChanged(nil);
end;

function TdxMDBarButton.InternalGetDropDownMenu: TdxBarPopupMenu;
begin
  if not Assigned(FDropDownMenu) then begin
    FDropDownMenu := TdxBarPopupMenu.Create(Self);
    FDropDownMenu.BarManager := BarManager;
  end;
  Result := FDropDownMenu;
end;

function TdxMDBarButton.InternalGetListItem: TdxMDBarListItem;
begin
  if not Assigned(FListItem) then begin
    FListItem := TdxMDBarListItem.Create(Self);
    FListItem.Name := 'ListItems';
    FListItem.ShowValues := True;
    FListItem.FOnItemsChanged := ListItemsChanged;
    FListItem.OnClick := ListItemClick;
    FListItem.BarManager := BarManager;
  end;
  Result := FListItem;
end;

function TdxMDBarButton.InternalGetItemLinks: TdxBarItemLinks;
begin
  Result := InternalGetDropDownMenu.ItemLinks
end;

function TdxMDBarButton.InternalIsItemLinksStored: Boolean;
begin
  Result := (FItemLinks.Count > 0);
end;

procedure TdxMDBarButton.InternalSetItemLinks(const Value: TdxBarItemLinks);
begin
  FItemLinks.Assign(Value)
end;

function TdxMDBarButton.GetItemIndex: Integer;
begin
  if Assigned(FListItem) then
    Result := FListItem.ItemIndex
  else
    Result := -1
end;

procedure TdxMDBarButton.ListItemClick(ASender: TObject);
begin
  if Assigned(OnClick) then
    OnClick(ASender)
end;

procedure TdxMDBarButton.ListItemsChanged(AItemLinks: TObject);
begin
  ItemLinksChanged(nil);
end;

procedure TdxMDBarButton.Loaded;
begin
  inherited;
  ItemLinksChanged(nil);
end;

procedure TdxMDBarButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FListItem then
      FListItem := nil
    else if AComponent = FDropDownMenu then
      FDropDownMenu := nil
end;

procedure TdxMDBarButton.ItemLinksChanged(AItemLinks: TObject);
var
  LListItemUsed: Boolean;

  procedure AssignDropDownMenuItemLinks;
  begin
    with InternalGetDropDownMenu do begin
      ItemLinks.BeginUpdate;
      try
        ItemLinks := FItemLinks;
        if LListItemUsed then
          ItemLinks.Add.Item := FListItem;
      finally
        ItemLinks.EndUpdate;
      end;
    end;
  end;

var
  LMenuUsed: Boolean;
begin
  if ([csLoading, csDestroying] * ComponentState) <> [] then Exit;

  LListItemUsed := Assigned(FListItem) and (FListItem.Items.Count > 0);
  LMenuUsed := LListItemUsed or (FItemLinks.Count > 0);

  if LMenuUsed <> ((inherited ButtonStyle) = bsDropDown) then begin
    if LMenuUsed then begin
      AssignDropDownMenuItemLinks;
      inherited DropDownMenu := FDropDownMenu;
      inherited ButtonStyle := bsDropDown;
      inherited CloseSubMenuOnClick := Assigned(OnClick);
    end else begin
      inherited ButtonStyle := bsDefault;
      inherited DropDownMenu:= nil;
      inherited CloseSubMenuOnClick := True;
    end;
  end else if LMenuUsed then
    AssignDropDownMenuItemLinks
  else if Assigned(FDropDownMenu) then
    FDropDownMenu.ItemLinks.Clear
end;

{ TdxMDBarButtonControl }

procedure TdxMDBarButtonControl.ControlUnclick(ByMouse: Boolean);
begin
  if (Kind <> bbckDropDownButton)
      or (not ByMouse)
      or
      (
        (FHotPartIndex = bcpButton)
        and
        (
          (not Assigned(ButtonItem.DropDownMenu))
//          or
//          ButtonItem.CloseSubMenuOnClick
          or
          Assigned(ButtonItem.OnClick)
        )
      )
  then
    inherited;
end;

{ TdxMDBarListItem }

constructor TdxMDBarListItem.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := MDItemsChanged;
  FItemIndex := -1;
  FItemList := TComponentList.Create;
  FShowValues  := False;
end;

destructor TdxMDBarListItem.Destroy;
begin
  inherited;
  FreeAndNil(FItemList);
  FreeAndNil(FItems);
end;

procedure TdxMDBarListItem.Assign(Source: TPersistent);
begin
  if Source is TdxMDBarListItem then
  begin
    Items := TdxMDBarListItem(Source).Items;
    ShowCheck := TdxMDBarListItem(Source).ShowCheck;
    ShowValues := TdxMDBarListItem(Source).ShowValues;
  end;
  inherited;
end;

procedure TdxMDBarListItem.PopulateListedItemLinks(
  AItemLinks: TdxBarItemLinks; ALinkData: TObject; AIndex: Integer);
var
  ABeginGroup: Boolean;
  ACreateItems: Boolean;
  ACurIndex, I: Integer;
begin
  inherited PopulateListedItemLinks(AItemLinks, ALinkData, AIndex);
  ABeginGroup := True;
  ACreateItems := FItemList.Count = 0;
  ACurIndex := 0;
  for I := 0 to Items.Count - 1 do
  begin
    if Items[I] <> ListItemSeparator then
    begin
      if ACreateItems then
        CreateItem(I, ACurIndex);
      AddListedItemLink(AItemLinks, ALinkData, AIndex + ACurIndex,
        TdxBarItem(FItemList[ACurIndex])).BeginGroup := ABeginGroup;
      ABeginGroup := False;
      Inc(ACurIndex);
    end
    else
      ABeginGroup := True;
  end;
end;

function TdxMDBarListItem.GetDataIndex: Integer;
var
  I: Integer;
begin
  Result := -1;
  if (0 <= FItemIndex) and (FItemIndex < Items.Count) then
    for I := 0 to FItemIndex do
      if Items[I] <> ListItemSeparator then Inc(Result);
end;

function TdxMDBarListItem.GetItemList: TList;
begin
  Result := FItemList;
end;

procedure TdxMDBarListItem.SetDataIndex(Value: Integer);
var
  J, I: Integer;
begin
  if Items.Count = 0 then
    I := -1
  else
  begin
    J := -1;
    for I := 0 to Items.Count - 1 do
    begin
      if Items[I] <> ListItemSeparator then
        Inc(J);
      if J = Value then Break;
    end;
  end;
  ItemIndex := I;
end;

procedure TdxMDBarListItem.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TdxMDBarListItem.ClickItem(Sender: TObject);
begin
  FItemIndex := TComponent(Sender).Tag;
  DirectClick;
end;

procedure TdxMDBarListItem.ClearItemList;

  function IsAnotherInstanceOpen: Boolean;
  begin
    Result := (FItemList.Count > 0) and (TdxBarItem(FItemList.First).LinkCount > 0);
  end;

begin
  if not IsAnotherInstanceOpen then
    FItemList.Clear;
  inherited ClearItemList;
end;

function TdxMDBarListItem.CreateItem(AIndex, ACurIndex: Integer): TdxBarButton;
begin
  Result := GetItemClass.Create(BarManager.Owner);
  BarDesignController.AddInternalItem(Result, FItemList);
  Result.Action := Action;
  if ShowCheck and (AIndex{ACurIndex} = FItemIndex) then
  begin
    Result.ButtonStyle := bsChecked;
    Result.Down := True;
  end;
  Result.Caption := GetDisplayText(AIndex);
  Result.Hint := GetDisplayHint(Items[AIndex]);
  Result.Tag := AIndex;  //ACurIndex;
  Result.OnClick := ClickItem;
  Result.Enabled := Enabled;
end;

function TdxMDBarListItem.GetItemClass: TdxBarButtonClass;
begin
  Result := TdxBarButton;
end;

function TdxMDBarListItem.GetDisplayHint(const AText: string): string;
begin
  Result := '';
end;

function TdxMDBarListItem.GetDisplayText(const AText: string): string;
begin
  Result := AText;
end;

function TdxMDBarListItem.GetDisplayText(AItemIndex: Integer): string;
begin
  if FShowValues then
    Result := GetDisplayText(Items.ValueFromIndex[AItemIndex])
  else
    Result := GetDisplayText(Items.Strings[AItemIndex]);
end;

function TdxMDBarListItem.InternalActuallyVisible: Boolean;
begin
  Result := inherited InternalActuallyVisible or (Items.Count > 0);
end;

procedure TdxMDBarListItem.MDItemsChanged(AItemLinks: TObject);
begin
  inherited ItemsChanged(AItemLinks);
  if Assigned(FOnItemsChanged) then
    FOnItemsChanged(AItemLinks);
end;

procedure TdxMDBarListItem.DirectClick;
begin
  if CurItemLink = nil then
    inherited;
end;

initialization
  dxBarRegisterItem(TdxMDBarButton, TdxMDBarButtonControl, True);
  dxBarRegisterItem(TdxMDBarListItem, TdxBarContainerItemControl, True);

finalization
  dxBarUnregisterItem(TdxMDBarButton);
  dxBarUnregisterItem(TdxMDBarListItem);

end.
