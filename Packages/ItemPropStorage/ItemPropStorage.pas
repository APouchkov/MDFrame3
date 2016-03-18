unit ItemPropStorage;

interface

uses
  SysUtils, Classes, dxBar, ImgList, cxStyles;

type
  TdxBarPropItem = class(TCollectionItem)
  private
    FItem: TdxBarItem;
  protected
    procedure SetItem(const Value: TdxBarItem); virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    function GetDisplayName: string; override;
  published
    property Item: TdxBarItem read FItem write SetItem;
  end;

  TdxBarPropsClass = class of TdxBarProps;
  TdxBarPropItemClass = class of TdxBarPropItem;

  TdxBarProps = class(TOwnedCollection)
  private
    FCategory: string;
    function GetBarManager: TdxBarManager;
  protected
    function BarPropItemClass: TdxBarPropItemClass;  virtual;
    function GetItem(Index: Integer): TdxBarPropItem;
    procedure SetItem(Index: Integer; Value: TdxBarPropItem);
  public
    property Category: string read FCategory write FCategory;
    constructor Create(AOwner: TPersistent);
    function FindByBarItem(AdxBarItem: TdxBarItem): TdxBarPropItem;
    property BarManager: TdxBarManager read GetBarManager;
    property Items[Index: Integer]: TdxBarPropItem read GetItem write SetItem; default;
  end;

  TdxItemPropStorage = class(TComponent)
  private
    FBarManager: TdxBarManager;

    FProperties: TdxBarProps;
    function IsPropertiesStored: Boolean;
{$IFDEF PACKAGE}
    function GetBarManager: TdxBarManager;
{$ENDIF}
  protected
    function BarPropsClass: TdxBarPropsClass; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property BarManager: TdxBarManager read {$IFDEF PACKAGE}GetBarManager{$ELSE}FBarManager{$ENDIF};
  published
    property Properties: TdxBarProps read FProperties write FProperties stored IsPropertiesStored;
  end;

//  TdxImageIndex = type Integer;
//  TdxCustomImages = class(TPersistent)
//  private
//    FBarManager: TdxBarManager;
//  public
//    property BarManager: TdxBarManager read FBarManager write FBarManager;
//  end;

implementation

{$IFNDEF PACKAGE}
uses
  SBaseForm;
{$ENDIF}

resourcestring
  SErrItemExists = 'ƒанный Item уже выбран';

{ TdxItemPropStorage }

function TdxItemPropStorage.BarPropsClass: TdxBarPropsClass;
begin
  Result := TdxBarProps;
end;

constructor TdxItemPropStorage.Create(AOwner: TComponent);
begin
  Assert(Assigned(AOwner));
  inherited;
  FProperties := BarPropsClass.Create(Self);
{$IFNDEF PACKAGE}
  Assert(AOwner is TSBaseFrm);
  FBarManager := TSBaseFrm(AOwner).BarManager;
  FBarManager.FreeNotification(Self);
{$ENDIF}
end;

destructor TdxItemPropStorage.Destroy;
begin
  FreeAndNil(FProperties);
  inherited;
end;

{$IFDEF PACKAGE}
function TdxItemPropStorage.GetBarManager: TdxBarManager;
var
  I: Integer;
begin
  if not Assigned(FBarManager) then
    for I := 0 to Pred(Owner.ComponentCount) do
      if Owner.Components[i] is TdxBarManager then begin
        FBarManager := TdxBarManager(Owner.Components[i]);
        FBarManager.FreeNotification(Self);
        Break;
      end;

  Result := FBarManager;
end;
{$ENDIF}

function TdxItemPropStorage.IsPropertiesStored: Boolean;
begin
  Result := FProperties.Count > 0
end;

procedure TdxItemPropStorage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (AComponent = FBarManager) and (Operation = opRemove) then
    FBarManager := nil;
end;

function TdxBarProps.BarPropItemClass: TdxBarPropItemClass;
begin
  Result := TdxBarPropItem
end;

constructor TdxBarProps.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, BarPropItemClass);
end;

function TdxBarProps.FindByBarItem(AdxBarItem: TdxBarItem): TdxBarPropItem;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Pred(Count) do
    //в наследнике может быть Items[i].Item <> AItem 
    if Assigned(Items[i]) and (
        (Items[i].Item = AdxBarItem) or
        ((csAncestor in AdxBarItem.ComponentState) and
         (Items[i].Item.Name = AdxBarItem.Name) and
        (Items[i].Item.BarManager.Name = AdxBarItem.BarManager.Name))
      )
    then begin
      Result := Items[i];
      exit;
    end;
end;

function TdxBarProps.GetBarManager: TdxBarManager;
begin
  Result := (GetOwner as TdxItemPropStorage).BarManager;
end;

function TdxBarProps.GetItem(Index: Integer): TdxBarPropItem;
begin
  Result := TdxBarPropItem(inherited GetItem(Index));
end;

procedure TdxBarProps.SetItem(Index: Integer; Value: TdxBarPropItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TdxBarPropItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TdxBarPropItem then
    TdxBarPropItem(Dest).Item := Self.Item
  else
    inherited;
end;

function TdxBarPropItem.GetDisplayName: string;
begin
  if Assigned(FItem) then begin
    if SameText(FItem.Caption, FItem.Name) then
      Result := FItem.Name
    else
      Result := Format('%s (%s)', [FItem.Name, FItem.Caption])
  end else
    Result := inherited GetDisplayName;
end;

procedure TdxBarPropItem.SetItem(const Value: TdxBarItem);
var
  i: Integer;
begin
  if Assigned((Collection as TdxBarProps).BarManager) and (FItem <> Value) then begin
    for i := Pred(Collection.Count) downto 0 do
      if (Collection.Items[i] as TdxBarPropItem).Item = Value then begin
        if csDesigning in
          (Collection as TdxBarProps).BarManager.Owner.ComponentState
        then
          raise Exception.Create(SErrItemExists)
        else
          Collection.Delete(i);

        break;
      end;

    FItem := Value;
  end;
end;

initialization
//  RegisterClass(TdxItemPropStorage);

finalization
//  UnRegisterClass(TdxItemPropStorage);

end.
