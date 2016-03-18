unit dxMDBar;

interface
uses
  dxBar, Windows, Types, Contnrs, Classes ;

type
  TdxMDBarManager = class(TdxBarManager)
  private
    FVersion: Byte;
  published
    property Version: Byte read FVersion write FVersion default 0;
  end;

  TdxBarItemHelper = class Helper for TdxBarItem
  public
    function SubItemLinks: TdxBarItemLinks;
    procedure SetItemClickEvent(AEvent: TNotifyEvent; ARecurse: Boolean = True);
  end;

  TdxMDBarDockControl = class(TdxBarDockControl)
  protected
    function GetDockedBarControlClass: TdxBarControlClass; override;
  end;

  TdxMDBarControl = class(TdxBarControl)
  protected
    // Определение класс Viewera
    function GetViewInfoClass: TCustomdxBarControlViewInfoClass; override;
    // Определение максимальной высоты Bara
    function GetMaxHeight(AStyle: TdxBarDockingStyle): Integer; override;
    // Получаем ширину бара с учетом позиции элементов
    function GetSizeForWidth(AStyle: TdxBarDockingStyle; AWishedWidth: Integer): TSize; override;
    // Вычисляем расположение элементов на Bare
    procedure CalcRowItemRects; override;
  end;

  TdxMDBarControlViewInfo = class(TdxBarControlViewInfo)
  protected
    procedure DoCalcSeparatorInfo(AItemLink: TdxBarItemLink; const AItemRect: TRect); override;
  end;

  TdxMDBarControlDesignHelper = class(TdxBarControlDesignHelper)
  public
    class function GetForbiddenActions: TdxBarCustomizationActions; override;
  end;

  TdxMDBarPopupMenu = class(TdxBarPopupMenu)
  private
    FButtonList: TComponentList;
  public
    destructor Destroy; override;
    function AddButton(AdxBarItemClass: TdxBarItemClass): TdxBarItemLink;
    procedure ClearButtons;
  end;

implementation

uses
  Math, cxGeometry, SysUtils;

type
  TdxBarItemControlCrack = class(TdxBarItemControl);
  TdxBarItemLinkCrack = class(TdxBarItemLink);
  TdxBarControlCrack = class(TdxBarControl);
  TdxBarItemLinksCrack = class(TdxBarItemLinks);

{ TdxMDBarControl }

function TdxMDBarControl.GetViewInfoClass: TCustomdxBarControlViewInfoClass;
begin
  Result := TdxMDBarControlViewInfo;
end;

function TdxMDBarControl.GetMaxHeight(AStyle: TdxBarDockingStyle): Integer;
var
  AHeight, I: Integer;
  AItemLink: TdxBarItemLink;
begin
  Result := GetMinHeight(AStyle);
  AHeight := 0;

  for I := 0 to ItemLinks.CanVisibleItemCount - 1 do
  begin
    AItemLink := ItemLinks.CanVisibleItems[I];
    AHeight := Max(AHeight, AItemLink.ItemRect.Bottom);
  end;
  if MDIButtonsOnBar and (AHeight < GetMDIHeight) then
    AHeight := GetMDIHeight;

  if Result < AHeight then Result := AHeight+BeginGroupSize div 2;
end;

procedure TdxMDBarControl.CalcRowItemRects;
var
  X, Y, ARowHeight, ABeginGroupSize, ASize, VerSize, AWidth, AHeight: Integer;
  AMultiLine, AVertical, ItemControlAlignApplied: Boolean;
  AItemLink: TdxBarItemLinkCrack;

  function GetMDIButtonSize(AIsVertical: Boolean): Integer;
  begin
    if not MDIButtonsOnBar then
      Result := 0
    else
      if AIsVertical then
        Result := GetMDIHeight
      else
        Result := GetMDIWidth;
  end;

  procedure InitValues;
  begin
    X := 0;
    //Определение позиции первого элемента с учетом положения
    if (ItemLinks.VisibleItemCount > 0 ) and (ItemLinks.VisibleItems[0].Align = iaCenter) and Assigned(ItemLinks.VisibleItems[0].Control)
      then Y := (Self.Height - BeginGroupSize) div 2 - TdxBarItemControlCrack(ItemLinks.VisibleItems[0].Control).Height div 2 else Y := 0;

    VerSize := 0;
    AItemLink := nil;
    AMultiLine := MultiLine;
    AVertical := Vertical;

    if AVertical then
      ASize := ClientHeight
    else
      ASize := ClientWidth;
    if DockingStyle <> dsNone then
        Dec(ASize, GetMarkSize);

    if AVertical then
      ARowHeight := ClientWidth
    else
      ARowHeight := ClientHeight;
    Dec(ASize, GetMDIButtonSize(AVertical));
  end;

  procedure InitItemControlValues;
  begin
    with TdxBarItemControlCrack(AItemLink.Control) do
    begin
      FLastInRow := False;
      if AVertical then
      begin
        AWidth := Height;
        AHeight := Width;
      end
      else
      begin
        AWidth := Width;
        AHeight := Height;
      end;
    end;
    if AItemLink.BeginGroup and (X > 0) then
      ABeginGroupSize := BeginGroupSize
    else
      ABeginGroupSize := 0;
  end;

  function GetItemControlRect: TRect;

    procedure ApplyAlign;

    begin
      ItemControlAlignApplied := True;
      case TdxBarItemControlCrack(AItemLink.Control).Align of
        iaLeft:
          ItemControlAlignApplied := False;
        iaCenter:
          if AVertical then
            OffsetRect(Result, 0, (ASize - Result.Bottom) div 2)
          else
            OffsetRect(Result, (ASize - Result.Right) div 2, 0);
        iaRight:
          if AVertical then
            OffsetRect(Result, 0, ASize - Result.Bottom)
          else
            OffsetRect(Result, ASize - Result.Right, 0);
        iaClient:
          with Result do
            if AVertical then
            begin
              Bottom := ASize;
              if Bottom - Top < TdxBarItemControlCrack(AItemLink.Control).MinWidth then
                Bottom := Top + TdxBarItemControlCrack(AItemLink.Control).MinWidth;
            end
            else
            begin
              Right := ASize;
              if Right - Left < TdxBarItemControlCrack(AItemLink.Control).MinWidth then
                Right := Left + TdxBarItemControlCrack(AItemLink.Control).MinWidth;
            end;
      end;
    end;

  begin
    case DockingStyle of
      //Изменена начальная позиция элемента
      dsTop, dsBottom:
        Result := Bounds(X + ABeginGroupSize, Y, AWidth, AHeight);
      dsLeft, dsRight:
        Result :=
          Bounds((ARowHeight - AHeight) div 2, X + ABeginGroupSize, AHeight, AWidth);
    end;
    ApplyAlign;
  end;

  //Модифицированна процедура расчета X координаты с учетом позиции элемента
  procedure CalcX(ALinkIndex: Integer);
  var
    ANextItemLink: TdxBarItemLink;

    function RollBackBeginColumn: Integer;
    var
      I: Integer;
    begin
      Result := 0;

      for I := ALinkIndex downto 0 do begin
        Result := ItemLinks.VisibleItems[I].ItemRect.Left;
        if (ItemLinks.VisibleItems[i].Position = ipBeginsNewColumn) then Break;
      end;
    end;

    function MaxWidthColumn: Integer;
    var i: Integer;
    begin
      Result := 0;
      for i := ALinkIndex downto 0 do begin
        Result := Max(Result, ItemLinks.VisibleItems[i].ItemRect.Right);
        if ItemLinks.VisibleItems[i].Position = ipBeginsNewColumn then Break;
      end;
      // Inc(Result, ABeginGroupSize);
    end;


  begin
    if ItemLinks.VisibleItemCount <= Succ(ALinkIndex) then Exit;
    ANextItemLink := ItemLinks.VisibleItems[ALinkIndex+1];

    if AItemLink.BeginGroup then AItemLink.Position := ipBeginsNewColumn;
    if AItemLink.Align = iaCenter then ANextItemLink.Position := ipBeginsNewColumn;

    case ANextItemLink.Position of
      ipBeginsNewColumn :X := MaxWidthColumn;
      ipBeginsNewRow    :if not ANextItemLink.BeginGroup then X := RollbackBeginColumn else Inc(X, ABeginGroupSize + AWidth);
      ipContinuesRow    :X := ItemLinks.VisibleItems[ALinkIndex].ItemRect.Right + ABeginGroupSize;
    end;
  end;
  //Добавлена процедура расчета Y координаты с учетом позиции элемента
  procedure CalcY(ALinkIndex: Integer);
  var
    ANextItemLink: TdxBarItemLink;

    function MaxColumnHeight: Integer;
    var i: Integer;
    begin
      Result := 0;
      for i := ALinkIndex downto 0 do begin
        Result := Max(Result, ItemLinks.VisibleItems[i].ItemRect.Bottom);
        if ItemLinks.VisibleItems[i].Position = ipBeginsNewColumn then Break;
      end;
    end;

  begin
    if ItemLinks.VisibleItemCount <= Succ(ALinkIndex) then Exit;
    ANextItemLink := ItemLinks.VisibleItems[ALinkIndex+1];

    case ANextItemLink.Position of
      ipBeginsNewColumn :if (ANextItemLink.Align = iaCenter) and Assigned(ANextItemLink.Control) then Y := (Self.Height - BeginGroupSize) div 2 - TdxBarItemControlCrack(ANextItemLink.Control).Height div 2 else Y := 0;
      ipBeginsNewRow    :if ANextItemLink.Position <> ipBeginsNewColumn then Y := MaxColumnHeight else Y := 0;
      ipContinuesRow    :Y := ItemLinks.VisibleItems[ALinkIndex].ItemRect.Top;
    end;
  end;

var
  I: Integer;
begin
  InitValues;
  for I := 0 to ItemLinks.VisibleItemCount - 1 do
  begin
    AItemLink := TdxBarItemLinkCrack(ItemLinks.VisibleItems[I]);
    if AItemLink.Control = nil then AItemLink.CreateControl;
    InitItemControlValues;
    AItemLink.ItemRect := GetItemControlRect;
    AItemLink.RowHeight := ARowHeight;
    if ARowHeight < AHeight then ARowHeight := AHeight;
    CalcX(I); CalcY(I);
  end;

  if AItemLink <> nil then
    TdxBarItemControlCrack(AItemLink.Control).FLastInRow := True;
end;

function TdxMDBarControl.GetSizeForWidth(AStyle: TdxBarDockingStyle; AWishedWidth: Integer): TSize;
var
  J, ABeginGroupSize: Integer;
  AItemLink: TdxBarItemLink;
  Found: Boolean;
  PrevSize, Size: TSize;

  function CalcSize(var ASize: TSize): Boolean;
  var
    I, J: Integer;
    AColumnWidth, ARowWidth: Integer;
  begin
    Result := False;
    ASize.cx := 0; I := 0;
    //Вычисляем ширину видимых элементов
    while I < ItemLinks.VisibleItemCount do
    begin
      AColumnWidth := 0; ARowWidth := 0;
      for J := I to  Pred(ItemLinks.VisibleItemCount) do begin
        AItemLink := ItemLinks.VisibleItems[J];
        if ((AItemLink.BeginGroup) or (AItemLink.Position = ipBeginsNewColumn)) and (J <> I) then Break;
        if AItemLink.Position = ipBeginsNewRow then ARowWidth := 0;
        inc(ARowWidth, TdxBarItemControlCrack(AItemLink.Control).Width);
        AColumnWidth := Max(AColumnWidth, ARowWidth);
      end;
      I := J;
      if AItemLink.BeginGroup then
        ABeginGroupSize := BeginGroupSize
      else
        ABeginGroupSize := 0;

      inc(ASize.cx, AColumnWidth + ABeginGroupSize);
      if (ASize.cx > AWishedWidth) and (I <> 0) then begin
        Result := True;
        Break;
      end;
    end;
  end;

begin
  TdxBarItemLinksCrack(ItemLinks).RecentItemCount := -1;
  CalcLayout;
  TdxBarItemLinksCrack(ItemLinks).RestoreRecentItemCount;

  case AStyle of
    dsTop, dsBottom:
      begin
        Result.cx := GetMinWidth(AStyle);
        Result.cy := GetMinHeight(AStyle);
        AWishedWidth := Max(AWishedWidth, Result.cx);
        if MDIButtonsOnBar then Dec(AWishedWidth, GetMDIWidth);

        TdxBarItemLinksCrack(ItemLinks).RecentItemCount := -1;
        if ItemLinks.CanVisibleItemCount > 0 then
        begin
          Dec(AWishedWidth, GetMarkSize);
          if TdxBarItemLinksCrack(ItemLinks).CanUseRecentItems and not BarManager.Designing then
          begin
            PrevSize := Result;
            Dec(PrevSize.cx, GetMarkSize);
            Found := False;
            for J := 1 to ItemLinks.CanVisibleItemCount do
            begin
              //Начинаем вывод элементов с конца списка
              TdxBarItemLinksCrack(ItemLinks).RecentItemCount := J;
              Found := CalcSize(Size);
              //Если указанное количество элементов не помещается на бар - скрываем последний элемент
              if Found then
              begin
                TdxBarItemLinksCrack(ItemLinks).RecentItemCount := J - 1;
                Size := PrevSize;
                Break;
              end;
              PrevSize.cx := Size.cx;
            end;
            if not Found then TdxBarItemLinksCrack(ItemLinks).RecentItemCount := -1;
          end
          else
            CalcSize(Size);

          if MarkExists then
            Inc(Size.cx, GetMarkSize);

          Result.cx := Max(Result.cx, Size.cx);
          Result.cy := Max(Result.cy, GetMaxHeight(AStyle));
          if MDIButtonsOnBar then
          begin
            Inc(Result.cx, GetMDIWidth);
            if Result.cy < GetMDIHeight then Result.cy := GetMDIHeight;
          end;
        end;
      end;
    dsLeft, dsRight:
      begin
        Result.cx := GetMinWidth(AStyle);
        Result.cy := GetMaxHeight(AStyle);
      end;
  else
    Result := cxNullSize;
  end;
end;
{ /-=MD=- }

{ -=MD=- }
{ TdxMDBarControlViewInfo }

procedure TdxMDBarControlViewInfo.DoCalcSeparatorInfo(AItemLink: TdxBarItemLink; const AItemRect: TRect);
var
  R: TRect;
  AMarkSize: Integer;
begin
  if TdxBarControlCrack(BarControl).DockingStyle <> dsNone then
    AMarkSize := TdxBarControlCrack(BarControl).GetMarkSize
  else
    AMarkSize := 0;

  if not TdxBarControlCrack(BarControl).Vertical and (AItemRect.Left = 0) and (AItemRect.Top > 0) or
    TdxBarControlCrack(BarControl).Vertical and (AItemRect.Top > 0) then
  begin  // begin group in vertical direction
    if TdxBarControlCrack(BarControl).Vertical then
      R := Bounds(AItemRect.Left - (TdxBarItemLinkCrack(AItemLink).FRowHeight - cxRectWidth(AItemRect)) div 2,
        AItemRect.Top - TdxBarControlCrack(BarControl).BeginGroupSize, TdxBarItemLinkCrack(AItemLink).FRowHeight, TdxBarControlCrack(BarControl).BeginGroupSize)
    else
      R := Bounds(
        0,
        AItemRect.Top - (TdxBarItemLinkCrack(AItemLink).FRowHeight - cxRectHeight(AItemRect)) div 2 - TdxBarControlCrack(BarControl).BeginGroupSize,
        BarControl.ClientWidth - AMarkSize,
        TdxBarControlCrack(BarControl).BeginGroupSize);
    AddSeparatorInfo(R, skHorizontal, AItemLink.Control);
  end else begin  // begin group in horizontal direction
    if TdxBarControlCrack(BarControl).Vertical then
      R := Bounds(
        AItemRect.Right + (TdxBarItemLinkCrack(AItemLink).FRowHeight - cxRectWidth(AItemRect)) div 2, 0,
        TdxBarControlCrack(BarControl).BeginGroupSize,
        BarControl.ClientHeight - AMarkSize)
    else
      //Модифицированна процедура расчета высоты разделителя
      R := Bounds(
        AItemRect.Left - TdxBarControlCrack(BarControl).BeginGroupSize, 0,
        TdxBarControlCrack(BarControl).BeginGroupSize, AItemLink.BarControl.Height);

    AddSeparatorInfo(R, skVertical, AItemLink.Control);
  end;
end;

{ TdxMDBarDockControl }

function TdxMDBarDockControl.GetDockedBarControlClass: TdxBarControlClass;
begin
  Result := TdxMDBarControl;
end;

{ TdxMDBarControlDesignHelper }

class function TdxMDBarControlDesignHelper.GetForbiddenActions: TdxBarCustomizationActions;
begin
  Result := [caChangeButtonPaintStyle{, caChangePosition}, caChangeViewLevels,
    caChangeButtonGroup, caChangeRecentList, caChangeDistributed]
end;

{ TdxMDBarPopupMenu }

function TdxMDBarPopupMenu.AddButton(AdxBarItemClass: TdxBarItemClass): TdxBarItemLink;
var
  LdxBarItem: TdxBarItem;
begin
  if not Assigned(FButtonList) then
    FButtonList := TComponentList.Create(True);

  LdxBarItem := AdxBarItemClass.Create(BarManager);
  FButtonList.Add(LdxBarItem);

  Result := ItemLinks.Add;
  Result.Item := LdxBarItem;
end;

procedure TdxMDBarPopupMenu.ClearButtons;
begin
  ItemLinks.Clear;
  if Assigned(FButtonList) then
    FButtonList.Clear;
end;

destructor TdxMDBarPopupMenu.Destroy;
begin
  FreeAndNil(FButtonList);
  inherited;
end;

{ TdxBarItemHelper }

procedure TdxBarItemHelper.SetItemClickEvent(AEvent: TNotifyEvent; ARecurse: Boolean);

  procedure SetLinksEvent(AItemLinks: TdxBarItemLinks);
  var
    I: Integer;
  begin
    for I := 0 to Pred(AItemLinks.Count) do
      AItemLinks[I].Item.SetItemClickEvent(AEvent, True)
  end;

var
  LSubItemLinks: TdxBarItemLinks;
begin
  LSubItemLinks := SubItemLinks;

  if Assigned(LSubItemLinks) then begin
    if ARecurse then
      SetLinksEvent(LSubItemLinks);
  end else
    OnClick := AEvent;
end;

function TdxBarItemHelper.SubItemLinks: TdxBarItemLinks;
begin
  if Self is TdxBarSubItem then
    Exit(TdxBarSubItem(Self).ItemLinks)
  else if (Self is TdxBarButton) then begin
    if Assigned(TdxBarButton(Self).DropDownMenu) then
      Exit(TdxBarButton(Self).DropDownMenu.ItemLinks)
  end;

  Result := nil;
end;

initialization
  BarDesignController.RegisterBarControlDesignHelper(TdxMDBarControl, TdxMDBarControlDesignHelper);

finalization

end.
