unit SBaseDxUtils;

interface

uses
  SysUtils, Classes, frxClass, fs_iinterpreter, fs_itools, fs_ievents, Variants,
  dxBar, dxStatusBar, cxBarEditItem, cxEdit, SBaseVariantFunctions, cxPC;

type
  TdxBarSubItemHelper = class Helper for TdxBarSubItem
    procedure CheckEnabled;
    procedure CheckVisible;
  end;

  TcxCustomBarEditItemHelper = class Helper for TcxCustomBarEditItem
//    procedure HideAll(AReason: TdxBarCloseUpReason);
    procedure SetEditAndDisplayValue(const Value: TcxEditValue);
  end;

implementation

type
  TcxBarEditItemControlCrack = class(TcxBarEditItemControl);

{ TdxBarSubItemHelper }

procedure TdxBarSubItemHelper.CheckEnabled;
var
  I: Integer;
  LEnabled: Boolean;
begin
  LEnabled := False;
  for I := 0 to ItemLinks.VisibleItemCount - 1 do begin
    if ItemLinks[I].Item is TdxBarSubItem then
      TdxBarSubItem(ItemLinks.VisibleItems[I].Item).CheckEnabled;
    LEnabled := ItemLinks.VisibleItems[I].Item.Enabled;
    if LEnabled then
      Break;
  end;
  Enabled := LEnabled;
end;

procedure TdxBarSubItemHelper.CheckVisible;
var
  I: Integer;
  LVisible: Boolean;
begin
  LVisible := False;
  for I := 0 to LinkCount - 1 do begin
    if ItemLinks[I].Item is TdxBarSubItem then
      TdxBarSubItem(ItemLinks[I].Item).CheckVisible;
    LVisible := not (ItemLinks[I].Item.Visible in [ivNever, ivInCustomizing]);
    if LVisible then
      Break;
  end;
  if LVisible then
    Visible := ivAlways
  else
    Visible := ivNever;
end;

{ TcxCustomBarEditItemHelper }

//procedure TcxCustomBarEditItemHelper.HideAll(AReason: TdxBarCloseUpReason);
//var
//  LdxBarItemLink: TdxBarItemLink;
//begin
//  if Assigned(LdxBarItemLink) then begin
//    if Assigned(LdxBarItemLink.Control) then
//      TcxBarEditItemControlCrack(LdxBarItemLink.Control).hide;
//  end;
//end;

procedure TcxCustomBarEditItemHelper.SetEditAndDisplayValue(const Value: TcxEditValue);
var
  LdxBarItemLink: TdxBarItemLink;
begin
  LdxBarItemLink := FocusedItemLink;
  if Assigned(LdxBarItemLink) then begin
    if Assigned(LdxBarItemLink.Control) then
      TcxBarEditItemControlCrack(LdxBarItemLink.Control).Hide(False);
  end;

  EditValue := Value;
end;

end.
