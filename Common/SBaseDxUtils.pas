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

implementation

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

end.
