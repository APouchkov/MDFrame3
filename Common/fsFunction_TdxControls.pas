unit fsFunction_TdxControls;

interface

implementation

uses
  Types, SysUtils, Classes, frxClass, fs_iinterpreter, fs_itools, fs_ievents, Variants,
  dxBar, dxStatusBar, dxMDStatusBar, cxBarEditItem, cxEdit, SBaseVariantFunctions,
  SBaseConstants;

type
  TFunctions_TdxControls = class(TfsRTTIModule)
  private
    function Call_TdxControls_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Call_TdxStatusBarPanels_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Call_TdxBarItemLinks_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

{ TFunctions_TdxControls }

constructor TFunctions_TdxControls.Create(AScript: TfsScript);
begin
  inherited Create(AScript);

  AScript.AddEnum('TdxBarItemVisible', 'ivNever, ivInCustomizing, ivNotInCustomizing, ivAlways');

  with AScript.AddClass(TdxBarPopupMenu, TdxBarPopupMenu.ClassParent.ClassName) do begin
    AddEvent('OnPopup', TfsNotifyEvent);
    AddMethod('procedure Popup(const X, Y: Integer; PopupComponent: TComponent)', Call_TdxControls_Method);
    AddMethod('procedure PopupFromCursorPos', Call_TdxControls_Method);
  end;

  with AScript.AddClass(TdxBarItem, TdxBarItem.ClassParent.ClassName) do begin
    AddMethod('procedure Click', Call_TdxControls_Method);
    AddEvent('OnClick', TfsNotifyEvent);
  end;

  with AScript.AddClass(TcxBarEditItem, TcxBarEditItem.ClassParent.ClassName) do begin
    AddEvent('OnChange', TfsNotifyEvent);
  end;

  with AScript.AddClass(TdxBarListItem, TdxBarListItem.ClassParent.ClassName) do begin
  end;

  with AScript.AddClass(TdxBarWindowItem, TdxBarWindowItem.ClassParent.ClassName) do begin
    AddMethod('procedure SetFocus', Call_TdxControls_Method);
  end;

  with AScript.AddClass(TdxStatusBarPanels, TdxStatusBarPanels.ClassParent.ClassName) do begin
    AddDefaultProperty(SConst_Items, DelphiTypeInteger, 'TdxStatusBarPanel', Call_TdxStatusBarPanels_Method, True);
  end;

  with AScript.AddClass(TdxBarItemLinks, TdxBarItemLinks.ClassParent.ClassName) do begin
    AddDefaultProperty(SConst_Items, DelphiTypeInteger, 'TdxBarItemLink', Call_TdxBarItemLinks_Method, True);
  end;
end;

function TFunctions_TdxControls.Call_TdxControls_Method(Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
  if ClassType = TdxBarItem then begin
    if MethodName = 'CLICK' then
      TdxBarItem(Instance).Click
  end else if ClassType = TdxBarWindowItem then begin
    if MethodName = 'SETFOCUS' then
      TdxBarWindowItem(Instance).SetFocus(True)
  end else if ClassType = TdxBarPopupMenu then begin
    if MethodName = 'POPUP' then
      TdxBarPopupMenu(Instance).Popup(Caller.Params[0], Caller.Params[1])
    else if MethodName = 'POPUPFROMCURSORPOS' then
      TdxBarPopupMenu(Instance).PopupFromCursorPos;
  end;
end;

function TFunctions_TdxControls.Call_TdxStatusBarPanels_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'ITEMS.GET' then
    Result := Integer(TdxStatusBarPanels(Instance)[Caller.Params[0]])
end;

function TFunctions_TdxControls.Call_TdxBarItemLinks_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'ITEMS.GET' then
    Result := Integer(TdxBarItemLinks(Instance)[Caller.Params[0]])
end;

initialization
  RegisterClasses(
    [
      TdxBarManager,
      TdxBarComponent,
      TdxMDStatusBar,
      TdxStatusBarPanel,
      TdxBarDockControl,
      TdxBarItemLinks,
      TdxBarItemLink,
      TdxBarPopupMenu
    ]
  );

  fsRTTIModules.Add(TFunctions_TdxControls);

finalization
  UnRegisterClasses(
    [
      TdxBarManager,
      TdxBarComponent,
      TdxMDStatusBar,
      TdxStatusBarPanel,
      TdxBarDockControl,
      TdxBarItemLinks,
      TdxBarItemLink,
      TdxBarPopupMenu
    ]
  );

  fsRTTIModules.Remove(TFunctions_TdxControls);
end.
