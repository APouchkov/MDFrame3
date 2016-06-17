unit fsFunction_TWinCtrl;

interface

implementation

uses
  Types, Forms, Controls, ImgList, fs_iinterpreter, Classes, Graphics, SBaseVariantFunctions,
  StdCtrls, ExtCtrls, Buttons,
  ActnList, fs_ievents, Fs_iextctrlsrtti, fsFunction_Other, ComCtrls, Variants, LayoutPanel,
  SBaseConstants,
  SynEdit, SynDBEdit, SynCompletionProposal;
  // Data.Bind.Components

type
  TControlCrack = class(TControl);

  TFunctions_WinCtlr = class(TfsRTTIModule)
  private
    function  Get_TControl_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TControl_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
    function  Call_TControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TWinControl_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function  Call_TWinControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

//    function  Call_TMenuItem_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TCustomEdit_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TCustomEdit_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Call_TCustomImageList_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Get_TCustomImageList_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Get_TPicture_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TPicture_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Get_TTextAttributes_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TTextAttributes_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Get_TCustomSynEdit_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TCustomSynEdit_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
  public
    constructor Create(AScript: TfsScript); override;
  end;

(*
  TfsTHTMLLabelAnchorClick = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject; Anchor: String);
    function GetMethod: Pointer; override;
  end;
*)

  TfsControlDragOver = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsControlDragDrop = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender, Source: TObject; X, Y: Integer);
    function GetMethod: Pointer; override;
  end;

  TfsCustomSynEditStatusChangeEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ASender: TObject; AChanges: TSynStatusChanges);
    function GetMethod: Pointer; override;
  end;

{ TFunctions_WinCtlr }

constructor TFunctions_WinCtlr.Create(AScript: TfsScript);
begin
  inherited Create(AScript);

  AScript.AddType('TImageIndex', fvtInt);

  with AScript.AddClass(TAction, TAction.ClassParent.ClassName) do begin
    AddEvent('OnExecute', TfsNotifyEvent);
  end;

//  with AScript.AddClass(TMenuItem, TMenuItem.ClassParent.ClassName) do begin
//    AddMethod('procedure Click', Call_TMenuItem_Method);
//  end;

  with AScript.AddClass(TControl, TControl.ClassParent.ClassName) do begin
//    AddProperty('Parent', 'TWinControl', Get_TControl_Property, Set_TControl_Property);
    AddProperty('Text', DelphiTypeString, Get_TControl_Property, Set_TControl_Property);
    AddProperty('Enabled', DelphiTypeBoolean, Get_TControl_Property, Set_TControl_Property);
    AddProperty('Visible', DelphiTypeBoolean, Get_TControl_Property, Set_TControl_Property);
    AddProperty('ClientHeight', DelphiTypeInteger, Get_TControl_Property, Set_TControl_Property);
    AddProperty('ClientWidth', DelphiTypeInteger, Get_TControl_Property, Set_TControl_Property);
    AddProperty('TopAfter', TControl.ClassName, nil, Set_TControl_Property);

    AddMethod('procedure Click', Call_TControl_Method);
    AddMethod('function ClientToScreen(const X, Y: Integer): VPoint', Call_TControl_Method);

    AddEvent('OnDragDrop', TfsControlDragDrop);
    AddEvent('OnDragOver', TfsControlDragOver);
  end;

  with AScript.AddClass(TWinControl, TWinControl.ClassParent.ClassName) do
  begin
    AddProperty('ControlCount', DelphiTypeInteger, Get_TWinControl_Property);
    AddProperty('CanFocus', DelphiTypeBoolean, Get_TWinControl_Property);
    AddIndexProperty('Controls', DelphiTypeInteger, 'TControl', Call_TWinControl_Method);
    AddMethod('function Focused: Boolean', Call_TWinControl_Method);
  end;

  with AScript.AddClass(TTextAttributes, TTextAttributes.ClassParent.ClassName) do
  begin
    AddProperty('Color', TConst_TColor, Get_TTextAttributes_Property, Set_TTextAttributes_Property);
    AddProperty('Name', DelphiTypeString, Get_TTextAttributes_Property, Set_TTextAttributes_Property);
    AddProperty('Size', DelphiTypeInteger, Get_TTextAttributes_Property, Set_TTextAttributes_Property);
    AddProperty('Style', 'TFontStyles', Get_TTextAttributes_Property, Set_TTextAttributes_Property);
  end;

  with AScript.AddClass(TCustomEdit, TCustomEdit.ClassParent.ClassName) do begin
    AddProperty('SelStart', DelphiTypeInteger, Get_TCustomEdit_Property, Set_TCustomEdit_Property);
  end;

  with AScript.AddClass(TPageControl, TPageControl.ClassParent.ClassName) do begin
    AddEvent('OnChange', TfsNotifyEvent);
  end;

  with AScript.AddClass(TCustomImageList, TCustomImageList.ClassParent.ClassName) do begin
    AddMethod('function GetBitmap(Index: Integer; Image: TBitmap): Boolean', Call_TCustomImageList_Method);
    AddProperty('Count', 'DelphiTypeInteger', Get_TCustomImageList_Property);
  end;

  with AScript.AddClass(TPicture, TPicture.ClassParent.ClassName) do begin
    AddProperty('Bitmap', 'TBitmap', Get_TPicture_Property, Set_TPicture_Property);
  end;

  with AScript.AddClass(TPicture, TPicture.ClassParent.ClassName) do begin
    AddProperty('Bitmap', 'TBitmap', Get_TPicture_Property, Set_TPicture_Property);
  end;

  AScript.AddClass(TPadding, TPadding.ClassParent.ClassName);

  AScript.AddEnumSet('TSynStatusChanges', 'scAll, scCaretX, scCaretY, scLeftChar, scTopLine,  scInsertMode, scModified, scSelection, scReadOnly');
  with AScript.AddClass(TCustomSynEdit, TCustomSynEdit.ClassParent.ClassName) do begin
    AddProperty('CaretX', DelphiTypeInteger, Get_TCustomSynEdit_Property, Set_TCustomSynEdit_Property);
    AddProperty('CaretY', DelphiTypeInteger, Get_TCustomSynEdit_Property, Set_TCustomSynEdit_Property);
    AddProperty('InsertMode', DelphiTypeBoolean, Get_TCustomSynEdit_Property, Set_TCustomSynEdit_Property);
    AddEvent('OnChange', TfsNotifyEvent);
    AddEvent('OnStatusChange', TfsCustomSynEditStatusChangeEvent);
  end;
  AScript.AddClass(TSynEdit, TSynEdit.ClassParent.ClassName);
  AScript.AddClass(TDBSynEdit, TDBSynEdit.ClassParent.ClassName);

//  with AScript.AddClass(THTMLabel, THTMLabel.ClassParent.ClassName) do begin
//    AddEvent('OnAnchorClick', TfsTHTMLLabelAnchorClick);
//  end;
end;

function TFunctions_WinCtlr.Call_TCustomImageList_Method(Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'GETBITMAP' then
    Result := TCustomImageList(Instance).GetBitmap(Caller.Params[0], VarToPointer(Caller.Params[1]));
end;

//function TFunctions_WinCtlr.Call_TMenuItem_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
//begin
//  if MethodName = 'CLICK' then
//    TMenuItem(Instance).Click
//end;

function TFunctions_WinCtlr.Get_TPicture_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'BITMAP' then
    Result := Integer(TPicture(Instance).Bitmap);
end;

function TFunctions_WinCtlr.Get_TCustomSynEdit_Property(Instance: TObject;
  ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TCustomSynEdit);
{$ENDIF}
  if PropName = 'CARETX' then
    Result := TCustomSynEdit(Instance).CaretX
  else if PropName = 'CARETY' then
    Result := TCustomSynEdit(Instance).CaretY
  else if PropName = 'INSERTMODE' then
    Result := TCustomSynEdit(Instance).InsertMode
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_WinCtlr.Get_TControl_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TControl);
{$ENDIF}
  if PropName = 'ENABLED' then
    Result := TControl(Instance).Enabled
  else if PropName = 'VISIBLE' then
    Result := TControl(Instance).Visible
  else if PropName = 'TEXT' then
    Result := TControlCrack(Instance).Text
  else if PropName = 'CLIENTHEIGHT' then
    Result := TControlCrack(Instance).ClientHeight
  else if PropName = 'CLIENTWIDTH' then
    Result := TControlCrack(Instance).ClientWidth
//  else if PropName = 'PARENT' then
//    Result := Integer(TControl(Instance).Parent)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_WinCtlr.Get_TCustomEdit_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'SELSTART' then
    Result := TCustomEdit(Instance).SelStart
end;

function TFunctions_WinCtlr.Get_TCustomImageList_Property(Instance: TObject;
  ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TCustomImageList);
{$ENDIF}
  if PropName = 'COUNT' then
    Result := Integer(TCustomImageList(Instance).Count)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TFunctions_WinCtlr.Set_TCustomEdit_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TCustomEdit);
{$ENDIF}
  if PropName = 'SELSTART' then
    TCustomEdit(Instance).SelStart := Value
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TFunctions_WinCtlr.Set_TControl_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TControl);
{$ENDIF}
  if PropName = 'ENABLED' then
    TControl(Instance).Enabled := Value
  else if PropName = 'VISIBLE' then
    TControl(Instance).Visible := Value
  else if PropName = 'TEXT' then
    TControlCrack(Instance).Text := Value
  else if PropName = 'CLIENTHEIGHT' then
    TControlCrack(Instance).ClientHeight := Value
  else if PropName = 'CLIENTWIDTH' then
    TControlCrack(Instance).ClientWidth := Value
  else if PropName = 'TOPAFTER' then
    TControl(Instance).Top := TControl(VarToPointer(Value)).Top + 1
//  else if PropName = 'PARENT' then
//    TControl(Instance).Parent := TWinControl(VarToPointer(Value))
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_WinCtlr.Call_TControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'CLICK' then
    TControlCrack(Instance).Click
  else if MethodName = 'CLIENTTOSCREEN' then
    with TControl(Instance).ClientToScreen(Point(Caller.Params[0], Caller.Params[1])) do
      Result := VarArrayOf([X, Y])
end;

procedure TFunctions_WinCtlr.Set_TPicture_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
   if PropName = 'BITMAP' then
    TPicture(Instance).Bitmap := TBitmap(Integer(Value));
end;

procedure TFunctions_WinCtlr.Set_TCustomSynEdit_Property(Instance: TObject;
  ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TCustomSynEdit);
{$ENDIF}
  if PropName = 'CARETX' then
    TCustomSynEdit(Instance).CaretX := Value
  else if PropName = 'CARETY' then
    TCustomSynEdit(Instance).CaretY := Value
  else if PropName = 'INSERTMODE' then
    TCustomSynEdit(Instance).InsertMode := Value
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_WinCtlr.Get_TTextAttributes_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
var
  LStyleSet: TFontStyles;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TPersistent);
{$ENDIF}
  if PropName = 'COLOR' then
    Result := (Instance as TTextAttributes).Color
  else if PropName = 'NAME' then
    Result := (Instance as TTextAttributes).Name
  else if PropName = 'SIZE' then
    Result := (Instance as TTextAttributes).Size
  else if PropName = 'STYLE' then
  begin
    LStyleSet := (Instance as TTextAttributes).Style;
    Result := VarFromSet(LStyleSet, SizeOf(TFontStyles));
  end
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TFunctions_WinCtlr.Set_TTextAttributes_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
var
  LStyleSet: TFontStyles;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TPersistent);
{$ENDIF}
  if PropName = 'COLOR' then
    (Instance as TTextAttributes).Color := Value
  else if PropName = 'NAME' then
    (Instance as TTextAttributes).Name := Value
  else if PropName = 'SIZE' then
    (Instance as TTextAttributes).Size := Value
  else if PropName = 'STYLE' then
  begin
    VarToSet(Value, LStyleSet, SizeOf(TFontStyles));
    (Instance as TTextAttributes).Style := LStyleSet;
  end
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_WinCtlr.Get_TWinControl_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'CONTROLCOUNT' then
    Result := TWinControl(Instance).ControlCount
  else if PropName = 'CANFOCUS' then
    Result := TWinControl(Instance).CanFocus;
end;

function TFunctions_WinCtlr.Call_TWinControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'CONTROLS.GET' then
    Result := Integer(TWinControl(Instance).Controls[Integer(Caller.Params[0])])
  else if MethodName = 'FOCUSED' then
    Result := TWinControl(Instance).Focused
end;

(*
{ TfsTHTMLLabelAnchorClick }

function TfsTHTMLLabelAnchorClick.GetMethod: Pointer;
begin
  Result := @TfsTHTMLLabelAnchorClick.DoEvent;
end;

procedure TfsTHTMLLabelAnchorClick.DoEvent(Sender: TObject; Anchor: String);
begin
  CallHandler([Sender, Anchor]);
end;
*)

{ TfsControlDragOver }

procedure TfsControlDragOver.DoEvent(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  CallHandler([Sender, Source, X, Y, Integer(State), Accept]);
  Accept := Handler.Params[5].Value;
end;

function TfsControlDragOver.GetMethod: Pointer;
begin
  Result := @TfsControlDragOver.DoEvent;
end;

{ TfsControlDragDrop }

procedure TfsControlDragDrop.DoEvent(Sender, Source: TObject; X, Y: Integer);
begin
  CallHandler([Source, X, Y]);
end;

function TfsControlDragDrop.GetMethod: Pointer;
begin
  Result := @TfsControlDragDrop.DoEvent;
end;

{ TfsCustomSynEditStatusChangeEvent }

procedure TfsCustomSynEditStatusChangeEvent.DoEvent(ASender: TObject; AChanges: TSynStatusChanges);
begin
  CallHandler([ASender, VarFromSet(AChanges, SizeOf(AChanges))]);
end;

function TfsCustomSynEditStatusChangeEvent.GetMethod: Pointer;
begin
  Result := @TfsCustomSynEditStatusChangeEvent.DoEvent;
end;

initialization
  RegisterClasses(
    [
      TSizeConstraints,
      TImageList,
      TShape,
      TScrollBox,
      TLabel,
      TPanel,
      TSplitter,
      TButton,
      TBevel,
      TRadioButton,
      TGroupBox,
      TBitBtn,
      TImage,
      TSpeedButton,
      TActionList,
      TAction,
      TLayoutPanel,
      TCustomLayoutPanelLabelProperties,
      TSynEdit, TDBSynEdit,
      TSynCompletionProposal
      // TBindingsList,
      // TLinkControlToProperty
    ]
  );

  fsRTTIModules.Add(TFunctions_WinCtlr);

finalization
  UnRegisterClasses(
    [
      TSizeConstraints,
      TImageList,
      TShape,
      TScrollBox,
      TLabel,
      TPanel,
      TSplitter,
      TButton,
      TBevel,
      TRadioButton,
      TGroupBox,
      TBitBtn,
      TImage,
      TSpeedButton,
      TActionList,
      TAction,
      TLayoutPanel,
      TCustomLayoutPanelLabelProperties,
      TSynEdit, TDBSynEdit,
      TSynCompletionProposal
      // TBindingsList,
      // TLinkControlToProperty
    ]
  );

end.

