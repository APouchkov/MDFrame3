unit fsFunction_TcxControls;

interface
{$I config.inc}

uses
  Types, SysUtils, Classes, DateUtils, Variants, Graphics, ComCtrls, Controls, Forms,

  frxClass, fs_iinterpreter, fs_itools, fs_ievents, fsFunction_Other, fsFunction_TWinCtrl,

  cxGraphics, cxTextEdit, cxButtonEdit, cxEdit, cxDBLookupComboBox,
  cxPC, cxTL, cxDBTL, cxLookupDBGrid, cxDBCheckComboBox, cxMDLookupTreeComboBox,
  SBaseCxUtils, cxDropDownEdit, cxDBEdit, RichEdit, cxRichEdit, Messages,
  cxContainer, cxImageComboBox, cxMultiFieldsDBDataBinding, cxScrollBox,
  dxMDWizardControl
{$IFDEF DELPHIXE3}
  , System.UITypes
{$ENDIF}
  ;

type
  TFunctions_TcxControls = class(TfsRTTIModule)
  private
    function Get_TcxCanvas_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Get_TcxCustomEdit_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TcxCustomEdit_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Call_TcxEditButtons_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Get_TcxCustomEditProperties_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TcxCustomEditProperties_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Get_TreeNode_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Get_TcxCustomLookupDBGrid_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TcxCustomLookupDBGrid_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Get_TcxCustomLookupComboBox_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function  Get_TcxLookupComboBoxProperties_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Call_TcxLookupDBGridColumns_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Call_TcxEditRepository_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TcxPageControl_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function  Call_TcxPageControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TcxTab_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TcxTab_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function Get_TcxTabs_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function Call_TcxNamedImageCollection_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function Call_TcxRichEdit_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Get_TcxRichEdit_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function Call_TcxCustomImageComboBoxProperties_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function Call_TcxCustomScrollBox_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

  TfsCxEditButtonClickEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject; AButtonIndex: Integer);
    function GetMethod: Pointer; override;
  end;

  TfsCxEditValidateEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject; var DisplayValue: Variant; var ErrorText: String; var Error: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsCxCustomLookupDBTreeComboBoxNodeSelectingEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject; Node: TcxDBTreeListNode; var Allow: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsCxRichEditPropertiesUrlClick = class(TfsCustomEvent)
    procedure DoEvent(ASender: TcxCustomRichEdit; const AURLText: string; AButton: TMouseButton);
    function GetMethod: Pointer; override;
  end;

implementation

uses
  SBaseConstants,
  SBaseVariantFunctions,
  cxControls;

{ TFunctions_TcxControls }

constructor TFunctions_TcxControls.Create(AScript: TfsScript);
begin
  inherited Create(AScript);

  AScript.AddEnum('TcxEditDropDownListStyle',  'lsEditFixedList, lsEditList, lsFixedList');
  AScript.AddEnum('TcxEditButtonKind', 'bkEllipsis, bkDown, bkGlyph, bkText');
  AScript.AddEnum('TcxCheckBoxState', 'cbsUnchecked,cbsChecked,cbsGrayed');

  AScript.AddClass(TcxControl, TcxControl.ClassParent.ClassName);

  with AScript.AddClass(TcxCanvas, TcxCanvas.ClassParent.ClassName) do begin
    AddProperty(SConst_Font, 'TFont', Get_TcxCanvas_Property);
    AddProperty('Brush', 'TBrush', Get_TcxCanvas_Property);
  end;

  with AScript.AddClass(TcxCustomEditProperties, TcxCustomEditProperties.ClassParent.ClassName) do begin
    AddProperty('Buttons', 'TcxEditButtons', Get_TcxCustomEditProperties_Property);
    AddProperty('ReadOnly', DelphiTypeBoolean, Get_TcxCustomEditProperties_Property, Set_TcxCustomEditProperties_Property);

//    AddEvent('OnChange', TfsNotifyEvent);
    AddEvent('OnButtonClick', TfsCxEditButtonClickEvent);
    AddEvent('OnValidate', TfsCxEditValidateEvent);
    AddEvent('OnEditValueChanged', TfsNotifyEvent);
  end;

  with AScript.AddClass(TcxCustomDropDownEditProperties, TcxCustomDropDownEditProperties.ClassParent.ClassName) do begin
    AddEvent('OnPopup', TfsNotifyEvent);
    AddEvent('OnInitPopup', TfsNotifyEvent);
  end;

  with AScript.AddClass(TcxCustomEdit, TcxCustomEdit.ClassParent.ClassName) do begin
    AddProperty(SConst_EditValue, DelphiTypeVariant, Get_TcxCustomEdit_Property, Set_TcxCustomEdit_Property);
    AddProperty('IsPresent', DelphiTypeBoolean, Get_TcxCustomEdit_Property);
  end;

  with AScript.AddClass(TcxCustomTabControl, TcxCustomTabControl.ClassParent.ClassName) do begin
    AddEvent('OnChange', TfsNotifyEvent);
  end;

  AScript.AddClass(TcxEditButton, TcxEditButton.ClassParent.ClassName);
  with AScript.AddClass(TcxEditButtons, TcxEditButtons.ClassParent.ClassName) do begin
    AddDefaultProperty(SConst_Items, DelphiTypeInteger, 'TcxEditButton', Call_TcxEditButtons_Method, True);
  end;

  with AScript.AddClass(TTreeNode, TTreeNode.ClassParent.ClassName) do begin
    AddProperty(SConst_Level, DelphiTypeInteger, Get_TreeNode_Property);
    AddProperty(SConst_Text , DelphiTypeString , Get_TreeNode_Property);
  end;

  with AScript.AddClass(TcxCustomMDLookupTreeComboBoxProperties, TcxCustomMDLookupTreeComboBoxProperties.ClassParent.ClassName) do begin
    AddEvent('OnSelecting', TfsCxCustomLookupDBTreeComboBoxNodeSelectingEvent);
  end;

  AScript.AddClass(TcxMDLookupTreeComboBoxProperties, TcxMDLookupTreeComboBoxProperties.ClassParent.ClassName);

  with AScript.AddClass(TcxLookupDBGridColumns, TcxLookupDBGridColumns.ClassParent.ClassName) do begin
    AddMethod('function ColumnByFieldName(const AFieldName: string): TcxLookupDBGridColumn', Call_TcxLookupDBGridColumns_Method);
  end;

  with AScript.AddClass(TcxEditRepository, TcxEditRepository.ClassParent.ClassName) do begin
    AddMethod('function ItemByName(const ARepositoryItemName: string): TcxEditRepositoryItem', Call_TcxEditRepository_Method);
  end;

  with AScript.AddClass(TcxDBEditDataBinding, TcxDBEditDataBinding.ClassParent.ClassName) do begin
    AddEvent('OnFieldChanged', TfsNotifyEvent);
  end;
  with AScript.AddClass(TcxMDDBEditDataBinding, TcxMDDBEditDataBinding.ClassParent.ClassName) do begin
    AddEvent('OnFieldChanged', TfsNotifyEvent);
  end;

  with AScript.AddClass(TcxLookupGridDBDataController, TcxLookupGridDBDataController.ClassParent.ClassName) do begin
  end;

  with AScript.AddClass(TcxCustomLookupDBGrid, TcxCustomLookupDBGrid.ClassParent.ClassName) do begin
    AddProperty(SConst_Columns, 'TcxLookupDBGridColumns', Get_TcxCustomLookupDBGrid_Property, Set_TcxCustomLookupDBGrid_Property);
    AddProperty('DataController', 'TcxLookupGridDBDataController', Get_TcxCustomLookupDBGrid_Property, Set_TcxCustomLookupDBGrid_Property);
  end;

  with AScript.AddClass(TcxLookupComboBoxProperties, TcxLookupComboBoxProperties.ClassParent.ClassName) do begin
    AddProperty('Grid', 'TcxCustomLookupDBGrid', Get_TcxLookupComboBoxProperties_Property);
  end;

  AScript.AddClass(TcxLookupComboBox, TcxLookupComboBox.ClassParent.ClassName);

  with AScript.AddClass(TcxCustomLookupComboBox, TcxCustomLookupComboBox.ClassParent.ClassName) do begin
    AddProperty('ActiveProperties', 'TcxLookupComboBoxProperties', Get_TcxCustomLookupComboBox_Property);
  end;

  with AScript.AddClass(TcxButtonEditProperties, TcxButtonEditProperties.ClassParent.ClassName) do begin
  end;

  with AScript.AddClass(TcxPageControl, TcxPageControl.ClassParent.ClassName) do begin
    AddProperty('PageCount', DelphiTypeInteger, Get_TcxPageControl_Property);
    AddIndexProperty('Pages', DelphiTypeInteger, 'TcxTabSheet', Call_TcxPageControl_Method, True);
  end;

  with AScript.AddClass(TcxTab, TcxTab.ClassParent.ClassName) do begin
    AddProperty(SConst_Visible, DelphiTypeBoolean, Get_TcxTab_Property, Set_TcxTab_Property);
  end;

  with AScript.AddClass(TcxTabs, TcxTabs.ClassParent.ClassName) do begin
    AddDefaultProperty('Tabs', DelphiTypeInteger, 'TcxTab', Get_TcxTabs_Method, True);
  end;

  AScript.AddClass(TcxDBCheckComboBox, TcxDBCheckComboBox.ClassParent.ClassName);

  AScript.AddClass(TcxLookupComboBox, TcxLookupComboBox.ClassParent.ClassName);
  AScript.AddClass(TcxDBLookupComboBox, TcxDBLookupComboBox.ClassParent.ClassName);

  AScript.AddClass(TcxMDLookupTreeComboBox, TcxMDLookupTreeComboBox.ClassParent.ClassName);
  AScript.AddClass(TcxMDLookupTreeComboBoxProperties, TcxMDLookupTreeComboBoxProperties.ClassParent.ClassName);
  AScript.AddClass(TcxMDDBLookupTreeComboBox, TcxMDDBLookupTreeComboBox.ClassParent.ClassName);

  with AScript.AddClass(TcxNamedImageCollection, TcxNamedImageCollection.ClassParent.ClassName) do begin
    AddMethod('function FindPicture(AImageName: String): TPicture', Call_TcxNamedImageCollection_Method);
  end;

//  AScript.AddMethod('procedure FillFontComboBox(ALines: TStrings)', Call_TcxRichEdit_Method);

  with AScript.AddClass(TcxRichEdit, TcxRichEdit.ClassParent.ClassName) do begin
    AddMethod('procedure SetFontStyle(AStyle: Byte; ASet: Boolean)', Call_TcxRichEdit_Method);
    AddMethod('function GetHTMLText: String', Call_TcxRichEdit_Method);
    AddMethod('function GetRtf: String', Call_TcxRichEdit_Method);
    AddMethod('procedure AddFormatedTextAtBegin(AText, AFontName: string; AColor, AFontSize, AStyle: Integer)', Call_TcxRichEdit_Method);

    AddProperty('SelAttributes', 'TTextAttributes', Get_TcxRichEdit_Property);
  end;

  with AScript.AddClass(TcxRichEditProperties, TcxRichEditProperties.ClassParent.ClassName) do begin
    AddEvent('OnSelectionChange', TfsNotifyEvent);
  end;
{
  with AScript.AddClass(TTextAttributes, TTextAttributes.ClassParent.ClassName) do begin
    AddProperty('Color', TConst_TColor, Get_TcxRichEdit_Property, Set_TTextAttributes_Property);
    AddProperty('Name', DelphiTypeString, Get_TcxRichEdit_Property, Set_TTextAttributes_Property);
    AddProperty('Size', DelphiTypeInteger, Get_TcxRichEdit_Property, Set_TTextAttributes_Property);
    AddProperty('Style', 'TFontStyles', Get_TcxRichEdit_Property, Set_TTextAttributes_Property);
  end;
}

  with AScript.AddClass(TcxRichEditProperties, TcxRichEditProperties.ClassParent.ClassName) do begin
    AddEvent('OnSelectionChange', TfsNotifyEvent);
    AddEvent('OnUrlClick', TfsCxRichEditPropertiesUrlClick);
  end;

  AScript.AddClass(TcxImageComboBoxItem, TcxImageComboBoxItem.ClassParent.ClassName);
  with AScript.AddClass(TcxCustomImageComboBoxProperties, TcxCustomImageComboBoxProperties.ClassParent.ClassName) do begin
    AddMethod('function FindItemByValue(const AValue: Variant): TcxImageComboBoxItem', Call_TcxCustomImageComboBoxProperties_Method);
    AddMethod('function FindImageIndexByValue(const AValue: Variant): Integer', Call_TcxCustomImageComboBoxProperties_Method);
  end;
  AScript.AddClass(TcxImageComboBoxProperties, TcxImageComboBoxProperties.ClassParent.ClassName);

  with AScript.AddClass(TcxCustomScrollBox, TcxCustomScrollBox.ClassParent.ClassName) do begin
    AddMethod('procedure MakeVisible(AControl: TWinControl)', Call_TcxCustomScrollBox_Method);
  end;

  AScript.AddClass(TdxMDWizardControl, TdxMDWizardControl.ClassParent.ClassName);
  AScript.AddClass(TdxMDWizardControlPage, TdxMDWizardControlPage.ClassParent.ClassName);
end;

function TFunctions_TcxControls.Get_TcxCanvas_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCanvas);
{$ENDIF}
  if PropName = 'FONT' then
    Result := Integer(TcxCanvas(Instance).Font)
  else if PropName = 'BRUSH' then
    Result := Integer(TcxCanvas(Instance).Brush)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxControls.Get_TcxCustomEdit_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomEdit);
{$ENDIF}
  if SameText(PropName, SConst_EditValue) then
    Result := TcxCustomEdit(Instance).EditValue
  else if PropName = 'ISPRESENT' then
    Result := VarIsPresent(TcxCustomEdit(Instance).EditValue)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TFunctions_TcxControls.Set_TcxCustomEditProperties_Property(
  Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomEditProperties);
{$ENDIF}
  if PropName = 'READONLY' then
    TcxCustomEditProperties(Instance).ReadOnly := Value
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TFunctions_TcxControls.Set_TcxCustomEdit_Property(
  Instance: TObject; ClassType: TClass; const PropName: String;
  Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomEdit);
{$ENDIF}
  if SameText(PropName, SConst_EditValue) then
    TcxCustomEdit(Instance).SetAndPostEditValue(Value)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxControls.Call_TcxCustomImageComboBoxProperties_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'FINDITEMBYVALUE' then
    Result := Integer(TcxCustomImageComboBoxProperties(Instance).FindItemByValue(Caller.Params[0]))
  else if MethodName = 'FINDIMAGEINDEXBYVALUE' then
    Result := TcxCustomImageComboBoxProperties(Instance).FindImageIndexByValue(Caller.Params[0])
end;

function TFunctions_TcxControls.Call_TcxCustomScrollBox_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'MAKEVISIBLE' then
    TcxCustomScrollBox(Instance).MakeVisible(TWinControl(Integer(Caller.Params[0])));
end;

function TFunctions_TcxControls.Call_TcxEditButtons_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxEditButtons);
{$ENDIF}
  if MethodName = 'ITEMS.GET' then
    Result := Integer(TcxEditButtons(Instance).Items[Caller.Params[0]])
  else if MethodName = 'ITEMS.SET' then
    TcxEditButtons(Instance).Items[Caller.Params[0]] := TcxEditButton(Integer(Caller.Params[1]))
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxControls.Get_TcxCustomEditProperties_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomEditProperties);
{$ENDIF}
  if PropName = 'BUTTONS' then
    Result := Integer(TcxCustomEditProperties(Instance).Buttons)
  else if PropName = 'READONLY' then
    Result := TcxCustomEditProperties(Instance).ReadOnly
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxControls.Get_TreeNode_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TTreeNode);
{$ENDIF}
  if PropName = 'LEVEL' then
    Result := TTreeNode(Instance).Level
  else
  if PropName = 'TEXT' then
    Result := TTreeNode(Instance).Text
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TfsCxCustomLookupDBTreeComboBoxNodeSelectingEvent.DoEvent(Sender: TObject; Node: TcxDBTreeListNode; var Allow: Boolean);
begin
  CallHandler([Sender, Node, Allow]);
  Allow := Handler.Params[2].Value = True;
end;

function TfsCxCustomLookupDBTreeComboBoxNodeSelectingEvent.GetMethod: Pointer;
begin
  Result := @TfsCxCustomLookupDBTreeComboBoxNodeSelectingEvent.DoEvent;
end;

function TFunctions_TcxControls.Get_TcxCustomLookupComboBox_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomLookupComboBox);
{$ENDIF}
  if PropName = 'ACTIVEPROPERTIES' then
    Result := Integer(TcxLookupComboBoxProperties(TcxCustomLookupComboBox(Instance).ActiveProperties))
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxControls.Get_TcxLookupComboBoxProperties_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxLookupComboBoxProperties);
{$ENDIF}
  if PropName = 'GRID' then
    Result := Integer(TcxCustomLookupDBGrid(TcxLookupComboBoxProperties(Instance).Grid))
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxControls.Get_TcxPageControl_Property(Instance: TObject;
  ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxPageControl);
{$ENDIF}
  if PropName = 'PAGECOUNT' then
    Result := TcxPageControl(Instance).PageCount
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxControls.Get_TcxTabs_Method(Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxTabs);
{$ENDIF}
  if MethodName = 'TABS.GET' then
    Result := Integer(TcxTab(TcxTabs(Instance).Tabs[Caller.Params[0]]))
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxControls.Get_TcxTab_Property(Instance: TObject;
  ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxTab);
{$ENDIF}
  if PropName = 'VISIBLE' then
    Result := TcxTab(Instance).Visible
  else if PropName = 'CAPTION' then
    Result := TcxTab(Instance).Caption
  else if PropName = 'ENABLED' then
    Result := TcxTab(Instance).Enabled
  else if PropName = 'IMAGEINDEX' then
    Result := TcxTab(Instance).ImageIndex
  else if PropName = 'INDEX' then
    Result := TcxTab(Instance).Index
  else if PropName = 'SELECTED' then
    Result := TcxTab(Instance).Selected
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxControls.Get_TcxCustomLookupDBGrid_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomLookupDBGrid);
{$ENDIF}
  if PropName = 'COLUMNS' then
    Result := Integer(TcxLookupDBGridColumns(TcxCustomLookupDBGrid(Instance).Columns))
  else if PropName = 'DATACONTROLLER' then
    Result := Integer(TcxLookupGridDBDataController(TcxCustomLookupDBGrid(Instance).DataController))
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TFunctions_TcxControls.Set_TcxCustomLookupDBGrid_Property(
  Instance: TObject; ClassType: TClass; const PropName: String;
  Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomLookupDBGrid);
{$ENDIF}
  if PropName = 'COLUMNS' then
    TcxCustomLookupDBGrid(Instance).Columns := TcxLookupDBGridColumns(Integer(Value))
  else if PropName = 'DATACONTROLLER' then
    TcxCustomLookupDBGrid(Instance).DataController := TcxLookupGridDBDataController(Integer(Value))
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TFunctions_TcxControls.Set_TcxTab_Property(Instance: TObject;
  ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxTab);
{$ENDIF}
  if PropName = 'VISIBLE' then
    TcxTab(Instance).Visible := Value
  else if PropName = 'CAPTION' then
    TcxTab(Instance).Caption := Value
  else if PropName = 'ENABLED' then
    TcxTab(Instance).Enabled := Value
  else if PropName = 'SELECTED' then
    TcxTab(Instance).Selected := Value
  else if PropName = 'IMAGEINDEX' then
    TcxTab(Instance).ImageIndex := Value
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxControls.Call_TcxEditRepository_Method(Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxEditRepository);
{$ENDIF}
  if MethodName = 'ITEMBYNAME' then
    Result := Integer(TcxEditRepositoryItem(TcxEditRepository(Instance).ItemByName(Caller.Params[0])))
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxControls.Call_TcxLookupDBGridColumns_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxLookupDBGridColumns);
{$ENDIF}
  if MethodName = 'COLUMNBYFIELDNAME' then
    Result := Integer(TcxLookupDBGridColumn(TcxLookupDBGridColumns(Instance).ColumnByFieldName(Caller.Params[0])))
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxControls.Call_TcxNamedImageCollection_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'FINDPICTURE' then
    Result := Integer(TcxNamedImageCollection(Instance).FindPicture(Caller.Params[0]))
end;

function TFunctions_TcxControls.Call_TcxPageControl_Method(Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxPageControl);
{$ENDIF}
  if MethodName = 'PAGES.GET' then
    Result := Integer(TcxPageControl(Instance).Pages[Caller.Params[0]])
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxControls.Call_TcxRichEdit_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  LFontStyles: TFontStyles;
begin
  if MethodName = 'SETFONTSTYLE' then
    TcxRichEdit(Instance).SetFontStyle(Caller.Params[0], Caller.Params[1])
  else if MethodName = 'GETHTMLTEXT' then
    Result := TcxRichEdit(Instance).GetHTMLText
  else if MethodName = 'GETRTF' then
    Result := TcxRichEdit(Instance).GetRTF
  else if MethodName = 'ADDFORMATEDTEXTATBEGIN' then begin
    VarToSet(Caller.Params[4], LFontStyles, SizeOf(TFontStyles));
    TcxRichEdit(Instance).AddFormatedTextAtBegin(Caller.Params[0], Caller.Params[1], Caller.Params[2], Caller.Params[3], LFontStyles);
  end
end;

function TFunctions_TcxControls.Get_TcxRichEdit_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'SELATTRIBUTES' then
    Result := Integer((Instance as TcxRichEdit).SelAttributes);
end;

{ TfsCxEditButtonClickEvent }

procedure TfsCxEditButtonClickEvent.DoEvent(Sender: TObject; AButtonIndex: Integer);
begin
  CallHandler([Sender, AButtonIndex]);
end;

function TfsCxEditButtonClickEvent.GetMethod: Pointer;
begin
  Result := @TfsCxEditButtonClickEvent.DoEvent;
end;

{ TfsCxEditValidateEvent }

procedure TfsCxEditValidateEvent.DoEvent(Sender: TObject; var DisplayValue: Variant; var ErrorText: String; var Error: Boolean);
begin
  CallHandler( [Sender, DisplayValue, ErrorText, Error] );
  DisplayValue  := Handler.Params[1].Value;
  ErrorText     := Handler.Params[2].Value;
  Error         := Handler.Params[3].Value = True;
end;

function TfsCxEditValidateEvent.GetMethod: Pointer;
begin
  Result := @TfsCxEditValidateEvent.DoEvent;
end;

{ TfsCxRichEditPropertiesUrlClick }

procedure TfsCxRichEditPropertiesUrlClick.DoEvent(ASender: TcxCustomRichEdit; const AURLText: string; AButton: TMouseButton);
begin
  CallHandler([ASender, AURLText, Integer(AButton)]);
end;

function TfsCxRichEditPropertiesUrlClick.GetMethod: Pointer;
begin
  Result := @TfsCxRichEditPropertiesUrlClick.DoEvent;
end;

initialization
  fsRTTIModules.Add(TFunctions_TcxControls);

finalization
  fsRTTIModules.Remove(TFunctions_TcxControls);

end.
