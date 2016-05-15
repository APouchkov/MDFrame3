unit cxEditedFieldCheckBox;

interface
{$I CONFIG.INC}

uses
{$IFDEF DELPHI6}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Controls, Graphics, StdCtrls,
  Forms, Math, cxClasses, cxControls, cxContainer, cxGraphics,
  cxDataStorage, cxDataUtils, cxVariants, cxEdit, cxEditUtils,
  cxTextEdit, cxMaskEdit, cxLookAndFeels, cxLookAndFeelPainters, DB, cxDB, cxDBEdit,
  cxCheckBox, dxCoreClasses, Variants;

type
  TcxCustomEditedFieldCheckBox = class;
  TcxDataBindingPair = record
    DataSource: TDataSource;
    DataField : String;
    function DataSet: TDataSet;
    function Field: TField;
  end;

  TcxCustomDBEditedFieldDataBinding = class(TcxDBEditDataBinding)
  private
    function GetEdit: TcxCustomEditedFieldCheckBox; inline;
//    FLinkedFieldName: String;
  public
//    constructor Create(AOwner, ADataComponent: TComponent); override;
//    destructor Destroy; override;

//    function GetLinkedField: TField;
    function CanModify: Boolean; override;
//    function IsControlReadOnly: Boolean; override;

//    function IsDataSourceLive: Boolean; override;
//    function IsDataStorage: Boolean; override;
//    function ExecuteAction(Action: TBasicAction): Boolean; override;

    function GetStoredValue: Variant; override;
    procedure SetStoredValue(const Value: TcxEditValue); override;

//    procedure Reset; override;
//    function SetEditMode: Boolean; override;
//    function UpdateAction(Action: TBasicAction): Boolean; override;
//    procedure UpdateDataSource; override;

//    property Field: TField read GetField;
//    property DataSource: TDataSource read GetDataSource write SetDataSource;
//    property DataField: String read GetDataField write SetDataField;
//    property DataLink: TcxCustomFieldDataLink read FDataLink;

//    property LinkedField: String read FLinkedFieldName write FLinkedFieldName;
  end;

  TcxDBEditedFieldDataBinding = class(TcxCustomDBEditedFieldDataBinding)
//  published
//    property LinkedField;
  end;

  TcxCustomEditedFieldCheckBoxProperties = class(TcxCustomCheckBoxProperties)
  private
    FLinkedComponent : TComponent;

    FLinkedDataSource: TDataSource;
    FLinkedFieldName : String;

    function GetLinkedFieldPair: TcxDataBindingPair;
    function GetLinkedFieldName: String; inline;
//    function GetLinkedDataSource: TDataSource;
  public
    class function GetContainerClass: TcxContainerClass; override;

    constructor Create(AOwner: TPersistent); override;

    property LinkedComponent : TComponent read FLinkedComponent write FLinkedComponent;

    property LinkedDataSource: TDataSource read FLinkedDataSource write FLinkedDataSource;
    property LinkedFieldName : String read FLinkedFieldName write FLinkedFieldName;
  end;

  { TcxEditedFieldCheckBoxProperties }

  TcxEditedFieldCheckBoxProperties = class(TcxCustomEditedFieldCheckBoxProperties)
  published
    property Alignment;
//    property AllowGrayed;
    property AssignedValues;
//    property Caption; // obsolete
//    property ClearKey;

    property DisplayChecked;
    property DisplayUnchecked;
//    property DisplayGrayed;
//    property FullFocusRect;
//    property Glyph;
//    property GlyphCount;
    property ImmediatePost;
//    property MultiLine;
//    property NullStyle;
    property ReadOnly;
//    property ShowEndEllipsis;
//    property UseAlignmentWhenInplace;

//    property ValueChecked;
//    property ValueGrayed;
//    property ValueUnchecked;

    property OnChange;
    property OnEditValueChanged;
    property OnValidate;

    { -=MD=- }
    property LinkedComponent;
  end;

  { TcxCustomEditedFieldCheckBox }

  TcxCustomEditedFieldCheckBox = class(TcxCustomCheckBox)
  private
    function GetProperties: TcxCustomEditedFieldCheckBoxProperties;
    procedure SetProperties(Value: TcxCustomEditedFieldCheckBoxProperties);
    function GetActiveProperties: TcxCustomEditedFieldCheckBoxProperties;
  private
    function GetDataBinding: TcxCustomDBEditedFieldDataBinding;
    procedure SetDataBinding(Value: TcxCustomDBEditedFieldDataBinding);
//  protected
//    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  protected
//    procedure SetState(Value: TcxCheckBoxState); override;
//    function GetLinkedComponentCanModify: Boolean;
    procedure DoEditValueChanged; override;
    procedure PropertiesChanged(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent); override;

    property ActiveProperties: TcxCustomEditedFieldCheckBoxProperties read GetActiveProperties;
//    function IsRepositoryItemAcceptable(ARepositoryItem: TcxEditRepositoryItem): Boolean; override;
  published
    property DataBinding: TcxCustomDBEditedFieldDataBinding read GetDataBinding write SetDataBinding;
    property Properties: TcxCustomEditedFieldCheckBoxProperties read GetProperties write SetProperties;
  end;

  TcxEditedFieldCheckBox = class(TcxCustomEditedFieldCheckBox)
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Action;
    property Anchors;
    property AutoSize;
    property Caption;
    property Checked;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBackground;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Properties;
    property ShowHint;
    property State;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

  TcxDBEditedFieldCheckBox = class(TcxCustomEditedFieldCheckBox)
  private
    function GetDataBinding: TcxDBEditedFieldDataBinding;
    procedure SetDataBinding(Value: TcxDBEditedFieldDataBinding);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property Checked;
  published
    property DataBinding: TcxDBEditedFieldDataBinding read GetDataBinding write SetDataBinding;

    property Action;
    property Anchors;
    property AutoSize;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBackground;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Transparent;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEditing;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

implementation

uses
  ActnList, dxThemeManager, SBaseStringFunctions, SBaseVariantFunctions, cxTL, cxDBTL,
  cxGridCustomView, cxGridDBTableView, cxGridDBBandedTableView, SBaseForm, SBaseControls
{$IFDEF DATAHANDLER}
  , DataHandler
{$ENDIF}
  ;

const
  scxSEditRepositoryEditedFieldCheckBox: String = 'EditedFieldCheckBox|Represents a checkbox indicating that the field has been changed';

type
  TcxEditDataBindingCrack = class(TcxEditDataBinding);
  TcxCustomEditCrack = class(TcxCustomEdit);

{ TcxCustomEditedFieldCheckBoxProperties }

constructor TcxCustomEditedFieldCheckBoxProperties.Create(AOwner: TPersistent);
begin
  inherited;
end;

class function TcxCustomEditedFieldCheckBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxCustomEditedFieldCheckBox
end;

function TcxCustomEditedFieldCheckBoxProperties.GetLinkedFieldPair: TcxDataBindingPair;
var
  LcxEditDataBinding: TcxEditDataBinding;
{$IFDEF DATAHANDLER}
  LDataSource: TDataSource;
  LDataHandler: TSubDataHandler absolute LDataSource;
{$ENDIF}
begin
  Result.DataSource := FLinkedDataSource;
  Result.DataField  := FLinkedFieldName;
  if Assigned(FLinkedDataSource) or (not FLinkedFieldName.IsEmpty) then Exit;

  if FLinkedComponent is TcxCustomEdit then begin
    LcxEditDataBinding := TcxCustomEditCrack(FLinkedComponent).DataBinding;
    if LcxEditDataBinding is TcxDBEditDataBinding then begin
      Result.DataSource := TcxDBEditDataBinding(LcxEditDataBinding).DataSource;
      Result.DataField := TcxDBEditDataBinding(LcxEditDataBinding).DataField;
    end;
  end
{$IFDEF DATAHANDLER}
  else if FLinkedComponent is TcxDBTreeList then
    LDataSource := TcxDBTreeList(FLinkedComponent).DataController.DataSource
  else if FLinkedComponent is TcxGridDBTableView then
    LDataSource := TcxGridDBTableView(FLinkedComponent).DataController.DataSource
  else if FLinkedComponent is TcxGridDBBandedTableView then
    LDataSource := TcxGridDBBandedTableView(FLinkedComponent).DataController.DataSource
  else
    Exit;

  if not Assigned(LDataSource) or not (LDataSource is TSubDataHandler) then Exit;
  Result.DataSource := LDataHandler.Parent;
  Result.DataField := LDataHandler.XMLPost.FieldName;
{$ENDIF}
end;

function TcxCustomEditedFieldCheckBoxProperties.GetLinkedFieldName: String;
begin
  Result := GetLinkedFieldPair.DataField
end;

//function TcxCustomEditedFieldCheckBoxProperties.GetLinkedDataSource: TDataSource;
//var
//  LcxEditDataBinding: TcxEditDataBinding;
//begin
//  if not Assigned(FLinkedComponent) then
//    Exit(nil);
//
//  if FLinkedComponent is TcxCustomEdit then begin
//    LcxEditDataBinding := TcxCustomEditCrack(FLinkedComponent).DataBinding;
//    if LcxEditDataBinding is TcxCustomDBDataBinding then
//      Exit(TcxCustomDBDataBinding(LcxEditDataBinding).DataSource);
//  end;
//end;

{ TcxCustomEditedFieldCheckBox }

constructor TcxCustomEditedFieldCheckBox.Create(AOwner: TComponent);
begin
{$IFDEF DEBUG}
  Assert(Assigned(AOwner) and (AOwner is TSBaseFrm));
{$ENDIF}

  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
end;

procedure TcxCustomEditedFieldCheckBox.DoEditValueChanged;
var
  LLinkedComponent: TComponent;
  LValue: Boolean;
begin
  if IsLoading or IsDestroying then
    Exit;

  inherited;
  LLinkedComponent := ActiveProperties.LinkedComponent;
  if Assigned(LLinkedComponent) then begin
    LValue := VarToBoolDef(EditValue, False);
    TControlStateRules.CreateRules(LLinkedComponent).ReadOnly := (not LValue) or (not DataBinding.CanModify);
    if not TSBaseFrm(Owner).Loading then begin
      TSBaseFrm(Owner).UpdateControlState(LLinkedComponent);
    end;
  end;
end;

function TcxCustomEditedFieldCheckBox.GetActiveProperties: TcxCustomEditedFieldCheckBoxProperties;
begin
  Result := TcxCustomEditedFieldCheckBoxProperties(InternalGetActiveProperties);
end;

function TcxCustomEditedFieldCheckBox.GetDataBinding: TcxCustomDBEditedFieldDataBinding;
begin
  Result := TcxCustomDBEditedFieldDataBinding(inherited DataBinding)
end;

//function TcxCustomEditedFieldCheckBox.GetLinkedComponentCanModify: Boolean;
//var
//  LDataSource: TDataSource;
//  LDataSet: TDataSet;
//  LLinkedFieldName: String;
//  LField: TField;
//begin
//  Result := False;
//
//  if not Assigned(ActiveProperties.LinkedComponent) then Exit;
//
//  LLinkedFieldName := ActiveProperties.LinkedFieldName;
//  if (LLinkedFieldName.IsEmpty) then Exit;
//
//  LDataSource := DataBinding.DataSource;
//  if not Assigned(LDataSource) then Exit;
//
//  LDataSet := LDataSource.DataSet;
//  if not Assigned(LDataSet) then Exit;
//
//  LField := LDataSet.FindField(LLinkedFieldName);
//  Result := Assigned(LField) and LField.CanModify;
//{$IFDEF DATAHANDLER}
//  Result := Result and (not (LDataSource is TCustomDataHandler)) or TCustomDataHandler(LDataSource).CanModify);
//{$ENDIF}
//end;

function TcxCustomEditedFieldCheckBox.GetProperties: TcxCustomEditedFieldCheckBoxProperties;
begin
  Result := TcxCustomEditedFieldCheckBoxProperties(FProperties);
end;

procedure TcxCustomEditedFieldCheckBox.PropertiesChanged(Sender: TObject);
begin
  inherited;
end;

//function TcxCustomEditedFieldCheckBox.IsRepositoryItemAcceptable(ARepositoryItem: TcxEditRepositoryItem): Boolean;
//begin
//  Result := (inherited IsRepositoryItemAcceptable(ARepositoryItem))
//        or
//        ARepositoryItem.Properties.InheritsFrom(TcxCustomCheckBoxProperties);
//end;

procedure TcxCustomEditedFieldCheckBox.SetDataBinding(Value: TcxCustomDBEditedFieldDataBinding);
begin
  inherited DataBinding := Value
end;

procedure TcxCustomEditedFieldCheckBox.SetProperties(Value: TcxCustomEditedFieldCheckBoxProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxDBEditedFieldCheckBox }

procedure TcxDBEditedFieldCheckBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(GetcxDBEditDataLink(Self));
end;

function TcxDBEditedFieldCheckBox.GetDataBinding: TcxDBEditedFieldDataBinding;
begin
  Result := TcxDBEditedFieldDataBinding(FDataBinding);
end;

class function TcxDBEditedFieldCheckBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditedFieldDataBinding;
end;

class function TcxDBEditedFieldCheckBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxEditedFieldCheckBoxProperties;
end;

procedure TcxDBEditedFieldCheckBox.SetDataBinding(Value: TcxDBEditedFieldDataBinding);
begin
  FDataBinding.Assign(Value);
end;

{ TcxCustomDBEditedFieldDataBinding }

function TcxCustomDBEditedFieldDataBinding.CanModify: Boolean;
var
  LLinkedField: TField;
{$IFDEF DATAHANDLER}
  LDataSource: TDataSource;
{$ENDIF}
begin
  Result := (inherited CanModify);
{$IFDEF DATAHANDLER}
  if Result then begin
    LDataSource := DataSource;
    Result := (not (LDataSource is TCustomDataHandler)) or TCustomDataHandler(LDataSource).CanModify;
{$ENDIF}
    if Result then begin
      LLinkedField := GetEdit.ActiveProperties.GetLinkedFieldPair.Field;
      Result := Assigned(LLinkedField) and LLinkedField.CanModify
    end;
{$IFDEF DATAHANDLER}
  end;
{$ENDIF}
end;

function TcxCustomDBEditedFieldDataBinding.GetEdit: TcxCustomEditedFieldCheckBox;
begin
  Result := TcxCustomEditedFieldCheckBox(FEdit);
end;

//function TcxCustomDBEditedFieldDataBinding.IsControlReadOnly: Boolean;
//var
//  LLinkedField: TField;
//begin
//  Result := (inherited ReadOnly);
//  if (not Result) then begin
//    LLinkedField := GetLinkedField;
//    Result := (not Assigned(LLinkedField)) or LLinkedField.ReadOnly
//  end;
//end;

function TcxCustomDBEditedFieldDataBinding.GetStoredValue: Variant;
var
  LEditedFieldNames: String;
  LValue: Boolean;
var
  LEditValueSource: TcxDataEditValueSource;
begin
  LEditValueSource := Edit.ActiveProperties.GetEditValueSource(Edit.InternalFocused);
  if not IsDataAvailable then
    Result := (inherited GetStoredValue)
  else begin
    LEditedFieldNames := Field.AsString;
    LValue := InArray(LEditedFieldNames, GetEdit.ActiveProperties.GetLinkedFieldName, ',');

    case LEditValueSource of
      evsText:
        Result := BoolToStr(LValue);
      else
        Result := LValue;
    end;
  end;
end;

procedure TcxCustomDBEditedFieldDataBinding.SetStoredValue(const Value: TcxEditValue);
var
  LValue: Boolean;
  LLinkedFieldName: String;
  LEditedFieldNames: String;
begin
  if IsDataAvailable then begin
    LValue := VarToBoolDef(Value, False);

    LEditedFieldNames := Field.AsString;
    LLinkedFieldName := GetEdit.ActiveProperties.GetLinkedFieldName;
    if LValue then
      LEditedFieldNames := ArraysMerge([LEditedFieldNames, LLinkedFieldName], ',')
    else
      LEditedFieldNames := ArraysAntiJoin(LEditedFieldNames, LLinkedFieldName, ',');

    if LEditedFieldNames.IsEmpty then
      inherited SetStoredValue(Null)
    else
      inherited SetStoredValue(LEditedFieldNames);
  end;
end;

{ TcxEditedFieldCheckBox }

class function TcxEditedFieldCheckBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxEditedFieldCheckBoxProperties;
end;

{ TcxDataBindingPair }

function TcxDataBindingPair.DataSet: TDataSet;
begin
  if Assigned(DataSource) then
    Result := DataSource.DataSet
  else
    Result := nil
end;

function TcxDataBindingPair.Field: TField;
var
  LDataSet: TDataSet;
begin
  if DataField.IsEmpty then Exit(nil);

  LDataSet := DataSet;
  if not Assigned(LDataSet) then Exit(nil);

  Result := LDataSet.FindField(DataField)
end;

initialization
  RegisterClasses([TcxDBEditedFieldDataBinding, TcxEditedFieldCheckBoxProperties, TcxEditedFieldCheckBox, TcxDBEditedFieldCheckBox]);
//  GetRegisteredEditProperties.Register(TcxEditedFieldCheckBoxProperties, scxSEditRepositoryEditedFieldCheckBox);

finalization
//  GetRegisteredEditProperties.Unregister(TcxEditedFieldCheckBoxProperties);
  UnRegisterClasses([TcxDBEditedFieldDataBinding, TcxEditedFieldCheckBoxProperties, TcxEditedFieldCheckBox, TcxDBEditedFieldCheckBox]);

end.
