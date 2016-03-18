unit cxMDRadioGroup;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI6}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Controls, Graphics, StdCtrls,
  Forms, Math, dxCore, cxClasses, cxControls, cxContainer, cxGraphics,
  cxDataStorage, cxDataUtils, cxVariants, cxEdit, cxEditUtils,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxRadioGroup, cxGeometry,
  cxExtEditUtils, cxLookAndFeels, cxExtEditConsts,
  cxLookAndFeelPainters, DB, cxDBEdit, dxCoreClasses;

type
  TDataEvents = set of TDataEvent;

  TcxCustomMDRadioGroupProperties = class;

  TcxCustomMDRadioGroupPropertiesListLink = class(TDataLink)
  private
    FBusy: Boolean;
    FProperties: TcxCustomMDRadioGroupProperties;
  protected
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    procedure ActiveChanged; override;
    procedure UpdateData; override;
    procedure DataSetRecordsChanged;
  public
    constructor Create(AProperties: TcxCustomMDRadioGroupProperties); reintroduce;
  end;

  TcxCustomMDRadioGroupProperties = class(TcxCustomRadioGroupProperties)
  private
    FListLink: TDataLink;
    FKeyField: String;
    FListField: String;
//    FDisplayField: String;
    function GetListSource: TDataSource;
//    function IsDefaultDescriptionStored: Boolean;
//    procedure SetDisplayField(const Value: String);
//    procedure SetEmptySelectionText(const Value: string);
    procedure SetKeyField(const Value: String);
    procedure SetListField(const Value: String);
    procedure SetListSource(const Value: TDataSource);
  protected
    procedure ListLinkChanged;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

//    property EmptySelectionText : string read FEmptySelectionText write SetEmptySelectionText stored IsEmptySelectionTextStored;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property KeyField: String read FKeyField write SetKeyField;
    property ListField: String read FListField write SetListField;
//    property DisplayField: String read FDisplayField write SetDisplayField;
//    property DefaultDescription stored IsDefaultDescriptionStored;
  end;

  { TcxMDRadioGroupProperties }

  TcxMDRadioGroupProperties = class(TcxCustomMDRadioGroupProperties)
  private
  published
    property Alignment;
    property AssignedValues;
    property Columns;
    property ClearKey;
    property ImmediatePost;
    property Items;
    property ReadOnly;
    property ValidateOnEnter;
    property OnButtonClick;
    property OnChange;
    property OnEditValueChanged;
    property OnValidate;

    property DefaultValue;

//    property EmptySelectionText;
    property ListSource;
    property KeyField;
    property ListField;
//    property DisplayField;
  end;

  { TcxMDRadioGroup }

  TcxMDRadioGroup = class(TcxCustomRadioGroup)
  private
    FAlwaysSelected: Boolean;
    function GetActiveProperties: TcxMDRadioGroupProperties;
    function GetProperties: TcxMDRadioGroupProperties;
    procedure SetProperties(Value: TcxMDRadioGroupProperties);
    function GetAlwaysSelected: Boolean;
    procedure SetAlwaysSelected(Value: Boolean);
  protected
    procedure PropertiesChanged(Sender: TObject); override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxMDRadioGroupProperties read GetActiveProperties;
  published
    property Properties: TcxMDRadioGroupProperties read GetProperties write SetProperties;

    property ItemIndex;
    property AlwaysSelected: Boolean read GetAlwaysSelected write SetAlwaysSelected;

    property Anchors;
    property AutoSize;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
    property PanelStyle;
    property Padding;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property EditValue;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
{$IFDEF DELPHI5}
    property OnContextPopup;
{$ENDIF}
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

  TcxDBMDRadioGroup = class(TcxCustomRadioGroup)
    function GetActiveProperties: TcxMDRadioGroupProperties;
    function GetDataBinding: TcxDBEditDataBinding;
    function GetProperties: TcxMDRadioGroupProperties;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure SetProperties(Value: TcxMDRadioGroupProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxMDRadioGroupProperties read GetActiveProperties;
  published
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
    property Properties: TcxMDRadioGroupProperties read GetProperties write SetProperties;

    property ItemIndex;

    property Anchors;
    property AutoSize;
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
    property PanelStyle;
    property Padding;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
//    property EditValue;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
{$IFDEF DELPHI5}
    property OnContextPopup;
{$ENDIF}
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

  procedure Register;

const
  scxSEditRepositoryMDRadioGroup: String = 'MDRadioGroup|Represents an MDRadioGroup';

implementation

uses
  dxThemeManager;

type
  TcxCustomRadioGroupCrack = class(TcxCustomRadioGroup);

procedure Register;
begin
  RegisterComponents ('MD Project Tools', [TcxMDRadioGroup, TcxDBMDRadioGroup]);
end;

{ TcxCustomMDRadioGroupPropertiesListLink }

constructor TcxCustomMDRadioGroupPropertiesListLink.Create(AProperties: TcxCustomMDRadioGroupProperties);
begin
  FProperties := AProperties;
  inherited Create;
  VisualControl := True;
end;

procedure TcxCustomMDRadioGroupPropertiesListLink.DataEvent(Event: TDataEvent; Info: NativeInt);
//var
//  LEnableEvents: TDataEvents;
begin
  inherited;
  if (Event in [deDataSetChange, deLayoutChange]) then
    DataSetRecordsChanged;
{
  if (Event in [deDataSetChange, deLayoutChange]) and (Info <> 0) then begin
    LEnableEvents := [];
    PWord(@LEnableEvents)^ := Info;
    if (LEnableEvents * [deFieldChange, deFieldListChange]) <> [] then
      DataSetRecordsChanged;
  end;
}
end;

procedure TcxCustomMDRadioGroupPropertiesListLink.DataSetRecordsChanged;
begin
  if (not FBusy) and (not Editing) then begin
    FBusy := True;
    try
      FProperties.ListLinkChanged;
    finally
      FBusy := False;
    end;
  end;
end;

procedure TcxCustomMDRadioGroupPropertiesListLink.ActiveChanged;
begin
  DataSetRecordsChanged;
end;

procedure TcxCustomMDRadioGroupPropertiesListLink.UpdateData;
begin
  DataSetRecordsChanged;
end;

{ TcxCustomMDRadioGroupProperties }

constructor TcxCustomMDRadioGroupProperties.Create(AOwner: TPersistent);
begin
  inherited;

//  FEmptySelectionText := cxGetResourceString(@cxSImageComboBoxEmptySelectionText);
//  DefaultDescription := cxGetResourceString(@cxSImageComboBoxEmptySelectionText);
  FListLink := TcxCustomMDRadioGroupPropertiesListLink.Create(Self);
end;

destructor TcxCustomMDRadioGroupProperties.Destroy;
begin
  FreeAndNil(FListLink);
  inherited;
end;

procedure TcxCustomMDRadioGroupProperties.Assign(Source: TPersistent);
begin
  if Source is TcxCustomMDRadioGroupProperties then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      with TcxCustomMDRadioGroupProperties(Source) do
      begin
        Self.ListSource := ListSource;
        Self.KeyField := KeyField;
        Self.ListField := ListField;
//        Self.DisplayField := DisplayField;
//        Self.EmptySelectionText := EmptySelectionText;
      end;
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

function TcxCustomMDRadioGroupProperties.GetListSource: TDataSource;
begin
  Result := FListLink.DataSource;
end;

procedure TcxCustomMDRadioGroupProperties.SetListSource(const Value: TDataSource);
begin
  FListLink.DataSource := Value;
end;
(*
function TcxCustomMDRadioGroupProperties.IsDefaultDescriptionStored: Boolean;
begin
  Result := DefaultDescription <> cxGetResourceString(@cxSImageComboBoxEmptySelectionText);
end;
*)
procedure TcxCustomMDRadioGroupProperties.ListLinkChanged;
var
  LKeyField: TField;
  LListField: TField;
//  LDisplayField: TField;
  LBookmark: TBookmark;
begin
  BeginUpdate;
  try
//    Items.BeginUpdate;
//    try
      Items.Clear;
      if (FListLink.DataSet <> nil) and (FListLink.DataSet.RecordCount > 0) and (FKeyField <> '') then
        with FListLink.DataSet do begin
          LKeyField   := FieldByName(FKeyField);
          LListField  := FieldByName(FListField);
  //        LDisplayField := FindField(FDisplayField);
          DisableControls;
          LBookmark := Bookmark;
          try
            First;
            while not Eof do begin
              with Items.Add do begin
                Value       := LKeyField.AsVariant;
                if LListField <> nil then
                  Caption := LListField.AsString;
  //              if LDisplayField <> nil then
  //                Description := LDisplayField.AsString
              end;
              Next;
            end;
          finally
            Bookmark := LBookmark;
            EnableControls;
          end;
        end;
//    finally
//      Items.EndUpdate;
//    end;
  finally
    EndUpdate;
  end;
end;

(*
procedure TcxCustomMDRadioGroupProperties.SetDisplayField(const Value: String);
begin
  if FDisplayField <> Value then begin
    FDisplayField := Value;
    ListLinkChanged;
  end;
end;

procedure TcxCustomMDRadioGroupProperties.SetEmptySelectionText(const Value: string);
begin
  if FEmptySelectionText <> Value then
  begin
    FEmptySelectionText := Value;
    Changed;
  end;
end;
*)

procedure TcxCustomMDRadioGroupProperties.SetKeyField(const Value: String);
begin
  if FKeyField <> Value then begin
    FKeyField := Value;
    ListLinkChanged;
  end;
end;

procedure TcxCustomMDRadioGroupProperties.SetListField(const Value: String);
begin
  if FListField <> Value then begin
    FListField := Value;
    ListLinkChanged;
  end;
end;

{ TcxMDRadioGroup }

function TcxMDRadioGroup.GetActiveProperties: TcxMDRadioGroupProperties;
begin
  Result := TcxMDRadioGroupProperties(InternalGetActiveProperties);
end;

function TcxMDRadioGroup.GetProperties: TcxMDRadioGroupProperties;
begin
  Result := TcxMDRadioGroupProperties(FProperties);
end;

class function TcxMDRadioGroup.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMDRadioGroupProperties;
end;

procedure TcxMDRadioGroup.PropertiesChanged(Sender: TObject);
begin
  inherited PropertiesChanged(Sender);
  if (AlwaysSelected) and (ItemIndex < 0) and (InternalButtons.Count > 0) then
    ItemIndex := 0;
end;

procedure TcxMDRadioGroup.SetProperties(Value: TcxMDRadioGroupProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxDBMDRadioGroup }

procedure TcxDBMDRadioGroup.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(GetcxDBEditDataLink(Self));
end;

function TcxDBMDRadioGroup.GetActiveProperties: TcxMDRadioGroupProperties;
begin
  Result := TcxMDRadioGroupProperties(InternalGetActiveProperties);
end;

function TcxDBMDRadioGroup.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

class function TcxDBMDRadioGroup.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

function TcxDBMDRadioGroup.GetProperties: TcxMDRadioGroupProperties;
begin
  Result := TcxMDRadioGroupProperties(FProperties);
end;

class function TcxDBMDRadioGroup.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMDRadioGroupProperties;
end;

procedure TcxDBMDRadioGroup.SetDataBinding(Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBMDRadioGroup.SetProperties(Value: TcxMDRadioGroupProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxMDRadioGroupProperties }

function TcxMDRadioGroup.GetAlwaysSelected: Boolean;
begin
  Result := FAlwaysSelected;
end;

procedure TcxMDRadioGroup.SetAlwaysSelected(Value: Boolean);
begin
  if FAlwaysSelected = Value then
    Exit;

  FAlwaysSelected := Value;
  if (FAlwaysSelected) and (Properties.Items.Count > 0) then
    if ItemIndex = -1 then
      ItemIndex := 0;
end;

initialization
  RegisterClasses([TcxMDRadioGroup, TcxDBMDRadioGroup, TcxMDRadioGroupProperties]);
  GetRegisteredEditProperties.Register(TcxMDRadioGroupProperties, scxSEditRepositoryMDRadioGroup);

finalization
  GetRegisteredEditProperties.Unregister(TcxMDRadioGroupProperties);
  UnRegisterClasses([TcxMDRadioGroup, TcxDBMDRadioGroup, TcxMDRadioGroupProperties]);

end.
