unit cxImageLookupComboBox;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI6}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Controls, Graphics, StdCtrls,
  Forms, Math, dxCore, cxClasses, cxControls, cxContainer, cxGraphics,
  cxDataStorage, cxDataUtils, cxVariants, cxEdit, cxEditUtils,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox, cxGeometry,
  cxExtEditUtils, cxLookAndFeels, cxExtEditConsts,
  cxLookAndFeelPainters, DB, cxDBEdit, dxCoreClasses;

type
  TDataEvents = set of TDataEvent;

  TcxCustomImageLookupComboBoxProperties = class;

  TcxCustomImageLookupComboBoxPropertiesListLink = class(TDataLink)
  private
    FBusy: Boolean;
    FProperties: TcxCustomImageLookupComboBoxProperties;
  protected
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    procedure ActiveChanged; override;
    procedure UpdateData; override;
    procedure DataSetRecordsChanged;
  public
    constructor Create(AProperties: TcxCustomImageLookupComboBoxProperties); reintroduce;
  end;

  TcxCustomImageLookupComboBoxProperties = class(TcxCustomImageComboBoxProperties)
  private
//    FEmptySelectionText: string;
    FListLink: TDataLink;
    FKeyField: String;
    FImageField: String;
    FListField: String;
//    FDisplayField: String;
    function GetListSource: TDataSource;
//    function IsDefaultDescriptionStored: Boolean;
//    procedure SetDisplayField(const Value: String);
//    procedure SetEmptySelectionText(const Value: string);
    procedure SetKeyField(const Value: String);
    procedure SetListField(const Value: String);
    procedure SetListSource(const Value: TDataSource);
    procedure SetImageField(const Value: String);
  protected
    procedure ListLinkChanged;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

//    property EmptySelectionText : string read FEmptySelectionText write SetEmptySelectionText stored IsEmptySelectionTextStored;
    property ListSource: TDataSource read GetListSource write SetListSource;
    property KeyField: String read FKeyField write SetKeyField;
    property ImageField: String read FImageField write SetImageField;
    property ListField: String read FListField write SetListField;
//    property DisplayField: String read FDisplayField write SetDisplayField;
//    property DefaultDescription stored IsDefaultDescriptionStored;
  end;

  { TcxImageLookupComboBoxProperties }

  TcxImageLookupComboBoxProperties = class(TcxCustomImageLookupComboBoxProperties)
  published
    property Alignment;
    property AssignedValues;
    property ButtonGlyph;
    property ClearKey;
    property DefaultDescription;
    property DefaultImageIndex;
    property DropDownRows;
    property ImageAlign;
    property Images;
    property ImeMode;
    property ImeName;
    property ImmediateDropDownWhenActivated;
    property ImmediateDropDownWhenKeyPressed;
    property ImmediatePost;
    property ImmediateUpdateText;
//    property Items;
    property LargeImages;
    property MultiLineText;
    property PopupAlignment;
    property PostPopupValueOnTab;
    property ReadOnly;
    property Revertable;
    property ShowDescriptions;
    property ValidateOnEnter;
    property OnButtonClick;
    property OnChange;
    property OnCloseQuery;
    property OnCloseUp;
    property OnEditValueChanged;
    property OnInitPopup;
    property OnPopup;
    property OnValidate;

//    property EmptySelectionText;
    property ListSource;
    property KeyField;
    property ImageField;
    property ListField;
//    property DisplayField;
  end;

  { TcxImageLookupComboBox }

  TcxImageLookupComboBox = class(TcxCustomImageComboBox)
  private
    function GetActiveProperties: TcxImageLookupComboBoxProperties;
    function GetProperties: TcxImageLookupComboBoxProperties;
    procedure SetProperties(Value: TcxImageLookupComboBoxProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxImageLookupComboBoxProperties read GetActiveProperties;
  published
    property Properties: TcxImageLookupComboBoxProperties read GetProperties write SetProperties;

    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
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

  TcxDBImageLookupComboBox = class(TcxCustomImageComboBox)
  private
    function GetActiveProperties: TcxImageLookupComboBoxProperties;
    function GetDataBinding: TcxDBEditDataBinding;
    function GetProperties: TcxImageLookupComboBoxProperties;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure SetProperties(Value: TcxImageLookupComboBoxProperties);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxImageLookupComboBoxProperties read GetActiveProperties;
  published
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
    property Properties: TcxImageLookupComboBoxProperties read GetProperties write SetProperties;

    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
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
  scxSEditRepositoryImageLookupComboBox: String = 'ImageLookupComboBox|Represents an ImageLookupComboBox';

implementation

uses
  dxThemeManager, SBaseStringFunctions;

procedure Register;
begin
  RegisterComponents ('MD Project Tools', [TcxImageLookupComboBox, TcxDBImageLookupComboBox]);
end;

{ TcxCustomImageLookupComboBoxPropertiesListLink }

constructor TcxCustomImageLookupComboBoxPropertiesListLink.Create(AProperties: TcxCustomImageLookupComboBoxProperties);
begin
  FProperties := AProperties;
  inherited Create;
  VisualControl := True;
end;

procedure TcxCustomImageLookupComboBoxPropertiesListLink.DataEvent(Event: TDataEvent; Info: NativeInt);
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

procedure TcxCustomImageLookupComboBoxPropertiesListLink.DataSetRecordsChanged;
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

procedure TcxCustomImageLookupComboBoxPropertiesListLink.ActiveChanged;
begin
  DataSetRecordsChanged;
end;

procedure TcxCustomImageLookupComboBoxPropertiesListLink.UpdateData;
begin
  DataSetRecordsChanged;
end;

{ TcxCustomImageLookupComboBoxProperties }

constructor TcxCustomImageLookupComboBoxProperties.Create(AOwner: TPersistent);
begin
  inherited;

  DropDownListStyle := lsEditFixedList;
//  FEmptySelectionText := cxGetResourceString(@cxSImageComboBoxEmptySelectionText);
//  DefaultDescription := cxGetResourceString(@cxSImageComboBoxEmptySelectionText);
  FListLink := TcxCustomImageLookupComboBoxPropertiesListLink.Create(Self);
end;

destructor TcxCustomImageLookupComboBoxProperties.Destroy;
begin
  FreeAndNil(FListLink);
  inherited;
end;

procedure TcxCustomImageLookupComboBoxProperties.Assign(Source: TPersistent);
begin
  if Source is TcxCustomImageLookupComboBoxProperties then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      with TcxCustomImageLookupComboBoxProperties(Source) do
      begin
        Self.ListSource := ListSource;
        Self.KeyField := KeyField;
        Self.ImageField := ImageField;
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

function TcxCustomImageLookupComboBoxProperties.GetListSource: TDataSource;
begin
  Result := FListLink.DataSource;
end;

procedure TcxCustomImageLookupComboBoxProperties.SetListSource(const Value: TDataSource);
begin
  FListLink.DataSource := Value;
end;
(*
function TcxCustomImageLookupComboBoxProperties.IsDefaultDescriptionStored: Boolean;
begin
  Result := DefaultDescription <> cxGetResourceString(@cxSImageComboBoxEmptySelectionText);
end;
*)
procedure TcxCustomImageLookupComboBoxProperties.ListLinkChanged;
var
  LKeyField: TField;
  LImageField: TField;
  LListField: TField;
//  LDisplayField: TField;
  LBookmark: TBookmark;
begin
  BeginUpdate;
  try
    Items.Clear;
    if (FListLink.DataSet <> nil) and (FListLink.DataSet.RecordCount > 0) and (FKeyField <> '') and (FImageField <> '') then
      with FListLink.DataSet do begin
        LKeyField   := FieldByName(FKeyField);
        LImageField := FieldByName(FImageField);
        LListField  := FieldByName(FListField);
//        LDisplayField := FindField(FDisplayField);
        DisableControls;
        LBookmark := Bookmark;
        try
          First;
          while not Eof do begin
            with Items.Add do begin
              ImageIndex  := LImageField.AsInteger;
              Value       := LKeyField.AsVariant;
              if LListField <> nil then
                Description := LListField.AsString;
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
  finally
    EndUpdate;
  end;
end;

(*
procedure TcxCustomImageLookupComboBoxProperties.SetDisplayField(const Value: String);
begin
  if FDisplayField <> Value then begin
    FDisplayField := Value;
    ListLinkChanged;
  end;
end;

procedure TcxCustomImageLookupComboBoxProperties.SetEmptySelectionText(const Value: string);
begin
  if FEmptySelectionText <> Value then
  begin
    FEmptySelectionText := Value;
    Changed;
  end;
end;
*)

procedure TcxCustomImageLookupComboBoxProperties.SetImageField(const Value: String);
begin
  if FImageField <> Value then begin
    FImageField := Value;
    ListLinkChanged;
  end;
end;

procedure TcxCustomImageLookupComboBoxProperties.SetKeyField(const Value: String);
begin
  if FKeyField <> Value then begin
    FKeyField := Value;
    ListLinkChanged;
  end;
end;

procedure TcxCustomImageLookupComboBoxProperties.SetListField(const Value: String);
begin
  if FListField <> Value then begin
    FListField := Value;
    ListLinkChanged;
  end;
end;

{ TcxImageLookupComboBox }

function TcxImageLookupComboBox.GetActiveProperties: TcxImageLookupComboBoxProperties;
begin
  Result := TcxImageLookupComboBoxProperties(InternalGetActiveProperties);
end;

function TcxImageLookupComboBox.GetProperties: TcxImageLookupComboBoxProperties;
begin
  Result := TcxImageLookupComboBoxProperties(FProperties);
end;

class function TcxImageLookupComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxImageLookupComboBoxProperties;
end;

procedure TcxImageLookupComboBox.SetProperties(Value: TcxImageLookupComboBoxProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxDBImageLookupComboBox }

procedure TcxDBImageLookupComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(GetcxDBEditDataLink(Self));
end;

function TcxDBImageLookupComboBox.GetActiveProperties: TcxImageLookupComboBoxProperties;
begin
  Result := TcxImageLookupComboBoxProperties(InternalGetActiveProperties);
end;

function TcxDBImageLookupComboBox.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

class function TcxDBImageLookupComboBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

function TcxDBImageLookupComboBox.GetProperties: TcxImageLookupComboBoxProperties;
begin
  Result := TcxImageLookupComboBoxProperties(FProperties);
end;

class function TcxDBImageLookupComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxImageLookupComboBoxProperties;
end;

procedure TcxDBImageLookupComboBox.SetDataBinding(Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxDBImageLookupComboBox.SetProperties(Value: TcxImageLookupComboBoxProperties);
begin
  FProperties.Assign(Value);
end;

initialization
  RegisterClasses([TcxImageLookupComboBoxProperties]);
  GetRegisteredEditProperties.Register(TcxImageLookupComboBoxProperties, scxSEditRepositoryImageLookupComboBox);

finalization
  GetRegisteredEditProperties.Unregister(TcxImageLookupComboBoxProperties);
  UnRegisterClasses([TcxImageLookupComboBoxProperties]);

end.
