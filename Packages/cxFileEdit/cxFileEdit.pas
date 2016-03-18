unit cxFileEdit;

interface

{$I cxVer.inc}

uses
{$IFDEF DELPHI6}
  Variants,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Controls, Graphics, StdCtrls,
  Forms, Math, cxClasses, cxControls, cxContainer, cxGraphics,
  cxDataStorage, cxDataUtils, cxVariants, cxEdit, cxEditUtils,
  cxTextEdit, cxMaskEdit, cxLookAndFeels, cxLookAndFeelPainters, DB, cxDBEdit,
  cxButtonEdit, Dialogs;

type
  TfeAction = (feaFile, feaDir, feaClear);

  TcxFileEditButton = class(TcxEditButton)
  private
    FAction: TfeAction;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property EditAction: TfeAction read FAction write FAction default feaFile;
  end;

  TcxFileEditButtons = class(TcxEditButtons)
  private
    function GetItem(Index: Integer): TcxFileEditButton;
    procedure SetItem(Index: Integer; Value: TcxFileEditButton);
  protected
    class function GetButtonClass: TcxEditButtonClass; override;
  public
    function Add: TcxFileEditButton;
    //
    property Items[Index: Integer]: TcxFileEditButton read GetItem write SetItem; default;
  end;

  TcxCustomFileEditProperties = class(TcxCustomButtonEditProperties)
  private
    FOpenDialog: TFileOpenDialog;
    FFileTypeItems: TFileTypeItems;

    function GetDefaultExtension: string;
    procedure SetDefaultExtension(const Value: string);

    function GetOptions: TFileDialogOptions;
    procedure SetOptions(const Value: TFileDialogOptions);

    function GetTitle: string;
    procedure SetTitle(const Value: string);

//    function GetFileTypes: TFileTypeItems;
    procedure SetFileTypes(const Value: TFileTypeItems);

    function  GetFileTypeIndex: Cardinal;
    procedure SetFileTypeIndex(const Value: Cardinal);

    function GetDefaultFolder: string;
    procedure SetDefaultFolder(const Value: string);
    function IsDialogFileTypesStored: Boolean;
    function GetButtons: TcxFileEditButtons;
    procedure SetButtons(const Value: TcxFileEditButtons);
  public
    class function GetButtonsClass: TcxEditButtonsClass; override;
    class function GetContainerClass: TcxContainerClass; override;

    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Buttons: TcxFileEditButtons read GetButtons write SetButtons;

    property DialogDefaultFolder: string read GetDefaultFolder write SetDefaultFolder;
    property DialogDefaultExtension: string read GetDefaultExtension write SetDefaultExtension;

    property DialogFileTypes: TFileTypeItems read FFileTypeItems write SetFileTypes stored IsDialogFileTypesStored;
    property DialogFileTypeIndex: Cardinal read GetFileTypeIndex write SetFileTypeIndex default 1;

    property DialogTitle: string read GetTitle write SetTitle;
    property DialogOptions: TFileDialogOptions read GetOptions write SetOptions default [];
  end;

  { TcxFileEditProperties }

  TcxFileEditProperties = class(TcxCustomFileEditProperties)
  published
    property Alignment;
    property AssignedValues;
    property AutoSelect;
    property BeepOnError;
    property Buttons;
    property CaseInsensitive;
    property CharCase;
    property ClearKey;
    property ClickKey;
    property EchoMode;
    property HideCursor; // deprecated
    property HideSelection;
//    property IgnoreMaskBlank;
    property Images;
    property ImeMode;
    property ImeName;
//    property IncrementalSearch;
    property LookupItems;
    property LookupItemsSorted;
    property MaskKind;
    property Nullstring;
    property EditMask;
    property MaxLength;
    property OEMConvert;
//    property PasswordChar;
    property ReadOnly;
    property UseLeftAlignmentOnEditing;
    property UseNullString;
    property ValidateOnEnter;
    property ViewStyle;
//    property OnButtonClick;
    property OnChange;
    property OnEditValueChanged;
    property OnNewLookupDisplayText;
    property OnValidate;

    property DialogDefaultExtension;
    property DialogFileTypes;
    property DialogFileTypeIndex;
//    property DialogDefaultFolder;
    property DialogOptions;
//    property DialogOptionsEx;
    property DialogTitle;
  end;

  { TcxFileEdit }

  TcxCustomFileEdit = class(TcxCustomButtonEdit)
  private
    function GetProperties: TcxFileEditProperties;
    procedure SetProperties(Value: TcxFileEditProperties);
    function GetActiveProperties: TcxFileEditProperties;
  protected
    procedure DoButtonClick(AButtonVisibleIndex: Integer); override;
  public
//    constructor Create(AOwner: TComponent); override;
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;

    property Properties: TcxFileEditProperties read GetProperties write SetProperties;
    property ActiveProperties: TcxFileEditProperties read GetActiveProperties;
  end;

  TcxFileEdit = class(TcxCustomFileEdit)
  published
    property Properties;

    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
//    property Properties: TcxButtonEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Text;
  {$IFDEF DELPHI12}
    property TextHint;
  {$ENDIF}
    property Visible;
    property DragCursor;
    property DragKind;
    property ImeMode;
    property ImeName;
    property OnClick;
{$IFDEF DELPHI5}
    property OnContextPopup;
{$ENDIF}
    property OnDblClick;
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

  TcxDBFileEdit = class(TcxCustomFileEdit)
  private
    function GetDataBinding: TcxDBEditDataBinding;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  published
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
    property Properties;

    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
//    property DataBinding: TcxDBTextEditDataBinding read GetDataBinding write SetDataBinding;
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
//    property Properties: TcxButtonEditProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
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

  procedure Register;

const
  scxSEditRepositoryFileEdit: String = 'FileEdit|Represents an File Select/Edit control';

implementation

uses
  dxThemeManager, SBaseStringFunctions;

const
  feSelectFileButtonIndex = 0;
  feSelectDirButtonIndex  = 1;
  feClearButtonIndex      = 2;

procedure Register;
begin
  RegisterComponents ('MD Project Tools', [TcxFileEdit, TcxDBFileEdit]);
end;

{ TcxCustomFileEditProperties }

constructor TcxCustomFileEditProperties.Create(AOwner: TPersistent);
begin
  FFileTypeItems := TFileTypeItems.Create(TFileTypeItem);
  FOpenDialog := TFileOpenDialog.Create(nil);
  inherited;
end;

destructor TcxCustomFileEditProperties.Destroy;
begin
  FreeAndNil(FFileTypeItems);
  FreeAndNil(FOpenDialog);
  inherited;
end;

function TcxCustomFileEditProperties.GetButtons: TcxFileEditButtons;
begin
  Result := TcxFileEditButtons(inherited Buttons)
end;

class function TcxCustomFileEditProperties.GetButtonsClass: TcxEditButtonsClass;
begin
  Result := TcxFileEditButtons
end;

class function TcxCustomFileEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxFileEdit
end;

function TcxCustomFileEditProperties.GetDefaultExtension: string;
begin
  Result := FOpenDialog.DefaultExtension
end;

//function TcxCustomFileEditProperties.GetFileTypes: TFileTypeItems;
//begin
//  Result := FOpenDialog.FileTypes
//end;

function TcxCustomFileEditProperties.GetFileTypeIndex: Cardinal;
begin
  Result := FOpenDialog.FileTypeIndex
end;

function TcxCustomFileEditProperties.GetDefaultFolder: string;
begin
  Result := FOpenDialog.DefaultFolder
end;

function TcxCustomFileEditProperties.GetOptions: TFileDialogOptions;
begin
  Result := FOpenDialog.Options
end;

function TcxCustomFileEditProperties.GetTitle: string;
begin
  Result := FOpenDialog.Title
end;

function TcxCustomFileEditProperties.IsDialogFileTypesStored: Boolean;
begin
  Result := (FFileTypeItems.Count > 0)
end;

procedure TcxCustomFileEditProperties.SetButtons(const Value: TcxFileEditButtons);
begin
  inherited Buttons := Value;
end;

procedure TcxCustomFileEditProperties.SetDefaultExtension(const Value: string);
begin
  FOpenDialog.DefaultExtension := Value
end;

procedure TcxCustomFileEditProperties.SetFileTypes(const Value: TFileTypeItems);
begin
  FFileTypeItems.Assign(Value);
end;

procedure TcxCustomFileEditProperties.SetFileTypeIndex(const Value: Cardinal);
begin
  FOpenDialog.FileTypeIndex := Value
end;

procedure TcxCustomFileEditProperties.SetDefaultFolder(const Value: string);
begin
  FOpenDialog.DefaultFolder := Value
end;

procedure TcxCustomFileEditProperties.SetOptions(const Value: TFileDialogOptions);
begin
  FOpenDialog.Options := Value
end;

procedure TcxCustomFileEditProperties.SetTitle(const Value: string);
begin
  FOpenDialog.Title := Value
end;

procedure TcxCustomFileEditProperties.Assign(Source: TPersistent);
begin
  inherited Assign(Source);

  if Source is TcxCustomFileEditProperties then
    with TcxCustomFileEditProperties(Source) do begin
      Self.DialogDefaultFolder    := DialogDefaultFolder;
      Self.DialogDefaultExtension := DialogDefaultExtension;

      Self.DialogFileTypes        := DialogFileTypes;
      Self.DialogFileTypeIndex    := DialogFileTypeIndex;

      Self.DialogOptions          := DialogOptions;
      Self.DialogTitle            := DialogTitle;
    end;
end;

{ TcxCustomFileEdit }

procedure TcxCustomFileEdit.DoButtonClick(AButtonVisibleIndex: Integer);
var
  LAction: TfeAction;
  LFilePath: String;
begin
  with Properties do begin
    LAction := Buttons[ViewInfo.ButtonsInfo[AButtonVisibleIndex].ButtonIndex].EditAction;
    if LAction = feaFile then
      with FOpenDialog do begin
        FOpenDialog.Options := FOpenDialog.Options - [fdoPickFolders];
        FOpenDialog.FileTypes := FFileTypeItems;

        LFilePath := Text;
        if (not LFilePath.IsEmpty) then
          DefaultFolder := ExtractFileDir(LFilePath);

        if Execute then
          EditValue := FileName
      end
    else if LAction = feaDir then
      with FOpenDialog do begin
        FOpenDialog.Options := FOpenDialog.Options + [fdoPickFolders];
        FOpenDialog.FileTypes.Clear;

        LFilePath := Text;
        if (not LFilePath.IsEmpty) then
          if SysUtils.DirectoryExists(LFilePath) then
            DefaultFolder := LFilePath
          else
            DefaultFolder := ExtractFileDir(LFilePath);

        if Execute then
          EditValue := FileName
      end
    else if LAction = feaClear then
      EditValue := Null;
  end;
end;

{ TcxCustomFileEdit }

function TcxCustomFileEdit.GetActiveProperties: TcxFileEditProperties;
begin
  Result := TcxFileEditProperties(InternalGetActiveProperties);
end;

function TcxCustomFileEdit.GetProperties: TcxFileEditProperties;
begin
  Result := TcxFileEditProperties(FProperties);
end;

class function TcxCustomFileEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxFileEditProperties;
end;

procedure TcxCustomFileEdit.SetProperties(Value: TcxFileEditProperties);
begin
  FProperties.Assign(Value);
end;

{ TcxDBFileEdit }

procedure TcxDBFileEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(GetcxDBEditDataLink(Self));
end;

function TcxDBFileEdit.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

class function TcxDBFileEdit.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBEditDataBinding;
end;

procedure TcxDBFileEdit.SetDataBinding(Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

{ TcxFileEditButtons }

function TcxFileEditButtons.Add: TcxFileEditButton;
begin
  Result := TcxFileEditButton(inherited Add);
end;

class function TcxFileEditButtons.GetButtonClass: TcxEditButtonClass;
begin
  Result := TcxFileEditButton
end;

function TcxFileEditButtons.GetItem(Index: Integer): TcxFileEditButton;
begin
  Result := TcxFileEditButton(inherited GetItem(Index));
end;

procedure TcxFileEditButtons.SetItem(Index: Integer; Value: TcxFileEditButton);
begin
  inherited SetItem(Index, Value);
end;

{ TcxFileEditButton }

procedure TcxFileEditButton.Assign(Source: TPersistent);
begin
  if Source is TcxFileEditButton then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      with Source as TcxFileEditButton do
      begin
        inherited;
        Self.EditAction := EditAction;
      end;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end
  end else
    inherited Assign(Source);
end;

initialization
  RegisterClasses([TcxFileEditProperties]);
  GetRegisteredEditProperties.Register(TcxFileEditProperties, scxSEditRepositoryFileEdit);

finalization
  GetRegisteredEditProperties.Unregister(TcxFileEditProperties);
  UnRegisterClasses([TcxFileEditProperties]);

end.
