unit cxMDLookupTreeComboBox;

interface

uses
  Messages, Controls, Classes, DB, Graphics, Windows, SysUtils, Variants, StdCtrls,
  cxDBLookupEdit, cxDBEdit, cxEdit, cxDBTL, cxMDDBTreeList, cxContainer, cxDBData, cxCustomData,
  cxLookupEdit, cxLookAndFeels, cxLookAndFeelPainters, cxDB, cxTL, cxDropDownEdit,
  cxVariants, cxBarEditItem, cxDataUtils, StrUtils, Math, cxClasses, cxTextEdit,
  cxStyles, FieldDataLink, cxGeometry, cxControls, dxCoreClasses, Types
  , cxMultiFieldsDBDataBinding
  ;

const
  cxSLookupComboBoxEmptySelectionText: String = '';
  cxSLookupTreeComboBoxDividedString: String = ' - ';

type
  TTreeViewCaptionTextStyle = (tvtcsShort, tvtcsFull);
  TcxMDDBTreeListCrack = class(TcxMDDBTreeList);
  TcxTreeListViewInfoCrack = class(TcxTreeListViewInfo);
  TcxTreeListNodeCrack = class(TcxTreeListNode);
  TcxDBTreeListNodeCrack = class(TcxDBTreeListNode);
  TcxBarEditItemControlCrack = class(TcxBarEditItemControl);

  TcxCustomMDLookupTreePopupEdit = class;
  TcxCustomMDLookupTreeComboBox = class;
  TcxCustomMDLookupTreeComboBoxNodeSelectingEvent = procedure(Sender: TObject; ANode: TcxDBTreeListNode; var Allow: Boolean) of object;

  TcxMDLookupDBTreeListColumn = class(TCollectionItem)
  private
    FCaption: String;
    FFieldName: String;
    FWidth: Integer;
    FAutoWidthSizable: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Caption: String read FCaption write FCaption;
    property FieldName: String read FFieldName write FFieldName;
    property Width: Integer read FWidth write FWidth default 0;
    property AutoWidthSizable: Boolean read FAutoWidthSizable write FAutoWidthSizable default False;
  end;

  TcxMDLookupDBTreeListColumns = class(TCollection)
  private
    function GetItem(Index: Integer): TcxMDLookupDBTreeListColumn;
  public
    function Add: TcxMDLookupDBTreeListColumn;
    property Items[Index: Integer]: TcxMDLookupDBTreeListColumn read GetItem; default;
  end;

  TcxCustomMDLookupTreePopupEditProperties = class(TcxCustomDropDownEditProperties)
  private
//    FListSource: TDataSource;
    FKeyField: String;
    FParentField: String;
    FListField: String;
    FListColumns: TcxMDLookupDBTreeListColumns;
    FDisplayField: String;
    FResultField: String;
    FCanSelectField: String;
    FExpandedField: String;

    FImageIndexField: String;
    FStateIndexField: String;

    FActualResultField: String;
    FActualDisplayField: String;

    FDividedString: String;
    FTextStyle: TTreeViewCaptionTextStyle;
    FEmptySelectionText: String;

    FDropDownWidth  : Integer;
    FDropDownHeight : Integer;
    FDropDownListStyle: TcxEditDropDownListStyle;

    FTreeViewAlwaysExpandedLevel: SmallInt;
    FTreeViewShowButtons: Boolean;
    FTreeViewShowLines: Boolean;
//    FTreeViewShowRoot: Boolean;
    FTreeViewShowHeaders: Boolean;

    FListSourceDataLink: TDataSourceDataLink;
//    FcxDBTreeList: TcxMDDBTreeListCrack;

    procedure DataSourceDataLinkActiveChanged(Sender: TObject); virtual;

    function GetListSource: TDataSource;
    procedure SetListSource(const Value: TDataSource);
    procedure SetParentField(const Value: String);
    procedure SetKeyField(const Value: String);
    procedure SetListField(const Value: String);
    procedure SetResultField(const Value: String);
    procedure SetDisplayField(const Value: String);
    procedure SetEmptySelectionText(const Value: String);
    function IsEmptySelectionTextStored: Boolean;
    procedure SetListColumns(const Value: TcxMDLookupDBTreeListColumns);
    function IsDividedStringStored: Boolean;
    procedure SetDropDownListStyle(const Value: TcxEditDropDownListStyle);
    procedure SetCanSelectField(const Value: String);
  protected
    FDataSet: TDataSet;
    FAllRequiredFieldsFound: Boolean;
  protected
    class function ReservedColumns: Byte; dynamic;
    class function GetViewDataClass: TcxCustomEditViewDataClass; override;

    function GetDefaultHorzAlignment: TAlignment; override;

    class function GetPopupWindowClass: TcxCustomEditPopupWindowClass; override;
    function UseLookupData: Boolean; override;
    function FindLookupText(const AText: string): Boolean; override;
    function IsLookupDataVisual: Boolean; override;
    function GetEditingStyle: TcxEditEditingStyle; override;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;

    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;

//    function FindNodeByKeyValue(const AKeyValue: TcxEditValue): TcxDBTreeListNode;
//    function FindNodeByResultValue(const AColumnValue: TcxEditValue): TcxDBTreeListNode;

    function GetEditValueDisplayText(ANodeResultValue: Variant): String;

    function EditValueToDisplayValue(AEditValue: TcxEditValue): String; dynamic; abstract;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;

    property PopupSizeable default True;
    property DropDownWidth: Integer read FDropDownWidth write FDropDownWidth default 0;
    property DropDownHeight: Integer read FDropDownHeight write FDropDownHeight default 400;
    property DropDownListStyle: TcxEditDropDownListStyle read FDropDownListStyle write SetDropDownListStyle default lsEditList;

    property ListSource: TDataSource read GetListSource write SetListSource;
    property KeyField: String read FKeyField write SetKeyField;
    property ParentField: String read FParentField write SetParentField;
    property ListField: String read FListField write SetListField;
    property ListColumns: TcxMDLookupDBTreeListColumns read FListColumns write SetListColumns;
    property DisplayField: String read FDisplayField write SetDisplayField;
    property ResultField: String read FResultField write SetResultField;
    property CanSelectField: String read FCanSelectField write SetCanSelectField;
    property ExpandedField: String read FExpandedField write FExpandedField;
    property ImageIndexField: String read FImageIndexField write FImageIndexField;
    property StateIndexField: String read FStateIndexField write FStateIndexField;

    property DividedString: String read FDividedString write FDividedString stored IsDividedStringStored;
    property TextStyle: TTreeViewCaptionTextStyle read FTextStyle write FTextStyle default tvtcsShort;
    property EmptySelectionText: String read FEmptySelectionText write SetEmptySelectionText stored IsEmptySelectionTextStored;

    property TreeViewAlwaysExpandedLevel: SmallInt read FTreeViewAlwaysExpandedLevel write FTreeViewAlwaysExpandedLevel Default 0;
    property TreeViewShowButtons: Boolean read FTreeViewShowButtons write FTreeViewShowButtons Default True;
    property TreeViewShowLines: Boolean read FTreeViewShowLines write FTreeViewShowLines Default True;
//    property TreeViewShowRoot: Boolean read FTreeViewShowRoot write FTreeViewShowRoot Default True;
    property TreeViewShowHeaders: Boolean read FTreeViewShowHeaders write FTreeViewShowHeaders Default False;
  end;

  TcxCustomMDLookupTreeComboBoxProperties = class(TcxCustomMDLookupTreePopupEditProperties)
  private
    FCanSelectParents: Boolean;
//    FIncrementalFiltering: Boolean;
    FIncrementalSearchContainMode: Boolean;
    FOnSelecting: TcxCustomMDLookupTreeComboBoxNodeSelectingEvent;
    procedure DataSourceDataLinkActiveChanged(Sender: TObject); override;
  protected
    class function GetLookupDataClass: TcxInterfacedPersistentClass; override;
    class function ReservedColumns: Byte; override;

    function PopupWindowCapturesFocus: Boolean; override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;

    function GetNodeDisplayText(AcxTreeList: TcxMDDBTreeList; ANodeIndex: Integer): String;
    function EditValueToDisplayValue(AEditValue: TcxEditValue): String; override;

//    property IncrementalFiltering: Boolean read FIncrementalFiltering write FIncrementalFiltering default False;
    property IncrementalSearchContainMode: Boolean read FIncrementalSearchContainMode write FIncrementalSearchContainMode default False;

    property CanSelectParents: Boolean read FCanSelectParents write FCanSelectParents default False;
    property OnSelecting: TcxCustomMDLookupTreeComboBoxNodeSelectingEvent read FOnSelecting write FOnSelecting;
  end;

  TcxCustomMDLookupTreePopupEditViewData = class(TcxCustomDropDownEditViewData)
  protected
    function GetProperties: TcxCustomMDLookupTreePopupEditProperties;
    function InternalEditValueToDisplayText(AEditValue: TcxEditValue): string; override;
    function IsComboBoxStyle: Boolean; override;
  public
    property Properties: TcxCustomMDLookupTreePopupEditProperties read GetProperties;
  end;

  TcxCustomMDLookupTreeComboBoxLookupData = class(TcxInterfacedPersistent, IUnknown, IcxTextEditLookupData)
  private
    FOwner: TPersistent;
    FVisible: Boolean;
    FOnCurrentKeyChanged: TNotifyEvent;
    FOnSelectItem: TNotifyEvent;
//    function GetDataController: TcxCustomDataController;
    function GetEdit: TcxCustomMDLookupTreeComboBox;
    function GetFocusedRecordIndex: Integer;
    function GetProperties: TcxCustomMDLookupTreeComboBoxProperties;
  protected
    FCurrentKey: Variant;
    function GetOwner: TPersistent; override;
    function GetItemCount: Integer;
    function GetItem(Index: Integer): string;
    function IndexOf(AText: string; APartialCompare: Boolean = False): Integer;
    procedure ChangeFocusedRecordIndex(AIndex: Integer);
  protected
    // IcxTextEditLookupData
    function CanResizeVisualArea(var NewSize: TSize; AMaxHeight: Integer = 0; AMaxWidth: Integer = 0): Boolean; virtual;
    procedure CloseUp; virtual;
    procedure Deinitialize; virtual;
    procedure DropDown; virtual;
    procedure DroppedDown(const AFindStr: string); virtual;
    function Find(const AText: string): Boolean; virtual;
    function GetActiveControl: TControl;
    function GetCurrentKey: TcxEditValue; virtual;
    function GetDisplayText(const AKey: TcxEditValue): string; virtual;
    function GetOnCurrentKeyChanged: TNotifyEvent;
    function GetOnSelectItem: TNotifyEvent;
    function GetSelectedItem: Integer; virtual;
    function GetVisualAreaPreferredSize(AMaxHeight: Integer; AWidth: Integer = 0): TSize;
    procedure Go(ADirection: TcxEditLookupDataGoDirection; ACircular: Boolean); virtual;
    procedure Initialize(AVisualControlsParent: TWinControl);
    function IsEmpty: Boolean; virtual;
    function IsMouseOverList(const P: TPoint): Boolean;
    function Locate(var AText, ATail: string; ANext: Boolean): Boolean; virtual;
    procedure PositionVisualArea(const AClientRect: TRect);
    procedure PropertiesChanged; virtual;
    procedure SelectItem; virtual;
    procedure SetCurrentKey(const AKey: TcxEditValue); virtual;
    procedure SetOnCurrentKeyChanged(Value: TNotifyEvent);
    procedure SetOnSelectItem(Value: TNotifyEvent);
    procedure SetSelectedItem(Value: Integer); virtual;
    procedure TextChanged; virtual;
  protected
//    procedure DoCurrentKeyChanged; virtual;
//    procedure DoSelectItem; virtual;
    procedure DoSetCurrentKey(ARecordIndex: Integer); virtual;
//    procedure DoSetKeySelection(AKeySelected: Boolean);
//    procedure DoSyncGrid; virtual;
    procedure GridClick(Sender: TObject); virtual;
    procedure GridCloseUp(Sender: TObject; AAccept: Boolean); virtual;
//    procedure GridFocusedRowChanged(Sender: TObject); virtual;
    function IsKeySelected: Boolean; virtual;
    function LocateText(const AText: string): Boolean; virtual;
//    procedure ResetIncrementalFilter; virtual;
    procedure SyncGrid; virtual;
    procedure SyncSelected; virtual;
//    procedure UpdateDropDownCount; virtual;
//    property DataController: TcxCustomDataController read GetDataController;

    property Edit: TcxCustomMDLookupTreeComboBox read GetEdit;
    property FocusedRecordIndex: Integer read GetFocusedRecordIndex;
    property Properties: TcxCustomMDLookupTreeComboBoxProperties read GetProperties;
  public
    constructor Create(AOwner: TPersistent); override;
//    procedure DisableChanging;
//    procedure EnableChanging;
  end;

//  protected
//    function GetItemCount: Integer; override;
//  end;

  TcxCustomMDCheckLookupTreeComboBoxProperties = class(TcxCustomMDLookupTreePopupEditProperties)
  private
    FKeysSeparator: Char;
    FValuesSeparator: String;
    FCheckGroupTypeField: String; // ќжидаем "C"(Check) или "R"(Radio)
    FTreeViewRootCheckGroupType: TcxTreeListNodeCheckGroupType;
    FParentKeysIfPossible: Boolean;

    procedure DataSourceDataLinkActiveChanged(Sender: TObject); override;

    function IsKeysSeparatorStored: Boolean;
    function GetKeysSeparator: String;
    procedure SetKeysSeparator(const AValue: String);

    function IsValuesSeparatorStored: Boolean;
  protected
    class function ReservedColumns: Byte; override;
    function PopupWindowCapturesFocus: Boolean; override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure DoAssign(AProperties: TcxCustomEditProperties); override;
    function EditValueToDisplayValue(AEditValue: TcxEditValue): String; override;

    property KeysSeparator: String read GetKeysSeparator write SetKeysSeparator stored IsKeysSeparatorStored;
    property CheckGroupTypeField: String read FCheckGroupTypeField write FCheckGroupTypeField;
    property ValuesSeparator: String read FValuesSeparator write FValuesSeparator stored IsValuesSeparatorStored;
    property TreeViewRootCheckGroupType: TcxTreeListNodeCheckGroupType read FTreeViewRootCheckGroupType write FTreeViewRootCheckGroupType Default ncgCheckGroup;
    property ParentKeysIfPossible: Boolean read FParentKeysIfPossible write FParentKeysIfPossible default False;
  end;

  TcxCustomMDLookupTreePopupEdit = class(TcxCustomDropDownEdit)
  strict private
    FInternalUpdate: Boolean;
    FSavedDropDownWidth: Integer;
  private
    FPrevPopupControlData: TcxPrevPopupControlData;
    FTreeViewStyle: TcxStyle;
    FcxDBTreeList: TcxMDDBTreeListCrack;

    function GetActiveProperties: TcxCustomMDLookupTreePopupEditProperties;
    procedure cxDBTreeListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure DataControllerDataChanged(Sender: TObject);
  protected
    function CanDropDown: Boolean; override;
    procedure Initialize; override;
    procedure Resize; override;

    procedure DoInitPopup; override;
    procedure cxDBTreeListAfterCreate; dynamic;
    procedure InitializeByEditValue; dynamic; abstract;
    function InternalSetText(const Value: string): Boolean; override;

    function  GetPopupFocusedControl: TWinControl; override;
    function  GetPopupWindowClientPreferredSize: TSize; override;
    procedure PopupWindowClosing(Sender: TObject); override;
    procedure PopupWindowClosed(Sender: TObject); override;
    procedure PositionPopupWindowChilds(const AClientRect: TRect); override;
    procedure SetupPopupWindow; override;
    procedure HidePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason); virtual;
    procedure RestorePopupControlData; virtual;
    procedure SavePopupControlData; virtual;

    procedure InitializePopupWindow; override;
    procedure CloseUp(AReason: TcxEditCloseUpReason); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PrepareEditValue(const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue; AEditFocused: Boolean); override;

    property ActiveProperties: TcxCustomMDLookupTreePopupEditProperties read GetActiveProperties;
  end;

  TcxCustomMDLookupTreeComboBox = class(TcxCustomMDLookupTreePopupEdit)
  private
    function GetActiveProperties: TcxCustomMDLookupTreeComboBoxProperties;
    function GetProperties: TcxCustomMDLookupTreeComboBoxProperties;

    procedure cxDBTreeListClick(Sender: TObject);
    function GetNodeResultValue(ANode: TcxDBTreeListNode): Variant;
    function GetLookupData: TcxCustomMDLookupTreeComboBoxLookupData;
  protected
    procedure DoInitPopup; override;
    procedure DoFinalizePopup; override;
    procedure cxDBTreeListAfterCreate; override;
    procedure InitializeByEditValue; override;

    procedure CloseUp(AReason: TcxEditCloseUpReason); override;
    function  CanSelectNode(ANode: TcxDBTreeListNode): Boolean;

    function IsValidChar(AChar: Char): Boolean; override;
    function LookupKeyToEditValue(const AKey: TcxEditValue): TcxEditValue; override;
    property LookupData: TcxCustomMDLookupTreeComboBoxLookupData read GetLookupData;
  public
    property ActiveProperties: TcxCustomMDLookupTreeComboBoxProperties read GetActiveProperties;
    property Properties: TcxCustomMDLookupTreeComboBoxProperties read GetProperties;
    procedure PrepareEditValue(const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue; AEditFocused: Boolean); override;
  end;

  TcxCustomMDCheckLookupTreeComboBox = class(TcxCustomMDLookupTreePopupEdit)
  private
    FInternalChecking: Boolean;

    function GetActiveProperties: TcxCustomMDCheckLookupTreeComboBoxProperties;
    function GetProperties: TcxCustomMDCheckLookupTreeComboBoxProperties;

    procedure cxDBTreeListNodeCheckChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AState: TcxCheckBoxState);
  protected
    procedure InitializePopupWindow; override;
    procedure cxDBTreeListAfterCreate; override;
    procedure InitializeByEditValue; override;
    function EditValueFromList: Variant;

    procedure CloseUp(AReason: TcxEditCloseUpReason); override;

    procedure PostListValues;
  public
    property ActiveProperties: TcxCustomMDCheckLookupTreeComboBoxProperties read GetActiveProperties;
    property Properties: TcxCustomMDCheckLookupTreeComboBoxProperties read GetProperties;
  end;

  TcxMDLookupTreeComboBoxProperties = class(TcxCustomMDLookupTreeComboBoxProperties)
  public
    class function GetContainerClass: TcxContainerClass; override;
  published
    property Alignment;
    property AssignedValues;
    property AutoSelect;
    property BeepOnError;
    property ButtonGlyph;
//    property CaseInsensitive;
//    property CharCase;
    property ClearKey;
//    property EchoMode;
    property HideSelection;
//    property IgnoreMaskBlank;
//    property ImeMode;
//    property ImeName;
    property ImmediatePost;
    property ImmediateDropDownWhenActivated;
    property ImmediateDropDownWhenKeyPressed;
//    property IncrementalSearch;

    property IncrementalSearchContainMode;

//    property MaxLength;
//    property MaskKind;
//    property EditMask;
//    property OEMConvert;
//    property PasswordChar;
//    property PopupAlignment;

    property PopupAutoSize;
//    property PopupClientEdge;

//    property PopupHeight;
//    property PopupMinHeight default 18;
//    property PopupMinWidth;
//    property PopupWidth;

    property PopupSizeable;
    property DropDownWidth;
    property DropDownHeight;
    property DropDownListStyle;

    property ReadOnly default False;

//    property UseLeftAlignmentOnEditing;
//    property ValidateOnEnter;
    property ValidationOptions;

    property OnChange;
    property OnCloseQuery;
    property OnCloseUp;
    property OnEditValueChanged;
    property OnInitPopup;
    property OnPopup;
    property OnValidate;

    property ListSource;
    property KeyField;
    property ParentField;
    property ResultField;
    property CanSelectField;
    property ExpandedField;
    property ListField;
    property ListColumns;
    property DisplayField;
    property ImageIndexField;
    property StateIndexField;

    property CanSelectParents;
    property DividedString;
    property TextStyle;
    property EmptySelectionText;

    property TreeViewAlwaysExpandedLevel;
    property TreeViewShowButtons;
    property TreeViewShowLines;
//    property TreeViewShowRoot;
    property TreeViewShowHeaders;
    property OnSelecting;
  end;

  TcxMDLookupTreeComboBox = class(TcxCustomMDLookupTreeComboBox)
  private
    function GetActiveProperties: TcxMDLookupTreeComboBoxProperties;
    function GetProperties: TcxMDLookupTreeComboBoxProperties;
    procedure SetProperties(Value: TcxMDLookupTreeComboBoxProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxMDLookupTreeComboBoxProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
//    property PopupMenu;
    property Properties: TcxMDLookupTreeComboBoxProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
//    property Text;
  {$IFDEF DELPHI12}
//    property TextHint;
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
//    property OnEditing;
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

  TcxMDDBLookupTreeComboBox = class(TcxMDLookupTreeComboBox)
  private
    function GetDataBinding: TcxDBEditDataBinding;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  published
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
  end;

  TcxMDMultiFieldsDBLookupTreeComboBox = class(TcxMDLookupTreeComboBox)
  private
    function GetDataBinding: TcxMDDBEditDataBinding;
    procedure SetDataBinding(Value: TcxMDDBEditDataBinding);
//    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  published
    property DataBinding: TcxMDDBEditDataBinding read GetDataBinding write SetDataBinding;
  end;

  { Check }

  TcxMDCheckLookupTreeComboBoxProperties = class(TcxCustomMDCheckLookupTreeComboBoxProperties)
  public
    class function GetContainerClass: TcxContainerClass; override;
  published
    property Alignment;
    property AssignedValues;
    property AutoSelect;
    property BeepOnError;
    property ButtonGlyph;
//    property CaseInsensitive;
//    property CharCase;
    property ClearKey;
//    property EchoMode;
    property HideSelection;
//    property IgnoreMaskBlank;
//    property ImeMode;
//    property ImeName;
    property ImmediatePost;
    property ImmediateDropDown;
    property ImmediatePopup;
//    property IncrementalSearch;
//    property MaxLength;
//    property MaskKind;
//    property EditMask;
//    property OEMConvert;
//    property PasswordChar;
//    property PopupAlignment;

    property PopupAutoSize;
//    property PopupClientEdge;

//    property PopupHeight;
//    property PopupMinHeight default 18;
//    property PopupMinWidth;
//    property PopupWidth;

    property PopupSizeable;
    property DropDownWidth;
    property DropDownHeight;

    property ReadOnly default False;
//    property UseLeftAlignmentOnEditing;
//    property ValidateOnEnter;
    property ValidationOptions;

    property OnChange;
    property OnCloseQuery;
    property OnCloseUp;
    property OnEditValueChanged;
    property OnInitPopup;
    property OnPopup;
    property OnValidate;

    property ListSource;
    property KeyField;
    property KeysSeparator;
    property ParentField;
    property ParentKeysIfPossible;
    property ResultField;
    property ListField;
    property ListColumns;
    property DisplayField;
    property ExpandedField;
    property ValuesSeparator;
    property ImageIndexField;
    property StateIndexField;
    property CheckGroupTypeField;

    property DividedString;
    property TextStyle;
    property EmptySelectionText;

    property TreeViewAlwaysExpandedLevel;
    property TreeViewShowButtons;
    property TreeViewShowLines;
//    property TreeViewShowRoot;
    property TreeViewShowHeaders;
    property TreeViewRootCheckGroupType;
  end;

  TcxMDCheckLookupTreeComboBox = class(TcxCustomMDCheckLookupTreeComboBox)
  private
    function GetActiveProperties: TcxMDCheckLookupTreeComboBoxProperties;
    function GetProperties: TcxMDCheckLookupTreeComboBoxProperties;
    procedure SetProperties(Value: TcxMDCheckLookupTreeComboBoxProperties);
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxMDCheckLookupTreeComboBoxProperties read GetActiveProperties;
  published
    property Anchors;
    property AutoSize;
    property BeepOnEnter;
    property Constraints;
    property DragMode;
    property Enabled;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
//    property PopupMenu;
    property Properties: TcxMDCheckLookupTreeComboBoxProperties read GetProperties write SetProperties;
    property ShowHint;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property TabOrder;
    property TabStop;
//    property Text;
  {$IFDEF DELPHI12}
//    property TextHint;
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
//    property OnEditing;
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

  TcxMDDBCheckLookupTreeComboBox = class(TcxMDCheckLookupTreeComboBox)
  private
    function GetDataBinding: TcxDBEditDataBinding;
    procedure SetDataBinding(Value: TcxDBEditDataBinding);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    class function GetDataBindingClass: TcxEditDataBindingClass; override;
  published
    property DataBinding: TcxDBEditDataBinding read GetDataBinding write SetDataBinding;
  end;

  TcxEditRepositoryMDLookupTreeComboBoxItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxMDLookupTreeComboBoxProperties;
    procedure SetProperties(Value: TcxMDLookupTreeComboBoxProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxMDLookupTreeComboBoxProperties read GetProperties write SetProperties;
  end;

  TcxEditRepositoryMDCheckLookupTreeComboBoxItem = class(TcxEditRepositoryItem)
  private
    function GetProperties: TcxMDCheckLookupTreeComboBoxProperties;
    procedure SetProperties(Value: TcxMDCheckLookupTreeComboBoxProperties);
  public
    class function GetEditPropertiesClass: TcxCustomEditPropertiesClass; override;
  published
    property Properties: TcxMDCheckLookupTreeComboBoxProperties read GetProperties write SetProperties;
  end;

implementation

uses SBaseVariantFunctions, SBaseStringFunctions;

const
  cxSEditRepositoryLookupTreeComboBoxEditItem       = 'LookupTreeComboBox|Represents an ultra-advanced lookup using the DBTreeList as its drop down control';
  cxSEditRepositoryCheckLookupTreeComboBoxEditItem  = 'CheckLookupTreeComboBox|Represents an ultra-advanced lookup checks using the DBTreeList as its drop down control';
  cxLookupComboBoxKeysSeparator: Char = ',';
  cxLookupComboBoxValuesSeparator     = '; ';

  cxLookupTreeListFieldIndex                    = 0;
  cxLookupTreeDisplayFieldIndex                 = 1;

//  cxLookupTreeResultFieldIndex                  = 2;
  cxLookupTreeComboBoxCanSelectFieldIndex       = 2;

  cxLookupTreeComboCheckBoxGroupTypeFieldIndex  = 2;
  cxLookupTreeComboCheckBoxResultFieldIndex     = 3;

{
type
  TcxDBTreeListColumnValue = packed record
    Count : Integer;
    PValue: PcxEditValue;
  end;
  PcxDBTreeListColumnValue = ^TcxDBTreeListColumnValue;
}

{ TcxCustomMDLookupTreeComboBox }

function TcxCustomMDLookupTreeComboBox.GetActiveProperties: TcxCustomMDLookupTreeComboBoxProperties;
begin
  Result := TcxCustomMDLookupTreeComboBoxProperties(InternalGetActiveProperties);
end;

function TcxCustomMDLookupTreeComboBox.GetLookupData: TcxCustomMDLookupTreeComboBoxLookupData;
begin
  Result := TcxCustomMDLookupTreeComboBoxLookupData(FLookupData);
end;

function TcxCustomMDLookupTreeComboBox.GetNodeResultValue(ANode: TcxDBTreeListNode): Variant;
begin
  with ActiveProperties do
    if (ANode = nil) or (not FAllRequiredFieldsFound) then
      Result := Null
    else if FResultField.Length = 0 then
      Result := ANode.KeyValue
    else
      Result := FDataSet.Lookup(FKeyField, ANode.KeyValue, FResultField)
end;

function TcxCustomMDLookupTreeComboBox.GetProperties: TcxCustomMDLookupTreeComboBoxProperties;
begin
  Result := TcxCustomMDLookupTreeComboBoxProperties(FProperties);
end;

procedure TcxCustomMDLookupTreeComboBox.DoInitPopup;
begin
  inherited;

  with FcxDBTreeList do begin
    OnClick := cxDBTreeListClick;
    OptionsBehavior.IncSearch := CanModify;
    if OptionsBehavior.IncSearch and (ColumnCount > 0) then
      OptionsBehavior.IncSearchItem := Columns[0]
  end;
end;

procedure TcxCustomMDLookupTreeComboBox.InitializeByEditValue;
var
  LcxDBTreeListNode : TcxDBTreeListNode;
  LKeyValue: Variant;
//  LAlwaysExpandedLevel: SmallInt;
//  LLevel: Integer;
begin
  inherited;

  with FcxDBTreeList, ActiveProperties do begin
    if Count > 0 then begin
      LcxDBTreeListNode := nil;
      if FAllRequiredFieldsFound then begin
        if (ResultField <> '') then
          LKeyValue := FDataSet.Lookup(FResultField, EditValue, FKeyField)
        else
          LKeyValue := EditValue;

        if VarIsPresent(LKeyValue) then
          LcxDBTreeListNode := FindNodeByKeyValue(LKeyValue);
      end;

      if Assigned(LcxDBTreeListNode) then begin
        LcxDBTreeListNode.Focused := True;
        HandleNeeded;
        LcxDBTreeListNode.MakeVisible;
//        LAlwaysExpandedLevel := OptionsBehavior.AlwaysExpandedLevel;
//        LLevel := LcxDBTreeListNode.Level;
//        if LAlwaysExpandedLevel > -1 then
//          while (LLevel > LAlwaysExpandedLevel) do begin
//            Dec(LLevel);
//            LcxDBTreeListNode := TcxDBTreeListNode(LcxDBTreeListNode.Parent);
//            if LcxDBTreeListNode = nil then Break;
//            LcxDBTreeListNode.Expanded := True;
//          end
      end else
        TopNode.Focused := True;
    end;
  end;
end;

function TcxCustomMDLookupTreeComboBox.IsValidChar(AChar: Char): Boolean;
var
  LField: TField;
begin
  Result := True;
  if (FcxDBTreeList <> nil) and (FcxDBTreeList.ColumnCount > cxLookupTreeListFieldIndex) then begin
   LField := TcxDBTreeListColumn(FcxDBTreeList.Columns[cxLookupTreeListFieldIndex]).DataBinding.Field;
   if LField <> nil then
    Result := LField.IsValidChar(AChar)
  end;
end;

function TcxCustomMDLookupTreeComboBox.LookupKeyToEditValue(const AKey: TcxEditValue): TcxEditValue;
begin
  Result := AKey
end;

procedure TcxCustomMDLookupTreeComboBox.PrepareEditValue(const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue; AEditFocused: Boolean);
begin
  if InternalCompareString(ILookupData.GetDisplayText(ILookupData.CurrentKey), ADisplayValue, ActiveProperties.CharCase = ecNormal) then
    EditValue := ILookupData.CurrentKey
  else if LookupData.LocateText(ADisplayValue) then
    EditValue := ILookupData.CurrentKey
  else
    EditValue := Null;
end;

procedure TcxCustomMDLookupTreeComboBox.DoFinalizePopup;
begin
  inherited;

  if Assigned(FcxDBTreeList) then
    FcxDBTreeList.OnClick := nil;
end;

procedure TcxCustomMDLookupTreeComboBox.CloseUp(AReason: TcxEditCloseUpReason);
var
  LFocusedNode: TcxDBTreeListNode;
begin
  LockChangeEvents(True);
  try
    if (AReason in [crEnter, crTab]) then begin
      if Assigned(FcxDBTreeList) then begin
        LFocusedNode := TcxDBTreeListNode(FcxDBTreeList.FocusedNode);
        if not CanSelectNode(LFocusedNode) then begin
          LFocusedNode.Expand(False);
          Exit;
        end;

        EditValue := GetNodeResultValue(LFocusedNode);
        PostEditValue;
      end;
    end;

    inherited;
  finally
    LockChangeEvents(False);
  end;
end;

procedure TcxCustomMDLookupTreeComboBox.cxDBTreeListAfterCreate;
begin
  inherited;

  with FcxDBTreeList do begin
    with TcxDBItemDataBinding(Columns[cxLookupTreeComboBoxCanSelectFieldIndex].DataBinding), ActiveProperties do begin
      FieldName := FCanSelectField;
    end;

    with TcxDBItemDataBinding(Columns[cxLookupTreeDisplayFieldIndex].DataBinding), ActiveProperties do begin
      FieldName := FDisplayField;
    end;

//    with TcxDBItemDataBinding(Columns[cxLookupTreeResultFieldIndex].DataBinding), ActiveProperties do begin
//      FieldName := FResultField;
//    end;
  end;
end;

procedure TcxCustomMDLookupTreeComboBox.cxDBTreeListClick(Sender: TObject);
begin
  with TcxMDDBTreeList(Sender).HitTest do
    if (HitAtColumn) and (not HitAtColumnHeader) then
      CloseUp(crEnter);
end;

function TcxCustomMDLookupTreeComboBox.CanSelectNode(ANode: TcxDBTreeListNode): Boolean;
begin
{$IFDEF PACKAGE}
  try
{$ENDIF}
//  Result := (not ActiveProperties.ReadOnly) and (DataBinding.CanModify)
  Result := CanModify
              and (ANode <> nil)
              and (ActiveProperties.CanSelectParents or (ANode.Count = 0))
              and (ANode.Values[cxLookupTreeComboBoxCanSelectFieldIndex] <> False);

  if Result and Assigned(ActiveProperties.OnSelecting) then
    ActiveProperties.OnSelecting(Owner, ANode, Result);
{$IFDEF PACKAGE}
  except
    Assert(False);
  end;
{$ENDIF}
end;

{ TcxCustomMDLookupTreePopupEditProperties }

procedure TcxCustomMDLookupTreePopupEditProperties.DoAssign(AProperties: TcxCustomEditProperties);
//var
//  LPopupControl: TControl;
begin
  if AProperties is TcxCustomMDLookupTreePopupEditProperties then
  begin
    BeginUpdate;
    try
      inherited;

      with TcxCustomMDLookupTreePopupEditProperties(AProperties) do begin
        Self.ListSource       := ListSource;
        Self.KeyField         := KeyField;
        Self.ParentField      := ParentField;
        Self.ListField        := ListField;
        Self.ListColumns      := ListColumns;
        Self.DisplayField     := DisplayField;
        Self.ResultField      := ResultField;
        Self.CanSelectField   := CanSelectField;
        Self.ExpandedField    := ExpandedField;
        Self.ImageIndexField  := ImageIndexField;
        Self.StateIndexField  := StateIndexField;

        Self.DividedString      := DividedString;
        Self.TextStyle          := TextStyle;
        Self.EmptySelectionText := EmptySelectionText;

        Self.DropDownWidth      := DropDownWidth;
        Self.DropDownHeight     := DropDownHeight;
        Self.DropDownListStyle  := DropDownListStyle;

        Self.TreeViewAlwaysExpandedLevel := TreeViewAlwaysExpandedLevel;
        Self.TreeViewShowButtons    := TreeViewShowButtons;
        Self.TreeViewShowLines      := TreeViewShowLines;
//        Self.TreeViewShowRoot       := TreeViewShowRoot;
        Self.TreeViewShowHeaders    := TreeViewShowHeaders;
      end;
    finally
      EndUpdate;
    end;
  end
end;

function TcxCustomMDLookupTreePopupEditProperties.FindLookupText(const AText: string): Boolean;
begin
  Result := True;
end;

constructor TcxCustomMDLookupTreePopupEditProperties.Create(AOwner: TPersistent);
begin
  FTextStyle := tvtcsShort;
  FDividedString := cxSLookupTreeComboBoxDividedString;
  FEmptySelectionText := cxSLookupComboBoxEmptySelectionText;

  FTreeViewShowButtons := True;
  FTreeViewShowLines := True;
//  FTreeViewShowRoot := True;
//  FTreeViewAlwaysExpandedLevel := -1;

  FDropDownListStyle := lsEditList;

  FListSourceDataLink := TDataSourceDataLink.Create;
  FListSourceDataLink.OnActiveChanged := DataSourceDataLinkActiveChanged;
  FListSourceDataLink.OnFieldListChanged := DataSourceDataLinkActiveChanged;

  FListColumns := TcxMDLookupDBTreeListColumns.Create(TcxMDLookupDBTreeListColumn);

  inherited;

  PopupAutoSize   := True;
  PopupSizeable   := True;

//  PopupHeight     := 0;
  FDropDownHeight  := 400;
//  PopupMinHeight  := 18;

//  PopupMinWidth   := 100;
//  PopupWidth      := 250;
end;

destructor TcxCustomMDLookupTreePopupEditProperties.Destroy;
begin
  inherited;
  FListColumns.Free;
  FListSourceDataLink.Free;
end;

procedure TcxCustomMDLookupTreePopupEditProperties.DataSourceDataLinkActiveChanged(Sender: TObject);
begin
  FDataSet := FListSourceDataLink.DataSet;

  if FDataSet <> nil then begin
    FAllRequiredFieldsFound :=      (FKeyField.Length > 0) and Assigned(FDataSet.FindField(FKeyField))
                                and (FParentField.Length > 0) and Assigned(FDataSet.FindField(FParentField))
                                and (FListField.Length > 0) and Assigned(FDataSet.FindField(FListField))

                                and (FDisplayField.Length = 0) or Assigned(FDataSet.FindField(FDisplayField));
  end else
    FAllRequiredFieldsFound := False;

//  if not FInternalUpdate then
  Changed;
end;

procedure TcxCustomMDLookupTreePopupEditProperties.PrepareDisplayValue(const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue; AEditFocused: Boolean);
begin
  DisplayValue := EditValueToDisplayValue(AEditValue);
end;

class function TcxCustomMDLookupTreePopupEditProperties.ReservedColumns: Byte;
begin
  Result := cxLookupTreeListFieldIndex
end;

function TcxCustomMDLookupTreePopupEditProperties.GetDefaultHorzAlignment: TAlignment;
begin
  Result := taLeftJustify;
end;

function TcxCustomMDLookupTreePopupEditProperties.GetEditingStyle: TcxEditEditingStyle;
const
  AEditingStyleMap: array[TcxEditDropDownListStyle] of TcxEditEditingStyle = (esEditList, esEdit, esFixedList);
begin
  Result := AEditingStyleMap[DropDownListStyle];
end;

function TcxCustomMDLookupTreePopupEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  Result := evsValue;
end;

function TcxCustomMDLookupTreePopupEditProperties.GetListSource: TDataSource;
begin
  Result := FListSourceDataLink.DataSource;
end;

function TcxCustomMDLookupTreePopupEditProperties.GetEditValueDisplayText(ANodeResultValue: Variant): String;
begin
  if not VarIsPresent(ANodeResultValue) then
    Result := FEmptySelectionText
  else begin
    Result := '';
    if FAllRequiredFieldsFound and FDataSet.Active then begin
      ANodeResultValue := FDataSet.Lookup(FActualResultField, ANodeResultValue, FActualDisplayField + ';' + FParentField);
      if not VarIsArray(ANodeResultValue) then
        Exit;

      Result := VarToStr(ANodeResultValue[0]);
      ANodeResultValue := ANodeResultValue[1];

      if FTextStyle = tvtcsFull then
        while VarIsPresent(ANodeResultValue) do begin
          ANodeResultValue := FDataSet.Lookup(FKeyField, ANodeResultValue, FActualDisplayField + ';' + FParentField);
          if VarIsArray(ANodeResultValue) then begin
            Result := VarToStr(ANodeResultValue[0]) + FDividedString + Result;
            ANodeResultValue := ANodeResultValue[1];
          end else
            Break;
        end;
    end;
  end;
end;

class function TcxCustomMDLookupTreePopupEditProperties.GetPopupWindowClass: TcxCustomEditPopupWindowClass;
begin
  Result := TcxPopupEditPopupWindow;
end;

class function TcxCustomMDLookupTreePopupEditProperties.GetViewDataClass: TcxCustomEditViewDataClass;
begin
  Result := TcxCustomMDLookupTreePopupEditViewData;
end;

function TcxCustomMDLookupTreePopupEditProperties.IsDividedStringStored: Boolean;
begin
  Result := FDividedString <> cxSLookupTreeComboBoxDividedString
end;

function TcxCustomMDLookupTreePopupEditProperties.IsEmptySelectionTextStored: Boolean;
begin
  Result := FEmptySelectionText <> cxSLookupComboBoxEmptySelectionText;
end;

function TcxCustomMDLookupTreePopupEditProperties.IsLookupDataVisual: Boolean;
begin
  Result := False;
end;

{
procedure TcxCustomMDLookupTreePopupEditProperties.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) then begin
    if (PopupControl <> nil) and (PopupControl = AComponent) then
      PopupControl := nil;
  end;
end;
}

procedure TcxCustomMDLookupTreePopupEditProperties.SetCanSelectField(const Value: String);
begin
  if FCanSelectField <> Value then begin
    FCanSelectField := Value;
    DataSourceDataLinkActiveChanged(nil);
  end;
end;

procedure TcxCustomMDLookupTreePopupEditProperties.SetDisplayField(const Value: String);
begin
  if FDisplayField <> Value then begin
    FDisplayField := Value;
    if Value.Length > 0 then
      FActualDisplayField := Value
    else
      FActualDisplayField := FListField;

    DataSourceDataLinkActiveChanged(nil);
  end;
end;

procedure TcxCustomMDLookupTreePopupEditProperties.SetDropDownListStyle(const Value: TcxEditDropDownListStyle);
begin
  if FDropDownListStyle <> Value then begin
    FDropDownListStyle := Value;
    Changed;
  end;
end;

procedure TcxCustomMDLookupTreePopupEditProperties.SetEmptySelectionText(const Value: String);
begin
  if FEmptySelectionText <> Value then begin
    FEmptySelectionText := Value;
    Changed;
  end;
end;

procedure TcxCustomMDLookupTreePopupEditProperties.SetKeyField(const Value: String);
begin
  if FKeyField <> Value then begin
    FKeyField := Value;
    if FResultField.Length = 0 then
      FActualResultField := Value;

    DataSourceDataLinkActiveChanged(nil);
  end;
end;

procedure TcxCustomMDLookupTreePopupEditProperties.SetListColumns(const Value: TcxMDLookupDBTreeListColumns);
begin
  FListColumns.Assign(Value);
end;

procedure TcxCustomMDLookupTreePopupEditProperties.SetListField(const Value: String);
begin
  if FListField <> Value then begin
    FListField := Value;
    if FDisplayField.Length = 0 then
      FActualDisplayField := Value;

    DataSourceDataLinkActiveChanged(nil);
  end;
end;

procedure TcxCustomMDLookupTreePopupEditProperties.SetListSource(const Value: TDataSource);
begin
  FListSourceDataLink.DataSource := Value;
end;

procedure TcxCustomMDLookupTreePopupEditProperties.SetParentField(const Value: String);
begin
  if FParentField <> Value then begin
    FParentField := Value;
    DataSourceDataLinkActiveChanged(nil);
  end;
end;

procedure TcxCustomMDLookupTreePopupEditProperties.SetResultField(const Value: String);
begin
  if FResultField <> Value then begin
    FResultField := Value;
    if Value.Length > 0 then
      FActualResultField := Value
    else
      FActualResultField := FKeyField;

    DataSourceDataLinkActiveChanged(nil);
  end;
end;

function TcxCustomMDLookupTreePopupEditProperties.UseLookupData: Boolean;
begin
  Result := True;
end;

{ TcxCustomMDLookupTreeComboBoxProperties }

constructor TcxCustomMDLookupTreeComboBoxProperties.Create(AOwner: TPersistent);
begin
  FCanSelectParents := False;
  inherited;
end;

procedure TcxCustomMDLookupTreeComboBoxProperties.DataSourceDataLinkActiveChanged(Sender: TObject);
var
  LFieldList: TFieldsList;
begin
  inherited;

  if FAllRequiredFieldsFound and (FResultField.Length > 0) then begin
    LFieldList := TFieldsList.Create;
    try
      GetFieldsProperty(LFieldList, FDataSet, nil, FResultField, False);
      FAllRequiredFieldsFound := LFieldList.Count > 0;
    finally
      LFieldList.Free;
    end;
  end;
end;

procedure TcxCustomMDLookupTreeComboBoxProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  if AProperties is TcxCustomMDLookupTreeComboBoxProperties then begin
    BeginUpdate;
    try
      inherited;
      with TcxCustomMDLookupTreeComboBoxProperties(AProperties) do begin
        Self.IncrementalSearchContainMode := IncrementalSearchContainMode;
        Self.CanSelectParents             := CanSelectParents;
        Self.OnSelecting                  := OnSelecting;
      end;
    finally
      EndUpdate;
    end;
  end
end;

function TcxCustomMDLookupTreeComboBoxProperties.EditValueToDisplayValue(AEditValue: TcxEditValue): String;
begin
  if not VarIsPresent(AEditValue) then
    Exit(FEmptySelectionText);

  if not FAllRequiredFieldsFound then
    Result := VarToStrDef(AEditValue, '')
  else
    Result := GetEditValueDisplayText(AEditValue)
end;

class function TcxCustomMDLookupTreeComboBoxProperties.GetLookupDataClass: TcxInterfacedPersistentClass;
begin
  Result := TcxCustomMDLookupTreeComboBoxLookupData
end;

function TcxCustomMDLookupTreeComboBoxProperties.GetNodeDisplayText(AcxTreeList: TcxMDDBTreeList; ANodeIndex: Integer): String;
var
  LNode: TcxDBTreeListNode;
  FActualDisplayFieldIndex: Integer;
  LNodeResultValue: Variant;
begin
  Result := '';

  if FAllRequiredFieldsFound and FDataSet.Active then begin
    LNode := TcxDBTreeListNode(AcxTreeList.AbsoluteItems[ANodeIndex]);
    if not FDisplayField.IsEmpty then
      FActualDisplayFieldIndex := cxLookupTreeDisplayFieldIndex
    else
      FActualDisplayFieldIndex := cxLookupTreeListFieldIndex;

    if not FResultField.IsEmpty then begin
      LNodeResultValue := FDataSet.Lookup(FKeyField, LNode.KeyValue, FResultField);
//      LNodeResultValue := LNode.Values[cxLookupTreeResultFieldIndex];
      if VarType(LNodeResultValue) in [varEmpty, varNull] then Exit;
    end;

    Result := VarToStr(LNode.Values[FActualDisplayFieldIndex]);

    if FTextStyle = tvtcsFull then
      repeat
        LNode := TcxDBTreeListNode(LNode.Parent);
        if not Assigned(LNode) then Exit;
        Result := VarToStr(LNode.Values[FActualDisplayFieldIndex]) + FDividedString + Result;
      until True;
  end;
end;

function TcxCustomMDLookupTreeComboBoxProperties.PopupWindowCapturesFocus: Boolean;
begin
  Result := False;
end;

class function TcxCustomMDLookupTreeComboBoxProperties.ReservedColumns: Byte;
begin
  Result := cxLookupTreeComboBoxCanSelectFieldIndex;
end;

{ TcxCustomMDCheckLookupTreeComboBoxProperties }

procedure TcxCustomMDCheckLookupTreeComboBoxProperties.DataSourceDataLinkActiveChanged(Sender: TObject);
begin
  inherited;

  if FAllRequiredFieldsFound and (FResultField.Length > 0) then
    FAllRequiredFieldsFound := Assigned(FDataSet.FindField(FResultField))
end;

procedure TcxCustomMDCheckLookupTreeComboBoxProperties.DoAssign(AProperties: TcxCustomEditProperties);
begin
  if AProperties is TcxCustomMDCheckLookupTreeComboBoxProperties then begin
    BeginUpdate;
    try
      with TcxCustomMDCheckLookupTreeComboBoxProperties(AProperties) do begin
        Self.KeysSeparator              := KeysSeparator;
        Self.CheckGroupTypeField        := CheckGroupTypeField;
        Self.ValuesSeparator            := ValuesSeparator;
        Self.TreeViewRootCheckGroupType := TreeViewRootCheckGroupType;
        Self.ParentKeysIfPossible       := ParentKeysIfPossible;
      end;

      inherited;
    finally
      EndUpdate;
    end;
  end
end;

constructor TcxCustomMDCheckLookupTreeComboBoxProperties.Create(AOwner: TPersistent);
begin
  FKeysSeparator := cxLookupComboBoxKeysSeparator;
  FValuesSeparator := cxLookupComboBoxValuesSeparator;
  FTreeViewRootCheckGroupType := ncgCheckGroup;
  inherited;
end;

function TcxCustomMDCheckLookupTreeComboBoxProperties.EditValueToDisplayValue(AEditValue: TcxEditValue): String;
var
  LItem         : TStringItem;
  LResultValue  : String;
  LBookmark     : TBookmark;
begin
  if not VarIsPresent(AEditValue) then
    Exit(FEmptySelectionText);

  LResultValue := VarToStr(AEditValue);
  if LResultValue <> '' then begin
    if not FAllRequiredFieldsFound then
      Exit(LResultValue)
    else begin
      LBookmark := FDataSet.Bookmark;
      FDataSet.DisableControls;
      try
        for LItem in SBaseStringFunctions.EnumStringItems(LResultValue, FKeysSeparator) do
          Result := ConcatIfNotEmpty(Result, FValuesSeparator, GetEditValueDisplayText(LItem.Value));
      finally
        FDataSet.Bookmark := LBookmark;
        FDataSet.EnableControls;
      end;
    end
  end else
    Result := FEmptySelectionText;
end;

procedure TcxCustomMDCheckLookupTreeComboBoxProperties.SetKeysSeparator(const AValue: String);
var
  LKeysSeparator: Char;
begin
{
  if AValue.IsEmpty then
    LKeysSeparator := #0
  else
}
  if AValue.Length = 1 then
    LKeysSeparator := AValue[1]
  else
    Raise Exception.Create('Incorrect KeysSeparator value "' + AValue + '"');

  if FKeysSeparator <> LKeysSeparator then begin
    FKeysSeparator := LKeysSeparator;
    Changed;
  end;
end;

function TcxCustomMDCheckLookupTreeComboBoxProperties.GetKeysSeparator: String;
begin
{
  if FKeysSeparator = #0 then
    Result := ''
  else
}
    Result := String(FKeysSeparator)
end;

function TcxCustomMDCheckLookupTreeComboBoxProperties.IsKeysSeparatorStored: Boolean;
begin
  Result := FKeysSeparator <> cxLookupComboBoxKeysSeparator;
end;

function TcxCustomMDCheckLookupTreeComboBoxProperties.IsValuesSeparatorStored: Boolean;
begin
  Result := FValuesSeparator <> cxLookupComboBoxValuesSeparator;
end;

function TcxCustomMDCheckLookupTreeComboBoxProperties.PopupWindowCapturesFocus: Boolean;
begin
  Result := True;
end;

class function TcxCustomMDCheckLookupTreeComboBoxProperties.ReservedColumns: Byte;
begin
  Result := cxLookupTreeComboCheckBoxResultFieldIndex
end;

{ TcxMDLookupTreeComboBox }

function TcxMDLookupTreeComboBox.GetActiveProperties: TcxMDLookupTreeComboBoxProperties;
begin
  Result := TcxMDLookupTreeComboBoxProperties(InternalGetActiveProperties);
end;

function TcxMDLookupTreeComboBox.GetProperties: TcxMDLookupTreeComboBoxProperties;
begin
  Result := TcxMDLookupTreeComboBoxProperties(FProperties);
end;

class function TcxMDLookupTreeComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMDLookupTreeComboBoxProperties;
end;

procedure TcxMDLookupTreeComboBox.SetProperties(Value: TcxMDLookupTreeComboBoxProperties);
begin
{$IFDEF PACKAGE}
  try
{$ENDIF}
  FProperties.Assign(Value);
{$IFDEF PACKAGE}
  except
    Assert(False);
  end;
{$ENDIF}
end;

{ TcxMDDBLookupTreeComboBox }

class function TcxMDDBLookupTreeComboBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBLookupEditDataBinding;
end;

function TcxMDDBLookupTreeComboBox.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

procedure TcxMDDBLookupTreeComboBox.SetDataBinding(Value: TcxDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

procedure TcxMDDBLookupTreeComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(GetcxDBEditDataLink(Self));
end;

{ TcxMDMultiFieldsDBLookupTreeComboBox }

class function TcxMDMultiFieldsDBLookupTreeComboBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxMDDBEditDataBinding;
end;

function TcxMDMultiFieldsDBLookupTreeComboBox.GetDataBinding: TcxMDDBEditDataBinding;
begin
  Result := TcxMDDBEditDataBinding(FDataBinding);
end;

procedure TcxMDMultiFieldsDBLookupTreeComboBox.SetDataBinding(Value: TcxMDDBEditDataBinding);
begin
  FDataBinding.Assign(Value);
end;

{ TcxCustomMDLookupTreePopupEditViewData }

function TcxCustomMDLookupTreePopupEditViewData.GetProperties: TcxCustomMDLookupTreePopupEditProperties;
begin
  Result := TcxCustomMDLookupTreePopupEditProperties(FProperties);
end;

function TcxCustomMDLookupTreePopupEditViewData.InternalEditValueToDisplayText(AEditValue: TcxEditValue): string;
begin
{$IFDEF PACKAGE}
  try
{$ENDIF}
  Result := Properties.EditValueToDisplayValue(AEditValue);
{$IFDEF PACKAGE}
  except
    Assert(False);
  end;
{$ENDIF}
end;

function TcxCustomMDLookupTreePopupEditViewData.IsComboBoxStyle: Boolean;
begin
  Result := True
end;

{ TcxMDLookupTreeComboBoxProperties }

class function TcxMDLookupTreeComboBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxMDLookupTreeComboBox;
end;

{ TcxMDCheckLookupTreeComboBoxProperties }

class function TcxMDCheckLookupTreeComboBoxProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxMDCheckLookupTreeComboBox;
end;

{ TcxCustomMDLookupTreePopupEdit }

procedure TcxCustomMDLookupTreePopupEdit.Initialize;
begin
  inherited;
  ControlStyle := ControlStyle - [csSetCaption];
end;

constructor TcxCustomMDLookupTreePopupEdit.Create(AOwner: TComponent);
begin
  inherited;
  FTreeViewStyle := TcxStyle.Create(nil);
end;

procedure TcxCustomMDLookupTreePopupEdit.cxDBTreeListAfterCreate;
var
  I: Integer;
  LcxTreeListColumn: TcxTreeListColumn;
begin
  with FcxDBTreeList do begin
//    Align := alClient;
    BorderStyle := cxcbsNone;
    Bands.Clear;
    Bands.Add;

    for I := 0 to ActiveProperties.ReservedColumns do begin
      LcxTreeListColumn := CreateColumn;
      LcxTreeListColumn.Visible := (I = cxLookupTreeListFieldIndex);
      LcxTreeListColumn.Options.AutoWidthSizable := LcxTreeListColumn.Visible;
    end;

//    OptionsData.Deleting  := False;
//    OptionsData.Inserting := False;
//    OptionsData.Editing   := False;
//    OptionsData.SyncMode  := False;

    OptionsView.ColumnAutoWidth := True;
    OptionsView.ScrollBars      := ssVertical;
    OptionsView.GridLines       := tlglNone;

    OptionsSelection.CellSelect := False;

//    OptionsBehavior.ExpandOnDblClick := False;

    DataController.OnDataChanged := DataControllerDataChanged;
//    CheckChanges;
  end;
end;

procedure TcxCustomMDLookupTreePopupEdit.cxDBTreeListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
{$IFDEF PACKAGE}
  try
{$ENDIF}
  if Key = VK_ESCAPE then
    CloseUp(crClose)
  else if (Key = VK_RETURN) then
    CloseUp(crEnter);
{$IFDEF PACKAGE}
  except
    Assert(False);
  end;
{$ENDIF}
end;

procedure TcxCustomMDLookupTreePopupEdit.DataControllerDataChanged(Sender: TObject);
begin
  if not FInternalUpdate then begin
    FInternalUpdate := True;
    try
      DataBinding.UpdateDisplayValue;
    finally
      FInternalUpdate := False;
    end;
  end;
end;

destructor TcxCustomMDLookupTreePopupEdit.Destroy;
begin
  FreeAndNil(FcxDBTreeList);
  FTreeViewStyle.Free;
  inherited;
end;

procedure TcxCustomMDLookupTreePopupEdit.DoInitPopup;
var
  I: Integer;
  LDropDownWidth, LDropDownHeight, LAlwaysExpandedLevel: Integer;
begin
  inherited;

  with ActiveProperties do begin
    FInternalUpdate := True;
    try
      if (FcxDBTreeList = nil) then begin
        FcxDBTreeList := TcxMDDBTreeListCrack(TcxMDDBTreeList.Create(nil));
        with FcxDBTreeList do begin
          BeginUpdate;
          try
            cxDBTreeListAfterCreate;
          finally
            EndUpdate;
          end;
        end;
      end;

      with FcxDBTreeList do begin
        BeginUpdate;
        try
          OptionsView.Headers   := FTreeViewShowHeaders;
          OptionsView.Buttons   := FTreeViewShowButtons;
          if FTreeViewShowLines then
            OptionsView.TreeLineStyle := tllsDot
          else
            OptionsView.TreeLineStyle := tllsNone;
          OptionsBehavior.AlwaysExpandedLevel := FTreeViewAlwaysExpandedLevel;

          DataController.KeyField   := FKeyField;
          DataController.ParentField:= FParentField;
          DataController.ExpandedField := FExpandedField;
          DataController.ImageIndexField := FImageIndexField;
          DataController.StateIndexField := FStateIndexField;

          with Columns[cxLookupTreeListFieldIndex] do begin
            TcxDBItemDataBinding(DataBinding).FieldName := FListField;
            Visible := (FListColumns.Count = 0);
          end;

          LDropDownWidth := 0;
          for I := 0 to FListColumns.Count - 1 do begin
            if ColumnCount = ReservedColumns + I + 1 then
              CreateColumn;
            with Columns[ReservedColumns + I + 1] do begin
              TcxDBItemDataBinding(DataBinding).FieldName := FListColumns[I].FieldName;
              Caption.Text := FListColumns[I].Caption;
              Width   := FListColumns[I].Width;
              Options.AutoWidthSizable := FListColumns[I].AutoWidthSizable;
              if (LDropDownWidth <> -1) and (FListColumns[I].Width > 0) then
                LDropDownWidth := LDropDownWidth + FListColumns[I].Width
              else
                LDropDownWidth := -1;
            end;
          end;

          while ColumnCount > ReservedColumns + FListColumns.Count + 1 do
            Columns[ColumnCount - 1].Free;

          DataController.DataSource := ListSource;
        finally
          EndUpdate
        end;

        with ActiveProperties do
          if DropDownWidth > LDropDownWidth then
            LDropDownWidth := DropDownWidth;
        if Self.Width > LDropDownWidth then
          LDropDownWidth := Self.Width;

        if (FSavedDropDownWidth <= 0) or ((FDropDownWidth <= 0) and (LDropDownWidth > FSavedDropDownWidth)) then
          Width := LDropDownWidth
        else
          Width := FSavedDropDownWidth;

        CheckChanges;
        with ActiveProperties do begin
          TcxTreeListViewInfoCrack(ViewInfo).CalculateDefaultHeights;
          TcxTreeListViewInfoCrack(ViewInfo).DoCalculate;
          LDropDownHeight := (ViewInfo.DefaultRowHeight * AbsoluteCount) + ViewInfo.HeadersHeight;
        end;

        if LDropDownHeight > DropDownHeight then
          Height  := DropDownHeight
        else
          Height  := LDropDownHeight;

        if not ActiveProperties.PopupAutoSize then
          with PopupWindow.ViewInfo.GetClientExtent do
            PopupHeight := FcxDBTreeList.Height + Top + Bottom;

        LAlwaysExpandedLevel := OptionsBehavior.AlwaysExpandedLevel;
        OptionsView.ShowRoot := (LAlwaysExpandedLevel = 0) and (Root.Count <> 1);

        if LAlwaysExpandedLevel > -1 then begin
//          if FExpandedField.IsEmpty then
          FullCollapse;
          if (LAlwaysExpandedLevel = 0) and (Root.Count = 1) then
            Root.Items[0].Expand(False);
        end;
      end;
    finally
      FInternalUpdate := False;
    end;
  end;
end;

function TcxCustomMDLookupTreePopupEdit.GetActiveProperties: TcxCustomMDLookupTreePopupEditProperties;
begin
  Result := TcxCustomMDLookupTreePopupEditProperties(InternalGetActiveProperties);
end;

function TcxCustomMDLookupTreePopupEdit.GetPopupFocusedControl: TWinControl;
begin
  Result := FcxDBTreeList
end;

function TcxCustomMDLookupTreePopupEdit.GetPopupWindowClientPreferredSize: TSize;
begin
  with FcxDBTreeList do
    Result := cxSize(Width, Height);
end;

procedure TcxCustomMDLookupTreePopupEdit.HidePopup(Sender: TcxControl; AReason: TcxEditCloseUpReason);
begin
  FCloseUpReason := AReason;
  PopupWindow.ClosePopup;
end;

procedure TcxCustomMDLookupTreePopupEdit.InitializePopupWindow;
begin
  with FcxDBTreeList do begin
//    PopupWindow.ViewInfo.ClientRect.Width := FcxDBTreeList.Width;
//    PopupWindow.ViewInfo.ClientRect.Height := FcxDBTreeList.Height;
    FTreeViewStyle.Color := Style.Color;
    Styles.Content    := FTreeViewStyle;
    Styles.Background := FTreeViewStyle;

    OnKeyDown := cxDBTreeListKeyDown;
  end;

  InitializeByEditValue;

  inherited;
end;

function TcxCustomMDLookupTreePopupEdit.InternalSetText(const Value: string): Boolean;
begin
  Result := False;
end;

procedure TcxCustomMDLookupTreePopupEdit.PopupWindowClosed(Sender: TObject);
begin
  RestorePopupControlData;
  inherited PopupWindowClosed(Sender);
end;

procedure TcxCustomMDLookupTreePopupEdit.PopupWindowClosing(Sender: TObject);
begin
  FPrevPopupControlData.ActiveControl := PopupWindow.ActiveControl;
end;

procedure TcxCustomMDLookupTreePopupEdit.PositionPopupWindowChilds(const AClientRect: TRect);
begin
  with AClientRect do
  begin
    FcxDBTreeList.Left := Left;
    FcxDBTreeList.Top := Top;
    FcxDBTreeList.Width := Right - Left;
    FcxDBTreeList.Height := Bottom - Top;
  end;
end;

procedure TcxCustomMDLookupTreePopupEdit.PrepareEditValue(const ADisplayValue: TcxEditValue; out EditValue: TcxEditValue; AEditFocused: Boolean);
begin
  EditValue := Null;
end;

function TcxCustomMDLookupTreePopupEdit.CanDropDown: Boolean;
begin
  Result := True;
end;

procedure TcxCustomMDLookupTreePopupEdit.CloseUp(AReason: TcxEditCloseUpReason);
begin
  inherited;

  with ActiveProperties do
    if FcxDBTreeList <> nil then with FcxDBTreeList do begin
      OnKeyDown         := nil;
      Styles.Content    := nil;
      Styles.Background := nil;
    end;
end;

procedure TcxCustomMDLookupTreePopupEdit.Resize;
begin
  inherited;

  Properties.PopupMinWidth := Width;
//  Properties.PopupWidth    := Width;
end;

procedure TcxCustomMDLookupTreePopupEdit.RestorePopupControlData;
begin
  with FcxDBTreeList do begin
    if Width > Self.Width then
      FSavedDropDownWidth := Width
    else
      FSavedDropDownWidth := 0;

    while (FPrevPopupControlData.ActiveControl <> nil) and (FPrevPopupControlData.ActiveControl <> PopupWindow) do begin
      FPrevPopupControlData.ActiveControl.Perform(CM_EXIT, 0, 0);
      FPrevPopupControlData.ActiveControl := FPrevPopupControlData.ActiveControl.Parent;
    end;
    Visible := False;
    Parent := FPrevPopupControlData.Parent;
    Align := FPrevPopupControlData.Align;
    BoundsRect := FPrevPopupControlData.Bounds;
    Visible := FPrevPopupControlData.Visible;
  end;
end;

procedure TcxCustomMDLookupTreePopupEdit.SavePopupControlData;
begin
  with FcxDBTreeList do begin
    FPrevPopupControlData.Align   := Align;
    FPrevPopupControlData.Bounds  := BoundsRect;
    FPrevPopupControlData.Parent  := Parent;
    FPrevPopupControlData.Visible := Visible;
    FPrevPopupControlData.ActiveControl := nil;
  end;
end;

procedure TcxCustomMDLookupTreePopupEdit.SetupPopupWindow;
begin
  with FcxDBTreeList do begin
    SavePopupControlData;
    Parent := PopupWindow;
    Visible := True;
    inherited SetupPopupWindow;
    Align := alClient; // TODO
  end;
end;

{ TcxCustomMDCheckLookupTreeComboBox }

procedure TcxCustomMDCheckLookupTreeComboBox.cxDBTreeListAfterCreate;
begin
  inherited;

  with FcxDBTreeList, ActiveProperties do begin
    OptionsView.CheckGroups := True;
    Root.CheckGroupType := FTreeViewRootCheckGroupType;
    TcxDBItemDataBinding(Columns[cxLookupTreeComboCheckBoxResultFieldIndex].DataBinding).FieldName := FResultField;
    TcxDBItemDataBinding(Columns[cxLookupTreeComboCheckBoxGroupTypeFieldIndex].DataBinding).FieldName := FCheckGroupTypeField;
  end;
end;

procedure TcxCustomMDCheckLookupTreeComboBox.cxDBTreeListNodeCheckChanged(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; AState: TcxCheckBoxState);
begin
  if (not FInternalChecking) and (not TcxMDDBTreeListCrack(Sender).FInternalCheckChanging) then
    PostListValues;
end;

procedure TcxCustomMDCheckLookupTreeComboBox.InitializeByEditValue;
var
  LEditValue: String;
{ CHECK / RADIO, ENABLED / DISABLED }

  procedure FillChilds(AcxDBTreeListNode: TcxTreeListNode);
  var
    I: Integer;
    P: String;
    LValue: Variant;
    LPrevChecked, LChecked: Boolean;
  begin
    with ActiveProperties do
      for I := 0 to AcxDBTreeListNode.Count - 1 do
        with TcxDBTreeListNode(AcxDBTreeListNode.Items[I]) do begin
          P := VarToStr(Values[cxLookupTreeComboCheckBoxGroupTypeFieldIndex]);
          if ((Length(P) > 0) and (P[1] = 'R')) then
            CheckGroupType := ncgRadioGroup
          else
            CheckGroupType := ncgCheckGroup;

          if FResultField = '' then
            LValue := KeyValue
          else
            LValue := Values[cxLookupTreeComboCheckBoxResultFieldIndex];

          Enabled := VarIsPresent(LValue) and (not ((Length(P) > 1) and (P[2] = 'D')));
          LPrevChecked := Checked;
          LChecked := LPrevChecked or (VarIsPresent(LValue) and InArray(LEditValue, VarToStr(LValue), FKeysSeparator));
          if LPrevChecked <> LChecked then
            Checked := LChecked;

          FillChilds(AcxDBTreeListNode.Items[I]);
          if (TcxMDTreeListOptionsBehavior(AcxDBTreeListNode.TreeList.OptionsBehavior).AlwaysExpandedLevel = 0) and (CheckState = cbsGrayed) then
            Expand(False);
        end;
  end;
begin
  inherited;

  with FcxDBTreeList do begin
    if Count > 0 then begin
      BeginUpdate;
      try
        LEditValue := VarToStr(EditValue);
        Root.Checked := False;
        FillChilds(Root);
      finally
        EndUpdate;
      end;
    end;
  end;
end;

function TcxCustomMDCheckLookupTreeComboBox.EditValueFromList: Variant;
var
  LResult: String;

  procedure CheckChilds(AcxDBTreeListNode: TcxTreeListNode);
  var
    I: Integer;
    LValue: Variant;
  begin
    with ActiveProperties do
      for I := 0 to AcxDBTreeListNode.Count - 1 do
        with TcxDBTreeListNode(AcxDBTreeListNode.Items[I]) do begin
          if (CheckState = cbsUnChecked) then Continue;
          if Enabled and (CheckState = cbsChecked) and (FParentKeysIfPossible or (Count = 0)) then begin
            if FResultField.IsEmpty then
              LValue := KeyValue
            else
              LValue := Values[cxLookupTreeComboCheckBoxResultFieldIndex];

            if VarIsPresent(LValue) then
              LResult := ConcatIfNotEmpty(LResult, FKeysSeparator, VarToStr(LValue));
          end else
            CheckChilds(AcxDBTreeListNode.Items[I]);
        end;
  end;
begin
  LResult := '';
  CheckChilds(FcxDBTreeList.Root);
  if LResult.IsEmpty then
    Result := Null
  else
    Result := LResult;
end;

procedure TcxCustomMDCheckLookupTreeComboBox.InitializePopupWindow;
begin
  FInternalChecking := True;
  try
    inherited;
    FcxDBTreeList.OnNodeCheckChanged := cxDBTreeListNodeCheckChanged;
  finally
    FInternalChecking := False;
  end;
end;

procedure TcxCustomMDCheckLookupTreeComboBox.CloseUp(AReason: TcxEditCloseUpReason);
begin
  with ActiveProperties do
    if Assigned(FcxDBTreeList) then
      FcxDBTreeList.OnNodeCheckChanged := nil;
  inherited;
end;

function TcxCustomMDCheckLookupTreeComboBox.GetActiveProperties: TcxCustomMDCheckLookupTreeComboBoxProperties;
begin
  Result := TcxCustomMDCheckLookupTreeComboBoxProperties(InternalGetActiveProperties)
end;

function TcxCustomMDCheckLookupTreeComboBox.GetProperties: TcxCustomMDCheckLookupTreeComboBoxProperties;
begin
  Result := TcxCustomMDCheckLookupTreeComboBoxProperties(FProperties);
end;

procedure TcxCustomMDCheckLookupTreeComboBox.PostListValues;
begin
{$IFDEF PACKAGE}
  try
{$ENDIF}
  if (ActiveProperties.ReadOnly) or (not DataBinding.CanModify) then Exit;

  EditValue := EditValueFromList;
  PostEditValue;
{$IFDEF PACKAGE}
  except
    Assert(False);
  end;
{$ENDIF}
end;

{ TcxMDCheckLookupTreeComboBox }

class function TcxMDCheckLookupTreeComboBox.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMDCheckLookupTreeComboBoxProperties;
end;

function TcxMDCheckLookupTreeComboBox.GetActiveProperties: TcxMDCheckLookupTreeComboBoxProperties;
begin
  Result := TcxMDCheckLookupTreeComboBoxProperties(InternalGetActiveProperties)
end;

function TcxMDCheckLookupTreeComboBox.GetProperties: TcxMDCheckLookupTreeComboBoxProperties;
begin
  Result := TcxMDCheckLookupTreeComboBoxProperties(FProperties)
end;

procedure TcxMDCheckLookupTreeComboBox.SetProperties(Value: TcxMDCheckLookupTreeComboBoxProperties);
begin
{$IFDEF PACKAGE}
  try
{$ENDIF}
  FProperties.Assign(Value);
{$IFDEF PACKAGE}
  except
    Assert(False);
  end;
{$ENDIF}
end;

{ TcxMDDBCheckLookupTreeComboBox }

class function TcxMDDBCheckLookupTreeComboBox.GetDataBindingClass: TcxEditDataBindingClass;
begin
  Result := TcxDBLookupEditDataBinding;
end;

function TcxMDDBCheckLookupTreeComboBox.GetDataBinding: TcxDBEditDataBinding;
begin
  Result := TcxDBEditDataBinding(FDataBinding);
end;

procedure TcxMDDBCheckLookupTreeComboBox.SetDataBinding(Value: TcxDBEditDataBinding);
begin
{$IFDEF PACKAGE}
  try
{$ENDIF}
  FDataBinding.Assign(Value);
{$IFDEF PACKAGE}
  except
    Assert(False);
  end;
{$ENDIF}
end;

procedure TcxMDDBCheckLookupTreeComboBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(GetcxDBEditDataLink(Self));
end;

{ TcxEditRepositoryMDLookupTreeComboBoxItem }

class function TcxEditRepositoryMDLookupTreeComboBoxItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMDLookupTreeComboBoxProperties
end;

function TcxEditRepositoryMDLookupTreeComboBoxItem.GetProperties: TcxMDLookupTreeComboBoxProperties;
begin
  Result := inherited Properties as TcxMDLookupTreeComboBoxProperties;
end;

procedure TcxEditRepositoryMDLookupTreeComboBoxItem.SetProperties(Value: TcxMDLookupTreeComboBoxProperties);
begin
  inherited Properties := Value;
end;

{ TcxMDLookupDBTreeListColumns }

function TcxMDLookupDBTreeListColumns.Add: TcxMDLookupDBTreeListColumn;
begin
  Result := TcxMDLookupDBTreeListColumn(inherited Add)
end;

function TcxMDLookupDBTreeListColumns.GetItem(Index: Integer): TcxMDLookupDBTreeListColumn;
begin
  Result := TcxMDLookupDBTreeListColumn(inherited GetItem(Index))
end;

{ TcxMDLookupDBTreeListColumn }

procedure TcxMDLookupDBTreeListColumn.AssignTo(Dest: TPersistent);
begin
  TcxMDLookupDBTreeListColumn(Dest).Caption         := Caption;
  TcxMDLookupDBTreeListColumn(Dest).FieldName       := FieldName;
  TcxMDLookupDBTreeListColumn(Dest).Width           := Width;
  TcxMDLookupDBTreeListColumn(Dest).AutoWidthSizable:= AutoWidthSizable;
end;

{ TcxEditRepositoryMDCheckLookupTreeComboBoxItem }

class function TcxEditRepositoryMDCheckLookupTreeComboBoxItem.GetEditPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxMDCheckLookupTreeComboBoxProperties;
end;

function TcxEditRepositoryMDCheckLookupTreeComboBoxItem.GetProperties: TcxMDCheckLookupTreeComboBoxProperties;
begin
  Result := inherited Properties as TcxMDCheckLookupTreeComboBoxProperties;
end;

procedure TcxEditRepositoryMDCheckLookupTreeComboBoxItem.SetProperties(Value: TcxMDCheckLookupTreeComboBoxProperties);
begin
  inherited Properties := Value;
end;

{ TcxCustomMDLookupTreeComboBoxLookupData }

function TcxCustomMDLookupTreeComboBoxLookupData.GetItem(Index: Integer): string;
begin
  with TcxCustomMDLookupTreeComboBox(Edit) do
    if Assigned(FcxDBTreeList) then
      Result := ActiveProperties.GetNodeDisplayText(FcxDBTreeList, Index)
    else
      Result := '';
end;


function TcxCustomMDLookupTreeComboBoxLookupData.GetItemCount: Integer;
begin
  with TcxCustomMDLookupTreePopupEdit(Edit) do
    if Assigned(FcxDBTreeList) then
      Result := FcxDBTreeList.AbsoluteCount
    else
      Result := 0;
end;

constructor TcxCustomMDLookupTreeComboBoxLookupData.Create(AOwner: TPersistent);
begin
  inherited Create(nil);
  FOwner := AOwner;
  FCurrentKey := Null;
end;

//procedure TcxCustomMDLookupTreeComboBoxLookupData.DisableChanging;
//begin
//  Inc(Properties.FLockCount);
//end;

//procedure TcxCustomMDLookupTreeComboBoxLookupData.EnableChanging;
//begin
//  Dec(Properties.FLockCount);
//end;

function TcxCustomMDLookupTreeComboBoxLookupData.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

// IcxTextEditLookupData

function TcxCustomMDLookupTreeComboBoxLookupData.CanResizeVisualArea(var NewSize: TSize; AMaxHeight: Integer = 0; AMaxWidth: Integer = 0): Boolean;
begin
  Result := True;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.ChangeFocusedRecordIndex(AIndex: Integer);
begin
  Edit.FcxDBTreeList.AbsoluteItems[AIndex].Focused := True;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.CloseUp;
begin
//  DisableChanging;
//  try
//    ResetIncrementalFilter;
    FVisible := False;

//    Properties.LookupGridInitEvents(nil, nil, nil);
//    Properties.LookupGridUnlockMouseMove;
//    Properties.LookupGridDeinitialize;
//    Properties.UnlockDataChanged;
//  finally
//    EnableChanging;
//  end;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.Deinitialize;
begin
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.DropDown;
begin
//  DisableChanging;
//  try
//    Properties.LockDataChanged;
//    Properties.LookupGridInitialize;
//    Properties.LookupGridInitEvents(GridClick, GridFocusedRowChanged, GridCloseUp);
//    Properties.LookupGridInitLookAndFeel(Edit.PopupControlsLookAndFeel, Edit.GetBackgroundColor, Edit.VisibleFont);
//    Properties.LookupGridLockMouseMove;
    FVisible := True;
    SyncGrid;
    SyncSelected;
//  finally
//    EnableChanging;
//  end;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.DroppedDown(const AFindStr: string);
begin
//  Properties.LookupGridDroppedDown(AFindStr);
end;

function TcxCustomMDLookupTreeComboBoxLookupData.Find(const AText: string): Boolean;
begin
  Result := IndexOf(AText) <> -1;
//  Result := Properties.FindLookupText(AText);
end;

function TcxCustomMDLookupTreeComboBoxLookupData.GetActiveControl: TControl;
begin
  Result := Edit.FcxDBTreeList
//  Result := Properties.GetLookupGridActiveControl;
end;

function TcxCustomMDLookupTreeComboBoxLookupData.GetCurrentKey: TcxEditValue;
begin
  Result := FCurrentKey;
end;

function TcxCustomMDLookupTreeComboBoxLookupData.GetDisplayText(const AKey: TcxEditValue): string;
begin
  Result := Properties.EditValueToDisplayValue(AKey);
end;

function TcxCustomMDLookupTreeComboBoxLookupData.GetOnCurrentKeyChanged: TNotifyEvent;
begin
  Result := FOnCurrentKeyChanged;
end;

function TcxCustomMDLookupTreeComboBoxLookupData.GetOnSelectItem: TNotifyEvent;
begin
  Result := FOnSelectItem;
end;

function TcxCustomMDLookupTreeComboBoxLookupData.GetSelectedItem: Integer;
begin
  Result := FocusedRecordIndex;
end;

function TcxCustomMDLookupTreeComboBoxLookupData.GetVisualAreaPreferredSize(AMaxHeight: Integer; AWidth: Integer = 0): TSize;
//var
//  ARowCount: Integer;
begin
  Result := cxNullSize;

//  Result.cx := Properties.GetLookupGridVisualAreaPreferredWidth;
//  // auto correct
//  ARowCount := Properties.DropDownRows;
//  if (DataController <> nil) and (DataController.GetRowCount < ARowCount) then
//    ARowCount := DataController.GetRowCount;
//  if ARowCount < 1 then
//    ARowCount := 1;
//  Result.cy := Properties.GetLookupGridPopupHeight(ARowCount);
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.Go(ADirection: TcxEditLookupDataGoDirection; ACircular: Boolean);
var
  LcxDBTreeList: TcxMDDBTreeListCrack;
begin
  LcxDBTreeList := Edit.FcxDBTreeList;
  if (not Properties.FAllRequiredFieldsFound) or (LcxDBTreeList = nil) or (LcxDBTreeList.AbsoluteCount = 0) then Exit;

//  DisableChanging;
//  try
//    ResetIncrementalFilter;
//    UpdateDropDownCount;
//    Properties.LockDataChanged;
    try
      if ADirection = egdBegin then
        LcxDBTreeList.TopNode.Visible := True
      else
        if ADirection = egdEnd then
          LcxDBTreeList.LastNode.Visible := True
        else begin
          SyncGrid;
          if ADirection in [egdNext, egdPageDown] then begin
            if ACircular and LcxDBTreeList.IsEOF then
              LcxDBTreeList.TopNode.Visible := True
            else if ADirection = egdNext then
              LcxDBTreeList.GotoNext
            else
              LcxDBTreeList.GotoNextPage;
          end else begin
            if ACircular and LcxDBTreeList.IsBOF then
              LcxDBTreeList.LastNode.Visible := True
            else if ADirection = egdPrev then
              LcxDBTreeList.GotoPrev
            else
              LcxDBTreeList.GotoPrevPage;
          end;
        end;
    finally
//      Properties.UnlockDataChanged;
    end;
    DoSetCurrentKey(GetFocusedRecordIndex);
//    DoSetKeySelection(GetFocusedRecordIndex <> -1);
//    DoSelectItem;
//  finally
//    EnableChanging;
//  end;
end;

function TcxCustomMDLookupTreeComboBoxLookupData.IndexOf(AText: string; APartialCompare: Boolean): Integer;
var
  ACount, I, LLength: Integer;
  LItem: String;
begin
  Result := -1;
  if AText.IsEmpty then Exit;
  AText := AnsiUpperCase(AText);

  ACount := GetItemCount;
  LLength := AText.Length;
  for I := 0 to ACount - 1 do begin
    LItem := AnsiUpperCase(GetItem(I));
    if APartialCompare then begin
      if Properties.IncrementalSearchContainMode then begin
        if Pos(AText, LItem) > 0 then
          Exit(I)
      end else
        if AText = LeftStr(LItem, LLength) then
          Exit(I)
    end else
      if AText = LItem then
        Exit(I)
  end;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.Initialize(AVisualControlsParent: TWinControl);
begin
//  if Edit.FcxDBTreeList <> nil then
//    Edit.FcxDBTreeList.Parent := AVisualControlsParent;
end;

function TcxCustomMDLookupTreeComboBoxLookupData.IsEmpty: Boolean;
begin
  Result := not (Properties.FAllRequiredFieldsFound) or (GetItemCount = 0);
end;

function TcxCustomMDLookupTreeComboBoxLookupData.IsMouseOverList(const P: TPoint): Boolean;
begin
  Result := PtInRect(Edit.FcxDBTreeList.BoundsRect, Edit.FcxDBTreeList.ScreenToClient(P));
end;

function TcxCustomMDLookupTreeComboBoxLookupData.Locate(var AText, ATail: string; ANext: Boolean): Boolean;

//  function SetGridFilter(AItemIndex: Integer; const AText: string): Integer;
//  var
//    APrevIncrementalFilterText: string;
//  begin
//    if AText = '' then
//    begin
//      ResetIncrementalFilter;
//      Result := DataController.FindRecordIndexByText(0, AItemIndex, AText, False{APartialCompare}, False, True);
//    end
//    else
//    begin
//      APrevIncrementalFilterText := DataController.GetIncrementalFilterText;
//      Result := DataController.SetIncrementalFilter(AItemIndex, AText);
//      if DataController.FilteredRecordCount = 0 then
//      begin
//        if Properties.DropDownListStyle <> lsEditList then
//          DataController.SetIncrementalFilter(AItemIndex, APrevIncrementalFilterText);
//        Result := -1;
//      end;
//    end;
//    UpdateDropDownCount;
//  end;

var
  ARecordIndex: Integer;
  S: string;
begin
  Result := False;
//  DisableChanging;
//  try
    if Properties.FAllRequiredFieldsFound then begin
      // TODO: Next
//      if FVisible and Properties.IncrementalFiltering {and (Properties.DropDownListStyle <> lsFixedList)} then
//        ARecordIndex := SetGridFilter(AItemIndex, AText)
//      else
        //DataController.FindRecordIndexByText(0, AItemIndex, AText, True{APartialCompare}, False, True);

      ARecordIndex := IndexOf(AText, True);

      if ARecordIndex <> -1 then begin
        ChangeFocusedRecordIndex(ARecordIndex);

        DoSetCurrentKey(ARecordIndex);
        Result := True;
        S := GetItem(ARecordIndex);
{ -=MD=-
        AText := Copy(S, 1, Length(AText));
        ATail := Copy(S, Length(AText) + 1, Length(S));
}
        if not Properties.IncrementalSearchContainMode then begin
          AText := Copy(S, 1, Length(AText));
          ATail := Copy(S, Length(AText) + 1, Length(S))
        end else
          ATail := '';

//        DoSetKeySelection(True);
      end
//      else
//        DoSetKeySelection(False);
    end;
//  finally
//    EnableChanging;
//  end;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.PositionVisualArea(const AClientRect: TRect);
begin
//  with AClientRect do
//  begin
//    Edit.FcxDBTreeList.SetBounds(Left, Top, Right - Left, Bottom - Top);
//    if Edit.FcxDBTreeList.HandleAllocated then
//      Edit.FcxDBTreeList.SetScrollWidth(Edit.FcxDBTreeList.ScrollWidth);
//  end;

//  if Properties.GetLookupGridControl <> nil then
//  begin
//    with AClientRect do
//      Properties.GetLookupGridControl.SetBounds(Left, Top, Right - Left, Bottom - Top);
//    Properties.GetLookupGridControl.HandleNeeded;
//    Properties.LookupGridMakeFocusedRowVisible;
//  end;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.PropertiesChanged;
begin
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.SelectItem;
begin
  if IsKeySelected then
  begin
    DoSetCurrentKey(FocusedRecordIndex);
//    DoSelectItem;
  end;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.SetCurrentKey(const AKey: TcxEditValue);
begin
  FCurrentKey := AKey;
  SyncGrid;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.SetOnCurrentKeyChanged(Value: TNotifyEvent);
begin
  FOnCurrentKeyChanged := Value;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.SetOnSelectItem(Value: TNotifyEvent);
begin
  FOnSelectItem := Value;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.SetSelectedItem(Value: Integer);
var
  LcxDBTreeList: TcxMDDBTreeListCrack;
begin
  LcxDBTreeList := Edit.FcxDBTreeList;
  if Assigned(LcxDBTreeList) then
    LcxDBTreeList.AbsoluteItems[Value].Focused := True;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.TextChanged;
begin
//  if Edit.CanSynchronizeLookupData then
//  begin
//    DisableChanging;
//    try
//      SyncSelected;
//    finally
//      EnableChanging;
//    end;
//  end;
end;

// end IcxTextEditLookupData

//procedure TcxCustomMDLookupTreeComboBoxLookupData.DoCurrentKeyChanged;
//begin
//  if Assigned(FOnCurrentKeyChanged) then
//    FOnCurrentKeyChanged(Self);
//end;

//procedure TcxCustomMDLookupTreeComboBoxLookupData.DoSelectItem;
//begin
//  if Assigned(FOnSelectItem) then
//    FOnSelectItem(Self);
//end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.DoSetCurrentKey(ARecordIndex: Integer);
begin
  with TcxDBTreeListNode(Edit.FcxDBTreeList.AbsoluteItems[ARecordIndex]), Properties do
    if FResultField.IsEmpty then
      FCurrentKey := KeyValue
    else
//      FCurrentKey := Values[cxLookupTreeResultFieldIndex]
      FCurrentKey := FDataSet.Lookup(FKeyField, KeyValue, FResultField);
end;

//procedure TcxCustomMDLookupTreeComboBoxLookupData.DoSetKeySelection(AKeySelected: Boolean);
//begin
//  if DataController <> nil then
//    DataController.SyncSelected(AKeySelected);
//end;

//procedure TcxCustomMDLookupTreeComboBoxLookupData.DoSyncGrid;
//begin
//  if DataController <> nil then
//    ChangeFocusedRecordIndex(Integer(GetCurrentKey));
//end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.GridClick(Sender: TObject);
begin
  SelectItem;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.GridCloseUp(Sender: TObject; AAccept: Boolean);
begin
  if AAccept then
    Edit.CloseUp(crEnter)
  else
    Edit.CloseUp(crCancel)
end;

//procedure TcxCustomMDLookupTreeComboBoxLookupData.GridFocusedRowChanged(Sender: TObject);
//begin
  // TODO: ImmediateUpdateText
//end;

function TcxCustomMDLookupTreeComboBoxLookupData.IsKeySelected: Boolean;
var
  LcxDBTreeList: TcxMDDBTreeListCrack;
begin
  LcxDBTreeList := Edit.FcxDBTreeList;
  Result := (LcxDBTreeList <> nil) and (LcxDBTreeList.SelectionCount = 1) and
    (LcxDBTreeList.FocusedNode = LcxDBTreeList.Selections[0]);
end;

function TcxCustomMDLookupTreeComboBoxLookupData.LocateText(const AText: string): Boolean;
var
  ARecordIndex: Integer;
begin
  Result := False;
  if Properties.FAllRequiredFieldsFound then begin
    // TODO: Search
    //ARecordIndex := DataController.FindRecordIndexByText(0, AItemIndex, AText, False{APartialCompare}, False, True);
    ARecordIndex := IndexOf(AText, False);
    if ARecordIndex <> -1 then begin
      ChangeFocusedRecordIndex(ARecordIndex);
      DoSetCurrentKey(ARecordIndex);
      Result := True;
//      DoSetKeySelection(True);
    end
//    else
//      DoSetKeySelection(False);
  end;
end;

//procedure TcxCustomMDLookupTreeComboBoxLookupData.ResetIncrementalFilter;
//begin
//  if DataController <> nil then
//    DataController.ResetIncrementalFilter;
//end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.SyncGrid;
var
  LcxDBTreeList: TcxMDDBTreeListCrack;
begin
  LcxDBTreeList := Edit.FcxDBTreeList;
  if Assigned(LcxDBTreeList) then begin
//    DoSyncGrid;
    LcxDBTreeList.MakeNodeVisible(LcxDBTreeList.FocusedNode);
  end;
end;

procedure TcxCustomMDLookupTreeComboBoxLookupData.SyncSelected;
var
  ASelected: Boolean;
  ARecordIndex: Integer;
  S: string;
begin
//  if Properties.FInGridDataChanged then Exit;

//  if FVisible and Properties.IncrementalFiltering then begin
//    if (DataController <> nil) and DataController.ChangeIncrementalFilterText(Edit.Text) then
//      UpdateDropDownCount;
//  end;

  ASelected := False;
  if Properties.FAllRequiredFieldsFound then begin
    ARecordIndex := FocusedRecordIndex;
    if ARecordIndex <> -1 then begin
      S := GetItem(ARecordIndex);
      ASelected := DataCompareText(S, Edit.Text, False);
    end;

    if not ASelected or not FVisible then // Properties.EditingStyle in [esEditList, esFixedList]
    begin
//      ARecordIndex := DataController.FindRecordIndexByText(0, AItemIndex, Edit.Text, False{APartialCompare}, False, True);
      ARecordIndex := IndexOf(Edit.Text, False);
      if ARecordIndex <> -1 then begin
//        ASelected := True;
        ChangeFocusedRecordIndex(ARecordIndex);
        DoSetCurrentKey(ARecordIndex);
      end;
    end;
  end;
//  DoSetKeySelection(ASelected);
end;

//procedure TcxCustomMDLookupTreeComboBoxLookupData.UpdateDropDownCount;
//var
//  AGridRowCount, ARowCount, AHeight: Integer;
//begin
//  if not FVisible then Exit;
//  if Edit.PopupWindow.IsVisible and (DataController <> nil) then
//  begin
//    AGridRowCount := DataController.GetRowCount;
//    ARowCount := Properties.DropDownRows;
//    if AGridRowCount > ARowCount then
//      AGridRowCount := ARowCount;
//    if AGridRowCount < 1 then
//      AGridRowCount := 1;
//    AHeight := Properties.GetLookupGridPopupHeight(AGridRowCount);
//    if (Properties.GetLookupGridControl <> nil) and
//      (Properties.GetLookupGridControl.ClientHeight <> AHeight) then
//    begin
//      if not Edit.PopupSizeChanged then
//        Edit.ResetPopupHeight;
//      Edit.SetupPopupWindow;
//    end;
//  end;
//end;

//function TcxCustomMDLookupTreeComboBoxLookupData.GetDataController: TcxCustomDataController;
//begin
//  if Assigned(Edit.FcxDBTreeList) then
//    Result := Edit.FcxDBTreeList.DataController
//  else
//    Result := nil
//end;

function TcxCustomMDLookupTreeComboBoxLookupData.GetEdit: TcxCustomMDLookupTreeComboBox;
begin
  Result := TcxCustomMDLookupTreeComboBox(FOwner);
end;

function TcxCustomMDLookupTreeComboBoxLookupData.GetFocusedRecordIndex: Integer;
var
  LcxDBTreeList: TcxMDDBTreeListCrack;
begin
  LcxDBTreeList := Edit.FcxDBTreeList;
  if (LcxDBTreeList <> nil) and (LcxDBTreeList.FocusedNode <> nil) then
    Result := LcxDBTreeList.FocusedNode.AbsoluteIndex
  else
    Result := -1;
end;

function TcxCustomMDLookupTreeComboBoxLookupData.GetProperties: TcxCustomMDLookupTreeComboBoxProperties;
begin
  Result := Edit.ActiveProperties;
end;

initialization
  GetRegisteredEditProperties.Register(TcxMDLookupTreeComboBoxProperties, cxSEditRepositoryLookupTreeComboBoxEditItem);
  GetRegisteredEditProperties.Register(TcxMDCheckLookupTreeComboBoxProperties, cxSEditRepositoryCheckLookupTreeComboBoxEditItem);

  RegisterClasses
  (
    [
      TcxMDLookupTreeComboBoxProperties,
      TcxMDLookupTreeComboBox,
      TcxEditRepositoryMDLookupTreeComboBoxItem,
      TcxEditRepositoryMDCheckLookupTreeComboBoxItem,
      TcxMDDBLookupTreeComboBox,
      TcxMDMultiFieldsDBLookupTreeComboBox,

      TcxMDCheckLookupTreeComboBoxProperties,
      TcxMDCheckLookupTreeComboBox,
      TcxMDDBCheckLookupTreeComboBox
    ]
  );

//  RegisterEditRepositoryItem(TcxEditRepositoryLabel, scxSEditRepositoryLabelItem);

finalization
  GetRegisteredEditProperties.Unregister(TcxMDLookupTreeComboBoxProperties);
  GetRegisteredEditProperties.Unregister(TcxMDCheckLookupTreeComboBoxProperties);
  UnRegisterClasses
  (
    [
      TcxMDLookupTreeComboBoxProperties,
      TcxMDLookupTreeComboBox,
      TcxEditRepositoryMDLookupTreeComboBoxItem,
      TcxEditRepositoryMDCheckLookupTreeComboBoxItem,
      TcxMDDBLookupTreeComboBox,
      TcxMDMultiFieldsDBLookupTreeComboBox,

      TcxMDCheckLookupTreeComboBoxProperties,
      TcxMDCheckLookupTreeComboBox,
      TcxMDDBCheckLookupTreeComboBox
    ]
  );

end.
