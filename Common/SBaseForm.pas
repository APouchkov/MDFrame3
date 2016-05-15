unit SBaseForm;
{$I config.inc}

{******************************************************************************}
{**  Базовая форма                                                           **}
{**  (c) -=MD=-, 2006                                                        **}
{**  Подал идею: Соковнин Андрей                                             **}
{**  Дата идеи: 15.08.2006                                                   **}
{******************************************************************************}

interface

{$IFDEF PACKAGE}
  Этот юнит не может быть использован при компиляции пакетов
{$ENDIF}

uses
  Types, UITypes, Generics.Collections,
  SBaseDataModule, SBaseDataHelperInternal, SBaseDataSourceControls, SBaseExpression, SBaseProgress,
  NamedVariables,
{$IFDEF SERVER_UPDATE_NOTIFICATIONS}
  SBaseEvents,
{$ENDIF}
  SBaseStringFunctions, SBaseFormDefinitions, SBaseControls, SBaseItemReactions,
  SBaseCxUtils, SBaseDataChanges, SBaseUtils, hsDialogs,

  Windows, Themes, Messages, SysUtils, StrUtils, Variants, Classes, Graphics, Controls, PropFilerEh,
  Forms, TypInfo, ComCtrls, Mask, ExtCtrls, StdCtrls, RegularExpressions,
  SysConst, Buttons, Clipbrd, ImgList,
  cxLookAndFeels, cxEditRepositoryItems, Dialogs, Math, dxStatusBar, dxMDStatusBar, cxSplitter
{IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter, fs_idbrtti, fs_iformsrtti, fs_iclassesrtti, fs_igraphicsrtti, fs_ievents
{ENDIF}

{$IFDEF FASTSCRIPT_RTTI}
  , fsFunction_TcxControls
{$ENDIF}

  , DB, FieldDataLink, DBCommon,

  dxBar, dxBarExtDBItems, dxMDBarButton, cxLookAndFeelPainters, dxSkinsForm, dxSkinsDefaultPainters,
  dxMDBar,

  cxControls, cxGrid, cxGridLevel, cxGridTableView, cxGridDBTableView, cxEdit,
  cxDBEdit, cxTL, cxDBTL, cxGridCustomView, cxGridCustomTableView, cxGridDBBandedTableView,
  cxGridDBDataDefinitions, cxInplaceContainer, cxBarEditItem, cxDBLookupComboBox,
  cxCustomData, cxDBData, cxContainer, cxStyles, cxButtonEdit, cxMaskEdit, cxCheckBox, cxImageComboBox,
  cxGraphics, cxPC, cxPCPainters, cxDropDownEdit, cxUserCheckBox, cxCheckLookupComboBox,
  cxVGrid, cxDBVGrid, System.Actions, ActnList, cxMultiFieldsDBDataBinding,
  cxTextEdit, cxClasses;

const
  WM_CUT               = WM_USER + $1001;
  WM_COPY              = WM_USER + $1002;
  WM_PASTE             = WM_USER + $1003;

  WM_TABLE             = WM_USER + $1004;
  WM_PRINT             = WM_USER + $1005;
  WM_EXCEL             = WM_USER + $1006;
  WM_FILTER            = WM_USER + $1007;
  WM_PROPERTIES        = WM_USER + $1008;
  WM_OBJECTINSPECTOR   = WM_USER + $1009;
  WM_DATESPERIOD       = WM_USER + $100A;

  WM_CONFIGCHANGED     = WM_USER + $101B;

  Privileges_None       = 0;

  Privilege_Select_Bit  = 1;
  Privilege_Insert_Bit  = 2;
  Privilege_Update_Bit  = 4;
  Privilege_Delete_Bit  = 8;
  Privilege_Init_Bit    = 16;   // Изменение пустого поля
  Privilege_Clear_Bit   = 32;   // Обнуление непустого поля
  Privilege_Erase_Bit   = 64;   // ???

  Privileges_ForRecord  = 255;  // Допустимые привилегии для записи датасета
  Privilege_Preview_Bit = 256;  // Право вывода формы в программе
  Privilege_Export_Bit  = 512;  // Право экспорта данных формы во внешние форматы

  DefaultTaskButtonWidth = 160;


  SAbortSelect              : String = 'Select Aborted';
  SConst_SubClassParams     : String = 'SubClassParams';
  SConst_InstanceParams     : String = 'InstanceParams';
  SConst_IdentityParams     : String = 'IdentityParams';
  SConst_ResultFields       : String = 'ResultFields';

  SBFSelectModeActive       : String = 'SELECT#MODE#ACTIVE';
  SBFSelectCallBackProc     : String = 'SELECT#CALLBACK#PROCEDURE';
  SBFSelectCallBackScript   : String = 'SELECT#CALLBACK#SCRIPT';

  SBFTestRunMode            : String = 'TEST#RUN#MODE';
  SBFScriptDebugMode        : String = 'SCRIPT#DEBUG#MODE';

  SBFSM_Single              : String = 'Single:Default';
  SBFSM_SingleRepeatAble    : String = 'Single:RepeatAble';
  SBFSM_Multiply            : String = 'Multiply:Array';
  SBFSM_MultiplyRepeatAble  : String = 'Multiply:RepeatAble';

  SConst_Log_Type           : String = 'Log:Type';
  SConst_Log_Message        : String = 'Log:Message';

  SVar_CaptionPathDelimeter : String = ' :: ';

  WM_WindowOnShow = WM_USER + $2000;

  lgMaxLines = 1500;
  lgWarning  = 'W';
  lgError    = 'E';
  lgInfo     = 'I';

  SConst_SQLConnectionProvider: String = 'SQLConnectionProvider';

type
  ESBaseAbortSelect = class(Exception);
  TSBaseExceptionProcedure = procedure (const AException: String);

  TmdiToolButton = (mtbCut, mtbCopy, mtbPaste, mtbRefresh, mtbDatesPeriod, mtbFilter, mtbProperties, mtbPrint, mtbExcel);
  TmdiToolButtons = Set Of TmdiToolButton;

type
  TSBaseFrm = class;
  TSBaseFrmClass = class of TSBaseFrm;

{ Vertical Line With Close Button }
  TVerticalCloseSeparator = class(TWinControl)
  private
    FCloseButton: TSpeedButton;
    procedure SetOnCloseButtonClick(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property OnCloseButtonClick: TNotifyEvent write SetOnCloseButtonClick;
  end;

  TSBaseMainFormMDIInterface = class(TComponent)
    procedure WindowButtonClick(Sender: TObject);
    procedure BarManagerShowToolbarsPopup(Sender: TdxBarManager; PopupItemLinks: TdxBarItemLinks);

    procedure WindowsCascadeClick(Sender: TObject);
    procedure WindowsTileHorClick(Sender: TObject);
    procedure WindowsTileVertClick(Sender: TObject);
    procedure WindowsMinimizeClick(Sender: TObject);
    procedure WindowsArrangeClick(Sender: TObject);

    procedure WindowMinimizeClick(Sender: TObject);
    procedure WindowRestoreClick(Sender: TObject);
    procedure WindowMaximizeClick(Sender: TObject);
    procedure WindowCloseClick(Sender: TObject);

    procedure ToolButtonWindowClick(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure ToolButtonRefreshClick(Sender: TObject);

    procedure WindowsStyleButtonClick(Sender: TObject);

    procedure dxStyleButtonClick(Sender: TObject);
    procedure dxSkinDefaultButtonClick(Sender: TObject);
    procedure dxSkinButtonClick(Sender: TObject);

    procedure LogPanelCloseClick(Sender: TObject);
    procedure LogCopyLineClick(Sender: TObject);
    procedure LogCopyAllClick(Sender: TObject);
    procedure LogDeleteLineClick(Sender: TObject);
    procedure LogDeleteAllClick(Sender: TObject);
    procedure LogPopupMenuOnPopup(Sender: TObject);

{$IFDEF SERVER_UPDATE_NOTIFICATIONS}
    procedure NotificationMessage(Sender: TSBaseNotificationTrap; Message: TSBaseEventMessage);
{$ENDIF}
  private
    FChilds       : TList<TForm>;
    FButtons      : TList<TdxBarButton>;

    FBarManager   : TdxBarManager;
    FTaskBar      : TdxBar;
//    FWindowsList  : TdxBarListItem;

    FChangeNotificationsCach: TChangeNotificationsCach;

    FLogPanel     : TPanel;
    FLogGrid      : TcxGrid;
    FLogSplitter  : TcxSplitter;
    FLogGridView  : TcxGridTableView;

    FdxBarWindowsStylesSubItem: TdxBarSubItem;
    FdxBarWindowsStylesListItem: TdxMDBarListItem;

    FdxDefaultStyle : Boolean;
    FdxSkinController: TdxSkinController;
    FdxBarDevExpressSkinsSubItem: TdxBarSubItem;

    FBarManagerShowToolbarsPopup: TdxBarShowPopupEvent;
    FWinRestore, FWinMaximize, FWinMinimize, FWinClose: TdxBarButton;
    FWinPrev, FWinNext, FCut, FCopy, FPaste, FRefresh, FDatesPeriod, FFilter, FProperties, FObjectInspector, FPrint, FExcel: TdxBarButton;
    FlgCopyLine, FlgCopyAll, FlgDeleteLine, FlgDeleteAll: TdxBarButton;

//    FSkins: TdxBarSubItem;

    FBusy         : Boolean;

    procedure SkinChangedNotify;
    function GetSkinButtons(AName: String): TdxBarButton;
    function GetActiveSkin: String;
    procedure SetActiveSkin(const Value: String);
    function GetActiveForm: TForm;
    function GetActiveStyle: String;
    procedure SetActiveStyle(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RegisterWindow(AForm: TForm);
    procedure WindowOnDestroy(AForm: TForm);
    procedure WindowOnActivate(AForm: TForm; Atbs: TMDIToolButtons);
    procedure WindowUpdated(AForm: TForm);

    procedure SendToActiveWindow(AMsg, ALo, AHi: Integer);
    procedure BroadCast(Msg: Integer;Lo: Integer; Hi: Integer);
    procedure BroadCastDataChanges(const ATablesChanges: TTablesChanges; AExternalEvent: Boolean = False; AExcludeForm: TSBaseFrm = nil);

    procedure Refresh;
    function Log(ALogLevel: Char; AMessages: String; AProcessMessages: Boolean = False): Boolean;

    procedure SaveWindows;
    procedure RestoreWindows;

    property  Childs : TList<TForm> read FChilds;
    property  Buttons: TList<TdxBarButton> read FButtons;
    property  dxDefaultStyle: Boolean read FdxDefaultStyle write FdxDefaultStyle Default True;
    property  dxSkinController: TdxSkinController read FdxSkinController;
    property  ActiveStyle: String read GetActiveStyle write SetActiveStyle;
    property  ActiveSkin: String read GetActiveSkin write SetActiveSkin;
    property  SkinButtons[Name: String]: TdxBarButton read GetSkinButtons;
  published
    property ActiveForm: TForm read GetActiveForm;
  end;

{ Base Form }

  TSBaseVirtualSubClassInfo = packed record
    IdentityParams    : Variant;
{$IFDEF DEBUG}
    DfmText           : String;
{$ENDIF}
    DFM               : TMemoryStream;
    Script            : String;
  public
    procedure Init; inline;
    procedure Release; inline;
  end;
  PSBaseVirtualSubClassInfo = ^TSBaseVirtualSubClassInfo;

  TSBaseVirtualInstanceInfo = packed record
    IdentityParams    : Variant;
    Privileges        : SmallInt;
    Identity          : String;
{$IFDEF DEBUG}
    DfmText           : String;
{$ENDIF}
    DFM               : TMemoryStream;
    Script            : String;
  public
    procedure Init; inline;
    procedure Release; inline;
  end;
  PSBaseVirtualInstanceInfo = ^TSBaseVirtualInstanceInfo;

  TSBaseVirtualClassInfo = class
  private
    VirtualClassName        : String;
  public
    TestRun                 : Boolean;

    PersistentClass         : TSBaseFrmClass; // Класс реального родителя виртуальной формы
    ParentClassName         : String;
    SubClassParams          : String;
    InstanceParams          : String;
    Unique                  : Boolean;
//    ClassProcedure          : String;
    Version                 : Byte;
    Privileges              : SmallInt;

    CreateFormParamsAllowed : String;
    CreateFormParamsRequired: String;

{$IFDEF DEBUG}
    DfmText                 : String;
{$ENDIF}
    DFM                     : TMemoryStream;
    Script                  : String;
    BeforeCreateProcedure   : String;
//    SubClassProcedure       : String;
    SubClasses              : Array of TSBaseVirtualSubClassInfo;
//    InstanceProcedure       : String;
    Instances               : Array of TSBaseVirtualInstanceInfo;
  private
    function GetVirtualSubClassInfo(const AParams: TNamedVariants; ATestRun: Boolean{$IFDEF DEBUG}; ADebugInfo: TStrings = nil; ACompile: Boolean = False{$ENDIF}): PSBaseVirtualSubClassInfo;
    function GetVirtualInstanceInfo(const AParams: TNamedVariants; ATestRun: Boolean{$IFDEF DEBUG}; ADebugInfo: TStrings = nil; ACompile: Boolean = False{$ENDIF}): PSBaseVirtualInstanceInfo;
  public
    constructor Create(const AVirtualClassName: String; APrivileges: SmallInt);
    destructor Destroy; override;
    procedure Release;

//    function IsEmpty: Boolean; inline;
  end;
//  PSBaseVirtualClassInfo = ^TSBaseVirtualClassInfo;

  TFormItemForSave  = (frsvLeftTop, frsvWidth, frsvHeight, frsvFormItems, frsvUserDefined);
  TFormItemsForSave = Set of TFormItemForSave;

  TSBaseFrmUserVariable = record
  private
    FName   : String;
    FVersion: Integer;
    FValue  : Variant;
  public
    property Name: String read FName write FName;
    property Version: Integer read FVersion write FVersion;
    property Value: Variant read FValue write FValue;
  end;

  TSBaseFrmUserVariables = record
  private
    function  GetCount: Integer;
    procedure SetCount(const Value: Integer);
  public
    Variables: Array Of TSBaseFrmUserVariable;

    constructor Create(AClear: Boolean);
    procedure Clear;

    function ExtractVariable: TSBaseFrmUserVariable; overload;
    function ExtractVariable(AName: String; AVersion: Integer = 0): Variant; overload;
    property Count: Integer read GetCount write SetCount;
  end;

  TSBaseGetUserClassVariablesFunction = function(const AClassName: String; const AClassVersion: Byte; const AClassIdentity: String): TSBaseFrmUserVariables of object;
  TSBaseSetUserClassVariableProcedure = procedure(const AClassName: String; const AClassVersion: Byte; const AClassIdentity, AVariableName: String; const AVariableVersion: Byte; AVariableValue: Variant) of object;

  TSBaseFormProperties = class(TComponent)
  private
    FSQLClassInfo   : String;
    FSQLSubClassInfo: String;
    FSQLInstanceInfo: String;
{IFDEF PACKAGE
    FSQLTestRunInfo : String;
ENDIF}

    FFontName: String;
    FSkinFiles: TStringList;
    FIcon16List: TCustomImageList;
    FIcon14List: TCustomImageList;
    FImageList: TcxNamedImageCollection;

    FIconIndex_View, FIconIndex_Edit, FIconIndex_New, FIconIndex_Post, FIconIndex_Restore, FIconIndex_Delete, FIconIndex_Copy, FIconIndex_Cut, FIconIndex_Paste, FIconIndex_MarkAsDeleted, FIconIndex_UnMarkFromDeleted, FIconIndex_Refresh,
    FIconIndex_ShowTree, FIconIndex_FindInTree, FIconIndex_DatesPeriod, FIconIndex_FilterBox, FIconIndex_FilterClear, FIconIndex_GroupBox, FIconIndex_GroupingClear, FIconIndex_ExpandAll, FIconIndex_CollapseAll,
    FIconIndex_Details, FIconIndex_Properties, FIconIndex_ObjectInspector,
    FIconIndex_FitToWidth, FIconIndex_FitToHeight, FIconIndex_FitToScreen,
    FIconIndex_Submit, FIconIndex_Select, FIconIndex_Take, FIconIndex_Backward, FIconIndex_Forward,
    FIconIndex_EditButtonCalc, FIconIndex_EditButtonDate,

    FIconIndex_Wizard, FIconIndex_Printer, FIconIndex_Excel, FIconIndex_FastReport,
    FIconIndex_FileOpen, FIconIndex_FileSave,
    FIconIndex_Run, FIconIndex_Pause, FIconIndex_Step, FIconIndex_BreakPoint, FIconIndex_Watches,
    FIconIndex_GutterExecutable, FIconIndex_GutterCurrent, FIconIndex_GutterError, FIconIndex_GutterBreakPoint,

    FIconIndex_MsgInfo, FIconIndex_MsgWarning, FIconIndex_MsgError,
    FIconIndex_WindowRestore, FIconIndex_WindowMinimize, FIconIndex_WindowMaximize, FIconIndex_WindowClose, FIconIndex_WindowPrev, FIconIndex_WindowNext,
    FIconIndex_WindowsCascade, FIconIndex_WindowsTileHor, FIconIndex_WindowsTileVert, FIconIndex_WindowsMinimize, FIconIndex_WindowsArrange,
    FIconIndex_DetailsNone, FIconIndex_DetailsRight, FIconIndex_DetailsBottom,
    FIconIndex_FolderClosed, FIconIndex_FolderOpened, FIconIndex_File
    : Integer;

    FIcon14Index_CheckBox_UnChecked, FIcon14Index_CheckBox_Checked, FIcon14Index_CheckBox_ParentChecked, FIcon14Index_CheckBox_ChildChecked,
    FIcon14Index_Root, FIcon14Index_NodeRoot, FIcon14Index_NodeParent,
    FIcon14Index_Find
    : Integer;

    FFont_Inspector: String;

    FdxDefaultStyles: TControlColors;
    FEditRepository_CheckBox: TcxEditRepositoryItem;
    FEditRepository_FileExtensions: TcxEditRepositoryImageComboBoxItem;

    //procedure cxGridDBViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
    procedure cxGridCustomTableViewCustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    procedure cxTreeListCustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);

    procedure Set_DefaultStyle_HighlightRecord(const AValue: TcxStyle);
    procedure Set_DefaultStyle_DeletedRecord(const Value: TcxStyle);
    procedure Set_DefaultStyle_ReadOnly(const Value: TcxStyle);
    procedure Set_DefaultStyle_WriteAble(const Value: TcxStyle);
    procedure SetIconIndex_EditButtonCalc(const Value: Integer);
    procedure SetIconIndex_EditButtonDate(const Value: Integer);
    procedure SetImageList(const Value: TcxNamedImageCollection);
    procedure SetSkinFile(const AValue: String);
    procedure Set_DefaultStyle_cxVGridHeader(const AValue: TcxStyle);
  public
    InterfaceVariables: TNamedVariants;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property dxDefaultStyles: TControlColors read FdxDefaultStyles write FdxDefaultStyles;
  published
    property SQLClassInfo   : String read FSQLClassInfo     write FSQLClassInfo;
    property SQLSubClassInfo: String read FSQLSubClassInfo  write FSQLSubClassInfo;
    property SQLInstanceInfo: String read FSQLInstanceInfo  write FSQLInstanceInfo;
{IFDEF PACKAGE
    property SQLTestRunInfo : String read FSQLTestRunInfo   write FSQLTestRunInfo;
ENDIF}

  //    property InterfaceVariables: TNamedVariants read FInterfaceVariables write FInterfaceVariables;

    property FontName: String read FFontName write FFontName;
    property SkinFile: String write SetSkinFile;

    property Icon14List: TCustomImageList read FIcon14List write FIcon14List;
    property Icon16List: TCustomImageList read FIcon16List write FIcon16List;
    property ImageList: TcxNamedImageCollection read FImageList write SetImageList;

    property IconIndex_View: Integer read FIconIndex_View write FIconIndex_View;
    property IconIndex_Edit: Integer read FIconIndex_Edit write FIconIndex_Edit;
    property IconIndex_New: Integer read FIconIndex_New write FIconIndex_New;
    property IconIndex_Copy: Integer read FIconIndex_Copy write FIconIndex_Copy;
    property IconIndex_Post: Integer read FIconIndex_Post write FIconIndex_Post;
    property IconIndex_Restore: Integer read FIconIndex_Restore write FIconIndex_Restore;
    property IconIndex_Cut: Integer read FIconIndex_Cut write FIconIndex_Cut;
    property IconIndex_Paste: Integer read FIconIndex_Paste write FIconIndex_Paste;
    property IconIndex_Delete: Integer read FIconIndex_Delete write FIconIndex_Delete;
    property IconIndex_MarkAsDeleted: Integer read FIconIndex_MarkAsDeleted write FIconIndex_MarkAsDeleted;
    property IconIndex_UnMarkFromDeleted: Integer read FIconIndex_UnMarkFromDeleted write FIconIndex_UnMarkFromDeleted;
    property IconIndex_ShowTree: Integer read FIconIndex_ShowTree write FIconIndex_ShowTree;
    property IconIndex_FindInTree: Integer read FIconIndex_FindInTree write FIconIndex_FindInTree;
    property IconIndex_DatesPeriod: Integer read FIconIndex_DatesPeriod write FIconIndex_DatesPeriod;
    property IconIndex_Refresh: Integer read FIconIndex_Refresh write FIconIndex_Refresh;
    property IconIndex_FilterBox: Integer read FIconIndex_FilterBox write FIconIndex_FilterBox;
    property IconIndex_FilterClear: Integer read FIconIndex_FilterClear write FIconIndex_FilterClear;
    property IconIndex_GroupBox: Integer read FIconIndex_GroupBox write FIconIndex_GroupBox;
    property IconIndex_GroupingClear: Integer read FIconIndex_GroupingClear write FIconIndex_GroupingClear;
    property IconIndex_ExpandAll: Integer read FIconIndex_ExpandAll write FIconIndex_ExpandAll;
    property IconIndex_CollapseAll: Integer read FIconIndex_CollapseAll write FIconIndex_CollapseAll;
    property IconIndex_Details: Integer read FIconIndex_Details write FIconIndex_Details;
    property IconIndex_Properties: Integer read FIconIndex_Properties write FIconIndex_Properties;
    property IconIndex_ObjectInspector: Integer read FIconIndex_ObjectInspector write FIconIndex_ObjectInspector;
    property IconIndex_FitToWidth: Integer read FIconIndex_FitToWidth write FIconIndex_FitToWidth;
    property IconIndex_FitToHeight: Integer read FIconIndex_FitToHeight write FIconIndex_FitToHeight;
    property IconIndex_FitToScreen: Integer read FIconIndex_FitToScreen write FIconIndex_FitToScreen;
    property IconIndex_Wizard: Integer read FIconIndex_Wizard write FIconIndex_Wizard;
    property IconIndex_Printer: Integer read FIconIndex_Printer write FIconIndex_Printer;
    property IconIndex_Excel: Integer read FIconIndex_Excel write FIconIndex_Excel;
    property IconIndex_FastReport: Integer read FIconIndex_FastReport write FIconIndex_FastReport;
    property IconIndex_Submit: Integer read FIconIndex_Submit write FIconIndex_Submit;
    property IconIndex_Select: Integer read FIconIndex_Select write FIconIndex_Select;
    property IconIndex_Take: Integer read FIconIndex_Take write FIconIndex_Take;
    property IconIndex_Backward: Integer read FIconIndex_Backward write FIconIndex_Backward;
    property IconIndex_Forward: Integer read FIconIndex_Forward write FIconIndex_Forward;

    property IconIndex_FileOpen: Integer read FIconIndex_FileOpen write FIconIndex_FileOpen;
    property IconIndex_FileSave: Integer read FIconIndex_FileSave write FIconIndex_FileSave;

    property IconIndex_Run: Integer read FIconIndex_Run write FIconIndex_Run;
    property IconIndex_Pause: Integer read FIconIndex_Pause write FIconIndex_Pause;
    property IconIndex_Step: Integer read FIconIndex_Step write FIconIndex_Step;
    property IconIndex_BreakPoint: Integer read FIconIndex_BreakPoint write FIconIndex_BreakPoint;
    property IconIndex_Watches: Integer read FIconIndex_Watches write FIconIndex_Watches;

    property IconIndex_GutterExecutable: Integer read FIconIndex_GutterExecutable write FIconIndex_GutterExecutable;
    property IconIndex_GutterCurrent: Integer read FIconIndex_GutterCurrent write FIconIndex_GutterCurrent;
    property IconIndex_GutterError: Integer read FIconIndex_GutterError write FIconIndex_GutterError;
    property IconIndex_GutterBreakPoint: Integer read FIconIndex_GutterBreakPoint write FIconIndex_GutterBreakPoint;

    property IconIndex_EditButtonCalc: Integer read FIconIndex_EditButtonCalc write SetIconIndex_EditButtonCalc;
    property IconIndex_EditButtonDate: Integer read FIconIndex_EditButtonDate write SetIconIndex_EditButtonDate;

    property IconIndex_MsgInfo: Integer read FIconIndex_MsgInfo write FIconIndex_MsgInfo;
    property IconIndex_MsgWarning: Integer read FIconIndex_MsgWarning write FIconIndex_MsgWarning;
    property IconIndex_MsgError: Integer read FIconIndex_MsgError write FIconIndex_MsgError;

    property IconIndex_WindowRestore: Integer read FIconIndex_WindowRestore write FIconIndex_WindowRestore;
    property IconIndex_WindowMinimize: Integer read FIconIndex_WindowMinimize write FIconIndex_WindowMinimize;
    property IconIndex_WindowMaximize: Integer read FIconIndex_WindowMaximize write FIconIndex_WindowMaximize;
    property IconIndex_WindowClose: Integer read FIconIndex_WindowClose write FIconIndex_WindowClose;
    property IconIndex_WindowPrev: Integer read FIconIndex_WindowPrev write FIconIndex_WindowPrev;
    property IconIndex_WindowNext: Integer read FIconIndex_WindowNext write FIconIndex_WindowNext;

    property IconIndex_WindowsCascade: Integer read FIconIndex_WindowsCascade write FIconIndex_WindowsCascade;
    property IconIndex_WindowsTileHor: Integer read FIconIndex_WindowsTileHor write FIconIndex_WindowsTileHor;
    property IconIndex_WindowsTileVert: Integer read FIconIndex_WindowsTileVert write FIconIndex_WindowsTileVert;
    property IconIndex_WindowsMinimize: Integer read FIconIndex_WindowsMinimize write FIconIndex_WindowsMinimize;
    property IconIndex_WindowsArrange: Integer read FIconIndex_WindowsArrange write FIconIndex_WindowsArrange;

    property IconIndex_DetailsNone: Integer read FIconIndex_DetailsNone write FIconIndex_DetailsNone;
    property IconIndex_DetailsRight: Integer read FIconIndex_DetailsRight write FIconIndex_DetailsRight;
    property IconIndex_DetailsBottom: Integer read FIconIndex_DetailsBottom write FIconIndex_DetailsBottom;

    property IconIndex_FolderClosed: Integer read FIconIndex_FolderClosed write FIconIndex_FolderClosed;
    property IconIndex_FolderOpened: Integer read FIconIndex_FolderOpened write FIconIndex_FolderOpened;
    property IconIndex_File: Integer read FIconIndex_File write FIconIndex_File;

    property Icon14Index_CheckBox_UnChecked: Integer read FIcon14Index_CheckBox_UnChecked write FIcon14Index_CheckBox_UnChecked;
    property Icon14Index_CheckBox_Checked: Integer read FIcon14Index_CheckBox_Checked write FIcon14Index_CheckBox_Checked;
    property Icon14Index_CheckBox_ParentChecked: Integer read FIcon14Index_CheckBox_ParentChecked write FIcon14Index_CheckBox_ParentChecked;
    property Icon14Index_CheckBox_ChildChecked: Integer read FIcon14Index_CheckBox_ChildChecked write FIcon14Index_CheckBox_ChildChecked;

    property Icon14Index_Root: Integer read FIcon14Index_Root write FIcon14Index_Root;
    property Icon14Index_NodeRoot: Integer read FIcon14Index_NodeRoot write FIcon14Index_NodeRoot;
    property Icon14Index_NodeParent: Integer read FIcon14Index_NodeParent write FIcon14Index_NodeParent;
    property Icon14Index_Find: Integer read FIcon14Index_Find write FIcon14Index_Find;

    property Font_Inspector: String read FFont_Inspector write FFont_Inspector;

    property DefaultStyle_WriteAble: TcxStyle write Set_DefaultStyle_WriteAble;
    property DefaultStyle_ReadOnly: TcxStyle write Set_DefaultStyle_ReadOnly;
    property DefaultStyle_HighlightRecord: TcxStyle write Set_DefaultStyle_HighlightRecord;
    property DefaultStyle_DeletedRecord: TcxStyle write Set_DefaultStyle_DeletedRecord;

    property DefaultStyle_cxVGridHeader: TcxStyle write Set_DefaultStyle_cxVGridHeader;

    property EditRepository_CheckBox: TcxEditRepositoryItem read FEditRepository_CheckBox write FEditRepository_CheckBox;
    property EditRepository_FileExtensions: TcxEditRepositoryImageComboBoxItem read FEditRepository_FileExtensions write FEditRepository_FileExtensions;

//    property NotifyObject_Terminate: Integer read FNotifyObject_Terminate write FNotifyObject_Terminate;
//    property NotifyObject_DataChanged: Integer read FNotifyObject_DataChanged write FNotifyObject_DataChanged;
  end;

{ Рекомендации по версиям процедур на сервере
  ;1 - @Class
  ;2 - @BeforeCreate
  ;3 - @SubClass
  ;4 - @Instance

  ;5 - @Menu (бывшая 4)
  ;6 - @Task (бывшая 5)
  ;7 - @Tree?List
  ;8 - @Tree?Select
  ;9 - Dynamic Form SQL
}

  TControlFrame = packed record
    ModifyState         : TControlModifyState;
    Colors              : TControlColors;
    ControlStateRules   : TControlStateRules;
  end;

  TScanSource = (ssSelf, ssApplication, ssFunctions, ssSender);
  TScanSources = set of TScanSource;

  TSBaseFormValuesEnumerator = class(TNamedVariantsCustomEnumerator)
  strict private
    FCurrentName: String;
    FNames: String;
  public
    constructor Create(ASBaseForm: Pointer; const ANames: String);

    function GetCurrent: TNamedVariant; override;
    function MoveNext: Boolean; override;
  end;

  TSBaseFrm = class(TForm, ISBaseInterface)
    BarManager: TdxMDBarManager;
    mnExcel: TdxBarButton;
    ActionList: TActionList;
    ActionObjectInspector: TAction;
    ActionScriptDebugger: TAction;

    { Реализованные обработчики событий TForm. Не подлежат наследованию! }
    procedure FormActivate(Sender: TObject); virtual;
    procedure FormShow(Sender: TObject); virtual;
    procedure FormClose(Sender: TObject; var Action: TCloseAction); virtual;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean); virtual;

    { Реализованные обработчики сообщений TForm. }
    procedure WMWindowOnShow(var Msg: TMessage); message WM_WindowOnShow;
    procedure WMConfigChanged(var Msg: TMessage); message WM_CONFIGCHANGED;
{$IFDEF DEBUG_FORMS}
    procedure WMObjectInspector(var Msg: TMessage); message WM_OBJECTINSPECTOR;
{$ENDIF}
    procedure WMExcel(var Msg: TMessage); message SBaseForm.WM_EXCEL;

    { Вспомогательные обработчики событий }
    procedure StringFieldChange(Sender: TObject);   // Удаляет пробельные символы в начале и в конце строки, а в середине заменяет их на пробел.
    procedure TextFieldChange(Sender: TObject);     // Удаляет пробельные символы в начале и в конце строки.
    procedure VariantFieldChange(Sender: TObject);  // Удаляет пробельные символы в начале и в конце строки.

    procedure cxDBVerticalGridDrawValue(Sender: TObject; ACanvas: TcxCanvas; APainter: TcxvgPainter; AValueInfo: TcxRowValueInfo; var Done: Boolean);

    { Реализованный обработчик события сбора публичных параметров формы для инициализации параметров ДатаСета }
    procedure DataSetProviderPrepareParams(ADataSetProvider: TCustomSQLDataSetProvider);

    //TcxTreeListValidateDrawValueEvent    =procedure(Sender: TcxTreeListColumn           ; ANode: TcxTreeListNode      ; const AValue: Variant     ; AData: TcxEditValidateInfo) of object;
    //TcxVerticalGridValidateDrawValueEvent=procedure(Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer       ; const AValue: Variant     ; AData: TcxEditValidateInfo) of object;
    //TcxGridValidateDrawValueEvent        =procedure(Sender: TcxCustomGridTableItem      ; ARecord: TcxCustomGridRecord; const AValue: TcxEditValue; AData: TcxEditValidateInfo) of object;
    procedure cxEditPropertiesValidateOnNullDrawValue(ASender: TObject; ARecordIndex: Integer; const AValue: Variant; AData: TcxEditValidateInfo);
    procedure cxEditPropertiesValidateOnNullOrEmptyDrawValue(ASender: TObject; ARecordIndex: Integer; const AValue: Variant; AData: TcxEditValidateInfo);

    procedure cxEditPropertiesValidateOnNull(ASender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure cxEditPropertiesValidateOnNullOrEmpty(ASender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);

    procedure ActionObjectInspectorExecute(Sender: TObject); dynamic;
    procedure ActionScriptDebuggerExecute(Sender: TObject = nil);
{$IFDEF DEBUG}
  strict private
    type
      TInfoRecompileMode = (irmIfNone, irmMetadata, irmAll);
{$ENDIF}
  strict private
    FGetBusy, FGetCaptionParamByName, FInternalGetParameters: TfsProcVariable;
    FVirtualClassInfo: TSBaseVirtualClassInfo;

    procedure DoPrepare;
    type
      TVirtualClassInfoNotFoundAction = (vcinfReturnNil, vcinfReturnEmpty, vcinfRaiseError);
  private
    { Форма прошла процедуру базового создания (inherited Create) }
    FCreated : Boolean;
    { Форма загружается }
    FLoading : Boolean;
    { Форма делает что-то нужное. Закрытию не подлежит }
    FBusy    : Boolean;

    { Это текущая форма прогресса }
    FProgress: TSBaseProgressFrm;

    { Форма делает что-то нужное. Окна не перерисовываются. Сообщение над контролами вывести не удастся }
    FMessageLockCount     : Integer;
    FMessageControl       : TControl;
    FMessageType          : Char;
    FMessageText          : String;
    FMessageX, FMessageY  : Integer;

    { Форма обладает способностью восстанавливаться при открытии программы }
    FSaveAllowed: Boolean;

    { Основной источнык данных формы. По-умолчанию инициализируется из SBaseDataModule.BaseSQLConnectionProvider }
    FSQLConnectionProvider: TSQLConnectionProvider;
    { Альтернативный источнык данных формы. Создаётся при необходимости на основании FSQLConnectionProvider }
    FSQLAlternateConnectionProvider: TSQLConnectionProvider;

    { Перечень параметров формы, подлежащих запоминанию до следующего открытия формы. Параметры уникальны в рамках значения property Identity }
    FStoreFormSettings: TFormItemsForSave;
    { Список пользовательских параметров. Параметры уникальны в рамках значения property Identity }
    FSettings: TNamedVariants;

    { Внутренняя переменная. Скомпиленная DFM. Компилится в InternalCreateByparams, а загружается в InternalCreate. Не спрашивай почему так. }
//    FParsedDFM: TMemoryStream;

    { Некое строковое значение, позволяющее хранить уникальные пользовательские настройки для различных сочетаний значений property IdentityParams }
    FIdentity   : String;
    { Версия формы. Параметры оной формы хранятся также в рамках версии, а не только в рамках property IdentityParams }
    FVersion    : Byte;

    { Перечень кнопок MDIMainForm которые будут активны, когда текущая форма будет активна }
    FMDIToolButtons: TMDIToolButtons;

    { Список внешних параметров формы, пришедших при создании экземпляра }
    FCreateFormParams: TNamedVariants;
    { Список внутренних параметров формы, динамически меняющихся при работе формы }
    FInternalFormParams: TNamedVariants;
    { Список статических параметров формы }
    FInternalFormConstants: TNamedVariants;
    { Дополнительные уникальные параметры формы }
    FCreateFormIdentityParams: String;
    { Формат вывода Caption формы }
    FCaptionFormat: String;

    { Список внутренних фильтров формы, сохраняющих свои значения в InternalFormParams }
    FLinkedFilterItems: TStringList;

    { Список параметров, ожидаемых к возврату из формы в режиме Select. }
    FResultFields: String;

    { Здесь и далее - внутренние методы. Описание не даю. }

    procedure SetIconIndex(AValue: Integer);
    procedure PrivateCreate(AOwner: TComponent);

    procedure AddParamItem(const AItem: String);
    procedure AddConstantItem(const AItem: String);

    procedure SetEncapsulateCreateFormParams(const Value: String);
//    procedure SetKeepCreateFormParams(const Value: String);
//    procedure SetRequireCreateFormParams(const Value: String);

    procedure SetClickItem(AValue: TComponent);
    procedure ClickItems;

    function  SetSaveAllowed: Boolean;
  private
    { Здесь и далее - внутренние методы. Описание не даю. }
    procedure InternalCreateByParams(AOwner: TComponent; ACallBeforeCreateProcedure: Boolean);
    procedure InternalCreateIfNotCreated(AOwner: TComponent);
    procedure SaveFormSettings(AStoreFormSettings: TFormItemsForSave);
    procedure LoadFormSettings(AStoreFormSettings: TFormItemsForSave);
    procedure DoEncapsulateCreateFormParams;
    procedure DoKeepCreateFormParams;
    procedure DoRequireCreateFormParams;
    procedure SetStoreFormSettings(const Value: TFormItemsForSave);
    function  GetVariables: TNamedVariants;
    procedure SetSQLConnectionProvider(const AValue: TSQLConnectionProvider);
  protected
{$IFDEF DEBUG}
    FDebugInfo: TStrings;
{$IFDEF DEBUG_FORMS}
    FScriptDebugMode: Boolean;
{$ENDIF}
{$ENDIF}
    { Внутренняя переменная. Список элементов, по которым должен будет обработаться Click, после того как форма станет видимой. }
    FClickItems: TList<TComponent>;

    { Скомпилированный код FastScrirpt }
    FScript   : TfsScript;
    FScriptDebuggerForm: TForm;

    { Виртуальное имя класса }
    FInternalClassName            : String;
    FTestRunMode                  : Boolean;

    { Привилегии класса - битовая маска - VIEW/INSERT/UPDATE/DELETE }
    FPrivileges                   : Word;

    { Пользовательские настройки загружены. }
    FSettingsLoaded               : Boolean;
    { Список реакций и условий активности контролов. Как правило это реакции OnClick }
    FItemReactions                : TBaseItemReactons;
    { Список контролируемых источников данных для обработки зависимых от них ItemsReactions }
    FDataSourceControls           : TDataSourceControls;
    { Адрес процедуры, вызывающейся в режиме Select при срабатывании собатия OnSelect.Click - если не предоставлено, то значения просто вернутся в вызванный Select метод. }
    FSelectCallBackProc           : TCallBackSelectFunction;
    FSelectCallBackScript         : String;

    { Переводит входные параметры создания формы во внутренние (InternalFormParams) }
    FEncapsulateCreateFormParams  : String;
    { Оставляет только эти входные параметры создания формы - останье стирает как ненужные, и даже вредные нашему делу. }
//    FKeepCreateFormParams         : String;
    { Проверяет на наличие этих входных параметров создания формы - без них жить нельзя! }
//    FRequireCreateFormParams      : String;

    // Процедура инициализации формы. Вызывается при создании формы.
    procedure InternalCreate; virtual;
    procedure SBaseFormLoaded; virtual;

    // Процедура инициализации формы. Вызывается при создании И при перевызове с изменёнными CreateFormParams формы
    procedure InternalPrepareParams; virtual;
    procedure InternalInit; virtual;
    procedure InternalPostInit; virtual;
    procedure InternalShow; virtual;
    // Процедура прекращения работы формы с данными. Вызывается при закрытии или ReInit'е.
    function  InternalCanSelect: Boolean; virtual;
    // Процедура прекращения работы формы с данными. Вызывается при закрытии или ReInit'е.
    procedure InternalClose(AFinalization: Boolean = True); virtual;

    procedure AfterSkinChanged; virtual;
    procedure AfterDataChanged(const ATablesChanges: TTablesChanges); dynamic;

    procedure SetFilterItem(const AComponent: TComponent; const AParamName: String);
    procedure cxFilterEditItemOnEditValueChanged(Sender: TObject);
    procedure FilterValueChanged; dynamic;

    { Процедура обновления Caption формы. Автоматически вызывается только из конструктора и при смене Периода дат }
    procedure UpdateCaption;
    { Функция которая собственно и генерит капшин. По-умолчанию может заменить :DatesPeriod из строки CaptionFormat на что-нибудь более привлекательное }
    function CaptionParamByName(const AParamName: String): String;
    procedure GetCaptionParamByName(const AParamName: String; var ADisplayText: String; var AFound: Boolean); virtual;

    { Если вы хотите что-то сделать при задании MDI-кнопок то перекрывайте этот метод }
    procedure SetMDIToolButtons(AValue: TMDIToolButtons); virtual;

    { -- Признак того что окно занято - можно переопределять для дополнительных проверок }
    function GetBusy: Boolean; virtual;

    { -- Событие чтения пользовательских настроек формы - перекрывается по желанию }
    procedure LoadUserConfig(AStoreFormSettings: TFormItemsForSave); virtual;
    { -- Событие сохранения пользовательских настроек формы - перекрывается по желанию }
    procedure SaveUserConfig(AStoreFormSettings: TFormItemsForSave); virtual;

    function  GetParameters(Level: UInt16): TNamedVariants;
    procedure InternalGetParameters(ALevel: UInt16; var AParameters: TNamedVariants; var ACollisions: Boolean); dynamic;
    // Вызывается при сохранении открытых форм в MDIMainForm, для восстановления при открытии программы
    function  GetFormRestoreParams: TNamedVariants; virtual;

{$IFDEF DEBUG_FORMS}
    procedure DoExceptionDueExecute(AScript: TfsScript; const AErrorPos: String; AException: Exception{; ASourceLines: String; ASourceLineNumber: Integer});
{$ENDIF}
  protected
  { Дополненные защищённые методы }
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Здесь и далее - внутренние методы. Описание не даю. }
    constructor Create(AOwner: TComponent); override; deprecated;

    class function GetVirtualClassInfo
                   (
                    const AClassName: String;
                    ATestRun: Boolean;
                    ANotFoundAction: TVirtualClassInfoNotFoundAction = vcinfReturnNil;
                    ARaiseIfNoAccess: Boolean = False
{$IFDEF DEBUG};
                    ADebugInfo: TStrings = nil;
                    ARecompile: TInfoRecompileMode = irmIfNone
{$ENDIF}
                   ): TSBaseVirtualClassInfo;

    //constructor CreateEmpty(const AParams: TNamedVariants);
    procedure WndProc(var AMessage: TMessage); override;

{$IFDEF DEBUG}
    function DfmText: String;
{$ENDIF}

    class procedure ExecuteSelectCallBackProcedures(Action: TCallBackSelectAction; var AResultValues: TNamedVariants; AOwner: TComponent; ACallBackProc: TCallBackSelectFunction; ACallBackScript: String);

    function GetFormParamValues(const AParamNames: String; var AUnknownParams: String): Variant; virtual;
    function GetVariable(const AParamName: String; ANullIfNotFound: Boolean = False): Variant;
  public
    { Если мы хотим в существующем экземпляре открыть новые данные }
    procedure InternalReInit(const AParams: TNamedVariants); overload;
    procedure InternalReInit(const AParams: array of TNamedVariant); overload;
  public
    { Режим "Select Mode" }
    SelectMode: TSelectMode;

    { Возвращаемые значения режима "Select" }
    ResultValues: TNamedVariants;

    { Вызов в режиме Select класса с неустановленным родителем }
//    class function Select(const AClassName: String; const AOwner: TComponent; AParams: TNamedVariants; const ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants; overload;
//    class function Select(const AClassName: String; const AOwner: TComponent; const AParams: Array of TNamedVariant; const ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants; overload;

    { Вызов в режиме Select }
    class function Select(const AOwner: TComponent; const AParams: TNamedVariants; const ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants; overload;
    class function Select(const AOwner: TComponent; const AParams: Array of TNamedVariant; const ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants; overload;

    { Собственно автонастройка с сервера. Достаточно возвратить Поле "Form" в формате DFM }
    class procedure RegisterClass(const AClassName: String{; const AClassProcedure: String}; const APrivileges: SmallInt = -1); overload;

    { Версия класса }
    class function Version: Byte; virtual;

    { Конструктор создания формы }
//    constructor CreateByParams(AOwner: TComponent; const AParams: Array of TNamedVariant); overload;
    constructor CreateByParams(AOwner: TComponent; const AParams: TNamedVariants; ACallBeforeCreateProcedure: Boolean = True);

    { Псевдоконструктор создания экземпляра класса по его виртуальному имени. Он действительно нужен. }
//    class function FindClassByParams(AClassName: String; var AParams: TNamedVariants): TSBaseFrmClass; overload;

    { Деструктор, как не банально. }
    destructor Destroy; override;

    function SQLAlternateConnectionProvider: TSQLConnectionProvider; inline;

    { Prepare parameters }
    function  PrepareParameters(AParamNames: String; const AParams: TNamedVariants; AGetVarValuesFunction: TGetVarValuesFunction = nil; ASender: TObject = nil): TNamedVariants;
    { Execute SQL }
    function  ExecSQL(const ACommandText: String; const AParams: TNamedVariants; AGetVarValuesFunction: TGetVarValuesFunction = nil; ASender: TObject = nil; AThread: Boolean = False): TNamedVariants;
    function  OpenSQLQuery(const ACommandText: String; AParams: TNamedVariants; AGetVarValuesFunction: TGetVarValuesFunction = nil; ASender: TObject = nil; AThread: Boolean = False): TCustomInternalQuery;
    procedure CloseSQLQuery; inline;

    function  OpenSQLPublicQuery(const ACommandText: String; AParams: TNamedVariants): TUserMemoryDataSet;
    procedure CloseSQLPublicQuery; inline;

    { Вызов функции FastScript по имени. }
    function  PrepareFunction(const AFunction: String; const ARaiseIfNotFound: Boolean = True): TfsProcVariable;

    function  CallFunction(const AFunction: String; const ARaiseIfNotFound: Boolean = True): Variant; overload; inline;
    function  CallFunction(const AFunction: TfsProcVariable): Variant; overload;

    function  CallFunction(const AFunction: String; const AParamNames: String; const AParamValues: Variant; const ARaiseIfNotFound: Boolean = True): Variant; overload; inline;
    function  CallFunction(const AFunction: TfsProcVariable; const AParamNames: String; const AParamValues: Variant): Variant; overload;

    function  CallFunction(const AFunction: String; const AParams: Array of TNamedVariant; const ARaiseIfNotFound: Boolean = True): Variant; overload;
//    function  CallFunction(const AFunction: TfsProcVariable; const AParams: Array of TNamedVariant): Variant; overload;

    function  CallFunction(const AFunction: String; const AParams: TNamedVariants; const ARaiseIfNotFound: Boolean = True): Variant; overload; inline;
    function  CallFunction(const AFunction: TfsProcVariable; const AParams: TNamedVariants): Variant; overload;

    { Вызов функции FastScript по имени, с возвратом параметров. }
    function  CallFunctionOut(AFunction: TfsProcVariable; var AParams: TNamedVariants): Variant; overload;
    function  CallFunctionOut(const AFunction: String; var AParams: TNamedVariants; const ARaiseIfNotFound: Boolean = True): Variant; overload; inline;
//    function  CallFunctionOut(const AFunction: String; var AParamNames: String; var AParamValues: Variant; const ARaiseIfNotFound: Boolean = True): Variant; overload;

    { Виртуальное имя класса. }
    property  InternalClassName: String read FInternalClassName;

    { Итоговый IdentityParams формы - общий + кастомный }
    property  CreateFormIdentityParams: String read FCreateFormIdentityParams;

    { Список внешних параметров формы, пришедших при создании экземпляра }
    property  CreateFormParams: TNamedVariants read FCreateFormParams;
    { Список внутренних параметров формы, динамически меняющихся при работе формы }
    property  InternalFormParams: TNamedVariants read FInternalFormParams;
    { Список статических параметров формы }
    property  InternalFormConstants: TNamedVariants read FInternalFormConstants;
    { Комбинированный список из трёх предыдущих списков с учётом их логического приоритета. }
    property  Variables: TNamedVariants read GetVariables;
    property  Parameters[Level: UInt16]: TNamedVariants read GetParameters;

    { Показать форму в нужном режиме }
    procedure Show; dynamic;
    { Показать форму в MDI режиме }
    procedure ShowMDI;
    { Показать форму в Modal режиме }
    function ShowModal: Integer; override;

    { Подвинуть форму в видимую область }
    procedure ShowMDIChildInVisibleArea;
    { Распахнуть форму до максимального размера, без включения режима Maximized }
    procedure ShowMDIChildMaximized;
    { Расположить форму по центру Parent'а }
    procedure MoveToParentCenter;

    procedure DoAlternateConnectionCancelRequest(Sender: TObject);

    procedure ShowProgress(ACancelRequest: TNotifyEvent = nil);
    procedure HideProgress;
    function  InProgress: Boolean; inline;

    procedure RegisterControlStateRule(AControl: TComponent; AControlModifyState: TControlDefaultModifyState = bfcsDefaultWriteAble; ADataSource: TDataSource = nil; const AFieldName: String = '');
    function  GetControlFrame(AControl: TComponent): TControlFrame;
    procedure UpdateFormState(AControl: TWinControl = nil);
    procedure UpdateControlState(AControl: TComponent);

    // Позволяет поднять контрол на видимый уровень
    class procedure DisplayControl(AControl: TComponent; ASetFocus: Boolean = False);

    procedure LockMessages;
    procedure PerformMessages;

    // Стандартное информационное сообщение
    class procedure InfoMessage(const AText: string; AControl: TObject = nil);
    // Стандартное сообщение о предупреждении с Абортом.
    procedure WarningMessage(const AText: string; AControl: TObject = nil);
    // Стандартное подтверждение с результатом типа Boolean.
    class function ConfirmMessage(const AText: string; AControl: TObject): Boolean; overload;
    class function ConfirmMessage(const AText: string): Boolean; overload;
    // Стандартное подтверждение с результатом типа Boolean или Abort при отмене.
    class function YesNoAbortMessage(const AText: string; AControl: TObject): Boolean; overload;
    class function YesNoAbortMessage(const AText: string): Boolean; overload;
    // Стандартное подтверждение с отменой.
    class function YesNoCancelMessage(const AText: string; AControl: TObject): Integer; overload;
    // Стандартное сообщение об ошибке
    procedure ErrorMessage(const AText: string; AControl: TObject = nil);

    // Стандартное информационное сообщение
    class procedure AdjustSize(AControl: TWinControl); reintroduce;

    // Проверяет - евляется ли указанный Вин-Контрол ИЛИ ЕГО PARENT - активным
    function IsControlActive(AControl: TWinControl): Boolean;

    // Позволяет получать / сохранять настройки формы (доступно только после подгрузки этих данных)
    function  GetSetting(const ASetting: String; AVersion: Byte = 0): Variant;
    function  SetSetting(const ASetting: String; const AValue: Variant; AVersion: Byte = 0): Boolean;

    { -- Логическая процедура получения значения параметра активной компоненты -- }
    function GetObjectsValues(const AParamNames: String; var AUnknownParams: String): Variant;
    function FindObjectsValues(AParamNames: String; var AUnknownParams: String; AScanSources: TScanSources = [ssSelf, ssApplication, ssFunctions, ssSender]; ASender: TObject = nil; ADisplayText: Boolean = False): Variant;
    function FormatCaption(const ACaptionFormat: String): String;
  published
    { Реализация интерфейса ICreateByParams. Пропустить не глядя. }
    function  IVirtualClassOwner(var AVirtualClassName: String; var AParams: TNamedVariants): TPersistentClass;
    function  IGetCreateParams: TNamedVariants;
    procedure IShow;
  published
    { Реализация published интерфейса ICreateByParams. Пропустить не глядя. }
    function  ICreateByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants): TComponent;
    function  ISelectByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants; ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants;
  published
//    property Owner;
    property SQLConnectionProvider: TSQLConnectionProvider read FSQLConnectionProvider write SetSQLConnectionProvider;

    { Загрузить иконку формы из стандартного списка }
    property IconIndex: Integer write SetIconIndex;

    { Имена возвращаемых полей режима "Select" }
    property ResultFields: String read FResultFields write FResultFields;

    { Привилегии экземпляра }
    property ActualPrivileges: Word read FPrivileges write FPrivileges default 0;

    property Created: Boolean read FCreated;
    property Loading: Boolean read FLoading write FLoading default False;
    property ModalResult;

    property CaptionFormat: String read FCaptionFormat write FCaptionFormat;
    property MDIToolButtons: TMDIToolButtons read FMDIToolButtons write SetMDIToolButtons;
    property StoreFormSettings: TFormItemsForSave read FStoreFormSettings write SetStoreFormSettings default [];
    property SaveAllowed: Boolean read SetSaveAllowed write FSaveAllowed default False;
    property Identity: String read FIdentity write FIdentity;

    // Вызывается при сохранении открытых форм в MDIMainForm, для восстановления при открытии программы
    property FormRestoreParams: TNamedVariants read GetFormRestoreParams;

    // Нужно для изменения значения параметра InternalFormParams при чтении с DFM
    property Parameter: String write AddParamItem;

    // Нужно для изменения значения константы InternalFormConstants при чтении с DFM
    property Constant: String write AddConstantItem;

    // property Constants: TNamedVariants read FInternalFormConstants;

    { -- Признак того что окно занято }
    property Busy: Boolean read GetBusy write FBusy default False;

    { Режим "Только чтение" }
    //property ReadOnly: Boolean read FReadOnly write SetReadOnly;

    { Переводит входные параметры создания формы во внутренние (InternalFormParams) }
    property EncapsulateCreateFormParams: String read FEncapsulateCreateFormParams write SetEncapsulateCreateFormParams;

    { Оставляет только эти входные параметры создания формы (остальные стирает) }
//    property KeepCreateFormParams: String read FKeepCreateFormParams write SetKeepCreateFormParams;

    { Проверяет на наличие этих входных параметров создания формы (даёт ошибку если один из параметров NULL) }
//    property RequireCreateFormParams: String read FRequireCreateFormParams write SetRequireCreateFormParams;

    property ItemsReactions: TBaseItemReactons read FItemReactions write FItemReactions;
    property DataSourceControls: TDataSourceControls read FDataSourceControls write FDataSourceControls;

    { Эмулирует нажитие на контрол }
    property ClickItem: TComponent write SetClickItem;
{$IFDEF DEBUG}
    property DebugInfo: TStrings read FDebugInfo;
{$ENDIF}
  end;

  TfsScriptErrorInfo = record
    Line: Integer;
//    Column: Integer;
    Scope: String;
  end;

  TfsScriptHelper = class Helper for TfsScript
    function ErrorInfo(const AErrorPos: String): TfsScriptErrorInfo;
  end;

{$REGION 'FASTSCRIPT_RTTI - Definition'}
{$IFDEF FASTSCRIPT_RTTI}
  TfsFunctions_TSBaseFrm = class(TfsRTTIModule)
  private
    function  Get_TSBaseFrm_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TSBaseFrm_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
    function  Call_TSBaseFrm_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}
{$ENDREGION 'FASTSCRIPT_RTTI - Definition'}

var
  SBaseFormProperties: TSBaseFormProperties;
  SBaseMainFormMDIInterface: TSBaseMainFormMDIInterface = nil;

  SBaseGetUserClassVariablesFunction: TSBaseGetUserClassVariablesFunction = nil;
  SBaseSetUserClassVariableProcedure: TSBaseSetUserClassVariableProcedure = nil;

resourcestring
  SRecordChangedWhileConfirming = 'При запросе подтверждения текущая позиция в гриде была изменена. Действие отменено.';

implementation

uses
  ActiveX
  , RTLConsts
  , SBaseConstants, SBaseVariantFunctions, cxMDGrid, cxMDDBTreeList
{$IFDEF DEBUG_FORMS}
  , SBaseObjectInspectorDialog, SBaseScriptDebuggerForm
{$ENDIF}
  , SBaseFileUtils
{$IFDEF XXVIEW_FORMS}
  , SBaseXXViewForm
{$ENDIF}
{$IFDEF PERIODIC_FORMS}
  , DBPeriodicTabControl
{$ENDIF}
  , SBaseDfmView
{$IFDEF SERVER_UPDATE_NOTIFICATIONS}
  , SBaseNotificationMessage
{$ENDIF}
{$IFDEF DATAHANDLER}
  , DataHandler
{$ENDIF}
  , DataModule, cxMDVGrid;

{$R *.dfm}

type
  TControlCrack = class(TControl);
  TWinControlAccess = class(TWinControl);
  TcxCustomTabControlCrack = class(TcxCustomTabControl);
  TcxExtEditingControlCrack = class(TcxExtEditingControl);
  TcxExtEditingControlViewInfoCrack = class(TcxExtEditingControlViewInfo);
{$IFDEF DATAHANDLER}
  TSubDataHandlerCrack = class(TSubDataHandler);
{$ENDIF}

var
  SBaseVirtualClasses: TObjectDictionary<String, TSBaseVirtualClassInfo>;

procedure HTMLLinkClick(const ALink: String);
var
  LParams: TNamedVariants;
  LIdx: Integer;
  LComponent: TComponent;
begin
  LParams := TNamedVariants.Create(True);
  LParams.AsClrTParams := ALink;

  Assert(LParams.Find(SConst_ClassName, LIdx));
  LComponent := SBaseFormDefinitions.ICreateByParams(TSBaseFrm.ClassName, Application.MainForm, LParams);
  if Assigned(LComponent) then
    (LComponent as ISBaseInterface).IShow;
end;

{$REGION 'TSBaseVirtualClassInfo'}
{ TSBaseVirtualClassInfo}

constructor TSBaseVirtualClassInfo.Create(const AVirtualClassName: String; APrivileges: SmallInt);
begin
  VirtualClassName        := AVirtualClassName;
  Privileges              := APrivileges;
  CreateFormParamsAllowed := '*';

(*
  PersistentClass       := nil;
  ParentClassName       := '';
  SubClassParams        := '';
  InstanceParams        := '';
  Unique                := False;
  Version               := 0;
{$IFDEF DEBUG}
  DfmText               := '';
{$ENDIF}
  DFM                   := nil;
  Script                := '';
  BeforeCreateProcedure := '';
  SubClasses            := nil;
  Instances             := nil;

  SetLength(SubClasses, 0);
  SetLength(Instances, 0);
*)
end;

procedure TSBaseVirtualClassInfo.Release;
var
  I: Integer;
begin
{$IFDEF DEBUG}
  try
{$ENDIF}

  {$IFDEF DEBUG}DEBUG_FreeAndNil{$ELSE}FreeAndNil{$ENDIF}(DFM);
{$IFDEF DEBUG}
  SetLength(DfmText, 0);
{$ENDIF}

  for I := Length(SubClasses) - 1 downto 0 do
    SubClasses[I].Release;
  SetLength(SubClasses, 0);

  for I := Length(Instances) - 1 downto 0 do
    Instances[I].Release;
  SetLength(Instances, 0);

{$IFDEF DEBUG}
  except
    Raise
  end;
{$ENDIF}
end;

destructor TSBaseVirtualClassInfo.Destroy;
begin
  Release;
  inherited;
end;
{$ENDREGION 'TSBaseVirtualClassInfo'}

{$REGION 'TSBaseFrmUserVariables'}
{ TSBaseFrmUserVariables }

function TSBaseFrmUserVariables.ExtractVariable: TSBaseFrmUserVariable;
var
  I: Integer;
begin
  if Count = 0 then
    raise Exception.Create('TSBaseFrmUserVariables: Count = 0');
  Result := Variables[0];
  for I := 0 to Count-2 do
    Variables[I] := Variables[I+1];

  SetLength(Variables, Count-1);
end;

procedure TSBaseFrmUserVariables.Clear;
begin
  SetCount(0);
end;

constructor TSBaseFrmUserVariables.Create(AClear: Boolean);
begin
  if AClear then
    Clear;
end;

function TSBaseFrmUserVariables.ExtractVariable(AName: String; AVersion: Integer): Variant;
var
  I: Integer;
  LCount: Integer;
begin
  LCount := Count;
  I := 0;

  while I < Count do begin
    with Variables[I] do
      if AnsiSameText(AName, Name) and (AVersion = Version) then begin
        Result := Variables[I].Value;
        while I < Count-1 do begin
          Variables[I] := Variables[I+1];
          Inc(I);
        end;
        Count := LCount - 1;
        Exit;
      end;
    Inc(I);
  end;

  Result := Null;
end;

function TSBaseFrmUserVariables.GetCount: Integer;
begin
  Result := Length(Variables)
end;

procedure TSBaseFrmUserVariables.SetCount(const Value: Integer);
begin
  SetLength(Variables, Value);
end;
{$ENDREGION 'TSBaseFrmUserVariables'}

{$REGION 'TSBaseFormProperties'}
{ TSBaseFormProperties }

constructor TSBaseFormProperties.Create(AOwner: TComponent);
begin
  FIconIndex_WindowsMinimize  := -1;
  FIconIndex_Wizard := -1;
  FSkinFiles := TStringList.Create;

  inherited;
end;

procedure TSBaseFormProperties.cxTreeListCustomDrawDataCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
begin
  if (not AViewInfo.Focused) and AViewInfo.Node.Focused
  //and (SBaseFormProperties.dxDefaultStyles.HighlightRecord <> nil)
  then
    ACanvas.Canvas.Brush.Color := SBaseFormProperties.dxDefaultStyles.HighlightRecord.Color;
end;

procedure TSBaseFormProperties.cxGridCustomTableViewCustomDrawCell(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
begin
  if (not AViewInfo.Focused) and (AViewInfo.GridRecord.Focused)
        and (Sender = TcxGrid(Sender.GetParentComponent).FocusedView)
//        and (dxDefaultStyles.HighlightRecord <> nil)
  then
    ACanvas.Canvas.Brush.Color := SBaseFormProperties.dxDefaultStyles.HighlightRecord.Color;
end;

//procedure TSBaseFormProperties.cxGridDBViewStylesGetContentStyle(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
//begin
//  if (ARecord <> nil)
//        and (Sender = TcxGrid(Sender.GetParentComponent).FocusedView)
//        and (ARecord is TcxGridDataRow)
//        and TcxGridDataRow(ARecord).Focused
//        and (dxDefaultStyles.HighlightRecord <> nil)
//  then
//    AStyle := SBaseFormProperties.dxDefaultStyles.HighlightRecord;
//end;

destructor TSBaseFormProperties.Destroy;
begin
  FreeAndNil(FSkinFiles);
  inherited;
end;

procedure TSBaseFormProperties.SetIconIndex_EditButtonCalc(const Value: Integer);
begin
  FIconIndex_EditButtonCalc := Value;
(*
  if (Value >= 0) and (IconList <> nil) then begin
    if ToolCtrlsE.NumberEditGlyphEh = nil then
      ToolCtrlsE.NumberEditGlyphEh := TBitMap.Create;
    IconList.GetBitmap(Value, ToolCtrlsE.NumberEditGlyphEh);
  end;
*)
end;

procedure TSBaseFormProperties.SetIconIndex_EditButtonDate(const Value: Integer);
begin
  FIconIndex_EditButtonDate := Value;
(*
  if (Value >= 0) and (IconList <> nil) then begin
    if ToolCtrlsE.DateTimeEditGlyphE = nil then
      ToolCtrlsE.DateTimeEditGlyphE := TBitMap.Create;
    IconList.GetBitmap(Value, ToolCtrlsE.DateTimeEditGlyphEh);
  end;
*)
end;

procedure TSBaseFormProperties.SetImageList(const Value: TcxNamedImageCollection);
begin
  FImageList := Value;
  if Assigned(FImageList) then begin
    TcxCustomUserCheckBoxProperties.LoadDefaultGlyph(FImageList.FindPicture('CHECKBOXES'));
  end;
end;

procedure TSBaseFormProperties.SetSkinFile(const AValue: String);
begin
  if (Length(AValue) > 0) and (FSkinFiles.IndexOf(AValue) = -1) then
    FSkinFiles.Add(AValue)
end;

procedure TSBaseFormProperties.Set_DefaultStyle_cxVGridHeader(const AValue: TcxStyle);
begin
  cxMDVGrid.cxVGridStyleHeader := AValue
end;

procedure TSBaseFormProperties.Set_DefaultStyle_DeletedRecord(const Value: TcxStyle);
begin
  FdxDefaultStyles.DeletedRecord := Value;
end;

procedure TSBaseFormProperties.Set_DefaultStyle_HighlightRecord(const AValue: TcxStyle);
begin
  if FdxDefaultStyles.HighlightRecord <> AValue then begin
    FdxDefaultStyles.HighlightRecord := AValue;
    if Assigned(AValue) then begin
      cxMDGrid.cxGridViewOnCustomDrawCellEvent := cxGridCustomTableViewCustomDrawCell;
      cxMDDBTreeList.cxTreeListCustomDrawDataCellEvent := cxTreeListCustomDrawDataCell;
    end else begin
      cxMDGrid.cxGridViewOnCustomDrawCellEvent := nil;
      cxMDDBTreeList.cxTreeListCustomDrawDataCellEvent := nil;
    end;
  end;
end;

procedure TSBaseFormProperties.Set_DefaultStyle_ReadOnly(const Value: TcxStyle);
begin
  FdxDefaultStyles.ReadOnly := Value;
end;

procedure TSBaseFormProperties.Set_DefaultStyle_WriteAble(const Value: TcxStyle);
begin
  FdxDefaultStyles.WriteAble := Value;
end;
{$ENDREGION 'TSBaseFormProperties'}

{ TSBaseFormValuesEnumerator }

constructor TSBaseFormValuesEnumerator.Create(ASBaseForm: Pointer; const ANames: String);
begin
  FNames := ANames;
  inherited Create(ASBaseForm);
end;

function TSBaseFormValuesEnumerator.GetCurrent: TNamedVariant;
begin
  Result := TNamedVariant.Create(FCurrentName, TSBaseFrm(FPointer).GetVariable(FCurrentName, True))
end;

function TSBaseFormValuesEnumerator.MoveNext: Boolean;
begin
  if not FNames.IsEmpty then begin
    FCurrentName := ExtractAndRemoveValue(FNames, ';');
    Result := True;
  end else
    Result := False;
end;

{$REGION 'TSBaseFrm'}
{ TSBaseFrm }

class procedure TSBaseFrm.RegisterClass
(
  const AClassName  : String;
  const APrivileges : SmallInt = -1
);
var
//  I, L: Integer;
  LClassInfo: TSBaseVirtualClassInfo;
begin
//  L := Length(SBaseVirtualClasses);
//  I := 0;
//  while (I < L) and (not SameText(AClassName, SBaseVirtualClasses[I].ClassName)) do
//    Inc(I);
//  if I = L then begin
//    SetLength(SBaseVirtualClasses, Succ(L));

  if not SBaseVirtualClasses.TryGetValue(AClassName, LClassInfo) then begin
    LClassInfo := TSBaseVirtualClassInfo.Create(AClassName, APrivileges);
    SBaseVirtualClasses.Add(AClassName, LClassInfo);
  end else
    Raise Exception.Create('Class '#171 + AClassName + #187' Cannot be overrided.')
end;

class function TSBaseFrm.GetVirtualClassInfo(const AClassName: String; ATestRun: Boolean; ANotFoundAction: TVirtualClassInfoNotFoundAction; ARaiseIfNoAccess: Boolean{$IFDEF DEBUG}; ADebugInfo: TStrings; ARecompile: TInfoRecompileMode{$ENDIF}): TSBaseVirtualClassInfo;
var
//  I, L
  LIdx: Integer;
{$IFDEF DEBUG}
  LTimeCounter: TTimeCounter;
{$ENDIF}
  LVarItems : TNamedVariants;
  LValue    : String;
begin
{$IFDEF DEBUG}
  try
{$ENDIF}
(*
  L := Length(SBaseVirtualClasses);
  I := 0;
  while (I < L) and (not SameText(AClassName, SBaseVirtualClasses[I].ClassName)) do
    Inc(I);

  if I = L then
    Result := @SBaseRealClassInfo
  else
*)
  if not SBaseVirtualClasses.TryGetValue(AClassName, Result) then
    if ANotFoundAction = vcinfReturnEmpty then
      Exit(TSBaseVirtualClassInfo.Create(AClassName, Privilege_Preview_Bit))
    else if ANotFoundAction = vcinfRaiseError then
      Raise Exception.Create('Can''t find virtual class with name '#171 + AClassName + #187)
    else
      Exit;

  if ATestRun or Result.TestRun or ((Result.Privileges = -1){$IFDEF DEBUG} or (ARecompile <> irmIfNone){$ENDIF}) then begin
{$IFDEF DEBUG}
    try
      if Assigned(ADebugInfo) then
        LTimeCounter.Start;
{$ENDIF}
      Result.TestRun := ATestRun;

      LVarItems := SBaseFormProperties.InterfaceVariables;
      LVarItems[SConst_ClassName] := AClassName;
      LVarItems[SBFTestRunMode]   := ATestRun;
      LVarItems := SBaseDataModule.BaseSQLConnectionProvider.ExecSQL(SBaseFormProperties.SQLClassInfo, LVarItems, True);

{$IFDEF DEBUG}
      if Assigned(ADebugInfo) then
        ADebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': ' + AClassName + ': Длительность выполнения классовой процедуры: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
    except
      Result.Privileges := -1;
      Raise;
    end;
{$ENDIF}

    if LVarItems.Find(SConst_ParentClassName, LIdx) then begin
      Result.ParentClassName := VarToStr(LVarItems.Items[LIdx].Value);
      LVarItems.Delete(LIdx);
    end;

    if LVarItems.Find(SConst_Version, LIdx) then begin
      Result.Version := LVarItems.Items[LIdx].Value;
      LVarItems.Delete(LIdx);
    end else
      Result.Version := 0;

    if LVarItems.Find(SConst_SubClassParams, LIdx) then begin
      Result.SubClassParams := VarToStr(LVarItems.Items[LIdx].Value);
      LVarItems.Delete(LIdx);
    end else
      Result.SubClassParams := '';

    if LVarItems.Find(SConst_InstanceParams, LIdx) {or LVarItems.Find(SConst_IdentityParams, Idx)} then begin
      Result.InstanceParams := VarToStr(LVarItems.Items[LIdx].Value);
      LVarItems.Delete(LIdx);
    end else
      Result.InstanceParams := '';

    if LVarItems.Find(SConst_Unique, LIdx) then begin
      Result.Unique := (LVarItems.Items[LIdx].Value = True);
      LVarItems.Delete(LIdx);
    end else
      Result.Unique := not Result.InstanceParams.IsEmpty;

    if LVarItems.Find(SConst_ClassName, LIdx) then begin
      LValue := VarToStr(LVarItems.Items[LIdx].Value);
      if not LValue.IsEmpty then begin
        Result.PersistentClass := TSBaseFrmClass(GetClass(LValue));
        Assert(Assigned(Result.PersistentClass), 'Class '#171 + LValue + #187' not found or not registered.');
      end;
      LVarItems.Delete(LIdx);
    end else
      Result.PersistentClass := TSBaseFrmClass(GetClass(AClassName));

    if LVarItems.Find('BeforeCreateProcedure', LIdx) then begin
      Result.BeforeCreateProcedure := VarToStr(LVarItems.Items[LIdx].Value);
      LVarItems.Delete(LIdx);
    end else
      Result.BeforeCreateProcedure := '';

    if LVarItems.Find('CreateFormParams:Allowed', LIdx) then begin
      Result.CreateFormParamsAllowed := VarToStr(LVarItems.Items[LIdx].Value);
      LVarItems.Delete(LIdx);
    end else
      Result.CreateFormParamsAllowed := '*';

    if LVarItems.Find('CreateFormParams:Required', LIdx) then begin
      Result.CreateFormParamsRequired := VarToStr(LVarItems.Items[LIdx].Value);
      LVarItems.Delete(LIdx);
    end else
      Result.CreateFormParamsRequired := '';

{$IFDEF DEBUG}
    Result.Release;
{$ENDIF}

    if LVarItems.Find(SConst_DFM, LIdx) then begin
{$IFDEF DEBUG}
      if ARecompile = irmAll then begin
        LValue := VarToStr(LVarItems.Items[LIdx].Value).Trim(CConst_LineBreaks);
        Result.DfmText := LValue;
        if not LValue.IsEmpty then begin
          LTimeCounter.Start;
          Result.DFM := TDfmParser.Parse(LValue);
          if Assigned(ADebugInfo) then
            ADebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': ' + AClassName + ': Длительность компиляции основной DFM: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
        end;
      end;
{$ELSE}
      Result.DFM := TDfmParser.Parse(VarToStr(LVarItems.Items[Idx].Value.Trim(CConst_LineBreaks)));
{$ENDIF}
      LVarItems.Delete(LIdx);
    end else begin
{$IFDEF DEBUG}
      Result.DfmText := '';
{$ENDIF}
      FreeAndNil(Result.DFM);
    end;

    if LVarItems.Find(SConst_Script, LIdx) then begin
{$IFDEF DEBUG}
      if ARecompile = irmAll then
{$ENDIF}
      Result.Script := VarToStr(LVarItems.Items[LIdx].Value).Trim(CConst_LineBreaks);
      LVarItems.Delete(LIdx);
    end else
      Result.Script := '';

    Result.Privileges := Privilege_Preview_Bit;
    if LVarItems.Find(SConst_Privileges, LIdx) then begin
      with LVarItems.Items[LIdx] do
        if VarIsPresent(Value) then begin
          Result.Privileges := Value;
          if ARaiseIfNoAccess and ((Result.Privileges and Privilege_Preview_Bit) = 0) then
            Raise Exception.CreateFmt(SBaseProperties.MessageFormat_ClassNotAccessible, [AClassName]);
        end;
      LVarItems.Delete(LIdx);
    end;

    if LVarItems.Count > 0 then
      Raise Exception.Create('[@Class процедура] вернула неизвестные параметры: '#171 + LVarItems.NamesString + #187);
  end;

{$IFDEF DEBUG}
  except
    Raise;
  end;
{$ENDIF}
end;

procedure TSBaseFrm.HideProgress;
begin
  FreeAndNil(FProgress);
end;

function TSBaseVirtualClassInfo.GetVirtualSubClassInfo(const AParams: TNamedVariants; ATestRun: Boolean{$IFDEF DEBUG}; ADebugInfo: TStrings; ACompile: Boolean{$ENDIF}): PSBaseVirtualSubClassInfo;
var
  I, L, Idx: Integer;
  LIdentityParams: Variant;
  LVarItems: TNamedVariants;
{$IFDEF DEBUG}
  LTimeCounter: TTimeCounter;
{$ENDIF}
begin
  Result := nil;
  if SubClassParams.IsEmpty or (Privileges <= 0) then
    Exit;

//  Assert(not AClassInfo.SubClassProcedure.IsEmpty);

  L := Length(SubClasses);
  I := 0;

  LIdentityParams := AParams[SubClassParams];
  while (I < L) do begin
    Result := @SubClasses[I];
    if VarIsEqual(LIdentityParams, Result.IdentityParams) then Break;
    Inc(I);
  end;

{$IFDEF DEBUG}
  if I < L then
    Result.Release
  else begin
{$ELSE}
  if I = L then begin
{$ENDIF}
    SetLength(SubClasses, Succ(L));
    Result := @SubClasses[I];
    Result.Init;
{$IFDEF DEBUG}
  end;
{$ENDIF}

    LVarItems.Clear;
    LVarItems.AssignValues(SubClassParams, AParams[SubClassParams]);
{$IFDEF DEBUG}
    LTimeCounter.Start;
{$ENDIF}

    LVarItems[SConst_ClassName] := VirtualClassName;
    LVarItems[SConst_Params]    := LVarItems.AsClrTParams;
    LVarItems[SBFTestRunMode]   := ATestRun;
    LVarItems.AssignValues(SBaseFormProperties.InterfaceVariables, amRaiseIfRepeated);
    LVarItems := SBaseDataModule.BaseSQLConnectionProvider.ExecSQL(SBaseFormProperties.SQLSubClassInfo, LVarItems, True);

{$IFDEF DEBUG}
    if Assigned(ADebugInfo) then
      ADebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': ' + VirtualClassName + ': Длительность выполнения субклассовой процедуры: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
{$ENDIF}
    Result.IdentityParams := LIdentityParams;

    if LVarItems.Find(SConst_DFM, Idx) then begin
{$IFDEF DEBUG}
      Result.DfmText := VarToStr(LVarItems.Items[Idx].Value).Trim(CConst_LineBreaks);
      if ACompile and (not Result.DfmText.IsEmpty) then begin
        LTimeCounter.Start;
        Result.DFM := TDfmParser.Parse(Result.DfmText);
        ADebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': ' + VirtualClassName + ': Длительность компиляции субклассовой DFM: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
      end;
{$ELSE}
      Result.DFM := TDfmParser.Parse(VarToStr(LVarItems.Items[Idx].Value).Trim(CConst_LineBreaks)));
{$ENDIF}
      LVarItems.Delete(Idx);
    end;

    if LVarItems.Find(SConst_Script, Idx) then begin
      Result.Script := VarToStr(LVarItems.Items[Idx].Value).Trim(CConst_LineBreaks);
      LVarItems.Delete(Idx);
    end;

    if LVarItems.Count > 0 then
      Raise Exception.Create('[@SubClass процедура] вернула неизвестные параметры: '#171 + LVarItems.NamesString + #187);

{$IFNDEF DEBUG}
  end;
{$ENDIF}
end;

function TSBaseVirtualClassInfo.GetVirtualInstanceInfo(const AParams: TNamedVariants; ATestRun: Boolean{$IFDEF DEBUG}; ADebugInfo: TStrings; ACompile: Boolean{$ENDIF}): PSBaseVirtualInstanceInfo;
var
  I, L, Idx: Integer;
  LIdentityParams: Variant;
  LValue: Variant;
  LVarItems: TNamedVariants;
{$IFDEF DEBUG}
  LTimeCounter: TTimeCounter;
{$ENDIF}
begin
  Result := nil;
  if (Privileges <= 0) or InstanceParams.IsEmpty then
    Exit;

//  Assert(not AClassInfo.InstanceProcedure.IsEmpty);

  L := Length(Instances);
  I := 0;
  LIdentityParams := AParams[InstanceParams];
  while (I < L) do begin
    Result := @Instances[I];
    if VarIsEqual(LIdentityParams, Result.IdentityParams) then Break;
    Inc(I);
  end;

{$IFDEF DEBUG}
  if I < L then
    Result.Release
  else begin
{$ELSE}
  if I = L then begin
{$ENDIF}
    SetLength(Instances, Succ(L));
    Result := @Instances[I];
    Result.Init;
{$IFDEF DEBUG}
  end;
{$ENDIF}

    LVarItems := TNamedVariants.Create(True);
    LVarItems.AssignValues(InstanceParams, AParams[InstanceParams]);
  {$IFDEF DEBUG}
    LTimeCounter.Start;
  {$ENDIF}

    LVarItems := TNamedVariants.Create([TNamedVariant.Create(SConst_Params, LVarItems.AsClrTParams)], True);
    LVarItems[SConst_ClassName] := VirtualClassName;
    LVarItems[SBFTestRunMode]   := ATestRun;

    LVarItems.AssignValues(SBaseFormProperties.InterfaceVariables, amRaiseIfRepeated);
    LVarItems := SBaseDataModule.BaseSQLConnectionProvider.ExecSQL(SBaseFormProperties.SQLInstanceInfo, LVarItems, True);

  {$IFDEF DEBUG}
    if Assigned(ADebugInfo) then
      ADebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': ' + VirtualClassName + ': Длительность выполнения инстансовой процедуры: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
  {$ENDIF}

    Result.IdentityParams := LIdentityParams;

    if LVarItems.Find(SConst_Identity, Idx) then begin
      Result.Identity := LVarItems.Items[Idx].Value;
      LVarItems.Delete(Idx);
    end;

    Result.Privileges := Privileges;
    if LVarItems.Find(SConst_Privileges, Idx) then begin
      LValue := LVarItems.Items[Idx].Value;
      if VarIsPresent(LValue) then
        Result.Privileges := Result.Privileges and Integer(LValue);
      LVarItems.Delete(Idx);
    end;

    if LVarItems.Find(SConst_DFM, Idx) then begin
  {$IFDEF DEBUG}
      Result.DfmText := VarToStr(LVarItems.Items[Idx].Value).Trim(CConst_LineBreaks);
      if ACompile and (not Result.DfmText.IsEmpty) then begin
        LTimeCounter.Start;
        Result.DFM := TDfmParser.Parse(Result.DfmText);
        ADebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': ' + VirtualClassName + ': Длительность компиляции инстансовой DFM: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
      end;
  {$ELSE}
      Result.DFM := TDfmParser.Parse(VarToStr(LVarItems.Items[Idx].Value).Trim(CConst_LineBreaks)));
  {$ENDIF}
      LVarItems.Delete(Idx);
    end;

    if LVarItems.Find(SConst_Script, Idx) then begin
      Result.Script := VarToStr(LVarItems.Items[Idx].Value).Trim(CConst_LineBreaks);
      LVarItems.Delete(Idx);
    end;

    if LVarItems.Count > 0 then
      Raise Exception.Create('[@Instance процедура] вернула неизвестные параметры: '#171 + LVarItems.NamesString + #187);
{$IFNDEF DEBUG}
  end;
{$ENDIF}
end;

procedure TSBaseFrm.ActionObjectInspectorExecute(Sender: TObject);
{$IFDEF DEBUG_FORMS}
var
  LComponent: TComponent;
{$ENDIF}
begin
{$IFDEF DEBUG_FORMS}
  LComponent := FindComponent(SConst_ObjectInspector);
  if (LComponent <> nil) and (LComponent is TSBaseObjectInspectorDlg) then
    TSBaseObjectInspectorDlg(LComponent).BringToFront
  else with TSBaseObjectInspectorDlg.Create(Self) do begin
    Name := SConst_ObjectInspector;
    if fsModal in FormState then begin
      ShowModal;
      Free
    end else
      Show
  end;
{$ENDIF}
end;

class function TSBaseFrm.Version: Byte;
var
  LClassInfo: TSBaseVirtualClassInfo;
begin
  LClassInfo := GetVirtualClassInfo(ClassName, False, vcinfReturnNil);
  if LClassInfo = nil then
    Result := 0
  else
    Result := LClassInfo.Version
end;

{ Select }

//class function TSBaseFrm.Select(const AClassName: String; const AOwner: TComponent; AParams: TNamedVariants; const ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants;
//var
//  LSBaseFrmClass : TSBaseFrmClass;
//begin
//  LSBaseFrmClass := TSBaseFrm.FindClassByParams(AClassName, AParams);
//  Result := LSBaseFrmClass.Select(AOwner, AParams, ACallBackProcedure)
//end;

//class function TSBaseFrm.Select(const AClassName: String; const AOwner: TComponent; const AParams: Array Of TNamedVariant; const ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants;
//begin
//  Result := Select(AClassName, AOwner, TNamedVariants.Create(AParams, True), ACallBackProcedure);
//end;

class function TSBaseFrm.Select(const AOwner: TComponent; const AParams: TNamedVariants; const ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants;
var
  LParams: TNamedVariants;
  LIdx: Integer;
begin
  LParams := TNamedVariants.Create(AParams, True);
  LParams[SConst_InstanceParams] := '';

  Result := TNamedVariants.Create(False);

  if not LParams.Find(SBFSelectMode, LIdx) then
    LParams[SBFSelectMode] := SBFSM_Single;

  if @ACallBackProcedure <> nil then
    LParams[SBFSelectCallBackProc] := VarArrayOf([Integer(TMethod(ACallBackProcedure).Code), Integer(TMethod(ACallBackProcedure).Data)]);

  try
    with TSBaseFrm(TSBaseFrmClass(Self).CreateByParams(AOwner, LParams)) do
      try
        if InternalCanSelect and (ShowModal = mrOk) and (ResultValues.Count > 0) then begin
          // Вариант под вопросом
          Result := ResultValues;
          ExecuteSelectCallBackProcedures(cbsaSingleRecord, ResultValues, Owner, FSelectCallBackProc, FSelectCallBackScript);
        end;
      finally
        if not (csDestroying in ComponentState) then
          Free
      end;
  except on E:Exception do begin
    if not (E is ESBaseAbortSelect) then
      Raise
  end end;
end;

class function TSBaseFrm.Select(const AOwner: TComponent; const AParams: Array of TNamedVariant; const ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants;
begin
  Result := Select(AOwner, TNamedVariants.Create(AParams, True), ACallBackProcedure);
end;

function TSBaseFrm.InternalCanSelect: Boolean;
begin
  Result := VarToBoolDef(CallFunction('InternalCanSelect', False), True);
end;

procedure TSBaseFrm.InternalClose(AFinalization: Boolean);
var
  LLoading: Boolean;
begin
  LLoading := FLoading;
  FLoading := True;
  try
    CallFunction('InternalClose', False);
  finally
    FLoading := LLoading;
  end;
end;

{$IFDEF DEBUG}
function TSBaseFrm.DfmText: String;
  function GetDfmText(const AClassName: String): String;
  var
    LClassInfo: TSBaseVirtualClassInfo;
    LSubClassInfo: PSBaseVirtualSubClassInfo;
    LInstanceInfo: PSBaseVirtualInstanceInfo;
  begin
    LClassInfo := GetVirtualClassInfo(AClassName, FTestRunMode, vcinfReturnNil);
    if not Assigned(LClassInfo) then Exit('');

    if (not LClassInfo.ParentClassName.IsEmpty) then
      Result := GetDfmText(LClassInfo.ParentClassName)
    else
      Result := '';
    Result := ConcatIfNotEmpty(Result, SConst_CR, LClassInfo.DfmText);

    LSubClassInfo := LClassInfo.GetVirtualSubClassInfo(CreateFormParams, FTestRunMode);
    if Assigned(LSubClassInfo) then
      Result := ConcatIfNotEmpty(Result, SConst_CR, LSubClassInfo.DfmText);

    LInstanceInfo := LClassInfo.GetVirtualInstanceInfo(CreateFormParams, FTestRunMode);
    if Assigned(LInstanceInfo) then
      Result := ConcatIfNotEmpty(Result, SConst_CR, LInstanceInfo.DfmText);
  end;

begin
  Result := GetDfmText(FInternalClassName);
end;
{$ENDIF}

procedure TSBaseFrm.InternalCreate;
var
  LClassName: String;
  LScript: String;
{$IFDEF DEBUG}
  LErrorScope: String;
  LTimeCounter: TTimeCounter;
{$ENDIF}

  LProcessInfoLevel: Byte;

  procedure ProcessInfo(const AClassName: String);
  var
    LClassInfo   : TSBaseVirtualClassInfo;
    LSubClassInfo: PSBaseVirtualSubClassInfo;
    LInstanceInfo: PSBaseVirtualInstanceInfo;
  begin
    Inc(LProcessInfoLevel);
    LClassInfo := GetVirtualClassInfo(AClassName, FTestRunMode, vcinfReturnNil, True{$IFDEF DEBUG}, FDebugInfo, irmAll{$ENDIF});
    if Assigned(LClassInfo) then with LClassInfo do begin
      if LClassName.IsEmpty then LClassName := AClassName;

      if LProcessInfoLevel = 1 then
        FPrivileges := Privileges
      else
        FPrivileges := FPrivileges and Privileges;

      if not ParentClassName.IsEmpty then ProcessInfo(ParentClassName);

      if not Script.IsEmpty then
        LScript := ConcatIfNotEmpty(LScript, SConst_CR + SConst_CR, Script);

{$IFDEF DFM_LOADER}
      if DFM <> nil then begin
  {$IFDEF DEBUG}
        LTimeCounter.Start;
  {$ENDIF}
        LoadDFM(DFM{$IFDEF DEBUG}, @DfmText{$ENDIF});
  {$IFDEF DEBUG}
        FDebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': ' + AClassName + ': Длительность загрузки основной DFM: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
  {$ENDIF}
      end;
{$ENDIF}

      // GetSubClass
      LSubClassInfo := LClassInfo.GetVirtualSubClassInfo(CreateFormParams, FTestRunMode{$IFDEF DEBUG}, FDebugInfo, True{$ENDIF});
      if Assigned(LSubClassInfo) then with LSubClassInfo^ do begin
{$IFDEF DFM_LOADER}
        if Assigned(DFM) then begin
  {$IFDEF DEBUG}
          LTimeCounter.Start;
  {$ENDIF}
          LoadDFM(DFM{$IFDEF DEBUG}, @DfmText{$ENDIF});
  {$IFDEF DEBUG}
          FDebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': ' + AClassName + ': Длительность загрузки субклассовой DFM: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
  {$ENDIF}
        end;
{$ENDIF}

        if not Script.IsEmpty then
          LScript := ConcatIfNotEmpty(LScript, SConst_CR + SConst_CR, Script);
      end;

      // Instance
      LInstanceInfo := LClassInfo.GetVirtualInstanceInfo(CreateFormParams, FTestRunMode{$IFDEF DEBUG}, FDebugInfo, True{$ENDIF});
      if Assigned(LInstanceInfo) then with LInstanceInfo^ do begin
        FPrivileges := Privileges;
        if (FPrivileges and Privilege_Preview_Bit) = 0 then
          Abort;

        FIdentity   := Identity;

{$IFDEF DFM_LOADER}
        if DFM <> nil then begin
  {$IFDEF DEBUG}
          LTimeCounter.Start;
  {$ENDIF}
          LoadDFM(DFM{$IFDEF DEBUG}, @DfmText{$ENDIF});
  {$IFDEF DEBUG}
          FDebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': ' + FInternalClassName + ': Длительность загрузки инстансовой DFM: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
  {$ENDIF}
        end;
{$ENDIF}

        if not Script.IsEmpty then
          LScript := ConcatIfNotEmpty(LScript, SConst_CR + SConst_CR, Script);
      end;
    end;
    Dec(LProcessInfoLevel);
  end;

var
  LLocalizeLoading: Boolean;
begin
//      Perform(WM_SETREDRAW, 0, 0);
//      LockWindowUpdate(Handle);

  LClassName  := '';
  LScript     := '';

  GlobalNameSpace.BeginWrite;
  try
    LLocalizeLoading := (ComponentState * [csInline, csLoading]) = [];
    if LLocalizeLoading then BeginGlobalLoadingEh;// push new loadlist onto stack
    try
      LProcessInfoLevel := 0;
      ProcessInfo(FInternalClassName);
      if LLocalizeLoading then begin
{$IFDEF DEBUG}
        LTimeCounter.Start;
{$ENDIF}
        NotifyGlobalLoadingEh{$IFDEF DEBUG}(FDebugInfo){$ENDIF};// call Loaded
{$IFDEF DEBUG}
        FDebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': NotifyGlobalLoadingEh: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
{$ENDIF}
      end;
    finally
      if LLocalizeLoading then begin
{$IFDEF DEBUG}
        LTimeCounter.Start;
{$ENDIF}
        EndGlobalLoadingEh;// pop loadlist off stack
{$IFDEF DEBUG}
        FDebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': EndGlobalLoadingEh: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
{$ENDIF}
      end;
    end;
  finally
    GlobalNameSpace.EndWrite;
  end;

  if LScript.IsEmpty then
    ActionScriptDebugger.Enabled := False
  else begin
    FScript := TfsScript.Create(nil);
    with FScript do begin
//        NeedDeclareVars           := BooleanFalse;
      AutoDeclareClasses        := True;

      Parent                    := fsGlobalUnit;
      Parent.AutoDeclareClasses := True;

      AddComponent(Self, SConst_Form);

      if Assigned(Application.MainForm) then
        AddComponent(Application.MainForm, 'MainForm');

      if Assigned(SBaseFormProperties) then
        AddVariable('InterfaceVariables', TNamedVariants.TypeName, Integer(@SBaseFormProperties.InterfaceVariables));

{$IFDEF DEBUG}
      LTimeCounter.Start;
{$ENDIF}
      Lines.Text := LScript + SConst_CR + 'BEGIN' + SConst_CR + 'END.';
      if not Compile then
{$IFDEF DEBUG}
        with ErrorInfo(ErrorPos) do begin
          LErrorScope := 'Script compilation error: ' + ErrorMsg + SConst_CR + 'Position: ' + ErrorPos + SConst_CR + Scope;
{$IFDEF DEBUG_FORMS}
          if hsDialogs.HSMessageDlg(LErrorScope, mtError, [mbOk, mbAbort], 0) = mrAbort then begin
            TSBaseScriptDebuggerFrm.CreateByParams
            (
              Self,
              TNamedVariants.Create
              (
                [
                  TNamedVariant.Create(SConst_Line, Line)
                ]
                , True
              )
            ).ShowModal;
            //Abort;
          end;
          Abort;
{$ELSE}  // DEBUG_FORMS
          Raise Exception.Create(LErrorScope);
{$ENDIF} // DEBUG_FORMS
        end;
{$ELSE}  // DEBUG
        Raise Exception.Create(ErrorMsg);
{$ENDIF} // DEBUG

{$IFDEF DEBUG}
      FDebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': ' + FInternalClassName + ': Длительность компиляции скрипта: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
{$ENDIF}

{$IFDEF DEBUG_FORMS}
      FScript.OnExceptionDueExecute := DoExceptionDueExecute;
{$IFDEF DEBUG}
      if FScriptDebugMode then begin
        FScriptDebuggerForm :=
          TSBaseScriptDebuggerFrm.CreateByParams
          (
            Self,
            TNamedVariants.Create
            (
              [
                TNamedVariant.Create(SConst_Paused, True)
              ]
              , True
            )
          );
        TSBaseScriptDebuggerFrm(FScriptDebuggerForm).Show;
      end;
{$ENDIF}
{$ENDIF}

      FGetBusy := PrepareFunction('GetBusy', False);
      FGetCaptionParamByName := PrepareFunction('GetCaptionParamByName', False);
      FInternalGetParameters := PrepareFunction('InternalGetParameters', False);

      CallFunction('InternalCreate', False);
    end;
  end;

  if BarManager.ImageOptions.Images = nil then
    BarManager.ImageOptions.Images := SBaseFormProperties.Icon16List;
end;

procedure TSBaseFrm.DoPrepare;
var
  LLoading: Boolean;
begin
  LLoading := FLoading;
  FLoading := True;
  try
    InternalPrepareParams;
    DoEncapsulateCreateFormParams;
    DoKeepCreateFormParams;
    DoRequireCreateFormParams;
  finally
    FLoading := LLoading;
  end;
  InternalInit;
  InternalPostInit;
  InternalShow;
end;

procedure TSBaseFrm.InternalPrepareParams;
var
  I, LIdx: Integer;
begin
  if FCreateFormParams.Find(SConst_ResultFields, LIdx) then begin
    if FCreateFormParams.Items[LIdx].Value <> '' then
      ResultFields := FCreateFormParams.Items[LIdx].Value;
    FCreateFormParams.Delete(LIdx);
  end;

  if FLinkedFilterItems <> nil then
    for I := 0 to FLinkedFilterItems.Count - 1 do
      if FCreateFormParams.Find(FLinkedFilterItems.Strings[I] , LIdx) then begin
        TcxCustomEdit(FLinkedFilterItems.Objects[I]).EditValue := FCreateFormParams.Items[LIdx].Value;
        FCreateFormParams.Delete(LIdx);
      end;

  CallFunction('InternalPrepareParams', FCreateFormParams, False);
end;

procedure TSBaseFrm.InternalInit;
var
  LIndex: Integer;
begin
  if FCreateFormParams.Find(SConst_CaptionFormat, LIndex) then begin
    FCaptionFormat := FCreateFormParams.Items[LIndex].Value;
    FCreateFormParams.Delete(LIndex);
  end;

  try
    CallFunction('InternalInit', FCreateFormParams, False);
  except
    Loading := True;
    Raise;
  end;
end;

procedure TSBaseFrm.InternalPostInit;
begin
  CallFunction('InternalPostInit', FCreateFormParams, False);
  UpdateCaption;
end;

procedure TSBaseFrm.InternalReInit(const AParams: TNamedVariants);
var
  LVarParams              : TNamedVariants;
  LIdentityParamValues    : Variant;
begin
  DisableSQLCursor(crSQLWait);
  try
    if (not FCreateFormIdentityParams.IsEmpty) then
      LIdentityParamValues := FCreateFormParams[FCreateFormIdentityParams];
    FCreateFormParams.Clear;
    if (not FCreateFormIdentityParams.IsEmpty) then
      FCreateFormParams[FCreateFormIdentityParams] := LIdentityParamValues;
    FCreateFormParams.AssignValues(AParams, amRaiseIfChanged);

    if (not FVirtualClassInfo.BeforeCreateProcedure.IsEmpty) then begin
      LVarParams := SQLConnectionProvider.ExecSQL(FVirtualClassInfo.BeforeCreateProcedure, SBaseFormProperties.InterfaceVariables + FCreateFormParams);
      FCreateFormParams.AssignValues(LVarParams, amOverride);
    end;

    InternalClose(False);
    DoPrepare;
  finally
    EnableSQLCursor;
  end;
end;

procedure TSBaseFrm.InternalReInit(const AParams: array of TNamedVariant);
begin
  InternalReInit(TNamedVariants.Create(AParams, True));
end;

procedure TSBaseFrm.InternalShow;
begin
  CallFunction('InternalShow', FCreateFormParams, False);
end;

{$WARNINGS OFF}
constructor TSBaseFrm.Create(AOwner: TComponent);
{$WARNINGS ON}
begin
  Raise Exception.Create(SAbstractError);
end;

procedure TSBaseFrm.PrivateCreate(AOwner: TComponent);
var
  LLoading_Saved: Boolean;
  LMethod: TMethod;
  LComponent: TComponent;
  LIdx: Integer;
  LSelectMode: String;
{$IFDEF DEBUG}
  LTimeCounter: TTimeCounter;
{$ENDIF}
begin
{$IFDEF DEBUG}
  LTimeCounter.Start;
{$ENDIF}
  FSettingsLoaded := False;

  ResultValues.UnknownAsNull := False;
  ResultValues.Clear;

  if FCreateFormParams.Find(SBFSelectMode, LIdx) then begin
    LSelectMode := VarToStr(FCreateFormParams.Items[LIdx].Value);
    if SameText(LSelectMode, SBFSM_Single) then
      SelectMode := smSingle
    else if SameText(LSelectMode, SBFSM_SingleRepeatAble) then
      SelectMode := smSingleRepeateAble
    else if SameText(LSelectMode, SBFSM_Multiply) then
      SelectMode := smMultiple
    else if SameText(LSelectMode, SBFSM_MultiplyRepeatAble) then
      SelectMode := smMultipleRepeateAble
    else
      Assert(False);

    if FCreateFormParams.Find(SBFSelectCallBackProc, LIdx) then begin
      LMethod.Code := Pointer(Integer(FCreateFormParams.Items[LIdx].Value[0]));
      LMethod.Data := Pointer(Integer(FCreateFormParams.Items[LIdx].Value[1]));

      FCreateFormParams.Delete(LIdx);
      FSelectCallBackProc := TCallBackSelectFunction(LMethod);
    end;

    if FCreateFormParams.Find(SBFSelectCallBackScript, LIdx) then begin
      FSelectCallBackScript := VarToStr(FCreateFormParams.Items[LIdx].Value);
      FCreateFormParams.Delete(LIdx);
    end;
  end else
    SelectMode := smNone;

  LLoading_Saved  := FLoading;
  FLoading        := True;

//  FKeepCreateFormParams   := '*';

  try
    if FCreateFormParams.Find(SConst_SQLConnectionProvider, LIdx) then begin
      SQLConnectionProvider := VarToPointer(FCreateFormParams.Items[LIdx].Value);
      FCreateFormParams.Delete(LIdx);
    end else
      SQLConnectionProvider := SBaseDataModule.BaseSQLConnectionProvider;

    FDataSourceControls := TDataSourceControls.Create(Self);
    FItemReactions   := TBaseItemReactons.Create(Self);

    inherited Create(AOwner);
    FCreated := True;

    if FCreateFormParams.Find(SConst_Parent, LIdx) then begin
      LComponent := VarToPointer(FCreateFormParams.Items[LIdx].Value);
      Assert(LComponent is TWinControl);
      Parent        := LComponent as TWinControl;
      BorderStyle   := bsNone;
      Align         := alClient;
    end;

    InternalCreate;
    Font.Name := SBaseFormProperties.FontName;

    if (not FSettingsLoaded) and (FStoreFormSettings <> []) then try
      LoadUserConfig(FStoreFormSettings);
    except on E:Exception do
      SBaseMainFormMDIInterface.Log(lgError, E.Message);
    end;

    FLoading      := False;
    SBaseFormLoaded;
    DoPrepare;
  except on E:Exception do begin
    InternalClose;
    if (E is ESBaseAbortSelect) or not SBaseMainFormMDIInterface.Log(lgError, E.Message) then
      Raise
    else
      Abort
  end end;

  FLoading        := LLoading_Saved;
{$IFDEF DEBUG}
  FDebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': ' + '-- Общее время создания экземпляра формы: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
{$ENDIF}
end;

procedure TSBaseFrm.cxDBVerticalGridDrawValue(Sender: TObject; ACanvas: TcxCanvas; APainter: TcxvgPainter; AValueInfo: TcxRowValueInfo; var Done: Boolean);
var
  LField: TField;
begin
  if AValueInfo.Row is TcxDBEditorRow then
    LField := TcxDBEditorRow(AValueInfo.Row).Properties.DataBinding.Field
  else if AValueInfo.Row is TcxDBMultiEditorRow then
    LField := TcxDBMultiEditorRow(AValueInfo.Row).Properties.Editors[AValueInfo.RowCellIndex].DataBinding.Field
  else
    Exit;

  if not Assigned(LField) or (not LField.CanModify) then
    SBaseFormProperties.dxDefaultStyles.ReadOnly.AssignStyleTo(ACanvas);
end;

procedure TSBaseFrm.cxEditPropertiesValidateOnNull(ASender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  Error := not VarIsPresent(DisplayValue);
  if Error then
    ErrorText :=  SBaseProperties.Message_ThisFieldMustHaveAValue;
end;

procedure TSBaseFrm.cxEditPropertiesValidateOnNullOrEmpty(ASender: TObject; var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
begin
  Error := (not VarIsPresent(DisplayValue)) or (VarIsStr(DisplayValue) and VarToStr(DisplayValue).IsEmpty);
  if Error then
    ErrorText :=  SBaseProperties.Message_ThisFieldMustHaveAValue;
end;

procedure TSBaseFrm.cxEditPropertiesValidateOnNullDrawValue(ASender: TObject; ARecordIndex: Integer; const AValue: Variant; AData: TcxEditValidateInfo);
begin
  if (ARecordIndex >= 0) and not VarIsPresent(AValue) then begin
    AData.ErrorType := eetError;
    AData.ErrorText := SBaseProperties.Message_ThisFieldMustHaveAValue;
  end;
end;

procedure TSBaseFrm.cxEditPropertiesValidateOnNullOrEmptyDrawValue(ASender: TObject; ARecordIndex: Integer; const AValue: Variant; AData: TcxEditValidateInfo);
begin
  if (ARecordIndex >= 0) and (not VarIsPresent(AValue)) or (VarIsStr(AValue) and VarToStr(AValue).IsEmpty) then begin
    AData.ErrorType := eetError;
    AData.ErrorText := SBaseProperties.Message_ThisFieldMustHaveAValue;
  end;
end;

procedure TSBaseFrm.cxFilterEditItemOnEditValueChanged(Sender: TObject);
var
  LIdx: Integer;
begin
  if not Assigned(FLinkedFilterItems) then Exit;
  LIdx := FLinkedFilterItems.IndexOfObject(Sender);
  Assert(LIdx >= 0);
  FInternalFormParams[FLinkedFilterItems.Strings[LIdx]] := TcxCustomEdit(Sender).EditValue;
  FilterValueChanged;
end;

procedure TSBaseFrm.InternalCreateIfNotCreated(AOwner: TComponent);
var
  I, J: Integer;
  LComponent: TComponent;
  LBaseFrm  : TSBaseFrm absolute LComponent;
begin
  for I := Pred(AOwner.ComponentCount) downto 0 do begin
    LComponent := TComponent(AOwner.Components[I]);
    if (LComponent is TSBaseFrm) then
      if (LBaseFrm.ClassName = ClassName) and SameText(LBaseFrm.InternalClassName, InternalClassName)
              and (LBaseFrm.Owner = AOwner)
  //          and (Identity = AIdentity)
              and (LBaseFrm.CreateFormIdentityParams = FCreateFormIdentityParams)
              and
              (
                (FCreateFormIdentityParams.IsEmpty)
                or
                (LBaseFrm.CreateFormParams.EqualVariables(CreateFormParams, FCreateFormIdentityParams))
              )
        then begin
          LBaseFrm.WindowState := wsNormal;
          LBaseFrm.Show;

          if LBaseFrm.CreateFormParams <> CreateFormParams then begin
            LBaseFrm.CreateFormParams.Clear;

            for J := Pred(CreateFormParams.Count) downto 0 do with CreateFormParams.Items[J] do
              LBaseFrm.CreateFormParams[Name] := Value;

            LBaseFrm.DoPrepare;
          end;

          Abort;
        end;
  end;

  PrivateCreate(AOwner);
end;

procedure TSBaseFrm.InternalCreateByParams(AOwner: TComponent; ACallBeforeCreateProcedure: Boolean);
var
  LIdentityParams: Variant;
  LBeforeCreateProcedure{, LDfm}: String;
//  LVarParams: TNamedVariants;
  Idx: Integer;
  LUnique: Boolean;
{$IFDEF DEBUG}
  LTimeCounter: TTimeCounter;
{$ENDIF}
//  LClassInfo: TSBaseVirtualClassInfo;
begin
  DisableSQLCursor(crSQLWait);
  try
{$IFDEF DEBUG}
    FDebugInfo := TStringList.Create;
{$ENDIF}

  //  if FCreateFormParams = nil then
  //    FCreateFormIdentityParams := UpperCase(IdentityParams)
  //  else

    if FCreateFormParams.Find(SConst_ClassName, Idx) then begin
      FInternalClassName := FCreateFormParams.Items[Idx].Value;
      FCreateFormParams.Delete(Idx);
    end else
      FInternalClassName := ClassName;

    if FCreateFormParams.Find(SBFTestRunMode, Idx) then begin
      FTestRunMode := VarToBoolDef(FCreateFormParams.Items[Idx].Value, False);
      FCreateFormParams.Delete(Idx);
    end;

{$IFDEF DEBUG}
{$IFDEF DEBUG_FORMS}
    if FCreateFormParams.Find(SBFScriptDebugMode, Idx) then begin
      FScriptDebugMode := VarToBoolDef(FCreateFormParams.Items[Idx].Value, False);
      FCreateFormParams.Delete(Idx);
    end;
{$ENDIF}
{$ENDIF}

    FInternalFormParams := TNamedVariants.Create(True);
    FInternalFormConstants := TNamedVariants.Create(True);

    FVirtualClassInfo := GetVirtualClassInfo(FInternalClassName, FTestRunMode, vcinfReturnEmpty, False{$IFDEF DEBUG}, nil, irmAll{$ENDIF});

    with FVirtualClassInfo do begin
      FCreateFormIdentityParams := ArraysMerge
                                   (
                                      [
                                        UpperCase(InstanceParams),
                                        UpperCase(VarToStr(CreateFormParams[SConst_InstanceParams]))
                                      ],
                                      ';'
                                   ).Trim([';']);

      LBeforeCreateProcedure := BeforeCreateProcedure;
      LUnique                := Unique;
      if Assigned(PersistentClass) and (PersistentClass <> Self.ClassType) then
        Raise Exception.Create('Form class '#171 + Self.ClassName + #187' varies with the class '#171 + PersistentClass.ClassName + #187' specified in the registration');
    end;

    if ACallBeforeCreateProcedure and (not LBeforeCreateProcedure.IsEmpty) then begin
  {$IFDEF DEBUG}
      LTimeCounter.Start;
  {$ENDIF}
      FCreateFormParams.AssignValues(SBaseDataModule.BaseSQLConnectionProvider.ExecSQL(LBeforeCreateProcedure, SBaseFormProperties.InterfaceVariables + CreateFormParams), amOverride);
  {$IFDEF DEBUG}
      FDebugInfo.Add(FormatDateTime(SDebugTime_Format, Now()) + ': ' + FInternalClassName + ': Длительность выполнения предварительной процедуры: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
  {$ENDIF}
    end;

    LIdentityParams := FCreateFormParams[SConst_InstanceParams];

    if (not LUnique)
      or (AOwner = nil)
      or not (AOwner is TForm)
      or (fsModal in TForm(AOwner).FormState)                    // Владелец в режиме Modal (форма тоже будет модальной)
      or (VarIsStr(LIdentityParams) and (LIdentityParams = '') ) // Форма ВНЕ уникальности
    then
      PrivateCreate(AOwner)
    else
      InternalCreateIfNotCreated(AOwner);
  finally
    EnableSQLCursor;
  end;
end;

constructor TSBaseFrm.CreateByParams(AOwner: TComponent; const AParams: TNamedVariants; ACallBeforeCreateProcedure: Boolean);
begin
  FCreateFormParams := AParams;
  FCreateFormParams.UnknownAsNull := True;

  InternalCreateByParams(AOwner, ACallBeforeCreateProcedure);
end;

function TSBaseFrm.ICreateByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants): TComponent;
begin
  Result := TSBaseFrmClass(AClass).CreateByParams(AOwner, AParams, False);
end;

procedure TSBaseFrm.IShow;
var
  Lidx: Integer;
  LShowModal: Boolean;
begin
  if CreateFormParams.Find(SConst_ShowModal, Lidx) then begin
    LShowModal := CreateFormParams.Items[Lidx].Value = True;
    CreateFormParams.Delete(Lidx);
    if LShowModal then
      ShowModal
    else
      Show
  end else
    Show
end;

function TSBaseFrm.IVirtualClassOwner(var AVirtualClassName: String; var AParams: TNamedVariants): TPersistentClass;
var
  LSBaseClassInfo : TSBaseVirtualClassInfo;
  LVarParams      : TNamedVariants;
  Idx             : Integer;
  LTrue           : Boolean;
  LClassName      : String;
begin
  Result := nil;
  // Получаем описание запрашиваемого класса
  LSBaseClassInfo := TSBaseFrm.GetVirtualClassInfo(AVirtualClassName, False, vcinfReturnNil, True{$IFDEF DEBUG}, nil, irmMetadata{$ENDIF});
  // Если его нет, то, наверное, этоне к нам
  if LSBaseClassInfo = nil then Exit;
{}

  LTrue  := True;
  while LTrue do begin
    // Если класс "настоящий", то примем его за "нужный".
    if Assigned(LSBaseClassInfo.PersistentClass) then
      Result := LSBaseClassInfo.PersistentClass;

    // Если нет BeforeCreateProcedure то выходим
    if LSBaseClassInfo.BeforeCreateProcedure.IsEmpty then
      Break;

    LVarParams := SBaseDataModule.BaseSQLConnectionProvider.ExecSQL(LSBaseClassInfo.BeforeCreateProcedure, SBaseFormProperties.InterfaceVariables + AParams);
    LVarParams.ClearUndefined;

    // Если есть ClassName, то сработал "редирект"
    if LVarParams.Find(SConst_ClassName, Idx) then begin
      LClassName := LVarParams.Items[Idx].Value;
      LVarParams.Delete(Idx);
      LTrue := not SameText(AVirtualClassName, LClassName);
      if LTrue then begin
        AVirtualClassName := LClassName;
        LSBaseClassInfo := TSBaseFrm.GetVirtualClassInfo(AVirtualClassName, False, vcinfReturnEmpty, True{$IFDEF DEBUG}, nil, irmMetadata{$ENDIF});
      end;
    end
    // Иначе остановим редиректилку
    else
      LTrue := False;

    AParams.AssignValues(LVarParams, amOverride);
  end;

{}
  while (Result = nil) do begin
    if LSBaseClassInfo.ParentClassName.IsEmpty then
      Raise Exception.Create('Can''t find PersistentClass for virtual class with name '#171 + LSBaseClassInfo.VirtualClassName + #187);
    LSBaseClassInfo := TSBaseFrm.GetVirtualClassInfo(LSBaseClassInfo.ParentClassName, False, vcinfRaiseError, True);
    Result := LSBaseClassInfo.PersistentClass;
  end;
end;

function TSBaseFrm.ISelectByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants; ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants;
begin
  Result := TSBaseFrmClass(AClass).Select(AOwner, AParams, ACallBackProcedure);
end;

function TSBaseFrm.IGetCreateParams: TNamedVariants;
begin
  Result := FCreateFormParams
end;

destructor TSBaseFrm.Destroy;
var
  LLoading: Boolean;
begin
  LLoading := FLoading;
  FLoading := True;
  try
    if Assigned(FScriptDebuggerForm) then
      FScriptDebuggerForm.Close;

    CallFunction('Destroy', False);

    inherited;

    FreeAndNil(FItemReactions);
    FreeAndNil(FDataSourceControls);
    FreeAndNil(FLinkedFilterItems);
    FreeAndNil(FClickItems);
{$IFDEF DEBUG}
    FreeAndNil(FDebugInfo);
{$ENDIF}
    FreeAndNil(FScript);
    FVirtualClassInfo := nil; // Delphi XE4 AutoFreeAndNil
  finally
    FLoading := LLoading;
  end;
end;

procedure TSBaseFrm.RegisterControlStateRule(AControl: TComponent; AControlModifyState: TControlDefaultModifyState = bfcsDefaultWriteAble; ADataSource: TDataSource = nil; const AFieldName: String = '');
begin
  with TControlStateRules.CreateRules(AControl) do begin
    ModifyState         := AControlModifyState;
    DataSource          := ADataSource;

    if ADataSource <> nil then
      FieldName         := AFieldName
    else
      FieldName         := '';
  end;
end;

function TSBaseFrm.GetControlFrame(AControl: TComponent): TControlFrame;
begin
  Result.Colors := SBaseFormProperties.dxDefaultStyles;
  Result.ControlStateRules := TControlStateRules.GetRules(AControl);
  if Result.ControlStateRules <> nil then with Result.ControlStateRules do begin
{
    if (DataSet <> nil) and (FieldName <> '') then
      FField   := DataSet.FindField(FieldName)
    else
      FField   := nil;
}
    if (ModifyState = bfcsWriteAble) then
      Result.ModifyState := mdstWriteAble
    else if {FReadOnly or} (ModifyState = bfcsReadOnly) then
      Result.ModifyState := mdstReadOnly
    else if (DataSource = nil) then
      if (ModifyState = bfcsWriteAbleIfEditing) then
        Result.ModifyState := mdstReadOnly
      else
        Result.ModifyState := mdstDefaultWriteAble
    else if DataSource.GetCanModify
              and ((FieldName.IsEmpty) or GetFieldsCanModify(DataSet, FieldName))
              and ((ModifyState <> bfcsWriteAbleIfEditing) or DataSet.Editing)
    then
      Result.ModifyState := mdstWriteAble
    else
      Result.ModifyState := mdstReadOnly;

    Exit;
  end;

  Result.ModifyState  := mdstNotSpecified;
end;

procedure TSBaseFrm.WMConfigChanged(var Msg: TMessage);
begin
  if Msg.WParam = 0 then
    SaveUserConfig(StoreFormSettings)
  else
    LoadUserConfig(StoreFormSettings);
end;

procedure TSBaseFrm.WMExcel(var Msg: TMessage);
begin
  mnExcel.Click
end;

{$IFDEF DEBUG_FORMS}
procedure TSBaseFrm.WMObjectInspector(var Msg: TMessage);
begin
  ActionObjectInspector.Execute
end;
{$ENDIF}

procedure TSBaseFrm.UpdateControlState(AControl: TComponent);
var
  LGUIInterface: ISBaseReadOnlyInterface;
  LInterface: TCustomControlInterface;
  LControlFrame: TControlFrame;
//  I: Integer;
begin
  if Supports(AControl, ISBaseReadOnlyInterface, LGUIInterface) then
    LGUIInterface.IUpdateState(SBaseFormProperties.dxDefaultStyles) //TODO: Сделать через фрейм
  else begin
    LInterface := TCustomControlInterface.CreateInterface(AControl);
    if LInterface = nil then Exit;

    try
      LControlFrame := GetControlFrame(AControl);
      with LControlFrame do begin
        if LInterface.FieldDepended then begin
          if ModifyState in [mdstNotSpecified, mdstDefaultWriteAble] then
            if //(not FReadOnly) and
                ((ModifyState = mdstDefaultWriteAble) or (not LInterface.ReadOnly))
                and LInterface.DataSourceCanModify
                and LInterface.FieldCanModify
            then
              ModifyState := mdstWriteAble
            else
              ModifyState := mdstReadOnly;
{ Visible ?
          if LInterface.Field <> nil then begin
            if (not LInterface.Field.Visible) then begin
              LInterface.Visible := LInterface.Field.Visible;

              // ищем и скрываем связанные по свойству FocusControl контролы
              for I := 0 to ComponentCount - 1 do
                if (Components[I] is TLabel) and ((Components[I] as TLabel).FocusControl = LInterface.FControl) then
                  (Components[I] as TControl).Visible := LInterface.Field.Visible;
            end;
          end;
}
        end else if LInterface.DataSetDepended then begin
          if ModifyState in [mdstNotSpecified, mdstDefaultWriteAble] then
            if {(not FReadOnly) and} (not LInterface.ReadOnly) and LInterface.DataSourceCanModify then
              ModifyState := mdstWriteAble
            else
              ModifyState := mdstReadOnly;
        end;


        if ModifyState = mdstReadOnly then
          LInterface.DrawState(True, Colors.ReadOnly)
        else if ModifyState in [mdstDefaultWriteAble, mdstWriteAble] then
          LInterface.DrawState(False, Colors.WriteAble);

        if (ControlStateRules <> nil) and (AControl is TWinControl) and ControlStateRules.DisableTabStopWhenReadOnly then
          TWinControl(AControl).TabStop := (ModifyState = mdstReadOnly);
      end;
    finally
      LInterface.Free
    end;
  end;
end;

procedure TSBaseFrm.UpdateFormState(AControl: TWinControl = nil);
var
  I : Smallint;
begin
  if AControl = nil then begin
    for I := Pred(ComponentCount) downto 0 do
      if (Components[I] is TWinControl) or (Components[I] is TcxCustomGridView) then
        UpdateControlState(Components[i]);
  end else begin
    for I := Pred(AControl.ControlCount) downto 0 do
      if (AControl.Controls[I] is TWinControl) then begin
        UpdateFormState(TWinControl(AControl.Controls[I]));
        UpdateControlState(AControl.Controls[I]);
      end;
  end
end;

class procedure TSBaseFrm.DisplayControl(AControl: TComponent; ASetFocus: Boolean);
var
  TheControl: TWinControl absolute AControl;
  LSavedControl: TWinControl;
begin
  if (AControl <> nil) then
    if AControl is TdxBarWindowItem then
      TdxBarWindowItem(AControl).SetFocus(True)
    else if AControl is TWinControl then begin
      LSavedControl := TheControl;
      while Assigned(TheControl.Parent) and (TheControl.Parent is TWinControl) do begin
        TWinControlAccess(TheControl.Parent).ShowControl(TheControl);
        AControl := TheControl.Parent;
      end;
      if (AControl is TForm) and TForm(AControl).Visible then
        TForm(AControl).BringToFront;

      if ASetFocus and LSavedControl.Visible and LSavedControl.CanFocus then
        LSavedControl.SetFocus;
    end;
end;

class procedure TSBaseFrm.InfoMessage(const AText: string; AControl: TObject);
begin
  with TControlRect.FindFocus(AControl) do begin
    TSBaseFrm.DisplayControl(Control, True);
    hsDialogs.HSShowInfo(AText, Control, Target.X, Target.Y);
  end;
end;

function TSBaseFrm.InProgress: Boolean;
begin
  Result := Assigned(FProgress)
end;

procedure TSBaseFrm.WarningMessage(const AText: string; AControl: TObject);
begin
  with TControlRect.FindFocus(AControl) do begin
    TSBaseFrm.DisplayControl(Control, True);
    if (Self = nil) or (Control = nil) or (FMessageLockCount = 0) then
      hsDialogs.HSShowWarning(AText, Control, Target.X, Target.Y)
    else begin
      FMessageType    := 'W';
      FMessageControl := Control;
      FMessageText    := AText;
      FMessageX       := Target.X;
      FMessageY       := Target.Y;
      Abort;
    end;
  end;
end;

class function TSBaseFrm.ConfirmMessage(const AText: string; AControl: TObject): Boolean;
begin
  with TControlRect.FindFocus(AControl) do begin
    TSBaseFrm.DisplayControl(Control, True);
    Result := hsDialogs.HSConfirmMessage(AText, Control, Target.X, Target.Y);
  end;
end;

class function TSBaseFrm.ConfirmMessage(const AText: string): Boolean;
begin
  Result := ConfirmMessage(AText, nil);
end;

class function TSBaseFrm.YesNoAbortMessage(const AText: string; AControl: TObject): Boolean;
var
  LResult: Integer;
begin
  with TControlRect.FindFocus(AControl) do begin
    TSBaseFrm.DisplayControl(Control, True);
    LResult := hsDialogs.HSMessageDlg(AText, mtConfirmation, [mbYes, mbNo, mbAbort], 0, Control, Target.X, Target.Y);
  end;

  if LResult = mrAbort then
    Abort;

  Result := LResult = mrYes
end;

class function TSBaseFrm.YesNoAbortMessage(const AText: string): Boolean;
begin
  Result := YesNoAbortMessage(AText, nil);
end;

class function TSBaseFrm.YesNoCancelMessage(const AText: string; AControl: TObject): Integer;
begin
  with TControlRect.FindFocus(AControl) do begin
    TSBaseFrm.DisplayControl(Control, True);
    Result := hsDialogs.HSAskYesNoCancel(AText, Control, Target.X, Target.Y);
  end;
end;

procedure TSBaseFrm.ErrorMessage(const AText: string; AControl: TObject);
begin
  with TControlRect.FindFocus(AControl) do begin
    TSBaseFrm.DisplayControl(Control, True);
    if (Self = nil) or (Control = nil) or (FMessageLockCount = 0) then
      hsDialogs.HSShowError(AText, Control, Target.X, Target.Y)
    else begin
      FMessageType    := 'E';
      FMessageControl := Control;
      FMessageText    := AText;
      FMessageX       := Target.X;
      FMessageY       := Target.Y;
      Abort;
    end;
  end;
end;

class procedure TSBaseFrm.ExecuteSelectCallBackProcedures(Action: TCallBackSelectAction; var AResultValues: TNamedVariants; AOwner: TComponent; ACallBackProc: TCallBackSelectFunction; ACallBackScript: String);
begin
  if (@ACallBackProc <> nil) or (not ACallBackScript.IsEmpty) then begin
    if (@ACallBackProc <> nil) then
      ACallBackProc(Action, AResultValues);

    if (not ACallBackScript.IsEmpty) then begin
      Assert(AOwner is TSBaseFrm);
      AResultValues[SConst_Action] := Action;
      (AOwner as TSBaseFrm).CallFunction(ACallBackScript, AResultValues, True);
    end;
    AResultValues.Clear;
  end;
end;

function TSBaseFrm.IsControlActive(AControl: TWinControl): Boolean;
var
  LControl: TWinControl;
begin
  Result := False;
  LControl := Self.ActiveControl;

  while LControl <> nil do begin
    Result := (LControl = AControl);
    if Result then Break;

    LControl := LControl.Parent;
  end;
end;

function TSBaseFrm.GetFormRestoreParams: TNamedVariants;
begin
  //
end;

function TSBaseFrm.GetBusy: Boolean;
begin
  Result := FBusy or (CallFunction(FGetBusy) = True);
end;

procedure TSBaseFrm.UpdateCaption;
var
  FCaption: String;
begin
  if (not FCaptionFormat.IsEmpty) then begin
    FCaption := FormatCaption(FCaptionFormat);
    if Caption <> FCaption then begin
      Caption := FCaption;
      SBaseMainFormMDIInterface.WindowUpdated(Self);
    end;
  end;
end;

function TSBaseFrm.CaptionParamByName(const AParamName: String): String;
var
  LFound: Boolean;
begin
  LFound := False;
  GetCaptionParamByName(AParamName, Result, LFound);
  if not LFound then
    Result := ':' + AParamName;
end;

procedure TSBaseFrm.GetCaptionParamByName(const AParamName: String; var ADisplayText: String; var AFound: Boolean);
{
  Версия для FastScript'а:
  function GetCaptionParamByName(const AParamName: String): Variant;
}
var
  LValue: Variant;
  LUnknownParamName: String;
begin
  LValue := CallFunction(FGetCaptionParamByName, SConst_ParamName, AParamName);

  if VarIsPresent(LValue) then begin
    AFound := True;
    ADisplayText := VarToStr(LValue);
  end else begin
    LValue := Variables[AParamName];
    if VarIsPresent(LValue) then begin
      AFound := True;
      ADisplayText := VarToStr(LValue);
    end else begin
      LValue := FindObjectsValues(AParamName, LUnknownParamName, [ssSelf, ssApplication, ssFunctions, ssSender], nil, True);
      if LUnknownParamName.IsEmpty then begin
        AFound := True;
        ADisplayText := VarToStr(LValue);
      end;
    end;
  end;
end;

procedure TSBaseFrm.SetMDIToolButtons(AValue: TMDIToolButtons);
begin
  FMDIToolButtons := AValue;
  if Visible and
  (
    ( (FormStyle = fsMDIChild) and (Self = Application.MainForm.ActiveMDIChild) )
    or
    ( (FormStyle = fsNormal) and (not (fsModal in FormState)) and Focused )
  )
  then
    SBaseMainFormMDIInterface.WindowOnActivate(Self, AValue);
end;

procedure TSBaseFrm.FormShow(Sender: TObject);
begin
  if FormStyle = fsMDIChild then
    SBaseMainFormMDIInterface.RegisterWindow(Self)
  else if fsModal in FormState then
    MoveToParentCenter;

  { Для отрабатывания после отображения формы }
  PostMessage(Handle, WM_WindowOnShow, 0, 0);

  CallFunction('FormShow', False);
end;

procedure TSBaseFrm.WMWindowOnShow(var Msg: TMessage);
begin
  AfterSkinChanged;
  ClickItems;
end;

procedure TSBaseFrm.FilterValueChanged;
begin
//
end;

procedure TSBaseFrm.FormActivate(Sender: TObject);
begin
  if FormStyle = fsMDIChild then
    SBaseMainFormMDIInterface.WindowOnActivate(Self, MDIToolButtons);
end;

function TSBaseFrm.FormatCaption(const ACaptionFormat: String): String;
var
  LParamItem: TStringParamItem;
begin
  Result := '';
  for LParamItem in EnumStringParamItems(ACaptionFormat, [epmFormat]) do begin
    Result := Result + LParamItem.Gap;
    if not LParamItem.Value.IsEmpty then
      Result := Result + IsEmptyString(CaptionParamByName(LParamItem.Value), LParamItem.Format);
  end;
end;

//-- Процедура, сохраняющая все BarManagerы, CxGrid и параметры формы
procedure TSBaseFrm.SaveFormSettings(AStoreFormSettings: TFormItemsForSave);
var
  LStream: TStringStream;
  LVariableName: String;
  LVariableVersion: Byte;
  LcxGridStorageOptions: TcxGridStorageOptions;

  I: Integer;
begin
  try
    LStream := TStringStream.Create('');

    if frsvFormItems in StoreFormSettings then
    for I := Pred(ComponentCount) downto 0 do begin
      //------------------------ dxBarManager ----------------------------------
      if (Components[I] is TdxBarManager) then begin
        with Components[i] as TdxBarManager do if Version > 0 then begin
          LVariableVersion  := Version;
          LVariableName     := Name;

          LStream.Size := 0;
          SaveToStream(LStream);
          SBaseSetUserClassVariableProcedure(FInternalClassName, FVersion, FIdentity, LVariableName, LVariableVersion, LStream.DataString);
        end;

      //------------------------ cxGrid ----------------------------------------
      end else if (Components[i] is TcxCustomGridView) then begin
        with Components[i] as TcxCustomGridView do if Version > 0 then begin
          LVariableName     := Name;
          LVariableVersion  := Version;

          LStream.Size := 0;
          if frsvUserDefined in AStoreFormSettings then
            LcxGridStorageOptions := [gsoUseFilter]
          else
            LcxGridStorageOptions := [];
          StoreToStream(LStream, LcxGridStorageOptions);
          SBaseSetUserClassVariableProcedure(FInternalClassName, FVersion, FIdentity, LVariableName, LVariableVersion, LStream.DataString);
        end;

      //------------------------ cxTree ----------------------------------------
      end else if (Components[i] is TcxCustomDBTreeList) then begin
        with Components[i] as TcxCustomDBTreeList do if Version > 0 then begin
          LVariableName     := Name;
          LVariableVersion  := Version;

          LStream.Size := 0;
          StoreToStream(LStream);
          SBaseSetUserClassVariableProcedure(FInternalClassName, FVersion, FIdentity, LVariableName, LVariableVersion, LStream.DataString);
        end;
      end
    end;
  finally
    FreeAndNil(LStream);
  end;

  //-- Сохраняем параметры формы
  if (WindowState = wsNormal) then begin
    if ((not Visible) or (FormStyle = fsMDIChild)) and (frsvLeftTop in AStoreFormSettings) then begin
      SetSetting(SConst_Left, Left, FVersion);
      //SBaseSetUserClassVariableProcedure(FInternalClassName, FVersion, FIdentity, SConst_Left, 0, Left);
      SetSetting(SConst_Top, Top, FVersion);
      //SBaseSetUserClassVariableProcedure(FInternalClassName, FVersion, FIdentity, SConst_Top, 0, Top);
    end;

    if (frsvWidth in AStoreFormSettings) and (BorderStyle in [bsNone, bsSizeable]) then
      SetSetting(SConst_Width, Width, FVersion);
      //SBaseSetUserClassVariableProcedure(FInternalClassName, FVersion, FIdentity, SConst_Width, 0, Width);

    if (frsvHeight in AStoreFormSettings) and (BorderStyle in [bsNone, bsSizeable]) then
      SetSetting(SConst_Height, Height, FVersion);
      //SBaseSetUserClassVariableProcedure(FInternalClassName, FVersion, FIdentity, SConst_Height, 0, Height);
  end;
end;

//-- Функция, восстанавливающая настройки всех BarManager и CxGrid формы

procedure TSBaseFrm.LoadFormSettings(AStoreFormSettings: TFormItemsForSave);
  procedure StoreLocal(const AName: String; AVersion: Byte; const AValue: Variant);
  begin
    FSettings['{' + IntToStr(AVersion) + '}' + AName] := AValue;
  end;

var
  FUserVariables: TSBaseFrmUserVariables;
  FVariant: Variant;

  I: Integer;
  LcxGridStorageOptions: TcxGridStorageOptions;
  LStream: TStringStream;
begin
  try
    LStream  := TStringStream.Create('');

    FUserVariables := SBaseGetUserClassVariablesFunction(FInternalClassName, FVersion, FIdentity);

    if frsvFormItems in AStoreFormSettings then
    for i := Pred(ComponentCount) downto 0 do try

       //------------------------ TdxBarManager ---------------------------------
      if (Components[I] is TdxBarManager) then begin
        with Components[I] as TdxBarManager do if Version > 0 then begin
          FVariant := FUserVariables.ExtractVariable(Name, Version);

          if VarIsStr(FVariant) then begin
            LStream.Size := 0;
            LStream.WriteString(FVariant);
            LStream.Position := 0;
            LoadFromStream(LStream);
          end;
        end

      //------------------------ cxGrid ----------------------------------------
      end else if (Components[I] is TcxCustomGridView) then begin
        with Components[I] as TcxCustomGridView do
        if Version > 0 then begin
          FVariant := FUserVariables.ExtractVariable(Name, Version);

          if VarIsStr(FVariant) then begin
            LStream.Size := 0;
            LStream.WriteString(FVariant);
            LStream.Position := 0;
            BeginUpdate;
            if frsvUserDefined in AStoreFormSettings then
              LcxGridStorageOptions := [gsoUseFilter]
            else
              LcxGridStorageOptions := [];

            try
              RestoreFromStream(LStream, False, False, LcxGridStorageOptions);
            finally
              EndUpdate;
            end;
          end;
        end

      //------------------------ cxTree ----------------------------------------
      end else if (Components[i] is TcxCustomTreeList) then begin
        with Components[i] as TcxCustomTreeList do
        if Version > 0 then begin
          FVariant := FUserVariables.ExtractVariable(Name, Version);

          if VarIsStr(FVariant) then begin
            LStream.Size := 0;
            LStream.WriteString(FVariant);
            LStream.Position := 0;
            BeginUpdate;
            try
              RestoreFromStream(LStream);
            finally
              EndUpdate;
            end;
          end;
        end

      end
    except on E:Exception do
      SBaseMainFormMDIInterface.Log(lgError, ClassName + '.' + Components[i].Name + ': ' + E.Message);
    end;

    //-- Восстанавливаем параметры формы
    if (WindowState = wsNormal) then begin
      if ((not Visible) or (FormStyle = fsMDIChild)) and (frsvLeftTop in AStoreFormSettings) then begin
        FVariant := FUserVariables.ExtractVariable(SConst_Left);
        if VarIsPresent(FVariant) then
          try
            Left := FVariant;
            StoreLocal(SConst_Left, Self.FVersion, Left);
          except end;

        FVariant := FUserVariables.ExtractVariable(SConst_Top);
        if VarIsPresent(FVariant) then
          try
            Top := FVariant;
            StoreLocal(SConst_Top, Self.FVersion, Top);
          except end;
      end;

      if (frsvWidth in AStoreFormSettings) and (BorderStyle in [bsNone, bsSizeable]) then begin
        FVariant := FUserVariables.ExtractVariable(SConst_Width);
        if VarIsPresent(FVariant) then
          try
            Width := FVariant;
            StoreLocal(SConst_Width, Self.FVersion, Width);
          except end;
      end;

      if (frsvHeight in AStoreFormSettings) and (BorderStyle in [bsNone, bsSizeable]) then begin
        FVariant := FUserVariables.ExtractVariable(SConst_Height);
        if VarIsPresent(FVariant) then
          try
            Height := FVariant;
            StoreLocal(SConst_Height, Self.FVersion, Height);
          except end;
      end;
    end;

    // Сохраняем прочие настройки формы
    if (frsvUserDefined in AStoreFormSettings) then
      while FUserVariables.Count > 0 do with FUserVariables.ExtractVariable do
        StoreLocal(Name, Version, Value);
  finally
    FreeAndNil(LStream);
  end;
end;

procedure TSBaseFrm.LoadUserConfig(AStoreFormSettings: TFormItemsForSave);
begin
  //-- Запрещаем обновление окна
//  LockWindowUpdate(Handle);
  //Perform(WM_SETREDRAW, 0, 0);

  try
    FSettings.Clear;

    LoadFormSettings(AStoreFormSettings);
    if frsvUserDefined in AStoreFormSettings then
      CallFunction('LoadUserConfig', FCreateFormParams, False);
  finally
    FSettingsLoaded := True;
    //Perform(WM_SETREDRAW, 1, 0);
    //Refresh;
//    LockWindowUpdate(0);
  end
end;

procedure TSBaseFrm.LockMessages;
begin
  Inc(FMessageLockCount);
end;

procedure TSBaseFrm.PerformMessages;
begin
  if FMessageLockCount > 0 then begin
    Dec(FMessageLockCount);
    if (FMessageLockCount = 0) and (Length(FMessageText) > 0) then
      try
        if FMessageType = 'W' then
          hsDialogs.HSShowWarning(FMessageText, FMessageControl, FMessageX, FMessageY)
        else if FMessageType = 'E' then
          hsDialogs.HSShowError(FMessageText, FMessageControl, FMessageX, FMessageY)
      finally
        FMessageText    := '';
        FMessageControl := nil;
      end;
  end;
end;

procedure TSBaseFrm.SaveUserConfig(AStoreFormSettings: TFormItemsForSave);
begin
  SaveFormSettings(AStoreFormSettings);
  if frsvUserDefined in AStoreFormSettings then
    CallFunction('SaveUserConfig', FCreateFormParams, False);
end;

procedure TSBaseFrm.SBaseFormLoaded;
begin
//
end;

procedure TSBaseFrm.FormClose(Sender: TObject; var Action: TCloseAction);
var
  I         : Integer;
  LComponent: TComponent;
begin
  if SelectMode = smNone then Action := caFree;

  FLoading := True;
  try
    if FSettingsLoaded and (FStoreFormSettings <> []) then
      SaveUserConfig(FStoreFormSettings);
  except on E:Exception do begin
    SBaseMainFormMDIInterface.Log(lgError, E.Message);
  end end;

  try
    if not (fsModal in FormState) then begin
      for I := 0 to ComponentCount-1 do begin
        LComponent := Components[I];
        if LComponent.InheritsFrom(TCustomForm) then
          with TCustomForm(LComponent) do
            if not (csDestroying in ComponentState) then
              Close
      end;
    end;

    CallFunction('FormClose', False);
  except
    FLoading := False;
    Raise;
  end;

  Perform(WM_SETREDRAW, 0, 0);
  try
    InternalClose;
  except on E:Exception do begin
    SBaseMainFormMDIInterface.Log(lgError, E.Message);
  end end;

  if FormStyle = fsMDIChild then
    SBaseMainFormMDIInterface.WindowOnDestroy(Self);

  if fsModal in FormState then
    Perform(WM_SETREDRAW, 1, 0);
end;

function TSBaseFrm.GetVariables: TNamedVariants;
begin
  Result.UnknownAsNull := True;
  Result[SBFSelectModeActive] := (SelectMode <> smNone);
  Result := Result + SBaseFormProperties.InterfaceVariables + FInternalFormConstants + FCreateFormParams + FInternalFormParams;
{
  Result.AssignValues(SBaseFormProperties.InterfaceVariables, amAppend);
  Result.AssignValues(FInternalFormConstants, amAppend);
  Result.AssignValues(FCreateFormParams, amAppend);
  Result.AssignValues(FInternalFormParams, amAppend);
}
end;

function TSBaseFrm.GetSetting(const ASetting: String; AVersion: Byte = 0): Variant;
begin
  Result := FSettings.GetValue('{' + IntToStr(AVersion)+ '}' + ASetting);
  if VarIsEmpty(Result) then
    Result := Null
end;

function TSBaseFrm.SetSetting(const ASetting: String; const AValue: Variant; AVersion: Byte = 0): Boolean;
var
  LName: String;
begin
  try
    Result := False;
    LName := '{' + IntToStr(AVersion) + '}' + ASetting;
    if not FSettings.IsValueEqual(LName, AValue) then begin
      SBaseSetUserClassVariableProcedure(FInternalClassName, FVersion, FIdentity, ASetting, AVersion, AValue);
      FSettings.SetValue(LName, AValue);
    end;
  except
    Result := True;
  end;
end;

procedure TSBaseFrm.SetSQLConnectionProvider(const AValue: TSQLConnectionProvider);
begin
  if FSQLConnectionProvider <> AValue then begin
    FSQLConnectionProvider := AValue;
    FreeAndNil(FSQLAlternateConnectionProvider);
  end;
end;

procedure TSBaseFrm.SetStoreFormSettings(const Value: TFormItemsForSave);
begin
  FStoreFormSettings := Value;
  if frsvLeftTop in FStoreFormSettings then
    Position := poDesigned;
end;

procedure TSBaseFrm.Show;
begin
  if Visible or Assigned(Parent) or (not Assigned(Owner) or not (Owner is TForm)) then
    if WindowState = wsMinimized then
      WindowState := wsNormal
    else
      inherited Show
  else if fsModal in TForm(Owner).FormState then
    ShowModal
  else begin
    FormStyle := fsMDIChild;
    Position := poDesigned;
  end;
end;

procedure TSBaseFrm.ShowMDI;
begin
  if Visible then begin
    if WindowState=wsMinimized then
      WindowState := wsNormal;
    inherited Show;
  end else begin
    FormStyle := fsMDIChild;
    Position  := poDesigned;
  end;
end;

procedure TSBaseFrm.ShowMDIChildInVisibleArea;
var
  ARect: Trect;
  ALeft, ATop: SmallInt;
begin
  Windows.GetClientRect(Application.MainForm.ClientHandle, ARect);

  ALeft   := ARect.Right - Width;
  ATop    := ARect.Bottom - Height;
  if ALeft < 0 then ALeft := 0; if Left > ALeft then Left := ALeft;
  if ATop  < 0 then ATop  := 0; if Top  > ATop  then Top  := ATop;
end;

procedure TSBaseFrm.ShowMDIChildMaximized;
var
  ARect: Trect;
begin
  Windows.GetClientRect(Application.MainForm.ClientHandle, ARect);

  if WindowState = wsMinimized then WindowState := wsNormal;

  Left   := ARect.Left;
  Top    := ARect.Top;
  Width  := ARect.Right;
  Height := ARect.Bottom;

  Application.ProcessMessages;
  Windows.GetClientRect(Application.MainForm.ClientHandle, ARect);
  if ARect.Right <> Width then Width := ARect.Right;
  if ARect.Bottom <> Height then Height := ARect.Bottom;
end;

function TSBaseFrm.ShowModal: Integer;
begin
  if Position = poDefaultPosOnly then
    Position := poMainFormCenter;
  Result := (inherited ShowModal);
end;

procedure TSBaseFrm.ShowProgress(ACancelRequest: TNotifyEvent);
begin
{$IFDEF DEBUG}
  Assert(not Assigned(FProgress), 'FProgress <> nil');
{$ENDIF}

  FProgress := TSBaseProgressFrm.Create(Self);
  if Assigned(ACancelRequest) then
    FProgress.OnCancelButtonClick := ACancelRequest;
  FProgress.Show(Self);
end;

procedure TSBaseFrm.DoAlternateConnectionCancelRequest(Sender: TObject);
begin
  if Assigned(FSQLAlternateConnectionProvider) then
    MainDataModule.ConnectionTerminate(FSQLAlternateConnectionProvider);
end;

function TSBaseFrm.SQLAlternateConnectionProvider: TSQLConnectionProvider;
begin
  if not Assigned(FSQLAlternateConnectionProvider) then begin
    FSQLAlternateConnectionProvider := FSQLConnectionProvider.Clone(Self);
    FSQLAlternateConnectionProvider.Connected := True;
  end;

  Result := FSQLAlternateConnectionProvider;
end;

procedure TSBaseFrm.MoveToParentCenter;
var
  LForm: TForm;
begin
  if (Owner <> nil) and (Owner is TForm) then
    LForm := TForm(Owner)
  else
    LForm := Application.MainForm;

  if LForm = nil then Exit;

  Left := ((LForm.Width - Width) div 2);
  Top := ((LForm.Height - Height) div 2);

  if Parent = nil then begin
    Left := LForm.Left + Left;
    Top := LForm.Top + Top;
  end;

  if (fsModal in FormState) and not (fsModal in LForm.FormState) then begin
    Left := Application.MainForm.Left + Left;
    Top := Application.MainForm.Top + Top;
  end;

  if Left < 0 then Left := 0;
  if Top < 0 then Top := 0;
end;

procedure TSBaseFrm.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if Operation = opRemove then begin
    if Assigned(FDataSourceControls) then
      FDataSourceControls.DeleteGridControl(AComponent);
    if Assigned(FItemReactions) then
      FItemReactions.DeleteItemProperties(AComponent);
  end;
end;

procedure TSBaseFrm.WndProc(var AMessage: TMessage);
begin
  if (FormStyle <> fsMDIChild) or not (biMaximize in BorderIcons) then
    inherited
  else
    with AMessage do
      case Msg of
        WM_SYSCOMMAND:
          case wParam of
            SC_MAXIMIZE: ShowMDIChildMaximized;
            else inherited
          end;
        WM_NCLBUTTONDBLCLK:
          if wParam = HTCAPTION then ShowMDIChildMaximized
            else inherited
        else
          inherited
      end
end;

procedure TSBaseFrm.ActionScriptDebuggerExecute(Sender: TObject);
begin
  if FScriptDebuggerForm = nil then
    TSBaseScriptDebuggerFrm.CreateByParams(Self, TNamedVariants.Create(True)).Show
  else begin
    FScriptDebuggerForm.Show;
    FScriptDebuggerForm.BringToFront;
  end
end;

procedure TSBaseFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  I     : Integer;
  LC    : TComponent;
  LForm : TCustomForm;

  LParams: TNamedVariants;
begin
  CanClose := not Busy;
  if CanClose then begin
    LParams[SConst_CanClose] := True;
    CallFunctionOut('FormCloseQuery', LParams, False);
    CanClose := (LParams[SConst_CanClose] = True);
  end;

  if CanClose and not (fsModal in FormState) then
    for I := 0 to ComponentCount-1 do begin
      LC := Components[I];
      if LC.InheritsFrom(TCustomForm) then begin
        LForm := TCustomForm(LC);
        CanClose := LForm.CloseQuery;
        if not CanClose then
          Exit;
      end;
    end;
end;

function TSBaseFrm.GetParameters(Level: UInt16): TNamedVariants;
var
  LCollisions: Boolean;
begin
  LCollisions := False;
  InternalGetParameters(Level, Result, LCollisions);
end;

procedure TSBaseFrm.InternalGetParameters(ALevel: UInt16; var AParameters: TNamedVariants; var ACollisions: Boolean);
var
  I, Idx: Integer;
  LVariables: TNamedVariants;
begin
  LVariables := Variables;
  for I := Pred(LVariables.Count) downto 0 do with LVariables.Items[I] do begin
    if AParameters.Find(Name, Idx) and not VarSameValue(AParameters.Items[Idx].Value, Value) then
      ACollisions := True
    else
      AParameters[Name] := Value;
  end;

  if Assigned(FInternalGetParameters) then begin
    LVariables.Clear;
    LVariables[SConst_ALevel]       := ALevel;
    LVariables[SConst_AParameters]  := Integer(@AParameters);
    LVariables[SConst_ACollisions]  := ACollisions;
    CallFunctionOut(FInternalGetParameters, LVariables);
    ACollisions := (LVariables[SConst_ACollisions] = True);
  end;
end;

function TSBaseFrm.GetObjectsValues(const AParamNames: String; var AUnknownParams: String): Variant;
begin
  Result := FindObjectsValues(AParamNames, AUnknownParams, [ssSelf, ssApplication, ssFunctions]);
end;

function TSBaseFrm.FindObjectsValues(AParamNames: String; var AUnknownParams: String; AScanSources: TScanSources; ASender: TObject; ADisplayText: Boolean): Variant;
var
  Idx: Integer;
  LCount: Integer;
  LParamAlias: String;
  LParamValue: Variant;

  LObjectName, LFunction: String;
  LObjectPath: String;
  LThis, LObject: TObject;
  LField: TField;
  LParams: TParams;
  LParam: TParam;
  LUseOldValue: Boolean;

  procedure FindPath;
  begin
    Idx := Pos('.', LObjectPath);
    if Idx > 0 then begin
      LObjectName := Copy(LObjectPath, 1, Pred(Idx));
      LObjectPath := Copy(LObjectPath, Succ(Idx), MaxInt);
    end else begin
      LObjectName := LObjectPath;
      LObjectPath := '';
    end;
  end;

  function FindDataSetProperty(ADataSet: TDataSet): Boolean;
  begin
    LField := ADataSet.FindField(LObjectName);
    if not Assigned(LField) and SameText(LeftStr(LObjectName, 4), 'OLD_') then begin
      LField := ADataSet.FindField(Copy(LObjectName, 5, MAXSHORT));
      LUseOldValue := Assigned(LField);
    end else
      LUseOldValue := False;

    if Assigned(LField) then
      if ADisplayText then begin
        if LUseOldValue then
          LParamValue := VarToStr(LField.OldValue)
        else
          LParamValue := LField.DisplayText
      end else begin
        if LUseOldValue then
          LParamValue := LField.OldValue
        else
          LParamValue := LField.AsVariant
      end
    else begin
      LParams := IProviderSupportNG(ADataSet).PSGetParams;
      if Assigned(LParams) then
        LParam := LParams.FindParam(LObjectName)
      else
        LParam := nil;

      if Assigned(LParam) then
        LParamValue := LParam.Value
      else
        Exit(False);
    end;
    Result := True;
  end;

  function GetValue(out AValue: Variant): Boolean;
    label LB_NOTFOUND;
  begin
    Result := True;

    LThis := Self;
    FindPath;

    if Assigned(ASender) and SameText(LObjectName, SConst_Sender) then begin
      if LObjectPath.IsEmpty then begin
        AValue := Integer(ASender);
        Exit;
      end else if (ASender is TdxMDBarListItem) and TdxMDBarListItem(ASender).ShowValues and SameText(LObjectPath, SConst_ItemName) then begin
        Idx := TdxMDBarListItem(ASender).ItemIndex;
        if Idx >= 0 then
          AValue := TdxMDBarListItem(ASender).Items.Names[Idx]
        else
          AValue := Null;
        Exit;
      end else if (ASender is TdxMDBarButton) and SameText(LObjectPath, SConst_ItemName) then begin
        Idx := TdxMDBarButton(ASender).ItemIndex;
        if Idx >= 0 then
          AValue := TdxMDBarButton(ASender).ListItems.Names[Idx]
        else
          AValue := Null;
        Exit;
      end else begin
        LThis := ASender;
        FindPath;
      end;
    end;

    while Idx > 0 do begin
      if IsPublishedProp(LThis, LObjectName) then
        LObject := GetObjectProp(LThis, LObjectName)
      else
        LObject := nil;

      if (LObject = nil) and (LThis is TComponent) then
        LObject := TComponent(LThis).FindComponent(LObjectName);

{$IFDEF DATAHANDLER}
      if (LObject = nil) and (LThis is TSubDataHandler) and SameText(LObjectName, SConst_MemoryDataSet) then
        LObject := TSubDataHandlerCrack(LThis).MemoryDataSet;
{$ENDIF}

      if LObject = nil then
        Raise Exception.Create('Object property value '#171 + LObjectName + #187' cannot be found.');

      LThis := LObject;
      FindPath;
    end;

    if (LThis is TDataSource) then begin
      if not FindDataSetProperty(TDataSource(LThis).DataSet) then
        AValue := GetPropValue(LThis, LObjectName);
    end else if (LThis is TDataSet) then begin
      if not FindDataSetProperty(TDataSet(LThis)) then
{$IFDEF DEBUG}
        try
{$ENDIF}
          AValue := GetPropValue(LThis, LObjectName)
{$IFDEF DEBUG}
        except on EPropertyError do
          if LThis is TComponent then
            Raise EPropertyError.CreateResFmt(@SUnknownProperty, [TComponent(LThis).Name + '.' + LObjectName])
          else
            Raise;
        end;
{$ENDIF}
    end

    else if (LThis is TcxGridTableView) then begin
      AValue :=  TcxGridTableView(LThis).FocusedRecordValues[LObjectName]
    end
    else if (LThis is TcxDBTreeList) then begin
      if SameText(LObjectName, 'CHECKEDVALUES') then
        AValue := TcxDBTreeList(LThis).CheckedValues
      else
        AValue :=  TcxDBTreeList(LThis).FocusedRecordValues[LObjectName]
    end

    else if (LThis is TSBaseCustomLookupComponentControl) and InArray(TSBaseCustomLookupComponentControl(LThis).ParamName, LObjectName, ';') then
      AValue := TSBaseCustomLookupComponentControl(LThis).ParamValues[LObjectName]
    else if IsPublishedProp(LThis, LObjectName) then
      AValue := GetPropValue(LThis, LObjectName)
{$IFDEF PERIODIC_FORMS}
    else if (LThis is TDBPeriodicTabControl) and SameText(LObjectName, SConst_ActiveDate) then
      AValue := TDBPeriodicTabControl(LThis).ActiveDate
{$ENDIF}
{$IFDEF XXVIEW_FORMS}
    else if (LThis is TSBaseViewFilter) and InArray(TSBaseViewFilter(LThis).ParamName, LObjectName, ';') then
      AValue := TSBaseViewFilter(LThis).ParamValues[LObjectName]
{$ENDIF}
    else if (LThis is TcxCustomEdit) and SameText(LObjectName, SConst_EditValue) then
      AValue := TcxCustomEdit(LThis).EditValue
    else begin
      if (LThis is TComponent) then begin
        LObject := TComponent(LThis).FindComponent(LObjectName);
        if LObject <> nil then begin
          AValue := Integer(LObject);
          Exit;
        end;
      end;

      AValue := Null;
      AUnknownParams := ConcatIfNotEmpty(AUnknownParams, ';', LParamAlias);
    end;
  end;

begin
  Result := unAssigned;

  AUnknownParams := '';
  LCount := 0;

  while (not AParamNames.IsEmpty) do begin
    Idx := Pos(';', AParamNames);
    if Idx = 0 then begin
      LParamAlias := AParamNames;
      AParamNames := '';
    end else begin
      LParamAlias  := Copy(AParamNames, 1, Pred(Idx));
      AParamNames := Copy(AParamNames, Succ(Idx), MaxInt);
    end;

    ExtractNameAndAlias(LParamAlias, nil, @LObjectPath);

    if (ssFunctions in AScanSources) and FindPos('(', LObjectPath, Idx) and (LObjectPath[LObjectPath.Length] = ')') then begin
      LFunction := Copy(LObjectPath, 1, Idx - 1);
      LObjectPath := Copy(LObjectPath, Idx + 1, LObjectPath.Length - Idx - 1);

//      if LObjectPath.IsEmpty then
        if SameText(LFunction, SConst_Today) then
          LParamValue := Date
        else if SameText(LFunction, SConst_Now) then
          LParamValue := Now
        else if SameText(LFunction, SConst_NewGUId) then
          LParamValue := NewGUIdString
        else
          Raise Exception.CreateFmt(MFunctionNotFound, [LFunction])
//      else begin
//        GetValue(LParamValue);
//      end;
    end else
      GetValue(LParamValue);

    if LCount = 0 then
      Result := LParamValue
    else if LCount = 1 then
      Result := VarArrayOf([Result, LParamValue])
    else begin
      VarArrayRedim(Result, LCount);
      Result[LCount] := LParamValue;
    end;
    Inc(LCount);
  end;
end;

{
procedure FieldChangeEventGetField(Sender: TObject; out AFields: TFieldsList);
var
  I: Integer;
begin
  AFields := TFieldsList.Create;
  try
    if Assigned(Sender) then
      if Sender is TField then
        AFields.Add(TField(Sender))
      else if (Sender is TMDFieldDataLink) then
        with TMDFieldDataLink(Sender) do
          for I := 0 to FieldsCount - 1 do
            AFields.Add(Fields[I])
  except
    AFields.Free;
    Raise;
  end;
end;
}

procedure TSBaseFrm.StringFieldChange(Sender: TObject);
var
  J: Integer;
  LString: String;
  LField: TField absolute Sender;
begin
  if not Assigned(Sender) or not (Sender is TField) then Exit;
      if not LField.IsNull then begin
        LString := LField.AsString.Trim;
        if LString.IsEmpty then
          LField.Clear
        else begin
          for J := Length(LString) downto 1 do
            if CharInSet(LString[J], [#0..#31]) then
              LString[J] := ' ';

          if LField.AsString <> LString then
            LField.AsString := LString;
        end;
      end;
end;

procedure TSBaseFrm.TextFieldChange(Sender: TObject);
var
  LText: String;
  LField: TField absolute Sender;
begin
  if not Assigned(Sender) or not (Sender is TField) then Exit;

      if not LField.IsNull then begin
        LText := LField.AsString.Trim;
        if LText.IsEmpty then
          LField.Clear
        else if LField.AsString <> LText then
          LField.AsString := LText;
      end;
end;

procedure TSBaseFrm.VariantFieldChange(Sender: TObject);
var
  LText: String;
  LField: TVariantField absolute Sender;
begin
  if not Assigned(Sender) or not (Sender is TVariantField) then Exit;

  if (not LField.IsNull) and VarIsStr(LField.AsVariant) then begin
    LText := LField.AsString.Trim;
    if LText.IsEmpty then
      LField.Clear
    else if LField.AsString <> LText then
      LField.AsString := LText;
  end;
end;

procedure TSBaseFrm.AddParamItem(const AItem: String);
begin
  with TNamedVariant.Create(AItem) do
    FInternalFormParams[Name] := Value;
end;

class procedure TSBaseFrm.AdjustSize(AControl: TWinControl);

  procedure SetWidth(AControl: TWinControl; AWidth: Integer);
  begin
    if (AWidth <= 0) or (AControl.Width = AWidth) then Exit;

    if AControl.Align in [alTop, alBottom, alClient] then
      SetWidth(AControl.Parent, AControl.Parent.Width + (AWidth - AControl.Width) {+ AControl.Parent.Padding.Right})
    else
      AControl.Width := AWidth;
  end;

  procedure SetHeight(AControl: TWinControl; AHeight: Integer);
  begin
    if (AHeight <= 0) or (AControl.Height = AHeight) then Exit;

    if AControl.Align in [alLeft, alRight, alClient] then
      SetHeight(AControl.Parent, AControl.Parent.Height + (AHeight - AControl.Height) {+ AControl.Parent.Padding.Bottom})
    else
      AControl.Height := AHeight;
  end;

var
  I: Integer;
  LWidth, LMaxWidth: Integer;
  LHeight, LMaxTopHeight, LMinBottom: Integer;
  LControl: TControl;
begin
  LMaxWidth  := 0;
  LMaxTopHeight := 0; LMinBottom := AControl.Height - AControl.Padding.Bottom;
  for i := 0 to AControl.ControlCount - 1 do begin
    LControl := AControl.Controls[i];
    if LControl.Visible then begin
      if LControl.Align = alClient then Exit;
      if LControl.Align in [alTop, alBottom] then
        LMaxWidth := -1
      else if LMaxWidth <> -1 then begin
        LWidth := LControl.Left + LControl.Width;
        if LMaxWidth < LWidth then
          LMaxWidth := LWidth;
      end;
      if LControl.Align in [alLeft, alRight] then
        LMaxTopHeight := -1
      else if LMaxTopHeight <> -1 then begin
        if LControl.Align = alBottom then begin
          if LControl.Top < LMinBottom then
            LMinBottom := LControl.Top;
        end else begin
          LHeight := LControl.Top + LControl.Height;
          if LMaxTopHeight < LHeight then
            LMaxTopHeight := LHeight;
        end;
      end;
    end;
  end;
  SetWidth(AControl, LMaxWidth);
  SetHeight(AControl, LMaxTopHeight + AControl.Padding.Top + AControl.Height - LMinBottom);
end;

procedure TSBaseFrm.AfterSkinChanged;
var
  LDefaultInNativeStyle : Boolean;
//  LCheckBoxInNativeStyle: Boolean;
  I: Integer;
  LComponent: TComponent;
begin
  LDefaultInNativeStyle := (SBaseMainFormMDIInterface = nil)
                            or SBaseMainFormMDIInterface.dxSkinController.NativeStyle
                            or SBaseMainFormMDIInterface.dxDefaultStyle;

//  LCheckBoxInNativeStyle := LDefaultInNativeStyle or SBaseMainFormMDIInterface.dxSkinController.UseSkins or (SBaseMainFormMDIInterface.dxSkinController.Kind in [lfOffice11]);
//  Perform(WM_SETREDRAW, 0, 0);
//  try

    for I := 0 to ComponentCount - 1 do begin
      LComponent := Components[I];
{
      if LComponent is TcxCustomCheckBox then
        with TcxCustomCheckBox(LComponent) do begin
          Style.LookAndFeel.NativeStyle := LCheckBoxInNativeStyle;
        end
      else
}
      if LComponent is TcxCustomGrid then
        with TcxCustomGrid(LComponent) do begin
          LookAndFeel.NativeStyle := LDefaultInNativeStyle;
        end
      else if LComponent is TcxCustomTreeList then
        with TcxCustomTreeList(LComponent) do begin
          LookAndFeel.NativeStyle := LDefaultInNativeStyle;
        end
      else if LComponent is TcxCustomTabControl then begin
        with TcxCustomTabControlCrack(LComponent) do
          if ParentBackground then
            if LDefaultInNativeStyle then
              Style := cxPCSlantedStyle
            else
              Style := cxPCDefaultStyle;
      end else if LComponent is TdxBarManager then
        with TdxBarManager(LComponent) do begin
          if LDefaultInNativeStyle then
            Style := bmsOffice11
          else
            Style := bmsUseLookAndFeel
        end
      else if LComponent is TdxMDStatusBar then
        with TdxMDStatusBar(LComponent) do begin
          if LDefaultInNativeStyle then
            PaintStyle := DefaultPaintStyle
          else
            PaintStyle := stpsUseLookAndFeel
        end
//      else if LComponent is TcxCustomSplitter then
//        TcxCustomSplitter(LComponent).CorrectSelfPosition
    end;

    CallFunction
    (
      'AfterSkinChanged',
      SConst_SkinStyle,
      (
        Assigned(SBaseMainFormMDIInterface)
        and
        (SBaseMainFormMDIInterface.dxSkinController.UseSkins)
      ),
      False
    );

//  finally
//    Perform(WM_SETREDRAW, 1, 0);
//    Refresh;
//  end;
end;

procedure TSBaseFrm.AddConstantItem(const AItem: String);
begin
  with TNamedVariant.Create(AItem) do
    FInternalFormConstants[Name] := Value;
end;

procedure TSBaseFrm.SetEncapsulateCreateFormParams(const Value: String);
begin
  FEncapsulateCreateFormParams := Value;
  if not Loading then
    DoEncapsulateCreateFormParams;
end;

procedure TSBaseFrm.SetFilterItem(const AComponent: TComponent; const AParamName: String);
begin
  Assert(Assigned(AComponent) and (Length(AParamName) > 0));

  if FLinkedFilterItems = nil then
    FLinkedFilterItems := TStringList.Create
  else
    Assert(FLinkedFilterItems.IndexOf(AParamName) = -1);

  if AComponent is TcxCustomEdit then
    TcxCustomEdit(AComponent).ActiveProperties.OnEditValueChanged := cxFilterEditItemOnEditValueChanged;

  FLinkedFilterItems.AddObject(AParamName, AComponent);

  if AComponent is TcxCustomEdit then
    cxFilterEditItemOnEditValueChanged(AComponent);
end;

procedure TSBaseFrm.SetIconIndex(AValue: Integer);
begin
  if (SBaseFormProperties.Icon16List <> nil) and (AValue >= 0) then
    SBaseFormProperties.Icon16List.GetIcon(AValue, Icon);
end;

procedure TSBaseFrm.DoEncapsulateCreateFormParams;
var
  LEncapsulateCreateFormParams: String;
  LParamName: String;
  Idx: Integer;
begin
  LEncapsulateCreateFormParams := FEncapsulateCreateFormParams;
  while LEncapsulateCreateFormParams <> '' do begin
    LParamName := ExtractAndRemoveValue(LEncapsulateCreateFormParams, ';');
    if CreateFormParams.Find(LParamName, Idx) then begin
      InternalFormParams[LParamName] := CreateFormParams.Items[Idx].Value;
      CreateFormParams.Delete(Idx)
    end;
  end
end;

procedure TSBaseFrm.DoKeepCreateFormParams;
var
  LCreateFormParamsAllowed: Variant;
begin
  if FVirtualClassInfo.CreateFormParamsAllowed.IsEmpty then
    CreateFormParams.Clear
  else if FVirtualClassInfo.CreateFormParamsAllowed <> '*' then begin
    LCreateFormParamsAllowed := CreateFormParams[FVirtualClassInfo.CreateFormParamsAllowed];
    CreateFormParams.Clear;
    CreateFormParams[FVirtualClassInfo.CreateFormParamsAllowed] := LCreateFormParamsAllowed;
  end;
end;

{$IFDEF DEBUG_FORMS}
procedure TSBaseFrm.DoExceptionDueExecute(AScript: TfsScript; const AErrorPos: String; AException: Exception{; ASourceLines: String; ASourceLineNumber: Integer});
begin
{$IFDEF DEBUG}
  if (FScriptDebuggerForm <> nil) and TSBaseScriptDebuggerFrm(FScriptDebuggerForm).IsDebugging then begin
    hsDialogs.HSMessageDlg(AException.Message, mtError, [mbOk], 0);
    Abort;
  end else if hsDialogs.HSMessageDlg(AException.Message, mtError, [mbOk, mbAbort], 0) = mrAbort then
    with AScript.ErrorInfo(AErrorPos) do begin

      if FScriptDebuggerForm = nil then
        FScriptDebuggerForm :=
          TSBaseScriptDebuggerFrm.CreateByParams
          (
            Self,
            TNamedVariants.Create
            (
              [
                TNamedVariant.Create(SConst_Paused, True),
                TNamedVariant.Create(SConst_Line, Line + 1)
              ]
              , True
            )
          )
      else
        TSBaseScriptDebuggerFrm(FScriptDebuggerForm).SetErrorLine(Line + 1);

      TSBaseScriptDebuggerFrm(FScriptDebuggerForm).Show;
      { TODO: ЧТо за хуйня?
      if AException.Message <> 'Run script debugger' then
        Abort;
      }
    end
  else
    Abort;
{$ELSE}
  hsDialogs.HSShowError(AException.Message + #13+#10 + AScript.ErrorInfo(AErrorPos).Scope);
{$ENDIF}
end;
{$ENDIF}

procedure TSBaseFrm.DoRequireCreateFormParams;
var
  LCreateFormParamsRequired: String;
  LParamName: String;
  Idx: Integer;
begin
  LCreateFormParamsRequired := FVirtualClassInfo.CreateFormParamsRequired;
  while (not LCreateFormParamsRequired.IsEmpty) do begin
    LParamName := ExtractAndRemoveValue(LCreateFormParamsRequired, ';');
    if not CreateFormParams.Find(LParamName, Idx) then
      Raise Exception.Create('TSBaseFrm: Abstract Error - Require parameter '#171 + LParamName + #187' is passed');
  end
end;

procedure TSBaseFrm.SetClickItem(AValue: TComponent);
  procedure StoreClick;
  begin
    if not Assigned(FClickItems) then
      FClickItems := TList<TComponent>.Create;

    FClickItems.Add(AValue);
  end;
begin
  if (AValue is TSBaseViewFilter) then
    AValue := TSBaseViewFilter(AValue).EditControl;


  if (AValue is TcxBarEditItem) then
    with TcxBarEditItem(AValue) do begin
      if not Self.Visible then
        StoreClick
      else begin
        SetFocus(False);
        with TcxBarEditItemControl(FocusedItemLink.Control) do
          if Edit is TcxCustomDropDownEdit then
            TcxCustomDropDownEdit(Edit).DroppedDown := True
          else if Edit is TcxCustomPopupEdit then
            TcxCustomDropDownEdit(Edit).DroppedDown := True;

  //      DroppedDown := True;
      end;
    end
  else if AValue is TdxBarItem then
    TdxBarItem(AValue).Click
  else if AValue is TControl then
    TControlCrack(AValue).Click
end;

procedure TSBaseFrm.ClickItems;
begin
  if not Assigned(FClickItems) or (not Visible) then Exit;

  while FClickItems.Count > 0 do begin
    ClickItem := FClickItems.Items[0];
    FClickItems.Delete(0);
  end;

  FreeAndNil(FClickItems);
end;

function TSBaseFrm.GetFormParamValues(const AParamNames: String; var AUnknownParams: String): Variant;
begin
  Result := Variables.ReadValues(AParamNames, AUnknownParams, True);
end;

function TSBaseFrm.GetVariable(const AParamName: String; ANullIfNotFound: Boolean): Variant;
var
  LIdx: Integer;
begin
  if FInternalFormConstants.Find(AParamName, LIdx) then
    Result := FInternalFormConstants.Items[lIdx].Value
  else if FCreateFormParams.Find(AParamName, LIdx) then
    Result := FCreateFormParams.Items[lIdx].Value
  else if FInternalFormParams.Find(AParamName, LIdx) then
    Result := FInternalFormParams.Items[lIdx].Value
  else if ANullIfNotFound then
    Result := Null
  else
    Result := Unassigned
end;

procedure TSBaseFrm.AfterDataChanged(const ATablesChanges: TTablesChanges);
var
  LLink: TDataSourceControlPropertiesLink;
  LTableChanges: TTableChanges;

  LDataSet: TDataSet;
  LDataSetProvider: TSQLDataSetProvider absolute LDataSet;

  LDataSetsAppliedChanges: TDataSetsAppliedChanges;
  LDataSetAppliedChanges: PDataSetAppliedChanges;

  procedure UpdateDataSourceControl(const AProperties: TDataSourceControlProperties);
  var
    I: Integer;

    LGridKeyFieldNames, LGridKeyFieldName: String;
    LMultiKeyFields, LKeyFieldMode, LKeyFieldMultiRecordsMode: Boolean;

    LInsertEmulate, LDeleteEmulate: Boolean;
    LInsertAllowed, LDeleteAllowed: Boolean;

    LDataSourceStateBookmark   : TDataSourceStateBookmark;

    LKeyFieldSValue: String;
    LKeyFieldValues: Variant;
    LFieldNames, LFieldName, LCondition: String;
    LParamValues: TNamedVariants;
    LParam: TParam;
  begin
    AProperties.BeginUpdate(LDataSourceStateBookmark);
    try
      if LTableChanges.FullRefresh then
        LDataSetAppliedChanges^.Refresh
      else begin
        LGridKeyFieldNames := AProperties.KeyFieldNames;
        Assert(not LGridKeyFieldNames.IsEmpty);
        I := Pos(';', LGridKeyFieldNames);
        if I = 0 then begin
          LMultiKeyFields   := False;
          LGridKeyFieldName := LGridKeyFieldNames;
        end else begin
          LMultiKeyFields   := True;
          LGridKeyFieldName := LeftStr(LGridKeyFieldNames, I - 1);
        end;

        LParam := LDataSetProvider.Params.FindParam(LLink.FieldNames);
        if Assigned(LParam) then begin
          LKeyFieldMode   := False;
          LKeyFieldMultiRecordsMode := False;
          LInsertEmulate  := False;
          LDeleteEmulate  := False;
        end else begin
          LKeyFieldMode := (Pos(';', LLink.FieldNames) = 0) and (LLink.KeyField or SameText(LGridKeyFieldName, LLink.FieldNames));
          LKeyFieldMultiRecordsMode := LKeyFieldMode and LMultiKeyFields;

          if LKeyFieldMode and LDataSet.CanModify and (not LDataSetProvider.AutoGenerateUpdateScripts) then begin
            LInsertEmulate := (LDataSetProvider.SQLInsert.Count = 0);
            LDeleteEmulate := (LDataSetProvider.SQLDelete.Count = 0);
          end else begin
            LInsertEmulate := False;
            LDeleteEmulate := False;
          end;
        end;

        LDataSetProvider.UpdateStatusFilter := Default_UpdateStatusFilter;
        LParamValues.Create(True);

        for I := 0 to LTableChanges.ChangeCount - 1 do
          with LTableChanges.Changes[I] do begin
            if Assigned(LParam) then begin
              if LParam.AsVariant = KeyValue then begin
                LDataSetAppliedChanges^.Refresh;
                Exit;
              end;
            end else begin
              if (not LKeyFieldMode) and (Action <> 'U') then Continue;
              LKeyFieldSValue := VarToStr(KeyValue);

              LInsertAllowed := LInsertEmulate and (Action <> 'D');
              LDeleteAllowed := LDeleteEmulate and (Action = 'D');

              LCondition := '';
              LFieldNames := LLink.FieldNames;
              while (not LFieldNames.IsEmpty) do begin
                LFieldName := ExtractAndRemoveValue(LFieldNames, ';');
                if (Action = 'U') or LKeyFieldMode {and InArray(LGridKeyFieldNames, LFieldName, ';'))} then begin
                  LCondition := ConcatIfNotEmpty(LCondition, ' OR ', LFieldName) + '=';

                  if LTableChanges.KeyFieldType in [ftString, ftWideString] then
                    LCondition := LCondition + QuotedStr(LKeyFieldSValue)
                  else
                    LCondition := LCondition + LKeyFieldSValue;
                end;
              end;
              if LCondition.IsEmpty then Continue;

              LDataSet.Filter := LCondition;
              LDataSet.Filtered := True;

              if LKeyFieldMultiRecordsMode and LInsertAllowed then begin
                if LDataSetAppliedChanges^.ApplyChange(KeyValue) then begin
                  LParamValues.AssignValues(LDataSetProvider.Params, amOverride);
                  LParamValues[LGridKeyFieldName] := KeyValue;
                  with LDataSetProvider.ConnectionProvider.OpenSQLQuery(LDataSetProvider.SQLRefresh.Text, LParamValues) do begin
                    ReadOnly := False;
                    if (not LDataSet.IsEmpty) then begin
                      LDataSet.First;
                      while (not LDataSet.Eof) do begin
                        LKeyFieldValues := LDataSet.FieldValues[LGridKeyFieldNames];
                        if Locate(LGridKeyFieldNames, LKeyFieldValues, []) then begin
                          LDataSet.Edit;
                            LDataSet.Fields.SetAllEditAble;
                            LDataSet.Fields.Load(Fields, lvcAlways);
                          LDataSet.Post;
                          if (not LDataSet.IsEmpty) and VarIsEqual(LKeyFieldValues, LDataSet.FieldValues[LGridKeyFieldNames]) then
                            LDataSet.Next;
                          Delete;
                        end else if LDeleteAllowed then
                          LDataSet.Delete
                        else begin
                          LDataSetProvider.RefreshRecord;
                          if (not LDataSet.IsEmpty) and VarIsEqual(LKeyFieldValues, LDataSet.FieldValues[LGridKeyFieldNames]) then
                            LDataSet.Next;
                        end;
                      end;
                    end;

                    if (not IsEmpty) then begin
                      First;
                      while (not Eof) do begin
                        LDataSet.Append;
                          LDataSet.Fields.SetAllEditAble;
                          LDataSet.Fields.Load(Fields, lvcAlways);
                        LDataSet.Post;
                        Next;
                      end;
                    end;
                  end;
                end;
              end else if (not LDataSet.IsEmpty) then begin
                LDataSet.First;
                while (not LDataSet.Eof) do begin
                  LKeyFieldValues := LDataSet.FieldValues[LGridKeyFieldNames];

                  if LDataSetAppliedChanges^.ApplyChange(LKeyFieldValues) then begin
                    if LDeleteAllowed then
                      LDataSet.Delete
                    else begin
                      LDataSetProvider.RefreshRecord;
                      if (not LDataSet.IsEmpty) and VarIsEqual(LKeyFieldValues, LDataSet.FieldValues[LGridKeyFieldName]) then
                        LDataSet.Next;
                    end
                  end else
                    LDataSet.Next;
                end;
              end else if LKeyFieldMode and LInsertAllowed and LDataSetAppliedChanges^.ApplyChange(KeyValue) then begin
                LDataSet.Append;
                if Assigned(LDataSetProvider.MasterSource) then
                  LDataSet.FieldValues[LDataSetProvider.DetailFields] := LDataSetProvider.MasterSource.DataSet.FieldValues[LDataSetProvider.MasterFields];
                LDataSet.FieldByName(LLink.FieldNames).AsVariant := KeyValue;
                LDataSet.Post;
                if not LDataSet.IsEmpty then
                  LDataSetProvider.RefreshRecord;
              end;
            end;
          end;

        if LDataSetProvider.CachedUpdates and LDataSetProvider.UpdatesPending then begin
          LDataSetProvider.ApplyUpdates;
          LDataSetProvider.CommitUpdates;
        end;
      end;
    finally
      AProperties.EndUpdate(LDataSourceStateBookmark);
    end;
  end;

var
  LDataSets: Array of TSQLDataSetProvider;

  function NeedRefresh(const AProperties: TDataSourceControlProperties): Boolean;
  var
    I: Integer;
  begin
    Result := False;

    LDataSet := AProperties.SourceDataSet.Source;
    Assert(LDataSet is TSQLDataSetProvider);
    if (not Assigned(LDataSet)) or (not LDataSet.Active) or TSQLDataSetProvider(LDataSet).Thread.Active then Exit;

    LDataSetAppliedChanges := LDataSetsAppliedChanges.RegisterDataSet(LDataSetProvider);
    if LDataSetAppliedChanges^.FullRefresh then Exit;
    for I := 0 to Length(LDataSets) - 1 do
      if LDataSets[I] = LDataSetProvider then
        Exit;

    Result := True;
  end;

var
  T, D, L: Integer;
begin
  if Busy then Exit;

  for T := 0 to ATablesChanges.TableCount - 1 do begin
    LTableChanges := ATablesChanges.Tables[T];
    if not CreateFormParams.Contains(LTableChanges.Params) then Continue;

    SetLength(LDataSets, 0);

    for D := 0 to FDataSourceControls.Count-1 do begin
      with FDataSourceControls.Items[D] do begin
        for L := 0 to Links.Count - 1 do begin
          LLink := Links.Items[L];
          if SameText(LLink.Table, LTableChanges.Table) and (not LLink.FieldNames.IsEmpty) and NeedRefresh(Properties) then begin
            UpdateDataSourceControl(Properties);
            Break;
          end;
        end;
      end;
    end;
  end;
end;

procedure TSBaseFrm.DataSetProviderPrepareParams(ADataSetProvider: TCustomSQLDataSetProvider);
var
  I: Integer;
  LParamName, LParamNames, LUnknownParamNames: String;
begin
  LParamNames := '';
  for I := 0 to ADataSetProvider.Params.Count - 1 do begin
    LParamName := ADataSetProvider.Params[I].Name;
    if not SameText(LeftStr(LParamName, 6), SConst_PARAM_) then
      LParamNames := ConcatIfNotEmpty(LParamNames, ';', LParamName);
  end;

  if (not LParamNames.IsEmpty) then begin
    ADataSetProvider.Params.ParamValues[LParamNames] := GetObjectsValues(LParamNames, LUnknownParamNames);
    if (not LUnknownParamNames.IsEmpty) then begin
//      LParamNames := LUnknownParamNames;
      ADataSetProvider.Params.ParamValues[LUnknownParamNames] := Parameters[ADataSetProvider.Level][LUnknownParamNames];
    end;
  end;
end;

function TSBaseFrm.SetSaveAllowed: Boolean;
begin
  Result := FSaveAllowed and (Owner = Application.MainForm);
end;

function TSBaseFrm.PrepareParameters(AParamNames: String; const AParams: TNamedVariants; AGetVarValuesFunction: TGetVarValuesFunction = nil; ASender: TObject = nil): TNamedVariants;
var
  LUnknownNames: String;
  LValues: Variant;
begin
  if (not AParamNames.IsEmpty) and (AParams.Count > 0) then begin
    LValues := AParams.ReadValues(AParamNames, LUnknownNames, True);
    Result := TNamedVariants.Create(False);
    Result.WriteValues(AParamNames, LValues, True);
    AParamNames := LUnknownNames;
  end else
    Result := TNamedVariants.Create(False);

  if (not AParamNames.IsEmpty) and Assigned(AGetVarValuesFunction) then begin
    Result.WriteValues(AParamNames, AGetVarValuesFunction(AParamNames, LUnknownNames), True);
    AParamNames := LUnknownNames;
  end;

  if (not AParamNames.IsEmpty) and Assigned(ASender) then begin
    Result.WriteValues(AParamNames, FindObjectsValues(AParamNames, LUnknownNames, [ssSender], ASender), True);
//    AParamNames := LUnknownNames;
  end;
end;

function TSBaseFrm.ExecSQL(const ACommandText: String; const AParams: TNamedVariants; AGetVarValuesFunction: TGetVarValuesFunction; ASender: TObject; AThread: Boolean): TNamedVariants;
var
  LDataSet: TDataSet;
begin
  LDataSet := OpenSQLQuery(ACommandText, AParams, AGetVarValuesFunction, ASender, AThread);
  Result := TNamedVariants.Create(False);

  try
    if LDataSet.Active and (not LDataSet.IsEmpty) then
      Result.AssignValues(LDataSet.Fields);
  finally
    CloseSQLQuery;
  end
end;

function TSBaseFrm.OpenSQLQuery(const ACommandText: String; AParams: TNamedVariants; AGetVarValuesFunction: TGetVarValuesFunction; ASender: TObject; AThread: Boolean): TCustomInternalQuery;
var
  LThread: TThread;
  LQuery : TCustomInternalQuery;
  LError : String;
begin
  AParams := PrepareParameters(ParseSQLParams(ACommandText), AParams, AGetVarValuesFunction, ASender);
  if AThread then begin
    if InProgress then
      Raise Exception.Create('Could not start Query.Execute process while TSBaseFrm.InProgress property is active');

    LQuery := SQLAlternateConnectionProvider.GetSQLQuery(ACommandText, AParams);
    Result := LQuery;
    LThread := TThread.CreateAnonymousThread
    (
      procedure
      begin
        CoInitializeEx(nil, COINIT_MULTITHREADED);
        try
          LQuery.Execute;
        except on E:Exception do
          LError := E.Message
        end;
        CoUninitialize;
      end
    );

    LError := '';
    ShowProgress(DoAlternateConnectionCancelRequest);
    try
      LThread.Start;
      while not LThread.Finished do
        HandleMessage;
    finally
      HideProgress
    end;
    if not LError.IsEmpty then begin
      LQuery.Close; LQuery.UnPrepare;
      Raise Exception.Create(LError);
    end;
  end else
    Result := SQLConnectionProvider.OpenSQLQuery(ACommandText, AParams);

  with Result do
    while True do begin
      if Active and (FieldCount = 2) and SameText(Fields[0].FieldName, SConst_Log_Type) and SameText(Fields[1].FieldName, SConst_Log_Message)
      then begin
        if not IsEmpty then begin
          First;
          while not Eof do begin
            SBaseMainFormMDIInterface.Log(Fields[0].AsString[1], Fields[1].AsString);
            Next;
          end;
        end;

        OpenNext;
        Continue;
      end else
        Break;
    end;
end;

procedure TSBaseFrm.CloseSQLQuery;
begin
  SQLConnectionProvider.CloseSQLQuery
end;

function TSBaseFrm.OpenSQLPublicQuery(const ACommandText: String; AParams: TNamedVariants): TUserMemoryDataSet;
var
  LParams: TParams;
begin
  LParams := TParams.Create(nil);
  try
    AParams := PrepareParameters(TCustomSQLDataSetProvider.ParseSQLParams(ACommandText, LParams), AParams);
    LParams.Load(AParams, lvcAlways);
    Result := SQLConnectionProvider.OpenSQLPublicQuery(ACommandText, LParams);
  finally
    LParams.Free;
  end;
end;

procedure TSBaseFrm.CloseSQLPublicQuery;
begin
  SQLConnectionProvider.CloseSQLPublicQuery
end;

function TSBaseFrm.PrepareFunction(const AFunction: String; const ARaiseIfNotFound: Boolean = True): TfsProcVariable;
var
  LVariable: TfsCustomVariable;
begin
  Result := nil;

  if FScript = nil then
    if ARaiseIfNotFound then
      Raise Exception.Create('The form does not contain any script lines.')
    else
      Exit;

  LVariable := FScript.FindLocal(AFunction);
  if (LVariable = nil) or not (LVariable is TfsProcVariable) then
    if ARaiseIfNotFound then
      Raise Exception.Create('The function '#171 + AFunction + #187' does not exists.')
    else
      Exit
  else
    Result := TfsProcVariable(LVariable);
end;

function TSBaseFrm.CallFunction(const AFunction: String; const ARaiseIfNotFound: Boolean = True): Variant;
begin
  Result := CallFunction(PrepareFunction(AFunction, ARaiseIfNotFound));
end;

function TSBaseFrm.CallFunction(const AFunction: TfsProcVariable): Variant;
begin
  if AFunction = nil then
    Result := unAssigned
  else
    with AFunction do begin
      Result := Value
    end;
end;

function TSBaseFrm.CallFunction(const AFunction: String; const AParams: TNamedVariants; const ARaiseIfNotFound: Boolean = True): Variant;
begin
  Result := CallFunction(PrepareFunction(AFunction, ARaiseIfNotFound), AParams);
end;

function TSBaseFrm.CallFunction(const AFunction: TfsProcVariable; const AParams: TNamedVariants): Variant;
var
  I: Integer;
begin
  if AFunction = nil then
    Result := unAssigned
  else
    with AFunction do begin
      for I := 0 to ParamCount - 1 do
        with Params[I] do
          Value := AParams[Name];

      Result := Value
    end;
end;

function TSBaseFrm.CallFunction(const AFunction: String; const AParams: Array of TNamedVariant; const ARaiseIfNotFound: Boolean = True): Variant;
begin
  Result := CallFunction(AFunction, TNamedVariants.Create(AParams), ARaiseIfNotFound);
end;

function TSBaseFrm.CallFunction(const AFunction, AParamNames: String; const AParamValues: Variant; const ARaiseIfNotFound: Boolean = True): Variant;
begin
  Result := CallFunction(PrepareFunction(AFunction, ARaiseIfNotFound), AParamNames, AParamValues);
end;

function TSBaseFrm.CallFunction(const AFunction: TfsProcVariable; const AParamNames: String; const AParamValues: Variant): Variant;
var
  I: Integer;
begin
  if AFunction = nil then
    Result := unAssigned
  else
    with AFunction do begin
      for I := 0 to ParamCount - 1 do
        with Params[I] do
          Value := VarArrayRead(AParamNames, AParamValues, Name);

      Result := Value
    end;
end;

{
function TSBaseFrm.CallFunction(const AFunction: String; const AParams: array of Variant; const ARaiseIfNotFound: Boolean): Variant;
var
  I: Integer;
  LFunction: TfsProcVariable;
begin
  LFunction := PrepareFunction(AFunction, ARaiseIfNotFound);
  if LFunction = nil then
    Result := unAssigned
  else
    with LFunction do begin
      for I := 0 to Length(AParams) - 1 do
        with Params[I] do
          Value := AParams[I];

      Result := Value
    end;
end;
}

function TSBaseFrm.CallFunctionOut(AFunction: TfsProcVariable; var AParams: TNamedVariants): Variant;
var
  I: Integer;
begin
  if AFunction = nil then
    Result := unAssigned
  else
    with AFunction do begin
      for I := 0 to ParamCount - 1 do
        with Params[I] do
          Value := AParams[Name];

      Result := Value;
      AParams.Clear;
      //AParams.UnknownAsNull := False;

      for I := 0 to ParamCount - 1 do
        with Params[I] do
          if IsVarParam then
            AParams[Name] := Value;
    end;
end;

function TSBaseFrm.CallFunctionOut(const AFunction: String; var AParams: TNamedVariants; const ARaiseIfNotFound: Boolean = True): Variant;
begin
  Result := CallFunctionOut(PrepareFunction(AFunction, ARaiseIfNotFound), AParams);
end;

procedure SBaseClassesFinalization;
begin
  FreeAndNil(SBaseVirtualClasses);
end;
{$ENDREGION 'TSBaseFrm'}

{$REGION 'TVerticalCloseSeparator'}
{ TVerticalCloseSeparator }

constructor TVerticalCloseSeparator.Create(AOwner: TComponent);
begin
  inherited;

  Width  := 10;
  Height := 16;

  FCloseButton := TSpeedButton.Create(Self);
  with FCloseButton do begin
    Caption := '';
    Parent  := Self;

    Left    := 0;
    Top     := 1;
    Width   := 10;
    Height  := 10;

    with Glyph do begin
//      Create;
      Width  := 6;
      Height := 6;
      with Canvas do begin
        Pen.Color := clBlack;
        MoveTo( 0, 0);
        LineTo( 5, 5);
        MoveTo( 4, 0);
        LineTo(-1, 5);
      end;
    end;
  end;

  with TImage.Create(Self) do begin
    Parent  := Self;

    Left    := 2;
    Top     := 12;
    Height  := 1;
    Width   := 6;

    with Picture do begin
      Create;
      with Bitmap do begin
        Width   := 6;
        Height  := 1;
        with Canvas do begin
          Pixels[0, 0] := clWindow;
          Pixels[1, 0] := clWindow;
          Pixels[2, 0] := clBtnShadow;
          Pixels[3, 0] := clWindow;
          Pixels[4, 0] := clWindow;
          Pixels[5, 0] := clBtnShadow;
        end;
      end;
    end;
  end;

  with TImage.Create(Self) do begin
    Parent  := Self;

    Left    := 2;
    Top     := 13;
    Height  := 1;
    Width   := 6;

    Stretch := True;

    with Picture do begin
      Create;
      with Bitmap do begin
        Width   := 6;
        Height  := 1;
        with Canvas do begin
          Pixels[0, 0] := clWindow;
          Pixels[1, 0] := clBtnFace;
          Pixels[2, 0] := clBtnShadow;
          Pixels[3, 0] := clWindow;
          Pixels[4, 0] := clBtnFace;
          Pixels[5, 0] := clBtnShadow;
        end;
      end;
    end;

    Anchors := [akLeft, akTop, akBottom];
  end;

  with TImage.Create(Self) do begin
    Parent  := Self;

    Left    := 2;
    Top     := 14;
    Height  := 1;
    Width   := 6;

    with Picture do begin
      Create;
      with Bitmap do begin
        Width   := 6;
        Height  := 1;
        with Canvas do begin
          Brush.Color := clBtnShadow;
          FillRect(ClipRect);
        end;
      end;
    end;
    Anchors := [akLeft, akBottom];
  end;
end;

procedure TVerticalCloseSeparator.SetOnCloseButtonClick(const Value: TNotifyEvent);
begin
  FCloseButton.OnClick := Value;
end;
{$ENDREGION 'TVerticalCloseSeparator'}

{$REGION 'TSBaseMainFormMDIInterface'}
{ TSBaseMainFormMDIInterface }

constructor TSBaseMainFormMDIInterface.Create(AOwner: TComponent);
var
  LdxBarCategory: Integer;
  I, LItemIndex: Integer;
  LBeginGroup: Boolean;
  LdxBarButton: TdxBarButton;
  LdxBarSubItem: TdxBarSubItem;
  LMainMenuWindows: TdxBarSubItem;
  LSkins: TStringList;
begin
  FBusy := False;
  inherited;

  FChilds  := TList<TForm>.Create;
  FButtons := TList<TdxBarButton>.Create;

  with TForm(AOwner) do begin
    FBarManager := TdxBarManager(FindComponent(SConst_BarManager));
    if not Assigned(FBarManager) then begin
      FBarManager := TdxBarManager.Create(AOwner);
      with FBarManager do begin
        Name              := SConst_BarManager;
        UseSystemFont     := True;
        LookAndFeel.NativeStyle := False;

        MenusShowRecentItemsFirst := False;
        ShowFullMenusAfterDelay   := False;
      end;
    end;

    FBarManagerShowToolbarsPopup := FBarManager.OnShowToolbarsPopup;
    FBarManager.OnShowToolbarsPopup := BarManagerShowToolbarsPopup;
    FBarManager.Images := SBaseFormProperties.Icon16List;

    FTaskBar := FBarManager.Bars.Add;
    with FTaskBar do begin
      Name                  := 'TaskBar';
      Visible               := False;
      Hidden                := True;
      MultiLine             := False;

      Caption               := 'Панель задач';

      AllowClose            := False;
      AllowCustomizing      := False;
      AllowQuickCustomizing := False;
      AllowReset            := False;

      DockingStyle          := dsBottom;
      DockedDockingStyle    := dsBottom;
//      DockedLeft            := 0;
//      DockedTop             := 0;

//      FloatLeft             := 1000;
//      FloatTop              := 601;
//      FloatClientWidth      := 23;
//      FloatClientHeight     := 22;

      NotDocking            := [dsNone, dsLeft, dsTop, dsRight, dsBottom];

      OneOnRow              := True;
      UseOwnFont            := False;
      UseRestSpace          := True;
      WholeRow              := False;

      Store                 := False;
    end;

    LdxBarCategory := FBarManager.Categories.IndexOf(SBaseProperties.Text_Windows);
    if LdxBarCategory = -1 then
      LdxBarCategory := FBarManager.Categories.Add(SBaseProperties.Text_Windows);

    LMainMenuWindows := TdxBarSubItem.Create(AOwner);
    with LMainMenuWindows do begin
      Name        := 'MainMenuWindows';
      Caption     := SBaseProperties.Text_Windows;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
    end;

    LdxBarButton := TdxBarButton.Create(FBarManager);
    with LdxBarButton do begin
      Name        := 'WindowsCascade';
      Caption     := SBaseProperties.Text_MenuWindowsCascade;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_WindowsCascade;
      OnClick     := WindowsCascadeClick;
    end;
    LMainMenuWindows.ItemLinks.Add.Item := LdxBarButton;

    LdxBarButton := TdxBarButton.Create(AOwner);
    with LdxBarButton do begin
      Name        := 'WindowsTileHor';
      Caption     := SBaseProperties.Text_MenuWindowsTileHor;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_WindowsTileHor;
      OnClick     := WindowsTileHorClick;
    end;
    LMainMenuWindows.ItemLinks.Add.Item := LdxBarButton;

    LdxBarButton := TdxBarButton.Create(AOwner);
    with LdxBarButton do begin
      Name        := 'WindowsTileVert';
      Caption     := SBaseProperties.Text_MenuWindowsTileVert;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_WindowsTileVert;
      OnClick     := WindowsTileVertClick;
    end;
    LMainMenuWindows.ItemLinks.Add.Item := LdxBarButton;

    LdxBarButton := TdxBarButton.Create(AOwner);
    with LdxBarButton do begin
      Name        := 'WindowsMinimize';
      Caption     := SBaseProperties.Text_MenuWindowsMinimize;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_WindowsMinimize;
      OnClick     := WindowsMinimizeClick;
    end;
    LMainMenuWindows.ItemLinks.Add.Item := LdxBarButton;

    LdxBarButton := TdxBarButton.Create(AOwner);
    with LdxBarButton do begin
      Name        := 'WindowsArrange';
      Caption     := SBaseProperties.Text_MenuWindowsArrange;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_WindowsArrange;
      OnClick     := WindowsArrangeClick;
    end;
    LMainMenuWindows.ItemLinks.Add.Item := LdxBarButton;

    FdxDefaultStyle := True;
    FdxSkinController := TdxSkinController.Create(AOwner);
    with FdxSkinController do begin
      Name        := 'dxSkinController';
//      SkinName    := SConst_Default;
      UseSkins    := False;
      Kind        := lfFlat;
      NativeStyle := True;
    end;

    if TStyleManager.Enabled then begin
      FdxBarWindowsStylesSubItem := TdxBarSubItem.Create(AOwner);
      with FdxBarWindowsStylesSubItem do begin
        Name        := 'dxBarWindowsStylesSubItem';
        Caption     := SBaseProperties.Text_WindowsStyles;
        Category    := LdxBarCategory;
        Visible     := ivNotInCustomizing;
      end;

      FdxBarWindowsStylesListItem := TdxMDBarListItem.Create(AOwner);
      with FdxBarWindowsStylesListItem do begin
        Name        := 'dxBarWindowsStylesListItem';
        Category    := LdxBarCategory;
        Visible     := ivNotInCustomizing;
        ShowCheck   := True;
        OnClick     := WindowsStyleButtonClick;
      end;

      LItemIndex := -1;
      for I := 0 to Length(TStyleManager.StyleNames) - 1 do begin
        if TStyleManager.StyleNames[I] = TStyleManager.ActiveStyle.Name then LItemIndex := I;
        FdxBarWindowsStylesListItem.Items.Add(TStyleManager.StyleNames[I]);
      end;
      if LItemIndex >= 0 then
        FdxBarWindowsStylesListItem.ItemIndex := LItemIndex;

      FdxBarWindowsStylesSubItem.ItemLinks.Add.Item := FdxBarWindowsStylesListItem;
    end;

    FdxBarDevExpressSkinsSubItem := TdxBarSubItem.Create(AOwner);
    with FdxBarDevExpressSkinsSubItem do begin
      Name        := 'dxBarDevExpressSkinsSubItem';
      Caption     := SBaseProperties.Text_DevExpressSkins;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
//      ImageIndex  := SBaseFormProperties.IconIndex_Excel;
//      OnClick     := ToolButtonClick;
    end;

    LdxBarButton := TdxBarButton.Create(FBarManager);
    with LdxBarButton do begin
      ButtonStyle := bsChecked;
      GroupIndex  := 2;
      Down        := True;
      Caption     := SBaseProperties.Text_SkinDefault;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      OnClick     := dxSkinDefaultButtonClick;
    end;
    FdxBarDevExpressSkinsSubItem.ItemLinks.Add.Item := LdxBarButton;

    for I := 0 to cxLookAndFeelPaintersManager.Count - 1 do
      if TcxCustomLookAndFeelPainter(cxLookAndFeelPaintersManager.Items[I]).LookAndFeelStyle <> lfsSkin then
      begin
        LdxBarButton := TdxBarButton.Create(FBarManager);
        with LdxBarButton do begin
          ButtonStyle := bsChecked;
          GroupIndex  := 2;
          Caption     := cxLookAndFeelPaintersManager.Items[I].LookAndFeelName;
          Category    := LdxBarCategory;
          Visible     := ivNotInCustomizing;
          OnClick     := dxStyleButtonClick;
        end;
        with FdxBarDevExpressSkinsSubItem.ItemLinks.Add do begin
          BeginGroup := (I = 0);
          Item := LdxBarButton;
        end;
      end;

    LBeginGroup := True;
    LSkins := TStringList.Create;
    try
      for I := 0 to SBaseFormProperties.FSkinFiles.Count - 1 do begin
        try dxSkinsUserSkinPopulateSkinNames(FinalizeFilePath(SBaseFormProperties.FSkinFiles.Strings[I]), LSkins, False) except end;
        while LSkins.Count > 0 do begin
          LdxBarButton := TdxBarButton.Create(FBarManager);
          with LdxBarButton do begin
            ButtonStyle := bsChecked;
            GroupIndex  := 2;
            Caption     := LSkins.Strings[0];
            Category    := LdxBarCategory;
            Visible     := ivNotInCustomizing;
            OnClick     := dxSkinButtonClick;
            Tag         := I;
          end;
          with FdxBarDevExpressSkinsSubItem.ItemLinks.Add do begin
            BeginGroup := LBeginGroup; LBeginGroup := False;
            Item := LdxBarButton;
          end;
          LSkins.Delete(0);
        end;
      end;


      if Assigned(FdxBarWindowsStylesSubItem) then
        with LMainMenuWindows.ItemLinks.Add do begin
          BeginGroup := True;
          Item := FdxBarWindowsStylesSubItem;
        end;

      with LMainMenuWindows.ItemLinks.Add do begin
        BeginGroup := not Assigned(FdxBarWindowsStylesSubItem);
        Item := FdxBarDevExpressSkinsSubItem;
      end;
    finally
      LSkins.Free;
    end;

    FWinRestore := TdxBarButton.Create(FBarManager);
    with FWinRestore do begin
      Caption     := SBaseProperties.Text_MenuRestore;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_WindowRestore;
      OnClick     := WindowRestoreClick;
    end;

    FWinMinimize := TdxBarButton.Create(FBarManager);
    with FWinMinimize do begin
      Caption     := SBaseProperties.Text_MenuMinimize;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_WindowMinimize;
      OnClick     := WindowMinimizeClick;
    end;

    FWinMaximize := TdxBarButton.Create(FBarManager);
    with FWinMaximize do begin
      Caption     := SBaseProperties.Text_MenuMaximize;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_WindowMaximize;
      OnClick     := WindowMaximizeClick;
    end;

    FWinClose := TdxBarButton.Create(FBarManager);
    with FWinClose do begin
      Caption     := SBaseProperties.Text_MenuClose;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_WindowClose;
      OnClick     := WindowCloseClick;
    end;

    LdxBarCategory := FBarManager.Categories.IndexOf(SBaseProperties.Text_Actions);
    if LdxBarCategory = -1 then
      LdxBarCategory := FBarManager.Categories.Add(SBaseProperties.Text_Actions);

    FWinPrev := TdxBarButton.Create(FBarManager);
    with FWinPrev do begin
      Caption     := SBaseProperties.Text_MenuWindowPrev;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_WindowPrev;
      OnClick     := ToolButtonWindowClick;
      Enabled     := False;
    end;

    FWinNext := TdxBarButton.Create(FBarManager);
    with FWinNext do begin
      Caption     := SBaseProperties.Text_MenuWindowNext;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_WindowNext;
      OnClick     := ToolButtonWindowClick;
      Enabled     := False;
    end;

    FCut := TdxBarButton.Create(FBarManager);
    with FCut do begin
      Caption     := SBaseProperties.Text_MenuCut;
      Tag         := SBaseForm.WM_CUT;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_Cut;
      OnClick     := ToolButtonClick;
      Enabled     := False;
    end;

    FCopy := TdxBarButton.Create(FBarManager);
    with FCopy do begin
      Caption     := SBaseProperties.Text_MenuCopy;
      Tag         := SBaseForm.WM_COPY;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_Copy;
      OnClick     := ToolButtonClick;
      Enabled     := False;
    end;

    FPaste := TdxBarButton.Create(FBarManager);
    with FPaste do begin
      Caption     := SBaseProperties.Text_MenuPaste;
      Tag         := SBaseForm.WM_PASTE;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_Paste;
      OnClick     := ToolButtonClick;
      Enabled     := False;
    end;

    FRefresh := TdxBarButton.Create(FBarManager);
    with FRefresh do begin
      Caption     := SBaseProperties.Text_MenuRefresh;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_Refresh;
      OnClick     := ToolButtonRefreshClick;
      Enabled     := False;
    end;

    FProperties := TdxBarButton.Create(FBarManager);
    with FProperties do begin
      Caption     := SBaseProperties.Text_MenuProperties;
      Tag         := SBaseForm.WM_PROPERTIES;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_Properties;
      OnClick     := ToolButtonClick;
      Enabled     := False;
    end;

    FObjectInspector := TdxBarButton.Create(FBarManager);
    with FObjectInspector do begin
      Caption     := SBaseProperties.Text_MenuObjectInspector;
      Tag         := SBaseForm.WM_OBJECTINSPECTOR;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_ObjectInspector;
      OnClick     := ToolButtonClick;
      Enabled     := False;
    end;

    FDatesPeriod := TdxBarButton.Create(FBarManager);
    with FDatesPeriod do begin
      Caption     := SBaseProperties.Text_MenuDatesPeriod;
      Tag         := SBaseForm.WM_DATESPERIOD;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_DatesPeriod;
      OnClick     := ToolButtonClick;
      Enabled     := False;
    end;

    FFilter := TdxBarButton.Create(FBarManager);
    with FFilter do begin
      Caption     := SBaseProperties.Text_MenuFilterBox;
      Tag         := SBaseForm.WM_FILTER;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_FilterBox;
      OnClick     := ToolButtonClick;
      Enabled     := False;
    end;

    FPrint := TdxBarButton.Create(FBarManager);
    with FPrint do begin
      Caption     := SBaseProperties.Text_MenuPrint;
      Tag         := SBaseForm.WM_PRINT;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_Printer;
      OnClick     := ToolButtonClick;
      Enabled     := False;
    end;

    FExcel := TdxBarButton.Create(FBarManager);
    with FExcel do begin
      Caption     := SBaseProperties.Text_MenuExportTo + ' Excel';
      Tag         := SBaseForm.WM_EXCEL;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_Excel;
      OnClick     := ToolButtonClick;
      Enabled     := False;
    end;

    with FBarManager.Bars.Add do begin
      Name                  := 'ToolBar';
      Hidden                := True;
      Visible               := False;
      MultiLine             := False;

      Caption               := SBaseProperties.Text_ToolBar;

      AllowClose            := False;
      AllowCustomizing      := False;
      AllowQuickCustomizing := False;
      AllowReset            := False;

      DockedDockingStyle    := dsTop;
      DockedTop             := 45;
      DockingStyle          := dsTop;
      NotDocking            := [dsNone, dsLeft, dsTop, dsRight, dsBottom];


      OneOnRow              := True;
//      UseOwnFont = False
      UseRestSpace          := True;
//      WholeRow              := True;
      Store                 := False;

//      LockUpdate := True;

      ItemLinks.Add.Item := FWinPrev;
      ItemLinks.Add.Item := FWinNext;

      with ItemLinks.Add do begin
        Item        := FCut;
        BeginGroup  := True;
      end;
      ItemLinks.Add.Item := FCopy;
      ItemLinks.Add.Item := FPaste;

      with ItemLinks.Add do begin
        Item        := FRefresh;
        BeginGroup  := True;
      end;

      with ItemLinks.Add do begin
        Item        := FDatesPeriod;
        BeginGroup  := True;
      end;
      ItemLinks.Add.Item := FFilter;
      ItemLinks.Add.Item := FProperties;
      ItemLinks.Add.Item := FObjectInspector;

      with ItemLinks.Add do begin
        Item        := FPrint;
        BeginGroup  := True;
      end;
      ItemLinks.Add.Item := FExcel;

//      LockUpdate := False;
      Visible               := True;
    end;

    FLogPanel     := TPanel.Create(Self);
    with FLogPanel do begin
      Visible     := False;
      Caption     := '';
      Align       := alBottom;
      Height      := 88;
      BevelInner  := bvLowered;
      BevelOuter  := bvNone;
      Parent      := TForm(AOwner);
//      OnResize   := pLogResize
    end;

    with TVerticalCloseSeparator.Create(FLogPanel) do begin
      Parent  := FLogPanel;
      Align   := alLeft;
      OnCloseButtonClick := LogPanelCloseClick;
    end;


{
    FlgCopyLine, FlgCopyAll, FlgDeleteLine, FlgDeleteAll: TdxBarButton;
}
    FlgCopyLine := TdxBarButton.Create(FBarManager);
    with FlgCopyLine do begin
      Caption     := SBaseProperties.Text_MenuCopy;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_Copy;
      OnClick     := LogCopyLineClick;
      Enabled     := False;
    end;

    FlgCopyAll := TdxBarButton.Create(FBarManager);
    with FlgCopyAll do begin
      Caption     := SBaseProperties.Text_MenuCopyAll;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
//      ImageIndex  := IconIndex_Copy;
      OnClick     := LogCopyAllClick;
      Enabled     := False;
    end;

    FlgDeleteLine := TdxBarButton.Create(FBarManager);
    with FlgDeleteLine do begin
      Caption     := SBaseProperties.Text_MenuDelete;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
      ImageIndex  := SBaseFormProperties.IconIndex_Delete;
      OnClick     := LogDeleteLineClick;
      Enabled     := False;
    end;

    FlgDeleteAll := TdxBarButton.Create(FBarManager);
    with FlgDeleteAll do begin
      Caption     := SBaseProperties.Text_MenuDeleteAll;
      Category    := LdxBarCategory;
      Visible     := ivNotInCustomizing;
//      ImageIndex  := IconIndex_Copy;
      OnClick     := LogDeleteAllClick;
      Enabled     := False;
    end;

    FLogGrid := TcxGrid.Create(AOwner);
    with FLogGrid do begin
      BeginUpdate;

      PopupMenu := TdxBarPopupMenu.Create(Self);
      with TdxBarPopupMenu(PopupMenu) do begin
        BarManager := FBarManager;

        ItemLinks.Add.Item := FlgCopyLine;
        ItemLinks.Add.Item := FlgCopyAll;
        with ItemLinks.Add do begin
          BeginGroup  := True;
          Item        := FlgDeleteLine;
        end;
        ItemLinks.Add.Item := FlgDeleteAll;
        OnPopup := LogPopupMenuOnPopup;
      end;

      Left              := 13;
      Top               := 0;
      Width             := FLogPanel.ClientWidth - 14;
      Height            := FLogPanel.ClientHeight;
      Align             := alCustom;
      Anchors           := [akLeft, akTop, akRight, akBottom];

      LookAndFeel.NativeStyle := True;

      FLogGridView := TcxGridTableView.Create(CreateView(TcxGridTableView));
      with FLogGridView do begin
        SetParentComponent(FLogGrid);

        OptionsBehavior.CellHints := True;
        OptionsBehavior.ImmediateEditor := False;

        OptionsCustomize.ColumnFiltering := False;
        OptionsCustomize.ColumnGrouping := False;
        OptionsCustomize.ColumnHorzSizing := False;
        OptionsCustomize.ColumnMoving := False;
        OptionsCustomize.ColumnSorting := False;

        OptionsData.Deleting := False;
        OptionsData.Editing := False;
        OptionsData.Inserting := False;

        OptionsSelection.CellSelect := False;
        OptionsSelection.HideFocusRectOnExit := True;
        OptionsSelection.UnselectFocusedRecordOnExit := False;

        OptionsView.Header := False;
        OptionsView.GroupByBox := False;
        OptionsView.GridLines := glNone;
        OptionsView.ScrollBars := ssVertical;
        OptionsView.CellEndEllipsis := True;
        OptionsView.ColumnAutoWidth := True;

        with CreateColumn do begin
          DataBinding.ValueType := DelphiTypeString;
          Width := 20;

          PropertiesClass := TcxImageComboBoxProperties;
          with TcxImageComboBoxProperties(Properties) do begin
            Images := SBaseFormProperties.Icon16List;
            with Items.Add do begin
              ImageIndex := SBaseFormProperties.IconIndex_MsgInfo;
              Value := 'I';
            end;
            with Items.Add do begin
              ImageIndex := SBaseFormProperties.IconIndex_MsgWarning;
              Value := 'W';
            end;
            with Items.Add do begin
              ImageIndex := SBaseFormProperties.IconIndex_MsgError;
              Value := 'E';
            end;
          end;
        end;

        with CreateColumn do begin
          DataBinding.ValueType := DelphiTypeString;
          Width := 2000;
        end;
      end;

      with Levels.Add do begin
        GridView := FLogGridView;
      end;

      Parent            := FLogPanel;
      EndUpdate;
    end;

    FLogSplitter  := TcxSplitter.Create(Self);
    with FLogSplitter do begin
      Visible       := False;
      AlignSplitter := salBottom;
      Cursor        := crVSplit;
      Height        := 2;
      //Align         := alBottom;
      //NativeBackground := False;
      //Color         := clBtnFace;
      ParentColor   := False;
      Parent        := TForm(AOwner);
      Control       := FLogPanel;
    end;
  end;
end;

destructor TSBaseMainFormMDIInterface.Destroy;
var
  LBarManagerShowToolbarsPopup: TdxBarShowPopupEvent;
begin
  LBarManagerShowToolbarsPopup := BarManagerShowToolbarsPopup;
  if TMethod(LBarManagerShowToolbarsPopup) = TMethod(FBarManager.OnShowToolbarsPopup) then
    FBarManager.OnShowToolbarsPopup := FBarManagerShowToolbarsPopup;

  FreeAndNil(FChilds);
  FreeAndNil(FButtons);

  inherited;
end;

function TSBaseMainFormMDIInterface.GetSkinButtons(AName: String): TdxBarButton;
var
  I: Integer;
begin
  if SameText(AName, SBaseProperties.Text_SkinDefault) then
    Result := TdxBarButton(FdxBarDevExpressSkinsSubItem.ItemLinks[0].Item)
  else begin
    for I := 1 to FdxBarDevExpressSkinsSubItem.ItemLinks.Count - 1 do
      if SameText(FdxBarDevExpressSkinsSubItem.ItemLinks[0].Item.Caption, AName) then begin
        Result := TdxBarButton(FdxBarDevExpressSkinsSubItem.ItemLinks[0].Item);
        Exit;
      end;
    Result := nil;
  end;
end;

procedure TSBaseMainFormMDIInterface.BroadCast(Msg, Lo, Hi: Integer);
var
  I: Integer;
begin
  if Application.MainForm <> nil then
    for I := Application.MainForm.MDIChildCount - 1 downto 0 do
      SendMessage(Application.MainForm.MDIChildren[I].Handle, Msg, Lo, Hi);
end;

procedure TSBaseMainFormMDIInterface.BroadCastDataChanges(const ATablesChanges: TTablesChanges; AExternalEvent: Boolean; AExcludeForm: TSBaseFrm);
var
  I: Integer;

  procedure ProcessForm(AForm: TCustomForm);
  begin
    if (AForm <> AExcludeForm) and (AForm is TSBaseFrm) then
      TSBaseFrm(AForm).AfterDataChanged(ATablesChanges);
  end;
begin
  if FChangeNotificationsCach.Accept(ATablesChanges.Notify) then begin
    for I := 0 to SBaseMainFormMDIInterface.Childs.Count - 1 do
      ProcessForm(TForm(SBaseMainFormMDIInterface.Childs[I]));

    if Application.ModalLevel > 0 then begin
      ProcessForm(Screen.FocusedForm);
      for I := 0 to Screen.SaveFocusedList.Count - 1 do
        ProcessForm(TForm(Screen.SaveFocusedList[I]));
    end;
  end;
end;

procedure TSBaseMainFormMDIInterface.RegisterWindow(AForm: TForm);
var
  Btn: TdxBarButton;
begin
  if (Self <> nil) and not (csDestroying in Owner.ComponentState) then begin
    if FChilds.IndexOf(AForm) <> -1 then Exit;

    Btn := TdxBarButton.Create(FBarManager);
    Btn.Caption := AForm.Caption;
    Btn.Hint := AForm.Caption;
    Btn.Glyph.Width := AForm.Icon.Width;
    Btn.Glyph.Height:= AForm.Icon.Height;
    Btn.Glyph.Canvas.Draw(0, 0, AForm.Icon);
    Btn.PaintStyle := psCaptionGlyph;
    Btn.ButtonStyle := bsChecked;
    Btn.GroupIndex := 1;

    Btn.OnClick := WindowButtonClick;

    FTaskBar.LockUpdate := True;
    try
      with FTaskBar.ItemLinks.Add do begin
        Item       := Btn;
        BeginGroup := True;
      end;
    finally
      FTaskBar.LockUpdate := False;
    end;


    FChilds.Add(AForm);
    FButtons.Add(Btn);

    Refresh;
    AForm.OnActivate(AForm);
  end;
end;

procedure TSBaseMainFormMDIInterface.WindowOnDestroy(AForm: TForm);
var
  Index: Integer;
begin
  if (Self <> nil) and not (csDestroying in Owner.ComponentState) then begin
    Index := FChilds.IndexOf(AForm);
    if Index >= 0 then begin
      if AForm.Active then begin

        FWinPrev.Enabled    := False;
        FWinNext.Enabled    := False;

        FCut.Enabled        := False;
        FCopy.Enabled       := False;
        FPaste.Enabled      := False;

        FRefresh.Enabled    := False;

        FDatesPeriod.Enabled      := False;
        FFilter.Enabled           := False;
        FFilter.Down              := False;
        FProperties.Enabled       := False;
        FObjectInspector.Enabled  := False;

        FPrint.Enabled      := False;
        FExcel.Enabled      := False;
      end;

      FChilds.Delete(Index);
      FButtons[Index].Destroy;
      FButtons.Delete(Index);

      Refresh;
    end;
  end;
end;

procedure TSBaseMainFormMDIInterface.WindowOnActivate(AForm: TForm; Atbs: TMDIToolButtons);
var
  Index: Integer;
begin
  if (Self <> nil) and not (csDestroying in Owner.ComponentState) then begin
    Index := FChilds.IndexOf(AForm);
    if Index >= 0 then begin
      FButtons[Index].Down := True;

      FWinPrev.Enabled    := FChilds.Count > 1;
      FWinNext.Enabled    := FWinPrev.Enabled;

      FCut.Enabled        := mtbCut in Atbs;
      FCopy.Enabled       := mtbCopy in Atbs;
      FPaste.Enabled      := mtbPaste in Atbs;

      FRefresh.Enabled    := mtbRefresh in Atbs;

      FDatesPeriod.Enabled:= mtbDatesPeriod in Atbs;
      FFilter.Enabled     := mtbFilter in Atbs;
      FProperties.Enabled := mtbProperties in Atbs;
      FObjectInspector.Enabled := True;

      FPrint.Enabled      := mtbPrint in Atbs;
      FExcel.Enabled      := mtbExcel in Atbs;

      Refresh;
    end;
  end;
end;

procedure TSBaseMainFormMDIInterface.SendToActiveWindow(AMsg, ALo, AHi: Integer);
var
  LForm: TForm;
begin
  LForm := ActiveForm;
  if Assigned(LForm) then
    PostMessage(LForm.Handle, AMsg, ALo, AHi);
end;


procedure TSBaseMainFormMDIInterface.SetActiveSkin(const Value: String);
var
  I: Integer;
begin
  for I := 0 to FdxBarDevExpressSkinsSubItem.ItemLinks.Count - 1 do
    if SameText(FdxBarDevExpressSkinsSubItem.ItemLinks[I].Item.Caption, Value) then begin
      if not TdxBarButton(FdxBarDevExpressSkinsSubItem.ItemLinks[I].Item).Down then
        FdxBarDevExpressSkinsSubItem.ItemLinks[I].Item.Click;
      Exit;
    end;
end;

procedure TSBaseMainFormMDIInterface.SetActiveStyle(const Value: String);
var
  I: Integer;
begin
  if Assigned(FdxBarWindowsStylesListItem) then
    for I := 0 to FdxBarWindowsStylesListItem.Items.Count - 1 do
      if FdxBarWindowsStylesListItem.Items[I] = Value then begin
        FdxBarWindowsStylesListItem.ItemIndex := I;
        WindowsStyleButtonClick(FdxBarWindowsStylesListItem);
        Exit;
      end;
end;

procedure TSBaseMainFormMDIInterface.WindowUpdated(AForm: TForm);
var
  B: TdxBarButton;
  Idx: Integer;
begin
  if (Self <> nil) and not (csDestroying in Owner.ComponentState) then begin
    Idx := FChilds.IndexOf(AForm);
    if Idx >= 0 then begin
      B := FButtons[Idx];
      if Assigned(B) then begin
        B.Hint := AForm.Caption;
        B.Glyph.Assign(nil); // Clear image
        B.Glyph.Width  := AForm.Icon.Width;
        B.Glyph.Height := AForm.Icon.Height;
        B.Glyph.Canvas.Draw(0, 0, AForm.Icon);

        Refresh;
      end;
    end;
  end;
end;

procedure TSBaseMainFormMDIInterface.Refresh;
var
  I: Integer;
  BarButtonWidth: Integer;
  K, N: Integer;
  S, FCaption: String;
  IsDown: Boolean;
begin
  if FButtons.Count <= 0 then begin
    FTaskBar.Visible := False;
    Exit;
  end;

  if not FTaskBar.Visible then begin
    FTaskBar.Visible := True;
    if FLogPanel.Visible then begin
      FLogPanel.Top := 10;
      FLogSplitter.Top  := 1
    end;
//    if Assigned(FFormTask) then FFormTask.UpdatePos;
  end;

  BarButtonWidth := TForm(Owner).ClientWidth div Buttons.Count - 7;
  if BarButtonWidth > DefaultTaskButtonWidth then
    BarButtonWidth := DefaultTaskButtonWidth;

  FTaskBar.LockUpdate := True;
  IsDown := False;
  try
    for I := FButtons.Count - 1 downto 0 do begin
      K := BarButtonWidth - FButtons[I].Glyph.Width - 10;
      FCaption := FChilds[I].Caption;
      N := TForm(Owner).Canvas.TextWidth(FCaption);

      S := FCaption;
      if N > K then begin
      //-- если надпись больше допустимой  длины
      	while (N > K) and (S <> '') do begin
          N := TForm(Owner).Canvas.TextWidth(S + '...');
          Delete(S, Length(S), 1);
        end;
        if S <> FCaption then S := S + '...';
      end else begin
      //-- если надпись меньше допустимой  длины
      	while (N < K) do begin
          S := S + ' ';
          N := TForm(Owner).Canvas.TextWidth(S);
        end;
      end;

      FButtons[I].Caption := S;
      FButtons[I].Hint := FCaption;
      if not IsDown and FButtons[I].Down then IsDown := True;
    end;
  finally
    FTaskBar.LockUpdate := False;
  end;

  //-- Дополнительно проверяем нажатие одной из кнопок (иногда глючит)
  if not IsDown then FButtons.Last.Down := True;
end;

procedure TSBaseMainFormMDIInterface.WindowButtonClick(Sender: TObject);
var
  Index: Smallint;
begin
  if (Sender is TdxBarButton) then begin
    Index := FButtons.IndexOf(TdxBarButton(Sender));
    if Index >= 0 then begin
      with FChilds[Index] do
        if Active and (WindowState <> wsMinimized) then
          WindowState := wsMinimized
        else if WindowState = wsMinimized then
          WindowState := wsNormal
        else
          Show;
    end;
  end
end;

function TSBaseMainFormMDIInterface.GetActiveForm: TForm;
var
  I: Integer;
begin
  Result := nil;
  if Assigned(Self) and not (csDestroying in Owner.ComponentState) then begin
    for I := FChilds.Count - 1 downto 0 do
      if FButtons[I].Down then begin
        Result := FChilds[I];
        Break;
      end;
  end;
end;

function TSBaseMainFormMDIInterface.GetActiveSkin: String;
var
  I: Integer;
  LcxCustomLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := SBaseProperties.Text_SkinDefault;
  if FdxDefaultStyle then
    Exit
  else if FdxSkinController.UseSkins then
    Result := FdxSkinController.SkinName
  else begin
    for I := 1 to FdxBarDevExpressSkinsSubItem.ItemLinks.Count - 1 do
      if TdxBarButton(FdxBarDevExpressSkinsSubItem.ItemLinks[I].Item).Down then begin
        if not cxLookAndFeelPaintersManager.GetPainter(FdxBarDevExpressSkinsSubItem.ItemLinks[I].Item.Caption, LcxCustomLookAndFeelPainter) then Exit;
          Result := LcxCustomLookAndFeelPainter.LookAndFeelName;
        Exit;
      end;
  end;
end;

function TSBaseMainFormMDIInterface.GetActiveStyle: String;
begin
  if Assigned(FdxBarWindowsStylesListItem) and (FdxBarWindowsStylesListItem.ItemIndex >= 0) then
    Result := FdxBarWindowsStylesListItem.Items[FdxBarWindowsStylesListItem.ItemIndex]
  else
    Result := '';
end;

procedure TSBaseMainFormMDIInterface.BarManagerShowToolbarsPopup(Sender: TdxBarManager; PopupItemLinks: TdxBarItemLinks);
var
  Index: Integer;
  Form: TForm;

  P: TPoint;
  dxBarItemControl: TdxBarItemControl;
  dxItem: TdxBarItem;
begin
  if Sender.IsCustomizing then Exit;
  if not FTaskBar.Visible then Exit;

  GetCursorPos(P);
  P := FTaskBar.ItemLinks.BarControl.ScreenToClient(P);
  dxBarItemControl := FTaskBar.ItemLinks.BarControl.ItemAtPos(P);
  if dxBarItemControl = nil then Exit;

  dxItem := dxBarItemControl.Item;
  Index := FButtons.IndexOf(TdxBarButton(dxItem));

  if Index = -1 then Exit;

  if not TDxBarButton(dxItem).Down then begin
    TDxBarButton(dxItem).Down := True;
  end;

  Form := FChilds[Index];
  if Form = nil then Exit;

  FWinRestore.Enabled := Form.WindowState = wsMinimized;
  FWinMinimize.Enabled := Form.WindowState <> wsMinimized;
  FWinMaximize.Enabled := (Form.WindowState <> wsMaximized) and (Form.BorderStyle <> bsSingle) and (biMaximize in Form.BorderIcons);

  FWinRestore.Tag  := Integer(Form);
  FWinMaximize.Tag := Integer(Form);
  FWinMinimize.Tag := Integer(Form);
  FWinClose.Tag    := Integer(Form);

  PopupItemLinks.Clear;
  with PopupItemLinks.Add do Item := FWinRestore;
  with PopupItemLinks.Add do Item := FWinMaximize;
  with PopupItemLinks.Add do Item := FWinMinimize;
  with PopupItemLinks.Add do begin
    BeginGroup := True;
    Item := FWinClose;
  end;
end;

procedure TSBaseMainFormMDIInterface.WindowsCascadeClick(Sender: TObject);
begin
  TForm(Owner).Cascade
end;

procedure TSBaseMainFormMDIInterface.WindowsTileHorClick(Sender: TObject);
begin
  with TForm(Owner) do begin
    TileMode := tbHorizontal;
    Tile
  end;
end;

procedure TSBaseMainFormMDIInterface.WindowsTileVertClick(Sender: TObject);
begin
  with TForm(Owner) do begin
    TileMode := tbVertical;
    Tile
  end;
end;

procedure TSBaseMainFormMDIInterface.WindowsMinimizeClick(Sender: TObject);
var
  I: Integer;
begin
  for I := Pred(Childs.Count) downto 0 do
    TForm(Childs[i]).WindowState := wsMinimized;
end;

procedure TSBaseMainFormMDIInterface.WindowsStyleButtonClick(Sender: TObject);
begin
  TStyleManager.TrySetStyle(FdxBarWindowsStylesListItem.Items[FdxBarWindowsStylesListItem.ItemIndex], False);
end;

procedure TSBaseMainFormMDIInterface.WindowsArrangeClick(Sender: TObject);
begin
  TForm(Owner).ArrangeIcons
end;

procedure TSBaseMainFormMDIInterface.WindowMinimizeClick(Sender: TObject);
begin
  with TForm(TdxBarItem(Sender).Tag) do
    PostMessage(Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

procedure TSBaseMainFormMDIInterface.WindowRestoreClick(Sender: TObject);
begin
  with TForm(TdxBarItem(Sender).Tag) do
    PostMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;

procedure TSBaseMainFormMDIInterface.WindowMaximizeClick(Sender: TObject);
begin
  with TForm(TdxBarItem(Sender).Tag) do
    PostMessage(Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
end;

procedure TSBaseMainFormMDIInterface.WindowCloseClick(Sender: TObject);
begin
  with TForm(TdxBarItem(Sender).Tag) do
    PostMessage(Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
end;

function TSBaseMainFormMDIInterface.Log(ALogLevel: Char; AMessages: String; AProcessMessages: Boolean = False): Boolean;
var
  LSize, LLength: Word;
  LRecordCount: Integer;
  LBusy: Boolean;
begin
  if (Self = nil) or (csDestroying in ComponentState) or (csDestroying in Owner.ComponentState) then Exit(False);

  LLength := Length(AMessages);
  if (ALogLevel <> lgError) or (LLength = 0) or (AMessages <> SOperationAborted) then begin
    FLogGridView.BeginUpdate;
    try
      if not FLogPanel.Visible then begin
        LogDeleteAllClick(nil);

        FLogPanel.Top := 10;
        FLogPanel.Visible := True;

        FLogSplitter.Top := 1;
        FLogSplitter.Visible := True;
      end;

      while True do begin
        with FLogGridView.DataController do begin
          LRecordCount := RecordCount;
          while LRecordCount >= lgMaxLines do begin
            DeleteRecord(0);
            Dec(LRecordCount);
          end;
        end;

        LSize := 1;
        while (LLength > 0) and (AMessages[LSize] <> #13) do begin
          Inc(LSize);
          Dec(LLength)
        end;

        if LSize > 1 then with FLogGridView.DataController do begin
          RecordCount := LRecordCount + 1;
          SetValue(LRecordCount, 0, ALogLevel);
          SetValue(LRecordCount, 1, Copy(AMessages, 1, Pred(LSize)));

//          FLogGridView.Columns[0].EditValue := ALogLevel;
//          FLogGridView.Columns[1].EditValue := Copy(AMessages, 1, Pred(LSize));
        end;

        if (LLength > 0) then
          repeat
            Inc(LSize);
            Dec(LLength)
          until (LLength = 0) or (AMessages[LSize] <> #10);

        if LLength = 0 then
          Break;

        Delete(AMessages, 1, Pred(LSize));
      end;
    finally
      FLogGridView.EndUpdate;
      FLogGridView.ViewData.Records[FLogGridView.ViewData.RecordCount - 1].Focused := True;
    end;

    if AProcessMessages then begin
      LBusy := FBusy;
      try
        FBusy := True;

        Application.ProcessMessages;
      finally
        FBusy := LBusy;
      end;
    end;
  end;

  Exit(TForm(Owner).Visible);
end;

procedure TSBaseMainFormMDIInterface.ToolButtonClick(Sender: TObject);
begin
  SendToActiveWindow(TdxBarItem(Sender).Tag, 0, 0)
end;

procedure TSBaseMainFormMDIInterface.ToolButtonRefreshClick(Sender: TObject);
var
  LForm: TForm;
begin
  LForm := ActiveForm;
  if Assigned(LForm) and (LForm is TSBaseFrm) then
    TSBaseFrm(LForm).AfterDataChanged(TTablesChanges.Create(True));
end;

{$WARNINGS OFF}
procedure TSBaseMainFormMDIInterface.dxSkinDefaultButtonClick(Sender: TObject);
var
  LWindowState: TWindowState;
begin
  FdxDefaultStyle := True;
  if Assigned(Application.MainForm) then begin
    LWindowState := Application.MainForm.WindowState;
    Application.MainForm.WindowState := wsMinimized;
  end;
  try
    FdxSkinController.BeginUpdate;
    try
      FdxSkinController.Kind        := lfFlat;
      FdxSkinController.UseSkins    := False;
      FdxSkinController.NativeStyle := True;
    finally
      FdxSkinController.EndUpdate;
    end;
    SkinChangedNotify;
  finally
    if Assigned(Application.MainForm) then
      Application.MainForm.WindowState := LWindowState;
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSBaseMainFormMDIInterface.dxStyleButtonClick(Sender: TObject);
var
  LWindowState: TWindowState;
  LcxCustomLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  if cxLookAndFeelPaintersManager.GetPainter(TdxBarButton(Sender).Caption, LcxCustomLookAndFeelPainter)
    and (LcxCustomLookAndFeelPainter.LookAndFeelStyle <> lfsSkin)
  then begin
    FdxDefaultStyle := False;

    if Assigned(Application.MainForm) then begin
      LWindowState := Application.MainForm.WindowState;
      Application.MainForm.WindowState := wsMinimized;
    end;
    try
      FdxSkinController.BeginUpdate;
      try
        case LcxCustomLookAndFeelPainter.LookAndFeelStyle of
          lfsFlat:
            FdxSkinController.Kind := lfFlat;
          lfsStandard:
            FdxSkinController.Kind := lfStandard;
          lfsUltraFlat:
            FdxSkinController.Kind := lfUltraFlat;
          lfsOffice11:
            FdxSkinController.Kind := lfOffice11;
        end;
        FdxSkinController.NativeStyle := (LcxCustomLookAndFeelPainter.LookAndFeelStyle = lfsNative);
        FdxSkinController.UseSkins := False;
      finally
        FdxSkinController.EndUpdate;
      end;
      SkinChangedNotify;
    finally
      if Assigned(Application.MainForm) then
        Application.MainForm.WindowState := LWindowState;
    end;
  end;
end;
{$WARNINGS ON}

{$WARNINGS OFF}
procedure TSBaseMainFormMDIInterface.dxSkinButtonClick(Sender: TObject);
var
  LWindowState: TWindowState;
begin
  FdxDefaultStyle := False;

  if Assigned(Application.MainForm) then begin
    LWindowState := Application.MainForm.WindowState;
    Application.MainForm.WindowState := wsMinimized;
  end;
  try
    FdxSkinController.BeginUpdate;
    try
      if cxLookAndFeelPaintersManager.GetPainter(TdxBarButton(Sender).Caption) = nil then
        dxSkinsUserSkinLoadFromFile(FinalizeFilePath(SBaseFormProperties.FSkinFiles[TdxBarButton(Sender).Tag]), TdxBarButton(Sender).Caption);
      FdxSkinController.SkinName := sdxSkinsUserSkinName;
      FdxSkinController.UseSkins := True;
      FdxSkinController.NativeStyle := False;
    finally
      FdxSkinController.EndUpdate;
    end;
    SkinChangedNotify;
  finally
    if Assigned(Application.MainForm) then
      Application.MainForm.WindowState := LWindowState;
  end;
end;
{$WARNINGS ON}

procedure TSBaseMainFormMDIInterface.SkinChangedNotify;
var
  I: Integer;
begin
//  LockWindowUpdate(TForm(Owner).Handle);
//  TSBaseFrm(SBaseMainFormMDIInterface.Owner).Perform(WM_SETREDRAW, 0, 0);

//  TSBaseFrm(SBaseMainFormMDIInterface.Owner).

//  try
//    dxSkinController.Refresh;

    for I := 0 to SBaseMainFormMDIInterface.Childs.Count - 1 do
      if TComponent(SBaseMainFormMDIInterface.Childs[I]) is TSBaseFrm then begin
        TSBaseFrm(SBaseMainFormMDIInterface.Childs[I]).AfterSkinChanged;
      end;

    TSBaseFrm(SBaseMainFormMDIInterface.Owner).AfterSkinChanged;

//  finally
//    TSBaseFrm(SBaseMainFormMDIInterface.Owner).Perform(WM_SETREDRAW, 1, 0);
//    TSBaseFrm(SBaseMainFormMDIInterface.Owner).Refresh;
//    LockWindowUpdate(0);
//  end;
end;

procedure TSBaseMainFormMDIInterface.ToolButtonWindowClick(Sender: TObject);
var
  i: Integer;
  Index: Integer;
begin
  if Sender = FWinPrev then
    Index := -1
  else
    Index := 1;

  if FButtons.Count > 1 then
    for I := FButtons.Count - 1 downto 0 do
      if FButtons[I].Down then begin
        FButtons[(I + Index + FButtons.Count) mod FButtons.Count].Click;
        Break;
      end;
end;

procedure TSBaseMainFormMDIInterface.LogPanelCloseClick(Sender: TObject);
begin
  FLogSplitter.Visible  := False;
  FLogPanel.Visible     := False;
  LogDeleteAllClick(nil);
end;

procedure TSBaseMainFormMDIInterface.LogCopyLineClick(Sender: TObject);
begin
  ClipBoard.AsText := FLogGridView.Columns[1].EditValue;
end;

procedure TSBaseMainFormMDIInterface.LogCopyAllClick(Sender: TObject);
var
  LText: String;
  LRecordIndex: Integer;
begin
  LText := '';
  with FLogGridView.DataController do
    for LRecordIndex := 0 to RecordCount - 1 do
      LText := ConcatIfNotEmpty(LText, SConst_CR, GetValue(LRecordIndex, 1));

  ClipBoard.AsText := LText
end;

procedure TSBaseMainFormMDIInterface.LogDeleteLineClick(Sender: TObject);
begin
  FLogGridView.DataController.DeleteFocused
end;

procedure TSBaseMainFormMDIInterface.LogDeleteAllClick(Sender: TObject);
begin
  FLogGridView.BeginUpdate;
  try
    with FLogGridView.DataController do
      while RecordCount > 0 do
        DeleteRecord(0);
  finally
    FLogGridView.EndUpdate
  end;
end;

procedure TSBaseMainFormMDIInterface.LogPopupMenuOnPopup(Sender: TObject);
begin
  FlgCopyLine.Enabled   := (FLogGridView.DataController.FilteredRecordCount > 0);
  FlgCopyAll.Enabled    := FlgCopyLine.Enabled;

  FlgDeleteLine.Enabled := FlgCopyLine.Enabled;
  FlgDeleteAll.Enabled  := FlgCopyLine.Enabled;
end;

{$IFDEF SERVER_UPDATE_NOTIFICATIONS}
procedure TSBaseMainFormMDIInterface.NotificationMessage(Sender: TSBaseNotificationTrap; Message: TSBaseEventMessage);
const
  PROCESS_TERMINATE = $0001;
  APPLICATION_TERMINATE_EXIT_CODE = 666;
var
  LHandle: THandle;
  LProcessId: Integer;
  LNotifyObject: Integer;
begin
  LNotifyObject := VarToIntDef(Message.Props[SConst_Object_Id], 0);

  if (LNotifyObject = SBaseNotificationProperties.Object_Terminate) then begin
    if
    (
      not VarIsPresent(Message.Params[SConst_Application])
      or
      SameText(Message.Params[SConst_Application], ExtractFileName(ApplicationExeName))
    ) then
    begin
      GetWindowThreadProcessID(TForm(Owner).Handle, @LProcessId);
      LHandle := OpenProcess(PROCESS_TERMINATE, False, LProcessId);
      TerminateProcess(LHandle, APPLICATION_TERMINATE_EXIT_CODE);
    end;
  end else if (LNotifyObject = SBaseNotificationProperties.Object_DataChanged) then
    BroadCastDataChanges(TTablesChanges.Create(Message.Params), True);
end;
{$ENDIF}

procedure TSBaseMainFormMDIInterface.RestoreWindows;
var
  LIdentity: Integer;
  FClassName: String;
//  FPersistentClass: TPersistentClass;
  FUserVariables: TSBaseFrmUserVariables;
  FVariables: TNamedVariants;
  LClassName: String;
  LVersion  : Byte;
begin
  if Owner is TSBaseFrm then begin
    LClassName := TSBaseFrm(Owner).InternalClassName;
    LVersion   := TSBaseFrm(Owner).Version;
  end else begin
    LClassName := TForm(Owner).ClassName;
    LVersion   := 0;
  end;

  try
    LIdentity  := 1;
    FVariables.UnknownAsNull := True;
    while True do
    begin
      FUserVariables := SBaseGetUserClassVariablesFunction(LClassName, LVersion, SConst_Window + ':' + IntToStr(LIdentity));

      if (FUserVariables.Count = 0) then
        Break
      else try
        FClassName := VarToStr(FUserVariables.ExtractVariable(SConst_Window));
        if (not FClassName.IsEmpty) then begin
          FVariables.Clear;
          while FUserVariables.Count > 0 do
            with FUserVariables.ExtractVariable do
              if Version = 0 then
                FVariables.SetValue(Name, Value);
          TSBaseFrm(SBaseFormDefinitions.ICreateByParams(FClassName, Owner, FVariables)).ShowMDI;
        end;

      except
      end;

      Inc(LIdentity);
    end;
  except
  end;
end;

procedure TSBaseMainFormMDIInterface.SaveWindows;
var
  I, K, LIdentity: Integer;
  LFormRestoreParams: TNamedVariants;
  LClassName: String;
  LVersion  : Byte;
begin
  if Owner is TSBaseFrm then begin
    LClassName := TSBaseFrm(Owner).InternalClassName;
    LVersion   := TSBaseFrm(Owner).Version;
  end else begin
    LClassName := TForm(Owner).ClassName;
    LVersion   := 0;
  end;

  LIdentity := 1;
  try
    for I := Pred(FChilds.Count) downto 0 do
      if FChilds[I] is TSBaseFrm then
        with TSBaseFrm(FChilds[I]) do
          if SaveAllowed and (Owner = Self.Owner) and (GetClass(ClassName) <> nil) then try
            LFormRestoreParams := FormRestoreParams;

            if (not CreateFormIdentityParams.IsEmpty) then
              LFormRestoreParams[CreateFormIdentityParams] := CreateFormParams[CreateFormIdentityParams];

            if not SameText(ClassName, InternalClassName) then
              LFormRestoreParams[SConst_ClassName] := InternalClassName;

            LFormRestoreParams[SConst_Window] := ClassName;
            for K := Pred(LFormRestoreParams.Count) downto 0 do
              if VarIsPresent(LFormRestoreParams.Items[K].Value) then
                SBaseSetUserClassVariableProcedure
                (
                          LClassName
                        , LVersion
                        , SConst_Window + ':' + IntToStr(LIdentity)
                        , LFormRestoreParams.Items[K].Name
                        , 0
                        , LFormRestoreParams.Items[K].Value
                );

              Inc(LIdentity);
          except
          end;
  except
  end;
end;
{$ENDREGION 'TSBaseMainFormMDIInterface'}

{$REGION 'FASTSCRIPT_RTTI'}
{$IFDEF FASTSCRIPT_RTTI}
{ TfsFunctions_TSBaseFrm }

constructor TfsFunctions_TSBaseFrm.Create(AScript: TfsScript);
const
  SCategory: String = 'Прочие';
  SBaseFormCategory: String = 'Функции базовой формы';
begin
  inherited;

//  AScript.AddMethod('function FindClass(Name: String): Pointer', Call_TSBaseFrm_Method, SCategory);
  AScript.AddMethod('procedure HandleMessage', Call_TSBaseFrm_Method, SCategory);
  AScript.AddMethod('procedure HTMLLinkClick(ALink: String)', Call_TSBaseFrm_Method, SCategory);
  AScript.AddMethod('function ICreateByParams(const AClassName: String; AOwner: Pointer; AParams: TNamedVariants): Pointer', Call_TSBaseFrm_Method);

  AScript.AddConst('lgInfo'   , DelphiTypeChar, 'I');
  AScript.AddConst('lgWarning', DelphiTypeChar, 'W');
  AScript.AddConst('lgError'  , DelphiTypeChar, 'E');
  AScript.AddMethod('procedure Log(ALogLevel: Char; AMessages: String; AProcessMessages: Boolean = False)', Call_TSBaseFrm_Method, SCategory);

  AScript.AddEnum('TControlDefaultModifyState', 'bfcsDefaultWriteAble, bfcsWriteAbleIfEditing, bfcsWriteAble, bfcsReadOnly');

  AScript.AddMethod('procedure InfoMessage(Message: String; Control: TObject = nil)', Call_TSBaseFrm_Method, SBaseFormCategory);
  AScript.AddMethod('procedure WarningMessage(Message: String; Control: TObject = nil)', Call_TSBaseFrm_Method, SBaseFormCategory);
  AScript.AddMethod('procedure ErrorMessage(Message: String; Control: TObject = nil)', Call_TSBaseFrm_Method, SBaseFormCategory);
  AScript.AddMethod('function ConfirmMessage(Message: String; Control: TObject = nil): Boolean', Call_TSBaseFrm_Method, SBaseFormCategory);
  AScript.AddMethod('function YesNoAbortMessage(Message: String; Control: TObject = nil): Boolean', Call_TSBaseFrm_Method, SBaseFormCategory);

  AScript.AddMethod('procedure StringFieldChange(Sender: TField)', Call_TSBaseFrm_Method);
  AScript.AddMethod('procedure TextFieldChange(Sender: TField)', Call_TSBaseFrm_Method);

  with AScript.AddClass(TSBaseFrm, TSBaseFrm.ClassParent.ClassName) do begin
    AddConstructor('conctructor CreateByParams(AOwner: Pointer; AParams: TNamedVariants)', Call_TSBaseFrm_Method);
    AddMethod('procedure Show', Call_TSBaseFrm_Method);

    AddMethod('function Select(AOwner: Pointer; AParams: TNamedVariants): Variant', Call_TSBaseFrm_Method);
    AddMethod('procedure InternalReInit(AParams: TNamedVariants)', Call_TSBaseFrm_Method);

    AddMethod('procedure WarningMessage(Message: String; Control: TObject = nil)', Call_TSBaseFrm_Method);
    AddMethod('procedure ErrorMessage(Message: String; Control: TObject = nil)', Call_TSBaseFrm_Method);

    AddMethod('procedure RegisterControlStateRule(AControl: TComponent; AControlModifyState: TControlDefaultModifyState; ADataSource: TDataSource = nil; const AFieldName: String = '''')', Call_TSBaseFrm_Method);
    AddMethod('procedure UpdateFormState(AControl: TWinControl = nil)', Call_TSBaseFrm_Method);
    AddMethod('procedure UpdateControlState(AControl: TComponent)', Call_TSBaseFrm_Method);

    AddMethod('procedure SetFilterItem(const AComponent: TComponent; const AParamName: String)', Call_TSBaseFrm_Method);

    AddMethod('procedure UpdateCaption', Call_TSBaseFrm_Method);
    AddMethod('function FormatCaption(AFormat: String): String', Call_TSBaseFrm_Method);

    AddProperty('CreateFormParams', TNamedVariants.TypeName, Get_TSBaseFrm_Property);
    AddProperty('InternalFormParams', TNamedVariants.TypeName, Get_TSBaseFrm_Property);
    AddProperty('InternalFormConstants', TNamedVariants.TypeName, Get_TSBaseFrm_Property);
    AddMethod('function GetFormParamValues(const AParamNames: String; var AUnknownParams: String): Variant', Call_TSBaseFrm_Method);

//    AddIndexProperty('Variables', DelphiTypeString, DelphiTypeVariant, Call_TSBaseFrm_Method, True);

    AddProperty('AdjustSize', 'TWinControl', nil, Set_TSBaseFrm_Property);
    AddProperty('ResultValues', TNamedVariants.TypeName, Get_TSBaseFrm_Property);

    AddMethod('function GetSetting(Name: String; Version: Byte = 0): Variant', Call_TSBaseFrm_Method);
    AddMethod('procedure SetSetting(Name: String; Value: Variant; Version: Byte = 0)', Call_TSBaseFrm_Method);

    AddMethod('function CallFunction(const AFunction: String; AParams: Variant; const ARaiseIfNotFound: Boolean = True): Variant', Call_TSBaseFrm_Method);
    AddMethod('function ExecSQL(const ACommandText: String; const AParams: Variant = nil): Variant', Call_TSBaseFrm_Method);
    AddMethod('function ExecSQL(const ACommandText: String; const AParams: TNamedVariants): Variant', Call_TSBaseFrm_Method);
  end;
end;

function TfsFunctions_TSBaseFrm.Get_TSBaseFrm_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
  if PropName = 'CREATEFORMPARAMS' then
    Result := Integer(@TSBaseFrm(Instance).CreateFormParams)
  else if PropName = 'INTERNALFORMPARAMS' then
    Result := Integer(@TSBaseFrm(Instance).InternalFormParams)
  else if PropName = 'INTERNALFORMCONSTANTS' then
    Result := Integer(@TSBaseFrm(Instance).InternalFormConstants)
  else if PropName = 'RESULTVALUES' then
    Result := Integer(@TSBaseFrm(Instance).ResultValues)
end;

procedure TfsFunctions_TSBaseFrm.Set_TSBaseFrm_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
//  if PropName = 'RESULTVALUES' then
//    TSBaseFrm(Instance).ResultValues := Value
  if PropName = 'ADJUSTSIZE' then
    TSBaseFrm.AdjustSize(TWinControl(VarToPointer(Value)));
end;

function TfsFunctions_TSBaseFrm.Call_TSBaseFrm_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  LString: String;
//  LDataSourceUpdateStates: TDataSourceUpdateStates;
begin
  if ClassType = nil then begin
//    if MethodName = 'FINDCLASS' then
//      Result := Integer(FindClass(Caller.Params[0]))
//    else
    if MethodName = 'LOG' then
      SBaseMainFormMDIInterface.Log(VarToStr(Caller.Params[0])[1], Caller.Params[1], Caller.Params[2] = True)
    else if MethodName = 'HANDLEMESSAGE' then
      HandleMessage
    else if MethodName = 'HTMLLINKCLICK' then
      HTMLLinkClick(Caller.Params[0])
    else if MethodName = 'ICREATEBYPARAMS' then
      Result := Integer(Pointer(SBaseFormDefinitions.ICreateByParams(Caller.Params[0], TComponent(VarToPointer(Caller.Params[1])), TNamedVariants.Create(Caller.Params[2], True))))

    else if MethodName = 'INFOMESSAGE' then
      TSBaseFrm.InfoMessage(Caller.Params[0], VarToPointer(Caller.Params[1]))
    else if MethodName = 'WARNINGMESSAGE' then
      TSBaseFrm(nil).WarningMessage(Caller.Params[0], VarToPointer(Caller.Params[1]))
    else if MethodName = 'ERRORMESSAGE' then
      TSBaseFrm(nil).ErrorMessage(Caller.Params[0], VarToPointer(Caller.Params[1]))
    else if MethodName = 'CONFIRMMESSAGE' then
      Result := TSBaseFrm.ConfirmMessage(Caller.Params[0], TControl(VarToPointer(Caller.Params[1])))
    else if MethodName = 'YESNOABORTMESSAGE' then
      Result := TSBaseFrm.YesNoAbortMessage(Caller.Params[0], TControl(VarToPointer(Caller.Params[1])))

    else if MethodName = 'STRINGFIELDCHANGE' then
      TSBaseFrm(nil).StringFieldChange(TField(VarToPointer(Caller.Params[0])))
    else if MethodName = 'TEXTFIELDCHANGE' then
      TSBaseFrm(nil).TextFieldChange(TField(VarToPointer(Caller.Params[0])))

  end else
    if MethodName = 'WARNINGMESSAGE' then
      TSBaseFrm(Instance).WarningMessage(Caller.Params[0], VarToPointer(Caller.Params[1]))
    else if MethodName = 'ERRORMESSAGE' then
      TSBaseFrm(Instance).ErrorMessage(Caller.Params[0], VarToPointer(Caller.Params[1]))
//    else if MethodName = 'VARIABLES.GET' then
//      Result := TSBaseFrm(Instance).Variables[Caller.Params[0]]
    else if MethodName = 'REGISTERCONTROLSTATERULE' then
      TSBaseFrm(Instance).RegisterControlStateRule(TComponent(VarToPointer(Caller.Params[0])), Caller.Params[1], VarToPointer(Caller.Params[2]), VarToStr(Caller.Params[3]))
    else if MethodName = 'UPDATEFORMSTATE' then
      TSBaseFrm(Instance).UpdateFormState(TWinControl(VarToPointer(Caller.Params[0])))
    else if MethodName = 'UPDATECONTROLSTATE' then
      TSBaseFrm(Instance).UpdateControlState(TComponent(VarToPointer(Caller.Params[0])))

    else if MethodName = 'SETFILTERITEM' then
      TSBaseFrm(Instance).SetFilterItem(TComponent(VarToPointer(Caller.Params[0])), Caller.Params[1])

    else if MethodName = 'UPDATECAPTION' then
      TSBaseFrm(Instance).UpdateCaption
    else if MethodName = 'FORMATCAPTION' then
      Result := TSBaseFrm(Instance).FormatCaption(Caller.Params[0])

    else if MethodName = 'GETSETTING' then
      Result := TSBaseFrm(Instance).GetSetting(Caller.Params[0], VarToIntDef(Caller.Params[1], 0))
    else if MethodName = 'SETSETTING' then
      TSBaseFrm(Instance).SetSetting(Caller.Params[0], Caller.Params[1], VarToIntDef(Caller.Params[2], 0))

    else if MethodName = 'CREATEBYPARAMS' then
      Result := Integer(TSBaseFrm(Instance).CreateByParams(TComponent(VarToPointer(Caller.Params[0])), TNamedVariants.Create(Caller.Params[1], True)))
    else if MethodName = 'SELECT' then
      Result := TSBaseFrmClass(Instance.ClassType).Select(TComponent(VarToPointer(Caller.Params[0])), TNamedVariants.Create(Caller.Params[1], True)).AsVariant
    else if MethodName = 'SHOW' then
      TSBaseFrm(Instance).Show
    else if MethodName = 'INTERNALREINIT' then
      TSBaseFrm(Instance).InternalReInit(TNamedVariants.Create(Caller.Params[0], True))
    else if MethodName = 'CALLFUNCTION' then
      Result := TSBaseFrm(Instance).CallFunction(Caller.Params[0], TNamedVariants.Create(Caller.Params[1], True), VarToBoolDef(Caller.Params[2], True))
    else if MethodName = 'EXECSQL' then
      Result := TSBaseFrm(Instance).ExecSQL(Caller.Params[0], TNamedVariants.Create(Caller.Params[1], True), TSBaseFrm(Instance).GetFormParamValues).AsVariant
    else if MethodName = 'GETFORMPARAMVALUES' then begin
      Result := TSBaseFrm(Instance).GetFormParamValues(Caller.Params[0], LString);
      Caller.Params[1] := LString;
    end;
end;

{$ENDIF}
{$ENDREGION 'FASTSCRIPT_RTTI'}

{$REGION 'TfsScriptHelper'}
{ TfsScriptHelper }

function TfsScriptHelper.ErrorInfo(const AErrorPos: String): TfsScriptErrorInfo;
var
  LIndex, I: Integer;
  LLineFrom, LLineTo: Integer;
begin
  Result.Scope := '';
  LIndex := Pos(':', AErrorPos);

  if LIndex > 0 then begin
    Result.Line := StrToIntDef(LeftStr(AErrorPos, LIndex - 1), 0) - 1;
    if Assigned(Lines) and (Result.Line >= 0) and (Result.Line < Lines.Count) then begin
      if Result.Line > 0  then
        LLineFrom := Result.Line - 1
      else
        LLineFrom := Result.Line;

      if Result.Line < Lines.Count - 1  then
        LLineTo := Result.Line + 1
      else
        LLineTo := Result.Line;

      for I := LLineFrom to LLineTo do begin
        Result.Scope := Result.Scope + IntToStr(I) + ':' + Lines[I];
        if i < LLineTo then
          Result.Scope := Result.Scope + SConst_CR;
      end;
    end;

  end else
    Result.Line := -1;
end;
{$ENDREGION 'TfsScriptHelper'}


{$REGION 'TSBaseVirtualSubClassInfo'}
{ TSBaseVirtualSubClassInfo }

procedure TSBaseVirtualSubClassInfo.Init;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

procedure TSBaseVirtualSubClassInfo.Release;
begin
  IdentityParams := unAssigned;
{$IFDEF DEBUG}
  SetLength(DfmText, 0);
{$ENDIF}
  FreeAndNil(DFM);
  SetLength(Script, 0);
end;

{$ENDREGION 'TSBaseVirtualSubClassInfo'}

{$REGION 'TSBaseVirtualInstanceInfo'}
{ TSBaseVirtualInstanceInfo }

procedure TSBaseVirtualInstanceInfo.Init;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

procedure TSBaseVirtualInstanceInfo.Release;
begin
  IdentityParams := unAssigned;
//  Privileges     := -1;
  SetLength(Identity, 0);
{$IFDEF DEBUG}
  SetLength(DfmText, 0);
{$ENDIF}
  FreeAndNil(DFM);
  SetLength(Script, 0);
end;
{$ENDREGION 'TSBaseVirtualInstanceInfo'}

initialization
  SBaseFormProperties := TSBaseFormProperties.Create(Application);
  SBaseFormProperties.Name := 'SBaseFormProperties';

  SBaseVirtualClasses := TObjectDictionary<String, TSBaseVirtualClassInfo>.Create([doOwnsValues]);

  RegisterClass(TSBaseFormProperties);
  RegisterClass(TSBaseFrm);
  RegisterVirtualClassOwner(TSBaseFrm);

{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_TSBaseFrm);
{$ENDIF}

finalization
  SBaseClassesFinalization;

  UnRegisterClass(TSBaseFormProperties);
  UnRegisterClass(TSBaseFrm);

{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TfsFunctions_TSBaseFrm);
{$ENDIF}

end.

