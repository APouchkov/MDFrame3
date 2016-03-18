unit SBaseUtils;
{$I config.inc}

interface

uses
  SysUtils, Types, DateUtils, Math, Classes, Generics.Collections, RegularExpressions

{$IFNDEF PACKAGE}
  , Forms, Controls, StdCtrls, Graphics
  , cxGridCustomTableView, cxGrid, cxTL, cxVGrid, hsDialogs, Dialogs
{$ENDIF}

{$IFDEF DFM_LOADER}
  , PropFilerEh
{$ENDIF}

{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter
{$ENDIF}
  ;

{$IFDEF DEBUG}
  procedure DEBUG_FreeAndNil(var Obj);
{$ENDIF}

{$IFNDEF PACKAGE}
  procedure HandleMessage;
{$ENDIF}

  function StartMonth(ADate: TDate): TDate; overload; inline;
  function StartMonth(ADateTime: TDateTime): TDate; overload; inline;

type
  TAppendMode = (amRaiseIfRepeated, amRaiseIfChanged, amAppend, amOverride, amAggregate);
  TLoadValueCondition = (lvcIfNotPresent, lvcIfReceive, lvcAlways);

  /// Фабрика енумераторов
  TBaseEnumeratorFactory<T> = record
  private
    FEnumerator: T;
  public
    constructor Create(const AEnumerator: T);
    function GetEnumerator: T;
  end;

//  TPointerEnumeratorFactory = TBaseEnumeratorFactory<Pointer>;

  TMDFreeNotificationEvent = procedure(Sender: TComponent) of object;
  TMDFreeNotificator = class(TComponent)
  private
    FOnFreeNotification: TMDFreeNotificationEvent;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure AddSender(ASender: TComponent);
    procedure RemoveSender(ASender: TComponent);
    property OnFreeNotification: TMDFreeNotificationEvent read FOnFreeNotification write FOnFreeNotification;
  end;

  TSBaseProperties = class(TComponent)
  private
    FText_MenuView, FText_MenuEdit,
    FText_Appending, FText_Copying, FText_Browsing, FText_Editing,  FText_StartEditing,
    FText_MenuNew, FText_MenuNewLevel, FText_MenuNewSubLevel, FText_MenuNewElement, FText_MenuNewSubElement, FText_MenuCreate,
    FText_MenuDelete, FText_MenuUnDelete, FText_MenuDeleteAll, FText_MenuClear, FText_MenuCopy, FText_MenuCopyAll, FText_MenuCut, FText_MenuPaste,
    FText_SubMenuMask, FText_SubMenuFile, FText_SubMenuFolder, FText_MenuLoadFromFile, FText_MenuSaveToFile,
    FText_MenuPost, FText_MenuApply,
    FText_MenuMarkAsDeleted, FText_MenuUnMarkFromDeleted,
    FText_MenuRefresh, FText_MenuRefreshRecord, FText_MenuSelect, FText_MenuTake, FText_MenuHighlightAll, FText_MenuDisplay, FText_MenuBackward, FText_MenuForward,
    FText_MenuFinish, FText_MenuHelp,
    FText_MenuPrint, FText_MenuExportTo,
    FText_MenuOk, FText_MenuSave, FText_MenuRestore, FText_MenuClose, FText_MenuCancel, FText_MenuWindowPrev, FText_MenuWindowNext, FText_MenuMinimize, FText_MenuMaximize,

    FText_MenuRun, FText_MenuPause, FText_MenuStep, FText_MenuBreakPoint, FText_MenuWatches,

    FText_MenuDatesPeriod, FText_MenuShowTree, FText_MenuFindInTree,
    FText_MenuFilterBox, FText_MenuFilterClear, FText_MenuFilterDelete, FText_MenuFilterNew, FText_MenuFilterAction,
    FText_MenuAddToList, FText_MenuDeleteFromList,
    FText_MenuGroupBox, FText_MenuGroupingClear, FText_MenuExpandAll, FText_MenuCollapseAll,
    FText_MenuDetails, FText_MenuProperties, FText_MenuObjectInspector,
    FText_MenuFitToWidth, FText_MenuFitToHeight, FText_MenuFitToScreen,
    FText_MenuWindowsCascade, FText_MenuWindowsTileHor, FText_MenuWindowsTileVert, FText_MenuWindowsMinimize, FText_MenuWindowsArrange,

    FText_RequestedFile, FText_RecommendedFiles,
    FText_ActionIncluding, FText_ActionExcluding,

    FText_RightPlace, FText_BottomPlace,
    FText_WindowsStyles, FText_DevExpressSkins, FText_SkinDefault, FText_Mask, FText_Find, FText_Date, FText_Time, FText_DateAndTime,
    FText_ToolBar, FText_Actions, FText_Windows, FText_RecordCount, FText_ExecutionTime,
    FText_ArrayOfValues,

    FText_Line, FText_Column,

    FConfirmation_ApplyRequired,

    FMessage_ViewAccessDenied,
    FMessage_FilterNotPresent, FMessage_TreeFilterNotPresent, FMessage_DataSetNotActive,
    FMessage_ThisFieldMustHaveAValue,
    FMessage_ProcessStarted, FMessage_ProcessWaitForCompletion, FMessage_ProcessTerminating,

    FMessageFormat_DataLoadedFromFile, FMessageFormat_DataLoadedFromSource,
    FMessageFormat_ErrorDataLoadFromSource, FMessageFormat_ErrorDataLoadFromFile,

    FMessage_DataUploaded,
    FMessage_CannotContinueWhileError, FMessage_CannotContinueWhileNoChecks,

    FMessageFormat_FileNotFound, FMessageFormat_MultipleFilesFound,
    FMessageFormat_CantFindText,
    FMessageFormat_ClassNotAccessible,
    FMessageFormat_TechnicalError
    : String;
  public
    constructor Create(AOwner: TComponent); override;

    procedure RaiseTechnicalError(const AError: String);
  published
    property Text_Appending: String read FText_Appending write FText_Appending;
    property Text_Copying: String read FText_Copying write FText_Copying;
    property Text_Browsing: String read FText_Browsing write FText_Browsing;
    property Text_Editing: String read FText_Editing write FText_Editing;
    property Text_StartEditing: String read FText_StartEditing write FText_StartEditing;
    property Text_MenuView: String read FText_MenuView write FText_MenuView;
    property Text_MenuEdit: String read FText_MenuEdit write FText_MenuEdit;
    property Text_MenuNew: String read FText_MenuNew write FText_MenuNew;
    property Text_MenuNewLevel: String read FText_MenuNewLevel write FText_MenuNewLevel;
    property Text_MenuNewSubLevel: String read FText_MenuNewSubLevel write FText_MenuNewSubLevel;
    property Text_MenuNewElement: String read FText_MenuNewElement write FText_MenuNewElement;
    property Text_MenuNewSubElement: String read FText_MenuNewSubElement write FText_MenuNewSubElement;
    property Text_MenuCreate: String read FText_MenuCreate write FText_MenuCreate;
    property Text_MenuDelete: String read FText_MenuDelete write FText_MenuDelete;
    property Text_MenuDeleteAll: String read FText_MenuDeleteAll write FText_MenuDeleteAll;
    property Text_MenuUnDelete: String read FText_MenuUnDelete write FText_MenuUnDelete;
    property Text_MenuClear: String read FText_MenuClear write FText_MenuClear;
    property Text_MenuCopy: String read FText_MenuCopy write FText_MenuCopy;
    property Text_MenuCopyAll: String read FText_MenuCopyAll write FText_MenuCopyAll;
    property Text_MenuCut: String read FText_MenuCut write FText_MenuCut;
    property Text_MenuPaste: String read FText_MenuPaste write FText_MenuPaste;

    property Text_SubMenuMask: String read FText_SubMenuMask write FText_SubMenuMask;
    property Text_SubMenuFile: String read FText_SubMenuFile write FText_SubMenuFile;
    property Text_SubMenuFolder: String read FText_SubMenuFolder write FText_SubMenuFolder;

    property Text_MenuLoadFromFile: String read FText_MenuLoadFromFile write FText_MenuLoadFromFile;
    property Text_MenuSaveToFile: String read FText_MenuSaveToFile write FText_MenuSaveToFile;
    property Text_MenuPost: String read FText_MenuPost write FText_MenuPost;
    property Text_MenuApply: String read FText_MenuApply write FText_MenuApply;
    property Text_MenuMarkAsDeleted: String read FText_MenuMarkAsDeleted write FText_MenuMarkAsDeleted;
    property Text_MenuUnMarkFromDeleted: String read FText_MenuUnMarkFromDeleted write FText_MenuUnMarkFromDeleted;
    property Text_MenuRefresh: String read FText_MenuRefresh write FText_MenuRefresh;
    property Text_MenuRefreshRecord: String read FText_MenuRefreshRecord write FText_MenuRefreshRecord;
    property Text_MenuDisplay: String read FText_MenuDisplay write FText_MenuDisplay;
    property Text_MenuSelect: String read FText_MenuSelect write FText_MenuSelect;
    property Text_MenuTake: String read FText_MenuTake write FText_MenuTake;
    property Text_MenuHighlightAll: String read FText_MenuHighlightAll write FText_MenuHighlightAll;
    property Text_MenuBackward: String read FText_MenuBackward write FText_MenuBackward;
    property Text_MenuForward: String read FText_MenuForward write FText_MenuForward;
    property Text_MenuFinish: String read FText_MenuFinish write FText_MenuFinish;
    property Text_MenuHelp: String read FText_MenuHelp write FText_MenuHelp;


    property Text_MenuPrint: String read FText_MenuPrint write FText_MenuPrint;
    property Text_MenuExportTo: String read FText_MenuExportTo write FText_MenuExportTo;
    property Text_MenuWindowPrev: String read FText_MenuWindowPrev write FText_MenuWindowPrev;
    property Text_MenuWindowNext: String read FText_MenuWindowNext write FText_MenuWindowNext;
    property Text_MenuMinimize: String read FText_MenuMinimize write FText_MenuMinimize;
    property Text_MenuMaximize: String read FText_MenuMaximize write FText_MenuMaximize;
    property Text_MenuRestore: String read FText_MenuRestore write FText_MenuRestore;
    property Text_MenuClose: String read FText_MenuClose write FText_MenuClose;
    property Text_MenuCancel: String read FText_MenuCancel write FText_MenuCancel;
    property Text_MenuOk: String read FText_MenuOk write FText_MenuOk;
    property Text_MenuSave: String read FText_MenuSave write FText_MenuSave;

    property Text_MenuRun       : String read FText_MenuRun         write FText_MenuRun;
    property Text_MenuPause     : String read FText_MenuPause       write FText_MenuPause;
    property Text_MenuStep      : String read FText_MenuStep        write FText_MenuStep;
    property Text_MenuBreakPoint: String read FText_MenuBreakPoint  write FText_MenuBreakPoint;
    property Text_MenuWatches   : String read FText_MenuWatches     write FText_MenuWatches;

    property Text_MenuDatesPeriod: String read FText_MenuDatesPeriod write FText_MenuDatesPeriod;
    property Text_MenuShowTree: String read FText_MenuShowTree write FText_MenuShowTree;
    property Text_MenuFindInTree: String read FText_MenuFindInTree write FText_MenuFindInTree;
    property Text_MenuFilterBox: String read FText_MenuFilterBox write FText_MenuFilterBox;
    property Text_MenuFilterClear: String read FText_MenuFilterClear write FText_MenuFilterClear;
    property Text_MenuFilterDelete: String read FText_MenuFilterDelete write FText_MenuFilterDelete;
    property Text_MenuFilterNew: String read FText_MenuFilterNew write FText_MenuFilterNew;
    property Text_MenuFilterAction: String read FText_MenuFilterAction write FText_MenuFilterAction;
    property Text_MenuAddToList: String read FText_MenuAddToList write FText_MenuAddToList;
    property Text_MenuDeleteFromList: String read FText_MenuDeleteFromList write FText_MenuDeleteFromList;

    property Text_MenuGroupBox: String read FText_MenuGroupBox write FText_MenuGroupBox;
    property Text_MenuGroupingClear: String read FText_MenuGroupingClear write FText_MenuGroupingClear;
    property Text_MenuExpandAll: String read FText_MenuExpandAll write FText_MenuExpandAll;
    property Text_MenuCollapseAll: String read FText_MenuCollapseAll write FText_MenuCollapseAll;
    property Text_MenuDetails: String read FText_MenuDetails write FText_MenuDetails;
    property Text_MenuProperties: String read FText_MenuProperties write FText_MenuProperties;
    property Text_MenuObjectInspector: String read FText_MenuObjectInspector write FText_MenuObjectInspector;
    property Text_MenuFitToWidth: String read FText_MenuFitToWidth write FText_MenuFitToWidth;
    property Text_MenuFitToHeight: String read FText_MenuFitToHeight write FText_MenuFitToHeight;
    property Text_MenuFitToScreen: String read FText_MenuFitToScreen write FText_MenuFitToScreen;

    property Text_MenuWindowsCascade: String read FText_MenuWindowsCascade write FText_MenuWindowsCascade;
    property Text_MenuWindowsTileHor: String read FText_MenuWindowsTileHor write FText_MenuWindowsTileHor;
    property Text_MenuWindowsTileVert: String read FText_MenuWindowsTileVert write FText_MenuWindowsTileVert;
    property Text_MenuWindowsMinimize: String read FText_MenuWindowsMinimize write FText_MenuWindowsMinimize;
    property Text_MenuWindowsArrange: String read FText_MenuWindowsArrange write FText_MenuWindowsArrange;

    property Text_RequestedFile: String read FText_RequestedFile write FText_RequestedFile;
    property Text_RecommendedFiles: String read FText_RecommendedFiles write FText_RecommendedFiles;
    property Text_ActionIncluding: String read FText_ActionIncluding write FText_ActionIncluding;
    property Text_ActionExcluding: String read FText_ActionExcluding write FText_ActionExcluding;

    property Text_RightPlace: String read FText_RightPlace write FText_RightPlace;
    property Text_BottomPlace: String read FText_BottomPlace write FText_BottomPlace;

    property Text_Date: String read FText_Date write FText_Date;
    property Text_Time: String read FText_Time write FText_Time;
    property Text_DateAndTime: String read FText_DateAndTime write FText_DateAndTime;
    property Text_Mask: String read FText_Mask write FText_Mask;
    property Text_Find: String read FText_Find write FText_Find;
    property Text_ToolBar: String read FText_ToolBar write FText_ToolBar;
    property Text_Actions: String read FText_Actions write FText_Actions;
    property Text_Windows: String read FText_Windows write FText_Windows;
    property Text_RecordCount: String read FText_RecordCount write FText_RecordCount;
    property Text_ExecutionTime: String read FText_ExecutionTime write FText_ExecutionTime;
    property Text_ArrayOfValues: String read FText_ArrayOfValues write FText_ArrayOfValues;

    property Text_Line: String read FText_Line write FText_Line;
    property Text_Column: String read FText_Column write FText_Column;

    property Confirmation_ApplyRequired: String read FConfirmation_ApplyRequired write FConfirmation_ApplyRequired;

    property Message_ViewAccessDenied: String read FMessage_ViewAccessDenied write FMessage_ViewAccessDenied;
    property Message_FilterNotPresent: String read FMessage_FilterNotPresent write FMessage_FilterNotPresent;
    property Message_TreeFilterNotPresent: String read FMessage_TreeFilterNotPresent write FMessage_TreeFilterNotPresent;
    property Message_DataSetNotActive: String read FMessage_DataSetNotActive write FMessage_DataSetNotActive;
    property Message_ThisFieldMustHaveAValue: String read FMessage_ThisFieldMustHaveAValue write FMessage_ThisFieldMustHaveAValue;

    property Message_ProcessStarted: String read FMessage_ProcessStarted write FMessage_ProcessStarted;
    property Message_ProcessWaitForCompletion: String read FMessage_ProcessWaitForCompletion write FMessage_ProcessWaitForCompletion;
    property Message_ProcessTerminating: String read FMessage_ProcessTerminating write FMessage_ProcessTerminating;

    property Message_DataUploaded: String read FMessage_DataUploaded write FMessage_DataUploaded;
    property Message_CannotContinueWhileError   : String read FMessage_CannotContinueWhileError     write FMessage_CannotContinueWhileError;
    property Message_CannotContinueWhileNoChecks: String read FMessage_CannotContinueWhileNoChecks  write FMessage_CannotContinueWhileNoChecks;

    property MessageFormat_TechnicalError         : String read FMessageFormat_TechnicalError           write FMessageFormat_TechnicalError;
    property MessageFormat_ClassNotAccessible     : String read FMessageFormat_ClassNotAccessible       write FMessageFormat_ClassNotAccessible;
    property MessageFormat_FileNotFound           : String read FMessageFormat_FileNotFound             write FMessageFormat_FileNotFound;
    property MessageFormat_MultipleFilesFound     : String read FMessageFormat_MultipleFilesFound       write FMessageFormat_MultipleFilesFound;
    property MessageFormat_CantFindText           : String read FMessageFormat_CantFindText             write FMessageFormat_CantFindText;
    property MessageFormat_DataLoadedFromFile     : String read FMessageFormat_DataLoadedFromFile       write FMessageFormat_DataLoadedFromFile;
    property MessageFormat_ErrorDataLoadFromFile  : String read FMessageFormat_ErrorDataLoadFromFile    write FMessageFormat_ErrorDataLoadFromFile;
    property MessageFormat_DataLoadedFromSource   : String read FMessageFormat_DataLoadedFromSource     write FMessageFormat_DataLoadedFromSource;
    property MessageFormat_ErrorDataLoadFromSource: String read FMessageFormat_ErrorDataLoadFromSource  write FMessageFormat_ErrorDataLoadFromSource;

    property Text_WindowsStyles: String read FText_WindowsStyles write FText_WindowsStyles;
    property Text_DevExpressSkins: String read FText_DevExpressSkins write FText_DevExpressSkins;

//    property Text_StyleDefault: String read FText_StyleDefault write FText_StyleDefault;
    property Text_SkinDefault: String read FText_SkinDefault write FText_SkinDefault;
  end;

  TTimeCounter = record
  public
    StartTime, StopTime: TDateTime;
    const TypeName: String = 'TTimeCounter';

    procedure Start;
    procedure Stop;
    function Restart: TDateTime;
    function Interval: TDateTime;
    function IntervalMilliSeconds: Int64;
    // ADefaultFormat: 0 = Milliseconds, 1 = Seconds, 2 = Hours, 3 = Days
    class function Format(AInterval: TDateTime; const ADefaultFormat: Byte = 1; ADisplayMilliseconds: Boolean = True): String; static;
  end;
  PTimeCounter = ^TTimeCounter;

{$IFNDEF PACKAGE}
  TControlRect = record
    Control: TControl;
    Rect: TRect;
    Target: TPoint;
  public
    constructor FindFocus(AControl: TObject);
    class function GetControl(AControl: TObject; ASubControlIndex: SmallInt): TObject; static;
  end;
{$ENDIF}

{$IFNDEF PACKAGE}
  TComponentHelper = class Helper for TComponent
  public
//    function DisplayName: String;
    function FindComponent(const AName: String; AClass: TClass; out AComponent: TComponent): Boolean; overload;
{$IFDEF DFM_LOADER}
    procedure LoadDFM(const ADFM: String); overload;
    procedure LoadDFM(ADFM: TMemoryStream{$IFDEF DEBUG}; ADFMSource: PString = nil{$ENDIF}); overload;
{$ENDIF}

    function DFM: String;
  end;

  TPersistentHelper = class Helper for TPersistent
  public
    function FindOwner(AClass: TClass): TPersistent;
  end;
{$ENDIF}

{$IFDEF FASTSCRIPT_RTTI}
  TfsRecords<T> = class(TfsClassVariable)
  type
    PT = ^T;
  private
    FRecords : TList<PT>;
  protected
    function GetValue: Variant; override;
  public
    constructor Register(AScript: TfsScript; const AName: String);
    destructor Destroy; override;
    procedure Delete(ARecord: PT);
  end;

  TfsFunctions_SBaseUtils = class(TfsRTTIModule)
  private
    FfsTimeCounters: TfsRecords<TTimeCounter>;
    function Call_TTimeCounter_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

var
  SBaseProperties: TSBaseProperties;

implementation

{$IFNDEF PACKAGE}
uses
  SBaseCxUtils,
{$IFDEF DFM_LOADER}
  SBaseDfmView,
{$ENDIF}
  NamedVariables,
  SBaseConstants;
{$ENDIF}

{$IFNDEF PACKAGE}
type
  TcxvgControllerCrack = class(TcxvgController);
  TcxCustomMultiEditorRowCrack = class(TcxCustomMultiEditorRow);
{$ENDIF}

{$IFDEF DEBUG}
procedure DEBUG_FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  try
    Temp.Free;
  except
    Raise;
  end;
end;
{$ENDIF}

{$IFNDEF PACKAGE}
procedure HandleMessage;
begin
  try
    Application.HandleMessage
  except
    Application.HandleException(Application);
  end;
end;
{$ENDIF}

{$REGION 'TBaseEnumeratorFactory'}
{ TBaseEnumeratorFactory<T> }

constructor TBaseEnumeratorFactory<T>.Create(const AEnumerator: T);
begin
  FEnumerator := AEnumerator;
end;

function TBaseEnumeratorFactory<T>.GetEnumerator: T;
begin
  Result := FEnumerator;
end;
{$ENDREGION}

function StartMonth(ADate: TDate): TDate;
begin
  Result := IncDay(ADate, 1 - DayOf(ADate));
end;

function StartMonth(ADateTime: TDateTime): TDate;
begin
  Result := Trunc(ADateTime);
  Result := StartMonth(Result);
end;

{$REGION 'TTimeCounter'}
{ TTimeCounter }

class function TTimeCounter.Format(AInterval: TDateTime; const ADefaultFormat: Byte; ADisplayMilliseconds: Boolean): String;
var
  LDays, LHours, LMinutes, LSeconds, LMilliSeconds: Word;
  LFormat: String;
begin
  DecodeTime(AInterval, LHours, LMinutes, LSeconds, LMilliSeconds);
  LDays := Floor(AInterval);
  LFormat := 'ss';
  if ADisplayMilliseconds then
    LFormat := LFormat + '.zzz';
  if (LDays > 0) or (LHours > 0) or (LMinutes > 0) or (ADefaultFormat > 1) then
    LFormat := 'nn:' + LFormat;
  if (LDays > 0) or (LHours > 0) or (ADefaultFormat > 2) then
    LFormat := 'hh:' + LFormat;
  Result := FormatDateTime(LFormat, AInterval);
  if LDays > 0 then
    Result := IntToStr(LDays) + 'д. ' + Result; //TODO: Language set
end;

function TTimeCounter.Interval: TDateTime;
begin
  if StopTime = 0 then
    Result := Now() - StartTime
  else
    Result := StopTime - StartTime
end;

function TTimeCounter.IntervalMilliSeconds: Int64;
var
  LStopTime: TDateTime;
begin
  if StopTime = 0 then
    LStopTime := Now()
  else
    LStopTime := StopTime;
  Result := MilliSecondsBetween(StartTime, LStopTime)
end;

function TTimeCounter.Restart: TDateTime;
var
  LNow: TDateTime;
begin
  LNow := Now();
  Result := LNow - StartTime;
  StartTime := LNow;
  StopTime := 0;
end;

procedure TTimeCounter.Start;
begin
  StartTime := Now();
  StopTime := 0;
end;

procedure TTimeCounter.Stop;
begin
  StopTime := Now();
end;
{$ENDREGION 'TTimeCounter'}

{$IFNDEF PACKAGE}
{$REGION 'TControlRect'}
{ TControlRect }

constructor TControlRect.FindFocus(AControl: TObject);
var
  LObject: TObject;

  procedure cxCustomGridTableItem(AItem: TcxCustomGridTableItem);
  begin
    AItem.MakeVisible;
    LObject := AItem.FocusedCellViewInfo;
    if Assigned(LObject) then
      Rect := AItem.GridView.ClientRectToGridRect(TcxGridTableDataCellViewInfo(LObject).GetAreaBoundsForPainting);
  end;

  procedure cxCustomGridFocus(AGridView: TcxCustomGridTableView);
  begin
    AGridView := TcxCustomGridTableView(AGridView.GetActualClone);
    if AGridView.OptionsSelection.CellSelect then begin
      LObject := AGridView.Controller.FocusedItem;
      if Assigned(LObject) then
        cxCustomGridTableItem(TcxCustomGridTableItem(LObject));
    end else begin
      LObject := AGridView.Controller.FocusedRecord;
      if Assigned(LObject) then begin
        TcxCustomGridRecord(LObject).MakeVisible;
        Rect := AGridView.ClientRectToGridRect(TcxCustomGridRecord(LObject).ViewInfo.GetAreaBoundsForPainting);
      end;
    end;
  end;

  procedure cxTreeListCell(ANode: TcxTreeListNode; AColumn: TcxTreeListColumn);
  begin
    ANode.MakeVisible;
    AColumn.MakeVisible;
    Rect := TcxCustomTreeList(Control).CellRect(ANode, AColumn);
  end;

  procedure cxCustomRow(ARow: TcxCustomRow; AValueIndex: Integer);
  var
    LIndex: Integer;
  begin
    LIndex := TcxCustomVerticalGrid(Control).Controller.FocusedRecordIndex;
    if LIndex = -1 then Exit;
    ARow.MakeVisible;
    TcxCustomVerticalGrid(Control).Controller.MakeFocusedRecordVisible;
    Rect := ARow.ViewInfo.RowValueInfo[LIndex, AValueIndex].ContentRect;
  end;
var
  LFrom, LSize: Integer;
  LcxCellEdit: TcxCellEdit absolute LObject;
begin
  Control := nil;
  Rect := TRect.Empty;
  Target.X := 0; Target.Y := 0;

  if Assigned(AControl) then begin
    if AControl is TcxCustomGrid then with TcxCustomGrid(AControl) do begin
      Control := TcxCustomGrid(AControl);
      if Assigned(LObject) then
        cxCustomGridFocus(TcxCustomGridTableView(TcxCustomGrid(AControl).FocusedView));
    end else if AControl is TcxCustomGridTableView then with TcxCustomGridTableView(AControl) do begin
      Self.Control := Control;
      cxCustomGridFocus(TcxCustomGridTableView(AControl));
    end else if AControl is TcxCustomGridTableItem then with TcxCustomGridTableItem(AControl) do begin
      LObject := TcxCustomGridTableItem(AControl).GridView;
      Control := TcxCustomGridTableView(LObject).Control;
      LObject := TcxCustomGridTableView(TcxCustomGridTableView(LObject).GetActualClone).FindItemByID(ID);
      if Assigned(LObject) then
        cxCustomGridTableItem(TcxCustomGridTableItem(LObject));
    end else if AControl is TcxCustomTreeList then with TcxCustomTreeList(AControl) do begin
      Control := TcxCustomTreeList(AControl);
      LObject := FocusedNode;
      if Assigned(LObject) then begin
        cxTreeListCell(TcxTreeListNode(LObject), FocusedColumn);
        if not OptionsSelection.CellSelect then begin
          Rect.Left := 0;
          Rect.Right := Width;
        end;
      end;
    end else if AControl is TcxTreeListColumn then with TcxTreeListColumn(AControl) do begin
      Control := TcxTreeListColumn(AControl).TreeList;
      LObject := TcxTreeList(Control).FocusedNode;
      if Assigned(LObject) then
        cxTreeListCell(TcxTreeListNode(LObject), TcxTreeListColumn(AControl));
    end else if AControl is TcxCustomVerticalGrid then with TcxCustomVerticalGrid(AControl) do begin
      Control := TcxCustomVerticalGrid(AControl);
      LObject := TcxvgControllerCrack(Controller).FocusedItem;
      if Assigned(LObject) then
        cxCustomRow(TcxCellEdit(LObject).Row, TcxCellEdit(LObject).CellIndex);
    end else if AControl is TcxCustomRow then with TcxCustomRow(AControl) do begin
      Control := VerticalGrid;
      cxCustomRow(TcxCustomRow(AControl), 0);
    end else if AControl is TcxCustomEditorRowProperties then with TcxCustomEditorRowProperties(AControl) do begin
      Control := Row.VerticalGrid;
      cxCustomRow(Row, Index);
    end else if AControl is TControl then begin
      Control := TControl(AControl);
    end;
  end;

  if Assigned(Control) then begin
    if Rect.IsEmpty then
      with Control do begin
        Rect.Left   := 0;
        Rect.Top    := 0;
        Rect.Width  := Width;
        Rect.Height := Height;
      end;

    if Rect.Left <= 0 then LFrom := 0 else LFrom := Rect.Left;
    if Rect.Right > Control.Width then LSize := Control.Width - LFrom else LSize := Rect.Right - LFrom;
    Target.X := LFrom + (LSize div 2);

    if Rect.Top <= 0 then LFrom := 0 else LFrom := Rect.Top;
    if Rect.Bottom > Control.Height then LSize := Control.Height - LFrom else LSize := Rect.Bottom - LFrom;
    Target.Y := LFrom + (LSize div 2);
  end;
end;

class function TControlRect.GetControl(AControl: TObject; ASubControlIndex: SmallInt): TObject;
begin
  Result := AControl;
  if (ASubControlIndex >= 0) then
    if Result is TcxCustomMultiEditorRow then
      Result := TcxCustomMultiEditorRowCrack(Result).Properties.Editors[ASubControlIndex];
end;
{$ENDREGION 'TControlRect'}
{$ENDIF}

{$REGION 'TComponentHelper'}
{$IFNDEF PACKAGE}
//function TComponentHelper.DisplayName: String;
//begin
//  if Self = nil then
//    Result := 'nil'
//  else begin
//    Result := Name;
//    if Result.IsEmpty then
//      Result := '<Noname>'
//  end;
//end;

function TComponentHelper.DFM: String;
var
  LComponentStream: TMemoryStream;
  LOutputStream: TStringStream;
begin
  Result := '';
  LComponentStream := TMemoryStream.Create;
  LOutputStream := TStringStream.Create;

  try
    LComponentStream.WriteComponent(Self);
    LComponentStream.Position := 0;
    ObjectBinaryToText(LComponentStream, LOutputStream);
    Result := LOutputStream.DataString;
  finally
    LComponentStream.Free;
    LOutputStream.Free;
  end;
end;

function TPersistentHelper.FindOwner(AClass: TClass): TPersistent;
var
  LOwner: TPersistent;
begin
  if ClassType.InheritsFrom(AClass) then
    Result := Self
  else begin
    LOwner := GetOwner;
    Assert(Assigned(LOwner));
    Result := LOwner.FindOwner(AClass)
  end;
end;

{$IFDEF DFM_LOADER}
procedure TComponentHelper.LoadDFM(const ADFM: String);
var
  MS: TMemoryStream;
begin
  if ADFM.IsEmpty then Exit;

  MS := TDfmParser.Parse(ADFM);
  if Assigned(MS) then try
    LoadDFM(MS);
  finally
    MS.Free
  end;
end;

procedure TComponentHelper.LoadDFM(ADFM: TMemoryStream{$IFDEF DEBUG}; ADFMSource: PString{$ENDIF});
var
  LPropReader: TPropReaderEh;
begin
  if (ADFM = nil) or (ADFM.Size = 0) then Exit;

  ADFM.Position := 0;

  LPropReader := TPropReaderEh.Create(ADFM, 1024);
{$IFDEF DEBUG}
  try
{$ENDIF}
    with LPropReader do
    try
      ReadRootComponent(Self)
    finally
      Free;
    end;
{$IFDEF DEBUG}
  except on E:Exception do begin
    if Assigned(ADFMSource) and (hsDialogs.HSMessageDlg(E.Message, mtError, [mbOk, mbAbort], 0) = mrAbort) then
      TSBaseDfmView.CreateByParams
      (
        Application.MainForm,
        TNamedVariants.Create
        (
          [
            TNamedVariant.Create(SConst_DFM, ADFMSource^),
            TNamedVariant.Create(SConst_Message, E.Message)
          ],
          True
        )
      ).ShowModal;

    Raise;
  end end;
{$ENDIF}
end;
{$ENDIF}
{$ENDIF}
{$ENDREGION}

{$REGION 'FASTSCRIPT_RTTI'}
{$IFDEF FASTSCRIPT_RTTI}
{ TfsTimeCounters }

constructor TfsRecords<T>.Register(AScript: TfsScript; const AName: String);
begin
  inherited Create(nil, AName);
  FProgram := AScript;
  AScript.Add(AName, Self);
  FRecords := TList<PT>.Create;
//  inherited Value := Value;
end;

destructor TfsRecords<T>.Destroy;
begin
  inherited;
  while FRecords.Count > 0 do begin
    Dispose(FRecords[0]);
    FRecords.Delete(0)
  end;
  FreeAndNil(FRecords);
end;

function TfsRecords<T>.GetValue: Variant;
var
  LNewVariable: PT;
begin
  New(LNewVariable);
  FRecords.Add(LNewVariable);
  Result := Integer(LNewVariable);
end;

procedure TfsRecords<T>.Delete(ARecord: PT);
var
  LIndex: Integer;
begin
  LIndex := FRecords.IndexOf(ARecord);
  if LIndex >= 0 then begin
    FRecords.Delete(LIndex);
    Dispose(ARecord);
  end;
end;

{ TfsFunctions_SBaseUtils }

constructor TfsFunctions_SBaseUtils.Create(AScript: TfsScript);
begin
  inherited;

  AScript.AddEnum('TAppendMode', 'amRaiseIfRepeated, amRaiseIfChanged, amAppend, amOverride, amAggregate');
  AScript.AddEnum('TLoadValueCondition', 'lvcIfNotPresent, lvcIfReceive, lvcAlways');

  FfsTimeCounters := TfsRecords<TTimeCounter>.Register(AScript, TTimeCounter.TypeName);
  with FfsTimeCounters do begin
    AddConstructor('constructor Create', Call_TTimeCounter_Method);
    AddMethod('procedure Free', Call_TTimeCounter_Method);
  end;
end;

function TfsFunctions_SBaseUtils.Call_TTimeCounter_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'CREATE' then
    Result := Integer(Instance)
  else if MethodName = 'FREE' then
    FfsTimeCounters.Delete(Pointer(Instance))
end;

{$ENDIF}
{$ENDREGION 'FASTSCRIPT_RTTI'}

{$REGION 'TMDFreeNotificator'}
{ TMDFreeNotificator }

procedure TMDFreeNotificator.AddSender(ASender: TComponent);
begin
  if ASender <> nil then
    ASender.FreeNotification(Self);
end;

procedure TMDFreeNotificator.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and Assigned(FOnFreeNotification) then
    FOnFreeNotification(AComponent);
end;

procedure TMDFreeNotificator.RemoveSender(ASender: TComponent);
begin
  if ASender <> nil then
    ASender.RemoveFreeNotification(Self);
end;
{$ENDREGION 'TMDFreeNotificator'}

{$REGION 'TSBaseProperties'}
{ TSBaseProperties }

constructor TSBaseProperties.Create(AOwner: TComponent);
begin
  inherited;

  FText_MenuOk          := 'Ok';
  FText_MenuRun         := 'Run';
  FText_MenuPause       := 'Pause';
  FText_MenuStep        := 'Step';
  FText_MenuBreakPoint  := 'BreakPoint';
  FText_MenuWatches     := 'Watches';

  FConfirmation_ApplyRequired := 'Для выполнения этой операции данные должны быть сохранены. Сохранить?';

  FMessageFormat_ClassNotAccessible := 'Класс '#171'%s'#187' не доступен - в доступе отказано.';
  FMessageFormat_FileNotFound       := 'Файл '#171'%s'#187' не найден.';

//  FMessage_ProcessStarted   := '';
  FMessage_ProcessWaitForCompletion := 'Ожидается завершение процесса';
  FMessage_ProcessTerminating       := 'Идёт прерывание процесса';
end;
{$ENDREGION 'TSBaseProperties'}

procedure TSBaseProperties.RaiseTechnicalError(const AError: String);
begin
  Raise Exception.CreateFmt(FMessageFormat_TechnicalError, [AError]);
end;

function TComponentHelper.FindComponent(const AName: String; AClass: TClass; out AComponent: TComponent): Boolean;
begin
  AComponent := FindComponent(AName);
  Result := Assigned(AComponent);
  if Result then begin
    Result := AComponent.InheritsFrom(AClass);
    if not Result then
      AComponent := nil;
  end;
end;

initialization
{$IFNDEF PACKAGE}
  SBaseProperties := TSBaseProperties.Create(Application);
  SBaseProperties.Name := 'SBaseProperties';

  RegisterClass(TSBaseProperties);
{$ENDIF}

{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Insert(0, TfsFunctions_SBaseUtils);
{$ENDIF}

finalization
{$IFNDEF PACKAGE}
  UnRegisterClass(TSBaseProperties);
{$ENDIF}

{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TfsFunctions_SBaseUtils);
{$ENDIF}

end.
