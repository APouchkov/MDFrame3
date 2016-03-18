unit fsFunction_TcxGrid;
{$I config.inc}

interface

uses
 cxCustomData;

type
  TcxDataSummaryHelper = class Helper for TcxDataSummary
  private
    function GetFooterSummaryValuesByName(AFieldName: String): Variant;
    procedure SetFooterSummaryValuesByName(AFieldName: String; const Value: Variant);
  public
    // Возращает индекс Footer-а по имени колонки, для которой он определен
    function GetFooterSummaryIndexByFieldName(AFieldName: String): Integer;
    property FooterSummaryValuesByName[AFieldName: String]: Variant read GetFooterSummaryValuesByName write SetFooterSummaryValuesByName;
  end;

implementation

uses
  Types, SysUtils, Classes, DateUtils,
  frxClass, fs_iinterpreter, fs_itools, fs_ievents,
  fsFunction_Other, Variants, cxLookAndFeelPainters,
  cxStyles, Graphics, cxGraphics, cxFilter, cxData, cxDataStorage, cxDBData,
  cxGridLevel, cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxInplaceContainer,
  cxGridDBTableView, cxGridBandedTableView, cxGridDBBandedTableView, cxGrid,
  cxGridDBDataDefinitions, Controls, cxEdit, SBaseVariantFunctions,
  cxDBTL, cxTL, cxTLData, cxDropDownEdit, ImgList, cxLookAndFeels, cxLookupDBGrid,
  SBaseConstants, SBaseCxUtils, cxVGrid, cxDBVGrid, UFlexCelGrid
{$IFDEF DELPHIXE3}
  , System.UITypes
{$ENDIF}
;

type
  PcxSummaryEventArguments = ^TcxSummaryEventArguments;
  PcxSummaryEventOutArguments = ^TcxSummaryEventOutArguments;
  PcxViewParams = ^TcxViewParams;

  TFunctions_TcxGrid = class(TfsRTTIModule)
  private
    function  Call_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Call_TcxCustomGridTableView_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Get_TcxCustomGridTableView_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Call_TcxGridTableView_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Get_TcxGridTableView_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Call_TcxGridBands_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Get_TcxGridBands_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function  Get_TcxGridBandedTableView_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Get_TcxCustomGridView_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Get_TcxGrid_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TcxGrid_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Call_TcxGrid_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Call_TcxLookupDBGridColumn_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Call_TcxGridDBTableView_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Call_TcxCustomGridView_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Get_TcxGridViewData_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function  Call_TcxGridViewData_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TcxGridTableController_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TcxGridTableController_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Get_TcxCustomGridTableItem_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TcxCustomGridTableItem_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Call_TcxGridTableController_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Call_TcxGridEditingController_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Call_TcxFilterCriteriaItemList_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Call_TcxCustomDataController_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Get_TcxDataController_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TcxCustomDataController_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Get_TcxDataFilterCriteria_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Get_TcxDBDataController_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TcxDBDataController_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
    function  Call_TcxDBDataController_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Call_TcxCustomGridRecord_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Get_TcxCustomGridRecord_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Call_TcxCustomGridRow_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TcxGridTableCellViewInfo_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function  Get_TcxGridTableDataCellViewInfo_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function  Get_TcxGridTableItem_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Get_TcxGridItemDBDataBinding_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Call_TcxDataSummary_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Call_TcxDataSummaryItems_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TcxCustomTreeList_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    // procedure  Set_TcxCustomTreeList_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
    function  Call_TcxCustomTreeList_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TcxTreeListEditCellViewInfo_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Get_TcxTreeListNode_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TcxTreeListNode_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
    function  Call_TcxTreeListNode_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Call_TcxDBTreeList_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Call_TcxDBTreeListDataController_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Get_TcxDBTreeListNode_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Get_TcxTreeListColumn_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TcxTreeListColumn_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Call_TcxGridLevel_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Get_TcxGridLevel_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
//    procedure Set_TcxGridLevel_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function  Call_TcxGridDBDataController_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Call_TcxEditorPropertiesCollection_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;

    function  Call_TcxCustomEditorRowProperties_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Get_TcxCustomEditorRowProperties_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TcxCustomEditorRowProperties_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function Get_TcxCustomVerticalGrid_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;

    function  Call_TcxCustomRow_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function  Get_TcxCustomRow_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    procedure Set_TcxCustomRow_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);

    function Get_TcxRowValueInfo_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
    function Call_TFlexCelGrid_Method(Instance: TObject; ClassType: TClass;
      const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

  TfsCustomGridTableItemGetPropertiesEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
    function GetMethod: Pointer; override;
  end;

  TfsCxGridViewCellClickEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsCxGridGetDataTextEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomGridTableItem; ARecordIndex: Integer; var AText: string);
    function GetMethod: Pointer; override;
  end;

  TfsCxGridGetDisplayTextEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: string);
    function GetMethod: Pointer; override;
  end;

{
  TfsCxGridGetFilterDisplayTextEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomGridTableItem; const AValue: Variant; var ADisplayText: string);
    function GetMethod: Pointer; override;
  end;

  TfsCxGridGetFilterValuesEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomGridTableItem; AValueList: TcxDataFilterValueList);
    function GetMethod: Pointer; override;
  end;
}

  TfsCxGridGetCellStyleEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
    function GetMethod: Pointer; override;
  end;

  TfsCxGridTableViewInitEditValueEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem;
         AEdit: TcxCustomEdit; var AValue: Variant);
    function GetMethod: Pointer; override;
  end;

  TfsCxGridGetGroupStyleEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxGridTableView; ARecord: TcxCustomGridRecord;
        ALevel: Integer; var AStyle: TcxStyle);
    function GetMethod: Pointer; override;
  end;

  TfsCxGridGetPropertiesEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
    function GetMethod: Pointer; override;
  end;

  TfsCxGridTableDataCellCustomDrawEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsCxDBDataDetailHasChildrenEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxDBDataController; ARecordIndex, ARelationIndex: Integer; const AMasterDetailKeyFieldNames: string; const AMasterDetailKeyValues: Variant; var HasChildren: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsCxGridColumnEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxGridTableView; AColumn: TcxGridColumn);
    function GetMethod: Pointer; override;
  end;

  TfsCxSummaryEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(ASender: TcxDataSummaryItems; Arguments: TcxSummaryEventArguments; var OutArguments: TcxSummaryEventOutArguments);
    function GetMethod: Pointer; override;
  end;

  TfsCxCustomDrawEditCellViewInfoEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsTreeListGetNodeImageIndexEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject; ANode: TcxTreeListNode; AIndexType: TcxTreeListImageIndexType; var AIndex: TImageIndex);
    function GetMethod: Pointer; override;
  end;

  TfsTreeListFocusedChangedEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
    function GetMethod: Pointer; override;
  end;

  TfsTreeListNodeChangingEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsTreeListNodeChangedEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomTreeList; ANode: TcxTreeListNode);
    function GetMethod: Pointer; override;
  end;

  TfsTreeListColumnChangedEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
    function GetMethod: Pointer; override;
  end;

  TfsTreeListColumnGetEditPropertiesEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
    function GetMethod: Pointer; override;
  end;

  TfsTreeListColumnGetEditingPropertiesEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject; AData: Pointer; var AEditProperties: TcxCustomEditProperties);
    function GetMethod: Pointer; override;
  end;

  TfsTreeListColumnGetDisplayTextEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxTreeListColumn;  ANode: TcxTreeListNode; var Value: string);
    function GetMethod: Pointer; override;
  end;

  TfsTreeListNodeAssignValuesEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomDBTreeList; ANode: TcxTreeListNode);
    function GetMethod: Pointer; override;
  end;

  TfsTreeListOnIsGroupNode = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomTreeList;ANode: TcxTreeListNode; var IsGroup: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsCxGridViewDblClickEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject);
    function GetMethod: Pointer; override;
  end;

  TfsDBTableViewFocusedChangedEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsVerticalGridOnDrawValueEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TObject; ACanvas: TcxCanvas; APainter: TcxvgPainter; AValueInfo: TcxRowValueInfo; var Done: Boolean);
    function GetMethod: Pointer; override;
  end;

  TfsVerticalGridGetEditPropertiesEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer; var AProperties: TcxCustomEditProperties);
    function GetMethod: Pointer; override;
  end;

  TfsEditorRowStylesOnGetContentParamsEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(AEditorRowProperties: TcxCustomEditorRowProperties; AFocused: Boolean; ARecordIndex: Integer; var AParams: TcxViewParams);
    function GetMethod: Pointer; override;
  end;

  TfsEditorRowPropertiesOnGetContentParams = class(TfsCustomEvent)
  public
    procedure DoEvent(AValueInfo: TcxRowValueInfo; ACanvas: TcxCanvas);
    function GetMethod: Pointer; override;
  end;

  TfsCustomEditorRowPropertiesValidateDrawValueEvent = class(TfsCustomEvent)
  public
    procedure DoEvent(Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer; const AValue: Variant; AData: TcxEditValidateInfo);
    function GetMethod: Pointer; override;
  end;

type
  TByteSet = set of 0..7;
  PByteSet = ^TByteSet;
  TcxTreeListNodeCrack = class(TcxTreeListNode);

{ TFunctions_TcxGrid }

constructor TFunctions_TcxGrid.Create(AScript: TfsScript);
var
  AClass: TClass;
begin
  inherited Create(AScript);

//  AScript.AddMethod('procedure DrawIcon(AViewInfo: TcxGridTableDataCellViewInfo; ACanvas: TcxCanvas; AIcon: TBitmap)', Call_TcxRichEdit_Method);

  AScript.AddEnum('TcxGridGroupFootersMode', 'gfInvisible, gfVisibleWhenExpanded, gfAlwaysVisible');
  AScript.AddEnum('TcxTreeListImageIndexType', 'tlitImageIndex, tlitSelectedIndex, tlitStateIndex');
  AScript.AddEnum('TcxLookAndFeelKind', 'lfFlat, lfStandard, lfUltraFlat, lfOffice11');
  AScript.AddEnum('TcxFilterOperatorKind', 'foEqual, foNotEqual, foLess, foLessEqual, ' +
    'foGreater, foGreaterEqual, foLike, foNotLike, foBetween, foNotBetween, ' +
    'foInList, foNotInList, ' +
    'foYesterday, foToday, foTomorrow, ' +
    'foLast7Days, foLastWeek, foLast14Days, foLastTwoWeeks, foLast30Days, foLastMonth, foLastYear, foInPast, ' +
    'foThisWeek, foThisMonth, foThisYear, ' +
    'foNext7Days, foNextWeek, foNext14Days, foNextTwoWeeks, foNext30Days, foNextMonth, foNextYear, foInFuture');

  AScript.AddEnum('TcxEditValidationOptions', 'evoRaiseException, evoShowErrorIcon, evoAllowLoseFocus');

  AScript.AddEnum('TcxSummaryKind', 'skNone, skSum, skMin, skMax, skCount, skAverage');

  AScript.AddMethod('function ExportCxGridToExcel(AGrid: TcxGrid; const AFileName: String = ''''; const ATitle: String = ''''; AUseNativeFormat: Boolean = True): Boolean', Call_Method);
  AScript.AddMethod('function ExportCxTreeToExcel(ATree: TcxDBTreeList; const AFileName: String = ''''; AUseNativeFormat: Boolean = True): Boolean', Call_Method);

  with AScript.AddClass(TcxGridColumn, TcxGridColumn.ClassParent.ClassName) do begin
    AddEvent('OnHeaderClick', TfsNotifyEvent);
  end;

  AScript.AddClass(TcxGridDBColumn, TcxGridDBColumn.ClassParent.ClassName);
  AScript.AddClass(TcxGridDBBandedColumn, TcxGridDBBandedColumn.ClassParent.ClassName);

  with AScript.AddClass(TcxGridTableEditingController, TcxGridTableEditingController.ClassParent.ClassName) do begin
  end;


  with AScript.AddClass(TcxCustomGridTableController, TcxCustomGridTableController.ClassParent.ClassName) do begin
    AddProperty('FocusedColumn', 'TcxGridColumn', Get_TcxGridTableController_Property);
    AddProperty('FocusedColumnIndex', DelphiTypeInteger, Get_TcxGridTableController_Property);

    AddProperty('FocusedRow', 'TcxCustomGridRow', Get_TcxGridTableController_Property);
    AddProperty('FocusedRowIndex', DelphiTypeInteger, Get_TcxGridTableController_Property, Set_TcxGridTableController_Property);

    AddProperty('FocusedRecord', 'TcxCustomGridRow', Get_TcxGridTableController_Property);
    AddProperty('FocusedRecordIndex', DelphiTypeInteger, Get_TcxGridTableController_Property, Set_TcxGridTableController_Property);
    AddProperty('TopRecordIndex', DelphiTypeInteger, Get_TcxGridTableController_Property);
    AddProperty('TopRowIndex', DelphiTypeInteger, Get_TcxGridTableController_Property);

    AddProperty('SelectedColumnCount', DelphiTypeInteger, Get_TcxGridTableController_Property);
    AddProperty('SelectedRowCount', DelphiTypeInteger, Get_TcxGridTableController_Property);

    AddIndexProperty('SelectedColumns', DelphiTypeInteger, 'TcxGridColumn', Call_TcxGridTableController_Method, False);
    AddIndexProperty('SelectedRows', DelphiTypeInteger, 'TcxCustomGridRow', Call_TcxGridTableController_Method, False);

    AddProperty('EditingController', 'TcxGridTableEditingController', Get_TcxGridTableController_Property);
  end;

  with AScript.AddClass(TcxCustomGridTableItem, TcxCustomGridTableItem.ClassParent.ClassName) do begin
    AddProperty('Focused', DelphiTypeBoolean, Get_TcxCustomGridTableItem_Property);
    AddProperty('PropertiesClassName', DelphiTypeString, Get_TcxCustomGridTableItem_Property, Set_TcxCustomGridTableItem_Property);
    AddProperty('PropertiesClass', 'TcxCustomEditPropertiesClass', Get_TcxCustomGridTableItem_Property, Set_TcxCustomGridTableItem_Property);
    AddProperty('Properties', 'TcxCustomEditProperties', Get_TcxCustomGridTableItem_Property, Set_TcxCustomGridTableItem_Property);
    AddProperty(SConst_EditValue, DelphiTypeVariant, Get_TcxCustomGridTableItem_Property, Set_TcxCustomGridTableItem_Property);
    AddEvent('OnGetPropertiesForEdit', TfsCustomGridTableItemGetPropertiesEvent);
    AddEvent('OnGetProperties', TfsCustomGridTableItemGetPropertiesEvent);
  end;

  with AScript.AddClass(TcxGridEditingController, TcxGridEditingController.ClassParent.ClassName) do begin
    AddMethod('procedure ShowEdit(AItem: TcxCustomGridTableItem = nil)', Call_TcxGridEditingController_Method);
    AddMethod('procedure HideEdit(Accept: Boolean)', Call_TcxGridEditingController_Method);
  end;


  with AScript.AddClass(TcxCustomGridTableView, TcxCustomGridTableView.ClassParent.ClassName) do begin
    AddMethod('procedure RefreshDataSet', Call_TcxCustomGridTableView_Method);

    AddProperty('Controller', 'TcxGridTableController', Get_TcxCustomGridTableView_Property);
    AddProperty('ViewData', 'TcxGridViewData', Get_TcxCustomGridTableView_Property);

    AddEvent('OnDblClick', TfsCxGridViewDblClickEvent);

    AddEvent('OnCellClick', TfsCxGridViewCellClickEvent);
    AddEvent('OnCustomDrawCell', TfsCxGridTableDataCellCustomDrawEvent);
    AddEvent('OnInitEditValue', TfsCxGridTableViewInitEditValueEvent);
  end;

  with AScript.AddClass(TcxGridTableView, TcxGridTableView.ClassParent.ClassName) do begin
    AddProperty('ViewInfo', 'TcxGridTableViewInfo', Get_TcxGridTableView_Property);

    AddIndexProperty('Columns', DelphiTypeInteger, 'TcxGridBandedColumn', Call_TcxGridTableView_Method, True);
    AddProperty('ColumnCount', DelphiTypeInteger, Get_TcxGridTableView_Property);

    AddIndexProperty('GroupedColumns', DelphiTypeInteger, 'TcxGridColumn', Call_TcxGridTableView_Method, True);
    AddProperty('GroupedColumnCount', DelphiTypeInteger, Get_TcxGridTableView_Property);

    AddMethod('function FindItemByFieldName(const AFieldName: String): TcxCustomGridTableItem', Call_TcxGridTableView_Method);
    AddMethod('function GetViewInfoHeight: Integer', Call_TcxGridTableView_Method);
    AddMethod('function GetViewInfoWidth: Integer', Call_TcxGridTableView_Method);

    AddMethod('procedure AutoCreateColumns', Call_TcxGridDBTableView_Method);
    AddMethod('procedure ClearItems', Call_TcxGridTableView_Method);

    AddEvent('OnColumnSizeChanged', TfsCxGridColumnEvent);
    AddEvent('OnColumnPosChanged', TfsCxGridColumnEvent);
  end;

  with AScript.AddClass(TcxGridBands, TcxGridBands.ClassParent.ClassName) do begin
    AddDefaultProperty(SConst_Items, DelphiTypeInteger, 'TcxGridBand', Call_TcxGridBands_Method, True);
    AddProperty(SConst_Count, DelphiTypeInteger, Get_TcxGridBands_Property);
  end;

  with AScript.AddClass(TcxGridBandedTableView, TcxGridBandedTableView.ClassParent.ClassName) do begin
    AddIndexProperty(SConst_Columns, DelphiTypeInteger, 'TcxGridBandedColumn', Call_TcxGridTableView_Method, True);
    AddProperty('Bands', 'TcxGridBands', Get_TcxGridBandedTableView_Property);
  end;

  with AScript.AddClass(TcxGridDBBandedTableView, TcxGridDBBandedTableView.ClassParent.ClassName) do begin
    AddIndexProperty('Columns', DelphiTypeInteger, 'TcxGridDBBandedColumn', Call_TcxGridTableView_Method, True);
  end;

  with AScript.AddClass(TcxGridViewData, TcxGridViewData.ClassParent.ClassName) do begin
    AddIndexProperty('Rows', DelphiTypeInteger, 'TcxCustomGridRow', Call_TcxGridViewData_Method, True);
    AddProperty('EditingRecord', 'TcxCustomGridRecord', Get_TcxGridViewData_Property);
    AddMethod('procedure Expand(ARecurse: Boolean)', Call_TcxGridViewData_Method);
    AddMethod('procedure Collapse(ARecurse: Boolean)', Call_TcxGridViewData_Method);
  end;

  with AScript.AddClass(TcxCustomGridView, TcxCustomGridView.ClassParent.ClassName) do begin
    AddProperty('DataController', 'TcxCustomDataController', Get_TcxCustomGridView_Property);
    AddProperty('Level', 'TComponent', Get_TcxCustomGridView_Property);
    AddProperty('PatternGridView', 'TcxCustomGridView', Get_TcxCustomGridView_Property);

    AddMethod('procedure BeginUpdate', Call_TcxCustomGridView_Method);
    AddMethod('procedure EndUpdate', Call_TcxCustomGridView_Method);
  end;

  AScript.AddClass(TcxLookAndFeel, TcxLookAndFeel.ClassParent.ClassName);

  AScript.AddClass(TcxCustomGridTableOptionsBehavior, TcxCustomGridTableOptionsBehavior.ClassParent.ClassName);
  AScript.AddClass(TcxGridTableOptionsBehavior, TcxGridTableOptionsBehavior.ClassParent.ClassName);

  AScript.AddClass(TcxCustomGridTableOptionsSelection, TcxCustomGridTableOptionsSelection.ClassParent.ClassName);
  AScript.AddClass(TcxGridTableOptionsSelection, TcxGridTableOptionsSelection.ClassParent.ClassName);//??

  AScript.AddClass(TcxCustomLookupDBGrid, TcxCustomLookupDBGrid.ClassParent.ClassName);
  AScript.AddClass(TcxLookupDBGridColumn, TcxLookupDBGridColumn.ClassParent.ClassName);

  with AScript.AddClass(TcxLookupDBGridColumns, TcxLookupDBGridColumns.ClassParent.ClassName) do begin
    AddDefaultProperty(SConst_Items, DelphiTypeInteger, 'TcxLookupDBGridColumn', Call_TcxLookupDBGridColumn_Method, True);
  end;

  with AScript.AddClass(TcxGrid, TcxGrid.ClassParent.ClassName) do begin
    AddProperty('Levels', 'TcxGridLevel', Get_TcxGrid_Property);
    AddProperty('ActiveLevel', 'TcxGridLevel', Get_TcxGrid_Property, Set_TcxGrid_Property);
    AddProperty('ActiveView', 'TcxCustomGridView', Get_TcxGrid_Property);
    AddProperty('FocusedView', 'TcxCustomGridView', Get_TcxGrid_Property);
    AddProperty('LookAndFeel', 'TcxLookAndFeel', Get_TcxGrid_Property, Set_TcxGrid_Property);
    AddMethod('function CreateView(AViewClass: String): TcxCustomGridView', Call_TcxGrid_Method);
  end;

  with AScript.AddClass(TcxGridDBTableView, TcxGridDBTableView.ClassParent.ClassName) do begin
    AddMethod('function CreateColumn: TcxGridDBColumn', Call_TcxGridDBTableView_Method);

    AddEvent('OnFocusedRecordChanged', TfsDBTableViewFocusedChangedEvent);
    AddEvent('OnSelectionChanged', TfsNotifyEvent);
  end;
//  AScript.AddType('TClassName', fvtString);

  with AScript.AddClass(TcxGridLevel, TcxGridLevel.ClassParent.ClassName) do begin
    AddMethod('function Add: TcxGridLevel', Call_TcxGridLevel_Method);
    AddProperty('Control', 'TcxControl', Get_TcxGridLevel_Property, nil);
  end;

  with AScript.AddClass(TcxGridDBDataController, TcxGridDBDataController.ClassParent.ClassName) do begin
    AddMethod('procedure CreateAllItems', Call_TcxGridDBDataController_Method);
    AddMethod('function GetItemByFieldName(const AFieldName: string): TcxCustomGridTableItem', Call_TcxGridDBDataController_Method);

    AddEvent('OnGroupingChanged', TfsNotifyEvent);
  end;

  with AScript.AddClass(TcxCustomDataController, TcxCustomDataController.ClassParent.ClassName) do begin
    AddProperty('FilteredRecordCount', DelphiTypeInteger, Get_TcxDataController_Property);
    AddProperty('RecordCount', DelphiTypeInteger, Get_TcxDataController_Property);
//    AddProperty('Groups', 'TcxDataControllerGroups', Get_TcxDataController_Property);

    AddMethod('function GetSelectedCount: Integer', Call_TcxGridDBDataController_Method);
    AddMethod('function GetSelectedRowIndex(Index: Integer): Integer;', Call_TcxGridDBDataController_Method);

    AddEvent('OnDetailHasChildren', TfsCxDBDataDetailHasChildrenEvent);
{
    AddEvent('OnAfterPost', TfsNotifyEvent);
//    AddEvent('OnAfterCancel', TfsNotifyEvent);
    AddEvent('OnAfterInsert', TfsNotifyEvent);
    AddEvent('OnAfterDelete', TfsNotifyEvent);
}

    AddIndexProperty('Values', 'Integer,Integer', DelphiTypeVariant, Call_TcxCustomDataController_Method, False);
    AddIndexProperty('FilteredRecordIndex', DelphiTypeInteger, DelphiTypeInteger, Call_TcxCustomDataController_Method, False);

    AddMethod('procedure Insert', Call_TcxCustomDataController_Method);
    AddMethod('procedure Append', Call_TcxCustomDataController_Method);
    AddMethod('procedure Edit', Call_TcxCustomDataController_Method);
    AddMethod('procedure Post', Call_TcxCustomDataController_Method);
    AddMethod('procedure Cancel', Call_TcxCustomDataController_Method);

    AddMethod('procedure PostEditingData', Call_TcxCustomDataController_Method);
    AddMethod('function  AppendRecord: Integer', Call_TcxCustomDataController_Method);
    AddMethod('procedure DeleteRecord(ARecordIndex: Integer)', Call_TcxCustomDataController_Method);

    AddMethod('procedure GotoFirst', Call_TcxCustomDataController_Method);
    AddMethod('procedure GotoLast', Call_TcxCustomDataController_Method);
    AddMethod('procedure GotoNext', Call_TcxCustomDataController_Method);
    AddMethod('procedure GotoPrev', Call_TcxCustomDataController_Method);

    AddMethod('function IsEOF: Boolean', Call_TcxCustomDataController_Method);
    AddMethod('function IsBOF: Boolean', Call_TcxCustomDataController_Method);

    AddMethod('procedure BeginUpdate', Call_TcxCustomDataController_Method);
    AddMethod('procedure EndUpdate', Call_TcxCustomDataController_Method);

    AddProperty('FocusedRecordIndex', DelphiTypeInteger, Get_TcxDataController_Property, Set_TcxCustomDataController_Property);

    AddMethod('function GetRowIndexByRecordIndex(ARecordIndex: Integer; AMakeVisible: Boolean): Integer', Call_TcxCustomDataController_Method);
  end;

//  with AScript.AddClass(TcxDataControllerGroups, TcxDataControllerGroups.ClassParent.ClassName) do begin
//    AddMethod('procedure FullExpand', Call_TcxDataControllerGroups_Method);
//  end;

  AScript.AddClass(TcxFilterCriteriaItem, TcxFilterCriteriaItem.ClassParent.ClassName);

  with AScript.AddClass(TcxFilterCriteriaItemList, TcxFilterCriteriaItemList.ClassParent.ClassName) do begin
    AddMethod('function AddItem(AItemLink: TObject; AOperatorKind: TcxFilterOperatorKind; const AValue: Variant; const ADisplayValue: string): TcxFilterCriteriaItem', Call_TcxFilterCriteriaItemList_Method);
  end;

  with AScript.AddClass(TcxDataFilterCriteria, TcxDataFilterCriteria.ClassParent.ClassName) do begin
    AddProperty(SConst_Root, TcxFilterCriteriaItemList.ClassName, Get_TcxDataFilterCriteria_Property);
  end;

  AScript.AddClass(TcxDBDataFilterCriteria, TcxDBDataFilterCriteria.ClassParent.ClassName);

  with AScript.AddClass(TcxDBDataController, TcxDBDataController.ClassParent.ClassName) do begin
    AddProperty(SConst_RecNo, DelphiTypeInteger, Get_TcxDBDataController_Property, Set_TcxDBDataController_Property);
    AddProperty(SConst_DataSet, 'TDataSet', Get_TcxDBDataController_Property);
    AddProperty(SConst_Filter, TcxDBDataFilterCriteria.ClassName, Get_TcxDBDataController_Property, Set_TcxDBDataController_Property);

    AddMethod('function LocateByKey(const AKeyFieldValues: Variant): Boolean', Call_TcxDBDataController_Method);
  end;

  with AScript.AddClass(TcxCustomGridTableViewStyles, TcxCustomGridTableViewStyles.ClassParent.ClassName) do begin
    AddEvent('OnGetContentStyle', TfsCxGridGetCellStyleEvent);
  end;

  with AScript.AddClass(TcxGridTableViewStyles, TcxGridTableViewStyles.ClassParent.ClassName) do begin
    AddEvent('OnGetGroupStyle', TfsCxGridGetGroupStyleEvent);
  end;

  with AScript.AddClass(TcxCustomGridTableItemStyles, TcxCustomGridTableItemStyles.ClassParent.ClassName) do begin
    AddEvent('OnGetContentStyle', TfsCxGridGetCellStyleEvent);
  end;

  with AScript.AddClass(TcxGridTableCellViewInfo, TcxGridTableCellViewInfo.ClassParent.ClassName) do begin
    AddProperty('GridRecord', TcxCustomGridRecord.ClassName, Get_TcxGridTableCellViewInfo_Property);
    AddProperty(SConst_Selected, DelphiTypeBoolean, Get_TcxGridTableCellViewInfo_Property);
  end;

  with AScript.AddClass(TcxGridTableDataCellViewInfo, TcxGridTableDataCellViewInfo.ClassParent.ClassName) do begin
    AddProperty('DisplayValue', 'TcxEditValue', Get_TcxGridTableDataCellViewInfo_Property);
    AddProperty(SConst_Item, TcxCustomGridTableItem.ClassName, Get_TcxGridTableDataCellViewInfo_Property);
    AddProperty('Properties', TcxCustomEditProperties.ClassName, Get_TcxGridTableDataCellViewInfo_Property);
    AddProperty('Style', TcxEditStyle.ClassName, Get_TcxGridTableDataCellViewInfo_Property);
    AddProperty(SConst_Value, DelphiTypeVariant, Get_TcxGridTableDataCellViewInfo_Property);
    AddProperty(SConst_Focused, DelphiTypeBoolean, Get_TcxGridTableDataCellViewInfo_Property);
  end;

  with AScript.AddClass(TcxCustomGridTableItem, TcxCustomGridTableItem.ClassParent.ClassName) do begin
    AddProperty(SConst_Id, DelphiTypeInteger, Get_TcxGridTableItem_Property);
    AddProperty(SConst_Index, DelphiTypeInteger, Get_TcxGridTableItem_Property);
    AddProperty(SConst_Options, TcxCustomGridTableItemOptions.ClassName, Get_TcxGridTableItem_Property);
    AddProperty('VisibleIndex', DelphiTypeInteger, Get_TcxGridTableItem_Property);

    AddEvent('OnGetDataText', TfsCxGridGetDataTextEvent);
    AddEvent('OnGetDisplayText', TfscxGridGetDisplayTextEvent);
    AddEvent('OnCustomDrawCell', TfsCxGridTableDataCellCustomDrawEvent);
    AddEvent('OnGetProperties', TfsCxGridGetPropertiesEvent);

{
    AddEvent('OnGetFilterDisplayText', TcxGridGetFilterDisplayTextEvent);
    AddEvent('OnGetFilterValues', TcxGridGetFilterValuesEvent);
}
  end;

  with AScript.AddClass(TcxCustomGridRecord, TcxCustomGridRecord.ClassParent.ClassName) do begin
    AddIndexProperty(SConst_Values, DelphiTypeInteger, DelphiTypeVariant, Call_TcxCustomGridRecord_Method);
    AddIndexProperty('DisplayTexts', DelphiTypeInteger, DelphiTypeString, Call_TcxCustomGridRecord_Method);

    AddProperty(SConst_Level, DelphiTypeInteger, Get_TcxCustomGridRecord_Property);
    AddProperty(SConst_Index, DelphiTypeInteger, Get_TcxCustomGridRecord_Property);
  end;

  with AScript.AddClass(TcxCustomGridRow, TcxCustomGridRow.ClassParent.ClassName) do begin
    AddIndexProperty(SConst_Values, DelphiTypeInteger, DelphiTypeVariant, Call_TcxCustomGridRow_Method, False);
    AddMethod('procedure Expand(ARecurse: Boolean)', Call_TcxCustomGridRow_Method);
  end;

  with AScript.AddClass(TcxGridGroupRow, TcxGridGroupRow.ClassParent.ClassName) do begin
  end;

  AScript.AddClass(TcxGridDataRow, TcxGridDataRow.ClassParent.ClassName);

  with AScript.AddClass(TcxGridItemDBDataBinding, TcxGridItemDBDataBinding.ClassParent.ClassName) do begin
    AddProperty('Field', 'TField', Get_TcxGridItemDBDataBinding_Property);
  end;

  with AScript.AddClass(TcxDataSummary, TcxDataSummary.ClassParent.ClassName) do begin
    AddIndexProperty('FooterSummaryValuesByName', DelphiTypeString, DelphiTypeVariant, Call_TcxDataSummary_Method, False);
    AddIndexProperty('FooterSummaryValues', DelphiTypeInteger, DelphiTypeVariant, Call_TcxDataSummary_Method, False);
    AddIndexProperty('FooterSummaryTexts', DelphiTypeInteger, DelphiTypeString, Call_TcxDataSummary_Method, True);
    AddIndexProperty('GroupFooterSummaryTexts', 'Integer,Integer,Integer', DelphiTypeString, Call_TcxDataSummary_Method, True);

    AddEvent('OnAfterSummary', TfsNotifyEvent);
  end;

  with AScript.AddRecordType('TcxSummaryEventArguments') do begin
    AddProperty('RecordIndex', DelphiTypeInteger, Word(@PcxSummaryEventArguments(nil).RecordIndex));
    AddProperty('SummaryItem', 'TcxDataSummaryItem', Word(@PcxSummaryEventArguments(nil).SummaryItem));
  end;

  with AScript.AddRecordType('TcxSummaryEventOutArguments') do begin
    AddProperty(SConst_Value, DelphiTypeVariant, Word(@PcxSummaryEventOutArguments(nil).Value));
    AddProperty('SummaryValue', DelphiTypeVariant, Word(@PcxSummaryEventOutArguments(nil).SummaryValue));
    AddProperty('CountValue', DelphiTypeInteger, Word(@PcxSummaryEventOutArguments(nil).CountValue));
    AddProperty('Done', DelphiTypeBoolean, Word(@PcxSummaryEventOutArguments(nil).Done));
  end;

  with AScript.AddClass(TcxDataSummaryItems, TcxDataSummaryItems.ClassParent.ClassName) do begin
    AddMethod('function IndexOfItemLink(ItemLink: TObject): Integer', Call_TcxDataSummaryItems_Method);
    AddMethod('function ItemOfItemLink(ItemLink: TObject): TcxDataSummaryItem', Call_TcxDataSummaryItems_Method);

    AddEvent('OnSummary', TfsCxSummaryEvent);
  end;

  { CxTreeList }

  with AScript.AddClass(TcxCustomTreeList, TcxCustomTreeList.ClassParent.ClassName) do begin
    AddProperty(SConst_Root, TcxTreeListNode.ClassName, Get_TcxCustomTreeList_Property);
    AddProperty('FocusedColumn', TcxTreeListColumn.ClassName, Get_TcxCustomTreeList_Property);
    AddProperty('FocusedNode', TcxTreeListNode.ClassName, Get_TcxCustomTreeList_Property);

    AddProperty('Count', DelphiTypeInteger, Get_TcxCustomTreeList_Property);
    AddIndexProperty(SConst_Items, DelphiTypeInteger, 'TcxTreeListNode', Call_TcxCustomTreeList_Method, False);

    AddProperty('SelectionCount', DelphiTypeInteger, Get_TcxCustomTreeList_Property);
    AddIndexProperty('Selections', DelphiTypeInteger, TcxTreeListNode.ClassName, Call_TcxCustomTreeList_Method, False);

    AddProperty('DragNode', 'TcxTreeListNode', Get_TcxCustomTreeList_Property);
    AddMethod('function GetNodeAt(X, Y: Integer): TcxTreeListNode', Call_TcxCustomTreeList_Method);

    AddMethod('procedure FullCollapse', Call_TcxCustomTreeList_Method);
    AddMethod('procedure FullExpand', Call_TcxCustomTreeList_Method);
    AddMethod('procedure DeleteSelection', Call_TcxCustomTreeList_Method);

    AddEvent('OnCustomDrawDataCell', TfsCxCustomDrawEditCellViewInfoEvent);
    AddEvent('OnGetNodeImageIndex', TfsTreeListGetNodeImageIndexEvent);
    AddEvent('OnFocusedNodeChanged', TfsTreeListFocusedChangedEvent);
    AddEvent('OnColumnHeaderClick', TfsTreeListColumnChangedEvent);
    AddEvent('OnColumnSizeChanged', TfsTreeListColumnChangedEvent);
    AddEvent('OnColumnPosChanged', TfsTreeListColumnChangedEvent);
    AddEvent('OnExpanding', TfsTreeListNodeChangingEvent);
    AddEvent('OnExpanded', TfsTreeListNodeChangedEvent);
    AddEvent('OnCollapsing', TfsTreeListNodeChangingEvent);
    AddEvent('OnCollapsed', TfsTreeListNodeChangedEvent);
    AddEvent('OnNodeAssignValues', TfsTreeListNodeAssignValuesEvent);
    AddEvent('OnIsGroupNode', TfsTreeListOnIsGroupNode);
  end;

  with AScript.AddClass(TcxTreeListEditCellViewInfo, TcxTreeListEditCellViewInfo.ClassParent.ClassName) do begin
    AddProperty(SConst_Column, 'TcxTreeListColumn', Get_TcxTreeListEditCellViewInfo_Property);
    AddProperty('Band', 'TcxTreeListBand', Get_TcxTreeListEditCellViewInfo_Property);
    AddProperty(SConst_Node, 'TcxTreeListNode', Get_TcxTreeListEditCellViewInfo_Property);
    AddProperty(SConst_Selected, DelphiTypeBoolean, Get_TcxTreeListEditCellViewInfo_Property);
  end;

  AScript.AddEnum('TcxTreeListNodeCheckGroupType', 'ncgNone,ncgCheckGroup,ncgRadioGroup');

  with AScript.AddClass(TcxTreeListNode, TcxTreeListNode.ClassParent.ClassName) do begin
    AddProperty('IsRoot', DelphiTypeBoolean, Get_TcxTreeListNode_Property);
    AddProperty(SConst_Count, DelphiTypeInteger, Get_TcxTreeListNode_Property);
    AddProperty(SConst_Data, 'Pointer', Get_TcxTreeListNode_Property);
    AddProperty(SConst_Focused, DelphiTypeBoolean, Get_TcxTreeListNode_Property);
    AddProperty('HasChildren', DelphiTypeBoolean, Get_TcxTreeListNode_Property);
    AddProperty(SConst_Index, DelphiTypeInteger, Get_TcxTreeListNode_Property);
    AddIndexProperty(SConst_Items, DelphiTypeInteger, TcxTreeListNode.ClassName, Call_TcxTreeListNode_Method, False);
    AddProperty(SConst_Level, DelphiTypeInteger, Get_TcxTreeListNode_Property);
    AddProperty(SConst_Parent, TcxTreeListNode.ClassName, Get_TcxTreeListNode_Property);
//    AddProperty('RecordIndex', DelphiTypeInteger, Get_TcxTreeListNode_Property);
    AddProperty('Selected', DelphiTypeBoolean, Get_TcxTreeListNode_Property, Set_TcxTreeListNode_Property);
    AddIndexProperty('Texts', DelphiTypeInteger, DelphiTypeString, Call_TcxTreeListNode_Method, False);
    AddProperty('ValueCount', DelphiTypeInteger, Get_TcxTreeListNode_Property);
    AddIndexProperty(SConst_Values, DelphiTypeInteger, DelphiTypeVariant, Call_TcxTreeListNode_Method, False);
    AddProperty('VisibleIndex', DelphiTypeInteger, Get_TcxTreeListNode_Property);
    AddProperty('CheckGroupType', 'TcxTreeListNodeCheckGroupType', Get_TcxTreeListNode_Property, Set_TcxTreeListNode_Property);
    AddProperty('CheckState', 'TcxCheckBoxState', Get_TcxTreeListNode_Property, Set_TcxTreeListNode_Property);
    AddProperty('TreeList', 'TcxCustomTreeList', Get_TcxTreeListNode_Property);

    AddMethod('procedure Expand(AResursive: Boolean)', Call_TcxTreeListNode_Method);
    AddMethod('procedure Collapse(AResursive: Boolean)', Call_TcxTreeListNode_Method);
    AddMethod('procedure SetCheckGroupType(AValue: TcxTreeListNodeCheckGroupType)', Call_TcxTreeListNode_Method);
    AddMethod('function GetFirstChild: TcxTreeListNode', Call_TcxTreeListNode_Method);
    AddMethod('function GetNextChild(Node: TcxTreeListNode): TcxTreeListNode', Call_TcxTreeListNode_Method);
  end;

  with AScript.AddClass(TcxTreeListColumn, TcxTreeListColumn.ClassParent.ClassName) do begin
    AddProperty(SConst_ItemIndex, DelphiTypeInteger, Get_TcxTreeListColumn_Property);
    AddProperty('PropertiesClassName', DelphiTypeString, Get_TcxTreeListColumn_Property, Set_TcxTreeListColumn_Property);
    AddProperty('PropertiesClass', TcxCustomEditPropertiesClass.ClassName, Get_TcxTreeListColumn_Property, Set_TcxTreeListColumn_Property);
    AddProperty('Properties', TcxCustomEditProperties.ClassName, Get_TcxTreeListColumn_Property, Set_TcxTreeListColumn_Property);
    AddProperty(SConst_EditValue, DelphiTypeVariant, Get_TcxTreeListColumn_Property, Set_TcxTreeListColumn_Property);
    AddEvent('OnGetEditProperties', TfsTreeListColumnGetEditPropertiesEvent);
    AddEvent('OnGetEditingProperties', TfsTreeListColumnGetEditingPropertiesEvent);
    AddEvent('OnGetDisplayText', TfsTreeListColumnGetDisplayTextEvent);
  end;

  with AScript.AddClass(TcxDBTreeList, TcxDBTreeList.ClassParent.ClassName) do begin
    AddIndexProperty('FocusedRecordValues', DelphiTypeString, DelphiTypeVariant, Call_TcxDBTreeList_Method);
  end;

  with AScript.AddClass(TcxDBTreeListDataController, TcxDBTreeListDataController.ClassParent.ClassName) do begin
    AddMethod('function LocateByKey(const AKeyValue: Variant): Boolean', Call_TcxDBTreeListDataController_Method);
  end;

  with AScript.AddClass(TcxDBTreeListNode, TcxDBTreeListNode.ClassParent.ClassName) do begin
    AddProperty(SConst_Parent, TcxDBTreeListNode.ClassName, Get_TcxTreeListNode_Property); // Get_TcxTreeListNode_Property = это не ошибка
    AddProperty(SConst_KeyValue, DelphiTypeVariant, Get_TcxDBTreeListNode_Property);
    AddProperty('ParentKeyValue', DelphiTypeVariant, Get_TcxDBTreeListNode_Property);
    AddIndexProperty(SConst_Items, DelphiTypeInteger, TcxDBTreeListNode.ClassName, Call_TcxTreeListNode_Method, False); // Call_TcxTreeListNode_Method = Это не ошибка!
  end;

  AScript.AddClass(TcxDBTreeListColumn, TcxDBTreeListColumn.ClassParent.ClassName);

  AScript.AddClass(TcxEditValidateInfo, TcxEditValidateInfo.ClassParent.ClassName);
  // ---------------------------------------
  // Поддержка VerticalGrid
  // ---------------------------------------
  with AScript.AddClass(TcxCustomVerticalGrid, TcxCustomVerticalGrid.ClassParent.ClassName) do begin
    AddEvent('OnExpandOrCollapse', TfsNotifyEvent);
    AddEvent('OnDrawValue', TfsVerticalGridOnDrawValueEvent);
    AddProperty(SConst_RecordCount, DelphiTypeInteger, Get_TcxCustomVerticalGrid_Property);
  end;

  with AScript.AddClass(TcxEditorPropertiesCollection, TcxEditorPropertiesCollection.ClassParent.ClassName) do begin
    AddDefaultProperty(SConst_Items, DelphiTypeInteger, TcxDBEditorRowItemProperties.ClassName, Call_TcxEditorPropertiesCollection_Method);
  end;
  AScript.AddClass(TcxDBEditorPropertiesCollection, TcxDBEditorPropertiesCollection.ClassParent.ClassName);

  with AScript.AddClass(TcxCustomRow, TcxCustomRow.ClassParent.ClassName) do begin
    AddProperty('VerticalGrid', TcxCustomVerticalGrid.ClassName, Get_TcxCustomRow_Property, Set_TcxCustomRow_Property);
    AddProperty(SConst_Parent, TcxCustomRow.ClassName, Get_TcxCustomRow_Property, Set_TcxCustomRow_Property);
    AddProperty(SConst_Count, DelphiTypeInteger, Get_TcxCustomRow_Property);
    AddProperty(SConst_Index, DelphiTypeInteger, Get_TcxCustomRow_Property);
    AddProperty(SConst_Styles, TcxvgCustomRowStyles.ClassName, Get_TcxCustomRow_Property);
    AddIndexProperty('Rows', DelphiTypeInteger, TcxCustomRow.ClassName, Call_TcxCustomRow_Method);
    AddMethod('function HasChildren: Boolean', Call_TcxCustomRow_Method);
    AddMethod('function HasParent: Boolean', Call_TcxCustomRow_Method);
    AddMethod('function IndexOf(ARow: TcxCustomRow): Integer', Call_TcxCustomRow_Method);
    AddMethod('function IsChild(ARow: TcxCustomRow): Boolean', Call_TcxCustomRow_Method);
  end;

  AScript.AddClass(TcxCustomEditorRow, TcxCustomEditorRow.ClassParent.ClassName);
  AScript.AddClass(TcxEditorRow, TcxEditorRow.ClassParent.ClassName);
  AScript.AddClass(TcxDBEditorRow, TcxDBEditorRow.ClassParent.ClassName);

  AScript.AddClass(TcxCustomMultiEditorRow, TcxCustomMultiEditorRow.ClassParent.ClassName);
  AScript.AddClass(TcxMultiEditorRow, TcxMultiEditorRow.ClassParent.ClassName);
  AScript.AddClass(TcxDBMultiEditorRow, TcxDBMultiEditorRow.ClassParent.ClassName);

  AScript.AddClass(TcxCategoryRow, TcxCategoryRow.ClassParent.ClassName);

  with AScript.AddClass(TcxCustomEditorRowProperties, TcxCustomEditorRowProperties.ClassParent.ClassName) do begin
    AddProperty(SConst_Value, DelphiTypeVariant, Get_TcxCustomEditorRowProperties_Property, Set_TcxCustomEditorRowProperties_Property);
    AddIndexProperty(SConst_Values, DelphiTypeInteger, DelphiTypeVariant, Call_TcxCustomEditorRowProperties_Method);
    AddEvent('OnValidateDrawValue', TfsCustomEditorRowPropertiesValidateDrawValueEvent);
  end;

  AScript.AddClass(TcxMultiEditorRowProperties, TcxMultiEditorRowProperties.ClassParent.ClassName);
  with AScript.AddClass(TcxEditorRowProperties, TcxEditorRowProperties.ClassParent.ClassName) do begin
    AddEvent('OnGetEditProperties', TfsVerticalGridGetEditPropertiesEvent);
    AddEvent('OnGetEditingProperties', TfsVerticalGridGetEditPropertiesEvent);
  end;
  with AScript.AddClass(TcxDBEditorRowItemProperties, TcxDBEditorRowItemProperties.ClassParent.ClassName) do begin
    AddEvent('OnGetEditProperties', TfsVerticalGridGetEditPropertiesEvent);
    AddEvent('OnGetEditingProperties', TfsVerticalGridGetEditPropertiesEvent);
  end;
  with AScript.AddClass(TcxDBEditorRowProperties, TcxDBEditorRowProperties.ClassParent.ClassName) do begin
    AddEvent('OnGetEditProperties', TfsVerticalGridGetEditPropertiesEvent);
    AddEvent('OnGetEditingProperties', TfsVerticalGridGetEditPropertiesEvent);
  end;

  AScript.AddClass(TcxEditorRowPropertiesOptions, TcxEditorRowPropertiesOptions.ClassParent.ClassName);
  AScript.AddClass(TcxMultiEditorRowPropertiesOptions, TcxMultiEditorRowPropertiesOptions.ClassParent.ClassName);

  AScript.AddEnum('TcxEditItemShowEditButtons', 'eisbDefault, eisbNever, eisbAlways');
  AScript.AddEnum('TcxEditingControlEditShowButton', 'ecsbAlways, ecsbFocused, ecsbNever');

  AScript.AddClass(TcxvgPainter, TcxvgPainter.ClassParent.ClassName);
  with AScript.AddClass(TcxRowValueInfo, TcxRowValueInfo.ClassParent.ClassName) do begin
    AddProperty(SConst_Focused, DelphiTypeBoolean, Get_TcxRowValueInfo_Property);
    AddProperty(SConst_Row, 'TcxCustomRow', Get_TcxRowValueInfo_Property);
    AddProperty('RowCellIndex', DelphiTypeInteger, Get_TcxRowValueInfo_Property);
  end;

  with AScript.AddRecordType('TcxViewParams') do begin
    AddProperty('Bitmap', TBitmap.ClassName, Word(@PcxViewParams(nil).Bitmap));
    AddProperty(SConst_Color, DelphiTypeInteger, Word(@PcxViewParams(nil).Color));
    AddProperty(SConst_Font, TFont.ClassName, Word(@PcxViewParams(nil).Font));
    AddProperty('TextColor', DelphiTypeInteger, Word(@PcxViewParams(nil).TextColor));
  end;

  AScript.AddClass(TcxvgCustomRowStyles, TcxvgCustomRowStyles.ClassParent.ClassName);
  AScript.AddClass(TcxCategoryRowStyles, TcxCategoryRowStyles.ClassParent.ClassName);
  with AScript.AddClass(TcxEditorRowStyles, TcxEditorRowStyles.ClassParent.ClassName) do begin
    AddEvent('OnGetContentParams', TfsEditorRowStylesOnGetContentParamsEvent);
  end;

  with AScript.AddClass(TcxCollectionItemEditorRowProperties, TcxCollectionItemEditorRowProperties.ClassParent.ClassName) do begin
    AddEvent('OnGetContentParams', TfsEditorRowPropertiesOnGetContentParams);
  end;

  with AScript.AddClass(TFlexCelGrid, TFlexCelGrid.ClassParent.ClassName) do begin
    AddMethod('procedure LoadSheet', Call_TFlexCelGrid_Method);
  end;
end;

function TFunctions_TcxGrid.Call_TcxGridTableController_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxGridTableController);
{$ENDIF}
  if MethodName = 'SELECTEDCOLUMNS.GET' then
    Result := Integer(TcxGridTableController(Instance).SelectedColumns[Caller.Params[0]])
  else if MethodName = 'SELECTEDROWS.GET' then
    Result := Integer(TcxGridTableController(Instance).SelectedRows[Caller.Params[0]])
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxGridEditingController_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxGridEditingController);
{$ENDIF}
  if MethodName = 'SHOWEDIT' then
    TcxGridEditingController(Instance).ShowEdit(TcxCustomGridTableItem(Integer(Caller.Params[0])))
  else if MethodName = 'HIDEEDIT' then
    TcxGridEditingController(Instance).HideEdit(Caller.Params[0] = True)
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxGridTableController_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxGridTableController);
{$ENDIF}
  if PropName = 'FOCUSEDCOLUMN' then
    Result := Integer(TcxGridTableController(Instance).FocusedColumn)
  else if PropName = 'FOCUSEDCOLUMNINDEX' then
    Result := TcxGridTableController(Instance).FocusedColumnIndex
  else if PropName = 'SELECTEDCOLUMNCOUNT' then
    Result := TcxGridTableController(Instance).SelectedColumnCount

  else if PropName = 'FOCUSEDROW' then
    Result := Integer(TcxGridTableController(Instance).FocusedRow)
  else if PropName = 'FOCUSEDROWINDEX' then
    Result := TcxGridTableController(Instance).FocusedRowIndex
  else if PropName = 'SELECTEDROWCOUNT' then
    Result := TcxGridTableController(Instance).SelectedRowCount

  else if PropName = 'FOCUSEDRECORD' then
    Result := Integer(TcxGridTableController(Instance).FocusedRecord)
  else if PropName = 'FOCUSEDRECORDINDEX' then
    Result := TcxGridTableController(Instance).FocusedRecordIndex
  else if PropName = 'TOPRECORDINDEX' then
    Result := TcxGridTableController(Instance).TopRecordIndex
  else if PropName = 'TOPROWINDEX' then
    Result := TcxGridTableController(Instance).TopRowIndex
  else if PropName = 'SELECTEDRECORDCOUNT' then
    Result := TcxGridTableController(Instance).SelectedRecordCount

  else if PropName = 'EDITINGCONTROLLER' then
    Result := Integer(TcxGridTableController(Instance).EditingController)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

(*
procedure TFunctions_TcxGrid.Set_TcxGridLevel_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxGridLevel);
{$ENDIF}
  if PropName = 'CONTROL' then
    TcxGridLevel(Instance).Control := VarToPointer(Value)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;
*)

procedure TFunctions_TcxGrid.Set_TcxGridTableController_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxGridTableController);
{$ENDIF}
  if PropName = 'FOCUSEDROWINDEX' then
    TcxGridTableController(Instance).FocusedRowIndex := Value
  else if PropName = 'FOCUSEDRECORDINDEX' then
    TcxGridTableController(Instance).FocusedRecordIndex := Value
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxCustomGridTableView_Property(Instance: TObject; ClassType: TClass;
  const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomGridTableView);
{$ENDIF}
  if PropName = 'CONTROLLER' then
    Result := Integer(TcxCustomGridTableView(Instance).Controller)
  else if PropName = 'VIEWDATA' then
    Result := Integer(TcxCustomGridTableView(Instance).ViewData)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxGridTableView_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxGridTableView);
{$ENDIF}
  if PropName = 'VIEWINFO' then
    Result := Integer(TcxGridTableView(Instance).ViewInfo)
  else if PropName = 'COLUMNCOUNT' then
    Result := TcxGridTableView(Instance).ColumnCount
  else if PropName = 'GROUPEDCOLUMNCOUNT' then
    Result := TcxGridTableView(Instance).GroupedColumnCount
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxGridViewData_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxGridViewData);
{$ENDIF}
  if PropName = 'EDITINGRECORD' then
    Result := Integer( TcxGridViewData(Instance).EditingRecord )
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxGridTableView_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxGridTableView);
{$ENDIF}
  if MethodName = 'COLUMNS.GET' then
    Result := Integer(TcxGridTableView(Instance).Columns[Caller.Params[0]])
  else if MethodName = 'GROUPEDCOLUMNS.GET' then
    Result := Integer(TcxGridTableView(Instance).GroupedColumns[Caller.Params[0]])
  else if MethodName = 'FINDITEMBYFIELDNAME' then
    Result := Integer(TcxGridTableView(Instance).FindItemByFieldName(Caller.Params[0]))
  else if MethodName = 'GETVIEWINFOHEIGHT' then
    Result := TcxGridTableView(Instance).GetViewInfoHeight
  else if MethodName = 'GETVIEWINFOWIDTH' then
    Result := TcxGridTableView(Instance).GetViewInfoWidth
  else if MethodName = 'CLEARITEMS' then
    TcxGridTableView(Instance).ClearItems
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxGridBands_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxGridBands);
{$ENDIF}
  if PropName = 'COUNT' then
    Result := TcxGridBands(Instance).Count
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxGridBands_Method(Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxGridBands);
{$ENDIF}
  if MethodName = 'ITEMS.GET' then
    Result := Integer(TcxGridBands(Instance)[Caller.Params[0]])
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxGridBandedTableView_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxGridBandedTableView);
{$ENDIF}
  if PropName = 'BANDS' then
    Result := Integer(TcxGridBandedTableView(Instance).Bands)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxGridTableCellViewInfo_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxGridTableCellViewInfo);
{$ENDIF}
  if PropName = 'GRIDRECORD' then
    Result := Integer(TcxGridTableCellViewInfo(Instance).GridRecord)
  else if PropName = 'SELECTED' then
    Result := TcxGridTableCellViewInfo(Instance).Selected
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxGridTableDataCellViewInfo_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxGridTableDataCellViewInfo);
{$ENDIF}
  if PropName = 'DISPLAYVALUE' then
    Result := Integer(TcxGridTableDataCellViewInfo(Instance).DisplayValue)
  else if PropName = 'ITEM' then
    Result := Integer(TcxGridTableDataCellViewInfo(Instance).Item)
  else if PropName = 'PROPERTIES' then
    Result := Integer(TcxGridTableDataCellViewInfo(Instance).Properties)
  else if PropName = 'STYLE' then
    Result := Integer(TcxGridTableDataCellViewInfo(Instance).Style)
  else if PropName = 'VALUE' then
    Result := TcxGridTableDataCellViewInfo(Instance).Value
  else if PropName = 'FOCUSED' then
    Result := TcxGridTableDataCellViewInfo(Instance).Focused
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxGridTableItem_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomGridTableItem);
{$ENDIF}
  if PropName = 'ID' then
    Result := TcxCustomGridTableItem(Instance).ID
  else if PropName = 'INDEX' then
    Result := TcxCustomGridTableItem(Instance).Index
  else if PropName = 'OPTIONS' then
    Result := Integer(TcxCustomGridTableItem(Instance).Options)
  else if PropName = 'VISIBLEINDEX' then
    Result := TcxCustomGridTableItem(Instance).VisibleIndex
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxCustomEditorRowProperties_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxCustomEditorRowProperties);
{$ENDIF}
  if MethodName = 'VALUES.GET' then
    Result := TcxCustomEditorRowProperties(Instance).Values[Caller.Params[0]]
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'EXPORTCXGRIDTOEXCEL' then
    SBaseCxUtils.ExportCxGridToExcel(TcxGrid(VarToPointer(Caller.Params[0])), VarToStr(Caller.Params[1]), VarToStr(Caller.Params[2]), (Caller.Params[3] = True))
  else if MethodName = 'EXPORTCXTREETOEXCEL' then
    SBaseCxUtils.ExportCxTreeToExcel(TcxDBTreeList(VarToPointer(Caller.Params[0])), VarToStr(Caller.Params[1]), (Caller.Params[3] = True))
end;

function TFunctions_TcxGrid.Call_TcxCustomDataController_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxCustomDataController);
{$ENDIF}
  if MethodName = 'VALUES.GET' then
    Result := TcxCustomDataController(Instance).Values[Caller.Params[0], Caller.Params[1]]
  else if MethodName = 'VALUES.SET' then
    TcxCustomDataController(Instance).Values[Caller.Params[0], Caller.Params[1]] := Caller.Params[2]
  else if MethodName = 'INSERT' then
    TcxCustomDataController(Instance).Insert
  else if MethodName = 'APPEND' then
    TcxCustomDataController(Instance).Append
  else if MethodName = 'EDIT' then
    TcxCustomDataController(Instance).Edit
  else if MethodName = 'POST' then
    TcxCustomDataController(Instance).Post
  else if MethodName = 'CANCEL' then
    TcxCustomDataController(Instance).Cancel

  else if MethodName = 'POSTEDITINGDATA' then
    TcxCustomDataController(Instance).PostEditingData
  else if MethodName = 'APPENDRECORD' then
    Result := TcxCustomDataController(Instance).AppendRecord
  else if MethodName = 'DELETERECORD' then
    TcxCustomDataController(Instance).DeleteRecord(Caller.Params[0])

  else if MethodName = 'GOTOFIRST' then
    TcxCustomDataController(Instance).GotoFirst
  else if MethodName = 'GOTOLAST' then
    TcxCustomDataController(Instance).GotoLast
  else if MethodName = 'GOTONEXT' then
    TcxCustomDataController(Instance).GotoNext
  else if MethodName = 'GOTOPREV' then
    TcxCustomDataController(Instance).GotoPrev

  else if MethodName = 'ISEOF' then
    Result := TcxCustomDataController(Instance).IsEOF
  else if MethodName = 'ISBOF' then
    Result := TcxCustomDataController(Instance).IsBOF

  else if MethodName = 'FILTEREDRECORDINDEX.GET' then
    Result := TcxCustomDataController(Instance).FilteredRecordIndex[Caller.Params[0]]
  else if MethodName = 'GETROWINDEXBYRECORDINDEX' then
    Result := TcxCustomDataController(Instance).GetRowIndexByRecordIndex(Caller.Params[0], Caller.Params[1] = True)
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxCustomGridRecord_Method(Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxCustomGridRecord);
{$ENDIF}
  if MethodName = 'VALUES.GET' then
    Result := TcxCustomGridRecord(Instance).Values[Caller.Params[0]]
  else if MethodName = 'VALUES.SET' then
    TcxCustomGridRecord(Instance).Values[Caller.Params[0]] := Caller.Params[1]
  else if MethodName = 'DISPLAYTEXTS.GET' then
    Result := (Instance as TcxCustomGridRecord).DisplayTexts[Caller.Params[0]]
  else if MethodName = 'DISPLAYTEXTS.SET' then
    (Instance as TcxCustomGridRecord).DisplayTexts[Caller.Params[0]] := Caller.Params[1]
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxCustomEditorRowProperties_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomEditorRowProperties);
{$ENDIF}
  if PropName = 'VALUE' then
    Result := TcxEditorRowProperties(Instance).Value
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxCustomGridRecord_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomGridRecord);
{$ENDIF}
  if PropName = 'LEVEL' then
    Result := TcxCustomGridRecord(Instance).Level
  else if PropName = 'INDEX' then
    Result := TcxCustomGridRecord(Instance).Index
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxCustomGridRow_Method(Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxCustomGridRow);
{$ENDIF}
  if MethodName = 'VALUES.GET' then
    Result := TcxCustomGridRow(Instance).Values[Caller.Params[0]]
  else if MethodName = 'VALUES.SET' then
    TcxCustomGridRow(Instance).Values[Caller.Params[0]] := Caller.Params[1]
  else if MethodName = 'EXPAND' then
    TcxCustomGridRow(Instance).Expand(Caller.Params[0] = True)
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxCustomGridTableView_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxGridTableView);
{$ENDIF}
  if MethodName = 'REFRESHDATASET' then
    TcxCustomGridTableView(Instance).RefreshDataSet
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxGridLevel_Method(Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxGridLevel);
{$ENDIF}
  if MethodName = 'ADD' then
    Result := Integer(TcxGridLevel(Instance).Add)
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxGrid_Method(Instance: TObject;  ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
var
  AClass: TClass;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxGrid);
{$ENDIF}
  AClass := FindClass(Caller.Params[0]);
  if Assigned(AClass) then
    Result := Integer(TcxGrid(Instance).CreateView(TcxCustomGridViewClass(AClass)))
{$IFDEF DEBUG}
  else
    raise Exception.CreateFmt('Не найден класс ''%s''', [String(Caller.Params[0])])
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxGridItemDBDataBinding_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxGridItemDBDataBinding);
{$ENDIF}
  if PropName = 'FIELD' then
    Result := Integer(TcxGridItemDBDataBinding(Instance).Field)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxGridLevel_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxGridLevel);
{$ENDIF}
  if PropName = 'CONTROL' then
    Result := Integer(TcxGridLevel(Instance).Control)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxDataController_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomDataController);
{$ENDIF}
  if PropName = 'FILTEREDRECORDCOUNT' then
    Result := TcxCustomDataController(Instance).FilteredRecordCount
  else if PropName = 'FOCUSEDRECORDINDEX' then
    Result := TcxCustomDataController(Instance).FocusedRecordIndex
  else if PropName = 'RECORDCOUNT' then
    Result := TcxCustomDataController(Instance).RecordCount
//  else if PropName = 'GROUPS' then
//    Result := Integer(TcxCustomDataController(Instance).Groups)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxDataSummary_Method(Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxDataSummary);
{$ENDIF}
  if MethodName = 'FOOTERSUMMARYVALUES.GET' then
    Result := TcxDataSummary(Instance).FooterSummaryValues[Caller.Params[0]]
  else if MethodName = 'FOOTERSUMMARYVALUES.SET' then
    TcxDataSummary(Instance).FooterSummaryValues[Caller.Params[0]] := Caller.Params[1]    
  else if MethodName = 'FOOTERSUMMARYTEXTS.GET' then
    Result := TcxDataSummary(Instance).FooterSummaryTexts[Caller.Params[0]]
  else if MethodName = 'GROUPFOOTERSUMMARYTEXTS.GET' then
    Result := TcxDataSummary(Instance).GROUPFOOTERSUMMARYTEXTS[Caller.Params[0], Caller.Params[1], Caller.Params[2]]

  else if MethodName = 'FOOTERSUMMARYVALUESBYNAME.GET' then
    Result := TcxDataSummary(Instance).FooterSummaryValuesByName[Caller.Params[0]]
  else if MethodName = 'FOOTERSUMMARYVALUESBYNAME.SET' then
    TcxDataSummary(Instance).FooterSummaryValuesByName[Caller.Params[0]] := Caller.Params[1]
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxDataSummaryItems_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxDataSummaryItems);
{$ENDIF}
  if MethodName = 'INDEXOFITEMLINK' then
    Result := TcxDataSummaryItems(Instance).IndexOfItemLink(TObject(Integer(Caller.Params[0])))
  else if MethodName = 'ITEMOFITEMLINK' then
    Result := Integer(TcxDataSummaryItems(Instance).ItemOfItemLink(TObject(Integer(Caller.Params[0]))))
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxGridViewData_Method(Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxGridViewData);
{$ENDIF}
  if MethodName = 'EXPAND' then
    TcxGridViewData(Instance).Expand(Caller.Params[0] = True)
  else if MethodName = 'COLLAPSE' then
    TcxGridViewData(Instance).Collapse(Caller.Params[0] = True)
  else if MethodName = 'ROWS.GET' then
    Result := Integer(TcxGridViewData(Instance).Rows[Caller.Params[0]])
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxCustomGridView_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxCustomGridView);
{$ENDIF}
  if MethodName = 'BEGINUPDATE' then
    TcxCustomGridView(Instance).BeginUpdate
  else if MethodName = 'ENDUPDATE' then
    TcxCustomGridView(Instance).EndUpdate
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxCustomRow_Method(Instance: TObject;
  ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxCustomRow);
{$ENDIF}
  if MethodName = 'ROWS.GET' then
    Result := Integer(TcxCustomRow(Instance).Rows[Caller.Params[0]])
  else if MethodName = 'HASCHILDREN' then
    Result := TcxCustomRow(Instance).HasChildren
  else if MethodName = 'HASPARENT' then
    Result := TcxCustomRow(Instance).HasParent
  else if MethodName = 'INDEXOF' then
    Result := TcxCustomRow(Instance).IndexOf( TcxCustomRow(Pointer(Integer(Caller.Params[0]))) )
  else if MethodName = 'ISCHILD' then
    Result := TcxCustomRow(Instance).IsChild( TcxCustomRow(Pointer(Integer(Caller.Params[0]))) )
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

procedure TFunctions_TcxGrid.Set_TcxCustomDataController_Property(
  Instance: TObject; ClassType: TClass; const PropName: String;
  Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomDataController);
{$ENDIF}
  if PropName = 'FOCUSEDRECORDINDEX' then
    TcxCustomDataController(Instance).FocusedRecordIndex := Value
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TFunctions_TcxGrid.Set_TcxCustomEditorRowProperties_Property(
  Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomEditorRowProperties);
{$ENDIF}
  if PropName = 'VALUE' then
    TcxEditorRowProperties(Instance).Value := Value
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxDBDataController_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxDBDataController);
{$ENDIF}
  if PropName = 'RECNO' then
    Result := TcxDBDataController(Instance).RecNo
  else if PropName = 'DATASET' then
    Result := Integer(TcxDBDataController(Instance).DataSet)
  else if PropName = 'FILTER' then
    Result := Integer(TcxDBDataFilterCriteria(TcxDBDataController(Instance).Filter))
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxDBTreeListNode_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxDBTreeListNode);
{$ENDIF}
  if PropName = 'KEYVALUE' then
    Result := TcxDBTreeListNode(Instance).KeyValue
  else if PropName = 'PARENTKEYVALUE' then
    Result := TcxDBTreeListNode(Instance).ParentKeyValue
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TFunctions_TcxGrid.Set_TcxDBDataController_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxDBDataController);
{$ENDIF}
  if PropName = 'RECNO' then
    TcxDBDataController(Instance).RecNo := Value
  else if PropName = 'FILTER' then
    TcxDBDataController(Instance).Filter := TcxDBDataFilterCriteria(Integer(Value))
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxGrid_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxGrid);
{$ENDIF}
  if PropName = 'ACTIVEVIEW' then
    Result := Integer(TcxGrid(Instance).ActiveView)
  else if PropName = 'FOCUSEDVIEW' then
    Result := Integer(TcxGrid(Instance).FocusedView)
  else if PropName = 'LEVELS' then
    Result := Integer(TcxGrid(Instance).Levels)
  else if PropName = 'LOOKANDFEEL' then
    Result := Integer(TcxGrid(Instance).LookAndFeel)
  else if PropName = 'ACTIVELEVEL' then
    Result := Integer(TcxGrid(Instance).ActiveLevel)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxRowValueInfo_Property(Instance: TObject;
  ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxRowValueInfo);
{$ENDIF}
  if PropName = 'ROW' then
    Result := Integer(TcxRowValueInfo(Instance).Row)
  else if PropName = 'FOCUSED' then
    Result := TcxRowValueInfo(Instance).Focused
  else if PropName = 'ROWCELLINDEX' then
    Result := TcxRowValueInfo(Instance).RowCellIndex
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TFunctions_TcxGrid.Set_TcxGrid_Property(Instance: TObject;
  ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxGrid);
{$ENDIF}
  if PropName = 'LOOKANDFEEL' then
    TcxGrid(Instance).LookAndFeel := VarToPointer(Value)
  else if PropName = 'ACTIVELEVEL' then
    TcxGrid(Instance).ActiveLevel := VarToPointer(Value)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxTreeListEditCellViewInfo_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxTreeListEditCellViewInfo);
{$ENDIF}
  if PropName = 'COLUMN' then
    Result := Integer(TcxTreeListEditCellViewInfo(Instance).Column)
  else if PropName = 'BAND' then
    Result := Integer(TcxTreeListEditCellViewInfo(Instance).Band)
  else if PropName = 'NODE' then
    Result := Integer(TcxTreeListEditCellViewInfo(Instance).Node)
  else if PropName = 'SELECTED' then
    Result := TcxTreeListEditCellViewInfo(Instance).Selected
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxTreeListNode_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxTreeListNode);
{$ENDIF}
  if MethodName = 'ITEMS.GET' then
    Result := Integer(TcxTreeListNode(Instance).Items[Caller.Params[0]])
  else if MethodName = 'TEXTS.GET' then
    Result := TcxTreeListNode(Instance).Texts[Caller.Params[0]]
  else if MethodName = 'VALUES.GET' then
    Result := TcxTreeListNode(Instance).Values[Caller.Params[0]]
  else if MethodName = 'VALUES.SET' then
    TcxTreeListNode(Instance).Values[Caller.Params[0]] := Caller.Params[1]
  else if MethodName = 'EXPAND' then
    TcxTreeListNode(Instance).Expand(Caller.Params[0] = True)
  else if MethodName = 'COLLAPSE' then
    TcxTreeListNode(Instance).Collapse(Caller.Params[0] = True)
  else if MethodName = 'SETCHECKGROUPTYPE' then
    TcxTreeListNode(Instance).SetCheckGroupType(Caller.Params[0])
  else if MethodName = 'GETFIRSTCHILD' then
    Result := Integer(TcxTreeListNode(TcxTreeListNode(Instance).GetFirstChild))
  else if MethodName = 'GETNEXTCHILD' then
    Result := Integer(TcxTreeListNode(TcxTreeListNode(Instance).GetNextChild(TcxTreeListNode(Integer(Caller.Params[0])))))
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxDBDataController_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'LOCATEBYKEY' then
    Result := TcxDBDataController(Instance).LocateByKey(Caller.Params[0])
end;

function TFunctions_TcxGrid.Call_TcxDBTreeListDataController_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'LOCATEBYKEY' then
    Result := TcxDBTreeListDataController(Instance).LocateByKey(Caller.Params[0])
end;

function TFunctions_TcxGrid.Call_TcxDBTreeList_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxDBTreeList);
{$ENDIF}
  if MethodName = 'FOCUSEDRECORDVALUES.GET' then
    Result := TcxDBTreeList(Instance).FocusedRecordValues[Caller.Params[0]]
  else if MethodName = 'FOCUSEDRECORDVALUES.SET' then
    TcxDBTreeList(Instance).FocusedRecordValues[Caller.Params[0]] := Caller.Params[1]
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxEditorPropertiesCollection_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxEditorPropertiesCollection);
{$ENDIF}
  if MethodName = 'ITEMS.GET' then
    Result := Integer(TcxEditorPropertiesCollection(Instance).Items[Caller.Params[0]])
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxTreeListNode_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxTreeListNode);
{$ENDIF}
  if PropName = 'ISROOT' then
    Result := TcxTreeListNodeCrack(Instance).IsRoot
  else if PropName = 'COUNT' then
    Result := TcxTreeListNode(Instance).Count
  else if PropName = 'DATA' then
    Result := Integer(TcxTreeListNode(Instance).Data)
  else if PropName = 'FOCUSED' then
    Result := TcxTreeListNode(Instance).Focused
  else if PropName = 'HASCHILDREN' then
    Result := TcxTreeListNode(Instance).HasChildren
  else if PropName = 'INDEX' then
    Result := TcxTreeListNode(Instance).Index
  else if PropName = 'LEVEL' then
    Result := TcxTreeListNode(Instance).Level
  else if PropName = 'PARENT' then
    Result := Integer(TcxTreeListNode(Instance).Parent)
//  else if PropName = 'RECORDINDEX' then
//    Result := TcxTreeListNode(Instance).RecordIndex
  else if PropName = 'SELECTED' then
    Result := TcxTreeListNode(Instance).Selected
  else if PropName = 'VALUECOUNT' then
    Result := TcxTreeListNode(Instance).ValueCount
  else if PropName = 'VISIBLEINDEX' then
    Result := TcxTreeListNode(Instance).VisibleIndex
  else if PropName = 'CHECKSTATE' then
    Result := (Instance as TcxTreeListNode).CheckState
  else if PropName = 'CHECKGROUPTYPE' then
    Result := (Instance as TcxTreeListNode).CheckGroupType
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TFunctions_TcxGrid.Set_TcxTreeListNode_Property(Instance: TObject;
  ClassType: TClass; const PropName: String; Value: Variant);
begin
  if PropName = 'CHECKSTATE' then
    (Instance as TcxTreeListNode).CheckState := TcxCheckBoxState(Word(Value))
  else if PropName = 'CHECKGROUPTYPE' then
    (Instance as TcxTreeListNode).CheckGroupType := TcxTreeListNodeCheckGroupType(Word(Value))
  else if PropName = 'SELECTED' then
    (Instance as TcxTreeListNode).Selected := Value
end;

function TFunctions_TcxGrid.Get_TcxTreeListColumn_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxTreeListColumn);
{$ENDIF}
  if PropName = 'ITEMINDEX' then
    Result := TcxTreeListColumn(Instance).ItemIndex
  else if PropName = 'PROPERTIESCLASSNAME' then
    Result := String(TcxTreeListColumn(Instance).PropertiesClassName)
  else if PropName = 'PROPERTIESCLASS' then
    Result := Integer(TcxTreeListColumn(Instance).PropertiesClass)
  else if PropName = 'PROPERTIES' then
    Result := Integer(TcxTreeListColumn(Instance).Properties)
  else if SameText(PropName, SConst_EditValue) then
    Result := TcxTreeListColumn(Instance).EditValue
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxCustomGridTableItem_Property(Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomGridTableItem);
{$ENDIF}
  if PropName = 'FOCUSED' then
    Result := TcxCustomGridTableItem(Instance).Focused
  else if PropName = 'PROPERTIESCLASSNAME' then
    Result := String(TcxCustomGridTableItem(Instance).PropertiesClassName)
  else if PropName = 'PROPERTIESCLASS' then
    Result := Integer(TcxCustomGridTableItem(Instance).PropertiesClass)
  else if PropName = 'PROPERTIES' then
    Result := Integer(TcxCustomGridTableItem(Instance).Properties)
  else if SameText(PropName, SConst_EditValue) then
    Result := TcxCustomGridTableItem(Instance).EditValue
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxCustomTreeList_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomTreeList);
{$ENDIF}
  if PropName = 'ROOT' then
    Result := Integer(TcxCustomTreeList(Instance).Root)
  else if PropName = 'FOCUSEDCOLUMN' then
    Result := Integer(TcxCustomTreeList(Instance).FocusedColumn)
  else if PropName = 'FOCUSEDNODE' then
    Result := Integer(TcxCustomTreeList(Instance).FocusedNode)
  else if PropName = 'COUNT' then
    Result := Integer(TcxCustomTreeList(Instance).Count)
  else if PropName = 'SELECTIONCOUNT' then
    Result := Integer(TcxCustomTreeList(Instance).SelectionCount)
  else if PropName = 'DRAGNODE' then
    Result := Integer(TcxCustomTreeList(Instance).DragNode)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxCustomVerticalGrid_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomVerticalGrid);
{$ENDIF}
  if PropName = 'RECORDCOUNT' then
    Result := TcxVirtualVerticalGrid(Instance).RecordCount
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxCustomTreeList_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxCustomTreeList);
{$ENDIF}
  if MethodName = 'GETNODEAT' then
    Result := Integer((Instance as TcxCustomTreeList).GetNodeAt(Caller.Params[0], Caller.Params[1]))
  else if MethodName = 'ITEMS.GET' then
    Result := Integer(TcxCustomTreeList(Instance).Items[Caller.Params[0]])
  else if MethodName = 'SELECTIONS.GET' then
    Result := Integer(TcxCustomTreeList(Instance).Selections[Caller.Params[0]])
  else if MethodName = 'FULLCOLLAPSE' then
    TcxCustomTreeList(Instance).FullCollapse
  else if MethodName = 'FULLEXPAND' then
    TcxCustomTreeList(Instance).FullExpand
  else if MethodName = 'DELETESELECTION' then
    TcxCustomTreeList(Instance).DeleteSelection
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxGridDBDataController_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxGridDBDataController);
{$ENDIF}
  if MethodName = 'CREATEALLITEMS' then
    TcxGridDBDataController(Instance).CreateAllItems
  else if MethodName = 'GETITEMBYFIELDNAME' then
    Result := Integer(TcxGridDBDataController(Instance).GetItemByFieldName(Caller.Params[0]))
  else if MethodName = 'GETSELECTEDCOUNT' then
    Result := Integer(TcxGridDBDataController(Instance).GetSelectedCount)
  else if MethodName = 'GETSELECTEDROWINDEX' then
    Result := Integer(TcxGridDBDataController(Instance).GetSelectedRowIndex(Caller.Params[0]))  
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxGridDBTableView_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxGridDBTableView);
{$ENDIF}
  if MethodName = 'CREATECOLUMN' then
    Result := Integer(TcxGridDBTableView(Instance).CreateColumn)
  else if MethodName = 'AUTOCREATECOLUMNS' then
    TcxGridDBTableView(Instance).AutoCreateColumns
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

procedure TFunctions_TcxGrid.Set_TcxCustomGridTableItem_Property(
  Instance: TObject; ClassType: TClass; const PropName: String;
  Value: Variant);
var
  LClass : TClass;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomGridTableItem);
{$ENDIF}
  if PropName = 'PROPERTIESCLASSNAME' then
    TcxCustomGridTableItem(Instance).PropertiesClassName := Value
  else if PropName = 'PROPERTIESCLASS' then begin
    LClass := TClass(Pointer(Integer(Value)));
    TcxCustomGridTableItem(Instance).PropertiesClass := TcxCustomEditPropertiesClass(LClass);
  end else if PropName = 'PROPERTIES' then
    TcxCustomGridTableItem(Instance).Properties := TcxCustomEditProperties(Integer(Value))
  else if SameText(PropName, SConst_EditValue) then
    TcxCustomGridTableItem(Instance).EditValue := Value
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TFunctions_TcxGrid.Set_TcxCustomRow_Property(Instance: TObject;
  ClassType: TClass; const PropName: String; Value: Variant);
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomRow);
{$ENDIF}
  if PropName = 'VERTICALGRID' then
    TcxCustomRow(Instance).VerticalGrid := TcxCustomVerticalGrid(Pointer(Integer(Value)))
  else if PropName = 'PARENT' then
    TcxCustomRow(Instance).Parent := TcxCustomRow(Pointer(Integer(Value)))
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

procedure TFunctions_TcxGrid.Set_TcxTreeListColumn_Property(Instance: TObject; ClassType: TClass; const PropName: String; Value: Variant);
var
  LClass : TClass;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxTreeListColumn);
{$ENDIF}
  if PropName = 'PROPERTIESCLASSNAME' then
    TcxTreeListColumn(Instance).PropertiesClassName := Value
  else if PropName = 'PROPERTIESCLASS' then begin
    LClass := TClass(Pointer(Integer(Value)));
    TcxTreeListColumn(Instance).PropertiesClass := TcxCustomEditPropertiesClass(LClass);
  end else if PropName = 'PROPERTIES' then
    TcxTreeListColumn(Instance).Properties := TcxCustomEditProperties(Integer(Value))
  else if SameText(PropName, SConst_EditValue) then
    TcxTreeListColumn(Instance).EditValue := Value
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxLookupDBGridColumn_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxLookupDBGridColumns);
{$ENDIF}
  if MethodName = 'ITEMS.GET' then
    Result := Integer(TcxLookupDBGridColumns(Instance).Items[Caller.Params[0]])
  else if MethodName = 'ITEMS.SET' then
    TcxLookupDBGridColumns(Instance).Items[Caller.Params[0]] := TcxLookupDBGridColumn(Integer(Caller.Params[1]))
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxCustomGridView_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomGridView);
{$ENDIF}
  if PropName = 'DATACONTROLLER' then
    Result := Integer(TcxCustomGridView(Instance).DataController)
  else if PropName = 'LEVEL' then
    Result := Integer(TcxCustomGridView(Instance).Level)
  else if PropName = 'PATTERNGRIDVIEW' then
    Result := Integer(TcxCustomGridView(Instance).PatternGridView)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxCustomRow_Property(Instance: TObject;
  ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxCustomRow);
{$ENDIF}
  if PropName = 'VERTICALGRID' then
    Result := Integer(TcxCustomRow(Instance).VerticalGrid)
  else if PropName = 'COUNT' then
    Result := TcxCustomRow(Instance).Count
  else if PropName = 'PARENT' then
    Result := Integer(TcxCustomRow(Instance).Parent)
  else if PropName = 'INDEX' then
    Result := Integer(TcxCustomRow(Instance).Index)
  else if PropName = 'STYLES' then
    Result := Integer(TcxCategoryRow(Instance).Styles)
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Get_TcxDataFilterCriteria_Property(
  Instance: TObject; ClassType: TClass; const PropName: String): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceProperty(Instance, ClassType, PropName, TcxFilterCriteria);
{$ENDIF}
  if PropName = 'ROOT' then
    Result := Integer(TcxFilterCriteriaItemList(TcxFilterCriteria(Instance).Root))
{$IFDEF DEBUG}
  else
    RaiseFSNoProperty(Instance, ClassType, PropName);
{$ENDIF}
end;

function TFunctions_TcxGrid.Call_TcxFilterCriteriaItemList_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TcxFilterCriteriaItemList);
{$ENDIF}
  if MethodName = 'ADDITEM' then
    Result := Integer(TcxFilterCriteriaItem(TcxFilterCriteriaItemList(Instance).AddItem(
      TObject(Integer(Caller.Params[0])),
      Caller.Params[1],
      Caller.Params[2],
      Caller.Params[3] )))
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;


{ TfsCxGridViewCellClickEvent }

procedure TfsCxGridViewCellClickEvent.DoEvent(Sender: TcxCustomGridTableView; ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton; AShift: TShiftState; var AHandled: Boolean);
var
  LShift: {$IFNDEF FPC}Byte{$ELSE}Integer{$ENDIF};
begin
{$IFNDEF FPC}
  LShift := Byte(PByteSet(@AShift)^);
{$ELSE}
  LShift := Integer(PByteSet(@AShift)^);
{$ENDIF}

  CallHandler([Sender, ACellViewInfo, Integer(AButton), LShift, AHandled]);
  AHandled := Handler.Params[4].Value;
end;

function TfsCxGridViewCellClickEvent.GetMethod: Pointer;
begin
  Result := @TfsCxGridViewCellClickEvent.DoEvent;
end;

{ TfsCxGridGetCellStyleEvent }

procedure TfsCxGridGetCellStyleEvent.DoEvent(Sender: TcxCustomGridTableView; ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem; var AStyle: TcxStyle);
var
  LOut: Variant;
begin
  CallHandler([Sender, ARecord, AItem, AStyle]);

  LOut := Handler.Params[3].Value;
  if VarIsOrdinal(LOut) then
    AStyle := Pointer(Integer(LOut))
  else
    AStyle := nil;
end;

function TfsCxGridGetCellStyleEvent.GetMethod: Pointer;
begin
  Result := @TfsCxGridGetCellStyleEvent.DoEvent;
end;

{ TfsCxGridGetPropertiesEvent }

procedure TfsCxGridGetPropertiesEvent.DoEvent(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AProperties: TcxCustomEditProperties);
begin
  CallHandler([Sender, ARecord, AProperties]);
  AProperties := VarToPointer(Handler.Params[2].Value);
end;

function TfsCxGridGetPropertiesEvent.GetMethod: Pointer;
begin
  Result := @TfsCxGridGetPropertiesEvent.DoEvent;
end;

{ TfsCxGridTableDataCellCustomDrawEvent }

procedure TfsCxGridTableDataCellCustomDrawEvent.DoEvent(Sender: TcxCustomGridTableView; ACanvas: TcxCanvas; AViewInfo: TcxGridTableDataCellViewInfo; var ADone: Boolean);
begin
  CallHandler([Sender, ACanvas, AViewInfo, ADone]);
  ADone := Handler.Params[3].Value;
end;

function TfsCxGridTableDataCellCustomDrawEvent.GetMethod: Pointer;
begin
  Result := @TfsCxGridTableDataCellCustomDrawEvent.DoEvent;
end;

{ TfsCxDBDataDetailHasChildrenEvent }

procedure TfsCxDBDataDetailHasChildrenEvent.DoEvent(Sender: TcxDBDataController; ARecordIndex, ARelationIndex: Integer; const AMasterDetailKeyFieldNames: string; const AMasterDetailKeyValues: Variant; var HasChildren: Boolean);
begin
  CallHandler([Sender, ARecordIndex, HasChildren]);
  HasChildren := Handler.Params[2].Value;
end;

function TfsCxDBDataDetailHasChildrenEvent.GetMethod: Pointer;
begin
  Result := @TfsCxDBDataDetailHasChildrenEvent.DoEvent;
end;

{ TfsCxGridGetDataTextEvent }

procedure TfsCxGridGetDataTextEvent.DoEvent(Sender: TcxCustomGridTableItem; ARecordIndex: Integer; var AText: string);
begin
  CallHandler([Sender, ARecordIndex, AText]);
  AText := Handler.Params[2].Value;
end;

function TfsCxGridGetDataTextEvent.GetMethod: Pointer;
begin
  Result := @TfsCxGridGetDataTextEvent.DoEvent;
end;

{ TfsCxGridGetDisplayTextEvent }

procedure TfsCxGridGetDisplayTextEvent.DoEvent(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: string);
begin
  CallHandler([Sender, ARecord, AText]);
  AText := Handler.Params[2].Value;
end;

function TfsCxGridGetDisplayTextEvent.GetMethod: Pointer;
begin
  Result := @TfsCxGridGetDisplayTextEvent.DoEvent;
end;

{ TfsCxGridColumnEvent }

procedure TfsCxGridColumnEvent.DoEvent(Sender: TcxGridTableView; AColumn: TcxGridColumn);
begin
  CallHandler([Sender, AColumn]);
end;

function TfsCxGridColumnEvent.GetMethod: Pointer;
begin
  Result := @TfsCxGridColumnEvent.DoEvent;
end;

{ TfsCxSummaryEvent }

procedure TfsCxSummaryEvent.DoEvent(ASender: TcxDataSummaryItems; Arguments: TcxSummaryEventArguments; var OutArguments: TcxSummaryEventOutArguments);
begin
  CallHandler([ASender, Integer(@Arguments), Integer(@OutArguments)]);
end;

function TfsCxSummaryEvent.GetMethod: Pointer;
begin
  Result := @TfsCxSummaryEvent.DoEvent;
end;

{ TfsCxCustomDrawEditCellViewInfoEvent }

procedure TfsCxCustomDrawEditCellViewInfoEvent.DoEvent(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo;
  var ADone: Boolean);
begin
  CallHandler([Sender, ACanvas, AViewInfo, ADone]);
  ADone := Handler.Params[3].Value = True
end;

function TfsCxCustomDrawEditCellViewInfoEvent.GetMethod: Pointer;
begin
  Result := @TfsCxCustomDrawEditCellViewInfoEvent.DoEvent;
end;

{ TfsCxGridGetGroupStyleEvent }

procedure TfsCxGridGetGroupStyleEvent.DoEvent(Sender: TcxGridTableView; ARecord: TcxCustomGridRecord; ALevel: Integer; var AStyle: TcxStyle);
var
  LOut: Variant;
begin
  CallHandler([Sender, ARecord, ALevel, AStyle]);

  LOut := Handler.Params[3].Value;
  if VarIsOrdinal(LOut) then
    AStyle := Pointer(Integer(LOut))
  else
    AStyle := nil;
end;

function TfsCxGridGetGroupStyleEvent.GetMethod: Pointer;
begin
  Result := @TfsCxGridGetGroupStyleEvent.DoEvent;
end;

{ TfsTreeListGetNodeImageIndexEvent }

procedure TfsTreeListGetNodeImageIndexEvent.DoEvent(
  Sender: TObject;
  ANode: TcxTreeListNode;
  AIndexType: TcxTreeListImageIndexType;
  var AIndex: TImageIndex);
begin
  CallHandler([Sender, ANode, Integer(AIndexType), AIndex]);
  AIndex := Handler.Params[3].Value
end;

function TfsTreeListGetNodeImageIndexEvent.GetMethod: Pointer;
begin
  Result := @TfsTreeListGetNodeImageIndexEvent.DoEvent;
end;

{ TfsTreeListFocusedChangedEvent }

procedure TfsTreeListFocusedChangedEvent.DoEvent(Sender: TcxCustomTreeList; APrevFocusedNode, AFocusedNode: TcxTreeListNode);
begin
  CallHandler([Sender, APrevFocusedNode, AFocusedNode]);
end;

function TfsTreeListFocusedChangedEvent.GetMethod: Pointer;
begin
  Result := @TfsTreeListFocusedChangedEvent.DoEvent;
end;

{ TfsCxGridTableViewInitEditValueEvent }

procedure TfsCxGridTableViewInitEditValueEvent.DoEvent(
  Sender: TcxCustomGridTableView; AItem: TcxCustomGridTableItem;
  AEdit: TcxCustomEdit; var AValue: Variant);
begin
  CallHandler([Sender, AItem, AEdit, AValue]);
  AValue := Handler.Params[3].Value;
end;

function TfsCxGridTableViewInitEditValueEvent.GetMethod: Pointer;
begin
  Result := @TfsCxGridTableViewInitEditValueEvent.DoEvent;
end;

{ TfsCxGridViewDblClickEvent }

procedure TfsCxGridViewDblClickEvent.DoEvent(Sender: TObject);
begin
  CallHandler([Sender]);
end;

function TfsCxGridViewDblClickEvent.GetMethod: Pointer;
begin
  Result := @TfsCxGridViewDblClickEvent.DoEvent;
end;

{ TfsDBTableViewFocusedChangedEvent }

procedure TfsDBTableViewFocusedChangedEvent.DoEvent(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord;
  ANewItemRecordFocusingChanged: Boolean);
begin
  CallHandler([Sender, APrevFocusedRecord, AFocusedRecord, ANewItemRecordFocusingChanged]);
end;

function TfsDBTableViewFocusedChangedEvent.GetMethod: Pointer;
begin
  Result := @TfsDBTableViewFocusedChangedEvent.DoEvent;
end;

{ TfsTreeListColumnGetEditPropertiesEvent }

procedure TfsTreeListColumnGetEditPropertiesEvent.DoEvent(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var EditProperties: TcxCustomEditProperties);
begin
  CallHandler([Sender, ANode, EditProperties]);
  EditProperties := VarToPointer(Handler.Params[2].Value);
end;

function TfsTreeListColumnGetEditPropertiesEvent.GetMethod: Pointer;
begin
  Result := @TfsTreeListColumnGetEditPropertiesEvent.DoEvent;
end;

{ TfsTreeListColumnChangedEvent }

procedure TfsTreeListColumnChangedEvent.DoEvent(Sender: TcxCustomTreeList; AColumn: TcxTreeListColumn);
begin
  CallHandler([Sender, AColumn]);
end;

function TfsTreeListColumnChangedEvent.GetMethod: Pointer;
begin
  Result := @TfsTreeListColumnChangedEvent.DoEvent;
end;

{ TfsTreeListNodeChangingEvent }

procedure TfsTreeListNodeChangingEvent.DoEvent(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var Allow: Boolean);
begin
  CallHandler([Sender, ANode, Allow]);
  Allow := (Handler.Params[2].Value = True);
end;

function TfsTreeListNodeChangingEvent.GetMethod: Pointer;
begin
  Result := @TfsTreeListNodeChangingEvent.DoEvent;
end;

{ TfsTreeListNodeChangedEvent }

procedure TfsTreeListNodeChangedEvent.DoEvent(Sender: TcxCustomTreeList; ANode: TcxTreeListNode);
begin
  CallHandler([Sender, ANode]);
end;

function TfsTreeListNodeChangedEvent.GetMethod: Pointer;
begin
  Result := @TfsTreeListNodeChangedEvent.DoEvent;
end;

{ TfsTreeListColumnGetDisplayTextEvent }

procedure TfsTreeListColumnGetDisplayTextEvent.DoEvent(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var Value: string);
begin
  CallHandler([Sender, ANode, Value]);
  Value := VarToStr(Handler.Params[2].Value);
end;

function TfsTreeListColumnGetDisplayTextEvent.GetMethod: Pointer;
begin
  Result := @TfsTreeListColumnGetDisplayTextEvent.DoEvent;
end;

{ TfsTreeListNodeAssignValuesEvent }

procedure TfsTreeListNodeAssignValuesEvent.DoEvent(Sender: TcxCustomDBTreeList; ANode: TcxTreeListNode);
begin
  CallHandler([Sender, ANode]);
end;

function TfsTreeListNodeAssignValuesEvent.GetMethod: Pointer;
begin
  Result := @TfsTreeListNodeAssignValuesEvent.DoEvent;
end;

{ TcxDataSummaryHelper }

function TcxDataSummaryHelper.GetFooterSummaryIndexByFieldName(AFieldName: String): Integer;
var
  LCol : TObject;
  LDb  : TcxGridItemDBDataBinding;
begin
  for Result := 0 to FooterSummaryItems.Count do begin
    LCol := FooterSummaryItems[Result].ItemLink;
    if LCol is TcxGridDBBandedColumn then
      LDb := TcxGridDBBandedColumn(LCol).DataBinding
    else if LCol is TcxGridDBColumn then
      LDb := TcxGridDBColumn(LCol).DataBinding
    else
      LDb := nil;
    if Assigned(LDb) and (LDb.FieldName = AFieldName) then
      Exit;
  end;
  Result := -1;
end;

function TcxDataSummaryHelper.GetFooterSummaryValuesByName(AFieldName: String): Variant;
var
  LIndex : Integer;
begin
  LIndex := GetFooterSummaryIndexByFieldName(AFieldName);
  if LIndex < 0 then
    Result := Unassigned
  else
    Result := FooterSummaryValues[LIndex];
end;

procedure TcxDataSummaryHelper.SetFooterSummaryValuesByName(AFieldName: String; const Value: Variant);
var
  LIndex : Integer;
begin
  LIndex := GetFooterSummaryIndexByFieldName(AFieldName);
  if LIndex >= 0 then
    FooterSummaryValues[LIndex] := Value;
end;

{ TfsCustomGridTableItemGetPropertiesEvent }

procedure TfsCustomGridTableItemGetPropertiesEvent.DoEvent(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord;
  var AProperties: TcxCustomEditProperties);
begin
  CallHandler([Sender, ARecord, AProperties]);
  AProperties := VarToPointer(Handler.Params[2].Value);
end;

function TfsCustomGridTableItemGetPropertiesEvent.GetMethod: Pointer;
begin
  Result := @TfsCustomGridTableItemGetPropertiesEvent.DoEvent;
end;

{ TfsVerticalGridOnDrawValueEvent }

procedure TfsVerticalGridOnDrawValueEvent.DoEvent(Sender: TObject; ACanvas: TcxCanvas;
  APainter: TcxvgPainter; AValueInfo: TcxRowValueInfo; var Done: Boolean);
begin
  CallHandler([Sender, ACanvas, APainter, AValueInfo, Done]);
end;

function TfsVerticalGridOnDrawValueEvent.GetMethod: Pointer;
begin
  Result := @TfsVerticalGridOnDrawValueEvent.DoEvent;
end;

function TFunctions_TcxGrid.Call_TFlexCelGrid_Method(
  Instance: TObject; ClassType: TClass; const MethodName: String;
  Caller: TfsMethodHelper): Variant;
begin
{$IFDEF DEBUG}
  FSCheckInstanceMethod(Instance, ClassType, MethodName, Caller, TFlexCelGrid);
{$ENDIF}
  if MethodName = 'LOADSHEET' then
    TFlexCelGrid(Instance).LoadSheet
{$IFDEF DEBUG}
  else
    RaiseFSNoMethod(Instance, ClassType, MethodName, Caller);
{$ENDIF}
end;

{ TfsVerticalGridGetEditPropertiesEvent }

procedure TfsVerticalGridGetEditPropertiesEvent.DoEvent(Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer; var AProperties: TcxCustomEditProperties);
begin
  CallHandler([Sender, ARecordIndex, AProperties]);
  AProperties := TcxCustomEditProperties(Integer(Handler.Params[2].Value));
end;

function TfsVerticalGridGetEditPropertiesEvent.GetMethod: Pointer;
begin
  Result := @TfsVerticalGridGetEditPropertiesEvent.DoEvent;
end;

{ TfsTreeListColumnGetEditingPropertiesEvent }

procedure TfsTreeListColumnGetEditingPropertiesEvent.DoEvent(Sender: TObject; AData: Pointer; var AEditProperties: TcxCustomEditProperties);
begin
  CallHandler([Sender, AData, AEditProperties]);
  AEditProperties := VarToPointer(Handler.Params[2].Value);
end;

function TfsTreeListColumnGetEditingPropertiesEvent.GetMethod: Pointer;
begin
  Result := @TfsTreeListColumnGetEditingPropertiesEvent.DoEvent;
end;

{ TfsEditorRowStylesOnGetContentParamsEvent }

procedure TfsEditorRowStylesOnGetContentParamsEvent.DoEvent(AEditorRowProperties: TcxCustomEditorRowProperties; AFocused: Boolean; ARecordIndex: Integer; var AParams: TcxViewParams);
begin
  CallHandler([AEditorRowProperties, AFocused, ARecordIndex, Integer(@AParams)]);
end;

function TfsEditorRowStylesOnGetContentParamsEvent.GetMethod: Pointer;
begin
  Result := @TfsEditorRowStylesOnGetContentParamsEvent.DoEvent;
end;

{ TfsEditorRowPropertiesOnGetContentParams }

procedure TfsEditorRowPropertiesOnGetContentParams.DoEvent(AValueInfo: TcxRowValueInfo; ACanvas: TcxCanvas);
begin
  CallHandler([AValueInfo, ACanvas]);
end;

function TfsEditorRowPropertiesOnGetContentParams.GetMethod: Pointer;
begin
  Result := @TfsEditorRowPropertiesOnGetContentParams.DoEvent;
end;

{ TfsCustomEditorRowPropertiesValidateDrawValueEvent }

procedure TfsCustomEditorRowPropertiesValidateDrawValueEvent.DoEvent(Sender: TcxCustomEditorRowProperties; ARecordIndex: Integer; const AValue: Variant; AData: TcxEditValidateInfo);
begin
  CallHandler([Sender, ARecordIndex, AValue, AData]);
end;

function TfsCustomEditorRowPropertiesValidateDrawValueEvent.GetMethod: Pointer;
begin
  Result := @TfsCustomEditorRowPropertiesValidateDrawValueEvent.DoEvent;
end;

{ TfsTreeListOnIsGroupNode }

procedure TfsTreeListOnIsGroupNode.DoEvent(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var IsGroup: Boolean);
begin
  CallHandler([Sender, ANode, IsGroup]);
  IsGroup := (Handler.Params[2].Value = True);
end;

function TfsTreeListOnIsGroupNode.GetMethod: Pointer;
begin
  Result := @TfsTreeListOnIsGroupNode.DoEvent;
end;

initialization

  RegisterClasses(
    [
      TcxGrid,
      TcxGridDBTableView, TcxGridDBTableView,
      TcxGridDBDataController, TcxGridTableController, TcxGridDataController,
      TcxGridBandedTableOptionsView, TcxGridTableOptionsData,
      TcxGridColumn, TcxGridColumnOptions,
      TcxGridDBColumn, TcxGridDBBandedColumn,
      TcxStyleRepository, TcxGridColumnStyles, TcxGridBandedColumnStyles, TcxGridBandedTableViewStyles,
      TcxGridBandedColumnPosition, TcxGridBand,
      TcxGridColumnSummary,
      TcxDataFooterSummaryItems, TcxDataSummaryItem, TcxCustomDataSummaryItem,
      TcxGridTableOptionsCustomize, TcxCustomGridTableOptionsCustomize, TcxCustomGridOptions,
      TcxDBDataFilterCriteria, TcxDataFilterCriteria, TcxFilterCriteria,

      TcxDBTreeList, TcxCustomDBTreeList, TcxCustomDataTreeList,
      TcxDBTreeListColumn, TcxDBTreeListDataController, TcxItemDataBinding, TcxDBItemDataBinding,

      TcxPopupEdit,

      TFlexCelGrid
    ]
  );

  fsRTTIModules.Add(TFunctions_TcxGrid);

finalization
  fsRTTIModules.Remove(TFunctions_TcxGrid);

  UnRegisterClasses(
    [
      TcxGrid,
      TcxGridDBTableView, TcxGridDBTableView,
      TcxGridDBDataController, TcxGridTableController, TcxGridDataController,
      TcxGridBandedTableOptionsView, TcxGridTableOptionsData,
      TcxGridColumn, TcxGridColumnOptions,
      TcxGridDBColumn, TcxGridDBBandedColumn,
      TcxStyleRepository, TcxGridColumnStyles, TcxGridBandedColumnStyles, TcxGridBandedTableViewStyles,
      TcxGridBandedColumnPosition, TcxGridBand,
      TcxGridColumnSummary,
      TcxDataFooterSummaryItems, TcxDataSummaryItem, TcxCustomDataSummaryItem,
      TcxGridTableOptionsCustomize, TcxCustomGridTableOptionsCustomize, TcxCustomGridOptions,
      TcxDBDataFilterCriteria, TcxDataFilterCriteria, TcxFilterCriteria,

      TcxDBTreeList, TcxCustomDBTreeList, TcxCustomDataTreeList,
      TcxDBTreeListColumn, TcxDBTreeListDataController, TcxItemDataBinding, TcxDBItemDataBinding,

      TcxPopupEdit,

      TFlexCelGrid
    ]
  );

end.
