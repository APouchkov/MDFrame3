unit SBaseCxUtils;
{$I config.inc}

interface

uses
  Forms, Messages, Windows, ComCtrls, DB, Variants, Math, SysUtils, Excel_TLB, ComObj, StrUtils, RTLConsts,
  TypInfo, RegularExpressions, Classes, Graphics, cxHint,
  SBaseVariantFunctions, SBaseStringFunctions,
  cxGrid, cxGridCustomView, cxGridTableView, cxGridLevel, cxGridCustomTableView,
  cxGridExportLink, cxTL, cxDBTL, cxTLExportLink, cxGridDBDataDefinitions, cxCustomData,
  cxGraphics, cxImageComboBox, cxEditRepositoryItems, cxMemo, cxRadioGroup,
  cxButtons, cxPC, cxTextEdit, cxButtonEdit, cxEdit, cxDBEdit, cxDBLookupComboBox,
  cxCalendar, cxDropDownEdit, cxMaskEdit, cxCheckBox, cxGroupBox, cxCalc, cxSpinEdit, cxFormats,
  cxLabel, cxDBLabel, cxStaticLabel, cxLookupDBGrid, cxTimeEdit, cxMRUEdit, cxDBCheckComboBox, cxCheckLookupComboBox,
  RichEdit, cxRichEdit, Rtf2Html, cxDBExtLookupComboBox, cxMDLookupTreeComboBox, cxFontNameComboBox,
  cxGridDBTableView, cxGridDBBandedTableView, cxDBData, cxUserCheckBox, cxSplitter, dxCoreClasses,
  cxVGrid, cxDBVGrid, cxInplaceContainer, cxMultiFieldsDBDataBinding, cxScrollBox, cxUserCheckGroup,

  cxLookAndFeels, cxLookAndFeelPainters, dxSkinsCore, dxSkinsForm,
  dxSkinsDefaultPainters, dxSkinsdxStatusBarPainter, dxSkinscxPCPainter, dxSkinsdxBarPainter,
  cxMDDBLookupComboBox, cxMDRadioGroup, SeparatorPanel

//  , dxSkinBlack, dxSkinBlue, dxSkinCaramel, dxSkinCoffee
//  , dxSkinDarkRoom, dxSkinDarkSide
//  , dxSkinFoggy, dxSkinGlassOceans, dxSkiniMaginary, dxSkinLilian, dxSkinLiquidSky, dxSkinLondonLiquidSky
//  , dxSkinMcSkin, dxSkinMoneyTwins, dxSkinOffice2007Black, dxSkinOffice2007Blue
//  , dxSkinOffice2007Green, dxSkinOffice2007Pink, dxSkinOffice2007Silver
//  , dxSkinOffice2010Black, dxSkinOffice2010Blue, dxSkinOffice2010Silver, dxSkinSharp
//  , dxSkinPumpkin, dxSkinSeven, dxSkinSilver, dxSkinSpringTime
//  , dxSkinStardust, dxSkinSummer2008, dxSkinValentine, dxSkinXmas2008Blue

{$IFDEF BASEDATAMODULE_SUPPORT}
  , SBaseDataModule
{$ENDIF}
  ;

const
//  OLEExcel    : String = 'Excel.Application';

  SConst_APrevFocusedNode : String = 'APrevFocusedNode';
  SConst_AFocusedNode     : String = 'AFocusedNode';

type
  TcxApplicationLookAndFeelPainter = class(TcxWinXPLookAndFeelPainter)
  public
    function LookAndFeelName: string; override;
  end;

  TcxCustomEditHelper = class Helper for TcxCustomEdit
    procedure SetAndPostEditValue(AValue: Variant);
    function IsPresent: Boolean;
  end;

  PcxCustomGridView = ^TcxCustomGridView;
  TcxCustomGridViewHelper  = class Helper for TcxCustomGridView
    function GetFocusedClone(AFocusedMasterClone: PcxCustomGridView = nil): TcxCustomGridView;
    function GetActualClone(ARaiseIfNotFound: Boolean = True): TcxCustomGridView;
    function Version: Word;
  end;

  TcxCustomImageComboBoxPropertiesHelper = class Helper for TcxCustomImageComboBoxProperties
  public
    function FindImageIndexByValue(const AValue: Variant): TcxImageIndex;
  end;

  TcxCustomGridTableViewHelper = class Helper for TcxCustomGridTableView
  private
    function GetVisibleRowIndex: Integer;
    procedure SetVisibleRowIndex(const AValue: Integer);

    function GetFilteredRecordIndex: Integer;
    procedure SetFilteredRecordIndex(const AValue: Integer);

    function GetScrollBarPos: Integer;
    procedure SetScrollBarPos(const AValue: Integer);

{$IFDEF BASEDATAMODULE_SUPPORT}
    function GetGridBookmark: TDataSourceBookmark;
  protected
    procedure SetGridBookmark(const Value: TDataSourceBookmark); overload;
{$ENDIF}
  public
{$IFDEF BASEDATAMODULE_SUPPORT}
    procedure SetGridBookmark(const AValue: TDataSourceBookmark; ARestores: TDataSourceBookmarkRestores); overload;
    procedure BeginUpdate(var AStateBookmark: TDataSourceStateBookmark); overload;
    procedure EndUpdate(const AStateBookmark : TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores = DataSetStateBookmarkRestores_All; ADataSourceRestore: TDataSourceBookmarkRestores = DataSourceBookmarkRestores_All); overload;
    procedure RefreshDataSet;
{$ENDIF}

    function  ClientToGrid(P: TPoint): TPoint;
    function  ClientRectToGridRect(R: TRect): TRect;

    property  FilteredRecordIndex: Integer read GetFilteredRecordIndex write SetFilteredRecordIndex;
    property  ScrollBarPos: Integer read GetScrollBarPos write SetScrollBarPos;
    property  VisibleRowIndex: Integer read GetVisibleRowIndex write SetVisibleRowIndex;
{$IFDEF BASEDATAMODULE_SUPPORT}
    property  GridBookmark: TDataSourceBookmark read GetGridBookmark write SetGridBookmark;
{$ENDIF}
  end;

  TcxGridTableViewHelper = class Helper for TcxGridTableView
{$IFDEF BASEDATAMODULE_SUPPORT}
  private
    procedure SetFocusedRecordValues(AItemNames: String; AValues: Variant); overload;
{$ENDIF}
  public
    function GetActive: Boolean;
    function FindItemByFieldName(const AFieldName: String): TcxCustomGridTableItem;
{$IFDEF BASEDATAMODULE_SUPPORT}
    function GetFocusedRecordValues(const AItemNames: String; AUnknownItemNames: PString): Variant; overload;
    function GetFocusedRecordValues(AItemNames: String): Variant; overload;
{$ENDIF}

    function GetViewInfoHeight: Integer;
    function GetViewInfoWidth: Integer;

{$IFDEF BASEDATAMODULE_SUPPORT}
    property FocusedRecordValues[AItemNames: String]: Variant read GetFocusedRecordValues write SetFocusedRecordValues;
{$ENDIF}
  end;

  TcxGridDBTableViewHelper  = class Helper for TcxGridDBTableView
    procedure AutoCreateColumns;
  end;

  TcxGridDBBandedTableViewHelper  = class Helper for TcxGridDBBandedTableView
    procedure AutoCreateColumns;
  end;

  TcxGridDataControllerHelper = class Helper for TcxGridDataController
    function FindItemByName(const AItemName: String): TcxCustomGridTableItem;
    function GetRecordValues(ARecordIndex: Integer; const AFields: String; AUnknownFields: PString = nil): Variant;
  end;

  TcxGridDBDataControllerHelper = class Helper for TcxGridDBDataController
    function GetRecordValues(ARecordIndex: Integer; const AFields: String; AUnknownFields: PString = nil): Variant;
    procedure DeleteFocusedRow;
  end;

  TcxCustomGridTableItemHelper = class Helper for TcxCustomGridTableItem
  private
    procedure SetFocusedValue(const Value: Variant);
    function GetFocusedValue: Variant;
  public
    property FocusedValue: Variant read GetFocusedValue write SetFocusedValue;
  end;

  TcxCustomEditorRowPropertiesHelper = class Helper for TcxCustomEditorRowProperties
  private
    procedure SetFocusedValue(const AValue: Variant);
    function GetFocusedValue: Variant;
  public
    property FocusedValue: Variant read GetFocusedValue write SetFocusedValue;
  end;

{
  TcxDBTreeListDataControllerHelper = class Helper for TcxDBTreeListDataController
    function GetRecordValues(ARecordIndex: Integer; const AFields: String; AUnknownFields: PString = nil): Variant;
  end;
}

  TcxCustomTreeListHelper = class Helper for TcxCustomTreeList
    function Version: Word;
  end;

  TcxTreeListNodeHelper = class Helper for TcxTreeListNode
    procedure SetCheckGroupType(AValue: TcxTreeListNodeCheckGroupType);
  end;

  TcxDBTreeListHelper = class Helper for TcxDBTreeList
  private
    function GetVisibleRowIndex: Integer;
    procedure SetVisibleRowIndex(const AValue: Integer);
    function GetFilteredRecordIndex: Integer;
    procedure SetFilteredRecordIndex(const AValue: Integer);
{$IFDEF BASEDATAMODULE_SUPPORT}
    procedure SetFocusedRecordValues(AItemNames: String; const AValues: Variant);
    function GetCheckedRecordValues: String;
    function GetGridBookmark: TDataSourceBookmark;
  protected
    procedure SetGridBookmark(const Value: TDataSourceBookmark); overload;
{$ENDIF}
  public
{$IFDEF BASEDATAMODULE_SUPPORT}
    procedure SetGridBookmark(const AValue: TDataSourceBookmark; ARestores: TDataSourceBookmarkRestores); overload;
    procedure BeginUpdate(var AStateBookmark: TDataSourceStateBookmark); overload;
    procedure EndUpdate(const AStateBookmark : TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores = DataSetStateBookmarkRestores_All; ADataSourceRestore: TDataSourceBookmarkRestores = DataSourceBookmarkRestores_All); overload;
    procedure RefreshDataSet;
{$ENDIF}

    property  FilteredRecordIndex: Integer read GetFilteredRecordIndex write SetFilteredRecordIndex;
    property  VisibleRowIndex: Integer read GetVisibleRowIndex write SetVisibleRowIndex;
{$IFDEF BASEDATAMODULE_SUPPORT}
    property  GridBookmark: TDataSourceBookmark read GetGridBookmark write SetGridBookmark;
{$ENDIF}
  public
    function GetActive: Boolean;
{$IFDEF BASEDATAMODULE_SUPPORT}
    function GetFocusedRecordValues(const AItemNames: String; var AUnknownItemNames: String): Variant; overload;
    function GetFocusedRecordValues(AItemNames: String): Variant; overload;
    function GetRecordValues(ARecordIndex: Integer; const AFields: String; AUnknownFields: PString = nil): Variant;
    property FocusedRecordValues[AItemNames: String]: Variant read GetFocusedRecordValues write SetFocusedRecordValues;
    property CheckedValues: String read GetCheckedRecordValues;
{$ENDIF}
  end;

  TcxDBTreeListColumnHelper = class Helper for TcxDBTreeListColumn
  private
    procedure SetFocusedValue(const Value: Variant);
    function GetFocusedValue: Variant;
  public
    property FocusedValue: Variant read GetFocusedValue write SetFocusedValue;
  end;

  TcxDBVerticalGridHelper = class Helper for TcxDBVerticalGrid
  private
{$IFDEF BASEDATAMODULE_SUPPORT}
    function  GetFocusedRecordValues(const AItemNames: String; var AUnknownItemNames: String): Variant; overload;
  protected
    function  GetFocusedRecordValues(AItemNames: String): Variant; overload;
    procedure SetFocusedRecordValues(AItemNames: String; const AValues: Variant);
    procedure SetGridBookmark(const Value: TDataSourceBookmark); overload;
{$ENDIF}
  public
    function GetActive: Boolean;
{$IFDEF BASEDATAMODULE_SUPPORT}
    function  GetGridBookmark: TDataSourceBookmark;
    procedure SetGridBookmark(const AValue: TDataSourceBookmark; ARestores: TDataSourceBookmarkRestores); overload;
    procedure BeginUpdate(var AStateBookmark: TDataSourceStateBookmark); overload;
    procedure EndUpdate(const AStateBookmark : TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores = DataSetStateBookmarkRestores_All; ADataSourceRestore: TDataSourceBookmarkRestores = DataSourceBookmarkRestores_All); overload;
    procedure RefreshDataSet;

    property FocusedRecordValues[AItemNames: String]: Variant read GetFocusedRecordValues write SetFocusedRecordValues;
    property GridBookmark: TDataSourceBookmark read GetGridBookmark write SetGridBookmark;
{$ENDIF}
  end;

  TcxNamedImageCollectionItem = class(TcxImageCollectionItem)
  private
    FImageName: String;
  published
    property ImageName: String read FImageName write FImageName;
  end;

  TcxNamedImageCollection = class(TcxImageCollection)
  public
    function FindPicture(AImageName: String): TPicture;
  end;

  TcxRichEditHelper = class Helper for TcxRichEdit
    procedure SetFontStyle(AStyle: TFontStyle; ASet: Boolean);
    function GetHTMLText: String;
    function GetRTF: String;
    procedure AddFormatedTextAtBegin(AText, AFontName: string; AColor, AFontSize: Integer; AStyle: TFontStyles);
  end;

  function GetTextEndEllipsis(const AText: String; Canvas: TCanvas; Width: Integer): String;

{ TreeView Overloades }
  function  TreeSavePosition(const ATree: TTreeView): String;
  function  TreeRestorePosition(const ATree: TTreeView; APosition: String): Boolean;

{ Export To Excel }
  function  ExportCxGridToExcel(AGrid: TcxGrid; const AFileName: String = ''; const ATitle: String = ''; AUseNativeFormat: Boolean = True): Boolean;
  function  ExportCxTreeToExcel(ATree: TcxDBTreeList; const AFileName: String = ''; AUseNativeFormat: Boolean = True) : Boolean;

  procedure cxColumnsResize(Sender: TcxGridTableView; AResizingColumn: TcxGridColumn = nil);
  function  cxTreeIsCellClick(ATreeList: TcxCustomTreeList): Boolean;

implementation

uses
  SBaseConstants
  , SBaseFileUtils
{$IFDEF BASEFORM_SUPPORT}
  , SBaseForm
{$ENDIF}
  , cxMDDBTreeList, cxMDGrid;

type
  TcxCustomGridViewCrack = class(TcxCustomGridView);
  TcxCustomGridTableItemCrack = class(TcxCustomGridTableItem);
  TcxCustomDataControllerCrack = class(TcxCustomDataController);
  TcxCustomItemDataBindingCrack = class(TcxCustomItemDataBinding);
  TcxCustomEditCrack = class(TcxCustomEdit);
  TcxEditDataBindingCrack = class(TcxEditDataBinding);
  TcxCustomDataControllerInfoCrack = class(TcxCustomDataControllerInfo);
  TcxGridMasterDataRowCrack = class(TcxGridMasterDataRow);
  TcxCustomGridTableControllerCrack = class(TcxCustomGridTableController);
  TcxCustomInplaceEditContainerCrack = class(TcxCustomInplaceEditContainer);
  TcxGridTableViewInfoCrack = class(TcxGridTableViewInfo);

{ TcxApplicationLookAndFeelPainter }

function TcxApplicationLookAndFeelPainter.LookAndFeelName: string;
begin
  Result := SConst_Default;
end;

{ TcxCustomEditHelper }

function TcxCustomEditHelper.IsPresent: Boolean;
begin
  Result := VarIsPresent(EditValue);
end;

procedure TcxCustomEditHelper.SetAndPostEditValue(AValue: Variant);
begin
  with TcxEditDataBindingCrack(TcxCustomEditCrack(Self).DataBinding) do
    if not SetEditMode then
      raise Exception.Create(Self.Name + ': Cannot set EditMode');

  EditValue := AValue;
  PostEditValue;
end;

{ TcxImageNamedCollection }

function TcxNamedImageCollection.FindPicture(AImageName: String): TPicture;
var
  I: Integer;
  LcxImageCollectionItem: TcxImageCollectionItem;
begin
  Result := nil;
  for I := 0 to Items.Count-1 do begin
    LcxImageCollectionItem := Items[I];
    if LcxImageCollectionItem is TcxNamedImageCollectionItem then
      with TcxNamedImageCollectionItem(LcxImageCollectionItem) do
        if ImageName = AImageName then begin
          Result := Picture;
          Exit
        end;

  end;
end;

function GetTextEndEllipsis(const AText: String; Canvas: TCanvas; Width: Integer): String;
var
  ATextWidth: Integer;
begin
  Result := AText;
  ATextWidth := Canvas.TextWidth(AText);
  if ATextWidth > Width then
    begin
      while (ATextWidth > Width) and (Result <> '') do
      begin
        ATextWidth := Canvas.TextWidth(Result + '...');
        Delete(Result, Length(Result), 1);
      end;
      if Result <> AText then Result := Result + '...';
    end;
end;

{}

function TreeSavePosition(const ATree: TTreeView): String;
var
  FNode: TTreeNode;
begin
  Result := '';
  FNode := ATree.Selected;
  while FNode <> nil do begin
    Result := IntToStr(Integer(FNode.Data)) + ';' + Result;
    FNode := FNode.Parent
  end;
end;

function TreeRestorePosition(const ATree: TTreeView; APosition: String): Boolean;
var
  i: Integer;
  FData: Integer;
  function RestoreNextLevel(const ANode: TTreeNode): Boolean;
  var
    i: Integer;
  begin
    Result := True;
    if APosition <> '' then begin
      FData := ExtractAndRemoveValue(APosition, ';', ftInteger);
      for i := Pred(ANode.Count) downto 0 do with ANode.Item[i] do
        if Integer(Data) = FData then begin
          if RestoreNextLevel(ANode.Item[i]) then
            Selected := True;
          Result := False;
          Exit
        end;
    end;
  end;
begin
  Result := False;
  if APosition <> '' then begin
    FData := ExtractAndRemoveValue(APosition, ';', ftInteger);
    for i := Pred(ATree.Items.Count) downto 0 do
      with ATree.Items[0] do
        if (Level = 0) and (Integer(Data) = FData) then begin
          if RestoreNextLevel(ATree.Items[0]) then
            Selected := True;
          Result := APosition = '';
          Exit
        end;
  end;
end;

function ExcelColumn(n: Integer): String;
var
  a,z: Integer;
begin
  a := Ord('A');
  z := Ord('Z');
  if a + n - 1 <= z then
    Result := chr(a + n - 1)
  else
    Result := chr(a + ((n-1) div (z-a+1)) - 1) + chr(a + ((n-1) mod (z-a+1)));
end;

function FormatFloatToExcelFormatFloat(Format: String; Separator : char = '.'): String;
var
  i,c: Integer;
  Text: String;
  ASeparator : char;
begin
  Result := '';
  i := 1; c := 1;
  Format := TRegEx.Replace(Format, '''(.)''', '$1', []);
  while i <= Length(Format) do begin
    if Format[i] = ',' then begin
      Result := Result + '# ##0';
      Format := TRegEx.Replace(Copy(Format, Succ(i), Length(Format) - i), '^[0#]+', '', []);
      i := 1;
    end;
    if (i >= Length(Format)) or (Format[i] = ';') then begin
      if (i <= Length(Format)) and (Format[i] = ';') then begin
        Text := Copy(Format,1,Pred(i)); Format := Copy(Format,Succ(i),Length(Format)-i);
      end else begin
        Text := Format; Format := '';
      end;
      Result := Result + Text;
      i := 1; Inc(c);
      if c>3 then break
      else if Length(Format)>0 then Result := Result + ';';
    end else
      Inc(i);
  end;
  while (Length(Result)>0) and (Result[Length(Result)]=';') do SetLength(Result,Pred(Length(Result)));

  if Separator = '.' then ASeparator := ','
  else ASeparator := '.';

  Result := StringReplace(Result, ASeparator, Separator, []);
end;

procedure ExportGrid4ToExcelEx(const AFileName: string; AGrid: TcxGrid; AExpand: Boolean; ASaveAll: Boolean; AUseNativeFormat: Boolean; const ATitle: String);
type
  TExportData = packed record
    Index: Integer;
    Width: Integer;
  end;
var
  I, J, LCount, LNewWidth, LWidth: Integer;
  LExportData: Array of TExportData;
  LView: TcxCustomGridTableView;

  procedure _Export;
  begin
    if ExtractFileExt(AFileName) = SFileExt_Xlsx then
      cxGridExportLink.ExportGridToXLSX(AFileName, AGrid, AExpand, ASaveAll, AUseNativeFormat, '', nil, ATitle)
    else
      cxGridExportLink.ExportGridToExcel(AFileName, AGrid, AExpand, ASaveAll, AUseNativeFormat, '', nil, ATitle)
  end;
begin
  if Assigned(AGrid.ActiveView) and (AGrid.ActiveView is TcxCustomGridTableView) then
    begin
      LView  := AGrid.ActiveView as TcxCustomGridTableView;
      LCount := 0;
      for I := Pred(LView.ItemCount) downto 0 do
        with TcxCustomGridTableItemCrack(LView.Items[I]) do
          if Properties is TcxImageComboBoxProperties then begin
            LWidth := Width;
            with TcxImageComboBoxProperties(Properties) do
              for J := 0 to Pred(Items.Count) do begin
                LNewWidth := AGrid.Canvas.TextWidth(Items[j].Description + 'W');
                if LNewWidth > LWidth then LWidth := LNewWidth;
              end;

            if Width <> LWidth then begin
              SetLength(LExportData, Succ(LCount));
              LExportData[LCount].Index := I;
              LExportData[LCount].Width := Width;
              Inc(LCount);
              Width := LWidth;
            end;
          end;

      try
        _Export;
      finally
        for I := Pred(LCount) downto 0 do
          with TcxCustomGridTableItemCrack(LView.Items[LExportData[I].Index]) do begin
            Width := LExportData[I].Width;
          end;
      end;
    end
  else
    _Export;
end;

function ExportCxGridToExcel(AGrid: TcxGrid; const AFileName: String = ''; const ATitle: String = ''; AUseNativeFormat: Boolean = True): Boolean;
var
  TmpFile: String;
  IExcel: Excel_TLB._Application;
  IWorkbook: Excel_TLB._Workbook;
begin
  Result := True;
  try
    if AFileName.IsEmpty then      
      TmpFile := GetTempFile(SFileExt_Xls)
    else
      TmpFile := GetTempPath + PrepareFileName(AFileName, SFileExt_Xls);

    ExportGrid4ToExcelEx(TmpFile, AGrid, False, True, AUseNativeFormat, ATitle);

    IExcel := CreateComObject(Excel_TLB.CLASS_ExcelApplication) as Excel_TLB._Application;
    IExcel.WindowState[LOCALE_USER_DEFAULT] := xlNormal;
    IExcel.Visible[LOCALE_USER_DEFAULT] := True;

    IWorkBook := IExcel.Workbooks.Open(TmpFile, EmptyParam, EmptyParam,
        EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
        EmptyParam, EmptyParam, EmptyParam, EmptyParam, False, EmptyParam, EmptyParam, LOCALE_USER_DEFAULT);

  except on E:Exception do begin
{$IFDEF BASEFORM_SUPPORT}
    SBaseMainFormMDIInterface.Log(lgError, E.Message);
{$ENDIF}
    Result := False
  end end;
end;

function ExportCxTreeToExcel(ATree: TcxDBTreeList; const AFileName: String = ''; AUseNativeFormat: Boolean = True) : Boolean;
var
  TmpFile: String;
  IExcel: Excel_TLB._Application;
  IWorkbook: Excel_TLB._Workbook;
begin
  Result := True;
  try
    if AFileName.IsEmpty then
      TmpFile := GetTempFile(SFileExt_Xls)
    else
      TmpFile := GetTempPath + PrepareFileName(AFileName, SFileExt_Xls);

    if ExtractFileExt(AFileName) = SFileExt_Xlsx then
      cxExportTLToXLSX(TmpFile, ATree, False, True, AUseNativeFormat)
    else
      cxExportTLToExcel(TmpFile, ATree, False, True, AUseNativeFormat);

    IExcel := CreateComObject(Excel_TLB.CLASS_ExcelApplication) as Excel_TLB._Application;
    IExcel.WindowState[LOCALE_USER_DEFAULT] := xlNormal;
    IExcel.Visible[LOCALE_USER_DEFAULT] := True;

    IWorkBook := IExcel.Workbooks.Open(TmpFile, EmptyParam, EmptyParam,
        EmptyParam, EmptyParam, EmptyParam, EmptyParam, EmptyParam,
        EmptyParam, EmptyParam, EmptyParam, EmptyParam, False, EmptyParam, EmptyParam, LOCALE_USER_DEFAULT);

  except on E:Exception do begin
{$IFDEF BASEFORM_SUPPORT}
    SBaseMainFormMDIInterface.Log(lgError, E.Message);
{$ENDIF}
    Result := False
  end end;
end;

procedure cxColumnsResize(Sender: TcxGridTableView; AResizingColumn: TcxGridColumn = nil);
{ѕроцедура автоматически выравнивает ширину колонок по ширине грида,
 более правильна€ альтернатива методу ColumnAutoWidth.
 ќб€зательно одна из колонок должна быть "жертвой" (Options.HorzSizing := False)
 - именно ее свободным пространством будут пользоватьс€ остальные колонки.
 ¬ качестве параметров передаетс€ грид и (необ€зательный параметр) -
 измен€ема€ колонка. ƒанную процедуру необходимо запускать по событию
 грида OnColumnSizeChanged и также по любому изменению размеров грида.
}
var
  I: Integer;
  LVariableColumn: TcxGridColumn;
  NewWidth: Integer;
  ColumnsWidth: Integer;
begin
  Assert(not Sender.OptionsView.ColumnAutoWidth);
{
  if Sender.OptionsView.ColumnAutoWidth then
    Sender.OptionsView.ColumnAutoWidth := False;
}

  //-- »щем колонку  - жертву и одновременно высчитываем то что ей осталось
  LVariableColumn := nil;
  ColumnsWidth := 0;
  for I := Pred(Sender.VisibleColumnCount) downto 0  do begin
    if Sender.VisibleColumns[I].Options.HorzSizing then
      ColumnsWidth := ColumnsWidth + Sender.VisibleColumns[I].Width
    else
      LVariableColumn := Sender.VisibleColumns[I]
  end;
  if LVariableColumn = nil then Exit;

  NewWidth := Sender.ViewInfo.ClientWidth - ColumnsWidth;

  //-- ≈сли перебор
  if NewWidth < LVariableColumn.MinWidth then begin
    NewWidth := LVariableColumn.MinWidth;
    //-- ¬ысчитываем новую ширину измененной колонки
    if AResizingColumn <> nil then
      AResizingColumn.Width := Sender.ViewInfo.ClientWidth - (ColumnsWidth  + LVariableColumn.MinWidth - AResizingColumn.Width);
  end;
  LVariableColumn.Width := NewWidth;
end;

function cxTreeIsCellClick(ATreeList: TcxCustomTreeList): Boolean;
var
  R: TRect;
  P: TPoint;
begin
  Result := False;
  if (ATreeList.FocusedNode <> nil) and (ATreeList.FocusedColumn <> nil) then begin
    GetCursorPos(P);
    P := ATreeList.ScreenToClient(P);
    R := ATreeList.CellRect(ATreeList.FocusedNode, ATreeList.FocusedColumn);
    if Assigned(ATreeList.Images) then begin
      R.Left := R.Left - ATreeList.Images.Width;
      R.Top := R.Top - ATreeList.Images.Height;
    end;
    if PtInRect (R, P) then Result := True;
  end;
end;

procedure AutoCreateColumns(AGridView: TcxGridTableView; ADataSet: TDataSet);
var
  I: Integer;
  LColumn: TcxGridColumn;
  LDataBinding: TcxGridItemDBDataBinding;
  LField: TField;
  LWidth: Integer;
begin
  AGridView.BeginUpdate;
  try
    for I := 0 to ADataSet.FieldCount - 1 do
    begin
      LColumn := AGridView.CreateColumn;
      LDataBinding := TcxGridItemDBDataBinding(GetOrdProp(LColumn, SConst_DataBinding));
      LField := ADataSet.Fields[I];
      LWidth := 0;
      case LField.DataType of
        ftString:
          begin
            LWidth := TStringField(LField).Size;
            if LWidth > 40 then
              LWidth := 40;
            LWidth := LWidth * 10;
          end;
        ftGuid:
          LWidth := 260;
        ftDateTime:
          with LColumn do begin
            HeaderAlignmentHorz := taCenter;
            PropertiesClass := TcxDateEditProperties;
            with TcxDateEditProperties(Properties) do begin
              Kind      := ckDateTime;
              ShowTime  := True;
            end;
          end;
        ftFMTBcd:
          begin
            LWidth := TFMTBCDField(LField).Precision;
            if LWidth > 38 then
              LWidth := 38;
            LWidth := LWidth * 5;
            with LColumn do begin
              PropertiesClass := TcxCalcEditProperties;
              with TcxCalcEditProperties(Properties) do begin
                FmtBcd      := True;
                FmtBcdScale := TFMTBCDField(LField).Size;
              end;
            end;
          end;
      end;

      if LWidth > 0 then
        LColumn.Width := LWidth;
      LDataBinding.FieldName := LField.FieldName;
    end;
  finally
    AGridView.EndUpdate
  end;
end;

{ TcxGridDBTableViewHelper }

procedure TcxGridDBTableViewHelper.AutoCreateColumns;
begin
  SBaseCxUtils.AutoCreateColumns(Self, DataController.DataSet);
end;

{ TcxGridDataControllerHelper }

function TcxGridDataControllerHelper.FindItemByName(const AItemName: String): TcxCustomGridTableItem;
var
  I: Integer;
  LItem: TObject;
begin
  for I := 0 to ItemCount - 1 do begin
    LItem := GetItem(I);
    if (LItem is TcxCustomGridTableItem) and SameText(TcxCustomGridTableItem(LItem).Name, AItemName) then
      Exit(LItem as TcxCustomGridTableItem);
  end;
  Result := nil;
end;

function TcxGridDataControllerHelper.GetRecordValues(ARecordIndex: Integer; const AFields: String; AUnknownFields: PString): Variant;
var
  LItem       : TStringItem;
  LName       : String;
  LFound      : Boolean;
  LValue      : Variant;
  LTableItem  : TcxCustomGridTableItem;
begin
  Assert(ARecordIndex >= 0);

  if AUnknownFields <> nil then
    AUnknownFields^ := '';

  for LItem in SBaseStringFunctions.EnumStringItems(AFields) do begin
    ExtractNameAndAlias(LItem.Value, nil, @LName);
    LTableItem := FindItemByName(LName);
    if (LTableItem <> nil) then begin
      LValue := Values[ARecordIndex, LTableItem.Index];
      LFound := True;
    end else begin
      LValue := Null;
      LFound := False;
    end;

    if (LItem.Index = 0) and LItem.Eof then
      Result  := LValue
    else begin
      if LItem.Index = 0 then
        Result  := VarArrayOf([LValue])
      else begin
        VarArrayRedim(Result, LItem.Index);
        Result[LItem.Index] := LValue;
      end;
    end;

    if (not LFound) and (AUnknownFields <> nil) then
      AUnknownFields^ := ConcatIfNotEmpty(AUnknownFields^, ';', LItem.Value);
  end;
end;

{ TcxGridDBDataControllerHelper }

procedure TcxGridDBDataControllerHelper.DeleteFocusedRow;
begin
  if DataModeController.SyncMode then
    DeleteFocused
  else begin
    if DataSet.Locate(KeyFieldNames, GetRecordId(FocusedRecordIndex), []) then
      DataSet.Delete
    else
      Raise Exception.Create('DataSet.Locate failed');
  end;
end;

function TcxGridDBDataControllerHelper.GetRecordValues(ARecordIndex: Integer; const AFields: String; AUnknownFields: PString = nil): Variant;
var
  LItem       : TStringItem;
  LName       : String;
  LFound      : Boolean;
  LValue      : Variant;
  LTableItem  : TcxCustomGridTableItem;
begin
  Assert(ARecordIndex >= 0);

  if AUnknownFields <> nil then
    AUnknownFields^ := '';

  for LItem in SBaseStringFunctions.EnumStringItems(AFields) do begin
    ExtractNameAndAlias(LItem.Value, nil, @LName);
    LTableItem := GetItemByFieldName(LName);
    if (LTableItem <> nil) then begin
      LValue := Values[ARecordIndex, LTableItem.Index];
      LFound := True;
    end else if InArray(KeyFieldNames, LName, ';') then begin
      LValue := VarArrayRead(KeyFieldNames, GetRecordId(ARecordIndex), LName);
      LFound := True;
    end else begin
      LValue := Null;
      LFound := False;
    end;

    if (LItem.Index = 0) and LItem.Eof then
      Result  := LValue
    else begin
      if LItem.Index = 0 then
        Result  := VarArrayOf([LValue])
      else begin
        VarArrayRedim(Result, LItem.Index);
        Result[LItem.Index] := LValue;
      end;
    end;

    if (not LFound) and (AUnknownFields <> nil) then
      AUnknownFields^ := ConcatIfNotEmpty(AUnknownFields^, ';', LItem.Value);
  end;
end;

{ TcxDBTreeListHelper }

{$IFDEF BASEDATAMODULE_SUPPORT}
function TcxDBTreeListHelper.GetRecordValues(ARecordIndex: Integer; const AFields: String; AUnknownFields: PString): Variant;
var
  LItem   : TStringItem;
  LName   : String;
  LFound  : Boolean;
  LValue  : Variant;
  LColumn : TcxDBTreeListColumn;
  LNode   : TcxDBTreeListNode;
begin
  if AUnknownFields <> nil then
    AUnknownFields^ := '';

  LNode := TcxDBTreeListNode(AbsoluteItems[ARecordIndex]);
  for LItem in SBaseStringFunctions.EnumStringItems(AFields) do begin
    ExtractNameAndAlias(LItem.Value, nil, @LName);
    LColumn := GetColumnByFieldName(LName);

    if (LColumn <> nil) then begin
      if ARecordIndex >= 0 then
        LValue := LNode.Values[LColumn.ItemIndex]
      else
        LValue := Null;
      LFound := True;
    end else if InArray(DataController.KeyField, LName, ';') then begin
      LValue := VarArrayRead(DataController.KeyField, LNode.KeyValue, LName);
      LFound := True;
    end else if SameText(LName, SConst_Level) then begin
      LValue := LNode.Level;
      LFound := True;
    end else begin
      LValue := Null;
      LFound := False;
    end;

    if (LItem.Index = 0) and LItem.Eof then
      Result  := LValue
    else begin
      if LItem.Index = 0 then
        Result  := VarArrayOf([LValue])
      else begin
        VarArrayRedim(Result, LItem.Index);
        Result[LItem.Index] := LValue;
      end;
    end;

    if (not LFound) and (AUnknownFields <> nil) then
      AUnknownFields^ := ConcatIfNotEmpty(AUnknownFields^, ';', LItem.Value);
  end;
end;

procedure TcxDBTreeListHelper.RefreshDataSet;
var
  LBookmark: TDataSourceStateBookmark;
  I: Integer;
  LAllow: Boolean;
begin
  if Assigned(DataController.DataSource) and Assigned(DataController.DataSource.DataSet) then begin
    BeginUpdate(LBookmark);
    try
      with DataController.DataSource.DataSet.Source do begin
        Close;
        Open;
      end;
    finally
      EndUpdate(LBookmark);
    end;

    if Assigned(OnCollapsing) then
      for I := Pred(AbsoluteCount) downto 0 do begin
        LAllow := True;
        OnCollapsing(Self, AbsoluteItems[I], LAllow);
        if not LAllow then
          AbsoluteItems[I].Expanded := True;
      end;
  end;
end;
{$ENDIF}

function TcxDBTreeListHelper.GetVisibleRowIndex: Integer;
begin
  if FocusedNode <> nil then
    Result := FocusedNode.AbsoluteIndex - TopVisibleNode.AbsoluteIndex
  else
    Result := -1;
end;

procedure TcxDBTreeListHelper.SetVisibleRowIndex(const AValue: Integer);
var
  FTopIndex: Integer;
begin
  if AValue >= 0 then begin
    if FocusedNode <> nil then
      FTopIndex := FocusedNode.AbsoluteIndex - AValue
    else
      FTopIndex := -1;

    if FTopIndex >= 0 then
      TopVisibleNode := AbsoluteItems[FTopIndex];
  end;
end;

{$IFDEF BASEDATAMODULE_SUPPORT}
procedure TcxDBTreeListHelper.BeginUpdate(var AStateBookmark: TDataSourceStateBookmark);
var
  LDataSet: TDataSet;
begin
  if Assigned(DataController) and Assigned(DataController.DataSource) then
    LDataSet := DataController.DataSource.DataSet.Source
  else
    LDataSet := nil;

  if Assigned(LDataSet) then
    LDataSet.BeginUpdate(AStateBookmark)
  else
    AStateBookmark.DataSetState.Clear;

  try
    AStateBookmark.DataSource := GetGridBookmark;
    BeginUpdate;
  except
    EndUpdate;
    Raise
  end;
end;

procedure TcxDBTreeListHelper.EndUpdate(const AStateBookmark: TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores);
var
  LDataSet: TDataSet;
//  LSourceDataSet: TDataSet;
begin
  LDataSet := nil;
  try
    if Assigned(DataController.DataSource) then
      LDataSet := DataController.DataSource.DataSet;
//    LSourceDataSet := ;

    if Assigned(LDataSet) then
      LDataSet.Source.EndUpdate(AStateBookmark, ADataSetStateRestore - [sbrDisableCount], []);

    if (sbrKeyFieldValues in ADataSourceRestore)
      and (OptionsData.SyncMode)
      and Assigned(LDataSet) and (LDataSet.RecordCount > 0)
      and VarIsPresent(AStateBookmark.DataSource.KeyFieldValues)
      and (not VarIsEqual(LDataSet.FieldValues[DataController.KeyField], AStateBookmark.DataSource.KeyFieldValues))
    then
      LDataSet.Locate(DataController.KeyField, AStateBookmark.DataSource.KeyFieldValues, []);
  finally
//    if sbrDisableCount in ADataSetStateRestore then begin
      if Assigned(LDataSet) then
        LDataSet.EnableControls;
//    end;
    EndUpdate;
  end;

  SetGridBookmark(AStateBookmark.DataSource, ADataSourceRestore)
end;
{$ENDIF}

function TcxDBTreeListHelper.GetActive: Boolean;
begin
  with DataController do
    Result := Assigned(DataSet) and DataSet.Active
end;

function TcxDBTreeListHelper.GetFilteredRecordIndex: Integer;
begin
  if FocusedNode <> nil then
    Result := FocusedNode.AbsoluteIndex
  else
    Result := -1
end;

procedure TcxDBTreeListHelper.SetFilteredRecordIndex(const AValue: Integer);
begin
  if AValue >= 0 then begin
    if Pred(AbsoluteCount) < AValue then begin
      if LastNode <> nil then
        LastNode.Focused := True
    end else
      AbsoluteItems[AValue].Focused := True;
  end;
end;

{$IFDEF BASEDATAMODULE_SUPPORT}
function TcxDBTreeListHelper.GetGridBookmark: TDataSourceBookmark;
begin
  Result.RecordIndex  := FilteredRecordIndex;
  Result.RowIndex     := VisibleRowIndex;

  if (Result.RecordIndex >= 0) then
    Result.KeyFieldValues := TcxDBTreeListNode(FocusedNode).KeyValue
  else
    Result.KeyFieldValues := Unassigned;

  Result.Reserved    := Unassigned;
end;

procedure TcxDBTreeListHelper.SetGridBookmark(const Value: TDataSourceBookmark);
begin
  SetGridBookmark(Value, DataSourceBookmarkRestores_All);
end;

procedure TcxDBTreeListHelper.SetGridBookmark(const AValue: TDataSourceBookmark; ARestores: TDataSourceBookmarkRestores);
var
  LFound: Boolean;
begin
  if (ARestores = []) or (AbsoluteCount = 0) then Exit;

  if (sbrKeyFieldValues in ARestores) and VarIsPresent(AValue.KeyFieldValues) then begin
    LFound := VarIsEqual(TcxDBTreeListNode(FocusedNode).KeyValue, AValue.KeyFieldValues);
    if not LFound then
      LFound := DataController.LocateByKey(AValue.KeyFieldValues);
  end else
    LFound := False;

  if (not LFound) and (sbrRecordIndex in ARestores) then
    FilteredRecordIndex := AValue.RecordIndex;

  if (sbrRowIndex in ARestores) then
    VisibleRowIndex := AValue.RowIndex;
end;

function TcxDBTreeListHelper.GetFocusedRecordValues(const AItemNames: String; var AUnknownItemNames: String): Variant;
var
  LFilteredRecordIndex: Integer;
begin
  LFilteredRecordIndex := FilteredRecordIndex;
  Assert(LFilteredRecordIndex >= 0);

  if OptionsData.SyncMode then
    Result := DataController.DataSet.Fields.GetValues(AItemNames, @AUnknownItemNames)
  else
    Result := GetRecordValues(FilteredRecordIndex, AItemNames, @AUnknownItemNames);
end;

function TcxDBTreeListHelper.GetCheckedRecordValues: String;
var
  I, LKeyIndex: Integer;
const
  RecordTag = 'RECORD';

  function AddChildren(ANode: TcxTreeListNode): String;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to ANode.Count - 1 do
    begin
      case ANode.Items[I].CheckState of
        cbsChecked:
          Result := Result +  ' <' + RecordTag + ' ' + UpperCase(DataController.KeyField) +
           '="' +  VarToStr(ANode.Items[I].Values[LKeyIndex]) + '"/>';
        cbsGrayed:
          Result := Result + AddChildren(ANode.Items[I])
      end;
    end;
  end;
begin
  LKeyIndex := -1;
  for I := 0 to ColumnCount - 1 do
    if SameText(TcxDBItemDataBinding(Columns[I].DataBinding).FieldName, DataController.KeyField) then begin
      LKeyIndex := I;
      Break;
    end;

  Result := '';
  for I := 0 to Count - 1  do begin
    if Items[I].CheckState <> cbsUnchecked then
      Result := Result + AddChildren(Items[I]);
  end;

  Result := '<RECORDS>' + Result + '</RECORDS>'
end;


function TcxDBTreeListHelper.GetFocusedRecordValues(AItemNames: String): Variant;
var
  LUnknownItemNames: String;
begin
  Result := GetRecordValues(FilteredRecordIndex, AItemNames, @LUnknownItemNames);
  if LUnknownItemNames <> '' then
    Raise Exception.CreateFmt(MError_ColumnByFieldNotFound, [LUnknownItemNames]);
end;

procedure TcxDBTreeListHelper.SetFocusedRecordValues(AItemNames: String; const AValues: Variant);
var
  LItem   : TStringItem;
  LColumn : TcxDBTreeListColumn;
begin
  for LItem in SBaseStringFunctions.EnumStringItems(AItemNames) do begin
    LColumn := GetColumnByFieldName(LItem.Value);
    if LColumn = nil then
      Raise Exception.CreateFmt(MError_ColumnByFieldNotFound, [LItem.Value]);

    if (LItem.Index = 0) and LItem.Eof then
      LColumn.FocusedValue := AValues
    else
      LColumn.FocusedValue := AValues[LItem.Index];
  end;
end;
{$ENDIF}

{ TcxGridDBBandedTableViewHelper }

procedure TcxGridDBBandedTableViewHelper.AutoCreateColumns;
begin
  SBaseCxUtils.AutoCreateColumns(Self, DataController.DataSet);
end;

{ TcxCustomGridViewHelper }

function TcxCustomGridViewHelper.GetFocusedClone(AFocusedMasterClone: PcxCustomGridView = nil): TcxCustomGridView;
var
  LIndex, LLevelIndex : Integer;
  LMasterPattern, LFocusedMasterClone: TcxCustomGridView;
begin
  Assert(Assigned(Self));
  LFocusedMasterClone := nil;

//  if (not IsPattern) then begin
{
    end else begin
      if (MasterGridView <> nil) and MasterGridView.Focused then
        LFocusedMasterClone := MasterGridView
      else
        LFocusedMasterClone := nil;
      Result := nil;
    end
}
  if Focused then begin
    LFocusedMasterClone := MasterGridView;
    Result := Self;
  end else begin
    LMasterPattern := MasterGridView;
    if LMasterPattern <> nil then //–азве так бывает?
      LMasterPattern := LMasterPattern.PatternGridView;

    if Level = nil then begin
      if AFocusedMasterClone <> nil then
        AFocusedMasterClone^ := nil;
      Exit(nil);
    end;
    LLevelIndex := TcxGridLevel(Level).Level;

    Result := TcxCustomGrid(Control).FocusedView;
    if Result.Level <> nil then begin
      LIndex := TcxGridLevel(Result.Level).Level;

      if LIndex < LLevelIndex then begin
        if (LIndex < LLevelIndex) and (Result.PatternGridView = LMasterPattern) then
          LFocusedMasterClone := Result;
        Result := nil;
      end else begin
        while LIndex > LLevelIndex do begin
          Result := Result.MasterGridView;
          LIndex := TcxGridLevel(Result.Level).Level;
        end;

        if Result.PatternGridView <> Self then begin
          Result := Result.MasterGridView;
          if (Result <> nil) and (Result.PatternGridView = LMasterPattern) then
            LFocusedMasterClone := Result;
          Result := nil;
        end else
          LFocusedMasterClone := Result.MasterGridView;
      end;
    end;
  end;

  if AFocusedMasterClone <> nil then
    AFocusedMasterClone^ := LFocusedMasterClone;
end;

function TcxCustomGridViewHelper.Version: Word;
begin
  if Self is TcxMDGridDBTableView then
    Result := TcxMDGridDBTableView(Self).Version
  else if Self is TcxMDGridDBBandedTableView then
    Result := TcxMDGridDBBandedTableView(Self).Version
  else
    Result := 0;
end;

{$IFDEF BASEDATAMODULE_SUPPORT}
function TcxGridTableViewHelper.GetFocusedRecordValues(const AItemNames: String; AUnknownItemNames: PString): Variant;
var
  LFields: TFields;
begin
  if GetDataControllerClass.InheritsFrom(TcxGridDBDataController) then
    with TcxGridDBDataController(DataController) do
      if DataModeController.SyncMode then begin
        if Assigned(DataSet) then
          LFields := DataSet.Fields
        else
          LFields := nil;
        Result := LFields.GetValues(AItemNames, AUnknownItemNames)
      end else
        Result := GetRecordValues(FocusedRecordIndex, AItemNames, AUnknownItemNames)
  else
    with TcxGridDataController(DataController) do Result := GetRecordValues(FocusedRecordIndex, AItemNames, AUnknownItemNames)
end;

function TcxGridTableViewHelper.GetFocusedRecordValues(AItemNames: String): Variant;
var
  LUnknownItemNames: String;
begin
  Result := GetFocusedRecordValues(AItemNames, @LUnknownItemNames);
  if not LUnknownItemNames.IsEmpty then
    Raise Exception.CreateFmt(MError_ColumnByFieldNotFound, [LUnknownItemNames]);
end;
{$ENDIF}

function TcxGridTableViewHelper.GetActive: Boolean;
begin
  if GetDataControllerClass = TcxGridDBDataController then
    with TcxGridDBDataController(DataController) do
      Result := (ItemCount > 0) and Assigned(DataSet) and DataSet.Active
  else
    Result := ItemCount > 0
end;

function TcxGridTableViewHelper.GetViewInfoHeight: Integer;
var
  I         : Integer;
  LViewIno  : TcxCustomGridRecordViewInfo;
  // LHeight   : Integer;
  // LVisible  : Boolean;
begin
  Result := ViewInfo.Bounds.Height - ViewInfo.RecordsViewInfo.Bounds.Height;
  for I := 0 to ViewData.RecordCount-1 do begin
    LViewIno := ViewData.Records[I].ViewInfo;
    if Assigned(LViewIno) then
      Inc(Result, LViewIno.Height)
    else
      Inc(Result, 18)
  end;

{
  TcxGridTableViewInfoCrack(ViewInfo).CalculateHeight(Point(1000, 1000), LHeight, LVisible);
  if LHeight > Result then
    Result := LHeight;
}
end;

function TcxGridTableViewHelper.GetViewInfoWidth: Integer;
begin
  Result := ViewInfo.RecordsViewInfo.Bounds.Width;
end;

{$IFDEF BASEDATAMODULE_SUPPORT}
procedure TcxGridTableViewHelper.SetFocusedRecordValues(AItemNames: String; AValues: Variant);
var
  LItem     : TStringItem;
  LTableItem: TcxCustomGridTableItem;
begin
  for LItem in SBaseStringFunctions.EnumStringItems(AItemNames) do begin
    LTableItem := FindItemByFieldName(LItem.Value);
    if LTableItem = nil then
      Raise Exception.CreateFmt(MError_ColumnByFieldNotFound, [LItem.Value]);

    if (LItem.Index = 0) and LItem.Eof then
      LTableItem.FocusedValue := AValues
    else
      LTableItem.FocusedValue := AValues[LItem.Index];
  end;
end;
{$ENDIF}

function TcxGridTableViewHelper.FindItemByFieldName(const AFieldName: String): TcxCustomGridTableItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ItemCount - 1 do
    with Items[I] do
      if DataBinding is TcxGridItemDBDataBinding then begin
        if SameText(TcxGridItemDBDataBinding(DataBinding).FieldName, AFieldName) then begin
          Result := Items[I];
          Break;
        end else if SameText(Name, AFieldName) then begin
          Result := Items[I];
          Break;
        end
      end;
end;

function TcxCustomGridViewHelper.GetActualClone(ARaiseIfNotFound: Boolean = True): TcxCustomGridView;
var
  LFocusedMasterClone: TcxCustomGridView;
  LcxCustomDataController: TcxCustomDataController;
  LFocusedRecord: TcxCustomGridRecord;
  LFocusedRecordIndex: Integer;
  LDetailActiveRelationIndex: Integer;
begin
  Result := GetFocusedClone(@LFocusedMasterClone);

  if (Result = nil) and (LFocusedMasterClone <> nil) and (LFocusedMasterClone.DataController <> nil) then
    with LFocusedMasterClone.DataController do begin
      LFocusedRecord := TcxCustomGridTableController(LFocusedMasterClone.Controller).FocusedRecord;
      if (LFocusedRecord <> nil) and LFocusedRecord.Expanded then begin
        LFocusedRecordIndex := LFocusedRecord.RecordIndex;
        LDetailActiveRelationIndex := GetDetailActiveRelationIndex(LFocusedRecordIndex);
        LcxCustomDataController := GetDetailDataController(LFocusedRecordIndex, LDetailActiveRelationIndex);
        if LcxCustomDataController <> nil then
          Result := TcxGridDBDataController(LcxCustomDataController).GridView
        else
          Result := Self;
      end;
    end;

  Assert((Result <> nil) or (not ARaiseIfNotFound));
end;

{ TcxTreeListNodeHelper }

procedure TcxTreeListNodeHelper.SetCheckGroupType(AValue: TcxTreeListNodeCheckGroupType);
var
  I: Integer;
begin
  CheckGroupType := AValue;
  for I := 0 to Count - 1 do
    Items[I].CheckGroupType := AValue;
end;

{ TcxCustomGridTableItemHelper }

function TcxCustomGridTableItemHelper.GetFocusedValue: Variant;
begin
  Result := DataController.Values[DataController.FocusedRecordIndex, Index]
end;

procedure TcxCustomGridTableItemHelper.SetFocusedValue(const Value: Variant);
begin
  EditValue := Value
//  DataController.Values[DataController.FocusedRecordIndex, Index] := Value;
end;

{ TcxRichEditHelper }

procedure TcxRichEditHelper.AddFormatedTextAtBegin(AText, AFontName: string; AColor, AFontSize: Integer; AStyle: TFontStyles);
var
  L: Integer;
begin
  SelStart    := 0;
  SelLength   := 0;

  SelText     := AText;

  L := SelStart;
  SelStart    := 0;
  SelLength   := L;

  with SelAttributes do begin
    Name      := AFontName;
    Color     := AColor;
    Style     := AStyle;
    Size      := AFontSize;
  end;
end;

function TcxRichEditHelper.GetHTMLText: String;
begin
  Result := TRtf2HtmlConverter.Convert(TRichEdit(Self.InnerControl))
end;

function TcxRichEditHelper.GetRTF: String;
var
  LStream: TStringStream;
begin
  LStream := TStringStream.Create;
  Result := '';
  try
    (InnerControl as TRichEdit).PlainText := False;
    Lines.SaveToStream(LStream);
    Result := LStream.DataString;
  finally
    LStream.Free;
  end;
end;

procedure TcxRichEditHelper.SetFontStyle(AStyle: TFontStyle; ASet: Boolean);
var
  LFormat: TCharFormat2;
begin
  FillChar(LFormat, SizeOf(TCharFormat2), 0);
  LFormat.cbSize := SizeOf(TCharFormat2);

  with LFormat do
    case AStyle of
      fsBold:
      begin
        dwMask    := CFM_BOLD;
        dwEffects := CFE_BOLD;
      end;

      fsItalic:
      begin
        dwMask    := CFM_ITALIC;
        dwEffects := CFE_ITALIC;
      end;

      fsUnderline:
      begin
        dwMask    := CFM_UNDERLINE;
        dwEffects := CFE_UNDERLINE;
      end;

      fsStrikeOut:
      begin
        dwMask    := CFM_STRIKEOUT;
        dwEffects := CFE_STRIKEOUT;
      end;
    end;

  if not ASet then
    LFormat.dwEffects := 0;

  Messages.SendStructMessage
  (
    InnerControl.Handle,
    EM_SETCHARFORMAT,
    SCF_SELECTION,
    LFormat
  );
end;

{ TcxDBTreeListColumnHelper }

function TcxDBTreeListColumnHelper.GetFocusedValue: Variant;
begin
  Result := TreeList.AbsoluteItems[TreeList.FocusedNode.AbsoluteIndex].Values[ItemIndex];
end;

procedure TcxDBTreeListColumnHelper.SetFocusedValue(const Value: Variant);
begin
//  EditValue := Value
  DataController.SetEditValue(ItemIndex, Value, EditValueSource);
end;

{ TcxCustomGridTableViewHelper }

{$IFDEF BASEDATAMODULE_SUPPORT}
function TcxCustomGridTableViewHelper.GetGridBookmark: TDataSourceBookmark;
var
  I, J: Integer;
  LGrid: TcxCustomGridTableView;

  function VisibleIndex(AItem: TcxCustomGridTableItem): Integer;
  begin
    if AItem = nil then
      Result := -1
    else
      Result := AItem.VisibleIndex
  end;
begin
  Result.RecordIndex  := ScrollBarPos;
  Result.RowIndex     := VisibleRowIndex;

  if (Result.RecordIndex >= 0) then
    Result.KeyFieldValues := DataController.GetRecordId(DataController.FocusedRecordIndex) //LcxDBDataController.GetKeyFieldsValues
  else
    Result.KeyFieldValues := Unassigned;

  Result.Reserved    := Unassigned;
  if IsMaster and (not Focused) then with DataController do begin
    I := FocusedRecordIndex;
    if (I >= 0) and Controller.FocusedRecord.Expanded then begin
      J := GetDetailActiveRelationIndex(I);
      if J >= 0 then
        with TcxGridDBDataController(GetDetailDataController(I, J)) do begin
          LGrid := TcxCustomGridTableView(GetOwner);
          if LGrid.Focused then
            Result.Reserved := VarArrayOf([LGrid.PatternGridView.Name, VisibleIndex(LGrid.Controller.FocusedItem), GetRecordId(FocusedRecordIndex)]);
        end;
    end;
  end;
end;

procedure TcxCustomGridTableViewHelper.BeginUpdate(var AStateBookmark: TDataSourceStateBookmark);
var
  LDataSet: TDataSet;
begin
  if Assigned(DataController) and (DataController is TcxDBDataController) and Assigned(TcxDBDataController(DataController).DataSource) then
    LDataSet := TcxDBDataController(DataController).DataSource.DataSet.Source
  else
    LDataSet := nil;

  if Assigned(LDataSet) then
    LDataSet.BeginUpdate(AStateBookmark)
  else
    AStateBookmark.DataSetState.Clear;

  try
    AStateBookmark.DataSource := GetGridBookmark;
    BeginUpdate;
  except
    EndUpdate;
    Raise
  end;
end;

procedure TcxCustomGridTableViewHelper.SetGridBookmark(const AValue: TDataSourceBookmark; ARestores: TDataSourceBookmarkRestores);
var
  LFound: Boolean;
  I, J, K: Integer;
  LName: String;
  DDataController: TcxCustomDataController;
  DGrid: TcxCustomGridView;
begin
  if (ARestores = []) or (ViewData.RecordCount = 0) then Exit;

  if (sbrKeyFieldValues in ARestores) and VarIsPresent(AValue.KeyFieldValues) and (DataController is TcxDBDataController) then begin
    // LFound := VarIsEqual(TcxDBDataController(DataController).GetKeyFieldsValues, AValue.KeyFieldValues);
    LFound := VarIsEqual(DataController.GetRecordId(DataController.FocusedRecordIndex), AValue.KeyFieldValues);
    if not LFound then begin
      I := TcxDBDataController(DataController).FindRecordIndexByKey(AValue.KeyFieldValues);
      LFound := (I >= 0);
      if LFound then
        DataController.FocusedRecordIndex := I;
    end;

    if LFound and IsMaster and VarIsArray(AValue.Reserved) then begin
      with Controller do begin
  //        if not (Assigned(FocusedRecord) and FocusedRecord.Expanded) then Exit;
        if not Assigned(FocusedRecord) then Exit;
        if not FocusedRecord.Expanded then FocusedRecord.Expanded := True;
      end;

      DDataController := DataController;
      I := DDataController.FocusedRecordIndex;
      J := DDataController.GetDetailActiveRelationIndex(I);
      if (I < 0) or (J < 0) then Exit;
      DDataController := DDataController.GetDetailDataController(I, J);
      if (DDataController = nil) or (DDataController.RecordCount = 0) then Exit;
      DGrid := nil;
      LName := AValue.Reserved[0];
      with TcxGridLevel(Level) do
        for K := Pred(VisibleCount) downto 0 do
          if VisibleItems[K].GridView.Name = LName then begin
            DGrid := TcxCustomGridTableView(VisibleItems[K].GridView);
            Break;
          end;
      if DGrid = nil then Exit;

      for J := Pred(DGrid.CloneCount) downto 0 do with TcxCustomGridTableView(DGrid.Clones[J]) do
        if MasterRecordIndex = I then begin
          Focused := True;
          K := AValue.Reserved[1];
          if (K >= 0) and (K < VisibleItemCount) then
            VisibleItems[K].Focused := True;

          I := TcxGridDBDataController(DDataController).FindRecordIndexByKey(AValue.Reserved[2]);
          if I >= 0 then
            TcxGridDBDataController(DDataController).FocusedRecordIndex := I;
            // TcxGridDBDataController(DDataController).DataSet.Locate(TcxGridDBDataController(DDataController).KeyFieldNames, AValue.Reserved[2], []);

          Break;
        end;
    end;
  end else
    LFound := False;

  if (not LFound) and (sbrRecordIndex in ARestores) then
    ScrollBarPos := AValue.RecordIndex;

  if Assigned(Controller.FocusedRecord) and not (Controller.FocusedRecord is TcxGridDataRow) then
    Controller.GoToNext(True);

  if (sbrRowIndex in ARestores) then
    VisibleRowIndex := AValue.RowIndex;
end;

procedure TcxCustomGridTableViewHelper.SetGridBookmark(const Value: TDataSourceBookmark);
begin
  SetGridBookmark(Value, DataSourceBookmarkRestores_All);
end;
{$ENDIF}

function TcxCustomGridTableViewHelper.ClientRectToGridRect(R: TRect): TRect;
begin
  Result.Create
  (
    ClientToGrid(R.TopLeft),
    R.Width,
    R.Height
  );
end;

function TcxCustomGridTableViewHelper.ClientToGrid(P: TPoint): TPoint;
var
  LViewOrigin: TPoint;
begin
  LViewOrigin := Controller.Site.ClientOrigin;
  with Control.ClientOrigin do
    Result.Create(P.X + LViewOrigin.X - X, P.Y + LViewOrigin.Y - Y);
end;

{$IFDEF BASEDATAMODULE_SUPPORT}
procedure TcxCustomGridTableViewHelper.EndUpdate(const AStateBookmark : TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores);
var
  LcxDBDataController: TcxDBDataController;
  LDataSet: TDataSet;
//  LSourceDataSet: TDataSet;
begin
  LDataSet := nil;
  TcxCustomGridViewCrack(Self).GridBeginUpdate; //TODO: Ќужно ли?
  try
    try
      if Assigned(DataController) and (DataController is TcxDBDataController) then begin
        LcxDBDataController := TcxDBDataController(DataController);
        if Assigned(LcxDBDataController.DataSource) then
          LDataSet := LcxDBDataController.DataSource.DataSet;
  //      LSourceDataSet := LDataSet.Source;

        if Assigned(LDataSet) then
          LDataSet.Source.EndUpdate(AStateBookmark, ADataSetStateRestore - [sbrDisableCount], []);

        if (sbrKeyFieldValues in ADataSourceRestore)
            and (LcxDBDataController.DataModeController.SyncMode or LcxDBDataController.DataModeController.GridMode)
            and Assigned(LDataSet) and (LDataSet.RecordCount > 0)
            and VarIsPresent(AStateBookmark.DataSource.KeyFieldValues)
            and (not VarIsEqual(LDataSet.FieldValues[LcxDBDataController.KeyFieldNames], AStateBookmark.DataSource.KeyFieldValues))
            and LDataSet.Locate(LcxDBDataController.KeyFieldNames, AStateBookmark.DataSource.KeyFieldValues, [])
        then
          if LcxDBDataController.DataModeController.GridMode and (AStateBookmark.DataSource.RowIndex > 0) then begin
            LDataSet.MoveBy(-Min(AStateBookmark.DataSource.RowIndex, LDataSet.RecNo));
          end;
      end;
    finally
  //    if sbrDisableCount in ADataSetStateRestore then begin
        if Assigned(LDataSet) then
          LDataSet.EnableControls;
  //    end;
      EndUpdate;
    end;

    SetGridBookmark(AStateBookmark.DataSource, ADataSourceRestore);
  finally
    TcxCustomGridViewCrack(Self).GridEndUpdate; //TODO: Ќужно ли?
  end;
end;

procedure TcxCustomGridTableViewHelper.RefreshDataSet;
var
  LBookmark: TDataSourceStateBookmark;
begin
  if Assigned(DataController) and (DataController is TcxDBDataController) and Assigned(TcxDBDataController(DataController).DataSource) then begin
    BeginUpdate(LBookmark);
    try
      with TcxDBDataController(DataController).DataSource.DataSet.Source do begin
        Close;
        Open;
      end;
    finally
      EndUpdate(LBookmark);
    end;
  end;
end;
{$ENDIF}

function TcxCustomGridTableViewHelper.GetVisibleRowIndex: Integer;
begin
  Result := Controller.FocusedRecordIndex - Controller.TopRecordIndex;
end;

function TcxCustomGridTableViewHelper.GetFilteredRecordIndex: Integer;
begin
  Result := Controller.FocusedRecordIndex
end;

procedure TcxCustomGridTableViewHelper.SetFilteredRecordIndex(const AValue: Integer);
begin
  if (AValue >= 0) then
    if AValue < ViewData.RecordCount then
      Controller.FocusedRecordIndex := AValue
    else
      Controller.FocusedRecordIndex := ViewData.RecordCount - 1;
end;

function TcxCustomGridTableViewHelper.GetScrollBarPos: Integer;
begin
  Result := TcxCustomGridTableControllerCrack(Controller).ScrollBarPos
end;

procedure TcxCustomGridTableViewHelper.SetScrollBarPos(const AValue: Integer);
begin
  if AValue < TcxCustomGridTableControllerCrack(Controller).ScrollBarRecordCount then
    TcxCustomGridTableControllerCrack(Controller).ScrollBarPos := AValue
  else
    TcxCustomGridTableControllerCrack(Controller).ScrollBarPos := TcxCustomGridTableControllerCrack(Controller).ScrollBarRecordCount - 1;
end;

procedure TcxCustomGridTableViewHelper.SetVisibleRowIndex(const AValue: Integer);
var
  FTopIndex: Integer;
begin
  if AValue >= 0 then begin
    FTopIndex := Controller.FocusedRecordIndex - AValue;

    if FTopIndex >= 0 then
      Controller.TopRecordIndex := FTopIndex;
  end;
end;

{ TcxCustomImageComboBoxPropertiesHelper }

function TcxCustomImageComboBoxPropertiesHelper.FindImageIndexByValue(const AValue: Variant): TcxImageIndex;
var
  LItem: TcxImageComboBoxItem;
begin
  LItem := FindItemByValue(AValue);
  if Assigned(LItem) then
    Result := LItem.ImageIndex
  else
    Result := DefaultImageIndex
end;

{ TcxDBVerticalGridHelper }

{$IFDEF BASEDATAMODULE_SUPPORT}
procedure TcxDBVerticalGridHelper.BeginUpdate(var AStateBookmark: TDataSourceStateBookmark);
var
  LDataSet: TDataSet;
begin
  if Assigned(DataController) and Assigned(DataController.DataSource) then
    LDataSet := DataController.DataSource.DataSet.Source
  else
    LDataSet := nil;

  if Assigned(LDataSet) then
    LDataSet.BeginUpdate(AStateBookmark)
  else
    AStateBookmark.DataSetState.Clear;

  try
    AStateBookmark.DataSource := GetGridBookmark;
    BeginUpdate;
  except
    EndUpdate;
    Raise
  end;
end;

procedure TcxDBVerticalGridHelper.EndUpdate(const AStateBookmark: TDataSourceStateBookmark; ADataSetStateRestore: TDataSetStateBookmarkRestores; ADataSourceRestore: TDataSourceBookmarkRestores);
var
  LDataSet: TDataSet;
begin
  LDataSet := nil;
  try
    if Assigned(DataController.DataSource) then
      LDataSet := DataController.DataSource.DataSet.Source;

    if Assigned(LDataSet) then
      LDataSet.EndUpdate(AStateBookmark, ADataSetStateRestore - [sbrDisableCount], []);

//    if Assigned(LDataSet) and (not LDataSet.IsEmpty) and VarIsPresent(AStateBookmark.DataSource.KeyFieldValues) then
//      LDataSet.Locate(DataController.KeyField, AStateBookmark.DataSource.KeyFieldValues, []);
  finally
//    if sbrDisableCount in ADataSetStateRestore then begin
      if Assigned(LDataSet) then
        LDataSet.EnableControls;
//    end;
    EndUpdate;
  end;

  SetGridBookmark(AStateBookmark.DataSource, ADataSourceRestore)
end;

function TcxDBVerticalGridHelper.GetFocusedRecordValues(const AItemNames: String; var AUnknownItemNames: String): Variant;
var
  LItem     : TStringItem;
  LTableItem: TObject;
  LValue    : Variant;
begin
  Result := Unassigned;
  AUnknownItemNames := '';

  for LItem in SBaseStringFunctions.EnumStringItems(AItemNames) do begin
    LTableItem := DataController.GetItemByFieldName(LItem.Value);
    if LTableItem <> nil then begin
      if DataController.FocusedRecordIndex >= 0 then
        LValue := TcxDBVerticalGridItemDataBinding(TcxCustomInplaceEditContainerCrack(LTableItem).DataBinding).Field.AsVariant
      else
        LValue := Null;
    end else begin
      AUnknownItemNames := ConcatIfNotEmpty(AUnknownItemNames, ';', LItem.Value);
      LValue := Null;
    end;

    if (LItem.Index = 0) and LItem.Eof then
      Result := LValue
    else begin
      if LItem.Index = 0 then
        Result := VarArrayOf([LValue])
      else begin
        VarArrayRedim(Result, LItem.Index);
        Result[LItem.Index] := LValue;
      end;
    end;
  end;
end;
{$ENDIF}

function TcxDBVerticalGridHelper.GetActive: Boolean;
begin
  with DataController do
    Result := Assigned(DataSet) and DataSet.Active
end;

{$IFDEF BASEDATAMODULE_SUPPORT}
function TcxDBVerticalGridHelper.GetFocusedRecordValues(AItemNames: String): Variant;
var
  LUnknownItemNames: String;
begin
  Result := GetFocusedRecordValues(AItemNames, LUnknownItemNames);
  if LUnknownItemNames <> '' then
    Raise Exception.CreateFmt(MError_ColumnByFieldNotFound, [LUnknownItemNames]);
end;

procedure TcxDBVerticalGridHelper.SetFocusedRecordValues(AItemNames: String; const AValues: Variant);
var
  LItem     : TStringItem;
  LTableItem: TObject;
begin
  DataController.Edit;
  for LItem in SBaseStringFunctions.EnumStringItems(AItemNames) do begin
    LTableItem := DataController.GetItemByFieldName(LItem.Value);
    if LTableItem = nil then
      Raise Exception.CreateFmt(MError_ColumnByFieldNotFound, [LItem.Value]);

    if (LItem.Index = 0) and LItem.Eof then
      TcxDBVerticalGridItemDataBinding(TcxCustomInplaceEditContainerCrack(LTableItem).DataBinding).Field.AsVariant := AValues
    else
      TcxDBVerticalGridItemDataBinding(TcxCustomInplaceEditContainerCrack(LTableItem).DataBinding).Field.AsVariant := AValues[LItem.Index];
  end;
end;

function TcxDBVerticalGridHelper.GetGridBookmark: TDataSourceBookmark;
begin
  Result.RecordIndex  := FocusedRecordIndex;
  Result.RowIndex     := LeftVisibleRecord;

//  if (Result.RecordIndex >= 0) then
//    Result.KeyFieldValues := TcxDBTreeListNode(FocusedNode).KeyValue
//  else
    Result.KeyFieldValues := Unassigned;

  Result.Reserved    := Unassigned;
end;

procedure TcxDBVerticalGridHelper.RefreshDataSet;
var
  LBookmark: TDataSourceStateBookmark;
begin
  if Assigned(DataController.DataSource) and Assigned(DataController.DataSource.DataSet) then begin
    BeginUpdate(LBookmark);
    try
      with DataController.DataSource.DataSet.Source do begin
        Close;
        Open;
      end;
    finally
      EndUpdate(LBookmark);
    end;
  end;
end;

procedure TcxDBVerticalGridHelper.SetGridBookmark(const AValue: TDataSourceBookmark; ARestores: TDataSourceBookmarkRestores);
var
  LFound: Boolean;
begin
  if (ARestores = []) or (RecordCount = 0) then Exit;
{
  if (sbrKeyFieldValues in ARestores) and VarIsPresent(AValue.KeyFieldValues) then begin
    LFound := VarIsEqual(TcxDBTreeListNode(FocusedNode).KeyValue, AValue.KeyFieldValues);
    if not LFound then
      LFound := DataController.LocateByKey(AValue.KeyFieldValues);
  end else
}
    LFound := False;

  if (not LFound) and (sbrRecordIndex in ARestores) then
    FocusedRecordIndex := AValue.RecordIndex;

  if (sbrRowIndex in ARestores) then
    LeftVisibleRecord := AValue.RowIndex;
end;

procedure TcxDBVerticalGridHelper.SetGridBookmark(const Value: TDataSourceBookmark);
begin
  SetGridBookmark(Value, DataSourceBookmarkRestores_All);
end;
{$ENDIF}

{ TcxCustomEditorRowPropertiesHelper }

function TcxCustomEditorRowPropertiesHelper.GetFocusedValue: Variant;
begin
  Result := Value;
end;

procedure TcxCustomEditorRowPropertiesHelper.SetFocusedValue(const AValue: Variant);
begin
  TcxCustomItemDataBindingCrack(DataBinding).DataController.Edit;
  Value := AValue;
end;

{ TcxCustomTreeListHelper }

function TcxCustomTreeListHelper.Version: Word;
begin
  if Self is TcxMDDBTreeList then
    Result := TcxMDDBTreeList(Self).Version
  else if Self is TcxMDTreeList then
    Result := TcxMDTreeList(Self).Version
  else
    Result := 0
end;

initialization
//  cxLookAndFeelPaintersManager.Register(TcxApplicationLookAndFeelPainter.Create);

  RegisterClasses(
    [
      TcxLookAndFeelController, TcxDefaultEditStyleController, TdxSkinController,
      TcxRadioGroup, TcxRadioGroupProperties, TcxRadioGroupItems, TcxDBRadioGroup,
      TcxDBEditDataBinding, TcxDBTextEditDataBinding,
      TcxMDDBEditDataBinding,
      TcxTextEdit, TcxDBTextEdit, TcxTextEditProperties,
      TcxMaskEdit, TcxDBMaskEdit, TcxMaskEditProperties,
      TcxMemo, TcxDBMemo, TcxMemoProperties,
      TcxButtonEdit, TcxDBButtonEdit, TcxButtonEditProperties,
      TcxEditButton, TcxEditButtons,
      TcxRadioButton,
      TcxButton, TcxSplitter,
      TcxPageControl, TcxTabSheet,
      TcxTabControl,  TcxTabControlProperties,
      TcxPopupEdit, TcxDBPopupEdit, TcxPopupEditProperties,
      TcxButtonEdit, TcxDBButtonEdit, TcxButtonEditProperties,
      TcxDateEdit, TcxDBDateEdit, TcxDateEditProperties,
      TcxTimeEdit, TcxDBTimeEdit, TcxTimeEditProperties,
      TcxCalcEdit, TcxDBCalcEdit, TcxCalcEditProperties,
      TcxSpinEdit, TcxDBSpinEdit, TcxSpinEditProperties,
      TcxMaskEdit, TcxMaskEditProperties,
      TcxCheckBox, TcxCheckBoxProperties, TcxDBCheckBox, TcxUserCheckBox, TcxDBUserCheckBox,
      TcxRichEdit, TcxRichEditProperties,
      TcxEditStyle,
      TcxScrollBox,
      TcxGroupBox, TcxLabel, TcxDBLabel, TcxStaticLabel, TcxLabelEditStyle,
      TcxUserCheckGroupProperties,
      TcxComboBox, TcxDBComboBox,
      TcxComboBoxProperties,
      TcxLookupComboBox, TcxDBLookupComboBox, TcxMDDBLookupComboBox,
      TcxLookupComboBoxProperties,
      TcxCheckLookupComboBoxProperties, TcxCheckLookupComboBox, TcxDBCheckLookupComboBox,
      TcxMDLookupTreeComboBoxProperties, TcxMDLookupTreeComboBox, TcxMDDBLookupTreeComboBox, TcxMDMultiFieldsDBLookupTreeComboBox, TcxDBCheckLookupComboBox,
      TcxTreeListColumnOptions,
      TcxMRUEditProperties,
      TcxImageList, TcxImageCollection, TcxImageCollectionItem, TcxNamedImageCollection, TcxNamedImageCollectionItem,
      TcxEditRepository, TcxEditRepositoryCalcItem, TcxEditRepositoryImageItem, TcxEditRepositoryDateItem, TcxEditRepositoryTimeItem,
      TcxHintStyleController,
      //TcxVerticalGrid, TcxDBVerticalGrid,
      TcxDBVerticalGridDataController,
      TcxCategoryRow,
      TcxEditorRow, TcxDBEditorRow,
      TcxMultiEditorRow, TcxDBMultiEditorRow,
      TcxEditorRowItemProperties, TcxDBEditorRowItemProperties,
      TcxMultiEditorRowProperties, TcxDBMultiEditorRowProperties,
      TcxEditorRowPropertiesOptions,
      TcxMultiEditorRowPropertiesOptions,
      TcxEditorRowStyles,
      TcxMDRadioGroup
    ]);

//  cxTextEdit.cxDefault_SINGLELINE                   := True;
  cxFormatController.UseDelphiDateTimeFormats       := True;

finalization
  UnRegisterClasses(
    [
      TcxLookAndFeelController, TcxDefaultEditStyleController, TdxSkinController,
      TcxRadioGroup, TcxRadioGroupProperties, TcxRadioGroupItems, TcxDBRadioGroup,
      TcxDBEditDataBinding, TcxDBTextEditDataBinding,
      TcxMDDBEditDataBinding,
      TcxTextEdit, TcxDBTextEdit, TcxTextEditProperties,
      TcxMaskEdit, TcxDBMaskEdit, TcxMaskEditProperties,
      TcxMemo, TcxDBMemo, TcxMemoProperties,
      TcxButtonEdit, TcxDBButtonEdit, TcxButtonEditProperties,
      TcxEditButton, TcxEditButtons,
      TcxRadioButton,
      TcxButton, TcxSplitter,
      TcxPageControl, TcxTabSheet,
      TcxTabControl, TcxTabControlProperties,
      TcxPopupEdit, TcxDBPopupEdit, TcxPopupEditProperties,
      TcxButtonEdit, TcxDBButtonEdit, TcxButtonEditProperties,
      TcxDateEdit, TcxDBDateEdit, TcxDateEditProperties,
      TcxTimeEdit, TcxDBTimeEdit, TcxTimeEditProperties,
      TcxCalcEdit, TcxDBCalcEdit, TcxCalcEditProperties,
      TcxSpinEdit, TcxDBSpinEdit, TcxSpinEditProperties,
      TcxMaskEdit, TcxMaskEditProperties,
      TcxCheckBox, TcxCheckBoxProperties, TcxDBCheckBox, TcxUserCheckBox, TcxDBUserCheckBox,
      TcxRichEdit, TcxRichEditProperties,
      TcxEditStyle,
      TcxScrollBox,
      TcxGroupBox, TcxLabel, TcxDBLabel, TcxStaticLabel, TcxLabelEditStyle,
      TcxUserCheckGroupProperties,
      TcxComboBox, TcxDBComboBox,
      TcxComboBoxProperties,
      TcxLookupComboBox, TcxDBLookupComboBox, TcxMDDBLookupComboBox,
      TcxLookupComboBoxProperties,
      TcxCheckLookupComboBoxProperties, TcxCheckLookupComboBox, TcxDBCheckLookupComboBox,
      TcxMDLookupTreeComboBoxProperties, TcxMDLookupTreeComboBox, TcxMDDBLookupTreeComboBox, TcxMDMultiFieldsDBLookupTreeComboBox, TcxDBCheckLookupComboBox,
      TcxTreeListColumnOptions,
      TcxMRUEditProperties,
      TcxImageList, TcxImageCollection, TcxImageCollectionItem, TcxNamedImageCollection, TcxNamedImageCollectionItem,
      TcxEditRepository, TcxEditRepositoryCalcItem, TcxEditRepositoryImageItem, TcxEditRepositoryDateItem, TcxEditRepositoryTimeItem,
      TcxHintStyleController,
      //TcxVerticalGrid, TcxDBVerticalGrid,
      TcxDBVerticalGridDataController,
      TcxCategoryRow,
      TcxEditorRow, TcxDBEditorRow,
      TcxMultiEditorRow, TcxDBMultiEditorRow,
      TcxEditorRowItemProperties, TcxDBEditorRowItemProperties,
      TcxMultiEditorRowProperties, TcxDBMultiEditorRowProperties,
      TcxEditorRowPropertiesOptions,
      TcxMultiEditorRowPropertiesOptions,
      TcxEditorRowStyles,
      TcxMDRadioGroup
    ]);

end.

