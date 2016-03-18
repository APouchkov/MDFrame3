unit SBaseReportFilter;
{$I config.inc}

interface

uses
  System.SysUtils, System.Types, System.Variants, System.Classes, Dialogs, Controls,
  ExtCtrls, Data.DB, dxBar, cxGridLevel, cxGridCustomTableView,
  cxGridTableView, cxGroupBox, cxButtons, cxGridDBTableView, cxGrid, StrUtils,
  NamedVariables
{$IFNDEF PACKAGE}
  , SBaseFormDefinitions, SBaseForm, SBaseItemReactions, SBaseDataModule, SBaseDataHelperInternal
{$ENDIF};

type
  TFilterType             = (ftInclude = 0, ftExcept = 1);
  TFilterCollection       = class;
  TFilterCollectionItem   = class;
  TDataSetUpdateProc      = reference to procedure;

  // Базовый класс свойств элемента фильтра
  TSBaseCustomFilterItemProperties = class(TPersistent)
  strict private
    FCaption:           string;
    FList:              string;
    FExtractSQL:        string;
    FAction:            TFilterType;

    procedure SetCaption(const AValue: string);
    procedure SetAction(const AValue: TFilterType);
    procedure SetList(const AValue: string);
    procedure SetExtractSQL(const AValue: String);
  private
    FOnChange: TNotifyEvent;

    function GetDisplayName: string;
  protected
    procedure AssignTo(ADest: TPersistent); override;

{$IFNDEF PACKAGE}
    function GetNewItemReaction:  TBaseItemReactionOnClick; virtual; abstract;
    function GetViewItemReaction: TBaseItemReactionOnClick; virtual; abstract;
{$ENDIF}
  published
    property Caption:   string read FCaption write SetCaption;
    property List:      string read FList write SetList;
    property Action:    TFilterType read FAction write SetAction;

    property ExtractSQL: String read FExtractSQL write SetExtractSQL;

{$IFNDEF PACKAGE}
    property NewItemReaction: TBaseItemReactionOnClick   read GetNewItemReaction;
    property ViewItemReaction: TBaseItemReactionOnClick  read GetViewItemReaction;
{$ENDIF}
  end;

  // Класс свойств, создается в фильтре
  TSBaseFilterItemProperties = class(TSBaseCustomFilterItemProperties)
  private
{$IFNDEF PACKAGE}
    FViewItemReaction:    TBaseItemReaction;
    FNewItemReaction:     TBaseItemReaction;

    procedure CreateItemReactions(AForm: TSBaseFrm; ACallBackProc: TCallBackSelectFunction);
{$ENDIF}
  protected
    procedure AssignTo(ADest: TPersistent); override;

{$IFNDEF PACKAGE}
    function GetNewItemReaction:  TBaseItemReactionOnClick; override;
    function GetViewItemReaction: TBaseItemReactionOnClick; override;
{$ENDIF}
  end;

  // Класс свойств, создается в репозитарии
  TSBaseRepositoryFilterItemProperties = class(TSBaseCustomFilterItemProperties)
  strict private
{$IFNDEF PACKAGE}
    FViewItemReaction:  TBaseItemReactionOnClick;
    FNewItemReaction:   TBaseItemReactionOnClick;
{$ENDIF}
  protected
    procedure AssignTo(ADest: TPersistent); override;

{$IFNDEF PACKAGE}
    function GetNewItemReaction:  TBaseItemReactionOnClick; override;
    function GetViewItemReaction: TBaseItemReactionOnClick; override;
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
  end;

  // Хранит общие свойства
  TSBaseReportFilterOptions = class(TPersistent)
  strict private
//    FExportPatternId  : Integer;
    FImportTemplateId : Integer;
    FExportObjectId   : Integer;
  protected
    procedure AssignTo(ADest: TPersistent); override;
  published
    // Отчет для формирования результирующего Xml
    property ExportObjectId: Integer read FExportObjectId write FExportObjectId default 0;

    // Шаблон отчета для формирования результирующего Xml
//    property ExportPatternId: Integer read FExportPatternId write FExportPatternId default 0;

    // Шаблон для чтения (эскпорта) Xml
    property ImportTemplateId: Integer read FImportTemplateId write FImportTemplateId default 0;
  end;

  // Панель фильтра
  TSBaseReportFilter  = class(TcxCustomGroupBox)
  strict private
    cxGridDBTableView:  TcxGridDBTableView;
    cxGridLevel:        TcxGridLevel;
    cxGrid:             TcxGrid;

    ColumnName:         TcxGridDBColumn;
    ColumnItemsName:    TcxGridDBColumn;
    CustomPanel:        TPanel;

{$IFNDEF PACKAGE}
    mnFilterInclude:    TdxBarButton;
    mnFilterExcept:     TdxBarButton;
{$ENDIF}

    procedure ColumnNameGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: string);
{$IFNDEF PACKAGE}
    procedure mnDeleteClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
    procedure mnOpenFileClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
    procedure mnNewClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
    procedure mnClearGroupClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
    procedure mnDeleteFilterClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
    procedure mnFilterExceptClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
    procedure mnFilterIncludeClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
    procedure mnSaveToFileClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
{$ENDIF}
  strict private
    FFilterItems:       TFilterCollection;
    FOptions:           TSBaseReportFilterOptions;

    {$IFNDEF PACKAGE}
    FFilterDataSource:  TDataSource;
    {$ENDIF}

    FFilterDataSet:     {$IFNDEF PACKAGE}TUserMemoryDataSet{$ELSE}TDataSet{$ENDIF};

    function GetCount: Integer;

    procedure CMControlChange(var AMessage: TCMControlChange); message CM_CONTROLCHANGE;
    procedure CreateGUIObjects;

    procedure SetFilterItems(const AValue: TFilterCollection);
    procedure SetOptions(const AValue: TSBaseReportFilterOptions);

{$IFNDEF PACKAGE}
    function TestFilterTypeMenu(ASender: TObject): Boolean;
    function GetItemByList(const AList: String): TFilterCollectionItem;

    procedure DoBeforePost(DataSet: TDataSet);
    procedure SetFilterByList(AList: string);

    procedure ChangeFilterAction(const AFilterAction: TFilterType);
    procedure ClearFilter;
    procedure DeleteFilterItems(AClearAll: Boolean);

    procedure DoViewClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
{$ENDIF}
  private
{$IFNDEF PACKAGE}
    mnNewFilter:  TdxBarSubItem;
//    ItemsPopup: TdxBarPopupMenu;

    procedure DoAddFilter(ASender: TObject);
    procedure DoSelectCallBack(AItem: TFilterCollectionItem; const AResultValues: TNamedVariants);
{$ENDIF}
  protected
    function GetChildParent: TComponent; override;
    procedure ValidateInsert(AComponent: TComponent); override;
  public
    function GetXml(const AFileName: string): string;
    function GetResultXml: string;
    procedure GetChildren(AProc: TGetChildProc; ARoot: TComponent); override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Count: Integer read GetCount;

  published
    property ParamValue: string read GetResultXml;
    property FilterItems: TFilterCollection read FFilterItems write SetFilterItems;
    property Options: TSBaseReportFilterOptions read FOptions write SetOptions;
{$IFNDEF PACKAGE}
    property FilterDataSet: TUserMemoryDataSet read FFilterDataSet;
{$ENDIF}

    property Align;
    property Anchors;
    property AutoSize;
    property Caption;
    property Constraints;
    property Font;
    property LookAndFeel; // deprecated
    property PanelStyle;
    property Style;
    property StyleDisabled;
    property StyleFocused;
    property StyleHot;
    property Margins;
    property Padding;
    property ParentFont;
    property Transparent;
    property Visible;
  end;

  // Коллекция элементов фильтра
  TFilterCollectionItem = class(TCollectionItem)
  strict private
    FProperties:        TSBaseFilterItemProperties;
    FMenuItem:          TdxBarButton;
    FEnableMenu:        Boolean;

{$IFNDEF PACKAGE}
    procedure DoSelectCallBack(ASelectAction: TCallBackSelectAction; const AResultValues: TNamedVariants);
    procedure SetEnable(const AValue: Boolean);

    function GetFilter: TSBaseReportFilter;
{$ENDIF}

    function GetProperties: TSBaseCustomFilterItemProperties;

    procedure CreateGUIObjects;
    procedure SetProperties(const AValue: TSBaseCustomFilterItemProperties);
    procedure DoPropertiesChange(ASender: TObject);
  private
{$IFNDEF PACKAGE}
    function ExtractSqlName: string;
    procedure FillDataSet(ADataSet: TDataSet; AItemName: String = ''; AItemId: Integer = -1);

    property EnableMenu: Boolean read FEnableMenu write SetEnable;
{$ENDIF}
  public
    function GetDisplayName: string; override;

    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property Properties: TSBaseCustomFilterItemProperties read GetProperties write SetProperties;
  end;

  // Элемент коллекции
  TFilterCollection = class(TCollection)
  strict private
    FFilter: TSBaseReportFilter;

    function GetFilterItem(AIndex: Integer): TFilterCollectionItem;
  public
    constructor Create(AFilter: TSBaseReportFilter); reintroduce;

    property Filter: TSBaseReportFilter read FFilter;
    property FilterItems[AIndex: Integer]: TFilterCollectionItem read GetFilterItem; default;
  end;

  // Элемент репозитория, хранит настройки элемента фильтра
  TSBaseFilterRepositoryItem = class(TComponent)
  strict private
    FProperties: TSBaseCustomFilterItemProperties;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Properties: TSBaseCustomFilterItemProperties read FProperties write FProperties;
  end;

  // Репозиторий
  TSBaseReportFilterRepository = class(TComponent)
  strict private
    FOptions: TSBaseReportFilterOptions;

  protected
    function GetChildOwner: TComponent; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Options: TSBaseReportFilterOptions read FOptions;
  end;

  procedure Register;

implementation

uses
  Graphics, Winapi.Windows, System.Generics.Collections,
{$IFNDEF PACKAGE}
  SBaseReport, SBaseReportTXT, SBaseImport,
{$ENDIF}
  SBaseVariantFunctions, SBaseStringFunctions, SBaseConstants, cxCustomData,
  SBaseUtils;

resourcestring
  SMessage_ItemExists         = 'Элемент "%s" уже есть в списке';
  SMessage_ItemNotFound       = 'Фильтр "%s" не найден';

  SConst_FLTSaveDialogFilter  = 'Фильтры (*.flt)|*.flt|Все файлы (*.*)|*.*';
  SConst_FltFileExtension     = 'flt';

  SConst_ListCount            = 'ListCount';

const
  IConst_BarManagerCategory   = 1;
  IConst_ButtonHeight         = 21;

var
  SBaseReportFilterRepository: TSBaseReportFilterRepository;

procedure Register;
begin
  RegisterComponents('MD Project Tools', [TSBaseReportFilter]);
end;

function FilterTypeToStr(const AFilterType: TFilterType): string;
begin
  case AFilterType of
    ftInclude:  Result := '+';
    ftExcept:   Result := '-';
    else        raise EAbstractError.Create('неизвестный тип');
  end;
end;

{$REGION 'TSBaseReportFilter'}

procedure TSBaseReportFilter.ColumnNameGetDisplayText(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AText: string);
begin
  if (ARecord is TcxGridGroupRow) and VarIsStr(ARecord.Values[-1]) then
    AText := ARecord.Values[-1];
end;

function TSBaseReportFilter.GetChildParent: TComponent;
begin
  Result := CustomPanel;
end;

procedure TSBaseReportFilter.GetChildren(AProc: TGetChildProc; ARoot: TComponent);
var
  i: Integer;
  LControl: TControl;
begin
  for i := 0 to CustomPanel.ControlCount - 1 do begin
    LControl := CustomPanel.Controls[i];
    if (LControl.Owner <> Self)
      and (LControl.Name <> '')
    then
      AProc(LControl);
  end;
end;

function TSBaseReportFilter.GetCount: Integer;
begin
  Result := FFilterItems.Count;
end;

constructor TSBaseReportFilter.Create(AOwner: TComponent);

  procedure _AddField(const AName: String; AType: TFieldType; ASize: Integer = 0);
  begin
    FFilterDataSet.FieldDefs.Add(AName, AType, ASize);
  end;

begin
  inherited;

  FOptions        := TSBaseReportFilterOptions.Create;
  Caption         := '';
  FFilterItems    := TFilterCollection.Create(Self);

{$IFNDEF PACKAGE}
  // получаем настройки из репозитория, потом их в dfm можно переопределить
  if Assigned(SBaseReportFilterRepository) then
    Options.Assign(SBaseReportFilterRepository.Options);

  Assert(AOwner is TSBaseFrm);

  FFilterDataSet := TUserMemoryDataSet.Create(Self);
  FFilterDataSet.BeforePost     := DoBeforePost;
  FFilterDataSource             := TDataSource.Create(Self);
  FFilterDataSource.DataSet     := FFilterDataSet;

  _AddField(SConst_Id,               ftInteger);
  _AddField(SConst_MasterField_Id,   ftInteger);
  _AddField(SConst_DisplayText,      ftString, 256);
  _AddField(SConst_List,             ftString, 256);
  _AddField(SConst_Action,           ftString, 10);
  _AddField(SConst_ItemName,         ftString, 256);
{$ENDIF}

  CreateGUIObjects;

{$IFNDEF PACKAGE}
  FFilterDataSet.Open;
{$ENDIF}
end;

destructor TSBaseReportFilter.Destroy;
begin
  FreeAndNil(FFilterItems);
  FreeAndNil(FOptions);

  inherited;
end;

procedure TSBaseReportFilter.CreateGUIObjects;
{$IFNDEF PACKAGE}
type
  // тип реакции
  _TMenuItemReactionType =
  (
    irtNone,      // недоступен - для нереализованных функций
    irtAll,       // всегда работает (если открыт датасет)
    irtFilter,    // работает на элементе фильтра (запись, пустая запись, заголовок фильтра)
    irtRecord     // работает только на непустом элементе фильтра
  );

  function _CreateItemReaction(AControl: TComponent; AItemReactionType: _TMenuItemReactionType; AOnClick: TBaseItemReactionOnClickProcedure): TBaseItemReaction;
  var
    LForm: TSBaseFrm;
  begin
    LForm                           := Owner as TSBaseFrm;
    Result                          := LForm.ItemsReactions.Add;
    Result.Item                     := AControl;
    Result.OnClick.SystemProcedure  := AOnClick;

    case AItemReactionType of
      irtAll:
      begin
        Result.OnClick.Reaction := biraCustomDataSourceActive;
        Result.LinkedComponent := FFilterDataSource;
      end;

      irtFilter:
      begin
        Result.LinkedComponent := FFilterDataSource;
        Result.OnClick.Reaction := biraCustomDataSourceRecord;
      end;

      irtRecord:
      begin
        Result.LinkedComponent := cxGridDBTableView;
        Result.OnClick.Reaction := biraCustomDataSourceRecord;
        Result.OnGetEnabled.Condition := SConst_Id + ' <> NULL';
      end;

      else begin
        Result.Enabled    := False;
      end;
    end;
  end;

  function _CreateMenuButton(AShortCut: TShortCut; AItemReactionType: _TMenuItemReactionType; AOnClick: TBaseItemReactionOnClickProcedure; AOnGetEnabled: TBooleanObjectRequest = nil): TdxBarButton;
  begin
    Result            := TdxBarButton.Create((Owner as TSBaseFrm).BarManager);
    Result.Visible    := ivAlways;
    Result.Category   := IConst_BarManagerCategory;
    Result.ShortCut   := AShortCut;
    Result.Enabled    := AItemReactionType = irtAll;

    _CreateItemReaction(Result, AItemReactionType, AOnClick).OnGetEnabled.SystemFunction := AOnGetEnabled;
  end;
{$ENDIF}

  procedure _AddMenuToPopup(APopupMenu: TdxBarPopupMenu; AItem: TdxBarItem; ABeginGroup: Boolean = False);
  begin
    if Assigned(AItem) then
      with APopupMenu.ItemLinks.Add do begin
        Item       := AItem;
        Visible    := True;
        BeginGroup := ABeginGroup;
      end;
  end;

var
{$IFNDEF PACKAGE}
  LForm: TSBaseFrm;

  mnOpenFile:         TdxBarButton;
  mnCopyAll:          TdxBarButton;
  mnSaveToFile:       TdxBarButton;

  mnNew:              TdxBarButton;
  mnClearGroup:       TdxBarButton;
  mnDeleteFilter:     TdxBarButton;
  mnDelete:           TdxBarButton;
  mnView:             TdxBarButton;
  mnCopy:             TdxBarButton;
  mnCut:              TdxBarButton;
  mnPaste:            TdxBarButton;
  mnFilterType:       TdxBarSubItem;

  FilterPopupMenu:    TdxBarPopupMenu;
{$ENDIF}
  ButtonDelete:       TcxButton;
  ButtonAdd:          TcxButton;
begin
  {$REGION 'Конструирование панелей и кнопок'}
  CustomPanel                     := TPanel.Create(Self);
  CustomPanel.Parent              := Self;
  CustomPanel.Align               := alLeft;
  CustomPanel.Width               := 120;
  CustomPanel.Caption             := '';
  CustomPanel.BevelOuter          := bvNone;
  CustomPanel.AlignWithMargins    := True;

  with CustomPanel.Margins do begin
    Top         := 0;
    Left        := 0;
    Right       := 5;
    Bottom      := 0;
  end;

  with CustomPanel.Padding do begin
    Top         := 0;
    Left        := 0;
    Right       := 0;
    Bottom      := 0;
  end;

  ButtonAdd                   := TcxButton.Create(Self);
  ButtonAdd.Height            := IConst_ButtonHeight;
  ButtonAdd.Parent            := CustomPanel;
  ButtonAdd.Align             := alBottom;
  ButtonAdd.Enabled           := False;

  ButtonDelete                := TcxButton.Create(Self);
  ButtonDelete.Height         := IConst_ButtonHeight;
  ButtonDelete.Parent         := CustomPanel;
  ButtonDelete.Align          := alBottom;
  ButtonDelete.Enabled        := False;

  {$ENDREGION 'Конструирование панелей и кнопок'}

  {$REGION 'Конструирование грида'}

  cxGrid            := TcxGrid.Create(Self);
  cxGrid.Parent     := Self;
  cxGrid.Align      := alClient;

  cxGridLevel       := cxGrid.Levels.Add;

  cxGridDBTableView := cxGrid.CreateView(TcxGridDBTableView) as TcxGridDBTableView;

  cxGridDBTableView.DataController.Options        := [dcoAssignGroupingValues, dcoAssignMasterDetailKeys, dcoSaveExpanding, dcoGroupsAlwaysExpanded];
  cxGridDBTableView.OptionsView.ColumnAutoWidth   := True;
  cxGridDBTableView.OptionsView.GridLineColor     := clSilver;
  cxGridDBTableView.OptionsView.GroupByBox        := False;
  cxGridDBTableView.OptionsView.Header            := False;
  cxGridDBTableView.OptionsSelection.InvertSelect := False;

  // Отключаем поведение по умолчанию
  with cxGridDBTableView.OptionsData do begin
    Appending   := False;
    Deleting    := False;
    Editing     := False;
    Inserting   := False;
  end;

  cxGridLevel.GridView                    := cxGridDBTableView;

  ColumnName                              := cxGridDBTableView.CreateColumn;
  ColumnName.Visible                      := False;
  ColumnName.OnGetDisplayText             := ColumnNameGetDisplayText;
  ColumnName.GroupIndex                   := 0;
  ColumnName.Options.Sorting              := False;
  ColumnName.VisibleForCustomization      := False;

  ColumnItemsName                         := cxGridDBTableView.CreateColumn;
  ColumnItemsName.MinWidth                := 64;
  ColumnItemsName.Options.Editing         := False;
  ColumnItemsName.Options.GroupFooters    := False;
  ColumnItemsName.Options.Grouping        := False;
  ColumnItemsName.Options.Moving          := False;
  ColumnItemsName.Options.ShowCaption     := False;
  ColumnItemsName.Options.Sorting         := False;
  ColumnItemsName.VisibleForCustomization := False;

  ColumnName.DataBinding.FieldName        := SConst_DisplayText;
  ColumnItemsName.DataBinding.FieldName   := SConst_ItemName;

  {$IFNDEF PACKAGE}
  cxGridDBTableView.Images                        := SBaseFormProperties.Icon16List;
  cxGridDBTableView.DataController.DataSource := FFilterDataSource;
  {$ENDIF}

  {$ENDREGION 'Конструирование грида'}

  {$REGION 'Конструирование контекстного меню'}
  {$IFNDEF PACKAGE}
  LForm           := Owner as TSBaseFrm;

//  ItemsPopup      := TdxBarPopupMenu.Create(Owner);
//  ItemsPopup.BarManager := LForm.BarManager;

  FilterPopupMenu := TdxBarPopupMenu.Create(Owner);
  FilterPopupMenu.BarManager := LForm.BarManager;

  cxGridDBTableView.PopupMenu := FilterPopupMenu;

  mnView    := _CreateMenuButton(ShortCut_F3, irtRecord, DoViewClick);
  mnDelete  := _CreateMenuButton(ShortCut_Del, irtRecord, mnDeleteClick);

  mnOpenFile    := _CreateMenuButton(0, irtAll, mnOpenFileClick);
  mnSaveToFile  := _CreateMenuButton(0, irtAll, mnSaveToFileClick);

  mnNewFilter           := (Owner as TSBaseFrm).BarManager.AddSubItem;
  mnNewFilter.Category  := IConst_BarManagerCategory;

  mnNew           := _CreateMenuButton(ShortCut_Ins,        irtFilter, mnNewClick);
  mnClearGroup    := _CreateMenuButton(ShortCut_Shift_Del,  irtFilter, mnClearGroupClick);
  mnDeleteFilter  := _CreateMenuButton(ShortCut_Ctrl_Del,   irtFilter, mnDeleteFilterClick);

  mnFilterInclude := _CreateMenuButton(0, irtFilter, mnFilterIncludeClick, TestFilterTypeMenu);
  mnFilterInclude.ButtonStyle := bsChecked;
  mnFilterInclude.GroupIndex  := 1;

  mnFilterExcept  := _CreateMenuButton(0, irtFilter, mnFilterExceptClick, TestFilterTypeMenu);
  mnFilterExcept.ButtonStyle  := bsChecked;
  mnFilterExcept.GroupIndex   := 1;

  mnFilterType := TdxBarSubItem.Create(LForm.BarManager);
  mnFilterType.Visible  := ivAlways;
  mnFilterType.Enabled  := False;
  _CreateItemReaction(mnFilterType, irtFilter, nil).OnClick.Reaction := biraDataSourceSubMenu;

  mnCopy          := _CreateMenuButton(ShortCut_Ctrl_C, irtNone, nil);
  mnPaste         := _CreateMenuButton(ShortCut_Ctrl_V, irtNone, nil);
  mnCut           := _CreateMenuButton(ShortCut_Ctrl_X, irtNone, nil);
  mnCopyAll       := _CreateMenuButton(ShortCut_Ctrl_A, irtNone, nil);

  with mnFilterType.ItemLinks.Add do begin
    Visible := True;
    Item    := mnFilterInclude
  end;

  with mnFilterType.ItemLinks.Add do begin
    Visible := True;
    Item    := mnFilterExcept
  end;

  _AddMenuToPopup(FilterPopupMenu, mnNew);
  _AddMenuToPopup(FilterPopupMenu, mnView);
  _AddMenuToPopup(FilterPopupMenu, mnDelete);

  _AddMenuToPopup(FilterPopupMenu, mnNewFilter, True);
  _AddMenuToPopup(FilterPopupMenu, mnFilterType);
  _AddMenuToPopup(FilterPopupMenu, mnClearGroup);
  _AddMenuToPopup(FilterPopupMenu, mnDeleteFilter);

  _AddMenuToPopup(FilterPopupMenu, mnCopy, True);
  _AddMenuToPopup(FilterPopupMenu, mnPaste);
  _AddMenuToPopup(FilterPopupMenu, mnCut);
  _AddMenuToPopup(FilterPopupMenu, mnCopyAll);

  _AddMenuToPopup(FilterPopupMenu, mnOpenFile, True);
  _AddMenuToPopup(FilterPopupMenu, mnSaveToFile);

  _CreateItemReaction(ButtonAdd,    irtFilter, mnNewClick);
  _CreateItemReaction(ButtonDelete, irtRecord, mnDeleteClick);
  {$ENDIF}
  {$ENDREGION 'Конструирование контекстного меню'}

{$REGION 'Присвоение системных заголовков и иконок элементам меню'}
{$IFDEF PACKAGE}
  ButtonAdd.Caption           := 'Добавить в список';
  ButtonDelete.Caption        := 'Удалить из списка';
{$ELSE}
  /// названия элементов меню
  mnOpenFile.Caption        := SBaseProperties.Text_MenuLoadFromFile;
  mnSaveToFile.Caption      := SBaseProperties.Text_MenuSaveToFile;
  mnFilterInclude.Caption   := SBaseProperties.Text_ActionIncluding;
  mnFilterExcept.Caption    := SBaseProperties.Text_ActionExcluding;
  mnClearGroup.Caption      := SBaseProperties.Text_MenuFilterClear;
  mnDeleteFilter.Caption    := SBaseProperties.Text_MenuFilterDelete;
  mnNewFilter.Caption       := SBaseProperties.Text_MenuFilterNew;
  mnFilterType.Caption      := SBaseProperties.Text_MenuFilterAction;

  mnNew.Caption             := SBaseProperties.Text_MenuNew;
  mnView.Caption            := SBaseProperties.Text_MenuView;
  mnDelete.Caption          := SBaseProperties.Text_MenuDelete;

  mnCopy.Caption            := SBaseProperties.Text_MenuCopy;
  mnPaste.Caption           := SBaseProperties.Text_MenuPaste;
  mnCut.Caption             := SBaseProperties.Text_MenuCut;
  mnCopyAll.Caption         := SBaseProperties.Text_MenuCopyAll;

  ButtonAdd.Caption         := SBaseProperties.Text_MenuAddToList;
  ButtonDelete.Caption      := SBaseProperties.Text_MenuDeleteFromList;

  /// иконки элементов меню
  mnNewFilter.ImageIndex    := SBaseFormProperties.IconIndex_New;
  mnNew.ImageIndex          := SBaseFormProperties.IconIndex_New;
  mnView.ImageIndex         := SBaseFormProperties.IconIndex_View;
  mnDelete.ImageIndex       := SBaseFormProperties.IconIndex_Delete;

  mnCopy.ImageIndex         := SBaseFormProperties.IconIndex_Copy;
  mnPaste.ImageIndex        := SBaseFormProperties.IconIndex_Paste;
  mnCut.ImageIndex          := SBaseFormProperties.IconIndex_Cut;

  mnSaveToFile.ImageIndex   := SBaseFormProperties.IconIndex_FileSave;
  mnOpenFile.ImageIndex     := SBaseFormProperties.IconIndex_FileOpen;

  mnDeleteFilter.ImageIndex := SBaseFormProperties.IconIndex_Delete;
{$ENDIF}
{$ENDREGION}
end;

function TSBaseReportFilter.GetResultXml: string;
begin
  Result := GetXml('');
end;

procedure TSBaseReportFilter.SetFilterItems(const AValue: TFilterCollection);
begin
  FFilterItems.Assign(AValue);
end;

procedure TSBaseReportFilter.SetOptions(const AValue: TSBaseReportFilterOptions);
begin
  FOptions.Assign(AValue);
end;

procedure TSBaseReportFilter.ValidateInsert(AComponent: TComponent);
begin
  inherited;

  if (AComponent <> Self)
    and (AComponent.Owner <> Self)
    and (AComponent is TControl)
    and Assigned(AComponent.Owner)
  then
    (AComponent as TControl).Parent := CustomPanel;
end;

function TSBaseReportFilter.GetXml(const AFileName: string): string;
{$IFNDEF PACKAGE}
var
  LStream:            TStringStream;
  LMasterDataSet:     TUserMemoryDataSet;

  LItem:              TFilterCollectionItem;
  LStateBookMark:     TDataSourceStateBookmark;
  i:                  Integer;
  LFilterEnable:      Boolean;

  LParams:            TNamedVariants;
begin
{$IFDEF DEBUG}
  Assert(Options.ExportObjectId > 0);
{$ENDIF}
  Result := '';

  if FFilterDataSet.IsEmpty then
    Exit;

  LMasterDataSet := TUserMemoryDataSet.Create(Self);
  with LMasterDataSet do begin
    FieldDefs.Add(SConst_Id,     ftInteger);
    FieldDefs.Add(SConst_List,   ftString, 256);
    FieldDefs.Add(SConst_Action, ftString, 256);
    Open;
  end;

  FFilterDataSet.BeginUpdate(LStateBookMark);
  try
    for I := 0 to Count - 1 do begin
      LItem := FFilterItems[I];
      LFilterEnable := (not LItem.EnableMenu)
        and (not VarIsNull(FFilterDataSet.Lookup(SConst_List, LItem.Properties.List, SConst_Id)));

      if LFilterEnable then begin
        LMasterDataSet.Append;
        LMasterDataSet.FieldByName(SConst_Id).AsInteger     := LItem.Index;
        LMasterDataSet.FieldByName(SConst_List).AsString    := LItem.Properties.List;
        LMasterDataSet.FieldByName(SConst_Action).AsString  := FilterTypeToStr(LItem.Properties.Action);
        LMasterDataSet.Post;
      end;
    end;

    if AFileName.IsEmpty then
      LStream := TStringStream.Create;

    try
      LParams := TNamedVariants.Create
      (
        [
          TNamedVariant.Create(SRepParam_ObjectId,       Options.ExportObjectId),

          TNamedVariant.Create(SRepParam_DataSets,       VarArrayOf([Integer(LMasterDataSet), Integer(FFilterDataSet)])),
          TNamedVariant.Create(SRepParam_DataSetCount,   2)
        ],
        True
      );

      LParams[SConst_Mode] := IfThen(AFileName.IsEmpty, SConst_Server, SConst_File);

//      if Options.ExportPatternId > 0 then
//        LParams[SRepParam_PatternId] := Options.ExportPatternId;

      if Assigned(LStream) then begin
        LParams[SRepParam_SaveStream]   := Integer(LStream);
        LParams[SRepParam_Action]       := SRepAction_Stream;
      end
      else begin
        LParams[SRepParam_SaveFileName] := AFileName;
        LParams[SRepParam_Action]       := SRepAction_Save;
      end;

      TSBaseReport.CreateByParams(Self, LParams).Free;

      if Assigned(LStream) then
        Result := LStream.DataString;
    finally
      FreeAndNil(LStream);
    end;
  finally
    FFilterDataSet.EndUpdate(LStateBookMark);
    LMasterDataSet.Free
  end;
end;
{$ELSE}
begin
  Result := '';
end;

{$ENDIF}

procedure TSBaseReportFilter.CMControlChange(var AMessage: TCMControlChange);
begin
  if AMessage.Inserting
    and (AMessage.Control.Owner <> Self)
  then begin
    AMessage.Control.Parent := CustomPanel;
  end;
end;

{$IFNDEF PACKAGE}

function TSBaseReportFilter.TestFilterTypeMenu(ASender: TObject): Boolean;
var
  LFilterType: TFilterType;
begin
  Result := FFilterDataSet.Active and (not FFilterDataSet.Eof);

  if Result then begin
    LFilterType := TFilterType(FFilterDataSet.FieldByName(SConst_Action).AsInteger);

    mnFilterInclude.Down  := LFilterType = ftInclude;
    mnFilterExcept.Down   := LFilterType = ftExcept;
  end;
end;

function TSBaseReportFilter.GetItemByList(const AList: String): TFilterCollectionItem;
var
  LItem: TCollectionItem;
begin
  for LItem in FilterItems do
    if (LItem as TFilterCollectionItem).Properties.List = AList then
      Exit(LItem as TFilterCollectionItem);

  raise EAbstractError.CreateFmt(SMessage_ItemNotFound, [AList]);
end;

procedure TSBaseReportFilter.SetFilterByList(AList: string);
begin
  FFilterDataSet.Filter   := SConst_List + '=''' + AList + '''';
  FFilterDataSet.Filtered := True;
end;

procedure TSBaseReportFilter.DoAddFilter(ASender: TObject);
var
  LStateBookmark: TDataSourceStateBookmark;
begin
  cxGridDBTableView.BeginUpdate;
  try
    FFilterDataSet.BeginUpdate(LStateBookmark);
    try
      with (ASender as TdxBarButton).Data as TFilterCollectionItem do begin
        FillDataSet(FFilterDataSet);
        EnableMenu := False;
      end;
    finally
      FFilterDataSet.EndUpdate(LStateBookmark);
    end;
  finally
    cxGridDBTableView.EndUpdate;
  end;
end;

procedure TSBaseReportFilter.DoSelectCallBack(AItem: TFilterCollectionItem; const AResultValues: TNamedVariants);
var
  LId, LParamId: Integer;
  LDisplayText: string;
  LStateBookmark: TDataSourceStateBookmark;
begin
  Assert(VarIsOrdinal(AResultValues[SConst_Id]));
  Assert(VarIsStr(AResultValues[SConst_DisplayText]));

  LId           := AResultValues[SConst_Id];
  LDisplayText  := '';

  if AResultValues.Find(SConst_DisplayText, LParamId) then
    LDisplayText := AResultValues.Items[LParamId].Value;

  cxGridDBTableView.BeginUpdate;
  FFilterDataSet.BeginUpdate(LStateBookmark);
  try
    SetFilterByList(AItem.Properties.List);

    if FFilterDataSet.Locate(SConst_Id, LId, []) then
      raise EAbstractError.CreateFmt(SMessage_ItemExists, [LDisplayText]);

    FFilterDataSet.First;
    if (not FFilterDataSet.Eof) and FFilterDataSet.FieldByName(SConst_Id).IsNull then
      FFilterDataSet.Delete;

    AItem.FillDataSet(FFilterDataSet, LDisplayText, LId);
  finally
    FFilterDataSet.EndUpdate(LStateBookmark);
    cxGridDBTableView.EndUpdate;
  end;
end;

procedure TSBaseReportFilter.ChangeFilterAction(const AFilterAction: TFilterType);
var
  LItem: TFilterCollectionItem;
  LStateBookmark: TDataSourceStateBookmark;
begin
  LItem := GetItemByList(FFilterDataSet.FieldByName(SConst_List).AsString);
  LItem.Properties.Action  := AFilterAction;

  FFilterDataSet.BeginUpdate(LStateBookmark);
  try
    SetFilterByList(LItem.Properties.List);
    FFilterDataSet.First;
    while not FFilterDataSet.Eof do begin
      FFilterDataSet.Edit;
      FFilterDataSet.FieldByName(SConst_Action).AsInteger      := Integer(AFilterAction);
      FFilterDataSet.FieldByName(SConst_DisplayText).AsString  := LItem.GetDisplayName;
      FFilterDataSet.Post;
      FFilterDataSet.Next;
    end;
  finally
    FFilterDataSet.EndUpdate(LStateBookmark);
  end;
end;

procedure TSBaseReportFilter.mnDeleteClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
var
  LList: String;
  LItem: TFilterCollectionItem;
  LStateBookmark: TDataSourceStateBookmark;
begin
  LList := FFilterDataSet.FieldByName(SConst_List).AsString;
  LItem := GetItemByList(LList);

  cxGridDBTableView.BeginUpdate;
  FFilterDataSet.BeginUpdate(LStateBookmark);
  try
    FFilterDataSet.Delete;

    // проверяем, есть ли ещё элементы фильтра
    SetFilterByList(LItem.Properties.List);
    FFilterDataSet.First;

    // если нет - добавляем пустой
    if FFilterDataSet.Eof then
      LItem.FillDataSet(FFilterDataSet);
  finally
    FFilterDataSet.EndUpdate(LStateBookmark);
    cxGridDBTableView.EndUpdate;
  end;
end;

procedure TSBaseReportFilter.DoBeforePost(DataSet: TDataSet);
var
  LList: string;
  LName: Variant;
begin
  LList := FFilterDataSet.FieldByName(SConst_List).AsString;
  LName := FFilterDataSet.FieldByName(SConst_ItemName).AsVariant;

  if VarIsNull(LName) or (VarToStrDef(LName, '') = '') then
    FFilterDataSet.FieldByName(SConst_ItemName).AsString := GetItemByList(LList).ExtractSqlName;
end;

procedure TSBaseReportFilter.mnClearGroupClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  DeleteFilterItems(False);
end;

procedure TSBaseReportFilter.mnDeleteFilterClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  DeleteFilterItems(True);
end;

procedure TSBaseReportFilter.mnFilterExceptClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  ChangeFilterAction(ftExcept);
end;

procedure TSBaseReportFilter.mnFilterIncludeClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  ChangeFilterAction(ftInclude);
end;

procedure TSBaseReportFilter.DoViewClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
var
  LItem: TFilterCollectionItem;
  LForm: TSBaseFrm;
begin
  LItem := GetItemByList(FFilterDataSet.FieldByName(SConst_List).AsString);
  LForm := Owner as TSBaseFrm;

  LItem.Properties.ViewItemReaction.ClassConstants[SConst_Id] := FFilterDataSet.FieldByName(SConst_Id).AsInteger;

  (LItem.Properties as TSBaseFilterItemProperties).FViewItemReaction.Execute
  (
    LForm.GetFormParamValues,
    LForm.ConfirmMessage
  );
end;

procedure TSBaseReportFilter.mnNewClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
var
  LItem: TFilterCollectionItem;
  LForm: TSBaseFrm;
begin
  LItem := GetItemByList(FFilterDataSet.FieldByName(SConst_List).AsString);
  LForm := Owner as TSBaseFrm;
  (LItem.Properties as TSBaseFilterItemProperties).FNewItemReaction.Execute
  (
    LForm.GetFormParamValues,
    LForm.ConfirmMessage
  );
end;

procedure TSBaseReportFilter.mnOpenFileClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);

  // получить массив с фильтрами: <Имя, Тип>
  function CreateFilterList: Variant;
  var
    i: Integer;
    LItem: TSBaseFilterItemProperties;
    LValue: Variant;
  begin
    Result := VarArrayCreate([0, Count - 1], varVariant);

    for i := 0 to Count - 1 do begin
      LItem      := FFilterItems[i].Properties as TSBaseFilterItemProperties;
      LValue     := VarArrayCreate([0, 2], varVariant);
      LValue[0]  := LItem.Caption;
      LValue[1]  := LItem.List;
      LValue[2]  := FFilterItems[i].Index;
      Result[i]  := LValue;
    end;
  end;

var
  i: Integer;
  LItem: TFilterCollectionItem;
begin
  ClearFilter;

  with TSBaseImport
    (
      TSBaseImport.CreateByParams
      (
        Owner,
        TNamedVariants.Create
        (
          [
            TNamedVariant.Create(SImpPrefix + SImpParam_TemplateId, Options.ImportTemplateId),
            TNamedVariant.Create(SImpPrefix + SConst_DataSet,       CastAsVariant(FFilterDataSet)),
            TNamedVariant.Create(SImpPrefix + SConst_OpenDialog,    SConst_File),
            TNamedVariant.Create(SImpPrefix + SConst_Thread,        False),

            // массив с фильтрами нужен для проверки файла
            TNamedVariant.Create(SConst_ListCount,  Count),
            TNamedVariant.Create(SConst_List,       CreateFilterList)
          ],
          True
        )
      )
    )
  do
  try
    RunByParams;
  finally
    Free;
  end;

  // Обновить Action у фильтров
  for i := 0 to Count - 1 do begin
    LItem := FFilterItems[i];
    if FFilterDataSet.Locate(SConst_List, LItem.Properties.List, []) then begin
      LItem.Properties.Action := TFilterType(FFilterDataSet.FieldByName(SConst_Action).AsInteger);
      LItem.EnableMenu := False;
    end;
  end;
end;

procedure TSBaseReportFilter.DeleteFilterItems(AClearAll: Boolean);
var
  LList: string;
  LItem: TFilterCollectionItem;
  LStateBookmark: TDataSourceStateBookmark;
begin
  LList := FFilterDataSet.FieldByName(SConst_List).AsString;
  LItem := GetItemByList(LList);
  Assert(Assigned(LItem));

  cxGridDBTableView.BeginUpdate;
  FFilterDataSet.BeginUpdate(LStateBookmark);
  try
    SetFilterByList(LItem.Properties.List);

    FFilterDataSet.First;
    while not FFilterDataSet.Eof do
      FFilterDataSet.Delete;

    if AClearAll then
      LItem.EnableMenu := True
    else
      LItem.FillDataSet(FFilterDataSet);
  finally
    FFilterDataSet.EndUpdate(LStateBookmark);
    cxGridDBTableView.EndUpdate;
  end;
end;

procedure TSBaseReportFilter.ClearFilter;
var
  LStateBookmark: TDataSourceStateBookmark;
begin
  cxGridDBTableView.BeginUpdate;
  FFilterDataSet.BeginUpdate(LStateBookmark);
  try
    FFilterDataSet.Filtered := False;
    FFilterDataSet.First;

    while not FFilterDataSet.Eof do
      FFilterDataSet.Delete;
  finally
    FFilterDataSet.EndUpdate(LStateBookmark);
    cxGridDBTableView.EndUpdate;
  end;
end;

procedure TSBaseReportFilter.mnSaveToFileClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
var
  LFilePath: String;
begin
  // выбор пути
  LFilePath := '';
  with TSaveDialog.Create(Self) do
    try
      Filter      := SConst_FLTSaveDialogFilter;
      DefaultExt  := SConst_FltFileExtension;
      if Execute then
        LFilePath := FileName
    finally
      Free;
    end;

  // сохранение
  if not LFilePath.IsEmpty then
    GetXml(LFilePath);
end;

{$ENDIF}
{$ENDREGION}

{$REGION 'TFilterCollection'}

constructor TFilterCollection.Create(AFilter: TSBaseReportFilter);
begin
  inherited Create(TFilterCollectionItem);

  FFilter := AFilter;
end;

function TFilterCollection.GetFilterItem(AIndex: Integer): TFilterCollectionItem;
begin
  Result := Items[AIndex] as TFilterCollectionItem;
end;

{$ENDREGION}

{$REGION 'TSBaseReportFilterRepository'}

constructor TSBaseReportFilterRepository.Create(AOwner: TComponent);
begin
  Assert(not Assigned(SBaseReportFilterRepository));

  inherited;

  SBaseReportFilterRepository := Self;
  FOptions := TSBaseReportFilterOptions.Create;
end;

destructor TSBaseReportFilterRepository.Destroy;
begin
  FreeAndNil(FOptions);
  SBaseReportFilterRepository := nil;

  inherited;
end;

function TSBaseReportFilterRepository.GetChildOwner: TComponent;
begin
  Result := Self;
end;

{$ENDREGION}

{$REGION 'TFilterCollectionItem'}

constructor TFilterCollectionItem.Create(ACollection: TCollection);
begin
  Assert(ACollection is TFilterCollection);

  inherited;
  FEnableMenu := True;
  FProperties := TSBaseFilterItemProperties.Create;
  FProperties.FOnChange := DoPropertiesChange;

  CreateGUIObjects;
end;

procedure TFilterCollectionItem.CreateGUIObjects;
{$IFNDEF PACKAGE}
var
  LFilter: TSBaseReportFilter;
  LForm: TSBaseFrm;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  LFilter               := GetFilter;
  LForm                 := LFilter.Owner as TSBaseFrm;

  FMenuItem             := TdxBarButton.Create(LFilter);
  FMenuItem.BarManager  := LForm.BarManager;
  FMenuItem.Data        := Self;
  FMenuItem.Caption     := Properties.Caption;
  FMenuItem.OnClick     := LFilter.DoAddFilter;

  with LFilter.mnNewFilter.ItemLinks.Add do begin
    Item      := FMenuItem;
    Visible   := True;
  end;

  FProperties.CreateItemReactions(LForm, DoSelectCallBack);
{$ENDIF}
end;

procedure TFilterCollectionItem.DoPropertiesChange(ASender: TObject);
begin
  if Assigned(FMenuItem) then
    FMenuItem.Caption := Properties.Caption;
end;

{$IFNDEF PACKAGE}

function TFilterCollectionItem.ExtractSqlName: string;
var
  LVarItems: TNamedVariants;
  LParams: TNamedVariants;
  Idx: Integer;
  LForm: TSBaseFrm;
begin
  Result := '';

  LForm   := GetFilter.Owner as TSBaseFrm;

  if not Properties.ExtractSQL.IsEmpty then begin
    LParams := LForm.Variables;
    LParams[SConst_Id] := GetFilter.FilterDataSet.FieldByName(SConst_Id).AsVariant;
    LVarItems := LForm.SQLConnectionProvider.ExecSQL(Properties.ExtractSQL, LParams);

    if LVarItems.Find(SConst_DisplayText, Idx) then
      Result := VarToStr(LVarItems.Items[Idx].Value);
  end;
end;

procedure TFilterCollectionItem.DoSelectCallBack(ASelectAction: TCallBackSelectAction; const AResultValues: TNamedVariants);
begin
  if ASelectAction = cbsaSingleRecord then
    GetFilter.DoSelectCallBack(Self, AResultValues);
end;

{$ENDIF}

destructor TFilterCollectionItem.Destroy;
begin
  FreeAndNil(FProperties);

  inherited;
end;

{$IFNDEF PACKAGE}

procedure TFilterCollectionItem.FillDataSet(ADataSet: TDataSet; AItemName: String; AItemId: Integer);
begin
  ADataSet.Insert;
  ADataSet.FieldByName(SConst_MasterField_Id).AsInteger   := Index;
  ADataSet.FieldByName(SConst_DisplayText).AsString       := GetDisplayName;
  ADataSet.FieldByName(SConst_List).AsString              := Properties.List;
  ADataSet.FieldByName(SConst_Action).AsInteger           := Integer(Properties.Action);

  if AItemId <> -1 then begin
    ADataSet.FieldByName(SConst_Id).AsInteger             := AItemId;
    ADataSet.FieldByName(SConst_ItemName).AsString        := AItemName;
  end
  else
    ADataSet.FieldByName(SConst_ItemName).AsString   := SBaseProperties.Message_FilterNotPresent;

  ADataSet.Post;
end;

procedure TFilterCollectionItem.SetEnable(const AValue: Boolean);
begin
  if FEnableMenu <> AValue then begin
    FEnableMenu := AValue;
    if FMenuItem <> nil then
      FMenuItem.Enabled := AValue;
  end;
end;

{$ENDIF}

function TFilterCollectionItem.GetDisplayName: string;
begin
  Result := FProperties.GetDisplayName;
end;

function TFilterCollectionItem.GetProperties: TSBaseCustomFilterItemProperties;
begin
  Result := FProperties as TSBaseCustomFilterItemProperties;
end;

procedure TFilterCollectionItem.SetProperties(const AValue: TSBaseCustomFilterItemProperties);
begin
  FProperties.Assign(AValue);
end;

{$ENDREGION 'TFilterCollectionItem'}

{$REGION 'TSBaseCustomFilterItemProperties'}

procedure TSBaseCustomFilterItemProperties.AssignTo(ADest: TPersistent);
begin
  Assert(ADest is TSBaseCustomFilterItemProperties);

  with TSBaseCustomFilterItemProperties(ADest) do begin
    Caption             := Self.Caption;
    List                := Self.List;
    Action              := Self.Action;
    ExtractSQL          := Self.ExtractSQL;
  end;
end;

function TSBaseCustomFilterItemProperties.GetDisplayName: string;
begin
  Result := FilterTypeToStr(Action) + ' ' + Caption;
end;

procedure TSBaseCustomFilterItemProperties.SetAction(const AValue: TFilterType);
begin
  if AValue <> FAction then begin
    FAction := AValue;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSBaseCustomFilterItemProperties.SetCaption(const AValue: string);
begin
  if AValue <> Caption then begin
    FCaption := AValue;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSBaseCustomFilterItemProperties.SetExtractSQL(const AValue: String);
begin
  if FExtractSQL <> AValue then begin
    FExtractSQL := AValue;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

procedure TSBaseCustomFilterItemProperties.SetList(const AValue: string);
begin
  if AValue <> List then begin
    FList := AValue;
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;
end;

{$ENDREGION 'TSBaseFilterItemProperties'}

{$REGION 'TSBaseFilterRepositoryItem'}

constructor TSBaseFilterRepositoryItem.Create(AOwner: TComponent);
begin
  FProperties := TSBaseRepositoryFilterItemProperties.Create;

  inherited;
end;

destructor TSBaseFilterRepositoryItem.Destroy;
begin
  FreeAndNil(FProperties);

  inherited;
end;

{$ENDREGION 'TSBaseFilterRepositoryItem'}

{$REGION 'TSBaseRepositoryFilterItemProperties'}

procedure TSBaseRepositoryFilterItemProperties.AssignTo(ADest: TPersistent);
begin
  inherited;

{$IFNDEF PACKAGE}
  with ADest as TSBaseFilterItemProperties do begin
    FViewItemReaction.OnClick.Assign(Self.ViewItemReaction);
    FNewItemReaction.OnClick.Assign(Self.NewItemReaction);

    FNewItemReaction.OnClick.ClassConstants[SBFSelectMode] := SBFSM_SingleRepeatAble;
  end;
{$ENDIF}
end;

constructor TSBaseRepositoryFilterItemProperties.Create;
begin
  inherited;

{$IFNDEF PACKAGE}
  FNewItemReaction    := TBaseItemReactionOnClick.Create;
  FViewItemReaction   := TBaseItemReactionOnClick.Create;
{$ENDIF}
end;

destructor TSBaseRepositoryFilterItemProperties.Destroy;
begin
{$IFNDEF PACKAGE}
  FreeAndNil(FNewItemReaction);
  FreeAndNil(FViewItemReaction);
{$ENDIF}

  inherited;
end;

{$IFNDEF PACKAGE}

function TFilterCollectionItem.GetFilter: TSBaseReportFilter;
begin
  Result := (Collection as TFilterCollection).Filter;
end;

function TSBaseRepositoryFilterItemProperties.GetNewItemReaction: TBaseItemReactionOnClick;
begin
  Result := FNewItemReaction;
end;

function TSBaseRepositoryFilterItemProperties.GetViewItemReaction: TBaseItemReactionOnClick;
begin
  Result := FViewItemReaction;
end;

{$ENDIF}
{$ENDREGION 'TSBaseRepositoryFilterItemProperties'}

{$REGION 'TSBaseFilterItemProperties'}

procedure TSBaseFilterItemProperties.AssignTo(ADest: TPersistent);
begin
  inherited;

{$IFNDEF PACKAGE}
  with ADest as TSBaseFilterItemProperties do begin
    FNewItemReaction.OnClick.Assign(Self.NewItemReaction);
    FViewItemReaction.OnClick.Assign(Self.ViewItemReaction);
  end;
{$ENDIF}
end;

{$IFNDEF PACKAGE}

procedure TSBaseFilterItemProperties.CreateItemReactions(AForm: TSBaseFrm; ACallBackProc: TCallBackSelectFunction);
begin
  FNewItemReaction                                      := AForm.ItemsReactions.Add;
  FNewItemReaction.OnSelect.Notify                      := ACallBackProc;
  FNewItemReaction.OnClick.ClassConstants[SBFSelectMode]:= SBFSM_SingleRepeatAble;

  FViewItemReaction                                     := AForm.ItemsReactions.Add;
end;

function TSBaseFilterItemProperties.GetNewItemReaction: TBaseItemReactionOnClick;
begin
  Result := FNewItemReaction.OnClick;
end;

function TSBaseFilterItemProperties.GetViewItemReaction: TBaseItemReactionOnClick;
begin
  Result := FViewItemReaction.OnClick;
end;

{$ENDIF}
{$ENDREGION 'TSBaseFilterItemProperties'}

{$REGION 'TSBaseReportFilterOptions'}

procedure TSBaseReportFilterOptions.AssignTo(ADest: TPersistent);
begin
  with ADest as TSBaseReportFilterOptions do begin
    ExportObjectId     := Self.ExportObjectId;
//    ExportPatternId   := Self.ExportPatternId;
    ImportTemplateId   := Self.ImportTemplateId;
  end;
end;

{$ENDREGION 'TSBaseReportFilterOptions'}

initialization
  RegisterClasses
  ([
    TSBaseReportFilter,
    TSBaseReportFilterRepository,
    TSBaseFilterRepositoryItem
  ]);

finalization
  UnRegisterClasses
  ([
    TSBaseReportFilter,
    TSBaseReportFilterRepository,
    TSBaseFilterRepositoryItem
  ]);
end.
