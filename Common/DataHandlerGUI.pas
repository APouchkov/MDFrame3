{******************************************************************************}
{                                 DataHandlerGUI                               }
{                                                                              }
{  - Генерит PopupMenu для cxGrid/cxTreeList/cxVGrid                           }
{                                                                              }
{                              build 2008-09-30                                }
{******************************************************************************}

unit DataHandlerGUI;

interface

uses
  Generics.Collections, DB, Controls, dxBar, dxMDBar, Classes, Contnrs, ImgList, Types,
  cxStyles, DataHandler, NamedVariables, FieldDataLink
, fs_iinterpreter
, Dialogs, SBaseDataSourceControls
, SBaseItemReactions
  ;

type
  EBaseGUISupport = class(EDataHandler);

  TCustomDataHandlerCrack = class(TCustomDataHandler);

  TdhGridControl = class;
  TMenuHolder = class;

  TBaseGUISupport = class(TComponent)
  private
    FGUI: TWinControl;
    [Weak] FBarManager: TdxBarManager;
    FMenuHolderList: TObjectList;
    FGridControlClasses: TClassList;
    FGridControlList: TObjectList;

    procedure GetGridControlClass(AClass: TPersistentClass);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    function DhGridControl(ADataHandler: TCustomDataHandler): TdhGridControl;
    function GetGrid(ADataHandler: TCustomDataHandler): TComponent;

    function DhMenuHolder(ADataHandler: TCustomDataHandler): TMenuHolder;
    procedure UpdateMenu(ADataHandler: TCustomDataHandler);

    constructor Create(AGUI: TWinControl); reintroduce;
    destructor Destroy; override;

    procedure Init(ADataHandler: TDataHandler);
  end;

  TGUISupportClass = class of TBaseGUISupport;

  {--- Контрол, к которому цепляется меню --- }
  TdhGridControl = class(TInterfacedPersistent)   //TODO: наследовать от TDataControl(обычные edit'ы)
  private
    FMenuHolder: TMenuHolder;
    procedure SetMenuHolder(AMenuHolder: TMenuHolder);
  protected
    class function AcceptDataSource(ADataSource: TDataSource; out ADataHandler: TCustomDataHandler): Boolean;
  public
    Properties: TDataSourceControlProperties;

    [Weak] Component: TComponent;
    [Weak] DataHandler: TSubDataHandler;

    constructor Create(AComponent: TComponent; ADataHandler: TSubDataHandler); reintroduce; virtual;
    destructor Destroy; override;

    procedure UpdateReadOnly;

    property MenuHolder: TMenuHolder read FMenuHolder write SetMenuHolder;

    //отдать датасет если контрол соответствующего типа
    class function Accept(AComponent: TComponent; out ADataHandler: TCustomDataHandler): Boolean; virtual; abstract;
  end;

  {--- Элемент меню --- }
  TMenuHolder = class
  private
    [Weak] FBarManager: TdxBarManager;

    FMultiNewButton : TObject;
    FMultiNewButtons: TObjectList;

    FPopupMenu      : TdxMDBarPopupMenu;
    FButtonList     : TObjectList;
    [Weak] FGridControl: TdhGridControl;

    FIsDataSetActive: Boolean;

    function GetDataHandler: TSubDataHandler;

    function ItemProps(AStorage: TdhItemPropStorage; AItem: TdxBarItem): TdhPropItem;
    function GetBeginGroup(AStorage: TdhItemPropStorage; AItem: TdxBarItem): Boolean;
  public
    procedure CreateButtons;
    procedure CreateMultiNewButtons;

    procedure UpdateButtons;

    constructor Create(ABarManager: TdxBarManager; AGridControl: TdhGridControl);
    destructor Destroy; override;

    property DataHandler: TSubDataHandler read GetDataHandler;
    property DhGridControl: TdhGridControl read FGridControl;
    property PopupMenu: TdxMDBarPopupMenu read FPopupMenu;
  end;


implementation

uses
  cxGridCustomView, cxGrid, cxDBData, cxDBTL, cxMDDBTreeList, SysUtils, cxGridLevel,
  TypInfo, cxGridCustomTableView, cxTL, cxCustomData, cxGridTableView, StrUtils,
  Math, Variants, Graphics, ItemPropStorage, SBaseDataModule, SBaseCxUtils,
  SBaseForm, SBaseConstants, SBaseUtils, cxVGrid, cxDBVGrid, SBaseControls, dxMDBarButton,
  SBaseVariantFunctions;

const
  SErrNoBarManager  : string = 'У PropStorage не указан BarManager';
  SErrNoPropStorage : string = 'Не указан PropStorage';

type
  TcxGridViewDhControl = class(TdhGridControl)
  public
    class function Accept(AComponent: TComponent; out ADataHandler: TCustomDataHandler): Boolean; override;
  end;

  TcxVGridViewDhControl = class(TdhGridControl)
  public
    class function Accept(AComponent: TComponent; out ADataHandler: TCustomDataHandler): Boolean; override;
  end;

  TcxDBTreeDhControl = class(TdhGridControl)
  public
    class function Accept(AComponent: TComponent; out ADataHandler: TCustomDataHandler): Boolean; override;
  end;
  TdhGridControlClass = class of TdhGridControl;

  {--- Кнопки меню ---}
  TCustomMenuButton = class
  private
    FMenuHolder: TMenuHolder;
    FButton    : TdxBarItem;

    FFreeNotifier: TMDFreeNotificator;

    function GetDataHandler: TSubDataHandler; inline;
    function GetGridControl: TdhGridControl; inline;

    procedure FreeNotification(Sender: TComponent);
  protected
    function BaseItemReactionKind: TBaseItemReactionKind; dynamic;

    function Images: TdhDefaultImages; inline;

    function CreateFreeNotifier: TMDFreeNotificator;
  public
    constructor Create(AMenuHolder: TMenuHolder); reintroduce;
    destructor Destroy; override;

    procedure ItemClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); dynamic; abstract;

    property Button: TdxBarItem read FButton;
    property DataHandler: TSubDataHandler read GetDataHandler;
    property DhGridControl: TdhGridControl read GetGridControl;
  end;

  TMenuButton = class(TCustomMenuButton)
  protected
    class function ItemClass: TdxBarItemClass; dynamic;

    procedure Init(AReaction: TBaseItemReaction); dynamic;
    function CanBeEnabled: Boolean; dynamic;

    function GetImageIndex: Integer; dynamic; abstract;
    class function GetCaption: String; dynamic; abstract;
    class function GetShortCut: TShortCut; dynamic;
  public
    constructor Create(AMenuHolder: TMenuHolder; ADefault: Boolean; ABeginGroup: Boolean); virtual; //reintroduce;
    destructor Destroy; override;
  end;

  TOptionsMenuButton = class(TMenuButton)
  protected
    function CanBeEnabled: Boolean; override;
    procedure Init(AReaction: TBaseItemReaction); override;
  public
    function GetActionProperties: TdhOptionsMenuActionProperties; dynamic; abstract;
  end;

  // New
  TMultiNewMenuButton = class(TOptionsMenuButton)
  protected
    function BaseItemReactionKind: TBaseItemReactionKind; override;
    class function ItemClass: TdxBarItemClass; override;

    function GetImageIndex: Integer; override;
    class function GetCaption: String; override;
    class function GetShortCut: TShortCut; override;

    function CanBeEnabled: Boolean; override;
    procedure ButtonClick(Sender: TObject);
    procedure ItemClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); override;
    procedure Init(AReaction: TBaseItemReaction); override;
  public
    constructor Create(AMenuHolder: TMenuHolder; ADefault: Boolean; ABeginGroup: Boolean); override;

    function GetActionProperties: TdhOptionsMenuActionProperties; override;
    function OnGetEnabled(ASender: TObject): Boolean;

    procedure LinksUpdated;
  end;

  // Copy
  TCopyMenuButton = class(TOptionsMenuButton)
  protected
    function BaseItemReactionKind: TBaseItemReactionKind; override;

    function GetImageIndex: Integer; override;
    class function GetCaption: String; override;
//    class function GetShortCut: TShortCut; override;

    function CanBeEnabled: Boolean; override;
    procedure Init(AReaction: TBaseItemReaction); override;
  public
    function GetActionProperties: TdhOptionsMenuActionProperties; override;

    procedure ItemClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); override;
  end;

  // View
  TViewMenuButton = class(TOptionsMenuButton)
  protected
    function BaseItemReactionKind: TBaseItemReactionKind; override;

    function GetImageIndex: Integer; override;

    class function GetCaption: String; override;
    class function GetShortCut: TShortCut; override;

    procedure Init(AReaction: TBaseItemReaction); override;
  public
    function GetActionProperties: TdhOptionsMenuActionProperties; override;

    procedure ItemClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); override;
  end;

  // Edit
  TEditMenuButton = class(TOptionsMenuButton)
  protected
    function BaseItemReactionKind: TBaseItemReactionKind; override;

    function GetImageIndex: Integer; override;
    class function GetCaption: String; override;
    class function GetShortCut: TShortCut; override;

    function CanBeEnabled: Boolean; override;
    procedure Init(AReaction: TBaseItemReaction); override;
  public
    function GetActionProperties: TdhOptionsMenuActionProperties; override;

    procedure ItemClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); override;
  end;

  // Delete
  TDeleteMenuButton = class(TOptionsMenuButton)
  protected
    function BaseItemReactionKind: TBaseItemReactionKind; override;

    function GetImageIndex: Integer; override;
    class function GetCaption: String; override;
    class function GetShortCut: TShortCut; override;

    function CanBeEnabled: Boolean; override;
    procedure Init(AReaction: TBaseItemReaction); override;
  public
    function GetActionProperties: TdhOptionsMenuActionProperties; override;

    procedure ItemClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); override;
  end;

  // Refresh
  TRefreshMenuButton = class(TMenuButton)
  private
    function BaseItemReactionKind: TBaseItemReactionKind; override;
  protected
    function GetImageIndex: Integer; override;
    class function GetCaption: String; override;
    class function GetShortCut: TShortCut; override;
  end;

  // Apply
  TApplyMenuButton = class(TMenuButton)
  private
    function BaseItemReactionKind: TBaseItemReactionKind; override;
  protected
    function GetImageIndex: Integer; override;
    class function GetCaption: String; override;
    class function GetShortCut: TShortCut; override;
  public
  end;

  // Expand
  TExpandMenuButton = class(TMenuButton)
  private
    function BaseItemReactionKind: TBaseItemReactionKind; override;
  protected
    function GetImageIndex: Integer; override;
    class function GetCaption: String; override;
//    class function GetShortCut: TShortCut; override;
  end;

  // Collapse
  TCollapseMenuButton = class(TMenuButton)
  private
    function BaseItemReactionKind: TBaseItemReactionKind; override;
  protected
    function GetImageIndex: Integer; override;
    class function GetCaption: String; override;
//    class function GetShortCut: TShortCut; override;
  end;

  // Custom User
  TUserMenuButton = class(TCustomMenuButton)
  private
    procedure SetButton(const Value: TdxBarItem);
  public
    constructor Create(AMenuHolder: TMenuHolder; AItem: TdxBarItem; ADefault: Boolean; ABeginGroup: Boolean); reintroduce;

    property Button: TdxBarItem read FButton write SetButton;
  end;
  TCustomMenuButtonClass = class of TCustomMenuButton;

  TdhCustomMenuActionPropertiesCrack = class(TdhCustomMenuActionProperties);

{ TBaseGUISupport }

constructor TBaseGUISupport.Create(AGUI: TWinControl);
begin
  inherited Create(AGUI);

  FGUI := AGUI;
  FMenuHolderList     := TObjectList.Create;
  FGridControlList    := TObjectList.Create;
  FGridControlClasses := TClassList.Create;
end;

destructor TBaseGUISupport.Destroy;
begin
  FreeAndNil(FMenuHolderList);
  FreeAndNil(FGridControlList);
  FreeAndNil(FGridControlClasses);

  inherited;
end;

function TBaseGUISupport.GetGrid(ADataHandler: TCustomDataHandler): TComponent;
var
  LGridControl: TdhGridControl;
begin
  LGridControl := DhGridControl(ADataHandler);
  if Assigned(LGridControl) then
    Result := LGridControl.Component
  else
    Result := nil
end;

function TBaseGUISupport.DhGridControl(ADataHandler: TCustomDataHandler): TdhGridControl;
var
  I: Integer;
begin
  for I := 0 to Pred(FGridControlList.Count) do
    if TdhGridControl(FGridControlList[i]).DataHandler = ADataHandler then begin
      Result := TdhGridControl(FGridControlList[i]);
      Exit;
    end;

  Result := nil;
end;

procedure TBaseGUISupport.Init(ADataHandler: TDataHandler);

  procedure CreateControlClasses(ARootComponent: TComponent);
  var
    I, J: Integer;
    LComponent: TComponent;
    LCurrDH   : TCustomDataHandler;
  begin
    for I := 0 to Pred(ARootComponent.ComponentCount) do begin
      LComponent := ARootComponent.Components[I];

      for J := 0 to Pred(FGridControlClasses.Count) do
        if TdhGridControlClass(FGridControlClasses[J]).Accept(LComponent, LCurrDH) then begin
          if (LCurrDH <> ADataHandler) then begin
            LComponent.FreeNotification(Self);
            FGridControlList.Add(TdhGridControlClass(FGridControlClasses[J]).Create(LComponent, TSubDataHandler(LCurrDH)));
            Break
          end
        end;

      if LComponent is TWinControl then
        CreateControlClasses(LComponent);
    end;
  end;

  procedure CreateMenuHolders;
  var
    LPointer: Pointer;
    LGridControl: TdhGridControl absolute LPointer;
  begin
    for LPointer in FGridControlList do
      if pmDxBar in LGridControl.Properties.CanAssignPopupMenu then
        FMenuHolderList.Add(TMenuHolder.Create(FBarManager, LGridControl))
  end;

begin
  if not Assigned(ADataHandler.PropStorage) then
    Raise EBaseGUISupport.Create(SErrNoPropStorage);

  FBarManager := ADataHandler.PropStorage.BarManager;
  if not Assigned(FBarManager) then
    Raise EBaseGUISupport.Create(SErrNoBarManager);

  FGridControlList.Clear;
  FMenuHolderList.Clear;
  if FGridControlClasses.Count = 0 then
    with TClassFinder.Create(TdhGridControl) do
      try
        GetClasses(GetGridControlClass);
      finally
        Free;
      end;

  CreateControlClasses(FGUI);
  CreateMenuHolders;
end;

function TBaseGUISupport.DhMenuHolder(ADataHandler: TCustomDataHandler): TMenuHolder;
var
  I: Integer;
begin
  if Assigned(FMenuHolderList) then
    for I := 0 to Pred(FMenuHolderList.Count) do begin
      Result := TMenuHolder(FMenuHolderList[I]);
      if Result.DataHandler = ADataHandler then
        Exit;
    end;

  Result := nil;
end;

procedure TBaseGUISupport.UpdateMenu(ADataHandler: TCustomDataHandler);
var
  I: Integer;
begin
  if Assigned(FMenuHolderList) then
    for I := 0 to Pred(FMenuHolderList.Count) do
      with TMenuHolder(FMenuHolderList[I]) do
        if DataHandler = ADataHandler then
          UpdateButtons;
end;

procedure TBaseGUISupport.GetGridControlClass(AClass: TPersistentClass);
begin
  if (AClass <> TdhGridControl) and AClass.InheritsFrom(TdhGridControl) then
    FGridControlClasses.Add(AClass);
end;

procedure TBaseGUISupport.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited;

  if Operation = opRemove then begin
    for I := Pred(FMenuHolderList.Count) downto 0 do
      if (FMenuHolderList[I] as TMenuHolder).DhGridControl.Component = AComponent then begin
        FMenuHolderList.Delete(I);
        Break;
      end;

    for I := Pred(FGridControlList.Count) downto 0 do
      if (FGridControlList[I] as TdhGridControl).Component = AComponent then begin
        FGridControlList.Delete(I);
        Break;
      end;
  end;
end;

{ TCustomMenuButton }

function TCustomMenuButton.BaseItemReactionKind: TBaseItemReactionKind;
begin
  Result := biraCustomDataSourceActive;
end;

constructor TCustomMenuButton.Create(AMenuHolder: TMenuHolder);
begin
  inherited Create;
  FMenuHolder := AMenuHolder;
end;

function TCustomMenuButton.CreateFreeNotifier: TMDFreeNotificator;
begin
  if not Assigned(FFreeNotifier) then begin
    FFreeNotifier := TMDFreeNotificator.Create(nil);
    FFreeNotifier.OnFreeNotification := FreeNotification;
  end;
  Result := FFreeNotifier
end;

function TCustomMenuButton.GetGridControl: TdhGridControl;
begin
  Result := FMenuHolder.DhGridControl;
end;

destructor TCustomMenuButton.Destroy;
begin
  inherited;

  if Assigned(FFreeNotifier) then begin
    if Assigned(FButton) then
      FFreeNotifier.RemoveSender(FButton);
    FreeAndNil(FFreeNotifier);
  end;
end;

procedure TCustomMenuButton.FreeNotification(Sender: TComponent);
begin
  if FButton = Sender then
    FButton := nil;
end;

function TCustomMenuButton.GetDataHandler: TSubDataHandler;
begin
  Result := FMenuHolder.DataHandler;
end;

function TCustomMenuButton.Images: TdhDefaultImages;
begin
  Result := DataHandler.GetRoot.PropStorage.DefaultImages;
end;

{ TEditMenuButton }

procedure TEditMenuButton.Init(AReaction: TBaseItemReaction);
begin
  inherited;
  if Assigned(AReaction) then
    AReaction.OnClick.SystemProcedure := ItemClick;
end;

procedure TEditMenuButton.ItemClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  inherited;
  DataHandler.Edit(True);
  ADone := True;
end;

function TEditMenuButton.BaseItemReactionKind: TBaseItemReactionKind;
begin
  if DataHandler.OptionsMenu.OptionsEdit.GetActionMode = amInplace then
    Result := biraDataSourceInPlaceEdit
  else
    Result := biraCustomDataSourceRecord
end;

function TEditMenuButton.GetActionProperties: TdhOptionsMenuActionProperties;
begin
  Result := DataHandler.OptionsMenu.OptionsEdit
end;

class function TEditMenuButton.GetCaption: String;
begin
  Result := SBaseProperties.Text_MenuEdit
end;

function TEditMenuButton.GetImageIndex: Integer;
begin
  Result := Images.Edit
end;

class function TEditMenuButton.GetShortCut: TShortCut;
begin
  Result := ShortCut_F4
end;

function TEditMenuButton.CanBeEnabled: Boolean;
begin
  Result := (inherited CanBeEnabled) and DataHandler.CanModify;
end;

{ TViewMenuButton }

procedure TViewMenuButton.Init(AReaction: TBaseItemReaction);
begin
  inherited;
  if Assigned(AReaction) then
    AReaction.OnClick.SystemProcedure := ItemClick;
end;

procedure TViewMenuButton.ItemClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  inherited;
  DataHandler.View;
  ADone := True;
end;

function TViewMenuButton.BaseItemReactionKind: TBaseItemReactionKind;
begin
  Result := biraCustomDataSourceRecord
end;

function TViewMenuButton.GetActionProperties: TdhOptionsMenuActionProperties;
begin
  Result := DataHandler.OptionsMenu.OptionsView
end;

class function TViewMenuButton.GetCaption: String;
begin
  Result := SBaseProperties.Text_MenuView
end;

function TViewMenuButton.GetImageIndex: Integer;
begin
  Result := Images.View
end;

class function TViewMenuButton.GetShortCut: TShortCut;
begin
  Result := ShortCut_F3
end;

{ TDeleteMenuButton }

procedure TDeleteMenuButton.Init(AReaction: TBaseItemReaction);
begin
  inherited;
  if Assigned(AReaction) then
    AReaction.OnClick.SystemProcedure := ItemClick;
end;

procedure TDeleteMenuButton.ItemClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
var
  LGridControl  : TdhGridControl;
  LDataSourceStateBookmark: TDataSourceStateBookmark;
begin
  inherited;
  LGridControl := DhGridControl;
  LGridControl.Properties.BeginUpdate(LDataSourceStateBookmark);
  try
    if DataHandler.DataSet.UpdateStatus = usDeleted then
      DataHandler.Restore()
    else
      DataHandler.Delete();
  finally
    LGridControl.Properties.EndUpdate(LDataSourceStateBookmark, [sbrDisableCount], [sbrKeyFieldValues, sbrRowIndex]);
  end;
  ADone := True;
end;

function TDeleteMenuButton.BaseItemReactionKind: TBaseItemReactionKind;
begin
  Result := biraCustomDataSourceRecord
end;

function TDeleteMenuButton.GetActionProperties: TdhOptionsMenuActionProperties;
begin
  Result := DataHandler.OptionsMenu.OptionsDelete
end;

class function TDeleteMenuButton.GetCaption: String;
begin
  Result := SBaseProperties.Text_MenuDelete
end;

function TDeleteMenuButton.GetImageIndex: Integer;
begin
  Result := Images.Delete
end;

class function TDeleteMenuButton.GetShortCut: TShortCut;
begin
  Result := ShortCut_Ctrl_Del
end;

function TDeleteMenuButton.CanBeEnabled: Boolean;
begin
  Result := (inherited CanBeEnabled)
              or
              (
                (not DataHandler.OptionsMenu.KeyFieldsMode)
                and
                (
                  DataHandler.OptionsMenu.OptionsNew.Enabled
                  or
                  DataHandler.OptionsMenu.OptionsCopy.Enabled
                )
              );
end;

{ TMultiNewMenuButton }

procedure TMultiNewMenuButton.ItemClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  DataHandler.New(nil);
  ADone := True;
end;

function TMultiNewMenuButton.BaseItemReactionKind: TBaseItemReactionKind;
begin
  Result := biraCustomDataSourceActive
end;

function TMultiNewMenuButton.GetActionProperties: TdhOptionsMenuActionProperties;
begin
  Result := DataHandler.OptionsMenu.OptionsNew
end;

class function TMultiNewMenuButton.GetCaption: String;
begin
  Result := SBaseProperties.Text_MenuNew
end;

function TMultiNewMenuButton.GetImageIndex: Integer;
begin
  Result := Images.New;
end;

class function TMultiNewMenuButton.GetShortCut: TShortCut;
begin
  Result := ShortCut_Ins
end;

procedure TMultiNewMenuButton.Init(AReaction: TBaseItemReaction);
begin
  inherited;

  if Assigned(AReaction) then begin
    AReaction.OnGetEnabled.SystemFunction := OnGetEnabled;
    FMenuHolder.CreateMultiNewButtons;
  end;
end;

procedure TMultiNewMenuButton.ButtonClick(Sender: TObject);
begin
  DataHandler.New(TdhMultiNewItem(TdxBarItem(Sender).Data));
end;

function TMultiNewMenuButton.CanBeEnabled: Boolean;
begin
  Result := (inherited CanBeEnabled) and DataHandler.CanModify;
end;

constructor TMultiNewMenuButton.Create(AMenuHolder: TMenuHolder; ADefault, ABeginGroup: Boolean);
begin
  AMenuHolder.FMultiNewButton := Self;
  inherited;
end;

function TMultiNewMenuButton.OnGetEnabled(ASender: TObject): Boolean;
var
  LFocusedRecordIsData: Boolean;

  function UpdateItemLinksButtons(AItemLinks: TdxBarItemLinks): Boolean;
  var
    I: Integer;
    LItem: TdxBarItem;
    LEnabled: Boolean;
    LMultiNewItem: TdhMultiNewItem;
  begin
    Result := False;
    for I := 0 to AItemLinks.Count - 1 do begin
      LItem := AItemLinks.Items[I].Item;
      LMultiNewItem := TdhMultiNewItem(TdxBarItem(LItem).Data);

      TdxBarItem(LItem).Visible := VisibleToDxBarVisible(LMultiNewItem.Properties.Visible);
      LEnabled := LMultiNewItem.Properties.Visible
                    and LMultiNewItem.Properties.Enabled
                    and
                    (
                      not (dhdFocus in LMultiNewItem.Dependencies)
                      or
                      (
                        LFocusedRecordIsData
                        and
                        DhGridControl.Properties.EvaluateBoolean(LMultiNewItem.Properties.OnGetEnabled.Condition)
                        and
                        VarToBoolDef
                        (
                          DataHandler.SBaseFrm.CallFunction
                          (
                            LMultiNewItem.Properties.OnGetEnabled.ScriptFunction,
                            SConst_Properties,
                            Integer(LMultiNewItem.Properties),
                            False
                          ),
                          True
                        )
                      )
                    );

      if LEnabled and (LItem is TdxBarSubItem) then
        LEnabled := UpdateItemLinksButtons(TdxBarSubItem(LItem).ItemLinks);

      TdxBarItem(LItem).Enabled := LEnabled;
      Result := Result or LEnabled;
    end;
  end;

//var
//  LDataSetProvider: TSQLDataSetProvider;
begin
  with DataHandler.OptionsMenu do
    if Assigned(OptionsNew.OnGetEnabled.SystemFunction) then
      Result := OptionsNew.OnGetEnabled.SystemFunction(OptionsNew)
    else
      Result := True;

  if Result and (DataHandler.OptionsMenu.OptionsNew.Items.Count > 0) then begin
//    LDataSetProvider      := DataHandler.DataSet;
    LFocusedRecordIsData  := DhGridControl.Properties.FocusedRecodIsData;

    Result := UpdateItemLinksButtons(TdxMDBarButton(Button).ItemLinks);
  end;
end;

class function TMultiNewMenuButton.ItemClass: TdxBarItemClass;
begin
  Result := TdxMDBarButton;
end;

procedure TMultiNewMenuButton.LinksUpdated;
begin
  Button.SetItemClickEvent(ButtonClick);
end;

{ TRefreshMenuButton }

function TRefreshMenuButton.BaseItemReactionKind: TBaseItemReactionKind;
begin
  Result := biraDataSourceRefresh
end;

class function TRefreshMenuButton.GetCaption: String;
begin
  Result := SBaseProperties.Text_MenuRefresh
end;

function TRefreshMenuButton.GetImageIndex: Integer;
begin
  Result := Images.Refresh
end;

class function TRefreshMenuButton.GetShortCut: TShortCut;
begin
  Result := ShortCut_F5
end;

{ TApplyMenuButton }

function TApplyMenuButton.BaseItemReactionKind: TBaseItemReactionKind;
begin
  Result := biraDataSourceInPlacePost
end;

class function TApplyMenuButton.GetCaption: String;
begin
  Result := SBaseProperties.Text_MenuPost
end;

function TApplyMenuButton.GetImageIndex: Integer;
begin
  Result := Images.Apply
end;

class function TApplyMenuButton.GetShortCut: TShortCut;
begin
  Result := ShortCut_F6
end;

{ TCollapseMenuButton }

function TCollapseMenuButton.BaseItemReactionKind: TBaseItemReactionKind;
begin
  Result := biraDataSourceCollapse
end;

class function TCollapseMenuButton.GetCaption: String;
begin
  Result := SBaseProperties.Text_MenuCollapseAll
end;

function TCollapseMenuButton.GetImageIndex: Integer;
begin
  Result := Images.Collapse
end;

{ TExpandMenuButton }

function TExpandMenuButton.BaseItemReactionKind: TBaseItemReactionKind;
begin
  Result := biraDataSourceExpand
end;

class function TExpandMenuButton.GetCaption: String;
begin
  Result := SBaseProperties.Text_MenuExpandAll
end;

function TExpandMenuButton.GetImageIndex: Integer;
begin
  Result := Images.Expand
end;

{ TMenuHolder }

constructor TMenuHolder.Create(ABarManager: TdxBarManager; AGridControl: TdhGridControl);
begin
  inherited Create;

  FButtonList  := TObjectList.Create;
  FBarManager  := ABarManager;
  FGridControl := AGridControl;

  //CreateButtons;
  FPopupMenu := TdxMDBarPopupMenu.Create(FBarManager);
end;

destructor TMenuHolder.Destroy;
begin
  FreeAndNil(FButtonList);
  FreeAndNil(FPopupMenu);
  inherited;
end;

function TMenuHolder.ItemProps(AStorage: TdhItemPropStorage; AItem: TdxBarItem): TdhPropItem;
begin
  Result := AStorage.Properties.FindByBarItem(AItem) as TdhPropItem;
end;

function TMenuHolder.GetBeginGroup(AStorage: TdhItemPropStorage; AItem: TdxBarItem): Boolean;
var
  LProp: TdhPropItem;
begin
  LProp := ItemProps(AStorage, AItem);
  Result := Assigned(LProp) and LProp.BeginMenuGroup;
end;

procedure TMenuHolder.CreateMultiNewButtons;
var
  LStorage: TdhItemPropStorage;

  function FillMultiNewButtonItemLinks(AItemLinks: TdxBarItemLinks; AItems: TdhMultiNewItems): TdhActionDependencies;
  var
    I: Integer;
    LItem: TdxBarItem;
  begin
    Result := [];
    for I := 0 to Pred(AItems.Count) do
      with AItems[I] do begin
        if Properties.Visible then begin
          if Items.Count = 0 then begin
            LItem := TdxBarButton.Create(FBarManager);
            FMultiNewButtons.Add(LItem);
            //TdxBarNewButton(LItem).NewItemLink := AItems[I];
            LItem.Data := AItems[I];
            LItem.OnClick := OnClick;
          end else begin
            LItem := TdxBarSubItem.Create(FBarManager);
            FMultiNewButtons.Add(LItem);
            //TdxBarNewSubItem(LItem).NewItemLink := AItems[I];
            LItem.Data := AItems[I];
            Dependencies := FillMultiNewButtonItemLinks(TdxBarSubItem(LItem).ItemLinks, Items);
          end;
          Result := Result + Dependencies;

          LItem.Caption := Caption;
          with AItemLinks.Add do begin
            Item := LItem;
            BeginGroup := GetBeginGroup(LStorage, LItem);
          end;
        end;
      end;
  end;

var
  LBaseItemReaction: TBaseItemReaction;
  LKeyFieldMode: Boolean;
  LDependencies: TdhActionDependencies;
begin
  LKeyFieldMode := DataHandler.OptionsMenu.KeyFieldsMode;
  LStorage := DataHandler.GetRoot.PropStorage;
  if Assigned(FMultiNewButtons) then begin
    TdxMDBarButton(TMultiNewMenuButton(FMultiNewButton).Button).ItemLinks.Clear;
    FMultiNewButtons.Clear;
  end else if DataHandler.OptionsMenu.OptionsNew.Items.Count > 0 then begin
    FMultiNewButtons := TObjectList.Create;
    FMultiNewButtons.OwnsObjects := True;
  end;

  LBaseItemReaction := DataHandler.SBaseFrm.ItemsReactions.FindItemProperties(TMultiNewMenuButton(FMultiNewButton).Button);
  if DataHandler.OptionsMenu.OptionsNew.Items.Count = 0 then begin
    TMultiNewMenuButton(FMultiNewButton).Button.ShortCut := TMultiNewMenuButton.GetShortCut;

    with LBaseItemReaction.OnClick do begin
      if dhdFocus in DataHandler.OptionsMenu.OptionsNew.Dependencies then
        Reaction := IfThen(LKeyFieldMode, biraCustomDataSourceRecord, biraDataSourceInPlaceInsertChild)
//      else if DataHandler.OptionsMenu.OptionsNew.Dependencies = [dhdActive, dhdFocus] then
//        Reaction := biraCustomDataSourceActiveAndRecord
      else if DataHandler.OptionsMenu.OptionsNew.Dependencies = [dhdData] then
        Reaction := IfThen(LKeyFieldMode, biraCustomDataSource, biraDataSourceInPlaceInsertData)
      else
        Reaction := IfThen(LKeyFieldMode, biraCustomDataSourceActive, biraDataSourceInPlaceInsert);

      SystemProcedure := TMultiNewMenuButton(FMultiNewButton).ItemClick;
    end;
  end else begin
    TMultiNewMenuButton(FMultiNewButton).Button.ShortCut := 0;

    if Assigned(LBaseItemReaction) then
      LDependencies := FillMultiNewButtonItemLinks
      (
        TdxMDBarButton(TMultiNewMenuButton(FMultiNewButton).Button).ItemLinks,
        DataHandler.OptionsMenu.OptionsNew.Items
      )
    else
      LDependencies := [];

    if LDependencies = [] then begin
      if Assigned(LBaseItemReaction) then
        LBaseItemReaction.OnGetEnabled.Active := False
    end else begin
      DataHandler.OptionsMenu.OptionsNew.Dependencies := LDependencies;
      LBaseItemReaction.OnGetEnabled.Active := True;

      with LBaseItemReaction.OnClick do begin
        if DataHandler.OptionsMenu.OptionsNew.Dependencies = [dhdFocus] then
          Reaction := IfThen(LKeyFieldMode, biraDataSourceRecordSubMenu, biraDataSourceInPlaceInsertChildSubMenu)
        else if DataHandler.OptionsMenu.OptionsNew.Dependencies = [dhdActive, dhdFocus] then
          Reaction := IfThen(LKeyFieldMode, biraDataSourceActiveAndRecordSubMenu, biraDataSourceInPlaceInsertComboSubMenu)
        else if DataHandler.OptionsMenu.OptionsNew.Dependencies = [dhdData] then
          Reaction := IfThen(LKeyFieldMode, biraDataSourceSubMenu, biraDataSourceInPlaceInsertDataSubMenu)
        else
          Reaction := IfThen(LKeyFieldMode, biraDataSourceActiveSubMenu, biraDataSourceInPlaceInsertSubMenu);

        SystemProcedure := nil;
      end;
      LBaseItemReaction.OnGetEnabled.Condition := DataHandler.OptionsMenu.OptionsNew.OnGetEnabled.Condition;
    end;

    TMultiNewMenuButton(FMultiNewButton).LinksUpdated();

//    if Assigned(DhGridControl) then
//      DhGridControl.Properties.SetInsertEnabled(False);
  end;
end;

procedure TMenuHolder.CreateButtons;
var
  LStorage  : TdhItemPropStorage;
  LPropItem : TdhPropItem;

//  function AddButton(AMenuButtonClass: TCustomMenuButtonClass; ADefault: Boolean; ABeginGroup: Boolean; AItem: TdxBarItem = nil): TCustomMenuButton;
//  begin
//    Result := AMenuButtonClass.Create(Self, AItem, ADefault, ABeginGroup);
//    FButtonList.Add(Result);
//  end;

  procedure PullDownItem(AIndex, ADownIndex: Integer);
  var
    I: Integer;
    LCurItem: TdxBarItem;
  begin
    LCurItem := FPopupMenu.ItemLinks[AIndex].Item;
    for i := AIndex to Pred(ADownIndex) do
      FPopupMenu.ItemLinks[i] := FPopupMenu.ItemLinks[Succ(i)];

    with FPopupMenu.ItemLinks[ADownIndex] do begin
      Item := LCurItem;
      BeginGroup := GetBeginGroup(LStorage, LCurItem);
    end;
  end;

var
  LItem: TdxBarItem;
  I: Integer;
  LLastUserIndex, LMiddleIndex, LBottomIndex: Integer;
  LHasMiddleButtons, LHasBottomButtons: Boolean;

  function MenuButtonBeginGroup: Boolean;
  begin
    Result := (LLastUserIndex >= 0) and (LLastUserIndex = FPopupMenu.ItemLinks.Count - 1)
  end;
begin
  FMultiNewButton := nil;
  FButtonList.Clear;

  if Assigned(FPopupMenu) then
    FPopupMenu.ClearButtons;

  // TUserMenuButton
  // Сначала добавляем пользовательские кнопки
  LStorage := DataHandler.GetRoot.PropStorage;

  for I := 0 to Pred(LStorage.Properties.Count) do
    if (LStorage.Properties[I] as TdhPropItem).DataHandler = DataHandler then begin
      LItem := DataHandler.GetRoot.PropStorage.Properties[I].Item;
      if Assigned(LItem) then
        FButtonList.Add(TUserMenuButton.Create(Self, LItem, False, GetBeginGroup(LStorage, LItem)));
    end;

  LLastUserIndex := FPopupMenu.ItemLinks.Count - 1;

  // TMultiNewMenuButton
  with DataHandler.OptionsMenu.OptionsNew do
    if InternalVisible then begin
      TMultiNewMenuButton.Create(Self, False, MenuButtonBeginGroup);
      FButtonList.Add(FMultiNewButton);
    end;

  // TViewMenuButton
  with DataHandler.OptionsMenu.OptionsView do
    if InternalVisible then
      FButtonList.Add(TViewMenuButton.Create(Self, True, MenuButtonBeginGroup));

  // TEditMenuButton
  with DataHandler.OptionsMenu.OptionsEdit do
    if InternalVisible then
      FButtonList.Add(TEditMenuButton.Create(Self, True, MenuButtonBeginGroup));

  // TCopyMenuButton
  with DataHandler.OptionsMenu.OptionsCopy do
    if InternalVisible then
      FButtonList.Add(TCopyMenuButton.Create(Self, False, MenuButtonBeginGroup));

  // TApplyMenuButton
  with DataHandler.OptionsMenu.OptionsEdit do
    if Visible and (not DataHandler.OptionsMenu.KeyFieldsMode)
        and (DataHandler.GetRoot.EditMode <> dhemReadOnly) and (GetActionMode = amInplace)
    then
      FButtonList.Add(TApplyMenuButton.Create(Self, False, MenuButtonBeginGroup));

  LMiddleIndex := FPopupMenu.ItemLinks.Count - 1;

  if DhGridControl.Properties.SupportFolding then begin
    FButtonList.Add(TExpandMenuButton.Create(Self, False, True));
    FButtonList.Add(TCollapseMenuButton.Create(Self, False, False));
  end;

  // TRefreshMenuButton
  with DataHandler do
    if OptionsMenu.KeyFieldsMode
         or (GetRoot.EditMode = dhemReadOnly)
    then
      FButtonList.Add(TRefreshMenuButton.Create(Self, False, False));

  // TDeleteMenuButton
  with DataHandler.OptionsMenu.OptionsDelete do
    if InternalVisible then
      FButtonList.Add(TDeleteMenuButton.Create(Self, False, True));

  LBottomIndex := FPopupMenu.ItemLinks.Count - 1;

  // Расстановка пользовательских кнопок
  LHasMiddleButtons := False;
  LHasBottomButtons := False;
  if (LLastUserIndex >= 0) and ((LMiddleIndex <> LLastUserIndex) or (LBottomIndex <> LLastUserIndex)) then begin
    for I := LLastUserIndex downto 0 do begin
      LPropItem := ItemProps(LStorage, PopupMenu.ItemLinks[i].Item);
      if Assigned(LPropItem) then
        case LPropItem.Position of
          ipMiddle: begin
            PullDownItem(I, LMiddleIndex);
            Dec(LMiddleIndex);
            LHasMiddleButtons := True;
          end;
          ipLast: begin
            PullDownItem(I, LBottomIndex);
            Dec(LMiddleIndex);
            Dec(LBottomIndex);
            LHasBottomButtons := True;
          end;
        end;
    end;
    if LHasMiddleButtons then FPopupMenu.ItemLinks[Succ(LMiddleIndex)].BeginGroup := True;
    if LHasBottomButtons then FPopupMenu.ItemLinks[Succ(LBottomIndex)].BeginGroup := True;
  end;
end;

procedure TMenuHolder.UpdateButtons;
var
  LIsActive: Boolean;
begin
  with DataHandler do
    LIsActive := Assigned(DataSet) and DataSet.Active;

  if FIsDataSetActive <> LIsActive then begin
    if LIsActive then begin
      DhGridControl.MenuHolder := Self;
      CreateButtons;
    end else begin
      DhGridControl.MenuHolder := nil;
      FButtonList.Clear;
    end;

    FIsDataSetActive := LIsActive;
  end;

  if FIsDataSetActive then
    DhGridControl.Properties.DoUpdate([dsusActive, dsusData, dsusFocusedRecord]);
end;

function TMenuHolder.GetDataHandler: TSubDataHandler;
begin
  Result := FGridControl.DataHandler;
end;

{ TdhGridControl }

class function TdhGridControl.AcceptDataSource(ADataSource: TDataSource; out ADataHandler: TCustomDataHandler): Boolean;
begin
  Result := (ADataSource <> nil) and (ADataSource is TCustomDataHandler);
  if Result then
    ADataHandler := TCustomDataHandler(ADataSource)
  else
    ADataHandler := nil;
end;

constructor TdhGridControl.Create(AComponent: TComponent; ADataHandler: TSubDataHandler);
begin
  inherited Create;

  if Assigned(AComponent) then begin
    Properties := TSBaseFrm(ADataHandler.Owner).DataSourceControls.FindOrCreateGridControl(AComponent);

    Component           := AComponent;
    DataHandler         := ADataHandler;
    //UpdateReadOnly;
  end;
end;

procedure TdhGridControl.SetMenuHolder(AMenuHolder: TMenuHolder);
begin
  if FMenuHolder <> AMenuHolder then begin
    FMenuHolder := AMenuHolder;

    if pmDxBar in Properties.CanAssignPopupMenu then
      if Assigned(FMenuHolder) {and (FMenuHolder.PopupMenu.ItemLinks.VisibleItemCount > 0)} then begin
        Properties.PopupMenu := FMenuHolder.PopupMenu
      end else begin
        Properties.PopupMenu := nil;
      end;
  end;
end;

destructor TdhGridControl.Destroy;
begin
  if Assigned(Component) and not (csDestroying in Component.ComponentState) then begin
    if Assigned(Properties) and (Properties.GetWinControl <> nil) then
      SetMenuHolder(nil);
  end;

  inherited;
end;

procedure TdhGridControl.UpdateReadOnly;
begin
  TControlStateRules.CreateRules(Component).ReadOnly := (not DataHandler.CanModify);
end;

{ TcxGridViewDhControl }

class function TcxGridViewDhControl.Accept(AComponent: TComponent; out ADataHandler: TCustomDataHandler): Boolean;
begin
  if (AComponent is TcxCustomGridTableView)
        and (TcxCustomGridTableView(AComponent).PatternGridView = AComponent)
  then
    Result := AcceptDataSource(TcxDBDataController(TcxCustomGridView(AComponent).DataController).DataSource, ADataHandler)
  else
    Result := False
end;

{ TcxDBTreeDhControl }

class function TcxDBTreeDhControl.Accept(AComponent: TComponent; out ADataHandler: TCustomDataHandler): Boolean;
begin
  if (AComponent is TcxMDDBTreeList) then
    Result := AcceptDataSource(TcxMDDBTreeList(AComponent).DataController.DataSource, ADataHandler)
  else
    Result := False;
end;

{ TcxVGridViewDhControl }

class function TcxVGridViewDhControl.Accept(AComponent: TComponent; out ADataHandler: TCustomDataHandler): Boolean;
begin
  if (AComponent is TcxDBVerticalGrid) and (TcxDBVerticalGrid(AComponent).LayoutStyle = lsMultiRecordView)
  then
    Result := AcceptDataSource(TcxDBVerticalGrid(AComponent).DataController.DataSource, ADataHandler)
  else
    Result := False
end;

{ TUserMenuButton }

constructor TUserMenuButton.Create(AMenuHolder: TMenuHolder; AItem: TdxBarItem; ADefault, ABeginGroup: Boolean);
var
  LItemLink: TdxBarItemLink;
begin
  inherited Create(AMenuHolder);

  FButton := AItem;
  LItemLink := AMenuHolder.FPopupMenu.ItemLinks.Add;
  LItemLink.Item := FButton;

  LItemLink.BeginGroup := ABeginGroup;
  CreateFreeNotifier.AddSender(FButton);
end;

procedure TUserMenuButton.SetButton(const Value: TdxBarItem);
begin
  if FButton <> Value then begin
    if Assigned(FButton) and Assigned(FFreeNotifier) then FFreeNotifier.RemoveFreeNotification(FButton);

    FButton := Value;

    if Assigned(FButton) then CreateFreeNotifier.AddSender(FButton);
  end;
end;

{ TCopyMenuButton }

function TCopyMenuButton.BaseItemReactionKind: TBaseItemReactionKind;
begin
  if DataHandler.OptionsMenu.OptionsEdit.GetActionMode = amInplace then
    Result := biraDataSourceInPlaceCopy
  else
    Result := biraCustomDataSourceRecord
end;

function TCopyMenuButton.GetActionProperties: TdhOptionsMenuActionProperties;
begin
  Result := DataHandler.OptionsMenu.OptionsCopy
end;

class function TCopyMenuButton.GetCaption: String;
begin
  Result := SBaseProperties.Text_MenuCopy
end;

function TCopyMenuButton.GetImageIndex: Integer;
begin
  Result := Images.Copy
end;

//class function TCopyMenuButton.GetShortCut: TShortCut;
//begin
//  Result := ShortCut_Ctrl_C
//end;

procedure TCopyMenuButton.Init(AReaction: TBaseItemReaction);
begin
  inherited;
  if Assigned(AReaction) then
    AReaction.OnClick.SystemProcedure := ItemClick;
end;

procedure TCopyMenuButton.ItemClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  inherited;
  DataHandler.Copy;
  ADone := True;
end;

function TCopyMenuButton.CanBeEnabled: Boolean;
begin
  Result := (inherited CanBeEnabled) and DataHandler.CanModify;
end;

{ TMenuButton }

{$IFNDEF PACKAGE}
function TMenuButton.CanBeEnabled: Boolean;
begin
  Result := True;
end;

{$ENDIF}

constructor TMenuButton.Create(AMenuHolder: TMenuHolder; ADefault, ABeginGroup: Boolean);
var
  LItemClass  : TdxBarItemClass;
  LItemLink   : TdxBarItemLink;
  LReaction   : TBaseItemReaction;
begin
  inherited Create(AMenuHolder);

  LItemClass := ItemClass;
  LItemLink  := AMenuHolder.FPopupMenu.AddButton(LItemClass);

  FButton := LItemLink.Item;
  FButton.Enabled := False;

  if CanBeEnabled then begin
    LReaction := AMenuHolder.DataHandler.SBaseFrm.ItemsReactions.Add;
    with LReaction do begin
      Item := FButton;
      LinkedComponent := AMenuHolder.DhGridControl.Component;
      Default := ADefault;

      OnClick.Reaction := BaseItemReactionKind;
    end;
  end else
    LReaction := nil;

  Init(LReaction);
  LItemLink.BeginGroup := ABeginGroup;
  CreateFreeNotifier.AddSender(FButton);
end;

destructor TMenuButton.Destroy;
begin
  FreeAndNil(FButton);

  inherited;
end;

class function TMenuButton.GetShortCut: TShortCut;
begin
  Result := 0;
end;

procedure TMenuButton.Init(AReaction: TBaseItemReaction);
begin
  Button.ImageIndex := GetImageIndex;
  Button.Caption    := GetCaption;
  Button.ShortCut   := GetShortCut;
end;

class function TMenuButton.ItemClass: TdxBarItemClass;
begin
  Result := TdxBarButton;
end;

{ TOptionsMenuButton }

function TOptionsMenuButton.CanBeEnabled: Boolean;
begin
  Result := GetActionProperties.Enabled;
end;

procedure TOptionsMenuButton.Init(AReaction: TBaseItemReaction);
var
  LProperties : TdhOptionsMenuActionProperties;
begin
  inherited;

  if Assigned(AReaction) then begin
    LProperties := GetActionProperties;
    if LProperties.ScanFields then
      AReaction.OnGetEnabled.Condition := LProperties.OnGetEnabled.Condition;
    AReaction.OnGetEnabled.ScriptFunction := LProperties.OnGetEnabled.ScriptFunction;
  end;
end;

initialization
//  dxBarRegisterItem(TdxBarNewButton, TdxBarButtonControl, True);
  RegisterClasses([TcxGridViewDhControl, TcxDBTreeDhControl, TcxVGridViewDhControl])

finalization
//  dxBarUnregisterItem(TdxBarNewButton);
  UnRegisterClasses([TcxGridViewDhControl, TcxDBTreeDhControl, TcxVGridViewDhControl])

end.
