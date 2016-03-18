unit NamedEditRepository;
{$I config.inc}

interface

uses
  System.Classes, Forms, cxEdit, System.SysUtils, Variants, Graphics, DB,
  cxTL, cxDBTL, cxTLData,
  cxGridTableView, cxGridCustomTableView,
  cxGraphics, DataHandler;

type
  TcxNamedEditRepositoryItems = class;

  TcxNamedEditRepositoryItem = class(TCollectionItem)
  private
    FName, FEditName: String;
    FItem: TcxEditRepositoryItem;
    FNamedEditRepositoryItems: TcxNamedEditRepositoryItems;
  public
    Constructor Create(AItems: TCollection); override;
  published
    property Name: String read FName write FName;
    property EditName: String Read FEditName write FEditName;
    property Item: TcxEditRepositoryItem Read FItem Write FItem;
  end;

  TcxGUIEditRepositoryItem = class(TCollectionItem)
  private
    FEditName: String;
    FItem: TcxEditRepositoryItem;
  published
    property EditName: String Read FEditName write FEditName;
    property Item: TcxEditRepositoryItem Read FItem Write FItem;
  end;

  TcxNamedEditRepository = class;

  TcxNamedEditRepositoryItems = class(TCollection)
  protected
    function GetItem(Index: Integer): TcxNamedEditRepositoryItem;
    procedure SetItem(Index: Integer; const Value: TcxNamedEditRepositoryItem);
  public
    function Add(Value:TcxEditRepositoryItem): TcxNamedEditRepositoryItem;
    property Items[Index: Integer]: TcxNamedEditRepositoryItem read GetItem write SetItem; default;
  end;

  TcxGUIEditRepositoryItems = class(TCollection)
  protected
    function GetItem(Index: Integer): TcxGUIEditRepositoryItem;
    procedure SetItem(Index: Integer; const Value: TcxGUIEditRepositoryItem);
  public
    function Add(Value:TcxEditRepositoryItem): TcxGUIEditRepositoryItem;
    property Items[Index: Integer]: TcxGUIEditRepositoryItem read GetItem write SetItem; default;
  end;

  TcxNamedEditRepository = class(TComponent)
  private
    FItems: TcxNamedEditRepositoryItems;
  protected
    function GetItemByName(AName: String): TcxEditRepositoryItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property NamedItems[AName: String]: TcxEditRepositoryItem read GetItemByName;
  published
    property Items: TcxNamedEditRepositoryItems read FItems write FItems;
  end;

  TcxVariantColumnLiteProperties = class(TComponent)
  private
    FColumnType : TComponent;
    FColumnValue: TComponent;
    FParent     : TComponent;

    procedure OnGetEditProperties(const AType: String; var AEditProperties: TcxCustomEditProperties);

    procedure GridViewColumnOnGetPropertiesForEdit(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AEditProperties: TcxCustomEditProperties);
    procedure SetColumnValue(const Value: TComponent);
  protected
    procedure SetParentComponent(Value: TComponent); override;
  public
//    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;

    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;
  published
    property ColumnType : TComponent read FColumnType  write FColumnType;
    property ColumnValue: TComponent read FColumnValue write SetColumnValue;
  end;

  TcxGridVariantColumnProperties = class(TComponent)
  private
    FItems: TcxGUIEditRepositoryItems;
    FColumnValue: TComponent;
    FParent: TComponent;
    FGroupColor: TColor;

    FDefinitionsDataSet: TDataSet;
    FDefinitionFieldKey, FDefinitionFieldType, FDefinitionFieldEditType, FDefinitionFieldEditName: String;

    procedure SetColumnValue(Value: TComponent);
  protected
    function GetItemByName(const AEditName: String): TcxEditRepositoryItem;
    procedure SetParentComponent(Value: TComponent); override;
    procedure OnGetEditProperties(AKey: Variant; var AEditProperties: TcxCustomEditProperties);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function HasParent: Boolean; override;
    function GetParentComponent: TComponent; override;

    procedure TreeListColumnOnGetEditProperties(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var AEditProperties: TcxCustomEditProperties);
    procedure TreeListOnIsGroupNode(Sender: TcxCustomTreeList;ANode: TcxTreeListNode; var IsGroup: Boolean);
    procedure TreeListCustomDrawCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);

    procedure GridViewColumnOnGetPropertiesForEdit(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AEditProperties: TcxCustomEditProperties);
  published
    property ColumnValue: TComponent read FColumnValue write SetColumnValue;

    property DefinitionsDataSet       : TDataSet  read FDefinitionsDataSet      write FDefinitionsDataSet;
    property DefinitionFieldKey       : String    read FDefinitionFieldKey      write FDefinitionFieldKey;
    property DefinitionFieldType      : String    read FDefinitionFieldType     write FDefinitionFieldType;
    property DefinitionFieldEditType  : String    read FDefinitionFieldEditType write FDefinitionFieldEditType;
    property DefinitionFieldEditName  : String    read FDefinitionFieldEditName write FDefinitionFieldEditName;

    property Items: TcxGUIEditRepositoryItems read FItems write FItems;
    property GroupColor: TColor read FGroupColor write FGroupColor;
  end;

type
  TcxCustomTreeListCrack = class(TcxCustomTreeList);

var
  cxNamedEditRepository: TcxNamedEditRepository;

implementation

uses DataModule, SBaseVariantFunctions, SBaseStringFunctions, SBaseDataModule;

{ TcxNamedEditRepository  }

constructor TcxNamedEditRepository.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TcxNamedEditRepositoryItems.Create(TcxNamedEditRepositoryItem);
end;

destructor TcxNamedEditRepository.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TcxNamedEditRepository.GetItemByName(AName: String): TcxEditRepositoryItem;
var
  I:integer;
begin
  for I := 0 to FItems.Count - 1 do
    if FItems[I].FEditName <> '' then begin
      if SameText(FItems[I].FEditName, AName) then begin
        Result := FItems[I].FItem;
        Exit;
      end;
    end else begin
      if SameText(FItems[I].FName, AName) then begin
        Result := FItems[I].FItem;
        Exit;
      end;
    end;
  Result := nil;
end;

{ TcxNamedEditRepositoryItems }

function TcxNamedEditRepositoryItems.GetItem(Index: Integer): TcxNamedEditRepositoryItem;
begin
  Result := TcxNamedEditRepositoryItem(inherited GetItem(Index))
end;

procedure TcxNamedEditRepositoryItems.SetItem(Index: Integer; const Value: TcxNamedEditRepositoryItem);
begin
  inherited SetItem(Index, Value);
end;

function TcxNamedEditRepositoryItems.Add(Value:TcxEditRepositoryItem): TcxNamedEditRepositoryItem;
begin
  Result := TcxNamedEditRepositoryItem(inherited Add);
end;

{ TcxNamedEditRepositoryItem }

constructor TcxNamedEditRepositoryItem.Create(AItems: TCollection);
begin
  inherited;
  FNamedEditRepositoryItems := TcxNamedEditRepositoryItems(AItems);
end;

{ TcxGridVariantColumnProperties }

constructor TcxGridVariantColumnProperties.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TcxGUIEditRepositoryItems.Create(TcxGUIEditRepositoryItem);
end;

destructor TcxGridVariantColumnProperties.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TcxGridVariantColumnProperties.GetItemByName(const AEditName: String): TcxEditRepositoryItem;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    if SameText(FItems[I].FEditName, AEditName) then
      Exit(FItems[I].FItem);

//  Result := nil;
  Raise Exception.Create('Repository item with name = "' + AEditName + '" not found')
end;

function TcxGridVariantColumnProperties.HasParent: Boolean;
begin
  Result := Assigned(FParent)
end;

function TcxGridVariantColumnProperties.GetParentComponent: TComponent;
begin
  Result := FParent;
end;

procedure TcxGridVariantColumnProperties.SetColumnValue(Value: TComponent);
begin
  FColumnValue := Value;

  if Assigned(FColumnValue) then
    if (FParent is TcxCustomTreeList)  then
      (FColumnValue as TcxTreeListColumn).OnGetEditProperties := TreeListColumnOnGetEditProperties

    else if FParent is TcxGridTableView then begin
      (FColumnValue as TcxGridColumn).OnGetProperties         := GridViewColumnOnGetPropertiesForEdit;
      (FColumnValue as TcxGridColumn).OnGetPropertiesForEdit  := GridViewColumnOnGetPropertiesForEdit;
    end

    else
      raise Exception.Create('Unexpected Parent component for TcxGridVariantColumnProperties');
end;

procedure TcxGridVariantColumnProperties.SetParentComponent(Value: TComponent);
begin
  FParent := Value;
  inherited;

  if (FParent is TcxCustomTreeList)  then begin
    TcxCustomTreeListCrack(FParent).OnIsGroupNode := TreeListOnIsGroupNode;
    TcxCustomTreeListCrack(FParent).OnCustomDrawDataCell := TreeListCustomDrawCell;
  end;

  if Assigned(ColumnValue) then
    SetColumnValue(ColumnValue);
end;

procedure TcxGridVariantColumnProperties.TreeListCustomDrawCell(Sender: TcxCustomTreeList; ACanvas: TcxCanvas; AViewInfo: TcxTreeListEditCellViewInfo; var ADone: Boolean);
begin
  if not AViewInfo.Node.Focused and AViewInfo.Node.IsGroupNode then
    if FGroupColor > 0 then
      ACanvas.Font.Color := FGroupColor
end;

procedure TcxGridVariantColumnProperties.OnGetEditProperties(AKey: Variant; var AEditProperties: TcxCustomEditProperties);
var
  LEditRepositoryItem: TcxEditRepositoryItem;
  LDefinitions: Variant;
begin
  if not VarIsPresent(AKey) then Exit;

  LDefinitions := FDefinitionsDataSet.Lookup
                  (
                    FDefinitionFieldKey,
                    AKey,
                    DefinitionFieldType + ';' + DefinitionFieldEditType + ';' + DefinitionFieldEditName
                  );
  if not VarIsPresent(LDefinitions) or not VarIsStr(LDefinitions[0]) then Exit;

  if VarIsStr(LDefinitions[2]) then
    LEditRepositoryItem := GetItemByName(LDefinitions[2])
  else if VarIsStr(LDefinitions[1]) then
    LEditRepositoryItem := cxNamedEditRepository.GetItemByName(LDefinitions[1])
  else
    LEditRepositoryItem := cxNamedEditRepository.GetItemByName(LDefinitions[0]);

  if Assigned(LEditRepositoryItem) then
    AEditProperties := LEditRepositoryItem.Properties
end;

procedure TcxGridVariantColumnProperties.TreeListColumnOnGetEditProperties(Sender: TcxTreeListColumn; ANode: TcxTreeListNode; var AEditProperties: TcxCustomEditProperties);
begin
  OnGetEditProperties(TcxDBTreeListNode(ANode).KeyValue, AEditProperties);
end;

procedure TcxGridVariantColumnProperties.TreeListOnIsGroupNode(Sender: TcxCustomTreeList; ANode: TcxTreeListNode; var IsGroup: Boolean);
var
  LDefinition: Variant;
begin
  LDefinition := FDefinitionsDataSet.Lookup
                  (
                    FDefinitionFieldKey,
                    TcxDBTreeListNode(ANode).KeyValue,
                    DefinitionFieldType
                  );

   IsGroup := VarIsNull(LDefinition);
end;

procedure TcxGridVariantColumnProperties.GridViewColumnOnGetPropertiesForEdit(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AEditProperties: TcxCustomEditProperties);
begin
  OnGetEditProperties(ARecord.GridView.DataController.GetRecordId(ARecord.Index), AEditProperties);
end;

{ TcxGUIEditRepositoryItems }

function TcxGUIEditRepositoryItems.Add(Value: TcxEditRepositoryItem): TcxGUIEditRepositoryItem;
begin
   Result := TcxGUIEditRepositoryItem(inherited Add);
end;

function TcxGUIEditRepositoryItems.GetItem(Index: Integer): TcxGUIEditRepositoryItem;
begin
  Result := TcxGUIEditRepositoryItem(inherited GetItem(Index));
end;

procedure TcxGUIEditRepositoryItems.SetItem(Index: Integer; const Value: TcxGUIEditRepositoryItem);
begin
  inherited SetItem(Index, Value);
end;

{ TcxVariantColumnLiteProperties }

//constructor TcxVariantColumnLiteProperties.Create(AOwner: TComponent);
//begin
//  inherited;
//  FItems := TcxGUIEditRepositoryItems.Create(TcxGUIEditRepositoryItem);
//end;
//
//destructor TcxVariantColumnLiteProperties.Destroy;
//begin
//  FItems.Free;
//  inherited;
//end;

function TcxVariantColumnLiteProperties.GetParentComponent: TComponent;
begin
  Result := FParent;
end;

procedure TcxVariantColumnLiteProperties.GridViewColumnOnGetPropertiesForEdit(Sender: TcxCustomGridTableItem; ARecord: TcxCustomGridRecord; var AEditProperties: TcxCustomEditProperties);
begin
  if Assigned(FColumnType) then
    OnGetEditProperties(VarToStr(ARecord.GridView.DataController.GetValue(ARecord.Index, TcxGridColumn(FColumnType).Index)), AEditProperties);
end;

function TcxVariantColumnLiteProperties.HasParent: Boolean;
begin
  Result := Assigned(FParent)
end;

procedure TcxVariantColumnLiteProperties.OnGetEditProperties(const AType: String; var AEditProperties: TcxCustomEditProperties);
var
  LEditRepositoryItem: TcxEditRepositoryItem;
begin
  if AType.IsEmpty then Exit;

  LEditRepositoryItem := cxNamedEditRepository.GetItemByName(AType);

  if Assigned(LEditRepositoryItem) then
    AEditProperties := LEditRepositoryItem.Properties
end;

procedure TcxVariantColumnLiteProperties.SetColumnValue(const Value: TComponent);
begin
  FColumnValue := Value;

  if Assigned(FColumnValue) then
//    if (FParent is TcxCustomTreeList)  then
//      (FColumnValue as TcxTreeListColumn).OnGetEditProperties := TreeListColumnOnGetEditProperties
//
//    else

    if FParent is TcxGridTableView then begin
      (FColumnValue as TcxGridColumn).OnGetProperties         := GridViewColumnOnGetPropertiesForEdit;
      (FColumnValue as TcxGridColumn).OnGetPropertiesForEdit  := GridViewColumnOnGetPropertiesForEdit;
    end

    else
      raise Exception.Create('Unexpected Parent component for TcxGridVariantColumnProperties');
end;

procedure TcxVariantColumnLiteProperties.SetParentComponent(Value: TComponent);
begin
  FParent := Value;
  inherited;

  if Assigned(ColumnValue) then
    SetColumnValue(ColumnValue);
end;

initialization
  cxNamedEditRepository := TcxNamedEditRepository.Create(Application);
  cxNamedEditRepository.Name := 'cxNamedEditRepository';

  RegisterClasses([TcxNamedEditRepository, TcxVariantColumnLiteProperties, TcxGridVariantColumnProperties]);

finalization
  UnRegisterClasses([TcxNamedEditRepository, TcxVariantColumnLiteProperties, TcxGridVariantColumnProperties]);

end.
