(*
  Негаш А.
  2012г
  Визуальная коллекция
*)

{$I CONFIG.INC}

unit SBaseItemHolder;

interface

uses
  Types, Classes, Windows, Messages, Graphics, Controls, ExtCtrls, cxScrollBox, StdCtrls,
  cxLabel, cxDropDownEdit, Forms
{$IFDEF DELPHIXE3}
  , System.UITypes
{$ENDIF}
  ;

type
  TResultArrayType            = (ratString = 1, ratVariant = 2);
  TCloseItemQueryEvent        = procedure(AObject: TObject; var ACanClose: Boolean);

  TBaseLabelItemsHolderItem   = class;
  TBaseItemsHolder            = class;
  TBaseEditItemsHolderItem    = class;
  TBaseCustomItemsHolderItem  = class;
  TBaseItemHolderClass        = class of TBaseCustomItemsHolderItem;

  TBaseItemWrapper = class(TCollectionItem)
  strict private
    FItemClass: TBaseItemHolderClass;
    FItem: TBaseCustomItemsHolderItem;

    procedure FreeOldObject;
  private
    function CreateItem(AItemClass: TBaseItemHolderClass): TBaseCustomItemsHolderItem;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Item: TBaseCustomItemsHolderItem read FItem;
  end;

  TBaseItemsHolderCollection = class(TCollection)
  strict private
    FParent: TBaseItemsHolder;
  protected
    procedure Update(AItem: TCollectionItem); override;
    constructor Create(AParent: TBaseItemsHolder);

    property Parent: TBaseItemsHolder read FParent;
  end;

  TBaseItemsHolder = class(TScrollingWinControl)
  strict private
    FItems: TBaseItemsHolderCollection;
    FUpdating: Boolean;
    FOnCloseQuery: TCloseItemQueryEvent;

    FReadOnly: Boolean;

    function GetCount: Integer;
    function GetItem(AIndex: Integer): TBaseCustomItemsHolderItem;

    procedure DeleteMsg(var AMsg: TMessage); message WM_USER + 50;
    procedure DoResize(ASender: TObject);

    function GetStringIdsArray: Variant;

    procedure SetReadOnly(const AValue: Boolean);
  private
    procedure DeleteItem(AItem: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddItem(AItemClass: TBaseItemHolderClass): TBaseCustomItemsHolderItem;
    function GetItemByField(const AFieldName: string; AValue: Variant): TBaseCustomItemsHolderItem;

    procedure Clear;

    procedure AlignItems;
    function GetItemsArray(AArrayType: TResultArrayType; AFieldName: string): Variant;

    property OnCloseQuery: TCloseItemQueryEvent read FOnCloseQuery write FOnCloseQuery;
    property Items[AIndex: Integer]: TBaseCustomItemsHolderItem read GetItem; default;
  published
    property Count: Integer read GetCount;
    property CollectionItems: TBaseItemsHolderCollection read FItems;
    property UpdatingControl: Boolean read FUpdating write FUpdating;
    property StringIdsArray: Variant read GetStringIdsArray;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;

    property Align;
    property Color;
    property ParentColor;
  end;

  TBaseCustomItemsHolderItem = class(TPersistent)
  strict private
    FReadOnly: Boolean;
    FWrapper: TBaseItemWrapper;

    function GetWidth: Integer;
    function GetHeight: Integer;
    function GetLeft: Integer;
    function GetTop: Integer;

    procedure SetLeft(const AValue: Integer);
    procedure SetTop(const AValue: Integer);
  private
    property Width: Integer read GetWidth;
    property Height: Integer read GetHeight;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;

    property Wrapper: TBaseItemWrapper read FWrapper;
  strict protected
    FPanel: TPanel;
    FOnClick: TNotifyEvent;

    function GetCaption: string; virtual; abstract;
    procedure SetCaption(const AValue: string); virtual; abstract;

    function GetValue(AName: string): Variant; virtual; abstract;
    procedure SetValue(AName: string; AValue: Variant); virtual; abstract;

    function GetParent: TBaseItemsHolder;

    function GetFont: TFont; virtual; abstract;

    procedure SetReadOnly(const AValue: Boolean); virtual;

    procedure SetOnClick(const Value: TNotifyEvent); virtual; abstract;

    function GetColor: TColor; virtual; abstract;
    procedure SetColor(const Value: TColor); virtual; abstract;
  protected
    function GetMinWidth: Integer; virtual; abstract;
    procedure SetUnfocused; virtual; abstract;

    procedure DoAlign; virtual; abstract;
  public
    constructor Create(AWrapper: TBaseItemWrapper); virtual;
    destructor Destroy; override;

    property Values[AName: string]: Variant read GetValue write SetValue;
  published
    property Caption: string read GetCaption write SetCaption;
    property Font: TFont read GetFont;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Color: TColor read GetColor write SetColor;

    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
  end;

  TBaseEditItemsHolderItem = class(TBaseCustomItemsHolderItem)
  strict private
    FEdit: TcxComboBox;
  strict protected
    function GetCaption: string; override;
    procedure SetCaption(const AValue: string); override;

    function GetValue(AName: string): Variant; override;
    procedure SetValue(AName: string; AValue: Variant); override;

    procedure DoAlign; override;
  protected
    function GetMinWidth: Integer; override;
  public
    constructor Create(AWrapper: TBaseItemWrapper); override;
    destructor Destroy; override;
  published
    property Edit: TcxComboBox read FEdit;
  end;

  TBaseLabelItemsHolderItem = class(TBaseCustomItemsHolderItem)
  strict private
    FLabel: TcxLabel;
    FClearLabel: TLabel;

    FId: Integer;
    FStringId: string;

    procedure DoMouseEnter(ASender: TObject);
    procedure DoMouseLeave(ASender: TObject);
    procedure DoClearClick(ASender: TObject);

    function IsPointInsideControl(AControl: TControl; APoint: TPoint): Boolean;

    procedure DoInternalClick(ASender: TObject);
  private
    FOnClearClick: TNotifyEvent;
  strict protected
    function GetCaption: string; override;
    procedure SetCaption(const AValue: string); override;

    function GetValue(AName: string): Variant; override;
    procedure SetValue(AName: string; AValue: Variant); override;

    procedure DoAlign; override;
    procedure SetReadOnly(const AValue: Boolean); override;

    function GetFont: TFont; override;

    procedure SetOnClick(const AValue: TNotifyEvent); override;

    function GetColor: TColor; override;
    procedure SetColor(const Value: TColor); override;
  protected
    function GetMinWidth: Integer; override;
    procedure SetUnfocused; override;
  public
    constructor Create(AWrapper: TBaseItemWrapper); override;
    destructor Destroy; override;
  published
    property Id: Integer read FId write FId;
    property StringId: string read FStringId write FStringId;
  end;

  procedure Register;

implementation

uses SysUtils, Math, Variants, cxEdit,
{$IFDEF FASTSCRIPT_RTTI}
  fs_ievents, fs_iinterpreter,
{$ENDIF}
  SBaseConstants, SBaseVariantFunctions;

const
  Const_LeftMargin            = 2;
  Const_Horizontal_Interval   = 1;
  Const_Vertical_Interval     = 1;

{$IFDEF FASTSCRIPT_RTTI}
type
  TfsFunctions_TBaseItemHolder = class(TfsRTTIModule)
  strict private
    function Call_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

procedure Register;
begin
  RegisterComponents('MD Project Tools', [TBaseItemsHolder]);
end;

{$REGION 'TSBaseHolderItem'}

constructor TBaseLabelItemsHolderItem.Create(AWrapper: TBaseItemWrapper);
begin
  inherited;

  FOnClearClick := GetParent.DeleteItem;

  {$REGION 'Create cross label'}
  FClearLabel                    := TLabel.Create(FPanel);
  FClearLabel.Parent             := FPanel;

  FClearLabel.Caption            := 'x';
  FClearLabel.Font.Size          := 10;
  FClearLabel.Font.Style         := [fsBold];
  FClearLabel.Font.Color         := clRed;

  FClearLabel.Visible            := False;

  FClearLabel.OnMouseEnter       := DoMouseEnter;
  FClearLabel.OnMouseLeave       := DoMouseLeave;
  {$ENDREGION}

  {$REGION 'Create text label'}
  FLabel                          := TcxLabel.Create(FPanel);
  FLabel.Parent                   := FPanel;
  FLabel.Style.Font.Style         := [];
  FLabel.OnMouseEnter             := DoMouseEnter;
  FLabel.OnMouseLeave             := DoMouseLeave;
  FLabel.AutoSize                 := True;

  FLabel.OnClick                  := DoInternalClick;
  {$ENDREGION}

   FPanel.Height                   := Max(FLabel.Height, FClearLabel.Height);
   FPanel.OnMouseEnter             := DoMouseEnter;
   FPanel.OnMouseLeave             := DoMouseLeave;

   ReadOnly := GetParent.ReadOnly;
   DoAlign;
end;

destructor TBaseLabelItemsHolderItem.Destroy;
begin
  FreeAndNil(FLabel);
  FreeAndNil(FClearLabel);
  FreeAndNil(FPanel);

  inherited;
end;

procedure TBaseLabelItemsHolderItem.DoAlign;
var
  LWidth: Integer;
begin
  FClearLabel.Left := FLabel.Width;

  LWidth := FLabel.Width;

  if not ReadOnly then
    LWidth := LWidth + FClearLabel.Width;

  FPanel.Width := LWidth;
end;

procedure TBaseLabelItemsHolderItem.DoClearClick(ASender: TObject);
begin
  if Assigned(FOnClearClick) then
    FOnClearClick(Self);
end;

procedure TBaseLabelItemsHolderItem.DoInternalClick(ASender: TObject);
begin
  if Assigned(FOnClick) then
  begin
    FOnClick(Self);
    SetUnfocused;
  end;
end;

procedure TBaseLabelItemsHolderItem.DoMouseEnter(ASender: TObject);
var
  LFontStyles: TFontStyles;
begin
  if not ReadOnly then
    FClearLabel.Visible := True;

  LFontStyles := FLabel.Style.Font.Style;
  Include(LFontStyles, fsUnderline);
  FLabel.Style.Font.Style := LFontStyles;
end;

procedure TBaseLabelItemsHolderItem.DoMouseLeave(ASender: TObject);
var
  LPoint: TPoint;
  b1, b2: Boolean;
  LFontStyles: TFontStyles;
begin
  LPoint := Mouse.CursorPos;

  b1 := not IsPointInsideControl(FPanel, LPoint);
  b2 := not IsPointInsideControl(FPanel.Parent, LPoint);

  if b1 or b2
  then begin
    FClearLabel.Visible     := False;

    LFontStyles := FLabel.Style.Font.Style;
    Exclude(LFontStyles, fsUnderline);
    FLabel.Style.Font.Style := LFontStyles;
    FLabel.Invalidate;
  end;
end;

function TBaseLabelItemsHolderItem.GetCaption: string;
begin
  Result := FLabel.Caption;
end;

function TBaseLabelItemsHolderItem.GetColor: TColor;
begin
  Result := FLabel.Style.TextColor;
end;

function TBaseLabelItemsHolderItem.GetFont: TFont;
begin
  Result := FLabel.Style.Font;
end;

function TBaseLabelItemsHolderItem.GetMinWidth: Integer;
begin
  Result := Width;
end;

function TBaseLabelItemsHolderItem.GetValue(AName: string): Variant;
begin
  AName := UpperCase(AName);
  Result := Null;

  if AName = 'ID' then
    Result := Id
  else if AName = 'STRINGID' then
    Result := StringId;
end;

function TBaseLabelItemsHolderItem.IsPointInsideControl(AControl: TControl; APoint: TPoint): Boolean;
begin
  Result := PtInRect(AControl.ClientRect, AControl.ScreenToClient(APoint));
end;

procedure TBaseLabelItemsHolderItem.SetCaption(const AValue: string);
begin
  FLabel.Caption := AValue;
  DoAlign;
end;

procedure TBaseLabelItemsHolderItem.SetColor(const Value: TColor);
begin
  FLabel.Style.TextColor := Value;
end;

procedure TBaseLabelItemsHolderItem.SetOnClick(const AValue: TNotifyEvent);
begin
  FOnClick := AValue;
end;

procedure TBaseLabelItemsHolderItem.SetReadOnly(const AValue: Boolean);
begin
  inherited;

  if AValue then begin
    FClearLabel.Visible     := False;
    FClearLabel.OnClick     := nil;
  end
  else
    FClearLabel.OnClick     := DoClearClick;

  DoAlign;
end;

procedure TBaseLabelItemsHolderItem.SetUnfocused;
begin
  DoMouseLeave(Self);
end;

procedure TBaseLabelItemsHolderItem.SetValue(AName: string; AValue: Variant);
begin
  AName := UpperCase(AName);
  if AName = 'ID' then
    FId := AValue
  else if AName = 'STRINGID' then
    FStringId := AValue;
end;

{$ENDREGION}

{$REGION 'TSBaseItemHolder'}

function TBaseItemsHolder.AddItem(AItemClass: TBaseItemHolderClass): TBaseCustomItemsHolderItem;
begin
  FUpdating := True;
  Result := (FItems.Add as TBaseItemWrapper).CreateItem(AItemClass);
  FUpdating := False;
end;

procedure TBaseItemsHolder.AlignItems;
var
  LTop, LLeft, LWidth, LOldPosition: Integer;
  LItem: TCollectionItem;
  LLabel: TBaseCustomItemsHolderItem;
begin
  LLabel := nil;
  if FUpdating or (Count = 0) then
    Exit;

  FUpdating := True;

  LTop  := 0;
  LLeft := Const_LeftMargin;

  LOldPosition := VertScrollBar.Position;
  VertScrollBar.Position := 0;

  with ClientRect do
    LWidth := Right - Left;

  for LItem in FItems do
  begin
    LLabel := (LItem as TBaseItemWrapper).Item;
    LLabel.DoAlign;

    if ((LLeft + LLabel.GetMinWidth + Const_Horizontal_Interval) > LWidth)
      and (LLeft > Const_LeftMargin)
    then begin
      LLeft := Const_LeftMargin;
      LTop  := LTop + LLabel.Height + Const_Vertical_Interval;
    end;

    LLabel.Left := LLeft;
    LLabel.Top  := LTop;

    LLeft := LLeft + LLabel.Width + Const_Horizontal_Interval;
  end;

  if Assigned(LLabel) then
    LTop := LTop + LLabel.Height;

  VertScrollBar.Range     := LTop;
  VertScrollBar.Position  := LOldPosition;
  FUpdating := False;
end;

procedure TBaseItemsHolder.Clear;
begin
  FItems.Clear;
end;

constructor TBaseItemsHolder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FItems := TBaseItemsHolderCollection.Create(Self);

  FUpdating := False;
  OnResize := DoResize;

  HorzScrollBar.Visible := False;

  Clear;
end;

procedure TBaseItemsHolder.DeleteMsg(var AMsg: TMessage);
begin
  with TBaseCustomItemsHolderItem(AMsg.LParam) do
    Wrapper.Free;
end;

procedure TBaseItemsHolder.DeleteItem(AItem: TObject);
var
  LCanClose: Boolean;
begin
  if ReadOnly then
    Exit;

  LCanClose := True;

  if Assigned(FOnCloseQuery) then
    FOnCloseQuery(Self, LCanClose);

  if LCanClose then
    PostMessage(Handle, WM_USER + 50, 0, Integer(AItem))
  else
    (AItem as TBaseCustomItemsHolderItem).SetUnfocused;
end;

destructor TBaseItemsHolder.Destroy;
begin
  FItems.Free;

  inherited;
end;

procedure TBaseItemsHolder.DoResize(ASender: TObject);
begin
  if not FUpdating then
    AlignItems;
end;

function TBaseItemsHolder.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TBaseItemsHolder.GetItem(AIndex: Integer): TBaseCustomItemsHolderItem;
begin
  Result := (FItems.Items[AIndex] as TBaseItemWrapper).Item;
end;

function TBaseItemsHolder.GetItemByField(const AFieldName: string; AValue: Variant): TBaseCustomItemsHolderItem;
var
  LItem: TCollectionItem;
begin
  Result := nil;
  for LItem in FItems do
    if VarIsEqual(TBaseItemWrapper(LItem).Item.Values[AFieldName], AValue) then begin
      Result := TBaseItemWrapper(LItem).Item;
      Break;
    end;
end;

function TBaseItemsHolder.GetItemsArray(AArrayType: TResultArrayType; AFieldName: string): Variant;
var
  i: Integer;
  LResult: string;
begin
  if AArrayType = ratString then
  begin
    LResult := '';
    for i := 0 to Count - 1 do begin
      if LResult <> '' then
        LResult := LResult + ';';

      LResult := LResult + VarToStr(Items[i].Values[AFieldName]);
    end;
    Result := LResult;
  end
  else begin
    if Count = 0 then
      Result := Null
    else begin
      Result := VarArrayCreate([0, Count - 1], varVariant);

      for i := 0 to Count - 1 do
        Result[i] := Items[i].Values[AFieldName];
    end;
  end;
end;

function TBaseItemsHolder.GetStringIdsArray: Variant;
begin
  Result := GetItemsArray(ratVariant, 'StringId');
end;

procedure TBaseItemsHolder.SetReadOnly(const AValue: Boolean);
var
  LItem: TCollectionItem;
begin
  if ReadOnly = AValue then
    Exit;

  for LItem in FItems do
    (LItem as TBaseItemWrapper).Item.ReadOnly := AValue;

  FReadOnly := AValue;
  AlignItems;
end;

{$ENDREGION}

{$REGION 'TSBaseItemsHolderCollection'}

constructor TBaseItemsHolderCollection.Create(AParent: TBaseItemsHolder);
begin
  inherited Create(TBaseItemWrapper);
  FParent := AParent;
end;

procedure TBaseItemsHolderCollection.Update(AItem: TCollectionItem);
begin
  FParent.AlignItems;
end;

{$ENDREGION}

{$REGION 'TCustomBaseItemsHolderItem'}

constructor TBaseCustomItemsHolderItem.Create(AWrapper: TBaseItemWrapper);
var
  LParent: TBaseItemsHolder;
begin
  Assert(ClassName <> 'TBaseCustomItemsHolderItem', 'Abstract_Class.Create');

  FWrapper := AWrapper;
  LParent := GetParent;

  {$REGION 'Create panel'}
  FPanel                := TPanel.Create(LParent);
  FPanel.Parent         := LParent;

  FPanel.ParentColor    := True;
  FPanel.BevelOuter     := bvNone;
  {$ENDREGION}
end;

destructor TBaseCustomItemsHolderItem.Destroy;
begin
  FreeAndNil(FPanel);

  inherited;
end;

function TBaseCustomItemsHolderItem.GetHeight: Integer;
begin
  Result := FPanel.Height;
end;

function TBaseCustomItemsHolderItem.GetLeft: Integer;
begin
  Result := FPanel.Left;
end;

function TBaseCustomItemsHolderItem.GetParent: TBaseItemsHolder;
begin
  Result := (FWrapper.Collection as TBaseItemsHolderCollection).Parent;
end;

function TBaseCustomItemsHolderItem.GetTop: Integer;
begin
  Result := FPanel.Top;
end;

function TBaseCustomItemsHolderItem.GetWidth: Integer;
begin
  Result := FPanel.Width;
end;

procedure TBaseCustomItemsHolderItem.SetLeft(const AValue: Integer);
begin
  FPanel.Left := AValue;
end;

procedure TBaseCustomItemsHolderItem.SetReadOnly(const AValue: Boolean);
begin
  FReadOnly := AValue
end;

procedure TBaseCustomItemsHolderItem.SetTop(const AValue: Integer);
begin
  FPanel.Top := AValue;
end;

{$ENDREGION}

{$REGION 'TSBaseEditItemsHolderItem'}

constructor TBaseEditItemsHolderItem.Create(AWrapper: TBaseItemWrapper);
begin
  inherited;

  FEdit           := TcxComboBox.Create(FPanel);
  FEdit.Parent    := FPanel;
  FEdit.Align     := alNone;
  FEdit.Style.BorderStyle := ebsNone;

  FPanel.Height := FEdit.Height;

  DoAlign;
end;

destructor TBaseEditItemsHolderItem.Destroy;
begin
  FreeAndNil(FEdit);

  inherited;
end;

procedure TBaseEditItemsHolderItem.DoAlign;
begin
  FPanel.Width := FEdit.Width;
end;

function TBaseEditItemsHolderItem.GetCaption: string;
begin
  Result := FEdit.Text;
end;

function TBaseEditItemsHolderItem.GetMinWidth: Integer;
begin
  Result := 30;
end;

function TBaseEditItemsHolderItem.GetValue(AName: string): Variant;
begin
  Result := Null;
end;

procedure TBaseEditItemsHolderItem.SetCaption(const AValue: string);
begin
  FEdit.Text := AValue;
end;

procedure TBaseEditItemsHolderItem.SetValue(AName: string; AValue: Variant);
begin
end;

{$ENDREGION}

{$REGION 'TBaseItemWrapper' }

constructor TBaseItemWrapper.Create(Collection: TCollection);
begin
  inherited;

  FItemClass := TBaseLabelItemsHolderItem;
  CreateItem(FItemClass);
end;

function TBaseItemWrapper.CreateItem(AItemClass: TBaseItemHolderClass): TBaseCustomItemsHolderItem;
begin
  (Collection as TBaseItemsHolderCollection).Parent.UpdatingControl := True;
  try
    FreeOldObject;

    FItemClass := AItemClass;
    FItem := AItemClass.Create(Self);
    Result := FItem;
  finally
    (Collection as TBaseItemsHolderCollection).Parent.UpdatingControl := False;
  end;
end;

destructor TBaseItemWrapper.Destroy;
begin
  FreeOldObject;

  inherited;
end;

procedure TBaseItemWrapper.FreeOldObject;
begin
  FreeAndNil(FItem);
end;

function TBaseItemWrapper.GetDisplayName: string;
begin
  if Assigned(FItem) then
    Result := FItem.Caption
  else
    Result := '<nil>';
end;


{$ENDREGION}

{$REGION 'TfsFunctions_TBaseItemHolder'}
{$IFDEF FASTSCRIPT_RTTI}

function TfsFunctions_TBaseItemHolder.Call_Method(Instance: TObject; ClassType: TClass;
  const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if Instance is TBaseItemsHolder then
  begin
    if MethodName = 'CLEAR' then
      (Instance as TBaseItemsHolder).Clear
    else if MethodName = 'ALIGNITEMS' then
      (Instance as TBaseItemsHolder).AlignItems
    else if MethodName = 'ADDITEMBYCLASS' then
      Result := Integer((Instance as TBaseItemsHolder).
        AddItem(TBaseItemHolderClass(GetClass(Caller.Params[0]))))
    else if MethodName = 'GETITEMSARRAY' then
      Result := (Instance as TBaseItemsHolder).GetItemsArray
      (
        Caller.Params[0],
        Caller.Params[1]
      )
    else if MethodName = 'GETITEMBYFIELD' then
      Result := Integer((Instance as TBaseItemsHolder).GetItemByField
      (
        Caller.Params[0],
        Caller.Params[1]
      ))
  end

  else if Instance is TBaseCustomItemsHolderItem then
  begin
    if MethodName = 'VALUES.GET' then
      Result := (Instance as TBaseCustomItemsHolderItem).Values[Caller.Params[0]]
    else if MethodName = 'VALUES.SET' then
      (Instance as TBaseCustomItemsHolderItem).Values[Caller.Params[0]] := Caller.Params[1];
  end
end;

constructor TfsFunctions_TBaseItemHolder.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TBaseItemsHolder, TBaseItemsHolder.ClassParent.ClassName) do
  begin
    AddMethod('procedure Clear', Call_Method);
    AddMethod('procedure AlignItems', Call_Method);
    AddMethod('function AddItemByClass(AItemClass: String): TBaseCustomItemsHolderItem', Call_Method);
    AddMethod('function GetItemsArray(AArrayType: Integer; AFieldName: string): Variant', Call_Method);
    AddMethod('function GetItemByField(const AFieldName: string; AValue: Variant): TBaseCustomItemsHolderItem;', Call_Method);
  end;

  with AScript.AddClass(TBaseCustomItemsHolderItem, TBaseCustomItemsHolderItem.ClassParent.ClassName) do
  begin
    AddEvent('OnClick', TfsNotifyEvent);
    AddIndexProperty('Values', DelphiTypeString, DelphiTypeVariant, Call_Method);
  end;
end;

{$ENDIF}
{$ENDREGION}

initialization
  Classes.RegisterClasses([TBaseItemsHolder, TBaseItemsHolderCollection, TBaseLabelItemsHolderItem]);
  {$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_TBaseItemHolder);
  {$ENDIF}
finalization
  Classes.UnRegisterClasses([TBaseItemsHolder, TBaseItemsHolderCollection, TBaseLabelItemsHolderItem]);
  {$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TfsFunctions_TBaseItemHolder);
  {$ENDIF}
end.
