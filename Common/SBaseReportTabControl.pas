unit SBaseReportTabControl;
{$I config.inc}

interface

uses
  Types, UITypes, SysUtils, Classes, cxGridLevel, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGridDBBandedTableView, cxGrid, cxPC, cxLookAndFeels, Controls,
  DB, cxRichEdit, cxEdit, ExtCtrls, Graphics, StdCtrls, StrUtils, Contnrs,
  cxGroupBox, cxGraphics, Variants, Windows, fs_iinterpreter;

type
  TCustomReportOwnedCollection = class(TOwnedCollection)
  protected
    procedure Update(Item: TCollectionItem); override;
  end;

  TSBaseReportTabControlItem = class(TCollectionItem)
  private
    FGridView : TcxGridTableView;
    FCaption  : String;
    FFilter   : String;
    procedure SetCaption(const Value: String);
    procedure SetGridView(const Value: TcxGridTableView);
  published
    property GridView: TcxGridTableView read FGridView write SetGridView;
    property Caption: String read FCaption write SetCaption;
    property Filter: String read FFilter write FFilter;
  end;

  TSBaseReportTabControlItems = class(TCustomReportOwnedCollection)
  private
    function GetItem(Index: Integer): TSBaseReportTabControlItem;
  public
    property Items[Index: Integer]: TSBaseReportTabControlItem read GetItem; default;
  end;

  TSBaseReportTabControl = class(TcxCustomTabControl)
  const
    cContentTextBegin = '{\rtf1 {\colortbl;\red0\green0\blue255;\red255\green255\blue255;\red0\green0\blue0;\red255\green0\blue0;}';
    sContentTextResultBeginExt = '\par\par';
    cContentTextResultBeginMain = '\trowd\trgaph50'
      + '\clbrdrl\brdrw1\brdrcf2\clbrdrt\brdrw1\brdrcf2\clbrdrr\brdrw1\brdrcf2\clbrdrb\brdrw1\brdrcf2\cellx3500'
      + '\clbrdrl\brdrw1\brdrcf2\clbrdrt\brdrw1\brdrcf2\clbrdrr\brdrw1\brdrcf2\clbrdrb\brdrw1\brdrcf2\cellx4500'
      + '\intbl\ql\b %s \b0\cell \qr\b %d\b0\cell\row';
    cContentTextResultTemplate = '\intbl\ql\cf1   {\field{\*\fldinst{HYPERLINK "Index=%d" }}{\fldrslt{%s:}}}\cell \cf0\qr\b %d \b0\cell\row';
    cContentTextEnd = '}';
  private
    FActive: Boolean;
    FCaption: String;
    FInfo: String;
    FItems: TSBaseReportTabControlItems;
    FGrid: TcxGrid;
    FPanelContent: TcxGroupBox;
    FPanelContentCaption: TPanel;
    FLabelContentCaption: TLabel;
    FPanelContentInfo: TPanel;
    FLabelContentInfo: TLabel;
    FBevelContent: TBevel;
    FRichEdit: TcxRichEdit;
    FContentText: String;
    FContentTextResult: String;
    FFooter: String;

    procedure SetCaption(const Value: String);
    procedure SetInfo(const Value: String);
    function GetCaption: String;
    function GetInfo: String;
    procedure CreateGUIObjects;
    function GetGrid: TcxGrid;
    procedure SetGrid(const Value: TcxGrid);
    procedure SetActive(const Value: Boolean);
    procedure OnURLClick(ASender: TcxCustomRichEdit; const AUrlText: string; AButton: TMouseButton);
//    procedure SetConentText(const Value: String);
//    procedure SetTabControl(const Value: TcxCustomReportTabControl);
    procedure SetPanelContent(const Value: TcxGroupBox);
    procedure SetItems(const Value: TSBaseReportTabControlItems);
    procedure SetProgress(const Value: boolean);
    procedure RefreshContentInfo;
    procedure SetFooter(Value: string);
    function IsCaptionStored: Boolean;
    function IsInfoStored: Boolean;
  protected
    procedure Change; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

//    property TabControl: TcxCustomReportTabControl read FTabControl write SetTabControl;
    property PanelContent: TcxGroupBox read FPanelContent write SetPanelContent;

    procedure ContentRefresh;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure UpdateTabs;
    procedure AddNewLine(const Value: string; AStyle: TFontStyles = []; AWriteLN: Boolean = True);
    procedure AddNewLN;
  published
    property Active: Boolean read FActive write SetActive default False;
    property Items: TSBaseReportTabControlItems read FItems write SetItems;
    property Grid: TcxGrid read GetGrid write SetGrid;
    property Caption: String read GetCaption write SetCaption stored IsCaptionStored;
    property Info: String read GetInfo write SetInfo stored IsInfoStored;
    property Footer: String read FFooter write SetFooter;
//    property ContentText: String read FContentText write SetConentText;
{$IFNDEF PACKAGE}
//    property NewLine: string write SetNewLine;
{$ENDIF}
  published
{ -=MD=- }
    property Padding;
    property AutoSize default False;

    property Align;
    property Anchors;
//    property BiDiMode;
    property Color;
    property Constraints;
//    property DockSite;
//    property DragCursor;
//    property DragKind;
//    property DragMode;
//    property Enabled;
    property Focusable;
    property Font;
    property ParentBackground;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
//    property PopupMenu;
//    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;

//    property Properties: TcxTabControlProperties read GetProperties write SetProperties;
//    property HideTabs;
//    property HotTrack;
    property ImageBorder;
    property Images;
    property LookAndFeel;
//    property MultiLine;
//    property MultiSelect;
    property NavigatorPosition;
    property Options;
    property OwnerDraw;
//    property RaggedRight;
//    property Rotate;
//    property MaxRotatedTabWidth;
//    property ScrollOpposite;
    property ShowFrame;
    property Style;
    property TabHeight;
    property TabIndex;
    property TabPosition;
//    property Tabs;
    property TabSlants;
    property TabWidth;
  end;

  TfsFunctions_ReportTabControl = class(TfsRTTIModule)
  private
    function Call_TSBaseReportTabControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

procedure Register;

implementation

uses SBaseConstants
{$IFNDEF PACKAGE}
  , SBaseFileUtils
{$ENDIF}
  , SBaseDataModule;

resourcestring
  SErrorGrid      = 'Не указано значение свойства Grid!';
  SErrorGridView  = 'Не указано значение свойства GridView!';
  SFirstTab       = 'Главная';
  SCaption        = 'Сверка';
  SInfo           = 'Информация';


procedure Register;
begin
  RegisterComponents('MD Project Tools', [TSBaseReportTabControl]);
end;

{ TSBaseReportTabControl }

procedure TSBaseReportTabControl.BeginUpdate;
begin
  SetProgress(True);
end;

procedure TSBaseReportTabControl.Change;
var
  LReportItem: TSBaseReportTabControlItem;
  LDataSource: TDataSource;
  LDataSet: TDataSet;
begin
  inherited;

  if TabIndex < 0 then
    Exit
  else if TabIndex = 0 then
    PanelContent.BringToFront
  else begin
    LReportItem := Items[TabIndex - 1];
    if LReportItem.GridView <> nil then begin
      if Grid.ActiveLevel.GridView <> LReportItem.GridView then
        Grid.ActiveLevel.GridView := LReportItem.GridView;

      if (LReportItem.GridView is TcxGridDBTableView) then
        LDataSource := TcxGridDBTableView(LReportItem.GridView).DataController.DataSource
      else if (LReportItem.GridView is TcxGridDBBandedTableView) then
        LDataSource := TcxGridDBBandedTableView(LReportItem.GridView).DataController.DataSource
      else
        LDataSource := nil;

      if LDataSource <> nil then begin
        LDataSet := LDataSource.DataSet.Source;

        if (LDataSet <> nil) and LDataSet.Active and (LReportItem.Filter <> '') then
          LDataSet.Filter := LReportItem.Filter;
      end;
    end else
      Raise EAbstractError.Create(SErrorGridView);

    Grid.BringToFront;
  end;
end;

procedure TSBaseReportTabControl.ContentRefresh;
begin
  if FActive then begin
    if not FContentText.IsEmpty then
      FContentTextResult := sContentTextResultBeginExt + FContentTextResult;

    FRichEdit.Text := cContentTextBegin + FContentText + FContentTextResult + cContentTextEnd;
  end;
end;

constructor TSBaseReportTabControl.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FItems := TSBaseReportTabControlItems.Create(Self, TSBaseReportTabControlItem);
  FActive := False;

  FCaption  := SCaption;
  FInfo     := SInfo;

  FFooter   := 'Обнаружено расхождений:';

  CreateGUIObjects;

  if Assigned(AOwner) then
    for I := 0 to Pred(AOwner.ComponentCount) do
      if AOwner.Components[I] is TcxGrid then begin
        Grid := TcxGrid(AOwner.Components[i]);
        Break;
      end;
end;

procedure TSBaseReportTabControl.CreateGUIObjects;
begin
//  FTabControl := TcxCustomReportTabControl.Create(Self);
//  with FTabControl do begin
    Focusable               := False;
    Font.Style              := [];
    HotTrack                := True;
    LookAndFeel.Kind        := lfUltraFlat;
    LookAndFeel.NativeStyle := False;
    NavigatorPosition       := npRightBottom;
    ParentFont              := False;
    Style                   := 9;
    TabIndex                := 0;
    TabPosition             := tpBottom;
    TabSlants.Positions     := [spLeft, spRight];
//  end;

  FPanelContent := TcxGroupBox.Create(Self);
  with FPanelContent do begin
    Parent            := Self;
    Align             := alClient;
    ParentBackground  := False;
    ParentColor       := False;
    PanelStyle.Active := True;
    Style.BorderStyle := ebsFlat;
    Style.Color       := clWindow;
  end;

  FPanelContentCaption := TPanel.Create(Self);
  with FPanelContentCaption do begin
    Parent           := FPanelContent;
    Align            := alTop;
    BevelOuter       := bvNone;
    Color            := clWindow;
    Height           := 35;
    ParentBackground := False;
    Padding.Left     := 5;
    Padding.Bottom   := 5;
  end;

  FLabelContentCaption := TLabel.Create(Self);
  with FLabelContentCaption do begin
    Parent      := FPanelContentCaption;
    Align       := alClient;
    AutoSize    := False;
    Caption     := FCaption;
    Font.Height := -17;
    Font.Style  := [fsBold];
    Layout      := tlBottom;
  end;

  FPanelContentInfo := TPanel.Create(Self);
  with FPanelContentInfo do begin
    Parent           := FPanelContent;
    Align            := alTop;
    BevelOuter       := bvNone;
    Color            := 11770523;
    Height           := 23;
    ParentBackground := False;
    Padding.Left     := 5;
    Top              := FPanelContentCaption.Top + 1;
  end;

  FLabelContentInfo := TLabel.Create(Self);
  with FLabelContentInfo do begin
    Parent      := FPanelContentInfo;
    Align       := alClient;
    AutoSize    := False;
    Caption     := FInfo;
    Font.Color  := clWhite;
    Font.Height := -11;
    Font.Style  := [fsBold];
    Layout      := tlCenter;
  end;

  FBevelContent := TBevel.Create(Self);
  with FBevelContent do begin
    Parent := FPanelContent;
    Align  := alTop;
    Height := 5;
    Shape  := bsBottomLine;
    Top    := FPanelContentInfo.Top + 1;
  end;

  FRichEdit := TcxRichEdit.Create(Self);
  with FRichEdit do begin
    Align                    := alClient;
    AlignWithMargins         := True;
    Margins.Left             := 5;
    Margins.Top              := 5;
    Margins.Right            := 5;
    Margins.Bottom           := 5;
    Parent                   := FPanelContent;
    ParentColor              := False;
    ParentFont               := False;
    Properties.ReadOnly      := True;
    Properties.AutoUrlDetect := True;
//    Properties.AllowObjects  := True;
    Style.BorderStyle        := ebsNone;
    Style.HotTrack           := False;
    Style.TransparentBorder  := False;
//    Properties.StreamModes   := [resmPlainRtf];
  end;
  FRichEdit.Properties.OnURLClick := OnURLClick;
end;

destructor TSBaseReportTabControl.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TSBaseReportTabControl.EndUpdate;
begin
  SetProgress(False);
end;

procedure TSBaseReportTabControl.SetActive(const Value: Boolean);
begin
  if FActive = Value then Exit;

  FActive := Value;

  //-- Скрываем/отображаем
  if Assigned(FGrid) then
    if FActive then begin
      FGrid.Parent  := Self;
      FGrid.Align   := alClient;
    end else if not FActive then begin
      FGrid.Parent  := nil;
    end;

  RefreshContentInfo;
end;


procedure TSBaseReportTabControl.UpdateTabs;
{$IFNDEF PACKAGE}
var
  I: Integer;
  LDataSource: TDataSource;
  LDataSet: TDataSet;

  LOldFiltered: Boolean;
  LOldFilter: String;

  LGridView: TcxGridTableView;
  LDataSetList: TObjectList;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  //-- Если не указан Parent - выходим (Active = False)
  //if not Assigned(FTabControl.Parent) then Exit;

  LDataSource := nil;
  LDataSetList := TObjectList.Create(False);
  try
    for I := 0 to Pred(Items.Count) do begin
      LGridView := Items[I].GridView;
      if (LGridView is TcxGridDBTableView) then
        LDataSource := TcxGridDBTableView(LGridView).DataController.DataSource
      else if (LGridView is TcxGridDBBandedTableView) then
        LDataSource := TcxGridDBBandedTableView(LGridView).DataController.DataSource
      else
        Continue;

      if LDataSource = nil then Continue;
      LDataSet := LDataSource.DataSet.Source;
      if LDataSet = nil then Continue;

      if LDataSetList.IndexOf(LDataSet) = -1 then begin
        LDataSet.DisableControls;
        LDataSetList.Add(LDataSet);
      end;

      LOldFiltered := LDataSet.Filtered;
      LOldFilter   := LDataSet.Filter;

      if Items[I].Filter <> '' then begin
        LDataSet.Filter := Items[I].Filter;
        if not LDataSet.Filtered then
          LDataSet.Filtered := True;
      end else
        LDataSet.Filtered := False;

      Tabs[I + 1].Visible := (LDataSet.RecordCount > 0);
      Tabs[I + 1].Caption := Items[I].Caption + ' (' + IntToStr(LDataSet.RecordCount) + ')';

      if not LOldFiltered then
        LDataSet.Filtered := False;

      LDataSet.Filter := LOldFilter;

      if LOldFiltered and not LDataSet.Filtered then
        LDataSet.Filtered := True;
    end;
  finally
    for I := 0 to LDataSetList.Count - 1 do
      TDataSet(LDataSetList.Items[I]).EnableControls;
    LDataSetList.Free;
  end;
{$ENDIF}
end;


procedure TSBaseReportTabControl.RefreshContentInfo;
var
  I: Integer;
  LGridView: TcxGridTableView;
{$IFNDEF PACKAGE}
  LDataSource: TDataSource;
  LDataSet: TDataSet;
  LOldFilter: String;
  LRecordCount: Integer;
  LShowResult: Boolean;
  LDataSetList: TObjectList;
{$ENDIF}
  LTotalRecordCount: Integer;
begin
  //-- Если не указан Parent - выходим (Active = False)
  //if not Assigned(FTabControl.Parent) then Exit;
  FContentTextResult := '';

{$IFNDEF PACKAGE}
  LShowResult := False;
  LRecordCount := 0;
{$ENDIF}
  LTotalRecordCount := 0;

  Tabs.Clear;
  Tabs.Add(SFirstTab);

{$IFNDEF PACKAGE}
  LDataSetList := TObjectList.Create(False);
{$ENDIF}

  try
    for I := 0 to Pred(Items.Count) do begin
      LGridView := Items[I].GridView;
      Tabs.Add(Items[I].Caption);

      //-- Если не указан грид, скрываем закладку, переходим к следующей  итерации
      if LGridView = nil then begin
        Tabs[I + 1].Visible := False;
        Continue;
      end;

{$IFNDEF PACKAGE}
      if (LGridView is TcxGridDBTableView) then
        LDataSource := TcxGridDBTableView(LGridView).DataController.DataSource
      else if (LGridView is TcxGridDBBandedTableView) then
        LDataSource := TcxGridDBBandedTableView(LGridView).DataController.DataSource
      else
        LDataSource := nil;

      if LDataSource <> nil then
        LDataSet := LDataSource.DataSet.Source
      else
        LDataSet := nil;

      //-- Если не указан датасет или он неактивен, скрываем закладку, переходим к следующей  итерации
      if (LDataSet = nil) or (LDataSet <> nil) and (LDataSet.Active = False) then begin
        Tabs[I + 1].Visible := False;
        Continue;
      end;

      if LDataSetList.IndexOf(LDataSet) = -1 then begin
        LDataSet.DisableControls;
        LDataSetList.Add(LDataSet);
      end;

      //-- Формируем результат
      LShowResult := True;
      LOldFilter := LDataSet.Filter;
      try
        if Items[I].Filter <> '' then begin
          LDataSet.Filter := Items[I].Filter;
          if not LDataSet.Filtered then
            LDataSet.Filtered := True;
        end;

        if LDataSet.RecordCount = 0 then begin
          Tabs[I + 1].Visible := False;
          Continue;
        end else begin
          LRecordCount := LDataSet.RecordCount;
          LTotalRecordCount := LTotalRecordCount + LRecordCount;
        end;
      finally
        if LOldFilter <> LDataSet.Filter then
          LDataSet.Filter := LOldFilter;
      end;
      FContentTextResult := FContentTextResult + Format(cContentTextResultTemplate, [I, Items[I].Caption, LRecordCount]);
      Tabs[I + 1].Caption := Tabs[I + 1].Caption + ' (' + IntToStr(LRecordCount) + ')';
{$ENDIF}
    end;
  finally
{$IFNDEF PACKAGE}
    for I := 0 to LDataSetList.Count - 1 do
      TDataSet(LDataSetList.Items[I]).EnableControls;
    LDataSetList.Free;
{$ENDIF}
  end;

{$IFNDEF PACKAGE}
  if LShowResult then
{$ENDIF}
    FContentTextResult := Format(cContentTextResultBeginMain, [FFooter, LTotalRecordCount]) + FContentTextResult;

  ContentRefresh;

  TabIndex := 0;
  FPanelContent.BringToFront;
end;

procedure TSBaseReportTabControl.SetCaption(const Value: String);
begin
  if FCaption <> Value then
    FCaption := Value;

  if Assigned(FLabelContentCaption) then
    FLabelContentCaption.Caption := FCaption;
end;

procedure TSBaseReportTabControl.SetFooter(Value: string);
begin
  FFooter := Value;
end;

procedure TSBaseReportTabControl.SetProgress(const Value: boolean);
begin
  if Value then begin
    FRichEdit.Clear;
    FContentText := '';
  end else begin
    ContentRefresh;
    RefreshContentInfo;
  end;
end;

procedure TSBaseReportTabControl.AddNewLine(const Value: string; AStyle: TFontStyles; AWriteLN: Boolean);
var
  LStringStream: TStringStream;
  LSelStart, LSelLength: Integer;
const
  rtfPrefix = '{\rtf1\ansi\ansicpg1251\deff0';
  rftSuffix = '}';
begin
  LStringStream := TStringStream.Create(rtfPrefix + IfThen(AWriteLN, '\par', '') + Value + rftSuffix);

  with FRichEdit do begin
    LSelStart  := SelStart;
    LSelLength := SelLength;
    try
      Properties.StreamModes := Properties.StreamModes + [resmSelection];
      try
        SelStart  := LSelStart;
        SelLength := LSelLength;
        Lines.LoadFromStream(LStringStream);
        ModifiedAfterEnter := True;
      finally
        Properties.StreamModes := Properties.StreamModes - [resmSelection];
      end;
    finally
      LStringStream.Free;
    end;
  end;
  FContentText := FContentText + '{\par}' + Value;
end;

procedure TSBaseReportTabControl.AddNewLN;
begin
  AddNewLine('', [], True);
//  with FRichEdit do begin
//    Lines.Add('{\par}');
//  end;
//  FContentText := FContentText + '{\par}';
end;

function TSBaseReportTabControl.GetGrid: TcxGrid;
begin
  Result := FGrid;
end;

procedure TSBaseReportTabControl.SetGrid(const Value: TcxGrid);
begin
  if Value <> FGrid then begin
    if Active then
      Active := False;

    FGrid := Value;
  end;
end;

function TSBaseReportTabControl.GetCaption: String;
begin
  Result := FCaption;
end;

procedure TSBaseReportTabControl.SetInfo(const Value: String);
begin
  if FInfo <> Value then
    FInfo := Value;

  if Assigned(FLabelContentInfo) then
    FLabelContentInfo.Caption := FInfo;
end;

procedure TSBaseReportTabControl.SetItems(const Value: TSBaseReportTabControlItems);
begin
  if FItems <> Value then Exit;

  FItems.Assign(Value);
end;

procedure TSBaseReportTabControl.SetPanelContent(const Value: TCxGroupBox);
begin
  if FPanelContent <> Value then Exit;

  FPanelContent.Assign(Value);
end;

function TSBaseReportTabControl.GetInfo: String;
begin
  Result := FInfo;
end;

function TSBaseReportTabControl.IsCaptionStored: Boolean;
begin
  Result := not SameText(FCaption, SCaption);
end;

function TSBaseReportTabControl.IsInfoStored: Boolean;
begin
  Result := not SameText(FInfo, SInfo);
end;

procedure TSBaseReportTabControl.OnURLClick(ASender: TcxCustomRichEdit; const AUrlText: string; AButton: TMouseButton);
const
  SIndex = 'Index=';
var
  LIndex: Integer;
begin
  if Pos(SIndex, AUrlText) > 0 then begin
    LIndex := StrToInt(Copy(AUrlText, Length(SIndex) + 1, Length(AUrlText) - Length(SIndex)));
    TabIndex := LIndex + 1;
  end
{$IFNDEF PACKAGE}
  else
    OpenFileWithShell(AUrlText);
{$ENDIF}
end;

{ TSBaseReportTabControlItems }

function TSBaseReportTabControlItems.GetItem(Index: Integer): TSBaseReportTabControlItem;
begin
  Result := TSBaseReportTabControlItem(inherited GetItem(Index));
end;

{ TCustomReportOwnedCollection }

procedure TCustomReportOwnedCollection.Update(Item: TCollectionItem);
begin
  inherited;

  if TSBaseReportTabControl(Owner).Active then
    TSBaseReportTabControl(Owner).Active := False;
end;

{ TSBaseReportTabControlItem }

procedure TSBaseReportTabControlItem.SetCaption(const Value: String);
begin
  FCaption := Value;
  if TSBaseReportTabControl(Collection.Owner).Active then
    TSBaseReportTabControl(Collection.Owner).Active := False;
end;

procedure TSBaseReportTabControlItem.SetGridView(const Value: TcxGridTableView);
begin
  if FGridView = Value then Exit;

  FGridView := Value;

  if FCaption = '' then
    FCaption := FGridView.Name;

  if TSBaseReportTabControl(Collection.Owner).Active then
    TSBaseReportTabControl(Collection.Owner).Active := False;
end;

{ TfsFunctions_ReportTabControl }

constructor TfsFunctions_ReportTabControl.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseReportTabControl, TSBaseReportTabControl.ClassParent.ClassName) do begin
    AddMethod('procedure AddNewLine(ALine: String)', Call_TSBaseReportTabControl_Method);
    AddMethod('procedure BeginUpdate', Call_TSBaseReportTabControl_Method);
    AddMethod('procedure EndUpdate', Call_TSBaseReportTabControl_Method);
    AddMethod('procedure UpdateTabs', Call_TSBaseReportTabControl_Method);
  end;
end;

function TfsFunctions_ReportTabControl.Call_TSBaseReportTabControl_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'ADDNEWLINE' then
    TSBaseReportTabControl(Instance).AddNewLine(Caller.Params[0])
  else if MethodName = 'BEGINUPDATE' then
    TSBaseReportTabControl(Instance).BeginUpdate
  else if MethodName = 'ENDUPDATE' then
    TSBaseReportTabControl(Instance).EndUpdate
  else if MethodName = 'UPDATETABS' then
    TSBaseReportTabControl(Instance).UpdateTabs;
end;

initialization
  RegisterClasses
  ([
    TSBaseReportTabControl,
    TSBaseReportTabControlItem
  ]);

  fsRTTIModules.Add(TfsFunctions_ReportTabControl);

finalization
  UnRegisterClasses
  ([
    TSBaseReportTabControl,
    TSBaseReportTabControlItem
  ]);
  fsRTTIModules.Remove(TfsFunctions_ReportTabControl);

end.
