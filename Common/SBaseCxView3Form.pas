unit SBaseCxView3Form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DBConsts, SBaseCxView2Form, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxStyles, cxCustomData, cxFilter,
  cxGridLevel, cxGridBandedTableView, cxGridDBBandedTableView, NamedVariables,
  cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, cxGridDBDataDefinitions,
  cxData, cxDataStorage, DB, cxDBData, SBaseDataModule, ExtCtrls, dxBar, cxClasses,
  cxSplitter, cxGroupBox, dxStatusBar, cxNavigator, System.Actions, ActnList,
  LayoutPanel, dxMDBarButton, dxMDBar, cxMDGrid, dxMDStatusBar, dxSkinsCore,
  dxSkinsDefaultPainters, SBaseItemReactions;

type
  TSBaseCxView3Frm = class(TSBaseCxView2Frm)
    mnNew3: TdxBarButton;
    mnView3: TdxBarButton;
    mnEdit3: TdxBarButton;
    mnCopy3: TdxBarButton;
    mnRefreshRecord3: TdxBarButton;
    mnDelete3: TdxBarButton;
    mnMarkAsDeleted3: TdxBarButton;
    cxGridLevel3: TcxMDGridLevel;
    DataSetProvider3: TSQLDataSetProvider;
    DataSource3: TDataSource;

    procedure Grid3AfterUpdateActions(Sender: TObject);
    procedure CxGridAfterUpdateActions(Sender: TObject); override;

    procedure DataSetBeforeOpen(DataSet: TDataSet); override;
//    procedure DataSetAfterClose(DataSet: TDataSet); override;

    procedure mnFitToHeightClick(Sender: TObject); override;
    procedure cxGridDBView3FocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);

    procedure mnRefreshRecord3Click(Sender: TObject);

    procedure mnViewXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); override;
    procedure mnEditXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); override;
    procedure mnDeleteXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); override;
    procedure mnMarkAsDeletedXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); override;
    procedure mnRefreshRecordXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean); override;
    procedure cxGridLevel3ViewChanged(Sender: TObject);
    procedure cxGridLevel3AddView(AView: TcxCustomGridView);
  private
    { Private declarations }
    FRecordsPrivileges3: Word;
    FRecordsPrivileges3Stored: Boolean;
    FDeletedCondition3: String;

    procedure SetRecordsPrivileges3(const Value: Word);
  protected
    { Protected declarations }
    procedure InternalCreate; override;
    procedure InternalInit; override;

    procedure CloseDataSet; override;
  published
    { Çublished declarations }
    property RecordsPrivileges3: Word read FRecordsPrivileges3 write SetRecordsPrivileges3;
    property DeletedCondition3: String read FDeletedCondition3 write FDeletedCondition3;
  end;

implementation

uses SBaseFormDefinitions, SBaseForm, SBaseConstants, SBaseDxUtils, SBaseUtils,
  SBaseDataSourceControls;

{$R *.dfm}

{ TSBaseCxView3Frm }

procedure TSBaseCxView3Frm.InternalCreate;
var
  LLinkedComponent3: TComponent;
begin
  mnNew3.Caption    := SBaseProperties.Text_MenuNewSubElement + ' (2)';
  mnView3.Caption   := SBaseProperties.Text_MenuView;
  mnEdit3.Caption   := SBaseProperties.Text_MenuEdit;
  mnCopy3.Caption   := SBaseProperties.Text_MenuCopy;
  mnDelete3.Caption := SBaseProperties.Text_MenuDelete;

  mnMarkAsDeleted3.Caption    := SBaseProperties.Text_MenuMarkAsDeleted;
  mnMarkAsDeleted3.ImageIndex := SBaseFormProperties.IconIndex_MarkAsDeleted;
  mnRefreshRecord3.Caption    := SBaseProperties.Text_MenuRefreshRecord;

  inherited;

  if mnNew3.ImageIndex = -1 then
    mnNew3.ImageIndex := SBaseFormProperties.IconIndex_New;

  if not FRecordsPrivileges3Stored then
    FRecordsPrivileges3 := Self.FPrivileges and Privileges_ForRecord;

  LLinkedComponent3 := cxGridLevel3.GridView;

  with ItemsReactions.FindOrCreateReaction(mnRefreshRecord3) do begin
    LinkedComponent := LLinkedComponent3;
    OnClick.Reaction := biraDataSourceRefreshRecord;
  end;

  if (not FDeletedCondition3.IsEmpty) then begin
//    mnMarkAsDeleted2.Caption := SBaseProperties.Text_MenuMarkAsDeleted;
    ItemsReactions.InitReaction(mnMarkAsDeleted3, LLinkedComponent3, biraCustomDataSourceRecord);
  end;

//  FDataSourceControls.FindOrCreateGridControl(cxGridLevel3.GridView).AfterUpdate := Grid3AfterUpdateActions;
  cxGridLevelViewChanged(cxGridLevel3);

  if (DataSetProvider3.DataSetObject <> bdsoMemory) and (not Assigned(DataSetProvider3.ConnectionProvider)) then
    DataSetProvider3.ConnectionProvider := SQLConnectionProvider;

  TcxGridDBDataController(cxGridLevel3.GridView.DataController).DataSource := DataSource3;
end;

procedure TSBaseCxView3Frm.InternalInit;
begin
  RecordsPrivileges3 := RecordsPrivileges3;
  inherited;
end;

procedure TSBaseCxView3Frm.CloseDataSet;
begin
  if DataSetProvider3 <> nil then begin
    DataSetProvider3.Close;
    DataSetProvider3.UnPrepare;
  end;

  inherited;
end;

procedure TSBaseCxView3Frm.CxGridAfterUpdateActions(Sender: TObject);
begin
  if (cxGrid.FocusedView.PatternGridView = cxGridLevel3.GridView) then
    Grid3AfterUpdateActions(cxGrid.FocusedView)
  else
    inherited
end;

procedure TSBaseCxView3Frm.cxGridDBView3FocusedRecordChanged(Sender: TcxCustomGridTableView; APrevFocusedRecord, AFocusedRecord: TcxCustomGridRecord; ANewItemRecordFocusingChanged: Boolean);
begin
  if InternalGetControlsEnabled then begin
    ShowDetails;
  end;
end;

procedure TSBaseCxView3Frm.cxGridLevel3AddView(AView: TcxCustomGridView);
begin
  AView.PopupMenu := PopupMenu;
  with TcxGridTableView(AView) do begin
//    OnCanSelectRecord       := cxGridDBViewCanSelectRecord;
//    OnCellClick             := cxGridDBViewCellClick;
    OnFocusedRecordChanged  := cxGridDBView3FocusedRecordChanged;
    DataController.OnDataChanged := DBGridDataChanged;
  end;
end;

procedure TSBaseCxView3Frm.cxGridLevel3ViewChanged(Sender: TObject);
var
  LcxGridView: TcxCustomGridView;
begin
  LcxGridView := cxGridLevel3.GridView;
  if Assigned(LcxGridView) then
    FDataSourceControls.FindOrCreateGridControl(LcxGridView).AfterUpdate := GridAfterUpdateActions;
end;

{
procedure TSBaseCxView3Frm.DataSetAfterClose(DataSet: TDataSet);
begin
  inherited;

//  if not TcxGridDBDataController(cxGridLevel3.GridView.DataController).DataModeController.DetailInSQLMode then
  if DataSetProvider3.DataSetObject <> bdsoMemory then
    DataSetProvider3.Close;
end;
}

procedure TSBaseCxView3Frm.DataSetBeforeOpen(DataSet: TDataSet);
var
  I: Integer;
  LParam: TParam;
begin
  inherited;

//  if not TcxGridDBDataController(cxGridLevel3.GridView.DataController).DataModeController.DetailInSQLMode then begin

  if DataSetProvider3.DataSetObject <> bdsoMemory then begin
    for I := Pred(DataSetProvider3.Params.Count) downto 0 do with DataSetProvider3.Params[I] do begin
      LParam := DataSetProvider2.Params.FindParam(Name);
      if LParam <> nil then
        Value := LParam.Value
      else
        Clear
    end;
    DataSetProvider3.Open;
  end;
//  end;
end;

procedure TSBaseCxView3Frm.Grid3AfterUpdateActions(Sender: TObject);
var
  LFocusedRecord: TcxCustomGridRecord;
begin
  LFocusedRecord := TcxGridTableView(Sender).Controller.FocusedRecord;
  if FDeletedCondition3 <> '' then
    if (Sender = nil)
          or (LFocusedRecord = nil)
          or not (LFocusedRecord is TcxGridDataRow)
          or (not TcxGridDBDataController(TcxGridTableView(Sender).DataController).DataSet.EvaluateBoolean(FDeletedCondition3))
    then begin
      mnMarkAsDeleted3.Caption    := SBaseProperties.Text_MenuMarkAsDeleted;
      mnMarkAsDeleted3.ImageIndex := SBaseFormProperties.IconIndex_MarkAsDeleted;
      mnMarkAsDeleted3.Tag        := 1;
      mnMarkAsDeleted3.ShortCut   := ShortCut_Shift_Del;
    end else begin
      mnMarkAsDeleted3.Caption    := SBaseProperties.Text_MenuUnMarkFromDeleted;
      mnMarkAsDeleted3.ImageIndex := SBaseFormProperties.IconIndex_UnMarkFromDeleted;
      mnMarkAsDeleted3.Tag        := 0;
      mnMarkAsDeleted3.ShortCut   := 0;
    end;

  mnNewX.CheckEnabled;

  if (cxGridLevel3.GridView = cxGrid.FocusedView.PatternGridView) then begin
    mnViewX.Enabled             := mnView3.Enabled;
    mnEditX.Enabled             := mnEdit3.Enabled;
    mnCopyX.Enabled             := mnCopy3.Enabled;
    mnDeleteX.Enabled           := mnDelete3.Enabled;

    mnRefreshRecordX.Enabled    := mnRefreshRecord3.Enabled;

    mnMarkAsDeletedX.Enabled    := mnMarkAsDeleted3.Enabled;
    mnMarkAsDeletedX.Caption    := mnMarkAsDeleted3.Caption;
    mnMarkAsDeletedX.ImageIndex := mnMarkAsDeleted3.ImageIndex;
    mnMarkAsDeletedX.Tag        := mnMarkAsDeleted3.Tag;
    mnMarkAsDeletedX.ShortCut   := mnMarkAsDeleted3.ShortCut;
  end;
end;

procedure TSBaseCxView3Frm.mnDeleteXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  if cxGrid.FocusedView.PatternGridView = cxGridLevel3.GridView then
    mnDelete3.Click
  else
    inherited;
end;

procedure TSBaseCxView3Frm.mnEditXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  if cxGrid.FocusedView.PatternGridView = cxGridLevel3.GridView then
    mnEdit3.Click
  else
    inherited;
end;

procedure TSBaseCxView3Frm.mnFitToHeightClick(Sender: TObject);
begin
  inherited;

  TcxGridTableView(cxGridLevel3.GridView).OptionsView.CellAutoHeight := mnFitToHeight.Down;
end;

procedure TSBaseCxView3Frm.mnMarkAsDeletedXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  if cxGrid.FocusedView.PatternGridView = cxGridLevel3.GridView then
    mnMarkAsDeleted3.Click
  else
    inherited;
end;

procedure TSBaseCxView3Frm.mnRefreshRecord3Click(Sender: TObject);
begin
  if DataSetProvider3.RecordCount > 0 then
    DataSetProvider3.RefreshRecord
end;

procedure TSBaseCxView3Frm.mnRefreshRecordXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  if cxGrid.FocusedView.PatternGridView = cxGridLevel3.GridView then
    mnRefreshRecord3.Click
  else
    inherited;
end;

procedure TSBaseCxView3Frm.mnViewXClick(AReaction: TBaseItemReaction; var AVariables: TNamedVariants; var ADone: Boolean);
begin
  if cxGrid.FocusedView.PatternGridView = cxGridLevel3.GridView then
    mnView3.Click
  else
    inherited;
end;

procedure TSBaseCxView3Frm.SetRecordsPrivileges3(const Value: Word);
var
  LLinkedComponent: TComponent;
begin
  FRecordsPrivileges3 := Value;
  FRecordsPrivileges3Stored := True;

  if not Loading then begin
    LLinkedComponent := cxGridLevel3.GridView;
    if FRecordsPrivileges3 and Privilege_Select_Bit = 0 then begin
      ItemsReactions.SetActive(mnView3, False);
      mnView3.Enabled := False;
    end else begin
      with ItemsReactions.FindOrCreateReaction(mnView3) do begin
        LinkedComponent := LLinkedComponent;
        Default := (SelectMode = smNone);
        OnGetEnabled.Active := True;
        OnClick.Reaction  := biraCustomDataSourceRecord;
      end;
    end;

    if FRecordsPrivileges3 and Privilege_Insert_Bit = 0 then begin
      ItemsReactions.SetActive(mnNew3, False);
      mnNew3.Enabled := False;
    end else begin
      with ItemsReactions.FindOrCreateReaction(mnNew3, biraDataSourceInPlaceInsertChild) do begin
        LinkedComponent := cxGridLevel2.GridView;
        OnGetEnabled.Active := True;
        if OnClick.Reaction = biraNone then OnClick.Reaction := biraCustomDataSourceRecord;
        OnClick.Target := cxGridLevel3.GridView;
      end;
    end;

    if FRecordsPrivileges3 and Privilege_Update_Bit = 0 then begin
      ItemsReactions.SetActive(mnEdit3, False);
      mnEdit3.Enabled := False;
    end else begin
      with ItemsReactions.FindOrCreateReaction(mnEdit3, biraDataSourceInPlaceEdit) do begin
        LinkedComponent := LLinkedComponent;
        OnGetEnabled.Active := True;
        if OnClick.Reaction = biraNone then OnClick.Reaction := biraCustomDataSourceRecord;
      end;
    end;

    if FRecordsPrivileges3 and Privilege_Delete_Bit = 0 then begin
      ItemsReactions.SetActive(mnDelete3, False);
      mnDelete3.Enabled := False;
    end else begin
      with ItemsReactions.FindOrCreateReaction(mnDelete3, biraDataSourceInPlaceDelete) do begin
        LinkedComponent := LLinkedComponent;
        OnGetEnabled.Active := True;
        if OnClick.Confirmation.IsEmpty then
          OnClick.Confirmation := SDeleteRecordQuestion;
        if OnClick.Reaction = biraNone then OnClick.Reaction := biraCustomDataSourceRecord;
      end;
    end;

    ItemsReactions.DataSourceOnUpdate(InternalGetGrid, [dsusFocusedRecord]);
    ItemsReactions.DataSourceOnUpdate(cxGridLevel3.GridView, [dsusActive, dsusData, dsusFocusedRecord]);
  end;
end;

initialization
  RegisterClass(TSBaseCxView3Frm);

finalization
  UnRegisterClass(TSBaseCxView3Frm);

end.
