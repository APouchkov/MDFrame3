unit formImportTemplateTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SBaseXXViewForm, SBaseCxViewForm,
  cxGraphics, cxControls, cxLookAndFeels, NamedVariables, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, DB, cxDBData, SBaseDataModule, ExtCtrls, dxBar,
  cxClasses, cxSplitter, cxGridLevel, cxGridBandedTableView,
  cxGridDBBandedTableView, cxGridCustomView, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGrid, cxGroupBox, dxStatusBar, cxLabel,
  cxButtonEdit, cxNavigator, System.Actions, Vcl.ActnList, dxMDBarButton;

type
  TfrmImportTemplateTest = class(TSBaseCxViewFrm)
    cxGridInputParamsTableView: TcxGridTableView;
    cxGridInputParamsLevel: TcxGridLevel;
    cxGridInputParams: TcxGrid;
    lbInputParams: TcxLabel;
    cxGridInputParamsColumnName: TcxGridColumn;
    cxGridInputParamsColumnValue: TcxGridColumn;
    cxGridOutputParams: TcxGrid;
    cxGridTableViewOutputParams: TcxGridTableView;
    cxGridOutputParamsColumnName: TcxGridColumn;
    cxGridOutputParamsColumnValue: TcxGridColumn;
    cxGridLevelOutputParams: TcxGridLevel;
    lbOutputParams: TcxLabel;
    cxGridMessages: TcxGrid;
    cxGridTableViewMessages: TcxGridTableView;
    cxGridTableViewMessagesTime: TcxGridColumn;
    cxGridTableViewMessagesText: TcxGridColumn;
    cxGridLevelMessages: TcxGridLevel;
    lbMessages: TcxLabel;
    pnlInputParams: TPanel;
    pnlOutputParams: TPanel;
    pnlMessages: TPanel;

    procedure mnSubmitClick(Sender: TObject);
    procedure cxGridInputParamsColumnValuePropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
  protected
    { Protected declarations }
  private
    { Private declarations }
  public
    { Public declarations }
  published
    flDate: TSBaseViewFilterDate;
    flFileName: TSBaseViewFilterFile;
    procedure DataSetAfterClose(DataSet: TDataSet); override;
  end;


implementation

uses SBaseFileImport, SBaseCxUtils, SBaseConstants, SBaseFieldEditForm,
  SBaseVariantFunctions, SBaseStringFunctions;

{$R *.dfm}


procedure TfrmImportTemplateTest.cxGridInputParamsColumnValuePropertiesButtonClick(Sender: TObject; AButtonIndex: Integer);
var
  LResultValues: TNamedVariants;
begin
  LResultValues := TSBaseFieldEditFrm.Select
            (
              Self,
              [
                TNamedVariant.Create(SConst_Value, cxGridInputParamsColumnValue.EditValue)
              ]
            );
  if (LResultValues.Count > 0) then
    cxGridInputParamsColumnValue.EditValue := LResultValues[SConst_Value];
end;

procedure TfrmImportTemplateTest.DataSetAfterClose(DataSet: TDataSet);
begin
  if (csDestroying in ComponentState) then Exit;

  inherited;
  cxGridDBTableView.ClearItems;
end;

procedure TfrmImportTemplateTest.mnSubmitClick(Sender: TObject);
var
  I: Integer;
  LName: String;
  LInput, LOutput: TNamedVariants;
begin
  cxGridDBTableView.BeginUpdate;
  try
    cxGridDBTableView.ClearItems;
    DataSource.DataSet := nil;
    DataSetProvider.Close;

    with cxGridTableViewOutputParams.DataController do
      while RecordCount > 0 do DeleteRecord(0);

    LInput.UnknownAsNull := True;
    with cxGridInputParamsTableView.DataController do
      for I := 0 to FilteredRecordCount - 1 do begin
        LName := Trim(VarToStr(GetValue(I, cxGridInputParamsColumnName.Index)), ' ');
        if LName <> '' then
          LInput[LName] := GetValue(I, cxGridInputParamsColumnValue.Index);
      end;

    LInput[SConst_Id]               := CreateFormParams[SConst_Id];
    LInput[SConst_DataSet]          := Integer(DataSetProvider);
    LInput[SConst_Date]             := flDate.ParamValue;
    LInput[SConst_FileName]         := flFileName.ParamValue;
    LInput[SConst_OpenDialog]       := SConst_File;
    LInput[SConst_Output]           := Integer(@LOutput);
    LInput[SConst_FreeOnTerminate]  := True;
    LInput[SConst_Thread]           := True;

    try
      TSBaseFileImport.CreateByParams(Self, LInput).RunByParams;
    except
      DataSetProvider.Close;
      Raise;
    end;

    with cxGridTableViewOutputParams.DataController do
      while LOutput.Count > 0 do begin
        Append;
        with LOutput.Items[0] do begin
          cxGridOutputParamsColumnName.EditValue := Name;
          cxGridOutputParamsColumnValue.EditValue := Value;
        end;
        PostEditingData;
        LOutput.Delete(0);
      end;

    DataSource.DataSet := DataSetProvider;
    cxGridDBTableView.AutoCreateColumns;
  finally
    cxGridDBTableView.EndUpdate
  end;
end;

initialization
  RegisterClass(TfrmImportTemplateTest);

finalization
  UnRegisterClass(TfrmImportTemplateTest);

end.
