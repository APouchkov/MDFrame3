unit SBaseDataSetInspectorDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Graphics,
  Controls, Forms, Dialogs, SBaseCxViewForm, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, cxNavigator, Data.DB, cxDBData,
  SBaseDataModule, ExtCtrls, dxBar, cxClasses, cxSplitter, cxGridLevel,
  cxGridBandedTableView, cxGridDBBandedTableView, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, cxGroupBox,
  dxStatusBar, NamedVariables, System.Actions, ActnList, LayoutPanel,
  dxMDBarButton, cxMDGrid, dxSkinsCore, dxSkinsDefaultPainters, dxMDBar,
  dxMDStatusBar;

type
  TSBaseDataSetInspectorDlg = class(TSBaseCxViewFrm)
  private
    { Private declarations }
    [Weak] cxGridDBTableView: TcxMDGridDBTableView;
  protected
    procedure InternalCreate; override;
    procedure InternalInit; override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses SBaseCxUtils, SBaseVariantFunctions, SBaseConstants, SBaseForm, SBaseUtils;

{ TSBaseDataSetInspectorDlg }

procedure TSBaseDataSetInspectorDlg.InternalCreate;
begin
  cxGridDBTableView := TcxMDGridDBTableView.Create(Self);
  cxGridDBTableView.SetParentComponent(cxGridLevel);
  cxGridDBTableView.Configure := ScxMDGridViewDefaultConfigure;
  FreeAndNil(PopupMenu);

  inherited;

  IconIndex := SBaseFormProperties.IconIndex_View;

  RecordsPrivileges := 0;
end;

procedure TSBaseDataSetInspectorDlg.InternalInit;
var
  LDataSet: TDataSet;
begin
  cxGridDBTableView.BeginUpdate;
  try
    DataSource.DataSet := nil;
    cxGridDBTableView.ClearItems;
    DataSetProvider.Close;

    LDataSet := TDataSet(VarToPointer(CreateFormParams[SConst_DataSet]));
    Caption := 'Содержимое датасета ' + LDataSet.Name;
    DataSetProvider.FieldDefs.Assign(LDataSet.FieldDefs);
    DataSetProvider.Open;

    if LDataSet.Editing then begin
      DataSetProvider.Append;
        DataSetProvider.Fields.Load(LDataSet.Fields, lvcAlways);
      DataSetProvider.Post;
      Caption := Caption + ' (Показана только текущая редактируемая запись)'
    end else begin
      DataSetProvider.Load(LDataSet);
      if not DataSetProvider.IsEmpty then
        DataSetProvider.First;
    end;

    DataSource.DataSet := DataSetProvider;
    cxGridDBTableView.AutoCreateColumns;
  finally
    cxGridDBTableView.EndUpdate
  end;

  inherited;
end;

end.
