unit SBaseReportListForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Graphics,
  Controls, Forms, Dialogs, SBaseReportForm, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxStyles,
  cxCustomData, cxFilter, cxData, cxDataStorage, Data.DB, cxDBData,
  cxImageComboBox, System.Actions, ActnList, dxBar, cxTextEdit,
  cxMaskEdit, cxDropDownEdit, cxImageLookupComboBox, LayoutPanel, cxButtons,
  StdCtrls, cxGridLevel, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxClasses, cxGridCustomView, cxGrid, ExtCtrls,
  cxGroupBox, SBaseReportFilter, cxNavigator, Menus, SBaseReport;

type
  TSBaseReportListFrm = class(TSBaseReportFrm)
    gbList: TcxLayoutGroupBox;
    rfList: TSBaseReportFilter;
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure PrepareParams(Sender: TSBaseReport); override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses SBaseConstants;

{ TSBaseReportListFrm }

procedure TSBaseReportListFrm.PrepareParams(Sender: TSBaseReport);
begin
  inherited;
  Sender.ReportParams[SConst_List] := rfList.ParamValue;
end;

initialization
  RegisterClass(TSBaseReportListFrm);

finalization
  UnRegisterClass(TSBaseReportListFrm);

end.
