program BackOffice;

uses
  FastMM4,
  Themes,
  Styles,
  Forms,
  SBaseFormDefinitions in 'Common\SBaseFormDefinitions.pas',
  SBaseUtils in 'Common\SBaseUtils.pas',
  SBaseFileUtils in 'Common\SBaseFileUtils.pas',
  DataModule in 'DataModule.pas' {MainDataModule: TDataModule},
  about in 'about.pas' {AboutBox},
  SBaseConstants in 'Common\SBaseConstants.pas',
  SBaseMathFunctions in 'Common\SBaseMathFunctions.pas',
  SBaseStringFunctions in 'Common\SBaseStringFunctions.pas',
  SBaseVariantFunctions in 'Common\SBaseVariantFunctions.pas',
  SBaseDataModule in 'Common\SBaseDataModule.pas',
  SBaseExpression in 'Common\SBaseExpression.pas',
  SBaseCxUtils in 'Common\SBaseCxUtils.pas',
  fsFunction_TcxControls in 'Common\fsFunction_TcxControls.pas',
  fsFunction_TcxGrid in 'Common\fsFunction_TcxGrid.pas',
  fsFunction_TDataSet in 'Common\fsFunction_TDataSet.pas',
  fsFunction_TdxControls in 'Common\fsFunction_TdxControls.pas',
  fsFunction_TWinCtrl in 'Common\fsFunction_TWinCtrl.pas',
  fsFunctions_Ru in 'Common\fsFunctions_Ru.pas',
  fsFunction_Other in 'Common\fsFunction_Other.pas',
  SBaseSplashForm in 'Common\SBaseSplashForm.pas' {SBaseSplashFrm},
  MainMDIForm in 'MainMDIForm.pas' {MainForm},
  SBaseNotificationMessage in 'Notifications\SBaseNotificationMessage.pas' {NotificationMessageEditor},
  SBaseEvents in 'Notifications\SBaseEvents.pas',
  SBaseForm in 'Common\SBaseForm.pas' {SBaseFrm},
  SBaseEditFormDh in 'Common\SBaseEditFormDh.pas' {SBaseEditFrmDh},
  SBaseXXViewForm in 'Common\SBaseXXViewForm.pas' {SBaseXXViewFrm},
  SBaseCxCustomTreeForm in 'Common\SBaseCxCustomTreeForm.pas' {SBaseCxCustomTreeFrm},
  SBaseProgress in 'Common\SBaseProgress.pas' {SBaseProgressFrm},
  SBasePreviewFrxForm in 'Common\SBasePreviewFrxForm.pas',
  SBaseCxViewForm in 'Common\SBaseCxViewForm.pas' {SBaseCxViewFrm},
  SBaseCxTree2Form in 'Common\SBaseCxTree2Form.pas',
  SBaseCxTreeForm in 'Common\SBaseCxTreeForm.pas' {SBaseCxTreeFrm},
  SBaseCxTreeViewForm in 'Common\SBaseCxTreeViewForm.pas' {SBaseCxTreeViewFrm},
  SBaseCxView2Form in 'Common\SBaseCxView2Form.pas' {SBaseCxView2Frm},
  SBaseCxView3Form in 'Common\SBaseCxView3Form.pas' {SBaseCxView3Frm},
  SBaseSelectForm in 'Common\SBaseSelectForm.pas' {SBaseSelectFrm},
  SBaseSelectDatesPeriodForm in 'Common\SBaseSelectDatesPeriodForm.pas' {SBaseSelectDatesPeriodFrm},
  DataHandler in 'Common\DataHandler.pas',
  DataHandlerGUI in 'Common\DataHandlerGUI.pas',
  DataHandlerSupport in 'Common\DataHandlerSupport.pas',
  SBaseReport in 'Common\SBaseReport.pas',
  SBaseReportDBF in 'Common\SBaseReportDBF.pas',
  SBaseReportExcel in 'Common\SBaseReportExcel.pas',
  SBaseReportFR3 in 'Common\SBaseReportFR3.pas',
  SBaseReportInterface in 'Common\SBaseReportInterface.pas',
  SBaseReportTXT in 'Common\SBaseReportTXT.pas',
  SBaseReportWord in 'Common\SBaseReportWord.pas',
  SBaseReportXML in 'Common\SBaseReportXML.pas',
  SBaseReportForm in 'Common\SBaseReportForm.pas' {SBaseReportFrm},
  fsFunction_TDataHandler in 'Common\fsFunction_TDataHandler.pas',
  SBaseSelectDateTimeForm in 'Common\SBaseSelectDateTimeForm.pas' {SBaseSelectDateFrm},
  SBaseDxUtils in 'Common\SBaseDxUtils.pas',
  DBPeriodicTabControl in 'Common\DBPeriodicTabControl.pas',
  SBaseImport in 'Common\SBaseImport.pas',
  SBaseFieldEditForm in 'Common\SBaseFieldEditForm.pas' {SBaseFieldEditFrm},
  SBaseTxtFileImport in 'Common\SBaseTxtFileImport.pas',
  SBaseXlsFileImport in 'Common\SBaseXlsFileImport.pas',
  SBaseXmlFileImport in 'Common\SBaseXmlFileImport.pas',
  SBaseCalendar in 'Common\SBaseCalendar.pas',
  SBasePeriodicUtils in 'Common\SBasePeriodicUtils.pas',
  SBaseDbfFileImport in 'Common\SBaseDbfFileImport.pas',
  fsFunctions_FileIO in 'Common\fsFunctions_FileIO.pas',
  SBaseTaskWindow in 'Common\SBaseTaskWindow.pas' {SBaseTaskWnd},
  DateTimeOffset in 'Common\DateTimeOffset.pas',
  SBaseItemHolder in 'Common\SBaseItemHolder.pas',
  SBaseNotificationWindow in 'Notifications\SBaseNotificationWindow.pas',
  BaseAccountPanel in 'Packages\BaseAccountPanel\BaseAccountPanel.pas',
  BackOfficeAccountPanel in 'Packages\BackOfficeAccountPanel\BackOfficeAccountPanel.pas',
  SBaseDfmView in 'Common\SBaseDfmView.pas',
  SBaseObjectInspectorDialog in 'Common\SBaseObjectInspectorDialog.pas' {SBaseObjectInspectorDlg},
  SBaseScriptDebuggerForm in 'Common\SBaseScriptDebuggerForm.pas' {SBaseScriptDebuggerFrm},
  SBaseDataServiceBroker in 'Common\SBaseDataServiceBroker.pas',
  PropFilerEh in 'Packages\EhLib\PropFilerEh.pas',
  EhLibVCL in 'Packages\EhLib\EhLibVCL.pas',
  SBaseDataHelperInternal in 'Common\SBaseDataHelperInternal.pas',
  SBaseDataHelperUniSDAC in 'Common\SBaseDataHelperUniSDAC.pas',
  SBaseReportFilter in 'Common\SBaseReportFilter.pas',
  SBaseReportListForm in 'Common\SBaseReportListForm.pas' {SBaseReportListFrm},
  SBaseReportTabControl in 'Common\SBaseReportTabControl.pas',
  SBaseUniDACImport in 'Common\SBaseUniDACImport.pas',
  SBaseDataSetInspectorDialog in 'Common\SBaseDataSetInspectorDialog.pas' {SBaseDataSetInspectorDlg},
  SBaseControls in 'Common\SBaseControls.pas',
  SBaseDataChanges in 'Common\SBaseDataChanges.pas',
  SBaseDataSourceControls in 'Common\SBaseDataSourceControls.pas',
  SBaseItemReactions in 'Common\SBaseItemReactions.pas',
  SBaseDialogForm in 'Common\SBaseDialogForm.pas' {SBaseDialogFrm},
  SBaseLinkImport in 'Common\SBaseLinkImport.pas',
  SBaseDataSetImport in 'Common\SBaseDataSetImport.pas',
  NamedEditRepository in 'Common\NamedEditRepository.pas',
  SBaseXmlFilesImport in 'Common\SBaseXmlFilesImport.pas',
  SBaseTxtFilesImport in 'Common\SBaseTxtFilesImport.pas',
  SBasePreviewTxtForm in 'Common\SBasePreviewTxtForm.pas' {SBasePreviewTxtFrm},
  SBaseCxTreeViewImportForm in 'Common\SBaseCxTreeViewImportForm.pas' {SBaseCxTreeViewImportFrm},
  NamedVariables in 'Packages\NamedVariables\NamedVariables.pas',
  RxVerInf in 'Packages\RX\Units\RxVerInf.pas',
  FileUtil in 'Packages\RX\Units\FileUtil.pas',
  WordReport in 'Packages\WordReport\WordReport.pas',
  MDSynCompletionProposal in 'Packages\MDSynEdit\MDSynCompletionProposal.pas',
  dxMDStatusBar in 'Packages\dxMDStatusBar\dxMDStatusBar.pas',
  SBaseWizardForm in 'Common\SBaseWizardForm.pas' {SBaseWizardFrm},
  dxMDWizardControl in 'Packages\dxMDWizardControl\dxMDWizardControl.pas',
  MDSynSearch in 'Packages\MDSynEdit\MDSynSearch.pas',
  cxMDDBTreeList in 'Packages\cxMDDBTreeList\cxMDDBTreeList.pas',
  cxMDGrid in 'Packages\cxMDGrid\cxMDGrid.pas',
  cxLayoutDataGroupBox in 'Packages\cxLayoutDataGroupBox\cxLayoutDataGroupBox.pas',
  dxMDBar in 'Packages\dxMDBar\dxMDBar.pas',
  cxMDVGrid in 'Packages\cxMDVGrid\cxMDVGrid.pas',
  SBaseXml in 'Common\SBaseXml.pas',
  SBaseCustomEditForm in 'Common\SBaseCustomEditForm.pas' {SBaseCustomEditFrm},
  SBaseEditForm in 'Common\SBaseEditForm.pas' {SBaseEditFrm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainDataModule, MainDataModule);
  Application.Run;

end.
