unit MainMDIForm;
{$I config.inc}

interface

uses Windows, Messages, SysUtils, Classes, Graphics, Forms, Controls,
  StdCtrls, ExtCtrls, ToolWin, Dialogs, IOUtils, DateUtils,
  Variants, hsDialogs, Math, SBaseFormDefinitions, SBaseForm, Clipbrd, SysConst, cxGraphics,
  cxControls, dxStatusBar, dxMDStatusBar, NamedVariables, Contnrs,
  SBaseEvents, SBaseTaskWindow, cxLookAndFeels, cxLookAndFeelPainters,
  dxBar, System.Actions, ActnList, cxClasses, dxSkinsCore,
  dxSkinsDefaultPainters, dxMDBar;

type
  TMainForm = class(TSBaseFrm)
    Timer: TTimer;
    StatusBar: TdxMDStatusBar;

    procedure FormShow(Sender: TObject); override;
//    procedure FormClose(Sender: TObject; var Action: TCloseAction); override;

    procedure mmCallBack(const AClassName: String; const AResultFields: String; const AResultValues: Variant);
    procedure TimerTimer(Sender: TObject);
    procedure MenuLoadFRReportClick(Sender: TObject);
    procedure MenuLoadXLReportClick(Sender: TObject);
  protected
    procedure LoadUserConfig(AStoreFormSettings: TFormItemsForSave); override;
    procedure SaveUserConfig(AStoreFormSettings: TFormItemsForSave); override;

    procedure InternalCreate; override;
    procedure InternalInit; override;
  public
    destructor Destroy; override;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses about, SBaseVariantFunctions, SBaseDataModule, SBaseConstants,
  DataModule, SBaseFileUtils, SBasePreviewFrxForm, SBaseReport,
  SBaseSelectForm, SBaseObjectInspectorDialog,
  DBCommon, DSIntf, SBaseUtils, SBaseNotificationMessage;

destructor TMainForm.Destroy;
var
  I: Integer;
begin
  for I := Pred(MDIChildCount) downto 0 do MDIChildren[I].Close;

  inherited;

  SBaseForm.SBaseMainFormMDIInterface := nil;
  MainForm := nil;
end;

procedure TMainForm.InternalCreate;
begin
  Caption := Application.Title;
  SBaseForm.SBaseMainFormMDIInterface := TSBaseMainFormMDIInterface.Create(Self);

  inherited;
  MainForm := Self;

  StatusBar.Panels[1].Width := StatusBar.Canvas.TextWidth(StatusBar.Panels[1].Text) + 10;
  StatusBar.Panels[2].Width := StatusBar.Canvas.TextWidth(StatusBar.Panels[2].Text) + 10;
end;

procedure TMainForm.InternalInit;
begin
  inherited;
  Timer.Enabled  := True;
end;

procedure TMainForm.LoadUserConfig(AStoreFormSettings: TFormItemsForSave);
var
  LSkinName: String;
begin
  inherited;
  if Visible then
    SBaseMainFormMDIInterface.RestoreWindows;

  LSkinName := VarToStr(GetSetting(SConst_Style));
  if (not LSkinName.IsEmpty) then
    SBaseMainFormMDIInterface.ActiveStyle := LSkinName;

  LSkinName := VarToStr(GetSetting(SConst_Skin));
  if (not LSkinName.IsEmpty) then
    SBaseMainFormMDIInterface.ActiveSkin := LSkinName;
end;

procedure TMainForm.SaveUserConfig(AStoreFormSettings: TFormItemsForSave);
begin
  MainDataModule.ClearUserClassVariables(TMainForm.ClassName, Null, Null);
  inherited;
  SBaseMainFormMDIInterface.SaveWindows;
  SetSetting(SConst_Style, SBaseMainFormMDIInterface.ActiveStyle);
  SetSetting(SConst_Skin, SBaseMainFormMDIInterface.ActiveSkin);
end;

{------------ System Procedures -------------}

procedure TMainForm.TimerTimer(Sender: TObject);
var
  LApplicationDate: TDateTime;
begin                    
  Timer.Enabled := False;
  try
(* NOW:
{1}
    CloseForgottenDependedDataSets;
*)
{2}
    LApplicationDate := TFile.GetLastWriteTime(ApplicationExeName);
    if (LApplicationDate <> 0) and (MainDataModule.ExceptedApplicationDate <> 0) and (SecondsBetween(MainDataModule.ExceptedApplicationDate, LApplicationDate) > 0) then begin
      case hsDialogs.HSMessageDlg
           (
            'Внимание! Обнаружена новая версия программы от ' + FormatDateTime('hh:mm dd.mm.yy', LApplicationDate) + '.' + SConst_CR + 'Текущая версия программы датирована ' + FormatDateTime('hh:mm dd.mm.yy', MainDataModule.ApplicationDate) + '.'#13'Для работы в новой версии необходимо перезапустить программу.' + SConst_CR + 'Завершить текущий сеанс работы?',
            mtConfirmation,
            mbYesNoCancel,
            0
           ) of
        mrYes: begin Close; Exit end;
        mrNo: MainDataModule.ExceptedApplicationDate := LApplicationDate;
      end;
    end
  finally
    Timer.Enabled := True;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  FNotificationTrap: TSBaseNotificationTrap;
//  L: TNamedVariant;
begin
  SBaseMainFormMDIInterface.RestoreWindows;

  FreeAndNil(MainDataModule.SplashForm);

  if SBaseNotificationProperties.Enabled then
    try
      FNotificationTrap := TSBaseNotificationTrap.Create(Self);
      FNotificationTrap.OnNotification := SBaseMainFormMDIInterface.NotificationMessage;
      FNotificationTrap.Enabled := True;
    except end;

  inherited;

  if SBaseNotificationProperties.Enabled then
    try SBaseEvents.NotificationInit; except end;

//  for L in TNamedVariants.Create([TNamedVariant.Create('TEST1', 1), TNamedVariant.Create('TEST2', 22)]) do
//    ShowMessage(L.Name + ' = ' + VarToStr(L.Value))
end;

{-------------- Menu Action --------------}

procedure TMainForm.mmCallBack(const AClassName: String; const AResultFields: String; const AResultValues: Variant);
begin
  SBaseMainFormMDIInterface.Log(lgInfo, VarToStr(AResultValues));
end;

procedure TMainForm.MenuLoadFRReportClick(Sender: TObject);
var
  Ext: String;
begin
  with TFileOpenDialog.Create(Self) do try
    with FileTypes.Add do begin
      DisplayName := 'Отчёты и шаблоны (*.fp3,*.fr3)';
      FileMask := '*.fp3;*.fr3';
    end;
    with FileTypes.Add do begin
      DisplayName := 'Файл отчёта (*.fp3)';
      FileMask := '*.fp3';
    end;
    with FileTypes.Add do begin
      DisplayName := 'Файл шаблона (*.fr3)';
      FileMask := '*.fr3';
    end;

    if Execute then begin
      Ext := SBaseFileUtils.ExtractFileExt(FileName);

      if ext = SFileExt_Fp3 then
        with TSBasePreviewFrxFrm.Create(Self) do try
          ShowSavedReport(FileName, frsMDIChild)
        except on E:Exception do begin
          SBaseMainFormMDIInterface.Log(lgError, E.Message);
          Free
        end end
      else if ext = SFileExt_Fr3 then
        TSBasePreviewFrxFrm.DesignReport(Self, FileName)
    end
  finally
    Free
  end;
end;

procedure TMainForm.MenuLoadXLReportClick(Sender: TObject);
begin
//  TxlCXPreviewDialog.Create(Self).FormStyle := fsMDIChild
end;

initialization
  RegisterClass(TMainForm);

end.
