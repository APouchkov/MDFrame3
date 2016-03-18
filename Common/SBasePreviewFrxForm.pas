unit SBasePreviewFrxForm;
{$I config.inc}

interface

uses
  Classes, Forms, frxClass, frxDBSet, frxPreview, SysUtils, DB
{IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter
{ENDIF}
  , frxDesgn, frxRich, frxChBox, frxGZip, frxDCtrl, SBaseReportFR3
  , frxExportImage, frxExportXLS, frxExportXML, frxExportMail, frxExportPDF, frxExportRTF, frxExportTXT, frxExportHTML;

type
  TFRShowStyle = (frsNormal, frsModal, frsMDIChild, frsPrint);

  TSBasePreviewFrxFrm = class(TfrxPreviewForm)
    procedure ShowReport(Style: TFRShowStyle);
    procedure ShowCompositeReport(Style: TFRShowStyle);
    procedure ShowSavedReport(AReport: TFileName; Style: TFRShowStyle);
    procedure SaveBClick(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    DefaultSaveName: String;
  public
    { Public declarations }
    frxReport     : TfrxReport;

    class procedure DesignReport(AOwner: TComponent; const AFileName: String);
    constructor Create(AOwner: TComponent); reintroduce; overload;
    constructor Create(AOwner: TComponent; ATemplate: TFileName; ADataSets: Array of TDataSet; ADefaultSaveName: String = ''); reintroduce; overload;

    function  AppendReport(ATemplate: TFileName; ADataSets: Array of TDataSet): boolean;
  end;

implementation

uses SBaseForm, DataModule, SBaseConstants;

{ Полноценный конструктор }
constructor TSBasePreviewFrxFrm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Position      := poDefaultPosOnly;
  FreeOnClose   := True;

//  if ADataModule = nil then
//    ADataModule := AOwner;

  try
    frxReport := TfrxReport.Create(self);
    with frxReport do begin
      IniFile := SRegistryPath_Software + '\' + MainDataModule.ApplicationName + '\' + SConst_FastReport;
      with PreviewOptions do begin
        Maximized := False;
        MDIChild  := False;
        Modal     := False;
      end;

      with Script do begin
        AutoDeclareClasses        := True;

        Parent                    := fsGlobalUnit;
        Parent.AutoDeclareClasses := True;

//        if AOwner <> nil then
//          DataModule              := ADataModule;
      end;
    end;

    OnShow      := FormShow;
    OnActivate  := FormActivate;
    OnDestroy   := FormDestroy;

    SBaseFormProperties.Icon16List.GetIcon(SBaseFormProperties.IconIndex_FastReport, Icon);
  except on E:Exception do begin
    SBaseMainFormMDIInterface.Log(lgError, E.Message);
    Abort
  end end;
end;

{ Вспомогательный конструктор создающий набор FrDataSet-ов и открывающий темплейт }
constructor TSBasePreviewFrxFrm.Create(AOwner: TComponent; ATemplate: TFileName; ADataSets: Array of TDataSet; ADefaultSaveName: String = '');
var
  I: Integer;
  LfrxDB: TfrxDBDataset;
begin
  Self.DefaultSaveName := ADefaultSaveName;
  Create(AOwner{, ADataModule});

  if (not DefaultSaveName.IsEmpty) then
    SaveB.OnClick := SaveBClick;

  try
    for I := Pred(Length(ADataSets)) downto 0 do begin
      LfrxDB := TfrxDBDataset.Create(Self);
      with LfrxDB do begin
        DataSet := ADataSets[i];
        Name := SBaseReportFR3.SPatternDataSetPrefix + IntToStr(Succ(I));
      end;
      frxReport.EnabledDataSets.Add(LfrxDB);
    end;

    with frxReport.Script do begin
      AutoDeclareClasses        := True;

      Parent                    := fsGlobalUnit;
      Parent.AutoDeclareClasses := True;

//      if AOwner <> nil then
//        DataModule              := AOwner;
    end;

    frxReport.LoadFromFile(ATemplate, True);
  except on E:Exception do begin
    SBaseMainFormMDIInterface.Log(lgError, E.Message);
    Abort
  end end;
end;

class procedure TSBasePreviewFrxFrm.DesignReport(AOwner: TComponent; const AFileName: String);
const
  Default_DataSetCount = 20;
var
  I: Integer;
  LName: String;
  LfrxDB: TfrxDBDataset;
  frxReport: TfrxReport;
begin
  frxReport := TfrxReport.Create(AOwner);
  with frxReport do begin
    EngineOptions.UseGlobalDataSetList := False;
//    DataModule := AOwner;
  end;

  for I := Pred(Default_DataSetCount) downto 0 do begin
    LName := SBaseReportFR3.SPatternDataSetPrefix + IntToStr(Succ(I));
    LfrxDB := TfrxDBDataset(AOwner.FindComponent(LName));
    if LfrxDB = nil then begin
      LfrxDB := TfrxDBDataset.Create(AOwner);
      LfrxDB.Name := LName;
    end;
    frxReport.EnabledDataSets.Add(LfrxDB);
  end;

  frxReport.LoadFromFile(AFileName, True);
  frxReport.DesignReport(False);
end;

{ Загрузить сохраненный отчет }
procedure TSBasePreviewFrxFrm.ShowSavedReport(AReport: TFileName; Style: TFRShowStyle);
begin
  Preview.Report       := frxReport;
  Preview.PreviewPages := frxReport.PreviewPages;
  Preview.LoadFromFile(AReport);

  if Style = frsPrint then
    frxReport.Print
  else begin
    Init;

    case Style of
      frsNormal   : Show;
      frsModal    : begin frxReport.PreviewOptions.Modal := True; Show end;
      frsMDIChild : begin frxReport.PreviewOptions.MDIChild := True; FormStyle := fsMDIChild end;
    end;
  end;
end;

{ Показать отчет }
procedure TSBasePreviewFrxFrm.ShowReport(Style: TFRShowStyle);
begin
  if frxReport.PrepareReport(True, True) then begin
    Preview.Report       := frxReport;
    Preview.PreviewPages := frxReport.PreviewPages;
    Init;
    case Style of
      frsNormal:   Show;
      frsModal:    begin frxReport.PreviewOptions.Modal := True; ShowModal end;
      frsMDIChild: begin frxReport.PreviewOptions.MDIChild := True; FormStyle := fsMDIChild end;
    end;
  end else
    Raise Exception.Create('Ошибка: Отчёт не построен!');
end;

{ Добавить отчет }
function TSBasePreviewFrxFrm.AppendReport(ATemplate: TFileName; ADataSets: Array of TDataSet): boolean;
var
  i, Offset: Integer;
  LfrxDB: TfrxDBDataset;
begin
  Result := False;
  try
    Offset := frxReport.DataSets.Count;
    for i := Pred(Length(ADataSets)) downto 0 do begin
      LfrxDB := TfrxDBDataset.Create(Self);
      with LfrxDB do begin
        DataSet := ADataSets[i];
        Name := SBaseReportFR3.SPatternDataSetPrefix + IntToStr( I + Offset + 1 );
      end;
      frxReport.EnabledDataSets.Add(LfrxDB);
    end;

    frxReport.LoadFromFile(ATemplate, True);
    Result := frxReport.PrepareReport(false);

  except on E:Exception do begin
    SBaseMainFormMDIInterface.Log(lgError, E.Message);
    Abort
  end end;

end;

{ Показать композитный отчет }
procedure TSBasePreviewFrxFrm.ShowCompositeReport(Style: TFRShowStyle);
begin
    Preview.Report       := frxReport;
    Preview.PreviewPages := frxReport.PreviewPages;
    Init;
    case Style of
      frsNormal:   Show;
      frsModal:    begin frxReport.PreviewOptions.Modal := True; ShowModal end;
      frsMDIChild: begin frxReport.PreviewOptions.MDIChild := True; FormStyle := fsMDIChild end;
    end;
end;

procedure TSBasePreviewFrxFrm.FormShow(Sender: TObject);
begin
// TO DO: FR3 Password Protects
//  if MainDataModule.ExportsEnabled then
  frxReport.PreviewOptions.Buttons := frxReport.PreviewOptions.Buttons + [pbExport];

  Caption := ChangeFileExt(ExtractFileName(frxReport.FileName),'');
  if frxReport.ReportOptions.Name <> '' then Caption := Caption + SVar_CaptionPathDelimeter + frxReport.ReportOptions.Name;

  if not (fsModal in FormState) then
    SBaseForm.SBaseMainFormMDIInterface.RegisterWindow(self)
end;

procedure TSBasePreviewFrxFrm.FormActivate(Sender: TObject);
begin
  if not (fsModal in FormState) then
    SBaseForm.SBaseMainFormMDIInterface.WindowOnActivate(self, [])
end;

procedure TSBasePreviewFrxFrm.FormDestroy(Sender: TObject);
begin
  if not (fsModal in FormState) then
    SBaseForm.SBaseMainFormMDIInterface.WindowOnDestroy(self)
end;

procedure TSBasePreviewFrxFrm.SaveBClick(Sender: TObject);
begin
  Preview.SaveToFileDefault(DefaultSaveName);
end;

initialization
  RegisterClasses
  (
    [
      TfrxXLSExport, TfrxXMLExport, TfrxMailExport, TfrxPDFExport, TfrxHTMLExport, TfrxRTFExport,
      TfrxBMPExport, TfrxJPEGExport, TfrxGIFExport, TfrxPNGExport, TfrxTXTExport
    ]
  );

finalization
  UnRegisterClasses
  (
    [
      TfrxXLSExport, TfrxXMLExport, TfrxMailExport, TfrxPDFExport, TfrxHTMLExport, TfrxRTFExport,
      TfrxBMPExport, TfrxJPEGExport, TfrxGIFExport, TfrxPNGExport, TfrxTXTExport
    ]
  );

end.
