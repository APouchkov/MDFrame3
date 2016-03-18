{******************************************************************************}
{                       -= Модуль печати документов =-                         }
{                                                                              }
{                  -= Шаблоны FastReport - 3 (TfrxReport) =-                   }
{                                                                              }
{                           v4.1.1 (С) 2007 - 2014                             }
{                                                                              }
{                                                                              }
{******************************************************************************}
unit SBaseReportFR3;
{$I config.inc}

interface

uses
  SBaseReportInterface, NamedVariables, fs_iinterpreter, frxCustomDBRTTI;

{$IFDEF PATTERN_EXTERNAL}
procedure RegisterFactoryPattern(Report: IReport; var FactoryPattern: IFactoryPattern);
{$ENDIF}

const
  SPatternDataSetPrefix = 'frxDB';

implementation

uses
  Classes, DB, SysUtils, Variants,
  {$IFNDEF PATTERN_EXTERNAL}
  SBaseReport,
  {$ENDIF}
  {$IFNDEF REPORT_EXTERNAL}
  Controls, Forms, frxPreview, SBaseForm,
  {$ENDIF}
  frxClass, frxDBSet, frxPrintDialog, frxVariables, frxGZip, frxDCtrl,
  frxDMPExport, frxGradient, frxChBox, frxCross, frxRich, frxOLE, frxBarcode,
  DataModule, SBaseFileUtils, StrUtils, SBaseVariantFunctions,
  SBaseStringFunctions;

type
  TPatternFR3 = class(TCustomPattern, IPatternMerge)
  private
    FReport: TfrxReport;
  protected
    class function GetCode: String; override;
    function ResultFileExt: String; override;
    function GetDataSetPrefix: String; override;
    procedure IncludeSubReport(const ComponentName, FileName: String);
  public
    constructor Create(Report: IReport); override;
    destructor Destroy; override;
    procedure PrepareReport(var APatternStorage: TPatternStorage; const ADataSets: Array of TDataSet; const AVariables: TNamedVariants); override;
    function SaveReport(const FileName: String): String; override;
    procedure PrintReport(Printer: String); override;
{$IFNDEF REPORT_EXTERNAL}
    procedure ShowReport(Owner: TForm; Caption: String; ShowModal: Boolean); override;
    procedure DesignReport(Owner: TForm); override;
{$ENDIF}
  end;

  TFactoryPatterns = class(TCustomFactoryPatterns)
  protected
    function GetPatternClass(Index: Integer): TCustomPatternClass; override;
  end;

{$IFNDEF REPORT_EXTERNAL}
  TPreviewFormFR3 = class(TfrxPreviewForm)
  private
    FDefaultSaveName: String;
  protected
    procedure FormShow(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveBClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure ShowMDI;
  end;
{$ENDIF}

const
  SPatternCode = 'FR3';
  SPatternResultFileExt = 'fp3';

resourcestring
  // SParam_MergeReport = 'MergeReport';
  SParam_SubReport = 'SubReport';
  SError_SubReportComponentNotFound = 'Вложенный отчет ''%s'' не найден';

{$IFDEF PATTERN_EXTERNAL}
procedure RegisterFactoryPatterns(Report: IReport; var FactoryPattern: IFactoryPatterns);
begin
  FactoryPattern := TFactoryPatterns.Create as IFactoryPatterns;
end;
{$ENDIF}

{ TPatternFR3 }
constructor TPatternFR3.Create(Report: IReport);
//var
//  AOwner: TPersistent;
begin
  inherited;
  FReport := TfrxReport.Create(nil);
//  AOwner := Report.GetOwner;
  FReport.PreviewOptions.Maximized := False;
  FReport.PreviewOptions.Buttons := FReport.PreviewOptions.Buttons - [pbExportQuick];
  FReport.PreviewOptions.ZoomMode := zmPageWidth;
  with FReport.Script do
  begin            
    AutoDeclareClasses := True;
    Parent := fsGlobalUnit;
    Parent.AutoDeclareClasses := True;
//    if (AOwner <> nil) and (AOwner is TComponent) then
//      DataModule := TComponent(AOwner);
  end;
  with FReport do
  begin
    with EngineOptions do
    begin
      UseGlobalDataSetList := False;
      EnableThreadSafe := True;
      DestroyForms := True;
      SilentMode := True;
    end;
  end;
end;

destructor TPatternFR3.Destroy;
var
  i: Integer;
begin
  for i := Pred(FReport.DataSets.Count) downto 0 do
  begin
    RemoveChildObject(FReport.DataSets.Items[i].DataSet);
    FReport.DataSets.Items[i].DataSet.Free;
  end;
  FReport.DataSets.Clear;
  if Assigned(FReport) and not Assigned(FReport.Owner) then FreeAndNil(FReport);
  inherited;
end;

class function TPatternFR3.GetCode: String;
begin
  Result := SPatternCode;
end;

function TPatternFR3.ResultFileExt: String;
begin
  Result := SPatternResultFileExt;
end;

function TPatternFR3.GetDataSetPrefix: String;
begin
  Result := SPatternDataSetPrefix;
end;

procedure TPatternFR3.IncludeSubReport(const ComponentName, FileName: String);

  procedure PreparePage(APage: TfrxPage);
  var
    i: Integer;
    AMasterData: TfrxMasterData;
    AMasterBandExists: Boolean;
  begin
    AMasterBandExists := False;
    for i := Pred(APage.AllObjects.Count) downto 0 do
    begin
      if TObject(APage.AllObjects[i]) is TfrxView then
        TfrxView(APage.AllObjects[i]).DataSet := nil
      else
      if TObject(APage.AllObjects[i]) is TfrxNullBand then
        TObject(APage.AllObjects[i]).Free
      else
      if TObject(APage.AllObjects[i]) is TfrxDataBand then
      begin
        TfrxDataBand(APage.AllObjects[i]).DataSet := nil;
        TfrxDataBand(APage.AllObjects[i]).RowCount := 1;
      end;
      AMasterBandExists := AMasterBandExists or (TObject(APage.AllObjects[i]) is TfrxMasterData);
    end;
    if not AMasterBandExists then
    begin
      AMasterData := TfrxMasterData.Create(APage);
      with AMasterData do
      begin
        Parent := APage;
        Height := APage.Height;
        DataSet := nil;
        RowCount := 1;
      end;
      for i := Pred(APage.AllObjects.Count) downto 0 do
        if TObject(APage.AllObjects[i]) is TfrxView then
          TfrxView(APage.AllObjects[i]).Parent := AMasterData;
    end;
  end;

var
  i, ACount: Integer;
  AReport: TfrxReport;
  AParent: TfrxComponent;
  ASubReport, ASubReportNew: TfrxSubReport;
begin
  ASubReport := FReport.FindObject(ComponentName) as TfrxSubreport;
  if not Assigned(ASubReport) then
    raise Exception.CreateFmt(SError_SubReportComponentNotFound, [ComponentName]);

  if SameText(SBaseFileUtils.ExtractFileExt(FileName), SPatternResultFileExt) then begin
    AReport := TfrxReport.Create(nil);
    try
      AReport.PreviewPages.LoadFromFile(FileName, True);
      ACount := AReport.PreviewPages.Count;
      for i := 1 to ACount do
      begin
        if i > 1 then
        begin
          ASubReportNew := TfrxSubReport.Create(FReport);
          if Assigned(ASubReport.Parent) then
          begin
            AParent := TfrxComponentClass(ASubReport.Parent.ClassType).Create(FReport);
            AParent.Parent := ASubReport.Parent.Parent;
            AParent.Assign(ASubReport.Parent);
            ASubReportNew.Parent := AParent;
          end;
          ASubReportNew.AssignAll(ASubReport);
          ASubReportNew.Page := TfrxReportPage.Create(FReport);
          ASubReportNew.Page.Parent := FReport;
        end
        else
          ASubReportNew := ASubReport;
        ASubReportNew.Page.AssignAll(AReport.PreviewPages.Page[ACount - i]);
        PreparePage(ASubReportNew.Page);
      end;
    finally
      AReport.Free;
    end;
  end
  else
  begin
    // TODO: !!!
  end;
end;

procedure TPatternFR3.PrepareReport(var APatternStorage: TPatternStorage; const ADataSets: Array of TDataSet; const AVariables: TNamedVariants);
var
  I: Integer;
  LDataSet: TfrxDBDataSet;
  LDataSetPrefix: String;
  LValue: Variant;
//  LParams: TStrings;
  LItem: TStringNameItem;
begin
// DataSets
  for I := Pred(FReport.EnabledDataSets.Count) downto 0 do with FReport.EnabledDataSets.Items[I] do begin
    RemoveChildObject(DataSet);
    DataSet.Free;
  end;

  FReport.EnabledDataSets.Clear;
  inherited;

  LDataSetPrefix := GetDataSetPrefix;
  for I := Pred(Length(ADataSets)) downto 0 do begin
    LDataSet := TfrxDBDataSet.Create(nil);
    AddChildObject(LDataSet);
    with LDataSet do begin
      DataSet := ADataSets[I];
      Name := LDataSetPrefix + IntToStr(Succ(I));
    end;
    FReport.EnabledDataSets.Add(LDataSet);
  end;

//  FReport.LoadFromFile(APatternPath, True);
  FReport.LoadFromStream(APatternStorage.PatternStream);

  // SubReports
  LValue := ReportProperty[SParam_SubReport];
  if VarIsPresent(LValue) then begin
    for LItem in EnumStringNameItems(VarToStr(LValue), ';', False) do
      IncludeSubReport(LItem.NameLeft, LItem.NameRight);
  end;

  // Variables
  for I := Pred(AVariables.Count) downto 0 do
    with AVariables.Items[I] do
      FReport.Script.Variables[Name] := Value;

  // Prepare
  FReport.PrepareReport(False, True);
end;

function TPatternFR3.SaveReport(const FileName: String): String;
begin
  FReport.PreviewPages.SaveToFile(FileName);
  Result := FileName;
end;

{$IFNDEF REPORT_EXTERNAL}
procedure TPatternFR3.ShowReport(Owner: TForm; Caption: String; ShowModal: Boolean);
var
  LOwner: TForm;
  LViewForm: TPreviewFormFR3;
begin
  if Assigned(Owner) then
    LOwner := Owner
  else
    LOwner := Application.MainForm;
  LViewForm := TPreviewFormFR3.Create(LOwner);
  LViewForm.InsertComponent(FReport);

//  if MainDataModule.ExportsEnabled then
  FReport.PreviewOptions.Buttons := FReport.PreviewOptions.Buttons + [pbExport];

  FReport.Preview := LViewForm.Preview;

  LViewForm.Init;
  LViewForm.Caption := Caption;
  LViewForm.FDefaultSaveName := Caption;
  LViewForm.ShowMDI;
end;

procedure TPatternFR3.DesignReport(Owner: TForm);
begin
  FReport.DesignReport;
end;
{$ENDIF}

procedure TPatternFR3.PrintReport(Printer: String);
begin
  FReport.PrintOptions.ShowDialog := False;
  FReport.PrintOptions.Printer := Printer;
  FReport.Print;
end;

{ TFactoryPatterns }
function TFactoryPatterns.GetPatternClass(Index: Integer): TCustomPatternClass;
begin
  Result := TPatternFR3;
end;

{$IFNDEF REPORT_EXTERNAL}
{ TPreviewFormFR3 }
constructor TPreviewFormFR3.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Position := poDefaultPosOnly;
  Width := Width + 40;
  SaveB.OnClick := SaveBClick;
  FreeOnClose := True;
  OnShow := FormShow;
  OnActivate := FormActivate;
  OnDestroy := FormDestroy;
  // TO DO: Password Protects
  SBaseFormProperties.Icon16List.GetIcon(SBaseFormProperties.IconIndex_FastReport, Icon);
end;

procedure TPreviewFormFR3.FormActivate(Sender: TObject);
begin
  if FormStyle = fsMDIChild then
    SBaseForm.SBaseMainFormMDIInterface.WindowOnActivate(self, []);
end;

procedure TPreviewFormFR3.FormDestroy(Sender: TObject);
begin
  if FormStyle = fsMDIChild then
    SBaseForm.SBaseMainFormMDIInterface.WindowOnDestroy(self);
end;

procedure TPreviewFormFR3.FormShow(Sender: TObject);
begin
  if FormStyle = fsMDIChild then
    SBaseForm.SBaseMainFormMDIInterface.RegisterWindow(self);
end;

procedure TPreviewFormFR3.ShowMDI;
begin
  if Visible then
  begin
    if WindowState = wsMinimized then
      WindowState := wsNormal;
    inherited Show;
  end
  else
  begin
    Position := poDesigned;
    FormStyle := fsMDIChild;
  end;
end;

procedure TPreviewFormFR3.SaveBClick(Sender: TObject);
begin
  Preview.SaveToFileDefault(FDefaultSaveName);
end;
{$ENDIF}

{$IFNDEF PATTERN_EXTERNAL}
initialization
  RegisterFactoryPatterns(TFactoryPatterns.Create as IFactoryPatterns);
{$ENDIF}

end.
