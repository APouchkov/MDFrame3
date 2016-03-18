unit SBaseReportForm;
{$I config.inc}

interface

uses
  NamedVariables, SBaseForm, SBaseProgress, SBaseReport, SBaseDataModule, SBaseDataHelperInternal,

  Classes, Clipbrd, SysUtils, Windows, Messages, Variants, Graphics, Dialogs,
  Controls, Forms, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, ExtCtrls,
  LayoutPanel, cxGroupBox, System.Actions, ActnList, dxBar, Menus,
  cxTextEdit, cxMaskEdit, cxDropDownEdit, cxImageComboBox,
  cxImageLookupComboBox, StdCtrls, cxButtons, cxStyles, cxCustomData,
  cxFilter, cxData, cxDataStorage, cxNavigator, Data.DB, cxDBData, cxGridLevel,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxClasses,
  cxGridCustomView, cxGrid, dxSkinsCore, dxSkinsDefaultPainters, dxMDBar;

type
  TCustomReportShow = procedure (Rep: TSBaseReport; ShowModal: Boolean; const AParams: TNamedVariants) of object;
  TCustomReportSave = procedure (Rep: TSBaseReport; const FileName: String; ReplaceType: TRepReplaceType; const AParams: TNamedVariants) of object;

  TDoReportType = (drShow, drShowModal, drSave, drMail, drBuild);

  TSBaseReportFrm = class(TSBaseFrm)
    PrintPrint: TAction;
    PrintSave: TAction;
    PrintMail: TAction;
    OnPrepareParams: TButton;
    FormatsDataSource: TDataSource;
    LogDataSource: TDataSource;
    PanelMain: TcxGroupBox;
    PanelForm: TGroupPanel;
    pTsMain: TGroupPanel;
    PanelLog: TPanel;
    cxGridLog: TcxGrid;
    cxGridLogDBTableView: TcxGridDBTableView;
    cxGridLogType: TcxGridDBColumn;
    cxGridLogText: TcxGridDBColumn;
    cxGridLogLevel: TcxGridLevel;
    SplitterLog: TSplitter;
    PanelButtons: TGroupPanel;
    btnPrint: TcxButton;
    btnClose: TcxButton;
    lpFormat: TLayoutPanel;
    lcbFormat: TcxImageLookupComboBox;
    gbParams: TcxLayoutGroupBox;
    mnPrint: TdxBarButton;
    mnSaveAs: TdxBarButton;
    mnLogCopy: TdxBarButton;
    mnLogDelete: TdxBarButton;
    mnLogSave: TdxBarButton;
    mnLogClear: TdxBarButton;
    pmPrint: TdxBarPopupMenu;
    pmLog: TdxBarPopupMenu;

    procedure PrintPrintExecute(Sender: TObject); virtual;
    procedure PrintSaveExecute(Sender: TObject); virtual;
    procedure btnCloseClick(Sender: TObject);
    procedure ActionListExecute(Action: TBasicAction; var Handled: Boolean);
    procedure mnLogSaveClick(Sender: TObject);
    procedure mnLogCopyClick(Sender: TObject);
    procedure mnLogDeleteClick(Sender: TObject);
    procedure mnLogClearClick(Sender: TObject);
    procedure pmLog1Popup(Sender: TObject);
    procedure SplitterLogCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
    procedure PanelLogResize(Sender: TObject);
  private
    FReport: TSBaseReport;

//    FObject_id: Integer;
    FMultiThread: Boolean;
    FPatternDefaultType: String;
    FLogHeight: Integer;
    FConstraintMaxHeight: Integer;
    FOnCustomReportShow: TCustomReportShow;
    FOnCustomReportSave: TCustomReportSave;

    function GetUserConfig(const Name: String): Variant;
    procedure SetUserConfig(const Name: String; const Value: Variant);
    procedure SetLogHeight(const Value: Integer);

    procedure DoReport(Action: TDoReportType; ATargetName: String = ''; AReplaceType: TRepReplaceType = rrtReplace);
    function GetProcedureName: String;
    function GetProcedureParams: String;
    procedure SetProcedureName(const Value: String);
    procedure SetProcedureParams(const Value: String);
    function GetObject_Id: Integer;
    procedure SetObject_Id(const Value: Integer);
    procedure SetMultiThread(const Value: Boolean);
  protected
    // Report
    FOverrideProcedure: TNamedVariants;
    FDirName: String;
    FNewPrintType: Integer;
    FProcedureName, FProcedureParams: String;

    FormatsDataSet : TUserMemoryDataSet;
    FormatsDataSetCode: TStringField;
    FormatsDataSetName: TStringField;
    FormatsDataSetImageIndex: TIntegerField;

    LogDataSet: TUserMemoryDataSet;
    LogDataSetType: TStringField;
    LogDataSetText: TStringField;

    procedure InternalCreate; override;
    procedure InternalInit;   override;
    procedure ImportSetings;  virtual;
    procedure FilterValueChanged; override;


    procedure CheckParams(var Result: Boolean); virtual;
    procedure PrepareParams(Sender: TSBaseReport); virtual;
    procedure BeforeConnect(Sender: TSBaseReport); virtual;
    procedure BeforeBuild(Sender: TSBaseReport); virtual;
    procedure AfterBuild(Sender: TSBaseReport); virtual;
    procedure BeforePrint(Sender: TSBaseReport; const AParams: TNamedVariants); virtual;
    procedure AfterPrint(Sender: TSBaseReport); virtual;
    procedure DoLogMessage(const Msg: String; LogType: Char); virtual;

    property Report: TSBaseReport read FReport;
    // Form
    procedure LoadUserConfig(AStoreFormSettings: TFormItemsForSave); override;
    function GetPrintType: Integer;
    procedure SetPrintType(Value: Integer);
    procedure PrepareFormats; virtual;
//    procedure PrepareVersions; virtual;
    procedure WMProperty(var Msg: TMessage); message WM_PROPERTIES;
    procedure RefreshForm; virtual;
    // Config
    procedure SaveConfig; virtual;
    procedure LoadConfig; virtual;
    property UserConfig[const Name: String]: Variant read GetUserConfig write SetUserConfig;
  public
    destructor Destroy; override;
    function CloseQuery: Boolean; override;
    function BuildReport: Boolean; virtual;

    procedure ShowReport(ShowModal: Boolean = False); virtual;
    procedure SaveReport(const AFileName: String; AReplaceType: TRepReplaceType = rrtReplace); virtual;
    procedure MailReport(const EMail: String = ''); virtual;

    property PrintType: Integer read GetPrintType write SetPrintType;
    property MultiThread: Boolean read FMultiThread write SetMultiThread;
    property OnCustomReportShow: TCustomReportShow read FOnCustomReportShow write FOnCustomReportShow;
    property OnCustomReportSave: TCustomReportSave read FOnCustomReportSave write FOnCustomReportSave;
  published
    property Object_Id: Integer read GetObject_Id write SetObject_Id;
    property ProcedureName: String read GetProcedureName write SetProcedureName;
    property ProcedureParams: String read GetProcedureParams write SetProcedureParams;
    property LogHeight: Integer read FLogHeight write SetLogHeight;
  end;

implementation

{$R *.dfm}

uses
  SBaseConstants, SBaseVariantFunctions, SBaseFileUtils, SBaseStringFunctions;

const
  _ComponentSpace = 7;
  SC_MenuItem_Designed = WM_USER + 1;
//  SLogPrefixType: Array [0..2] of String = ('INFO: ', 'ERROR: ', 'WARNING: ');

resourcestring
  SRepActionCategoryPrint = 'Print';
  SRepLogFileNamePostfix = ' (Log)';
  SRepLogFilter = 'Текстовый файл (*.txt)|*.txt';
  SRepLogFileNameExt = 'txt';
  SRepParam_MaxRecordCount = 'MaxRecordCount';
  SRepConfigParamPrintType = 'PrintType';
  SRepConfigParamDirName = 'DirName';
  SRepConfigParamPatternType = 'PatternType';

{ TReportForm }
procedure TSBaseReportFrm.InternalCreate;
begin
  FOverrideProcedure.UnknownAsNull := True;
  FReport := TSBaseReport.Create(Self);

  inherited;

  lcbFormat.Properties.Images := SBaseFormProperties.Icon16List;

  FormatsDataSet := TUserMemoryDataSet.Create(Self);
  FormatsDataSetCode := TStringField.Create(Self);
  with FormatsDataSetCode do begin
    FieldName := SConst_Code;
    Size      := 20;
    DataSet   := FormatsDataSet;
  end;
  FormatsDataSetName := TStringField.Create(Self);
  with FormatsDataSetName do begin
    FieldName := SConst_Name;
    Size      := 100;
    DataSet   := FormatsDataSet;
  end;
  FormatsDataSetImageIndex := TIntegerField.Create(Self);
  with FormatsDataSetImageIndex do begin
    FieldName := SConst_ImageIndex;
    DataSet   := FormatsDataSet;
  end;
  FormatsDataSet.Open;
  FormatsDataSource.DataSet := FormatsDataSet;

  LogDataSet := TUserMemoryDataSet.Create(Self);
  LogDataSetType := TStringField.Create(LogDataSet);
  with LogDataSetType do begin
    FieldName := SConst_Type;
    Size      := 1;
    DataSet   := LogDataSet;
  end;
  LogDataSetText := TStringField.Create(LogDataSet);
  with LogDataSetText do begin
    FieldName := SConst_Text;
    Size      := 2048;
    DataSet   := LogDataSet;
  end;
  LogDataSet.Open;
  LogDataSource.DataSet := LogDataSet;


  with TcxImageComboBoxProperties(cxGridLogType.Properties) do begin
    Images := SBaseFormProperties.Icon16List;
    with Items.Add do begin
      ImageIndex := SBaseFormProperties.IconIndex_MsgInfo;
      Value := 'I';
    end;
    with Items.Add do begin
      ImageIndex := SBaseFormProperties.IconIndex_MsgWarning;
      Value := 'W';
    end;
    with Items.Add do begin
      ImageIndex := SBaseFormProperties.IconIndex_MsgError;
      Value := 'E';
    end;
  end;

  pmLog.Images            := SBaseFormProperties.Icon16List;
  mnLogCopy.ImageIndex    := SBaseFormProperties.IconIndex_Copy;
  mnLogDelete.ImageIndex  := SBaseFormProperties.IconIndex_Delete;
  mnLogSave.ImageIndex    := SBaseFormProperties.IconIndex_FileSave;

  Busy := False;
  SplitterLog.Height := 4;
  LogHeight := 60;

  FMultiThread := True;
//  MDIToolButtons := [mtbPrint, mtbProperties];

  pmPrint.Images      := SBaseFormProperties.Icon16List;
  mnPrint.ImageIndex  := SBaseFormProperties.IconIndex_Printer;
  mnSaveAs.ImageIndex := SBaseFormProperties.IconIndex_FileSave;
end;

destructor TSBaseReportFrm.Destroy;
begin
  FreeAndNil(FReport);
  inherited;
end;

procedure TSBaseReportFrm.PanelLogResize(Sender: TObject);
begin
  inherited;
  FLogHeight := PanelLog.Height;
end;

procedure TSBaseReportFrm.FilterValueChanged;
begin
  inherited;
  if not Loading then
    PrepareFormats;
end;

procedure TSBaseReportFrm.CheckParams(var Result: Boolean);
begin
  Result := True;
end;

procedure TSBaseReportFrm.PrepareParams(Sender: TSBaseReport);
begin
  OnPrepareParams.Click;

  Sender.ReportParams[SRepParam_Report] := Integer(Sender);
  Sender.AssignReportParams(InternalFormConstants + CreateFormParams + InternalFormParams);

{$IFDEF FASTSCRIPT_RTTI}
  CallFunction('PrepareParams', [TNamedVariant.Create(SConst_Sender, CastAsVariant(Sender))], False);
{$ENDIF}

//  AReport.ReportParams[SRepParam_TestVersion] := EditVersion.Checked;
  Sender.ReportParams[SRepParam_PatternTypeCode] := lcbFormat.EditValue;

  case FNewPrintType of
    1: Sender.ReportParams[SRepParam_MaxRecordCount] := ReportMaxCountListOpen;
    3: Sender.ReportParams[SRepParam_MaxRecordCount] := ReportMaxCountListMail;
  end;
end;

procedure TSBaseReportFrm.AfterBuild(Sender: TSBaseReport);
begin
  PrintType := FNewPrintType;
end;

procedure TSBaseReportFrm.BeforeConnect(Sender: TSBaseReport);
begin
end;

procedure TSBaseReportFrm.BeforeBuild(Sender: TSBaseReport);
begin
end;

procedure TSBaseReportFrm.BeforePrint(Sender: TSBaseReport; const AParams: TNamedVariants);
begin
end;

procedure TSBaseReportFrm.AfterPrint(Sender: TSBaseReport);
begin
end;

procedure TSBaseReportFrm.DoLogMessage(const Msg: String; LogType: Char);
begin
  LogDataSet.DisableControls;
  try
    LogDataSet.Append;
      LogDataSetType.AsString := LogType;
      LogDataSetText.AsString := Msg;
    LogDataSet.Post;
  finally
    LogDataSet.EnableControls;
  end;

  if not PanelLog.Visible then
  begin
    FConstraintMaxHeight := Constraints.MaxHeight;
    Constraints.MaxHeight := 0;
//    TabSheetPanel.Align := alTop;
    PanelLog.Visible := True;
    SplitterLog.Visible := True;
    Height := Height + FLogHeight + SplitterLog.Height;
    SplitterLog.Top := PanelLog.Top;
  end;
end;

function TSBaseReportFrm.BuildReport: Boolean;
begin
  Result := False;

  with FReport do
  try
    Self.PrepareParams(FReport);
    ReportParams[SRepParam_BuildInThread] := FMultiThread;
    FReport.OpenReport;

    FReport.ReadOnly := False;
    OnLogMessage := DoLogMessage;
    Busy := True;
    BeginProcess(True, Self);
    try
      BeforeConnect(FReport);
      Connect(FProcedureName);
      BeforeBuild(FReport);
      try
        Result := BuildReport(FOverrideProcedure);
      finally
        AfterBuild(FReport);
      end;
    finally
      EndProcess;
      Busy := False;
    end;
  finally
    if not Result then
      FreeAndNil(FReport);
  end;
end;

procedure TSBaseReportFrm.ShowReport(ShowModal: Boolean = False);
var
  Action: TDoReportType;
begin
  if ShowModal then
    Action := drShowModal
  else
    Action := drShow;
  DoReport(Action);
end;

procedure TSBaseReportFrm.SaveReport(const AFileName: String; AReplaceType: TRepReplaceType = rrtReplace);
begin
  DoReport(drSave, AFileName, AReplaceType);
end;

procedure TSBaseReportFrm.MailReport(const EMail: String = '');
begin
  DoReport(drMail, EMail);
end;

procedure TSBaseReportFrm.LoadConfig;
begin
  if not Assigned(btnPrint.Action) then
    btnPrint.Action := PrintPrint;

  PrintType := VarToIntDef(UserConfig[SRepConfigParamPrintType], PrintType);
  FDirName  := VarToStr(UserConfig[SRepConfigParamDirName]);
  FPatternDefaultType := VarToStr(UserConfig[SRepConfigParamPatternType]);
end;

procedure TSBaseReportFrm.SaveConfig;
begin
  UserConfig[SRepConfigParamPrintType] := PrintType;
  UserConfig[SRepConfigParamDirName] := FDirName;
  UserConfig[SRepConfigParamPatternType] := lcbFormat.EditValue;
end;

procedure TSBaseReportFrm.LoadUserConfig(AStoreFormSettings: TFormItemsForSave);
begin
  inherited;
  if frsvUserDefined in AStoreFormSettings then LoadConfig;
end;

function TSBaseReportFrm.GetUserConfig(const Name: String): Variant;
var
  Str: String;
begin
  Str := VarToStr(GetSetting(Name));
  if Str = '' then
    Result := Null
  else if Str = ' ' then
    Result := ''
  else
    Result := Str;
end;

procedure TSBaseReportFrm.SetUserConfig(const Name: String; const Value: Variant);
var
  AValue: String;
begin
  case VarType(Value) of
    varEmpty, varNull: AValue := '';
    varString, varUString: if Value = '' then AValue := ' ' else AValue := Value;
    else AValue := VarToStr(Value);
  end;
  SetSetting(Name, AValue);
end;

function TSBaseReportFrm.GetObject_Id: Integer;
begin
  Result := VarToIntDef(GetVariable(SRepParam_ObjectId, True), 0)
end;

procedure TSBaseReportFrm.SetMultiThread(const Value: Boolean);
begin
  FMultiThread := Value;
  FReport.ReportParams[SRepParam_BuildInThread] := FMultiThread;
end;

procedure TSBaseReportFrm.SetObject_Id(const Value: Integer);
begin
  InternalFormParams[SRepParam_ObjectId] := Value;
  FReport.ReportParams[SRepParam_ObjectId] := Object_Id;
  FReport.UnPrepareReport;
end;

procedure TSBaseReportFrm.SetLogHeight(const Value: Integer);
begin
  FLogHeight := Value;
  PanelLog.Height := FLogHeight;
end;

procedure TSBaseReportFrm.WMProperty(var Msg: TMessage);
begin
  SaveConfig;
end;

procedure TSBaseReportFrm.RefreshForm;
begin
end;

function TSBaseReportFrm.CloseQuery: Boolean;
begin
  Result := not Busy;
end;

procedure TSBaseReportFrm.ActionListExecute(Action: TBasicAction; var Handled: Boolean);
begin
  inherited;
  FNewPrintType := Action.Tag;
  mnLogClear.Click;
end;

procedure TSBaseReportFrm.PrintPrintExecute(Sender: TObject);
var
  LDirName: String;
begin
  if Sender = PrintSave then begin
    LDirName := RepNewDirName(FDirName);
    if LDirName.IsEmpty then Exit;
    FDirName := LDirName;
    SaveReport(FDirName);
  end else
    ShowReport;
end;

procedure TSBaseReportFrm.PrintSaveExecute(Sender: TObject);
var
  LDirName: String;
begin
  if Sender = mnSaveAs then begin
    LDirName := RepNewDirName(FDirName);
    if LDirName.IsEmpty then Exit;
    FDirName := LDirName;
    SaveReport(FDirName);
  end else if Sender = PrintMail then
    MailReport
  else
    ShowReport;
end;

procedure TSBaseReportFrm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

function TSBaseReportFrm.GetPrintType: Integer;
begin
  if not Assigned(btnPrint.Action) then
    Result := -1
  else
    Result := btnPrint.Action.Tag;
end;

function TSBaseReportFrm.GetProcedureName: String;
begin
  Result := VarToStr(FOverrideProcedure[SRepParam_ProcedureName]);
end;

procedure TSBaseReportFrm.SetProcedureName(const Value: String);
begin
  FOverrideProcedure[SRepParam_ProcedureName] := NullIf(Value, '');
end;

function TSBaseReportFrm.GetProcedureParams: String;
begin
  Result := VarToStr(FOverrideProcedure[SRepParam_ProcedureParams]);
end;

procedure TSBaseReportFrm.SetProcedureParams(const Value: String);
begin
  FOverrideProcedure[SRepParam_ProcedureParams] := NullIf(Value, '');
end;

procedure TSBaseReportFrm.SetPrintType(Value: Integer);
var
  I: Integer;
begin
  if PrintType = Value then Exit;
  for I := Pred(ActionList.ActionCount) downto 0 do
    with ActionList.Actions[I] do
      if (Category = SRepActionCategoryPrint) and (Tag = Value) then begin
        btnPrint.Glyph.Assign(nil);
        btnPrint.Action := ActionList.Actions[I];
        break;
      end;
end;

procedure TSBaseReportFrm.pmLog1Popup(Sender: TObject);
begin
  mnLogCopy.Enabled := (not LogDataSet.IsEmpty);
  mnLogDelete.Enabled := mnLogCopy.Enabled;
  mnLogClear.Enabled := mnLogCopy.Enabled;
end;

procedure TSBaseReportFrm.mnLogSaveClick(Sender: TObject);
var
  LRecNo: Integer;
  LStrings: TStringList;
begin
  if LogDataSet.IsEmpty then exit;
  with TSaveDialog.Create(nil) do
  try
    FileName := SBaseFileUtils.PrepareFileName(Caption + SRepLogFileNamePostfix);
    Filter := SRepLogFilter;
    DefaultExt := SRepLogFileNameExt;

    if Execute then begin
      LStrings := TStringList.Create;
      try
        LRecNo := LogDataSet.RecNo;
        LogDataSet.DisableControls;
        try
          LogDataSet.First;
          while not LogDataSet.Eof do begin
            LStrings.Add(LogDataSetType.AsString + LogDataSetText.AsString);
            LogDataSet.Next;
          end;
          LStrings.SaveToFile(FileName);
        finally
          LogDataSet.RecNo := LRecNo;
          LogDataSet.EnableControls;
        end;
      finally
        LStrings.Free;
      end;
    end;
  finally
    Free;
  end;
end;

procedure TSBaseReportFrm.mnLogCopyClick(Sender: TObject);
begin
  if not LogDataSet.IsEmpty then
    ClipBoard.AsText := LogDataSetText.AsString
end;

procedure TSBaseReportFrm.mnLogDeleteClick(Sender: TObject);
begin
  if not LogDataSet.IsEmpty then LogDataSet.Delete;
  if LogDataSet.IsEmpty then
  begin
    SplitterLog.Visible := False;
    PanelLog.Visible := False;
//    TabSheetPanel.Align := alClient;
  end;
end;

procedure TSBaseReportFrm.mnLogClearClick(Sender: TObject);
begin
  if Assigned(LogDataSet) and not LogDataSet.IsEmpty then
    LogDataSet.EmptyTable;

  if PanelLog.Visible then begin
    FLogHeight := PanelLog.Height;
    SplitterLog.Visible := False;
    PanelLog.Visible := False;
    Height := Height - FLogHeight - SplitterLog.Height;
    Constraints.MaxHeight := FConstraintMaxHeight;
  end;
end;

procedure TSBaseReportFrm.SplitterLogCanResize(Sender: TObject; var NewSize: Integer; var Accept: Boolean);
begin
  Accept := NewSize + 20 < pTsMain.Height - PanelMain.Constraints.MinHeight;
end;

procedure TSBaseReportFrm.PrepareFormats;
var
  LObjectParams: String;
begin
  FReport.PrepareReport;
  LObjectParams := FReport.ObjectParams;
  if LObjectParams <> '' then
    FReport.ReportParams[LObjectParams] := Variables[LObjectParams];
  FReport.GetPatternTypeList(FormatsDataSet);

  if (FormatsDataSet.RecordCount > 0) and (FPatternDefaultType = '') then begin
    FormatsDataSet.First;
    FPatternDefaultType := FormatsDataSet.FieldByName(SConst_Code).AsString;
  end;

  if (not FPatternDefaultType.IsEmpty)
    and
    (
      not VarIsPresent(lcbFormat.EditValue)
      or
      not FormatsDataSet.Locate(lcbFormat.Properties.KeyField, lcbFormat.EditValue, [])
    )
  then
    lcbFormat.EditValue := FPatternDefaultType;

  lpFormat.Visible := (lcbFormat.Properties.ListSource.DataSet.RecordCount > 1);
  btnPrint.Enabled := (not FormatsDataSet.IsEmpty);
end;

procedure TSBaseReportFrm.DoReport(Action: TDoReportType; ATargetName: String = ''; AReplaceType: TRepReplaceType = rrtReplace);
var
//  LRep: TSBaseReport;
  {$IFDEF REPORT_DEBUG}
  LTime, LBuildTime, LPrintTime, LBuildTimeAll, LPrintTimeAll: TTime;
  {$ENDIF}
//  LOpenReportRequired: Boolean;
  LResult: Boolean;
begin
  CheckParams(LResult);
  if not LResult then exit;

  {$IFDEF REPORT_DEBUG}
  ATime := Time;
  ABuildTimeAll := 0;
  APrintTimeAll := 0;
  {$ENDIF}

//  LOpenReportRequired := False;

//  if Owner is TSBaseReport then
//    LRep := TSBaseReport(Owner)
//  else if Assigned(FRep) then
//    LRep := FRep
//  else begin
//    LRep := TSBaseReport.Create(nil);
//    LRep.ReportParams[SRepParam_ObjectId] := Object_Id;
//    LRep.ReportParams[SRepParam_TestVersion] := EditVersion.Checked;
//    LOpenReportRequired := True;
//  end;

  with FReport do
  try
    OnLogMessage := DoLogMessage;
    Busy := True;
    BeginProcess(True, Self);
    try
      Self.PrepareParams(FReport);
//      if LOpenReportRequired then
      FReport.OpenReport;

      BeforeConnect(FReport);
      Connect(FProcedureName);

      BeforeBuild(FReport);
      try
        {$IFDEF REPORT_DEBUG}
        ABuildTime := Time;
        {$ENDIF}

        if BuildReport(FOverrideProcedure) then begin
          repeat
            {$IFDEF REPORT_DEBUG}
            ABuildTimeAll := ABuildTimeAll + Time - ABuildTime;
            {$ENDIF}

            BeforePrint(FReport, Data.Params);

            {$IFDEF REPORT_DEBUG}
            APrintTime := Time;
            {$ENDIF}
            case Action of
              drShow, drShowModal: begin
                if Assigned(FOnCustomReportShow) then
                  FOnCustomReportShow(FReport, Action = drShowModal, Data.Params)
                else
                  ShowReport(Action = drShowModal);
              end;
              drSave: begin
                if Assigned(FOnCustomReportSave) then
                  FOnCustomReportSave(FReport, ATargetName, AReplaceType, Data.Params)
                else
                  SaveReport(ATargetName, AReplaceType);
              end;
              drMail: begin
                SaveReport(TEMP_DIR, rrtNew);
              end;
            end;
            {$IFDEF REPORT_DEBUG}
            APrintTimeAll := APrintTimeAll + Time - APrintTime;
            {$ENDIF}
            AfterPrint(FReport);
            {$IFDEF REPORT_DEBUG}
            ABuildTime := Time;
            {$ENDIF}
          until not BuildReportNext;
          {$IFDEF REPORT_DEBUG}
          ABuildTimeAll := ABuildTimeAll + Time - ABuildTime;
          {$ENDIF}
        end;
        if (Action = drMail) and (FileNames.Count > 0) then begin
          ATargetName := ATargetName.Trim;
          if ATargetName <> '' then
            SendMail(GetMailConfig([]), ATargetName, Self.Caption, '', '', FileNames.DelimitedText)
          else
            OpenMail('', Self.Caption, '', '', FileNames.DelimitedText);
        end;
      finally
        AfterBuild(FReport);
        Disconnect;
      end;
    finally
      EndProcess;
      Busy := False;
    end;
  finally
    FReport.ClearData;
//    if Assigned(LRep) and not(Self.Owner is TSBaseReport) and (FRep <> LRep) then LRep.Free;
{$IFDEF REPORT_DEBUG}
    InfoMessage(SDebugTime_Exec + TimeToStr(ABuildTimeAll));
    InfoMessage(SDebugTime_Buld + TimeToStr(APrintTimeAll));
    InfoMessage(SDebugTime_Total + TimeToStr(Time - ATime));
{$ENDIF}
  end;
end;

procedure TSBaseReportFrm.ImportSetings;
begin
  {virtual}
end;

procedure TSBaseReportFrm.InternalInit;
begin
  ImportSetings;
  inherited;

  PanelLog.Visible := False;

  FReport.ReportParams[SRepParam_ObjectId] := Object_Id;
  FReport.ReportParams[SRepParam_BuildInThread] := FMultiThread;
{$IFDEF DEBUG}
  if Object_Id = 0 then
    SBaseMainFormMDIInterface.Log(lgInfo, 'Форма запущена с параметром Object_Id = 0. В режиме отладки это не является ошибкой!')
  else
{$ENDIF}
  PrepareFormats;
end;

initialization
  Classes.RegisterClass(TSBaseReportFrm);
//  RegisterReportForm(TSBaseReportFrm);

finalization
  Classes.UnRegisterClass(TSBaseReportFrm);

end.


