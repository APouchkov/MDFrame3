unit SBaseScriptDebuggerForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Types,
  Dialogs
{IFDEF Script_RTTI}
  , fs_iinterpreter, fs_idbrtti, fs_iformsrtti, fs_iclassesrtti, fs_igraphicsrtti, fs_ievents
  , fs_synmemo
{ENDIF}
  , SBaseForm, hsDialogs, dxBar, cxClasses, ImgList, cxGraphics, dxBarExtItems,
  System.Actions, ActnList, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  dxStatusBar, dxMDStatusBar, SynEdit, dxBarBuiltInMenu, cxPC, cxStyles, cxCustomData,
  cxFilter, cxData, cxDataStorage, cxEdit, cxNavigator, Data.DB, cxDBData,
  cxGridLevel, cxGridCustomView, cxGridCustomTableView, cxGridTableView,
  cxGridDBTableView, cxGrid, MemDS, VirtualTable, cxSplitter, cxMDGrid,
  dxSkinsCore, dxSkinsDefaultPainters, dxMDBar, SBaseDataModule;

type
  TSBaseFrmCrack = class(TSBaseFrm);

  TSBaseScriptDebuggerFrm = class(TSBaseFrm)
    ActionFind                : TAction;
    ActionFindNext            : TAction;

    ToolBar                   : TdxBar;
    mnRun                     : TdxBarButton;
    mnPause                   : TdxBarButton;
    mnStep                    : TdxBarButton;
    mnSetBreakpoint           : TdxBarButton;
    mnResetBreakpoint         : TdxBarButton;
    mnWatch                   : TdxBarButton;
    mnWatchAdd                : TdxBarButton;
    mnWatchDelete             : TdxBarButton;
    mnWatchEdit               : TdxBarButton;

    pmWatches                 : TdxBarPopupMenu;

    dxBar                     : TdxBarStatic;

    SynEdit                   : TSynEdit;
    dxStatusBar               : TdxMDStatusBar;
    splDebugger               : TcxSplitter;
    pcDebugger                : TcxPageControl;
    tsVariables               : TcxTabSheet;
    tsWatch                   : TcxTabSheet;
    DataSourceVariables       : TDataSource;

    GridVairables             : TcxMDGrid;
    GridVairablesLevel        : TcxMDGridLevel;
    GridVairablesDBTableView  : TcxMDGridDBTableView;
    ColumnVariableName        : TcxGridDBColumn;
    ColumnVariableType        : TcxGridDBColumn;
    ColumnVariableClass       : TcxGridDBColumn;
    ColumnVariableValue       : TcxGridDBColumn;
    DataSourceWatch           : TDataSource;

    GridWatch                 : TcxMDGrid;
    GridWatchLevel            : TcxMDGridLevel;
    GridWatchDBTableView      : TcxMDGridDBTableView;
    ColumnWatchName           : TcxGridDBColumn;
    ColumnWatchType           : TcxGridDBColumn;
    ColumnWatchClass          : TcxGridDBColumn;
    ColumnWatchValue          : TcxGridDBColumn;
    VTableVariables: TSQLDataSetProvider;
    VTableWatch: TSQLDataSetProvider;
    VTableVariablesVarName: TStringField;
    VTableVariablesVarType: TStringField;
    VTableVariablesVarClassType: TStringField;
    VTableVariablesVarValueStr: TStringField;
    VTableWatchVarName: TStringField;
    VTableWatchVarType: TStringField;
    VTableWatchVarClassType: TStringField;
    VTableWatchVarValueStr: TStringField;

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean); override;
    procedure mnRunClick(Sender: TObject);
    procedure mnPauseClick(Sender: TObject);
    procedure mnStepClick(Sender: TObject);
    procedure ActionObjectInspectorExecute(Sender: TObject); override;
    procedure SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure SynEditSpecialLineColors(Sender: TObject; Line: Integer;
      var Special: Boolean; var FG, BG: TColor);
    procedure SynEditGutterClick(Sender: TObject; Button: TMouseButton; X, Y,
      Line: Integer; Mark: TSynEditMark);
    procedure FormDestroy(Sender: TObject);
    procedure SynEditGutterPaint(Sender: TObject; aLine, X, Y: Integer);
    procedure mnSetBreakpointClick(Sender: TObject);
    procedure mnWatchClick(Sender: TObject);
    procedure pcDebuggerChange(Sender: TObject);
    procedure VTableWatchBeforePost(DataSet: TDataSet);
    procedure GridWatchDBTableViewDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure GridWatchDBTableViewDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
  private
    FBreakPoints              : TList;
    FRunLineLock              : Boolean;
    FWaitNextStepLock         : Boolean;

    FErrorLine                : Integer;
    FLine                     : Integer;
    FPaused                   : Boolean;
    FGotoNextStep             : Boolean;

    FNeewShowVariables        : Boolean;
    FNeewShowWatch            : Boolean;

    FLockGetWatchVariableInfo : Boolean;
    FCurrentScript            : TfsScript;

    const CPanelLine          : Integer = 1;
    const CPanelChar          : Integer = 2;

    function DebugForm: TSBaseFrmCrack; Inline;
  protected
    procedure ReshowButtons;
    procedure InternalCreate; override;
    procedure InternalInit; override;

    procedure SetActiveLine(ALine: Integer);

    function IsBreakpointLine(ALine: Integer): Boolean;
    function IsExecuteable(ALine: Integer): Boolean;
    procedure SetBreakPoint(ALine: Integer);
    procedure ResetBreakPoint(ALine: Integer);

    // --
    procedure FeelVTableFromVariable(AVariable : TfsCustomVariable; AVDataSet: TDataSet; AForceFill : Boolean);
    procedure ReedWatchVariableInfo(ACurrentScript : TfsScript; AVDataSet: TDataSet);
  public
    procedure CheckShowDebugValues;
    procedure ShowVariables(ACurrentScript : TfsScript);
    procedure ShowWatch(ACurrentScript : TfsScript);

    procedure SetErrorLine(ALine : Integer);
    procedure RunLine(AScript: TfsScript; const UnitName, SourcePos: String);
    procedure WaitNextStep(ALine : Integer);
    function IsDebugging: Boolean;

    destructor Destroy; override;
  end;

implementation

uses
  SBaseVariantFunctions, StrUtils, SBaseConstants, SBaseUtils, SynEditTypes,
  SBaseItemReactions, DataModule, MDSynSearch;

{$R *.dfm}

{ TSBaseScriptDebuggerFrm }

procedure TSBaseScriptDebuggerFrm.InternalCreate;
var
  LIR : TBaseItemReaction;
begin
  Assert(Assigned(Owner) and (Owner is TSBaseFrm));
  inherited;

  FLine := -1;
  FBreakPoints := TList.Create;
  DebugForm.FScriptDebuggerForm := Self;

{DESIGN}
  IconIndex := SBaseFormProperties.IconIndex_FindInTree;

  GridVairablesDBTableView.Configure := 'Indicator,AutoWidth';
  GridWatchDBTableView.Configure := 'Indicator,SyncMode,AutoWidth';

  mnRun.Caption := SBaseProperties.Text_MenuRun;
  mnRun.ImageIndex := SBaseFormProperties.IconIndex_Run;

  mnPause.Caption := SBaseProperties.Text_MenuPause;
  mnPause.ImageIndex := SBaseFormProperties.IconIndex_Pause;

  mnStep.Caption := SBaseProperties.Text_MenuStep;
  mnStep.ImageIndex := SBaseFormProperties.IconIndex_Step;

  mnSetBreakpoint.Caption := SBaseProperties.Text_MenuBreakPoint;
  mnSetBreakpoint.ImageIndex := SBaseFormProperties.IconIndex_BreakPoint;

  mnWatch.Caption := SBaseProperties.Text_MenuWatches;
  mnWatch.ImageIndex := SBaseFormProperties.IconIndex_Watches;

  mnWatchAdd.Caption := SBaseProperties.Text_MenuNew;
  mnWatchAdd.ImageIndex := SBaseFormProperties.IconIndex_New;

  mnWatchEdit.Caption := SBaseProperties.Text_MenuEdit;
  mnWatchEdit.ImageIndex := SBaseFormProperties.IconIndex_Edit;

  mnWatchDelete.Caption := SBaseProperties.Text_MenuDelete;
  mnWatchDelete.ImageIndex := SBaseFormProperties.IconIndex_Delete;

  LIR := ItemsReactions.Add;
  LIR.Item := mnWatchAdd;
  LIR.LinkedComponent := GridWatchDBTableView;
  LIR.OnClick.Reaction := biraDataSourceInPlaceInsert;

  LIR := ItemsReactions.Add;
  LIR.Item := mnWatchEdit;
  LIR.LinkedComponent := GridWatchDBTableView;
  LIR.OnClick.Reaction := biraDataSourceInPlaceEdit;

  LIR := ItemsReactions.Add;
  LIR.Item := mnWatchDelete;
  LIR.LinkedComponent := GridWatchDBTableView;
  LIR.OnClick.Reaction := biraDataSourceInPlaceDelete;
{/DESIGN}

  with TMDSynSearch.Create(Self) do begin
    ActionFind      := Self.ActionFind;
    ActionFindNext  := Self.ActionFindNext;
    //ActionReplace   := Self.ActionReplace;
  end;
end;

procedure TSBaseScriptDebuggerFrm.InternalInit;
var
  LIdx: Integer;
begin
  with DebugForm.FScript do begin
    SynEdit.Lines := Lines;
    OnRunLine := RunLine;
  end;

  if CreateFormParams.Find(SConst_Line, LIdx) then begin
    SetActiveLine(CreateFormParams.Items[LIdx].Value);
    CreateFormParams.Delete(Lidx);
  end else
    SetActiveLine(-1);

  FPaused := (CreateFormParams[SConst_Paused] = True);
  if FPaused and (FLine >= 0) then
    SetErrorLine(FLine)
  else
    FErrorLine := -1;

  ReshowButtons;

  VTableWatch.Open;
end;

function TSBaseScriptDebuggerFrm.IsBreakpointLine(ALine: Integer): Boolean;
begin
  Result := Assigned(FBreakPoints) and (FBreakPoints.IndexOf(Pointer(ALine)) >= 0);
end;

function TSBaseScriptDebuggerFrm.IsDebugging: Boolean;
begin
  Result := FPaused or FGotoNextStep
         or FRunLineLock or FWaitNextStepLock;
end;

function TSBaseScriptDebuggerFrm.IsExecuteable(ALine: Integer): Boolean;
begin
  Result := Assigned(DebugForm.FScript);
  Result := Result and DebugForm.FScript.IsExecutableLine(aLine);
end;

destructor TSBaseScriptDebuggerFrm.Destroy;
begin
  if Assigned(Owner) then
    with DebugForm do
      if FScriptDebuggerForm = Self then begin
        FScriptDebuggerForm := nil;
        FScript.OnRunLine := nil;
      end;

  inherited;
end;

procedure TSBaseScriptDebuggerFrm.SetActiveLine(ALine: Integer);
begin
  if SynEdit.Lines.Count >= ALine then begin
    if ALine > 0 then
      SynEdit.CaretY := ALine;
    FLine := ALine;
  end;
end;

procedure TSBaseScriptDebuggerFrm.SetBreakPoint(ALine: Integer);
var
  LIndex : Integer;
begin
  LIndex := FBreakPoints.IndexOf(Pointer(ALine));
  if LIndex < 0 then
    FBreakPoints.Add( Pointer(ALine) );
end;

procedure TSBaseScriptDebuggerFrm.SetErrorLine(ALine: Integer);
begin
  FErrorLine := ALine;
  if FErrorLine >= 0 then
    FPaused := True;
  ReshowButtons;
end;

procedure TSBaseScriptDebuggerFrm.ShowVariables(ACurrentScript: TfsScript);
var
  I         : Integer;
  LVariable : TfsCustomVariable;
begin
  VTableVariables.DisableControls;
  try
    VTableVariables.Active := True;
//    VTableVariables.Clear;

    if (not pcDebugger.Visible) or (pcDebugger.ActivePage <> tsVariables) then begin
      FNeewShowVariables := True;
      Exit;
    end;
    FNeewShowVariables := False;

    if Assigned(ACurrentScript) then begin
      for I := 0 to ACurrentScript.VariableCount-1 do begin
        LVariable := ACurrentScript.VariableByIndex(I);
        if (not LVariable.IsMacro) and (not LVariable.InheritsFrom(TfsProcVariable))
            and (LeftStr(LVariable.Name, 10) <> '_WithList_')
          // and (not (LVariable.Typ = fvtClass))
        then begin
          VTableVariables.Append;
            FeelVTableFromVariable(LVariable, VTableVariables, False);
          VTableVariables.Post;
        end;
      end;
    end;

  finally
    VTableVariables.EnableControls;
  end;
end;

procedure TSBaseScriptDebuggerFrm.ShowWatch(ACurrentScript : TfsScript);
begin
  VTableWatch.DisableControls;
  try
    VTableWatch.Active := True;

    if (not pcDebugger.Visible) or (pcDebugger.ActivePage <> tsWatch) then begin
      FNeewShowWatch := True;
      Exit;
    end;
    FNeewShowWatch := False;

    VTableWatch.First;
    while (not VTableWatch.Eof) do begin
      ReedWatchVariableInfo(ACurrentScript, VTableWatch);
      VTableWatch.Next;
    end;
  finally
    VTableWatch.EnableControls;
  end;
end;

procedure TSBaseScriptDebuggerFrm.SynEditGutterClick(Sender: TObject;
  Button: TMouseButton; X, Y, Line: Integer; Mark: TSynEditMark);
begin
  if IsBreakpointLine(Line) then
    ResetBreakPoint(Line)
  else
    SetBreakPoint(Line);
  SynEdit.Invalidate;
end;

procedure TSBaseScriptDebuggerFrm.SynEditGutterPaint(Sender: TObject; aLine, X, Y: Integer);
var
  LImageIndex : Integer;
begin
  LImageIndex := -1;
  if aLine = FErrorLine then
    LImageIndex := SBaseFormProperties.IconIndex_GutterError
  else if aLine = FLine then
    LImageIndex := SBaseFormProperties.IconIndex_GutterCurrent
  else if IsExecuteable(aLine) then
    LImageIndex := SBaseFormProperties.IconIndex_GutterExecutable;

  SBaseFormProperties.Icon16List.Draw(SynEdit.Canvas, X + 16, Y, LImageIndex);

  if IsBreakpointLine(aLine) then
    SBaseFormProperties.Icon16List.Draw(SynEdit.Canvas, X, Y, SBaseFormProperties.IconIndex_GutterBreakPoint);
end;

procedure TSBaseScriptDebuggerFrm.SynEditSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
begin
  inherited;
  if Line = FErrorLine then begin
    Special := True;
    FG := clWhite;
    BG := clRed;
  end else if Line = FLine then begin
    Special := True;
    FG := clHighlightText;
    BG := clHighlight;
  end else if FBreakPoints.IndexOf(Pointer(Line)) >= 0 then begin
    Special := True;
    FG := clInactiveCaptionText;
    BG := clInactiveCaption;
  end;
end;

procedure TSBaseScriptDebuggerFrm.SynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
begin
  if Changes * [scAll, scCaretX, scCaretY] <> [] then begin
    dxStatusBar.Panels[CPanelLine].Text := 'СТР: ' + IntToStr(SynEdit.CaretY);
    dxStatusBar.Panels[CPanelChar].Text := 'ПОЗ: ' + IntToStr(SynEdit.CaretX);
  end;
end;

procedure TSBaseScriptDebuggerFrm.VTableWatchBeforePost(DataSet: TDataSet);
begin
  inherited;
  ReedWatchVariableInfo(FCurrentScript, VTableWatch);
end;

procedure TSBaseScriptDebuggerFrm.WaitNextStep(ALine : Integer);
begin
  if FWaitNextStepLock then
    Exit;
  FWaitNextStepLock := True;
  DebugForm.FScript.OnRunLine := nil;
  try
    // -- теперь залипаем, если мы должны остановиться
    if FPaused or FGotoNextStep then begin
      SetActiveLine(ALine);

      FNeewShowVariables := True;
      FNeewShowWatch := True;
      CheckShowDebugValues;
      Self.BringToFront;
      repeat
        ReshowButtons;
        HandleMessage;
      until (not FPaused) or (FGotoNextStep);
      FGotoNextStep := False;
      FLine := -1;
    end;
  finally
    FWaitNextStepLock := False;
    DebugForm.FScript.OnRunLine := RunLine;
  end;
end;

procedure TSBaseScriptDebuggerFrm.ActionObjectInspectorExecute(Sender: TObject);
begin
  if DebugForm <> nil then
    DebugForm.ActionObjectInspector.Execute
  else
    inherited;
end;

procedure TSBaseScriptDebuggerFrm.CheckShowDebugValues;
begin
  if (pcDebugger.ActivePage = tsVariables) and (FNeewShowVariables) then
    ShowVariables(FCurrentScript)
  else if (pcDebugger.ActivePage = tsWatch) and (FNeewShowWatch) then
    ShowWatch(FCurrentScript);
end;

function TSBaseScriptDebuggerFrm.DebugForm: TSBaseFrmCrack;
begin
  Result := TSBaseFrmCrack(Owner)
end;

procedure TSBaseScriptDebuggerFrm.FeelVTableFromVariable(AVariable: TfsCustomVariable; AVDataSet: TDataSet; AForceFill: Boolean);

  procedure _CheckEdit;
  begin
    if not (AVDataSet.State in [dsEdit, dsInsert]) then
      AVDataSet.Edit;
  end;

begin
  if (not Assigned(AVariable)) and (not AForceFill) then
    Exit;

  _CheckEdit;

  if not Assigned(AVariable) then begin
    AVDataSet.FieldValues['VarType']      := Null;
    AVDataSet.FieldValues['VarClassType'] := Null;
    AVDataSet.FieldValues['VarValueStr']  := 'Имя не найдено!';
  end else begin
    if not VarIsEqual(AVDataSet.FieldValues['VarName'], AVariable.Name) then
      AVDataSet.FieldValues['VarName'] := AVariable.Name;

    AVDataSet.FieldValues['VarType']      := AVariable.TypeName;
    AVDataSet.FieldValues['VarClassType'] := AVariable.ClassName;

    if  (not AVariable.InheritsFrom(TfsProcVariable)) and (not (AVariable.Typ = fvtClass))
    then
      AVDataSet.FieldValues['VarValueStr'] := VarToStr( Variant(Pointer(AVariable.PValue)^) )
    else
      AVDataSet.FieldValues['VarValueStr'] := '...';
  end;
end;

procedure TSBaseScriptDebuggerFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    if FWaitNextStepLock then begin
      // Если попали сюда из цикла внутри WaitNextStep (там есть  HandleMessage)
      hsDialogs.HSShowInfo('Для закрытия отладчика необходимо перевести его в режим Run!');
      CanClose := False;
    end else
      inherited;
end;

procedure TSBaseScriptDebuggerFrm.FormDestroy(Sender: TObject);
begin
  VTableWatch.Close;

  FErrorLine := -1;
  FLine := -1;
  FreeAndNil(FBreakPoints);
  inherited;
end;

procedure TSBaseScriptDebuggerFrm.GridWatchDBTableViewDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  LVarName : String;
begin
  if Source = SynEdit then begin
    LVarName := SynEdit.SelText;
    if not LVarName.IsEmpty then
      VTableWatch.Append;
        VTableWatch.FieldValues['VarName'] := LVarName;
      VTableWatch.Post;

      if FWaitNextStepLock then
        ShowWatch(FCurrentScript)
      else
        FNeewShowWatch := True;
  end;
end;

procedure TSBaseScriptDebuggerFrm.GridWatchDBTableViewDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Source = SynEdit then
    Accept := True
  else
    Accept := False;
end;

procedure TSBaseScriptDebuggerFrm.ReedWatchVariableInfo(ACurrentScript: TfsScript; AVDataSet: TDataSet);

  function _SeekVariableByName(AScript: TfsScript; const AName: String): TfsCustomVariable;
  var
    I : Integer;
  begin
    if not Assigned(AScript) then
      Exit(nil);

    for I := 0 to AScript.VariableCount - 1 do begin
      Result := AScript.VariableByIndex(I);
      if Result.Name = AName then
        Exit;
    end;

    Result := _SeekVariableByName(AScript.Parent, AName)
  end;

var
  LVarName  : Variant;
  LVariable : TfsCustomVariable;
begin
  if FLockGetWatchVariableInfo then
    Exit;

  FLockGetWatchVariableInfo := True;
  try
    LVarName := VTableWatch.FieldValues['VarName'];
    if VarIsPresent(LVarName) and (not String(LVarName).IsEmpty) then begin
      LVariable := _SeekVariableByName(ACurrentScript, LVarName);
      FeelVTableFromVariable(LVariable, AVDataSet, True);
    end;
  finally
    FLockGetWatchVariableInfo := False;
  end;
end;

procedure TSBaseScriptDebuggerFrm.mnStepClick(Sender: TObject);
begin
  if FErrorLine >= 0 then begin
    FErrorLine := -1;
    FLine := -1;
    FPaused := True;
  end else
    FGotoNextStep := True;

  ReshowButtons;
  SynEdit.Invalidate;
end;

procedure TSBaseScriptDebuggerFrm.mnPauseClick(Sender: TObject);
begin
  FPaused := True;
  ReshowButtons;
end;

procedure TSBaseScriptDebuggerFrm.mnRunClick(Sender: TObject);
begin
  if FErrorLine >=0 then begin
    FErrorLine := -1;
    FLine := -1;
  end;
  FPaused := False;
  ReshowButtons;
  SynEdit.Invalidate;
end;

procedure TSBaseScriptDebuggerFrm.mnSetBreakpointClick(Sender: TObject);
var
  LLine : Integer;
begin
  LLine := SynEdit.CaretY;
  if IsBreakpointLine(LLine) then
    ResetBreakPoint(LLine)
  else
    SetBreakPoint(LLine);
end;

procedure TSBaseScriptDebuggerFrm.mnWatchClick(Sender: TObject);
begin
  pcDebugger.Visible := mnWatch.Down;
  splDebugger.Visible := pcDebugger.Visible;
  if pcDebugger.Visible then
    splDebugger.Top := pcDebugger.Top - splDebugger.Height;

  CheckShowDebugValues;
end;

procedure TSBaseScriptDebuggerFrm.pcDebuggerChange(Sender: TObject);
begin
  CheckShowDebugValues;
end;

procedure TSBaseScriptDebuggerFrm.ResetBreakPoint(ALine: Integer);
begin
  FBreakPoints.Remove(Pointer(ALine));
end;

procedure TSBaseScriptDebuggerFrm.ReshowButtons;
begin
  if FPaused and (not FGotoNextStep) then begin
    mnRun.Enabled := True;
    mnPause.Enabled := False;
    mnStep.Enabled := True;
    SynEdit.Color := clWindow;
  end else if FGotoNextStep then begin
    mnRun.Enabled := True;
    mnPause.Enabled := False;
    mnStep.Enabled := False;
    SynEdit.Color := clWindow;
  end else begin
    mnRun.Enabled := False;
    mnPause.Enabled := True;
    mnStep.Enabled := False;
    SynEdit.Color := clBtnFace;
  end;
end;

procedure TSBaseScriptDebuggerFrm.RunLine(AScript: TfsScript; const UnitName, SourcePos: String);
var
  LIndex        : Integer;
  LLine         : Integer;
begin
  if FRunLineLock then
    Exit;
  FRunLineLock := True;
  try
    LIndex := Pos(':', SourcePos);
    if LIndex > 0 then begin
      LLine := StrToIntDef(LeftStr(SourcePos, LIndex - 1), 0);
      if (not FPaused) and (FBreakPoints.IndexOf(Pointer(LLine)) >= 0) then
        FPaused := True;
    end else
      LLine := -1;

    // -- теперь залипаем, если мы должны остановиться
    FCurrentScript := AScript;
    try
      WaitNextStep(LLine);
    finally
      FCurrentScript := nil;
    end;
  finally
    FRunLineLock := False;
  end;
end;

end.
