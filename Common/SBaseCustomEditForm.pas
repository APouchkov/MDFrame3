unit SBaseCustomEditForm;
{$I config.inc}

{******************************************************************************}
{**  Базова форма редактирования                                             **}
{**  (c) None, 2006                                                         **}
{**  Автор: Соковнин Андрей                                                  **}
{**  Дата создания: 15.08.2006                                               **}
{******************************************************************************}

interface

uses
  Types, NamedVariables, StrUtils, DB,
  Windows, Messages, Generics.Collections, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  dxBar, System.Actions, LayoutPanel, SBaseForm, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  cxPCdxBarPopupMenu, cxPC, cxTextEdit, StdCtrls, cxButtons, ExtCtrls,
  cxGroupBox, ActnList, Menus, cxClasses, dxBarBuiltInMenu, SBaseControls,
  dxMDBar, dxSkinsCore, dxSkinsDefaultPainters
{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter
{$ENDIF}
  ;

type
  TSQLTransactionMode = (stmNone, stmPost, stmServer, stmClient);
  TFormMode = (fmView, fmEdit, fmNew, fmCopy);
//  TCaptions = Array[TFormMode] of String;

{$IFDEF REPORT_FROMEDIT}
  TPrintProperties = class;
{$ENDIF}

{$REGION 'TSBaseCustomEditFrm - Definition'}
  TSBaseCustomEditFrm = class(TSBaseFrm)
    PanelForm: TGroupPanel;
    PanelButtons: TGroupPanel;
    pcMain: TcxPageControl;
    tsMain: TcxTabSheet;
    pTsMain: TGroupPanel;
    btnOk: TcxButton;
    btnCancel: TcxButton;
    btnApply: TcxButton;
    btnPrint: TcxButton;
    ActionPgUp: TAction;
    ActionPgDown: TAction;
    PanelMain: TcxGroupBox;
    ViewID: TcxTextEdit;
    ActionApply: TAction;

    procedure btnApplyClick(Sender: TObject); dynamic;
    procedure btnOkClick(Sender: TObject); dynamic;
    procedure btnCancelClick(Sender: TObject); dynamic;

    procedure FormShow(Sender: TObject); override;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean); override;
    procedure ActionPageChangeExecute(Sender: TObject);
    procedure ActionApplyExecute(Sender: TObject);
  private
    FTransactionMode: TSQLTransactionMode;
    FFormMode: TFormMode;
    FCanClose : Boolean;
{$IFDEF REPORT_FROMEDIT}
    FPrintProperties: TPrintProperties;
{$ENDIF}
    FDefaultEditControl: TWinControl;

    function GetInserting: Boolean;
    function GetReadOnly: Boolean;
  protected
//    InternalCaption: TCaptions;

//    function GetCaptionFormatNew: string;
//    function GetCaptionFormatView: string;
//    procedure SetCaptionFormatNew(const Value: string);
//    procedure SetCaptionFormatView(const Value: string);
  protected
    procedure DoApply; dynamic; abstract;
    procedure DoCancel; dynamic;

    procedure InternalCreate; override;
{$IFDEF REPORT_FROMEDIT}
    procedure InternalInit; override;
{$ENDIF}
    procedure InternalPostInit; override;
    procedure InternalShow; override;
    procedure InternalPrepareParams; override;
    procedure InternalClose(AFinalization: Boolean); override;

  { -- Были ли изменения в данных --
    Например:
      Result := qTmp.Editing or qTmp.UpdatesPending
  }
    function  DataChanged: Boolean; virtual;
    function  CheckApplied: Boolean; dynamic;

  { -- Проверка корректности ввода --
    Например:
      if VarIsNull(EditDate.Value) then
        WarningMessage(CONTROL_MUST_HAVE_VALUE, EditDate)
  }
    procedure CheckValues; virtual;

  { -- Передача (сохранение) данных на сервер (или как захотите) --
    Например:
      if qTmp.Editing then qTmp.Post;
      qTmp.ApplyUpdates;
  }
    procedure BeforeApplyUpdates; virtual;
    procedure ApplyUpdates; virtual;

  { -- Клиентское подтверждение изменений. --
    Вызов этой процедуры означает что все данные были успешно сохранены, а транзакция (если она была)
    подтверждена. В этом блоке рекомендуется, при необходимости, вызывать оповещение WM_UPDATETABLE,
    а также, при AApplyMode = True (что означает что форма продолжит свою работу по окончанию всего блока
    Apply) необходимо всем датасетам SDAC режима CachedUpdates сделать CommitUpdates, переоткрыть или обновить
    какие-нибудь датасеты, а также заменить Caption формы (например при переходе Режима Inserting из True в False
    сменить надпись с "Добавление записи" на "Редактирование записи"; кстати только по выходу из процедуры
    CommitUpdates флаг Inserting установится в False (если он был True) автоматически)

    Например:
      if AApplyMode then begin
        qTmp.CommitUpdates;
        Caption := 'Редактирование записи "' + qTmpName.AsString + '"';
      end;
      SBaseMainFormMDIInterface.BroadCast(WM_UpdateTable, tbl_XXX, qTmpIdentity.AsInteger);
  }

    procedure CommitUpdates; virtual;
    procedure AfterCommitUpdates; virtual;

  { -- Клиентская отмена изменений --
    Необходимо, например, для возврата признаков UnApplied для записей SDAC-ДатаСетов:
      qTmp.RestoreUpdates;
  }
    procedure RestoreUpdates; virtual;
    procedure CancelUpdates; virtual;
  public
{$IFDEF REPORT_FROMEDIT}
    destructor Destroy; override;
{$ENDIF}
    procedure FillResultValues(const AResultFields: String); dynamic;
  published
  { Режим работы формы }
    property FormMode: TFormMode read FFormMode write FFormMode default fmEdit;

//    property CaptionFormatNew: string read GetCaptionFormatNew write SetCaptionFormatNew;
//    property CaptionFormatView: string read GetCaptionFormatView write SetCaptionFormatView;

  { Режим "Добавления записи" }
    property Inserting: Boolean read GetInserting default False;
    property ReadOnly: Boolean read GetReadOnly;

  { Режим сохранения "Без транзакции" / "Транзакция на сервере" / "Транзакция на клиенте" }
    property TransactionMode: TSQLTransactionMode read FTransactionMode write FTransactionMode default stmNone;
{$IFDEF REPORT_FROMEDIT}
    property PrintProperties: TPrintProperties read FPrintProperties;
{$ENDIF}

    property DefaultEditControl: TWinControl read FDefaultEditControl write FDefaultEditControl;
  end;
{$ENDREGION 'TSBaseCustomEditFrm - Definition'}

{$REGION 'TPrintProperties - Definition'}
{$IFDEF REPORT_FROMEDIT}
  TPrintProperties = class(TPersistent)
  strict private
    FPrintMenu: TdxBarPopupMenu;
    FSQLMenuController: TSBaseCustomSQLMenuController;
    [Weak] FOwner: TSBaseCustomEditFrm;

    function  GetSQL: TStrings;
    procedure SetSQL(AValue: TStrings);

    procedure PrintButtonClick(Sender: TObject);
    procedure PrintButtonDropDownMenuPopup(Sender: TObject; var APopupMenu: TComponent; var AHandled: Boolean);

//    procedure DoChange;
  protected
    procedure AssignTo(ADest: TPersistent); override;
  public
    constructor Create(AOwner: TSBaseCustomEditFrm);
    destructor  Destroy; override;

    procedure Update;
  published
    property SQL: TStrings read GetSQL write SetSQL;
  end;
{$ENDIF}
{$ENDREGION 'TPrintProperties - Definition'}

{$REGION 'FASTSCRIPT_RTTI - Definition'}
{$IFDEF FASTSCRIPT_RTTI}
  TfsFunctions_TSBaseCustomEditFrm = class(TfsRTTIModule)
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}
{$ENDREGION 'FASTSCRIPT_RTTI - Definition'}

implementation

uses
  SBaseVariantFunctions, SBaseDataModule, SBaseConstants,
{$IFDEF REPORT_FROMEDIT}
  SBaseReport,
{$ENDIF}
  SBaseUtils, SBaseFormDefinitions;

type
  TcxButtonCrack = class(TcxButton);

{$R *.dfm}

{$REGION 'TSBaseCustomEditFrm'}
procedure TSBaseCustomEditFrm.InternalCreate;
begin
  with btnPrint.OptionsImage do begin
    Images      := SBaseFormProperties.Icon16List;
    ImageIndex  := SBaseFormProperties.IconIndex_Printer;
  end;
  btnPrint.Caption := SBaseProperties.Text_MenuPrint;
{$IFDEF REPORT_FROMEDIT}
  FPrintProperties := TPrintProperties.Create(Self);
{$ENDIF}

  //FReadOnly := False;
  FFormMode := fmEdit;
  FTransactionMode := stmNone;

  inherited;

//  if InternalCaption[fmView] = '' then
//    InternalCaption[fmView] := CaptionFormat;
//
//  if InternalCaption[fmEdit] = '' then
//    InternalCaption[fmEdit] := CaptionFormat;
//
//  if InternalCaption[fmNew] = '' then
//    InternalCaption[fmNew] := CaptionFormat;
//
//  if InternalCaption[fmCopy] = '' then
//    InternalCaption[fmCopy] := CaptionFormat;
end;

{$IFDEF REPORT_FROMEDIT}
procedure TSBaseCustomEditFrm.InternalInit;
begin
  inherited;

  if FormMode in [fmView, fmEdit] then
    FPrintProperties.Update;
end;
{$ENDIF}

procedure TSBaseCustomEditFrm.InternalPostInit;
begin
  inherited;

  btnCancel.Visible := (Parent = nil) or (FormMode <> fmView);
  if btnCancel.Visible then
    btnCancel.Caption := IfThen((FormMode = fmView), SBaseProperties.Text_MenuClose, SBaseProperties.Text_MenuCancel);

  if btnApply.Parent = PanelButtons then
    if (FormMode = fmView) then begin
      btnApply.Visible  := (ActualPrivileges and Privilege_Update_Bit) <> 0;
      if btnApply.Visible then begin
        btnApply.Caption  := SBaseProperties.Text_StartEditing;
        btnApply.Enabled  := True;
        ActionApply.Enabled := False;
      end;
    end else begin
      btnApply.Visible  := not (TransactionMode in [stmNone, stmPost]);
      if btnApply.Visible then begin
        btnApply.Caption  := SBaseProperties.Text_MenuApply;
        ActionApply.Enabled := True;
      end;
    end;

  btnOk.Visible := (Parent = nil) and (FormMode <> fmView);
  if btnOk.Visible then
    btnOk.Caption := IfThen((TransactionMode in [stmNone, stmPost]), SBaseProperties.Text_MenuPost, SBaseProperties.Text_MenuSave);
end;

procedure TSBaseCustomEditFrm.InternalShow;
begin
  inherited;
  UpdateFormState;
end;

{$IFDEF REPORT_FROMEDIT}
destructor TSBaseCustomEditFrm.Destroy;
begin
  FreeAndNil(FPrintProperties);

  inherited;
end;
{$ENDIF}

procedure TSBaseCustomEditFrm.InternalClose(AFinalization: Boolean);
begin
  inherited;

  if (FormMode <> fmView) and (TransactionMode = stmPost) then
    DoCancel;
end;

//function TSBaseCustomEditFrm.GetCaptionFormatNew: string;
//begin
//  Result := InternalCaption[fmNew]
//end;
//
//function TSBaseCustomEditFrm.GetCaptionFormatView: string;
//begin
//  Result := InternalCaption[fmView]
//end;
//
//procedure TSBaseCustomEditFrm.SetCaptionFormatNew(const Value: string);
//begin
//  InternalCaption[fmNew] := Value;
//  InternalCaption[fmCopy] := Value;
//end;
//
//procedure TSBaseCustomEditFrm.SetCaptionFormatView(const Value: string);
//begin
//  InternalCaption[fmView] := Value;
//  InternalCaption[fmEdit] := Value;
//end;

procedure TSBaseCustomEditFrm.InternalPrepareParams;
var
  LModeString: String;
begin
  inherited;

  LModeString := VarToStr(CreateFormParams[SConst_Mode]);
  if SameText(LModeString, SConst_Insert) then
    FormMode := fmNew
  else if SameText(LModeString, SConst_Copy) then
    FormMode := fmCopy
  else if SameText(LModeString, SConst_Edit) then
    FormMode := fmEdit
  else if SameText(LModeString, SConst_View) then
    FormMode := fmView
  else begin
    Raise Exception.Create('Ошибочное значение параметра Mode = "' + LModeString + '"!');
  end;
end;

//procedure TSBaseCustomEditFrm.SetFormMode(const AMode: TFormMode);
//begin
//  if FFormMode <> AMode then begin
//    FFormMode := AMode;
//  end;
//end;

function TSBaseCustomEditFrm.GetInserting: Boolean;
begin
  Result := FFormMode in [fmNew, fmCopy]
end;

function TSBaseCustomEditFrm.GetReadOnly: Boolean;
begin
  Result := (FormMode = fmView)
end;

function TSBaseCustomEditFrm.DataChanged: Boolean;
begin
  Result := (FFormMode in [fmNew, fmCopy]){$IFDEF FASTSCRIPT_RTTI} or (CallFunction('DataChanged', False) = True){$ENDIF};
end;

function TSBaseCustomEditFrm.CheckApplied: Boolean;
begin
  Result := (not DataChanged);
  if (not Result) then
    if ConfirmMessage(SBaseProperties.Confirmation_ApplyRequired) then begin
      btnApply.Click;
      Result := True;
    end
end;

procedure TSBaseCustomEditFrm.CheckValues;
begin
{$IFDEF FASTSCRIPT_RTTI}
  CallFunction('CheckValues', False);
{$ENDIF}
end;

procedure TSBaseCustomEditFrm.BeforeApplyUpdates;
begin
{$IFDEF FASTSCRIPT_RTTI}
  CallFunction('BeforeApplyUpdates', False);
{$ENDIF}
end;

procedure TSBaseCustomEditFrm.ApplyUpdates;
begin
{$IFDEF FASTSCRIPT_RTTI}
  CallFunction('ApplyUpdates', False);
{$ENDIF}
end;

procedure TSBaseCustomEditFrm.CommitUpdates;
begin
{$IFDEF FASTSCRIPT_RTTI}
  CallFunction('CommitUpdates', False);
{$ENDIF}
end;

procedure TSBaseCustomEditFrm.AfterCommitUpdates;
begin
{$IFDEF FASTSCRIPT_RTTI}
  CallFunction('AfterCommitUpdates', False);
{$ENDIF}
end;

procedure TSBaseCustomEditFrm.RestoreUpdates;
begin
{$IFDEF FASTSCRIPT_RTTI}
  CallFunction('RestoreUpdates', False);
{$ENDIF}
end;

procedure TSBaseCustomEditFrm.CancelUpdates;
begin
{$IFDEF FASTSCRIPT_RTTI}
  CallFunction('CancelUpdates', False);
{$ENDIF}
end;

{ Процедура не вызывается из перекрытого класса Dh }
procedure TSBaseCustomEditFrm.DoCancel;
begin
  CancelUpdates;
end;

procedure TSBaseCustomEditFrm.btnApplyClick(Sender: TObject);
begin
//
end;

procedure TSBaseCustomEditFrm.btnOkClick(Sender: TObject);
begin
  if (FormMode = fmView) then
    Close
  else begin
    if (TransactionMode = stmPost) or DataChanged then
      DoApply;

    FCanClose := True;
    if fsModal in FormState then begin
      ResultValues.Clear;
      ModalResult := mrOK;
      if not ResultFields.IsEmpty then
        FillResultValues(ResultFields);
    end else
      Close;
  end;
end;

procedure TSBaseCustomEditFrm.btnCancelClick(Sender: TObject);
var
  LParams: TNamedVariants;
begin
  if (Parent = nil) or (FFormMode <> fmEdit) then begin
    ResultValues.Clear;
    Close;
    ModalResult := mrCancel;
  end else begin
    LParams := CreateFormParams;
    LParams.Values[SConst_Mode] := SConst_View;
    InternalReInit(LParams);
  end;
end;

procedure TSBaseCustomEditFrm.FormShow(Sender: TObject);
begin
  inherited;

  if (FormMode = fmView) then begin
    if (not btnOk.Focused) and btnOk.CanFocus then
      btnOk.SetFocus;
  end else if Assigned(FDefaultEditControl) and FDefaultEditControl.CanFocus and (FFormMode = fmEdit) then
    FDefaultEditControl.SetFocus
end;

procedure TSBaseCustomEditFrm.FillResultValues(const AResultFields: String);
var
  LResultFields: String;
begin
  ResultValues.WriteValues(AResultFields, Variables.ReadValues(AResultFields, LResultFields, True), True);
end;

procedure TSBaseCustomEditFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  LButton: TWinControl;
begin
  inherited;

  if CanClose and (not FCanClose) and (FormMode <> fmView) then try
    if {((TransactionMode <> stmPost) or (FormMode = fmNew)) and} DataChanged then begin
      if btnOk.Visible then
        LButton := btnOk
      else if btnApply.Visible then
        LButton := btnApply
      else
        LButton := nil;

      case TSBaseFrm.YesNoCancelMessage(MError_ConfirmSaveChanges, LButton) of
        mrYes: btnOk.Click;
        mrNo : FCanClose := True;
        else CanClose := False;
      end;
    end;
  except
    CanClose := False;
  end;
end;

procedure TSBaseCustomEditFrm.ActionApplyExecute(Sender: TObject);
begin
  if btnApply.Visible then
    btnApply.Click
end;

procedure TSBaseCustomEditFrm.ActionPageChangeExecute(Sender: TObject);
begin
  if IsControlActive(pcMain) then
    pcMain.SelectNextPage(Sender = ActionPgUp)
end;
{$ENDREGION 'TSBaseCustomEditFrm'}

{$REGION 'TPrintProperties'}
{$IFDEF REPORT_FROMEDIT}
{ TPrintProperties }

constructor TPrintProperties.Create(AOwner: TSBaseCustomEditFrm);
begin
  Assert(Assigned(AOwner));
  FOwner := AOwner;

  FPrintMenu := TdxBarPopupMenu.Create(AOwner.BarManager);
  FPrintMenu.BarManager := AOwner.BarManager;
  AOwner.btnPrint.DropDownMenu := FPrintMenu;

  FSQLMenuController := TSBaseCustomSQLMenuController.CustomCreate(AOwner);
  FSQLMenuController.MenuComponent := FPrintMenu;

  AOwner.btnPrint.OnClick := PrintButtonClick;
  AOwner.btnPrint.OnDropDownMenuPopup := PrintButtonDropDownMenuPopup;

  inherited Create;
end;

destructor TPrintProperties.Destroy;
begin
  FOwner.btnPrint.OnClick := nil;
  FOwner.btnPrint.OnDropDownMenuPopup := nil;

  FreeAndNil(FSQLMenuController);

  inherited;
end;

procedure TPrintProperties.AssignTo(ADest: TPersistent);
begin
  TPrintProperties(ADest).SQL := SQL;
end;

function TPrintProperties.GetSQL: TStrings;
begin
  Result := FSQLMenuController.SQL
end;

procedure TPrintProperties.SetSQL(AValue: TStrings);
begin
  FSQLMenuController.SQL := AValue;
end;

procedure TPrintProperties.PrintButtonClick(Sender: TObject);
begin
  if FOwner.CheckApplied and FOwner.btnPrint.Visible then begin
    if (FSQLMenuController.DefaultItem <> nil) then
      FSQLMenuController.DefaultItem.Click
    else if (FOwner.btnPrint.Kind = cxbkDropDown) then
      TcxButtonCrack(FOwner.btnPrint).DoDropDownMenu
  end;
end;

procedure TPrintProperties.PrintButtonDropDownMenuPopup(Sender: TObject; var APopupMenu: TComponent; var AHandled: Boolean);
begin
  if FOwner.CheckApplied and FOwner.btnPrint.Visible then begin
    if FSQLMenuController.ClickableCount = 1 then begin
      FSQLMenuController.DefaultItem.Click;
      AHandled := True;
    end;
  end else
    AHandled := True;
end;

procedure TPrintProperties.Update;
var
  LPrintButton: TcxButton;
begin
  LPrintButton := FOwner.btnPrint;

  FSQLMenuController.UnConfigure;
  if SQL.Count = 0 then begin
    LPrintButton.Visible := False;
    LPrintButton.Enabled := False;
    Exit;
  end;

  FSQLMenuController.Configure;
  if (not FSQLMenuController.Root.Enabled) then begin
    LPrintButton.Visible := False;
    LPrintButton.Enabled := False;
  end else begin
    if (FSQLMenuController.ClickableCount > 1) and (FSQLMenuController.DefaultItem <> nil) then
      LPrintButton.Kind := cxbkDropDownButton
    else
      LPrintButton.Kind := cxbkDropDown;

    LPrintButton.Enabled := True;
    LPrintButton.Visible := True;
  end;
end;
{$ENDIF}
{$ENDREGION 'TPrintProperties'}

{$REGION 'FASTSCRIPT_RTTI - Definition'}
{$IFDEF FASTSCRIPT_RTTI}
constructor TfsFunctions_TSBaseCustomEditFrm.Create(AScript: TfsScript);
begin
  AScript.AddEnum('TFormMode', 'fmView, fmEdit, fmNew, fmCopy');
end;
{$ENDIF}
{$ENDREGION 'FASTSCRIPT_RTTI - Definition'}

initialization
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_TSBaseCustomEditFrm);
{$ENDIF}

finalization
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TfsFunctions_TSBaseCustomEditFrm);
{$ENDIF}

end.
