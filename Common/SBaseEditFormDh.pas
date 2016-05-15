{******************************************************************************}
{            Базовая Форма DB-редактирования с поддержкой DataHandler          }
{                                                                              }
{                              build 2008-12-12                                }
{******************************************************************************}
unit SBaseEditFormDh;
{$I config.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, SysConst, Generics.Collections,
  Dialogs, SBaseCustomEditForm, cxLookAndFeelPainters, cxPC, cxControls, StdCtrls, Mask, cxButtons,

{$IFDEF FASTSCRIPT_RTTI}
  fs_iinterpreter, fs_iclassesrtti, fs_ievents,
{$ENDIF}
  ExtCtrls, DB, NamedVariables, ActnList, DataHandler, DataHandlerSupport, dxBar,
  cxPropertiesStore, ItemPropStorage, SBaseDataModule, cxGraphics,
  cxContainer, cxEdit, cxTextEdit, cxGroupBox, cxLookAndFeels, cxPCdxBarPopupMenu,
  System.Actions, FieldDataLink, LayoutPanel, cxStyles, cxCustomData, cxFilter,
  cxData, cxDataStorage, cxNavigator, cxDBData, cxMemo, cxGridCustomTableView,
  cxGridTableView, cxGridDBTableView, cxGridLevel, cxClasses, cxGridCustomView,
  cxGrid, Menus, dxBarBuiltInMenu, dxMDBar, dxSkinsCore, dxSkinsDefaultPainters;

type
  //автоматическое поведение формы
  TBehaviorMode = (bmRefreshOnUpdate, bmNewIfInserted);
  TBehaviorModes = set of TBehaviorMode;

  TCaptionFieldInfo = packed record
    OnChangeEvent: TFieldNotifyEvent;
    Field: TField;
  end;

  TSBaseEditFrmDh = class(TSBaseCustomEditFrm)
    PropStorageDh   : TdhItemPropStorage;
    DataHandlerMain : TDataHandler;
    DataSetMain     : TSQLDataSetProvider;

    procedure btnApplyClick(Sender: TObject); override;

    procedure DataHandlerMainBeforeValidate(Sender: TCustomDataHandler; var ADone: Boolean);
    procedure DataHandlerMainBeforeApply(Sender: TCustomDataHandler; var ADone: Boolean);
    procedure DataHandlerMainBeforeRollBack(Sender: TCustomDataHandler; var ADone: Boolean);

    procedure DataHandlerMainBeforeCommit(Sender: TCustomDataHandler; var ADone: Boolean);
    procedure DataHandlerMainAfterCommit(Sender: TCustomDataHandler);

    procedure DataHandlerMainLockError(ADataHandler: TCustomDataHandler; const ALockErrorMessage: string);
    procedure DataHandlerChildLockError(ADataHandler: TCustomDataHandler; const ALockErrorMessage: String);
    procedure DataHandlerMainApplyUpdates(ADataHandler: TCustomDataHandler; var ADone: Boolean);
    procedure DataHandlerMainModifiedChanged(Sender: TObject);
  strict private
    FFieldDataLinks: TMDFieldDataLinks;
  protected
    {for backward compatibility only}
    procedure DoApply; override;
    procedure DoCancel; override;

    function DataChanged: Boolean; override;
  private
    FBehaviorModes: TBehaviorModes;
  private
    FExternalDataSet: TSQLDataSetProvider;

    procedure CopyData;
    procedure OnCaptionFieldChanged(Sender: TObject);

    function  ExternalDataUsed: Boolean; inline;
  protected
    procedure InternalCreate; override;
    procedure InternalPrepareParams; override;
    procedure InternalInit; override;
    procedure InternalPostInit; override;
    procedure InternalClose(AFinalization: Boolean); override;

    //-- В элемент ViewID пишем идентификатор записи. Также контролируем Visible (для вставки = False)
    procedure ReshowViewID; virtual;

    procedure GetCaptionParamByName(const AParamName: String; var ADisplayText: String; var AFound: Boolean); override;
  public
    destructor Destroy; override;

    procedure FillResultValues(const AResultFields: String); override;
  published
    property BehaviorModes: TBehaviorModes read FBehaviorModes write FBehaviorModes default [];
  end;

resourcestring
  SConst_UnCopiedFields   = 'UnCopiedFields';  //Строка вида 'q1.f1;q1.f2;q2.f1'

implementation

uses
  SBaseConstants, SBaseStringFunctions, SBaseVariantFunctions,
  SBaseForm, Math, StrUtils, Contnrs, DataModule, SBaseUtils;

{$R *.dfm}

{ TSBaseDBEditFrom }

procedure TSBaseEditFrmDh.InternalClose(AFinalization: Boolean);
begin
  inherited;

  if (not ExternalDataUsed) and Assigned(DataHandlerMain) then begin
    DataHandlerMain.Close;
    if AFinalization then
      DataHandlerMain.FullEnableControls;
  end;
end;

procedure TSBaseEditFrmDh.InternalCreate;
begin
  FBehaviorModes := [];

  inherited;

  if TransactionMode = stmNone then
    TransactionMode := stmServer;

  with PropStorageDh.DefaultImages, SBaseFormProperties do begin
    View      := IconIndex_View;
    New       := IconIndex_New;
    Edit      := IconIndex_Edit;
    Copy      := IconIndex_Copy;
    Delete    := IconIndex_Delete;
    Apply     := IconIndex_Post;
    Restore   := IconIndex_Restore;
    Refresh   := IconIndex_Refresh;
    Collapse  := IconIndex_CollapseAll;
    Expand    := IconIndex_ExpandAll;
  end;

  DataSetMain.ConnectionProvider := SQLConnectionProvider;
end;

destructor TSBaseEditFrmDh.Destroy;
begin
  FreeAndNil(FFieldDataLinks);
  inherited;
end;

procedure TSBaseEditFrmDh.InternalPostInit;
var
  LParamItem  : TStringParamItem;
  LPos        : Integer;
  LString     : String;
  LComponent  : TComponent;
  LDataSource : TDataSource absolute LComponent;
  LDataHandler: TCustomDataHandler;

  LReadOnly: Boolean;
  [Weak] LDataHandlerList: TDataHandlerList;
begin
  if DataHandlerMain.DataSetActive then begin
    if Assigned(FFieldDataLinks) then
      FFieldDataLinks.Clear
    else
      FFieldDataLinks := TMDFieldDataLinks.Create(True);

    for LParamItem in EnumStringParamItems(CaptionFormat, [epmSkipGap, epmFormat]) do begin
      if Assigned(DataHandlerMain.DataSet.FindField(LParamItem.Value)) then
        FFieldDataLinks.AddField(DataHandlerMain, LParamItem.Value)
      else if FindPos('.', LParamItem.Value, LPos) and FindComponent(LeftStr(LParamItem.Value, LPos - 1), TDataSource, LComponent) then begin
        LString := Copy(LParamItem.Value, LPos + 1, LParamItem.Value.Length - LPos);
        if Assigned(LDataSource.DataSet.FindField(LString)) then
          FFieldDataLinks.AddField(LDataSource, LString);
      end;
    end;

    if FFieldDataLinks.Count > 0 then
      FFieldDataLinks.OnDataChanged := OnCaptionFieldChanged;
  end;

  LReadOnly := (FormMode = fmView);

  LDataHandlerList := DataHandlerMain.Childrens;
  try
    for LDataHandler in LDataHandlerList do begin
      if LDataHandler.IsRoot or not (TSubDataHandler(LDataHandler).OptionsMenu.KeyFieldsMode) then
        LDataHandler.InternalReadOnly := LReadOnly;
      LDataHandler.ApplyReadOnly;
    end;
  finally
    LDataHandlerList.Free;
  end;

  inherited;

  if ExternalDataUsed then
    DataHandlerMain.Enabled := True
  else
    DataHandlerMain.EnableControls(True);

  ReshowViewID;
end;

procedure TSBaseEditFrmDh.InternalPrepareParams;
var
  LIdx: Integer;
begin
  inherited;

  if ResultFields.IsEmpty then
    ResultFields := DataHandlerMain.KeyFieldNames;

  if CreateFormParams.Find(SConst_DataSet, LIdx) then begin
    FExternalDataSet  := VarToPointer(CreateFormParams.Items[LIdx].Value);
    CreateFormParams.Delete(LIdx);
  end;
end;

procedure TSBaseEditFrmDh.InternalInit;
var
  I: Integer;
{$IFDEF DEBUG}
  LTimeCounter: TTimeCounter;
  LDataSetProviderMain: TSQLDataSetProvider;
{$ENDIF}
begin
  try
    if ExternalDataUsed then begin
      DataHandlerMain.Enabled := False;

      //-- Работаем на чужом DataHandler или DataSet
      TransactionMode := stmPost;
      DataHandlerMain.EditMode := dhemPostOnly;

{$IFDEF FASTSCRIPT_RTTI}
      if Assigned(FScript) then
        TfsComponentHelper(TfsComponentVariable(FScript.FindLocal(SConst_Form)).Find('DataSetMain')).Component := FExternalDataSet;
{$ENDIF}
      //ReplaceDataSetMain(FExternalDataSet);
      ReplaceDataSet(DataSetMain, FExternalDataSet);
      LDataSetProviderMain := FExternalDataSet;

      if FormMode = fmNew then begin
        //-- Если вызывали карточку для добавления
        LDataSetProviderMain.DisableControls;
        try
          LDataSetProviderMain.Append;
          //if bmInitFields in BehaviorModes then
          LDataSetProviderMain.LoadFields(CreateFormParams, lvcIfReceive);
        finally
          LDataSetProviderMain.EnableControls;
        end;
      end else begin
        if (bmNewIfInserted in BehaviorModes) and (FormMode = fmEdit) and (LDataSetProviderMain.Inserted) then begin
          //-- Если вызывали повторно дорекдактировать карточку еще не сохраненной записи
          FormMode := fmNew;
        end else if (bmRefreshOnUpdate in BehaviorModes) and (not LDataSetProviderMain.Modified) then
          //-- Перечитываем запись
          LDataSetProviderMain.RefreshRecord;
      end;
    end else begin
      LDataSetProviderMain := DataHandlerMain.DataSet;
      DataHandlerMain.DisableControls(True);
      DataHandlerMain.EditMode := dhemCommitable;
    end;

  //  InitUnboundEdits(Self);

    if (not ExternalDataUsed) then begin
      //-- Инициализируем параметры DataHandler-а
      LDataSetProviderMain.Params.Load(Variables, lvcAlways);
    end;

    if (ExternalDataUsed) then begin
//      DataHandlerMain.DisableControls;
//      try
//        FIsBusy := True;
//        DataHandlerMain.FIsOpening := True;

        for I := 0 to Pred(DataHandlerMain.Count) do begin
          DataHandlerMain.Children[I].Open;
        end;
//      finally
//        DataHandlerMain.EnableControls;
//        DataHandlerMain.FIsOpening := False;
//        DataHandlerMain.FIsBusy := False;
//      end;

    end else begin
      //-- Работаем со СВОИМИ DataHandler и DataSet
      try
        //-- Если идет повтороное переоткрытие карточки с открытым DataHandlerMain
        DataHandlerMain.Close;
{$IFDEF DEBUG}
        LTimeCounter.Start;
{$ENDIF}
        case FormMode of
          fmNew:
            begin
              //-- Режим Вставки
              if LDataSetProviderMain.State <> dsInsert then begin
                DataHandlerMain.NewObject;
                //if bmInitFields in BehaviorModes then
                with LDataSetProviderMain do begin
                  Edit;
                  LoadFields(CreateFormParams, lvcIfReceive);
                end;
              end;
            end;
          fmCopy:
            begin
              //-- Режим Копирования
              CopyData;
            end;
          fmView, fmEdit:
            begin
              //-- Режим Просмотра или Редактирования
              DataHandlerMain.Open;
            end;
        end;
{$IFDEF DEBUG}
        FDebugInfo.Add('Длительность открытия дерева DataHandlerMain: ' + LTimeCounter.Format(LTimeCounter.Interval, 0) + ' сек.');
{$ENDIF}
      except
        DataHandlerMain.Close;
        Raise;
      end;
    end;

    if (FormMode = fmView) then
      DataHandlerMain.EditMode := dhemReadOnly;

    if (FormMode = fmView) and (not ExternalDataUsed) then begin
      with DataSetMain.FieldsAutomations do
        if (not UpdateAbleFields.IsEmpty) or (not InitializeAbleFields.IsEmpty)
              or
              (
                (not ModifiAbleFieldsFieldName.IsEmpty)
                and
                (not DataSetMain.FieldByName(ModifiAbleFieldsFieldName).AsString.IsEmpty)
              )
        then begin
          ActualPrivileges := ActualPrivileges or Privilege_Update_Bit;
        end else if (ActualPrivileges or Privilege_Update_Bit) <> 0 then begin
          ActualPrivileges := ActualPrivileges xor Privilege_Update_Bit;
        end;
    end;

    inherited;

    if (not ExternalDataUsed) and (FormMode in [fmView, fmEdit]) and LDataSetProviderMain.IsEmpty then
      Raise Exception.Create(SError_RecordNoFound);

    DataHandlerMainModifiedChanged(DataHandlerMain);
  except
    if not (csLoading in ComponentState) then
      if ExternalDataUsed then
        DataHandlerMain.Enabled := True
      else
        DataHandlerMain.EnableControls(True);

    Raise;
  end;
end;

procedure TSBaseEditFrmDh.DoApply;
begin
  if (not btnOK.Focused) and (btnOK.CanFocus) then btnOK.SetFocus;

  DataHandlerMain.Validate;
  try
    if TransactionMode = stmClient then SQLConnectionProvider.BeginTran;
{ }
    DataHandlerMain.Apply;
{ }
    if TransactionMode = stmClient then SQLConnectionProvider.CommitTran;
  except on E:Exception do begin
    try
      if TransactionMode in [stmClient, stmServer] then
        SQLConnectionProvider.RollbackTran(True);
    except
    end;

    try
      DataHandlerMain.RollBack
    except
    end;

    if E.Message <> SOperationAborted then
      ErrorMessage(E.Message, btnOk)
    else
      Raise;
  end end;

  try
    DataHandlerMain.Commit;
  except
  end;
end;

procedure TSBaseEditFrmDh.DoCancel;
begin
  inherited;
  DataHandlerMain.Cancel
end;

procedure TSBaseEditFrmDh.ReshowViewID;
var
  LKeyFieldNames  : String;
  LKeyValues      : Variant;
  LVisible        : Boolean;
begin
  LVisible := ViewID.Enabled;
  try
    if ExternalDataUsed or Inserting or (not DataHandlerMain.DataSetActive) then begin
      LVisible := False;
    end else if LVisible then begin
      LKeyFieldNames := DataHandlerMain.DataSet.KeyFieldNames;
      if LKeyFieldNames.IsEmpty then
        LKeyFieldNames := DataHandlerMain.KeyFieldNames;

      if LKeyFieldNames.IsEmpty then
        LVisible := False
      else begin
        LKeyValues := DataHandlerMain.DataSet.FieldValues[LKeyFieldNames];
        ViewID.Text := VarArrayToString(LKeyValues, ';');
      end;
    end;
  finally
    ViewID.Visible := LVisible;
  end;
end;

procedure TSBaseEditFrmDh.DataHandlerMainBeforeApply(Sender: TCustomDataHandler; var ADone: Boolean);
begin
  BeforeApplyUpdates;
end;

procedure TSBaseEditFrmDh.DataHandlerMainBeforeCommit(Sender: TCustomDataHandler; var ADone: Boolean);
begin
  CommitUpdates;
end;

procedure TSBaseEditFrmDh.DataHandlerMainBeforeRollBack(Sender: TCustomDataHandler; var ADone: Boolean);
begin
  RestoreUpdates;
end;

procedure TSBaseEditFrmDh.DataHandlerMainBeforeValidate(Sender: TCustomDataHandler; var ADone: Boolean);
begin
  CheckValues;
end;

procedure TSBaseEditFrmDh.DataHandlerMainLockError(ADataHandler: TCustomDataHandler; const ALockErrorMessage: string);
var
  LParam: TParam;
  LFormMode: TFormMode;
begin
  LFormMode := FormMode;

  ADataHandler.ReadOnly := True;
  FormMode := fmView;
  CreateFormParams[SConst_Mode] := SConst_View;
  LParam := DataSetMain.Params.FindParam(SConst_Mode);
  if Assigned(LParam) then
    LParam.AsString := SConst_View;

  if LFormMode in [fmNew, fmCopy] then
    ErrorMessage(ALockErrorMessage, Self)
  else
    InfoMessage(ALockErrorMessage, Self);
end;

procedure TSBaseEditFrmDh.DataHandlerChildLockError(ADataHandler: TCustomDataHandler; const ALockErrorMessage: String);
begin
  ADataHandler.InternalReadOnly := True;
  InfoMessage(ALockErrorMessage, Self);
end;

procedure TSBaseEditFrmDh.DataHandlerMainModifiedChanged(Sender: TObject);
begin
  btnApply.Enabled := DataHandlerMain.Modified;
  btnOk.Enabled := btnApply.Enabled;
end;

{$WARN SYMBOL_DEPRECATED ON}

//procedure TSBaseEditFrmDh.InitUnboundEdits(AWinControl: TWinControl);
//var
//  i: Integer;
//  AControl: TControl;
//begin
//  for i := 0 to Pred(AWinControl.ControlCount) do begin
//    AControl := AWinControl.Controls[i];
//
//    if AControl is TWinControl then
//      InitUnboundEdits(TWinControl(AControl))
//  end;
//end;

function TSBaseEditFrmDh.DataChanged: Boolean;
begin
  Result := (inherited DataChanged) or (Assigned(DataHandlerMain) and DataHandlerMain.IsDataChanged);
end;

procedure TSBaseEditFrmDh.btnApplyClick(Sender: TObject);
var
  LFocusedControl: TWinControl;
begin
  if FormMode = fmView then begin
    InternalReInit
    (
        CreateFormParams.AddValues
        (
          TNamedVariants.Create([TNamedVariant.Create(SConst_Mode, SConst_Edit)]),
          amOverride
        )
    );
  end else begin
    LFocusedControl := ActiveControl;
{ }
    DataHandlerMain.ApplyUpdates;
{ }
    if Assigned(LFocusedControl) and not (csDestroying in LFocusedControl.ComponentState) and LFocusedControl.CanFocus then
      LFocusedControl.SetFocus;
  end;
end;

procedure TSBaseEditFrmDh.GetCaptionParamByName(const AParamName: String; var ADisplayText: String; var AFound: Boolean);
begin
  if SameText(AParamName, SConst_Mode) then begin
    case FormMode of
      fmView: ADisplayText := SBaseProperties.Text_Browsing;
      fmNew : ADisplayText := SBaseProperties.Text_Appending;
      fmCopy: ADisplayText := SBaseProperties.Text_Copying;
      fmEdit: ADisplayText := SBaseProperties.Text_Editing;
    end;

    AFound := True;
  end else
    inherited;
end;

function TSBaseEditFrmDh.ExternalDataUsed: Boolean;
begin
  Result := Assigned(FExternalDataSet)
end;

procedure TSBaseEditFrmDh.FillResultValues(const AResultFields: String);
var
  LResultFields: String;
begin
  ResultValues.WriteValues(ResultFields, DataSetMain.Fields.GetValues(ResultFields, @LResultFields), True);
  if not LResultFields.IsEmpty then
    inherited FillResultValues(LResultFields);
end;

procedure TSBaseEditFrmDh.OnCaptionFieldChanged(Sender: TObject);
begin
  UpdateCaption;
end;

procedure TSBaseEditFrmDh.CopyData;
var
  LRecCount : Integer;
begin
  DataHandlerMain.Open;
  LRecCount := DataHandlerMain.DataSet.RecordCount;
  try
    DataHandlerMain.DataSetBackup(True);
    DataHandlerMain.DataSetClear(True);
    if (LRecCount <= 0) then begin
      //-- Если в режиме Copy процедура чтения не вернула шаблона главной записи,
      //-- то формируем главную запись
      DataHandlerMain.DataSet.Append;
//      if bmInitFields in BehaviorModes then
      DataHandlerMain.DataSet.Fields.Load(CreateFormParams, lvcIfReceive);
    end else begin
      DataHandlerMain.DataSetRestore(True);
    end;
  finally
    DataHandlerMain.DataSetBackupClear(True);
  end;
end;

procedure TSBaseEditFrmDh.DataHandlerMainAfterCommit(Sender: TCustomDataHandler);
begin
  if (not ExternalDataUsed) and (fsModal in FormState) and (not ResultFields.IsEmpty) then
    ResultValues.WriteValues(ResultFields, DataSetMain.Fields.GetValues(ResultFields), True);

  AfterCommitUpdates;
end;

procedure TSBaseEditFrmDh.DataHandlerMainApplyUpdates(ADataHandler: TCustomDataHandler; var ADone: Boolean);
var
  LParams: TNamedVariants;
begin
  Assert(not ExternalDataUsed);

  if DataChanged then begin
    //Perform(WM_SETREDRAW, 0, 0);
    LockMessages;
    LockWindowUpdate(Handle);
    try
      DoApply;

      LParams := CreateFormParams;
      //-- Меняем режим на Редактирование
      LParams.Values[SConst_Mode] := SConst_Edit;
      //-- Добавляем в параметры Key-значения
      LParams.Values[DataHandlerMain.KeyFieldNames] := DataHandlerMain.KeyFieldValues;

      //-- Переоткрытие данных
      DataHandlerMain.DisableControls(True);
      try
        InternalReInit(LParams);
      finally
        DataHandlerMain.EnableControls(True);
      end;
    finally
      LockWindowUpdate(0);
      //Perform(WM_SETREDRAW, 1, 0); Refresh;
      PerformMessages;
    end;
  end;

  ADone := True;
end;

initialization
  RegisterClass(TSBaseEditFrmDh);

finalization
  UnRegisterClass(TSBaseEditFrmDh);

end.
