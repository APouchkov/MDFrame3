{******************************************************************************}
{                              DataHandlerSupport                              }
{                                                                              }
{ - Логика работы с MSDataSet, ClientDataSet(ReadOnly)                         }
{                                                                              }
{******************************************************************************}

unit DataHandlerSupport;
{$I config.inc}

interface

uses
  DB, Classes, DataHandler
{$IFNDEF PACKAGE}
  , dxBar, SBaseFormDefinitions, SBaseForm, NamedVariables, Contnrs, Controls,
  DataHandlerGUI, Forms, SBaseDataModule, SBaseDataHelperInternal
{$ENDIF}
  ;

type
  EBaseDataHandlerSupport = class(EDataHandler);

  TBaseDataHandlerSupport = class(TCustomDataHandlerSupport)
{$IFNDEF PACKAGE}
  private
    FGUISupport : TBaseGUISupport;
    function GetCreateIntf: ISBaseInterface;
  protected
    property GUISupport: TBaseGUISupport read FGUISupport;
    function ShowAction(AActionProperties: TdhClassMenuActionProperties; ADHItems: TNamedVariants): TNamedVariants;
    function GUISupportClass: TGUISupportClass; virtual;
    function DhGridControl(ADataHandler: TCustomDataHandler): TdhGridControl;

    property CreateIntf: ISBaseInterface read GetCreateIntf;
  protected
    procedure InternalUnLockRecord(ADataHandler: TCustomDataHandler); virtual;
    procedure InternalCheckLockErrror(ADataHandler: TCustomDataHandler); virtual;
  protected
    procedure AssignTo(Dest: TPersistent); override;
   { parent abstract overrides }
    procedure UpdateReadOnly(ADataHandler: TCustomDataHandler); override;

    procedure DoPostEditValues(ADataHandler: TCustomDataHandler); override;
    procedure DoOnAfterCommit(ADataHandler: TCustomDataHandler); override;

    procedure DoNewObject(ADataHandler: TCustomDataHandler); override;

    procedure DoView(ADataHandler: TSubDataHandler; const AVariables : TNamedVariants); override;
    procedure DoNew(ADataHandler: TSubDataHandler; AItem: TdhMultiNewItem; var AVariables: TNamedVariants); override;
    procedure DoEdit(ADataHandler: TSubDataHandler; var AVariables: TNamedVariants); override;
    procedure DoCopy(ADataHandler: TSubDataHandler; var AVariables: TNamedVariants); override;

    procedure DoOpen(ADataHandler: TCustomDataHandler); override;
    procedure DoOnAfterOpen(ADataHandler: TCustomDataHandler); override;
    procedure DoOnBeforeClose(ADataHandler: TCustomDataHandler); override;
    procedure DoOnAfterClose(ADataHandler: TCustomDataHandler); override;
    procedure DoCancel(ADataHandler: TCustomDataHandler); override;
    procedure DoValidate(ADataHandler: TCustomDataHandler); override;
//    procedure DoOnRowChanged(ADataHandler: TCustomDataHandler; IsAfterScroll: Boolean); override;
    procedure DoOptionsMenuNewItemsChanged(ADataHandler: TCustomDataHandler); override;
//    procedure DoOptionsMenuChanged(ADataHandler: TCustomDataHandler); override;

    function  GetObjectValue(AName: string): Variant; override;
    function  DoShowMessage(const AText: string; AMode: TdhMessageMode; AControl: TObject = nil): TModalResult; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetSBaseForm: TSBaseFrm; inline;

    function GetGrid(ADataHandler: TCustomDataHandler): TComponent; override;
{$ENDIF}
  strict private
    FUpdatedObjectsFieldName: String;
  published
    property UpdatedObjectsFieldName: String read FUpdatedObjectsFieldName write FUpdatedObjectsFieldName;
  end;

implementation
{$IFNDEF PACKAGE}
uses
  SysUtils, Math, Variants, StrUtils, SBaseVariantFunctions,
  TypInfo, cxVariants, DBConsts, SBaseConstants,
  SBaseStringFunctions, SBaseReport, DataModule, SBaseDfmView,
  SBaseDataChanges, SBaseUtils, SBaseControls;
{$ENDIF}

{$IFNDEF PACKAGE}
type
  TCustomDataHandlerCrack = class(TCustomDataHandler);
  TSubDataHandlerCrack = class(TSubDataHandler);
  TSubDataHandlerXMLPostPropertiesCrack = class(TSubDataHandlerXMLPostProperties);

resourcestring
  SErrNoIntf = 'Класс %s не поддерживает ISBaseInterface';
  SErrClassNotSupported = 'Класс %s не поддерживается';

{ TBaseDataHandlerSupport }

procedure TBaseDataHandlerSupport.DoValidate(ADataHandler: TCustomDataHandler);
var
  I: Integer;
begin
  with ADataHandler.DataSet, ADataHandler.RequiredFields do
    for I := 0 to Count - 1 do begin
      with FieldByName(Items[I].FieldName) do
        if (FieldKind = fkData) and (not ReadOnly) and IsNullOrWhiteSpace then with Items[I] do begin
          DoShowMessage(SBaseProperties.Message_ThisFieldMustHaveAValue, mWarn, TControlRect.GetControl(Control, SubControlIndex));
          Break;
        end;
    end;
end;

procedure TBaseDataHandlerSupport.DoCancel(ADataHandler: TCustomDataHandler);
begin
  InternalUnLockRecord(ADataHandler);
  ADataHandler.DataSet.Cancel;
end;

procedure TBaseDataHandlerSupport.DoOnAfterCommit(ADataHandler: TCustomDataHandler);
begin
  if ADataHandler.IsRoot
        and (TDataHandler(ADataHandler).EditMode = dhemCommitable)
        and (not FUpdatedObjectsFieldName.IsEmpty)
  then begin
    SBaseMainFormMDIInterface.BroadCastDataChanges
    (
      TTablesChanges.Create
      (
        TNamedVariants.Create
        (
          ADataHandler.DataSet.FieldByName(FUpdatedObjectsFieldName).AsString,
          True
        )
      )
    );
  end;
end;

procedure TBaseDataHandlerSupport.DoOptionsMenuNewItemsChanged(ADataHandler: TCustomDataHandler);
var
  LGridControl: TdhGridControl;
begin
  if Assigned(FGUISupport) then begin
    LGridControl := FGUISupport.DhGridControl(ADataHandler);
    if Assigned(LGridControl) then
      with LGridControl do begin
        MenuHolder.CreateMultiNewButtons();
        //AssignPopupMenu;
      end;
  end;
end;

procedure TBaseDataHandlerSupport.AssignTo(Dest: TPersistent);
begin
  Assert(Dest is TBaseDataHandlerSupport);

  with TBaseDataHandlerSupport(Dest) do begin
    UpdatedObjectsFieldName := Self.UpdatedObjectsFieldName;
  end;
  inherited;
end;

constructor TBaseDataHandlerSupport.Create(AOwner: TComponent);
begin
  Assert(Assigned(AOwner) and (AOwner is TSBaseFrm));
  inherited;
  FGUISupport := GUISupportClass.Create(TForm(FOwner));
end;

function TBaseDataHandlerSupport.ShowAction(AActionProperties: TdhClassMenuActionProperties; ADHItems: TNamedVariants): TNamedVariants;

var
  LComponent    : TComponent;
  LIdx          : Integer;
  LResult       : TNamedVariants;
  LMemoryDataSet: TDataSet;
  LMemoryDataSetProvider: TSQLDataSetProvider absolute LMemoryDataSet;
  LActionMode   : TdhActionMode;
begin
  Assert(Assigned(AActionProperties.DataHandler));

  LResult.UnknownAsNull := False;
  LActionMode := AActionProperties.GetActionMode;

  if (AActionProperties.ActionKind = dhakView) and (LActionMode = amDataSet) then begin
    ADHItems[SConst_DataSet] := Integer(AActionProperties.DataHandler.DataSet);
  end else if (AActionProperties.ActionKind in [dhakView, dhakEdit, dhakCopy])
                                and (AActionProperties.ActionMode = amDataSetSelect)
  then
    Raise Exception.Create('Incorrect combination (ActionMode = amDataSetSelect) and (ActionKind in [dhakView, dhakEdit, dhakCopy])')
  else if LActionMode in [amDataSet, amDataSetSelect] then begin
    LMemoryDataSet := TSubDataHandlerCrack(AActionProperties.DataHandler).MemoryDataSet;
    if not Assigned(LMemoryDataSet) then begin
      LMemoryDataSetProvider := TSQLDataSetProvider.Create(AActionProperties.DataHandler);
      TSubDataHandlerCrack(AActionProperties.DataHandler).MemoryDataSet := LMemoryDataSet;
      LMemoryDataSetProvider.DataSetObject := bdsoMemory;
    end else begin
      Assert(LMemoryDataSet is TSQLDataSetProvider);
      LMemoryDataSetProvider.Close;
      LMemoryDataSetProvider.Fields.Clear;
      LMemoryDataSetProvider.FieldDefs.Clear;
    end;

    LMemoryDataSetProvider.FieldDefs.Assign(AActionProperties.DataHandler.DataSet.FieldDefs);
    LMemoryDataSetProvider.FieldsAutomations.BeginUpdate;
    try
      LMemoryDataSetProvider.FieldsAutomations.Assign(AActionProperties.DataHandler.DataSet.FieldsAutomations);
      LMemoryDataSetProvider.CachedUpdates := AActionProperties.DataHandler.DataSet.CachedUpdates;

      if (LActionMode = amDataSet) and Assigned(AActionProperties.DataHandler.OnMemoryPrepare) then
        AActionProperties.DataHandler.OnMemoryPrepare(AActionProperties.DataHandler, LMemoryDataSetProvider);

      LMemoryDataSetProvider.Open;
      if AActionProperties.ActionKind <> dhakNew then begin
        LMemoryDataSetProvider.BeforePost := nil;
        LMemoryDataSetProvider.Append;
          LMemoryDataSetProvider.LoadFields(AActionProperties.DataHandler.DataSet.Fields, lvcAlways);
        LMemoryDataSetProvider.Post;

        with AActionProperties.DataHandler.DataSet do if CachedUpdates and (not Inserted) then
          LMemoryDataSetProvider.ApplyUpdates;
      end else if (LActionMode = amDataSetSelect) then begin
        LMemoryDataSetProvider.Append;
          LMemoryDataSetProvider.LoadFields(ADHItems, lvcAlways);
      end;
    finally
      LMemoryDataSetProvider.FieldsAutomations.EndUpdate;
    end;

    LMemoryDataSetProvider.BeforePost := TSubDataHandlerCrack(AActionProperties.DataHandler).MemoryDataSetProviderBeforePost;
    if LActionMode = amDataSet then
      ADHItems[SConst_DataSet] := Integer(LMemoryDataSetProvider);
  end;

  //-- Для всех режимов, кроме Вставки (т.е. для режимов Просмотра, Редактирования и Копирования)
  //-- надо передать ключевые значения
  if (LActionMode in [amKeyFields, amKeyFieldsModal]) and (AActionProperties.ActionKind <> dhakNew) then
    ADHItems[AActionProperties.DataHandler.KeyFieldNames] := AActionProperties.DataHandler.KeyFieldValues;

  if LActionMode <> amDataSetSelect then
    case AActionProperties.ActionKind of
      dhakNew : ADHItems[SConst_Mode] := SConst_Insert;
      dhakView: ADHItems[SConst_Mode] := SConst_View;
      dhakEdit: ADHItems[SConst_Mode] := SConst_Edit;
      dhakCopy: ADHItems[SConst_Mode] := SConst_Copy;
    end;

  //-- Далее, собственно определяется класс и вызывается сама форма
  if ADHItems.Find(SBFSelectMode, LIdx) then
    Result := SBaseFormDefinitions.ISelectByParams(AActionProperties.GetClassName, Owner, ADHItems)
  else if (LActionMode = amDataSetSelect) then begin
    Result := SBaseFormDefinitions.ISelectByParams(AActionProperties.GetClassName, Owner, ADHItems);
    if Result.Count > 0 then begin
      LMemoryDataSetProvider.Fields.Load(Result, lvcIfReceive);
      LMemoryDataSetProvider.Post;
    end;
  end else begin
    LComponent := SBaseFormDefinitions.ICreateByParams(AActionProperties.GetClassName, Owner, ADHItems);
    if Assigned(LComponent) then
      if (LComponent is TForm) and (not (LActionMode in [amKeyFields])) then
        try
          if TForm(LComponent).ShowModal <> mrOk then
            Abort;
        finally
          LComponent.Free;
        end
      else
        (LComponent as ISBaseInterface).IShow;
  end;
end;

procedure TBaseDataHandlerSupport.InternalCheckLockErrror(ADataHandler: TCustomDataHandler);
var
  LDataSet        : TDataSet;
  LFieldLockId    : TField;
  LFieldLockError : TField;
  LErrorMessage   : String;
begin
  LDataSet := ADataHandler.DataSet;
  if (not ADataHandler.Enabled) or (not LDataSet.Active) or (ADataHandler.LockOptions.KeyFieldName.IsEmpty) then
    Exit;

  //-- Проверяем успешность блокировки в случае, если пришли на редактирование
  LFieldLockId := LDataSet.FindField(ADataHandler.LockOptions.KeyFieldName);
  if not Assigned(LFieldLockId) then
    Raise Exception.Create('В результируещем наборе данных не найдено поле "' + ADataHandler.LockOptions.KeyFieldName + '" (идентификатор блокировки)!');

  LFieldLockError := LDataSet.FindField(ADataHandler.LockOptions.MessageFieldName);
  if not Assigned(LFieldLockError) then
    Raise Exception.Create('В результируещем наборе данных не найдено поле "' + ADataHandler.LockOptions.KeyFieldName + '" (сообщение о блокировке)!');

  if (LFieldLockError.AsString <> '') then begin
    ADataHandler.InternalReadOnly := True;

    LErrorMessage := LFieldLockError.AsString;
    if Assigned(ADataHandler.OnLockError) then
      ADataHandler.OnLockError(ADataHandler, LErrorMessage);
  end;
end;

procedure TBaseDataHandlerSupport.InternalUnLockRecord(ADataHandler: TCustomDataHandler);
var
  LFieldLockId: TField;
begin
  if (not ADataHandler.Active) or (not ADataHandler.DataSet.Active) then
    Exit;

  if (not ADataHandler.LockOptions.KeyFieldName.IsEmpty) then begin
    LFieldLockId := ADataHandler.DataSet.FindField(ADataHandler.LockOptions.KeyFieldName);
    if Assigned(LFieldLockId) and (not LFieldLockId.IsNull) then begin
      //-- Сниманием блокировку, если она была успешно сформирована
      MainDataModule.UnLockRecord(LFieldLockId.AsLargeInt);
    end;
  end;
end;

procedure TBaseDataHandlerSupport.DoEdit(ADataHandler: TSubDataHandler; var AVariables: TNamedVariants);
begin
  if ADataHandler.OptionsMenu.OptionsEdit.GetActionMode = amInplace then begin
    ADataHandler.DataSet.Edit;
    AVariables.Clear;
  end else
    AVariables := ShowAction
              (
                ADataHandler.OptionsMenu.OptionsEdit,
                AVariables
              );
end;

procedure TBaseDataHandlerSupport.DoCopy(ADataHandler: TSubDataHandler; var AVariables: TNamedVariants);
var
  LValues: TNamedVariants;
begin
  if ADataHandler.OptionsMenu.OptionsCopy.GetActionMode = amInplace then begin
    LValues := TNamedVariants.Create(True);
    with ADataHandler.DataSet do begin
      Fields.Save(LValues, lvcAlways, ADataHandler.KeyFieldNames);
      Append;
      LoadFields(LValues, lvcIfNotPresent);
    end;
    AVariables.Clear;
  end else
    AVariables := ShowAction
              (
                ADataHandler.OptionsMenu.OptionsCopy,
                AVariables
              );
end;

procedure TBaseDataHandlerSupport.DoNew(ADataHandler: TSubDataHandler; AItem: TdhMultiNewItem; var AVariables: TNamedVariants);
var
//  LGridControl: TdhGridControl;
  LActionProperties: TdhClassMenuActionProperties;
  LDataSet    : TSQLDataSetProvider;
//  LClassName  : String;
begin
  if Assigned(AItem) then
    LActionProperties := AItem.Properties
  else
    LActionProperties := ADataHandler.OptionsMenu.OptionsNew;

  if LActionProperties.GetActionMode = amInplace then begin
    LDataSet := ADataHandler.DataSet;
    if LDataSet.State in dsEditModes then
      LDataSet.Post;

    LDataSet.DisableControls;
    try
      LDataSet.Append;
      LDataSet.LoadFields(AVariables, lvcIfReceive);
    finally
      LDataSet.EnableControls;
    end;
  end else begin
    AVariables := ShowAction
                  (
                   LActionProperties,
                   AVariables
                  );
  end;
end;

procedure TBaseDataHandlerSupport.DoNewObject(ADataHandler: TCustomDataHandler);
var
  LDataSet              : TDataSet;
  LNeedAppendRootRecord : Boolean;

  procedure _Backup(ADh: TCustomDataHandler);
  var
    i   : Integer;
    LDs : TDataSet;
  begin
    LDs := ADh.DataSet;
    LDs.Filtered := False;

    for i := 0 to ADh.Count-1 do
      _Backup(ADh.Children[i]);

    if (not LDs.IsEmpty) then
      ADh.DataSetBackup(False);
  end;

  procedure _Restore(ADh: TCustomDataHandler; ADoAppend: Boolean = False);
  var
    I             : Integer;
    LDs           : TDataSet;
//    LGridControl  : TdhGridControl;
  begin
    ADh.DataSetRestore(False);
    LDs := ADh.DataSet;

    if ADoAppend then
      LDs.Append;
    if LDs.Editing then
      LDs.Post;

    for I := 0 to ADh.Count-1 do
      _Restore(ADh.Children[i]);

//    LGridControl := DhGridControl(ADataHandler);
//    if Assigned(LGridControl) and LGridControl.NeedEmptyRec and ADataHandler.CachedUpdates then begin
//      LDs.Post;
//      LDs.Edit;
//    end;
  end;

begin
  LDataSet := ADataHandler.DataSet;
  if LDataSet.State in dsEditModes then
    LDataSet.Post;

  if ADataHandler.IsRoot then begin
    //-- Если из БД пришел(и) шаблон(ы), то его(их) копируем в себя в режиме Append
    ADataHandler.DisableControls(True);
    try
      _Backup(ADataHandler);
      LNeedAppendRootRecord := LDataSet.IsEmpty;
      ADataHandler.DataSetClear(True);
      _Restore(ADataHandler, LNeedAppendRootRecord);
    finally
      ADataHandler.EnableControls(True);
      ADataHandler.DataSetBackupClear(True);
    end;
  end;
end;


procedure TBaseDataHandlerSupport.DoView(ADataHandler: TSubDataHandler; const AVariables: TNamedVariants);
begin
  ShowAction
  (
    ADataHandler.OptionsMenu.OptionsView,
    AVariables
  )
end;

procedure TBaseDataHandlerSupport.DoOnAfterClose(ADataHandler: TCustomDataHandler);
begin
  if Assigned(GUISupport) then
    GUISupport.UpdateMenu(ADataHandler);
end;

procedure TBaseDataHandlerSupport.DoOnAfterOpen(ADataHandler: TCustomDataHandler);
var
  LdhGridControl: TdhGridControl;
begin
  if Assigned(GUISupport) then
    GUISupport.UpdateMenu(ADataHandler);

  LdhGridControl := DhGridControl(ADataHandler);
  if Assigned(LdhGridControl) and (LdhGridControl.Properties.GetWinControl <> nil) then
    LdhGridControl.Properties.GetWinControl.AccessGranted
end;

//procedure TBaseDataHandlerSupport.DoOnRowChanged(ADataHandler: TCustomDataHandler; IsAfterScroll: Boolean);
//begin
//  if (not ADataHandler.GetIsBusy) and Assigned(GUISupport) then
//    GUISupport.UpdateMenu(ADataHandler);
//end;

procedure TBaseDataHandlerSupport.DoOpen(ADataHandler: TCustomDataHandler);
var
  LdhGridControl: TdhGridControl;
  LDataSetProvider: TSQLDataSetProvider;
begin
  if ADataHandler.IsRoot and Assigned(GUISupport) then
    GUISupport.Init(ADataHandler as TDataHandler)
  else if not (ADataHandler.IsRoot) and TSubDataHandler(ADataHandler).Parent.DataSet.Active then
    TSubDataHandler(ADataHandler).AssignParams;

  LDataSetProvider := ADataHandler.DataSet;
  LdhGridControl := DhGridControl(ADataHandler);
  try
    LDataSetProvider.Open;
//    TCustomDataHandlerCrack(ADataHandler).SyncActive;
//    DSHelper(ADataHandler).Init;

    InternalCheckLockErrror(ADataHandler);
    ADataHandler.InternalReadOnly := False;

//    if Assigned(LdhGridControl) then
//      LdhGridControl.UpdateReadOnly;
  except on E:Exception do begin
    ADataHandler.InternalReadOnly := True;

    if ADataHandler.IsRoot or (not Assigned(LdhGridControl)) or (LdhGridControl.Properties.GetWinControl = nil) then
      Raise
    else if LdhGridControl.Properties.GetWinControl.AccessDenied(E.Message) then begin
      LdhGridControl.Properties.NoDataToDisplayInfoText := ''; //SBaseProperties.Message_ViewAccessDenied;
{$IFDEF DEBUG}
      SBaseMainFormMDIInterface.Log(lgError, E.Message);
{$ENDIF}
//      LdhGridControl.UpdateReadOnly;
    end else
      Raise;

    //TODO: CheckDenied в GUISupport(?)
  end end;
end;

function TBaseDataHandlerSupport.GUISupportClass: TGUISupportClass;
begin
  Result := TBaseGUISupport;
end;

procedure TBaseDataHandlerSupport.DoOnBeforeClose(ADataHandler: TCustomDataHandler);
begin
  if ADataHandler.IsRoot then InternalUnLockRecord(ADataHandler);
end;

function TBaseDataHandlerSupport.DoShowMessage(const AText: string; AMode: TdhMessageMode; AControl: TObject): TModalResult;
begin
  Result := mrOk;
  case AMode of
    mInfo : TSBaseFrm.InfoMessage(AText, AControl);
    mAsk  : Result := IfThen(TSBaseFrm.ConfirmMessage(AText, AControl), mrOk, mrCancel);
    mWarn : TSBaseFrm(nil).WarningMessage(AText, AControl);
    mErr  : TSBaseFrm(nil).ErrorMessage(AText, AControl);
    else
      Result := mrNone;
  end;
end;

function TBaseDataHandlerSupport.GetGrid(ADataHandler: TCustomDataHandler): TComponent;
begin
  if Assigned(FGUISupport) and Assigned(ADataHandler) then
    Result := FGUISupport.GetGrid(ADataHandler)
  else
    Result := nil;
end;

function TBaseDataHandlerSupport.DhGridControl(ADataHandler: TCustomDataHandler): TdhGridControl;
begin
  if Assigned(FGUISupport) then
    Result := FGUISupport.DhGridControl(ADataHandler)
  else
    Result := nil;
end;

procedure TBaseDataHandlerSupport.UpdateReadOnly(ADataHandler: TCustomDataHandler);
var
  LGridControl: TdhGridControl;
begin
  LGridControl := DhGridControl(ADataHandler);
  if Assigned(LGridControl) then
    LGridControl.UpdateReadOnly;
end;

procedure TBaseDataHandlerSupport.DoPostEditValues(ADataHandler: TCustomDataHandler);
var
  LDataSet, LParentDataSet: TDataSet;
  LField                  : TField;
  LDataSetProvider        : TSQLDataSetProvider absolute LDataSet;
  LXMLRows                : String;
  LDataSetStateBookmark   : TDataSourceStateBookmark;
  LDone                   : Boolean;

  LSubDataHandler: TSubDataHandler absolute ADataHandler;
begin
  LDataSet := ADataHandler.DataSet;
{$IFDEF DEBUG}
  Assert((LDataSet <> nil));
{$ENDIF}

  if LDataSet.Editing then
    LDataSet.Post;

  if ADataHandler is TSubDataHandler then
    with LSubDataHandler do
      if (not XMLPost.FieldName.IsEmpty) then begin
        LParentDataSet  := Parent.DataSet;
{$IFDEF DEBUG}
        Assert(Assigned(LParentDataSet));
{$ENDIF}
        if LParentDataSet.IsEmpty then begin
{$IFDEF DEBUG}
          Assert(LDataSet.IsEmpty, 'LDataSet.IsEmpty');
{$ENDIF}
          Exit;
        end;

        LField := LParentDataSet.FieldByName(XMLPost.FieldName);

        LXMLRows := '';
        LDone := XMLPost.OnlyIfModified
                    and
                    not
                    (
                      LDataSet.Editing
                      or
                      LDataSetProvider.UpdatesPending
                    );
        if Assigned(XMLPostOnBeforeApply) then
          XMLPostOnBeforeApply(LSubDataHandler, LXMLRows, LDone);

        if (not LDone) then begin
          LDataSet.BeginUpdate(LDataSetStateBookmark);
          try
            if XMLPost.Filter <> '' then begin
              LDataSet.Filter := XMLPost.Filter;
              LDataSet.Filtered := True;
            end else
              LDataSet.Filtered := False;

            if XMLPost.UpdateStatusFilter <> [] then
              LDataSetProvider.UpdateStatusFilter := XMLPost.UpdateStatusFilter;
            LDataSet.First;

            LXMLRows := DataSetToXML
                        (
                          LDataSet,
                          XMLPost.DataTag,
                          KeyFieldNames,
                          XMLPost.StatusAttributeName,
                          XMLPost.AllExcludedFields
                        );
          finally
            LDataSet.EndUpdate(LDataSetStateBookmark);
          end;
        end;

        if (not XMLPost.OnlyIfModified) or (Length(LXMLRows) > 0) then begin
          LParentDataSet.Edit;
          LField.AsString := LXMLRows;
        end else if (XMLPost.OnlyIfModified) and (not LField.IsNull) then begin
          LParentDataSet.Edit;
          LField.Clear;
        end;
      end;
end;

function TBaseDataHandlerSupport.GetObjectValue(AName: string): Variant;
begin
  Result := CreateIntf.CreateParams[AName];
end;

function TBaseDataHandlerSupport.GetSBaseForm: TSBaseFrm;
begin
  Result := TSBaseFrm(Owner);
end;

function TBaseDataHandlerSupport.GetCreateIntf: ISBaseInterface;
begin
  Supports(Owner, ISBaseInterface, Result);
  if not Assigned(Result) then
    raise EBaseDataHandlerSupport.CreateFmt(SErrNoIntf, [Owner.ClassName]);
end;

{$ENDIF}

initialization
  RegisterClasses([TBaseDataHandlerSupport]);
  TCustomDataHandlerSupport.RegisterClass(TBaseDataHandlerSupport);

finalization
//  TCustomDataHandlerSupport.UnRegisterClass(TBaseDataHandlerSupport);
  UnRegisterClasses([TBaseDataHandlerSupport]);

end.
