unit SBaseCxTreeViewImportForm;
{$I config.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Graphics, Controls, Forms, Dialogs, Math, Types, Data.DB,
  ExtCtrls, System.Actions, ActnList, Buttons, StdCtrls,

  cxGraphics, cxImageComboBox, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxStyles, cxCustomData, cxFilter, cxData, cxDataStorage,
  cxNavigator, cxDBData, cxTL, cxTextEdit, cxMaskEdit, cxTLdxBarBuiltInMenu,
  cxClasses, cxInplaceContainer, dxBar, cxDBTL, cxTLData, cxSplitter,
  cxGridLevel, cxGridBandedTableView, cxGridDBBandedTableView, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, cxGroupBox,
  dxStatusBar, cxPCdxBarPopupMenu, cxPC, dxMDBarButton,

  SBaseUtils, SBaseDataModule, SBaseFormDefinitions, SBaseForm, SBaseXXViewForm,
  SBaseCxViewForm, SBaseCxTreeViewForm, SBaseReportTabControl,

  fs_iinterpreter, dxBarBuiltInMenu, cxMDGrid, dxMDStatusBar, cxMDDBTreeList,
  dxSkinsCore, dxSkinsDefaultPainters;

type
  TSBaseCxTreeViewImportFrm = class(TSBaseCxViewFrm)
    PanelLeft: TcxGroupBox;
    cxTree: TcxMDTreeList;
    cxTreeColumnName: TcxTreeListColumn;

    SplitterLeft: TSplitter;
    TreeSetProvider: TSQLDataSetProvider;

    ReportTabControl: TSBaseReportTabControl;
    PopupMenuTree: TdxBarPopupMenu;
    mnAddFileOrMask: TdxBarSubItem;
    mnDeleteFileOrMask: TdxBarButton;
    DataSetSources: TSQLDataSetProvider;
    cxTreeColumnMask: TcxTreeListColumn;
    cxTreeColumnSource: TcxTreeListColumn;
    TreeSetProviderSource: TIntegerField;
    TreeSetProviderParent: TIntegerField;
    TreeSetProviderName: TWideStringField;
    TreeSetProviderMethod: TStringField;
    TreeSetProviderTemplate_Id: TWordField;
    TreeSetProviderTemplateType_Id: TWordField;
    TreeSetProviderImageIndex: TIntegerField;
    TreeSetProviderParams: TWideMemoField;
    cxTreeColumnFile: TcxTreeListColumn;
    mnAddFile: TdxBarButton;
    mnAddMask: TdxBarButton;
    mnSetFileOrMask: TdxBarSubItem;
    mnRefreshTree: TdxBarButton;
    mnSetMask: TdxBarButton;
    mnSetFile: TdxBarButton;
    TypesSetProvider: TSQLDataSetProvider;
    DataSetSourcesIndex: TIntegerField;
    DataSetSourcesPath: TStringField;
    DataSetSourcesParams: TWideMemoField;
    DataSourceSources: TDataSource;
    DataSetSourcesSource: TIntegerField;
    cxTreeColumnMethod: TcxTreeListColumn;
    cxTreeColumnTemplate_Id: TcxTreeListColumn;
    mnAddFolder: TdxBarButton;
    mnSetFolder: TdxBarButton;
    TreeSetProviderMask: TWideStringField;

    procedure DataSetImportAfterInsert(ADataSet: TDataSet);
    procedure flDateTimeOnFilterValueChanged(Sender: TObject);
    procedure TreeSetProviderAfterOpen(DataSet: TDataSet);

    procedure PopupMenuTreePopup(Sender: TObject);
    procedure mnAddFileClick(Sender: TObject);
    procedure mnSetFileClick(Sender: TObject);
    procedure mnAddMaskClick(Sender: TObject);
    procedure mnSetMaskClick(Sender: TObject);

    procedure mnRefreshTreeClick(Sender: TObject);
    procedure mnDeleteFileOrMaskClick(Sender: TObject);
  private
    { Private declarations }
    FAllOrNone: Boolean;
    FServerImportSQL: String;

//    FSource_NewIndex: Integer;
    FImportIndex: Integer;
    FDataSets: TArray<TSQLDataSetProvider>;

//    function NewId: Integer;
    procedure FillCustom(ANode: TcxTreeListNode);

    function  GetFileImageIndex(const AFile: String; ADefault: Integer): Integer;
    function  GetFileNameByMask(const AMask: String; var AFile, AName: String): Boolean;

//    procedure UpdateFileNameByMask(ANode: TcxTreeListNode);
//    procedure UpdateFileNamesByMask;
  protected
    procedure InternalCreate; override;
    procedure InternalInit; override;
    procedure SaveNodeConfig(ANode: TcxTreeListNode);
    procedure SaveUserConfig(AStoreFormSettings: TFormItemsForSave); override;

    procedure InternalImpot;
    procedure SetFile(ANew: Boolean; AFolderOnly: Boolean);
    procedure SetMask(ANew: Boolean);
  public
    { Public declarations }
    procedure AddDataSet(ADataSet: TSQLDataSetProvider);
  published
    flDateTime: TSBaseViewFilterDate;

    procedure DataSetAfterOpen(DataSet: TDataSet); override;
    procedure DataSetBeforeOpen(DataSet: TDataSet); override;

    property AllOrNone: Boolean read FAllOrNone write FAllOrNone default False;
    property ServerImportSQL: String read FServerImportSQL write FServerImportSQL;
  end;

  TfsFunctions_SBaseCxTreeViewImportForm = class(TfsRTTIModule)
  private
    function Call_TSBaseCxTreeViewImportFrm_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

implementation

{$R *.dfm}

uses
  SBaseConstants, SBaseDataHelperInternal, SBaseVariantFunctions, SBaseFileUtils,
  SBaseStringFunctions, SBaseImport, StrUtils, SBaseFieldEditForm, NamedVariables;

const
  Format_ParamSources     = 'Sources[%d:%d]';
  Format_ParamSourceItems = 'Sources[%d:%d].Items[%d]';

type
  TcxTreeListNodeCrack = class(TcxTreeListNode);

{ TSBaseCxTreeViewCompareFrm }

procedure TSBaseCxTreeViewImportFrm.InternalCreate;
begin
//  SetLength(FDataSets, 0);

  TreeSetProvider.ConnectionProvider := SQLConnectionProvider;

  TypesSetProvider.ConnectionProvider := SQLConnectionProvider;
  TypesSetProvider.SQL.Text := SBaseImportProperties.SQLTypes;

  cxTree.Root.CheckGroupType := ncgCheckGroup;
  cxTree.Images := SBaseFormProperties.Icon16List;
//  SBaseFormProperties.EditRepository_FileExtensions;
  inherited;

  DataSetSources.ConnectionProvider := SQLConnectionProvider;

  mnAddFileOrMask.Caption     := SBaseProperties.Text_MenuNew;
  mnAddFileOrMask.ImageIndex  := SBaseFormProperties.IconIndex_New;

  mnAddMask.Caption := SBaseProperties.Text_SubMenuMask;
  mnAddFile.Caption := SBaseProperties.Text_SubMenuFile;
  mnAddFolder.Caption := SBaseProperties.Text_SubMenuFolder;

  mnSetFileOrMask.Caption     := SBaseProperties.Text_MenuEdit;
  mnSetFileOrMask.ImageIndex  := SBaseFormProperties.IconIndex_Edit;
  mnSetMask.Caption := SBaseProperties.Text_SubMenuMask;
  mnSetFile.Caption := SBaseProperties.Text_SubMenuFile;
  mnSetFolder.Caption := SBaseProperties.Text_SubMenuFolder;

  mnRefreshTree.Caption := SBaseProperties.Text_MenuRefresh;
  mnRefreshTree.ImageIndex := SBaseFormProperties.IconIndex_Refresh;

  mnDeleteFileOrMask.Caption := SBaseProperties.Text_MenuDelete;
  mnDeleteFileOrMask.ImageIndex := SBaseFormProperties.IconIndex_Delete;

  DataSetSources.ConnectionProvider := SQLConnectionProvider;

//  DataSetImport.ConnectionProvider  := SQLConnectionProvider;
//  DataSetImport.AfterInsert := DataSetImportAfterInsert;

  if Assigned(flDateTime) then
    flDateTime.OnFilterValueChanged := flDateTimeOnFilterValueChanged;
end;

procedure TSBaseCxTreeViewImportFrm.InternalInit;
begin
  TypesSetProvider.Open;
  inherited;
  ReportTabControl.Active := True;
end;

procedure TSBaseCxTreeViewImportFrm.InternalImpot;
var
//  LFileImport: TSBaseImport;
  LFilterItems, LOutParams: TNamedVariants;
  LSuccCount, LErrorCount: Integer;

  function ProcessNode(ANode: TcxTreeListNode; ACheckOnly: Boolean): Boolean;
  var
    I: Integer;
    LTemplate_Id: Integer;
    LFileName, LPath: String;
    LInParams, LParams: TNamedVariants;

    procedure SaveSource(ASource: Integer);
    begin
      DataSetSources.Append;
        DataSetSourcesIndex.AsInteger := FImportIndex;
        DataSetSourcesSource.AsInteger:= ASource;
        DataSetSourcesPath.AsString   := LPath;
        DataSetSourcesParams.AsString := (LInParams + LOutParams).AsClrTParams;
      DataSetSources.Post;
    end;

    function RunLocalImport(ASource: Integer; INode: TcxTreeListNode): Boolean;
    begin
      LPath := cxTreeColumnName.Values[INode];
      try
        Inc(FImportIndex);
        LParams := LFilterItems;
        LParams[SConst_Index]   := FImportIndex;
        LParams[SConst_Source]  := ASource;

        LOutParams :=
          ExecSQL
          (
            FServerImportSQL,
            LParams,
            GetFormParamValues
          );

        SaveSource(ASource);
        ReportTabControl.AddNewLine
        (
          StringReplace(Format(SBaseProperties.MessageFormat_DataLoadedFromSource, [LPath]), '\', '\\', [rfReplaceAll, rfIgnoreCase])
        );

        Result := False;
      except on E:Exception do begin
        Result := True;
        Inc(LErrorCount);

        ReportTabControl.AddNewLine
        (
          StringReplace(Format(SBaseProperties.MessageFormat_ErrorDataLoadFromSource, [LPath, E.Message]), '\', '\\', [rfReplaceAll, rfIgnoreCase])
        );
      end end;
    end;

    function RunImport(ASource: Integer; INode: TcxTreeListNode; AFileName: String = ''): Boolean;
    var
      LFormat: String;
    begin
      LOutParams.Clear;

      try
        Inc(FImportIndex);
        LParams[SConst_Index]   := FImportIndex;
        LParams[SConst_Source]  := ASource;

        //LParams := (LFilterItems + LTemplateParams);

        LParams[SImpPrefix + SConst_SQLConnectionProvider]:= Integer(SQLConnectionProvider);
        LParams[SImpPrefix + SImpParam_TemplateId]        := LTemplate_Id;
        LParams[SImpPrefix + SConst_FileName]             := AFileName;
        LParams[SImpPrefix + SConst_DataSets]             := VarFromArrayOfObjects(TArray<TObject>(FDataSets));
     // LParams[SImpPrefix + SImpParam_DataSetPrepare]    := VarFromArrayReplicate('EMPTY', Length(FDataSets));
        LParams[SImpPrefix + SImpParam_DataSetDump]       := VarFromArrayReplicate('POST', Length(FDataSets));
        LParams[SImpPrefix + SImpParam_DataSetFinal]      := VarFromArrayReplicate('EMPTY', Length(FDataSets));
        LParams[SImpPrefix + SConst_Thread]               := True;
        LParams[SImpPrefix + SConst_ShowProgress]         := False;
        LParams[SImpPrefix + SConst_FreeOnTerminate]      := True;
        LParams[SImpPrefix + SConst_Output]               := Integer(@LOutParams);

        if Assigned(flDateTime) then begin
          LParams[SImpPrefix + SImpParam_TemplateDate]  := VarFromDate(flDateTime.ParamValue);
       // LParams[SConst_Date]                          := VarFromDate(flDateTime.ParamValue);
          LParams[flDateTime.ParamName]                 := flDateTime.ParamValue;
        end;

        TSBaseImport(TSBaseImport.CreateByParams(Self, LParams)).RunByParams;

        if AFileName.IsEmpty then begin
          LFormat := SBaseProperties.MessageFormat_DataLoadedFromSource;
          LPath := cxTreeColumnName.Values[INode];
        end else begin
          LFormat := SBaseProperties.MessageFormat_DataLoadedFromFile;
          LPath := AFileName;
          AFileName := '{<' + AFileName + '>}';
        end;

        SaveSource(ASource);
        ReportTabControl.AddNewLine
        (
          StringReplace(Format(SBaseProperties.MessageFormat_DataLoadedFromFile, [AFileName]), '\', '\\', [rfReplaceAll, rfIgnoreCase])
        );
        // 'Загружен файл' + ' ' + '{<' + StringReplace(AFileName, '\', '\\', [rfReplaceAll, rfIgnoreCase]) + '>}'

        Result := False;
      except on E:Exception do begin
        Result := True;
        Inc(LErrorCount);

        if AFileName.IsEmpty then begin
          LFormat := SBaseProperties.MessageFormat_ErrorDataLoadFromSource;
          AFileName := cxTreeColumnName.Values[INode];
        end else begin
          LFormat := SBaseProperties.MessageFormat_ErrorDataLoadFromFile;
          AFileName := '{<' + AFileName + '>}';
        end;

        ReportTabControl.AddNewLine
        (
          StringReplace(Format(LFormat, [AFileName, E.Message]), '\', '\\', [rfReplaceAll, rfIgnoreCase])
        );
      end end;
    end;

  var
    LNode: TcxTreeListNode;
    LSource: Integer;
    LMethod: Char;
    LExists: Boolean;
    LFileInfo: TFilePath;
  begin
    Result := False;
    if ANode.CheckState = cbsUnChecked then Exit;

    if not TcxTreeListNodeCrack(ANode).IsRoot then begin
      LMethod := CharFromString(cxTreeColumnMethod.Values[ANode]);
      LSource := cxTreeColumnSource.Values[ANode];
      if LMethod <> #0 then begin
        LTemplate_Id  := cxTreeColumnTemplate_Id.Values[ANode];
        LInParams     := TNamedVariants.Create(VarToStr( TreeSetProvider.Lookup(SConst_Source, LSource, SConst_Params) ), True);
        LParams       := (LFilterItems + LInParams);
      end;
    end else begin
      LMethod := #0;
      LSource := 0; // Warning
    end;

    if (LMethod = 'S') and (not FServerImportSQL.IsEmpty) then begin
      if ANode.Checked then
        if (ACheckOnly or not RunLocalImport(LSource, ANode)) then
          Inc(LSuccCount)
        else
          Result := True
    end else if CharInSet(LMethod, ['S', 'L']) then begin
      if ANode.Checked then
        if (ACheckOnly or not RunImport(LSource, ANode)) then
          Inc(LSuccCount)
        else
          Result := True
    end else if CharInSet(LMethod, [#0, 'F', 'D']) then begin
      if ANode.CheckState <> cbsUnChecked then
        for I := 0 to ANode.Count - 1 do begin
          LNode := ANode.Items[I];
          if LNode.CheckState <> cbsUnChecked then
            if LMethod = #0 then
              Result := ProcessNode(LNode, ACheckOnly) or Result
            else if LNode.Checked then begin // CharInSet(LMethod, ['F', 'D'])
              LFileName := VarToStr(cxTreeColumnFile.Values[LNode]);
              // -- ACheckOnly
              if ACheckOnly then begin
                LExists := (not LFileName.IsEmpty);
                if LExists then begin
                  LFileInfo := TFilePath.Create(LFileName);
                  if not LFileInfo.IsInternetUrl then begin
                    LExists := FileExists(LFileName);
                    if (not LExists) and (LMethod = 'D') then
                      LExists := DirectoryExists(LFileName);
                  end;
                end;

                if LExists then
                  Inc(LSuccCount)
                else begin
                  if LFileName.IsEmpty then LFileName := VarToStr(cxTreeColumnMask.Values[LNode]);
                  ReportTabControl.AddNewLine(' - ' + Format(SBaseProperties.MessageFormat_FileNotFound, [StringReplace(LFileName, '\', '\\', [rfReplaceAll, rfIgnoreCase])]));
                  Result := True;
                end;
              end else
                if (CharInSet(LMethod, ['F', 'D']) and LFileName.IsEmpty) or RunImport(LSource, LNode, LFileName) then
                  Result := True
                else
                  Inc(LSuccCount)
            end;
        end;
    end else
      Raise Exception.Create('UnSupported import method '#171 + LMethod + #187);
  end;

//  procedure PrintError(AContent: String);
//  begin
//    ReportTabControl.AddNewLine('\b' + 'Загрузка отменена.' + '\b0\par' + AContent, [fsBold]);
//  end;

var
  I: Integer;
begin
  DataSetSources.Close;
    DataSetSources.SQLApplyOptions.TempBeginDump;
  DataSetSources.Open;

  //-- Проверяем список источников, формируем ошибку, если ничего толкового не выбрано
  if FAllOrNone then begin
    LSuccCount := 0;
    if ProcessNode(cxTree.Root, True) then begin
      ReportTabControl.AddNewLN;
      ReportTabControl.AddNewLine(SBaseProperties.Message_CannotContinueWhileError, [fsBold]);
      Abort;
    end else if LSuccCount = 0 then begin
      ReportTabControl.AddNewLN;
      ReportTabControl.AddNewLine(SBaseProperties.Message_CannotContinueWhileNoChecks, [fsBold]);
      Abort;
    end;
  end;

  FImportIndex := 0;
  LFilterItems := TNamedVariants.Create(True);
  InternalGetLocalFilter(LFilterItems);

  LOutParams := TNamedVariants.Create(True);
  for I := 0 to Length(FDataSets) - 1 do with FDataSets[I] do begin
    Close;
      SQLApplyOptions.TempBeginDump;
    Open;
  end;

  LSuccCount := 0;
  if not ProcessNode(cxTree.Root, False) then begin
    if LSuccCount = 0 then begin
      ReportTabControl.AddNewLN;
      ReportTabControl.AddNewLine(SBaseProperties.Message_CannotContinueWhileNoChecks, [fsBold]);
      Abort;
    end;
  end else if FAllOrNone then
    Abort;

  DataSetSources.SQLApplyOptions.TempPostDump;
end;

procedure TSBaseCxTreeViewImportFrm.mnAddFileClick(Sender: TObject);
begin
  SetFile(True, (Sender = mnAddFolder));
end;

procedure TSBaseCxTreeViewImportFrm.SetFile(ANew: Boolean; AFolderOnly: Boolean);
var
  LMasterNode, LNode: TcxTreeListNode;
  LSource, LType: Variant;
begin
  LMasterNode := cxTree.FocusedNode;
{$IFDEF DEBUG}
  Assert(Assigned(LMasterNode));
{$ENDIF}
  if ANew then
    LNode := nil
  else begin
    LNode := LMasterNode;
    LMasterNode := LNode.Parent;
  end;

  LSource := TreeSetProvider.Lookup(SConst_Source, cxTreeColumnSource.Values[LMasterNode], SConst_TemplateType_Id);
  LType   := TypesSetProvider.Lookup(SConst_Id, LSource, SConst_Description + ';' + SConst_Filter);
  with TFileOpenDialog.Create(nil) do try
    if AFolderOnly then
      Options := [fdoPickFolders]
    else if VarIsStr(cxTreeColumnMask.Values[LMasterNode]) then
      with TFileTypeItem(FileTypes.Add) do begin
        DisplayName := SBaseProperties.Text_RecommendedFiles;
        FileMask    := cxTreeColumnMask.Values[LMasterNode];
      end
    else if VarIsArray(LType) then
      with TFileTypeItem(FileTypes.Add) do begin
        DisplayName := VarToStr(LType[0]);
        FileMask    := VarToStr(LType[1]);
      end;

    if not ANew then
      FileName := cxTreeColumnFile.Values[LNode];

    if Execute then begin
      if ANew then begin
        LNode := LMasterNode.AddChild;
        LNode.Checked := True;
      end;
      LNode.ImageIndex := GetFileImageIndex(FileName, LNode.Parent.ImageIndex);
      cxTreeColumnFile.Values[LNode] := FileName;
      cxTreeColumnName.Values[LNode] := FileName;
    end;
  finally
    Free;
  end;
end;

procedure TSBaseCxTreeViewImportFrm.mnSetFileClick(Sender: TObject);
begin
  SetFile(False, (Sender = mnSetFolder));
end;

procedure TSBaseCxTreeViewImportFrm.mnAddMaskClick(Sender: TObject);
begin
  SetMask(True);
end;

procedure TSBaseCxTreeViewImportFrm.mnDeleteFileOrMaskClick(Sender: TObject);
begin
  cxTree.FocusedNode.Delete
end;

procedure TSBaseCxTreeViewImportFrm.mnSetMaskClick(Sender: TObject);
begin
  SetMask(False);
end;

procedure TSBaseCxTreeViewImportFrm.SetMask(ANew: Boolean);
var
  LMasterNode, LNode: TcxTreeListNode;
  LSource, LType: Variant;
  LMask, LFile, LName: String;
begin
  LMasterNode := cxTree.FocusedNode;
{$IFDEF DEBUG}
  Assert(Assigned(LMasterNode));
{$ENDIF}
  if ANew then
    LNode := nil
  else begin
    LNode := LMasterNode;
    LMasterNode := LNode.Parent;
  end;

  LSource := TreeSetProvider.Lookup(SConst_Source, cxTreeColumnSource.Values[LMasterNode], SConst_Template_Id);
  LType := TypesSetProvider.Lookup(SConst_Id, LSource, SConst_Description + ';' + SConst_Filter);

  if ANew then
    LMask := ''
  else
    LMask := cxTreeColumnMask.Values[LNode];

  if StringFieldEdit(Self, LMask, SBaseProperties.Text_Mask, 600) then begin
    GetFileNameByMask(LMask, LFile, LName);
    if ANew then begin
      LNode := LMasterNode.AddChild;
      LNode.Checked := True;
    end;
    cxTreeColumnMask.Values[LNode] := LMask;
    cxTreeColumnFile.Values[LNode] := LFile;
    cxTreeColumnName.Values[LNode] := LName;
    LNode.ImageIndex := GetFileImageIndex(LFile, LNode.Parent.ImageIndex);
  end;
end;

procedure TSBaseCxTreeViewImportFrm.mnRefreshTreeClick(Sender: TObject);
begin
  TreeSetProvider.ReOpen;
end;

procedure TSBaseCxTreeViewImportFrm.PopupMenuTreePopup(Sender: TObject);
var
  LNode: TcxTreeListNode;
  LFileMode, LFileNode: Boolean;
  LMethod: Char;
begin
  LMethod := #0;
  LNode := cxTree.FocusedNode;
  LFileMode := Assigned(LNode);
  if LFileMode then begin
    LFileNode := not VarIsPresent(cxTreeColumnSource.Values[LNode]);
    if not LFileNode then begin
      LMethod   := VarToChar(cxTreeColumnMethod.Values[LNode]);
      LFileMode := CharInSet(LMethod, ['F', 'D']);
    end;
  end else
    LFileNode := False;

  mnAddFileOrMask.Enabled := LFileMode and (not LFileNode);
  if mnAddFileOrMask.Enabled then
    mnAddFolder.Visible := VisibleTodxBarVisible(LMethod = 'D');
  mnSetFileOrMask.Enabled := LFileNode;
  if mnSetFileOrMask.Enabled then
    mnSetFolder.Visible := VisibleTodxBarVisible(LMethod = 'D');

  mnDeleteFileOrMask.Enabled  := LFileNode;
end;

procedure TSBaseCxTreeViewImportFrm.SaveNodeConfig(ANode: TcxTreeListNode);
  var
    I: Integer;
    LSource, LTemplate_Id: Integer;
    LMethod: Char;
    LString: String;
  begin
    if not TcxTreeListNodeCrack(ANode).IsRoot then begin
      LSource := cxTreeColumnSource.Values[ANode];
      if VarIsOrdinal(LSource) then begin
        LMethod := VarToChar(cxTreeColumnMethod.Values[ANode]);

        if LMethod <> #0 then begin
          LTemplate_Id := cxTreeColumnTemplate_Id.Values[ANode];
          if CharInSet(LMethod, ['F', 'D']) then begin
            //-- Сохраняем количество
            SetSetting(Format(Format_ParamSources, [LSource, LTemplate_Id]),  ANode.Count);
            //-- Сохраняем остальные настройки

            for I := 0 to ANode.Count - 1 do begin
              LString := VarToStr(cxTreeColumnMask.Values[ANode.Items[I]]);
              if LString.IsEmpty then
                LString := VarToStr(cxTreeColumnFile.Values[ANode.Items[I]]);

              LString := IfThen(ANode.Items[I].Checked, '+', '-') + IfThen(LString.IsEmpty, '@', '#') + LString;
              if LString.IsEmpty then
                LString := LString + VarToStr(cxTreeColumnFile.Values[ANode.Items[I]]);

              SetSetting(Format(Format_ParamSourceItems, [LSource, LTemplate_Id, I]), LString);
            end;
          end else
            SetSetting(Format(Format_ParamSources, [LSource, LTemplate_Id]),  ANode.Checked);

          Exit;
        end;
      end;
    end;

    for I:= 0 to ANode.Count - 1 do
      SaveNodeConfig(ANode.Items[I])
end;

procedure TSBaseCxTreeViewImportFrm.SaveUserConfig(AStoreFormSettings: TFormItemsForSave);
begin
  inherited;

  if frsvUserDefined in AStoreFormSettings then
    SaveNodeConfig(cxTree.Root);
end;

procedure TSBaseCxTreeViewImportFrm.TreeSetProviderAfterOpen(DataSet: TDataSet);
  function FindById(ANode: TcxTreeListNode; AValue: Variant; var AIndex: Integer): Boolean;
  begin
    AIndex := ANode.Count - 1;
    while AIndex >= 0 do begin
      if cxTreeColumnSource.Values[ANode.Items[AIndex]] = AValue then Exit(True);
      Dec(AIndex);
    end;
    Result := False;
  end;

  procedure ReFill(ANode: TcxTreeListNode; AParent: Variant);
  var
    LRecNo, LIndex, LValueCount: Integer;
    LValues: Array of Variant;
    LNode: TcxTreeListNode;

    function FindInArray(AIndex: Variant): Boolean;
    var
      I: Integer;
    begin
      for I := Pred(LValueCount) downto 0 do
        if LValues[I] = AIndex then Exit(True);
      Result := False;
    end;

  var
    LOldMethod, LMethod: Char;
  begin
    LRecNo := TreeSetProvider.RecNo;
    if TreeSetProvider.Active and TreeSetProvider.Locate(SConst_Parent, AParent, []) then begin
      LValueCount := 0;
      repeat
        Inc(LValueCount);
        SetLength(LValues, LValueCount);
        LValues[Pred(LValueCount)] := TreeSetProviderSource.AsInteger;
        if FindById(ANode, TreeSetProviderSource.AsInteger, LIndex) then begin
          LNode   := ANode.Items[LIndex];
          LOldMethod := VarToChar(cxTreeColumnMethod.Values[LNode]);
          LMethod    := CharFromString(TreeSetProviderMethod.AsString);
          if (LOldMethod <> LMethod) or (cxTreeColumnTemplate_Id.Values[LNode] <> TreeSetProviderTemplate_Id.AsVariant) then begin
            if CharInSet(LOldMethod, ['F', 'D']) then
              SaveNodeConfig(LNode);
            LNode.DeleteChildren;
            cxTreeColumnMethod.Values[LNode] := String(LMethod);
            cxTreeColumnTemplate_Id.Values[LNode] := TreeSetProviderTemplate_Id.AsInteger;
            if CharInSet(LMethod, ['F', 'D']) then
              FillCustom(LNode);
          end;
        end else begin
          LNode := ANode.AddChild;
          LNode.CheckGroupType := ncgCheckGroup;
          LNode.CheckState     := cbsUnchecked;
          cxTreeColumnSource.Values[LNode] := TreeSetProviderSource.AsInteger;
          cxTreeColumnName.Values[LNode]   := TreeSetProviderName.AsString;

          LMethod := CharFromString(TreeSetProviderMethod.AsString);

          LNode.ImageIndex := TreeSetProviderImageIndex.AsInteger;
          cxTreeColumnMethod.Values[LNode] := String(LMethod);
          cxTreeColumnTemplate_Id.Values[LNode] := TreeSetProviderTemplate_Id.AsInteger;
          cxTreeColumnMask.Values[LNode] := TreeSetProviderMask.AsVariant;

          if CharInSet(LMethod, ['F', 'D']) then
            FillCustom(LNode)
          else if LMethod <> #0 then
            LNode.Checked := VarToBoolDef(GetSetting(Format(Format_ParamSources, [TreeSetProviderSource.AsInteger, TreeSetProviderTemplate_Id.AsInteger])), False);
        end;
      until not TreeSetProvider.LocateEx(SConst_Parent, AParent, [bloNext]);

      for LIndex := Pred(ANode.Count) downto 0 do begin
        LNode := ANode.Items[LIndex];
        if not FindInArray(cxTreeColumnSource.Values[LNode]) then begin
          SaveNodeConfig(LNode);
          LNode.Delete;
        end;
      end;

      TreeSetProvider.RecNo := LRecNo;
    end else begin
      SaveNodeConfig(ANode);
      if TcxTreeListNodeCrack(ANode).IsRoot then
        ANode.DeleteChildren
      else
        ANode.Delete
    end;
  end;
begin
  cxTree.BeginUpdate;
  try
//    if TreeSetProvider.IsEmpty then
//      cxTree.Root.DeleteChildren
//    else
    ReFill(cxTree.Root, Null);
  finally
    cxTree.EndUpdate;
  end;
end;

function TSBaseCxTreeViewImportFrm.GetFileImageIndex(const AFile: String; ADefault: Integer): Integer;
var
  LFileExt: String;
  LcxImageComboBoxItem: TcxImageComboBoxItem;
begin
  LFileExt := ExtractFileExt(AFile);
  LcxImageComboBoxItem := SBaseFormProperties.EditRepository_FileExtensions.Properties.FindItemByValue(LFileExt);
  if Assigned(LcxImageComboBoxItem) then
    Result := LcxImageComboBoxItem.ImageIndex
  else
    Result := ADefault;
end;

function TSBaseCxTreeViewImportFrm.GetFileNameByMask(const AMask: String; var AFile, AName: String): Boolean;
var
  LFilePath, LFileName: String;
  LFileList: TArray<String>;
  LFileCount: Integer;
  LFileInfo: TFilePath;
begin
  LFileName := CompileFilePathByDate('', AMask, flDateTime.ParamValue);
  LFileInfo := TFilePath.Create(LFileName);
  if LFileInfo.IsInternetUrl then begin
    AFile  := LFileName;
    Result := True;
  end else begin
    LFilePath := IncludeTrailingPathDelimiter(ExtractFilePath(LFileName));
    LFileName := ExtractFileName(LFileName);

    LFileList   := GetFileList(LFilePath, LFileName);
    LFileCount  := Length(LFileList);
    if LFileCount = 0 then begin
      AFile := '';
      AName := format(SBaseProperties.MessageFormat_FileNotFound, [LFilePath + LFileName]);
      Result := False;
    end else if LFileCount > 1 then begin
      AFile := '';
      AName := format(SBaseProperties.MessageFormat_MultipleFilesFound, [LFileCount, LFilePath + LFileName]);
      Result := False;
    end else begin
      AFile  := LFileList[0];
      AName  := AFile;
      Result := True;
    end;
  end;
end;

//procedure TSBaseCxTreeViewImportFrm.UpdateFileNameByMask(ANode: TcxTreeListNode);
//var
//  LMask, LFile, LName: String;
//begin
//  LMask := VarToStr(cxTreeColumnMask.Values[ANode]);
//  if not LMask.IsEmpty then begin
//     GetFileNameByMask(LMask, LFile, LName);
//     cxTreeColumnName.Values[ANode] := LName;
//     cxTreeColumnFile.Values[ANode] := LFile;
//     ANode.ImageIndex := GetFileImageIndex(IsEmptyString(LFile, LName), ANode.Parent.ImageIndex);
//  end;
//end;

//procedure TSBaseCxTreeViewImportFrm.UpdateFileNamesByMask;
//var
//  I: Integer;
//begin
//  if not VarIsPresent(flDateTime.ParamValue) then Exit;
//  for I := 0 to cxTree.AbsoluteCount - 1 do
//    UpdateFileNameByMask(cxTree.AbsoluteItems[I]);
//end;

procedure TSBaseCxTreeViewImportFrm.FillCustom(ANode: TcxTreeListNode);
var
  I, LCount: Integer;
  LMask, LFile, LName: String;
  LNode: TcxTreeListNode;
begin
  LCount := VarToIntDef(GetSetting(Format(Format_ParamSources, [TreeSetProviderSource.AsInteger, TreeSetProviderTemplate_Id.AsInteger])), 0);
  if LCount > 0 then begin
    for I := 0 to LCount - 1 do begin
      LMask := VarToStr(GetSetting(Format(Format_ParamSourceItems, [TreeSetProviderSource.AsInteger, TreeSetProviderTemplate_Id.AsInteger, I])));
      if LMask.Length > 2 then begin
        LNode := ANode.AddChild;
        LNode.ImageIndex := ANode.ImageIndex;
        LNode.Checked := (LMask[1] = '+');
        if LMask[2] = '#' then begin
          LMask := Copy(LMask, 3, LMask.Length - 2);
          GetFileNameByMask(LMask, LFile, LName);
          cxTreeColumnMask.Values[LNode] := LMask;
          cxTreeColumnFile.Values[LNode] := LFile;
          cxTreeColumnName.Values[LNode] := LName;
        end else begin
          LFile := Copy(LMask, 3, LMask.Length - 2);
          cxTreeColumnFile.Values[LNode] := LFile;
          cxTreeColumnName.Values[LNode] := LFile;
        end;
        LNode.ImageIndex := GetFileImageIndex(LFile, ANode.ImageIndex);
      end;
    end;
  end;
end;

procedure TSBaseCxTreeViewImportFrm.DataSetBeforeOpen(DataSet: TDataSet);
begin
  inherited;

  DataSetSources.Close;
  ReportTabControl.Active := False;
  ReportTabControl.Active := True;
  ReportTabControl.BeginUpdate;

  ReportTabControl.AddNewLine('\b' + SBaseProperties.Message_ProcessStarted + '\b0: ' + TimeToStr(Now));
  ReportTabControl.AddNewLN;
//  Application.ProcessMessages();

  InternalImpot;

  ReportTabControl.AddNewLN;
  ReportTabControl.AddNewLine('\b' + SBaseProperties.Message_DataUploaded + '\b0: ' + TimeToStr(Now), [fsBold]);
end;

procedure TSBaseCxTreeViewImportFrm.DataSetImportAfterInsert(ADataSet: TDataSet);
begin
  ADataSet.FieldByName(SConst_Index).AsInteger := FImportIndex;
end;

procedure TSBaseCxTreeViewImportFrm.flDateTimeOnFilterValueChanged(Sender: TObject);
var
  LOldActive, LActive: Boolean;
begin
  LOldActive := TreeSetProvider.Active;
  TreeSetProvider.ReOpen([rofOnlyIfChanged, rofCloseIfParamsRequired]);
  LActive := TreeSetProvider.Active;
  if LOldActive and LActive then
    TreeSetProviderAfterOpen(TreeSetProvider);
//    UpdateFileNamesByMask
end;

procedure TSBaseCxTreeViewImportFrm.AddDataSet(ADataSet: TSQLDataSetProvider);
var
  LCount: Integer;
begin
  LCount := Length(FDataSets);
  SetLength(FDataSets, Succ(LCount));
  FDataSets[LCount] := ADataSet;
end;

procedure TSBaseCxTreeViewImportFrm.DataSetAfterOpen(DataSet: TDataSet);
begin
  inherited;

//  DataSetSources.Open;
  ReportTabControl.Active := False;
  ReportTabControl.Active := True;
end;

{ TfsFunctions_SBaseCxTreeViewImportForm }

constructor TfsFunctions_SBaseCxTreeViewImportForm.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseCxTreeViewImportFrm, TSBaseCxTreeViewImportFrm.ClassParent.ClassName) do begin
    AddMethod('procedure AddDataSet(ADataSet: TSQLDataSetProvider)', Call_TSBaseCxTreeViewImportFrm_Method);
  end;
end;

function TfsFunctions_SBaseCxTreeViewImportForm.Call_TSBaseCxTreeViewImportFrm_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'ADDDATASET' then
    TSBaseCxTreeViewImportFrm(Instance).AddDataSet(VarToPointer(Caller.Params[0]));
end;

initialization
  RegisterClass(TSBaseCxTreeViewImportFrm);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_SBaseCxTreeViewImportForm);
{$ENDIF}

finalization
  UnRegisterClass(TSBaseCxTreeViewImportFrm);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TfsFunctions_SBaseCxTreeViewImportForm);
{$ENDIF}

end.
