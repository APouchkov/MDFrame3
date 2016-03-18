unit SBaseImport;
{$I config.inc}

interface

uses
  Types, Classes, Forms, StdCtrls, TypInfo, Graphics, SBaseDataHelperInternal, SBaseDataModule,
  Controls, SysUtils, Variants, Messages, RegularExpressions, DB, NamedVariables, Generics.Collections,
  Math, cxStyles, SBaseProgress, Windows, Dialogs, SBaseFormDefinitions, SBaseForm, StrUtils, hsDialogs
  {IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter, fs_isysrtti, fs_idbrtti
  {ENDIF}
  ;

const
  WM_FileImportComplite = WM_USER + $5000;

const
  Progress_CaptionRecNo               : string = 'Обрабатывается строка №%d';
  Progress_CaptionRecNoFromTotalCount : string = 'Обрабатывается строка №%d из %d';

  Message_RaiseExecute : string          = 'Выполнение импорта данных отменено пользователем!';
  Message_RaiseTooFewDataSets : string   = 'Для запуска импорта передано слишком мало(%d) датасетов, хотя требуется %d!';
  Message_RaiseNotInitFields : string    = 'Для датасета №%d отсутствует описание полей! Создайте поля самостоятельно.';

  SImpPrefix  : String = '#Import#';

  SConst_Template_Id        : String = 'Template_Id';
  SConst_TemplateType_Id    : String = 'TemplateType_Id';
  SImpParam_TemplateId      : String = 'Template#Id';
  SImpParam_TemplateName    : String = 'Template#Name';
  SImpParam_TemplateDate    : String = 'Template#Date';

  SImpParam_DataSetDump     : String = 'DataSet#Dump';    // BEGIN, POST, COMMIT
  SImpParam_DataSetPrepare  : String = 'DataSet#Prepare'; // CLEAR = Close+Open+FieldsClear, EMPTY = Close+Open
  SImpParam_DataSetFinal    : String = 'DataSet#Final';   // COMMIT, EMPTY, CLOSE, CLEAR

  SImpParam_FinalSQL        : String = 'Final#SQL';

//  SImpParam_TemplateId      : String = '#Import#Template#Id';
//  SImpParam_TemplateName    : String = '#Import#Template#Name';
//  SImpParam_TemplateDate    : String = '#Import#Template#Date';
//  SImpParam_DataSet         : String = '#Import#DataSet';
//  SImpParam_DataSets        : String = '#Import#DataSets';
//  SImpParam_FreeOnTerminate : String = '#Import#FreeOnTerminate';
//  SImpParam_OpenDialog      : String = '#Import#OpenDialog';
//  SImpParam_Output          : String = '#Import#Output';
//  SImpParam_Thread          : String = '#Import#Thread';

//********************************************************************************************
//********************************************************************************************
//********************************************************************************************

type
  TSBaseImport = class;
  TfsRTTIModuleClass = class of TfsRTTIModule;

  TSBaseImportFileMode    = (bifmNone, bifmFile, bifmDir);
//  TSBaseImportConditions  = (bicAllPossible, bicAllOrNone);
  TImportDataSetRecord = record
    Name          : String;
    AutoPostCount : Integer;
  end;

  TImportFinalDataSetAction = (iedaNone, iedaCommit, iedaEmpty, iedaClose, iedaClear);

  TImportDataSetProperties = record
//    AutoCreated       : Boolean;

    FirstBeginDump    : Boolean;
    FinalPostDump     : Boolean;
    FinalCommitDump   : Boolean;

    FinalDataSetAction: TImportFinalDataSetAction;
  end;

  TImportValueSource = record
    Index: Integer;
    CastAs: String;
  public
    constructor Create(AIndex: Integer; const ACastAs: String);
  end;

  TSBaseFilesImportItem = record
    Code  : String;
    Mask  : String;
    Name  : String;
  end;

  TSBaseImportProvider = class abstract(TPersistent)
  private
    [weak] FOwner : TSBaseImport;
    FCanceled     : Boolean;
  protected
    procedure UpdateProgress; overload;

    procedure Log(AType: Char; const AText: string);
    procedure DrawTextToProgress(const AText: string);
    procedure UpdateProgress(const AText: String; ARecordNo, ARecordCount: Integer); overload;

    function RecordNo: Integer; dynamic; //abstract;
    function RecordCount: Integer; dynamic; //abstract;
    function FileMode: TSBaseImportFileMode; dynamic;

    procedure InternalExecute; dynamic; abstract;
    procedure Execute;
  public
    class function Code: String; dynamic; abstract;
    class function FastScriptClass: TfsRTTIModuleClass; dynamic; abstract;

    constructor Create(AOwner: TSBaseImport); reintroduce; virtual;

    procedure Connect(const AConnectionString: String); dynamic;
    procedure Open(const AFileName: String); dynamic; abstract;
    procedure Close; dynamic; abstract;
    procedure Disconnect; dynamic;

    property Owner: TSBaseImport read FOwner;
  published
    property Canceled: Boolean read FCanceled;
  end;
  TSBaseImportProviderClass = class of TSBaseImportProvider;

  TSBaseFileImportFormat = packed record
    Code: String;
    ReaderClass: TSBaseImportProviderClass;
  end;
  TSBaseFileImportFormats = Array of TSBaseFileImportFormat;

  TSBaseImportProperties = class(TComponent)
  private
    FSQLTypes   : String;
    FSQLTemplate: String;
    FSQLDataSets: String;
    FSQLLinkExec: String;
  private
    class var FFormats: TSBaseFileImportFormats;
    class var FFileImportUnit: TfsScript;
  public
    class procedure RegisterFormat(AReaderClass: TSBaseImportProviderClass);
    class procedure UnRegisterFormat(AReaderClass: TSBaseImportProviderClass);

    class property Formats: TSBaseFileImportFormats read FFormats;
    class function fsFileImportUnit: TfsScript;
    class procedure Init;
    class procedure Clear;
  published
    property SQLTypes   : String read FSQLTypes    write FSQLTypes;
    property SQLTemplate: String read FSQLTemplate write FSQLTemplate;
    property SQLDataSets: String read FSQLDataSets write FSQLDataSets;
    property SQLLinkExec: String read FSQLLinkExec write FSQLLinkExec;
  end;

  TFileImportThread   = class;
  TSBaseImport = class(TComponent, ISBaseInterface)
  strict private
    procedure DataSetsClear;
  private
//    FTemplate           : String;
    FDescription          : String;
    FCreateParams         : TNamedVariants;
    FSQLConnectionProvider: TSQLConnectionProvider;

    FScript               : TfsScript;

    FEnvironment          : TNamedVariants;
    FFileTypeItems        : TFileTypeItems;

    FDeclareDataSets      : TArray<TImportDataSetRecord>;
    FDeclareFields        : TObjectList<TFields>;

    FImportProvider       : TSBaseImportProvider;

    FProgress             : TSBaseProgressFrm;

    FThreadExecute        : Boolean;
    FHandle               : HWND;

    FFileImportThread     : TFileImportThread;
    FThreadException      : String;

    //FInputParams        : TNamedVariants;
    FOutputParams         : TNamedVariants;
    FDataSets             : TArray<TCustomBaseDataSetProvider>;
    FDataSetProperties    : TArray<TImportDataSetProperties>;
    FFinalSQL             : String;

    FFirstBeginDump       : Boolean;
    FFinalPostDump        : Boolean;
    FFinalCommitDump      : Boolean;

    procedure CancelClick(Sender: TObject);
    procedure MessageThreadFinished(var AMessage: TMessage);
    procedure InternalEndExecute;

    function GetImportParams(AName: String): Variant;
    procedure SetImportParams(AName: String; const AValue: Variant);
    function GetDataSets(Index: Integer): TCustomBaseDataSetProvider;
    procedure SQLConnectionProviderOnInfoMessage(Sender: TInternalConnection; const AMessage: String);
  public
    destructor Destroy; override;

    constructor CreateByParams(AOwner: TComponent; const AParams: Array of TNamedVariant); overload;
    constructor CreateByParams(AOwner: TComponent; const AParams: TNamedVariants); overload;
    procedure RunByParams;

    procedure Execute(const AFileName: String);
    procedure Log(AType: Char; const AText: string);
    procedure DrawTextToProgress(const AText: String);
    procedure UpdateProgress(const AText: String; ARecordNo, ARecordCount: Integer);

    function ScriptExist: Boolean; inline;
    procedure ScriptExecute;
    function ScriptFindFunction(const AFunction: String; ARaiseIfNotFound: Boolean = True): TfsCustomVariable;
    function ScriptCallFunction(const AFunction: String; ARaiseIfNotFound: Boolean = True): Variant; overload;
    function UserParamNames: String;

    property CreateParams: TNamedVariants read FCreateParams;
    property ImportParams[Name: String]: Variant read GetImportParams write SetImportParams;
    property Environment: TNamedVariants read FEnvironment;

    function DataSetByName(const AName: String): TCustomBaseDataSetProvider;

    property OutputParams: TNamedVariants read FOutputParams;
    property DataSets[Index: Integer]: TCustomBaseDataSetProvider read GetDataSets;
    procedure PostDataSet(AIndex: Integer; AFinal: Boolean = False);
  published
    { Реализация интерфейса ICreateByParams. Пропустить не глядя. }
    function  IVirtualClassOwner(var AVirtualClassName: String; var AParams: TNamedVariants): TPersistentClass;
    function  ICreateByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants): TComponent;
    function  IGetCreateParams: TNamedVariants;
    procedure ISBaseInterface.IShow = RunByParams;
    function  ISelectByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants; ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants;

    property ThreadExecute: Boolean read FThreadExecute write FThreadExecute;
    property FinalCommandText: String read FFinalSQL write FFinalSQL;
  end;
  TSBaseFileImportClass = class of TSBaseImport;

  TFileImportThread = class(TThread)
  private
    FOutputText       : String;
    FOutputType       : Char;
    FOutputRecordNo   : Integer;
    FOutputRecordCount: Integer;

    procedure _Log;
    procedure _DrawTextToProgress;
    procedure _UpdateProgress;
  protected
    FOwner          : TSBaseImport;
    FTerminated     : Boolean;
  public
    constructor Create(AOwner: TSBaseImport);
    procedure Execute; override;

    procedure DrawTextToProgress(const AText: String);
    procedure UpdateProgress(const AText: String; ARecordNo, ARecordCount: Integer);
    procedure Log(AType: Char; const AText: string);
  end;

  TfsFunctions_FileImport = class(TfsRTTIModule)
  private
    function Call_TSBaseFileImport_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
    function Call_TSBaseImportProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

{$IFDEF FASTSCRIPT_RTTI}
  TfsFunctions_FileImportEx = class(TfsRTTIModule)
  private
    function Call_TFileImport_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

var
  SBaseImportProperties: TSBaseImportProperties;

implementation

uses
  SBaseStringFunctions, SBaseConstants, SBaseVariantFunctions, ComObj, ActiveX,
  SBaseFileUtils, DataModule, fsFunction_TDataSet, SBaseUtils,
  SBaseMathFunctions, SBaseXml;

{ TSBaseImport }

constructor TSBaseImport.CreateByParams(AOwner: TComponent; const AParams: Array of TNamedVariant);
begin
  CreateByParams(AOwner, TNamedVariants.Create(AParams, True));
end;

constructor TSBaseImport.CreateByParams(AOwner: TComponent; const AParams: TNamedVariants);
var
  LType : String;
  LFormat, LIdx, LDataIndex: Integer;
  LDataSet: TCustomInternalQuery;
//  LDataSetCount: Word;
  LFields: TFields;
  LVariant: Variant;
begin
  Assert(Assigned(AOwner) and (AOwner is TSBaseFrm));
  Assert(Assigned(SBaseImportProperties) and (not SBaseImportProperties.SQLTemplate.IsEmpty));
  FCreateParams.AssignValues(AParams);

  if VarIsOrdinal(ImportParams[SImpParam_TemplateId]) then
    ImportParams[SImpParam_TemplateName] := Null
  else begin
    LVariant := ImportParams[SImpParam_TemplateName];
    if VarIsStr(LVariant) and (Length(LVariant) > 0) then
      ImportParams[SImpParam_TemplateId] := Null
    else
      Exception.Create('Не задан обязательный параметр '#171 + SImpParam_TemplateName + #187' - Имя шаблона.');
  end;

  FOutputParams.UnknownAsNull := True;

  // открываем общую инфу
  LDataSet := BaseSQLConnectionProvider.OpenSQLQuery
              (
                SBaseImportProperties.SQLTemplate,
                FCreateParams
              );
  try
    FEnvironment.AssignValues(LDataSet.Fields);
    LDataSet.OpenNext;
    if not LDataSet.IsEmpty then
      FEnvironment.AssignValues(LDataSet.Fields);
    FEnvironment.ClearUndefined;

    LDataSet.OpenNext;
    if LDataSet.Active and (not LDataSet.IsEmpty) then begin
      FFileTypeItems := TFileTypeItems.Create(TFileTypeItem);
      LDataSet.First;
      while not LDataSet.Eof do begin
        with FFileTypeItems.Add do begin
          DisplayName := LDataSet.FieldByName(SConst_Description).AsString;
          FileMask := LDataSet.FieldByName(SConst_Filter).AsString;
        end;
        LDataSet.Next
      end;

      if Assigned(ArchivesFileTypeItems) then // Не знаю что делать когла список пуст
        for LIdx := 0 to ArchivesFileTypeItems.Count - 1 do
          FFileTypeItems.Add.Assign(ArchivesFileTypeItems[LIdx]);
    end;

  finally
    BaseSQLConnectionProvider.CloseSQLQuery;
  end;

  LType         := FEnvironment[SConst_Type];
  FDescription  := FEnvironment[SConst_Description];

  LFormat := Length(SBaseImportProperties.FFormats) - 1;
  while LFormat >= 0 do begin
    if SameText(SBaseImportProperties.FFormats[LFormat].Code, LType) then
      Break;
    Dec(LFormat);
  end;

  if (LFormat < 0) then
    Raise Exception.Create('Importing template '#171 + LType + #187' is not supported by system.');

  LDataSet := BaseSQLConnectionProvider.OpenSQLQuery
              (
                SBaseImportProperties.SQLDataSets,
                FEnvironment
              );
  try
    if LDataSet.RecordCount > 0 then begin
      SetLength(FDeclareDataSets, LDataSet.RecordCount);

      LDataSet.First;
      while not LDataSet.Eof do begin
        with LDataSet.FieldByName(SConst_Index) do
          if AsInteger <> LDataSet.RecNo then
            Raise Exception.Create('Incorrect import data: DataSet.RecNo = ' + IntToStr(LDataSet.RecNo) + ', DataSet.Index = ' + AsString);

        with FDeclareDataSets[LDataSet.RecNo - 1] do begin
          Name          := LDataSet.FieldByName(SConst_Name).AsString;
          AutoPostCount := LDataSet.FieldByName(SConst_AutoPostCount).AsInteger;
        end;
        LDataSet.Next;
      end;

      LDataSet.OpenNext;
      FDeclareFields := TObjectList<TFields>.Create(True);
      for LDataIndex := 0 to Length(FDeclareDataSets) - 1 do begin
        LDataSet.Filter := SConst_DataSet + '=' + IntToStr(LDataIndex + 1);
        LDataSet.Filtered := True;
        if LDataSet.RecordCount > 0 then begin
          LFields := TFields.Create(nil);
          FDeclareFields.Add(LFields);
          LDataSet.First;
          while not LDataSet.Eof do begin
            LFields.CreateField
            (
              LDataSet.FieldByName(SConst_Name).AsString,
              LDataSet.FieldByName(SConst_Type).AsString,
              LDataSet.FieldByName(SConst_Length).AsInteger,
              LDataSet.FieldByName(SConst_Scale).AsInteger
            );

            LDataSet.Next;
          end;
        end else
          FDeclareFields.Add(nil);
      end;
    end;
  finally
    BaseSQLConnectionProvider.CloseSQLQuery;
  end;

  FImportProvider := SBaseImportProperties.FFormats[LFormat].ReaderClass.Create(Self);
  if FEnvironment.Find(SConst_Script, LIdx) then begin
    FScript := TfsScript.Create(nil);
    with FScript do begin
      AutoDeclareClasses        := True;

      Parent                    := TSBaseImportProperties.fsFileImportUnit;
      Parent.AutoDeclareClasses := True;

      AddObject(SConst_Target, Self);
      AddObject(SConst_Source, FImportProvider);

      Lines.Text := FEnvironment.Items[LIdx].Value;
    end;
  end;

  inherited Create(AOwner);
end;

destructor TSBaseImport.Destroy;
begin
  FreeAndNil(FImportProvider);
  FreeAndNil(FProgress);
  FreeAndNil(FFileTypeItems);
  FreeAndNil(FFileImportThread);
  FreeAndNil(FDeclareFields);
  FreeAndNil(FScript);
  DataSetsClear; // FreeAndNil(FDataSets);

//  Terminate;

  inherited;
end;

procedure TSBaseImport.CancelClick(Sender: TObject);
begin
  FImportProvider.FCanceled := True;
  DrawTextToProgress('Остановка обработки');
end;

procedure TSBaseImport.Log(AType: Char; const AText: string);
begin
  SBaseMainFormMDIInterface.Log(AType, AText, True)
end;

procedure TSBaseImport.DrawTextToProgress(const AText: String);
begin
  if FProgress <> nil then
    FProgress.Text := AText;
end;

procedure TSBaseImport.Execute(const AFileName: String);
var
  LDataSet: TCustomBaseDataSetProvider;
  I, LDataSetCount, LDataSetIndex: Integer;
  LFields: TFields;
{$IFDEF DEBUG}
  LErrorScope: String;
{$ENDIF}

  function WinControl(AComponent: TComponent): TWinControl;
  begin
    if AComponent is TWinControl then
      Result := TWinControl(AComponent)
    else
      Result := nil;
  end;
begin
  Assert(Length(FDataSets) > 0);
  FImportProvider.Connect(AFileName);

  FThreadException := '';
  FOutputParams.Clear;
  FImportProvider.FCanceled := False;

  FreeAndNil(FProgress);
  FImportProvider.Open(AFileName);

  if VarToBoolDef(ImportParams[SConst_ShowProgress], True) then begin
    FProgress := TSBaseProgressFrm.Create(Owner);
    FProgress.Cancel.OnClick := CancelClick;
    FProgress.Show(WinControl(Owner));
  end;

  if FScript <> nil then with FScript do begin
    if not Compile then begin
{$IFDEF DEBUG}
      if not FThreadExecute then
        with ErrorInfo(ErrorPos) do begin
          LErrorScope := 'Ошибка компиляции: ' + ErrorMsg + SConst_CR + 'Позиция: ' + ErrorPos + SConst_CR + Scope;
          hsDialogs.HSMessageDlg(LErrorScope, mtError, [mbOK], 0);
        end;
{$ENDIF}
      Raise Exception.Create(FScript.ErrorMsg);
    end;
  end;

  if Assigned(FDeclareFields) then begin
    LDataSetCount := FDeclareFields.Count;
    if LDataSetCount > Length(FDataSets) then
      Raise Exception.CreateFmt(Message_RaiseTooFewDataSets, [Length(FDataSets), LDataSetCount]);

    LDataSetIndex := LDataSetCount - 1;
    try
      while LDataSetIndex >= 0 do begin
        LDataSet := FDataSets[LDataSetIndex];
        LDataSet.DisableControls;
        if (LDataSet.FieldCount = 0) then begin
          LFields := FDeclareFields[LDataSetIndex];
          if not Assigned(LFields) then
            Raise Exception.CreateFmt(Message_RaiseNotInitFields, [LDataSetIndex + 1]);
          for I := 0 to LFields.Count - 1 do
            LFields[I].Clone(LDataSet).DataSet := LDataSet.ActiveDataSet;
        end;
        LDataSet.Active := True;
        Dec(LDataSetIndex);
      end;
    except
      for I := LDataSetIndex to LDataSetCount - 1 do try
        FDataSets[I].EnableControls;
      except end;
      Raise;
    end;
  end else begin
    LDataSetCount := Length(FDataSets);
    LDataSetIndex := LDataSetCount - 1;
    try
      while LDataSetIndex >= 0 do begin
        LDataSet := FDataSets[LDataSetIndex];
        if LDataSet.Fields.Count = 0 then
          Raise Exception.CreateFmt(Message_RaiseNotInitFields, [LDataSetIndex + 1]);
        LDataSet.DisableControls;
      end;
    except
      for I := LDataSetIndex + 1 to LDataSetCount - 1 do try
        FDataSets[I].EnableControls;
      except end;
      Raise;
    end;
  end;

  try
    if FThreadExecute then begin
      try
        if FHandle = 0 then
          FHandle := Classes.AllocateHWnd(MessageThreadFinished);

        FreeAndNil(FFileImportThread);
        FFileImportThread := TFileImportThread.Create(Self);
        FFileImportThread.Priority := tpLowest;
        FFileImportThread.Start;

        while not FFileImportThread.Finished do
          HandleMessage;

        if (not FThreadException.IsEmpty) then            // исключения были погашены внутри потока
          Raise Exception.Create(FThreadException);
      finally
        FreeAndNil(FProgress);
      end;

    end else begin
      // загрузка в основном потоке, без индикатора
      FImportProvider.Execute;
    end;
  finally
    FImportProvider.Close;
    for I := 0 to LDataSetCount - 1 do
      FDataSets[I].EnableControls;
  end;

  if FImportProvider.Canceled then
    Abort;
end;

function TSBaseImport.GetDataSets(Index: Integer): TCustomBaseDataSetProvider;
begin
  Result := FDataSets[Index]
end;

function TSBaseImport.DataSetByName(const AName: String): TCustomBaseDataSetProvider;
var
  I: Integer;
begin
  for I := 0 to Length(FDataSets) - 1 do begin
    Result := FDataSets[I];
    if (Result is TSQLDataSetProvider) then begin
      if SameText(AName, TSQLDataSetProvider(Result).SQLApplyOptions.TempTable) then Exit;
    end else if AName.IsEmpty then
      Exit;
  end;
  Result := nil;
end;

procedure TSBaseImport.DataSetsClear;
var
  I: Integer;
begin
  for I := 0 to Pred(Length(FDataSets)) do
    if FDataSets[I].Owner = Self then
      FreeAndNil(FDataSets[I]);

  SetLength(FDataSets, 0);
end;

function TSBaseImport.GetImportParams(AName: String): Variant;
begin
  Result := FCreateParams.GetValue(SImpPrefix + AName)
end;

procedure TSBaseImport.SetImportParams(AName: String; const AValue: Variant);
begin
  FCreateParams.SetValue(SImpPrefix + AName, AValue)
end;

procedure TSBaseImport.SQLConnectionProviderOnInfoMessage(Sender: TInternalConnection; const AMessage: String);
begin
  if Assigned(FImportProvider) then
    FImportProvider.Log(lgInfo, AMessage)
  else
    Log(lgInfo, AMessage);
end;

function TSBaseImport.ICreateByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants): TComponent;
begin
  Result := TSBaseFileImportClass(AClass).CreateByParams(AOwner, AParams);
end;

function TSBaseImport.IGetCreateParams: TNamedVariants;
begin
  Result := FCreateParams;
end;

procedure TSBaseImport.InternalEndExecute;
begin
//
end;

function TSBaseImport.ISelectByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants; ACallBackProcedure: TCallBackSelectFunction): TNamedVariants;
var
  LCallBackScript : String;
  LIdx            : Integer;
  LOutput         : TNamedVariants;
  LImport         : TSBaseImport;
begin
  Result := TNamedVariants.Create(True);

  LOutput.Clear;
  AParams[SImpPrefix + SConst_Output] := Integer(@LOutput);
  AParams[SImpPrefix + SConst_FreeOnTerminate] := True;

  LImport := TSBaseImport(TSBaseFileImportClass(AClass).CreateByParams(AOwner, AParams));
  LImport.RunByParams;

  if AParams.Find(SConst_ResultFields, LIdx) then begin
    with AParams.Items[LIdx] do
      Result[Value] := LOutput[Value];
  end else begin
    Result := LOutput;
  end;

  LCallBackScript := VarToStr(AParams[SBFSelectCallBackScript]);
  TSBaseFrm.ExecuteSelectCallBackProcedures(cbsaSingleRecord, Result, AOwner, ACallBackProcedure, LCallBackScript);
end;

function TSBaseImport.IVirtualClassOwner(var AVirtualClassName: String; var AParams: TNamedVariants): TPersistentClass;
begin
  Result := nil;
end;

procedure TSBaseImport.RunByParams;
var
  LDataSet            : TCustomBaseDataSetProvider;
  LSQLDataSetProvider : TSQLDataSetProvider absolute LDataSet;

  LHasImplementedDataSets: Boolean;
  LDataSetPrepare, LDataSetDump: Variant;
  LDataSetDumpHighBound: Integer;
  LDataSetPrepareItem, LDataSetDumpItem: String;
  LDumpBeginItem, LDumpPostItem, LDumpCommitItem: Boolean;

  LDataSetFinal: Variant;
  LDataSetFinalItem: String;

  L: Integer;

  procedure LocalFillProperties(AIndex: Integer);
  begin
    LDumpBeginItem  := InArray(LDataSetDumpItem, 'BEGIN');
    LDumpPostItem   := InArray(LDataSetDumpItem, 'POST');
    LDumpCommitItem := InArray(LDataSetDumpItem, 'COMMIT'); if LDumpCommitItem then LDumpPostItem := True;

    with FDataSetProperties[AIndex] do begin
      FirstBeginDump  := LDumpBeginItem;
      FinalPostDump   := LDumpPostItem;
      FinalCommitDump := LDumpCommitItem;

      if LDataSetFinalItem = 'CLEAR' then
        FinalDataSetAction := iedaClear
      else if LDataSetFinalItem = 'EMPTY' then
        FinalDataSetAction := iedaEmpty
      else if LDataSetFinalItem = 'CLOSE' then
        FinalDataSetAction := iedaClose
      else if LDataSetFinalItem = 'COMMIT' then
        FinalDataSetAction := iedaCommit
      else
        FinalDataSetAction := iedaNone;
    end;

    if LDumpBeginItem  then FFirstBeginDump  := True;
    if LDumpPostItem   then FFinalPostDump   := True;
    if LDumpCommitItem then FFinalCommitDump := True;
  end;

  procedure DataSetsImplementation(const ADataSets: Variant);
  var
    I: Integer;
  begin
    L := VarArrayHighBound(ADataSets, 1);
    SetLength(FDataSetProperties, Succ(L));
    SetLength(FDataSets, Succ(L));
    for I := 0 to L do begin
      if VarIsArray(LDataSetPrepare) then LDataSetPrepareItem := LDataSetPrepare[I];

      LDataSet := VarToPointer(ADataSets[I]);
      if Assigned(LDataSet) then begin
        LHasImplementedDataSets := True;
        Assert(LDataSet is TCustomBaseDataSetProvider);
        FDataSets[I] := LDataSet;

        if LDataSetPrepareItem = 'EMPTY' then
          LDataSet.Close
        else if LDataSetPrepareItem = 'CLEAR' then begin
          LDataSet.Close;
          LDataSet.Fields.Clear;
        end;
      end;

      if VarIsArray(LDataSetDump) then LDataSetDumpItem := LDataSetDump[I];
      if VarIsArray(LDataSetFinal) then LDataSetFinalItem := LDataSetFinal[I];

      LocalFillProperties(I);
      if (LDumpBeginItem or LDumpPostItem or LDumpCommitItem or (FDataSetProperties[I].FinalDataSetAction = iedaCommit))
          and Assigned(LDataSet) and not (LDataSet is TSQLDataSetProvider)
      then
        Raise Exception.Create('DataSets array item[' + IntToStr(I) + '] is not TSQLDataSetProvider.');

      if LDumpBeginItem  then FFirstBeginDump  := True;
      if LDumpPostItem   then FFinalPostDump   := True;
      if LDumpCommitItem then FFinalCommitDump := True;
    end;
  end;

var
  LFileName, LDir : String;
  LOutput         : PNamedVariants;

  LFilePath : TFilePath;
  LFileMask: String;

  LTempDir, LTempFile: String;

  LOpenDialog: String;
  LStoreFilePath: Boolean;
  LFileMode, LRequestedFileMode: TSBaseImportFileMode;
  LDate: TDate;

  I, LIdx: Integer;

{
  -- #FileName  : String
  -- #FileDir   : String;
  -- #DataSet   : TCustomBaseDataSetProvider
  -- #Output    : PNamedVariants

  -- #OpenDialog: String = 'File'
  -- #FileMask  : String - Маска искомых файлов (для распаковки из архива)
}
begin
  LTempDir  := '';
  LTempFile := '';
  LFileMask := '';

  try
    LFileMode := FImportProvider.FileMode;
    LRequestedFileMode := LFileMode;
    LDate := VarToDateTimeDef(ImportParams[SImpParam_TemplateDate], 0);

    if LFileMode = bifmNone then
      LFileName := ''
    else begin
      LDir      := VarToStr(ImportParams[SConst_Dir]);
      LFileName := VarToStr(ImportParams[SConst_FileName]);

      if (LDate <> 0) then begin
        if (not LDir.IsEmpty) and (Pos('%', LDir) > 0) then
          LDir := CompileFilePathByDate('', LDir, LDate);
        if (not LFileName.IsEmpty) and (Pos('%', LFileName) > 0) then
          LFileName := CompileFilePathByDate('', LFileName, LDate);
      end;

      LOpenDialog := VarToStr(ImportParams[SConst_OpenDialog]);
      if (not LOpenDialog.IsEmpty) then begin
        if SameText(LOpenDialog, SConst_File) then
          with TFileOpenDialog.Create(Owner) do try
            if (LFileMode = bifmFile) then begin
              if Assigned(FFileTypeItems) then
                FileTypes := FFileTypeItems;
              if LDir.IsEmpty and (not LFileName.IsEmpty) then
                LDir := ExtractFileDir(LFileName);
              FileName := LFileName;
            end else if Assigned(ArchivesFileTypeItems) and (not LFileName.IsEmpty) then begin
              LRequestedFileMode := bifmFile;
              if LDir.IsEmpty then
                LDir := ExtractFileDir(LFileName);
              FileName := ExtractFileName(LFileName);
              FileTypes := ArchivesFileTypeItems;
              DefaultExtension := '*.' + ExtractFileExt(LFileName);
            end else begin
              Options := Options + [fdoPickFolders];
              LFileName := '';
            end;

            if (not LDir.IsEmpty) then begin
              LStoreFilePath := False;
              DefaultFolder := LDir;
            end else if (frsvUserDefined in TSBaseFrm(Owner).StoreFormSettings) then begin
              LStoreFilePath := True;
              DefaultFolder := VarToStr(TSBaseFrm(Owner).GetSetting(FEnvironment[SConst_Name]));
            end else
              LStoreFilePath := False;

            if Execute then begin
              LFileName := FileName;
              if LRequestedFileMode = bifmFile then
                LDir := ExtractFileDir(LFileName)
              else begin
                LDir := LFileName;
                LFileName := '';
              end;
              if LStoreFilePath then begin
                TSBaseFrm(Owner).SetSetting(FEnvironment[SConst_Name], LDir);
              end;
            end else
              Abort;
          finally
            Free
          end
        else
          Raise Exception.Create('Open file dialog type '#171 + LOpenDialog + #187' is not supported');
      end else begin
        if LFileName.IsEmpty then begin
          if (LFileMode = bifmFile) or LDir.IsEmpty then
            Raise Exception.Create('Unable to start import process until the '#171'FileName'#187' parameter is empty');
        end else begin
          LFilePath := TFilePath.Create(LFileName);
          if LFilePath.IsInternetUrl then begin
            LFileName := LFilePath.FileName;
            if LFileName.IsEmpty then begin
              LFileName := 'Downloaded-file'
            end;
            LTempDir  := GetTempPath;
            LTempFile := IncludeTrailingPathDelimiter(LTempDir) + ExtractFileName(LFileName);
            LFileName := LTempFile;
            DownloadInternetFile(LTempFile, LFilePath.URL);
          end else if (LFileMode = bifmDir) and SysUtils.DirectoryExists(LFileName) then begin
            LDir      := LFileName;
            LFileName := '';
          end;
        end;
      end;

{$IFDEF ARCHIVES}
      try
        if (not LFileName.IsEmpty) then begin
          LFileMask := VarToStr(ImportParams[SConst_FileMask]);
          if LFileMask.IsEmpty and Assigned(FFileTypeItems) and (FFileTypeItems.Count > 0) then begin
            for I := 0 to FFileTypeItems.Count - 1 do
              if FFileTypeItems[I].FileMask <> SFileMask_All then
                LFileMask := ConcatIfNotEmpty(LFileMask, '|', FFileTypeItems[I].FileMask)
          end;
        end;

        if LFileMode = bifmFile then
          LFileName := ExtractFileIfNeeded(LFileName, LFileMask, LTempDir)
        else if (not LFileName.IsEmpty) then
          LFileName := ExtractFiles(LFileName, LFileMask, LTempDir)
        else
          LFileName := LDir;
      finally
        if (not LTempFile.IsEmpty) and (LTempFile <> LFileName) then
          SysUtils.DeleteFile(LTempFile)
      end;
{$ENDIF}
    end;

    LHasImplementedDataSets := False;
    DataSetsClear;

    FFirstBeginDump := False;
    FFinalPostDump  := False;
    FFinalCommitDump  := False;

    FThreadExecute  := (ImportParams[SConst_Thread] = True);
    FFinalSQL   := VarToStr(ImportParams[SImpParam_FinalSQL]);

    LDataSetPrepare := ImportParams[SImpParam_DataSetPrepare];
    if not VarIsArray(LDataSetPrepare) then LDataSetPrepareItem := VarToStr(LDataSetPrepare) else LDataSetPrepareItem := '';

    LDataSetDump     := ImportParams[SImpParam_DataSetDump];
    if not VarIsArray(LDataSetDump) then begin
      LDataSetDumpItem := VarToStr(LDataSetDump);
      LDataSetDumpHighBound := -1;
    end else begin
      LDataSetDumpItem := '';
      LDataSetDumpHighBound := VarArrayHighBound(LDataSetDump, 1);
    end;

    LDataSetFinal := ImportParams[SImpParam_DataSetFinal];
    if not VarIsArray(LDataSetFinal) then
      LDataSetFinalItem := VarToStr(LDataSetFinal)
    else
      LDataSetFinalItem := '';

    try
      if FCreateParams.Find(SImpPrefix + SConst_SQLConnectionProvider, LIdx) then with FCreateParams.Items[LIdx] do begin
        FSQLConnectionProvider := TSQLConnectionProvider(VarToPointer(Value));
        Assert(Assigned(FSQLConnectionProvider) and (FSQLConnectionProvider is TSQLConnectionProvider));
      end else if FFinalSQL.IsEmpty then
        FSQLConnectionProvider := nil
      else if FThreadExecute then begin
        FSQLConnectionProvider := BaseSQLConnectionProvider.Clone(Self);
        FSQLConnectionProvider.OnInfoMessage := SQLConnectionProviderOnInfoMessage;
        FSQLConnectionProvider.Connected := True;
      end else
        FSQLConnectionProvider := BaseSQLConnectionProvider;

//

      if FCreateParams.Find(SImpPrefix + SConst_DataSets, LIdx) then with FCreateParams.Items[LIdx] do begin
        Assert(VarIsArray(Value) and (VarArrayLowBound(Value, 1) = 0));
        DataSetsImplementation(Value);
      end else if FCreateParams.Find(SImpPrefix + SConst_DataSet, LIdx) then with FCreateParams.Items[LIdx] do begin
        Assert(VarIsOrdinal(Value));
        DataSetsImplementation(VarArrayOf([Value]));
      end;

// Автосоздание недостающих датасетов

      Assert
      (
        LHasImplementedDataSets
        or
        (
          LDataSetFinalItem.IsEmpty
          or
          (
            // Если есть определение датасетов
            (Length(FDeclareDataSets) > 0)
            and
            (
              // Нет финального скрипта
              (not FFinalSQL.IsEmpty)
              or
              // Есть не автоматический провайдер подключения
              (Assigned(FSQLConnectionProvider) and (FSQLConnectionProvider.Owner <> Self))
            )
          )
        ),
        'Final test failed'
      );

//      if VarIsEmpty(LDataSetDump) then
//        LDataSetDumpItem := 'BEGIN,POST';

//      Assert(, 'Invalid argument: ' + SImpParam_DataSetFinal);
//      FFirstBeginDump   := True;

      L := Length(FDeclareDataSets);
      if L > Length(FDataSets) then begin
        SetLength(FDataSetProperties, L);
        SetLength(FDataSets, L);
      end;

      for I := 0 to Pred(L) do if not Assigned(FDataSets[I]) then begin
        LDataSet := TSQLDataSetProvider.Create(Self);
        FDataSets[I] := LDataSet;

        LSQLDataSetProvider.DataSetObject := bdsoMemory;
        LSQLDataSetProvider.CachedUpdates := True;
        LSQLDataSetProvider.AutoGenerateUpdateScripts := False;
        LSQLDataSetProvider.ConnectionProvider := FSQLConnectionProvider;

        with LSQLDataSetProvider.SQLApplyOptions, FDeclareDataSets[I] do begin
          TempTable     := Name;
          // AutoPostAfter := AutoPostCount;
        end;

        if VarIsArray(LDataSetDump) then
          if I <= LDataSetDumpHighBound then
            LDataSetDumpItem := LDataSetDump[I]
          else
            LDataSetDumpItem := '';

        LocalFillProperties(I);
        with FDataSetProperties[I] do begin
          FirstBeginDump := FinalPostDump;
          Assert(not FinalCommitDump, 'Invalid argument value: DataSet#Final = COMMIT');
        end;
        if LDumpPostItem then FFinalPostDump := True;
      end;
      Assert(LHasImplementedDataSets or FFinalPostDump, 'PostDump required');

//

      LOutput := VarToPointer(ImportParams[SConst_Output]);
      if Assigned(LOutput) then LOutput^.Clear;
      Execute(LFileName);
      if Assigned(LOutput) then LOutput^.AssignValues(FOutputParams);
    finally
      if Assigned(FSQLConnectionProvider) and (FSQLConnectionProvider.Owner = Self) then
        FreeAndNil(FSQLConnectionProvider);

      if (not LTempDir.IsEmpty) then
        ClearDirectory(LTempDir, True)
    end;
  finally
    if ImportParams[SConst_FreeOnTerminate] = True then
      Destroy;
  end;
end;

procedure TSBaseImport.MessageThreadFinished(var AMessage: TMessage);
begin
  with AMessage do
    if (Msg = WM_BaseDataSetThreadFinished) and (Integer(FFileImportThread) = Integer(wParam)) then
      try
        InternalEndExecute
      except
        Application.HandleException(Self);
      end
    else
      DefWindowProc(FHandle, Msg, wParam, lParam);
end;

procedure TSBaseImport.PostDataSet(AIndex: Integer; AFinal: Boolean);
var
  LDataSet: TDataSet;
  LSQLDataSetProvider: TSQLDataSetProvider absolute LDataSet;
begin
  with FDataSetProperties[AIndex] do
    if FinalPostDump and (AFinal or (FinalDataSetAction in [iedaEmpty, iedaCommit]))
    then begin
      LDataSet := FDataSets[AIndex];
      if (not AFinal) then
        with FDeclareDataSets[AIndex] do if (AutoPostCount <= 0) or (LDataSet.RecordCount < AutoPostCount) then Exit;

      LSQLDataSetProvider.SQLApplyOptions.TempPostDump([usInserted], (FinalDataSetAction = iedaEmpty));

      if (not AFinal) and (FinalDataSetAction = iedaCommit) then begin
        LSQLDataSetProvider.ApplyUpdates;
        LSQLDataSetProvider.CommitUpdates;
      end;
    end;
end;

procedure TSBaseImport.ScriptExecute;
begin
  Assert(Assigned(FScript));
  FScript.Execute;
end;

function TSBaseImport.ScriptExist: Boolean;
begin
  Result := Assigned(FScript)
end;

procedure TSBaseImport.UpdateProgress(const AText: String; ARecordNo, ARecordCount: Integer);
begin
  if Assigned(FProgress) then begin
    if (not AText.IsEmpty) then
      FProgress.Text          := AText;

    if ARecordCount >= 0 then
      FProgress.ProgressCount := ARecordCount;

    if ARecordNo >= 0 then
      FProgress.ProgressPos   := ARecordNo;
  end;
end;

function TSBaseImport.UserParamNames: String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FCreateParams.Count - 1 do with FCreateParams.Items[I] do
    if not SameText(LeftStr(Name, SImpPrefix.Length), SImpPrefix) then
      Result := ConcatIfNotEmpty(Result, ';', Name)
end;

function TSBaseImport.ScriptFindFunction(const AFunction: String; ARaiseIfNotFound: Boolean): TfsCustomVariable;
begin
  if FScript = nil then
    if ARaiseIfNotFound then
      Raise Exception.Create('The template does not contain any script lines.')
    else
      Exit(nil);

  Result := FScript.FindLocal(AFunction);
  if (Result = nil) or not (Result is TfsProcVariable) then
    if ARaiseIfNotFound then
      Raise Exception.Create('The function '#171 + AFunction + #187' does not exists.')
    else
      Exit(nil);
end;

//function TSBaseImport.ScriptCallFunction(const AFunction: TfsCustomVariable): Variant;
//begin
//  Result := AFunction.Value
//end;

function TSBaseImport.ScriptCallFunction(const AFunction: String; ARaiseIfNotFound: Boolean): Variant;
var
  LVariable: TfsCustomVariable;
begin
  LVariable := ScriptFindFunction(AFunction, ARaiseIfNotFound);
  if LVariable <> nil then
    Result := TfsProcVariable(LVariable).Value
  else
    Result := unAssigned;
end;

{ TFileImportThread }

constructor TFileImportThread.Create(AOwner: TSBaseImport);
begin
  FOwner := AOwner;
  FreeOnTerminate := False;

  inherited Create(True);
end;

procedure TFileImportThread.DrawTextToProgress(const AText: string);
begin
  FOutputText := AText;
  Synchronize(_DrawTextToProgress);
end;

procedure TFileImportThread.Log(AType: Char; const AText: string);
begin
  FOutputType := AType;
  FOutputText := AText;
  Synchronize(_Log);
end;

procedure TFileImportThread.Execute;
begin
  inherited;

  CoInitializeEx(nil, COINIT_MULTITHREADED);

  try
    FOwner.FImportProvider.Execute;
  except on E: Exception do
    FOwner.FThreadException := E.Message;
  end;

  CoUninitialize;

  if (not Finished) then begin
    Terminate;
    if FOwner.FHandle <> 0 then
      PostMessage(FOwner.FHandle, WM_BaseDataSetThreadFinished, Integer(Self), 0);
  end
end;

procedure TFileImportThread.UpdateProgress(const AText: String; ARecordNo, ARecordCount: Integer);
begin
  FOutputText         := AText;
  FOutputRecordNo     := ARecordNo;
  FOutputRecordCount  := ARecordCount;
  Synchronize(_UpdateProgress);
end;

procedure TFileImportThread._DrawTextToProgress;
begin
  FOwner.DrawTextToProgress(FOutputText);
end;

procedure TFileImportThread._UpdateProgress;
begin
  FOwner.UpdateProgress(FOutputText, FOutputRecordNo, FOutputRecordCount);
end;

procedure TFileImportThread._Log;
begin
  FOwner.Log(FOutputType, FOutputText);
end;

{ TSBaseImportProperties }

class procedure TSBaseImportProperties.Init;
begin
  SBaseImportProperties := TSBaseImportProperties.Create(Application);
  SBaseImportProperties.Name := 'SBaseImportProperties';
end;

class procedure TSBaseImportProperties.Clear;
begin
// Destroying by Application
//  if Assigned(SBaseImportProperties) then
//    FreeAndNil(SBaseImportProperties);

  if Assigned(FFileImportUnit) then begin
    FFileImportUnit.ClearRTTI;
    FreeAndNil(FFileImportUnit);
  end;
end;

class function TSBaseImportProperties.fsFileImportUnit: TfsScript;
var
  I: Integer;

  procedure AddRTTI(AClass: TfsRTTIModuleClass);
  var
    RTTI: TfsRTTIModule;
  begin
    if not Assigned(AClass) then Exit;

    RTTI := AClass.Create(FFileImportUnit);
    FFileImportUnit.Add('', RTTI);
  end;
begin
  if (FFileImportUnit = nil) {and not FGlobalUnitDestroyed} then
  begin
    FFileImportUnit := TfsScript.Create(nil);
    FFileImportUnit.RTTIAdded := True;

    AddRTTI(TfsSysFunctions);
    AddRTTI(TfsDBRTTI);
    AddRTTI(TfsFunctions_SBaseUtils);
    AddRTTI(TfsFunctions_TDataSet);
    AddRTTI(TfsFunctions_Variant);
    AddRTTI(TfsFunctions_FileImport);
    AddRTTI(TfsFunctions_String);
    AddRTTI(TfsFunctions_Xml);
    AddRTTI(TfsFunctions_NamedVariants);
    AddRTTI(TfsFunctions_Math);

    for I := 0 to Length(FFormats) - 1 do
      AddRTTI(FFormats[I].ReaderClass.FastScriptClass);
  end;

  Result := FFileImportUnit;
end;

class procedure TSBaseImportProperties.RegisterFormat(AReaderClass: TSBaseImportProviderClass);
var
  I: Integer;
begin
  I := Length(FFormats);
  Setlength(FFormats, I + 1);
  with SBaseImportProperties.FFormats[I] do begin
    Code := UpperCase(AReaderClass.Code);
    ReaderClass := AReaderClass;
  end;
end;

class procedure TSBaseImportProperties.UnRegisterFormat(AReaderClass: TSBaseImportProviderClass);
var
  I, J: Integer;
begin
  for I := Length(SBaseImportProperties.FFormats) - 1 downto 0 do
    if FFormats[I].ReaderClass = AReaderClass then begin
      for J := I to Length(FFormats) - 2 do
        FFormats[J] := FFormats[J+1];
      SetLength(FFormats, Length(SBaseImportProperties.FFormats) - 1);
      Break;
    end;
end;

{ TSBaseImportProvider }

procedure TSBaseImportProvider.Connect(const AConnectionString: String);
begin
//
end;

constructor TSBaseImportProvider.Create(AOwner: TSBaseImport);
begin
  FCanceled := False;
  FOwner := AOwner;

  inherited Create;
end;

procedure TSBaseImportProvider.UpdateProgress;
var
  LRecordNo, LRecordCount: Integer;
  LText: String;
begin
  LRecordNo := RecordNo;
  LRecordCount := RecordCount;
  if LRecordNo <= LRecordCount then
    LText := Format(Progress_CaptionRecNoFromTotalCount, [LRecordNo, LRecordCount])
  else
    LText := Format(Progress_CaptionRecNo, [LRecordNo]);

  UpdateProgress(LText, LRecordNo, LRecordCount);
  // DrawTextToProgress(LText);
end;

procedure TSBaseImportProvider.Disconnect;
begin
//
end;

procedure TSBaseImportProvider.Log(AType: Char; const AText: string);
begin
  if Owner.FFileImportThread <> nil then
    Owner.FFileImportThread.Log(AType, AText)
  else
    Owner.Log(AType, AText)
end;

procedure TSBaseImportProvider.DrawTextToProgress(const AText: string);
begin
  if Owner.FFileImportThread <> nil then
    Owner.FFileImportThread.DrawTextToProgress(AText)
  else
    Owner.DrawTextToProgress(AText)
end;

procedure TSBaseImportProvider.UpdateProgress(const AText: String; ARecordNo, ARecordCount: Integer);
begin
  if Owner.FFileImportThread <> nil then
    Owner.FFileImportThread.UpdateProgress(AText, ARecordNo, ARecordCount)
  else
    Owner.UpdateProgress(AText, ARecordNo, ARecordCount)
end;

procedure TSBaseImportProvider.Execute;
var
  I: Integer;
  LDataSet: TCustomBaseDataSetProvider;
  LSQLDataSetProvider: TSQLDataSetProvider absolute LDataSet;
  LTables: String;
  LParams: TNamedVariants;
  LParamNames: String;
begin
  FCanceled := False;

  if Owner.FFirstBeginDump then
    for I := 0 to Length(FOwner.FDataSets) - 1 do if FOwner.FDataSetProperties[I].FirstBeginDump then
      TSQLDataSetProvider(FOwner.FDataSets[I]).SQLApplyOptions.TempBeginDump;

  InternalExecute;
  if FCanceled then Exit;

  LTables := '';
  for I := 0 to Length(FOwner.FDataSets) - 1 do with FOwner.FDataSetProperties[I] do begin
    if FinalPostDump then begin
      LDataSet := FOwner.FDataSets[I];
      Assert(LSQLDataSetProvider.SQLApplyOptions.TempDumpActive, 'DataSet dump is not beginned');
      DrawTextToProgress('Отправка набора данных №' + IntToStr(Succ(I)) + ' на сервер...');
      FOwner.PostDataSet(I, True);
      LTables := ConcatIfNotEmpty(LTables, ',', LSQLDataSetProvider.SQLApplyOptions.InternalTempTable);
      if FinalCommitDump then
        LSQLDataSetProvider.SQLApplyOptions.TempCommitDump;
    end;
    if FCanceled then Exit;

    case FinalDataSetAction of
      iedaClose: FOwner.FDataSets[I].Close;
      iedaClear: with FOwner.FDataSets[I] do begin Close; Fields.Clear end;
      iedaEmpty: FOwner.FDataSets[I].EmptyTable;
      iedaCommit:
        begin
          LDataSet := FOwner.FDataSets[I];
          LSQLDataSetProvider.ApplyUpdates;
          LSQLDataSetProvider.CommitUpdates;
        end;
    end;
  end;

  if not FOwner.FFinalSQL.IsEmpty then begin
    DrawTextToProgress('Выполнение завершающего скрипта на сервере...');
    LParams := SBaseFormProperties.InterfaceVariables;
    LParamNames := FOwner.UserParamNames;
    if not LParamNames.IsEmpty then
      LParams.AssignValues(LParamNames, FOwner.FCreateParams[LParamNames], amRaiseIfRepeated);
    LParams.AssignValues(FOwner.OutputParams, amRaiseIfRepeated);
    LParams.SetValue(SConst_Tables, LTables);

    with FOwner.FSQLConnectionProvider.OpenSQLQuery(FOwner.FFinalSQL, LParams) do try
      while Active do begin
        if (not IsEmpty) and (FieldCount = 2) and SameText(Fields[0].FieldName, SConst_Log_Type) and SameText(Fields[1].FieldName, SConst_Log_Message)
        then begin
          First;
          while not Eof do begin
            Log(Fields[0].AsString[1], Fields[1].AsString);
            Next;
          end;
        end;

        OpenNext;
      end;
    finally
      FOwner.FSQLConnectionProvider.CloseSQLQuery;
    end;
  end;
end;

function TSBaseImportProvider.FileMode: TSBaseImportFileMode;
begin
  Result := bifmNone;
end;

function TSBaseImportProvider.RecordCount: Integer;
begin
  Result := -1;
end;

function TSBaseImportProvider.RecordNo: Integer;
begin
  Result := 0;
end;

{ TfsFunctions_FileImport }
constructor TfsFunctions_FileImport.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseImport, TSBaseImport.ClassParent.ClassName) do begin
    AddIndexProperty('InputParams', DelphiTypeString, DelphiTypeVariant, Call_TSBaseFileImport_Method, True);
    AddIndexProperty('OutputParams', DelphiTypeString, DelphiTypeVariant, Call_TSBaseFileImport_Method, False);
    AddIndexProperty('DataSets', DelphiTypeInteger, TDataSet.ClassName, Call_TSBaseFileImport_Method, False);
    AddMethod('function DataSet:' + TDataSet.ClassName, Call_TSBaseFileImport_Method);
    AddMethod('procedure PostDataSet(AIndex: Integer)', Call_TSBaseFileImport_Method);
  end;

  with AScript.AddClass(TSBaseImportProvider, TSBaseImportProvider.ClassParent.ClassName) do begin
    AddMethod('procedure DrawTextToProgress(const AText: String);', Call_TSBaseImportProvider_Method);
    AddMethod('procedure Log(AType: Char; const AText: String);', Call_TSBaseImportProvider_Method);
  end;
end;

function TfsFunctions_FileImport.Call_TSBaseFileImport_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'INPUTPARAMS.GET' then
    Result := TSBaseImport(Instance).FCreateParams[Caller.Params[0]]
  else if MethodName = 'OUTPUTPARAMS.GET' then
    Result := TSBaseImport(Instance).FOutputParams[Caller.Params[0]]
  else if MethodName = 'OUTPUTPARAMS.SET' then
    TSBaseImport(Instance).FOutputParams[Caller.Params[0]] := Caller.Params[1]
  else if MethodName = 'DATASETS.GET' then
    Result := Integer(TSBaseImport(Instance).FDataSets[Integer(Caller.Params[0])])
  else if MethodName = 'DATASET' then
    Result := Integer(TSBaseImport(Instance).FDataSets[0])
  else if MethodName = 'POSTDATASET' then
    TSBaseImport(Instance).PostDataSet(Caller.Params[0])
end;


function TfsFunctions_FileImport.Call_TSBaseImportProvider_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'DRAWTEXTTOPROGRESS' then
    TSBaseImportProvider(Instance).DrawTextToProgress(Caller.Params[0])
  else if MethodName = 'LOG' then
    TSBaseImportProvider(Instance).Log(VarToChar(Caller.Params[0]), Caller.Params[1]);
end;

{$IFDEF FASTSCRIPT_RTTI}
constructor TfsFunctions_FileImportEx.Create(AScript: TfsScript);
const
  SCategory: String = 'Прочие';
begin
  inherited;

  with AScript.AddClass(TSBaseImport, TSBaseImport.ClassParent.ClassName) do begin
    AddMethod('function CreateByParams(AOwner: Pointer; AParams: Variant): TComponent', Call_TFileImport_Method);
    AddMethod('procedure RunByParams;', Call_TFileImport_Method);
  end;
end;

function TfsFunctions_FileImportEx.Call_TFileImport_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'CREATEBYPARAMS' then begin
    Result := Integer
              (
                TSBaseImport(Instance).CreateByParams
                (
                  TComponent(VarToPointer(Caller.Params[0])),
                  TNamedVariants.Create(Caller.Params[1])
                )
              );
  end else if MethodName = 'RUNBYPARAMS' then
    TSBaseImport(Instance).RunByParams;
end;
{$ENDIF}

{ TImportValueSource }

constructor TImportValueSource.Create(AIndex: Integer; const ACastAs: String);
begin
  Index  := AIndex;
  CastAs := ACastAs;
end;

initialization
  TSBaseImportProperties.Init;
  RegisterClasses([TSBaseImportProperties, TSBaseImport]);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_FileImportEx);
{$ENDIF}

finalization
  UnRegisterClasses([TSBaseImportProperties, TSBaseImport]);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TfsFunctions_FileImportEx);
{$ENDIF}
  TSBaseImportProperties.Clear;

end.

