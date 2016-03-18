{******************************************************************************}
{                       -= Модуль печати документов =-                         }
{                                                                              }
{                           v4.1.1 (С) 2007 - 2014                             }
{                                                                              }
{                                                                              }
{******************************************************************************}
{******************************************************************************}
{                                                                              }
{   Наборы данных содержащие менее трех  полей считаются служебными и в отчет  }
{ не попадают, если в наборе  данных  нет  ни одного  параметра перечисленных  }
{ выше, то все строки такого набора данных выводяться в лог программы.         }
{                                                                              }
{   Cообщения сервера выводяться  в лог программы (только при указании типа),  }
{ для выбора типа сообщения нужно добавлять в начало сообщения строку:         }
{                                        'ERROR:'            - ошибка          }
{                                        'WARNING:'          - предупрежднение }
{                                        'INFO:'             - информация      }
{                                                                              }
{   В модуле возможно использование параметров (возвращаемых в наборе данных)  }
{ для этого нужно создать поле с именем параметра ("Имя поля" = <значение>):   }
{                                                                              }
{  1. Основные параметры:                                                      }
{      - Связь таблиц Master-Detail:                                           }
{          "#.Detail_" + имя поля Master рекордсета                            }
{      - Связь таблиц Master-Detail (мастер и детейл должны быть отсортированы)}
{          "Link_" + имя поля Master рекордсета*                               }
{      - Добавление переменной в отчет:                                        }
{          "#.Variable#" + имя переменной                                      }
{      - Количество датасетов в отчете: "#.DataSetCount"                       }
{      - Объединение отчетов: "#.ReportMerge"*                                 }
{                                                                              }
{  2. Операции с наборами данных:                                              }
{      - Пустой набор данных: "#.EmptyRecordset",                              }
{          если указано <значение>, то выводиться в лог                        }
{      - Служебный набор данных: "#.SystemRecordset"                           }
{      - Глобальный набор данных "#.GlobalRecordset"                           }
{      - Набор данных для печати отчета на каждую запись: "#.ReportRecordset"* }
{      - Набор данных для постраничной печати: "#.PageRecordset"*              }
{      - Набор данных преобразовать в переменные: "#.VariableRecordset"        }
{      - Клонировать набор данных: "#.CloneLastRecordSet"                      }
{                                                                              }
{  3. Вывод отчета:                                                            }
{      - Имя шаблона: "#.PathFileName"                                         }
{      - Строка для поиска шаблона по папкам: "#.PathFileSearch"               }
{      - Имя файла: <имя файла> - "#.FileName"                                 }
{                                                                              }
{  * - Не все типы шаблонов поддерживают данную возможность                    }
{******************************************************************************}
unit SBaseReport;
{$I config.inc}

interface

uses
  SysUtils, Classes, Generics.Collections, Contnrs, DB, VirtualTable,
  {$IFNDEF REPORT_EXTERNAL}
  Controls, Forms, Messages, Dialogs, SBaseProgress, SBaseFormDefinitions, SBaseForm, FileCtrl,
  {$ENDIF}
  {$IFDEF FASTSCRIPT_RTTI}
  fs_iinterpreter,
  {$ENDIF}
  NamedVariables, SBaseReportInterface, SBaseDataHelperInternal, SBaseDataModule;

const
  SRepParam_Date            = '#Report#Date';
  SRepParam_ObjectId        = '#Report#Object#Id';
  SRepParam_ObjectCode      = '#Report#Object#Code';
  SRepParam_ObjectParams    = '#Report#Object#Params';

type
  TSBaseReport = class;
  TReportClass = class of TSBaseReport;
  TReportData = class;

  TProcNamePriority = (pnpServer, pnpClient);
  TRepPatternType = Byte;
  TRepReplaceType = (rrtReplace, rrtBackup, rrtNew, rrtInternal);

  TFactoryPatternRegisterProc = procedure (Report: IReport; var FactoryPattern: IFactoryPatterns);
  TRepLogMessageEvent = procedure(const Msg: String; LogType: Char) of object;
  TSaveFileEvent = procedure(Sender: TObject; const FileName: String) of object;

  TVirtualDataSet = (vdsNone, vdsMaster, vdsPage);

  TReportVersion = record
    Caption         : String;
    ProcedureName   : String;
    ProcedureParams : String;
  end;
  PReportVersion = ^TReportVersion;

  TReportInternalDataSet = class(TSQLDataSetProvider)
  public
    constructor Create(Owner: TComponent); override;
  end;

  TReportInternalData = class(TVirtualTable, IReportDataSet)
  protected
    FQuickMasterDetail: Boolean;
    FMasterDataSet: TDataSet;
    FMasterValues: Variant;
    FMasterFieldNames: String;
    FDetailFieldNames: String;
  public
    constructor Create(AOwner: TComponent); override;
    function EOF: Boolean; reintroduce;
    procedure First; reintroduce;
    procedure Next; reintroduce;
    procedure SetQuickMasterDetail;
  end;

  TReportDataSetLink = class(TStringList)
  private
    FSorted: Boolean;
  public
    DataSet: TDataSet;
    property Sorted: Boolean read FSorted write FSorted;
  end;

  TReportDataSet = class(TComponent)
  private
    FData: TReportData;
    FDetailLinks: TObjectList;
    FDataSetAfterScroll: TDataSetNotifyEvent;
    function GetName: String;
    procedure InternalDataSetAfterScroll(ADataSet: TDataSet);
  protected
    FDataSet: TDataSet;
    FDataSourceStateBookmark: TDataSourceStateBookmark;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetDataSet(const Value: TDataSet); virtual;
    procedure LoadFromDataSet(Source: TDataSet; RowCount: Integer = -1);
    property DataSetList: TReportData read FData;
    property DetailLinks: TObjectList read FDetailLinks;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddDetail(ADataSet: TReportDataSet; const AFields: String; Sorted: Boolean);
    property Name: String read GetName;
    property DataSet: TDataSet read FDataSet write SetDataSet;
  end;

  TReportDataSetType = (rdstData, rdstSystem, rdstInfo, rdstMaster, rdstReport, rdstPage, rdstClone);
  TReportMessageType = (rmtInfo, rmtWarning, rmtError);
  TReportMessageEvent = procedure (Sender: TObject; const Message: String; MessageType: TReportMessageType) of object;
  TReportDataSetStorage = (rdssExternal, rdssLocalCopy, rdssLocalSource);

  TReportData = class(TComponent)
  private
    FActive: Boolean;

    FVariables: TNamedVariants;
    FParams: TNamedVariants;
    FDataSetParams: TNamedVariants;

    FReportDataSet: TReportDataSet;
    FReportDataSetLink: TReportDataSet;

    FPageDataSet: TReportDataSet;
    FPageDataSetFieldName: String;

    FList: TList<TReportDataSet>;
    FGlobalDataSets: TList<TReportDataSet>;
    FVirtualDataSets: TStrings;

    FOnMessage: TReportMessageEvent;

    function GetDataSet(Index: Integer): TReportDataSet;
    function GetDataSetCount: Integer;
  protected
    function PrepareDataSet(ADataSet: TDataSet): Boolean;
    function InternalAddDataSet(const Value: Variant; DataSetType: TReportDataSetType): Boolean;
    procedure PrepareParams(ADataSet: TDataSet);
    procedure InitDataSet(ADataSet: TReportDataSet; var ADataSetParams: TNamedVariants);
    procedure AddMasterDetailLink(ADataSet: TReportDataSet; const AFields: String; Sorted: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear(ClearAll: Boolean);
    function AddDataSet(ADataSet: TDataSet; ADataSetSource: TReportDataSetStorage = rdssLocalSource): TReportDataSet;
    procedure Delete(Index: Integer);
    function ExportDataSet(Index: Integer): TDataSet;
    function GetDataSetByName(const DataSetName: String): TReportDataSet;

    property Active: Boolean read FActive write FActive;
    property Variables: TNamedVariants read FVariables;
    property Params: TNamedVariants read FParams;

    property DataSets[Index: Integer]: TReportDataSet read GetDataSet; default;
    property DataSetCount: Integer read GetDataSetCount;

    property ReportDataSet: TReportDataSet read FReportDataSet;
    property PageDataSet: TReportDataSet read FPageDataSet;
    property PageDataSetFieldName: String read FPageDataSetFieldName;

    property OnMessage: TReportMessageEvent read FOnMessage write FOnMessage;
  end;

  TRepMailConfig = packed record
    Server: String;
    EMail: String;
    UserName: String;
    Password: String;
    ConnectionTimeout: Integer;
    ServerPort: Integer;
    ProxyServer: String;
    ProxyBypass: String;
  end;

  TRepThread = class(TThread)
  private
    FRep: TSBaseReport;
  protected
    property Rep: TSBaseReport read FRep write FRep;
    procedure Execute; override;
    procedure DoTerminate; override;
  public
    constructor Create(Rep: TSBaseReport);
  end;

  TReportActionType = (atShow, atSave, atStream, atPrint);
  TReportAction = record
    ActionType   : TReportActionType;
    FileName     : String;
    Stream       : TStream;
    Printer      : String;
    ReplaceType  : TRepReplaceType;
    ShowModal    : Boolean;
  end;

  TReportMergeInfo = record
    Pattern: IPattern;
    Value: Variant;
  end;

  TSBaseReport = class(TComponent, IReport{$IFNDEF REPORT_EXTERNAL}, ISBaseInterface{$ENDIF})
  private
    {$IFNDEF REPORT_EXTERNAL}
    FOwnerForm: TForm;
    FParentForm: TForm;
    FProgressForm: TSBaseProgressFrm;
    FParentFormResize: TNotifyEvent;
    {$ENDIF}
    FConnectionProvider: TSQLConnectionProvider;

    FMainConnectionProvider: TSQLConnectionProvider;
    FData: TReportData;
    FReportInfo: TNamedVariants;
    FReadOnly: Boolean;
    FDataSet: TReportInternalDataSet;
    FPatterns: TReportInternalDataSet;

    FProcess: Boolean;
    FTerminated: Boolean;
    FOpenAfterSave: Boolean;
    FProcNamePriority: TProcNamePriority;
    FStatusMessages: Boolean;
    FDisableSystemDataSet: Boolean;
    FLastAction: TReportAction;
    FMergeInfo: TReportMergeInfo;
    FFileNames: TStrings;
    FOriginalFileNames: TStrings;
//    FPatternFileName: String;
    FInfoMessageEvent: TInternalConnectionOnInfoMessage;
    FMultiThread: Boolean;
    FOnLogMessage: TRepLogMessageEvent;
    FDataSetCachedUpdates: Boolean;
    FCopyAllDataSet: Boolean;
    FPrintIfEmpty: Boolean;
    FRunning: Boolean;
    {$IFNDEF REPORT_EXTERNAL}
    FPatternTypeListParams: TNamedVariants;
    FPatternTypeList: TReportInternalDataSet;
    {$ENDIF}

//    function GetCaption: String;
    function GetVariables(const Name: String): Variant;
    procedure SetVariables(const Name: String; const Value: Variant);
    function GetDataSets(Index: Integer): TDataSet;
    function GetDataSetCount: Integer;
    function GetVariablesCount: Integer;
    {$IFNDEF REPORT_EXTERNAL}
    function GetOwnerForm: TForm;
    {$ENDIF}

    function GetReportParams(Index: String): Variant;
    procedure SetReportParams(Index: String; const Value: Variant);
  protected
    // Events
    procedure Wait;
    procedure DoLogMessage(const Msg: String; MessageType: TReportMessageType); virtual;
    procedure ProgressFormCancelClick(Sender: TObject);
    procedure DoParentFormResize(Sender: TObject); virtual;
    procedure DoInfoMessageEvent(AConnection: TInternalConnection; const AInfoMessage: String); virtual;
    // Internal
    function CurrentObjectID: Integer;
    function InternalOpenNext(ADataSetProvider: TSQLDataSetProvider{; out CanOpenNext: Boolean}): Boolean;
    {$IFNDEF REPORT_EXTERNAL}class {$ENDIF}function InternalExecProc(const ASQLText: String; const AParams: TNamedVariants; ADataSet: TReportInternalDataSet = nil): TReportInternalDataSet;
    procedure InternalMessage(Sender: TObject; const Message: String; MessageType: TreportMessageType);
    procedure InternalDoAction(AAction: TReportAction; APattern: IPattern = nil);
    // IReport
    function SupportProperty(Name: String): Boolean;
    function GetProperty(Name: String): Variant;
    function GetPageDataSet: TDataSet;
    function GetPageDataSetFieldName: String;
    procedure Progress(const AText: String; Pos, Count: Integer; var Cancel: Boolean);
    property Terminated: Boolean read FTerminated write FTerminated;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearData;
    {$IFNDEF REPORT_EXTERNAL}
    procedure GetPatternTypeList(AFormats: TDataSet);
    {$ENDIF}
    function GetMailConfig(const AParams: Array of TNamedVariant): TRepMailConfig; overload;
    function GetMailConfig(const AParams: TNamedVariants): TRepMailConfig; overload;
    procedure Terminate;

    // Prepare
    procedure PrepareReport;
    function  Prepared: Boolean;
    function  ObjectParams: String;
    procedure UnPrepareReport;
    // Open
    procedure OpenReport; overload; virtual;

    procedure OpenPattern(AParams: TNamedVariants); overload; virtual;
    // Build
    function BuildReport(const AOverrideProcedure: TNamedVariants): Boolean; overload;
    function BuildReportNext(ANextRowSet: Boolean = True): Boolean;
    // Output
    procedure PrepareParams;
    procedure ShowReport(AShowModal: Boolean = False);
    procedure SaveReport(const AFileName: String; AReplaceType: TRepReplaceType = rrtReplace); overload;
    procedure SaveReport(AStream: TStream); overload;
    procedure PrintReport(AShowDialog: Boolean = False);
    // File
    function PrepareSaveFileName(const AFileName: String): String;
    procedure PreparePatternStorage(var APatternStorage: TPatternStorage);
    class function CreateTempFileName(const AFileName: String; const AExt: String = ''; AFileIndex: Integer = 0): String;
    function CreateFileName(const AFileName, AExt: String; AReplaceType: TRepReplaceType): String;
    // Messages
    function ShowMessage(const Msg: String; MessageType: TReportMessageType = rmtError): Boolean;
    procedure UnsupportedOperation;
    // Process
    function InProcess: Boolean;
    procedure BeginProcess(ShowProgress: Boolean = True{$IFNDEF REPORT_EXTERNAL}; Parent: TWinControl = nil{$ENDIF}); virtual;
    procedure EndProcess; virtual;
    // SQL
    function GetMainConnectionProvider: TSQLConnectionProvider;
    procedure SetMainConnectionProvider(const Value: TSQLConnectionProvider);
    function GetConnectionProvider: TSQLConnectionProvider;
    procedure Connect(const AProcedureName: String = '');
    procedure Disconnect;
//{IFNDEF REPORT_EXTERNAL}
//    procedure InsertFieldValues(const ATableName, AFieldNames: String; AFields: Array of TField);
//{ENDIF}
//    function GetFieldValuesStr(AField:TField; ADelim: string): widestring;
    // Data
    property Data: TReportData read FData;
    // DataSets
    function DataSetsIsEmpty: Boolean; virtual;
    function ExportDataSet(Index: Integer): TDataSet;
    procedure DisableSystemDataSet;

    procedure ClearDataSets; inline;
    procedure AssignDataSets(const ADataSets: Array of TDataSet); overload;
    procedure AssignDataSets(const ADataSets: Variant); overload;
    property DataSetCount: Integer read GetDataSetCount;
    property DataSets[Index: Integer]: TDataSet read GetDataSets;

    // Variables
    procedure AssignVariables(const AVariables: TNamedVariants);
    property VariablesCount: Integer read GetVariablesCount;
    property Variables[const Name: String]: Variant read GetVariables write SetVariables;

    // Params
    procedure ClearReportParams;
    procedure AssignReportParams(const AParams: TNamedVariants);
    { Входящие параметры отчёта (?) }
    property ReportParams[Index: String]: Variant read GetReportParams write SetReportParams;

    // Report property
//    property Caption: String read GetCaption;
    property FileNames: TStrings read FFileNames;
    property OriginalFileNames: TStrings read FOriginalFileNames;
//    property PatternFileName: String read FPatternFileName write FPatternFileName;
    property ConnectionProvider: TSQLConnectionProvider read GetConnectionProvider;
    property MainConnectionProvider: TSQLConnectionProvider read GetMainConnectionProvider write SetMainConnectionProvider;
    property DataSetCachedUpdates: Boolean read FDataSetCachedUpdates write FDataSetCachedUpdates;
    {$IFNDEF REPORT_EXTERNAL}
    property OwnerForm: TForm read FOwnerForm write FOwnerForm;
    {$ENDIF}
    property ProcNamePriority: TProcNamePriority read FProcNamePriority write FProcNamePriority default pnpServer;
  public
    class function CreateByParams(AOwner: TComponent; const AParams: Array of TNamedVariant): TComponent; overload;
    class function CreateByParams(AOwner: TComponent; AParams: TNamedVariants): TComponent; overload; dynamic;
  published
    {$IFNDEF REPORT_EXTERNAL}
    { Реализация интерфейса ICreateByParams. Пропустить не глядя. }
    function  IVirtualClassOwner(var AVirtualClassName: String; var AParams: TNamedVariants): TPersistentClass;
    function  ICreateByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants): TComponent;
    function  ISelectByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants; ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants;

    function  IGetCreateParams: TNamedVariants;
    procedure IShow;
    {$ENDIF}
  published
    property ReadOnly: Boolean read FReadOnly write FReadOnly default True;
    property PrintIfEmpty: Boolean read FPrintIfEmpty write FPrintIfEmpty default False;
//    property MultiThread: Boolean read FMultiThread write FMultiThread default False;
    property CopyAllDataSet: Boolean read FCopyAllDataSet write FCopyAllDataSet default False;
    property StatusMessages: Boolean read FStatusMessages write FStatusMessages default True;
    property OpenAfterSave: Boolean read FOpenAfterSave write FOpenAfterSave default False;
    property OnLogMessage: TRepLogMessageEvent read FOnLogMessage write FOnLogMessage;
  end;

// Reports
//function RepParam(const Name: String; const Value: Variant): TNamedVariant;
//function RepVar(const Name: String; const Value: Variant): TNamedVariant;
//function RepTempFile(const FileName: String): String;

{$IFNDEF REPORT_EXTERNAL}
function RepMessage(const Msg: String; MessageType: TReportMessageType = rmtError{$IFNDEF REPORT_EXTERNAL}; AOwner: TForm = nil{$ENDIF}): Boolean;
//function RepMessageFmt(const Format: String; const Args: Array of const; MessageType: TReportMessageType = rmtError): Boolean;
function RepNewDirName(const DirName: String = ''{$IFNDEF REPORT_EXTERNAL}; AOwner: TForm = nil{$ENDIF}): String;
//function RepNewFileName(const FileName: String = ''; const FileExt: String = ''): String;

// CheckValue
//function CheckDate(const ADate: Variant; AEdit: TWinControl = nil; const AMessage: String = ''): Boolean;
//function CheckDatesPeriod(const ADateBegin, ADateEnd: Variant; AEdit1: TWinControl = nil; AEdit2: TWinControl = nil; const AMessage: String = ''): Boolean;
//function CheckNotNull(const AValue: Variant; AEdit: TWinControl = nil; const AMessage: String = ''): Boolean;
//function CheckNotEmpty(ADataSet: TDataSet; AEdit: TWinControl = nil; const AMessage: String = ''): Boolean;

// Utils
//function GetFieldString(AField: TField): String;
//function GetItemString(const Str: String; var APos: Integer; Delimeter: Char): String;
//procedure SetFieldString(AField: TField; const Text: String);
//function ListElementChecked(const Conditions: Array of Boolean; const Values: Array of Variant; DefaultValue: Variant): Variant;
{$ENDIF}
//function FieldValuesToStr(AFieldName: String; ADataSet: TDataSet; ADelim: string): WideString;

{$IFNDEF REPORT_EXTERNAL}
// SQL
// procedure ExecProcessSQL(const Text: String; const Params: Array of Variant; Connection: TSQLConnectionProvider = nil);
//procedure InsertFieldValues(const ATableName, AFieldNames: String; AFields: Array of TField; AConnectionProvider: TSQLConnectionProvider = nil);

// Mail
procedure OpenMail(const EMailTo, Subject, TextBody, HTMLBody, FileNames: String);
{$ENDIF}
function GetMailConfig
         (
          const Server, EMailFrom: String; const UserName: String = ''; const Password: String = '';
          ConnectionTimeout: Integer = 60; ServerPort: Integer = 25; const ProxyServer: String = ''; const ProxyBypass: String = ''
         ): TRepMailConfig;
procedure SendMail(Config: TRepMailConfig; const EMailTo, Subject, TextBody, HTMLBody, FileNames: String);

// System
procedure RegisterFactoryPatterns(FactoryPatterns: IFactoryPatterns);
//procedure RegisterReportForm(AClass: TComponentClass);

const
  ReportDelimeterList: Char = ';';
//  ReportDelimeterItem: Char = ',';
//  ReportDelimeterValue: Char = '=';
//  ReportQuotedList: Char = '"';
//  ReportMaxCountListConfig = 1000;
  ReportMaxCountListOpen = 5;
  ReportMaxCountListMail = 5;

//  ReportParamListOperator_Union = 0;
//  ReportParamListOperator_Intersect = 1;
//  ReportParamListOperator_Except = 2;

resourcestring
  SRepVersion = '6.1';
  SRepDefaulFileName = 'Report';

  // Var names
  SRepParam_Report = 'Report';

  // Params
//  SRepParam_CodePageFrom = 'CodePageFrom';
//  SRepParam_CodePageTo = 'CodePageTo';

//  SRepParam_List_Pattern  = '%s="%s%s"';
//  SRepParam_List_Delim = ', ';

  // Action
  SRepParam_Action  = 'Action';
  SRepAction_Show   = 'Show';
  SRepAction_Save   = 'Save';
  SRepAction_Stream = 'Stream';
//  SRepAction_Run = 'Run';
  SRepAction_Build  = 'Build';

  // Sys params
//  SConst_PatternType_Id   = 'PatternType_Id';
//  SConst_PatternType_Code = 'PatternType_Code';


  SRepParam_Caption         = '#Report#Object#Caption';
  SRepParam_Params          = '#Report#Params';
  SRepParam_Merge           = '#Report#Merge';

  SRepParam_ClassName       = '#Report#ClassName';

  SReportParam_Empty          = '#Report#EmptyRecordset';
  SRepSysParam_PrintIfEmpty   = '#Report#PrintIfEmpty';
  SRepSysParam_CopyAllDataSet = '#Report#CopyAllDataSet';

//  SRepParam_ObjectName      = '#Report#Object#Name';

  SRepParam_VersionId       = '#Report#Version#Id';
//  SRepParam_VersionName     = '#Report#Version#Name';

  SRepParam_PatternId       = '#Report#Pattern#Id';
  SRepParam_PatternName     = '#Report#Pattern#Name';
  SRepPatternCaption        = '#Report#Pattern#Caption';
  SRepPatternIconIndex      = '#Report#Pattern#IconIndex';
  SRepPatternDefault        = '#Report#Pattern#Default';

  SRepParam_PatternTypeCode = '#Report#Pattern#Type#Code';
  SRepParam_PatternFileName = '#Report#Pattern#File#Name';
  SRepParam_PatternBinary   = '#Report#Pattern#Binary';
//  SRepParam_PatternFileAddon= '#Report#Pattern#File#Addon';
  SRepParam_PatternFilePath = '#Report#Pattern#File#Path';

  SRepParam_ScriptFileName  = '#Report#Script#File#Name';
  SRepParam_ScriptBinary    = '#Report#Script#Binary';

  SRepParam_SaveEncoding    = '#Report#Save#Encoding';
  SRepParam_SaveFileName    = '#Report#Save#File#Name';
//  SRepParam_SaveFileAddon   = '#Report#Save#File#Addon';
  SRepParam_SaveStream      = '#Report#Save#Stream';

  SRepParam_ProcedureName   = '#Report#Procedure#Name';
  SRepParam_ProcedureParams = '#Report#Procedure#Params';

  SRepParam_BuildInThread   = '#Report#Thread';

  SRepParam_DataSets        = '#Report#DataSets';
  SRepParam_DataSetCount    = '#Report#DataSets#Count';
//  SRepParam_FileName        = '#Report#File#Name';

//  SRepSysParam_AbsolutePath = 'is_asolute_path';

  // Dialogs
//  SRepCaption = 'Отчет';
//  SRepDialogEmptyReport = 'Отчет не содержит данных, выводить?';
//  SRepDialogCaption = 'Подождите, идет сохранение документа ..... ';
//  SRepDialogDirCaption = 'Сохранить отчет в ...';
//  SRepDialogDirSelect = 'Выберите папку, куда следует сохранить отчет';
//  SRepDialogFileFilter = 'Список параметров отчета(*.rlp)|*.rlp|Все файлы|*.*';
//  SRepDialogFileDefaultExt = '*.rlp';

  //  Report states
  SReportState_Prepare = 'Формирование отчета ...';
  SReportState_Save = 'Сохранение отчета ...';
  SReportState_Print = 'Печать отчета ...';

  // Error messages
  SRepErrorNoConnect = 'Ошибка подключения !';
  SRepErrorRunning = 'Отчет уже выполняется !';
//  SRepErrorNoDate = 'Не указана дата !';
//  SRepErrorPeriodDate = 'Неверный диапазон дат !';
//  SRepErrorNoPriceType = 'Не указана цена !';
//  SRepErrorNoParams = 'Не указаны необходимые параметры !';
//  SRepErrorEmptyList = 'Не заполнен список !';
//  SRepErrorStreamError = 'Неверный формат файла';
  SRepErrorUnsupportedOperation = 'Операция недоступна для данного типа шаблона';

var
// Proc names
  ReportSQL_ObjectInfo    : String = 'EXEC [Reports].[Object::Info] @Id = :[#Report#Object#Id], @Code = :[#Report#Object#Code]';
  ReportSQL_VersionInfo   : String = 'DECLARE @Params TParams = Cast(:[#Report#Params] AS TParams); ' + 'EXEC [Reports].[Object::Version::Info] @Interface = :[Interface:Language], @Object_Id = :[#Report#Object#Id], @Date = :[#Report#Date], @Params = @Params';
  ReportSQL_Patterns      : String = 'DECLARE @Params TParams = Cast(:[#Report#Params] AS TParams); ' + 'EXEC [Reports].[Object::Patterns::Info] @Interface = :[Interface:Language], @Object_Id = :[#Report#Object#Id], @Params = @Params, @Type_Code = :[#Report#Pattern#Type#Code], @Date = :[#Report#Date]';

  ReportSQL_PatternTypes  : String = 'DECLARE @Params TParams = Cast(:[#Report#Params] AS TParams); ' + 'EXEC [Reports].[Object::Patterns::Type List] @Interface = :[Interface:Language], @Object_Id = :[#Report#Object#Id], @Date = :[#Report#Date], @Params = @Params';
  ReportSQL_MailConfig    : String = 'EXEC [Reports].[sp_mail_config]';

  ReportShowEmptyMessage  : Boolean = True;

//  ReportSQL_ObjectVersionInfo : String = 'EXEC [Reports].[Object::Version::Info] @Id = :[#Report#Object#Id], @Date = :[#Report#Date], @Params = :[#Report#Params]';

implementation

uses
  Windows, Types, Variants, TypInfo, StrUtils, FileUtil, Printers,
  {$IFNDEF REPORT_EXTERNAL}
  SysConst,
  {$ENDIF}
  //ShellAPI, ShlObj,
  ComObj, ActiveX,
  SBaseVariantFunctions, SBaseConstants, SBaseFileUtils, SBaseStringFunctions,
  SBaseUtils;

const
  cdoSendUsingPickup = 1;
  cdoSendUsingPort = 2;
  cdoAnonymous = 0;
  cdoBasic = 1;

  SReportFieldPrefixLength   = 2;
  SReportFieldPrefix: String = '#.';

resourcestring
  SRepParam_Mask = '@%s = %s';

  //File
  SRepFileBackupExt = '.BAK';
  SRepFilePrefixCopy = '~';
  SRepFilePagePostfix = ' (Стр. %d из %d)';

  // DataSet Names
  SReportDataSet_Page = 'Page';
  SReportDataSet_Report = 'Report';

  // Process
  //  SRepProcessText = 'Подготовка данных отчета...';

  // Errors
  SReportError_NoStoredProc = 'Нет процедуры ''%s'' или прав доступа к ней на сервере ''%s''';
  SReportError_DetailFields = 'Не найдено поле ''%s'' для Detail набора данных ''%s''';
  SReportError_DataSetFilter = 'Ошибка формирования фильтра для набора данных ''%s'' для фильтра ''%s'': %s';
  SReportError_FileNameEmpty = 'Не указано имя файла (директории) для сохранения';
  SReportError_ObjectNotFound = 'Не найден отчет с параметрами: %s';
  SReportError_PatternEmptyCode = 'Задан пустой код для шаблона';
  SReportError_PatternUnknownType = 'Неизвестный тип шаблона ''%s''';
  SReportError_Pattern = 'Ошибка в шаблоне ''%s'': ''%s''';

  // TODO: Remove !!!
  SReportError_InsertFieldCount = 'Неверное количество полей для вствки в таблицу ''%s''';
  SReportError_CreateDir = 'Не удается создать директорию ''%s'': %s';
  SReportError_DocFormNoFound = 'Не найдена форма документа c параметрами: %s';
  SReportError_DocFormNotRegister = 'Класс ''%s'' не зарегестрирован в системе отчетности';
  SReportError_DocFormNoInterface = 'Класс ''%s'' не поддреживает интерфейс ''IRepForm''';
  SReportError_SQLDefNoScript = 'Не найден скипт SQLDEF ''%s''';

  // Message prefix
  SReportMessagePrefix_Error = 'ERROR:';
  SReportMessagePrefix_Warning = 'WARNING:';
  SReportMessagePrefix_Info = 'INFO:';
  SReportMessagePrefix_Status = 'STATUS:';
  SReportMessagePrefix_ProgressCount = 'PROGRESS_COUNT:';
  SReportMessagePrefix_ProgressStep = 'PROGRESS_POS:';

  // Form Params
//  SRepFormParam_FormID = 'form_id';
//  SRepFormParam_ClassName = 'class_name';
//  SRepFormParam_Params = 'params';
//  SRepFormParam_ParamName = 'name';
//  SRepFormParam_ParamType = 'param_type';
//  SRepFormParam_Identity = 'identity';
//  SRepFormParam_ParamCaption = 'caption';
//  SRepFormParam_ParamValue = 'value';
//  SRepFormParam_ParamDefaultValue = 'default_value';
//  SRepFormParam_ParamNewRow = 'is_new_row';

  // Report Params
//  SReportParam_ReportParams = 'ReportParams';
  SReportFieldPrefix_Variable = 'Variable#';
//  SReportParam_FileName = 'FileName';

  // Report DataSet Params
  SReportDataSetParam_SystemRecordset = 'SystemRecordset';
  SReportDataSetParam_VariableRecordset = 'VariableRecordset';
  SReportDataSetParam_ReportRecordset = 'ReportRecordset';
  SReportDataSetParam_PageRecordset = 'PageRecordset';
  SReportDataSetParam_CloneRecordset  = 'CloneRecordSet';
  SReportDataSetParam_MasterDetailLinks = 'MasterDetailLinks';
  SReportDataSetParam_MasterDetailRecordset = 'MasterDetailRecordset';
  SReportDataSetParam_MasterRecordset = 'MasterRecordset';
  SReportDataSetParam_GlobalRecordset = 'GlobalRecordset';
  SReportDataSetParam_Page = 'Page';

  // Prefix Field Params
  SReportFieldPrefix_MasterDetailLink = 'Link_';
  SReportFieldPrefix_DetailField      = 'Detail_';

  // Mail Params
  SReportParamMail_Server = 'Server';
  SReportParamMail_EMail = 'EMail';
  SReportParamMail_UserName = 'UserName';
  SReportParamMail_Password = 'Password';
  SReportParamMail_ConnectionTimeout = 'ConnectionTimeout';
  SReportParamMail_ServerPort = 'ServerPort';
  SReportParamMail_ProxyServer = 'ProxyServer';
  SReportParamMail_ProxyBypass = 'ProxyBypass';

  // Mail Constants
  SReportMail_OLEClassNameOutlook = 'Outlook.Application';
  SReportMail_OLEClassNameMessage = 'CDO.Message';
  SReportMail_OLEClassNameConfig = 'CDO.Configuration';

  SReportMail_cdoSendUsingMethod = 'http://schemas.microsoft.com/cdo/configuration/sendusing';
  SReportMail_cdoSMTPServer = 'http://schemas.microsoft.com/cdo/configuration/smtpserver';
  SReportMail_cdoSMTPConnectionTimeout = 'http://schemas.microsoft.com/cdo/configuration/smtpconnectiontimeout';
  SReportMail_cdoSMTPServerPort = 'http://schemas.microsoft.com/cdo/configuration/smtpserverport';
  SReportMail_cdoSMTPAuthenticate = 'http://schemas.microsoft.com/cdo/configuration/smtpauthenticate';
  SReportMail_cdoSendUserName = 'http://schemas.microsoft.com/cdo/configuration/sendusername';
  SReportMail_cdoSendPassword = 'http://schemas.microsoft.com/cdo/configuration/sendpassword';
  SReportMail_cdoURLProxyServer = 'http://schemas.microsoft.com/cdo/configuration/urlproxyserver';
  SReportMail_cdoURLProxyBypass = 'http://schemas.microsoft.com/cdo/configuration/urlproxybypass';
  SReportMail_cdoURLGetLatestVersion = 'http://schemas.microsoft.com/cdo/configuration/urlgetlatestversion';

type
  TComponentAcess = class(TComponent);

  TPatternInfo = packed record
    Factory: IFactoryPatterns;
    Index: Integer;
    Code: String;
  end;
  PPatternInfo = ^TPatternInfo;

var
  FRegisteredPatterns: TList = nil;
//  FReportForms: Array of TComponentClass;

// System
function GetPatternInfo(const Code: String): PPatternInfo;
var
  i: Integer;
begin
  Result := nil;
  if not Assigned(FRegisteredPatterns) then exit;
  with FRegisteredPatterns do
  for i := Pred(Count) downto 0 do
  if AnsiSameStr(PPatternInfo(Items[i]).Code, Code) then
  begin
    Result := Items[i];
    break;
  end;
end;

procedure RegisterFactoryPatterns(FactoryPatterns: IFactoryPatterns);
var
  i: Integer;
  APatternInfo: PPatternInfo;
begin
  if Assigned(FactoryPatterns) then
  begin
    if not Assigned(FRegisteredPatterns) then FRegisteredPatterns := TList.Create;
    with FactoryPatterns do
      for i := 0 to Pred(PatternCount) do
      if not Assigned(GetPatternInfo(PatternCode(i))) then
      begin
        New(APatternInfo);
        APatternInfo.Code := PatternCode(i);
        APatternInfo.Factory := FactoryPatterns;
        APatternInfo.Index := i;
        FRegisteredPatterns.Add(APatternInfo);
      end;
  end;
end;

procedure DestroyFactoryPatterns;
var
  i: Integer;
begin
  if Assigned(FRegisteredPatterns) then
  with FRegisteredPatterns do
    for i := Pred(Count) downto 0 do
      Dispose(PPatternInfo(Items[i]));
  FreeAndNil(FRegisteredPatterns);
end;

// Mail
function GetMailConfig(const Server, EMailFrom: String; const UserName: String = ''; const Password: String = '';
  ConnectionTimeout: Integer = 60; ServerPort: Integer = 25; const ProxyServer: String = ''; const ProxyBypass: String = ''): TRepMailConfig;
begin
  Result.Server := Server;
  Result.UserName := UserName;
  Result.Password := Password;
  Result.ConnectionTimeout := ConnectionTimeout;
  Result.ServerPort := ServerPort;
  Result.ProxyServer := ProxyServer;
  Result.ProxyBypass := ProxyBypass;
end;

procedure SendMail(Config: TRepMailConfig; const EMailTo, Subject, TextBody, HTMLBody, FileNames: String);
var
//  i: Integer;
  LStringItem: TStringItem;
//  AFileNames: TStrings;
  iMsg, iConf, Flds: OLEVariant;
begin
  iMsg := CreateOLEObject(SReportMail_OLEClassNameMessage);
  iConf := CreateOLEObject(SReportMail_OLEClassNameConfig);
  Flds := iConf.Fields;
  // Server
  Flds.Item(SReportMail_cdoSendUsingMethod) := cdoSendUsingPort;
  Flds.Item(SReportMail_cdoSMTPServer) := Config.Server;
  Flds.Item(SReportMail_cdoSMTPConnectionTimeout) := Config.ConnectionTimeout;
  Flds.Item(SReportMail_cdoSMTPServerPort) := Config.ServerPort;
  // User
  if String.IsNullOrWhiteSpace(Config.UserName) then
    Flds.Item(SReportMail_cdoSMTPAuthenticate) := cdoAnonymous
  else
  begin
    Flds.Item(SReportMail_cdoSendUserName) := Config.UserName;
    Flds.Item(SReportMail_cdoSendPassword) := Config.Password;
  end;
  // Proxy
  if not String.IsNullOrWhiteSpace(Config.ProxyServer) then
  begin
    Flds.Item(SReportMail_cdoURLProxyServer) := Config.ProxyServer;
    Flds.Item(SReportMail_cdoURLProxyBypass) := Config.ProxyBypass;
    Flds.Item(SReportMail_cdoURLGetLatestVersion) := True;
  end;
  Flds.Update;
  // Message
  iMsg.Configuration := iConf;
  iMsg.To := EMailTo;
  iMsg.From := Config.EMail;
  iMsg.Subject := Subject;
  if not String.IsNullOrWhiteSpace(HTMLBody) then
    iMsg.HtmlBody := HTMLBody
  else
    iMsg.TextBody := TextBody;

  for LStringItem in SBaseStringFunctions.EnumStringItems(FileNames, ReportDelimeterList) do
    iMsg.AddAttachment(LStringItem.Value);
  iMsg.Send;
end;

procedure OpenMail(const EMailTo, Subject, TextBody, HTMLBody, FileNames: String);
var
//  i: Integer;
//  AFileNames: TStrings;
  LStringItem: TStringItem;
  OutlookApp, MailItem: OLEVariant;
begin
  OutlookApp := CreateOleObject(SReportMail_OLEClassNameOutlook);
  MailItem := OutlookApp.CreateItem(0);
  MailItem.To := EMailTo;
  MailItem.Subject := Subject;
  MailItem.Body := TextBody;
  MailItem.HTMLBody := HTMLBody;

  for LStringItem in SBaseStringFunctions.EnumStringItems(FileNames, ReportDelimeterList) do
    MailItem.Attachments.Add(LStringItem.Value, 1, 1);

//  try
  MailItem.Display;
//  except
    // TODO: Error ???!!!
//  end;
end;

function RepMessage(const Msg: String; MessageType: TReportMessageType{$IFNDEF REPORT_EXTERNAL}; AOwner: TForm{$ENDIF}): Boolean;
begin
  Result := True;
{$IFNDEF REPORT_EXTERNAL}
  case MessageType of
    rmtError:
      begin
        SBaseMainFormMDIInterface.Log(lgError, Msg, False);
        exit;
      end;
    rmtWarning:
      begin
        SBaseMainFormMDIInterface.Log(lgWarning, Msg, False);
        exit;
      end;
    rmtInfo:
      begin
        SBaseMainFormMDIInterface.Log(lgInfo, Msg, False);
        exit;
      end;
  else
    exit;
  end;
{$ENDIF}
end;

{$IFNDEF REPORT_EXTERNAL}
function RepNewDirName(const DirName: String = ''{$IFNDEF REPORT_EXTERNAL}; AOwner: TForm = nil{$ENDIF}): String;
begin
  with TFileOpenDialog.Create(nil) do
  try
    Options := [fdoPickFolders];
//    DefaultFolder := VarToStr(Caller.Params[1]);
    if Execute then
      Result := IncludeTrailingPathDelimiter(String(FileName).Trim)
    else
      Result := '';
  finally
    Free;
  end;
end;

{$ENDIF}

// Converts
function VarToRepMessageType(const Value: Variant): TReportMessageType;
begin
  if (not VarIsStr(Value)) or (Length(Value) <> 1) then
    Result := rmtWarning
  else
    case VarToStr(Value)[1] of
      lgError : Result := rmtError;
      lgInfo  : Result := rmtInfo;
      else Result := rmtWarning;
    end;
end;

function ExtractProcName(ACommandText: String): String;
var
  I, J: Integer;
  LSubStr: String;
begin
  I := Pos(' ', ACommandText);
  if I = 0 then
    Result := ACommandText
  else begin
    LSubStr := LeftStr(ACommandText, I-1);
    if SameText(LSubStr, SConst_Exec) or SameText(LSubStr, SConst_Call) then
      Result := LSubStr
    else begin
      J := PosEx(' ', ACommandText, I+1);
      if J = 0 then
        Result := Copy(ACommandText, I+1, MAXINT)
      else
        Result := Copy(ACommandText, I+1, J-I-1)
    end;

  end;
end;

{ TReportInternalData }
constructor TReportInternalData.Create(AOwner: TComponent);
begin
  inherited;
  FQuickMasterDetail := False;
end;

procedure TReportInternalData.SetQuickMasterDetail;
begin
  FQuickMasterDetail := True;
end;

function TReportInternalData.EOF: Boolean;
begin
  if FQuickMasterDetail and Assigned(FMasterDataSet) then
    Result := inherited EOF or FieldValues[FDetailFieldNames] <> FMasterValues
  else
    Result := inherited EOF
end;

procedure TReportInternalData.First;
begin
  if not(FQuickMasterDetail and Assigned(FMasterDataSet)) then
    inherited First;
end;

procedure TReportInternalData.Next;
begin
  if FQuickMasterDetail and Assigned(FMasterDataSet) then
    begin
     if not EOF then inherited Next;
    end
  else
    inherited Next;
end;

{ TReportDataSet }
constructor TReportDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDetailLinks := TObjectList.Create;
  if AOwner is TReportData then FData := TReportData(AOwner);
end;

destructor TReportDataSet.Destroy;
var
  LNotify: TDataSetNotifyEvent;
  LIndex: Integer;
begin
  LNotify := InternalDataSetAfterScroll;
  if Assigned(FDataSet) and (@FDataSet.AfterScroll = @LNotify) then FDataSet.AfterScroll := FDataSetAfterScroll;
  FreeAndNil(FDetailLinks);
  if Assigned(FData) and Assigned(FData.FList) then
    with FData.FList do begin
      LIndex := IndexOf(Self);
      if LIndex >= 0 then
        Delete(LIndex);
    end;
  inherited;
end;

function TReportDataSet.GetName: String;
begin
  if Assigned(FDataSet) then
    Result := DataSet.Name
  else
    Result := '';
end;

procedure TReportDataSet.LoadFromDataSet(Source: TDataSet; RowCount: Integer = -1);
begin
  Assert(Source.Active and (FDataSet <> Source));

  if Assigned(FDataSet) then
    FreeAndNil(FDataSet);
  FDataSet := TReportInternalData.Create(Self);

  TReportInternalData(FDataSet).Load(Source, RowCount);
  FDataSet.Active := True;
end;

procedure TReportDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) and (AComponent = FDataSet) then
    FDataSet := nil;
end;

procedure TReportDataSet.SetDataSet(const Value: TDataSet);
begin
  if Assigned(FDataSet) and (FDataSet.Owner = Self) then
    FreeAndNil(FDataSet);
  FDetailLinks.Clear;
  FDataSet := Value;
end;

procedure TReportDataSet.AddDetail(ADataSet: TReportDataSet; const AFields: String; Sorted: Boolean);
var
  I: Integer;
  LFieldName: String;
  LLinks: TReportDataSetLink;
  LNotify: TDataSetNotifyEvent;
begin
  LLinks := TReportDataSetLink.Create;
  try
    LLinks.Sorted := Sorted;
    LLinks.Delimiter := ';';
    LLinks.DelimitedText := AFields;
    LLinks.DataSet := ADataSet.DataSet;
    for I := 0 to Pred(LLinks.Count) do begin
      if LLinks.Names[i].IsEmpty then
        LFieldName := LLinks.ValueFromIndex[I]
      else
        LFieldName := LLinks.Names[I];
      if not ADataSet.DataSet.Active then ADataSet.DataSet.Active := True;
      LLinks[I] := ADataSet.DataSet.FieldByName(LLinks.ValueFromIndex[I]).FieldName;

      if not FDataSet.Active then FDataSet.Active := True;
      LLinks.Objects[I] := FDataSet.FindField(LFieldName);
      if not Assigned(LLinks.Objects[I]) then
        raise EReportError.CreateFmt(SReportError_DetailFields, [ADataSet.Name, LFieldName]);
    end;
    FDetailLinks.Add(LLinks);
    LNotify := InternalDataSetAfterScroll;
    if @LNotify <> @FDataSet.AfterScroll then
    begin
      FDataSetAfterScroll  := FDataSet.AfterScroll;
      FDataSet.AfterScroll := InternalDataSetAfterScroll;
    end;
  except
    LLinks.Free;
    Raise;
  end;
end;

procedure TReportDataSet.InternalDataSetAfterScroll(ADataSet: TDataSet);

  function GetFilterText(Fields: TStrings): String;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to Pred(Fields.Count) do
    with TField(Fields.Objects[i]) do begin
      Result := ConcatIfNotEmpty(Result, ' AND ', '[' + Fields[I] + ']');

      if IsNull then
        Result := Result + '=Null'
      else if DataType in [ftString, ftDate, ftTime, ftDateTime] then
        Result := Result + '=' + QuotedStr(AsString)
      else
        Result := Result + '=' + AsString;
    end;
  end;

  function GetMasterFieldNames(Fields: TStrings): String;
  var
    i: Integer;
  begin
    Result := '';
    for i := 0 to Pred(Fields.Count) do
    with TField(Fields.Objects[i]) do
    begin
      if Result <> '' then Result := Result + ';';
      Result := Result + FieldName;
    end;
  end;

var
  I: Integer;
  LFilter: String;
begin
  if not FData.Active then exit;

  for i := Pred(FDetailLinks.Count) downto 0 do begin
    if TReportDataSetLink(FDetailLinks[i]).Sorted and
       (TReportDataSetLink(FDetailLinks[i]).DataSet is TReportInternalData) and
       TReportInternalData(TReportDataSetLink(FDetailLinks[i]).DataSet).FQuickMasterDetail then
    // Quick Master-Detail
    with TReportInternalData(TReportDataSetLink(FDetailLinks[i]).DataSet) do begin
      if not Assigned(FMasterDataSet) then begin
        FMasterDataSet := FDataSet;
        FMasterFieldNames := GetMasterFieldNames(TStrings(FDetailLinks[i]));
        FDetailFieldNames := TStrings(FDetailLinks[i]).DelimitedText;
      end;
      FMasterValues := FDataSet[FMasterFieldNames];
    end else begin
      LFilter := GetFilterText(TReportDataSetLink(FDetailLinks[i]));
      with TReportDataSetLink(FDetailLinks[i]).DataSet do
      if not Filtered or (LFilter <> Filter) then
      try
        Filter := LFilter;
        Filtered := True;
      except
        on E: Exception do raise ERepError.CreateFmt(SReportError_DataSetFilter, [Name, LFilter, E.Message]);
      end;
    end;
  end;
  if Assigned(FDataSetAfterScroll) then FDataSetAfterScroll(ADataSet);
end;

{ TReportData }
constructor TReportData.Create(AOwner: TComponent);
begin
  inherited;
  FActive := False;
  FList := TList<TReportDataSet>.Create;

  FVirtualDataSets := TStringList.Create;
  TStringList(FVirtualDataSets).OwnsObjects := True;

  FGlobalDataSets := TList<TReportDataSet>.Create;

  FVariables.UnknownAsNull := True;
  FParams.UnknownAsNull := True;
  FDataSetParams.UnknownAsNull := True;

  FReportDataSet := TReportDataSet.Create(Self);
  FPageDataSet := TReportDataSet.Create(Self);
end;

destructor TReportData.Destroy;
begin
  Clear(True);
  FreeAndNil(FGlobalDataSets);
  FreeAndNil(FVirtualDataSets);
  FreeAndNil(FList);
  inherited;
end;

procedure TReportData.Clear(ClearAll: Boolean);
var
  I   : Integer;
  LDS : TReportDataSet;
begin
  for I := Pred(DataSetCount) downto 0 do
    if ClearAll or (FGlobalDataSets.IndexOf(DataSets[I]) < 0) then begin
      LDS := DataSets[I];
      if Assigned(LDS) //and (LDS.Owner = Self)
      then begin
        with LDS do
          if Assigned(DataSet) and FDataSourceStateBookmark.DataSetState.NotNull then
            LDS.DataSet.EndUpdate(FDataSourceStateBookmark);
        FreeAndNil(LDS);
      end else begin
        FList.Delete(I);
      end;
    end;

  FReportDataSet.DataSet := nil;
  FPageDataSet.DataSet := nil;

//  if ClearAll then begin
//    FParams.Clear;
//    FVariables.Clear;
//  end;

  inherited;
end;

function TReportData.GetDataSet(Index: Integer): TReportDataSet;
begin
  Result := FList.Items[Index];
end;

function TReportData.GetDataSetCount: Integer;
begin
  Result := FList.Count;
end;

function TReportData.GetDataSetByName(const DataSetName: String): TReportDataSet;
var
  i: Integer;
begin
  for i := 0 to Pred(DataSetCount) do
    if SameText(DataSetName, DataSets[i].Name) then
    begin
      Result := DataSets[i];
      exit;
    end;
  Result := nil;
end;

function TReportData.ExportDataSet(Index: Integer): TDataSet;
begin
  Result := DataSets[Index].DataSet;
  if Assigned(Result.Owner) and (Result.Owner is TReportDataSet) then
    TReportDataSet(Result.Owner).RemoveComponent(Result);
end;

function TReportData.InternalAddDataSet(const Value: Variant; DataSetType: TReportDataSetType): Boolean;
begin
  Result := VarIsPresent(Value);
  if Result then begin
    FVirtualDataSets.AddObject(IntToStr(Integer(DataSetType)) + '=' + VarToStr(Value), TNamedVariantsObject.Create(FDataSetParams));
    FDataSetParams.Clear;
  end;
end;

procedure TReportData.PrepareParams(ADataSet: TDataSet);
var
  I, LVarLength: Integer;
  LVarName: String;
begin
  LVarLength := Length(SReportFieldPrefix_Variable);
  for I := Pred(ADataSet.FieldCount) downto 0 do
    with ADataSet.Fields[I] do
      if Copy(FieldName, 1, SReportFieldPrefixLength) = SReportFieldPrefix then begin
        LVarName := Copy(FieldName, 3, MaxInt);
        // Variable
        if SameText(Copy(LVarName, 1, LVarLength), SReportFieldPrefix_Variable) then
          Variables[Copy(LVarName, LVarLength + 1, MaxInt)] := AsVariant
        else
        // Params
        begin
          FParams[LVarName]         := AsVariant;
          FDataSetParams[LVarName]  := AsVariant;
        end;
      end;
end;

function TReportData.PrepareDataSet(ADataSet: TDataSet): Boolean;
var
  I, LVarLength: Integer;
  LVarName: String;
begin
  Result := False;
  if not Assigned(ADataSet) then exit;
  LVarLength := Length(SReportFieldPrefix_Variable);

  if FDataSetParams[SReportDataSetParam_VariableRecordset] = 1 then begin
    for I := Pred(ADataSet.FieldCount) downto 0 do
      with ADataSet.Fields[I] do
        if Copy(FieldName, 1, SReportFieldPrefixLength) = SReportFieldPrefix then
          FVariables[Copy(FieldName, 3, MaxInt)] := AsVariant;
  end else
    for I := Pred(ADataSet.FieldCount) downto 0 do
      with ADataSet.Fields[i] do
        if Copy(FieldName, 1, SReportFieldPrefixLength) = SReportFieldPrefix then begin
          LVarName := Copy(FieldName, 3, MaxInt);
          // Variable
          if SameText(Copy(LVarName, 1, LVarLength), SReportFieldPrefix_Variable) then
            Variables[Copy(LVarName, LVarLength + 1, MaxInt)] := AsVariant
          else if SameText(Copy(LVarName, 1, Length(SReportFieldPrefix_DetailField)), SReportFieldPrefix_DetailField) then begin
            FDataSetParams[SReportDataSetParam_MasterDetailRecordset] :=
              ConcatIfNotEmpty
              (
                VarToStr(FDataSetParams[SReportDataSetParam_MasterDetailRecordset]),
                ';',
                Copy(LVarName, Succ(Length(SReportFieldPrefix_DetailField)), MaxInt)
                + '='
                + FieldName
              )
//            if VarIsNull(FDataSetParams[SReportDataSetParam_MasterDetailRecordset]) then
//              FDataSetParams[SReportDataSetParam_MasterDetailRecordset] :=
//                  Copy(LVarName, Succ(Length(SReportFieldPrefix_DetailField)), MaxInt)
//                  + '='
//                  + FieldName
//            else
//              FDataSetParams[SReportDataSetParam_MasterDetailRecordset] :=
//                  FDataSetParams[SReportDataSetParam_MasterDetailRecordset]
//                  + ';'
//                  + Copy(LVarName, Succ(Length(SReportFieldPrefix_DetailField)), MaxInt)
//                  + '='
//                  + FieldName;
          end else begin
            FParams[LVarName] := AsVariant;
            FDataSetParams[LVarName] := AsVariant;
          end;
        end else begin
          Result := True;

//          if SameText(Copy(FieldName, 1, Length(SReportFieldPrefix_MasterField)), SReportFieldPrefix_MasterField) then
//            FDataSetParams[SReportDataSetParam_MasterDetailRecordset] :=
//              ConcatIfNotEmpty
//              (
//                VarToStr(FDataSetParams[SReportDataSetParam_MasterDetailRecordset]),
//                ';',
//                Copy(FieldName, Succ(Length(SReportFieldPrefix_MasterField)), MaxInt)  + '=' + FieldName
//              )
//          else

          if SameText(Copy(FieldName, 1, Length(SReportFieldPrefix_MasterDetailLink)), SReportFieldPrefix_MasterDetailLink) then
            FDataSetParams[SReportDataSetParam_MasterDetailLinks] :=
              ConcatIfNotEmpty
              (
                VarToStr(FDataSetParams[SReportDataSetParam_MasterDetailLinks]),
                ';',
                Copy(FieldName, Succ(Length(SReportFieldPrefix_MasterDetailLink)), MaxInt)  + '=' + FieldName
              )
//            if VarIsNull(FDataSetParams[SReportDataSetParam_MasterDetailLinks]) then
//              FDataSetParams[SReportDataSetParam_MasterDetailLinks] := Copy(FieldName, Succ(Length(SReportFieldPrefix_MasterDetailLink)), MaxInt) + '=' + FieldName
//            else
//              FDataSetParams[SReportDataSetParam_MasterDetailLinks] := FDataSetParams[SReportDataSetParam_MasterDetailLinks] + ';' + Copy(FieldName, Succ(Length(SReportFieldPrefix_MasterDetailLink)), MaxInt)  + '=' + FieldName;
        end;

  Result := Result and (FDataSetParams[SReportDataSetParam_SystemRecordset] <> 1);

  with ADataSet do
    if Result and (FieldCount = 2) and SameText(Fields[0].FieldName, SConst_Log_Type) and SameText(Fields[1].FieldName, SConst_Log_Message) then begin
      Result := False;
      if Assigned(FOnMessage) then begin
        First;
        while not ADataSet.Eof do begin
          FOnMessage(Self, Fields[1].AsString, VarToRepMessageType(ADataSet.Fields[0].AsString));
          Next;
        end;
      end;
    end;

  InternalAddDataSet(FDataSetParams[SReportDataSetParam_MasterRecordset], rdstMaster);
  InternalAddDataSet(FDataSetParams[SReportDataSetParam_CloneRecordset], rdstClone);
  InternalAddDataSet(FDataSetParams[SReportDataSetParam_ReportRecordset], rdstReport);

  if InternalAddDataSet(FDataSetParams[SReportDataSetParam_PageRecordset], rdstPage) then begin
    if Result then
      FPageDataSetFieldName := SReportFieldPrefix + SReportDataSetParam_PageRecordset
    else
      FPageDataSetFieldName := '';
  end;
end;

function TReportData.AddDataSet(ADataSet: TDataSet; ADataSetSource: TReportDataSetStorage): TReportDataSet;
var
  I, LIndex: Integer;
  LDataSet, LLastDataSet: TReportDataSet;
  LObj: TObject;
begin
  Result := nil;
  if not PrepareDataSet(ADataSet) then exit;
  Result := TReportDataSet.Create(Self);
  LLastDataSet := Result;

  if ADataSetSource = rdssLocalCopy then
    Result.LoadFromDataSet(ADataSet)
  else begin
    Result.DataSet := ADataSet;
    if ADataSetSource = rdssExternal then
      ADataSet.BeginUpdate(Result.FDataSourceStateBookmark);
  end;

  LIndex := FList.Count;
  for I := Pred(FVirtualDataSets.Count) downto 0 do
    case TReportDataSetType(StrToInt(FVirtualDataSets.Names[I])) of
      rdstMaster : begin
                     LDataSet := TReportDataSet.Create(Self);
                     LDataSet.LoadFromDataSet(LLastDataSet.DataSet);
                     LDataSet.AddDetail(LLastDataSet, FVirtualDataSets.ValueFromIndex[I], False);
                     FList.Insert(0, LDataSet);
                     LDataSet.DataSet.Name := SReportDataSetPrefix + IntToStr(LIndex + I);
                     InitDataSet(LDataSet, TNamedVariantsObject(FVirtualDataSets.Objects[I]).Variables);
                     LLastDataSet := LDataSet;
                   end;
      rdstReport : begin
                     FReportDataSet.DataSet := Result.DataSet;
                     Result.FDataSet := nil;
                     Result.LoadFromDataSet(FReportDataSet.DataSet, 1);
                     FReportDataSetLink := Result;
                   end;
      rdstPage   : begin
                     if FPageDataSetFieldName.IsEmpty then
                       FPageDataSetFieldName := FVirtualDataSets.ValueFromIndex[I];
                     FPageDataSet.DataSet := Result.DataSet;
                   end;
      rdstClone  : begin
                     LDataSet := TReportDataSet.Create(Self);
                     LDataSet.LoadFromDataSet(LLastDataSet.DataSet);
                     FList.Insert(0, LDataSet);
                     LDataSet.DataSet.Name := SReportDataSetPrefix + IntToStr(LIndex + I);
                     InitDataSet(LDataSet, TNamedVariantsObject(FVirtualDataSets.Objects[I]).Variables);
                     LLastDataSet := LDataSet;
                   end;
    end;

  for I := Pred(FVirtualDataSets.Count) downto 0 do begin
    LObj := FVirtualDataSets.Objects[I];
    if Assigned(LObj) then begin
      FVirtualDataSets.Objects[i] := nil;
      LObj.Free;
    end;
  end;

  FVirtualDataSets.Clear;
  FList.Add(Result);
  InitDataSet(Result, FDataSetParams);
end;

procedure TReportData.InitDataSet(ADataSet: TReportDataSet; var ADataSetParams: TNamedVariants);
var
//  LIndex: Integer;
  LName: String;
begin
//  LIndex := FList.IndexOf(ADataSet);
//  if (LIndex >= 0) and (ADataSet.DataSet.Name = '') then
//    ADataSet.DataSet.Name := SReportDataSetPrefix + IntToStr(Succ(LIndex));

  if VarIsPresent(ADataSetParams[SReportDataSetParam_MasterDetailLinks]) then
    AddMasterDetailLink(ADataSet, VarToStr(ADataSetParams[SReportDataSetParam_MasterDetailLinks]), True);

  if VarIsPresent(ADataSetParams[SReportDataSetParam_MasterDetailRecordset]) then
    AddMasterDetailLink(ADataSet, VarToStr(ADataSetParams[SReportDataSetParam_MasterDetailRecordset]), False);

  if VarIsPresent(ADataSetParams[SReportDataSetParam_GlobalRecordset]) then begin
    LName := ADataSet.FDataSet.Name;
    ADataSet.FDataSet.Name := '';
    ADataSet.LoadFromDataSet(ADataSet.FDataSet);
    ADataSet.FDataSet.Name := LName;
    FGlobalDataSets.Add(ADataSet);
  end;

  ADataSetParams.Clear;
end;

procedure TReportData.AddMasterDetailLink(ADataSet: TReportDataSet; const AFields: String; Sorted: Boolean);
var
  LNewFields: String;
  LFields: TStrings;

  function CheckFields(ADataSet: TDataSet): Boolean;
  var
    I: Integer;
    LName: String;
  begin
    Result := True;
    LNewFields := '';
    for I := 0 to Pred(LFields.Count) do begin
      LName := LFields.Names[I];
      if not Assigned(ADataSet.FindField(LName)) then begin
        LName := SReportFieldPrefix + SReportFieldPrefix_DetailField + LName;
        if not Assigned(ADataSet.FindField(LName)) then
          Exit(False);
      end;
      LNewFields := ConcatIfNotEmpty(LNewFields, ';', LName + '=' + LFields.ValueFromIndex[I])
    end;
  end;

var
  i, LIndex: Integer;
  LMasterDataSet: TReportDataSet;
begin
  LMasterDataSet := nil;
  LFields := TStringList.Create;
  try
    LFields.Delimiter := ';';
    LFields.DelimitedText := AFields;
    for I := 0 to Pred(LFields.Count) do
      if LFields.Names[I] = '' then LFields[I] := LFields[I] + '=' + LFields[i];

    LIndex := FList.IndexOf(ADataSet);
    if LIndex < 0 then LIndex := Pred(FList.Count);

    while LIndex > 0 do begin
      LIndex := Pred(LIndex);
      LMasterDataSet := GetDataSet(LIndex);
      LMasterDataSet.DataSet.Active := True;
      if CheckFields(LMasterDataSet.DataSet) then
        Break
      else
        LMasterDataSet := nil;
    end;

    if Assigned(LMasterDataSet) then
      LMasterDataSet.AddDetail(ADataSet, LNewFields, Sorted)
    else // Unable to find a data set containing the master field
      Raise Exception.Create('Could not find dataset containing master fields: ' + AFields);
  finally
    LFields.Free;
  end;
end;

procedure TReportData.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

{ TRepThread }
constructor TRepThread.Create(Rep: TSBaseReport);
begin
  FreeOnTerminate := False;
  inherited Create(True);

  FRep := Rep;
end;

procedure TRepThread.DoTerminate;
begin
  inherited;
end;

procedure TRepThread.Execute;
begin
end;

{ TSBaseReport }
constructor TSBaseReport.Create(AOwner: TComponent);
begin
  inherited;

  FRunning := False;
  FData := TReportData.Create(Self);
  FData.OnMessage := InternalMessage;
  FReadOnly := True;
  FCopyAllDataSet := False;
  FPrintIfEmpty := False;
  FMultiThread := False;
  FTerminated := False;
  FProcNamePriority := pnpServer;
  FStatusMessages := True;
  FProcess := False;
  FOpenAfterSave := False;
  FDisableSystemDataSet := False;
  FFileNames := TStringList.Create;
  FOriginalFileNames := TStringList.Create;
  FOriginalFileNames.Delimiter := ReportDelimeterList;
  FFileNames.Delimiter := ReportDelimeterList;
  FDataSetCachedUpdates := False;
  FReportInfo.UnknownAsNull := True;

  FPatterns := TReportInternalDataSet.Create(Self);
  FPatterns.ReadOnly := True;

  FMergeInfo.Value := Null;
  {$IFNDEF REPORT_EXTERNAL}
  FPatterns.ConnectionProvider := SBaseDataModule.BaseSQLConnectionProvider;
  FPatternTypeListParams.UnknownAsNull := True;
  {$ENDIF}
end;

destructor TSBaseReport.Destroy;
begin
  if Assigned(FMergeInfo.Pattern) then InternalDoAction(FLastAction, FMergeInfo.Pattern);
  Disconnect;
  EndProcess;

  FreeAndNil(FDataSet);
  FreeAndNil(FPatterns);
  FreeAndNil(FOriginalFileNames);
  FreeAndNil(FFileNames);
  FReportInfo.Clear;
{$IFNDEF REPORT_EXTERNAL}
  FreeAndNil(FPatternTypeList);
{$ENDIF}
  inherited;
end;

function TSBaseReport.GetMailConfig(const AParams: Array of TNamedVariant): TRepMailConfig;
begin
  Result := GetMailConfig(TNamedVariants.Create(AParams, True));
end;

function TSBaseReport.GetMailConfig(const AParams: TNamedVariants): TRepMailConfig;
var
  LFields: TNamedVariants;
begin
  try
    LFields := MainConnectionProvider.ExecSQL(ReportSQL_MailConfig, []);
    LFields.UnknownAsNull := True;
    try
      Result.Server             := VarToStr(LFields[SReportParamMail_Server]);
      Result.EMail              := VarToStr(LFields[SReportParamMail_EMail]);
      Result.UserName           := VarToStr(LFields[SReportParamMail_UserName]);
      Result.Password           := VarToStr(LFields[SReportParamMail_Password]);
      Result.ConnectionTimeout  := VarToIntDef(LFields[SReportParamMail_ConnectionTimeout], 60);
      Result.ServerPort         := VarToIntDef(LFields[SReportParamMail_ServerPort], 25);
      Result.ProxyServer        := VarToStr(LFields[SReportParamMail_ProxyServer]);
      Result.ProxyBypass        := VarToStr(LFields[SReportParamMail_ProxyBypass]);
    finally
      MainConnectionProvider.CloseSQLQuery{$IFDEF REPORT_EXTERNAL}(ADataSet){$ENDIF};
    end;
  except
    // Ignore all errors
  end;
end;

{$IFNDEF REPORT_EXTERNAL}
procedure TSBaseReport.GetPatternTypeList(AFormats: TDataSet);
var
  LPatternTypeListParamNames: String;
  LPatternTypeListParams: Variant;
begin
  AFormats.DisableControls;
  try
    AFormats.EmptyTable;
    PrepareReport;

    LPatternTypeListParams := VarArrayOf([FData.Params[SRepParam_ObjectId], FData.Params[SRepParam_Date], FData.Params.GetAsClrTParams(ObjectParams)]);
    LPatternTypeListParamNames := SRepParam_ObjectId + ';' + SRepParam_Date + ';' + SRepParam_Params;
    if (FPatternTypeListParams.Count = 0) or (FPatternTypeList = nil) or (not FPatternTypeList.Active)
        or (not SBaseVariantFunctions.VarIsEqual(LPatternTypeListParams, FPatternTypeListParams[LPatternTypeListParamNames]))
    then begin
      FreeAndNil(FPatternTypeList);
      FPatternTypeListParams.Clear;
      FPatternTypeListParams[LPatternTypeListParamNames]:= LPatternTypeListParams;
      FPatternTypeList := InternalExecProc(ReportSQL_PatternTypes, FPatternTypeListParams);
    end;

    AFormats.Load(FPatternTypeList);
  finally
    AFormats.EnableControls;
  end;
end;
{$ENDIF}

function TSBaseReport.GetProperty(Name: String): Variant;
var
  AIndex: Integer;
begin
  Result := FData.FParams.Find(Name, AIndex);
  if Result then
    Result := FData.FParams.Items[AIndex].Value
  else
    Result := GetPropValue(Self, Name);
end;

function TSBaseReport.GetReportParams(Index: String): Variant;
begin
  Result := FData.Params[Index];
end;

function TSBaseReport.SupportProperty(Name: String): Boolean;
var
  AIndex: Integer;
begin
  Result := FData.FParams.Find(Name, AIndex);
  if not Result then
    Result := IsPublishedProp(Self, Name);
end;

function TSBaseReport.CurrentObjectID: Integer;
begin
  Result := FReportInfo[SRepParam_ObjectId];
end;

function TSBaseReport.InternalOpenNext(ADataSetProvider: TSQLDataSetProvider{; out CanOpenNext: Boolean}): Boolean;
begin
  ADataSetProvider.Filter := ''; // Clear Master-Detail
{$IFNDEF REPORT_EXTERNAL}
  if FMultiThread then begin
    ADataSetProvider.Thread.OpenNext;
//      FDataSetThread := TRepDataSetThread.Create(Self, DataSet);
//        FDataSetThread.FreeOnTerminate := False;
//        FDataSetThread.Resume;
    Wait;
    Result := ADataSetProvider.Active;
//    CanOpenNext := Result and ADataSetProvider.!asNextResultSet;
    if FTerminated then Exit;

//    ShowMessage(ADataSetProvider.Thread.LastMessages.Text, rmtInfo);
    DoInfoMessageEvent(nil, ADataSetProvider.Thread.LastMessages.Text);
    if ADataSetProvider.Thread.LastError <> '' then
      Raise ERepError.Create(ADataSetProvider.Thread.LastError);
  end else
{$ENDIF}
    try
      ADataSetProvider.OpenNext;
//      CanOpenNext := ADataSetProvider.!asNextResultSet;
      Result := FDataSet.Active;
    except
      FTerminated := True;
      Raise;
    end;
end;

{$IFNDEF REPORT_EXTERNAL}class {$ENDIF}function TSBaseReport.InternalExecProc(const ASQLText: String; const AParams: TNamedVariants; ADataSet: TReportInternalDataSet = nil): TReportInternalDataSet;
begin
  Result := nil;
  if ASQLText.IsEmpty then exit;

  try
    if Assigned(ADataSet) then
    begin
      Result := ADataSet;
      if ADataSet.Active then ADataSet.Close;
    end else begin
      Result := TReportInternalDataSet.Create(nil);
      Result.ConnectionProvider := SBaseDataModule.BaseSQLConnectionProvider;
      Result.ReadOnly := True;
    end;

    Assert(Assigned(Result.ConnectionProvider));
    Assert(Result.ConnectionProvider.Connected);

    Result.SQL.Text := ASQLText;
    if Result.Params.Count = 0 then
      Raise ERepError.CreateFmt(SReportError_NoStoredProc, [Result.SQL.Text, Result.ConnectionProvider.ConnectionInfo]);

//    Result.PrepareParams(AParams);
    Result.Params.Load(SBaseFormProperties.InterfaceVariables, lvcAlways);
    Result.Params.Load(AParams, lvcIfNotPresent);
    Result.Open;
  except
    if Result <> ADataSet then
      Result.Free;
    Raise;
  end;
end;

procedure TSBaseReport.InternalMessage(Sender: TObject; const Message: String; MessageType: TreportMessageType);
begin
  ShowMessage(Message, MessageType);
end;

procedure TSBaseReport.InternalDoAction(AAction: TReportAction; APattern: IPattern = nil);

  procedure DoPattern(APattern: IPattern; const AFileName: String);
  var
    LNewFileMask, LNewFileName: String;
  begin
    with APattern do begin
      with FPatterns.FieldByName(SRepParam_SaveEncoding) do if not IsNull then
        SetEncoding(AsString);

      case AAction.ActionType of
{$IFNDEF REPORT_EXTERNAL}
        atShow:
          ShowReport(Application.MainForm, AFileName, AAction.ShowModal);
{$ENDIF}
        atSave:
          begin
            if not AFileName.IsEmpty then
              LNewFileMask := AFileName
            else
              LNewFileMask := FPatterns.FieldByName(SRepParam_SaveFileName).AsString;

            FOriginalFileNames.Add(AFileName);
            LNewFileName := CreateFileName(AFileName, '', AAction.ReplaceType);
            SaveReport(LNewFileName);
            FileNames.Add(LNewFileName);

            if OpenAfterSave then
              OpenReport(LNewFileName, AFileName);
          end;
        atStream:
          SaveReport(AAction.Stream);
{$IFNDEF REPORT_EXTERNAL}
        atPrint: PrintReport(AAction.Printer);
{$ENDIF}
      end;
      FMergeInfo.Pattern := nil;
    end;
  end;

var
  I: Integer;
  LPattern: IPattern;
  LPatternTypeCode: String;
  LPatternInfo: PPatternInfo;
  LDataSets: Array of TDataSet;
//  LVariables: TNamedVariants;
  LSaveFileName, LSaveFileDir: String;
  LPatternStorage: TPatternStorage;
begin
  if FTerminated then Exit;

  Assert((AAction.ActionType <> atSave) or (not AAction.FileName.IsEmpty));

  if Assigned(APattern) then
    DoPattern(APattern, AAction.FileName)
  else
    repeat
      PrepareParams;
      if FTerminated then Exit;

      if Assigned(FMergeInfo.Pattern) and (not VarIsNull(FMergeInfo.Value) and (FMergeInfo.Value <> ReportParams[SRepParam_Merge])) then
        InternalDoAction(FLastAction, FMergeInfo.Pattern);

      if FRunning then
        Raise ERepError.Create(SRepErrorRunning);

      FRunning := True;
      try
        if not FPatterns.Active then OpenPattern(FData.FParams);
        FData.Active := True;
        try
          SetLength(LDataSets, FData.DataSetCount);
          for i := Pred(FData.DataSetCount) downto 0 do
            LDataSets[I] := FData.DataSets[i].DataSet;
          if not PrintIfEmpty and DataSetsIsEmpty then exit;
          if Assigned(Data.PageDataSet.DataSet) then Data.PageDataSet.DataSet.First; // To actual Master-Detail link

//          LVariables := FData.FVariables;

          FPatterns.First;
          while not FPatterns.Eof do begin
            PreparePatternStorage(LPatternStorage);
            try
              if AAction.ActionType = atSave then begin
                LSaveFileDir  := ExtractFilePath(AAction.FileName);
                LSaveFileName := ExtractFileName(AAction.FileName);
                Assert((FPatterns.RecordCount = 1) or (LSaveFileName.IsEmpty));

                if AAction.ReplaceType = rrtInternal then begin
                  LSaveFileName := PrepareSaveFileName(LSaveFileName);
                  if LSaveFileName = '' then
                    LSaveFileName := SRepDefaulFileName;
                end else begin
                  if not SysUtils.DirectoryExists(LSaveFileDir) then
                    try
                      ForceDirectories(LSaveFileDir);
                    except on E: Exception do
                      Raise ERepError.CreateFmt(SReportError_CreateDir, [LSaveFileDir, E.Message]);
                    end;
                  LSaveFileName := PrepareSaveFileName(LSaveFileName);
                end;

                LSaveFileName := ExpandFileName(LSaveFileDir + LSaveFileName); // Накуя?

                if ExtractFileName(LSaveFileName) = '' then
                  Raise ERepError.Create(SReportError_FileNameEmpty);
              end else
                LSaveFileName := PrepareSaveFileName('');

              try
                LPatternTypeCode := VarToStr(FPatterns.Fields.GetValue(SRepParam_PatternTypeCode)).Trim;
                if (LPatternTypeCode.IsEmpty) then raise ERepError.Create(SReportError_PatternEmptyCode);

                LPatternInfo := GetPatternInfo(LPatternTypeCode);
                if not Assigned(LPatternInfo) then
                  Raise ERepError.CreateFmt(SReportError_PatternUnknownType, [LPatternTypeCode]);

                if Assigned(FMergeInfo.Pattern) and (FMergeInfo.Pattern.PatternCode <> LPatternTypeCode) then
                  InternalDoAction(FLastAction, FMergeInfo.Pattern);

                if Assigned(FMergeInfo.Pattern) then
                  LPattern := FMergeInfo.Pattern
                else
                  LPattern := LPatternInfo^.Factory.GetPattern(Self as IReport, LPatternInfo^.Index);

                if not Assigned(LPattern) then
                  Raise ERepError.CreateFmt(SReportError_PatternUnknownType, [LPatternTypeCode]);
                LPatternStorage.CheckExists;

                try
{$IFNDEF REPORT_EXTERNAL}
//                  if foUtils.DebugDesignerMode then begin
//                    try
//                      LPattern.PrepareReport(LPatternStorage, LDataSets, LVariables);
//                    except
//                    end;
//                    LPattern.DesignReport(Application.MainForm)
//                  end
//                  else
{$ENDIF}
                  begin
                    LPattern.PrepareReport(LPatternStorage, LDataSets, FData.FVariables);
                    if VarIsNull(ReportParams[SRepParam_Merge]) or not Supports(LPattern, IPatternMerge) then
                      DoPattern(LPattern, LSaveFileName)
                    else begin
                      FMergeInfo.Pattern := LPattern;
                      FMergeInfo.Value   := ReportParams[SRepParam_Merge];
                    end;
                  end;
                except on E: Exception do begin
                  Terminate;
                  Raise ERepError.CreateFmt(SReportError_Pattern, [LPatternStorage.StoragePath, E.Message]);
                end end;
                if FTerminated then break;
              except
                Terminate;
                Raise;
              end;
              if FTerminated then break;
              FPatterns.Next;
            finally
              LPatternStorage.Release;
            end;
          end;
        finally
          FData.Active := False;
          FPatterns.Close;
        end;
        FLastAction := AAction;
        FLastAction.FileName := LSaveFileName;
      finally
        FRunning := False;
      end;

      if Assigned(Data.ReportDataSet.DataSet) then
        with Data do begin
          ReportDataSet.DataSet.Next;
          if not ReportDataSet.DataSet.Eof then begin
            FReportDataSetLink.DataSet.Delete;
            FReportDataSetLink.DataSet.Load(ReportDataSet.DataSet, 1);
            Data.PrepareParams(FReportDataSetLink.DataSet);
          end;
        end;
    until not Assigned(Data.ReportDataSet.DataSet) or Data.ReportDataSet.DataSet.Eof;
end;

function ShowParam(const AParams: TNamedVariants): String;
var
  i: Integer;
begin
  for i := 0 to Pred(AParams.Count) do
    with AParams.Items[i] do
    begin
      if Result <> '' then
        Result := Result + ';';
      Result := Result + Name + '=' + VarToStr(SBaseVariantFunctions.IsNull(Value, 'Null'));
    end;
end;

procedure TSBaseReport.DoParentFormResize(Sender: TObject);
begin
  {$IFNDEF REPORT_EXTERNAL}
  if Assigned(FProgressForm) then
    FProgressForm.UpdatePos;
  if Assigned(FParentFormResize) then
    FParentFormResize(Sender);
  {$ENDIF}
end;

procedure TSBaseReport.OpenReport;
var
  LObjectParams: String;
  LParams: TNamedVariants;
begin
  if FRunning then
    Raise ERepError.Create(SRepErrorRunning);

  FMultiThread := (FData.FParams[SRepParam_BuildInThread] = True);
  PrepareReport;

  LObjectParams := ObjectParams;
  if (LObjectParams.IsEmpty) then
    FReportInfo[SRepParam_Params] := ''
  else
    FReportInfo[SRepParam_Params] := FData.FParams.GetAsClrTParams(LObjectParams);

  LParams := FData.FParams;
  LParams.UnknownAsNull := True;
  LParams.AssignValues(FReportInfo);
  FReportInfo.AssignValues(MainConnectionProvider.ExecSQL(ReportSQL_VersionInfo, SBaseFormProperties.InterfaceVariables + LParams));
end;

procedure TSBaseReport.OpenPattern(AParams: TNamedVariants);
begin
  AParams.AssignValues(FData.FParams);
  AParams.AssignValues(FReportInfo);
  InternalExecProc(ReportSQL_Patterns, AParams, FPatterns);
end;

procedure TSBaseReport.DoInfoMessageEvent(AConnection: TInternalConnection; const AInfoMessage: String);
var
  LStringLine: String;

  function iSReportMessage_Type(const APrefix: String; var APrefixLen: Integer): Boolean;
  begin
    APrefixLen := Length(APrefix);
    Result := CompareText(APrefix, Copy(LStringLine, 1, APrefixLen)) = 0;
  end;

var
  LPrefixLen: Integer;
  LStringItem: TStringItem;
begin
  for LStringItem in SBaseStringFunctions.EnumStringItems(AInfoMessage, #13) do begin
    LStringLine := LStringItem.Value.Trim(CConst_LineBreaks);

    if iSReportMessage_Type(SReportMessagePrefix_Error, LPrefixLen) then
      ShowMessage(Copy(LStringLine, LPrefixLen + 1, LStringLine.Length - LPrefixLen), rmtError)
    else if iSReportMessage_Type(SReportMessagePrefix_Warning, LPrefixLen) then
      ShowMessage(Copy(LStringLine, LPrefixLen + 1, LStringLine.Length - LPrefixLen), rmtWarning)
    else if iSReportMessage_Type(SReportMessagePrefix_Info, LPrefixLen) then
      ShowMessage(Copy(LStringLine, LPrefixLen + 1, LStringLine.Length - LPrefixLen), rmtInfo)
{$IFDEF REPORT_DESIGNER}
    else
      ShowMessage(AStrings[i], rmtWarning);
{$ENDIF}
  end;
end;

function TSBaseReport.GetPageDataSet: TDataSet;
begin
  Result := FData.PageDataSet.DataSet;
end;

function TSBaseReport.GetPageDataSetFieldName: String;
begin
  Result := FData.FPageDataSetFieldName;
end;

procedure TSBaseReport.Progress(const AText: String; Pos, Count: Integer; var Cancel: Boolean);
begin
{$IFNDEF REPORT_EXTERNAL}
  if Assigned(FProgressForm) then
  begin
    if AText.Trim <> '' then FProgressForm.Text := AText;

    if (Pos > 0) and (Count > Pos) then begin
      FProgressForm.ProgressPos   := Pos;
      FProgressForm.ProgressCount := Count;
    end else
      FProgressForm.ProgressCount := 0;
  end;
{$ENDIF}
  Cancel := FTerminated;
end;

function TSBaseReport.DataSetsIsEmpty: Boolean;
var
  i: Integer;
begin
  if DataSetCount = 0 then
    Result := False
  else
  begin
    Result := True;
    for i := 0 to Pred(DataSetCount) do
    begin
      Result := Result and DataSets[i].IsEmpty;
      if not Result then break;
    end;
  end;
end;

function TSBaseReport.ExportDataSet(Index: Integer): TDataSet;
begin
  Result := FData.ExportDataSet(Index);
end;

function TSBaseReport.GetMainConnectionProvider: TSQLConnectionProvider;
begin
  if Assigned(FMainConnectionProvider) then
    Result := FMainConnectionProvider
  else
  {$IFDEF REPORT_EXTERNAL}
    raise Exception.Create(SRepErrorNoConnect);
  {$ELSE}
    Result := SBaseDataModule.BaseSQLConnectionProvider;
  {$ENDIF}
end;

procedure TSBaseReport.SetMainConnectionProvider(const Value: TSQLConnectionProvider);
begin
  {$IFNDEF REPORT_EXTERNAL}
  FMainConnectionProvider := nil;
  if Value <> SBaseDataModule.BaseSQLConnectionProvider then
  {$ENDIF}
    FMainConnectionProvider := Value;

  FPatterns.ConnectionProvider := Value;
end;

procedure TSBaseReport.SetReportParams(Index: String; const Value: Variant);
begin
  FData.Params[Index] := Value;
end;

function TSBaseReport.GetConnectionProvider: TSQLConnectionProvider;
begin
  if not Assigned(FConnectionProvider) then Connect;
  Result := FConnectionProvider
end;

function TSBaseReport.ObjectParams: String;
begin
  Result := VarToStr(FReportInfo[SRepParam_ObjectParams]);
end;

procedure TSBaseReport.Connect(const AProcedureName: String);
var
  LProcedureName: String;
begin
  if Assigned(FConnectionProvider) then exit;

  if AProcedureName = '' then
    LProcedureName := FReportInfo[SRepParam_ProcedureName]
  else
    LProcedureName := AProcedureName;

{$IFNDEF REPORT_CONNECTION}
  FConnectionProvider := MainConnectionProvider;
{$ELSE}
  FConnectionProvider := SBaseDataModule.BaseSQLConnectionProviderStorage.AssignConnection
                          (
                            Self,
                            TNamedVariants.Create
                            (
                              [
                                TNamedVariant.Create(SConst_Object, LProcedureName)
                              ]
                            ),
                            FMultiThread,
                            FConnectionProvider
                          );

{$ENDIF}
  if not FMultiThread then
    with FConnectionProvider do begin
      FInfoMessageEvent := OnInfoMessage;
      OnInfoMessage := DoInfoMessageEvent;
    end;

  if Assigned(FConnectionProvider) and not FConnectionProvider.Connected then
    FConnectionProvider.Connected := True;
end;

procedure TSBaseReport.Disconnect;
var
  AOnInfoMessageSet, AOnInfoMessageLocal: TInternalConnectionOnInfoMessage;
begin
  if not Assigned(FConnectionProvider) then Exit;

  with FConnectionProvider do begin
    AOnInfoMessageSet := OnInfoMessage;
    AOnInfoMessageLocal := DoInfoMessageEvent;
    if @AOnInfoMessageSet = @AOnInfoMessageLocal then
      OnInfoMessage := FInfoMessageEvent;
  end;

{$IFDEF REPORT_CONNECTION}
  try
    SBaseDataModule.BaseSQLConnectionProviderStorage.ReleaseConnection(Self, FConnectionProvider);
  finally
    FConnectionProvider := nil;
  end;
{$ENDIF}
end;

{$IFNDEF REPORT_EXTERNAL}
function TSBaseReport.GetOwnerForm: TForm;
begin
  if Assigned(FOwnerForm) then
    Result := FOwnerForm
  else
    Result := Application.MainForm;
end;
{$ENDIF}

//{IFNDEF REPORT_EXTERNAL}
//procedure TSBaseReport.InsertFieldValues(const ATableName, AFieldNames: String; AFields: Array of TField);
//begin
//  SBaseReport.InsertFieldValues(ATableName, AFieldNames, AFields, GetConnectionProvider);
//end;
//{ENDIF}

procedure TSBaseReport.DisableSystemDataSet;
begin
  FDisableSystemDataSet := True;
end;

function TSBaseReport.GetDataSetCount: Integer;
begin
  Result := FData.DataSetCount;
end;

procedure TSBaseReport.ClearDataSets;
begin
  FData.Clear(False);
end;

procedure TSBaseReport.AssignDataSets(const ADataSets: Array of TDataSet);
var
  I, ACount: Integer;
begin
  ACount := Length(ADataSets);
  for I := 0 to Pred(ACount) do
  begin
    FData.AddDataSet(ADataSets[I], rdssExternal);
    with ADataSets[I] do if not Active then Open;
  end;
end;

procedure TSBaseReport.AssignDataSets(const ADataSets: Variant);
var
  I: Integer;
begin
  if VarIsPresent(ADataSets) then
    if VarIsArray(ADataSets) then
      for I := VarArrayLowBound(ADataSets, 1) to VarArrayHighBound(ADataSets, 1) do
        AssignDataSets([TDataSet(Integer(ADataSets[I]))])
    else
      AssignDataSets([TDataSet(Integer(ADataSets))]);
end;

procedure TSBaseReport.AssignReportParams(const AParams: TNamedVariants);
begin
  FData.FParams.AssignValues(AParams);
end;

function TSBaseReport.GetDataSets(Index: Integer): TDataSet;
begin
  if Index < DataSetCount then
    Result := FData[Index].DataSet
  else
    Result := nil;
end;

function TSBaseReport.GetVariablesCount: Integer;
begin
  Result := FData.FVariables.Count;
end;

procedure TSBaseReport.AssignVariables(const AVariables: TNamedVariants);
var
  i: Integer;
begin
  for i := Pred(AVariables.Count) downto 0 do
    with AVariables.Items[i] do
      Variables[Name] := Value;
end;

// Params
procedure TSBaseReport.ClearReportParams;
begin
  FData.FParams.Clear;
end;

function TSBaseReport.GetVariables(const Name: String): Variant;
begin
  Result := FData.Variables[Name];
end;

procedure TSBaseReport.SetVariables(const Name: String; const Value: Variant);
begin
  FData.Variables[Name] := Value;
end;

function TSBaseReport.BuildReport(const AOverrideProcedure: TNamedVariants): Boolean;
var
  LProcedureName, LProcedureParams: String;
  LReportParams: String;
begin
  if FRunning then
    Raise ERepError.Create(SRepErrorRunning);

  Result := False;
  FTerminated := False;

//  if VarToIntDef(FData.Params[SConst_DataSetCount], MaxWord) <= 0 then Exit;

  if FProcNamePriority = pnpServer then begin
    LProcedureName    := VarToStr(FReportInfo[SRepParam_ProcedureName]);
    LProcedureParams  := VarToStr(FReportInfo[SRepParam_ProcedureParams]);
    if (LProcedureName.IsEmpty) then begin
      LProcedureName    := VarToStr(AOverrideProcedure[SRepParam_ProcedureName]);
      LProcedureParams  := VarToStr(AOverrideProcedure[SRepParam_ProcedureParams]);
    end;
  end else begin
    LProcedureName    := VarToStr(AOverrideProcedure[SRepParam_ProcedureName]);
    LProcedureParams  := VarToStr(AOverrideProcedure[SRepParam_ProcedureParams]);
    if (LProcedureName.IsEmpty) then begin
      LProcedureName    := VarToStr(FReportInfo[SRepParam_ProcedureName]);
      LProcedureParams  := VarToStr(FReportInfo[SRepParam_ProcedureParams]);
    end;
  end;

  if (LProcedureName.IsEmpty) then begin
    if Assigned(FDataSet) then
      FreeAndNil(FDataSet);

    // Result := True;
    Exit;
  end;

  if Assigned(FDataSet) then
    FDataSet.Close
  else
    FDataSet := TReportInternalDataSet.Create(nil);

  FDataSet.ReadOnly := FReadOnly;
  with FDataSet do begin
    Connect(LProcedureName);
    ConnectionProvider := Self.GetConnectionProvider;
    CachedUpdates := FDataSetCachedUpdates;

    SQL.Text := SConst_Exec + ' ' + LProcedureName + ' ' + LProcedureParams;
    if ParamCount = 0 then
      raise ERepError.CreateFmt(SReportError_NoStoredProc, [LProcedureName, Self.GetConnectionProvider.ConnectionInfo]);
  end;

  LReportParams := VarToStr(FReportInfo[SRepParam_ObjectParams]);

  FDataSet.Params.Load(SBaseFormProperties.InterfaceVariables, lvcAlways);
  FDataSet.Params.Load(FData.FParams.Copy(LReportParams), lvcIfReceive);
  FDataSet.Params.Load(AOverrideProcedure.Copy(LReportParams), lvcIfReceive);

  Result := BuildReportNext(False);
end;

function TSBaseReport.BuildReportNext(ANextRowSet: Boolean): Boolean;
var
  LCancel, LOpened, LCopyAllDataSet: Boolean;
  LDataSetCount, LGlobalDataSetCount: Integer;
  LStorage: TReportDataSetStorage;
begin
  Result := False;
  if (not ANextRowSet) then
    FDataSet.Close
  else if (not FDataSet.Active) then
    Exit;

//  if FDataSet.Active and (not FDataSet.!asNextResultSet) then begin
//    FDataSet.Close;
//    Exit;
//  end;

  FData.Clear(False);
  Progress(SReportState_Prepare, 0, 0, LCancel);
  if LCancel or FTerminated or not Assigned(FDataSet) then Exit;

  LDataSetCount   := VarToIntDef(FReportInfo[SRepParam_DataSetCount], MaxWord);
  LCopyAllDataSet := VarToBoolDef(ReportParams[SRepSysParam_CopyAllDataSet], FCopyAllDataSet);
  LGlobalDataSetCount := FData.DataSetCount;
  repeat
    LOpened := InternalOpenNext(FDataSet{, LOpenNext});
    if FTerminated or (not LOpened) then Break;
    Result := True;

    if LCopyAllDataSet or (FData.DataSetCount - LGlobalDataSetCount < Pred(LDataSetCount)) then
      LStorage := rdssLocalCopy
    else
      LStorage := rdssExternal;
    FData.AddDataSet(FDataSet.ActiveDataSet, LStorage);
  until (FData.DataSetCount - LGlobalDataSetCount >= LDataSetCount);
  FDataSet.ClearNextResultSets;
end;

procedure TSBaseReport.PrepareParams;
begin
  PrintIfEmpty      := VarToBoolDef(ReportParams[SRepSysParam_PrintIfEmpty], PrintIfEmpty);
  if not VarIsNull(ReportParams[SReportParam_Empty]) then begin
    ShowMessage(VarToStr(ReportParams[SReportParam_Empty]), rmtInfo);
    Terminate;
    Exit;
  end;
end;

procedure TSBaseReport.ShowReport(AShowModal: Boolean);
var
  Action: TReportAction;
begin
  Action.ActionType   := atShow;
  Action.ReplaceType  := rrtReplace;
  Action.ShowModal    := AShowModal;
  InternalDoAction(Action);
end;

procedure TSBaseReport.SaveReport(const AFileName: String; AReplaceType: TRepReplaceType = rrtReplace);
var
  LCancel: Boolean;
  LAction: TReportAction;
begin
  LAction.ActionType   := atSave;
  LAction.FileName     := AFileName;
  LAction.ReplaceType  := AReplaceType;
  Progress(SReportState_Save, 0, 0, LCancel);
  if not LCancel then InternalDoAction(LAction);
end;

procedure TSBaseReport.SaveReport(AStream: TStream);
var
  LCancel: Boolean;
  LAction: TReportAction;
begin
  LAction.ActionType   := atStream;
  LAction.Stream       := AStream;
  LAction.ReplaceType  := rrtReplace;
  Progress(SReportState_Save, 0, 0, LCancel);
  if not LCancel then InternalDoAction(LAction);
end;

procedure TSBaseReport.PrintReport(AShowDialog: Boolean);
var
  ACancel: Boolean;
  Action: TReportAction;
  ADevice, ADriver, APort: array[0..MAX_PATH] of Char;
  hDeviceMode: THandle;
begin
  Action.ActionType := atPrint;
{$IFNDEF REPORT_EXTERNAL}
  if AShowDialog then with TPrinterSetupDialog.Create(Self) do
    try
      if not Execute then exit;
    finally
      Free;
    end;
{$ENDIF}
  Printer.GetPrinter(ADevice, ADriver, APort, hDeviceMode);
  Action.Printer := ADevice;
  Progress(SReportState_Print, 0, 0, ACancel);
  if not ACancel then InternalDoAction(Action);
end;

procedure TSBaseReport.Wait;
begin
  {$IFNDEF REPORT_EXTERNAL}
  while InProcess do
  try
    Application.HandleMessage;
  except
    Application.HandleException(Application);
  end;
  {$ENDIF}
end;

procedure TSBaseReport.DoLogMessage(const Msg: String; MessageType: TReportMessageType);
begin
  if not Assigned(FOnLogMessage) then exit;

  case MessageType of
    rmtError  : FOnLogMessage(Msg, lgError);
    rmtWarning: FOnLogMessage(Msg, lgWarning);
    rmtInfo   : FOnLogMessage(Msg, lgInfo);
  end;
end;

procedure TSBaseReport.Terminate;
begin
  FTerminated := True;
  if Assigned(FDataSet) and FDataSet.Thread.Active then
    FDataSet.Thread.Stop;
end;

// File routins
function TSBaseReport.Prepared: Boolean;
begin
  Result := (FReportInfo.Count > 0);
end;

function TSBaseReport.PrepareSaveFileName(const AFileName: String): String;
//var
//  LSaveFileAddon: String;
begin
//  LSaveFileAddon := VarToStr(ReportParams[SRepParam_SaveFileAddon]);

  if not AFileName.IsEmpty then begin
    Result := AFileName;
//    Result := ExtractFileName(Result);
  end else begin
    Result := VarToStr(ReportParams[SRepParam_SaveFileName]);
    if Result.IsEmpty then begin
      Result := FPatterns.FieldByName(SRepParam_SaveFileName).AsString;
      if Result.IsEmpty then
        Result := FPatterns.FieldByName(SRepParam_PatternFileName).AsString;
    end;
  end;

  if Pos('%', Result) > 0 then
    Result := FormatString(Result, FData.FParams);
end;

procedure TSBaseReport.PreparePatternStorage(var APatternStorage: TPatternStorage);
var
//  LPatternFileAddon: String;
  LPatternFileName, LPatternFilePath: String;
  LBinaryField: TField;

  LScriptFileName: String;
begin
  APatternStorage.Clear;

  { Сам файл шаблона в формате "<ОТНОСИТЕЛЬНЫЙ ПУТЬ К ШАБЛОНАМ>\<ИМЯ ФАЙЛА ШАБЛОНА>" }

  { из параметров вызова }
  LPatternFileName := VarToStr(ReportParams[SRepParam_PatternFileName]);

  { из настроек сервера }
  if LPatternFileName.IsEmpty then begin
    // 1.
    LBinaryField := FPatterns.FindField(SRepParam_ScriptBinary);
    if Assigned(LBinaryField) and (not LBinaryField.IsNull) then begin
      APatternStorage.ScriptStream := FPatterns.CreateBlobStream(LBinaryField, bmRead);
      APatternStorage.ScriptStream.Position := 0;
      // Exit; // !!!!
    end else
      LScriptFileName  := FPatterns.FieldByName(SRepParam_ScriptFileName).AsString;

    LBinaryField := FPatterns.FindField(SRepParam_PatternBinary);
    if Assigned(LBinaryField) and (not LBinaryField.IsNull) then begin
      APatternStorage.PatternStream := FPatterns.CreateBlobStream(LBinaryField, bmRead);
      APatternStorage.PatternStream.Position := 0;
      Assert(LScriptFileName.IsEmpty, 'Incorrect combination "ScriptFileName" and "PatternBinary"');
      Exit;
    end;

    LPatternFileName := FPatterns.FieldByName(SRepParam_PatternFileName).AsString;
    { Место нахождения шаблонов }
    LPatternFilePath := TEMPLATES_DIR;
  end else begin
    LScriptFileName := VarToStr(ReportParams[SRepParam_ScriptFileName]);
    { Место нахождения шаблонов }
    LPatternFilePath:= VarToStr(ReportParams[SRepParam_PatternFilePath]);
    if not LPatternFilePath.IsEmpty then
      LPatternFilePath := SysUtils.IncludeTrailingPathDelimiter(LPatternFilePath);
  end;

  if not LPatternFileName.IsEmpty then begin
    APatternStorage.PatternFileName := LPatternFilePath + LPatternFileName;
    if not LScriptFileName.IsEmpty then
      APatternStorage.ScriptFileName  := LPatternFilePath + LScriptFileName;
  end;

//  { Дополнительная часть пути к шаблону }
//  LPatternFileAddon := SysUtils.ExcludeTrailingPathDelimiter(VarToStr(ReportParams[SRepParam_PatternFileAddon]));
//
//  if Pos('%', LPatternFileName) = 0 then
//    APatternStorage.PatternFileName := LPatternFilePath + LPatternFileName
//  else if (LPatternFileAddon.IsEmpty) then
//    APatternStorage.PatternFileName := LPatternFilePath + Format(LPatternFileName, [''])
//  else begin
//    repeat
//      if not LPatternFileAddon.IsEmpty then
//        APatternStorage.PatternFileName := LPatternFilePath + Format(LPatternFileName, [SysUtils.IncludeTrailingPathDelimiter(LPatternFileAddon)])
//      else begin
//        APatternStorage.PatternFileName := LPatternFilePath + Format(LPatternFileName, ['']);
//        Break;
//      end;
//      LPatternFileAddon := ExtractFileDir(LPatternFileAddon);
//    until FileExists(APatternStorage.PatternFileName);
//  end;
end;

procedure TSBaseReport.PrepareReport;
begin
  if not Prepared then
    FReportInfo := MainConnectionProvider.ExecSQL(ReportSQL_ObjectInfo, FData.FParams);
end;

procedure TSBaseReport.UnPrepareReport;
begin
  FReportInfo.Clear;
end;

class function TSBaseReport.CreateTempFileName(const AFileName: String; const AExt: String = ''; AFileIndex: Integer = 0): String;

  function GetNewTempFileName(const AFileName, AExt: String; const AddPostPrefix: String = ''): String;
  begin
    if AFileIndex > 0 then
      Result := AFileName + '(' + IntToStr(AFileIndex) + ')' + AddPostPrefix + AExt
    else
      Result := AFileName + AddPostPrefix + AExt;
  end;

var
  I: Integer;
  LPathName, LFileName, LExt: String;
begin
//  if AFileName.IsEmpty then
//    Exit(GetTempFile(AExt));

  AFileIndex := 0;
  LPathName := TEMP_DIR;
  if not SysUtils.DirectoryExists(LPathName) then ForceDirectories(LPathName);
  LFileName := ExtractFileName(AFileName);


  if AExt = '' then
    LExt := SysUtils.ExtractFileExt(LFileName)
  else if (Length(AExt) > 0) and (AExt[1] <> '.') then
    LExt := '.' + AExt
  else
    LExt := AExt;

  I := LastDelimiter('.' + PathDelim + DriveDelim, LFileName);
  if (I = 0) or (LFileName[I] <> '.') then I := MaxInt;
  LFileName := LPathName + Copy(LFileName, 1, Pred(I));
  Result := GetNewTempFileName(LFileName, LExt);
  I := 1;
  while FileExists(Result) do begin
    Result := GetNewTempFileName(LFileName, LExt, SRepFilePrefixCopy + IntToStr(I));
    Inc(I);
  end;
end;

function TSBaseReport.CreateFileName(const AFileName, AExt: String; AReplaceType: TRepReplaceType): String;
var
  I: Integer;
  LFileName, LFileExt: String;
begin
  if AReplaceType = rrtInternal then
  begin
    Result := CreateTempFileName(AFileName, '');
    Exit;
  end;

  if (Length(AExt) > 0) then begin
    if AExt[1] <> '.' then
      LFileExt := '.' + AExt
    else
      LFileExt := AExt;
    Result := ChangeFileExt(AFileName, LFileExt);
  end else begin
    Result := AFileName;
    LFileExt := '.' + ExtractFileExt(AFileName);
  end;

  I := 1;
  while FFileNames.IndexOf(Result) >= 0 do begin
    Result := ChangeFileExt(AFileName, '(' + IntToStr(I) + ')' + LFileExt);
    Inc(I);
  end;

  case AReplaceType of
    rrtReplace: if FileExists(Result) then SysUtils.DeleteFile(Result);
    rrtBackup : if FileExists(Result) then
      begin
        LFileName := ChangeFileExt(Result, SRepFileBackupExt);
        if FileExists(LFileName) then SysUtils.DeleteFile(LFileName);
        RenameFile(Result, LFileName);
      end;
    rrtNew    :
      begin
        i := 1;
        LFileName := Result;
        while FileExists(LFileName) do begin
          LFileName := ChangeFileExt(Result, '') + SRepFilePrefixCopy + IntToStr(I) + SysUtils.ExtractFileExt(Result);
          Inc(I);
        end;
        Result := LFileName;
      end;
  end;
end;

// Messages
function TSBaseReport.ShowMessage(const Msg: String; MessageType: TReportMessageType): Boolean;
begin
  Result := False;
  if Msg = '' then Exit;

{$IFDEF REPORT_EXTERNAL}
  DoLogMessage(Msg, MessageType);
  Result := True;
{$ELSE}
  if Msg = SOperationAborted then Exit;

  if FMultiThread then begin
    DoLogMessage(Msg, MessageType);
    Result := True;
  end else
    Result := RepMessage(Msg, MessageType, GetOwnerForm);
{$ENDIF}
end;

procedure TSBaseReport.UnsupportedOperation;
begin
  raise ERepError.Create(SRepErrorUnsupportedOperation);
end;

// Process
function TSBaseReport.InProcess: Boolean;
begin
  Result := FDataSet.Thread.Active;
end;

procedure TSBaseReport.BeginProcess(ShowProgress: Boolean = True{$IFNDEF REPORT_EXTERNAL}; Parent: TWinControl = nil{$ENDIF});
begin
  FProcess := True;
  {$IFNDEF REPORT_EXTERNAL}
  if ShowProgress then begin
    if Assigned(Parent) then
      if Parent is TForm then
        FParentForm := TForm(Parent)
      else
        FParentForm := nil
    else
      FParentForm := Application.MainForm;

    if Assigned(FParentForm) then
      with FParentForm do begin
        FParentFormResize := OnResize;
        OnResize := DoParentFormResize;
        Cursor := crHourGlass;
      end;

    FProgressForm := TSBaseProgressFrm.Create(Self);
    FProgressForm.Text := SReportState_Prepare;
    FProgressForm.OnCancelButtonClick := ProgressFormCancelClick;
    FProgressForm.Show(Parent);
  end;
  {$ENDIF}
end;

procedure TSBaseReport.EndProcess;
begin
  {$IFNDEF REPORT_EXTERNAL}
  FProcess := False;
  if Assigned(FParentForm) then
    with FParentForm do
    begin
      Cursor := crDefault;
      OnResize := FParentFormResize;
      FParentForm := nil;
    end;
  if Assigned(FProgressForm) then
  begin
    FProgressForm.Close;
    FreeAndNil(FProgressForm);
  end;
  {$ENDIF}
end;

procedure TSBaseReport.ProgressFormCancelClick(Sender: TObject);
begin
  Terminate;
end;

{$IFNDEF REPORT_EXTERNAL}
function TSBaseReport.ICreateByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants): TComponent;
begin
  Result := TSBaseReport.CreateByParams(AOwner, AParams);
end;

function TSBaseReport.IGetCreateParams: TNamedVariants;
begin
  Result.UnknownAsNull := True;
end;

procedure TSBaseReport.IShow;
begin
  Destroy
end;

function TSBaseReport.IVirtualClassOwner(var AVirtualClassName: String; var AParams: TNamedVariants): TPersistentClass;
begin
  Result := nil;
end;

function TSBaseReport.ISelectByParams(AClass: TPersistentClass; AOwner: TComponent; AParams: TNamedVariants; ACallBackProcedure: TCallBackSelectFunction = nil): TNamedVariants;
begin
  Result := TNamedVariants.Create(False);
end;

class function TSBaseReport.CreateByParams(AOwner: TComponent; const AParams: Array of TNamedVariant): TComponent;
begin
  Result := CreateByParams(AOwner, TNamedVariants.Create(AParams, True));
end;

class function TSBaseReport.CreateByParams(AOwner: TComponent; AParams: TNamedVariants): TComponent;
var
  LAction   : String;
  LReport   : TSBaseReport;
  LIdx      : Integer;
  LDataSets : Variant;
  LExternalDataSets: Boolean;
begin
  LAction := VarToStrDef(AParams[SRepParam_Action], SRepAction_Show);
  LReport := Create(AOwner);
  with LReport do try
    if AParams.Find(SRepParam_DataSets, LIdx) then begin
      LDataSets := AParams.Items[LIdx].Value;
      AParams.Delete(LIdx);
      LExternalDataSets := True;
    end else
      LExternalDataSets := False;

    AssignReportParams(AParams);
    OpenReport;
    AssignVariables(AParams);
    if LExternalDataSets then
      AssignDataSets(LDataSets);

    if LExternalDataSets or BuildReport(AParams) then
      repeat
        if LAction = SRepAction_Save then
          SaveReport(VarToStr(AParams[SRepParam_SaveFileName]))
        else if LAction = SRepAction_Stream then
          SaveReport(VarToPointer(AParams[SRepParam_SaveStream]))
        else if LAction = SRepAction_Show then
          ShowReport(True);
      until LExternalDataSets or (not BuildReportNext);

    Result := nil;
  finally
    Free;
  end;
end;

{$ENDIF}

procedure TSBaseReport.ClearData;
begin
  FData.Clear(True);
end;

{$IFNDEF REPORT_EXTERNAL}
{$IFDEF FASTSCRIPT_RTTI}
type
  TfsFunctions_TReport = class(TfsRTTIModule)
  private
    function Call_TReport_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
  public
    constructor Create(AScript: TfsScript); override;
  end;

constructor TfsFunctions_TReport.Create(AScript: TfsScript);
begin
  inherited;

  with AScript.AddClass(TSBaseReport, TSBaseReport.ClassParent.ClassName) do begin
    AddMethod('function CreateByParams(AOwner: Pointer; AParams: Variant): TComponent', Call_TReport_Method);
    AddIndexProperty('Variables', DelphiTypeString, DelphiTypeVariant, Call_TReport_Method, False);
    AddIndexProperty('ReportParams', DelphiTypeString, DelphiTypeVariant, Call_TReport_Method, False);
  end;
end;

function TfsFunctions_TReport.Call_TReport_Method(Instance: TObject; ClassType: TClass; const MethodName: String; Caller: TfsMethodHelper): Variant;
begin
  if MethodName = 'CREATEBYPARAMS' then
    Result := Integer(TReportClass(Instance.ClassType).CreateByParams(TComponent(Integer(Caller.Params[0])), TNamedVariants.Create(Caller.Params[1], True)))
  else if MethodName = 'VARIABLES.GET' then
    Result := TSBaseReport(Instance).Variables[Caller.Params[0]]
  else if MethodName = 'VARIABLES.SET' then
    TSBaseReport(Instance).Variables[Caller.Params[0]] := Caller.Params[1]
  else if MethodName = 'REPORTPARAMS.GET' then
    Result := TSBaseReport(Instance).ReportParams[Caller.Params[0]]
  else if MethodName = 'REPORTPARAMS.SET' then
    TSBaseReport(Instance).ReportParams[Caller.Params[0]] := Caller.Params[1]
end;
{$ENDIF}
{$ENDIF}

//function TSBaseReport.GetFieldValuesStr(AField: TField;
//  ADelim: string): widestring;
//begin
//  Result := SBaseReport.FieldValuesToStr(AField.FieldName, AField.Dataset, ADelim);
//end;

{ TReportInternalDataSet }

constructor TReportInternalDataSet.Create(Owner: TComponent);
begin
  inherited;

  AutoGenerateUpdateScripts := False;
end;

initialization
  RegisterClasses([TSBaseReport, TReportInternalDataSet]);

{$IFNDEF REPORT_EXTERNAL}
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_TReport);
{$ENDIF}
{$ENDIF}

finalization
  DestroyFactoryPatterns;
//  SetLength(FReportForms, 0);
  UnRegisterClasses([TSBaseReport, TReportInternalDataSet]);

end.
