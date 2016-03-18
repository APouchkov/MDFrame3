unit BackOfficeAccountPanel;
{$I config.inc}

interface

uses
  Types, Classes, DB, Controls, StdCtrls, ExtCtrls, Graphics, LayoutPanel, cxButtonEdit, cxEdit,
  cxTextEdit, cxDBEdit, SysUtils, FieldDataLink, cxDBLookupComboBox, cxLookupDBGrid, Dialogs,
  StrUtils, cxGroupBox, Math, cxMDLookupTreeComboBox, cxMDDBLookupComboBox
{$IFNDEF PACKAGE}
  , SBaseFormDefinitions
  , SBaseForm
  , SBaseControls
  , SBaseDataModule
  , SBaseConstants
{$ENDIF}
{$IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter
{$ENDIF}
  ;

type
  TCustomBackOfficeAccountPanel = class;

  TBackOfficeAccountPanelWidths = class(TPersistent)
  private
    [weak] FOwner: TCustomBackOfficeAccountPanel;

    function  GetAccount: Integer;
    procedure SetAccount(Value: Integer);
    function  IsAccountStored: Boolean;

    function  GetAccountPart: Integer;
    procedure SetAccountPart(Value: Integer);
    function  IsAccountPartStored: Boolean;

    function  GetCaption: Integer;
    procedure SetCaption(Value: Integer);
    function  IsCaptionStored: Boolean;
    function  GetContract: Integer;
    procedure SetContract(Value: Integer);
    function  IsContractStored: Boolean;
  public
    constructor Create(AOwner: TCustomBackOfficeAccountPanel); reintroduce;
  published
    property Caption: Integer read GetCaption write SetCaption stored IsCaptionStored;
    property Contract: Integer read GetContract write SetContract stored IsContractStored;
    property Account: Integer read GetAccount write SetAccount stored IsAccountStored;
    property AccountPart: Integer read GetAccountPart write SetAccountPart stored IsAccountPartStored;
  end;

  TCustomBackOfficeAccountPanelCaptions = class(TPersistent)
  private
    [weak] FOwner: TCustomBackOfficeAccountPanel;
    FAutoAlign: Boolean;

    function GetAccount: String;
    procedure SetAccount(const Value: String);
    function IsAccountStored: Boolean;

    function GetAccountPart: String;
    procedure SetAccountPart(const Value: String);
    function IsAccountPartStored: Boolean;

    function GetTrust: String;
    procedure SetTrust(const Value: String);
    function IsTrustStored: Boolean;

    function GetClient: String;
    procedure SetClient(const Value: String);
    function IsClientStored: Boolean;

    function GetTitle: String;
    procedure SetTitle(const Value: String);

    function GetContract: String;
    procedure SetContract(const Value: String);
    function IsContractStored: Boolean;

    procedure SetAutoAlign(const Value: Boolean);
  protected
    procedure UpdateLeft;
  public
    constructor Create(AOwner: TCustomBackOfficeAccountPanel); reintroduce;
  protected
//    function GetOwner: TPersistent; override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Title: String read GetTitle write SetTitle; // stored IsTitleStored;
    property Client: String read GetClient write SetClient stored IsClientStored;
    property Contract: String read GetContract write SetContract stored IsContractStored;
    property Account: String read GetAccount write SetAccount stored IsAccountStored;
    property AccountPart: String read GetAccountPart write SetAccountPart stored IsAccountPartStored;
    property Trust: String read GetTrust write SetTrust stored IsTrustStored;
    property AutoAlign: Boolean read FAutoAlign write SetAutoAlign Default False;
  end;

  TBackOfficeAccountPanelReadOnly = class(TPersistent)
  private
    [weak] FOwner: TCustomBackOfficeAccountPanel;

{$IFDEF PACKAGE}
    FClient: Boolean;
    FContract: Boolean;
    FAccount: Boolean;
    FAccountPart: Boolean;
    FTrust: Boolean;
{$ELSE}
    function GetClient: Boolean;
    procedure SetClient(const Value: Boolean);

    function GetContract: Boolean;
    procedure SetContract(const Value: Boolean);

    function GetAccount: Boolean;
    procedure SetAccount(const Value: Boolean);

    function GetAccountPart: Boolean;
    procedure SetAccountPart(const Value: Boolean);

    function GetTrust: Boolean;
    procedure SetTrust(const Value: Boolean);
{$ENDIF}
  public
    constructor Create(AOwner: TCustomBackOfficeAccountPanel); reintroduce;
  published
    property Client     : Boolean read {$IFDEF PACKAGE}FClient      {$ELSE}GetClient      {$ENDIF} write {$IFDEF PACKAGE}FClient      {$ELSE}SetClient      {$ENDIF} Default False;
    property Contract   : Boolean read {$IFDEF PACKAGE}FContract    {$ELSE}GetContract    {$ENDIF} write {$IFDEF PACKAGE}FContract    {$ELSE}SetContract    {$ENDIF} Default False;
    property Account    : Boolean read {$IFDEF PACKAGE}FAccount     {$ELSE}GetAccount     {$ENDIF} write {$IFDEF PACKAGE}FAccount     {$ELSE}SetAccount     {$ENDIF} Default False;
    property AccountPart: Boolean read {$IFDEF PACKAGE}FAccountPart {$ELSE}GetAccountPart {$ENDIF} write {$IFDEF PACKAGE}FAccountPart {$ELSE}SetAccountPart {$ENDIF} Default False;
    property Trust      : Boolean read {$IFDEF PACKAGE}FTrust       {$ELSE}GetTrust       {$ENDIF} write {$IFDEF PACKAGE}FTrust       {$ELSE}SetTrust       {$ENDIF} Default False;
  end;

  TBackOfficeAccountPanelTrustLevel = (baptlAuto, baptlClient, baptlContract, baptlAccount, baptlAccountPart);

  TCustomBackOfficeAccountPanel = class(TcxCustomLayoutGroupBox{$IFNDEF PACKAGE}, ISBaseReadOnlyInterface{$ENDIF})
  strict private
    FReAlignPanelsRequited: Boolean;
{$IFNDEF PACKAGE}
    FReBuildRequired: Boolean;
{$ENDIF}
  private
//    FInitializing: Boolean;

    FWidths: TBackOfficeAccountPanelWidths;
    FCaptions: TCustomBackOfficeAccountPanelCaptions;
    FReadOnly: TBackOfficeAccountPanelReadOnly;
    FEditStyle: Boolean;

    FCaptionLabel: TLabel;

    FlpClient: TCustomLayoutPanel;
    FbetClient: TcxButtonEdit;

    FpnlContract: TCustomInternalPanel;
//    FpnlContract2: TPanel;
    FlpContractName: TCustomLayoutPanel;
    FlcbContractName: TcxDBLookupComboBox;
    FlpContractNumber: TCustomLayoutPanel;
    FlcbContractNumber: TcxDBLookupComboBox;
    FlpContractDate: TCustomLayoutPanel;
    FlcbContractDate: TcxDBLookupComboBox;

    FpnlAccount: TCustomInternalPanel;
    FlpAccountName: TCustomLayoutPanel;
    FlcbAccountName: TcxMDDBLookupComboBox;
    FlpAccountNumber: TCustomLayoutPanel;
    FlcbAccountNumber: TcxDBLookupComboBox;

    FpnlAccountPart: TCustomInternalPanel;
    FlpAccountPartName: TCustomLayoutPanel;
    FlcbAccountPartName: TcxDBLookupComboBox;
    FlpAccountPartNumber: TCustomLayoutPanel;
    FlcbAccountPartNumber: TcxDBLookupComboBox;

    FlpTrust: TCustomLayoutPanel;
    FlcbTrust: TcxMDDBLookupTreeComboBox;

    FDataFieldDateTime: String;
    FDataFieldTrustKind: String;

    FLookupParentTrust: TBackOfficeAccountPanelTrustLevel;
    FLookupConditionTrust: String;

    FLookupParamDateTime: String;
    FLookupParamClientId: String;
    FLookupParamTrustKind: String;


{$IFDEF PACKAGE}
    FDataFieldClientId: String;
{$ELSE}
    FLookupClient : TSBaseDBLookupComponentControl;
    FLookupAccount: TSBaseCustomDBLookupComponentControl;

    FLookupMDDCContract     : TMasterDetailDataSetControl;
    FLookupMDDCAccount      : TMasterDetailDataSetControl;
    FLookupMDDCAccountPart  : TMasterDetailDataSetControl;
    FLookupMDDCTrust        : TMasterDetailDataSetControl;
{$ENDIF}

    function  GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);

    function  GetDataFieldClientId: String;
    procedure SetDataFieldClientId(const Value: String);

    function  GetDataFieldContractId: String;
    procedure SetDataFieldContractId(const Value: String);

    function  GetDataFieldAccountId: String;
    procedure SetDataFieldAccountId(const Value: String);

    function  GetDataFieldAccountPartId: String;
    procedure SetDataFieldAccountPartId(const Value: String);

    function  GetDataFieldTrustId: String;
    procedure SetDataFieldTrustId(const Value: String);

    procedure SetDataFieldTrustKind(const Value: String);

    function  GetLookupSourceContract: TDataSource;
    procedure SetLookupSourceContract(const Value: TDataSource);

    function  GetLookupSourceAccount: TDataSource;
    procedure SetLookupSourceAccount(const Value: TDataSource);

    function  GetLookupSourceAccountPart: TDataSource;
    procedure SetLookupSourceAccountPart(const Value: TDataSource);

    function  GetLookupSourceTrust: TDataSource;
    procedure SetLookupSourceTrust(const Value: TDataSource);

    function  GetShowClient: Boolean;
    procedure SetShowClient(const Value: Boolean);

    function  GetShowContract: Boolean;
    procedure SetShowContract(const Value: Boolean);

    function  GetShowAccount: Boolean;
    procedure SetShowAccount(const Value: Boolean);

    function  GetShowAccountPart: Boolean;
    procedure SetShowAccountPart(const Value: Boolean);

    function  GetShowTrust: Boolean;
    procedure SetShowTrust(const Value: Boolean);

    procedure ReAlignPanels;
{$IFNDEF PACKAGE}
    procedure SetLookupClient(const Value: TSBaseDBLookupComponentControl);
    procedure SetLookupAccount(const Value: TSBaseCustomDBLookupComponentControl);

    procedure Rebuild;
{$ENDIF}

    procedure SetWidths(const Value: TBackOfficeAccountPanelWidths);
    procedure SetCaptions(const Value: TCustomBackOfficeAccountPanelCaptions);

    function GetListColumnsContract: TcxLookupDBGridColumns;
    procedure SetListColumnsContract(const Value: TcxLookupDBGridColumns);

    function GetListColumnsAccount: TcxLookupDBGridColumns;
    procedure SetListColumnsAccount(const Value: TcxLookupDBGridColumns);

    function GetListColumnsAccountPart: TcxLookupDBGridColumns;
    procedure SetListColumnsAccountPart(const Value: TcxLookupDBGridColumns);

    function GetListColumnsTrust: TcxMDLookupDBTreeListColumns;
    procedure SetListColumnsTrust(const Value: TcxMDLookupDBTreeListColumns);

    function GetLookupFieldContractId: String;
    procedure SetLookupFieldContractId(const Value: String);

    function GetLookupFieldContractName: String;
    procedure SetLookupFieldContractName(const Value: String);

    function GetLookupFieldContractNumber: String;
    procedure SetLookupFieldContractNumber(const Value: String);

    function GetLookupFieldContractDate: String;
    procedure SetLookupFieldContractDate(const Value: String);

    function GetLookupFieldAccountId: String;
    procedure SetLookupFieldAccountId(const Value: String);

    function GetLookupFieldAccountName: String;
    procedure SetLookupFieldAccountName(const Value: String);

    function GetLookupFieldAccountNumber: String;
    procedure SetLookupFieldAccountNumber(const Value: String);

    function GetLookupFieldAccountPartId: String;
    procedure SetLookupFieldAccountPartId(const Value: String);

    function GetLookupFieldAccountPartName: String;
    procedure SetLookupFieldAccountPartName(const Value: String);

    function GetLookupFieldAccountPartNumber: String;
    procedure SetLookupFieldAccountPartNumber(const Value: String);

    function GetLookupFieldTrustId: String;
    procedure SetLookupFieldTrustId(const Value: String);

    function GetLookupFieldTrustName: String;
    procedure SetLookupFieldTrustName(const Value: String);

    function GetLookupFieldTrustParent: String;
    procedure SetLookupFieldTrustParent(const Value: String);

    procedure SetReadOnly(const Value: TBackOfficeAccountPanelReadOnly);
    procedure SetDataFieldDateTime(const Value: String);
    procedure SetLookupParamDateTime(const Value: String);
    procedure SetLookupParamClientId(const Value: String);
    procedure SetLookupParamTrustKind(const Value: String);

    procedure PanelContractOnResize(Sender: TObject);
    procedure PanelAccountOnResize(Sender: TObject);
    procedure PanelAccountPartOnResize(Sender: TObject);
    procedure SetEditStyle(const Value: Boolean);
    procedure SetLookupParentTrust(const Value: TBackOfficeAccountPanelTrustLevel);
    procedure SetLookupConditionTrust(const Value: String);
  protected
{$IFNDEF PACKAGE}
    procedure IUpdateState(AColors: TControlColors);
{$ENDIF}
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AutoSizePercents default 0;

    property ShowClient: Boolean read GetShowClient write SetShowClient default True;
    property ShowContract: Boolean read GetShowContract write SetShowContract default False;
    property ShowAccount: Boolean read GetShowAccount write SetShowAccount default True;
    property ShowAccountPart: Boolean read GetShowAccountPart write SetShowAccountPart default False;
    property ShowTrust: Boolean read GetShowTrust write SetShowTrust default False;

    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataFieldDateTime: String read FDataFieldDateTime write SetDataFieldDateTime;
    property DataFieldClientId: String read GetDataFieldClientId write SetDataFieldClientId;
    property DataFieldContractId: String read GetDataFieldContractId write SetDataFieldContractId;
    property DataFieldAccountId: String read GetDataFieldAccountId write SetDataFieldAccountId;
    property DataFieldAccountPartId: String read GetDataFieldAccountPartId write SetDataFieldAccountPartId;
    property DataFieldTrustId: String read GetDataFieldTrustId write SetDataFieldTrustId;
    property DataFieldTrustKind: String read FDataFieldTrustKind write SetDataFieldTrustKind;

{$IFNDEF PACKAGE}
    property ControlClient: TcxButtonEdit read FbetClient;
    property ControlContract: TcxDBLookupComboBox read FlcbContractNumber;
    property ControlAccount: TcxMDDBLookupComboBox read FlcbAccountName;
    property ControlAccountPart: TcxDBLookupComboBox read FlcbAccountPartName;
    property ControlTrust: TcxMDDBLookupTreeComboBox read FlcbTrust;

    property LookupClient: TSBaseDBLookupComponentControl read FLookupClient write SetLookupClient;
    property LookupAccount: TSBaseCustomDBLookupComponentControl read FLookupAccount write SetLookupAccount;

    property LookupMDDCContract: TMasterDetailDataSetControl read FLookupMDDCContract;
    property LookupMDDCAccount: TMasterDetailDataSetControl read FLookupMDDCAccount;
    property LookupMDDCAccountPart: TMasterDetailDataSetControl read FLookupMDDCAccountPart;
    property LookupMDDCTrust: TMasterDetailDataSetControl read FLookupMDDCTrust;
{$ENDIF}

    property LookupSourceContract: TDataSource read GetLookupSourceContract write SetLookupSourceContract;
    property LookupSourceAccount: TDataSource read GetLookupSourceAccount write SetLookupSourceAccount;
    property LookupSourceAccountPart: TDataSource read GetLookupSourceAccountPart write SetLookupSourceAccountPart;
    property LookupSourceTrust: TDataSource read GetLookupSourceTrust write SetLookupSourceTrust;

    property LookupParamDateTime: String read FLookupParamDateTime write SetLookupParamDateTime;
    property LookupParamClientId: String read FLookupParamClientId write SetLookupParamClientId;
    property LookupParamTrustKind: String read FLookupParamTrustKind write SetLookupParamTrustKind;

    property LookupFieldContractId: String read GetLookupFieldContractId write SetLookupFieldContractId;
    property LookupFieldContractName: String read GetLookupFieldContractName write SetLookupFieldContractName;
    property LookupFieldContractNumber: String read GetLookupFieldContractNumber write SetLookupFieldContractNumber;
    property LookupFieldContractDate: String read GetLookupFieldContractDate write SetLookupFieldContractDate;

    property LookupFieldAccountId: String read GetLookupFieldAccountId write SetLookupFieldAccountId;
    property LookupFieldAccountName: String read GetLookupFieldAccountName write SetLookupFieldAccountName;
    property LookupFieldAccountNumber: String read GetLookupFieldAccountNumber write SetLookupFieldAccountNumber;

    property LookupFieldAccountPartId: String read GetLookupFieldAccountPartId write SetLookupFieldAccountPartId;
    property LookupFieldAccountPartName: String read GetLookupFieldAccountPartName write SetLookupFieldAccountPartName;
    property LookupFieldAccountPartNumber: String read GetLookupFieldAccountPartNumber write SetLookupFieldAccountPartNumber;

    property LookupFieldTrustId: String read GetLookupFieldTrustId write SetLookupFieldTrustId;
    property LookupFieldTrustName: String read GetLookupFieldTrustName write SetLookupFieldTrustName;
    property LookupFieldTrustParent: String read GetLookupFieldTrustParent write SetLookupFieldTrustParent;
    property LookupParentTrust: TBackOfficeAccountPanelTrustLevel read FLookupParentTrust write SetLookupParentTrust;
    property LookupConditionTrust: String read FLookupConditionTrust write SetLookupConditionTrust;

    property ListColumnsContract: TcxLookupDBGridColumns read GetListColumnsContract write SetListColumnsContract;
    property ListColumnsAccount: TcxLookupDBGridColumns read GetListColumnsAccount write SetListColumnsAccount;
    property ListColumnsAccountPart: TcxLookupDBGridColumns read GetListColumnsAccountPart write SetListColumnsAccountPart;
    property ListColumnsTrust: TcxMDLookupDBTreeListColumns read GetListColumnsTrust write SetListColumnsTrust;

    property Widths: TBackOfficeAccountPanelWidths read FWidths write SetWidths;
    property Captions: TCustomBackOfficeAccountPanelCaptions read FCaptions write SetCaptions;
    property ReadOnly: TBackOfficeAccountPanelReadOnly read FReadOnly write SetReadOnly;
    property EditStyle: Boolean read FEditStyle write SetEditStyle default True;
  end;

  TBackOfficeAccountPanel = class(TCustomBackOfficeAccountPanel)
  published
    property Align;
//    property AlignWithMargins;
    property AutoSize;
    property Anchors;
//    property BevelEdges;
//    property BevelInner;
//    property BevelKind;
//    property BevelOuter;
//    property BevelWidth;
    property BorderStyle;
//    property BorderWidth;
    property Caption;
    property Constraints;
    property Enabled;
//    property Height;
//    property Width;
//    property Margins;
    property PanelStyle;
    property Padding;
//    property ParentBackground;
//    property ParentColor;
//    property ParentFont;
//    property ParentShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Transparent default True;
    property Visible;
    property Style;
    property StyleDisabled;

{$IFNDEF PACKAGE}
    property ControlClient;
    property ControlContract;
    property ControlAccount;
    property ControlAccountPart;
    property ControlTrust;

    property LookupClient;
    property LookupAccount;
    property LookupMDDCContract;
    property LookupMDDCAccount;
    property LookupMDDCAccountPart;
    property LookupMDDCTrust;
{$ENDIF}

    property ShowClient;
    property ShowContract;
    property ShowAccount;
    property ShowAccountPart;
    property ShowTrust;

    property DataSource;

    property DataFieldDateTime;
    property DataFieldClientId;
    property DataFieldContractId;
    property DataFieldAccountId;
    property DataFieldAccountPartId;
    property DataFieldTrustId;
    property DataFieldTrustKind;

    property LookupSourceContract;
    property LookupSourceAccount;
    property LookupSourceAccountPart;
    property LookupSourceTrust;

    property LookupParamDateTime;
    property LookupParamClientId;
    property LookupParamTrustKind;

    property LookupFieldContractId;
    property LookupFieldContractName;
    property LookupFieldContractNumber;
    property LookupFieldContractDate;

    property LookupFieldAccountId;
    property LookupFieldAccountName;
    property LookupFieldAccountNumber;

    property LookupFieldAccountPartId;
    property LookupFieldAccountPartName;
    property LookupFieldAccountPartNumber;

    property LookupFieldTrustId;
    property LookupFieldTrustParent;
    property LookupFieldTrustName;
    property LookupParentTrust;
    property LookupConditionTrust;

    property ListColumnsContract;
    property ListColumnsAccount;
    property ListColumnsAccountPart;
    property ListColumnsTrust;

//    property LookupInterAccountSource;
    property Widths;
    property Captions;
    property ReadOnly;
    property EditStyle;
  end;

{$IFDEF FASTSCRIPT_RTTI}
  TfsFunctions_BackOfficeAccountPanel = class(TfsRTTIModule)
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}

  procedure Register;

implementation

uses
  SBaseStringFunctions, SBaseVariantFunctions, cxGraphics;

const
//  SCaption_Title        : String = 'Реальный счёт';
  SCaption_Client       : String = 'Клиент';
  SCaption_Contract     : String = 'Договор';
  SCaption_ByDate       : String = 'от';
  SCaption_Account      : String = 'Счёт';
  SCaption_AccountPart  : String = 'Раздел';
  SCaption_Trust        : String = 'Представитель';

  IConst_DefaultWidth             = 500;
  IConst_DefaultHeight            = 105;
  IConst_DefaultCaptionWidth      = 85;
  IConst_DefaultPaddingRight      = 0;
  IConst_CaptionWidthOffset       = 5;
  IConst_DefaultLayoutHeight      = 23;
  IConst_DefaultContractWidth     = 120;
  IConst_DefaultAccountWidth      = 90;
  IConst_DefaultAccountPartWidth  = 120;

  IConst_FirstCaptionLeft         = 5;
  IConst_SecondCaptionLeft        = 10;

procedure Register;
begin
  RegisterComponents ('BackOffice', [TBackOfficeAccountPanel]);
end;

{ TCustomBackOfficeAccountPanel }

constructor TCustomBackOfficeAccountPanel.Create(AOwner: TComponent);
begin
  inherited;

//  ControlStyle := ControlStyle - [csSetCaption];
//  BevelOuter := bvNone;
//  Style.BorderStyle := ebsNone;
//  PanelStyle.Active := True;

  AutoSizePercents := 0;
  Transparent := True;
  Width   := IConst_DefaultWidth;
  Height  := IConst_DefaultHeight;

//  FInitializing := True;

  { Капшин компонента }
  FCaptionLabel := TLabel.Create(Self);
  with FCaptionLabel do begin
    ParentFont  := False;
    Font.Style  := [fsBold];

    Margins.Left      := IConst_FirstCaptionLeft;
    Margins.Top       := 0;
    Margins.Right     := 0;
    Margins.Bottom    := 0;
    AlignWithMargins  := True;

    Caption := ''; //SCaption_Title;
//    Parent  := Self;
    Top     := 0;
    Align   := alTop;
  end;

  { Фрейм "Клиент" }
  FlpClient := TCustomLayoutPanel.Create(Self);
  with FlpClient do begin
    Caption.Text  := SCaption_Client;
    Caption.Left  := IConst_FirstCaptionLeft;
    Caption.Width := IConst_DefaultCaptionWidth;
    ParentFont    := False;
    Caption.Font.Style := [];
    Padding.Right := IConst_DefaultPaddingRight;
//    TabOrder      := 0;

    Parent := Self;
    Top    := 30;
    Align  := alTop;
    Height := IConst_DefaultLayoutHeight;
  end;

  { Компонента "Клиент" }
  FbetClient := TcxButtonEdit.Create(Self);
  with FbetClient do begin
    ParentFont  := False;
    with Properties do begin
      ReadOnly      := True;
      AutoSelect    := False;
    end;

    Parent  := FlpClient;
    Align   := alClient;
  end;

  { Панель "Договор"}
  FpnlContract := TCustomInternalPanel.Create(Self);
  with FpnlContract do begin
//    Caption       := '';
//    BevelOuter    := bvNone;
    Height        := IConst_DefaultLayoutHeight;
    Padding.Right := IConst_DefaultPaddingRight;
//    ParentFont    := False;
//    TabStop       := False;
//    TabOrder      := 1;

//    Parent        := Self;
//Name := 'FpnlContract';
    Top           := 60;
    Align         := alTop;

    OnResize := PanelContractOnResize;
  end;

//  { Панель "Договор" (2) }
//  FpnlContract2 := TPanel.Create(Self);
//  with FpnlContract2 do begin
//    Caption       := '';
//    BevelOuter    := bvNone;
//    Width         := IConst_DefaultAccountWidth + IConst_DefaultContractWidth;
//    ParentFont    := False;
//    TabStop       := False;
//
//    Align         := alRight;
//    Parent        := FpnlContract;
//  end;

  { Фрейм "Договор :: Номер" (Left) }
  FlpContractNumber := TCustomLayoutPanel.Create(Self);
  with FlpContractNumber do begin
    Width := IConst_DefaultCaptionWidth + IConst_DefaultContractWidth;
    Caption.Text  := SCaption_Contract;
    Caption.Left  := IConst_FirstCaptionLeft;
    Caption.Width := IConst_DefaultCaptionWidth;
    Caption.Font.Style  := [];
    TabStop       := True;

    Align  := alLeft;
    Parent := FpnlContract;
  end;

  { Компонента "Договор :: Номер" (Left) }
  FlcbContractNumber := TcxDBLookupComboBox.Create(Self);
  with FlcbContractNumber do begin
    ParentFont          := False;
    Style.Color         := clBtnFace;
//    TabStop             := False;
    with Properties do begin
      AutoSelect    := False;
      ImmediatePost := True;
{$IFNDEF PACKAGE}
      ClearKey := ShortCut_Del;
{$ENDIF}
//      ListOptions.ShowHeader  := False;
      ListOptions.GridLines   := glNone;
      IncrementalFilterContainMode := True;
    end;

    Align     := alClient;
    Parent    := FlpContractNumber;
  end;

  { Фрейм "Договор :: Дата" (Right) }
  FlpContractDate := TCustomLayoutPanel.Create(Self);
  with FlpContractDate do begin
    Caption.Width       := 15;
    Caption.Text        := SCaption_ByDate;
    Caption.Font.Style  := [];
    TabStop             := False;
    Width               := IConst_DefaultAccountWidth;

    Parent := FpnlContract;
    Align  := alRight;
  end;

  { Компонента "Договор :: Дата" (Right) }
  FlcbContractDate := TcxDBLookupComboBox.Create(Self);
  with FlcbContractDate do begin
    ParentFont          := False;
    Style.Color         := clBtnFace;
    with Properties do begin
      AutoSelect    := False;
      ReadOnly      := True;
      Alignment.Horz:= taCenter;
      Buttons.Clear;
    end;
    TabStop             := False;

    Align     := alClient;
    Parent    := FlpContractDate;
  end;

  { Фрейм "Договор :: Наименование" (Middle) }
  FlpContractName := TCustomLayoutPanel.Create(Self);
  with FlpContractName do begin
    Caption.Width       := 0;
    Caption.Text        := '';
    Caption.Font.Style  := [];

    TabStop             := False;
//    TabOrder      := 0;

    Align   := alClient;
    Parent  := FpnlContract;
  end;

  { Компонента "Договор :: Наименование" (Middle) }
  FlcbContractName := TcxDBLookupComboBox.Create(Self);
  with FlcbContractName do begin
    ParentFont          := False;
    TabStop             := False;
    Style.Color         := clBtnFace;
    with Properties do begin
      AutoSelect    := False;
      ReadOnly      := True;
      Buttons.Clear;
    end;

    Align   := alClient;
    Parent  := FlpContractName;
  end;

  { Панель "Счёт"}
  FpnlAccount := TCustomInternalPanel.Create(Self);
  with FpnlAccount do begin
//    Caption       := '';
//    BevelOuter    := bvNone;
    Height        := IConst_DefaultLayoutHeight;
    Padding.Right := IConst_DefaultPaddingRight;
//    ParentFont    := False;
//    TabStop       := False;
//    TabOrder      := 1;

    Top           := 60;
    Align         := alTop;
    Parent        := Self;

    OnResize := PanelAccountOnResize;
  end;

  { Фрейм "Счёт :: Номер" (2) }
  FlpAccountNumber := TCustomLayoutPanel.Create(Self);
  with FlpAccountNumber do begin
    Caption.Width       := 0;
    Caption.Font.Style  := [];
//    TabStop             := False;
//    TabOrder          := 1;
    Width               := IConst_DefaultAccountWidth;

    Parent := FpnlAccount;
    Align  := alRight;
  end;

  { Компонента "Счёт :: Номер" (2) }
  FlcbAccountNumber := TcxDBLookupComboBox.Create(Self);
  with FlcbAccountNumber do begin
    ParentFont          := False;
    Style.Color         := clBtnFace;
    with Properties do begin
      AutoSelect    := False;
      ReadOnly      := True;
      Buttons.Clear;
    end;
    TabStop             := False;

    Parent    := FlpAccountNumber;
    Align     := alClient;
  end;

  { Фрейм "Счёт :: Наименование" (1) }
  FlpAccountName := TCustomLayoutPanel.Create(Self);
  with FlpAccountName do begin
    Caption.Text  := SCaption_Account;
    Caption.Left  := IConst_FirstCaptionLeft;
//    Caption.Left  := Caption.Left;
    Caption.Width := IConst_DefaultCaptionWidth;
    Caption.Font.Style := [];
//    TabStop       := False;
//    TabOrder      := 0;

    Parent  := FpnlAccount;
    Align   := alClient;
  end;

  { Компонента "Счёт :: Наименование" (1) }
  FlcbAccountName := TcxMDDBLookupComboBox.Create(Self);
  with FlcbAccountName do begin
    ParentFont          := False;
//    TabStop             := False;
    with Properties do begin
      AutoSelect    := False;
      ImmediatePost := True;
{$IFNDEF PACKAGE}
      ClearKey := ShortCut_Del;
{$ENDIF}
//      ListOptions.ShowHeader  := False;
      ListOptions.GridLines   := glNone;
      IncrementalFilterContainMode := True;
    end;

    Parent  := FlpAccountName;
    Align   := alClient;
  end;

  { Панель "Раздел счёта"}
  FpnlAccountPart := TCustomInternalPanel.Create(Self);
  with FpnlAccountPart do begin
//    Caption       := '';
//    BevelOuter    := bvNone;
    Height        := IConst_DefaultLayoutHeight;
    Padding.Right := IConst_DefaultPaddingRight;
//    ParentFont    := False;
//    TabStop       := False;
//    TabOrder      := 1;

//    Parent        := Self; DefaultVisible = False
    Top           := 60;
    Align         := alTop;

    OnResize := PanelAccountPartOnResize;
  end;

  { Фрейм "Раздел счёта :: Номер" (2) }
  FlpAccountPartNumber := TCustomLayoutPanel.Create(Self);
  with FlpAccountPartNumber do begin
    Caption.Width       := 0;
    Caption.Font.Style  := [];
//    TabStop             := False;
//    TabOrder          := 1;
    Width               := IConst_DefaultAccountPartWidth;

    Parent := FpnlAccountPart;
    Align  := alRight;
  end;

  { Компонента "Раздел счёта :: Номер" (2) }
  FlcbAccountPartNumber := TcxDBLookupComboBox.Create(Self);
  with FlcbAccountPartNumber do begin
    ParentFont          := False;
    Style.Color         := clBtnFace;
    with Properties do begin
      AutoSelect    := False;
      ReadOnly      := True;
      Buttons.Clear;
    end;
    TabStop             := False;

    Parent    := FlpAccountPartNumber;
    Align     := alClient;
  end;

  { Фрейм "Раздел счёта :: Наименование" (1) }
  FlpAccountPartName := TCustomLayoutPanel.Create(Self);
  with FlpAccountPartName do begin
    Caption.Text  := SCaption_AccountPart;
    Caption.Left  := IConst_FirstCaptionLeft;
//    Caption.Left  := Caption.Left;
    Caption.Width := IConst_DefaultCaptionWidth;
    Caption.Font.Style := [];
//    TabStop       := False;
//    TabOrder      := 0;

    Parent  := FpnlAccountPart;
    Align   := alClient;
  end;

  { Компонента "Раздел счёта :: Наименование" (1) }
  FlcbAccountPartName := TcxDBLookupComboBox.Create(Self);
  with FlcbAccountPartName do begin
    ParentFont          := False;
//    TabStop             := False;
    with Properties do begin
      AutoSelect    := False;
      ImmediatePost := True;
{$IFNDEF PACKAGE}
      ClearKey := ShortCut_Del;
{$ENDIF}
//      ListOptions.ShowHeader  := False;
      ListOptions.GridLines   := glNone;
      IncrementalFilterContainMode := True;
    end;

    Parent  := FlpAccountPartName;
    Align   := alClient;
  end;

  { Фрейм "Представитель" }
  FlpTrust := TCustomLayoutPanel.Create(Self);
  with FlpTrust do begin
    Caption.Text  := SCaption_Trust;
    Caption.Left  := IConst_FirstCaptionLeft;
    Caption.Width := IConst_DefaultCaptionWidth;
    ParentFont := False;
    Caption.Font.Style := [];
    Padding.Right := IConst_DefaultPaddingRight;

    Height := IConst_DefaultLayoutHeight;
    Top    := 90;
    Align  := alTop;
  end;

  { Компонента "Представитель" }
  FlcbTrust := TcxMDDBLookupTreeComboBox.Create(Self);
  with FlcbTrust do begin
    ParentFont    := False;
    with Properties do begin
      AutoSelect    := False;
      TreeViewAlwaysExpandedLevel := -1;
      TreeViewShowHeaders         := True;
{$IFNDEF PACKAGE}
      ClearKey := ShortCut_Del;
{$ENDIF}
      ImmediatePost     := True;
      CanSelectParents  := True;
      TextStyle         := tvtcsFull;
      DividedString     := ' :: ';
    end;

    Parent  := FlpTrust;
    Align   := alClient;
  end;

{$IFNDEF PACKAGE}
  FLookupClient := TSBaseDBLookupComponentControl.Create(Self);
  FLookupClient.Control := FbetClient;

  FLookupMDDCContract := TMasterDetailDataSetControl.Create(Self);
  FLookupMDDCContract.Method := mdfmParams;
  FLookupMDDCContract.DataHelpers := [mdfhClearIfNotInList, mdfhSetWhenOneRecord];

  FLookupMDDCAccount := TMasterDetailDataSetControl.Create(Self);
  FLookupMDDCAccount.Method := mdfmParams;
  FLookupMDDCAccount.DataHelpers := [mdfhClearIfNotInList, mdfhSetWhenOneRecord];

  FLookupAccount := TSBaseCustomDBLookupComponentControl.Create(Self);
  FLookupAccount.Control := FlcbAccountName;

  FLookupMDDCAccountPart := TMasterDetailDataSetControl.Create(Self);
  FLookupMDDCAccountPart.Method := mdfmParams;
  FLookupMDDCAccountPart.DataHelpers := [mdfhClearIfNotInList, mdfhSetWhenOneRecord];

  FLookupMDDCTrust := TMasterDetailDataSetControl.Create(Self);
  FLookupMDDCTrust.Method := mdfmParams;
  FLookupMDDCTrust.DataHelpers := [mdfhClearIfNotInList];
{$ENDIF}

  FWidths   := TBackOfficeAccountPanelWidths.Create(Self);
  FCaptions := TCustomBackOfficeAccountPanelCaptions.Create(Self);
  FReadOnly := TBackOfficeAccountPanelReadOnly.Create(Self);
  FEditStyle:= True;
  FLookupParentTrust := baptlAuto;

//  FInitializing := False;
end;

destructor TCustomBackOfficeAccountPanel.Destroy;
begin
  inherited;
{$IFNDEF PACKAGE}
  FreeAndNil(FWidths);
  FreeAndNil(FCaptions);
  FreeAndNil(FReadOnly);
{$ENDIF}
end;

function TCustomBackOfficeAccountPanel.GetDataSource: TDataSource;
begin
{$IFNDEF PACKAGE}
  Result := FLookupClient.DataSource
{$ELSE}
  Result := FlcbAccountName.DataBinding.DataSource
{$ENDIF}
end;

procedure TCustomBackOfficeAccountPanel.SetDataSource(const Value: TDataSource);
begin
{$IFNDEF PACKAGE}
  FLookupClient.DataSource := Value;
  FLookupAccount.DataSource := Value;

  FlcbContractName.DataBinding.DataSource := Value;
  FlcbContractNumber.DataBinding.DataSource := Value;
  FlcbContractDate.DataBinding.DataSource := Value;
{$ENDIF}

  FlcbAccountName.DataBinding.DataSource := Value;

{$IFNDEF PACKAGE}
  FlcbAccountNumber.DataBinding.DataSource := Value;
  FlcbAccountPartName.DataBinding.DataSource := Value;
  FlcbAccountPartNumber.DataBinding.DataSource := Value;
  FlcbTrust.DataBinding.DataSource := Value;

  FLookupMDDCContract.DataSource := Value;
  FLookupMDDCContract.MasterSource := Value;

  FLookupMDDCAccount.DataSource := Value;
  FLookupMDDCAccount.MasterSource := Value;

  FLookupMDDCAccountPart.DataSource := Value;
  FLookupMDDCAccountPart.MasterSource := Value;

  FLookupMDDCTrust.DataSource := Value;
  FLookupMDDCTrust.MasterSource := Value;
{$ENDIF}
end;

procedure TCustomBackOfficeAccountPanel.SetEditStyle(const Value: Boolean);
begin
  if FEditStyle <> Value then begin
    FEditStyle := Value;
{$IFNDEF PACKAGE}
    ReBuild;
{$ENDIF}
  end;
end;

function TCustomBackOfficeAccountPanel.GetDataFieldAccountId: String;
begin
  Result := FlcbAccountName.DataBinding.DataField
end;

function TCustomBackOfficeAccountPanel.GetDataFieldAccountPartId: String;
begin
  Result := FlcbAccountPartName.DataBinding.DataField
end;

function TCustomBackOfficeAccountPanel.GetDataFieldClientId: String;
begin
{$IFNDEF PACKAGE}
  Result := FLookupClient.DataField
{$ELSE}
  Result := FDataFieldClientId
{$ENDIF}
end;

function TCustomBackOfficeAccountPanel.GetDataFieldTrustId: String;
begin
  Result := FlcbTrust.DataBinding.DataField
end;


procedure TCustomBackOfficeAccountPanel.SetDataFieldTrustKind(const Value: String);
begin
  if (FDataFieldTrustKind <> Value) then begin
    FDataFieldTrustKind := Value;
{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
  end;
end;

procedure TCustomBackOfficeAccountPanel.SetDataFieldClientId(const Value: String);
begin
{$IFNDEF PACKAGE}
  FLookupClient.DataField := Value;
  FLookupClient.ParamName := Value;
  Rebuild;
{$ELSE}
  FDataFieldClientId := Value;
{$ENDIF}
end;

function TCustomBackOfficeAccountPanel.GetDataFieldContractId: String;
begin
  Result := FlcbContractNumber.DataBinding.DataField
end;

procedure TCustomBackOfficeAccountPanel.SetDataFieldContractId(const Value: String);
begin
  if (DataFieldContractId <> Value) then begin
    FlcbContractNumber.DataBinding.DataField  := Value;
    FlcbContractName.DataBinding.DataField  := Value;
    FlcbContractDate.DataBinding.DataField    := Value;
{$IFNDEF PACKAGE}
    FLookupMDDCContract.DataField := Value;
    Rebuild;
{$ENDIF}
  end;
end;

procedure TCustomBackOfficeAccountPanel.SetDataFieldDateTime(const Value: String);
begin
  if FDataFieldDateTime <> Value then begin
    FDataFieldDateTime := Value;
{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
  end;
end;

procedure TCustomBackOfficeAccountPanel.SetLookupParamClientId(const Value: String);
begin
  if FLookupParamClientId <> Value then begin
    FLookupParamClientId := Value;
{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
  end;
end;

procedure TCustomBackOfficeAccountPanel.SetLookupParamDateTime(const Value: String);
begin
  if FLookupParamDateTime <> Value then begin
    FLookupParamDateTime := Value;
{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
  end;
end;

procedure TCustomBackOfficeAccountPanel.SetLookupParamTrustKind(const Value: String);
begin
  if FLookupParamTrustKind <> Value then begin
    FLookupParamTrustKind := Value;
{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
  end;
end;

procedure TCustomBackOfficeAccountPanel.SetLookupParentTrust(const Value: TBackOfficeAccountPanelTrustLevel);
begin
  if FLookupParentTrust <> Value then begin
    FLookupParentTrust := Value;
{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
  end;
end;

procedure TCustomBackOfficeAccountPanel.SetCaptions(const Value: TCustomBackOfficeAccountPanelCaptions);
begin
  FCaptions.Assign(Value);
end;

procedure TCustomBackOfficeAccountPanel.SetDataFieldAccountId(const Value: String);
begin
  if (DataFieldAccountId <> Value) then begin
    FlcbAccountName.DataBinding.DataField := Value;
    FlcbAccountNumber.DataBinding.DataField := Value;
{$IFNDEF PACKAGE}
    FLookupMDDCAccount.DataField := Value;
    Rebuild;
{$ENDIF}
  end;
end;

procedure TCustomBackOfficeAccountPanel.SetDataFieldAccountPartId(const Value: String);
begin
  if (DataFieldAccountPartId <> Value) then begin
    FlcbAccountPartName.DataBinding.DataField := Value;
    FlcbAccountPartNumber.DataBinding.DataField := Value;
{$IFNDEF PACKAGE}
    FLookupMDDCAccountPart.DataField := Value;
    Rebuild;
{$ENDIF}
  end;
end;

procedure TCustomBackOfficeAccountPanel.SetDataFieldTrustId(const Value: String);
begin
  if (DataFieldTrustId <> Value) then begin
    FlcbTrust.DataBinding.DataField := Value;
    FlcbTrust.Properties.KeyField := Value;
{$IFNDEF PACKAGE}
    FLookupMDDCTrust.DataField := Value;
{$ENDIF}
  end;
end;

procedure TCustomBackOfficeAccountPanel.SetListColumnsAccount(const Value: TcxLookupDBGridColumns);
begin
  FlcbAccountName.Properties.ListColumns := Value;
end;

procedure TCustomBackOfficeAccountPanel.SetListColumnsAccountPart(const Value: TcxLookupDBGridColumns);
begin
  FlcbAccountPartName.Properties.ListColumns := Value;
end;

procedure TCustomBackOfficeAccountPanel.SetListColumnsTrust(const Value: TcxMDLookupDBTreeListColumns);
begin
  FlcbTrust.Properties.ListColumns := Value;
end;

{$IFNDEF PACKAGE}
procedure TCustomBackOfficeAccountPanel.SetLookupAccount(const Value: TSBaseCustomDBLookupComponentControl);
begin
  FLookupAccount.Assign(Value);
end;

procedure TCustomBackOfficeAccountPanel.SetLookupClient(const Value: TSBaseDBLookupComponentControl);
begin
  FLookupClient.Assign(Value);
end;
{$ENDIF}

procedure TCustomBackOfficeAccountPanel.SetLookupConditionTrust(const Value: String);
begin
  if FLookupConditionTrust <> Value then begin
    FLookupConditionTrust := Value;
{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
  end;
end;

function TCustomBackOfficeAccountPanel.GetListColumnsAccount: TcxLookupDBGridColumns;
begin
  Result := FlcbAccountName.Properties.ListColumns;
end;

function TCustomBackOfficeAccountPanel.GetListColumnsAccountPart: TcxLookupDBGridColumns;
begin
  Result := FlcbAccountPartName.Properties.ListColumns;
end;

function TCustomBackOfficeAccountPanel.GetListColumnsContract: TcxLookupDBGridColumns;
begin
  Result := FlcbContractNumber.Properties.ListColumns;
end;

procedure TCustomBackOfficeAccountPanel.SetListColumnsContract(const Value: TcxLookupDBGridColumns);
begin
  FlcbContractNumber.Properties.ListColumns := Value;
end;

function TCustomBackOfficeAccountPanel.GetListColumnsTrust: TcxMDLookupDBTreeListColumns;
begin
  Result := FlcbTrust.Properties.ListColumns;
end;

function TCustomBackOfficeAccountPanel.GetLookupFieldAccountId: String;
begin
  Result := FlcbAccountName.Properties.KeyFieldNames
end;

function TCustomBackOfficeAccountPanel.GetLookupFieldAccountName: String;
begin
  Result := FlcbAccountName.Properties.ListFieldNames
end;

procedure TCustomBackOfficeAccountPanel.SetLookupFieldAccountId(const Value: String);
begin
  if LookupFieldAccountId <> Value then begin
    FlcbAccountName.Properties.KeyFieldNames := Value;
    FlcbAccountNumber.Properties.KeyFieldNames := Value;
{$IFNDEF PACKAGE}
    FLookupMDDCAccount.ListField := Value;
    Rebuild;
{$ENDIF}
  end;
end;

procedure TCustomBackOfficeAccountPanel.SetLookupFieldAccountName(const Value: String);
begin
  FlcbAccountName.Properties.ListFieldNames := Value;
end;

function TCustomBackOfficeAccountPanel.GetLookupFieldTrustId: String;
begin
  Result := FlcbTrust.Properties.KeyField
end;

function TCustomBackOfficeAccountPanel.GetLookupFieldTrustName: String;
begin
  Result := FlcbTrust.Properties.ListField
end;

procedure TCustomBackOfficeAccountPanel.SetLookupFieldTrustId(const Value: String);
begin
  FlcbTrust.Properties.KeyField := Value;
{$IFNDEF PACKAGE}
  FLookupMDDCTrust.ListField := Value;
  Rebuild;
{$ENDIF}
end;

procedure TCustomBackOfficeAccountPanel.SetLookupFieldTrustName(const Value: String);
begin
  FlcbTrust.Properties.ListField := Value
end;

function TCustomBackOfficeAccountPanel.GetLookupFieldTrustParent: String;
begin
  Result := FlcbTrust.Properties.ParentField
end;

procedure TCustomBackOfficeAccountPanel.SetLookupFieldTrustParent(const Value: String);
begin
  FlcbTrust.Properties.ParentField := Value
end;

function TCustomBackOfficeAccountPanel.GetLookupFieldAccountNumber: String;
begin
  Result := FlcbAccountNumber.Properties.ListFieldNames
end;

procedure TCustomBackOfficeAccountPanel.SetLookupFieldAccountNumber(const Value: String);
begin
  FlcbAccountNumber.Properties.ListFieldNames := Value;
end;

function TCustomBackOfficeAccountPanel.GetLookupFieldAccountPartId: String;
begin
  Result := FlcbAccountPartName.Properties.KeyFieldNames
end;

function TCustomBackOfficeAccountPanel.GetLookupFieldAccountPartName: String;
begin
  Result := FlcbAccountPartName.Properties.ListFieldNames
end;

function TCustomBackOfficeAccountPanel.GetLookupFieldAccountPartNumber: String;
begin
  Result := FlcbAccountPartNumber.Properties.ListFieldNames
end;

function TCustomBackOfficeAccountPanel.GetLookupFieldContractDate: String;
begin
  Result := FlcbContractDate.Properties.ListFieldNames
end;

function TCustomBackOfficeAccountPanel.GetLookupFieldContractId: String;
begin
  Result := FlcbContractNumber.Properties.KeyFieldNames
end;

function TCustomBackOfficeAccountPanel.GetLookupFieldContractName: String;
begin
  Result := FlcbContractName.Properties.ListFieldNames
end;

function TCustomBackOfficeAccountPanel.GetLookupFieldContractNumber: String;
begin
  Result := FlcbContractNumber.Properties.ListFieldNames
end;

procedure TCustomBackOfficeAccountPanel.SetLookupFieldAccountPartId(const Value: String);
begin
  FlcbAccountPartName.Properties.KeyFieldNames := Value;
  FlcbAccountPartNumber.Properties.KeyFieldNames := Value;
{$IFNDEF PACKAGE}
  FLookupMDDCAccountPart.ListField := Value;
  Rebuild;
{$ENDIF}
end;

procedure TCustomBackOfficeAccountPanel.SetLookupFieldAccountPartName(const Value: String);
begin
  FlcbAccountPartName.Properties.ListFieldNames := Value;
end;

procedure TCustomBackOfficeAccountPanel.SetLookupFieldAccountPartNumber(const Value: String);
begin
  FlcbAccountPartNumber.Properties.ListFieldNames := Value;
end;

procedure TCustomBackOfficeAccountPanel.SetLookupFieldContractDate(const Value: String);
begin
  FlcbContractDate.Properties.ListFieldNames := Value;
end;

procedure TCustomBackOfficeAccountPanel.SetLookupFieldContractId(const Value: String);
begin
  FlcbContractNumber.Properties.KeyFieldNames := Value;
  FlcbContractName.Properties.KeyFieldNames := Value;
  FlcbContractDate.Properties.KeyFieldNames := Value;
{$IFNDEF PACKAGE}
  FLookupMDDCContract.ListField := Value;
  Rebuild;
{$ENDIF}
end;

procedure TCustomBackOfficeAccountPanel.SetLookupFieldContractName(const Value: String);
begin
  FlcbContractName.Properties.ListFieldNames := Value;
end;

procedure TCustomBackOfficeAccountPanel.SetLookupFieldContractNumber(const Value: String);
begin
  FlcbContractNumber.Properties.ListFieldNames := Value;
end;

function TCustomBackOfficeAccountPanel.GetLookupSourceAccount: TDataSource;
begin
  Result := FlcbAccountName.Properties.ListSource
end;

procedure TCustomBackOfficeAccountPanel.SetLookupSourceAccount(const Value: TDataSource);
begin
  FlcbAccountNumber.Properties.ListSource := Value;
  FlcbAccountName.Properties.ListSource := Value;
{$IFNDEF PACKAGE}
  FLookupMDDCAccount.DetailSource := Value;
{$ENDIF}
end;

function TCustomBackOfficeAccountPanel.GetLookupSourceAccountPart: TDataSource;
begin
  Result := FlcbAccountPartName.Properties.ListSource
end;

procedure TCustomBackOfficeAccountPanel.SetLookupSourceAccountPart(const Value: TDataSource);
begin
  FlcbAccountPartName.Properties.ListSource := Value;
  FlcbAccountPartNumber.Properties.ListSource := Value;
{$IFNDEF PACKAGE}
  FLookupMDDCAccountPart.DetailSource := Value;
{$ENDIF}
end;

function TCustomBackOfficeAccountPanel.GetLookupSourceContract: TDataSource;
begin
  Result := FlcbContractNumber.Properties.ListSource
end;

procedure TCustomBackOfficeAccountPanel.SetLookupSourceContract(const Value: TDataSource);
begin
  FlcbContractName.Properties.ListSource   := Value;
  FlcbContractNumber.Properties.ListSource := Value;
  FlcbContractDate.Properties.ListSource   := Value;
{$IFNDEF PACKAGE}
  FLookupMDDCContract.DetailSource := Value;
{$ENDIF}
end;

function TCustomBackOfficeAccountPanel.GetLookupSourceTrust: TDataSource;
begin
  Result := FlcbTrust.Properties.ListSource
end;

procedure TCustomBackOfficeAccountPanel.SetLookupSourceTrust(const Value: TDataSource);
begin
  FlcbTrust.Properties.ListSource := Value;
{$IFNDEF PACKAGE}
  FLookupMDDCTrust.DetailSource := Value;
{$ENDIF}
end;

procedure TCustomBackOfficeAccountPanel.SetReadOnly(const Value: TBackOfficeAccountPanelReadOnly);
begin
  FReadOnly.Assign(Value);
end;

function TCustomBackOfficeAccountPanel.GetShowTrust: Boolean;
begin
  Result := FlpTrust.Parent <> nil;
end;

procedure TCustomBackOfficeAccountPanel.SetShowTrust(const Value: Boolean);
begin
  if ShowTrust <> Value then begin
    if Value then
      FlpTrust.Parent := Self
    else
      FlpTrust.Parent := nil;

    ReAlignPanels;
  end;
end;

function TCustomBackOfficeAccountPanel.GetShowAccount: Boolean;
begin
  Result := FpnlAccount.Parent <> nil;
end;

procedure TCustomBackOfficeAccountPanel.SetShowAccount(const Value: Boolean);
begin
  if ShowAccount <> Value then begin
    if Value then
      FpnlAccount.Parent := Self
    else
      FpnlAccount.Parent := nil;

    ReAlignPanels;
  end;
end;

function TCustomBackOfficeAccountPanel.GetShowAccountPart: Boolean;
begin
  Result := FpnlAccountPart.Parent <> nil;
end;

procedure TCustomBackOfficeAccountPanel.SetShowAccountPart(const Value: Boolean);
begin
  if ShowAccountPart <> Value then begin
    if Value then
      FpnlAccountPart.Parent := Self
    else
      FpnlAccountPart.Parent := nil;

    ReAlignPanels;
{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
  end;
end;

function TCustomBackOfficeAccountPanel.GetShowContract: Boolean;
begin
  Result := FpnlContract.Parent <> nil;
end;

procedure TCustomBackOfficeAccountPanel.SetShowContract(const Value: Boolean);
begin
  if ShowContract <> Value then begin
    if Value then
      FpnlContract.Parent := Self
    else
      FpnlContract.Parent := nil;

    ReAlignPanels;
{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
  end;
end;

function TCustomBackOfficeAccountPanel.GetShowClient: Boolean;
begin
  Result := FlpClient.Parent <> nil;
end;

procedure TCustomBackOfficeAccountPanel.SetShowClient(const Value: Boolean);
begin
  if ShowClient <> Value then begin
    if Value then
      FlpClient.Parent := Self
    else
      FlpClient.Parent := nil;

    ReAlignPanels;
    FCaptions.UpdateLeft;
{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
  end;
end;

procedure TCustomBackOfficeAccountPanel.SetWidths(const Value: TBackOfficeAccountPanelWidths);
begin
  FWidths.Assign(Value);
end;

{$IFNDEF PACKAGE}
procedure TCustomBackOfficeAccountPanel.IUpdateState(AColors: TControlColors);
begin
  if (not FLookupClient.ReadOnly) and (FLookupClient.SelectButtonIndex <> -1) and FLookupClient.CanModify then
    FbetClient.Style.Color := AColors.WriteAble.Color
  else
    FbetClient.Style.Color := AColors.ReadOnly.Color;

  if (not FlcbContractNumber.Properties.ReadOnly)
      and Assigned(FlcbContractNumber.DataBinding.Field)
      and FlcbContractNumber.DataBinding.Field.CanModify
      and FlcbContractNumber.DataBinding.DataSource.GetInternalCanModify
  then begin
    FlcbContractNumber.Style.Color := AColors.WriteAble.Color;
    FlcbContractNumber.Properties.ImmediateDropDownWhenActivated := True;
  end else begin
    FlcbContractNumber.Style.Color := AColors.ReadOnly.Color;
    FlcbContractNumber.Properties.ImmediateDropDownWhenActivated := False;
  end;

  if (not FlcbAccountName.Properties.ReadOnly)
      and FlcbAccountName.DataBinding.CanModify
      and FlcbAccountName.DataBinding.DataSource.GetInternalCanModify
  then begin
    FlcbAccountName.Style.Color := AColors.WriteAble.Color;
    FlcbAccountName.Properties.ImmediateDropDownWhenActivated := True;
  end else begin
    FlcbAccountName.Style.Color := AColors.ReadOnly.Color;
    FlcbAccountName.Properties.ImmediateDropDownWhenActivated := False;
  end;

  if (not FlcbAccountPartName.Properties.ReadOnly)
      and Assigned(FlcbAccountPartName.DataBinding.Field)
      and FlcbAccountPartName.DataBinding.Field.CanModify
      and FlcbAccountPartName.DataBinding.DataSource.GetInternalCanModify
  then begin
    FlcbAccountPartName.Style.Color := AColors.WriteAble.Color;
    FlcbAccountPartName.Properties.ImmediateDropDownWhenActivated := True;
  end else begin
    FlcbAccountPartName.Style.Color := AColors.ReadOnly.Color;
    FlcbAccountPartName.Properties.ImmediateDropDownWhenActivated := False;
  end;

  if (not FlcbTrust.Properties.ReadOnly)
      and Assigned(FlcbTrust.DataBinding.Field)
      and FlcbTrust.DataBinding.Field.CanModify
      and FlcbTrust.DataBinding.DataSource.GetInternalCanModify
  then begin
    FlcbTrust.Style.Color := AColors.WriteAble.Color;
    FlcbTrust.Properties.ImmediateDropDownWhenActivated := True;
  end else begin
    FlcbTrust.Style.Color := AColors.ReadOnly.Color;
    FlcbTrust.Properties.ImmediateDropDownWhenActivated := False;
  end;
end;

{$ENDIF}

procedure TCustomBackOfficeAccountPanel.PanelContractOnResize(Sender: TObject);
begin
  FlcbContractNumber.Properties.DropDownWidth := FpnlContract.Width
        - FpnlContract.Padding.Right
        - FlpContractNumber.Caption.Width
        - FlpContractNumber.Caption.Left
end;

procedure TCustomBackOfficeAccountPanel.Loaded;
begin
  inherited;

  if FReAlignPanelsRequited then ReAlignPanels;
{$IFNDEF PACKAGE}
  if FReBuildRequired then Rebuild;
{$ENDIF}
end;

procedure TCustomBackOfficeAccountPanel.ReAlignPanels;
var
  LTop      : Integer;
  LTabOrder : Integer;

  procedure _AlignPanel(APanel: TCustomInternalPanel);
  begin
    APanel.TabOrder := LTabOrder;
    Inc(LTabOrder);
    APanel.Top := LTop;
    LTop := APanel.Top + APanel.Height + 1;
  end;

begin
  if csLoading in ComponentState then begin
    FReAlignPanelsRequited := True;
    Exit;
  end;
  FReAlignPanelsRequited := False;

  LTop := 0;
  LTabOrder := 0;

  if ShowClient then
    _AlignPanel(FlpClient);

  if ShowContract then
    _AlignPanel(FpnlContract);

  if ShowAccount then
    _AlignPanel(FpnlAccount);

  if ShowAccountPart then
    _AlignPanel(FpnlAccountPart);

  if ShowTrust then
    _AlignPanel(FlpTrust);
end;

{$IFNDEF PACKAGE}
procedure TCustomBackOfficeAccountPanel.Rebuild;
var
//  LDateDepended: Boolean;
  LAndDateMasterFields, LAndDateDetailFields, LAndDateCondition: String;

  LDataFieldClientId, LLookupParamClientId: String;
  LDataFieldContractId, LLookupFieldContractId: String;
  LConfigContract: Boolean;

  LDataField, LParamName: String;
  LDataFieldAccountId, LLookupFieldAccountId: String;
  LConfigAccount: Boolean;
  LConfigAccountPart: Boolean;
  LConfigTrust: Boolean;

  LLookupField: String;
  LIdx: Integer;
  LDataFieldClientIdCondition, LCondition: String;
begin
  if csLoading in ComponentState then begin
    FReBuildRequired := True;
    Exit;
  end;
  FReBuildRequired := False;

  // Contract

  if (not FDataFieldDateTime.IsEmpty) and (not FLookupParamDateTime.IsEmpty) then begin
    LAndDateMasterFields  := ';' + FDataFieldDateTime;
    LAndDateDetailFields  := ';' + FLookupParamDateTime;
    LAndDateCondition     := ' ' + SConst_AND + ' ' + QuoteString(DataFieldDateTime, '[') + SCondition_IS_NOT_NULL;
  end else begin
    LAndDateMasterFields  := '';
    LAndDateDetailFields  := '';
    LAndDateCondition     := '';
  end;

  LDataFieldClientId      := DataFieldClientId;
  if LDataFieldClientId.IsEmpty then
    LDataFieldClientIdCondition := ''
  else begin
    if FindPos(';', LDataFieldClientId, LIdx) then
      LDataFieldClientIdCondition := Copy(LDataFieldClientId, 1, Pred(LIdx))
    else
      LDataFieldClientIdCondition := LDataFieldClientId;
    LDataFieldClientIdCondition := QuoteString(LDataFieldClientIdCondition, '[') + SCondition_IS_NOT_NULL
  end;

  LLookupParamClientId    := LookupParamClientId;
  LDataFieldContractId    := DataFieldContractId;
  LLookupFieldContractId  := LookupFieldContractId;
  LConfigContract         := (not LDataFieldContractId.IsEmpty) and (not LLookupFieldContractId.IsEmpty);

  if ShowContract and LConfigContract and (not EditStyle) then begin
    FLookupMDDCContract.MasterFields := LDataFieldContractId + LAndDateMasterFields;
    FLookupMDDCContract.DetailFields := LLookupFieldContractId + LAndDateDetailFields;
    FLookupMDDCContract.Condition    := QuoteString(LDataFieldContractId, '[') + SCondition_IS_NOT_NULL + LAndDateCondition;
    FLookupMDDCContract.Enabled      := True;
  end else if ShowContract and LConfigContract and EditStyle and (not LDataFieldClientId.IsEmpty) and (not LLookupParamClientId.IsEmpty) then begin
    FLookupMDDCContract.MasterFields := LDataFieldClientId + LAndDateMasterFields;
    FLookupMDDCContract.DetailFields := LLookupParamClientId + LAndDateDetailFields;
    FLookupMDDCContract.Condition    := LDataFieldClientIdCondition + LAndDateCondition;
    FLookupMDDCContract.Enabled      := True;
  end else begin
    FLookupMDDCContract.Enabled      := False;
    FLookupMDDCContract.MasterFields := '';
    FLookupMDDCContract.DetailFields := '';
  end;

  //  Account

  LDataFieldAccountId     := DataFieldAccountId;
  LLookupFieldAccountId   := LookupFieldAccountId;
  LConfigAccount          := (not LDataFieldAccountId.IsEmpty) and (not LLookupFieldAccountId.IsEmpty);

  if ShowAccount and LConfigAccount and (not EditStyle) and (not LDataFieldAccountId.IsEmpty) then begin
    FLookupMDDCAccount.MasterFields := LDataFieldAccountId + LAndDateMasterFields;
    FLookupMDDCAccount.DetailFields := LLookupFieldAccountId + LAndDateDetailFields;
    FLookupMDDCAccount.Condition    := QuoteString(LDataFieldAccountId, '[') + SCondition_IS_NOT_NULL + LAndDateCondition;
    FLookupMDDCAccount.Enabled      := True;
  end else if ShowAccount and LConfigAccount and LConfigContract then begin
    FLookupMDDCAccount.MasterFields := LDataFieldContractId + LAndDateMasterFields;
    FLookupMDDCAccount.DetailFields := LLookupFieldContractId + LAndDateDetailFields;
    FLookupMDDCAccount.Condition    := QuoteString(LDataFieldContractId, '[') + SCondition_IS_NOT_NULL + LAndDateCondition;
    FLookupMDDCAccount.Enabled      := True;
  end else if ShowAccount and LConfigAccount and (not LConfigContract) and (not LDataFieldClientId.IsEmpty) then begin
    FLookupMDDCAccount.MasterFields := LDataFieldClientId + LAndDateMasterFields;
    FLookupMDDCAccount.DetailFields := LLookupParamClientId + LAndDateDetailFields;
    FLookupMDDCAccount.Condition    := LDataFieldClientIdCondition + LAndDateCondition;
    FLookupMDDCAccount.Enabled      := True;
  end else begin
    FLookupMDDCAccount.Enabled      := False;
    FLookupMDDCAccount.MasterFields := '';
    FLookupMDDCAccount.DetailFields := '';
  end;

  if ShowAccount and LConfigAccount then begin
    if ShowClient and (not DataFieldClientId.IsEmpty) and (not LookupParamClientId.IsEmpty) then begin
      LDataField := DataFieldClientId;
      LParamName := LookupParamClientId;
    end else begin
      LDataField := '';
      LParamName := '';
    end;

    if ShowContract and (not DataFieldContractId.IsEmpty) and (not LookupFieldContractId.IsEmpty) then begin
      LDataField := ConcatIfNotEmpty(LDataField, ';', DataFieldContractId);
      LParamName := ConcatIfNotEmpty(LParamName, ';', LookupFieldContractId);
    end;

    if (not DataFieldAccountId.IsEmpty) and (not LookupFieldAccountId.IsEmpty) then begin
      LDataField := ConcatIfNotEmpty(LDataField, ';', DataFieldAccountId);
      LParamName := ConcatIfNotEmpty(LParamName, ';', LookupFieldAccountId);
    end;

    FLookupAccount.DataField := LDataField;
    FLookupAccount.ParamName := LParamName;
  end;

  // AccountPart
  LConfigAccountPart  := (not DataFieldAccountPartId.IsEmpty) and (not LookupFieldAccountPartId.IsEmpty);

  if ShowAccountPart and LConfigAccountPart and (not LDataFieldAccountId.IsEmpty) and (not LLookupFieldAccountId.IsEmpty) then begin
    FLookupMDDCAccountPart.MasterFields := LDataFieldAccountId + LAndDateMasterFields;
    FLookupMDDCAccountPart.DetailFields := LLookupFieldAccountId + LAndDateDetailFields;
    FLookupMDDCAccountPart.Condition := QuoteString(LDataFieldAccountId, '[') + SCondition_IS_NOT_NULL + LAndDateCondition;

    FLookupMDDCAccountPart.Enabled      := True;
  end else begin
    FLookupMDDCAccountPart.Enabled      := False;
    FLookupMDDCAccountPart.MasterFields := '';
    FLookupMDDCAccountPart.DetailFields := '';
  end;

  // Trust
  LConfigTrust  := (not DataFieldTrustId.IsEmpty) and (not LookupFieldTrustId.IsEmpty);
  if ShowTrust and LConfigTrust then begin
    if LConfigAccountPart and (FLookupParentTrust in [baptlAuto, baptlAccountPart]) then begin
      LDataField    := DataFieldAccountPartId;
      LLookupField  := LookupFieldAccountPartId;
    end else if LConfigAccount and (FLookupParentTrust in [baptlAuto, baptlAccount]) then begin
      LDataField    := DataFieldAccountId;
      LLookupField  := LookupFieldAccountId;
    end else if LConfigContract and (FLookupParentTrust in [baptlAuto, baptlContract]) then begin
      LDataField    := DataFieldContractId;
      LLookupField  := LookupFieldContractId;
    end else if (FLookupParentTrust in [baptlAuto, baptlClient]) then begin
      LDataField    := DataFieldClientId;
      LLookupField  := LookupParamClientId;
    end else begin
      LDataField    := '';
      LLookupField  := '';
    end;
    LConfigTrust := (not LDataField.IsEmpty) and (not LLookupField.IsEmpty);
  end;

  if ShowTrust and LConfigTrust then begin
    if FindPos(';', LDataField, LIdx) then
      LCondition := Copy(LDataField, 1, Pred(LIdx))
    else
      LCondition := LDataField;
    LCondition := QuoteString(LCondition, '[') + SCondition_IS_NOT_NULL;

    if not FLookupConditionTrust.IsEmpty then
      LCondition := '((' + FLookupConditionTrust + ') OR (' + LCondition;

    if (not DataFieldTrustKind.IsEmpty) and (not LookupParamTrustKind.IsEmpty) then begin
      LDataField    := LDataField + ';' + DataFieldTrustKind;
      LLookupField  := LLookupField + ';' + LookupParamTrustKind;
      LCondition    := LCondition +  ' ' + SConst_AND + ' ' + QuoteString(DataFieldTrustKind, '[') + SCondition_IS_NOT_NULL;
    end;

    if not FLookupConditionTrust.IsEmpty then
      LCondition := LCondition + '))';

    FLookupMDDCTrust.MasterFields := LDataField + LAndDateMasterFields;
    FLookupMDDCTrust.DetailFields := LLookupField + LAndDateDetailFields;
    FLookupMDDCTrust.Condition    := LCondition + LAndDateCondition;
    FLookupMDDCTrust.Enabled := True;
  end else begin
    FLookupMDDCTrust.Enabled := False;
    FLookupMDDCTrust.MasterFields := '';
    FLookupMDDCTrust.DetailFields := '';
  end;
end;
{$ENDIF}

procedure TCustomBackOfficeAccountPanel.PanelAccountOnResize(Sender: TObject);
begin
  FlcbAccountName.Properties.DropDownWidth := FpnlAccount.Width
        - FpnlAccount.Padding.Right
        - FlpAccountName.Caption.Width
        - FlpAccountName.Caption.Left
end;

procedure TCustomBackOfficeAccountPanel.PanelAccountPartOnResize(Sender: TObject);
begin
  FlcbAccountPartName.Properties.DropDownWidth := FpnlAccountPart.Width
        - FpnlAccountPart.Padding.Right
        - FlpAccountPartName.Caption.Width
        - FlpAccountPartName.Caption.Left
end;

{ TBackOfficeAccountPanelWidths }

constructor TBackOfficeAccountPanelWidths.Create(AOwner: TCustomBackOfficeAccountPanel);
begin
  FOwner := AOwner;
  inherited Create;
end;

function TBackOfficeAccountPanelWidths.GetCaption: Integer;
begin
  Result := FOwner.FlpClient.Caption.Width
end;

procedure TBackOfficeAccountPanelWidths.SetCaption(Value: Integer);
var
  LDelta: Integer;
begin
  if (Value < 0) then Value := 0;
  LDelta := (Value - GetCaption);
  if LDelta = 0 then Exit;

  FOwner.FlpClient.Caption.Width          := Value;
  FOwner.FlpContractNumber.Caption.Width  := Value - IfThen(FOwner.Captions.AutoAlign, IConst_CaptionWidthOffset, 0);
  FOwner.FlpContractNumber.Width := FOwner.FlpContractNumber.Width + LDelta;

  FOwner.FlpAccountName.Caption.Width     := Value - IfThen(FOwner.Captions.AutoAlign, IConst_CaptionWidthOffset, 0);
  FOwner.FlpAccountPartName.Caption.Width := Value - IfThen(FOwner.Captions.AutoAlign, IConst_CaptionWidthOffset, 0);
  FOwner.FlpTrust.Caption.Width           := Value;
end;

function TBackOfficeAccountPanelWidths.IsCaptionStored: Boolean;
begin
  Result := (FOwner.FlpClient.Caption.Width <> IConst_DefaultCaptionWidth);
end;

function TBackOfficeAccountPanelWidths.GetContract: Integer;
begin
  Result := FOwner.FlpContractNumber.Width - FOwner.FlpContractNumber.Caption.Width
end;

procedure TBackOfficeAccountPanelWidths.SetContract(Value: Integer);
begin
  if Value < 50 then Value := 50;

  FOwner.FlpContractNumber.Width := Value + FOwner.FlpContractNumber.Caption.Width;
//  FOwner.FlpContractNumber.Width := Value;
end;

function TBackOfficeAccountPanelWidths.IsContractStored: Boolean;
begin
  Result := (Contract <> IConst_DefaultContractWidth)
end;

function TBackOfficeAccountPanelWidths.GetAccount: Integer;
begin
  Result := FOwner.FlpAccountNumber.Width
end;

procedure TBackOfficeAccountPanelWidths.SetAccount(Value: Integer);
begin
  if Value < 50 then Value := 50;

  FOwner.FlpAccountNumber.Width := Value;
end;

function TBackOfficeAccountPanelWidths.IsAccountStored: Boolean;
begin
  Result := (Account <> IConst_DefaultAccountWidth)
end;

function TBackOfficeAccountPanelWidths.GetAccountPart: Integer;
begin
  Result := FOwner.FlpAccountPartNumber.Width
end;

procedure TBackOfficeAccountPanelWidths.SetAccountPart(Value: Integer);
begin
  if Value < 50 then Value := 50;

  FOwner.FlpAccountPartNumber.Width := Value;
end;

function TBackOfficeAccountPanelWidths.IsAccountPartStored: Boolean;
begin
  Result := (AccountPart <> IConst_DefaultAccountPartWidth)
end;

{ TCustomBackOfficeAccountPanelCaptions }

procedure TCustomBackOfficeAccountPanelCaptions.AssignTo(Dest: TPersistent);
begin
  inherited;
  with TCustomBackOfficeAccountPanelCaptions(Dest) do begin
    Title         := Self.Title;
    Client        := Self.Client;
    Account       := Self.Account;
    AccountPart   := Self.AccountPart;
    Trust         := Self.Trust;
  end;
end;

constructor TCustomBackOfficeAccountPanelCaptions.Create(AOwner: TCustomBackOfficeAccountPanel);
begin
  FOwner := AOwner;
  inherited Create;
end;

function TCustomBackOfficeAccountPanelCaptions.GetAccount: String;
begin
  Result := FOwner.FlpAccountName.Caption.Text
end;

procedure TCustomBackOfficeAccountPanelCaptions.SetAccount(const Value: String);
begin
  FOwner.FlpAccountName.Caption.Text := Value
end;

function TCustomBackOfficeAccountPanelCaptions.IsAccountStored: Boolean;
begin
  Result := (Account <> SCaption_Account)
end;

function TCustomBackOfficeAccountPanelCaptions.GetAccountPart: String;
begin
  Result := FOwner.FlpAccountPartName.Caption.Text
end;

procedure TCustomBackOfficeAccountPanelCaptions.SetAccountPart(const Value: String);
begin
  FOwner.FlpAccountPartName.Caption.Text := Value
end;

procedure TCustomBackOfficeAccountPanelCaptions.SetAutoAlign(const Value: Boolean);
begin
  if FAutoAlign <> Value then begin
    FAutoAlign := Value;
    UpdateLeft;
  end;
end;

function TCustomBackOfficeAccountPanelCaptions.IsAccountPartStored: Boolean;
begin
  Result := (AccountPart <> SCaption_AccountPart)
end;

function TCustomBackOfficeAccountPanelCaptions.GetTrust: String;
begin
  Result := FOwner.FlpTrust.Caption.Text
end;

procedure TCustomBackOfficeAccountPanelCaptions.SetTrust(const Value: String);
begin
  FOwner.FlpTrust.Caption.Text := Value;
end;

procedure TCustomBackOfficeAccountPanelCaptions.UpdateLeft;
var
  LLeft1, LLeft2: Integer;
begin
  LLeft1 := IfThen((Length(Title) > 0), IConst_SecondCaptionLeft, IConst_FirstCaptionLeft);
  LLeft2 := LLeft1 + IfThen(FOwner.ShowClient, IConst_CaptionWidthOffset, 0);

  FOwner.FlpClient.Caption.Left           := LLeft1;
  FOwner.FlpContractNumber.Caption.Left   := IfThen(FAutoAlign, LLeft2, LLeft1);
  FOwner.FlpAccountName.Caption.Left      := IfThen(FAutoAlign, LLeft2, LLeft1);
  FOwner.FlpAccountPartName.Caption.Left  := IfThen(FAutoAlign, LLeft2, LLeft1);
  FOwner.FlpTrust.Caption.Left            := LLeft1;
end;

function TCustomBackOfficeAccountPanelCaptions.IsTrustStored: Boolean;
begin
  Result := (Trust <> SCaption_Trust)
end;

function TCustomBackOfficeAccountPanelCaptions.GetClient: String;
begin
  Result := FOwner.FlpClient.Caption.Text
end;

function TCustomBackOfficeAccountPanelCaptions.GetContract: String;
begin
  Result := FOwner.FlpContractNumber.Caption.Text
end;

procedure TCustomBackOfficeAccountPanelCaptions.SetContract(const Value: String);
begin
  FOwner.FlpContractNumber.Caption.Text := Value;
end;

function TCustomBackOfficeAccountPanelCaptions.IsContractStored: Boolean;
begin
  Result := (Contract <> SCaption_Contract)
end;

procedure TCustomBackOfficeAccountPanelCaptions.SetClient(const Value: String);
begin
  FOwner.FlpClient.Caption.Text := Value
end;

function TCustomBackOfficeAccountPanelCaptions.IsClientStored: Boolean;
begin
  Result := (Client <> SCaption_Client)
end;

function TCustomBackOfficeAccountPanelCaptions.GetTitle: String;
begin
  Result := FOwner.FCaptionLabel.Caption
end;

procedure TCustomBackOfficeAccountPanelCaptions.SetTitle(const Value: String);
begin
  FOwner.FCaptionLabel.Caption := Value;

  if (Length(Value) > 0) <> (FOwner.FCaptionLabel.Parent <> nil) then begin
    if (Length(Value) > 0) then begin
      FOwner.FCaptionLabel.Parent := FOwner;
      FOwner.FCaptionLabel.Top := 0;
    end else
      FOwner.FCaptionLabel.Parent := nil;

    UpdateLeft;
  end;
end;

{ TBackOfficeAccountPanelReadOnly }

constructor TBackOfficeAccountPanelReadOnly.Create(AOwner: TCustomBackOfficeAccountPanel);
begin
  FOwner := AOwner;
  inherited Create;
end;

{$IFNDEF PACKAGE}
function TBackOfficeAccountPanelReadOnly.GetClient: Boolean;
begin
  Result := FOwner.LookupClient.ReadOnly;
end;

procedure TBackOfficeAccountPanelReadOnly.SetClient(const Value: Boolean);
begin
  FOwner.LookupClient.ReadOnly := Value;
end;

function TBackOfficeAccountPanelReadOnly.GetContract: Boolean;
begin
  Result := FOwner.FlcbContractNumber.Properties.ReadOnly;
end;

procedure TBackOfficeAccountPanelReadOnly.SetContract(const Value: Boolean);
begin
  if (Contract <> Value) then begin
    FOwner.FlcbContractNumber.Properties.ReadOnly := Value;

    if Contract then
      FOwner.LookupMDDCContract.DataHelpers := []
    else
      FOwner.LookupMDDCContract.DataHelpers := [mdfhClearIfNotInList, mdfhSetWhenOneRecord];
  end;
end;

function TBackOfficeAccountPanelReadOnly.GetAccount: Boolean;
begin
  Result := FOwner.FlcbAccountName.Properties.ReadOnly;
end;

procedure TBackOfficeAccountPanelReadOnly.SetAccount(const Value: Boolean);
begin
  if (Account <> Value) then begin
    FOwner.FlcbAccountName.Properties.ReadOnly := Value;
    FOwner.FLookupAccount.ReadOnly := Value;

    if Account then
      FOwner.LookupMDDCAccount.DataHelpers := []
    else
      FOwner.LookupMDDCAccount.DataHelpers := [mdfhClearIfNotInList, mdfhSetWhenOneRecord];
  end;
end;

function TBackOfficeAccountPanelReadOnly.GetAccountPart: Boolean;
begin
  Result := FOwner.FlcbAccountPartName.Properties.ReadOnly
end;

procedure TBackOfficeAccountPanelReadOnly.SetAccountPart(const Value: Boolean);
begin
  if (AccountPart <> Value) then begin
    FOwner.FlcbAccountPartName.Properties.ReadOnly := Value;

    if AccountPart then
      FOwner.LookupMDDCAccount.DataHelpers := []
    else
      FOwner.LookupMDDCAccount.DataHelpers := [mdfhClearIfNotInList, mdfhSetWhenOneRecord];
  end;
end;

function TBackOfficeAccountPanelReadOnly.GetTrust: Boolean;
begin
  Result := FOwner.FlcbTrust.Properties.ReadOnly;
end;

procedure TBackOfficeAccountPanelReadOnly.SetTrust(const Value: Boolean);
begin
  if (Trust <> Value) then begin
    FOwner.FlcbTrust.Properties.ReadOnly := Value;
    with FOwner.FLookupMDDCTrust do
      if Value then
        DataHelpers := DataHelpers - [mdfhClearIfNotInList]
      else
        DataHelpers := DataHelpers + [mdfhClearIfNotInList]
  end;
end;
{$ENDIF}

{$IFDEF FASTSCRIPT_RTTI}
{ TfsFunctions_BackOfficeAccountPanel }

constructor TfsFunctions_BackOfficeAccountPanel.Create(AScript: TfsScript);
begin
  inherited;
//  AScript.AddClass(TCustomBackOfficeAccountPanel, TCustomBackOfficeAccountPanel.ClassParent.ClassName);
//  AScript.AddClass(TBackOfficeAccountPanel, TBackOfficeAccountPanel.ClassParent.ClassName);
end;
{$ENDIF}

initialization
  RegisterClasses([TCustomBackOfficeAccountPanel, TBackOfficeAccountPanel, TBackOfficeAccountPanelReadOnly, TBackOfficeAccountPanelWidths]);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_BackOfficeAccountPanel);
{$ENDIF}

finalization
  UnRegisterClasses([TCustomBackOfficeAccountPanel, TBackOfficeAccountPanel, TBackOfficeAccountPanelReadOnly, TBackOfficeAccountPanelWidths]);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TfsFunctions_BackOfficeAccountPanel);
{$ENDIF}

end.
