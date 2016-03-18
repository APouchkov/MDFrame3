unit BaseAccountPanel;
{$I config.inc}

interface

uses
  Classes, DB, Controls, StdCtrls, ExtCtrls, Graphics, LayoutPanel, cxButtonEdit, cxEdit,
  cxTextEdit, cxDBEdit, SysUtils, FieldDataLink, cxDBLookupComboBox, cxLookupDBGrid, Dialogs,
  StrUtils, cxGroupBox, cxMDLookupTreeComboBox
{$IFNDEF PACKAGE}
  , SBaseForm
  , SBaseFormDefinitions
  , SBaseControls
  , SBaseDataModule
  , SBaseConstants
{$ENDIF}
  ;

type
  TCustomBaseAccountPanel = class;

  TBaseAccountPanelWidths = class(TPersistent)
  private
    [weak] FOwner: TCustomBaseAccountPanel;

    function  GetAccount: Integer;
    procedure SetAccount(Value: Integer);
    function  IsAccountStored: Boolean;

    function  GetCaption: Integer;
    procedure SetCaption(Value: Integer);
    function  IsCaptionStored: Boolean;
  public
    constructor Create(AOwner: TCustomBaseAccountPanel); reintroduce;
  published
    property Caption: Integer read GetCaption write SetCaption stored IsCaptionStored;
    property Account: Integer read GetAccount write SetAccount stored IsAccountStored;
  end;

  TCustomBaseAccountPanelCaptions = class(TPersistent)
  private
    [weak] FOwner: TCustomBaseAccountPanel;

    function GetOrganization: String;
    procedure SetOrganization(const Value: String);
    function IsOrganizationStored: Boolean;

    function GetIntermediary: String;
    procedure SetIntermediary(const Value: String);
    function IsIntermediaryStored: Boolean;

    function GetAccount: String;
    procedure SetAccount(const Value: String);
    function IsAccountStored: Boolean;

    function GetAccountPart: String;
    procedure SetAccountPart(const Value: String);
    function IsAccountPartStored: Boolean;

    function GetAccountPartDetails: String;
    procedure SetAccountPartDetails(const Value: String);
    function IsAccountPartDetailsStored: Boolean;

    function GetTitle: String;
    procedure SetTitle(const Value: String);
  public
    constructor Create(AOwner: TCustomBaseAccountPanel); reintroduce;
  protected
//    function GetOwner: TPersistent; override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Title: String read GetTitle write SetTitle; // stored IsTitleStored;
    property Organization: String read GetOrganization write SetOrganization stored IsOrganizationStored;
    property Account: String read GetAccount write SetAccount stored IsAccountStored;
    property AccountPart: String read GetAccountPart write SetAccountPart stored IsAccountPartStored;
    property Intermediary: String read GetIntermediary write SetIntermediary stored IsIntermediaryStored;
    property Details: String read GetAccountPartDetails write SetAccountPartDetails stored IsAccountPartDetailsStored;
  end;

  TBaseAccountPanelReadOnly = class(TPersistent)
  private
    [weak] FOwner: TCustomBaseAccountPanel;

    FInterOrganization: Boolean;
    FInterAccount: Boolean;
{$IFDEF PACKAGE}
    FAccount: Boolean;
    FAccountPart: Boolean;
{$ELSE}
    function GetAccount: Boolean;
    procedure SetAccount(const Value: Boolean);

    function GetAccountPart: Boolean;
    procedure SetAccountPart(const Value: Boolean);

    procedure SetInterOrganization(const Value: Boolean);
    procedure SetInterAccount(const Value: Boolean);
{$ENDIF}
  public
    constructor Create(AOwner: TCustomBaseAccountPanel); reintroduce;
  published
    property Account          : Boolean read {$IFDEF PACKAGE}FAccount           {$ELSE}GetAccount           {$ENDIF} write {$IFDEF PACKAGE}FAccount           {$ELSE}SetAccount           {$ENDIF} Default False;
    property AccountPart      : Boolean read {$IFDEF PACKAGE}FAccountPart       {$ELSE}GetAccountPart       {$ENDIF} write {$IFDEF PACKAGE}FAccountPart       {$ELSE}SetAccountPart       {$ENDIF} Default False;
    property InterOrganization: Boolean read FInterOrganization write {$IFDEF PACKAGE}FInterOrganization {$ELSE}SetInterOrganization {$ENDIF} Default False;
    property InterAccount     : Boolean read FInterAccount      write {$IFDEF PACKAGE}FInterAccount      {$ELSE}SetInterAccount      {$ENDIF} Default False;
  end;

  TCustomBaseAccountPanel = class(TcxCustomLayoutGroupBox{$IFNDEF PACKAGE}, ISBaseReadOnlyInterface{$ENDIF})
  strict private
    FReAlignPanelsRequired: Boolean;
{$IFNDEF PACKAGE}
    FReBuildRequired: Boolean;
{$ENDIF}
  private
    FWidths   : TBaseAccountPanelWidths;
    FCaptions : TCustomBaseAccountPanelCaptions;
    FReadOnly : TBaseAccountPanelReadOnly;
    FEditStyle: Boolean;

    FCaptionLabel: TLabel;
    FlpOrganization: TCustomLayoutPanel;
    FlcbAccountOrganizationName: TcxDBLookupComboBox;

    FlpAccount: TCustomLayoutPanel;
    FlcbAccountOwner: TcxDBLookupComboBox;
    FlcbAccountNumber: TcxDBLookupComboBox;

    FlpInter: TCustomLayoutPanel;
    FlcbInterOrganizationName: TcxDBLookupComboBox;
    FlcbInterAccountNumber: TcxDBLookupComboBox;

    FlpAccountPart: TCustomLayoutPanel;
    FltcbAccountPartName: TcxMDDBLookupTreeComboBox;
    FlcbAccountPartNumber: TcxDBLookupComboBox;

    FlpAccountPartDetails: TCustomLayoutPanel;
    FlcbAccountPartDetails: TcxDBLookupComboBox;

//    FDataFieldAccountOrganizationId: String;
    FDataFieldInterOrganizationId: String;
    FDataFieldInterAccountId: String;
    FDataFieldAccountRootId: String;

//    FLookupFieldAccountOrganizationId: String;
    FLookupFieldInterOrganizationId: String;
    FLookupFieldInterAccountId: String;
    FDataFieldParent: String;
{$IFDEF PACKAGE}
    FLookupSourceInterOrganization: TDataSource;
    FLookupSourceInterAccount: TDataSource;
    FLookupParamParent: String;
{$ELSE}
    FLookup: TSBaseCustomDBLookupComponentControl;
//    FLookupInterOrganization: TSBaseCustomDBLookupComponentControl;

    FLookupMDDC                 : TMasterDetailDataSetControl;
    FLookupMDDCInterOrganization: TMasterDetailDataSetControl;
    FLookupMDDCInterAccount     : TMasterDetailDataSetControl;
    FLookupMDDCAccountPart      : TMasterDetailDataSetControl;
{$ENDIF}

    function  GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);

    procedure SetDataFieldParent(const Value: String);

    function  GetDataFieldAccountId: String;
    procedure SetDataFieldAccountId(const Value: String);

    procedure SetDataFieldInterOrganizationId(const Value: String);

    function  GetDataFieldInterAccountId: String;
    procedure SetDataFieldInterAccountId(const Value: String);

    function  GetDataFieldAccountPartId: String;
    procedure SetDataFieldAccountPartId(const Value: String);

    procedure SetDataFieldAccountRootId(const Value: String);

    function  GetLookupSource: TDataSource;
    procedure SetLookupSource(const Value: TDataSource);

    function  GetLookupSourceInterOrganization: TDataSource;
    procedure SetLookupSourceInterOrganization(const Value: TDataSource);

    function  GetLookupSourceAccountPart: TDataSource;
    procedure SetLookupSourceAccountPart(const Value: TDataSource);

    function  GetLookupFieldAccountId: String;
    procedure SetLookupFieldAccountId(const Value: String);

    function  GetLookupFieldAccountOrganizationName: String;
    procedure SetLookupFieldAccountOrganizationName(const Value: String);

    function  GetLookupFieldAccountNumber: String;
    procedure SetLookupFieldAccountNumber(const Value: String);

    function  GetLookupFieldAccountOwner: String;
    procedure SetLookupFieldAccountOwner(const Value: String);

    procedure SetLookupFieldInterOrganizationId(const Value: String);
    procedure SetLookupFieldInterAccountId(const Value: String);

    function  GetLookupSourceInterAccount: TDataSource;
    procedure SetLookupSourceInterAccount(const Value: TDataSource);

    function  GetLookupFieldInterOrganizationName: String;
    procedure SetLookupFieldInterOrganizationName(const Value: String);

    function  GetLookupFieldInterAccountNumber: String;
    procedure SetLookupFieldInterAccountNumber(const Value: String);

    function  GetLookupFieldAccountPartId: String;
    procedure SetLookupFieldAccountPartId(const Value: String);

    function  GetLookupFieldAccountPartName: String;
    procedure SetLookupFieldAccountPartName(const Value: String);

    function  GetLookupFieldAccountPartNumber: String;
    procedure SetLookupFieldAccountPartNumber(const Value: String);

    function  GetLookupFieldAccountPartDetails: String;
    procedure SetLookupFieldAccountPartDetails(const Value: String);

    function GetLookupParamParent: String;
    procedure SetLookupParamParent(const Value: String);

//    procedure SetLookupFieldAccountOrganizationId(const Value: String);

    function  GetShowIntermediary: Boolean;
    procedure SetShowIntermediary(const Value: Boolean);

    function  GetShowAccountPart: Boolean;
    procedure SetShowAccountPart(const Value: Boolean);

    function  GetShowAccountPartDetails: Boolean;
    procedure SetShowAccountPartDetails(const Value: Boolean);

    procedure ReAlignPanels;
{$IFNDEF PACKAGE}
    procedure SetLookup(const Value: TSBaseCustomDBLookupComponentControl);

    procedure Rebuild;
{$ENDIF}

    procedure SetWidths(const Value: TBaseAccountPanelWidths);
    procedure SetCaptions(const Value: TCustomBaseAccountPanelCaptions);
//    procedure SetDataFieldAccountOrganizationId(const Value: String);

    function  GetListColumnsInterAccount: TcxLookupDBGridColumns;
    procedure SetListColumnsInterAccount(const Value: TcxLookupDBGridColumns);

    function  GetListColumnsAccountPart: TcxMDLookupDBTreeListColumns;
    procedure SetListColumnsAccountPart(const Value: TcxMDLookupDBTreeListColumns);

    procedure SetReadOnly(const Value: TBaseAccountPanelReadOnly);

    procedure DataFieldAccountPartIdChanged(Sender: TObject);
    function GetButtonAccountLookup: Boolean;
    function GetButtonAccountSelect: Boolean;
    function GetButtonAccountClear: Boolean;
    procedure SetButtonAccountLookup(const Value: Boolean);
    procedure SetButtonAccountSelect(const Value: Boolean);
    procedure SetButtonAccountClear(const Value: Boolean);
    function GetLookupFieldAccountPartParentId: String;
    procedure SetLookupFieldAccountPartParentId(const Value: String);

    procedure PanelAccountPartOnResize(Sender: TObject);
    procedure SetEditStyle(const Value: Boolean);
  protected
{$IFNDEF PACKAGE}
    procedure IUpdateState(AColors: TControlColors);
{$ENDIF}
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property AutoSizePercents default 0;

    property ButtonAccountLookup: Boolean read GetButtonAccountLookup write SetButtonAccountLookup default True;
    property ButtonAccountSelect: Boolean read GetButtonAccountSelect write SetButtonAccountSelect default False;
    property ButtonAccountClear:  Boolean read GetButtonAccountClear write SetButtonAccountClear default False;

    property ShowIntermediary: Boolean read GetShowIntermediary write SetShowIntermediary default False;
    property ShowAccountPart: Boolean read GetShowAccountPart write SetShowAccountPart default False;
    property ShowAccountPartDetails: Boolean read GetShowAccountPartDetails write SetShowAccountPartDetails default False;

    property DataSource: TDataSource read GetDataSource write SetDataSource;

//    property DataFieldAccountOrganizationId: String read FDataFieldAccountOrganizationId write SetDataFieldAccountOrganizationId;
    property DataFieldParent: String read FDataFieldParent write SetDataFieldParent;

    property DataFieldAccountId: String read GetDataFieldAccountId write SetDataFieldAccountId;
    property DataFieldAccountPartId: String read GetDataFieldAccountPartId write SetDataFieldAccountPartId;
    property DataFieldAccountRootId: String read FDataFieldAccountRootId write SetDataFieldAccountRootId;

    property DataFieldInterOrganizationId: String read FDataFieldInterOrganizationId write SetDataFieldInterOrganizationId;
    property DataFieldInterAccountId: String read GetDataFieldInterAccountId write SetDataFieldInterAccountId;

{$IFNDEF PACKAGE}
    property Lookup: TSBaseCustomDBLookupComponentControl read FLookup write SetLookup;
    property LookupMDDC: TMasterDetailDataSetControl read FLookupMDDC;
    property LookupMDDCInterOrganization: TMasterDetailDataSetControl read FLookupMDDCInterOrganization;
    property LookupMDDCInterAccount: TMasterDetailDataSetControl read FLookupMDDCInterAccount;
    property LookupMDDCAccountPart: TMasterDetailDataSetControl read FLookupMDDCAccountPart;
{$ENDIF}
    property LookupSource: TDataSource read GetLookupSource write SetLookupSource;
    property LookupSourceInterOrganization: TDataSource read GetLookupSourceInterOrganization write SetLookupSourceInterOrganization;
    property LookupSourceInterAccount: TDataSource read GetLookupSourceInterAccount write SetLookupSourceInterAccount;
    property LookupSourceAccountPart: TDataSource read GetLookupSourceAccountPart write SetLookupSourceAccountPart;

    property LookupParamParent: String read GetLookupParamParent write SetLookupParamParent;

    property LookupFieldAccountId: String read GetLookupFieldAccountId write SetLookupFieldAccountId;
    property LookupFieldAccountNumber: String read GetLookupFieldAccountNumber write SetLookupFieldAccountNumber;
//    property LookupFieldAccountOrganizationId: String read FLookupFieldAccountOrganizationId write SetLookupFieldAccountOrganizationId;
    property LookupFieldAccountOrganizationName: String read GetLookupFieldAccountOrganizationName write SetLookupFieldAccountOrganizationName;
    property LookupFieldAccountOwner: String read GetLookupFieldAccountOwner write SetLookupFieldAccountOwner;

    property LookupFieldAccountPartId: String read GetLookupFieldAccountPartId write SetLookupFieldAccountPartId;
    property LookupFieldAccountPartParentId: String read GetLookupFieldAccountPartParentId write SetLookupFieldAccountPartParentId;
    property LookupFieldAccountPartName: String read GetLookupFieldAccountPartName write SetLookupFieldAccountPartName;
    property LookupFieldAccountPartNumber: String read GetLookupFieldAccountPartNumber write SetLookupFieldAccountPartNumber;

    property LookupFieldAccountPartDetails: String read GetLookupFieldAccountPartDetails write SetLookupFieldAccountPartDetails;

//    property LookupParamAccountRootId: String read GetLookupFieldAccountId write SetLookupFieldAccountId;

    property LookupFieldInterOrganizationId: String read FLookupFieldInterOrganizationId write SetLookupFieldInterOrganizationId;
    property LookupFieldInterOrganizationName: String read GetLookupFieldInterOrganizationName write SetLookupFieldInterOrganizationName;
    property LookupFieldInterAccountId: String read FLookupFieldInterAccountId write SetLookupFieldInterAccountId;
    property LookupFieldInterAccountNumber: String read GetLookupFieldInterAccountNumber write SetLookupFieldInterAccountNumber;

    property ListColumnsInterAccount: TcxLookupDBGridColumns read GetListColumnsInterAccount write SetListColumnsInterAccount;
    property ListColumnsAccountPart: TcxMDLookupDBTreeListColumns read GetListColumnsAccountPart write SetListColumnsAccountPart;

    property Widths: TBaseAccountPanelWidths read FWidths write SetWidths;
    property Captions: TCustomBaseAccountPanelCaptions read FCaptions write SetCaptions;
    property ReadOnly: TBaseAccountPanelReadOnly read FReadOnly write SetReadOnly;
    property EditStyle: Boolean read FEditStyle write SetEditStyle default True;
  end;

  TBaseAccountPanel = class(TCustomBaseAccountPanel)
  published
{IFNDEF PACKAGE}
//    property TabOrderAfter;
//    property TabOrderBefore;
//    property TopAfter;
//    property TopBefore;
{ENDIF}

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
    property ParentBackground;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property TabOrder;
    property TabStop;
    property Tag;
    property Transparent default True;
    property Visible;
    property Style;
    property StyleDisabled;

    property ButtonAccountLookup;
    property ButtonAccountSelect;
    property ButtonAccountClear;

    property ShowIntermediary;
    property ShowAccountPart;
    property ShowAccountPartDetails;
    property DataSource;

    property DataFieldParent;
    property DataFieldAccountId;
    property DataFieldAccountPartId;
    property DataFieldAccountRootId;
    property DataFieldInterOrganizationId;
    property DataFieldInterAccountId;

{$IFNDEF PACKAGE}
    property Lookup;
//    property LookupInterOrganization;

    property LookupMDDC;
    property LookupMDDCInterOrganization;
    property LookupMDDCInterAccount;
    property LookupMDDCAccountPart;
{$ENDIF}
    property LookupSource;
    property LookupSourceAccountPart;
//    property LookupParamAccountId;

    property LookupParamParent;

//    property LookupFieldAccountOrganizationId;
    property LookupFieldAccountOrganizationName;
    property LookupFieldAccountOwner;
    property LookupFieldAccountId;
    property LookupFieldAccountNumber;
    property LookupFieldAccountPartDetails;

    property LookupFieldAccountPartId;
    property LookupFieldAccountPartParentId;
    property LookupFieldAccountPartName;
    property LookupFieldAccountPartNumber;

    property LookupFieldInterOrganizationId;
    property LookupFieldInterOrganizationName;

    property LookupFieldInterAccountId;
    property LookupFieldInterAccountNumber;

    property ListColumnsInterAccount;
    property ListColumnsAccountPart;

    property LookupSourceInterOrganization;
    property LookupSourceInterAccount;
    property Widths;
    property Captions;
    property ReadOnly;
    property EditStyle;
  end;

  procedure Register;

implementation

uses
  SBaseStringFunctions, SBaseVariantFunctions;

const
//  SCaption_Title        : String = 'Реальный счёт';
  SCaption_Organization : String = 'Организация';
  SCaption_Account      : String = 'Счёт';
  SCaption_AccountPart  : String = 'Раздел';
  SCaption_Intermediary : String = 'Корреспондент';
  SCaption_Details      : String = 'Детализация';

  IConst_DefaultWidth         = 500;
  IConst_DefaultHeight        = 105;
  IConst_DefaultCaptionWidth  = 85;
  IConst_DefaultLayoutHeight  = 23;
  IConst_DefaultAccountWidth  = 140;
  IConst_DefaultPaddingRight  = 0;
  IConst_DefaultMarginLeft    = 5;

  IConst_FirstCaptionLeft     = 5;
  IConst_SecondCaptionLeft    = 10;

procedure Register;
begin
  RegisterComponents ('BackOffice', [TBaseAccountPanel]);
end;

{ TCustomBaseAccountPanel }

constructor TCustomBaseAccountPanel.Create(AOwner: TComponent);
begin
  inherited;

//  ControlStyle := ControlStyle - [csSetCaption];

  AutoSizePercents := 0;
  Transparent := True;
  Width   := IConst_DefaultWidth;
  Height  := IConst_DefaultHeight;

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

    Caption := '';
//    Parent  := Self;
    Top     := 0;
    Align   := alTop;
  end;

  { Фрейм "Организация" }
  FlpOrganization := TCustomLayoutPanel.Create(Self);
  with FlpOrganization do begin
    Caption.Text  := SCaption_Organization;
    Caption.Left  := IConst_FirstCaptionLeft;
    Caption.Width := IConst_DefaultCaptionWidth;
    ParentFont    := False;
    Caption.Font.Style := [];
    Padding.Right := IConst_DefaultPaddingRight;
    TabOrder      := 0;

    Top    := 30;
    Parent := Self;
//    Height := IConst_DefaultLayoutHeight;
    Align  := alTop;
  end;

  { Компонента "Организация" }
  FlcbAccountOrganizationName := TcxDBLookupComboBox.Create(Self);
  with FlcbAccountOrganizationName do begin
    ParentFont    := False;
    with Properties do begin
      AutoSelect  := False;
{$IFNDEF PACKAGE}
      Properties.ClearKey := ShortCut_Del;
{$ENDIF}
    end;

    Parent  := FlpOrganization;
    Align   := alClient;
  end;

  { Фрейм "Счёт" }
  FlpAccount := TCustomLayoutPanel.Create(Self);
  with FlpAccount do begin
    Caption.Text  := SCaption_Account;
    Caption.Left  := IConst_FirstCaptionLeft;
    Caption.Width := IConst_DefaultCaptionWidth;
    Caption.Font.Style := [];
    TabStop       := False;
//    TabOrder      := 1;

    Top           := 60;
    Align         := alTop;
    Parent        := Self;
  end;

  { Компонента "Счёт :: Номер" (2) }
  FlcbAccountNumber := TcxDBLookupComboBox.Create(Self);
  with FlcbAccountNumber do begin
    ParentFont          := False;
    Style.Color         := clBtnFace;
    with Properties do begin
      AutoSelect  := False;
      ReadOnly    := True;
      Buttons[0].Visible := False;
    end;
    TabStop             := False;

    Width   := IConst_DefaultAccountWidth;
    Align   := alRight;
    Margins.SetBounds(IConst_DefaultMarginLeft, 0, 0, 0);
    AlignWithMargins := True;
    Parent  := FlpAccount;
  end;

  { Компонента "Счёт :: Владелец" (1) }
  FlcbAccountOwner := TcxDBLookupComboBox.Create(Self);
  with FlcbAccountOwner do begin
    ParentFont          := False;
    Style.Color         := clBtnFace;
    with Properties do begin
      AutoSelect  := False;
      ReadOnly    := True;
      Buttons[0].Visible := False;
    end;
    TabStop             := False;

    Parent  := FlpAccount;
    Align   := alClient;
  end;

  { Фрейм "Корреспондент" }
  FlpInter := TCustomLayoutPanel.Create(Self);
  with FlpInter do begin
    Caption.Text  := SCaption_Intermediary;
    Caption.Left  := IConst_FirstCaptionLeft;
    Caption.Width := IConst_DefaultCaptionWidth;
    ParentFont    := False;
    Caption.Font.Style := [];
    Padding.Right := IConst_DefaultPaddingRight;
    TabOrder      := 1;

    Top    := 60;
    Align  := alTop;
  end;

  { Компонента "Корреспондент :: Счёт"}
  FlcbInterAccountNumber := TcxDBLookupComboBox.Create(Self);
  with FlcbInterAccountNumber do begin
    ParentFont := False;
    Style.Color := clBtnFace;
    with Properties do begin
      AutoSelect    := False;
      ImmediatePost := True;
      DropDownRows  := 15;
      ReadOnly      := True;

      Buttons[0].Visible := False;
      ListOptions.ShowHeader := False;
{$IFNDEF PACKAGE}
      ClearKey := ShortCut_Del;
{$ENDIF}
    end;

    Width       := IConst_DefaultAccountWidth;
    Margins.SetBounds(IConst_DefaultMarginLeft, 0, 0, 0);
    AlignWithMargins := True;
    Align       := alRight;
    Parent      := FlpInter;
  end;

  { Компонента "Корреспондент :: Организация"}
  FlcbInterOrganizationName := TcxDBLookupComboBox.Create(Self);
  with FlcbInterOrganizationName do begin
    ParentFont          := False;
    with Properties do begin
      AutoSelect    := False;
      ImmediatePost := True;
      DropDownRows  := 15;
      ReadOnly      := True;

      Buttons[0].Visible := False;
      ListOptions.ShowHeader := False;
{$IFNDEF PACKAGE}
      ClearKey := ShortCut_Del;
{$ENDIF}
    end;
    Style.Color         := clBtnFace;
    TabStop             := False;
    Align               := alClient;
    Parent              := FlpInter;
  end;

//  FlpInterAccount.TabOrder := 1;

  { Фрейм "Раздел счета" }
  FlpAccountPart := TCustomLayoutPanel.Create(Self);
  with FlpAccountPart do begin
    Caption.Text  := SCaption_AccountPart;
    Caption.Left  := IConst_FirstCaptionLeft;
    Caption.Left  := Caption.Left;
    Caption.Width := IConst_DefaultCaptionWidth;
    ParentFont := False;
    Caption.Font.Style := [];
    Padding.Right := IConst_DefaultPaddingRight;

    Top    := 90;
//    Height := IConst_DefaultLayoutHeight;
    Align  := alTop;

    OnResize := PanelAccountPartOnResize;
  end;

  { Компонента "Раздел счета :: Номер" }
  FlcbAccountPartNumber := TcxDBLookupComboBox.Create(Self);
  with FlcbAccountPartNumber do begin
    ParentFont    := False;
    Style.Color   := clBtnFace;
    with Properties do begin
      AutoSelect    := False;
      ReadOnly      := True;
      Buttons[0].Visible := False;
    end;
    TabStop       := False;

    Width             := IConst_DefaultAccountWidth;
    Margins.SetBounds(IConst_DefaultMarginLeft, 0, 0, 0);
    AlignWithMargins  := True;

//    Parent  := FlpAccountPart;
    Align   := alRight;
  end;

  { Компонента "Раздел счета :: Наименование" }
  FltcbAccountPartName := TcxMDDBLookupTreeComboBox.Create(Self);
  with FltcbAccountPartName do begin
    ParentFont    := False;
    with Properties do begin
      TreeViewShowHeaders := True;
      ImmediatePost := True;
{$IFNDEF PACKAGE}
      ClearKey := ShortCut_Del;
{$ENDIF}
      TreeViewAlwaysExpandedLevel := -1;
      IncrementalSearchContainMode := True;
    end;

    Parent  := FlpAccountPart;
    Align   := alClient;
  end;

  { Фрейм "Детализация" }
  FlpAccountPartDetails := TCustomLayoutPanel.Create(Self);
  with FlpAccountPartDetails do begin
    Caption.Text  := SCaption_Details;
    Caption.Left  := IConst_FirstCaptionLeft;
    Caption.Width := IConst_DefaultCaptionWidth;
    ParentFont := False;
    Caption.Font.Style := [];
    Padding.Right := IConst_DefaultPaddingRight;
    TabStop       := False;
//    TabOrder      := 0;

    Top    := 120;
//    Height := IConst_DefaultLayoutHeight;
    Align  := alTop;
  end;

  { Компонента "Детализация" }
  FlcbAccountPartDetails := TcxDBLookupComboBox.Create(Self);
  with FlcbAccountPartDetails do begin
    ParentFont    := False;
    Style.Color   := clBtnFace;
    with Properties do begin
      AutoSelect    := False;
      ReadOnly      := True;
      Buttons[0].Visible := False;
    end;
    TabStop       := False;

    Parent  := FlpAccountPartDetails;
    Align   := alClient;
  end;

{$IFNDEF PACKAGE}
  FLookup := TSBaseCustomDBLookupComponentControl.Create(Self);
  FLookup.Control := FlcbAccountOrganizationName;

  FLookupMDDC := TMasterDetailDataSetControl.Create(Self);
//  FLookupMDDC.DataHelpers := [mdfhClearIfNotInList, mdfhSetWhenOneRecord];

  FLookupMDDCInterOrganization := TMasterDetailDataSetControl.Create(Self);
  FLookupMDDCInterOrganization.Method := mdfmParams;
  FLookupMDDCInterOrganization.DataHelpers := [mdfhClearIfNotInList, mdfhSetWhenOneRecord];

  FLookupMDDCInterAccount := TMasterDetailDataSetControl.Create(Self);
  FLookupMDDCInterAccount.Method := mdfmParams;
  FLookupMDDCInterAccount.DataHelpers := [mdfhClearIfNotInList, mdfhSetWhenOneRecord];

  FLookupMDDCAccountPart := TMasterDetailDataSetControl.Create(Self);
  FLookupMDDCAccountPart.Method := mdfmParams;
  FLookupMDDCAccountPart.DataHelpers := [mdfhClearIfNotInList, mdfhSetWhenOneRecord];
{$ELSE}
  with FlcbAccountOrganizationName.Properties.Buttons.Add do begin
    Visible := False;
    Kind    := bkEllipsis;
  end;
{$ENDIF}

  FWidths   := TBaseAccountPanelWidths.Create(Self);
  FCaptions := TCustomBaseAccountPanelCaptions.Create(Self);
  FReadOnly := TBaseAccountPanelReadOnly.Create(Self);
  FEditStyle:= True;
end;

procedure TCustomBaseAccountPanel.DataFieldAccountPartIdChanged(Sender: TObject);
var
  LDataSet: TDataSet;
  LValue: Variant;
begin
  if Assigned(DataSource) then
    LDataSet := DataSource.DataSet
  else
    LDataSet := nil;

  if (FDataFieldAccountRootId <> '') and Assigned(LDataSet) and (LDataSet.State in [dsInsert, dsEdit]) then begin
    LValue := IsNull(LDataSet.FieldByName(DataFieldAccountPartId).AsVariant, LDataSet.FieldByName(DataFieldAccountId).AsVariant);
    with LDataSet.FieldByName(FDataFieldAccountRootId) do
      if not VarIsEqual(LValue, AsVariant) then
        AsVariant := LValue;
  end;
end;

destructor TCustomBaseAccountPanel.Destroy;
begin
  inherited;
  FreeAndNil(FWidths);
  FreeAndNil(FCaptions);
  FreeAndNil(FReadOnly);
{IFNDEF PACKAGE}
{ENDIF}
end;

function TCustomBaseAccountPanel.GetDataSource: TDataSource;
begin
  Result := FlcbAccountOrganizationName.DataBinding.DataSource
end;

procedure TCustomBaseAccountPanel.SetDataSource(const Value: TDataSource);
begin
{$IFNDEF PACKAGE}
  FLookup.DataSource := Value;
//  FLookupInterOrganization.DataSource := Value;

  FLookupMDDC.DataSource := Value;
  FLookupMDDC.MasterSource := Value;

  FLookupMDDCInterOrganization.DataSource := Value;
  FLookupMDDCInterOrganization.MasterSource := Value;

  FLookupMDDCInterAccount.DataSource := Value;
  FLookupMDDCInterAccount.MasterSource := Value;

  FLookupMDDCAccountPart.DataSource := Value;
  FLookupMDDCAccountPart.MasterSource := Value;
{$ENDIF}

  FlcbAccountOrganizationName.DataBinding.DataSource := Value;
  FlcbAccountOwner.DataBinding.DataSource := Value;
  FlcbAccountNumber.DataBinding.DataSource := Value;

  FlcbInterOrganizationName.DataBinding.DataSource := Value;
  FlcbInterAccountNumber.DataBinding.DataSource := Value;

  FltcbAccountPartName.DataBinding.DataSource := Value;
  FlcbAccountPartNumber.DataBinding.DataSource := Value;
  FlcbAccountPartDetails.DataBinding.DataSource := Value;
end;

procedure TCustomBaseAccountPanel.SetEditStyle(const Value: Boolean);
begin
  if FEditStyle <> Value then begin
    FEditStyle := Value;
{$IFNDEF PACKAGE}
    ReBuild;
{$ENDIF}
  end;
end;

procedure TCustomBaseAccountPanel.SetCaptions(const Value: TCustomBaseAccountPanelCaptions);
begin
  FCaptions.Assign(Value);
end;

function TCustomBaseAccountPanel.GetButtonAccountClear: Boolean;
begin
  Result := {$IFNDEF PACKAGE}(FLookup.ClearButtonIndex = 2) and {$ENDIF}FlcbAccountOrganizationName.Properties.Buttons[2].Visible
end;

function TCustomBaseAccountPanel.GetButtonAccountLookup: Boolean;
begin
  Result := FlcbAccountOrganizationName.Properties.Buttons[0].Visible
end;

procedure TCustomBaseAccountPanel.SetButtonAccountClear(const Value: Boolean);
{$IFNDEF PACKAGE}
var
  LSelectButton : Boolean;
{$ENDIF}
begin
{$IFNDEF PACKAGE}
  if Value then begin
    LSelectButton := ButtonAccountSelect;
    FLookup.ClearButtonIndex := 2;
    {TODO: Исправить}
    if not LSelectButton then
      FlcbAccountOrganizationName.Properties.Buttons[1].Visible := False;
  end else
    FLookup.ClearButtonIndex := -1
{$ELSE}
  FlcbAccountOrganizationName.Properties.Buttons[2].Visible := Value;
{$ENDIF}
end;

procedure TCustomBaseAccountPanel.SetButtonAccountLookup(const Value: Boolean);
begin
  FlcbAccountOrganizationName.Properties.Buttons[0].Visible := Value;
  FlcbAccountOrganizationName.Properties.ReadOnly := not Value;
end;

function TCustomBaseAccountPanel.GetButtonAccountSelect: Boolean;
begin
  Result := {$IFNDEF PACKAGE}(FLookup.SelectButtonIndex = 1) and {$ENDIF}FlcbAccountOrganizationName.Properties.Buttons[1].Visible
end;

procedure TCustomBaseAccountPanel.SetButtonAccountSelect(const Value: Boolean);
begin
{$IFNDEF PACKAGE}
  if Value then
    FLookup.SelectButtonIndex := 1
  else
    FLookup.SelectButtonIndex := -1
{$ELSE}
  FlcbAccountOrganizationName.Properties.Buttons[1].Visible := Value;
{$ENDIF}
end;

function TCustomBaseAccountPanel.GetDataFieldAccountId: String;
begin
  Result := FlcbAccountOrganizationName.DataBinding.DataField
end;

procedure TCustomBaseAccountPanel.SetDataFieldAccountId(const Value: String);
begin
  if DataFieldAccountId <> Value then begin
    FlcbAccountOrganizationName.DataBinding.DataField := Value;
    FlcbAccountOwner.DataBinding.DataField := Value;
    FlcbAccountNumber.DataBinding.DataField := Value;

{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
  end;
end;

function TCustomBaseAccountPanel.GetDataFieldAccountPartId: String;
begin
  Result := FltcbAccountPartName.DataBinding.DataField
end;

procedure TCustomBaseAccountPanel.SetDataFieldAccountPartId(const Value: String);
begin
  FltcbAccountPartName.DataBinding.DataField := Value;
  FlcbAccountPartNumber.DataBinding.DataField := Value;
  FlcbAccountPartDetails.DataBinding.DataField := Value;
{$IFNDEF PACKAGE}
  FLookupMDDCAccountPart.DataField := Value;
{$ENDIF}
end;

procedure TCustomBaseAccountPanel.SetDataFieldAccountRootId(const Value: String);
begin
  FDataFieldAccountRootId := Value;
  if FDataFieldAccountRootId <> '' then begin
    FltcbAccountPartName.DataBinding.OnFieldChanged := DataFieldAccountPartIdChanged;
{$IFNDEF PACKAGE}
    FLookupMDDCAccountPart.OnMasterDataChanged := DataFieldAccountPartIdChanged;
{$ENDIF}
  end else begin
    FltcbAccountPartName.DataBinding.OnFieldChanged := nil;
{$IFNDEF PACKAGE}
    FLookupMDDCAccountPart.OnMasterDataChanged := nil;
{$ENDIF}
  end;
end;

procedure TCustomBaseAccountPanel.SetDataFieldInterOrganizationId(const Value: String);
begin
  if FDataFieldInterOrganizationId <> Value then begin
    FDataFieldInterOrganizationId := Value;
{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
    if LookupSourceInterOrganization <> nil then
      FlcbInterOrganizationName.DataBinding.DataField := Value
    else
      FlcbInterOrganizationName.DataBinding.DataField := DataFieldAccountId
  end;
end;

procedure TCustomBaseAccountPanel.SetDataFieldParent(const Value: String);
begin
  if DataFieldParent <> Value then begin
    FDataFieldParent := Value;
{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
  end;
end;

function TCustomBaseAccountPanel.GetLookupParamParent: String;
begin
{$IFNDEF PACKAGE}
  Result := FLookupMDDC.DetailFields
{$ELSE}
  Result := FLookupParamParent
{$ENDIF}
end;

procedure TCustomBaseAccountPanel.SetLookupParamParent(const Value: String);
begin
  if LookupParamParent <> Value then begin
{$IFNDEF PACKAGE}
    FLookupMDDC.DetailFields := Value;
    Rebuild;
{$ELSE}
    FLookupParamParent := Value;
{$ENDIF}
  end;
end;

{
procedure TCustomBaseAccountPanel.SetDataFieldAccountOrganizationId(const Value: String);
begin
  if FDataFieldAccountOrganizationId <> Value then begin
    FDataFieldAccountOrganizationId := Value;
    UpdateMDDCInterAccountParams;
  end;
end;
}

function TCustomBaseAccountPanel.GetDataFieldInterAccountId: String;
begin
  Result := FDataFieldInterAccountId
end;

procedure TCustomBaseAccountPanel.SetDataFieldInterAccountId(const Value: String);
begin
  if FDataFieldInterAccountId <> Value then begin
    FDataFieldInterAccountId := Value;
    FlcbInterAccountNumber.DataBinding.DataField := Value;
{$IFNDEF PACKAGE}
    FLookupMDDCInterAccount.DataField := Value;
{$ENDIF}
  end;
end;

{$IFNDEF PACKAGE}
procedure TCustomBaseAccountPanel.SetLookup(const Value: TSBaseCustomDBLookupComponentControl);
begin
  FLookup.Assign(Value);
end;
{$ENDIF}

function TCustomBaseAccountPanel.GetLookupFieldAccountId: String;
begin
  Result := FlcbAccountNumber.Properties.KeyFieldNames
end;

procedure TCustomBaseAccountPanel.SetLookupFieldAccountId(const Value: String);
begin
  FlcbAccountNumber.Properties.KeyFieldNames := Value;
  FlcbAccountOrganizationName.Properties.KeyFieldNames := Value;
  FlcbAccountOwner.Properties.KeyFieldNames := Value;
  FlcbAccountNumber.Properties.KeyFieldNames := Value;

{$IFNDEF PACKAGE}
  Rebuild;
{$ENDIF}
end;

function TCustomBaseAccountPanel.GetLookupFieldAccountNumber: String;
begin
  Result := FlcbAccountNumber.Properties.ListFieldNames
end;

procedure TCustomBaseAccountPanel.SetLookupFieldAccountNumber(const Value: String);
begin
  FlcbAccountNumber.Properties.ListFieldNames := Value;
end;

function TCustomBaseAccountPanel.GetLookupFieldAccountPartName: String;
begin
  Result := FltcbAccountPartName.Properties.ListField
end;

procedure TCustomBaseAccountPanel.SetLookupFieldAccountPartName(const Value: String);
begin
  FltcbAccountPartName.Properties.ListField := Value
end;

function TCustomBaseAccountPanel.GetLookupFieldAccountPartNumber: String;
begin
  Result := FlcbAccountPartNumber.Properties.ListFieldNames
end;

procedure TCustomBaseAccountPanel.SetLookupFieldAccountPartNumber(const Value: String);
begin
  FlcbAccountPartNumber.Properties.ListFieldNames := Value;
  if Value.IsEmpty then
    FlcbAccountPartNumber.Parent := nil
  else
    FlcbAccountPartNumber.Parent := FlpAccountPart
end;

function TCustomBaseAccountPanel.GetLookupFieldAccountPartParentId: String;
begin
  Result := FltcbAccountPartName.Properties.ParentField
end;

procedure TCustomBaseAccountPanel.SetLookupFieldAccountPartParentId(const Value: String);
begin
  FltcbAccountPartName.Properties.ParentField := Value
end;

function TCustomBaseAccountPanel.GetLookupFieldAccountPartId: String;
begin
  Result := FltcbAccountPartName.Properties.KeyField
end;

procedure TCustomBaseAccountPanel.SetLookupFieldAccountPartId(const Value: String);
begin
  FltcbAccountPartName.Properties.KeyField := Value;
  FlcbAccountPartNumber.Properties.KeyFieldNames := Value;
  FlcbAccountPartDetails.Properties.KeyFieldNames := Value;
{$IFNDEF PACKAGE}
  FLookupMDDCAccountPart.ListField := Value;
{$ENDIF}
end;

function TCustomBaseAccountPanel.GetLookupFieldAccountPartDetails: String;
begin
  Result := FlcbAccountPartDetails.Properties.ListFieldNames
end;

procedure TCustomBaseAccountPanel.SetLookupFieldAccountPartDetails(const Value: String);
begin
  FlcbAccountPartDetails.Properties.ListFieldNames := Value
end;

function TCustomBaseAccountPanel.GetLookupFieldAccountOrganizationName: String;
begin
  Result := FlcbAccountOrganizationName.Properties.ListFieldNames;
end;

{
procedure TCustomBaseAccountPanel.SetLookupFieldAccountOrganizationId(const Value: String);
begin
  if FLookupFieldAccountOrganizationId <> Value then begin
    FLookupFieldAccountOrganizationId := Value;
  end;
end;
}

procedure TCustomBaseAccountPanel.SetLookupFieldInterOrganizationId(const Value: String);
begin
  if FLookupFieldInterOrganizationId <> Value then begin
    FLookupFieldInterOrganizationId := Value;
{$IFNDEF PACKAGE}
    Rebuild;
{$ENDIF}
  end;
end;

procedure TCustomBaseAccountPanel.SetLookupFieldInterAccountId(const Value: String);
begin
  if FLookupFieldInterAccountId <> Value then begin
    FLookupFieldInterAccountId := Value;
{$IFNDEF PACKAGE}
    FLookupMDDCInterAccount.ListField := Value;
{$ENDIF}
  end;
end;

procedure TCustomBaseAccountPanel.SetLookupFieldAccountOrganizationName(const Value: String);
begin
  FlcbAccountOrganizationName.Properties.ListFieldNames := Value;
end;

function TCustomBaseAccountPanel.GetLookupFieldAccountOwner: String;
begin
  Result := FlcbAccountOwner.Properties.ListFieldNames
end;

procedure TCustomBaseAccountPanel.SetLookupFieldAccountOwner(const Value: String);
begin
  FlcbAccountOwner.Properties.ListFieldNames := Value;
end;

function TCustomBaseAccountPanel.GetLookupFieldInterAccountNumber: String;
begin
  Result := FlcbInterAccountNumber.Properties.ListFieldNames
end;

procedure TCustomBaseAccountPanel.SetLookupFieldInterAccountNumber(const Value: String);
begin
  FlcbInterAccountNumber.Properties.ListFieldNames := Value;
end;

function TCustomBaseAccountPanel.GetListColumnsInterAccount: TcxLookupDBGridColumns;
begin
  Result := FlcbInterAccountNumber.Properties.ListColumns
end;

procedure TCustomBaseAccountPanel.SetListColumnsInterAccount(const Value: TcxLookupDBGridColumns);
begin
  FlcbInterAccountNumber.Properties.ListColumns := Value;
end;

function TCustomBaseAccountPanel.GetListColumnsAccountPart: TcxMDLookupDBTreeListColumns;
begin
  Result := FltcbAccountPartName.Properties.ListColumns
end;

procedure TCustomBaseAccountPanel.SetListColumnsAccountPart(const Value: TcxMDLookupDBTreeListColumns);
begin
  FltcbAccountPartName.Properties.ListColumns := Value
end;

function TCustomBaseAccountPanel.GetLookupFieldInterOrganizationName: String;
begin
  Result := FlcbInterOrganizationName.Properties.ListFieldNames
end;

procedure TCustomBaseAccountPanel.SetLookupFieldInterOrganizationName(const Value: String);
begin
  FlcbInterOrganizationName.Properties.ListFieldNames := Value
end;

function TCustomBaseAccountPanel.GetLookupSource: TDataSource;
begin
  Result := FlcbAccountOrganizationName.Properties.ListSource
end;

procedure TCustomBaseAccountPanel.SetLookupSource(const Value: TDataSource);
begin
  if LookupSource <> Value then begin
    FlcbAccountOrganizationName.Properties.ListSource := Value;
    FlcbAccountOwner.Properties.ListSource := Value;
    FlcbAccountNumber.Properties.ListSource := Value;
{$IFNDEF PACKAGE}
    FLookupMDDC.DetailSource := Value;
    Rebuild;
{$ENDIF}
  end;
end;

function TCustomBaseAccountPanel.GetLookupSourceAccountPart: TDataSource;
begin
  Result := FltcbAccountPartName.Properties.ListSource;
end;

procedure TCustomBaseAccountPanel.SetLookupSourceAccountPart(const Value: TDataSource);
begin
  FltcbAccountPartName.Properties.ListSource := Value;
  FlcbAccountPartNumber.Properties.ListSource := Value;
  FlcbAccountPartDetails.Properties.ListSource := Value;
{$IFNDEF PACKAGE}
  FLookupMDDCAccountPart.DetailSource := Value;
{$ENDIF}
end;

procedure TCustomBaseAccountPanel.SetReadOnly(const Value: TBaseAccountPanelReadOnly);
begin
  FReadOnly.Assign(Value);
end;

function TCustomBaseAccountPanel.GetLookupSourceInterAccount: TDataSource;
begin
{$IFNDEF PACKAGE}
  Result := FLookupMDDCInterAccount.DetailSource
{$ELSE}
  Result := FLookupSourceInterAccount
{$ENDIF}
end;

procedure TCustomBaseAccountPanel.SetLookupSourceInterAccount(const Value: TDataSource);
begin
  if LookupSourceInterAccount <> Value then begin
{$IFNDEF PACKAGE}
    FLookupMDDCInterAccount.DetailSource := Value;
    Rebuild;
{$ELSE}
    FLookupSourceInterAccount := Value
{$ENDIF}
  end;
end;

function TCustomBaseAccountPanel.GetLookupSourceInterOrganization: TDataSource;
begin
{$IFNDEF PACKAGE}
  Result := FLookupMDDCInterOrganization.DetailSource
{$ELSE}
  Result := FLookupSourceInterOrganization
{$ENDIF}
end;

procedure TCustomBaseAccountPanel.SetLookupSourceInterOrganization(const Value: TDataSource);
begin
  if LookupSourceInterOrganization <> Value then begin
{$IFNDEF PACKAGE}
    FLookupMDDCInterOrganization.DetailSource := Value;
    Rebuild;
{$ELSE}
    FLookupSourceInterOrganization := Value
{$ENDIF}
  end;
end;

function TCustomBaseAccountPanel.GetShowAccountPartDetails: Boolean;
begin
  Result := FlpAccountPartDetails.Parent <> nil;
end;

procedure TCustomBaseAccountPanel.SetShowAccountPartDetails(const Value: Boolean);
begin
  if (FlpAccountPartDetails.Parent <> nil) <> Value then begin
    if Value then
      FlpAccountPartDetails.Parent := Self
    else
      FlpAccountPartDetails.Parent := nil;

    ReAlignPanels;
  end;
end;

function TCustomBaseAccountPanel.GetShowAccountPart: Boolean;
begin
  Result := FlpAccountPart.Parent <> nil;
end;

procedure TCustomBaseAccountPanel.SetShowAccountPart(const Value: Boolean);
begin
  if (FlpAccountPart.Parent <> nil) <> Value then begin
    if Value then
      FlpAccountPart.Parent := Self
    else
      FlpAccountPart.Parent := nil;

    ReAlignPanels;
  end;
end;

function TCustomBaseAccountPanel.GetShowIntermediary: Boolean;
begin
  Result := FlpInter.Parent <> nil;
end;

procedure TCustomBaseAccountPanel.SetShowIntermediary(const Value: Boolean);
begin
  if (FlpInter.Parent <> nil) <> Value then begin
    if Value then
      FlpInter.Parent := Self
    else
      FlpInter.Parent := nil;

    ReAlignPanels;
  end;
end;

procedure TCustomBaseAccountPanel.SetWidths(const Value: TBaseAccountPanelWidths);
begin
  FWidths.Assign(Value);
end;

{$IFNDEF PACKAGE}
procedure TCustomBaseAccountPanel.IUpdateState(AColors: TControlColors);
begin
  if (not FReadOnly.Account) and (FLookup.SelectButtonIndex <> -1) and FLookup.CanModify then
    FlcbAccountOrganizationName.Style.Color := AColors.WriteAble.Color
  else
    FlcbAccountOrganizationName.Style.Color := AColors.ReadOnly.Color;

  if (not FReadOnly.InterOrganization)
      and Assigned(LookupSourceInterOrganization)
      and FlcbInterOrganizationName.DataBinding.DataLink.CanModify
      and FlcbInterOrganizationName.DataBinding.DataSource.GetInternalCanModify
  then
    FlcbInterOrganizationName.Style.Color := AColors.WriteAble.Color
  else
    FlcbInterOrganizationName.Style.Color := AColors.ReadOnly.Color;

  if (not FReadOnly.InterAccount)
      and Assigned(LookupSourceInterAccount)
      and FlcbInterAccountNumber.DataBinding.DataLink.CanModify
      and FlcbInterAccountNumber.DataBinding.DataSource.GetInternalCanModify
  then begin
    FlcbInterAccountNumber.Style.Color := AColors.WriteAble.Color;
    FlcbInterAccountNumber.Properties.ImmediateDropDownWhenActivated := True;
  end else begin
    FlcbInterAccountNumber.Style.Color := AColors.ReadOnly.Color;
    FlcbInterAccountNumber.Properties.ImmediateDropDownWhenActivated := False;
  end;

  if (not FReadOnly.AccountPart)
      and Assigned(FltcbAccountPartName.DataBinding.Field)
      and FltcbAccountPartName.DataBinding.Field.CanModify
      and FltcbAccountPartName.DataBinding.DataSource.GetInternalCanModify
  then begin
    FltcbAccountPartName.Style.Color := AColors.WriteAble.Color;
    FltcbAccountPartName.Properties.ImmediateDropDownWhenActivated := True;
  end else begin
    FltcbAccountPartName.Style.Color := AColors.ReadOnly.Color;
    FltcbAccountPartName.Properties.ImmediateDropDownWhenActivated := False;
  end;
end;

{$ENDIF}

procedure TCustomBaseAccountPanel.Loaded;
begin
  inherited;

  if FReAlignPanelsRequired then ReAlignPanels;
{$IFNDEF PACKAGE}
  if FReBuildRequired then Rebuild;
{$ENDIF}
end;

procedure TCustomBaseAccountPanel.PanelAccountPartOnResize(Sender: TObject);
begin
  FltcbAccountPartName.Properties.DropDownWidth := FlpAccountPart.Width
        - FlpAccountPart.Padding.Right
        - FlpAccountPart.Caption.Width
        - FlpAccountPart.Caption.Left
end;

procedure TCustomBaseAccountPanel.ReAlignPanels;
var
  LTop      : Integer;
  LTabOrder : Integer;

  procedure _AlignPanel(APanel: TCustomLayoutPanel);
  begin
    APanel.TabOrder := LTabOrder;
    Inc(LTabOrder);
    APanel.Top := LTop;
    LTop := APanel.Top + APanel.Height + 1;
  end;

begin
{$IFDEF PACKAGE}
  if ([csLoading, csDesigning] * ComponentState) = [csLoading] then
{$ELSE}
  if csLoading in ComponentState then
{$ENDIF}
  begin
    FReAlignPanelsRequired := True;
    Exit;
  end;
  FReAlignPanelsRequired := False;

  LTop := 0;
  LTabOrder := 0;

  if Assigned(FCaptionLabel.Parent) then begin
    FCaptionLabel.Top := 0;
    LTop := FCaptionLabel.Top + FCaptionLabel.Height + 1;
  end;

  _AlignPanel(FlpOrganization);
  _AlignPanel(FlpAccount);

  if ShowIntermediary then _AlignPanel(FlpInter);
  if ShowAccountPart then _AlignPanel(FlpAccountPart);
  if ShowAccountPartDetails then _AlignPanel(FlpAccountPartDetails);
end;

{$IFNDEF PACKAGE}
procedure TCustomBaseAccountPanel.Rebuild;
var
  LConfigAccount: Boolean;
  LConfigAccountPart: Boolean;
  LConfigInterOrganization: Boolean;
  LConfigInterAccount: Boolean;

  LDataFieldAccountId, LLookupFieldAccountId: String;
  LDataFieldAccountPartId, LLookupFieldAccountPartId: String;
  LDataFieldInterOrganizationId, LLookupFieldInterOrganizationId: String;
  LDataFieldInterAccountId, LLookupFieldInterAccountId: String;
begin
  if csLoading in ComponentState then begin
    FReBuildRequired := True;
    Exit;
  end;
  FReBuildRequired := False;

  // Account
  LDataFieldAccountId   := DataFieldAccountId;
  LLookupFieldAccountId := LookupFieldAccountId;

  LConfigAccount := (DataFieldParent.IsEmpty = LookupParamParent.IsEmpty) and (not LDataFieldAccountId.IsEmpty) and (not LLookupFieldAccountId.IsEmpty);
  FLookupMDDC.Enabled := False;

  LDataFieldInterOrganizationId   := DataFieldInterOrganizationId;
  LLookupFieldInterOrganizationId := LookupFieldInterOrganizationId;

  LDataFieldInterAccountId   := DataFieldInterAccountId;
  LLookupFieldInterAccountId := LookupFieldInterAccountId;

  if (not LDataFieldAccountId.IsEmpty) and (not LLookupFieldAccountId.IsEmpty) then
    if (not LDataFieldInterOrganizationId.IsEmpty) and (not LLookupFieldInterOrganizationId.IsEmpty)
        and (not LDataFieldInterAccountId.IsEmpty) and (not LLookupFieldInterAccountId.IsEmpty)
    then begin
      FLookup.DataField := LDataFieldAccountId + ';' + LDataFieldInterOrganizationId + ';' + LDataFieldInterAccountId;
      FLookup.ParamName := LLookupFieldAccountId + ';' + LLookupFieldInterOrganizationId + ';' + LLookupFieldInterAccountId;
    end else begin
      FLookup.DataField := LDataFieldAccountId;
      FLookup.ParamName := LLookupFieldAccountId;
    end
  else begin
    FLookup.DataField := '';
    FLookup.ParamName := '';
  end;

  if LConfigAccount then begin
    if EditStyle and (not DataFieldParent.IsEmpty) then begin
      FLookupMDDC.Condition     := QuoteString(DataFieldParent, '[') + SCondition_IS_NOT_NULL + ' ' + SConst_OR + ' ' + LDataFieldAccountId + SCondition_IS_NOT_NULL;
      FLookupMDDC.MasterFields  := DataFieldParent;
      FLookupMDDC.DetailFields  := LookupParamParent;
      FLookupMDDC.ListField     := LookupParamParent;
      FLookupMDDC.Method        := mdfmParamsEx;
    end else begin
      FLookupMDDC.Condition     := QuoteString(LDataFieldAccountId, '[') + SCondition_IS_NOT_NULL;
      FLookupMDDC.MasterFields  := LDataFieldAccountId;
      FLookupMDDC.DetailFields  := LLookupFieldAccountId;
      FLookupMDDC.Method        := mdfmParams;
    end;
    FLookupMDDC.Enabled := True;
  end;
//  else begin
//    FLookupMDDC.Enabled       := False
//    FLookupMDDC.Condition     := '';
//    FLookupMDDC.MasterFields  := '';
//    FLookupMDDC.DetailFields  := '';
//  end;

  // AccountPart

  LDataFieldAccountPartId   := DataFieldAccountPartId;
  LLookupFieldAccountPartId := LookupFieldAccountPartId;
  LConfigAccountPart := (not LDataFieldAccountPartId.IsEmpty) and (not LLookupFieldAccountPartId.IsEmpty);
  if ShowAccountPart and LConfigAccountPart and (not EditStyle) then begin
    FLookupMDDCAccountPart.MasterFields := LDataFieldAccountPartId;
    FLookupMDDCAccountPart.DetailFields := LLookupFieldAccountPartId;
    FLookupMDDCAccountPart.Condition := QuoteString(LDataFieldAccountId, '[') + SCondition_IS_NOT_NULL;
    FLookupMDDCAccountPart.Enabled      := True;
  end else if ShowAccountPart and LConfigAccountPart then begin
    FLookupMDDCAccountPart.MasterFields := LDataFieldAccountId;
    FLookupMDDCAccountPart.DetailFields := LLookupFieldAccountId;
    FLookupMDDCAccountPart.Condition := QuoteString(LDataFieldAccountId, '[') + SCondition_IS_NOT_NULL;
    FLookupMDDCAccountPart.Enabled      := True;
  end else begin
    FLookupMDDCAccountPart.Enabled      := False;
//    FLookupMDDCAccountPart.MasterFields := '';
//    FLookupMDDCAccountPart.DetailFields := '';
  end;

  // InterOrganization
  if Assigned(LookupSourceInterOrganization) and (not FReadOnly.InterOrganization) then
    with FlcbInterOrganizationName.Properties do begin
      Buttons[0].Visible := True;
      ReadOnly := False
    end
  else
    with FlcbInterOrganizationName.Properties do begin
      Buttons[0].Visible := False;
      ReadOnly := True
    end;

  if LookupSourceInterOrganization = nil then begin
    FlcbInterOrganizationName.DataBinding.DataField    := LDataFieldAccountId;
    FlcbInterOrganizationName.Properties.KeyFieldNames := LLookupFieldAccountId;
    FlcbInterOrganizationName.Properties.ListSource    := LookupSource;
  end else begin
    FlcbInterOrganizationName.DataBinding.DataField    := LDataFieldInterOrganizationId;
    FlcbInterOrganizationName.Properties.KeyFieldNames := LLookupFieldInterOrganizationId;
    FlcbInterOrganizationName.Properties.ListSource    := LookupSourceInterOrganization;
  end;

  LConfigInterOrganization := LConfigAccount and Assigned(LookupSourceInterOrganization) and (not LDataFieldInterOrganizationId.IsEmpty) and (not LLookupFieldInterOrganizationId.IsEmpty);
  if ShowIntermediary and LConfigInterOrganization and (not EditStyle) then begin
    FLookupMDDCInterOrganization.MasterFields := LDataFieldInterOrganizationId;
    FLookupMDDCInterOrganization.DetailFields := LLookupFieldInterOrganizationId;
    FLookupMDDCInterOrganization.Condition    := QuoteString(LDataFieldInterOrganizationId, '[') + SCondition_IS_NOT_NULL;
    FLookupMDDCInterOrganization.Enabled      := True;
  end else if ShowIntermediary and LConfigInterOrganization then begin
    FLookupMDDCInterOrganization.MasterFields := LDataFieldAccountId;
    FLookupMDDCInterOrganization.DetailFields := LLookupFieldAccountId;
    FLookupMDDCInterOrganization.Condition    := QuoteString(LDataFieldAccountId, '[') + SCondition_IS_NOT_NULL;
    FLookupMDDCInterOrganization.Enabled      := True;
  end else begin
    FLookupMDDCInterOrganization.Enabled      := False;
//    FLookupMDDCInterOrganization.MasterFields := '';
//    FLookupMDDCInterOrganization.DetailFields := '';
  end;

  // InterAccount

  if Assigned(LookupSourceInterAccount) and (not FReadOnly.InterAccount) then
    with FlcbInterAccountNumber.Properties do begin
      Buttons[0].Visible := True;
      ReadOnly := False
    end
  else
    with FlcbInterAccountNumber.Properties do begin
      Buttons[0].Visible := False;
      ReadOnly := True
    end;

  if LookupSourceInterAccount = nil then begin
    FlcbInterAccountNumber.DataBinding.DataField    := LDataFieldAccountId;
    FlcbInterAccountNumber.Properties.KeyFieldNames := LLookupFieldAccountId;
    FlcbInterAccountNumber.Properties.ListSource    := LookupSource;
  end else begin
    FlcbInterAccountNumber.DataBinding.DataField    := LDataFieldInterAccountId;
    FlcbInterAccountNumber.Properties.KeyFieldNames := LLookupFieldInterAccountId;
    FlcbInterAccountNumber.Properties.ListSource    := LookupSourceInterAccount;
  end;

  LConfigInterAccount := LConfigInterOrganization and Assigned(LookupSourceInterAccount) and (not LDataFieldInterAccountId.IsEmpty) and (not LLookupFieldInterAccountId.IsEmpty);

  if ShowIntermediary and LConfigInterAccount and (not EditStyle) then begin
    FLookupMDDCInterAccount.MasterFields := LDataFieldInterAccountId;
    FLookupMDDCInterAccount.DetailFields := LLookupFieldInterAccountId;
    FLookupMDDCInterAccount.Condition    := QuoteString(LDataFieldInterAccountId, '[') + SCondition_IS_NOT_NULL;
    FLookupMDDCInterAccount.Enabled      := True;
  end else if ShowIntermediary and LConfigInterAccount then begin
    FLookupMDDCInterAccount.MasterFields := LDataFieldAccountId + ';' + DataFieldInterOrganizationId;
    FLookupMDDCInterAccount.DetailFields := LLookupFieldAccountId + ';' + LookupFieldInterOrganizationId;
    FLookupMDDCInterAccount.Condition    := QuoteString(LDataFieldAccountId, '[') + SCondition_IS_NOT_NULL
                                              + ' ' + SConst_AND + ' '
                                              + QuoteString(LDataFieldInterOrganizationId, '[') + SCondition_IS_NOT_NULL;
    FLookupMDDCInterAccount.Enabled      := True;
  end else begin
    FLookupMDDCInterAccount.Enabled      := False;
//    FLookupMDDCInterAccount.MasterFields := '';
//    FLookupMDDCInterAccount.DetailFields := '';
  end;
end;
{$ENDIF}

{ TBaseAccountPanelWidths }

constructor TBaseAccountPanelWidths.Create(AOwner: TCustomBaseAccountPanel);
begin
  FOwner := AOwner;
  inherited Create;
end;

function TBaseAccountPanelWidths.GetCaption: Integer;
begin
  Result := FOwner.FlpOrganization.Caption.Width
end;

procedure TBaseAccountPanelWidths.SetCaption(Value: Integer);
begin
  if Value < 0 then Value := 0;

  FOwner.FlpOrganization.Caption.Width := Value;
  FOwner.FlpAccount.Caption.Width := Value;
  FOwner.FlpInter.Caption.Width := Value;
  FOwner.FlpAccountPart.Caption.Width := Value;
  FOwner.FlpAccountPartDetails.Caption.Width := Value;
end;

function TBaseAccountPanelWidths.IsCaptionStored: Boolean;
begin
  Result := (FOwner.FlpOrganization.Caption.Width <> IConst_DefaultCaptionWidth);
end;

function TBaseAccountPanelWidths.GetAccount: Integer;
begin
  Result := FOwner.FlcbAccountNumber.Width
end;

procedure TBaseAccountPanelWidths.SetAccount(Value: Integer);
begin
  if Value < 50 then Value := 50;

  FOwner.FlcbAccountNumber.Width := Value;
  FOwner.FlcbInterAccountNumber.Width := Value;
  FOwner.FlcbAccountPartNumber.Width := Value;
end;

function TBaseAccountPanelWidths.IsAccountStored: Boolean;
begin
  Result := (Account <> IConst_DefaultAccountWidth)
end;

{ TCustomBaseAccountPanelCaptions }

procedure TCustomBaseAccountPanelCaptions.AssignTo(Dest: TPersistent);
begin
  inherited;
  with TCustomBaseAccountPanelCaptions(Dest) do begin
    Title         := Self.Title;
    Organization  := Self.Organization;
    Account       := Self.Account;
    Intermediary  := Self.Intermediary;
    Details       := Self.Details;
  end;
end;

constructor TCustomBaseAccountPanelCaptions.Create(AOwner: TCustomBaseAccountPanel);
begin
  FOwner := AOwner;
  inherited Create;
end;

function TCustomBaseAccountPanelCaptions.GetAccount: String;
begin
  Result := FOwner.FlpAccount.Caption.Text
end;

procedure TCustomBaseAccountPanelCaptions.SetAccount(const Value: String);
begin
  FOwner.FlpAccount.Caption.Text := Value
end;

function TCustomBaseAccountPanelCaptions.GetAccountPart: String;
begin
  Result := FOwner.FlpAccountPart.Caption.Text
end;

procedure TCustomBaseAccountPanelCaptions.SetAccountPart(const Value: String);
begin
  FOwner.FlpAccountPart.Caption.Text := Value
end;

function TCustomBaseAccountPanelCaptions.IsAccountPartStored: Boolean;
begin
  Result := (AccountPart <> SCaption_AccountPart)
end;

function TCustomBaseAccountPanelCaptions.IsAccountStored: Boolean;
begin
  Result := (Account <> SCaption_Account)
end;

function TCustomBaseAccountPanelCaptions.GetAccountPartDetails: String;
begin
  Result := FOwner.FlpAccountPartDetails.Caption.Text
end;

procedure TCustomBaseAccountPanelCaptions.SetAccountPartDetails(const Value: String);
begin
  FOwner.FlpAccountPartDetails.Caption.Text := Value;
end;

function TCustomBaseAccountPanelCaptions.IsAccountPartDetailsStored: Boolean;
begin
  Result := (Details <> SCaption_Details)
end;

function TCustomBaseAccountPanelCaptions.GetIntermediary: String;
begin
  Result := FOwner.FlpInter.Caption.Text
end;

procedure TCustomBaseAccountPanelCaptions.SetIntermediary(const Value: String);
begin
  FOwner.FlpInter.Caption.Text := Value
end;

function TCustomBaseAccountPanelCaptions.IsIntermediaryStored: Boolean;
begin
  Result := (Intermediary <> SCaption_Intermediary)
end;

function TCustomBaseAccountPanelCaptions.GetOrganization: String;
begin
  Result := FOwner.FlpOrganization.Caption.Text
end;

procedure TCustomBaseAccountPanelCaptions.SetOrganization(const Value: String);
begin
  FOwner.FlpOrganization.Caption.Text := Value
end;

function TCustomBaseAccountPanelCaptions.IsOrganizationStored: Boolean;
begin
  Result := (Organization <> SCaption_Organization)
end;

function TCustomBaseAccountPanelCaptions.GetTitle: String;
begin
  Result := FOwner.FCaptionLabel.Caption
end;

procedure TCustomBaseAccountPanelCaptions.SetTitle(const Value: String);
begin
  FOwner.FCaptionLabel.Caption := Value;

  if (Value.Length > 0) <> (FOwner.FCaptionLabel.Parent <> nil) then begin
    if (Value.Length > 0) then begin
      FOwner.FlpOrganization.Caption.Left       := IConst_SecondCaptionLeft;
      FOwner.FlpAccount.Caption.Left            := IConst_SecondCaptionLeft;
      FOwner.FlpInter.Caption.Left              := IConst_SecondCaptionLeft;
      FOwner.FlpAccountPart.Caption.Left        := IConst_SecondCaptionLeft;
      FOwner.FlpAccountPartDetails.Caption.Left := IConst_SecondCaptionLeft;

      FOwner.FCaptionLabel.Parent := FOwner;
    end else begin
      FOwner.FCaptionLabel.Parent := nil;

      FOwner.FlpOrganization.Caption.Left       := IConst_FirstCaptionLeft;
      FOwner.FlpAccount.Caption.Left            := IConst_FirstCaptionLeft;
      FOwner.FlpInter.Caption.Left              := IConst_FirstCaptionLeft;
      FOwner.FlpAccountPart.Caption.Left        := IConst_FirstCaptionLeft;
      FOwner.FlpAccountPartDetails.Caption.Left := IConst_FirstCaptionLeft;
    end;

    FOwner.ReAlignPanels;
  end;
end;

{ TBaseAccountPanelReadOnly }

constructor TBaseAccountPanelReadOnly.Create(AOwner: TCustomBaseAccountPanel);
begin
  FOwner := AOwner;
  inherited Create;
end;

{$IFNDEF PACKAGE}
function TBaseAccountPanelReadOnly.GetAccount: Boolean;
begin
  Result := FOwner.Lookup.ReadOnly;
end;

procedure TBaseAccountPanelReadOnly.SetAccount(const Value: Boolean);
begin
  FOwner.Lookup.ReadOnly := Value;
end;

function TBaseAccountPanelReadOnly.GetAccountPart: Boolean;
begin
  Result := FOwner.FltcbAccountPartName.Properties.ReadOnly
end;

procedure TBaseAccountPanelReadOnly.SetAccountPart(const Value: Boolean);
begin
  FOwner.FltcbAccountPartName.Properties.ReadOnly := Value
end;

procedure TBaseAccountPanelReadOnly.SetInterOrganization(const Value: Boolean);
begin
  if FInterOrganization <> Value then begin
    FInterOrganization := Value;
    FOwner.Rebuild
  end;
end;

procedure TBaseAccountPanelReadOnly.SetInterAccount(const Value: Boolean);
begin
  if FInterAccount <> Value then begin
    FInterAccount := Value;
    FOwner.Rebuild
  end;
end;
{$ENDIF}

initialization
  RegisterClasses([TBaseAccountPanel, TBaseAccountPanelReadOnly, TBaseAccountPanelWidths]);

finalization
  UnRegisterClasses([TBaseAccountPanel, TBaseAccountPanelReadOnly, TBaseAccountPanelWidths]);

end.
