unit SBaseWizardForm;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Actions,
  System.Generics.Collections,

  Winapi.Messages,

  Vcl.ActnList,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  Data.DB,
  DataHandler,
  SBaseForm,
  SBaseDataModule,

  dxBar, cxClasses, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxCustomWizardControl, dxMDWizardControl;

type
{$REGION 'TSBaseWizardFrm - Definition'}
  TSBaseWizardFrm = class(TSBaseFrm)
    WizardControl           : TdxMDWizardControl;
    DataSetMain             : TSQLDataSetProvider;
    WizardControlPageStart  : TdxMDWizardControlPage;
    DataSourceMain          : TDataSource;

    // Преобразуем событие ButtonClick в вызов ItemReaction, которая связана с вызыванной кнопкой
    procedure WizardControlButtonClick(Sender: TObject; AKind: TdxWizardControlButtonKind; var AHandled: Boolean);
  private
  protected
    procedure DoDefaultWizardCancel; virtual;
    procedure DoDefaultWizardFinish; virtual;
    function DoDefaultWizardBack: Boolean; virtual;
    function DoDefaultWizardNext: Boolean; virtual;
  public
    procedure InternalCreate; override;
    procedure InternalInit; override;
  end;
{$ENDREGION 'TSBaseWizardFrm - Definition'}

{$REGION 'FASTSCRIPT_RTTI - Definition'}
{$IFDEF FASTSCRIPT_RTTI}
  TfsFunctions_TSBaseWizardFrm = class(TfsRTTIModule)
  public
    constructor Create(AScript: TfsScript); override;
  end;
{$ENDIF}
{$ENDREGION 'FASTSCRIPT_RTTI - Definition'}

implementation

uses
  SBaseUtils,
  SBaseItemReactions,
  SBaseVariantFunctions,
  fs_iinterpreter;

{$R *.dfm}

{$REGION 'TSBaseWizardFrm'}
{ TSBaseWizardFrm }

procedure TSBaseWizardFrm.InternalCreate;
begin
  inherited;

  WizardControl.Buttons.Back.Caption := SBaseProperties.Text_MenuBackward;
  WizardControl.Buttons.Next.Caption := SBaseProperties.Text_MenuForward;
  WizardControl.Buttons.Cancel.Caption := SBaseProperties.Text_MenuCancel;
  WizardControl.Buttons.Finish.Caption := SBaseProperties.Text_MenuFinish;
  WizardControl.Buttons.Help.Caption := SBaseProperties.Text_MenuHelp;

  if Icon.Empty then
    IconIndex := SBaseFormProperties.IconIndex_Wizard;

  DataSetMain.ConnectionProvider := SQLConnectionProvider;
end;

procedure TSBaseWizardFrm.InternalInit;
begin
  DataSetMain.Open;
  if DataSetMain.IsEmpty then
    DataSetMain.Append;
  {
  if ...непонятны пока условия... then begin
    //-- Инициализируем DataSetMain из параметров
    DataSetMain.Params.Load(Variables, lvcAlways);
  end;
  }
  inherited;
end;

function TSBaseWizardFrm.DoDefaultWizardBack: Boolean;
begin
  CallFunction('WizardDefaultBack', False);
  // Делаем шаг назад по стеку закладок
  Result := WizardControl.DefaultStepBack;
end;

function TSBaseWizardFrm.DoDefaultWizardNext: Boolean;
begin
  CallFunction('WizardDefaultNext', False);
  // Делаем шаг вперед "стандартный"
  Result := WizardControl.DefaultStepNext;
end;

procedure TSBaseWizardFrm.DoDefaultWizardCancel;
begin
  CallFunction('WizardDefaultCancel', False);
  // Закрываемся с mrCancel
  Close;
  ModalResult := mrCancel;
end;

procedure TSBaseWizardFrm.DoDefaultWizardFinish;
begin
  CallFunction('WizardDefaultFinish', False);
  // Закрываемся с mrOk
  Close;
  ModalResult := mrOk;
end;

procedure TSBaseWizardFrm.WizardControlButtonClick
(
  Sender: TObject;
  AKind: TdxWizardControlButtonKind; var AHandled: Boolean
);
var
  LCurrentPage  : TdxMDWizardControlPage;
  LMDButton     : TdxMDWizardControlPageCustomButton;
  LItemReaction : TBaseItemReaction;
  LHandled      : Variant;
begin
  // Преобразуем событие ButtonClick в вызов ItemReaction, которая связана с вызыванной кнопкой
  inherited;
  LCurrentPage := TdxMDWizardControlPage(WizardControl.ActivePage);
  Assert(Assigned(LCurrentPage));
  Assert(LCurrentPage.InheritsFrom(TdxMDWizardControlPage));
  case AKind of
    wcbkBack:
      LMDButton := LCurrentPage.Buttons.Back;
    wcbkNext:
      LMDButton := LCurrentPage.Buttons.Next;
    wcbkCancel:
      LMDButton := LCurrentPage.Buttons.Cancel;
    wcbkFinish:
      LMDButton := LCurrentPage.Buttons.Finish;
    wcbkHelp:
      LMDButton := LCurrentPage.Buttons.Help;
    else
      LMDButton := nil;
  end;
  if not Assigned(LMDButton) then
    Exit;

  LItemReaction := ItemsReactions.FindItemProperties(LMDButton);
  if Assigned(LItemReaction) then begin
    LItemReaction.ItemClick(Sender);
    LHandled := LItemReaction.OnClick.ClassConstants['Handled'];
    if VarIsPresent(LHandled) then
      AHandled := LHandled;
  end;

  if (AHandled) then
    Exit;
  // Обработчики по умолчанию:
  if LMDButton = LCurrentPage.Buttons.Cancel then begin
    DoDefaultWizardCancel;
    AHandled := True;
  end else
  if LMDButton = LCurrentPage.Buttons.Finish then begin
    DoDefaultWizardFinish;
    AHandled := True;
  end else
  if LMDButton = LCurrentPage.Buttons.Back then begin
    if DoDefaultWizardBack then
      AHandled := True;
  end else
  if LMDButton = LCurrentPage.Buttons.Next then begin
    if DoDefaultWizardNext then
      AHandled := True;
  end;
end;


{$ENDREGION 'TSBaseWizardFrm'}

{$REGION 'FASTSCRIPT_RTTI - Definition'}
{$IFDEF FASTSCRIPT_RTTI}
constructor TfsFunctions_TSBaseEditFrm.Create(AScript: TfsScript);
begin
  AScript.AddEnum('TFormMode', 'fmView, fmEdit, fmNew, fmCopy');
end;
{$ENDIF}
{$ENDREGION 'FASTSCRIPT_RTTI - Definition'}

initialization
  RegisterClass(TSBaseWizardFrm);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Add(TfsFunctions_TSBaseEditFrm);
{$ENDIF}

finalization
  UnRegisterClass(TSBaseWizardFrm);
{$IFDEF FASTSCRIPT_RTTI}
  fsRTTIModules.Remove(TfsFunctions_TSBaseEditFrm);
{$ENDIF}


end.
