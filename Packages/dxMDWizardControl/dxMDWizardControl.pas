unit dxMDWizardControl;

interface

uses
  System.Classes,
  System.Generics.Collections,
  Vcl.Controls,
  dxCoreClasses,
  dxCustomWizardControl,
  dxWizardControl;

type
  TdxMDWizardControlPageCustomButton = class(TcxOwnedPersistent)
  private
    FWizardButton  : TdxWizardControlCustomButton;
  protected
  public
    constructor Create(AOwner: TPersistent; AButton : TdxWizardControlCustomButton); reintroduce;
    destructor Destroy; override;
  published
  end;

  { TdxWizardControlButtons }

  TdxMDWizardControlPageButtons = class(TcxOwnedPersistent)
  private
    FBack             : TdxMDWizardControlPageCustomButton;
    FCancel           : TdxMDWizardControlPageCustomButton;
    FFinish           : TdxMDWizardControlPageCustomButton;
    FHelp             : TdxMDWizardControlPageCustomButton;
    FNext             : TdxMDWizardControlPageCustomButton;
    // -- Потом как-нибудь
    // FCustomButtons    : TdxMDWizardControlCustomButtons;
    procedure SetBack(AValue: TdxMDWizardControlPageCustomButton);
    procedure SetCancel(AValue: TdxMDWizardControlPageCustomButton);
    procedure SetFinish(AValue: TdxMDWizardControlPageCustomButton);
    procedure SetHelp(AValue: TdxMDWizardControlPageCustomButton);
    procedure SetNext(AValue: TdxMDWizardControlPageCustomButton);
    function GetPage: TdxWizardControlCustomPage;
  protected
    procedure DoAssign(Source: TPersistent); override;
    property Page : TdxWizardControlCustomPage read GetPage;
    procedure InitButtons; virtual;
    procedure ClearButtons; virtual;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
  published
    property Back           : TdxMDWizardControlPageCustomButton  read FBack    write SetBack;
    property Cancel         : TdxMDWizardControlPageCustomButton  read FCancel  write SetCancel;
    property Finish         : TdxMDWizardControlPageCustomButton  read FFinish  write SetFinish;
    property Help           : TdxMDWizardControlPageCustomButton  read FHelp    write SetHelp;
    property Next           : TdxMDWizardControlPageCustomButton  read FNext    write SetNext;
    // -- Потом как-нибудь
    // property CustomButtons  : TdxMDWizardControlCustomButtons   read FCustomButtons write SetCustomButtons;
  end;


  { TdxMDWizardControlPage }

  TdxMDWizardControlPage = class(TdxWizardControlCustomPage)
  private
    FButtons          : TdxMDWizardControlPageButtons;
    FDefaultNextPage  : TdxWizardControlCustomPage;
    procedure SetButtons(const Value: TdxMDWizardControlPageButtons);
    procedure SetDefaultNextPage(const Value: TdxWizardControlCustomPage);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
  published
    property Buttons          : TdxMDWizardControlPageButtons read FButtons write SetButtons;
    property DefaultNextPage  : TdxWizardControlCustomPage read FDefaultNextPage write SetDefaultNextPage;

    property DoubleBuffered;
    property Font;
    property Header;
    property OptionsSize;
    property PageIndex;
    property PageVisible;
  {$IFDEF DELPHI14}
    property ParentDoubleBuffered;
  {$ENDIF}
    property ParentFont;
    property Watermark;

    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
  end;

  { TdxMDWizardControl }

  TdxMDWizardControl = class(TdxCustomWizardControl)
  private
    FPagesStack : TList<TdxMDWizardControlPage>;
    function PagesStackPrev     : TdxMDWizardControlPage; inline;
    procedure PagesStackPush(APage : TdxMDWizardControlPage); inline;
    procedure PagesStackDeleteFrom(AIndex: Integer);
  protected
    procedure DoPageChanged; override;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    function AddPage: TdxMDWizardControlPage; overload;

    // Переход по стеку на один шаг назад
    function DefaultStepBack: Boolean;
    // Переход на страницу, которая указана как DefaultNextPage; если нет - то шаг+1
    function DefaultStepNext: Boolean;
  published
    property ActivePage stored False;
    property Align;
    property AutoSize;
    property Buttons;
    property Font;
    property Header;
    property InfoPanel;
    property LookAndFeel;
    property OptionsAnimate;
    property OptionsViewStyleAero;
    property ParentFont;
    property ViewStyle;
    property Watermark;

    property OnButtonClick;
    property OnClick;
    property OnDblClick;
    property OnInfoPanelClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPageChanged;
    property OnPageChanging;
    property OnResize;
  end;

implementation

uses
  System.SysUtils,
  Dialogs;

{ TdxMDWizardControl }

function TdxMDWizardControl.AddPage: TdxMDWizardControlPage;
begin
  Result := TdxMDWizardControlPage(inherited AddPage(TdxMDWizardControlPage));
end;

constructor TdxMDWizardControl.Create(AComponent: TComponent);
begin
  inherited;
  FPagesStack := TList<TdxMDWizardControlPage>.Create;
end;

function TdxMDWizardControl.DefaultStepBack: Boolean;
var
  LPrevPage : TdxMDWizardControlPage;
begin
  LPrevPage := PagesStackPrev;
  if Assigned(LPrevPage) then begin
    ActivePage := LPrevPage;
    Result := True;
  end else
    Result := False;
end;

function TdxMDWizardControl.DefaultStepNext: Boolean;
var
  LPage : TdxMDWizardControlPage;
begin
  Result := False;

  LPage := TdxMDWizardControlPage(ActivePage);
  if Assigned(LPage) then begin
    if LPage.InheritsFrom(TdxMDWizardControlPage)
    and Assigned(LPage.DefaultNextPage) then begin
      ActivePage := LPage.DefaultNextPage;
      Result := True;
    end else
    if ActivePageIndex < Pred(PageCount) then begin
      ActivePageIndex := Ord(ActivePageIndex);
      Result := True;
    end;
  end;
end;

destructor TdxMDWizardControl.Destroy;
begin
  FreeAndNil(FPagesStack);
  inherited;
end;

procedure TdxMDWizardControl.DoPageChanged;
var
  LIndex : Integer;
begin
  inherited;
  if not Assigned(FPagesStack) then
    Exit;

  LIndex := FPagesStack.IndexOf(TdxMDWizardControlPage(Self.ActivePage));
  if LIndex < 0 then
    PagesStackPush(TdxMDWizardControlPage(Self.ActivePage))
  else
    PagesStackDeleteFrom(LIndex);
end;

procedure TdxMDWizardControl.PagesStackDeleteFrom(AIndex: Integer);
begin
  if AIndex < 0 then
    FPagesStack.Clear
  else
    while FPagesStack.Count > Succ(AIndex) do
      FPagesStack.Delete(Pred(FPagesStack.Count));
end;

function TdxMDWizardControl.PagesStackPrev: TdxMDWizardControlPage;
begin
  if FPagesStack.Count > 1 then
    Result := FPagesStack[Pred(Pred(FPagesStack.Count))]
  else
    Result := nil;
end;

procedure TdxMDWizardControl.PagesStackPush(APage: TdxMDWizardControlPage);
begin
  FPagesStack.Add(APage);
end;

{ TdxMDWizardControlPageCustomButton }

constructor TdxMDWizardControlPageCustomButton.Create(AOwner: TPersistent;
  AButton: TdxWizardControlCustomButton);
begin
  inherited Create(AOwner);
  FWizardButton := AButton;
end;

destructor TdxMDWizardControlPageCustomButton.Destroy;
begin
  inherited;
end;

{ TdxMDWizardControlPageButtons }

procedure TdxMDWizardControlPageButtons.ClearButtons;
begin
  FreeAndNil(FBack);
  FreeAndNil(FNext);
  FreeAndNil(FCancel);
  FreeAndNil(FFinish);
  FreeAndNil(FHelp);
end;

constructor TdxMDWizardControlPageButtons.Create(AOwner: TPersistent);
begin
{$IFDEF DEBUG}
  Assert(Assigned(AOwner), 'TdxMDWizardControlPageButtons: Не определен AOwner!');
  Assert(AOwner.InheritsFrom(TdxWizardControlCustomPage), 'TdxMDWizardControlPageButtons: AOwner не наследник TdxWizardControlCustomPage!');
{$ENDIF}
  inherited Create(AOwner);
  InitButtons;
end;

destructor TdxMDWizardControlPageButtons.Destroy;
begin
  ClearButtons;
  inherited;
end;

procedure TdxMDWizardControlPageButtons.DoAssign(Source: TPersistent);
begin
  Assert(Source.InheritsFrom(TdxMDWizardControlPageButtons));
  inherited;
  FBack.Assign(TdxMDWizardControlPageButtons(Source).Back);
  FNext.Assign(TdxMDWizardControlPageButtons(Source).Next);
  FCancel.Assign(TdxMDWizardControlPageButtons(Source).Cancel);
  FFinish.Assign(TdxMDWizardControlPageButtons(Source).Finish);
  FHelp.Assign(TdxMDWizardControlPageButtons(Source).Help);
end;

function TdxMDWizardControlPageButtons.GetPage: TdxWizardControlCustomPage;
begin
  Result := TdxWizardControlCustomPage(Owner);
end;

procedure TdxMDWizardControlPageButtons.InitButtons;

  procedure _CheckCreate(var AMDPageButton : TdxMDWizardControlPageCustomButton; AWizardControlButton: TdxWizardControlCustomButton);
  begin
    if  Assigned(AMDPageButton)
    and (AMDPageButton.FWizardButton <> AWizardControlButton) then
      FreeAndNil(AMDPageButton);

    if (not Assigned(AMDPageButton)) then
      AMDPageButton := TdxMDWizardControlPageCustomButton.Create(Self, AWizardControlButton);
  end;

begin
  if  Assigned(Page)
  and Assigned(Page.WizardControl)
  and Assigned(Page.WizardControl.Buttons) then begin
    _CheckCreate(FBack, Page.WizardControl.Buttons.Back);
    _CheckCreate(FNext, Page.WizardControl.Buttons.Next);
    _CheckCreate(FCancel, Page.WizardControl.Buttons.Cancel);
    _CheckCreate(FFinish, Page.WizardControl.Buttons.Finish);
    _CheckCreate(FHelp, Page.WizardControl.Buttons.Help);
  end else
    ClearButtons;
end;

procedure TdxMDWizardControlPageButtons.SetBack(AValue: TdxMDWizardControlPageCustomButton);
begin
  FBack.Assign(AValue);
end;

procedure TdxMDWizardControlPageButtons.SetCancel(AValue: TdxMDWizardControlPageCustomButton);
begin
  FCancel.Assign(AValue);
end;

procedure TdxMDWizardControlPageButtons.SetFinish(AValue: TdxMDWizardControlPageCustomButton);
begin
  FFinish.Assign(AValue);
end;

procedure TdxMDWizardControlPageButtons.SetHelp(AValue: TdxMDWizardControlPageCustomButton);
begin
  FHelp.Assign(AValue);
end;

procedure TdxMDWizardControlPageButtons.SetNext(AValue: TdxMDWizardControlPageCustomButton);
begin
  FNext.Assign(AValue);
end;

{ TdxMDWizardControlPage }

constructor TdxMDWizardControlPage.Create(AComponent: TComponent);
begin
  inherited;
  FButtons := TdxMDWizardControlPageButtons.Create(Self);
end;

destructor TdxMDWizardControlPage.Destroy;
begin
  FreeAndNil(FButtons);
  inherited;
end;

procedure TdxMDWizardControlPage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = FDefaultNextPage then
      FDefaultNextPage := nil;
end;

procedure TdxMDWizardControlPage.SetButtons(const Value: TdxMDWizardControlPageButtons);
begin
  FButtons.Assign(Value);
end;

procedure TdxMDWizardControlPage.SetDefaultNextPage(const Value: TdxWizardControlCustomPage);
begin
  if FDefaultNextPage = Value then
    Exit;

  if Assigned(FDefaultNextPage) then
    FDefaultNextPage.RemoveFreeNotification(Self);

  FDefaultNextPage := Value;

  if Assigned(FDefaultNextPage) then
    FDefaultNextPage.FreeNotification(Self);
end;

procedure TdxMDWizardControlPage.SetParent(AParent: TWinControl);
begin
  inherited;
  if Assigned(FButtons) then begin
    if Assigned(AParent) then
      FButtons.InitButtons
    else
      FButtons.ClearButtons;
  end;
end;

initialization
  RegisterClass(TdxMDWizardControl);

finalization
  UnRegisterClass(TdxMDWizardControl);

end.
