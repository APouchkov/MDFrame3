unit SBaseSearchBar;

interface

uses
  Classes, Controls, ExtCtrls, cxButtons, cxCheckBox, cxUserCheckBox, cxTextEdit;

type
  TSearchBarVisualElements = set of (veBackButton, veForwardButton,
    veMatchCase, veWholeWord, veClose);

  TSBaseSearchBar = class(TCustomPanel)
  strict private
    FVisualElements: TSearchBarVisualElements;
    FSearchControl: TControl;
    FOnFinishSearch: TNotifyEvent;

    // последнее направление поиска - используется при нажатии на F3
    FLastDirection: Boolean;

    procedure SearchText(AForward: Boolean);
    procedure SetVisualElements(const AValue: TSearchBarVisualElements);

    function GetEditValue: string;
    procedure SetEditValue(const AValue: string);
  published
    BackButton: TcxButton;
    ForwardButton: TcxButton;
    CloseButton: TcxButton;
    MatchCaseCheckBox: TcxUserCheckBox;
    WholeWordCheckBox: TcxUserCheckBox;
    EditBox: TcxTextEdit;

    procedure CloseButtonClick(Sender: TObject);
    procedure BackButtonClick(Sender: TObject);
    procedure ForwardButtonClick(Sender: TObject);
    procedure EditBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    property SearchControl: TControl read FSearchControl write FSearchControl;
    property VisualElements: TSearchBarVisualElements read FVisualElements write SetVisualElements default [veForwardButton];

    property Align;
    property TabOrder;
    property Visible;

    property EditValue: string read GetEditValue write SetEditValue;
    property OnFinishSearch: TNotifyEvent read FOnFinishSearch write FOnFinishSearch;
  public
    procedure SearchLastDirection;
    procedure Show;
    constructor Create(AOwner: TComponent); override;
  end;

  procedure Register;

implementation

{$RESOURCE *.dfm}

uses
  Windows, ComCtrls, cxRichEdit;

procedure Register;
begin
  RegisterComponents('MD Project Tools', [TSBaseSearchBar]);
end;

{$REGION 'TSBaseSearchBar'}

procedure TSBaseSearchBar.BackButtonClick(Sender: TObject);
begin
  SearchText(False);
end;

procedure TSBaseSearchBar.CloseButtonClick(Sender: TObject);
begin
  Visible := False;
end;

constructor TSBaseSearchBar.Create(AOwner: TComponent);
begin
  inherited;

  Height := 21;
  BevelOuter := bvNone;
  FLastDirection := True;
  FVisualElements := [];

  ShowCaption := False;

  InitInheritedComponent(Self, TComponent);

  SetVisualElements([veForwardButton]);
end;

procedure TSBaseSearchBar.EditBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    SearchText(True);
    Key := 0;
  end;
end;

procedure TSBaseSearchBar.ForwardButtonClick(Sender: TObject);
begin
  SearchText(True);
end;

function TSBaseSearchBar.GetEditValue: string;
begin
  Result := EditBox.Text;
end;

procedure TSBaseSearchBar.SearchLastDirection;
begin
  SearchText(FLastDirection);
end;

procedure TSBaseSearchBar.SearchText(AForward: Boolean);
var
  LPosition: Integer;
  LSearchOptions: TSearchTypes;
  LRichEdit: TcxRichEdit;
begin
  LSearchOptions := [];

  if not (Assigned(FSearchControl) and (FSearchControl is TcxRichEdit)) then
    Exit;

  FLastDirection := AForward;

  if EditBox.Text = '' then
    Exit;

  if MatchCaseCheckBox.Checked then
    Include(LSearchOptions, stMatchCase);

  if WholeWordCheckBox.Checked then
    Include(LSearchOptions, stWholeWord);

  LRichEdit := FSearchControl as TcxRichEdit;
  LPosition := LRichEdit.SelStart;
  if AForward then
    LPosition := LRichEdit.FindTexT(EditBox.Text, LPosition + 1, Length(LRichEdit.Text), LSearchOptions, True)
  else
    LPosition := LRichEdit.FindTexT(EditBox.Text, LPosition, LPosition, LSearchOptions, False);

  if LPosition <> -1 then begin
    LRichEdit.SelStart   := LPosition;
    LRichEdit.SelLength  := Length(EditBox.Text);
  end
  else
    if Assigned(FOnFinishSearch) then
      FOnFinishSearch(Self);
end;

procedure TSBaseSearchBar.SetEditValue(const AValue: string);
begin
  if AValue <> EditBox.Text then
    EditBox.Text := AValue;
end;

procedure TSBaseSearchBar.SetVisualElements(const AValue: TSearchBarVisualElements);
begin
  if FVisualElements = AValue then
    Exit;

  FVisualElements := AValue;

  BackButton.Visible        := veBackButton in AValue;
  ForwardButton.Visible     := veForwardButton in AValue;
  CloseButton.Visible       := veClose in AValue;
  MatchCaseCheckBox.Visible := veMatchCase in AValue;
  WholeWordCheckBox.Visible := veWholeWord in AValue;

  if not MatchCaseCheckBox.Visible then
    MatchCaseCheckBox.Checked := False;

  if not WholeWordCheckBox.Visible then
    WholeWordCheckBox.Checked := False;
end;

procedure TSBaseSearchBar.Show;
begin
  Visible := True;

  EditBox.SetFocus;
  EditBox.SelectAll;
end;

{$ENDREGION}

initialization
  Classes.RegisterClasses([TSBaseSearchBar]);

finalization
  Classes.UnRegisterClasses([TSBaseSearchBar]);

end.
