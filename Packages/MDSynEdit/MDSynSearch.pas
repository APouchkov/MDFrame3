unit MDSynSearch;

interface
uses
  SynEdit, SynEditTypes, SynEditMiscClasses, SynEditSearch,
  ActnList, Dialogs, Windows, SysUtils, Forms, Controls, Classes;

type
  TMDSynSearch = class(TComponent)
  strict private
    class var FFindDialog   : TFindDialog;
    class var FReplaceDialog: TReplaceDialog;
  protected
    class procedure Finalize;
  private
    FSynEdit        : TCustomSynEdit;

    FActionFindNext : TAction;
    FActionFind     : TAction;
    FActionReplace  : TAction;

    procedure SetActionFind(const AValue: TAction);
    procedure SetActionFindNext(const AValue: TAction);
    procedure SetActionReplace(const AValue: TAction);

    procedure ActionFindExecute(Sender: TObject);
    procedure ActionFindNextExecute(Sender: TObject);
    procedure ActionReplaceExecute(Sender: TObject);

    function GetSynEdit: TCustomSynEdit;
    procedure DoFind(Sender: TObject);
    procedure DoReplaceText(Sender: TObject);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property SynEdit: TCustomSynEdit read FSynEdit write FSynEdit;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property ActionFind     : TAction read FActionFind      write SetActionFind;
    property ActionFindNext : TAction read FActionFindNext  write SetActionFindNext;
    property ActionReplace  : TAction read FActionReplace   write SetActionReplace;
  end;

implementation

uses
  SBaseForm, SBaseUtils;

{ TMDSynSearch }

constructor TMDSynSearch.Create(AOwner: TComponent);
begin
{$IFDEF DEBUG}
  Assert(Assigned(AOwner) and (AOwner is TSBaseFrm));
{$ENDIF}

  inherited;

  if not Assigned(FFindDialog) then
    FFindDialog := TFindDialog.Create(nil);

  if not Assigned(FReplaceDialog) then
    FReplaceDialog:= TReplaceDialog.Create(nil);
end;

destructor TMDSynSearch.Destroy;
begin
  if Assigned(FActionFind) then
    FActionFind.OnExecute := nil;
  if Assigned(FActionFindNext) then
    FActionFindNext.OnExecute := nil;
  if Assigned(FActionReplace) then
    FActionReplace.OnExecute := nil;

  inherited;
end;

procedure TMDSynSearch.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  if (Operation = opRemove) then begin
    if (AComponent = FActionFind) then
      FActionFind := nil
    else if (AComponent = FActionFindNext) then
      FActionFindNext := nil
    else if (AComponent = FActionReplace) then
      FActionReplace := nil;
  end;
end;

procedure TMDSynSearch.SetActionFind(const AValue: TAction);
begin
  if FActionFind <> AValue then begin
    if Assigned(FActionFind) then begin
      FActionFind.OnExecute := nil;
      FActionFind.RemoveFreeNotification(Self);
    end;
    FActionFind := AValue;
    if Assigned(FActionFind) then begin
      FActionFind.OnExecute := ActionFindExecute;
      FActionFind.FreeNotification(Self);
    end;
  end;
end;

procedure TMDSynSearch.SetActionFindNext(const AValue: TAction);
begin
  if FActionFindNext <> AValue then begin
    if Assigned(FActionFindNext) then begin
      FActionFindNext.OnExecute := nil;
      FActionFindNext.RemoveFreeNotification(Self);
    end;
    FActionFindNext := AValue;
    if Assigned(FActionFindNext) then begin
      FActionFindNext.OnExecute := ActionFindNextExecute;
      FActionFindNext.FreeNotification(Self);
    end;
  end;
end;

procedure TMDSynSearch.SetActionReplace(const AValue: TAction);
begin
  if FActionReplace <> AValue then begin
    if Assigned(FActionReplace) then begin
      FActionReplace.OnExecute := nil;
      FActionReplace.RemoveFreeNotification(Self);
    end;
    FActionReplace := AValue;
    if Assigned(FActionReplace) then begin
      FActionReplace.OnExecute := ActionReplaceExecute;
      FActionReplace.FreeNotification(Self);
    end;
  end;
end;

{ TMDSynSearch }

procedure TMDSynSearch.ActionFindExecute(Sender: TObject);
begin
  SynEdit := GetSynEdit;

  FFindDialog.OnFind := DoFind;
  FFindDialog.Execute;
end;

procedure TMDSynSearch.ActionFindNextExecute(Sender: TObject);
begin
  SynEdit := GetSynEdit;

  FFindDialog.OnFind := DoFind;
  DoFind(Sender);
  SynEdit.SetFocus;
end;

procedure TMDSynSearch.ActionReplaceExecute(Sender: TObject);
begin
  SynEdit := GetSynEdit;
  if not SynEdit.ReadOnly then begin
    FReplaceDialog.OnFind := DoFind;
    FReplaceDialog.OnReplace := DoReplaceText;
    FReplaceDialog.Execute;
  end;
end;

procedure TMDSynSearch.DoFind(Sender: TObject);
var
  LOptions    : TSynSearchOptions;
  LDialog     : TFindDialog;
  LSearchText : String;
begin
  if Sender = FReplaceDialog then
    LDialog := FReplaceDialog
  else
    LDialog := FFindDialog;

  LSearchText := LDialog.FindText;
  if LSearchText.IsEmpty then
    Beep
  else begin
    ActionFindNext.Enabled := not FFindDialog.FindText.IsEmpty;
    LOptions := [];
    if not (frDown in LDialog.Options) then
      Include(LOptions, ssoBackwards);
    if frMatchCase in LDialog.Options then
      Include(LOptions, ssoMatchCase);
    if frWholeWord in LDialog.Options then
      Include(LOptions, ssoWholeWord);

    if SynEdit.SearchReplace(LSearchText, '', LOptions) = 0 then
      TSBaseFrm(Owner).InfoMessage(Format(SBaseProperties.MessageFormat_CantFindText, [LSearchText]));
//      Beep;
  end;
end;

procedure TMDSynSearch.DoReplaceText(Sender: TObject);
var
  LOptions: TSynSearchOptions;
  LSearchText: String;
begin
  LSearchText := FReplaceDialog.FindText;
  if LSearchText.IsEmpty then
    Beep
  else begin
    LOptions := [ssoReplace, ssoPrompt];
    if frMatchCase in FReplaceDialog.Options then
      Include(LOptions, ssoMatchCase);
    if frWholeWord in FReplaceDialog.Options then
      Include(LOptions, ssoWholeWord);
    if frReplaceAll in FReplaceDialog.Options then
      Include(LOptions, ssoReplaceAll);

    if SynEdit.SearchReplace(LSearchText, FReplaceDialog.ReplaceText, LOptions) = 0 then
      TSBaseFrm(Owner).InfoMessage(Format(SBaseProperties.MessageFormat_CantFindText, [LSearchText]))
    else
      SynEdit.SetFocus;
      //Beep;
  end;
end;

class procedure TMDSynSearch.Finalize;
begin
  FreeAndNil(FFindDialog);
  FreeAndNil(FReplaceDialog);
end;

function TMDSynSearch.GetSynEdit: TCustomSynEdit;
var
  LActiveControl: TWinControl;
begin
  Result := nil;
  LActiveControl := TCustomForm(Owner).ActiveControl;
  if Assigned(LActiveControl) and (LActiveControl is TCustomSynEdit) then
    Result := TCustomSynEdit(LActiveControl)
  else
    Abort;
end;

initialization
  RegisterClass(TMDSynSearch);

finalization
  TMDSynSearch.Finalize;
  UnRegisterClass(TMDSynSearch);

end.
