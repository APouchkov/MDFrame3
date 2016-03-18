unit SBaseEditForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SBaseCustomEditForm, cxGraphics,
  cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit,
  dxSkinsCore, dxSkinsDefaultPainters, Vcl.Menus, dxBarBuiltInMenu,
  System.Actions, Vcl.ActnList, dxBar, cxClasses, dxMDBar, cxPC, cxTextEdit,
  Vcl.StdCtrls, cxButtons, Vcl.ExtCtrls, LayoutPanel, cxGroupBox;

type
  TSBaseEditFrm = class(TSBaseCustomEditFrm)
    procedure btnApplyClick(Sender: TObject); override;
  private
    { Private declarations }
  protected
    procedure DoApply; override;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TSBaseEditFrm }

procedure TSBaseEditFrm.btnApplyClick(Sender: TObject);
var
  LFocusedControl: TWinControl;
begin
  Assert(FormMode = fmEdit);

  if DataChanged then begin
    LFocusedControl := ActiveControl;

    DoApply;
    InternalReInit(CreateFormParams);

    if Assigned(LFocusedControl) and not (csDestroying in LFocusedControl.ComponentState) and LFocusedControl.CanFocus then
      LFocusedControl.SetFocus;
  end;
end;

procedure TSBaseEditFrm.DoApply;
begin
  if (not btnOK.Focused) and (btnOK.CanFocus) then btnOK.SetFocus;

  CheckValues;
  BeforeApplyUpdates;
  try
    if TransactionMode = stmClient then SQLConnectionProvider.BeginTran;

    ApplyUpdates;

    if TransactionMode = stmClient then SQLConnectionProvider.CommitTran;
  except on E:Exception do begin
    try
      if (TransactionMode in [stmClient, stmServer]) then
        SQLConnectionProvider.RollbackTran(True);
    except
    end;

    try
      RestoreUpdates
    except
    end;

    if not (E is EAbort) then
      ErrorMessage(E.Message, btnOk)
    else
      Raise;
  end end;

  try
    CommitUpdates;
    AfterCommitUpdates;
  except
  end;
end;

initialization
  RegisterClass(TSBaseEditFrm);

finalization
  UnRegisterClass(TSBaseEditFrm);

end.
