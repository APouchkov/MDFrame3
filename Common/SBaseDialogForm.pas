unit SBaseDialogForm;
{$I config.inc}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Graphics,
  Controls, Forms, Dialogs, cxGraphics, cxLookAndFeels,
  cxLookAndFeelPainters, Menus, cxControls, cxContainer, cxEdit, cxGroupBox,
  LayoutPanel, StdCtrls, cxButtons, ExtCtrls, SBaseForm, System.Actions,
  ActnList, dxBar
{IFDEF FASTSCRIPT_RTTI}
  , fs_iinterpreter, cxClasses, dxSkinsCore, dxSkinsDefaultPainters, dxMDBar
{ENDIF}
  ;

type
  TSBaseDialogFrm = class(TSBaseFrm)
    PanelMain: TcxGroupBox;
    PanelForm: TGroupPanel;
    PanelButtons: TGroupPanel;
    btnOk: TcxButton;
    btnCancel: TcxButton;
    lgbParams: TcxLayoutGroupBox;
    PanelParams: TPanel;

    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
  protected
    { Protected declarations }
    procedure InternalCreate; override;
    procedure InternalOk; dynamic;
  public
    { Public declarations }
  published
    { Published declarations }
    procedure FormShow(Sender: TObject); override;

    property BorderStyle default bsDialog;
    property BorderIcons default [biSystemMenu];
  end;

implementation

{$R *.dfm}

uses SBaseUtils;

procedure TSBaseDialogFrm.btnCancelClick(Sender: TObject);
begin
  ResultValues.Clear;
  Close;
  ModalResult := mrCancel;
end;

procedure TSBaseDialogFrm.btnOkClick(Sender: TObject);
begin
  if not btnCancel.Visible then
    Close
  else begin
    // FCanClose := True;
    if fsModal in FormState then begin
      InternalOk;
      ModalResult := mrOK;
    end else
      Close;
  end;
end;

procedure TSBaseDialogFrm.FormShow(Sender: TObject);
begin
  inherited;
  AdjustSize(lgbParams);
end;

procedure TSBaseDialogFrm.InternalCreate;
begin
  BorderStyle := bsDialog;
  BorderIcons := [biSystemMenu];

  btnOk.Caption := SBaseProperties.Text_MenuOk;
  btnCancel.Caption := SBaseProperties.Text_MenuCancel;

  inherited;
end;

procedure TSBaseDialogFrm.InternalOk;
begin
  CallFunction('InternalOk', False);
end;

initialization
  RegisterClass(TSBaseDialogFrm);

finalization
  UnRegisterClass(TSBaseDialogFrm);

end.
