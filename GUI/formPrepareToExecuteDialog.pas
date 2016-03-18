unit formPrepareToExecuteDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SBaseDialogForm, SBaseEditForm, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxPCdxBarPopupMenu,
  ActnList, dxBar, cxPC, cxTextEdit, StdCtrls, cxButtons, ExtCtrls, cxGroupBox,
  cxMaskEdit, cxButtonEdit, cxCheckBox, cxUserCheckBox, System.Actions,
  LayoutPanel, Menus, dxBarBuiltInMenu, cxClasses, dxSkinsCore,
  dxSkinsDefaultPainters, dxMDBar;

type
  TfrmPrepareToExecuteDialog = class(TSBaseEditFrm)
    cbShow: TcxUserCheckBox;
    cbSave: TcxUserCheckBox;
    beFilePath: TcxButtonEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

initialization
  RegisterClass(TfrmPrepareToExecuteDialog);

finalization
  UnRegisterClass(TfrmPrepareToExecuteDialog);

end.
