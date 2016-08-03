unit SBasePreviewUrlForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Graphics,
  Controls, Forms, Dialogs, SBaseForm, System.Actions, ActnList,
  dxBar, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxTextEdit, cxMemo, cxClasses, dxSkinsCore,
  dxSkinsDefaultPainters, dxMDBar, Vcl.OleCtrls, SHDocVw;

type
  TSBasePreviewUrlFrm = class(TSBaseFrm)
    WebBrowser: TWebBrowser;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InternalInit; override;
  end;

implementation

{$R *.dfm}

uses SBaseConstants, SBaseVariantFunctions;

{ TSBasePreviewUrlForm }

procedure TSBasePreviewUrlFrm.InternalInit;
var
  LIdx: Integer;
begin
  inherited;

  if CreateFormParams.Find(SConst_Url, LIdx) then begin
    WebBrowser.Navigate(VarToStr(CreateFormParams.Items[LIdx].Value));
  end;
end;

initialization
  RegisterClass(TSBasePreviewUrlFrm);

finalization
  UnRegisterClass(TSBasePreviewUrlFrm);

end.
