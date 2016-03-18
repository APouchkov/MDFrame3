unit SBasePreviewTxtForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Graphics,
  Controls, Forms, Dialogs, SBaseForm, System.Actions, ActnList,
  dxBar, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxContainer, cxEdit, cxTextEdit, cxMemo, cxClasses;

type
  TSBasePreviewTxtFrm = class(TSBaseFrm)
    Memo: TcxMemo;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure InternalInit; override;
  end;

implementation

{$R *.dfm}

uses SBaseConstants, SBaseVariantFunctions;

{ TSBasePreviewTxtFrm }

procedure TSBasePreviewTxtFrm.InternalInit;
var
  LIdx: Integer;
  LStream: TStream;
begin
  inherited;
  if CreateFormParams.Find(SConst_Stream, LIdx) then begin
    LStream := VarToPointer(CreateFormParams.Items[LIdx].Value);
    Memo.Lines.LoadFromStream(LStream);
  end;
end;

end.
