unit SBaseSelectDateTimeForm;
(************************************************************
 * Code edited by FESS©
 * Time: 08.07.2009 18:00
 ************************************************************)

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ExtCtrls, SBaseForm, cxControls,
  cxContainer, cxEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxCalendar,
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, dxBar, cxLabel, cxButtons,
  cxTimeEdit, LayoutPanel;

type
  TSBaseSelectDateTimeFrm = class(TSBaseFrm)
    MainPanel   : TPanel;
    LayoutPanel : TLayoutPanel;
    EditValue   : TcxCustomMaskEdit;
    ButtonOk    : TcxButton;
    procedure ButtonOkClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
  protected
    procedure InternalCreate; override;
  public
  end;

  TSBaseSelectDateFrm = class(TSBaseSelectDateTimeFrm);
  TSBaseSelectTimeFrm = class(TSBaseSelectDateTimeFrm);

implementation

uses SBaseConstants, SBaseVariantFunctions, SBaseUtils, NamedVariables;

procedure TSBaseSelectDateTimeFrm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = VK_ESCAPE then
    Close;
end;

procedure TSBaseSelectDateTimeFrm.InternalCreate;
var
  Idx: Integer;
begin
  Self.FormStyle := fsNormal;
  Self.BorderStyle := bsToolWindow;
  Self.BorderIcons := [biSystemMenu];
  Self.Position := poMainFormCenter;
  Self.OnKeyDown := FormKeyDown;
  Self.KeyPreview := True;

  MainPanel := TPanel.Create(Self);
  MainPanel.Parent := Self;
  MainPanel.Align := alClient;
  MainPanel.Caption := '';
  MainPanel.BevelOuter := bvLowered;

  LayoutPanel := TLayoutPanel.Create(Self);
  LayoutPanel.Parent := MainPanel;
  LayoutPanel.Align := alLeft;

  if (Self is TSBaseSelectDateFrm) then begin
    EditValue := TcxDateEdit.Create(Self);
    LayoutPanel.Caption.Text := SBaseProperties.Text_Date;
    LayoutPanel.Caption.Width := 60;
    LayoutPanel.Width := LayoutPanel.Caption.Width + 85;
    TcxDateEdit(EditValue).Properties.Kind:= ckDate;
    TcxDateEdit(EditValue).Properties.ShowTime := False;
    TcxDateEdit(EditValue).Properties.SaveTime := False;
  end else if Self is TSBaseSelectTimeFrm then begin
    EditValue := TcxTimeEdit.Create(Self);
    LayoutPanel.Caption.Text := SBaseProperties.Text_Time;
    LayoutPanel.Caption.Width := 60;
    LayoutPanel.Width := LayoutPanel.Caption.Width + 75;
    TcxTimeEdit(EditValue).Properties.ShowDate := False;
  end else if (Self is TSBaseSelectDateTimeFrm) then begin
    EditValue := TcxDateEdit.Create(Self);
    LayoutPanel.Caption.Text := SBaseProperties.Text_DateAndTime;
    LayoutPanel.Caption.Width := 90;
    LayoutPanel.Width := LayoutPanel.Caption.Width + 130;
    TcxDateEdit(EditValue).Properties.Kind:= ckDateTime;
    TcxDateEdit(EditValue).Properties.ShowTime := True;
    TcxDateEdit(EditValue).Properties.SaveTime := True;
  end;

  EditValue.Parent := LayoutPanel;
  EditValue.Align := alClient;
  EditValue.TabOrder := 1;
  EditValue.TabStop := True;

  if CreateFormParams.Find(SConst_Default, Idx) then
    EditValue.EditValue := CreateFormParams.Items[Idx].Value
  else if (Self is TSBaseSelectTimeFrm) then
    EditValue.EditValue := Now - Date
  else if (Self is TSBaseSelectDateFrm) then
    EditValue.EditValue := Date
  else if (Self is TSBaseSelectDateTimeFrm) then
    EditValue.EditValue := Now;

  if CreateFormParams.Find(SConst_Caption, Idx) then
    Caption := CreateFormParams.Items[Idx].Value;

  ButtonOk := TcxButton.Create(Self);
  ButtonOk.Parent := MainPanel;
  ButtonOk.OnClick := Self.ButtonOkClick;
  ButtonOk.Align := alClient;

  Self.ClientHeight := 21 + 2;
  Self.ClientWidth := LayoutPanel.Width + ButtonOk.Width + 4;

  ButtonOk.Anchors := [akTop, akRight];
  ButtonOk.TabOrder := 0;
  ButtonOk.TabStop := False;
  ButtonOk.Default := True;
  ButtonOk.Caption := 'Ok';

  inherited;
end;

procedure TSBaseSelectDateTimeFrm.ButtonOkClick(Sender: TObject);
var
  LValue  : Variant;
begin
  EditValue.PostEditValue;
  LValue := VarFromDateTime(EditValue.EditValue);

  if not VarIsNull(LValue) then begin
    //ResultValues.UnknownAsNull := True;
    ResultValues[SConst_Value] := LValue;
    if not ResultFields.IsEmpty then
      ResultValues := PrepareParameters(ResultFields, ResultValues, GetFormParamValues);

    ModalResult := mrOk;
  end;
end;

initialization
  RegisterClass(TSBaseSelectDateTimeFrm);
  RegisterClass(TSBaseSelectDateFrm);
  RegisterClass(TSBaseSelectTimeFrm);

finalization
  UnRegisterClass(TSBaseSelectTimeFrm);
  UnRegisterClass(TSBaseSelectDateFrm);
  UnRegisterClass(TSBaseSelectDateTimeFrm);

end.
