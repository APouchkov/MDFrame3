unit SBaseFieldEditForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, FMTBcd,
  Dialogs, StdCtrls, Mask, DB, SBaseForm, NamedVariables,
  cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters, cxContainer,
  cxEdit, cxTextEdit, cxDBEdit, cxMemo, cxGroupBox, cxMaskEdit, cxDropDownEdit,
  cxLookupEdit, cxDBLookupEdit, cxDBLookupComboBox, cxLabel, cxCalc, ExtCtrls,
  cxCalendar, cxSpinEdit, cxTimeEdit, cxCheckBox, cxUserCheckBox, Math, dxBar, LayoutPanel,
  ComCtrls, dxCore, cxDateUtils, System.Actions, ActnList, cxClasses,
  dxSkinsCore, dxSkinsDefaultPainters, dxMDBar;

type
  TSBaseFieldEditFrm = class(TSBaseFrm)
    GroupBox: TcxGroupBox;
    pType: TLayoutPanel;
    cbType: TcxComboBox;
    pValue: TLayoutPanel;
    eMemoEdit: TcxMemo;
    eCalcEdit: TcxCalcEdit;
    eDateEdit: TcxDateEdit;
    eTimeEdit: TcxTimeEdit;
    eStringEdit: TcxTextEdit;
    eBooleanEdit: TcxUserCheckBox;
    eSpinEdit: TcxSpinEdit;
    pButtons: TPanel;
    btnOk: TButton;
    btnCancel: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure cbTypePropertiesChange(Sender: TObject);
    procedure FormShow(Sender: TObject); override;
    procedure eBooleanEditPropertiesChange(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }
    FField: TField;
    FcxEdit: TcxCustomEdit;
  protected
    { Protected declarations }
    procedure InternalInit; override;
    procedure AfterSkinChanged; override;
  public
    { Public declarations }
    procedure Show; override;
    class function Edit(AOwner: TForm; AField: TField; ACaption: String = ''): Boolean;
  end;

function StringFieldEdit (AOwner: TForm; var Value: String  ; Caption: String = ''; TextWidth: Integer = 200): Boolean;
function TextFieldEdit   (AOwner: TForm; var Value: String  ; Caption: String = ''; TextWidth: Integer = 300; TextHeight: Integer = 200; ReadOnly: Boolean = False): Boolean;
//function FloatFieldEdit  (AOwner: TForm; var Value: Extended; DecimalPlaces: Byte = 5; Caption: String = ''): Boolean;
//function IntegerFieldEdit(AOwner: TForm; var Value: Integer ; Caption: String = ''; Style: TEditButtonStyleEh = ebsGlyphEh): Boolean;

implementation

uses
  SBaseConstants, SBaseVariantFunctions, SBaseDataModule, SBaseStringFunctions;

{$R *.dfm}

procedure TSBaseFieldEditFrm.InternalInit;
var
  LIdx: Integer;
  LReadOnly: Boolean;
  LValue: Variant;
  LText, LFieldType: String;
begin
  inherited;

  LText := VarToStr(CreateFormParams[SConst_Title]);
  if (not LText.IsEmpty) then
    Caption := LText;

  LReadOnly := (CreateFormParams[SConst_ReadOnly] = True);
  if CreateFormParams.Find(SConst_Field, LIdx) then begin
    FField := TField(Integer(CreateFormParams.Items[LIdx].Value));
    CreateFormParams.Delete(LIdx);
    LReadOnly := LReadOnly or (FField.FieldKind = fkCalculated) or (not FField.CanModify);
    if not LReadOnly then
      FField.DataSet.Edit;


    case FField.DataType of
      ftSmallint, ftInteger, ftWord, ftLargeint:
        LFieldType := DelphiTypeInteger;
      ftFloat, ftExtended, ftCurrency:
        LFieldType := DelphiTypeExtended;
      ftBCD, ftFMTBCD:
        LFieldType := DelphiTypeFMTBcd;
      ftDate:
        LFieldType := DelphiTypeDate;
      ftTime:
        LFieldType := DelphiTypeTime;
      ftDateTime:
        LFieldType := DelphiTypeDateTime;
      ftMemo, ftWideMemo:
        LFieldType := DelphiTypeText;
      ftString, ftWideString:
        LFieldType := DelphiTypeString;
      ftBoolean:
        LFieldType := DelphiTypeBoolean;
      ftVariant:
        LFieldType := '';
      else
        Assert(False);
    end;

    LValue := FField.AsVariant;
  end else begin
    LFieldType      := VarToStr(CreateFormParams[SConst_Type]);
    LValue          := CreateFormParams[SConst_Value];
  end;

  if LFieldType.IsEmpty then begin
    case VarType(LValue) of
      varByte, varWord, varSmallint, varShortInt, varInteger, varInt64, varLongWord, varUInt64:
        LFieldType := DelphiTypeInteger;
      varSingle, varDouble, varCurrency, varDecimal:
        LFieldType := DelphiTypeExtended;
      varDate:
        if Floor(LValue) = LValue then
          LFieldType := DelphiTypeDate
        else if LValue < 1.0 then
          LFieldType := DelphiTypeTime
        else
          LFieldType := DelphiTypeDateTime;
      varString, varUString, varNull:
        LFieldType := DelphiTypeString;
      varBoolean:
        LFieldType := DelphiTypeBoolean;
      else
        if VarIsFMTBcd(LValue) then
          LFieldType := DelphiTypeFMTBcd
        else
          Assert(False);
    end;

    GroupBox.Padding.SetBounds(0, 3, 5, 3);
    GroupBox.Constraints.MinHeight := pType.Height + pValue.Height;
    pType.Visible   := True;
  end else begin
    pValue.Caption.Left := 0;
    pValue.Caption.Width := 0;
  end;

  Width := VarToIntDef(CreateFormParams[SConst_Width], Width);

  cbType.EditValue := LFieldType;
  if LReadOnly then begin
    FcxEdit.ActiveProperties.ReadOnly := True;
    FcxEdit.Style.Color := SBaseFormProperties.dxDefaultStyles.ReadOnly.Color;
//    ReadOnly            := LReadOnly;
    btnCancel.Visible   := False;
    btnOk.Cancel        := True;
  end;

  FcxEdit.EditValue := LValue;
end;

procedure TSBaseFieldEditFrm.AfterSkinChanged;
begin
  inherited;

  Constraints.MaxHeight := 0;
  AdjustSize(GroupBox);
  if FcxEdit <> eMemoEdit then
    Constraints.MaxHeight := Height;
end;

procedure TSBaseFieldEditFrm.Show;
begin
  inherited ShowModal;
end;

function StringFieldEdit(AOwner: TForm; var Value: String; Caption: String; TextWidth: Integer): Boolean;
var
  LResult: TNamedVariants;
begin
  LResult := TSBaseFieldEditFrm.Select
             (
                AOwner,
                [
                  TNamedVariant.Create(SConst_Type, DelphiTypeString),
                  TNamedVariant.Create(SConst_Title, Caption),
                  TNamedVariant.Create(SConst_Width, TextWidth),
                  TNamedVariant.Create(SConst_Value, Value)
                ]
             );

  Result := (LResult.Count = 1);
  if Result then
    Value := VarToStr(LResult.Items[0].Value);
end;

function TextFieldEdit(AOwner: TForm; var Value: String; Caption: String = ''; TextWidth: Integer = 300; TextHeight: Integer = 200; ReadOnly: Boolean = False): Boolean;
var
  LResult: TNamedVariants;
begin
  LResult := TSBaseFieldEditFrm.Select
              (
                AOwner,
                [
                  TNamedVariant.Create(SConst_Type, DelphiTypeText),
                  TNamedVariant.Create(SConst_Title, Caption),
                  TNamedVariant.Create(SConst_Width, TextWidth),
                  TNamedVariant.Create(SConst_Height, TextHeight),
                  TNamedVariant.Create(SConst_ReadOnly, ReadOnly),
                  TNamedVariant.Create(SConst_Value, Value)
                ]
              );
  Result := (LResult.Count = 1);
  if Result then
    Value := LResult.Items[0].Value;
end;

class function TSBaseFieldEditFrm.Edit(AOwner: TForm; AField: TField; ACaption: String = ''): Boolean;
var
  LResult: TNamedVariants;
begin
  LResult := TSBaseFieldEditFrm.Select
             (
                AOwner,
                [
                  TNamedVariant.Create(SConst_Field, Integer(AField)),
                  TNamedVariant.Create(SConst_Title, ACaption)
                ]
             );
  Result := (LResult.Count = 1);
end;

procedure TSBaseFieldEditFrm.FormShow(Sender: TObject);
begin
  inherited;
  FcxEdit.SetFocus;
end;

procedure TSBaseFieldEditFrm.btnCancelClick(Sender: TObject);
begin
  if (not FcxEdit.ActiveProperties.ReadOnly) and Assigned(FField) and FField.DataSet.Editing then begin
    FField.DataSet.Cancel;
  end;
  Close;
end;

procedure TSBaseFieldEditFrm.btnOkClick(Sender: TObject);
var
  LValue: Variant;
  SValue: String;
begin
  if FcxEdit.ActiveProperties.ReadOnly then
    btnCancelClick(nil)
  else begin
    FcxEdit.PostEditValue;

    LValue := FcxEdit.EditValue;
    if VarIsPresent(LValue) then
      if (FcxEdit = eStringEdit) or (FcxEdit = eMemoEdit) then begin
        SValue := VarToStr(LValue).Trim;
        if SValue.IsEmpty then
          LValue := Null;
      end;

    if (not VarIsPresent(LValue)) and not (CreateFormParams[SConst_NullAble] = True) then
      WarningMessage(SError_ControlMustHaveValue, FcxEdit);

    if Assigned(FField) then begin
      if FField.DataSet.Editing then begin
        FField.AsVariant := LValue;
        FField.DataSet.Post;
        ResultValues[SConst_Result] := True;
      end;
    end else begin
      ResultValues[SConst_Value] := LValue;
      if not ResultFields.IsEmpty then
        ResultValues := PrepareParameters(ResultFields, ResultValues, GetFormParamValues);
    end;

    if fsModal in FormState then
      ModalResult := mrOk
    else
      Close;
  end;
end;

procedure TSBaseFieldEditFrm.cbTypePropertiesChange(Sender: TObject);
var
  LFieldType: String;
  LValue: Variant;
begin
  LFieldType := cbType.EditValue;
  if Assigned(FcxEdit) then begin
    LValue := FcxEdit.EditValue;
    FcxEdit.Visible := False;
    FcxEdit.Parent := nil;
  end else
    LValue := Null;

  if LFieldType = DelphiTypeString then begin
    FcxEdit           := eStringEdit;
  end else if LFieldType = DelphiTypeText then begin
    FcxEdit           := eMemoEdit;
  end else if SameText(LFieldType, DelphiTypeExtended) or SameText(LFieldType, DelphiTypeFloat) then begin
    FcxEdit           := eCalcEdit;
    eCalcEdit.Properties.DisplayFormat  := '0,.' + StringOfChar('#', VarToIntDef(CreateFormParams[SConst_DecimalPlaces], 8));
  end else if SameText(LFieldType, DelphiTypeInteger) then begin
    if (CreateFormParams[SConst_SpinEdit] = True) then begin
      FcxEdit           := eSpinEdit;
    end else begin
      FcxEdit           := eCalcEdit;
      eCalcEdit.Properties.DisplayFormat  := '0,';
    end;
  end else if SameText(LFieldType, DelphiTypeDate) then begin
    FcxEdit           := eDateEdit;
    eDateEdit.Properties.Kind := ckDate;
  end else if SameText(LFieldType, DelphiTypeDateTime) then begin
    FcxEdit           := eDateEdit;
    eDateEdit.Properties.Kind := ckDateTime;
  end else if SameText(LFieldType, DelphiTypeTime) then begin
    FcxEdit           := eTimeEdit;
  end else if SameText(LFieldType, DelphiTypeBoolean) then begin
    FcxEdit           := eBooleanEdit;
    eBooleanEdit.Properties.AllowGrayed := (CreateFormParams[SConst_NullAble] = True);
    if (not eBooleanEdit.Properties.AllowGrayed) and (not VarIsType(LValue, varBoolean)) then
      LValue := False;
  end else
    Assert(False);

  if FcxEdit = eMemoEdit then begin
    pValue.Align := alClient;
    Constraints.MaxHeight := 0;
    Height := VarToIntDef(CreateFormParams[SConst_Height], 200);
  end else begin
    pValue.Align := alTop;
    pValue.ClientHeight := FcxEdit.Height;
    if HandleAllocated then
      AfterSkinChanged
  end;

  FcxEdit.Parent    := pValue;
  FcxEdit.Align     := alClient;
  FcxEdit.Visible   := True;
  FcxEdit.EditValue := LValue;
end;

procedure TSBaseFieldEditFrm.eBooleanEditPropertiesChange(Sender: TObject);
begin
  eBooleanEdit.Caption := eBooleanEdit.Properties.GetDisplayText(eBooleanEdit.EditValue, True, False)
end;

initialization
  RegisterClass(TSBaseFieldEditFrm);

finalization
  UnRegisterClass(TSBaseFieldEditFrm);

end.
