unit SBaseObjectInspectorDialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxOI, cxVGrid, cxLookAndFeels, cxControls, cxContainer, cxEdit,
  cxTextEdit, cxHyperLinkEdit, StdCtrls, ExtCtrls, cxGraphics,
  cxLookAndFeelPainters, cxMaskEdit, cxDropDownEdit, cxGroupBox,
  cxButtons, cxMDLookupTreeComboBox, Data.DB, MemDS, VirtualTable, dxBar,
  cxLookupEdit, cxDBLookupEdit, cxDBLookupComboBox, FieldDataLink, Menus,
  dxSkinsCore, dxSkinsDefaultPainters;

type
  TSBaseObjectInspectorDlg = class(TForm)
    gbComponents: TcxGroupBox;
    pButtons: TPanel;
    btnFastScript: TcxButton;
    btnDfmCurrent: TcxButton;
    btnDfmLoaded: TcxButton;
    btnHelper: TcxButton;
    DataSet: TVirtualTable;
    DataSource: TDataSource;
    ltcbComponents: TcxMDLookupTreeComboBox;
    DataSetId: TIntegerField;
    DataSetParent_Id: TIntegerField;
    DataSetName: TStringField;
    DataSetCanSelect: TBooleanField;
    lcbComponents: TcxLookupComboBox;
    btnObjectList: TcxButton;
    btnFocusedControl: TcxButton;
//    procedure cbComponentsPropertiesEditValueChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

{$IFDEF DEBUG}
    procedure btnFastScriptClick(Sender: TObject);
    procedure btnDfmLoadedClick(Sender: TObject);
    procedure btnDfmCurrentClick(Sender: TObject);
    procedure btnHelperClick(Sender: TObject);
    procedure btnFocusedControlClick(Sender: TObject);
{$ENDIF}
    procedure ltcbComponentsPropertiesEditValueChanged(Sender: TObject);
    procedure lcbComponentsPropertiesEditValueChanged(Sender: TObject);
    procedure ComponentsPropertiesEditValueChanged(AComponent: TComponent);

    procedure btnObjectListClick(Sender: TObject);
  private
    FObjectInspector: TcxRTTIInspector;
//    FInspectedObjectChanged: TNotifyEvent;
    function GetInspectedObject: TPersistent;
    procedure SetInspectedObject(const Value: TPersistent);
//    procedure DoObjectInspectorChange(Sender: TObject);
    procedure DoObjectInspectorFilterProperty(Sender: TObject; const PropertyName: string; var Accept: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    property InspectedObject: TPersistent read GetInspectedObject write SetInspectedObject;
//    property OnInspectedObjectChanged: TNotifyEvent read FInspectedObjectChanged write FInspectedObjectChanged;
  end;

implementation

{$R *.dfm}

uses
  SBaseConstants, SBaseDfmView, NamedVariables,
  SBaseScriptDebuggerForm, SBaseForm, SBaseDataSetInspectorDialog,
  SBaseDataModule, SBaseVariantFunctions, SBaseControls, SBaseUtils,
  cxGridCustomView, DataHandler;

type
  TSBaseFrmCrack = class(TSBaseFrm);

{ TfrmInspector }

constructor TSBaseObjectInspectorDlg.Create(AOwner: TComponent);
var
  I: Integer;
  LComponent: TComponent;
  LParent: Integer;
  LSelf, LName: String;
  LDataSources, LDataSets, LControllers: Boolean;
begin
  inherited Create(AOwner);
  btnObjectList.OptionsImage.Images := SBaseFormProperties.Icon14List;
  btnObjectList.OptionsImage.ImageIndex := SBaseFormProperties.Icon14Index_NodeRoot;

  btnFocusedControl.OptionsImage.Images := SBaseFormProperties.Icon14List;
  btnFocusedControl.OptionsImage.ImageIndex := SBaseFormProperties.Icon14Index_Find;

{$IFDEF DEBUG}
  if Owner is TSBaseFrm then begin
    pButtons.Visible := True;
    if Assigned(TSBaseFrmCrack(Owner).FScript) then
      btnFastScript.OnClick := btnFastScriptClick
    else
      btnFastScript.Enabled := False;

    btnDfmLoaded.OnClick  := btnDfmLoadedClick;
    btnDfmCurrent.OnClick := btnDfmCurrentClick;
    btnHelper.OnClick := btnHelperClick;
    btnFocusedControl.OnClick := btnFocusedControlClick;
  end;
{$ENDIF}

{$IFDEF VERTGRID}
  pnlBottom.Visible := False;
{$ENDIF}
  fObjectInspector := TcxRTTIInspector.Create(self);
  fObjectInspector.Parent := Self;
  fObjectInspector.Align := alClient;
  fObjectInspector.OptionsView.RowHeaderWidth := 160;
  fObjectInspector.OnFilterProperty := DoObjectInspectorFilterProperty;

  DataSet.Clear;
  DataSet.Active := True;

  if Owner.Name <> '' then
    LSelf := Owner.Name
  else
    LSelf := SConst_Self;
  LSelf := '∙ ' + LSelf + ': ' + Owner.ClassName;

  DataSet.AppendRecord([Integer(Owner), NULL, LSelf, NULL]);
  LDataSources  := False;
  LDataSets     := False;
  LControllers  := False;

  for I := 0 to Owner.ComponentCount - 1 do begin
    LComponent := Owner.Components[I];
    if (LComponent <> Self) and (LComponent.Name <> '') then begin
      LName := LComponent.Name + ': ' + LComponent.ClassName;

      if LComponent is TdxBarPopupMenu then
        LParent := Integer(TdxBarPopupMenu(LComponent).BarManager)
      else if LComponent is TdxBarItem then
        LParent := Integer(TdxBarItem(LComponent).BarManager)
      else if LComponent is TcxCustomGridView then begin
        LParent := Integer(TcxCustomGridView(LComponent).Level);
        if LParent = 0 then
          LParent := Integer(TcxCustomGridView(LComponent).GetParentComponent)
      end else if LComponent is TcxCustomRow then begin
        LParent := Integer(TcxCustomRow(LComponent).Parent);
        if LParent = 0 then
          LParent := Integer(TcxCustomRow(LComponent).VerticalGrid)
      end else if (LComponent is TDataSource) and not (LComponent is TCustomDataHandler) then begin
        if not LDataSources then begin
          DataSet.AppendRecord([-1, Integer(Owner), '∙ ' + SConst_DataSources, False]);
          LDataSources := True;
        end;
        LParent := -1;
      end else if LComponent is TDataSet then begin
        if not LDataSets then begin
          DataSet.AppendRecord([-2, Integer(Owner), '∙ ' + SConst_DataSets, False]);
          LDataSets := True;
        end;
        LParent := -2;
      end else if LComponent is TField then
        LParent := Integer(TField(LComponent).DataSet.Source)
      else if (LComponent is TSBaseCustomLookupComponentControl)
           or (LComponent is TMasterDetailDataSetControl)
           or (LComponent is TFieldDataLinkController)
      then begin
        if not LControllers then begin
          DataSet.AppendRecord([-3, Integer(Owner), '∙ ' + SConst_Controllers, False]);
          LControllers := True;
        end;
        LParent := -3;
      end else if LComponent.HasParent then
        LParent := Integer(LComponent.GetParentComponent)
      else
        LParent := 0;

      if LParent = 0 then
        LParent := Integer(LComponent.Owner);

      DataSet.AppendRecord([Integer(LComponent), LParent, LName, NULL]);
    end;
  end;

  btnFocusedControl.Click;
  if not VarIsPresent(ltcbComponents.EditValue) then
    ltcbComponents.EditValue := Integer(Owner);
end;

{$IFDEF DEBUG}
procedure TSBaseObjectInspectorDlg.btnDfmLoadedClick(Sender: TObject);
begin
  TSBaseDfmView.CreateByParams
  (
    Self,
    TNamedVariants.Create
    (
      [
        TNamedVariant.Create(SConst_DFM, TSBaseFrm(Owner).DfmText)
      ],
      True
    )
  ).ShowModal;
end;

procedure TSBaseObjectInspectorDlg.btnDfmCurrentClick(Sender: TObject);
var
  LComponent: TComponent;
begin
  if ltcbComponents.Visible then
    LComponent := TComponent(Integer(ltcbComponents.EditValue))
  else
    LComponent := TComponent(Integer(lcbComponents.EditValue));

  if Assigned(LComponent) then
    TSBaseDfmView.CreateByParams
    (
      Self,
      TNamedVariants.Create
      (
        [
          TNamedVariant.Create(SConst_Caption, Name + ' (' + ClassName + ')'),
          TNamedVariant.Create(SConst_DFM, LComponent.DFM)
        ],
        True
      )
    ).ShowModal;
end;

procedure TSBaseObjectInspectorDlg.btnFastScriptClick(Sender: TObject);
begin
  if Owner.InheritsFrom(TSBaseFrm) then
    TSBaseFrm(Owner).ActionScriptDebuggerExecute
  else
    TSBaseScriptDebuggerFrm.CreateByParams(Owner, TNamedVariants.Create(True)).ShowModal
end;

procedure TSBaseObjectInspectorDlg.btnFocusedControlClick(Sender: TObject);
var
  LComponent: TComponent;
begin
  if (Owner is TCustomForm) then begin
    LComponent := TCustomForm(Owner).ActiveControl;
    if not Assigned(LComponent) then Exit;
    if LComponent is TcxGridSite then
      LComponent := TcxGridSite(LComponent).GridView;

    while Assigned(LComponent) do begin
      if LComponent.Name <> '' then Break;
      LComponent := LComponent.Owner;
    end;

    if Assigned(LComponent) and DataSet.Locate(SConst_Id, Integer(LComponent), []) then
      if ltcbComponents.Visible then
        ltcbComponents.EditValue := Integer(LComponent)
      else
        lcbComponents.EditValue := Integer(LComponent)
  end;
end;

procedure TSBaseObjectInspectorDlg.btnHelperClick(Sender: TObject);
var
  LDataSet: Integer;
begin
  LDataSet := ltcbComponents.EditValue;
  if TComponent(Pointer(LDataSet)) is TDataSource then begin
    LDataSet := Integer(TDataSource(Pointer(LDataSet)).DataSet.Source);
    if LDataSet = 0 then
      Exit;
  end;

  TSBaseDataSetInspectorDlg.CreateByParams
  (
    Self,
    TNamedVariants.Create
    (
      [
        TNamedVariant.Create(SConst_DataSet, LDataSet)
      ],
      True
    )
  ).ShowModal;
end;

{$ENDIF}

(*
procedure TSBaseObjectInspectorDlg.cbComponentsPropertiesEditValueChanged(Sender: TObject);
var
  I: Integer;
begin
  I := lcbComponents.Properties.Items.IndexOf(lcbComponents.EditValue);
  if I >= 0 then
    fObjectInspector.InspectedObject := TComponent(lcbComponents.Properties.Items.Objects[I])
  else
    fObjectInspector.InspectedObject := nil;
end;

procedure TSBaseObjectInspectorDlg.DoObjectInspectorChange(Sender: TObject);
begin
  if Assigned(FInspectedObjectChanged) then
    FInspectedObjectChanged(Sender);
end;
*)

procedure TSBaseObjectInspectorDlg.DoObjectInspectorFilterProperty(Sender: TObject; const PropertyName: string; var Accept: Boolean);
begin
  if (Pos('Fake', PropertyName) > 0) or (Pos('CustomDataSource', PropertyName) > 0) then
    Accept := False
  else
    Accept := True;
end;

procedure TSBaseObjectInspectorDlg.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree
end;

function TSBaseObjectInspectorDlg.GetInspectedObject: TPersistent;
begin
  Result := fObjectInspector.InspectedObject;
end;

procedure TSBaseObjectInspectorDlg.SetInspectedObject(const Value: TPersistent);
begin
  FObjectInspector.InspectedObject := Value;
end;

procedure TSBaseObjectInspectorDlg.lcbComponentsPropertiesEditValueChanged(Sender: TObject);
begin
  if not lcbComponents.Visible then Exit;
  ComponentsPropertiesEditValueChanged(TComponent(Pointer(Integer(lcbComponents.EditValue))));
end;

procedure TSBaseObjectInspectorDlg.ltcbComponentsPropertiesEditValueChanged(Sender: TObject);
begin
  if not ltcbComponents.Visible then Exit;
  ComponentsPropertiesEditValueChanged(TComponent(Pointer(Integer(ltcbComponents.EditValue))));
end;

procedure TSBaseObjectInspectorDlg.ComponentsPropertiesEditValueChanged(AComponent: TComponent);
begin
  fObjectInspector.InspectedObject := AComponent;
{$IFDEF DEBUG}
  btnHelper.Visible := (AComponent is TDataSet)
                      or ((AComponent is TDataSource) and Assigned(TDataSource(AComponent).DataSet));
{$ENDIF}
end;

procedure TSBaseObjectInspectorDlg.btnObjectListClick(Sender: TObject);
begin
  if ltcbComponents.Visible then begin
    ltcbComponents.Visible := False;
    ltcbComponents.Properties.ListSource := nil;
    DataSet.Filtered := True;

    lcbComponents.Properties.ListSource := DataSource;
    lcbComponents.EditValue := ltcbComponents.EditValue;
    lcbComponents.Parent := gbComponents;
    lcbComponents.Align := alClient;

    lcbComponents.Visible := True;
    btnObjectList.OptionsImage.ImageIndex := SBaseFormProperties.Icon14Index_NodeParent;
  end else begin
    lcbComponents.Visible := False;
    lcbComponents.Properties.ListSource := nil;
    DataSet.Filtered := False;

    ltcbComponents.Properties.ListSource := DataSource;
    ltcbComponents.EditValue := lcbComponents.EditValue;
    ltcbComponents.Parent := gbComponents;
    ltcbComponents.Visible := True;
    btnObjectList.OptionsImage.ImageIndex := SBaseFormProperties.Icon14Index_NodeRoot;
  end;
end;

end.
