unit dxMDWizardControlReg;

interface
uses
  Classes,
  dxCustomWizardControl,
  dxWizardControlReg;

  procedure Register;

type
  { TdxMDWizardControlComponentEditor }

  TdxMDWizardControlComponentEditor = class(TdxWizardControlCustomComponentEditor)
  protected
    function GetWizardControl: TdxCustomWizardControl; override;
  public
    procedure AddPage; override;
  end;

  { TdxMDWizardControlPageComponentEditor }

  TdxMDWizardControlPageComponentEditor = class(TdxWizardControlCustomComponentEditor)
  protected
    function GetWizardControl: TdxCustomWizardControl; override;
  public
    procedure AddPage; override;
  end;


implementation

uses
  DesignIntf,
  dxMDWizardControl,
  dxCoreReg,
  cxLibraryReg;

procedure Register;
begin
  RegisterComponents('MD Project Tools', [TdxMDWizardControl]);
  RegisterClasses([TdxMDWizardControlPage]);
  //---------------------------------
  RegisterNoIcon([TdxMDWizardControlPage]);
  RegisterComponents(dxCoreLibraryProductPage, [TdxMDWizardControl]);
  RegisterComponentEditor(TdxMDWizardControl, TdxMDWizardControlComponentEditor);
  RegisterComponentEditor(TdxMDWizardControlPage, TdxMDWizardControlPageComponentEditor);
  HideClassProperties(TdxMDWizardControl, ['ActivePage']);
end;

{ TdxMDWizardControlComponentEditor }

procedure TdxMDWizardControlComponentEditor.AddPage;
var
  APage: TdxWizardControlCustomPage;
begin
  APage := WizardControl.AddPage(TdxMDWizardControlPage);
  APage.Name := Designer.UniqueName(TdxMDWizardControlPage.ClassName);
  Designer.SelectComponent(APage);
end;

function TdxMDWizardControlComponentEditor.GetWizardControl: TdxCustomWizardControl;
begin
  Result := Component as TdxMDWizardControl;
end;

{ TdxMDWizardControlPageComponentEditor }

procedure TdxMDWizardControlPageComponentEditor.AddPage;
var
  APage: TdxWizardControlCustomPage;
begin
  APage := WizardControl.AddPage(TdxMDWizardControlPage);
  APage.Name := Designer.UniqueName(TdxMDWizardControlPage.ClassName);
  Designer.SelectComponent(APage);
end;

function TdxMDWizardControlPageComponentEditor.GetWizardControl: TdxCustomWizardControl;
begin
  Result := (Component as TdxMDWizardControlPage).WizardControl;
end;

end.
