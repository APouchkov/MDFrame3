unit dxMDBarReg;

interface

uses
  DesignEditors, DesignIntf,
  dxCore, dxBar, Classes, Windows, ImgList, cxClasses, dxBarStrs, cxControls,
  SysUtils, Math, dxBarAccessibility, cxLookAndFeels;

  procedure Register;

implementation

uses
  dxMDBarButton, dxBarPopupMenuEd, dxMDBar;

type
  TdxBarMDItemLinksPropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

procedure Register;
begin
  RegisterComponents('MD Project Tools', [TdxMDBarManager, TdxMDBarDockControl]);

  RegisterNoIcon([TdxMDBar]);
  RegisterNoIcon([TdxMDBarButton, TdxMDBarListItem]);

  RegisterPropertyEditor(TypeInfo(TdxBarItemLinks), TdxMDBarButton, 'ItemLinks', TdxBarMDItemLinksPropertyEditor);
end;

{ TdxBarMDItemLinksPropertyEditor }

procedure TdxBarMDItemLinksPropertyEditor.Edit;
begin
  if not (GetComponent(0) is TdxBar) then
    ShowdxBarSubMenuEditor(TdxBarItemLinks(GetOrdValue));
end;

function TdxBarMDItemLinksPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly];
  if not (GetComponent(0) is TdxBar) then Include(Result, paDialog);
end;

function TdxBarMDItemLinksPropertyEditor.GetValue: string;
begin
  Result := Format('(%s)', [TdxBarItemLinks.ClassName]);
end;

end.
