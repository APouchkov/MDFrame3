unit cxLayoutDataGroupBoxReg;

interface
uses
  Classes, cxLayoutDataGroupBox, DB;

  procedure Register;

implementation

uses
  DesignEditors, DesignIntf;

type
  TcxLayoutDataGroupBoxFilterComponentProperty = class(TComponentProperty)
  private
    FProc: TGetStrProc;
    procedure CheckComponent(const Value: string);
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;


procedure Register;
begin
  RegisterComponents ('BackOffice', [TcxLayoutDataGroupBox]);
  RegisterPropertyEditor(TypeInfo(TComponent), TcxLayoutDataGroupBox, 'FilterComponent',
    TcxLayoutDataGroupBoxFilterComponentProperty);
end;

{ TcxLayoutDataGroupBoxFilterComponentProperty }

procedure TcxLayoutDataGroupBoxFilterComponentProperty.GetValues(Proc: TGetStrProc);
begin
  FProc := Proc;
  inherited GetValues(CheckComponent);
end;

procedure TcxLayoutDataGroupBoxFilterComponentProperty.CheckComponent(const Value: string);
var
  LComponent: TComponent;
begin
  LComponent := Designer.GetComponent(Value);
  if (LComponent <> nil)
    and
    (
      (LComponent is TDataSet)
      or
      (LComponent is TDataSource)
    )
  then
    FProc(Value);
end;

end.
