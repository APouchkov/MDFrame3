unit cxEditedFieldCheckBoxReg;

interface
uses
  Classes;

  procedure Register;

implementation

uses cxEditedFieldCheckBox;

procedure Register;
begin
  RegisterComponents ('MD Project Tools', [TcxEditedFieldCheckBox]);
end;

end.
