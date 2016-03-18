unit cxMDDBLookupComboBoxReg;

interface
uses
  Classes;

  procedure Register;

implementation

uses cxMDDBLookupComboBox;

procedure Register;
begin
  RegisterComponents ('MD Project Tools', [TcxMDDBLookupComboBox]);
end;

end.
