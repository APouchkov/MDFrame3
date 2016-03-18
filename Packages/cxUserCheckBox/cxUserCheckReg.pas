unit cxUserCheckReg;

interface
uses
  Classes;

  procedure Register;

implementation

uses cxUserCheckBox, cxUserCheckGroup;

procedure Register;
begin
  RegisterComponents ('MD Project Tools', [TcxUserCheckBox, TcxDBUserCheckBox]);
  RegisterComponents ('MD Project Tools', [TcxUserCheckGroup]);
end;

end.
