unit cxMDVGridReg;

interface
uses
  Classes;

  procedure Register;

implementation

uses cxMDVGrid;

procedure Register;
begin
  RegisterComponents ('MD Project Tools', [TcxMDDBVerticalGrid]);
end;

end.
