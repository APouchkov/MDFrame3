unit cxStaticLabelReg;

interface
uses
  Classes;

  procedure Register;

implementation

uses cxStaticLabel;

procedure Register;
begin
  RegisterComponents ('MD Project Tools', [TcxStaticLabel]);
end;

end.
