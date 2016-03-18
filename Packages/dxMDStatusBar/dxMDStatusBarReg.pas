unit dxMDStatusBarReg;

interface
uses
  Classes;

  procedure Register;

implementation

uses dxMDStatusBar;

procedure Register;
begin
  RegisterComponents ('MD Project Tools', [TdxMDStatusBar]);
end;

end.
