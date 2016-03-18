unit cxMDGridReg;

interface
uses
  Classes;

  procedure Register;

implementation

uses cxMDGrid;

procedure Register;
begin
  RegisterComponents ('MD Project Tools', [TcxMDGrid, TcxMDGridLevel, TcxMDGridDBTableView, TcxMDGridDBBandedTableView]);
end;

end.
