unit cxMDDBTreeListReg;

interface
uses
  Classes;

  procedure Register;

implementation

uses cxMDDBTreeList;

procedure Register;
begin
  RegisterComponents ('MD Project Tools', [TcxMDTreeList, TcxMDDBTreeList]);
end;

end.
