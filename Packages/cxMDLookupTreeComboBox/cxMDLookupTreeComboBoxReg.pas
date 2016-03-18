unit cxMDLookupTreeComboBoxReg;

interface
uses
  Classes;

  procedure Register;

implementation

uses cxMDLookupTreeComboBox;

procedure Register;
begin
  RegisterComponents
  (
    'MD Project Tools',
    [
      TcxMDLookupTreeComboBox,
      TcxMDDBLookupTreeComboBox,
      TcxMDMultiFieldsDBLookupTreeComboBox,
      TcxMDCheckLookupTreeComboBox,
      TcxMDDBCheckLookupTreeComboBox
    ]
  );
end;

end.
