EXEC [SQL].[Default::Objects Roles & Privileges]
  @ObjectName         = '[BackOffice].[Firms]',
  @SecurityModes      = 'TABLE',
  @SecurityParams     = 'Firm_Id=TinyInt',
  @SecurityPrivileges = 'VELES-CAP=Firm_Id=1;VELES-MAN=Firm_Id=2;VELES-INT=Firm_Id=3;BASE-A=Firm_Id=4'

EXEC [SQL].[Default::Objects Roles & Privileges]
  @ObjectName         = '[BackOffice].[Firms]',
  @SecurityModes      = 'TABLE',
  @SecurityParams     = 'Firm_Id=TinyInt;FirmType_Id=TinyInt',
  @SecurityPrivileges = 'VELES-CAP=Firm_Id=1;VELES-MAN=Firm_Id=2;VELES-INT=Firm_Id=3;BASE-A=Firm_Id=4;VELES-CAP#DLR=Firm_Id=1 AND FirmType_Id=11;VELES-CAP#BRK=Firm_Id=1 AND FirmType_Id=12;VELES-CAP#M-SC=Firm_Id=1 AND FirmType_Id=13;VELES-CAP#DEP=Firm_Id=1 AND FirmType_Id=16;VELES-CAP#BIL=Firm_Id=1 AND FirmType_Id=18'

EXEC [SQL].[Default::Objects Roles & Privileges]
  @ObjectName         = '[BackOffice].[Orders::View]',
  @SecurityModes      = 'PROCEDURE',
  @SecurityParams     = 'Firm_Id=TinyInt;FirmType_Id=TinyInt',
  @SecurityPrivileges = 'VELES-CAP=Firm_Id=1;VELES-MAN=Firm_Id=2;VELES-INT=Firm_Id=3;BASE-A=Firm_Id=4;VELES-CAP#DLR=Firm_Id=1 AND FirmType_Id=11;VELES-CAP#BRK=Firm_Id=1 AND FirmType_Id=12;VELES-CAP#M-SC=Firm_Id=1 AND FirmType_Id=13;VELES-CAP#DEP=Firm_Id=1 AND FirmType_Id=16;VELES-CAP#BIL=Firm_Id=1 AND FirmType_Id=18'
