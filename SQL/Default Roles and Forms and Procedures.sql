EXEC [SQL].[Default::Objects Roles & Privileges]
  @ObjectName         = '[BackOffice].[Dic:Deals:Numerations]',
  @SecurityModes      = 'TABLE',
  @SecurityParams     = 'Firm_Id=TinyInt',
  @SecurityPrivileges = 'VELES-CAP=Firm_Id=1;VELES-MAN=Firm_Id=2;VELES-INT=Firm_Id=3;BASE-A=Firm_Id=4;KENVAY=Firm_Id=5;GROUP-CYP=Firm_Id=12;GROUP-IK=Firm_Id=13;GROUP-VIP=Firm_Id=11',
  @Interfaces         = 0

RETURN


--EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm]
--  @ParentClassName      = 'TSBaseCxTreeViewFrm',
--  @IdentityParams       = 'Firm_Id=TinyInt',
--  @ObjectName           = '[BackOffice].[Persons]',
--  @KeyField             = 'Id',
--  @ParentField          = NULL,
--  @ObjectNameSingle     = '[BackOffice].[Person]',
--  @Category             = '����������',
--  @Caption              = '������������ ����',
--  @ObjectViewClassName  = 'TfrmBackOfficePersonView',
--  @ObjectViewIdentityParams = 'Firm_Id=TinyInt;Type_Id=Char(1)',
--  @DirectDelete         = 0

--EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm]
--  @ParentClassName      = 'TSBaseCxTreeViewFrm',
--  @IdentityParams       = 'Firm_Id=TinyInt;FirmType_Id=TinyInt',
--  @ObjectName           = '[BackOffice].[Accounts]',
--  @KeyField             = 'Id',
--  @ParentField          = NULL,
--  @ObjectNameSingle     = '[BackOffice].[Account]',
--  @Category             = '����������',
--  @Caption              = '������������ ����',
--  @ObjectViewClassName  = 'TfrmBackOfficeAccountView',
--  @ObjectViewIdentityParams = 'Firm_Id=TinyInt;FirmType_Id=TinyInt',
--  @DirectDelete         = 0
  
  
--EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm]
--  @ParentClassName      = 'TSBaseCxView2Frm',
--  @IdentityParams       = 'OrganizationType_Id=TinyInt;Organization_Id=Int',
--  @ObjectName           = '[Base].[Persons:Accounts]',
--  @ObjectNameSingle     = '[Base].[Person:Account]',
--  @KeyField             = 'Id',
--  @Object2Name          = '[Base].[Persons:Accounts:Parts]',
--  @Object2NameSingle    = '[Base].[Person:Account:Part]',
--  @MasterFields2        = 'Id',
--  @DetailFields2        = 'Account_Id',
--  @Category             = '����������',
--  @Caption              = '�������� �����',
--  @ObjectViewClassName  = 'TfrmBasePersonAccountView',
--  @ObjectViewIdentityParams  = 'OrganizationType_Id=TinyInt',
--  @Object2ViewClassName = 'TfrmBasePersonAccountPartView',
--  @DirectDelete         = 0


--EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm]
--  @ParentClassName      = 'TSBaseCxViewFrm',
--  @IdentityParams       = NULL, --'Firm_Id=TinyInt',
--  @ObjectName           = '[Import].[Templates]',
--  @KeyField             = 'Id',
--  @ParentField          = NULL,
--  @ObjectNameSingle     = '[Import].[Template]',
--  @Category             = '�����������������',
--  @Caption              = '������� ������������� ����������',
--  @CaptionFormatSingle  = '������ �������������� ��������� � ������� :FileType',
--  @ObjectViewClassName  = 'TfrmImportTemplateView',
--  @ObjectViewIdentityParams = 'FileType_Id=TinyInt',
--  @DirectDelete         = 0


--EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm]
--  @ParentClassName      = 'TSBaseCxTreeViewFrm',
--  @IdentityParams       = 'Firm_Id=TinyInt',
--  @ObjectName           = '[BackOffice].[Dic:Accounts:Managers]',
--  @KeyField             = 'Id',
--  @ParentField          = NULL,
--  @ObjectNameSingle     = '[BackOffice].[Dic:Accounts:Manager]',
--  @Category             = '����������',
--  @Caption              = '������������ ����',
--  --@ObjectViewClassName  = 'TfrmBasePersonView',
--  @DirectDelete         = 0

EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm]
  @ParentClassName      = 'TSBaseCxViewFrm',
  --@ParentField          = 'Parent_Id',
  @IdentityParams       = 'Firm_Id=TinyInt',
  @ObjectName           = '[BackOffice].[Dic:Deals:Numerations]',
  @ObjectNameSingle     = '[BackOffice].[Dic:Deal:Numeration]',
  @KeyField             = 'Code',
  @Category             = '����������',
  @Caption              = '������� ��������� ������',
  @InsertableControl    = 0,
  @ObjectViewClassName  = NULL,
  @ObjectViewIdentityParams  = NULL,
  @DirectDelete         = 0


