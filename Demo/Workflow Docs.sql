-- ������������ WorkFlow
CREATE TABLE [Workflow].[Users]
(
	[Id]    [tinyint]       NOT NULL,
	[Code]  [varchar](50)   NOT NULL, -- ��� ������������
	[Name]  [nvarchar](200) NOT NULL, -- ������������
	[Login] [nvarchar](128) NOT NULL, -- SQL-�����, ������� ����� ��� ��� �������� ��� "*" ��� ������ ��� ��������
) ON [PRIMARY]
GO
-- �������/�������� WorkFlow
CREATE TABLE [Workflow].[Objects]
(
	[Id]                      [smallint] IDENTITY(1,1) NOT NULL,
	[Code]                    [varchar](50)   NOT NULL,   -- ��� ��������
	[Name]                    [nvarchar](200) NOT NULL,   -- ������������
	[Modes]                   [varchar](3)    NOT NULL,   -- ��������� ������
	                                                      -- [
	                                                      --    D - ������ ������ (��� �������� ��������),
	                                                      --    S - ������ � ���������� ������
	                                                      --    A - ������ � ����������� ������
	                                                      -- ]
	[IntermediateParamNames]  [varchar](4000)     NULL,   -- ��������� ��� �������� �� ���������� �������� (��� ������������� ���������)
	[IntermediateDataNames]   [varchar](4000)     NULL,   -- ������ ��� �������� �� ���������� �������� (��� ������������� ���������)
) ON [PRIMARY]
GO
-- ����� ������� �� �������� ��������
CREATE TABLE [Workflow].[Objects:SubScriptions]
(
	[User_Id]   [tinyint]   NOT NULL, -- ������ �� ������������
	[Object_Id] [smallint]  NOT NULL, -- ������ �� �������
) ON [PRIMARY]
GO
-- ����� ���������� ��������
CREATE TABLE [Workflow].[Objects:Items]
(
	[Object_Id]               [smallint]      NOT NULL, -- ������ �� �������
	[Id]                      [tinyint]       NOT NULL, -- �� ������ ��������
	[Description]             [nvarchar](200) NOT NULL, -- �������� �����
	[Procedure]               [sysname]           NULL, -- SQL ���������-���������� (����� ���� NULL)
	[Transaction]             [bit]           NOT NULL, -- ��������� �� ������� �������������� ����������
	[RepeateCondition]        [nvarchar](1000)    NULL, -- ������� ����������
	[RepeateCount]            [smallint]          NULL, -- ���-�� ����������
	[RepeateDelay]            [varchar](10)       NULL, -- �������� ����������
	[RepeateOverrunConstants] [TParams]           NULL, -- ��������� ��� ���������� � SavedParams ��� ������������� ������� ����������
	[RepeateOverrunData]      [xml]               NULL, -- ������ ��� ���������� � SavedData ��� ������������� ������� ����������
	[SaveParamNames]          [varchar](4000)     NULL, -- ����� ���������� �� OutParams ������� ���������� � SavedParams
	[SaveDataNames]           [varchar](4000)     NULL, -- ����� ����� ������ �� OutData ������� ���������� � SavedData
) ON [PRIMARY]
GO
-- ��������� (��� ��������� [Params]) ������������� XML ��� ����� � [Create] ��� [Continue]
CREATE TABLE [Workflow].[Objects:Params]
(
	[Object_Id]   [smallint]      NOT NULL, -- ������ �� �������
	[Item_Id]     [tinyint]       NOT NULL, -- ������ �� �� ����� ��������
	[Name]        [varchar](128)  NOT NULL, -- ��� ��������� (���� ���������� � "@", �� ��� ������� �������� �������� XML)
	[Type]        [sysname]       NOT NULL, -- ��� ���������
	[Description] [nvarchar](200) NOT NULL, -- ��������
	[Required]    [bit]           NOT NULL, -- ������� �������������� �������
	[Default]     [nvarchar](4000)    NULL, -- �������� ��-���������
) ON [PRIMARY]
GO
-- ������� ��������� ����� ������� ���������
CREATE TABLE [Workflow].[Objects:Steps]
(
	[Object_Id]         [smallint]      NOT NULL, -- ������ �� �������
	[Id]                [smallint]      NOT NULL, -- �� ������� ��������
	[ParentItem_Id]     [tinyint]           NULL, -- ������ �� ���������� ����� (NULL - ��� �������� ��������)
	[NextItem_Id]       [tinyint]           NULL, -- ������ �� ��������� ����� (NULL - ��� ���������� ��������)
	[HookItem_Id]       [tinyint]           NULL, -- ������ �� �����-������� (������� �������� ���������� �������, ����� ������ �� ��������)
	[HookUser_Id]       [tinyint]           NULL, -- ������ �� ������������, �������� ����� ���������� �������-�������
	[Description]       [nvarchar](200)     NULL, -- �������� ��������
	[User_Id]           [tinyint]           NULL, -- ������ ������� �� ������������
	[Mode]              [char](1)           NULL, -- ������ ������� �� ������ ������
	[Condition]         [nvarchar](1000)    NULL, -- ������ ������� �� �������� ��������� [Params]
	[WaitFor]           [varchar](10)       NULL, -- �������� ��� ��������
	[Async]             [bit]           NOT NULL, -- ����������� �������
  -- ��������� ���������� ��� ��������� ��������� �����
	[QueueParamNames]   [varchar](4000)     NULL, -- �� ���������� ������� ��������
	[QueueDataNames]    [varchar](4000)     NULL, -- �� ������ ������� ��������
	[SavedParamNames]   [varchar](4000)     NULL, -- �� ����������, ���������� ��� ������ ��������
	[SavedDataNames]    [varchar](4000)     NULL, -- �� ������, ���������� ��� ������ ��������
	[ParentParamNames]  [varchar](4000)     NULL, -- �� ����������, ��������� �� ���������� �����
	[ParentDataNames]   [varchar](4000)     NULL, -- �� ������, ��������� �� ���������� �����
	[Constants]         [TParams]           NULL, -- ���������
) ON [PRIMARY]
GO

-------------------------------------------------------------
-- ��������� �������
-------------------------------------------------------------

-- ������������������ ��������
CREATE TABLE [Workflow].[Queue]
(
	[Id]              [int] IDENTITY(1,1) NOT NULL,
	[Object_Id]       [smallint]    NOT NULL,
	[User_Id]         [tinyint]     NOT NULL,
	[Mode]            [char](1)     NOT NULL,
	[Identity]        [varchar](50)     NULL,
	[Parent_Id]       [int]             NULL,
	[Params]          [TParams]         NULL,
	[Data]            [xml]             NULL,
	[CreateDateTime]  [datetime]    NOT NULL,
	[AsyncThreads]    [tinyint]         NULL,
	[SavedData]       [xml]             NULL,
	[SavedParams]     [TParams]         NULL,
) ON [PRIMARY]
GO
