CREATE TABLE [BackOffice].[Persons=Third Parties]
(
	[Person_Id]     [int]       NOT NULL,
	[ThirdParty_Id] [int]           NULL,
	[Firm_Id]       [tinyint]   NOT NULL,
--	[Deleted]       [bit]           NULL,
  [Code]          [nvarchar](20)  NULL,
  [NameInternal]  [nvarchar](256) NULL,
	[Identity]      [int]       NOT NULL,
CONSTRAINT [PK_BackOffice.Persons=Third Parties] PRIMARY KEY NONCLUSTERED 
(
	[Identity] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
) ON [PRIMARY]
GO
CREATE UNIQUE CLUSTERED INDEX [UQ_BackOffice.Persons=Third Parties#Person_Id#ThirdParty_Id#Firm_Id] ON [BackOffice].[Persons=Third Parties]
(
	[Person_Id]     ASC,
	[ThirdParty_Id] ASC,
	[Firm_Id]       ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
GO
CREATE NONCLUSTERED INDEX [IX_BackOffice.Persons=Third Parties#Firm_Id] ON [BackOffice].[Persons]
(
	[Firm_Id] ASC
)WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON [PRIMARY]
GO
ALTER TABLE [BackOffice].[Persons=Third Parties]  WITH CHECK ADD CONSTRAINT [FK_BackOffice.Persons=Third Parties_BackOffice.Firms]
FOREIGN KEY([Firm_Id])
REFERENCES [BackOffice].[Firms] ([Id])
GO
ALTER TABLE [BackOffice].[Persons=Third Parties] CHECK CONSTRAINT [FK_BackOffice.Persons=Third Parties_BackOffice.Firms]
GO
ALTER TABLE [BackOffice].[Persons=Third Parties] WITH CHECK ADD CONSTRAINT [FK_BackOffice.Persons=Third Parties_Base.Persons]
FOREIGN KEY([Person_Id])
REFERENCES [Base].[Persons] ([Id])
GO

ALTER TABLE [BackOffice].[Persons=Third Parties] CHECK CONSTRAINT [FK_BackOffice.Persons=Third Parties_Base.Persons]
GO

EXEC sys.sp_addextendedproperty @name=N'Description', @value=N'Третьи лица' , @level0type=N'SCHEMA',@level0name=N'BackOffice', @level1type=N'TABLE',@level1name=N'Persons=Third Parties'
GO
EXEC sys.sp_addextendedproperty @name=N'DescriptionScript', @value=N'''«'' + [Base].[Person](%TABLE%.[Person_Id], Default, Default, Default) + ''» в «'' + [BackOffice].[Firm](%TABLE%.[Firm_Id], Default, Default) + ''»''' , @level0type=N'SCHEMA',@level0name=N'BackOffice', @level1type=N'TABLE',@level1name=N'Persons=Third Parties'
GO
EXEC sys.sp_addextendedproperty @name=N'Description', @value=N'Уникальное сочетание «Персона» + «3е лицо» + «Фирма»' , @level0type=N'SCHEMA',@level0name=N'BackOffice', @level1type=N'TABLE',@level1name=N'Persons=Third Parties', @level2type=N'INDEX',@level2name=N'UQ_BackOffice.Persons=Third Parties#Person_Id#ThirdParty_Id#Firm_Id'
GO


