IF  EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[BackOffice].[Transactions]') AND name = N'IX_BackOffice.Transactions')
DROP INDEX [IX_BackOffice.Transactions] ON [BackOffice].[Transactions] WITH ( ONLINE = OFF )
GO
CREATE CLUSTERED INDEX [IX_BackOffice.Transactions] ON [BackOffice].[Transactions] 
(
	[Account_Id] ASC,
	[DateTime] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON, DATA_COMPRESSION = PAGE) 
ON [TDateTimePartitionScheme]([DateTime])
GO
------------------------------------------------
DROP PARTITION SCHEME [TDateTimePartitionScheme]
GO
DROP PARTITION FUNCTION [TDateTimePartition]
GO
CREATE PARTITION FUNCTION [TDateTimePartition](datetime2(0)) AS RANGE RIGHT 
FOR VALUES
(
  N'2014-01-01T00:00:00.000', N'2014-02-01T00:00:00.000', N'2014-03-01T00:00:00.000', N'2014-04-01T00:00:00.000', N'2014-05-01T00:00:00.000', N'2014-06-01T00:00:00.000', N'2014-07-01T00:00:00.000', N'2014-08-01T00:00:00.000', N'2014-09-01T00:00:00.000', N'2014-10-01T00:00:00.000', N'2014-11-01T00:00:00.000', N'2014-12-01T00:00:00.000'
, N'2015-01-01T00:00:00.000', N'2015-02-01T00:00:00.000', N'2015-03-01T00:00:00.000', N'2015-04-01T00:00:00.000', N'2015-05-01T00:00:00.000', N'2015-06-01T00:00:00.000', N'2015-07-01T00:00:00.000', N'2015-08-01T00:00:00.000', N'2015-09-01T00:00:00.000', N'2015-10-01T00:00:00.000', N'2015-11-01T00:00:00.000', N'2015-12-01T00:00:00.000'
, N'2016-01-01T00:00:00.000', N'2016-02-01T00:00:00.000', N'2016-03-01T00:00:00.000', N'2016-04-01T00:00:00.000', N'2016-05-01T00:00:00.000', N'2016-06-01T00:00:00.000', N'2016-07-01T00:00:00.000', N'2016-08-01T00:00:00.000', N'2016-09-01T00:00:00.000', N'2016-10-01T00:00:00.000', N'2016-11-01T00:00:00.000', N'2016-12-01T00:00:00.000'
, N'2017-01-01T00:00:00.000', N'2017-02-01T00:00:00.000', N'2017-03-01T00:00:00.000', N'2017-04-01T00:00:00.000', N'2017-05-01T00:00:00.000', N'2017-06-01T00:00:00.000', N'2017-07-01T00:00:00.000', N'2017-08-01T00:00:00.000', N'2017-09-01T00:00:00.000', N'2017-10-01T00:00:00.000', N'2017-11-01T00:00:00.000', N'2017-12-01T00:00:00.000'
, N'2018-01-01T00:00:00.000', N'2018-02-01T00:00:00.000', N'2018-03-01T00:00:00.000', N'2018-04-01T00:00:00.000', N'2018-05-01T00:00:00.000', N'2018-06-01T00:00:00.000', N'2018-07-01T00:00:00.000', N'2018-08-01T00:00:00.000', N'2018-09-01T00:00:00.000', N'2018-10-01T00:00:00.000', N'2018-11-01T00:00:00.000', N'2018-12-01T00:00:00.000'

)
GO
CREATE PARTITION SCHEME [TDateTimePartitionScheme] AS PARTITION [TDateTimePartition] TO ([PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY], [PRIMARY])
GO
------------------------------------------------
IF  EXISTS (SELECT * FROM sys.indexes WHERE object_id = OBJECT_ID(N'[BackOffice].[Transactions]') AND name = N'IX_BackOffice.Transactions')
DROP INDEX [IX_BackOffice.Transactions] ON [BackOffice].[Transactions] WITH ( ONLINE = OFF )
GO
CREATE CLUSTERED INDEX [IX_BackOffice.Transactions] ON [BackOffice].[Transactions] 
(
	[Account_Id] ASC,
	[DateTime] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON, DATA_COMPRESSION = PAGE) 
ON [TDateTimePartitionScheme]([DateTime])
GO
