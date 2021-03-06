IF @@SERVERNAME <> 'SRV-DEV02' RaisError('FATAL', 20, 1) WITH LOG
GO

TRUNCATE TABLE [Security].[User Roles(Cach)]
TRUNCATE TABLE [Security].[User Privileges(Cach)]
GO

EXEC [Notifications].[Table Changes::Begin]
EXEC [Base].[Log::Begin] 1
EXEC [BackOffice].[Log::Begin] 1

TRUNCATE TABLE [Export].[Inserted]
TRUNCATE TABLE [Export].[Updated]
TRUNCATE TABLE [Export].[Deleted]

TRUNCATE TABLE [Export].[Updated(Time)]

TRUNCATE TABLE [Export].[Waiting]
TRUNCATE TABLE [Export].[Waiting(Time)]

TRUNCATE TABLE [LOG].[DDL]
TRUNCATE TABLE [Notifications].[User Messages]
DELETE [Notifications].[Messages]

TRUNCATE TABLE [Base].[Log:Incoming:Records:Periodic Values]
TRUNCATE TABLE [Base].[Log:Incoming:Records:Values]
TRUNCATE TABLE [Base].[Log:Incoming:Records]
TRUNCATE TABLE [Base].[Log:Records:Values]
TRUNCATE TABLE [Base].[KLADR:Base]
TRUNCATE TABLE [Base].[KLADR:Doma]
TRUNCATE TABLE [Base].[KLADR:Streets]

DELETE [Base].[Log:Records]
TRUNCATE TABLE [Import].[BackOffice:Statements:Logs]

DELETE [GUI].[User Variables]
DELETE [GUI].[Forms:User Defaults]

TRUNCATE TABLE [BackOffice].[Log:Incoming:Records:Periodic Values]
TRUNCATE TABLE [BackOffice].[Log:Incoming:Records:Values]
TRUNCATE TABLE [BackOffice].[Log:Incoming:Records]
TRUNCATE TABLE [BackOffice].[Log:Records:Values]
TRUNCATE TABLE [BackOffice].[Alerts]
DELETE [BackOffice].[Log:Records]
GO
ALTER VIEW [BackOffice].[Transactions(Daily)#States]
---
--  WITH SCHEMABINDING
---
AS
  SELECT
    [Account_Id]                  = T.[Account_Id],
    [Date]                        = CAST(T.[DateTime] AS Date),
    [AccountPart_Id]              = T.[AccountPart_Id],
    [Instrument_Id]               = T.[Instrument_Id],
    [StorageClientRoot_Id]        = T.[StorageClientRoot_Id],
    [StorageIntermediaryRoot_Id]  = T.[StorageIntermediaryRoot_Id],
    [RemotePerson_Id]             = T.[RemotePerson_Id],
    [State_Id]                    = TT.[State_Id],
    [PlannedDate]                 = T.[PlannedDate],
    [PlannedInstrument_Id]        = T.[PlannedInstrument_Id],
    [Value]                       = SUM(T.[Value] * TT.[Sign]),
    [Count]                       = COUNT_BIG(*)
  FROM [BackOffice].[Transactions] T
  INNER JOIN [BackOffice].[Dic:Transactions:Types(Single)] TT ON T.[Type_Id] = TT.[Id]
  GROUP BY
    T.[Account_Id],
    CAST(T.[DateTime] AS Date),
    T.[AccountPart_Id],
    T.[Instrument_Id],
    T.[StorageClientRoot_Id],
    T.[StorageIntermediaryRoot_Id],
    T.[RemotePerson_Id],
    TT.[State_Id],
    T.[PlannedDate],
    T.[PlannedInstrument_Id]
GO
TRUNCATE TABLE [BackOffice].[Transactions]
GO
ALTER VIEW [BackOffice].[Transactions(Daily)#States]
---
  WITH SCHEMABINDING
---
AS
  SELECT
    [Account_Id]                  = T.[Account_Id],
    [Date]                        = CAST(T.[DateTime] AS Date),
    [AccountPart_Id]              = T.[AccountPart_Id],
    [Instrument_Id]               = T.[Instrument_Id],
    [StorageClientRoot_Id]        = T.[StorageClientRoot_Id],
    [StorageIntermediaryRoot_Id]  = T.[StorageIntermediaryRoot_Id],
    [RemotePerson_Id]             = T.[RemotePerson_Id],
    [State_Id]                    = TT.[State_Id],
    [PlannedDate]                 = T.[PlannedDate],
    [PlannedInstrument_Id]        = T.[PlannedInstrument_Id],
    [Value]                       = SUM(T.[Value] * TT.[Sign]),
    [Count]                       = COUNT_BIG(*)
  FROM [BackOffice].[Transactions] T
  INNER JOIN [BackOffice].[Dic:Transactions:Types(Single)] TT ON T.[Type_Id] = TT.[Id]
  GROUP BY
    T.[Account_Id],
    CAST(T.[DateTime] AS Date),
    T.[AccountPart_Id],
    T.[Instrument_Id],
    T.[StorageClientRoot_Id],
    T.[StorageIntermediaryRoot_Id],
    T.[RemotePerson_Id],
    TT.[State_Id],
    T.[PlannedDate],
    T.[PlannedInstrument_Id]
GO
CREATE UNIQUE CLUSTERED INDEX [PK_BackOffice.Transactions(Daily)#States] ON [BackOffice].[Transactions(Daily)#States] 
(
	[Account_Id] ASC,
	[Date] ASC,
	[AccountPart_Id] ASC,
	[Instrument_Id] ASC,
	[StorageClientRoot_Id] ASC,
	[StorageIntermediaryRoot_Id] ASC,
	[RemotePerson_Id] ASC,
	[State_Id] ASC,
	[PlannedDate] ASC,
	[PlannedInstrument_Id] ASC
) WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON) ON [PRIMARY]
GO
DISABLE TRIGGER ALL ON [BackOffice].[Consolidated Transactions]
GO
EXEC [Notifications].[Table Changes::Begin]
EXEC [BackOffice].[Log::Begin] 1

DECLARE @RowCount Int
SELECT TOP 1 @RowCount = 1 FROM [BackOffice].[Consolidated Transactions] WHERE [Parent_Id] IS NOT NULL
WHILE @@ROWCOUNT > 0
  DELETE TOP (100000) [BackOffice].[Consolidated Transactions] WHERE [Parent_Id] IS NOT NULL

SELECT TOP 1 @RowCount = 1 FROM [BackOffice].[Consolidated Transactions]
WHILE @@ROWCOUNT > 0
  DELETE TOP (100000) [BackOffice].[Consolidated Transactions]
GO
ENABLE TRIGGER ALL ON [BackOffice].[Consolidated Transactions]
GO

TRUNCATE TABLE [BackOffice].[Persons:Trusts:Rights]
TRUNCATE TABLE [BackOffice].[Persons:Trusts:Transferences]
TRUNCATE TABLE [BackOffice].[Trusts:Rights]

TRUNCATE TABLE [BackOffice].[Orders:Deals]
TRUNCATE TABLE [BackOffice].[Orders:Valuations]
TRUNCATE TABLE [BackOffice].[Orders:Other:Items]
GO
DISABLE TRIGGER ALL ON [BackOffice].[Trusts]
GO
DISABLE TRIGGER ALL ON [BackOffice].[Persons:Trusts]
GO
DISABLE TRIGGER ALL ON [BackOffice].[Orders:Other]
GO
DISABLE TRIGGER ALL ON [BackOffice].[Orders]
GO
EXEC [Notifications].[Table Changes::Begin]
EXEC [BackOffice].[Log::Begin] 1

DELETE [BackOffice].[Trusts]
DELETE [BackOffice].[Persons:Trusts]

UPDATE [BackOffice].[Accounts:Parts] SET [Order_Id] = NULL WHERE [Order_Id] IS NOT NULL
UPDATE [BackOffice].[Accounts] SET [Order_Id] = NULL WHERE [Order_Id] IS NOT NULL

DELETE [BackOffice].[Orders:Other]
DELETE [BackOffice].[Orders] WHERE [Parent_Id] IS NOT NULL

DECLARE @RowCount Int
SELECT TOP 1 @RowCount = 1 FROM [BackOffice].[Orders]
WHILE @@ROWCOUNT > 0
  DELETE TOP (100000) [BackOffice].[Orders]
GO
ENABLE TRIGGER ALL ON [BackOffice].[Trusts]
GO
ENABLE TRIGGER ALL ON [BackOffice].[Persons:Trusts]
GO
ENABLE TRIGGER ALL ON [BackOffice].[Orders:Other]
GO
ENABLE TRIGGER ALL ON [BackOffice].[Orders]


GO
ALTER VIEW [Base].[Instruments:Trade Prices#Basis]
---
--  WITH SCHEMABINDING
---
AS
  SELECT
    [Instrument_Id] = TP.[Instrument_Id],
    [Place_Id]      = T.[Place_Id],
    [Board_Id]      = TP.[Board_Id],
    [DateTime]      = T.[DateTime],
    --T.[Time],
    [Currency_Id]   = TP.[Currency_Id],
    [Value]         = TP.[Value]
  FROM [Base].[Instruments:Trade Prices] TP
  INNER JOIN [Base].[Instruments:Trades] T ON TP.[Trade_Id] = T.[Id]
  WHERE TP.[Type_Id] = 1
GO
ALTER VIEW [Base].[Instruments:Trade Prices#Boards]
---
--  WITH SCHEMABINDING
---
AS
  SELECT
    [Trade_Id]      = P.[Trade_Id],
    [Instrument_Id] = P.[Instrument_Id],
    [Board_Id]      = P.[Board_Id],
    [Count]         = COUNT_BIG(*)
  FROM [Base].[Instruments:Trade Prices] P
  GROUP BY 
    P.[Trade_Id],
    P.[Instrument_Id],
    P.[Board_Id]
GO
ALTER VIEW [Base].[Instruments:Trade Prices#Trades]
---
--  WITH SCHEMABINDING
---
AS
  SELECT
    [Instrument_Id] = P.[Instrument_Id],
    [Place_Id]      = T.[Place_Id],
    [Date]          = T.[Date],
    [Trade_Id]      = P.[Trade_Id],
    [Count]         = COUNT_BIG(*)
  FROM [Base].[Instruments:Trade Prices] P
  INNER JOIN [Base].[Instruments:Trades] T ON P.[Trade_Id] = T.[Id]
  GROUP BY 
    P.[Instrument_Id],
    T.[Place_Id],
    T.[Date],
    P.[Trade_Id]
GO
DISABLE TRIGGER ALL ON [Base].[Instruments:Trades]
GO
TRUNCATE TABLE [Base].[Instruments:Trade Prices]
DELETE [Base].[Instruments:Trades]
TRUNCATE TABLE [Base].[Instruments:Own Prices]
GO
ENABLE TRIGGER ALL ON [Base].[Instruments:Trades]
GO
ALTER VIEW [Base].[Instruments:Trade Prices#Basis]
---
  WITH SCHEMABINDING
---
AS
  SELECT
    [Instrument_Id] = TP.[Instrument_Id],
    [Place_Id]      = T.[Place_Id],
    [Board_Id]      = TP.[Board_Id],
    [DateTime]      = T.[DateTime],
    --T.[Time],
    [Currency_Id]   = TP.[Currency_Id],
    [Value]         = TP.[Value]
  FROM [Base].[Instruments:Trade Prices] TP
  INNER JOIN [Base].[Instruments:Trades] T ON TP.[Trade_Id] = T.[Id]
  WHERE TP.[Type_Id] = 1
GO
CREATE UNIQUE CLUSTERED INDEX [PK_Base.Instruments:Trade Prices#Basis] ON [Base].[Instruments:Trade Prices#Basis] 
(
	[Instrument_Id] ASC,
	[Place_Id] ASC,
	[Board_Id] ASC,
	[DateTime] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON, DATA_COMPRESSION = PAGE) ON [PRIMARY]
GO
ALTER VIEW [Base].[Instruments:Trade Prices#Boards]
---
  WITH SCHEMABINDING
---
AS
  SELECT
    [Trade_Id]      = P.[Trade_Id],
    [Instrument_Id] = P.[Instrument_Id],
    [Board_Id]      = P.[Board_Id],
    [Count]         = COUNT_BIG(*)
  FROM [Base].[Instruments:Trade Prices] P
  GROUP BY 
    P.[Trade_Id],
    P.[Instrument_Id],
    P.[Board_Id]
GO
CREATE UNIQUE CLUSTERED INDEX [PK_Base.Instruments:Trade Prices#Boards)] ON [Base].[Instruments:Trade Prices#Boards] 
(
	[Trade_Id] ASC,
	[Instrument_Id] ASC,
	[Board_Id] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON, DATA_COMPRESSION = PAGE) ON [PRIMARY]
GO
ALTER VIEW [Base].[Instruments:Trade Prices#Trades]
---
  WITH SCHEMABINDING
---
AS
  SELECT
    [Instrument_Id] = P.[Instrument_Id],
    [Place_Id]      = T.[Place_Id],
    [Date]          = T.[Date],
    [Trade_Id]      = P.[Trade_Id],
    [Count]         = COUNT_BIG(*)
  FROM [Base].[Instruments:Trade Prices] P
  INNER JOIN [Base].[Instruments:Trades] T ON P.[Trade_Id] = T.[Id]
  GROUP BY 
    P.[Instrument_Id],
    T.[Place_Id],
    T.[Date],
    P.[Trade_Id]
GO
CREATE UNIQUE CLUSTERED INDEX [PK_Base.Instruments:Trade Prices#Trades] ON [Base].[Instruments:Trade Prices#Trades] 
(
	[Instrument_Id] ASC,
	[Place_Id] ASC,
	[Date] ASC
)WITH (PAD_INDEX  = OFF, STATISTICS_NORECOMPUTE  = OFF, SORT_IN_TEMPDB = OFF, IGNORE_DUP_KEY = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS  = ON, ALLOW_PAGE_LOCKS  = ON, DATA_COMPRESSION = PAGE) ON [PRIMARY]
GO


TRUNCATE TABLE [BackOffice].[Deals:Actions:Contracts]
TRUNCATE TABLE [BackOffice].[Deals:Actions:Items]
TRUNCATE TABLE [BackOffice].[Deals:Actions:Reports]
TRUNCATE TABLE [BackOffice].[Deals:Actions:Requisites]
TRUNCATE TABLE [BackOffice].[Deals:Actions:Schedules]
GO
DISABLE TRIGGER ALL ON [BackOffice].[Deals:Items]
GO
DISABLE TRIGGER ALL ON [BackOffice].[Deals:Parts]
GO
DISABLE TRIGGER ALL ON [BackOffice].[Deals:Actions]
GO
DISABLE TRIGGER ALL ON [BackOffice].[Deals]
GO
  DECLARE @RowCount Int

  SELECT TOP 1 @RowCount = 1 FROM [BackOffice].[Deals:Items]
  WHILE @@ROWCOUNT > 0
    DELETE TOP (100000) [BackOffice].[Deals:Items]

  SELECT TOP 1 @RowCount = 1 FROM [BackOffice].[Deals:Actions]
  WHILE @@ROWCOUNT > 0
    DELETE TOP (100000) [BackOffice].[Deals:Actions]

  SELECT TOP 1 @RowCount = 1 FROM [BackOffice].[Deals:Parts]
  WHILE @@ROWCOUNT > 0
    DELETE TOP (100000) [BackOffice].[Deals:Parts]

  SELECT TOP 1 @RowCount = 1 FROM [BackOffice].[Deals]
  WHILE @@ROWCOUNT > 0
    DELETE TOP (100000) [BackOffice].[Deals]
GO
ENABLE TRIGGER ALL ON [BackOffice].[Deals:Items]
GO
ENABLE TRIGGER ALL ON [BackOffice].[Deals:Parts]
GO
ENABLE TRIGGER ALL ON [BackOffice].[Deals:Actions]
GO
ENABLE TRIGGER ALL ON [BackOffice].[Deals]
GO

DBCC SHRINKFILE (N'Data' , 0)
DBCC SHRINKFILE (N'Log' , 0)
