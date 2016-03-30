ALTER TABLE [BackOffice].[Dic:Actives:States] DISABLE TRIGGER [Dic:Actives:States (Translate Update)]
BEGIN TRAN

DELETE [BackOffice].[Translate:Values] WHERE [Field_Id] = [BackOffice].[Translate::Field::Id]('Dic:Actives:States', 'Name')

DECLARE
  @Id     TinyInt,
  @New_Id TinyInt

DECLARE @TT TABLE([Id] SmallInt NOT NULL PRIMARY KEY CLUSTERED, [DecActiveState_Id] TinyInt, [IncActiveState_Id] TinyInt)

DECLARE ITEMS_CURSOR CURSOR FAST_FORWARD FOR
  WITH CTE AS
  (
    SELECT
      *
    FROM
    (
      SELECT
        [Id],
        [New:Id] = ROW_NUMBER() OVER (ORDER BY CASE WHEN [Id] = 0 THEN 0 WHEN [Parent_Id] IS NULL THEN 1 ELSE 2 END, [Code]) - 1
      FROM [BackOffice].[Dic:Actives:States]
    ) I
    WHERE [Id] <> [New:Id]
  )
  SELECT
    T1.[Id],
    [New:Id] = CASE WHEN T2.[Id] IS NOT NULL THEN 100 + T1.[New:Id] ELSE T1.[New:Id] END
  -- , T2.*
  FROM CTE T1
  LEFT JOIN CTE T2 ON T1.[New:Id] > T1.[Id] AND T1.[New:Id] = T2.[Id]
  -- WHERE T2.[Id] IS NULL
  ORDER BY CASE
            WHEN T1.[New:Id] < T1.[Id] THEN 1
            ELSE 2
           END

  OPEN ITEMS_CURSOR
  WHILE (1 = 1) BEGIN
    FETCH NEXT FROM ITEMS_CURSOR
    INTO @Id, @New_Id
    IF @@FETCH_STATUS <> 0 BREAK

    DELETE @TT
    UPDATE TT SET
      [DecActiveState_Id] = NULL,
      [IncActiveState_Id] = NULL
    OUTPUT DELETED.[Id], DELETED.[DecActiveState_Id], DELETED.[IncActiveState_Id] INTO @TT([Id], [DecActiveState_Id], [IncActiveState_Id])
    FROM [BackOffice].[Dic:Transactions:Types] TT
    WHERE [DecActiveState_Id] = @Id OR [IncActiveState_Id] = @Id

    UPDATE [BackOffice].[Dic:Actives:States] SET
      [Id]        = CASE WHEN [Id] = @Id THEN @New_Id ELSE [Id] END,
      [Parent_Id] = CASE WHEN [Parent_Id] = @Id THEN @New_Id ELSE [Parent_Id] END
    WHERE [Id] = @Id OR [Parent_Id] = @Id

    UPDATE TT SET
      [DecActiveState_Id] = CASE WHEN A.[DecActiveState_Id] = @Id THEN @New_Id ELSE A.[DecActiveState_Id] END,
      [IncActiveState_Id] = CASE WHEN A.[IncActiveState_Id] = @Id THEN @New_Id ELSE A.[IncActiveState_Id] END
    FROM @TT A
    INNER JOIN [BackOffice].[Dic:Transactions:Types] TT ON A.[Id] = TT.[Id]
  END
  CLOSE ITEMS_CURSOR
  DEALLOCATE ITEMS_CURSOR


UPDATE [BackOffice].[Dic:Actives:States(Translate Update)] SET [Name] = [Name], [Row:Fields] = 'Name' WHERE [Translate:Language] = 'RU'
--ROLLBACK TRAN
COMMIT TRAN

ALTER TABLE [BackOffice].[Dic:Actives:States] ENABLE TRIGGER [Dic:Actives:States (Translate Update)]


/*
UPDATE TT SET [IncActiveState_Id] = S.[Id]
FROM [BackOffice].[Dic:Transactions:Types] TT
INNER JOIN [SRV-DEV01\FUTURE].[BackOffice.Test].[BackOffice].[Dic:Transactions:Types] FTT ON TT.[Id] = FTT.[Id]
INNER JOIN [SRV-DEV01\FUTURE].[BackOffice.Test].[BackOffice].[Dic:Actives:States] FS ON FTT.[IncActiveState_Id] = FS.[Id]
INNER JOIN [BackOffice].[Dic:Actives:States] S ON FS.[Code] = S.[Code] AND FS.[Id] <> S.[Id]
*/
