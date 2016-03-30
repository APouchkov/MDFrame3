--SET IDENTITY_INSERT [BackOffice].[Dic:Transactions:Types] ON

DROP TABLE #TEMP

SELECT
  [Id],
  [Identity] = ROW_NUMBER() OVER (ORDER BY [Code]),
  [Code]
INTO #TEMP
FROM [BackOffice].[Dic:Transactions:Types] ORDER BY [Code]

SELECT * FROM #TEMP

UPDATE T SET
  [Id] = I.[Identity],
  [Parent_Id] = P.[Identity]
FROM [BackOffice].[Dic:Transactions:Types] T
INNER JOIN #TEMP I ON T.[Id] = I.[Id]
LEFT JOIN #TEMP P ON T.[Parent_Id] = P.[Id]
