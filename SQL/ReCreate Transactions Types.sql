BEGIN TRY
  SELECT 
    [Name],[Code],[Firm_Id],[DecActiveState_Id],[IncActiveState_Id],[CanBeOrder],[CanBeConsolidatedTransaction],[CanBeSimpleTransaction],[NameRoot],[GUIRoot],[Description]
  INTO #TEMP
  FROM [BackOffice].[Dic:Transactions:Types]

  TRUNCATE TABLE [BackOffice].[Dic:Transactions:Types]

  INSERT INTO [BackOffice].[Dic:Transactions:Types]([Name],[Code],[Firm_Id],[DecActiveState_Id],[IncActiveState_Id],[CanBeOrder],[CanBeConsolidatedTransaction],[CanBeSimpleTransaction],[NameRoot],[GUIRoot],[Description])
  SELECT TOP 1000000 [Name],[Code],[Firm_Id],[DecActiveState_Id],[IncActiveState_Id],[CanBeOrder],[CanBeConsolidatedTransaction],[CanBeSimpleTransaction],[NameRoot],[GUIRoot],[Description]
  FROM #TEMP
  ORDER BY [Firm_Id], [Code]
  
  UPDATE T SET [Parent_Id] = P.[Id]
  FROM [BackOffice].[Dic:Transactions:Types] T
  INNER JOIN [BackOffice].[Dic:Transactions:Types] P ON Left(T.[Code], [Pub].[CharIndex(Back)](':', T.[Code], Default) - 1) = P.[Code] AND (T.[Parent_Id] IS NULL OR T.[Parent_Id] <> P.[Id])
END TRY
BEGIN CATCH
  EXEC [System].[ReRaise Error] @ProcedureName = 'Blank'
END CATCH
