UPDATE T SET [Parent_Id] = P.[Id]
FROM [BackOffice].[Dic:Transactions:Types] T
INNER JOIN [BackOffice].[Dic:Transactions:Types] P ON Left(T.[Code], [Pub].[CharIndex(Back)](':', T.[Code], Default) - 1) = P.[Code] AND (T.[Parent_Id] IS NULL OR T.[Parent_Id] <> P.[Id])
--WHERE T.[Code] LIKE 'BDM:%'

