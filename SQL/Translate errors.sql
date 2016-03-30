SELECT * 
-- DELETE TV
FROM [Base].[Translate:Fields] TF
INNER JOIN [Base].[Translate:Values] TV ON TF.[Id] = TV.[Field_Id]
LEFT JOIN [Base].[Dic:Instruments:Places] P ON TV.[Key] = P.[Id]
WHERE TF.[Table_Id] = [Base].[Translate::Table::Id]('Dic:Instruments:Places')
AND P.[Id] IS NULL
