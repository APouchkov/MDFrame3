INSERT INTO [BackOffice].[Consolidated Transactions:Numerating]
SELECT DISTINCT
  [Firm_Id],
  [FirmType_Id],
  [DateTime],
  [Type_Id]
FROM [BackOffice].[Consolidated Transactions]
WHERE [Firm_Id] = 1 AND [FirmType_Id] = 16 AND [Number] IS NULL
