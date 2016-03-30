EXEC [Notifications].[Table Changes::Begin]
EXEC [BackOffice].[Log::Begin] 1
INSERT INTO [BackOffice].[Deals:Numerating]
(
  [Firm_Id]
 ,[DateTime]
 ,[UpperOrganization_Id]
 ,[Place_Id]
 ,[Type_Id]
 ,[Id]
)
SELECT
 [Firm_Id]
 ,[DateTime]
 ,[UpperOrganization_Id]
 ,[Place_Id]
 ,[Type_Id]
 ,[Id]
FROM [BackOffice].[Deals]
WHERE [Firm_Id] = 1 AND [Date] = '20141105'

SELECT
 [Firm_Id]
 ,[DateTime]
 ,[Number]
 ,[UpperOrganization_Id]
 ,[Place_Id]
 ,[Type_Id]
 ,[Id]
FROM [BackOffice].[Deals]
WHERE [Firm_Id] = 1 AND [Date] = '20141105'




