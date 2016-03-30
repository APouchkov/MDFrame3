EXEC [BackOffice].[Log::Begin] @Skip = 1

--UPDATE [BackOffice].[Accounts] SET [Counter] = 0
--WHERE Firm_Id = 1 AND FirmType_Id = 12

DECLARE @Data XML 

--SET @Data =
--(
--  SELECT
--    [Id],
--    [ACTION]  = 'U',
--    [FIELDS]  = 'Number',
--    (
--      SELECT
--        [DATE]    = OpenDate, 
--        [Number]  = ''
--      FOR XML RAW('PERIODIC'), TYPE
--    )
--  FROM [BackOffice].[Accounts]
--  WHERE Firm_Id = 1 AND FirmType_Id = 12
--  FOR XML RAW('RECORD')
--)

--EXEC [BackOffice].[Accounts(Periodic Set)] @Data = @Data

SET @Data =
(
  SELECT
    [Id],
    [ACTION]  = 'U',
    [FIELDS]  = 'Number',
    (
      SELECT
        [DATE]    = OpenDate, 
        [Number]  = Left([Number], Len([Number]) - 2) + '0' + Right([Number], 2)
      FOR XML RAW('PERIODIC'), TYPE
    )
  FROM
  (
    SELECT DISTINCT Person_Id FROM [BackOffice].[Accounts]
    WHERE Firm_Id = 1 AND FirmType_Id = 12 AND [Counter] > 99
  ) I
  INNER JOIN [BackOffice].[Accounts] A ON I.[Person_Id] = A.[Person_Id] AND A.Firm_Id = 1 AND A.FirmType_Id = 12 AND A.[Counter] < 100
  FOR XML RAW('RECORD')
)

EXEC [BackOffice].[Accounts(Periodic Set)] @Data = @Data

--SELECT * FROM [BackOffice].[Accounts]
--WHERE Firm_Id = 1 AND FirmType_Id = 12
--ORDER BY Person_Id, Number

