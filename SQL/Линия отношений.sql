DECLARE
  @DateBegin  Date = '20050101',
  @DateEnd    Date = '20051231'

DECLARE @Contracts TABLE(Person_Id Int, [Date] Date, CancelDate Date, [Type_Id] TinyInt)
INSERT INTO @Contracts VALUES
(1, '20010101', '20050201', 1),
(1, '20050111', '20050331', 1),
(1, '20050601', '20050701', 1),
(1, '20020601', '20050901', 2),
(2, '20050102', '20050831', 1)

SELECT
  R.[Person_Id],
  R.[Type_Id],
  [From] = R.[Prior],
  [To]   = R.[Date]
FROM
(
  SELECT
    R.[Person_Id],
    R.[Type_Id],
    R.[Date],
    R.[Prior],
    R.[Next],
    [RowNumber] = ROW_NUMBER() OVER(PARTITION BY R.[Person_Id], R.[Type_Id] ORDER BY R.[Date])
  FROM
  (
    SELECT
      R.[Person_Id],
      R.[Type_Id],
      R.[Date],
      [Prior] = LAG(R.[Date]) OVER (PARTITION BY R.[Person_Id], R.[Type_Id] ORDER BY R.[Date]),
      [Next]  = LEAD(R.[Date]) OVER (PARTITION BY R.[Person_Id], R.[Type_Id] ORDER BY R.[Date])
    FROM
    (
      SELECT DISTINCT
        D.[Person_Id],
        D.[Type_Id],
        C.[Date]
      FROM @Contracts D
      INNER JOIN [System].[Calendar] C ON C.[Date] >= @DateBegin AND C.[Date] <= @DateEnd
                        AND D.[Date] <= C.[Date] AND (D.[CancelDate] IS NULL OR D.[CancelDate] >= C.[Date])
      WHERE D.[Date] <= @DateEnd AND (D.[CancelDate] IS NULL OR D.[CancelDate] >= @DateBegin)
    ) R
  ) R
  WHERE R.[Prior] IS NULL OR DATEDIFF(Day, R.[Prior], R.[Date]) > 1
     OR R.[Next] IS NULL OR DATEDIFF(Day, R.[Date], R.[Next]) > 1
) R
WHERE R.[RowNumber] % 2 = 0