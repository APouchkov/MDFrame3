DECLARE @TRAN TABLE
(
  [Id] Int NOT NULL IDENTITY(1,1),
  [DateTime] DateTime NOT NULL,
  [Value] Numeric(18) NOT NULL,
  [Cost] Numeric(18) NOT NULL,
  PRIMARY KEY CLUSTERED([DateTime], [Id])
)


INSERT INTO @TRAN
SELECT
  V.*
FROM [Pub].[Repeat](1, 10000) R
CROSS APPLY
(
  VALUES
    (DateAdd(Second, [Index] * 2, '20130913'), 100, 500),
    (DateAdd(Second, [Index] * 2 + 1, '20130913'), -100, 501)
) V([DateTime], [Value], [Cost])
UNION ALL
SELECT *
FROM
(
  VALUES
  ('20130909', -100, 600),
  ('20130910', 200, 1200),
  ('20130913 14:00', 100, 700)
) V([DateTime], [Value], [Cost])


DECLARE 
  @DateTime DateTime      = GetDate(),
  @Pos      Numeric(18,0) = 200

DECLARE
  @Sign     Bit     = SIGN(@Pos)

SELECT
  [Cost]  = SUM(CASE WHEN [Total] > @Pos THEN [Cost] * ([Total] - [PreTotal]) / [Total] ELSE [Cost] END)
FROM
(
  SELECT
    *
  FROM
  (
    SELECT
      *,
      [Total]     = SUM([Value]) OVER (ORDER BY [DateTime] DESC, [Id] DESC),
      [PreTotal]  = SUM([Value]) OVER (ORDER BY [DateTime] DESC, [Id] DESC ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING)
    FROM @TRAN
    WHERE [DateTime] < @DateTime AND SIGN([Value]) = @Sign
  ) I
  WHERE [PreTotal] IS NULL OR [PreTotal] < @Pos
) I
