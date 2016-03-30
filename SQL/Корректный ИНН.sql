SELECT
  Sum(Cast(INN.[Value] AS SmallInt) * W.[Weight]) % 11 % 10
FROM
(
  VALUES
  (3, 8),
  (4, 6),
  (5, 4),
  (6, 9),
  (7, 5),
  (8, 3),
  (9, 10),
  (10, 4),
  (11, 2),
  (12, 7)
) W([Index], [Weight])
INNER JOIN [Pub].[Foreach]('165300180570') INN ON W.[Index] = 11 - INN.[Index]

SELECT
  Sum(Cast(INN.[Value] AS SmallInt) * W.[Weight]) % 11 % 10
FROM
(
  VALUES
  (2, 8),
  (3, 6),
  (4, 4),
  (5, 9),
  (6, 5),
  (7, 3),
  (8, 10),
  (9, 4),
  (10, 2),
  (11, 7),
  (12, 3)
) W([Index], [Weight])
INNER JOIN [Pub].[Foreach]('165300180570') INN ON W.[Index] = 12 - INN.[Index]