SET NOCOUNT ON

DECLARE @ISINS TABLE
(
  [ISIN]  VarChar(50) NOT NULL PRIMARY KEY CLUSTERED,
  [CFI]   VarChar(20)     NULL,
  [STATE] VarChar(20)     NULL
)

DECLARE
  @Header   NVarChar(4000),
  @Body     NVarChar(Max),
  @XML      XML,
  @Page     Int = 1

WHILE 1 = 1 BEGIN
  WHILE 1 = 1 BEGIN
    SELECT
      @Header  = [Header],
      @Body     = [Body]
    FROM [System].[HTTP::Request]('http://www.isin.ru/ru/foreign_isin/db/index.php?page=' + Cast(@Page AS NVarChar(10)), 'GET', NULL)

    IF CharIndex('Server Unavailable', @Header) = 0 BREAK
    WAITFOR DELAY '00:00:01'
  END

  SELECT
    @Body = Replace([Match], '&nbsp;', '')
  FROM
    [Pub].[RegExp::Matches]
    (
      @Body,
      '<table\s+class="db_ru_table_select".*?>\s*<tr.*?>.*?</tr.*?>\s*(.*?)\s*</table>',
      'ignorecase,singleline'
    )
  WHERE [GroupIndex] = 1 AND [MatchIndex] = 1

  IF @@ROWCOUNT = 0 BEGIN
    EXEC [SQL].[Print] @Header
    BREAK
  END

  EXEC [SQL].[Print] @Page --@Body

  SET @XML = Cast(@Body AS XML)

  INSERT INTO @ISINS([ISIN], [CFI], [STATE])
  SELECT
    I.[ISIN],
    I.[CFI],
    I.[STATE]
  FROM
  (
    SELECT
      [ISIN]  = I.Node.value('td[1]', 'NVarChar(200)'),
      [CFI]   = I.Node.value('td[4]', 'NVarChar(20)'),
      [STATE] = I.Node.value('td[5]', 'NVarChar(200)')
    FROM @XML.nodes('/tr') I(Node)
  ) I
  WHERE I.[ISIN] <> ''

  SET @Page += 1
END

SELECT * FROM @ISINS