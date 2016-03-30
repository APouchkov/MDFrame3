DECLARE
  @Body NvarChar(Max),
  @XML  XML

SELECT
  @Body = [Body] 
FROM [System].[HTTP::Request]('http://www.reuters.com/finance/currencies/quote?srcAmt=1.00&srcCurr=HKD&destAmt=&destCurr=USD', 'GET', NULL)

  SELECT
    @Body = Replace([Match], '&nbsp;', '')
  FROM
    [Pub].[RegExp::Matches]
    (
      @Body,
      '(<div class="module">.*?)<div class="linebreak">',
      'ignorecase,singleline'
    )
  WHERE [GroupIndex] = 1 AND [MatchIndex] = 1
  IF @@ROWCOUNT <> 1 RaisError('Данные не найдены', 16, 2)

    SET @XML = Cast(@Body AS XML)

    SELECT
      [time]      = I.Node.value('(div[@class[1]="fourUp currQuote"]/div[@class[1]="time"])[1]', 'NVarChar(200)'),
      [Day High]  = I.Node.value('(div[@class[1]="fourUp highLow" and h5[1]="Day High"]/div[@class="currData norm"])[1]', 'Numeric(18,4)'),
      [Day Low]   = I.Node.value('(div[@class[1]="fourUp highLow" and h5[1]="Day Low"]/div[@class="currData norm"])[1]', 'Numeric(18,4)'),
      [Bid]       = I.Node.value('(div[@class[1]="fourUp bidAsk changeUp" and h5[1]="Bid"]/div[@class="currData norm"])[1]', 'Numeric(18,4)'),
      [Offer]       = I.Node.value('(div[@class[1]="fourUp bidAsk last changeUp" and h5[1]="Offer"]/div[@class="currData norm"])[1]', 'Numeric(18,4)')
    FROM @XML.nodes('/div/div[@class[1]="moduleBody"]/div[@class[1]="currencyQuote"]') I(Node)

