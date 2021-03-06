SET NOCOUNT ON

  DECLARE
    @Firm_Id        TinyInt,
    @FirmType_Id    TinyInt,
    @CloseDate      Date,

    @AccountPart_Id   Int,
    @AccountPart_IDs  VarChar(1000),
    @Text             NVarChar(Max),

    @Accounts       [Pub].[TList:Int],
    @Params         TParams

  -- Закрываемые счета и разделы
  DECLARE @ClosingAccountsParts TABLE
  (
    [Account_Id]     Int     NOT NULL,
    [AccountPart_Id] Int     NOT NULL,
    [Firm_Id]        TinyInt NOT NULL,
    [FirmType_Id]    TinyInt NOT NULL,
    [CloseDate]      Date    NOT NULL,
    PRIMARY KEY CLUSTERED ([Account_Id], [AccountPart_Id], [CloseDate])
  )

    SET @Params = TParams::New()
    SET @Params.AddParam('Evening', Cast(1 AS Bit))
    SET @Params.AddParam('Planning', Cast(1 AS Bit))
    SET @Params.AddParam('Schema', '[INTERNAL]')
    SET @Params.AddParam('Actives:States', '[*]')

    INSERT @ClosingAccountsParts([Account_Id], [AccountPart_Id], [Firm_Id], [FirmType_Id], [CloseDate])
    SELECT
      [Account_Id]      = [Account_Id],
      [AccountPart_Id]  = [Id],
      [Firm_Id]         = [Firm_Id],
      [FirmType_Id]     = [FirmType_Id],
      [CloseDate]       = [@CloseDate]
    FROM [BackOffice].[Accounts:Parts(Expanded)]
    WHERE [@CloseDate] IS NOT NULL AND [Firm_Id] = 1 AND [FirmType_Id] = 12
    OPTION (FORCE ORDER, MAXDOP 1)

    IF @@ROWCOUNT = 0 RETURN

    -- Проверка операций
    SELECT TOP 1
      @AccountPart_Id = A.[AccountPart_Id]
    FROM @ClosingAccountsParts A
    INNER LOOP JOIN [BackOffice].[Transactions] TR ON A.[Account_Id] = TR.[Account_Id] AND TR.[AccountPart_Id] = A.[AccountPart_Id]
                                                        AND TR.[DateTime] > DateAdd(Day, 1, A.[CloseDate])
    OPTION (FORCE ORDER, MAXDOP 1)

    IF @AccountPart_Id IS NOT NULL
      PRINT 'Раздел счета «' + [BackOffice].[Account::Part](@AccountPart_Id) + N'» не может быть закрыт, т.к. после даты закрытия по нему имеются неудалённые проводки'

    DECLARE ITEMS_CURSOR CURSOR FAST_FORWARD LOCAL FOR
    SELECT
      I.[CloseDate],
      I.[Firm_Id],
      I.[FirmType_Id]
    FROM
    (
      SELECT DISTINCT
        [CloseDate],
        [Firm_Id],
        [FirmType_Id]
      FROM @ClosingAccountsParts
    ) I
    INNER HASH JOIN
    (
      SELECT
        RP.[Firm_Id],
        RP.[FirmType_Id],
        [Date]  = MIN(RP.[Date])
      FROM [BackOffice].[Rests:Points] RP
      WHERE RP.[Identifier] IS NOT NULL
      GROUP BY RP.[Firm_Id], RP.[FirmType_Id]
    ) RP ON I.[Firm_Id] = RP.[Firm_Id] AND I.[FirmType_Id] = RP.[FirmType_Id] AND I.[CloseDate] >= RP.[Date]
    ORDER BY I.[CloseDate] ASC, I.[Firm_Id] ASC, I.[FirmType_Id] ASC
    OPTION (FORCE ORDER, MAXDOP 1)

    OPEN ITEMS_CURSOR
    WHILE (1 = 1) BEGIN
      FETCH NEXT FROM ITEMS_CURSOR
      INTO @CloseDate, @Firm_Id, @FirmType_Id
      IF @@FETCH_STATUS <> 0 BREAK

      SET @Params.AddParam('Firm_Id', @Firm_Id)
      SET @Params.AddParam('FirmType_Id', @FirmType_Id)
      SET @Params.AddParam('DateTime', @CloseDate)
      SET @Params.Prepare('[BackOffice].[Analytics::Rests]')

      -- Формирование всего списка закрываемых счетов
      DELETE @Accounts
      INSERT @Accounts([Id])
      SELECT DISTINCT
        [Id] = [Account_Id]
      FROM @ClosingAccountsParts
      WHERE [CloseDate] = @CloseDate AND [Firm_Id] = @Firm_Id AND [FirmType_Id] = @FirmType_Id

      SELECT
        @Text =
          [Pub].[Concat]
          (
            'Раздел счета «' + [BackOffice].[Account::Part]([AccountPart_Id]) + N'» не может быть закрыт, т.к. на вечер дня закрытия (' + Convert(NVarChar(10), @CloseDate, 104) + N') на нем имеются остатки',
            '
'
          )
      FROM
      (
        SELECT DISTINCT
          CAP.[AccountPart_Id]
        FROM [BackOffice].[Analytics::Rests](@Accounts, @Params) R
        INNER JOIN @ClosingAccountsParts CAP ON R.[Account_Id] = CAP.[Account_Id] AND R.[AccountPart_Id] = CAP.[AccountPart_Id] AND CAP.[CloseDate] = @CloseDate
      ) I
      OPTION (FORCE ORDER, MAXDOP 1)

      IF @Text IS NOT NULL
        EXEC [SQL].[Print] @Text
    END
    CLOSE ITEMS_CURSOR
    DEALLOCATE ITEMS_CURSOR
GO

