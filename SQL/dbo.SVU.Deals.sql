/*
  -- Пример вызова:

  EXEC [dbo].[SVU::Deals]
    @TradeAccounts    = NULL,
    @DateTimeBegin    = NULL,
    @DateTimeEnd      = NULL
*/
CREATE PROCEDURE [dbo].[SVU::Deals]
  @TradeAccounts    NVarChar(Max),
  @DateTimeBegin    DateTime,
  @DateTimeEnd      DateTime
AS
  SET NOCOUNT ON

  IF @TradeAccounts IS NULL BEGIN
    SET @TradeAccounts = '
<SYSTEM CODE="MICEX-SM1">
  <ACCOUNT NUMBER="MC0154200000"/>
</SYSTEM>
<SYSTEM CODE="MICEX-CM1">
  <ACCOUNT NUMBER="MB0154200000"/>
</SYSTEM>
<SYSTEM CODE="FORTS">
  <ACCOUNT NUMBER="D200"/>
  <ACCOUNT NUMBER="D201"/>
  <ACCOUNT NUMBER="D202"/>
  <ACCOUNT NUMBER="D203"/>
  <ACCOUNT NUMBER="D2DU"/>
</SYSTEM>'

    IF @DateTimeBegin IS NULL AND @DateTimeEnd IS NULL BEGIN
      SET @DateTimeBegin  = '20140305 00:00'
      SET @DateTimeEnd    = '20140305 23:59'
    END
  END

  DECLARE
    @@TradeAccounts XML = CAST(@TradeAccounts AS XML),
    @Forts          Bit,
    @OptTime        DateTime,
    @FutTime        DateTime

  --SET @Forts = @@TradeAccounts.exist('(/SYSTEM[@CODE="FORTS"]/ACCOUNT)[1]')
  --IF @Forts = 1 BEGIN
  --  SELECT @OptTime = Min([moment]) FROM [fortsgate].[plaza2opt].[orders_log]
  --  SELECT @FutTime = Min([moment]) FROM [fortsgate].[plaza2fut].[orders_log]
  --END

  SELECT
    [Exchange] = 'MICEX:S',
    T.*
  FROM @@TradeAccounts.nodes('/SYSTEM[@CODE="MICEX-SM"]/ACCOUNT') AS I(Node)
  CROSS APPLY
  (
    VALUES
    (
      I.Node.value('../@CODE', 'VarChar(20)'),
      I.Node.value('@NUMBER', 'VarChar(20)')
    )
  ) II([System], [Account])
  CROSS APPLY
  (
                      --'T', -- Обычная
                      --'N', -- Адресная
                      --'P'  -- Первичное размещение
                      --'F', -- Перевод денег/бумаг
                      --'R', -- Адресная сделка первой части РЕПО
                      --'G', -- Сделка по операции РЕПО с ЦК
                      --'H', -- Первая часть сделки по операции РЕПО с ЦК
                      --'h', -- Вторая часть сделки по операции РЕПО с ЦК
                      --'I', -- Адресная сделка по операции РЕПО с ЦК
                      --'J', -- Первая часть адресной сделки по операции РЕПО с ЦК
                      --'j'  -- Вторая часть адресной сделки по операции РЕПО с ЦК
                      --'L'  -- Техническая сделка по возврату активов РЕПО с ЦК

    SELECT
      [DealNo]              = CASE WHEN T.[PARENTTRADENO] <> 0 THEN T.[PARENTTRADENO] ELSE T.[TRADENO] END,
      [Reason]              = CASE
                                WHEN T.[SECBOARD] IN ('TADM', 'NADM') THEN 'O'
                              END,
      [OrderNo]             = T.[ORDERNO],
      --[Type]                = CASE WHEN T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('R', 'N') THEN 'O' WHEN T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('P') THEN 'P' END,
      [Type]                = CASE
                                WHEN B.[MARKETID] IN ('RPS') AND T.[SETTLECODE] <> 'T0' THEN 'S'
                                WHEN B.[MARKETID] IN ('RPNG') THEN 'O'
                              END,
      [Kind]                = CASE
                                WHEN T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('R', 'H', 'J') THEN 'R'
                                --WHEN T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('r', 'h', 'j') THEN 'r'
                              END,
      [BuySell]             = T.[BUYSELL],
      [DateTime]            = CAST(CAST(T.[CreateDateTime] AS Date) AS DateTime) + CAST(T.[TRADETIME] AS DateTime),

      --[UserId]              = T.[USERID],
      [TradeAccount]        = T.[FIRMID],
      [TradeAccountPart]    = T.[ACCOUNT],
      [TradeCode]           = NullIf(T.[CLIENTCODE], ''),
      [BrokerRef]           = NullIf(NullIf(BR1.[BROKERREF], T.[CLIENTCODE]), ''),
      [ExternalRef]         = NullIf(T.[EXTREF], ''),

      [TradeBoard]          = T.[SECBOARD],
      [ScheduleRulesCode]   = T.[SETTLECODE],
      [ScheduleDate]        = Cast(T.[SETTLEDATE] AS Date),
      [InstrumentKind]      = 'S',
      [InstrumentCode]      = T.[SECCODE],

      [PriceCurrency]       = CASE WHEN S.[QUOTEBASIS] = 'F' THEN '%' WHEN S.[CURRENCYID] IN ('SUR', 'RUR') THEN 'RUB' ELSE S.[CURRENCYID] END,
      [PriceValue]          = T.[PRICE],
      [PriceRound]          = S.[DECIMALS],
      [Quantity]            = T.[QUANTITY] * T.[LOTSIZE],
      [CostValue]           = CASE WHEN T.[REPOVALUE] <> 0 THEN T.[REPOVALUE] - T.[ACCRUEDINT] ELSE T.[VALUE] END,
      [TotalCoupon]         = NullIf(T.[ACCRUEDINT], 0),
      --[Yield]               = NullIf(T.[YIELD], 0),
      --[Period]              = T.[PERIOD],
      -- MICROSECONDS

      [RepoDays]              = CASE WHEN T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('R', 'r', 'H', 'h', 'J', 'j') THEN T.[REPOTERM] END,
      --[RepoScheduleRulesCode] = IsNull(U.[SETTLECODE], R.[SETTLECODE]),
      [RepoScheduleDate]      = CASE
                                  WHEN T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('R', 'r', 'H', 'h', 'J', 'j') THEN
                                    Cast
                                    (
                                      Coalesce
                                      (
                                        U.[SETTLEDATE],
                                        R.[SETTLEDATE],
                                        DateAdd(Day, T.[REPOTERM], T.[SETTLEDATE])
                                      )
                                      AS Date
                                    )
                                END,
      [RepoPercents]          = NullIf(T.[REPORATE], 0),
      [RepoPriceValue]        = NullIf(T.[PRICE2], 0),
      [RepoCostValue]         = NullIf(T.[REPO2VALUE] - T.[ACCRUED2], 0),
      [RepoTotalCoupon]       = NullIf(T.[ACCRUED2], 0),

      --[StartDiscount]       = NullIf(T.[STARTDISCOUNT], 0),
      --[LowerDiscount]       = NullIf(T.[LOWERDISCOUNT], 0),
      --[UpperDiscount]       = NullIf(T.[UPPERDISCOUNT], 0),

      [CalculationCurrency] = CASE WHEN S.[CURRENCYID] IN ('SUR', 'RUR') THEN 'RUB' ELSE S.[CURRENCYID] END,

      [RemoteTradeFirm]     = NULL,
      [RemoteTradeAccount]  = NullIf(T.[CPFIRMID], ''),

      [CommissionClr]       = NullIf(T.[CLEARINGCENTERCOMM], 0),
      [CommissionExch]      = NullIf(T.[EXCHANGECOMM], 0),
      [CommissionTS]        = NullIf(T.[TRADINGSYSTEMCOMM], 0),

      [RepoCommissionClr]   = NullIf(r.[CLEARINGCENTERCOMM], 0),
      [RepoCommissionExch]  = NullIf(r.[EXCHANGECOMM], 0),
      [RepoCommissionTS]    = NullIf(r.[TRADINGSYSTEMCOMM], 0)
    FROM [dbo].[Mcx_Trades] T
    INNER JOIN [dbo].[Mcx_BOARDS] B on T.[SECBOARD] = B.[BOARDID]
                                          --AND B.[MARKETID] NOT IN ('TECH')
                                          AND T.[SECBOARD] NOT IN ('RFND', 'TRAN')
    INNER JOIN [dbo].[Mcx_SECURITIES] S on T.[SECBOARD] = S.[SECBOARD] AND T.[SECCODE] = S.[SECCODE]
    CROSS APPLY (VALUES([Pub].[Trim Right](T.[BROKERREF], '/'))) BR1([BROKERREF])
    LEFT LOOP JOIN [dbo].[Mcx_USTRADES] U ON T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('R')
                                        AND U.[REPOTRADENO] <> 0
                                        AND T.[TRADENO] = U.[REPOTRADENO]
                                        AND T.[BUYSELL] <> U.[BUYSELL] COLLATE Latin1_General_BIN
                                        AND LOWER(T.[TRADETYPE]) COLLATE Latin1_General_BIN = U.[TYPE]
    LEFT LOOP JOIN [dbo].[Mcx_Trades] r ON R.[PARENTTRADENO] <> 0
                                        AND T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('H', 'J')
                                        AND T.[PARENTTRADENO] = R.[PARENTTRADENO]
                                        AND T.[BUYSELL] <> R.[BUYSELL] COLLATE Latin1_General_BIN
                                        AND LOWER(T.[TRADETYPE]) COLLATE Latin1_General_BIN = R.[TRADETYPE]
    WHERE T.[FIRMID] = II.[Account]
                    AND T.[CreateDateTime] >= Cast(@DateTimeBegin AS Date) AND T.[CreateDateTime] < DateAdd(Day, 1, Cast(@DateTimeEnd AS Date))
                    AND CAST(CAST(T.[CreateDateTime] AS Date) AS DateTime) + CAST(T.[TRADETIME] AS DateTime) BetWeen @DateTimeBegin AND @DateTimeEnd
                    --AND T.[TRADETYPE] COLLATE Latin1_General_BIN NOT IN ('R', 'r', 'F', 'G', 'h', 'I', 'j')
                    AND T.[TRADETYPE] COLLATE Latin1_General_BIN NOT IN ('r', 'F', 'G', 'h', 'I', 'j', 'L')
  ) T

--------------------------------------------------------------------------------------------
  UNION ALL
------------------------------------------------------------------------------------------

  SELECT
    [Exchange] = 'MICEX:C',
    T.*
  FROM @@TradeAccounts.nodes('/SYSTEM[@CODE="MICEX-CM"]/ACCOUNT') AS I(Node)
  CROSS APPLY
  (
    VALUES
    (
      I.Node.value('../@CODE', 'VarChar(20)'),
      I.Node.value('@NUMBER', 'VarChar(20)')
    )
  ) II([System], [Account])
  CROSS APPLY
  (
                      --'T', -- Обычная
                      --'N', -- Внебиржевая
                      --'P'  -- Первичное размещение
                      --'F', -- Перевод денег/бумаг

                      --'S', -- Расчетная по операции своп
                      --'W', -- Расчетная по внебиржевой операции своп
                      --'E', -- Расчетная сделка бивалютной корзины
                      --'K', -- Расчетная внебиржевая сделка бивалютной корзины
    SELECT
      [DealNo]              = CASE WHEN T.[PARENTTRADENO] <> 0 THEN T.[PARENTTRADENO] ELSE T.[TRADENO] END,
      [Reason]              = NULL,
      [OrderNo]             = T.[ORDERNO],
      --[Type]                = CASE WHEN T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('W', 'K') THEN 'O' WHEN T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('P') THEN 'P' END,
      [Type]                = NULL, --CASE WHEN T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('N', 'W', 'K') THEN 'O' END,
      [Kind]                = CASE
                                WHEN T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('S', 'W') THEN 'R'
                              END,
      [BuySell]             = T.[BUYSELL],
      [DateTime]            = CAST(CAST(T.[CreateDateTime] AS Date) AS DateTime) + CAST(T.[TRADETIME] AS DateTime),

      --[UserId]              = T.[USERID],
      [TradeAccount]        = T.[FIRMID],
      [TradeAccountPart]    = T.[ACCOUNT],
      [TradeCode]           = NullIf(T.[CLIENTCODE], ''),
      [BrokerRef]           = NullIf(NullIf(BR1.[BROKERREF], T.[CLIENTCODE]), ''),
      [ExternalRef]         = NullIf(T.[EXTREF], ''),

      [TradeBoard]          = T.[SECBOARD],
      [ScheduleRulesCode]   = T.[SETTLECODE],
      [ScheduleDate]        = Cast(T.[SETTLEDATE] AS Date),
      [InstrumentKind]      = 'C',
      [InstrumentCode]      = S.[FACEUNIT],

      [PriceCurrency]       = CASE WHEN S.[CURRENCYID] IN ('SUR', 'RUR') THEN 'RUB' ELSE S.[CURRENCYID] END,
      [PriceValue]          = T.[PRICE],
      [PriceRound]          = S.[DECIMALS],
      [Quantity]            = T.[QUANTITY] * T.[LOTSIZE],
      [CostValue]           = T.[VALUE],
      [TotalCoupon]         = NULL,
      --[Yield]               = NullIf(T.[YIELD], 0),
      --[Period]              = T.[PERIOD],
      -- MICROSECONDS

      [RepoDays]              = NULL,
      --[RepoScheduleRulesCode] = NULL,
      [RepoScheduleDate]      = Cast(R.[SETTLEDATE] AS Date),
      [RepoPercents]          = NULL,
      [RepoPriceValue]        = NullIf(r.[PRICE], 0),
      [RepoCostValue]         = NullIf(r.[VALUE], 0),
      [RepoTotalCoupon]       = NULL,

      --[StartDiscount]       = NullIf(T.[STARTDISCOUNT], 0),
      --[LowerDiscount]       = NullIf(T.[LOWERDISCOUNT], 0),
      --[UpperDiscount]       = NullIf(T.[UPPERDISCOUNT], 0),

      [CalculationCurrency] = CASE WHEN S.[CURRENCYID] IN ('SUR', 'RUR') THEN 'RUB' ELSE S.[CURRENCYID] END,

      [RemoteTradeFirm]     = NULL,
      [RemoteTradeAccount]  = NullIf(T.[CPFIRMID], ''),

      [CommissionClr]       = NullIf(T.[CLEARINGCENTERCOMM], 0),
      [CommissionExch]      = NullIf(T.[EXCHANGECOMM], 0),
      [CommissionTS]        = NullIf(T.[TRADINGSYSTEMCOMM], 0),

      [RepoCommissionClr]   = NullIf(r.[CLEARINGCENTERCOMM], 0),
      [RepoCommissionExch]  = NullIf(r.[EXCHANGECOMM], 0),
      [RepoCommissionTS]    = NullIf(r.[TRADINGSYSTEMCOMM], 0)
    FROM [dbo].[Mcx_VR_Trades] T
    INNER JOIN [dbo].[Mcx_VR_BOARDS] B on T.[SECBOARD] = B.[BOARDID] --AND B.[MARKETID] NOT IN ('TECH') AND T.[SECBOARD] NOT IN ('RFND')
    INNER JOIN [dbo].[Mcx_VR_SECURITIES] S on T.[SECBOARD] = S.[SECBOARD] AND T.[SECCODE] = S.[SECCODE]
    INNER JOIN [dbo].[Mcx_VR_INSTRS] I ON S.[INSTRID] = I.[INSTRID] AND I.[INSTRTYPE] COLLATE Latin1_General_BIN = 'C'
    CROSS APPLY (VALUES([Pub].[Trim Right](T.[BROKERREF], '/'))) BR1([BROKERREF])
    LEFT JOIN [dbo].[Mcx_VR_Trades] r ON R.[PARENTTRADENO] <> 0
                                        AND T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('S', 'W')
                                        AND T.[PARENTTRADENO] = R.[PARENTTRADENO]
                                        AND T.[BUYSELL] <> R.[BUYSELL] COLLATE Latin1_General_BIN
                                        -- AND LOWER(T.[TRADETYPE]) COLLATE Latin1_General_BIN = R.[TRADETYPE]
    WHERE T.[FIRMID] = II.[Account]
                    AND T.[CreateDateTime] >= Cast(@DateTimeBegin AS Date) AND T.[CreateDateTime] < DateAdd(Day, 1, Cast(@DateTimeEnd AS Date))
                    AND CAST(CAST(T.[CreateDateTime] AS Date) AS DateTime) + CAST(T.[TRADETIME] AS DateTime) BetWeen @DateTimeBegin AND @DateTimeEnd

                    AND T.[TRADETYPE] COLLATE Latin1_General_BIN NOT IN ('F')
                    AND
                    (
                      T.[PARENTTRADENO] = 0
                      OR
                      T.[PARENTTRADENO] + 1 = T.[TRADENO]
                      OR T.[TRADETYPE] COLLATE Latin1_General_BIN IN ('E', 'K') -- , 'S', 'W'
                    )
  ) T

--------------------------------------------------------------------------------------------
  UNION ALL
------------------------------------------------------------------------------------------

  SELECT
    T.*
  FROM
  (
/*
  -- Статус:
  0x00000004: 1 – внесистемная сделка (нерыночная цена); 0 – простая сделка (цена, близкая к рыночной).
  0x00000020: 1 – сделка исполнения опциона; 0 – не сделка исполнения опциона.
  0x00000080: 1 – индикатор истечения времени действия инструмента (для фьючерсов – исполнение, для опционов – истечение). Поддерживается для сохранения совместимости.
  0x00008000: 1 – сделка T+0 по переносу позиции; 0 – не сделка T+0 по переносу позиции.
  0x00020000: 1 – сделка Репо; 0 – не сделка Репо.
  0x00040000: 1 – набор сделок; 0 – не набор сделок.
  0x00800000: 1 – сделка истечения опциона; 0 – не сделка истечения опциона.
  0x01000000: 1 – сделка поставки через RTS Standard; 2 – не сделка поставки через RTS Standard.
  0x04000000: 1 – адресная сделка; 0 – безадресная сделка.
  0x08000000: 1 – связка; 0 – не связка.
  0x10000000: 1 – сделка при непоставке; 0 – не является сделкой при непоставке.
  0x40000000: 1 – сделка исполнения фьючерса или инструмента RTS Standard (кроме исполнения фьючерса через RTS Standard); 0 – не сделка исполнения.

          CASE
            WHEN source.[Flags] & 0x800084 = 0x800084 THEN 'L'  -- Закрытие опционной позиции в результате истечения опциона
            WHEN source.[Flags] & 0x000024 = 0x000024 THEN 'E'  -- Закрытие опционной позиции в результате исполнения опциона
          END,
*/
    SELECT
      [Exchange]              = 'FORTS',
      [DealNo]                = T.[id_deal],
      [Reason]                = NULL,
      [OrderNo]               = CASE WHEN O.[id_ord1] <> 0 THEN O.[id_ord1] ELSE BS.[OrderNo] END,
      [Type]                  = NULL,
      [Kind]                  = CASE
                                  WHEN BS.[Status] & 0x800084 = 0x800084 THEN 'L'   -- Закрытие опционной позиции в результате истечения опциона
                                  WHEN BS.[Status] & 0x000024 = 0x000024 THEN 'E'   -- Закрытие опционной позиции в результате исполнения/заявки экспирации опциона
                                  WHEN SC.[signs] & 0x02 <> 0 THEN 'M'              -- Маржируемая сделка
                                END,
      [BuySell]               = BS.[BuySell],
      [DateTime]              = T.[moment],

      --[UserId]                = T.[USERID],
      [TradeAccount]          = BS.[TradeAccount],
      [TradeAccountPart]      = NULL,
      [TradeCode]             = BS.[ClientCode],
      [BrokerRef]             = NullIf(NullIf(BS.[Comment], BS.[TradeCode]), ''),
      [ExternalRef]           = NullIf(Cast(BS.[ExternalRef] AS VarChar(20)), ''),

      [TradeBoard]            = NULL,
      [ScheduleRulesCode]     = NULL,
      --[ScheduleDate]          = ss.[date_clr],
      [ScheduleDate]          = CAST(CASE WHEN ss.[date_clr] IS NOT NULL THEN ss.[date_clr] ELSE sess.[end] END AS Date),
      [InstrumentKind]        = 'D' + CASE WHEN SC.[signs] & 0x02 <> 0 THEN 'M' ELSE 'P' END,
      [InstrumentCode]        = sc.[isin],

      [PriceCurrency]         = '^',
      [PriceValue]            = T.[PRICE],
      [PriceRound]            = sc.[roundto],
      [Quantity]              = T.[amount],
      [CostValue]             = NULL,
      [TotalCoupon]           = NULL,
      --[Yield]               = NullIf(T.[YIELD], 0),
      --[Period]              = T.[PERIOD],
      -- MICROSECONDS

      [RepoDays]              = NULL,
      --[RepoScheduleRulesCode] = NULL,
      [RepoScheduleDate]      = NULL,
      [RepoPercents]          = NULL,
      [RepoPriceValue]        = NULL,
      [RepoCostValue]         = NULL,
      [RepoTotalCoupon]       = NULL,

      --[StartDiscount]       = NullIf(T.[STARTDISCOUNT], 0),
      --[LowerDiscount]       = NullIf(T.[LOWERDISCOUNT], 0),
      --[UpperDiscount]       = NullIf(T.[UPPERDISCOUNT], 0),

      [CalculationCurrency]   = 'RUB',

      [RemoteTradeFirm]       = BS.[RemoteTradeFirm],
      [RemoteTradeAccount]    = BS.[RemoteTradeAccount],

      [CommissionClr]         = NULL,
      [CommissionExch]        = BS.[CommissionExch],
      [CommissionTS]          = NULL,

      [RepoCommissionClr]     = NULL,
      [RepoCommissionExch]    = NULL,
      [RepoCommissionTS]      = NULL
    FROM [fortsgate].[dbo].[plaza2_OPT_user_deal] T
    CROSS APPLY
    (
      SELECT
        [BuySell]             = 'S',
        [OrderNo]             = NullIf(T.[id_ord_sell], 0),
        [Status]              = T.[status_sell],
        [TradeCode]           = T.[code_sell],
        [TradeAccount]        = SubString(T.[code_sell], 1, 4),
        [ClientCode]          = SubString(T.[code_sell], 5, 3),
        [ExternalRef]         = NullIf(T.[ext_id_sell], 0),
        [comment]             = [Pub].[Trim Right](NullIf(T.[comment_sell], ''), '/'),
        [CommissionExch]      = NullIf(T.[fee_sell], 0),
        [RemoteTradeFirm]     = NullIf(T.[code_rts_buy], ''),
        [RemoteTradeAccount]  = CASE WHEN T.[code_buy] <> '       ' THEN SubString(T.[code_buy], 1, 4) END
      WHERE T.[code_sell] <> '       '
        UNION ALL
      SELECT
        [BuySell]             = 'B',
        [OrderNo]             = NullIf(T.[id_ord_buy], 0),
        [Status]              = T.[status_buy],
        [TradeCode]           = T.[code_buy],
        [TradeAccount]        = SubString(T.[code_buy], 1, 4),
        [ClientCode]          = SubString(T.[code_buy], 5, 3),
        [ExternalRef]         = NullIf(T.[ext_id_buy], 0),
        [comment]             = [Pub].[Trim Right](NullIf(T.[comment_buy], ''), '/'),
        [CommissionExch]      = NullIf(T.[fee_buy], 0),
        [RemoteTradeFirm]     = NullIf(T.[code_rts_sell], ''),
        [RemoteTradeAccount]  = CASE WHEN T.[code_sell] <> '       ' THEN SubString(T.[code_sell], 1, 4) END
      WHERE T.[code_buy] <> '       '
    ) BS
		INNER LOOP JOIN [fortsgate].[dbo].[Plaza2_FUT_session] sess on sess.OPT_sess_id  = T.sess_id
    INNER LOOP JOIN [fortsgate].[dbo].[plaza2_OPT_opt_sess_contents] sc on sc.isin_id = T.isin_id  and sc.sess_id = T.sess_id
    LEFT LOOP JOIN [fortsgate].[dbo].[plaza2_opt_sess_settl] ss on ss.isin_id = T.isin_id  and ss.sess_id = T.sess_id
    LEFT JOIN [fortsgate].[dbo].[plaza2_OPT_orders_log] O ON BS.[OrderNo] IS NOT NULL AND BS.[OrderNo] = O.[id_ord] AND O.[action] = 1
    --INNER LOOP JOIN [fortsgate].[dbo].[plaza2_fut_instruments] sc on sc.isin_id = T.isin_id

    WHERE --(@OptTime < @DateTimeEnd OR @DateTimeBegin < @FutTime) AND (@OptTime IS NULL OR T.[moment] < @OptTime) AND
      T.[moment] >= @DateTimeBegin AND T.[moment] < @DateTimeEnd
      AND BS.[TradeAccount] IN (SELECT I.Node.value('@NUMBER', 'VarChar(20)') FROM @@TradeAccounts.nodes('/SYSTEM[@CODE="FORTS"]/ACCOUNT') AS I(Node))

    UNION ALL

/*
  -- Статус:
  --  0x4  – внесистемная сделка (нерыночная цена)
  --  0x20 - сделка является сделкой экспирации
  --  0x80 – признак истечения времени действия инструмента
  --  0x8000 – сделка T+0 по переносу позиции
  --  0x20000 – Техническая сделка (Репо)
  --  0x40000 – Техническая сделка (Связанная парная заявка)
  --  0x1000000 – сделка поставки через RTS Standard
  --  0x4000000 – адресная сделка
  --  0x8000000 – сделка по связке
  --  0x10000000 – сделка при непоставке
  --  0x40000000 – сделка исполнения фьючерса или инструмента RTS Standard (кроме исполнения фьючерса через RTS Standard)

          -- [Deal:Type]
          CASE
            WHEN source.[Flags] & 0x40000085 = 0x40000085 THEN 'e'        -- Исполнение ЦБ позиции
            WHEN source.[Flags] & 0x40000084 = 0x40000084 THEN 'E'        -- Исполнение/закрытие Фьючерсной/ЦБ позиции
            WHEN source.[Flags] & 0x01000084 = 0x01000084
              THEN CASE
                     WHEN source.[Sign:Spot] = 0 THEN 'E'                 -- Исполнение/Закрытие фьючерсной позиции
                     ELSE 'P'                                             -- или открытие ЦБ позиции заключением сделки на РТСС
                   END
            WHEN source.[Flags] & 0x00000024 = 0x00000024 THEN 'P'        -- Открытие фьючерсной позиции в результате исполнения опциона
            WHEN source.[Flags] & 0x00060000 = 0x00060000 THEN 'r'        -- Обратная РЕПО
            WHEN source.[Flags] & 0x00060000 = 0x00020000 THEN 'R'        -- Прямая РЕПО
            WHEN source.[Flags] & 0x04000000 <> 0         THEN 'A'        -- Просто адресная заявка
          END,
*/
    SELECT
      [Exchange]              = CASE WHEN IK.[Sign:Spot] = 1 THEN 'RTSS' ELSE 'FORTS' END,
      [DealNo]                = T.[id_deal],
      [Reason]                = CASE
                                  WHEN BS.[Status] & 0x40000084 = 0x40000084 THEN NULL         -- Исполнение/Закрытие Фьючерсной/ЦБ позиции + признак истечения времени действия инструмента Standard (кроме исполнения фьючерса через RTS Standard)
                                  WHEN BS.[Status] & 0x01000084 = 0x01000084                   -- Cделка при непоставке + признак истечения времени действия инструмента
                                    THEN CASE
                                           WHEN IK.[Sign:Spot] = 0 THEN NULL                   -- Исполнение/Закрытие фьючерсной позиции
                                           ELSE 'D'                                            -- или открытие ЦБ позиции заключением сделки на РТСС
                                         END
                                  WHEN BS.[Status] & 0x00000024 = 0x00000024 THEN 'D'          -- Открытие фьючерсной позиции в результате экспирации опциона
                                END,
      [OrderNo]               = CASE WHEN O.[id_ord1] <> 0 THEN O.[id_ord1] ELSE BS.[OrderNo] END,
      [Type]                  = NULL,
      [Kind]                  = CASE
                                  WHEN BS.[Status] & 0x40000084 = 0x40000084 THEN 'e'          -- Исполнение/Закрытие Фьючерсной/ЦБ позиции + признак истечения времени действия инструмента Standard (кроме исполнения фьючерса через RTS Standard)
                                  WHEN BS.[Status] & 0x01000084 = 0x01000084                   -- Cделка при непоставке + признак истечения времени действия инструмента
                                    THEN CASE
                                           WHEN IK.[Sign:Spot] = 0 THEN 'e'                    -- Исполнение/Закрытие фьючерсной позиции
                                           ELSE NULL                                           -- или открытие ЦБ позиции заключением сделки на РТСС
                                         END
                                  WHEN BS.[Status] & 0x00060000 = 0x00060000 THEN 'r'          -- Обратная РЕПО
                                  WHEN BS.[Status] & 0x00060000 = 0x00020000 THEN 'R'          -- Прямая РЕПО
                                  --WHEN BS.[Status] & 0x04000000 <> 0         THEN 'A'        -- Просто адресная заявка
                                  --WHEN BS.[Status] & 0x00000024 = 0x00000024 THEN NULL         -- Открытие фьючерсной позиции в результате экспирации опциона
                                  WHEN IK.[Sign:Spot] = 0                    THEN 'M'          -- Маржируемая сделка
                                END,
      [BuySell]               = BS.[BuySell],
      [DateTime]              = T.[moment],

      --[UserId]              = T.[USERID],
      [TradeAccount]          = BS.[TradeAccount],
      [TradeAccountPart]      = NULL,
      [TradeCode]             = BS.[ClientCode],
      [BrokerRef]             = NullIf(NullIf(BS.[Comment], BS.[TradeCode]), ''),
      [ExternalRef]           = NullIf(Cast(BS.[ExternalRef] AS VarChar(20)), ''),

      [TradeBoard]            = NULL,
      [ScheduleRulesCode]     = NULL,
      -- [ScheduleDate]          = Cast(CASE WHEN IK.[Sign:Spot] = 1 THEN sc.[d_exp] ELSE ss.[date_clr] END AS Date),
      [ScheduleDate]          = Cast(CASE WHEN IK.[Sign:Spot] = 1 THEN sc.[d_exp] WHEN ss.[date_clr] IS NOT NULL THEN ss.[date_clr] ELSE sess.[end] END AS Date),
      [InstrumentKind]        = CASE WHEN IK.[Sign:Spot] = 1 THEN 'S' WHEN sc.[signs] & 0x02 <> 0 THEN 'DM' ELSE 'DP' END,
      [InstrumentCode]        = CASE WHEN IK.[Sign:Spot] = 1 THEN IsNull(RTSS2.[short_isin], RTSS1.[short_isin]) + 'S' ELSE sc.[isin] END,

      [PriceCurrency]         = CASE WHEN IK.[Sign:Spot] = 1 OR sc.[signs] & 0x02 = 0 THEN 'RUB' ELSE '^' END,
      [PriceValue]            = T.[PRICE],
      [PriceRound]            = sc.[roundto],
      [Quantity]              = CASE WHEN IK.[Sign:Spot] = 1 THEN T.[amount] * sc.[lot_volume] ELSE T.[amount] END,
      [CostValue]             = NULL,
      [TotalCoupon]           = NULL,
      --[Yield]               = NullIf(T.[YIELD], 0),
      --[Period]              = T.[PERIOD],
      -- MICROSECONDS

      [RepoDays]              = NULL,
      --[RepoScheduleRulesCode] = NULL,
      [RepoScheduleDate]      = NULL,
      [RepoPercents]          = NULL,
      [RepoPriceValue]        = NULL,
      [RepoCostValue]         = NULL,
      [RepoTotalCoupon]       = NULL,

      --[StartDiscount]       = NullIf(T.[STARTDISCOUNT], 0),
      --[LowerDiscount]       = NullIf(T.[LOWERDISCOUNT], 0),
      --[UpperDiscount]       = NullIf(T.[UPPERDISCOUNT], 0),

      [CalculationCurrency]   = 'RUB',

      [RemoteTradeFirm]       = BS.[RemoteTradeFirm],
      [RemoteTradeAccount]    = BS.[RemoteTradeAccount],

      [CommissionClr]         = NULL,
      [CommissionExch]        = BS.[CommissionExch],
      [CommissionTS]          = NULL,

      [RepoCommissionClr]     = NULL,
      [RepoCommissionExch]    = NULL,
      [RepoCommissionTS]      = NULL
    FROM [fortsgate].[dbo].[plaza2_FUT_user_deal] T
    CROSS APPLY
    (
      SELECT
        [BuySell]             = 'S',
        [OrderNo]             = NullIf(T.[id_ord_sell], 0),
        [Status]              = T.[status_sell],
        [TradeCode]           = T.[code_sell],
        [TradeAccount]        = SubString(T.[code_sell], 1, 4),
        [ClientCode]          = SubString(T.[code_sell], 5, 3),
        [ExternalRef]         = NullIf(T.[ext_id_sell], 0),
        [comment]             = [Pub].[Trim Right](NullIf(T.[comment_sell], ''), '/'),
        [CommissionExch]      = NullIf(T.[fee_sell], 0),
        [RemoteTradeFirm]     = NullIf(T.[code_rts_buy], ''),
        [RemoteTradeAccount]  = CASE WHEN T.[code_buy] <> '       ' THEN SubString(T.[code_buy], 1, 4) END
      WHERE T.[code_sell] <> '       ' AND T.[status_sell] & 0x40000085 <> 0x40000085
        UNION ALL
      SELECT
        [BuySell]             = 'B',
        [OrderNo]             = NullIf(T.[id_ord_buy], 0),
        [Status]              = T.[status_buy],
        [TradeCode]           = T.[code_buy],
        [TradeAccount]        = SubString(T.[code_buy], 1, 4),
        [ClientCode]          = SubString(T.[code_buy], 5, 3),
        [ExternalRef]         = NullIf(T.[ext_id_buy], 0),
        [comment]             = [Pub].[Trim Right](NullIf(T.[comment_buy], ''), '/'),
        [CommissionExch]      = NullIf(T.[fee_buy], 0),
        [RemoteTradeFirm]     = NullIf(T.[code_rts_sell], ''),
        [RemoteTradeAccount]  = CASE WHEN T.[code_sell] <> '       ' THEN SubString(T.[code_sell], 1, 4) END
      WHERE T.[code_buy] <> '       ' AND T.[status_buy] & 0x40000085 <> 0x40000085
    ) BS
		INNER LOOP JOIN [fortsgate].[dbo].[Plaza2_FUT_session] sess on sess.sess_id  = T.sess_id
    INNER LOOP JOIN [fortsgate].[dbo].[plaza2_FUT_sess_contents] sc on sc.isin_id = T.isin_id  and sc.sess_id = T.sess_id
    LEFT LOOP JOIN [fortsgate].[dbo].[plaza2_FUT_sess_settl] ss on ss.isin_id = T.isin_id  and ss.sess_id = T.sess_id
    CROSS APPLY (VALUES(CASE WHEN sc.[signs] & 0x0c <> 0 THEN 1 ELSE 0 END)) IK([Sign:Spot])
    LEFT LOOP JOIN [fortsgate].[dbo].[plaza2_fut_sess_contents] RTSS1 ON IK.[Sign:Spot] = 1 AND sc.ticker = RTSS1.isin_id and T.sess_id = RTSS1.sess_id
    LEFT LOOP JOIN [fortsgate].[dbo].[plaza2_fut_sess_contents] RTSS2 ON IK.[Sign:Spot] = 1 AND RTSS1.ticker <> RTSS1.isin_id AND RTSS1.ticker = RTSS2.isin_id and T.sess_id = RTSS2.sess_id
    LEFT JOIN [fortsgate].[dbo].[plaza2_FUT_orders_log] O ON BS.[OrderNo] IS NOT NULL AND BS.[OrderNo] = O.[id_ord] AND O.[action] = 1
    WHERE --(@FutTime < @DateTimeEnd OR @DateTimeBegin < @FutTime) AND
      T.[moment] >= @DateTimeBegin AND T.[moment] < @DateTimeEnd
      --AND (@FutTime IS NULL OR T.[moment] < @FutTime)
      AND BS.[TradeAccount] IN (SELECT I.Node.value('@NUMBER', 'VarChar(20)') FROM @@TradeAccounts.nodes('/SYSTEM[@CODE="FORTS"]/ACCOUNT') AS I(Node))
  ) T
--  ORDER BY T.[DateTime]
  OPTION (FORCE ORDER, MAXDOP 1)
GO
/*
  -- Пример вызова:
  EXEC [dbo].[SVU::Micex Last Prices]
    @PrevDays = 3
*/
CREATE PROCEDURE [dbo].[SVU::Micex Last Prices]
  @PrevDays Int = NULL
AS
  SET NOCOUNT ON

  SELECT
    [PlaceCode]         = 'MICEX:S',
    [InstrumentCode]    = P.[SECCODE],
    [LastPrice]         = P.[LAST],
    [Accruedint]        = P.[ACCRUEDINT],
    [CurrencyCode]      = CASE WHEN P.[CURRENCYID] = 'SUR' THEN 'RUB' ELSE P.[CURRENCYID] END
  FROM
  (
    SELECT
      [SECCODE],
      [LAST]            = CASE
                            WHEN S.[QUOTEBASIS] = 'R' THEN S.[LAST]
                            WHEN S.[QUOTEBASIS] = 'F' THEN S.[LAST] * S.[FACEVALUE]
                          END,
      [CURRENCYID]      = CASE
                            WHEN S.[QUOTEBASIS] = 'R' THEN S.[CURRENCYID]
                            WHEN S.[QUOTEBASIS] = 'F' THEN S.[FACEUNIT]
                          END,
      [ACCRUEDINT]      = NULLIF(S.[ACCRUEDINT], 0),
      [RowNumber]       = ROW_NUMBER() OVER (PARTITION BY [SECCODE] ORDER BY [LastModifyDateTime] DESC),
      [LastModifyDateTime],
      [MARKETCODE],
      [TRADINGSTATUS]
    FROM [dbo].[Mcx_SECURITIES] S
    WHERE [MARKETCODE] IN ('FOND', 'FNDT')
          -- AND [TRADINGSTATUS] = 'T'
          AND [LastModifyDateTime] > CASE WHEN @PrevDays IS NULL THEN CAST(GETDATE() AS Date) ELSE DATEADD(D, -@PrevDays, CAST(GETDATE() AS Date)) END
          AND ISNULL([LAST], 0) <> 0
  ) P
  WHERE P.[RowNumber] = 1
GO