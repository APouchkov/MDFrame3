/*
  EXEC [dbo].[SVU::Orders]
    @TradeAccounts  = NULL,
    @DateTimeBegin  = NULL,
    @DateTimeEnd    = NULL
*/
CREATE PROCEDURE [dbo].[SVU::Orders]
  @TradeAccounts    NVarChar(Max),
  @DateTimeBegin    DateTime,
  @DateTimeEnd      DateTime
AS
  SET NOCOUNT ON

  IF @TradeAccounts IS NULL BEGIN
    SET @TradeAccounts = '
<SYSTEM CODE="MICEX-SM">
  <ACCOUNT NUMBER="MC0154200000"/>
</SYSTEM>
<SYSTEM CODE="MICEX-CM1">
  <ACCOUNT NUMBER="MB0154200000"/>
</SYSTEM>
<SYSTEM CODE="FORTS1">
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
    @Forts          Bit
    --@OptTime        DateTime,
    --@FutTime        DateTime

  SET @Forts = @@TradeAccounts.exist('(/SYSTEM[@CODE="FORTS"]/ACCOUNT)[1]')
  --IF @Forts = 1 BEGIN
  --  SELECT @OptTime = Min([moment]) FROM [fortsgate].[plaza2opt].[orders_log]
  --  SELECT @FutTime = Min([moment]) FROM [fortsgate].[plaza2fut].[orders_log]
  --END

  SELECT
    [Exchange] = 'MICEX:S',
    O.*
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
    SELECT
      [OrderNo]             = O.[ORDERNO],
      [Reason]              = NULL,
      [Type]                = NULL,
      [Kind]                = CASE WHEN O.[REPOFLAG] = 'R' THEN 'R' END,
      [BuySell]             = CASE WHEN B.[MARKETID] = 'REPT' THEN CASE WHEN O.[BUYSELL] = 'B' THEN 'S' ELSE 'B' END ELSE O.[BUYSELL] END,
      [CreateDateTime]      = CAST(CAST(O.[CreateDateTime] AS Date) AS DateTime) + CAST(O.[ORDERTIME] AS DateTime),
      [ExpireDateTime]      = CASE
                                WHEN O.[IMMCANCEL] IN ('N', 'W')
                                  THEN O.[CreateDateTime]
                                ELSE
                                  CAST(CAST(O.[CreateDateTime] AS Date) AS DateTime)
                                  + CAST('23:59:59' AS DateTime)
                              END,
      [UpdateDateTime]      = CASE
                                WHEN O.[UPDATETIME] <> '' THEN CAST(CAST(O.[CreateDateTime] AS Date) AS DateTime) + CAST(O.[UPDATETIME] AS DateTime)
                                WHEN O.[STATUS] IN ('M', 'W', 'C', 'F', 'R') THEN O.[CreateDateTime]
                              END,
      --[Status]              = O.[STATUS],

    --  O - активная
    --  M - исполнена
    --  W - снята
    --  С - снята системой
    --  F - отклонена контрагентом
    --  R - отклонена ТС
    --  T - Время активации не наступило
    --  E - Ожидает наступления события

      [CloseStatus]         = CASE O.[STATUS]
                                WHEN 'O' THEN NULL
                                WHEN 'T' THEN NULL
                                WHEN 'E' THEN NULL
                                WHEN 'M' THEN 'E'
                                WHEN 'W' THEN 'C'
                                ELSE 'R'
                              END,
      [RejectReason]        = CASE O.[STATUS]
                                WHEN 'C' THEN N'Снята торговой системой'
                                WHEN 'F' THEN N'Отклонена контрагентом'
                                WHEN 'R' THEN N'Отклонена торговой системой'
                              END,
      [CannotBeSplitted]    = Cast(CASE WHEN O.[SPLITFLAG] = 'O' THEN 1 END AS Bit),
      [ExecuteOrCancel]     = Cast(CASE O.[IMMCANCEL] WHEN 'N' THEN 1 WHEN 'W' THEN 0 END AS Bit),
      [UserId]              = O.[USERID],
      [TradeAccount]        = O.[FIRMID],
      [TradeAccountPart]    = O.[ACCOUNT],
      [TradeCode]           = NullIf(O.[CLIENTCODE], ''),
      [BrokerRef]           = NullIf(NullIf(BR1.[BROKERREF], O.[CLIENTCODE]), ''),
      [ExternalRef]         = NullIf(O.[EXTREF], ''),

      [TradeBoard]            = O.[SECBOARD],
      [ScheduleRulesCode]     = O.[SETTLECODE],
      [ScheduleRulesDate]     = Cast(O.[SETTLEDATE] AS Date),
      [InstrumentKind]        = 'S',
      [InstrumentCode]        = O.[SECCODE],

      [EntryType]             = CASE
                                  WHEN O.[REPOFLAG] = 'R' THEN
                                    CASE
                                      WHEN O.[VALUEENTRYTYPE] = 'Q' THEN 'QD' -- Количество + Дисконт
                                      ELSE 'SD'                               -- Сумма + Дисконт
                                    END
                                  --WHEN O.[VALUEENTRYTYPE] IN ('Q', 'V') THEN
                                  ELSE
                                    ISNULL(O.[VALUEENTRYTYPE], 'Q')
                                    +
                                    CASE
                                      WHEN O.[MKTLIMIT]  = 'M' THEN 'M'   -- (По рыночной цене)
                                      WHEN O.[MKTLIMIT]  = 'C' THEN 'C'   -- (По средневзвешенной цене)
                                      WHEN O.[ENTRYTYPE] = 'Y' THEN 'Y'   --  + Доходность
                                      WHEN O.[ENTRYTYPE] = 'W' THEN 'W'   -- (По средневзвешенной цене)
                                      ELSE 'P'                            -- Количество + Цена
                                    END
                                END,
      --[PriceEntryType]      = O.[ENTRYTYPE],
      --[PriceType]           = CASE WHEN O.[MKTLIMIT] IN ('M', 'C') THEN 'M' END,

      [PriceCurrency]       = CASE WHEN S.[QUOTEBASIS] = 'F' THEN '%' WHEN S.[CURRENCYID] IN ('SUR', 'RUR') THEN 'RUB' ELSE S.[CURRENCYID] END,
      [PriceValue]          = O.[PRICE],
      [Quantity]            = (O.[QUANTITY] + O.[HIDDEN]) * O.[LOTSIZE],
      [CostValue]           = CASE WHEN O.[REPOVALUE] <> 0 THEN O.[REPOVALUE] ELSE O.[VALUE] END, -- O.[VALUE],
      [TotalCoupon]         = NullIf(O.[ACCRUEDINT], 0),
      [Yield]               = NullIf(O.[YIELD], 0),
      [Period]              = O.[PERIOD],
      -- MICROSECONDS

      [RepoDays]            = CASE WHEN O.[REPOFLAG] = 'R' THEN O.[REPOTERM] END,
      [RepoPercents]        = NULL,
      [RepoPriceValue]      = NullIf(O.[PRICE2], 0),
      [RepoCostValue]       = NullIf(O.[REPO2VALUE], 0),
      [RepoRefundRate]      = NULL,

      [Discount]            = NullIf(O.[STARTDISCOUNT], 0),

      [ActivationType]      = NullIf(O.[ACTIVATIONTYPE], ''),
      [ActivationDateTime]  = CASE WHEN O.[ACTIVATIONTIME] <> '' THEN CAST(CAST(O.[CreateDateTime] AS Date) AS DateTime) + CAST(O.[ACTIVATIONTIME] AS DateTime) END,
      [CalculationCurrency] = CASE WHEN S.[CURRENCYID] IN ('SUR', 'RUR') THEN 'RUB' ELSE S.[CURRENCYID] END,

      [RemoteTradeFirm]     = NULL,
      [RemoteTradeAccount]  = NULL

      --[MarketMaker]       = CASE WHEN O.[MARKETMAKER] = 'Y' THEN CAST(1 AS Bit) END
      --[BANKACCID]         = O.[BANKACCID],
    FROM [dbo].[Mcx_ORDERS] O
    INNER JOIN [dbo].[Mcx_BOARDS] B on O.[SECBOARD] = B.[BOARDID] AND B.[MARKETID] <> 'TECH'
    INNER JOIN [dbo].[Mcx_SECURITIES] S on O.[SECBOARD] = S.[SECBOARD] AND O.[SECCODE] = S.[SECCODE]
    CROSS APPLY (VALUES([Pub].[Trim Right](O.[BROKERREF], '/'))) BR1([BROKERREF])
    WHERE O.[FIRMID] = II.[Account]
      AND O.[CreateDateTime] >= @DateTimeBegin AND O.[CreateDateTime] < @DateTimeEnd

    UNION ALL

    SELECT
      [OrderNo]             = O.[DEALNO],
      [Reason]              = CASE
                                WHEN O.[SECBOARD] IN ('TADM', 'NADM') THEN 'O'
                              END,
      [Type]                = CASE WHEN B.[MARKETID] IN ('RPS') THEN 'S' WHEN B.[MARKETID] IN ('RPNG') THEN 'O' END,
      [Kind]                = CASE WHEN O.[REPOENTRY] <> 0 THEN 'R' END,
      [BuySell]             = O.[BUYSELL],
      [CreateDateTime]      = CAST(CAST(O.[CreateDateTime] AS Date) AS DateTime) + CAST(O.[DEALTIME] AS DateTime),
      [ExpireDateTime]      = CASE
                                WHEN O.[ACTIVATIONDATE] IS NOT NULL THEN CAST(O.[ACTIVATIONDATE] AS DateTime)
                                ELSE CAST(CAST(O.[CreateDateTime] AS Date) AS DateTime)
                              END
                              + CAST('23:59:59' AS DateTime),
      [UpdateDateTime]      = CASE
                                WHEN O.[UPDATETIME] <> '' THEN CAST(CAST(O.[CreateDateTime] AS Date) AS DateTime) + CAST(O.[UPDATETIME] AS DateTime)
                                WHEN O.[STATUS] IN ('M', 'W', 'C', 'F', 'R') THEN O.[CreateDateTime]
                              END,

      [CloseStatus]         = CASE O.[STATUS]
                                WHEN 'O' THEN NULL
                                WHEN 'T' THEN NULL
                                WHEN 'E' THEN NULL
                                WHEN 'M' THEN 'E'
                                WHEN 'W' THEN 'C'
                                ELSE 'R'
                              END,
      [RejectReason]        = CASE O.[STATUS]
                                WHEN 'C' THEN N'Снята торговой системой'
                                WHEN 'F' THEN N'Отклонена контрагентом'
                                WHEN 'R' THEN N'Отклонена торговой системой'
                              END,

      [CannotBeSplitted]    = Cast(1 AS Bit),
      [ExecuteOrCancel]     = Cast(1 AS Bit),

      [UserId]              = O.[USERID],
      [TradeAccount]        = O.[FIRMID],
      [TradeAccountPart]    = O.[ACCOUNT],
      [TradeCode]           = NullIf(O.[CLIENTCODE], ''),
      [BrokerRef]           = NullIf(NullIf(BR1.[BROKERREF], O.[CLIENTCODE]), ''),
      [ExternalRef]         = NullIf(O.[EXTREF], ''),

      [TradeBoard]            = O.[SECBOARD],
      [ScheduleRulesCode]     = O.[SETTLECODE],
      [ScheduleRulesDate]     = Cast(O.[SETTLEDATE] AS Date),
      [InstrumentKind]        = 'S',
      [InstrumentCode]        = O.[SECCODE],

      [EntryType]           = CASE
                                WHEN O.[REPOENTRY] = 1 THEN 'QP%'
                                WHEN O.[REPOENTRY] = 2 THEN 'Q%P'
                                WHEN O.[REPOENTRY] = 3 THEN 'QPP'
                                WHEN O.[REPOENTRY] = 4 THEN 'QS'
                                WHEN O.[REPOENTRY] = 5 THEN 'SD'
                                WHEN O.[REPOENTRY] = 6 THEN 'QD'
                                WHEN O.[REPOENTRY] = 7 THEN 'S'
                                WHEN O.[REPOENTRY] = 8 THEN 'Q'
                                WHEN O.[NEGDEALENTRY] = 'Q' THEN 'QP'
                                WHEN O.[NEGDEALENTRY] = 'V' THEN 'SM'
                              END,
      --[PriceEntryType]      = NULL,
      --[PriceType]           = CASE WHEN O.[REPOENTRY] IN (5,6,7,8) OR O.[NEGDEALENTRY] IN ('V') THEN 'M' END,

      [PriceCurrency]       = CASE WHEN S.[QUOTEBASIS] = 'F' THEN '%' WHEN S.[CURRENCYID] IN ('SUR', 'RUR') THEN 'RUB' ELSE S.[CURRENCYID] END,
      [PriceValue]          = O.[PRICE],
      [Quantity]            = O.[QUANTITY] * S.[LOTSIZE],
      [CostValue]           = CASE WHEN O.[REPOVALUE] <> 0 THEN O.[REPOVALUE] ELSE O.[VALUE] END, -- O.[VALUE],
      [TotalCoupon]         = NullIf(O.[ACCRUEDINT], 0),
      [Yield]               = NullIf(O.[YIELD], 0),
      [Period]              = O.[PERIOD],
      -- MICROSECONDS

      [RepoDays]            = CASE WHEN O.[REPOENTRY] <> 0 THEN O.[REPOTERM] END,
      [RepoPercents]        = NullIf(O.[REPORATE], 0),
      [RepoPriceValue]      = NullIf(O.[PRICE2], 0),
      [RepoCostValue]       = NullIf(O.[REPO2VALUE], 0),
      [RepoRefundRate]      = NullIf(O.[REFUNDRATE], 0),

      [Discount]            = NullIf(O.[STARTDISCOUNT], 0),

      [ActivationType]      = CASE WHEN O.[ACTIVATIONTIME] <> '' THEN 'T' END,
      [ActivationDateTime]  = CASE WHEN O.[ACTIVATIONTIME] <> '' THEN CAST(CAST(O.[ACTIVATIONDATE] AS Date) AS DateTime) + CAST(O.[ACTIVATIONTIME] AS DateTime) END,
      [CalculationCurrency] = CASE WHEN S.[CURRENCYID] IN ('SUR', 'RUR') THEN 'RUB' ELSE S.[CURRENCYID] END,

      [RemoteTradeFirm]     = NULL,
      [RemoteTradeAccount]  = O.[CPFIRMID]

      --[BANKACCID]         = O.[BANKACCID],
      --[MarketMaker]       = NULL,
    FROM [dbo].[Mcx_NEGDEALS] O
    INNER JOIN [dbo].[Mcx_BOARDS] B on O.SECBOARD = B.BOARDID
    --INNER JOIN [dbo].[Mcx_MARKETS] M ON B.[MARKETID] = M.[MARKETID]
    INNER JOIN [dbo].[Mcx_SECURITIES] S on O.[SECBOARD] = S.[SECBOARD] AND O.[SECCODE] = S.[SECCODE]
    CROSS APPLY (VALUES([Pub].[Trim Right](O.[BROKERREF], '/'))) BR1([BROKERREF])
    WHERE O.[FIRMID] = II.[Account]
      AND O.[CreateDateTime] >= @DateTimeBegin AND O.[CreateDateTime] < @DateTimeEnd
      AND O.[SECBOARD] NOT IN ('RFND', 'TRAN')
      -- AND B.[MARKETID] <> 'TECH'
  ) O

------------------------------------------------------------------------------------------
  UNION ALL
------------------------------------------------------------------------------------------

  SELECT
    [Exchange] = 'MICEX:C',
    O.*
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
    SELECT
      [OrderNo]               = O.[ORDERNO],
      [Reason]              = NULL,
      [Type]                  = NULL,
      [Kind]                  = NULL,
      [BuySell]               = O.[BUYSELL],
      [CreateDateTime]        = CAST(CAST(O.[CreateDateTime] AS Date) AS DateTime) + CAST(O.[ORDERTIME] AS DateTime),
      [ExpireDateTime]        = CASE
                                  WHEN O.[IMMCANCEL] IN ('N', 'W')
                                    THEN O.[CreateDateTime]
                                  ELSE
                                    CAST(CAST(O.[CreateDateTime] AS Date) AS DateTime)
                                    + CAST('23:59:59' AS DateTime)
                                END,
      [UpdateDateTime]        = CASE
                                  WHEN O.[UPDATETIME] <> '' THEN CAST(CAST(O.[CreateDateTime] AS Date) AS DateTime) + CAST(O.[UPDATETIME] AS DateTime)
                                  WHEN O.[STATUS] IN ('M', 'W', 'C', 'F', 'R') THEN O.[CreateDateTime]
                                END,
      --[Status]                = O.[STATUS],

      [CloseStatus]           = CASE O.[STATUS]
                                  WHEN 'O' THEN NULL
                                  WHEN 'T' THEN NULL
                                  WHEN 'E' THEN NULL
                                  WHEN 'M' THEN 'E'
                                  WHEN 'W' THEN 'C'
                                  ELSE 'R'
                                END,
      [RejectReason]          = CASE O.[STATUS]
                                  WHEN 'C' THEN N'Снята торговой системой'
                                  WHEN 'F' THEN N'Отклонена контрагентом'
                                  WHEN 'R' THEN N'Отклонена торговой системой'
                                END,
      [CannotBeSplitted]      = Cast(CASE WHEN O.[SPLITFLAG] = 'O' THEN 1 END AS Bit),
      [ExecuteOrCancel]       = Cast(CASE O.[IMMCANCEL] WHEN 'N' THEN 1 WHEN 'W' THEN 0 END AS Bit),
      [UserId]                = O.[USERID],
      [TradeAccount]          = O.[FIRMID],
      [TradeAccountPart]      = O.[ACCOUNT],
      [TradeCode]             = NullIf(O.[CLIENTCODE], ''),
      [BrokerRef]             = NullIf(NullIf(BR1.[BROKERREF], O.[CLIENTCODE]), ''),
      [ExternalRef]           = NullIf(O.[EXTREF], ''),

      [TradeBoard]            = O.[SECBOARD],
      [ScheduleRulesCode]     = S.[SETTLECODE],
      [ScheduleRulesDate]     = Cast(O.[SETTLEDATE] AS Date),
      [InstrumentKind]        = 'CA',
      [InstrumentCode]        = O.[SECCODE],

      [EntryType]             = CASE
                                  WHEN O.[MKTLIMIT] = 'L' THEN 'QP'
                                  ELSE 'QM'
                                END,

      --[PriceEntryType]        = O.[ENTRYTYPE],
      --[PriceType]             = CASE WHEN O.[MKTLIMIT] IN ('M', 'C') THEN 'M' END,

      [PriceCurrency]         = CASE WHEN S.[CURRENCYID] IN ('SUR', 'RUR') THEN 'RUR' ELSE S.[CURRENCYID] END,
      [PriceValue]            = O.[PRICE],
      [Quantity]              = (O.[QUANTITY] + O.[HIDDEN]) * S.[LOTSIZE],
      [CostValue]             = O.[VALUE],
      [TotalCoupon]           = NULL,
      [Yield]                 = NULL,
      [Period]                = O.[PERIOD],
      -- MICROSECONDS

      [RepoDays]              = NULL,
      [RepoPercents]          = NULL,
      [RepoPriceValue]        = NULL,
      [RepoCostValue]         = NULL,
      [RepoRefundRate]        = NULL,

      [Discount]              = NULL,

      [ActivationType]        = NULL,
      [ActivationDateTime]    = NULL,
      [CalculationCurrency]   = CASE WHEN S.[CURRENCYID] IN ('SUR', 'RUR') THEN 'RUB' ELSE S.[CURRENCYID] END,

      [RemoteTradeFirm]       = NULL,
      [RemoteTradeAccount]    = NULL

      --[MarketMaker]         = CASE WHEN O.[MARKETMAKER] = 'Y' THEN CAST(1 AS Bit) END
      --[BANKACCID]           = O.[BANKACCID],
    FROM [dbo].[Mcx_VR_ORDERS] O
    INNER JOIN [dbo].[Mcx_VR_BOARDS] B on O.[SECBOARD] = B.[BOARDID] AND B.[MARKETID] <> 'TECH'
    INNER JOIN [dbo].[Mcx_VR_SECURITIES] S on O.[SECBOARD] = S.[SECBOARD] AND O.[SECCODE] = S.[SECCODE]
    CROSS APPLY (VALUES([Pub].[Trim Right](O.[BROKERREF], '/'))) BR1([BROKERREF])
    WHERE O.[FIRMID] = II.[Account]
      AND O.[CreateDateTime] >= @DateTimeBegin AND O.[CreateDateTime] < @DateTimeEnd

    UNION ALL

    SELECT
      [OrderNo]               = O.[DEALNO],
      [Reason]              = NULL,
      [Type]                  = NULL,
      [Kind]                  = NULL,
      [BuySell]               = O.[BUYSELL],
      [CreateDateTime]        = CAST(CAST(O.[CreateDateTime] AS Date) AS DateTime) + CAST(O.[DEALTIME] AS DateTime),
      [ExpireDateTime]        = CAST(CAST(O.[CreateDateTime] AS Date) AS DateTime)
                                + CAST('23:59:59' AS DateTime),
      [UpdateDateTime]        = CASE
                                  WHEN O.[UPDATETIME] <> '' THEN CAST(CAST(O.[CreateDateTime] AS Date) AS DateTime) + CAST(O.[UPDATETIME] AS DateTime)
                                  WHEN O.[STATUS] IN ('M', 'W', 'C', 'F', 'R') THEN O.[CreateDateTime]
                                END,

      [CloseStatus]           = CASE O.[STATUS]
                                  WHEN 'O' THEN NULL
                                  WHEN 'T' THEN NULL
                                  WHEN 'E' THEN NULL
                                  WHEN 'M' THEN 'E'
                                  WHEN 'W' THEN 'C'
                                  ELSE 'R'
                                END,
      [RejectReason]          = CASE O.[STATUS]
                                  WHEN 'C' THEN N'Снята торговой системой'
                                  WHEN 'F' THEN N'Отклонена контрагентом'
                                  WHEN 'R' THEN N'Отклонена торговой системой'
                                END,

      [CannotBeSplitted]      = Cast(1 AS Bit),
      [ExecuteOrCancel]       = Cast(1 AS Bit),

      [UserId]                = O.[USERID],
      [TradeAccount]          = O.[FIRMID],
      [TradeAccountPart]      = O.[ACCOUNT],
      [TradeCode]             = NullIf(O.[CLIENTCODE], ''),
      [BrokerRef]             = NullIf(NullIf(BR1.[BROKERREF], O.[CLIENTCODE]), ''),
      [ExternalRef]           = NullIf(O.[EXTREF], ''),

      [TradeBoard]            = O.[SECBOARD],
      [ScheduleRulesCode]     = O.[SETTLECODE],
      [ScheduleRulesDate]     = Cast(O.[SETTLEDATE] AS Date),
      [InstrumentKind]        = 'CA',
      [InstrumentCode]        = O.[SECCODE],

      [EntryType]             = 'QP',
      --[PriceEntryType]        = NULL,
      --[PriceType]             = NULL,
      [PriceCurrency]         = CASE WHEN S.[CURRENCYID] IN ('SUR', 'RUR') THEN 'RUB' ELSE S.[CURRENCYID] END,
      [PriceValue]            = O.[PRICE],
      [Quantity]              = O.[QUANTITY] * S.[LOTSIZE],
      [CostValue]             = O.[VALUE],
      [TotalCoupon]           = NULL,
      [Yield]                 = NULL,
      [Period]                = O.[PERIOD],
      -- MICROSECONDS

      [RepoDays]              = NULL,
      [RepoPercents]          = NULL,
      [RepoPriceValue]        = NULL,
      [RepoCostValue]         = NULL,
      [RepoRefundRate]        = NULL,

      [Discount]              = NULL,

      [ActivationType]        = NULL,
      [ActivationDateTime]    = NULL,
      [CalculationCurrency]   = CASE WHEN S.[CURRENCYID] IN ('SUR', 'RUR') THEN 'RUB' ELSE S.[CURRENCYID] END,

      [RemoteTradeFirm]       = NULL,
      [RemoteTradeAccount]    = O.[CPFIRMID]

      --[BANKACCID]           = O.[BANKACCID],
      --[MarketMaker]         = NULL,
    FROM [dbo].[Mcx_VR_NEGDEALS] O
    INNER JOIN [dbo].[Mcx_VR_BOARDS] B on O.SECBOARD = B.BOARDID AND B.[MARKETID] <> 'TECH'
    INNER JOIN [dbo].[Mcx_VR_SECURITIES] S on O.[SECBOARD] = S.[SECBOARD] AND O.[SECCODE] = S.[SECCODE]
    CROSS APPLY (VALUES([Pub].[Trim Right](O.[BROKERREF], '/'))) BR1([BROKERREF])
    WHERE O.[FIRMID] = II.[Account]
      AND O.[CreateDateTime] >= @DateTimeBegin AND O.[CreateDateTime] < @DateTimeEnd
  ) O

------------------------------------------------------------------------------------------
  UNION ALL
------------------------------------------------------------------------------------------

  SELECT
    O.*
  FROM
  (
    SELECT
      [Exchange]              = 'FORTS',
      [OrderNo]               = I.[id_ord],
      [Reason]                = NULL,
      [Type]                  = NULL,
      [Kind]                  = NULL,
      [BuySell]               = CASE WHEN O.[dir] = 1 THEN 'B' ELSE 'S' END,
      [CreateDateTime]        = O.[moment],
      [ExpireDateTime]        = CASE WHEN O.[date_exp] <> Cast(0 AS DateTime) THEN O.[date_exp] + CAST('23:59:59' AS DateTime) ELSE sess.[end] END,
      [UpdateDateTime]        = I.[UpdateDateTime],

      [CloseStatus]           = CASE
                                  WHEN I.[action] = 2 AND I.[amount_rest] = 0 THEN 'E'
                                  WHEN I.[action] = 0 AND (I.[status] & 0x800000 <> 0) THEN 'R'
                                  WHEN I.[action] = 0 AND O.[date_exp] <> Cast(0 AS DateTime) AND Cast([moment] AS Date) > Cast([date_exp] AS Date) THEN 'R'
                                  WHEN I.[action] = 0 THEN 'C'
                                END,
      [RejectReason]          = CASE
                                  WHEN I.[action] = 0 AND (I.[status] & 0x800000 <> 0) THEN 'Кросс-сделка'
                                  WHEN I.[action] = 0 AND O.[date_exp] <> Cast(0 AS DateTime) AND Cast([moment] AS Date) > Cast([date_exp] AS Date) THEN 'Время действия истекло'
                                END,
      [CannotBeSplitted]      = NULL,
      [ExecuteOrCancel]       = NULL,
      [UserId]                = O.[login_from],
      [TradeAccount]          = BR.[FirmAccount],
      [TradeAccountPart]      = NULL,
      [TradeCode]             = BR.[ClientCode],
      [BrokerRef]             = NullIf(NullIf(BR1.[Comment], O.[client_code]), ''),
      [ExternalRef]           = Cast(NullIf(O.[Ext_id], 0) AS VarChar(20)),

      [TradeBoard]            = NULL,
      [ScheduleRulesCode]     = NULL,
      [ScheduleRulesDate]     = NULL, --sess.[end],
      [InstrumentKind]        = 'D' + CASE WHEN sc.[signs] & 0x02 <> 0 THEN 'M' ELSE 'P' END,
      [InstrumentCode]        = sc.[isin],

      [EntryType]             = CASE
                                  WHEN O.[status] & 0x01 <> 0 THEN 'QP'
                                  ELSE 'QM'
                                END,

      --[PriceEntryType]        = NULL,
      --[PriceType]             = CASE
      --                            WHEN O.[status] & 0x01 = 0 THEN 'M'
      --                          END,
      [PriceCurrency]         = '^',
      [Price]                 = O.[price],
      [Quantity]              = O.[amount],
      [CostValue]             = NULL,
      [TotalCoupon]           = NULL,
      [Yield]                 = NULL,
      [Period]                = NULL,

      [RepoDays]              = NULL,
      [RepoPercents]          = NULL,
      [RepoPriceValue]        = NULL,
      [RepoCostValue]         = NULL,
      [RepoRefundRate]        = NULL,

      [Discount]              = NULL,

      [ActivationType]        = NULL,
      [ActivationDateTime]    = NULL,
      [CalculationCurrency]   = 'RUB',

      [RemoteTradeFirm]       = NullIf(O.[broker_to_rts], ''),
      [RemoteTradeAccount]    = NullIf(O.[broker_to], '')
    FROM
    (
      SELECT
        [id_ord]          = CASE WHEN O.[id_ord1] <> 0 THEN O.[id_ord1] ELSE O.[id_ord] END,
        [UpdateDateTime]  = O.[moment],
        [action]          = O.[action],
        [status]          = O.[status],
        [amount_rest]     = O.[amount_rest],
        [Row:Number]      = ROW_NUMBER() OVER (PARTITION BY CASE WHEN O.[id_ord1] <> 0 THEN O.[id_ord1] ELSE O.[id_ord] END ORDER BY O.[id_ord] DESC, CASE O.[action] WHEN 0 THEN 0 WHEN 2 THEN 1 ELSE 3 END ASC)
      FROM [fortsgate].[dbo].[plaza2_OPT_orders_log] O
      WHERE O.[moment] >= @DateTimeBegin AND O.[moment] < @DateTimeEnd
      --(@FutTime IS NULL OR @DateTimeBegin < @FutTime) AND (@FutTime IS NULL OR O.[moment] < @FutTime) AND
    ) I
    INNER JOIN [fortsgate].[dbo].[plaza2_OPT_orders_log] O ON I.[id_ord] = O.[id_ord] AND O.[action] = 1

    CROSS APPLY (VALUES(SubString(O.[client_code], 1, 4), SubString(O.[client_code], 5, 3))) BR([FirmAccount], [ClientCode])
    CROSS APPLY (VALUES([Pub].[Trim Right](NullIf(O.[comment], ''), '/'))) BR1([comment])
		INNER LOOP JOIN [fortsgate].[dbo].[Plaza2_FUT_session] sess on sess.sess_id  = O.sess_id
    INNER LOOP JOIN [fortsgate].[dbo].[plaza2_OPT_opt_sess_contents] sc on sc.isin_id = O.isin_id  and sc.sess_id = O.sess_id

    WHERE I.[Row:Number] = 1
      AND BR.[FirmAccount] IN (SELECT I.Node.value('@NUMBER', 'VarChar(20)') FROM @@TradeAccounts.nodes('/SYSTEM[@CODE="FORTS"]/ACCOUNT') AS I(Node))
--------------
    UNION ALL
--------------
    SELECT
      [Exchange]              = CASE WHEN IK.[Sign:Spot] = 1 THEN 'RTSS' ELSE 'FORTS' END,
      [OrderNo]               = I.[id_ord],
      [Reason]                = NULL,
      [Type]                  = NULL,
      [Kind]                  = NULL,
      [BuySell]               = CASE WHEN O.[dir] = 1 THEN 'B' ELSE 'S' END,

      [CreateDateTime]        = O.[moment],
      [ExpireDateTime]          = CASE WHEN O.[date_exp] <> Cast(0 AS DateTime) THEN O.[date_exp] + CAST('23:59:59' AS DateTime) ELSE sess.[end] END,
      [UpdateDateTime]        = I.[UpdateDateTime],

      [CloseStatus]           = CASE
                                  WHEN I.[action] = 2 AND I.[amount_rest] = 0 THEN 'E'
                                  WHEN I.[action] = 0 AND (I.[status] & 0x800000 <> 0) THEN 'R'
                                  WHEN I.[action] = 0 AND O.[date_exp] <> Cast(0 AS DateTime) AND Cast([moment] AS Date) > Cast([date_exp] AS Date) THEN 'R'
                                  WHEN I.[action] = 0 THEN 'C'
                                END,
      [RejectReason]          = CASE
                                  WHEN I.[action] = 0 AND (I.[status] & 0x800000 <> 0) THEN 'Кросс-сделка'
                                  WHEN I.[action] = 0 AND O.[date_exp] <> Cast(0 AS DateTime) AND Cast([moment] AS Date) > Cast([date_exp] AS Date) THEN 'Время действия истекло'
                                END,

      [CannotBeSplitted]      = NULL,
      [ExecuteOrCancel]       = NULL,
      [UserId]                = O.[login_from],
      [TradeAccount]          = BR.[FirmAccount],
      [TradeAccountPart]      = NULL,
      [TradeCode]             = BR.[ClientCode],
      [BrokerRef]             = NullIf(NullIf(BR1.[Comment], O.[client_code]), ''),
      [ExternalRef]           = Cast(NullIf(O.[Ext_id], 0) AS VarChar(20)),

      [TradeBoard]            = NULL,
      [ScheduleRulesCode]     = NULL,
      [ScheduleRulesDate]     = CASE WHEN IK.[Sign:Spot] = 1 THEN Cast(sc.[d_exp] AS Date) END,
      [InstrumentKind]        = CASE WHEN IK.[Sign:Spot] = 1 THEN 'S' WHEN sc.[signs] & 0x02 <> 0 THEN 'DM' ELSE 'DP' END,
      [InstrumentCode]        = CASE WHEN IK.[Sign:Spot] = 1 THEN IsNull(RTSS2.[short_isin], RTSS1.[short_isin]) + 'S' ELSE sc.[isin] END,

      [EntryType]             = CASE
                                  WHEN O.[status] & 0x01 <> 0 THEN 'QP'
                                  ELSE 'QM'
                                END,

      --[PriceEntryType]        = NULL,
      --[PriceType]             = CASE
      --                            WHEN O.[status] & 0x01 = 0 THEN 'M'
      --                          END,
      [PriceCurrency]         = '^',
      [Price]                 = O.[price],
      [Quantity]              = O.[amount],
      [CostValue]             = NULL,
      [TotalCoupon]           = NULL,
      [Yield]                 = NULL,
      [Period]                = NULL,

      [RepoDays]              = NULL,
      [RepoPercents]          = NULL,
      [RepoPriceValue]        = NULL,
      [RepoCostValue]         = NULL,
      [RepoRefundRate]        = NULL,

      [Discount]              = NULL,

      [ActivationType]        = NULL,
      [ActivationDateTime]    = NULL,
      [CalculationCurrency]   = 'RUB',

      [RemoteTradeFirm]     = NullIf(O.[broker_to_rts], ''),
      [RemoteTradeAccount]  = NullIf(O.[broker_to], '')
    FROM
    (
      SELECT
        [id_ord]          = CASE WHEN O.[id_ord1] <> 0 THEN O.[id_ord1] ELSE O.[id_ord] END,
        [UpdateDateTime]  = O.[moment],
        [action]          = O.[action],
        [status]          = O.[status],
        [amount_rest]     = O.[amount_rest],
        [Row:Number]      = ROW_NUMBER() OVER (PARTITION BY CASE WHEN O.[id_ord1] <> 0 THEN O.[id_ord1] ELSE O.[id_ord] END ORDER BY O.[id_ord] DESC, CASE O.[action] WHEN 0 THEN 0 WHEN 2 THEN 1 ELSE 3 END ASC)
      FROM [fortsgate].[dbo].[plaza2_FUT_orders_log] O
      WHERE O.[moment] >= @DateTimeBegin AND O.[moment] < @DateTimeEnd
      --(@FutTime IS NULL OR @DateTimeBegin < @FutTime) AND (@FutTime IS NULL OR O.[moment] < @FutTime) AND
    ) I
    INNER JOIN [fortsgate].[dbo].[plaza2_FUT_orders_log] O ON I.[id_ord] = O.[id_ord] AND O.[action] = 1
    CROSS APPLY (VALUES(SubString(O.[client_code], 1, 4), SubString(O.[client_code], 5, 3))) BR([FirmAccount], [ClientCode])
    CROSS APPLY (VALUES([Pub].[Trim Right](NullIf(O.[comment], ''), '/'))) BR1([comment])
		INNER LOOP JOIN [fortsgate].[dbo].[Plaza2_FUT_session] sess on sess.sess_id  = O.sess_id
    INNER LOOP JOIN [fortsgate].[dbo].[plaza2_fut_sess_contents] sc on sc.isin_id = O.isin_id  and sc.sess_id = O.sess_id
    CROSS APPLY (VALUES(CASE WHEN sc.[signs] & 0x0c <> 0 THEN 1 ELSE 0 END)) IK([Sign:Spot])
    LEFT LOOP JOIN [fortsgate].[dbo].[plaza2_fut_sess_contents] RTSS1 ON IK.[Sign:Spot] = 1 AND sc.ticker = RTSS1.isin_id and O.sess_id = RTSS1.sess_id
    LEFT LOOP JOIN [fortsgate].[dbo].[plaza2_fut_sess_contents] RTSS2 ON IK.[Sign:Spot] = 1 AND RTSS1.ticker <> RTSS1.isin_id AND RTSS1.ticker = RTSS2.isin_id and O.sess_id = RTSS2.sess_id
    WHERE I.[Row:Number] = 1
      AND BR.[FirmAccount] IN (SELECT I.Node.value('@NUMBER', 'VarChar(20)') FROM @@TradeAccounts.nodes('/SYSTEM[@CODE="FORTS"]/ACCOUNT') AS I(Node))
  ) O
  WHERE @Forts = 1
  ORDER BY 7, 1, 2
  OPTION (FORCE ORDER, MAXDOP 2)
GO