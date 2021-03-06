DECLARE
  @CommunicationKind_Id TinyInt = 1, -- PERSONAL
  @TimeBegin            Time    = '09:30',
  @TimeEnd              Time    = '18:30',
  @DelayMinMinutes      Int     = 10,
  @DelayRandomSeconds   Int     = 3600,
  @Accepter_Id          Int     = 15


    EXEC [BackOffice].[Log::Begin] 1
BEGIN TRAN

DECLARE @Identities TABLE([Id] Int NOT NULL)

    INSERT INTO [BackOffice].[Orders(Insert)#Deal]
    (
      [Row:Index], [Row:Fields], 
      --[GUId], 
      --[UpperOrganization_Id], 
      [Type_Id], [CreateDateTime], [DateTime], [Accepter_Id], [ExpireDateTime], [AccountPart_Id],
      [Instrument_Id], [Quantity], [CommunicationKind_Id], 
      --[RemotePerson_Id], 
      [Place_Id], [TradeBoard_Id], 
      --[TradePeriod_Id], 
      [ScheduleRules_Id],
      [EntryType_Id], [PriceCurrency_Lnk], [PriceValue], [CostCurrency_Id], [CostValue],
      --[Yield], [Discount],
      --[SignCannotBeSplitted], [SignExecuteOrCancel],
      [CalculationCurrency_Id], 
      --[PlannedDeliveryDateTime], [PlannedPaymentDateTime],
      [RepoDirect], [RepoCalendarKind_Id], [RepoDays], [RepoPercentsBasis], [RepoPercents], 
      --[RepoPriceValue], [RepoCostValue],
      [PlaceOrderNo], [ExternalReference], [TradeCode], [ExternalBriefCaseCode], 
      --[TradeUserId], [RemoteTradeFirmCode], [RemoteTradeAccount],
      --[ActivationType_Id], [ActivationDateTime],
      [ACTIONS],
      [Comments]
    )
    SELECT
--      DB.[CodeInstrument], DB.[CodePlace], DB.[CodeSource], DB.[CodeKind], DB.[Firm_Id],
      [Row:Index]                 = ROW_NUMBER() OVER(ORDER BY DB.[DateTime]),
      [Row:Fields]                = 'GUId,Type_Id,CreateDateTime,DateTime,ExpireDateTime,AccountPart_Id,Instrument_Id,Quantity,CommunicationKind_Id,RemotePerson_Id,Comments,Place_Id,TradeBoard_Id,TradePeriod_Id,ScheduleRules_Id,EntryType_Id,PriceCurrency_Lnk,PriceValue,CostCurrency_Id,CostValue,Yield,Discount,SignCannotBeSplitted,SignExecuteOrCancel,CalculationCurrency_Id,PlannedDeliveryDateTime,PlannedPaymentDateTime,RepoDirect,RepoCalendarKind_Id,RepoDays,RepoPercentsBasis,RepoPercents,RepoPriceValue,RepoCostValue,PlaceOrderNo,ExternalReference,TradeCode,ExternalBriefCaseCode,TradeUserId,RemoteTradeFirmCode,RemoteTradeAccount,ActivationType_Id,ActivationDateTime,ACTIONS',
      --[GUId]                      = DB.[GUId],
      [Type_Id]                   = DOT.[Type_Id],
      [CreateDateTime]            = T2.[DateTime],
      [DateTime]                  = T2.[DateTime],
      [Accepter_Id]               = @Accepter_Id,

      [ExpireDateTime]            = [Pub].[DateTime::Floor To Date](DB.[DateTime]) + Cast('23:59:59' AS DateTime),
      [AccountPart_Id]            = DB.[InstrumentAccountPart_Id],
      [Instrument_Id]             = DB.[Instrument_Id],
      [Quantity]                  = DB.[Quantity],
      [CommunicationKind_Id]      = @CommunicationKind_Id,
      --[RemotePerson_Id]           = DB.[RemotePerson_Id],
      [Place_Id]                  = DB.[Place_Id],
      [TradeBoard_Id]             = DB.[TradeBoard_Id],
      [ScheduleRules_Id]          = DB.[ScheduleRules_Id],
      [EntryType_Id]              = CASE WHEN DB.[CodeKind] = 'REPO' THEN 'QP%' ELSE 'QP' END,
      --[PriceEntryType_Id]         = ?,
      --[PriceType_Id]              = ?,
      [PriceCurrency_Lnk]         = DB.[PriceCurrency_Lnk],
      [PriceValue]                = DB.[PriceValue],
      [CostCurrency_Id]           = DB.[CostCurrency_Id],
      [CostValue]                 = DB.[CostValue],
      --[SignCannotBeSplitted]      = NULL,
      --[SignExecuteOrCancel]       = DB.[SignExecuteOrCancel],
      [CalculationCurrency_Id]    = DB.[TransactionCurrency_Id],
      --[PlannedDeliveryDateTime]   = DB.[PlannedDeliveryDateTime],
      --[PlannedPaymentDateTime]    = DB.[PlannedPaymentDateTime],
      [RepoDirect]                = DB.[RepoDirect],
      [RepoCalendarKind_Id]       = CASE WHEN DB.[CodeKind] = 'REPO' THEN 'CD' END,
      [RepoDays]                  = CASE WHEN DB.[CodeKind] <> 'REPO' THEN NULL WHEN DB.[RepoDirect] = 'M' THEN DateDiff(Day, DB.[PaymentDate], DB.[RepoPaymentDate]) ELSE DateDiff(Day, DB.[DeliveryDate], DB.[RepoDeliveryDate]) END,
      [RepoPercentsBasis]         = DB.[RepoPercentsBasis],
      [RepoPercents]              = DB.[RepoPercents],
      --[RepoPriceValue]            = DB.[RepoPriceValue],
      --[RepoCostValue]             = DB.[RepoCostValue],
      [PlaceOrderNo]              = DB.[PlaceOrderNo],
      [ExternalReference]         = DB.[ExternalReference],
      [TradeCode]                 = DB.[TradeCode],
      [ExternalBriefCaseCode]     = DB.[ExternalBriefCaseCode],
      [ACTIONS]                   =
      (
        SELECT
          [ACTION]    = 'I',
          [Status_Id] = 'E',
          [DateTime]  = DB.[DateTime]
        FOR XML RAW('RECORD'), TYPE
      ),
      [Comments]                  = Cast(DB.[Id] AS VarChar(10))
                                      + N':' + Cast(DB.[PartIndex] AS Char(1))
                                      + N':' + Cast(DB.[Item_Id] AS VarChar(10))
                                      + N':' + IsNull(Cast(DB.[SignSecondary] AS Char(1)), '0')
    FROM [BackOffice].[Deals#Bargains(Expanded)] DB
    INNER JOIN [BackOffice].[Dic:Deals:Orders:Types] DOT ON DB.[Type_Id] = DOT.[DealType_Id] AND DB.[Direction] = DOT.[Direction] AND DB.[PartKind_Id] = DOT.[PartKind_Id] AND DB.[Reason_Id] = DOT.[Reason_Id]
    CROSS APPLY
    (
      VALUES (DateAdd(Second, -Cast(RAND() * @DelayRandomSeconds AS Int), DateAdd(Minute, -@DelayMinMinutes, DB.[DateTime])))
    ) T1 ([DateTime])
    CROSS APPLY
    (
      VALUES
      (
        CASE
          WHEN Cast(T1.[DateTime] AS Time) < @TimeBegin THEN DateAdd(Second, -Cast(RAND() * 60 AS Int), DB.[DateTime])
          ELSE T1.[DateTime]
        END
      )
    ) T2 ([DateTime])
    WHERE DB.[Order_Id] IS NULL
      AND DB.[CodePlace] IN ('OTC')
      AND DB.[InstrumentAccountPart_Id] IS NOT NULL
AND CAST(DB.[DateTime] AS Time) >= CAST(@TimeBegin AS Time)
    OPTION (FORCE ORDER, MAXDOP 1)

INSERT INTO @Identities([Id])
SELECT [Identity] FROM [System].[Scope Identities]

SELECT
  O.[Id],
  [Comments]      = NULL,
  [Deal_Id]       = [Pub].[Extract Value](O.[Comments], 1, ':'),
  [PartIndex]     = [Pub].[Extract Value](O.[Comments], 2, ':'),
  [Item_Id]       = [Pub].[Extract Value](O.[Comments], 4, ':'),
  [SignSecondary] = [Pub].[Extract Value](O.[Comments], 3, ':')
FROM @Identities I
INNER JOIN [BackOffice].[Orders] O ON I.[Id] = O.[Id]



ROLLBACK
    EXEC [BackOffice].[Log::RollBack]

