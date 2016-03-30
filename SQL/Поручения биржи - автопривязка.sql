EXEC [System].[Session Variable::Set] 'DEBUG', 1
EXEC [BackOffice].[Log::Begin] 1

    INSERT INTO [BackOffice].[Deals(Update)]([Id], [Row:Fields], [ITEMS])
    SELECT
      [Id]          = DB.[Id],
      [Row:Fields]  = 'ITEMS',
      [ITEMS]       =
        [Pub].[Concat]
        (
          N'<RECORD ACTION="U" Action_Id="' + Cast(DB.[Action_Id] AS NVarChar)
            + N'" Item_Id="' + Cast(DB.[Item_Id] AS NVarChar)
            + N'" ' + DB.[Party] + N'Order_Id="' + Cast(DO.[Id] AS NVarChar(10))
            + N'" FIELDS="' + DB.[Party] + N'Order_Id"/>'
          , N'
'
        )
    FROM [BackOffice].[Deals#Bargains(Expanded)] DB
    INNER LOOP JOIN [BackOffice].[Orders] DO ON DO.[PlaceOrderNo] IS NOT NULL AND DB.[Firm_Id] = DO.[Firm_Id] AND DB.[PlaceOrderNo] = DO.[PlaceOrderNo]
    WHERE DB.[Order_Id] IS NULL
      AND DB.[CodeSource] IN ('EXCH', 'SYS') AND DB.[PlaceOrderNo] IS NOT NULL
    GROUP BY DB.[Id]
    OPTION (FORCE ORDER, MAXDOP 1)

EXEC [BackOffice].[Log::RollBack]
