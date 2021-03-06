DECLARE @DATA TABLE
(
  [Code]            VarChar(100) COLLATE Cyrillic_General_BIN NOT NULL PRIMARY KEY CLUSTERED,
  [CodeInstrument]  VarChar(20)  COLLATE Cyrillic_General_BIN NOT NULL,
  [Name]          NVarChar(100)   NOT NULL, 
  [CanBeConsolidatedTransaction]  Bit, 
  [CanBeSimpleTransaction]        Bit, 
  [NameRoot]                      Bit, 
  [GUIRoot]                       Bit,
  [DecActiveState_Id]             TinyInt, 
  [IncActiveState_Id]             TinyInt
)

;WITH CTE AS
(
  SELECT
    [BS] = CASE WHEN T.[Code] LIKE '%:BUY:%' THEN 'B' ELSE 'S' END,
    [ICode] =
      CASE
        WHEN T.[Code] LIKE 'BDM:MON:DOC:%' THEN 'MON:DOC'
        WHEN T.[Code] LIKE 'BDM:MON:ELE:%' THEN 'MON:ELE'
        WHEN T.[Code] LIKE 'BDM:DER:DOC:%' THEN 'DER:DOC'
        WHEN T.[Code] LIKE 'BDM:DER:ELE:%' THEN 'DER:ELE'
        WHEN T.[Code] LIKE 'BDM:SEC:DOC:%' THEN 'SEC:DOC'
        WHEN T.[Code] LIKE 'BDM:SEC:ELE:%' THEN 'SEC:ELE'
      END,
    [Code] = T.[Code],
    [Name] = T.[Name]
  FROM [BackOffice].[Dic:Transactions:Types] T
  WHERE (T.[Code] LIKE 'BDM:%:BUY:%' OR T.[Code] LIKE 'BDM:%:SALE:%')
    AND T.[Code] NOT LIKE '%:ACT:%' AND T.[Code] NOT LIKE '%:ACT'
    AND T.[Code] NOT LIKE '%:SRV:%' AND T.[Code] NOT LIKE '%:SRV'
    AND T.[Code] NOT LIKE '%:BRK:%' AND T.[Code] NOT LIKE '%:BRK'
    --AND T.[Code] NOT LIKE '%:CLN:ORD:%' AND T.[Code] NOT LIKE '%:CLN:ORD'
    --AND (T.[Code] NOT LIKE '%:CLN:ORD*' AND T.[Code] NOT LIKE '%:CLN:ORD*:%' OR T.[Code] NOT LIKE '%:EXCH:%' AND T.[Code] NOT LIKE '%:SYS:%')
    AND EXISTS
    (
      SELECT 1 FROM [BackOffice].[Dic:Transactions:Types] TF WHERE TF.[Parent_Id] = T.[Id]
        AND TF.[Code] NOT LIKE '%:DEL' AND TF.[Code] NOT LIKE '%:CHN' AND TF.[Code] NOT LIKE '%:NEW'
    )
)
  INSERT INTO @DATA([Name], [Code], [CodeInstrument], [DecActiveState_Id], [IncActiveState_Id], [NameRoot], [GUIRoot])
  SELECT V.[Name], V.[Code], SubString(V.[Code], 5, 7), V.[DecActiveState_Id], V.[IncActiveState_Id], V.[NameRoot], V.[GUIRoot]
  FROM
  (
    VALUES
    -- MON:ELE
    ('Перерасчёт', 'BDM:MON:ELE:RECALC', NULL, NULL, 1, 1),

    ('Поставка по сделке', 'BDM:MON:ELE:DEC:DELIVERY', NULL, NULL, 1, 1),
    ('Поставка по сделке', 'BDM:MON:ELE:INC:DELIVERY', NULL, NULL, 1, 1),

    ('Поставка по сделке', 'BDM:MON:ELE:PRE-DEC:DELIVERY', NULL, NULL, 1, 1),
    ('Поставка по сделке', 'BDM:MON:ELE:PRE-INC:DELIVERY', NULL, NULL, 1, 1),

    ('Оплата по сделке', 'BDM:MON:ELE:DEC:PAYMENT', NULL, NULL, 1, 1),
    ('Оплата по сделке', 'BDM:MON:ELE:INC:PAYMENT', NULL, NULL, 1, 1),
    ('Оплаты по сделке', 'BDM:MON:ELE:RECALC:PAYMENT', NULL, NULL, 1, 1),

    ('Оплата по сделке', 'BDM:MON:ELE:PRE-DEC:PAYMENT', NULL, NULL, 1, 1),
    ('Оплата по сделке', 'BDM:MON:ELE:PRE-INC:PAYMENT', NULL, NULL, 1, 1),

    --('По сделке', 'BDM:MON:ELE:REJ-DEC:DEAL', NULL, NULL, 1, 1),
    --('По сделке', 'BDM:MON:ELE:REJ-INC:DEAL', NULL, NULL, 1, 1),

    -- MON:ELE:DELIVERY

    --('С Валютой', 'BDM:MON:ELE:DEC:DELIVERY:MON', NULL, NULL, 0, 0),
    --('С Валютой', 'BDM:MON:ELE:INC:DELIVERY:MON', NULL, NULL, 0, 0),
    --('С Валютой', 'BDM:MON:ELE:PRE-DEC:DELIVERY:MON', NULL, NULL, 0, 0),
    --('С Валютой', 'BDM:MON:ELE:PRE-INC:DELIVERY:MON', NULL, NULL, 0, 0),

    --('ЭДС', 'BDM:MON:ELE:DEC:DELIVERY:MON:ELE', NULL, NULL, 0, 0),
    --('ЭДС', 'BDM:MON:ELE:INC:DELIVERY:MON:ELE', NULL, NULL, 0, 0),
    --('ЭДС', 'BDM:MON:ELE:PRE-DEC:DELIVERY:MON:ELE', NULL, NULL, 0, 0),
    --('ЭДС', 'BDM:MON:ELE:PRE-INC:DELIVERY:MON:ELE', NULL, NULL, 0, 0),

    -- MON:ELE:PAYMENT

    ('С Валютой', 'BDM:MON:ELE:DEC:PAYMENT:MON', NULL, NULL, 0, 0),
    ('С Валютой', 'BDM:MON:ELE:INC:PAYMENT:MON', NULL, NULL, 0, 0),
    ('С Валютой', 'BDM:MON:ELE:RECALC:PAYMENT:MON', NULL, NULL, 0, 0),
    ('С Валютой', 'BDM:MON:ELE:PRE-DEC:PAYMENT:MON', NULL, NULL, 0, 0),
    ('С Валютой', 'BDM:MON:ELE:PRE-INC:PAYMENT:MON', NULL, NULL, 0, 0),
    --('С Валютой', 'BDM:MON:ELE:REJ-DEC:DEAL:MON', NULL, NULL, 0, 0),
    --('С Валютой', 'BDM:MON:ELE:REJ-INC:DEAL:MON', NULL, NULL, 0, 0),

    ('ЭДС', 'BDM:MON:ELE:DEC:PAYMENT:MON:ELE', NULL, NULL, 0, 0),
    ('ЭДС', 'BDM:MON:ELE:INC:PAYMENT:MON:ELE', NULL, NULL, 0, 0),
    ('ЭДС', 'BDM:MON:ELE:RECALC:PAYMENT:MON:ELE', NULL, NULL, 0, 0),
    ('ЭДС', 'BDM:MON:ELE:PRE-DEC:PAYMENT:MON:ELE', NULL, NULL, 0, 0),
    ('ЭДС', 'BDM:MON:ELE:PRE-INC:PAYMENT:MON:ELE', NULL, NULL, 0, 0),
    --('ЭДС', 'BDM:MON:ELE:REJ-DEC:DEAL:MON:ELE', NULL, NULL, 0, 0),
    --('ЭДС', 'BDM:MON:ELE:REJ-INC:DEAL:MON:ELE', NULL, NULL, 0, 0),

    ('С ПИ', 'BDM:MON:ELE:DEC:PAYMENT:DER', NULL, NULL, 0, 0),
    ('С ПИ', 'BDM:MON:ELE:INC:PAYMENT:DER', NULL, NULL, 0, 0),
    ('С ПИ', 'BDM:MON:ELE:RECALC:PAYMENT:DER', NULL, NULL, 0, 0),

    ('ДПИ', 'BDM:MON:ELE:DEC:PAYMENT:DER:DOC', NULL, NULL, 0, 0),
    ('ДПИ', 'BDM:MON:ELE:INC:PAYMENT:DER:DOC', NULL, NULL, 0, 0),
    ('ДПИ', 'BDM:MON:ELE:RECALC:PAYMENT:DER:DOC', NULL, NULL, 0, 0),

    ('ЭПИ', 'BDM:MON:ELE:DEC:PAYMENT:DER:ELE', NULL, NULL, 0, 0),
    ('ЭПИ', 'BDM:MON:ELE:INC:PAYMENT:DER:ELE', NULL, NULL, 0, 0),
    ('ЭПИ', 'BDM:MON:ELE:RECALC:PAYMENT:DER:ELE', NULL, NULL, 0, 0),

    ('С ЦБ', 'BDM:MON:ELE:DEC:PAYMENT:SEC', NULL, NULL, 0, 0),
    ('С ЦБ', 'BDM:MON:ELE:INC:PAYMENT:SEC', NULL, NULL, 0, 0),
    ('С ЦБ', 'BDM:MON:ELE:RECALC:PAYMENT:SEC', NULL, NULL, 0, 0),

    ('ДЦБ', 'BDM:MON:ELE:DEC:PAYMENT:SEC:DOC', NULL, NULL, 0, 0),
    ('ДЦБ', 'BDM:MON:ELE:INC:PAYMENT:SEC:DOC', NULL, NULL, 0, 0),
    ('ДЦБ', 'BDM:MON:ELE:RECALC:PAYMENT:SEC:DOC', NULL, NULL, 0, 0),

    ('ЭЦБ', 'BDM:MON:ELE:DEC:PAYMENT:SEC:ELE', NULL, NULL, 0, 0),
    ('ЭЦБ', 'BDM:MON:ELE:INC:PAYMENT:SEC:ELE', NULL, NULL, 0, 0),
    ('ЭЦБ', 'BDM:MON:ELE:RECALC:PAYMENT:SEC:ELE', NULL, NULL, 0, 0),

    ('С ПИ', 'BDM:MON:ELE:PRE-DEC:PAYMENT:DER', NULL, NULL, 0, 0),
    ('С ПИ', 'BDM:MON:ELE:PRE-INC:PAYMENT:DER', NULL, NULL, 0, 0),
    --('С ПИ', 'BDM:MON:ELE:REJ-DEC:DEAL:DER', NULL, NULL, 0, 0),
    --('С ПИ', 'BDM:MON:ELE:REJ-INC:DEAL:DER', NULL, NULL, 0, 0),

    ('ДПИ', 'BDM:MON:ELE:PRE-DEC:PAYMENT:DER:DOC', NULL, NULL, 0, 0),
    ('ДПИ', 'BDM:MON:ELE:PRE-INC:PAYMENT:DER:DOC', NULL, NULL, 0, 0),
    --('ДПИ', 'BDM:MON:ELE:REJ-DEC:DEAL:DER:DOC', NULL, NULL, 0, 0),
    --('ДПИ', 'BDM:MON:ELE:REJ-INC:DEAL:DER:DOC', NULL, NULL, 0, 0),

    ('ЭПИ', 'BDM:MON:ELE:PRE-DEC:PAYMENT:DER:ELE', NULL, NULL, 0, 0),
    ('ЭПИ', 'BDM:MON:ELE:PRE-INC:PAYMENT:DER:ELE', NULL, NULL, 0, 0),
    --('ЭПИ', 'BDM:MON:ELE:REJ-DEC:DEAL:DER:ELE', NULL, NULL, 0, 0),
    --('ЭПИ', 'BDM:MON:ELE:REJ-INC:DEAL:DER:ELE', NULL, NULL, 0, 0),

    ('С ЦБ', 'BDM:MON:ELE:PRE-DEC:PAYMENT:SEC', NULL, NULL, 0, 0),
    ('С ЦБ', 'BDM:MON:ELE:PRE-INC:PAYMENT:SEC', NULL, NULL, 0, 0),
    --('С ЦБ', 'BDM:MON:ELE:REJ-DEC:DEAL:SEC', NULL, NULL, 0, 0),
    --('С ЦБ', 'BDM:MON:ELE:REJ-INC:DEAL:SEC', NULL, NULL, 0, 0),

    ('ДЦБ', 'BDM:MON:ELE:PRE-DEC:PAYMENT:SEC:DOC', NULL, NULL, 0, 0),
    ('ДЦБ', 'BDM:MON:ELE:PRE-INC:PAYMENT:SEC:DOC', NULL, NULL, 0, 0),
    --('ДЦБ', 'BDM:MON:ELE:REJ-DEC:DEAL:SEC:DOC', NULL, NULL, 0, 0),
    --('ДЦБ', 'BDM:MON:ELE:REJ-INC:DEAL:SEC:DOC', NULL, NULL, 0, 0),

    ('ЭЦБ', 'BDM:MON:ELE:PRE-DEC:PAYMENT:SEC:ELE', NULL, NULL, 0, 0),
    ('ЭЦБ', 'BDM:MON:ELE:PRE-INC:PAYMENT:SEC:ELE', NULL, NULL, 0, 0),
    --('ЭЦБ', 'BDM:MON:ELE:REJ-DEC:DEAL:SEC:ELE', NULL, NULL, 0, 0),
    --('ЭЦБ', 'BDM:MON:ELE:REJ-INC:DEAL:SEC:ELE', NULL, NULL, 0, 0),

    -- DER
    ('Поставка по сделке', 'BDM:DER:DOC:DEC:DELIVERY', NULL, NULL, 1, 1),
    ('Поставка по сделке', 'BDM:DER:DOC:INC:DELIVERY', NULL, NULL, 1, 1),

    ('Поставка по сделке', 'BDM:DER:ELE:DEC:DELIVERY', NULL, NULL, 1, 1),
    ('Поставка по сделке', 'BDM:DER:ELE:INC:DELIVERY', NULL, NULL, 1, 1),

    ('Поставка по сделке', 'BDM:DER:DOC:PRE-DEC:DELIVERY', NULL, NULL, 1, 1),
    ('Поставка по сделке', 'BDM:DER:DOC:PRE-INC:DELIVERY', NULL, NULL, 1, 1),

    ('Поставка по сделке', 'BDM:DER:ELE:PRE-DEC:DELIVERY', NULL, NULL, 1, 1),
    ('Поставка по сделке', 'BDM:DER:ELE:PRE-INC:DELIVERY', NULL, NULL, 1, 1),

    -- SEC
    ('Поставка по сделке' , 'BDM:SEC:DOC:DEC:DELIVERY', NULL, NULL, 1, 1),
    ('Поставка по сделке' , 'BDM:SEC:DOC:INC:DELIVERY', NULL, NULL, 1, 1),

    ('Поставка по сделке' , 'BDM:SEC:ELE:DEC:DELIVERY', NULL, NULL, 1, 1),
    ('Поставка по сделке' , 'BDM:SEC:ELE:INC:DELIVERY', NULL, NULL, 1, 1),

    ('Поставка по сделке' , 'BDM:SEC:DOC:PRE-DEC:DELIVERY', NULL, NULL, 1, 1),
    ('Поставка по сделке' , 'BDM:SEC:DOC:PRE-INC:DELIVERY', NULL, NULL, 1, 1),

    ('Поставка по сделке' , 'BDM:SEC:ELE:PRE-DEC:DELIVERY', NULL, NULL, 1, 1),
    ('Поставка по сделке' , 'BDM:SEC:ELE:PRE-INC:DELIVERY', NULL, NULL, 1, 1)

  ) V([Name], [Code], [DecActiveState_Id], [IncActiveState_Id], [NameRoot], [GUIRoot])

  UNION ALL

  SELECT DISTINCT
    SY.[Name],
    SY.[Code],
    CTE.[ICode],
    [DecActiveState_Id]             = STD.[Id],
    [IncActiveState_Id]             = STI.[Id],
    [NameRoot]                      = 0,
    [GUIRoot]                       = 0
  FROM CTE
  CROSS APPLY
  (
    SELECT [Final] = CASE WHEN EXISTS(SELECT 1 FROM CTE F WHERE F.[Code] LIKE CTE.[Code] + ':%') THEN 0 ELSE 1 END
  ) F
  CROSS APPLY
  (
    VALUES
    (''),
    ('PRE-')
    --('REJ-')
  ) S1([Prefix])
  CROSS APPLY
  (
    SELECT
      [PCode] = 'DELIVERY',
      [Name]  = 'Поставка'
    WHERE S1.[Prefix] NOT IN ('REJ-')
      UNION ALL
    SELECT
      [PCode] = 'PAYMENT',
      [Name]  = 'Оплата'
    WHERE CTE.[Code] NOT LIKE '%:MARG%' AND CTE.[Code] NOT LIKE '%:LOAN' AND CTE.[Code] NOT LIKE '%:LOAN:%'
  ) S2
  CROSS APPLY
  (
    SELECT
      [Action]  = 'D',
      [Code] = REPLACE
               (
                REPLACE
                (
                  CTE.[Code], 
                  ':BUY:', 
                  ':' + S1.[Prefix] + '<% ACTION %>:DELIVERY:'
                ),
                ':SALE:',
                ':' + S1.[Prefix] + '<% ACTION %>:DELIVERY:'
               ),
      [Name] = CTE.[Name]
    WHERE S2.[PCode] = 'DELIVERY'
      UNION ALL
    SELECT
      [Action]  = 'P',
      [Code]    = V.[Code],
      [Name]    = CTE.[Name]
    FROM
    (
      VALUES
      (
        REPLACE
        (
          REPLACE
          (
            'BDM:MON:ELE' + SubString(CTE.[Code], Len(CTE.[ICode]) + 5, 1000),
            ':BUY:',
            ':' + S1.[Prefix] + '<% ACTION %>:' + CASE WHEN S1.[Prefix] = 'REJ-' THEN 'DEAL' ELSE 'PAYMENT' END + ':' + CTE.[ICode] + ':'
          ),
          ':SALE:',
          ':' + S1.[Prefix] + '<% ACTION %>:' + CASE WHEN S1.[Prefix] = 'REJ-' THEN 'DEAL' ELSE 'PAYMENT' END + ':' + CTE.[ICode] + ':'
        )
      )
    ) V([Code])
    WHERE S2.[PCode] = 'PAYMENT' AND CTE.[Code] NOT LIKE '%:MARG%'
  ) S3
  CROSS APPLY
  (
    SELECT
      [Action]          = 'DEC',
      [Code]            = REPLACE(S3.[Code], '<% ACTION %>', 'DEC'),
      [Name]            = S3.[Name],
      [DecActiveState]  = CASE
                            WHEN F.[Final] = 0 THEN NULL
                            WHEN S1.[Prefix] = '' THEN 'REAL:SERVICED'
                            WHEN S1.[Prefix] = 'REJ-' THEN 'DEBT:DELIVERY'
                            WHEN S3.[Action] = 'D' THEN 'LIABILITIES:DELIVERY:' -- AND CTE.[ICode] NOT IN ('MON:DOC', 'MON:ELE')
                                  +
                                  CASE
                                    WHEN S3.[Code] LIKE '%:REPO:%' THEN 'REPO'
                                    WHEN S3.[Code] LIKE '%:REPO_BACK:%' THEN 'REPO_BACK'
                                    WHEN S3.[Code] LIKE '%:LOAN:%' THEN 'LOAN'
                                    WHEN S3.[Code] LIKE '%:LOAN_BACK:%' THEN CASE WHEN S3.[Action] = 'P' THEN 'LOAN%' ELSE 'LOAN_BACK' END
                                    ELSE 'DEF'
                                  END
                                  + ':'
                                  +
                                  CASE
                                    WHEN S3.[Code] LIKE '%:UP:%' THEN 'UP'
                                    WHEN S3.[Code] LIKE '%:SYS:%' THEN 'SYS'
                                    WHEN S3.[Code] LIKE '%:EXCH:%' THEN 'EXCH'
                                    ELSE 'OTC'
                                  END
                                  + ':DEC'
                            ELSE 'LIABILITIES:PAYMENT:'
                              +
                              CASE
                                WHEN S3.[Code] LIKE '%:REPO:%' THEN 'REPO'
                                WHEN S3.[Code] LIKE '%:REPO_BACK:%' THEN 'REPO_BACK'
                                WHEN S3.[Code] LIKE '%:LOAN:%' THEN 'LOAN'
                                WHEN S3.[Code] LIKE '%:LOAN_BACK:%' THEN CASE WHEN S3.[Action] = 'P' THEN 'LOAN%' ELSE 'LOAN_BACK' END
                                ELSE 'DEF' 
                              END
                              + ':'
                              +
                              CASE
                                WHEN S3.[Code] LIKE '%:UP:%' THEN 'UP'
                                WHEN S3.[Code] LIKE '%:SYS:%' THEN 'SYS'
                                WHEN S3.[Code] LIKE '%:EXCH:%' THEN 'EXCH'
                                ELSE 'OTC'
                              END
                              + ':DEC'
                          END,
      [IncActiveState]  = NULL,
      [Final]           = F.[Final]

    UNION ALL

    SELECT
      [Action]          = 'INC',
      [Code]            = REPLACE(S3.[Code], '<% ACTION %>', 'INC'),
      [Name]            = S3.[Name],
      [DecActiveState]  = NULL,
      [IncActiveState]  = CASE
                            WHEN F.[Final] = 0 THEN NULL
                            WHEN S1.[Prefix] = '' THEN 'REAL:SERVICED'
                            WHEN S1.[Prefix] = 'REJ-' THEN 'DEBT:DELIVERY'
                            WHEN S3.[Action] = 'D' THEN 'LIABILITIES:DELIVERY:' --AND CTE.[ICode] NOT IN ('MON:DOC', 'MON:ELE')
                                  +
                                  CASE
                                    WHEN S3.[Code] LIKE '%:REPO:%' THEN 'REPO'
                                    WHEN S3.[Code] LIKE '%:REPO_BACK:%' THEN 'REPO_BACK'
                                    WHEN S3.[Code] LIKE '%:LOAN:%' THEN 'LOAN'
                                    WHEN S3.[Code] LIKE '%:LOAN_BACK:%' THEN CASE WHEN S3.[Action] = 'P' THEN 'LOAN%' ELSE 'LOAN_BACK' END
                                    ELSE 'DEF'
                                  END
                                  + ':'
                                  +
                                  CASE
                                    WHEN S3.[Code] LIKE '%:UP:%' THEN 'UP'
                                    WHEN S3.[Code] LIKE '%:SYS:%' THEN 'SYS'
                                    WHEN S3.[Code] LIKE '%:EXCH:%' THEN 'EXCH'
                                    ELSE 'OTC'
                                  END
                                  + ':INC'
                            ELSE 'LIABILITIES:PAYMENT:'
                                  +
                                  CASE
                                    WHEN S3.[Code] LIKE '%:REPO:%' THEN 'REPO'
                                    WHEN S3.[Code] LIKE '%:REPO_BACK:%' THEN 'REPO_BACK'
                                    WHEN S3.[Code] LIKE '%:LOAN:%' THEN 'LOAN'
                                    WHEN S3.[Code] LIKE '%:LOAN_BACK:%' THEN CASE WHEN S3.[Action] = 'P' THEN 'LOAN%' ELSE 'LOAN_BACK' END
                                    ELSE 'DEF'
                                  END
                                  + ':'
                                  +
                                  CASE 
                                    WHEN S3.[Code] LIKE '%:UP:%' THEN 'UP'
                                    WHEN S3.[Code] LIKE '%:SYS:%' THEN 'SYS'
                                    WHEN S3.[Code] LIKE '%:EXCH:%' THEN 'EXCH'
                                    ELSE 'OTC'
                                  END
                                  + ':INC'
                          END,
      [Final]           = F.[Final]

    UNION ALL

    SELECT
      [Action]          = 'RECALC',
      [Code]            = REPLACE(S3.[Code], '<% ACTION %>', 'RECALC'),
      [Name]            = S3.[Name],
      [DecActiveState]  = NULL,
      [IncActiveState]  = NULL,
      [Final]           = F.[Final]
    WHERE S1.[Prefix] = '' AND S3.[Action] = 'P' AND S3.[Code] NOT LIKE '%:EXCH%' AND S3.[Code] NOT LIKE '%:SYS%'
  ) S4
  CROSS APPLY
  (
    SELECT
      [Code]                    = S4.[Code],
      [Name]                    = S4.[Name],
      [Final]                   = NULL,
      [CanBeSimpleTransaction]  = NULL
    FROM (VALUES(CASE WHEN S4.[Final] = 1 AND S1.[Prefix] = '' AND S2.[PCode] IN ('DELIVERY') THEN 1 ELSE 0 END)) V([PFinal])
    UNION ALL
    SELECT
      [Code]                    = S4.[Code] + ':EXT',
      [Name]                    = 'Внешнее',
      [Final]                   = 1,
      [CanBeSimpleTransaction]  = 1
    WHERE S4.[Final] = 1 AND S1.[Prefix] = '' AND S4.[Action] <> 'RECALC'
    UNION ALL
    SELECT
      [Code]                    = S4.[Code] + ':INT',
      [Name]                    = 'Внутреннее',
      [Final]                   = 1,
      [CanBeSimpleTransaction]  = 1
    WHERE S4.[Final] = 1 AND S1.[Prefix] = '' AND S4.[Action] <> 'RECALC'
                    AND CTE.[Code] NOT LIKE '%:EXCH:%'
                    AND S3.[Code] NOT LIKE '%:SELF' AND S3.[Code] NOT LIKE '%:DU' AND S3.[Code] NOT LIKE '%:UP:%'
    UNION ALL
    SELECT
      [Code]                    = S4.[Code] + ':NET',
      [Name]                    = 'Взаимозачёт',
      [Final]                   = 1,
      [CanBeSimpleTransaction]  = 0
    WHERE S4.[Final] = 1 AND S1.[Prefix] = '' AND S4.[Action] <> 'RECALC'
      AND CTE.[Code] NOT LIKE '%:EXCH:%' --AND S3.[Code] NOT LIKE '%:UP:%'
  ) SX
  CROSS APPLY
  (
    SELECT
      [Code]            = SX.[Code],
      [Name]            = SX.[Name],
      [DecActiveState]  = CASE WHEN SX.[CanBeSimpleTransaction] = 0 THEN NULL ELSE S4.[DecActiveState] END,
      [IncActiveState]  = CASE WHEN SX.[CanBeSimpleTransaction] = 0 THEN NULL ELSE S4.[IncActiveState] END
    FROM
    (
      VALUES
      (
        CASE
          WHEN S3.[Action] = 'P' 
              AND 
              (
                SX.[Final] = 1 -- EXT/INT/NET
                OR
                S4.[Final] = 1 -- DEAL EOC
                AND (CTE.[ICode] NOT IN ('SEC:DOC', 'SEC:ELE') OR S1.[Prefix] <> '')
              ) 
            THEN 1
          ELSE 0
        END
      )
    ) V([Expand])
    UNION ALL
    SELECT
      [Code]            = SX.[Code] + ':$',
      [Name]            = '$',
      [DecActiveState]  = S4.[DecActiveState],
      [IncActiveState]  = S4.[IncActiveState]
    WHERE S4.[Action] <> 'RECALC' AND (S4.[Final] = 1 AND S1.[Prefix] = 'PRE-' OR SX.[Final] = 1)
                AND S3.[Action] = 'P' 
                AND CTE.[Code] NOT LIKE '%:LOAN_BACK:%'
                --AND (CTE.[Code] LIKE '%:REPO_BACK:%' OR CTE.[ICode] IN ('SEC:DOC', 'SEC:ELE'))
      UNION ALL
    SELECT
      [Code]            = SX.[Code] + ':#',
      [Name]            = '#',
      [DecActiveState]  = S4.[DecActiveState],
      [IncActiveState]  = S4.[IncActiveState]
    WHERE S4.[Action] <> 'RECALC' AND (S4.[Final] = 1 AND S1.[Prefix] = 'PRE-' OR SX.[Final] = 1)
                AND S3.[Action] = 'P'
                AND CTE.[ICode] IN ('SEC:DOC', 'SEC:ELE')
                AND CTE.[Code] NOT LIKE '%:LOAN_BACK:%'
      UNION ALL
    SELECT
      [Code]            = SX.[Code] + ':%',
      [Name]            = '%',
      [DecActiveState]  = CASE WHEN S1.[Prefix] = 'PRE-' THEN REPLACE(S4.[DecActiveState], ':REPO_BACK:', ':REPO%:') ELSE S4.[DecActiveState] END,
      [IncActiveState]  = CASE WHEN S1.[Prefix] = 'PRE-' THEN REPLACE(S4.[IncActiveState], ':REPO_BACK:', ':REPO%:') ELSE S4.[IncActiveState] END
    WHERE S4.[Action] <> 'RECALC' AND (S4.[Final] = 1 AND S1.[Prefix] = 'PRE-' OR SX.[Final] = 1)
                AND S3.[Action] = 'P'
                AND CTE.[Code] LIKE '%:REPO_BACK:%' --OR CTE.[Code] LIKE '%:LOAN_BACK:%')
  ) SY
  LEFT JOIN [BackOffice].[Dic:Actives:States] STD ON SY.[DecActiveState] = STD.[Code]
  LEFT JOIN [BackOffice].[Dic:Actives:States] STI ON SY.[IncActiveState] = STI.[Code]

--SELECT * FROM @DATA WHERE [Code] LIKE 'BDM:MON:ELE:PRE-INC:PAYMENT:SEC:ELE:DEF:OTC:CLN:COM%'
--RETURN

--SELECT D.Code, F.*, C.Code,
UPDATE D SET
  [CanBeConsolidatedTransaction]  = CASE
                                      WHEN
                                        (F.[Prefix] = '' OR F.[Prefix] = 'PRE-' AND (F.[Payment] = 1 OR D.[CodeInstrument] IN ('SEC:DOC', 'SEC:ELE')))
                                        AND (F.[NFinal] = 1 OR C.[Code] IS NULL AND F.[Final] = 0)
                                        OR
                                        F.[Prefix] IN ('RECALC') AND C.[Code] IS NULL
                                          THEN 1
                                      WHEN F.[Prefix] IN ('', 'PRE-') AND F.[Final] = 1
                                          THEN NULL
                                      ELSE 0
                                    END,
  [CanBeSimpleTransaction]        = CASE
                                      WHEN (D.[DecActiveState_Id] IS NOT NULL OR D.[IncActiveState_Id] IS NOT NULL)
                                        AND
                                        (
                                          F.[Prefix] = '' AND (F.[NFinal] = 1 OR C.[Code] IS NULL AND F.[Final] = 0)
                                          OR
                                          F.[Prefix] NOT IN ('', 'RECALC') AND C.[Code] IS NULL
                                        )
                                          THEN 1
                                      ELSE 0 
                                    END,
  [DecActiveState_Id]             = CASE 
                                      WHEN F.[Prefix] = '' AND (F.[NFinal] = 1 OR C.[Code] IS NULL AND F.[Final] = 0)
                                        OR F.[Prefix] NOT IN ('', 'RECALC') AND C.[Code] IS NULL
                                        THEN D.[DecActiveState_Id]
                                    END,
  [IncActiveState_Id]             = CASE
                                      WHEN F.[Prefix] = '' AND (F.[NFinal] = 1 OR C.[Code] IS NULL AND F.[Final] = 0)
                                        OR F.[Prefix] NOT IN ('', 'RECALC') AND C.[Code] IS NULL
                                          THEN D.[IncActiveState_Id]
                                    END
FROM @DATA D
OUTER APPLY
(
  SELECT TOP 1 C.[Code]
  FROM @DATA C
  WHERE C.[Code] LIKE D.[Code] + ':%' AND CharIndex(':', C.[Code], LEN(D.[Code]) + 2) = 0
) C
CROSS APPLY
(
  VALUES
  (
    CASE WHEN Right(D.[Code], 2) IN (':$', ':#', ':%') THEN 1 ELSE 0 END,       -- [Final]
    CASE WHEN Right(C.[Code], 2) IN (':$', ':#', ':%') THEN 1 ELSE 0 END,       -- [NFinal]
    CASE WHEN Right(D.[Code], 4) IN (':EXT', ':INT', ':NET') THEN 1 ELSE 0 END, -- [EXT]
    CASE WHEN D.[Code] LIKE '%:PRE-%' THEN 'PRE-' WHEN D.[Code] LIKE '%:REJ-%' THEN 'REJ-' WHEN D.[Code] LIKE '%:RECALC%' THEN 'RECALC' ELSE '' END,
    CASE WHEN D.[Code] LIKE '%:PAYMENT:%' THEN 1 ELSE 0 END
  )
) F([Final], [NFinal], [EXT], [Prefix], [Payment])
--WHERE D.[Code] = 'BDM:MON:ELE:PRE-DEC:PAYMENT:SEC:ELE:REPO_BACK:SYS:CLN:COM'
--RETURN

--SELECT * FROM @DATA 
--ORDER BY 1
--RETURN

--DELETE T
SELECT [DELETE] = 'DELETE', *
FROM [BackOffice].[Dic:Transactions:Types] T
WHERE
  (
    T.[Code] LIKE 'BDM:%:DEC:DEAL%' OR T.[Code] LIKE 'BDM:%:INC:DEAL%'
    OR T.[Code] LIKE 'BDM:%:PRE-DEC:DEAL%' OR T.[Code] LIKE 'BDM:%:PRE-INC:DEAL%'
    OR T.[Code] LIKE 'BDM:%:RECALC:DEAL%'

    OR T.[Code] LIKE 'BDM:%:DEC:DELIVERY:%' OR T.[Code] LIKE 'BDM:%:INC:DELIVERY:%'
    OR T.[Code] LIKE 'BDM:%:PRE-DEC:DELIVERY:%' OR T.[Code] LIKE 'BDM:%:PRE-INC:DELIVERY:%'

    OR T.[Code] LIKE 'BDM:%:DEC:PAYMENT:%' OR T.[Code] LIKE 'BDM:%:INC:PAYMENT:%'
    OR T.[Code] LIKE 'BDM:%:PRE-DEC:PAYMENT:%' OR T.[Code] LIKE 'BDM:%:PRE-INC:PAYMENT:%'
    OR T.[Code] LIKE 'BDM:%:RECALC:PAYMENT%'

    OR T.[Code] LIKE 'BDM:%:REJ-DEC:DEAL:%' OR T.[Code] LIKE 'BDM:%:REJ-INC:DEAL:%'
  )
  AND T.[Code] NOT IN (SELECT [Code] FROM @DATA)
--AND T.[Code] LIKE '%:$' OR T.[Code] LIKE '%:#' OR T.[Code] LIKE '%:[%]'
--ORDER BY Code

--RETURN

--PRINT 1
  UPDATE T SET
  --SELECT [UPDATE] = 'UPDATE', 
    [Row:Fields]                    = 'Name,DecActiveState_Id,IncActiveState_Id,CanBeConsolidatedTransaction,CanBeSimpleTransaction,NameRoot,GUIRoot',
    [Name]                          = (SELECT I.[Name] FROM @DATA I WHERE T.[Code] = I.[Code]),
    [DecActiveState_Id]             = (SELECT I.[DecActiveState_Id] FROM @DATA I WHERE T.[Code] = I.[Code]),
    [IncActiveState_Id]             = (SELECT I.[IncActiveState_Id] FROM @DATA I WHERE T.[Code] = I.[Code]),
    [CanBeConsolidatedTransaction]  = (SELECT I.[CanBeConsolidatedTransaction] FROM @DATA I WHERE T.[Code] = I.[Code]),
    [CanBeSimpleTransaction]        = (SELECT I.[CanBeSimpleTransaction] FROM @DATA I WHERE T.[Code] = I.[Code]),
    [NameRoot]                      = (SELECT I.[NameRoot] FROM @DATA I WHERE T.[Code] = I.[Code]),
    [GUIRoot]                       = (SELECT I.[GUIRoot] FROM @DATA I WHERE T.[Code] = I.[Code])
  FROM [BackOffice].[Dic:Transactions:Types(Translate Update)] T
  WHERE T.[Translate:Language] = 'RU' AND T.[Code] IN (SELECT [Code] FROM @DATA)


  --INSERT INTO [BackOffice].[Dic:Transactions:Types]
  --([Name], [Code], [DecActiveState_Id], [IncActiveState_Id], [CanBeOrder], [CanBeConsolidatedTransaction], [CanBeSimpleTransaction], [NameRoot], [GUIRoot])
  SELECT --'INSERT',
    I.[Name], I.[Code], I.[DecActiveState_Id], I.[IncActiveState_Id], 0, I.[CanBeConsolidatedTransaction], I.[CanBeSimpleTransaction], I.[NameRoot], 0
  FROM @DATA I
  WHERE I.[Code] NOT IN (SELECT [Code] FROM [BackOffice].[Dic:Transactions:Types])

  RETURN

-- DELETE [BackOffice].[Dic:Transactions:Types] WHERE [Code] LIKE '%:REJ-DEC%' OR [Code] LIKE '%:REJ-INC%'


UPDATE T SET 
  [Parent_Id] = P.[Id]
FROM [BackOffice].[Dic:Transactions:Types] T
LEFT JOIN [BackOffice].[Dic:Transactions:Types] P ON Left(T.[Code], [Pub].[CharIndex(Back)](':', T.[Code], Default) - 1) = P.[Code]
WHERE IsNull(T.[Parent_Id], 0) <> IsNull(P.[Id], 0)


--DELETE BackOffice.[Dic:Transactions:Types] WHERE Code LIKE '%:RECALC:%' AND Code NOT LIKE '%:MON:%'