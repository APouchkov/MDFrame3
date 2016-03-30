DECLARE @DATA TABLE
(
  [Code]        VarChar(100) COLLATE Cyrillic_General_BIN NOT NULL PRIMARY KEY CLUSTERED,
  [Name]        NVarChar(150) NOT NULL,
  [CanBeOrder]  Bit,
  [CanBeConsolidatedTransaction] Bit,
  [GUIRoot]     Bit
)

INSERT INTO @DATA
SELECT
  *
FROM
(
  SELECT
    K6.[Code],
    [Name] = CASE K6.[Name] WHEN 'Премиальным' THEN 'Премиального' WHEN 'Маржируемым' THEN 'Маржируемого' ELSE K6.[Name] END,
    K6.[CanBeOrder],
    K6.[CanBeConsolidatedTransaction],
    [GUIRoot] = CASE WHEN K6.[GUIRoot] = 1 AND (K6.[Code] IN ('EXCH', 'SYS', 'OTC') OR K6.[Code] LIKE '%:EXCH' OR K6.[Code] LIKE '%:SYS' OR K6.[Code] LIKE '%:OTC') THEN 1 ELSE 0 END
  FROM [BackOffice].[Dic:Transactions:Types] T
  CROSS APPLY
  (
    VALUES(SubString(T.[Code], 6, 100))
  ) K1([Code])
  CROSS APPLY
  (
    VALUES ('BUY', 'Покупка'), ('SALE', 'Продажа')
  ) K2([Code], [Name])
  CROSS APPLY
  (
    SELECT
      [ICode] =
        CASE
          WHEN K1.[Code] LIKE 'MON:DOC:%' THEN 'MON:DOC'
          WHEN K1.[Code] LIKE 'MON:ELE:%' THEN 'MON:ELE'
          WHEN K1.[Code] LIKE 'DER:DOC:%' THEN 'DER:DOC'
          WHEN K1.[Code] LIKE 'DER:ELE:%' THEN 'DER:ELE'
          WHEN K1.[Code] LIKE 'SEC:DOC:%' THEN 'SEC:DOC'
          WHEN K1.[Code] LIKE 'SEC:ELE:%' THEN 'SEC:ELE'
        END,
      [TCode] =
        CASE
          WHEN K1.[Code] LIKE 'MON:DOC:%' THEN SubString(K1.[Code], 9, 100)
          WHEN K1.[Code] LIKE 'MON:ELE:%' THEN SubString(K1.[Code], 9, 100)
          WHEN K1.[Code] LIKE 'DER:DOC:%' THEN SubString(K1.[Code], 9, 100)
          WHEN K1.[Code] LIKE 'DER:ELE:%' THEN SubString(K1.[Code], 9, 100)
          WHEN K1.[Code] LIKE 'SEC:DOC:%' THEN SubString(K1.[Code], 9, 100)
          WHEN K1.[Code] LIKE 'SEC:ELE:%' THEN SubString(K1.[Code], 9, 100)
        END
  ) K3
  CROSS APPLY
  (
    VALUES('BDM:' + K3.[ICode] + ':' + K2.[Code] + ':' + K3.[TCode])
  ) K4([Code])
  CROSS APPLY
  (
    SELECT
      [Code]        = K4.[Code],
      [Name]        = T.[Name],
      [GUIRoot]     = 1, --CASE WHEN [Pub].[Char Count](K3.[TCode], ':') = 1 THEN 1 ELSE 0 END,
      [CanBeOrder]  = CASE WHEN CharIndex(':', K4.[Code]) = 0 THEN 1 ELSE 0 END
      --CASE WHEN K4.[Code] IN ('EXCH', 'SYS', 'OTC') OR K4.[Code] LIKE '%:EXCH' OR K4.[Code] LIKE '%:SYS' OR K4.[Code] LIKE '%:OTC' THEN 1 ELSE 0 END
    UNION ALL
    SELECT
      [Code]        = REPLACE(REPLACE(K4.[Code], ':REPO', ':REPO_BACK'), ':LOAN', ':LOAN_BACK'),
      [Name]        = CASE
                        WHEN T.[Name] = 'РЕПО' THEN 'РЕПО (ч.2)' 
                        WHEN T.[Name] = 'Займ' THEN 'Займ (Возврат)'
                        ELSE T.[Name] 
                      END,
      [GUIRoot]     = 0,
      [CanBeOrder]  = 0
    WHERE K4.[Code] LIKE '%:REPO%' OR K4.[Code] LIKE '%:LOAN%'
  ) K5
  CROSS APPLY
  (
    SELECT
      [Code]        = K5.[Code],
      [Name]        = K5.[Name],
      [GUIRoot]     = K5.[GUIRoot],
      [CanBeOrder]  = K5.[CanBeOrder],
      [CanBeConsolidatedTransaction] = 0
    UNION ALL
    SELECT
      [Code]        = I3.[Code],
      [Name]        = I3.[Name],
      [GUIRoot]     = I3.[GUIRoot],
      [CanBeOrder]  = I3.[CanBeOrder],
      [CanBeConsolidatedTransaction] = I3.[CanBeConsolidatedTransaction]
    FROM
    (
      SELECT
        [Code]        = K5.[Code] + ':' + PK.[Code],
        [Name]        = CASE
                          WHEN PK.[Code] = 'CLN' THEN 'Клиента'
                          WHEN PK.[Code] = 'SELF' THEN 'Собственная'
                          ELSE PK.[Name]
                        END,
        [GUIRoot]     = 0,
        [CanBeOrder]  = CASE WHEN PK.[Code] <> 'CLN' THEN 1 ELSE 0 END,
        [CanBeDeal]   = CASE WHEN PK.[Code] <> 'CLN' THEN 1 ELSE 0 END
      FROM [BackOffice].[Dic:Deals:Parts:Kinds] PK
      WHERE PK.[Code] <> 'TARGET'
            AND (PK.[Code] NOT IN ('CLN:ORD', 'CLN:ORD*') OR K3.[TCode] NOT LIKE '%:EXCH%' AND K3.[TCode] NOT LIKE '%:SYS%')

      UNION ALL
      SELECT
        [Code]        = K5.[Code] + ':' + R.[Code],
        [Name]        = R.[Name],
        [GUIRoot]     = 0,
        [CanBeOrder]  = CASE WHEN R.[Code] IN ('BRK', 'SRV') THEN 0 ELSE 1 END,
        [CanBeDeal]   = 0
      FROM
      (
        SELECT DISTINCT 
          [Code] = IsNull(R.[OrderCode], R.[Code]), 
          R.[Name]
        FROM [BackOffice].[Dic:Deals:Reasons] R
        WHERE [Code] NOT IN ('ORDER', 'CNT:DU') AND ([OrderCode] <> '' OR [Code] IN ('BRK', 'SRV'))
      ) R
      WHERE K3.[TCode] = 'DEF:EXCH'
        AND (R.[Code] <> 'SRV:EXPENSES' OR K2.[Code] = 'SALE')
        AND (R.[Code] NOT IN ('BRK', 'BRK:RISK') OR K3.[ICode] IN ('SEC:ELE'))
    ) I
    CROSS APPLY
    (
      SELECT
        [Code]        = I.[Code],
        [Name]        = I.[Name],
        [GUIRoot]     = I.[GUIRoot],
        [CanBeOrder]  = CASE WHEN I.[Code] NOT LIKE '%:REPO_BACK%' AND I.[Code] NOT LIKE '%:LOAN_BACK%' THEN I.[CanBeOrder] ELSE 0 END,
        [CanBeConsolidatedTransaction]  = 0,
        [SignChange]  = 0
      --WHERE I.[Code] NOT LIKE '%:CLN'
      UNION ALL
      SELECT
        [Code]        = I.[Code] + ':BARG',
        [Name]        = 'Заключена',
        [GUIRoot]     = 0,
        [CanBeOrder]  = 0,
        [CanBeConsolidatedTransaction]  = NULL,
        [SignChange]  = 0
      WHERE I.[CanBeDeal] = 1

      --UNION ALL
      --SELECT
      --  [Code]        = I.[Code] + ':BARG+EXEC',
      --  [Name]        = 'Заключена и исполнена',
      --  [GUIRoot]     = 0,
      --  [CanBeOrder]  = 0,
      --  [CanBeConsolidatedTransaction]  = NULL,
      --  [SignChange]  = 0
      --WHERE I.[Code] NOT LIKE '%:CLN' 
      --            AND
      --            (
      --              I.[Code] LIKE '%:EXCH:%'
      --              --OR I.[Code] LIKE '%:SYS:%'
      --            )
      --            AND I.[Code] NOT LIKE '%:REPO_BACK:%' AND I.[Code] NOT LIKE '%:LOAN_BACK:%'

        UNION ALL
      SELECT
        [Code]        = I.[Code] + ':CNTR',
        [Name]        = 'Договор',
        [GUIRoot]     = 0,
        [CanBeOrder]  = 0,
        [CanBeConsolidatedTransaction]  = NULL,
        [SignChange]  = 1
      WHERE I.[CanBeDeal] = 1 AND I.[Code] LIKE '%:OTC:%'
        UNION ALL
      SELECT
        [Code]        = I.[Code] + ':GLBL',
        [Name]        = 'Конвертация',
        [GUIRoot]     = 0,
        [CanBeOrder]  = 0,
        [CanBeConsolidatedTransaction]  = 0,
        [SignChange]  = 1
      WHERE I.[CanBeDeal] = 1
                        AND (I.[Code] LIKE '%:SEC:%' OR I.[Code] LIKE '%:MON:%') 
                        AND
                        (
                          I.[Code] NOT LIKE '%:EXCH:%' AND I.[Code] NOT LIKE '%:SYS:%'
                          OR
                          (I.[Code] LIKE '%:REPO_BACK:%' OR I.[Code] LIKE '%:LOAN_BACK:%')
                        )
                                      -- Конвертации бывают только с ЦБ или Валютой и только в обратном РЕПО
      UNION ALL
      SELECT
        [Code]        = I.[Code] + ':SUPP',
        [Name]        = 'Дополнение',
        [GUIRoot]     = 0,
        [CanBeOrder]  = 0,
        [CanBeConsolidatedTransaction]  = 0,
        [SignChange]  = 1
      WHERE I.[CanBeDeal] = 1
                        AND (I.[Code] LIKE '%:SEC:%' OR I.[Code] LIKE '%:MON:%') 
                        AND I.[Code] NOT LIKE '%:EXCH:%' AND I.[Code] NOT LIKE '%:SYS:%'
                                      -- Дополнения не бывают на бирже
      UNION ALL
      SELECT
        [Code]        = I.[Code] + ':TERM',
        [Name]        = 'Расторжение',
        [GUIRoot]     = 0,
        [CanBeOrder]  = 0,
        [CanBeConsolidatedTransaction]  = NULL,
        [SignChange]  = 1
      WHERE I.[CanBeDeal] = 1
                      --AND (I.[Code] NOT LIKE '%:DER:%' OR I.[Code] NOT LIKE '%:MARG:%')
                      --AND I.[Code] NOT LIKE '%:EXCH:%' AND I.[Code] NOT LIKE '%:SYS:%'
                      --AND I.[Code] NOT LIKE '%:REPO_BACK:%' AND I.[Code] NOT LIKE '%:LOAN_BACK:%'
                                      -- Расторжения с открытыми обязательствами бессмысленны для обратного РЕПО а также на Бирже

      UNION ALL
      SELECT
        [Code]        = I.[Code] + ':DELI',
        [Name]        = 'Поставлена',
        [GUIRoot]     = 0,
        [CanBeOrder]  = 0,
        [CanBeConsolidatedTransaction]  = NULL,
        [SignChange]  = 0
      WHERE I.[CanBeDeal] = 1
                      AND I.[Code] LIKE '%:SEC:%'
                      --AND I.[Code] NOT LIKE '%:LOAN:%'
                      --AND
                      --(
                      --  I.[Code] LIKE '%:OTC:%'
                      --  OR I.[Code] LIKE '%:REPO_BACK:%'
                      --  OR I.[Code] LIKE '%:LOAN_BACK:%'
                      --)
                                        -- Поставки без исполнения не бывают с ДС и ПИ
                                        -- Бывают только вне биржи, либо с обратным РЕПО

      UNION ALL
      SELECT
        [Code]        = I.[Code] + ':PAID',
        [Name]        = 'Оплачена',
        [GUIRoot]     = 0,
        [CanBeOrder]  = 0,
        [CanBeConsolidatedTransaction]  = NULL,
        [SignChange]  = 0
      WHERE I.[CanBeDeal] = 1
                      AND I.[Code] LIKE '%:SEC:%'
                      --AND I.[Code] NOT LIKE '%:DER:%:MARG:%' 
                      --AND I.[Code] NOT LIKE '%:MON:%' 
                      --AND I.[Code] NOT LIKE '%:CLN' 
                      --AND I.[Code] NOT LIKE '%:MON:%'
                      --AND I.[Code] NOT LIKE '%:DER:DOC:%' AND I.[Code] NOT LIKE '%:DER:ELE:%'
                      --AND (I.[Code] NOT LIKE '%:EXCH:%' OR I.[Code] LIKE '%:REPO_BACK:%' OR I.[Code] LIKE '%:LOAN_BACK:%')
                                         --Оплаты без исполнения бывают только вне биржи, либо с обратным РЕПО

      UNION ALL
      SELECT
        [Code]        = I.[Code] + ':EXEC',
        [Name]        = 'Исполнена',
        [GUIRoot]     = 0,
        [CanBeOrder]  = 0,
        [CanBeConsolidatedTransaction]  = NULL,
        [SignChange]  = 0
      WHERE I.[CanBeDeal] = 1

      --UNION ALL
      --SELECT
      --  [Code]        = I.[Code] + ':EXEC^DVP',
      --  [Name]        = 'Исполнена ППП',
      --  [GUIRoot]     = 0,
      --  [CanBeOrder]  = 0,
      --  [CanBeConsolidatedTransaction]  = NULL,
      --  [SignChange]  = 0
      --WHERE I.[Code] NOT LIKE '%:CLN'
      --                AND I.[Code] NOT LIKE '%:LOAN:%'
      --                AND I.[Code] NOT LIKE '%:MARG:%'
                      -- Мнгновенные исполнения с Прямыми Займами и МПИ бессмысленны

      --UNION ALL
      --SELECT
      --  [Code]        = I.[Code] + ':TERM+EXEC',
      --  [Name]        = 'Расторгнута',
      --  [GUIRoot]     = 0,
      --  [CanBeOrder]  = 0,
      --  [CanBeConsolidatedTransaction]  = NULL,
      --  [SignChange]  = 0
      --WHERE I.[Code] NOT LIKE '%:CLN' 
                      --AND I.[Code] NOT LIKE '%:DER:%'
                      --AND I.[Code] NOT LIKE '%:EXCH:%'
                      --AND I.[Code] NOT LIKE '%:REPO_BACK:%' AND I.[Code] NOT LIKE '%:LOAN_BACK:%'
    ) I2
    CROSS APPLY
    (
      SELECT
        [Code]        = I2.[Code],
        [Name]        = I2.[Name],
        [GUIRoot]     = I2.[GUIRoot],
        [CanBeOrder]  = I2.[CanBeOrder],
        [CanBeConsolidatedTransaction]  = I2.[CanBeConsolidatedTransaction]
      UNION ALL
      SELECT
        [Code]        = I2.[Code] + ':DEL',
        [Name]        = '(-)',
        [GUIRoot]     = 0,
        [CanBeOrder]  = 0,
        [CanBeConsolidatedTransaction]  = NULL
      WHERE I2.[SignChange] = 1
            AND I2.[Code] NOT LIKE '%:CNTR'
            AND I2.[Code] NOT LIKE '%:DER:%'
                      AND I.[Code] NOT LIKE '%:EXCH:%' AND I.[Code] NOT LIKE '%:SYS:%'
                      AND I.[Code] NOT LIKE '%:REPO:%' AND I.[Code] NOT LIKE '%:LOAN:%'
                      AND I.[Code] NOT LIKE '%:REPO_BACK:%' AND I.[Code] NOT LIKE '%:LOAN_BACK:%'

      UNION ALL
      SELECT
        [Code]        = I2.[Code] + ':CHN',
        [Name]        = '(~)',
        [GUIRoot]     = 0,
        [CanBeOrder]  = 0,
        [CanBeConsolidatedTransaction]  = NULL
      WHERE I2.[SignChange] = 1
            AND (I2.[Code] NOT LIKE '%:DER:%' OR I2.[Code] NOT LIKE '%:MARG:%')
                      AND I.[Code] NOT LIKE '%:EXCH:%' AND I.[Code] NOT LIKE '%:SYS:%'
                      AND I.[Code] NOT LIKE '%:REPO:%' AND I.[Code] NOT LIKE '%:LOAN:%'
                      AND I.[Code] NOT LIKE '%:REPO_BACK:%' AND I.[Code] NOT LIKE '%:LOAN_BACK:%'
      UNION ALL
      SELECT
        [Code]        = I2.[Code] + ':NEW',
        [Name]        = '(+)',
        [GUIRoot]     = 0,
        [CanBeOrder]  = 0,
        [CanBeConsolidatedTransaction]  = NULL
      WHERE I2.[SignChange] = 1 
            AND I2.[Code] NOT LIKE '%:CNTR' 
            AND I2.[Code] NOT LIKE '%:DER:%'
                      AND I.[Code] NOT LIKE '%:EXCH:%' AND I.[Code] NOT LIKE '%:SYS:%'
                      AND I.[Code] NOT LIKE '%:REPO:%' AND I.[Code] NOT LIKE '%:LOAN:%'
                      AND I.[Code] NOT LIKE '%:REPO_BACK:%' AND I.[Code] NOT LIKE '%:LOAN_BACK:%'
    ) I3
    WHERE K3.[TCode] LIKE '%:EXCH' OR K3.[TCode] LIKE '%:SYS' OR K3.[TCode] LIKE '%:OTC'
  ) K6
  WHERE T.[Code] LIKE 'DEAL:%' AND K3.[ICode] IS NOT NULL
) I

--SELECT T.*
--FROM [BackOffice].[Dic:Transactions:Types] T
--WHERE (T.[Code] LIKE 'BDM:%' AND (T.[Code] LIKE '%:BUY:%' OR T.[Code] LIKE '%:SALE:%'))
--  AND T.[Code] NOT IN (SELECT [Code] FROM @DATA)
--ORDER BY 4

--SELECT
--  I.[Name], I.[Code], NULL, NULL, I.[CanBeOrder], I.[CanBeConsolidatedTransaction], 0, 0, I.[GUIRoot]
--FROM @DATA I
--WHERE [Code] NOT IN (SELECT [Code] FROM [BackOffice].[Dic:Transactions:Types])
--ORDER BY 2

--SELECT * FROM @DATA WHERE Code LIKE '%SYS:%' ORDER BY 1
--RETURN

--SELECT * FROM @DATA WHERE Code LIKE '%PAID%' ORDER BY 1
--RETURN

DELETE T
FROM [BackOffice].[Dic:Transactions:Types] T
WHERE (T.[Code] LIKE 'BDM:%' AND (T.[Code] LIKE '%:BUY:%' OR T.[Code] LIKE '%:SALE:%'))
  AND T.[Code] NOT IN (SELECT [Code] FROM @DATA)


UPDATE T SET
  [Name] = (SELECT I.[Name] FROM @DATA I WHERE I.[Code] = T.[Code]),
  [CanBeOrder] = (SELECT I.[CanBeOrder] FROM @DATA I WHERE I.[Code] = T.[Code]), 
  [CanBeConsolidatedTransaction] = (SELECT I.[CanBeConsolidatedTransaction] FROM @DATA I WHERE I.[Code] = T.[Code]), 
  [GUIRoot] = (SELECT I.[GUIRoot] FROM @DATA I WHERE I.[Code] = T.[Code]),
  [Row:Fields] = 'Name,CanBeOrder,CanBeConsolidatedTransaction,GUIRoot'
FROM [BackOffice].[Dic:Transactions:Types(Translate Update)] T
WHERE T.[Translate:Language] = 'RU' AND T.[Code] IN (SELECT [Code] FROM @DATA)


INSERT INTO [BackOffice].[Dic:Transactions:Types]
([Name], [Code], [DecActiveState_Id], [IncActiveState_Id], [CanBeOrder], [CanBeConsolidatedTransaction], [CanBeSimpleTransaction], [NameRoot], [GUIRoot])
SELECT
  I.[Name], I.[Code], NULL, NULL, I.[CanBeOrder], I.[CanBeConsolidatedTransaction], 0, 0, I.[GUIRoot]
FROM @DATA I
WHERE [Code] NOT IN (SELECT [Code] FROM [BackOffice].[Dic:Transactions:Types])

UPDATE T SET 
  [Parent_Id] = P.[Id]
FROM [BackOffice].[Dic:Transactions:Types] T
LEFT JOIN [BackOffice].[Dic:Transactions:Types] P ON Left(T.[Code], [Pub].[CharIndex(Back)](':', T.[Code], Default) - 1) = P.[Code]
WHERE IsNull(T.[Parent_Id], 0) <> IsNull(P.[Id], 0)
