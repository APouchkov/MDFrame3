SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO
/*
  -- Пример использования:
  EXEC [dbo].[SVU::Derivatives]
    @Date = '20131223'
*/
CREATE PROCEDURE [dbo].[SVU::Derivatives]
  @Date Date
AS
  SET NOCOUNT ON

  ;WITH OPT AS
  (
    SELECT DISTINCT [isin_id], [sess_id]
    FROM [dbo].[plaza2_OPT_orders_log] O
    WHERE [moment] >= @Date and [moment] < DATEADD(DAY, 1, @Date)
      UNION
    SELECT DISTINCT [isin_id], [sess_id]
    FROM [dbo].[plaza2_OPT_user_deal] D
    WHERE [moment] >= @Date and [moment] < DATEADD(DAY, 1, @Date)
  )
  SELECT
    [Place]                     = 'FORTS',
    [Kind]                      = 'O',
    [Code]                      = S.[isin],
    [CodeShort]                 = S.[short_isin],
    [ClassCode]                 = S.[code_vcb],
    [ClassName]                 = FV.[name],
    [Name]                      = S.[name],
    [BaseActivePlace]           = 'FORTS',
    [BaseActiveCode]            = FS.[isin],
    [BaseActiveLOT]             = S.[lot_volume],
    [BaseActiveStrike]          = S.[strike],
    [BaseActiveStrikeCurrency]  = CASE WHEN FS.[signs] & 0x02 <> 0 THEN '^' ELSE 'RUB' END,
    [Type]                      = CASE WHEN S.[signs] & 0x02 <> 0 THEN 'M' ELSE 'P' END,
    [Style]                     = CASE WHEN S.[europe] = 1 THEN 'E' ELSE 'A' END,
    [Direction]                 = CASE WHEN S.[put] = 1 THEN 'P' ELSE 'C' END,
    [CalculationCurrency]       = CASE WHEN V.[curr] IN ('USR', 'RUR') THEN 'RUB' ELSE V.[curr] END,
    [CalculationPriceRound]     = S.[roundto],
    [PriceStep]                 = S.[min_step],
    [BeginDate]                 = (SELECT TOP 1 BD.[CreateDateTime] FROM [dbo].[plaza2_OPT_opt_sess_contents] BD WHERE BD.[isin_id] = I.[isin_id]),
    [ExpirationDateBegin]       = Cast(S.[d_exec_beg] AS Date),
    [ExpirationDateEnd]         = Cast(S.[d_exec_end] AS Date),
    [LastTradeDate]             = Cast(S.[d_pg] AS Date),
    [EndDate]                   = Cast(S.[d_pg] AS Date),
    [ExecuteMethod]             = CASE V.[exec_type] WHEN 'I' THEN 'C' WHEN 'D' THEN 'D' WHEN 'S' THEN 'T' ELSE '?' END
  FROM
  (
    SELECT
      [isin_id], [sess_id] = MAX([sess_id])
    FROM OPT
    GROUP BY [isin_id]
  ) I
  INNER JOIN [dbo].[plaza2_OPT_opt_sess_contents] S ON I.[isin_id] = S.[isin_id] AND I.[sess_id] = S.[sess_id]
  INNER JOIN [dbo].[plaza2_opt_vcb] V ON S.[code_vcb] = V.[code_vcb]
  INNER JOIN [dbo].[plaza2_FUT_sess_contents] FS ON S.[fut_isin_id] = FS.[isin_id] AND I.[sess_id] = FS.[sess_id]
  INNER JOIN [dbo].[plaza2_FUT_vcb] FV ON FS.[code_vcb] = FV.[code_vcb]

    UNION ALL

  SELECT
    [Place]                     = 'FORTS',
    [Kind]                      = 'F',
    [Code]                      = S.[isin],
    [CodeShort]                 = S.[short_isin],
    [ClassCode]                 = S.[code_vcb],
    [ClassName]                 = V.[name],
    [Name]                      = S.[name],
    [BaseActivePlace]           = NULL,
    [BaseActiveCode]            = NULL,
    [BaseActiveLOT]             = S.[lot_volume],
    [BaseActiveStrike]          = NULL,
    [BaseActiveStrikeCurrency]  = NULL,
    [Type]                      = CASE WHEN S.[signs] & 0x02 <> 0 THEN 'M' ELSE 'P' END,
    [Style]                     = NULL,
    [Direction]                 = NULL,
    [CalculationCurrency]       = CASE WHEN V.[curr] IN ('USR', 'RUR') THEN 'RUB' ELSE V.[curr] END,
    [CalculationPriceRound]     = S.[roundto],
    [PriceStep]                 = S.[min_step],
    [BeginDate]                 = (SELECT TOP 1 BD.[CreateDateTime] FROM [dbo].[plaza2_FUT_sess_contents] BD WHERE BD.[isin_id] = I.[isin_id]),
    [ExpirationDateBegin]       = NULL,
    [ExpirationDateEnd]         = NULL,
    [LastTradeDate]             = Cast(S.[d_pg] AS Date),
    [EndDate]                   = Cast(S.[d_pg] AS Date),
    [ExecuteMethod]             = CASE V.[exec_type] WHEN 'I' THEN 'C' WHEN 'D' THEN 'D' WHEN 'S' THEN 'T' ELSE '?' END
  FROM
  (
    SELECT
      [isin_id], [sess_id] = MAX([sess_id])
    FROM
    (
      SELECT DISTINCT [isin_id], [sess_id]
      FROM [dbo].[plaza2_FUT_orders_log] O
      WHERE [moment] >= @Date and [moment] < DATEADD(DAY, 1, @Date)
        UNION
      SELECT DISTINCT [isin_id], [sess_id]
      FROM [dbo].[plaza2_FUT_user_deal] D
      WHERE [moment] >= @Date and [moment] < DATEADD(DAY, 1, @Date)
        UNION
      SELECT
        DISTINCT S.[fut_isin_id], I.[sess_id]
      FROM OPT I
      INNER JOIN [dbo].[plaza2_OPT_opt_sess_contents] S ON I.[isin_id] = S.[isin_id] AND I.[sess_id] = S.[sess_id]
    ) I
    GROUP BY [isin_id]
  ) I
  INNER JOIN [dbo].[plaza2_FUT_sess_contents] S ON I.[isin_id] = S.[isin_id] AND I.[sess_id] = S.[sess_id] AND S.[signs] & 0x0C = 0
  INNER JOIN [dbo].[plaza2_fut_vcb] V ON S.[code_vcb] = V.[code_vcb]
  --INNER JOIN [dbo].[plaza2_FUT_sess_contents] PS ON s.[ticker] = PS.[isin_id] AND I.[sess_id] = PS.[sess_id] --AND S.[signs] & 0x0C = 0

GO

