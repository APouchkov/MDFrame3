SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO
/*
  -- Пример вызова:
  EXEC [dbo].[SVU::CCP Risk Rates]
*/
CREATE PROCEDURE [dbo].[SVU::CCP Risk Rates]
AS
  SET NOCOUNT ON

  SELECT
    [PlaceCode]       = 'MICEX:S',
    [InstrumentCode]  = [SECCODE],
    [Num]             = [CCPNUM],
    [ValueFrom]       = PR.[QTYBEGIN],
    [ValueTo]         = NULLIF(PR.[QTYEND], 0),
    [RateLow]         = PR.[DISCOUNT_L] * 10,
    [RateHigh]        = PR.[DISCOUNT_H] * 10
  FROM [dbo].[Mcx_CCP_PRICERANGE] PR
GO
