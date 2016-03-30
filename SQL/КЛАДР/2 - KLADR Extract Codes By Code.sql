ALTER FUNCTION [dbo].[KLADR::Extract Codes::By Code] (@Code VarChar(20))
  RETURNS TABLE
AS
  RETURN
  (
    SELECT
      [CodeFull1]  = V1.[Value] + '00000000000' Collate Cyrillic_General_BIN, -- регион
      [CodeFull2]  = V2.[Value] + '00000000'    Collate Cyrillic_General_BIN, -- район
      [CodeFull3]  = V3.[Value] + '00000'       Collate Cyrillic_General_BIN, -- город
      [CodeFull4]  = V4.[Value] + '00'          Collate Cyrillic_General_BIN, -- населенный пункт
      [CodeFull5]  = V5.[Value]                 Collate Cyrillic_General_BIN, -- улица
      [CodeFull6]  = V6.[Value]                 Collate Cyrillic_General_BIN, -- дом

      [CodeShort1] = V1.[Value]                 Collate Cyrillic_General_BIN,
      [CodeShort2] = V2.[Value]                 Collate Cyrillic_General_BIN,
      [CodeShort3] = V3.[Value]                 Collate Cyrillic_General_BIN,
      [CodeShort4] = V4.[Value]                 Collate Cyrillic_General_BIN,
      [CodeShort5] = V5.[Value]                 Collate Cyrillic_General_BIN,
      [CodeShort6] = V6.[Value]                 Collate Cyrillic_General_BIN
    FROM (VALUES(CASE WHEN LEN(@Code) >= 2  AND LEFT(@Code, 2) <> '00' THEN LEFT(@Code, 2) END)) V1([Value])
    CROSS APPLY (VALUES(CASE WHEN LEN(@Code) >= 5  THEN LEFT(@Code, 5)  ELSE V1.[Value] + '000' END)) V2([Value])
    CROSS APPLY (VALUES(CASE WHEN LEN(@Code) >= 8  THEN LEFT(@Code, 8)  ELSE V2.[Value] + '000' END)) V3([Value])
    CROSS APPLY (VALUES(CASE WHEN LEN(@Code) >= 11 THEN LEFT(@Code, 11) ELSE V3.[Value] + '000' END)) V4([Value])
    CROSS APPLY (VALUES(CASE WHEN LEN(@Code) >= 15 THEN LEFT(@Code, 15) ELSE V4.[Value] + '0000' END)) V5([Value])
    CROSS APPLY (VALUES(CASE WHEN LEN(@Code) >= 19 THEN LEFT(@Code, 19) ELSE V5.[Value] + '0000' END)) V6([Value])
  )
GO


    SELECT
      *,
      LEN(CodeFull1),
      LEN(CodeFull2),
      LEN(CodeFull3),
      LEN(CodeFull4),
      LEN(CodeFull5)
    FROM [dbo].[KLADR::Extract Codes::By Code]('7700000200000')
