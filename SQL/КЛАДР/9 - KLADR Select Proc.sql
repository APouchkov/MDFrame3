USE [opendb]
GO

/****** Object:  StoredProcedure [dbo].[KLADR::Select]    Script Date: 03/30/2016 11:32:04 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[KLADR::Select]
  @Level1Socr   VarChar(20)   = NULL,
  @Level1Name   VarChar(100)  = NULL,
  @Level2Socr   VarChar(20)   = NULL,
  @Level2Name   VarChar(100)  = NULL,
  @Level3Socr   VarChar(20)   = NULL,
  @Level3Name   VarChar(100)  = NULL,
  @Level4Socr   VarChar(20)   = NULL,
  @Level4Name   VarChar(100)  = NULL,
  @Level5Socr   VarChar(20)   = NULL,
  @Level5Name   VarChar(100)  = NULL,
  @Level6Socr   VarChar(20)   = NULL,
  @Level6Name   VarChar(100)  = NULL,
  @SelectLevel  TinyInt
AS
  SET NOCOUNT ON

  DECLARE @ParentCodes TABLE
  (
    [CODE:1-2]  Char(2) COLLATE Cyrillic_General_BIN  NOT NULL,
    [CODE:3-5]  Char(3) COLLATE Cyrillic_General_BIN      NULL,
    [CODE:6-8]  Char(3) COLLATE Cyrillic_General_BIN      NULL,
    [CODE:9-11] Char(3) COLLATE Cyrillic_General_BIN      NULL
  )

  DECLARE
    @ParentLevel  TinyInt = 0

  IF @SelectLevel = 1 BEGIN
    SELECT
      [Level1Name] = [NAME], [Level1Socr] = [SOCR]
      , [NAME], [SOCR] -- TODO: OLD
    FROM [dbo].[KLADR:Base]
    WHERE [LEVEL] = 1
            AND (@Level1Socr IS NULL OR @Level1Socr = [SOCR])
            AND CHARINDEX(@Level1Name, [Name]) > 0
    ORDER BY 1, 2

    RETURN
  END

  IF @Level1Name IS NOT NULL BEGIN
    INSERT INTO @ParentCodes([CODE:1-2])
    SELECT [@CODE:1-2]
    FROM [dbo].[KLADR:Base]
    WHERE [LEVEL] = 1 AND [Name] = @Level1Name AND [SOCR] = @Level1Socr
    
    SET @ParentLevel = 1
  END

  IF @SelectLevel = 2 BEGIN
    IF @ParentLevel = 0
      SELECT DISTINCT
        [Level2Name] = K2.[NAME], [Level2Socr] = K2.[SOCR], K2.[@CODE:STATUS],
        [Level1Name] = K1.[NAME], [Level1Socr] = K1.[SOCR]
      FROM [dbo].[KLADR:Base] K2
      LEFT JOIN [dbo].[KLADR:Base] K1 ON K2.[@CODE:1-2] = K1.[@CODE] AND K1.[@CODE:STATUS] = '00'
      WHERE K2.[LEVEL] = 2
              AND (@Level2Socr IS NULL OR @Level2Socr = K2.[SOCR])
              AND CHARINDEX(@Level2Name, K2.[Name]) > 0
      ORDER BY 1, 2
    ELSE
      SELECT DISTINCT
        [Level2Name] = K2.[NAME], [Level2Socr] = K2.[SOCR], K2.[@CODE:STATUS],
        [Level1Name] = K1.[NAME], [Level1Socr] = K1.[SOCR]
      FROM @ParentCodes P
      INNER JOIN [dbo].[KLADR:Base] K2 ON P.[CODE:1-2] = K2.[@CODE:1-2]
              AND K2.[LEVEL] = 2
              AND (@Level2Socr IS NULL OR @Level2Socr = K2.[SOCR])
              AND CHARINDEX(@Level2Name, K2.[Name]) > 0
      LEFT JOIN [dbo].[KLADR:Base] K1 ON K2.[@CODE:1-2] = K1.[@CODE]
      ORDER BY 1, 2
      OPTION (RECOMPILE)

    RETURN
  END

  IF @Level2Name IS NOT NULL BEGIN
    IF @ParentLevel = 0
      INSERT INTO @ParentCodes([CODE:1-2], [CODE:3-5])
      SELECT [@CODE:1-2], [@CODE:3-5]
      FROM [dbo].[KLADR:Base]
      WHERE [LEVEL] = 2 AND [Name] = @Level2Name AND [SOCR] = @Level2Socr
    ELSE
      MERGE @ParentCodes P
      USING
      (
        SELECT
          [CODE:1-2] = K.[@CODE:1-2], [CODE:3-5] = K.[@CODE:3-5]
        FROM @ParentCodes P
        INNER JOIN [dbo].[KLADR:Base] K ON K.[LEVEL] = 2 AND K.[Name] = @Level2Name AND K.[SOCR] = @Level2Socr
                                              AND P.[CODE:1-2] = K.[@CODE:1-2]
      ) I ON 1 = 0
      WHEN NOT MATCHED BY TARGET THEN INSERT ([CODE:1-2], [CODE:3-5]) VALUES(I.[CODE:1-2], I.[CODE:3-5])
      WHEN NOT MATCHED BY SOURCE THEN DELETE
      OPTION (RECOMPILE)
      ;

    SET @ParentLevel = 2
  END

  IF @SelectLevel = 3 BEGIN
    IF @ParentLevel = 0
      SELECT DISTINCT
        [Level3Name] = K3.[NAME], [Level3Socr] = K3.[SOCR], K3.[@CODE:STATUS],
        [Level2Name] = K2.[NAME], [Level2Socr] = K2.[SOCR],
        [Level1Name] = K1.[NAME], [Level1Socr] = K1.[SOCR]
      FROM [dbo].[KLADR:Base] K3
      LEFT JOIN [dbo].[KLADR:Base] K2 ON K3.[@CODE:1-2] + K3.[@CODE:3-5] = K2.[@CODE] AND K2.[@CODE:STATUS] = '00'
      LEFT JOIN [dbo].[KLADR:Base] K1 ON K3.[@CODE:1-2] = K1.[@CODE] AND K1.[@CODE:STATUS] = '00'
      WHERE K3.[LEVEL] = 3
              AND (@Level3Socr IS NULL OR @Level3Socr = K3.[SOCR])
              AND CHARINDEX(@Level3Name, K3.[Name]) > 0
              AND K3.[@CODE:STATUS] = '00'
      ORDER BY 1, 2
    ELSE
      SELECT DISTINCT
        [Level3Name] = K3.[NAME], [Level3Socr] = K3.[SOCR], K3.[@CODE:STATUS],
        [Level2Name] = K2.[NAME], [Level2Socr] = K2.[SOCR],
        [Level1Name] = K1.[NAME], [Level1Socr] = K1.[SOCR]
      FROM @ParentCodes P
      INNER JOIN [dbo].[KLADR:Base] K3 ON P.[CODE:1-2] = K3.[@CODE:1-2]
              AND (@Level2Name IS NULL OR P.[CODE:3-5] = K3.[@CODE:3-5])
              AND K3.[LEVEL] = 3
              AND (@Level3Socr IS NULL OR @Level3Socr = K3.[SOCR])
              AND CHARINDEX(@Level3Name, K3.[Name]) > 0
              AND K3.[@CODE:STATUS] = '00'
      LEFT JOIN [dbo].[KLADR:Base] K2 ON K3.[@CODE:1-2] + K3.[@CODE:3-5] = K2.[@CODE] AND K2.[@CODE:STATUS] = '00'
      LEFT JOIN [dbo].[KLADR:Base] K1 ON K3.[@CODE:1-2] = K1.[@CODE] AND K1.[@CODE:STATUS] = '00'
      ORDER BY 1, 2
      OPTION (RECOMPILE)

    RETURN
  END

  IF @Level3Name IS NOT NULL BEGIN
    IF @ParentLevel = 0
      INSERT INTO @ParentCodes([CODE:1-2], [CODE:3-5], [CODE:6-8])
      SELECT [@CODE:1-2], [@CODE:3-5], [@CODE:6-8]
      FROM [dbo].[KLADR:Base]
      WHERE [LEVEL] = 3 AND [Name] = @Level3Name AND [SOCR] = @Level3Socr
    ELSE
      MERGE @ParentCodes P
      USING
      (
        SELECT
          [CODE:1-2] = K.[@CODE:1-2], [CODE:3-5] = K.[@CODE:3-5], [CODE:6-8] = K.[@CODE:6-8]
        FROM @ParentCodes P
        INNER JOIN [dbo].[KLADR:Base] K ON K.[LEVEL] = 3 AND K.[Name] = @Level3Name AND K.[SOCR] = @Level3Socr
                                              AND P.[CODE:1-2] = K.[@CODE:1-2]
                                              AND (@Level2Name IS NULL OR P.[CODE:3-5] = K.[@CODE:3-5])
      ) I ON 1 = 0
      WHEN NOT MATCHED BY TARGET THEN INSERT ([CODE:1-2], [CODE:3-5], [CODE:6-8]) VALUES(I.[CODE:1-2], I.[CODE:3-5], I.[CODE:6-8])
      WHEN NOT MATCHED BY SOURCE THEN DELETE
      OPTION (RECOMPILE)
      ;

    SET @ParentLevel = 3
  END

  IF @SelectLevel = 4 BEGIN
    IF @ParentLevel = 0
      SELECT DISTINCT
        [Level4Name] = K4.[NAME], [Level4Socr] = K4.[SOCR], K4.[@CODE:STATUS],
        [Level3Name] = K3.[NAME], [Level3Socr] = K3.[SOCR],
        [Level2Name] = K2.[NAME], [Level2Socr] = K2.[SOCR],
        [Level1Name] = K1.[NAME], [Level1Socr] = K1.[SOCR]
      FROM [dbo].[KLADR:Base] K4
      LEFT JOIN [dbo].[KLADR:Base] K3 ON K4.[@CODE:1-2] + K4.[@CODE:3-5] + K4.[@CODE:6-8] = K3.[@CODE] AND K3.[@CODE:STATUS] = '00'
      LEFT JOIN [dbo].[KLADR:Base] K2 ON K3.[@CODE:1-2] + K3.[@CODE:3-5] = K2.[@CODE] AND K2.[@CODE:STATUS] = '00'
      LEFT JOIN [dbo].[KLADR:Base] K1 ON K3.[@CODE:1-2] = K1.[@CODE] AND K1.[@CODE:STATUS] = '00'
      WHERE K4.[LEVEL] = 4
              AND (@Level4Socr IS NULL OR @Level4Socr = K4.[SOCR])
              AND CHARINDEX(@Level4Name, K4.[Name]) > 0
      ORDER BY 1, 2
    ELSE
      SELECT DISTINCT
        [Level4Name] = K4.[NAME], [Level4Socr] = K4.[SOCR], K4.[@CODE:STATUS],
        [Level3Name] = K3.[NAME], [Level3Socr] = K3.[SOCR],
        [Level2Name] = K2.[NAME], [Level2Socr] = K2.[SOCR],
        [Level1Name] = K1.[NAME], [Level1Socr] = K1.[SOCR]
      FROM @ParentCodes P
      INNER JOIN [dbo].[KLADR:Base] K4 ON P.[CODE:1-2] = K4.[@CODE:1-2]
              AND (@Level2Name IS NULL OR P.[CODE:3-5] = K4.[@CODE:3-5])
              AND (@Level3Name IS NULL OR P.[CODE:6-8] = K4.[@CODE:6-8])
              AND K4.[LEVEL] = 4
              AND (@Level4Socr IS NULL OR @Level4Socr = K4.[SOCR])
              AND CHARINDEX(@Level4Name, K4.[Name]) > 0
      LEFT JOIN [dbo].[KLADR:Base] K3 ON K4.[@CODE:1-2] + K4.[@CODE:3-5] + K4.[@CODE:6-8] = K3.[@CODE] AND K3.[@CODE:STATUS] = '00'
      LEFT JOIN [dbo].[KLADR:Base] K2 ON K4.[@CODE:1-2] + K4.[@CODE:3-5] = K2.[@CODE] AND K2.[@CODE:STATUS] = '00'
      LEFT JOIN [dbo].[KLADR:Base] K1 ON K4.[@CODE:1-2] = K1.[@CODE] AND K1.[@CODE:STATUS] = '00'
      ORDER BY 1, 2
      OPTION (RECOMPILE)

    RETURN
  END

  IF @Level4Name IS NOT NULL BEGIN
    IF @ParentLevel = 0
      INSERT INTO @ParentCodes([CODE:1-2], [CODE:3-5], [CODE:6-8], [CODE:9-11])
      SELECT [@CODE:1-2], [@CODE:3-5], [@CODE:6-8], [@CODE:9-11]
      FROM [dbo].[KLADR:Base]
      WHERE [LEVEL] = 4 AND [Name] = @Level4Name AND [SOCR] = @Level4Socr
    ELSE
      MERGE @ParentCodes P
      USING
      (
        SELECT
          [CODE:1-2] = K.[@CODE:1-2], [CODE:3-5] = K.[@CODE:3-5], [CODE:6-8] = K.[@CODE:6-8], [CODE:9-11] = K.[@CODE:9-11]
        FROM @ParentCodes P
        INNER JOIN [dbo].[KLADR:Base] K ON K.[LEVEL] = 4 AND K.[Name] = @Level4Name AND K.[SOCR] = @Level4Socr
                                              AND P.[CODE:1-2] = K.[@CODE:1-2]
                                              AND (@Level2Name IS NULL OR P.[CODE:3-5] = K.[@CODE:3-5])
                                              AND (@Level3Name IS NULL OR P.[CODE:6-8] = K.[@CODE:6-8])
      ) I ON 1 = 0
      WHEN NOT MATCHED BY TARGET THEN INSERT ([CODE:1-2], [CODE:3-5], [CODE:6-8], [CODE:9-11]) VALUES(I.[CODE:1-2], I.[CODE:3-5], I.[CODE:6-8], I.[CODE:9-11])
      WHEN NOT MATCHED BY SOURCE THEN DELETE
      OPTION (RECOMPILE)
      ;

    SET @ParentLevel = 4
  END

  IF @SelectLevel = 5 BEGIN
    IF @ParentLevel = 0 RETURN

    SELECT DISTINCT
      [Level5Name] = K5.[NAME], [Level5Socr] = K5.[SOCR], [@CODE:STATUS] = K5.[@CODE:STATUS],
      [Level4Name] = K4.[NAME], [Level4Socr] = K4.[SOCR],
      [Level3Name] = K3.[NAME], [Level3Socr] = K3.[SOCR],
      [Level2Name] = K2.[NAME], [Level2Socr] = K2.[SOCR],
      [Level1Name] = K1.[NAME], [Level1Socr] = K1.[SOCR]
    FROM @ParentCodes P
    INNER JOIN [dbo].[KLADR:Streets] K5 ON
            P.[CODE:1-2] + IsNull(P.[CODE:3-5], '000') + IsNull(P.[CODE:6-8], '000') + IsNull(P.[CODE:9-11], '000') = K5.[@CODE:PARENT]
            AND (@Level5Socr IS NULL OR @Level5Socr = K5.[SOCR])
            AND CHARINDEX(@Level5Name, K5.[Name]) > 0
    LEFT JOIN [dbo].[KLADR:Base] K4 ON K5.[@CODE:1-2] + K5.[@CODE:3-5] + K5.[@CODE:6-8] + K5.[@CODE:9-11] = K4.[@CODE] AND K4.[@CODE:STATUS] = '00'
    LEFT JOIN [dbo].[KLADR:Base] K3 ON K5.[@CODE:1-2] + K5.[@CODE:3-5] + K5.[@CODE:6-8] = K3.[@CODE] AND K3.[@CODE:STATUS] = '00'
    LEFT JOIN [dbo].[KLADR:Base] K2 ON K5.[@CODE:1-2] + K5.[@CODE:3-5] = K2.[@CODE] AND K2.[@CODE:STATUS] = '00'
    LEFT JOIN [dbo].[KLADR:Base] K1 ON K5.[@CODE:1-2] = K1.[@CODE] AND K1.[@CODE:STATUS] = '00'
    ORDER BY 1, 2
    OPTION (RECOMPILE)

    RETURN
  END



GO

