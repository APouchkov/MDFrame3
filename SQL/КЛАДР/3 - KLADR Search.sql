ALTER FUNCTION [dbo].[KLADR::Search]
(
  @Level_Id       TinyInt,
  @ParentLevel_Id TinyInt       = 0,
  @ParentCode     VarChar(19)   = NULL, -- код родителя,
  @Article        VarChar(20)   = NULL,
  @Name           VarChar(100)  = NULL, -- наименование
  @Next1Name      VarChar(100)  = NULL,
  @Next2Name      VarChar(100)  = NULL,
  @Index          VarChar(10)   = NULL
)
  RETURNS @Result TABLE
  (
    [Level_Id]          TinyInt       NOT NULL,
    [Code]              VarChar(19)       NULL,
    [Name]              VarChar(100)  NOT NULL,
    [NextName]          VarChar(100)      NULL,
    [Article]           VarChar(19)       NULL,
    [Index]             VarChar(7)        NULL,
    [ErrorDescription]  NVarChar(200)     NULL
  )
AS BEGIN
  IF @Level_Id IS NULL OR @Name IS NULL OR @Level_Id > 4 AND @ParentCode IS NULL
    RETURN

  DECLARE
    @Code           VarChar(19),
    @ParentCodeLen  TinyInt,
    @NewLevel_Id    TinyInt,
    @SQL            NVarChar(Max),
    @I              Int

    -- Фильтр по родителям
    SELECT
      @ParentCode = CASE
                      WHEN @Level_Id = 5 OR @ParentLevel_Id = 4 THEN COALESCE ([CodeShort4], [CodeShort3], [CodeShort1])                -- Улицы по населенному пункту, городу и региону (Москва - регион)
                      WHEN @Level_Id = 6 OR @ParentLevel_Id = 5 THEN COALESCE ([CodeShort5], [CodeShort4], [CodeShort3], [CodeShort1])  -- Дома
                      WHEN @ParentLevel_Id = 0 OR ISNULL(@ParentCode, '') = '' THEN NULL
                      WHEN @ParentLevel_Id = 1 THEN [CodeShort1]                                                          -- Районы по региону
                      WHEN @ParentLevel_Id = 2 THEN COALESCE ([CodeShort2], [CodeShort1])                                 -- Города по району, региону
                      WHEN @ParentLevel_Id = 3 THEN COALESCE ([CodeShort3], [CodeShort2], [CodeShort1])                   -- Населенные пункты по городу, району, региону
                    END
    FROM [dbo].[KLADR::Extract Codes::By Code](@ParentCode)

    SET @ParentCodeLen = LEN(@ParentCode)
    IF @Level_Id <= 4 BEGIN
      INSERT INTO @Result ([Level_Id], [Code], [Article], [Name], [NextName], [Index])
      SELECT TOP 2
        [Level_Id]  = @Level_Id,
        [Code]      = [Code],
        [Article]   = [Article],
        [Name]      = [Name],
        [NextName]  = @Next1Name,
        [Index]     = [Index]
      FROM
      (
        SELECT
          [Code]        = [Code],
          [Name]        = [Name],
          [Article]     = [Socr],
          [Index]       = [Index],
          [Level]       = [Level],
          [Status]      = [STATUS],
          [@CODE:12-13] = [@CODE:12-13],
          [Count]       = COUNT(*) OVER()
          --[Min Level]   = MIN([Level]) OVER()
          --[Min Status]  = MIN([STATUS]) OVER()
        FROM [dbo].[KLADR:Base]
        WHERE [Name] = @Name
          AND [Level] = @Level_Id
          AND (@Article IS NULL OR [Socr] = @Article)
          AND (@ParentLevel_Id = 0 OR [Code] LIKE @ParentCode + '%')
      ) I
      WHERE I.[Count] = 1
        OR
        I.[@CODE:12-13] = 0
        --(I.[STATUS] IN (2,3) AND I.[Level] = I.[Min Level])
        --(I.[Count] = 2 AND I.[Min] = 0 AND I.[STATUS] = 2)

      SET @I = @@ROWCOUNT
      IF @I > 1 BEGIN
        DELETE @Result WHERE @Index IS NULL OR [Index] <> @Index
        SET @I = (SELECT COUNT(*) FROM @Result)
        IF @I <> 1 BEGIN
          DELETE @Result
          INSERT INTO @Result([Level_Id], [Article], [Name], [NextName], [ErrorDescription])
          VALUES(@Level_Id, @Article, @Name, @Next1Name, 'В КЛАДРе найдено больше одной записи с таким наименованием субъекта. Уточнение по почтовому индексу ситуацию не изменило')
        END
      END ELSE IF @I = 1 BEGIN
        SELECT
          @Code = [Code]
        FROM @Result

        IF Right(@Code, 2) = '51' BEGIN
          SET @I = -1
          WHILE @I = -1 OR @@ROWCOUNT = 1 BEGIN
            SET @I += 1
            SELECT
              @NewLevel_Id  = [level],
              @Code         = [NewCode]
            FROM [dbo].[KLADR:AltNames] AN
            WHERE [OldCode] = Left(@Code, Len(@Code) - 2) + '00'
          END

          IF @I > 0 BEGIN
            DELETE @Result
            IF @NewLevel_Id < 5
              INSERT INTO @Result ([Level_Id], [Code], [Article], [Name], [NextName], [Index])
              SELECT TOP 2 [Level], [Code], [SOCR], [Name], @Next1Name, [Index]
              FROM [dbo].[KLADR:Base]
              WHERE [Code] = @Code
            ELSE IF @NewLevel_Id = 5
              INSERT INTO @Result ([Level_Id], [Code], [Article], [Name], [NextName], [Index])
              SELECT TOP 2 @NewLevel_Id, [Code], [SOCR], [Name], @Next1Name, [Index]
              FROM [dbo].[KLADR:Streets]
              WHERE [Code] = @Code

            IF @@ROWCOUNT > 1 BEGIN
              DELETE @Result
              INSERT INTO @Result([Level_Id], [Article], [Name], [NextName], [ErrorDescription]) VALUES(@Level_Id, @Article, @Name, @Next1Name, 'Объект был переподчинён. По таблице переименований/переподчинений новое наименование не найдено.')
            END
          END
        END
      END
    END ELSE IF @Level_Id = 5 BEGIN
      INSERT INTO @Result ([Level_Id], [Code], [Article], [Name], [NextName], [Index])
      SELECT
        [Level_Id]        = @Level_Id,
        [Code]            = [Code],
        [Article]         = [Article],
        [Name]            = [Name],
        [NextName]        = @Next1Name,
        [Index]           = [Index]
      FROM
      (
        SELECT
          [Code]        = [Code],
          [@CODE:16-17] = [@CODE:16-17],
          [Name]        = [Name],
          [Article]     = [Socr],
          [Index]       = [Index],
          [Count]       = COUNT(*) OVER()
          --[Count:Index] = SUM(CASE WHEN [@CODE:16-17] = '00' AND [Index] = @Index THEN 1 ELSE 0 END) OVER()
        FROM [dbo].[KLADR:Streets]
        WHERE [@CODE:1-2] = SubString(@ParentCode, 1, 2)
          AND (LEN(@ParentCode) < 5 OR [@CODE:3-5] = SubString(@ParentCode, 3, 3))
          AND (LEN(@ParentCode) < 8 OR [@CODE:6-8] = SubString(@ParentCode, 6, 3))
          AND (LEN(@ParentCode) < 11 OR [@CODE:9-11] = SubString(@ParentCode, 9, 3))
          --AND LEFT([CODE], LEN(@ParentCode)) = @ParentCode
          AND (@Article IS NULL OR [Socr] = @Article)
          AND [Name] = @Name
      ) I
      WHERE I.[Count] = 1
            OR
            (
              I.[Article] = @Article
              AND
              I.[@CODE:16-17] = '00' -- NOT IN (51, 99)
              --AND
              --(I.[Count:Index] = 0 OR I.[Index] = @Index)
            )

      SET @I = @@ROWCOUNT
      IF @I = 1 BEGIN
        SELECT
          @Code = [Code]
        FROM @Result

        IF Right(@Code, 2) = '51' BEGIN
          SELECT
            @Code = [NewCode]
          FROM [dbo].[KLADR:AltNames] AN
          WHERE /*[level] = @Level_Id AND*/ [OldCode] = Left(@Code, Len(@Code) - 2) + '00'
          IF @@ROWCOUNT = 1 BEGIN
            DELETE @Result
            INSERT INTO @Result ([Level_Id], [Code], [Article], [Name], [NextName], [Index])
            SELECT TOP 2 @Level_Id, [Code], [SOCR], [Name], @Next1Name, [Index]
            FROM [dbo].[KLADR:Streets]
            WHERE [Code] = @Code
            IF @@ROWCOUNT > 1 BEGIN
              DELETE @Result
              INSERT INTO @Result([Level_Id], [Article], [Name], [NextName], [ErrorDescription]) VALUES(@Level_Id, @Article, @Name, @Next1Name, 'Улица была переименована. По таблице переименований/переподчинений новое наименование не найдено.')
            END
          END
        END ELSE IF Cast(Right(@Code, 2) AS TinyInt) BetWeen 1 AND 50
          UPDATE R SET
            [Article] = S.[SOCR],
            [Name]    = S.[NAME],
            [Code]    = S.[Code]
          FROM @Result R
          INNER JOIN [dbo].[KLADR:Streets] S ON S.[Code] = Left(@Code, Len(@Code) - 2) + '00'
      END IF @I > 1 BEGIN
        UPDATE R SET
          [Name] = '-'
        FROM @Result R
        WHERE NOT EXISTS(SELECT 1 FROM [dbo].[KLADR::Search Dom]([Code], NULL, @Next1Name, NULL, NULL) WHERE [Code] IS NOT NULL)

        SET @I = (SELECT COUNT(*) FROM @Result WHERE [Name] <> '-')
        IF @I <> 1 AND @Index <> '' BEGIN
          -- Удаляем "Ненужные при множественном совпадении домов" или "Без индекса" или "С другим индексом"
          DELETE @Result WHERE (@I > 1 AND [Name] = '-') OR [Index] IS NULL OR [Index] <> @Index
          SET @I = (SELECT COUNT(*) FROM @Result)
          IF @I = 1
            UPDATE @Result SET [Name] = @Name
        END ELSE IF @I = 1
          DELETE @Result WHERE [Name] = '-'

        IF @I <> 1 BEGIN
          DELETE @Result
          INSERT INTO @Result([Level_Id], [Article], [Name], [NextName], [ErrorDescription]) VALUES(@Level_Id, @Article, @Name, @Next1Name, 'В КЛАДРе найдено больше одной записи с таким наименованием улицы. Уточнение по номеру дома или почтовому индексу ситуацию не изменило')
        END
      END
    END ELSE IF @Level_Id = 6 BEGIN
      IF @Next1Name LIKE 'стр.%' OR @Next1Name LIKE 'корп.%' BEGIN
        SET @Name = @Name + ' ' + @Next1Name
        SET @Next1Name = @Next2Name
        SET @Next2Name = NULL
        IF @Next1Name LIKE 'стр.%' OR @Next1Name LIKE 'корп.%' BEGIN
          SET @Name = @Name + ' ' + @Next1Name
          SET @Next1Name = NULL
        END
      END
    
      INSERT INTO @Result ([Level_Id], [Code], [Article], [Name], [NextName], [Index], [ErrorDescription])
      SELECT
        [Level_Id]        = @Level_Id,
        [Code]            = [Code],
        [Article]         = @Article,
        [Name]            = @Name,
        [NextName]        = @Next1Name,
        [Index]           = [Index],
        [ErrorDescription]= [ErrorDescription]
      FROM [dbo].[KLADR::Search Dom](@ParentCode, NULL, @Name, NULL, NULL)
    END

  SELECT @Code = [Code] FROM @Result
  SET @I = @@ROWCOUNT
  IF @Level_Id < 5
    IF (@I = 0 OR @Level_Id = 4 AND Cast(Right(@Code, 2) AS TinyInt) BetWeen 1 AND 50) BEGIN
      INSERT INTO @Result ([Level_Id], [Code], [Article], [Name], [NextName], [Index], [ErrorDescription])
      SELECT [Level_Id], [Code], [Article], [Name], [NextName], [Index], [ErrorDescription]
      FROM [dbo].[KLADR::Search](@Level_Id + 1, @ParentLevel_Id, @ParentCode, @Article, @Name, @Next1Name, @Next2Name, @Index)
      WHERE @I = 0 OR [Code] IS NOT NULL
      IF @@ROWCOUNT = 1 AND @I > 0
        DELETE @Result WHERE [Level_Id] = @Level_Id
    END ELSE IF @I = 1 AND Cast(Right(@Code, 2) AS TinyInt) = 99
      UPDATE @Result SET
        [Code]              = NULL,
        [ErrorDescription]  = N'Адресный объект не существует, т.е. нет соответствующего ему актуального адресного объекта'

  RETURN
END
GO


SELECT * FROM [dbo].[KLADR::Search](4, 3, '7700000200000', NULL, N'к.914', 'дом 5А', 'корп.2', NULL)
--SELECT * FROM [dbo].[KLADR::Search](5, 3, '7700000200000', 'ул', N'к.914', 'дом 5А', NULL)
--SELECT * FROM [dbo].[KLADR::Search](3, 2, '6001400000000', 'г', N'Остров', NULL, NULL, NULL)

--SELECT * FROM [dbo].[KLADR::Search Dom]('7700000200000', NULL, N'к.914', NULL, NULL)

--SELECT * FROM [dbo].[KLADR::Search(Pre)](3, '0100000100000', N'ул.Прямая', 'дом 5А', NULL)

--SELECT * FROM [dbo].[KLADR::Search](6, 4, '3500600001000', 'ДОМ', N'66', NULL, NULL)
--SELECT * FROM [dbo].[KLADR:AltNames] WHERE RIght(OLDCODE, 2) <> '00'
