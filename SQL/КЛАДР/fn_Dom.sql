/****** Object:  UserDefinedFunction [dbo].[KLADR::Search Dom]    Script Date: 30.07.2015 18:26:22 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER FUNCTION [dbo].[KLADR::Search Dom]
(
  @StreetCode   VarChar(17),
  @HouseKind    VarChar(50),  -- NULL, ДОМ, Влд, ДВлд
  @House        VarChar(50),  -- ЦЦ, ЦЦ-ЦЦ, ЦЦ/ЦЦ, ЦЦ<ЛИТЕРА>
  @Housing      VarChar(10)   = NULL,
  @Building     VarChar(10)   = NULL
)
  RETURNS @Result TABLE
  (
    [Code]              VarChar(19)        NULL,
    [Index]             VarChar(7)         NULL,
    [ErrorDescription]  NVarChar(200)      NULL
  )
AS BEGIN
  DECLARE
    @FullHouse    VarChar(50), -- полный адрес (дом + корпус + строение)
    @NumberType   Bit,         -- 1-нечетное, 0 - четное
    @Name         VarChar(50),
    @Code         VarChar(19),
    @Index        VarChar(7),
    @I            TinyInt,
    @BeginNumber  Int,
    @EndNumber    Int


  SET @I = LEN(@StreetCode)
  IF @I = 8
    SET @StreetCode += '0000000'
  ELSE IF @I = 11
    SET @StreetCode += '0000'
  ELSE IF @I = 13
    SET @StreetCode = Left(@StreetCode, 11) + '0000'
  ELSE IF @I = 17
    SET @StreetCode = Left(@StreetCode, 15)
  ELSE IF @I <> 15
    RETURN

  IF LEFT(@House, 4) IN ('дом ') SET @House = SUBSTRING(@House, 5, 100)
  IF LEFT(@House, 2) IN ('д ', 'д.') SET @House = SUBSTRING(@House, 3, 100)

  SET @House = LOWER(REPLACE(REPLACE(REPLACE(REPLACE(@House, ' ', ''), '.', ''), '"', ''), '-', '_'))

  IF @House = '' BEGIN
    SET @HouseKind  = NULL
    SET @House      = NULL
  END ELSE BEGIN
    --IF @House LIKE 'дом[0-9]%' SET @House = SUBSTRING(@House, 4, 100)
    --IF @House LIKE 'д[0-9]%' SET @House = SUBSTRING(@House, 2, 100)

    SET @House = REPLACE(@House, 'корп', 'к')
    SET @House = REPLACE(@House, 'строение', 'стр')
  END

  IF @Housing IS NULL AND @Building IS NULL AND @House NOT LIKE '%[^0-9]%'
    SET @NumberType = Cast(@House AS Int) % 2 -- допустим только цифра (0 - четное 1 - не четное)
  ELSE
    SET @NumberType = NULL
 
  SET @FullHouse = IsNull(NullIf(@HouseKind, 'дом'), '') + @House
  IF @Housing IS NOT NULL
    SET @FullHouse = IsNull(@FullHouse, '') + 'к' + @Housing
  IF @Building IS NOT NULL
    SET @FullHouse = IsNull(@FullHouse, '') + 'стр' + @Building

  DECLARE ITEMS_CURSOR CURSOR FAST_FORWARD LOCAL FOR
  SELECT DISTINCT
    [Name]  = V2.[Value],
    [Code]  = D.[Code],
    [Index] = D.[Index]
  FROM [dbo].[KLADR:Doma] D
  CROSS APPLY [Pub].[Split](D.[Name], ',') V1
  CROSS APPLY (VALUES(REPLACE(LTRIM(RTRIM(V1.[Value])), 'литер', ''))) V2([Value])
  WHERE D.[@CODE:1-15] = @StreetCode
    AND
    (
      V2.[Value] IN (@FullHouse, 'влд' + @FullHouse, 'двлд' + @FullHouse)
      OR
      @NumberType IS NOT NULL AND (LEFT(V2.[Value], 1) IN ('Н', 'Ч') OR CharIndex('-', V2.[Value]) > 1)
    )

  -- Цикл по каждому дому
  OPEN ITEMS_CURSOR
  WHILE (1 = 1) BEGIN
    FETCH NEXT FROM ITEMS_CURSOR
    INTO @Name, @Code, @Index
    IF @@FETCH_STATUS <> 0 BEGIN
      INSERT INTO @Result([ErrorDescription])
      SELECT
        [ErrorDescription] = CASE
                              WHEN C.[Count] = 0 THEN N'В КЛАДР отсутствуют данные по номерам домов данного объекта'
                              ELSE N'Конкретный дом в КЛАДР не обнаружен'
                             END
      FROM
      (
        SELECT
          [Count] = COUNT(*)
        FROM [dbo].[KLADR:Doma] D
        WHERE D.[@CODE:1-15] = @StreetCode
      ) C

      RETURN
    END

    IF @Name IN (@FullHouse, 'влд' + @FullHouse, 'двлд' + @FullHouse)
      BREAK
    ELSE IF @NumberType IS NOT NULL
      IF LEFT(@Name, 2) = 'Н(' BEGIN
        IF @NumberType = 1
          IF LEN(@Name) = 1
            BREAK
          ELSE BEGIN
            SET @Name = SUBSTRING(@Name, 3, LEN(@Name) - 3)
            SET @I = CHARINDEX('-', @Name)
            SET @BeginNumber  = CAST(SUBSTRING(@Name, 1, @I - 1) AS Int)
            SET @EndNumber    = CAST(SUBSTRING(@Name, @I + 1, LEN(@Name)) AS Int)
            IF CAST(@House AS Int) BETWEEN @BeginNumber AND @EndNumber
              BREAK
          END
      END ELSE IF LEFT(@Name, 2) = 'Ч(' BEGIN
        IF @NumberType = 0
          IF LEN(@Name) = 1
            BREAK
          ELSE BEGIN
            SET @Name = SUBSTRING(@Name, 3, LEN(@Name) - 3)
            SET @I = CHARINDEX('-', @Name)
            SET @BeginNumber  = CAST(SUBSTRING(@Name, 1, @I - 1) AS Int)
            SET @EndNumber    = CAST(SUBSTRING(@Name, @I + 1, LEN(@Name)) AS Int)
            IF CAST(@House AS Int) BETWEEN @BeginNumber AND @EndNumber
              BREAK
          END
      END ELSE IF CHARINDEX('-', @Name) > 1 /*AND LEFT(@Name, 2) NOT IN ('Ч(', 'Н(')*/ BEGIN
        SET @I = CHARINDEX('-', @Name)
        SET @BeginNumber = CAST(SUBSTRING(@Name, 1, @I - 1) AS Int)
        SET @EndNumber   = CAST(SUBSTRING(@Name, @I + 1, LEN(@Name)) AS Int)
        IF CAST(@House AS Int) BETWEEN @BeginNumber AND @EndNumber
          BREAK
      END
  END

  CLOSE ITEMS_CURSOR
  DEALLOCATE ITEMS_CURSOR

  INSERT INTO @Result([Code], [Index])
  VALUES(@Code, @Index)

  RETURN
END
