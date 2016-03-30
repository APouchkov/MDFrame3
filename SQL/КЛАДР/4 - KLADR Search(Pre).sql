-- —— ––– √√√ ѕѕѕ јј, где
-- —— Ц код субъекта –оссийской ‘едерации (региона), коды регионов представлены в ѕриложении 2 к ќписанию классификатора адресов –оссийской ‘едерации ( Ћјƒ–);
-- ––– Ц код района;
-- √√√ Ц код города;      
-- ѕѕѕ Ц код населенного пункта;	
-- јј Ц признак актуальности наименовани€ адресного объекта.

-- —— ––– √√√ ѕѕѕ ”””” јј, где
-- —— Ц код субъекта –оссийской ‘едерации (региона), коды регионов представлены в ѕриложении 2 к ќписанию классификатора адресов –оссийской ‘едерации ( Ћјƒ–);
-- ––– Ц код района;
-- √√√ Ц код города;      
-- ѕѕѕ Ц код населенного пункта;	
-- ”””” Ц код улицы;
-- јј Ц признак актуальности наименовани€ адресного объекта.
/*
У00Ф Ц актуальный объект (его наименование, подчиненность соответствуют состо€нию на данный момент адресного пространства). 
            У01Ф-У50Ф Ц объект был переименован, в данной записи приведено одно из прежних его наименований (актуальный адресный объект присутствует в базе данных с тем же кодом, но с признаком актуальности У00Ф;
            У51Ф      Ц объект был переподчинен или влилс€ в состав другого объекта (актуальный адресный объект определ€етс€ по базе Altnames.dbf;
            У52Ф-У98Ф Ц резервные значени€ признака актуальности;
            Ф99Ф      Ц адресный объект не существует, т.е. нет соответствующего ему актуального адресного объекта.
*/
ALTER FUNCTION [dbo].[KLADR::Search(Pre)]
(
  @ParentLevel  TinyInt,
  @ParentCode   VarChar(20),
  @Name         VarChar(100),
  @Next1Name    VarChar(100),
  @Next2Name    VarChar(100),
  @Index        VarChar(10)
)
  RETURNS @Result TABLE
  (
    [Level_Id] TinyInt            NOT NULL,
    [Code]     VarChar(19)            NULL,
    [Name]     NVarChar(100)      NOT NULL,
    [NextName] NVarChar(100)          NULL,
    [Article]  NVarChar(19)           NULL,
    [Index]    VarChar(7)             NULL,
    [ErrorDescription]  NVarChar(200) NULL
  )
AS BEGIN
  IF @Name IS NULL OR @ParentLevel IS NULL OR @ParentLevel = 6 RETURN

  DECLARE
    @@NAME      VarChar(100),
    @SOCR       VarChar(50),
    @I          Int,
    @J          Int

  SET @I = IsNull(CharIndex(' ', @Name), 0)
  IF Right(@Name, 1) = '.'
    SET @J = LEN(@Name)
  ELSE
    SET @J = IsNull(CharIndex('.', @Name), 0)

  -- EQUAL
  IF @Name IN ('„увашска€ –еспублика', '„увашска€ –есп.', '–еспублика „уваши€', '„увашска€ –еспублика - „уваши€')
    SELECT
      @SOCR  = '„уваши€',
      @@NAME = '„увашска€ –еспублика -'

  -- LEFT
  ELSE IF Left(@Name, 11) = '–еспублика '
    SELECT
      @SOCR = '–есп',
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 12, 1000))
  ELSE IF Left(@Name, 9) = 'ѕроспект '
    SELECT
      @SOCR = 'пр-кт',
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 10, 1000))
  ELSE IF Left(@Name, 3) = 'ул.'
    SELECT
      @SOCR  = 'ул',
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 4, 1000))
  ELSE IF Left(@Name, 4) = 'б-р '
    SELECT
      @SOCR  = 'б-р',
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 5, 1000))
  ELSE IF Left(@Name, 6) IN (
                              'платф.',
                              'пл-ка.',
                              'пр-кт.',
                              'ст-ца.'
                            )
    SELECT
      @SOCR = Left(@Name, 5),
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 7, 1000))
  ELSE IF Left(@Name, 6) IN (
                              'алле€ ',
                              'арбан ',
                              'балка ',
                              'берег ',
                              'бугор ',
                              'бухта ',
                              'въезд ',
                              'высел ',
                              'горка ',
                              'заезд ',
                              'канал ',
                              'лини€ ',
                              'округ ',
                              'платф ',
                              'пл-ка ',
                              'пр-кт ',
                              'сквер ',
                              'спуск ',
                              'ст-ца ',
                              'тракт ',
                              'ферма '
                            )
    SELECT
      @SOCR = Left(@Name, 5),
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 7, 1000))
  ELSE IF Left(@Name, 7) IN (
                              'заимка ',
                              'кольцо ',
                              'кордон ',
                              'массив ',
                              'остров ',
                              'погост ',
                              'причал ',
                              'проезд ',
                              'просек ',
                              'проток '
                              )
    SELECT
      @SOCR  = Left(@Name, 6),
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 8, 1000))
  ELSE IF Left(@Name, 8) IN (
                              'волость ',
                              'городок ',
                              --'ж/д_рзд',
                              'жилзона ',
                              'казарма ',
                              'переезд ',
                              'починок ',
                              'просека ',
                              'протока ',
                              'проулок ',
                              'тоннель '
                            )
    SELECT
      @SOCR  = Left(@Name, 7),
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 9, 1000))
  ELSE IF Left(@Name, 9) IN (
                              'жилрайон ',
                              'промзона ',
                              'проселок '
                            )
    SELECT
      @SOCR  = Left(@Name, 8),
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 10, 1000))
  ELSE IF Left(@Name, 9) = 'местечко '
    SELECT
      @SOCR = 'м',
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 10, 1000))

  -- RIGHT
  ELSE IF Right(@Name, 11) = ' –еспублика'
    SELECT
      @SOCR = '–есп',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 11))
  ELSE IF Right(@Name, 6) = ' респ.'
    SELECT
      @SOCR = '–есп',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 6))
  ELSE IF Right(@Name, 17) = ' јвтономный округ'
    SELECT
      @SOCR  = 'јќ',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 17))

  ELSE IF Right(@Name, 2) IN (
                              ' г',
                              ' д',
                              ' м',
                              ' п',
                              ' с',
                              ' у',
                              ' х',
                              ' ш'
                              )
    SELECT
      @SOCR  = Right(@Name, 1),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 2))

  ELSE IF Right(@Name, 3) IN (
                              ' јќ',
                              ' гп',
                              ' дп',
                              ' жт',
                              ' км',
                              ' кп',
                              ' нп',
                              ' пл',
                              ' рп',
                              ' сл',
                              ' ст',
                              ' ул'
                              )
    SELECT
      @SOCR  = Right(@Name, 2),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 3))

  ELSE IF Right(@Name, 4) IN (
                              ' а/€',
                              ' аал',
                              ' аул',
                              ' б-р',
                              ' вал',
                              ' гск',
                              ' днп',
                              ' ƒќћ',
                              ' дор',
                              ' лпх',
                              ' мкр',
                              ' мыс',
                              ' н/п',
                              ' наб',
                              ' обл',
                              ' п/о',
                              ' п/р',
                              ' пгт',
                              ' пер',
                              ' рзд',
                              ' р-н',
                              ' с/а',
                              ' с/о',
                              ' с/п',
                              ' с/с',
                              ' сад',
                              ' снт',
                              ' стр',
                              ' тер',
                              ' туп',
                              ' ф/х'
                              )
    SELECT
      @SOCR  = Right(@Name, 3),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 4))

  ELSE IF Right(@Name, 5) IN (
                              ' јобл',
                              ' зона',
                              ' кв-л',
                              ' коса',
                              ' край',
                              ' ма€к',
                              ' мост',
                              ' п/ст',
                              ' парк',
                              ' –есп',
                              ' р€ды',
                              ' с/мо',
                              ' уч-к'
                              )
    SELECT
      @SOCR  = Right(@Name, 4),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 5))

  ELSE IF Right(@Name, 6) IN (
                              ' алле€',
                              ' арбан',
                              ' балка',
                              ' берег',
                              ' бугор',
                              ' бухта',
                              ' въезд',
                              ' высел',
                              ' горка',
                              ' заезд',
                              ' канал',
                              ' лини€',
                              ' округ',
                              ' платф',
                              ' пл-ка',
                              ' пр-кт',
                              ' сквер',
                              ' спуск',
                              ' ст-ца',
                              ' тракт',
                              ' ферма'
                              )
    SELECT
      @SOCR  = Right(@Name, 5),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 6))
  ELSE IF Right(@Name, 7) IN (
                              ' ж/д_оп',
                              ' ж/д_ст',
                              ' заимка',
                              ' кольцо',
                              ' кордон',
                              ' массив',
                              ' остров',
                              ' погост',
                              ' причал',
                              ' проезд',
                              ' просек',
                              ' проток'
                              )
    SELECT
      @SOCR  = Right(@Name, 6),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 7))
  ELSE IF Right(@Name, 8) IN (
                              ' волость',
                              ' городок',
                              ' ж/д_рзд',
                              ' жилзона',
                              ' казарма',
                              ' переезд',
                              ' починок',
                              ' просека',
                              ' протока',
                              ' проулок',
                              ' тоннель'
                              --' „уваши€'
                              )
    SELECT
      @SOCR  = Right(@Name, 7),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 8))
  ELSE IF Right(@Name, 9) IN (
                              ' ж/д_пост',
                              ' жилрайон',
                              ' промзона',
                              ' проселок'
                              )
    SELECT
      @SOCR  = Right(@Name, 8),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 9))
  ELSE IF Right(@@NAME, 8) = ' бульвар'
    SELECT
      @SOCR = 'б-р',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 8))
  ELSE IF Right(@Name, 9) = ' проспект'
    SELECT
      @SOCR = 'пр-кт',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 9))
  ELSE IF Right(@Name, 6) = ' шоссе'
    SELECT
      @SOCR = 'ш',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 6))
  ELSE IF Right(@Name, 16) = ' автономна€ обл.'
    SELECT
      @SOCR = 'јобл',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 16))

  -- Find '.' POINT
  ELSE IF @J > 0 BEGIN
    IF @I = 0 OR @I > @J
      SELECT
        @SOCR = [Pub].[Trim(Spaces)](Left(@Name, @J - 1)),
        @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, @J + 1, 1000))
    ELSE IF @I > 0 AND @I < @J AND @J = LEN(@Name)
      SELECT
        @I    = [Pub].[CharIndex(Back)](' ', @Name, @J - 1),
        @SOCR = [Pub].[Trim(Spaces)](SubString(@Name, @I + 1, @J - @I - 1)),
        @@NAME = [Pub].[Trim(Spaces)](Left(@Name, @I - 1))
  END ELSE
    SET @@NAME = @Name

  IF @SOCR = '–есп' AND @@NAME = '—аха (якути€)'
    SET @@NAME = '—аха /якути€/'

  INSERT INTO @Result([Level_Id], [Code], [Name], [NextName], [Article], [Index], [ErrorDescription])
  SELECT *
  FROM [dbo].[KLADR::Search](@ParentLevel + 1, @ParentLevel, @ParentCode, @SOCR, CASE WHEN @ParentLevel + 1 < 6 THEN @@NAME ELSE @Name END, @Next1Name, @Next2Name, @Index)

  -- ѕоиск по номерам домов без улиц
  IF @@ROWCOUNT = 0 AND @ParentLevel < 5 BEGIN
    INSERT INTO @Result ([Level_Id], [Code], [Name], [NextName], [Article], [Index], [ErrorDescription])
    SELECT *
    FROM [dbo].[KLADR::Search](6, @ParentLevel, @ParentCode, NULL, @Name, @Next1Name, @Next2Name, NULL)
  END

  RETURN
END
GO


--SELECT * FROM [dbo].[KLADR::Search(Pre)](2, '6001400000000', N'ќстров г.', NULL, NULL, NULL)

--  SELECT *
--  FROM [dbo].[KLADR::Search](2 + 1, 2, '6001400000000', 'г', 'ќстров', 'дом 24', null)

--SELECT * FROM [KLADR:Base] WHERE CharIndex('город', NAME) > 0
--SELECT DISTINCT SOCR FROM [KLADR:Base] ORDER BY 1