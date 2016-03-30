-- Export.Execute.sql
SET NOCOUNT ON
SET ANSI_NULLS ON
SET QUOTED_IDENTIFIER ON
GO
EXEC [SQL].[Object Exists] @Object_Name = '[Export].[Execute]', @Type = 'P', @Force_Create = 1
GO
/*
    - NULL            - для получения общей информации
    - 'ONLINE'        - OnLine
    - 'ONTIME'        - OnTime
    - 'OFFLINE'       - OffLine

    - 'ACCESS'              - возвращает информацию о доступе пользователя к подписке или подписчику через RESULT_VALUE
    - 'OBJECTS'             - возвращает информацию о всех обьектах подписчика
    - 'TARGETS'             - возвращает информацию о всех целевых обьектах подписки
    - 'METADATA'            - возвращение информации о метаданных ('<PARAMS SubObject="<SubObject>"/>')

    - 'BEGIN'               - начало онлайновой синхронизации (локировка и подготовка актуальных данных
    - 'ROWS'                - возвращение идентификаторов актуальных записей онлайновой синхронизации
    - 'REFILL'              - перезаполнение буфера

    - 'DATASET',                  - возвращение информации о синхронизируемой(мых) записи(ях)
    - 'DATASET:STATIC'            - возвращение непериодической информации о синхронизируемой(мых) записи(ях)
    - 'DATASET:PERIODIC:RAW'      - возвращение периодической информации о синхронизируемой(мых) записи(ях) в формате FIELD;DATA;VALUE
    - 'DATASET:PERIODIC:PAGE'     - возвращение объединённой информации о синхронизируемой(мых) записи(ях) в формате исторической страницы
    - 'XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT'
                                  - возвращение информации о синхронизируемой(мых) записи(ях) в полноценном XML-Дереве

    - 'RESULTS'             - принятие или отмена синхронизируемой(мых) записи(ях) онлайновой синхронизации
    - 'END'                 - окончание онлайновой синхронизации ('<PARAMS Lock_Id="<Lock_Id>"/>')
*/
ALTER PROCEDURE [Export].[Execute]
  @SubScriber       SysName,
  @Object           SysName       = NULL,
  @Method           VarChar(20)   = NULL,
  @Action           VarChar(50),
  @Params           XML           = NULL,
  @Output           XML           = NULL OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @SQLDeclare         NVarChar(Max),
    @SQLInner           NVarChar(Max),
    @SQLWhere           NVarChar(Max),
    @SQL                NVarChar(Max),
    @SQLTabs            NVarChar(100),

    --@Function           SysName,
    @SysName            SysName,
    @Table_Id           Int,

    @SubScriber_Id      SmallInt,
    @SubScriber_Callers SysName,
    @Object_Id          SmallInt,
    @Enabled            VarChar(2),
    @SubScriberVerified Bit,

    @TimeOut            Int,
    @Target             SysName,
    @IdentityField      SysName,
    @ParentField        SysName,
    @UseView            Bit,
    @OnLineRoot         Bit               = 0,
    @View               SysName,
    @Translate          SysName,
    @Periodic           SysName,

    @Schema             SysName,
    @Name               SysName,
    @SystemObject_Id    Int,

    @Language           Char(2),

    @FieldsArray        NVarChar(Max),
    @FieldsExcluded     Bit,
    @FieldsXML          XML,
    @FieldsTranslate    NVarChar(Max),
    @FieldsPeriodic     NVarChar(Max),

    @FieldsRequired     NVarChar(Max),

    @Debug              Bit

  DECLARE
    @RunTime                DateTime,
    @RecieveTimeStamp       TTimeStamp,
    @LastRunTime            DateTime,
    @Procedure              SysName,
    @Properties             XML,
    @FiltersXML             XML,
    @@Action                VarChar(50)

  DECLARE
    @Lock_Object    SysName       = N'[Export].[Execute]',
    @Lock_Key       NVarChar(50),
    @Lock_LifeTime  Time          = '01:00:00',
    @Lock_Id        BigInt

  DECLARE @Fields [Export].[Fields]

  DECLARE @Filters TABLE
  (
    [Name]      SysName       NOT NULL PRIMARY KEY CLUSTERED,
    [Type]      SysName       NOT NULL,
    [NullAble]  Bit           NOT NULL,
    [ListAble]  Bit           NOT NULL,
    [Value]     SQL_Variant       NULL,
    [Values]    XML               NULL,
    [Index]     SmallInt      NOT NULL IDENTITY(1, 1)
  )

  DECLARE @FiltersPrepared [Export].[Filters]

  DECLARE @Where TABLE
  (
    [Name]      SysName       NOT NULL PRIMARY KEY CLUSTERED,
    [Value]     SQL_Variant       NULL,
    [Values]    XML               NULL
  )

  DECLARE @WaitingOnLine TABLE
  (
    [Identity]  Int                                   NOT NULL,
    [Action]    Char(1) COLLATE Cyrillic_General_BIN  NOT NULL,
    [TimeStamp] TTimeStamp                                NULL,
    [Params]    TParams                                   NULL,
    PRIMARY KEY CLUSTERED([Identity])
  )
  DECLARE @RowsOnLine [Export].[Rows:OnLine]

  DECLARE @WaitingOnTime TABLE
  (
    [Identity]  Int         NOT NULL,
    [DateTime]  DateTime    NOT NULL,
    [TimeStamp] TTimeStamp      NULL,
    PRIMARY KEY CLUSTERED([Identity], [DateTime])
  )
  DECLARE @RowsOnTime [Export].[Rows:OnTime]

  BEGIN TRY
    SET @Debug = Cast([System].[Session Variable]('DEBUG') AS Bit)

    IF @SubScriber IS NULL OR @Action IS NULL OR (@Object IS NULL AND @Action NOT IN ('ACCESS', 'OBJECTS')) OR (@Method IS NULL AND @Action NOT IN ('ACCESS', 'OBJECTS'))
      RaisError('Не указан один или несколько обязательных параметров процедуры: @SubScriber = %s, @Object = %s, @Method = %s, @Action = %s', 16, 1, @SubScriber, @Object, @Method, @Action)

    -- Идентификатор подписчика
    SELECT
      @SubScriber_Id      = [Id],
      @SubScriber_Callers = [Callers]
    FROM [Export].[SubScribers]
    WHERE [Name] = @SubScriber

    IF @@ROWCOUNT <> 1
      RaisError('Синхронизация отклонена - подписчик "%s" не зарегистрирован!', 16, 1, @SubScriber)

    -- Проверка доступа к подписчику
    EXECUTE AS CALLER
      IF USER_NAME() = 'dbo' OR [Pub].[In Array](@SubScriber_Callers, USER_NAME(), ',') = 1 OR IS_ROLEMEMBER(@SubScriber_Callers) = 1
        SET @SubScriberVerified = 1
      ELSE
        SET @SubScriberVerified = 0
    REVERT

    IF @Action = 'ACCESS' AND (@Object IS NULL OR @SubScriberVerified = 0)
      RETURN @SubScriberVerified
    ELSE IF @SubScriberVerified = 0
      RaisError('Синхронизация отклонена, нет доступа к подписчику %s!', 16, 1, @SubScriber)

    IF @Action = 'OBJECTS' BEGIN
      IF @Method IS NULL
        SELECT
          [Object]  = O.[Name],
          [OffLine] = SS.[OffLine:Enabled],
          [OnLine]  = SS.[OnLine:Enabled],
          [OnTime]  = SS.[OnTime:Enabled]
        FROM [Export].[SubScriptions] SS
        INNER JOIN [Export].[Objects] O ON SS.[Object_Id] = O.[Id] AND (@Object IS NULL OR @Object = O.[Name])
        WHERE SS.[SubScriber_Id] = @SubScriber_Id
      ELSE
        SELECT
          [Object]  = O.[Name]
        FROM [Export].[SubScriptions] SS
        INNER JOIN [Export].[Objects] O ON SS.[Object_Id] = O.[Id] AND (@Object IS NULL OR @Object = O.[Name])
        WHERE SS.[SubScriber_Id] = @SubScriber_Id
          AND CASE
                WHEN @Method = 'OFFLINE'  THEN SS.[OffLine:Enabled]
                WHEN @Method = 'ONLINE'   THEN SS.[OnLine:Enabled]
                WHEN @Method = 'ONTIME' AND SS.[OnTime:Enabled] IS NOT NULL THEN CAST(1 AS Bit)
              END = 1

      RETURN @@ROWCOUNT
    END

    -- Идентификатор объекта
    SELECT
      @Object_Id = [Id]
    FROM [Export].[Objects]
    WHERE [Name] = @Object

    IF @@ROWCOUNT <> 1
      RaisError('Синхронизация отклонена, объект [Name] = "%s" не экспортируется!', 16, 1, @Object)

    IF @Method NOT IN
    (
      'ONLINE', 'ONTIME', 'OFFLINE'
    )
      RaisError('Неизвестный метод экспорта @Method = %s!', 16, 1, @Method)

    IF @Action NOT IN
    (
      'ACCESS', 'OBJECTS',
      'TARGETS', 'METADATA',
      'BEGIN', 'ROWS', 'REFILL',
      'DATASET', 'DATASET:STATIC', 'DATASET:PERIODIC:RAW', 'DATASET:PERIODIC:PAGE',
      'XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT',
      'RESULTS', 'END'
    )
      RaisError('Неизвестное действие @Action = %s!', 16, 1, @Action)

    SET @RunTime = GetDate()

    SELECT
      --@Procedure          = CASE @Method WHEN 'ONLINE' THEN [OnLine:Procedure] WHEN 'ONTIME' THEN [OnTime:Procedure] WHEN 'OFFLINE' THEN [OffLine:Procedure] END,
      @Properties         = CASE @Method WHEN 'ONLINE' THEN [OnLine:Properties] WHEN 'ONTIME' THEN [OnTime:Properties] WHEN 'OFFLINE' THEN [OffLine:Properties] END,
      @RecieveTimeStamp   = CASE @Method WHEN 'ONLINE' THEN [OnLine:Recieve:TimeStamp] WHEN 'ONTIME' THEN [OnTime:Recieve:TimeStamp] END,
      @LastRunTime        = CASE @Method WHEN 'ONLINE' THEN [OnLine:LastRun:Time] WHEN 'ONTIME' THEN [OnTime:LastRun:Time] WHEN 'OFFLINE' THEN [OffLine:LastRun:Time] END,
      @Enabled            = CASE @Method WHEN 'ONLINE' THEN Cast([OnLine:Enabled] AS VarChar(2)) WHEN 'ONTIME' THEN [OnTime:Enabled] WHEN 'OFFLINE' THEN Cast([OffLine:Enabled] AS VarChar(2)) END
    FROM [Export].[SubScriptions]
    WHERE [Object_Id] = @Object_Id AND [SubScriber_Id] = @SubScriber_Id

    IF @@ROWCOUNT <> 1
      RaisError ('Подписка «%s»:«%s» не существует!', 16, 1, @SubScriber, @Object)

    IF @Params IS NOT NULL
      SET @Target = @Params.value('(/PARAMS/PARAM[@NAME="Target"]/@VALUE)[1]', 'SysName')

    IF @Action = 'ACCESS' AND (@Target IS NULL OR @Enabled = '0')
      RETURN CASE WHEN @Enabled <> '0' THEN 1 ELSE 0 END
    ELSE IF @Enabled IS NULL OR @Enabled IN ('', '0')
      RaisError ('Метод «%s» в подписке «%s»:«%s» не доступен!', 16, 1, @Method, @SubScriber, @Object)

    IF @Method NOT IN ('ONLINE', 'ONTIME') AND @Action IN ('BEGIN', 'ROWS', 'REFILL', 'RESULTS', 'END')
      RaisError('Действите «%s» возможно только в OnLine или OnTime экспорте!', 16, 1, @Action)

    IF @Target IS NOT NULL BEGIN
      IF @Action NOT IN ('ACCESS', 'METADATA', 'DATASET', 'DATASET:STATIC', 'DATASET:PERIODIC:RAW', 'DATASET:PERIODIC:PAGE', 'XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT')
        RaisError('Действите «%s» в подписке «%s»:«%s» не поддерживает параметр «Target»', 16, 1, @Action, @SubScriber, @Object)
      SET @Params.modify('delete /PARAMS/PARAM[@NAME="Target"]')
    END ELSE IF @Method = 'ONLINE'
      SET @OnLineRoot = 1

    IF @Params IS NOT NULL
      SET @FieldsRequired = @Params.value('(/PARAMS/PARAM[@NAME="Fields"]/@VALUE)[1]', 'NVarChar(Max)')

    IF @FieldsRequired IS NOT NULL BEGIN
      IF @Action NOT IN ('DATASET', 'DATASET:STATIC', 'DATASET:PERIODIC:RAW', 'DATASET:PERIODIC:PAGE', 'XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT')
        RaisError('Действите «%s» в подписке «%s»:«%s» не поддерживает параметр «Fields»', 16, 1, @Action, @SubScriber, @Object)
      SET @Params.modify('delete /PARAMS/PARAM[@NAME="Fields"]')
    END

    IF @Action IN ('ACCESS', 'METADATA', 'BEGIN', 'ROWS', 'REFILL', 'DATASET', 'DATASET:STATIC', 'DATASET:PERIODIC:RAW', 'DATASET:PERIODIC:PAGE', 'XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT') BEGIN
      IF @Target IS NULL
        SET @Target = @Object
      EXEC [SQL].[Extract Object Path] @Path = @Target, @Schema = @Schema OUT, @Name = @Name OUT

      SELECT
        @IdentityField  = V.[IdentityField],
        @ParentField    = IsNull(I.node.value('@PARENT', 'SysName'), @IdentityField),

        @Procedure      = V.[Procedure],
        @UseView        = CASE WHEN V.[View] IS NOT NULL THEN 1 ELSE 0 END,
        @View           = IsNull(V.[View], @Target),
        @Translate      = CASE WHEN V.[Translate] IS NOT NULL THEN V.[Translate] WHEN TT.[Id] IS NOT NULL THEN QuoteName(@Schema) + N'.' + QuoteName(@Name + N'@Translate') END,
        @Periodic       = CASE WHEN V.[Periodic] IS NOT NULL THEN V.[Periodic] WHEN PT.[Id] IS NOT NULL THEN QuoteName(@Schema) + N'.' + QuoteName(@Name + N'@Periodic') END,

        @FieldsArray    = NullIf(I.node.value('@FIELDS', 'NVarChar(Max)'), N'*'),
        @FieldsXML      = CASE WHEN I.node.exist('FIELD') = 1 THEN I.node.query('FIELD') END,
        @FiltersXML     = CASE WHEN I.node.exist('FILTER') = 1 THEN I.node.query('FILTER') END
      FROM @Properties.nodes('/TARGET[@NAME=sql:variable("@Target")]') I (Node)
      CROSS APPLY
      (
        SELECT
          [IdentityField] = I.node.value('@IDENTITY', 'SysName'),
          [Translate]     = I.node.value('@TRANSLATE', 'SysName'),
          [Periodic]      = I.node.value('@PERIODIC', 'SysName'),
          [View]          = CASE WHEN @Method = 'ONTIME' THEN I.node.value('@ROOT', 'SysName') ELSE I.node.value('@VIEW', 'SysName') END,
          [Procedure]     = I.node.value('@PROCEDURE', 'SysName')
      ) V
      LEFT JOIN [System].[Translate->Tables$] TT ON V.[Translate] IS NULL AND @Schema = TT.[Schema] AND @Name = TT.[Name]
      LEFT JOIN [System].[Periodic->Tables$] PT ON V.[Periodic] IS NULL AND @Schema = PT.[Schema] AND @Name = PT.[Name]

      IF @@ROWCOUNT <> 1 BEGIN
        IF @Action = 'ACCESS'
          RETURN 0
        RaisError('Отсутствует описание объекта «%s» в подписке «%s»:«%s»', 16, 1, @Target, @SubScriber, @Object)
      END ELSE IF @Action = 'ACCESS'
        RETURN 1

      IF @Periodic IS NULL AND @Action IN ('DATASET:STATIC', 'DATASET:PERIODIC:RAW', 'DATASET:PERIODIC:PAGE')
        RaisError('Действите «%s» не актуально в подписке «%s»:«%s» по отношению к объекту «%s»', 16, 1, @Action, @SubScriber, @Object, @Target)

      IF @Procedure IS NULL AND OBJECT_ID(@View) IS NULL
        RaisError('Действите по-умолчанию «%s» в подписке «%s»:«%s» по отношению к объекту «%s» невозможно - не найден объект «%s»', 16, 1, @Action, @SubScriber, @Object, @Target, @View)

      IF @UseView = 0 AND @Method = 'ONTIME' AND @FiltersXML IS NOT NULL
        RaisError('В настройках подписки «%s»:«%s» отсутствует атрибут «ROOT»', 16, 1, @SubScriber, @Object)

      IF @IdentityField IS NULL AND (@Method = 'ONLINE' OR @Method = 'ONTIME' AND @UseView = 1)
        IF @Target = @Object
          RaisError('В настройках подписки «%s»:«%s» отсутствует атрибут «IDENTITY»', 16, 1, @SubScriber, @Object)
        ELSE IF @ParentField IS NULL
          RaisError('В настройках подписки «%s»:«%s»:«%s» отсутствуют атрибуты «IDENTITY» и «PARENT»', 16, 1, @SubScriber, @Object, @Target)

      IF @FiltersXML IS NOT NULL BEGIN
        INSERT INTO @Filters([Name], [Type], [NullAble], [ListAble], [Value], [Values])
        SELECT
          [Name]      = V.[Name],
          [Type]      = V.[Type],
          [NullAble]  = IsNull(V.[NullAble], 1),
          [ListAble]  = IsNull(V.[ListAble], CASE WHEN @Procedure IS NULL THEN 1 ELSE 0 END),
          [Value]     = [Pub].[VarChar To SQL_Variant](V.[Value], V.[Type]),
          [Values]    = CASE WHEN V.[ListExist] = 1 THEN I.node.query('LIST') END
        FROM @FiltersXML.nodes('/FILTER') I(Node)
        CROSS APPLY
        (
          VALUES
          (
            I.node.value('@NAME', 'SysName'),
            I.node.value('@TYPE', 'SysName'),
            I.node.value('@NULLABLE', 'Bit'),
            I.node.value('@LIST', 'Bit'),
            I.node.value('@VALUE', 'NVarChar(Max)'),
            I.node.exist('LIST')
          )
        ) V([Name], [Type], [NullAble], [ListAble], [Value], [ListExist])
      END
    END

    IF @Action IN ('METADATA', 'ROWS', 'REFILL', 'DATASET', 'DATASET:STATIC', 'DATASET:PERIODIC:RAW', 'DATASET:PERIODIC:PAGE', 'XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT') BEGIN
      --IF @FieldsXML IS NOT NULL AND @FieldsArray IS NOT NULL
      --  RaisError('В описание объекта «%s» в подписке «%s»:«%s» допущено двоякое толкование', 16, 1, @Target, @SubScriber, @Object)

      IF @FieldsArray IS NOT NULL AND LEFT(@FieldsArray, 1) = '-' BEGIN
        SET @FieldsExcluded = 1
        SET @FieldsArray = SubString(@FieldsArray, 2, Len(@FieldsArray) - 1)
      END ELSE
        SET @FieldsExcluded = 0

      IF @FieldsRequired IS NOT NULL BEGIN
        IF @FieldsArray IS NULL
          SET @FieldsArray = @FieldsRequired
        ELSE IF @FieldsExcluded = 1 BEGIN
          SET @FieldsArray = [Pub].[Arrays Join](@FieldsRequired, @FieldsArray, ',')
          IF @FieldsArray IS NOT NULL
            RaisError('Поле(я) «%s» исключено(ы) из списка экспортируемых', 16, 1, @FieldsArray)
          SET @FieldsArray = @FieldsRequired
          SET @FieldsExcluded = 0
        END ELSE BEGIN
          SET @FieldsArray = [Pub].[Arrays Anti Join](@FieldsRequired, @FieldsArray, ',')
          IF @FieldsArray IS NOT NULL
            RaisError('Поле(я) «%s» не включено(ы) в список экспортируемых', 16, 1, @FieldsArray)
          SET @FieldsArray = @FieldsRequired
        END
      END

      IF @FieldsXML IS NULL BEGIN
        SET @SystemObject_Id = OBJECT_ID(@View)
        IF @SystemObject_Id IS NULL
          RaisError('В свойствах подписки «%s»:«%s» отсутствует описание полей объекта «%s»', 16, 1, @SubScriber, @Object, @Target)
      END

      IF @FieldsXML IS NULL BEGIN
        INSERT INTO @Fields([Index], [Name], [Type], [Clr], [Collation], [Required], [Filter], [Translate], [Periodic], [Origin:Schema], [Origin:Translate:Field_Id], [Origin:Periodic:Field_Id])
        SELECT
          [Index]                     = ROW_NUMBER() OVER (ORDER BY CASE WHEN @FieldsArray IS NULL OR @FieldsExcluded = 1 THEN SF.[column_id] ELSE FF.[Index] END),
          [Name]                      = SF.[name],
          [Type]                      = [SQL].[Field Type Compile](SF.[type_name], SF.[max_length], SF.[precision], SF.[scale], NULL),
          [Clr]                       = SF.[Clr],
          [Collation]                 = SF.[collation_name],
          [Required]                  = 0,
          [Filter]                    = CASE FL.[NullAble] WHEN 1 THEN '?' WHEN 0 THEN 'Y' ELSE 'N' END,
          [Translate]                 = Cast(CASE WHEN FT.[Id] IS NOT NULL THEN 1 ELSE 0 END AS Bit),
          [Periodic]                  = Cast(CASE WHEN FP.[Id] IS NOT NULL THEN 1 ELSE 0 END AS Bit),
          [Origin:Schema]             = @Schema,
          [Origin:Translate:Field_Id] = FT.[Id],
          [Origin:Periodic:Field_Id]  = FP.[Id]
        FROM
        (
          SELECT
            SF.[column_id],
            SF.[name],
            [type_name] = CASE WHEN UT.[is_assembly_type] = 1 THEN N'VarBinary' ELSE IsNull(ST.[name], UT.[name]) END,
            [Clr] = UT.[is_assembly_type],
            SF.[max_length],
            SF.[precision],
            SF.[scale],
            SF.[collation_name]
          FROM [sys].[columns] SF WITH (NOLOCK)
          INNER JOIN [sys].[types] UT WITH (NOLOCK) ON SF.[user_type_id] = UT.[user_type_id]
          LEFT JOIN [sys].[types] ST WITH (NOLOCK) ON UT.[is_user_defined] = 1 AND SF.[system_type_id] = ST.[user_type_id]
          WHERE SF.[object_id] = @SystemObject_Id
        ) SF
        FULL OUTER JOIN [Pub].[Array To RowSet Of Values](@FieldsArray, ',') FF ON SF.[name] = FF.[Value]
        LEFT JOIN @Filters FL ON SF.[name] = FL.[Name]
        LEFT JOIN [System].[Translate->Fields] FT ON @Translate IS NOT NULL AND FT.[Schema] = @Schema AND FT.[Table_Name] = @Name AND FT.[Name] = SF.[name]
        LEFT JOIN [System].[Periodic->Fields] FP ON @Periodic IS NOT NULL AND FP.[Schema] = @Schema AND FP.[Table_Name] = @Name AND FP.[Name] = SF.[name]
        WHERE [System].[Raise Error]
              (
                @@PROCID,
                CASE
                  WHEN @FieldsArray IS NOT NULL AND SF.[name] IS NULL THEN 'Неверное имя поля «' + FF.[Value] + N'»'
                END
              ) IS NULL
              AND
              (
                @FieldsExcluded = 0 AND (@FieldsArray IS NULL OR FF.[Value] IS NOT NULL)
                OR
                @FieldsExcluded = 1 AND FF.[Value] IS NULL
              )
      END ELSE BEGIN
        INSERT INTO @Fields([Index], [Name], [Type], [Clr], [Collation], [Required], [Filter], [Translate], [Periodic], [Origin:Schema], [Origin:Translate:Field_Id], [Origin:Periodic:Field_Id])
        SELECT
          [Index]                     = ROW_NUMBER() OVER (ORDER BY CASE WHEN @FieldsArray IS NULL OR @FieldsExcluded = 1 THEN O.[Id] ELSE FF.[Index] END),
          [Name]                      = O.[Name],
          [Type]                      = O.[Type],
          [Clr]                       = O.[Clr],
          [Collation]                 = O.[Collation],
          [Required]                  = O.[Required],
          [Filter]                    = CASE FL.[NullAble] WHEN 1 THEN '?' WHEN 0 THEN 'Y' ELSE 'N' END,
          [Translate]                 = CASE WHEN O.[Translate] = 1 OR O.[Origin:Translate:Field_Id] IS NOT NULL THEN 1 ELSE 0 END,
          [Periodic]                  = CASE WHEN O.[Periodic] = 1 OR O.[Origin:Periodic:Field_Id] IS NOT NULL THEN 1 ELSE 0 END,
          [Origin:Schema]             = O.[Schema],
          [Origin:Translate:Field_Id] = O.[Origin:Translate:Field_Id],
          [Origin:Periodic:Field_Id]  = O.[Origin:Periodic:Field_Id]
        FROM @FieldsXML.nodes('/FIELD') I (Node)
        OUTER APPLY
        (
          SELECT
            [Id]                        = O.[Id],
            [Name]                      = O.[Name],
            [Type]                      = CASE WHEN O.[Type] IS NOT NULL AND (@Procedure IS NOT NULL OR SC.[column_id] IS NULL) THEN O.[Type] WHEN SC.[column_id] IS NOT NULL THEN [SQL].[Field Type Compile](SC.[system_type_id], SC.[max_length], SC.[precision], SC.[scale], Default) END,
            [Clr]                       = IsNull(O.[Clr], 0),
            [Collation]                 = CASE WHEN O.[Collation] IS NOT NULL AND (@Procedure IS NOT NULL OR SC.[column_id] IS NULL) THEN O.[Collation] ELSE SC.[collation_name] END,
            [Required]                  = O.[Required],
            [Translate]                 = O.[Translate],
            [Periodic]                  = O.[Periodic],
            [Schema]                    = O.[Schema],
            [Table]                     = O.[Table],
            [Field]                     = O.[Field],
            [Origin:Translate:Field_Id] = CASE WHEN @Translate IS NOT NULL AND @Procedure IS NULL THEN [System].[Translate->Field@Id](O.[Schema], O.[Table], O.[Field]) END,
            [Origin:Periodic:Field_Id]  = CASE WHEN @Periodic IS NOT NULL AND @Procedure IS NULL THEN [System].[Periodic->Field@Id](O.[Schema], O.[Table], O.[Field]) END
          FROM
          (
            SELECT
              [Id]        = O.[Id],
              [Name]      = O.[Name],
              [Type]      = O.[Type],
              [Clr]       = O.[Clr],
              [Collation] = O.[Collation],
              [Required]  = O.[Required],
              [Translate] = O.[Translate],
              [Periodic]  = O.[Periodic],
              [Schema]    = CASE WHEN O.[Origin] IS NULL THEN @Schema ELSE PARSENAME(O.[Origin], 3) END,
              [Table]     = CASE WHEN O.[Origin] IS NULL THEN @Name ELSE PARSENAME(O.[Origin], 2) END,
              [Field]     = CASE WHEN O.[Origin] IS NULL THEN @Schema ELSE PARSENAME(O.[Origin], 1) END
            FROM
            (
              SELECT
                [Id]        = ROW_NUMBER() OVER (ORDER BY I.node.value('@ID', 'SmallInt')),
                [Name]      = I.node.value('@NAME', 'SysName'),
                [Type]      = I.node.value('@TYPE', 'SysName'),
                [Clr]       = I.node.value('@CLR', 'Bit'),
                [Collation] = I.node.value('@COLLATION', 'SysName'),
                [Required]  = IsNull(I.node.value('@REQUIRED', 'Bit'), 0),
                [Translate] = Cast(CASE WHEN @Translate IS NOT NULL THEN I.node.value('@TRANSLATE', 'Bit') ELSE 0 END AS Bit),
                [Periodic]  = Cast(CASE WHEN @Periodic IS NOT NULL THEN I.node.value('@PERIODIC', 'Bit') ELSE 0 END AS Bit),
                [Origin]    = I.node.value('@ORIGIN', 'SysName')
            ) O
          ) O
          LEFT JOIN [sys].[schemas] SS WITH (NOLOCK) ON (O.[Type] IS NULL OR O.[Translate] IS NULL OR O.[Periodic] IS NULL) AND O.[Schema] = SS.[name]
          LEFT JOIN [sys].[objects] SO WITH (NOLOCK) ON SS.[schema_id] IS NOT NULL AND SS.[schema_id] = SO.[schema_id] AND O.[Table] = SO.[name]
          LEFT JOIN [sys].[columns] SC WITH (NOLOCK) ON SO.[object_id] IS NOT NULL AND SO.[object_id] = SC.[object_id] AND O.[Field] = SC.[name]
        ) O
        FULL OUTER JOIN [Pub].[Array To RowSet Of Values](@FieldsArray, ',') FF ON O.[Name] = FF.[Value]
        LEFT JOIN @Filters FL ON O.[Name] = FL.[Name]
        WHERE [System].[Raise Error]
              (
                @@PROCID,
                CASE
                  WHEN @FieldsArray IS NOT NULL AND O.[Name] IS NULL THEN 'Неверное имя поля «' + FF.[Value] + N'»'
                  WHEN O.[Required] = 1 AND FF.[Value] IS NULL THEN 'Имя поля «' + O.[Name] + N'» маркировано как обязательное к возврату'
                  WHEN @Procedure IS NULL AND O.[Translate] = 1 AND O.[Origin:Translate:Field_Id] IS NULL THEN N'Автоматический перевод поля «' + O.[Name] + N'» невозможен - не найдено соответствие в системе.'
                  WHEN @Procedure IS NULL AND O.[Periodic] = 1 AND O.[Origin:Periodic:Field_Id] IS NULL THEN N'Автоматический анализ периодики поля «' + O.[Name] + N'» невозможен - не найдено соответствие в системе.'
                END
              ) IS NULL
              AND
              (
                @FieldsExcluded = 0 AND (@FieldsArray IS NULL OR FF.[Value] IS NOT NULL)
                OR
                @FieldsExcluded = 1 AND FF.[Value] IS NULL
              )
      END

      SELECT
        @FieldsTranslate = [Pub].[Concat](CASE WHEN [Translate] = 1 THEN [Name] END, N','),
        @FieldsPeriodic  = [Pub].[Concat](CASE WHEN [Periodic] = 1 THEN [Name] END, N',')
      FROM @Fields

      IF @Action IN ('ROWS', 'REFILL', 'DATASET', 'DATASET:STATIC', 'DATASET:PERIODIC:RAW', 'DATASET:PERIODIC:PAGE', 'XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT') BEGIN
        INSERT INTO @Where([Name], [Value], [Values])
        SELECT
          [Name], [Value], [Values]
        FROM
        (
          SELECT
            [Name]      = W.[Name],
            [NullAble]  = F.[NullAble],
            [Value]     = [Pub].[VarChar To SQL_Variant](I.node.value('@VALUE', 'NVarChar(Max)'), F.[Type]),
            [Values]    = CASE WHEN W.[ListExist] = 1 THEN I.node.query('LIST') END
          FROM @Params.nodes('/FILTERS/FILTER') I(Node)
          CROSS APPLY
          (
            VALUES
            (
              I.node.value('@NAME', 'SysName'),
              I.node.exist('LIST')
            )
          ) W([Name], [ListExist])
          FULL OUTER JOIN @Filters F ON W.[Name] = F.[Name]
          WHERE [System].[Raise Error]
                (
                  @@PROCID,
                  CASE
                    WHEN F.[Type] IS NULL THEN N'Параметр фильтра ' + IsNull(N'«' + W.[Name] + N'»', N'Null') + N' не поддерживается данным объектом экспорта.'
                    WHEN W.[Name] IS NULL AND F.[NullAble] = 0 THEN N'Параметр фильтра «' + F.[Name] + N'» является обязательным для данного объекта экспорта.'
                    WHEN F.[ListAble] = 0 AND W.[ListExist] = 1 THEN N'Параметр фильтра «' + F.[Name] + N'» не допускает перечисляемого множества.'
                  END
                ) IS NULL
        ) I
        WHERE I.[Name] IS NOT NULL OR I.[NullAble] = 0

        INSERT INTO @FiltersPrepared([Index], [Name], [Value], [Values])
        SELECT
          [Index]   = ROW_NUMBER() OVER(ORDER BY F.[Name]),
          [Name]    = F.[Name],
          [Value]   = CASE WHEN COUNT(1) = 1 THEN MAX(IsNull(W.[Value], F.[Value])) END,
          [Values]  = CASE
                        WHEN COUNT(1) > 1 THEN
                          (
                            SELECT
                              [NAME] = F.[Name],
                              Cast
                              (
                                [Pub].[ConCat]
                                (
                                  N'<LIST ' + [Pub].[XML::Attribute::Create](N'VALUE', IsNull(W.[Value], F.[Value])) + N'/>',
                                  N''
                                )
                                AS XML
                              )
                            FOR XML RAW('FILTER')
                          )
                      END
        FROM
        (
          SELECT
            [Name]      = F.[Name],
            [Type]      = F.[Type],
            [Value]     = IsNull(L.[Value], F.[Value]),
            [Where]     = CAST(CASE WHEN WF.[Name] IS NOT NULL THEN 1 ELSE 0 END AS Bit)
          FROM @Filters F
          LEFT JOIN @Where WF ON F.[Name] = WF.[Name]
          OUTER APPLY
          (
            SELECT
              [Value] = [Pub].[VarChar To SQL_Variant](I.node.value('@VALUE', 'NVarChar(Max)'), F.[Type])
            FROM F.[Values].nodes('/LIST') I (Node)
            WHERE F.[Values] IS NOT NULL
                AND [System].[Raise Error]
                    (
                      @@PROCID,
                      CASE
                        WHEN I.node.exist('@VALUE') = 0 THEN 'Значение фильтра «' + F.[Name] + N'» не указано в настройках объекта.'
                      END
                    ) IS NULL
          ) L
          WHERE F.[Values] IS NOT NULL OR F.[Value] IS NOT NULL OR WF.[Name] IS NOT NULL
        ) F
        FULL OUTER JOIN
        (
          SELECT
            [Name]      = W.[Name],
            [Value]     = W.[Value]
          FROM
          (
            SELECT
              [Name]      = W.[Name],
              [Value]     = IsNull(L.[Value], W.[Value])
            FROM @Where W
            INNER JOIN @Filters F ON W.[Name] = F.[Name]
            OUTER APPLY
            (
              SELECT
                [Value] = [Pub].[VarChar To SQL_Variant](I.node.value('@VALUE', 'NVarChar(Max)'), F.[Type])
              FROM W.[Values].nodes('/LIST') I (Node)
              WHERE W.[Values] IS NOT NULL
            ) L
            WHERE [System].[Raise Error]
                  (
                    @@PROCID,
                    CASE
                      WHEN L.[Value] IS NULL AND W.[Value] IS NULL THEN N'Пустое значение параметра фильтра «' + W.[Name] + N'» недопустимо.'
                    END
                  ) IS NULL
          ) W
          --FULL OUTER JOIN @Filters F ON W.[Name] = F.[Name]
          --WHERE (W.[Name] IS NOT NULL OR F.[NullAble] = 0)
        ) W ON F.[Name] = W.[Name] AND (F.[Value] IS NULL OR [Pub].[Is Equal Variants](F.[Value], W.[Value]) = 1)
        WHERE [System].[Raise Error]
              (
                @@PROCID,
                CASE
                  WHEN F.[Type] IS NULL THEN 'Значение параметр фильтра ' + QuoteName(W.[Name]) + N'=«' + Cast(W.[Value] AS NVarChar(Max)) + N'» недопустимо для данного объекта экспорта.'
                END
              ) IS NULL
              AND (F.[Where] = 0 OR W.[Name] IS NOT NULL)
        GROUP BY F.[Name]

        SET @FiltersXML = (SELECT (SELECT [Values]) FROM @FiltersPrepared WHERE [Values] IS NOT NULL FOR XML PATH(''), TYPE)
        IF @Params IS NOT NULL
          SET @Params.modify('delete FILTERS')

        SET @SQLTabs = CASE
                        WHEN @Action IN ('DATASET:PERIODIC:RAW', 'DATASET:PERIODIC:PAGE') THEN N'  '
                        WHEN @Action IN ('XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT') THEN N'        '
                        ELSE N''
                       END
        SELECT
          @SQLDeclare = IsNull
                        (
                          N'DECLARE'
                          +
                          [Pub].[ConCat]
                          (
                            N'
  @'
                            + FP.[Name] + Replicate(' ', [Pub].[Is Negative Int](16 - Len(FP.[Name]), 0)) + N' '
                            +
                            CASE
                              WHEN FP.[Values] IS NOT NULL THEN N'XML = @Filters.query(''/FILTER[@NAME="' + FP.[Name] + N'"]/LIST'')'
                              ELSE F.[Type] + N' = ' + [SQL].[SQL_Variant As Text](FP.[Value])
                              -- ELSE F.[Type] + N' = @Filters.value(''(FILTER[@NAME="' + FP.[Name] + N'"]/@VALUE)[1]'', ' + [Pub].[Quote String](F.[Type]) + N')'
                            END,
                            N','
                          ) + N'

',
                          N''
                        ),
          @SQLInner   = IsNull
                        (
                          [Pub].[ConCat]
                          (
                            CASE
                              WHEN FP.[Values] IS NOT NULL THEN N'
INNER JOIN
(
  SELECT
    ' + QuoteName(FP.[Name]) + N' = I.Node.value(''@VALUE'', ' + [Pub].[Quote String](F.[Type]) + N')
  FROM @' + FP.[Name] + N'.nodes(''/LIST'') AS I (Node)
) F' + Cast(FP.[Index] AS NVarChar) + N' ON '
                                +
                                CASE
                                  WHEN @Action IN ('ROWS') THEN N'CASE WHEN W.[Action] = ''D'' THEN W.[Params].' + [SQL].[TParams::Method By Type]([Type]) + N'(' + [Pub].[Quote String](FP.[Name]) + N') ELSE T.' + QuoteName(FP.[Name]) + N' END'
                                  ELSE N'T.' + QuoteName(FP.[Name])
                                END
                                + N' = F' + Cast(FP.[Index] AS NVarChar) + N'.' + QuoteName(FP.[Name])
                            END,
                            N''
                          ),
                          N''
                        ),
          @SQLWhere   = [Pub].[ConCat]
                        (
                          CASE
                            WHEN FP.[Value] IS NOT NULL THEN
                              CASE
                                WHEN @Method = 'ONLINE' AND @Action IN ('ROWS', 'XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT') THEN N'CASE WHEN W.[Action] = ''D'' THEN W.[Params].' + [SQL].[TParams::Method By Type]([Type]) + N'(' + [Pub].[Quote String](FP.[Name]) + N') ELSE T.' + QuoteName(FP.[Name]) + N' END'
                                ELSE N'T.' + QuoteName(FP.[Name])
                              END
                              + N' = @' + FP.[Name]
                          END,
                          N'
' + @SQLTabs + N'      AND '
                        )
        FROM @FiltersPrepared FP
        INNER JOIN @Filters F ON FP.[Name] = F.[Name]
      END ELSE IF @Params.exist('/FILTERS/FILTER') = 1
        RaisError('Действите «%s» в подписке «%s»:«%s» не поддерживает фильтрацию', 16, 1, @Action, @SubScriber, @Object)

      SET @Properties.modify('delete TARGET/FILTER')

      SET @Language = @Params.value('(/PARAMS/PARAM[@NAME="Language"]/@VALUE)[1]', 'Char(2)')
      IF @Translate IS NOT NULL AND @Action IN ('DATASET', 'DATASET:STATIC', 'DATASET:PERIODIC:RAW', 'DATASET:PERIODIC:PAGE') BEGIN
        --IF @Language IS NULL
        --  RaisError('Действите «%s» в подписке «%s»:«%s» требует параметр «Language» для объекта «%s»', 16, 1, @Action, @SubScriber, @Object, @Target)
        IF @IdentityField IS NULL AND @UseView = 1
          RaisError('В подписке «%s»:«%s» для работы системы перевода объекта «%s» требуется атрибут «IDENTITY»', 16, 1, @SubScriber, @Object, @Target)
        IF @Procedure IS NULL AND @Params IS NOT NULL
          SET @Params.modify('delete /PARAMS/PARAM[@NAME="Language"]')
      END ELSE IF @Language IS NOT NULL
        RaisError('Действите «%s» в подписке «%s»:«%s» не поддерживает параметр «Language» для объекта «%s»', 16, 1, @Action, @SubScriber, @Object, @Target)
    END

    SET @Lock_Key = Cast(@SubScriber_Id AS NVarChar(25)) + N':' + Cast(@Object_Id AS NVarChar(25))

    --------------------------------- TARGETS ----------------------------
    IF @Action = 'TARGETS' BEGIN
      IF @Method IN ('OFFLINE', 'ONTIME')
        SELECT
          [Name]      = V.[Name],
          [Identity]  = I.node.value('@IDENTITY', 'SysName'),
          [Translate] = CASE WHEN V.[Translate] IS NOT NULL OR TT.[Id] IS NOT NULL THEN 1 ELSE 0 END,
          [Periodic]  = CASE WHEN V.[Periodic] IS NOT NULL OR PT.[Id] IS NOT NULL THEN 1 ELSE 0 END
        FROM @Properties.nodes('/TARGET') I (Node)
        CROSS APPLY
        (
          SELECT
            [Name]      = I.node.value('@NAME', 'SysName'),
            [Translate] = I.node.value('@TRANSLATE', 'SysName'),
            [Periodic]  = I.node.value('@PERIODIC', 'SysName')
        ) V
        LEFT JOIN [System].[Translate->Tables$] TT ON V.[Translate] IS NULL AND PARSENAME(V.[Name], 2) = TT.[Schema] AND PARSENAME(V.[Name], 1) = TT.[Name]
        LEFT JOIN [System].[Periodic->Tables$] PT ON V.[Periodic] IS NULL AND PARSENAME(V.[Name], 2) = PT.[Schema] AND PARSENAME(V.[Name], 1) = PT.[Name]
      ELSE IF @Method = 'ONLINE'
        SELECT
          [Name]      = V.[Name],
          [Identity]  = I.node.value('@IDENTITY', 'SysName'),
          [Parent]    = IsNull(I.node.value('@PARENT', 'SysName'), I.node.value('@IDENTITY', 'SysName')),
          [Translate] = CASE WHEN V.[Translate] IS NOT NULL OR TT.[Id] IS NOT NULL THEN 1 ELSE 0 END,
          [Periodic]  = CASE WHEN V.[Periodic] IS NOT NULL OR PT.[Id] IS NOT NULL THEN 1 ELSE 0 END
        FROM @Properties.nodes('/TARGET') I (Node)
        CROSS APPLY
        (
          SELECT
            [Name]      = I.node.value('@NAME', 'SysName'),
            [Translate] = I.node.value('@TRANSLATE', 'SysName'),
            [Periodic]  = I.node.value('@PERIODIC', 'SysName')
        ) V
        LEFT JOIN [System].[Translate->Tables$] TT ON V.[Translate] IS NULL AND PARSENAME(V.[Name], 2) = TT.[Schema] AND PARSENAME(V.[Name], 1) = TT.[Name]
        LEFT JOIN [System].[Periodic->Tables$] PT ON V.[Periodic] IS NULL AND PARSENAME(V.[Name], 2) = PT.[Schema] AND PARSENAME(V.[Name], 1) = PT.[Name]
    END
    ------------------------------------------ METADATA ---------------
    ELSE IF @Action = 'METADATA' BEGIN
      SELECT
        [Name],
        [Type],
        [Collation],
        [Filter],
        [Translate],
        [Periodic]
      FROM @Fields
      ORDER BY [Index]
    END
    -------------------------------- BEGIN ----------------------------
    ELSE IF @Action IN ('BEGIN') BEGIN
      SET @Lock_LifeTime = IsNull(@Params.value('(/PARAMS/PARAM[@NAME="LifeTime"]/@VALUE)[1]', 'Time'), @Lock_LifeTime)
      EXEC [System].[Lock]
        @Id         = @Lock_Id OUT,
        @Object     = @Lock_Object,
        @Key        = @Lock_Key,
        @LifeTime   = @Lock_LifeTime,
        @RaisError  = 1

      IF @Method = 'ONLINE' BEGIN
        IF NOT EXISTS(SELECT TOP 1 1 FROM @Filters WHERE [Value] IS NOT NULL OR [Values] IS NOT NULL)
          INSERT INTO @WaitingOnLine([Identity], [Action], [TimeStamp], [Params])
          SELECT
            [Identity],
            [Action],
            [TimeStamp],
            [Params]
          FROM [Export].[Records](@SubScriber_Id, @Object_Id)
        ELSE BEGIN
          SELECT
            @SQL =
              N'DECLARE'
              +
              [Pub].[ConCat]
              (
                N'
  @' + [Name] + Replicate(' ', [Pub].[Is Negative Int](16 - Len([Name]), 0)) + N' '
                +
                CASE
                  WHEN [Values] IS NOT NULL THEN N'XML = @Filters.query(''/FILTER[@NAME="' + [Name] + N'"]/LIST'')'
                  ELSE [Type] + N' = @Filters.value(''(/FILTER[@NAME="' + [Name] + N'"]/@VALUE)[1]'', ' + [Pub].[Quote String]([Type]) + N')'
                END,
                N','
              )
              + N'

SELECT
  R.[Identity],
  R.[Action],
  R.[TimeStamp],
  R.[Params]
FROM [Export].[Records](' + Cast(@SubScriber_Id AS NVarChar) + N', ' + Cast(@Object_Id AS NVarChar) + N') R
LEFT JOIN ' + @View + N' T ON R.[Action] <> ''D'' AND R.[Identity] = T.' + QuoteName(@IdentityField)
              +
              IsNull
              (
                [Pub].[ConCat]
                (
                  CASE
                    WHEN [Values] IS NOT NULL THEN N'
INNER JOIN
(
  SELECT
    ' + QuoteName([Name]) + N' = I.Node.value(''@VALUE'', ' + [Pub].[Quote String]([Type]) + N')
  FROM @' + [Name] + N'.nodes(''/LIST'') AS I (Node)
) F' + Cast([Index] AS NVarChar) + N' ON CASE WHEN R.[Action] = ''D'' THEN R.[Params].' + [SQL].[TParams::Method By Type]([Type]) + N'(' + [Pub].[Quote String]([Name]) + N') ELSE T.' + QuoteName([Name]) + N' END = F' + Cast([Index] AS NVarChar) + N'.' + QuoteName([Name])
                  END,
                  N''
                ),
                N''
              )
              +
              IsNull
              (
                N'
' + @SQLTabs + N'WHERE '
                +
                [Pub].[ConCat]
                (
                  CASE WHEN [Values] IS NULL THEN N'CASE WHEN R.[Action] = ''D'' THEN R.[Params].' + [SQL].[TParams::Method By Type]([Type]) + N'(' + [Pub].[Quote String]([Name]) + N') ELSE T.' + QuoteName([Name]) + N' END = @' + [Name] END,
                  N'
  AND '
                ),
                N''
              )
          FROM @Filters
          WHERE [Value] IS NOT NULL OR [Values] IS NOT NULL

          IF @Debug = 1
            EXEC [SQL].[Print] @SQL

          INSERT INTO @WaitingOnLine([Identity], [Action], [TimeStamp], [Params])
          EXEC sp_executesql @SQL, N'@Filters XML', @FiltersXML
        END

        SELECT
          @RecieveTimeStamp = MAX([TimeStamp])
        FROM @WaitingOnLine
        WHERE [TimeStamp] IS NOT NULL

        BEGIN TRY
          DELETE W
          FROM [Export].[Waiting] W
          LEFT JOIN @WaitingOnLine R ON R.[Action] <> ' ' AND W.[Identity] = R.[Identity]
          WHERE W.[Object_Id] = @Object_Id AND W.[SubScriber_Id] = @SubScriber_Id
                AND R.[Identity] IS NULL

          ;MERGE [Export].[Waiting] WITH (ROWLOCK) W
          USING (SELECT * FROM @WaitingOnLine WHERE [Action] <> ' ') R
          ON W.[Object_Id] = @Object_Id AND W.[SubScriber_Id] = @SubScriber_Id
                AND W.[Identity] = R.[Identity]
          WHEN MATCHED AND (W.[Action] <> R.[Action])
            THEN UPDATE SET
              [Action] = R.[Action],
              [Params] = R.[Params]
          WHEN NOT MATCHED BY TARGET THEN
            INSERT ([Object_Id], [SubScriber_Id], [Identity], [Action], [Params])
            VALUES
            (
              @Object_Id,
              @SubScriber_Id,
              R.[Identity],
              R.[Action],
              R.[Params]
            )
          OPTION (FORCE ORDER, MAXDOP 2, HASH JOIN);

          UPDATE [Export].[SubScriptions] SET
            [OnLine:Recieve:TimeStamp]  = IsNull(@RecieveTimeStamp, [OnLine:Recieve:TimeStamp]),
            [OnLine:LastRun:Time]       = @RunTime
          WHERE [Object_Id] = @Object_Id AND [SubScriber_Id] = @SubScriber_Id

          RETURN @Lock_Id
        END TRY
        BEGIN CATCH
          EXEC [System].[UnLock] @Id = @Lock_Id
          EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID
        END CATCH
      END ELSE IF @Method = 'ONTIME' BEGIN
        IF @FiltersXML IS NULL OR NOT EXISTS(SELECT 1 FROM @Filters WHERE [Value] IS NOT NULL OR [Values] IS NOT NULL)
          INSERT INTO @WaitingOnTime([Identity], [DateTime], [TimeStamp])
          SELECT
            [Identity],
            [DateTime],
            [TimeStamp]
          FROM [Export].[Records(Time)](@SubScriber_Id, @Object_Id, @RecieveTimeStamp, @Enabled)
        ELSE BEGIN
          SELECT
            @SQL =
              N'DECLARE'
              +
              [Pub].[ConCat]
              (
                N'
  @' + [Name] + Replicate(' ', [Pub].[Is Negative Int](16 - Len([Name]), 0)) + N' '
                +
                CASE
                  WHEN [Values] IS NOT NULL THEN N'XML = @Filters.query(''/FILTER[@NAME="' + [Name] + N'"]/LIST'')'
                  ELSE [Type] + N' = @Filters.value(''(/FILTER[@NAME="' + [Name] + N'"]/@VALUE)[1]'', ' + [Pub].[Quote String]([Type]) + N')'
                END,
                N','
              )
              + N'

SELECT
  R.[Identity],
  R.[DateTime],
  R.[TimeStamp]
FROM [Export].[Records(' + CASE WHEN Left(@Enabled, 1) = 'M' THEN N'Min ' ELSE N'' END + N'Time)](' + Cast(@SubScriber_Id AS NVarChar) + N', ' + Cast(@Object_Id AS NVarChar) + N', ' + Convert(NVarChar(18), @RecieveTimeStamp, 1) + N', ' + [Pub].[Quote String](@Enabled) + N') R
INNER JOIN ' + @View + N' T ON R.[Identity] = T.' + QuoteName(@IdentityField)
              +
              IsNull
              (
                [Pub].[ConCat]
                (
                  CASE
                    WHEN [Values] IS NOT NULL THEN N'
INNER JOIN
(
  SELECT
    ' + QuoteName([Name]) + N' = I.Node.value(''@VALUE'', ' + [Pub].[Quote String]([Type]) + N')
  FROM @' + [Name] + N'.nodes(''/LIST'') AS I (Node)
) F' + Cast([Index] AS NVarChar) + N' ON T.' + QuoteName([Name]) + N' = F' + Cast([Index] AS NVarChar) + N'.' + QuoteName([Name])
                  END,
                  N''
                ),
                N''
              )
              +
              IsNull
              (
                N'
' + @SQLTabs + N'WHERE '
                +
                [Pub].[ConCat]
                (
                  CASE
                    WHEN [Values] IS NULL THEN N'T.' + QuoteName([Name]) + N' = @' + [Name]
                  END,
                  N'
  AND '
                ),
                N''
              )
          FROM @Filters
          WHERE [Value] IS NOT NULL OR [Values] IS NOT NULL

          IF @SQL IS NULL
            RaisError('Не удалось сформировать SQL-запрос к БД. Обратитесь к разработчику!
>> @View = %s
>> @IdentityField = %s', 16, 1, @View, @IdentityField)

          IF @Debug = 1
            EXEC [SQL].[Print] @SQL

          INSERT INTO @WaitingOnTime([Identity], [DateTime], [TimeStamp])
          EXEC sp_executesql @SQL, N'@Filters XML', @FiltersXML
        END

        SELECT
          @RecieveTimeStamp = MAX([TimeStamp])
        FROM @WaitingOnTime
        WHERE [TimeStamp] IS NOT NULL

        BEGIN TRY
          DELETE W
          FROM [Export].[Waiting(Time)] W
          LEFT JOIN @WaitingOnTime R ON W.[Identity] = R.[Identity] AND W.[DateTime] = R.[DateTime]
          WHERE W.[Object_Id] = @Object_Id AND W.[SubScriber_Id] = @SubScriber_Id
                AND R.[Identity] IS NULL

          ;MERGE [Export].[Waiting(Time)] WITH (ROWLOCK) W
          USING @WaitingOnTime R
          ON W.[Object_Id] = @Object_Id AND W.[SubScriber_Id] = @SubScriber_Id
                AND W.[Identity] = R.[Identity] AND W.[DateTime] = R.[DateTime]
          WHEN NOT MATCHED BY TARGET THEN
            INSERT ([Object_Id], [SubScriber_Id], [Identity], [DateTime])
            VALUES
            (
              @Object_Id,
              @SubScriber_Id,
              R.[Identity],
              R.[DateTime]
            )
          OPTION (FORCE ORDER, MAXDOP 2, HASH JOIN);

          UPDATE [Export].[SubScriptions] SET
            [OnTime:Recieve:TimeStamp]  = IsNull(@RecieveTimeStamp, [OnTime:Recieve:TimeStamp]),
            [OnTime:LastRun:Time]       = @RunTime
          WHERE [Object_Id] = @Object_Id AND [SubScriber_Id] = @SubScriber_Id

          RETURN @Lock_Id
        END TRY
        BEGIN CATCH
          EXEC [System].[UnLock] @Id = @Lock_Id
          EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID
        END CATCH
      END
    END
    -------------------------------- [Locked In This Session] ----------------------------
    ELSE IF @Method IN ('ONLINE', 'ONTIME') AND @Action IN ('ROWS', 'REFILL', 'DATASET', 'DATASET:STATIC', 'DATASET:PERIODIC:RAW', 'DATASET:PERIODIC:PAGE', 'XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT', 'RESULTS') AND IsNull([System].[Locked In This Session] (@Lock_Object, @Lock_Key), 0) = 0
      RaisError('Действие «%s» отклонено - отсутствует локировка.', 16, 1, @Action)
    ------------------------------------------ ROWS -----------------------
    ELSE IF @Action = 'ROWS' BEGIN
      IF @Method = 'ONLINE'
        IF @SQLDeclare IS NULL
          SELECT
            [Id]      = W.[Identity],
            [Action]  = W.[Action]
          FROM [Export].[Waiting] W
          WHERE W.[Object_Id] = @Object_Id AND W.[SubScriber_Id] = @SubScriber_Id
        ELSE BEGIN
          SET @SQL = @SQLDeclare
              +
N'SELECT
  [Id]      = W.[Identity],
  [Action]  = W.[Action]
FROM [Export].[Waiting] W
LEFT JOIN ' + @View + N' T ON W.[Action] <> ''D'' AND W.[Identity] = T.' + QuoteName(@IdentityField)
              + @SQLInner
              + N'
WHERE W.[Object_Id] = ' + Cast(@Object_Id AS NVarChar) + N' AND W.[SubScriber_Id] = ' + Cast(@SubScriber_Id AS NVarChar)
              + IsNull(N'
' + @SQLTabs + N'      AND ' + @SQLWhere, N'')

          IF @SQL IS NULL
            RaisError('Не удалось сформировать SQL-запрос к БД. Обратитесь к разработчику!
>> @View = %s
>> @IdentityField = %s', 16, 1, @View, @IdentityField)

          IF @Debug = 1
            EXEC [SQL].[Print] @SQL

          EXEC sp_executesql @SQL, N'@Filters XML', @FiltersXML
        END
      ELSE IF @Method = 'ONTIME' BEGIN
        IF @SQLDeclare IS NULL
          SELECT
            [Id]        = W.[Identity],
            [DateTime]  = W.[DateTime]
          FROM [Export].[Waiting(Time)] W
          WHERE W.[Object_Id] = @Object_Id AND W.[SubScriber_Id] = @SubScriber_Id
        ELSE BEGIN
          SET @SQL = @SQLDeclare
              +
N'SELECT
  [Id]        = W.[Identity],
  [DateTime]  = W.[DateTime]
FROM [Export].[Waiting(Time)] W
INNER JOIN ' + @View + N' T ON W.[Identity] = T.' + QuoteName(@IdentityField)
              + @SQLInner
              + N'
WHERE W.[Object_Id] = ' + Cast(@Object_Id AS NVarChar) + N' AND W.[SubScriber_Id] = ' + Cast(@SubScriber_Id AS NVarChar)
              + IsNull(N'
' + @SQLTabs + N'      AND ' + @SQLWhere, N'')

          IF @SQL IS NULL
            RaisError('Не удалось сформировать SQL-запрос к БД. Обратитесь к разработчику!
>> @View = %s
>> @IdentityField = %s', 16, 1, @View, @IdentityField)

          IF @Debug = 1
            EXEC [SQL].[Print] @SQL

          EXEC sp_executesql @SQL, N'@Filters XML', @FiltersXML
        END
      END
    END
    ------------------------------------------ ROWS -----------------------
    ELSE IF @Action = 'REFILL' BEGIN
      IF @Method = 'ONLINE' BEGIN
        SET @SQL = @SQLDeclare
              +
N'MERGE [Export].[Waiting] W
USING
(
  SELECT
    [Identity] = T.' + QuoteName(@IdentityField) + N'
  FROM ' + @View + N' T'
              + @SQLInner
              + IsNull(N'
' + @SQLTabs + N'  WHERE ' + @SQLWhere, N'')
              + N'
) I ON W.[Object_Id] = ' + Cast(@Object_Id AS NVarChar) + N' AND W.[SubScriber_Id] = ' + Cast(@SubScriber_Id AS NVarChar) + N' AND W.[Identity] = I.[Identity]
WHEN MATCHED AND W.[Action] <> ''R'' THEN
  UPDATE SET [Action] = ''R'', [Error] = NULL, [Params] = NULL
WHEN NOT MATCHED BY TARGET THEN
  INSERT ([Object_Id], [SubScriber_Id], [Identity], [Action])
  VALUES (' + Cast(@Object_Id AS NVarChar) + N', ' + Cast(@SubScriber_Id AS NVarChar) + N', I.[Identity], ''R'');'

        IF @Debug = 1
          EXEC [SQL].[Print] @SQL

        EXEC sp_executesql @SQL, N'@Filters XML', @FiltersXML
      END ELSE IF @Method = 'ONTIME'
        RaisError('Abstract error: Действие не реализованно!', 16, 2)
    END
    ------------------------------------------ ROWS INFO ------------------
    ELSE IF @Action IN ('DATASET', 'DATASET:STATIC', 'DATASET:PERIODIC:RAW', 'DATASET:PERIODIC:PAGE', 'XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT') BEGIN
      IF @Procedure IS NOT NULL BEGIN
        IF @Action IN ('XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT')
          SET @@Action = 'XML'
        ELSE
          SET @@Action = @Action

        IF @Method = 'OFFLINE'
          EXEC @Procedure
            @Action         = @@Action,
            @Properties     = @Properties,
            @Params         = @Params,
            @Filters        = @FiltersPrepared,
            @Fields         = @Fields,
            @Output         = @Output OUT
        ELSE IF @Method = 'ONLINE' BEGIN
          INSERT INTO @RowsOnLine([Identity], [Action], [Params])
          SELECT
            [Identity],
            [Action],
            [Params]
          FROM [Export].[Waiting]
          WHERE [Object_Id] = @Object_Id AND [SubScriber_Id] = @SubScriber_Id

          EXEC @Procedure
            @Action         = @@Action,
            @Properties     = @Properties,
            @Params         = @Params,
            @Filters        = @FiltersPrepared,
            @Fields         = @Fields,
            @Rows           = @RowsOnLine,
            @Output         = @Output OUT
        END ELSE IF @Method = 'ONTIME' BEGIN
          INSERT INTO @RowsOnTime([Identity], [DateTime])
          SELECT
            [Identity],
            [DateTime]
          FROM [Export].[Waiting(Time)]
          WHERE [Object_Id] = @Object_Id AND [SubScriber_Id] = @SubScriber_Id

          EXEC @Procedure
            @Action         = @@Action,
            @Properties     = @Properties,
            @Params         = @Params,
            @Filters        = @FiltersPrepared,
            @Fields         = @Fields,
            @Rows           = @RowsOnTime,
            @Output         = @Output OUT
        END

        IF @@Action = 'XML' BEGIN
          SET @Output =
            (
              SELECT
                [NAME] = @Target,
                @Output
              FOR XML RAW('OBJECT'), BINARY BASE64, TYPE
            )

          IF @Action = 'XML:SELECT' BEGIN
            SELECT [XML] = @Output
            SET @Output = NULL
          END ELSE IF @Action = 'XML:SELECT:TEXT' BEGIN
            SELECT [XML] = Cast(@Output AS NVarChar(Max))
            SET @Output = NULL
          END
        END
      END ELSE BEGIN
        IF @Action IN ('DATASET:STATIC')
          DELETE @Fields WHERE [Periodic] = 1
        ELSE IF @Action IN ('DATASET:PERIODIC:RAW', 'DATASET:PERIODIC:PAGE')
          DELETE @Fields WHERE [Periodic] = 0

        IF NOT EXISTS(SELECT TOP 1 1 FROM @Fields)
          RaisError('Подписка «%s»:«%s», объект «%s» - получен пустой набор данных с описанием полей.', 16, 1, @SubScriber, @Object, @Target)

        SET @SQL = @SQLTabs + N'SELECT'
              +
              CASE
                WHEN @OnLineRoot = 1 AND @Action IN ('XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT') THEN N'
  ' + @SQLTabs + N'[ACTION]  = W.[Action],'
                ELSE N''
              END
              +
              CASE
                WHEN @Action IN ('DATASET', 'DATASET:STATIC', 'XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT') THEN
                (
                  SELECT
                    [Pub].[Concat]
                    (
                      N'
  ' + @SQLTabs
                      +
                      CASE
                        WHEN @Action IN ('XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT') AND F.[Periodic] = 1 THEN
                          N'(SELECT [NAME] = ' + [Pub].[Quote String](F.[Name]) + N', ' + QuoteName([Origin:Schema]) + N'.[Periodic:Field:Values@XML](' + Cast(F.[Origin:Periodic:Field_Id] AS NVarChar) + N', T.' + QuoteName(@IdentityField) + N')'
                          + CASE WHEN @OnLineRoot = 1 THEN N' WHERE W.[ACTION] <> ''D''' ELSE N'' END
                          + N' FOR XML RAW(''FIELD''), BINARY BASE64, TYPE)'
                        WHEN @Action IN ('XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT') AND F.[Translate] = 1 THEN
                          N'(SELECT [NAME] = ' + [Pub].[Quote String](F.[Name]) + N', ' + QuoteName([Origin:Schema]) + N'.[Translate:Field:Values@XML](' + Cast(F.[Origin:Translate:Field_Id] AS NVarChar) + N', T.' + QuoteName(@IdentityField) + N')'
                          + CASE WHEN @OnLineRoot = 1 THEN N' WHERE W.[ACTION] <> ''D''' ELSE N'' END
                          + N' FOR XML RAW(''FIELD''), BINARY BASE64, TYPE)'
                        WHEN @OnLineRoot = 1 AND F.[Name] = @IdentityField THEN QuoteName(F.[Name]) + N' = W.[Identity]'
                        WHEN @OnLineRoot = 1 AND F.[Filter] <> 'N' THEN QuoteName(F.[Name]) + N' = CASE WHEN W.[Action] = ''D'' THEN W.[Params].' + [SQL].[TParams::Method By Type](F.[Type]) + N'(' + [Pub].[Quote String](F.[Name]) + N') ELSE T.' + QUOTENAME(F.[Name]) + N' END'
                        ELSE
                          CASE
                            WHEN F.[Clr] = 1 THEN QUOTENAME(F.[Name]) + N' = Cast('
                            ELSE N''
                          END
                          +
                          CASE
                            WHEN @Language IS NOT NULL AND @Action IN ('DATASET', 'DATASET:STATIC') AND @UseView = 1 AND F.[Translate] = 1 THEN N'L'
                            ELSE N'T'
                          END
                          + N'.' + QUOTENAME(F.[Name])
                          +
                          CASE
                            WHEN F.[Clr] = 1 THEN N' AS ' + F.[Type] + N')'
                            ELSE N''
                          END
                      END,
                      N','
                    )
                  FROM
                  (
                    SELECT TOP 1000
                      [Name],
                      [Type],
                      [Clr],
                      [Filter],
                      [Translate],
                      [Periodic],
                      [Origin:Schema],
                      [Origin:Periodic:Field_Id],
                      [Origin:Translate:Field_Id]
                    FROM @Fields
                    WHERE @Action IN ('DATASET', 'XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT') OR [Periodic] = 0
                    ORDER BY [Index]
                  ) F
                )
                ELSE
                  N' T.' + QuoteName(@IdentityField)
              END
              + N'
' + @SQLTabs + N'FROM '
              +
              CASE
                WHEN @Method IN ('ONLINE') THEN N'[Export].[Waiting] W
' + @SQLTabs
                  + CASE WHEN @OnLineRoot = 1 AND @Action IN ('XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT') THEN N'LEFT LOOP' ELSE N'INNER' END
                  + N' JOIN '
                ELSE N''
              END
              +
              CASE
                WHEN @Language IS NULL OR @Action NOT IN ('DATASET', 'DATASET:STATIC') OR @UseView = 1 THEN @View
                ELSE @Translate + N'(' + [Pub].[Quote String](@Language) + N')'
              END
              + N' T'
              +
              CASE
                WHEN @Method IN ('ONLINE', 'ONTIME') THEN N' ON W.[Identity] = T.' + QuoteName(@ParentField)
                ELSE N''
              END
              +
              CASE
                WHEN @Language IS NOT NULL AND @Action IN ('DATASET', 'DATASET:STATIC') AND @UseView = 1 THEN N'
' + @SQLTabs + N'INNER JOIN ' + @Translate + N'(' + [Pub].[Quote String](@Language) + N') L ON T.' + QuoteName(@IdentityField) + N' = L.' + QuoteName(@IdentityField)
                ELSE N''
              END
              + @SQLInner
              +
              CASE
                WHEN @Method IN ('ONLINE', 'ONTIME') THEN N'
' + @SQLTabs + N'WHERE W.[Object_Id] = ' + Cast(@Object_Id AS NVarChar) + N' AND W.[SubScriber_Id] = ' + Cast(@SubScriber_Id AS NVarChar)
                  + CASE WHEN @Method IN ('ONLINE') AND @OnLineRoot = 0 THEN N' AND W.[Action] <> ''D''' ELSE N'' END
                  +
                  IsNull
                  (
                    N'
' + @SQLTabs + N'      AND ' + @SQLWhere,
                    N''
                  )
                ELSE
                  IsNull
                  (
                    N'
' + @SQLTabs + N'WHERE ' + @SQLWhere,
                    N''
                  )
              END

        IF @Action IN ('DATASET:PERIODIC:RAW')
          SET @SQL = N'SELECT
  [#ID#]    = T.' + QuoteName(@IdentityField) + N',
  [#FIELD#] = PV.[FieldName],
  [#DATE#]  = PV.[Date],
  [#VALUE#] = PV.[Value]
FROM
(
'
            + @SQL
            + N'
) T
CROSS APPLY
('
            +
            (
              SELECT
                [Pub].[Concat]
                (
                  N'
  SELECT [FieldName] = '
                  + [Pub].[Quote String]([Name]) + N', [Date], [Value] FROM '
                  + QuoteName([Origin:Schema]) + N'.[Periodic:Field:Values@Enum]('
                  + Cast([Origin:Periodic:Field_Id] AS NVarChar)
                  + N', T.' + QuoteName(@IdentityField)
                  + CASE WHEN S.[Translate] = 1 THEN N', ' + IsNull([Pub].[Quote String](@Language), N'NULL') ELSE N'' END
                  + N')',
                  N'
    UNION ALL'
                )
              FROM @Fields F
              INNER JOIN
              (
                SELECT
                  [Schema]    = S.[Origin:Schema],
                  [Translate] = [SQL].[Procedure Parameter Exists](QuoteName([Origin:Schema]) + N'.[Periodic:Field:Values@Enum]', 0, '@Language')
                FROM
                (
                  SELECT DISTINCT
                    [Origin:Schema]
                  FROM @Fields
                  WHERE [Periodic] = 1
                ) S
              ) S ON F.[Origin:Schema] = S.[Schema]
              WHERE [Periodic] = 1
            )
            + N'
) PV'
        ELSE IF @Action IN ('DATASET:PERIODIC:PAGE')
          SET @SQL = N'SELECT
  [#ID#]    = T.' + QuoteName(@IdentityField) + N',
  [#DATE#]  = D.[Date]'
                +
                (
                  SELECT
                    [Pub].[Concat]
                    (
                      N',
  '
                      + N'P.'
                      + QUOTENAME(F.[Name]),
                      N''
                    )
                  FROM
                  (
                    SELECT TOP 1000
                      [Name], [Periodic]
                    FROM @Fields
                    WHERE [Periodic] = 1
                    ORDER BY [Index]
                  ) F
                )
                + N'
FROM
(
'
            + @SQL
            + N'
) T
CROSS APPLY
('
            +
            (
              SELECT
                [Pub].[Concat]
                (
                  N'
  SELECT [Date] = [BeginDate] FROM '
                  + QuoteName([Origin:Schema]) + N'.[Periodic:Field@Periods]('
                  + Cast([Origin:Periodic:Field_Id] AS NVarChar)
                  + N', T.' + QuoteName(@IdentityField)
                  + N', NULL, NULL'
                  + CASE WHEN S.[Translate] = 1 THEN N', ' + IsNull([Pub].[Quote String](@Language), N'NULL') ELSE N'' END
                  + N')',
                  N'
    UNION'
                )
              FROM @Fields F
              INNER JOIN
              (
                SELECT
                  [Schema]    = S.[Origin:Schema],
                  [Translate] = [SQL].[Procedure Parameter Exists](QuoteName([Origin:Schema]) + N'.[Periodic:Field:Values@Enum]', 0, '@Language')
                FROM
                (
                  SELECT DISTINCT
                    [Origin:Schema]
                  FROM @Fields
                  WHERE [Periodic] = 1
                ) S
              ) S ON F.[Origin:Schema] = S.[Schema]
              WHERE [Periodic] = 1
            )
            + N'
) D
CROSS APPLY (SELECT P.* FROM ' + @Periodic + N'(D.[Date]' + CASE WHEN [SQL].[Procedure Parameter Exists](@Periodic, 0, '@Language') = 1 THEN N', ' + IsNull([Pub].[Quote String](@Language), N'NULL') ELSE N'' END
            + N') P WHERE T.' + QuoteName(@IdentityField) + N' = P.' + QuoteName(@IdentityField) + N') P'
        ELSE IF @Action IN ('XML:SELECT', 'XML:SELECT:TEXT', 'XML:OUTPUT')
          SET @SQL =
            N'SELECT
  '
            +
            CASE
              WHEN @Action IN ('XML:SELECT', 'XML:SELECT:TEXT') THEN N'[XML]'
              ELSE N'@XML'
            END
            + N' =
  (
    SELECT
      [NAME] = ' + [Pub].[Quote String](@Target) + N',
      (
'
              + @SQL
              + N'
        FOR XML RAW(''RECORD''), BINARY BASE64, TYPE
      )
    FOR XML RAW(''OBJECT''), BINARY BASE64' + CASE WHEN @Action <> N'XML:SELECT:TEXT' THEN N', TYPE' ELSE N'' END + N'
  )'

        SET @SQL = @SQLDeclare + @SQL
          +
          CASE
            WHEN @Method IN ('ONLINE', 'ONTIME') THEN N'
  OPTION (FORCE ORDER, MAXDOP 1)'
            ELSE N''
          END

        IF @SQL IS NULL
          RaisError('Не удалось сформировать SQL-запрос к БД. Обратитесь к разработчику!', 16, 1)

        IF @Debug = 1
          EXEC [SQL].[Print] @SQL

        EXEC sp_executesql @SQL, N'@Filters XML, @XML XML OUT', @FiltersXML, @Output OUT
      END

      IF @Method = 'ONLINE'
        UPDATE [Export].[SubScriptions] SET
          [OnLine:LastRun:Time]     = @RunTime
        WHERE [Object_Id] = @Object_Id AND [SubScriber_Id] = @SubScriber_Id
      ELSE IF @Method = 'OFFLINE'
        UPDATE [Export].[SubScriptions] SET
          [OffLine:LastRun:Time]     = @RunTime
        WHERE [Object_Id] = @Object_Id AND [SubScriber_Id] = @SubScriber_Id
      ELSE IF @Method = 'ONTIME'
        UPDATE [Export].[SubScriptions] SET
          [OnTime:LastRun:Time]     = @RunTime
        WHERE [Object_Id] = @Object_Id AND [SubScriber_Id] = @SubScriber_Id
    END
    ------------------------------------------ ROWS RESULTS ---------------------
    ELSE IF @Action = 'RESULTS' BEGIN
      IF @Method = 'ONLINE'
        MERGE [Export].[Waiting] W
        USING
        (
          SELECT
            [Identity]  = I.node.value('@ID', 'Int'),
            [Error]     = I.node.value('@ERROR', 'NVarChar(2048)')
         FROM @Params.nodes('/RESULTS/RESULT') I (Node)
        ) A ON W.[Identity] = A.[Identity]
        AND W.[Object_Id] = @Object_Id AND W.[SubScriber_Id] = @SubScriber_Id
        WHEN MATCHED AND A.[Error] IS NOT NULL THEN
          UPDATE SET
            [Error] = A.[Error]
        WHEN MATCHED THEN
          DELETE;
      ELSE IF @Method = 'ONTIME'
        MERGE [Export].[Waiting(Time)] W
        USING
        (
          SELECT
            [Identity]  = I.node.value('@ID', 'Int'),
            [DateTime]  = I.node.value('@DATETIME', 'DateTime'),
            [Error]     = I.node.value('@ERROR', 'NVarChar(2048)')
         FROM @Params.nodes('/RESULTS/RESULT') I (Node)
        ) A ON W.[Identity] = A.[Identity] AND W.[DateTime] = A.[DateTime]
        AND W.[Object_Id] = @Object_Id AND W.[SubScriber_Id] = @SubScriber_Id
        WHEN MATCHED AND A.[Error] IS NOT NULL THEN
          UPDATE SET
            [Error] = A.[Error]
        WHEN MATCHED THEN
          DELETE;
    END
    ------------------------------------------ END ----------------------------
    ELSE IF @Action IN ('END') BEGIN
      SET @Lock_Id = @Params.value('(/PARAMS/PARAM[@NAME="Lock_Id"]/@VALUE)[1]', 'BigInt')
      IF @Lock_Id IS NULL
        SET @Lock_Id = [System].[Lock::Get Id(In This Session)](NULL, @Lock_Object, @Lock_Key)
      EXEC [System].[UnLock] @Id = @Lock_Id
    END

    RETURN -1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID
  END CATCH
GO

GRANT EXECUTE ON [Export].[Execute] TO [public]
GO
