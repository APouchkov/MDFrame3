ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm]
  @ClassName                VarChar(256)  = NULL,   -- Имя класса для журнала
  @ParentClassName          VarChar(256),           -- Имя базового класса журнала
  @IdentityParams           VarChar(256)  = NULL,   -- Имя Identity параметров журнала в формате "NAME1=VALUE1;NAME2=VALUE2"

  @Category                 VarChar(256)  = NULL,   -- Имя категории справочника - нужно для формирования св-ва "Caption"
  @Caption                  VarChar(256),           -- Имя справочника - нужно для формирования св-ва "Caption"

  @ObjectName               SysName,                -- Имя таблицы или вьюхи на основе которой будут формироваться шаблоны
  @ObjectNameSingle         SysName       = NULL,   -- Как пишется @ObjectName для объекта в единственном числе
  @IdentityField            SysName       = NULL,   -- Автовычисляемое "Identity/Calculated" поле. По-умолчанию берётся поле со св-вом [is_identity] = 1
  @KeyField                 SysName       = NULL,   -- Ключевое поле. По-умолчанию берётся значение @IdentityField
  @ParentField              SysName       = NULL,   -- Поле "Parent" для списка в формате "Дерево"

  @ProcedureObjectsView     SysName       = NULL,   -- Имя процедуры журнала. Если не указано, то сформируется по-умолчанию.
  @InsertableControl        Bit           = 0,      -- Если указано "1" то контроль за правом Insert отдаётся процедуре журнала.

  @CaptionFormatSingle      VarChar(256)  = NULL,   -- Имя справочника в единственном числе - нужно для формирования св-ва "CaptionFormat" формы просмотра
  @ObjectViewClassName      VarChar(256)  = NULL,   -- Имя (если есть) класса для просмотра / редактирования элемета справочника. InPlaceEdit, если не задано
  @ObjectViewIdentityParams VarChar(256)  = NULL,   -- Имя Identity параметров формы просмотра в формате "NAME1=VALUE1;NAME2=VALUE2". Если не задан то берётся из @IdentityParams

  @Object2Name              SysName       = NULL,   -- Имя таблицы или вьюхи на основе которой будут формироваться шаблоны для списка в формате "CxView2"
  @Object2NameSingle        SysName       = NULL,   -- Как пишется @Object2Name для объекта в единственном числе
  @IdentityField2           SysName       = NULL,   -- Автовычисляемое "Identity/Calculated" поле для списка в формате "CxView2". По-умолчанию берётся поле со св-вом [is_identity] = 1
  @KeyField2                SysName       = NULL,   -- Ключевое поле. По-умолчанию берётся значение @IdentityField2
  @MasterFields2            VarChar(512)  = NULL,   -- Поле "MasterFields" для списка в формате "CxView2"
  @DetailFields2            VarChar(512)  = NULL,   -- Поле "DetailFields" для списка в формате "CxView2"
  @Object2ViewClassName     VarChar(256)  = NULL,   -- Имя (если есть) класса для просмотра / редактирования элемета справочника №2. InPlaceEdit, если не задано

  @Object3Name              SysName       = NULL,   -- Имя таблицы или вьюхи на основе которой будут формироваться шаблоны для списка в формате "CxView3"
  @Object3NameSingle        SysName       = NULL,   -- Как пишется @Object3Name для объекта в единственном числе
  @IdentityField3           SysName       = NULL,   -- Автовычисляемое "Identity/Calculated" поле для списка в формате "CxView3". По-умолчанию берётся поле со св-вом [is_identity] = 1
  @KeyField3                SysName       = NULL,   -- Ключевое поле. По-умолчанию берётся значение @IdentityField3
  @MasterFields3            VarChar(512)  = NULL,   -- Поле "MasterFields" для списка в формате "CxView3"
  @DetailFields3            VarChar(512)  = NULL,   -- Поле "DetailFields" для списка в формате "CxView3"
  @Object3ViewClassName     VarChar(256)  = NULL,   -- Имя (если есть) класса для просмотра / редактирования элемета справочника №3. InPlaceEdit, если не задано

  @DirectDelete             Bit           = 0,      -- Прямое удаление из справочника, или же делается обёртка из процедуры
  @DirectDelete2            Bit           = 0,      -- Прямое удаление из справочника, или же делается обёртка из процедуры
  @DirectDelete3            Bit           = 0       -- Прямое удаление из справочника, или же делается обёртка из процедуры
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @Object_Id                  Int,
    @Schema                     SysName,
    @Name                       SysName,
    @Translation                Bit,

    @IdentityMethod             Char(1),      -- 'I' = '[is_identity]', 'C' = [is_computed], 'D' = [default_object_id]
    @IdentityFieldType          SysName,
    @KeyFieldType               SysName,
    @KeyFieldUnique             Bit,          -- Признак того, что ключевое поле справочника является асолютно уникальным для всей таблицы а не только в контексте текущей выборки

    @@Integer                   Int,
    @@Function                  SysName,
    @@IdentityField             SysName,

    @@ObjectName                SysName,

    @@ObjectNameSingle          SysName,
    @@Object2NameSingle         SysName,
    @@Object3NameSingle         SysName,

    @Object2_Id                 Int,
    @Schema2                    SysName,
    @Name2                      SysName,
    @Translation2               Bit,

    @Identity2Method            Char(1),
    @IdentityField2Type         SysName,
    @KeyField2Type              SysName,
    @KeyField2Unique            Bit,

    @Object3_Id                 Int,
    @Schema3                    SysName,
    @Name3                      SysName,
    @Translation3               Bit,

    @Identity3Method            Char(1),
    @IdentityField3Type         SysName,
    @KeyField3Type              SysName,
    @KeyField3Unique            Bit,

    @ObjectTranslateViewName      SysName,
    @Object2TranslateViewName     SysName,
    @Object3TranslateViewName     SysName,
    @ObjectTranslateInsertName    SysName,
    @Object2TranslateInsertName   SysName,
    @Object3TranslateInsertName   SysName,
    @ObjectTranslateUpdateName    SysName,
    @Object2TranslateUpdateName   SysName,
    @Object3TranslateUpdateName   SysName,

    @ObjectPrivileges_Id        SmallInt,
    @ObjectPrivilegesView       SysName,

    @Object2Privileges_Id       SmallInt,
    @Object2PrivilegesView      SysName,

    @Object3Privileges_Id       SmallInt,
    @Object3PrivilegesView      SysName,

    @IsSecurityModeTable        Bit       = 0,
    @IsSecurityModeProcedure    Bit       = 0,

    @IsSecurityModeTable2       Bit       = 0,
--    @IsSecurityModeProcedure2   Bit       = 0,

    @IsSecurityModeTable3       Bit       = 0,
    @IsSecurityModeProcedure3   Bit       = 0,

    @ProcedureObjectsViewDelphi SysName,

    @ProcedureObjectViewDelphi  SysName,
    @ProcedureObject2ViewDelphi SysName,
    @ProcedureObject3ViewDelphi SysName,

    @ProcedureObjectView        SysName,
    @ProcedureObject2View       SysName,
    @ProcedureObject3View       SysName,

    @ProcedureObjectModify      SysName,
    @ProcedureObject2Modify     SysName,
    @ProcedureObject3Modify     SysName,

    @ProcedureObjectDelete      SysName,
    @ProcedureObject2Delete     SysName,
    @ProcedureObject3Delete     SysName,

    @Periodic                   Bit       = 0,
    @PeriodicAgeFieldName       SysName,
    @PeriodicFields             NVarChar(Max),

    @Logging                    Bit       = 0,

    @@RoleName                  SysName,
    @RoleView                   SysName,
    @RoleModify                 SysName,
    @RoleDelete                 SysName,
    
    @@ProcedureRoleView         SysName,

    @SQL                        NVarChar(Max),
    @OUT_Object                 SysName,
    @OUT_Message                NVarChar(256)

  DECLARE @Results TABLE
  (
    [Object]  SysName       NOT NULL,
    [Message] NVarChar(256) NOT NULL
  )

  BEGIN TRY
    IF @DirectDelete IS NULL
      SET @DirectDelete = 1

    IF @ParentClassName IS NULL OR @ParentClassName NOT IN ('TSBaseCxViewFrm', 'TSBaseCxView2Frm', 'TSBaseCxView3Frm', 'TSBaseCxTreeViewFrm', 'TSBaseCxTreeFrm', 'TSBaseCxTree2Frm')
      RaisError('Неверное значение параметра @ParentClassName = %s', 16, 1, @ParentClassName)

    IF @ParentClassName IN ('TSBaseCxTreeFrm', 'TSBaseCxTree2Frm') AND @ParentField IS NULL
      RaisError('Не задан обязательный параметр @ParentField', 16, 1)

    SET @Object_Id = OBJECT_ID(@ObjectName)
    IF @Object_Id IS NULL
      RaisError('Объект базы данных "%s" не обнаружен', 16, 1, @ObjectName)

    SET @Schema = OBJECT_SCHEMA_NAME(@Object_Id)
    SET @Name   = OBJECT_NAME(@Object_Id)
    SET @ObjectName = QuoteName(@Schema) + '.' + QuoteName(@Name)

    SELECT
      @Object_Id = CASE WHEN [type] IN ('U', 'V') THEN [object_id] END
    FROM [sys].[objects]
    WHERE [object_id] = @Object_Id
        AND
        [System].[Raise Error]
        (
          @@PROCID,
          CASE WHEN [type] NOT IN ('U', 'V', 'P') THEN 'Объект базы данных "' + @ObjectName + '" с типом "' + [type_desc] + '" не распознан' END
        ) IS NULL

    IF @@ROWCOUNT <> 1
      RaisError('Объект базы данных "%s" не распознан', 16, 1, @ObjectName)

    IF @IdentityField IS NULL
      SELECT
        @IdentityField      = [name],
        @IdentityFieldType  = [SQL].[Field Type Compile](TYPE_NAME([user_type_id]), [max_length], [precision], [scale], NULL),
        @IdentityMethod     = 'I'
      FROM [sys].[columns]
      WHERE [object_id] = @Object_Id AND [is_identity] = 1
    ELSE BEGIN
      SELECT
        @IdentityField      = [name],
        @IdentityFieldType  = [SQL].[Field Type Compile](TYPE_NAME([user_type_id]), [max_length], [precision], [scale], NULL),
        @IdentityMethod     = CASE WHEN [is_identity] = 1 THEN 'I' WHEN [is_computed] = 1 THEN 'C' WHEN [default_object_id] <> 0 THEN 'D' END
      FROM [sys].[columns]
      WHERE [object_id] = @Object_Id AND [name] = @IdentityField
          AND
          [System].[Raise Error]
          (
            @@PROCID,
            CASE WHEN [is_identity] = 0 AND [is_computed] = 0 AND [default_object_id] = 0 THEN 'Указанное вами Identity-поле "' + @IdentityField + '" не имеет признаков identity, computed или default' END
          ) IS NULL

      IF @@ROWCOUNT <> 1
        RaisError('Поле «%s» объекта «%s» не обнаружено', 16, 1, @IdentityField, @ObjectName)
    END

    IF @KeyField IS NULL BEGIN
      IF @IdentityField IS NULL
        RaisError('Значение параметра @KeyField не может быть определенно автоматически для объекта «%s»', 16, 1, @ObjectName)
      SET @KeyField       = @IdentityField
      SET @KeyFieldType   = @IdentityFieldType
      SET @KeyFieldUnique = 1
    END ELSE BEGIN
      SELECT
        @KeyField     = SC.[name],
        @KeyFieldType = [SQL].[Field Type Compile](TYPE_NAME([user_type_id]), [max_length], [precision], [scale], NULL)
      FROM [sys].[columns] SC WITH (NOLOCK)
      WHERE SC.[object_id] = @Object_Id AND SC.[name] = @KeyField

      IF @@ROWCOUNT <> 1
        RaisError('Поле «%s» объекта «%s» не обнаружено', 16, 1, @KeyField, @ObjectName)
      SET @KeyFieldUnique = [SQL].[Is Unique Column](@Object_Id, @KeyField)
    END

    SET @ObjectTranslateViewName = LEFT(@ObjectName, LEN(@ObjectName) - 1) + '(Translate)]'
    IF @Translation IS NULL BEGIN
      SET @Translation = CASE WHEN OBJECT_ID(@ObjectTranslateViewName, 'IF') IS NOT NULL THEN 1 ELSE 0 END
      IF @Translation = 1 BEGIN
        SET @SQL = 'SELECT @IdentityField = [KeyField] FROM ' + QuoteName(@Schema) + '.[Translate:Tables] WHERE [Name] = @Name'
        EXEC sp_executesql @SQL, N'@IdentityField SysName OUT,@Name SysName', @@IdentityField OUT, @Name
        IF @@IdentityField IS NULL OR @@IdentityField <> @IdentityField
          RaisError('При переводе таблицы "%s" указано отличное от текущего поле Identity = "%s"', 16, 1, @ObjectName, @@IdentityField)
      END
    END

    IF @Translation = 1 BEGIN
      SET @ObjectTranslateInsertName = LEFT(@ObjectName, LEN(@ObjectName) - 1) + '(Translate Insert)]'
      SET @ObjectTranslateUpdateName = LEFT(@ObjectName, LEN(@ObjectName) - 1) + '(Translate Update)]'
    END ELSE
      SET @ObjectTranslateViewName = NULL

    IF @ClassName IS NULL
      SET @ClassName = 'Tfrm' +  REPLACE(REPLACE(REPLACE(REPLACE(REPLACE(@ObjectName, '[', ''), ']', ''), ':', ''), '.', ''), ' ', '') + 'View'

    IF @ParentClassName IN ('TSBaseCxView2Frm', 'TSBaseCxView3Frm') BEGIN
      IF (@Object2Name IS NULL OR @MasterFields2 IS NULL OR @DetailFields2 IS NULL)
        RaisError('Не задан один или несколько обязательных параметров для работы DataSetProvider2: (@Object2Name IS NULL OR @MasterFields2 IS NULL OR @DetailFields2 IS NULL)', 16, 1)

      SET @Object2_Id = OBJECT_ID(@Object2Name)
      IF @@ROWCOUNT <> 1
        RaisError('Объект базы данных "%s" не обнаружен', 16, 1, @Object2Name)

      SET @Schema2 = OBJECT_SCHEMA_NAME(@Object2_Id)
      SET @Name2   = OBJECT_NAME(@Object2_Id)
      SET @Object2Name = QuoteName(@Schema2) + '.' + QuoteName(@Name2)
      IF @DirectDelete2 IS NULL
        SET @DirectDelete2 = 1

      SELECT
        @Object2_Id = CASE WHEN [type] IN ('U', 'V') THEN [object_id] END
      FROM [sys].[objects]
      WHERE [object_id] = @Object2_Id
          AND
          [System].[Raise Error]
          (
            @@PROCID,
            CASE WHEN [type] NOT IN ('U', 'V', 'P') THEN 'Объект базы данных "' + @Object2Name + '" с типом "' + [type_desc] + '" не распознан' END
          ) IS NULL

      IF @@ROWCOUNT <> 1
        RaisError('Объект базы данных "%s" не распознан', 16, 1, @Object2Name)

      IF @IdentityField2 IS NULL
        SELECT
          @IdentityField2     = [name],
          @IdentityField2Type = [SQL].[Field Type Compile](TYPE_NAME([user_type_id]), [max_length], [precision], [scale], NULL),
          @Identity2Method    = 'I'
        FROM [sys].[columns]
        WHERE [object_id] = @Object2_Id AND [is_identity] = 1
      ELSE BEGIN
        SELECT
          @IdentityField2     = [name],
          @IdentityField2Type = [SQL].[Field Type Compile](TYPE_NAME([user_type_id]), [max_length], [precision], [scale], NULL),
          @Identity2Method    = CASE WHEN [is_identity] = 1 THEN 'I' WHEN [is_computed] = 1 THEN 'C' WHEN [default_object_id] <> 0 THEN 'D' END
        FROM [sys].[columns]
        WHERE [object_id] = @Object2_Id AND [name] = @IdentityField2
            AND
            [System].[Raise Error]
            (
              @@PROCID,
              CASE WHEN [is_identity] = 0 AND [is_computed] = 0 AND [default_object_id] = 0 THEN 'Указанное вами Identity-поле "' + @IdentityField2 + '" не имеет признаков identity, computed или default' END
            ) IS NULL

        IF @@ROWCOUNT <> 1
          RaisError('Поле «%s» объекта «%s» не обнаружено', 16, 1, @IdentityField2, @Object2Name)
      END

      IF @KeyField2 IS NULL BEGIN
        IF @IdentityField2 IS NULL
          RaisError('Значение параметра @KeyField2 не может быть определенно автоматически для объекта «%s»', 16, 1, @Object2Name)
        SET @KeyField2 = @IdentityField2
      END ELSE IF @IdentityField2 IS NULL OR @IdentityField2 <> @KeyField2 BEGIN
        SELECT
          @KeyField2 = SC.[name]
        FROM [sys].[columns] SC WITH (NOLOCK)
        WHERE SC.[object_id] = @Object2_Id AND SC.[name] = @KeyField2

        IF @@ROWCOUNT <> 1
          RaisError('Поле «%s» объекта «%s» не обнаружено', 16, 1, @KeyField2, @Object2Name)
      END

      SET @Object2TranslateViewName = LEFT(@Object2Name, LEN(@Object2Name) - 1) + '(Translate)]'
      IF @Translation2 IS NULL BEGIN
        SET @Translation2 = CASE WHEN OBJECT_ID(@Object2TranslateViewName, 'IF') IS NOT NULL THEN 1 ELSE 0 END

        IF @Translation2 = 1 BEGIN
          SET @SQL = 'SELECT @IdentityField = [KeyField] FROM ' + QuoteName(@Schema2) + '.[Translate:Tables] WHERE [Name] = @Name'
          EXEC sp_executesql @SQL, N'@IdentityField SysName OUT,@Name SysName', @@IdentityField OUT, @Name2
          IF @@IdentityField IS NULL OR @@IdentityField <> @IdentityField2
            RaisError('При переводе таблицы "%s" указано отличное от текущего поле Identity = "%s"', 16, 1, @Object2Name, @@IdentityField)
        END
      END

      IF @Translation2 = 1 BEGIN
        SET @Object2TranslateInsertName = LEFT(@Object2Name, LEN(@Object2Name) - 1) + '(Translate Insert)]'
        SET @Object2TranslateUpdateName = LEFT(@Object2Name, LEN(@Object2Name) - 1) + '(Translate Update)]'
      END ELSE
        SET @Object2TranslateViewName = NULL

      IF @ParentClassName IN ('TSBaseCxView3Frm') BEGIN
        IF (@Object3Name IS NULL OR @MasterFields3 IS NULL OR @DetailFields3 IS NULL)
          RaisError('Не задан один или несколько обязательных параметров для работы DataSetProvider3: (@Object3Name IS NULL OR @MasterFields3 IS NULL OR @DetailFields3 IS NULL)', 16, 1)

        SET @Object3_Id = OBJECT_ID(@Object3Name)
        IF @@ROWCOUNT <> 1
          RaisError('Объект базы данных "%s" не обнаружен', 16, 1, @Object3Name)

        SET @Schema3 = OBJECT_SCHEMA_NAME(@Object3_Id)
        SET @Name3   = OBJECT_NAME(@Object3_Id)
        SET @Object3Name = QuoteName(@Schema3) + '.' + QuoteName(@Name3)
        IF @DirectDelete3 IS NULL
          SET @DirectDelete3 = 1

        SELECT
          @Object3_Id = CASE WHEN [type] IN ('U', 'V') THEN [object_id] END
        FROM [sys].[objects]
        WHERE [object_id] = @Object3_Id
            AND
            [System].[Raise Error]
            (
              @@PROCID,
              CASE WHEN [type] NOT IN ('U', 'V', 'P') THEN 'Объект базы данных "' + @Object3Name + '" с типом "' + [type_desc] + '" не распознан' END
            ) IS NULL

        IF @@ROWCOUNT <> 1
          RaisError('Объект базы данных "%s" не распознан', 16, 1, @Object3Name)

        IF @IdentityField3 IS NULL
          SELECT
            @IdentityField3     = [name],
            @IdentityField3Type = [SQL].[Field Type Compile](TYPE_NAME([user_type_id]), [max_length], [precision], [scale], NULL),
            @Identity3Method    = 'I'
          FROM [sys].[columns]
          WHERE [object_id] = @Object3_Id AND [is_identity] = 1
        ELSE BEGIN
          SELECT
            @IdentityField3     = [name],
            @IdentityField3Type = [SQL].[Field Type Compile](TYPE_NAME([user_type_id]), [max_length], [precision], [scale], NULL),
            @Identity3Method    = CASE WHEN [is_identity] = 1 THEN 'I' WHEN [is_computed] = 1 THEN 'C' WHEN [default_object_id] <> 0 THEN 'D' END
          FROM [sys].[columns]
          WHERE [object_id] = @Object3_Id AND [name] = @IdentityField3
              AND
              [System].[Raise Error]
              (
                @@PROCID,
                CASE WHEN [is_identity] = 0 AND [is_computed] = 0 AND [default_object_id] = 0 THEN 'Указанное вами Identity-поле "' + @IdentityField3 + '" не имеет признаков identity, computed или default' END
              ) IS NULL

          IF @@ROWCOUNT <> 1
            RaisError('Поле «%s» объекта «%s» не обнаружено', 16, 1, @IdentityField3, @Object3Name)
        END

        IF @KeyField3 IS NULL BEGIN
          IF @IdentityField3 IS NULL
            RaisError('Значение параметра @KeyField3 не может быть определенно автоматически для объекта «%s»', 16, 1, @Object3Name)
          SET @KeyField3 = @IdentityField3
        END ELSE IF @IdentityField3 IS NULL OR @IdentityField3 <> @KeyField3 BEGIN
          SELECT
            @KeyField3 = SC.[name]
          FROM [sys].[columns] SC WITH (NOLOCK)
          WHERE SC.[object_id] = @Object3_Id AND SC.[name] = @KeyField3

          IF @@ROWCOUNT <> 1
            RaisError('Поле «%s» объекта «%s» не обнаружено', 16, 1, @KeyField3, @Object3Name)
        END

        SET @Object3TranslateViewName = LEFT(@Object3Name, LEN(@Object3Name) - 1) + '(Translate)]'
        IF @Translation3 IS NULL BEGIN
          SET @Translation3 = CASE WHEN OBJECT_ID(@Object3TranslateViewName, 'IF') IS NOT NULL THEN 1 ELSE 0 END

          IF @Translation3 = 1 BEGIN
            SET @SQL = 'SELECT @IdentityField = [KeyField] FROM ' + QuoteName(@Schema3) + '.[Translate:Tables] WHERE [Name] = @Name'
            EXEC sp_executesql @SQL, N'@IdentityField SysName OUT,@Name SysName', @@IdentityField OUT, @Name3
            IF @@IdentityField IS NULL OR @@IdentityField <> @IdentityField3
              RaisError('При переводе таблицы "%s" указано отличное от текущего поле Identity = "%s"', 16, 1, @Object3Name, @@IdentityField)
          END
        END

        IF @Translation3 = 1 BEGIN
          SET @Object3TranslateInsertName = LEFT(@Object3Name, LEN(@Object3Name) - 1) + '(Translate Insert)]'
          SET @Object3TranslateUpdateName = LEFT(@Object3Name, LEN(@Object3Name) - 1) + '(Translate Update)]'
        END ELSE
          SET @Object3TranslateViewName = NULL
      END ELSE BEGIN
        SET @Object3Name = NULL
        SET @Translation3 = 0
        SET @DirectDelete3 = NULL
        SET @IsSecurityModeTable3 = NULL
      END
    END ELSE BEGIN
      SET @Object2Name = NULL
      SET @Translation2 = NULL
      SET @DirectDelete2 = NULL
      SET @IsSecurityModeTable2 = NULL

      SET @Object3Name = NULL
      SET @Translation3 = NULL
      SET @DirectDelete3 = NULL
      SET @IsSecurityModeTable3 = NULL
    END

    SET @@RoleName = 'Custom_' + REPLACE(REPLACE(@ObjectName, '[', ''), ']', '') + '_'
    SET @RoleView   = QuoteName(@@RoleName + 'View')
    SET @RoleModify = QuoteName(@@RoleName + 'Modify')
    SET @RoleDelete = QuoteName(@@RoleName + 'Delete')

    SET @@ObjectName = REPLACE(@ObjectName, ':', '::')
    SET @@ObjectName = LEFT(@@ObjectName, LEN(@@ObjectName) - 1)

    IF @ProcedureObjectsView IS NULL BEGIN
      SET @ProcedureObjectsView = @@ObjectName        + '::View]'
      SET @ProcedureObjectsViewDelphi = @@ObjectName  + '::View(Delphi)]'
    END ELSE
      SET @ProcedureObjectsViewDelphi = LEFT(@ProcedureObjectsView, LEN(@ProcedureObjectsView) - 1)  + '(Delphi)]'

    IF @IsSecurityModeProcedure = 1
      SET @@ProcedureRoleView = QuoteName('Custom_' + REPLACE(REPLACE(@ProcedureObjectsView, '[', ''), ']', ''))
    ELSE
      SET @@ProcedureRoleView = @RoleView

    SET @ObjectPrivileges_Id = (SELECT [Id] FROM [Security].[Dic:Privileges:Objects] WHERE [Name] = @ObjectName)
    IF @ObjectPrivileges_Id IS NOT NULL BEGIN
      SET @IsSecurityModeTable = 1
      SET @ObjectPrivilegesView = LEFT(@ObjectName, LEN(@ObjectName) - 1) + '(Privileges)]'
      IF OBJECT_ID(@ObjectPrivilegesView, 'V') IS NULL
        RaisError('Объект №1 присутствует в таблице псевдопривилегий, однако соответствующее представление (Privileges) не создано.', 16, 1)
    END

    SET @ObjectPrivileges_Id = (SELECT [Id] FROM [Security].[Dic:Privileges:Objects] WHERE [Name] = @ProcedureObjectsView)
    IF @ObjectPrivileges_Id IS NOT NULL BEGIN
      SET @IsSecurityModeTable = 1
      SET @IsSecurityModeProcedure = 1
    END

    IF @Object2Name IS NOT NULL BEGIN
      SET @Object2Privileges_Id = (SELECT [Id] FROM [Security].[Dic:Privileges:Objects] WHERE [Name] = @Object2Name)
      IF @Object2Privileges_Id IS NOT NULL BEGIN
        SET @IsSecurityModeTable2 = 1
        SET @Object2PrivilegesView = LEFT(@Object2Name, LEN(@Object2Name) - 1) + '(Privileges)]'
        IF OBJECT_ID(@Object2PrivilegesView, 'V') IS NULL
          RaisError('Объект №2 присутствует в таблице псевдопривилегий, однако соответствующее представление (Privileges) не создано.', 16, 1)
      END

      IF @Object3Name IS NOT NULL BEGIN
        SET @Object3Privileges_Id = (SELECT [Id] FROM [Security].[Dic:Privileges:Objects] WHERE [Name] = @Object3Name)
        IF @Object3Privileges_Id IS NOT NULL BEGIN
          SET @IsSecurityModeTable3 = 1
          SET @Object3PrivilegesView = LEFT(@Object3Name, LEN(@Object3Name) - 1) + '(Privileges)]'
          IF OBJECT_ID(@Object3PrivilegesView, 'V') IS NULL
            RaisError('Объект №3 присутствует в таблице псевдопривилегий, однако соответствующее представление (Privileges) не создано.', 16, 1)
        END
      END
    END

    -- [Schema].[Object::View] + [Schema].[Object::View(Delphi)]
    IF @ObjectNameSingle IS NOT NULL BEGIN
      SET @@ObjectNameSingle = REPLACE(@ObjectNameSingle, ':', '::')
      SET @@ObjectNameSingle = LEFT(@@ObjectNameSingle, LEN(@@ObjectNameSingle) - 1)
    END ELSE IF @ObjectViewClassName IS NOT NULL
      RaisError('Не указан обязательный параметр процедуры - @ObjectNameSingle', 16, 2)
    ELSE
      SET @@ObjectNameSingle = LEFT(@ProcedureObjectsView, LEN(@ProcedureObjectsView) - 1)

    IF @ObjectViewClassName IS NOT NULL BEGIN
      SET @ProcedureObjectView   = @@ObjectNameSingle  + '::View]'
      SET @ProcedureObjectViewDelphi = @@ObjectNameSingle  + '::View(Delphi)]'
    END
    SET @ProcedureObjectModify = @@ObjectNameSingle  + '::Modify]'
    SET @ProcedureObjectDelete = @@ObjectNameSingle  + '::Delete]'

    IF @ObjectViewIdentityParams IS NULL --AND @ObjectViewClassName IS NOT NULL -- Так нужно для Modify процедуры
      SET @ObjectViewIdentityParams = @IdentityParams

    -- [Schema].[Object2::View] + [Schema].[Object2::View(Delphi)]
    IF @Object2Name IS NOT NULL BEGIN
      IF @Object2NameSingle IS NOT NULL BEGIN
        SET @@Object2NameSingle = REPLACE(@Object2NameSingle, ':', '::')
        SET @@Object2NameSingle = LEFT(@@Object2NameSingle, LEN(@@Object2NameSingle) - 1)
      END ELSE IF @Object2ViewClassName IS NOT NULL
        RaisError('Не указан обязательный параметр процедуры - @Object2NameSingle', 16, 2)

      IF @Object2ViewClassName IS NOT NULL BEGIN
        SET @ProcedureObject2View   = @@Object2NameSingle  + '::View]'
        SET @ProcedureObject2ViewDelphi = @@Object2NameSingle  + '::View(Delphi)]'
      END
      SET @ProcedureObject2Modify = IsNull(@@Object2NameSingle  + '::Modify]', @ProcedureObjectModify + ';2')
      SET @ProcedureObject2Delete = IsNull(@@Object2NameSingle  + '::Delete]', @ProcedureObjectDelete + ';2')
    END

    -- [Schema].[Object3::View] + [Schema].[Object3::View(Delphi)]
    IF @Object3Name IS NOT NULL BEGIN
      IF @Object3NameSingle IS NOT NULL BEGIN
        SET @@Object3NameSingle = REPLACE(@Object3NameSingle, ':', '::')
        SET @@Object3NameSingle = LEFT(@@Object3NameSingle, LEN(@@Object3NameSingle) - 1)
      END ELSE IF @Object3ViewClassName IS NOT NULL
        RaisError('Не указан обязательный параметр процедуры - @Object3NameSingle', 16, 2)

      IF @Object3ViewClassName IS NOT NULL BEGIN
        SET @ProcedureObject3View   = @@Object3NameSingle  + '::View]'
        SET @ProcedureObject3ViewDelphi = @@Object3NameSingle  + '::View(Delphi)]'
      END
      SET @ProcedureObject3Modify = IsNull(@@Object3NameSingle  + '::Modify]', @ProcedureObjectModify + ';3')
      SET @ProcedureObject3Delete = IsNull(@@Object3NameSingle  + '::Delete]', @ProcedureObjectDelete + ';3')
    END

    -- Periodic
    SET @@Function = QuoteName(@Schema) + N'.[Periodic::Table::Id]'
    IF OBJECT_ID(@@Function) IS NOT NULL BEGIN
      EXEC @@Integer = @@Function @Name = @Name
      IF @@Integer IS NOT NULL BEGIN
        SET @@Function = QuoteName(@Schema) + N'.[Periodic::Fields Array]'
        EXEC @PeriodicFields = @@Function @Table_Id = @@Integer, @Delimeter = N','
        IF @PeriodicFields <> N'' BEGIN
          SET @Periodic = 1
          SET @SQL = N'SELECT @Name = SC.[name], @KeyField = PT.[KeyField] FROM ' + QuoteName(@Schema) + '.[Periodic:Tables] PT LEFT JOIN sys.columns SC WITH (NOLOCK) ON SC.[object_id] = ' + Cast(@Object_Id AS NVarChar) + N' AND PT.[DateFirstField] = SC.[name] AND SC.[is_nullable] = 0 WHERE PT.[Id] = ' + Cast(@@Integer AS NVarChar)
          EXEC sp_executesql @SQL, N'@Name SysName OUT, @KeyField SysName OUT', @PeriodicAgeFieldName OUT, @@IdentityField OUT
          IF @@IdentityField IS NULL OR @@IdentityField <> @IdentityField
            RaisError('При регистрации периодики таблицы "%s" указано отличное от текущего поле Identity = "%s"', 16, 1, @ObjectName, @@IdentityField)
        END
      END
    END

    -- Logging
    IF OBJECT_ID(QuoteName(@Schema) + N'.[Log::Begin]') IS NOT NULL
      SET @Logging = 1

    -- (Delphi);1
    EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];2
      @ClassName                  = @ClassName,
      @ParentClassName            = @ParentClassName,
      @ObjectName                 = @ObjectName,
      @Object2Name                = @Object2Name,
      @Object3Name                = @Object3Name,
      @KeyField                   = @KeyField,
      @IdentityField              = @IdentityField,
      @KeyField2                  = @KeyField2,
      @IdentityField2             = @IdentityField2,
      @KeyField3                  = @KeyField3,
      @IdentityField3             = @IdentityField3,
      @ParentField                = @ParentField,
      @MasterFields2              = @MasterFields2,
      @MasterFields3              = @MasterFields3,
      @DetailFields2              = @DetailFields2,
      @DetailFields3              = @DetailFields3,
      @IdentityParams             = @IdentityParams,
      @ProcedureObjectsView       = @ProcedureObjectsView,
      @InsertableControl          = @InsertableControl,
      @ObjectViewClassName        = @ObjectViewClassName,
      @Object2ViewClassName       = @Object2ViewClassName,
      @Object3ViewClassName       = @Object3ViewClassName,
      @DirectDelete               = @DirectDelete,
      @DirectDelete2              = @DirectDelete2,
      @DirectDelete3              = @DirectDelete3,
      @Translation                = @Translation,
      @Translation2               = @Translation2,
      @Translation3               = @Translation3,
      @Category                   = @Category,
      @Caption                    = @Caption,
      @Object_Id                  = @Object_Id,
      @Object2_Id                 = @Object2_Id,
      @Object3_Id                 = @Object3_Id,
      @ProcedureObjectsViewDelphi = @ProcedureObjectsViewDelphi,
      --@ProcedureObjectViewDelphi  = @ProcedureObjectViewDelphi,
      @ProcedureObjectView        = @ProcedureObjectView,
      @ProcedureObjectModify      = @ProcedureObjectModify,
      @ProcedureObject2Modify     = @ProcedureObject2Modify,
      @ProcedureObject3Modify     = @ProcedureObject3Modify,
      @ProcedureObjectDelete      = @ProcedureObjectDelete,
      @ProcedureObject2Delete     = @ProcedureObject2Delete,
      @ProcedureObject3Delete     = @ProcedureObject3Delete,
      @IsSecurityModeTable        = @IsSecurityModeTable,
      @IsSecurityModeTable2       = @IsSecurityModeTable2,
      @IsSecurityModeTable3       = @IsSecurityModeTable3,
      @ObjectPrivilegesView       = @ObjectPrivilegesView,
      @Object2PrivilegesView      = @Object2PrivilegesView,
      @Object3PrivilegesView      = @Object3PrivilegesView,

      @OUT_Object                 = @OUT_Object   OUT,
      @OUT_Message                = @OUT_Message  OUT

    INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)

    -- (Delphi);3
    IF @IdentityParams <> '' BEGIN
      EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];3
        @ClassName                  = @ClassName,
        @ObjectName                 = @ObjectName,
        @IdentityParams             = @IdentityParams,
        @ProcedureObjectsView       = @ProcedureObjectsView,
        @Category                   = @Category,
        @Caption                    = @Caption,
        @ProcedureObjectsViewDelphi = @ProcedureObjectsViewDelphi,
        @IsSecurityModeTable        = @IsSecurityModeTable,
        @IsSecurityModeProcedure    = @IsSecurityModeProcedure,

        @OUT_Object                 = @OUT_Object   OUT,
        @OUT_Message                = @OUT_Message  OUT

      INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)
    END

    -- (Delphi);4
    EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];4
      @ObjectName                 = @ObjectName,
      @ClassName                  = @ClassName,
      @IdentityParams             = @IdentityParams,
      @ProcedureObjectsView       = @ProcedureObjectsView,
      @Category                   = @Category,
      @Caption                    = @Caption,
      @ProcedureObjectsViewDelphi = @ProcedureObjectsViewDelphi,

      @OUT_Object                 = @OUT_Object   OUT,
      @OUT_Message                = @OUT_Message  OUT

    INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)

    -- (Delphi);6
    IF @ParentClassName IN ('TSBaseCxTreeViewFrm', 'TSBaseCxTree2Frm') BEGIN
      EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];5
        @ClassName                  = @ClassName,
        @Translation                = @Translation,
        @ProcedureObjectsViewDelphi = @ProcedureObjectsViewDelphi,
        @IdentityParams             = @IdentityParams,
        @Category                   = @Category,
        @Caption                    = @Caption,

        @OUT_Object                 = @OUT_Object   OUT,
        @OUT_Message                = @OUT_Message  OUT

      INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)
    END

    PRINT 'GRANT EXEC ON ' + @ProcedureObjectsViewDelphi + ' TO ' + @@ProcedureRoleView
    PRINT 'GO'

    -- Views;1
    EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];6
      @ClassName                  = @ClassName,
      @ObjectName                 = @ObjectName,
      @IdentityParams             = @IdentityParams,
      @ProcedureObjectsView       = @ProcedureObjectsView,
      @IsSecurityModeTable        = @IsSecurityModeTable,
      @IsSecurityModeProcedure    = @IsSecurityModeProcedure,
      @Category                   = @Category,
      @Caption                    = @Caption,

      @OUT_Object                 = @OUT_Object   OUT,
      @OUT_Message                = @OUT_Message  OUT

    INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)

    -- Views;2
    EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];7
      @ClassName                  = @ClassName,
      @ObjectName                 = @ObjectName,
      @Object2Name                = @Object2Name,
      @Object_Id                  = @Object_Id,
      @IdentityField              = @IdentityField,
      @IdentityFieldType          = @IdentityFieldType,
      @KeyField                   = @KeyField,
      @KeyFieldType               = @KeyFieldType,
      @IdentityParams             = @IdentityParams,
      @ProcedureObjectsView       = @ProcedureObjectsView,
      @InsertableControl          = @InsertableControl,
      @Translation                = @Translation,

      @MasterFields2              = @MasterFields2,
      @DetailFields2              = @DetailFields2,

      @ObjectTranslateViewName    = @ObjectTranslateViewName,
      @ObjectPrivilegesView       = @ObjectPrivilegesView,
      @ObjectViewClassName        = @ObjectViewClassName,
      @IsSecurityModeTable        = @IsSecurityModeTable,
      @IsSecurityModeTable2       = @IsSecurityModeTable2,
      @IsSecurityModeProcedure    = @IsSecurityModeProcedure,
      @Category                   = @Category,
      @Caption                    = @Caption,

      @OUT_Object                 = @OUT_Object   OUT,
      @OUT_Message                = @OUT_Message  OUT

    INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)

    PRINT 'GRANT EXEC ON ' + @ProcedureObjectsView + ' TO ' + @@ProcedureRoleView
    PRINT 'GO'

    -- Views;3
    IF @Object2Name IS NOT NULL BEGIN
      EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];8
        @ClassName                = @ClassName,
        @Object2Name              = @Object2Name,
        @Object2_Id               = @Object2_Id,
        @DetailFields2            = @DetailFields2,
        @IdentityField2           = @IdentityField2,
        @IdentityField2Type       = @IdentityField2Type,
        @KeyField2                = @KeyField,
        @KeyField2Type            = @KeyFieldType,
        @IdentityParams           = @IdentityParams,
        @ProcedureObjectsView     = @ProcedureObjectsView,
        @Translation2             = @Translation2,

        @Object2TranslateViewName = @Object2TranslateViewName,
        @Object2PrivilegesView    = @Object2PrivilegesView,
        @Object2ViewClassName     = @Object2ViewClassName,
        @IsSecurityModeTable2     = @IsSecurityModeTable2,
        @IsSecurityModeProcedure  = @IsSecurityModeProcedure,
        @Category                 = @Category,
        @Caption                  = @Caption,

        @OUT_Object               = @OUT_Object   OUT,
        @OUT_Message              = @OUT_Message  OUT

      INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)
    END

    -- Views;4
    IF @Object3Name IS NOT NULL BEGIN
      EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];9
        @ClassName                = @ClassName,
        @Object3Name              = @Object3Name,
        @Object3_Id               = @Object3_Id,
        @DetailFields3            = @DetailFields3,
        @IdentityField3           = @IdentityField3,
        @IdentityField3Type       = @IdentityField3Type,
        @KeyField3                = @KeyField,
        @KeyField3Type            = @KeyFieldType,
        @IdentityParams           = @IdentityParams,
        @ProcedureObjectsView     = @ProcedureObjectsView,
        @Translation3             = @Translation3,

        @Object3TranslateViewName = @Object3TranslateViewName,
        @Object3PrivilegesView    = @Object3PrivilegesView,
        @Object3ViewClassName     = @Object3ViewClassName,
        @IsSecurityModeTable3     = @IsSecurityModeTable3,
        @IsSecurityModeProcedure  = @IsSecurityModeProcedure,
        @Category                 = @Category,
        @Caption                  = @Caption,

        @OUT_Object               = @OUT_Object   OUT,
        @OUT_Message              = @OUT_Message  OUT

      INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)
    END

    IF @Object_Id IS NOT NULL BEGIN
      IF @ProcedureObjectView IS NOT NULL BEGIN
        -- View(Delphi);1
        EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];10
          @ObjectName                 = @ObjectName,
          @Object_Id                  = @Object_Id,
          @KeyField                   = @KeyField,
          @IdentityField              = @IdentityField,
          @IdentityFieldType          = @IdentityFieldType,
          @MasterFields2              = @MasterFields2,
          @DetailFields2              = @DetailFields2,
          @ObjectViewClassName        = @ObjectViewClassName,
          @ObjectViewIdentityParams   = @ObjectViewIdentityParams,
          @ObjectPrivilegesView       = @ObjectPrivilegesView,
          @CaptionFormatSingle        = @CaptionFormatSingle,
          @ProcedureObjectViewDelphi  = @ProcedureObjectViewDelphi,
          @ProcedureObjectView        = @ProcedureObjectView,
          @ProcedureObjectModify      = @ProcedureObjectModify,
          @Translation                = @Translation,
          @PeriodicFields             = @PeriodicFields,
          @PeriodicAgeFieldName       = @PeriodicAgeFieldName,
          @IsSecurityModeTable        = @IsSecurityModeTable,
          @Category                   = @Category,
          @Caption                    = @Caption,

          @OUT_Object                 = @OUT_Object   OUT,
          @OUT_Message                = @OUT_Message  OUT

        INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)
        PRINT 'GRANT EXEC ON ' + @ProcedureObjectViewDelphi + ' TO ' + @@ProcedureRoleView
        PRINT 'GO'

        -- View;1
        EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];11
          @ObjectName                 = @ObjectName,
          --@KeyField                   = @KeyField,
          @ProcedureObjectView        = @ProcedureObjectView,
          @ObjectViewClassName        = @ObjectViewClassName,
          @ObjectViewIdentityParams   = @ObjectViewIdentityParams,
          @IsSecurityModeTable        = @IsSecurityModeTable,
          @Category                   = @Category,
          @Caption                    = @Caption,

          @OUT_Object                 = @OUT_Object   OUT,
          @OUT_Message                = @OUT_Message  OUT

        INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)

      -- View;2
        EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];12
          @Schema                     = @Schema,
          @Name                       = @Name,
          @ObjectName                 = @ObjectName,
          @Object_Id                  = @Object_Id,
          @KeyField                   = @KeyField,
          @KeyFieldType               = @KeyFieldType,
          @IdentityField              = @IdentityField,
          @IdentityFieldType          = @IdentityFieldType,
          @ProcedureObjectView        = @ProcedureObjectView,
          @Translation                = @Translation,
          @ObjectViewClassName        = @ObjectViewClassName,
          @ObjectTranslateViewName    = @ObjectTranslateViewName,
          @ObjectPrivilegesView       = @ObjectPrivilegesView,
          @ObjectViewIdentityParams   = @ObjectViewIdentityParams,
          @IsSecurityModeTable        = @IsSecurityModeTable,
          @PeriodicAgeFieldName       = @PeriodicAgeFieldName,
          @PeriodicFields             = @PeriodicFields,

          @Category                   = @Category,
          @Caption                    = @Caption,

          @OUT_Object                 = @OUT_Object   OUT,
          @OUT_Message                = @OUT_Message  OUT

        INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)

        IF @PeriodicFields IS NOT NULL BEGIN
          -- View;3
          EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];13
            @Schema                     = @Schema,
            @Name                       = @Name,
            @ObjectName                 = @ObjectName,
            @Object_Id                  = @Object_Id,
            @KeyField                   = @KeyField,
            @KeyFieldType               = @KeyFieldType,
            @KeyFieldUnique             = @KeyFieldUnique,
            @IdentityField              = @IdentityField,
            @IdentityFieldType          = @IdentityFieldType,
            @ProcedureObjectView        = @ProcedureObjectView,
            @Translation                = @Translation,
            @ObjectViewClassName        = @ObjectViewClassName,
            @ObjectPrivilegesView       = @ObjectPrivilegesView,
            @IsSecurityModeTable        = @IsSecurityModeTable,
            @PeriodicFields             = @PeriodicFields,

            @Category                   = @Category,
            @Caption                    = @Caption,

            @OUT_Object                 = @OUT_Object   OUT,
            @OUT_Message                = @OUT_Message  OUT

          INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)
        END

        PRINT 'GRANT EXEC ON ' + @ProcedureObjectView + ' TO ' + @@ProcedureRoleView
        PRINT 'GO'
      END

      -- Modify
      EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];14
        @Schema                     = @Schema,
        @Name                       = @Name,
        @ClassName                  = @ClassName,
        @ObjectName                 = @ObjectName,
        @Object_Id                  = @Object_Id,
        @KeyField                   = @KeyField,
        @KeyFieldType               = @KeyFieldType,
        @KeyFieldUnique             = @KeyFieldUnique,
        @IdentityMethod             = @IdentityMethod,
        @IdentityField              = @IdentityField,
        @IdentityFieldType          = @IdentityFieldType,
        @ProcedureObjectModify      = @ProcedureObjectModify,
        @Translation                = @Translation,
        @ObjectTranslateInsertName  = @ObjectTranslateInsertName,
        @ObjectTranslateUpdateName  = @ObjectTranslateUpdateName,
        @ObjectPrivilegesView       = @ObjectPrivilegesView,
        @ObjectViewClassName        = @ObjectViewClassName,
        @ObjectViewIdentityParams   = @ObjectViewIdentityParams,
        @IsSecurityModeTable        = @IsSecurityModeTable,
        @PeriodicAgeFieldName       = @PeriodicAgeFieldName,
        @PeriodicFields             = @PeriodicFields,

        @Category                   = @Category,
        @Caption                    = @Caption,

        @Logging                    = @Logging,

        @OUT_Object                 = @OUT_Object   OUT,
        @OUT_Message                = @OUT_Message  OUT

      INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)

      PRINT 'GRANT EXEC ON ' + @ProcedureObjectModify + ' TO ' + @RoleModify
      PRINT 'GO'
    END

    IF @DirectDelete = 0 BEGIN
      -- Delete
      EXEC [SQL].[Default::Delphi::TSBaseXXViewFrm];15
        @Schema                     = @Schema,
        @ObjectName                 = @ObjectName,
        @IdentityParams             = @IdentityParams,
        @Object_Id                  = @Object_Id,
        @IdentityField              = @IdentityField,
        @IdentityFieldType          = @IdentityFieldType,
        @KeyField                   = @KeyField,
        @KeyFieldType               = @KeyFieldType,
        @KeyFieldUnique             = @KeyFieldUnique,
        @ProcedureObjectDelete      = @ProcedureObjectDelete,
        @ObjectViewClassName        = @ObjectViewClassName,
        @ObjectPrivilegesView       = @ObjectPrivilegesView,
        --@IsSecurityModeTable        = @IsSecurityModeTable,

        @Category                   = @Category,
        @Caption                    = @Caption,

        @Logging                    = @Logging,

        @OUT_Object                 = @OUT_Object   OUT,
        @OUT_Message                = @OUT_Message  OUT

      INSERT INTO @Results([Object], [Message]) VALUES(@OUT_Object, @OUT_Message)

      PRINT 'GRANT EXEC ON ' + @ProcedureObjectDelete + ' TO ' + @RoleDelete
      PRINT 'GO'
    END ELSE IF @Object_Id IS NOT NULL BEGIN
      PRINT 'GRANT SELECT ON ' + CASE WHEN @IsSecurityModeTable = 1 THEN @ObjectPrivilegesView ELSE @ObjectName END + '(' + QuoteName(@KeyField) + ') TO ' + @RoleDelete
      IF @IdentityParams <> '' BEGIN
        SELECT
          @SQL = N'GRANT SELECT ON ' + CASE WHEN @IsSecurityModeTable = 1 THEN @ObjectPrivilegesView ELSE @ObjectName END
                + N'('
                + [Pub].[ConCat](QuoteName([Name]),', ')
                + N') TO '
                + @RoleDelete
        FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')

        EXEC [SQL].[Print] @Text = @SQL
      END

      PRINT 'GRANT DELETE ON ' + CASE WHEN @IsSecurityModeTable = 1 THEN @ObjectPrivilegesView ELSE @ObjectName END + ' TO ' + @RoleDelete
      PRINT 'GO'
    END

    SELECT * FROM @Results

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID
  END CATCH
GO
-- (Delphi);1
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];2
  @ClassName                  VarChar(256),
  @ParentClassName            VarChar(256),

  @ObjectName                 SysName,
  @Object2Name                SysName,
  @Object3Name                SysName,

  @KeyField                   SysName,
  @IdentityField              SysName,
  @KeyField2                  SysName,
  @IdentityField2             SysName,
  @KeyField3                  SysName,
  @IdentityField3             SysName,

  @ParentField                SysName,
  @MasterFields2              VarChar(512),
  @MasterFields3              VarChar(512),
  @DetailFields2              VarChar(512),
  @DetailFields3              VarChar(512),

  @IdentityParams             VarChar(256),

  @ProcedureObjectsView       SysName,
  @ObjectViewClassName        VarChar(256),
  @Object2ViewClassName       VarChar(256),
  @Object3ViewClassName       VarChar(256),

  @DirectDelete               Bit,
  @DirectDelete2              Bit,
  @DirectDelete3              Bit,

  @Translation                Bit,
  @Translation2               Bit,
  @Translation3               Bit,

  @Category                   VarChar(256),
  @Caption                    VarChar(256),

  @Object_Id                  Int,
  @Object2_Id                 Int,
  @Object3_Id                 Int,

  @ProcedureObjectsViewDelphi SysName,
  --@ProcedureObjectViewDelphi  SysName,
  @ProcedureObjectView        SysName,
  @InsertableControl          Bit,

  @ProcedureObjectModify      SysName,
  @ProcedureObject2Modify     SysName,
  @ProcedureObject3Modify     SysName,

  @ProcedureObjectDelete      SysName,
  @ProcedureObject2Delete     SysName,
  @ProcedureObject3Delete     SysName,

  @IsSecurityModeTable        Bit,
  @IsSecurityModeTable2       Bit,
  @IsSecurityModeTable3       Bit,

  @ObjectPrivilegesView       SysName,
  @Object2PrivilegesView      SysName,
  @Object3PrivilegesView      SysName,

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @@IdentityField     SysName,
    @@IdentityField2    SysName,
    @@IdentityField3    SysName,
    @SQL                NVarChar(Max),
    @SQLDelete          NVarChar(Max),
    @SQLDelete2         NVarChar(Max),
    @SQLDelete3         NVarChar(Max)
  
  BEGIN TRY
    SET @@IdentityField  = IsNull(@IdentityField, @KeyField)
    SET @@IdentityField2 = IsNull(@IdentityField2, @KeyField2)
    SET @@IdentityField3 = IsNull(@IdentityField3, @KeyField3)

    SELECT @SQLDelete =
      CASE
        WHEN @DirectDelete = 1 THEN
          N'DELETE '
          + CASE WHEN @IsSecurityModeTable = 1 THEN @ObjectPrivilegesView ELSE @ObjectName END
          + N' WHERE '
          + IsNull([Pub].[ConCat](QuoteName([Name]) + N' = :' + Name + N' AND ', N''), N'')
          +
          CASE
            WHEN @IdentityField <> N'' THEN QuoteName(@IdentityField) + N' = :OLD_' + @IdentityField
            ELSE QuoteName(@KeyField) + N' = :OLD_' + @KeyField
          END
        ELSE
          N'EXEC ' + @ProcedureObjectDelete
          +
          CASE
            WHEN @ObjectViewClassName IS NOT NULL THEN N' @Interface = :[Interface:Language]'
            ELSE N'''''
      ''''  @Interface            = :[Interface:Language]'
          END
          +
          IsNull
          (
            [Pub].[ConCat]
            (
              CASE
                WHEN @ObjectViewClassName IS NOT NULL THEN N', @' + [Name] + N' = :' + [Name]
                ELSE N',''''
      ''''  @'
              + [Name]
              + Replicate(N' ', [Pub].[Is Negative Int](20 - Len([Name]), 0))
              + N' = :' + [Name]
              END,
              N''
            ),
            N''
          )
          +
          CASE
            WHEN @ObjectViewClassName IS NOT NULL THEN N', @' + @@IdentityField + N' = :OLD_' + @@IdentityField
            ELSE N',''''
      ''''  @' + @@IdentityField + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(@@IdentityField), 0)) + N' = :OLD_' + @@IdentityField
          END
      END
    FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')

    SELECT @SQLDelete2 =
      CASE
        WHEN @DirectDelete2 = 1 THEN
          N'DELETE '
          + CASE WHEN @IsSecurityModeTable2 = 1 THEN @Object2PrivilegesView ELSE @Object2Name END
          + N' WHERE '
          + IsNull([Pub].[ConCat](QuoteName([Name]) + N' = :' + Name + N' AND ', N''), N'')
          +
          CASE
            WHEN @IdentityField2 <> N'' THEN QuoteName(@IdentityField2) + N' = :OLD_' + @IdentityField2
            ELSE QuoteName(@KeyField2) + N' = :OLD_' + @KeyField2
          END
        ELSE
          N'EXEC ' + @ProcedureObjectDelete
          +
          CASE
            WHEN @Object2ViewClassName IS NOT NULL THEN N' @Interface = :[Interface:Language]'
            ELSE N'''''
      ''''  @Interface            = :[Interface:Language]'
          END
          +
          IsNull
          (
            [Pub].[ConCat]
            (
              CASE
                WHEN @Object2ViewClassName IS NOT NULL THEN N', @' + [Name] + N' = :' + [Name]
                ELSE N',''''
      ''''  @'
              + [Name]
              + Replicate(N' ', [Pub].[Is Negative Int](20 - Len([Name]), 0))
              + N' = :' + [Name]
              END,
              N''
            ),
            N''
          )
          +
          CASE
            WHEN @Object2ViewClassName IS NOT NULL THEN N', @' + @@IdentityField2 + N' = :OLD_' + @@IdentityField2
            ELSE N',''''
      ''''  @' + @@IdentityField2 + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(@@IdentityField2), 0)) + N' = :OLD_' + @@IdentityField2
          END
      END
    FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')

    SELECT @SQLDelete3 =
      CASE
        WHEN @DirectDelete3 = 1 THEN
          N'DELETE '
          + CASE WHEN @IsSecurityModeTable3 = 1 THEN @Object3PrivilegesView ELSE @Object3Name END
          + N' WHERE '
          + IsNull([Pub].[ConCat](QuoteName([Name]) + N' = :' + Name + N' AND ', N''), N'')
          +
          CASE
            WHEN @IdentityField3 <> N'' THEN QuoteName(@IdentityField3) + N' = :OLD_' + @IdentityField3
            ELSE QuoteName(@KeyField3) + N' = :OLD_' + @KeyField3
          END
        ELSE
          N'EXEC ' + @ProcedureObjectDelete
          +
          CASE
            WHEN @Object3ViewClassName IS NOT NULL THEN N' @Interface = :[Interface:Language]'
            ELSE N'''''
      ''''  @Interface            = :[Interface:Language]'
          END
          +
          IsNull
          (
            [Pub].[ConCat]
            (
              CASE
                WHEN @Object3ViewClassName IS NOT NULL THEN N', @' + [Name] + N' = :' + [Name]
                ELSE N',''''
      ''''  @'
              + [Name]
              + Replicate(N' ', [Pub].[Is Negative Int](30 - Len([Name]), 0))
              + N' = :' + [Name]
              END,
              N''
            ),
            N''
          )
          +
          CASE
            WHEN @Object3ViewClassName IS NOT NULL THEN N', @' + @@IdentityField3 + N' = :OLD_' + @@IdentityField3
            ELSE N',''''
      ''''  @' + @@IdentityField3 + Replicate(N' ', [Pub].[Is Negative Int](30 - Len(@@IdentityField3), 0)) + N' = :OLD_' + @@IdentityField3
          END
      END
    FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')

    SET @OUT_Object = @ProcedureObjectsViewDelphi
    SET @SQL =
N'--===============================================================================================================
-- <Назначение>:        Процедура регистрации DFM для журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Меню>:              ;
-- <Класс формы>:       ' + @ClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*
  EXEC ' + @OUT_Object + N' @Interface = ''RU''
*/
--===============================================================================================================
CREATE PROCEDURE '
      + @OUT_Object + N'
  @Interface  Char(2),
  @Debug      Bit       = 0
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @Privileges SmallInt,
    @Script     NVarChar(Max),
    @DFM        NVarChar(Max)'
      +
      CASE
        WHEN @Translation = 1 OR @Translation2 = 1 OR @Translation3 = 1 OR @InsertableControl = 1 THEN N',
    @Languages  NVarChar(512),

    @DFM_ViewMenuItems      NVarChar(Max),
    @DFM_ViewItemReactions  NVarChar(Max),
    @DFM_ViewSubMenu        NVarChar(Max)'
        ELSE N''
      END
      + N'

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)

    EXECUTE AS CALLER
      IF ORIGINAL_LOGIN() <> SYSTEM_USER
        RaisError(''Вызывающим процедуру пользователем должен быть инициатор соединения.'', 16, 2)
      SET @Privileges = 256'
      +
      CASE
        WHEN @ObjectViewClassName IS NOT NULL THEN '
                            | CASE WHEN [System].[Execute Privilege](' + [Pub].[Quote String](@ProcedureObjectView) + ') = 1 THEN 1 ELSE 0 END'
        ELSE N''
      END
      + N'
                            | CASE WHEN [System].[Execute Privilege](' + [Pub].[Quote String](@ProcedureObjectModify) + ') = 1 THEN ' + CASE WHEN @InsertableControl = 1 THEN N'4' ELSE N'6' END + N' ELSE 0 END
                            | '
      +
      CASE
        WHEN @DirectDelete = 1 THEN N'([System].[Table::Privileges](' + [Pub].[Quote String](@ObjectName) + ') & 8)'
        ELSE N'CASE WHEN [System].[Execute Privilege](' + [Pub].[Quote String](@ProcedureObjectDelete) + ') = 1 THEN 8 ELSE 0 END'
      END
      + N'
    REVERT'

      +
      CASE
        WHEN @Translation = 1 OR @Translation2 = 1 OR @Translation3 = 1 OR @InsertableControl = 1 THEN
          N'

    SELECT
      @Languages =  [Pub].[Concat]
                    (
                      [Pub].[Quote String]([Code] + N''='' + [Name]),
                      N''
''
                    )
    FROM
    (
      SELECT TOP 100
        [Code],
        [Name]
      FROM [System].[Languages]
      ORDER BY [Priority]
    ) L

    SET @Script = N'''
          +
          CASE
            WHEN @Translation = 1 OR @Translation2 = 1 OR @Translation3 = 1 THEN
              CASE
                WHEN @ParentClassName IN ('TSBaseCxTreeViewFrm', 'TSBaseCxTree2Frm') THEN N'
procedure InternalCreate;
begin
  with Form do begin
    flcbLanguages.OnFilterValueChanged := @flcbLanguagesOnFilterValueChanged;
  end;
end;'
                ELSE N''
              END
              + N'
procedure InternalInit;
var
  LIdx: Integer;
begin
  with Form do begin
    if VarIsPresent(flcbLanguages.ParamValue) then
      flcbLanguages.Enabled := False
    else
      flcbLanguages.ParamValue := '' + [Pub].[Quote String](@Interface) + N'';
  end;
end;
'
              +
              CASE
                WHEN @ParentClassName IN ('TSBaseCxTreeViewFrm', 'TSBaseCxTree2Frm') THEN N'
procedure flcbLanguagesOnFilterValueChanged;
begin
  with Form do begin
    GroupSetProvider.ReOpen;
  end;
end;
' 
                ELSE N''
              END
            ELSE N''
          END
          +
          CASE
            WHEN @InsertableControl = 1 THEN N'
procedure DBGridAfterDataSetOpen;
begin
  with Form do begin
    if DataSetProvider.Params.ParamByName(''''Privilege:Insertable'''').AsBoolean then
      RecordsPrivileges := RecordsPrivileges or 2
    else
      RecordsPrivileges := RecordsPrivileges and $FFFD;
  end
end;
'
            ELSE N''
          END
          + N''''
        ELSE N''
      END
      + N'

    SET @DFM = N'''
        +
        CASE
          WHEN @IdentityParams <> '' THEN N''
          ELSE N'
  Caption = '' + [Pub].[Quote String]('
            + CASE WHEN @Category IS NOT NULL THEN '[GUI].[Delphi::Translate](''MAINMENU'', ' + [Pub].[Quote String](@Category) + ', @Interface) + '' :: '' + ' ELSE N'' END
            + '[GUI].[Delphi::Translate](''MAINMENU'', ' + [Pub].[Quote String](@Caption) + ', @Interface)) + N'''
        END
        + N'
  IconIndex = '' + Cast([GUI].[Delphi::IconLists::Get Icon Index](''Icon16List'', ''DICTIONARY'') AS NVarChar) + N''
  Width = 1000
  Height = 500

  MDIToolButtons = [mtbRefresh, mtbExcel]
  StoreFormSettings = [frsvLeftTop, frsvWidthHeight, frsvFormItems, frsvUserDefined]'

        +
        CASE
          WHEN @IdentityParams <> '' THEN
          (
            SELECT
              N'

  RequireCreateFormParams = ' + [Pub].[Deep Quote String]([Pub].[ConCat]([Name], ';'), 2) + N'
  KeepCreateFormParams    = ' + [Pub].[Deep Quote String]([Pub].[ConCat]([Name], ';'), 2)
            FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
          )
          ELSE N''
        END
        +
        CASE
          WHEN @ClassName IN ('TSBaseCxTreeViewFrm', 'TSBaseCxTree2Frm') THEN N'
  AutoLocalFilter = True'
          ELSE N''
        END
        +
        CASE
          WHEN @Translation = 1 THEN N'''

        + [GUI].[Delphi::DFM::TSQLDataSetProvider](''[System].[Languages]'', ''DataSetLanguages'', ''bdsoCDSCached'', Default, Default, 1, ''DataSourceLanguages'')

        + '''
          ELSE N'''

  --      + [GUI].[Delphi::DFM::TSQLDataSetProvider](''[Schema].[Table]'', ''DataSetTable'', ''bdsoCDSCached'', Default, Default, 1, ''DataSourceTable'')

        + '''
        END

        +
        CASE
          WHEN @ParentClassName IN ('TSBaseCxTreeViewFrm', 'TSBaseCxTree2Frm') THEN N'
  inherited GroupSetProvider: TSQLDataSetProvider
    SQL.Strings = (''''EXEC ' + @ProcedureObjectsViewDelphi + N';7 @Interface = :[Interface:Language]' + CASE WHEN @Translation = 1 THEN N', @Language = :Language' ELSE N'' END
            -- + (SELECT IsNull([Pub].[Concat](N', @' + [Name] + N' = :' + [Name], N', '), N'') FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';'))
            + N''''')
  end'
          ELSE N''
        END

      + N'

  inherited DataSetProvider: TSQLDataSetProvider'
      + N'
    KeyFieldNames = ' + [Pub].[Deep Quote String](@@IdentityField, 2)
      +
      CASE
        WHEN @InsertableControl = 1 THEN N'
    Params =
    <
      item
        DataType  = ftBoolean
        Name      = ''''Privilege:Insertable''''
        ParamType = ptOutput
      end    
    >'
        ELSE N''
      END
      + N'
    SQL.Strings =
    (
      ''''EXEC ' + @ProcedureObjectsView + N';2''''
      ''''  @Interface            = :[Interface:Language]'
      +
      CASE
        WHEN @Translation = 1 THEN N',''''
      ''''  @Language             = :Language'
        ELSE N''
      END
      +
      (
        SELECT
          IsNull
          (
            [Pub].[ConCat]
            (
              N',''''
      ''''  @' + [Name] + Replicate(N' ', [Pub].[Is Negative Int](20 - Len([Name]), 0)) + N' = :' + Name,
              N''
            ),
            N''
          )
        FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
      )
      +
      CASE
        WHEN @InsertableControl = 1 THEN N',''''
      ''''  @PrivilegeInsertable  = :[Privilege:Insertable] OUT'
        ELSE N''
      END
      + N'''''
    )
    SQLRefresh.Strings =
    (
      ''''EXEC ' + @ProcedureObjectsView + N';2''''
      ''''  @Interface            = :[Interface:Language]'
      +
      CASE
        WHEN @Translation = 1 THEN N',''''
      ''''  @Language             = :Language'
        ELSE N''
      END
      +
      (
        SELECT
          IsNull
          (
            [Pub].[ConCat]
            (
              N',''''
      ''''  @' + [Name] + Replicate(N' ', [Pub].[Is Negative Int](20 - Len([Name]), 0)) + N' = :' + Name,
              ''
            ),
            N''
          )
        FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
      )
      + ',''''
      ''''  @' + @@IdentityField + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(@@IdentityField), 0)) + ' = :' + @@IdentityField + '''''
    )'
      +
      CASE
        WHEN @ObjectViewClassName IS NULL THEN N'

    CachedUpdates = False
    FieldsAutomations.ModifiedFieldsFieldName = ''''#ModifiedFields''''
    RefreshOptions = [broAfterInsert, broAfterUpdate]
    OnNewRecord = DataSetNewRecord'
          +
          (
            SELECT
              N'

    SQLInsert.Strings =
    (
      ''''EXEC ' + @ProcedureObjectModify + N'''''
      ''''  @Interface            = :[Interface:Language]'
              +
              CASE
                WHEN @Translation = 1 THEN N',''''
      ''''  @Language             = :Language'
                ELSE N''
              END
              + N',''''
      ''''  @'
              +
              CASE
                WHEN @IdentityField <> N'' THEN @IdentityField + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(@IdentityField), 0)) + N' = :OUT_OLD_' + @IdentityField + N' OUT'
                ELSE @KeyField + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(@KeyField), 0)) + N' = :' + @KeyField
              END
              + IsNull([Fields], ' %%FIELDS%%')
              + ',''''
      ''''  @FIELDS               = :[#ModifiedFields]''''
    )
    SQLUpdate.Strings =
    (
      ''''EXEC ' + @ProcedureObjectModify + N'''''
      ''''  @Interface            = :[Interface:Language]'
              +
              CASE
                WHEN @Translation = 1 THEN N',''''
      ''''  @Language             = :Language'
                ELSE N''
              END
              + ',''''
      ''''  @'
              +
              CASE
                WHEN @IdentityField <> N'' THEN @IdentityField + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(@IdentityField), 0)) + ' = :OLD_' + @IdentityField 
                ELSE 'OLD_' + @KeyField + Replicate(N' ', [Pub].[Is Negative Int](16 - Len(@KeyField), 0)) + ' = :OLD_' + @KeyField + ',''''
      ''''  @' + @KeyField + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(@KeyField), 0)) + ' = :' + @KeyField
              END
              + IsNull([Fields], ' %%FIELDS%%')
              + ',''''
      ''''  @FIELDS               = :[#ModifiedFields]''''
    )'
            FROM
            (
              SELECT
                [Fields] = [Pub].[Concat]
                           (
                             N',''''
      ''''  @' + [name] + Replicate(N' ', [Pub].[Is Negative Int](20 - Len([Name]), 0)) + N' = :' + [name],
                             N''
                           )
              FROM [sys].[columns]
              WHERE [object_id] = @Object_Id AND (@IdentityField IS NULL AND @KeyField <> [name] OR @IdentityField <> [name])
            ) F
          )
        ELSE N''
      END
      +
      CASE
        WHEN @ObjectViewClassName IS NULL THEN N'
    SQLDelete.Strings =
    (
      ''''' + @SQLDelete + N'''''
    )'
        ELSE N''
      END
      + N'
  end'
---------------- 2222222222222222222222222 ------------------
    +
    CASE
      WHEN @ParentClassName IN ('TSBaseCxView2Frm', 'TSBaseCxView3Frm') THEN N'

  inherited DataSetProvider2: TSQLDataSetProvider
    IndexFieldNames = ' + [Pub].[Deep Quote String](@DetailFields2, 2) + N'
    SQL.Strings =
    (
      ''''EXEC ' + @ProcedureObjectsView + N';3''''
      ''''  @Interface          = :[Interface:Language]'
      +
      CASE
        WHEN @Translation2 = 1 THEN N',''''
      ''''  @Language           = :Language'
        ELSE N''
      END
      +
      (
        SELECT
          IsNull
          (
            [Pub].[ConCat]
            (
              N',''''
      ''''  @' + [Name] + Replicate(N' ', [Pub].[Is Negative Int](18 - Len([Name]), 0)) + N' = :' + Name,
              N''
            ),
            N''
          )
        FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
      )
      + N'''''
    )
    SQLRefresh.Strings =
    (
      ''''EXEC ' + @ProcedureObjectsView + N';3''''
      ''''  @Interface          = :[Interface:Language]'
      +
      CASE
        WHEN @Translation2 = 1 THEN N',''''
      ''''  @Language           = :Language'
        ELSE N''
      END
      +
      (
        SELECT
          IsNull
          (
            [Pub].[ConCat]
            (
              N',''''
      ''''  @' + [Name] + Replicate(N' ', [Pub].[Is Negative Int](18 - Len([Name]), 0)) + N' = :' + Name,
              ''
            ),
            N''
          )
        FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
      )
      + ',''''
      ''''  @' + IsNull(@IdentityField2, @KeyField2) + Replicate(N' ', [Pub].[Is Negative Int](18 - Len(IsNull(@IdentityField2, @KeyField2)), 0)) + ' = :' + IsNull(@IdentityField2, @KeyField2) + '''''
    )'
      +
      CASE
        WHEN @Object2ViewClassName IS NULL THEN N'

    CachedUpdates = False
    FieldsAutomations.ModifiedFieldsFieldName = ''''#ModifiedFields''''
    OnNewRecord = DataSetNewRecord'
          +
          (
            SELECT
              N'
    SQLInsert.Strings =
    (
      ''''EXEC ' + @ProcedureObject2Modify + N'''''
      ''''  @Interface          = :[Interface:Language]'
              +
              CASE
                WHEN @Translation2 = 1 THEN N',''''
      ''''  @Language           = :Language'
                ELSE N''
              END
              + ',''''
      ''''  @'
              +
              CASE
                WHEN @IdentityField2 <> N'' THEN @IdentityField2 + Replicate(N' ', [Pub].[Is Negative Int](18 - Len(@IdentityField2), 0)) + N' = :OUT_OLD_' + @IdentityField2 + N' OUT'
                ELSE @KeyField2 + Replicate(N' ', [Pub].[Is Negative Int](18 - Len(@KeyField2), 0)) + N' = :' + @KeyField2
              END
              + IsNull([Fields], ' %%FIELDS%%')
              + ',''''
      ''''  @FIELDS             = :[#ModifiedFields]''''
    )
    SQLUpdate.Strings =
    (
      ''''EXEC ' + @ProcedureObject2Modify + N'''''
      ''''  @Interface          = :[Interface:Language]'
              +
              CASE
                WHEN @Translation2 = 1 THEN N',''''
      ''''  @Language           = :Language'
                ELSE N''
              END
              + ',''''
      ''''  @'
              +
              CASE
                WHEN @IdentityField2 <> N'' THEN @IdentityField2 + Replicate(N' ', [Pub].[Is Negative Int](18 - Len(@IdentityField2), 0)) + ' = :OLD_' + @IdentityField2
                ELSE 'OLD_' + @KeyField2 + Replicate(N' ', [Pub].[Is Negative Int](18 - Len(@KeyField2), 0)) + ' = :OLD_' + @KeyField2 + ', @' + @KeyField2 + ' = :' + @KeyField2
              END
              + IsNull([Fields], ' %%FIELDS%%')
              + ',''''
      ''''  @FIELDS             = :[#ModifiedFields]''''
    )'
            FROM
            (
              SELECT
                [Fields] = [Pub].[Concat]
                           (
                             N',''''
      ''''  @' + [name] + Replicate(N' ', [Pub].[Is Negative Int](18 - Len([Name]), 0)) + N' = :' + [name],
                             N''
                           )
              FROM [sys].[columns]
              WHERE [object_id] = @Object2_Id AND (@IdentityField2 IS NULL AND @KeyField2 <> [name] OR @IdentityField2 <> [name])
            ) F
          )
        ELSE N''
      END
      +
      CASE
        WHEN @Object2ViewClassName IS NULL THEN N'
    SQLDelete.Strings =
    (
      ''''' + @SQLDelete2 + N'''''
    )'
        ELSE N''
      END
      + N'
  end'
      ELSE N''
    END
---------------- 333333333333333333 ------------------
    +
    CASE
      WHEN @ParentClassName IN ('TSBaseCxView3Frm') THEN N'

  inherited DataSetProvider3: TSQLDataSetProvider
    IndexFieldNames = ' + [Pub].[Quote String](@DetailFields3) + '
    SQL.Strings =
    (
      ''''EXEC ' + @ProcedureObjectsView + N';4 @Interface = :[Interface:Language]' + CASE WHEN @Translation3 = 1 THEN N', @Language = :Language' ELSE '' END + (SELECT IsNull([Pub].[ConCat](', @' + [Name] + N' = :' + Name, ''), '') FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')) + N'''''
    )
    SQLRefresh.Strings =
    (
      ''''EXEC ' + @ProcedureObjectsView + N';4 @Interface = :[Interface:Language]' + CASE WHEN @Translation3 = 1 THEN N', @Language = :Language' ELSE '' END + (SELECT IsNull([Pub].[ConCat](', @' + [Name] + N' = :' + Name, ''), '') FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')) + ', @' + @KeyField3 + ' = :' + @KeyField3 + '''''
    )'
      + CASE
          WHEN @ObjectViewClassName IS NULL THEN N'

    CachedUpdates = False
    FieldsAutomations.ModifiedFieldsFieldName = ''''#ModifiedFields''''
    OnNewRecord = DataSetNewRecord'
          +
          (
            SELECT
              N'
    SQLInsert.Strings =
    (
      ''''EXEC ' + @ProcedureObjectModify + N';3 @Interface = :[Interface:Language], ' + CASE WHEN @Translation3 = 1 THEN N'@Language = :Language, ' ELSE '' END + '@' + @KeyField3 + ' = :OUT_OLD_' + @KeyField3 + ' OUT,' + IsNull([Fields], ' %%FIELDS%%') + '''''
    )
    SQLUpdate.Strings =
    (
      ''''EXEC ' + @ProcedureObjectModify + N';3 @Interface = :[Interface:Language], ' + CASE WHEN @Translation3 = 1 THEN N'@Language = :Language, ' ELSE '' END + '@' + @KeyField3 + ' = :OLD_' + @KeyField3 + ',' + IsNull([Fields], ' %%FIELDS%%') + ', @FIELDS = :[#ModifiedFields]''''
    )'
            FROM
            (
              SELECT
                [Fields] = IsNull([Pub].[Concat](' @' + [name] + ' = :' + [name], ','), '%FIELDS%')
              FROM [sys].[columns]
              WHERE [object_id] = @Object3_Id AND [name] <> @KeyField3
            ) F
          )
        ELSE N'' END
      +
      CASE
        WHEN @Object3ViewClassName IS NULL THEN N'
    SQLDelete.Strings =
    (
      ''''' + @SQLDelete3 + N'''''
    )'
        ELSE N''
      END
      + N'
  end'
      ELSE N''
    END

    + N'

  inherited BarManager: TdxBarManager'
    +
    CASE
      WHEN @ObjectViewClassName IS NULL THEN N'
    inherited mnView' + CASE WHEN @ParentClassName IN ('TSBaseCxView2Frm', 'TSBaseCxView3Frm') THEN N'X' ELSE N'' END + N': TdxBarButton
      Visible = ivNever
    end'
      ELSE N''
    END

    +
    CASE
      WHEN @Translation = 1 THEN N'
    object flcbLanguages: TSBaseViewFilterLookupComboBox
      Control.Caption = '' + [Pub].[Quote String]([GUI].[Delphi::Translate](Default, ''Язык перевода'', @Interface)) + ''

      Control.Width = 100
      Control.Properties.DropDownRows = 5
      Control.Properties.KeyFieldNames = ''''Code''''
      Control.Properties.ListColumns = <item FieldName = ''''Name'''' end>
      Control.Properties.ListSource = DataSourceLanguages

      AllowNull = False
      ParamName = ''''Language''''
    end

    inherited ToolBar: TdxBar
      ItemLinks.SkipClear = True
      ItemLinks =
      <
        item
          BeginGroup = True
          Item = flcbLanguages.Control
        end
      >
    end'
        +
        CASE
          WHEN @ObjectViewClassName IS NOT NULL THEN N'''
  +
  CASE
    WHEN @Privileges & 1 = 1 THEN N''
    object mnViewLanguage: TdxBarListItem
      ShowValues     = True
      ShowNumbers    = False
      Items.Strings  = ('' + @Languages + N'')
      Enabled        = False
      OnClick        = DataSourceRecordActionClick
    end
    object pmView: TdxBarPopupMenu
      BarManager    = BarManager
      ItemLinks     = <item Item = mnViewLanguage end>
    end
    inherited mnView: TdxBarButton
      ButtonStyle   = bsDropDown
      DropDownMenu  = pmView
    end''
    ELSE N''''
  END
  +
  CASE
    WHEN @Privileges & 2 = 2 THEN N''
    object mnNewLanguage: TdxBarListItem
      ShowValues     = True
      ShowNumbers    = False
      Items.Strings  = ('' + @Languages + N'')
      Enabled        = False
      OnClick        = DataSourceActiveActionClick
    end
    object pmNew: TdxBarPopupMenu
      BarManager    = BarManager
      ItemLinks     = <item Item = mnNewLanguage end>
    end
    inherited mnNew: TdxBarButton
      ButtonStyle   = bsDropDown
      DropDownMenu  = pmNew
    end''
    ELSE N''''
  END
  +
  CASE
    WHEN @Privileges & 4 = 4 THEN N''
    object mnEditLanguage: TdxBarListItem
      ShowValues     = True
      ShowNumbers    = False
      Items.Strings  = ('' + @Languages + N'')
      Enabled        = False
      OnClick        = DataSourceRecordActionClick
    end
    object pmEdit: TdxBarPopupMenu
      BarManager    = BarManager
      ItemLinks     = <item Item = mnEditLanguage end>
    end
    inherited mnEdit: TdxBarButton
      ButtonStyle   = bsDropDown
      DropDownMenu  = pmEdit
    end''
    ELSE N''''
  END
  + N'''
          ELSE N''
        END

      ELSE N''
    END
    + N'
  end'

      +
      CASE
        WHEN @ParentClassName IN ('TSBaseCxViewFrm', 'TSBaseCxView2Frm', 'TSBaseCxView3Frm', 'TSBaseCxTreeViewFrm') THEN N'

  inherited cxGridDBTableView: TcxGridDBTableView
    Version = 1
    DataController.KeyFieldNames = ' + [Pub].[Deep Quote String](@KeyField, 2) + N'
    DataController.DataModeController.SyncMode = ' + CASE WHEN @ObjectViewClassName IS NULL THEN 'True' ELSE 'False' END
          +
          CASE
            WHEN @Object_Id IS NOT NULL THEN
              (
                SELECT
                  [Pub].[Concat]
                  (
                    N'
    object Column' + Replace(C.[name], N' ', N'') + N': TcxGridDBColumn'
                    +
                    CASE
                      WHEN C.[Index] = 1 THEN N'
      SortOrder = soAscending'
                      ELSE N''
                    END
                    + N'
      HeaderAlignmentHorz = taCenter
      Caption = '' + [Pub].[Quote String]([GUI].[Delphi::Translate](Default, ' + [Pub].[Quote String](C.[caption]) + ', @Interface)) + N'''
                    +
                    CASE
                      WHEN C.[type] = 'bit' THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryCheckBox' + CASE WHEN C.[NullAble] = 1 THEN N'NullAble' ELSE N'' END + N'
      Options.AutoWidth = False
      Width = 25'
                      WHEN C.[type] = 'date' THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryDate
      Options.AutoWidth = False
      Width = 70'
                      WHEN C.[type] = 'time' THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryTime
      Options.AutoWidth = False
      Width = 50'
                      WHEN C.[type] IN ('smalldatetime', 'datetime', 'datetime2') THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryDateTime
      Options.AutoWidth = False
      Width = 115'
                      WHEN C.[type] IN ('varchar', 'nvarchar') THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryText
      Options.FilteringAddValueItems = False
      Width = 100'
                      ELSE N'
      Options.AutoWidth = False
      Width = 50'
                    END
                    +
                    CASE
                      WHEN @ObjectViewClassName IS NULL AND C.[NullAble] = 0 THEN N'
      OnValidateDrawValue = cxEditPropertiesValidateOnNull' + CASE WHEN C.[type] IN ('varchar', 'nvarchar') THEN N'OrEmpty' ELSE N'' END + N'DrawValue'
                      ELSE N''
                    END
                    + N'
      DataBinding.FieldName = ' + [Pub].[Deep Quote String](C.[name], 2)
                    + N'
    end'
                    , N''
                  )
                  +
                  IsNull
                  (
                     N'
    object ColumnsStringDataLinkController: TFieldDataLinkController
      DataSource = DataSource
      FieldNames = ' + [Pub].[Deep Quote String]([Pub].[Concat](CASE WHEN @ObjectViewClassName IS NULL AND C.[type] IN ('varchar', 'nvarchar') THEN C.[name] END, ';'), 2) + N'
      OnFieldsChanged = StringFieldChange
    end',
                    N''
                  )
                FROM
                (
                  SELECT
                    [name]      = C.[name],
                    [caption]   = CASE C.[name]
                                    WHEN 'Name'     THEN N'Наименование'
                                    WHEN 'Code'     THEN N'Код'
                                    WHEN 'Date'     THEN N'Дата'
                                    WHEN 'DateTime' THEN N'Дата и время'
                                    ELSE C.[name]
                                  END,
                    [type]      = TYPE_NAME(C.[system_type_id]),
                    [Index]     = ROW_NUMBER() OVER (ORDER BY C.[column_id]),
                    [NullAble]  = C.[is_nullable]
                  FROM [sys].[columns] C
                  LEFT JOIN [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';') I ON C.[name] = I.[name]
                  WHERE C.[object_id] = @Object_Id AND C.[is_identity] = 0 AND C.[is_computed] = 0 AND I.[name] IS NULL
                ) C
              )
            ELSE N'
    object ColumnName: TcxGridDBColumn
      DataBinding.FieldName = ''''Name''''
      Caption = '' + [Pub].[Quote String]([GUI].[Delphi::Translate](Default, ''Наименование'', @Language)) + N''
      SortOrder = soAscending
      Width = 120
    end'
          END

          +
          CASE
            WHEN @IsSecurityModeTable = 1 AND @ObjectViewClassName IS NOT NULL THEN N'
    object ColumnPrivilegeUpdatable: TcxGridDBColumn
      Visible = False
      Hidden = True
      DataBinding.FieldName = ''''Privilege:Updatable''''
    end
    object ColumnPrivilegeDeletable: TcxGridDBColumn
      Visible = False
      Hidden = True
      DataBinding.FieldName = ''''Privilege:Deletable''''
    end'
            ELSE N''
          END

          + N'
  end
  inherited cxGridDBTableView: TcxGridDBTableView
    OptionsView.ColumnAutoWidth = True
  end'
          
          
          
          +
          CASE
            WHEN @ParentClassName IN ('TSBaseCxView2Frm', 'TSBaseCxView3Frm') THEN N'

  inherited cxGridDBTableView2: TcxGridDBTableView
    Version = 1
    DataController.KeyFieldNames = ' + [Pub].[Deep Quote String](@KeyField2, 2) + N'
    DataController.MasterKeyFieldNames = ' + [Pub].[Deep Quote String](@MasterFields2, 2) + N'
    DataController.DetailKeyFieldNames = ' + [Pub].[Deep Quote String](@DetailFields2, 2) + N'
    DataController.DataModeController.SyncMode = ' + CASE WHEN @Object2ViewClassName IS NULL AND @Object2ViewClassName IS NULL THEN 'True' ELSE 'False' END
              +
              CASE
                WHEN @Object2_Id IS NOT NULL THEN
                  (
                    SELECT
                      [Pub].[Concat]
                      (
                        N'
    object Column2' + Replace(C.[name], N' ', N'') + N': TcxGridDBColumn'
                        +
                        CASE
                          WHEN C.[Index] = 1 THEN N'
      SortOrder = soAscending'
                          ELSE N''
                        END
                        + N'
      HeaderAlignmentHorz = taCenter
      Caption = '' + [Pub].[Quote String]([GUI].[Delphi::Translate](Default, ' + [Pub].[Quote String](C.[caption]) + ', @Interface)) + N'''
                        +
                        CASE
                          WHEN C.[type] = 'bit' THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryCheckBox' + CASE WHEN C.[NullAble] = 1 THEN N'NullAble' ELSE N'' END + N'
      Options.AutoWidth = False
      Width = 25'
                          WHEN C.[type] = 'date' THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryDate
      Options.AutoWidth = False
      Width = 70'
                          WHEN C.[type] = 'time' THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryTime
      Options.AutoWidth = False
      Width = 50'
                          WHEN C.[type] IN ('smalldatetime', 'datetime', 'datetime2') THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryDateTime
      Options.AutoWidth = False
      Width = 115'
                          WHEN C.[type] IN ('varchar', 'nvarchar') THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryText
      Options.FilteringAddValueItems = False
      Width = 100'
                          ELSE N'
      Options.AutoWidth = False
      Width = 50'
                        END
                        +
                        CASE
                          WHEN @Object2ViewClassName IS NULL AND C.[NullAble] = 0 THEN N'
      OnValidateDrawValue = cxEditPropertiesValidateOnNull' + CASE WHEN C.[type] IN ('varchar', 'nvarchar') THEN N'OrEmpty' ELSE N'' END + N'DrawValue'
                          ELSE N''
                        END
                        + N'
      DataBinding.FieldName = ' + [Pub].[Deep Quote String](C.[name], 2)
                        + N'
    end'
                        , N''
                      )
                      +
                      IsNull
                      (
                         N'
    object ColumnsStringDataLinkController2: TFieldDataLinkController
      DataSource = DataSource2
      FieldNames = ' + [Pub].[Deep Quote String]([Pub].[Concat](CASE WHEN @ObjectViewClassName IS NULL AND @Object2ViewClassName IS NULL AND C.[type] IN ('varchar', 'nvarchar') THEN C.[name] END, ';'), 2) + N'
      OnFieldsChanged = StringFieldChange
    end',
                        N''
                      )
                    FROM
                    (
                      SELECT
                        [name]      = C.[name],
                        [caption]   = CASE C.[name]
                                        WHEN 'Name'     THEN N'Наименование'
                                        WHEN 'Code'     THEN N'Код'
                                        WHEN 'Date'     THEN N'Дата'
                                        WHEN 'DateTime' THEN N'Дата и время'
                                        ELSE C.[name]
                                      END,
                        [type]      = TYPE_NAME(C.[system_type_id]),
                        [Index]     = ROW_NUMBER() OVER (ORDER BY C.[column_id]),
                        [NullAble]  = C.[is_nullable]
                      FROM [sys].[columns] C
                      LEFT JOIN [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';') I ON C.[name] = I.[name]
                      WHERE C.[object_id] = @Object2_Id AND C.[is_identity] = 0 AND C.[is_computed] = 0 AND I.[name] IS NULL
                    ) C
                  )
                ELSE N'
    object Column2Name: TcxGridDBColumn
      DataBinding.FieldName = ''''Name''''
      Caption = '' + [Pub].[Quote String]([GUI].[Delphi::Translate](Default, ''Наименование'', @Language)) + N''
      SortOrder = soAscending
      Width = 120
    end'
              END

              +
              CASE
                WHEN @IsSecurityModeTable2 = 1 AND @Object2ViewClassName IS NOT NULL THEN N'
    object Column2PrivilegeUpdatable: TcxGridDBColumn
      Visible = False
      Hidden = True
      DataBinding.FieldName = ''''Privilege:Updatable''''
    end
    object Column2PrivilegeDeletable: TcxGridDBColumn
      Visible = False
      Hidden = True
      DataBinding.FieldName = ''''Privilege:Deletable''''
    end'
                ELSE N''
              END
              + N'
  end
  inherited cxGridDBTableView2: TcxGridDBTableView
    OptionsView.ColumnAutoWidth = True
  end'

              +
              CASE
                WHEN @ParentClassName IN ('TSBaseCxView3Frm') THEN N'

  inherited cxGridDBTableView3: TcxGridDBTableView
    Version = 1
    DataController.KeyFieldNames = ' + [Pub].[Deep Quote String](@KeyField3, 2) + N'
    DataController.MasterKeyFieldNames = ' + [Pub].[Deep Quote String](@MasterFields3, 2) + N'
    DataController.DetailKeyFieldNames = ' + [Pub].[Deep Quote String](@DetailFields3, 2) + N'
    DataController.DataModeController.SyncMode = ' + CASE WHEN @Object3ViewClassName IS NULL AND @Object3ViewClassName IS NULL THEN 'True' ELSE 'False' END
                  +
                  CASE
                    WHEN @Object3_Id IS NOT NULL THEN
                      (
                        SELECT
                          [Pub].[Concat]
                          (
                            N'
    object Column3' + Replace(C.[name], N' ', N'') + N': TcxGridDBColumn'
                            +
                            CASE
                              WHEN C.[Index] = 1 THEN N'
      SortOrder = soAscending'
                              ELSE N''
                            END
                            + N'
      HeaderAlignmentHorz = taCenter
      Caption = '' + [Pub].[Quote String]([GUI].[Delphi::Translate](Default, ' + [Pub].[Quote String](C.[caption]) + ', @Interface)) + N'''
                            +
                            CASE
                              WHEN C.[type] = 'bit' THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryCheckBox' + CASE WHEN C.[NullAble] = 1 THEN N'NullAble' ELSE N'' END + N'
      Options.AutoWidth = False
      Width = 25'
                              WHEN C.[type] = 'date' THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryDate
      Options.AutoWidth = False
      Width = 70'
                              WHEN C.[type] = 'time' THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryTime
      Options.AutoWidth = False
      Width = 50'
                              WHEN C.[type] IN ('smalldatetime', 'datetime', 'datetime2') THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryDateTime
      Options.AutoWidth = False
      Width = 115'
                              WHEN C.[type] IN ('varchar', 'nvarchar') THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryText
      Options.FilteringAddValueItems = False
      Width = 100'
                              ELSE N'
      Options.AutoWidth = False
      Width = 50'
                            END
                            +
                            CASE
                              WHEN @Object3ViewClassName IS NULL AND C.[NullAble] = 0 THEN N'
      OnValidateDrawValue = cxEditPropertiesValidateOnNull' + CASE WHEN C.[type] IN ('varchar', 'nvarchar') THEN N'OrEmpty' ELSE N'' END + N'DrawValue'
                              ELSE N''
                            END
                            + N'
      DataBinding.FieldName = ' + [Pub].[Deep Quote String](C.[name], 2)
                            + N'
    end'
                            , N''
                          )
                          +
                          IsNull
                          (
                             N'
    object ColumnsStringDataLinkController3: TFieldDataLinkController
      DataSource = DataSource
      FieldNames = ' + [Pub].[Deep Quote String]([Pub].[Concat](CASE WHEN @ObjectViewClassName IS NULL AND @Object2ViewClassName IS NULL AND @Object3ViewClassName IS NULL AND C.[type] IN ('varchar', 'nvarchar') THEN C.[name] END, ';'), 2) + N'
      OnFieldsChanged = StringFieldChange
    end',
                            N''
                          )
                        FROM
                        (
                          SELECT
                            [name]      = C.[name],
                            [caption]   = CASE C.[name]
                                            WHEN 'Name'     THEN N'Наименование'
                                            WHEN 'Code'     THEN N'Код'
                                            WHEN 'Date'     THEN N'Дата'
                                            WHEN 'DateTime' THEN N'Дата и время'
                                            ELSE C.[name]
                                          END,
                            [type]      = TYPE_NAME(C.[system_type_id]),
                            [Index]     = ROW_NUMBER() OVER (ORDER BY C.[column_id]),
                            [NullAble]  = C.[is_nullable]
                          FROM [sys].[columns] C
                          LEFT JOIN [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';') I ON C.[name] = I.[name]
                          WHERE C.[object_id] = @Object3_Id AND C.[is_identity] = 0 AND C.[is_computed] = 0 AND I.[name] IS NULL
                        ) C
                      )
                    ELSE N'
    object Column3Name: TcxGridDBColumn
      DataBinding.FieldName = ''''Name''''
      Caption = '' + [Pub].[Quote String]([GUI].[Delphi::Translate](Default, ''Наименование'', @Language)) + N''
      SortOrder = soAscending
      Width = 120
    end'
                  END

                  +
                  CASE
                    WHEN @IsSecurityModeTable3 = 1 AND @Object3ViewClassName IS NOT NULL THEN N'
    object Column3PrivilegeUpdatable: TcxGridDBColumn
      Visible = False
      Hidden = True
      DataBinding.FieldName = ''''Privilege:Updatable''''
    end
    object Column3PrivilegeDeletable: TcxGridDBColumn
      Visible = False
      Hidden = True
      DataBinding.FieldName = ''''Privilege:Deletable''''
    end'
                    ELSE N''
                  END

                  + N'
  end
  inherited cxGridDBTableView3: TcxGridDBTableView
    OptionsView.ColumnAutoWidth = True
  end'
                ELSE N''
              END
            ELSE N''
          END

        WHEN @ParentClassName IN ('TSBaseCxTreeFrm', 'TSBaseCxTree2Frm') THEN N'

  inherited cxDBTreeList: TcxDBTreeList
    Version = 1
    DataController.KeyField = ' + [Pub].[Deep Quote String](@KeyField, 2) + N'
    DataController.ParentField = ' + [Pub].[Deep Quote String](@ParentField, 2) + N'
    OptionsData.SyncMode = ' + CASE WHEN @ObjectViewClassName IS NULL THEN 'True' ELSE 'False' END
          +
          CASE
            WHEN @ObjectViewClassName IS NULL THEN N'''
    +
    CASE
      WHEN @Privileges & 4 <> 0 THEN N''
    DragMode = dmAutomatic
    OptionsBehavior.DragFocusing = True''
      ELSE N''''
    END

    + N'''
            ELSE N''
          END

          + N'
    Bands =
    <
      item
      end
    >'
          +
          CASE
            WHEN @Object_Id IS NOT NULL THEN
              (
                SELECT
                  [Pub].[Concat]
                  (
                    N'
    object Column' + C.[name] + N': TcxDBTreeListColumn
      Caption.AlignHorz = taCenter
      Caption.Text = '' + [Pub].[Quote String]([GUI].[Delphi::Translate](Default, ' + [Pub].[Quote String](C.[caption]) + ', @Interface)) + N'''
                    +
                    CASE
                      WHEN C.[type] = 'bit' THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryCheckBox' + CASE WHEN C.[NullAble] = 1 THEN N'NullAble' ELSE N'' END + N'
      Options.AutoWidth = False
      Width = 25'
                      WHEN C.[type] = 'date' THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryDate
      Options.AutoWidth = False
      Width = 70'
                      WHEN C.[type] = 'time' THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryTime
      Options.AutoWidth = False
      Width = 50'
                      WHEN C.[type] IN ('smalldatetime', 'datetime', 'datetime2') THEN N'
      RepositoryItem = MainDataModule.cxEditRepositoryDateTime
      Options.AutoWidth = False
      Width = 115'
                      WHEN C.[type] IN ('varchar', 'nvarchar') THEN N'
      Width = 50'
                      ELSE N'
      Options.AutoWidth = False
      Width = 50'
                    END
                    +
                    CASE
                      WHEN @ObjectViewClassName IS NULL AND C.[NullAble] = 0 THEN N'
      OnValidateDrawValue = cxEditPropertiesValidateOnNull' + CASE WHEN C.[type] IN ('varchar', 'nvarchar') THEN N'OrEmpty' ELSE N'' END + N'DrawValue'
                      ELSE N''
                    END
                    + N'
      Position.BandIndex = 0
      DataBinding.FieldName = ' + [Pub].[Deep Quote String](C.[name], 2)
                    + N'
    end'
                    , N''
                  )
                  +
                  IsNull
                  (
                     N'
    object ColumnsStringDataLinkController: TFieldDataLinkController
      DataSource = DataSource
      FieldNames = ' + [Pub].[Deep Quote String]([Pub].[Concat](CASE WHEN @ObjectViewClassName IS NULL AND C.[type] IN ('varchar', 'nvarchar') THEN C.[name] END, ';'), 2) + N'
      OnFieldsChanged = StringFieldChange
    end',
                    N''
                  )
                FROM
                (
                  SELECT
                    [name]      = C.[name],
                    [caption]   = CASE C.[name]
                                    WHEN 'Name'     THEN N'Наименование'
                                    WHEN 'Code'     THEN N'Код'
                                    WHEN 'Date'     THEN N'Дата'
                                    WHEN 'DateTime' THEN N'Дата и время'
                                    ELSE C.[name]
                                  END,
                    [type]      = TYPE_NAME(C.[system_type_id]),
                    [Index]     = ROW_NUMBER() OVER (ORDER BY C.[column_id]),
                    [NullAble]  = C.[is_nullable]
                  FROM [sys].[columns] C
                  LEFT JOIN [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';') I ON C.[name] = I.[name]
                  WHERE C.[object_id] = @Object_Id AND C.[is_identity] = 0 AND C.[is_computed] = 0 AND I.[name] IS NULL
                    AND C.[name] <> @ParentField
                ) C
              )
            ELSE N'
    object ColumnName: TcxDBTreeListColumn
      Caption.AlignHorz = taCenter
      Caption.Text = '' + [Pub].[Quote String]([GUI].[Delphi::Translate](Default, ''Наименование'', @Language)) + ''
      DataBinding.FieldName = ''''Name''''
      Width = 120
      Position.BandIndex = 0
    end'
          END
          +
          CASE
            WHEN @IsSecurityModeTable = 1 AND @ObjectViewClassName IS NOT NULL THEN N'
    object ColumnPrivilegeUpdatable: TcxDBTreeListColumn
      Visible = False
      Hidden = True
      DataBinding.FieldName = ''''Privilege:Updatable''''
    end
    object ColumnPrivilegeDeletable: TcxDBTreeListColumn
      Visible = False
      Hidden = True
      DataBinding.FieldName = ''''Privilege:Deletable''''
    end'
            ELSE N''
          END

      + N'
  end'
        ELSE N''
      END

    +
    CASE
      WHEN @ObjectViewClassName IS NOT NULL OR @Object2ViewClassName IS NOT NULL OR @Object3ViewClassName IS NOT NULL OR @IsSecurityModeTable = 1 OR @IsSecurityModeTable2 = 1 OR @IsSecurityModeTable3 = 1 THEN N'

  inherited Self: TSBaseFrm'
        +
        CASE
          WHEN @ObjectViewClassName IS NOT NULL OR @Object2ViewClassName IS NOT NULL OR @Object3ViewClassName IS NOT NULL THEN N'
    DataSourceControls.SkipClear = True
    DataSourceControls =
    <'
            +
            CASE
              WHEN @ObjectViewClassName IS NOT NULL THEN N'
      item
        Item  = '
                +
                CASE
                  WHEN @ParentClassName IN ('TSBaseCxViewFrm', 'TSBaseCxView2Frm', 'TSBaseCxView3Frm', 'TSBaseCxTreeViewFrm') THEN N'cxGridDBTableView'
                  WHEN @ParentClassName IN ('TSBaseCxTreeFrm', 'TSBaseCxTree2Frm', 'TSBaseCxCustomTreeFrm') THEN N'cxDBTreeList'
                  ELSE N'<Unknown1>'
                END
                + N'
        Links =
        <
          item
            Table = ' + [Pub].[Deep Quote String](@ObjectName, 2) + N'
            FieldNames = ' + [Pub].[Deep Quote String](@@IdentityField, 2) + N'
          end
        >
      end'
              ELSE N''
            END
            +
            CASE
              WHEN @Object2ViewClassName IS NOT NULL THEN N'
      item
        Item  = '
                +
                CASE
                  WHEN @ParentClassName IN ('TSBaseCxView2Frm', 'TSBaseCxView3Frm') THEN N'cxGridDBTableView2'
                  ELSE N'<Unknown2>'
                END
                + N'
        Links =
        <
          item
            Table = ' + [Pub].[Quote String](@Object2Name) + N'
            FieldNames = ' + [Pub].[Quote String](IsNull(@IdentityField2, @KeyField2)) + N'
          end
        >
      end'
              ELSE N''
            END
            +
            CASE
              WHEN @Object3ViewClassName IS NOT NULL THEN N'
      item
        Item  = '
                +
                CASE
                  WHEN @ParentClassName IN ('TSBaseCxView3Frm') THEN N'cxGridDBTableView3'
                  ELSE N'<Unknown3>'
                END
                + N'
        Links =
        <
          item
            Table = ' + [Pub].[Quote String](@Object3Name) + N'
            FieldNames = ' + [Pub].[Quote String](IsNull(@IdentityField3, @KeyField3)) + N'
          end
        >
      end'
              ELSE N''
            END
            + N'
    >'
          ELSE N''
        END

        + N'

    ItemsReactions =
    <'''
        +
        CASE
          WHEN @ObjectViewClassName IS NOT NULL THEN N'
      +
      CASE
        WHEN @Privileges & 1 = 1 THEN N''
      item
        Item                    = mnView
        OnClick.ClassName       = ' + [Pub].[Deep Quote String](@ObjectViewClassName, 2) + N'
        OnClick.ClassConstant   = ''''Mode:String=View''''
        OnClick.ClassParams     = ' + [Pub].[Deep Quote String](@KeyField + CASE WHEN @Translation = 1 THEN N';Language' ELSE N'' END, 2) + N'
      end'
            +
            CASE
              WHEN @Translation = 1 THEN N'
      item
        Item                    = mnViewLanguage
        OnClick.ClassName       = ' + [Pub].[Deep Quote String](@ObjectViewClassName, 2) + N'
        OnClick.ClassConstant   = ''''Mode:String=View''''
        OnClick.ClassParams     = ' + [Pub].[Deep Quote String](@KeyField + N';Language=Sender.ItemName', 2) + N'
      end'
              ELSE N''
            END
            + N'''
        ELSE N''''
      END
      +
      CASE
        WHEN @Privileges & 2 = 2 THEN N''
      item
        Item                    = mnNew
        OnClick.ClassName       = ' + [Pub].[Deep Quote String](@ObjectViewClassName, 2) + N'
        OnClick.ClassConstant   = ''''Mode:String=Insert'''''
            +
            CASE
              WHEN @Translation = 1 OR @IdentityParams <> '' THEN N'
        OnClick.ClassParams     = '
                +
                [Pub].[Deep Quote String]
                (
                  CASE WHEN @IdentityParams <> '' THEN (SELECT [Pub].[Concat]([Name], N';') FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')) ELSE N'' END
                  +
                  CASE WHEN @Translation = 1 AND @IdentityParams <> '' THEN N';' ELSE N'' END
                  +
                  CASE WHEN @Translation = 1 THEN N'Language' ELSE N'' END,
                  2
                )
              ELSE N''
            END
            + N'
      end'
            +
            CASE
              WHEN @Translation = 1 THEN N'
      item
        Item                    = mnNewLanguage
        OnClick.ClassName       = ' + [Pub].[Deep Quote String](@ObjectViewClassName, 2) + N'
        OnClick.ClassConstant   = ''''Mode:String=Insert''''
        OnClick.ClassParams     = '
                +
                [Pub].[Deep Quote String]
                (
                  CASE WHEN @IdentityParams <> '' THEN (SELECT [Pub].[Concat]([Name] + N';', N'') FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')) ELSE N'' END
                  + N'Language=Sender.ItemName',
                  2
                )
                + N'
      end'
              ELSE N''
            END
            + N'''
        ELSE N''''
      END'
          ELSE N''
        END
        +
        CASE
          WHEN @ObjectViewClassName IS NOT NULL OR @IsSecurityModeTable = 1 THEN N'
      +
      CASE
        WHEN @Privileges & 4 = 4 THEN N''
      item
        Item                    = mnEdit'
            +
            CASE
              WHEN @IsSecurityModeTable = 1 THEN N'
        OnGetEnabled.Condition  = ''''[Privilege:Updatable]'''''
              ELSE N''
            END
            +
            CASE
              WHEN @ObjectViewClassName IS NOT NULL THEN N'
        OnClick.ClassName       = ' + [Pub].[Deep Quote String](@ObjectViewClassName, 2) + N'
        OnClick.ClassConstant   = ''''Mode:String=Edit''''
        OnClick.ClassParams     = ' + [Pub].[Deep Quote String](@KeyField + CASE WHEN @Translation = 1 THEN N';Language' ELSE N'' END, 2)
              ELSE N'
        OnClick.Reaction        = biraDataSourceInPlaceEdit'
            END
            + N'
      end'

            +
            CASE
              WHEN @ObjectViewClassName IS NOT NULL AND @Translation = 1 THEN N'
      item
        Item                    = mnEditLanguage
        OnClick.ClassName       = ' + [Pub].[Deep Quote String](@ObjectViewClassName, 2) + N'
        OnClick.ClassConstant   = ''''Mode:String=Edit''''
        OnClick.ClassParams     = ' + [Pub].[Deep Quote String](@KeyField + N';Language=Sender.ItemName', 2) + N'
      end'
              ELSE N''
            END
            + N'''
        ELSE N''''
      END'
          ELSE N''
        END

        +
        CASE
          WHEN @ObjectViewClassName IS NOT NULL OR @IsSecurityModeTable = 1 THEN N'
      +
      CASE
        WHEN @Privileges & 8 = 8 THEN N''
      item
        Item                    = mnDelete'
            +
            CASE
              WHEN @IsSecurityModeTable = 1 THEN N'
        OnGetEnabled.Condition  = ''''[Privilege:Deletable]'''''
              ELSE N''
            END
            +
            CASE
              WHEN @ObjectViewClassName IS NOT NULL THEN N'
        OnClick.CommandText     = ''''' + @SQLDelete + N''''''
              ELSE N'
        OnClick.Reaction        = biraDataSourceDelete'
            END
            + N'
      end''
        ELSE N''''
      END'
          ELSE N''
        END

        +
        CASE
          WHEN @Object2ViewClassName IS NOT NULL THEN N' + N''
      item
        Item                    = mnView2
        OnClick.ClassName       = ' + [Pub].[Deep Quote String](@Object2ViewClassName, 2) + N'
        OnClick.ClassConstant   = ''''Mode:String=View''''
        OnClick.ClassParams     = ' + [Pub].[Deep Quote String](@KeyField2 + CASE WHEN @Translation2 = 1 THEN N';Language' ELSE N'' END, 2) + N'
      end'''
          ELSE N''
        END
        +
        CASE
          WHEN @Object2ViewClassName IS NOT NULL OR @IsSecurityModeTable2 = 1 THEN N' + N''
      item
        Item                    = mnNew2'
            +
            CASE
              WHEN @IsSecurityModeTable2 = 1 THEN N'
        OnGetEnabled.Condition  = ''''[Privilege2:Insertable]'''''
              ELSE N''
            END
            +
            CASE
              WHEN @Object2ViewClassName IS NOT NULL THEN N'
        OnClick.ClassName       = ' + [Pub].[Deep Quote String](@Object2ViewClassName, 2) + N'
        OnClick.ClassConstant   = ''''Mode:String=Insert''''
        OnClick.ClassParams     = ' + [Pub].[Deep Quote String](@DetailFields2 + N'=' + @MasterFields2 + CASE WHEN @Translation2 = 1 THEN N';Language' ELSE N'' END, 2)
              ELSE N'
        OnClick.Reaction        = biraDataSourceInPlaceInsertChild'
            END
            + N'
      end
      item
        Item                    = mnEdit2'
            +
            CASE
              WHEN @IsSecurityModeTable2 = 1 THEN N'
        OnGetEnabled.Condition  = ''''[Privilege:Updatable]'''''
              ELSE N''
            END
            +
            CASE
              WHEN @Object2ViewClassName IS NOT NULL THEN N'
        OnClick.ClassName       = ' + [Pub].[Deep Quote String](@Object2ViewClassName, 2) + N'
        OnClick.ClassConstant   = ''''Mode:String=Edit''''
        OnClick.ClassParams     = ' + [Pub].[Deep Quote String](@KeyField2 + CASE WHEN @Translation2 = 1 THEN N';Language' ELSE N'' END, 2)
              ELSE N'
        OnClick.Reaction        = biraDataSourceInPlaceEdit'
            END
            + N'
      end'''
            +
            CASE
              WHEN @Object2ViewClassName IS NOT NULL OR @IsSecurityModeTable2 = 1 THEN N' + N''
      item
        Item                    = mnDelete2'
                +
                CASE
                  WHEN @IsSecurityModeTable2 = 1 THEN N'
        OnGetEnabled.Condition  = ''''[Privilege:Deletable]'''''
                  ELSE N''
                END
                +
                CASE
                  WHEN @Object2ViewClassName IS NOT NULL THEN N'
        OnClick.CommandText     = ''''' + @SQLDelete2 + N''''''
                  ELSE N'
        OnClick.Reaction        = biraDataSourceDelete'
                END
                + N'
      end'''
              ELSE N''
            END
          ELSE N''
        END

                +
        CASE
          WHEN @Object3ViewClassName IS NOT NULL THEN N' + N''
      item
        Item                    = mnView3
        OnClick.ClassName       = ' + [Pub].[Deep Quote String](@Object3ViewClassName, 2) + N'
        OnClick.ClassConstant   = ''''Mode:String=View''''
        OnClick.ClassParams     = ' + [Pub].[Deep Quote String](@KeyField3 + CASE WHEN @Translation3 = 1 THEN N';Language' ELSE N'' END, 2) + N'
      end'''
          ELSE N''
        END
        +
        CASE
          WHEN @Object3ViewClassName IS NOT NULL OR @IsSecurityModeTable3 = 1 THEN N' + N''
      item
        Item                    = mnNew3'
            +
            CASE
              WHEN @IsSecurityModeTable3 = 1 THEN N'
        OnGetEnabled.Condition  = ''''[Privilege3:Insertable]'''''
              ELSE N''
            END
            +
            CASE
              WHEN @Object3ViewClassName IS NOT NULL THEN N'
        OnClick.ClassName       = ' + [Pub].[Deep Quote String](@Object3ViewClassName, 2) + N'
        OnClick.ClassConstant   = ''''Mode:String=Insert''''
        OnClick.ClassParams     = ' + [Pub].[Deep Quote String](@DetailFields3 + N'=' + @MasterFields3 + CASE WHEN @Translation3 = 1 THEN N';Language' ELSE N'' END, 2)
              ELSE N'
        OnClick.Reaction        = biraDataSourceInPlaceInsertChild'
            END
            + N'
      end
      item
        Item                    = mnEdit3'
            +
            CASE
              WHEN @IsSecurityModeTable3 = 1 THEN N'
        OnGetEnabled.Condition  = ''''[Privilege:Updatable]'''''
              ELSE N''
            END
            +
            CASE
              WHEN @Object3ViewClassName IS NOT NULL THEN N'
        OnClick.ClassName       = ' + [Pub].[Deep Quote String](@Object3ViewClassName, 2) + N'
        OnClick.ClassConstant   = ''''Mode:String=Edit''''
        OnClick.ClassParams     = ' + [Pub].[Deep Quote String](@KeyField3 + CASE WHEN @Translation3 = 1 THEN N';Language' ELSE N'' END, 2)
              ELSE N'
        OnClick.Reaction        = biraDataSourceInPlaceEdit'
            END
            + N'
      end'''
            +
            CASE
              WHEN @Object3ViewClassName IS NOT NULL OR @IsSecurityModeTable3 = 1 THEN N' + N''
      item
        Item                    = mnDelete3'
                +
                CASE
                  WHEN @IsSecurityModeTable3 = 1 THEN N'
        OnGetEnabled.Condition  = ''''[Privilege:Deletable]'''''
                  ELSE N''
                END
                +
                CASE
                  WHEN @Object3ViewClassName IS NOT NULL THEN N'
        OnClick.CommandText     = ''''' + @SQLDelete3 + N''''''
                  ELSE N'
        OnClick.Reaction        = biraDataSourceDelete'
                END
                + N'
      end'''
              ELSE N''
            END
          ELSE N''
        END

        + N'
      + N''
    >
  end'
      ELSE N''
    END

    + N'''

    SELECT
      [ClassName]             = ' + [Pub].[Quote String](@ParentClassName) + ',
      [Privileges]            = @Privileges,
      [Unique]                = CAST(1 As Bit),
--      [BeforeCreateProcedure] = ' + [Pub].[Quote String]('EXEC ' + @ProcedureObjectsViewDelphi + ';2 @Interface = :[Interface:Language]') + N'
--      [SubClassParams]        = ''<Param1>;<Param2>'',
--      [SubClassProcedure]     = ' + [Pub].[Quote String]('EXEC ' + @ProcedureObjectsViewDelphi + ';3 @Interface = :[Interface:Language], @<Param1> = :<Param1>, @<Param2> = :<Param2>')
      +
      CASE
        WHEN @IdentityParams <> '' THEN N',
      [InstanceParams]        = ' + [Pub].[Quote String]((SELECT [Pub].[ConCat]([Name], ';') FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';'))) + N',
      [InstanceProcedure]     = ' + [Pub].[Quote String]
                                    (
                                      'EXEC ' + @ProcedureObjectsViewDelphi + ';4 @Interface = :[Interface:Language]'
                                      +
                                      (
                                        SELECT
                                          [Pub].[ConCat](', @' + [Name] + ' = :' + [Name], '')
                                        FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
                                      )
                                    )
        ELSE N''
      END
      + N',
      [Script]                = @Script,
      [DFM]                   = @DFM

    IF @Debug = 1 BEGIN
      EXEC [SQL].[Print] @Text = @Script
      EXEC [SQL].[Print] @Text = @DFM
    END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID
  END CATCH
GO'

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    IF @SQL IS NULL
      SELECT
        [ParentClassName]             = @ParentClassName,
        [ObjectName]                  = @ObjectName,
        [KeyField]                    = @KeyField,
        [IdentityField]               = @IdentityField,
        [ParentField]                 = @ParentField,
        [IdentityParams]              = @IdentityParams,
        [ProcedureObjectsView]        = @ProcedureObjectsView,
        [ObjectViewClassName]         = @ObjectViewClassName,
        [DirectDelete]                = @DirectDelete,
        [Translation]                 = @Translation,
        [Category]                    = @Category,
        [Caption]                     = @Caption,
        [Object_Id]                   = @Object_Id,
        [ProcedureObjectsViewDelphi]  = @ProcedureObjectsViewDelphi,
        [ProcedureObjectView]         = @ProcedureObjectView,
        [ProcedureObjectModify]       = @ProcedureObjectModify,
        [ProcedureObjectDelete]       = @ProcedureObjectDelete,
        [IsSecurityModeTable]         = @IsSecurityModeTable,
        [ObjectPrivilegesView]        = @ObjectPrivilegesView,

        [ObjectName2]                 = @Object2Name,
        [KeyField2]                   = @KeyField2,
        [IdentityField2]              = @IdentityField2,
        [MasterFields2]               = @MasterFields2,
        [DetailFields2]               = @DetailFields2,
        [ObjectViewClassName2]        = @Object2ViewClassName,
        [DirectDelete2]               = @DirectDelete2,
        [Translation2]                = @Translation2,
        [Object_Id2]                  = @Object2_Id,
        [ProcedureObjectModify2]      = @ProcedureObject2Modify,
        [ProcedureObjectDelete2]      = @ProcedureObject2Delete,
        [IsSecurityModeTable2]        = @IsSecurityModeTable2,
        [Object2PrivilegesView]       = @Object2PrivilegesView,

        [ObjectName3]                 = @Object3Name,
        [KeyField3]                   = @KeyField3,
        [IdentityField3]              = @IdentityField3,
        [MasterFields3]               = @MasterFields3,
        [DetailFields3]               = @DetailFields3,

        [ObjectViewClassName3]        = @Object3ViewClassName,
        [DirectDelete3]               = @DirectDelete3,
        [Translation3]                = @Translation3,
        [Object_Id3]                  = @Object3_Id,
        [ProcedureObjectModify3]      = @ProcedureObject3Modify,
        [ProcedureObjectDelete3]      = @ProcedureObject3Delete,
        [IsSecurityModeTable3]        = @IsSecurityModeTable3,
        [Object3PrivilegesView]       = @Object3PrivilegesView

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 2
  END CATCH
GO
-- (Delphi);4
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];3
  @ClassName                  VarChar(256),
  @ObjectName                 SysName,
  @IdentityParams             VarChar(256),
  @ProcedureObjectsView       SysName,

  @Category                   VarChar(256),
  @Caption                    VarChar(256),

  @ProcedureObjectsViewDelphi SysName,
  @IsSecurityModeTable        Bit,
  @IsSecurityModeProcedure    Bit,

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @SQL NVarChar(Max)
  
  BEGIN TRY
    SET @OUT_Object = @ProcedureObjectsViewDelphi + N';4'
    SET @SQL =
N'--===============================================================================================================
-- <Назначение>:        Процедура доопределения Instance-версии DFM для журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*'
          +
          (
            SELECT
              N'
  EXEC ' + @OUT_Object + N' @Interface = ''RU''' + IsNull([Pub].[ConCat](', @' + [Name] + N' = ?', N''), N'')
              + N'
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object
              + N'
  @Interface        Char(2)'
              +
              IsNull
              (
                [Pub].[ConCat]
                (
                  ',
  @' + [Name] + Replicate(' ', [Pub].[Is Negative Int](16 - Len([Name]), 0)) + N' ' + [Value],
                  ''
                ),
                N''
              )
              + N',
  @Debug            Bit         = 0
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @Script     NVarChar(Max),
    @DFM        NVarChar(Max)'
              +
              CASE
                WHEN @IsSecurityModeTable = 1 OR @IsSecurityModeProcedure = 1 THEN N',
    @Params     TParams,
    @Fields     VarChar(Max)'
                ELSE N''
              END
              + N',
    @Privileges SmallInt  = 256 | ' + CASE WHEN @IsSecurityModeTable = 1 THEN '13' ELSE '15' END + N'

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)'

              +
              CASE
                WHEN @IsSecurityModeTable = 1 OR @IsSecurityModeProcedure = 1 THEN N'

    SET @Params = TParams::New()'
              +
              IsNull
              (
                [Pub].[ConCat]
                (
                  '
    SET @Params.AddParam(' + [Pub].[Quote String]([Name]) + N', @' + [Name] + N')',
                  N''
                ),
                N''
              )
              + N'
    EXEC ' + @ProcedureObjectsView + ' @Interface = @Interface, @Params = @Params'
                ELSE N''
              END
              +
              CASE
                WHEN @IsSecurityModeTable = 1 THEN N'

    SET @Fields = ' + LEFT(@ObjectName, LEN(@ObjectName) - 1) + N'(Privileged Row Fields)](2, @Params)
    IF @Fields IS NOT NULL
      SET @Privileges |= 2'
                ELSE N''
              END

              + N'

    SET @DFM = N''
  Caption = ''
          + [Pub].[Quote String]
            (
              ' + CASE WHEN @Category IS NOT NULL THEN '[GUI].[Delphi::Translate](''MAINMENU'', ' + [Pub].[Quote String](@Category) + ', @Interface)
              + '' :: '' + ' ELSE '' END
              + '[GUI].[Delphi::Translate](''MAINMENU'', ' + [Pub].[Quote String](@Caption) + ', @Interface)'
              +
              IsNull
              (
                [Pub].[ConCat]
                (
                  '
              + '' :: '' + [GUI].[Delphi::Translate](''MAINMENU'', <@' + [Name] + '>, @Interface)',
                  N''
                ),
                N''
              )
              + N'
            )

    IF @Debug = 1 BEGIN
      EXEC [SQL].[Print] @Text = @Script
      EXEC [SQL].[Print] @Text = @DFM
    END

    SELECT
      [Privileges]  = @Privileges'
              +
              CASE
                WHEN @IdentityParams <> N'' THEN N',
      [Identity]    = ' + (SELECT [Pub].[ConCat]('Cast(@' + [Name] + N' AS VarChar)', ' + '';'' + ') FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';'))
                ELSE N''
              END
              + N',
      [Script]      = @Script,
      [DFM]         = @DFM'
            FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
          )
          + N'

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 4
  END CATCH
GO'

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    IF @SQL IS NULL
      SELECT
        [ObjectName]                  = @ObjectName,
        [IdentityParams]              = @IdentityParams,
        [ProcedureObjectsView]        = @ProcedureObjectsView,
        [Category]                    = @Category,
        [Caption]                     = @Caption,
        [ProcedureObjectsViewDelphi]  = @ProcedureObjectsViewDelphi,
        [IsSecurityModeTable]         = @IsSecurityModeTable,
        [IsSecurityModeProcedure]     = @IsSecurityModeProcedure

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 3
  END CATCH
GO
-- (Delphi);5
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];4
  @ObjectName                 SysName,
  @ClassName                  VarChar(256),
  @IdentityParams             VarChar(256),
  @ProcedureObjectsView       SysName,

  @Category                   VarChar(256),
  @Caption                    VarChar(256),

  @ProcedureObjectsViewDelphi SysName,

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @SQL NVarChar(Max)
  
  BEGIN TRY
    SET @OUT_Object = @ProcedureObjectsViewDelphi + N';5'
    SET @SQL =
N'--===============================================================================================================
-- <Назначение>:        Процедура создания кнопки вызова журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*
  DECLARE
    @MenuItems      NVarChar(Max),
    @ItemsReactions NVarChar(Max)'
        +
        (
          SELECT
            N'

  EXEC ' + @OUT_Object + N' @Interface = ''RU'''
            +
            CASE
              WHEN @IdentityParams IS NOT NULL THEN N', @ItemReactionConstants = ''' + IsNull([Pub].[ConCat](QuoteName([Name]) + N'=?', N';'), N'') + ''''
              ELSE N''
            END
            + N', @MenuItems = @MenuItems OUT, @ItemsReactions = @ItemsReactions OUT
  EXEC [SQL].[Print] @Text = @MenuItems
  EXEC [SQL].[Print] @Text = @ItemsReactions
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N'
  @Interface                Char(2),

  @MenuItemName             NVarChar(128) = NULL  OUT,  -- Имя кнопки
  @MenuItemCaption          NVarChar(250) = NULL  OUT,  -- Капшин кнопки
  @MenuItemImageIndex       SmallInt      = NULL,       -- Картинка кнопки
  @MenuItemCategoryIndex    TinyInt       = NULL,       -- Индекс категории bxBar''a

  @ItemReactionConfirmation NVarChar(512) = NULL,       -- Вопрос перед запуском
  @ItemReactionConstants    NVarChar(Max) = NULL,       -- Константы запуска в формате TParams "[<NAME1>]:<TYPE1>=<VALUE1>;[<NAME2>]:<TYPE2>=<VALUE2>"

  @MenuItems                NVarChar(Max)         OUT,
  @ItemsReactions           NVarChar(Max) = NULL  OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @Params     TParams,
    @Enabled    Bit'
            +
            IsNull
            (
              [Pub].[ConCat]
              (
                N',
    @' + [Name] + Replicate(' ', [Pub].[Is Negative Int](10 - Len([Name]), 0)) + N' ' + [Value],
                N''
              )
              , N''
            )
            + N'

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)

    SET @Params = @ItemReactionConstants
    EXEC @Enabled = ' + @ProcedureObjectsView + N' @Interface = @Interface, @Params = @Params, @RaiseError = 0'
            +
            IsNull
            (
              [Pub].[ConCat]
              (
                N'
    SET @' + [Name] + ' = @Params.' + [SQL].[TParams::Method By Type]([Value]) + N'(' + [Pub].[Quote String](Name) + N')',
                N''
              ),
              ''
            )
            + N'

    IF @MenuItemName IS NULL
      SET @MenuItemName = ' + [Pub].[Quote String]('mm' + Replace(Replace(Replace(Replace(Replace(@ObjectName, ' ', ''), '[', ''), ']', ''), '.', ''), ':', ''))
            +
            IsNull
            (
              [Pub].[ConCat](N' + ''_'' + Cast(@' + [Name] + N' AS NVarChar)', N''),
              N''
            )

            + N'

    IF @MenuItemCaption IS NULL
      SET @MenuItemCaption = ' + [Pub].[Quote String](@Caption) + '
    SET @MenuItemCaption = [GUI].[Delphi::Translate](''MAINMENU'', @MenuItemCaption, @Interface)

    --IF @MenuItemImageIndex IS NULL
    --  SET @MenuItemImageIndex = [GUI].[Delphi::IconLists::Get Icon Index](''Icon16List'', ''<IconName>'')

    SET @MenuItems = N''
    object '' + @MenuItemName + N'': TdxBarButton''
        +
        CASE
          WHEN @MenuItemCategoryIndex IS NOT NULL THEN N''
      Category = '' + Cast(@MenuItemCategoryIndex AS NVarChar)
          ELSE N''''
        END
        + N''
      Caption = '' + [Pub].[Quote String](@MenuItemCaption) 
        +
        CASE 
          WHEN @MenuItemImageIndex IS NOT NULL THEN ''
      ImageIndex = '' + Cast(@MenuItemImageIndex AS NVarChar) 
          ELSE N''''
        END
        + 
        CASE 
          WHEN @Enabled = 1 THEN N''
      OnClick = BaseItemClick''
          ELSE N''
      Enabled = False''
        END 
        + N''
    end''

    IF @Enabled = 1
      SET @ItemsReactions = N''
      item
        Item = '' + @MenuItemName
        + 
        CASE
          WHEN @ItemReactionConfirmation IS NOT NULL THEN N''
        OnClick.Confirmation  = '' + [Pub].[Quote String](@ItemReactionConfirmation) 
          ELSE N''''
        END
        + N''
        OnClick.ClassName     = ''''TSBaseFrm''''
        OnClick.ClassConstant = ' + [Pub].[Deep Quote String]('ClassName:String=' + @ClassName, 2)
            +
            IsNull
            (
              [Pub].[ConCat]
              (
                N'
        OnClick.ClassConstant = '''''
                +
                [Name] + N':'
                +
                CASE
                  WHEN [Value] IN ('Bit', 'TinyInt', 'SmallInt', 'Int', 'BigInt') THEN [Value] + '='' + Cast(@' + [Name] + ' AS NVarChar) + N'''''''
                  WHEN [Value] IN ('VarChar(Max)', 'NVarChar(Max)') OR [Value] LIKE 'VarChar%' OR [Value] LIKE 'NVarChar%' THEN 'String:'' + Replace(@' + [Name] + ', '''''', '''''''''')) + N'''''''
                  ELSE '<TYPE>='' + <@' +[Name] + '> + N'''
                END,
                N''
              ),
              N''
            )
            + N'
      end''

    RETURN @Enabled
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 5
  END CATCH
GO'
            FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
          )

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 4
  END CATCH
GO
-- (Delphi);7+8
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];5
  @ClassName                  VarChar(128),
  @Translation                Bit,
  @ProcedureObjectsViewDelphi SysName,
  @IdentityParams             VarChar(256),
  @Category                   NVarChar(128),
  @Caption                    NVarChar(128),

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @SQL NVarChar(Max)
  
  BEGIN TRY
    SET @OUT_Object = @ProcedureObjectsViewDelphi + ';7'
    SET @SQL =
N'--===============================================================================================================
-- <Назначение>:        Процедура построения дерева фильтра для журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*'
        +
        (
          SELECT
            N'
  EXEC ' + @OUT_Object + N' @Interface = ''RU''' + CASE WHEN @Translation = 1 THEN N', @Language = ''RU''' ELSE N'' END
            -- + IsNull([Pub].[Concat](N', @' + [Name] + N' = :' + [Name], N', '), N'')
            + N'
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N'
  @Interface  Char(2)'
  --          +
  --          IsNull
  --          (
  --            [Pub].[Concat]
  --            (
  --              N',
  --@' + [Name] + Replicate(' ', [Pub].[Is Negative Int](10 - Len([Name]), 0)) + N' ' + [Value],
  --              N''
  --            ),
  --            N''
  --          )
            +
            CASE
              WHEN @Translation = 1 THEN N',
  @Language   Char(2)'
              ELSE N''
            END
            + N'
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)'
        +
        CASE
          WHEN @Translation = 1 THEN N'

    IF @Language IS NULL
      RaisError(''Не задан обязательный параметр @Language'', 16, 2)'
          ELSE N''
        END
        + N'

    SELECT
      [Code]      = ''Types'',
      [Title]     = CAST([GUI].[Delphi::Translate](Default, N''Типы'', @' + CASE WHEN @Translation = 1 THEN N'Language' ELSE N'Interface' END + ') AS VarChar(255)),
      [SQL]       = ''EXEC ' + @ProcedureObjectsViewDelphi + ';8 @Interface = :[Interface:Language]' + CASE WHEN @Translation = 1 THEN N', @Language = :Language' ELSE N'' END + ', @Method = ''''Types'''''
      + IsNull([Pub].[Concat](N', @' + [Name] + N' = :' + [Name], N', '), N'')
      + N''',
      [Expand]    = 255,
      [Expanded]  = 255,
      [CanSelect] = 0,
      [Fields]    = ''Type_Id''

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 7
  END CATCH
GO
--===============================================================================================================
-- <Назначение>:        Процедура построения дерева фильтра для журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*'
        +
            N'
  EXEC ' + @ProcedureObjectsViewDelphi + N';8 @Interface = ''RU'', @Method = ''Types''' + CASE WHEN @Translation = 1 THEN N', @Language = ''RU''' ELSE N'' END + IsNull([Pub].[Concat](N', @' + [Name] + N' = :' + [Name], N', '), N'') + N'
*/
--===============================================================================================================
CREATE PROCEDURE ' + @ProcedureObjectsViewDelphi + N';8
  @Interface  Char(2),
  @Method     VarChar(100)  = NULL'
            +
            CASE
              WHEN @Translation = 1 THEN N',
  @Language   Char(2)'
              ELSE N''
            END
            +
            IsNull
            (
              [Pub].[Concat]
              (
                N',
  @' + [Name] + Replicate(' ', [Pub].[Is Negative Int](10 - Len([Name]), 0)) + N' ' + [Value],
                N''
              ),
              N''
            )
            + N'
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)'
        +
        CASE
          WHEN @Translation = 1 THEN N'

    IF @Language IS NULL
      RaisError(''Не задан обязательный параметр @Language'', 16, 2)'
          ELSE N''
        END
        + N'

    IF @Method = ''Types''
      SELECT
        [Id],
        [Parent_Id],
        [Image],
        [ImageFocused],
        [Text],
        [Type_Id]
      FROM
      (  
        SELECT
          [Id]            = CAST(0 AS VarChar(10)),      
          [Parent_Id]     = CAST(NULL AS VarChar(10)),       
          [Image]         = [GUI].[Delphi::IconLists::Get Icon Index](''Icon16List'', ''ROOT''),
          [ImageFocused]  = [GUI].[Delphi::IconLists::Get Icon Index](''Icon16List'', ''ROOT''),
          [Text]          = CAST([GUI].[Delphi::Translate](Default, N''Root level'', @' + CASE WHEN @Translation = 1 THEN N'Language' ELSE N'Interface' END + ') AS VarChar(255)),
          [Type_Id]       = CAST(NULL AS TinyInt),
          [SortIndex]     = CAST(NULL AS TinyInt)
        UNION ALL
        SELECT
          [Id]            = [Id],
          [Parent_Id]     = CAST(0 AS VarChar(10)), 
          [Image]         = [GUI].[Delphi::IconLists::Get Icon Index](''Icon16List'', ''FOLDER:DEFAULT:CLOSED''),
          [ImageFocused]  = [GUI].[Delphi::IconLists::Get Icon Index](''Icon16List'', ''FOLDER:DEFAULT:OPENED''),
          [Text]          = [Names],
          [Type_Id]       = [Id],
          [SortIndex]     = [SortIndex]
        FROM <Schema>.<Name>
      ) F
      ORDER BY [SortIndex]'
          FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
        )
        + N'

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 8
  END CATCH
GO'

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 5
  END CATCH
GO
-- Views;1
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];6
  @ClassName                  VarChar(128),
  @ObjectName                 SysName,
  @IdentityParams             VarChar(256),
  @ProcedureObjectsView       SysName,

  @IsSecurityModeTable        Bit,
  @IsSecurityModeProcedure    Bit,

  @Category                   NVarChar(128),
  @Caption                    NVarChar(128),

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @SQL NVarChar(Max)
  
  BEGIN TRY
    SET @OUT_Object = @ProcedureObjectsView
    SET @SQL =
N'--===============================================================================================================
-- <Назначение>:        Процедура проверки прав для вызова журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*'
        +
        (
          SELECT
            N'
  EXEC ' + @OUT_Object + N' @Interface = ''RU''' + CASE WHEN @IdentityParams IS NOT NULL THEN N', @Params = ''' + [Pub].[ConCat](QuoteName([Name]) + N'=?', N';') + '''' ELSE N'' END
            + N'
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N'
  @Interface    Char(2),
  @Params       TParams = NULL,
  @RaiseError   Bit     = 1
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON
  
  DECLARE'
            +
            CASE
              WHEN @IsSecurityModeTable = 0 AND @IsSecurityModeProcedure = 0 THEN N'
    @Enabled  Bit'
              ELSE N'
    @Fields         VarChar(Max)'
                +
                IsNull
                (
                  [Pub].[ConCat]
                  (
                    N',
    @' + [Name] + Replicate(' ', [Pub].[Is Negative Int](14 - Len([Name]), 0)) + N' ' + [Value],
                    N''
                  ),
                  N''
                )
            END
            + N'

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)'
            +
            CASE
              WHEN @IsSecurityModeTable = 0 AND @IsSecurityModeProcedure = 0 THEN N'
    EXEC @Enabled = [System].[Execute Privilege(Extended)] @Object = ' + [Pub].[Quote String](@ProcedureObjectsView) + N'

    IF @Enabled = 1'
              ELSE
                IsNull
                (
                  [Pub].[ConCat]
                  (
                    N'

    SET @' + [Name] + ' = @Params.' + [SQL].[TParams::Method By Type]([Value]) + N'(' + [Pub].[Quote String](Name) + ')
    IF @' + [Name] + ' IS NULL
      RaisError(''Не передано значение обязательного параметра @' + [Name] + ''', 16, 2)',
                    N''
                  ),
                  N''
                )
                + 
                CASE
                  WHEN @IsSecurityModeProcedure = 1 THEN N'

    EXEC [Security].[Privileged Row Fields]
      @Object     = ' + [Pub].[Quote String](@ProcedureObjectsView) + N',
      @Privilege  = 256,
      @Params     = ' + CASE WHEN @IdentityParams IS NOT NULL THEN N'@Params' ELSE N'NULL' END + N',
      @Fields     = @Fields OUT'
                  ELSE N'

    SET @Fields = ' + LEFT(@ObjectName, LEN(@ObjectName) - 1) + N'(Privileged Row Fields)](1, ' + CASE WHEN @IdentityParams IS NOT NULL THEN N'@Params' ELSE N'NULL' END + N')'
                END
                + N'

    IF @Fields IS NOT NULL'
            END

      + N'
      RETURN 1
    ELSE IF @RaiseError = 1
      RaisError(50229, 16, 1, ''EXECUTE'', ' + [Pub].[Quote String](@ProcedureObjectsView) + ')
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID
  END CATCH
GO'
          FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
        )

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 6
  END CATCH
GO
-- Views;2
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];7
  @ClassName                  VarChar(128),
  @ObjectName                 SysName,
  @Object2Name                SysName,
  @Object_Id                  Int,
  @IdentityField              SysName,
  @IdentityFieldType          SysName,
  @KeyField                   SysName,
  @KeyFieldType               SysName,
  @IdentityParams             VarChar(256),
  @ProcedureObjectsView       SysName,
  @InsertableControl          Bit,
  @Translation                Bit,

  @MasterFields2              VarChar(512),
  @DetailFields2              VarChar(512),

  @ObjectTranslateViewName    VarChar(256),
  @ObjectPrivilegesView       VarChar(256),
  @ObjectViewClassName        VarChar(256),

  @IsSecurityModeTable        Bit,
  @IsSecurityModeTable2       Bit,
  @IsSecurityModeProcedure    Bit,

  @Category                   NVarChar(128),
  @Caption                    NVarChar(128),

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @@IdentityField     SysName,
    @@IdentityFieldType SysName,
    @SQL                NVarChar(Max),
    @Tab                NVarChar(10)

  BEGIN TRY
    SET @@IdentityField     = IsNull(@IdentityField, @KeyField)
    SET @@IdentityFieldType = IsNull(@IdentityFieldType, @KeyFieldType)

    SET @Tab = CASE WHEN (@IsSecurityModeTable = 1 OR @IsSecurityModeProcedure = 0) AND @InsertableControl = 1 THEN N'  ' ELSE N'' END

    SET @OUT_Object = @ProcedureObjectsView + N';2'
    SET @SQL =
N'--===============================================================================================================
-- <Назначение>:        Процедура выборки данных для журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*'
        +
        (
          SELECT
            N'
  EXEC ' + @OUT_Object + N' @Interface = ''RU''' + CASE WHEN @Translation = 1 THEN N', @Language = ''RU''' ELSE N'' END + CASE WHEN @IdentityParams IS NOT NULL THEN N', @Params = ''' + [Pub].[ConCat](N', @' + [Name] + N' = ?', N'') + '''' ELSE N'' END
          FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
        )
            + N'
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N'
  @Interface            Char(2)'
      + CASE WHEN @Translation = 1 THEN N',
  @Language             Char(2)' ELSE N'' END
      +
      (
        SELECT
          IsNull
          (
            [Pub].[ConCat]
            (
              ',
  @' + [Name] + Replicate(' ', [Pub].[Is Negative Int](20 - Len([Name]), 0)) + N' ' + [Value], ''),
            ''
          )
        FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
      )


      + N',
  @'
      + @@IdentityField
      + Replicate(' ', [Pub].[Is Negative Int](20 - Len(@@IdentityField), 0))
      + N' '
      + @@IdentityFieldType
      + Replicate(' ', [Pub].[Is Negative Int](13 - Len(@@IdentityFieldType), 0))
      + N' = NULL'
      +
      CASE
        WHEN @InsertableControl = 1 THEN N',
  @PrivilegeInsertable  Bit           = NULL OUT'
        ELSE N''
      END
      + N'
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON'
          + CASE
              WHEN (@IsSecurityModeTable = 0 AND @IsSecurityModeProcedure = 1) OR @InsertableControl = 1 THEN N'

  DECLARE
    @Params     TParams'
              ELSE N''
            END
          + N'

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)'
      +
      CASE
        WHEN @Translation = 1 THEN N'

    IF @Language IS NULL
      RaisError(''Не задан обязательный параметр @Language'', 16, 2)'
        ELSE N''
      END
      +
      CASE
        WHEN @InsertableControl = 1 AND (@IsSecurityModeTable = 1 OR @IsSecurityModeProcedure = 0) THEN N'

    IF @' + @@IdentityField + N' IS NULL BEGIN'
        WHEN @IsSecurityModeTable = 0 AND @IsSecurityModeProcedure = 1 THEN N'
'
        ELSE N''
      END
      +
      CASE
        WHEN (@IsSecurityModeTable = 0 AND @IsSecurityModeProcedure = 1) OR @InsertableControl = 1 THEN N'
    ' + @Tab + N'SET @Params = TParams::New()'
          +
          CASE
            WHEN @IdentityParams IS NULL THEN N'
      --SET @Params.AddParam(''NAME'', @VALUE)'
            ELSE
              (
                SELECT
                  [Pub].[ConCat]
                  (
                    N'
    ' + @Tab + N'SET @Params.AddParam(' + [Pub].[Quote String]([Name]) + N', @' + [Name] + N')',
                    N''
                  )
                FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
              )
          END
        ELSE
          N''
      END
      +
      CASE
        WHEN @IsSecurityModeTable = 0 AND @IsSecurityModeProcedure = 1 THEN N'
    ' + @Tab + N'EXEC ' + @ProcedureObjectsView + N' @Interface = @Interface, @Params = @Params'
        ELSE N''
      END
      +
      CASE
        WHEN @InsertableControl = 1 THEN N'
    ' + @Tab + N'SET @PrivilegeInsertable = CASE WHEN ' + LEFT(@ObjectName, LEN(@ObjectName) - 1) + N'(Privileged Row Fields)](2, @Params) IS NOT NULL THEN 1 ELSE 0 END'
        ELSE N''
      END
      +
      CASE
        WHEN @InsertableControl = 1 THEN N'
    END'
        ELSE N''
      END

    + N'

    SELECT'
        +
        (
          SELECT
            IsNull
            (
              [Pub].[ConCat]
              (
                N'
      ' + QuoteName(SC.[Name]) + Replicate(' ', [Pub].[Is Negative Int](25 - Len(SC.[Name]), 0)) + N' = T.' + QuoteName(SC.[Name]),
                N','
              ),
              N''
            )
          FROM sys.columns SC WITH (NOLOCK)
          LEFT JOIN [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';') IP ON SC.[name] = IP.[Name]
          WHERE SC.[object_Id] = @Object_Id AND IP.[Index] IS NULL
        )
        +
        CASE
          WHEN @IsSecurityModeTable = 1 THEN N',
      [Privilege:Updatable]       = ' + CASE WHEN @ObjectTranslateViewName IS NOT NULL THEN N'P' ELSE N'T' END + N'.[Privilege:Updatable],
      [Privilege:Deletable]       = ' + CASE WHEN @ObjectTranslateViewName IS NOT NULL THEN N'P' ELSE N'T' END + N'.[Privilege:Deletable]'
          ELSE N''
        END
        +
        CASE
          WHEN @IsSecurityModeTable2 = 1 THEN N',
      [Privilege2:Insertable]     = Cast(CASE WHEN ' + Left(@Object2Name, Len(@Object2Name) - 1) + N'(Privileged Row Fields)](2, (SELECT '
              -- + N' T.*'
              +
              (
                SELECT
                  [Pub].[Concat]
                  (
                    CASE WHEN M.[Value] = D.[Value] THEN N'T.' + QuoteName(M.[Value]) ELSE QuoteName(D.[Value]) + N' = T.' + QuoteName(M.[Value]) END
                    , N', '
                  )
                FROM [Pub].[Array To RowSet Of Values](@MasterFields2, ';') M
                INNER JOIN [Pub].[Array To RowSet Of Values](@DetailFields2, ';') D ON M.[Index] = D.[Index]
              )
              + N' FOR XML RAW(''Param''), ROOT(''Params''), TYPE, XMLSCHEMA)) IS NOT NULL THEN 1 ELSE 0 END AS Bit)'
          ELSE N''
        END
        +
        CASE
          WHEN @ObjectViewClassName IS NULL THEN N',
      [#ModifiedFields]           = Cast(NULL AS VarChar(256))'
          ELSE N''
        END
        + N'
    FROM '
        +
        CASE
          WHEN @ObjectTranslateViewName IS NOT NULL THEN @ObjectTranslateViewName + '(@Language)'
          WHEN @IsSecurityModeTable = 1 THEN @ObjectPrivilegesView
          ELSE @ObjectName
        END + N' T'
        +
        CASE
          WHEN @ObjectTranslateViewName IS NOT NULL AND @IsSecurityModeTable = 1 THEN N'
    INNER JOIN ' + @ObjectPrivilegesView + N' P ON T.' + QuoteName(IsNull(@IdentityField, @KeyField)) + ' = P.' + QuoteName(IsNull(@IdentityField, @KeyField))
          ELSE N''
        END
        + N'
    WHERE '
          +
          (
            SELECT
              IsNull
              (
                [Pub].[ConCat]
                (
                  'T.' + QuoteName([Name]) + N' = @' + [Name],
                  ' AND '
                ) + N'
      AND ',
                ''
              )
            FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
          )
        + N'(@' + @@IdentityField + ' IS NULL OR @' + @@IdentityField + ' = T.' + QuoteName(@@IdentityField) + ')
    ORDER BY 1

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 2
  END CATCH
GO'

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 7
  END CATCH
GO
-- Views;3
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];8
  @ClassName                  VarChar(128),
  @Object2Name                SysName,
  @Object2_Id                 Int,
  @DetailFields2              VarChar(1000),
  @IdentityField2             SysName,
  @IdentityField2Type         SysName,
  @KeyField2                  SysName,
  @KeyField2Type              SysName,
  @IdentityParams             VarChar(256),
  @ProcedureObjectsView       SysName,
  @Translation2               Bit,

  @Object2TranslateViewName   VarChar(256),
  @Object2PrivilegesView      VarChar(256),
  @Object2ViewClassName       VarChar(256),

  @IsSecurityModeTable2       Bit,
  @IsSecurityModeProcedure    Bit,

  @Category                   NVarChar(128),
  @Caption                    NVarChar(128),

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @SQL NVarChar(Max)

  BEGIN TRY
    SET @OUT_Object = @ProcedureObjectsView + N';3'
    SET @SQL =
N'--===============================================================================================================
-- <Назначение>:        Процедура выборки данных второго уровня для журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*'
        +
        (
          SELECT
            N'
  EXEC ' + @OUT_Object + N' @Interface = ''RU''' + CASE WHEN @Translation2 = 1 THEN N', @Language = ''RU''' ELSE N'' END + CASE WHEN @IdentityParams IS NOT NULL THEN N', @Params = ''' + [Pub].[ConCat](N', @' + [Name] + N' = ?', N'') + '''' ELSE N'' END
          FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
        )
        + N'
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N'
  @Interface            Char(2)'
      + CASE WHEN @Translation2 = 1 THEN N',
  @Language             Char(2)' ELSE N'' END
      +
      (
        SELECT
          IsNull
          (
            [Pub].[ConCat]
            (
              ',
  @' + [Name] + Replicate(' ', [Pub].[Is Negative Int](20 - Len([Name]), 0)) + N' ' + [Value], ''),
            ''
          )
        FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
      )


      + N',
  @'
      + IsNull(@IdentityField2, @KeyField2)
      + Replicate(' ', [Pub].[Is Negative Int](20 - Len(IsNull(@IdentityField2, @KeyField2)), 0))
      + N' '
      + IsNull(@IdentityField2Type, @KeyField2Type)
      + N' = NULL
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON'
          + CASE
              WHEN @IsSecurityModeTable2 = 0 AND @IsSecurityModeProcedure = 1 THEN N'

  DECLARE
    @Params     TParams'
              ELSE N''
            END
          + N'

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)'
      + CASE WHEN @Translation2 = 1 THEN N'

    IF @Language IS NULL
      RaisError(''Не задан обязательный параметр @Language'', 16, 2)' ELSE N'' END
      +
      CASE
        WHEN @IsSecurityModeTable2 = 0 AND @IsSecurityModeProcedure = 1 THEN N'

    SET @Params = TParams::New()'
          +
          (
            SELECT
              [Pub].[ConCat]
              (
                N'
    SET @Params.AddParam(' + [Pub].[Quote String]([Name]) + N', @' + [Name] + N')',
                N''
              )
            FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
          )
          + N'
    EXEC ' + @ProcedureObjectsView + N' @Interface = @Interface, @Params = @Params'
        ELSE
          N''
      END

    + N'

    SELECT'
        +
        (
          SELECT
            IsNull
            (
              [Pub].[ConCat]
              (
                '
      ' + QuoteName(SC.[Name]) + Replicate(' ', [Pub].[Is Negative Int](25 - Len(SC.[Name]), 0)) + N' = T.' + QuoteName(SC.[Name]),
                ','
              ),
              ''
            )
          FROM sys.columns SC WITH (NOLOCK)
          LEFT JOIN [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';') IP ON SC.[name] = IP.[Name]
          WHERE SC.[object_Id] = @Object2_Id AND IP.[Index] IS NULL
        )
        +
        CASE
          WHEN @IsSecurityModeTable2 = 1 THEN N',
      [Privilege:Updatable]       = ' + CASE WHEN @Object2TranslateViewName IS NOT NULL THEN N'P' ELSE N'T' END + N'.[Privilege:Updatable],
      [Privilege:Deletable]       = ' + CASE WHEN @Object2TranslateViewName IS NOT NULL THEN N'P' ELSE N'T' END + N'.[Privilege:Deletable]'
          ELSE N''
        END
        + CASE WHEN @Object2ViewClassName IS NULL THEN N',
      [#ModifiedFields]           = Cast(NULL AS VarChar(256))' ELSE N'' END
        + N'
    FROM '
        +
        CASE
          WHEN @Object2TranslateViewName IS NOT NULL THEN @Object2TranslateViewName + '(@Language)'
          WHEN @IsSecurityModeTable2 = 1 THEN @Object2PrivilegesView
          ELSE @Object2Name
        END + N' T'
        +
        CASE
          WHEN @Object2TranslateViewName IS NOT NULL OR @IsSecurityModeTable2 = 0 THEN N'
    INNER JOIN ' + @Object2PrivilegesView + N' P ON T.' + QuoteName(IsNull(@IdentityField2, @KeyField2)) + ' = P.' + QuoteName(IsNull(@IdentityField2, @KeyField2))
          ELSE N''
        END
        + N'
    WHERE '
          +
          (
            SELECT
              IsNull
              (
                [Pub].[ConCat]
                (
                  'T.' + QuoteName([Name]) + N' = @' + [Name],
                  ' AND '
                ) + N'
      AND ',
                ''
              )
            FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
          )
        + N'(@' + IsNull(@IdentityField2, @KeyField2) + ' IS NULL OR @' + IsNull(@IdentityField2, @KeyField2) + ' = T.' + QuoteName(IsNull(@IdentityField2, @KeyField2)) + ')
    ORDER BY '
        +
        (
          SELECT
            [Pub].[ConCat](N'T.' + QuoteName([Value]), N', ')
          FROM [Pub].[Array To RowSet Of Values](@DetailFields2, ';')
        )
        + N'

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 3
  END CATCH
GO'

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 8
  END CATCH
GO
-- Views;4
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];9
  @ClassName                  VarChar(128),
  @Object3Name                SysName,
  @Object3_Id                 Int,
  @DetailFields3              VarChar(1000),
  @IdentityField3             SysName,
  @IdentityField3Type         SysName,
  @KeyField3                  SysName,
  @KeyField3Type              SysName,
  @IdentityParams             VarChar(256),
  @ProcedureObjectsView       SysName,
  @Translation3               Bit,

  @Object3TranslateViewName   VarChar(256),
  @Object3PrivilegesView      VarChar(256),
  @Object3ViewClassName       VarChar(256),

  @IsSecurityModeTable3       Bit,
  @IsSecurityModeProcedure    Bit,

  @Category                   NVarChar(128),
  @Caption                    NVarChar(128),

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @SQL NVarChar(Max)
  
  BEGIN TRY
    SET @OUT_Object = @ProcedureObjectsView + N';4'
    SET @SQL =
N'--===============================================================================================================
-- <Назначение>:        Процедура выборки данных третьего уровня для журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*'
        +
        (
          SELECT
            N'
  EXEC ' + @OUT_Object + N' @Interface = ''RU''' + CASE WHEN @Translation3 = 1 THEN N', @Language = ''RU''' ELSE N'' END + CASE WHEN @IdentityParams IS NOT NULL THEN N', @Params = ''' + [Pub].[ConCat](N', @' + [Name] + N' = ?', N'') + '''' ELSE N'' END
          FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
        )
        + N'
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N'
  @Interface      Char(2)'
      + CASE WHEN @Translation3 = 1 THEN N',
  @Language       Char(2)' ELSE N'' END
      +
      (
        SELECT
          IsNull
          (
            [Pub].[ConCat]
            (
              ',
  @' + [Name] + Replicate(' ', [Pub].[Is Negative Int](14 - Len([Name]), 0)) + N' ' + [Value], ''),
            ''
          )
        FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
      )


      + N',
  @'
      + IsNull(@IdentityField3, @KeyField3)
      + Replicate(' ', [Pub].[Is Negative Int](14 - Len(IsNull(@IdentityField3, @KeyField3)), 0))
      + N' '
      + IsNull(@IdentityField3Type, @KeyField3Type)
      + N' = NULL
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON'
          + CASE
              WHEN @IsSecurityModeTable3 = 0 AND @IsSecurityModeProcedure = 1 THEN N'

  DECLARE
    @Params     TParams'
              ELSE N''
            END
          + N'

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)'
      + CASE WHEN @Translation3 = 1 THEN N'

    IF @Language IS NULL
      RaisError(''Не задан обязательный параметр @Language'', 16, 2)' ELSE N'' END
      +
      CASE
        WHEN @IsSecurityModeTable3 = 0 AND @IsSecurityModeProcedure = 1 THEN N'

    SET @Params = TParams::New()'
          +
          (
            SELECT
              [Pub].[ConCat]
              (
                '
    SET @Params.AddParam(' + [Pub].[Quote String]([Name]) + N', @' + [Name] + N')',
                ''
              )
            FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
          )
          + N'
    EXEC ' + @ProcedureObjectsView + N' @Interface = @Interface, @Params = @Params'
        ELSE
          N''
      END

    + N'

    SELECT'
        +
        (
          SELECT
            IsNull
            (
              [Pub].[ConCat]
              (
                N'
      ' + QuoteName(SC.[Name]) + Replicate(' ', [Pub].[Is Negative Int](25 - Len(SC.[Name]), 0)) + N' = T.' + QuoteName(SC.[Name]),
                N','
              ),
              N''
            )
          FROM sys.columns SC WITH (NOLOCK)
          LEFT JOIN [Pub].[Array To RowSet Of Named Values](@IdentityParams, N';') IP ON SC.[name] = IP.[Name]
          WHERE SC.[object_Id] = @Object3_Id AND IP.[Index] IS NULL
        )
        +
        CASE
          WHEN @IsSecurityModeTable3 = 1 THEN N',
      [Privilege:Updatable]       = ' + CASE WHEN @Object3TranslateViewName IS NOT NULL THEN N'P' ELSE N'T' END + N'.[Privilege:Updatable],
      [Privilege:Deletable]       = ' + CASE WHEN @Object3TranslateViewName IS NOT NULL THEN N'P' ELSE N'T' END + N'.[Privilege:Deletable]'
          ELSE N''
        END
        + CASE WHEN @Object3ViewClassName IS NULL THEN N',
      [#ModifiedFields]           = Cast(NULL AS VarChar(256))' ELSE N'' END
        + N'
    FROM '
        +
        CASE
          WHEN @Object3TranslateViewName IS NOT NULL THEN @Object3TranslateViewName + '(@Language)'
          WHEN @IsSecurityModeTable3 = 1 THEN @Object3PrivilegesView
          ELSE @Object3Name
        END + N' T'
        +
        CASE
          WHEN @Object3TranslateViewName IS NOT NULL OR @IsSecurityModeTable3 = 0 THEN N'
    INNER JOIN ' + @Object3PrivilegesView + N' P ON T.' + QuoteName(@IdentityField3) + ' = P.' + QuoteName(@IdentityField3)
          ELSE N''
        END
        + N'
    WHERE '
          +
          (
            SELECT
              IsNull
              (
                [Pub].[ConCat]
                (
                  'T.' + QuoteName([Name]) + N' = @' + [Name],
                  ' AND '
                ) + N'
      AND ',
                ''
              )
            FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
          )
        + N'(@' + IsNull(@IdentityField3, @KeyField3) + ' IS NULL OR @' + IsNull(@IdentityField3, @KeyField3) + ' = T.' + QuoteName(IsNull(@IdentityField3, @KeyField3)) + ')
    ORDER BY '
        +
        (
          SELECT
            [Pub].[ConCat](N'T.' + QuoteName([Value]), N', ')
          FROM [Pub].[Array To RowSet Of Values](@DetailFields3, ';')
        )
        + N'

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 4
  END CATCH
GO'

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 9
  END CATCH
GO
-- View(Delphi);1
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];10
  @ObjectName                 SysName,
  @Object_Id                  Int,
  @KeyField                   SysName,
  @IdentityField              SysName,
  @IdentityFieldType          SysName,
  @MasterFields2              VarChar(1000),
  @DetailFields2              VarChar(1000),
  @ObjectViewClassName        VarChar(256),
  @ObjectViewIdentityParams   VarChar(256),
  @ObjectPrivilegesView       VarChar(256),
  @CaptionFormatSingle        VarChar(256),
  @ProcedureObjectViewDelphi  SysName,
  @ProcedureObjectView        SysName,
  @ProcedureObjectModify      SysName,
  @Translation                Bit,
  @PeriodicFields             VarChar(Max),
  @PeriodicAgeFieldName       SysName,

  @IsSecurityModeTable        Bit,

  @Category                   NVarChar(128),
  @Caption                    NVarChar(128),

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @@IdentityField     SysName,
    @SQL                NVarChar(Max)
  
  BEGIN TRY
    SET @@IdentityField     = IsNull(@IdentityField, @KeyField)

    SET @OUT_Object = @ProcedureObjectViewDelphi
    SET @SQL =
N'--===============================================================================================================
-- <Назначение>:        Процедура регистрации DFM для просмотра элемента журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ObjectViewClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*
  EXEC ' + @OUT_Object + N' @Interface = ''RU''
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N'
  @Interface  Char(2),
  @Debug      Bit       = 0
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @Script NVarChar(Max),
    @DFM    NVarChar(Max)

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)

    SET @Script = N''
procedure InternalCreate;
begin
  with Form do begin
    DataHandlerMain.OnBeforeValidate := @DataHandlerMainBeforeValidate;    
  end;  
end;

procedure AfterSkinChanged;
var
  LHeight : Integer;
begin
  with Form do begin
    Constraints.MinHeight := 0;
    Constraints.MaxHeight := 0;
    AdjustSize := pTsMain;
    Constraints.MinHeight := Height;
    Constraints.MaxHeight := Height;
  end;
end;

procedure DataHandlerMainBeforeValidate(Sender: TCustomDataHandler; var ADone: Boolean);
begin
  with Form do begin    
    
  end;  
end;''

    SET @DFM = N'''
      +
      CASE
        WHEN @ObjectViewIdentityParams IS NULL THEN N'
  Caption = '' + [Pub].[Quote String]([GUI].[Delphi::Translate](''MENU'', ' + [Pub].[Quote String](IsNull(@CaptionFormatSingle, 'Имя формы')) + N', @Interface)) + N'''
        ELSE
          N''
      END
      + N'
  IconIndex = '' + Cast([GUI].[Delphi::IconLists::Get Icon Index](''Icon16List'', ''DICTIONARY'') AS NVarChar) + N''

  inherited DataSetMain: TSQLDataSetProvider
    FieldsAutomations.ModifiAbleFieldsFieldName = ''''DataSetMain:ModifiAbleFields'''''
      +
      CASE
        WHEN @PeriodicFields IS NOT NULL THEN N'
    FieldsAutomations.ModifiedFieldsFieldName = ''''DataSetMain:UpdatedFields'''''
        ELSE N''
      END
  +
  (
    SELECT
      N'
    SQL.Strings =
    (
      ''''EXEC ' + @ProcedureObjectView + N';2''''
      ''''  @Interface            = :[Interface:Language]'
      +
      CASE
        WHEN @Translation = 1 THEN N',''''
      ''''  @Language             = :Language'
        ELSE N''
      END
      +
      IsNull
      (
        [Pub].[ConCat]
        (
          N',''''
      ''''  @' + C.[IdentityName] + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(C.[IdentityName]), 0)) + N' = :' + C.[IdentityName],
          N''
        ),
        N''
      )
      + N',''''
      ''''  @' + @@IdentityField + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(@@IdentityField), 0)) + N' = :' + @@IdentityField
      + N'''''
    )
    SQLInsert.Strings =
    (
      ''''EXEC ' + @ProcedureObjectModify + N'''''
      ''''  @Interface            = :[Interface:Language]'
      +
      CASE
        WHEN @Translation = 1 THEN N',''''
      ''''  @Language             = :Language'
        ELSE N''
      END
      +
      IsNull
      (
        [Pub].[ConCat]
        (
          CASE
            WHEN @PeriodicFields IS NULL OR C.[IdentityName] IS NOT NULL OR C.[Name] = @IdentityField THEN
              N',''''
      ''''  @'
              + C.[Name]
              + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(C.[Name]), 0))
              + N' = :'
              + CASE WHEN C.[Name] = @IdentityField THEN 'OUT_OLD_' ELSE '' END
              + C.[Name]
              + CASE WHEN C.[Name] = @IdentityField THEN ' OUT' ELSE '' END
          END,
          N''
        ),
        N''
      )
      +
      CASE
        WHEN @PeriodicFields IS NOT NULL AND @PeriodicAgeFieldName IS NULL THEN N',''''
      ''''  @AGE                  = :[AGE]'
        ELSE N''
      END
      +
      CASE
        WHEN @PeriodicFields IS NOT NULL THEN N',''''
      ''''  @PERIODIC             = :[DataSetPeriodic:Xml]'
        ELSE N',''''
      ''''  @FIELDS               = :[DataSetMain:UpdatedFields]'
      END
      + N',''''
      ''''  @UpdatedObjects       = :[OUT_OLD_DataHandler:UpdatedObjects] OUT''''
    )
    SQLUpdate.Strings =
    (
      ''''EXEC ' + @ProcedureObjectModify + N'''''
      ''''  @Interface            = :[Interface:Language]'
      +
      CASE
        WHEN @Translation = 1 THEN N',''''
      ''''  @Language             = :Language'
        ELSE N''
      END
      + N',''''
      ''''  @Lock_Id              = :[DataHandler:Lock:Id]'
      +
      +
      IsNull
      (
        [Pub].[ConCat]
        (
          CASE
            WHEN @PeriodicFields IS NULL OR C.[IdentityName] IS NOT NULL OR C.[Name] = @IdentityField THEN
              ',''''
      ''''  @'
              + C.[Name]
              + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(C.[Name]), 0))
              + N' = :'
              + CASE WHEN C.[Name] = @@IdentityField THEN 'OLD_' ELSE '' END + C.[Name]
          END,
          N''
        ),
        N''
      )
      +
      CASE
        WHEN @PeriodicFields IS NOT NULL AND @PeriodicAgeFieldName IS NULL THEN N',''''
      ''''  @AGE                  = :[AGE]'
        ELSE N''
      END
      +
      CASE
        WHEN @PeriodicFields IS NOT NULL THEN N',''''
      ''''  @PERIODIC             = :[DataSetPeriodic:Xml]'
        ELSE N',''''
      ''''  @FIELDS               = :[DataSetMain:UpdatedFields]'
      END

      + N',''''
      ''''  @UpdatedObjects       = :[OUT_OLD_DataHandler:UpdatedObjects] OUT''''
    )
  end'
      +
      CASE
        WHEN @PeriodicFields IS NOT NULL THEN N'

  object DataSetPeriodic: TSQLDataSetProvider
    FieldsAutomations.ServiceFields = ''' + @@IdentityField + N',Fake:Identity,DATE''
    ConnectionProvider = SQLConnectionProvider
    SQL.Strings =
    (
      ''''EXEC ' + @ProcedureObjectView + N';3''''
      ''''  @Interface            = :[Interface:Language]'
          +
          CASE
            WHEN @Translation = 1 THEN N',''''
      ''''  @Language             = :Language'
            ELSE N''
          END
          +
          IsNull
          (
            [Pub].[ConCat]
            (
              N',''''
      ''''  @' + C.[IdentityName] + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(C.[IdentityName]), 0)) + N' = :' + C.[IdentityName],
              N''
            ),
            N''
          )
          + N',''''
      ''''  @' + @@IdentityField + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(@@IdentityField), 0)) + N' = :' + @@IdentityField + N',''''
      ''''  @Mode                 = :Mode''''
    )
  end'
        ELSE N''
      END

      + N'

  inherited DataHandlerMain: TDataHandler
    DataSet = DataSetMain
    KeyFieldNames = ' + [Pub].[Deep Quote String](@KeyField, 2)
      + N'
    LockOptions.IdFieldName     = ''''DataHandler:Lock:Id''''
    LockOptions.MessageFieldName= ''''DataHandler:Lock:Message'''''
      +
      CASE
        WHEN @PeriodicFields IS NOT NULL THEN N'
    object DataHandlerPeriodic: TSubDataHandler
      DataSet                     = DataSetPeriodic
      OptionsMenu.ActionMode      = amKeyFields
      Settings.GenerateKeyField   = True
      KeyFieldNames               = ''Fake:Identity''
      MasterKeyFieldNames         = ' + [Pub].[Quote String](@@IdentityField) + N'
      DetailKeyFieldNames         = ' + [Pub].[Quote String](@@IdentityField) + N'
      XMLPost.FieldName           = ''DataSetPeriodic:Xml''
      XMLPost.ExcludedFields      = ' + [Pub].[Quote String](@@IdentityField + N',Fake:Identity') + N'
      XMLPost.StatusAttributeName = ''ACTION''
      XMLPost.UpdateStatusFilter  = [usInserted, usModified]
    end'
        ELSE N''
      END
      + N'
  end  

  inherited tsMain: TcxTabSheet
    Caption = '' + [Pub].[Quote String]([GUI].[Delphi::Translate](Default, ''Параметры'', @Interface)) + N''
  end'
      +
      CASE
        WHEN @PeriodicFields IS NOT NULL THEN N'
  inherited ptcPeriodic: TDBPeriodicTabControl
    Periodic.DefaultDate      = pddEmptyDataPage
    Periodic.DataHandler      = DataHandlerPeriodic
    Periodic.DateFieldName    = ''DATE''
    Periodic.AgeFieldName     = ' + [Pub].[Quote String](IsNull(@PeriodicAgeFieldName, N'AGE'))
        +
        IsNull
        (
          N'
    Periodic.StaticFieldNames = ' + [Pub].[Quote String]
                                    (
                                      [Pub].[Concat]
                                      (
                                        CASE
                                          WHEN C.[PeriodicName] IS NULL AND C.[IdentityName] IS NULL AND @KeyField <> C.[name] AND (@IdentityField IS NULL OR @IdentityField <> C.[name])
                                            THEN C.[name]
                                        END,
                                        N';'
                                      )
                                    )
          , N''
        )
      + N'
  end'
        ELSE N''
      END
      + N'
  inherited lpLayoutPanel: TLayoutPanel
    Caption.Text = '' + [Pub].[Quote String]([GUI].[Delphi::Translate](Default, ''<<Имя метки>>'', @Interface)) + N''
    Caption.Font.Style = [fsUnderline]
    inherited etEditText: TcxDBTextEdit
      DataBinding.DataSource = DataHandlerMain
      DataBinding.DataField  = ''<<SOME FIELD NAME>>''
    end
  end'''

    FROM
    (
      SELECT TOP 1000
        [ColumnName]    = C.[ColumnName],
        [IdentityName]  = C.[IdentityName],
        [IdentityType]  = C.[IdentityType],
        [PeriodicName]  = PF.[Value],
        [Name]          = C.[Name]
      FROM
      (
        SELECT
          [ColumnName]    = C.[name],
          [ColumnIndex]   = C.[column_id],
          [IdentityName]  = I.[Name],
          [IdentityType]  = I.[Value],
          [IdentityIndex] = I.[Index],
          [Name]          = IsNull(C.[name], I.[Name])
        FROM (SELECT C.[column_id], C.[name] FROM [sys].[columns] C WITH (NOLOCK) WHERE C.[object_Id] = @Object_Id) C
        FULL OUTER JOIN [Pub].[Array To RowSet Of Named Values](@ObjectViewIdentityParams, ';') I ON C.[name] = I.[Name]
      ) C
      LEFT JOIN [Pub].[Array To RowSet Of Values](@PeriodicFields, ',') PF ON C.[ColumnName] = PF.[Value]
      ORDER BY CASE WHEN C.[IdentityIndex] IS NOT NULL THEN 1 ELSE 2 END, IsNull(C.[IdentityIndex], C.[ColumnIndex])
    ) C
  )

  + N'

    SELECT
      [ClassName]             = ' + [Pub].[Quote String](@ObjectViewClassName)
        +
        CASE
          WHEN @ObjectViewIdentityParams <> '' THEN
            (
              SELECT
                N',
      [InstanceParams]        = ' + [Pub].[Quote String]([Pub].[Concat]([Name], ';')) + N',
      [Unique]                = Cast(0 AS Bit)'
                + N',
      [BeforeCreateProcedure] = ''EXEC ' + @ProcedureObjectViewDelphi + ';2 @Interface = :[Interface:Language]'
                +
                CASE WHEN @IdentityField <> N'' THEN N', @' + @IdentityField + N' = :' + @IdentityField ELSE [Pub].[Concat](', @' + [Name] + ' = :' + [Name], '') END
                + N''',
      [InstanceProcedure]     = ''EXEC ' + @ProcedureObjectViewDelphi + ';3 @Interface = :[Interface:Language]'
                +
                [Pub].[Concat](', @' + [Name] + ' = :' + [Name], '')
              FROM [Pub].[Array To RowSet Of Named Values](@ObjectViewIdentityParams, ';')
            )
            + N''''
          ELSE N''
        END
        + N',
      [Script]                = @Script,
      [DFM]                   = @DFM

    IF @Debug = 1 BEGIN
      EXEC [SQL].[Print] @Text = @Script
      EXEC [SQL].[Print] @Text = @DFM
    END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID
  END CATCH
GO'
    +
    CASE
      WHEN @ObjectViewIdentityParams <> '' AND @IdentityField <> N'' THEN
N'--===============================================================================================================
-- <Назначение>:        Процедура предобработки входящих параметров (BeforeСreateProcedure) для просмотра элемента журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ObjectViewClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*
  EXEC ' + @OUT_Object + N';2 @Interface = ''RU'', @' + @IdentityField + N' = ?
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N';2
  @Interface    Char(2),
  @' + @IdentityField + Replicate(' ', [Pub].[Is Negative Int](12 - Len(@IdentityField), 0)) + N' ' + @IdentityFieldType + N' = NULL
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)

    IF @' + @IdentityField + N' IS NOT NULL BEGIN
      SELECT'
      +
      (
        SELECT
          [Pub].[ConCat]
          (
            '
        ' + QuoteName([Name]) + Replicate(' ', [Pub].[Is Negative Int](12 - Len([Name]), 0)) + ' = T.' + QuoteName([Name]),
            ','
          )
        FROM [Pub].[Array To RowSet Of Named Values](@ObjectViewIdentityParams, ';')
      )
      + N'
      FROM ' + IsNull(@ObjectPrivilegesView, @ObjectName) + N' T
      WHERE T.' + QuoteName(@IdentityField) + N' = @' + @IdentityField + N'

      IF @@ROWCOUNT <> 1
        RaisError(''Запись с ' + QuoteName(@IdentityField) + N' = %d в базе данных не обнаружена'', 16, 1, @' + @IdentityField + N')
    END'

        + N'

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 2
  END CATCH
GO'
        +
        (
          SELECT N'
--===============================================================================================================
-- <Назначение>:        Процедура доопределения Instance-версии DFM для просмотра элемента журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ObjectViewClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*
  EXEC ' + @OUT_Object + N';3 @Interface = ''RU''' + [Pub].[ConCat](N', @' + [Name] + N' = ?', N'') + N'
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N';3
  @Interface        Char(2)'
            +
            [Pub].[ConCat]
            (
              ',
  @' + [Name] + Replicate(' ', [Pub].[Is Negative Int](12 - Len([Name]), 0)) + N' ' + [Value],
              ''
            )
            + N',
  @Debug            Bit       = 0
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @Script NVarChar(Max),
    @DFM    NVarChar(Max)

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)

    SET @DFM = N''
  Caption = ''
        + [Pub].[Quote String]
        (
          [GUI].[Delphi::Translate](''MENU'', ' + [Pub].[Quote String](IsNull(@CaptionFormatSingle, 'Имя формы')) + N', @Interface)
          + '' :: '''
            +
            [Pub].[ConCat]
            (
              '
          + <@' + [Name] + '>',
              ''
            )
            + N'
        )

    IF @Debug = 1 BEGIN
      EXEC [SQL].[Print] @Text = @Script
      EXEC [SQL].[Print] @Text = @DFM
    END

    SELECT
      [Identity]    = ' + [Pub].[ConCat]('Cast(@' + [Name] + N' AS NVarChar(1000))', ' + '';'' + ') + N',
      [Script]      = @Script,
      [DFM]         = @DFM

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 3
  END CATCH
GO'
          FROM [Pub].[Array To RowSet Of Named Values](@ObjectViewIdentityParams, ';')
        )
      ELSE
        N''
    END

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 10
  END CATCH
GO
-- View;1
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];11
  @ObjectName                 SysName,
  @ProcedureObjectView        SysName,
  @ObjectViewClassName        VarChar(128),
  @ObjectViewIdentityParams   VarChar(256),

  @IsSecurityModeTable        Bit,

  @Category                   NVarChar(128),
  @Caption                    NVarChar(128),

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @SQL NVarChar(Max)
  
  BEGIN TRY
    SET @OUT_Object = @ProcedureObjectView
    SET @SQL = 
N'--===============================================================================================================
-- <Назначение>:        Процедура проверки прав для вызова формы просмотра элемента журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ObjectViewClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*
  EXEC ' + @OUT_Object + N' @Interface = ''RU''' + CASE WHEN @ObjectViewIdentityParams IS NOT NULL THEN N', @Params = ''' + (SELECT [Pub].[ConCat](QuoteName([Name]) + N'=?', N';') FROM [Pub].[Array To RowSet Of Named Values](@ObjectViewIdentityParams, ';')) + N'''' ELSE N'' END + N'
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N'
  @Interface    Char(2),
  @Params       TParams = NULL,
  @RaiseError   Bit     = 1
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE'
      +
      CASE
        WHEN @IsSecurityModeTable = 0 THEN N'
    @Enabled  Bit'
        ELSE N'
    @Fields         VarChar(Max)'
      END
      + N'

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)'
      +
      CASE
        WHEN @IsSecurityModeTable = 0 THEN N'
    EXEC @Enabled = [System].[Execute Privilege(Extended)] @Object = ' + [Pub].[Quote String](@ProcedureObjectView) + N'

    IF @Enabled = 1'
        ELSE
        N'

    SET @Fields = ' + LEFT(@ObjectName, LEN(@ObjectName) - 1) + N'(Privileged Row Fields)](1, @Params)

    IF @Fields IS NOT NULL'
      END

      + N'
      RETURN 1
    ELSE IF @RaiseError = 1
      RaisError(50229, 16, 1, ''EXECUTE'', ' + [Pub].[Quote String](@ProcedureObjectView) + ')
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID
  END CATCH
GO'

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 11
  END CATCH
GO
-- View;2
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];12
  @Schema                     SysName,
  @Name                       SysName,
  @ObjectName                 SysName,
  @Object_Id                  Int,
  @KeyField                   SysName,
  @KeyFieldType               SysName,
  @IdentityField              SysName,
  @IdentityFieldType          SysName,
  @ProcedureObjectView        SysName,
  @Translation                Bit,

  @ObjectViewClassName        VarChar(128),
  @ObjectTranslateViewName    VarChar(256),
  @ObjectPrivilegesView       VarChar(256),
  @ObjectViewIdentityParams   VarChar(256),

  @IsSecurityModeTable        Bit,
  @PeriodicAgeFieldName       SysName,
  @PeriodicFields             NVarChar(Max),

  @Category                   NVarChar(128),
  @Caption                    NVarChar(128),

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @@IdentityField     SysName,
    @@IdentityFieldType SysName,
    @SQL                NVarChar(Max)

  BEGIN TRY
    SET @@IdentityField     = IsNull(@IdentityField, @KeyField)
    SET @@IdentityFieldType = IsNull(@IdentityFieldType, @KeyFieldType)

    SET @OUT_Object = @ProcedureObjectView + N';2'
    SET @SQL = 
N'--===============================================================================================================
-- <Назначение>:        Процедура выборки данных для формы элемента журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ObjectViewClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*'
          +
          (
            SELECT
              N'
  EXEC ' + @OUT_Object + N' @Interface = ''RU'''
              + CASE WHEN @Translation = 1 THEN N', @Language = ''RU''' ELSE N'' END
              + IsNull([Pub].[ConCat](CASE WHEN C.[IdentityName] IS NOT NULL OR C.[ColumnName] = @@IdentityField THEN N', @' + C.[Name] + N' = ?' END, N''), N'')
              + N', @Mode = ''View''
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N'
  @Interface            Char(2)'
          +
          CASE
            WHEN @Translation = 1 THEN N',
  @Language             Char(2)'
            ELSE N''
          END
              +
              IsNull
              (
                [Pub].[ConCat]
                (
                  CASE
                    WHEN C.[IdentityName] IS NOT NULL OR C.[ColumnName] = @@IdentityField THEN ',
  @' + C.[Name] + Replicate(' ', [Pub].[Is Negative Int](20 - Len(C.[Name]), 0)) + N' ' + C.[Type]
                  END,
                  N''
                ),
                N''
              )
          + N',
  @Mode                 VarChar(50),
  @Debug                Bit         = 0
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @Lock_Object      SysName       = ' + [Pub].[Quote String](@ObjectName) + ',
    @Lock_Key         NVarChar(50),
    @Lock_Message     NVarChar(256) = NULL,
    @Lock_Id          BigInt        = NULL'
        +
        CASE
          WHEN @PeriodicFields <> N'' THEN N',
    @PeriodicFields   NVarChar(Max)'
          ELSE N''
        END
        +
        CASE
          WHEN @IsSecurityModeTable = 1 THEN N',
    @Params           TParams,
    @InsertableFields VarChar(Max)'
          ELSE N''
        END
        + N'

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Не задан параметр @Interface'', 16, 2)'
          + CASE WHEN @Translation = 1 THEN N'
    IF @Language IS NULL
      RaisError(''Не задан параметр @Language'', 16, 2)' ELSE N'' END + N'

    IF @Mode IS NULL OR @Mode NOT IN (''View'', ''Edit'', ''Insert'', ''Copy'')
      RaisError(''Не поддерживаемое значение параметра @Mode = "%s"'', 16, 2, @Mode)

    IF @' + @@IdentityField + N' IS NULL AND @Mode IN (''View'', ''Edit'', ''Copy'')
      RaisError(''Не задан параметр @' + @@IdentityField + N' для режима "%s"'', 16, 2, @Mode)'
        +
        CASE
          WHEN @PeriodicFields <> N'' THEN N'

    SET @PeriodicFields = ' + QuoteName(@Schema) + N'.[Periodic::Fields Array](' + QuoteName(@Schema) + N'.[Periodic::Table::Id](' + [Pub].[Quote String](@Name) + N'), N'','')'
          ELSE N''
        END
        +
        CASE
          WHEN @IsSecurityModeTable = 1 THEN N'

    IF @Mode IN (''Insert'', ''Copy'') BEGIN
      SET @Params = TParams::New()'
            +
            IsNull
            (
              [Pub].[ConCat]
              (
                '
      SET @Params.AddParam(' + [Pub].[Quote String](C.[IdentityName]) + ', @' + C.[IdentityName] + ')',
                ''    
              ),
            N''
            )
            + N'
      SET @InsertableFields = ' + LEFT(@ObjectName, LEN(@ObjectName) - 1) + '(Privileged Row Fields)](2, @Params)
      IF @InsertableFields IS NULL
        RaisError(50229, 16, 1, ''INSERT'', ' + [Pub].[Quote String](@ObjectName) + N')
    END ELSE '
          ELSE N'
'
        END
        +
  N'IF @Mode = ''Edit'' BEGIN
      SET @Lock_Key = CAST(@' + @@IdentityField + N' AS NVarChar(50))

      EXEC [System].[Lock]
        @Interface      = @Interface,
        @Id             = @Lock_Id      OUT,
        @Object         = @Lock_Object,
        @Key            = @Lock_Key,
        @RaisError      = 0,
        @ErrorMessage   = @Lock_Message OUT,
        @Debug          = @Debug
    END

    IF @Mode IN (''View'', ''Edit'', ''Copy'') BEGIN
      SELECT'
        +
        [Pub].[ConCat]
        (
          CASE
            WHEN C.[IdentityName] IS NULL THEN '
        '
              + QuoteName(C.[ColumnName])
              + Replicate(' ', [Pub].[Is Negative Int](31 - Len(C.[ColumnName]), 0))
              + ' = '
              +
              CASE
                WHEN C.[ColumnName] <> @@IdentityField AND @IsSecurityModeTable = 1 THEN
                  'CASE WHEN @Mode <> ''Copy'' OR [Pub].[In Array](@InsertableFields, ' + [Pub].[Quote String](C.[ColumnName]) + ', '','') = 1 THEN T.' + QuoteName(C.[ColumnName]) + ' END'
                ELSE
                  'T.' + QuoteName(C.[ColumnName])
              END
              + ','
            ELSE ''
          END,
          ''
        )
        + N'
        [DataSetMain:UpdatedFields]       = CAST(NULL AS VarChar(8000)),
        [DataHandler:UpdatedObjects]      = CAST(NULL AS VarChar(8000)),
        [DataHandler:Lock:Id]             = @Lock_Id,
        [DataHandler:Lock:Message]        = @Lock_Message'
        +
        CASE
          WHEN @ObjectPrivilegesView IS NOT NULL THEN N',
        [DataSetMain:ModifiAbleFields]    = '
            +
            CASE
              WHEN @PeriodicFields <> N'' THEN N'[Pub].[Arrays Anti Join]
                                            (
                                              CASE @Mode
                                                WHEN ''Edit'' THEN P.[DataSetProvider:ModifiAbleFields]
                                                WHEN ''Copy'' THEN @InsertableFields
                                              END,
                                              @PeriodicFields,
                                              '',''
                                            ),
        [DataSetPeriodic:ModifiAbleFields]= [Pub].[Arrays Join]
                                            (
                                              CASE @Mode
                                                WHEN ''Edit'' THEN P.[DataSetProvider:ModifiAbleFields]
                                                WHEN ''Copy'' THEN @InsertableFields
                                              END,
                                              @PeriodicFields,
                                              '',''
                                            )'
          
                +
                CASE
                  WHEN @PeriodicAgeFieldName IS NULL THEN N',
        [DataSetPeriodic:Age]             = PRI.[Age]'
                  ELSE N''
                END
                +
                CASE
                  WHEN @PeriodicFields IS NULL THEN N',
        [DataSetPeriodic:Xml]             = Cast(NULL AS NVarChar(Max))'
                  ELSE N''
                END
              ELSE N'CASE @Mode
                                              WHEN ''Edit'' THEN P.[DataSetProvider:ModifiAbleFields]
                                              WHEN ''Copy'' THEN @InsertableFields
                                            END'
            END
          ELSE N''
        END
        + N'
      FROM ' + CASE WHEN @Translation = 1 THEN @ObjectTranslateViewName + '(@Language)' ELSE @ObjectName END + N' T'
        +
        CASE
          WHEN @ObjectPrivilegesView IS NOT NULL THEN N'
      CROSS APPLY
      (
        SELECT
          [DataSetProvider:ModifiAbleFields] =
            CASE
              WHEN @Mode = ''Edit'' THEN
                [Pub].[Arrays Merge]
                (
                  [Privileged Fields:Update],
                  [Pub].[Arrays Join]([Privileged Fields:Initialize], [Checked Fields:Null], '',''),
                  '',''
                )
            END
        FROM ' + @ObjectPrivilegesView + N'
        WHERE ' + QuoteName(@@IdentityField) + N' = @' + @@IdentityField
          +
          IsNull
          (
            [Pub].[ConCat]
            (
              ' AND ' + QuoteName(C.[IdentityName]) + ' = @' + C.[IdentityName],
              ''
            ),
            ''
          )
          + N'
      ) P'
          ELSE N''
        END
        +
        CASE
          WHEN @PeriodicFields <> N'' AND @PeriodicAgeFieldName IS NULL THEN N'
      CROSS APPLY ' + QuoteName(@Schema) + N'.[Periodic::Row::Info] (' + QuoteName(@Schema) + N'.[Periodic::Table::Id](' + [Pub].[Quote String](@Name) + N'), @' + @@IdentityField + N', Default) PRI'
          ELSE N''
        END
        + N'
      WHERE T.' + QuoteName(@@IdentityField) + N' = @' + @@IdentityField 
          +
          IsNull
          (
            [Pub].[ConCat]
            (
              CASE
                WHEN C.[IdentityName] IS NOT NULL AND C.[ColumnName] IS NOT NULL THEN ' AND T.' + QuoteName(C.[IdentityName]) + ' = @' + C.[IdentityName]
              END,
              ''
            ),
            ''
          )
          + N'

      -- Проверяем корректность значения @Id 
      IF @@ROWCOUNT <> 1
        RaisError(''Данная запись не найдена, либо у Вас отсутствуют права на её просмотр.'', 16, 1)
    END ELSE BEGIN
      SELECT'
        +
        [Pub].[ConCat]
        (
          CASE
            WHEN C.[IdentityName] IS NULL THEN '
        ' + QuoteName(C.[ColumnName]) + Replicate(' ', [Pub].[Is Negative Int](31 - Len(C.[ColumnName]), 0)) + ' = T.' + QuoteName(C.[ColumnName]) + ','
            ELSE ''
          END,
          ''
        )
        + N'
        [DataSetMain:UpdatedFields]       = CAST(NULL AS VarChar(8000)),
        [DataHandler:UpdatedObjects]      = CAST(NULL AS VarChar(8000)),
        [DataHandler:Lock:Id]             = @Lock_Id,
        [DataHandler:Lock:Message]        = @Lock_Message'
        +
        CASE
          WHEN @ObjectPrivilegesView IS NOT NULL THEN N',
        [DataSetMain:ModifiAbleFields]    = '
            +
            CASE
              WHEN @PeriodicFields <> N'' THEN N'[Pub].[Arrays Anti Join](@InsertableFields, @PeriodicFields, '',''),
        [DataSetPeriodic:ModifiAbleFields]= [Pub].[Arrays Join](@InsertableFields, @PeriodicFields, '','')'
                +
                CASE
                  WHEN @PeriodicAgeFieldName IS NULL THEN N',
        [DataSetPeriodic:Age]             = Cast(NULL AS Date)'
                  ELSE N''
                END
              ELSE N'@InsertableFields'
            END
          ELSE N''
        END
        + N'
      FROM (SELECT [Fake] = CAST(NULL AS Int)) N
      LEFT JOIN ' + @ObjectName + N' T ON 1 = 0 
    END'
      FROM
      (
        SELECT TOP 1000
          [ColumnName]    = C.[name],
          [ColumnType]    = C.[Type],
          [IdentityName]  = I.[Name],
          [IdentityType]  = I.[Value],
          [Name]          = IsNull(C.[name], I.[Name]),
          [Type]          = IsNull(C.[Type], I.[Value])
        FROM
        (
          SELECT
            [column_id] = C.[column_id],
            [name]      = C.[name],
            [Type]    =  [SQL].[Field Type Compile](TYPE_NAME(C.[system_type_id]), C.[max_length], C.[precision], C.[scale], NULL)
          FROM [sys].[columns] C WITH (NOLOCK)
          LEFT JOIN [Pub].[Array To RowSet Of Values](@PeriodicFields, ',') PF ON C.[name] = PF.[Value]
          WHERE C.[object_Id] = @Object_Id AND PF.[Index] IS NULL
        ) C
        FULL OUTER JOIN [Pub].[Array To RowSet Of Named Values](@ObjectViewIdentityParams, ';') I ON C.[name] = I.[Name]
        ORDER BY CASE WHEN I.[Index] IS NOT NULL THEN 1 ELSE 2 END, CASE WHEN C.[column_id] IS NOT NULL THEN C.[column_id] ELSE I.[Index] END
      ) C
    )
    + N'

    RETURN 1
  END TRY
  BEGIN CATCH
    IF @Lock_Id IS NOT NULL
      EXEC [System].[UnLock] @Id = @Lock_Id

    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 2
  END CATCH
GO'

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 12
  END CATCH
GO
-- View;3
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];13
  @Schema                     SysName,
  @Name                       SysName,
  @ObjectName                 SysName,
  @Object_Id                  Int,
  @KeyField                   SysName,
  @KeyFieldType               SysName,
  @KeyFieldUnique             Bit,
  @IdentityField              SysName,
  @IdentityFieldType          SysName,
  @ProcedureObjectView        SysName,
  @Translation                Bit,

  @ObjectViewClassName        VarChar(128),
  @ObjectPrivilegesView       VarChar(256),
  @IsSecurityModeTable        Bit,

  @PeriodicFields             NVarChar(Max),

  @Category                   NVarChar(128),
  @Caption                    NVarChar(128),

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @@IdentityField     SysName,
    @@IdentityFieldType SysName,
    @SQL                NVarChar(Max)
  
  BEGIN TRY
    IF @IdentityField IS NOT NULL BEGIN
      SET @@IdentityField     = @IdentityField
      SET @@IdentityFieldType = @IdentityFieldType
    END ELSE IF @KeyFieldUnique = 1 BEGIN
      SET @@IdentityField     = @KeyField
      SET @@IdentityFieldType = @KeyFieldType
    END ELSE
      RaisError('Abstract error', 16, 2)

    SET @OUT_Object = @ProcedureObjectView + N';3'
    SET @SQL = 
N'--===============================================================================================================
-- <Назначение>:        Процедура выборки данных периодики для формы элемента журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + @ObjectViewClassName + N'; 
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*
  EXEC ' + @OUT_Object + N' @Interface = ''RU'''
              + CASE WHEN @Translation = 1 THEN N', @Language = ''RU''' ELSE N'' END
              + N', @' + @@IdentityField + N' = ?, @Mode = ''View''
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N'
  @Interface            Char(2)'
          +
          CASE
            WHEN @Translation = 1 THEN N',
  @Language             Char(2)'
            ELSE N''
          END
          + N',
  @' + @@IdentityField + Replicate(' ', [Pub].[Is Negative Int](20 - Len(@@IdentityField), 0)) + N' ' + @@IdentityFieldType
          + N',
  @Mode                 VarChar(50),
  @Debug                Bit         = 0
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @Table_Id SmallInt

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Не задан параметр @Interface'', 16, 2)'
          +
          CASE
            WHEN @Translation = 1 THEN N'
    IF @Language IS NULL
      RaisError(''Не задан параметр @Language'', 16, 2)'
            ELSE N''
          END
          + N'

    IF @Mode IS NULL OR @Mode NOT IN (''View'', ''Edit'', ''Insert'', ''Copy'')
      RaisError(''Не поддерживаемое значение параметра @Mode = "%s"'', 16, 2, @Mode)

    IF @' + @@IdentityField + N' IS NULL AND @Mode IN (''View'', ''Edit'', ''Copy'')
      RaisError(''Не задан параметр @' + @@IdentityField + N' для режима "%s"'', 16, 2, @Mode)

    SET @Table_Id = ' + QuoteName(@Schema) + N'.[Periodic::Table::Id](' + [Pub].[Quote String](@Name) + N')'
          +
          CASE
            WHEN @IsSecurityModeTable = 1 THEN N'

    IF (@Mode <> ''Insert'') AND NOT EXISTS(SELECT TOP 1 1 FROM '
              + @ObjectPrivilegesView
              + N' WHERE '
              + QuoteName(@@IdentityField) + N' = @' + @@IdentityField
              + N')
      RaisError(50229, 16, 1, ''SELECT'', ' + [Pub].[Quote String](@ObjectName) + N')'
            ELSE N''
          END
          + N'

    SELECT
      [Fake:Identity]         = ROW_NUMBER() OVER (ORDER BY D.[Date]),
      ' + QuoteName(@@IdentityField) + Replicate(' ', [Pub].[Is Negative Int](21 - Len(@@IdentityField), 0)) + N' = @' + @@IdentityField
          + N',
      [DATE]                  = D.[Date]'
        +
        (
          SELECT
            [Pub].[ConCat]
            (
              N',
      '
              + QuoteName(C.[ColumnName])
              + Replicate(' ', [Pub].[Is Negative Int](21 - Len(C.[ColumnName]), 0))
              + ' = '
              + N'P.' + QuoteName(C.[ColumnName]),
              N''
            )
          FROM
          (
            SELECT TOP 1000
              [ColumnName]  = C.[name],
              [ColumnType]  = [SQL].[Field Type Compile](TYPE_NAME(C.[system_type_id]), C.[max_length], C.[precision], C.[scale], NULL)
            FROM [sys].[columns] C WITH (NOLOCK)
            INNER JOIN [Pub].[Array To RowSet Of Values](@PeriodicFields, ',') PF ON C.[name] = PF.[Value]
            WHERE C.[object_Id] = @Object_Id
            ORDER BY C.[column_id]
          ) C
        )
        + N'
    FROM ' + QuoteName(@Schema) + N'.[Periodic::Row::Periods](@Table_Id, @' + @@IdentityField + N') D
    CROSS APPLY (SELECT * FROM ' + QuoteName(@Schema) + N'.' + QuoteName(@Name + N'(Periodic)') + N'(D.[Date]' + CASE WHEN @Translation = 1 THEN N', @Language' ELSE N'' END + N') P WHERE P.' + QuoteName(@@IdentityField) + N' = @' + @@IdentityField + N') P
    WHERE @Mode <> ''Insert''

    IF (@Mode <> ''Insert'') AND (@@ROWCOUNT = 0)
      RaisError(''Периодика записи не найдена, либо у Вас отсутствуют права на её просмотр.'', 16, 1)

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 3
  END CATCH
GO'

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 13
  END CATCH
GO
-- Modify
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];14
  @Schema                     SysName,
  @Name                       SysName,
  @ClassName                  SysName,
  @ObjectName                 SysName,
  @Object_Id                  Int,
  @KeyField                   SysName,
  @KeyFieldType               SysName,
  @KeyFieldUnique             Bit,
  @IdentityField              SysName,
  @IdentityFieldType          SysName,
  @IdentityMethod             SysName,
  @ProcedureObjectModify      SysName,
  @Translation                Bit,

  @ObjectTranslateInsertName  VarChar(256),
  @ObjectTranslateUpdateName  VarChar(256),
  @ObjectPrivilegesView       VarChar(256),
  @ObjectViewClassName        VarChar(256),
  @ObjectViewIdentityParams   VarChar(256),

  @IsSecurityModeTable        Bit,
  @PeriodicAgeFieldName       SysName,
  @PeriodicFields             NVarChar(Max),
  
  @Category                   NVarChar(128),
  @Caption                    NVarChar(128),

  @Logging                    Bit,

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @LockAlgorithm      Bit,
    @@IdentityField     SysName,
    @@OLD_IdentityField SysName,
    @SQL                NVarChar(Max)

  BEGIN TRY
    SET @OUT_Object = @ProcedureObjectModify

    SET @LockAlgorithm = CASE WHEN @ObjectViewClassName IS NOT NULL AND (@IdentityField IS NOT NULL OR @KeyFieldUnique = 1) THEN 1 ELSE 0 END

    SET @@IdentityField     = IsNull(@IdentityField, @KeyField)
    SET @@OLD_IdentityField = CASE WHEN @IdentityField IS NULL THEN N'OLD_' + @KeyField ELSE @IdentityField END

    SET @SQL =
N'--===============================================================================================================
-- <Назначение>:        Процедура модификации данных элемента журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Класс формы>:       ' + IsNull(@ObjectViewClassName, @ClassName) + N';
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:
/*
  EXEC ' + @OUT_Object + N'
    @Interface = ''RU'''
        +
        CASE
          WHEN @Translation = 1 THEN N',
    @Language = ''RU'''
          ELSE N''
        END
        + N',
    ...
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N'
  @Interface            Char(2)'
        +
        CASE
          WHEN @Translation = 1 THEN N',
  @Language             Char(2)'
          ELSE N''
        END
        +
        CASE
          WHEN @IdentityField IS NULL THEN N',
  @OLD_' + @KeyField + Replicate(N' ', [Pub].[Is Negative Int](17 - Len(@KeyField), 0)) + @KeyFieldType + Replicate(N' ', [Pub].[Is Negative Int](17 - Len(@KeyFieldType), 0)) + N' = NULL'
          ELSE N''
        END
        +
        CASE
--          WHEN @ObjectViewClassName IS NOT NULL AND @PeriodicFields IS NOT NULL AND @IdentityField IS NOT NULL THEN N',
          WHEN @IdentityField IS NOT NULL THEN N',
  @' + @IdentityField + Replicate(N' ', [Pub].[Is Negative Int](21 - Len(@IdentityField), 0)) + @IdentityFieldType + Replicate(N' ', [Pub].[Is Negative Int](17 - Len(@IdentityFieldType), 0)) + N' = NULL  OUT'
          ELSE N''
        END
        +
        (
          SELECT
            IsNull
            (
              [Pub].[Concat]
              (
                CASE
                  WHEN (@ObjectViewClassName IS NULL OR SC.[IdentityName] IS NOT NULL OR @PeriodicFields IS NULL) 
                          AND (@IdentityField IS NULL OR @IdentityField <> SC.[Name])
                    THEN N',
  @'
                    + SC.[Name]
                    + Replicate(N' ', [Pub].[Is Negative Int](20 - Len(SC.[Name]), 0))
                    + N' ' + SC.[Type]
                    +
                    CASE
                      WHEN SC.[AutoCalculated] = 1 OR SC.[Name] = @@IdentityField AND SC.[HasDefault] = 1 THEN
                        Replicate(N' ', [Pub].[Is Negative Int](17 - Len(SC.[type]), 0)) + N'         OUT'
                      ELSE N''
                    END
                END,
                N''
              ),
              N''
            )
            +
            CASE
              WHEN @ObjectViewClassName IS NOT NULL AND @PeriodicFields IS NOT NULL THEN N',
  @PERIODIC             XML'
              ELSE N',
  @FIELDS               VarChar(Max)'
            END
            +
            CASE
              WHEN @LockAlgorithm = 1 THEN N',
  @Lock_Id              BigInt            = NULL'
              ELSE N''
            END
            +
            CASE
              WHEN @ObjectViewClassName IS NOT NULL THEN N',
  @UpdatedObjects       VarChar(Max)      = NULL OUT'
              ELSE N''
            END
            + N'
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @DeadLockRetries        TinyInt   = 5,
    @DeadLockDelay          DateTime  = ''00:00:00.500'',
    @SavePoint              SysName   = ' + [Pub].[Quote String]([Pub].[RegExp::Replace](@OUT_Object, '[^A-Z]', '', Default) + '_TRAN')
            +
            CASE
              WHEN @LockAlgorithm = 1 THEN N',
    @Lock_Object            SysName   = ' + [Pub].[Quote String](@ObjectName) + N',
    @Lock_CheckId           BigInt'
              ELSE N''
            END
            +
            CASE
              WHEN @IsSecurityModeTable = 1 THEN N',
    @Params                 TParams'
              ELSE N''
            END
            +
            CASE
              WHEN @IdentityField IS NOT NULL THEN N',
    @OUT_' + @IdentityField + REPLICATE(N' ', [Pub].[Is Negative Int](18 - Len(@IdentityField), 0)) + N' ' + @IdentityFieldType
              ELSE N''
            END
            +
            CASE
              WHEN @PeriodicFields IS NOT NULL THEN N',
    @FIELDS                 VarChar(Max)'
              ELSE N''
            END
            + N',

    @TranCount              Int,
    @Retry                  TinyInt,
    @ErrorNumber            Int

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)'
            +
            CASE
              WHEN @Translation = 1 THEN N'

    IF @Language IS NULL
      RaisError(''Не задан обязательный параметр @Language'', 16, 2)'
              ELSE N''
            END

            +
            CASE
              WHEN @ObjectViewClassName IS NOT NULL AND @PeriodicFields IS NOT NULL THEN N''
              ELSE N'

    IF @FIELDS = ''*''
      RaisError(''Неверное значение параметра @FIELDS = %s'', 16, 2, @FIELDS)'
            END

            +
            CASE
              WHEN @LockAlgorithm = 1 THEN N'

    IF @' + @@OLD_IdentityField + ' IS NOT NULL AND @Lock_Id IS NULL
      RaisError(''Не задан обязательный параметр @Lock_Id'', 16, 2)'
              ELSE N''
            END

            +
            CASE
              WHEN @PeriodicFields IS NOT NULL THEN N'

    SET @FIELDS = [Pub].[Arrays Merge] (@FIELDS, @PERIODIC.value(''(/RECORD/@FIELDS)[1]'', ''VarChar(Max)''), '','')'
              ELSE
                IsNull
                (
                  N'
'
                  +
                  [Pub].[Concat]
                  (
                    CASE
                      WHEN [IdentityName] IS NULL AND ([Type] LIKE 'VarChar(%' OR [Type] LIKE 'NVarChar(%') THEN N'
    SET @' + [Name] + REPLICATE(N' ', [Pub].[Is Negative Int](12 - Len([Name]), 0)) + N' = '
                        + CASE WHEN [NullAble] = 1 THEN N'NullIf(' ELSE N'' END
                        + N'[Pub].[Trim](@' + [Name] + N', '' '')'
                        + CASE WHEN [NullAble] = 1 THEN N', N'''')' ELSE N'' END
                    END,
                    N''
                  ),
                  N''
                )
            END
            +
            CASE
              WHEN @IsSecurityModeTable = 1 THEN
                N'

    IF @' + @@OLD_IdentityField + N' IS NULL BEGIN'
                +
                CASE
                  WHEN @ObjectViewClassName IS NOT NULL AND @PeriodicFields IS NOT NULL THEN N'
      IF @PERIODIC IS NULL
        RaisError(''XML набор "@PERIODIC" не может быть пустым в режиме "Insert"'', 16, 2)

      IF @PERIODIC.exist(''/RECORD/@' + @@IdentityField + N'[1]'') = 1
        RaisError(''XML набор не может содержать ключевое поле «' + @@IdentityField + N'»'', 16, 2)
'
                  ELSE N''
                END
                + N'
      SET @Params = TParams::New()'
                +
                IsNull
                (
                  [Pub].[ConCat]
                  (
                    N'
      SET @Params.AddParam(' + [Pub].[Quote String]([IdentityName]) + ', @' + [IdentityName] + ')',
                    N''
                  )
                  +
                  CASE
                    WHEN @ObjectViewClassName IS NOT NULL AND @PeriodicFields IS NOT NULL THEN N'

      SET @PERIODIC.modify(''insert attribute INDEX {"1"} into (/RECORD)[1]'')'
                      +
                      [Pub].[ConCat]
                      (
                        N'
      SET @PERIODIC.modify(''insert attribute ' + [IdentityName] + ' {sql:variable("@' + [IdentityName] + '")} into (/RECORD)[1]'')',
                        N''
                      ) + N'
'
                    ELSE N''
                  END
                  ,
                  N'
      --SET @Params.AddParam(''NAME'', @VALUE)'
                )
                +
                IsNull
                (
                  N'
      SET @FIELDS = [Pub].[Arrays Merge](@FIELDS, ' + [Pub].[Quote String]([Pub].[ConCat]([IdentityName], ',')) + N', '','')'
                  , N''
                )
                +
                N'
      SET @FIELDS = [Pub].[Arrays Join](@FIELDS, ' + LEFT(@ObjectName, LEN(@ObjectName) - 1) + '(Privileged Row Fields)](2, @Params), '','')
      IF @FIELDS IS NULL
        RaisError(50229, 16, 1, ''INSERT'', ' + [Pub].[Quote String](@ObjectName) + N')
    END'
                +
                CASE
                  WHEN @PeriodicFields IS NOT NULL AND @ObjectViewClassName IS NOT NULL THEN N' ELSE IF @PERIODIC IS NOT NULL
      SET @PERIODIC.modify(''insert attribute ' + @@IdentityField + N' {sql:variable("@' + @@OLD_IdentityField + '")} into (/RECORD)[1]'')'
                  ELSE N''
                END
                + N'

    EXEC [Notifications].[Table Changes::Begin]'
              WHEN @ObjectViewIdentityParams IS NOT NULL THEN N'

    SET @FIELDS = [Pub].[Arrays Merge](@FIELDS, ' + [Pub].[Quote String]([Pub].[ConCat]([IdentityName], ',')) + N', '','')'
              ELSE N''
            END

            + N'

    SET @TranCount = @@TRANCOUNT
    SET @Retry = CASE WHEN @TranCount = 0 THEN @DeadLockRetries ELSE 1 END

    WHILE (@Retry > 0)
    BEGIN TRY
      IF @TranCount > 0
        SAVE TRAN @SavePoint
      ELSE
        BEGIN TRAN
'
            +
            CASE
              WHEN @Logging = 1 THEN N'
      EXEC ' + QuoteName(@Schema) + N'.[Log::Begin]
'
              ELSE N''
            END
            +
            CASE
              WHEN @PeriodicFields IS NULL OR @ObjectViewClassName IS NULL THEN N'
      IF @' + @@OLD_IdentityField + N' IS NULL BEGIN
        INSERT INTO '
                  +
                  CASE
                    WHEN OBJECT_ID(@ObjectTranslateInsertName) IS NOT NULL THEN @ObjectTranslateInsertName + N' (' + CASE WHEN @IdentityField IS NOT NULL THEN '[Row:Index], ' ELSE N'' END + N'[Row:Fields], [Translate:Language], '
                    ELSE @ObjectName + N' ('
                  END
                  +
                  [Pub].[Concat]
                  (
                    CASE
                      WHEN SC.[AutoCalculated] = 0 THEN QuoteName([name])
                    END,
                    N', '
                  )
                  + N')
        VALUES
        ('
                  +
                  CASE
                    WHEN OBJECT_ID(@ObjectTranslateInsertName) IS NOT NULL THEN
                      CASE
                        WHEN @IdentityField IS NOT NULL THEN '
          1,'
                        ELSE N''
                      END
                      + N'
          @FIELDS,
          @Language,'
                    ELSE ''
                  END
                  +
                  [Pub].[Concat]
                  (
                    CASE
                      WHEN SC.[AutoCalculated] = 1 THEN NULL
                      WHEN OBJECT_ID(@ObjectTranslateInsertName) IS NOT NULL
                        THEN N'
          @' + [name]
                      ELSE N'
          CASE WHEN [Pub].[In Array](@FIELDS, ' + [Pub].[Quote String]([name]) + ', '','') = 1 THEN @' + [name] + ' END'
                    END,
                    N','
                  )
                  + N'
        )'
                  +
                  CASE
                    WHEN @IdentityMethod = 'I' OR @IdentityField IS NOT NULL AND OBJECT_ID(@ObjectTranslateInsertName) IS NOT NULL THEN N'

        SET @OUT_'
                      + @IdentityField
                      + ' = '
                      + CASE WHEN OBJECT_ID(@ObjectTranslateInsertName) IS NOT NULL THEN '[System].[Scope Identity]()' ELSE 'SCOPE_IDENTITY()' END
                    ELSE N''
                  END
                  + N'
      END ELSE '
              ELSE N'
      IF @' + @@OLD_IdentityField + ' IS NOT NULL'
            END

            + N' BEGIN'
            +
            CASE
              WHEN @IdentityField IS NOT NULL THEN N'
        SET @OUT_' + @IdentityField + N' = @' + @IdentityField
              ELSE N''
            END
            + N'
        IF @FIELDS IS NULL
          GOTO END_UPDATE_MAIN

        SELECT @' + @@OLD_IdentityField + N' = ' + QuoteName(@@IdentityField) + N' FROM ' + @ObjectName + N' (UPDLOCK) WHERE '
            +
            IsNull
            (
              [Pub].[ConCat]
              (
                QuoteName(SC.[IdentityName]) + N' = @' + SC.[IdentityName],
                ' AND '
              )
              + N' AND ',
              ''
            )
            + QuoteName(@@IdentityField) + N' = @' + @@OLD_IdentityField
            + N'
        IF (@@ROWCOUNT <> 1)
          RaisError(''Сохранение невозможно! Запись не найдена.'', 16, 1)'

            +
            CASE
              WHEN @LockAlgorithm = 1
                THEN N'

        -- Проверяем блокировку
        SET @Lock_CheckId = [System].[Lock::Get Id(In This Session)](NULL, @Lock_Object, @' + @@OLD_IdentityField + N')
        IF (@Lock_CheckId IS NULL) OR (@Lock_CheckId <> @Lock_Id)
          RaisError(''Сохранение невозможно! Время блокировки истекло.'', 16, 1)'
              ELSE N''
            END
            +
            CASE
              WHEN @IsSecurityModeTable = 1 THEN N'

        -- Получаем привилегии
        SELECT
          @FIELDS = [Pub].[Arrays Join]
                    (
                      @FIELDS,
                      [Pub].[Arrays Merge] -- Update + Initialize
                      (
                        [Privileged Fields:Update],
                        [Pub].[Arrays Join]
                        (
                          [Privileged Fields:Initialize],
                          [Checked Fields:Null],
                          '',''
                        ),
                        '',''
                      ),
                      '',''
                    )
        FROM ' + @ObjectPrivilegesView + '
        WHERE '
                  +
                  IsNull
                  (
                    [Pub].[ConCat]
                    (
                      QuoteName(SC.[IdentityName]) + N' = @' + SC.[IdentityName],
                      ' AND '
                    )
                    + N' AND ',
                    ''
                  )
                  + QuoteName(@@IdentityField) + ' = @' + @@OLD_IdentityField
                  + N'

        IF @@ROWCOUNT <> 1
          RaisError(50229, 16, 1, ''UPDATE'', ' + [Pub].[Quote String](@ObjectName) + N')

        IF @FIELDS IS NULL
          GOTO END_UPDATE_MAIN'
              ELSE N''
            END
            +
            CASE
              WHEN @Translation = 1 AND @IdentityField IS NULL THEN N'

        IF [Pub].[In Array](@FIELDS, ' + [Pub].[Quote String](@KeyField) + N', '','') = 1 BEGIN
          UPDATE ' + @ObjectName + N' SET
            ' + QuoteName(@KeyField) + N' = @' + @KeyField + N'
          WHERE ' + QuoteName(@KeyField) + N' = @OLD_' + @KeyField + N'

          SET @FIELDS = [Pub].[Arrays Anti Join](@FIELDS, ' + [Pub].[Quote String](@KeyField) + N', '','')
          IF @FIELDS IS NULL
            GOTO END_UPDATE_MAIN
        END'
              ELSE N''
            END
            +
            CASE
              WHEN @PeriodicFields IS NULL OR @ObjectViewClassName IS NULL THEN N'

        UPDATE ' + CASE WHEN OBJECT_ID(@ObjectTranslateUpdateName) IS NOT NULL THEN @ObjectTranslateUpdateName ELSE @ObjectName END + ' SET
          '
                +
                CASE
                  WHEN OBJECT_ID(@ObjectTranslateUpdateName) IS NOT NULL THEN
                    N'[Row:Fields]              = @FIELDS,
          '
                  ELSE N''
                END
                +
                [Pub].[Concat]
                (
                  CASE
                    WHEN SC.[AutoCalculated] = 0 AND SC.[IdentityName] IS NULL AND (@IdentityField IS NOT NULL OR @Translation = 0 OR SC.[name] <> @KeyField)
                      THEN QuoteName(SC.[name])
                            + Replicate(N' ', [Pub].[Is Negative Int](23 - Len(SC.[name]), 0))
                            + ' = '
                            +
                            CASE
                              WHEN OBJECT_ID(@ObjectTranslateUpdateName) IS NOT NULL THEN N'@' + SC.[name]
                              ELSE 
                                'CASE WHEN [Pub].[In Array](@FIELDS, '
                                + [Pub].[Quote String](SC.[name])
                                + N', '','') = 1 THEN @'
                                + SC.[name]
                                + ' ELSE '
                                + QuoteName(SC.[name])
                                + ' END'
                            END
                            
                  END,
                  N',
          '
                )
                + N'
        WHERE '
                +
                IsNull
                (
                  [Pub].[ConCat]
                  (
                    QuoteName(SC.[IdentityName]) + N' = @' + SC.[IdentityName],
                    ' AND '
                  )
                  + N' AND ',
                  ''
                )
                + QuoteName(@@IdentityField) + ' = @' + @@OLD_IdentityField
                + CASE WHEN OBJECT_ID(@ObjectTranslateUpdateName) IS NOT NULL THEN ' AND [Translate:Language] = @Language' ELSE N'' END
                + N'
      END'
              ELSE
                CASE
                  WHEN @IdentityField IS NULL THEN N'

        IF [Pub].[In Array](@FIELDS, ' + [Pub].[Quote String](@KeyField) + N', '','') = 1 AND @OLD_' + @KeyField + N' <> @' + @KeyField + N' BEGIN
          UPDATE ' + @ObjectName + N' SET ' + QuoteName(@KeyField) + N' = @' + @KeyField + N' WHERE ' + QuoteName(@KeyField) + N' = @OLD_' + @KeyField + N'
          SET @FIELDS = [Pub].[Arrays Anti Join](@FIELDS, ' + [Pub].[Quote String](@KeyField) + N', '','')
          IF @FIELDS IS NULL
            GOTO END_UPDATE_MAIN
        END'
                  ELSE N''
                END
                + N'
      END

      SET @PERIODIC.modify(''replace value of (/RECORD/@FIELDS)[1] with sql:variable("@FIELDS")'')
      EXEC ' + QuoteName(@Schema) + N'.' + QuoteName(@Name + N'(Periodic Set)') + N' @Data = @PERIODIC'
                +
                CASE
                  WHEN @IdentityField IS NOT NULL THEN N'
      IF @' + @IdentityField + N' IS NULL
        SET @OUT_' + @IdentityField + N' = [System].[Scope Identity]()'
                  ELSE N''
                END
            END
          FROM
          (
            SELECT TOP 1000
              [Name]              = IsNull(SC.[Name], I.[Name]),
              [Type]              = IsNull(SC.[Type], I.[Value]),
              [ColumnName]        = SC.[Name],
              [ColumnType]        = SC.[Type],
              [AutoCalculated]    = SC.[AutoCalculated],
              [HasDefault]        = SC.[HasDefault],
              [NullAble]          = SC.[NullAble],
              [PeriodicField_Id]  = SC.[PeriodicField_Id],
              [PeriodicFieldType] = SC.[PeriodicFieldType],
              [IdentityName]      = CASE WHEN I.[Index] IS NOT NULL THEN IsNull(SC.[Name], I.[Name]) END,
              [IdentityType]      = CASE WHEN I.[Index] IS NOT NULL THEN IsNull(SC.[Type], I.[Value]) END
            FROM
            (
              SELECT
                [Name]              = SC.[name],
                --[TypeName]          = TYPE_NAME(SC.[system_type_id]),
                [Type]              = [SQL].[Field Type Compile](TYPE_NAME(SC.[user_type_id]), SC.[max_length], SC.[precision], SC.[scale], NULL),
                [Index]             = SC.[column_id],
                [AutoCalculated]    = Cast(CASE WHEN SC.[is_identity] = 1 OR SC.[is_computed] = 1 THEN 1 ELSE 0 END AS Bit),
                [HasDefault]        = Cast(CASE WHEN SC.[default_object_id] <> 0 THEN 1 ELSE 0 END AS Bit),
                [NullAble]          = SC.[is_nullable],
                [PeriodicField_Id]  = PF.[Index],
                [PeriodicFieldType] = PF.[Value]
                --[IdentityParam]     = Cast(CASE WHEN I.[Index] IS NOT NULL THEN 1 ELSE 0 END AS Bit)
              FROM [sys].[columns] SC WITH (NOLOCK)
              LEFT JOIN [Pub].[Array To RowSet Of Values](@PeriodicFields, ',') PF ON SC.[name] = PF.[Value]
              WHERE SC.[object_id] = @Object_Id
            ) SC
            FULL OUTER JOIN [Pub].[Array To RowSet Of Named Values](@ObjectViewIdentityParams, ';') I ON SC.[name] = I.[Name]
            ORDER BY CASE WHEN I.[Index] IS NOT NULL THEN 1 ELSE 2 END, CASE WHEN I.[Index] IS NOT NULL THEN I.[Index] ELSE SC.[Index] END
          ) SC
        )
        + N'

END_UPDATE_MAIN:
OK_EXIT:'
          +
          CASE
            WHEN @Logging = 1 THEN N'
      EXEC ' + QuoteName(@Schema) + N'.[Log::Commit]'
            ELSE N''
          END

        + N'
      WHILE @@TRANCOUNT > @TranCount COMMIT TRAN
      SET @Retry = 0'

        +
        CASE
          WHEN @IdentityField IS NOT NULL THEN N'

      -- Только в случае полного успеха присваеваем OUT-параметрам новые значения
      IF @' + @IdentityField + N' IS NULL
        SET @' + @IdentityField + N' = @OUT_' + @IdentityField
          ELSE N''
        END

        + N'
    END TRY
    BEGIN CATCH
      SET @ErrorNumber = ERROR_NUMBER()
      IF @ErrorNumber IN (1205, 51205) BEGIN -- DEAD LOCK OR USER DEAD LOCK
        SET @ErrorNumber = 51205
        SET @Retry = @Retry - 1
      END ELSE
        SET @Retry = 0

      IF XACT_STATE() <> 0
        IF XACT_STATE() = -1 OR @@TRANCOUNT > @TranCount
          ROLLBACK TRAN
        ELSE IF @@TRANCOUNT = @TranCount
          ROLLBACK TRAN @SavePoint

      IF @@TRANCOUNT > 0 OR @Retry = 0
        EXEC [System].[ReRaise Error] @ErrorNumber = @ErrorNumber, @ProcedureId = @@PROCID
      ELSE
        WAITFOR DELAY @DeadLockDelay
    END CATCH
'
        +
        CASE
          WHEN @ObjectViewClassName <> '' THEN N'
    EXEC [Notifications].[Table Changes::Commit] @UpdatedObjects = @UpdatedObjects OUT'
          ELSE N''
        END
        + N'
    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID
  END CATCH
GO'

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 14
  END CATCH
/*
    SET @PERIODIC =
    (
      SELECT
        [INSERT]              = CASE WHEN @' + @@OLD_IdentityField + N' IS NULL THEN 1 END,
        [FIELDS]              = '''''
                    +
                    CASE
                      WHEN @PeriodicAgeFieldName IS NULL THEN N',
        [AGE]                 = @AGE'
                      ELSE N''
                    END
                    +
                    [Pub].[Concat]
                    (
                      CASE
                        WHEN SC.[PeriodicField_Id] IS NULL THEN N',
        '
                          + QuoteName(SC.[name])
                          + Replicate(N' ', [Pub].[Is Negative Int](19 - Len(SC.[name]), 0))
                          + N' = '
                          +
                          CASE
                            WHEN SC.[name] = @@IdentityField THEN '@' + @@IdentityField
                            ELSE 'CASE WHEN [Pub].[In Array](@FIELDS, ' + [Pub].[Quote String](SC.[name]) + N', '','') = 1 THEN @' + SC.[name] + N' END'
                          END
                      END,
                      N''
                    )
                    + N',
        (
          SELECT
            [DATE]                = I.Node.value(''@DATE'', ''Date'')'
                    +
                    [Pub].[Concat]
                    (
                      N',
            '
                      + QuoteName(SC.[PeriodicFieldType]) -- Внимание! Тут PF.[Value] как способ обнуления непериодических полей!!!
                      + Replicate(N' ', [Pub].[Is Negative Int](19 - Len(SC.[Name]), 0))
                      + N' = '
                      + 'CASE WHEN [Pub].[In Array](@FIELDS, ' + [Pub].[Quote String](SC.[name]) + N', '','') = 1 THEN I.Node.value('
                      + [Pub].[Quote String]('@' + SC.[name])
                      + N', '
                      + [Pub].[Quote String](SC.[Type]) + N') END'
                      ,
                      N''
                    )
                    + N'
          FROM @PERIODIC.nodes(''/RECORD/PERIODIC'') AS I (Node)
          WHERE [Pub].[Arrays Positive Join](@FIELDS, @PeriodicFields, '','') = 1
          FOR XML RAW(''PERIODIC''), TYPE
       )
       FOR XML RAW(''RECORD'')
    )
*/
GO
-- Delete
ALTER PROCEDURE [SQL].[Default::Delphi::TSBaseXXViewFrm];15
  @Schema                     SysName,
  @ObjectName                 SysName,
  @IdentityParams             VarChar(256),
  @Object_Id                  Int,
  @IdentityField              SysName,
  @IdentityFieldType          SysName,
  @KeyField                   SysName,
  @KeyFieldType               SysName,
  @KeyFieldUnique             Bit,
  @ProcedureObjectDelete      SysName,

  @ObjectViewClassName        VarChar(256),
  @ObjectPrivilegesView       VarChar(256),
--  @IsSecurityModeTable        Bit,

  @Category                   NVarChar(128),
  @Caption                    NVarChar(128),

  @Logging                    Bit,

  @OUT_Object                 SysName       OUT,
  @OUT_Message                NVarChar(256) OUT
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @LockAlgorithm      Bit,
    @@IdentityField     SysName,
    @@IdentityFieldType SysName,
    @SQL                NVarChar(Max)
  
  BEGIN TRY
    SET @LockAlgorithm = CASE WHEN @ObjectViewClassName IS NOT NULL AND (@IdentityField IS NOT NULL OR @KeyFieldUnique = 1) THEN 1 ELSE 0 END

    SET @@IdentityField     = IsNull(@IdentityField, @KeyField)
    SET @@IdentityFieldType = IsNull(@IdentityFieldType, @KeyFieldType)

    SET @OUT_Object = @ProcedureObjectDelete
    SET @SQL =
N'--===============================================================================================================
-- <Назначение>:        Процедура удаления элемента журнала "' + IsNull(@Category + N' :: ', N'') + @Caption + N'";
-- <Автор>:             ' + CASE WHEN ORIGINAL_LOGIN() = 'sa' THEN HOST_NAME() ELSE ORIGINAL_LOGIN() END + N';
-- <Дата создания>:     ' + Convert(NVarChar(10), GetDate(), 104) + N';
-----------------------------------------------------------------------------------------------------------------
-- <Пример>:'
      +
      (
        SELECT
          N'
/*
  EXEC ' + @OUT_Object + N' @Interface = ''RU''' + IsNull([Pub].[ConCat](N', @' + [Name] + N' = ?', N''), N'') + N', @' + @@IdentityField + N' = ?
*/
--===============================================================================================================
CREATE PROCEDURE ' + @OUT_Object + N'
  @Interface            Char(2)'
          +
          IsNull
          (
            [Pub].[ConCat]
            (
              N',
  @' + [Name] + Replicate(' ', [Pub].[Is Negative Int](20 - Len([Name]), 0)) + N' ' + [Value],
              N''
            ),
            N''
          )
        FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';')
      )
      + N',
  @' + @@IdentityField + Replicate(' ', [Pub].[Is Negative Int](20 - Len(@@IdentityField), 0)) + N' ' + @@IdentityFieldType
      + N',
  @UpdatedObjects       VarChar(Max)  = NULL OUT,
  @Debug                Bit           = 0
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @DeadLockRetries    TinyInt   = 5,
    @DeadLockDelay      DateTime  = ''00:00:00.500'',
    @SavePoint          SysName   = ' + [Pub].[Quote String]([Pub].[RegExp::Replace](@OUT_Object, '[^A-Z]', '', Default) + '_TRAN') + N','
      +
      CASE
        WHEN @LockAlgorithm = 1 THEN N'

    @Lock_Object        SysName       = ' + [Pub].[Quote String](@ObjectName) + ',
    @Lock_Key           NVarChar(50),
    @Lock_Id            BigInt        = NULL,'
        ELSE N''
      END
      + N'

    @TranCount          Int,
    @Retry              TinyInt,
    @ErrorNumber        Int

  BEGIN TRY
    IF @Interface IS NULL
      RaisError(''Abstract error'', 16, 2)'
      +
      CASE
        WHEN @LockAlgorithm = 1 THEN N'

    SET @Lock_Key = CAST(@' + @@IdentityField + N' AS NVarChar(50))
    EXEC [System].[Lock]
      @Interface      = @Interface,
      @Id             = @Lock_Id      OUT,
      @Object         = @Lock_Object,
      @Key            = @Lock_Key,
      @RaisError      = 1,
      @Debug          = @Debug'
        ELSE N''
      END
      + N'

    EXEC [Notifications].[Table Changes::Begin]

    SET @TranCount = @@TRANCOUNT
    SET @Retry = CASE WHEN @TranCount = 0 THEN @DeadLockRetries ELSE 1 END

    WHILE (@Retry > 0)
    BEGIN TRY
      IF @TranCount > 0
        SAVE TRAN @SavePoint
      ELSE
        BEGIN TRAN
'
      +
      CASE
        WHEN @Logging = 1 THEN N'
      EXEC ' + QuoteName(@Schema) + N'.[Log::Begin]
'
        ELSE N''
      END
      +
      (
        SELECT
          N'
      IF NOT EXISTS(SELECT TOP 1 1 FROM ' + @ObjectName + N' WITH (UPDLOCK) WHERE ' + QuoteName(@@IdentityField) + N' = @' + @@IdentityField
          +
          IsNull
          (
            [Pub].[ConCat]
            (
              N' AND ' + QuoteName(SC.[name]) + N' = @' + SC.[Name],
              N''
            ),
            N''
          )
          + N')
        RaisError(''Удаление невозможно! Запись не найдена.'', 16, 1)

      DELETE ' + IsNull(@ObjectPrivilegesView, @ObjectName) + N' WHERE '
          +
          IsNull
          (
            [Pub].[ConCat]
            (
              QuoteName(IP.[Name]) + N' = @' + IP.[Name],
              N' AND '
            )
            + N' AND ',
            ''
          )
        FROM [Pub].[Array To RowSet Of Named Values](@IdentityParams, ';') IP
        LEFT JOIN sys.columns SC WITH (NOLOCK) ON SC.[object_id] = @Object_Id AND IP.[Name] = SC.[name]
      )
      + QuoteName(@@IdentityField) + N' = @' + @@IdentityField
      + CASE
          WHEN @ObjectPrivilegesView IS NOT NULL THEN N'

      IF (@@ROWCOUNT <> 1)
        RaisError(''Удаление невозможно! Нет прав доступа к записи.'', 16, 1)'
          ELSE N''
        END
      +
      CASE
        WHEN @Logging = 1 THEN N'

      EXEC ' + QuoteName(@Schema) + N'.[Log::Commit]'
          ELSE N''
        END

      + N'

      WHILE @@TRANCOUNT > @TranCount COMMIT TRAN
      SET @Retry = 0
    END TRY
    BEGIN CATCH
      SET @ErrorNumber = ERROR_NUMBER()
      IF @ErrorNumber IN (1205, 51205) BEGIN -- DEAD LOCK OR USER DEAD LOCK
        SET @ErrorNumber = 51205
        SET @Retry = @Retry - 1
      END ELSE
        SET @Retry = 0

      IF XACT_STATE() <> 0
        IF XACT_STATE() = -1 OR @@TRANCOUNT > @TranCount
          ROLLBACK TRAN
        ELSE IF @@TRANCOUNT = @TranCount
          ROLLBACK TRAN @SavePoint

      IF @@TRANCOUNT > 0 OR @Retry = 0
        EXEC [System].[ReRaise Error] @ErrorNumber = @ErrorNumber, @ProcedureId = @@PROCID
      ELSE
        WAITFOR DELAY @DeadLockDelay
    END CATCH
'
      +
      CASE
        WHEN @LockAlgorithm = 1 THEN N'
    EXEC [System].[UnLock] @Id = @Lock_Id'
        ELSE N''
      END
      + N'
    EXEC [Notifications].[Table Changes::Commit] @UpdatedObjects = @UpdatedObjects OUT

    RETURN 1
  END TRY
  BEGIN CATCH'
      +
      CASE
        WHEN @LockAlgorithm = 1 THEN N'
    IF @Lock_Id IS NOT NULL
      EXEC [System].[UnLock] @Id = @Lock_Id
'
        ELSE N''
      END
      + N'
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID
  END CATCH
GO'

    EXEC [SQL].[Print] @SQL
    SET @OUT_Message = CASE WHEN @SQL IS NULL THEN 'Ошибка формирования' ELSE 'Создано' END

    RETURN 1
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 15
  END CATCH
GO
