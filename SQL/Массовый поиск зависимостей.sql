DECLARE
  @Table          SysName = 'tAccounts',
  @IdentityField  SysName = 'hrnNom',
  @Identity       NVarChar(Max)  = 1,
  @TOP            Int     = 0

  SET NOCOUNT ON

  DECLARE
    @Object_Id      Int

  DECLARE @References TABLE
  (
    [Object_Id]   Int NOT NULL,
    [Description] NVarChar(Max)
  )

  DECLARE
    @SQL            NVarChar(Max),
    @Fields         NVarChar(2048)

  BEGIN TRY
    SET @Object_Id = OBJECT_ID(@Table, 'U')
    IF @Object_Id IS NULL
      RaisError('Таблица %s не обнаружена в системе.', 16, 1, @Table)

    IF @IdentityField IS NULL BEGIN
      SELECT
        @IdentityField = Cast([value] AS SysName)
      FROM sys.extended_properties
      WHERE [major_id] = @Object_Id AND [minor_id] = 0 AND [name] = 'IdentityField'

      IF @IdentityField IS NULL
        RaisError('Таблица %s не содержит в своём описании свойство [IdentityField]', 16, 1, @Table)
    END

    SELECT
      @SQL =
        [Pub].[Concat]
        (
          N'
      EXISTS(SELECT TOP 1 1 FROM ' + FK.[Remote Table:Name] + N' ' + FLD.[Condition] + ')'
          , N'
      OR'),
      @Fields = [Pub].[Merge](FLD.[Fields], ';')
    FROM [SQL].[Table::Foreign Keys::Depended](@Table) FK
    CROSS APPLY
    (
      SELECT
        [Columns] = [Pub].[Concat](QuoteName(C.[name]), ', ')
      FROM sys.columns C (NOLOCK)
      INNER JOIN sys.types T (NOLOCK) ON C.[user_type_id] = T.[user_type_id] AND T.[is_assembly_type] = 0 AND T.[name] NOT IN (N'xml', N'binary', N'varbinary', N'text', N'ntext', N'image')
      WHERE FK.[Remote Table:Id] = C.[object_id] AND C.[is_computed] = 0
    ) C
    CROSS APPLY
    (
      SELECT
        [Condition] = N'WHERE ' + [Pub].[Concat](RK.[Value] + ' = T.' + TK.[Value], ' AND '),
        [Fields]    = [Pub].[Merge]([Pub].[UnQuote](TK.[Value], '['), ';')
      FROM [Pub].[Array To RowSet Of Values](FK.[Remote Table:Keys], ';') RK
      INNER JOIN [Pub].[Array To RowSet Of Values](FK.[Table:Keys], ';') TK ON RK.[Index] = TK.[Index]
    ) FLD
    LEFT JOIN sys.extended_properties EPDF ON EPDF.[major_id] = FK.[Remote Table:Id] AND EPDF.[minor_id] = 0 AND EPDF.[name] = 'DescriptionField'
    LEFT JOIN sys.extended_properties EPDS ON EPDS.[major_id] = FK.[Remote Table:Id] AND EPDS.[minor_id] = 0 AND EPDS.[name] = 'DescriptionScript'
    WHERE FK.[Action:On Delete] = 'NO_ACTION' AND FK.[Foreign Key:Disabled] = 0

    SELECT
      @SQL = 
N'SELECT
  I.part_uid,
  [Found] =
    CASE WHEN' + @SQL + N'
        THEN 1
      ELSE 0
    END
FROM MiddleOffice.dbo.[=TEMP] I
INNER JOIN ' + @Table + ' T ON I.part_uid = T.[UID]'

    FROM [sys].[columns] C
    INNER JOIN [Pub].[Array To RowSet Of Values](@Fields, ';') F ON C.[name] = F.[Value]
    WHERE [object_id] = @Object_Id

    EXEC [SQL].[Print] @SQL
  END TRY
  BEGIN CATCH
    EXEC [System].[ReRaise Error] @ProcedureId = @@PROCID, @ProcedureVersion = 2
  END CATCH
