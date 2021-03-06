DECLARE
  @Object   SysName = '[BackOffice].[Rests:Data]',
  @SQL      NVarChar(Max),
  @File     NVarChar(1000) = N'\\bd14-0125-1-0\from2014-11-01\BackOffice\Tables\[BackOffice].[Rests%3AData].sql',
  @FileSQL  NVarChar(Max)

SELECT @FileSQL = [File].[Read](@File)

SELECT
  @SQL = [Pub].[Trim Right(Spaces)](T.[Create])
            +
            CAST
            (
              (
                SELECT
                  IsNull
                  (
                    [Pub].[Concat]
                    (
                      N'
GO
'
                        + T.[definition],
                        N''
                      ),
                    N''
                  )
                FROM
                (
                  SELECT TOP 100000
                    [definition] = [Pub].[Trim(Spaces)](OBJECT_DEFINITION(T.[object_id]))
                  FROM [sys].[triggers] T
                  WHERE T.[parent_id] = object_id(@Object)
                  ORDER BY T.[name]
                ) T
              )
              AS NVarChar(Max)
            )
FROM [SQL].[Table](@Object, NULL, 1, 1) T

--EXEC [SQL].[Print] @SQL

IF @SQL = @FileSQL
  PRINT 'EQUAL !!!'
ELSE
  SELECT
    [Index]     = IsNull(V.[Index], F.[Index]),
    [DataBase]  = V.[Value],
    [File]      = F.[Value]
  FROM [Pub].[Array To RowSet Of Values:VarChar(Non Unique)](@SQL, char(13)) V
  FULL OUTER JOIN [Pub].[Array To RowSet Of Values:VarChar(Non Unique)](@FileSQL, char(13)) F ON V.[Index] = F.[Index]
  WHERE IsNull(V.[Value], N'') <> IsNull(F.[Value], N'')
  ORDER BY 1

--EXEC [SQL].[Print] @SQL


--EXEC [Base].[Translate::Table::Rebuild] 'Dic:Instruments:Params'
--EXEC [Base].[Periodic::Table::Rebuild] 'Dic:Places'

