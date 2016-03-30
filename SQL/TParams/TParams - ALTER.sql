PRINT N'
CREATE SCHEMA [TParams_EMPTY] AUTHORIZATION [Sys_Schema]
GO
GRANT EXECUTE ON SCHEMA::[TParams_EMPTY] TO [public] AS [Sys_Schema]
GRANT REFERENCES ON SCHEMA::[TParams_EMPTY] TO [public] AS [Sys_Schema]
GRANT SELECT ON SCHEMA::[TParams_EMPTY] TO [public] AS [Sys_Schema]
GO'

DECLARE
  @NEWColumns           NVarChar(Max) = N'',
  @ConvertVarBinary     NVarChar(Max) = N'',
  @RESTOREColumns       NVarChar(Max) = N'',
  @ConvertBack          NVarChar(Max) = N'',
--  @PeriodicClear      NVarChar(Max) = N'',
  @PeriodicRebuild      NVarChar(Max) = N'',
--  @TranslateClear     NVarChar(Max) = N'',
  @TranslateRebuild     NVarChar(Max) = N''

SELECT
  @NEWColumns       += N'
ALTER TABLE ' + [SQL].[Object Name](O.object_id) + ' ADD ' + [Pub].[ConCat](QuoteName(C.name + '=') + ' TParams_EMPTY', ', '),
  @ConvertVarBinary += N'
ALTER TABLE ' + [SQL].[Object Name](O.object_id) + ' DISABLE TRIGGER ALL
UPDATE ' + [SQL].[Object Name](O.object_id) + ' SET ' 
  + [Pub].[ConCat](QuoteName(C.name + '=') + ' = Cast(Cast(' + QuoteName(C.name) + ' AS VarBinary(Max)) AS TParams_EMPTY)', N', ')
  + N' WHERE ' + [Pub].[ConCat](QuoteName(C.name) + ' IS NOT NULL', ' OR ')
  + N'
IF @@ERROR = 0 BEGIN
  ALTER TABLE ' + [SQL].[Object Name](O.object_id) + ' DROP COLUMN ' + [Pub].[ConCat](QuoteName(C.name), ', ')
  +
  [Pub].[ConCat]
  (
    N'
  EXEC sp_rename @objname = ' + [Pub].[Quote String]([SQL].[Object Name](O.object_id) + '.' + QuoteName(C.[name] + N'=')) + ' , @newname = ' + [Pub].[Quote String](C.[name]) + N', @objtype = ''COLUMN'''
    , N'
'
  )
  + N'
END'
,
  @RESTOREColumns   += N'
ALTER TABLE ' + [SQL].[Object Name](O.object_id) + ' ADD ' + [Pub].[ConCat](QuoteName(C.name + '*') + ' TParams', ', '),
  @ConvertBack      += N'
UPDATE ' + [SQL].[Object Name](O.object_id) + ' SET ' + [Pub].[ConCat](QuoteName(C.name + '*') + ' = Cast(Cast(' + QuoteName(C.name) + ' AS VarBinary(Max)) AS TParams)', ', ')
  + N'
IF @@ERROR = 0 BEGIN
  ALTER TABLE ' + [SQL].[Object Name](O.object_id) + ' DROP COLUMN ' + [Pub].[ConCat](QuoteName(C.name), ', ') 
  +
  [Pub].[ConCat]
  (
    N'
  EXEC sp_rename @objname = ' + [Pub].[Quote String]([SQL].[Object Name](O.object_id) + '.' + QuoteName(C.[name] + N'*')) + ' , @newname = ' + [Pub].[Quote String](C.[name]) + N', @objtype = ''COLUMN'''
    , N'
'
  )
  + N'
  ALTER TABLE ' + [SQL].[Object Name](O.object_id) + ' ENABLE TRIGGER ALL
END'
FROM sys.columns C
INNER JOIN sys.objects O ON C.object_id = O.object_id AND O.type in ('U')
WHERE C.[user_type_id] = TYPE_ID('TParams')
GROUP BY O.object_id

SELECT
  @PeriodicRebuild += N'
EXEC ' + QuoteName([Schema]) + '.[Periodic@Rebuild] @Name = NULL'
FROM
(
  SELECT DISTINCT [Schema] FROM [System].[Periodic->Tables$]
) I

SELECT
  @TranslateRebuild += N'
EXEC ' + QuoteName([Schema]) + '.[Translate@Rebuild] @Name = NULL'
FROM
(
  SELECT DISTINCT [Schema] FROM [System].[Translate->Tables$]
) I

PRINT N'CREATE TYPE [Table].[(Int,TParams=EMPTY)] AS TABLE([Id] [int] NOT NULL PRIMARY KEY CLUSTERED,	[Params] [dbo].[TParams_EMPTY] NULL)
GO
CREATE TYPE [Table].[(SmallInt,TParams=EMPTY)] AS TABLE([Id] [int] NOT NULL PRIMARY KEY CLUSTERED,	[Params] [dbo].[TParams_EMPTY] NULL)
GO
CREATE TYPE [Table].[(Date,TParams=EMPTY)] AS TABLE([Date] [date] NOT NULL PRIMARY KEY CLUSTERED, [Params] [dbo].[TParams_EMPTY] NULL)
GO
CREATE TYPE [Export].[Rows:OnLine=EMPTY] AS TABLE([Identity] [int] NOT NULL PRIMARY KEY CLUSTERED, [Action] [char](1) COLLATE Cyrillic_General_BIN NOT NULL, [Params] [dbo].[TParams_EMPTY] NULL)
GO

  ALTER TABLE [Delphi].[Classes] DISABLE TRIGGER [Classes (Notify Update)]
GO
  EXEC [SQL].[Replace Metadata]
    @Object_OLD = '' INSERT INTO @Accounts([Id], [Params])'', @Object_NEW = ''--INSERT INTO @Accounts([Id], [Params])'',
    @Replacement = ''RND'', @Execute = 1

  EXEC [SQL].[Replace Metadata];3
    @Object = ''[Export].[SubScription::*::BackOffice.Rests@OffLine];3'',
    @OLD = '' INSERT INTO @@AccountsParts([Id], [Params])'', @NEW = ''--INSERT INTO @@AccountsParts([Id], [Params])'',
    @Replacement = ''RND'', @SKIP_Warnings = 1

  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''[Table].[(Int,TParams)]'',
    @Object_NEW  = ''[Table].[(Int,TParams=EMPTY)]'',
    @Replacement = ''RND'',
    @Execute     = 1

  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''[Table].[(SmallInt,TParams)]'',
    @Object_NEW  = ''[Table].[(SmallInt,TParams=EMPTY)]'',
    @Replacement = ''RND'',
    @Execute     = 1

  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''[Table].[(Date,TParams)]'',
    @Object_NEW  = ''[Table].[(Date,TParams=EMPTY)]'',
    @Replacement = ''RND'',
    @Execute     = 1

  IF OBJECT_ID(''[Export].[Execute]'', ''P'') IS NOT NULL DROP PROC [Export].[Execute]

  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''[Export].[Rows:OnLine]'',
    @Object_NEW  = ''[Export].[Rows:OnLine=EMPTY]'',
    @Replacement = ''RND'',
    @Execute     = 1
GO'
    
EXEC [SQL].[Print] @NEWColumns
PRINT 'GO'
EXEC [SQL].[Print] @ConvertVarBinary
PRINT 'GO'
EXEC [SQL].[Print] @TranslateRebuild
EXEC [SQL].[Print] @PeriodicRebuild
PRINT 'GO'

PRINT N'
  DECLARE @SQL NVarChar(Max)
  SELECT
    @SQL =
      [Pub].[Concat]
      (
        N''EXEC sp_refreshview '''''' + QuoteName(SCHEMA_NAME(O.[schema_id])) + N''.'' + QuoteName(O.[name]) + N'''''''',
        N''
''
      )
  FROM
  (
    SELECT DISTINCT [object_id]
    FROM sys.columns
    WHERE [user_type_id] = TYPE_ID(''TParams'')
  ) C
  INNER JOIN sys.objects O ON C.[object_id] = O.[object_id] AND O.[type] = ''V''
  EXEC(@SQL)
GO
  EXEC [Security].[Privileges@Rebuild] @Object = NULL
GO
  DROP TYPE [Table].[(Int,TParams)]
  DROP TYPE [Table].[(SmallInt,TParams)]
  DROP TYPE [Table].[(Date,TParams)]
  DROP TYPE [Export].[Rows:OnLine]
GO

-- [UDT.TList]
  DROP FUNCTION [TList].[Enum<Int8>]
  DROP FUNCTION [TList].[Enum<Int16>]
  DROP FUNCTION [TList].[Enum<Int32>]
  DROP FUNCTION [TList].[Is Equal<Int32>]
  DROP FUNCTION [TList].[Enum<Int64>]
  DROP FUNCTION [TList].[Enum<String>]
  DROP FUNCTION [TList].[Enum<DateTime>]
  DROP FUNCTION [TDictionary].[Enum<String,Int32>]
  DROP FUNCTION [TDictionary].[Enum<String,String>]
  DROP FUNCTION [TDictionary].[Enum<Int32,String>]
  DROP FUNCTION [TDictionary].[Enum<Date,String>]
GO
  DROP AGGREGATE [TList].[Aggregate<Int8>]
  DROP AGGREGATE [TList].[Aggregate<Int16>]
  DROP AGGREGATE [TList].[Aggregate<Int32>]
  DROP AGGREGATE [TList].[Aggregate<Int64>]
  DROP AGGREGATE [TList].[Aggregate<String>]
  DROP AGGREGATE [TList].[Aggregate<DateTime>]
  DROP AGGREGATE [TDictionary].[Aggregate<String,Int32>]
  DROP AGGREGATE [TDictionary].[Aggregate<String,String>]
  DROP AGGREGATE [TDictionary].[Aggregate<Int32,String>]
  DROP AGGREGATE [TDictionary].[Aggregate<Date,String>]
GO

-- [UDT.TParams]
  --DROP FUNCTION [TParams].[Merge]
  --DROP FUNCTION [TParams].[Is Equal]
  DROP FUNCTION [TParams].[Enum]
  DROP FUNCTION [TParams].[Enum(As Text)]
  DROP AGGREGATE [TParams].[Aggregate]
-- DROP AGGREGATE [TParams].[Aggregate(Max)]
  DROP AGGREGATE [TParams].[Aggregate(Text)]
GO
  DROP FUNCTION [TParams].[TList.Enum<Int8>]
GO
  DROP FUNCTION [TParams].[TList.Enum<Int16>]
GO
  CREATE FUNCTION [TParams].[TList.Enum<Int16>](@Params [dbo].[TParams_EMPTY], @Name SysName) RETURNS TABLE AS RETURN (SELECT TOP(0) [Value] = 0, [Index] = 0)
GO
  CREATE FUNCTION [TParams_EMPTY].[TList.Enum<Int16>](@Params [dbo].[TParams_EMPTY], @Name SysName) RETURNS TABLE AS RETURN (SELECT TOP(0) [Value] = 0, [Index] = 0)
GO
  DROP FUNCTION [TParams].[TList.Enum<Int32>]
  DROP FUNCTION [TParams].[TList.Enum<Int64>]
  DROP FUNCTION [TParams].[TList.Enum<String>]
  --DROP FUNCTION [TParams].[TList.Enum<DateTime>]
GO'

PRINT N'
DROP FUNCTION [TParams].[TDictionary.Enum<String,Int32>]
GO
CREATE FUNCTION [TParams].[TDictionary.Enum<String,Int32>](@Params [dbo].[TParams_EMPTY], @Name SysName) RETURNS TABLE AS RETURN (SELECT TOP(0) [Name] = N'''', [Value] = 0, [Index] = 0)
GO
CREATE FUNCTION [TParams_EMPTY].[TDictionary.Enum<String,Int32>](@Params [dbo].[TParams_EMPTY], @Name SysName) RETURNS TABLE AS RETURN (SELECT TOP(0) [Name] = N'''', [Value] = 0, [Index] = 0)
GO

DROP FUNCTION [TParams].[TDictionary.Enum<String,String>]
GO
CREATE FUNCTION [TParams].[TDictionary.Enum<String,String>](@Params [dbo].[TParams_EMPTY], @Name SysName) RETURNS TABLE AS RETURN (SELECT TOP(0) [Name] = N'''', [Value] = N'''', [Index] = 0)
GO

DROP FUNCTION [TParams].[TDictionary.Enum<Int32,String>]
GO
CREATE FUNCTION [TParams].[TDictionary.Enum<Int32,String>](@Params [dbo].[TParams_EMPTY], @Name SysName) RETURNS TABLE AS RETURN (SELECT TOP(0) [Name] = 0, [Value] = N'''', [Index] = 0)
GO
CREATE FUNCTION [TParams_EMPTY].[TDictionary.Enum<Int32,String>](@Params [dbo].[TParams_EMPTY], @Name SysName) RETURNS TABLE AS RETURN (SELECT TOP(0) [Name] = 0, [Value] = N'''', [Index] = 0)
GO

--DROP FUNCTION [TParams].[TDictionary.Enum<Date,String>]
--GO
--CREATE FUNCTION [TParams].[TDictionary.Enum<Date,String>](@Params [dbo].[TParams_EMPTY], @Name SysName) RETURNS TABLE AS RETURN (SELECT TOP(0) [Name] = Cast(NULL AS Date), [Value] = N'''', [Index] = 0)
--GO'

PRINT N'
-- [UDP.DynamicSQL]
  DROP FUNCTION [TParams].[Execute Row]
  DROP FUNCTION [TParams].[Execute Remote Row]
  DROP FUNCTION [TParams].[Open Query]
  DROP FUNCTION [TParams].[Open Query(Params)]
  DROP FUNCTION [TParams].[Open Remote Query]
  DROP FUNCTION [TParams].[Open Remote Query(Params)]
  DROP FUNCTION [SQL].[Final SQL]
  DROP FUNCTION [SQL].[Execute Scalar]
  DROP FUNCTION [SQL].[Execute Scalar(Params)]
  DROP FUNCTION [SQL].[Execute Remote Scalar]
  DROP FUNCTION [SQL].[Execute Remote Scalar(Params)]

  DROP PROCEDURE [SQL].[Execute]
  DROP PROCEDURE [SQL].[Execute(Params)]
  DROP PROCEDURE [SQL].[Execute=>Non Query]
  DROP PROCEDURE [SQL].[Execute(Params)=>Non Query]
  DROP PROCEDURE [SQL].[Execute=>XML]
  DROP PROCEDURE [SQL].[Execute(Params)=>XML]

-- [UDF.GUI]
  DROP PROCEDURE [GUI].[Compile]
GO'

PRINT N'
 EXEC [SQL].[Replace Metadata]
   @Object_OLD  = ''\.AddTListInt16\(''''([^'''']+)'''',([^)]+)\)'',
   @Object_NEW  = ''.AddVarBinaryMax/*AddTListInt16*/(''''${1}'''',Cast($2 AS VarBinary(Max)))'',
   @Replacement = ''REG'',
   @Execute     = 1

 EXEC [SQL].[Replace Metadata]
   @Object_OLD  = ''\.AddTDictionaryStringInt32\(''''([^'''']+)'''',([^)]+)\)'',
   @Object_NEW  = ''.AddVarBinaryMax/*AddTDictionaryStringInt32*/(''''${1}'''',Cast($2 AS VarBinary(Max)))'',
   @Replacement = ''REG'',
   @Execute     = 1

 EXEC [SQL].[Replace Metadata]
   @Object_OLD  = ''\.AddTDictionaryStringString\(''''([^'''']+)'''',([^)]+)\)'',
   @Object_NEW  = ''.AddVarBinaryMax/*AddTDictionaryStringString*/(''''${1}'''',Cast($2 AS VarBinary(Max)))'',
   @Replacement = ''REG'',
   @Execute     = 1

 EXEC [SQL].[Replace Metadata]
   @Object_OLD  = ''\.AddTDictionaryInt32String\(''''([^'''']+)'''',([^)]+)\)'',
   @Object_NEW  = ''.AddVarBinaryMax/*AddTDictionaryInt32String*/(''''${1}'''',Cast($2 AS VarBinary(Max)))'',
   @Replacement = ''REG'',
   @Execute     = 1
GO'

PRINT N'
  -- 1 Попытка
  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''[dbo].[TParams]'',
    @Object_NEW  = ''[dbo].[TParams_EMPTY]'',
    @Replacement = ''RND'',
    @Execute     = 1

  -- 2 Попытка
  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''[dbo].[TParams]'',
    @Object_NEW  = ''[dbo].[TParams_EMPTY]'',
    @Replacement = ''RND'',
    @Execute     = 1

  -- 3 Попытка
  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''TParams'',
    @Object_NEW  = ''TParams_EMPTY'',
    @Replacement = ''UNQ'',
    @Execute     = 1

  -- 4 Попытка
  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''TParams'',
    @Object_NEW  = ''TParams_EMPTY'',
    @Replacement = ''UNQ'',
    @Execute     = 1

  -- 5 Попытка
  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''TParams'',
    @Object_NEW  = ''TParams_EMPTY'',
    @Replacement = ''UNQ'',
    @Execute     = 1
GO
  DECLARE
    @Text NVarChar(Max)

  SELECT
    @Text = [Pub].[Concat](''EXEC [SQL].[Replace Metadata];3 @Object = '' + [Pub].[Quote String]([SQL].[Object Name](O.[object_id])) + '', @OLD = ''''SELECT'''', @NEW = ''''SELECT'''', @Replacement = ''''UNQ'''', @SKIP_Warnings = 1, @Execute = 1'', Char(13) + Char(10))
  FROM sys.columns C
  INNER JOIN sys.objects O ON C.object_id = O.object_id AND O.type in (''IF'', ''V'')
  WHERE C.[user_type_id] = TYPE_ID(''TParams'')

  EXEC(@Text)
GO
  -- 6 Попытка
  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''[TParams]'',
    @Object_NEW  = ''[TParams_EMPTY]'',
    @Replacement = ''RND'',
    @Execute     = 1
GO'

PRINT N'
  DROP TYPE [TParams]
GO
  DROP TYPE [TList].[<Int8>]
  DROP TYPE [TList].[<Int16>]
  DROP TYPE [TList].[<Int32>]
  DROP TYPE [TList].[<Int64>]
  DROP TYPE [TList].[<String>]
  DROP TYPE [TList].[<DateTime>]
GO
  DROP TYPE [TDictionary].[<String,Int32>]
  DROP TYPE [TDictionary].[<String,String>]
  DROP TYPE [TDictionary].[<Int32,String>]
  DROP TYPE [TDictionary].[<Date,String>]
GO
  DROP ASSEMBLY [UDF.GUI]
  DROP ASSEMBLY [UDP.DynamicSQL]
  DROP ASSEMBLY [UDT.TParams.XmlSerializers]
  DROP ASSEMBLY [UDT.TParams]
  DROP ASSEMBLY [INT.TParams] 
  DROP ASSEMBLY [UDT.TList]
GO'

PRINT N'
  ALTER ASSEMBLY [UDF.Pubs] 
  FROM ''E:\Programms\Extended Stored Procedures\UDF.Pubs.dll''
  WITH PERMISSION_SET = SAFE, UNCHECKED DATA 
GO
  CREATE ASSEMBLY [UDT.TList] 
  FROM ''E:\Programms\Extended Stored Procedures\UDT.TList.dll''
  WITH PERMISSION_SET = SAFE

  ALTER ASSEMBLY [UDT.TList] 
  WITH VISIBILITY = ON
GO
  CREATE ASSEMBLY [INT.TParams] 
  FROM ''E:\Programms\Extended Stored Procedures\INT.TParams.dll''
  WITH PERMISSION_SET = SAFE
GO
  CREATE ASSEMBLY [UDT.TParams] 
  FROM ''E:\Programms\Extended Stored Procedures\UDT.TParams.dll'' 
  WITH PERMISSION_SET = SAFE
GO
  CREATE ASSEMBLY [UDT.TParams.XmlSerializers] 
  FROM ''E:\Programms\Extended Stored Procedures\UDT.TParams.XmlSerializers.dll''
  WITH PERMISSION_SET = SAFE
GO
  CREATE ASSEMBLY [UDP.DynamicSQL] 
  FROM ''E:\Programms\Extended Stored Procedures\UDP.DynamicSQL.dll''
  WITH PERMISSION_SET = SAFE
GO
  CREATE ASSEMBLY [UDF.GUI] 
  FROM ''E:\Programms\Extended Stored Procedures\UDF.GUI.dll''
  WITH PERMISSION_SET = SAFE
GO
CREATE TYPE [TList].[<Int8>]      EXTERNAL NAME [UDT.TList].[TListInt8]
CREATE TYPE [TList].[<Int16>]     EXTERNAL NAME [UDT.TList].[TListInt16]
CREATE TYPE [TList].[<Int32>]     EXTERNAL NAME [UDT.TList].[TListInt32]
CREATE TYPE [TList].[<Int64>]     EXTERNAL NAME [UDT.TList].[TListInt64]
CREATE TYPE [TList].[<String>]    EXTERNAL NAME [UDT.TList].[TListString]
CREATE TYPE [TList].[<DateTime>]  EXTERNAL NAME [UDT.TList].[TListDateTime]
GO
CREATE TYPE [TDictionary].[<String,Int32>] EXTERNAL NAME [UDT.TList].[TDictionaryStringInt32]
CREATE TYPE [TDictionary].[<String,String>] EXTERNAL NAME [UDT.TList].[TDictionaryStringString]
CREATE TYPE [TDictionary].[<Int32,String>] EXTERNAL NAME [UDT.TList].[TDictionaryInt32String]
CREATE TYPE [TDictionary].[<Date,String>] EXTERNAL NAME [UDT.TList].[TDictionaryDateString]
GO
CREATE TYPE [dbo].[TParams]  EXTERNAL NAME [UDT.TParams].[UDT.TParams]
GO
GRANT EXECUTE ON TYPE::[dbo].[TParams] TO [public]
GRANT REFERENCES ON TYPE::[dbo].[TParams] TO [public]
GO'


PRINT N'


CREATE TYPE [Table].[(Int,TParams)] AS TABLE([Id] [Int] NOT NULL PRIMARY KEY CLUSTERED,	[Params] [dbo].[TParams] NULL)
GO
CREATE TYPE [Table].[(SmallInt,TParams)] AS TABLE([Id] [SmallInt] NOT NULL PRIMARY KEY CLUSTERED,	[Params] [dbo].[TParams] NULL)
GO
CREATE TYPE [Table].[(Date,TParams)] AS TABLE([Date] [Date] NOT NULL PRIMARY KEY CLUSTERED, [Params] [dbo].[TParams] NULL)
GO
CREATE TYPE [Export].[Rows:OnLine] AS TABLE([Identity] [int] NOT NULL PRIMARY KEY CLUSTERED, [Action] [char](1) COLLATE Cyrillic_General_BIN NOT NULL, [Params] [dbo].[TParams] NULL)
GO

  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''[Table].[(Int,TParams=EMPTY)]'',
    @Object_NEW  = ''[Table].[(Int,TParams)]'',
    @Replacement = ''RND'',
    @Execute     = 1

  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''[Table].[(SmallInt,TParams=EMPTY)]'',
    @Object_NEW  = ''[Table].[(SmallInt,TParams)]'',
    @Replacement = ''RND'',
    @Execute     = 1

  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''[Table].[(Date,TParams=EMPTY)]'',
    @Object_NEW  = ''[Table].[(Date,TParams)]'',
    @Replacement = ''RND'',
    @Execute     = 1

  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''[Export].[Rows:OnLine=EMPTY]'',
    @Object_NEW  = ''[Export].[Rows:OnLine]'',
    @Replacement = ''RND'',
    @Execute     = 1

DROP TYPE [Table].[(Int,TParams=EMPTY)]
DROP TYPE [Table].[(SmallInt,TParams=EMPTY)]
DROP TYPE [Table].[(Date,TParams=EMPTY)]
DROP TYPE [Export].[Rows:OnLine=EMPTY]
GO'


EXEC [SQL].[Print] @RESTOREColumns
PRINT 'GO'
EXEC [SQL].[Print] @ConvertBack
PRINT 'GO'
EXEC [SQL].[Print] @TranslateRebuild
EXEC [SQL].[Print] @PeriodicRebuild
PRINT 'GO'

PRINT N'
  DECLARE @SQL NVarChar(Max)
  SELECT
    @SQL =
      [Pub].[Concat]
      (
        N''EXEC sp_refreshview '''''' + QuoteName(SCHEMA_NAME(O.[schema_id])) + N''.'' + QuoteName(O.[name]) + N'''''''',
        N''
''
      )
  FROM
  (
    SELECT DISTINCT [object_id]
    FROM sys.columns
    WHERE [user_type_id] = TYPE_ID(''TParams_Empty'')
  ) C
  INNER JOIN sys.objects O ON C.[object_id] = O.[object_id] AND O.[type] = ''V''
  EXEC(@SQL)
GO
  EXEC [Security].[Privileges@Rebuild] @Object = NULL
GO'

PRINT N'
  -- 1 Попытка
  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''[dbo].[TParams_EMPTY]'',
    @Object_NEW  = ''[dbo].[TParams]'',
    @Replacement = ''RND'',
    @Execute     = 1

  -- 2 Попытка
  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''[dbo].[TParams_EMPTY]'',
    @Object_NEW  = ''[dbo].[TParams]'',
    @Replacement = ''RND'',
    @Execute     = 1

  -- 3 Попытка
  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''TParams_EMPTY'',
    @Object_NEW  = ''TParams'',
    @Replacement = ''UNQ'',
    @Execute     = 1

  -- 4 Попытка
  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''TParams_EMPTY'',
    @Object_NEW  = ''TParams'',
    @Replacement = ''UNQ'',
    @Execute     = 1

  -- 5 Попытка
  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''TParams_EMPTY'',
    @Object_NEW  = ''TParams'',
    @Replacement = ''UNQ'',
    @Execute     = 1

  -- 6 Попытка
  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''TParams[=_]EMPTY'',
    @Object_NEW  = ''TParams'',
    @Replacement = ''REG'',
    @Execute     = 1


  EXEC [SQL].[Replace Metadata]
    @Object_OLD = ''--INSERT INTO @Accounts([Id], [Params])'', @Object_NEW = '' INSERT INTO @Accounts([Id], [Params])'',
    @Replacement = ''RND'', @Execute = 1

  EXEC [SQL].[Replace Metadata];3
    @Object = ''[Export].[SubScription::*::BackOffice.Rests@OffLine];3'',
    @OLD = ''--INSERT INTO @@AccountsParts([Id], [Params])'', @NEW = '' INSERT INTO @@AccountsParts([Id], [Params])'',
    @Replacement = ''RND'', @SKIP_Warnings = 1

DECLARE
  @Text NVarChar(Max)

SELECT
  @Text = [Pub].[Concat](''EXEC [SQL].[Replace Metadata];3 @Object = '' + [Pub].[Quote String]([SQL].[Object Name](O.[object_id])) + '', @OLD = ''''SELECT'''', @NEW = ''''SELECT'''', @Replacement = ''''UNQ'''', @SKIP_Warnings = 1, @Execute = 1'', Char(13) + Char(10))
FROM sys.columns C
INNER JOIN sys.objects O ON C.object_id = O.object_id AND O.type in (''IF'', ''V'')
WHERE C.[user_type_id] = TYPE_ID(''TParams_EMPTY'')

EXEC(@Text)
GO'

PRINT N'
CREATE FUNCTION [TParams].[Enum](@Params TParams)
RETURNS TABLE([Name] [nvarchar](1024) NULL, [Type] [SysName] NULL, [Value] [SQL_Variant] NULL) WITH EXECUTE AS CALLER
AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[Enum]
GO
CREATE FUNCTION [TParams].[Enum(As Text)](@Params TParams, @Style NVarChar(4))
RETURNS TABLE([Name] [nvarchar](1024) NULL, [Type] [SysName] NULL, [Value] [NVarChar](Max) NULL) WITH EXECUTE AS CALLER
AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[EnumAsText]
GO'

PRINT N'
CREATE FUNCTION [TList].[Enum<Int8>] (@List [TList].[<Int8>])
RETURNS TABLE ([Value] TinyInt, [Index] SmallInt)
AS EXTERNAL NAME [UDT.TList].[TListInt8].[Enum];
GO
CREATE FUNCTION [TParams].[TList.Enum<Int8>] (@Params TParams, @Name NVarChar(4000))
RETURNS TABLE ([Value] TinyInt, [Index] SmallInt)
AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[TListInt8Enum];
GO
CREATE FUNCTION [TList].[Enum<Int16>] (@List [TList].[<Int16>])
RETURNS TABLE ([Value] SmallInt, [Index] Int)
AS EXTERNAL NAME [UDT.TList].[TListInt16].[Enum];
GO
DROP FUNCTION [TParams].[TList.Enum<Int16>]
DROP FUNCTION [TParams_EMPTY].[TList.Enum<Int16>]
GO
CREATE FUNCTION [TParams].[TList.Enum<Int16>] (@Params TParams, @Name NVarChar(4000))
RETURNS TABLE ([Value] SmallInt, [Index] Int)
AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[TListInt16Enum];
GO
CREATE FUNCTION [TList].[Enum<Int32>] (@List [TList].[<Int32>])
RETURNS TABLE ([Value] Int, [Index] Int)
AS EXTERNAL NAME [UDT.TList].[TListInt32].[Enum];
GO
CREATE FUNCTION [TList].[Is Equal<Int32>] (@List1 [TList].[<Int32>], @List2 [TList].[<Int32>])
RETURNS [bit]
AS EXTERNAL NAME [UDT.TList].[TListInt32].[IsEqual];
GO
CREATE FUNCTION [TParams].[TList.Enum<Int32>] (@Params TParams, @Name NVarChar(4000))
RETURNS TABLE ([Value] Int, [Index] Int)
AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[TListInt32Enum];
GO
CREATE FUNCTION [TList].[Enum<Int64>] (@List [TList].[<Int64>])
RETURNS TABLE ([Value] BigInt, [Index] Int)
AS EXTERNAL NAME [UDT.TList].[TListInt64].[Enum];
GO
CREATE FUNCTION [TParams].[TList.Enum<Int64>] (@Params TParams, @Name NVarChar(4000))
RETURNS TABLE ([Value] BigInt, [Index] Int)
AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[TListInt64Enum];
GO
CREATE FUNCTION [TList].[Enum<String>] (@List [TList].[<String>])
RETURNS TABLE ([Value] NVarChar(4000), [Index] Int)
AS EXTERNAL NAME [UDT.TList].[TListString].[Enum];
GO
CREATE FUNCTION [TParams].[TList.Enum<String>] (@Params TParams, @Name NVarChar(4000))
RETURNS TABLE ([Value] NVarChar(4000), [Index] Int)
AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[TListStringEnum];
GO
CREATE FUNCTION [TList].[Enum<DateTime>] (@List [TList].[<DateTime>])
RETURNS TABLE ([Value] DateTime, [Index] Int)
AS EXTERNAL NAME [UDT.TList].[TListDateTime].[Enum];
GO
--CREATE FUNCTION [TParams].[TList.Enum<DateTime>] (@Params TParams, @Name NVarChar(4000))
--RETURNS TABLE ([Value] DateTime, [Index] Int)
--AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[TListDateTimeEnum];
--GO'

PRINT N'
CREATE FUNCTION [TDictionary].[Enum<String,Int32>] (@List [TDictionary].[<String,Int32>])
RETURNS TABLE ([Name] NVarChar(4000), [Value] Int, [Index] Int)
AS EXTERNAL NAME [UDT.TList].[TDictionaryStringInt32].[Enum];
GO
  DROP FUNCTION [TParams].[TDictionary.Enum<String,Int32>]
  DROP FUNCTION [TParams_EMPTY].[TDictionary.Enum<String,Int32>]
GO
CREATE FUNCTION [TParams].[TDictionary.Enum<String,Int32>] (@Params TParams, @Name NVarChar(4000))
RETURNS TABLE ([Name] NVarChar(4000), [Value] Int, [Index] Int)
AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[TDictionaryStringInt32Enum];
GO
CREATE FUNCTION [TDictionary].[Enum<String,String>] (@List [TDictionary].[<String,String>])
RETURNS TABLE ([Name] NVarChar(4000), [Value] NVarChar(4000), [Index] Int)
AS EXTERNAL NAME [UDT.TList].[TDictionaryStringString].[Enum];
GO
  DROP FUNCTION [TParams].[TDictionary.Enum<String,String>]
GO
CREATE FUNCTION [TParams].[TDictionary.Enum<String,String>] (@Params TParams, @Name NVarChar(4000))
RETURNS TABLE ([Name] NVarChar(4000), [Value] NVarChar(4000), [Index] Int)
AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[TDictionaryStringStringEnum];
GO
CREATE FUNCTION [TDictionary].[Enum<Int32,String>] (@List [TDictionary].[<Int32,String>])
RETURNS TABLE ([Name] Int, [Value] NVarChar(4000), [Index] Int)
AS EXTERNAL NAME [UDT.TList].[TDictionaryInt32String].[Enum];
GO
  DROP FUNCTION [TParams].[TDictionary.Enum<Int32,String>]
  DROP FUNCTION [TParams_EMPTY].[TDictionary.Enum<Int32,String>]
GO
CREATE FUNCTION [TParams].[TDictionary.Enum<Int32,String>] (@Params TParams, @Name NVarChar(4000))
RETURNS TABLE ([Name] Int, [Value] NVarChar(4000), [Index] Int)
AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[TDictionaryInt32StringEnum];
GO
CREATE FUNCTION [TDictionary].[Enum<Date,String>] (@List [TDictionary].[<Date,String>])
RETURNS TABLE ([Name] Date, [Value] NVarChar(4000), [Index] Int)
AS EXTERNAL NAME [UDT.TList].[TDictionaryDateString].[Enum];
GO
--  DROP FUNCTION [TParams].[TDictionary.Enum<Date,String>]
--GO
--CREATE FUNCTION [TParams].[TDictionary.Enum<Date,String>] (@Params TParams, @Name NVarChar(4000))
--RETURNS TABLE ([Name] Date, [Value] NVarChar(4000), [Index] Int)
--AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[TDictionaryDateStringEnum];
--GO'

PRINT N'
CREATE AGGREGATE [TList].[Aggregate<Int8>](@Value TinyInt)
RETURNS [TList].[<Int8>] EXTERNAL NAME [UDT.TList].[TListInt8Aggregate]
GO
CREATE AGGREGATE [TList].[Aggregate<Int16>](@Value SmallInt)
RETURNS [TList].[<Int16>] EXTERNAL NAME [UDT.TList].[TListInt16Aggregate]
GO
CREATE AGGREGATE [TList].[Aggregate<Int32>](@Value Int)
RETURNS [TList].[<Int32>] EXTERNAL NAME [UDT.TList].[TListInt32Aggregate]
GO
CREATE AGGREGATE [TList].[Aggregate<Int64>](@Value BigInt)
RETURNS [TList].[<Int64>] EXTERNAL NAME [UDT.TList].[TListInt64Aggregate]
GO
CREATE AGGREGATE [TList].[Aggregate<String>](@Value NVarChar(4000))
RETURNS [TList].[<String>] EXTERNAL NAME [UDT.TList].[TListStringAggregate]
GO
CREATE AGGREGATE [TList].[Aggregate<DateTime>](@Value DateTime)
RETURNS [TList].[<DateTime>] EXTERNAL NAME [UDT.TList].[TListDateTimeAggregate]
GO'


PRINT N'
CREATE AGGREGATE [TDictionary].[Aggregate<String,Int32>](@Name NVarChar(4000), @Value Int)
RETURNS [TDictionary].[<String,Int32>] EXTERNAL NAME [UDT.TList].[TDictionaryStringInt32Aggregate]
GO
CREATE AGGREGATE [TDictionary].[Aggregate<String,String>](@Name NVarChar(4000), @Value NVarChar(4000))
RETURNS [TDictionary].[<String,String>] EXTERNAL NAME [UDT.TList].[TDictionaryStringStringAggregate]
GO
CREATE AGGREGATE [TDictionary].[Aggregate<Int32,String>](@Name Int, @Value NVarChar(4000))
RETURNS [TDictionary].[<Int32,String>] EXTERNAL NAME [UDT.TList].[TDictionaryInt32StringAggregate]
GO
CREATE AGGREGATE [TDictionary].[Aggregate<Date,String>](@Name Date, @Value NVarChar(4000))
RETURNS [TDictionary].[<Date,String>] EXTERNAL NAME [UDT.TList].[TDictionaryDateStringAggregate]
GO'


PRINT N'

 EXEC [SQL].[Replace Metadata]
   @Object_OLD  = ''\.AddVarBinaryMax/\*AddTListInt16\*/\(''''([^'''']+)'''',Cast\(([^)]+) AS VarBinary\(Max\)\)\)'',
   @Object_NEW  = ''.AddTListInt16(''''${1}'''',$2)'',
   @Replacement = ''REG'',
   @Execute     = 1

 EXEC [SQL].[Replace Metadata]
   @Object_OLD  = ''\.AddVarBinaryMax/\*AddTDictionaryStringInt32\*/\(''''([^'''']+)'''',Cast\(([^)]+) AS VarBinary\(Max\)\)\)'',
   @Object_NEW  = ''.AddTDictionaryStringInt32(''''${1}'''',$2)'',
   @Replacement = ''REG'',
   @Execute     = 1

 EXEC [SQL].[Replace Metadata]
   @Object_OLD  = ''\.AddVarBinaryMax/\*AddTDictionaryStringString\*/\(''''([^'''']+)'''',Cast\(([^)]+) AS VarBinary\(Max\)\)\)'',
   @Object_NEW  = ''.AddTDictionaryStringString(''''${1}'''',$2)'',
   @Replacement = ''REG'',
   @Execute     = 1

 EXEC [SQL].[Replace Metadata]
   @Object_OLD  = ''\.AddVarBinaryMax/\*AddTDictionaryInt32String\*/\(''''([^'''']+)'''',Cast\(([^)]+) AS VarBinary\(Max\)\)\)'',
   @Object_NEW  = ''.AddTDictionaryInt32String(''''${1}'''',$2)'',
   @Replacement = ''REG'',
   @Execute     = 1
GO

  -- 7 Попытка
  EXEC [SQL].[Replace Metadata]
    @Object_OLD  = ''TParams_EMPTY'',
    @Object_NEW  = ''TParams'',
    @Replacement = ''UNQ'',
    @Execute     = 1
GO
  ALTER TABLE [Delphi].[Classes] ENABLE TRIGGER [Classes (Notify Update)]
GO'

PRINT N'
-- [UDP.DynamicSQL]
GO
CREATE FUNCTION [TParams].[Execute Row] (@AQuery [nvarchar](Max))
RETURNS TParams
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[ExecuteRow]
GO
CREATE FUNCTION [TParams].[Execute Remote Row] (@ALinkedServer [nvarchar](128), @AQuery [nvarchar](Max))
RETURNS TParams
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[ExecuteRemoteRow]
GO
CREATE FUNCTION [TParams].[Open Query](@ASQL [nvarchar](max))
RETURNS TABLE ([Fields] TParams NULL) WITH EXECUTE AS CALLER
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[OpenQuery]
GO
CREATE FUNCTION [TParams].[Open Query(Params)](@ASQL [NVarChar](max), @Params [dbo].[TParams])
RETURNS TABLE([Fields] TParams NULL) WITH EXECUTE AS CALLER
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[OpenParameterizedQuery]
GO
CREATE FUNCTION [TParams].[Open Remote Query](@Server SysName, @ASQL [NVarChar](max))
RETURNS TABLE ([Fields] TParams NULL) WITH EXECUTE AS CALLER
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[OpenRemoteQuery]
GO
CREATE FUNCTION [TParams].[Open Remote Query(Params)](@Server SysName, @ASQL [NVarChar](max), @Params [dbo].[TParams])
RETURNS TABLE([Fields] TParams NULL) WITH EXECUTE AS CALLER
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[OpenParameterizedRemoteQuery]
GO'

PRINT N'
CREATE AGGREGATE [TParams].[Aggregate](@Name [nvarchar](256), @Value SQL_Variant)
RETURNS [dbo].[TParams]
EXTERNAL NAME [UDT.TParams].[UDT.TParamsAggregate]
GO
CREATE AGGREGATE [TParams].[Aggregate(Text)](@Name [nvarchar](256), @Value [nvarchar](max), @Type sysname)
RETURNS [dbo].[TParams]
EXTERNAL NAME [UDT.TParams].[UDT.TParamsAggregateText]
GO

CREATE FUNCTION [SQL].[Final SQL](@CommandText [nvarchar](max), @Params TParams)
RETURNS [nvarchar](max) WITH EXECUTE AS CALLER AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[FinalSQL]
GO
CREATE FUNCTION [SQL].[Execute Scalar](@SQL [nvarchar](max))
RETURNS [sql_variant] WITH EXECUTE AS CALLER, RETURNS NULL ON NULL INPUT
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[ExecuteScalar]
GO
CREATE FUNCTION [SQL].[Execute Scalar(Params)](@SQL [nvarchar](max), @Params TParams)
RETURNS [sql_variant] WITH EXECUTE AS CALLER
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[ExecuteParameterizedScalar]
GO
CREATE FUNCTION [SQL].[Execute Remote Scalar](@Server [sysname], @SQL [nvarchar](max))
RETURNS [sql_variant] WITH EXECUTE AS CALLER, RETURNS NULL ON NULL INPUT
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[ExecuteRemoteScalar]
GO
CREATE FUNCTION [SQL].[Execute Remote Scalar(Params)](@Server [sysname], @SQL [nvarchar](max), @Params TParams)
RETURNS [sql_variant] WITH EXECUTE AS CALLER
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[ExecuteRemoteParameterizedScalar]
GO'

PRINT N'
CREATE PROCEDURE [SQL].[Execute] @Query [nvarchar](Max), @Map [nvarchar](2000) = NULL
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[Execute]
GO
CREATE PROCEDURE [SQL].[Execute(Params)] @Query [nvarchar](Max), @Params TParams, @Map [nvarchar](2000) = NULL
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[ExecuteParameterized]
GO
GRANT EXEC ON [SQL].[Execute(Params)] TO [Grant_Login]
GRANT EXEC ON [SQL].[Execute(Params)] TO [Sys_Schema]
GRANT EXEC ON [SQL].[Execute(Params)] TO [Sys_Base]
GRANT EXEC ON [SQL].[Execute(Params)] TO [Sys_BackOffice]
GO
CREATE PROCEDURE [SQL].[Execute=>Non Query] @Query [nvarchar](Max)
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[ExecuteNonQuery]
GO
CREATE PROCEDURE [SQL].[Execute(Params)=>Non Query] @Query [nvarchar](Max), @Params TParams
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[ExecuteParameterizedNonQuery]
GO
CREATE PROCEDURE [SQL].[Execute=>XML]
  @Query      [nvarchar](Max),
  @Map        [nvarchar](2000)  = NULL,
  @XML        [xml]                     OUT,
  @NamePrefix [nvarchar](20)    = NULL,
  @RootTag    [nvarchar](128)   = NULL,
  @RowsetTag  [nvarchar](128)   = NULL,
  @RowTag     [nvarchar](128)   = NULL
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[ExecuteToXML]
GO
CREATE PROCEDURE [SQL].[Execute(Params)=>XML]
  @Query      [nvarchar](Max),
  @Params     [TParams],
  @Map        [nvarchar](2000)  = NULL,
  @XML        [xml]                     OUT,
  @NamePrefix [nvarchar](20)    = NULL,
  @RootTag    [nvarchar](128)   = NULL,
  @RowsetTag  [nvarchar](128)   = NULL,
  @RowTag     [nvarchar](128)   = NULL
AS EXTERNAL NAME [UDP.DynamicSQL].[DynamicSQL].[ExecuteParameterizedToXML]
GO


-- [UDF.GUI]
CREATE PROCEDURE [GUI].[Compile](@Text [nvarchar](max), @Params TParams, @Comments [nvarchar](2000) = NULL, @Literal [nchar](1), @TranslateScalar [nvarchar](4000) = NULL, @UseScalar [nvarchar](4000) = NULL, @Result [nvarchar](MAX) OUTPUT)
AS EXTERNAL NAME [UDF.GUI].[Compiler].[Compile]
GO
DROP SCHEMA [TParams_EMPTY]
GO'


PRINT N'


SELECT * FROM [Base].[Analytics::Documents::Types(Table)](''[INTERNAL]'', ''[*]'', Default)'
