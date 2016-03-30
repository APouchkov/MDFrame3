  DROP AGGREGATE [TList].[Aggregate<Int32>]
GO
  DROP AGGREGATE [TDictionary].[Aggregate<String,String>]
GO
  DROP FUNCTION [TDictionary].[Enum<String,Int32>]
GO
  DROP FUNCTION [TDictionary].[Enum<String,String>]
GO
  DROP FUNCTION [TParams].[TDictionary.Enum<String,Int32>]
GO
  DROP FUNCTION [TParams].[TDictionary.Enum<String,String>]
GO
  DROP TYPE [TParams]
GO
  DROP TYPE [TList].[<Int32>]
GO
  DROP TYPE [TDictionary].[<String,Int32>]
GO
  DROP TYPE [TDictionary].[<String,String>]
GO
  DROP ASSEMBLY [UDT.TParams.XmlSerializers]
GO
  DROP ASSEMBLY [UDT.TParams]
GO
  DROP ASSEMBLY [INT.TParams] 
GO
  DROP ASSEMBLY [UDT.TList]
GO
  ALTER ASSEMBLY [UDF.Pubs] 
  FROM 'E:\Programms\Extended Stored Procedures\UDF.Pubs.dll'
  WITH PERMISSION_SET = SAFE, UNCHECKED DATA 
GO
  CREATE ASSEMBLY [UDT.TList] 
  FROM 'E:\Programms\Extended Stored Procedures\UDT.TList.dll' 
  WITH PERMISSION_SET = SAFE
GO
  CREATE ASSEMBLY [INT.TParams] 
  FROM 'E:\Programms\Extended Stored Procedures\INT.TParams.dll'
  WITH PERMISSION_SET = SAFE
GO
  CREATE ASSEMBLY [UDT.TParams] 
  FROM 'E:\Programms\Extended Stored Procedures\UDT.TParams.dll' 
  WITH PERMISSION_SET = SAFE
GO
  CREATE ASSEMBLY [UDT.TParams.XmlSerializers] 
  FROM 'E:\Programms\Extended Stored Procedures\UDT.TParams.XmlSerializers.dll'
  WITH PERMISSION_SET = SAFE
GO
CREATE TYPE [dbo].[TParams]
EXTERNAL NAME [UDT.TParams].[UDT.TParams]
GO
GRANT EXECUTE ON TYPE::[dbo].[TParams] TO [public]
GRANT REFERENCES ON TYPE::[dbo].[TParams] TO [public]
GO
--CREATE TYPE [TDictionary].[<String,Int32>]
--EXTERNAL NAME [UDT.TList].[TDictionaryStringInt32]
--GO
--CREATE TYPE [TDictionary].[<String,String>]
--EXTERNAL NAME [UDT.TList].[TDictionaryStringString]
--GO

--CREATE FUNCTION [TDictionary].[Enum<String,Int32>] (@List [TDictionary].[<String,Int32>])
--RETURNS TABLE ([Name] NVarChar(4000), [Value] Int, [Index] Int)
--AS EXTERNAL NAME [UDT.TList].[TDictionaryStringInt32].[Enum];
--GO
--CREATE FUNCTION [TParams].[TDictionary.Enum<String,Int32>] (@Params TParams, @Name NVarChar(4000))
--RETURNS TABLE ([Name] NVarChar(4000), [Value] Int, [Index] Int)
--AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[TDictionaryStringInt32Enum];
--GO
--CREATE FUNCTION [TDictionary].[Enum<String,String>] (@List [TDictionary].[<String,String>])
--RETURNS TABLE ([Name] NVarChar(4000), [Value] NVarChar(4000), [Index] Int)
--AS EXTERNAL NAME [UDT.TList].[TDictionaryStringString].[Enum];
--GO
--CREATE FUNCTION [TParams].[TDictionary.Enum<String,String>] (@Params TParams, @Name NVarChar(4000))
--RETURNS TABLE ([Name] NVarChar(4000), [Value] NVarChar(4000), [Index] Int)
--AS EXTERNAL NAME [UDT.TParams].[UDT.TParams].[TDictionaryStringStringEnum];
--GO
