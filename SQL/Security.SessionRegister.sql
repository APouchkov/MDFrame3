DECLARE @Session_Id BigInt
EXEC [Security].[Session@Register] @Session_Id = @Session_Id OUT
SELECT [Session_Id] = @Session_Id