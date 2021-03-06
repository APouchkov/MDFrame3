/****** Object:  StoredProcedure [Events].[Receive@Manager]    Script Date: 04/30/2015 17:41:08 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROCEDURE [Events].[Receive@Manager]
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE @TRANCOUNT INT
  SET @TRANCOUNT = @@TRANCOUNT

  BEGIN TRY
    WHILE (1 = 1) BEGIN
      BEGIN TRAN

      DECLARE 
        @Conversation_Group_Id  UniqueIdentifier

      WAITFOR(GET CONVERSATION GROUP @Conversation_Group_Id FROM [Events].[Events]), TIMEOUT 500;

      --INSERT INTO [Events].[Errors]([Procedure], [Error])
      --VALUES('Error', (SELECT * FROM [Events].[Events] FOR XML RAW('RECORD'), BINARY BASE64))

      IF @Conversation_Group_Id IS NULL BEGIN
        ROLLBACK TRAN
        BREAK
      END

      EXEC [Events].[Receive@Process] @Conversation_Group_Id = @Conversation_Group_Id

      WHILE @@TRANCOUNT > @TRANCOUNT COMMIT TRAN  
    END
  END TRY
  BEGIN CATCH
    -- Ошибок не бывает?
    IF @@TRANCOUNT > @TRANCOUNT ROLLBACK TRAN  
    RETURN 1
  END CATCH
GO

