/****** Object:  StoredProcedure [Events].[Receive@Process]    Script Date: 04/30/2015 17:57:58 ******/
SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO
ALTER PROCEDURE [Events].[Receive@Process]
  @Conversation_Group_Id  UniqueIdentifier
---
  WITH EXECUTE AS OWNER
---
AS
  SET NOCOUNT ON

  DECLARE
    @Conversation_Handle    UniqueIdentifier,

    --@Sequence               BigInt,
    @Type                   SysName,
    @Body                   VarBinary(Max),
    @Service                SysName,
    @Contract               SysName,

    @Procedure              SysName,
    @CoreParams             TParams,
    @Params                 TParams,

    @Message                VarBinary(Max),
    @MessageCount           Int                   = 0,
    
    @Error_Id               Int

  WHILE (1 = 1) BEGIN
    RECEIVE TOP(1)
       @Conversation_Handle = [conversation_handle],
       @Type                = [message_type_name],
       @Body                = [message_body]
    FROM [Events].[Events]
    WHERE conversation_group_id = @Conversation_Group_Id;

    IF @@ROWCOUNT = 0 OR @@Error <> 0 BREAK;

    IF @Type = 'http://schemas.microsoft.com/SQL/ServiceBroker/EndDialog'
      END CONVERSATION @Conversation_Handle
    ELSE IF @Type = 'http://schemas.microsoft.com/SQL/ServiceBroker/Error' BEGIN
      END CONVERSATION @Conversation_Handle
      BREAK
    END ELSE BEGIN
      SET @CoreParams = CAST(@Body AS TParams)
      SET @Type = @CoreParams.AsVarChar('@Event:Type')
      SET @CoreParams.DeleteParam('@Event:Type')
      SET @Message = @CoreParams.AsVarBinary('@Message:Body')
      SET @CoreParams.DeleteParam('@Message:Body')
    
      DECLARE RECEIVERS_CURSOR CURSOR LOCAL FAST_FORWARD FORWARD_ONLY FOR
      SELECT [Procedure], [Params], [On Contract], [Message Type]
      FROM [Events].[Dic:Senders]
      WHERE [Type] = @Type
              AND [Active] = 1

      OPEN RECEIVERS_CURSOR

      WHILE 1 = 1 BEGIN
        FETCH NEXT FROM RECEIVERS_CURSOR
        INTO @Procedure, @Params, @Contract, @Type
        IF @@FETCH_STATUS <> 0 BREAK

        BEGIN TRY
          EXEC @Procedure
            @Params       = @Params,
            @Contract     = @Contract,
            @MessageType  = @Type,
            @MessageBody  = @Message,
            @CoreParams   = @CoreParams
        END TRY
        BEGIN CATCH
          -- Сохраняем стэк ошибок
          INSERT INTO [Events].[Errors]([Procedure], [Params], [Message], [Error])
          VALUES(@Procedure, @Params, @Body, ERROR_MESSAGE())

          --SET @Error_Id = SCOPE_IDENTITY()
          --INSERT INTO [Events].[Errors:Messages]([Error_Id], [Sequence], [Type], [Body])
          --SELECT @Error_Id, [Sequence], [Type], [Body] FROM @Messages
        END CATCH
      END

      CLOSE RECEIVERS_CURSOR
      DEALLOCATE RECEIVERS_CURSOR
    END

  END
