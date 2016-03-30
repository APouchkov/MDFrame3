SET NOCOUNT ON
DECLARE
  @DialogHandle     UniqueIdentifier,
  @MessageTypeName  SysName,
  @Message          NVarChar(Max),
  @Time             DateTime

--BEGIN TRANSACTION

  WHILE @MessageTypeName IS NULL OR @MessageTypeName <> 'http://schemas.microsoft.com/SQL/ServiceBroker/EndDialog' BEGIN
    IF @Time IS NULL SET @Time = GetDate()
    WAITFOR
    (
      RECEIVE TOP(1)
        @DialogHandle     = conversation_handle,
        @MessageTypeName  = message_type_name,
        @Message          = message_body
      FROM [ReceiverQueueName]
    ),
    TIMEOUT 100000
    PRINT @Message
  END

  SELECT [Total Time] = Cast(GetDate() - @Time As Time)
  SELECT
    [@DialogHandle]     = @DialogHandle,
    [@MessageTypeName]  = @MessageTypeName,
    [@Message]          = @Message

--COMMIT TRANSACTION
GO

