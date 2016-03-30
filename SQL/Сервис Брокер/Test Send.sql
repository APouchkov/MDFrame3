DECLARE
  @DialogHandle   UniqueIdentifier,
  @MessageName    SysName,
  @Message        NVarChar(Max),
  @Counter        Int = 1000,
  @Time           DateTime  = GetDate(),
  @ServiceFrom    SysName   = N'SenderServiceName',
  @ServiceTo      SysName   = N'ReceiverServiceName',
  @СontractName   SysName   = N'СontractName',
  @MessageType    SysName   = N'Text'

--BEGIN TRANSACTION

    BEGIN DIALOG CONVERSATION @DialogHandle
    FROM SERVICE @ServiceFrom
    TO SERVICE @ServiceTo
    ON CONTRACT @СontractName
    WITH ENCRYPTION = OFF

  WHILE @Counter >= 0 BEGIN
    SET @Counter -= 1


    IF @Counter >= 0
      SET @Message = N'Тест сообщение; ' + CONVERT(NVarChar(20), GETDATE(), 112) + N' ' + CONVERT(NVarChar(20), GETDATE(), 114) + N'; Отправитель: ' + @@SERVERNAME;
    ELSE
      SET @Message = 'END'

    ;SEND ON CONVERSATION @DialogHandle
    MESSAGE TYPE @MessageType(@Message)
  END

    END CONVERSATION @DialogHandle

  --SELECT
  --  [SentRequestMsg] = @Message

--COMMIT TRANSACTION

  SELECT [Total Time] = Cast(GetDate() - @Time As Time)
GO
--SELECT * FROM sys.transmission_queue
GO
 --SELECT 'END CONVERSATION ''' + Cast(conversation_handle as nvarchar(50)) + ''' WITH CLEANUP ' FROM sys.conversation_endpoints


 --SELECT * FROm sys.service_contracts