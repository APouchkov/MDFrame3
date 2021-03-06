INSERT [Export].[SubScribers]([Id], [Name], [Description], [Callers], [Enabled])
VALUES
  (1, 'Courier', 'Courier', '*', 1)
GO
INSERT [Export].[Objects]([Id], [Name], [Caption], [IdentityField], [Enabled])
VALUES
  (1, '[dbo].[client]', 'клиенты', 'client_id', 1)
GO
INSERT [Export].[SubScriptions]([Object_Id], [SubScriber_Id], [OnLine:Enabled], [OnLine:Properties], [OnLine:Recieve:TimeStamp], [OnLine:LastRun:Time], [OnLine:Optimization], [OffLine:Enabled], [OffLine:Properties], [OffLine:LastRun:Time], [OnTime:Enabled], [OnTime:Properties], [OnTime:Recieve:TimeStamp], [OnTime:LastRun:Time])
VALUES
  (1, 1, 1, '<TARGET NAME="[dbo].[client]" IDENTITY="client_id"/>', NULL, '20141031', 1, 0, Null, Null, Null, Null, Null, Null)
GO




GO
CREATE TRIGGER [dbo].[client (Export Insert)] ON [dbo].[client]
---
  WITH EXECUTE AS OWNER
---
  FOR INSERT
AS
  IF NOT EXISTS(SELECT TOP 1 1 FROM Inserted) RETURN
  SET NOCOUNT ON

  DECLARE
    @Object_Id SmallInt

  SET @Object_Id = [Export].[Object::Id(If Enabled)]('[dbo].[client]')
  IF @Object_Id IS NOT NULL
    INSERT INTO [Export].[Inserted] ([Object_Id], [Identity])
    SELECT
      [Object_Id] = @Object_Id,
      [Identity]  = [client_id]
    FROM Inserted
GO
CREATE TRIGGER [dbo].[client (Export Update)] ON [dbo].[client]
---
  WITH EXECUTE AS OWNER
---
  FOR UPDATE
AS
  IF NOT EXISTS(SELECT TOP 1 1 FROM Inserted) RETURN
  SET NOCOUNT ON

  DECLARE
    @Object_Id SmallInt

  SET @Object_Id = [Export].[Object::Id(If Enabled)]('[dbo].[client]')
  IF @Object_Id IS NOT NULL
    INSERT INTO [Export].[Updated] ([Object_Id], [Identity])
    SELECT
      [Object_Id] = @Object_Id,
      [Identity]  = [client_id]
    FROM Inserted
GO
CREATE TRIGGER [dbo].[client (Export Delete)] ON [dbo].[client]
---
  WITH EXECUTE AS OWNER
---
  FOR DELETE
AS
  IF NOT EXISTS(SELECT TOP 1 1 FROM Deleted) RETURN
  SET NOCOUNT ON

  DECLARE
    @Object_Id SmallInt

  SET @Object_Id = [Export].[Object::Id(If Enabled)]('[dbo].[client]')
  IF @Object_Id IS NOT NULL
    INSERT INTO [Export].[Deleted] ([Object_Id], [Identity], [Params])
    SELECT
      [Object_Id] = @Object_Id,
      [Identity]  = [client_id],
      [Params]    = TParams::New().[Add]('client_type', [client_type]).[Add]('client_name', [client_name])
    FROM Deleted
GO
