--ALTER TABLE [BackOffice].[Dic:Transactions:Types] DISABLE TRIGGER [Dic:Transactions:Types On Insert/Update/Delete]
--ALTER TABLE [BackOffice].[Dic:Transactions:Types] DROP CONSTRAINT [FK_BackOffice.Dic:Transactions:Types#Parent_Id]
--GO
--EXEC [BackOffice].[Translate::Table::Rebuild] 'Dic:Transactions:Types'
--GO

SET NOCOUNT ON
DECLARE 
  @Code   VarChar(100), 
  @Id     Int, 
  @NewId  Int

DECLARE
  @LogOrder_Id SmallInt = [BackOffice].[Log::Field::Id]([BackOffice].[Log::Object::Id]('Orders'), 'Type_Id')

LB:
SELECT TOP 1
  @Id = [Id],
  @Code = [Code],
  @NewId = [NewId]
FROM
(
  SELECT
    [Id],
    [Code],
    [NewId] = ROW_NUMBER() OVER (ORDER BY [Parent_Id], [Code], [Id])
  FROM [BackOffice].[Dic:Transactions:Types]
) I
WHERE I.[Id] > I.[NewId]
ORDER BY [NewId]

    IF @@ROWCOUNT = 1 BEGIN
      PRINT Cast(@Id AS VarChar) + '(' + @Code + ') ==> ' + Cast(@NewId AS VarChar)
      EXEC [BackOffice].[Log::Begin] @Skip = 1

      BEGIN TRAN
      BEGIN TRY
        UPDATE [BackOffice].[Dic:Transactions:Types] SET [Id] = -@NewId WHERE [Id] = @NewId
        UPDATE [BackOffice].[Dic:Transactions:Types] SET [Parent_Id] = -@NewId WHERE [Parent_Id] = @NewId
        UPDATE [BackOffice].[Log:Records:Values] SET [Inserted] = Cast(-@NewId AS VarBinary(2)) WHERE [Field_Id] = @LogOrder_Id AND [Inserted] = Cast(@NewId AS VarBinary(2))
        UPDATE [BackOffice].[Log:Records:Values] SET [Deleted] = Cast(-@NewId AS VarBinary(2)) WHERE [Field_Id] = @LogOrder_Id AND [Deleted] = Cast(@NewId AS VarBinary(2))

        UPDATE [BackOffice].[Dic:Transactions:Types] SET [Id] = @NewId WHERE [Id] = @Id
        UPDATE [BackOffice].[Dic:Transactions:Types] SET [Parent_Id] = @NewId WHERE [Parent_Id] = @Id
        UPDATE [BackOffice].[Log:Records:Values] SET [Inserted] = Cast(@NewId AS VarBinary(2)) WHERE [Field_Id] = @LogOrder_Id AND [Inserted] = Cast(@Id AS VarBinary(2))
        UPDATE [BackOffice].[Log:Records:Values] SET [Deleted] = Cast(@NewId AS VarBinary(2)) WHERE [Field_Id] = @LogOrder_Id AND [Deleted] = Cast(@Id AS VarBinary(2))

        UPDATE [BackOffice].[Dic:Transactions:Types] SET [Id] = @Id WHERE [Id] = -@NewId
        UPDATE [BackOffice].[Dic:Transactions:Types] SET [Parent_Id] = @Id WHERE [Parent_Id] = -@NewId
        UPDATE [BackOffice].[Log:Records:Values] SET [Inserted] = Cast(@Id AS VarBinary(2)) WHERE [Field_Id] = @LogOrder_Id AND [Inserted] = Cast(-@NewId AS VarBinary(2))
        UPDATE [BackOffice].[Log:Records:Values] SET [Deleted] = Cast(@Id AS VarBinary(2)) WHERE [Field_Id] = @LogOrder_Id AND [Deleted] = Cast(-@NewId AS VarBinary(2))

        COMMIT
        GOTO LB
      END TRY
      BEGIN CATCH
        IF @@TRANCOUNT > 0 ROLLBACK
        EXEC [System].[ReRaise Error] @ProcedureName = 'Blank'
      END CATCH
    END

GO
--ALTER TABLE [BackOffice].[Dic:Transactions:Types]  WITH CHECK ADD  CONSTRAINT [FK_BackOffice.Dic:Transactions:Types#Parent_Id] FOREIGN KEY([Parent_Id])
--REFERENCES [BackOffice].[Dic:Transactions:Types] ([Id])
--GO
--ALTER TABLE [BackOffice].[Dic:Transactions:Types] CHECK CONSTRAINT [FK_BackOffice.Dic:Transactions:Types#Parent_Id]
GO

