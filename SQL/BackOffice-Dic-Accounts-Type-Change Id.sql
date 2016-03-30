BEGIN TRAN

DECLARE
  @FirmType_Id TinyInt = 16,
  @OLD_TypeId  TinyInt = 82,
  @NEW_TypeId  TinyInt = 192,
  @HLP_TypeId  TinyInt = 255,
  @Field_Id     SmallInt = [BackOffice].[Periodic::Field::Id]('Accounts', 'Type_Id')

UPDATE [BackOffice].[Accounts(Periodic Update)] SET
  [Type_Id] = @HLP_TypeId,
  [Row:Fields] = 'Type_Id',
  [Periodic:Date] = (SELECT [Date] FROM [BackOffice].[Periodic::Field::Value::Info](@Field_Id, [Id], [Pub].[Today](), 'RU'))
WHERE [FirmType_Id] = @FirmType_Id AND [Type_Id] = @OLD_TypeId

UPDATE [BackOffice].[Dic:Accounts:Types] SET
  [Id] = CASE WHEN [Id] = @OLD_TypeId THEN @NEW_TypeId ELSE [Id] END,
  [Parent_Id] = CASE WHEN [Parent_Id] = @OLD_TypeId THEN @NEW_TypeId ELSE [Parent_Id] END
WHERE [FirmType_Id] = @FirmType_Id AND ([Id] = @OLD_TypeId OR [Parent_Id] = @OLD_TypeId)

UPDATE [BackOffice].[Accounts(Periodic Update)] SET
  [Type_Id] = @NEW_TypeId,
  [Row:Fields] = 'Type_Id',
  [Periodic:Date] = (SELECT [Date] FROM [BackOffice].[Periodic::Field::Value::Info](@Field_Id, [Id], [Pub].[Today](), 'RU'))
WHERE [FirmType_Id] = @FirmType_Id AND [Type_Id] = @HLP_TypeId


--ROLLBACK TRAN
COMMIT TRAN
