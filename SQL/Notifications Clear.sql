DELETE FROM [Notifications].[User Messages] WHERE [User_Id] = 13403
DELETE FROM [Notifications].[User Messages] WHERE [Message_Id] IN (SELECT [Id] FROM [Notifications].[Messages] WHERE [User_Id] = 13403)
DELETE FROM [Notifications].[Actions] WHERE [User_Id] = 13403
DELETE FROM [Notifications].[Messages] WHERE [User_Id] = 13403

