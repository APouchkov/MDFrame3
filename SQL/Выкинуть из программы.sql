  EXEC [Notifications].[Send]
    @Object_Id      = 666,
    @Recipients     = '{}',
    @Params         = '[Application]="BackOffice.exe"'