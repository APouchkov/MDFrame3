  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[BackOffice].[Rests]',
    @Method     = 'ONTIME',
    @Action     = 'BEGIN'

  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[BackOffice].[Rests]',
    @Method     = 'ONTIME',
    @Action     = 'ROWS',
    @Params     = '<FILTERS><FILTER NAME="Firm_Id" VALUE="1"/><FILTER NAME="FirmType_Id" VALUE="12"/></FILTERS>'


  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[BackOffice].[Rests]',
    @Method     = 'ONTIME',
    @Action     = 'END'
