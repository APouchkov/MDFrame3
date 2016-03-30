  EXEC [Export].[Execute]
    @SubScriber = 'System',
    @Object     = '[BackOffice].[Consolidated Transactions]',
    @Method     = 'OFFLINE',
    @Action     = 'DATASET',
    @Params     = '<PARAMS><PARAM NAME="Target" VALUE="[BackOffice].[Consolidated Transactions#Overturns#Sec#For Dep]"/></PARAMS>
    <FILTERS><FILTER NAME="Firm_Id" VALUE="1"/><FILTER NAME="FirmType_Id" VALUE="12"/><FILTER NAME="Date" VALUE="2014-05-12"/></FILTERS>'
