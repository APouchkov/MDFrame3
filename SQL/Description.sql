EXEC sys.sp_addextendedproperty
  @name       =N'Description',
  @value      =N'Связь между таблицей поручений и операциями',
  @level0type =N'SCHEMA',
  @level0name =N'BackOffice', 
  @level1type =N'TABLE',
  @level1name =N'Consolidated Transactions', 
  @level2type =N'CONSTRAINT',
  @level2name =N'FK_BackOffice.Consolidated Transactions_BackOffice.Orders'
GO
