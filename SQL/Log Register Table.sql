EXEC [Base].[Log@Register?Table]
  @Name                               = 'Persons:Params',
  @IdentityField                      = NULL,
  @Caption                            = 'Параметры персоны',

  @Relation                           = 'V',
  @Parent                             = 'Persons',
  @MasterFieldNames                   = 'Id',
  @DetailFieldNames                   = 'Parent_Id',

  @VirtualDescriptionScript           = '<% TABLE %>.[Param_Id]',
  @VirtualDescriptionType             = 'SmallInt',
  @VirtualDescriptionDictionary       = '[Base].[Persons:Params->Types]',

  @VirtualValueFieldName              = 'Value',
  @VirtualValueFieldType              = 'SQL_Variant',
  @VirtualValueDictionary             = NULL,

  @VirtualDateFieldName               = 'Date'


--SELECT * FROM [System].[Log->Tables$]

