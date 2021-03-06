  DECLARE
    @Firm_Id      TinyInt   = 1,
    @FirmType_Id  TinyInt   = 12,
    @BeginDate    DateTime  = '20140301',
    @EndDate      DateTime  = '20140305',
    @Accounts     [Pub].[TList:Int],
    @Params             TParams = TParams::New(),
    @Params_Deliveries  TParams,
    @Params_Sources     TParams,
    @Organization_Id    Int = 2554,
    @State_Id           TinyInt = (SELECT [Id] FROM [BackOffice].[Dic:Actives:States] WHERE [Code] = 'REAL')

  -- Подготовка параметров
  SET @Params.AddParam('Firm_Id', @Firm_Id)
  SET @Params.AddParam('FirmType_Id', @FirmType_Id)
  SET @Params.AddParam('BeginDate', @BeginDate)
  SET @Params.AddParam('EndDate', @EndDate)
  SET @Params.AddParam('Schema', '[STANDARD]')  

  SET @Params_Deliveries = @Params.[Add]('Transactions', '[DEALS CALCULATIONS:DELIVERIES:SEC:ELE]')
  SET @Params_Sources = @Params.[Add]('Transactions', '[DEALS CALCULATIONS:SOURCES:EXCH],[DEALS CALCULATIONS:SOURCES:SYS]')

  -- Заполнение списка
  INSERT @Accounts([Id])
  SELECT
    [Id]
  FROM [BackOffice].[Accounts]
  WHERE [Firm_Id] = @Firm_Id AND [FirmType_Id] = @FirmType_Id
          AND [OpenDate] <= Cast(@EndDate AS Date)
          AND ([CloseDate] IS NULL OR [CloseDate] >= Cast(@BeginDate AS Date))
  OPTION (FORCE ORDER, MAXDOP 1)

  SET @Params_Deliveries.Prepare('[BackOffice].[Analytics::Transactions#Via Accounts]')
  SET @Params_Sources.Prepare('[BackOffice].[Analytics::Transactions::Types]')

  SELECT
    [AccountPart_Id]                = I.[StorageClientRoot_Id],
    [StorageIntermediaryRoot_Id]    = I.[StorageIntermediaryRoot_Id],
    [Instrument_Id]                 = I.[Instrument_Id],
    [RemotePerson_Id]               = I.[RemotePerson_Id],
    [Action]                        = CASE WHEN I.[Value] < 0 THEN 'O' ELSE 'I' END,
    [Value]                         = Abs(I.[Value])
  FROM
  (
    SELECT
      T.[StorageClientRoot_Id],
      T.[StorageIntermediaryRoot_Id],
      T.[Instrument_Id],
      D.[RemotePerson_Id],
      [Value] = SUM(TTS.[Sign] * T.[Value])
    FROM [BackOffice].[Analytics::Transactions#Via Accounts](@Accounts, @Params_Deliveries) D
    INNER JOIN [BackOffice].[Analytics::Transactions::Types](@Params_Sources) S ON D.[Type_Id] = S.[Type_Id]
    INNER JOIN [BackOffice].[Transactions] T ON D.[Id] = T.[Consolidate_Id]
    INNER JOIN [BackOffice].[Dic:Transactions:Types(Single)] TTS ON T.[Type_Id] = TTS.[Id] AND TTS.[State_Id] = @State_Id
    WHERE D.[StorageClientRoot_Id] IS NOT NULL
    GROUP BY 
      T.[StorageClientRoot_Id],
      T.[StorageIntermediaryRoot_Id],
      T.[Instrument_Id],
      D.[RemotePerson_Id]
  ) I
  CROSS APPLY [Base].[Person::Account(Root)](I.[StorageClientRoot_Id]) A
  WHERE I.[Value] <> 0 AND A.[Organization_Id] = @Organization_Id AND A.[Kind_Id] = 'P'
  OPTION (FORCE ORDER, MAXDOP 1)


