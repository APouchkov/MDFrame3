/*
  @Method
    - 'OFFLINE'       - OffLine

  @Action
    - 'ACCESS'              - возвращает информацию о доступе пользователя к подписке или подписчику через RESULT_VALUE
    - 'OBJECTS'             - возвращает информацию о всех обьектах подписки
    - 'METADATA'            - возвращение информации о метаданных ('<PARAMS SubObject="<SubObject>"/>')

    - 'DATASET',                  - возвращение информации о синхронизируемой(мых) записи(ях)
    - 'DATASET:STATIC'            - возвращение непериодической информации о синхронизируемой(мых) записи(ях)
    - 'DATASET:PERIODIC:RAW'      - возвращение периодической информации о синхронизируемой(мых) записи(ях) в формате FIELD;DATA;VALUE
    - 'DATASET:PERIODIC:PAGE'     - возвращение объединённой информации о синхронизируемой(мых) записи(ях) в формате исторической страницы
    - 'XML:SELECT', 'XML:OUTPUT'  - возвращение информации о синхронизируемой(мых) записи(ях) в полноценном XML-Дереве
*/


  -- Список всех подписок
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Method     = 'OFFLINE',
    @Action     = 'OBJECTS'

  -- Список всех целей подписки "[Base].[Persons]"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'TARGETS'

  -- Описание полей подписки "[Base].[Persons]", цель "[Base].[Persons]"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'METADATA'

  -- Описание полей подписки "[Base].[Persons]", цель "[Base].[Persons:Addresses]"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'METADATA',
    @Params     = '<PARAMS><PARAM NAME="Target" VALUE="[Base].[Persons:Addresses]"/></PARAMS>'

  -- Простой вариант получения данных подписки "[Base].[Persons]", цель "[Base].[Persons]" (С указанием языка перевода) по фильтру "Type_Id = J",
  -- с указанием последовательности полей экспорта "Type_Id,GUId,Id"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'DATASET',
    @Params     = '<PARAMS><PARAM NAME="Language" VALUE="EN"/><PARAM NAME="Fields" VALUE="Type_Id,GUId,Id"/></PARAMS><FILTERS><FILTER NAME="Type_Id" VALUE="J"/></FILTERS>'

  -- Простой вариант получения ТЕКУЩИХ данных подписки "[Base].[Persons]", цель "[Base].[Persons:Addresses]" (С указанием языка перевода) по фильтру "Person_Id = 5754 OR Person_Id = 5755"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'DATASET',
    @Params     = '<PARAMS><PARAM NAME="Target" VALUE="[Base].[Persons:Addresses]"/><PARAM NAME="Language" VALUE="EN"/></PARAMS><FILTERS><FILTER NAME="Person_Id"><LIST VALUE="5754"/><LIST VALUE="5755"/></FILTER></FILTERS>'

  -- Простой вариант получения НЕПЕРИОДИЧЕСКИХ данных подписки "[Base].[Persons]", цель "[Base].[Persons]" (С указанием языка перевода) по фильтру "Type_Id = I"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'DATASET:STATIC',
    @Params     = '<PARAMS><PARAM NAME="Language" VALUE="EN"/></PARAMS><FILTERS><FILTER NAME="Type_Id"><LIST VALUE="I"/></FILTER></FILTERS>'

  -- Получения ПЕРИОДИЧЕСКИХ данных в формате IDENTITY/FIELD/DATE/VALUE подписки "[Base].[Persons]", цель "[Base].[Persons]" (С указанием языка перевода) по фильтру "Type_Id = I"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'DATASET:PERIODIC:RAW',
    @Params     = '<PARAMS><PARAM NAME="Language" VALUE="EN"/></PARAMS><FILTERS><FILTER NAME="Type_Id"><LIST VALUE="I"/></FILTER></FILTERS>'

  -- Получения ПЕРИОДИЧЕСКИХ данных в формате IDENTITY/DATE/<ALL PERIODIC FIELDS> подписки "[Base].[Persons]", цель "[Base].[Persons]" (С указанием языка перевода) по фильтру "Type_Id = I"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'DATASET:PERIODIC:PAGE',
    @Params     = '<PARAMS><PARAM NAME="Language" VALUE="EN"/></PARAMS><FILTERS><FILTER NAME="Type_Id"><LIST VALUE="I"/></FILTER></FILTERS>'

  -- Получения всех данных в формате XML подписки "[Base].[Persons]", цель "[Base].[Persons]" по фильтру "Type_Id = I"
  -- Результат через SELECT [XML]
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'XML:SELECT',
    @Params     = '<FILTERS><FILTER NAME="Type_Id"><LIST VALUE="I"/></FILTER></FILTERS>'

  -- Получения всех данных в формате XML подписки "[Base].[Persons]", цель "[Base].[Persons]" по фильтру "Type_Id = I"
  -- Результат через параметр @Output
DECLARE @XML XML
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'XML:OUTPUT',
    @Params     = '<FILTERS><FILTER NAME="Type_Id"><LIST VALUE="I"/></FILTER></FILTERS>',
    @Output     = @XML OUT
SELECT [OUTPUT] = @XML
