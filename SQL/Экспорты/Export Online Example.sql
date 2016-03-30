/*
  @Method
    - 'ONLINE'       - OnLine

  @Action
    - 'ACCESS'              - возвращает информацию о доступе пользователя к подписке или подписчику через RESULT_VALUE
    - 'OBJECTS'             - возвращает информацию о всех обьектах подписки
    - 'METADATA'            - возвращение информации о метаданных ('<PARAMS SubObject="<SubObject>"/>')

    - 'BEGIN'               - начало онлайновой синхронизации (локировка и подготовка актуальных данных
    - 'ROWS'                - возвращение идентификаторов актуальных записей онлайновой синхронизации
    - 'REFILL'              - перезаполнение буфера

    - 'DATASET',                  - возвращение информации о синхронизируемой(мых) записи(ях)
    - 'DATASET:STATIC'            - возвращение непериодической информации о синхронизируемой(мых) записи(ях)
    - 'DATASET:PERIODIC:RAW'      - возвращение периодической информации о синхронизируемой(мых) записи(ях) в формате FIELD;DATA;VALUE
    - 'DATASET:PERIODIC:PAGE'     - возвращение объединённой информации о синхронизируемой(мых) записи(ях) в формате исторической страницы
    - 'XML:SELECT', 'XML:OUTPUT'  - возвращение информации о синхронизируемой(мых) записи(ях) в полноценном XML-Дереве
*/

  -- Запуск ONLINE экспорта данных по подписке "[dbo].[client]"
  EXEC [Export].[Execute]
    @SubScriber = 'Courier',
    @Object     = '[dbo].[client]',
    @Method     = 'ONLINE',
    @Action     = 'BEGIN'

  -- Получение списка ID-шников и действий в ДатаСете [Id], [Action]
  -- [Action] = 'I' -- Запись была добавлена
  -- [Action] = 'U' -- Запись была изменена
  -- [Action] = 'R' -- Запись была удалена и вновь добавлена
  -- [Action] = 'D' -- Запись была удалена
  EXEC [Export].[Execute]
    @SubScriber = 'Courier',
    @Object     = '[dbo].[client]',
    @Method     = 'ONLINE',
    @Action     = 'ROWS'
    --@Params     = '<FILTERS><FILTER NAME="Type_Id"><LIST VALUE="J"/></FILTER></FILTERS>'

  -- Получение ДатаСета с изменёнными данными в формате SQL-RowSet
  --EXEC [Export].[Execute]
  --  @SubScriber = 'Courier',
  --  @Object     = '[dbo].[client]',
  --  @Method     = 'ONLINE',
  --  @Action     = 'DATASET:PERIODIC:RAW',
    --@Params     = '<PARAMS><PARAM NAME="Language" VALUE="EN"/></PARAMS>'

  -- Получение изменённых данных с признаком действия («ACTION»), во всех языках и по всем периодическим датам в формате XML
  EXEC [Export].[Execute]
    @SubScriber = 'Courier',
    @Object     = '[dbo].[client]',
    @Method     = 'ONLINE',
    @Action     = 'DATASET'

  EXEC [Export].[Execute]
    @SubScriber = 'Courier',
    @Object     = '[dbo].[client]',
    @Method     = 'ONLINE',
    @Action     = 'XML:SELECT'

  -- Проставление признаков синхронизации записям в буфере экспорта
  -- ID="2874"                              -- Синхронизированно успешно
  -- ID="2988" ERROR="Неопознанная ошибка"  -- Ошибка при синхронизации
  -- Все неуказанные в этой операции записи, а также записи с ошибкой остаются в буфере до следующего запуска системы экспорта данных
/*
  EXEC [Export].[Execute]
    @SubScriber = 'Courier',
    @Object     = '[dbo].[client]',
    @Method     = 'ONLINE',
    @Action     = 'RESULTS',
    @Params     = '<RESULTS><RESULT ID="2874"/><RESULT ID="2988" ERROR="Неопознанная ошибка"/></RESULTS>'
*/

  -- Окончание ONLINE экспорта данных по подписке "[dbo].[client]"
  EXEC [Export].[Execute]
    @SubScriber = 'Courier',
    @Object     = '[dbo].[client]',
    @Method     = 'ONLINE',
    @Action     = 'END'
GO

