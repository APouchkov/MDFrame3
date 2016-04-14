-- Пользователи WorkFlow
CREATE TABLE [Workflow].[Users]
(
	[Id]    [tinyint]       NOT NULL,
	[Code]  [varchar](50)   NOT NULL, -- Код пользователя
	[Name]  [nvarchar](200) NOT NULL, -- Наименование
	[Login] [nvarchar](128) NOT NULL, -- SQL-Логин, имеющий право под ним работать или "*" для работы без проверки
) ON [PRIMARY]
GO
-- Объекты/процессы WorkFlow
CREATE TABLE [Workflow].[Objects]
(
	[Id]                      [smallint] IDENTITY(1,1) NOT NULL,
	[Code]                    [varchar](50)   NOT NULL,   -- Код процесса
	[Name]                    [nvarchar](200) NOT NULL,   -- Наименование
	[Modes]                   [varchar](3)    NOT NULL,   -- Доступные режимы
	                                                      -- [
	                                                      --    D - Прямой доступ (без создания процесса),
	                                                      --    S - Запуск в синхронном режиме
	                                                      --    A - Запуск в асинхронном режиме
	                                                      -- ]
	[IntermediateParamNames]  [varchar](4000)     NULL,   -- Параметры для возврата из стартового процесса (Для незаконченных процессов)
	[IntermediateDataNames]   [varchar](4000)     NULL,   -- Данные для возврата из стартового процесса (Для незаконченных процессов)
) ON [PRIMARY]
GO
-- Права доступа на создание процесса
CREATE TABLE [Workflow].[Objects:SubScriptions]
(
	[User_Id]   [tinyint]   NOT NULL, -- Ссылка на пользователя
	[Object_Id] [smallint]  NOT NULL, -- Ссылка на процесс
) ON [PRIMARY]
GO
-- Точки исполнения процесса
CREATE TABLE [Workflow].[Objects:Items]
(
	[Object_Id]               [smallint]      NOT NULL, -- Ссылка на процесс
	[Id]                      [tinyint]       NOT NULL, -- ИД внутри процесса
	[Description]             [nvarchar](200) NOT NULL, -- Описание точки
	[Procedure]               [sysname]           NULL, -- SQL процедура-обработчик (может быть NULL)
	[Transaction]             [bit]           NOT NULL, -- Выполнять во внешней контролируемой транзакции
	[RepeateCondition]        [nvarchar](1000)    NULL, -- Условие повторения
	[RepeateCount]            [smallint]          NULL, -- Кол-во повторений
	[RepeateDelay]            [varchar](10)       NULL, -- Задержка повторения
	[RepeateOverrunConstants] [TParams]           NULL, -- Константы для сохранения в SavedParams при возникновении условия повторения
	[RepeateOverrunData]      [xml]               NULL, -- Данные для сохранения в SavedData при возникновении условия повторения
	[SaveParamNames]          [varchar](4000)     NULL, -- Имена параметров из OutParams которые сохранятся в SavedParams
	[SaveDataNames]           [varchar](4000)     NULL, -- Имена тэгов данных из OutData которые сохранятся в SavedData
) ON [PRIMARY]
GO
-- Параметры (для коллекций [Params]) распознавания XML при входе в [Create] или [Continue]
CREATE TABLE [Workflow].[Objects:Params]
(
	[Object_Id]   [smallint]      NOT NULL, -- Ссылка на процесс
	[Item_Id]     [tinyint]       NOT NULL, -- Ссылка на ИД точки процесса
	[Name]        [varchar](128)  NOT NULL, -- Имя параметра (Если начинается с "@", то это атрибут главного элемента XML)
	[Type]        [sysname]       NOT NULL, -- Тип параметра
	[Description] [nvarchar](200) NOT NULL, -- Описание
	[Required]    [bit]           NOT NULL, -- Признак обязательности наличия
	[Default]     [nvarchar](4000)    NULL, -- Значение по-умолчанию
) ON [PRIMARY]
GO
-- Правила переходов между точками процессов
CREATE TABLE [Workflow].[Objects:Steps]
(
	[Object_Id]         [smallint]      NOT NULL, -- Ссылка на процесс
	[Id]                [smallint]      NOT NULL, -- ИД правила перехода
	[ParentItem_Id]     [tinyint]           NULL, -- Ссылка на предыдущую точку (NULL - для входного процесса)
	[NextItem_Id]       [tinyint]           NULL, -- Ссылка на следующую точку (NULL - для завершения процесса)
	[HookItem_Id]       [tinyint]           NULL, -- Ссылка на точку-ловушку (внешние ожидания созданются заранее, чтобы успеть ДО перехода)
	[HookUser_Id]       [tinyint]           NULL, -- Ссылка на пользователя, имеющего право продолжить процесс-ловушку
	[Description]       [nvarchar](200)     NULL, -- Описание перехода
	[User_Id]           [tinyint]           NULL, -- Фильтр правила по пользователю
	[Mode]              [char](1)           NULL, -- Фильтр правила по режиму работы
	[Condition]         [nvarchar](1000)    NULL, -- Фильтр правила по активной коллекции [Params]
	[WaitFor]           [varchar](10)       NULL, -- Задержка при переходе
	[Async]             [bit]           NOT NULL, -- Асинхронный переход
  -- Коллекция параметров для активации следующей точки
	[QueueParamNames]   [varchar](4000)     NULL, -- Из параметров запуска процесса
	[QueueDataNames]    [varchar](4000)     NULL, -- Из данных запуска процесса
	[SavedParamNames]   [varchar](4000)     NULL, -- Из параметров, сохранённых при работе процесса
	[SavedDataNames]    [varchar](4000)     NULL, -- Из данных, сохранённых при работе процесса
	[ParentParamNames]  [varchar](4000)     NULL, -- Из параметров, исходящих из предыдущей точки
	[ParentDataNames]   [varchar](4000)     NULL, -- Из данных, исходящих из предыдущей точки
	[Constants]         [TParams]           NULL, -- Константы
) ON [PRIMARY]
GO

-------------------------------------------------------------
-- Служебные таблицы
-------------------------------------------------------------

-- Зарегистрированные процессы
CREATE TABLE [Workflow].[Queue]
(
	[Id]              [int] IDENTITY(1,1) NOT NULL,
	[Object_Id]       [smallint]    NOT NULL,
	[User_Id]         [tinyint]     NOT NULL,
	[Mode]            [char](1)     NOT NULL,
	[Identity]        [varchar](50)     NULL,
	[Parent_Id]       [int]             NULL,
	[Params]          [TParams]         NULL,
	[Data]            [xml]             NULL,
	[CreateDateTime]  [datetime]    NOT NULL,
	[AsyncThreads]    [tinyint]         NULL,
	[SavedData]       [xml]             NULL,
	[SavedParams]     [TParams]         NULL,
) ON [PRIMARY]
GO
