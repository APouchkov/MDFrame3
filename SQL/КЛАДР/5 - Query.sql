-- ALTER TABLE taxes_clients_2014_addresses ADD [Error] TinyInt NULL

--ALTER TABLE taxes_clients_2014_addresses ADD [A1Name] VarChar(200) NULL
--ALTER TABLE taxes_clients_2014_addresses ADD [A1Socr] VarChar(20) NULL
--ALTER TABLE taxes_clients_2014_addresses ADD [A1Code] VarChar(13) NULL


--ALTER TABLE taxes_clients_2014_addresses ADD [A2Name] VarChar(200) NULL
--ALTER TABLE taxes_clients_2014_addresses ADD [A2Socr] VarChar(20) NULL
--ALTER TABLE taxes_clients_2014_addresses ADD [A2Code] VarChar(15) NULL

--ALTER TABLE taxes_clients_2014_addresses ADD [A3Name] VarChar(200) NULL
--ALTER TABLE taxes_clients_2014_addresses ADD [A3Socr] VarChar(20) NULL
--ALTER TABLE taxes_clients_2014_addresses ADD [A3Code] VarChar(19) NULL

--ALTER TABLE taxes_clients_2014_addresses ADD [A4Name] VarChar(200) NULL
--ALTER TABLE taxes_clients_2014_addresses ADD [A4Socr] VarChar(20) NULL
--ALTER TABLE taxes_clients_2014_addresses ADD [A4Code] VarChar(19) NULL

--ALTER TABLE taxes_clients_2014_addresses ADD [A5Name] VarChar(200) NULL
--ALTER TABLE taxes_clients_2014_addresses ADD [A5Socr] VarChar(20) NULL
--ALTER TABLE taxes_clients_2014_addresses ADD [A5Code] VarChar(19) NULL

--ALTER TABLE taxes_clients_2014_addresses ADD [A6Name] VarChar(200) NULL
--ALTER TABLE taxes_clients_2014_addresses ADD [A6Socr] VarChar(20) NULL
--ALTER TABLE taxes_clients_2014_addresses ADD [A6Code] VarChar(19) NULL

--SELECT *
--INTO taxes_clients_2014_addresses
--FROM TITAN.opendb.dbo.taxes_clients_2014_addresses

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '.гор.', 'гор.')
--WHERE CharIndex('.гор.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '.г.', 'г.')
--WHERE CharIndex('.г.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '. г.', 'г.')
--WHERE CharIndex('. г.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '.с.', 'с.')
--WHERE CharIndex('.с.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '.ст.', 'ст.')
--WHERE CharIndex('.ст.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '.п.', 'п.')
--WHERE CharIndex('.п.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', -,', ',')
--WHERE CharIndex(', -,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',,', ',')
--WHERE CharIndex(',,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', ,', ',')
--WHERE CharIndex(', ,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' пр-т,', ' пр-кт.,')
--WHERE CharIndex(' пр-т,', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Тюменская обл.,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Тюменская обл.,', [Address]) > 0 AND CharIndex('Ямало-Ненецкий а', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Тюменская обл.,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Тюменская обл.,', [Address]) > 0 AND CharIndex('Ханты-Мансийский', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Тюменская обл.,', 'Ханты-Мансийский автономный округ - Югра,')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Тюменская обл.,', [Address]) > 0 AND CharIndex('г.Сургут,', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Ханты-Мансийский Автономный округ,', 'Ханты-Мансийский автономный округ - Югра,')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Ханты-Мансийский Автономный округ,', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Приморский р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Приморский р-н,', [Address]) > 0 AND (CharIndex('Санкт-Петербург', [Address]) > 0 OR CharIndex('Новороссийск', [Address]) > 0)

--UPDATE A SET [Address] = REPLACE([Address], 'р-н Приморский,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('р-н Приморский,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Сергиев-Посад', 'Сергиев Посад')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Сергиев-Посад', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Фрунзенский р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Фрунзенский р-н,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'р-н Фрунзенский,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('р-н Фрунзенский,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Выборгский р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Выборгский р-н,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'р-н Выборгский,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('р-н Выборгский,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Василеостровский р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Василеостровский р-н,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'р-н Василеостровский,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('р-н Василеостровский,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Красногвардейский р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Красногвардейский р-н,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'р-н Красногвардейский,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('р-н Красногвардейский,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Адмиралтейский р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Адмиралтейский р-н,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Петроградский р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Петроградский р-н,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'р-н Московский,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('р-н Московский,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Московский р-н.,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Московский р-н.,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Московский р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Московский р-н,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Невский р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Невский р-н,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Красносельский р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Красносельский р-н,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Кировский р-н.,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Кировский р-н.,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Пушкинский р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Пушкинский р-н,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Калининский р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Калининский р-н,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'р-н Калининский,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('р-н Калининский,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Центральный р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Центральный р-н,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'р-н Центральный,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('р-н Центральный,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], 'Кировский р-н,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('Кировский р-н,', [Address]) > 0 AND CharIndex('Санкт-Петербург', [Address]) > 0

--UPDATE A SET [Address] =
--  [Pub].[Trim Left](SubString(A.[Address], PATINDEX('%[^0-9]%', A.[Address]), 1000), ' ,.')
--FROM taxes_clients_2014_addresses A
--WHERE A.[Address] LIKE '[0-9]%'

--UPDATE A SET [Address] =
--  [Pub].[Trim Left](SubString(A.[Address], PATINDEX('%,%', A.[Address]), 1000), ' ,.')
--FROM taxes_clients_2014_addresses A
--WHERE A.[Address] LIKE 'Российская Федерация,%' OR A.[Address] LIKE 'Россия,%' OR A.[Address] LIKE 'РФ,%'

--UPDATE A SET [Address] =
--  [Pub].[Trim Left](A.[Address], ' ,.?')
--FROM taxes_clients_2014_addresses A
--WHERE A.[Address] LIKE '[., ?]%'

--UPDATE taxes_clients_2014_addresses SET [Address] = [Pub].[Trim(Spaces)]([Address])
--WHERE Left([Address], 1) = ' ' COLLATE Cyrillic_General_BIN OR Right([Address], 1) = ' ' COLLATE Cyrillic_General_BIN

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '  ', ' ')
--WHERE CharIndex('  ', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Республики ', 'Республика ')
--WHERE [Address] LIKE '%Республики %' COLLATE Cyrillic_General_BIN

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Республика Удмуртия', 'Удмуртская республика')
--WHERE [Address] LIKE '%Республика Удмуртия%' COLLATE Cyrillic_General_BIN

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Чувашия Чувашская республика', 'Чувашская Республика - Чувашия')
--WHERE CharIndex('Чувашия Чувашская республика', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Чувашская Республики', 'Чувашская Республика - Чувашия')
--WHERE CharIndex('Чувашская Республики', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' Респ,', ' респ.,')
--WHERE [Address] LIKE '% Респ,%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' область,', ' обл.,')
--WHERE [Address] LIKE '% область,%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' район,', ' р-н.,')
--WHERE [Address] LIKE '% район,%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' обл,', ' обл.,')
--WHERE [Address] LIKE '% обл,%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' пр-д,', ' пр-д.,')
--WHERE [Address] LIKE '% пр-д,%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' кр.,', ' край,')
--WHERE [Address] LIKE '% кр[.],%'

--UPDATE taxes_clients_2014_addresses SET [Address] = 'г.' + SubString([Address], 3, 1000)
--WHERE [Address] LIKE 'г %'

--UPDATE taxes_clients_2014_addresses SET [Address] = 'г.' + [Pub].[Trim Right(Spaces)](SubString([Address], 4, 1000))
--WHERE [Address] LIKE 'г[.][.,]%'

--UPDATE taxes_clients_2014_addresses SET [Address] = 'г.' + [Pub].[Trim Right(Spaces)](SubString([Address], 7, 1000))
--WHERE [Address] LIKE 'г[.]гор[.]%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'гор.', 'г.')
--WHERE [Address] LIKE 'гор[.]%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', гор.', ', г.')
--WHERE [Address] LIKE '%, гор[.]%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'г. ', 'г.')
--WHERE [Address] LIKE 'г[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' г,', ' г.,')
--WHERE CharIndex(' г,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',г. ', ', г.')
--WHERE [Address] LIKE '%,г[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', г. ', ', г.')
--WHERE [Address] LIKE '%, г[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = 'г.' + [Pub].[Trim(Spaces)](SubString([Address], 7, 1000))
--WHERE [Address] LIKE 'город %'

--UPDATE taxes_clients_2014_addresses SET [Address] = 'г.' + [Pub].[Trim(Spaces)](SubString([Address], 5, 1000))
--WHERE [Address] LIKE 'гор %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',д. ', ', д.')
--WHERE [Address] LIKE '%,д[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', д. ', ', д.')
--WHERE [Address] LIKE '%, д[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',к. ', ', к.')
--WHERE [Address] LIKE '%,к[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', к. ', ', к.')
--WHERE [Address] LIKE '%, к[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',кв. ', ', кв.')
--WHERE [Address] LIKE '%,кв[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', кв. ', ', кв.')
--WHERE [Address] LIKE '%, кв[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',ул. ', ', ул.')
--WHERE [Address] LIKE '%,ул[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', ул. ', ', ул.')
--WHERE [Address] LIKE '%, ул[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',дом ', ',д.')
--WHERE [Address] LIKE '%,дом %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',д. ', ', д.')
--WHERE [Address] LIKE '%,д[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', д. ', ', д.')
--WHERE [Address] LIKE '%, д[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',д ', ', д.')
--WHERE [Address] LIKE '%,д %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', д ', ', д.')
--WHERE [Address] LIKE '%, д %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',корп. ', ', корп.')
--WHERE CharIndex(',корп. ', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', корп. ', ', корп.')
--WHERE CharIndex(', корп. ', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' Осетия-Алания', ' Осетия - Алания')
--WHERE CharIndex(' Осетия-Алания', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты Мансийский', 'Ханты-Мансийский')
--WHERE CharIndex('Ханты Мансийский', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' округ-Югра', ' округ - Югра')
--WHERE CharIndex(' округ-Югра', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' округ- Югра', ' округ - Югра')
--WHERE CharIndex(' округ- Югра', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' округ-Юрга', ' округ - Югра')
--WHERE CharIndex(' округ-Юрга', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' АО - Югра', ' округ - Югра')
--WHERE CharIndex(' АО - Югра', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' АО-Югра', ' округ - Югра')
--WHERE CharIndex(' АО-Югра', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийский округ - Югра', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийский округ - Югра', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'ХМАО- ЮГРА', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('ХМАО- ЮГРА', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'ХМАО-ЮГРА', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('ХМАО-ЮГРА', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'АО Ханты-Мансийский Автономный округ', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('АО Ханты-Мансийский Автономный округ', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийский Автономный округ Тюменской области', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийский Автономный округ Тюменской области', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийский Автономный округ Тюменской област', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийский Автономный округ Тюменской област', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'ХАНТЫ-МАНСИЙСКИЙ АВТОНОМНЫЙ ОКРУГ ТЮМЕНСКАЯ обл.', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('ХАНТЫ-МАНСИЙСКИЙ АВТОНОМНЫЙ ОКРУГ ТЮМЕНСКАЯ обл.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Хнты-Мансийский автономный округ - Югра', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Хнты-Мансийский автономный округ - Югра', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийский АО-Югре', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийский АО-Югре', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийский автоносный округ - Югра', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийский автоносный округ - Югра', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийский Автономный округ - Югре', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийский Автономный округ - Югре', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийский автономный окргу - Югра', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийский автономный окргу - Югра', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийский автономный окгур-Югра', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийский автономный окгур-Югра', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийский автономный круг - Югра', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийский автономный круг - Югра', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийский автоном. окр-Югра', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийский автоном. окр-Югра', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийский Автоном. окр. - Югра', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийский Автоном. окр. - Югра', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийсикй автономный округ - Югра', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийсикй автономный округ - Югра', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийский автономный окр.', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийский автономный окр.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийский автономный окгруг', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийский автономный окгруг', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийсикй автономный округ', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийсикй автономный округ', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'ХМАО-Юрга', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('ХМАО-Юрга', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'ХМАО-ЮГРЕ', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('ХМАО-ЮГРЕ', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Манскийский автономный округ', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Манскийский автономный округ', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ханты-Мансийского Автономный округ', 'Ханты-Мансийский автономный округ - Югра')
--WHERE CharIndex('Ханты-Мансийского Автономный округ', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'ХМАО,', 'Ханты-Мансийский автономный округ - Югра,')
--WHERE CharIndex('ХМАО,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Санк-Петербург', 'Санкт-Петербург')
--WHERE CharIndex('Санк-Петербург', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Санкт Петербург', 'Санкт-Петербург')
--WHERE CharIndex('Санкт Петербург', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Санкт- Петербург', 'Санкт-Петербург')
--WHERE CharIndex('Санкт- Петербург', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Санкт_Петербург', 'Санкт-Петербург')
--WHERE CharIndex('Санкт_Петербург', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Санкт-Перетбург', 'Санкт-Петербург')
--WHERE CharIndex('Санкт-Перетбург', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Санкт-Петербутг', 'Санкт-Петербург')
--WHERE CharIndex('Санкт-Петербутг', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Санкт-Петерубрг', 'Санкт-Петербург')
--WHERE CharIndex('Санкт-Петерубрг', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Санкт-Петрбург', 'Санкт-Петербург')
--WHERE CharIndex('Санкт-Петрбург', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Санкт-Петребург', 'Санкт-Петербург')
--WHERE CharIndex('Санкт-Петребург', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Санкт-Петурбург', 'Санкт-Петербург')
--WHERE CharIndex('Санкт-Петурбург', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Пермский край г', 'Пермский край, г')
--WHERE CharIndex('Пермский край г', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Нижегородская обл. г', 'Нижегородская обл., г')
--WHERE CharIndex('Нижегородская обл. г', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Московская обл. г', 'Московская обл., г')
--WHERE CharIndex('Московская обл. г', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Москвоская обл.', 'Московская обл.')
--WHERE CharIndex('Москвоская обл.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Моосковская обл.', 'Московская обл.')
--WHERE CharIndex('Моосковская обл.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Москвовская обл.', 'Московская обл.')
--WHERE CharIndex('Москвовская обл.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Московкая обл.', 'Московская обл.')
--WHERE CharIndex('Московкая обл.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Московская бласть', 'Московская обл.')
--WHERE CharIndex('Московская бласть', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Московская боласть', 'Московская обл.')
--WHERE CharIndex('Московская боласть', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' обл..', ' обл.,')
--WHERE CharIndex(' обл..', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' область.,', ' область,')
--WHERE CharIndex(' область.,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' авт.обл.', ' Аобл.')
--WHERE CharIndex(' авт.обл.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Москва.,', 'Москва,')
--WHERE CharIndex('Москва.,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Москва. ', 'Москва, ')
--WHERE CharIndex('Москва. ', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'г.Москве', 'г.Москва')
--WHERE CharIndex('г.Москве', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'г.Москвы', 'г.Москва')
--WHERE CharIndex('г.Москвы', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'АО Ямало-Ненецкий', 'Ямало-Ненецкий АО.')
--WHERE CharIndex('АО Ямало-Ненецкий', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'АОБЛ Еврейская', 'Еврейская Аобл.')
--WHERE CharIndex('АОБЛ Еврейская', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Башкортостан ', 'Башкортостан Респ.,')
--WHERE [Address] LIKE 'Башкортостан [гс].%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'г.Владимир (обл)', 'Владимирская обл., г.Владимир')
--WHERE CharIndex('г.Владимир (обл)', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'г.Владимир (обл.)', 'Владимирская обл., г.Владимир')
--WHERE CharIndex('г.Владимир (обл.)', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Н.Новгород', 'Нижний Новгород')
--WHERE CharIndex('Н.Новгород', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Самарская обл. г', 'Самарская обл., г')
--WHERE CharIndex('Самарская обл. г', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Самарская область г', 'Самарская обл., г')
--WHERE CharIndex('Самарская область г', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], 'Ростовкая обл', 'Ростовская обл')
--WHERE CharIndex('Ростовкая обл', [Address]) > 0

-- UPDATE taxes_clients_2014_addresses SET [Error] = NULL

--UPDATE A SET [Error] = 0
--FROM taxes_clients_2014_addresses A
--WHERE A.[Country] IS NULL OR A.[Country] <> 643
--  OR A.[Address] IS NULL
--  OR A.[Address] IN
--      (
--        '',
--        'не указан',
--        'снят с регистрационного учета',
--        'апвпвп',
--        'арошкарозщ',
--        'Тест',
--        'тест студент',
--        'г.Тест',
--        'кщшоезщоз',
--        'шопшозу',
--        'пшкещроз',
--        'рор',
--        'папрркир'
--        )
--  OR A.[Address] NOT LIKE '%[^-]%'
--  OR [Pub].[Trim(Spaces)]([Pub].[Extract Value](A.[Address], 1, ','))
--        IN
--        ('Республика Беларусь', 'Республика Казахстан', 'Республика Молдова', 'Брестская обл.', 'Винницкая обл.', 'г.Минск', 'г.Львов', 'Украина', 'г.Тест')

--SELECT
--  I.*,
--  K1.[Code],
--  K1.[Level_Id]

GO
ALTER PROCEDURE [Taxes2014::KLADR Extract]
AS
  SET NOCOUNT ON

  UPDATE A SET
  --SELECT I.[Address],
    [Error]             = E.[Error],
    [ErrorDescription]  = COALESCE(K6.[ErrorDescription], K5.[ErrorDescription], K4.[ErrorDescription], K3.[ErrorDescription], K2.[ErrorDescription], K1.[ErrorDescription]),

    [A1Name]  = CASE WHEN K1.[Level_Id] = 1 THEN K1.[Name]    END,
    [A1Socr]  = CASE WHEN K1.[Level_Id] = 1 THEN K1.[Article] END,
    [A1Code]  = CASE WHEN K1.[Level_Id] = 1 THEN K1.[Code]    END,

    [A2Name]  = CASE WHEN K1.[Level_Id] = 2 THEN K1.[Name]    WHEN K2.[Level_Id] = 2 THEN K2.[Name]    END,
    [A2Socr]  = CASE WHEN K1.[Level_Id] = 2 THEN K1.[Article] WHEN K2.[Level_Id] = 2 THEN K2.[Article] END,
    [A2Code]  = CASE WHEN K1.[Level_Id] = 2 THEN K1.[Code]    WHEN K2.[Level_Id] = 2 THEN K2.[Code]    END,

    [A3Name]  = CASE WHEN K1.[Level_Id] = 3 THEN K1.[Name]    WHEN K2.[Level_Id] = 3 THEN K2.[Name]    WHEN K3.[Level_Id] = 3 THEN K3.[Name]    END,
    [A3Socr]  = CASE WHEN K1.[Level_Id] = 3 THEN K1.[Article] WHEN K2.[Level_Id] = 3 THEN K2.[Article] WHEN K3.[Level_Id] = 3 THEN K3.[Article] END,
    [A3Code]  = CASE WHEN K1.[Level_Id] = 3 THEN K1.[Code]    WHEN K2.[Level_Id] = 3 THEN K2.[Code]    WHEN K3.[Level_Id] = 3 THEN K3.[Code]    END,

    [A4Name]  = CASE WHEN K1.[Level_Id] = 4 THEN K1.[Name]    WHEN K2.[Level_Id] = 4 THEN K2.[Name]    WHEN K3.[Level_Id] = 4 THEN K3.[Name]    WHEN K4.[Level_Id] = 4 THEN K4.[Name]     END,
    [A4Socr]  = CASE WHEN K1.[Level_Id] = 4 THEN K1.[Article] WHEN K2.[Level_Id] = 4 THEN K2.[Article] WHEN K3.[Level_Id] = 4 THEN K3.[Article] WHEN K4.[Level_Id] = 4 THEN K4.[Article]  END,
    [A4Code]  = CASE WHEN K1.[Level_Id] = 4 THEN K1.[Code]    WHEN K2.[Level_Id] = 4 THEN K2.[Code]    WHEN K3.[Level_Id] = 4 THEN K3.[Code]    WHEN K4.[Level_Id] = 4 THEN K4.[Code]     END,

    [A5Name]  = CASE WHEN K2.[Level_Id] = 5 THEN K2.[Name]    WHEN K3.[Level_Id] = 5 THEN K3.[Name]    WHEN K4.[Level_Id] = 5 THEN K4.[Name]    WHEN K5.[Level_Id] = 5 THEN K5.[Name]     END,
    [A5Socr]  = CASE WHEN K2.[Level_Id] = 5 THEN K2.[Article] WHEN K3.[Level_Id] = 5 THEN K3.[Article] WHEN K4.[Level_Id] = 5 THEN K4.[Article] WHEN K5.[Level_Id] = 5 THEN K5.[Article]  END,
    [A5Code]  = CASE WHEN K2.[Level_Id] = 5 THEN K2.[Code]    WHEN K3.[Level_Id] = 5 THEN K3.[Code]    WHEN K4.[Level_Id] = 5 THEN K4.[Code]    WHEN K5.[Level_Id] = 5 THEN K5.[Code]     END,

    [A6Name]  = CASE WHEN K3.[Level_Id] = 6 THEN K3.[Name]    WHEN K4.[Level_Id] = 6 THEN K4.[Name]    WHEN K5.[Level_Id] = 6 THEN K5.[Name]    WHEN K6.[Level_Id] = 6 THEN K6.[Name]     END,
    [A6Socr]  = CASE WHEN K3.[Level_Id] = 6 THEN K3.[Article] WHEN K4.[Level_Id] = 6 THEN K4.[Article] WHEN K5.[Level_Id] = 6 THEN K5.[Article] WHEN K6.[Level_Id] = 6 THEN K6.[Article]  END,
    [A6Code]  = CASE WHEN K3.[Level_Id] = 6 THEN K3.[Code]    WHEN K4.[Level_Id] = 6 THEN K4.[Code]    WHEN K5.[Level_Id] = 6 THEN K5.[Code]    WHEN K6.[Level_Id] = 6 THEN K6.[Code]     END,

    [A7Name]  = CASE WHEN K3.[Level_Id] = 6 THEN K3.[NextName]WHEN K4.[Level_Id] = 6 THEN K4.[NextName]WHEN K5.[Level_Id] = 6 THEN K5.[NextName]WHEN K6.[Level_Id] = 6 THEN K6.[NextName] END
  FROM
  (
    SELECT
      [Client_Id] = A.[client_Id],
      [Address]   = Addr.[Address],
      [Index]     = A.[Index], --CASE WHEN A.[Index] IN ('468320') THEN A.[Index] WHEN A.[Index] <> '' THEN LEFT(A.[Index], 3) + '000' END,
      A1          = [Pub].[Trim(Spaces)]([Pub].[Extract Value](Addr.[Address], 1, ',')),
      A2          = [Pub].[Trim(Spaces)]([Pub].[Extract Value](Addr.[Address], 2, ',')),
      A3          = [Pub].[Trim(Spaces)]([Pub].[Extract Value](Addr.[Address], 3, ',')),
      A4          = [Pub].[Trim(Spaces)]([Pub].[Extract Value](Addr.[Address], 4, ',')),
      A5          = [Pub].[Trim(Spaces)]([Pub].[Extract Value](Addr.[Address], 5, ',')),
      A6          = [Pub].[Trim(Spaces)]([Pub].[Extract Value](Addr.[Address], 6, ',')),
      A7          = [Pub].[Trim(Spaces)]([Pub].[Extract Value](Addr.[Address], 7, ',')),
      A8          = [Pub].[Trim(Spaces)]([Pub].[Extract Value](Addr.[Address], 8, ','))
    FROM taxes_clients_2014_addresses A WITH (NOLOCK)
    CROSS APPLY (VALUES([Pub].[Trim Right](A.[Address], '.'))) Addr([Address])
    WHERE
    --A.[Error] = 1
    (A.[Error] IS NULL OR A.[Error] > 0)
      --AND [client_id] IN (select distinct client_base_id from Client cl join dogovor d on cl.client_id = d.client_id where dogovor_type=643)
  ) I
  OUTER APPLY [dbo].[KLADR::Search(Pre)](0, NULL, I.[A1], NULL, NULL, NULL) K1
  OUTER APPLY
  (
    SELECT K2.*
    FROM [dbo].[KLADR::Search(Pre)](K1.[Level_Id], K1.[Code], I.[A2], I.[A3], I.[A4], I.[Index]) K2
    WHERE K1.[Code] IS NOT NULL
  ) K2
  OUTER APPLY
  (
    SELECT K3.*
    FROM [dbo].[KLADR::Search(Pre)](K2.[Level_Id], K2.[Code], I.[A3], I.[A4], I.[A5], I.[Index]) K3
    WHERE K2.[Code] IS NOT NULL
  ) K3
  OUTER APPLY
  (
    SELECT K4.*
    FROM [dbo].[KLADR::Search(Pre)](K3.[Level_Id], K3.[Code], I.[A4], I.[A5], I.[A6], I.[Index]) K4
    WHERE K3.[Code] IS NOT NULL AND K3.[Level_Id] < 6
  ) K4
  OUTER APPLY
  (
    SELECT K5.*
    FROM [dbo].[KLADR::Search(Pre)](K4.[Level_Id], K4.[Code], I.[A5], I.[A6], I.[A7], I.[Index]) K5
    WHERE K4.[Code] IS NOT NULL AND K4.[Level_Id] < 6
  ) K5
  OUTER APPLY
  (
    SELECT K6.*
    FROM [dbo].[KLADR::Search(Pre)](K5.[Level_Id], K5.[Code], I.[A6], I.[A7], I.[A8], NULL) K6
    WHERE K5.[Code] IS NOT NULL AND K5.[Level_Id] < 6
  ) K6
  OUTER APPLY
  (
    SELECT
      [Error] = CASE
                  WHEN K1.[Code] IS NULL THEN 1
                  WHEN K2.[Code] IS NULL THEN 2
                  WHEN K3.[Code] IS NULL THEN 3
                  WHEN K3.[Level_Id] = 6 AND K3.[Code] IS NOT NULL 
                    OR K4.[Level_Id] = 6 AND K4.[Code] IS NOT NULL 
                    OR K5.[Level_Id] = 6 AND K5.[Code] IS NOT NULL 
                    OR K6.[Level_Id] = 6 AND K6.[Code] IS NOT NULL
                      THEN NULL
                  WHEN I.[A4] IS NOT NULL AND K4.[Code] IS NULL THEN 4
                  WHEN I.[A5] IS NOT NULL AND K5.[Code] IS NULL THEN 5
                  WHEN I.[A6] IS NOT NULL AND K6.[Code] IS NULL THEN 6
                END
  ) E

  INNER JOIN taxes_clients_2014_addresses A ON I.[Client_Id] = A.[Client_Id]
GO

--ORDER BY I.[Address]


--SELECT * FROM taxes_clients_2014_addresses WHERE [Error] IS NULL OR [Error] IN (1,2,3,4,5,6)
--SELECT * FROM taxes_clients_2014_addresses WHERE [Error] = 0

--EXEC [Taxes2014::KLADR Extract]
--SELECT * FROM taxes_clients_2014_addresses WHERE [Error] = 2 ORDER BY 6
--SELECT * FROM taxes_clients_2014_addresses WHERE [Error] IN (2,3,4,5,6)


  --SELECT DISTINCT
  --  [Name]  = LTRIM(RTRIM(V.[Value])),
  --  [Code]  = D.[Code],
  --  [Index] = D.[Index]
  --FROM [dbo].[KLADR:Doma] D
  --OUTER APPLY [Pub].[Split](D.[Name], ',') V
  --WHERE CHARINDEX('-', D.Name) > 1
  --  AND LEFT(V.Value, 2) NOT IN ('Ч(', 'Н(')
  --  AND CHARINDEX('-', V.Value) > 1 AND ISNUMERIC(LEFT(V.Value, 1)) = 0
  --ORDER BY 1

--SELECT DISTINCT [SOCR] FROM [KLADR:Doma]

-- SELECT * FROM [KLADR:Streets] WHERE CharIndex('ИЗЛУЧИНСК', [NAME])  > 0 AND [Code] LIKE '78%'
-- SELECT * FROM [KLADR:Base] WHERE CharIndex('Топольное', [NAME])  > 0 AND [Code] LIKE '22%'

