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

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '.���.', '���.')
--WHERE CharIndex('.���.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '.�.', '�.')
--WHERE CharIndex('.�.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '. �.', '�.')
--WHERE CharIndex('. �.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '.�.', '�.')
--WHERE CharIndex('.�.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '.��.', '��.')
--WHERE CharIndex('.��.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '.�.', '�.')
--WHERE CharIndex('.�.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', -,', ',')
--WHERE CharIndex(', -,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',,', ',')
--WHERE CharIndex(',,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', ,', ',')
--WHERE CharIndex(', ,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' ��-�,', ' ��-��.,')
--WHERE CharIndex(' ��-�,', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '��������� ���.,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('��������� ���.,', [Address]) > 0 AND CharIndex('�����-�������� �', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '��������� ���.,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('��������� ���.,', [Address]) > 0 AND CharIndex('�����-����������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '��������� ���.,', '�����-���������� ���������� ����� - ����,')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('��������� ���.,', [Address]) > 0 AND CharIndex('�.������,', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '�����-���������� ���������� �����,', '�����-���������� ���������� ����� - ����,')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('�����-���������� ���������� �����,', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '���������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('���������� �-�,', [Address]) > 0 AND (CharIndex('�����-���������', [Address]) > 0 OR CharIndex('������������', [Address]) > 0)

--UPDATE A SET [Address] = REPLACE([Address], '�-� ����������,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('�-� ����������,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '�������-�����', '������� �����')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('�������-�����', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '����������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('����������� �-�,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '�-� �����������,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('�-� �����������,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '���������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('���������� �-�,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '�-� ����������,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('�-� ����������,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '���������������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('���������������� �-�,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '�-� ����������������,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('�-� ����������������,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '����������������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('����������������� �-�,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '�-� �����������������,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('�-� �����������������,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '�������������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('�������������� �-�,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '������������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('������������� �-�,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '�-� ����������,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('�-� ����������,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '���������� �-�.,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('���������� �-�.,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '���������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('���������� �-�,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('������� �-�,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '�������������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('�������������� �-�,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '��������� �-�.,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('��������� �-�.,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '���������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('���������� �-�,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '����������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('����������� �-�,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '�-� �����������,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('�-� �����������,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '����������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('����������� �-�,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '�-� �����������,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('�-� �����������,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] = REPLACE([Address], '��������� �-�,', '')
--FROM taxes_clients_2014_addresses A
--WHERE CharIndex('��������� �-�,', [Address]) > 0 AND CharIndex('�����-���������', [Address]) > 0

--UPDATE A SET [Address] =
--  [Pub].[Trim Left](SubString(A.[Address], PATINDEX('%[^0-9]%', A.[Address]), 1000), ' ,.')
--FROM taxes_clients_2014_addresses A
--WHERE A.[Address] LIKE '[0-9]%'

--UPDATE A SET [Address] =
--  [Pub].[Trim Left](SubString(A.[Address], PATINDEX('%,%', A.[Address]), 1000), ' ,.')
--FROM taxes_clients_2014_addresses A
--WHERE A.[Address] LIKE '���������� ���������,%' OR A.[Address] LIKE '������,%' OR A.[Address] LIKE '��,%'

--UPDATE A SET [Address] =
--  [Pub].[Trim Left](A.[Address], ' ,.?')
--FROM taxes_clients_2014_addresses A
--WHERE A.[Address] LIKE '[., ?]%'

--UPDATE taxes_clients_2014_addresses SET [Address] = [Pub].[Trim(Spaces)]([Address])
--WHERE Left([Address], 1) = ' ' COLLATE Cyrillic_General_BIN OR Right([Address], 1) = ' ' COLLATE Cyrillic_General_BIN

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '  ', ' ')
--WHERE CharIndex('  ', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '���������� ', '���������� ')
--WHERE [Address] LIKE '%���������� %' COLLATE Cyrillic_General_BIN

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '���������� ��������', '���������� ����������')
--WHERE [Address] LIKE '%���������� ��������%' COLLATE Cyrillic_General_BIN

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '������� ��������� ����������', '��������� ���������� - �������')
--WHERE CharIndex('������� ��������� ����������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '��������� ����������', '��������� ���������� - �������')
--WHERE CharIndex('��������� ����������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' ����,', ' ����.,')
--WHERE [Address] LIKE '% ����,%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' �������,', ' ���.,')
--WHERE [Address] LIKE '% �������,%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' �����,', ' �-�.,')
--WHERE [Address] LIKE '% �����,%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' ���,', ' ���.,')
--WHERE [Address] LIKE '% ���,%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' ��-�,', ' ��-�.,')
--WHERE [Address] LIKE '% ��-�,%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' ��.,', ' ����,')
--WHERE [Address] LIKE '% ��[.],%'

--UPDATE taxes_clients_2014_addresses SET [Address] = '�.' + SubString([Address], 3, 1000)
--WHERE [Address] LIKE '� %'

--UPDATE taxes_clients_2014_addresses SET [Address] = '�.' + [Pub].[Trim Right(Spaces)](SubString([Address], 4, 1000))
--WHERE [Address] LIKE '�[.][.,]%'

--UPDATE taxes_clients_2014_addresses SET [Address] = '�.' + [Pub].[Trim Right(Spaces)](SubString([Address], 7, 1000))
--WHERE [Address] LIKE '�[.]���[.]%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '���.', '�.')
--WHERE [Address] LIKE '���[.]%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', ���.', ', �.')
--WHERE [Address] LIKE '%, ���[.]%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�. ', '�.')
--WHERE [Address] LIKE '�[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' �,', ' �.,')
--WHERE CharIndex(' �,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',�. ', ', �.')
--WHERE [Address] LIKE '%,�[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', �. ', ', �.')
--WHERE [Address] LIKE '%, �[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = '�.' + [Pub].[Trim(Spaces)](SubString([Address], 7, 1000))
--WHERE [Address] LIKE '����� %'

--UPDATE taxes_clients_2014_addresses SET [Address] = '�.' + [Pub].[Trim(Spaces)](SubString([Address], 5, 1000))
--WHERE [Address] LIKE '��� %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',�. ', ', �.')
--WHERE [Address] LIKE '%,�[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', �. ', ', �.')
--WHERE [Address] LIKE '%, �[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',�. ', ', �.')
--WHERE [Address] LIKE '%,�[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', �. ', ', �.')
--WHERE [Address] LIKE '%, �[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',��. ', ', ��.')
--WHERE [Address] LIKE '%,��[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', ��. ', ', ��.')
--WHERE [Address] LIKE '%, ��[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',��. ', ', ��.')
--WHERE [Address] LIKE '%,��[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', ��. ', ', ��.')
--WHERE [Address] LIKE '%, ��[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',��� ', ',�.')
--WHERE [Address] LIKE '%,��� %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',�. ', ', �.')
--WHERE [Address] LIKE '%,�[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', �. ', ', �.')
--WHERE [Address] LIKE '%, �[.] %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',� ', ', �.')
--WHERE [Address] LIKE '%,� %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', � ', ', �.')
--WHERE [Address] LIKE '%, � %'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ',����. ', ', ����.')
--WHERE CharIndex(',����. ', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ', ����. ', ', ����.')
--WHERE CharIndex(', ����. ', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' ������-������', ' ������ - ������')
--WHERE CharIndex(' ������-������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '����� ����������', '�����-����������')
--WHERE CharIndex('����� ����������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' �����-����', ' ����� - ����')
--WHERE CharIndex(' �����-����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' �����- ����', ' ����� - ����')
--WHERE CharIndex(' �����- ����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' �����-����', ' ����� - ����')
--WHERE CharIndex(' �����-����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' �� - ����', ' ����� - ����')
--WHERE CharIndex(' �� - ����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' ��-����', ' ����� - ����')
--WHERE CharIndex(' ��-����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ����� - ����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ����� - ����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '����- ����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('����- ����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '����-����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('����-����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�� �����-���������� ���������� �����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�� �����-���������� ���������� �����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ���������� ����� ��������� �������', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ���������� ����� ��������� �������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ���������� ����� ��������� ������', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ���������� ����� ��������� ������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ���������� ����� ��������� ���.', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ���������� ����� ��������� ���.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '����-���������� ���������� ����� - ����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('����-���������� ���������� ����� - ����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ��-����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ��-����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ���������� ����� - ����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ���������� ����� - ����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ���������� ����� - ����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ���������� ����� - ����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ���������� ����� - ����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ���������� ����� - ����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ���������� �����-����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ���������� �����-����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ���������� ���� - ����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ���������� ���� - ����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� �������. ���-����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� �������. ���-����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� �������. ���. - ����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� �������. ���. - ����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ���������� ����� - ����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ���������� ����� - ����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ���������� ���.', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ���������� ���.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ���������� ������', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ���������� ������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������� ���������� �����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-���������� ���������� �����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '����-����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('����-����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '����-����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('����-����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-����������� ���������� �����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-����������� ���������� �����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-����������� ���������� �����', '�����-���������� ���������� ����� - ����')
--WHERE CharIndex('�����-����������� ���������� �����', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '����,', '�����-���������� ���������� ����� - ����,')
--WHERE CharIndex('����,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '����-���������', '�����-���������')
--WHERE CharIndex('����-���������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '����� ���������', '�����-���������')
--WHERE CharIndex('����� ���������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����- ���������', '�����-���������')
--WHERE CharIndex('�����- ���������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����_���������', '�����-���������')
--WHERE CharIndex('�����_���������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������', '�����-���������')
--WHERE CharIndex('�����-���������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������', '�����-���������')
--WHERE CharIndex('�����-���������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������', '�����-���������')
--WHERE CharIndex('�����-���������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-��������', '�����-���������')
--WHERE CharIndex('�����-��������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������', '�����-���������')
--WHERE CharIndex('�����-���������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�����-���������', '�����-���������')
--WHERE CharIndex('�����-���������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�������� ���� �', '�������� ����, �')
--WHERE CharIndex('�������� ���� �', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '������������� ���. �', '������������� ���., �')
--WHERE CharIndex('������������� ���. �', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '���������� ���. �', '���������� ���., �')
--WHERE CharIndex('���������� ���. �', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '���������� ���.', '���������� ���.')
--WHERE CharIndex('���������� ���.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '����������� ���.', '���������� ���.')
--WHERE CharIndex('����������� ���.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '����������� ���.', '���������� ���.')
--WHERE CharIndex('����������� ���.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '��������� ���.', '���������� ���.')
--WHERE CharIndex('��������� ���.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '���������� ������', '���������� ���.')
--WHERE CharIndex('���������� ������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '���������� �������', '���������� ���.')
--WHERE CharIndex('���������� �������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' ���..', ' ���.,')
--WHERE CharIndex(' ���..', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' �������.,', ' �������,')
--WHERE CharIndex(' �������.,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], ' ���.���.', ' ����.')
--WHERE CharIndex(' ���.���.', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '������.,', '������,')
--WHERE CharIndex('������.,', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '������. ', '������, ')
--WHERE CharIndex('������. ', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�.������', '�.������')
--WHERE CharIndex('�.������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�.������', '�.������')
--WHERE CharIndex('�.������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�� �����-��������', '�����-�������� ��.')
--WHERE CharIndex('�� �����-��������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '���� ���������', '��������� ����.')
--WHERE CharIndex('���� ���������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '������������ ', '������������ ����.,')
--WHERE [Address] LIKE '������������ [��].%'

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�.�������� (���)', '������������ ���., �.��������')
--WHERE CharIndex('�.�������� (���)', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�.�������� (���.)', '������������ ���., �.��������')
--WHERE CharIndex('�.�������� (���.)', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '�.��������', '������ ��������')
--WHERE CharIndex('�.��������', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '��������� ���. �', '��������� ���., �')
--WHERE CharIndex('��������� ���. �', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '��������� ������� �', '��������� ���., �')
--WHERE CharIndex('��������� ������� �', [Address]) > 0

--UPDATE taxes_clients_2014_addresses SET [Address] = REPLACE([Address], '��������� ���', '���������� ���')
--WHERE CharIndex('��������� ���', [Address]) > 0

-- UPDATE taxes_clients_2014_addresses SET [Error] = NULL

--UPDATE A SET [Error] = 0
--FROM taxes_clients_2014_addresses A
--WHERE A.[Country] IS NULL OR A.[Country] <> 643
--  OR A.[Address] IS NULL
--  OR A.[Address] IN
--      (
--        '',
--        '�� ������',
--        '���� � ���������������� �����',
--        '������',
--        '����������',
--        '����',
--        '���� �������',
--        '�.����',
--        '���������',
--        '�������',
--        '��������',
--        '���',
--        '��������'
--        )
--  OR A.[Address] NOT LIKE '%[^-]%'
--  OR [Pub].[Trim(Spaces)]([Pub].[Extract Value](A.[Address], 1, ','))
--        IN
--        ('���������� ��������', '���������� ���������', '���������� �������', '��������� ���.', '��������� ���.', '�.�����', '�.�����', '�������', '�.����')

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
  --  AND LEFT(V.Value, 2) NOT IN ('�(', '�(')
  --  AND CHARINDEX('-', V.Value) > 1 AND ISNUMERIC(LEFT(V.Value, 1)) = 0
  --ORDER BY 1

--SELECT DISTINCT [SOCR] FROM [KLADR:Doma]

-- SELECT * FROM [KLADR:Streets] WHERE CharIndex('���������', [NAME])  > 0 AND [Code] LIKE '78%'
-- SELECT * FROM [KLADR:Base] WHERE CharIndex('���������', [NAME])  > 0 AND [Code] LIKE '22%'

