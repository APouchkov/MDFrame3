/*
  @Method
    - 'ONLINE'       - OnLine

  @Action
    - 'ACCESS'              - ���������� ���������� � ������� ������������ � �������� ��� ���������� ����� RESULT_VALUE
    - 'OBJECTS'             - ���������� ���������� � ���� �������� ��������
    - 'METADATA'            - ����������� ���������� � ���������� ('<PARAMS SubObject="<SubObject>"/>')

    - 'BEGIN'               - ������ ���������� ������������� (��������� � ���������� ���������� ������
    - 'ROWS'                - ����������� ��������������� ���������� ������� ���������� �������������
    - 'REFILL'              - �������������� ������

    - 'DATASET',                  - ����������� ���������� � ����������������(���) ������(��)
    - 'DATASET:STATIC'            - ����������� ��������������� ���������� � ����������������(���) ������(��)
    - 'DATASET:PERIODIC:RAW'      - ����������� ������������� ���������� � ����������������(���) ������(��) � ������� FIELD;DATA;VALUE
    - 'DATASET:PERIODIC:PAGE'     - ����������� ����������� ���������� � ����������������(���) ������(��) � ������� ������������ ��������
    - 'XML:SELECT', 'XML:OUTPUT'  - ����������� ���������� � ����������������(���) ������(��) � ����������� XML-������
*/

  -- ������ ONLINE �������� ������ �� �������� "[dbo].[client]"
  EXEC [Export].[Execute]
    @SubScriber = 'Courier',
    @Object     = '[dbo].[client]',
    @Method     = 'ONLINE',
    @Action     = 'BEGIN'

  -- ��������� ������ ID-������ � �������� � �������� [Id], [Action]
  -- [Action] = 'I' -- ������ ���� ���������
  -- [Action] = 'U' -- ������ ���� ��������
  -- [Action] = 'R' -- ������ ���� ������� � ����� ���������
  -- [Action] = 'D' -- ������ ���� �������
  EXEC [Export].[Execute]
    @SubScriber = 'Courier',
    @Object     = '[dbo].[client]',
    @Method     = 'ONLINE',
    @Action     = 'ROWS'
    --@Params     = '<FILTERS><FILTER NAME="Type_Id"><LIST VALUE="J"/></FILTER></FILTERS>'

  -- ��������� �������� � ���������� ������� � ������� SQL-RowSet
  --EXEC [Export].[Execute]
  --  @SubScriber = 'Courier',
  --  @Object     = '[dbo].[client]',
  --  @Method     = 'ONLINE',
  --  @Action     = 'DATASET:PERIODIC:RAW',
    --@Params     = '<PARAMS><PARAM NAME="Language" VALUE="EN"/></PARAMS>'

  -- ��������� ��������� ������ � ��������� �������� (�ACTION�), �� ���� ������ � �� ���� ������������� ����� � ������� XML
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

  -- ������������ ��������� ������������� ������� � ������ ��������
  -- ID="2874"                              -- ����������������� �������
  -- ID="2988" ERROR="������������ ������"  -- ������ ��� �������������
  -- ��� ����������� � ���� �������� ������, � ����� ������ � ������� �������� � ������ �� ���������� ������� ������� �������� ������
/*
  EXEC [Export].[Execute]
    @SubScriber = 'Courier',
    @Object     = '[dbo].[client]',
    @Method     = 'ONLINE',
    @Action     = 'RESULTS',
    @Params     = '<RESULTS><RESULT ID="2874"/><RESULT ID="2988" ERROR="������������ ������"/></RESULTS>'
*/

  -- ��������� ONLINE �������� ������ �� �������� "[dbo].[client]"
  EXEC [Export].[Execute]
    @SubScriber = 'Courier',
    @Object     = '[dbo].[client]',
    @Method     = 'ONLINE',
    @Action     = 'END'
GO

