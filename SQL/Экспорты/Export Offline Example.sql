/*
  @Method
    - 'OFFLINE'       - OffLine

  @Action
    - 'ACCESS'              - ���������� ���������� � ������� ������������ � �������� ��� ���������� ����� RESULT_VALUE
    - 'OBJECTS'             - ���������� ���������� � ���� �������� ��������
    - 'METADATA'            - ����������� ���������� � ���������� ('<PARAMS SubObject="<SubObject>"/>')

    - 'DATASET',                  - ����������� ���������� � ����������������(���) ������(��)
    - 'DATASET:STATIC'            - ����������� ��������������� ���������� � ����������������(���) ������(��)
    - 'DATASET:PERIODIC:RAW'      - ����������� ������������� ���������� � ����������������(���) ������(��) � ������� FIELD;DATA;VALUE
    - 'DATASET:PERIODIC:PAGE'     - ����������� ����������� ���������� � ����������������(���) ������(��) � ������� ������������ ��������
    - 'XML:SELECT', 'XML:OUTPUT'  - ����������� ���������� � ����������������(���) ������(��) � ����������� XML-������
*/


  -- ������ ���� ��������
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Method     = 'OFFLINE',
    @Action     = 'OBJECTS'

  -- ������ ���� ����� �������� "[Base].[Persons]"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'TARGETS'

  -- �������� ����� �������� "[Base].[Persons]", ���� "[Base].[Persons]"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'METADATA'

  -- �������� ����� �������� "[Base].[Persons]", ���� "[Base].[Persons:Addresses]"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'METADATA',
    @Params     = '<PARAMS><PARAM NAME="Target" VALUE="[Base].[Persons:Addresses]"/></PARAMS>'

  -- ������� ������� ��������� ������ �������� "[Base].[Persons]", ���� "[Base].[Persons]" (� ��������� ����� ��������) �� ������� "Type_Id = J",
  -- � ��������� ������������������ ����� �������� "Type_Id,GUId,Id"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'DATASET',
    @Params     = '<PARAMS><PARAM NAME="Language" VALUE="EN"/><PARAM NAME="Fields" VALUE="Type_Id,GUId,Id"/></PARAMS><FILTERS><FILTER NAME="Type_Id" VALUE="J"/></FILTERS>'

  -- ������� ������� ��������� ������� ������ �������� "[Base].[Persons]", ���� "[Base].[Persons:Addresses]" (� ��������� ����� ��������) �� ������� "Person_Id = 5754 OR Person_Id = 5755"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'DATASET',
    @Params     = '<PARAMS><PARAM NAME="Target" VALUE="[Base].[Persons:Addresses]"/><PARAM NAME="Language" VALUE="EN"/></PARAMS><FILTERS><FILTER NAME="Person_Id"><LIST VALUE="5754"/><LIST VALUE="5755"/></FILTER></FILTERS>'

  -- ������� ������� ��������� ��������������� ������ �������� "[Base].[Persons]", ���� "[Base].[Persons]" (� ��������� ����� ��������) �� ������� "Type_Id = I"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'DATASET:STATIC',
    @Params     = '<PARAMS><PARAM NAME="Language" VALUE="EN"/></PARAMS><FILTERS><FILTER NAME="Type_Id"><LIST VALUE="I"/></FILTER></FILTERS>'

  -- ��������� ������������� ������ � ������� IDENTITY/FIELD/DATE/VALUE �������� "[Base].[Persons]", ���� "[Base].[Persons]" (� ��������� ����� ��������) �� ������� "Type_Id = I"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'DATASET:PERIODIC:RAW',
    @Params     = '<PARAMS><PARAM NAME="Language" VALUE="EN"/></PARAMS><FILTERS><FILTER NAME="Type_Id"><LIST VALUE="I"/></FILTER></FILTERS>'

  -- ��������� ������������� ������ � ������� IDENTITY/DATE/<ALL PERIODIC FIELDS> �������� "[Base].[Persons]", ���� "[Base].[Persons]" (� ��������� ����� ��������) �� ������� "Type_Id = I"
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'DATASET:PERIODIC:PAGE',
    @Params     = '<PARAMS><PARAM NAME="Language" VALUE="EN"/></PARAMS><FILTERS><FILTER NAME="Type_Id"><LIST VALUE="I"/></FILTER></FILTERS>'

  -- ��������� ���� ������ � ������� XML �������� "[Base].[Persons]", ���� "[Base].[Persons]" �� ������� "Type_Id = I"
  -- ��������� ����� SELECT [XML]
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'XML:SELECT',
    @Params     = '<FILTERS><FILTER NAME="Type_Id"><LIST VALUE="I"/></FILTER></FILTERS>'

  -- ��������� ���� ������ � ������� XML �������� "[Base].[Persons]", ���� "[Base].[Persons]" �� ������� "Type_Id = I"
  -- ��������� ����� �������� @Output
DECLARE @XML XML
  EXEC [Export].[Execute]
    @SubScriber = 'Risk(Old)',
    @Object     = '[Base].[Persons]',
    @Method     = 'OFFLINE',
    @Action     = 'XML:OUTPUT',
    @Params     = '<FILTERS><FILTER NAME="Type_Id"><LIST VALUE="I"/></FILTER></FILTERS>',
    @Output     = @XML OUT
SELECT [OUTPUT] = @XML
