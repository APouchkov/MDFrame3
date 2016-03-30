-- �� ��� ��� ��� ��, ���
-- �� � ��� �������� ���������� ��������� (�������), ���� �������� ������������ � ���������� 2 � �������� �������������� ������� ���������� ��������� (�����);
-- ��� � ��� ������;
-- ��� � ��� ������;      
-- ��� � ��� ����������� ������;	
-- �� � ������� ������������ ������������ ��������� �������.

-- �� ��� ��� ��� ���� ��, ���
-- �� � ��� �������� ���������� ��������� (�������), ���� �������� ������������ � ���������� 2 � �������� �������������� ������� ���������� ��������� (�����);
-- ��� � ��� ������;
-- ��� � ��� ������;      
-- ��� � ��� ����������� ������;	
-- ���� � ��� �����;
-- �� � ������� ������������ ������������ ��������� �������.
/*
�00� � ���������� ������ (��� ������������, ������������� ������������� ��������� �� ������ ������ ��������� ������������). 
            �01�-�50� � ������ ��� ������������, � ������ ������ ��������� ���� �� ������� ��� ������������ (���������� �������� ������ ������������ � ���� ������ � ��� �� �����, �� � ��������� ������������ �00�;
            �51�      � ������ ��� ������������ ��� ������ � ������ ������� ������� (���������� �������� ������ ������������ �� ���� Altnames.dbf;
            �52�-�98� � ��������� �������� �������� ������������;
            �99�      � �������� ������ �� ����������, �.�. ��� ���������������� ��� ����������� ��������� �������.
*/
ALTER FUNCTION [dbo].[KLADR::Search(Pre)]
(
  @ParentLevel  TinyInt,
  @ParentCode   VarChar(20),
  @Name         VarChar(100),
  @Next1Name    VarChar(100),
  @Next2Name    VarChar(100),
  @Index        VarChar(10)
)
  RETURNS @Result TABLE
  (
    [Level_Id] TinyInt            NOT NULL,
    [Code]     VarChar(19)            NULL,
    [Name]     NVarChar(100)      NOT NULL,
    [NextName] NVarChar(100)          NULL,
    [Article]  NVarChar(19)           NULL,
    [Index]    VarChar(7)             NULL,
    [ErrorDescription]  NVarChar(200) NULL
  )
AS BEGIN
  IF @Name IS NULL OR @ParentLevel IS NULL OR @ParentLevel = 6 RETURN

  DECLARE
    @@NAME      VarChar(100),
    @SOCR       VarChar(50),
    @I          Int,
    @J          Int

  SET @I = IsNull(CharIndex(' ', @Name), 0)
  IF Right(@Name, 1) = '.'
    SET @J = LEN(@Name)
  ELSE
    SET @J = IsNull(CharIndex('.', @Name), 0)

  -- EQUAL
  IF @Name IN ('��������� ����������', '��������� ����.', '���������� �������', '��������� ���������� - �������')
    SELECT
      @SOCR  = '�������',
      @@NAME = '��������� ���������� -'

  -- LEFT
  ELSE IF Left(@Name, 11) = '���������� '
    SELECT
      @SOCR = '����',
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 12, 1000))
  ELSE IF Left(@Name, 9) = '�������� '
    SELECT
      @SOCR = '��-��',
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 10, 1000))
  ELSE IF Left(@Name, 3) = '��.'
    SELECT
      @SOCR  = '��',
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 4, 1000))
  ELSE IF Left(@Name, 4) = '�-� '
    SELECT
      @SOCR  = '�-�',
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 5, 1000))
  ELSE IF Left(@Name, 6) IN (
                              '�����.',
                              '��-��.',
                              '��-��.',
                              '��-��.'
                            )
    SELECT
      @SOCR = Left(@Name, 5),
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 7, 1000))
  ELSE IF Left(@Name, 6) IN (
                              '����� ',
                              '����� ',
                              '����� ',
                              '����� ',
                              '����� ',
                              '����� ',
                              '����� ',
                              '����� ',
                              '����� ',
                              '����� ',
                              '����� ',
                              '����� ',
                              '����� ',
                              '����� ',
                              '��-�� ',
                              '��-�� ',
                              '����� ',
                              '����� ',
                              '��-�� ',
                              '����� ',
                              '����� '
                            )
    SELECT
      @SOCR = Left(@Name, 5),
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 7, 1000))
  ELSE IF Left(@Name, 7) IN (
                              '������ ',
                              '������ ',
                              '������ ',
                              '������ ',
                              '������ ',
                              '������ ',
                              '������ ',
                              '������ ',
                              '������ ',
                              '������ '
                              )
    SELECT
      @SOCR  = Left(@Name, 6),
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 8, 1000))
  ELSE IF Left(@Name, 8) IN (
                              '������� ',
                              '������� ',
                              --'�/�_���',
                              '������� ',
                              '������� ',
                              '������� ',
                              '������� ',
                              '������� ',
                              '������� ',
                              '������� ',
                              '������� '
                            )
    SELECT
      @SOCR  = Left(@Name, 7),
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 9, 1000))
  ELSE IF Left(@Name, 9) IN (
                              '�������� ',
                              '�������� ',
                              '�������� '
                            )
    SELECT
      @SOCR  = Left(@Name, 8),
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 10, 1000))
  ELSE IF Left(@Name, 9) = '�������� '
    SELECT
      @SOCR = '�',
      @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, 10, 1000))

  -- RIGHT
  ELSE IF Right(@Name, 11) = ' ����������'
    SELECT
      @SOCR = '����',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 11))
  ELSE IF Right(@Name, 6) = ' ����.'
    SELECT
      @SOCR = '����',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 6))
  ELSE IF Right(@Name, 17) = ' ���������� �����'
    SELECT
      @SOCR  = '��',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 17))

  ELSE IF Right(@Name, 2) IN (
                              ' �',
                              ' �',
                              ' �',
                              ' �',
                              ' �',
                              ' �',
                              ' �',
                              ' �'
                              )
    SELECT
      @SOCR  = Right(@Name, 1),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 2))

  ELSE IF Right(@Name, 3) IN (
                              ' ��',
                              ' ��',
                              ' ��',
                              ' ��',
                              ' ��',
                              ' ��',
                              ' ��',
                              ' ��',
                              ' ��',
                              ' ��',
                              ' ��',
                              ' ��'
                              )
    SELECT
      @SOCR  = Right(@Name, 2),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 3))

  ELSE IF Right(@Name, 4) IN (
                              ' �/�',
                              ' ���',
                              ' ���',
                              ' �-�',
                              ' ���',
                              ' ���',
                              ' ���',
                              ' ���',
                              ' ���',
                              ' ���',
                              ' ���',
                              ' ���',
                              ' �/�',
                              ' ���',
                              ' ���',
                              ' �/�',
                              ' �/�',
                              ' ���',
                              ' ���',
                              ' ���',
                              ' �-�',
                              ' �/�',
                              ' �/�',
                              ' �/�',
                              ' �/�',
                              ' ���',
                              ' ���',
                              ' ���',
                              ' ���',
                              ' ���',
                              ' �/�'
                              )
    SELECT
      @SOCR  = Right(@Name, 3),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 4))

  ELSE IF Right(@Name, 5) IN (
                              ' ����',
                              ' ����',
                              ' ��-�',
                              ' ����',
                              ' ����',
                              ' ����',
                              ' ����',
                              ' �/��',
                              ' ����',
                              ' ����',
                              ' ����',
                              ' �/��',
                              ' ��-�'
                              )
    SELECT
      @SOCR  = Right(@Name, 4),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 5))

  ELSE IF Right(@Name, 6) IN (
                              ' �����',
                              ' �����',
                              ' �����',
                              ' �����',
                              ' �����',
                              ' �����',
                              ' �����',
                              ' �����',
                              ' �����',
                              ' �����',
                              ' �����',
                              ' �����',
                              ' �����',
                              ' �����',
                              ' ��-��',
                              ' ��-��',
                              ' �����',
                              ' �����',
                              ' ��-��',
                              ' �����',
                              ' �����'
                              )
    SELECT
      @SOCR  = Right(@Name, 5),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 6))
  ELSE IF Right(@Name, 7) IN (
                              ' �/�_��',
                              ' �/�_��',
                              ' ������',
                              ' ������',
                              ' ������',
                              ' ������',
                              ' ������',
                              ' ������',
                              ' ������',
                              ' ������',
                              ' ������',
                              ' ������'
                              )
    SELECT
      @SOCR  = Right(@Name, 6),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 7))
  ELSE IF Right(@Name, 8) IN (
                              ' �������',
                              ' �������',
                              ' �/�_���',
                              ' �������',
                              ' �������',
                              ' �������',
                              ' �������',
                              ' �������',
                              ' �������',
                              ' �������',
                              ' �������'
                              --' �������'
                              )
    SELECT
      @SOCR  = Right(@Name, 7),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 8))
  ELSE IF Right(@Name, 9) IN (
                              ' �/�_����',
                              ' ��������',
                              ' ��������',
                              ' ��������'
                              )
    SELECT
      @SOCR  = Right(@Name, 8),
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 9))
  ELSE IF Right(@@NAME, 8) = ' �������'
    SELECT
      @SOCR = '�-�',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 8))
  ELSE IF Right(@Name, 9) = ' ��������'
    SELECT
      @SOCR = '��-��',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 9))
  ELSE IF Right(@Name, 6) = ' �����'
    SELECT
      @SOCR = '�',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 6))
  ELSE IF Right(@Name, 16) = ' ���������� ���.'
    SELECT
      @SOCR = '����',
      @@NAME = [Pub].[Trim(Spaces)](Left(@Name, Len(@Name) - 16))

  -- Find '.' POINT
  ELSE IF @J > 0 BEGIN
    IF @I = 0 OR @I > @J
      SELECT
        @SOCR = [Pub].[Trim(Spaces)](Left(@Name, @J - 1)),
        @@NAME = [Pub].[Trim(Spaces)](SubString(@Name, @J + 1, 1000))
    ELSE IF @I > 0 AND @I < @J AND @J = LEN(@Name)
      SELECT
        @I    = [Pub].[CharIndex(Back)](' ', @Name, @J - 1),
        @SOCR = [Pub].[Trim(Spaces)](SubString(@Name, @I + 1, @J - @I - 1)),
        @@NAME = [Pub].[Trim(Spaces)](Left(@Name, @I - 1))
  END ELSE
    SET @@NAME = @Name

  IF @SOCR = '����' AND @@NAME = '���� (������)'
    SET @@NAME = '���� /������/'

  INSERT INTO @Result([Level_Id], [Code], [Name], [NextName], [Article], [Index], [ErrorDescription])
  SELECT *
  FROM [dbo].[KLADR::Search](@ParentLevel + 1, @ParentLevel, @ParentCode, @SOCR, CASE WHEN @ParentLevel + 1 < 6 THEN @@NAME ELSE @Name END, @Next1Name, @Next2Name, @Index)

  -- ����� �� ������� ����� ��� ����
  IF @@ROWCOUNT = 0 AND @ParentLevel < 5 BEGIN
    INSERT INTO @Result ([Level_Id], [Code], [Name], [NextName], [Article], [Index], [ErrorDescription])
    SELECT *
    FROM [dbo].[KLADR::Search](6, @ParentLevel, @ParentCode, NULL, @Name, @Next1Name, @Next2Name, NULL)
  END

  RETURN
END
GO


--SELECT * FROM [dbo].[KLADR::Search(Pre)](2, '6001400000000', N'������ �.', NULL, NULL, NULL)

--  SELECT *
--  FROM [dbo].[KLADR::Search](2 + 1, 2, '6001400000000', '�', '������', '��� 24', null)

--SELECT * FROM [KLADR:Base] WHERE CharIndex('�����', NAME) > 0
--SELECT DISTINCT SOCR FROM [KLADR:Base] ORDER BY 1