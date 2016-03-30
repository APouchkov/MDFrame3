--1) ��� ����� � ������, ���� ����������� �� ��� CFI ����� ��������� ��������:                                  ������ ����� - �������� "E", ������ ����� - �������� "S", "P", "R", "C", "F", "V", �� ����������� �������, ����� ������ ����� ����� �������� "Z" ��� "A";
--2) ��� ������������ �������� �� ����� � ������, ���� ����������� �� ��� CFI ����� ��������� ��������:         ������ ����� - �������� "E", ������ ����� - �������� "S", "P", "R", "C", "F", "V", ������ ����� - �������� "Z" ��� "A";
--3) ��� ������������ �������� �� ��� ��� ����� �������������� ������ � ������, ���� ����������� �� ��� CFI ����� ��������� ��������: 
--                                                                                                              ������ ����� - �������� "E", ������ ����� - �������� "U", ������ ����� - �������� "Z" ��� "A";
--4) ��� ��� ��� ����� �������������� ������ � ������, ���� ����������� �� ��� CFI ����� ��������� ��������:    ������ ����� - �������� "E", ������ ����� - �������� "U", �� ����������� �������, ����� ������ ����� ����� �������� "Z" ��� "A";
--5) ��� ��������� � ������, ���� ����������� �� ��� CFI ����� ��������� ��������:                              ������ ����� - �������� "D", ������ ����� - �������� "B", "C", "T", "Y", �� ����������� �������, ����� ������ ����� ����� �������� "Z" ��� "A";
--6) ��� ������������ �������� �� ��������� � ������, ���� ����������� �� ��� CFI ����� ��������� ��������:     ������ ����� - �������� "D", ������ ����� - �������� "B", "C", "T", "Y", ������ ����� - �������� "Z" ��� "A".

DECLARE
  @Firm_Id      TinyInt = 1,
  @FirmType_Id  TinyInt = 16,
  @Date         Date    = '20120101'

SELECT BSI.*, SI.CodeCFI, IT.[Code] 
FROM [BackOffice].[Instruments:Securities:Issues] BSI
INNER JOIN [Base].[Instruments:Securities:Issues] SI ON BSI.Instrument_Id = SI.Id
INNER JOIN [Base].[Instruments] I ON BSI.Instrument_Id = I.Id
INNER JOIN [Base].[Dic:Instruments:Types] IT ON I.[Type_Id] = IT.[Id] AND IT.[Code] LIKE '%:FOREIGN:%'
WHERE BSI.Firm_Id = @Firm_Id AND BSI.[FirmType_Id] = @FirmType_Id
    AND (BSI.[DateEnd] IS NULL OR BSI.[DateEnd] >= @Date)
    AND
    (
      SI.CodeCFI IS NULL
      OR
      NOT
      (
        (IT.[Code] LIKE '%:STOCKS:%' OR IT.[Code] LIKE '%:DR:%' OR IT.[Code] LIKE '%:SHARES:%') AND (SI.CodeCFI LIKE 'E[SPRCFV]%' OR SI.CodeCFI LIKE 'EU%')
        OR (IT.[Code] LIKE '%:BONDS:%' OR IT.[Code] LIKE '%:DR:%') AND (SI.CodeCFI LIKE 'D[BCTY]%')
        --OR IT.[Code] LIKE '%:SHARES:%' AND (SI.CodeCFI LIKE 'EU%')
      )
    )