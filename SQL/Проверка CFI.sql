--1) как акции в случае, если присвоенный им код CFI имеет следующие значения:                                  первая буква - значение "E", вторая буква - значения "S", "P", "R", "C", "F", "V", за исключением случаев, когда шестая буква имеет значения "Z" или "A";
--2) как депозитарные расписки на акции в случае, если присвоенный им код CFI имеет следующие значения:         первая буква - значение "E", вторая буква - значения "S", "P", "R", "C", "F", "V", шестая буква - значение "Z" или "A";
--3) как депозитарные расписки на паи или акции инвестиционных фондов в случае, если присвоенный им код CFI имеет следующие значения: 
--                                                                                                              первая буква - значение "E", вторая буква - значение "U", шестая буква - значения "Z" или "A";
--4) как паи или акции инвестиционных фондов в случае, если присвоенный им код CFI имеет следующие значения:    первая буква - значение "E", вторая буква - значение "U", за исключением случаев, когда шестая буква имеет значения "Z" или "A";
--5) как облигации в случае, если присвоенный им код CFI имеет следующие значения:                              первая буква - значение "D", вторая буква - значения "B", "C", "T", "Y", за исключением случаев, когда шестая буква имеет значения "Z" или "A";
--6) как депозитарные расписки на облигации в случае, если присвоенный им код CFI имеет следующие значения:     первая буква - значение "D", вторая буква - значения "B", "C", "T", "Y", шестая буква - значения "Z" или "A".

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