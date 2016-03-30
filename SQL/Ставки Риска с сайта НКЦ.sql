DECLARE
  @Date         Date = '20141201',

  @Body         NVarChar(Max),
  @Header       NVarChar(1000),
  @I            Int,
  @IInstrument  Int,
  @IRange       Int,
  @IRangeBegin  Int,
  @IRangeEnd    Int,
  @IDown        Int,
  @IUp          Int

-- ����������;��������;������ ���������, ��.;����� ���������, ��.;
-- ������ ����� ������� ����,%;������ ����� ����� ����,%;
-- ������ �������, ���.;��������� ����, ���;������� �������, ���.;
-- �������� ������ �� ������� ������� �� �������, %;�������� ������ �� ������� ������� �� �������, %;
-- ����� �/� � �����������;������ �������� ������;���� � ����� ��������/ ��������� ����������;

SELECT 
  @Body = R.[Body]
FROM [System].[HTTP::Request]('http://nkcbank.ru/fondMarketDateCsv.do?date=' + Convert(NVarChar(10), @Date, 104), 'GET', Default) R

SET @Body = SubString(@Body, CharIndex(Char(13), @Body) + 1, Len(@Body))
SET @I = CharIndex(Char(13), @Body)
SET @Header = [Pub].[Trim(Spaces)](SubString(@Body, 1, @I - 1))
SET @Body = SubString(@Body, @I + 1, Len(@Body))

SELECT
  @IInstrument = CASE WHEN [Value] = N'����������' THEN [Index] ELSE @IInstrument END,
  @IRange      = CASE WHEN [Value] = N'��������' THEN [Index] ELSE @IRange END,
  @IRangeBegin = CASE WHEN [Value] = N'������ ���������, ��.' THEN [Index] ELSE @IRangeBegin END,
  @IRangeEnd   = CASE WHEN [Value] = N'����� ���������, ��.' THEN [Index] ELSE @IRangeEnd END,
  @IDown       = CASE WHEN [Value] = N'������ ����� ������� ����,%' THEN [Index] ELSE @IDown END,
  @IUp         = CASE WHEN [Value] = N'������ ����� ����� ����,%' THEN [Index] ELSE @IUp END
FROM [Pub].[Array To RowSet Of Values:VarChar(Non Unique)](@Header, N';')

SELECT
  [Instrument]  = [Pub].[Extract Value](S.[Value], @IInstrument, ';'),
  [Range]       = [Pub].[Extract Value](S.[Value], @IRange, ';'),
  [RangeBegin]  = [Pub].[Extract Value](S.[Value], @IRangeBegin, ';'),
  [RangeEnd]    = [Pub].[Extract Value](S.[Value], @IRangeEnd, ';'),
  [Down]        = [Pub].[Extract Value](S.[Value], @IDown, ';'),
  [Up]          = [Pub].[Extract Value](S.[Value], @IUp, ';')
FROM [Pub].[Array To RowSet Of Values:VarChar(Non Unique)](@Body, Char(13)) S
