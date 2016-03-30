SELECT TV.*, PTVA.[Language]
-- DELETE TV
FROM [Base].[Translate:Values] TV
INNER JOIN [Base].[Periodic:Fields(Translating)] PFT ON TV.[Field_Id] = PFT.[Translate:Field_Id]
LEFT JOIN
(
  SELECT DISTINCT
    [Key],
    [Field_Id],
    [Language]
  FROM [Base].[Periodic:Values:Translate]
) PTV ON PFT.[Periodic:Field_Id] = PTV.[Field_Id] AND TV.[Key] = PTV.[Key] AND TV.[Language] = PTV.[Language]
OUTER APPLY
(
  SELECT
    [Language] = [Pub].[Concat](DISTINCT PTVA.[Language], ',')
  FROM [Base].[Periodic:Values:Translate] PTVA
  WHERE PFT.[Periodic:Field_Id] = PTVA.[Field_Id] AND TV.[Key] = PTVA.[Key] AND TV.[Language] = PTVA.[Language]
) PTVA
WHERE PTV.[Field_Id] IS NULL


SELECT
  [Translate:Field_Id]  = TV.[Field_Id],
  [Translate:Language]  = TV.[Language],
  [Translate:Value]     = TV.[Value],
  [Key]                 = TV.[Key],
  [Periodic:Value]      = PVM.[Value],
  [Periodic:Translate:Value]= PVT.[Value]
-- UPDATE TV SET [Value] = PVT.[Value]
FROM [Base].[Translate:Values] TV
INNER JOIN [Base].[Periodic:Fields(Translating)] PFT ON TV.[Field_Id] = PFT.[Translate:Field_Id]
CROSS APPLY
(
  SELECT TOP 1
    PVM.[Date],
    PVM.[Value]
  FROM [Base].[Periodic:Values] PVM
  WHERE PFT.[Periodic:Field_Id] = PVM.[Field_Id] AND TV.[Key] = PVM.[Key] AND PVM.[Date] <= [Pub].[Today]()
  ORDER BY PVM.[Date] DESC
) PVM
INNER JOIN [Base].[Periodic:Values:Translate] PVT ON PFT.[Periodic:Field_Id] = PVT.[Field_Id] AND TV.[Key] = PVT.[Key] AND PVM.[Date] = PVT.[Date] AND TV.[Language] = PVT.[Language]
WHERE TV.[Value] <> Cast(PVT.[Value] AS NVarChar(4000))

--INSERT INTO [Base].[Translate:Values]([Field_Id], [Key], [Language], [Value])
SELECT
  [Translate:Field_Id]  = PFT.[Translate:Field_Id],
  [Key]       = PTV.[Key],
  [Language]  = PTV.[Language],
  [Value]     = PTVM.[Value]
  , [Periodic:Value] = PVM.[Value], [Translates] = TVA.[Language]
FROM
(
  SELECT DISTINCT
    [Key],
    [Field_Id],
    [Language]
  FROM [Base].[Periodic:Values:Translate]
) PTV
CROSS APPLY
(
  SELECT TOP 1
    PVM.[Date],
    PVM.[Value]
  FROM [Base].[Periodic:Values] PVM
  WHERE PTV.[Field_Id] = PVM.[Field_Id] AND PTV.[Key] = PVM.[Key] AND PVM.[Date] <= [Pub].[Today]()
  ORDER BY PVM.[Date] DESC
) PVM
INNER JOIN [Base].[Periodic:Fields(Translating)] PFT ON PTV.[Field_Id] = PFT.[Periodic:Field_Id]
LEFT JOIN
(
  SELECT
    [Key],
    [Field_Id],
    [Language]
  FROM [Base].[Translate:Values]
) TV ON PFT.[Translate:Field_Id] = TV.[Field_Id] AND TV.[Key] = PTV.[Key] AND TV.[Language] = PTV.[Language]
INNER JOIN [Base].[Periodic:Values:Translate] PTVM ON PTV.[Field_Id] = PTVM.[Field_Id] AND PTV.[Key] = PTVM.[Key] AND PTV.[Language] = PTVM.[Language] AND PVM.[Date] = PTVM.[Date]
OUTER APPLY
(
  SELECT
    [Language] = [Pub].[Concat](TVA.[Language] + '=' + TVA.[Value], ',')
  FROM [Base].[Translate:Values] TVA
  WHERE PFT.[Translate:Field_Id] = TVA.[Field_Id] AND TVA.[Key] = PTV.[Key] AND TVA.[Language] = PTV.[Language]
) TVA
WHERE TV.[Field_Id] IS NULL
