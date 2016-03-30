UPDATE T SET
  [IncActiveState_Id] = CASE WHEN T.[Code] LIKE '%:ACC' THEN 11 ELSE 0 END,
  [DecActiveState_Id] = CASE WHEN T.[Code] LIKE '%:EXE' THEN 11 END
FROM [BackOffice].[Dic:Transactions:Types] T
WHERE T.[Code] LIKE 'BDM:MON:IN:%' AND [CanBeSimpleTransaction] = 1

UPDATE T SET
  [IncActiveState_Id] = CASE WHEN T.[Code] LIKE '%:ACC' THEN 11 ELSE 0 END,
  [DecActiveState_Id] = CASE WHEN T.[Code] LIKE '%:EXE' THEN 11 END
FROM [BackOffice].[Dic:Transactions:Types] T
WHERE T.[Code] LIKE 'BDM:MON:INC:%' AND [CanBeSimpleTransaction] = 1

UPDATE T SET
  [IncActiveState_Id] = 0
FROM [BackOffice].[Dic:Transactions:Types] T
WHERE T.[Code] LIKE 'BDM:MON:TRANS:%:INC' AND [CanBeSimpleTransaction] = 1

UPDATE T SET
  [DecActiveState_Id] = CASE WHEN T.[Code] LIKE '%:ACC' THEN 12 ELSE 0 END,
  [IncActiveState_Id] = CASE WHEN T.[Code] LIKE '%:EXE' THEN 12 END
FROM [BackOffice].[Dic:Transactions:Types] T
WHERE T.[Code] LIKE 'BDM:MON:DEC:%' AND [CanBeSimpleTransaction] = 1

UPDATE T SET
  [DecActiveState_Id] = 0
FROM [BackOffice].[Dic:Transactions:Types] T
WHERE (T.[Code] LIKE 'BDM:MON:OUT:%' OR T.[Code] LIKE 'BDM:MON:TRANS:%:DEC') AND [CanBeSimpleTransaction] = 1

UPDATE T SET
  [IncActiveState_Id] = 0
FROM [BackOffice].[Dic:Transactions:Types] T
WHERE (T.[Code] LIKE 'BDM:SEC:IN:%' OR T.[Code] LIKE 'BDM:SEC:TRANS:%:INC') AND [CanBeSimpleTransaction] = 1

UPDATE T SET
  [DecActiveState_Id] = 0
FROM [BackOffice].[Dic:Transactions:Types] T
WHERE (T.[Code] LIKE 'BDM:SEC:OUT:%' OR T.[Code] LIKE 'BDM:SEC:TRANS:%:DEC') AND [CanBeSimpleTransaction] = 1
