SELECT TOP 3 [Id], [Object_Id], [User_Id], [Mode], [Identity], [Parent_Id], [Params] = Cast([Params] AS NVarChar(Max)), [Data], [CreateDateTime], [AsyncThreads], [SavedParams] = Cast([SavedParams] AS NVarChar(Max)), [SavedData]
FROM [Workflow].[Queue]
ORDER BY [CreateDateTime] DESC

SELECT *
FROM [Workflow].[Queue:Active Threads]

SELECT TOP 9 R.[Queue_Id], R.[Thread], R.[Step_Id], R.[Repeate], R.[Id], Cast(R.[InputParams] AS NVarChar(Max)), R.[InputData]
, C.[DateTime], Cast(C.[Params] AS NVarChar(Max)), C.[Data], C.[Error]
FROM [Workflow].[Queue:Log:Run] R
LEFT JOIN [Workflow].[Queue:Log:Complete] C ON R.[Id] = C.[Id]
ORDER BY R.[DateTime] DESC
