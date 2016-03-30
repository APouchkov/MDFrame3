USE [msdb]
GO

/****** Object:  Job [CIS - Drop Queues]    Script Date: 03/30/2016 15:24:53 ******/
BEGIN TRANSACTION
DECLARE @ReturnCode INT
SELECT @ReturnCode = 0
/****** Object:  JobCategory [[Uncategorized (Local)]]]    Script Date: 03/30/2016 15:24:53 ******/
IF NOT EXISTS (SELECT name FROM msdb.dbo.syscategories WHERE name=N'[Uncategorized (Local)]' AND category_class=1)
BEGIN
EXEC @ReturnCode = msdb.dbo.sp_add_category @class=N'JOB', @type=N'LOCAL', @name=N'[Uncategorized (Local)]'
IF (@@ERROR <> 0 OR @ReturnCode <> 0) GOTO QuitWithRollback

END

DECLARE @jobId BINARY(16)
EXEC @ReturnCode =  msdb.dbo.sp_add_job @job_name=N'CIS - Drop Queues', 
		@enabled=1, 
		@notify_level_eventlog=0, 
		@notify_level_email=0, 
		@notify_level_netsend=0, 
		@notify_level_page=0, 
		@delete_level=0, 
		@description=N'No description available.', 
		@category_name=N'[Uncategorized (Local)]', 
		@owner_login_name=N'sa', @job_id = @jobId OUTPUT
IF (@@ERROR <> 0 OR @ReturnCode <> 0) GOTO QuitWithRollback
/****** Object:  Step [BackOffice]    Script Date: 03/30/2016 15:24:53 ******/
EXEC @ReturnCode = msdb.dbo.sp_add_jobstep @job_id=@jobId, @step_name=N'BackOffice', 
		@step_id=1, 
		@cmdexec_success_code=0, 
		@on_success_action=1, 
		@on_success_step_id=0, 
		@on_fail_action=2, 
		@on_fail_step_id=0, 
		@retry_attempts=0, 
		@retry_interval=0, 
		@os_run_priority=0, @subsystem=N'TSQL', 
		@command=N'DECLARE @SQL VarChar(Max)

SET @SQL = ''''
SELECT
  @SQL = @SQL + ''DROP SERVICE '' + quotename(SS.name) + ''
''
FROM [sys].[services] SS WITH (NOLOCK)
left hash join sys.dm_exec_connections EC on SubString(SS.name, 23, 100) = EC.connection_id
WHERE SS.name like ''Notifications.Session:%'' AND EC.connection_id IS NULL
OPTION (FORCE ORDER, MAXDOP 1)

IF @SQL <> ''''
  EXEC(@SQL)

SET @SQL = ''''
SELECT
  @SQL = @SQL + ''DROP QUEUE '' + quotename(db_name()) + ''.'' + quotename(schema_name(SQ.[schema_id])) + ''.'' + quotename(object_name(SQ.[object_id])) + ''
''
FROM [sys].[service_queues] SQ WITH (NOLOCK)
left hash join sys.dm_exec_connections EC on SubString(SQ.name, 9, 100) = EC.connection_id
WHERE object_name(SQ.[object_id]) like ''Session:%'' AND EC.connection_id IS NULL

IF @SQL <> ''''
  EXEC(@SQL)
', 
		@database_name=N'CIS', 
		@flags=0
IF (@@ERROR <> 0 OR @ReturnCode <> 0) GOTO QuitWithRollback
EXEC @ReturnCode = msdb.dbo.sp_update_job @job_id = @jobId, @start_step_id = 1
IF (@@ERROR <> 0 OR @ReturnCode <> 0) GOTO QuitWithRollback
EXEC @ReturnCode = msdb.dbo.sp_add_jobschedule @job_id=@jobId, @name=N'00:20', 
		@enabled=1, 
		@freq_type=4, 
		@freq_interval=1, 
		@freq_subday_type=1, 
		@freq_subday_interval=10, 
		@freq_relative_interval=0, 
		@freq_recurrence_factor=0, 
		@active_start_date=20090325, 
		@active_end_date=99991231, 
		@active_start_time=2000, 
		@active_end_time=235959, 
		@schedule_uid=N'120e2e04-7297-4323-9987-83508e362068'
IF (@@ERROR <> 0 OR @ReturnCode <> 0) GOTO QuitWithRollback
EXEC @ReturnCode = msdb.dbo.sp_add_jobserver @job_id = @jobId, @server_name = N'(local)'
IF (@@ERROR <> 0 OR @ReturnCode <> 0) GOTO QuitWithRollback
COMMIT TRANSACTION
GOTO EndSave
QuitWithRollback:
    IF (@@TRANCOUNT > 0) ROLLBACK TRANSACTION
EndSave:

GO

