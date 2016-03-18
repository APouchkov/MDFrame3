unit SBaseDataHelperCDS;
{$I config.inc}

interface

uses
  Classes, DB, DBClient, Provider, Variants, MidasLib,

{IFDEF FASTSCRIPT_RTTI
  fs_iinterpreter, fs_idbrtti, fs_itools,
$ENDIF}

  SBaseDataModule;

type
  TClientDataSetAccess = class(TClientDataSet);

  TClientDataSetHelper = class(TBaseDataSetHelper)
  protected
    function  GetCachedUpdates: Boolean; override;
    function  GetReadOnly: Boolean; override;
    procedure SetReadOnly(const Value: Boolean); override;
    function  GetUpdateStatusFilter: TUpdateStatusSet; override;
    procedure SetUpdateStatusFilter(Value: TUpdateStatusSet); override;
    function  GetAutoGenerateUpdateScripts: Boolean; override;
    procedure SetAutoGenerateUpdateScripts(const Value: Boolean); override;

    function  GetParams: TParams; override;

    function GetDetailFields: String; override;
    procedure SetDetailFields(const Value: String); override;

    function GetMasterFields: String; override;
    procedure SetMasterFields(const Value: String); override;

    function GetMasterSource: TDataSource; override;
    procedure SetMasterSource(const Value: TDataSource); override;

    function GetIndexFieldNames: String; override;
    procedure SetIndexFieldNames(const Value: String); override;

    function GetSQL: String; override;
    procedure SetSQL(const Value: String); override;
  public
    class function Supports: TBaseDataSetSupportedActions; override;
    constructor Create(ADataSet: TDataSet); override;

    procedure RefreshRecord; override;
    procedure ApplyUpdates; override;
    procedure CommitUpdates; override;
    procedure CancelUpdates; override;
//    procedure RestoreUpdates; override;
    procedure RevertRecord; override;
    function  UpdatesPending: Boolean; override;
    procedure RefreshDetail; override;
  end;

{IFDEF FASTSCRIPT_RTTI
  TFunctions_CDS = class(TfsRTTIModule)
  private
  public
    constructor Create(AScript: TfsScript); override;
  end;
$ENDIF}


implementation

uses SysUtils, SBaseConstants;

{ TClientDataSetHelper }

procedure TClientDataSetHelper.SetAutoGenerateUpdateScripts(const Value: Boolean);
begin
  if not Value then
    Raise Exception.Create(SAbstractError);
end;

procedure TClientDataSetHelper.CommitUpdates;
begin
  TClientDataSet(DataSet).Reconcile(Null);
end;

procedure TClientDataSetHelper.CancelUpdates;
begin
  TClientDataSet(DataSet).CancelUpdates;
end;

{
procedure TClientDataSetHelper.RestoreUpdates;
begin
  TClientDataSet(DataSet).RestoreUpdates;
end;
}

procedure TClientDataSetHelper.ApplyUpdates;
begin
  TClientDataSet(DataSet).ApplyUpdates(0)
end;

function TClientDataSetHelper.GetAutoGenerateUpdateScripts: Boolean;
begin
  Result := True
end;

constructor TClientDataSetHelper.Create(ADataSet: TDataSet);
begin
  if ADataSet is TClientDataSet then
    inherited
  else
    Raise Exception.Create('Class ' + ADataSet.ClassName + ' is not inherited from TClientDataSet');
end;

function TClientDataSetHelper.GetCachedUpdates: Boolean;
begin
  Result := True;
end;

function TClientDataSetHelper.GetDetailFields: String;
begin
  if TClientDataSet(DataSet).MasterFields <> '' then
    Result := TClientDataSet(DataSet).IndexFieldNames
  else
    Result := ''
end;

function TClientDataSetHelper.GetIndexFieldNames: String;
begin
  Result := TClientDataSet(DataSet).IndexFieldNames;
end;

procedure TClientDataSetHelper.SetIndexFieldNames(const Value: String);
begin
  TClientDataSet(DataSet).IndexFieldNames := Value;
end;

function TClientDataSetHelper.GetMasterFields: String;
begin
  Result := TClientDataSet(DataSet).MasterFields
end;

function TClientDataSetHelper.GetMasterSource: TDataSource;
begin
  Result := TClientDataSet(DataSet).MasterSource
end;

function TClientDataSetHelper.GetParams: TParams;
begin
  Result := TClientDataSet(DataSet).Params
end;

procedure TClientDataSetHelper.SetDetailFields(const Value: String);
begin
  TClientDataSet(DataSet).IndexFieldNames := Value
end;

procedure TClientDataSetHelper.SetMasterFields(const Value: String);
begin
  TClientDataSet(DataSet).MasterFields := Value
end;

procedure TClientDataSetHelper.SetMasterSource(const Value: TDataSource);
begin
  TClientDataSet(DataSet).MasterSource := Value
end;

procedure TClientDataSetHelper.RefreshDetail;
begin
  TClientDataSetAccess(DataSet).DataEvent(deParentScroll, 0);
end;

procedure TClientDataSetHelper.RefreshRecord;
begin
  TClientDataSet(DataSet).RefreshRecord;
end;

procedure TClientDataSetHelper.RevertRecord;
begin
  TClientDataSet(DataSet).RevertRecord
end;

function TClientDataSetHelper.GetReadOnly: Boolean;
begin
  Result := TClientDataSetAccess(DataSet).ReadOnly
end;

procedure TClientDataSetHelper.SetReadOnly(const Value: Boolean);
begin
  TClientDataSetAccess(DataSet).ReadOnly := Value;
end;

class function TClientDataSetHelper.Supports: TBaseDataSetSupportedActions;
begin
  Result := [bdssaSQL];
end;

function TClientDataSetHelper.UpdatesPending: Boolean;
begin
  Result := TClientDataSetAccess(DataSet).ChangeCount > 0
end;

function TClientDataSetHelper.GetSQL: String;
begin
  Result := TClientDataSetAccess(DataSet).CommandText
end;

function TClientDataSetHelper.GetUpdateStatusFilter: TUpdateStatusSet;
begin
  Result := TClientDataSetAccess(DataSet).StatusFilter;
  if Result = [] then
    Result := Default_UpdateStatusFilter
end;

procedure TClientDataSetHelper.SetUpdateStatusFilter(Value: TUpdateStatusSet);
begin
  if Value = Default_UpdateStatusFilter then
    Value := [];

  if TClientDataSetAccess(DataSet).StatusFilter <> Value then
    TClientDataSetAccess(DataSet).StatusFilter := Value;
end;

procedure TClientDataSetHelper.SetSQL(const Value: String);
begin
  TClientDataSetAccess(DataSet).CommandText := Value;
end;

{IFDEF FASTSCRIPT_RTTI
 TFunctions_CDS

constructor TFunctions_CDS.Create(AScript: TfsScript);
begin
  inherited;
end;

$ENDIF}

initialization
  TBaseDataSetHelper.RegisterHelperClass(TClientDataSet, TClientDataSetHelper);

  RegisterClasses(
    [
      TClientDataSet, TDataSetProvider
    ]);

{IFDEF FASTSCRIPT_RTTI
  fsRTTIModules.Add(TFunctions_CDS);
$ENDIF}

finalization
  UnRegisterClasses(
    [
      TClientDataSet, TDataSetProvider
    ]);

end.
